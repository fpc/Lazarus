unit DaemonUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, DaemonWorkerThread
  {$IFDEF UNIX}
  , DaemonSystemdInstallerUnit // Some systemd installer support for -install | -ininstall
  {$ENDIF};

// -------------------------------------------------------------------------------
// Demo application on how to use the TDaemon application type
// 2/2022 by arminlinder@arminlinder.de

// Based on document
// "Taming the daemon: Writing cross-platform service applications in FPC/Lazarus"
// by Michaël Van Canneyt, February 4, 2007
// -------------------------------------------------------------------------------

const
  DAEMON_VERSION = '1.2';  // Just for logging

type

  { TTestDaemon }

  TTestDaemon = class(TDaemon)
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    procedure DataModuleContinue(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModulePause(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: boolean);
  private
    FDaemonWorkerThread: TDaemonWorkerThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure LogToFile(aMessage: string);

var
  TestDaemon: TTestDaemon;

implementation

{$R *.lfm}

// ---------------------------------------------------------------------
// Quick and dirty write log message to file
// Note: TDaemonApplication has extensive logging capabilities built-in!
// ---------------------------------------------------------------------

procedure LogToFile(aMessage: string);

  function TimeStamped(S: string): string;
    // Return a timestamped copy of a string

  begin
    Result := FormatDateTime('hh:mm:ss', now) + ' ' + S;
  end;

var
  f: Text;
  LogFilePath: string;

begin
  // create a daily log file in the .exe directory
  LogFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
    FormatDateTime('YYYYMMDD', now) + '.log';
  AssignFile(f, LogFilePath);
  try
    if FileExists(LogFilePath) then
      Append(f)
    else
    begin
      Rewrite(f);
      writeln(f, TimeStamped('Log created'));
    end;
    Writeln(f, TimeStamped(aMessage));
  finally
    CloseFile(f);
  end;
end;

{ TTestDaemon }

// -------------------------------
// Installation and deinstallation
// -------------------------------
// Note: Windows code is already in the DaemonApp unit
// Code here tries to approximate same functionality for Unix

procedure TTestDaemon.DataModuleAfterInstall(Sender: TCustomDaemon);

var
  isInstalled: boolean;
  FilePath: string;

begin
  isInstalled := True;
  {$IFDEF UNIX}
  FilePath := GetSystemdControlFilePath(self.Name);
  isInstalled := CreateSystemdControlFile(self, FilePath);
  if not isInstalled then
    LogToFile('Error creating systemd control file: ' + FilePath);
  {$ENDIF}
  if isInstalled then
    LogToFile('Daemon installed, version:' + DAEMON_VERSION);
end;

procedure TTestDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);

var
  isUnInstalled: boolean;
  FilePath: string;

begin
  isUninstalled := True;
  {$IFDEF UNIX}
  FilePath := GetSystemdControlFilePath(self.Name);
  isUnInstalled := RemoveSystemdControlFile(FilePath);
  if not isUninstalled then
    LogToFile('Error removing systemd control file: ' + FilePath);
  {$ENDIF}
  if isUninstalled then
    LogToFile('Daemon uninstalled');
end;

// -----------------------
// Service control signals
// -----------------------

procedure TTestDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: boolean);

begin
  LogToFile(Format('Daemon received start signal, PID:%d', [GetProcessID]));
  // Create a suspended worker thread
  FDaemonWorkerThread := TDaemonWorkerThread.Create;
  // Parametrize it
  FDaemonWorkerThread.FreeOnTerminate := False;
  // Start the worker
  FDaemonWorkerThread.Start;
  OK := True;
end;

procedure TTestDaemon.DataModulePause(Sender: TCustomDaemon; var OK: boolean);
// Note: Pause and resume signals are provided by Windows only

begin
  LogToFile('Daemon recedived pause signal');
  FDaemonWorkerThread.Suspend;    // deprecated, yet still working
  OK := True;
end;

procedure TTestDaemon.DataModuleContinue(Sender: TCustomDaemon; var OK: boolean);
// Note: Pause and resume signals are provided by Windows only

begin
  LogToFile('Daemon recedived continue signal');
  FDaemonWorkerThread.resume;    // deprecated, yet still working
  OK := True;
end;

procedure TTestDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: boolean);

begin
  LogToFile('Daemon received stop signal');
  // stop and terminate the worker
  FDaemonWorkerThread.Terminate;
  // Wait for the thread to terminate.
  FDaemonWorkerThread.WaitFor;
  FDaemonWorkerThread.Free;
  LogToFile('Daemon stopped');
  OK := True;
end;

// --------------------------
// constructor and destructor
// --------------------------

constructor TTestDaemon.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  // Nothing to do here, just for logging
  LogToFile('Daemon object created');
end;

destructor TTestDaemon.Destroy;

begin
  // Nothing to do here, just for logging
  LogToFile('Daemon object destroyed');
  inherited Destroy;
end;

// ----------------------------------------
// Daemon registration. Created by template
// ----------------------------------------

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TTestDaemon);
end;

initialization
  RegisterDaemon;
end.
