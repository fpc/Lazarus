{  $Id$  }
unit LazarusManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,
  FileCtrl, Forms,
  LazConf,
  StartLazOpts, Splash;
  
type
  TLazarusProcess = class
  private
    FOnStart: TNotifyEvent;
    FProcess: TProcess;
    FLazarusPath: string;
    FWantsRestart: boolean;
  public
    constructor Create(const LazarusPath: string);
    destructor Destroy; override;
    procedure Execute;
    procedure WaitOnExit;
    property WantsRestart: boolean read FWantsRestart;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;
  
type
  TLazarusManager = class(TComponent)
  private
    FStartLazarusOptions: TStartLazarusOptions;
    FLazarusProcess: TLazarusProcess;
    FLazarusPath: string;
    function GetLazarusPath(const FileName: string): string;
    procedure RenameLazarusExecutables;
    procedure LazarusProcessStart(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Run;
    procedure ShowSplash;
  end;

implementation

constructor TLazarusManager.Create;
begin
  inherited Create(nil);
  SplashForm := TSplashForm.Create(Self);
  ShowSplash;
  FStartLazarusOptions := TStartLazarusOptions.Create;
end;

destructor TLazarusManager.Destroy;
begin
  FreeAndNil(FStartLazarusOptions);
  inherited Destroy;
end;

function TLazarusManager.GetLazarusPath(const FileName: string) : string;
begin
  result := AppendPathDelim(FStartLazarusOptions.LazarusDir) + FileName +
    GetDefaultExecutableExt;
end;

procedure TLazarusManager.RenameLazarusExecutables;
var
  NewFileName: string;
  BackupFileName: String;
begin
  NewFileName := GetLazarusPath('lazarus.new');
  FLazarusPath := GetLazarusPath('lazarus');
  BackupFileName := GetLazarusPath('lazarus.old');
  if FileExists(NewFileName) then
  begin
    if FileExists(FLazarusPath) then
    begin
      if FileExists(BackupFileName)
        then DeleteFile(BackupFileName);
      RenameFile(FLazarusPath, BackupFileName);
    end;
    RenameFile(NewFileName, FLazarusPath);
  end;
end;

procedure TLazarusManager.LazarusProcessStart(Sender: TObject);
begin
  SplashForm.Hide;
end;

procedure TLazarusManager.Run;
var
  Restart: boolean;
begin
  repeat
    SplashForm.Show;
    Application.ProcessMessages;
    RenameLazarusExecutables();
    FLazarusProcess := TLazarusProcess.Create(FLazarusPath);
    FLazarusProcess.OnStart := @LazarusProcessStart;
    FLazarusProcess.Execute;
    FLazarusProcess.WaitOnExit;
    Restart := FLazarusProcess.WantsRestart;
    FreeAndNil(FLazarusProcess);
  until not Restart;
  Application.Terminate;
end;

procedure TLazarusManager.ShowSplash;
begin
  with SplashForm do begin
    Show;
    Paint;
  end;
  Application.ProcessMessages; // process splash paint message
end;

{ TLazarusProcess }

constructor TLazarusProcess.Create(const LazarusPath: string);
begin
  FLazarusPath := LazarusPath;
  FProcess := TProcess.Create(nil);
  FProcess.Options := [];
  FProcess.CommandLine := FLazarusPath + ' --no-splash-screen';
end;

destructor TLazarusProcess.Destroy;
begin
  FreeAndNil(FProcess);
  inherited Destroy;
end;

procedure TLazarusProcess.Execute;
begin
  FProcess.Execute;
  {$IFNDEF VER1_0}
  Sleep(2000);
  {$ENDIF}
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TLazarusProcess.WaitOnExit;
begin
  FProcess.WaitOnExit;
  FWantsRestart := FProcess.ExitStatus=99
end;

end.
{
  $Log$
  Revision 1.4  2004/09/04 23:02:56  mattias
  added reintroduce to get rid of the warning

  Revision 1.3  2004/09/03 21:14:50  vincents
  fix showing splash screen on restart

}

