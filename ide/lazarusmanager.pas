{  $Id$  }
unit LazarusManager;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF win32}
  Windows,
{$ENDIF}
{$IFDEF unix}
  BaseUnix,
{$ENDIF}
  Classes, SysUtils, Process,
  LCLProc, FileUtil, Forms,
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
    FLazarusPID: Integer;
    FCmdLineParams: TStrings;
    procedure ParseCommandLine;
    function GetLazarusPath(const FileName: string): string;
    procedure RenameLazarusExecutables;
    procedure LazarusProcessStart(Sender: TObject);
    procedure WaitForLazarus;
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
  ParseCommandLine;
end;

destructor TLazarusManager.Destroy;
begin
  FreeAndNil(FCmdLineParams);
  FreeAndNil(FStartLazarusOptions);
  inherited Destroy;
end;

procedure TLazarusManager.ParseCommandLine;
const
  LazarusPidOpt='--lazarus-pid=';
var
  i: Integer;
  Param: string;
begin
  FCmdLineParams := TStringList.Create;
  FLazarusPID := 0;
  for i := 1 to ParamCount do begin
    Param := ParamStr(i);
    if LeftStr(Param,length(LazarusPidOpt))=LazarusPidOpt then begin
      try
        FLazarusPID :=
          StrToInt(RightStr(Param,Length(Param)-Length(LazarusPidOpt)));
      except
        DebugLn('Failed to parse %s',[Param]);
        FLazarusPid := 0;
      end;
    end
    else
      FCmdLineParams.Add(Param);
  end;
end;

function TLazarusManager.GetLazarusPath(const FileName: string) : string;
begin
  Result := AppendPathDelim(FStartLazarusOptions.LazarusDir) + FileName +
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

procedure TLazarusManager.WaitForLazarus;
  procedure WaitForPid(PID: integer);
  {$IFDEF win32}
  var
    ProcessHandle: THandle;
  begin
    ProcessHandle := OpenProcess(SYNCHRONIZE, false, PID);
    WaitForSingleObject(ProcessHandle, INFINITE);
  end;
  {$ELSE}
  {$IFDEF UNIX}
  var
    Result: integer;
  begin
    repeat
      Sleep(100);
      Result := fpKill(PID, 0);
    until Result<>0;
  end;
  {$ELSE}
  begin
    DebugLn('WaitForPid not implemented for this OS. We just wait 5 seconds');
    Sleep(5000);
  end;
  {$ENDIF}
  {$ENDIF}
begin
  if FLazarusPID<>0 then begin
    WaitForPID(FLazarusPID);
  end;
end;

procedure TLazarusManager.Run;
var
  Restart: boolean;
begin
  WaitForLazarus;
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
  FProcess.ShowWindow := swoShow;
  FProcess.CommandLine := FLazarusPath + ' --no-splash-screen --started-by-startlazarus';
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
  Revision 1.8  2004/11/03 14:18:34  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.7  2004/10/31 21:17:34  vincents
  - Implemented restarting by starting startlazarus on unix (for 1.9.x only).
  - Add Restart After Succesfull Build CheckBox to the Configure Build Lazarus dialog.

  Revision 1.6  2004/10/27 20:49:26  vincents
  Lazarus can be restarted, even if not started by startlazarus (only win32 implemented).

  Revision 1.5  2004/09/27 22:05:40  vincents
  splitted off unit FileUtil, it doesn't depend on other LCL units

  Revision 1.4  2004/09/04 23:02:56  mattias
  added reintroduce to get rid of the warning

  Revision 1.3  2004/09/03 21:14:50  vincents
  fix showing splash screen on restart

}

