{  $Id$  }
{
 /***************************************************************************
                              lazarusmanager.pas
                             --------------------
               Class to manage starting and restarting of lazarus

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
(*
 Abstract:
   This is the worker unit of the 'startlazarus' application.

   It waits for running lazarus, if its pid was passed as command line
   parameter.
   It searches the new lazarus executable.
     1. open the build lazarus options and look for a custom target directory
     2. look in the directory of startlazarus (the lazarus main directory)
        and in $(ConfigDir)/bin/ and use the newest lazarus executable.
   On systems which lock executables on run it renames the
     lazarus to lazarus.old and lazarus.new to lazarus.
   Then it starts lazarus and waits until it is finished. If lazarus gives a
   special exit code (ExitCodeRestartLazarus), it goes to step 1.
   Any other exit code, also stops startlazarus.

    
  Why that?
   - To install a package into the IDE statically, it must be relinked.
     This creates a new lazarus[.exe] executable. With the help of startlazarus
     the IDE can then automatically restart itself.
   - Some platforms like windows locks there executable while running.
     This means the new executable created needs another name.
   - If the IDE is installed as root/administrator the whole lazarus directory
     is readonly for users. This means the new executable created will be
     created in another directory.
   - Building can result in a broken IDE. Therefore backups are created.
   - Copying is slow (especially the huge IDE). So only 'rename' is used for
     backup.
   - The IDE calls 'make' to rebuild itself. This deletes the old lazarus
     executable on some systems. So, the backup must be made before building
     for these systems.
   - When the original directory can't be used (readonly), the build directory
     is <primary config path>/bin/, which results in ~/.lazarus/bin/ on unix
     style systems like linux, bsd, macosx and {AppData}\Lazarus\bin on windows.
   - For debugging purposes you can work without startlazarus.
   - To not confuse the user, the running IDE executable is 'lazarus' or
     'lazarus.exe', not 'lazarus.new' or 'lazarus.old'.
   - The user can define the Target Directory.
   - The IDE can be cross compiled. The resulting executable will be created
     in <primary config path>/bin/<TargetOS>
*)
unit LazarusManager;

{$mode objfpc}{$H+}

interface

uses
{$IFdef MSWindows}
  Windows,
{$ENDIF}
{$IFDEF unix}
  BaseUnix,
{$ENDIF}
  Classes, SysUtils, Process,
  LCLProc, FileUtil, Forms, Controls, Dialogs,
  LazConf, Splash;
  
type
  TLazarusProcess = class
  private
    FOnStart: TNotifyEvent;
    FProcess: TProcess;
    FWantsRestart: boolean;
  public
    constructor Create(const LazarusPath: string; const CommandLine: string);
    destructor Destroy; override;
    procedure Execute;
    procedure WaitOnExit;
    property WantsRestart: boolean read FWantsRestart;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;
  
type

  { TLazarusManager }

  TLazarusManager = class(TComponent)
  private
    FLazarusProcess: TLazarusProcess;
    FLazarusPath: string;
    FLazarusPID: Integer;
    FLazarusDebug: Boolean;
    FCmdLineParams: TStrings;
    procedure ParseCommandLine;
    function GetCommandLineParameters: string;
    function GetLazarusPath(const FileName: string): string;
    function RenameLazarusExecutables: TModalResult;
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
  SplashForm := nil;
  ShowSplash;
  ParseCommandLine;
end;

destructor TLazarusManager.Destroy;
begin
  FreeAndNil(FCmdLineParams);
  inherited Destroy;
end;

procedure TLazarusManager.ParseCommandLine;
const
  LazarusPidOpt='--lazarus-pid=';
  LazarusDebugOpt ='--debug';
var
  i: Integer;
  Param: string;
begin
  FCmdLineParams := TStringList.Create;
  FLazarusPID := 0;
  FLazarusDebug := false;
  for i := 1 to ParamCount do begin
    Param := ParamStr(i);
    if LeftStr(Param,length(LazarusDebugOpt))=LazarusDebugOpt then 
      FLazarusDebug := true;
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

function TLazarusManager.GetCommandLineParameters: string;
var
  i: Integer;
begin
  Result := ' --no-splash-screen --started-by-startlazarus';
  for i := 0 to FCmdLineParams.Count - 1 do
    Result := Result + ' ' + FCmdLineParams[i];
  if FLazarusDebug then
   Result := Result + ' --debug-log=' +
                        AppendPathDelim(GetPrimaryConfigPath) + 'debug.log';
end;

function TLazarusManager.GetLazarusPath(const FileName: string) : string;
begin
  // first try in the bin dir of the primary config directory
  Result := AppendPathDelim(GetPrimaryConfigPath) + 'bin' + PathDelim +
    FileName + GetExeExt;
  // if no lazarus executable exists in that directory, try the same directory
  // as the startlazarus executable
  if not FileExists(Result) then
    Result := AppendPathDelim(ExtractFilePath(ExpandFileName(ParamStr(0)))) +
      FileName + GetExeExt;
end;

function TLazarusManager.RenameLazarusExecutables: TModalResult;
var
  NewFileName: string;
  BackupFileName: String;
begin
  Result := mrOK;
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
  if not FileExists(FLazarusPath) then begin
    MessageDlg(format('Can''t find lazarus executable: %s', [FLazarusPath]),
      mtError, [mbOK], 0);
    Result := mrAbort;
  end;
end;

procedure TLazarusManager.LazarusProcessStart(Sender: TObject);
begin
  if SplashForm<>nil then SplashForm.Hide;
  FreeThenNil(SplashForm);
  Application.ProcessMessages;
end;

procedure TLazarusManager.WaitForLazarus;
  procedure WaitForPid(PID: integer);
  {$IFdef MSWindows}
  var
    ProcessHandle: THandle;
  begin
    ProcessHandle := OpenProcess(SYNCHRONIZE, false, PID);
    if ProcessHandle<>0 then begin
      WaitForSingleObject(ProcessHandle, INFINITE);
      CloseHandle(ProcessHandle);
    end;
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
  if FLazarusPID<>0 then
    WaitForPID(FLazarusPID);
end;

procedure TLazarusManager.Run;
var
  Restart: boolean;
begin
  WaitForLazarus;
  repeat
    ShowSplash;
    Restart := false;
    if RenameLazarusExecutables=mrOK then begin
      FLazarusProcess :=
        TLazarusProcess.Create(FLazarusPath, GetCommandLineParameters);
      FLazarusProcess.OnStart := @LazarusProcessStart;
      FLazarusProcess.Execute;
      FLazarusProcess.WaitOnExit;
      Restart := FLazarusProcess.WantsRestart;
      FreeAndNil(FLazarusProcess);
    end;
  until not Restart;
  Application.Terminate;
end;

procedure TLazarusManager.ShowSplash;
begin
  if SplashForm=nil then SplashForm := TSplashForm.Create(Self);
  with SplashForm do begin
    Show;
    Paint;
  end;
  Application.ProcessMessages; // process splash paint message
end;

{ TLazarusProcess }

constructor TLazarusProcess.Create(const LazarusPath: string;
  const CommandLine: string);
begin
  FProcess := TProcess.Create(nil);
  FProcess.Options := [];
  FProcess.ShowWindow := swoShow;
  FProcess.CommandLine := LazarusPath + CommandLine;
end;

destructor TLazarusProcess.Destroy;
begin
  FreeAndNil(FProcess);
  inherited Destroy;
end;

procedure TLazarusProcess.Execute;
begin
  FProcess.Execute;
  Sleep(2000);
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TLazarusProcess.WaitOnExit;
begin
  FProcess.WaitOnExit;
  FWantsRestart := FProcess.ExitStatus=ExitCodeRestartLazarus;
end;

end.

