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
     style systems like linux, bsd, macosx and {AppData}\Lazarus\bin on windows
     (this is still a todo on windows).
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
  Classes, SysUtils, Process, UTF8Process,
  LCLProc, FileProcs, FileUtil, Forms, Controls, Dialogs,
  IDECmdLine, LazConf, Splash;
  
type
  TLazarusProcess = class
  private
    FOnStart: TNotifyEvent;
    FProcess: TProcessUTF8;
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
    FCmdLineParams: TStrings;
    FCmdLineFiles: string;
    FShowSplashOption: boolean;
    function RenameLazarusExecutable(const Directory: string): TModalResult;
    procedure LazarusProcessStart(Sender: TObject);
    procedure WaitForLazarus;
  public
    destructor Destroy; override;
    procedure Initialize;
    procedure Run;
    procedure ShowSplash;
    property ShowSplashOption: boolean read FShowSplashOption write FShowSplashOption;
  end;

implementation

destructor TLazarusManager.Destroy;
begin
  FreeAndNil(FCmdLineParams);
  inherited Destroy;
end;

function TLazarusManager.RenameLazarusExecutable(const Directory: string
  ): TModalResult;
var
  NewFilename: String;
  BackupFilename: String;
  CurFilename: String;
begin
  NewFilename:=AppendPathDelim(Directory)+'lazarus.new'+GetExeExt;
  BackupFilename:=AppendPathDelim(Directory)+'lazarus.old'+GetExeExt;
  CurFilename:=AppendPathDelim(Directory)+'lazarus'+GetExeExt;
  if FileExistsUTF8(NewFileName) then
  begin
    if FileExistsUTF8(CurFilename) then
    begin
      if FileExistsUTF8(BackupFileName) then
        if not DeleteFileUTF8(BackupFileName) then begin
          MessageDlg(format('Can''t delete "%s"'#13'%s',
            [BackupFileName, SysErrorMessageUTF8(GetLastOSError)]),
            mtError, [mbOK], 0);
          Result := mrAbort;
          exit;
        end;
      if not FileProcs.RenameFileUTF8(CurFilename, BackupFileName) then begin
        MessageDlg(format('Can''t rename "%s" to "%s"'#13'%s',
          [CurFilename, BackupFileName, SysErrorMessageUTF8(GetLastOSError)]),
          mtError, [mbOK], 0);
        Result := mrAbort;
        exit;
      end;
      InvalidateFileStateCache;
    end;
    if not FileProcs.RenameFileUTF8(NewFileName, CurFilename) then begin
      MessageDlg(format('Can''t rename "%s" to "%s"'#13'%s',
        [NewFileName, CurFilename, SysErrorMessageUTF8(GetLastOSError)]),
        mtError, [mbOK], 0);
      Result := mrAbort;
      exit;
    end;
    InvalidateFileStateCache;
  end;
  Result:=mrOk;
end;

procedure TLazarusManager.LazarusProcessStart(Sender: TObject);
begin
  if SplashForm<>nil then SplashForm.Hide;
  FreeThenNil(SplashForm);
  Application.ProcessMessages;
end;

procedure TLazarusManager.WaitForLazarus;
  procedure WaitForPid(PID: integer);
  {$IFDEF WINDOWS}
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

procedure TLazarusManager.Initialize;
var
  CmdLineFiles: TStrings;
  i: integer;
begin
  FShowSplashOption:=true;
  SplashForm := nil;
  FCmdLineParams := TStringList.Create;
  ParseCommandLine(FCmdLineParams, FLazarusPID, FShowSplashOption);
  if FShowSplashOption then
    ShowSplash;
  CmdLineFiles := ExtractCmdLineFilenames;
  if CmdLineFiles<>nil then
  begin
    for i := 0 to CmdLineFiles.Count-1 do
      if pos(' ',CmdLineFiles[i])>0 then
        CmdLineFiles[i] := '"' + CmdLineFiles[i] + '"';
    CmdLineFiles.Delimiter:=' ';
    FCmdLineFiles:=CmdLineFiles.DelimitedText;
  end;
end;

procedure TLazarusManager.Run;
var
  Restart: boolean;
  DefaultDir: String;
  CustomDir: String;
  DefaultExe: String;
  CustomExe: String;
  MsgResult: TModalResult;
  StartPath: String;
begin
  WaitForLazarus;
  try
    StartPath:=ExpandFileNameUTF8(ParamStrUTF8(0));
    if FileIsSymlink(StartPath) then
      StartPath:=ReadAllLinks(StartPath,true);
    DefaultDir:=ExtractFilePath(StartPath);
    if DirectoryExistsUTF8(DefaultDir) then
      DefaultDir:=ReadAllLinks(DefaultDir,true);
  except
    on E: Exception do begin
      MessageDlg('Error',E.Message,mtError,[mbCancel],0);
      exit;
    end;
  end;
  DefaultDir:=AppendPathDelim(DefaultDir);
  CustomDir:=AppendPathDelim(GetPrimaryConfigPath) + 'bin' + PathDelim;

  repeat
    Restart := false;
    if FShowSplashOption then
      ShowSplash;
    { There are four places where the newest lazarus exe can be:
      1. in the same directory as the startlazarus exe
      1.1 as lazarus.new(.exe) (if the executable was write locked (windows))
      1.2 as lazarus(.exe) (if the executable was writable (non windows))
      2. in the config directory (e.g. ~/.lazarus/bin/)
      2.1 as lazarus.new(.exe) (if the executable was write locked (windows))
      2.2 as lazarus(.exe) (if the executable was writable (non windows))
    }
    if (RenameLazarusExecutable(DefaultDir)=mrOK)
      and (RenameLazarusExecutable(CustomDir)=mrOK) then
    begin
      DefaultExe:=DefaultDir+'lazarus'+GetExeExt;
      CustomExe:=CustomDir+'lazarus'+GetExeExt;
      if FileExistsUTF8(DefaultExe) then begin
        if FileExistsUTF8(CustomExe) then begin
          // both exist
          if (FileAgeUTF8(CustomExe)>=FileAgeUTF8(DefaultExe)) then begin
            // the custom exe is newer or equal => use custom
            // Equal files ages catches the case where the two names refer to the same file on disk
            FLazarusPath:=CustomExe;
          end else begin
            // the custom exe is older => let user choose
            MsgResult:=QuestionDlg('Multiple lazarus found',
              'Which Lazarus should be started?'#13
              +#13
              +'The system default executable'#13
              +DefaultExe+#13
              +'(date: '+DateTimeToStr(FileDateToDateTimeDef(FileAgeUTF8(DefaultExe)))+')'#13
              +#13
              +'Or your custom executable'#13
              +CustomExe+#13
              +'(date: '+DateTimeToStr(FileDateToDateTimeDef(FileAgeUTF8(CustomExe)))+')'#13
              ,mtConfirmation,
              [mrYes,'Start system default',mrNo,'Start my custom',mrAbort],0
              );
            case MsgResult of
            mrYes: FLazarusPath:=DefaultExe;
            mrNo: FLazarusPath:=CustomExe;
            else break;
            end;
          end;
        end else begin
          // only the default exists => use default
          FLazarusPath:=DefaultExe;
        end;
      end else begin
        if FileExistsUTF8(CustomExe) then begin
          // only the custom exists => warn user
          MessageDlg('System default is missing',
            'The system default lazarus executable "'+DefaultExe+'" is missing, but your custom'
            +'executable is still there:'#13
            +CustomExe+#13
            +'This will be started ...'
            ,mtInformation,[mbOk],0);
          FLazarusPath:=CustomExe;
        end else begin
          // no exe exists
          MessageDlg('File not found','Can''t find the lazarus executable '+DefaultExe,
            mtError,[mbAbort],0);
          break;
        end;
      end;
      {$IFDEF darwin}
      if FileExistsUTF8(FLazarusPath+'.app') then begin
        // start the bundle instead
        FLazarusPath:=FLazarusPath+'.app/Contents/MacOS/'+ExtractFileName(FLazarusPath);
      end;
      {$ENDIF}

      DebugLn(['TLazarusManager.Run starting ',FLazarusPath,' ...']);
      FLazarusProcess :=
        TLazarusProcess.Create(FLazarusPath,
             GetCommandLineParameters(FCmdLineParams, True)+' '+FCmdLineFiles);
      // clear the command line files, so that they are passed only once.
      FCmdLineFiles:='';
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
  with SplashForm do 
  begin
    Show;
    Update;
  end;
  Application.ProcessMessages; // process splash paint message
end;

{ TLazarusProcess }

constructor TLazarusProcess.Create(const LazarusPath: string;
  const CommandLine: string);
begin
  FProcess := TProcessUTF8.Create(nil);
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

