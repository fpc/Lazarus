{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Initial Revision  : Tue Dec 06 09:00:00 CET 2005
}

unit UTF8Process;

{$mode objfpc}{$H+}

{$IF defined(UNIX) and (FPC_FULLVERSION<30301)}
  // FPC 3.2.x TProcess.Execute converts CurrentDirectory after fpFork, which
  // can deadlock if another thread was converting strings (iconv lock)
  {$DEFINE ForkChDirWorkaround}
{$ENDIF}

interface

uses
  Classes, SysUtils, Process,
  {$IFDEF ForkChDirWorkaround}
  BaseUnix,
  {$ENDIF}
  // LazUtils
  FileUtil, LazFileUtils, LazUtilsStrConsts;

{ TProcessUTF8 }

type
  TProcessUTF8 = class(TProcess)
  {$IFDEF ForkChDirWorkaround}
  private
    FSysCurrentDir: RawByteString;
    FOrigForkEvent: TProcessForkEvent;
    procedure ChDirForkEvent(Sender: TObject);
  {$ENDIF}
  public
    {$IFDEF ForkChDirWorkaround}
    procedure Execute; override;
    {$ENDIF}
    procedure ParseCmdLine(const CmdLine: string; ReadBackslash: boolean = false);
  end;

// poWaitOnExit prevents a zombie process but locks the calling program until the process
// terminates. When runnning a GUI application you may want to use [] as ProcessOpts.
procedure RunCmdFromPath(const ProgramFilename, CmdLineParameters: string;
  ProcessOpts: TProcessOptions = [poWaitOnExit]);

function FindFilenameOfCmd(ProgramFilename: string): string;

function GetSystemThreadCount: integer; // guess number of cores

procedure Register;

implementation

{$IF defined(windows)}
uses Windows;
{$ELSEIF defined(freebsd) or defined(darwin)}
uses ctypes, sysctl;
{$ELSEIF defined(linux)}
{$linklib c}
uses ctypes;
{$ENDIF}

{$IFDEF Linux}
const
  _SC_NPROCESSORS_CONF = 83;
  _SC_NPROCESSORS_ONLN = 84;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

function GetSystemThreadCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  SystemInfo: SYSTEM_INFO;
  {$IFnDEF WinCE}
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  {$ENDIF}
begin
  {$IFnDEF WinCE}
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask{%H-}, SystemAffinityMask{%H-})
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
    exit;
  end;
  {$ENDIF}
  //can't get the affinity mask so we just report the total number of processors
  GetSystemInfo(SystemInfo{%H-});
  Result := SystemInfo.dwNumberOfProcessors;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(@mib, 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_CONF);
  end;

{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}

function FindFilenameOfCmd(ProgramFilename: string): string;
begin
  Result:=TrimFilename(ProgramFilename);
  if not FilenameIsAbsolute(Result) then begin
    if Pos(PathDelim,Result)>0 then begin
      // with sub directory => relative to current directory
      Result:=CleanAndExpandFilename(Result);
    end else begin
      // search in PATH
      Result:=FindDefaultExecutablePath(Result);
    end;
  end;
  if (Result<>'') and not FileExistsUTF8(Result) then
    Result:='';
end;

// Runs a short command which should point to an executable in the environment PATH
// For example: ProgramFilename='ls' CmdLineParameters='-l /home'
// Will locate and execute the file '/bin/ls'
// If the command isn't found, an exception will be raised
procedure RunCmdFromPath(const ProgramFilename, CmdLineParameters: string;
  ProcessOpts: TProcessOptions);
var
  NewProgramFilename: String;
  BrowserProcess: TProcessUTF8;
begin
  NewProgramFilename:=FindFilenameOfCmd(ProgramFilename);

  if NewProgramFilename='' then
    raise EFOpenError.Create(Format(lrsProgramFileNotFound, [ProgramFilename]));
  if not FileIsExecutable(NewProgramFilename) then
    raise EFOpenError.Create(Format(lrsCanNotExecute, [NewProgramFilename]));

  // run
  BrowserProcess := TProcessUTF8.Create(nil);
  try
    BrowserProcess.InheritHandles:=false;
    BrowserProcess.Options := ProcessOpts;
    // Encloses the executable with "" if its name has spaces
    if Pos(' ',NewProgramFilename)>0 then
      NewProgramFilename:='"'+NewProgramFilename+'"';

    {$Push}
    {$WARN SYMBOL_DEPRECATED OFF}
    BrowserProcess.CommandLine := NewProgramFilename;
    if CmdLineParameters<>'' then
      BrowserProcess.CommandLine := BrowserProcess.CommandLine + ' ' + CmdLineParameters;
    {$Pop}
    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('System',[TProcessUTF8]);
end;

{$IFDEF ForkChDirWorkaround}
procedure TProcessUTF8.ChDirForkEvent(Sender: TObject);
begin
  // Note: this runs in the forked child, where only the forking thread exists.
  // Do not convert strings or allocate memory here, the locks may be held by
  // threads which do not exist in the child.
  if FpChdir(PAnsiChar(FSysCurrentDir))<>0 then
    FpExit(127);
  if Assigned(FOrigForkEvent) then
    FOrigForkEvent(Sender);
end;

procedure TProcessUTF8.Execute;
var
  OldCurrentDir: TProcessString;
begin
  if CurrentDirectory='' then begin
    inherited Execute;
    exit;
  end;
  // convert the directory before forking and change to it in the child
  // via OnForkEvent without string conversion
  OldCurrentDir:=CurrentDirectory;
  FSysCurrentDir:=ToSingleByteFileSystemEncodedFileName(OldCurrentDir);
  FOrigForkEvent:=OnForkEvent;
  CurrentDirectory:='';
  OnForkEvent:=@ChDirForkEvent;
  try
    inherited Execute;
  finally
    CurrentDirectory:=OldCurrentDir;
    OnForkEvent:=FOrigForkEvent;
    FOrigForkEvent:=nil;
    FSysCurrentDir:='';
  end;
end;
{$ENDIF}

procedure TProcessUTF8.ParseCmdLine(const CmdLine: string; ReadBackslash: boolean);
var
  List: TStringList;
begin
  List:=TStringList.Create;
  try
    SplitCmdLineParams(CmdLine, List, ReadBackslash);
    if List.Count>0 then begin
      Executable:=List[0];
      List.Delete(0);
    end else begin
      Executable:='';
    end;
    Parameters.Assign(List);
  finally
    List.Free;
  end;
end;

end.
