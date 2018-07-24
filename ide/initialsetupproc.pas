{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Procedures and dialogs to check environment. The IDE uses these procedures
    at startup to check for example the lazarus directory and warns if it looks
    suspicious and choose another.
}
unit InitialSetupProc;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL + LCL
  Classes, SysUtils, strutils, contnrs,
  // CodeTools
  DefineTemplates, CodeToolManager, FileProcs,
  // LazUtils
  LazFileCache, LazUTF8, LazUTF8Classes, LazFileUtils, FileUtil,
  LazLoggerBase, Laz2_XMLCfg,
  // IDE
  LazarusIDEStrConsts, LazConf, EnvironmentOpts;

type
  TSDFilenameQuality = (
    sddqInvalid,
    sddqWrongMinorVersion,
    sddqWrongVersion,
    sddqIncomplete,
    sddqCompatible
    );

  TSDFileInfo = class
  public
    Filename: string; // macros resolved, trimmed, expanded
    Caption: string; // filename with macros
    Note: string;
    Quality: TSDFilenameQuality;
  end;

  TSDFileInfoList = class (TObjectList)
  public
    function AddNewItem(aFilename, aCaption: string): TSDFileInfo;
    function CaptionExists(aCaption: string): boolean;
    function BestDir: TSDFileInfo;
  end;

  TSDFilenameType = (
    sddtLazarusSrcDir,
    sddtCompilerFilename,
    sddtFPCSrcDir,
    sddtMakeExeFilename,
    sddtDebuggerFilename
    );

  TSDFlag = (
    sdfCompilerFilenameNeedsUpdate,
    sdfFPCSrcDirNeedsUpdate,
    sdfMakeExeFilenameNeedsUpdate,
    sdfDebuggerFilenameNeedsUpdate
    );
  TSDFlags = set of TSDFlag;

// Lazarus Directory
function CheckLazarusDirectoryQuality(ADirectory: string; out Note: string): TSDFilenameQuality;
function SearchLazarusDirectoryCandidates(StopIfFits: boolean): TSDFileInfoList;
procedure SetupLazarusDirectory;

// FreePascal Compiler
function CheckFPCExeQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;
function SearchFPCExeCandidates(StopIfFits: boolean;
  const TestSrcFilename: string): TSDFileInfoList;
procedure SetupFPCExeFilename;

// Pas2js compiler
function CheckPas2jsQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;

// FPC Source
function CheckFPCSrcDirQuality(ADirectory: string; out Note: string;
  const FPCVer: String; aUseFileCache: Boolean = True): TSDFilenameQuality;
function SearchFPCSrcDirCandidates(StopIfFits: boolean;
  const FPCVer: string): TSDFileInfoList;

// Make
// Checks a given file to see if it is a valid make executable
function CheckMakeExeQuality(AFilename: string; out Note: string): TSDFilenameQuality;
// Search make candidates and add them to the list, including quality level
function SearchMakeExeCandidates(StopIfFits: boolean): TSDFileInfoList;

function GetValueFromPrimaryConfig(OptionFilename, Path: string): string;
function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
function GetValueFromIDEConfig(OptionFilename, Path: string): string;

function SafeFormat(const Fmt: String; const Args: Array of const): String;

implementation

function CheckLazarusDirectoryQuality(ADirectory: string;
  out Note: string): TSDFilenameQuality;

  function SubDirExists(SubDir: string; var q: TSDFilenameQuality): boolean;
  begin
    SubDir:=GetForcedPathDelims(SubDir);
    if DirPathExistsCached(ADirectory+SubDir) then exit(true);
    Result:=false;
    Note:=Format(lisDirectoryNotFound2, [SubDir]);
    q:=sddqIncomplete;
  end;

  function SubFileExists(SubFile: string; var q: TSDFilenameQuality): boolean;
  begin
    SubFile:=GetForcedPathDelims(SubFile);
    if FileExistsCached(ADirectory+SubFile) then exit(true);
    Result:=false;
    Note:=Format(lisFileNotFound3, [SubFile]);
    q:=sddqIncomplete;
  end;

var
  sl: TStringListUTF8;
  VersionIncFile: String;
  Version: String;
begin
  Result:=sddqInvalid;
  ADirectory:=TrimFilename(ADirectory);
  if not DirPathExistsCached(ADirectory) then
  begin
    Note:=lisISDDirectoryNotFound;
    exit;
  end;
  ADirectory:=AppendPathDelim(ADirectory);
  if not SubDirExists('lcl',Result) then exit;
  if not SubDirExists('packager/globallinks',Result) then exit;
  if not SubDirExists('ide',Result) then exit;
  if not SubDirExists('components',Result) then exit;
  if not SubFileExists('ide/lazarus.lpi',Result) then exit;
  VersionIncFile:=GetForcedPathDelims('ide/version.inc');
  if not SubFileExists(VersionIncFile,Result) then exit;
  sl:=TStringListUTF8.Create;
  try
    try
      sl.LoadFromFile(ADirectory+VersionIncFile);
      if (sl.Count=0) or (sl[0]='') or (sl[0][1]<>'''') then
      begin
        Note:=Format(lisInvalidVersionIn, [VersionIncFile]);
        exit;
      end;
      Version:=copy(sl[0],2,length(sl[0])-2);
      if Version<>LazarusVersionStr then
      begin
        Note:=Format(lisWrongVersionIn, [VersionIncFile, Version]);
        Result:=sddqWrongVersion;
        exit;
      end;
      Note:=lisOk;
      Result:=sddqCompatible;
    except
      on E: Exception do begin
        Note:=Format(lisUnableToLoadFile2, [VersionIncFile, E.Message]);
        exit;
      end;
    end;
  finally
    sl.Free;
  end;
end;

function SearchLazarusDirectoryCandidates(StopIfFits: boolean): TSDFileInfoList;

  function CheckDir(Dir: string; var List: TSDFileInfoList): boolean;
  var
    Item: TSDFileInfo;
    RealDir: String;
  begin
    Result:=false;
    if Dir='' then Dir:='.';
    ForcePathDelims(Dir);
    Dir:=ChompPathDelim(Dir);
    // check if already checked
    if Assigned(List) and List.CaptionExists(Dir) then exit;
    EnvironmentOptions.LazarusDirectory:=Dir;
    RealDir:=ChompPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
    DebugLn(['SearchLazarusDirectoryCandidates Value=',Dir,' File=',RealDir]);
    // check if exists
    if not DirPathExistsCached(RealDir) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.create(true);
    Item:=List.AddNewItem(RealDir, Dir);
    Item.Quality:=CheckLazarusDirectoryQuality(RealDir, Item.Note);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

  function CheckViaExe(Filename: string; var List: TSDFileInfoList): boolean;
  begin
    Result:=false;
    Filename:=FindDefaultExecutablePath(Filename);
    if Filename='' then exit;
    Filename:=GetPhysicalFilenameCached(Filename,true);
    if Filename='' then exit;
    Result:=CheckDir(ExtractFilePath(Filename),List);
  end;

var
  Dir: String;
  ResolvedDir: String;
  Dirs: TStringList;
  i: Integer;
  OldLazarusDir: String;
begin
  Result:=nil;

  OldLazarusDir:=EnvironmentOptions.LazarusDirectory;
  try
    // first check the value in the options
    if CheckDir(EnvironmentOptions.LazarusDirectory,Result) then exit;

    // then check the directory of the executable
    Dir:=ProgramDirectoryWithBundle;
    if CheckDir(Dir,Result) then exit;
    ResolvedDir:=GetPhysicalFilenameCached(Dir,false);
    if (ResolvedDir<>Dir) and (CheckDir(ResolvedDir,Result)) then exit;

    // check the primary options
    Dir:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                     'EnvironmentOptions/LazarusDirectory/Value');
    if CheckDir(Dir,Result) then exit;

    // check the secondary options
    Dir:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                     'EnvironmentOptions/LazarusDirectory/Value');
    if CheckDir(Dir,Result) then exit;

    // check common directories
    Dirs:=GetDefaultLazarusSrcDirectories;
    try
      for i:=0 to Dirs.Count-1 do
        if CheckDir(Dirs[i],Result) then exit;
    finally
      Dirs.Free;
    end;

    // check history
    Dirs:=EnvironmentOptions.LazarusDirHistory;
    if Dirs<>nil then
      for i:=0 to Dirs.Count-1 do
        if CheckDir(Dirs[i],Result) then exit;

    // search lazarus-ide and lazarus in PATH, then follow the links,
    // which will lead to the lazarus directory
    if CheckViaExe('lazarus-ide'+GetExecutableExt,Result) then exit;
    if CheckViaExe('lazarus'+GetExecutableExt,Result) then exit;

  finally
    EnvironmentOptions.LazarusDirectory:=OldLazarusDir;
  end;
end;

procedure SetupLazarusDirectory;
var
  Dir, Note: String;
  Quality: TSDFilenameQuality;
  List: TSDFileInfoList;
begin
  Dir:=EnvironmentOptions.GetParsedLazarusDirectory;
  Quality:=CheckLazarusDirectoryQuality(Dir,Note);
  if Quality<>sddqInvalid then exit;
  // bad lazarus directory => searching a good one
  dbgout('SetupLazarusDirectory:');
  if EnvironmentOptions.LazarusDirectory<>'' then
  begin
    dbgout(' The Lazarus directory "',EnvironmentOptions.LazarusDirectory,'"');
    if EnvironmentOptions.LazarusDirectory<>Dir then
      dbgout(' => "',Dir,'"');
    dbgout(' is invalid (Error: ',Note,')');
    debugln(' Searching a proper one ...');
  end else begin
    debugln(' Searching ...');
  end;
  List:=SearchLazarusDirectoryCandidates(true);
  try
    if (List=nil) or (List.BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupLazarusDirectory: no proper Lazarus directory found.']);
      exit;
    end;
    EnvironmentOptions.LazarusDirectory:=List.BestDir.Filename;
    debugln(['SetupLazarusDirectory: using ',EnvironmentOptions.LazarusDirectory]);
  finally
    List.Free;
  end;
end;

function CheckFPCExeQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;
var
  CfgCache: TPCTargetConfigCache;

  function CheckPPU(const AnUnitName: string): boolean;
  begin
    if (CfgCache.Units=nil)
    or (CompareFileExt(CfgCache.Units[AnUnitName],'ppu',false)<>0) then
    begin
      Note:=Format(lisPpuNotFoundCheckYourFpcCfg, [AnUnitName]);
      Result:=false;
    end else
      Result:=true;
  end;

var
  i: LongInt;
  ShortFilename: String;
begin
  Result:=sddqInvalid;
  AFilename:=TrimFilename(AFilename);
  if not FileExistsCached(AFilename) then
  begin
    Note:=lisFileNotFound4;
    exit;
  end;
  if DirPathExistsCached(AFilename) then
  begin
    Note:=lisFileIsDirectory;
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then
  begin
    Note:=lisFileIsNotAnExecutable;
    exit;
  end;

  // do not execute unusual exe files
  ShortFilename:=ExtractFileNameOnly(AFilename);
  if (CompareFilenames(ShortFilename,'fpc')<>0)
  and (CompareFilenames(LeftStr(ShortFilename,3),'ppc')<>0)
  then begin
    Note:=lisUnusualCompilerFileNameUsuallyItStartsWithFpcPpcOr;
    exit(sddqIncomplete);
  end;

  if TestSrcFilename<>'' then
  begin
    CfgCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                                       AFilename,'','','',true);
    if CfgCache.NeedsUpdate then
      CfgCache.Update(TestSrcFilename);
    i:=CfgCache.IndexOfUsedCfgFile;
    if i<0 then
    begin
      Note:=SafeFormat(lisCompilerCfgIsMissing,['fpc.cfg']);
      exit;
    end;
    if not CfgCache.HasPPUs then
    begin
      Note:=lisSystemPpuNotFoundCheckYourFpcCfg;
      exit;
    end;
    if (CfgCache.RealTargetCPU='jvm') then
    begin
      if not CheckPPU('uuchar') then exit;
    end else begin
      if not CheckPPU('classes') then exit;
    end;
  end;

  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchFPCExeCandidates(StopIfFits: boolean;
  const TestSrcFilename: string): TSDFileInfoList;
var
  ShortCompFile: String;

  function CheckFile(AFilename: string; var List: TSDFileInfoList): boolean;
  var
    Item: TSDFileInfo;
    RealFilename: String;
  begin
    Result:=false;
    if AFilename='' then exit;
    ForcePathDelims(AFilename);
    // check if already checked
    if Assigned(List) and List.CaptionExists(AFilename) then exit;
    EnvironmentOptions.CompilerFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    debugln(['SearchCompilerCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.create(true);
    Item:=List.AddNewItem(RealFilename, AFilename);
    Item.Quality:=CheckFPCExeQuality(RealFilename, Item.Note, TestSrcFilename);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

  function CheckSubDirs(ADir: string; var List: TSDFileInfoList): boolean;
  // search for ADir\bin\i386-win32\fpc.exe
  // and for ADir\*\bin\i386-win32\fpc.exe
  var
    FileInfo: TSearchRec;
    SubFile: String;
  begin
    Result:=true;
    ADir:=AppendPathDelim(TrimFilename(ExpandFileNameUTF8(TrimFilename(ADir))));
    SubFile:='bin/$(TargetCPU)-$(TargetOS)/'+ShortCompFile;
    if CheckFile(ADir+SubFile,List) then
      exit;
    try
      if FindFirstUTF8(ADir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
            continue;
          if ((FileInfo.Attr and faDirectory)>0)
          and CheckFile(ADir+FileInfo.Name+PathDelim+SubFile,List) then
            exit;
        until FindNextUTF8(FileInfo)<>0;
      end;
    finally
      FindCloseUTF8(FileInfo);
    end;
    Result:=false;
  end;

var
  AFilename: String;
  Files: TStringList;
  i: Integer;
  SysDrive: String;
  ProgDir: String;
  OldCompilerFilename: String;
begin
  Result:=nil;

  OldCompilerFilename:=EnvironmentOptions.CompilerFilename;
  try
    // check current setting
    if CheckFile(EnvironmentOptions.CompilerFilename,Result) then exit;

    // check the primary options
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/CompilerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // check the secondary options
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/CompilerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // check environment variable PP
    AFileName := GetEnvironmentVariableUTF8('PP');
    if CheckFile(AFilename,Result) then exit;

    // search fpc(.exe) in PATH
    if CheckFile('fpc'+ExeExt,Result) then exit;

    // search ppccpu(.exe) in PATH
    if CheckFile(GetDefaultCompilerFilename(GetCompiledTargetCPU),Result) then exit;

    // check history
    Files:=EnvironmentOptions.CompilerFileHistory;
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check paths with versions
    ShortCompFile:='fpc'+ExeExt;

    // check $(LazarusDir)\fpc\3.0.0\bin\i386-win32\fpc.exe
    if CheckFile(GetForcedPathDelims('$(LazarusDir)/fpc/'+{$I %FPCVERSION%}+'/bin/'+GetCompiledTargetCPU+'-'+GetCompiledTargetOS+'/')+ShortCompFile,Result)
      then exit;

    // check $(LazarusDir)\fpc\bin\i386-win32\fpc.exe
    if CheckFile(GetForcedPathDelims('$(LazarusDir)/fpc/bin/'+GetCompiledTargetCPU+'-'+GetCompiledTargetOS+'/')+ShortCompFile,Result)
      then exit;

    // check common directories
    Files:=TStringList.Create;
    try
      GetDefaultCompilerFilenames(Files);
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;
    finally
      Files.Free;
    end;

    // Windows-only locations:
    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      SysDrive:=GetEnvironmentVariableUTF8('SYSTEMDRIVE');
      if SysDrive='' then SysDrive:='C:';
      SysDrive:=AppendPathDelim(SysDrive);
      // %SYSTEMDRIVE%\fpc\
      if CheckSubDirs(SysDrive+'FPC',Result) then exit;
      // %SYSTEMDRIVE%\pp\
      if CheckSubDirs(SysDrive+'pp',Result) then exit;
      // %PROGRAMFILES%\FPC\*
      ProgDir:=AppendPathDelim(GetEnvironmentVariableUTF8('PROGRAMFILES'));
      if (ProgDir<>'')
      and CheckSubDirs(ProgDir+'FPC',Result) then exit;
    end;

  finally
    EnvironmentOptions.CompilerFilename:=OldCompilerFilename;
  end;
end;

procedure SetupFPCExeFilename;
var
  Filename, Note: String;
  Quality: TSDFilenameQuality;
  List: TSDFileInfoList;
begin
  Filename:=EnvironmentOptions.GetParsedCompilerFilename;
  Quality:=CheckFPCExeQuality(Filename,Note,'');
  if Quality<>sddqInvalid then exit;
  // bad compiler
  dbgout('SetupCompilerFilename:');
  if EnvironmentOptions.CompilerFilename<>'' then
  begin
    dbgout(' The compiler path "',EnvironmentOptions.CompilerFilename,'"');
    if EnvironmentOptions.CompilerFilename<>Filename then
      dbgout(' => "',Filename,'"');
    dbgout(' is invalid (Error: ',Note,')');
    debugln(' Searching a proper one ...');
  end else begin
    debugln(' Searching compiler ...');
  end;
  List:=SearchFPCExeCandidates(true, CodeToolBoss.CompilerDefinesCache.TestFilename);
  try
    if (List=nil) or (List.BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupCompilerFilename: no proper compiler found.']);
      exit;
    end;
    EnvironmentOptions.CompilerFilename:=List.BestDir.Filename;
    debugln(['SetupCompilerFilename: using ',EnvironmentOptions.CompilerFilename]);
  finally
    List.Free;
  end;
end;

function ValueOfKey(const aLine, aKey: string; var aValue: string): boolean;
// If aKey is found in aLine, separate a quoted number following "aKey =",
//  save it to aValue and return True. Return False if aKey is not found.
// Example line:     version_nr = '2';
var
  i,j: Integer;
begin
  Result:=False;
  i:=Pos(aKey, aLine);
  if i>0 then begin            // aKey found
    i:=PosEx('=', aLine, i+Length(aKey));
    if i>0 then begin          // '=' found
      i:=PosEx('''', aLine, i+1);
      if i>0 then begin        // Opening quote found
        j:=PosEx('''', aLine, i+1);
        if j>0 then begin      // Closing quote found
          aValue:=Copy(aLine, i+1, j-i-1);
          Result:=True;
        end;
      end;
    end;
  end;
end;

function CheckPas2jsQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;
var
  i: LongInt;
  ShortFilename: String;
  CfgCache: TPCTargetConfigCache;
begin
  Result:=sddqInvalid;
  AFilename:=TrimFilename(AFilename);
  if not FileExistsCached(AFilename) then
  begin
    Note:=lisFileNotFound4;
    exit;
  end;
  if DirPathExistsCached(AFilename) then
  begin
    Note:=lisFileIsDirectory;
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then
  begin
    Note:=lisFileIsNotAnExecutable;
    exit;
  end;

  // do not execute unusual exe files
  ShortFilename:=ExtractFileNameOnly(AFilename);
  if (CompareText(LeftStr(ShortFilename,6),'pas2js')<>0)
  then begin
    Note:=lisUnusualPas2jsCompilerFileNameUsuallyItStartsWithPa;
    exit(sddqIncomplete);
  end;

  if TestSrcFilename<>'' then
  begin
    CfgCache:=CodeToolBoss.CompilerDefinesCache.ConfigCaches.Find(
                                                       AFilename,'','','',true);
    if CfgCache.NeedsUpdate then
      CfgCache.Update(TestSrcFilename);
    i:=CfgCache.IndexOfUsedCfgFile;
    if i<0 then
    begin
      Note:=SafeFormat(lisCompilerCfgIsMissing,['pas2js.cfg']);
      exit;
    end;
    //if not CheckPas('classes') then exit;
  end;

  Note:=lisOk;
  Result:=sddqCompatible;
end;

function CheckFPCSrcDirQuality(ADirectory: string; out Note: string;
  const FPCVer: String; aUseFileCache: Boolean = True): TSDFilenameQuality;
// aUseFileCache = False when this function is called from a thread.
// File Cache is not thread safe.

  function DirPathExistsInternal(const FileName: String): Boolean;
  begin
    if aUseFileCache then
      Result:=DirPathExistsCached(FileName)
    else
      Result:=DirPathExists(FileName)
  end;

  function FileExistsInternal(const Filename: string): boolean;
  begin
    if aUseFileCache then
      Result:=FileExistsCached(FileName)
    else
      Result:=FileExistsUTF8(FileName)
  end;

  function SubDirExists(SubDir: string): boolean;
  begin
    SubDir:=GetForcedPathDelims(SubDir);
    if DirPathExistsInternal(ADirectory+SubDir) then exit(true);
    Result:=false;
    Note:=Format(lisDirectoryNotFound2, [SubDir]);
  end;

  function SubFileExists(SubFile: string): boolean;
  begin
    SubFile:=GetForcedPathDelims(SubFile);
    if FileExistsInternal(ADirectory+SubFile) then exit(true);
    Result:=false;
    Note:=Format(lisFileNotFound3, [SubFile]);
  end;

var
  VersionFile: String;
  sl: TStringListUTF8;
  i: Integer;
  VersionNr: String;
  ReleaseNr: String;
  PatchNr: String;
  SrcVer: String;
  Line: String;
begin
  Result:=sddqInvalid;
  Note:='';
  ADirectory:=TrimFilename(ADirectory);
  if not DirPathExistsInternal(ADirectory) then
  begin
    Note:=lisISDDirectoryNotFound;
    exit;
  end;
  ADirectory:=AppendPathDelim(ADirectory);
  if not SubDirExists('rtl') then exit;
  if not SubDirExists('packages') then exit;
  Result:=sddqIncomplete;
  if not SubFileExists('rtl/linux/system.pp') then exit;
  // check version
  if (FPCVer<>'') then
  begin
    VersionFile:=ADirectory+'compiler'+PathDelim+'version.pas';
    if FileExistsInternal(VersionFile) then
    begin
      sl:=TStringListUTF8.Create;
      try
        try
          sl.LoadFromFile(VersionFile);
          VersionNr:='';
          ReleaseNr:='';
          PatchNr:='';
          for i:=0 to sl.Count-1 do
          begin
            Line:=sl[i];
            if ValueOfKey(Line,'version_nr', VersionNr) then begin end
            else if ValueOfKey(Line,'release_nr', ReleaseNr) then begin end
            else if ValueOfKey(Line,'patch_nr',   PatchNr) then
              break;
          end;
          SrcVer:=VersionNr+'.'+ReleaseNr+'.'+PatchNr;
          if SrcVer<>FPCVer then
          begin
            Note:=Format(lisFoundVersionExpected, [SrcVer, FPCVer]);
            SrcVer:=VersionNr+'.'+ReleaseNr+'.';
            if LeftStr(FPCVer,length(SrcVer))=SrcVer then
              Result:=sddqWrongMinorVersion
            else
              Result:=sddqWrongVersion;
            exit;
          end;
        except
        end;
      finally
        sl.Free;
      end;
    end;
  end;
  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchFPCSrcDirCandidates(StopIfFits: boolean; const FPCVer: string): TSDFileInfoList;

  function Check(Dir: string; var List: TSDFileInfoList): boolean;
  var
    Item: TSDFileInfo;
    RealDir: String;
  begin
    Result:=false;
    Dir:=ChompPathDelim(GetForcedPathDelims(Dir));
    if Dir='' then exit;
    // check if already checked
    if Assigned(List) and List.CaptionExists(Dir) then exit;
    EnvironmentOptions.FPCSourceDirectory:=Dir;
    RealDir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
    debugln(['SearchFPCSrcDirCandidates Value=',Dir,' File=',RealDir]);
    if RealDir='' then exit;
    // check if exists
    if not DirPathExistsCached(RealDir) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.Create(true);
    Item:=List.AddNewItem(RealDir, Dir);
    Item.Quality:=CheckFPCSrcDirQuality(RealDir, Item.Note, FPCVer);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

var
  AFilename: String;
  Dirs: TStringList;
  i: Integer;
  OldFPCSrcDir: String;
begin
  Result:=nil;

  OldFPCSrcDir:=EnvironmentOptions.FPCSourceDirectory;
  try
    // check current setting
    if Check(EnvironmentOptions.FPCSourceDirectory,Result) then exit;

    // check the primary options
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                 'EnvironmentOptions/FPCSourceDirectory/Value');
    if Check(AFilename,Result) then exit;

    // check the secondary options
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                 'EnvironmentOptions/FPCSourceDirectory/Value');
    if Check(AFilename,Result) then exit;

    // check environment variable FPCDIR
    AFileName := GetEnvironmentVariableUTF8('FPCDIR');
    if Check(AFilename,Result) then exit;

    // check relative to FPCDIR
    if AFileName <> '' then
      if Check(AFilename + '/../fpcsrc', Result) then exit;

    // check history
    Dirs:=EnvironmentOptions.FPCSourceDirHistory;
    if Dirs<>nil then
      for i:=0 to Dirs.Count-1 do
        if Check(Dirs[i],Result) then exit;

    // $(LazarusDir)/fpc/$(FPCVer)/source
    AFilename:='$(LazarusDir)/fpc/$(FPCVer)/source';
    if Check(AFilename,Result) then exit;

    // check relative to fpc.exe
    AFilename:='$Path($(CompPath))/../../source';
    if Check(AFilename,Result) then exit;

    // check common directories
    Dirs:=GetDefaultFPCSrcDirectories;
    try
      if Dirs<>nil then
        for i:=0 to Dirs.Count-1 do
          if Check(Dirs[i],Result) then exit;
    finally
      Dirs.Free;
    end;
  finally
    EnvironmentOptions.FPCSourceDirectory:=OldFPCSrcDir;
  end;
end;

function CheckMakeExeQuality(AFilename: string; out Note: string
  ): TSDFilenameQuality;
begin
  Result:=sddqInvalid;
  AFilename:=TrimFilename(AFilename);
  if not FileExistsCached(AFilename) then
  begin
    Note:=lisFileNotFound4;
    exit;
  end;
  if DirPathExistsCached(AFilename) then
  begin
    Note:=lisFileIsDirectory;
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then
  begin
    Note:=lisFileIsNotAnExecutable;
    exit;
  end;

  // Windows-only locations:
  if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
    // under Windows, make.exe is in the same directory as fpc.exe
    // other make.exe are often incompatible
    if not FileExistsCached(ExtractFilePath(AFilename)+'fpc.exe') then begin
      Note:=Format(lisThereIsNoFpcExeInTheDirectoryOfUsuallyTheMakeExecu, [
        ExtractFilename(AFilename)]);
      Result:=sddqIncomplete;
      exit;
    end;
  end;

  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchMakeExeCandidates(StopIfFits: boolean): TSDFileInfoList;

  function CheckFile(AFilename: string; var List: TSDFileInfoList): boolean;
  var
    Item: TSDFileInfo;
    RealFilename: String;
  begin
    Result:=false;
    if AFilename='' then exit;
    ForcePathDelims(AFilename);
    // check if already checked
    if Assigned(List) and List.CaptionExists(AFilename) then exit;
    EnvironmentOptions.MakeFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedMakeFilename;
    debugln(['SearchMakeExeCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.create(true);
    Item:=List.AddNewItem(RealFilename, AFilename);
    Item.Quality:=CheckMakeExeQuality(RealFilename, Item.Note);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

var
  OldMakeFilename: String;
  AFilename: String;
  Files: TStringList;
  i: Integer;
begin
  Result:=nil;

  OldMakeFilename:=EnvironmentOptions.MakeFilename;
  try
    // check current setting
    if CheckFile(EnvironmentOptions.MakeFilename,Result) then exit;

    // check the primary options
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/MakeFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // check the secondary options
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/MakeFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // Windows-only locations:
    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // check make in fpc.exe directory
      if CheckFile(GetForcedPathDelims('$Path($(CompPath))\make.exe'),Result)
      then exit;
      // check $(LazarusDir)\fpc\bin\i386-win32\fpc.exe
      if CheckFile(GetForcedPathDelims('$(LazarusDir)\fpc\bin\$(TargetCPU)-$(TargetOS)\make.exe'),Result)
        then exit;
    end;

    // check history
    Files:=EnvironmentOptions.MakeFileHistory;
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check PATH
    {$IFDEF FreeBSD}
    AFilename:='gmake';
    {$ELSE}
    AFilename:='make';
    {$ENDIF}
    AFilename+=GetExecutableExt;
    if CheckFile(FindDefaultExecutablePath(AFilename),Result) then exit;

    // check common directories
    Files:=TStringList.Create;
    try
      GetDefaultMakeFilenames(Files);
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;
    finally
      Files.Free;
    end;
  finally
    EnvironmentOptions.MakeFilename:=OldMakeFilename;
  end;
end;

function GetValueFromPrimaryConfig(OptionFilename, Path: string): string;
begin
  if not FilenameIsAbsolute(OptionFilename) then
    OptionFilename:=AppendPathDelim(GetPrimaryConfigPath)+OptionFilename;
  Result:=GetValueFromIDEConfig(OptionFilename,Path);
end;

function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
begin
  if not FilenameIsAbsolute(OptionFilename) then
    OptionFilename:=AppendPathDelim(GetSecondaryConfigPath)+OptionFilename;
  Result:=GetValueFromIDEConfig(OptionFilename,Path);
end;

function GetValueFromIDEConfig(OptionFilename, Path: string): string;
var
  XMLConfig: TXMLConfig;
begin
  Result:='';
  if FileExistsCached(OptionFilename) then
  begin
    try
      XMLConfig:=TXMLConfig.Create(OptionFilename);
      try
        Result:=XMLConfig.GetValue(Path,'');
      finally
        XMLConfig.Free;
      end;
    except
      on E: Exception do begin
        debugln(['GetValueFromIDEConfig File='+OptionFilename+': '+E.Message]);
      end;
    end;
  end;
end;

function SafeFormat(const Fmt: String; const Args: array of const): String;
begin
  // try with translated resourcestring
  try
    Result:=Format(Fmt,Args);
    exit;
  except
    on E: Exception do
      debugln(['ERROR: SafeFormat: ',E.Message]);
  end;
  // translation didn't work
  // ToDo: find out how to get the resourcestring default value
  //ResetResourceTables;

  // use a safe fallback
  Result:=SimpleFormat(Fmt,Args);
end;

{ TSDFileInfoList }

function TSDFileInfoList.AddNewItem(aFilename, aCaption: string): TSDFileInfo;
begin
  Result:=TSDFileInfo.Create;
  Result.Filename:=aFilename;
  Result.Caption:=aCaption;
  Add(Result);
end;

function TSDFileInfoList.CaptionExists(aCaption: string): boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to Count-1 do
    if CompareFilenames(aCaption,TSDFileInfo(Items[i]).Caption)=0 then
      exit(true);
end;

function TSDFileInfoList.BestDir: TSDFileInfo;
begin
  if Count > 0 then
    Result:=TSDFileInfo(Items[Count-1])
  else
    Result:=Nil;
end;

end.

