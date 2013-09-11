{
 /***************************************************************************
                            initialsetupdlgs.pas
                            --------------------
       Contains the dialogs to help users setup basic settings.


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

  Author: Mattias Gaertner
  
  Abstract:
    Procedures and dialogs to check environment. The IDE uses these procedures
    at startup to check for example the lazarus directory and warns if it looks
    suspicious and choose another.
}
unit InitialSetupDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, LCLProc, Forms, Controls, Buttons, Dialogs,
  FileUtil, Laz2_XMLCfg, lazutf8classes, Graphics, ComCtrls, ExtCtrls, StdCtrls,
  DefineTemplates, CodeToolManager,
  TextTools,
  TransferMacros, LazarusIDEStrConsts, LazConf, EnvironmentOpts, IDEProcs,
  AboutFrm, IDETranslations;
  
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

  TSDFilenameType = (
    sddtLazarusSrcDir,
    sddtCompilerFilename,
    sddtFPCSrcDir
    );

  { TInitialSetupDialog }

  TInitialSetupDialog = class(TForm)
    BtnPanel: TPanel;
    CompilerBrowseButton: TButton;
    CompilerComboBox: TComboBox;
    CompilerLabel: TLabel;
    CompilerMemo: TMemo;
    FPCSrcDirBrowseButton: TButton;
    FPCSrcDirComboBox: TComboBox;
    FPCSrcDirLabel: TLabel;
    FPCSrcDirMemo: TMemo;
    ImageList1: TImageList;
    LazDirBrowseButton: TButton;
    LazDirLabel: TLabel;
    LazDirComboBox: TComboBox;
    LazDirMemo: TMemo;
    PropertiesPageControl: TPageControl;
    PropertiesTreeView: TTreeView;
    Splitter1: TSplitter;
    StartIDEBitBtn: TBitBtn;
    LazarusTabSheet: TTabSheet;
    CompilerTabSheet: TTabSheet;
    FPCSourcesTabSheet: TTabSheet;
    WelcomePaintBox: TPaintBox;
    procedure CompilerBrowseButtonClick(Sender: TObject);
    procedure CompilerComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPCSrcDirBrowseButtonClick(Sender: TObject);
    procedure FPCSrcDirComboBoxChange(Sender: TObject);
    procedure LazDirBrowseButtonClick(Sender: TObject);
    procedure LazDirComboBoxChange(Sender: TObject);
    procedure OnAppActivate(Sender: TObject);
    procedure PropertiesPageControlChange(Sender: TObject);
    procedure PropertiesTreeViewSelectionChanged(Sender: TObject);
    procedure StartIDEBitBtnClick(Sender: TObject);
    procedure WelcomePaintBoxPaint(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    FLazarusDirChanged: boolean;
    fCompilerFilenameChanged: boolean;
    FLastParsedLazDir: string;
    fLastParsedCompiler: string;
    fLastParsedFPCSrcDir: string;
    FIdleConnected: boolean;
    ImgIDError: LongInt;
    ImgIDWarning: LongInt;
    FHeadGraphic: TPortableNetworkGraphic;
    FSelectingPage: boolean;
    FCandidates: array[TSDFilenameType] of TObjectList; // list of TSDFileInfo
    procedure UpdateCaptions;
    procedure SelectPage(const NodeText: string);
    function SelectDirectory(aTitle: string): string;
    procedure UpdateLazarusDirCandidates;
    procedure UpdateCompilerFilenameCandidates;
    procedure UpdateFPCSrcDirCandidates;
    procedure FillComboboxWithFileInfoList(ABox: TComboBox; List: TObjectList;
       ItemIndex: integer = 0);
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateLazDirNote;
    procedure UpdateCompilerNote;
    procedure UpdateFPCSrcDirNote;
    function FirstErrorNode: TTreeNode;
    function GetFPCVer: string;
    function GetFirstCandidate(Candidates: TObjectList;
      MinQuality: TSDFilenameQuality = sddqCompatible): TSDFileInfo;
    function QualityToImgIndex(Quality: TSDFilenameQuality): integer;
    procedure TranslateResourceStrings;
  public
    TVNodeLazarus: TTreeNode;
    TVNodeCompiler: TTreeNode;
    TVNodeFPCSources: TTreeNode;
    procedure Init;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

function ShowInitialSetupDialog: TModalResult;

function CheckLazarusDirectoryQuality(ADirectory: string;
  out Note: string): TSDFilenameQuality;
function SearchLazarusDirectoryCandidates(StopIfFits: boolean): TObjectList;
procedure SetupLazarusDirectory;

function CheckCompilerQuality(AFilename: string;
  out Note: string; TestSrcFilename: string): TSDFilenameQuality;
function SearchCompilerCandidates(StopIfFits: boolean;
  const TestSrcFilename: string): TObjectList;
procedure SetupCompilerFilename;

function CheckFPCSrcDirQuality(ADirectory: string;
  out Note: string; FPCVer: string): TSDFilenameQuality;
function SearchFPCSrcDirCandidates(StopIfFits: boolean;
  const FPCVer: string): TObjectList;
procedure SetupFPCSrcDir(FPCVer: string);

function CheckMakeExeQuality(AFilename: string;
  out Note: string): TSDFilenameQuality;
function SearchMakeExeCandidates(StopIfFits: boolean): TObjectList;
procedure SetupMakeExe;

function GetValueFromPrimaryConfig(OptionFilename, Path: string): string;
function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
function GetValueFromIDEConfig(OptionFilename, Path: string): string;

implementation

type

  { TSetupMacros }

  TSetupMacros = class(TTransferMacroList)
  protected
    procedure DoSubstitution({%H-}TheMacro: TTransferMacro; const MacroName: string;
      var s: string; const {%H-}Data: PtrInt; var Handled, {%H-}Abort: boolean;
      {%H-}Depth: integer); override;
  public
    FPCVer: string;
    LazarusDir: string;
  end;

function CaptionInSDFileList(aCaption: string; SDFileList: TObjectList): boolean;
var
  i: Integer;
begin
  Result:=false;
  if SDFileList=nil then exit;
  for i:=0 to SDFileList.Count-1 do
    if CompareFilenames(aCaption,TSDFileInfo(SDFileList[i]).Caption)=0 then
      exit(true);
end;

function CheckLazarusDirectoryQuality(ADirectory: string;
  out Note: string): TSDFilenameQuality;

  function SubDirExists(SubDir: string; var q: TSDFilenameQuality): boolean;
  begin
    SubDir:=SetDirSeparators(SubDir);
    if DirPathExistsCached(ADirectory+SubDir) then exit(true);
    Result:=false;
    Note:=Format(lisDirectoryNotFound2, [SubDir]);
    q:=sddqIncomplete;
  end;

  function SubFileExists(SubFile: string; var q: TSDFilenameQuality): boolean;
  begin
    SubFile:=SetDirSeparators(SubFile);
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
  if not SubDirExists('ideintf',Result) then exit;
  if not SubFileExists('ide/lazarus.lpi',Result) then exit;
  VersionIncFile:=SetDirSeparators('ide/version.inc');
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

function SearchLazarusDirectoryCandidates(StopIfFits: boolean): TObjectList;

  function CheckDir(Dir: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    RealDir: String;
  begin
    Result:=false;
    if Dir='' then Dir:='.';
    DoDirSeparators(Dir);
    Dir:=ChompPathDelim(Dir);
    // check if already checked
    if CaptionInSDFileList(Dir,List) then exit;
    EnvironmentOptions.LazarusDirectory:=Dir;
    RealDir:=ChompPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
    debugln(['SearchLazarusDirectoryCandidates Value=',Dir,' File=',RealDir]);
    // check if exists
    if not DirPathExistsCached(RealDir) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=RealDir;
    Item.Quality:=CheckLazarusDirectoryQuality(RealDir,Item.Note);
    Item.Caption:=Dir;
    if List=nil then
      List:=TObjectList.create(true);
    List.Add(Item);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

  function CheckViaExe(Filename: string; var List: TObjectList): boolean;
  begin
    Result:=false;
    Filename:=FindDefaultExecutablePath(Filename);
    if Filename='' then exit;
    Filename:=ReadAllLinks(Filename,false);
    if Filename='' then exit;
    Result:=CheckDir(ExtractFilePath(ExpandFileNameUTF8(Filename)),List);
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
    Dir:=ProgramDirectory(true);
    if CheckDir(Dir,Result) then exit;
    ResolvedDir:=ReadAllLinks(Dir,false);
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
  Note: string;
  Dir: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
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
    BestDir:=nil;
    if List<>nil then
      BestDir:=TSDFileInfo(List[List.Count-1]);
    if (BestDir=nil) or (BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupLazarusDirectory: no proper Lazarus directory found.']);
      exit;
    end;
    EnvironmentOptions.LazarusDirectory:=BestDir.Filename;
    debugln(['SetupLazarusDirectory: using ',EnvironmentOptions.LazarusDirectory]);
  finally
    List.Free;
  end;
end;

function CheckCompilerQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;
var
  CfgCache: TFPCTargetConfigCache;
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
  and (CompareFilenames(copy(ShortFilename,1,3),'ppc')<>0)
  then begin
    Note:=lisUnusualCompilerFileNameUsuallyItStartsWithFpcPpcOr;
    exit(sddqIncomplete);
  end;

  if TestSrcFilename<>'' then
  begin
    CfgCache:=CodeToolBoss.FPCDefinesCache.ConfigCaches.Find(
                                                       AFilename,'','','',true);
    if CfgCache.NeedsUpdate then
      CfgCache.Update(TestSrcFilename);
    i:=CfgCache.IndexOfUsedCfgFile;
    if i<0 then
    begin
      Note:=lisFpcCfgIsMissing;
      exit;
    end;
    if not CfgCache.HasPPUs then
    begin
      Note:=lisSystemPpuNotFoundCheckYourFpcCfg;
      exit;
    end;
    if CompareFileExt(CfgCache.Units['classes'],'ppu',false)<>0 then
    begin
      Note:=lisClassesPpuNotFoundCheckYourFpcCfg;
      exit;
    end;
  end;

  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchCompilerCandidates(StopIfFits: boolean;
  const TestSrcFilename: string): TObjectList;
var
  ShortCompFile: String;

  function CheckFile(AFilename: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    RealFilename: String;
  begin
    Result:=false;
    if AFilename='' then exit;
    DoDirSeparators(AFilename);
    // check if already checked
    if CaptionInSDFileList(AFilename,List) then exit;
    EnvironmentOptions.CompilerFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedCompilerFilename;
    debugln(['SearchCompilerCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=RealFilename;
    Item.Quality:=CheckCompilerQuality(RealFilename,Item.Note,TestSrcFilename);
    Item.Caption:=AFilename;
    if List=nil then
      List:=TObjectList.create(true);
    List.Add(Item);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

  function CheckSubDirs(ADir: string; var List: TObjectList): boolean;
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
      if FindFirstUTF8(ADir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
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

    // check PATH
    if CheckFile(FindDefaultCompilerPath,Result) then exit;

    // check history
    Files:=EnvironmentOptions.CompilerFileHistory;
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check paths with versions
    ShortCompFile:=GetDefaultCompilerFilename;

    // check $(LazarusDir)\fpc\bin\i386-win32\fpc.exe
    if CheckFile(SetDirSeparators('$(LazarusDir)/fpc/bin/$(TargetCPU)-$(TargetOS)/')+ShortCompFile,Result)
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

    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // Windows has some special places
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

procedure SetupCompilerFilename;
var
  Note: string;
  Filename: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
begin
  Filename:=EnvironmentOptions.GetParsedCompilerFilename;
  Quality:=CheckCompilerQuality(Filename,Note,'');
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
  List:=SearchCompilerCandidates(true,CodeToolBoss.FPCDefinesCache.TestFilename);
  try
    BestDir:=nil;
    if List<>nil then
      BestDir:=TSDFileInfo(List[List.Count-1]);
    if (BestDir=nil) or (BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupCompilerFilename: no proper compiler found.']);
      exit;
    end;
    EnvironmentOptions.CompilerFilename:=BestDir.Filename;
    debugln(['SetupCompilerFilename: using ',EnvironmentOptions.CompilerFilename]);
  finally
    List.Free;
  end;
end;

function CheckFPCSrcDirQuality(ADirectory: string; out Note: string;
  FPCVer: string): TSDFilenameQuality;

  function SubDirExists(SubDir: string): boolean;
  begin
    SubDir:=SetDirSeparators(SubDir);
    if DirPathExistsCached(ADirectory+SubDir) then exit(true);
    Result:=false;
    Note:=Format(lisDirectoryNotFound2, [SubDir]);
  end;

  function SubFileExists(SubFile: string): boolean;
  begin
    SubFile:=SetDirSeparators(SubFile);
    if FileExistsCached(ADirectory+SubFile) then exit(true);
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
begin
  Result:=sddqInvalid;
  Note:='';
  ADirectory:=TrimFilename(ADirectory);
  if not DirPathExistsCached(ADirectory) then
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
    if FileExistsCached(VersionFile) then
    begin
      sl:=TStringListUTF8.Create;
      try
        try
          sl.LoadFromFile(VersionFile);
          for i:=0 to sl.Count-1 do
          begin
            if REMatches(sl[i],' version_nr *= *''([0-9]+)''','I') then
              VersionNr:=REVar(1)
            else if REMatches(sl[i],' release_nr *= *''([0-9]+)''','I') then
              ReleaseNr:=REVar(1)
            else if REMatches(sl[i],' patch_nr *= *''([0-9]+)''','I') then begin
              PatchNr:=REVar(1);
              break;
            end;
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

function SearchFPCSrcDirCandidates(StopIfFits: boolean;
  const FPCVer: string): TObjectList;

  function Check(Dir: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    RealDir: String;
  begin
    Result:=false;
    DoDirSeparators(Dir);
    Dir:=ChompPathDelim(Dir);
    if Dir='' then exit;
    // check if already checked
    if CaptionInSDFileList(Dir,List) then exit;
    EnvironmentOptions.FPCSourceDirectory:=Dir;
    RealDir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
    debugln(['SearchFPCSrcDirCandidates Value=',Dir,' File=',RealDir]);
    if RealDir='' then exit;
    // check if exists
    if not DirPathExistsCached(RealDir) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=RealDir;
    Item.Quality:=CheckFPCSrcDirQuality(RealDir,Item.Note,FPCVer);
    Item.Caption:=Dir;
    if List=nil then
      List:=TObjectList.create(true);
    List.Add(Item);
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

    // check history
    Dirs:=EnvironmentOptions.FPCSourceDirHistory;
    if Dirs<>nil then
      for i:=0 to Dirs.Count-1 do
        if Check(Dirs[i],Result) then exit;

    // $(LazarusDir)/fpc/$(FPCVer)/source
    if Check(SetDirSeparators('$(LazarusDir)/fpc/$(FPCVer)/source'),Result) then
      exit;

    // check relative to fpc.exe
    if Check(SetDirSeparators('$Path($(CompPath))/../../source'),Result) then
      exit;

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

procedure SetupFPCSrcDir(FPCVer: string);
var
  Note: string;
  Dir: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
begin
  Dir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
  Quality:=CheckFPCSrcDirQuality(Dir,Note,FPCVer);
  if Quality<>sddqInvalid then exit;
  // bad fpc src directory => searching a good one
  dbgout('SetupFPCSourceDirectory:');
  if EnvironmentOptions.FPCSourceDirectory<>'' then
  begin
    dbgout(' The FPC source directory "',EnvironmentOptions.FPCSourceDirectory,'"');
    if EnvironmentOptions.FPCSourceDirectory<>Dir then
      dbgout(' => "',Dir,'"');
    dbgout(' is invalid (Error: ',Note,')');
    debugln(' Searching a proper one ...');
  end else begin
    debugln(' Searching ...');
  end;
  List:=SearchFPCSrcDirCandidates(true,FPCVer);
  try
    BestDir:=nil;
    if List<>nil then
      BestDir:=TSDFileInfo(List[List.Count-1]);
    if (BestDir=nil) or (BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupFPCSourceDirectory: no proper FPC source directory found.']);
      exit;
    end;
    EnvironmentOptions.FPCSourceDirectory:=BestDir.Filename;
    debugln(['SetupFPCSourceDirectory: using ',EnvironmentOptions.FPCSourceDirectory]);
  finally
    List.Free;
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

  if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
    // under Windows the make.exe is in the same directory as fpc.exe
    if not FileExistsCached(ExtractFilePath(AFilename)+'fpc.exe') then begin
      Note:='There is no fpc.exe in the directory of the '+ExtractFilename(AFilename)+'. Usually the make executable is installed together with the fpc compiler.';
      Result:=sddqIncomplete;
    end;
  end;

  Result:=sddqCompatible;
end;

function SearchMakeExeCandidates(StopIfFits: boolean): TObjectList;

  function CheckFile(AFilename: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    RealFilename: String;
  begin
    Result:=false;
    if AFilename='' then exit;
    DoDirSeparators(AFilename);
    // check if already checked
    if CaptionInSDFileList(AFilename,List) then exit;
    EnvironmentOptions.MakeFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedMakeFilename;
    debugln(['SearchMakeExeCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=RealFilename;
    Item.Quality:=CheckMakeExeQuality(RealFilename,Item.Note);
    Item.Caption:=AFilename;
    if List=nil then
      List:=TObjectList.create(true);
    List.Add(Item);
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

    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // check make in fpc.exe directory
      if CheckFile(SetDirSeparators('$Path($(CompPath))/make.exe'),Result)
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

procedure SetupMakeExe;
var
  Note: string;
  Filename: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
begin
  Filename:=EnvironmentOptions.GetParsedMakeFilename;
  Quality:=CheckMakeExeQuality(Filename,Note);
  if Quality<>sddqInvalid then exit;
  // bad make exe
  dbgout('SetupMakeExe:');
  if EnvironmentOptions.MakeFilename<>'' then
  begin
    dbgout(' The "make" executable "',EnvironmentOptions.MakeFilename,'"');
    if EnvironmentOptions.MakeFilename<>Filename then
      dbgout(' => "',Filename,'"');
    dbgout(' is invalid (Error: ',Note,')');
    debugln(' Searching a proper one ...');
  end else begin
    debugln(' Searching "make" ...');
  end;
  List:=SearchMakeExeCandidates(true);
  try
    BestDir:=nil;
    if List<>nil then
      BestDir:=TSDFileInfo(List[List.Count-1]);
    if (BestDir=nil) or (BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupMakeExe: no proper "make" found.']);
      exit;
    end;
    EnvironmentOptions.MakeFilename:=BestDir.Filename;
    debugln(['SetupMakeExe: using ',EnvironmentOptions.MakeFilename]);
  finally
    List.Free;
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

function ShowInitialSetupDialog: TModalResult;
var
  InitialSetupDialog: TInitialSetupDialog;
begin
  InitialSetupDialog:=TInitialSetupDialog.Create(nil);
  try
    Application.TaskBarBehavior:=tbMultiButton;
    InitialSetupDialog.Init;
    Result:=InitialSetupDialog.ShowModal;
  finally
    InitialSetupDialog.Free;
    Application.TaskBarBehavior:=tbDefault;
  end;
end;

{ TSetupMacros }

procedure TSetupMacros.DoSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt; var Handled,
  Abort: boolean; Depth: integer);
begin
  Handled:=true;
  if CompareText(MacroName,'ENV')=0 then
    s:=GetEnvironmentVariableUTF8(MacroName)
  else if CompareText(MacroName,'PrimaryConfigPath')=0 then
    s:=GetPrimaryConfigPath
  else if CompareText(MacroName,'SecondaryConfigPath')=0 then
    s:=GetSecondaryConfigPath
  else if CompareText(MacroName,'FPCVer')=0 then begin
    if FPCVer<>'' then
      s:=FPCVer
    else
      s:={$I %FPCVERSION%};
  end else if CompareText(MacroName,'LazarusDir')=0 then begin
    if LazarusDir<>'' then
      s:=LazarusDir
    else
      s:='<LazarusDirNotSet>';
  end else if (CompareText(MacroName,'TargetOS')=0) then
    s:=GetCompiledTargetOS
  else if (CompareText(MacroName,'TargetCPU')=0) then
    s:=GetCompiledTargetCPU
  else if (CompareText(MacroName,'SrcOS')=0) then
    s:=GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)
  else
    Handled:=false;
  //debugln(['TSetupMacros.DoSubstitution MacroName=',MacroName,' Value="',s,'"']);
end;

{$R *.lfm}

{ TInitialSetupDialog }

procedure TInitialSetupDialog.FormCreate(Sender: TObject);
begin
  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;

  FHeadGraphic:=TPortableNetworkGraphic.Create;
  FHeadGraphic.LoadFromLazarusResource('ide_icon48x48');

  TVNodeLazarus:=PropertiesTreeView.Items.Add(nil,LazarusTabSheet.Caption);
  TVNodeCompiler:=PropertiesTreeView.Items.Add(nil,CompilerTabSheet.Caption);
  TVNodeFPCSources:=PropertiesTreeView.Items.Add(nil,FPCSourcesTabSheet.Caption);
  ImgIDError := ImageList1.AddLazarusResource('state_error');
  ImgIDWarning := ImageList1.AddLazarusResource('state_warning');

  UpdateCaptions;

  Application.AddOnActivateHandler(@OnAppActivate);
end;

procedure TInitialSetupDialog.CompilerComboBoxChange(Sender: TObject);
begin
  UpdateCompilerNote;
end;

procedure TInitialSetupDialog.CompilerBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='fpc'+GetExecutableExt;
    Dlg.Title:=Format(lisSelectPathTo, [Filename]);
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    Filter:=dlgAllFiles+'|'+GetAllFilesMask;
    if ExtractFileExt(Filename)<>'' then
      Filter:=lisExecutable+'|*'+ExtractFileExt(Filename)+'|'+Filter;
    Dlg.Filter:=Filter;
    if not Dlg.Execute then exit;
    Filename:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  CompilerComboBox.Text:=Filename;
  UpdateCompilerNote;
end;

procedure TInitialSetupDialog.FormDestroy(Sender: TObject);
var
  d: TSDFilenameType;
begin
  IdleConnected:=false;
  for d:=low(FCandidates) to high(FCandidates) do
    FreeAndNil(FCandidates[d]);
  FreeAndNil(FHeadGraphic);
end;

procedure TInitialSetupDialog.FPCSrcDirBrowseButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir:=SelectDirectory(lisSelectFPCSourceDirectory);
  if Dir='' then exit;
  FPCSrcDirComboBox.Text:=Dir;
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.FPCSrcDirComboBoxChange(Sender: TObject);
begin
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.LazDirBrowseButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir:=SelectDirectory(lisSelectLazarusSourceDirectory);
  if Dir='' then exit;
  LazDirComboBox.Text:=Dir;
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.LazDirComboBoxChange(Sender: TObject);
begin
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.OnAppActivate(Sender: TObject);
begin
  // switched back from another application
  InvalidateFileStateCache;
end;

procedure TInitialSetupDialog.PropertiesPageControlChange(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  if PropertiesPageControl.ActivePage=nil then exit;
  s:=PropertiesPageControl.ActivePage.Caption;
  for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do
    if PropertiesTreeView.Items.TopLvlItems[i].Text=s then
      PropertiesTreeView.Selected:=PropertiesTreeView.Items.TopLvlItems[i];
end;

procedure TInitialSetupDialog.PropertiesTreeViewSelectionChanged(Sender: TObject);
begin
  if PropertiesTreeView.Selected=nil then
    SelectPage(TVNodeLazarus.Text)
  else
    SelectPage(PropertiesTreeView.Selected.Text);
end;

procedure TInitialSetupDialog.StartIDEBitBtnClick(Sender: TObject);
var
  Node: TTreeNode;
  s: String;
  MsgResult: TModalResult;
begin
  Node:=FirstErrorNode;
  if Node=TVNodeLazarus then
    s:=lisWithoutAProperLazarusDirectoryYouWillGetALotOfWarn
  else if Node=TVNodeCompiler then
    s:=lisWithoutAProperCompilerTheCodeBrowsingAndCompilingW
  else if Node=TVNodeFPCSources then
    s:=lisWithoutTheProperFPCSourcesCodeBrowsingAndCompletio;
  if s<>'' then begin
    MsgResult:=MessageDlg(lisCCOWarningCaption, s, mtWarning, [mbIgnore,
      mbCancel], 0);
    if MsgResult<>mrIgnore then exit;
  end;

  s:=LazDirComboBox.Text;
  if s<>'' then
    EnvironmentOptions.LazarusDirectory:=s;
  s:=CompilerComboBox.Text;
  if s<>'' then
    EnvironmentOptions.CompilerFilename:=s;
  s:=FPCSrcDirComboBox.Text;
  if s<>'' then
    EnvironmentOptions.FPCSourceDirectory:=s;

  SetupMakeExe;

  ModalResult:=mrOk;
end;

procedure TInitialSetupDialog.WelcomePaintBoxPaint(Sender: TObject);
begin
  with WelcomePaintBox.Canvas do begin
    GradientFill(WelcomePaintBox.ClientRect,$854b32,$c88e60,gdHorizontal);
    Draw(0,WelcomePaintBox.ClientHeight-FHeadGraphic.Height,FHeadGraphic);
    Font.Color:=clWhite;
    Font.Height:=30;
    Brush.Style:=bsClear;
    TextOut(FHeadGraphic.Width+15, 5, lisConfigureLazarusIDE);
  end;
end;

procedure TInitialSetupDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if FLazarusDirChanged then begin
    UpdateCompilerFilenameCandidates;
    UpdateCompilerNote;
  end else if fCompilerFilenameChanged then begin
    UpdateFPCSrcDirCandidates;
    UpdateFPCSrcDirNote;
  end else
    IdleConnected:=false;
end;

procedure TInitialSetupDialog.UpdateCaptions;
begin
  Caption:=Format(lisWelcomeToLazarusIDE, [GetLazarusVersionString]);

  StartIDEBitBtn.Caption:=lisStartIDE;

  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;

  TVNodeLazarus.Text:=LazarusTabSheet.Caption;
  TVNodeCompiler.Text:=CompilerTabSheet.Caption;
  TVNodeFPCSources.Text:=FPCSourcesTabSheet.Caption;

  LazDirBrowseButton.Caption:=lisPathEditBrowse;
  LazDirLabel.Caption:=Format(
    lisTheLazarusDirectoryContainsTheSourcesOfTheIDEAndTh, [PathDelim]);

  CompilerBrowseButton.Caption:=lisPathEditBrowse;
  CompilerLabel.Caption:=Format(
    lisTheFreePascalCompilerExecutableTypicallyHasTheName, [DefineTemplates.
    GetDefaultCompilerFilename, DefineTemplates.GetDefaultCompilerFilename(
    GetCompiledTargetCPU)]);

  FPCSrcDirBrowseButton.Caption:=lisPathEditBrowse;
  FPCSrcDirLabel.Caption:=Format(
    lisTheSourcesOfTheFreePascalPackagesAreRequiredForBro, [SetDirSeparators('rtl'
    +'/linux/system.pp')]);
end;

procedure TInitialSetupDialog.SelectPage(const NodeText: string);
var
  i: Integer;
  Node: TTreeNode;
begin
  if FSelectingPage then exit;
  FSelectingPage:=true;
  try
    for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do begin
      Node:=PropertiesTreeView.Items.TopLvlItems[i];
      if Node.Text=NodeText then begin
        PropertiesTreeView.Selected:=Node;
        PropertiesPageControl.ActivePageIndex:=i;
        break;
      end;
    end;
  finally
    FSelectingPage:=false;
  end;
end;

function TInitialSetupDialog.SelectDirectory(aTitle: string): string;
var
  DirDlg: TSelectDirectoryDialog;
begin
  Result:='';
  DirDlg:=TSelectDirectoryDialog.Create(nil);
  try
    DirDlg.Title:=aTitle;
    DirDlg.Options:=DirDlg.Options+[ofPathMustExist,ofFileMustExist];
    if not DirDlg.Execute then exit;
    Result:=DirDlg.FileName;
  finally
    DirDlg.Free;
  end;
end;

procedure TInitialSetupDialog.UpdateLazarusDirCandidates;
var
  Dirs: TObjectList;
begin
  Dirs:=SearchLazarusDirectoryCandidates(false);
  FreeAndNil(FCandidates[sddtLazarusSrcDir]);
  FCandidates[sddtLazarusSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(LazDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateCompilerFilenameCandidates;
var
  Files: TObjectList;
begin
  FLazarusDirChanged:=false;
  Files:=SearchCompilerCandidates(false,
                    CodeToolBoss.FPCDefinesCache.TestFilename);
  FreeAndNil(FCandidates[sddtCompilerFilename]);
  FCandidates[sddtCompilerFilename]:=Files;
  FillComboboxWithFileInfoList(CompilerComboBox,Files);
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirCandidates;
var
  Dirs: TObjectList;
begin
  fCompilerFilenameChanged:=false;
  Dirs:=SearchFPCSrcDirCandidates(false,GetFPCVer);
  FreeAndNil(FCandidates[sddtFPCSrcDir]);
  FCandidates[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.FillComboboxWithFileInfoList(ABox: TComboBox;
  List: TObjectList; ItemIndex: integer);
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    if List<>nil then
      for i:=0 to List.Count-1 do
        sl.Add(TSDFileInfo(List[i]).Caption);
    ABox.Items.Assign(sl);
    if (ItemIndex>=0) and (ItemIndex<sl.Count) then
      ABox.Text:=sl[ItemIndex]
    else if ABox.Text=ABox.Name then
      ABox.Text:='';
  finally
    sl.Free;
  end;
end;

procedure TInitialSetupDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TInitialSetupDialog.UpdateLazDirNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=LazDirComboBox.Text;
  CurCaption:=ChompPathDelim(CurCaption);
  EnvironmentOptions.LazarusDirectory:=CurCaption;
  if FLastParsedLazDir=EnvironmentOptions.GetParsedLazarusDirectory then exit;
  FLastParsedLazDir:=EnvironmentOptions.GetParsedLazarusDirectory;
  //debugln(['TInitialSetupDialog.UpdateLazDirNote ',FLastParsedLazDir]);
  Quality:=CheckLazarusDirectoryQuality(FLastParsedLazDir,Note);
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.LazarusDirectory<>EnvironmentOptions.GetParsedLazarusDirectory
  then
    s:=lisDirectory+EnvironmentOptions.GetParsedLazarusDirectory+LineEnding+
      LineEnding+s;
  LazDirMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeLazarus.ImageIndex:=ImageIndex;
  TVNodeLazarus.SelectedIndex:=ImageIndex;

  FLazarusDirChanged:=true;
  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateCompilerNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=CompilerComboBox.Text;
  EnvironmentOptions.CompilerFilename:=CurCaption;
  if fLastParsedCompiler=EnvironmentOptions.GetParsedCompilerFilename then exit;
  fLastParsedCompiler:=EnvironmentOptions.GetParsedCompilerFilename;
  //debugln(['TInitialSetupDialog.UpdateCompilerNote ',fLastParsedCompiler]);
  Quality:=CheckCompilerQuality(fLastParsedCompiler,Note,
                                CodeToolBoss.FPCDefinesCache.TestFilename);
  if Quality<>sddqInvalid then begin
    CodeToolBoss.FPCDefinesCache.ConfigCaches.Find(
      fLastParsedCompiler,'','','',true);
  end;

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.CompilerFilename<>EnvironmentOptions.GetParsedCompilerFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedCompilerFilename+LineEnding+
      LineEnding+s;
  CompilerMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeCompiler.ImageIndex:=ImageIndex;
  TVNodeCompiler.SelectedIndex:=ImageIndex;

  fCompilerFilenameChanged:=true;
  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=FPCSrcDirComboBox.Text;
  CurCaption:=ChompPathDelim(CurCaption);
  EnvironmentOptions.FPCSourceDirectory:=CurCaption;
  if fLastParsedFPCSrcDir=EnvironmentOptions.GetParsedFPCSourceDirectory then exit;
  fLastParsedFPCSrcDir:=EnvironmentOptions.GetParsedFPCSourceDirectory;
  //debugln(['TInitialSetupDialog.UpdateFPCSrcDirNote ',fLastParsedFPCSrcDir]);
  Quality:=CheckFPCSrcDirQuality(fLastParsedFPCSrcDir,Note,GetFPCVer);
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.FPCSourceDirectory<>EnvironmentOptions.GetParsedFPCSourceDirectory
  then
    s:=lisDirectory+EnvironmentOptions.GetParsedFPCSourceDirectory+LineEnding+
      LineEnding+s;
  FPCSrcDirMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeFPCSources.ImageIndex:=ImageIndex;
  TVNodeFPCSources.SelectedIndex:=ImageIndex;
end;

function TInitialSetupDialog.FirstErrorNode: TTreeNode;
var
  i: Integer;
begin
  for i:=0 to PropertiesTreeView.Items.TopLvlCount-1 do
  begin
    Result:=PropertiesTreeView.Items.TopLvlItems[i];
    if Result.ImageIndex=ImgIDError then exit;
  end;
  Result:=nil;
end;

function TInitialSetupDialog.GetFPCVer: string;
begin
  Result:='$(FPCVer)';
  GlobalMacroList.SubstituteStr(Result);
end;

function TInitialSetupDialog.GetFirstCandidate(Candidates: TObjectList;
  MinQuality: TSDFilenameQuality): TSDFileInfo;
var
  i: Integer;
begin
  if Candidates<>nil then
    for i:=0 to Candidates.Count-1 do begin
      Result:=TSDFileInfo(Candidates[i]);
      if Result.Quality>=MinQuality then
        exit;
    end;
  Result:=nil;
end;

function TInitialSetupDialog.QualityToImgIndex(Quality: TSDFilenameQuality
  ): integer;
begin
  if Quality=sddqCompatible then
    Result:=-1
  else if Quality=sddqWrongMinorVersion then
    Result:=ImgIDWarning
  else
    Result:=ImgIDError;
end;

procedure TInitialSetupDialog.TranslateResourceStrings;
begin
  IDETranslations.TranslateResourceStrings(
                           EnvironmentOptions.GetParsedLazarusDirectory,
                           EnvironmentOptions.LanguageID);
  UpdateCaptions;
end;

procedure TInitialSetupDialog.Init;
var
  Node: TTreeNode;
  Candidate: TSDFileInfo;
  IsFirstStart: Boolean;
  PrimaryFilename: String;
  SecondaryFilename: String;
  PrimaryEnvs: TStringListUTF8;
  SecondaryEnvs: TStringListUTF8;
begin
  IsFirstStart:=not FileExistsCached(EnvironmentOptions.Filename);
  if not IsFirstStart then begin
    PrimaryFilename:=EnvironmentOptions.Filename;
    SecondaryFilename:=AppendPathDelim(GetSecondaryConfigPath)+ExtractFilename(PrimaryFilename);
    if FileExistsUTF8(PrimaryFilename)
    and FileExistsUTF8(SecondaryFilename) then begin
      // compare content of primary and secondary config
      PrimaryEnvs:=TStringListUTF8.Create;
      SecondaryEnvs:=TStringListUTF8.Create;
      try
        PrimaryEnvs.LoadFromFile(PrimaryFilename);
      except
        on E: Exception do
          debugln(['TInitialSetupDialog.Init unable to read "'+PrimaryFilename+'": '+E.Message]);
      end;
      try
        SecondaryEnvs.LoadFromFile(SecondaryFilename);
      except
        on E: Exception do
          debugln(['TInitialSetupDialog.Init unable to read "'+SecondaryFilename+'": '+E.Message]);
      end;
      IsFirstStart:=PrimaryEnvs.Text=SecondaryEnvs.Text;
      PrimaryEnvs.Free;
      SecondaryEnvs.Free;
    end;
  end;
  //debugln(['TInitialSetupDialog.Init IsFirstStart=',IsFirstStart,' ',EnvironmentOptions.Filename]);

  // Lazarus directory
  UpdateLazarusDirCandidates;
  if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedLazarusDirectory))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtLazarusSrcDir]);
    if Candidate<>nil then
      EnvironmentOptions.LazarusDirectory:=Candidate.Caption;
    if FileExistsCached(EnvironmentOptions.GetParsedLazarusDirectory) then
      TranslateResourceStrings;
  end;
  LazDirComboBox.Text:=EnvironmentOptions.LazarusDirectory;
  FLastParsedLazDir:='. .';
  UpdateLazDirNote;
  FLazarusDirChanged:=false;

  // compiler filename
  UpdateCompilerFilenameCandidates;
  if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedCompilerFilename))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtCompilerFilename]);
    if Candidate<>nil then
      EnvironmentOptions.CompilerFilename:=Candidate.Caption;
  end;
  CompilerComboBox.Text:=EnvironmentOptions.CompilerFilename;
  fLastParsedCompiler:='. .';
  UpdateCompilerNote;
  fCompilerFilenameChanged:=false;

  // FPC source directory
  UpdateFPCSrcDirCandidates;
  if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedFPCSourceDirectory))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtFPCSrcDir]);
    if Candidate<>nil then
      EnvironmentOptions.FPCSourceDirectory:=Candidate.Caption;
  end;
  FPCSrcDirComboBox.Text:=EnvironmentOptions.FPCSourceDirectory;
  fLastParsedFPCSrcDir:='. .';
  UpdateFPCSrcDirNote;

  // select first error
  Node:=FirstErrorNode;
  if Node=nil then
    Node:=TVNodeLazarus;
  PropertiesTreeView.Selected:=Node;
end;

end.

