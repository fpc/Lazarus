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
  FileUtil, Graphics, ComCtrls, ExtCtrls, StdCtrls,
  Laz_XMLCfg,
  DefineTemplates, CodeToolManager,
  TextTools, IDEDialogs,
  TransferMacros, LazarusIDEStrConsts, LazConf, EnvironmentOpts, IDEProcs,
  AboutFrm;
  
type
  TSDFilenameQuality = (
    sddqInvalid,
    sddqWrongVersion,
    sddqIncomplete,
    sddqCompatible
    );

  TSDFileInfo = class
  public
    Filename: string;
    Caption: string;
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
    procedure PropertiesPageControlChange(Sender: TObject);
    procedure PropertiesTreeViewSelectionChanged(Sender: TObject);
    procedure StartIDEBitBtnClick(Sender: TObject);
    procedure WelcomePaintBoxPaint(Sender: TObject);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    FFPCSrcNeedsUpdate: boolean;
    FCompilerNeedsUpdate: boolean;
    FFPCVer: string;
    FIdleConnected: boolean;
    FLazarusDir: string;
    ImgIDError: LongInt;
    FHeadGraphic: TPortableNetworkGraphic;
    FSelectingPage: boolean;
    FDirs: array[TSDFilenameType] of TObjectList; // list of TSDFileInfo
    procedure SelectPage(const NodeText: string);
    function SelectDirectory(aTitle: string): string;
    procedure InitLazarusDir;
    procedure InitCompilerFilename;
    procedure InitFPCSrcDir;
    procedure FillComboboxWithFileInfoList(ABox: TComboBox; List: TObjectList;
       ItemIndex: integer = 0);
    procedure SetFPCVer(const AValue: string);
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetLazarusDir(const AValue: string);
    procedure UpdateLazDirNote;
    procedure UpdateCompilerNote;
    procedure UpdateFPCSrcDirNote;
    function FirstErrorNode: TTreeNode;
    function GetCurrentLazarusDir: string;
    function GetCurrentCompilerFilename: string;
    function GetCurrentFPCSrcDir: string;
  public
    TVNodeLazarus: TTreeNode;
    TVNodeCompiler: TTreeNode;
    TVNodeFPCSources: TTreeNode;
    procedure Init;
    property LazarusDir: string read FLazarusDir write SetLazarusDir; // expanded
    property FPCVer: string read FFPCVer write SetFPCVer;
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
  const LazarusDir, TestSrcFilename: string): TObjectList;
procedure SetupCompilerFilename;

function CheckFPCSrcDirQuality(ADirectory: string;
  out Note: string; FPCVer: string): TSDFilenameQuality;
function SearchFPCSrcDirCandidates(StopIfFits: boolean;
  const LazarusDir, FPCVer: string): TObjectList;

function GetValueFromPrimaryConfig(OptionFilename, Path: string): string;
function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
function GetValueFromIDEConfig(OptionFilename, Path: string): string;

implementation

type

  { TSetupMacros }

  TSetupMacros = class(TTransferMacroList)
  protected
    procedure DoSubstitution(TheMacro: TTransferMacro; const MacroName: string;
      var s: string; const Data: PtrInt; var Handled, Abort: boolean;
      Depth: integer); override;
  public
    FPCVer: string;
    LazarusDir: string;
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
  sl: TStringList;
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
  sl:=TStringList.Create;
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
      Note:=rsOk;
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
    i: Integer;
  begin
    Result:=false;
    Dir:=TrimFilename(Dir);
    if Dir='' then exit;
    Dir:=ChompPathDelim(ExpandFileNameUTF8(Dir));
    // check if already checked
    if List<>nil then begin
      for i:=0 to List.Count-1 do
        if CompareFilenames(Dir,TSDFileInfo(List[i]).Filename)=0 then exit;
    end;
    // check if exists
    if not DirPathExistsCached(Dir) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=Dir;
    Item.Quality:=CheckLazarusDirectoryQuality(Dir,Item.Note);
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
begin
  Result:=nil;

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
end;

procedure SetupLazarusDirectory;
var
  Note: string;
  Dir: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
begin
  Dir:=EnvironmentOptions.LazarusDirectory;
  Quality:=CheckLazarusDirectoryQuality(Dir,Note);
  if Quality<>sddqInvalid then exit;
  debugln(['SetupLazarusDirectory: The Lazarus directory "',Dir,'" is invalid (Error: ',Note,'). Searching a proper one ...']);
  List:=SearchLazarusDirectoryCandidates(true);
  try
    BestDir:=nil;
    if List<>nil then
      BestDir:=TSDFileInfo(List[List.Count-1]);
    if (BestDir=nil) or (BestDir.Quality=sddqInvalid) then begin
      debugln(['SetupCompilerFilename: no proper Lazarus directory found.']);
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
begin
  Result:=sddqInvalid;
  AFilename:=TrimFilename(AFilename);
  if not FileExistsCached(AFilename) then
  begin
    Note:=lisFileNotFound4;
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then
  begin
    Note:=lisFileIsNotAnExecutable;
    exit;
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

  Note:=rsOk;
  Result:=sddqCompatible;
end;

function SearchCompilerCandidates(StopIfFits: boolean;
  const LazarusDir, TestSrcFilename: string): TObjectList;
var
  Macros: TSetupMacros;
  Target: String;
  ShortCompFile: String;

  function CheckFile(AFilename: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    i: Integer;
    ResolvedFilename: String;
  begin
    Result:=false;
    AFilename:=TrimFilename(AFilename);
    if AFilename='' then exit;
    // check if already checked
    if List<>nil then
    begin
      for i:=0 to List.Count-1 do
        if CompareFilenames(AFilename,TSDFileInfo(List[i]).Filename)=0 then exit;
    end;
    // replace macros
    ResolvedFilename:=AFilename;
    if TSetupMacros.StrHasMacros(ResolvedFilename) then
    begin
      if Macros=nil then
      begin
        Macros:=TSetupMacros.Create;
        Macros.LazarusDir:=LazarusDir;
      end;
      if not Macros.SubstituteStr(ResolvedFilename) then exit;
      ResolvedFilename:=TrimFilename(ResolvedFilename);
      if ResolvedFilename='' then exit;
    end;
    // expand file name
    ResolvedFilename:=ExpandFileNameUTF8(ResolvedFilename);
    // check if exists
    if not FileExistsCached(ResolvedFilename) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=AFilename;
    Item.Quality:=CheckCompilerQuality(ResolvedFilename,Item.Note,TestSrcFilename);
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
    SubFile:='bin'+PathDelim+Target+PathDelim+ShortCompFile;
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
begin
  Result:=nil;

  Macros:=nil;
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

    // check common directories
    Files:=TStringList.Create;
    try
      GetDefaultCompilerFilenames(Files);
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;
    finally
      Files.Free;
    end;

    // check paths with versions
    Target:=GetCompiledTargetCPU+'-'+GetCompiledTargetOS;
    ShortCompFile:=GetDefaultCompilerFilename;

    // check $(LazDir)\fpc\bin\i386-win32\fpc.exe
    if (LazarusDir<>'')
    and CheckFile(AppendPathDelim(LazarusDir)
      +SetDirSeparators('fpc/bin/'+Target+'/'+ShortCompFile),Result)
    then exit;

    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // windows has some special places
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
    if Macros<>nil then
      Macros.Free;
  end;
end;

procedure SetupCompilerFilename;
var
  Note: string;
  CompFile: String;
  Quality: TSDFilenameQuality;
  BestDir: TSDFileInfo;
  List: TObjectList;
begin
  CompFile:=EnvironmentOptions.GetCompilerFilename;
  Quality:=CheckCompilerQuality(CompFile,Note,'');
  if Quality<>sddqInvalid then exit;
  debugln(['SetupCompilerFilename: The compiler path "',CompFile,'" is invalid (Error: ',Note,'). Searching a proper one ...']);
  List:=SearchCompilerCandidates(true,EnvironmentOptions.LazarusDirectory,
                                 CodeToolBoss.FPCDefinesCache.TestFilename);
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
  VersionFile: String;
  sl: TStringList;
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
  if not SubDirExists('rtl',Result) then exit;
  if not SubDirExists('packages',Result) then exit;
  if not SubFileExists('rtl/linux/system.pp',Result) then exit;
  // check version
  if (FPCVer<>'') then
  begin
    VersionFile:=ADirectory+'compiler'+PathDelim+'version.pas';
    if FileExistsCached(VersionFile) then
    begin
      sl:=TStringList.Create;
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
  Note:=rsOk;
  Result:=sddqCompatible;
end;

function SearchFPCSrcDirCandidates(StopIfFits: boolean;
  const LazarusDir, FPCVer: string): TObjectList;
var
  Macros: TSetupMacros;

  function InList(AFilename: string; List: TObjectList): boolean;
  var
    i: Integer;
  begin
    Result:=false;
    if List=nil then exit;
    for i:=0 to List.Count-1 do
      if CompareFilenames(AFilename,TSDFileInfo(List[i]).Filename)=0 then
        exit(true);
  end;

  function Check(AFilename: string; var List: TObjectList): boolean;
  var
    Item: TSDFileInfo;
    ResolvedFilename: String;
  begin
    Result:=false;
    AFilename:=TrimFilename(AFilename);
    if AFilename='' then exit;
    // check if already checked
    if InList(AFilename,List) then exit;
    ResolvedFilename:=AFilename;
    // replace macros
    if TSetupMacros.StrHasMacros(ResolvedFilename) then
    begin
      if Macros=nil then
      begin
        Macros:=TSetupMacros.Create;
        Macros.LazarusDir:=LazarusDir;
        Macros.FPCVer:=FPCVer;
      end;
      if not Macros.SubstituteStr(ResolvedFilename) then exit;
      ResolvedFilename:=TrimFilename(ResolvedFilename);
      if ResolvedFilename='' then exit;
    end;
    // expand file name
    ResolvedFilename:=ChompPathDelim(ExpandFileNameUTF8(ResolvedFilename));
    // check if exists
    if not DirPathExistsCached(ResolvedFilename) then exit;
    // add to list and check quality
    Item:=TSDFileInfo.Create;
    Item.Filename:=AFilename;
    Item.Quality:=CheckFPCSrcDirQuality(ResolvedFilename,Item.Note,FPCVer);
    Item.Caption:=AFilename;
    if List=nil then
      List:=TObjectList.create(true);
    List.Add(Item);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

var
  AFilename: String;
  Dirs: TStringList;
  i: Integer;
begin
  Result:=nil;

  Macros:=nil;
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
    if Macros<>nil then
      Macros.Free;
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
    InitialSetupDialog.Init;
    Result:=InitialSetupDialog.ShowModal;
  finally
    InitialSetupDialog.Free;
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
  end else if CompareText(MacroName,'TargetOS')=0 then
    s:=GetCompiledTargetOS
  else if CompareText(MacroName,'TargetCPU')=0 then
    s:=GetCompiledTargetCPU
  else if CompareText(MacroName,'SrcOS')=0 then
    s:=GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)
  else
    Handled:=false;
  //debugln(['TSetupMacros.DoSubstitution MacroName=',MacroName,' Value="',s,'"']);
end;

{$R *.lfm}

{ TInitialSetupDialog }

procedure TInitialSetupDialog.FormCreate(Sender: TObject);
begin
  Caption:=Format(lisWelcomeToLazarusIDE, [GetLazarusVersionString]);

  StartIDEBitBtn.Caption:=lisStartIDE;

  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;

  FHeadGraphic:=TPortableNetworkGraphic.Create;
  FHeadGraphic.LoadFromLazarusResource('ide_icon48x48');

  TVNodeLazarus:=PropertiesTreeView.Items.Add(nil,LazarusTabSheet.Caption);
  TVNodeCompiler:=PropertiesTreeView.Items.Add(nil,CompilerTabSheet.Caption);
  TVNodeFPCSources:=PropertiesTreeView.Items.Add(nil,FPCSourcesTabSheet.Caption);
  ImgIDError := ImageList1.AddLazarusResource('state_error');

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

procedure TInitialSetupDialog.CompilerComboBoxChange(Sender: TObject);
begin
  UpdateCompilerNote;
  UpdateFPCSrcDirNote;
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
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.FormDestroy(Sender: TObject);
var
  d: TSDFilenameType;
begin
  IdleConnected:=false;
  for d:=low(FDirs) to high(FDirs) do
    FreeAndNil(FDirs[d]);
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
  UpdateCompilerNote;
end;

procedure TInitialSetupDialog.FPCSrcDirComboBoxChange(Sender: TObject);
begin
  UpdateFPCSrcDirNote;
  UpdateCompilerNote;
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

  s:=GetCurrentLazarusDir;
  if s<>'' then EnvironmentOptions.LazarusDirectory:=s;
  s:=GetCurrentCompilerFilename;
  if s<>'' then
    EnvironmentOptions.CompilerFilename:=s;
  s:=GetCurrentFPCSrcDir;
  if s<>'' then
    EnvironmentOptions.FPCSourceDirectory:=s;

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
  if FCompilerNeedsUpdate then
    InitCompilerFilename
  else if FFPCSrcNeedsUpdate then
    InitFPCSrcDir
  else
    IdleConnected:=false;
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

procedure TInitialSetupDialog.InitLazarusDir;
var
  Dirs: TObjectList;
begin
  FreeAndNil(FDirs[sddtLazarusSrcDir]);
  Dirs:=SearchLazarusDirectoryCandidates(false);;
  FDirs[sddtLazarusSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(LazDirComboBox,Dirs);
  UpdateLazDirNote;
end;

procedure TInitialSetupDialog.InitCompilerFilename;
var
  Files: TObjectList;
begin
  FCompilerNeedsUpdate:=false;
  FreeAndNil(FDirs[sddtCompilerFilename]);
  Files:=SearchCompilerCandidates(false,LazDirComboBox.Text,
                                  CodeToolBoss.FPCDefinesCache.TestFilename);
  FDirs[sddtCompilerFilename]:=Files;
  FillComboboxWithFileInfoList(CompilerComboBox,Files);
  UpdateCompilerNote;
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.InitFPCSrcDir;
var
  Dirs: TObjectList;
begin
  FFPCSrcNeedsUpdate:=false;
  FreeAndNil(FDirs[sddtFPCSrcDir]);
  Dirs:=SearchFPCSrcDirCandidates(false,LazDirComboBox.Text,FPCVer);
  FDirs[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
  UpdateCompilerNote;
  UpdateFPCSrcDirNote;
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

procedure TInitialSetupDialog.SetFPCVer(const AValue: string);
begin
  if FFPCVer=AValue then exit;
  FFPCVer:=AValue;
  FFPCSrcNeedsUpdate:=true;
  FCompilerNeedsUpdate:=true;
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

procedure TInitialSetupDialog.SetLazarusDir(const AValue: string);
begin
  if FLazarusDir=AValue then exit;
  FLazarusDir:=AValue;
  FCompilerNeedsUpdate:=true;
end;

procedure TInitialSetupDialog.UpdateLazDirNote;

  function NormDir(const Dir: string): string;
  begin
    Result:=ChompPathDelim(TrimFilename(Dir));
    if Result<>'' then
      Result:=ChompPathDelim(TrimFilename(ExpandFileNameUTF8(Result)));
  end;

var
  i: Integer;
  Dirs: TObjectList;
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  i:=-1;
  Dirs:=FDirs[sddtLazarusSrcDir];
  CurCaption:=LazDirComboBox.Text;
  if Dirs<>nil then begin
    i:=Dirs.Count-1;
    while (i>=0) and (TSDFileInfo(Dirs[i]).Caption<>CurCaption) do dec(i);
  end;
  if i>=0 then begin
    Quality:=TSDFileInfo(Dirs[i]).Quality;
    Note:=TSDFileInfo(Dirs[i]).Note;
    LazarusDir:=NormDir(TSDFileInfo(Dirs[i]).Filename);
  end else begin
    LazarusDir:=NormDir(CurCaption);
    Quality:=CheckLazarusDirectoryQuality(CurCaption,Note);
  end;
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  LazDirMemo.Text:=s+Note;

  if Quality=sddqCompatible then
    ImageIndex:=-1
  else
    ImageIndex:=ImgIDError;
  TVNodeLazarus.ImageIndex:=ImageIndex;
  TVNodeLazarus.SelectedIndex:=ImageIndex;
end;

procedure TInitialSetupDialog.UpdateCompilerNote;

  function NormFile(const AFilename: string): string;
  var
    Macros: TSetupMacros;
  begin
    Result:=TrimFilename(AFilename);
    if TSetupMacros.StrHasMacros(Result) then begin
      Macros:=TSetupMacros.Create;
      try
        Macros.LazarusDir:=LazarusDir;
        Macros.SubstituteStr(Result);
      finally
        Macros.Free;
      end;
    end;
    if Result<>'' then
      Result:=TrimFilename(ExpandFileNameUTF8(Result));
  end;

var
  i: Integer;
  Files: TObjectList;
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
  CompilerFile: String;
  CfgCache: TFPCTargetConfigCache;
begin
  i:=-1;
  Files:=FDirs[sddtCompilerFilename];
  CurCaption:=CompilerComboBox.Text;
  if Files<>nil then begin
    i:=Files.Count-1;
    while (i>=0) and (TSDFileInfo(Files[i]).Caption<>CurCaption) do dec(i);
  end;
  if i>=0 then begin
    Quality:=TSDFileInfo(Files[i]).Quality;
    Note:=TSDFileInfo(Files[i]).Note;
    CompilerFile:=NormFile(TSDFileInfo(Files[i]).Filename);
  end else begin
    CompilerFile:=NormFile(CurCaption);
    Quality:=CheckCompilerQuality(CurCaption,Note,
                                  CodeToolBoss.FPCDefinesCache.TestFilename);
  end;
  if Quality=sddqInvalid then
    FPCVer:=''
  else begin
    CfgCache:=CodeToolBoss.FPCDefinesCache.ConfigCaches.Find(
      CompilerFile,'','','',true);
    FPCVer:=CfgCache.GetFPCVer;
  end;

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  CompilerMemo.Text:=s+Note;

  if Quality=sddqCompatible then
    ImageIndex:=-1
  else
    ImageIndex:=ImgIDError;
  TVNodeCompiler.ImageIndex:=ImageIndex;
  TVNodeCompiler.SelectedIndex:=ImageIndex;
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirNote;

  function NormDir(const AFilename: string): string;
  var
    Macros: TSetupMacros;
  begin
    Result:=TrimFilename(AFilename);
    if TSetupMacros.StrHasMacros(Result) then begin
      Macros:=TSetupMacros.Create;
      try
        Macros.LazarusDir:=LazarusDir;
        Macros.FPCVer:=FPCVer;
        Macros.SubstituteStr(Result);
      finally
        Macros.Free;
      end;
    end;
    Result:=ChompPathDelim(Result);
    if Result<>'' then
      Result:=ChompPathDelim(TrimFilename(ExpandFileNameUTF8(Result)));
  end;

var
  i: Integer;
  Dirs: TObjectList;
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  i:=-1;
  Dirs:=FDirs[sddtFPCSrcDir];
  CurCaption:=FPCSrcDirComboBox.Text;
  if Dirs<>nil then begin
    i:=Dirs.Count-1;
    while (i>=0) and (TSDFileInfo(Dirs[i]).Caption<>CurCaption) do dec(i);
  end;
  if i>=0 then begin
    Quality:=TSDFileInfo(Dirs[i]).Quality;
    Note:=TSDFileInfo(Dirs[i]).Note;
  end else begin
    Quality:=CheckFPCSrcDirQuality(CurCaption,Note,FPCVer);
  end;
  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  FPCSrcDirMemo.Text:=s+Note;

  if Quality=sddqCompatible then
    ImageIndex:=-1
  else
    ImageIndex:=ImgIDError;
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

function TInitialSetupDialog.GetCurrentLazarusDir: string;
var
  Dirs: TObjectList;
  i: Integer;
begin
  Dirs:=FDirs[sddtLazarusSrcDir];
  Result:=LazDirComboBox.Text;
  if Dirs<>nil then begin
    i:=Dirs.Count-1;
    while (i>=0) and (TSDFileInfo(Dirs[i]).Caption<>Result) do dec(i);
    if i>=0 then
      Result:=TSDFileInfo(Dirs[i]).Filename;
  end;
  Result:=ChompPathDelim(TrimFilename(Result));
  if Result<>'' then
    Result:=ChompPathDelim(TrimFilename(ExpandFileNameUTF8(Result)));
end;

function TInitialSetupDialog.GetCurrentCompilerFilename: string;
var
  Dirs: TObjectList;
  i: Integer;
begin
  Dirs:=FDirs[sddtCompilerFilename];
  Result:=CompilerComboBox.Text;
  if Dirs<>nil then begin
    i:=Dirs.Count-1;
    while (i>=0) and (TSDFileInfo(Dirs[i]).Caption<>Result) do dec(i);
    if i>=0 then
      Result:=TSDFileInfo(Dirs[i]).Filename;
  end;
  Result:=TrimFilename(Result);
end;

function TInitialSetupDialog.GetCurrentFPCSrcDir: string;
var
  Dirs: TObjectList;
  i: Integer;
begin
  Dirs:=FDirs[sddtFPCSrcDir];
  Result:=FPCSrcDirComboBox.Text;
  if Dirs<>nil then begin
    i:=Dirs.Count-1;
    while (i>=0) and (TSDFileInfo(Dirs[i]).Caption<>Result) do dec(i);
    if i>=0 then
      Result:=TSDFileInfo(Dirs[i]).Filename;
  end;
  Result:=ChompPathDelim(TrimFilename(Result));
end;

procedure TInitialSetupDialog.Init;
var
  Node: TTreeNode;
begin
  InitLazarusDir;
  InitCompilerFilename;
  InitFPCSrcDir;
  // select first error
  Node:=FirstErrorNode;
  if Node=nil then
    Node:=TVNodeLazarus;
  PropertiesTreeView.Selected:=Node;
end;

end.

