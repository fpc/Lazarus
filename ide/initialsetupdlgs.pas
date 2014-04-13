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

{off $DEFINE VerboseFPCSrcScanThead}

interface

uses
  Classes, SysUtils, strutils, contnrs, LCLProc, Forms, Controls, Buttons,
  Dialogs, FileUtil, Laz2_XMLCfg, lazutf8classes, LazFileUtils, Graphics,
  ComCtrls, ExtCtrls, StdCtrls, DefineTemplates, CodeToolManager,
  TransferMacros, MacroDefIntf, GDBMIDebugger, Debugger, LazarusIDEStrConsts,
  LazConf, EnvironmentOpts, IDEProcs, AboutFrm, IDETranslations;

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

  TInitialSetupDialog = class;

  { TSearchFpcSourceThread }

  TSearchFpcSourceThread = class(TThread)
  private
    fSetupDialog: TInitialSetupDialog;
    fFPCVer: string;
    fFoundFPCSrc: TSDFileInfo;
    {$IFDEF VerboseFPCSrcScanThead}
    fPath: string;
    fFileInfo: TSearchRec;
    procedure Debug;
    {$ENDIF}
    function CheckFPCSrcDir(Dir: string): TSDFileInfo;
    procedure DoSearch(const APath: String);
    procedure UpdateFPCSrcDir;
    procedure Finishing;
  protected
    procedure Execute; override;
  public
    constructor Create(aSetupDialog: TInitialSetupDialog);
    destructor Destroy; override;
  end;

  { TInitialSetupDialog }

  TInitialSetupDialog = class(TForm)
    BtnPanel: TPanel;
    CompilerBrowseButton: TButton;
    CompilerComboBox: TComboBox;
    CompilerLabel: TLabel;
    CompilerMemo: TMemo;
    CompilerTabSheet: TTabSheet;
    DebuggerBrowseButton: TButton;
    DebuggerComboBox: TComboBox;
    DebuggerLabel: TLabel;
    DebuggerMemo: TMemo;
    DebuggerTabSheet: TTabSheet;
    FPCSourcesTabSheet: TTabSheet;
    FPCSrcDirBrowseButton: TButton;
    FPCSrcDirComboBox: TComboBox;
    FPCSrcDirLabel: TLabel;
    FPCSrcDirMemo: TMemo;
    ImageList1: TImageList;
    LazarusTabSheet: TTabSheet;
    LazDirBrowseButton: TButton;
    LazDirComboBox: TComboBox;
    LazDirLabel: TLabel;
    LazDirMemo: TMemo;
    MakeExeBrowseButton: TButton;
    MakeExeComboBox: TComboBox;
    MakeExeLabel: TLabel;
    MakeExeMemo: TMemo;
    MakeExeTabSheet: TTabSheet;
    PropertiesPageControl: TPageControl;
    PropertiesTreeView: TTreeView;
    ScanLabel: TLabel;
    ScanProgressBar: TProgressBar;
    Splitter1: TSplitter;
    StartIDEBitBtn: TBitBtn;
    StopScanButton: TBitBtn;
    WelcomePaintBox: TPaintBox;
    procedure CompilerBrowseButtonClick(Sender: TObject);
    procedure CompilerComboBoxChange(Sender: TObject);
    procedure DebuggerBrowseButtonClick(Sender: TObject);
    procedure DebuggerComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPCSrcDirBrowseButtonClick(Sender: TObject);
    procedure FPCSrcDirComboBoxChange(Sender: TObject);
    procedure LazDirBrowseButtonClick(Sender: TObject);
    procedure LazDirComboBoxChange(Sender: TObject);
    procedure MakeExeBrowseButtonClick(Sender: TObject);
    procedure MakeExeComboBoxChange(Sender: TObject);
    procedure OnAppActivate(Sender: TObject);
    procedure PropertiesPageControlChange(Sender: TObject);
    procedure PropertiesTreeViewSelectionChanged(Sender: TObject);
    procedure StartIDEBitBtnClick(Sender: TObject);
    procedure StopScanButtonClick(Sender: TObject);
    procedure WelcomePaintBoxPaint(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    FFlags: TSDFlags;
    FLastParsedLazDir: string;
    fLastParsedCompiler: string;
    fLastParsedFPCSrcDir: string;
    fLastParsedMakeExe: string;
    fLastParsedDebugger: string;
    FIdleConnected: boolean;
    ImgIDError: LongInt;
    ImgIDWarning: LongInt;
    FHeadGraphic: TPortableNetworkGraphic;
    FInitialDebuggerFileName: String;
    FSelectingPage: boolean;
    FCandidates: array[TSDFilenameType] of TSDFileInfoList; // list of TSDFileInfo
    fSearchFpcSourceThread: TSearchFpcSourceThread;
    procedure UpdateCaptions;
    procedure SelectPage(const NodeText: string);
    function SelectDirectory(aTitle: string): string;
    procedure StartFPCSrcThread;
    procedure UpdateLazarusDirCandidates;
    procedure UpdateCompilerFilenameCandidates;
    procedure UpdateFPCSrcDirCandidates;
    procedure UpdateFPCSrcDirCandidate(aFPCSrcDirInfo: TSDFileInfo);
    procedure UpdateMakeExeCandidates;
    procedure UpdateDebuggerCandidates;
    procedure FillComboboxWithFileInfoList(ABox: TComboBox; List: TSDFileInfoList;
       ItemIndex: integer = 0);
    procedure SetIdleConnected(const AValue: boolean);
    procedure UpdateLazDirNote;
    procedure UpdateCompilerNote;
    procedure UpdateFPCSrcDirNote;
    procedure UpdateMakeExeNote;
    procedure UpdateDebuggerNote;
    function FirstErrorNode: TTreeNode;
    function GetFPCVer: string;
    function GetFirstCandidate(Candidates: TSDFileInfoList;
      MinQuality: TSDFilenameQuality = sddqCompatible): TSDFileInfo;
    function QualityToImgIndex(Quality: TSDFilenameQuality): integer;
    procedure ShowHideScanControls(aShow: Boolean);
    procedure ThreadTerminated(Sender: TObject); // called in main thread by fSearchFpcSourceThread.OnTerminate
    procedure TranslateResourceStrings;
  public
    TVNodeLazarus: TTreeNode;
    TVNodeCompiler: TTreeNode;
    TVNodeFPCSources: TTreeNode;
    TVNodeMakeExe: TTreeNode;
    TVNodeDebugger: TTreeNode;
    procedure Init; //Check for config errors, find and show alternatives
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;


function ShowInitialSetupDialog: TModalResult;

// Lazarus Directory
function CheckLazarusDirectoryQuality(ADirectory: string; out Note: string): TSDFilenameQuality;
function SearchLazarusDirectoryCandidates(StopIfFits: boolean): TSDFileInfoList;
procedure SetupLazarusDirectory;

// Compiler
function CheckCompilerQuality(AFilename: string; out Note: string;
  TestSrcFilename: string): TSDFilenameQuality;
function SearchCompilerCandidates(StopIfFits: boolean;
  const TestSrcFilename: string): TSDFileInfoList;
procedure SetupCompilerFilename;

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

// Debugger
// Checks a given file to see if it is a valid debugger (only gdb supported for now)
function CheckDebuggerQuality(AFilename: string; out Note: string): TSDFilenameQuality;
// Search debugger candidates and add them to list, including quality level
function SearchDebuggerCandidates(StopIfFits: boolean): TSDFileInfoList;

function GetValueFromPrimaryConfig(OptionFilename, Path: string): string;
function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
function GetValueFromIDEConfig(OptionFilename, Path: string): string;

implementation

const
  DefaultDebuggerClass: TDebuggerClass = TGDBMIDebugger;

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
    debugln(['SearchLazarusDirectoryCandidates Value=',Dir,' File=',RealDir]);
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
    Item.Quality:=CheckCompilerQuality(RealFilename, Item.Note, TestSrcFilename);
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

procedure SetupCompilerFilename;
var
  Filename, Note: String;
  Quality: TSDFilenameQuality;
  List: TSDFileInfoList;
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
  List:=SearchCompilerCandidates(true, CodeToolBoss.FPCDefinesCache.TestFilename);
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
    SubDir:=SetDirSeparators(SubDir);
    if DirPathExistsInternal(ADirectory+SubDir) then exit(true);
    Result:=false;
    Note:=Format(lisDirectoryNotFound2, [SubDir]);
  end;

  function SubFileExists(SubFile: string): boolean;
  begin
    SubFile:=SetDirSeparators(SubFile);
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
    Dir:=ChompPathDelim(SetDirSeparators(Dir));
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

function CheckDebuggerQuality(AFilename: string; out Note: string): TSDFilenameQuality;
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

  { We could call gdb and parse the output looking for something like
  GNU gdb, but that may be going too far. }
  Note:=lisOk;
  Result:=sddqCompatible;
end;

function SearchDebuggerCandidates(StopIfFits: boolean): TSDFileInfoList;

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
    EnvironmentOptions.DebuggerFilename:=AFilename;
    RealFilename:=EnvironmentOptions.GetParsedDebuggerFilename;
    debugln(['SearchDebuggerCandidates Value=',AFilename,' File=',RealFilename]);
    if RealFilename='' then exit;
    // check if exists
    if not FileExistsCached(RealFilename) then exit;
    // add to list and check quality
    if List=nil then
      List:=TSDFileInfoList.create(true);
    Item:=List.AddNewItem(RealFilename, AFilename);
    Item.Quality:=CheckDebuggerQuality(RealFilename, Item.Note);
    Result:=(Item.Quality=sddqCompatible) and StopIfFits;
  end;

const
  DebuggerFileName='gdb'; //For Windows, .exe will be appended
var
  OldDebuggerFilename: String;
  s, AFilename: String;
  Files: TStringList;
  i: Integer;
begin
  Result:=nil;

  OldDebuggerFilename:=EnvironmentOptions.DebuggerFilename;
  try
    // check current setting
    if CheckFile(EnvironmentOptions.DebuggerFilename,Result) then exit;

    // check the primary options
    AFilename:=GetValueFromPrimaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // check the secondary options
    AFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
                                    'EnvironmentOptions/DebuggerFilename/Value');
    if CheckFile(AFilename,Result) then exit;

    // Check locations proposed by debugger class
    s := DefaultDebuggerClass.ExePaths;
    while s <> '' do begin
      AFilename := GetPart([], [';'], s);
      if CheckFile(AFilename, Result) then exit;
      if s <> '' then delete(s, 1, 1);
    end;


    // Windows-only locations:
    if (GetDefaultSrcOSForTargetOS(GetCompiledTargetOS)='win') then begin
      // check for debugger in fpc.exe directory - could be a lucky shot
      if CheckFile(SetDirSeparators('$Path($(CompPath))/'+DebuggerFileName+GetExecutableExt),Result)
        then exit;
    end;

    // check history
    Files:=EnvironmentOptions.DebuggerFileHistory;
    if Files<>nil then
      for i:=0 to Files.Count-1 do
        if CheckFile(Files[i],Result) then exit;

    // check PATH
    AFilename:=DebuggerFileName+GetExecutableExt;
    if CheckFile(AFilename,Result) then exit;

    // There are no common directories apart from the PATH
    // where gdb would be installed. Otherwise we could do something similar as
    // in SearchMakeExeCandidates.
  finally
    EnvironmentOptions.DebuggerFilename:=OldDebuggerFilename;
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

{ TSearchFpcSourceThread }

constructor TSearchFpcSourceThread.Create(aSetupDialog: TInitialSetupDialog);
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  fSetupDialog:=aSetupDialog;
end;

destructor TSearchFpcSourceThread.Destroy;
begin
  inherited Destroy;
end;

procedure TSearchFpcSourceThread.Execute;
var
  RootDir: String;
begin
  // ToDo: RootDir must be changed for Windows and maybe other systems.
  //       GetUserDir returns the user profile dir on Windows.
  RootDir:=GetUserDir;
  // Scan directories under root directory.
  DoSearch(AppendPathDelim(RootDir));
  if Assigned(fFoundFPCSrc) then
    Synchronize(@UpdateFPCSrcDir); // Update GUI in main thread.
  Synchronize(@Finishing);
end;

function TSearchFpcSourceThread.CheckFPCSrcDir(Dir: string): TSDFileInfo;
var
  RealDir: String;
begin
  Result:=Nil;
  RealDir:=TrimFilename(Dir);
  if RealDir='' then exit;
  if not DirPathExistsCached(RealDir) then exit;   // check if exists
  Result:=TSDFileInfo.Create;
  Result.Filename:=RealDir;
  Result.Caption:=Dir;                             // check quality
  Result.Quality:=CheckFPCSrcDirQuality(RealDir, Result.Note, fFPCVer, False);
  if Result.Quality<>sddqCompatible then           // return only exact matches
    FreeAndNil(Result);
end;

procedure TSearchFpcSourceThread.DoSearch(const APath: String);
var
  PathInfo: TSearchRec;
  FPCSrc: TSDFileInfo;
begin
  if FindFirstUTF8(APath+AllDirectoryEntriesMask, faDirectory, PathInfo) = 0 then
  try
    repeat
      if Terminated then Break;
      if (PathInfo.Name='') or (PathInfo.Name[1]='.')
      or ((PathInfo.Attr and faDirectory) = 0) then Continue;
      {$IFDEF VerboseFPCSrcScanThead}
      fPath := APath;
      fFileInfo := PathInfo;
      Synchronize(@Debug);
      {$ENDIF}
      DoSearch(AppendPathDelim(APath+PathInfo.Name));  // Recursive call
      FPCSrc:=CheckFPCSrcDir(APath+PathInfo.Name);
      if Assigned(FPCSrc) then begin
        fFoundFPCSrc:=FPCSrc;                 // An exact match was found.
        Terminate;
      end;
    until (FindNextUTF8(PathInfo) <> 0);
  finally
    FindCloseUTF8(PathInfo);
  end;
end;

{$IFDEF VerboseFPCSrcScanThead}
procedure TSearchFpcSourceThread.Debug;
begin
  DebugLn(['* TSearchFpcSourceThread.Debug: Path=', fPath, ', Name=', fFileInfo.Name]);
end;
{$ENDIF}

procedure TSearchFpcSourceThread.UpdateFPCSrcDir;
begin
  DebugLn(['TSearchFpcSourceThread.UpdateFPCSrcDir']);
  fSetupDialog.UpdateFPCSrcDirCandidate(fFoundFPCSrc);
  fSetupDialog.UpdateFPCSrcDirNote;
end;

procedure TSearchFpcSourceThread.Finishing;
begin
  DebugLn(['TSearchFpcSourceThread.Finishing']);
  fSetupDialog.ShowHideScanControls(False); // Hide scan controls
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
  MakeExeTabSheet.Caption:='Make';
  DebuggerTabSheet.Caption:=lisDebugger;

  FHeadGraphic:=TPortableNetworkGraphic.Create;
  FHeadGraphic.LoadFromLazarusResource('ide_icon48x48');

  TVNodeLazarus:=PropertiesTreeView.Items.Add(nil,LazarusTabSheet.Caption);
  TVNodeCompiler:=PropertiesTreeView.Items.Add(nil,CompilerTabSheet.Caption);
  TVNodeFPCSources:=PropertiesTreeView.Items.Add(nil,FPCSourcesTabSheet.Caption);
  TVNodeMakeExe:=PropertiesTreeView.Items.Add(nil,MakeExeTabSheet.Caption);
  TVNodeDebugger:=PropertiesTreeView.Items.Add(nil,DebuggerTabSheet.Caption);
  ImgIDError := ImageList1.AddLazarusResource('state_error');
  ImgIDWarning := ImageList1.AddLazarusResource('state_warning');

  StopScanButton.LoadGlyphFromLazarusResource('menu_stop');

  UpdateCaptions;

  Application.AddOnActivateHandler(@OnAppActivate);
end;

procedure TInitialSetupDialog.CompilerComboBoxChange(Sender: TObject);
begin
  UpdateCompilerNote;
  UpdateFPCSrcDirNote;
end;

procedure TInitialSetupDialog.DebuggerBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='gdb'+GetExecutableExt;
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
  DebuggerComboBox.Text:=Filename;
  UpdateDebuggerNote;
end;

procedure TInitialSetupDialog.DebuggerComboBoxChange(Sender: TObject);
begin
  UpdateDebuggerNote;
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
  if Assigned(fSearchFpcSourceThread) then begin
    fSearchFpcSourceThread.Terminate;
    fSearchFpcSourceThread.WaitFor;
  end;
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

procedure TInitialSetupDialog.MakeExeBrowseButtonClick(Sender: TObject);
var
  Filename: String;
  Dlg: TOpenDialog;
  Filter: String;
begin
  Dlg:=TOpenDialog.Create(nil);
  try
    Filename:='make'+GetExecutableExt;
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
  MakeExeComboBox.Text:=Filename;
  UpdateMakeExeNote;
end;

procedure TInitialSetupDialog.MakeExeComboBoxChange(Sender: TObject);
begin
  UpdateMakeExeNote;
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
    s:=lisWithoutTheProperFPCSourcesCodeBrowsingAndCompletio
  else if Node=TVNodeMakeExe then
    s:=lisWithoutAProperMakeExecutableTheCompilingOfTheIDEIs
  else if Node=TVNodeDebugger then
    s:=lisWithoutAProperDebuggerDebuggingWillBeDisappointing;
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
  s:=MakeExeComboBox.Text;
  if s<>'' then
    EnvironmentOptions.MakeFilename:=s;
  s:=DebuggerComboBox.Text;
  if s<>'' then begin
    EnvironmentOptions.DebuggerFilename:=s;
    if s <> FInitialDebuggerFileName then
      EnvironmentOptions.DebuggerConfig.DebuggerClass := 'TGDBMIDebugger';
  end;

  ModalResult:=mrOk;
end;

procedure TInitialSetupDialog.StopScanButtonClick(Sender: TObject);
begin
  if fSearchFpcSourceThread<>nil then
    fSearchFpcSourceThread.Terminate;
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
  if sdfCompilerFilenameNeedsUpdate in FFlags then begin
    UpdateCompilerFilenameCandidates;
    UpdateCompilerNote;
  end else if sdfFPCSrcDirNeedsUpdate in FFlags then begin
    UpdateFPCSrcDirCandidates;
    UpdateFPCSrcDirNote;
  end else if sdfMakeExeFilenameNeedsUpdate in FFlags then begin
    UpdateMakeExeCandidates;
    UpdateMakeExeNote;
  end else if sdfDebuggerFilenameNeedsUpdate in FFlags then begin
    UpdateDebuggerCandidates;
    UpdateDebuggerNote;
  end else
    IdleConnected:=false;
end;

procedure TInitialSetupDialog.UpdateCaptions;
var
  s: String;
begin
  Caption:=Format(lisWelcomeToLazarusIDE, [GetLazarusVersionString]);

  StartIDEBitBtn.Caption:=lisStartIDE;

  LazarusTabSheet.Caption:='Lazarus';
  CompilerTabSheet.Caption:=lisCompiler;
  FPCSourcesTabSheet.Caption:=lisFPCSources;
  MakeExeTabSheet.Caption:='Make';
  DebuggerTabSheet.Caption:=lisDebugger;

  TVNodeLazarus.Text:=LazarusTabSheet.Caption;
  TVNodeCompiler.Text:=CompilerTabSheet.Caption;
  TVNodeFPCSources.Text:=FPCSourcesTabSheet.Caption;
  TVNodeMakeExe.Text:=MakeExeTabSheet.Caption;
  TVNodeDebugger.Text:=DebuggerTabSheet.Caption;

  LazDirBrowseButton.Caption:=lisPathEditBrowse;
  LazDirLabel.Caption:=Format(
    lisTheLazarusDirectoryContainsTheSourcesOfTheIDEAndTh, [PathDelim]);

  CompilerBrowseButton.Caption:=lisPathEditBrowse;
  CompilerLabel.Caption:=Format(lisTheFreePascalCompilerExecutableTypicallyHasTheName,
    [DefineTemplates.GetDefaultCompilerFilename,
     DefineTemplates.GetDefaultCompilerFilename(GetCompiledTargetCPU)]);

  FPCSrcDirBrowseButton.Caption:=lisPathEditBrowse;
  FPCSrcDirLabel.Caption:=Format(lisTheSourcesOfTheFreePascalPackagesAreRequiredForBro,
    [SetDirSeparators('rtl/linux/system.pp')]);
  ScanLabel.Caption := lisScanning;
  StopScanButton.Caption:=lisStop;

  MakeExeBrowseButton.Caption:=lisPathEditBrowse;
  MakeExeLabel.Caption:=Format(
    lisTheMakeExecutableTypicallyHasTheName, ['make'+GetExecutableExt('')]);

  DebuggerBrowseButton.Caption:=lisPathEditBrowse;
  s:=Format(lisTheDebuggerExecutableTypicallyHasTheNamePleaseGive, [
    'gdb'+GetExecutableExt]);
  {$IFDEF Windows}
  s+=' '+lisAUsefulSettingOnWindowsSystemsIsLazarusDirMingwBin;
  {$ENDIF}
  DebuggerLabel.Caption:=s;
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

procedure TInitialSetupDialog.StartFPCSrcThread;
begin
  fSearchFpcSourceThread:=TSearchFpcSourceThread.Create(Self);
  fSearchFpcSourceThread.OnTerminate:=@ThreadTerminated;
  fSearchFpcSourceThread.fFPCVer:=GetFPCVer;
  ShowHideScanControls(True); // Show scan controls while thread is running
  fSearchFpcSourceThread.Start;
end;

procedure TInitialSetupDialog.UpdateLazarusDirCandidates;
var
  Dirs: TSDFileInfoList;
begin
  Dirs:=SearchLazarusDirectoryCandidates(false);
  FreeAndNil(FCandidates[sddtLazarusSrcDir]);
  FCandidates[sddtLazarusSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(LazDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateCompilerFilenameCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfCompilerFilenameNeedsUpdate);
  Files:=SearchCompilerCandidates(false,CodeToolBoss.FPCDefinesCache.TestFilename);
  FreeAndNil(FCandidates[sddtCompilerFilename]);
  FCandidates[sddtCompilerFilename]:=Files;
  FillComboboxWithFileInfoList(CompilerComboBox,Files);
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirCandidates;
var
  Dirs: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFPCSrcDirNeedsUpdate);
  Dirs:=SearchFPCSrcDirCandidates(false,GetFPCVer);
  FreeAndNil(FCandidates[sddtFPCSrcDir]);
  FCandidates[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateFPCSrcDirCandidate(aFPCSrcDirInfo: TSDFileInfo);
var
  Dirs: TSDFileInfoList;
begin
  Exclude(FFlags,sdfFPCSrcDirNeedsUpdate);
  FreeAndNil(FCandidates[sddtFPCSrcDir]);
  Dirs:=TSDFileInfoList.Create;
  Dirs.Add(aFPCSrcDirInfo);
  FCandidates[sddtFPCSrcDir]:=Dirs;
  FillComboboxWithFileInfoList(FPCSrcDirComboBox,Dirs);
end;

procedure TInitialSetupDialog.UpdateMakeExeCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfMakeExeFilenameNeedsUpdate);
  Files:=SearchMakeExeCandidates(false);
  FreeAndNil(FCandidates[sddtMakeExeFileName]);
  FCandidates[sddtMakeExeFileName]:=Files;
  FillComboboxWithFileInfoList(MakeExeComboBox,Files);
end;

procedure TInitialSetupDialog.UpdateDebuggerCandidates;
var
  Files: TSDFileInfoList;
begin
  Exclude(FFlags,sdfDebuggerFilenameNeedsUpdate);
  Files:=SearchDebuggerCandidates(false);
  FreeAndNil(FCandidates[sddtDebuggerFilename]);
  FCandidates[sddtDebuggerFilename]:=Files;
  FillComboboxWithFileInfoList(DebuggerComboBox,Files);
end;

procedure TInitialSetupDialog.FillComboboxWithFileInfoList(ABox: TComboBox;
  List: TSDFileInfoList; ItemIndex: integer);
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

  FFlags:=FFlags+[sdfCompilerFilenameNeedsUpdate,sdfFPCSrcDirNeedsUpdate,
                  sdfMakeExeFilenameNeedsUpdate,sdfDebuggerFilenameNeedsUpdate];
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

  FFlags:=FFlags+[sdfFPCSrcDirNeedsUpdate,
                  sdfMakeExeFilenameNeedsUpdate,sdfDebuggerFilenameNeedsUpdate];
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
  s+=Note;
  if Quality<>sddqCompatible then
    s+=#13+lisYouCanDownloadFPCAndTheFPCSourcesFromHttpSourcefor;
  FPCSrcDirMemo.Text:=s;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeFPCSources.ImageIndex:=ImageIndex;
  TVNodeFPCSources.SelectedIndex:=ImageIndex;
end;

procedure TInitialSetupDialog.UpdateMakeExeNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=MakeExeComboBox.Text;
  EnvironmentOptions.MakeFilename:=CurCaption;
  if fLastParsedMakeExe=EnvironmentOptions.GetParsedMakeFilename then exit;
  fLastParsedMakeExe:=EnvironmentOptions.GetParsedMakeFilename;
  //debugln(['TInitialSetupDialog.UpdateMakeExeNote ',fLastParsedMakeExe]);
  Quality:=CheckMakeExeQuality(fLastParsedMakeExe,Note);

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.MakeFilename<>EnvironmentOptions.GetParsedMakeFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedMakeFilename+LineEnding+
      LineEnding+s;
  MakeExeMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeMakeExe.ImageIndex:=ImageIndex;
  TVNodeMakeExe.SelectedIndex:=ImageIndex;

  IdleConnected:=true;
end;

procedure TInitialSetupDialog.UpdateDebuggerNote;
var
  CurCaption: String;
  Note: string;
  Quality: TSDFilenameQuality;
  s: String;
  ImageIndex: Integer;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=DebuggerComboBox.Text;
  EnvironmentOptions.DebuggerFilename:=CurCaption;
  if fLastParsedDebugger=EnvironmentOptions.GetParsedDebuggerFilename then exit;
  fLastParsedDebugger:=EnvironmentOptions.GetParsedDebuggerFilename;
  //debugln(['TInitialSetupDialog.UpdateDebuggerNote ',fLastParsedDebugger]);
  Quality:=CheckDebuggerQuality(fLastParsedDebugger,Note);

  case Quality of
  sddqInvalid: s:=lisError;
  sddqCompatible: s:='';
  else s:=lisWarning;
  end;
  if EnvironmentOptions.DebuggerFilename<>EnvironmentOptions.GetParsedDebuggerFilename
  then
    s:=lisFile2+EnvironmentOptions.GetParsedDebuggerFilename+LineEnding+
      LineEnding+s;
  DebuggerMemo.Text:=s+Note;

  ImageIndex:=QualityToImgIndex(Quality);
  TVNodeDebugger.ImageIndex:=ImageIndex;
  TVNodeDebugger.SelectedIndex:=ImageIndex;

  IdleConnected:=true;
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

function TInitialSetupDialog.GetFirstCandidate(Candidates: TSDFileInfoList;
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

function TInitialSetupDialog.QualityToImgIndex(Quality: TSDFilenameQuality): integer;
begin
  if Quality=sddqCompatible then
    Result:=-1
  else if Quality=sddqWrongMinorVersion then
    Result:=ImgIDWarning
  else if Quality=sddqIncomplete then
    Result:=ImgIDWarning
  else
    Result:=ImgIDError;
end;

procedure TInitialSetupDialog.ShowHideScanControls(aShow: Boolean);
begin
  // Show ProgressBar and Stop button durin scanning
  ScanLabel.Visible:=aShow;
  ScanProgressBar.Visible:=aShow;
  StopScanButton.Visible:=aShow;
  // At the same time disable other GUI controls so a user can not mess with it
  StartIDEBitBtn.Enabled:=not aShow;
  FPCSrcDirBrowseButton.Enabled:=not aShow;
  FPCSrcDirComboBox.Enabled:=not aShow;
//  FPCSrcDirMemo.Enabled:=not aShow;
end;

procedure TInitialSetupDialog.ThreadTerminated(Sender: TObject);
begin
  debugln(['TInitialSetupDialog.ThreadTerminated ']);
  fSearchFpcSourceThread:=nil; // Thread will free itself. Make the variable nil, too.
  ShowHideScanControls(false);
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
  if IsFirstStart
  or (not FileExistsCached(EnvironmentOptions.GetParsedLazarusDirectory))
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

  // compiler filename
  UpdateCompilerFilenameCandidates;
  if IsFirstStart
  or (EnvironmentOptions.CompilerFilename='')
  or (not FileExistsCached(EnvironmentOptions.GetParsedCompilerFilename))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtCompilerFilename]);
    if Candidate<>nil then
      EnvironmentOptions.CompilerFilename:=Candidate.Caption;
  end;
  CompilerComboBox.Text:=EnvironmentOptions.CompilerFilename;
  fLastParsedCompiler:='. .';
  UpdateCompilerNote;

  // FPC source directory
  UpdateFPCSrcDirCandidates;
  {$IFDEF DebugSearchFPCSrcThread}
  IsFirstStart:=true;
  {$ENDIF}
  if IsFirstStart or (EnvironmentOptions.FPCSourceDirectory='')
  or (not FileExistsCached(EnvironmentOptions.GetParsedFPCSourceDirectory))
  then begin
    // first start => choose first best candidate
    {$IFDEF DebugSearchFPCSrcThread}
    Candidate:=nil;
    {$ELSE}
    Candidate:=GetFirstCandidate(FCandidates[sddtFPCSrcDir]);
    {$ENDIF}
    if Candidate<>nil then begin
      EnvironmentOptions.FPCSourceDirectory:=Candidate.Caption;
    end
    else begin
      // No candidates found => start a thread to scan the file system.
      {$IFNDEF LCLCarbon}
      // carbon interface does not support Synchronize outside Application.Run
      StartFPCSrcThread;
      SelectPage(TVNodeFPCSources.Text);
      {$ENDIF}
    end;
  end;
  ShowHideScanControls(fSearchFpcSourceThread<>nil);
  FPCSrcDirComboBox.Text:=EnvironmentOptions.FPCSourceDirectory;
  fLastParsedFPCSrcDir:='. .';
  UpdateFPCSrcDirNote;

  // Make executable
  UpdateMakeExeCandidates;
  if IsFirstStart or (EnvironmentOptions.MakeFilename='') then
  begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtMakeExeFilename]);
    if Candidate<>nil then
      EnvironmentOptions.MakeFilename:=Candidate.Caption;
  end;
  MakeExeComboBox.Text:=EnvironmentOptions.MakeFilename;
  fLastParsedMakeExe:='. .';
  UpdateMakeExeNote;

  // Debugger
  FInitialDebuggerFileName := EnvironmentOptions.DebuggerFilename;
  UpdateDebuggerCandidates;
  if IsFirstStart or (not FileExistsCached(EnvironmentOptions.GetParsedDebuggerFilename))
  then begin
    // first start => choose first best candidate
    Candidate:=GetFirstCandidate(FCandidates[sddtDebuggerFilename]);
    if Candidate<>nil then
      EnvironmentOptions.DebuggerFilename:=Candidate.Caption;
  end;
  DebuggerComboBox.Text:=EnvironmentOptions.DebuggerFilename;
  fLastParsedDebugger:='. .';
  UpdateDebuggerNote;

  // select first error
  if PropertiesTreeView.Selected=nil then begin
    Node:=FirstErrorNode;
    if Node=nil then
      Node:=TVNodeLazarus;
    PropertiesTreeView.Selected:=Node;
  end;
end;

end.

