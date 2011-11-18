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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Abstract:
   This dialog is typically called by the 'Test' button on the compiler options
   dialog.
   A dialog testing for common misconfigurations in some compiler options.
}
unit CheckCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, FileUtil,
  Clipbrd, StdCtrls, Buttons, Process, AVL_Tree, AsyncProcess, Menus, ExtCtrls,
  UTF8Process, ButtonPanel,
  // codetools
  KeywordFuncLists, CodeToolManager, FileProcs, DefineTemplates,
  CodeToolsStructs,
  // IDEIntf
  ProjectIntf, MacroIntf, IDEExternToolIntf,
  // IDE
  Project, PackageSystem, ExtToolEditDlg, IDEProcs, EnvironmentOpts,
  LazarusIDEStrConsts, PackageDefs, CompilerOptions, TransferMacros, LazConf;

type
  TCompilerOptionsTest = (
    cotNone,
    cotCheckCompilerExe,
    cotCheckAmbiguousFPCCfg,
    cotCheckMissingFPCPPUs,
    cotCheckCompilerDate,
    cotCheckCompilerConfig, // e.g. fpc.cfg
    cotCheckAmbiguousPPUsInUnitPath,
    cotCheckFPCUnitPathsContainSources,
    cotCompileBogusFiles
    );
    
  TCompilerCheckMsgLvl = (
    ccmlHint,
    ccmlWarning,
    ccmlError
    );

  { TCheckCompilerOptsDlg }

  TCheckCompilerOptsDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    CopyOutputMenuItem: TMenuItem;
    OutputPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    TestMemo: TMemo;
    TestGroupbox: TGroupBox;
    OutputListbox: TListbox;
    OutputGroupBox: TGroupBox;
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure CopyOutputMenuItemClick(Sender: TObject);
  private
    FIdleConnected: boolean;
    FMacroList: TTransferMacroList;
    FOptions: TCompilerOptions;
    FTest: TCompilerOptionsTest;
    FLastLineIsProgress: boolean;
    FDirectories: TStringList;
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure SetOptions(const AValue: TCompilerOptions);
    procedure SetMsgDirectory(Index: integer; const CurDir: string);
    function CheckSpecialCharsInPath(const Title, ExpandedPath: string): TModalResult;
    function CheckNonExistingSearchPaths(const Title, ExpandedPath: string): TModalResult;
    function CheckCompilerExecutable(const CompilerFilename: string): TModalResult;
    function CheckCompilerConfig(CfgCache: TFPCTargetConfigCache): TModalResult;
    function FindAllPPUFiles(const AnUnitPath: string): TStrings;
    function CheckMissingFPCPPUs(CfgCache: TFPCTargetConfigCache): TModalResult;
    function CheckCompilerDate(CfgCache: TFPCTargetConfigCache): TModalResult;
    function CheckForAmbiguousPPUs(SearchForPPUs: TStrings;
                                   SearchInPPUs: TStrings = nil): TModalResult;
    function CheckFPCUnitPathsContainSources(const FPCCfgUnitPath: string
                                              ): TModalResult;
    function CheckOutputPathInSourcePaths(CurOptions: TCompilerOptions): TModalResult;
    function CheckOrphanedPPUs(CurOptions: TCompilerOptions): TModalResult;
    function CheckCompileBogusFile(const CompilerFilename: string): TModalResult;
    function CheckPackagePathsIntersections(CurOptions: TCompilerOptions): TModalResult;
  public
    function DoTestAll: TModalResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function RunTool(ExtTool: TExternalToolOptions; ShowAbort: Boolean): TModalResult;
    procedure Add(const Msg, CurDir: String; ProgressLine: boolean;
                  OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: String; OriginalIndex: integer);
    procedure AddProgress(Line: TIDEScanMessageLine);
    procedure AddHint(const Msg: string);
    procedure AddWarning(const Msg: string);
    procedure AddMsg(const Level: TCompilerCheckMsgLvl; const Msg: string);
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  public
    property Options: TCompilerOptions read FOptions write SetOptions;
    property Test: TCompilerOptionsTest read FTest;
    property MacroList: TTransferMacroList read FMacroList write SetMacroList;
  end;

var
  CheckCompilerOptsDlg: TCheckCompilerOptsDlg;

type
  TCCOSpecialCharType = (
    ccoscNonASCII,
    ccoscWrongPathDelim,
    ccoscUnusualChars,
    ccoscSpecialChars,
    ccoscNewLine
    );
  TCCOSpecialChars = set of TCCOSpecialCharType;

procedure FindSpecialCharsInPath(const Path: string; out HasChars: TCCOSpecialChars);
function SpecialCharsToStr(const HasChars: TCCOSpecialChars): string;


implementation

{$R *.lfm}

procedure FindSpecialCharsInPath(const Path: string; out
  HasChars: TCCOSpecialChars);
var
  i: Integer;
begin
  HasChars := [];
  for i := 1 to length(Path) do
  begin
    case Path[i] of
      #10,#13: Include(HasChars,ccoscNewLine);
      #0..#9,#11,#12,#14..#31: Include(HasChars,ccoscSpecialChars);
      '/','\': if Path[i]<>PathDelim then Include(HasChars,ccoscWrongPathDelim);
      '@','#','$','&','*','(',')','[',']','+','~','<','>','?','|': Include(HasChars,ccoscUnusualChars);
      #128..#255: Include(HasChars,ccoscNonASCII);
    end;
  end;
end;

function SpecialCharsToStr(const HasChars: TCCOSpecialChars): string;

  procedure AddStr(var s: string; const Addition: string);
  begin
    if s='' then
      s:=lisCCOContains
    else
      s:=s+', ';
    s:=s+Addition;
  end;

begin
  Result:='';
  if ccoscNonASCII in HasChars then AddStr(Result,lisCCONonASCII);
  if ccoscWrongPathDelim in HasChars then AddStr(Result,lisCCOWrongPathDelimiter);
  if ccoscUnusualChars in HasChars then AddStr(Result,lisCCOUnusualChars);
  
  if ccoscSpecialChars in HasChars then AddStr(Result,lisCCOSpecialCharacters);
  if ccoscNewLine in HasChars then AddStr(Result,lisCCOHasNewLine);
end;

{ TCheckCompilerOptsDlg }

procedure TCheckCompilerOptsDlg.ApplicationOnIdle(Sender: TObject;
  var Done: Boolean);
begin
  IdleConnected:=false;
  DoTestAll;
end;

procedure TCheckCompilerOptsDlg.CopyOutputMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText:=OutputListbox.Items.Text;
end;

procedure TCheckCompilerOptsDlg.SetOptions(const AValue: TCompilerOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TCheckCompilerOptsDlg.SetMsgDirectory(Index: integer;
  const CurDir: string);
begin
  if FDirectories=nil then FDirectories:=TStringList.Create;
  while FDirectories.Count<=Index do FDirectories.Add('');
  FDirectories[Index]:=CurDir;
end;

function TCheckCompilerOptsDlg.CheckSpecialCharsInPath(const Title, ExpandedPath: string
  ): TModalResult;
var
  Warning: String;
  ErrorMsg: String;
  HasChars: TCCOSpecialChars;
begin
  FindSpecialCharsInPath(ExpandedPath, HasChars);
  Warning := SpecialCharsToStr(HasChars * [ccoscNonASCII, ccoscWrongPathDelim, ccoscUnusualChars]);
  ErrorMsg := SpecialCharsToStr(HasChars * [ccoscSpecialChars, ccoscNewLine]);

  if Warning <> '' then
    AddWarning(Title + ' ' + Warning);
  if ErrorMsg <> '' then
  begin
    Result := QuestionDlg(lisCCOInvalidSearchPath, Title + ' ' + ErrorMsg, mtError,
      [mrIgnore, lisCCOSkip, mrAbort], 0);
  end else
  begin
    if Warning = '' then
      Result := mrOk
    else
      Result := mrIgnore;
  end;
end;

function TCheckCompilerOptsDlg.CheckNonExistingSearchPaths(const Title,
  ExpandedPath: string): TModalResult;
var
  p: Integer;
  CurPath: String;
begin
  Result:=mrOk;
  p:=1;
  repeat
    CurPath:=GetNextDirectoryInSearchPath(ExpandedPath,p);
    if (CurPath<>'') and (not IDEMacros.StrHasMacros(CurPath))
    and (FilenameIsAbsolute(CurPath)) then begin
      if not DirPathExistsCached(CurPath) then begin
        AddWarning(Format(lisDoesNotExists, [Title, CurPath]));
      end;
    end;
  until p>length(ExpandedPath);
end;

function TCheckCompilerOptsDlg.CheckCompilerExecutable(
  const CompilerFilename: string): TModalResult;
var
  CompilerFiles: TStrings;
begin
  FTest:=cotCheckCompilerExe;
  TestGroupbox.Caption:=dlgCCOTestCheckingCompiler;
  try
    CheckIfFileIsExecutable(CompilerFilename);
  except
    on e: Exception do begin
      Result:=QuestionDlg(lisCCOInvalidCompiler,
        Format(lisCCOCompilerNotAnExe,[CompilerFilename,#13,E.Message]),
        mtError,[mrIgnore,lisCCOSkip,mrAbort],0);
      exit;
    end;
  end;

  // check if there are several compilers in path
  CompilerFiles:=SearchAllFilesInPath(GetDefaultCompilerFilename,'',
              GetEnvironmentVariableUTF8('PATH'),':',[sffDontSearchInBasePath]);
  try
    ResolveLinksInFileList(CompilerFiles,false);
    RemoveDoubles(CompilerFiles);
    if (CompilerFiles<>nil) and (CompilerFiles.Count>1) then begin
      Result:=MessageDlg(lisCCOAmbiguousCompiler,
        Format(lisCCOSeveralCompilers,[#13#13,CompilerFiles.Text,#13]),
        mtWarning,[mbAbort,mbIgnore],0);
      if Result<>mrIgnore then exit;
    end;
  finally
    CompilerFiles.Free;
  end;
  
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckCompileBogusFile(
  const CompilerFilename: string): TModalResult;
var
  TestDir: String;
  BogusFilename: String;
  CmdLineParams: String;
  CompileTool: TExternalToolOptions;
begin
  // compile bogus file
  FTest:=cotCompileBogusFiles;
  TestGroupbox.Caption:=dlgCCOTestCompilingEmptyFile;
  
  // get Test directory
  TestDir:=AppendPathDelim(EnvironmentOptions.TestBuildDirectory);
  if not DirPathExists(TestDir) then begin
    MessageDlg(lisCCOInvalidTestDir,
      Format(lisCCOCheckTestDir,[#13]),
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  // create bogus file
  BogusFilename:=CreateNonExistingFilename(TestDir+'testcompileroptions.pas');
  if not CreateEmptyFile(BogusFilename) then begin
    MessageDlg(lisCCOUnableToCreateTestFile,
      Format(lisCCOUnableToCreateTestPascalFile,[BogusFilename]),
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  try
    // create compiler command line options
    CmdLineParams:=Options.MakeOptionsString(BogusFilename,
              [ccloAddVerboseAll,ccloDoNotAppendOutFileOption,ccloAbsolutePaths])
              +' '+BogusFilename;

    CompileTool:=TExternalToolOptions.Create;
    CompileTool.Title:=dlgCCOTestToolCompilingEmptyFile;
    CompileTool.ScanOutputForFPCMessages:=true;
    CompileTool.ScanOutputForMakeMessages:=true;
    CompileTool.WorkingDirectory:=TestDir;
    CompileTool.Filename:=CompilerFilename;
    CompileTool.CmdLineParams:=CmdLineParams;

    Result:=RunTool(CompileTool,false);
    FreeThenNil(CompileTool);
  finally
    DeleteFileUTF8(BogusFilename);
  end;
  
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckPackagePathsIntersections(
  CurOptions: TCompilerOptions): TModalResult;
// check if the search paths contains source directories of used packages
// instead of only the output directories
var
  CurProject: TProject;
  CurPkg: TLazPackage;
  FirstDependency: TPkgDependency;
  PkgList: TFPList;
  i: Integer;
  UsedPkg: TLazPackage;
  UnitPath: String;
  OtherOutputDir: String;
  OtherSrcPath: String;
  p: Integer;
  SrcDir: String;
begin
  if CurOptions.BaseDirectory='' then exit(mrOk);

  // get dependencies
  CurProject:=nil;
  CurPkg:=nil;
  if CurOptions.Owner is TProject then begin
    CurProject:=TProject(CurOptions.Owner);
    FirstDependency:=CurProject.FirstRequiredDependency;
  end;
  if CurOptions.Owner is TLazPackage then begin
    CurPkg:=TLazPackage(CurOptions.Owner);
    FirstDependency:=CurPkg.FirstRequiredDependency;
  end;
  if FirstDependency=nil then exit(mrOK);
  try
    // get used packages
    PackageGraph.GetAllRequiredPackages(FirstDependency,PkgList);
    if PkgList=nil then exit(mrOk);

    // get search path
    UnitPath:=CurOptions.GetParsedPath(pcosUnitPath,icoNone,false,true);
    // check each used package
    for i:=0 to PkgList.Count-1 do begin
      UsedPkg:=TLazPackage(PkgList[i]);
      if UsedPkg.CompilerOptions.BaseDirectory='' then exit;
      // get source directories of used package (excluding the output directory)
      OtherSrcPath:=UsedPkg.CompilerOptions.GetParsedPath(pcosUnitPath,icoNone,false,true);
      OtherOutputDir:=UsedPkg.CompilerOptions.GetUnitOutPath(false);
      OtherSrcPath:=RemoveSearchPaths(OtherSrcPath,OtherOutputDir);
      // find intersections
      p:=1;
      repeat
        SrcDir:=GetNextDirectoryInSearchPath(UnitPath,p);
        if SearchDirectoryInSearchPath(OtherSrcPath,SrcDir)>0 then begin
          AddWarning(Format(
            lisTheUnitSearchPathOfContainsTheSourceDirectoryOfPac, ['"',
            CurOptions.GetOwnerName, '"', '"', SrcDir, '"', UsedPkg.Name]));
        end;
      until p>length(UnitPath);
    end;
  finally
    PkgList.Free;
  end;
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckCompilerConfig(
  CfgCache: TFPCTargetConfigCache): TModalResult;
var
  i: Integer;
  CfgFile: TFPCConfigFileState;
  CfgCount: Integer;
begin
  FTest:=cotCheckCompilerConfig;
  TestGroupbox.Caption:=dlgCCOTestCheckingCompilerConfig;

  CfgCount:=0;
  for i:=0 to CfgCache.ConfigFiles.Count-1 do begin
    CfgFile:=CfgCache.ConfigFiles[i];
    if CfgFile.FileExists then inc(CfgCount);
  end;
  if CfgCount<0 then begin
    // missing config file => warning
    AddWarning(lisCCONoCfgFound);
  end else if CfgCount=1 then begin
    // exactly one config, sounds good, but might still the be wrong one
    // => hint
    for i:=0 to CfgCache.ConfigFiles.Count-1 do begin
      CfgFile:=CfgCache.ConfigFiles[i];
      if CfgFile.FileExists then begin
        AddHint('using config file '+CfgFile.Filename);
        break;
      end;
    end;
  end else if CfgCount>1 then begin
    // multiple config files => warning
    for i:=0 to CfgCache.ConfigFiles.Count-1 do begin
      CfgFile:=CfgCache.ConfigFiles[i];
      if CfgFile.FileExists then
        AddWarning(lisCCOMultipleCfgFound+CfgFile.Filename);
    end;
  end;

  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.FindAllPPUFiles(const AnUnitPath: string
  ): TStrings;
var
  Directory: String;
  p: Integer;
  FileInfo: TSearchRec;
begin
  Result:=TStringList.Create;

  p:=1;
  while p<=length(AnUnitPath) do begin
    Directory:=TrimAndExpandDirectory(GetNextDirectoryInSearchPath(AnUnitPath,p));
    if Directory<>'' then begin
      if FindFirstUTF8(Directory+GetAllFilesMask,faAnyFile,FileInfo)=0
      then begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
            continue;
          // check extension
          if CompareFileExt(FileInfo.Name,'.ppu',
            {$IFDEF MSWINDOWS}false{$ELSE}true{$ENDIF})=0 then
            Result.Add(Directory+FileInfo.Name);
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
    end;
  end;
end;

function TCheckCompilerOptsDlg.CheckMissingFPCPPUs(
  CfgCache: TFPCTargetConfigCache): TModalResult;
  
  function Check(const TheUnitname: string; Severity: TCompilerCheckMsgLvl
    ): Boolean;
  begin
    if (CfgCache.Units<>nil)
    and (CfgCache.Units.Contains(TheUnitname)) then exit(true);
    AddMsg(Severity,Format(lisCCOMsgPPUNotFound,[TheUnitname]));
    Result:=ord(Severity)>=ord(ccmlError);
    if not Result then begin
      if MessageDlg(lisCCOMissingUnit,
        Format(lisCCOPPUNotFoundDetailed,[TheUnitname, #13]),
        mtError,[mbIgnore,mbAbort],0)=mrIgnore then
          Result:=true;
    end;
  end;
  
begin
  FTest:=cotCheckMissingFPCPPUs;
  TestGroupbox.Caption:=dlgCCOTestMissingPPU;

  Result:=mrCancel;
  // rtl
  if not Check('system',ccmlError) then exit;
  if not Check('sysutils',ccmlError) then exit;
  if not Check('classes',ccmlError) then exit;
  // fcl
  if not Check('avl_tree',ccmlError) then exit;
  if not Check('zstream',ccmlError) then exit;

  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckCompilerDate(CfgCache: TFPCTargetConfigCache
  ): TModalResult;
var
  MinPPUDate: LongInt;
  MaxPPUDate: LongInt;
  CompilerDate: LongInt;
  MinPPU: String;
  MaxPPU: String;
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  
  procedure CheckFileAge(const aFilename: string);
  var
    CurDate: LongInt;
  begin
    CurDate:=FileAgeCached(aFilename);
    //DebugLn(['CheckFileAge ',aFilename,' ',CurDate]);
    if (CurDate=-1) then exit;
    if (MinPPUDate=-1) or (MinPPUDate>CurDate) then begin
      MinPPUDate:=CurDate;
      MinPPU:=aFilename;
    end;
    if (MaxPPUDate=-1) or (MaxPPUDate<CurDate) then begin
      MaxPPUDate:=CurDate;
      MaxPPU:=aFilename;
    end;
  end;
  
  procedure CheckFileAgeOfUnit(const aUnitName: string);
  var
    Filename: string;
  begin
    Filename:=CfgCache.Units[aUnitName];
    if Filename='' then exit;
    CheckFileAge(Filename);
  end;
  
begin
  if CfgCache.Units=nil then exit(mrOK);

  FTest:=cotCheckCompilerDate;
  TestGroupbox.Caption:=dlgCCOTestCompilerDate;

  Result:=mrCancel;
  
  CompilerDate:=CfgCache.CompilerDate;

  // first check some rtl and fcl units
  // They are normally installed in one step, so the dates should be nearly
  // the same. If not, then probably two different installations are mixed up.
  MinPPUDate:=-1;
  MinPPU:='';
  MaxPPUDate:=-1;
  MaxPPU:='';
  CheckFileAgeOfUnit('system');
  CheckFileAgeOfUnit('sysutils');
  CheckFileAgeOfUnit('classes');
  CheckFileAgeOfUnit('base64');
  CheckFileAgeOfUnit('avl_tree');
  CheckFileAgeOfUnit('fpimage');
  
  //DebugLn(['TCheckCompilerOptsDlg.CheckCompilerDate MinPPUDate=',MinPPUDate,' MaxPPUDate=',MaxPPUDate,' compdate=',CompilerDate]);

  if MinPPU<>'' then begin
    if MaxPPUDate-MinPPUDate>3600 then begin
      // the FPC .ppu files dates differ more than one hour
      Result:=MessageDlg(lisCCOWarningCaption,
        Format(lisCCODatesDiffer,[#13,#13,MinPPU,#13,MaxPPU]),
        mtError,[mbIgnore,mbAbort],0);
      if Result<>mrIgnore then
        exit;
    end;
  end;

  // check file dates of all .ppu
  // if a .ppu is much older than the compiler itself, then the ppu is probably
  // a) a leftover from a installation
  // b) not updated
  Node:=CfgCache.Units.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    if (Item^.Value<>'') and (CompareFileExt(Item^.Value,'.ppu',false)=0) then
      CheckFileAge(Item^.Value);
    Node:=CfgCache.Units.Tree.FindSuccessor(Node);
  end;

  if MinPPU<>'' then begin
    if CompilerDate-MinPPUDate>300 then begin
      // the compiler is more than 5 minutes newer than one of the ppu files
      Result:=MessageDlg(lisCCOWarningCaption,Format(lisCCOPPUOlderThanCompiler,[#13,MinPPU]),
        mtError,[mbIgnore,mbAbort],0);
      if Result<>mrIgnore then
        exit;
    end;
  end;

  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckForAmbiguousPPUs(SearchForPPUs: TStrings;
  SearchInPPUs: TStrings): TModalResult;
var
  i: Integer;
  j: Integer;
  CurUnitName: String;
  AnotherUnitName: String;
begin
  if SearchInPPUs=nil then
    SearchInPPUs:=SearchForPPUs;

  // resolve links and remove doubles
  ResolveLinksInFileList(SearchForPPUs,true);
  RemoveDoubles(SearchForPPUs);
  if SearchForPPUs<>SearchInPPUs then begin
    ResolveLinksInFileList(SearchInPPUs,true);
    RemoveDoubles(SearchInPPUs);
  end;

  for i:=1 to SearchForPPUs.Count-1 do begin
    CurUnitName:=ExtractFileNameOnly(SearchForPPUs[i]);
    if SearchForPPUs=SearchInPPUs then
      j:=i-1
    else
      j:=SearchInPPUs.Count-1;
    while j>=0 do begin
      AnotherUnitName:=ExtractFileNameOnly(SearchInPPUs[j]);
      if CompareText(AnotherUnitName,CurUnitName)=0 then begin
        // unit exists twice
        AddWarning(Format(lisCCOPPUExistsTwice,[SearchForPPUs[i],SearchInPPUs[j]]));
        break;
      end;
      dec(j);
    end;
  end;
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckFPCUnitPathsContainSources(
  const FPCCfgUnitPath: string): TModalResult;
// The FPC standard unit path does not include source directories.
// If it contain source directories the user added these unit paths himself.
// This is probably a hack and has two disadvantages:
// 1. The IDE ignores these paths
// 2. The user risks to create various .ppu for these sources which leads to
//    strange further compilation errors.
var
  p: Integer;
  Directory: String;
  FileInfo: TSearchRec;
  WarnedDirectories: TStringList;
begin
  FTest:=cotCheckFPCUnitPathsContainSources;
  TestGroupbox.Caption:=dlgCCOTestSrcInPPUPaths;

  Result:=mrCancel;
  WarnedDirectories:=TStringList.Create;
  p:=1;
  while p<=length(FPCCfgUnitPath) do begin
    Directory:=TrimFilename(GetNextDirectoryInSearchPath(FPCCfgUnitPath,p));
    if (Directory<>'') then begin
      Directory:=TrimAndExpandDirectory(GetNextDirectoryInSearchPath(FPCCfgUnitPath,p));
      if (Directory<>'') and (FilenameIsAbsolute(Directory))
      and (WarnedDirectories.IndexOf(Directory)<0) then begin
        //DebugLn(['TCheckCompilerOptsDlg.CheckFPCUnitPathsContainSources Directory="',Directory,'"']);
        if FindFirstUTF8(Directory+GetAllFilesMask,faAnyFile,FileInfo)=0
        then begin
          repeat
            // check if special file
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
              continue;
            // check extension
            if FilenameIsPascalUnit(FileInfo.Name) then begin
              AddWarning(lisCCOFPCUnitPathHasSource+Directory+FileInfo.Name);
              WarnedDirectories.Add(Directory);
              break;
            end;
          until FindNextUTF8(FileInfo)<>0;
        end;
        FindCloseUTF8(FileInfo);
      end;
    end;
  end;
  WarnedDirectories.Free;
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckOutputPathInSourcePaths(
  CurOptions: TCompilerOptions): TModalResult;
var
  OutputDir: String;
  SrcPath: String;
begin
  OutputDir:=CurOptions.GetUnitOutPath(false);
  if OutputDir='' then begin
    if CurOptions.Owner is TLazPackage then
      AddWarning(CurOptions.GetOwnerName+' has no output directory set');
    exit(mrOk);
  end;
  // check unit search path
  SrcPath:=CurOptions.GetParsedPath(pcosUnitPath,icoNone,false);
  if SearchDirectoryInSearchPath(SrcPath,OutputDir)>0 then begin
    AddWarning(Format(lisTheOutputDirectoryOfIsListedInTheUnitSearchPathOf, [
      CurOptions.GetOwnerName, CurOptions.GetOwnerName])
      +lisTheOutputDirectoryShouldBeASeparateDirectoryAndNot);
  end;
  // check include search path
  SrcPath:=CurOptions.GetParsedPath(pcosIncludePath,icoNone,false);
  if SearchDirectoryInSearchPath(SrcPath,OutputDir)>0 then begin
    AddWarning(Format(lisTheOutputDirectoryOfIsListedInTheIncludeSearchPath, [
      CurOptions.GetOwnerName, CurOptions.GetOwnerName])
      +lisTheOutputDirectoryShouldBeASeparateDirectoryAndNot);
  end;
  // check inherited unit search path
  SrcPath:=CurOptions.GetParsedPath(pcosNone,icoUnitPath,false);
  if SearchDirectoryInSearchPath(SrcPath,OutputDir)>0 then begin
    AddWarning(Format(lisTheOutputDirectoryOfIsListedInTheInheritedUnitSear, [
      CurOptions.GetOwnerName, CurOptions.GetOwnerName])
      +lisTheOutputDirectoryShouldBeASeparateDirectoryAndNot);
  end;
  // check inherited include search path
  SrcPath:=CurOptions.GetParsedPath(pcosNone,icoIncludePath,false);
  if SearchDirectoryInSearchPath(SrcPath,OutputDir)>0 then begin
    AddWarning(Format(lisTheOutputDirectoryOfIsListedInTheInheritedIncludeS, [
      CurOptions.GetOwnerName, CurOptions.GetOwnerName])
      +lisTheOutputDirectoryShouldBeASeparateDirectoryAndNot);
  end;
  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckOrphanedPPUs(CurOptions: TCompilerOptions
  ): TModalResult;
// check for ppu and .o files that were not created from known .pas/.pp/.p files
var
  FileInfo: TSearchRec;
  PPUFiles: TStringList;
  i: Integer;
  OutputDir: String;
  PPUFilename: string;
  AUnitName: String;
  SrcPath: String;
  Directory: String;
  CurProject: TLazProject;
  ProjFile: TLazProjectFile;
begin
  OutputDir:=CurOptions.GetUnitOutPath(false);
  if OutputDir='' then exit(mrOk);

  PPUFiles:=TStringList.Create;
  try
    // search .ppu and .o files in output directory
    Directory:=AppendPathDelim(OutputDir);
    if FindFirstUTF8(Directory+GetAllFilesMask,faAnyFile,FileInfo)=0 then
    begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        // check extension
        if (CompareFileExt(FileInfo.Name,'.ppu',
          {$IFDEF MSWINDOWS}false{$ELSE}true{$ENDIF})<>0)
        and (CompareFileExt(FileInfo.Name,'.o',
          {$IFDEF MSWINDOWS}false{$ELSE}true{$ENDIF})<>0)
        then
          continue;
        PPUFiles.Add(Directory+FileInfo.Name);
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);

    // remove all .ppu/.o files with a unit source
    SrcPath:=Options.GetParsedPath(pcosUnitPath,icoNone,false,true);
    //DebugLn(['TCheckCompilerOptsDlg.CheckOrphanedPPUs SrcPath="',SrcPath,'" OutDir="',OutputDir,'"']);
    for i:=PPUFiles.Count-1 downto 0 do begin
      PPUFilename:=PPUFiles[i];
      AUnitName:=ExtractFileNameOnly(PPUFilename);
      // search .pas/.pp/.p file
      if SearchPascalUnitInPath(AUnitName,'',SrcPath,';',ctsfcAllCase)<>'' then
        PPUFiles.Delete(i)
      // check for main source
      else if (Options.Owner is TLazProject) then begin
        CurProject:=TLazProject(Options.Owner);
        if (CurProject.MainFileID>=0) then begin
          ProjFile:=CurProject.MainFile;
          if (SysUtils.CompareText(ExtractFileNameOnly(ProjFile.Filename),AUnitName)=0)
          then
            PPUFiles.Delete(i);
        end;
      end;
    end;

    // PPUFiles now contains all orphaned ppu/o files
    PPUFiles.Sort;
    for i:=0 to PPUFiles.Count-1 do
      AddWarning('orphaned file found: '+PPUFiles[i]);
  finally
    PPUFiles.Free;
  end;

  Result:=mrOk;
end;

procedure TCheckCompilerOptsDlg.SetMacroList(const AValue: TTransferMacroList);
begin
  if FMacroList=AValue then exit;
  FMacroList:=AValue;
end;

procedure TCheckCompilerOptsDlg.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@ApplicationOnIdle)
  else
    Application.RemoveOnIdleHandler(@ApplicationOnIdle);
end;

function TCheckCompilerOptsDlg.DoTestAll: TModalResult;
var
  CompilerFilename: String;
  CompileTool: TExternalToolOptions;
  CompilerFiles: TStrings;
  FPCCfgUnitPath: string;
  TargetUnitPath: String;
  Target_PPUs: TStrings;
  cp: TParsedCompilerOptString;
  TargetCPU: String;
  TargetOS: String;
  CfgCache: TFPCTargetConfigCache;
  FPC_PPUs: TStrings;
begin
  Result:=mrCancel;
  if Test<>cotNone then exit;
  CompileTool:=nil;
  TestMemo.Lines.Clear;
  CompilerFiles:=nil;
  Target_PPUs:=nil;
  FPC_PPUs:=nil;
  try
    // do not confuse the user with cached data
    InvalidateFileStateCache();

    // check for special characters in search paths
    for cp:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    begin
      if cp in ParsedCompilerSearchPaths then begin
        Result:=CheckSpecialCharsInPath(copy(EnumToStr(cp),5,100),
                                        Options.ParsedOpts.GetParsedValue(cp));
        if not (Result in [mrOk,mrIgnore]) then exit;
      end;
    end;
    
    // check for non existing paths
    CheckNonExistingSearchPaths('include search path',
                                Options.GetIncludePath(false));
    CheckNonExistingSearchPaths('library search path',
                                Options.GetLibraryPath(false));
    CheckNonExistingSearchPaths('unit search path',
                                Options.GetUnitPath(false));
    CheckNonExistingSearchPaths('source search path',
                                Options.GetSrcPath(false));

    // fetch compiler filename
    CompilerFilename:=Options.ParsedOpts.GetParsedValue(pcosCompilerPath);

    // check compiler filename
    Result:=CheckCompilerExecutable(CompilerFilename);
    if not (Result in [mrOk,mrIgnore]) then exit;

    TargetOS:=Options.TargetOS;
    TargetCPU:=Options.TargetCPU;
    CfgCache:=CodeToolBoss.FPCDefinesCache.ConfigCaches.Find(CompilerFilename,
                                                    '',TargetOS,TargetCPU,true);

    // check compiler config
    Result:=CheckCompilerConfig(CfgCache);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if compiler paths include base units
    Result:=CheckMissingFPCPPUs(CfgCache);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if compiler is older than fpc ppu
    Result:=CheckCompilerDate(CfgCache);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if there are ambiguous fpc ppu
    FPCCfgUnitPath:=CfgCache.GetUnitPaths;
    FPC_PPUs:=FindAllPPUFiles(FPCCfgUnitPath);
    Result:=CheckForAmbiguousPPUs(FPC_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if FPC unit paths contain sources
    Result:=CheckFPCUnitPathsContainSources(FPCCfgUnitPath);
    if not (Result in [mrOk,mrIgnore]) then exit;

    if Options is TPkgCompilerOptions then begin
      // check if package has no separate output directory
      Result:=CheckOutputPathInSourcePaths(Options);
      if not (Result in [mrOk,mrIgnore]) then exit;
    end;

    // gather PPUs in project/package unit search paths
    TargetUnitPath:=Options.GetUnitPath(false);
    Target_PPUs:=FindAllPPUFiles(TargetUnitPath);

    // check if there are ambiguous ppu in project/package unit path
    Result:=CheckForAmbiguousPPUs(Target_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if there are ambiguous ppu in fpc and project/package unit path
    Result:=CheckForAmbiguousPPUs(FPC_PPUs,Target_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check that all ppu in the output directory have sources in project/package
    Result:=CheckOrphanedPPUs(Options);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // compile bogus file
    Result:=CheckCompileBogusFile(CompilerFilename);
    if not (Result in [mrOk,mrIgnore]) then exit;
    
    // check if search paths of packages/projects intersects
    Result:=CheckPackagePathsIntersections(Options);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // ToDo: check ppu checksums and versions

    if OutputListbox.Items.Count=0 then
      AddMsg(lisCCOTestsSuccess,'',-1);

  finally
    CompilerFiles.Free;
    CompileTool.Free;
    FTest:=cotNone;
    TestGroupbox.Caption:=dlgCCOTest;
    FPC_PPUs.Free;
    Target_PPUs.Free;
  end;
  Result:=mrOk;
end;

constructor TCheckCompilerOptsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdleConnected:=true;
  Caption:=dlgCCOCaption;
  TestGroupbox.Caption:=dlgCCOTest;
  OutputGroupBox.Caption:=dlgCCOResults;
  CopyOutputMenuItem.Caption:=lisCCOCopyOutputToCliboard;
end;

destructor TCheckCompilerOptsDlg.Destroy;
begin
  IdleConnected:=false;;
  FDirectories.Free;
  inherited Destroy;
end;

function TCheckCompilerOptsDlg.RunTool(ExtTool: TExternalToolOptions;
  ShowAbort: Boolean): TModalResult;
begin
  TestMemo.Lines.Text:=ExtTool.Filename+' '+ExtTool.CmdLineParams;
  Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,MacroList,ShowAbort);
end;

procedure TCheckCompilerOptsDlg.Add(const Msg, CurDir: String;
  ProgressLine: boolean; OriginalIndex: integer);
var
  i: Integer;
begin
  if FLastLineIsProgress then begin
    OutputListbox.Items[OutputListbox.Items.Count-1]:=Msg;
  end else begin
    OutputListbox.Items.Add(Msg);
  end;
  FLastLineIsProgress:=ProgressLine;
  i:=OutputListbox.Items.Count-1;
  SetMsgDirectory(i,CurDir);
  OutputListbox.TopIndex:=OutputListbox.Items.Count-1;
  if OriginalIndex=0 then ;
end;

procedure TCheckCompilerOptsDlg.AddMsg(const Msg, CurDir: String;
  OriginalIndex: integer);
begin
  Add(Msg,CurDir,false,OriginalIndex);
end;

procedure TCheckCompilerOptsDlg.AddProgress(Line: TIDEScanMessageLine);
begin
  Add(Line.Line,Line.WorkingDirectory,false,Line.LineNumber);
end;

procedure TCheckCompilerOptsDlg.AddHint(const Msg: string);
begin
  AddMsg(ccmlHint,Msg);
end;

procedure TCheckCompilerOptsDlg.AddWarning(const Msg: string);
begin
  AddMsg(ccmlWarning,Msg);
end;

procedure TCheckCompilerOptsDlg.AddMsg(const Level: TCompilerCheckMsgLvl;
  const Msg: string);
begin
  case Level of
  ccmlWarning: Add(lisCCOWarningMsg+Msg,'',false,-1);
  ccmlHint:    Add(lisCCOHintMsg+Msg,'',false,-1);
  else         Add(lisCCOErrorMsg+Msg,'',false,-1);
  end;
end;

end.

