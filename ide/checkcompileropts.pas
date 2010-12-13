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
}
unit CheckCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  FileUtil, Clipbrd, StdCtrls, Buttons, Process, AsyncProcess, Menus, ExtCtrls,
  UTF8Process, ButtonPanel,
  // codetools
  KeywordFuncLists, CodeToolManager, FileProcs,
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
    FMacroList: TTransferMacroList;
    FOptions: TCompilerOptions;
    FTest: TCompilerOptionsTest;
    FLastLineIsProgress: boolean;
    FDirectories: TStringList;
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure SetOptions(const AValue: TCompilerOptions);
    procedure SetMsgDirectory(Index: integer; const CurDir: string);
    function CheckSpecialCharsInPath(const Title, ExpandedPath: string): TModalResult;
    function CheckNonExistsingSearchPaths(const Title, ExpandedPath: string): TModalResult;
    function CheckCompilerExecutable(const CompilerFilename: string): TModalResult;
    function CheckAmbiguousFPCCfg(const CompilerFilename: string): TModalResult;
    function CheckCompilerConfig(const CompilerFilename: string;
                                 out FPCCfgUnitPath: string): TModalResult;
    function FindAllPPUFiles(const FPCCfgUnitPath: string): TStrings;
    function CheckMissingFPCPPUs(PPUs: TStrings): TModalResult;
    function CheckCompilerDate(const CompilerFilename: string;
                               PPUs: TStrings): TModalResult;
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
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
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
  
  procedure AddStr(var s: string; const Addition: string);
  begin
    if s='' then
      s:=lisCCOContains
    else
      s:=s+', ';
    s:=s+Addition;
  end;
  
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

function TCheckCompilerOptsDlg.CheckNonExistsingSearchPaths(const Title,
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

function TCheckCompilerOptsDlg.CheckAmbiguousFPCCfg(
  const CompilerFilename: string): TModalResult;
var
  CfgFiles: TStringList;
  Dir: String;
  Filename: String;
  i: Integer;
  
  procedure AddFile(const aFilename: string);
  begin
    if (CfgFiles.IndexOf(aFilename)<0) and FileExistsUTF8(aFilename) then
      CfgFiles.Add(aFilename);
  end;
  
begin
  FTest:=cotCheckAmbiguousFPCCfg;
  TestGroupbox.Caption:=dlgCCOTestCheckingFPCConfigs;

  CfgFiles:=TStringList.Create;
  
  // check $HOME/.fpc.cfg
  Dir:=GetEnvironmentVariableUTF8('HOME');
  if Dir<>'' then begin
    Filename:=CleanAndExpandDirectory(Dir)+'.fpc.cfg';
    AddFile(Filename);
  end;

  // check compiler path + fpc.cfg
  Dir:=ExtractFilePath(CompilerFilename);
  Dir:=ReadAllLinks(Dir,false);
  if Dir<>'' then begin
    Filename:=CleanAndExpandDirectory(Dir)+'fpc.cfg';
    AddFile(Filename);
  end;

  // check working directory + fpc.cfg
  Dir:=ExtractFilePath(Options.BaseDirectory);
  Dir:=ReadAllLinks(Dir,false);
  if Dir<>'' then begin
    Filename:=CleanAndExpandDirectory(Dir)+'fpc.cfg';
    AddFile(Filename);
  end;

  // check /etc/fpc.cfg
  {$IFDEF Unix}
    Dir:=ExtractFilePath(CompilerFilename);
    Dir:=GetEnvironmentVariableUTF8('HOME');
    if Dir<>'' then begin
      Filename:='/etc/fpc.cfg';
      AddFile(Filename);
    end;
  {$ENDIF}

  // warn about missing or too many fpc.cfg
  if CfgFiles.Count<1 then begin
    AddWarning(lisCCONoCfgFound);
  end else if CfgFiles.Count>1 then begin
    for i:=0 to CfgFiles.Count-1 do
      AddWarning(lisCCOMultipleCfgFound+CfgFiles[i]);
  end;

  CfgFiles.Free;
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
    CmdLineParams:=Options.MakeOptionsString(BogusFilename,nil,
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
  const CompilerFilename: string; out FPCCfgUnitPath: string): TModalResult;
var
  TestDir: String;

  procedure ProcessOutputLine(const Line: string);
  const
    USING_UNIT_PATH = 'USING UNIT PATH: ';
    READING_OPTIONS_FROM_FILE = 'READING OPTIONS FROM FILE ';
    HANDLING_OPTION = 'HANDLING OPTION ';
  var
    len, curpos: integer;
    NewPath: String;
    UpLine: String;
  begin
    len := length(Line);
    if len <= 6 then Exit; // shortest match

    CurPos := 1;
    // strip timestamp e.g. [0.306]
    if Line[CurPos] = '[' then begin
      repeat
        inc(CurPos);
        if CurPos > len then Exit;
      until line[CurPos] = ']';
      Inc(CurPos, 2); //skip space also
      if len - CurPos < 6 then Exit; // shortest match
    end;

    UpLine:=UpperCaseStr(Line);

    case UpLine[CurPos] of
    'C':
      if (StrLComp(@UpLine[CurPos], READING_OPTIONS_FROM_FILE,
          length(READING_OPTIONS_FROM_FILE)) = 0) then
      begin
        // show a hint what cfg file is read by FPC
        AddHint(Line);
      end;
    'U':
      if (StrLComp(@UpLine[CurPos], USING_UNIT_PATH, length(USING_UNIT_PATH)) = 0)
      then begin
        Inc(CurPos, length(USING_UNIT_PATH));
        NewPath:=copy(Line,CurPos,len);
        if NewPath<>'' then begin
          if not FilenameIsAbsolute(NewPath) then begin
            AddWarning(Format(lisCCORelUnitPathFoundInCfg,[NewPath]));
            NewPath:=ExpandFileNameUTF8(NewPath);
          end;
          NewPath:=AppendPathDelim(TrimFilename(NewPath));
          if (CompareFilenames(NewPath,Options.BaseDirectory)<>0)
          and (CompareFilenames(NewPath,TestDir)<>0)
          then begin
            //DebugLn(['TCheckCompilerOptsDlg.CheckCompilerConfig: Using unit path: "',NewPath,'"']);
            FPCCfgUnitPath:=FPCCfgUnitPath+NewPath+';';
          end;
        end;
      end;
    end;
  end;
  
var
  ATestPascalFile: String;
  CurCompilerOptions: String;
  TargetOS: String;
  TargetCPU: String;
  OutputLine: String;
  TheProcess: TProcessUTF8;
  OutLen: Integer;
  LineStart: integer;
  i: Integer;
  CmdLine: string;
  Buf: string;
begin
  FPCCfgUnitPath:='';

  FTest:=cotCheckCompilerConfig;
  TestGroupbox.Caption:=dlgCCOTestCheckingCompilerConfig;

  Result:=CheckAmbiguousFPCCfg(CompilerFilename);
  if not (Result in [mrOk,mrIgnore]) then exit;

  TestDir:=AppendPathDelim(EnvironmentOptions.TestBuildDirectory);
  ATestPascalFile:=CreateNonExistingFilename(TestDir+'testcompileroptions.pas');
  
  CurCompilerOptions:='';
  TargetOS:=Options.TargetOS;
  if TargetOS<>'' then
    CurCompilerOptions:=AddCmdLineParameter(CurCompilerOptions,'-T'+TargetOS);
  TargetCPU:=Options.TargetCPU;
  if TargetCPU<>'' then
    CurCompilerOptions:=AddCmdLineParameter(CurCompilerOptions,'-P'+TargetCPU);

  CmdLine:=CompilerFilename+' -va ';
  
  // set english message file to be able to parse the fpc output
  if FileExistsCached(CodeToolBoss.DefinePool.EnglishErrorMsgFilename) then
    CmdLine:=CmdLine+'-Fr'+CodeToolBoss.DefinePool.EnglishErrorMsgFilename+' '
  else
    AddWarning(lisCCOEnglishMessageFileMissing);

  if CurCompilerOptions<>'' then
    CmdLine:=CmdLine+CurCompilerOptions+' ';
  CmdLine:=CmdLine+ATestPascalFile;

  TheProcess := TProcessUTF8.Create(nil);
  TheProcess.CommandLine := CmdLine;
  TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
  TheProcess.ShowWindow := swoHide;
  TheProcess.CurrentDirectory:=Options.BaseDirectory;
  //DebugLn(['TCheckCompilerOptsDlg.CheckCompilerConfig Options.BaseDirectory=',Options.BaseDirectory]);
  try
    TheProcess.Execute;
    OutputLine:='';
    SetLength(Buf,1024);
    repeat
      if (TheProcess.Output<>nil) then begin
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
      end else
        OutLen:=0;
      LineStart:=1;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
          ProcessOutputLine(OutputLine);
          OutputLine:='';
          if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
          then
            inc(i);
          LineStart:=i+1;
        end;
        inc(i);
      end;
      OutputLine:=copy(Buf,LineStart,OutLen-LineStart+1);
    until OutLen=0;
    TheProcess.WaitOnExit;
  finally
    TheProcess.Free;
  end;
  FPCCfgUnitPath:=TrimSearchPath(FPCCfgUnitPath,'');

  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.FindAllPPUFiles(const FPCCfgUnitPath: string
  ): TStrings;
var
  Directory: String;
  p: Integer;
  FileInfo: TSearchRec;
begin
  Result:=TStringList.Create;

  p:=1;
  while p<=length(FPCCfgUnitPath) do begin
    Directory:=CleanAndExpandDirectory(GetNextDirectoryInSearchPath(FPCCfgUnitPath,p));
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

function TCheckCompilerOptsDlg.CheckMissingFPCPPUs(PPUs: TStrings
  ): TModalResult;
  
  function Check(const TheUnitname: string; Severity: TCompilerCheckMsgLvl
    ): Boolean;
  var
    i: Integer;
  begin
    for i:=0 to PPUs.Count-1 do begin
      if ExtractFileNameOnly(PPUs[i])=TheUnitname then exit(true);
    end;
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

function TCheckCompilerOptsDlg.CheckCompilerDate(
  const CompilerFilename: string; PPUs: TStrings): TModalResult;
var
  MinPPUDate: LongInt;
  MaxPPUDate: LongInt;
  CompilerDate: LongInt;
  MinPPU: String;
  MaxPPU: String;
  i: Integer;
  
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
    i: Integer;
  begin
    for i:=0 to PPUs.Count-1 do
      if ExtractFileNameOnly(PPUs[i])=aUnitName then begin
        CheckFileAge(PPUs[i]);
        exit;
      end;
    //DebugLn(['CheckFileAgeOfUnit Unit not found: ',aUnitName]);
  end;
  
begin
  FTest:=cotCheckCompilerDate;
  TestGroupbox.Caption:=dlgCCOTestCompilerDate;

  Result:=mrCancel;
  
  CompilerDate:=FileAgeCached(CompilerFilename);
  if CompilerDate=-1 then begin
    Result:=MessageDlg(lisCCOErrorCaption,Format(lisCCOUnableToGetFileDate,[CompilerFilename]),
      mtError,[mbIgnore,mbAbort],0);
    exit;
  end;

  // first check some rtl and fcl units
  // They are normally installed in one step, so there dates should be nearly
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
  for i:=0 to PPUs.Count-1 do
    CheckFileAge(PPUs[i]);

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
      Directory:=CleanAndExpandDirectory(GetNextDirectoryInSearchPath(FPCCfgUnitPath,p));
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

function TCheckCompilerOptsDlg.DoTestAll: TModalResult;
var
  CompilerFilename: String;
  CompileTool: TExternalToolOptions;
  CompilerFiles: TStrings;
  FPCCfgUnitPath: string;
  FPC_PPUs: TStrings;
  TargetUnitPath: String;
  Target_PPUs: TStrings;
  cp: TParsedCompilerOptString;
begin
  Result:=mrCancel;
  if Test<>cotNone then exit;
  CompileTool:=nil;
  TestMemo.Lines.Clear;
  CompilerFiles:=nil;
  FPC_PPUs:=nil;
  Target_PPUs:=nil;
  try
    // do not confuse the user with cached data
    InvalidateFileStateCache();

    // check for special characters in search paths
    for cp:=Low(TParsedCompilerOptString) to High(TParsedCompilerOptString) do
    begin
      if cp in ParsedCompilerSearchPaths then begin
        Result:=CheckSpecialCharsInPath(
          copy(ParsedCompilerOptStringNames[cp],5,100),
          Options.ParsedOpts.GetParsedValue(cp));
        if not (Result in [mrOk,mrIgnore]) then exit;
      end;
    end;
    
    // check for non existing paths
    CheckNonExistsingSearchPaths('include search path',
                                 Options.GetIncludePath(false));
    CheckNonExistsingSearchPaths('library search path',
                                 Options.GetLibraryPath(false));
    CheckNonExistsingSearchPaths('unit search path',
                                 Options.GetUnitPath(false));
    CheckNonExistsingSearchPaths('source search path',
                                 Options.GetSrcPath(false));

    // fetch compiler filename
    CompilerFilename:=Options.ParsedOpts.GetParsedValue(pcosCompilerPath);

    // check compiler filename
    Result:=CheckCompilerExecutable(CompilerFilename);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check compiler config
    Result:=CheckCompilerConfig(CompilerFilename,FPCCfgUnitPath);
    if not (Result in [mrOk,mrIgnore]) then exit;

    FPC_PPUs:=FindAllPPUFiles(FPCCfgUnitPath);

    // check if compiler paths include base units
    Result:=CheckMissingFPCPPUs(FPC_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if compiler is older than fpc ppu
    Result:=CheckCompilerDate(CompilerFilename,FPC_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // check if there are ambiguous fpc ppu
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
  Application.AddOnIdleHandler(@ApplicationOnIdle,true);
  Caption:=dlgCCOCaption;
  TestGroupbox.Caption:=dlgCCOTest;
  OutputGroupBox.Caption:=dlgCCOResults;
  CopyOutputMenuItem.Caption:=lisCCOCopyOutputToCliboard;
end;

destructor TCheckCompilerOptsDlg.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
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

