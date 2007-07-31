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
  Classes, SysUtils, LCLProc,  LResources, Forms, Controls, Graphics, Dialogs,
  Clipbrd, StdCtrls, Buttons, FileUtil, Process,
  KeywordFuncLists, CodeToolManager,
  IDEExternToolIntf,
  IDEProcs, EnvironmentOpts, LazarusIDEStrConsts,
  CompilerOptions, ExtToolEditDlg, TransferMacros, LazConf, Menus;

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
    CloseButton1: TBitBtn;
    CopyOutputMenuItem: TMenuItem;
    OutputPopupMenu: TPopupMenu;
    TestMemo: TMemo;
    TestGroupbox: TGroupBox;
    OutputListbox: TListbox;
    OutputGroupBox: TGroupBox;
    procedure ApplicationOnIdle(Sender: TObject; var Done: Boolean);
    procedure CloseButtonCLICK(Sender: TObject);
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
    function CheckCompilerExecutable(const CompilerFilename: string): TModalResult;
    function CheckAmbiguousFPCCfg(const CompilerFilename: string): TModalResult;
    function CheckCompilerConfig(const CompilerFilename: string;
                                 out FPCCfgUnitPath: string): TModalResult;
    function FindAllPPUFiles(const FPCCfgUnitPath: string): TStrings;
    function CheckMissingFPCPPUs(PPUs: TStrings): TModalResult;
    function CheckCompilerDate(const CompilerFilename: string;
                               PPUs: TStrings): TModalResult;
    function CheckForAmbiguousPPUs(PPUs: TStrings): TModalResult;
    function CheckFPCUnitPathsContainSources(const FPCCfgUnitPath: string
                                              ): TModalResult;
    function CheckCompileBogusFile(const CompilerFilename: string): TModalResult;
  public
    function DoTest: TModalResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function RunTool(ExtTool: TExternalToolOptions): TModalResult;
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

implementation

{ TCheckCompilerOptsDlg }

procedure TCheckCompilerOptsDlg.ApplicationOnIdle(Sender: TObject;
  var Done: Boolean);
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  DoTest;
end;

procedure TCheckCompilerOptsDlg.CloseButtonCLICK(Sender: TObject);
begin
  ModalResult:=mrOk;
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

function TCheckCompilerOptsDlg.CheckCompilerExecutable(
  const CompilerFilename: string): TModalResult;
var
  CompilerFiles: TStrings;
begin
  FTest:=cotCheckCompilerExe;
  TestGroupbox.Caption:='Test: Checking compiler ...';
  try
    CheckIfFileIsExecutable(CompilerFilename);
  except
    on e: Exception do begin
      Result:=QuestionDlg('Invalid compiler',
        'The compiler "'+CompilerFilename+'" is not an executable file.'#13
        +'Details: '+E.Message,
        mtError,[mrCancel,'Skip',mrAbort],0);
      exit;
    end;
  end;

  // check if there are several compilers in path
  CompilerFiles:=SearchAllFilesInPath(GetDefaultCompilerFilename,'',
       SysUtils.GetEnvironmentVariable('PATH'),':',[sffDontSearchInBasePath]);
  ResolveLinksInFileList(CompilerFiles,false);
  RemoveDoubles(CompilerFiles);
  if (CompilerFiles<>nil) and (CompilerFiles.Count>1) then begin
    Result:=MessageDlg('Ambiguous Compiler',
      'There are several FreePascal Compilers in your path.'#13#13
      +CompilerFiles.Text+#13
      +'Maybe you forgot to delete an old compiler?',
      mtWarning,[mbCancel,mbIgnore],0);
    if Result<>mrIgnore then exit;
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
    if (CfgFiles.IndexOf(aFilename)<0) and FileExists(aFilename) then
      CfgFiles.Add(aFilename);
  end;
  
begin
  FTest:=cotCheckAmbiguousFPCCfg;
  TestGroupbox.Caption:='Test: Checking fpc configs ...';

  CfgFiles:=TStringList.Create;
  
  // check $HOME/.fpc.cfg
  Dir:=SysUtils.GetEnvironmentVariable('HOME');
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
    Dir:=SysUtils.GetEnvironmentVariable('HOME');
    if Dir<>'' then begin
      Filename:='/etc/fpc.cfg';
      AddFile(Filename);
    end;
  {$ENDIF}

  // warn about missing or too many fpc.cfg
  if CfgFiles.Count<1 then begin
    AddWarning('no fpc.cfg found');
  end else if CfgFiles.Count>1 then begin
    for i:=0 to CfgFiles.Count-1 do
      AddWarning('multiple compiler configs found: '+CfgFiles[i]);
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
  TestGroupbox.Caption:='Test: Compiling an empty file ...';
  
  // get Test directory
  TestDir:=AppendPathDelim(EnvironmentOptions.TestBuildDirectory);
  if not DirPathExists(TestDir) then begin
    MessageDlg('Invalid Test Directory',
      'Please check the Test directory under'#13
      +'Environment -> Environment Options -> Files -> Directory for building test projects',
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  // create bogus file
  BogusFilename:=CreateNonExistingFilename(TestDir+'testcompileroptions.pas');
  if not CreateEmptyFile(BogusFilename) then begin
    MessageDlg('Unable to create Test File',
      'Unable to create Test pascal file "'+BogusFilename+'".',
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  try
    // create compiler command line options
    CmdLineParams:=Options.MakeOptionsString(BogusFilename,nil,
                              [ccloAddVerboseAll,ccloDoNotAppendOutFileOption])
                   +' '+BogusFilename;

    CompileTool:=TExternalToolOptions.Create;
    CompileTool.Title:='Test: Compiling empty file';
    CompileTool.ScanOutputForFPCMessages:=true;
    CompileTool.ScanOutputForMakeMessages:=true;
    CompileTool.WorkingDirectory:=TestDir;
    CompileTool.Filename:=CompilerFilename;
    CompileTool.CmdLineParams:=CmdLineParams;

    Result:=RunTool(CompileTool);
    FreeThenNil(CompileTool);
  finally
    DeleteFile(BogusFilename);
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
            AddWarning('relative unit path found in fpc cfg: '+NewPath);
            NewPath:=ExpandFileName(NewPath);
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
  TheProcess: TProcess;
  OutLen: Integer;
  LineStart: integer;
  i: Integer;
  CmdLine: string;
  Buf: string;
begin
  FPCCfgUnitPath:='';

  FTest:=cotCheckCompilerConfig;
  TestGroupbox.Caption:='Test: Checking compiler configuration ...';

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
    AddWarning('english message file for fpc is missing: components/codetools/fpc.errore.msg');

  if CurCompilerOptions<>'' then
    CmdLine:=CmdLine+CurCompilerOptions+' ';
  CmdLine:=CmdLine+ATestPascalFile;

  TheProcess := TProcess.Create(nil);
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
      if SysUtils.FindFirst(Directory+GetAllFilesMask,faAnyFile,FileInfo)=0
      then begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
            continue;
          // check extension
          if CompareFileExt(FileInfo.Name,'.ppu',
            {$IFDEF MSWINDOWS}false{$ELSE}true{$ENDIF})=0 then
            Result.Add(Directory+FileInfo.Name);
        until SysUtils.FindNext(FileInfo)<>0;
      end;
      SysUtils.FindClose(FileInfo);
    end;
  end;
end;

function TCheckCompilerOptsDlg.CheckMissingFPCPPUs(PPUs: TStrings
  ): TModalResult;
  
  function Check(const Unitname: string; Severity: TCompilerCheckMsgLvl
    ): Boolean;
  var
    i: Integer;
  begin
    for i:=0 to PPUs.Count-1 do begin
      if ExtractFileNameOnly(PPUs[i])=UnitName then exit(true);
    end;
    AddMsg(Severity,'compiled FPC unit not found: '+Unitname+'.ppu');
    Result:=ord(Severity)>=ord(ccmlError);
    if not Result then begin
      if MessageDlg('Missing unit',
        'The compiled FPC unit '+Unitname+'.ppu was not found.'#13
        +'This typically means your fpc.cfg has a bug. Or your FPC installation is broken.',
        mtError,[mbIgnore,mbAbort],0)=mrIgnore then
          Result:=true;
    end;
  end;
  
begin
  FTest:=cotCheckMissingFPCPPUs;
  TestGroupbox.Caption:='Test: Checking missing fpc ppu ...';

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
    CurDate:=FileAge(aFilename);
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
  TestGroupbox.Caption:='Test: Checking compiler date ...';

  Result:=mrCancel;
  
  CompilerDate:=FileAge(CompilerFilename);
  if CompilerDate=-1 then begin
    Result:=MessageDlg('Error','Unable to get file date of '+CompilerFilename+'.',
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
      Result:=MessageDlg('Warning','The dates of the .ppu files of FPC'
        +' differ more than one hour.'#13
        +'This can mean, they are from two different installations.'#13
        +'File1: '+MinPPU+#13
        +'File2: '+MaxPPU,
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
      Result:=MessageDlg('Warning','There is a .ppu file older than the compiler itself.'#13
        +MinPPU,
        mtError,[mbIgnore,mbAbort],0);
      if Result<>mrIgnore then
        exit;
    end;
  end;

  Result:=mrOk;
end;

function TCheckCompilerOptsDlg.CheckForAmbiguousPPUs(PPUs: TStrings
  ): TModalResult;
var
  i: Integer;
  j: Integer;
  CurUnitName: String;
  AnotherUnitName: String;
begin
  // resolve links and remove doubles
  ResolveLinksInFileList(PPUs,true);
  RemoveDoubles(PPUs);
  for i:=1 to PPUs.Count-1 do begin
    CurUnitName:=ExtractFileNameOnly(PPUs[i]);
    j:=i-1;
    while j>0 do begin
      AnotherUnitName:=ExtractFileNameOnly(PPUs[j]);
      if CompareText(AnotherUnitName,CurUnitName)=0 then begin
        // unit exists twice
        AddWarning('ppu exists twice: '+PPUs[i]+', '+PPUs[j]);
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
  TestGroupbox.Caption:='Test: Checking sources in fpc ppu search paths ...';

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
        if SysUtils.FindFirst(Directory+GetAllFilesMask,faAnyFile,FileInfo)=0
        then begin
          repeat
            // check if special file
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
              continue;
            // check extension
            if FilenameIsPascalUnit(FileInfo.Name) then begin
              AddWarning('FPC unit path contains a source: '+Directory+FileInfo.Name);
              WarnedDirectories.Add(Directory);
              break;
            end;
          until SysUtils.FindNext(FileInfo)<>0;
        end;
        SysUtils.FindClose(FileInfo);
      end;
    end;
  end;
  WarnedDirectories.Free;
  Result:=mrOk;
end;

procedure TCheckCompilerOptsDlg.SetMacroList(const AValue: TTransferMacroList);
begin
  if FMacroList=AValue then exit;
  FMacroList:=AValue;
end;

function TCheckCompilerOptsDlg.DoTest: TModalResult;
var
  CompilerFilename: String;
  CompileTool: TExternalToolOptions;
  CompilerFiles: TStrings;
  FPCCfgUnitPath: string;
  FPC_PPUs: TStrings;
  TargetUnitPath: String;
  Target_PPUs: TStrings;
begin
  Result:=mrCancel;
  if Test<>cotNone then exit;
  CompileTool:=nil;
  TestMemo.Lines.Clear;
  CompilerFiles:=nil;
  FPC_PPUs:=nil;
  Target_PPUs:=nil;
  try
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

    // check if unit paths do not contain sources
    Result:=CheckFPCUnitPathsContainSources(FPCCfgUnitPath);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // gather PPUs in project/package unit search paths
    TargetUnitPath:=Options.GetUnitPath(false);
    Target_PPUs:=FindAllPPUFiles(TargetUnitPath);

    // check if there are ambiguous ppu in project/package unit path
    Result:=CheckForAmbiguousPPUs(Target_PPUs);
    if not (Result in [mrOk,mrIgnore]) then exit;

    // compile bogus file
    Result:=CheckCompileBogusFile(CompilerFilename);
    if not (Result in [mrOk,mrIgnore]) then exit;
    
    if OutputListbox.Items.Count=0 then
      AddMsg('All tests succeeded.','',-1);

  finally
    CompilerFiles.Free;
    CompileTool.Free;
    FTest:=cotNone;
    TestGroupbox.Caption:='Test';
    FPC_PPUs.Free;
    Target_PPUs.Free;
  end;
  Result:=mrOk;
end;

constructor TCheckCompilerOptsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Application.AddOnIdleHandler(@ApplicationOnIdle,true);
  Caption:='Checking compiler options';
  CloseButton1.Caption:='Close';
  TestGroupbox.Caption:='Test';
  OutputGroupBox.Caption:='Results';
  CopyOutputMenuItem.Caption:='Copy output to clipboard';
end;

destructor TCheckCompilerOptsDlg.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  FDirectories.Free;
  inherited Destroy;
end;

function TCheckCompilerOptsDlg.RunTool(ExtTool: TExternalToolOptions
  ): TModalResult;
begin
  TestMemo.Lines.Text:=ExtTool.Filename+' '+ExtTool.CmdLineParams;
  Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,MacroList);
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
  ccmlWarning: Add('WARNING: '+Msg,'',false,-1);
  ccmlHint:    Add('HINT: '+Msg,'',false,-1);
  else         Add('ERROR: '+Msg,'',false,-1);
  end;
end;

initialization
  {$I checkcompileropts.lrs}

end.

