unit CheckCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, FileUtil,
  IDEExternToolIntf,
  IDEProcs, EnvironmentOpts, LazarusIDEStrConsts,
  CompilerOptions, ExtToolEditDlg, TransferMacros, LazConf;

type
  TCompilerOptionsTest = (
    cotNone,
    cotCheckCompilerExe,
    cotCompileBogusFiles
    );

  { TCheckCompilerOptsDlg }

  TCheckCompilerOptsDlg = class(TForm)
    CloseButton1: TBitBtn;
    TestMemo: TMEMO;
    TestGroupbox: TGROUPBOX;
    OutputListbox: TLISTBOX;
    OutputGroupBox: TGROUPBOX;
    procedure ApplicationOnIdle(Sender: TObject);
    procedure CloseButtonCLICK(Sender: TObject);
  private
    FMacroList: TTransferMacroList;
    FOptions: TCompilerOptions;
    FTest: TCompilerOptionsTest;
    FLastLineIsProgress: boolean;
    FDirectories: TStringList;
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure SetOptions(const AValue: TCompilerOptions);
    procedure SetMsgDirectory(Index: integer; const CurDir: string);
  public
    function DoTest: TModalResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function RunTool(ExtTool: TExternalToolOptions): TModalResult;
    procedure Add(const Msg, CurDir: String; ProgressLine: boolean;
                  OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: String; OriginalIndex: integer);
    procedure AddProgress(Line: TIDEScanMessageLine);
  public
    property Options: TCompilerOptions read FOptions write SetOptions;
    property Test: TCompilerOptionsTest read FTest;
    property MacroList: TTransferMacroList read FMacroList write SetMacroList;
  end;

var
  CheckCompilerOptsDlg: TCheckCompilerOptsDlg;

implementation

{ TCheckCompilerOptsDlg }

procedure TCheckCompilerOptsDlg.ApplicationOnIdle(Sender: TObject);
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  DoTest;
end;

procedure TCheckCompilerOptsDlg.CloseButtonCLICK(Sender: TObject);
begin
  ModalResult:=mrOk;
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

procedure TCheckCompilerOptsDlg.SetMacroList(const AValue: TTransferMacroList);
begin
  if FMacroList=AValue then exit;
  FMacroList:=AValue;
end;

function TCheckCompilerOptsDlg.DoTest: TModalResult;
var
  TestDir: String;
  BogusFilename: String;
  CompilerFilename: String;
  CompileTool: TExternalToolOptions;
  CmdLineParams: String;
  CompilerFiles: TStrings;
begin
  Result:=mrCancel;
  if Test<>cotNone then exit;
  CompileTool:=nil;
  TestMemo.Lines.Clear;
  CompilerFiles:=nil;
  try
    // check compiler filename
    FTest:=cotCheckCompilerExe;
    TestGroupbox.Caption:='Test: Checking compiler ...';
    CompilerFilename:=Options.ParsedOpts.GetParsedValue(pcosCompilerPath);
    try
      CheckIfFileIsExecutable(CompilerFilename);
    except
      on e: Exception do begin
        Result:=MessageDlg('Invalid compiler',
          'The compiler "'+CompilerFilename+'" is not an executable file.',
          mtError,[mbCancel,mbAbort],0);
        exit;
      end;
    end;
    
    // check if there are several compilers in path
    CompilerFiles:=SearchAllFilesInPath(GetDefaultCompilerFilename,'',
         SysUtils.GetEnvironmentVariable('PATH'),':',[sffDontSearchInBasePath]);
    if (CompilerFiles<>nil) and (CompilerFiles.Count>1) then begin
      Result:=MessageDlg('Ambiguous Compiler',
        'There are several FreePascal Compilers in your path.'#13#13
        +CompilerFiles.Text+#13
        +'Maybe you forgot to delete an old compiler?',
        mtWarning,[mbCancel,mbIgnore],0);
      if Result<>mrIgnore then exit;
    end;
    
    // TODO: compiler check: check if compiler paths includes base units
    // TODO: compiler check: check if compiler is older than fpc units (ppu version)

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

  finally
    CompilerFiles.Free;
    CompileTool.Free;
    FTest:=cotNone;
    TestGroupbox.Caption:='Test';
  end;
end;

constructor TCheckCompilerOptsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Application.AddOnIdleHandler(@ApplicationOnIdle,true);
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
Begin
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

initialization
  {$I checkcompileropts.lrs}

end.

