unit CheckCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, LazarusIDEStrConsts, FileCtrl, IDEProcs, EnvironmentOpts,
  CompilerOptions, ExtToolEditDlg, TransferMacros;

type
  TCompilerOptionsTest = (
    cotNone,
    cotCheckCompilerExe,
    cotCompileBogusFiles
    );

  TCheckCompilerOptsDlg = class(TForm)
    CloseButton: TBUTTON;
    OutputLabel: TLABEL;
    OutputMemo: TMEMO;
    OutputGroupBox: TGROUPBOX;
    procedure ApplicationOnIdle(Sender: TObject);
    procedure CloseButtonCLICK(Sender: TObject);
  private
    FMacroList: TTransferMacroList;
    FOptions: TCompilerOptions;
    FTest: TCompilerOptionsTest;
    procedure SetMacroList(const AValue: TTransferMacroList);
    procedure SetOptions(const AValue: TCompilerOptions);
  public
    function DoTest: TModalResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function RunTool(ExtTool: TExternalToolOptions): TModalResult;
    procedure AddMsg(const Msg, CurDir: String);
    procedure AddProgress(const Msg, CurDir: String);
  public
    property Options: TCompilerOptions read FOptions write SetOptions;
    property Test: TCompilerOptionsTest read FTest;
    property MacroList: TTransferMacroList read FMacroList write SetMacroList;
  end;


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
begin
  Result:=mrCancel;
  if Test<>cotNone then exit;
  try
    // check compiler filename
    FTest:=cotCheckCompilerExe;
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
    
    // compile bogus file
    FTest:=cotCompileBogusFiles;
    // get Test directory
    TestDir:=AppendPathDelim(EnvironmentOptions.TestBuildDirectory);
    if not DirectoryExists(TestDir) then begin
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
    // create compiler command line options
    CmdLineParams:=Options.MakeOptionsString(BogusFilename,[ccloAddVerboseAll]);
    
    CompileTool:=TExternalToolOptions.Create;
    try
      CompileTool.Title:='Test: Compiling bogus filename';
      CompileTool.ScanOutputForFPCMessages:=true;
      CompileTool.ScanOutputForMakeMessages:=true;
      CompileTool.WorkingDirectory:=TestDir;
      CompileTool.Filename:=CompilerFilename;
      CompileTool.CmdLineParams:=CmdLineParams;
      
      Result:=RunTool(CompileTool);
    finally
      // clean up
      CompileTool.Free;
    end;

  finally
    FTest:=cotNone;
  end;
end;

constructor TCheckCompilerOptsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Application.AddOnIdleHandler(@ApplicationOnIdle);
end;

destructor TCheckCompilerOptsDlg.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  inherited Destroy;
end;

function TCheckCompilerOptsDlg.RunTool(ExtTool: TExternalToolOptions
  ): TModalResult;
begin
  Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,MacroList);
end;

procedure TCheckCompilerOptsDlg.AddMsg(const Msg, CurDir: String);
begin
  // ToDo
end;

procedure TCheckCompilerOptsDlg.AddProgress(const Msg, CurDir: String);
begin
  // ToDo
end;

initialization
  {$I checkcompileropts.lrs}

end.

