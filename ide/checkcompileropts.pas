unit CheckCompilerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, LazarusIDEStrConsts, IDEProcs, EnvironmentOpts, CompilerOptions,
  ExtToolEditDlg;

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
    FOptions: TCompilerOptions;
    FTest: TCompilerOptionsTest;
    procedure SetOptions(const AValue: TCompilerOptions);
  public
    procedure DoTest;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Options: TCompilerOptions read FOptions write SetOptions;
    property Test: TCompilerOptionsTest read FTest;
  end;


function ShowCheckCompilerOptsDlg(CompOpts: TCompilerOptions): TModalResult;

implementation

function ShowCheckCompilerOptsDlg(CompOpts: TCompilerOptions): TModalResult;
var
  CheckCompilerOptsDlg: TCheckCompilerOptsDlg;
begin
  CheckCompilerOptsDlg:=TCheckCompilerOptsDlg.Create(Application);
  CheckCompilerOptsDlg.Options:=CompOpts;
  Result:=CheckCompilerOptsDlg.ShowModal;
end;

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

procedure TCheckCompilerOptsDlg.DoTest;
var
  TestDir: String;
  BogusFilename: String;
  CompilerFilename: String;
  CompileTool: TExternalToolOptions;
  CmdLineParams: String;
begin
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
        'Environment -> Environment Options -> Files -> Directory for building test projects',
        mtError,[mbCancel],0);
      exit;
    end;
    // create bogus file
    BogusFilename:=CreateNonExistingFilename(TestDir+'testcompileroptions.pas');
    if not CreateEmptyFile(BogusFilename) then begin
      MessageDlg('Unable to create Test File',
        'Unable to create Test pascal file "'+BogusFilename+'".',
        mtError,[mbCancel],0);
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
      
      //Result:=EnvironmentOptions.ExternalTools.Run(CompileTool,MacroList);
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

initialization
  {$I checkcompileropts.lrs}

end.

