unit compiler_codegen_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StdCtrls, IDEOptionsIntf, CompilerOptions, PackageDefs,
  LazarusIDEStrConsts;

type

  { TCompilerCodegenOptionsFrame }

  TCompilerCodegenOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkChecksIO: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksStack: TCheckBox;
    chkLinkSmart: TCheckBox;
    chkOptionsLinkOpt: TCheckBox;
    chkSmartLinkUnit: TCheckBox;
    chkRelocatableUnit: TCheckBox;
    chkVerifyObjMethodCall: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    edtHeapSize: TEdit;
    edtOptionsLinkOpt: TEdit;
    edtStackSize: TEdit;
    grpLinking: TGroupBox;
    grpChecks: TGroupBox;
    grpHeapStackSize: TGroupBox;
    grpOptimizations: TGroupBox;
    grpUnitStyle: TGroupBox;
    lbHeapSize: TLabel;
    lbStackSize: TLabel;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;
    radOptLevelNone: TRadioButton;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerCodegenOptionsFrame }

function TCompilerCodegenOptionsFrame.GetTitle: string;
begin
  Result := dlgCompilationAndLinking;
end;

procedure TCompilerCodegenOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpUnitStyle.Caption := dlgCOUnitStyle;
  chkSmartLinkUnit.Caption := dlgCOSmartLinkable + ' (-CX)';
  chkRelocatableUnit.Caption := dlgCORelocatable + ' (-WR)';

  grpChecks.Caption := dlgCOChecks;
  chkChecksIO.Caption := 'I/O (-Ci)';
  chkChecksRange.Caption := dlgCORange + ' (-Cr)';
  chkChecksOverflow.Caption := dlgCOOverflow + ' (-Co)';
  chkChecksStack.Caption := dlgCOStack + ' (-Ct)';
  chkVerifyObjMethodCall.Caption := lisVerifyMethodCalls + ' (-CR)';

  grpHeapStackSize.Caption := dlgHeapAndStackSize;
  lbHeapSize.Caption := dlgHeapSize + ' (-Ch)';
  lbStackSize.Caption := dlgStackSize + ' (-Cs)';
  edtHeapSize.Text := '';
  edtStackSize.Text := '';

  grpOptimizations.Caption := dlgOptimizationLevels;
  radOptLevelNone.Caption := dlgLevelNoneOpt;
  radOptLevel1.Caption := dlgLevel1Opt + ' (-O1)';
  radOptLevel2.Caption := dlgLevel2Opt + ' (-O2)';
  radOptLevel3.Caption := dlgLevel3Opt + ' (-O3)';

  grpLinking.Caption := dlgCOLinking;
  chkLinkSmart.Caption := dlgLinkSmart + ' (-XX)';
  chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG)';
  chkOptionsLinkOpt.Caption := dlgPassOptsLinker;
  edtOptionsLinkOpt.Text := '';
end;

procedure TCompilerCodegenOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    chkSmartLinkUnit.Checked := SmartLinkUnit;
    chkRelocatableUnit.Checked := RelocatableUnit;

    chkChecksIO.Checked := IOChecks;
    chkChecksRange.Checked := RangeChecks;
    chkChecksOverflow.Checked := OverflowChecks;
    chkChecksStack.Checked := StackChecks;
    chkVerifyObjMethodCall.Checked := VerifyObjMethodCall;

    grpHeapStackSize.Visible := NeedsLinkerOpts;
    edtHeapSize.Text := IntToStr(HeapSize);
    edtStackSize.Text := IntToStr(StackSize);

    //chkOptVarsInReg.Checked := VariablesInRegisters;
    //chkOptUncertain.Checked := UncertainOptimizations;
    //chkOptSmaller.Checked := SmallerCode;

    case OptimizationLevel of
      1: radOptLevel1.Checked := True;
      2: radOptLevel2.Checked := True;
      3: radOptLevel3.Checked := True;
      else
        radOptLevelNone.Checked := True;
    end;
    grpLinking.Enabled := NeedsLinkerOpts;
    chkLinkSmart.Checked := LinkSmart;
    //chkLinkSmart.Enabled := NeedsLinkerOpts;
    chkWin32GraphicApp.Checked := Win32GraphicApp;
    //chkWin32GraphicApp.Enabled := NeedsLinkerOpts;
    chkOptionsLinkOpt.Checked := PassLinkerOptions;
    edtOptionsLinkOpt.Text := LinkerOptions;
    //chkOptionsLinkOpt.Enabled := NeedsLinkerOpts;
  end;
end;

procedure TCompilerCodegenOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  hs, code: integer;
begin
  with AOptions as TBaseCompilerOptions do
  begin
    SmartLinkUnit := chkSmartLinkUnit.Checked;
    RelocatableUnit := chkRelocatableUnit.Checked;

    IOChecks := chkChecksIO.Checked;
    RangeChecks := chkChecksRange.Checked;
    OverflowChecks := chkChecksOverflow.Checked;
    StackChecks := chkChecksStack.Checked;
    VerifyObjMethodCall := chkVerifyObjMethodCall.Checked;

    Val(edtHeapSize.Text, hs, code);
    if (code <> 0) then
      HeapSize := 0
    else
      HeapSize := hs;

    Val(edtStackSize.Text, hs, code);
    if (code <> 0) then
      StackSize := 0
    else
      StackSize := hs;

    //VariablesInRegisters := chkOptVarsInReg.Checked;
    //UncertainOptimizations := chkOptUncertain.Checked;
    //SmallerCode := chkOptSmaller.Checked;

    if (radOptLevel1.Checked) then
      OptimizationLevel := 1
    else
    if (radOptLevel2.Checked) then
      OptimizationLevel := 2
    else
    if (radOptLevel3.Checked) then
      OptimizationLevel := 3
    else
      OptimizationLevel := 0;

    Win32GraphicApp := chkWin32GraphicApp.Checked;
    LinkSmart := chkLinkSmart.Checked;
    PassLinkerOptions := chkOptionsLinkOpt.Checked;
    LinkerOptions := edtOptionsLinkOpt.Text;
  end;
end;

class function TCompilerCodegenOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerCodegenOptionsFrame,
    CompilerOptionsCodeGeneration);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerCodegenOptionsFrame,
    CompilerOptionsCodeGeneration);

end.

