unit compiler_codegen_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  CompilerOptions, LazarusIDEStrConsts;

type

  { TCompilerCodegenOptionsFrame }

  TCompilerCodegenOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkOptSmaller: TCheckBox;
    chkLinkSmart: TCheckBox;
    chkOptionsLinkOpt: TCheckBox;
    chkSmartLinkUnit: TCheckBox;
    chkRelocatableUnit: TCheckBox;
    edtHeapSize: TEdit;
    edtOptionsLinkOpt: TEdit;
    edtStackSize: TEdit;
    grpOptimizations: TGroupBox;
    grpLinking: TGroupBox;
    grpHeapStackSize: TGroupBox;
    grpOptimizationLevels: TGroupBox;
    grpUnitStyle: TGroupBox;
    lbHeapSize: TLabel;
    lbStackSize: TLabel;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;
    radOptLevel4: TRadioButton;
    radOptLevelNone: TRadioButton;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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

  grpHeapStackSize.Caption := dlgHeapAndStackSize;
  lbHeapSize.Caption := dlgHeapSize + ' (-Ch)';
  lbStackSize.Caption := dlgStackSize + ' (-Cs)';
  edtHeapSize.Text := '';
  edtStackSize.Text := '';

  grpOptimizationLevels.Caption := dlgOptimizationLevels;
  radOptLevelNone.Caption := dlgLevelNoneOpt;
  radOptLevel1.Caption := dlgLevel1Opt + ' (-O1)';
  radOptLevel2.Caption := dlgLevel2Opt + ' (-O2)';
  radOptLevel3.Caption := dlgLevel3Opt + ' (-O3)';
  radOptLevel4.Caption := dlgLevel4Opt + ' (-O4)';

  grpOptimizations.Caption := dlgOtherOptimizations;
  chkOptSmaller.Caption := lisSmallerRatherThanFaster + ' (-Os)';

  grpLinking.Caption := dlgCOLinking;
  chkLinkSmart.Caption := dlgLinkSmart + ' (-XX)';
  chkOptionsLinkOpt.Caption := dlgPassOptsLinker;
  edtOptionsLinkOpt.Text := '';
end;

procedure TCompilerCodegenOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    chkSmartLinkUnit.Checked := SmartLinkUnit;
    chkRelocatableUnit.Checked := RelocatableUnit;

    grpHeapStackSize.Visible := NeedsLinkerOpts;
    edtHeapSize.Text := IntToStr(HeapSize);
    edtStackSize.Text := IntToStr(StackSize);

    //chkOptVarsInReg.Checked := VariablesInRegisters;
    //chkOptUncertain.Checked := UncertainOptimizations;
    chkOptSmaller.Checked := SmallerCode;

    case OptimizationLevel of
      1: radOptLevel1.Checked := True;
      2: radOptLevel2.Checked := True;
      3: radOptLevel3.Checked := True;
      4: radOptLevel4.Checked := True;
      else
        radOptLevelNone.Checked := True;
    end;
    grpLinking.Visible := NeedsLinkerOpts;
    chkLinkSmart.Checked := LinkSmart;
    chkLinkSmart.Enabled := NeedsLinkerOpts;
    chkOptionsLinkOpt.Checked := PassLinkerOptions;
    chkOptionsLinkOpt.Enabled := NeedsLinkerOpts;
    edtOptionsLinkOpt.Text := LinkerOptions;
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
    SmallerCode := chkOptSmaller.Checked;

    if (radOptLevel1.Checked) then
      OptimizationLevel := 1
    else
    if (radOptLevel2.Checked) then
      OptimizationLevel := 2
    else
    if (radOptLevel3.Checked) then
      OptimizationLevel := 3
    else
    if (radOptLevel4.Checked) then
      OptimizationLevel := 4
    else
      OptimizationLevel := 0;

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

