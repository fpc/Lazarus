unit compiler_codegen_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LCLProc,
  DefineTemplates, IDEOptionsIntf, Project, CompilerOptions,
  LazarusIDEStrConsts;

type

  { TCompilerCodegenOptionsFrame }

  TCompilerCodegenOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1:TBevel;
    chkChecksIO: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksStack: TCheckBox;
    chkOptSmaller: TCheckBox;
    chkOptUncertain: TCheckBox;
    chkOptVarsInReg: TCheckBox;
    chkSmartLinkUnit: TCheckBox;
    chkRelocatableUnit: TCheckBox;
    chkVerifyObjMethodCall: TCheckBox;
    edtHeapSize: TEdit;
    edtStackSize: TEdit;
    grpChecks: TGroupBox;
    grpHeapStackSize: TGroupBox;
    grpOptimizations: TGroupBox;
    grpUnitStyle: TGroupBox;
    grpTargetPlatform: TGroupBox;
    lbHeapSize: TLabel;
    lbStackSize: TLabel;
    lblTargetCPU: TLabel;
    lblTargetOS: TLabel;
    lblTargetProcessorProc: TLabel;
    radOptLevel1: TRadioButton;
    radOptLevel2: TRadioButton;
    radOptLevel3: TRadioButton;
    radOptLevelNone: TRadioButton;
    TargetCPUComboBox: TComboBox;
    TargetOSComboBox: TComboBox;
    TargetProcessorProcComboBox: TComboBox;
  private
    fLoaded: Boolean;
    FSaved: Boolean;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function CaptionToOS(const OS: string): string;
begin
  Result:=LowerCase(OS);
end;

function CaptionToCPU(const CPU: string): string;
begin
  Result:=LowerCase(CPU);
end;

function ProcessorToCaption(const Processor: string): string;
begin
  if SysUtils.CompareText(Processor, '386') = 0 then
    Result := '386/486' + ' (-Op386)'
  else if SysUtils.CompareText(Processor, 'pentium') = 0 then
    Result := 'Pentium/Pentium MMX (-OpPENTIUM)'
  else if SysUtils.CompareText(Processor, 'pentium2') = 0 then
    Result := 'Pentium Pro/Pentium II/C6x86/K6 (-OpPENTIUM2)'
  else if SysUtils.CompareText(Processor, 'pentium3') = 0 then
    Result := 'Pentium III (-OpPENTIUM3)'
  else if SysUtils.CompareText(Processor, 'pentium4') = 0 then
    Result := 'Pentium IV (-OpPENTIUM4)'
  else if SysUtils.CompareText(Processor, 'pentiumm') = 0 then
    Result := 'Pentium M (-OpPENTIUMM)'
  else
    Result := '(' + rsiwpDefault + ')';
end;

function CaptionToProcessor(const Caption: string): string;
begin
  if System.Pos('-Op386', Caption) > 0 then
    Result := '386'
  else if System.Pos('-OpPENTIUMM', Caption) > 0 then
    Result := 'pentiumm'
  else if System.Pos('-OpPENTIUM4', Caption) > 0 then
    Result := 'pentium4'
  else if System.Pos('-OpPENTIUM3', Caption) > 0 then
    Result := 'pentium3'
  else if System.Pos('-OpPENTIUM2', Caption) > 0 then
    Result := 'pentium2'
  else if System.Pos('-OpPENTIUM', Caption) > 0 then
    Result := 'pentium'
  else
    Result := '';
end;


{ TCompilerCodegenOptionsFrame }

function TCompilerCodegenOptionsFrame.GetTitle: string;
begin
  Result := dlgCodeGeneration;
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

  grpTargetPlatform.Caption := dlgTargetPlatform;
  lblTargetOS.Caption := dlgTargetOS + ' (-T)';

  with TargetOSComboBox do
  begin
    with Items do
    begin
      Add('(' + rsiwpDefault + ')');
      Add('Darwin');
      Add('FreeBSD');
      Add('Linux');
      Add('NetBSD');
      Add('OpenBSD');
      Add('Solaris');
      Add('Win32');
      Add('Win64');
      Add('WinCE');
      Add('go32v2');
      Add('os2');
      Add('beos');
      Add('haiku');
      Add('qnx');
      Add('netware');
      Add('wdosx');
      Add('emx');
      Add('watcom');
      Add('netwlibc');
      Add('amiga');
      Add('atari');
      Add('palmos');
      Add('gba');
      Add('nds');
      Add('macos');
      Add('morphos');
      Add('embedded');
      Add('symbian');
    end;
    ItemIndex := 0;
  end;

  lblTargetCPU.Caption := dlgTargetCPUFamily + ' (-P)';

  with TargetCPUComboBox do
  begin
    with Items do
    begin
      Add('(' + rsiwpDefault + ')');
      Add('arm');
      Add('i386');
      Add('m68k');
      Add('powerpc');
      Add('sparc');
      Add('x86_64');
    end;
    ItemIndex := 0;
  end;

  lblTargetProcessorProc.Caption := dlgTargetProc;

  with TargetProcessorProcComboBox do
  begin
    with Items do
    begin
      Clear;
      Add(ProcessorToCaption(''));
      Add(ProcessorToCaption('386'));
      Add(ProcessorToCaption('Pentium'));
      Add(ProcessorToCaption('Pentium2'));
      Add(ProcessorToCaption('Pentium3'));
      Add(ProcessorToCaption('Pentium4'));
      Add(ProcessorToCaption('PentiumM'));
    end;
    ItemIndex := 0;
  end;

  grpOptimizations.Caption := dlgOptimiz;
  radOptLevelNone.Caption := dlgLevelNoneOpt + ' (none)';
  radOptLevel1.Caption := dlgLevel1Opt + ' (-O1)';
  radOptLevel2.Caption := dlgLevel2Opt + ' (-O2)';
  radOptLevel3.Caption := dlgLevel3Opt + ' (-O3)';
  chkOptVarsInReg.Caption := dlgCOKeepVarsReg + ' (-Or)';
  chkOptUncertain.Caption := dlgUncertOpt + ' (-Ou)';
  chkOptSmaller.Caption := lisSmallerRatherThanFaster + ' (-Os)';
end;

procedure TCompilerCodegenOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: longint;
begin
  if fLoaded then exit;
  fLoaded:=true;
  with AOptions as TBaseCompilerOptions do
  begin
    chkSmartLinkUnit.Checked := SmartLinkUnit;
    chkRelocatableUnit.Checked := RelocatableUnit;

    chkChecksIO.Checked := IOChecks;
    chkChecksRange.Checked := RangeChecks;
    chkChecksOverflow.Checked := OverflowChecks;
    chkChecksStack.Checked := StackChecks;
    chkVerifyObjMethodCall.Checked := VerifyObjMethodCall;

    grpHeapStackSize.Enabled := NeedsLinkerOpts;
    edtHeapSize.Text := IntToStr(HeapSize);
    edtStackSize.Text := IntToStr(StackSize);

    i := TargetOSComboBox.Items.IndexOf(TargetOS);
    if i < 0 then
      i := 0;  // 0 is default
    TargetOSComboBox.ItemIndex := i;
    TargetOSComboBox.Text := TargetOS;
    i := TargetCPUComboBox.Items.IndexOf(TargetCPU);
    if i < 0 then
      i := 0;  // 0 is default
    TargetCPUComboBox.ItemIndex := i;
    TargetCPUComboBox.Text := TargetCPU;

    TargetProcessorProcComboBox.Text := ProcessorToCaption(TargetProcessor);

    chkOptVarsInReg.Checked := VariablesInRegisters;
    chkOptUncertain.Checked := UncertainOptimizations;
    chkOptSmaller.Checked := SmallerCode;

    case OptimizationLevel of
      1: radOptLevel1.Checked := True;
      2: radOptLevel2.Checked := True;
      3: radOptLevel3.Checked := True;
      else
        radOptLevelNone.Checked := True;
    end;
  end;
end;

procedure TCompilerCodegenOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  hs, code: integer;
  NewTargetOS: string;
  NewTargetCPU: string;
begin
  if FSaved then exit;
  FSaved:=true;
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

    NewTargetOS := TargetOSComboBox.Text;
    if TargetOSComboBox.Items.IndexOf(NewTargetOS) <= 0 then
      NewTargetOS := '';
    TargetOS := CaptionToOS(NewTargetOS);

    NewTargetCPU := TargetCPUComboBox.Text;
    if TargetCPUComboBox.Items.IndexOf(NewTargetCPU) <= 0 then
      NewTargetCPU := '';
    TargetCPU := CaptionToCPU(NewTargetCPU);

    TargetProcessor := CaptionToProcessor(TargetProcessorProcComboBox.Text);
    VariablesInRegisters := chkOptVarsInReg.Checked;
    UncertainOptimizations := chkOptUncertain.Checked;
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
      OptimizationLevel := 0;
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

