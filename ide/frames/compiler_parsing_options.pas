unit compiler_parsing_options;

{$mode objfpc}{$H+}

interface

uses
  ExtCtrls, StdCtrls, SysUtils, IDEOptionsIntf, CompilerOptions,
  LinkScanner, PackageDefs, LazarusIDEStrConsts;

type

  { TCompilerParsingOptionsFrame }

  TCompilerParsingOptionsFrame = class(TAbstractIDEOptionsEditor)
    cmbSyntaxMode: TComboBox;
    grpAsmStyle: TRadioGroup;
    grpSyntaxMode: TGroupBox;
    grpSyntaxOptions: TCheckGroup;
  private
    FIsPackage: boolean;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function SyntaxModeToCaption(const Mode: string): string;
begin
  if CompareText(Mode, 'ObjFPC') = 0 then
    Result := lisObjectPascalDefault + ' (-Mobjfpc)'
  else if CompareText(Mode, 'Delphi') = 0 then
    Result := 'Delphi (-Mdelphi)'
  else if CompareText(Mode, 'DelphiUnicode') = 0 then
    Result := 'DelphiUnicode (-Mdelphiunicode)'
  else if CompareText(Mode, 'tp') = 0 then
    Result := 'Turbo Pascal (-Mtp)'
  else if CompareText(Mode, 'fpc') = 0 then
    Result := 'Free Pascal (-Mfpc)'
  else if CompareText(Mode, 'macpas') = 0 then
    Result := 'Mac Pascal (-Mmacpas)'
  else if CompareText(Mode, 'iso') = 0 then
    Result := 'ISO/IEC 7185 Pascal (-Miso)'
  else
    Result := '';
end;

function CaptionToSyntaxMode(const Caption: string): string;
begin
  if Pos('-Mdelphi', Caption) > 0 then
    Result := 'Delphi'
  else if Pos('-Mdelphiunicode', Caption) > 0 then
    Result := 'delphiunicode'
  else if Pos('-Mfpc', Caption) > 0 then
    Result := 'fpc'
  else if Pos('-Mtp', Caption) > 0 then
    Result := 'tp'
  else if Pos('-Mmacpas', Caption) > 0 then
    Result := 'macpas'
  else if Pos('-Miso', Caption) > 0 then
    Result := 'iso'
  else
    Result := 'ObjFPC';
end;


{ TCompilerParsingOptionsFrame }

function TCompilerParsingOptionsFrame.GetTitle: string;
begin
  Result := dlgCOParsing;
end;

procedure TCompilerParsingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  m: TCompilerMode;
  s: string;
begin
  grpSyntaxMode.Caption := lisSyntaxMode + ' (-M, {$MODE})';
  cmbSyntaxMode.Items.BeginUpdate;
  cmbSyntaxMode.Items.Clear;
  for m := Low(TCompilerMode) to High(TCompilerMode) do
  begin
    s := SyntaxModeToCaption(CompilerModeNames[m]);
    if s <> '' then
      cmbSyntaxMode.Items.Add(s);
  end;
  cmbSyntaxMode.Items.EndUpdate;

  with grpAsmStyle do
  begin
    Caption := dlgCOAsmStyle + ' (-R)';
    Items.BeginUpdate;
    Items.Clear;
    Items.Add(lisDefault);
    Items.Add('Intel');
    Items.Add('AT&&T');
    Items.EndUpdate;
  end;

  with grpSyntaxOptions do
  begin
    AutoSize := True;
    Caption := dlgSyntaxOptions;
    Items.BeginUpdate;
    Items.Add(dlgCOCOps + ' (-Sc, {$COPERATORS ON})');
    Items.Add(dlgLabelGoto + ' (-Sg, {$GOTO ON})');
    Items.Add(dlgCppInline + ' (-Si, {$INLINE ON})');
    Items.Add(dlgCMacro + ' (-Sm, {$MACRO ON})');
    Items.Add(dlgInitDoneOnly + ' (-Ss)');
    Items.Add(dlgStaticKeyword + ' (-St)');
    Items.Add(dlgCOAnsiStr + ' (-Sh, {$H+})');
    Items.EndUpdate;
  end;
end;

procedure TCompilerParsingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  CurOptions: TBaseCompilerOptions;
begin
  CurOptions := AOptions as TBaseCompilerOptions;
  FIsPackage := CurOptions is TPkgCompilerOptions;
  with CurOptions do
  begin
    cmbSyntaxMode.Text := SyntaxModeToCaption(SyntaxMode);

    if (AssemblerStyle in [1,2,3]) then
      grpAsmStyle.ItemIndex := AssemblerStyle
    else
      grpAsmStyle.ItemIndex := 0;

    with grpSyntaxOptions do
    begin
      Checked[0] := CStyleOperators;
      Checked[1] := AllowLabel;
      Checked[2] := CPPInline;
      Checked[3] := CStyleMacros;
      Checked[4] := InitConstructor;
      Checked[5] := StaticKeyword;
      Checked[6] := UseAnsiStrings;
    end;
  end;
end;

procedure TCompilerParsingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    SyntaxMode := CaptionToSyntaxMode(cmbSyntaxMode.Text);

    AssemblerStyle := grpAsmStyle.ItemIndex;

    with grpSyntaxOptions do
    begin
      CStyleOperators := Checked[0];
      AllowLabel := Checked[1];
      CPPInline := Checked[2];
      CStyleMacros := Checked[3];
      InitConstructor := Checked[4];
      StaticKeyword := Checked[5];
      UseAnsiStrings := Checked[6];
    end;
  end;
end;

class function TCompilerParsingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerParsingOptionsFrame,
    CompilerOptionsParsing);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerParsingOptionsFrame,
    CompilerOptionsParsing);

end.

