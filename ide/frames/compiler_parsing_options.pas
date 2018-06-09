unit compiler_parsing_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  ExtCtrls, StdCtrls,
  // CodeTools
  LinkScanner,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  CompilerOptions, PackageDefs, LazarusIDEStrConsts;

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

const
  // CompilerMode names to be shown after -M...
  CompilerModesPretty: array[TCompilerMode] of shortstring = (
      'fpc', 'Delphi', 'DelphiUnicode', 'gpc', 'tp', 'ObjFPC', 'MacPas', 'iso',
      'ExtPas'
    );
  // CompilerMode descriptions.
  CompilerModesDescr: array[TCompilerMode] of shortstring = (
      'Free Pascal', 'Delphi', 'Delphi Unicode', 'GNU Pascal', 'Turbo Pascal',
      'Object Pascal', 'Mac Pascal', 'ISO/IEC 7185 Pascal',
      'Extended Pascal, ISO 10206'
    );

function SyntaxModeToCaption(const ModeStr: string): string;
var
  cm: TCompilerMode;
begin
  Result := '';
  for cm := Low(TCompilerMode) to High(TCompilerMode) do
    if CompareText(ModeStr, CompilerModeNames[cm]) = 0 then
    begin
      if cm = cmOBJFPC then
        Result := lisObjectPascalDefault  // Is this needed?
      else
        Result := CompilerModesDescr[cm];
      Result := Result + ' (-M' + CompilerModesPretty[cm] + ')';
      Break;
    end;
end;

function CaptionToSyntaxMode(const Caption: string): string;
var
  cm: TCompilerMode;
begin
  Result := 'ObjFPC';
  // Some modes would go wrong if iterated forward, thus iterate backwards.
  for cm := High(TCompilerMode) downto Low(TCompilerMode) do
    if Pos('-M' + CompilerModesPretty[cm], Caption) > 0 then
    begin
      Result := CompilerModesPretty[cm];
      Break;
    end;
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

