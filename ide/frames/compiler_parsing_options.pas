unit compiler_parsing_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IDEOptionsIntf, Project, CompilerOptions, LinkScanner,
  LazarusIDEStrConsts;

type

  { TCompilerParsingOptionsFrame }

  TCompilerParsingOptionsFrame = class(TAbstractIDEOptionsEditor)
    cmbSyntaxMode: TComboBox;
    grpAsmStyle: TRadioGroup;
    grpSyntaxMode: TGroupBox;
    grpSyntaxOptions: TCheckGroup;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function SyntaxModeToCaption(const Mode: string): string;
begin
  if SysUtils.CompareText(Mode, 'ObjFPC') = 0 then
    Result := lisObjectPascalDefault + ' (-Mobjfpc)'
  else if SysUtils.CompareText(Mode, 'Delphi') = 0 then
    Result := lisDelphi + ' (-Mdelphi)'
  else if SysUtils.CompareText(Mode, 'tp') = 0 then
    Result := lisTurboPascal + ' (-Mtp)'
  else if SysUtils.CompareText(Mode, 'fpc') = 0 then
    Result := lisFreePascal + ' (-Mfpc)'
  else if SysUtils.CompareText(Mode, 'macpas') = 0 then
    Result := lisMacPascal + ' (-Mmacpas)'
  else
    Result := '';
end;

function CaptionToSyntaxMode(const Caption: string): string;
begin
  if System.Pos('-Mdelphi', Caption) > 0 then
    Result := 'Delphi'
  else if System.Pos('-Mtp', Caption) > 0 then
    Result := 'tp'
  else if System.Pos('-Mmacpas', Caption) > 0 then
    Result := 'macpas'
  else if System.Pos('-Mfpc', Caption) > 0 then
    Result := 'fpc'
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
  with grpAsmStyle do
  begin
    Caption := dlgCOAsmStyle + ' (-R)';

    with Items do
    begin
      BeginUpdate;
      Add(dlgAssemblerDefault);
      Add('Intel');
      Add('AT&&T');
      EndUpdate;
    end;
  end;

  with grpSyntaxOptions do
  begin
    AutoSize := True;
    Caption := dlgSyntaxOptions;

    Items.Add(dlgCOCOps + ' (-Sc)');
    Items.Add(dlgAssertCode + ' (-Sa)');
    Items.Add(dlgLabelGoto + ' (-Sg)');
    Items.Add(dlgCppInline + ' (-Si)');
    Items.Add(dlgCMacro + ' (-Sm)');
    Items.Add(dlgInitDoneOnly + ' (-Ss)');
    Items.Add(dlgStaticKeyword + ' (-St)');
    Items.Add(dlgCOAnsiStr + ' (-Sh)');
  end;

  grpSyntaxMode.Caption := lisSyntaxMode + ' (-M)';
  cmbSyntaxMode.Items.BeginUpdate;
  cmbSyntaxMode.Items.Clear;
  for m := Low(TCompilerMode) to High(TCompilerMode) do
  begin
    s := SyntaxModeToCaption(CompilerModeNames[m]);
    if s <> '' then
      cmbSyntaxMode.Items.Add(s);
  end;
  cmbSyntaxMode.Items.EndUpdate;
end;

procedure TCompilerParsingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    if (AssemblerStyle in [1,2,3]) then
      grpAsmStyle.ItemIndex := AssemblerStyle
    else
      grpAsmStyle.ItemIndex := 0;

    with grpSyntaxOptions do
    begin
      Checked[0] := CStyleOperators;
      Checked[1] := IncludeAssertionCode;
      Checked[2] := AllowLabel;
      Checked[3] := CPPInline;
      Checked[4] := CStyleMacros;
      Checked[5] := InitConstructor;
      Checked[6] := StaticKeyword;
      Checked[7] := UseAnsiStrings;
    end;

    cmbSyntaxMode.Text := SyntaxModeToCaption(SyntaxMode);
  end;
end;

procedure TCompilerParsingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    AssemblerStyle := grpAsmStyle.ItemIndex;

    with grpSyntaxOptions do
    begin
      CStyleOperators := Checked[0];
      IncludeAssertionCode := Checked[1];
      AllowLabel := Checked[2];
      CPPInline := Checked[3];
      CStyleMacros := Checked[4];
      InitConstructor := Checked[5];
      StaticKeyword := Checked[6];
      UseAnsiStrings := Checked[7];
    end;

    SyntaxMode := CaptionToSyntaxMode(cmbSyntaxMode.Text);
  end;
end;

class function TCompilerParsingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerParsingOptionsFrame,
    CompilerOptionsParsing);

end.

