unit compiler_parsing_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IDEOptionsIntf, Project, CompilerOptions, //LinkScanner,
  LazarusIDEStrConsts;

type

  { TCompilerParsingOptionsFrame }

  TCompilerParsingOptionsFrame = class(TAbstractIDEOptionsEditor)
    grpAsmStyle: TRadioGroup;
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

{ TCompilerParsingOptionsFrame }

function TCompilerParsingOptionsFrame.GetTitle: string;
begin
  Result := dlgCOParsing;
end;

procedure TCompilerParsingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  with grpAsmStyle do
  begin
    Caption := dlgCOAsmStyle + ' (-R)';

    with Items do
    begin
      BeginUpdate;
      Add(lisDefault);
      Add('Intel');
      Add('AT&&T');
      EndUpdate;
    end;
  end;

  with grpSyntaxOptions do
  begin
    AutoSize := True;
    Caption := dlgSyntaxOptions;

    Items.Add(dlgCOCOps + ' (-Sc, {$COPERATORS ON})');
    Items.Add(dlgAssertCode + ' (-Sa, {$ASSERTIONS ON})');
    Items.Add(dlgLabelGoto + ' (-Sg, {$GOTO ON})');
    Items.Add(dlgCppInline + ' (-Si, {$INLINE ON})');
    Items.Add(dlgCMacro + ' (-Sm, {$MACRO ON})');
    Items.Add(dlgInitDoneOnly + ' (-Ss)');
    Items.Add(dlgStaticKeyword + ' (-St)');
    Items.Add(dlgCOAnsiStr + ' (-Sh, {$H+})');
  end;
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

