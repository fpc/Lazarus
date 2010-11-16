unit compiler_linking_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  StdCtrls, IDEOptionsIntf, Project, CompilerOptions, LazarusIDEStrConsts;

type

  { TCompilerLinkingOptionsFrame }

  TCompilerLinkingOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkDebugGDB: TCheckBox;
    chkGenerateDwarf: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkLinkSmart: TCheckBox;
    chkOptionsLinkOpt: TCheckBox;
    chkSymbolsStrip: TCheckBox;
    chkUseExternalDbgSyms: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseValgrind: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    edtOptionsLinkOpt: TEdit;
    grpDebugging: TGroupBox;
    grpLinkLibraries: TGroupBox;
    grpOptions: TGroupBox;
    TargetSpecificsGrpBox: TGroupBox;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}


{ TCompilerLinkingOptionsFrame }

function TCompilerLinkingOptionsFrame.GetTitle: string;
begin
  Result := dlgCOLinking;
end;

procedure TCompilerLinkingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Setup the Linking Tab
  with grpDebugging do
  begin
    AutoSize := True;
    Caption := dlgCODebugging;
  end;

  chkDebugGDB.Caption := dlgCOGDB + ' (-g)';
  chkUseLineInfoUnit.Caption := dlgLNumsBct + ' (-gl)';
  chkGenerateDwarf.Caption := dlgGenerateDwarf + '(-gw)';
  chkUseHeaptrc.Caption := dlgCOHeaptrc + ' (-gh)';
  chkUseValgrind.Caption := dlgCOValgrind + ' (-gv)';
  chkGenGProfCode.Caption := dlgGPROF + ' (-pg)';
  chkSymbolsStrip.Caption := dlgCOStrip + ' (-Xs)';
  chkUseExternalDbgSyms.Caption := dlgExtSymb + ' (-Xg)';

  grpLinkLibraries.Caption := dlgLinkLibraries;
  chkLinkSmart.Caption := dlgLinkSmart + ' (-XX)';

  TargetSpecificsGrpBox.Caption := lisCOTargetOSSpecificOptions;
  chkWin32GraphicApp.Caption := dlgWin32GUIApp + ' (-WG)';

  grpOptions.Caption := dlgCOOpts + ' (-k)';
  chkOptionsLinkOpt.Caption := dlgPassOptsLinker;
  edtOptionsLinkOpt.Text := '';
end;

procedure TCompilerLinkingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    chkDebugGDB.Checked := GenerateDebugInfo;
    chkUseLineInfoUnit.Checked := UseLineInfoUnit;
    chkGenerateDwarf.Checked := GenerateDwarf;
    chkUseHeaptrc.Checked := UseHeaptrc;
    chkUseValgrind.Checked := UseValgrind;
    chkGenGProfCode.Checked := GenGProfCode;
    chkSymbolsStrip.Checked := StripSymbols;
    chkSymbolsStrip.Enabled := NeedsLinkerOpts;
    chkUseExternalDbgSyms.Checked := UseExternalDbgSyms;

    chkLinkSmart.Checked := LinkSmart;
    grpLinkLibraries.Enabled := NeedsLinkerOpts;

    chkOptionsLinkOpt.Checked := PassLinkerOptions;
    edtOptionsLinkOpt.Text := LinkerOptions;
    chkWin32GraphicApp.Checked := Win32GraphicApp;
    chkWin32GraphicApp.Enabled := NeedsLinkerOpts;
    grpOptions.Enabled := NeedsLinkerOpts;
  end;
end;

procedure TCompilerLinkingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    GenerateDebugInfo := chkDebugGDB.Checked;
    UseLineInfoUnit := chkUseLineInfoUnit.Checked;
    GenerateDwarf := chkGenerateDwarf.Checked;
    UseHeaptrc := chkUseHeaptrc.Checked;
    UseValgrind := chkUseValgrind.Checked;
    GenGProfCode := chkGenGProfCode.Checked;
    StripSymbols := chkSymbolsStrip.Checked;
    UseExternalDbgSyms := chkUseExternalDbgSyms.Checked;

    PassLinkerOptions := chkOptionsLinkOpt.Checked;
    LinkerOptions := edtOptionsLinkOpt.Text;
    Win32GraphicApp := chkWin32GraphicApp.Checked;
    LinkSmart := chkLinkSmart.Checked;
  end;
end;

class function TCompilerLinkingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerLinkingOptionsFrame,
    CompilerOptionsLinking);

end.

