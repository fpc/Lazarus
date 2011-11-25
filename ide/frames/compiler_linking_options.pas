unit compiler_linking_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  StdCtrls, IDEOptionsIntf, Project, CompilerOptions, CompOptsIntf, LazarusIDEStrConsts;

type

  { TCompilerLinkingOptionsFrame }

  TCompilerLinkingOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkDebugGDB: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkLinkSmart: TCheckBox;
    chkOptionsLinkOpt: TCheckBox;
    chkSymbolsStrip: TCheckBox;
    chkUseExternalDbgSyms: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseValgrind: TCheckBox;
    chkWin32GraphicApp: TCheckBox;
    dropDbgSymbolType: TComboBox;
    edtOptionsLinkOpt: TEdit;
    grpDebug2: TGroupBox;
    grpDebugging: TGroupBox;
    grpLinkLibraries: TGroupBox;
    grpOptions: TGroupBox;
    lblEmpty: TLabel;
    lblDbgSymbolType: TLabel;
    TargetSpecificsGrpBox: TGroupBox;
    procedure chkDebugGDBChange(Sender: TObject);
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

const
  ST_AUTO_IDX      = 0;
  ST_STABS_IDX     = 3;
  ST_DWARF2_IDX    = 2;
  ST_DWARF2SET_IDX = 1;
  ST_DWARF3_IDX    = 4;

function SymbolToIndex(SymbolType: TCompilerDbgSymbolType): Integer;
begin
  case SymbolType of
    dsAuto:      Result := ST_AUTO_IDX;
    dsStabs:     Result := ST_STABS_IDX;
    dsDwarf2:    Result := ST_DWARF2_IDX;
    dsDwarf2Set: Result := ST_DWARF2SET_IDX;
    dsDwarf3:    Result := ST_DWARF3_IDX;
  end;
end;

function IndexToSymbol(Index: Integer): TCompilerDbgSymbolType;
begin
  case Index of
    ST_AUTO_IDX:      Result := dsAuto;
    ST_STABS_IDX:     Result := dsStabs;
    ST_DWARF2_IDX:    Result := dsDwarf2;
    ST_DWARF2SET_IDX: Result := dsDwarf2Set;
    ST_DWARF3_IDX:    Result := dsDwarf3;
  end;
end;

{ TCompilerLinkingOptionsFrame }

procedure TCompilerLinkingOptionsFrame.chkDebugGDBChange(Sender: TObject);
begin
  grpDebugging.Enabled := chkDebugGDB.Checked;
end;

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

  grpDebug2.Caption := dlgCODebugging2;

  chkDebugGDB.Caption := dlgCOGDB;
  lblDbgSymbolType.Caption := dlgCOSymbolType;
  dropDbgSymbolType.Items.Clear;
  // Adjust constants above, if re-ordering
  dropDbgSymbolType.Items.Add(dlgCOSymbolTypeAuto+' (-g)');                      // 0: automatic
  dropDbgSymbolType.Items.Add(dlgCOSymbolTypeDwarf2Set+ ' (-gw -godwarfsets)');  // 1: dwarf2 + set
  dropDbgSymbolType.Items.Add(dlgCOSymbolTypeDwarf2+ ' (-gw2)');                 // 2: dwarf2
  dropDbgSymbolType.Items.Add(dlgCOSymbolTypeStabs+ ' (-gs)');                   // 3: stabs
  dropDbgSymbolType.Items.Add(dlgCOSymbolTypeDwarf3+ ' (-gw3)');
  chkUseLineInfoUnit.Caption := dlgLNumsBct + ' (-gl)';
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
  if fLoaded then exit;
  fLoaded:=true;
  with AOptions as TBaseCompilerOptions do
  begin
    chkDebugGDB.Checked := GenerateDebugInfo;
    dropDbgSymbolType.ItemIndex := SymbolToIndex(DebugInfoType);
    chkUseLineInfoUnit.Checked := UseLineInfoUnit;
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

  grpDebugging.Enabled := chkDebugGDB.Checked;
end;

procedure TCompilerLinkingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
  with AOptions as TBaseCompilerOptions do
  begin
    GenerateDebugInfo := chkDebugGDB.Checked;
    DebugInfoType := IndexToSymbol(dropDbgSymbolType.ItemIndex);
    UseLineInfoUnit := chkUseLineInfoUnit.Checked;
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
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerLinkingOptionsFrame,
    CompilerOptionsLinking);

end.

