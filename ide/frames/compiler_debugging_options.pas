unit compiler_debugging_options;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, IDEOptionsIntf, CompilerOptions, CompOptsIntf, LazarusIDEStrConsts;

type

  { TCompilerDebuggingOptionsFrame }

  TCompilerDebuggingOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkDebugGDB: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkSymbolsStrip: TCheckBox;
    chkUseExternalDbgSyms: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseValgrind: TCheckBox;
    dropDbgSymbolType: TComboBox;
    grpOtherDebuggingInfo: TGroupBox;
    grpInfoForGDB: TGroupBox;
    lblEmpty: TLabel;
    lblDbgSymbolType: TLabel;
    procedure chkDebugGDBChange(Sender: TObject);
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

{ TCompilerDebuggingOptionsFrame }

procedure TCompilerDebuggingOptionsFrame.chkDebugGDBChange(Sender: TObject);
begin
  grpInfoForGDB.Enabled := chkDebugGDB.Checked;
end;

function TCompilerDebuggingOptionsFrame.GetTitle: string;
begin
  Result := dlgCODebugging;
end;

procedure TCompilerDebuggingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  grpInfoForGDB.Caption := dlgCOInfoForGDB;
  grpOtherDebuggingInfo.Caption := dlgCOOtherDebuggingInfo;

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
end;

procedure TCompilerDebuggingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
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
  end;
  grpInfoForGDB.Enabled := chkDebugGDB.Checked;
end;

procedure TCompilerDebuggingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
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
  end;
end;

class function TCompilerDebuggingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerDebuggingOptionsFrame,
    CompilerOptionsDebugging);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerDebuggingOptionsFrame,
    CompilerOptionsDebugging);

end.

