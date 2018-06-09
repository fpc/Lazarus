unit compiler_debugging_options;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  StdCtrls,
  // LazUtils
  LazLoggerBase,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, CompOptsIntf,
  // IDE
  CompilerOptions, LazarusIDEStrConsts;

type

  { TCompilerDebuggingOptionsFrame }

  TCompilerDebuggingOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkChecksIO: TCheckBox;
    chkChecksOverflow: TCheckBox;
    chkChecksRange: TCheckBox;
    chkChecksStack: TCheckBox;
    chkDebugGDB: TCheckBox;
    chkGenGProfCode: TCheckBox;
    chkSymbolsStrip: TCheckBox;
    chkUseExternalDbgSyms: TCheckBox;
    chkUseHeaptrc: TCheckBox;
    chkTrashVariables: TCheckBox;
    chkUseLineInfoUnit: TCheckBox;
    chkUseValgrind: TCheckBox;
    chkVerifyObjMethodCall: TCheckBox;
    chkAssertion: TCheckBox;
    dropDbgSymbolType: TComboBox;
    grpChecks: TGroupBox;
    grpOtherDebuggingInfo: TGroupBox;
    grpInfoForGDB: TGroupBox;
    lblDbgSymbolType: TLabel;
    procedure chkDebugGDBChange(Sender: TObject);
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
  grpChecks.Caption := dlgCOChecksAndAssertion;
  chkChecksIO.Caption := 'I/O (-Ci)';
  chkChecksRange.Caption := dlgCORange + ' (-Cr)';
  chkChecksOverflow.Caption := dlgCOOverflow + ' (-Co)';
  chkChecksStack.Caption := dlgCOStack + ' (-Ct)';
  chkVerifyObjMethodCall.Caption := lisVerifyMethodCalls + ' (-CR)';
  chkAssertion.Caption := dlgAssertCode + ' (-Sa)';

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
  chkUseValgrind.Caption := dlgCOValgrind + ' (-gv)';
  chkUseExternalDbgSyms.Caption := dlgExtSymb + ' (-Xg)';
  chkUseHeaptrc.Caption := dlgCOHeaptrc + ' (-gh)';
  chkTrashVariables.Caption := dlgCOTrashVariables + ' (-gt)';
  chkGenGProfCode.Caption := dlgGPROF + ' (-pg, '+lisOnly32bit+')';
  chkSymbolsStrip.Caption := dlgCOStrip + ' (-Xs)';
end;

procedure TCompilerDebuggingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    chkChecksIO.Checked := IOChecks;
    chkChecksRange.Checked := RangeChecks;
    chkChecksOverflow.Checked := OverflowChecks;
    chkChecksStack.Checked := StackChecks;
    chkVerifyObjMethodCall.Checked := VerifyObjMethodCall;
    chkAssertion.Checked := IncludeAssertionCode;

    chkDebugGDB.Checked := GenerateDebugInfo;
    dropDbgSymbolType.ItemIndex := SymbolToIndex(DebugInfoType);
    chkUseLineInfoUnit.Checked := UseLineInfoUnit;
    chkUseValgrind.Checked := UseValgrind;
    chkUseExternalDbgSyms.Checked := UseExternalDbgSyms;
    chkUseHeaptrc.Checked := UseHeaptrc;
    chkTrashVariables.Checked := TrashVariables;
    chkGenGProfCode.Checked := GenGProfCode;
    chkSymbolsStrip.Checked := StripSymbols;
    chkSymbolsStrip.Enabled := NeedsLinkerOpts;
  end;
  grpInfoForGDB.Enabled := chkDebugGDB.Checked;
end;

procedure TCompilerDebuggingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TBaseCompilerOptions do
  begin
    IOChecks := chkChecksIO.Checked;
    RangeChecks := chkChecksRange.Checked;
    OverflowChecks := chkChecksOverflow.Checked;
    StackChecks := chkChecksStack.Checked;
    VerifyObjMethodCall := chkVerifyObjMethodCall.Checked;
    IncludeAssertionCode := chkAssertion.Checked;

    GenerateDebugInfo := chkDebugGDB.Checked;
    DebugInfoType := IndexToSymbol(dropDbgSymbolType.ItemIndex);
    UseLineInfoUnit := chkUseLineInfoUnit.Checked;
    UseValgrind := chkUseValgrind.Checked;
    UseExternalDbgSyms := chkUseExternalDbgSyms.Checked;
    UseHeaptrc := chkUseHeaptrc.Checked;
    TrashVariables := chkTrashVariables.Checked;
    GenGProfCode := chkGenGProfCode.Checked;
    StripSymbols := chkSymbolsStrip.Checked;
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

