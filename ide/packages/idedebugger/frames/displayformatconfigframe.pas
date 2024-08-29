unit DisplayFormatConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons, Graphics, Spin,
  LCLType, DividerBevel, IdeDebuggerWatchValueIntf, LazLoggerBase, IdeDebuggerWatchResPrinter,
  IdeDebuggerStringConstants, IdeDebuggerDisplayFormats;

type
  TValueDisplayFormatArray = array of TValueDisplayFormat;
  TValueDisplayFormatHexSeperatorArray = array of TValueDisplayFormatHexSeperator;
  TValueDisplayFormatArrayTypeArray = array of TValueDisplayFormatArrayType;
  TBoolSet = set of boolean;

  TRadioButton = class;

  { TRadioMultiMarker }

  TRadioMultiMarker = class(TPanel)
  private
    FRadio: TRadioButton;
    procedure DoClicked(Sender: TObject);
  protected
    procedure VisibleChanged; override;
    procedure BoundsChanged; override;
  public
    constructor Create(TheOwner: TComponent; TheRadio: TRadioButton); reintroduce;
  end;

  { TRadioButton }

  TRadioButton = class(StdCtrls.TRadioButton)
  private
    FMultiMarker: TRadioMultiMarker;
  protected
    procedure ClearMultiMarkers;
  public
    procedure ShowMultiMarker;
    procedure HideMultiMarker;
  end;

  { TDisplayFormatFrame }

  TDisplayFormatFrame = class(TFrame)
    cbAddrSign: TCheckBox;
    cbAddrNoLeadZero: TCheckBox;
    cbArrayShowPrefix: TCheckBox;
    cbArrayShowPrefixEmbedded: TCheckBox;
    cbEnumValSign: TCheckBox;
    cbMemDump: TCheckBox;
    cbOverrideArray: TCheckBox;
    cbOverrideEnumVal: TCheckBox;
    cbOverrideArrayNavBar: TCheckBox;
    cbOverrideNum2Base: TCheckBox;
    cbOverridePointerDeref: TCheckBox;
    cbOverrideAddressFormat: TCheckBox;
    cbOverrideNumBase: TCheckBox;
    cbOverrideEnum: TCheckBox;
    cbOverrideFloat: TCheckBox;
    cbOverrideIndent: TCheckBox;
    cbOverrideStruct: TCheckBox;
    cbNum2Visibile: TCheckBox;
    cbNumSeparator: TCheckBox;
    cbNum2Separator: TCheckBox;
    cbEnumSign: TCheckBox;
    cbStructAddrTyped: TCheckBox;
    cbPointerAddrTyped: TCheckBox;
    cbArrayNavAutoHide: TCheckBox;
    cbArrayNavEnforceBounds: TCheckBox;
    DigitSpacer3: TLabel;
    DigitSpacer4: TLabel;
    DividerBevelArray: TDividerBevel;
    DividerBevelEnumVal: TDividerBevel;
    DividerBevelNum2: TDividerBevel;
    DividerBevelPointerDeref: TDividerBevel;
    DividerBevelAddressFormat: TDividerBevel;
    DividerBevelNum: TDividerBevel;
    DividerBevelMemDump: TDividerBevel;
    DividerBevelEnum: TDividerBevel;
    DividerBevelFloat: TDividerBevel;
    DividerBevelPointerDeref1: TDividerBevel;
    DividerBevelArrayNavBar: TDividerBevel;
    DividerBevelStruct: TDividerBevel;
    Label1: TLabel;
    lbArrayCombine: TLabel;
    lbOverrideArray: TLabel;
    lbPageSize: TLabel;
    lbMaxWrapLvl: TLabel;
    lbOverrideIndent: TLabel;
    lbOverrideArrayNavBar: TLabel;
    lbArrayLenMaxNest: TLabel;
    lbStructAddrTypedFiller: TLabel;
    Label3: TLabel;
    lbEnumValBaseSpace: TLabel;
    lbOverrideEnumVal: TLabel;
    lpAddrSpace: TLabel;
    lbEnumBaseSpace: TLabel;
    lbNum2Digits: TLabel;
    lbOverrideFloat: TLabel;
    lbOverridePointerDeref: TLabel;
    lbOverrideStruct: TLabel;
    lbOverrideNum2Base: TLabel;
    lbOverrideEnum: TLabel;
    lbNumSepGroup: TLabel;
    lbNumDigits: TLabel;
    lbOverrideAddressFormat: TLabel;
    lbOverrideNumBase: TLabel;
    lbNum2SepGroup: TLabel;
    PanelAddressLeadZero: TPanel;
    PanelArray: TPanel;
    PanelArrayShowPrefix: TPanel;
    PanelEnumVal: TPanel;
    PanelENumValBase: TPanel;
    PanelEnumValRb: TPanel;
    PanelNum2All: TPanel;
    PanelNum2Digits: TPanel;
    PanelENumBase: TPanel;
    PanelNumSepGroup: TPanel;
    PanelNum2: TPanel;
    PanelNum2Base: TPanel;
    PanelNum2Visible: TPanel;
    PanelNum2Sign: TPanel;
    PanelNum2SepGroup: TPanel;
    PanelIndent: TPanel;
    PanelIndentMax: TPanel;
    PanelArrayNavBar: TPanel;
    PanelArrayNavBarOpts: TPanel;
    PanelArrayPrefixCombine: TPanel;
    rbClear: TRadioButton;
    rbClear1: TRadioButton;
    rbClear10: TRadioButton;
    rbClear11: TRadioButton;
    rbClear12: TRadioButton;
    rbClear13: TRadioButton;
    rbClear14: TRadioButton;
    rbClear15: TRadioButton;
    rbClear16: TRadioButton;
    rbClear17: TRadioButton;
    rbClear2: TRadioButton;
    rbClear3: TRadioButton;
    rbClear4: TRadioButton;
    rbClear5: TRadioButton;
    rbClear6: TRadioButton;
    rbClear7: TRadioButton;
    rbClear8: TRadioButton;
    rbClear9: TRadioButton;
    rbENumValBin: TRadioButton;
    rbENumValChar: TRadioButton;
    rbENumValDec: TRadioButton;
    rbENumValHex: TRadioButton;
    rbEnumValName: TRadioButton;
    rbEnumValNameAndOrd: TRadioButton;
    rbENumValOct: TRadioButton;
    rbEnumValOrd: TRadioButton;
    rbNum2SepByte: TRadioButton;
    rbNum2SepLong: TRadioButton;
    rbENumBin: TRadioButton;
    rbENumChar: TRadioButton;
    rbENumDec: TRadioButton;
    rbENumHex: TRadioButton;
    rbENumOct: TRadioButton;
    rbAddrNumBin: TRadioButton;
    rbNumSepNone: TRadioButton;
    rbNumSepByte: TRadioButton;
    rbNum2Char: TRadioButton;
    rbNum2Bin: TRadioButton;
    rbNum2Dec: TRadioButton;
    rbNum2Hex: TRadioButton;
    rbNum2Oct: TRadioButton;
    rbNum2SepNone: TRadioButton;
    rbNumSepWord: TRadioButton;
    rbNumSepLong: TRadioButton;
    rbNum2SepWord: TRadioButton;
    rbSign2Auto: TRadioButton;
    rbSign2Signed: TRadioButton;
    rbSign2Unsigned: TRadioButton;
    rbArrayCombineNone: TRadioButton;
    rbArrayCombineAll: TRadioButton;
    rbArrayCombineStat: TRadioButton;
    rbArrayCombineDyn: TRadioButton;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape18: TShape;
    Spacer1: TLabel;
    lbFloatPrec: TLabel;
    Label5: TLabel;
    PanelAddressFormat: TPanel;
    PanelMemDump: TPanel;
    PanelPointerDeref: TPanel;
    PanelFloat: TPanel;
    PanelFloatRb: TPanel;
    PanelNumBase: TPanel;
    PanelNumSign: TPanel;
    PanelNumDigits: TPanel;
    PanelNum: TPanel;
    PanelAddressBase: TPanel;
    PanelStruct: TPanel;
    PanelEnum: TPanel;
    PanelPointer: TPanel;
    PanelStructFld: TPanel;
    PanelEnumRb1: TPanel;
    PanelAddressType: TPanel;
    PanelStructPointer: TPanel;
    rbPointerDerefOff: TRadioButton;
    rbAddrNumHex: TRadioButton;
    rbAddrNumDec: TRadioButton;
    rbPointerDerefOnly: TRadioButton;
    rbPointerDerefOn: TRadioButton;
    rbFloatPoint: TRadioButton;
    rbFloatScience: TRadioButton;
    rbNumBin: TRadioButton;
    rbNumDec: TRadioButton;
    rbNumHex: TRadioButton;
    rbNumOct: TRadioButton;
    rbNumChar: TRadioButton;
    rbAddrNumOct: TRadioButton;
    rbSignAuto: TRadioButton;
    rbSignSigned: TRadioButton;
    rbSignUnsigned: TRadioButton;
    rbStructFields: TRadioButton;
    rbEnumOrd: TRadioButton;
    rbAddressTyped: TRadioButton;
    rbStructAddrOn: TRadioButton;
    rbStructFull: TRadioButton;
    rbEnumNameAndOrd: TRadioButton;
    rbStructAddrOnly: TRadioButton;
    rbStructValOnly: TRadioButton;
    rbEnumName: TRadioButton;
    rbAddressPlain: TRadioButton;
    rbStructAddrOff: TRadioButton;
    Spacer10: TLabel;
    Spacer11: TLabel;
    Spacer12: TLabel;
    Spacer13: TLabel;
    Spacer14: TLabel;
    DigitSpacer1: TLabel;
    DigitSpacer2: TLabel;
    Spacer15: TLabel;
    Spacer16: TLabel;
    Spacer17: TLabel;
    Spacer18: TLabel;
    Spacer19: TLabel;
    Spacer2: TLabel;
    Spacer20: TLabel;
    Spacer21: TLabel;
    Spacer22: TLabel;
    Spacer3: TLabel;
    Spacer4: TLabel;
    Spacer5: TLabel;
    Spacer6: TLabel;
    Spacer7: TLabel;
    Spacer8: TLabel;
    Spacer9: TLabel;
    SpinPageSize: TSpinEdit;
    SpinIndentMaxWrap: TSpinEdit;
    SpinFloatDigits: TSpinEdit;
    SpinDigits: TSpinEdit;
    Spin2Digits: TSpinEdit;
    spinArrayLenMaxNest: TSpinEdit;
    tbIndent: TSpeedButton;
    ToolBar1: TToolBar;
    tbCurrent: TSpeedButton;
    tbStruct: TSpeedButton;
    tbPointer: TSpeedButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton2: TToolButton;
    tbAll: TSpeedButton;
    ToolButton4: TToolButton;
    tbNumber: TSpeedButton;
    tbEnum: TSpeedButton;
    tbBool: TSpeedButton;
    tbChar: TSpeedButton;
    tbFloat: TSpeedButton;
    tbArray: TSpeedButton;
    ToolButton5: TToolButton;
    procedure cbArrayShowPrefixChange(Sender: TObject);
    procedure DataCheckboxChange(Sender: TObject);
    procedure cbMemDumpChange(Sender: TObject);
    procedure CheckLabelClicked(Sender: TObject);
    procedure FormatAddrTypeChanged(Sender: TObject);
    procedure FormatNumGroupChanged(Sender: TObject);
    procedure FormatNumSepChanged(Sender: TObject);
    procedure FormatRadioChanged(Sender: TObject);
    procedure FormatSignCBChanged(Sender: TObject);
    procedure FormatSpinChanged(Sender: TObject);
    procedure OverrideCheckChanged(Sender: TObject);
    procedure Spin2DigitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Spin2DigitsKeyPress(Sender: TObject; var Key: char);
    procedure tbAllClick(Sender: TObject);
    procedure tbCurrentClick(Sender: TObject);
    procedure tbFormatClick(Sender: TObject);
    procedure tbIndentClick(Sender: TObject);
  private type
    TFmtButtons = (bsNum, bsEnum, bsBool, bsChar, bsFloat, bsStruct, bsPtr, bsArray, bsIndent);
  private
    FAllowMultiTabs: boolean;
    FHighlightModifiedTabs: boolean;
    FShowAll: boolean;
    FShowArrayNavBarOpts: boolean;
    FShowCurrent: boolean;

    FDisplayFormatCount: integer;
    FDisplayFormat: array of TWatchDisplayFormat;
    FCurrentResDataType: TWatchResultDataKind;
    FShowExtraSettings: boolean;
    FShowFullAddressInMulti: boolean;
    FShowMemDump: boolean;
    FShowMultiRadio: boolean;
    FShowOverrideChecks: boolean;

    FUpdateCount: integer;
    FNeedUpdateDisplay, FUpdatingSpin: boolean;
    FUpdatingDisplay: Integer;
    FInButtonClick: Boolean;
    FButtonStates: array[TFmtButtons] of boolean;

    procedure EnableParentOverride(c: TControl; AEnableVisible: Boolean = False);
    function GetDisplayFormat: TWatchDisplayFormat;
    function GetDisplayFormats(AIndex: integer): TWatchDisplayFormat;
    procedure SetCurrentResDataType(AValue: TWatchResultDataKind);
    procedure SetDisplayFormat(ADispFormat: TWatchDisplayFormat);
    procedure SetDisplayFormatCount(AValue: integer);
    procedure SetDisplayFormats(AIndex: integer; AValue: TWatchDisplayFormat);
    procedure SetHighlightModifiedTabs(AValue: boolean);
    procedure SetShowAll(AValue: boolean);
    procedure SetShowArrayNavBarOpts(AValue: boolean);
    procedure SetShowCurrent(AValue: boolean);
    procedure SetShowExtraSettings(AValue: boolean);
    procedure SetShowMemDump(AValue: boolean);
    procedure SetShowMultiRadio(AValue: boolean);
    procedure SetShowOverrideChecks(AValue: boolean);
  protected
    procedure ClearRadios(APanel: TWinControl; ARecursive: Boolean = False; AnExcludeSelf: boolean = False);
    procedure ApplyDispForm(APanel: TPanel; ADispForm: TValueDisplayFormats; AnRbOrder: TValueDisplayFormatArray);
    procedure ApplyDispForm(APanel: TPanel; ADispForm: TValueDisplayFormatHexSeperators; AnRbOrder: TValueDisplayFormatHexSeperatorArray);
    procedure ApplyDispForm(APanel: TPanel; ADispForm: TValueDisplayFormatArrayTypes; AnRbOrder: TValueDisplayFormatArrayTypeArray);
    function ReadDispForm(APanel: TPanel; AnRbOrder: TValueDisplayFormatArray): TValueDisplayFormats;
    function ReadDispForm(APanel: TPanel; AnRbOrder: TValueDisplayFormatHexSeperatorArray): TValueDisplayFormatHexSeperators;
    function ReadDispForm(APanel: TPanel; AnRbOrder: TValueDisplayFormatArrayTypeArray): TValueDisplayFormatArrayTypes;
    function BtnDownCount: integer;

    procedure UpdateButtonStates;
    procedure UpdateTabs;
    procedure UpdateVisiblePanels;
    procedure UpdateNumDigitPanel;
    procedure UpdateNum2Visibility;
    procedure UpdateNum2DigitPanel;
    procedure UpdateDisplay;
    procedure UpdateFormatOverrides;
    procedure UpdateFormat;
    procedure CreateHandle; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Setup;
    procedure BeginUdpate;
    procedure EndUdpate;
    procedure SelectDefaultButton;
    property DisplayFormat: TWatchDisplayFormat read GetDisplayFormat write SetDisplayFormat;
    property DisplayFormats [AIndex: integer]: TWatchDisplayFormat read GetDisplayFormats write SetDisplayFormats;
    property DisplayFormatCount: integer read FDisplayFormatCount write SetDisplayFormatCount;
    property CurrentResDataType: TWatchResultDataKind read FCurrentResDataType write SetCurrentResDataType;
  published
    property ShowCurrent: boolean read FShowCurrent write SetShowCurrent;
    property ShowAll: boolean read FShowAll write SetShowAll;
    property AllowMultiTabs: boolean read FAllowMultiTabs write FAllowMultiTabs;
    property ShowMemDump: boolean read FShowMemDump write SetShowMemDump;
    property ShowMultiRadio: boolean read FShowMultiRadio write SetShowMultiRadio;
    property ShowOverrideChecks: boolean read FShowOverrideChecks write SetShowOverrideChecks;
    property ShowArrayNavBarOpts: boolean read FShowArrayNavBarOpts write SetShowArrayNavBarOpts;
    // ShowExtraSettings: Show PanelEnumVal  (project and global opts)
    property ShowExtraSettings: boolean read FShowExtraSettings write SetShowExtraSettings;
    property ShowFullAddressInMulti: boolean read FShowFullAddressInMulti write FShowFullAddressInMulti;
    property HighlightModifiedTabs: boolean read FHighlightModifiedTabs write SetHighlightModifiedTabs;
  end;

implementation

{$R *.lfm}

const
  {$WriteableConst off}
  RBA_Num: TValueDisplayFormatArray = (vdfBaseDecimal, vdfBaseHex, vdfBaseOct, vdfBaseBin, vdfBaseChar);
  RBA_Sign: TValueDisplayFormatArray = (vdfSignAuto, vdfSignSigned, vdfSignUnsigned);
  RBA_Group: TValueDisplayFormatHexSeperatorArray = (vdfhsNone, vdfhsByte, vdfhsWord, vdfhsLong);
  RBA_Enum: TValueDisplayFormatArray = (vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd);
  RBA_Float: TValueDisplayFormatArray = (vdfFloatPoint, vdfFloatScientific);
  RBA_StructVal: TValueDisplayFormatArray = (vdfStructValOnly, vdfStructFields, vdfStructFull);
  RBA_StructPtr: TValueDisplayFormatArray = (vdfStructPointerOff, vdfStructPointerOn, vdfStructPointerOnly);
  RBA_PtrDeref: TValueDisplayFormatArray = (vdfPointerDerefOn, vdfPointerDerefOff, vdfPointerDerefOnly);
  RBA_Addr: TValueDisplayFormatArray = (vdfAddressPlain, vdfAddressTyped);
  RBA_AddrNum: TValueDisplayFormatArray = (vdfBaseHex, vdfBaseDecimal, vdfBaseOct, vdfBaseBin);
  RBA_ArrayCombine: TValueDisplayFormatArrayTypeArray = (vdfatNone, vdfatAll, vdfatStat, vdfatDyn);

  INT_MIX = low(integer);
  INT_UNK = low(integer)+1;

procedure UpdateIntSetting(var CurVal: Integer; NewVal: integer);
begin
  if (CurVal = INT_UNK) or (CurVal = NewVal)
  then CurVal := NewVal
  else CurVal := INT_MIX;
end;

procedure SetSpinEditToInherit(ASpin: TSpinEdit);
begin
  ASpin.Tag   := 1;
  ASpin.Value := 0;
  ASpin.Text  := '';
end;

procedure IntToSpinEdit(ASpin: TSpinEdit; aVal: integer);
begin
  if (aVal = INT_MIX) or (aVal = INT_UNK) then begin
    ASpin.Tag      := 2;
    ASpin.Value    := 0;
    ASpin.Text     := '';
  end
  else begin
    ASpin.Tag      := 0;
    ASpin.Value    := aVal;
    ASpin.Text     := IntToStr(aVal);
  end;
end;

function BoolsetToCBState(bs: TBoolSet; ARevert: Boolean = True): TCheckBoxState;
begin
  if bs =[False, True] then
    Result := cbGrayed
  else
  if (bs =[False]) xor ARevert then
    Result := cbUnchecked
  else
    Result := cbChecked;
end;

procedure BoolFromCBState(cs: TCheckBoxState; var b: boolean; ARevert: Boolean = True);
begin
  case cs of
    cbUnchecked: b := ARevert;
    cbChecked:   b := not ARevert;
    //cbGrayed: ;
  end;
end;

procedure BoolFromCB(cb: TCheckBox; var b: boolean; ARevert: Boolean = True);
begin
  if cb.Visible and cb.Enabled and (cb.Tag = 0) then
    BoolFromCBState(cb.State, b, ARevert);
end;

function HexSepCount(hs: TValueDisplayFormatHexSeperators): integer;
var
  d: TValueDisplayFormatHexSeperator;
begin
  Result := 0;
  for d in TValueDisplayFormatHexSeperators do
    if d in hs then inc(Result);
end;

function LenCombineCount(hs: TValueDisplayFormatArrayTypes): integer;
var
  d: TValueDisplayFormatArrayType;
begin
  Result := 0;
  for d in TValueDisplayFormatArrayTypes do
    if d in hs then inc(Result);
end;

procedure AddToStr(var txt: string; add, sep: string);
begin
  if (txt = add) then
    exit;
  if txt <> '' then txt := txt + sep;
  txt := txt + add;
end;

{ TRadioMultiMarker }

procedure TRadioMultiMarker.DoClicked(Sender: TObject);
begin
  FRadio.Checked := True;
end;

procedure TRadioMultiMarker.VisibleChanged;
begin
  inherited VisibleChanged;
  if IsVisible then BringToFront;
end;

procedure TRadioMultiMarker.BoundsChanged;
begin
  inherited BoundsChanged;
  if Parent = nil then
    exit;
  BorderSpacing.Left := max(0, (Parent.Height-5) div 2 - 2);
  //Height := max(1, min(3, (Parent.Height + 3) div 6));
  //Width := Height;
  Height := max(1, min(3, (Parent.Height) div 12));
  Width :=  max(3, min(15, (Parent.Height) div 3));
  BorderSpacing.Left := Max(2, (Parent.Height - Width) div 2 - 2);
end;

constructor TRadioMultiMarker.Create(TheOwner: TComponent; TheRadio: TRadioButton);
begin
  inherited Create(TheOwner);
  FRadio := TheRadio;
  Parent := TheRadio.Parent;
  AutoSize := False;
  Visible := False;
  Height := 3;
  Width := 3;
  Color := clGrayText;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  AnchorSide[akTop].Control  := TheRadio;
  AnchorSide[akTop].Side     := asrCenter;
  AnchorSide[akLeft].Control := TheRadio;
  AnchorSide[akLeft].Side    := asrTop;
  BorderSpacing.Left := 5;
  OnClick := @DoClicked;
end;

{ TRadioButton }

procedure TRadioButton.ClearMultiMarkers;
var
  i: Integer;
begin
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] is TRadioButton then
      TRadioButton(Parent.Controls[i]).HideMultiMarker;
end;

procedure TRadioButton.ShowMultiMarker;
begin
  if FMultiMarker = nil then
    FMultiMarker := TRadioMultiMarker.Create(Owner, Self);
  FMultiMarker.Visible := True;
end;

procedure TRadioButton.HideMultiMarker;
begin
  if FMultiMarker <> nil then
    FMultiMarker.Visible := False;
end;

{ TDisplayFormatFrame }

procedure TDisplayFormatFrame.cbMemDumpChange(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;
  TControl(Sender).Tag := 0;
  PanelNum.Enabled    := not cbMemDump.Checked;
  PanelEnum.Enabled := not cbMemDump.Checked;
  PanelFloat.Enabled := not cbMemDump.Checked;
  PanelStruct.Enabled := not cbMemDump.Checked;
  PanelPointer.Enabled := not cbMemDump.Checked;
  PanelAddressFormat.Enabled := not cbMemDump.Checked;
end;

procedure TDisplayFormatFrame.DataCheckboxChange(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;
  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender));
  UpdateFormat;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.cbArrayShowPrefixChange(Sender: TObject);
begin
  cbArrayShowPrefixEmbedded.Enabled := cbArrayShowPrefix.Checked;
  DataCheckboxChange(Sender);
end;

procedure TDisplayFormatFrame.CheckLabelClicked(Sender: TObject);
var
  i: Integer;
  p: TWinControl;
begin
  p := TControl(Sender).Parent;
  for i := 0 to p.ControlCount-1 do
    if p.Controls[i] is TCheckBox then begin
      TCheckBox(p.Controls[i]).Checked := not TCheckBox(p.Controls[i]).Checked;
      exit;
    end;
end;

procedure TDisplayFormatFrame.FormatAddrTypeChanged(Sender: TObject);
begin
  // "ALL" page only. On the "all" page this does not have an "override" checkbox.
  if FUpdatingDisplay > 0 then
    exit;
  TControl(Sender).Tag := 0;
  UpdateFormat;
end;

procedure TDisplayFormatFrame.FormatNumGroupChanged(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;
  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender));
  UpdateFormat;
  TRadioButton(Sender).ClearMultiMarkers;
end;

procedure TDisplayFormatFrame.FormatNumSepChanged(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;
  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender));
  UpdateFormat;
end;

procedure TDisplayFormatFrame.FormatRadioChanged(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;

  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender), True);

  UpdateFormat;
  TRadioButton(Sender).ClearMultiMarkers;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.FormatSignCBChanged(Sender: TObject);
begin
  if FUpdatingDisplay > 0 then
    exit;

  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender), True);

  UpdateFormat;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.FormatSpinChanged(Sender: TObject);
begin
  if FUpdatingSpin then
    exit;
  if FUpdatingDisplay = 0 then
    TControl(Sender).Tag := 0;

  if (TSpinEdit(Sender).Tag <> 0) then begin
    FUpdatingSpin := True;
    TSpinEdit(Sender).Text := '';
    FUpdatingSpin := False;
  end;
  if (TSpinEdit(Sender).Tag = 0) and (TSpinEdit(Sender).Value = -1) and (Sender <> SpinFloatDigits)
  then begin
    FUpdatingSpin := True;
    TSpinEdit(Sender).Text := DispFormatNumDigitsFull;
    TSpinEdit(Sender).SelectAll;
    FUpdatingSpin := False;
  end;

  if FUpdatingDisplay > 0 then
    exit;

  TControl(Sender).Tag := 0;
  EnableParentOverride(TControl(Sender), True);
  UpdateFormat;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.OverrideCheckChanged(Sender: TObject);
begin
  if (FUpdatingDisplay > 0) or not FShowOverrideChecks then
    exit;
  TControl(Sender).Tag := 0;
  UpdateFormatOverrides;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.Spin2DigitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (TSpinEdit(Sender).Value = -1) then begin
    if(Key = VK_DOWN) then
      key := 0;
    if (key >= VK_0) and (key <= VK_9) then begin
      TSpinEdit(Sender).Value := Key - VK_0;
      key := 0;
    end;
  end;
end;

procedure TDisplayFormatFrame.Spin2DigitsKeyPress(Sender: TObject; var Key: char);
begin
  TControl(Sender).Tag := 0;
  if Key = '-' then begin
    if (TSpinEdit(Sender).MinValue < 0) and (Sender <> SpinFloatDigits) then
      TSpinEdit(Sender).Value := -1;
    Key := #0;
  end;
end;

procedure TDisplayFormatFrame.tbAllClick(Sender: TObject);
begin
  if FInButtonClick then
    exit;
  FInButtonClick := True;
  try
    UpdateFormat;
    tbCurrent.Down := tbCurrent.Enabled;
    tbNumber.Down  := tbAll.Down;
    tbEnum.Down    := tbAll.Down;
    tbBool.Down    := tbAll.Down;
    tbChar.Down    := tbAll.Down;
    tbFloat.Down   := tbAll.Down;
    tbStruct.Down  := tbAll.Down;
    tbPointer.Down := tbAll.Down;
    tbArray.Down   := tbAll.Down;
    tbIndent.Down  := False;
  finally
    FInButtonClick := False;
  end;

  if (not tbAll.Down) and (Sender <> nil) then begin
    tbCurrentClick(nil);
  end
  else
    UpdateDisplay;
end;

procedure TDisplayFormatFrame.tbCurrentClick(Sender: TObject);
begin
  if FInButtonClick then
    exit;

  if FCurrentResDataType = rdkUnknown then begin
    FInButtonClick := True;
    if FShowAll then begin
      tbAll.Down := True;
      FInButtonClick := False;
      tbAllClick(nil);
    end
    else begin
      tbNumber.Down := True;
      FInButtonClick := False;
      tbFormatClick(tbNumber);
    end;
    exit;
  end;

  FInButtonClick := True;
  try
    UpdateFormat;
    tbAll.Down     := False;
    tbCurrent.Down := True;
    tbNumber.Down  := False;
    tbEnum.Down    := False;
    tbBool.Down    := False;
    tbChar.Down    := False;
    tbFloat.Down   := False;
    tbStruct.Down  := False;
    tbPointer.Down := False;
    tbArray.Down   := False;
    tbIndent.Down  := False;
    case FCurrentResDataType of
      rdkError: ;
      rdkPrePrinted: ;
      rdkString: ;
      rdkWideString: ;
      rdkChar:           tbChar.Down := True;
      rdkSignedNumVal:   tbNumber.Down := True;
      rdkUnsignedNumVal: tbNumber.Down := True;
      rdkPointerVal:     tbPointer.Down := True;
      rdkFloatVal:       tbFloat.Down := True;
      rdkBool:           tbBool.Down := True;
      rdkEnum:           tbEnum.Down := True;
      rdkEnumVal:        tbEnum.Down := True;
      rdkSet:            tbEnum.Down := True;
      rdkVariant: ;
      rdkPCharOrString: ;
      rdkArray:          tbArray.Down := True;
      rdkStruct:         tbStruct.Down := True;
      rdkConvertRes: ;
      rdkFunction: ;
      rdkProcedure: ;
      rdkFunctionRef: ;
      rdkProcedureRef: ;
    end;

  finally
    FInButtonClick := False;
  end;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.tbFormatClick(Sender: TObject);
begin
  if FInButtonClick then
    exit;
  FInButtonClick := True;
  try
    UpdateFormat;
    tbIndent.Down  := False;
    if (not(ssShift in GetKeyShiftState)) or
       (not FAllowMultiTabs) or
       (not(tbNumber.Down or tbEnum.Down or tbBool.Down or tbChar.Down or tbFloat.Down or tbStruct.Down or tbPointer.Down or tbArray.Down))
    then begin
      tbNumber.Down  := tbNumber  = Sender;
      tbEnum.Down    := tbEnum    = Sender;
      tbBool.Down    := tbBool    = Sender;
      tbChar.Down    := tbChar    = Sender;
      tbFloat.Down   := tbFloat   = Sender;
      tbStruct.Down  := tbStruct  = Sender;
      tbPointer.Down := tbPointer = Sender;
      tbArray.Down   := tbArray   = Sender;
    end;
    case FCurrentResDataType of
      rdkChar:           tbCurrent.Down := tbChar.Down;
      rdkSignedNumVal:   tbCurrent.Down := tbNumber.Down;
      rdkUnsignedNumVal: tbCurrent.Down := tbNumber.Down;
      rdkPointerVal:     tbCurrent.Down := tbPointer.Down;
      rdkFloatVal:       tbCurrent.Down := tbFloat.Down;
      rdkBool:           tbCurrent.Down := tbBool.Down;
      rdkEnum:           tbCurrent.Down := tbEnum.Down;
      rdkEnumVal:        tbCurrent.Down := tbEnum.Down;
      rdkSet:            tbCurrent.Down := tbEnum.Down;
      rdkStruct:         tbCurrent.Down := tbStruct.Down;
      rdkArray:          tbCurrent.Down := tbArray.Down;
      else tbCurrent.Down := False;
    end;
    if FShowAll then
      tbAll.Down := tbNumber.Down and tbEnum.Down and tbBool.Down and tbChar.Down and
                    tbFloat.Down and tbStruct.Down and tbPointer.Down and tbArray.Down;
  finally
    FInButtonClick := False;
  end;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.tbIndentClick(Sender: TObject);
begin
  if FInButtonClick then
    exit;
  FInButtonClick := True;
  try
    UpdateFormat;
    tbIndent.Down  := True;
    tbCurrent.Down := False;
    tbAll.Down     := False;
    tbNumber.Down  := False;
    tbEnum.Down    := False;
    tbBool.Down    := False;
    tbChar.Down    := False;
    tbFloat.Down   := False;
    tbStruct.Down  := False;
    tbPointer.Down := False;
    tbArray.Down   := False;
  finally
    FInButtonClick := False;
  end;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.EnableParentOverride(c: TControl; AEnableVisible: Boolean);
var
  pnl: TWinControl;
  i: Integer;
begin
  if not ShowOverrideChecks then
    exit;

  inc(FUpdatingDisplay);
  try
    pnl := c.Parent;
    while (pnl.Parent <> self) do
      pnl := pnl.Parent;
    if (pnl = PanelNum2) and AEnableVisible then begin
      cbNum2Visibile.Tag := 0;
      cbNum2Visibile.Checked := True;
    end;
    for i := 0 to pnl.ControlCount-1 do
      if pnl.Controls[i] is TCheckBox then begin
        if TCheckBox(pnl.Controls[i]).State = cbUnchecked then
          TCheckBox(pnl.Controls[i]).Checked := True;
        exit;
      end;
  finally
    dec(FUpdatingDisplay);
  end;
end;

function TDisplayFormatFrame.GetDisplayFormat: TWatchDisplayFormat;
begin
  UpdateFormat;
  Result := FDisplayFormat[0];
end;

function TDisplayFormatFrame.GetDisplayFormats(AIndex: integer): TWatchDisplayFormat;
begin
  UpdateFormat;
  Result := FDisplayFormat[AIndex];
end;

procedure TDisplayFormatFrame.SetCurrentResDataType(AValue: TWatchResultDataKind);
begin
  if FCurrentResDataType = AValue then Exit;
  FCurrentResDataType := AValue;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.SetDisplayFormat(ADispFormat: TWatchDisplayFormat);
begin
  FDisplayFormat[0] := ADispFormat;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.SetDisplayFormatCount(AValue: integer);
var
  l, i: integer;
begin
  if AValue < 1 then
    AValue := 1;
  if FDisplayFormatCount = AValue then
    Exit;
  FDisplayFormatCount := AValue;
  l := Length(FDisplayFormat);
  SetLength(FDisplayFormat, AValue);
  for i := l to AValue - 1 do
    FDisplayFormat[i] := DefaultWatchDisplayFormat;

  UpdateDisplay;
end;

procedure TDisplayFormatFrame.SetDisplayFormats(AIndex: integer; AValue: TWatchDisplayFormat);
begin
  FDisplayFormat[AIndex] := AValue;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.SetHighlightModifiedTabs(AValue: boolean);
begin
  if FHighlightModifiedTabs = AValue then Exit;
  FHighlightModifiedTabs := AValue;
  if not FHighlightModifiedTabs then
    UpdateTabs;
end;

procedure TDisplayFormatFrame.SetShowAll(AValue: boolean);
begin
  if FShowAll = AValue then Exit;
  FShowAll := AValue;
  tbAll.Visible := AValue;
  ToolButton4.Visible := FShowCurrent;
end;

procedure TDisplayFormatFrame.SetShowArrayNavBarOpts(AValue: boolean);
begin
  if FShowArrayNavBarOpts = AValue then Exit;
  FShowArrayNavBarOpts := AValue;
end;

procedure TDisplayFormatFrame.SetShowCurrent(AValue: boolean);
begin
  if FShowCurrent = AValue then Exit;
  FShowCurrent := AValue;
  tbCurrent.Visible := FShowCurrent;
  ToolButton2.Visible := FShowCurrent;
end;

procedure TDisplayFormatFrame.SetShowExtraSettings(AValue: boolean);
begin
  if FShowExtraSettings = AValue then Exit;
  FShowExtraSettings := AValue;
end;

procedure TDisplayFormatFrame.SetShowMemDump(AValue: boolean);
begin
  if FShowMemDump = AValue then Exit;
  FShowMemDump := AValue;
  PanelMemDump.Visible := FShowMemDump;
end;

procedure TDisplayFormatFrame.SetShowMultiRadio(AValue: boolean);
begin
  if FShowMultiRadio = AValue then Exit;
  FShowMultiRadio := AValue;
end;

procedure TDisplayFormatFrame.SetShowOverrideChecks(AValue: boolean);
begin
  if FShowOverrideChecks = AValue then Exit;
  FShowOverrideChecks := AValue;
  cbOverrideNumBase.Visible       := FShowOverrideChecks;
  cbOverrideNum2Base.Visible      := FShowOverrideChecks;
  cbOverrideEnum.Visible          := FShowOverrideChecks;
  cbOverrideEnumVal.Visible          := FShowOverrideChecks;
  cbOverrideFloat.Visible         := FShowOverrideChecks;
  cbOverrideStruct.Visible        := FShowOverrideChecks;
  cbOverridePointerDeref.Visible  := FShowOverrideChecks;
  cbOverrideAddressFormat.Visible := FShowOverrideChecks;
  cbOverrideIndent.Visible        := FShowOverrideChecks;
  cbOverrideArrayNavBar.Visible   := FShowOverrideChecks;
end;

procedure TDisplayFormatFrame.ClearRadios(APanel: TWinControl; ARecursive: Boolean;
  AnExcludeSelf: boolean);
var
  i: Integer;
begin
  for i := APanel.ControlCount - 1 downto 0 do begin
    if (APanel.Controls[i] is TRadioButton) and not AnExcludeSelf and
       (copy(APanel.Controls[i].Name, 1, 7) = 'rbClear')
    then
      TRadioButton(APanel.Controls[i]).Checked := True
    else
    if (APanel.Controls[i] is TRadioButton) and not AnExcludeSelf then begin
      TRadioButton(APanel.Controls[i]).Checked := False;
      TRadioButton(APanel.Controls[i]).HideMultiMarker;
    end;
    if ARecursive then begin
      if (APanel.Controls[i] is TCheckBox) and not AnExcludeSelf then
        TCheckBox(APanel.Controls[i]).Checked := False;
      if APanel.Controls[i] is TWinControl then
        ClearRadios(TWinControl(APanel.Controls[i]), True);
    end;
  end;
end;

procedure TDisplayFormatFrame.ApplyDispForm(APanel: TPanel; ADispForm: TValueDisplayFormats;
  AnRbOrder: TValueDisplayFormatArray);
var
  i, j: Integer;
begin
  ClearRadios(APanel);

  if IdeDebuggerDisplayFormats.DisplayFormatCount(ADispForm) = 1 then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).Checked := True;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end
  else
  if FShowMultiRadio then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).ShowMultiMarker
      else
        TRadioButton(APanel.Controls[i]).HideMultiMarker;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end;
end;

procedure TDisplayFormatFrame.ApplyDispForm(APanel: TPanel;
  ADispForm: TValueDisplayFormatHexSeperators; AnRbOrder: TValueDisplayFormatHexSeperatorArray);
var
  i, j: Integer;
begin
  ClearRadios(APanel);

  if HexSepCount(ADispForm) = 1 then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).Checked := True;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end
  else
  if FShowMultiRadio then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).ShowMultiMarker
      else
        TRadioButton(APanel.Controls[i]).HideMultiMarker;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end;
end;

procedure TDisplayFormatFrame.ApplyDispForm(APanel: TPanel;
  ADispForm: TValueDisplayFormatArrayTypes; AnRbOrder: TValueDisplayFormatArrayTypeArray);
var
  i, j: Integer;
begin
  ClearRadios(APanel);

  if LenCombineCount(ADispForm) = 1 then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).Checked := True;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end
  else
  if FShowMultiRadio then begin
    j := 0;
    for i := 0 to APanel.ControlCount - 1 do begin
      if not (APanel.Controls[i] is TRadioButton) then
        continue;
      if AnRbOrder[j] in ADispForm then
        TRadioButton(APanel.Controls[i]).ShowMultiMarker
      else
        TRadioButton(APanel.Controls[i]).HideMultiMarker;
      inc(j);
      if j >= Length(AnRbOrder) then break;
    end;
  end;
end;

function TDisplayFormatFrame.ReadDispForm(APanel: TPanel; AnRbOrder: TValueDisplayFormatArray): TValueDisplayFormats;
var
  i, j: Integer;
begin
  Result := [];
  j := 0;
  for i := 0 to APanel.ControlCount - 1 do begin
    if not (APanel.Controls[i] is TRadioButton) then
      continue;
    if TRadioButton(APanel.Controls[i]).Checked then
      include(Result, AnRbOrder[j]);
    inc(j);
    if j >= Length(AnRbOrder) then break;
  end
end;

function TDisplayFormatFrame.ReadDispForm(APanel: TPanel; AnRbOrder: TValueDisplayFormatHexSeperatorArray): TValueDisplayFormatHexSeperators;
var
  i, j: Integer;
begin
  Result := [];
  j := 0;
  for i := 0 to APanel.ControlCount - 1 do begin
    if not (APanel.Controls[i] is TRadioButton) then
      continue;
    if TRadioButton(APanel.Controls[i]).Checked then
      include(Result, AnRbOrder[j]);
    inc(j);
    if j >= Length(AnRbOrder) then break;
  end
end;

function TDisplayFormatFrame.ReadDispForm(APanel: TPanel;
  AnRbOrder: TValueDisplayFormatArrayTypeArray): TValueDisplayFormatArrayTypes;
var
  i, j: Integer;
begin
  Result := [];
  j := 0;
  for i := 0 to APanel.ControlCount - 1 do begin
    if not (APanel.Controls[i] is TRadioButton) then
      continue;
    if TRadioButton(APanel.Controls[i]).Checked then
      include(Result, AnRbOrder[j]);
    inc(j);
    if j >= Length(AnRbOrder) then break;
  end
end;

function TDisplayFormatFrame.BtnDownCount: integer;
var
  i: TFmtButtons;
begin
  Result := 0;
  for i := low(TFmtButtons) to high(TFmtButtons) do
    if FButtonStates[i] then inc(Result);
end;

procedure TDisplayFormatFrame.UpdateButtonStates;
begin
  FButtonStates[bsNum]    := tbNumber.Down;
  FButtonStates[bsEnum]   := tbEnum.Down;
  FButtonStates[bsBool]   := tbBool.Down;
  FButtonStates[bsChar]   := tbChar.Down;
  FButtonStates[bsFloat]  := tbFloat.Down;
  FButtonStates[bsStruct] := tbStruct.Down;
  FButtonStates[bsPtr]    := tbPointer.Down;
  FButtonStates[bsArray]  := tbArray.Down;
  FButtonStates[bsIndent] := tbIndent.Down;
end;

procedure TDisplayFormatFrame.UpdateVisiblePanels;
var
  CaptDivEnum, CaptRbEnumName: String;
  CheckAddrFormatVis, ArrayOnly: Boolean;
begin
  CheckAddrFormatVis := (tbPointer.Down or tbStruct.Down) and
                        (tbNumber.Down or tbEnum.Down or tbBool.Down or tbChar.Down or tbFloat.Down or tbArray.Down) and // any other
                        (not FShowFullAddressInMulti);
  ArrayOnly := tbArray.Down and
    not(tbPointer.Down or tbStruct.Down or tbNumber.Down or tbEnum.Down or tbBool.Down or tbChar.Down or tbFloat.Down);  // any other

  PanelNum.Visible           := tbNumber.Down;
  PanelNum2.Visible          := tbNumber.Down;
  PanelEnum.Visible          := tbEnum.Down or tbBool.Down or tbChar.Down;
  PanelEnumVal.Visible       := FShowExtraSettings and
                                tbEnum.Down and
                                not(tbNumber.Down or tbFloat.Down or tbStruct.Down or tbPointer.Down);
  PanelFloat.Visible         := tbFloat.Down;
  PanelStruct.Visible        := tbStruct.Down;
  PanelPointer.Visible       := tbPointer.Down;
  PanelAddressFormat.Visible := (tbPointer.Down or tbStruct.Down) and (not CheckAddrFormatVis);
  PanelArray.Visible         := tbArray.Down;
  PanelArrayPrefixCombine.Visible := ArrayOnly;
  PanelArrayNavBar.Visible   := ArrayOnly and FShowArrayNavBarOpts;

  PanelIndent.Visible        := tbIndent.Down;

  lbStructAddrTypedFiller.Visible := CheckAddrFormatVis;
  cbStructAddrTyped.Visible       := CheckAddrFormatVis;
  cbPointerAddrTyped.Visible      := CheckAddrFormatVis;



  CaptRbEnumName := '';
  CaptDivEnum    := '';
  if FButtonStates[bsEnum] then begin
    CaptDivEnum    := DispFormatGroupEnum;
    CaptRbEnumName := DispFormatEnumName;
  end;
  if FButtonStates[bsBool] then begin
    AddToStr(CaptDivEnum,    DispFormatGroupBool, ', ');
    AddToStr(CaptRbEnumName, DispFormatBoolName, '/');
  end;
  if FButtonStates[bsChar] then begin
    AddToStr(CaptDivEnum,    DispFormatGroupChar, ', ');
    AddToStr(CaptRbEnumName, DispFormatCharLetter, '/');
  end;

  rbEnumName.Caption        := CaptRbEnumName;
  lbOverrideEnum.Caption    := CaptDivEnum;
  if FButtonStates[bsStruct] and FButtonStates[bsPtr] then
    lbOverrideAddressFormat.Caption := {DispFormatDlgBtnAdrFormat+ ': ' +} DispFormatDlgBtnStruct + ', ' + DispFormatDlgBtnPointer
  else
    lbOverrideAddressFormat.Caption := DispFormatDlgBtnAdrFormat;
end;

procedure TDisplayFormatFrame.UpdateNumDigitPanel;
var
  d: TValueDisplayFormats;
  FormatNumDigits, i: integer;
begin
  PanelNumDigits.Enabled := (cbOverrideNumBase.State <> cbUnchecked) or ShowOverrideChecks;

  d := ReadDispForm(PanelNumBase, RBA_Num);

  PanelNumSign.Enabled   := d <> [vdfBaseChar];
  PanelNumDigits.Enabled := d <> [vdfBaseChar];

  if vdfBaseBin in d then SpinDigits.MaxValue := 64
  else if vdfBaseOct in d then SpinDigits.MaxValue := 22
  else if vdfBaseDecimal in d then SpinDigits.MaxValue := 20
  else SpinDigits.MaxValue := 16;

  if SpinDigits.Tag = 0 then begin
    FormatNumDigits     := INT_UNK;
    for i := 0 to FDisplayFormatCount - 1 do
      UpdateIntSetting(FormatNumDigits, FDisplayFormat[i].Num.MinDigits[FDisplayFormat[i].Num.BaseFormat]);

    IntToSpinEdit(SpinDigits, FormatNumDigits);
  end;

  lbNumSepGroup.Visible    := (d * [vdfBaseHex, vdfBaseBin] = d) and (d <> []);
  PanelNumSepGroup.Visible := (d * [vdfBaseHex, vdfBaseBin] = d) and (d <> []);
  cbNumSeparator.Enabled   := d = [vdfBaseDecimal];
  cbNumSeparator.Visible   := not lbNumSepGroup.Visible;
end;

procedure TDisplayFormatFrame.UpdateNum2Visibility;
begin
  PanelNum2All.Visible :=
     ( (BtnDownCount = 1) and (FButtonStates[bsNum]) ) or
     ( ((cbOverrideNum2Base.State <> cbUnchecked) or (not ShowOverrideChecks)) and
       (cbNum2Visibile.State <> cbUnchecked) );
end;

procedure TDisplayFormatFrame.UpdateNum2DigitPanel;
var
  d: TValueDisplayFormats;
  FormatNumDigits: LongInt;
  i: Integer;
begin
  PanelNum2Digits.Enabled := (cbOverrideNum2Base.State <> cbUnchecked) or ShowOverrideChecks;

  d := ReadDispForm(PanelNum2Base, RBA_Num);

  PanelNum2Sign.Enabled   := d <> [vdfBaseChar];
  PanelNum2Digits.Enabled := d <> [vdfBaseChar];

  if vdfBaseBin in d then Spin2Digits.MaxValue := 64
  else if vdfBaseOct in d then Spin2Digits.MaxValue := 22
  else if vdfBaseDecimal in d then Spin2Digits.MaxValue := 20
  else Spin2Digits.MaxValue := 16;

  if Spin2Digits.Tag = 0 then begin
    FormatNumDigits     := INT_UNK;
    for i := 0 to FDisplayFormatCount - 1 do
      UpdateIntSetting(FormatNumDigits, FDisplayFormat[i].Num2.MinDigits[FDisplayFormat[i].Num2.BaseFormat]);

    IntToSpinEdit(Spin2Digits, FormatNumDigits);
  end;

  lbNum2SepGroup.Visible    := (d * [vdfBaseHex, vdfBaseBin] = d) and (d <> []);
  PanelNum2SepGroup.Visible := (d * [vdfBaseHex, vdfBaseBin] = d) and (d <> []);
  cbNum2Separator.Enabled   := d = [vdfBaseDecimal];
  cbNum2Separator.Visible   := not lbNum2SepGroup.Visible;
end;

procedure TDisplayFormatFrame.UpdateFormatOverrides;
var
  i: Integer;
begin
  if not ShowOverrideChecks then
    exit;
  for i := 0 to FDisplayFormatCount - 1 do begin
    if FButtonStates[bsNum] then begin
      BoolFromCBState(cbOverrideNumBase.State, FDisplayFormat[i].Num.UseInherited);
      BoolFromCBState(cbOverrideNum2Base.State, FDisplayFormat[i].Num2.UseInherited);
    end;
    if FButtonStates[bsEnum] then begin
      BoolFromCBState(cbOverrideEnum.State, FDisplayFormat[i].Enum.UseInherited);
      if PanelEnumVal.Visible then
        BoolFromCBState(cbOverrideEnumVal.State, FDisplayFormat[i].EnumVal.UseInherited);
    end;
    if FButtonStates[bsBool] then begin
      BoolFromCBState(cbOverrideEnum.State, FDisplayFormat[i].Bool.UseInherited);
    end;
    if FButtonStates[bsChar] then begin
      BoolFromCBState(cbOverrideEnum.State, FDisplayFormat[i].Char.UseInherited);
    end;
    if FButtonStates[bsFloat] then begin
      BoolFromCBState(cbOverrideFloat.State, FDisplayFormat[i].Float.UseInherited);
    end;
    if FButtonStates[bsStruct] then begin
      BoolFromCBState(cbOverrideStruct.State, FDisplayFormat[i].Struct.UseInherited);
      if cbStructAddrTyped.Visible then begin
        if cbStructAddrTyped.State <> cbGrayed then
          FDisplayFormat[i].Struct.Address.UseInherited := False;
      end
      else
        BoolFromCBState(cbOverrideAddressFormat.State, FDisplayFormat[i].Struct.Address.UseInherited);
    end;
    if FButtonStates[bsPtr] then begin
      BoolFromCBState(cbOverridePointerDeref.State, FDisplayFormat[i].Pointer.UseInherited);
      if cbPointerAddrTyped.Visible then begin
        if cbPointerAddrTyped.State <> cbGrayed then
          FDisplayFormat[i].Pointer.Address.UseInherited := False;
      end
      else
        BoolFromCBState(cbOverrideAddressFormat.State, FDisplayFormat[i].Pointer.Address.UseInherited);
    end;
    if FButtonStates[bsArray] then begin
      BoolFromCBState(cbOverrideArray.State, FDisplayFormat[i].ArrayLen.UseInherited);
      if FShowArrayNavBarOpts then
        BoolFromCBState(cbOverrideArrayNavBar.State, FDisplayFormat[i].ArrayNavBar.UseInherited);
    end;
    if FButtonStates[bsIndent] then begin
      BoolFromCBState(cbOverrideIndent.State, FDisplayFormat[i].MultiLine.UseInherited);
    end;
  end;
end;

procedure TDisplayFormatFrame.UpdateFormat;
var
  d: TValueDisplayFormat;
  ds: TValueDisplayFormats;
  g: TValueDisplayFormatHexSeperator;
  gs: TValueDisplayFormatHexSeperators;
  i: Integer;
  als: TValueDisplayFormatArrayTypes;
  al: TValueDisplayFormatArrayType;
begin
  UpdateFormatOverrides;

  for i := 0 to FDisplayFormatCount - 1 do begin
    if FButtonStates[bsNum] then begin
      // Primary format
      // first values that depend on OLD base
      if (FDisplayFormat[i].Num.BaseFormat = vdfBaseDecimal) then
        BoolFromCB(cbNumSeparator, FDisplayFormat[i].Num.SeparatorDec, False);
      if (FDisplayFormat[i].Num.BaseFormat in [vdfBaseHex, vdfBaseBin]) then begin
        gs := ReadDispForm(PanelNumSepGroup, RBA_Group);
        if HexSepCount(gs) = 1 then
          for g := low(TValueDisplayFormatHexSeperator) to high(TValueDisplayFormatHexSeperator) do
            if g in gs then
              FDisplayFormat[i].Num.SeparatorHexBin := g;
      end;
      if (SpinDigits.Tag = 0) and (FDisplayFormat[i].Num.BaseFormat <> vdfBaseChar) then
        FDisplayFormat[i].Num.MinDigits[FDisplayFormat[i].Num.BaseFormat] := SpinDigits.Value;

      ds := ReadDispForm(PanelNumBase, RBA_Num);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
          if d in ds then
            FDisplayFormat[i].Num.BaseFormat := d;
      ds := ReadDispForm(PanelNumSign, RBA_Sign);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do
          if d in ds then
            FDisplayFormat[i].Num.SignFormat := d;

      // 2nd format
      if PanelNum2All.Visible then begin
        // first values that depend on OLD base
        if (FDisplayFormat[i].Num2.BaseFormat = vdfBaseDecimal) then
          BoolFromCB(cbNum2Separator, FDisplayFormat[i].Num2.SeparatorDec, False);
        if (FDisplayFormat[i].Num2.BaseFormat in [vdfBaseHex, vdfBaseBin]) then begin
          gs := ReadDispForm(PanelNum2SepGroup, RBA_Group);
          if HexSepCount(gs) = 1 then
            for g := low(TValueDisplayFormatHexSeperator) to high(TValueDisplayFormatHexSeperator) do
              if g in gs then
                FDisplayFormat[i].Num2.SeparatorHexBin := g;
        end;
        if (Spin2Digits.Tag = 0) and (FDisplayFormat[i].Num2.BaseFormat <> vdfBaseChar) then
          FDisplayFormat[i].Num2.MinDigits[FDisplayFormat[i].Num2.BaseFormat] := Spin2Digits.Value;

        ds := ReadDispForm(PanelNum2Base, RBA_Num);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
            if d in ds then
              FDisplayFormat[i].Num2.BaseFormat := d;
        ds := ReadDispForm(PanelNum2Sign, RBA_Sign);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do
            if d in ds then
              FDisplayFormat[i].Num2.SignFormat := d;
      end;
      BoolFromCB(cbNum2Visibile, FDisplayFormat[i].Num2.Visible, False);
    end;
    if FButtonStates[bsEnum] then begin
      ds := ReadDispForm(PanelEnumRb1, RBA_Enum);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do
          if d in ds then
            FDisplayFormat[i].Enum.MainFormat := d;
      ds := ReadDispForm(PanelENumBase, RBA_Num);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
          if d in ds then
            FDisplayFormat[i].Enum.BaseFormat := d;
      if cbEnumSign.Tag = 0 then
        case cbEnumSign.State of
          cbUnchecked: FDisplayFormat[i].Enum.SignFormat := vdfSignAuto;
          cbChecked:   FDisplayFormat[i].Enum.SignFormat := vdfSignUnsigned;
        end;
      if FShowExtraSettings then begin
        ds := ReadDispForm(PanelEnumValRb, RBA_Enum);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do
            if d in ds then
              FDisplayFormat[i].EnumVal.MainFormat := d;
        ds := ReadDispForm(PanelENumValBase, RBA_Num);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
            if d in ds then
              FDisplayFormat[i].EnumVal.BaseFormat := d;
        if cbEnumValSign.Tag = 0 then
          case cbEnumValSign.State of
            cbUnchecked: FDisplayFormat[i].EnumVal.SignFormat := vdfSignAuto;
            cbChecked:   FDisplayFormat[i].EnumVal.SignFormat := vdfSignUnsigned;
          end;
      end;
    end;
    if FButtonStates[bsBool] then begin
      ds := ReadDispForm(PanelEnumRb1, RBA_Enum);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do
          if d in ds then
            FDisplayFormat[i].Bool.MainFormat := FormatEnumToBool[d];
      ds := ReadDispForm(PanelENumBase, RBA_Num);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
          if d in ds then
            FDisplayFormat[i].Bool.BaseFormat := d;
      if cbEnumSign.Tag = 0 then
        case cbEnumSign.State of
          cbUnchecked: FDisplayFormat[i].Bool.SignFormat := vdfSignAuto;
          cbChecked:   FDisplayFormat[i].Bool.SignFormat := vdfSignUnsigned;
        end;
    end;
    if FButtonStates[bsChar] then begin
      ds := ReadDispForm(PanelEnumRb1, RBA_Enum);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do
          if d in ds then
            FDisplayFormat[i].Char.MainFormat := FormatEnumToChar[d];
      ds := ReadDispForm(PanelENumBase, RBA_Num);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
          if d in ds then
            FDisplayFormat[i].Char.BaseFormat := d;
      if cbEnumSign.Tag = 0 then
        case cbEnumSign.State of
          cbUnchecked: FDisplayFormat[i].Char.SignFormat := vdfSignAuto;
          cbChecked:   FDisplayFormat[i].Char.SignFormat := vdfSignUnsigned;
        end;
    end;
    if FButtonStates[bsFloat] then begin
      ds := ReadDispForm(PanelFloatRb, RBA_Float);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatFloat) to high(TValueDisplayFormatFloat) do
          if d in ds then
            FDisplayFormat[i].Float.NumFormat := d;
      if (SpinFloatDigits.Tag = 0) then
        FDisplayFormat[i].Float.Precission := SpinFloatDigits.Value;
    end;
    if FButtonStates[bsStruct] then begin
      ds := ReadDispForm(PanelStructFld, RBA_StructVal);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatStruct) to high(TValueDisplayFormatStruct) do
          if d in ds then
            FDisplayFormat[i].Struct.DataFormat := d;
      ds := ReadDispForm(PanelStructPointer, RBA_StructPtr);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatStructPointer) to high(TValueDisplayFormatStructPointer) do
          if d in ds then
            FDisplayFormat[i].Struct.ShowPointerFormat := d;

      if cbStructAddrTyped.Visible then begin
        case cbStructAddrTyped.State of
          cbUnchecked: FDisplayFormat[i].Struct.Address.TypeFormat := vdfAddressPlain;
          cbChecked:   FDisplayFormat[i].Struct.Address.TypeFormat := vdfAddressTyped;
        end;
      end
      else begin
        ds := ReadDispForm(PanelAddressType, RBA_Addr);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatAddress) to high(TValueDisplayFormatAddress) do
            if d in ds then
              FDisplayFormat[i].Struct.Address.TypeFormat := d;
        ds := ReadDispForm(PanelAddressBase, RBA_AddrNum);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
            if d in ds then
              FDisplayFormat[i].Struct.Address.BaseFormat := d;
        BoolFromCB(cbAddrSign, FDisplayFormat[i].Struct.Address.Signed, False);
        BoolFromCB(cbAddrNoLeadZero, FDisplayFormat[i].Struct.Address.NoLeadZero, False);
      end;
    end;
    if FButtonStates[bsPtr] then begin
      ds := ReadDispForm(PanelPointerDeref, RBA_PtrDeref);
      if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
        for d := low(TValueDisplayFormatPointerDeref) to high(TValueDisplayFormatPointerDeref) do
          if d in ds then
            FDisplayFormat[i].Pointer.DerefFormat := d;

      if cbPointerAddrTyped.Visible then begin
        case cbPointerAddrTyped.State of
          cbUnchecked: FDisplayFormat[i].Pointer.Address.TypeFormat := vdfAddressPlain;
          cbChecked:   FDisplayFormat[i].Pointer.Address.TypeFormat := vdfAddressTyped;
        end;
      end
      else begin
        ds := ReadDispForm(PanelAddressType, RBA_Addr);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatAddress) to high(TValueDisplayFormatAddress) do
            if d in ds then
              FDisplayFormat[i].Pointer.Address.TypeFormat := d;
        ds := ReadDispForm(PanelAddressBase, RBA_AddrNum);
        if IdeDebuggerDisplayFormats.DisplayFormatCount(ds) = 1 then
          for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
            if d in ds then
              FDisplayFormat[i].Pointer.Address.BaseFormat := d;
        BoolFromCB(cbAddrSign, FDisplayFormat[i].Pointer.Address.Signed, False);
        BoolFromCB(cbAddrNoLeadZero, FDisplayFormat[i].Pointer.Address.NoLeadZero, False);
      end;
    end;
    if FButtonStates[bsArray] then begin
      BoolFromCB(cbArrayShowPrefix, FDisplayFormat[i].ArrayLen.ShowLenPrefix, False);
      BoolFromCB(cbArrayShowPrefixEmbedded, FDisplayFormat[i].ArrayLen.ShowLenPrefixEmbedded, False);
      if (spinArrayLenMaxNest.Tag = 0) then
        FDisplayFormat[i].ArrayLen.LenPrefixMaxNest := spinArrayLenMaxNest.Value;
      als := ReadDispForm(PanelArrayPrefixCombine, RBA_ArrayCombine);
      if LenCombineCount(als) = 1 then
        for al := low(TValueDisplayFormatArrayType) to high(TValueDisplayFormatArrayType) do
          if al in als then
            FDisplayFormat[i].ArrayLen.LenPrefixCombine := al;

      if FShowArrayNavBarOpts then begin
        BoolFromCB(cbArrayNavAutoHide, FDisplayFormat[i].ArrayNavBar.AutoHideNavBar, False);
        BoolFromCB(cbArrayNavEnforceBounds, FDisplayFormat[i].ArrayNavBar.EnforceBounds, False);
        if (SpinPageSize.Tag = 0) then
          FDisplayFormat[i].ArrayNavBar.PageSize := SpinPageSize.Value;
      end;
    end;
    if FButtonStates[bsIndent] then begin
      if (SpinIndentMaxWrap.Tag = 0) then
        FDisplayFormat[i].MultiLine.MaxMultiLineDepth := SpinIndentMaxWrap.Value;
    end;

    BoolFromCBState(cbMemDump.State, FDisplayFormat[i].MemDump, False);
DebugLn(dbgs(FDisplayFormat[i]));
  end;
end;

procedure TDisplayFormatFrame.CreateHandle;
begin
  inc(FUpdatingDisplay); // gtk2 send extra events
  try
    inherited;
  finally
    dec(FUpdatingDisplay);
  end;
  UpdateDisplay;
end;

procedure TDisplayFormatFrame.UpdateDisplay;
var
  InherhitNum, InherhitNum2, InherhitEnum, InherhitEnumVal, InherhitFloat,
  InherhitStruct, InherhitPtr, InherhitAddress, InherhitIndent, InherhitArrayLen, InherhitArrayNav: TBoolSet;

  FormatNumBase:       TValueDisplayFormats;
  FormatNumSign:       TValueDisplayFormats;
  FormatNumSep:        TBoolSet;
  FormatNumGroup:      TValueDisplayFormatHexSeperators;

  FormatNum2Visible:   TBoolSet;
  FormatNum2Base:      TValueDisplayFormats;
  FormatNum2Sign:      TValueDisplayFormats;
  FormatNum2Sep:       TBoolSet;
  FormatNum2Group:     TValueDisplayFormatHexSeperators;

  FormatEnum:          TValueDisplayFormats;
  FormatEnumBase:      TValueDisplayFormats;
  FormatEnumSign:      TBoolSet;

  FormatEnumVal:          TValueDisplayFormats;
  FormatEnumValBase:      TValueDisplayFormats;
  FormatEnumValSign:      TBoolSet;

  FormatFloat:         TValueDisplayFormats;
  FormatFloatPrec:     integer;

  FormatStruct:        TValueDisplayFormats;
  FormatStructPointer: TValueDisplayFormats;

  FormatPointerDeref:  TValueDisplayFormats;

  FormatAddress:       TValueDisplayFormats;
  FormatAddressStr_Ty:    TBoolSet;
  FormatAddressPtr_Ty:    TBoolSet;
  FormatAddressBase:   TValueDisplayFormats;
  FormatAddressSign:   TBoolSet;
  FormatAddressLead:   TBoolSet;

  FormatArrayShowLen:         TBoolSet;
  FormatArrayShowLenEmbedded: TBoolSet;
  FormatArrayLenMaxNest:      integer;
  FormatArrayLenCombine:      TValueDisplayFormatArrayTypes;

  FormatArrayNavAutoHide:    TBoolSet;
  FormatArrayNavForceBounds: TBoolSet;
  FormatPageSize:     integer;

  FormatIndentMaxWrap: integer;

  FormatIsMemDump: TBoolSet;

  i: Integer;
begin
  if FUpdateCount > 0 then begin
    FNeedUpdateDisplay := True;
    exit;
  end;
  inc(FUpdatingDisplay);

  UpdateButtonStates;

  DisableAutoSizing;
  try
    UpdateTabs;
    UpdateVisiblePanels;

    InherhitNum     := [];
    InherhitNum2    := [];
    InherhitEnum    := [];
    InherhitEnumVal := [];
    InherhitFloat   := [];
    InherhitStruct  := [];
    InherhitPtr     := [];
    InherhitAddress := [];
    InherhitArrayLen:= [];
    InherhitArrayNav:= [];
    InherhitIndent  := [];

    FormatNumBase       := [];
    FormatNumSign       := [];
    FormatNumSep        := [];
    FormatNumGroup      := [];

    FormatNum2Visible   := [];
    FormatNum2Base      := [];
    FormatNum2Sign      := [];
    FormatNum2Sep       := [];
    FormatNum2Group     := [];

    FormatEnum          := [];
    FormatENumBase      := [];
    FormatENumSign      := [];

    FormatEnumVal       := [];
    FormatENumValBase   := [];
    FormatENumValSign   := [];

    FormatFloat         := [];
    FormatFloatPrec     := INT_UNK;

    FormatStruct        := [];
    FormatStructPointer := [];

    FormatPointerDeref  := [];

    FormatAddress       := [];
    FormatAddressStr_Ty    := [];
    FormatAddressPtr_Ty    := [];
    FormatAddressBase   := [];
    FormatAddressSign   := [];
    FormatAddressLead   := [];

    FormatArrayShowLen         := [];
    FormatArrayShowLenEmbedded := [];
    FormatArrayLenMaxNest      := INT_UNK;
    FormatArrayLenCombine      := [];

    FormatArrayNavAutoHide    := [];
    FormatArrayNavForceBounds := [];
    FormatPageSize:= INT_UNK;

    FormatIndentMaxWrap := INT_UNK;

    FormatIsMemDump := [];

    for i := 0 to FDisplayFormatCount - 1 do begin
      FormatIsMemDump := FormatIsMemDump + [boolean(FDisplayFormat[i].MemDump)];

      if FButtonStates[bsNum] then begin
        include(InherhitNum,       FDisplayFormat[i].Num.UseInherited);
        if (not FDisplayFormat[i].Num.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatNumBase,     FDisplayFormat[i].Num.BaseFormat);
          include(FormatNumSign,     FDisplayFormat[i].Num.SignFormat);
          include(FormatNumSep,      FDisplayFormat[i].Num.SeparatorDec);
          include(FormatNumGroup,    FDisplayFormat[i].Num.SeparatorHexBin);
        end;

        include(InherhitNum2,      FDisplayFormat[i].Num2.UseInherited);
        if (not FDisplayFormat[i].Num2.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatNum2Visible, FDisplayFormat[i].Num2.Visible);
          include(FormatNum2Base,    FDisplayFormat[i].Num2.BaseFormat);
          include(FormatNum2Sign,    FDisplayFormat[i].Num2.SignFormat);
          include(FormatNum2Sep,     FDisplayFormat[i].Num2.SeparatorDec);
          include(FormatNum2Group,   FDisplayFormat[i].Num2.SeparatorHexBin);
        end;
      end;
      if FButtonStates[bsEnum] then begin
        include(InherhitEnum,      FDisplayFormat[i].Enum.UseInherited);
        if (not FDisplayFormat[i].Enum.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatEnum,        FDisplayFormat[i].Enum.MainFormat);
          include(FormatEnumBase,    FDisplayFormat[i].Enum.BaseFormat);
          include(FormatEnumSign,    FDisplayFormat[i].Enum.SignFormat = vdfSignUnsigned);
        end;
        include(InherhitEnumVal,   FDisplayFormat[i].EnumVal.UseInherited);
        if (not FDisplayFormat[i].EnumVal.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatEnumVal,     FDisplayFormat[i].EnumVal.MainFormat);
          include(FormatEnumValBase, FDisplayFormat[i].EnumVal.BaseFormat);
          include(FormatEnumValSign, FDisplayFormat[i].EnumVal.SignFormat = vdfSignUnsigned);
        end;
      end;
      if FButtonStates[bsBool] then begin
        include(InherhitEnum,      FDisplayFormat[i].Bool.UseInherited);
        if (not FDisplayFormat[i].Bool.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatEnum,        FormatBoolToEnum[FDisplayFormat[i].Bool.MainFormat]);
          include(FormatEnumBase,    FDisplayFormat[i].Bool.BaseFormat);
          include(FormatEnumSign,    FDisplayFormat[i].Bool.SignFormat = vdfSignUnsigned);
        end;
      end;
      if FButtonStates[bsChar] then begin
        include(InherhitEnum,      FDisplayFormat[i].Char.UseInherited);
        if (not FDisplayFormat[i].Char.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatEnum,        FormatCharToEnum[FDisplayFormat[i].Char.MainFormat]);
          include(FormatEnumBase,    FDisplayFormat[i].Char.BaseFormat);
          include(FormatEnumSign,    FDisplayFormat[i].Char.SignFormat = vdfSignUnsigned);
        end;
      end;
      if FButtonStates[bsFloat] then begin
        include(InherhitFloat,     FDisplayFormat[i].Float.UseInherited);
        if (not FDisplayFormat[i].Float.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatFloat,       FDisplayFormat[i].Float.NumFormat);
          UpdateIntSetting(FormatFloatPrec, FDisplayFormat[i].Float.Precission);
        end;
      end;
      if FButtonStates[bsStruct] then begin
        include(InherhitStruct,      FDisplayFormat[i].Struct.UseInherited);
        if (not FDisplayFormat[i].Struct.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatStruct,        FDisplayFormat[i].Struct.DataFormat);
          include(FormatStructPointer, FDisplayFormat[i].Struct.ShowPointerFormat);
        end;
        include(InherhitAddress,     FDisplayFormat[i].Struct.Address.UseInherited);
        if (not FDisplayFormat[i].Struct.Address.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatAddress,       FDisplayFormat[i].Struct.Address.TypeFormat);
          include(FormatAddressStr_Ty, FDisplayFormat[i].Struct.Address.TypeFormat = vdfAddressTyped);
          include(FormatAddressBase,   FDisplayFormat[i].Struct.Address.BaseFormat);
          include(FormatAddressSign,   FDisplayFormat[i].Struct.Address.Signed);
          include(FormatAddressLead,   FDisplayFormat[i].Struct.Address.NoLeadZero);
        end
        else
        if (FDisplayFormat[i].Struct.Address.UseInherited) then
          FormatAddressStr_Ty := [True, False];
      end;
      if FButtonStates[bsPtr] then begin
        include(InherhitPtr, FDisplayFormat[i].Pointer.UseInherited);
        if (not FDisplayFormat[i].Pointer.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatPointerDeref, FDisplayFormat[i].Pointer.DerefFormat);
        end;
        include(InherhitAddress, FDisplayFormat[i].Pointer.Address.UseInherited);
        if (not FDisplayFormat[i].Pointer.Address.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatAddress,       FDisplayFormat[i].Pointer.Address.TypeFormat);
          include(FormatAddressPtr_Ty, FDisplayFormat[i].Pointer.Address.TypeFormat = vdfAddressTyped);
          include(FormatAddressBase,   FDisplayFormat[i].Pointer.Address.BaseFormat);
          include(FormatAddressSign,   FDisplayFormat[i].Pointer.Address.Signed);
          include(FormatAddressLead,   FDisplayFormat[i].Pointer.Address.NoLeadZero);
        end
        else
        if (FDisplayFormat[i].Pointer.Address.UseInherited) then
          FormatAddressPtr_Ty := [True, False];
      end;

      if FButtonStates[bsArray] then begin
        include(InherhitArrayLen,  FDisplayFormat[i].ArrayLen.UseInherited);
        if (not FDisplayFormat[i].ArrayLen.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatArrayShowLen,  FDisplayFormat[i].ArrayLen.ShowLenPrefix);
          include(FormatArrayShowLenEmbedded,  FDisplayFormat[i].ArrayLen.ShowLenPrefixEmbedded);
          UpdateIntSetting(FormatArrayLenMaxNest, FDisplayFormat[i].ArrayLen.LenPrefixMaxNest);
          include(FormatArrayLenCombine,  FDisplayFormat[i].ArrayLen.LenPrefixCombine);
        end;

        include(InherhitArrayNav,  FDisplayFormat[i].ArrayNavBar.UseInherited);
        if (not FDisplayFormat[i].ArrayNavBar.UseInherited) or (not ShowOverrideChecks) then begin
          include(FormatArrayNavAutoHide,    FDisplayFormat[i].ArrayNavBar.AutoHideNavBar);
          include(FormatArrayNavForceBounds, FDisplayFormat[i].ArrayNavBar.EnforceBounds);
          UpdateIntSetting(FormatPageSize, FDisplayFormat[i].ArrayNavBar.PageSize);
        end;
      end;

      if FButtonStates[bsIndent] then begin
        include(InherhitIndent,  FDisplayFormat[i].MultiLine.UseInherited);
        UpdateIntSetting(FormatIndentMaxWrap, FDisplayFormat[i].MultiLine.MaxMultiLineDepth);
      end;
    end;

    if ShowOverrideChecks then begin
      cbOverrideNumBase.State       := BoolsetToCBState(InherhitNum);
      cbOverrideNum2Base.State      := BoolsetToCBState(InherhitNum2);
      cbOverrideEnum.State          := BoolsetToCBState(InherhitEnum);
      cbOverrideEnumVal.State       := BoolsetToCBState(InherhitEnumVal);
      cbOverrideFloat.State         := BoolsetToCBState(InherhitFloat);
      cbOverrideStruct.State        := BoolsetToCBState(InherhitStruct);
      cbOverridePointerDeref.State  := BoolsetToCBState(InherhitPtr);
      cbOverrideAddressFormat.State := BoolsetToCBState(InherhitAddress);
      cbOverrideArray.State         := BoolsetToCBState(InherhitArrayLen);
      cbOverrideArrayNavBar.State   := BoolsetToCBState(InherhitArrayNav);
      cbOverrideIndent.State        := BoolsetToCBState(InherhitIndent);
    end
    else begin
      InherhitNum     := [False];
      InherhitNum2    := [False];
      InherhitEnum    := [False];
      InherhitEnumVal := [False];
      InherhitFloat   := [False];
      InherhitStruct  := [False];
      InherhitPtr     := [False];
      InherhitAddress := [False];
      InherhitArrayLen:= [False];
      InherhitArrayNav:= [False];
      InherhitIndent  := [False];
    end;


    if (InherhitNum = [True]) or (not FButtonStates[bsNum]) then begin
      ClearRadios(PanelNumBase);
      ClearRadios(PanelNumSign);
      cbNumSeparator.State :=         cbUnchecked;
      cbNumSeparator.Tag   :=         1; // do not read back
      ClearRadios(PanelNumSepGroup);
      SetSpinEditToInherit(SpinDigits);
    end
    else begin
      ApplyDispForm(PanelNumBase,     FormatNumBase, RBA_Num);
      ApplyDispForm(PanelNumSign,     FormatNumSign, RBA_Sign);
      cbNumSeparator.State :=         BoolsetToCBState(FormatNumSep, False);
      cbNumSeparator.Tag   :=         0;
      ApplyDispForm(PanelNumSepGroup, FormatNumGroup, RBA_Group);
      SpinDigits.Tag       := 0;
    end;

    if InherhitNum2 = [True] then begin
      cbNum2Visibile.State  :=         cbUnchecked;
      cbNum2Visibile.Tag    :=         1;
      ClearRadios(PanelNum2Base);
      ClearRadios(PanelNum2Sign);
      cbNum2Separator.State :=         cbUnchecked;
      cbNum2Separator.Tag   :=         1;
      ClearRadios(PanelNum2SepGroup);
      SetSpinEditToInherit(Spin2Digits);
    end
    else begin
      cbNum2Visibile.State  :=         BoolsetToCBState(FormatNum2Visible, False);
      cbNum2Visibile.Tag    :=         0;
      ApplyDispForm(PanelNum2Base,     FormatNum2Base, RBA_Num);
      ApplyDispForm(PanelNum2Sign,     FormatNum2Sign, RBA_Sign);
      cbNum2Separator.State :=         BoolsetToCBState(FormatNum2Sep, False);
      cbNum2Separator.Tag   :=         0;
      ApplyDispForm(PanelNum2SepGroup, FormatNum2Group, RBA_Group);
      Spin2Digits.Tag       := 0;
    end;

    if InherhitEnum = [True] then begin
      ClearRadios(PanelEnumRb1);
      ClearRadios(PanelEnumBase);
      //ClearRadios(PanelEnumSign);
      cbEnumSign.State := cbUnchecked;
      cbEnumSign.Tag   := 1;
    end
    else begin
      ApplyDispForm(PanelEnumRb1,  FormatEnum,     RBA_Enum);
      ApplyDispForm(PanelEnumBase, FormatEnumBase, RBA_Num);
      //ApplyDispForm(PanelEnumSign, FormatEnumSign, RBA_Sign);
      cbEnumSign.State := BoolsetToCBState(FormatEnumSign, False);
      cbEnumSign.Tag   := 0;
    end;

    if InherhitEnumVal = [True] then begin
      ClearRadios(PanelEnumValRb);
      ClearRadios(PanelEnumValBase);
      cbEnumValSign.State := cbUnchecked;
      cbEnumValSign.Tag   := 1;
    end
    else begin
      ApplyDispForm(PanelEnumValRb,   FormatEnumVal,     RBA_Enum);
      ApplyDispForm(PanelEnumValBase, FormatEnumValBase, RBA_Num);
      cbEnumValSign.State := BoolsetToCBState(FormatEnumValSign, False);
      cbEnumValSign.Tag   := 0;
    end;

    if InherhitFloat = [True] then begin
      ClearRadios(PanelFloat);
      SetSpinEditToInherit(SpinFloatDigits)
    end
    else begin
      ApplyDispForm(PanelFloatRb, FormatFloat, RBA_Float);
      IntToSpinEdit(SpinFloatDigits, FormatFloatPrec);
    end;

    if InherhitStruct = [True] then begin
      ClearRadios(PanelStructFld);
      ClearRadios(PanelStructPointer);
    end
    else begin
      ApplyDispForm(PanelStructFld,     FormatStruct,        RBA_StructVal);
      ApplyDispForm(PanelStructPointer, FormatStructPointer, RBA_StructPtr);
    end;

    if InherhitPtr = [True] then begin
      ClearRadios(PanelPointerDeref);
    end
    else begin
      ApplyDispForm(PanelPointerDeref,  FormatPointerDeref,  RBA_PtrDeref);
    end;

    if InherhitAddress = [True] then begin
      ClearRadios(PanelAddressType);
      ClearRadios(PanelAddressBase);
      cbAddrSign.State := cbUnchecked;
      cbAddrNoLeadZero.State := cbUnchecked;
      cbStructAddrTyped.State  := cbGrayed;
      cbPointerAddrTyped.State := cbGrayed;
      cbAddrSign.Tag   := 1;
    end
    else begin
      ApplyDispForm(PanelAddressType, FormatAddress,     RBA_Addr);
      ApplyDispForm(PanelAddressBase, FormatAddressBase, RBA_AddrNum);
      cbAddrSign.State := BoolsetToCBState(FormatAddressSign, False);
      cbAddrNoLeadZero.State := BoolsetToCBState(FormatAddressLead, False);
      cbStructAddrTyped.State  := BoolsetToCBState(FormatAddressStr_Ty, False);
      cbPointerAddrTyped.State := BoolsetToCBState(FormatAddressPtr_Ty, False);
      cbAddrSign.Tag   := 0;
    end;

    if InherhitArrayLen = [True] then begin
      cbArrayShowPrefix.State := cbUnchecked;
      cbArrayShowPrefixEmbedded.State := cbUnchecked;
      cbArrayShowPrefix.Tag := 1;
      cbArrayShowPrefixEmbedded.Tag := 1;
      SetSpinEditToInherit(spinArrayLenMaxNest);
      ClearRadios(PanelArrayPrefixCombine);
    end
    else begin
      cbArrayShowPrefix.State := BoolsetToCBState(FormatArrayShowLen, False);
      cbArrayShowPrefixEmbedded.State := BoolsetToCBState(FormatArrayShowLenEmbedded, False);
      IntToSpinEdit(spinArrayLenMaxNest, FormatArrayLenMaxNest);
      ApplyDispForm(PanelArrayPrefixCombine, FormatArrayLenCombine, RBA_ArrayCombine);
      cbArrayShowPrefix.Tag := 0;
      cbArrayShowPrefixEmbedded.Tag := 0;
      spinArrayLenMaxNest.Tag := 0;
    end;

    if InherhitArrayNav = [True] then begin
      cbArrayNavAutoHide.State := cbUnchecked;
      cbArrayNavEnforceBounds.State := cbUnchecked;
      cbArrayNavAutoHide.Tag := 1;
      cbArrayNavEnforceBounds.Tag := 1;
      SetSpinEditToInherit(SpinPageSize)
    end
    else begin
      cbArrayNavAutoHide.State := BoolsetToCBState(FormatArrayNavAutoHide, False);
      cbArrayNavEnforceBounds.State := BoolsetToCBState(FormatArrayNavForceBounds, False);
      IntToSpinEdit(SpinPageSize, FormatPageSize);
      cbArrayNavAutoHide.Tag := 0;
      cbArrayNavEnforceBounds.Tag := 0;
      SpinPageSize.Tag := 0;
    end;

    if InherhitIndent = [True] then begin
      SetSpinEditToInherit(SpinIndentMaxWrap);
    end
    else begin
      IntToSpinEdit(SpinIndentMaxWrap, FormatIndentMaxWrap);
    end;

    UpdateNumDigitPanel;
    UpdateNum2Visibility;
    UpdateNum2DigitPanel;


    cbMemDump.State := BoolsetToCBState(FormatIsMemDump, False);

  finally
    EnableAutoSizing;

    // workaround issue that SpinEdit.Text / EmptyValue don't allways work
    FormatSpinChanged(SpinDigits);
    FormatSpinChanged(Spin2Digits);
    FormatSpinChanged(SpinFloatDigits);
    FormatSpinChanged(SpinIndentMaxWrap);
    FormatSpinChanged(SpinPageSize);
    FormatSpinChanged(spinArrayLenMaxNest);

    dec(FUpdatingDisplay);
  end;
end;

procedure TDisplayFormatFrame.UpdateTabs;
var
  MarkTabNumber, MarkTabEnum, MarkTabBool, MarkTabChar, MarkTabFloat, MarkTabStruct,
    MarkTabPointer, MarkTabArray: Boolean;
  i: Integer;
begin
  MarkTabNumber  := False;
  MarkTabEnum    := False;
  MarkTabBool    := False;
  MarkTabChar    := False;
  MarkTabFloat   := False;
  MarkTabStruct  := False;
  MarkTabPointer := False;
  MarkTabArray   := False;

  if FHighlightModifiedTabs then begin
    for i := 0 to FDisplayFormatCount - 1 do begin
      MarkTabNumber  := MarkTabNumber or
      ((not FDisplayFormat[i].Num.UseInherited) and (
        (FDisplayFormat[i].Num.BaseFormat            = DefaultWatchDisplayFormat.Num.BaseFormat) or
        (FDisplayFormat[i].Num.SignFormat            = DefaultWatchDisplayFormat.Num.SignFormat) or
        (FDisplayFormat[i].Num.MinDigits                = DefaultWatchDisplayFormat.Num.MinDigits) or
        (FDisplayFormat[i].Num.SeparatorDec          = DefaultWatchDisplayFormat.Num.SeparatorDec) or
        (FDisplayFormat[i].Num.SeparatorHexBin       = DefaultWatchDisplayFormat.Num.SeparatorHexBin)
      )) or
      ((not FDisplayFormat[i].Num2.UseInherited) and (
        (FDisplayFormat[i].Num2.Visible              = DefaultWatchDisplayFormat.Num2.Visible) or
        (FDisplayFormat[i].Num2.BaseFormat           = DefaultWatchDisplayFormat.Num2.BaseFormat) or
        (FDisplayFormat[i].Num2.SignFormat           = DefaultWatchDisplayFormat.Num2.SignFormat) or
        (FDisplayFormat[i].Num2.MinDigits               = DefaultWatchDisplayFormat.Num2.MinDigits) or
        (FDisplayFormat[i].Num2.SeparatorDec         = DefaultWatchDisplayFormat.Num2.SeparatorDec) or
        (FDisplayFormat[i].Num2.SeparatorHexBin      = DefaultWatchDisplayFormat.Num2.SeparatorHexBin)
      ));
      MarkTabEnum    := MarkTabEnum or
      ((not FDisplayFormat[i].Enum.UseInherited) and (
        (FDisplayFormat[i].Enum.MainFormat           = DefaultWatchDisplayFormat.Enum.MainFormat) or
        (FDisplayFormat[i].Enum.BaseFormat           = DefaultWatchDisplayFormat.Enum.BaseFormat) or
        (FDisplayFormat[i].Enum.SignFormat           = DefaultWatchDisplayFormat.Enum.SignFormat)
      )) or
      ((not FDisplayFormat[i].EnumVal.UseInherited) and (
        (FDisplayFormat[i].EnumVal.MainFormat        = DefaultWatchDisplayFormat.EnumVal.MainFormat) or
        (FDisplayFormat[i].EnumVal.BaseFormat        = DefaultWatchDisplayFormat.EnumVal.BaseFormat) or
        (FDisplayFormat[i].EnumVal.SignFormat        = DefaultWatchDisplayFormat.EnumVal.SignFormat)
      ));
      MarkTabBool    := MarkTabBool or
      ((not FDisplayFormat[i].Bool.UseInherited) and (
        (FDisplayFormat[i].Bool.MainFormat               = DefaultWatchDisplayFormat.Bool.MainFormat) or
        (FDisplayFormat[i].Bool.BaseFormat           = DefaultWatchDisplayFormat.Bool.BaseFormat) or
        (FDisplayFormat[i].Bool.SignFormat           = DefaultWatchDisplayFormat.Bool.SignFormat)
      ));
      MarkTabChar    := MarkTabChar or
      ((not FDisplayFormat[i].Char.UseInherited) and (
        (FDisplayFormat[i].Char.MainFormat               = DefaultWatchDisplayFormat.Char.MainFormat) or
        (FDisplayFormat[i].Char.BaseFormat           = DefaultWatchDisplayFormat.Char.BaseFormat) or
        (FDisplayFormat[i].Char.SignFormat           = DefaultWatchDisplayFormat.Char.SignFormat)
      ));
      MarkTabFloat   := MarkTabFloat or
      ((not FDisplayFormat[i].Float.UseInherited) and (
        (FDisplayFormat[i].Float.NumFormat              = DefaultWatchDisplayFormat.Float.NumFormat) or
        (FDisplayFormat[i].Float.Precission          = DefaultWatchDisplayFormat.Float.Precission)
      ));
      MarkTabStruct  := MarkTabStruct or
      ((not FDisplayFormat[i].Struct.UseInherited) and (
        (FDisplayFormat[i].Struct.DataFormat             = DefaultWatchDisplayFormat.Struct.DataFormat) or
        (FDisplayFormat[i].Struct.ShowPointerFormat      = DefaultWatchDisplayFormat.Struct.ShowPointerFormat)
      )) or
        ((not FDisplayFormat[i].Struct.Address.UseInherited) and (
        (FDisplayFormat[i].Struct.Address.TypeFormat      = DefaultWatchDisplayFormat.Struct.Address.TypeFormat) or
        (FDisplayFormat[i].Struct.Address.BaseFormat  = DefaultWatchDisplayFormat.Struct.Address.BaseFormat) or
        (FDisplayFormat[i].Struct.Address.Signed      = DefaultWatchDisplayFormat.Struct.Address.Signed)
      ));
      MarkTabPointer := MarkTabPointer or
      ((not FDisplayFormat[i].Pointer.UseInherited) and (
        (FDisplayFormat[i].Pointer.DerefFormat       = DefaultWatchDisplayFormat.Pointer.DerefFormat)
      )) or
      ((not FDisplayFormat[i].Pointer.Address.UseInherited) and (
        (FDisplayFormat[i].Pointer.Address.TypeFormat     = DefaultWatchDisplayFormat.Pointer.Address.TypeFormat) or
        (FDisplayFormat[i].Pointer.Address.BaseFormat = DefaultWatchDisplayFormat.Pointer.Address.BaseFormat) or
        (FDisplayFormat[i].Pointer.Address.Signed     = DefaultWatchDisplayFormat.Pointer.Address.Signed)
      ));
      MarkTabArray := MarkTabBool or
      ((not FDisplayFormat[i].ArrayLen.UseInherited) and (
        (FDisplayFormat[i].ArrayLen.ShowLenPrefix          = DefaultWatchDisplayFormat.ArrayLen.ShowLenPrefix) or
        (FDisplayFormat[i].ArrayLen.ShowLenPrefixEmbedded  = DefaultWatchDisplayFormat.ArrayLen.ShowLenPrefixEmbedded) or
        (FDisplayFormat[i].ArrayLen.LenPrefixMaxNest       = DefaultWatchDisplayFormat.ArrayLen.LenPrefixMaxNest) or
        (FDisplayFormat[i].ArrayLen.LenPrefixCombine       = DefaultWatchDisplayFormat.ArrayLen.LenPrefixCombine)
      )) or
      ((not FDisplayFormat[i].ArrayNavBar.UseInherited) and FShowArrayNavBarOpts and (
        (FDisplayFormat[i].ArrayNavBar.AutoHideNavBar = DefaultWatchDisplayFormat.ArrayNavBar.AutoHideNavBar) or
        (FDisplayFormat[i].ArrayNavBar.EnforceBounds  = DefaultWatchDisplayFormat.ArrayNavBar.EnforceBounds) or
        (FDisplayFormat[i].ArrayNavBar.PageSize       = DefaultWatchDisplayFormat.ArrayNavBar.PageSize)
      ));
    end;
  end;

  tbNumber.Font.Underline  := MarkTabNumber;
  tbEnum.Font.Underline    := MarkTabEnum;
  tbBool.Font.Underline    := MarkTabBool;
  tbChar.Font.Underline    := MarkTabChar;
  tbFloat.Font.Underline   := MarkTabFloat;
  tbStruct.Font.Underline  := MarkTabStruct;
  tbPointer.Font.Underline := MarkTabPointer;
  tbArray.Font.Underline   := MarkTabArray;
end;

constructor TDisplayFormatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DisplayFormatCount := 1;
  FHighlightModifiedTabs := True;
  FShowCurrent := True;
  FShowAll := True;
  FAllowMultiTabs := True;
  FShowMemDump := True;
  FShowMultiRadio := True;
  FShowOverrideChecks := True;
end;

procedure TDisplayFormatFrame.Setup;
var
  s: String;
begin
  tbCurrent.Visible := FShowCurrent;

  tbCurrent.Caption := DispFormatDlgBtnCurrent;
  tbAll.Caption     := DispFormatDlgBtnAll;
  tbNumber.Caption  := DispFormatDlgBtnNumber;
  tbEnum.Caption    := DispFormatDlgBtnEnum;
  tbBool.Caption    := DispFormatDlgBtnBool;
  tbChar.Caption    := DispFormatDlgBtnChar;
  tbFloat.Caption   := DispFormatDlgBtnFloat;
  tbStruct.Caption  := DispFormatDlgBtnStruct;
  tbPointer.Caption := DispFormatDlgBtnPointer;
  tbArray.Caption   := DispFormatDlgBtnArray;
  tbIndent.Caption  := DispFormatDlgBtnOptions;


  lbOverrideNumBase.Caption       := DispFormatDlgBtnNumber;
  rbNumDec.Caption                := DispFormatBaseDecimal;
  rbNumHex.Caption                := DispFormatBaseHex;
  rbNumOct.Caption                := DispFormatBaseOct;
  rbNumBin.Caption                := DispFormatBaseBin;
  rbNumChar.Caption               := DispFormatBaseChar;
  rbSignAuto.Caption              := DispFormatSignAuto;
  rbSignSigned.Caption            := DispFormatSignSigned;
  rbSignUnsigned.Caption          := DispFormatSignUnsigned;
  lbNumDigits.Caption             := DispFormatNumDigits;
  cbNumSeparator.Caption          := DispFormatNumSeperator;
  lbNumSepGroup.Caption           := DispFormatNumSepGroup;
  rbNumSepNone.Caption            := DispFormatNumSepGroupNone;
  rbNumSepByte.Caption            := DispFormatNumSepGroupByte;
  rbNumSepWord.Caption            := DispFormatNumSepGroupWord;
  rbNumSepLong.Caption            := DispFormatNumSepGroupLong;

  lbOverrideNum2Base.Caption      := DispFormatDlgBtnNumber2;
  cbNum2Visibile.Caption          := DispFormatDlgBtnNum2Visible;
  rbNum2Dec.Caption               := DispFormatBaseDecimal;
  rbNum2Hex.Caption               := DispFormatBaseHex;
  rbNum2Oct.Caption               := DispFormatBaseOct;
  rbNum2Bin.Caption               := DispFormatBaseBin;
  rbNum2Char.Caption              := DispFormatBaseChar;
  rbSign2Auto.Caption             := DispFormatSignAuto;
  rbSign2Signed.Caption           := DispFormatSignSigned;
  rbSign2Unsigned.Caption         := DispFormatSignUnsigned;
  lbNum2Digits.Caption            := DispFormatNumDigits;
  cbNum2Separator.Caption         := DispFormatNumSeperator;
  lbNum2SepGroup.Caption          := DispFormatNumSepGroup;
  rbNum2SepNone.Caption           := DispFormatNumSepGroupNone;
  rbNum2SepByte.Caption           := DispFormatNumSepGroupByte;
  rbNum2SepWord.Caption           := DispFormatNumSepGroupWord;
  rbNum2SepLong.Caption           := DispFormatNumSepGroupLong;

  lbOverrideEnum.Caption          := DispFormatDlgBtnEnum +', ' + DispFormatDlgBtnBool +', ' + DispFormatDlgBtnChar;
  s := DispFormatEnumName;
  AddToStr(s, DispFormatBoolName,   '/');
  AddToStr(s, DispFormatCharLetter, '/');
  rbEnumName.Caption            := s;
  rbEnumOrd.Caption             := DispFormatEnumOrd;
  rbEnumNameAndOrd.Caption      := DispFormatEnumNameAndOrd;
  rbENumDec.Caption                := DispFormatBaseDecimal;
  rbENumHex.Caption                := DispFormatBaseHex;
  rbENumOct.Caption                := DispFormatBaseOct;
  rbENumBin.Caption                := DispFormatBaseBin;
  rbENumChar.Caption               := DispFormatBaseChar;
  cbEnumSign.Caption               := DispFormatSignUnsigned;

  lbOverrideEnumVal.Caption     := DispFormatDlgBtnEnumVal;
  rbEnumValName.Caption         := DispFormatEnumName;
  rbEnumValOrd.Caption          := DispFormatEnumOrd;
  rbEnumValNameAndOrd.Caption   := DispFormatEnumNameAndOrd;
  rbENumValDec.Caption          := DispFormatBaseDecimal;
  rbENumValHex.Caption          := DispFormatBaseHex;
  rbENumValOct.Caption          := DispFormatBaseOct;
  rbENumValBin.Caption          := DispFormatBaseBin;
  rbENumValChar.Caption         := DispFormatBaseChar;
  cbEnumValSign.Caption         := DispFormatSignUnsigned;

  lbOverrideFloat.Caption       := DispFormatDlgBtnFloat;
  rbFloatPoint.Caption          := DispFormatFloatPoint;
  rbFloatScience.Caption        := DispFormatFloatScientific;
  lbFloatPrec.Caption           := DispFormatNumDigits;

  lbOverrideStruct.Caption      := DispFormatDlgBtnStruct;
  rbStructValOnly.Caption       := DispFormatStructValOnly;
  rbStructFields.Caption        := DispFormatStructFields;
  rbStructFull.Caption          := DispFormatStructFull;
  rbStructAddrOn.Caption        := DispFormatStructAddressOn;
  rbStructAddrOff.Caption       := DispFormatStructAddressOff;
  rbStructAddrOnly.Caption      := DispFormatStructAddressOnly;

  lbOverridePointerDeref.Caption := DispFormatDlgBtnPointer;
  rbPointerDerefOff.Caption     := DispFormatPointerDerefOff;
  rbPointerDerefOn.Caption      := DispFormatPointerDerefOn;
  rbPointerDerefOnly.Caption    := DispFormatPointerDerefOnly;

  lbOverrideAddressFormat.Caption := DispFormatDlgBtnAdrFormat;
  rbAddressPlain.Caption        := DispFormatPointerAddressPlain;
  rbAddressTyped.Caption        := DispFormatPointerAddressTyped;
  rbAddrNumHex.Caption       := DispFormatBaseHex;
  rbAddrNumDec.Caption       := DispFormatBaseDecimal;
  rbAddrNumOct.Caption       := DispFormatBaseOct;
  rbAddrNumBin.Caption       := DispFormatBaseBin;
  cbAddrSign.Caption         := DispFormatSignSigned;
  cbAddrNoLeadZero.Caption   := DispFormatNoLeadZero;

  cbStructAddrTyped.Caption  := DispFormatPointerAddressTyped;
  cbPointerAddrTyped.Caption := DispFormatPointerAddressTyped;

  lbOverrideIndent.Caption := DispFormatDlgIndent;
  lbMaxWrapLvl.Caption     := DispFormatIndentMaxWrap;

  lbOverrideArray.Caption           := DispFormatDlgArrayLen;
  cbArrayShowPrefix.Caption         := DispFormatDlgArrayShowPrefix;
  cbArrayShowPrefixEmbedded.Caption := DispFormatDlgArrayShowPrefixEmbedded;
  lbArrayLenMaxNest.Caption         := DispFormatDlgArrayMaxNest;
  lbArrayCombine.Caption            := DispFormatDlgArrayCombine;
  rbArrayCombineNone.Caption        := DispFormatDlgArrayCombineNone;
  rbArrayCombineAll.Caption         := DispFormatDlgArrayCombineAll;
  rbArrayCombineStat.Caption        := DispFormatDlgArrayCombineStat;
  rbArrayCombineDyn.Caption         := DispFormatDlgArrayCombineDyn;

  lbOverrideArrayNavBar.Caption   := DispFormatDlgArrayNav;
  cbArrayNavAutoHide.Caption      := DispFormatArrayNavAutoHide;
  cbArrayNavEnforceBounds.Caption := DispFormatArrayNavEnforceBounds;
  lbPageSize.Caption              := DispFormatArrayNavPageSize;

  DividerBevelMemDump.Caption       := '';
  cbMemDump.Caption             := DispFormatCategoryMemDump;

end;

procedure TDisplayFormatFrame.BeginUdpate;
begin
  inc(FUpdateCount);
end;

procedure TDisplayFormatFrame.EndUdpate;
begin
  dec(FUpdateCount);
  if (FUpdateCount = 0) and FNeedUpdateDisplay then
    UpdateDisplay;
end;

procedure TDisplayFormatFrame.SelectDefaultButton;
begin
  FInButtonClick := True;
  try
    tbCurrent.Down := True;
    tbCurrent.Enabled := True;
    tbAll.Down     := False;
    tbNumber.Down  := False;
    tbEnum.Down    := False;
    tbBool.Down    := False;
    tbChar.Down    := False;
    tbFloat.Down   := False;
    tbStruct.Down  := False;
    tbPointer.Down := False;
    tbArray.Down   := False;
    case FCurrentResDataType of
        rdkChar:           tbChar.Down := True;
        rdkSignedNumVal:   tbNumber.Down := True;
        rdkUnsignedNumVal: tbNumber.Down := True;
        rdkPointerVal:     tbPointer.Down := True;
        rdkFloatVal:       tbFloat.Down := True;
        rdkBool:           tbBool.Down := True;
        rdkEnum:           tbEnum.Down := True;
        rdkEnumVal:        tbEnum.Down := True;
        rdkSet:            tbEnum.Down := True;
        rdkStruct:         tbStruct.Down := True;
        rdkArray:          tbArray.Down := True;
        else begin
                           if FShowAll then tbAll.Down := True;
                           tbNumber.Down  := True;
                           tbEnum.Down    := True;
                           tbBool.Down    := True;
                           tbChar.Down    := True;
                           tbFloat.Down   := True;
                           tbStruct.Down  := True;
                           tbPointer.Down := True;
                           tbArray.Down := True;
                           tbCurrent.Down := False;
                           tbCurrent.Enabled := False;
          end;
    end;
  finally
    FInButtonClick := False;
  end;
  UpdateDisplay;
end;

end.

