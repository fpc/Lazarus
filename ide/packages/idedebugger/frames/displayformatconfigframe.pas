unit DisplayFormatConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons, Graphics,
  DividerBevel, IdeDebuggerWatchValueIntf, IdeDebuggerWatchResPrinter,
  IdeDebuggerStringConstants, IdeDebuggerDisplayFormats;

type

  { TDisplayFormatFrame }

  TDisplayFormatFrame = class(TFrame)
    cbMemDump: TCheckBox;
    DividerBevelFloat: TDividerBevel;
    DividerBevelNum: TDividerBevel;
    DividerBevelMemDump: TDividerBevel;
    DividerBevelNumChar: TDividerBevel;
    DividerBevelAddressFormat: TDividerBevel;
    DividerBevelStruct: TDividerBevel;
    DividerBevelEnum: TDividerBevel;
    DividerBevelPointer: TDividerBevel;
    PanelAddressFormat: TPanel;
    PanelMemDump: TPanel;
    PanelPointerDeref: TPanel;
    PanelFloat: TPanel;
    PanelFloatRb: TPanel;
    PanelNumBase: TPanel;
    PanelNumSign: TPanel;
    PanelNumChar: TPanel;
    PanelNum: TPanel;
    PanelPointerNum: TPanel;
    PanelStruct: TPanel;
    PanelEnum: TPanel;
    PanelPointer: TPanel;
    PanelStructFld: TPanel;
    PanelEnumRb1: TPanel;
    PanelPointerType: TPanel;
    PanelStructAddr: TPanel;
    rbPointerDerefDefault: TRadioButton;
    rbPointerNumDefault: TRadioButton;
    rbPointerDerefOff: TRadioButton;
    rbPointerNumHex: TRadioButton;
    rbPointerNumDec: TRadioButton;
    rbPointerDerefOnly: TRadioButton;
    rbPointerDerefOn: TRadioButton;
    rbFloatDefault: TRadioButton;
    rbFloatPoint: TRadioButton;
    rbFloatScience: TRadioButton;
    rbNumCharOff: TRadioButton;
    rbNumCharDefault: TRadioButton;
    rbNumCharUni: TRadioButton;
    rbNumCharUniNum: TRadioButton;
    rbNumBin: TRadioButton;
    rbNumDec: TRadioButton;
    rbNumDefault: TRadioButton;
    rbNumHex: TRadioButton;
    rbNumOct: TRadioButton;
    rbNumPtr: TRadioButton;
    rbPointerNumOct: TRadioButton;
    rbSignDefault: TRadioButton;
    rbSignSigned: TRadioButton;
    rbSignUnsigned: TRadioButton;
    rbStructDefault: TRadioButton;
    rbEnumDefault: TRadioButton;
    rbPointerDefault: TRadioButton;
    rbStructAddrDefault: TRadioButton;
    rbStructFields: TRadioButton;
    rbEnumOrd: TRadioButton;
    rbPointerTypedAddress: TRadioButton;
    rbStructAddrOn: TRadioButton;
    rbStructFull: TRadioButton;
    rbEnumNameAndOrd: TRadioButton;
    rbStructAddrOnly: TRadioButton;
    rbStructValOnly: TRadioButton;
    rbEnumName: TRadioButton;
    rbPointerAddress: TRadioButton;
    rbStructAddrOff: TRadioButton;
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
    procedure cbMemDumpChange(Sender: TObject);
    procedure FormatRadioClicked(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure tbAllClick(Sender: TObject);
    procedure tbCurrentClick(Sender: TObject);
    procedure tbFormatClick(Sender: TObject);
  private type
    TFmtButtons = (bsNum, bsEnum, bsBool, bsChar, bsFloat, bsStruct, bsPtr);
  private
    FHighlightModifiedTabs: boolean;
    FShowCurrent: boolean;
    FLastWidth: integer;

    FDisplayFormatCount: integer;
    FDisplayFormat: array of TWatchDisplayFormat;
    FCurrentResDataType: TWatchResultDataKind;
    FShowMemDump: boolean;
    FShowMultiRadio: boolean;

    FUpdateCount: integer;
    FNeedUpdateDisplay, FUpdatingDisplay: boolean;
    FInButtonClick: Boolean;
    FRadioMap: array [TValueDisplayFormat] of TRadioButton;
    FButtonStates: array[TFmtButtons] of boolean;

    function GetDisplayFormat: TWatchDisplayFormat;
    function GetDisplayFormats(AIndex: integer): TWatchDisplayFormat;
    procedure SetCurrentResDataType(AValue: TWatchResultDataKind);
    procedure SetDisplayFormat(ADispFormat: TWatchDisplayFormat);
    procedure SetDisplayFormatCount(AValue: integer);
    procedure SetDisplayFormats(AIndex: integer; AValue: TWatchDisplayFormat);
    procedure SetHighlightModifiedTabs(AValue: boolean);
    procedure SetShowCurrent(AValue: boolean);
    procedure SetShowMemDump(AValue: boolean);
    procedure SetShowMultiRadio(AValue: boolean);
  protected
    procedure UpdateFormat;
    procedure UpdateDisplay;
    procedure UpdateTabs;
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
    property ShowMemDump: boolean read FShowMemDump write SetShowMemDump;
    property ShowMultiRadio: boolean read FShowMultiRadio write SetShowMultiRadio;
    property HighlightModifiedTabs: boolean read FHighlightModifiedTabs write SetHighlightModifiedTabs;
  end;

implementation

{$R *.lfm}

procedure AddToStr(var txt: string; add, sep: string);
begin
  if (txt = add) then
    exit;
  if txt <> '' then txt := txt + sep;
  txt := txt + add;
end;

{ TDisplayFormatFrame }

procedure TDisplayFormatFrame.cbMemDumpChange(Sender: TObject);
begin
  PanelNum.Enabled    := not cbMemDump.Checked;
  PanelEnum.Enabled := not cbMemDump.Checked;
  PanelFloat.Enabled := not cbMemDump.Checked;
  PanelStruct.Enabled := not cbMemDump.Checked;
  PanelPointer.Enabled := not cbMemDump.Checked;
  PanelAddressFormat.Enabled := not cbMemDump.Checked;
end;

procedure TDisplayFormatFrame.FormatRadioClicked(Sender: TObject);
var
  i: Integer;
begin
  if FUpdatingDisplay then
    exit;
  UpdateFormat;
  UpdateTabs;
  for i := 0 to TRadioButton(Sender).Parent.ControlCount - 1 do
    if TRadioButton(Sender).Parent.Controls[i] is TRadioButton then
      TRadioButton(Sender).Parent.Controls[i].Font.Italic := False;
end;

procedure TDisplayFormatFrame.FrameResize(Sender: TObject);
var
  w, i, pw, ph: Integer;
begin
  w := Width;
  if abs(FLastWidth - w) < 3 then
    exit;
  FLastWidth := w;

  DisableAutoSizing;
  w := ((ClientWidth-10-9-8) div 4); // 10 = borderspacing // 9 = 3 * spacing of 3 // 8 = safety
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TRadioButton) or
       ( (Components[i] is TLabel) {and (TLabel(Components[i]).Caption = '')} )
    then begin
      TControl(Components[i]).GetPreferredSize(pw, ph);
      TControl(Components[i]).AutoSize := False;
      TControl(Components[i]).Width := w;
      TControl(Components[i]).Height := ph;
    end;
  EnableAutoSizing;
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
    tbAll.Down := True;
    FInButtonClick := False;
    tbAllClick(nil);
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
      rdkArray: ;
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
    if (not(ssShift in GetKeyShiftState)) or
       (not(tbNumber.Down or tbEnum.Down or tbBool.Down or tbChar.Down or tbFloat.Down or tbStruct.Down or tbPointer.Down))
    then begin
      tbNumber.Down  := tbNumber  = Sender;
      tbEnum.Down    := tbEnum    = Sender;
      tbBool.Down    := tbBool    = Sender;
      tbChar.Down    := tbChar    = Sender;
      tbFloat.Down   := tbFloat   = Sender;
      tbStruct.Down  := tbStruct  = Sender;
      tbPointer.Down := tbPointer = Sender;
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
      else tbCurrent.Down := False;
    end;
    tbAll.Down := tbNumber.Down and tbEnum.Down and tbBool.Down and tbChar.Down and
                  tbFloat.Down and tbStruct.Down and tbPointer.Down;
  finally
    FInButtonClick := False;
  end;
  UpdateDisplay;
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

procedure TDisplayFormatFrame.SetShowCurrent(AValue: boolean);
begin
  if FShowCurrent = AValue then Exit;
  FShowCurrent := AValue;
  tbCurrent.Visible := FShowCurrent;
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

procedure TDisplayFormatFrame.UpdateFormat;
var
  d: TValueDisplayFormat;
  ds, dsPtr: TValueDisplayFormats;
  OnlyPointerOrStruct: Boolean;
  i: Integer;
begin
  OnlyPointerOrStruct := (FButtonStates[bsPtr] or FButtonStates[bsStruct]) and
    not (FButtonStates[bsNum] or FButtonStates[bsEnum] or FButtonStates[bsBool] or FButtonStates[bsChar]);

  ds := [];
  for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do
    if (FRadioMap[d] <> nil) and (FRadioMap[d].Checked) then
      include(ds, d);

  if not OnlyPointerOrStruct then begin
    dsPtr := [];
    if rbPointerNumDefault.Checked then include(dsPtr, vdfBaseDefault);
    if rbPointerNumHex.Checked     then include(dsPtr, vdfBaseHex);
    if rbPointerNumDec.Checked     then include(dsPtr, vdfBaseDecimal);
    if rbPointerNumOct.Checked     then include(dsPtr, vdfBaseOct);
  end
  else
    dsPtr := ds;

  for i := 0 to FDisplayFormatCount - 1 do begin
    if FButtonStates[bsNum] then begin
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do if d in ds then
        FDisplayFormat[i].NumBaseFormat := d;
      for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
        FDisplayFormat[i].NumSignFormat := d;
      for d := low(TValueDisplayFormatNumChar) to high(TValueDisplayFormatNumChar) do if d in ds then
        FDisplayFormat[i].NumCharFormat := d;
    end;
    if FButtonStates[bsEnum] then begin
      for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do if d in ds then
        FDisplayFormat[i].EnumFormat := d;
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do if d in ds then
        FDisplayFormat[i].EnumBaseFormat := d;
      for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
        FDisplayFormat[i].EnumSignFormat := d;
    end;
    if FButtonStates[bsBool] then begin
      for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do if d in ds then
        FDisplayFormat[i].BoolFormat := FormatEnumToBool[d];
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do if d in ds then
        FDisplayFormat[i].BoolBaseFormat := d;
      for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
        FDisplayFormat[i].BoolSignFormat := d;
    end;
    if FButtonStates[bsChar] then begin
      for d := low(TValueDisplayFormatEnum) to high(TValueDisplayFormatEnum) do if d in ds then
        FDisplayFormat[i].CharFormat := FormatEnumToChar[d];
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do if d in ds then
        FDisplayFormat[i].CharBaseFormat := d;
      for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
        FDisplayFormat[i].CharSignFormat := d;
    end;
    if FButtonStates[bsFloat] then begin
      for d := low(TValueDisplayFormatFloat) to high(TValueDisplayFormatFloat) do if d in ds then
        FDisplayFormat[i].FloatFormat := d;
    end;
    if FButtonStates[bsStruct] then begin
      for d := low(TValueDisplayFormatStruct) to high(TValueDisplayFormatStruct) do if d in ds then
        FDisplayFormat[i].StructFormat := d;
      for d := low(TValueDisplayFormatStructAddr) to high(TValueDisplayFormatStructAddr) do if d in ds then
        FDisplayFormat[i].StructAddrFormat := d;
      for d := low(TValueDisplayFormatPointer) to high(TValueDisplayFormatPointer) do if d in ds then
        FDisplayFormat[i].StructPointerFormat := d;
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
        if d in dsPtr then
          FDisplayFormat[i].StructPointerBaseFormat := d;
      if OnlyPointerOrStruct then
        for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
          FDisplayFormat[i].StructPointerSignFormat := d;
    end;
    if FButtonStates[bsPtr] then begin
      for d := low(TValueDisplayFormatPointer) to high(TValueDisplayFormatPointer) do if d in ds then
        FDisplayFormat[i].PointerFormat := d;
      for d := low(TValueDisplayFormatPointerDeref) to high(TValueDisplayFormatPointerDeref) do if d in ds then
        FDisplayFormat[i].PointerDerefFormat := d;
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
        if d in dsPtr then
          FDisplayFormat[i].PointerBaseFormat := d;
      if OnlyPointerOrStruct then
        for d := low(TValueDisplayFormatSign) to high(TValueDisplayFormatSign) do if d in ds then
          FDisplayFormat[i].PointerSignFormat := d;
    end;

    if cbMemDump.State <> cbGrayed then
      FDisplayFormat[i].MemDump := cbMemDump.Checked;
  end;
end;

procedure TDisplayFormatFrame.UpdateDisplay;
  procedure ApplyDispForm(ADispForm: TValueDisplayFormats);
  var
    d, d2: TValueDisplayFormat;
    m: TValueDisplayFormats;
  begin
    if IdeDebuggerDisplayFormats.DisplayFormatCount(ADispForm) = 1 then begin
      for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do begin
        if (FRadioMap[d] <> nil) and (d in ADispForm) then
          FRadioMap[d].Checked := True;
      end;
    end
    else
    if FShowMultiRadio then begin
      d2 := vdfCategoryData;
      for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do
        if d in ADispForm then begin
          d2 := d;
          break;
        end;
      if d2 = vdfCategoryData then
        exit;
      m := DisplayFormatMask([ValueDisplayFormatGroupMap[d2]]);
      for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do begin
        if (FRadioMap[d] <> nil) and (d in m) then
          FRadioMap[d].Font.Italic := (d in ADispForm);
      end;
    end;
  end;

var
  CaptDivEnum, CaptRbEnumName, CaptDivNumber: String;
  FormatOrdinal:      TValueDisplayFormats;
  FormatSign:         TValueDisplayFormats;
  FormatNumChar:      TValueDisplayFormats;
  FormatEnum:         TValueDisplayFormats;
  FormatFloat:        TValueDisplayFormats;
  FormatStruct:       TValueDisplayFormats;
  FormatStructAddr:   TValueDisplayFormats;
  FormatPointer:      TValueDisplayFormats;
  FormatPointerDeref: TValueDisplayFormats;
  FormatPtrOrdinal:   TValueDisplayFormats; // Pointer uses separate ordinal radios
  FormatIsMemDump: set of boolean;
  OnlyPointerOrStruct: Boolean; // Pointer uses separate ordinal radios
  d: TValueDisplayFormat;
  fb: TFmtButtons;
  BtnDownCount, y, i: Integer;
begin
  if FUpdateCount > 0 then begin
    FNeedUpdateDisplay := True;
    exit;
  end;
  FUpdatingDisplay := True;

  FButtonStates[bsNum]    := tbNumber.Down;
  FButtonStates[bsEnum]   := tbEnum.Down;
  FButtonStates[bsBool]   := tbBool.Down;
  FButtonStates[bsChar]   := tbChar.Down;
  FButtonStates[bsFloat]  := tbFloat.Down;
  FButtonStates[bsStruct] := tbStruct.Down;
  FButtonStates[bsPtr]    := tbPointer.Down;

  // Not checking for float
  OnlyPointerOrStruct := (FButtonStates[bsPtr] or FButtonStates[bsStruct]) and
    not (FButtonStates[bsNum] or FButtonStates[bsEnum] or FButtonStates[bsBool] or FButtonStates[bsChar]);

  BtnDownCount := 0;
  for fb := low(TFmtButtons) to High(TFmtButtons) do
    if FButtonStates[fb] then
      inc(BtnDownCount);

  DisableAutoSizing;
  BeginUpdateBounds;
  try
    UpdateTabs;

    PanelNum.Visible           := tbNumber.Down or tbEnum.Down or tbBool.Down or tbChar.Down or tbStruct.Down or tbPointer.Down;
    PanelNumChar.Visible       := tbNumber.Down;
    PanelEnum.Visible          := tbEnum.Down or tbBool.Down or tbChar.Down;
    PanelFloat.Visible         := tbFloat.Down;
    PanelStruct.Visible        := tbStruct.Down;
    PanelPointer.Visible       := tbPointer.Down;
    PanelAddressFormat.Visible := tbPointer.Down or tbStruct.Down;

    PanelPointerNum.Visible     := not OnlyPointerOrStruct;  // Limited num-base for address
    DividerBevelNum.Visible     := not OnlyPointerOrStruct;      // Hide "Numbers" header / settings for Ptr or Struct
    DividerBevelNumChar.Visible := FButtonStates[bsNum] and (tbEnum.Down or tbBool.Down or tbChar.Down);

    // Vertical ordering
    ToolBar1.Top := 0;
    y := ToolBar1.Height;
    if tbNumber.Down then begin
      PanelNum.Top          := y;    inc(y, PanelNum.Height);
    end;
    PanelEnum.Top           :=  y;   inc(y, PanelEnum.Height);
    if (not tbNumber.Down) and not OnlyPointerOrStruct then begin
      PanelNum.Top          := y;    inc(y, PanelNum.Height);
    end;
    PanelFloat.Top          := y;    inc(y, PanelFloat.Height);
    PanelStruct.Top         := y;    inc(y, PanelStruct.Height);
    PanelPointer.Top        := y;    inc(y, PanelPointer.Height);
    PanelAddressFormat.Top  := y;    inc(y, PanelAddressFormat.Height);
    if (not tbNumber.Down) and OnlyPointerOrStruct then begin
      PanelNum.Top          := y;    inc(y, PanelNum.Height);
    end;
    PanelMemDump.Top        := y;

    //
    CaptRbEnumName := '';
    CaptDivEnum    := '';
    CaptDivNumber  := DispFormatDlgBtnOrdFormat;
    if OnlyPointerOrStruct then
      CaptDivNumber  := '';

    if FButtonStates[bsNum] then begin
      CaptDivNumber  := DispFormatDlgBtnNumber;
    end;
    if FButtonStates[bsEnum] then begin
      CaptDivEnum    := DispFormatGroupEnum;
      CaptRbEnumName := DispFormatEnumName;
      if (FButtonStates[bsNum]) then
        AddToStr(CaptDivNumber, DispFormatDlgBtnEnum, ', ');
    end;
    if FButtonStates[bsBool] then begin
      AddToStr(CaptDivEnum,    DispFormatGroupBool, ', ');
      AddToStr(CaptRbEnumName, DispFormatBoolName, '/');
      if (FButtonStates[bsNum]) then
        AddToStr(CaptDivNumber,  DispFormatDlgBtnBool, ', ');
    end;
    if FButtonStates[bsChar] then begin
      AddToStr(CaptDivEnum,    DispFormatGroupChar, ', ');
      AddToStr(CaptRbEnumName, DispFormatCharLetter, '/');
      if (FButtonStates[bsNum]) then
        AddToStr(CaptDivNumber,  DispFormatDlgBtnChar, ', ');
    end;

    rbEnumName.Caption          := CaptRbEnumName;
    DividerBevelEnum.Caption    := CaptDivEnum;
    DividerBevelNum.Caption     := CaptDivNumber;
    if FButtonStates[bsStruct] and FButtonStates[bsPtr] then
      DividerBevelAddressFormat.Caption := DispFormatDlgBtnAdrFormat+ ': ' + DispFormatDlgBtnStruct + ', ' + DispFormatDlgBtnPointer
    else
      DividerBevelAddressFormat.Caption := DispFormatDlgBtnAdrFormat;


    FormatOrdinal := [];
    FormatSign := [];
    FormatNumChar := [];
    FormatEnum := [];
    FormatFloat := [];
    FormatStruct := [];
    FormatStructAddr := [];
    FormatPointer := [];
    FormatPointerDeref := [];
    FormatPtrOrdinal := [];
    FormatIsMemDump := [];

    for i := 0 to FDisplayFormatCount - 1 do begin
      FormatIsMemDump := FormatIsMemDump + [boolean(FDisplayFormat[i].MemDump)];

      if FButtonStates[bsNum] then begin
        include(FormatOrdinal, FDisplayFormat[i].NumBaseFormat);
        include(FormatSign,    FDisplayFormat[i].NumSignFormat);
        include(FormatNumChar, FDisplayFormat[i].NumCharFormat);
      end;
      if FButtonStates[bsEnum] then begin
        include(FormatEnum,    FDisplayFormat[i].EnumFormat);
        include(FormatOrdinal, FDisplayFormat[i].EnumBaseFormat);
        include(FormatSign,    FDisplayFormat[i].EnumSignFormat);
      end;
      if FButtonStates[bsBool] then begin
        include(FormatEnum,    FormatBoolToEnum[FDisplayFormat[i].BoolFormat]);
        include(FormatOrdinal, FDisplayFormat[i].BoolBaseFormat);
        include(FormatSign,    FDisplayFormat[i].BoolSignFormat);
      end;
      if FButtonStates[bsChar] then begin
        include(FormatEnum,    FormatCharToEnum[FDisplayFormat[i].CharFormat]);
        include(FormatOrdinal, FDisplayFormat[i].CharBaseFormat);
        include(FormatSign,    FDisplayFormat[i].CharSignFormat);
      end;
      if FButtonStates[bsFloat] then begin
        include(FormatFloat,   FDisplayFormat[i].FloatFormat);
      end;
      if FButtonStates[bsStruct] then begin
        include(FormatStruct,     FDisplayFormat[i].StructFormat);
        include(FormatStructAddr, FDisplayFormat[i].StructAddrFormat);
        include(FormatPointer,    FDisplayFormat[i].StructPointerFormat);
        if OnlyPointerOrStruct then begin
          include(FormatOrdinal,    FDisplayFormat[i].StructPointerBaseFormat);
          include(FormatSign,       FDisplayFormat[i].StructPointerSignFormat);
        end
        else
          include(FormatPtrOrdinal, FDisplayFormat[i].StructPointerBaseFormat);
      end;
      if FButtonStates[bsPtr] then begin
        include(FormatPointer,      FDisplayFormat[i].PointerFormat);
        include(FormatPointerDeref, FDisplayFormat[i].PointerDerefFormat);
        if OnlyPointerOrStruct then begin
          include(FormatOrdinal,    FDisplayFormat[i].PointerBaseFormat);
          include(FormatSign,       FDisplayFormat[i].PointerSignFormat);
        end
        else
          include(FormatPtrOrdinal, FDisplayFormat[i].PointerBaseFormat);
      end;
    end;



    for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do
      if FRadioMap[d] <> nil then
        FRadioMap[d].Checked := False;
    rbPointerNumDefault.Checked := False;
    rbPointerNumHex.Checked := False;
    rbPointerNumDec.Checked := False;
    rbPointerNumOct.Checked := False;
    ApplyDispForm(FormatOrdinal);
    ApplyDispForm(FormatSign);
    ApplyDispForm(FormatNumChar);
    ApplyDispForm(FormatEnum);
    ApplyDispForm(FormatFloat);
    ApplyDispForm(FormatStruct);
    ApplyDispForm(FormatStructAddr);
    ApplyDispForm(FormatPointer);
    ApplyDispForm(FormatPointerDeref);
    if IdeDebuggerDisplayFormats.DisplayFormatCount(FormatPtrOrdinal) = 1 then begin
      for d := low(TValueDisplayFormatBase) to high(TValueDisplayFormatBase) do
        if d in FormatPtrOrdinal then
          case d of
            vdfBaseDefault: rbPointerNumDefault.Checked := True;
            vdfBaseHex:     rbPointerNumHex.Checked     := True;
            vdfBaseDecimal: rbPointerNumDefault.Checked := True;
            vdfBaseOct:      rbPointerNumOct.Checked    := True;
          end;
    end;

    if True in FormatIsMemDump then begin
      if False in FormatIsMemDump then
        cbMemDump.State := cbGrayed
      else
        cbMemDump.Checked := True;
    end
    else
      cbMemDump.Checked := False;


  finally
    EndUpdateBounds;
    EnableAutoSizing;
    FUpdatingDisplay := False;
  end;
end;

procedure TDisplayFormatFrame.UpdateTabs;
var
  MarkTabNumber, MarkTabEnum, MarkTabBool, MarkTabChar, MarkTabFloat, MarkTabStruct,
    MarkTabPointer: Boolean;
  i: Integer;
begin
  MarkTabNumber  := False;
  MarkTabEnum    := False;
  MarkTabBool    := False;
  MarkTabChar    := False;
  MarkTabFloat   := False;
  MarkTabStruct  := False;
  MarkTabPointer := False;

  if FHighlightModifiedTabs then begin
    for i := 0 to FDisplayFormatCount - 1 do begin
      MarkTabNumber  := MarkTabNumber or
                        (FDisplayFormat[i].NumBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].NumSignFormat <> vdfSignDefault) or
                        (FDisplayFormat[i].NumCharFormat <> vdfNumCharDefault)
                        ;
      MarkTabEnum    := MarkTabEnum or
                        (FDisplayFormat[i].EnumFormat <> vdfEnumDefault) or
                        (FDisplayFormat[i].EnumBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].EnumSignFormat <> vdfSignDefault)
                        ;
      MarkTabBool    := MarkTabBool or
                        (FDisplayFormat[i].BoolFormat <> vdfBoolDefault) or
                        (FDisplayFormat[i].BoolBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].BoolSignFormat <> vdfSignDefault)
                        ;
      MarkTabChar    := MarkTabChar or
                        (FDisplayFormat[i].CharFormat <> vdfCharDefault) or
                        (FDisplayFormat[i].CharBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].CharSignFormat <> vdfSignDefault)
                        ;
      MarkTabFloat   := MarkTabFloat or
                        (FDisplayFormat[i].FloatFormat <> vdfFloatDefault)
                        ;
      MarkTabStruct  := MarkTabStruct or
                        (FDisplayFormat[i].StructFormat <> vdfStructDefault) or
                        (FDisplayFormat[i].StructAddrFormat <> vdfStructAddressDefault) or
                        (FDisplayFormat[i].StructPointerFormat <> vdfPointerDefault) or
                        (FDisplayFormat[i].StructPointerBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].StructPointerSignFormat <> vdfSignDefault)
                        ;
      MarkTabPointer := MarkTabPointer or
                        (FDisplayFormat[i].PointerDerefFormat <> vdfPointerDerefDefault) or
                        (FDisplayFormat[i].PointerFormat <> vdfPointerDefault) or
                        (FDisplayFormat[i].PointerBaseFormat <> vdfBaseDefault) or
                        (FDisplayFormat[i].PointerSignFormat <> vdfSignDefault)
                        ;
    end;
  end;


  tbNumber.Font.Italic  := MarkTabNumber;
  tbEnum.Font.Italic    := MarkTabEnum;
  tbBool.Font.Italic    := MarkTabBool;
  tbChar.Font.Italic    := MarkTabChar;
  tbFloat.Font.Italic   := MarkTabFloat;
  tbStruct.Font.Italic  := MarkTabStruct;
  tbPointer.Font.Italic := MarkTabPointer;
end;

constructor TDisplayFormatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DisplayFormatCount := 1;
  FHighlightModifiedTabs := True;
  FShowCurrent := True;
  FShowMemDump := True;
  FShowMultiRadio := True;

  FillByte(FRadioMap, SizeOf(FRadioMap), 0);

  FRadioMap[vdfBaseDefault]          := rbNumDefault;
  FRadioMap[vdfBaseDecimal]          := rbNumDec;
  FRadioMap[vdfBasePointer]           := rbNumPtr;
  FRadioMap[vdfBaseHex]              := rbNumHex;
  FRadioMap[vdfBaseOct]               := rbNumOct;
  FRadioMap[vdfBaseBin]               := rbNumBin;

  FRadioMap[vdfSignDefault]          := rbSignDefault;
  FRadioMap[vdfSignSigned]           := rbSignSigned;
  FRadioMap[vdfSignUnsigned]         := rbSignUnsigned;

  FRadioMap[vdfNumCharDefault]       := rbNumCharDefault;
  FRadioMap[vdfNumCharOff]           := rbNumCharOff;
  FRadioMap[vdfNumCharOrdAndUnicode] := rbNumCharUniNum;
  FRadioMap[vdfNumCharOnlyUnicode]   := rbNumCharUni;

  FRadioMap[vdfEnumDefault]          := rbEnumDefault;
  FRadioMap[vdfEnumName]             := rbEnumName;
  FRadioMap[vdfEnumOrd]              := rbEnumOrd;
  FRadioMap[vdfEnumNameAndOrd]       := rbEnumNameAndOrd;

  FRadioMap[vdfBoolDefault]          := rbEnumDefault;
  FRadioMap[vdfBoolName]             := rbEnumName;
  FRadioMap[vdfBoolOrd]              := rbEnumOrd;
  FRadioMap[vdfBoolNameAndOrd]       := rbEnumNameAndOrd;

  FRadioMap[vdfCharDefault]          := rbEnumDefault;
  FRadioMap[vdfCharLetter]           := rbEnumName;
  FRadioMap[vdfCharOrd]              := rbEnumOrd;
  FRadioMap[vdfCharLetterAndOrd]     := rbEnumNameAndOrd;

  FRadioMap[vdfFloatDefault]         := rbFloatDefault;
  FRadioMap[vdfFloatPoint]           := rbFloatPoint;
  FRadioMap[vdfFloatScientific]      := rbFloatScience;

  FRadioMap[vdfStructDefault]        := rbStructDefault;
  FRadioMap[vdfStructValOnly]        := rbStructValOnly;
  FRadioMap[vdfStructFields]         := rbStructFields;
  FRadioMap[vdfStructFull]           := rbStructFull;

  FRadioMap[vdfStructAddressDefault] := rbStructAddrDefault;
  FRadioMap[vdfStructAddressOn]      := rbStructAddrOn;
  FRadioMap[vdfStructAddressOff]     := rbStructAddrOff;
  FRadioMap[vdfStructAddressOnly]    := rbStructAddrOnly;

  FRadioMap[vdfPointerDefault]       := rbPointerDefault;
  FRadioMap[vdfPointerAddress]       := rbPointerAddress;
  FRadioMap[vdfPointerTypedAddress]  := rbPointerTypedAddress;

  FRadioMap[vdfPointerDerefDefault]  := rbPointerDerefDefault;
  FRadioMap[vdfPointerDerefOff]      := rbPointerDerefOff;
  FRadioMap[vdfPointerDerefOn]       := rbPointerDerefOn;
  FRadioMap[vdfPointerDerefOnly]     := rbPointerDerefOnly;

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

  DividerBevelEnum.Caption          := DispFormatDlgBtnEnum +', ' + DispFormatDlgBtnBool +', ' + DispFormatDlgBtnChar;
  DividerBevelNum.Caption           := DispFormatDlgBtnNumber;
  DividerBevelNumChar.Caption       := DispFormatDlgBtnNumber;
  DividerBevelFloat.Caption         := DispFormatDlgBtnFloat;
  DividerBevelStruct.Caption        := DispFormatDlgBtnStruct;
  DividerBevelPointer.Caption       := DispFormatDlgBtnPointer;
  DividerBevelAddressFormat.Caption := DispFormatDlgBtnAdrFormat;
  DividerBevelMemDump.Caption       := '';

  rbNumDefault.Caption          := DispFormatBaseDefault;
  rbNumDec.Caption              := DispFormatBaseDecimal;
  rbNumPtr.Caption              := DispFormatBasePointer;
  rbNumHex.Caption              := DispFormatBaseHex;
  rbNumOct.Caption              := DispFormatBaseOct;
  rbNumBin.Caption              := DispFormatBaseBin;
  rbSignDefault.Caption         := DispFormatSignDefault + ' ' + DispFormatDlgCaptionSign;
  rbSignSigned.Caption          := DispFormatSignSigned;
  rbSignUnsigned.Caption        := DispFormatSignUnsigned;
  rbNumCharDefault.Caption      := DispFormatNumCharDefault + ' ' + DispFormatDlgCaptionShowChar;
  rbNumCharOff.Caption          := DispFormatNumCharOff;
  rbNumCharUniNum.Caption       := DispFormatNumCharOrdAndUnicode;
  rbNumCharUni.Caption          := DispFormatNumCharOnlyUnicode;

  rbEnumDefault.Caption         := DispFormatDlgCaptionDefault;
  s := DispFormatEnumName;
  AddToStr(s, DispFormatBoolName,   '/');
  AddToStr(s, DispFormatCharLetter, '/');
  rbEnumName.Caption            := s;
  rbEnumOrd.Caption             := DispFormatEnumOrd;
  rbEnumNameAndOrd.Caption      := DispFormatEnumNameAndOrd;

  rbFloatDefault.Caption        := DispFormatFloatDefault;
  rbFloatPoint.Caption          := DispFormatFloatPoint;
  rbFloatScience.Caption        := DispFormatFloatScientific;

  rbStructDefault.Caption       := DispFormatStructDefault;
  rbStructValOnly.Caption       := DispFormatStructValOnly;
  rbStructFields.Caption        := DispFormatStructFields;
  rbStructFull.Caption          := DispFormatStructFull;
  rbStructAddrDefault.Caption   := DispFormatStructAddressDefault + ' ' + DispFormatDlgCaptionAddress;
  rbStructAddrOn.Caption        := DispFormatStructAddressOff;
  rbStructAddrOff.Caption       := DispFormatStructAddressOn;
  rbStructAddrOnly.Caption      := DispFormatStructAddressOnly;

  rbPointerDefault.Caption      := DispFormatPointerDefault + ' ' + DispFormatDlgCaptionTyped;
  rbPointerAddress.Caption      := DispFormatPointerAddress;
  rbPointerTypedAddress.Caption := DispFormatPointerTypedAddress;
  rbPointerDerefDefault.Caption := DispFormatPointerDerefDefault + ' ' + DispFormatDlgCaptionDeref;
  rbPointerDerefOff.Caption     := DispFormatPointerDerefOff;
  rbPointerDerefOn.Caption      := DispFormatPointerDerefOn;
  rbPointerDerefOnly.Caption    := DispFormatPointerDerefOnly;

  rbPointerNumDefault.Caption   := DispFormatBaseDefault + ' ' +DispFormatDlgCaptionNumber;
  rbPointerNumHex.Caption       := DispFormatBaseHex;
  rbPointerNumDec.Caption       := DispFormatBaseDecimal;
  rbPointerNumOct.Caption       := DispFormatBaseOct;

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
        else begin
                           tbAll.Down := True;
                           tbNumber.Down  := True;
                           tbEnum.Down    := True;
                           tbBool.Down    := True;
                           tbChar.Down    := True;
                           tbFloat.Down   := True;
                           tbStruct.Down  := True;
                           tbPointer.Down := True;
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

