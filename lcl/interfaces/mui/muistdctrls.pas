unit MuiStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Contnrs, Exec, AmigaDos, Intuition, Utility, Mui, Forms,
  muidrawing, buttons, Math, Graphics,
  {$ifdef HASAMIGA}
  agraphics,
  cybergraphics,
  {$ifdef MorphOS}
  amigalib,
  {$endif}
  {$endif}
  MuiBaseUnit, tagsparamshelper, muiglobal,
  StdCtrls, muistringsunit, LCLMessageGlue, LMessages;

  { TMuiButton }
type

  TMuiButton = class(TMuiArea)
  public
    constructor Create(const Params: TAParamList); overload; reintroduce; virtual;
  end;

  TMuiBitBtn = class(TMuiArea)
  private
    FCaption: string;
  protected
    procedure SetCaption(const AValue: string); override;
    function GetCaption: string; override;
    procedure DoReDraw(); override;
  public
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    FBitmap: TMUIBitmap;
  end;

  { TMuiText }

  TMuiText = class(TMuiArea)
  public
    constructor Create(const Tags: TATagList); overload; reintroduce; virtual;
  end;

  { TMuiCheckMark }

  TMuiCheckMark = class(TMuiArea)
  protected
    FCheckWidth: Integer;
    FullWidth: Integer;
    CheckLabel: TMuiText;
    procedure SetParent(const AValue: TMUIObject); override;
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;
    procedure SetLeft(ALeft: Integer); override;
    procedure SetTop(ATop: Integer); override;
    procedure SetWidth(AWidth: Integer); override;
    procedure SetHeight(AHeight: Integer); override;
    function GetWidth(): Integer; override;
    procedure SetVisible(const AValue: Boolean); override;
    procedure SetColor(const AValue: TColor); override;
    procedure InstallHooks; override;
  public
    constructor Create(const Params: TAParamList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure SetOwnSize; override;
  end;

  { TMuiRadioButton }

  TMuiRadioButton = class(TMuiCheckMark)
  protected
    procedure SetChecked(const AValue: Boolean); override;
  public
    procedure MakeOneChecked;
    procedure RemoveCheck;
  end;


  { TMuiToggleButton }

  TMuiToggleButton = class(TMuiArea)
  protected
    procedure InstallHooks; override;
  public
    constructor Create(ObjType: LongInt; const Params: TAParamList); override;
  end;

  { TMuiStringEdit }

  TMuiStringEdit = class(TMuiArea)
  private
    EditHook: THook;
    FNumbersOnly: Boolean;
    FText: PChar;
    function GetText: string;
    procedure SetText(const AValue: string);
    function GetNumbersOnly: Boolean;
    procedure SetNumbersOnly(const AValue: Boolean);
  protected
    procedure InstallHooks; override;
  public
    constructor Create(const Tags: TATagList); overload; reintroduce; virtual;
    destructor Destroy; override;
    property Text:string read GetText write SetText;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly;
  end;

  { TMuiSpinEdit }

  TMuiSpinEdit = class(TMuiArea)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FDecimals: Integer;
    FIncrement: Double;
    //
    FText: PChar;
    BtnUp: PObject_;
    BtnDown: PObject_;
    UpDownPanel: PObject_;
    Edit: PObject_;
  protected
    function GetNumValue: Double;
    procedure SetNumValue(const AValue: Double);
    //
    procedure SetMinValue(const AValue: Double);
    procedure SetMaxValue(const AValue: Double);
    procedure SetIncrement(const AValue: Double);
    procedure SetDecimals(const AValue: Integer);
    function GetTabStop: boolean; override;
    procedure SetTabStop(const AValue: boolean); override;
    function GetFocusObject: PObject_; override;
    procedure InstallHooks; override;
  public
    constructor Create(const Tags: TATagList); overload; reintroduce; virtual;
    destructor Destroy; override;
    property CurValue: Double read GetNumValue write SetNumValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property Increment: Double read FIncrement write SetIncrement;
    property Decimals: Integer read FDecimals write SetDecimals;
  end;


  { TMuiCycle }

  TMuiCycle = class(TMuiArea)
  private
    FEditable: Boolean;
    StrObj: PObject_;
    BtnObj: PObject_;
    FStrings: TMuiStrings;
    StringPtrs: TStringPtrs;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    function GetText: string;
    procedure SetText(const AText: string);
    procedure ChangedItems(Sender: TObject);
  protected
    procedure InstallHooks; override;
  public
    constructor Create(ACaption: PChar; AStrings: TStrings; AEditable: Boolean); overload; reintroduce; virtual;
    Destructor Destroy; override;
    //
    property Strings: TMuiStrings read FStrings write FStrings;
    property Active: LongInt read GetActive write SetActive;
    property Editable: Boolean read FEditable;
    property Text: string read GetText write SetText;
  end;

  { TMuiTextEdit }

  TMuiTextEdit = class;

  { TFlowString }

  TFlowString = class(TStrings)
  private
    SL: TStringList;
    function GetMUIText: string;
  public
    FMuiObject: TMuiTextEdit;
    constructor Create;
    destructor Destroy; override;
    function GetCount: Integer; override;
    function Add(const S: String): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1: Integer; Index2: Integer); override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure LoadFromFile(const FileName: String); override;
  end;

  TMuiTextEdit = class(TMuiObject)
  private
    FText: PChar;
    FStrings: TFlowString;
    FTextObj: pObject_;
    procedure SetReadOnly(AReadOnly: Boolean);
    function GetReadOnly: Boolean;
  public
    constructor Create(AStrings: TStrings; const Tags: TATagList); overload; reintroduce; virtual;
    Destructor Destroy; override;
    property Strings: TFlowString read FStrings write FStrings;
    property TextObj: pObject_ read FTextObj write FTextObj;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
  end;

  { TMuiListView }

  TMuiListView = class(TMuiArea)
  private
    StrObj: PObject_;
    Texts: array of PChar;
    FStrings: TStringList;
    function GetActive: LongInt;
    procedure SetActive(const AValue: LongInt);
    procedure TextChanged(Sender: TObject);
  protected
    procedure InstallHooks; override;
  public
    constructor Create(AStrings:TStrings; const Tags: TATagList); overload; reintroduce; virtual;
    destructor Destroy; override;
    procedure SetOwnSize; override;
    property Strings: TStringList read FStrings;
    property Active: LongInt read GetActive write SetActive;
  end;

  { TMUIScrollbar }

  TMUIScrollBar = class(TMuiGroup)
  private
    FMinValue: Integer;
    FMaxValue: Integer;
    FPageSize: Integer;
    //FPosition: Integer;
    BlockScrollEvent: Boolean;
    function GetHoriz: Boolean;
    function GetMaxValue: Integer;
    function GetMinValue: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    procedure SetHoriz(AValue: Boolean);
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetPageSize(AValue: Integer);
    procedure SetPosition(AValue: Integer);
  protected
    procedure InstallHooks; override;
  public
    constructor Create(const Tags: TATagList); overload; reintroduce; virtual;

    property Horizontal: Boolean read GetHoriz write SetHoriz;
    property MinValue: Integer read GetMinValue write SetMinValue;
    property MaxValue: Integer read GetMaxValue write SetMaxValue;
    property Position: Integer read GetPosition write SetPosition;
    property PageSize: Integer read GetPageSize write SetPageSize;
  end;

  {TMUIGroupBox}

  TMUIGroupBox = class(TMUIGroup)
  protected
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;
  public
    FText: PChar;
    function GetClientRect: TRect; override;
    constructor Create(const Tags: TATagList); overload; reintroduce; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  LCLType;

{ TMUIScrollBar }

function TMUIScrollBar.GetHoriz: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_Group_Horiz));
end;

function TMUIScrollBar.GetMaxValue: Integer;
begin
  Result := FMaxValue;
end;

function TMUIScrollBar.GetMinValue: Integer;
begin
  Result := FMinValue;
end;

function TMUIScrollBar.GetPageSize: Integer;
begin
  Result := GetAttribute(MUIA_Prop_Visible);
end;

function TMUIScrollBar.GetPosition: Integer;
begin
  Result := GetAttribute(MUIA_Prop_First) + FMinValue;
  //SysDebugLn('LCL: GetPosition: ' + IntToStr(Result) + ' MinValue: ' + IntToStr(FMinValue));
end;

procedure TMUIScrollBar.SetHoriz(AValue: Boolean);
begin
  SetAttribute(MUIA_Group_Horiz, AValue);
end;

procedure TMUIScrollBar.SetMaxValue(AValue: Integer);
var
  Pos: Integer;
begin
  //sysdebugln('set MaxValue ' + IntToStr(AValue));
  if (AValue = FMaxValue) or (AValue <= FMinValue) then
    Exit;
  Pos := Position;
  FMaxValue := AValue;
  SetAttribute(MUIA_Prop_Entries, FMaxValue - FMinValue);
  SetAttribute(MUIA_Prop_Visible, FPageSize);
  Position := Pos;
end;

procedure TMUIScrollBar.SetMinValue(AValue: Integer);
var
  Pos: Integer;
begin
  if AValue = FMinValue then
    Exit;
  //sysdebugln('set MinValue ' + IntToStr(AValue));
  Pos := Position;
  FMinValue := AValue;
  SetAttribute(MUIA_Prop_Entries, FMaxValue - FMinValue);
  SetAttribute(MUIA_Prop_Visible, FPageSize);
  Position := Pos;
end;

procedure TMUIScrollBar.SetPageSize(AValue: Integer);
var
  Pos: Integer;
begin
  if AValue = FPageSize then
    Exit;
  //sysdebugln('set page size ' + IntToStr(AValue));
  Pos := Position;
  FPageSize := AValue;
  SetAttribute(MUIA_Prop_Entries, FMaxValue - FMinValue);
  SetAttribute(MUIA_Prop_Visible, FPageSize);
  Position := Pos;
end;

procedure TMUIScrollBar.SetPosition(AValue: Integer);
begin
  //sysDebugLn('LCL: set to '+ IntToStr(AValue) + ' Position is ' + IntToStr(Position) + ' MinValue: ' + IntToStr(FMinValue)+ ' MaxValue: ' + IntToStr(FMaxValue));
  if AValue <> Position then
  begin
    SetAttribute(MUIA_Prop_First, AValue - FMinValue);
  end;
end;

function ChangeScroll(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;
var
  Parent, MuiObject: TMuiObject;
  ScrollMsg: TLMVScroll;
  SendMsg: Boolean;
begin
  //debugln('--> Scroll hook');
  //Exit;
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    if TMUIScrollbar(MUIObject).Horizontal then
      ScrollMsg.Msg := LM_HSCROLL
    else
      ScrollMsg.Msg := LM_VScroll;
    ScrollMsg.Pos := TMUIScrollBar(MUIObject).Position;
    ScrollMsg.ScrollBar := PtrUInt(MuiObject);
    ScrollMsg.ScrollCode := SB_THUMBPOSITION;//SB_ENDSCROLL;
    if MuiObject.PasObject is TScrollbar then
    begin
      if TScrollbar(MuiObject.PasObject).Position <> ScrollMsg.Pos then
      begin
        TScrollbar(MuiObject.PasObject).Position := ScrollMsg.Pos;
        DeliverMessage(TControl(MuiObject.PasObject), ScrollMsg);
      end;
    end else
    begin
      Parent := MUIObject.Parent;
      SendMsg := False;
      ScrollMsg.Pos := TMUIScrollBar(MUIObject).Position;
      if (Parent.VScroll = MUIObject) then
      begin
        ScrollMsg.Msg := LM_VScroll;
        if ScrollMsg.Pos <> Parent.VScrollPos then
        begin
          Parent.VScrollPos := Scrollmsg.Pos;
          SendMsg := True;
        end;
      end;
      if (Parent.HScroll = MUIObject) then
      begin
        ScrollMsg.Msg := LM_HScroll;
        if ScrollMsg.Pos <> Parent.HScrollPos then
        begin
          Parent.HSCrollPos := Scrollmsg.Pos;
          SendMsg := True;
        end;
      end;
      if SendMsg then
        DeliverMessage(TControl(MuiObject.PasObject), ScrollMsg);
    end;
  end;
  //debugln('<-- Scroll hook');
end;


procedure TMUIScrollBar.InstallHooks;
begin
  inherited InstallHooks;
  ConnectHook(MUIA_Prop_First, LongWord(MUIV_EveryTime), @ChangeScroll);
end;

constructor TMUIScrollBar.Create(const Tags: TATagList);
begin
  FMinValue := 0;
  FMaxValue := 100;
  FPageSize := 10;
  BlockScrollEvent := False;
  inherited Create(MUIC_Scrollbar, Tags);
end;


{ TMuiRadioButton }

procedure TMuiRadioButton.SetChecked(const AValue: Boolean);
var
  i: Integer;
  RB: TMUIObject;
begin
  if AValue and Assigned(Parent) then
  begin
    for i := 0 to Parent.FChilds.Count - 1 do
    begin
      RB := TMUIObject(Parent.FChilds.Items[i]);
      if (RB is TMuiRadioButton) and (RB.obj <> Self.obj) then
      begin
        if TMuiRadioButton(RB).checked then
          TMuiRadioButton(RB).RemoveCheck;
      end;
    end;
  end;
  inherited SetChecked(AValue);
end;

procedure TMuiRadioButton.MakeOneChecked;
var
  i: Integer;
  RB: TObject;
begin
  if Assigned(Parent) then
  begin
    for i := 0 to Parent.FChilds.Count - 1 do
    begin
      RB := TMUIObject(Parent.FChilds.Items[i]);
      if (RB is TMuiRadioButton) then
      begin
        if TMuiRadioButton(RB).checked then
        begin
          Exit;
        end;
      end;
    end;
    Self.Checked := True;
  end;
end;

procedure TMuiRadioButton.RemoveCheck;
begin
  inherited SetChecked(False);
end;

procedure TMuiBitBtn.DoReDraw();
var
  GlyphPos: TPoint;
  TextPos: TPoint;
  Len, Hi: Integer;
  TextLength: Integer;
  GLeft, GWidth:Integer;
  GTop, GHeight: Integer;
  DHeight, DWidth: Integer;
  Ma, Spa: Integer;

  procedure GlyphLeft;
  var
    GlyTextWi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Len + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);

    if FMargin < 0 then
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.Y := ((DHeight - Hi) div 2) + 2;
      GlyTextWi := GWidth + FSpacing + Len; // Width of Glyph + Text + spacing
      GlyphPos.X := (DWidth - GlyTextWi) div 2;
      TextPos.X := GlyphPos.X + GWidth + FSpacing;
    end else
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      GlyphPos.X := FMargin;
      TextPos.Y := DHeight div 2 - ((Hi div 2) - 2);
      if FSpacing < 0 then
      begin
        TextPos.X := (DWidth - Len) div 2
      end else
        TextPos.X := GlyphPos.X + GWidth + FSpacing;
    end;
  end;

  procedure GlyphRight;
  var
    GlyTextWi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Len + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);

    if FMargin < 0 then
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.Y := ((DHeight - Hi) div 2) + 2;
      GlyTextWi := GWidth + FSpacing + Len; // Width of Glyph + Text + spacing
      TextPos.X := (DWidth - GlyTextWi) div 2;
      GlyphPos.X := TextPos.X + FSpacing + Len;
    end else
    begin
      GlyphPos.Y := DHeight div 2 - (GHeight div 2);
      TextPos.X := FMargin;
      TextPos.Y := DHeight div 2 - ((Hi div 2) - 2);
      if FSpacing < 0 then
      begin
        GlyphPos.X := (DWidth - GWidth) div 2
      end else
        GlyphPos.X := TextPos.X + Len + FSpacing;
    end;
  end;

  procedure GlyphTop;
  var
    GlyTextHi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Hi + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);
    if FMargin < 0 then
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.X := ((DWidth - Len) div 2) + 2;
      GlyTextHi := GHeight + FSpacing + Hi; // Width of Glyph + Text + spacing
      GlyphPos.Y := (DHeight - GlyTextHi) div 2;
      TextPos.Y := GlyphPos.Y + GHeight + FSpacing + 2;
    end else
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      GlyphPos.Y := FMargin;
      TextPos.X := DWidth div 2 - (Len div 2);
      if FSpacing < 0 then
      begin
        TextPos.Y := ((DHeight - Hi) div 2) + 2
      end else
        TextPos.Y := GlyphPos.Y + GHeight + FSpacing + 2;
    end;
  end;

  procedure GlyphBottom;
  var
    GlyTextHi: Integer;
  begin
    GWidth := Min(FBitmap.FWidth, DWidth - ((Ma * 2) + Spa));
    GHeight := Min(FBitmap.FHeight, DHeight - ((Ma * 2) + Hi + Spa));
    GLeft := Max(0,(FBitmap.FWidth - GWidth) div 2);
    GTop := Max(0, (FBitmap.FHeight - GHeight) div 2);
    if FMargin < 0 then
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.X := ((DWidth - Len) div 2);
      GlyTextHi := GHeight + FSpacing + Hi; // Width of Glyph + Text + spacing
      TextPos.Y := ((DHeight - GlyTextHi) div 2) + (Hi div 2) + 2;
      GlyphPos.Y := TextPos.Y + FSpacing + Hi div 2;
    end else
    begin
      GlyphPos.X := DWidth div 2 - (GWidth div 2);
      TextPos.Y := FMargin + 2;
      TextPos.X := DWidth div 2 - (Len div 2);
      if FSpacing < 0 then
      begin
        GlyphPos.Y := (DHeight - GHeight) div 2
      end else
        GlyphPos.Y := TextPos.Y + Hi + FSpacing;
    end;
  end;

begin
  inherited;
  if Assigned(MUICanvas) then
  begin
    Ma := Max(4, FMargin);
    Spa:= Max(4, FSpacing);
    DHeight := MUICanvas.DrawRect.Bottom - MUICanvas.DrawRect.Top;
    DWidth := MUICanvas.DrawRect.Right - MUICanvas.DrawRect.Left;
    SetDrMd(MUICanvas.RastPort, JAM1);
    TextLength := Length(FCaption);
    Hi := MUICanvas.TextHeight(PChar(FCaption), TextLength);
    Len := MUICanvas.TextWidth(PChar(FCaption), TextLength);
    TextPos.Y := (MUICanvas.DrawRect.Bottom - MUICanvas.DrawRect.Top) div 2 - ((Hi div 2) - 2);
    TextPos.X := ((MUICanvas.DrawRect.Right - MUICanvas.DrawRect.Left) - Len) div 2;
    if Assigned(FBitmap) then
    begin
      case FLayout of
        blGlyphLeft: GlyphLeft;
        blGlyphRight: GlyphRight;
        blGlyphTop: GlyphTop;
        blGlyphBottom: GlyphBottom;
      end;
      {$ifndef AMIGA}
      WritePixelArrayAlpha(FBitmap.FImage, GLeft, GTop, FBitmap.FWidth * SizeOf(LongWord), MUICanvas.RastPort, MUICanvas.GetOffset.X + GlyphPos.X, MUICanvas.GetOffset.Y + GlyphPos.Y, GWidth, GHeight, $FFFFFFFF);
      {$endif}
    end;
    MUICanvas.MoveTo(TextPos.X, TextPos.Y);
    MUICanvas.WriteText(PChar(FCaption), TextLength);
  end;
end;

procedure TMuiBitBtn.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

function TMuiBitBtn.GetCaption: string;
begin
  Result := FCaption;
end;

function ListChangeFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiListView;
  Idx: Integer;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiListView then
  begin
    MuiObject := TMuiListView(Hook^.h_Data);
    Idx := MuiObject.Active;
    // LCLSendMouseDownMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    if Idx > 0 then
      LCLSendChangedMsg(TControl(MuiObject.PasObject), Idx);
    //LCLSendMouseUpMsg(TControl(MuiObject.PasObject), 0,0, mbLeft, []);
    LCLSendClickedMsg(TControl(MuiObject.PasObject));
  end;
end;

function DoubleClickFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiListView;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiListView then
  begin
    MuiObject := TMuiListView(Hook^.h_Data);
    LCLSendMouseMultiClickMsg(TControl(MuiObject.PasObject), 0, 0, mbLeft, 2);
  end;
end;


{ TMuiListView }

constructor TMuiListView.Create(AStrings:TStrings; const Tags: TATagList);
var
  StrTags: TATagList;
begin
  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  StrTags.Clear;
  StrObj := MUI_NewObjectA(MUIC_List, StrTags);
  Tags.AddTags([MUIA_Listview_List, PtrUInt(StrObj)]);
  inherited Create(MUIC_ListView, Tags);
  FStrings.OnChange := @TextChanged;
end;

destructor TMuiListView.Destroy;
var
  i: Integer;
begin
  FStrings.Free;
  for i := 0 to High(Texts) do
    System.FreeMem(Texts[i]);
  inherited Destroy;
end;

procedure TMuiListView.InstallHooks;
begin
  inherited;
  ConnectHook(MUIA_List_Active, LongWord(MUIV_EveryTime), @ListChangeFunc);
  ConnectHook(MUIA_ListView_DoubleClick, LongWord(True), @DoubleClickFunc);
end;

procedure TMuiListView.TextChanged(Sender: TObject);
var
  str: String;
  i: Integer;
begin
  if Assigned(FStrings) then
  begin
    DoMethodObj(StrObj, [MUIM_List_Clear]);
    for i := 0 to High(Texts) do
      System.FreeMem(Texts[i]);
    SetLength(Texts, FStrings.Count + 1);
    for i := 0 to FStrings.Count - 1 do
    begin
      str := FStrings[i] + #0;
      Texts[i] := System.AllocMem(Length(str));
      Move(str[1], Texts[i]^, Length(str));
    end;
    Texts[FStrings.Count] := nil;
    DoMethodObj(StrObj, [PtrUInt(MUIM_List_Insert), PtrUInt(@(Texts[0])), FStrings.Count, PtrUInt(MUIV_List_Insert_Bottom)]);
    DoMethodObj(StrObj, [PtrUInt(MUIM_List_Redraw), PtrUInt(MUIV_List_Redraw_All)]);
  end;
end;

function TMuiListView.GetActive: LongInt;
var
  Res: NativeUInt;
begin
  Result := 0;
  GetAttr(NativeUInt(MUIA_List_Active), StrObj, @Res);
  if Res = MUIV_List_Active_Off then
    Result := 0
  else
  begin
    if (Res >= 0) and (Res < Strings.Count) then
      Result := Res
    else
    begin
      if (Res < 0) then
        Result := 0
      else
        Result := Strings.Count - 1;
      SetActive(Result);
    end;
  end
end;

procedure TMuiListView.SetActive(const AValue: LongInt);
var
  Res: LongInt;
  Res1: NativeUInt;
  TagList: TATagList;
begin
  if AValue = -1 then
    Res := MUIV_List_Active_Off
  else
    Res := AValue;
  GetAttr(NativeUInt(MUIA_List_Active), StrObj, @Res1);
  if LongInt(Res1) <> Res then
  begin
    TagList.AddTags([
      MUIA_List_Active, Res
      ]);
    SetAttrsA(StrObj, TagList);
  end;
end;

procedure TMuiListView.SetOwnSize;
begin
  //writeln('Listview set own size: ', FWidth);
  inherited;
  //MUI_Layout(FObject, FLeft, FTop, FHeight, FHeight, 0);
  //MUI_Layout(StrObj, FLeft, FTop, FWidth, FHeight, 0);

  //MUI_Layout(StrObj, FLeft, FTop, FWidth, FHeight, 0);
end;

{ TMuiButton }

constructor TMuiButton.Create(const Params: TAParamList);
begin
  inherited Create(MUIO_Button, Params);
end;

{ TMuiText }

constructor TMuiText.Create(const Tags: TATagList);
begin
  //AddTags(Tags, [LongInt(MUIA_BACKGROUND), MUII_BACKGROUND]);
  //AddTags(Tags, [LongInt(MUIA_BACKGROUND), MUII_FILLSHINE]);
  inherited Create(MUIC_Text, Tags);
end;

{ TMuiCheckMark }

function CheckFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiObject;
  SendMessages: Boolean;
begin
  Result := 0;
  SendMessages := True;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    if MUIObject is TMUIArea then
    begin
      if TMUIArea(MUIObject).FBlockChecked then
        SendMessages := False;
    end;
    if MuiObject is TMUIRadioButton then
    begin
      if TMUIRadioButton(MUIObject).Checked then
      begin
        TMUIRadioButton(MUIObject).Checked := True;
      end else
      begin
        TMUIRadioButton(MUIObject).MakeOneChecked;
      end;
    end;
    if SendMessages then
      LCLSendChangedMsg(TControl(MuiObject.PasObject), 0);
  end;
end;

constructor TMuiCheckMark.Create(const Params: TAParamList);
var
  Tags: TATagList;
  ObjType: LongInt;
begin
  if self is TMUIRadioButton then
  begin
    FCheckWidth := 16;
    ObjType := MUIO_Radio
  end
  else
  begin
    FCheckWidth := 20;
    ObjType := MUIO_Checkmark;
  end;
  if ObjType = MUIO_Radio then
  begin
    Tags.AddTags([
      MUIA_InputMode, MUIV_InputMode_Immediate,
      MUIA_ShowSelState, TagFalse,
      MUIA_Image_Spec, MUII_RadioButton
      ]);
    inherited Create(MUIC_Image, Tags);
  end else
    inherited Create(ObjType, Params);
  Tags.Clear;
  CheckLabel := TMuiText.Create(Tags);
  //CheckLabel.Visible := False;
end;

destructor TMuiCheckMark.Destroy;
begin
  CheckLabel.Free;
  CheckLabel := Nil;
  inherited;
end;

procedure TMuiCheckMark.InstallHooks;
begin
  inherited;
  ConnectHook(MUIA_Selected, LongWord(MUIV_EveryTime), @CheckFunc);
end;

function TMuiCheckMark.GetCaption: string;
begin
  Result := '';
  if Assigned(CheckLabel) then
    Result := CheckLabel.Caption;
end;

procedure TMuiCheckMark.SetCaption(const AValue: string);
begin
  if Assigned(CheckLabel) then
  begin
    CheckLabel.Caption := AValue;
    CheckLabel.Visible := (AValue <> '') and Visible;
  end;
end;

procedure TMuiCheckMark.SetParent(const AValue: TMUIObject);
begin
  inherited;
  if Assigned(CheckLabel) then
    CheckLabel.Parent := AValue;
end;

procedure TMuiCheckMark.SetLeft(ALeft: Integer);
begin
  inherited;
  if Assigned(CheckLabel) then
  begin
    CheckLabel.Left := Left + Height + 2;
  end;
end;

procedure TMuiCheckMark.SetTop(ATop: Integer);
begin
  inherited;
  if Assigned(CheckLabel) then
    CheckLabel.Top := Top + (FCheckWidth  div 2 - CheckLabel.Height div 2);
end;

procedure TMuiCheckMark.SetWidth(AWidth: Integer);
begin
  FullWidth := AWidth;
  inherited SetWidth(FCheckWidth);
  if Assigned(CheckLabel) and (CheckLabel.Visible) then
  begin
    CheckLabel.Left := Left + FCheckWidth + 2;
    CheckLabel.Width := FullWidth - (FCheckWidth + 2);
  end;
end;

procedure TMuiCheckMark.SetHeight(AHeight: Integer);
begin
  inherited SetHeight(FCheckWidth);
  SetWidth(FullWidth);
  if Assigned(CheckLabel) and (CheckLabel.Visible) then
    CheckLabel.Height := Height;
end;

function TMuiCheckMark.GetWidth(): Integer;
begin
  Result := FullWidth;
end;

procedure TMuiCheckMark.SetOwnSize;
var
  w: Integer;
  MinMax: TMUI_MinMax;
begin
  // try to get current Width from the Item
  w := GetAttribute(MUIA_Width);
  // ups Width is zero -> inital Size so we ask for min max
  if w = 0 then
  begin
    // ask the checkmark for min max
    FillChar(MinMax, SizeOf(MinMax), 0);
    DoMethod([MUIM_AskMinMax, NativeUInt(@MinMax)]);
    // in principle we only want the width
    w := MinMax.DefWidth;
  end;
  // better recheck if the width and height is really set
  if w > 0 then
  begin
    // strange on Amiga the height is sometimes the Height = 0
    // sadly read the MinMax does not work always return 0, so we just put a default here
    if CheckLabel.Height = 0 then
      CheckLabel.Height := 16;
    FCheckWidth := w;
    // set the coords for the label
    if Assigned(CheckLabel) and (CheckLabel.Visible) then
    begin
      CheckLabel.Left := Left + FCheckWidth + 2;
      CheckLabel.Width := FullWidth - (FCheckWidth + 2);
      CheckLabel.Top := Top + w  div 2 - CheckLabel.Height div 2;
      // lets do it :-)
      CheckLabel.SetOwnSize;
    end;
  end;
  // let the checkmark set its size
  inherited;
end;

procedure TMuiCheckMark.SetVisible(const AValue: Boolean);
begin
  inherited;
  if Assigned(CheckLabel) then
    CheckLabel.Visible := AValue and (Caption <> '');
end;

procedure TMuiCheckMark.SetColor(const AValue: TColor);
begin
  CheckLabel.Color := AValue;
end;

{ TMuiToggleButton }

constructor TMuiToggleButton.Create(ObjType: LongInt; const Params: TAParamList);
begin
  inherited Create(MUIO_Button, Params);
  SetAttribute(MUIA_InputMode, MUIV_InputMode_Toggle);
end;

procedure TMuiToggleButton.InstallHooks;
begin
  inherited;
  ConnectHook(MUIA_Selected, LongWord(MUIV_EveryTime), @CheckFunc);
end;

{ TMuiStringEdit }

function TextDoneFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiObject;
  CharCode: Word;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    if MUIObject is TMUISpinEdit then
    begin
      TMUISpinEdit(MUIObject).CurValue := TMUISpinEdit(MUIObject).CurValue;
    end;
    CharCode := VK_RETURN;
    LCLSendKeyDownEvent(MUIObject.PasObject, CharCode, 0, True, False);
    LCLSendKeyUpEvent(MUIObject.PasObject, CharCode, 0, True, False);
  end;
end;


function TextChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiObject;
begin
  Result := 0;
  //writeln('edit text changed');
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    MuiObject := TMuiObject(Hook^.h_Data);
    SendSimpleMessage(MuiObject.PasObject, CM_TEXTCHANGED);
  end;
end;

function TextEditFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
//var
//  MuiObject: TMuiObject;
begin
  //writeln('edit text Event');
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiObject then
  begin
    //MuiObject := TMuiObject(Hook^.h_Data);
    //SendSimpleMessage(MuiObject.PasObject, CM_TEXTCHANGED);
  end;
end;

function TMuiStringEdit.GetText: string;
var
  Pc: PChar;
begin
  Result := '';
  if Obj = nil then
    Exit;
  Pc := PChar(GetAttribute(MUIA_String_Contents));
  if Assigned(PC) then
    Result := string(Pc);
end;

procedure TMuiStringEdit.SetText(const AValue: string);
begin
  if AValue <> GetText then
  begin
    FreeMem(FText);
    FText := System.AllocMem(Length(AValue) + 1);
    Move(AValue[1], FText^, Length(AValue));
    SetAttribute(MUIA_String_Contents, FText);
  end;
end;

constructor TMuiStringEdit.Create(const Tags: TATagList);
//var
//  p: Pointer;
begin
  SetHook(EditHook, @TextEditFunc, Self);
  //EditHook.h_Entry := NativeUInt(@TextEditFunc);
  //EditHook.h_SubEntry := 0;
  //EditHook.h_Data := Self;
  //P := @EditHook;
  // Edithook does not work currently
  Tags.AddTags([
    MUIA_Background, MUII_TextBack,
    MUIA_Font, NativeUInt(MUIV_Font_Button),
    MUIA_Frame, MUIV_Frame_String
    //,
    //MUIA_String_EditHook, P,
    //MUIA_String_LonelyEditHook, 1
  ]);
  inherited Create(MUIC_String, Tags);
  //
  FNumbersOnly := False;
  FText := System.AllocMem(2048);

end;

destructor TMuiStringEdit.Destroy;
begin
  FreeMem(FText);
  inherited;
end;

procedure TMuiStringEdit.InstallHooks;
begin
  inherited;
  ConnectHook(MUIA_String_Contents, LongWord(MUIV_EveryTime), @TextChangedFunc);
  ConnectHook(MUIA_String_Acknowledge, LongWord(MUIV_EveryTime), @TextDoneFunc);
end;

function TMuiStringEdit.GetNumbersOnly: Boolean;
begin
  Result := FNumbersOnly;
end;

var
  IntegerChars: string = '0123456789-';
  FloatChars: string = '0123456789-.,';
  NoChars: string = '';
procedure TMuiStringEdit.SetNumbersOnly(const AValue: Boolean);
var
  StrTxt: String;
begin
  if FNumbersOnly = AValue then
    Exit;
  FNumbersOnly := AValue;
  if FNumbersOnly then
  begin
    StrTxt := GetText;
    SetAttribute(MUIA_String_Integer, StrToIntDef(StrTxt, 0));
    SetAttribute(MUIA_String_Accept, PChar(IntegerChars));
  end else
  begin
    SetAttribute(MUIA_String_Accept, PChar(NoChars));
  end;
end;


{ TMuiSpinEdit }

function BtnDownClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;
var
  MuiSpin: TMuiSpinEdit;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiSpinEdit then
  begin
    MuiSpin := TMuiSpinEdit(Hook^.h_Data);
    MuiSpin.CurValue := MuiSpin.CurValue - MuiSpin.Increment;
  end;
end;

function BtnUpClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): LongInt;
var
  MuiSpin: TMuiSpinEdit;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiSpinEdit then
  begin
    MuiSpin := TMuiSpinEdit(Hook^.h_Data);
    MuiSpin.CurValue := MuiSpin.CurValue + MuiSpin.Increment;
  end;
end;

function TMuiSpinEdit.GetNumValue: Double;
var
  PC: PChar;
  strValue: string;
begin
  Result := 0;
  if Assigned(Edit) then
  begin
    PC := PChar(GetAttObj(Edit, MUIA_String_Contents));
    if Assigned(PC) then
    begin
      // we accept , and . :-P but set it to the system set DECIMALSEPARATOR
      strValue := StringReplace(string(PC), ',', FormatSettings.DECIMALSEPARATOR, [rfReplaceAll]);
      strValue := StringReplace(strValue, '.', FormatSettings.DECIMALSEPARATOR, [rfReplaceAll]);
      Result := StrToFloatDef(string(PC), 0);
      Result := Min(FMaxValue, Max(FMinValue, Result));
    end;
  end;
end;

procedure TMuiSpinEdit.SetNumValue(const AValue: Double);
var
  StrValue: String;
  Val: Double;
begin
  Val := Min(FMaxValue, Max(FMinValue, AValue));
  StrValue := FloatToStrF(Val, ffFixed, 8, FDecimals);
  FillChar(FText^, Length(StrValue) + 2, 0);
  Move(StrValue[1], FText^, Length(strValue));
  SetAttObj(Edit, [NativeUInt(MUIA_String_Contents), NativeUInt(PChar(FText))]);
  SetAttObj(Edit, [NativeUInt(MUIA_String_BufferPos), Length(FText)]);
end;

constructor TMuiSpinEdit.Create(const Tags: TATagList);
var
  GrpTags: TATagList;
  BtnUpTags: TATagList;
  BtnDownTags: TATagList;
  BtnGroupTags: TATagList;
  EditTags: TATagList;
begin
  FIncrement := 1;
  FMinValue := -1e308;
  FMaxValue := 1e308;
  FDecimals := 2;
  // BUTTON DOWN  ##################################
  FText := System.AllocMem(100);
  BtnUpTags.AddTags([
    MUIA_InputMode, MUIV_InputMode_RelVerify,
    MUIA_ShowSelState, NativeUInt(True),
    //MUIA_Frame, MUIV_Frame_ImageButton,
    MUIA_InnerLeft, 0, MUIA_InnerRight, 0,
    MUIA_InnerTop, 0, MUIA_InnerBottom, 0,
    MUIA_Image_Spec, MUII_ArrowUp
    ]);
  btnUp := MUI_NewObjectA(MUIC_Image, BtnUpTags.GetTagPointer);

  // BUTTON UP #######################################
  BtnDownTags.AddTags([
    MUIA_InputMode, MUIV_InputMode_RelVerify,
    MUIA_ShowSelState, NativeUInt(True),
    //MUIA_Frame, MUIV_Frame_ImageButton,
    MUIA_InnerLeft, 0, MUIA_InnerRight, 0,
    MUIA_InnerTop, 0, MUIA_InnerBottom, 0,
    MUIA_Image_Spec, MUII_ArrowDown
    ]);
  btndown := MUI_NewObjectA(MUIC_Image, BtnDownTags.GetTagPointer);

  // BUTTON GROUP ####################################
  BtnGroupTags.AddTags([
    MUIA_Background, MUII_TextBack,
    MUIA_Group_Child, NativeUInt(BtnUp),
    MUIA_Group_Child, NativeUInt(BtnDown),
    MUIA_InnerLeft, 0, MUIA_InnerRight, 0,
    MUIA_InnerTop, 0, MUIA_InnerBottom, 0,
    MUIA_Group_Spacing, 0,
    MUIA_Group_Horiz, NativeUInt(LTrue)
    //MUIA_Group_Horiz, NativeUInt(False)
    ]);

  UpDownPanel := MUI_NewObjectA(MUIC_Group, BtnGroupTags.GetTagPointer);
  //
  // Editor ###########################################
  EditTags.AddTags([
    MUIA_String_Format, MUIV_String_Format_Right,
    MUIA_Background, MUII_TextBack,
    MUIA_Frame, MUIV_Frame_String,
    MUIA_Font, NativeUInt(MUIV_Font_Fixed),
    MUIA_Group_Spacing, 0,
    MUIA_String_MaxLen, 100,
    MUIA_String_Accept, NativeUInt(PChar(FloatChars))
  ]);
  Edit := MUI_NewObjectA(MUIC_String, EditTags.GetTagPointer);
  //
  // Group #############################################
  GrpTags.AddTags([
    MUIA_InnerLeft, 0, MUIA_InnerRight, 0,
    MUIA_InnerTop, 0, MUIA_InnerBottom, 0,
    MUIA_Group_Spacing, 0,
    MUIA_Group_Child, NativeUInt(Edit),
    MUIA_Group_Child, NativeUInt(UpDownPanel),
    MUIA_Frame, MUIV_Frame_string,
    MUIA_Group_Horiz, NativeUInt(True)
    ]);
  inherited Create(MUIC_Group, GrpTags);
end;

destructor TMuiSpinEdit.Destroy;
begin
  System.FreeMem(FText);
  inherited;
end;

procedure TMuiSpinEdit.InstallHooks;
begin
  inherited;
  ConnectHookObject(btnUp, MUIA_Timer, LongWord(MUIV_EveryTime), @BtnUpClickFunc);
  ConnectHookObject(btnDown, MUIA_Timer, LongWord(MUIV_EveryTime), @BtnDownClickFunc);
  //
  ConnectHookObject(Edit, MUIA_String_Contents, LongWord(MUIV_EveryTime), @TextChangedFunc);
  ConnectHookObject(Edit, MUIA_String_Acknowledge, LongWord(MUIV_EveryTime), @TextDoneFunc);
end;

procedure TMuiSpinEdit.SetMinValue(const AValue: Double);
begin
  if FMinValue = AValue then
    Exit;
  FMinValue := AValue;
  if CurValue < FMinValue then
    CurValue := FMinValue;
end;

procedure TMuiSpinEdit.SetMaxValue(const AValue: Double);
begin
  if FMaxValue = AValue then
    Exit;
  FMaxValue := AValue;
  if CurValue > FMaxValue then
    CurValue := FMaxValue;
end;

procedure TMuiSpinEdit.SetIncrement(const AValue: Double);
begin
  FIncrement := AValue;
end;

procedure TMuiSpinEdit.SetDecimals(const AValue: Integer);
begin
  if FDecimals = AValue then
    Exit;
  if FDecimals = 0 then
  begin
    if AValue <> 0 then
      SetAttObj(Edit, [MUIA_String_Accept, NativeUInt(PChar(FloatChars))]);
  end else
  begin
    if AValue = 0 then
      SetAttObj(Edit, [MUIA_String_Accept, NativeUInt(PChar(IntegerChars))]);
  end;
  FDecimals := AValue;
  CurValue := CurValue;
end;

function TMuiSpinEdit.GetTabStop: boolean;
begin
  Result := GetAttObj(Edit, NativeUInt(MUIA_CycleChain)) <> 0;
end;

procedure TMuiSpinEdit.SetTabStop(const AValue: boolean);
var
  Val: Integer;
begin
  if AValue then
    Val := 1
  else
    Val := 0;
  SetAttObj(Edit, [MUIA_CycleChain, Val]);
  SetAttribute(MUIA_CycleChain, 0);
end;

function TMuiSpinEdit.GetFocusObject: PObject_;
begin
  Result := Edit;
end;


{ TMuiCycle }

function ActiveItemChangedFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiCycle;
  ItemIndex: Integer;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiCycle then
  begin
    MuiObject := TMuiCycle(Hook^.h_Data);
    ItemIndex := MuiObject.Active;
    LCLSendChangedMsg(MuiObject.PasObject, ItemIndex);
  end;
end;

function TextEnteredFunc(Hook: PHook; Obj: PObject_; Msg:Pointer): LongInt;
var
  MuiObject: TMuiCycle;
  ItemIndex: Integer;
begin
  Result := 0;
  if TObject(Hook^.h_Data) is TMuiCycle then
  begin
    MuiObject := TMuiCycle(Hook^.h_Data);
    ItemIndex := MuiObject.Active;
    if MuiObject.Editable then
    begin
      if ItemIndex < 0 then
        Exit;
    end;
    LCLSendChangedMsg(MuiObject.PasObject, ItemIndex);
  end;
end;

function TMuiCycle.GetActive: LongInt;
var
  str: string;
begin
  if FEditable then
  begin
    str := PChar(GetAttObj(StrObj, MUIA_String_Contents));
    Result := FStrings.IndexOf(str);
  end else
  begin
    Result := LongInt(GetAttribute(MUIA_Cycle_Active));
  end;
end;

procedure TMuiCycle.SetActive(const AValue: LongInt);
begin
  if FEditable then
  begin
    SetAttObj(StrObj, [MUIA_String_Contents, NativeUInt(PChar(FStrings[AValue]))]);
  end else
  begin
    SetAttribute(MUIA_Cycle_Active, AValue);
  end;
end;


function TMuiCycle.GetText: string;
var
  Idx: Integer;
begin
  Result := '';
  if FEditable then
  begin
    Result := PChar(GetAttObj(StrObj, MUIA_String_Contents));
  end else
  begin
    Idx := GetActive;
    if (Idx >= 0) and (Idx < FStrings.Count) then
    begin
      Result := FStrings[Idx];
    end;
  end;
end;

procedure TMuiCycle.SetText(const AText: string);
var
  Idx: Integer;
begin
  if FEditable then
  begin
    SetAttObj(StrObj, [MUIA_String_Contents, NativeUInt(PChar(AText))]);
  end else
  begin
    Idx := FStrings.IndexOf(AText);
    if (Idx >= 0) and (Idx < FStrings.Count) then
      SetActive(Idx);
  end;
end;


procedure TMuiCycle.ChangedItems(Sender: TObject);
begin
  //sysdebugln('recreate Cycle ' + HexStr(Self));
  // on change -> recreate the combobox (items only set on initialization in MUI)
  RecreateWnd(TWinControl(PasObject));
end;

constructor TMuiCycle.Create(ACaption: PChar; AStrings: TStrings; AEditable: Boolean);
var
  str: string;
  Len: Integer;
  i: LongInt;
  ListTags: TATagList;
  BtnTags: TATagList;
  StrTags: TATagList;
  Params: TAParamList;
begin
  FEditable := AEditable;
  //
  FStrings := TMuiStrings.create;
  if AStrings.Count = 0 then
  begin
    SetLength(StringPtrs, 2);
    str := ' ' + #0;
    FStrings.Add(str);
    Len := Length(Str);
    StringPtrs[0] := System.AllocMem(Len + 1);
    Move(Str[1], StringPtrs[0]^, Len);
  end
  else
  begin
    SetLength(StringPtrs, AStrings.Count + 1);
    for i:= 0 to AStrings.Count - 1 do
    begin
      str := AStrings.strings[i] + #0;
      FStrings.Add(str);
      Len := Length(Str);
      StringPtrs[i] := System.AllocMem(Len + 1);
      Move(Str[1], StringPtrs[i]^, Len);
    end;
  end;
  StringPtrs[High(StringPtrs)] := nil;
  if FEditable then
  begin
    BtnTags.AddTags([
      MUIA_InputMode, MUIV_InputMode_RelVerify,
      MUIA_ShowSelState, TagTrue,
      MUIA_Frame, MUIV_Frame_Button,
      MUIA_Image_Spec, MUII_PopUp
    ]);
    BtnObj := MUI_NewObjectA(MUIC_Image, BtnTags.GetTagPointer);

    StrTags.AddTags([
      MUIA_Frame, MUIV_Frame_String
    ]);
    StrObj := MUI_NewObjectA(MUIC_String, StrTags.GetTagPointer);

    ListTags.AddTags([
      MUIA_Popstring_String, NativeUInt(StrObj),
      MUIA_Popstring_Button, NativeUInt(BtnObj),
      MUIA_PopList_Array, NativeUInt(@(StringPtrs[0]))
      ]);

    inherited Create(MUIC_PopList, ListTags);
  end else
  begin
    Params.SetParams([NativeUInt(PChar(ACaption)), NativeUInt(@(StringPtrs[0]))]);
    inherited Create(MUIO_Cycle, Params);
  end;
  FStrings.OnChange := @ChangedItems;
end;

Destructor TMuiCycle.Destroy;
begin
  inherited;
  FStrings.Free;
end;

procedure TMuiCycle.InstallHooks;
begin
  inherited;
  if FEditable then
  begin
    ConnectHookObject(StrObj, MUIA_String_Contents, LongWord(MUIV_EveryTime), @TextEnteredFunc);
    ConnectHookObject(StrObj, MUIA_String_Acknowledge, LongWord(MUIV_EveryTime), @ActiveItemChangedFunc);
  end else
  begin
    // event for item changed
    ConnectHook(MUIA_Cycle_Active, LongWord(MUIV_EveryTime), @ActiveItemChangedFunc);
  end;
end;

{ TMuiTextEdit }

const
  TextEditor_Dummy = $ad000000;
  MUIA_TextEditor_Contents = TextEditor_Dummy + $2;
  MUIA_TextEditor_CursorX = TextEditor_Dummy + $4;
  MUIA_TextEditor_CursorY = TextEditor_Dummy + $5;
  MUIA_TextEditor_ReadOnly = TextEditor_Dummy + $19;
  MUIA_TextEditor_Slider = TextEditor_Dummy + $1a;
  MUIM_TextEditor_ClearText = TextEditor_Dummy + $24;
  MUIM_TextEditor_ExportText = TextEditor_Dummy + $25;
  MUIM_TextEditor_InsertText = TextEditor_Dummy + $26;

  MUIV_TextEditor_InsertText_Cursor = 0;
  MUIV_TextEditor_InsertText_Top = 1;
  MUIV_TextEditor_InsertText_Bottom = 2;

constructor TMuiTextEdit.Create(AStrings: TStrings; const Tags: TATagList);
var
  Scroll: PObject_;
  CreateTags: TATagList;
begin
  FStrings := TFlowString.Create;
  FStrings.FMuiObject := Self;
  //
  FTextObj := MUI_NewObjectA(PChar('TextEditor.mcc'), Tags.GetTagPointer);
  //
  Tags.Clear;
  Scroll := MUI_NewObjectA(MUIC_ScrollBar, Tags.GetTagPointer);
  //
  CreateTags.AddTags([
    MUIA_Group_Horiz, TagTrue,
    MUIA_Group_Child, NativeUInt(FTextObj),
    MUIA_Group_Child, NativeUInt(Scroll)
    ]);
  inherited Create(MUIC_Group, CreateTags);
  SetAttObj(FTextObj, [MUIA_TextEditor_Slider, NativeUInt(scroll)]);
  //Create(PChar('TextEditor.mcc'), Tags);
  FText := AStrings.GetText;
  SetAttribute(MUIA_TextEditor_Contents, NativeUInt(FText));
end;

Destructor TMuiTextEdit.Destroy;
begin;
  FStrings.FMUIObject := nil;
  FStrings.Free;
  inherited;
end;



procedure TMuiTextEdit.SetReadOnly(AReadOnly: Boolean);
begin
  SetAttribute(MUIA_TextEditor_ReadOnly, AReadOnly);
end;

function TMuiTextEdit.GetReadOnly: Boolean;
begin
  Result := Boolean(GetAttribute(MUIA_TextEditor_ReadOnly));
end;

{ TFlowString }

constructor TFlowString.Create;
begin
  inherited;
  SL := TStringList.Create;
end;

destructor TFlowString.Destroy;
begin
  SL.Free;
  inherited;
end;

function TFlowString.GetMUIText: string;
var
  PC: PChar;
  //Param: array[0..2] of DWord;
begin
  PC := nil;
  Result := '';
  if Assigned(FMuiObject) then
  begin
    //sysdebugln('-->GetMUIText: ');
    //Param[0] := MUIM_TextEditor_ExportText; //$ad000025;
    //Param[1] := 0;
    //Param[2] := 0;
    //PC := PChar(AmigaLib.DoMethodA(FMuiObject.FTextObj, @Param[0])); // crashes later
    //PC := PChar(AmigaLib.DoMethod(FMuiObject.FTextObj, [MUIM_TextEditor_ExportText, 0, 0])); // crashes later sometimes
    PC := PChar(CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [MUIM_TextEditor_ExportText]));
    if Assigned(PC) then
    begin
      Result := PC;
    end;
    //sysdebugln('<--GetMUIText: ' + Result + ' ' + HexStr(PC));
    FreeVec(PC);
  end;
end;

function TFlowString.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Text := GetMUIText;
    Result := SL.Count;
    SL.EndUpdate;
  end;
end;

function TFlowString.Add(const S: String): Integer;
var
  PC: PChar;
begin
  Result := -1;
  if Assigned(FMuiObject) then
  begin
    {SL.BeginUpdate;
    SL.Text := GetMUIText;
    Result := SL.Add(S);
    //sysdebugln('Add: ' + s);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
    // Hacky jump to end on Add :-P
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_CursorY, NativeUInt(SL.Count)]);}

    // New Method, directly use the Add of TextObject:
    //DoMethod(FMuiObject.FTextObj, [MUIM_TextEditor_InsertText, PtrUInt(PChar(s + #13)), MUIV_TextEditor_InsertText_Bottom]);
    CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [MUIM_TextEditor_InsertText, PtrUInt(PChar(s + #13)), MUIV_TextEditor_InsertText_Bottom]);
  end;
end;

procedure TFlowString.Clear;
//var
//  Param: array[0..2] of PtrUInt;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    //Param[0] := MUIM_TextEditor_ClearText;
    //Param[1] := 0;
    //Param[2] := 0;
    //sysdebugln('Clear ' + FText);
    //DoMethodA(FMuiObject.FTextObj, @Param[0]);
    CallHook(PHook(OCLASS(FMuiObject.FTextObj)), FMuiObject.FTextObj, [MUIM_TextEditor_ClearText]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Delete(Index: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin;
    SL.BeginUpdate;
    SL.Clear;
    SL.Text := GetMUIText;
    SL.Delete(Index);
    //sysdebugln('Delete' + FText);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Exchange(Index1: Integer; Index2: Integer);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.Text := GetMUIText;
    SL.Exchange(Index1, Index2);
    //sysdebugln('Exchange ' + FText);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
  end;
end;

function TFlowString.Get(Index: Integer): string;
begin
  Result := '';
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.Text := GetMUIText;
    //sysdebugln('Get ' + FText);
    Result := SL.strings[Index];
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Put(Index: Integer; const S: string);
var
  PC: PChar;
begin
  inherited Put(Index, S);
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.Text := GetMUIText;
    SL.strings[Index] := S;
    //sysdebugln('Put ' + FText);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.Insert(Index: Integer; const S: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.Text := GetMUIText;
    SL.Insert(Index, S);
    //sysdebugln('Insert ' + FText);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
  end;
end;

procedure TFlowString.LoadFromFile(const FileName: String);
var
  PC: PChar;
begin
  if Assigned(FMuiObject) then
  begin
    SL.BeginUpdate;
    SL.Clear;
    SL.LoadFromFile(FileName);
    //sysdebugln('LoadFromFile ' + FText);
    PC := SL.GetText;
    FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(PC)]);
    //FText := SL.Text;
    //FMuiObject.SetAttObj(FMuiObject.FTextObj,[MUIA_TextEditor_Contents, NativeUInt(@(FText[1]))]);
    SL.EndUpdate;
  end;
end;

{TMUIGroupBox}

constructor TMUIGroupBox.Create(const Tags: TATagList);
begin
  //Tags.AddTags([
  //  MUIA_FillArea, LFalse
  //  ]);
  inherited Create(LCLGroupClass, Tags);
  MUIDrawing := True;
  FText := nil;
end;

destructor TMUIGroupBox.Destroy;
begin
  FreeMem(FText);
  inherited;
end;

function TMUIGroupBox.GetCaption: string;
begin
  Result := FText;//PChar(GetAttribute(MUIA_FrameTitle));
end;

procedure TMUIGroupBox.SetCaption(const AValue: string);
begin
  //Set is not supported
  //SetAttribute([MUIA_FrameTitle, AValue]);
end;

function TMUIGroupBox.GetClientRect: TRect;
begin
  //writeln(TGroupBox(pasobject).caption);
  Result.Left := GetAttribute(MUIA_InnerLeft);
  Result.Top := GetAttribute(MUIA_InnerTop);
  Result.Right:= FWidth - (GetAttribute(MUIA_InnerRight) + Result.Left + 1);
  Result.Bottom := FHeight - (GetAttribute(MUIA_InnerBottom) + Result.Top + 1);
  //writeln('get clientrect ', Result.Top, ' ', Result.Bottom);
  //writeln('               ', Result.Left, ' ', Result.Right);
end;



end.

