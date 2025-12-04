unit SynGutterBase;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, math,
  // LCL
  Graphics, Controls, Menus, LCLIntf, LCLType,
  // LazUtils
  LazMethodList,
  // SynEdit
  SynEditMarks, SynEditMiscClasses, SynEditMouseCmds, LazSynTextArea, SynEditHighlighter,
  SynEditTypes, LazSynEditMouseCmdsTypes, LazEditTextGridPainter, LazEditTextAttributes;

type

  TSynGutterColorAttributes = class(TLazEditTextAttribute)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  end;

  { TSynGutterColorAttributesModifier }

  TSynGutterColorAttributesModifier = class(TLazEditTextAttributeModifier)
  published
    property Foreground;
    property Background;
    property FrameColor;

    property ForePriority;
    property BackPriority;
    property FramePriority;

    property FrameStyle;
    property FrameEdges;

    property Style;
    property BoldPriority;
    property ItalicPriority;
    property UnderlinePriority;
    property StrikeOutPriority;

    property OnChange;
  published
    property BackAlpha;
    property ForeAlpha;
    property FrameAlpha;

    property StyleMask;
  end;

  { TSynGutterDefColorAttributesModifier }

  TSynGutterDefColorAttributesModifier = class(TSynGutterColorAttributesModifier)
  protected
    procedure Init; override;
  end;

  TSynGutterBase = class;
  TSynGutterPartBase = class;
  TSynGutterPartBaseClass = class of TSynGutterPartBase;
  TSynGutterPartListBase = class;

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark) of object deprecated 'Use TGutterClickEvent in unit SynEdit';

  TSynGutterClickInfo = record
    MouseX, MouseY: integer;
    LinePos: IntPos; // 1 based
    Shift: TShiftState;
    GutterPart: TSynGutterPartBase;
  end;
  TSynGutterClickEvent = procedure(ASynEdit: TSynEditBase; AGutter: TSynGutterBase; const AnInfo: TSynGutterClickInfo) of object;


  { TSynGutterBase }

  TSynGutterSide = (gsLeft, gsRight);

  TSynGutterBase = class(TSynGutterDispatchBase)
  private
    FGutterArea: TLazSynSurfaceWithText;
    FGutterPartList: TSynGutterPartListBase;
    FSide: TSynGutterSide;
    FSynEdit: TSynEditBase;
    FTextDrawer: TLazEditTextGridPainter;
    FColor: TSynGutterColorAttributesModifier;
    FCurrentLineColor: TSynGutterColorAttributesModifier;
    FMarkupInfoCurLineMerged: TLazEditTextAttributeMergeResult;

    FLeft, FWidth, FHeight, FTop: Integer;
    FVisible: boolean;
    FAutoSize: boolean;
    FRightOffset, FLeftOffset: integer;
    FInDoChange: Boolean;
    FChangeLock: Integer;
    FNeedOnChange, FNeedOnResize: Boolean;
    FFlags: set of (gfNeedChildBounds);
    FOnResize: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnClick: TSynGutterClickEvent;
    FMouseDownPart: Integer;
    FMouseActions: TSynEditMouseInternalActions;
    FOnResizeHandler: TMethodList;
    FOnChangeHandler: TMethodList;

    procedure DoColorChanged(Sender: TObject);
    procedure UpdateInternalColors;
    function GetColor: TColor;
    function GetMouseActions: TSynEditMouseActions;
    procedure SetAutoSize(const AValue: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetCurrentLineColor(AValue: TSynGutterColorAttributesModifier);
    procedure SetGutterParts(const AValue: TSynGutterPartListBase);
    procedure SetLeftOffset(const AValue: integer);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetRightOffset(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
    procedure SetWidth(Value: integer);
  protected
    FCaretRow: integer;
    function PixelToPartIndex(X: Integer): Integer;
    procedure SetChildBounds;
    procedure DoChange(Sender: TObject);
    procedure DoResize(Sender: TObject);
    procedure DoClick(AGutterPart: TSynGutterPartBase; const AnInfo: TSynEditMouseActionInfo); virtual;
    procedure MouseDown(const AnInfo: TSynEditMouseActionInfo); override;
    procedure MouseUp(const AnInfo: TSynEditMouseActionInfo); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure IncChangeLock;
    procedure DecChangeLock;
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark); virtual;
    procedure RegisterNewGutterPartList(APartList: TSynGutterPartListBase);
    function  PartCount: integer;
    function  CreatePartList: TSynGutterPartListBase; virtual; abstract;
    function  CreateMouseActions: TSynEditMouseInternalActions; virtual;
    procedure Clear;
    function GetOwner: TPersistent; override;
    property GutterArea: TLazSynSurfaceWithText read FGutterArea write FGutterArea;

    property MarkupInfoCurLineMerged: TLazEditTextAttributeMergeResult read FMarkupInfoCurLineMerged;
    property CaretRow: integer read FCaretRow; // vaild only during paint
  public
    constructor Create(AOwner : TSynEditBase; ASide: TSynGutterSide; ATextDrawer: TLazEditTextGridPainter);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RecalcBounds;
    procedure ScalePPI(const AScaleFactor: Double);
    procedure DoAutoSize;
    function  HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); deprecated 'Use MouseDown(TSynEditMouseActionInfo) / To be removed in 5.99';
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); deprecated 'Use MouseUp(TSynEditMouseActionInfo) / To be removed in 5.99';
    procedure DoOnGutterClick(X, Y: integer); deprecated 'Use DoClick / To be removed in 5.99';
    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                 HandleActionProc: TSynEditMouseActionHandler): Boolean; virtual;
    procedure ResetMouseActions; virtual; // set mouse-actions according to current Options / may clear them
    procedure RegisterResizeHandler(AHandler: TNotifyEvent);
    procedure UnregisterResizeHandler(AHandler: TNotifyEvent);
    procedure RegisterChangeHandler(AHandler: TNotifyEvent);
    procedure UnregisterChangeHandler(AHandler: TNotifyEvent);
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Height:Integer read FHeight;
    property Width: integer read FWidth write SetWidth;
    property Side:TSynGutterSide read FSide;
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Visible: boolean read FVisible write SetVisible default True;
    property LeftOffset: integer read FLeftOffset write SetLeftOffset
      default 0;
    property RightOffset: integer read FRightOffset write SetRightOffset
      default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  public
    property Parts: TSynGutterPartListBase read FGutterPartList write SetGutterParts;
  public
    // properties available for the GutterPartClasses
    property SynEdit: TSynEditBase read FSynEdit;
    property TextDrawer: TLazEditTextGridPainter read FTextDrawer;
    property Color: TColor read GetColor write SetColor default clBtnFace;
    property CurrentLineColor: TSynGutterColorAttributesModifier read FCurrentLineColor write SetCurrentLineColor;
    property OnClick: TSynGutterClickEvent read FOnClick write FOnClick;
    property MouseActions: TSynEditMouseActions
      read GetMouseActions write SetMouseActions;
  end;

  { TSynGutterPartListBase }

  TSynGutterPartListBase = class(TSynObjectList)
  private
    FGutter: TSynGutterBase;
    function GetByClass(AClass: TSynGutterPartBaseClass; Index: Integer): TSynGutterPartBase;
    function GetByClassCount(AClass: TSynGutterPartBaseClass): Integer;
    function GetPart(Index: Integer): TSynGutterPartBase;
    function GetSynEdit: TSynEditBase;
    procedure PutPart(Index: Integer; const AValue: TSynGutterPartBase);
  protected
    procedure RegisterItem(AnItem: TSynObjectListItem); override;
    property Gutter: TSynGutterBase read FGutter;
    property SynEdit:TSynEditBase read GetSynEdit;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AGutter: TSynGutterBase);
    destructor  Destroy; override;
    property Part[Index: Integer]: TSynGutterPartBase
      read GetPart write PutPart; default;
    property ByClassCount[AClass: TSynGutterPartBaseClass]: Integer
      read GetByClassCount;
    property ByClass[AClass: TSynGutterPartBaseClass; Index: Integer]: TSynGutterPartBase
      read GetByClass;
  end;

  { TSynGutterPartList
    GutterPartList for the left side Gutter. Historically the left Gutter is reffered to as Gutter without prefix }

  TSynGutterPartList = class(TSynGutterPartListBase);

  { TSynRightGutterPartList
    GutterPartList for the right side Gutter. }

  TSynRightGutterPartList = class(TSynGutterPartListBase);

  { TSynGutterPartBase }

  TSynGutterPartBase = class(TSynObjectListItem)
  private
    FLeft, FWidth, FHeight, FTop: Integer;
    FLeftOffset: integer;
    FRightOffset: integer;
    FAutoSize : boolean;
    FVisible: Boolean;
    FSynEdit: TSynEditBase;
    FGutter: TSynGutterBase;
    FMarkupInfo: TSynGutterColorAttributes;
    FMarkupInfoInternal: TLazEditTextAttributeModifier;
    FMarkupInfoCurrentLine: TSynGutterColorAttributesModifier;
    FMarkupInfoCurLineMerged: TLazEditTextAttributeMergeResult;
    FCursor: TCursor;
    FOnChange: TNotifyEvent;
    FOnClick: TSynGutterClickEvent;
    FOnGutterClick: TGutterClickEvent{%H-};
    FMouseActions: TSynEditMouseInternalActions;
    procedure DoColorChanged(Sender: TObject);
    function GetCaretRow: integer; inline;
    function GetFullWidth: integer;
    function GetGutterArea: TLazSynSurfaceWithText;
    function GetGutterParts: TSynGutterPartListBase;
    function GetMouseActions: TSynEditMouseActions;
    procedure SetLeftOffset(AValue: integer);
    procedure SetMarkupInfo(const AValue: TSynGutterColorAttributes);
    procedure SetMarkupInfoCurrentLine(AValue: TSynGutterColorAttributesModifier);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetRightOffset(AValue: integer);
  protected
    function  CreateMouseActions: TSynEditMouseInternalActions; virtual;
    function Scale96ToFont(const ASize: Integer): Integer;
    function  PreferedWidth: Integer; virtual; // at PPI 96
    function  PreferedWidthAtCurrentPPI: Integer; virtual;
    procedure SetBounds(ALeft, ATop, AHeight: Integer);
    procedure DoAutoSize;
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure GutterVisibilityChanged; virtual;
    procedure UpdateInternalColors; virtual;
    procedure PaintBackground(Canvas: TCanvas; AClip: TRect);
    procedure SetWidth(const AValue : integer); virtual;
    procedure Init; override;
    procedure VisibilityOrSize(aCallDoChange: Boolean = False);
    procedure DoResize(Sender: TObject); virtual;
    procedure DoChange(Sender: TObject); virtual;
    procedure DoClick(const AnInfo: TSynEditMouseActionInfo); virtual;
    procedure MouseDown(const AnInfo: TSynEditMouseActionInfo); virtual;
    procedure MouseUp(const AnInfo: TSynEditMouseActionInfo); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    property GutterParts: TSynGutterPartListBase read GetGutterParts;
    property Gutter: TSynGutterBase read FGutter;
    property SynEdit:TSynEditBase read FSynEdit;
    property GutterArea: TLazSynSurfaceWithText read GetGutterArea;
    property MarkupInfoInternal: TLazEditTextAttributeModifier read FMarkupInfoInternal;
    property MarkupInfoCurLineMerged: TLazEditTextAttributeMergeResult read FMarkupInfoCurLineMerged;
    property CaretRow: integer read GetCaretRow;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Height:Integer read FHeight;
    procedure PaintAll(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); virtual;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); virtual; abstract;
    procedure ScalePPI(const AScaleFactor: Double); virtual;
  public
    // X/Y are relative to the gutter, not the gutter part
    function HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; deprecated 'Use MouseDown(TSynEditMouseActionInfo) / To be removed in 5.99';
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual; deprecated 'Use MouseUp(TSynEditMouseActionInfo) / To be removed in 5.99';
    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                 HandleActionProc: TSynEditMouseActionHandler): Boolean; virtual;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean; virtual;
    procedure ResetMouseActions; virtual; // set mouse-actions according to current Options / may clear them
    procedure DoOnGutterClick(X, Y: integer);  virtual;
    property OnGutterClick: TGutterClickEvent read FOnGutterClick write FOnGutterClick; deprecated 'Use OnClick // To be removed in 5.99';
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property MarkupInfo: TSynGutterColorAttributes read FMarkupInfo write SetMarkupInfo;
    property MarkupInfoCurrentLine: TSynGutterColorAttributesModifier read FMarkupInfoCurrentLine write SetMarkupInfoCurrentLine;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Width: integer read FWidth write SetWidth default 10;
    property FullWidth: integer read GetFullWidth; // includes Offsets
    property LeftOffset: integer read FLeftOffset write SetLeftOffset default 0;
    property RightOffset: integer read FRightOffset write SetRightOffset default 0;
    property Visible: boolean read FVisible write SetVisible default True;
    property OnClick: TSynGutterClickEvent read FOnClick write FOnClick;
    property MouseActions: TSynEditMouseActions
      read GetMouseActions write SetMouseActions;
  end;


const
  ScrollBarWidth=0;

implementation

{ TSynGutterBase }

constructor TSynGutterBase.Create(AOwner: TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TLazEditTextGridPainter);
begin
  FMouseDownPart := -1;

  FOnResizeHandler := TMethodList.Create;
  FOnChangeHandler := TMethodList.Create;

  FColor := TSynGutterDefColorAttributesModifier.Create;
  FColor.AddChangeHandler(@DoColorChanged);

  FCurrentLineColor := TSynGutterColorAttributesModifier.Create;
  FCurrentLineColor.AddChangeHandler(@DoColorChanged);
  FMarkupInfoCurLineMerged := TLazEditTextAttributeMergeResult.Create;

  inherited Create;
  FSide := ASide;
  FSynEdit := AOwner;
  CreatePartList;
  FMouseActions := CreateMouseActions;


  FInDoChange := False;
  FChangeLock := 0;
  FTextDrawer := ATextDrawer;
  FWidth := -1;
  FLeftOffset := 0;
  FRightOffset := 0;
  Color := clBtnFace;
  FVisible := True;
  AutoSize := True;
end;

destructor TSynGutterBase.Destroy;
begin
  FOnChange := nil;
  FOnResize := nil;
  FreeAndNil(FGutterPartList);
  FreeAndNil(FMouseActions);
  FreeAndNil(FOnChangeHandler);
  FreeAndNil(FOnResizeHandler);
  FreeAndNil(FMarkupInfoCurLineMerged);
  FreeAndNil(FCurrentLineColor);
  FreeAndNil(FColor);
  inherited Destroy;
end;

procedure TSynGutterBase.Assign(Source: TPersistent);
begin
  if Source is TSynGutterBase then
  begin
    IncChangeLock;
    try
      FGutterPartList.Assign(TSynGutterBase(Source).FGutterPartList);
      Color    := TSynGutterBase(Source).Color;
      CurrentLineColor := TSynGutterBase(Source).FCurrentLineColor;
      Visible  := TSynGutterBase(Source).FVisible;
      AutoSize := TSynGutterBase(Source).FAutoSize;
      Width     := TSynGutterBase(Source).FWidth;
      LeftOffset  := TSynGutterBase(Source).FLeftOffset;
      RightOffset := TSynGutterBase(Source).FRightOffset;
    finally
      DecChangeLock;
    end;
  end else
    inherited;
end;

procedure TSynGutterBase.RecalcBounds;
var
  NewTop, NewLeft, NewHeight: Integer;
begin
  // gutters act as alLeft or alRight, so Width is not computed here
  NewTop := 0;
  case FSide of
    gsLeft:
      begin
        NewLeft   := 0;
        NewHeight := SynEdit.ClientHeight;
      end;
    gsRight:
      begin
        NewLeft   := SynEdit.ClientWidth - Width - ScrollBarWidth;
        NewHeight := SynEdit.ClientHeight;
      end;
  end;
  if (NewLeft = FLeft) and (NewTop = FTop) and (NewHeight = FHeight) then
    exit;
  FLeft   := NewLeft;
  FTop    := NewTop;
  FHeight := NewHeight;

  //Resize parts
  IncChangeLock;
  try
    SetChildBounds;
    DoResize(Self);
  finally
    DecChangeLock;
  end;
end;

function TSynGutterBase.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := HandleActionProc(FMouseActions.GetActionsForOptions(SynEdit.MouseOptions), AnInfo);
end;

procedure TSynGutterBase.ResetMouseActions;
begin
  FMouseActions.Options := SynEdit.MouseOptions;
  FMouseActions.ResetUserActions;
end;

procedure TSynGutterBase.ScalePPI(const AScaleFactor: Double);
var
  I: Integer;
begin
  for I := 0 to PartCount-1 do
    Parts[I].ScalePPI(AScaleFactor);
end;

procedure TSynGutterBase.RegisterResizeHandler(AHandler: TNotifyEvent);
begin
  FOnResizeHandler.Add(TMethod(AHandler));
end;

procedure TSynGutterBase.UnregisterResizeHandler(AHandler: TNotifyEvent);
begin
  FOnResizeHandler.Remove(TMethod(AHandler));
end;

procedure TSynGutterBase.RegisterChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeHandler.Add(TMethod(AHandler));
end;

procedure TSynGutterBase.UnregisterChangeHandler(AHandler: TNotifyEvent);
begin
  FOnChangeHandler.Remove(TMethod(AHandler));
end;

procedure TSynGutterBase.SetColor(const Value: TColor);
begin
  if FColor.Background = Value then
    exit;
  FColor.Background := Value;
end;

procedure TSynGutterBase.SetCurrentLineColor(AValue: TSynGutterColorAttributesModifier);
begin
  FCurrentLineColor.Assign(AValue);
end;

procedure TSynGutterBase.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then exit;
  FAutoSize := AValue;
  if FAutoSize then
    DoAutoSize;
end;

function TSynGutterBase.GetMouseActions: TSynEditMouseActions;
begin
  Result := FMouseActions.UserActions;
end;

function TSynGutterBase.GetColor: TColor;
begin
  Result := FColor.Background;
end;

procedure TSynGutterBase.DoColorChanged(Sender: TObject);
begin
  UpdateInternalColors;
  DoChange(Self);
end;

procedure TSynGutterBase.UpdateInternalColors;
var
  i: Integer;
begin
  FMarkupInfoCurLineMerged.Clear;
  FMarkupInfoCurLineMerged.Assign(FColor);
  FMarkupInfoCurLineMerged.Merge(FCurrentLineColor);
  FMarkupInfoCurLineMerged.FinishMerge;
  for i := 0 to PartCount - 1 do
    Parts[i].UpdateInternalColors;
end;

procedure TSynGutterBase.SetGutterParts(const AValue: TSynGutterPartListBase);
begin
  FGutterPartList.Assign(AValue);
end;

procedure TSynGutterBase.SetLeftOffset(const AValue: integer);
begin
  if FLeftOffset <> AValue then
  begin
    FLeftOffset := Max(0, AValue);
    DoChange(Self);
  end;
end;

procedure TSynGutterBase.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  FMouseActions.UserActions := AValue;
end;

procedure TSynGutterBase.SetRightOffset(const AValue: integer);
begin
  if FRightOffset <> AValue then
  begin
    FRightOffset := Max(0, AValue);
    DoChange(Self);
  end;
end;

procedure TSynGutterBase.SetVisible(const AValue: boolean);
var
  i: Integer;
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoResize(Self);
    DoChange(Self);
    for i := 0 to PartCount - 1 do
      Parts[i].GutterVisibilityChanged;
  end;
end;

procedure TSynGutterBase.SetWidth(Value: integer);
begin
  Value := Max(0, Value);
  if (FWidth=Value) or (FAutoSize) then exit;
  FWidth := Value;
  DoResize(Self);
end;

function TSynGutterBase.PixelToPartIndex(X: Integer): Integer;
begin
  Result := 0;
  x := x - Left - LeftOffset;
  while Result < PartCount-1 do begin
    if Parts[Result].Visible then begin
      if x >= Parts[Result].FullWidth then
        x := x - Parts[Result].FullWidth
      else
        break;
    end;
    inc(Result)
  end;
end;

procedure TSynGutterBase.DoAutoSize;
var
  NewWidth, i: Integer;
begin
  NewWidth := FLeftOffset + FRightOffset;
  for i := PartCount-1 downto 0 do
    if Parts[i].Visible then
    begin
      if Parts[i].AutoSize then
        Parts[i].DoAutoSize;
      NewWidth := NewWidth + Parts[i].FullWidth;
    end;

  if FWidth = NewWidth then begin
    SetChildBounds;
    exit;
  end;

  IncChangeLock;
  try
    FWidth := NewWidth;
    Include(FFlags, gfNeedChildBounds);

    RecalcBounds;

    if gfNeedChildBounds in FFlags then
      SetChildBounds;

    DoResize(Self);
  finally
    DecChangeLock;
  end;
end;

function TSynGutterBase.HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
begin
  if (FMouseDownPart >= 0) and (FMouseDownPart < PartCount) then
    Result := Parts[FMouseDownPart].HasCustomPopupMenu(PopMenu)
  else
    Result := False;
end;

procedure TSynGutterBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownPart := PixelToPartIndex(X);
  Parts[FMouseDownPart].MouseDown(Button, Shift, X, Y){%H-};
  if (Button = Controls.mbLeft) then
    DoOnGutterClick(X, Y){%H-};
end;

procedure TSynGutterBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Parts[FMouseDownPart].MouseUp(Button, Shift, X, Y){%H-};
end;

procedure TSynGutterBase.DoOnGutterClick(X, Y: integer);
begin
  Parts[PixelToPartIndex(X)].DoOnGutterClick(X, Y);
end;

procedure TSynGutterBase.SetChildBounds;
var
  i, NewLeft: Integer;
begin
  NewLeft := Left + LeftOffset;
  for i := 0 to PartCount - 1 do
    if Parts[i].Visible then begin
      Parts[i].SetBounds(NewLeft, Top, Height);
      NewLeft := NewLeft + Parts[i].FullWidth;
    end;
  Exclude(FFlags, gfNeedChildBounds);
end;

function TSynGutterBase.PartCount: integer;
begin
  if FGutterPartList <> nil then
    result := FGutterPartList.Count
  else
    Result := 0;
end;

function TSynGutterBase.CreateMouseActions: TSynEditMouseInternalActions;
begin
  Result := TSynEditMouseInternalActions.Create(Self);
end;

procedure TSynGutterBase.DoChange(Sender: TObject);
begin
  if FInDoChange then exit;
  if FChangeLock > 0 then begin;
    FNeedOnChange := True;
    exit;
  end;
  FNeedOnChange := False;
  If FAutoSize then begin
    FInDoChange := True;
    try
      DoAutoSize;
    finally
      FInDoChange := False;
    end;
  end;
  FOnChangeHandler.CallNotifyEvents(Self);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutterBase.DoResize(Sender: TObject);
begin
  if FChangeLock > 0 then begin;
    FNeedOnResize := True;
    exit;
  end;
  FNeedOnResize := False;
  FOnResizeHandler.CallNotifyEvents(Self);
  if Assigned(FOnResize) then
    FOnResize(Self)
  else
    DoChange(Self);
end;

procedure TSynGutterBase.DoClick(AGutterPart: TSynGutterPartBase;
  const AnInfo: TSynEditMouseActionInfo);
var
  ClickInfo: TSynGutterClickInfo;
begin
  if Assigned(FOnClick) then begin
    ClickInfo.MouseX     := AnInfo.MouseX;
    ClickInfo.MouseY     := AnInfo.MouseY;
    ClickInfo.Shift      := AnInfo.Shift;
    ClickInfo.LinePos    := AnInfo.NewCaret.LinePos;
    ClickInfo.GutterPart := AGutterPart;
    FOnClick(SynEdit, Self, ClickInfo);
  end;
end;

procedure TSynGutterBase.MouseDown(const AnInfo: TSynEditMouseActionInfo);
begin
  FMouseDownPart := PixelToPartIndex(AnInfo.MouseX);
  Parts[FMouseDownPart].MouseDown(AnInfo);

  MouseDown(SynMouseButtonBackMap[AnInfo.Button], AnInfo.Shift, AnInfo.MouseX, AnInfo.MouseY){%H-};

  if (AnInfo.Button = LazSynEditMouseCmdsTypes.mbLeft) then
    DoClick(Parts[FMouseDownPart], AnInfo);
end;

procedure TSynGutterBase.MouseUp(const AnInfo: TSynEditMouseActionInfo);
begin
  if (FMouseDownPart >= 0) and (FMouseDownPart < PartCount) then
    Parts[FMouseDownPart].MouseUp(AnInfo);

  MouseUp(SynMouseButtonBackMap[AnInfo.Button], AnInfo.Shift, AnInfo.MouseX, AnInfo.MouseY){%H-};

  FMouseDownPart := -1;
end;

procedure TSynGutterBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (FMouseDownPart >= 0) and (FMouseDownPart < PartCount) then
    Parts[FMouseDownPart].MouseMove(Shift, X, Y);
end;

procedure TSynGutterBase.IncChangeLock;
begin
  inc(FChangeLock);
end;

procedure TSynGutterBase.DecChangeLock;
begin
  dec(FChangeLock);
  if FChangeLock = 0 then begin
    if FNeedOnResize then
      DoResize(Self);
    if FNeedOnChange then
      DoChange(Self);
  end;
end;

procedure TSynGutterBase.DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
end;

procedure TSynGutterBase.RegisterNewGutterPartList(APartList: TSynGutterPartListBase);
begin
  if (APartList = nil) or (APartList = FGutterPartList) then
    exit;
  if FGutterPartList <> nil then
    FreeAndNil(FGutterPartList);
  FGutterPartList := APartList;
  FGutterPartList.OnChange := @DoChange;
end;

procedure TSynGutterBase.Clear;
var
  i: Integer;
begin
  if FGutterPartList = nil then exit;
  for i := FGutterPartList.Count - 1 downto 0 do
    FGutterPartList[i].Free;
  FGutterPartList.Clear;
end;

function TSynGutterBase.GetOwner: TPersistent;
begin
  Result := FSynEdit;
end;

{ TSynGutterDefColorAttributesModifier }

procedure TSynGutterDefColorAttributesModifier.Init;
begin
  inherited Init;
  Background := clHighLight;
  Foreground := clHighLightText;
  FrameColor := clNone;
  FrameStyle := slsSolid;
  FrameEdges := sfeAround;
  InternalSaveDefaultValues;
end;

{ TSynGutterPartBase }

function TSynGutterPartBase.GetGutterParts: TSynGutterPartListBase;
begin
  Result := TSynGutterPartListBase(Owner);
end;

function TSynGutterPartBase.GetMouseActions: TSynEditMouseActions;
begin
  Result := FMouseActions.UserActions;
end;

procedure TSynGutterPartBase.SetLeftOffset(AValue: integer);
begin
  if FLeftOffset = AValue then Exit;
  FLeftOffset := AValue;
  VisibilityOrSize;
end;

function TSynGutterPartBase.GetGutterArea: TLazSynSurfaceWithText;
begin
  Result := Gutter.GutterArea;
end;

function TSynGutterPartBase.GetFullWidth: integer;
begin
  Result := FWidth + FLeftOffset + FRightOffset;
end;

procedure TSynGutterPartBase.DoColorChanged(Sender: TObject);
begin
  UpdateInternalColors;
  DoChange(Self);
end;

function TSynGutterPartBase.GetCaretRow: integer;
begin
  Result := Gutter.CaretRow;
end;

procedure TSynGutterPartBase.SetMarkupInfo(const AValue: TSynGutterColorAttributes);
begin
  FMarkupInfo.Assign(AValue);
end;

procedure TSynGutterPartBase.SetMarkupInfoCurrentLine(AValue: TSynGutterColorAttributesModifier);
begin
  if FMarkupInfoCurrentLine = AValue then Exit;
  FMarkupInfoCurrentLine.Assign(AValue);
end;

procedure TSynGutterPartBase.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  FMouseActions.UserActions := AValue;
end;

procedure TSynGutterPartBase.SetRightOffset(AValue: integer);
begin
  if FRightOffset = AValue then Exit;
  FRightOffset := AValue;
  VisibilityOrSize;
end;

function TSynGutterPartBase.PreferedWidth: Integer;
begin
  Result := 12;
end;

function TSynGutterPartBase.PreferedWidthAtCurrentPPI: Integer;
begin
  Result := Scale96ToFont(PreferedWidth);
end;

procedure TSynGutterPartBase.SetBounds(ALeft, ATop, AHeight: Integer);
begin
  if (ALeft = FLeft) and (ATop = FTop) and (AHeight = FHeight) then
    exit;
  FLeft   := ALeft;
  FTop    := ATop;
  FHeight := AHeight;
  DoResize(Self);
end;

procedure TSynGutterPartBase.DoAutoSize;
var
  NewWidth: Integer;
begin
  NewWidth := PreferedWidthAtCurrentPPI;
  if FWidth = NewWidth then exit;
  FWidth := NewWidth;
  VisibilityOrSize;
end;

procedure TSynGutterPartBase.SetAutoSize(const AValue : boolean);
var
  OldSize: Integer;
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
  OldSize := FWidth;
  if FAutoSize then
    DoAutoSize;
  if FWidth = OldSize then
    DoChange(Self); // Size Did not Change
end;

procedure TSynGutterPartBase.SetVisible(const AValue : boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  VisibilityOrSize(True);
end;

procedure TSynGutterPartBase.GutterVisibilityChanged;
begin
  //
end;

procedure TSynGutterPartBase.UpdateInternalColors;
begin
  if Gutter = nil then
    exit;
  FMarkupInfoInternal.Assign(FMarkupInfo);
  if FMarkupInfoInternal.Background = clNone then
    FMarkupInfoInternal.Background := Gutter.Color;
  if FMarkupInfoInternal.Foreground = clNone then
    FMarkupInfoInternal.Foreground := SynEdit.Font.Color;

  FMarkupInfoCurLineMerged.Clear;
  FMarkupInfoCurLineMerged.Assign(FMarkupInfoInternal);
  FMarkupInfoCurLineMerged.Merge(Gutter.FCurrentLineColor);
  FMarkupInfoCurLineMerged.Merge(MarkupInfoCurrentLine);
  FMarkupInfoCurLineMerged.FinishMerge;
end;

procedure TSynGutterPartBase.PaintBackground(Canvas: TCanvas; AClip: TRect);
var
  t: Integer;
begin
  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
    LCLIntf.SetBkColor(Canvas.Handle, TColorRef(Canvas.Brush.Color));
    Canvas.FillRect(AClip);
  end;
  if (MarkupInfoCurLineMerged.Background <> clNone) and (CaretRow >= 0) then
  begin
    t := GutterArea.Top;
    aClip.Top := t + CaretRow * SynEdit.LineHeight;
    AClip.Bottom := AClip.Top + SynEdit.LineHeight;
    Canvas.Brush.Color := MarkupInfoCurLineMerged.Background;
    LCLIntf.SetBkColor(Canvas.Handle, TColorRef(Canvas.Brush.Color));
    Canvas.FillRect(AClip);
  end;
end;

procedure TSynGutterPartBase.SetWidth(const AValue : integer);
begin
  if (FWidth=AValue) or ((FAutoSize) and not(csLoading in ComponentState)) then exit;
  FWidth:=AValue;
  VisibilityOrSize;
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSynGutterPartBase.DoClick(const AnInfo: TSynEditMouseActionInfo);
var
  ClickInfo: TSynGutterClickInfo;
begin
  if Assigned(FOnClick) then begin
    ClickInfo.MouseX     := AnInfo.MouseX;
    ClickInfo.MouseY     := AnInfo.MouseY;
    ClickInfo.Shift      := AnInfo.Shift;
    ClickInfo.LinePos    := AnInfo.NewCaret.LinePos;
    ClickInfo.GutterPart := Self;
    FOnClick(SynEdit, Gutter, ClickInfo);
  end;
end;

procedure TSynGutterPartBase.MouseDown(const AnInfo: TSynEditMouseActionInfo);
begin
  if (AnInfo.Button = LazSynEditMouseCmdsTypes.mbLeft) then
    DoClick(AnInfo);
end;

procedure TSynGutterPartBase.MouseUp(const AnInfo: TSynEditMouseActionInfo);
begin
  //
end;

procedure TSynGutterPartBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

function TSynGutterPartBase.CreateMouseActions: TSynEditMouseInternalActions;
begin
  Result := TSynEditMouseInternalActions.Create(Self);
end;

function TSynGutterPartBase.Scale96ToFont(const ASize: Integer): Integer;
begin
  Result := ASize;
  if SynEdit<>nil then
    Result := SynEdit.Scale96ToFont(Result);
end;

constructor TSynGutterPartBase.Create(AnOwner: TComponent);
begin
  if (AnOwner = nil) or not(AnOwner is TSynGutterPartListBase) then
    raise Exception.Create('Invalid Owner');

  FMarkupInfo := TSynGutterColorAttributes.Create;
  FMarkupInfo.Background := clBtnFace;
  FMarkupInfo.Foreground := clNone;
  FMarkupInfo.FrameColor := clNone;

  FMarkupInfoCurrentLine := TSynGutterDefColorAttributesModifier.Create;
  FMarkupInfoCurrentLine.Background := clNone;
  FMarkupInfoCurrentLine.Foreground := clNone;
  FMarkupInfoCurrentLine.FrameColor := clNone;

  FMarkupInfoInternal := TLazEditTextAttributeModifier.Create;
  FMarkupInfoCurLineMerged := TLazEditTextAttributeMergeResult.Create;

  FMouseActions := CreateMouseActions;

  FVisible := True;
  FAutoSize := True;
  FLeftOffset := 0;
  FRightOffset := 0;
  Inherited Create(AnOwner); // Todo: Lock the DoChange from RegisterItem, and call DoChange at the end (after/in autosize)

  FMarkupInfo.AddChangeHandler(@DoColorChanged);
  FMarkupInfoCurrentLine.AddChangeHandler(@DoColorChanged);
  UpdateInternalColors;
end;

procedure TSynGutterPartBase.Init;
begin
  inherited Init;
  FGutter := GutterParts.Gutter;
  FSynEdit := GutterParts.SynEdit;
  FriendEdit := FSynEdit;
end;

procedure TSynGutterPartBase.VisibilityOrSize(aCallDoChange: Boolean);
begin
  if (csLoading in ComponentState) then exit;
  Gutter.IncChangeLock;
  try
    if Gutter.AutoSize then
      Gutter.DoAutoSize;  // Calculate new total width of gutter
    DoResize(Self);
    if aCallDoChange then
      DoChange(Self);
  finally
    Gutter.DecChangeLock;
  end;
end;

procedure TSynGutterPartBase.DoResize(Sender: TObject);
begin
  DoChange(Sender);
end;

destructor TSynGutterPartBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMouseActions);
  FreeAndNil(FMarkupInfoCurLineMerged);
  FreeAndNil(FMarkupInfo);
  FreeAndNil(FMarkupInfoInternal);
  FreeAndNil(FMarkupInfoCurrentLine);
end;

procedure TSynGutterPartBase.Assign(Source : TPersistent);
var
  Src: TSynGutterPartBase;
begin
  if Source is TSynGutterPartBase then
  begin
    Src := TSynGutterPartBase(Source);
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FAutoSize := Src.FAutoSize;
    MarkupInfo.Assign(Src.MarkupInfo);
    MarkupInfoCurrentLine.Assign(Src.MarkupInfoCurrentLine);
    UpdateInternalColors;
    DoChange(Self);
    // Todo, maybe on Resize?
  end else
    inherited;
end;

procedure TSynGutterPartBase.PaintAll(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: integer);
var
  OffsRect: TRect;
begin
  if not Visible then exit;

    if (MarkupInfo.Background = clNone) and
       ( (MarkupInfoCurLineMerged.Background = clNone) or (CaretRow < 0))
    then
  begin
    AClip.Left := AClip.Left + FLeftOffset;
    AClip.Right := AClip.Right - FRightOffset;
    Paint(Canvas, AClip, FirstLine, LastLine);
    exit;
  end;

  if FLeftOffset > 0 then begin
    OffsRect := AClip;
    OffsRect.Left := FLeft;
    OffsRect.Right := FLeft + FLeftOffset;
    PaintBackground(Canvas, OffsRect);
  end;

  if FRightOffset > 0 then begin
    OffsRect := AClip;
    OffsRect.Right := FLeft + FullWidth;
    OffsRect.Left := OffsRect.Right - FRightOffset;
    PaintBackground(Canvas, OffsRect);
  end;

  AClip.Left := AClip.Left + FLeftOffset;
  AClip.Right := AClip.Right - FRightOffset;
  Paint(Canvas, AClip, FirstLine, LastLine);
end;

function TSynGutterPartBase.HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
begin
  Result := False;
end;

procedure TSynGutterPartBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TSynGutterPartBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

function TSynGutterPartBase.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := HandleActionProc(FMouseActions.GetActionsForOptions(SynEdit.MouseOptions), AnInfo);
end;

function TSynGutterPartBase.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;
end;

procedure TSynGutterPartBase.ResetMouseActions;
begin
  FMouseActions.Options := SynEdit.MouseOptions;
  FMouseActions.ResetUserActions;
end;

procedure TSynGutterPartBase.ScalePPI(const AScaleFactor: Double);
begin
  if not FAutoSize then
    Width := Round(Width*AScaleFactor)
  else
    DoAutoSize;
end;

procedure TSynGutterPartBase.DoOnGutterClick(X, Y : integer);
begin
  if Assigned(FOnGutterClick) then
    FOnGutterClick(Self, X, Y, 0, nil);
end;

{ TSynGutterPartListBase }

constructor TSynGutterPartListBase.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  include(FComponentStyle, csTransient);
  if (FGutter = nil) and (SynEdit.FindGutterFromGutterPartList(Self) <> nil) then
    FGutter := SynEdit.FindGutterFromGutterPartList(Self) as TSynGutterBase;
  Gutter.RegisterNewGutterPartList(self);
end;

constructor TSynGutterPartListBase.Create(AOwner: TComponent; AGutter: TSynGutterBase);
begin
  FGutter := AGutter;
  Create(AOwner);
end;

destructor TSynGutterPartListBase.Destroy;
begin
  FGutter.FGutterPartList := nil;
  OnChange := nil;
  inherited Destroy;
end;

procedure TSynGutterPartListBase.RegisterItem(AnItem: TSynObjectListItem);
begin
  TSynGutterPartBase(AnItem).OnChange := @DoChange;
  TSynGutterPartBase(AnItem).OnGutterClick := @Gutter.DoDefaultGutterClick;
  inherited RegisterItem(AnItem);
end;

function TSynGutterPartListBase.GetSynEdit: TSynEditBase;
begin
  Result := TSynEditBase(Owner);
end;

function TSynGutterPartListBase.GetPart(Index: Integer): TSynGutterPartBase;
begin
  Result := TSynGutterPartBase(BaseItems[Index]);
end;

procedure TSynGutterPartListBase.PutPart(Index: Integer; const AValue: TSynGutterPartBase);
begin
  BaseItems[Index] := TSynObjectListItem(AValue);
end;

function TSynGutterPartListBase.GetByClass(AClass: TSynGutterPartBaseClass; Index: Integer): TSynGutterPartBase;
var
  i: Integer;
begin
  for i := 0 to Count -1 do
    if Part[i] is AClass then begin
      if Index = 0 then
        exit(Part[i]);
      dec(Index);
    end;
  Result := nil;
end;

function TSynGutterPartListBase.GetByClassCount(AClass: TSynGutterPartBaseClass): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count -1 do
    if Part[i] is AClass then
      inc(Result);
end;

end.

