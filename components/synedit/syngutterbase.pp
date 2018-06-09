unit SynGutterBase;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, math,
  // LCL
  LCLProc, Graphics, Controls, Menus,
  // LazUtils
  LazMethodList,
  // SynEdit
  SynEditMarks, SynEditMiscClasses, SynTextDrawer, SynEditMouseCmds, SynEditFoldedView;

type

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

  TSynGutterPartBase = class;
  TSynGutterPartBaseClass = class of TSynGutterPartBase;
  TSynGutterPartListBase = class;

  { TSynGutterBase }

  TSynGutterSide = (gsLeft, gsRight);

  TSynGutterBase = class(TPersistent)
  private
    FGutterPartList: TSynGutterPartListBase;
    FSide: TSynGutterSide;
    FSynEdit: TSynEditBase;
    FTextDrawer: TheTextDrawer;
    FColor: TColor;
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
    FMouseActions: TSynEditMouseInternalActions;
    FOnResizeHandler: TMethodList;
    FOnChangeHandler: TMethodList;

    function GetMouseActions: TSynEditMouseActions;
    procedure SetAutoSize(const AValue: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetGutterParts(const AValue: TSynGutterPartListBase);
    procedure SetLeftOffset(const AValue: integer);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetRightOffset(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
    procedure SetWidth(Value: integer);
  protected
    procedure DoAutoSize;
    procedure SetChildBounds;
    procedure DoChange(Sender: TObject);
    procedure DoResize(Sender: TObject);
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
  public
    constructor Create(AOwner : TSynEditBase; ASide: TSynGutterSide; ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RecalcBounds;
    procedure ScalePPI(const AScaleFactor: Double);
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
    property TextDrawer: TheTextDrawer read FTextDrawer;
    property Color: TColor read FColor write SetColor default clBtnFace;
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
    function FindGutter: TSynGutterBase; virtual; abstract;
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

  TSynGutterPartList = class(TSynGutterPartListBase)
  protected
    function FindGutter: TSynGutterBase; override;
  end;

  { TSynRightGutterPartList
    GutterPartList for the right side Gutter. }

  TSynRightGutterPartList = class(TSynGutterPartListBase)
  protected
    function FindGutter: TSynGutterBase; override;
  end;

  { TSynGutterPartBase }

  TSynGutterPartBase = class(TSynObjectListItem)
  private
    FLeft, FWidth, FHeight, FTop: Integer;
    FAutoSize : boolean;
    FVisible: Boolean;
    FSynEdit: TSynEditBase;
    FGutter: TSynGutterBase;
    FMarkupInfo: TSynSelectedColor;
    FCursor: TCursor;
    FOnChange: TNotifyEvent;
    FOnGutterClick: TGutterClickEvent;
    FMouseActions: TSynEditMouseInternalActions;
    function GetFoldView: TSynEditFoldedView;
    function GetGutterParts: TSynGutterPartListBase;
    function GetMouseActions: TSynEditMouseActions;
    procedure SetMarkupInfo(const AValue: TSynSelectedColor);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
  protected
    function  CreateMouseActions: TSynEditMouseInternalActions; virtual;
    function  PreferedWidth: Integer; virtual;
    procedure SetBounds(ALeft, ATop, AHeight: Integer);
    procedure DoAutoSize;
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure GutterVisibilityChanged; virtual;
    procedure SetWidth(const AValue : integer); virtual;
    procedure Init; override;
    procedure VisibilityOrSize(aCallDoChange: Boolean = False);
    procedure DoResize(Sender: TObject); virtual;
    procedure DoChange(Sender: TObject); virtual;
    property GutterParts: TSynGutterPartListBase read GetGutterParts;
    property Gutter: TSynGutterBase read FGutter;
    property SynEdit:TSynEditBase read FSynEdit;
    property FoldView: TSynEditFoldedView read GetFoldView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Height:Integer read FHeight;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      virtual abstract;
    procedure ScalePPI(const AScaleFactor: Double); virtual;
  public
    // X/Y are relative to the gutter, not the gutter part
    function HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                 HandleActionProc: TSynEditMouseActionHandler): Boolean; virtual;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean; virtual;
    procedure ResetMouseActions; virtual; // set mouse-actions according to current Options / may clear them
    procedure DoOnGutterClick(X, Y: integer);  virtual;
    property OnGutterClick: TGutterClickEvent
      read FOnGutterClick write FOnGutterClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property MarkupInfo: TSynSelectedColor read FMarkupInfo write SetMarkupInfo;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property Width: integer read FWidth write SetWidth default 10;
    property Visible: boolean read FVisible write SetVisible default True;
    property MouseActions: TSynEditMouseActions
      read GetMouseActions write SetMouseActions;
  end;


implementation
uses SynEdit;

{ TSynGutterBase }

constructor TSynGutterBase.Create(AOwner: TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TheTextDrawer);
begin
  FOnResizeHandler := TMethodList.Create;
  FOnChangeHandler := TMethodList.Create;

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
  FColor := clBtnFace;
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
  inherited Destroy;
end;

procedure TSynGutterBase.Assign(Source: TPersistent);
begin
  if Source is TSynGutterBase then
  begin
    IncChangeLock;
    try
      FGutterPartList.Assign(TSynGutterBase(Source).FGutterPartList);
      Color    := TSynGutterBase(Source).FColor;
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
  Result := HandleActionProc(FMouseActions.GetActionsForOptions(TCustomSynEdit(SynEdit).MouseOptions), AnInfo);
end;

procedure TSynGutterBase.ResetMouseActions;
begin
  FMouseActions.Options := TCustomSynEdit(SynEdit).MouseOptions;
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
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange(Self);
  end;
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

procedure TSynGutterBase.DoAutoSize;
var
  NewWidth, i: Integer;
begin
  NewWidth := FLeftOffset + FRightOffset;
  for i := PartCount-1 downto 0 do
    if Parts[i].Visible then
      NewWidth := NewWidth + Parts[i].Width;

  if FWidth = NewWidth then exit;

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

procedure TSynGutterBase.SetChildBounds;
var
  i, NewLeft: Integer;
begin
  NewLeft := Left + LeftOffset;
  for i := 0 to PartCount - 1 do
    if Parts[i].Visible then begin
      Parts[i].SetBounds(NewLeft, Top, Height);
      NewLeft := NewLeft + Parts[i].Width;
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

{ TSynGutterPartBase }

function TSynGutterPartBase.GetGutterParts: TSynGutterPartListBase;
begin
  Result := TSynGutterPartListBase(Owner);
end;

function TSynGutterPartBase.GetMouseActions: TSynEditMouseActions;
begin
  Result := FMouseActions.UserActions;
end;

function TSynGutterPartBase.GetFoldView: TSynEditFoldedView;
begin
  Result := TSynEditFoldedView(FoldedTextBuffer);
end;

procedure TSynGutterPartBase.SetMarkupInfo(const AValue: TSynSelectedColor);
begin
  FMarkupInfo.Assign(AValue);
end;

procedure TSynGutterPartBase.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  FMouseActions.UserActions := AValue;
end;

function TSynGutterPartBase.PreferedWidth: Integer;
begin
  Result := 12;
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
  NewWidth := PreferedWidth;
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

procedure TSynGutterPartBase.SetWidth(const AValue : integer);
begin
  if (FWidth=AValue) or (FAutoSize) then exit;
  FWidth:=AValue;
  VisibilityOrSize;
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynGutterPartBase.CreateMouseActions: TSynEditMouseInternalActions;
begin
  Result := TSynEditMouseInternalActions.Create(Self);
end;

constructor TSynGutterPartBase.Create(AOwner: TComponent);
begin
  FMarkupInfo := TSynSelectedColor.Create;
  FMarkupInfo.Background := clBtnFace;
  FMarkupInfo.Foreground := clNone;
  FMarkupInfo.FrameColor := clNone;
  FMarkupInfo.OnChange := @DoChange;

  FMouseActions := CreateMouseActions;

  FVisible := True;
  FAutoSize := True;
  Inherited Create(AOwner); // Todo: Lock the DoChange from RegisterItem, and call DoChange at the end (after/in autosize)
  DoAutoSize; // Calls PreferedWidth(), must be after Init();
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
  FreeAndNil(FMarkupInfo);
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
    DoChange(Self);
    // Todo, maybe on Resize?
  end else
    inherited;
end;

function TSynGutterPartBase.HasCustomPopupMenu(out PopMenu: TPopupMenu): Boolean;
begin
  Result := False;
end;

procedure TSynGutterPartBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TSynGutterPartBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TSynGutterPartBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

function TSynGutterPartBase.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := HandleActionProc(FMouseActions.GetActionsForOptions(TCustomSynEdit(SynEdit).MouseOptions), AnInfo);
end;

function TSynGutterPartBase.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;
end;

procedure TSynGutterPartBase.ResetMouseActions;
begin
  FMouseActions.Options := TCustomSynEdit(SynEdit).MouseOptions;
  FMouseActions.ResetUserActions;
end;

procedure TSynGutterPartBase.ScalePPI(const AScaleFactor: Double);
begin
  Width := Round(Width*AScaleFactor);
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
  if FindGutter <> nil then
    FGutter := FindGutter;
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

{ TSynGutterPartList }

function TSynGutterPartList.FindGutter: TSynGutterBase;
begin
  Result := TCustomSynEdit(SynEdit).Gutter;
end;

{ TSynRightGutterPartList }

function TSynRightGutterPartList.FindGutter: TSynGutterBase;
begin
  Result := TCustomSynEdit(SynEdit).RightGutter;
end;

end.

