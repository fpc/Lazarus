unit SynGutterBase;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Menus, math, SynEditMarks,
  SynEditMiscClasses, SynTextDrawer, SynEditMouseCmds, SynEditFoldedView;

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
    FOnResize: TNotifyEvent;
    FOnChange: TNotifyEvent;

    procedure SetAutoSize(const AValue: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetGutterParts(const AValue: TSynGutterPartListBase);
    procedure SetLeftOffset(const AValue: integer);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetRightOffset(const AValue: integer);
    procedure SetVisible(const AValue: boolean);
    procedure SetWidth(Value: integer);
  protected
    FMouseActions: TSynEditMouseActions;
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
    procedure Clear;
  public
    constructor Create(AOwner : TSynEditBase; ASide: TSynGutterSide; ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RecalcBounds;
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
      read FMouseActions write SetMouseActions;
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
    function GetFoldView: TSynEditFoldedView;
    function GetGutterParts: TSynGutterPartListBase;
    procedure SetMarkupInfo(const AValue: TSynSelectedColor);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
  protected
    FMouseActions: TSynEditMouseActions;
    function  PreferedWidth: Integer; virtual;
    procedure SetBounds(ALeft, ATop, AHeight: Integer);
    procedure DoAutoSize;
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure SetWidth(const AValue : integer); virtual;
    procedure Init; override;
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
      read FMouseActions write SetMouseActions;
  end;


implementation
uses SynEdit;

{ TSynGutterBase }

constructor TSynGutterBase.Create(AOwner: TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TheTextDrawer);
begin
  inherited Create;
  FSide := ASide;
  FSynEdit := AOwner;
  CreatePartList;

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
  inherited Destroy;
end;

procedure TSynGutterBase.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TSynGutterBase) then
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
  if AValue = nil then
    FMouseActions.Clear
  else
    FMouseActions.Assign(AValue);
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
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoResize(Self);
    DoChange(Self);
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

  FWidth := NewWidth;
  DoResize(Self);
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
end;

function TSynGutterBase.PartCount: integer;
begin
  if FGutterPartList <> nil then
    result := FGutterPartList.Count
  else
    Result := 0;
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
  FGutterPartList.OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
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

{ TSynGutterPartBase }

function TSynGutterPartBase.GetGutterParts: TSynGutterPartListBase;
begin
  Result := TSynGutterPartListBase(Owner);
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
  if AValue = nil then
    FMouseActions.Clear
  else
    FMouseActions.Assign(AValue);
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
  Gutter.SetChildBounds;
  DoResize(Self);
end;

procedure TSynGutterPartBase.SetAutoSize(const AValue : boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
  if FAutoSize then
    DoAutoSize;
end;

procedure TSynGutterPartBase.SetVisible(const AValue : boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  Gutter.SetChildBounds;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetWidth(const AValue : integer);
begin
  if (FWidth=AValue) or (FAutoSize) then exit;
  FWidth:=AValue;
  Gutter.SetChildBounds;
  DoResize(Self);
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSynGutterPartBase.Create(AOwner: TComponent);
begin
  FMarkupInfo := TSynSelectedColor.Create;
  FMarkupInfo.Background := clBtnFace;
  FMarkupInfo.Foreground := clNone;
  FMarkupInfo.FrameColor := clNone;
  FMarkupInfo.OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;

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

procedure TSynGutterPartBase.DoResize(Sender: TObject);
begin
  DoChange(Sender);
end;

destructor TSynGutterPartBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMarkupInfo);
end;

procedure TSynGutterPartBase.Assign(Source : TPersistent);
var
  Src: TSynGutterPartBase;
begin
  if Assigned(Source) and (Source is TSynGutterPartBase) then
  begin
    Src := TSynGutterPartBase(Source);
    FVisible := Src.FVisible;
    FWidth := Src.FWidth;
    FAutoSize := Src.FAutoSize;
    MarkupInfo.Assign(Src.MarkupInfo);
    DoChange(Self);
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
  Result := False;
  if assigned(FMouseActions) then
    Result := HandleActionProc(MouseActions, AnInfo);
end;

function TSynGutterPartBase.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;
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
  TSynGutterPartBase(AnItem).OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
  TSynGutterPartBase(AnItem).OnGutterClick := {$IFDEF FPC}@{$ENDIF}Gutter.DoDefaultGutterClick;
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

