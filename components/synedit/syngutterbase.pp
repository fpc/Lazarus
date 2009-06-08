unit SynGutterBase;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, Controls, Menus, SynEditMarks,
  SynEditMiscClasses, SynEditFoldedView, SynTextDrawer, SynEditMouseCmds;

type

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

  TSynGutterPartBase = class;
  TSynGutterPartBaseClass = class of TSynGutterPartBase;
  TSynGutterPartList = class;

  { TSynGutterBase }

  TSynGutterBase = class(TPersistent)
  private
    FGutterPartList: TSynGutterPartList;
    FFoldView: TSynEditFoldedView;
    FTextDrawer: TheTextDrawer;
    FColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetGutterParts(const AValue: TSynGutterPartList);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
  protected
    FMouseActions: TSynEditMouseActions;
    procedure DoChange(Sender: TObject); virtual;
    procedure DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark); virtual;
    procedure RegisterNewGutterPartList(APartList: TSynGutterPartList);
    function  PartCount: integer;
    procedure Clear;
  public
    constructor Create(AOwner : TSynEditBase; AFoldedLinesView: TSynEditFoldedView;
                      ATextDrawer: TheTextDrawer);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Parts: TSynGutterPartList read FGutterPartList write SetGutterParts;
  public
    // properties available for the GutterPartClasses
    property FoldView: TSynEditFoldedView read FFoldView;
    property TextDrawer: TheTextDrawer read FTextDrawer;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property MouseActions: TSynEditMouseActions
      read FMouseActions write SetMouseActions;
  end;

  { TSynGutterPartList }

  TSynGutterPartList = class(TSynObjectList)
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

  { TSynGutterPartBase }

  TSynGutterPartBase = class(TSynObjectListItem)
  private
    FSynEdit: TSynEditBase;
    FGutter: TSynGutterBase;
    FAutoSize : boolean;
    FMarkupInfo: TSynSelectedColor;
    FCursor: TCursor;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FOnGutterClick: TGutterClickEvent;
    function GetGutterParts: TSynGutterPartList;
    procedure SetMarkupInfo(const AValue: TSynSelectedColor);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetRealWidth(const AValue: Integer);
  protected
    FWidth : integer;
    FMouseActions: TSynEditMouseActions;
    procedure SetAutoSize(const AValue : boolean); virtual;
    procedure SetVisible(const AValue : boolean); virtual;
    procedure SetWidth(const AValue : integer); virtual;
    procedure DoChange(Sender: TObject); virtual;
    property GutterParts: TSynGutterPartList read GetGutterParts;
    property Gutter: TSynGutterBase read FGutter;
    property SynEdit:TSynEditBase read FSynEdit;
    property RealWidth: Integer write SetRealWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer);
      virtual abstract;
  public
    function RealGutterWidth(CharWidth: integer): integer;  virtual; abstract;
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

constructor TSynGutterBase.Create(AOwner: TSynEditBase; AFoldedLinesView: TSynEditFoldedView; ATextDrawer: TheTextDrawer);
begin
  inherited Create;
  TSynGutterPartList.Create(AOwner, self);

  FTextDrawer := ATextDrawer;
  FFoldView := AFoldedLinesView;
  Color := clBtnFace;
end;

destructor TSynGutterBase.Destroy;
begin
  FreeAndNil(FGutterPartList);
  inherited Destroy;
end;

procedure TSynGutterBase.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TSynGutterBase) then
  begin
    FColor := TSynGutterBase(Source).FColor;
    FGutterPartList.Assign(TSynGutterBase(Source).FGutterPartList);
  end else
    inherited;
end;

procedure TSynGutterBase.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange(Self);
  end;
end;

procedure TSynGutterBase.SetGutterParts(const AValue: TSynGutterPartList);
begin
  FGutterPartList.Assign(AValue);
end;

procedure TSynGutterBase.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  if AValue = nil then
    FMouseActions.Clear
  else
    FMouseActions.Assign(AValue);
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
end;

procedure TSynGutterBase.DoDefaultGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
end;

procedure TSynGutterBase.RegisterNewGutterPartList(APartList: TSynGutterPartList);
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

function TSynGutterPartBase.GetGutterParts: TSynGutterPartList;
begin
  Result := TSynGutterPartList(Owner);
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

procedure TSynGutterPartBase.SetRealWidth(const AValue: Integer);
begin
  if FWidth = AValue then exit;
  FWidth :=  AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetAutoSize(const AValue : boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetVisible(const AValue : boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.SetWidth(const AValue : integer);
begin
  if (FWidth=AValue) or (FAutoSize) then exit;
  FWidth:=AValue;
  DoChange(self);
end;

procedure TSynGutterPartBase.DoChange(Sender : TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSynGutterPartBase.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FGutter := GutterParts.Gutter;
  FSynEdit := GutterParts.SynEdit;

  FMarkupInfo := TSynSelectedColor.Create;
  FMarkupInfo.Background := clBtnFace;
  FMarkupInfo.Foreground := clNone;
  FMarkupInfo.FrameColor := clNone;
  FMarkupInfo.OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;

  FVisible := True;
  FAutoSize := True;
  FWidth := 10;
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

{ TSynGutterPartList }

constructor TSynGutterPartList.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  if assigned(TSynEdit(SynEdit).Gutter) then
    FGutter := TSynEdit(SynEdit).Gutter;
  Gutter.RegisterNewGutterPartList(self);
end;

constructor TSynGutterPartList.Create(AOwner: TComponent; AGutter: TSynGutterBase);
begin
  FGutter := AGutter;
  Create(AOwner);
end;

destructor TSynGutterPartList.Destroy;
begin
  FGutter.FGutterPartList := nil;
  OnChange := nil;
  inherited Destroy;
end;

procedure TSynGutterPartList.RegisterItem(AnItem: TSynObjectListItem);
begin
  TSynGutterPartBase(AnItem).OnChange := {$IFDEF FPC}@{$ENDIF}DoChange;
  TSynGutterPartBase(AnItem).OnGutterClick := {$IFDEF FPC}@{$ENDIF}Gutter.DoDefaultGutterClick;
  inherited RegisterItem(AnItem);
end;

function TSynGutterPartList.GetSynEdit: TSynEditBase;
begin
  Result := TSynEditBase(Owner);
end;

function TSynGutterPartList.GetPart(Index: Integer): TSynGutterPartBase;
begin
  Result := TSynGutterPartBase(BaseItems[Index]);
end;

procedure TSynGutterPartList.PutPart(Index: Integer; const AValue: TSynGutterPartBase);
begin
  BaseItems[Index] := TSynObjectListItem(AValue);
end;

function TSynGutterPartList.GetByClass(AClass: TSynGutterPartBaseClass; Index: Integer): TSynGutterPartBase;
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

function TSynGutterPartList.GetByClassCount(AClass: TSynGutterPartBaseClass): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count -1 do
    if Part[i] is AClass then
      inc(Result);
end;


end.

