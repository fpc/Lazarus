unit DebuggerTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, SpinEx, LMessages, Controls;

type

  TDbgTreeView = class;

  TDbgTreeNodeData = record
    Item: TObject;  // Must be the first field.  Node.AddChild will write the new "Item" at UserData^  (aka the memory at the start of UserData)
    CachedText: Array of String;
    Control: TControl;
  end;
  PDbgTreeNodeData = ^TDbgTreeNodeData;

  TItemRemovedEvent = procedure (Sender: TDbgTreeView; AnItem: TObject; ANode: PVirtualNode) of object;

  { TDbgTreeView }

  TDbgTreeView = class(TLazVirtualStringTree)
  private
    FFirstVisibleBeforeExpanding, FLastVisibleBeforeExpanding: PVirtualNode;
    FOnItemRemoved: TItemRemovedEvent;
    function GetNodeControl(Node: PVirtualNode): TControl;
    function GetNodeItem(Node: PVirtualNode): TObject;
    function GetNodeText(Node: PVirtualNode; AColumn: integer): String;
    procedure SetNodeControl(Node: PVirtualNode; AValue: TControl);
    procedure SetNodeItem(Node: PVirtualNode; AValue: TObject);
    procedure SetNodeText(Node: PVirtualNode; AColumn: integer; AValue: String);
  protected
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
    function DoExpanding(Node: PVirtualNode): Boolean; override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure ValidateNodeDataSize(var Size: Integer); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DetermineLineImageAndSelectLevel(Node: PVirtualNode;
      var LineImage: TLineImage): Integer; override;
    function DetermineDropMode(const P: TPoint; var HitInfo: THitInfo;
      var NodeRect: TRect): TDropMode; override;
    procedure HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo); override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var AText: String); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
  public
    function GetNodeData(Node: PVirtualNode): PDbgTreeNodeData; reintroduce;

    function GetFocusedNode(OnlySelected: Boolean = True): PVirtualNode;
    function FocusedData(OnlySelected: Boolean = True): PDbgTreeNodeData;
    function FocusedItem(OnlySelected: Boolean = True): TObject;

    procedure SelectNode(Node: PVirtualNode; ASetFocus: boolean = True);
    function FindNodeForItem(AnItem: TObject): PVirtualNode;
    function FindNodeForText(AText: String; AColumn: integer; ATopLvlOnly: Boolean = False): PVirtualNode;
    procedure DeleteNodeEx(Node: PVirtualNode; FreeItem: Boolean; Reindex: Boolean = True);
    // LazMoveTo: Don't mess with children
    procedure LazMoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode);

    property NodeItem[Node: PVirtualNode]: TObject read GetNodeItem write SetNodeItem;
    property NodeText[Node: PVirtualNode; AColumn: integer]: String read GetNodeText write SetNodeText;
    property NodeControl[Node: PVirtualNode]: TControl read GetNodeControl write SetNodeControl;

    property OnItemRemoved: TItemRemovedEvent read FOnItemRemoved write FOnItemRemoved;
  end;

implementation

{ TDbgTreeView }

function TDbgTreeView.GetNodeControl(Node: PVirtualNode): TControl;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.Control;
end;

function TDbgTreeView.GetNodeItem(Node: PVirtualNode): TObject;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.Item;
end;

function TDbgTreeView.GetNodeText(Node: PVirtualNode; AColumn: integer): String;
var
  Data: PDbgTreeNodeData;
begin
  Result := '';
  Data := GetNodeData(Node);
  if (Data <> nil) and (AColumn < Length(Data^.CachedText)) then
    Result := Data^.CachedText[AColumn];
end;

procedure TDbgTreeView.SetNodeControl(Node: PVirtualNode; AValue: TControl);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data = nil then
    exit;
  if Data^.Control = AValue then
    exit;

  Data^.Control.Free;
  Data^.Control := AValue;
  if AValue <> nil then begin
    AValue.Visible := False;
    AValue.Parent := Self;
    AValue.AutoSize := False;
  end;
end;

procedure TDbgTreeView.SetNodeItem(Node: PVirtualNode; AValue: TObject);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);

  if Data <> nil then begin

    if (FOnItemRemoved <> nil) and (Data^.Item <> nil) and (Data^.Item <> AValue) then
      FOnItemRemoved(Self, Data^.Item, Node);

    Data^.Item := AValue;
  end;
end;

procedure TDbgTreeView.SetNodeText(Node: PVirtualNode; AColumn: integer;
  AValue: String);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then begin
    if AColumn >= Length(Data^.CachedText) then
      SetLength(Data^.CachedText, AColumn + 1);
    Data^.CachedText[AColumn] := AValue;
  end;
end;

function TDbgTreeView.DoCollapsing(Node: PVirtualNode): Boolean;
  procedure RecursivelyHideControls(N: PVirtualNode);
  var
    N2: PVirtualNode;
    NData: PDbgTreeNodeData;
  begin
    NData := GetNodeData(N);
    if NData^.Control <> nil then
      NData^.Control.Visible := False;

    while N <> nil do begin
      N2 := GetFirstChildNoInit(N);
      if N2 <> nil then
        RecursivelyHideControls(N2);
      N := GetNextSiblingNoInit(N);
    end;
  end;
var
  n: PVirtualNode;
begin
  n := GetFirstChildNoInit(Node);
  if n <> nil then
    RecursivelyHideControls(n);

  Result := inherited DoCollapsing(Node);
end;

function TDbgTreeView.DoExpanding(Node: PVirtualNode): Boolean;
begin
  FFirstVisibleBeforeExpanding := GetFirstVisibleNoInit(Node);
  FLastVisibleBeforeExpanding := GetLastVisibleNoInit(Node);
  Result := inherited DoExpanding(Node);
end;

procedure TDbgTreeView.DoExpanded(Node: PVirtualNode);
var
  N: PVirtualNode;
  NData: PDbgTreeNodeData;
begin
  N := FFirstVisibleBeforeExpanding;
  if N = nil then
    N := GetFirstNoInit;
  while (N <> nil) do begin
    NData := GetNodeData(N);
    if NData^.Control <> nil then
      NData^.Control.Visible := False;

    if N = FLastVisibleBeforeExpanding then
      break;
    N := GetNextNoInit(N);
  end;
  inherited DoExpanded(Node);
end;

procedure TDbgTreeView.ValidateNodeDataSize(var Size: Integer);
begin
  Size := SizeOf(TDbgTreeNodeData);
end;

procedure TDbgTreeView.DoFreeNode(Node: PVirtualNode);
var
  NData: PDbgTreeNodeData;
begin
  NData := GetNodeData(Node);
  if NData <> nil then begin
    if (FOnItemRemoved <> nil) and (NData^.Item <> nil) then
      FOnItemRemoved(Self, NData^.Item, Node);

    FreeAndNil(NData^.Control);
    NData^ := Default(TDbgTreeNodeData);
  end;

  inherited DoFreeNode(Node);
end;

function TDbgTreeView.DetermineLineImageAndSelectLevel(Node: PVirtualNode;
  var LineImage: TLineImage): Integer;
begin
  Result := inherited DetermineLineImageAndSelectLevel(Node, LineImage);
  if Length(LineImage) > 1 then
    LineImage[0] := ltNone
  else
  if (Length(LineImage) > 0) and (LineImage[0] <> ltNone) then
    LineImage[0] := ltRight;
end;

function TDbgTreeView.DetermineDropMode(const P: TPoint; var HitInfo: THitInfo;
  var NodeRect: TRect): TDropMode;
begin
  if Assigned(HitInfo.HitNode) then
  begin
    if ((NodeRect.Top + NodeRect.Bottom) div 2) > P.Y then
      Result := dmAbove
    else
      Result := dmBelow;
  end
  else
    Result := dmNowhere;
end;

procedure TDbgTreeView.HandleMouseDown(var Message: TLMMouse;
  var HitInfo: THitInfo);
begin
  if Message.Msg = LM_LBUTTONDOWN then
    DragMode := dmAutomatic
  else
    DragMode := dmManual;
  inherited HandleMouseDown(Message, HitInfo);
end;

procedure TDbgTreeView.HandleMouseDblClick(var Message: TLMMouse;
  const HitInfo: THitInfo);
begin
  inherited HandleMouseDblClick(Message, HitInfo);
  if (HitInfo.HitNode = nil) and (HitInfo.HitColumn < 0) then
    DoNodeDblClick(HitInfo);

end;

procedure TDbgTreeView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var AText: String);
begin
  if Assigned(OnGetText) then begin
    inherited DoGetText(Node, Column, TextType, AText);
  end
  else begin
    AText := NodeText[Node, Column];
  end;
end;

procedure TDbgTreeView.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  NData: PDbgTreeNodeData;
  r: TRect;
begin
  NData := GetNodeData(PaintInfo.Node);
  if NData^.Control <> nil then begin
    if PaintInfo.Column = 0 then begin
      r := GetDisplayRect(PaintInfo.Node, 0, True, False);
      r.Right := ClientWidth - 1;
      NData^.Control.BoundsRect := r;
      NData^.Control.Visible := True;
      if (r.Top < (r.Bottom - r.Height) * 2 + 5) or
         (r.Bottom > ClientHeight - (r.Bottom - r.Height) * 2 - 5)
      then
        NData^.Control.Invalidate;
    end;
    exit;
  end;

  inherited DoPaintNode(PaintInfo);
end;

function TDbgTreeView.GetNodeData(Node: PVirtualNode): PDbgTreeNodeData;
begin
  Result := PDbgTreeNodeData(inherited GetNodeData(Node));
end;

function TDbgTreeView.GetFocusedNode(OnlySelected: Boolean): PVirtualNode;
begin
  Result := FocusedNode;
  if (Result = nil) or (OnlySelected and not Selected[Result]) then
    Result := nil;
end;

function TDbgTreeView.FocusedData(OnlySelected: Boolean): PDbgTreeNodeData;
var
  VNode: PVirtualNode;
begin
  Result := nil;
  VNode := FocusedNode;
  if (VNode = nil) or (OnlySelected and not Selected[VNode]) then
    exit;
  Result := PDbgTreeNodeData(GetNodeData(VNode));
end;

function TDbgTreeView.FocusedItem(OnlySelected: Boolean): TObject;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := FocusedData(OnlySelected);
  if Data <> nil then
    Result := Data^.Item;
end;

procedure TDbgTreeView.SelectNode(Node: PVirtualNode; ASetFocus: boolean);
begin
  ClearSelection;
  FocusedNode := Node;
  Selected[Node] := True;
end;

function TDbgTreeView.FindNodeForItem(AnItem: TObject): PVirtualNode;
var
  VNode: PVirtualNode;
begin
  for VNode in NoInitNodes do begin
    if GetNodeItem(VNode) = AnItem then
      exit(VNode);
  end;
  Result := nil;
end;

function TDbgTreeView.FindNodeForText(AText: String; AColumn: integer;
  ATopLvlOnly: Boolean): PVirtualNode;
var
  VNode: PVirtualNode;
begin
  VNode := GetFirstNoInit;
  while VNode <> nil do begin
    if GetNodeText(VNode, AColumn) = AText then
      exit(VNode);
    if ATopLvlOnly then
      VNode := GetNextSiblingNoInit(VNode)
    else
      VNode := GetNextNoInit(VNode);
  end;
  Result := nil;
end;

procedure TDbgTreeView.DeleteNodeEx(Node: PVirtualNode; FreeItem: Boolean;
  Reindex: Boolean);
var
  Item: TObject;
begin
  if Node = nil then
    exit;
  Item := GetNodeItem(Node);

  DeleteNode(Node, Reindex);
  if FreeItem then
    Item.Free;
end;

procedure TDbgTreeView.LazMoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode);
var
  NewNode: PVirtualNode;
begin
  if Target = nil then
  begin
    Target := RootNode;
    Mode := amAddChildFirst;
  end;

  if Target = RootNode then
  begin
    case Mode of
      amInsertBefore:
        Mode := amAddChildFirst;
      amInsertAfter:
        Mode := amAddChildLast;
    end;
  end;

  if (Source <> Target) and HasAsParent(Target, Source) then
    exit;

  // Disconnect from old location.
  InternalDisconnectNode(Source, True);
  // Connect to new location.
  InternalConnectNode(Source, Target, Self, Mode);
  DoNodeMoved(Source);


  InvalidateCache;
  if (UpdateCount = 0) then
  begin
    ValidateCache;
    UpdateScrollBars(True);
    Invalidate;
  end;
  StructureChange(Source, crNodeMoved);
end;

initialization
  RegisterClass(TDbgTreeView);

end.

