unit DebuggerTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, laz.VirtualTrees, SpinEx, LMessages, Controls,
  ImgList;

type

  TDbgTreeView = class;

  PVTVirtualItemNodeEnumeration = ^TVTVirtualItemNodeEnumeration;

  { TVTVirtualItemNodeEnumerator }

  TVTVirtualItemNodeEnumerator = class
  private
    FNode: PVirtualNode;
    FCanModeNext: Boolean;
    FEnumeration: PVTVirtualItemNodeEnumeration;
    function GetCurrent: PVirtualNode; {$ifdef COMPILER_10_UP}inline;{$endif}
  public
    function MoveNext: Boolean; {$ifdef COMPILER_10_UP}inline;{$endif}
    property Current: PVirtualNode read GetCurrent;
  end;

  { TVTVirtualItemNodeEnumeration }

  TVTVirtualItemNodeEnumeration = object
  private
    FTree: TDbgTreeView;
    FMode: TVZVirtualNodeEnumerationMode;
    FConsiderChildrenAbove, FIncludeNonItemNodes: Boolean;
    FControlNodes: Boolean;
    FNextNode: PVirtualNode;
    function GetNext(Node: PVirtualNode): PVirtualNode;
    function DoGetNext(Node: PVirtualNode): PVirtualNode;
  public
    function GetEnumerator: TVTVirtualItemNodeEnumerator;
  end;

  TDbgTreeNodeData = record
    Item: TObject;  // Must be the first field.  Node.AddChild will write the new "Item" at UserData^  (aka the memory at the start of UserData)
    ImageIndex: Array of Integer;
    CachedText: Array of String;
    Control: TControl;
    PrevControlNode, NextControlNode: PVirtualNode;
  end;
  PDbgTreeNodeData = ^TDbgTreeNodeData;

  TItemRemovedEvent = procedure (Sender: TDbgTreeView; AnItem: TObject; ANode: PVirtualNode) of object;

  { TDbgTreeView }

  TDbgTreeView = class(TLazVirtualStringTree)
  private
    FFirstControlNode: PVirtualNode; // not ordered
    FOnItemRemoved: TItemRemovedEvent;
    function GetNodeControl(Node: PVirtualNode): TControl;
    function GetNodeImageIndex(Node: PVirtualNode; AColumn: integer): Integer;
    function GetNodeItem(Node: PVirtualNode): TObject;
    function GetNodeText(Node: PVirtualNode; AColumn: integer): String;
    procedure SetNodeControl(Node: PVirtualNode; AValue: TControl);
    procedure SetNodeImageIndex(Node: PVirtualNode; AColumn: integer;
      AValue: Integer);
    procedure SetNodeItem(Node: PVirtualNode; AValue: TObject);
    procedure SetNodeText(Node: PVirtualNode; AColumn: integer; AValue: String);
    procedure ChangeControl(Node: PVirtualNode; NData: PDbgTreeNodeData; AControl: TControl);
  protected
    procedure CheckControlsVisible;
    procedure EndUpdate; override;
    function DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions;
      ClipRect: PRect = nil): Boolean; override;
    function DoCollapsing(Node: PVirtualNode): Boolean; override;
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
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer
      ): TCustomImageList; override;
  public
    function GetNodeData(Node: PVirtualNode): PDbgTreeNodeData; reintroduce;

    function GetFocusedNode(OnlySelected: Boolean = True; AnIncludeControlNodes: Boolean = False): PVirtualNode;
    function FocusedData(OnlySelected: Boolean = True): PDbgTreeNodeData;
    function FocusedItem(OnlySelected: Boolean = True): TObject;

    procedure SelectNode(Node: PVirtualNode; ASetFocus: boolean = True);
    function FindNodeForItem(AnItem: TObject): PVirtualNode;
    function FindNodeForControl(AControl: TObject): PVirtualNode;
    function FindNodeForText(AText: String; AColumn: integer; ATopLvlOnly: Boolean = False): PVirtualNode;
    procedure DeleteNodeEx(Node: PVirtualNode; FreeItem: Boolean; Reindex: Boolean = True);
    // LazMoveTo: Don't mess with children
    procedure LazMoveTo(Source, Target: PVirtualNode; Mode: TVTNodeAttachMode);

    (* NoInitItemNodes, SelectedItemNodes
       - By default iterate only nodes WITHOUT control
       - Allow the current node to be deleted, or moved (if moved, it can be seen twice)
    *)
    function NoInitItemNodes(ConsiderChildrenAbove: Boolean = False; IncludeNonItemNodes: Boolean = False): TVTVirtualItemNodeEnumeration;
    function SelectedItemNodes(ConsiderChildrenAbove: Boolean = False; IncludeNonItemNodes: Boolean = False): TVTVirtualItemNodeEnumeration;
    function ControlNodes: TVTVirtualItemNodeEnumeration;

    property NodeItem[Node: PVirtualNode]: TObject read GetNodeItem write SetNodeItem;
    property NodeText[Node: PVirtualNode; AColumn: integer]: String read GetNodeText write SetNodeText;
    property NodeImageIndex[Node: PVirtualNode; AColumn: integer]: Integer read GetNodeImageIndex write SetNodeImageIndex;
    property NodeControl[Node: PVirtualNode]: TControl read GetNodeControl write SetNodeControl;

    property OnItemRemoved: TItemRemovedEvent read FOnItemRemoved write FOnItemRemoved;
  end;

implementation

{ TVTVirtualItemNodeEnumerator }

function TVTVirtualItemNodeEnumerator.GetCurrent: PVirtualNode;
begin
  Result := FNode;
end;

function TVTVirtualItemNodeEnumerator.MoveNext: Boolean;
begin
  Result := FCanModeNext;
  if Result then
  begin
    FNode := FEnumeration^.GetNext(FNode);
    Result := FNode <> nil;
    FCanModeNext := Result;
  end;
end;

{ TVTVirtualItemNodeEnumeration }

function TVTVirtualItemNodeEnumeration.GetEnumerator: TVTVirtualItemNodeEnumerator;
begin
  Result := TVTVirtualItemNodeEnumerator.Create;
  Result.FNode := nil;
  Result.FCanModeNext := True;
  Result.FEnumeration := @Self;
  FNextNode := DoGetNext(nil);
end;

function TVTVirtualItemNodeEnumeration.GetNext(Node: PVirtualNode
  ): PVirtualNode;
begin
  Result := FNextNode;
  if FNextNode <> nil then
    FNextNode := DoGetNext(FNextNode); // if the current node gets deleted or moved, continue from the original pos
end;

function TVTVirtualItemNodeEnumeration.DoGetNext(Node: PVirtualNode
  ): PVirtualNode;
begin
  if FControlNodes then begin
    if Node = nil then
      Result := FTree.FFirstControlNode
    else
      Result := FTree.GetNodeData(Node)^.NextControlNode;
    exit;
  end;

  repeat
    case FMode of
      vneNoInit:
        if Node = nil then
          Result := FTree.GetFirstNoInit(FConsiderChildrenAbove)
        else
          Result := FTree.GetNextNoInit(Node, FConsiderChildrenAbove);

      vneSelected:
        if Node = nil then
          Result := FTree.GetFirstSelected(FConsiderChildrenAbove)
        else
          Result := FTree.GetNextSelected(Node, FConsiderChildrenAbove);
    else
      Result := nil;
    end;
    Node := Result;
  until (Result=nil) or FIncludeNonItemNodes or (FTree.NodeControl[Result] = nil);
end;

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

function TDbgTreeView.GetNodeImageIndex(Node: PVirtualNode; AColumn: integer
  ): Integer;
var
  Data: PDbgTreeNodeData;
begin
  Result := -1;
  Data := GetNodeData(Node);
  if (Data <> nil) and (AColumn < Length(Data^.ImageIndex)) then
    Result := Data^.ImageIndex[AColumn] - 1;
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
  ChangeControl(Node, Data, AValue);
  if AValue <> nil then begin
    AValue.Visible := False;
    AValue.Parent := Self;
    AValue.AutoSize := False;
  end;
end;

procedure TDbgTreeView.SetNodeImageIndex(Node: PVirtualNode; AColumn: integer;
  AValue: Integer);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then begin
    if AColumn >= Length(Data^.ImageIndex) then
      SetLength(Data^.ImageIndex, AColumn + 1);
    Data^.ImageIndex[AColumn] := AValue + 1;
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

procedure TDbgTreeView.ChangeControl(Node: PVirtualNode;
  NData: PDbgTreeNodeData; AControl: TControl);
var
  NData2: PDbgTreeNodeData;
begin
  if (NData^.Control = nil) = (AControl = nil) then begin
    NData^.Control := AControl;
    exit;
  end;

  NData^.Control := AControl;

  if AControl = nil then begin
    // node must have had a control
    if NData^.PrevControlNode = nil then begin
      assert(FFirstControlNode = Node, 'TDbgTreeView.DoFreeNode: FFirstControlNode = Node');
      FFirstControlNode := NData^.NextControlNode;
    end
    else begin
      NData2 := GetNodeData(NData^.PrevControlNode);
      assert((NData2 <> nil) and (NData2^.NextControlNode = Node), 'TDbgTreeView.DoFreeNode: (NData2 <> nil) and (NData2^.NextControlNode = Node)');
      NData2^.NextControlNode := NData^.NextControlNode;
    end;

    if NData^.NextControlNode <> nil then begin
      NData2 := GetNodeData(NData^.NextControlNode);
      assert((NData2 <> nil) and (NData2^.PrevControlNode = Node), 'TDbgTreeView.DoFreeNode: (NData2 <> nil) and (NData2^.PrevControlNode = Node)');
      NData2^.PrevControlNode := NData^.PrevControlNode;
    end;
    NData^.PrevControlNode := nil;
    NData^.NextControlNode := nil;
  end
  else begin
    assert((NData^.NextControlNode = nil) and (NData^.PrevControlNode = nil), 'TDbgTreeView.ChangeControl: (NData^.NextControlNode = nil) and (NData^.PrevControlNode = nil)');
    assert(FFirstControlNode <> Node, 'TDbgTreeView.ChangeControl: FFirstControlNode <> Node');
    if FFirstControlNode <> nil then begin
      NData2 := GetNodeData(FFirstControlNode);
      NData2^.PrevControlNode := Node;
      NData^.NextControlNode := FFirstControlNode;
    end;
    FFirstControlNode := Node;
  end;
end;

procedure TDbgTreeView.CheckControlsVisible;
var
  VNode: PVirtualNode;
  Y, H: Integer;
  Ctrl: TControl;
  Chg: Boolean;
begin
  if (FFirstControlNode = nil) or (UpdateCount > 0) then
    exit;

  Y := OffsetY;
  Chg := False;
  for VNode in VisibleNoInitNodes do begin
    Ctrl := NodeControl[VNode];
    H := NodeHeight[VNode];
    if (Ctrl <> nil) then begin
      if (Y < 0) or (Y + H >= ClientHeight) then begin
        Chg := Chg or Ctrl.Visible;
        Ctrl.Visible := False;
      end
      else begin
        Chg := Chg or
               (Ctrl.Top     <> Y) or
               (Ctrl.Height  <> H) or
               (Ctrl.Visible <> True);

        Ctrl.Top     := Y;
        Ctrl.Height  := H;
        Ctrl.Visible := True;
      end;
    end;
    Y := Y + H;
  end;

  if Chg then
    Invalidate;
end;

procedure TDbgTreeView.EndUpdate;
begin
  inherited EndUpdate;
  CheckControlsVisible;
end;

function TDbgTreeView.DoSetOffsetXY(Value: TPoint;
  Options: TScrollUpdateOptions; ClipRect: PRect): Boolean;
begin
  Result := inherited DoSetOffsetXY(Value, Options, ClipRect);
  {$if defined(LCLGtk) or defined(LCLGtk2)}
  CheckControlsVisible;
  {$endif}
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
  CheckControlsVisible;

  Result := inherited DoCollapsing(Node);
end;

procedure TDbgTreeView.DoExpanded(Node: PVirtualNode);
begin
  inherited DoExpanded(Node);
  CheckControlsVisible;
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

    NData^.Control.Free;
    ChangeControl(Node, NData, nil);
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

function TDbgTreeView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer
  ): TCustomImageList;
begin
  Result := nil;
  Ghosted := False;
  Index := GetNodeImageIndex(Node, Column);
end;

function TDbgTreeView.GetNodeData(Node: PVirtualNode): PDbgTreeNodeData;
begin
  Result := PDbgTreeNodeData(inherited GetNodeData(Node));
end;

function TDbgTreeView.GetFocusedNode(OnlySelected: Boolean;
  AnIncludeControlNodes: Boolean): PVirtualNode;
begin
  Result := FocusedNode;
  if (not AnIncludeControlNodes) and (Result <> nil) and (NodeControl[Result] <> nil) then
    Result := nil;
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

function TDbgTreeView.FindNodeForControl(AControl: TObject): PVirtualNode;
var
  VNode: PVirtualNode;
begin
  for VNode in NoInitNodes do begin
    if GetNodeControl(VNode) = AControl then
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

function TDbgTreeView.NoInitItemNodes(ConsiderChildrenAbove: Boolean;
  IncludeNonItemNodes: Boolean): TVTVirtualItemNodeEnumeration;
begin
  Result.FMode := vneNoInit;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
  Result.FControlNodes := False;
  Result.FIncludeNonItemNodes := IncludeNonItemNodes;
end;

function TDbgTreeView.SelectedItemNodes(ConsiderChildrenAbove: Boolean;
  IncludeNonItemNodes: Boolean): TVTVirtualItemNodeEnumeration;
begin
  Result.FMode := vneSelected;
  Result.FTree := Self;
  Result.FConsiderChildrenAbove := ConsiderChildrenAbove;
  Result.FControlNodes := False;
  Result.FIncludeNonItemNodes := IncludeNonItemNodes;
end;

function TDbgTreeView.ControlNodes: TVTVirtualItemNodeEnumeration;
begin
  Result.FMode := vneNoInit;
  Result.FTree := Self;
  Result.FControlNodes := True;
end;

initialization
  RegisterClass(TDbgTreeView);

end.

