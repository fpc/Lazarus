unit DebuggerTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, laz.VirtualTrees, SpinEx, GraphType, LMessages, Controls,
  ImgList, LCLIntf, Graphics;

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
    RightSideButton: record
      ImageIndex: integer;
      Rect: TRect;
    end;
    CachedText: Array of String;
    CachedColumnData: array of record
      ShortenWidth1, ShortenWidth2: integer;
      ShortenResText: string;
      ColWidth: integer;
    end;
    CachedFirstCellLeft: integer;
    Control: TControl;
    ControlTop, ControlWidth, ControlHeight: integer; // ControlWidth -1 = full
    ControlHidden, ControlOutside: Boolean; // below current node, overlaps next
    PrevControlNode, NextControlNode: PVirtualNode;
  end;
  PDbgTreeNodeData = ^TDbgTreeNodeData;

  TItemRemovedEvent = procedure (Sender: TDbgTreeView; AnItem: TObject; ANode: PVirtualNode) of object;
  TDetermineDropModeEvent = procedure (const P: TPoint; var HitInfo: THitInfo; var NodeRect: TRect; var DropMode: TDropMode) of object;
  TDbgTreeRightButtonClickEvent = procedure (Sender: TDbgTreeView; ANode: PVirtualNode) of object;

  { TDbgTreeView }

  TDbgTreeView = class(TLazVirtualStringTree)
  private
    FCheckControlsVisibleLock: integer;
    FCheckControlsVisibleRunning, FCheckControlsVisibleAgain: Boolean;
    FInToggle: boolean;
    FFirstControlNode: PVirtualNode; // not ordered
    FLazImages: TCustomImageList;
    FOnDetermineDropMode: TDetermineDropModeEvent;
    FOnItemRemoved: TItemRemovedEvent;
    FOnNodeRightButtonClick: TDbgTreeRightButtonClickEvent;

    // Text/Image
    function  GetNodeImageIndex(Node: PVirtualNode; AColumn: integer): Integer;
    function  GetNodeText(Node: PVirtualNode; AColumn: integer): String;
    function  GetNodeRightButtonImgIdx(Node: PVirtualNode): Integer;
    function  GetNodeRightButtonRect(Node: PVirtualNode): TRect;
    procedure SetNodeImageIndex(Node: PVirtualNode; AColumn: integer; AValue: Integer);
    procedure SetNodeText(Node: PVirtualNode; AColumn: integer; AValue: String);
    procedure SetNodeRightButtonImgIdx(Node: PVirtualNode; AValue: Integer);
    // Item
    procedure SetNodeItem(Node: PVirtualNode; AValue: TObject);
    function GetNodeItem(Node: PVirtualNode): TObject;
    // Control
    function  GetNodeControl(Node: PVirtualNode): TControl;
    function  GetNodeControlVisible(Node: PVirtualNode): Boolean;
    function  GetNodeControlOutside(Node: PVirtualNode): Boolean;
    function  GetNodeControlWidth(Node: PVirtualNode): Integer;
    function  GetNodeControlHeight(Node: PVirtualNode): Integer;
    procedure SetNodeControl(Node: PVirtualNode; AValue: TControl);
    procedure SetNodeControlVisible(Node: PVirtualNode; AValue: Boolean);
    procedure SetNodeControlOutside(Node: PVirtualNode; AValue: Boolean);
    procedure SetNodeControlWidth(Node: PVirtualNode; AValue: Integer);
    procedure SetNodeControlHeight(Node: PVirtualNode; AValue: Integer);
    procedure ChangeControl(Node: PVirtualNode; NData: PDbgTreeNodeData; AControl: TControl);
    function  GetIsVisible(Node: PVirtualNode): Boolean; reintroduce;
    procedure SetIsVisible(Node: PVirtualNode; AValue: Boolean); reintroduce;
  protected
    procedure DoAllAutoSize; override;
    procedure CheckControlsVisible;
    procedure VisibleChanged; override;

    // Paint / Invalidate
    procedure EndUpdate; override;
    function  DoSetOffsetXY(Value: TPoint; Options: TScrollUpdateOptions; ClipRect: PRect = nil): Boolean; override;
    function  DoCollapsing(Node: PVirtualNode): Boolean; override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure DoStateChange(Enter: TVirtualTreeStates; Leave: TVirtualTreeStates = []); override;
    function  DetermineLineImageAndSelectLevel(Node: PVirtualNode; var LineImage: TLineImage): Integer; override;
    procedure DoBeforeItemErase(ACanvas: TCanvas; ANode: PVirtualNode; const ItemRect: TRect;
      var AColor: TColor; var EraseAction: TItemEraseAction); override;
    procedure PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: Integer); override;  // the background
    procedure PaintTreeLines(const PaintInfo: TVTPaintInfo; VAlignment, IndentSize: Integer;
      LineImage: TLineImage); override;
    procedure PaintNodeButton(ACanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const R: TRect; ButtonX, ButtonY: Integer; ABidiMode: TBiDiMode); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;

    // Mouse
    function DetermineDropMode(const P: TPoint; var HitInfo: THitInfo;
      var NodeRect: TRect): TDropMode; override;
    procedure HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo); override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;

    // Data
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var AText: String); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer
      ): TCustomImageList; override;
    function DoGetNodeWidth(ANode: PVirtualNode; AColumn: TColumnIndex; ACanvas: TCanvas =
      nil): Integer; override;
    function DoShortenString(ACanvas: TCanvas; ANode: PVirtualNode; AColumn: TColumnIndex;
      const S: String; AWidth: Integer; AnEllipsisWidth: Integer = 0): String; override;
    procedure DoNodeMoved(ANode: PVirtualNode); override;

    procedure ValidateNodeDataSize(var Size: Integer); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
  public
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

    function GetNodeData(Node: PVirtualNode): PDbgTreeNodeData; reintroduce;
    property NodeItem[Node: PVirtualNode]: TObject read GetNodeItem write SetNodeItem;
    property NodeText[Node: PVirtualNode; AColumn: integer]: String read GetNodeText write SetNodeText;
    property NodeImageIndex[Node: PVirtualNode; AColumn: integer]: Integer read GetNodeImageIndex write SetNodeImageIndex;
    property IsVisible[Node: PVirtualNode]: Boolean read GetIsVisible write SetIsVisible;
    // May need reintroduction //property NodeHeight[Node: PVirtualNode]: Cardinal read GetNodeHeight write SetNodeHeight;
    // Control
    property NodeControl[Node: PVirtualNode]: TControl read GetNodeControl write SetNodeControl;
    property NodeControlVisible[Node: PVirtualNode]: Boolean read GetNodeControlVisible write SetNodeControlVisible;
    property NodeControlWidth[Node: PVirtualNode]: Integer read GetNodeControlWidth write SetNodeControlWidth;
    property NodeControlHeight[Node: PVirtualNode]: Integer read GetNodeControlHeight write SetNodeControlHeight;
    property NodeControlOutside[Node: PVirtualNode]: Boolean read GetNodeControlOutside write SetNodeControlOutside;

    property OnItemRemoved: TItemRemovedEvent read FOnItemRemoved write FOnItemRemoved;
    property OnDetermineDropMode: TDetermineDropModeEvent read FOnDetermineDropMode write FOnDetermineDropMode;
    property LastDropMode;

    property LazImages: TCustomImageList read FLazImages write FLazImages;
    property NodeRightButtonImgIdx[Node: PVirtualNode]: Integer read GetNodeRightButtonImgIdx write SetNodeRightButtonImgIdx;
    property NodeRightButtonRect[Node: PVirtualNode]: TRect read GetNodeRightButtonRect;
    property OnNodeRightButtonClick: TDbgTreeRightButtonClickEvent read FOnNodeRightButtonClick write FOnNodeRightButtonClick;
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

function TDbgTreeView.GetNodeText(Node: PVirtualNode; AColumn: integer): String;
var
  Data: PDbgTreeNodeData;
begin
  Result := '';
  Data := GetNodeData(Node);
  if (Data <> nil) and (AColumn < Length(Data^.CachedText)) then
    Result := Data^.CachedText[AColumn];
end;

function TDbgTreeView.GetNodeRightButtonImgIdx(Node: PVirtualNode): Integer;
var
  Data: PDbgTreeNodeData;
begin
  Result := -1;
  Data := GetNodeData(Node);
  if (Data <> nil) then
    Result := Data^.RightSideButton.ImageIndex - 1;
end;

function TDbgTreeView.GetNodeRightButtonRect(Node: PVirtualNode): TRect;
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if (Data <> nil) then
    Result := Data^.RightSideButton.Rect
  else
    Result := Rect(-1,-1,-1,-1);
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

procedure TDbgTreeView.SetNodeText(Node: PVirtualNode; AColumn: integer;
  AValue: String);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then begin
    if AColumn >= Length(Data^.CachedText) then begin
      SetLength(Data^.CachedText, AColumn + 1);
      SetLength(Data^.CachedColumnData, AColumn + 1);
    end;
    Data^.CachedText[AColumn] := AValue;
    Data^.CachedColumnData[AColumn].ColWidth := -1;
    Data^.CachedColumnData[AColumn].ShortenWidth1 := -1;
    Data^.CachedColumnData[AColumn].ShortenResText := '';
  end;
end;

procedure TDbgTreeView.SetNodeRightButtonImgIdx(Node: PVirtualNode; AValue: Integer);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then begin
    if Data^.RightSideButton.ImageIndex <> AValue then begin
      InvalidateNode(Node);
      Data^.RightSideButton.ImageIndex := AValue + 1;
    end;
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

function TDbgTreeView.GetNodeItem(Node: PVirtualNode): TObject;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.Item;
end;

function TDbgTreeView.GetNodeControl(Node: PVirtualNode): TControl;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.Control;
end;

function TDbgTreeView.GetNodeControlVisible(Node: PVirtualNode): Boolean;
var
  Data: PDbgTreeNodeData;
begin
  Result := False;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := not Data^.ControlHidden;
end;

function TDbgTreeView.GetNodeControlOutside(Node: PVirtualNode): Boolean;
var
  Data: PDbgTreeNodeData;
begin
  Result := False;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.ControlOutside;
end;

function TDbgTreeView.GetNodeControlWidth(Node: PVirtualNode): Integer;
var
  Data: PDbgTreeNodeData;
begin
  Result := 0;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.ControlWidth;
end;

function TDbgTreeView.GetNodeControlHeight(Node: PVirtualNode): Integer;
var
  Data: PDbgTreeNodeData;
begin
  Result := 0;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.ControlHeight;
end;

procedure TDbgTreeView.SetNodeControl(Node: PVirtualNode; AValue: TControl);
var
  Data: PDbgTreeNodeData;
  h: LongWord;
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

  if (Data^.Item <> nil) and (Data^.ControlTop = 0) then begin
    h := NodeHeight[Node];
    Data^.ControlTop := h;
    Data^.ControlHeight := DefaultNodeHeight;
    if (Data^.Control <> nil) and (not Data^.ControlHidden) and not Data^.ControlOutside then
      NodeHeight[Node] := h + Data^.ControlHeight
    else
      NodeHeight[Node] := h;
  end
  else
  if AValue = nil then begin
    if Data^.ControlTop > 0 then
      NodeHeight[Node] := Data^.ControlTop;
    Data^.ControlTop := 0;
  end;
end;

procedure TDbgTreeView.SetNodeControlVisible(Node: PVirtualNode; AValue: Boolean);
var
  Data: PDbgTreeNodeData;
  h: LongWord;
begin
  Data := GetNodeData(Node);
  if Data = nil then
    exit;
  if not Data^.ControlHidden = AValue then
    exit;

  Data^.ControlHidden := not AValue;

  if (Data^.ControlTop = 0) then begin
    h := NodeHeight[Node];
    if Data^.Item <> nil then
      Data^.ControlTop := h;
  end
  else
    h := Data^.ControlTop;

  if (Data^.Control <> nil) and Data^.ControlHidden  then
    Data^.Control.Visible := False;

  if (Data^.Item <> nil) then begin
    if (Data^.Control <> nil) and (not Data^.ControlHidden) and not Data^.ControlOutside then
      NodeHeight[Node] := h + Data^.ControlHeight
    else
      NodeHeight[Node] := h;
  end;
  InvalidateToBottom(Node);
end;

procedure TDbgTreeView.SetNodeControlOutside(Node: PVirtualNode; AValue: Boolean);
var
  Data: PDbgTreeNodeData;
  h: LongWord;
begin
  Data := GetNodeData(Node);
  if Data = nil then
    exit;
  if Data^.ControlOutside = AValue then
    exit;

  Data^.ControlOutside := AValue;

  if (Data^.ControlTop = 0) then begin
    h := NodeHeight[Node];
    if Data^.Item <> nil then
      Data^.ControlTop := h;
  end
  else
    h := Data^.ControlTop;

  if (Data^.Item <> nil) then begin
    if (Data^.Control <> nil) and (not Data^.ControlHidden) and not Data^.ControlOutside then
      NodeHeight[Node] := h + Data^.ControlHeight
    else
      NodeHeight[Node] := h;
  end;
  InvalidateToBottom(Node);
end;

procedure TDbgTreeView.SetNodeControlWidth(Node: PVirtualNode; AValue: Integer);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data = nil then
    exit;
  if Data^.ControlWidth = AValue then
    exit;

  Data^.ControlWidth := AValue;
  InvalidateToBottom(Node);
end;

procedure TDbgTreeView.SetNodeControlHeight(Node: PVirtualNode; AValue: Integer);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data = nil then
    exit;
  if AValue < 0 then AValue := 0;
  if Data^.ControlHeight = AValue then
    exit;

  if (Data^.Item = nil) then // control takes full node
    NodeHeight[Node] := AValue
  else
  if (Data^.Control <> nil) and (not Data^.ControlHidden) and not Data^.ControlOutside then
    NodeHeight[Node] := NodeHeight[Node] + AValue - Data^.ControlHeight;

  Data^.ControlHeight := AValue;
  InvalidateToBottom(Node);
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

function TDbgTreeView.GetIsVisible(Node: PVirtualNode): Boolean;
begin
  Result := inherited IsVisible[Node];
end;

procedure TDbgTreeView.SetIsVisible(Node: PVirtualNode; AValue: Boolean);
var
  Data: PDbgTreeNodeData;
begin
  inherited IsVisible[Node] := AValue;

  Data := GetNodeData(Node);
  if (Data <> nil) and (Data^.Control <> nil) and (not AValue) then
    Data^.Control.Visible := False;
end;

procedure TDbgTreeView.DoAllAutoSize;
begin
  FCheckControlsVisibleRunning := True;
  inherited DoAllAutoSize;
  FCheckControlsVisibleRunning := False;
  if FCheckControlsVisibleAgain then
    CheckControlsVisible;
end;

procedure TDbgTreeView.CheckControlsVisible;
var
  VNode: PVirtualNode;
  NData: PDbgTreeNodeData;
  Y, Y2, H, H2, T, B, offs, Limit: Integer;
  Ctrl: TControl;
  Chg: Boolean;
  r2: TRect;
begin
  FCheckControlsVisibleAgain := FCheckControlsVisibleRunning;
  if (FCheckControlsVisibleLock > 0) or FCheckControlsVisibleRunning or
     (FFirstControlNode = nil) or (UpdateCount > 0) or
     (not TControl(self).IsVisible) or (not HandleAllocated)
  then
    exit;

  DisableAutoSizing;
  FCheckControlsVisibleRunning := True;
  try
    Limit := 5 + ClientHeight div (DefaultNodeHeight * 2);
    repeat
      dec(Limit);
      T := Header.Height;
      B := ClientHeight - 1 + T; // subtracted by laz.VirtualTree
      Y := OffsetY + T;
      Chg := False;
      for VNode in VisibleNoInitNodes do begin
        NData := GetNodeData(VNode);
        offs := 0;
        H := NodeHeight[VNode];
        if NData <> nil then begin
          Ctrl := NData^.Control;
          if NData^.ControlHidden then begin
            if (Ctrl <> nil) then
              Ctrl.Visible := False;
          end
          else
          if (Ctrl <> nil) then begin
            offs := NData^.ControlTop;
            Y2 := Y + offs;
            if NData^.ControlOutside then
              H2 := NData^.ControlHeight
            else
              H2 := H - offs;
            if (Y2 < T) or (Y2 >= B) then begin
              Chg := Chg or Ctrl.Visible;
              Ctrl.Visible := False;
            end
            else begin
              Chg := Chg or
                     (Ctrl.Top     <> Y2) or
                     (Ctrl.Height  <> H2) or
                     (Ctrl.Visible <> True);


              if NData^.ControlOutside then begin
                Ctrl.SetBounds(OffsetX, Y2, Ctrl.Width, H2);
              end
              else begin
                if (NData^.CachedFirstCellLeft <= 0) then begin
                  r2 := GetDisplayRect(vNode, 0, True, False);
                  NData^.CachedFirstCellLeft := r2.Left;
                end;
                Ctrl.SetBounds(NData^.CachedFirstCellLeft + OffsetX, Y2, Ctrl.Width, H2);
              end;
              ctrl.Visible := True;
              H2 := NodeHeight[VNode];
              if H <> H2 then
                Ctrl.Height := H2 - offs;
            end;
          end;
        end;
        Y := Y + NodeHeight[VNode];
      end;
    until (not FCheckControlsVisibleAgain) or (Limit <= 0);

  finally
    FCheckControlsVisibleAgain := False;
    FCheckControlsVisibleRunning := False;
    // EnableAutoSizing: Some controls may change their node's height
    EnableAutoSizing;
  end;
end;

procedure TDbgTreeView.VisibleChanged;
begin
  inc(FCheckControlsVisibleLock);
  inherited VisibleChanged;
  dec(FCheckControlsVisibleLock);
  CheckControlsVisible;
end;

procedure TDbgTreeView.EndUpdate;
begin
  inc(FCheckControlsVisibleLock);
  inherited EndUpdate;
  dec(FCheckControlsVisibleLock);
  CheckControlsVisible;
end;

function TDbgTreeView.DoSetOffsetXY(Value: TPoint;
  Options: TScrollUpdateOptions; ClipRect: PRect): Boolean;
begin
  inc(FCheckControlsVisibleLock);
  DisableAutoSizing;
  Result := inherited DoSetOffsetXY(Value, Options, ClipRect);
  dec(FCheckControlsVisibleLock);
  CheckControlsVisible;
  EnableAutoSizing;
end;

function TDbgTreeView.DoCollapsing(Node: PVirtualNode): Boolean;
  procedure RecursivelyHideControls(N: PVirtualNode);
  var
    N2: PVirtualNode;
    NData: PDbgTreeNodeData;
  begin
    while N <> nil do begin
      NData := GetNodeData(N);
      if NData^.Control <> nil then
        NData^.Control.Visible := False;

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

procedure TDbgTreeView.DoExpanded(Node: PVirtualNode);
begin
  inc(FCheckControlsVisibleLock);
  inherited DoExpanded(Node);
  dec(FCheckControlsVisibleLock);
  CheckControlsVisible;
end;

procedure TDbgTreeView.DoStateChange(Enter: TVirtualTreeStates;
  Leave: TVirtualTreeStates);
begin
  if tsToggling in Enter then begin
    if not FInToggle then
      inc(FCheckControlsVisibleLock);
    FInToggle := True;
  end;
  inherited DoStateChange(Enter, Leave);
  if tsToggling in Leave then begin
    if FInToggle then
      dec(FCheckControlsVisibleLock);
    FInToggle := False;
    CheckControlsVisible;
    if not AutoSizeDelayed then // e.g. called by mouseclick / update before syncronizing with dbg-results
      Update;
  end;
end;

function TDbgTreeView.DetermineLineImageAndSelectLevel(Node: PVirtualNode;
  var LineImage: TLineImage): Integer;
var
  NData: PDbgTreeNodeData;
  i: Integer;
begin
  Result := inherited DetermineLineImageAndSelectLevel(Node, LineImage);
  if Length(LineImage) > 1 then
    LineImage[0] := ltNone
  else
  if (Length(LineImage) > 0) and (LineImage[0] <> ltNone) then
    LineImage[0] := ltRight;

  NData := GetNodeData(Node);
  if (NData^.Control <> nil) and (NData^.Item <> nil) then
    for i := 1 to Length(LineImage) - 1 do
      if LineImage[i] = ltNone then
        LineImage[i] := ltTopDown;

end;

procedure TDbgTreeView.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  NData: PDbgTreeNodeData;
  b: LongInt;
begin
  NData := GetNodeData(PaintInfo.Node);
  b := PaintInfo.ContentRect.Bottom;
  if NData^.Item = nil then
    exit;
  if (NData^.Control <> nil) and (not NData^.ControlHidden) and (not NData^.ControlOutside) then
    PaintInfo.ContentRect.Bottom := NData^.ControlTop;

  inherited DoPaintNode(PaintInfo);

  if (PaintInfo.Column=0) and (FLazImages <> nil) and (NData^.RightSideButton.ImageIndex > 0) then begin
    NData^.RightSideButton.Rect := PaintInfo.ContentRect;
    NData^.RightSideButton.Rect.Left := Max(
      PaintInfo.ContentRect.Right - FLazImages.Width,
      PaintInfo.ContentRect.Left + FLazImages.Width
    );
    //if PaintInfo.;
    //DrawEffect := gdeNormal
    //DrawEffect := gdeShadowed;
    FLazImages.Draw(PaintInfo.Canvas,
       NData^.RightSideButton.Rect.Left,
       (NData^.RightSideButton.Rect.Bottom - NData^.RightSideButton.Rect.Top - FLazImages.Height) div 2,
       NData^.RightSideButton.ImageIndex - 1,
       gdeNormal);
  end;

  PaintInfo.ContentRect.Bottom := b;
end;

procedure TDbgTreeView.PaintNodeButton(ACanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const R: TRect; ButtonX, ButtonY: Integer; ABidiMode: TBiDiMode);
var
  NData: PDbgTreeNodeData;
  offs: Integer;
begin
  NData := GetNodeData(Node);
  offs := NData^.ControlTop;

  if offs > 0 then
    ButtonY := ButtonY - (r.Bottom - offs) div 2;
  inherited PaintNodeButton(ACanvas, Node, Column, R, ButtonX, ButtonY, ABidiMode);
end;

procedure TDbgTreeView.PaintTreeLines(const PaintInfo: TVTPaintInfo; VAlignment,
  IndentSize: Integer; LineImage: TLineImage);
var
  NData: PDbgTreeNodeData;
  offs: Integer;
  PaintI2: TVTPaintInfo;
  VAlignment2: LongInt;
begin

  NData := GetNodeData(PaintInfo.Node);
  offs := NData^.ControlTop;
  PaintI2 := PaintInfo;
  if offs > 0 then begin
    PaintI2.CellRect.Top := min(PaintInfo.CellRect.Top + offs, PaintInfo.CellRect.Bottom);
    VAlignment2 := (PaintI2.CellRect.Bottom - PaintI2.CellRect.Top) div 2 - 1;
    inherited PaintTreeLines(PaintI2, VAlignment2, IndentSize, LineImage);
  end;

  inherited PaintTreeLines(PaintInfo, VAlignment, IndentSize -1, LineImage);
end;

procedure TDbgTreeView.PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: Integer);
var
  NData: PDbgTreeNodeData;
  offs: Integer;
  b, b2: LongInt;
begin
  NData := GetNodeData(PaintInfo.Node);
  offs := NData^.ControlTop;
  if offs = 0 then begin
    inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);
    exit;
  end;

  b := PaintInfo.CellRect.Bottom;
  b2 := PaintInfo.ContentRect.Bottom;
  PaintInfo.CellRect.Bottom := PaintInfo.CellRect.Top + offs;
  PaintInfo.ContentRect.Bottom := PaintInfo.ContentRect.Top + offs;

  inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);
  PaintInfo.CellRect.Bottom := b;
  PaintInfo.ContentRect.Bottom := b2;
end;

procedure TDbgTreeView.DoBeforeItemErase(ACanvas: TCanvas; ANode: PVirtualNode;
  const ItemRect: TRect; var AColor: TColor; var EraseAction: TItemEraseAction);
var
  NData: PDbgTreeNodeData;
  offs: Integer;
  r, r2: TRect;
  Temp: PVirtualNode;
begin
  inherited DoBeforeItemErase(ACanvas, ANode, ItemRect, AColor, EraseAction);
  EraseAction := eaColor;

  NData := GetNodeData(ANode);
  if NData^.Control <> nil then begin
    if not NData^.ControlHidden then begin
      r := ItemRect;

      if NData^.CachedFirstCellLeft <= 0 then begin
        r2 := GetDisplayRect(ANode, 0, True, False);
        NData^.CachedFirstCellLeft := r2.Left;
        r.Top := r2.Top;
      end
      else begin
        r.Top := OffsetY;
        Temp := ANode;
        repeat
          Temp := GetPreviousVisibleNoInit(Temp, True);
          if Temp = nil then
            Break;
          Inc(r.Top, NodeHeight[Temp]);
        until False;
        if hoVisible in Header.Options then
          inc(r.Top, Header.Height);
      end;

      offs := NData^.ControlTop;
      r.Bottom := r.Top + ItemRect.Bottom - ItemRect.Top;
      r.Top := r.Top + offs;
      if r.Top >= Header.Height then begin
        if NData^.ControlOutside then begin
          NData^.Control.SetBounds(OffsetX, r.Top, NData^.ControlWidth, NData^.ControlHeight);
        end
        else begin
          r.Left := NData^.CachedFirstCellLeft + OffsetX;
          if NData^.ControlWidth <= 0 then
            r.Right := RangeX
          else
            r.Right := r.Left+NData^.ControlWidth;
          NData^.Control.BoundsRect := r;
        end;
        NData^.Control.Visible := True;
      end;
    end
    else
      NData^.Control.Visible := False;
  end;
end;

function TDbgTreeView.DetermineDropMode(const P: TPoint; var HitInfo: THitInfo;
  var NodeRect: TRect): TDropMode;
begin
  if not Assigned(HitInfo.HitNode) and (HitInfo.HitPositions * [hiAbove,hiBelow] = []) then begin
    HitInfo.HitNode := GetLastVisibleNoInit;
    NodeRect := GetDisplayRect(HitInfo.HitNode, HitInfo.HitColumn, False);
    Result := dmBelow;
  end
  else
  if Assigned(HitInfo.HitNode) then
  begin
    if ((NodeRect.Top + NodeRect.Bottom) div 2) > P.Y then
      Result := dmAbove
    else
      Result := dmBelow;
  end
  else
    Result := dmNowhere;

  if FOnDetermineDropMode <> nil then
    FOnDetermineDropMode(P, HitInfo, NodeRect, Result);
end;

procedure TDbgTreeView.HandleMouseDown(var Message: TLMMouse;
  var HitInfo: THitInfo);
var
  NData: PDbgTreeNodeData;
begin
  if Message.Msg = LM_LBUTTONDOWN then
    DragMode := dmAutomatic
  else
    DragMode := dmManual;

  if (HitInfo.HitColumn = 0) then begin
    NData := GetNodeData(HitInfo.HitNode);
    if (NData^.RightSideButton.ImageIndex > 0) and
       (NData^.RightSideButton.Rect.Top >= 0) and
       (HitInfo.HitPoint.X >= NData^.RightSideButton.Rect.Left) and
       (HitInfo.HitPoint.X < NData^.RightSideButton.Rect.Right)
    then begin
      if FOnNodeRightButtonClick <> nil then
        FOnNodeRightButtonClick(Self, HitInfo.HitNode);
      exit;
    end;
  end;

  inherited HandleMouseDown(Message, HitInfo);
end;

procedure TDbgTreeView.HandleMouseDblClick(var Message: TLMMouse;
  const HitInfo: THitInfo);
begin
  if hiOnItemButton in HitInfo.HitPositions then
    exit;

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

function TDbgTreeView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer
  ): TCustomImageList;
begin
  Result := nil;
  Ghosted := False;
  Index := GetNodeImageIndex(Node, Column);
end;

function TDbgTreeView.DoGetNodeWidth(ANode: PVirtualNode; AColumn: TColumnIndex; ACanvas: TCanvas
  ): Integer;
var
  NData: PDbgTreeNodeData;
begin
  NData := GetNodeData(ANode);
  if (AColumn > 0) and (AColumn < Length(NData^.CachedText)) then begin
    Result := NData^.CachedColumnData[AColumn].ColWidth;
    if Result > 0 then
      exit;

    Result := inherited DoGetNodeWidth(ANode, AColumn, ACanvas);
    NData^.CachedColumnData[AColumn].ColWidth := Result;
    exit;
  end;


  Result := inherited DoGetNodeWidth(ANode, AColumn, ACanvas);
end;

function TDbgTreeView.DoShortenString(ACanvas: TCanvas; ANode: PVirtualNode;
  AColumn: TColumnIndex; const S: String; AWidth: Integer; AnEllipsisWidth: Integer): String;
var
  NData: PDbgTreeNodeData;
begin
  NData := GetNodeData(ANode);
  if (AColumn < Length(NData^.CachedText)) and (s = NData^.CachedText[AColumn]) then begin
    if (AWidth = NData^.CachedColumnData[AColumn].ShortenWidth1) and (AnEllipsisWidth = NData^.CachedColumnData[AColumn].ShortenWidth2) then begin
      Result := NData^.CachedColumnData[AColumn].ShortenResText;
      exit;
    end;

    Result := inherited DoShortenString(ACanvas, ANode, AColumn, S, AWidth, AnEllipsisWidth);
    NData^.CachedColumnData[AColumn].ShortenWidth1 := AWidth;
    NData^.CachedColumnData[AColumn].ShortenWidth2 := AnEllipsisWidth;
    NData^.CachedColumnData[AColumn].ShortenResText := Result;
    exit;
  end;

  Result := inherited DoShortenString(ACanvas, ANode, AColumn, S, AWidth, AnEllipsisWidth);
end;

procedure TDbgTreeView.DoNodeMoved(ANode: PVirtualNode);
var
  NData: PDbgTreeNodeData;
  i: Integer;
  N: PVirtualNode;
begin
  inherited DoNodeMoved(ANode);

  NData := GetNodeData(ANode);
  for i := 0 to Length(NData^.CachedColumnData) - 1 do begin
    NData^.CachedColumnData[i].ShortenResText := '';
    NData^.CachedColumnData[i].ShortenWidth1 := -1;
    NData^.CachedColumnData[i].ColWidth := -1;
    NData^.CachedFirstCellLeft := -1;
  end;

  N := GetFirstChildNoInit(ANode);
  if N <> nil then DoNodeMoved(N);
  N := GetNextSiblingNoInit(ANode);
  if N <> nil then DoNodeMoved(N);
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

function TDbgTreeView.GetNodeData(Node: PVirtualNode): PDbgTreeNodeData;
begin
  Result := PDbgTreeNodeData(inherited GetNodeData(Node));
end;

function TDbgTreeView.GetFocusedNode(OnlySelected: Boolean;
  AnIncludeControlNodes: Boolean): PVirtualNode;
begin
  Result := FocusedNode;
  if (not AnIncludeControlNodes) and (Result <> nil) and
     (NodeControl[Result] <> nil) and (NodeItem[Result] = nil)
  then
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

