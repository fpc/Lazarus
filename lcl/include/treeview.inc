// included by comctrls.pp

{******************************************************************************
                                  TTreeView
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Abstract:
    TTreeView for LCL

  ToDo:
    - Drag&Drop
    - Editing
    - Columns
}


{ $DEFINE TREEVIEW_DEBUG}

const
  TTreeNodeStreamVersion : word = 1;
  
  // maximum scroll range
  MAX_SCROLL = 32767;



procedure TreeViewError(const Msg: string);
begin
  raise ETreeViewError.Create(Msg);
end;

{procedure TreeViewErrorFmt(const Msg: string; Format: array of const);
begin
  raise ETreeViewError.CreateFmt(Msg, Format);
end;}

procedure TreeNodeError(const Msg: string);
begin
  raise ETreeNodeError.Create(Msg);
end;

{procedure TreeNodeErrorFmt(const Msg: string; Format: array of const);
begin
  raise ETreeNodeError.CreateFmt(Msg, Format);
end;}

function IndexOfNodeAtTop(NodeArray: TTreeNodeArray; Count, y: integer): integer;
// NodeArray must be sorted via Top
// returns index of Node with Node.Top <= y < Node[+1].Top
var l, m, r: integer;
begin
  if (Count=0) or (NodeArray=nil) then exit(-1);
  l:=0;
  r:=Count-1;
  while (l<=r) do begin
    m:=(l+r) shr 1;
//writeln(':0 [IndexOfNodeAtTop] m=',m,' y=',y,' ',NodeArray[m].Text,' NodeArray[m].Top=',NodeArray[m].Top,' NodeArray[m].BottomExpanded=',NodeArray[m].BottomExpanded);
    if NodeArray[m].Top>y then
      r:=m-1
    else if NodeArray[m].BottomExpanded<=y then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;


{ TTreeNode }

function TTreeNode.DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
begin
  if (Node1.TreeView<>nil) and  Assigned(Node1.TreeView.OnCompare) then
    Node1.TreeView.OnCompare(Node1.TreeView,Node1, Node2, Result)
  else
    Result := AnsiCompareStr(Node1.Text,Node2.Text);
end;

constructor TTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create;
  FOverlayIndex := -1;
  FStateIndex := -1;
  FStates := [];
  FOwner := AnOwner;
  FSubTreeCount:=1;
  if Owner<>nil then inc(Owner.FCount);
end;

destructor TTreeNode.Destroy;
//var
//  Node: TTreeNode;
//  CheckValue: Integer;
begin
{$IFDEF TREEVIEW_DEBUG}
writeln('[TTreeNode.Destroy] Self=',HexStr(Cardinal(Self),8),' Self.Text=',Text);
{$ENDIF}
  FDeleting := True;
  HasChildren := false;
  Unbind;
  if Owner<>nil then dec(Owner.FCount);
  {if Owner.Owner.FLastDropTarget = Self then
    Owner.Owner.FLastDropTarget := nil;
  Node := Parent;
  if (Node <> nil) and (not Node.Deleting) then begin
    if Node.IndexOf(Self) <> -1 then
      CheckValue := 1
    else
      CheckValue := 0;
    if Node.CompareCount(CheckValue) then begin
      Expanded := False;
      Node.HasChildren := False; // delete all childs
    end;
  end;
  if ItemId <> nil then TreeView_DeleteItem(Handle, ItemId);}
  Data := nil;
  if FItems<>nil then begin
    FreeMem(FItems);
    FItems:=nil;
  end;
  inherited Destroy;
end;

function TTreeNode.GetHandle: THandle;
begin
  if TreeView<>nil then
    Result := TreeView.Handle
  else
    Result := 0;
end;

function TTreeNode.GetTreeNodes: TTreeNodes;
begin
  if (Owner<>nil) and (Owner is TTreeNodes) then
    Result:=TTreeNodes(Owner)
  else
    Result:=nil;
end;

function TTreeNode.GetTreeView: TCustomTreeView;
begin
  if Owner<>nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

function TTreeNode.GetTop: integer;
begin
  if TreeView<>nil then
    TreeView.UpdateAllTops;
  Result:=FTop;
end;

function TTreeNode.HasAsParent(AValue: TTreeNode): Boolean;
begin
  if AValue<>nil then begin
    if Parent=nil then Result := False
    else if Parent=AValue then Result := True
    else Result := Parent.HasAsParent(AValue);
  end
  else Result := True;
end;

procedure TTreeNode.SetText(const S: string);
//var Item: TTVItem;
begin
  if S=FText then exit;
  FText := S;
  if TreeView=nil then exit;
  // ToDo:
  {
  with Item do begin
    mask := TVIF_TEXT;
    hItem := ItemId;
    pszText := LPSTR_TEXTCALLBACK;
  end;
  TreeView_SetItem(Handle, Item);
  }
  Include(TreeView.FStates,tvsMaxRightNeedsUpdate);
  if (TreeView.SortType in [stText, stBoth]) and FInTree then begin
    if (Parent <> nil) then Parent.AlphaSort
    else TreeView.AlphaSort;
  end;
  Update;
end;

procedure TTreeNode.SetData(AValue: Pointer);
begin
  if FData=AValue then exit;
  FData := AValue;
  if (TreeView<>nil)
  and (TreeView.SortType in [stData, stBoth]) and Assigned(TreeView.OnCompare)
  and (not Deleting) and FInTree then
  begin
    if Parent <> nil then Parent.AlphaSort
    else TreeView.AlphaSort;
  end;
end;

function TTreeNode.GetState(NodeState: TNodeState): Boolean;
//var Item: TTVItem;
begin
  // ToDo:
  Result:=NodeState in FStates;
  {
  Result := False;
  with Item do begin
    mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      case NodeState of
        nsCut: Result := (state and TVIS_CUT) <> 0;
        nsFocused: Result := (state and TVIS_FOCUSED) <> 0;
        nsSelected: Result := (state and TVIS_SELECTED) <> 0;
        nsExpanded: Result := (state and TVIS_EXPANDED) <> 0;
        nsDropHilited: Result := (state and TVIS_DROPHILITED) <> 0;
      end;
  end;
  }
end;

procedure TTreeNode.SetHeight(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if AValue=FHeight then exit;
  FHeight:=AValue;
  if TreeView<>nil then
    TreeView.FStates:=TreeView.FStates+[tvsScrollbarChanged,tvsTopsNeedsUpdate];
  Update;
end;

procedure TTreeNode.SetImageIndex(AValue: integer);
//var Item: TTVItem;
begin
  if FImageIndex=AValue then exit;
  FImageIndex := AValue;
  Update;
  // ToDo
  {
  with Item do
  begin
    mask := TVIF_IMAGE or TVIF_HANDLE;
    hItem := ItemId;
    if Assigned(TCustomTreeView(Owner.Owner).OnGetImageIndex) then
      iImage := I_IMAGECALLBACK
    else
      iImage := FImageIndex;
  end;
  TreeView_SetItem(Handle, Item);
  }
end;

procedure TTreeNode.SetSelectedIndex(AValue: Integer);
//var Item: TTVItem;
begin
  if FSelectedIndex = AValue then exit;
  FSelectedIndex := AValue;
  Update;
  { ToDo:
  with Item do begin
    mask := TVIF_SELECTEDIMAGE or TVIF_HANDLE;
    hItem := ItemId;
    if Assigned(TCustomTreeView(Owner.Owner).OnGetSelectedIndex) then
      iSelectedImage := I_IMAGECALLBACK
    else
      iSelectedImage := FSelectedIndex;
  end;
  TreeView_SetItem(Handle, Item);
  }
end;

procedure TTreeNode.SetOverlayIndex(AValue: Integer);
//var Item: TTVItem;
begin
  if FOverlayIndex = AValue then exit;
  FOverlayIndex := AValue;
  Update;
  { ToDo:
  with Item do begin
    mask := TVIF_STATE or TVIF_HANDLE;
    stateMask := TVIS_OVERLAYMASK;
    hItem := ItemId;
    state := IndexToOverlayMask(FOverlayIndex + 1);
  end;
  TreeView_SetItem(Handle, Item);
  }
end;

procedure TTreeNode.SetStateIndex(AValue: Integer);
//var Item: TTVItem;
begin
  if FStateIndex = AValue then exit;
  FStateIndex := AValue;
  Update;
  { ToDo:
  if Value >= 0 then Dec(Value);
  with Item do
  begin
    mask := TVIF_STATE or TVIF_HANDLE;
    stateMask := TVIS_STATEIMAGEMASK;
    hItem := ItemId;
    state := IndexToStateImageMask(Value + 1);
  end;
  TreeView_SetItem(Handle, Item);
  }
end;

function TTreeNode.AreParentsExpanded: Boolean;
var ANode: TTreeNode;
begin
  Result:=false;
  ANode:=Parent;
  while ANode<>nil do begin
    if not ANode.Expanded then exit;
    ANode:=ANode.Parent;
  end;
  Result:=true;
end;

procedure TTreeNode.BindToMultiSelected;
var
  TheTreeNodes: TTreeNodes;
begin
  TheTreeNodes:=TreeNodes;
  if TheTreeNodes=nil then exit;
  FNextMultiSelected:=TheTreeNodes.FFirstMultiSelected;
  FPrevMultiSelected:=nil;
  if FNextMultiSelected<>nil then
    FNextMultiSelected.FPrevMultiSelected:=Self;
  TheTreeNodes.FFirstMultiSelected:=Self;
end;

function TTreeNode.CompareCount(CompareMe: Integer): Boolean;
{var
  ACount: integer;
  Node: TTreeNode;}
Begin
  Result:=(CompareMe=Count);
  {
  ACount := 0;
  Result := False;
  Node := GetFirstChild;
  while Node <> nil do begin
    Inc(ACount);
    Node := Node.GetNextChild(Node);
    if ACount > CompareMe then Exit;
  end;
  if ACount = CompareMe then Result := True;}
end;

function TTreeNode.DoCanExpand(ExpandIt: Boolean): Boolean;
begin
  Result := False;
  if (TreeView<>nil) and HasChildren then begin
    if ExpandIt then Result := TreeView.CanExpand(Self)
    else Result := TreeView.CanCollapse(Self);
  end;
end;

procedure TTreeNode.DoExpand(ExpandIt: Boolean);
begin
//writeln('[TTreeNode.DoExpand] Self=',HexStr(Cardinal(Self),8),' Text=',Text,
//' HasChildren=',HasChildren,' ExpandIt=',ExpandIt,' Expanded=',Expanded);
  if HasChildren and (Expanded<>ExpandIt) then begin
    if (TreeView<>nil) then begin
      if ExpandIt then
        TreeView.Expand(Self)
      else
        TreeView.Collapse(Self);
    end;
    if ExpandIt then
      Include(FStates,nsExpanded)
    else begin
      Exclude(FStates,nsExpanded);
      if (not Owner.KeepCollapsedNodes) then begin
        while GetLastChild<>nil do
          GetLastChild.Free;
      end;
    end;
    if TreeView<>nil then begin
      TreeView.FStates:=(TreeView.FStates+[tvsTopsNeedsUpdate,
                tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate,
                tvsScrollbarChanged,tvsMaxRightNeedsUpdate]);
      TreeView.Invalidate;
    end;
  end;
end;

procedure TTreeNode.ExpandItem(ExpandIt: Boolean; Recurse: Boolean);
var
  //Flag: Integer;
  ANode: TTreeNode;
begin
  if Recurse then begin
    ExpandItem(ExpandIt, False);
    ANode := GetFirstChild;
    while ANode<>nil do begin
      ANode.ExpandItem(ExpandIt, true);
      ANode := ANode.FNextBrother;
    end;
  end
  else begin
    if TreeView<>nil then
      Include(TreeView.FStates,tvsManualNotify);
    try
      if DoCanExpand(ExpandIt) then
        DoExpand(ExpandIt);
      //if Flag <> 0 then TreeView_Expand(Handle, ItemId, Flag);
    finally
      if TreeView<>nil then
        Exclude(TreeView.FStates,tvsManualNotify);
    end;
  end;
end;

procedure TTreeNode.Expand(Recurse: Boolean);
begin
  ExpandItem(True, Recurse);
end;

procedure TTreeNode.ExpandParents;
var ANode: TTreeNode;
begin
  ANode:=Parent;
  while ANode<>nil do begin
    ANode.Expanded:=true;
    ANode:=ANode.Parent;
  end;
end;

procedure TTreeNode.Collapse(Recurse: Boolean);
begin
  ExpandItem(False, Recurse);
end;

function TTreeNode.GetExpanded: Boolean;
begin
  Result := GetState(nsExpanded);
end;

procedure TTreeNode.SetExpanded(AValue: Boolean);
begin
  if AValue=Expanded then exit;
  if AValue then
    Expand(False)
  else
    Collapse(False);
end;

function TTreeNode.GetSelected: Boolean;
begin
  Result := GetState(nsSelected);
end;

procedure TTreeNode.SetSelected(AValue: Boolean);
begin
  if AValue=GetSelected then exit;
  if AValue then
    Include(FStates,nsSelected)
  else begin
    Exclude(FStates,nsSelected);
    if (TreeView<>nil) and (TreeView.Selected=Self) then
      TreeView.Selected:=nil;
  end;
  Update;
end;

function TTreeNode.GetCut: Boolean;
begin
  Result := GetState(nsCut);
end;

procedure TTreeNode.SetCut(AValue: Boolean);
{var
  Item: TTVItem;
  Template: DWORD;}
begin
  if AValue=Cut then exit;
  // ToDo
  if AValue then
    Include(FStates,nsCut)
  else
    Exclude(FStates,nsCut);
{  if Value then
    Template := DWORD(-1)
  else
    Template := 0;
  with Item do begin
    mask := TVIF_STATE;
    hItem := ItemId;
    stateMask := TVIS_CUT;
    state := stateMask and Template;
  end;
  TreeView_SetItem(Handle, Item);}
end;

function TTreeNode.GetDropTarget: Boolean;
begin
  Result := GetState(nsDropHilited);
end;

procedure TTreeNode.SetDropTarget(AValue: Boolean);
begin
  if AValue=GetDropTarget then exit;
  if AValue then begin
    Include(FStates,nsDropHilited);
    if TreeView<>nil then
      TreeView.FLastDropTarget:=Self;
  end else begin
    Exclude(FStates,nsDropHilited);
    if TreeView<>nil then
      TreeView.FLastDropTarget:=nil;
  end;
  {if Value then TreeView_SelectDropTarget(Handle, ItemId)
  else if DropTarget then TreeView_SelectDropTarget(Handle, nil);}
end;

function TTreeNode.GetHasChildren: Boolean;
begin
  Result := GetState(nsHasChildren);
end;

procedure TTreeNode.SetFocused(AValue: Boolean);
{var
  Item: TTVItem;
  Template: DWORD;}
begin
  if AValue=GetFocused then exit;
  // ToDo
  if AValue then
    Include(FStates,nsFocused)
  else
    Exclude(FStates,nsFocused);
  {if Value then Template := DWORD(-1)
  else Template := 0;
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    stateMask := TVIS_FOCUSED;
    state := stateMask and Template;
  end;
  TreeView_SetItem(Handle, Item);}
  Update;
end;

function TTreeNode.Bottom: integer;
begin
  Result:=Top+Height;
end;

function TTreeNode.BottomExpanded: integer;
begin
  if GetNextSibling<>nil then
    Result:=GetNextSibling.Top
  else if GetLastChild<>nil then
    Result:=GetLastChild.BottomExpanded
  else
    Result:=Bottom;
end;

function TTreeNode.GetFocused: Boolean;
begin
  Result := GetState(nsFocused);
end;

procedure TTreeNode.SetHasChildren(AValue: Boolean);
//var Item: TTVItem;
begin
  if AValue=HasChildren then exit;
//writeln('[TTreeNode.SetHasChildren] Self=',HexStr(Cardinal(Self),8),
//' Self.Text=',Text,' AValue=',AValue);
  if AValue then
    Include(FStates,nsHasChildren)
  else begin
    while GetLastChild<>nil do
      GetLastChild.Free;
    Exclude(FStates,nsHasChildren)
  end;
  { Delphi:
  with Item do
  begin
    mask := TVIF_CHILDREN;
    hItem := ItemId;
    cChildren := Ord(Value);
  end;
  TreeView_SetItem(Handle, Item);
  }
  Update;
end;

function TTreeNode.GetNextSibling: TTreeNode;
begin
  Result:=FNextBrother;
end;

function TTreeNode.GetPrevSibling: TTreeNode;
begin
  Result:=FPrevBrother;
end;

function TTreeNode.GetNextVisible: TTreeNode;
begin
  if Expanded and (GetFirstChild<>nil) then
    Result:=GetFirstChild
  else begin
    Result:=Self;
    while (Result<>nil) and (Result.GetNextSibling=nil) do
      Result:=Result.Parent;
    if Result<>nil then Result:=Result.GetNextSibling;
  end;
  if (Result<>nil) and (not Result.IsVisible) then
    Result:=nil;
end;

function TTreeNode.GetPrevVisible: TTreeNode;
begin
  Result:=GetPrev;
  if (Result<>nil) and (TreeView<>nil) and (not TreeView.IsNodeVisible(Result))
  then
    Result:=nil;
end;

function TTreeNode.GetNextChild(AValue: TTreeNode): TTreeNode;
begin
  if AValue <> nil then
    Result := AValue.GetNextSibling
  else
    Result := nil;
end;

function TTreeNode.GetNextMultiSelected: TTreeNode;
begin
  Result:=FNextMultiSelected;
end;

function TTreeNode.GetPrevChild(AValue: TTreeNode): TTreeNode;
begin
  if AValue <> nil then
    Result := AValue.GetPrevSibling
  else
    Result := nil;
end;

function TTreeNode.GetPrevMultiSelected: TTreeNode;
begin
  Result:=FPrevMultiSelected;
end;

function TTreeNode.GetFirstChild: TTreeNode;
begin
  if Count>0 then
    Result:=FItems[0]
  else
    Result:=nil;
end;

function TTreeNode.GetLastSibling: TTreeNode;
begin
  if Parent<>nil then
    Result:=Parent.GetLastChild
  else begin
    Result:=Self;
    while Result.FNextBrother<>nil do
      Result:=Result.FNextBrother;
  end;
end;

function TTreeNode.GetLastChild: TTreeNode;
begin
  if Count>0 then
    Result:=FItems[Count-1]
  else
    Result:=nil;
end;

function TTreeNode.GetLastSubChild: TTreeNode;
var Node: TTreeNode;
begin
  Result:=GetLastChild;
  if Result<>nil then begin
    Node:=Result.GetLastSubChild;
    if Node<>nil then
      Result:=Node;
  end;
end;

function TTreeNode.GetNext: TTreeNode;
{var
  NodeID, ParentID: HTreeItem;
  Handle: HWND;}
begin
  Result:=GetFirstChild;
  if Result=nil then begin
    // no childs, search next
    Result:=Self;
    while (Result<>nil) and (Result.FNextBrother=nil) do
      Result:=Result.Parent;
    if Result<>nil then Result:=Result.FNextBrother;
  end;
  {Handle := FOwner.Handle;
  NodeID := TreeView_GetChild(Handle, ItemId);
  if NodeID = nil then NodeID := TreeView_GetNextSibling(Handle, ItemId);
  ParentID := ItemId;
  while (NodeID = nil) and (ParentID <> nil) do
  begin
    ParentID := TreeView_GetParent(Handle, ParentID);
    NodeID := TreeView_GetNextSibling(Handle, ParentID);
  end;
  Result := FOwner.GetNode(NodeID);}
end;

function TTreeNode.GetPrev: TTreeNode;
var
  ANode: TTreeNode;
begin
  Result := GetPrevSibling;
  if Result <> nil then begin
    ANode := Result;
    repeat
      Result := ANode;
      ANode := Result.GetLastChild;
    until ANode = nil;
  end else
    Result := Parent;
end;

function TTreeNode.GetAbsoluteIndex: Integer;
// - first node has index 0
// - the first child of a node has an index one bigger than its parent
// - a node without childs has an index one bigger than its previous brother
var
  ANode: TTreeNode;
begin
  Result:=-1;
  ANode:=Self;
  repeat
    inc(Result);
    while ANode.FPrevBrother<>nil do begin
      ANode:=ANode.FPrevBrother;
      inc(Result,ANode.FSubTreeCount);
    end;
    ANode:=ANode.Parent;
  until ANode=nil;
end;

function TTreeNode.GetHeight: integer;
begin
  if FHeight<=0 then begin
    if TreeView<>nil then
      Result:=TreeView.FDefItemHeight
    else
      Result:=20;
  end else
    Result:=FHeight;
end;

function TTreeNode.GetIndex: Integer;
// returns number of previous siblings (nodes on same lvl with same parent)
var
  ANode: TTreeNode;
begin
  // many algorithms uses the last sibling, so we check that first for speed
  if (Parent<>nil) and (Parent[Parent.Count-1]=Self) then begin
    Result:=Parent.Count-1;
    exit;
  end;
  // count previous siblings
  Result := -1;
  ANode := Self;
  while ANode <> nil do begin
    Inc(Result);
    ANode := ANode.GetPrevSibling;
  end;
end;

function TTreeNode.GetItems(AnIndex: Integer): TTreeNode;
begin
  if (AnIndex<0) or (AnIndex>=Count) then
    TreeNodeError('TTreeNode.GetItems: Index '+IntToStr(AnIndex)
                 +' out of bounds '+IntToStr(Count));
  Result:=FItems[AnIndex];
  {Result := GetFirstChild;
  while (Result <> nil) and (Index > 0) do
  begin
    Result := GetNextChild(Result);
    Dec(Index);
  end;
  if Result = nil then TreeViewError(SListIndexError);}
end;

procedure TTreeNode.SetItems(AnIndex: Integer; AValue: TTreeNode);
begin
  if (AnIndex<0) or (AnIndex>=Count) then
    TreeNodeError('TTreeNode.SetItems: Index '+IntToStr(AnIndex)
                 +' out of bounds '+IntToStr(Count));
  Items[AnIndex].Assign(AValue);
end;

procedure TTreeNode.SetMultiSelected(const AValue: Boolean);
begin
  if AValue=GetMultiSelected then exit;
  if AValue then begin
    if (Treeview<>nil) and (not (tvoAllowMultiselect in TreeView.Options)) then
      exit;
    Include(FStates,nsMultiSelected);
    if TreeNodes<>nil then BindToMultiSelected;
  end else begin
    Exclude(FStates,nsMultiSelected);
    if TreeNodes<>nil then UnbindFromMultiSelected;
  end;
  Update;
end;

function TTreeNode.IndexOf(AValue: TTreeNode): Integer;
begin
  if AValue=nil then begin
    Result:=-1;
    exit;
  end;
  Result:=Count-1;
  while Result>=0 do begin
    if FItems[Result]=AValue then exit;
    dec(Result);
  end;
end;

function TTreeNode.GetCount: Integer;
//var Node: TTreeNode;
begin
  Result:=FCount;
  {
  Result := 0;
  Node := GetFirstChild;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetNextChild(Node);
  end;}
end;

procedure TTreeNode.EndEdit(Cancel: Boolean);
begin
  // ToDo:
  //TreeView_EndEditLabelNow(Handle, Cancel);
  if Cancel then begin
  end;
end;

procedure TTreeNode.Unbind;
// unbind from parent and neighbor siblings
var OldIndex, i: integer;
  HigherNode: TTreeNode;
begin
  {$IFDEF TREEVIEW_DEBUG}
  writeln('[TTreeNode.Unbind] Self=',HexStr(Cardinal(Self),8),' Self.Text=',Text);
  {$ENDIF}
  Selected:=false;
  if Owner<>nil then begin
    Owner.ClearCache;
    if FParent=nil then
      Owner.MoveTopLvlNode(Owner.IndexOfTopLvlItem(Self),-1,Self);
    if Owner.Owner<>nil then begin
      Owner.Owner.FStates:=Owner.Owner.FStates+[tvsMaxRightNeedsUpdate,
        tvsTopsNeedsUpdate,tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate];
      if Owner.Owner.FLastDropTarget=Self then
        Owner.Owner.FLastDropTarget:=nil;
    end;
  end;
  UnbindFromMultiSelected;
  if FPrevBrother<>nil then FPrevBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPrevBrother:=FPrevBrother;
  FPrevBrother:=nil;
  FNextBrother:=nil;
  if FParent<>nil then begin
    HigherNode:=FParent;
    while HigherNode<>nil do begin
      dec(HigherNode.FSubTreeCount,FSubTreeCount);
      HigherNode:=HigherNode.Parent;
    end;
    //if TreeNodes<>nil then Dec(TreeNodes.FCount,FSubTreeCount);
    OldIndex:=Index;
    for i:=OldIndex to Count-1 do
      FParent.FItems[i]:=FParent.FItems[i+1];
    dec(FParent.FCount);
    if (FParent.FCapacity>15) and (FParent.FCount<(FParent.FCapacity shr 2))
    then begin
      // shrink FParent.FItems
      FParent.FCapacity:=FParent.FCapacity shr 1;
      ReAllocMem(FParent.FItems,SizeOf(Pointer)*FParent.FCapacity);
    end;
    if FParent.Count=0 then begin
      FParent.Expanded:=false;
      FParent.HasChildren:=false;
    end;
    FParent:=nil;
  end;
end;

procedure TTreeNode.UnbindFromMultiSelected;
var
  TheTreeNodes: TTreeNodes;
begin
  TheTreeNodes:=TreeNodes;
  if TheTreeNodes=nil then exit;
  if TheTreeNodes.FFirstMultiSelected=Self then
    TheTreeNodes.FFirstMultiSelected:=FNextMultiSelected;
  if FNextMultiSelected<>nil then
    FNextMultiSelected.FPrevMultiSelected:=FPrevMultiSelected;
  if FPrevMultiSelected<>nil then
    FPrevMultiSelected.FNextMultiSelected:=FNextMultiSelected;
  FNextMultiSelected:=nil;
  FPrevMultiSelected:=nil;
end;

procedure TTreeNode.InternalMove(ANode: TTreeNode;
  AddMode: TAddMode);
{
  TAddMode = (taAddFirst, taAdd, taInsert);

  taAdd:      add Self as last child of ANode
  taAddFirst: add Self as first child of ANode
  taInsert:   add Self in front of ANode
}
var HigherNode: TTreeNode;
  NewIndex, NewParentItemSize, i: integer;
begin
  {$IFDEF TREEVIEW_DEBUG}
  write('[TTreeNode.InternalMove]  Self=',HexStr(Cardinal(Self),8),' Self.Text=',Text
  ,' ANode=',ANode<>nil,' AddMode=',AddModeNames[AddMode]);
  if ANode<>nil then write(' ANode.Text=',ANode.Text);
  writeln('');
  {$ENDIF}
  Unbind;
  // set parent
  if AddMode in [taAdd, taAddFirst] then
    FParent:=ANode
  else begin // taInsert
    if (ANode=nil) then
      TreeNodeError('TTreeNode.InternalMove AddMode=taInsert but ANode=nil');
    FParent:=ANode.Parent;
    FPrevBrother:=ANode.FPrevBrother;
    FNextBrother:=ANode;
  end;
  if FParent<>nil then begin
    FParent.HasChildren:=true;
    if (FParent.FCount=FParent.FCapacity) then begin
      // grow FParent.FItems
      if FParent.FCapacity=0 then
        FParent.FCapacity:=5
      else
        FParent.FCapacity:=FParent.FCapacity shl 1;
      NewParentItemSize:=SizeOf(Pointer)*FParent.FCapacity;
      if FParent.FItems=nil then
        GetMem(FParent.FItems,NewParentItemSize)
      else
        ReAllocMem(FParent.FItems,NewParentItemSize);
    end;
    inc(FParent.FCount);
    // calculate new Index
    case AddMode of
    taAdd: NewIndex:=FParent.Count-1;
    taAddFirst: NewIndex:=0;
    else
      // taInsert
      NewIndex:=ANode.Index;
    end;
    // move next siblings
    for i:=FParent.FCount-1 downto NewIndex+1 do
      FParent.FItems[i]:=FParent.FItems[i-1];
    // insert this node to parent's items
    FParent.FItems[NewIndex]:=Self;
    // set Next and Prev sibling
    if NewIndex>0 then
      FPrevBrother:=FParent.FItems[NewIndex-1]
    else
      FPrevBrother:=nil;
    if NewIndex<FParent.Count-1 then
      FNextBrother:=FParent.FItems[NewIndex+1]
    else
      FNextBrother:=nil;
    // update total node count of all parents
    HigherNode:=FParent;
    while HigherNode<>nil do begin
      inc(HigherNode.FSubTreeCount,FSubTreeCount);
      HigherNode:=HigherNode.Parent;
    end;
    //if TreeNodes<>nil then inc(TreeNodes.FCount,FSubTreeCount);
  end else begin
    // add as top level node
    case AddMode of
    taAdd:
      begin
        // add as last top level node
        if Owner<>nil then begin
          FPrevBrother:=Owner.GetLastNode;
          Owner.MoveTopLvlNode(-1,Owner.FTopLvlCount,Self);
        end;
      end;
    taAddFirst:
      begin
        // add as first top level node = root node
        if Owner<>nil then begin
          FNextBrother:=Owner.GetFirstNode;
          Owner.MoveTopLvlNode(-1,0,Self);
        end;
      end;
    taInsert:
      begin
        // insert node in front of ANode
//writeln('[TTreeNode.InternalMove] ANode.Index=',ANode.Index,' ANode=',HexStr(Cardinal(ANode),8));
        FNextBrother:=ANode;
        FPrevBrother:=ANode.GetPrevSibling;
        if Owner<>nil then begin
          Owner.MoveTopLvlNode(-1,ANode.Index,Self);
        end;
      end;
    end;
  end;
  // connect Next and Prev sibling
  if FPrevBrother<>nil then FPrevBrother.FNextBrother:=Self;
  if FNextBrother<>nil then FNextBrother.FPrevBrother:=Self;
  if Owner.Owner<>nil then
    Owner.Owner.FStates:=Owner.Owner.FStates+[tvsMaxRightNeedsUpdate,
      tvsTopsNeedsUpdate,tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate];

{$IFDEF TREEVIEW_DEBUG}
write('[TTreeNode.InternalMove] END Self=',HexStr(Cardinal(Self),8),' Self.Text=',Text
,' ANode=',ANode<>nil,' AddMode=',AddModeNames[AddMode]);
if ANode<>nil then write(' ANode.Text=',ANode.Text);
writeln('');
{$ENDIF}

{var
  I: Integer;
  NodeId: HTreeItem;
  TreeViewItem: TTVItem;
  Children: Boolean;
  IsSelected: Boolean;
begin
  Owner.ClearCache;
  if (AddMode = taInsert) and (Node <> nil) then
    NodeId := Node.ItemId else
    NodeId := nil;
  Children := HasChildren;
  IsSelected := Selected;
  if (Parent <> nil) and (Parent.CompareCount(1)) then
  begin
    Parent.Expanded := False;
    Parent.HasChildren := False;
  end;
  with TreeViewItem do
  begin
    mask := TVIF_PARAM;
    hItem := ItemId;
    lParam := 0;
  end;
  TreeView_SetItem(Handle, TreeViewItem);
  with Owner do
    HItem := AddItem(HItem, NodeId, CreateItem(Self), AddMode);
  if HItem = nil then
    raise EOutOfResources.Create(sInsertError);
  for I := Count - 1 downto 0 do
    Item[I].InternalMove(Self, nil, HItem, taAddFirst);
  TreeView_DeleteItem(Handle, ItemId);
  FItemId := HItem;
  Assign(Self);
  HasChildren := Children;
  Selected := IsSelected;}
end;

procedure TTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
{
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  naAdd:           add as last sibling of Destination
  naAddFirst:      add as first sibling of Destnation
  naAddChild:      add as last child of Destination
  naAddChildFirst: add as first child of Destination
  naInsert:        insert in front of Destination
}
var
  AddMode: TAddMode;
  //ANode: TTreeNode;
  //HItem: HTreeItem;
  OldOnChanging: TTVChangingEvent;
  OldOnChange: TTVChangedEvent;
begin
  if (Destination=nil) and (Mode in [naAddChild,naAddChildFirst,naInsert]) then
    TreeNodeError('TTreeNode.MoveTo Destination=nil');
  if (Destination = nil) or not Destination.HasAsParent(Self) then begin
    OldOnChanging := TreeView.OnChanging;
    OldOnChange := TreeView.OnChange;
    TreeView.OnChanging := nil;
    TreeView.OnChange := nil;
    try
      if (Destination <> nil) and (Mode in [naAdd, naAddFirst]) then
        Destination := Destination.Parent;
      case Mode of
        naAdd,
        naAddChild: AddMode := taAdd;
        naAddFirst,
        naAddChildFirst: AddMode := taAddFirst;
        naInsert: AddMode := taInsert;
      else
        AddMode:=taAdd;
      end;
      {if ANode <> nil then
        HItem := ANode.ItemId else
        HItem := nil;}
      if (Destination <> Self) then
        InternalMove(Destination, AddMode);
      if Parent <> nil then
        Parent.Expanded := True;
    finally
      TreeView.OnChanging := OldOnChanging;
      TreeView.OnChange := OldOnChange;
    end;
  end;
end;

procedure TTreeNode.MultiSelectGroup;
var
  FirstNode, LastNode, ANode: TTreeNode;
begin
  if (TreeView<>nil) and (not (tvoAllowMultiselect in TreeView.Options)) then
    exit;
  FirstNode:=GetPrevSibling;
  while (FirstNode<>nil) and (not FirstNode.MultiSelected) do
    FirstNode:=FirstNode.GetPrevSibling;
  if FirstNode=nil then FirstNode:=Self;
  LastNode:=GetNextSibling;
  while (LastNode<>nil) and (not LastNode.MultiSelected) do
    LastNode:=LastNode.GetNextSibling;
  if LastNode=nil then LastNode:=Self;
  ANode:=FirstNode;
  while ANode<>nil do begin
    ANode.MultiSelected:=true;
    if ANode=LastNode then break;
    ANode:=ANode.GetNextSibling;
  end;
end;

procedure TTreeNode.MakeVisible;
begin
  if TreeView<>nil then
    TreeView.EnsureNodeIsVisible(Self)
  else
    ExpandParents;
end;

function TTreeNode.GetLevel: Integer;
// root is on level 0
var
  ANode: TTreeNode;
begin
  Result := 0;
  ANode := Parent;
  while ANode <> nil do begin
    Inc(Result);
    ANode := ANode.Parent;
  end;
end;

function TTreeNode.GetMultiSelected: Boolean;
begin
  Result := GetState(nsMultiSelected);
end;

function TTreeNode.IsNodeVisible: Boolean;
//var Rect: TRect;
begin
  if TreeView<>nil then
    Result:=TreeView.IsNodeVisible(Self)
  else
    Result:=AreParentsExpanded;
  //Result := TreeView_GetItemRect(Handle, ItemId, Rect, True);
end;

procedure TTreeNode.Update;
begin
  if (TreeView<>nil) and (not (csLoading in TreeView.ComponentState)) then
    TreeView.Invalidate;
end;

function TTreeNode.EditText: Boolean;
begin
  // ToDo:
  Result:=false;
  //Result := TreeView_EditLabel(Handle, ItemId) <> 0;
end;

function TTreeNode.DisplayRect(TextOnly: Boolean): TRect;
begin
  FillChar(Result, SizeOf(Result), 0);
  if TreeView<>nil then begin
    Result.Left:=TreeView.BorderWidth;
    Result.Top:=Top-TreeView.ScrolledTop+TreeView.BorderWidth;
    Result.Right:=TreeView.ClientWidth-TreeView.BorderWidth;
    Result.Bottom:=Result.Top+Height;
    if TextOnly then begin
      Result.Left:=DisplayTextLeft;
      if Result.Left>Result.Right then
        Result.Left:=Result.Right;
      Result.Right:=DisplayTextRight;
    end;
    //TreeView_GetItemRect(Handle, ItemId, Result, TextOnly);
  end;
end;

function TTreeNode.DisplayExpandSignLeft: integer;
begin
  Result:=0;
  if TreeView<>nil then begin
    inc(Result,TreeView.Indent*Level+TreeView.BorderWidth);
  end;
end;

function TTreeNode.DisplayExpandSignRect: TRect;
begin
  FillChar(Result, SizeOf(Result), 0);
  if TreeView<>nil then begin
    Result.Left:=DisplayExpandSignLeft;
    Result.Top:=Top;
    Result.Right:=Result.Left+TreeView.Indent;
    Result.Bottom:=Top+Height;
  end;
end;

function TTreeNode.DisplayExpandSignRight: integer;
begin
  Result:=DisplayExpandSignLeft;
  if TreeView<>nil then begin
    inc(Result,TreeView.Indent);
  end;
end;

function TTreeNode.DisplayIconLeft: integer;
begin
  Result:=DisplayExpandSignLeft;
  if (TreeView<>nil) then
    inc(Result,TreeView.Indent);
end;

function TTreeNode.DisplayStateIconLeft: integer;
begin
  Result:=DisplayIconLeft;
  if (TreeView<>nil) and (TreeView.Images<>nil) then
    inc(Result,TreeView.Images.Width);
end;

function TTreeNode.DisplayTextLeft: integer;
begin
  Result:=DisplayStateIconLeft;
  if (TreeView<>nil) and (TreeView.StateImages<>nil) then
    inc(Result,TreeView.StateImages.Width);
end;

function TTreeNode.DisplayTextRight: integer;
begin
  Result:=DisplayTextLeft;
  if TreeView<>nil then
    Inc(Result,TreeView.Canvas.TextWidth(Text));
end;

function TTreeNode.AlphaSort: Boolean;
begin
  Result := CustomSort(nil);
end;

function TTreeNode.CustomSort(SortProc: TTreeNodeCompare): Boolean;
//var SortCB: TTVSortCB;

  procedure Merge(Src,Buffer: TTreeNodeArray; Pos1, Pos2, Pos3: integer);
  // merge two sorted arrays (result is in Src)
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
  begin
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=SortProc(Src[Src1Pos],Src[Src2Pos]);
      if cmp>0 then begin
        Buffer[DestPos]:=Src[Src1Pos];
        dec(Src1Pos);
      end else begin
        Buffer[DestPos]:=Src[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      Buffer[DestPos]:=Src[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      Src[a]:=Buffer[a];
  end;

  procedure MergeSort(Src,Buffer: TTreeNodeArray; StartPos, EndPos: integer);
  // sort Src from Position StartPos to EndPos (both included)
  var cmp,mid:integer;
  begin
    if StartPos>=EndPos then begin
      // sort one element -> very easy :)
    end else if StartPos+1=EndPos then begin
      // sort two elements -> quite easy :)
      cmp:=SortProc(Src[StartPos],Src[EndPos]);
      if cmp>0 then begin
        Buffer[StartPos]:=Src[StartPos];
        Src[StartPos]:=Src[EndPos];
        Src[EndPos]:=Buffer[StartPos];
      end;
    end else begin
      // sort more than two elements -> Mergesort
      mid:=(StartPos+EndPos) shr 1;
      MergeSort(Src,Buffer,StartPos,mid);
      MergeSort(Src,Buffer,mid+1,EndPos);
      Merge(Src,Buffer,StartPos,mid+1,EndPos);
    end;
  end;

var FMergedItems: TTreeNodeArray;
begin
  if FCount>0 then begin
    if Owner<>nil then Owner.ClearCache;
    if SortProc=nil then SortProc:=@DefaultTreeViewSort;
    GetMem(FMergedItems,SizeOf(Pointer)*FCount);
    MergeSort(FItems,FMergedItems,0,FCount-1);
    {
    with SortCB do begin
      if not Assigned(SortProc) then lpfnCompare := @DefaultTreeViewSort
      else lpfnCompare := SortProc;
      hParent := ItemId;
      lParam := Data;
    end;
    Result := TreeView_SortChildrenCB(Handle, SortCB, 0);
    }
  end;
  Result:=true;
end;

procedure TTreeNode.Delete;
begin
  if not Deleting then Free;
end;

procedure TTreeNode.DeleteChildren;
begin
  if Owner<>nil then Owner.ClearCache;
  Collapse(true);
  HasChildren := False;
end;

procedure TTreeNode.Assign(Source: TPersistent);
var
  ANode: TTreeNode;
begin
  if Owner<>nil then Owner.ClearCache;
  if Source is TTreeNode then
  begin
    ANode := TTreeNode(Source);
    Text := ANode.Text;
    Data := ANode.Data;
    ImageIndex := ANode.ImageIndex;
    SelectedIndex := ANode.SelectedIndex;
    StateIndex := ANode.StateIndex;
    OverlayIndex := ANode.OverlayIndex;
    Height := ANode.Height;
    Focused := ANode.Focused;
    //DropTarget := ANode.DropTarget;
    Cut := ANode.Cut;
    HasChildren := ANode.HasChildren;
  end
  else inherited Assign(Source);
end;

function TTreeNode.IsEqual(Node: TTreeNode): Boolean;
begin
  Result := (Text = Node.Text) and (Data = Node.Data);
end;

procedure TTreeNode.ReadData(Stream: TStream; StreamVersion: integer;
  Info: PTreeNodeInfo);
var
  I, ItemCount: Integer;
  NewExpanded: boolean;
begin
  if Owner<>nil then Owner.ClearCache;
  Stream.ReadBuffer(Info^, SizeOf(TTreeNodeInfo));
  ImageIndex := Info^.ImageIndex;
  SelectedIndex := Info^.SelectedIndex;
  StateIndex := Info^.StateIndex;
  OverlayIndex := Info^.OverlayIndex;
  Data := Info^.Data;
  Height := Info^.Height;
  NewExpanded := Info^.Expanded;
  SetLength(FText,Info^.TextLen);
  if FText<>'' then
    Stream.Read(FText[1],length(FText));
  if Owner<>nil then begin
    ItemCount := Info^.Count;
    for I := 0 to ItemCount - 1 do
      Owner.AddChild(Self, '').ReadData(Stream, StreamVersion, Info);
  end;
  Expanded := NewExpanded;
end;

procedure TTreeNode.ReadDelphiData(Stream: TStream; Info: PDelphiNodeInfo);
var
  I, Size, ItemCount: Integer;
begin
  if Owner<>nil then Owner.ClearCache;
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Info^, Size);
  Text := Info^.Text;
  ImageIndex := Info^.ImageIndex;
  SelectedIndex := Info^.SelectedIndex;
  StateIndex := Info^.StateIndex;
  OverlayIndex := Info^.OverlayIndex;
  Data := Info^.Data;
  if Owner<>nil then begin
    ItemCount := Info^.Count;
    for I := 0 to ItemCount - 1 do
      Owner.AddChild(Self, '').ReadDelphiData(Stream, Info);
  end;
end;

procedure TTreeNode.WriteData(Stream: TStream; Info: PTreeNodeInfo);
var i: integer;
begin
  Info^.ImageIndex := ImageIndex;
  Info^.SelectedIndex := SelectedIndex;
  Info^.OverlayIndex := OverlayIndex;
  Info^.StateIndex := StateIndex;
  Info^.Data := Data;
  Info^.Height := FHeight;
  Info^.Count := Count;
  Info^.Expanded := Expanded;
  Info^.TextLen := Length(Text);
  Stream.WriteBuffer(Info^, SizeOf(TTreeNodeInfo));
  if Text<>'' then
    Stream.Write(FText[1],length(Text));
  for i := 0 to Count - 1 do
    Items[i].WriteData(Stream, Info);
end;

procedure TTreeNode.WriteDelphiData(Stream: TStream; Info: PDelphiNodeInfo);
var
  I, Size, L, ItemCount: Integer;
begin
  L := Length(Text);
  if L > 255 then L := 255;
  Size := SizeOf(TDelphiNodeInfo) + L - 255;
  Info^.Text := Text;
  Info^.ImageIndex := ImageIndex;
  Info^.SelectedIndex := SelectedIndex;
  Info^.OverlayIndex := OverlayIndex;
  Info^.StateIndex := StateIndex;
  Info^.Data := Data;
  ItemCount := Count;
  Info^.Count := ItemCount;
  Stream.WriteBuffer(Size, SizeOf(Size));
  Stream.WriteBuffer(Info^, Size);
  for I := 0 to ItemCount - 1 do
    Items[I].WriteDelphiData(Stream, Info);
end;

function TTreeNode.ConsistencyCheck: integer;
var RealSubTreeCount: integer;
  i: integer;
  Node1: TTreeNode;
begin
  if FOwner<>nil then begin
  end;
  if FCapacity<0 then exit(-1);
  if FCapacity<FCount then exit(-2);
  if FCount<0 then exit(-3);
  if FHeight<0 then exit(-4);
  if (FItems<>nil) and (FCapacity<=0) then exit(-5);
  if (FCapacity>0) and (FItems=nil) then exit(-6);
  if (FNextBrother<>nil) and (FNextBrother.FPrevBrother<>Self) then exit(-7);
  if (FPrevBrother<>nil) and (FPrevBrother.FNextBrother<>Self) then exit(-8);
  // check childs
  RealSubTreeCount:=1;
  for i:=0 to FCount-1 do begin
    if (Items[i]=nil) then exit(-9);
    if (i=0) and (Items[i].FPrevBrother<>nil) then exit(-10);
    if (i>0) and (Items[i].FPrevBrother<>Items[i-1]) then exit(-11);
    if (i<FCount-1) and (Items[i].FNextBrother<>Items[i+1]) then exit(-12);
    if (i=FCount-1) and (Items[i].FNextBrother<>nil) then exit(-13);
    if Items[i].FParent<>Self then exit(-14);
    Result:=Items[i].ConsistencyCheck;
    if Result<>0 then exit;
    inc(RealSubTreeCount,Items[i].SubTreeCount);
  end;
  if FParent<>nil then begin
    if FParent.IndexOf(Self)<0 then exit(-15);
  end;
  if RealSubTreeCount<>SubTreeCount then exit(-16);
  if FTop<0 then exit(-17);
  // check for circles
  if FNextBrother=Self then exit(-18);
  if FPrevBrother=Self then exit(-19);
  if FParent=Self then exit(-20);
  Node1:=FParent;
  while Node1<>nil do begin
    if (Node1=Self) then exit(-21);
    Node1:=Node1.FParent;
  end;
  Result:=0;
end;

procedure TTreeNode.WriteDebugReport(const Prefix: string; Recurse: boolean);
var i: integer;
begin
  write(Prefix);
  write('TTreeNode.WriteDebugReport Self=',HexStr(Cardinal(Self),8));
  write(' Consistency=',ConsistencyCheck);
  write(' Text=',Text);
  writeln('');
  if Recurse then begin
    for i:=0 to FCount-1 do
      Items[i].WriteDebugReport(Prefix+'  ',true);
  end;
end;


{ TTreeNodes }

constructor TTreeNodes.Create(AnOwner: TCustomTreeView);
begin
  inherited Create;
  FOwner := AnOwner;
end;

destructor TTreeNodes.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TTreeNodes.GetCount: Integer;
begin
  Result:=FCount;
  //if Owner.HandleAllocated then Result := TreeView_GetCount(Handle)
  //else Result := 0;
end;

function TTreeNodes.GetHandle: THandle;
begin
  if Owner<>nil then
    Result:=Owner.Handle
  else
    Result:=0;
end;

procedure TTreeNodes.Delete(Node: TTreeNode);
begin
  if Owner<>nil then
  //if (Node.ItemId = nil) then
    Owner.Delete(Node);
  Node.Delete;
end;

procedure TTreeNodes.Clear;
begin
  ClearCache;
  while GetLastNode<>nil do
    GetLastNode.Delete;
end;

procedure TTreeNodes.ClearMultiSelection;
var
  ANode, OldNode: TTreeNode;
begin
  ANode:=FFirstMultiSelected;
  while ANode<>nil do begin
    OldNode:=ANode;
    ANode:=ANode.GetNextMultiSelected;
    OldNode.MultiSelected:=false;
  end;
end;

function TTreeNodes.AddChildFirst(ParentNode: TTreeNode; const S: string): TTreeNode;
begin
  Result := AddChildObjectFirst(ParentNode, S, nil);
end;

function TTreeNodes.AddChildObjectFirst(ParentNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
begin
  Result := InternalAddObject(ParentNode, S, Data, taAddFirst);
end;

function TTreeNodes.AddChild(ParentNode: TTreeNode; const S: string): TTreeNode;
begin
  Result := AddChildObject(ParentNode, S, nil);
end;

function TTreeNodes.AddChildObject(ParentNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
begin
  Result := InternalAddObject(ParentNode, S, Data, taAdd);
end;

function TTreeNodes.AddFirst(SiblingNode: TTreeNode; const S: string): TTreeNode;
begin
  Result := AddObjectFirst(SiblingNode, S, nil);
end;

function TTreeNodes.AddObjectFirst(SiblingNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
var ParentNode: TTreeNode;
begin
  if SiblingNode <> nil then
    ParentNode := SiblingNode.Parent
  else
    ParentNode := nil;
  Result := InternalAddObject(ParentNode, S, Data, taAddFirst);
end;

function TTreeNodes.Add(SiblingNode: TTreeNode; const S: string): TTreeNode;
begin
  Result := AddObject(SiblingNode, S, nil);
end;

procedure TTreeNodes.Repaint(ANode: TTreeNode);
var
  R: TRect;
begin
  if (FUpdateCount < 1) and (Owner<>nil) then begin
    while (ANode <> nil) and not ANode.IsVisible do ANode := ANode.Parent;
    if ANode <> nil then begin
      R := ANode.DisplayRect(False);
      InvalidateRect(Owner.Handle, @R, True);
    end;
  end;
end;

function TTreeNodes.AddObject(SiblingNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
var ParentNode: TTreeNode;
begin
  if SiblingNode <> nil then
    ParentNode := SiblingNode.Parent
  else
    ParentNode := nil;
  Result := InternalAddObject(ParentNode, S, Data, taAdd);
end;

procedure TTreeNodes.AddedNode(AValue: TTreeNode);
begin
  if AValue <> nil then begin
    AValue.HasChildren := True;
    Repaint(AValue);
  end;
end;

function TTreeNodes.Insert(NextNode: TTreeNode; const S: string): TTreeNode;
begin
  Result := InsertObject(NextNode, S, nil);
end;

function TTreeNodes.InsertObject(NextNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
// create a new node with Text=S and Data=Data and insert in front of
// NextNode (as sibling with same parent).
begin
  Result:=InternalAddObject(NextNode,S,Data,taInsert);
end;

function TTreeNodes.InsertBehind(PrevNode: TTreeNode; const S: string
  ): TTreeNode;
begin
  Result := InsertObjectBehind(PrevNode, S, nil);
end;

function TTreeNodes.InsertObjectBehind(PrevNode: TTreeNode; const S: string;
  Data: Pointer): TTreeNode;
// create a new node with Text=S and Data=Data and insert in front of
// NextNode (as sibling with same parent).
begin
  if (PrevNode<>nil) and (PrevNode.GetNextSibling<>nil) then
    Result:=InternalAddObject(PrevNode.GetNextSibling,S,Data,taInsert)
  else
    Result:=AddObject(PrevNode,S,Data);
end;

function TTreeNodes.InternalAddObject(Node: TTreeNode; const S: string;
  Data: Pointer; AddMode: TAddMode): TTreeNode;
{
  TAddMode = (taAddFirst, taAdd, taInsert);

  taAdd:      add Result as last child of Node
  taAddFirst: add Result as first child of Node
  taInsert:   add Result in front of Node
}
//var Item: HTreeItem;
var ok: boolean;
begin
  if Owner=nil then
    TreeNodeError('TTreeNodes.InternalAddObject Owner=nil');
  {$IFDEF TREEVIEW_DEBUG}
  write('[TTreeNodes.InternalAddObject] Node=',Node<>nil,' S=',S,
  ' AddMode=',AddModeNames[AddMode]);
  if Node<>nil then write(' Node.Text=',Node.Text);
  writeln('');
  {$ENDIF}
  Result := Owner.CreateNode;
  ok:=false;
  try
    Result.Data := Data;
    Result.Text := S;
    // move node in tree (tree of TTreeNode)
    Result.InternalMove(Node,AddMode);
    if (Owner<>nil) and Owner.AutoExpand and (Result.Parent<>nil) then
      Result.Parent.Expanded:=true;
    if (FUpdateCount=0) and (Owner<>nil) then
      Owner.Invalidate;
    ok:=true;
  finally
    // this construction creates nicer exception output
    if not ok then
      Result.Free;
  end;
end;

{function TTreeNodes.CreateItem(Node: TTreeNode): TTVItem;
begin
  Node.FInTree := True;
  with Result do
  begin
    mask := TVIF_TEXT or TVIF_PARAM or TVIF_IMAGE or TVIF_SELECTEDIMAGE;
    lParam := Longint(Node);
    pszText := LPSTR_TEXTCALLBACK;
    iImage := I_IMAGECALLBACK;
    iSelectedImage := I_IMAGECALLBACK;
  end;
end;}

{function TTreeNodes.AddItem(Parent, Target: HTreeItem;
  const Item: TTVItem; AddMode: TAddMode): HTreeItem;
var
  InsertStruct: TTVInsertStruct;
begin
  ClearCache;
  with InsertStruct do begin
    hParent := Parent;
    case AddMode of
      taAddFirst:
        hInsertAfter := TVI_FIRST;
      taAdd:
        hInsertAfter := TVI_LAST;
      taInsert:
        hInsertAfter := Target;
    end;
  end;
  InsertStruct.item := Item;
  FOwner.FChangeTimer.Enabled := False;
  Result := TreeView_InsertItem(Handle, InsertStruct);
end;}

function TTreeNodes.GetFirstNode: TTreeNode;
begin
  if FTopLvlItems<>nil then
    Result := FTopLvlItems[0]
  else
    Result := nil;
  //Result := GetNode(TreeView_GetRoot(Handle));
end;

function TTreeNodes.GetLastNode: TTreeNode;
begin
  if FTopLvlItems<>nil then
    Result := FTopLvlItems[FTopLvlCount-1]
  else
    Result := nil;
end;

function TTreeNodes.GetLastSubNode: TTreeNode;
// absolute last node
var Node: TTreeNode;
begin
  Result:=GetLastNode;
  if Result<>nil then begin
    Node:=Result.GetLastSubChild;
    if Node<>nil then Result:=Node;
  end;
end;

function TTreeNodes.GetLastExpandedSubNode: TTreeNode;
// absolute last expanded node
var Node: TTreeNode;
begin
  Result:=GetLastNode;
  while (Result<>nil) and (Result.Expanded) do begin
    Node:=Result.GetLastChild;
    if Node<>nil then Result:=Node;
  end;
end;

function TTreeNodes.GetNodeFromIndex(Index: Integer): TTreeNode;
// find node with absolute index in ALL nodes (even collapsed)
var
  I, J: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    TreeNodeError('TTreeNodes.GetNodeFromIndex Index '+IntToStr(Index)
           +' out of bounds (Count='+IntToStr(FCount)+')');
  if (FNodeCache.CacheNode <> nil) and (Abs(FNodeCache.CacheIndex - Index) <= 1)
  then begin
    with FNodeCache do
    begin
      if Index = CacheIndex then Result := CacheNode
      else if Index < CacheIndex then Result := CacheNode.GetPrev
      else Result := CacheNode.GetNext;
    end;
  end
  else begin
    Result := GetFirstNode;
    I:=0;
    while (Result<>nil) and (Index>I) do begin
      Repeat
        // calculate the absolute index of the next sibling
        J:=I+Result.FSubTreeCount;
        if J=I then
          TreeNodeError(
            'TTreeNodes.GetNodeFromIndex: Consistency error - SubTreeCount=0');
        if J<=Index then begin
          // Index > absolute index of next sibling -> search in next sibling
          Result:=Result.GetNext;
          I:=J;
        end else
          break;
      until false;
      if (Result<>nil) and (Index>I) then begin
        // Index is somewhere in subtree of Result
        Result:=Result.GetFirstChild;
        if Result=nil then
          TreeNodeError(
            'TTreeNodes.GetNodeFromIndex: Consistency error'
            +' - invalid SubTreeCount');
        inc(I);
      end;
    end;
  end;
  if Result = nil then
    TreeNodeError(
      'TTreeNodes.GetNodeFromIndex: Consistency Error - Count too big');
  FNodeCache.CacheNode := Result;
  FNodeCache.CacheIndex := Index;
end;

{function TTreeNodes.GetNode(ItemId: HTreeItem): TTreeNode;
var
  Item: TTVItem;
begin
  with Item do
  begin
    hItem := ItemId;
    mask := TVIF_PARAM;
  end;
  if TreeView_GetItem(Handle, Item) then Result := TTreeNode(Item.lParam)
  else Result := nil;
end;}

procedure TTreeNodes.SetItem(Index: Integer; AValue: TTreeNode);
begin
  GetNodeFromIndex(Index).Assign(AValue);
end;

procedure TTreeNodes.SetTopLvlItems(Index: integer; AValue: TTreeNode);
begin
  GetTopLvlItems(Index).Assign(AValue);
end;

procedure TTreeNodes.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TTreeNodes.SetUpdateState(Updating: Boolean);
begin
  //SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if Updating then
    Include(Owner.FStates,tvsUpdating)
  else
    Exclude(Owner.FStates,tvsUpdating);
  if not Updating then Owner.Refresh;
end;

procedure TTreeNodes.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

procedure TTreeNodes.GrowTopLvlItems;
begin
  if FTopLvlItems<>nil then begin
    FTopLvlCapacity:=FTopLvlCapacity shl 1;
    ReAllocMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
  end else begin
    FTopLvlCapacity:=10;
    GetMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
  end;
end;

function TTreeNodes.GetTopLvlItems(Index: integer): TTreeNode;
begin
  Result:=FTopLvlItems[Index];
end;

procedure TTreeNodes.ShrinkTopLvlItems;
begin
  if FTopLvlCount>0 then begin
    FTopLvlCapacity:=FTopLvlCapacity shr 1;
    if FTopLvlCapacity<FTopLvlCount then FTopLvlCapacity:=FTopLvlCount;
    ReAllocMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
  end else begin
    FTopLvlCapacity:=0;
    FreeMem(FTopLvlItems);
    FTopLvlItems:=nil;
  end;
end;

function TTreeNodes.IndexOfTopLvlItem(Node: TTreeNode): integer;
begin
  if (FTopLvlCount>0) and (FTopLvlItems[0]=Node) then exit(0);
  Result:=FTopLvlCount-1;
  while (Result>=0) and (FTopLvlItems[Result]<>Node) do dec(Result);
end;

procedure TTreeNodes.MoveTopLvlNode(TopLvlFromIndex, TopLvlToIndex: integer;
  Node: TTreeNode);
var i: integer;
begin
{$IFDEF TREEVIEW_DEBUG}
writeln('[TTreeNodes.MoveTopLvlNode] TopLvlFromIndex=',TopLvlFromIndex,
' TopLvlToIndex=',TopLvlToIndex,' Node.Text=',Node.Text);
{$ENDIF}
  if (TopLvlFromIndex>=FTopLvlCount) then
    TreeNodeError('TTreeNodes.MoveTopLvlNode TopLvlFromIndex>FTopLvlCount');
  if (TopLvlToIndex>FTopLvlCount) then
    TreeNodeError('TTreeNodes.MoveTopLvlNode TopLvlFromIndex>FTopLvlCount');
  if (TopLvlFromIndex>=0) then begin
    Node:=FTopLvlItems[TopLvlFromIndex];
    if (TopLvlToIndex>=0) then begin
      // move node
      if TopLvlToIndex=TopLvlFromIndex then exit;
      if TopLvlFromIndex<TopLvlToIndex then begin
        // move forward
        for i:=TopLvlFromIndex to TopLvlToIndex-1 do
          FTopLvlItems[i]:=FTopLvlItems[i+1];
      end else begin
        // move backward
        for i:=TopLvlToIndex downto TopLvlFromIndex+1 do
          FTopLvlItems[i]:=FTopLvlItems[i-1];
      end;
      FTopLvlItems[TopLvlToIndex]:=Node;
    end else begin
      // remove node
      for i:=TopLvlFromIndex to FTopLvlCount-2 do
        FTopLvlItems[i]:=FTopLvlItems[i+1];
      Dec(FTopLvlCount);
      if FTopLvlCount<(FTopLvlCapacity shr 2) then ShrinkTopLvlItems;
    end;
  end else begin
    if (TopLvlToIndex>=0) then begin
      if Node=nil then
        TreeNodeError('TTreeNodes.MoveTopLvlNode inserting nil');
      // insert node
      if FTopLvlCount=FTopLvlCapacity then GrowTopLvlItems;
      inc(FTopLvlCount);
      for i:=FTopLvlCount-1 downto TopLvlToIndex+1 do
        FTopLvlItems[i]:=FTopLvlItems[i-1];
      FTopLvlItems[TopLvlToIndex]:=Node;
    end else begin
      // nothing to do
    end;
  end;
end;

procedure TTreeNodes.Assign(Source: TPersistent);
var
  TreeNodes: TTreeNodes;
  MemStream: TMemoryStream;
begin
  ClearCache;
  if Source is TTreeNodes then begin
    TreeNodes := TTreeNodes(Source);
    Clear;
    MemStream := TMemoryStream.Create;
    try
      TreeNodes.WriteData(MemStream);
      MemStream.Position := 0;
      ReadData(MemStream);
    finally
      MemStream.Free;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TTreeNodes.DefineProperties(Filer: TFiler);

  function WriteNodes: Boolean;
  var
    I: Integer;
    Nodes: TTreeNodes;
  begin
    Nodes := TTreeNodes(Filer.Ancestor);
    if Nodes = nil then
      Result := Count > 0
    else if Nodes.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Items[I].IsEqual(Nodes[I]);
        if Result then Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, WriteNodes);
end;

procedure TTreeNodes.ReadData(Stream: TStream);
var
  I, NewCount, MagicNumber: Integer;
  NodeInfo: TDelphiNodeInfo;
  StreamVersion: word;
begin
  Clear;
  // -7 for lcl stream
  Stream.ReadBuffer(MagicNumber,SizeOf(Integer));
  if MagicNumber=LCLStreamID then begin
    // read stream version
    Stream.ReadBuffer(StreamVersion,SizeOf(StreamVersion));
    // read top level node count
    Stream.ReadBuffer(NewCount, SizeOf(NewCount));
    for I := 0 to NewCount - 1 do
      Add(nil, '').ReadData(Stream, StreamVersion, @NodeInfo);
  end else begin
    // delphi stream
    NewCount:=MagicNumber;
    for I := 0 to NewCount - 1 do
      Add(nil, '').ReadDelphiData(Stream, @NodeInfo);
  end;
end;

procedure TTreeNodes.WriteData(Stream: TStream);
var
  ANode: TTreeNode;
  NodeInfo: TDelphiNodeInfo;
  MagicNumber: integer;
begin
  // -7 for lcl stream
  MagicNumber:=LCLStreamID;
  Stream.WriteBuffer(MagicNumber,SizeOf(MagicNumber));
  // write stream version
  Stream.WriteBuffer(TTreeNodeStreamVersion,SizeOf(Word));
  // write top level node count
  Stream.WriteBuffer(FTopLvlCount, SizeOf(Integer));
  // write all nodes recursively
  ANode := GetFirstNode;
  while ANode <> nil do begin
    ANode.WriteData(Stream, @NodeInfo);
    ANode := ANode.GetNextSibling;
  end;
end;

procedure TTreeNodes.ReadExpandedState(Stream: TStream);
var
  ItemCount,
  Index: Integer;
  Node: TTreeNode;
  NodeExpanded: Boolean;
begin
  // ToDo: read different stream formats
  if Stream.Position < Stream.Size then
    Stream.ReadBuffer(ItemCount, SizeOf(ItemCount))
  else Exit;
  Index := 0;
  Node := GetFirstNode;
  while (Index < ItemCount) and (Node <> nil) do begin
    Stream.ReadBuffer(NodeExpanded, SizeOf(NodeExpanded));
    Node.Expanded := NodeExpanded;
    Inc(Index);
    Node := Node.GetNext;
  end;
end;

procedure TTreeNodes.WriteExpandedState(Stream: TStream);
var
  Size: Integer;
  ANode: TTreeNode;
  NodeExpanded: Boolean;
begin
  // ToDo: read different stream formats
  Size := SizeOf(Boolean) * Count;
  Stream.WriteBuffer(Size, SizeOf(Size));
  ANode := GetFirstNode;
  while (ANode <> nil) do begin
    NodeExpanded := ANode.Expanded;
    Stream.WriteBuffer(NodeExpanded, SizeOf(Boolean));
    ANode := ANode.GetNext;
  end;
end;

procedure TTreeNodes.ClearCache;
begin
  FNodeCache.CacheNode := nil;
end;

function TTreeNodes.ConsistencyCheck: integer;
var Node: TTreeNode;
  RealCount, i: integer;
  OldCache: TNodeCache;
begin
  if FUpdateCount<0 then exit(-1);
  RealCount:=0;
  Node:=GetFirstNode;
  while Node<>nil do begin
    Result:=Node.ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);
      exit;
    end;
    inc(RealCount,Node.SubTreeCount);
//writeln(' ConsistencyCheck: B  ',RealCount,',',Node.SubTreeCount);
    Node:=Node.FNextBrother;
  end;
//writeln(' ConsistencyCheck: B  ',RealCount,',',FCount);
  if RealCount<>FCount then exit(-3);
  if (FTopLvlCapacity<=0) and (FTopLvlItems<>nil) then exit(-4);
  if (FTopLvlCapacity>0) and (FTopLvlItems=nil) then exit(-5);
  if FTopLvlCapacity<FTopLvlCount then exit(-6);
  if (FTopLvlCount<0) then exit(-7);
  for i:=0 to FTopLvlCount-1 do begin
    if (i=0) and (FTopLvlItems[i].FPrevBrother<>nil) then exit(-8);
    if (i>0) and (FTopLvlItems[i].FPrevBrother<>FTopLvlItems[i-1]) then
      exit(-9);
    if (i<FTopLvlCount-1) and (FTopLvlItems[i].FNextBrother<>FTopLvlItems[i+1])
    then begin
writeln(' CONSISTENCY i=',i,' FTopLvlCount=',FTopLvlCount,' FTopLvlItems[i]=',HexStr(Cardinal(FTopLvlItems[i]),8),' FTopLvlItems[i].FNextBrother=',HexStr(Cardinal(FTopLvlItems[i].FNextBrother),8),' FTopLvlItems[i+1]=',HexStr(Cardinal(FTopLvlItems[i+1]),8));
      exit(-10);
    end;
    if (i=FTopLvlCount-1) and (FTopLvlItems[i].FNextBrother<>nil) then
      exit(-11);
  end;
  if FNodeCache.CacheNode<>nil then begin
    OldCache:=FNodeCache;
    ClearCache;
    if GetNodeFromIndex(OldCache.CacheIndex)<>OldCache.CacheNode then exit(-12);
  end;
  Result:=0;
end;

procedure TTreeNodes.WriteDebugReport(const Prefix: string; AllNodes: boolean);
var Node: TTreeNode;
begin
  write(Prefix);
  write('TTreeNodes.WriteDebugReport Self=',HexStr(Cardinal(Self),8));
  write(' Consistency=',ConsistencyCheck);
  writeln('');
  if AllNodes then begin
    Node:=GetFirstNode;
    while Node<>nil do begin
      Node.WriteDebugReport(Prefix+'  ',true);
      Node:=Node.GetNextSibling;
    end;
  end;
end;


type
  TTreeStrings = class(TStrings)
  private
    FOwner: TTreeNodes;
  protected
    function Get(Index: Integer): string; override;
    function GetBufStart(Buffer: PChar; var Level: Integer): PChar;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(AnOwner: TTreeNodes);
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure LoadTreeFromStream(Stream: TStream);
    procedure SaveTreeToStream(Stream: TStream);
    function ConsistencyCheck: integer;
    procedure WriteDebugReport(const Prefix: string);
    property Owner: TTreeNodes read FOwner;
  end;

constructor TTreeStrings.Create(AnOwner: TTreeNodes);
begin
  inherited Create;
  FOwner := AnOwner;
end;

function TTreeStrings.Get(Index: Integer): string;
const
  TabChar = #9;
var
  Level, I: Integer;
  Node: TTreeNode;
begin
  Result := '';
  Node := Owner.GetNodeFromIndex(Index);
  Level := Node.Level;
  for I := 0 to Level - 1 do Result := Result + TabChar;
  Result := Result + Node.Text;
end;

function TTreeStrings.GetBufStart(Buffer: PChar; var Level: Integer): PChar;
begin
  Level := 0;
  while Buffer^ in [' ', #9] do
  begin
    Inc(Buffer);
    Inc(Level);
  end;
  Result := Buffer;
end;

function TTreeStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(Owner.GetNodeFromIndex(Index).Data);
end;

procedure TTreeStrings.PutObject(Index: Integer; AObject: TObject);
begin
  Owner.GetNodeFromIndex(Index).Data := AObject;
end;

function TTreeStrings.GetCount: Integer;
begin
  Result := Owner.Count;
end;

procedure TTreeStrings.Clear;
begin
  Owner.Clear;
end;

procedure TTreeStrings.Delete(Index: Integer);
begin
  Owner.GetNodeFromIndex(Index).Delete;
end;

procedure TTreeStrings.SetUpdateState(Updating: Boolean);
begin
  //SendMessage(Owner.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then Owner.Owner.Refresh;
end;

function TTreeStrings.Add(const S: string): Integer;
var
  Level, OldLevel, I: Integer;
  NewStr: string;
  Node: TTreeNode;
begin
  Result := GetCount;
  if (Length(S) = 1) and (S[1] = Chr($1A)) then Exit;
  Node := nil;
  OldLevel := 0;
  NewStr := GetBufStart(PChar(S), Level);
  if Result > 0 then
  begin
    Node := Owner.GetNodeFromIndex(Result - 1);
    OldLevel := Node.Level;
  end;
  if (Level > OldLevel) or (Node = nil) then
  begin
    if Level - OldLevel > 1 then
      TreeViewError('TTreeStrings.Add: Invalid level, Level='+IntToStr(Level)
        +' OldLevel='+IntToStr(OldLevel));
  end
  else begin
    for I := OldLevel downto Level do
    begin
      Node := Node.Parent;
      if (Node = nil) and (I - Level > 0) then
        TreeViewError('TTreeStrings.Add: Invalid level, Node=nil I='+IntToStr(I)
          +' Level='+IntToStr(Level));
    end;
  end;
  Owner.AddChild(Node, NewStr);
end;

procedure TTreeStrings.Insert(Index: Integer; const S: string);
begin
  with Owner do
    Insert(GetNodeFromIndex(Index), S);
end;

procedure TTreeStrings.LoadTreeFromStream(Stream: TStream);
var
  List: TStringList;
  ANode, NextNode: TTreeNode;
  ALevel, i: Integer;
  CurrStr: string;
  ok: boolean;
begin
  List := TStringList.Create;
  Owner.BeginUpdate;
  ok:=false;
  try
    Clear;
    List.LoadFromStream(Stream);
    ANode := nil;
    for i := 0 to List.Count - 1 do
    begin
      CurrStr := GetBufStart(PChar(List[i]), ALevel);
      if ANode = nil then
        ANode := Owner.AddChild(nil, CurrStr)
      else if ANode.Level = ALevel then
        ANode := Owner.AddChild(ANode.Parent, CurrStr)
      else if ANode.Level = (ALevel - 1) then
        ANode := Owner.AddChild(ANode, CurrStr)
      else if ANode.Level > ALevel then
      begin
        NextNode := ANode.Parent;
        while NextNode.Level > ALevel do
          NextNode := NextNode.Parent;
        ANode := Owner.AddChild(NextNode.Parent, CurrStr);
      end
      else TreeViewError('TTreeStrings.LoadTreeFromStream: Level='
        +IntToStr(ALevel)+' CuurStr="'+CurrStr+'"');
    end;
    ok:=true;
  finally
    Owner.EndUpdate;
    List.Free;
    if not ok then
      Owner.Owner.Invalidate;  // force repaint on exception
  end;
end;

procedure TTreeStrings.SaveTreeToStream(Stream: TStream);
const
  TabChar = #9;
  EndOfLine = #13#10;
var
  i: Integer;
  ANode: TTreeNode;
  NodeStr: string;
begin
  if Count > 0 then
  begin
    ANode := Owner[0];
    while ANode <> nil do
    begin
      NodeStr := '';
      for i := 0 to ANode.Level - 1 do NodeStr := NodeStr + TabChar;
      NodeStr := NodeStr + ANode.Text + EndOfLine;
      Stream.Write(Pointer(NodeStr)^, Length(NodeStr));
      ANode := ANode.GetNext;
    end;
  end;
end;

function TTreeStrings.ConsistencyCheck: integer;
begin
  Result:=0;
end;

procedure TTreeStrings.WriteDebugReport(const Prefix: string);
begin
  write(Prefix);
  write('TTreeStrings.WriteDebugReport Self=',HexStr(Cardinal(Self),8));
  write(' Consistency=',ConsistencyCheck);
  writeln('');
end;


{ TCustomTreeView }

constructor TCustomTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  ControlStyle := ControlStyle - [csCaptureMouse]
                               + [csDisplayDragImage, csReflector];
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FBackgroundColor := clWhite;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FDefItemHeight:=20;
  FExpandSignType:=tvestPlusMinus;
  FExpandSignSize:=9;
  FTreeNodes := TTreeNodes.Create(Self);
  FBorderStyle := bsSingle;
  BorderWidth := 2;
  FOptions := [tvoShowRoot, tvoShowLines, tvoShowButtons, tvoHideSelection,
               tvoToolTips, tvoKeepCollapsedNodes];
  Items.KeepCollapsedNodes:=KeepCollapsedNodes;
  FScrollBars:=ssBoth;
  FDragImage := TDragImageList.CreateSize(32, 32);
  FIndent:=15;
  FChangeTimer := TTimer.Create(Self);
  FChangeTimer.Enabled := False;
  FChangeTimer.Interval := 0;
  FChangeTimer.OnTimer := @OnChangeTimer;
  //FEditInstance := MakeObjectInstance(EditWndProc);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FSelectedColor:=clHighlight;
  fSeparatorColor:=clGray;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := @ImageListChange;
  FStates:=[tvsMaxLvlNeedsUpdate,tvsMaxRightNeedsUpdate,tvsScrollbarChanged];
  FTreeLineColor := clWindowFrame;
end;

destructor TCustomTreeView.Destroy;
begin
  FTreeNodes.Free;
  FTreeNodes:=nil;
  FChangeTimer.Free;
  FSaveItems.Free;
  FDragImage.Free;
  //FMemStream.Free;
  //FreeObjectInstance(FEditInstance);
  FImageChangeLink.Free;
  FStateChangeLink.Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TCustomTreeView.CreateParams(var Params: TCreateParams);
{const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  LineStyles: array[Boolean] of DWORD = (0, TVS_HASLINES);
  RootStyles: array[Boolean] of DWORD = (0, TVS_LINESATROOT);
  ButtonStyles: array[Boolean] of DWORD = (0, TVS_HASBUTTONS);
  EditStyles: array[Boolean] of DWORD = (TVS_EDITLABELS, 0);
  HideSelections: array[Boolean] of DWORD = (TVS_SHOWSELALWAYS, 0);
  DragStyles: array[TDragMode] of DWORD = (TVS_DISABLEDRAGDROP, 0);
  RTLStyles: array[Boolean] of DWORD = (0, TVS_RTLREADING);
  ToolTipStyles: array[Boolean] of DWORD = (TVS_NOTOOLTIPS, 0);
  AutoExpandStyles: array[Boolean] of DWORD = (0, TVS_SINGLEEXPAND);
  HotTrackStyles: array[Boolean] of DWORD = (0, TVS_TRACKSELECT);
  RowSelectStyles: array[Boolean] of DWORD = (0, TVS_FULLROWSELECT);}
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  //InitCommonControl(ICC_TREEVIEW_CLASSES);
  inherited CreateParams(Params);
  //CreateSubClass(Params, WC_TREEVIEW);
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeCheckOn}{$R-}{$ENDIF}
    WindowClass.Style := WindowClass.Style and not Cardinal(ClassStylesOff);
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[fBorderStyle]
      or WS_CLIPCHILDREN;
    {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
    if NewStyleControls and Ctl3D and (fBorderStyle = bsSingle) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
  {with Params do begin
    Style := Style or LineStyles[FShowLines] or BorderStyles[FBorderStyle] or
      RootStyles[FShowRoot] or ButtonStyles[FShowButtons] or
      EditStyles[FReadOnly] or HideSelections[FHideSelection] or
      DragStyles[DragMode] or RTLStyles[UseRightToLeftReading] or
      ToolTipStyles[FToolTips] or AutoExpandStyles[FAutoExpand] or
      HotTrackStyles[FHotTrack] or RowSelectStyles[FRowSelect];
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;}
end;

procedure TCustomTreeView.CreateWnd;
begin
  Exclude(FStates,tvsStateChanging);
  inherited CreateWnd;
  //TreeView_SetBkColor(Handle, ColorToRGB(Color));
  //TreeView_SetTextColor(Handle, ColorToRGB(Font.Color));
  {if FMemStream <> nil then begin
    Items.ReadData(FMemStream);
    Items.ReadExpandedState(FMemStream);
    FMemStream.Destroy;
    FMemStream := nil;
    SetTopItem(Items.GetNodeFromIndex(FSaveTopIndex));
    FSaveTopIndex := 0;
    SetSelection(Items.GetNodeFromIndex(FSaveIndex));
    FSaveIndex := 0;
  end;}
  //if (Images <> nil) and Images.HandleAllocated then
  //  SetImageList(Images.Handle, TVSIL_NORMAL);
  //if (StateImages <> nil) and StateImages.HandleAllocated then
  //  SetImageList(StateImages.Handle, TVSIL_STATE);
end;

procedure TCustomTreeView.DestroyWnd;
//var Node: TTreeNode;
begin
  Include(FStates,tvsStateChanging);
  {if (Items<>nil) and (Items.Count > 0) then begin
    FMemStream := TMemoryStream.Create;
    Items.WriteData(FMemStream);
    Items.WriteExpandedState(FMemStream);
    FMemStream.Position := 0;
    Node := GetTopItem;
    if Node <> nil then FSaveTopIndex := Node.AbsoluteIndex;
    Node := Selected;
    if Node <> nil then FSaveIndex := Node.AbsoluteIndex;
  end;}
  inherited DestroyWnd;
end;

procedure TCustomTreeView.EditWndProc(var Message: TLMessage);
var ok: boolean;
begin
  try
    ok:=false;
    with Message do
    begin
      case Msg of
        LM_KEYDOWN,
        LM_SYSKEYDOWN: if DoKeyDown(TLMKey(Message)) then Exit;
        LM_CHAR: if DoKeyPress(TLMKey(Message)) then Exit;
        LM_KEYUP,
        LM_SYSKEYUP: if DoKeyUp(TLMKey(Message)) then Exit;
        CN_KEYDOWN,
        CN_CHAR, CN_SYSKEYDOWN,
        CN_SYSCHAR:
          begin
            WndProc(Message);
            Exit;
          end;
      end;
      Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam);
    end;
    ok:=true;
  finally
    if not ok then
      Application.HandleException(Self);
  end;
end;

{procedure TCustomTreeView.CMColorChanged(var Message: TLMessage);
begin
  inherited;
  RecreateWnd;
end;}

{procedure TCustomTreeView.CMCtl3DChanged(var Message: TLMessage);
begin
  inherited;
  if FBorderStyle = bsSingle then RecreateWnd;
end;}

{procedure TCustomTreeView.CMFontChanged(var Message: TLMessage);
begin
  inherited;
  TreeView_SetTextColor(Handle, ColorToRGB(Font.Color));
end;}

{procedure TCustomTreeView.CMSysColorChange(var Message: TLMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Message.Msg := WM_SYSCOLORCHANGE;
    DefaultHandler(Message);
  end;
end;}

procedure TCustomTreeView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCustomTreeView.EndUpdate;
begin
//  if FUpdateCount<=0 then
//    writeln('TCustomTreeView.EndUpdate UpdateCount=',FUpdateCount);
  if FUpdateCount<=0 then exit;
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    // ToDo: only refresh if something changed
    UpdateScrollBars;
    Invalidate;
  end;
end;

function TCustomTreeView.AlphaSort: Boolean;
var
  Node: TTreeNode;
begin
  if HandleAllocated then begin
    BeginUpdate;
    Result := CustomSort(nil);
    Node := FTreeNodes.GetFirstNode;
    while Node <> nil do begin
      if Node.HasChildren then Node.AlphaSort;
      Node := Node.GetNext;
    end;
    EndUpdate;
  end
  else
    Result := False;
end;

function TCustomTreeView.CustomSort(SortProc: TTreeNodeCompare): Boolean;
//var SortCB: TTVSortCB;
var Node: TTreeNode;
begin
  Result := False;
  if HandleAllocated then begin
    // ToDo: sort root nodes

    {with SortCB do begin
      if not Assigned(SortProc) then lpfnCompare := @DefaultTreeViewSort
      else lpfnCompare := SortProc;
      hParent := TVI_ROOT;
      lParam := Data;
      Result := TreeView_SortChildrenCB(Handle, SortCB, 0);
    end;}
    Node := FTreeNodes.GetFirstNode;
    while Node <> nil do begin
      if Node.HasChildren then Node.CustomSort(SortProc);
      Node := Node.GetNext;
    end;
    Items.ClearCache;
  end;
end;

procedure TCustomTreeView.SetAutoExpand(Value: Boolean);
begin
  if AutoExpand <> Value then begin
    if Value then
      Include(FOptions,tvoAutoExpand)
    else
      Exclude(FOptions,tvoAutoExpand);
    //SetComCtlStyle(Self, TVS_SINGLEEXPAND, Value);
  end;
end;

procedure TCustomTreeView.SetHotTrack(Value: Boolean);
begin
  if HotTrack <> Value then begin
    if Value then
      Include(FOptions,tvoHotTrack)
    else
      Exclude(FOptions,tvoHotTrack);
    //SetComCtlStyle(Self, TVS_TRACKSELECT, Value);
  end;
end;

procedure TCustomTreeView.SetRowSelect(Value: Boolean);
begin
  if RowSelect <> Value then begin
    if Value then
      Include(FOptions,tvoRowSelect)
    else
      Exclude(FOptions,tvoRowSelect);
    if FSelectedNode<>nil then
      Invalidate;
    //SetComCtlStyle(Self, TVS_FULLROWSELECT, Value);
  end;
end;

procedure TCustomTreeView.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    RecreateWnd;
    Include(FStates,tvsScrollbarChanged);
    UpdateScrollBars;
  end;
end;

procedure TCustomTreeView.SetScrolledLeft(AValue: integer);
begin
//writeln('@@@@@ ',FScrolledTop,',',AValue);
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
  if AValue>GetMaxScrollLeft then AValue:=GetMaxScrollLeft;
  if AValue=FScrolledLeft then exit;
  FScrolledLeft:=AValue;
  Include(FStates,tvsScrollbarChanged);
  Invalidate;
end;

procedure TCustomTreeView.SetScrolledTop(AValue: integer);
begin
//writeln('$$$$$ ',FScrolledTop,',',AValue);
  if FScrolledTop=AValue then exit;
  if AValue<0 then AValue:=0;
  if AValue>GetMaxScrollTop then AValue:=GetMaxScrollTop;
  if AValue=FScrolledTop then exit;
  FScrolledTop:=AValue;
  FStates:=FStates+[tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate,
                    tvsScrollbarChanged];
  Invalidate;
end;

procedure TCustomTreeView.SetToolTips(Value: Boolean);
begin
  if ToolTips <> Value then begin
    if Value then
      Include(FOptions,tvoToolTips)
    else
      Exclude(FOptions,tvoToolTips);
    //SetComCtlStyle(Self, TVS_NOTOOLTIPS, not Value);
  end;
end;

procedure TCustomTreeView.SetSortType(Value: TSortType);
begin
  if SortType <> Value then begin
    FSortType := Value;
    if ((SortType in [stData, stBoth]) and Assigned(OnCompare)) or
      (SortType in [stText, stBoth]) then
      AlphaSort;
  end;
end;

procedure TCustomTreeView.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor<>Value then begin
    FBackgroundColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomTreeView.SetSelectedColor(Value: TColor);
begin
  if FSelectedColor<>Value then begin
    FSelectedColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomTreeView.SetBorderStyle(Value: TBorderStyle);
begin
  if BorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TCustomTreeView.Paint;
begin
  DoPaint;
end;

procedure TCustomTreeView.SetDragMode(Value: TDragMode);
begin
  // ToDo: implement Drag&Drop
  //if Value <> DragMode then
  //  SetComCtlStyle(Self, TVS_DISABLEDRAGDROP, Value = dmManual);
  inherited;
end;

procedure TCustomTreeView.SetOptions(NewOptions: TTreeViewOptions);
var ChangedOptions: TTreeViewOptions;
begin
  if FOptions=NewOptions then exit;
  ChangedOptions:=(FOptions-NewOptions)+(NewOptions-FOptions);
  FOptions:=NewOptions;
  if tvoKeepCollapsedNodes in ChangedOptions then
    Items.KeepCollapsedNodes:=(tvoKeepCollapsedNodes in FOptions);
  if (tvoReadOnly in ChangedOptions) and (not (tvoReadOnly in FOptions)) then
    EndEditing;
  if (tvoAllowMultiselect in ChangedOptions) then begin
    if (tvoAllowMultiselect in FOptions) then begin
      if Selected<>nil then Selected.MultiSelected:=true;
    end else begin
      Items.ClearMultiSelection;
    end;
  end;
  if ([tvoHideSelection,tvoReadOnly,tvoShowButtons,tvoShowRoot,tvoShowLines]
    * ChangedOptions)<>[]
  then
    Invalidate;
end;

procedure TCustomTreeView.UpdateAllTops;

  procedure CalculateTops(FirstSibling: TTreeNode; var CurTop: integer;
    Expanded: boolean);
  begin
    while FirstSibling<>nil do begin
      FirstSibling.fTop:=CurTop;
      if Expanded then
        inc(CurTop,FirstSibling.Height);
      CalculateTops(FirstSibling.GetFirstChild,CurTop,
        Expanded and FirstSibling.Expanded);
      FirstSibling:=FirstSibling.GetNextSibling;
    end;
  end;

var i: integer;
begin
  if not (tvsTopsNeedsUpdate in FStates) then exit;
  i:=0;
  CalculateTops(Items.GetFirstNode,i,true);
  Exclude(FStates,tvsTopsNeedsUpdate);
  Include(FStates,tvsScrollbarChanged);
end;

procedure TCustomTreeView.UpdateMaxLvl;

  procedure LookInChildsAndBrothers(Node: TTreeNode; CurLvl: integer);
  begin
    if Node=nil then exit;
    if CurLvl>FMaxLvl then FMaxLvl:=CurLvl;
    LookInChildsAndBrothers(Node.GetFirstChild,CurLvl+1);
    LookInChildsAndBrothers(Node.GetNextSibling,CurLvl);
  end;

begin
  if not (tvsMaxLvlNeedsUpdate in FStates) then exit;
  FMaxLvl:=0;
  LookInChildsAndBrothers(Items.GetFirstNode,1);
  Exclude(FStates,tvsMaxRightNeedsUpdate);
end;

procedure TCustomTreeView.UpdateMaxRight;
var Node: TTreeNode;
  i: integer;
begin
  if not (tvsMaxRightNeedsUpdate in FStates) then exit;
  FMaxRight:=0;
  Node:=Items.GetFirstNode;
  while Node<>nil do begin
    i:=Node.DisplayTextRight;
    if FMaxRight<i then FMaxRight:=i;
    Node:=Node.GetNext;
  end;
  Exclude(FStates,tvsMaxRightNeedsUpdate);
  Include(FStates,tvsScrollbarChanged);
end;

procedure TCustomTreeView.UpdateTopItem;
begin
//writeln('TCustomTreeView.UpdateTopItem tvsTopItemNeedsUpdate in FStates=',tvsTopItemNeedsUpdate in FStates);
  if (FStates*[tvsTopItemNeedsUpdate,tvsTopsNeedsUpdate]=[]) then exit;
  FTopItem:=GetNodeAtY(BorderWidth);
  Exclude(FStates,tvsTopItemNeedsUpdate);
end;

procedure TCustomTreeView.UpdateBottomItem;
begin
  if (FStates*[tvsTopItemNeedsUpdate,tvsTopsNeedsUpdate,
    tvsBottomItemNeedsUpdate]=[])
  then exit;
  if not (tvsBottomItemNeedsUpdate in FStates) then exit;
  FBottomItem:=TopItem;
  while (FBottomItem<>nil) and (FBottomItem.GetNextVisible<>nil) do
    FBottomItem:=FBottomItem.GetNextVisible;
  Exclude(FStates,tvsBottomItemNeedsUpdate);
end;

procedure TCustomTreeView.SetBottomItem(Value: TTreeNode);
begin
  if HandleAllocated and (Value <> nil) then begin
    Value.MakeVisible;
    ScrolledTop:=Value.Top+Value.Height-(ClientHeight-ScrollBarWidth);
    //TreeView_SelectSetFirstVisible(Handle, Value.ItemId);
  end;
end;

procedure TCustomTreeView.SetShowButton(Value: Boolean);
begin
  if ShowButtons <> Value then begin
    if Value then
      Include(FOptions,tvoShowButtons)
    else
      Exclude(FOptions,tvoShowButtons);
    Invalidate;
    //SetComCtlStyle(Self, TVS_HASBUTTONS, Value);
  end;
end;

procedure TCustomTreeView.SetShowLines(Value: Boolean);
begin
  if ShowLines <> Value then begin
    if Value then
      Include(FOptions,tvoShowLines)
    else
      Exclude(FOptions,tvoShowLines);
    Invalidate;
    //SetComCtlStyle(Self, TVS_HASLINES, Value);
  end;
end;

procedure TCustomTreeView.SetShowRoot(Value: Boolean);
begin
  if ShowRoot <> Value then begin
    if Value then
      Include(FOptions,tvoShowRoot)
    else
      Exclude(FOptions,tvoShowRoot);
    Invalidate;
    //SetComCtlStyle(Self, TVS_LINESATROOT, Value);
  end;
end;

procedure TCustomTreeView.SetShowSeparators(Value: Boolean);
begin
  if ShowSeparators <> Value then begin
    if Value then
      Include(FOptions,tvoShowSeparators)
    else
      Exclude(FOptions,tvoShowSeparators);
    Invalidate;
  end;
end;

procedure TCustomTreeView.SetKeepCollapsedNodes(Value: Boolean);
begin
  if KeepCollapsedNodes=Value then exit;
  if Value then
    Include(FOptions,tvoKeepCollapsedNodes)
  else
    Exclude(FOptions,tvoKeepCollapsedNodes);
  Items.KeepCollapsedNodes:=Value;
end;

procedure TCustomTreeView.SetReadOnly(Value: Boolean);
begin
  if ReadOnly <> Value then begin
    if Value then
      Include(FOptions,tvoRightClickSelect)
    else
      Exclude(FOptions,tvoRightClickSelect);
    if not Value then EndEditing;
    //SetComCtlStyle(Self, TVS_EDITLABELS, not Value);
  end;
end;

procedure TCustomTreeView.SetRightClickSelect(Value: Boolean);
begin
  if Value then
    Include(FOptions,tvoRightClickSelect)
  else
    Exclude(FOptions,tvoRightClickSelect);
end;

procedure TCustomTreeView.SetHideSelection(Value: Boolean);
begin
  if HideSelection <> Value then begin
    if Value then
      Include(FOptions,tvoHideSelection)
    else
      Exclude(FOptions,tvoHideSelection);
    //SetComCtlStyle(Self, TVS_SHOWSELALWAYS, not Value);
    if FSelectedNode<>nil then Invalidate;
  end;
end;

function TCustomTreeView.GetMaxLvl: integer;
begin
  UpdateMaxRight;
  Result:=FMaxRight;
end;

function TCustomTreeView.GetMaxScrollLeft: integer;
begin
  UpdateMaxRight;
  Result:=FMaxRight-(ClientWidth-ScrollBarWidth-2*BorderWidth);
  if Result<0 then Result:=0;
end;

function TCustomTreeView.GetMaxScrollTop: integer;
var LastVisibleNode: TTreeNode;
begin
  LastVisibleNode:=Items.GetLastExpandedSubNode;
  if LastVisibleNode=nil then
    Result:=0
  else begin
    Result:=LastVisibleNode.Top+LastVisibleNode.Height
             -(ClientHeight-ScrollBarWidth)+2*BorderWidth;
//writeln('>>> ',LastVisibleNode.Text,' ',Result);
    if Result<0 then Result:=0;
  end;
end;

function TCustomTreeView.GetNodeAtInternalY(Y: Integer): TTreeNode;
// search in all expanded nodes for the node at the absolute coordinate Y
var i: integer;
begin
  i:=IndexOfNodeAtTop(Items.FTopLvlItems,Items.FTopLvlCount,Y);
  if i>=0 then begin
    Result:=Items.FTopLvlItems[i];
    while Result.Expanded do begin
      i:=IndexOfNodeAtTop(Result.FItems,Result.FCount,Y);
      if i>=0 then
        Result:=Result.Items[i]
      else
        break;
    end;
  end else
    Result:=nil;
end;

function TCustomTreeView.GetNodeAtY(Y: Integer): TTreeNode;
// search in all expanded nodes for the node at the screen coordinate Y
begin
  Result:=nil;
  if (Y>=BorderWidth) and (Y<(ClientHeight-ScrollBarWidth)-BorderWidth) then
  begin
    inc(Y,FScrolledTop-BorderWidth);
    Result:=GetNodeAtInternalY(Y);
  end;
end;

function TCustomTreeView.GetNodeDrawAreaWidth: integer;
begin
  Result:=ClientWidth-ScrollBarWidth-BorderWidth*2;
end;

function TCustomTreeView.GetNodeDrawAreaHeight: integer;
begin
  Result:=ClientHeight-ScrollBarWidth-BorderWidth*2;
end;

function TCustomTreeView.GetNodeAt(X, Y: Integer): TTreeNode;
//var HitTest: TTVHitTestInfo;
begin
  Result:=nil;
  if (X>=BorderWidth) and (X<ClientWidth-BorderWidth) then begin
    Result:=GetNodeAtY(Y);
    if Result<>nil then begin
      inc(X,FScrolledLeft-BorderWidth);
      if (X<Result.DisplayExpandSignLeft) then
        Result:=nil;
    end;
  end;
  {with HitTest do begin
    pt.X := X;
    pt.Y := Y;
    if TreeView_HitTest(Handle, HitTest) <> nil then
      Result := Items.GetNode(HitTest.hItem)
    else
    Result := nil;
  end;}
end;

function TCustomTreeView.GetHitTestInfoAt(X, Y: Integer): THitTests;
//var HitTest: TTVHitTestInfo;
var Node: TTreeNode;
begin
  // ToDo
  Result := [];
  if (X>=0) and (X<ClientWidth) and (Y>=0) and (Y<(ClientHeight-ScrollBarWidth))
  then begin
    inc(Y,FScrolledTop);
    Node:=GetNodeAtY(Y);
    if Node<>nil then begin
      inc(X,FScrolledLeft);
      if X<Node.DisplayExpandSignLeft then
        Include(Result,htOnIndent)
      else if X<Node.DisplayIconLeft then
        Include(Result,htOnButton)
      else if X<Node.DisplayStateIconLeft then
        Include(Result,htOnItem)
      else if X<Node.DisplayTextLeft then
        Include(Result,htOnStateIcon)
      else if X<Node.DisplayTextRight then
        Include(Result,htOnLabel);
    end else
      Include(Result,htNowhere);
  end;
  {with HitTest do begin
    pt.X := X;
    pt.Y := Y;
    TreeView_HitTest(Handle, HitTest);
    if (flags and TVHT_ABOVE) <> 0 then Include(Result, htAbove);
    if (flags and TVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and TVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and TVHT_ONITEM) = TVHT_ONITEM then
      Include(Result, htOnItem)
    else
    begin
      if (flags and TVHT_ONITEM) <> 0 then Include(Result, htOnItem);
      if (flags and TVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
      if (flags and TVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
      if (flags and TVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    end;
    if (flags and TVHT_ONITEMBUTTON) <> 0 then Include(Result, htOnButton);
    if (flags and TVHT_ONITEMINDENT) <> 0 then Include(Result, htOnIndent);
    if (flags and TVHT_ONITEMRIGHT) <> 0 then Include(Result, htOnRight);
    if (flags and TVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and TVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;}
end;

procedure TCustomTreeView.SetTreeLineColor(Value: TColor);
begin
  if FTreeLineColor<>Value then begin
    FTreeLineColor:=Value;
    Invalidate;
  end;
end;

procedure TCustomTreeView.SetTreeNodes(Value: TTreeNodes);
begin
  Items.Assign(Value);
end;

procedure TCustomTreeView.SetIndent(Value: Integer);
begin
  if Value <> Indent then begin
    FIndent := Value;
    Invalidate;
    //TreeView_SetIndent(Handle, Value);
  end;
end;

procedure TCustomTreeView.FullExpand;
var
  Node: TTreeNode;
begin
  Node := Items.GetFirstNode;
  while Node <> nil do begin
    Node.Expand(True);
    Node := Node.GetNextSibling;
  end;
end;

procedure TCustomTreeView.FullCollapse;
var
  Node: TTreeNode;
begin
  Node := Items.GetFirstNode;
  while Node <> nil do begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;
end;

function TCustomTreeView.IsNodeVisible(ANode: TTreeNode): Boolean;
begin
  Result:=(ANode<>nil) and (ANode.AreParentsExpanded);
//writeln('[TCustomTreeView.IsNodeVisible] A Node=',HexStr(Cardinal(ANode),8),
//' ANode.AreParentsExpanded=',ANode.AreParentsExpanded);
  if Result then begin
//writeln('[TCustomTreeView.IsNodeVisible] B Node=',HexStr(Cardinal(ANode),8),
//' ',FScrolledTop,'>=',ANode.Top,'+',ANode.Height,' or ',FScrolledTop,'+',ClientHeight,'<',ANode.Top);
    if (FScrolledTop>=ANode.Top+ANode.Height)
    or (FScrolledTop+(ClientHeight-ScrollBarWidth)-2*BorderWidth<ANode.Top)
    then
      Result:=false;
  end;
//writeln('[TCustomTreeView.IsNodeVisible] END Node=',HexStr(Cardinal(ANode),8),
//' Node.Text=',ANode.Text,' Visible=',Result);
end;

procedure TCustomTreeView.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then FullExpand;
end;

function TCustomTreeView.GetTopItem: TTreeNode;
begin
  if HandleAllocated then begin
    UpdateTopItem;
    Result := FTopItem;
  end else
    Result := nil;
end;

procedure TCustomTreeView.SetTopItem(Value: TTreeNode);
begin
  if HandleAllocated and (Value <> nil) then begin
    Value.MakeVisible;
    ScrolledTop:=Value.Top;
    //TreeView_SelectSetFirstVisible(Handle, Value.ItemId);
  end;
end;

procedure TCustomTreeView.OnChangeTimer(Sender: TObject);
begin
  FChangeTimer.Enabled := False;
  Change(TTreeNode(FChangeTimer.Tag));
end;

procedure TCustomTreeView.UpdateScrollbars;

  function Max(i1, i2: integer): integer;
  begin
    if i1>i2 then
      Result:=i1
    else
      Result:=i2;
  end;

var
  ScrollInfo: TScrollInfo;
begin
  if not (tvsScrollbarChanged in FStates) then exit;
  if not HandleAllocated or (FUpdateCount>0) then begin
    //Include(FStates,tvsScrollbarChanged);
  end else begin
    if ScrolledLeft>GetMaxScrollLeft then ScrolledLeft:=GetMaxScrollLeft;
    if ScrolledTop>GetMaxScrollTop then ScrolledTop:=GetMaxScrollTop;
    Exclude(FStates,tvsScrollbarChanged);
    if fScrollBars <> ssNone then begin
      if fScrollBars in [ssBoth, ssHorizontal] then begin
        // horizontal scrollbar
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
        ScrollInfo.nTrackPos := 0;
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := Max(1,(ClientWidth-ScrollBarWidth)-2*BorderWidth);
        ScrollInfo.nMax := Max(1,GetMaxScrollLeft+ScrollInfo.nPage);
        ScrollInfo.nPos := Max(FScrolledLeft,0);
        if not CompareMem(@ScrollInfo,@FLastHorzScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastHorzScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
          ShowScrollBar(Handle,SB_HORZ,True);
        end;
        //writeln('>>>>>>>>>> [TCustomTreeView.UpdateScrollbars] nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,' GetMaxScrollLeft=',GetMaxScrollLeft,
        //' ClientW=',ClientWidth,
        //' MaxRight=',FMaxRight
        //);
      end else begin
      
        // ToDo: tell interface to remove horizontal scrollbar
      
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        // vertical scrollbar
        ScrollInfo.cbSize := SizeOf(ScrollInfo);
        ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
        ScrollInfo.nTrackPos := 0;
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := Max(1,(ClientHeight-ScrollBarWidth)-FDefItemHeight);
        ScrollInfo.nMax := Max(1,GetMaxScrollTop+ScrollInfo.nPage);
        ScrollInfo.nTrackPos := 0;
        ScrollInfo.nPos := Max(0,FScrolledTop);
        if not CompareMem(@ScrollInfo,@FLastVertScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastVertScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
          ShowScrollBar(Handle,SB_VERT,True);
        end;
        //writeln('>>>>>>>>>> [TCustomTreeView.UpdateScrollbars] nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,' GetMaxScrollTop=',GetMaxScrollTop);
      end else begin

        // ToDo: tell interface to remove vertical scrollbar

      end;
    end;
  end;
end;

function TCustomTreeView.GetSelection: TTreeNode;
begin
  if HandleAllocated then
  begin
    if RightClickSelect and Assigned(FRClickNode) then
      Result := FRClickNode
    else
      Result := FSelectedNode;
  end
  else Result := nil;
end;

procedure TCustomTreeView.SetSelection(Value: TTreeNode);
begin
  if FSelectedNode=Value then exit;
  FSelectedNode:=Value;
  if Value <> nil then begin
    Value.Selected := True;
    Value.MakeVisible;
  end;
  Invalidate;
end;

function TCustomTreeView.GetShowButtons: boolean;
begin
  Result:=(tvoShowButtons in FOptions);
end;

function TCustomTreeView.GetShowLines: boolean;
begin
  Result:=(tvoShowLines in FOptions);
end;

function TCustomTreeView.GetShowRoot: boolean;
begin
  Result:=(tvoShowRoot in FOptions);
end;

function TCustomTreeView.GetShowSeparators: boolean;
begin
  Result:=(tvoShowSeparators in FOptions);
end;

function TCustomTreeView.GetToolTips: boolean;
begin
  Result:=(tvoToolTips in FOptions);
end;

procedure TCustomTreeView.SetExpandSignType(Value: TTreeViewExpandSignType);
begin
  if Value=FExpandSignType then exit;
  FExpandSignType:=Value;
  Invalidate;
end;

procedure TCustomTreeView.SetChangeDelay(Value: Integer);
begin
  FChangeTimer.Interval := Value;
end;

procedure TCustomTreeView.SetDefaultItemHeight(Value: integer);
begin
  if Value<=0 then Value:=20;
  if Value=FDefItemHeight then exit;
  FDefItemHeight:=Value;
  Include(FStates,tvsTopsNeedsUpdate);
  Invalidate;
end;

function TCustomTreeView.GetAutoExpand: boolean;
begin
  Result:=(tvoAutoExpand in FOptions);
end;

function TCustomTreeView.GetBottomItem: TTreeNode;
begin
  if HandleAllocated then begin
    UpdateBottomItem;
    Result := FBottomItem;
  end else
    Result := nil;
end;

function TCustomTreeView.GetChangeDelay: Integer;
begin
  Result := FChangeTimer.Interval;
end;

function TCustomTreeView.GetDropTarget: TTreeNode;
begin
  if HandleAllocated then
  begin
    //Result := Items.GetNode(TreeView_GetDropHilite(Handle));
    //if Result = nil then Result := FLastDropTarget;
    Result := FLastDropTarget;
  end
  else
    Result := nil;
end;

function TCustomTreeView.GetHideSelection: boolean;
begin
  Result:=(tvoHideSelection in FOptions);
end;

function TCustomTreeView.GetHotTrack: boolean;
begin
  Result:=(tvoHotTrack in FOptions);
end;

function TCustomTreeView.GetKeepCollapsedNodes: boolean;
begin
  Result:=(tvoKeepCollapsedNodes in FOptions);
end;

function TCustomTreeView.GetReadOnly: boolean;
begin
  Result:=(tvoReadOnly in FOptions);
end;

function TCustomTreeView.GetRightClickSelect: boolean;
begin
  Result:=(tvoRightClickSelect in FOptions);
end;

function TCustomTreeView.GetRowSelect: boolean;
begin
  Result:=(tvoRowSelect in FOptions);
end;

procedure TCustomTreeView.SetDropTarget(Value: TTreeNode);
begin
  if HandleAllocated then
    if Value <> nil then
      Value.DropTarget := True;
    {else
      TreeView_SelectDropTarget(Handle, nil);}
end;

{function TCustomTreeView.GetNodeFromItem(const Item: TTVItem): TTreeNode;
begin
  Result := nil;
  if Items <> nil then
    with Item do
      if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
      else Result := Items.GetNode(hItem);
end;
}
function TCustomTreeView.IsEditing: Boolean;
//var ControlHand: HWnd;
begin
  Result:=tvsIsEditing in FStates;
  //ControlHand := TreeView_GetEditControl(Handle);
  //Result := (ControlHand <> 0) and IsWindowVisible(ControlHand);
end;

{procedure TCustomTreeView.CNNotify(var Message: TWMNotify);
var
  Node: TTreeNode;
  MousePos: TPoint;
  R: TRect;
  DefaultDraw, PaintImages: Boolean;
  TmpItem: TTVItem;
  LogFont: TLogFont;
begin
  with Message do
    case NMHdr^.code of
      NM_CUSTOMDRAW:
        with PNMCustomDraw(NMHdr)^ do
        begin
          FCanvas.Lock;
          try
            Result := CDRF_DODEFAULT;
            if (dwDrawStage and CDDS_ITEM) = 0 then
            begin
              R := ClientRect;
              case dwDrawStage of
                CDDS_PREPAINT:
                begin
                  if IsCustomDrawn(dtControl, cdPrePaint) then
                  begin
                    try
                      FCanvas.Handle := hdc;
                      FCanvas.Font := Font;
                      FCanvas.Brush := Brush;
                      DefaultDraw := CustomDraw(R, cdPrePaint);
                    finally
                      FCanvas.Handle := 0;
                    end;
                    if not DefaultDraw then
                    begin
                      Result := CDRF_SKIPDEFAULT;
                      Exit;
                    end;
                  end;
                  if IsCustomDrawn(dtItem, cdPrePaint) or IsCustomDrawn(dtItem, cdPreErase) then
                    Result := Result or CDRF_NOTIFYITEMDRAW;
                  if IsCustomDrawn(dtItem, cdPostPaint) then
                    Result := Result or CDRF_NOTIFYPOSTPAINT;
                  if IsCustomDrawn(dtItem, cdPostErase) then
                    Result := Result or CDRF_NOTIFYPOSTERASE;
                end;
                CDDS_POSTPAINT:
                  if IsCustomDrawn(dtControl, cdPostPaint) then
                    CustomDraw(R, cdPostPaint);
                CDDS_PREERASE:
                  if IsCustomDrawn(dtControl, cdPreErase) then
                    CustomDraw(R, cdPreErase);
                CDDS_POSTERASE:
                  if IsCustomDrawn(dtControl, cdPostErase) then
                    CustomDraw(R, cdPostErase);
              end;
            end else
            begin
              FillChar(TmpItem, SizeOf(TmpItem), 0);
              TmpItem.hItem := HTREEITEM(dwItemSpec);
              Node := GetNodeFromItem(TmpItem);
              if Node = nil then Exit;
              case dwDrawStage of
                CDDS_ITEMPREPAINT:
                  try
                    FCanvas.Handle := hdc;
                    FCanvas.Font := Font;
                    FCanvas.Brush := Brush;
                    // Unlike the list view, the tree view doesn't override the text
                    //  foreground and background colors of selected items.
                    if uItemState and CDIS_SELECTED <> 0 then
                    begin
                      FCanvas.Font.Color := clHighlightText;
                      FCanvas.Brush.Color := clHighlight;
                    end;
                    FCanvas.Font.OnChange := CanvasChanged;
                    FCanvas.Brush.OnChange := CanvasChanged;
                    FCanvasChanged := False;
                    DefaultDraw := CustomDrawItem(Node,
                      TCustomDrawState(Word(uItemState)), cdPrePaint, PaintImages);
                    if not PaintImages then
                      Result := Result or TVCDRF_NOIMAGES;
                    if not DefaultDraw then
                      Result := Result or CDRF_SKIPDEFAULT
                    else if FCanvasChanged then
                    begin
                      FCanvasChanged := False;
                      FCanvas.Font.OnChange := nil;
                      FCanvas.Brush.OnChange := nil;
                      with PNMTVCustomDraw(NMHdr)^ do
                      begin
                        clrText := ColorToRGB(FCanvas.Font.Color);
                        clrTextBk := ColorToRGB(FCanvas.Brush.Color);
                        if GetObject(FCanvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
                        begin
                          FCanvas.Handle := 0;  // disconnect from hdc
                          // don't delete the stock font
                          SelectObject(hdc, CreateFontIndirect(LogFont));
                          Result := Result or CDRF_NEWFONT;
                        end;
                      end;
                    end;
                    if IsCustomDrawn(dtItem, cdPostPaint) then
                      Result := Result or CDRF_NOTIFYPOSTPAINT;
                  finally
                    FCanvas.Handle := 0;
                  end;
                CDDS_ITEMPOSTPAINT:
                    if IsCustomDrawn(dtItem, cdPostPaint) then
                      CustomDrawItem(Node, TCustomDrawState(Word(uItemState)), cdPostPaint, PaintImages);
                CDDS_ITEMPREERASE:
                    if IsCustomDrawn(dtItem, cdPreErase) then
                      CustomDrawItem(Node, TCustomDrawState(Word(uItemState)), cdPreErase, PaintImages);
                CDDS_ITEMPOSTERASE:
                    if IsCustomDrawn(dtItem, cdPostErase) then
                      CustomDrawItem(Node, TCustomDrawState(Word(uItemState)), cdPostErase, PaintImages);
              end;
            end;
          finally
            FCanvas.Unlock;
          end;
        end;
      TVN_BEGINDRAG:
        begin
          FDragged := True;
          with PNMTreeView(NMHdr)^ do
            FDragNode := GetNodeFromItem(ItemNew);
        end;
      TVN_BEGINLABELEDIT:
        begin
          with PTVDispInfo(NMHdr)^ do
            if Dragging or not CanEdit(GetNodeFromItem(item)) then
              Result := 1;
          if Result = 0 then
          begin
            FEditHandle := TreeView_GetEditControl(Handle);
            FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
            SetWindowLong(FEditHandle, GWL_WNDPROC, LongInt(FEditInstance));
          end;
        end;
      TVN_ENDLABELEDIT: Edit(PTVDispInfo(NMHdr)^.item);
      TVN_ITEMEXPANDING:
        if not FManualNotify then
        begin
          with PNMTreeView(NMHdr)^ do
          begin
            Node := GetNodeFromItem(ItemNew);
            if (action = TVE_EXPAND) and not CanExpand(Node) then
              Result := 1
            else if (action = TVE_COLLAPSE) and
              not CanCollapse(Node) then Result := 1;
          end;
        end;
      TVN_ITEMEXPANDED:
        if not FManualNotify then
        begin
          with PNMTreeView(NMHdr)^ do
          begin
            Node := GetNodeFromItem(itemNew);
            if (action = TVE_EXPAND) then Expand(Node)
            else if (action = TVE_COLLAPSE) then Collapse(Node);
          end;
        end;
      TVN_SELCHANGINGA, TVN_SELCHANGINGW:
        if not CanChange(GetNodeFromItem(PNMTreeView(NMHdr)^.itemNew)) then
          Result := 1;
      TVN_SELCHANGEDA, TVN_SELCHANGEDW:
        with PNMTreeView(NMHdr)^ do
          if FChangeTimer.Interval > 0 then
          with FChangeTimer do
          begin
            Enabled := False;
            Tag := Integer(GetNodeFromItem(itemNew));
            Enabled := True;
          end
          else
            Change(GetNodeFromItem(itemNew));
      TVN_DELETEITEM:
        begin
          Node := GetNodeFromItem(PNMTreeView(NMHdr)^.itemOld);
          if Node <> nil then
          begin
            Node.FItemId := nil;
            FChangeTimer.Enabled := False;
            if FStateChanging then Node.Delete
            else Items.Delete(Node);
          end;
        end;
      TVN_SETDISPINFO:
        with PTVDispInfo(NMHdr)^ do
        begin
          Node := GetNodeFromItem(item);
          if (Node <> nil) and ((item.mask and TVIF_TEXT) <> 0) then
            Node.Text := item.pszText;
        end;
      TVN_GETDISPINFO:
        with PTVDispInfo(NMHdr)^ do
        begin
          Node := GetNodeFromItem(item);
          if Node <> nil then
          begin
            if (item.mask and TVIF_TEXT) <> 0 then
              StrLCopy(item.pszText, PChar(Node.Text), item.cchTextMax);
            if (item.mask and TVIF_IMAGE) <> 0 then
            begin
              GetImageIndex(Node);
              item.iImage := Node.ImageIndex;
            end;
            if (item.mask and TVIF_SELECTEDIMAGE) <> 0 then
            begin
              GetSelectedIndex(Node);
              item.iSelectedImage := Node.SelectedIndex;
            end;
          end;
        end;
      NM_RCLICK:
        begin
          FRClickNode := nil;
          GetCursorPos(MousePos);
          if RightClickSelect then
            with PointToSmallPoint(ScreenToClient(MousePos)) do
            begin
              FRClickNode := GetNodeAt(X, Y);
              Perform(WM_CONTEXTMENU, Handle, Integer(PointToSmallPoint(MousePos)));
              FRClickNode := nil;
            end
          else
            // Win95/98 eat WM_CONTEXTMENU when posted to the message queue
            PostMessage(Handle, CN_BASE+WM_CONTEXTMENU, Handle, Integer(PointToSmallPoint(MousePos)));
          Message.Result := 1;  // tell treeview not to perform default response
        end;
    end;
end;}

function TCustomTreeView.GetDragImages: TDragImageList;
begin
  if FDragImage.Count > 0 then
    Result := FDragImage
  else
    Result := nil;
end;

procedure TCustomTreeView.WndProc(var Message: TLMessage);
begin
  if not (csDesigning in ComponentState)
  and ((Message.Msg = LM_LBUTTONDOWN)
    or (Message.Msg = LM_LBUTTONDBLCLK))
  and not Dragging and
    (DragMode = dmAutomatic) and (DragKind = dkDrag) then
  begin
    if not IsControlMouseMsg(TLMMouse(Message)) then begin
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);
    end;
  end
  {else if Message.Msg = CN_BASE+LM_CONTEXTMENU then
    Message.Result := Perform(LM_CONTEXTMENU, Message.WParam, Message.LParam)
  }
  else
    inherited WndProc(Message);
end;

procedure TCustomTreeView.DoStartDrag(var DragObject: TDragObject);
{var
  ImageHandle: HImageList;
  DragNode: TTreeNode;
  P: TPoint;}
begin
  inherited DoStartDrag(DragObject);
  {DragNode := FDragNode;
  FLastDropTarget := nil;
  FDragNode := nil;
  if DragNode = nil then begin
    GetCursorPos(P);
    with ScreenToClient(P) do DragNode := GetNodeAt(X, Y);
  end;
  if DragNode <> nil then begin
    // ToDo: implement Drag&Drop
    ImageHandle := 0; TreeView_CreateDragImage(Handle, DragNode.ItemId);
    if ImageHandle <> 0 then
      with FDragImage do
      begin
        Handle := ImageHandle;
        SetDragImage(0, 2, 2);
      end;
  end;}
end;

procedure TCustomTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
  FLastDropTarget := nil;
end;

procedure TCustomTreeView.CMDrag(var AMessage: TCMDrag);
begin
  inherited CMDrag(AMessage);
writeln('TCustomTreeView.CMDrag ',ord(AMessage.DragMessage));
  with AMessage, DragRec^ do
    case DragMessage of
      dmDragMove:
        with ScreenToClient(Pos) do
          DoDragOver(Source, X, Y, AMessage.Result <> 0);
      dmDragLeave:
        begin
          TDragObject(Source).HideDragImage;
          FLastDropTarget := DropTarget;
          DropTarget := nil;
          TDragObject(Source).ShowDragImage;
        end;
      dmDragDrop: FLastDropTarget := nil;
    end;
end;

procedure TCustomTreeView.DoDragOver(Source: TDragObject; X, Y: Integer;
  CanDrop: Boolean);
var
  Node: TTreeNode;
begin
  Node := GetNodeAt(X, Y);
writeln('TCustomTreeView.DoDragOver ',Node<>nil,' ',Node <> DropTarget,' ',Node = FLastDropTarget);
  if (Node <> nil)
  and ((Node <> DropTarget) or (Node = FLastDropTarget)) then
  begin
    FLastDropTarget := nil;
    TDragObject(Source).HideDragImage;
    Node.DropTarget := True;
    TDragObject(Source).ShowDragImage;
  end;
end;

procedure TCustomTreeView.DoPaint;
var
  a,HalfBorderWidth:integer;
  SpaceRect, DrawRect: TRect;
  Node: TTreeNode;
begin
  if tvsUpdating in FStates then exit;
  UpdateScrollbars;
  with Canvas do begin
    if Assigned(FOnCustomDraw) then begin
      DrawRect:=ClientRect;
      if not CustomDraw(DrawRect,cdPrePaint) then exit;
    end;
    // draw nodes
    Node:=TopItem;
    //write('[TCustomTreeView.DoPaint] A Node=',HexStr(Cardinal(Node),8));
    //if Node<>nil then writeln(' Node.Text=',Node.Text) else writeln('');
    while Node<>nil do begin
      DoPaintNode(Node);
      Node:=Node.GetNextVisible;
      //write('[TCustomTreeView.DoPaint] B Node=',HexStr(Cardinal(Node),8));
      //if Node<>nil then writeln(' Node.Text=',Node.Text) else writeln('');
    end;
    // draw unused space below nodes
    SpaceRect:=Rect(BorderWidth,BorderWidth,
                    (ClientWidth-ScrollBarWidth)-BorderWidth,
                    (ClientHeight-ScrollBarWidth)-BorderWidth);
    Node:=BottomItem;
    if Node<>nil then
      SpaceRect.Top:=Node.Top+Node.Height-FScrolledTop+BorderWidth;
    //if Node<>nil then writeln('BottomItem=',BottomItem.text) else writeln('NO BOTTOMITEM!!!!!!!!!');
    // TWinControl(Parent).InvalidateRect(Self,SpaceRect,true);
    if (FBackgroundColor<>clNone) and (SpaceRect.Top<SpaceRect.Bottom) then
    begin
      //writeln('  SpaceRect=',SpaceRect.Left,',',SpaceRect.Top,',',SpaceRect.Right,',',SpaceRect.Bottom);
      Brush.Color:=FBackgroundColor;
      FillRect(SpaceRect);
    end;
    // draw border
    HalfBorderWidth:=BorderWidth shr 1;
    Pen.Color:=clGray;
    for a:=0 to BorderWidth-1 do begin
      if a=HalfBorderWidth then
        Pen.Color:=clBlack;
      MoveTo(a,(ClientHeight-ScrollBarWidth)-1-a);
      LineTo(a,a);
      LineTo((ClientWidth-ScrollBarWidth)-1-a,a);
    end;
    Pen.Color:=clWhite;
    for a:=0 to BorderWidth-1 do begin
      if a=HalfBorderWidth then
        Pen.Color:=clLtGray;
      MoveTo((ClientWidth-ScrollBarWidth)-1-a,a);
      LineTo((ClientWidth-ScrollBarWidth)-1-a,(ClientHeight-ScrollBarWidth)-1-a);
      LineTo(a,(ClientHeight-ScrollBarWidth)-1-a);
    end;
    if Assigned(FOnCustomDraw) then begin
      DrawRect:=ClientRect;
      if not CustomDraw(DrawRect,cdPostPaint) then exit;
    end;
  end;
end;

procedure TCustomTreeView.DoPaintNode(Node: TTreeNode);
var
  NodeRect: TRect;
  VertMid: integer;
  NodeSelected: boolean;

  function InvertColor(AColor: TColor): TColor;
  var Red, Green, Blue: integer;
  begin
    Result:=clWhite;
    Red:=(AColor shr 16) and $ff;
    Green:=(AColor shr 8) and $ff;
    Blue:=AColor and $ff;
    if Red+Green+Blue>$180 then
      Result:=clBlack;
    //writeln('[TCustomTreeView.DoPaintNode.InvertColor] Result=',Result,' ',Red,',',Green,',',Blue);
  end;

  function DrawTreeLines(CurNode: TTreeNode): integer;
  // paints tree lines, returns indent
  var CurMid: integer;
  begin
    if CurNode<>nil then begin
      Result:=DrawTreeLines(CurNode.Parent);
      if ShowLines then begin
        CurMid:=Result+(Indent shr 1);
        if CurNode=Node then begin
          // draw horizontal line
          Canvas.MoveTo(CurMid,VertMid);
          Canvas.LineTo(Result+Indent,VertMid);
        end;
        if CurNode.GetNextSibling<>nil then begin
          // draw vertical line to next brother
          Canvas.MoveTo(CurMid,NodeRect.Top);
          Canvas.LineTo(CurMid,NodeRect.Bottom);
        end else if CurNode=Node then begin
          // draw vertical line from top to horizontal line
          Canvas.MoveTo(CurMid,NodeRect.Top);
          Canvas.LineTo(CurMid,VertMid);
        end;
      end;
      inc(Result,Indent);
    end else begin
      Result:=BorderWidth-FScrolledLeft;
    end;
  end;

  procedure DrawExpandSign(MidX,MidY: integer; CollapseSign: boolean);
  var HalfSize, ALeft, ATop, ARight, ABottom: integer;
    Points: PPoint;
  begin
    if not ShowButtons then exit;
    with Canvas do begin
      Brush.Color:=BackgroundColor;
      Pen.Color:=TreeLineColor;
      Pen.Style:=psSolid;
      HalfSize:=fExpandSignSize shr 1;
      if ((FExpandSignSize and 1)=0) then dec(HalfSize);
      ALeft:=MidX-HalfSize;
      ATop:=MidY-HalfSize;
      ARight:=ALeft+(HalfSize shl 1);
      ABottom:=ATop+(HalfSize shl 1);
      case ExpandSignType of
      tvestPlusMinus:
        begin
          // draw a plus or a minus sign
          Rectangle(ALeft, ATop, ARight, ABottom);
          MoveTo(ALeft+2,MidY);
          LineTo(ARight-2+1,MidY);
          if not CollapseSign then begin
            MoveTo(MidX,ATop+2);
            LineTo(MidX,ABottom-2+1);
          end;
        end;
      tvestArrow:
        begin
          // draw an arrow. down for collapse and right for expand
          GetMem(Points,SizeOf(TPoint)*3);
          if CollapseSign then begin
            // draw an arrow down
            Points[0]:=Point(ALeft,MidY);
            Points[1]:=Point(ARight,MidY);
            Points[2]:=Point(MidX,ABottom);
          end else begin
            // draw an arrow right
            Points[0]:=Point(MidX-1,ATop);
            Points[1]:=Point(ARight-1,MidY);
            Points[2]:=Point(MidX-1,ABottom);
          end;
          Polygon(Points,3,false);
          FreeMem(Points);
        end;
      end;
    end;
  end;


var x, ImgIndex: integer;
  CurBackgroundColor, OldFontColor: TColor;
  CurTextRect: TRect;
  DrawState: TCustomDrawState;
  PaintImages: boolean;
begin
  NodeRect:=Node.DisplayRect(false);
  if (NodeRect.Bottom<0) or (NodeRect.Top>=(ClientHeight-ScrollBarWidth)) then
    exit;
  NodeSelected:=(Node.Selected) or (Node.MultiSelected);
  if Assigned(OnCustomDrawItem) then begin
    DrawState:=[];
    if NodeSelected then
      Include(DrawState,cdsSelected);
    if Node.Focused then
      Include(DrawState,cdsFocused);
    if Node.MultiSelected then
      Include(DrawState,cdsMarked);
    if not CustomDrawItem(Node,DrawState,cdPrePaint,PaintImages) then exit;
  end else begin
    PaintImages:=true;
  end;
  VertMid:=(NodeRect.Top+NodeRect.Bottom) shr 1;
  //writeln('[TCustomTreeView.DoPaintNode] Node=',HexStr(Cardinal(Node),8),' Node.Text=',Node.Text,' NodeRect=',NodeRect.Left,',',NodeRect.Top,',',NodeRect.Right,',',NodeRect.Bottom,' VertMid=',VertMid);
  with Canvas do begin
    // draw background
    if (tvoRowSelect in FOptions) and NodeSelected then
      CurBackgroundColor:=FSelectedColor
    else
      CurBackgroundColor:=FBackgroundColor;
    if CurBackgroundColor<>clNone then begin
      Brush.Color:=CurBackgroundColor;
      FillRect(NodeRect);
    end;
    // draw tree lines
    Pen.Color:=TreeLineColor;
    Pen.Style:=psDot;
    x:=DrawTreeLines(Node);
    Pen.Style:=psSolid;
    // draw expand sign
    if Node.HasChildren then begin
      DrawExpandSign(x-Indent+(Indent shr 1),VertMid,Node.Expanded);
    end;
    // draw icon
    if (Images<>nil) and PaintImages then begin
      if FSelectedNode<>Node then
        ImgIndex:=Node.ImageIndex
      else
        ImgIndex:=Node.SelectedIndex;
      if (ImgIndex>=0) and (ImgIndex<Images.Count) then
        Images.Draw(Canvas,x,NodeRect.Top,ImgIndex,true);
      inc(x,Images.Width);
    end;
    // draw state icon
    if (StateImages<>nil) and PaintImages then begin
      if (Node.StateIndex>=0) and (Node.StateIndex<StateImages.Count) then
        StateImages.Draw(Canvas,x,NodeRect.Top,Node.StateIndex,true);
      inc(x,StateImages.Width);
    end;
    // draw text
    if NodeSelected and (FSelectedColor<>clNone) then begin
      Brush.Color:=FSelectedColor;
      CurTextRect:=NodeRect;
      CurTextRect.Left:=x;
      CurTextRect.Right:=x+TextWidth(Node.Text);
      OldFontColor:=Font.Color;
      Font.Color:=InvertColor(Brush.Color);
      FillRect(CurTextRect);
      TextOut(x,NodeRect.Top+(TextHeight(Node.Text) shr 1),Node.Text);
      Font.Color:=OldFontColor;
    end else begin
      TextOut(x,NodeRect.Top+(TextHeight(Node.Text) shr 1),Node.Text);
    end;
    // draw separator
    if (tvoShowSeparators in FOptions) then begin
      Pen.Color:=SeparatorColor;
      MoveTo(NodeRect.Left,NodeRect.Bottom-1);
      LineTo(NodeRect.Right,NodeRect.Bottom-1);
    end;
  end;
  if Assigned(OnCustomDrawItem) then begin
    DrawState:=[];
    if Node.Selected then
      Include(DrawState,cdsSelected);
    if Node.Focused then
      Include(DrawState,cdsFocused);
    if Node.MultiSelected then
      Include(DrawState,cdsMarked);
    if not CustomDrawItem(Node,DrawState,cdPostPaint,PaintImages) then exit;
  end else begin
    PaintImages:=true;
  end;
end;

procedure TCustomTreeView.GetImageIndex(Node: TTreeNode);
begin
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, Node);
end;

procedure TCustomTreeView.GetSelectedIndex(Node: TTreeNode);
begin
  if Assigned(FOnGetSelectedIndex) then FOnGetSelectedIndex(Self, Node);
end;

function TCustomTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, Node, Result);
end;

procedure TCustomTreeView.Change(Node: TTreeNode);
begin
  if Assigned(FOnChange) then FOnChange(Self, Node);
end;

procedure TCustomTreeView.Delete(Node: TTreeNode);
begin
  if Assigned(FOnDeletion) then FOnDeletion(Self, Node);
end;

procedure TCustomTreeView.Expand(Node: TTreeNode);
begin
  if Assigned(FOnExpanded) then FOnExpanded(Self, Node);
end;

function TCustomTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnExpanding) then FOnExpanding(Self, Node, Result);
end;

procedure TCustomTreeView.Collapse(Node: TTreeNode);
begin
  if Assigned(FOnCollapsed) then FOnCollapsed(Self, Node);
end;

function TCustomTreeView.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnCollapsing) then FOnCollapsing(Self, Node, Result);
end;

function TCustomTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnEditing) then FOnEditing(Self, Node, Result);
end;

{procedure TCustomTreeView.Edit(const Item: TTVItem);
var
  S: string;
  Node: TTreeNode;
begin
  with Item do
    if pszText <> nil then begin
      S := pszText;
      Node := GetNodeFromItem(Item);
      if Assigned(FOnEdited) then FOnEdited(Self, Node, S);
      if Node <> nil then Node.Text := S;
    end;
end;}

procedure TCustomTreeView.EndEditing;
begin
  if not (tvsIsEditing in FStates) then exit;
  // ToDo:
  // restore value

  // end editing
  Exclude(FStates,tvsIsEditing);
  Invalidate;
end;

procedure TCustomTreeView.EnsureNodeIsVisible(ANode: TTreeNode);
var b: integer;
begin
  if ANode=nil then exit;
  ANode.ExpandParents;
  if ANode.Top<ScrolledTop then
    ScrolledTop:=ANode.Top
  else begin
    b:=ANode.Top+ANode.Height-GetNodeDrawAreaHeight;
    if ScrolledTop<b then ScrolledTop:=b;
  end;
end;

function TCustomTreeView.CreateNode: TTreeNode;
begin
  Result := TTreeNode.Create(Items);
end;

{procedure TCustomTreeView.SetImageList(Value: HImageList; Flags: Integer);
begin
  if HandleAllocated then TreeView_SetImageList(Handle, Value, Flags);
end;}

procedure TCustomTreeView.ImageListChange(Sender: TObject);
//var ImageHandle: HImageList;
begin
  // ToDo
  Invalidate;
  {Delphi:
  if HandleAllocated then
  begin
    if TCustomImageList(Sender).HandleAllocated then
      ImageHandle := TCustomImageList(Sender).Handle
    else
      ImageHandle := 0;
    if Sender = Images then
      SetImageList(ImageHandle, TVSIL_NORMAL)
    else if Sender = StateImages then
      SetImageList(ImageHandle, TVSIL_STATE);
  end;}
end;

procedure TCustomTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CursorNode: TTreeNode;
  bStartDrag: boolean;
begin
  fMouseDownX := X;
  fMouseDownY := Y;
  if Button=mbMiddle then begin
    if ([ssDouble,ssTriple,ssQuad]*Shift)<>[] then Exit;
    if tvsIsEditing in FStates then begin
      // ToDo: insert clipboard text into node text
      //  :=PrimarySelection.AsText;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
  CursorNode:=GetNodeAt(X,Y);
  bStartDrag := false;
  if ([ssDouble,ssTriple,ssQuad]*Shift)=[] then begin
    if (Button = mbLeft) and (CursorNode<>nil) then begin
      Exclude(fStates,tvsWaitForDragging);
      if CursorNode.HasChildren
      and (x>=CursorNode.DisplayExpandSignLeft)
      and (x<CursorNode.DisplayExpandSignRight) then begin
        // mousedown occured on expand sign -> expand/collapse
        CursorNode.Expanded:=not CursorNode.Expanded;
      end else if x>=CursorNode.DisplayTextLeft then begin
        // mousedown occured in text -> select node and begin drag operation
        Include(FStates,tvsMouseCapture);
        if not (tvoAllowMultiselect in Options) then begin
          Selected:=CursorNode;
        end else begin
          if (ssShift in Shift) then begin
            CursorNode.MultiSelectGroup;
          end else if (ssCtrl in Shift) then begin
            CursorNode.MultiSelected:=not CursorNode.MultiSelected;
          end else begin
            Items.ClearMultiSelection;
            CursorNode.MultiSelected:=true;
          end;
        end;
        bStartDrag := true;
      end;
    end;
    if (bStartDrag) then
      Include(fStates, tvsWaitForDragging);
    if Button=mbMiddle then begin
      // insert primary selection text

    end;
  end;
  //LCLLinux.SetFocus(Handle);
end;

procedure TCustomTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, x, y);
  if {MouseCapture and} (tvsWaitForDragging in fStates) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
    or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG))
    then begin
      Exclude(fStates, tvsWaitForDragging);
      BeginDrag(false);
    end;
  end;
end;

procedure TCustomTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  MouseCapture := False;
  Exclude(fStates, tvsWaitForDragging);
  if (Button=mbLeft)
  and (fStates * [tvsDblClicked, tvsTripleClicked, tvsQuadClicked,
    tvsWaitForDragging] = [])
  then begin
    //AquirePrimarySelection;
  end;
  fStates:=fStates-[tvsDblClicked,tvsTripleClicked,tvsQuadClicked];
end;

procedure TCustomTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = Images then Images := nil;
    if AComponent = StateImages then StateImages := nil;
  end;
end;

procedure TCustomTreeView.SetImages(Value: TCustomImageList);
begin
  if Images = Value then exit;
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    //SetImageList(Images.Handle, TVSIL_NORMAL)
  end;
  //else SetImageList(0, TVSIL_NORMAL);
  Invalidate;
end;

procedure TCustomTreeView.SetStateImages(Value: TCustomImageList);
begin
  if FStateImages=Value then exit;
  if StateImages <> nil then
    StateImages.UnRegisterChanges(FStateChangeLink);
  FStateImages := Value;
  if StateImages <> nil then begin
    StateImages.RegisterChanges(FStateChangeLink);
    StateImages.FreeNotification(Self);
    //SetImageList(StateImages.Handle, TVSIL_STATE)
  end;
  //else SetImageList(0, TVSIL_STATE);
  Invalidate;
end;

procedure TCustomTreeView.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomTreeView.LoadFromStream(Stream: TStream);
begin
  with TTreeStrings.Create(Items) do
    try
      LoadTreeFromStream(Stream);
    finally
      Free;
  end;
end;

procedure TCustomTreeView.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomTreeView.SaveToStream(Stream: TStream);
begin
  with TTreeStrings.Create(Items) do
    try
      SaveTreeToStream(Stream);
    finally
      Free;
  end;
end;

{procedure TCustomTreeView.WMContextMenu(var Message: TWMContextMenu);
var
  R: TRect;
begin
  if (Message.XPos < 0) and (Selected <> nil) then begin
    R := Selected.DisplayRect(True);
    Message.Pos := PointToSmallPoint(ClientToScreen(Point(R.Left, R.Bottom)));
  end;
  inherited;
end;}

procedure TCustomTreeView.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrolledTop := 0;
    SB_BOTTOM:     ScrolledTop := GetMaxScrollTop;
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefItemHeight div 2;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + (ClientHeight-ScrollBarWidth)
                                     - FDefItemHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - (ClientHeight-ScrollBarWidth)
                                     + FDefItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledTop := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

procedure TCustomTreeView.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_LEFT:       ScrolledLeft := 0;
    SB_RIGHT:      ScrolledLeft := GetMaxScrollLeft;
      // Scrolls one line left / right
    SB_LINERIGHT:  ScrolledLeft := ScrolledLeft + FDefItemHeight div 2;
    SB_LINELEFT:   ScrolledLeft := ScrolledLeft - FDefItemHeight div 2;
      // Scrolls one page of lines left / right
    SB_PAGERIGHT:  ScrolledLeft := ScrolledLeft + (ClientHeight-ScrollBarWidth)
                                       - FDefItemHeight;
    SB_PAGELEFT:   ScrolledLeft := ScrolledLeft - (ClientHeight-ScrollBarWidth)
                                       + FDefItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledLeft := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

procedure TCustomTreeView.WMLButtonDown(var AMessage: TLMLButtonDown);
var
  Node: TTreeNode;
  MousePos: TPoint;
begin
  Exclude(FStates,tvsDragged);
  FDragNode := nil;
  try
    inherited;
    if (DragMode = dmAutomatic) and (DragKind = dkDrag) then
    begin
      SetFocus;
      if not (tvsDragged in FStates) then begin
        GetCursorPos(MousePos);
        with PointToSmallPoint(ScreenToClient(MousePos)) do
          Perform(LM_LBUTTONUP, 0, MakeLong(X, Y));
      end
      else begin
        Node := GetNodeAt(AMessage.XPos, AMessage.YPos);
        if Node <> nil then
        begin
          Node.Focused := True;
          Node.Selected := True;
          BeginDrag(False);
        end;
      end;
    end;
  finally
    FDragNode := nil;
  end;
end;

procedure TCustomTreeView.WMNotify(var AMessage: TLMNotify);
{var
  Node: TTreeNode;
  MaxTextLen: Integer;
  Pt: TPoint;}
begin
  {with Message do
    if NMHdr^.code = TTN_NEEDTEXTW then
    begin
      // Work around NT COMCTL32 problem with tool tips >= 80 characters
      GetCursorPos(Pt);
      Pt := ScreenToClient(Pt);
      Node := GetNodeAt(Pt.X, Pt.Y);
      if (Node = nil) or (Node.Text = '') or
        (PToolTipTextW(NMHdr)^.uFlags and TTF_IDISHWND = 0) then Exit;
      if (GetComCtlVersion >= ComCtlVersionIE4) and (Length(Node.Text) < 80) then
      begin
        inherited;
        Exit;
      end;
      FWideText := Node.Text;
      MaxTextLen := SizeOf(PToolTipTextW(NMHdr)^.szText) div SizeOf(WideChar);
      if Length(FWideText) >= MaxTextLen then
        SetLength(FWideText, MaxTextLen - 1);
      PToolTipTextW(NMHdr)^.lpszText := PWideChar(FWideText);
      FillChar(PToolTipTextW(NMHdr)^.szText, MaxTextLen, 0);
      Move(Pointer(FWideText)^, PToolTipTextW(NMHdr)^.szText, Length(FWideText) * SizeOf(WideChar));
      PToolTipTextW(NMHdr)^.hInst := 0;
      SetWindowPos(NMHdr^.hwndFrom, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or
        SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);
      Result := 1;
    end
    else}
  inherited;
end;

procedure TCustomTreeView.WMSize(var Msg: TLMSize);
begin
  FStates:=FStates+[tvsScrollbarChanged,
                    tvsBottomItemNeedsUpdate];
  inherited;
end;

{ CustomDraw support }

procedure TCustomTreeView.CanvasChanged(Sender: TObject);
begin
  Include(FStates,tvsCanvasChanged);
end;

function TCustomTreeView.IsCustomDrawn(Target: TCustomDrawTarget;
  Stage: TCustomDrawStage): Boolean;
begin
  { Tree view doesn't support erase notifications }
  if Stage = cdPrePaint then begin
    if Target = dtItem then
      Result := Assigned(FOnCustomDrawItem)
                or Assigned(FOnAdvancedCustomDrawItem)
    else if Target = dtControl then
      Result := Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) or
              Assigned(FOnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem)
    else
      Result := False;
  end else begin
    if Target = dtItem then
      Result := Assigned(FOnAdvancedCustomDrawItem)
    else if Target = dtControl then
      Result := Assigned(FOnAdvancedCustomDraw)
                or Assigned(FOnAdvancedCustomDrawItem)
    else
      Result := False;
  end;
end;

function TCustomTreeView.CustomDraw(const ARect: TRect;
  Stage: TCustomDrawStage): Boolean;
begin
  Result := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, ARect, Result);
  if Assigned(FOnAdvancedCustomDraw) then
    FOnAdvancedCustomDraw(Self, ARect, Stage, Result);
end;

function TCustomTreeView.CustomDrawItem(Node: TTreeNode;
  State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
begin
  Result := True;
  PaintImages := True;
  if (Stage = cdPrePaint) and Assigned(FOnCustomDrawItem) then
    FOnCustomDrawItem(Self, Node, State, Result);
  if Assigned(FOnAdvancedCustomDrawItem) then
    FOnAdvancedCustomDrawItem(Self, Node, State, Stage, PaintImages, Result);
end;

function TCustomTreeView.ConsistencyCheck: integer;
var OldMaxRight, OldLastTop, OldMaxLvl: integer;
  OldTopItem, OldBottomItem: TTreeNode;
begin
  if FCanvas=nil then exit(-1);
  if (fExpandSignSize<0) then exit(-2);
  if FDefItemHeight<0 then exit(-3);
  if FIndent<0 then exit(-4);
  if FMaxRight<0 then exit(-5);
  if FTreeNodes=nil then exit(-6);
  Result:=FTreeNodes.ConsistencyCheck;
  if Result<>0 then begin
    dec(Result,1000);
    exit;
  end;
  if FUpdateCount<0 then exit(-7);
  if (not (tvsTopsNeedsUpdate in FStates)) then begin
    if Items.GetLastSubNode<>nil then begin
      OldLastTop:=Items.GetLastSubNode.Top;
      Include(FStates,tvsTopsNeedsUpdate);
      UpdateAllTops;
      if OldLastTop<>Items.GetLastSubNode.Top then exit(-8);
    end;
  end;
  if not (tvsMaxRightNeedsUpdate in FStates) then begin
    OldMaxRight:=FMaxRight;
    Include(FStates,tvsMaxRightNeedsUpdate);
    UpdateMaxRight;
    if OldMaxRight<>FMaxRight then exit(-9);
  end;
  if not (tvsMaxLvlNeedsUpdate in FStates) then begin
    OldMaxLvl:=FMaxLvl;
    Include(FStates,tvsMaxLvlNeedsUpdate);
    UpdateMaxLvl;
    if OldMaxLvl<>FMaxLvl then exit(-10);
  end;
  if (tvsIsEditing in FStates) and (FSelectedNode=nil) then exit(-11);
  if (FSelectedNode<>nil) then begin
    if not FSelectedNode.IsVisible then exit(-12);
  end;
  if not (tvsTopItemNeedsUpdate in FStates) then begin
    OldTopItem:=FTopItem;
    UpdateTopItem;
    if FTopItem<>OldTopItem then exit(-13);
  end;
  if not (tvsBottomItemNeedsUpdate in FStates) then begin
    OldBottomItem:=FBottomItem;
    UpdateBottomItem;
    if FBottomItem<>OldBottomItem then exit(-14);
  end;
  Result:=0;
end;

procedure TCustomTreeView.WriteDebugReport(const Prefix: string;
  AllNodes: boolean);
begin
  write(Prefix);
  write('TCustomTreeView.WriteDebugReport Self=',HexStr(Cardinal(Self),8));
  write(' Consistency=',ConsistencyCheck);
  writeln('');
  if AllNodes then begin
    Items.WriteDebugReport(Prefix+'  ',true);
  end;
end;

procedure TCustomTreeView.SetSeparatorColor(const AValue: TColor);
begin
  if fSeparatorColor=AValue then exit;
  fSeparatorColor:=AValue;
  if tvoShowSeparators in Options then
    Invalidate;
end;

// back to comctrls.pp

