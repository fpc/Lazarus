{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TTreeView
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
    - Columns
}


{ $DEFINE TREEVIEW_DEBUG}

{ TTreeViewAccessibleObject }

const
  TTreeNodeWithPointerStreamVersion : word = 1;
  TTreeNodeStreamVersion : word = 2;
  TVAutoHeightString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789|\()^';
  MinNodeCapacity = 10;

  // maximum scroll range
  //MAX_SCROLL = 32767;

function CompareExpandedNodes(Data1, Data2: Pointer): integer;
var
  Node1: TTreeNodeExpandedState;
  Node2: TTreeNodeExpandedState;
begin
  Node1:=TTreeNodeExpandedState(Data1);
  Node2:=TTreeNodeExpandedState(Data2);
  Result:=AnsiCompareText(Node1.NodeText,Node2.NodeText);
end;

function CompareTextWithExpandedNode(Key, Data: Pointer): integer;
var
  NodeText: String;
  Node: TTreeNodeExpandedState;
begin
  NodeText:=String(Key);
  Node:=TTreeNodeExpandedState(Data);
  Result:=AnsiCompareText(NodeText,Node.NodeText);
end;

procedure TreeViewError(const Msg: string);
begin
  raise ETreeViewError.Create(Msg);
end;

procedure TreeNodeError(const Msg: string);
begin
  raise ETreeNodeError.Create(Msg);
end;

procedure TreeNodeErrorFmt(const Msg: string; Format: array of const);
begin
  raise ETreeNodeError.CreateFmt(Msg, Format);
end;

function IndexOfNodeAtTop(NodeArray: TTreeNodeArray; Count, y: integer): integer;
// NodeArray must be sorted via Top
// returns index of Node with Node.Top <= y < Node[+1].Top
var
  l, m, r, VisibleCount: integer;
  VisibleNodesAlloc: Boolean;
  VisibleNodes: TTreeNodeArray;
begin
  if (Count = 0) or (NodeArray = nil) then
    Exit(-1);
  // Count the visible nodes
  VisibleCount := 0;
  VisibleNodesAlloc := False;
  for l := 0 to Count-1 do
    if NodeArray[l].Visible then
      Inc(VisibleCount);
  try
    // Make a temporary array of visible nodes if there are hidden nodes
    if VisibleCount < Count then begin
      GetMem(VisibleNodes,SizeOf(Pointer)*VisibleCount);
      m := 0;
      for l := 0 to Count-1 do
        if NodeArray[l].Visible then begin
          VisibleNodes[m] := NodeArray[l];
          Inc(m);
        end;
      Count := VisibleCount;
      VisibleNodesAlloc := True;
    end
    else
      VisibleNodes := NodeArray;
    // Binary search for the Y coordinate
    l := 0;
    r := Count - 1;
    while (l <= r) do
    begin
      m := (l + r) shr 1;
      //DebugLn(':0 [IndexOfNodeAtTop] m=',m,' y=',y,' ',NodeArray[m].Text,' NodeArray[m].Top=',NodeArray[m].Top,' NodeArray[m].BottomExpanded=',NodeArray[m].BottomExpanded);
      if VisibleNodes[m].Top > y then
        r := m - 1
      else if VisibleNodes[m].BottomExpanded <= y then
        l := m + 1
      else
        Exit(VisibleNodes[m].Index);
    end;
    Result := -1;
  finally
    if VisibleNodesAlloc then
      Freemem(VisibleNodes);
  end;
end;

// procedure for sorting a TTreeNodeArray
procedure Sort(Nodes: TTreeNodeArray; Count: integer;
  SortProc: TTreeNodeCompare; UpdateIndex: Boolean);
// Sorts the nodes using merge sort and updates the sibling links
var
  Buffer: TTreeNodeArray;
  i: Integer;

  procedure MergeNodeArrays(Pos1, Pos2, Pos3: integer);
  // merge two sorted arrays (result is in Src)
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
  begin
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=SortProc(Nodes[Src1Pos],Nodes[Src2Pos]);
      if cmp>0 then begin
        Buffer[DestPos]:=Nodes[Src1Pos];
        dec(Src1Pos);
      end else begin
        Buffer[DestPos]:=Nodes[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      Buffer[DestPos]:=Nodes[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      Nodes[a]:=Buffer[a];
  end;

  procedure MergeSort(StartPos, EndPos: integer);
  // sort Src from Position StartPos to EndPos (both included)
  var cmp,mid:integer;
  begin
    if StartPos>=EndPos then begin
      // sort one element -> very easy :)
    end else if StartPos+1=EndPos then begin
      // sort two elements -> quite easy :)
      cmp:=SortProc(Nodes[StartPos],Nodes[EndPos]);
      if cmp>0 then begin
        Buffer[StartPos]:=Nodes[StartPos];
        Nodes[StartPos]:=Nodes[EndPos];
        Nodes[EndPos]:=Buffer[StartPos];
      end;
    end else begin
      // sort more than two elements -> Mergesort
      mid:=(StartPos+EndPos) shr 1;
      MergeSort(StartPos,mid);
      MergeSort(mid+1,EndPos);
      MergeNodeArrays(StartPos,mid+1,EndPos);
    end;
  end;

begin
  if Count>0 then begin
    Buffer := GetMem(SizeOf(Pointer)*Count);
    MergeSort(0,Count-1);
    FreeMem(Buffer);
    // update sibling links
    Nodes[0].FPrevBrother := nil;
    Nodes[Count-1].FNextBrother := nil;
    if UpdateIndex then Nodes[0].FIndex:=0;
    for i:= 1 to Count-1 do begin
      Nodes[i].FPrevBrother := Nodes[i-1];
      Nodes[i-1].FNextBrother := Nodes[i];
      if UpdateIndex then Nodes[i].FIndex:=i;
    end;
  end;
end;

{ TTreeNodeExpandedState }

constructor TTreeNodeExpandedState.Create(FirstTreeNode: TTreeNode);
begin
  CreateChildNodes(FirstTreeNode);
end;

constructor TTreeNodeExpandedState.Create(TreeView: TCustomTreeView);
begin
  CreateChildNodes(TreeView.Items.GetFirstNode);
end;

destructor TTreeNodeExpandedState.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTreeNodeExpandedState.Clear;
begin
  if Children<>nil then begin
    Children.FreeAndClear;
    FreeThenNil(Children);
  end;
end;

procedure TTreeNodeExpandedState.CreateChildNodes(FirstTreeNode: TTreeNode);
var
  ChildNode: TTreeNode;
  NewExpandedNode: TTreeNodeExpandedState;
begin
  if (FirstTreeNode<>nil) and (FirstTreeNode.Parent<>nil) then
    NodeText:=FirstTreeNode.Parent.Text
  else
    NodeText:='';
  Clear;
  ChildNode:=FirstTreeNode;
  while ChildNode<>nil do begin
    if ChildNode.Expanded then begin
      if Children=nil then Children:=TAvgLvlTree.Create(@CompareExpandedNodes);
      NewExpandedNode:=TTreeNodeExpandedState.Create(ChildNode.GetFirstChild);
      Children.Add(NewExpandedNode);
    end;
    ChildNode:=ChildNode.GetNextSibling;
  end;
end;

procedure TTreeNodeExpandedState.Apply(FirstTreeNode: TTreeNode;
  CollapseToo: boolean);
var
  ChildNode: TTreeNode;
  ANode: TAvgLvlTreeNode;
  ChildNodeText: String;
begin
  if Children=nil then exit;
  ChildNode:=FirstTreeNode;
  while ChildNode<>nil do begin
    ChildNodeText:=ChildNode.Text;
    ANode:=Children.FindKey(PChar(ChildNodeText),@CompareTextWithExpandedNode);
    if ANode<>nil then
      ChildNode.Expanded:=true
    else if CollapseToo then
      ChildNode.Expanded:=false;
    if ANode<>nil then
      TTreeNodeExpandedState(ANode.Data).Apply(ChildNode.GetFirstChild,CollapseToo);
    ChildNode:=ChildNode.GetNextSibling;
  end;
end;

procedure TTreeNodeExpandedState.Apply(TreeView: TCustomTreeView;
  CollapseToo: boolean);
begin
  Apply(TreeView.Items.GetFirstNode,CollapseToo);
end;

{ TTreeNode }

function TTreeNode.DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
begin
  if (Node1.TreeView<>nil) and  Assigned(Node1.TreeView.OnCompare) then begin
    Result:=0;
    Node1.TreeView.OnCompare(Node1.TreeView,Node1, Node2, Result);
  end else
    Result := AnsiCompareStr(Node1.Text,Node2.Text);
end;

constructor TTreeNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create;
  FNodeEffect := gdeNormal;
  FImageIndex := -1;
  FOverlayIndex := -1;
  FSelectedIndex := -1;
  FStateIndex := -1;
  FStates := [];
  FOwner := AnOwner;
  FSubTreeCount := 1;
  FIndex := -1;
  FVisible := True;
  if Owner<>nil then inc(Owner.FCount);
end;

destructor TTreeNode.Destroy;
begin
  {$IFDEF TREEVIEW_DEBUG}
  DebugLn('[TTreeNode.Destroy] Self=',DbgS(Self),' Self.Text=',Text);
  {$ENDIF}
  Include(FStates,nsDeleting);

  // we must trigger TCustomTreeView.OnDeletion event before
  // unbinding.See issue #17832.
  if Assigned(Owner) and Assigned(Owner.Owner) then
    Owner.Owner.Delete(Self);

  // delete children
  HasChildren := false;
  // unbind all references
  Unbind;

  if Assigned(Owner) then begin
    dec(Owner.FCount);
    if FStates * [nsSelected, nsMultiSelected] <> [] then
      Owner.FSelection.Remove(Self);
  end;

  Data := nil;
  // free data
  if Assigned(FItems) then
  begin
    FreeMem(FItems);
    FItems := nil;
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

function TTreeNode.GetParentNodeOfAbsoluteLevel(
  TheAbsoluteLevel: integer): TTreeNode;
var
  i: integer;
  l: LongInt;
begin
  l:=Level;
  if (TheAbsoluteLevel > l) or (TheAbsoluteLevel < 0) then
    Result := nil
  else
  begin
    Result := Self;
    for i := TheAbsoluteLevel to l-1 do
      Result := Result.Parent;
  end;
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
  if Owner <> nil then
    Result := Owner.Owner
  else
    Result := nil;
end;

function TTreeNode.GetTop: integer;
begin
  if TreeView <> nil then
    TreeView.UpdateAllTops;
  Result := FTop;
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
begin
  if S=FText then exit;
  FText := S;
  if TreeView=nil then exit;
  Include(TreeView.FStates,tvsMaxRightNeedsUpdate);
  if (TreeView.SortType in [stText, stBoth])
  and (nsInTree in FStates) then begin
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
  and (not Deleting)
  and (nsInTree in FStates) then
  begin
    if Parent <> nil then
      Parent.AlphaSort
    else
      TreeView.AlphaSort;
  end;
end;

function TTreeNode.GetState(NodeState: TNodeState): Boolean;
begin
  Result:=NodeState in FStates;
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

procedure TTreeNode.SetImageEffect(AValue: TGraphicsDrawEffect);
begin
  if FNodeEffect=AValue then exit;
  FNodeEffect := AValue;
  Update;
end;

procedure TTreeNode.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex=AValue then exit;
  FImageIndex := AValue;
  Update;
end;

procedure TTreeNode.SetIndex(const AValue: Integer);

  procedure RaiseMissingTreeNodes;
  begin
    TreeViewError('TTreeNode.SetIndex missing Owner');
  end;

var
  OldIndex: LongInt;
begin
  OldIndex:=Index;
  if OldIndex=AValue then exit;
  if Parent=nil then begin
    // moving a top level node
    if Owner=nil then RaiseMissingTreeNodes;
    if AValue=0 then
      MoveTo(Owner.GetFirstNode,naInsert)
    else if AValue<OldIndex then
      MoveTo(Owner.TopLvlItems[AValue-1],naInsertBehind)
    else
      MoveTo(Owner.TopLvlItems[AValue],naInsertBehind);
  end else begin
    // moving a normal node
    if AValue=0 then
      MoveTo(Parent.GetFirstChild,naInsert)
    else if AValue<OldIndex then
      MoveTo(Parent.Items[AValue-1],naInsertBehind)
    else
      MoveTo(Parent.Items[AValue],naInsertBehind);
  end;
end;

procedure TTreeNode.SetSelectedIndex(AValue: Integer);
begin
  if FSelectedIndex = AValue then exit;
  FSelectedIndex := AValue;
  Update;
end;

procedure TTreeNode.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  Selected := False;
  if TreeView<>nil then
    TreeView.FStates:=TreeView.FStates+[tvsScrollbarChanged,tvsTopsNeedsUpdate,tvsBottomItemNeedsUpdate, tvsTopItemNeedsUpdate];
  Update;
end;

procedure TTreeNode.SetOverlayIndex(AValue: Integer);
begin
  if FOverlayIndex = AValue then exit;
  FOverlayIndex := AValue;
  Update;
end;

procedure TTreeNode.SetStateIndex(AValue: Integer);
begin
  if FStateIndex = AValue then exit;
  FStateIndex := AValue;
  Update;
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
  CurNode: TTreeNode;
begin
  TheTreeNodes:=TreeNodes;
  if TheTreeNodes=nil then exit;
  
  // Get the first selected node of the tree
  CurNode := TheTreeNodes.FFirstMultiSelected;
  
  // Initialize self unbinded
  Self.FPrevMultiSelected := nil;
  Self.FNextMultiSelected := nil;
  
  // If there isn't any selected node, set self as first
  if CurNode = nil then
    TheTreeNodes.FFirstMultiSelected := Self
  else
  begin
  
    // if last selected node was the previous one
    if (TheTreeNodes.FLastMultiSelected.AbsoluteIndex+1=Self.AbsoluteIndex) and (TheTreeNodes.FLastMultiSelected.FNextMultiSelected=nil) then
    begin
      TheTreeNodes.FLastMultiSelected.FNextMultiSelected := Self;
      Self.FPrevMultiSelected := TheTreeNodes.FLastMultiSelected;
    end
    else
    begin
      // if last selected node was the next one
      if (TheTreeNodes.FLastMultiSelected.AbsoluteIndex=Self.AbsoluteIndex+1) and (TheTreeNodes.FLastMultiSelected.FPrevMultiSelected=nil) then
      begin
        TheTreeNodes.FLastMultiSelected.FPrevMultiSelected := Self;
        Self.FNextMultiSelected := TheTreeNodes.FLastMultiSelected;
        TheTreeNodes.FFirstMultiSelected := Self
      end
      else
      begin

        // Scan linked list of selected nodes until one has a lower absolute index or we reach the end
        While (CurNode.GetNextMultiSelected<>Nil) and (CurNode.AbsoluteIndex<Self.AbsoluteIndex) do
          CurNode := CurNode.GetNextMultiSelected;

        // last of the list
        if CurNode.AbsoluteIndex < Self.AbsoluteIndex then
        begin
           CurNode.FNextMultiSelected := Self;
           Self.FPrevMultiSelected    := CurNode;
        end
        else
        // insert between two nodes
        begin
           Self.FPrevMultiSelected    := CurNode.FPrevMultiSelected;
           Self.FNextMultiSelected    := CurNode;
           if CurNode.FPrevMultiSelected <> nil then
              CurNode.FPrevMultiSelected.FNextMultiSelected := Self;
           CurNode.FPrevMultiSelected := Self;
        end;
        // Set self as head of the list if needed
        if Self.FPrevMultiSelected = nil then
          TheTreeNodes.FFirstMultiSelected := Self;
      end;
      
    end;
    
  end;
  
  // Set self as last selected node
  TheTreeNodes.FLastMultiSelected := Self;

end;

function TTreeNode.CompareCount(CompareMe: Integer): Boolean;
Begin
  Result:=(CompareMe=Count);
end;

function TTreeNode.DoCanExpand(ExpandIt: Boolean): Boolean;
begin
  Result := False;
  if (TreeView<>nil) and HasChildren then begin
    if ExpandIt then
      Result := TreeView.CanExpand(Self)
    else
      Result := TreeView.CanCollapse(Self);
  end;
end;

procedure TTreeNode.DoExpand(ExpandIt: Boolean);
begin
//DebugLn('[TTreeNode.DoExpand] Self=',DbgS(Self),' Text=',Text,
//' HasChildren=',HasChildren,' ExpandIt=',ExpandIt,' Expanded=',Expanded);
  if HasChildren and (Expanded<>ExpandIt) then begin
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
      Update;
      if ExpandIt then
        TreeView.Expand(Self)
      else
        TreeView.Collapse(Self);
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
var
  MultiSelect: Boolean;
  TV: TCustomTreeView;
begin
  if AValue=GetSelected then exit;
  TV:=TreeView;
  if AValue then
  begin
    Include(FStates,nsSelected);
    if (TV<>nil) then
    begin
      TV.EndEditing(true);
      MultiSelect:=tvoAllowMultiselect in TV.Options;

      if not MultiSelect and Assigned(FOwner) then
        FOwner.SelectionsChanged(Self, True);

      if MultiSelect then TV.LockSelectionChangeEvent;
      try
        TV.Selected:=Self;
        if TV.Selected<>Self then
          Exclude(FStates,nsSelected);
        if (nsSelected in FStates) and MultiSelect then
          MultiSelected:=true;
      finally
        if MultiSelect then TV.UnlockSelectionChangeEvent;
      end;
    end;
  end else
  begin
    if not MultiSelected and Assigned(FOwner) then
      FOwner.SelectionsChanged(Self, False);
    Exclude(FStates,nsSelected);
    if (TV<>nil) and (TV.Selected=Self) then
    begin
      TV.EndEditing(true);
      TV.Selected:=nil;
      if TV.Selected=Self then
        Include(FStates,nsSelected);
    end;
  end;
  Update;
end;

function TTreeNode.GetCut: Boolean;
begin
  Result := GetState(nsCut);
end;

procedure TTreeNode.SetCut(AValue: Boolean);
begin
  if AValue=Cut then exit;
  if AValue then
    Include(FStates,nsCut)
  else
    Exclude(FStates,nsCut);
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
begin
  if AValue=GetFocused then exit;
  if AValue then
    Include(FStates,nsFocused)
  else
    Exclude(FStates,nsFocused);
  Update;
end;

function TTreeNode.Bottom: integer;
begin
  Result := Top + Height;
end;

function TTreeNode.BottomExpanded: integer;
var
  Node: TTreeNode;
begin
  Node := GetNextVisibleSibling;
  if Node <> nil then
    Result := Node.Top
  else begin
    Node := GetLastVisibleChild;
    if Expanded and (Node <> nil) then
      Result := Node.BottomExpanded
    else
      Result := Bottom;
  end;
end;

function TTreeNode.GetFocused: Boolean;
begin
  Result := GetState(nsFocused);
end;

procedure TTreeNode.SetHasChildren(AValue: Boolean);
begin
  if AValue=HasChildren then exit;
  //DebugLn('[TTreeNode.SetHasChildren] Self=',DbgS(Self),' Self.Text=',Text,' AValue=',AValue);
  if AValue then
    Include(FStates,nsHasChildren)
  else begin
    while GetLastChild<>nil do
      GetLastChild.Free;
    Exclude(FStates,nsHasChildren)
  end;
  Update;
end;

function TTreeNode.GetNextSibling: TTreeNode;
begin
  Result := FNextBrother;
end;

function TTreeNode.GetPrevSibling: TTreeNode;
begin
  Result := FPrevBrother;
end;

function TTreeNode.GetNextVisible: TTreeNode;
begin
  if FVisible and Expanded and (GetFirstVisibleChild<>nil) then
    Result:=GetFirstVisibleChild
  else begin
    Result:=Self;
    while (Result<>nil) and (Result.GetNextVisibleSibling=nil) do
      Result:=Result.Parent;
    if Result<>nil then Result:=Result.GetNextVisibleSibling;
  end;
  if (Result<>nil) and ((not Result.IsVisible) or (not Result.FVisible)) then
    Result:=nil;
end;

function TTreeNode.GetNextVisibleSibling: TTreeNode;
begin
  Result := Self;
  repeat
    Result := Result.GetNextSibling;
  until ((Result=nil) or (Result.FVisible));
  if (Result<>nil) and (not Result.FVisible) then
    Result := nil
end;

function TTreeNode.GetPrevVisible: TTreeNode;
var
  ANode: TTreeNode;
begin
  Result:=GetPrevVisibleSibling;
  if Result <> nil then begin
    while Result.Visible and Result.Expanded do begin
      ANode:=Result.GetLastChild;
      if ANode=nil then break;
      Result:=ANode;
    end;
  end else
    Result := Parent;
  if (Result<>nil) and (TreeView<>nil) and (not TreeView.IsNodeVisible(Result))
  then
    Result:=nil;
end;

function TTreeNode.GetPrevVisibleSibling: TTreeNode;
begin
  Result := Self;
  repeat
    Result := Result.GetPrevSibling;
  until ((Result=nil) or (Result.FVisible));
  if (Result<>nil) and (not Result.FVisible) then
    Result := nil
end;

function TTreeNode.GetPrevExpanded: TTreeNode;
var
  ANode: TTreeNode;
begin
  Result:=GetPrevVisibleSibling;
  if Result <> nil then begin
    while Result.Visible and Result.Expanded do begin
      ANode:=Result.GetLastVisibleChild;
      if ANode=nil then break;
      Result:=ANode;
    end;
  end
  else
    Result:=Parent;
end;

function TTreeNode.GetNextChild(AValue: TTreeNode): TTreeNode;
begin
  if AValue <> nil then
    Result := AValue.GetNextSibling
  else
    Result := nil;
end;

function TTreeNode.GetNextExpanded: TTreeNode;
var
  ANode: TTreeNode;
begin
  ANode := GetFirstVisibleChild;
  if Expanded and (ANode<>nil) then
    Result:=ANode
  else begin
    Result:=Self;
    while (Result<>nil) and (Result.GetNextVisibleSibling=nil) do
      Result:=Result.Parent;
    if Result<>nil then
      Result:=Result.GetNextVisibleSibling;
  end;
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

function TTreeNode.GetFirstVisibleChild: TTreeNode;
begin
  Result := GetFirstChild;
  if (Result<>nil) and (not Result.FVisible) then
    Result := Result.GetNextVisibleSibling;
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
  if Count > 0 then
    Result := FItems[Count - 1]
  else
    Result := nil;
end;

function TTreeNode.GetLastVisibleChild: TTreeNode;
begin
  Result := GetLastChild;
  if Assigned(Result) and not Result.Visible then begin
    Result := Result.GetPrevVisible;
    // ToDo: implement this better. Now it works only when Self is visible.
    if Result = Self then                      // No visible nodes found.
      Result := Nil;
  end;
end;

function TTreeNode.GetLastSubChild: TTreeNode;
var
  Node: TTreeNode;
begin
  Result:=GetLastChild;
  if Result<>nil then begin
    Node:=Result.GetLastSubChild;
    if Node<>nil then
      Result:=Node;
  end;
end;

function TTreeNode.GetNext: TTreeNode;
begin
  Result:=GetFirstChild;
  if Result=nil then
    Result:=GetNextSkipChildren;
end;

function TTreeNode.GetNextSkipChildren: TTreeNode;
begin
  Result:=Self;
  while (Result<>nil) and (Result.FNextBrother=nil) do
    Result:=Result.Parent;
  if Result<>nil then
    Result:=Result.FNextBrother;
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
// - a node without children has an index one bigger than its previous brother
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

function TTreeNode.GetDeleting: Boolean;
begin
  Result := nsDeleting in FStates;
end;

function TTreeNode.GetHeight: integer;
begin
  if FHeight <= 0 then
  begin
    if TreeView <> nil then
      Result := TreeView.FDefItemHeight
    else
      Result := 20;
  end
  else
    Result := FHeight;
end;

function TTreeNode.GetIndex: Integer;
// returns number of previous siblings (nodes on same lvl with same parent)
var
  ANode: TTreeNode;
begin
  if FIndex>=0 then begin
    Result:=FIndex;
    exit;
  end;
  // many algorithms uses the last sibling, so we check that first for speed
  if (Parent<>nil) and (Parent[Parent.Count-1]=Self) then begin
    Result:=Parent.Count-1;
    FIndex:=Result;
    exit;
  end;
  // count previous siblings
  Result := -1;
  ANode := Self;
  while ANode <> nil do begin
    Inc(Result);
    if ANode.FIndex>=0 then begin
      inc(Result,ANode.FIndex);
      break;
    end;
    ANode := ANode.GetPrevSibling;
  end;
  FIndex:=Result;
end;

function TTreeNode.GetItems(AnIndex: Integer): TTreeNode;
begin
  if (AnIndex<0) or (AnIndex>=Count) then
    TreeNodeErrorFmt(rsIndexOutOfBounds,[ClassName, AnIndex, Count-1]);
  Result:=FItems[AnIndex];
end;

procedure TTreeNode.SetItems(AnIndex: Integer; AValue: TTreeNode);
begin
  if (AnIndex<0) or (AnIndex>=Count) then
    TreeNodeErrorFmt(rsIndexOutOfBounds, [ClassName, AnIndex, Count-1]);
  Items[AnIndex].Assign(AValue);
end;

procedure TTreeNode.SetMultiSelected(const AValue: Boolean);
begin
  if AValue=GetMultiSelected then exit;
  if AValue then begin
    if (Treeview<>nil) and (not (tvoAllowMultiselect in TreeView.Options)) then
      exit;
    if Assigned(FOwner) then
      FOwner.SelectionsChanged(Self, True);
    Include(FStates,nsMultiSelected);
    if TreeNodes<>nil then BindToMultiSelected;
  end else begin
    if Assigned(FOwner) then
      FOwner.SelectionsChanged(Self, False);
    Exclude(FStates,nsMultiSelected);
    if TreeNodes<>nil then UnbindFromMultiSelected;
  end;
  if TreeView<>nil then TreeView.InternalSelectionChanged;
  Update;
end;

function TTreeNode.IndexOf(AValue: TTreeNode): Integer;
begin
  if (AValue = nil) or (AValue.FParent <> Self) then
  begin
    Result:=-1;
    exit;
  end;
  Result := AValue.GetIndex;
end;

function TTreeNode.IndexOfText(const NodeText: string): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do
  begin
    if FItems[Result].Text = NodeText then exit;
    dec(Result);
  end;
end;

function TTreeNode.FindNode(const NodeText: string): TTreeNode;
begin
  Result:=GetFirstChild;
  while (Result<>nil) and (Result.Text<>NodeText) do
    Result:=Result.GetNextSibling;
end;

function TTreeNode.GetTextPath: string;
var
  Node: TTreeNode;
begin
  Result := '';
  Node := Self;
  while Assigned(Node) do
  begin
    if Result <> '' then
      Result := '/' + Result;
    Result := Node.Text + Result;
    Node := Node.Parent;
  end;
end;

function TTreeNode.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TTreeNode.EndEdit(Cancel: Boolean);
var
  TV: TCustomTreeView;
begin
  TV:=TreeView;
  if TV=nil then exit;
  TV.EndEditing(Cancel);
end;

procedure TTreeNode.Unbind;
// unbind from parent and neighbor siblings, but not from owner
var OldIndex, i: integer;
  HigherNode: TTreeNode;
  TheTreeView: TCustomTreeView;
begin
  {$IFDEF TREEVIEW_DEBUG}
  DebugLn('[TTreeNode.Unbind] Self=',DbgS(Self),' Self.Text=',Text);
  {$ENDIF}
  if not (nsBound in FStates) then exit;
  Exclude(FStates,nsBound);
  // remove single select
  Selected:=false;
  // invalidate caches of TreeView and if root item, remove from TreeView.Items
  if Owner<>nil then begin
    Owner.ClearCache;
    if FParent=nil then
      Owner.MoveTopLvlNode(Owner.IndexOfTopLvlItem(Self),-1,Self);
    TheTreeView:=Owner.Owner;
    if TheTreeView<>nil then begin
      TheTreeView.FStates:=TheTreeView.FStates+[tvsMaxRightNeedsUpdate,
        tvsTopsNeedsUpdate,tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate];
      if TheTreeView.FLastDropTarget=Self then
        TheTreeView.FLastDropTarget:=nil;
      if TheTreeView.FInsertMarkNode=Self then
        TheTreeView.FInsertMarkNode:=nil;
    end;
  end;
  // unmultiselect (keeping MultiSelected, but leaving multiselection list)
  UnbindFromMultiSelected;
  // remove from sibling list
  if FPrevBrother<>nil then FPrevBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPrevBrother:=FPrevBrother;
  FPrevBrother:=nil;
  FNextBrother:=nil;
  // remove from parent
  if FParent<>nil then begin
    // update all FSubTreeCount
    HigherNode:=FParent;
    while HigherNode<>nil do begin
      dec(HigherNode.FSubTreeCount,FSubTreeCount);
      HigherNode:=HigherNode.Parent;
    end;
    //if TreeNodes<>nil then Dec(TreeNodes.FCount,FSubTreeCount);
    // remove from parents list
    OldIndex:=FIndex;
    if OldIndex<0 then
      RaiseGDBException('');
    for i:=OldIndex to FParent.FCount-2 do begin
      FParent.FItems[i]:=FParent.FItems[i+1];
      FParent.FItems[i].FIndex:=i;
    end;
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
  // Reset last multiselected node
  if TheTreeNodes.FLastMultiSelected=Self then
  begin
    if Self.FNextMultiSelected <> nil then
      TheTreeNodes.FLastMultiSelected := Self.FNextMultiSelected
    else
      if Self.FPrevMultiSelected <> nil then
        TheTreeNodes.FLastMultiSelected := Self.FPrevMultiSelected
      else
        TheTreeNodes.FLastMultiSelected := nil;
  end;
  if FNextMultiSelected<>nil then
    FNextMultiSelected.FPrevMultiSelected:=FPrevMultiSelected;
  if FPrevMultiSelected<>nil then
    FPrevMultiSelected.FNextMultiSelected:=FNextMultiSelected;
  FNextMultiSelected:=nil;
  FPrevMultiSelected:=nil;
end;

function AddModeStr(AddMode: TAddMode): string;
begin
  WriteStr(Result, AddMode);
end;

procedure TTreeNode.InternalMove(ANode: TTreeNode; AddMode: TAddMode);
{
  TAddMode = (taAddFirst, taAdd, taInsert);

  taAdd:      add Self as last child of ANode
  taAddFirst: add Self as first child of ANode
  taInsert:   add Self in front of ANode
}
var
  HigherNode: TTreeNode;
  NewIndex, NewParentItemSize, i: integer;
  WasSelected: Boolean;
begin
  {$IFDEF TREEVIEW_DEBUG}
  DbgOut('[TTreeNode.InternalMove]  Self=',DbgS(Self),' Self.Text=',Text
         ,' ANode=',ANode<>nil,' AddMode=', AddModeStr(AddMode));
  if ANode<>nil then
    DbgOut(' ANode.Text=',ANode.Text);
  DebugLn('');
  {$ENDIF}
  if TreeView<>nil then TreeView.BeginUpdate;
  try
    WasSelected:=Selected;
    Unbind;
    Include(FStates,nsBound);
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
      for i:=FParent.FCount-1 downto NewIndex+1 do begin
        FParent.FItems[i]:=FParent.FItems[i-1];
        FParent.FItems[i].FIndex:=i;
      end;
      // insert this node to parent's items
      FParent.FItems[NewIndex]:=Self;
      FIndex:=NewIndex;
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
          //DebugLn('[TTreeNode.InternalMove] ANode.Index=',ANode.Index,' ANode=',DbgS(ANode));
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
    // re-add to multiselection list
    if MultiSelected then
      BindToMultiSelected;
    if WasSelected then
      Selected:=true;
  finally
    if TreeView<>nil then TreeView.EndUpdate;
  end;

  {$IFDEF TREEVIEW_DEBUG}
  DbgOut('[TTreeNode.InternalMove] END Self=',DbgS(Self),' Self.Text=',Text
         ,' ANode=',DbgS(ANode<>nil),' AddMode=',AddModeStr(AddMode));
  if ANode<>nil then
    DbgOut(' ANode.Text=',ANode.Text);
  DebugLn('');
  {$ENDIF}
end;

procedure TTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
{
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  naAdd:           add as last sibling of Destination
  naAddFirst:      add as first sibling of Destnation
  naAddChild:      add as last child of Destination
  naAddChildFirst: add as first child of Destination
  naInsert:        insert in front of Destination
  naInsertBehind:  insert behind Destination
}
var
  AddMode: TAddMode;
  //ANode: TTreeNode;
  //HItem: HTreeItem;
  OldOnChanging: TTVChangingEvent;
  OldOnChange: TTVChangedEvent;
begin
  if (Destination=nil) and not(Mode in [naAdd,naAddFirst]) then
    TreeNodeError('TTreeNode.MoveTo Destination=nil');
  if Mode=naInsertBehind then begin      // convert naInsertBehind
    if Destination.GetNextSibling=nil then
      Mode:=naAdd
    else begin
      Mode:=naInsert;
      Destination:=Destination.GetNextSibling;
    end;
  end;
  if (Destination = nil) or not Destination.HasAsParent(Self) then begin
    OldOnChanging := TreeView.OnChanging;
    OldOnChange := TreeView.OnChange;
    TreeView.OnChanging := nil;
    TreeView.OnChange := nil;
    try
      if (Destination <> nil) and (Mode in [naAdd, naAddFirst]) then
        Destination := Destination.Parent;
      case Mode of
        naAddFirst,
        naAddChildFirst: AddMode := taAddFirst;
        naInsert:        AddMode := taInsert;
      else
        AddMode:=taAdd;
      end;
      if (Destination <> Self) then
        InternalMove(Destination, AddMode);
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
  if Assigned(TreeView) and not (tvoAllowMultiselect in TreeView.Options) then
    Exit;
  if Assigned(TreeView) then TreeView.LockSelectionChangeEvent;
  try
    // We need to select the nodes between the selected node and the current node
    FirstNode := GetPrevSibling;
    while Assigned(FirstNode) and not FirstNode.Selected do
      FirstNode := FirstNode.GetPrevSibling;
    if not Assigned(FirstNode) then FirstNode := Self;
    LastNode := GetNextSibling;
    while Assigned(LastNode) and not LastNode.Selected do
      LastNode := LastNode.GetNextSibling;
    if not Assigned(LastNode) then LastNode := Self;
    ANode := FirstNode;
    while Assigned(ANode) do
    begin
      ANode.MultiSelected := True;
      if ANode = LastNode then Break;
      ANode := ANode.GetNextSibling;
    end;
  finally
    if Assigned(TreeView) then TreeView.UnlockSelectionChangeEvent;
  end;
end;

procedure TTreeNode.MakeVisible;
begin
  if Assigned(TreeView) then
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
  while Assigned(ANode) do
  begin
    Inc(Result);
    ANode := ANode.Parent;
  end;
end;

function TTreeNode.GetMultiSelected: Boolean;
begin
  Result := GetState(nsMultiSelected);
end;

function TTreeNode.IsNodeVisible: Boolean;
begin
  if Assigned(TreeView) then
    Result := TreeView.IsNodeVisible(Self)
  else
    Result := AreParentsExpanded;
end;

function TTreeNode.IsNodeHeightFullVisible: Boolean;
begin
  if Assigned(TreeView) then
    Result := TreeView.IsNodeHeightFullVisible(Self)
  else
    Result := AreParentsExpanded;
end;

procedure TTreeNode.Update;
var
  TV: TCustomTreeView;
begin
  TV := TreeView;
  if Assigned(TV) and (Owner.FUpdateCount = 0) and (not (csLoading in TV.ComponentState)) then
    TV.Invalidate;
end;

function TTreeNode.EditText: Boolean;
var
  TV: TCustomTreeView;
begin
  TV := TreeView;
  Result := Assigned(TV) and (tvsIsEditing in TreeView.FStates);
  TV.BeginEditing(Self);
end;

function TTreeNode.DisplayRect(TextOnly: Boolean): TRect;
begin
  FillChar(Result, SizeOf(Result), 0);
  if TreeView <> nil then
  begin
    Result.Left := TreeView.BorderWidth;
    Result.Top := Top - TreeView.ScrolledTop + TreeView.BorderWidth;
    Result.Right := TreeView.ClientWidth - TreeView.BorderWidth;
    Result.Bottom := Result.Top + Height;
    if TextOnly then
    begin
      Result.Left := DisplayTextLeft;
      if Result.Left > Result.Right then
        Result.Left := Result.Right;
      Result.Right := DisplayTextRight;
      if Result.Right < Result.Left then
        Result.Right := Result.Left;
    end;
  end;
end;

function TTreeNode.DisplayExpandSignLeft: integer;
var
  TV: TCustomTreeView;
  l: LongInt;
begin
  Result := 0;
  TV := TreeView;
  if TV = nil then Exit;
  l := Level;
  if not (tvoShowRoot in TV.Options) then
    inc(Result, TV.Indent * (l - 1) + (TV.Indent shr 2) + TV.BorderWidth - TV.FScrolledLeft)
  else
    inc(Result, TV.Indent * l + TV.BorderWidth - TV.FScrolledLeft);
end;

function TTreeNode.DisplayExpandSignRect: TRect;
begin
  FillChar(Result, SizeOf(Result), 0);
  if TreeView <> nil then
  begin
    Result.Left := DisplayExpandSignLeft;
    Result.Top := Top;
    Result.Right := Result.Left + TreeView.Indent;
    Result.Bottom := Top + Height;
  end;
end;

function TTreeNode.DisplayExpandSignRight: integer;
begin
  Result := DisplayExpandSignLeft;
  if TreeView <> nil then
    inc(Result, TreeView.Indent);
end;

function TTreeNode.DisplayIconLeft: integer;
begin
  Result := DisplayExpandSignRight;
end;

function TTreeNode.DisplayStateIconLeft: integer;
var
  TV: TCustomTreeView;
begin
  Result := DisplayIconLeft;
  TV := TreeView;
  if (TV <> nil) and (TV.Images <> nil) then
    inc(Result, TV.Images.Width + 2);
end;

function TTreeNode.DisplayTextLeft: integer;
var
  TV: TCustomTreeView;
begin
  Result := DisplayStateIconLeft;
  TV := TreeView;
  if (TV <> nil) and (TV.StateImages <> nil) then
    inc(Result, TV.StateImages.Width + 2);
end;

function TTreeNode.DisplayTextRight: integer;
var
  TV: TCustomTreeView;
begin
  Result := DisplayTextLeft;
  TV := TreeView;
  if TV <> nil then
    Inc(Result, TV.Canvas.TextWidth(Text) + TV.Indent div 2);
end;

function TTreeNode.AlphaSort: Boolean;
begin
  Result := CustomSort(nil);
end;

function TTreeNode.CustomSort(SortProc: TTreeNodeCompare): Boolean;
begin
  if FCount>0 then begin
    if Owner<>nil then Owner.ClearCache;
    if not Assigned(SortProc) then SortProc:=@DefaultTreeViewSort;
    Sort(FItems, FCount, SortProc, true);
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

procedure TTreeNode.ReadData(Stream: TStream; StreamVersion: integer);
var
  I, ItemCount: Integer;
  NewExpanded: boolean;
  OldInfo: TOldTreeNodeInfo;
  Info: TTreeNodeInfo;
  Node: TTreeNode;
begin
  if Owner<>nil then Owner.ClearCache;
  if StreamVersion=TTreeNodeWithPointerStreamVersion then
  begin
    Stream.ReadBuffer(OldInfo, SizeOf(TOldTreeNodeInfo));
    ImageIndex := OldInfo.ImageIndex;
    SelectedIndex := OldInfo.SelectedIndex;
    StateIndex := OldInfo.StateIndex;
    OverlayIndex := OldInfo.OverlayIndex;
    Data := OldInfo.Data;
    Height := OldInfo.Height;
    NewExpanded := OldInfo.Expanded;
    ItemCount := OldInfo.Count;
    SetLength(FText,OldInfo.TextLen)
  end
  else
  begin
    Stream.ReadBuffer(Info, SizeOf(TTreeNodeInfo));
    ImageIndex := Info.ImageIndex;
    SelectedIndex := Info.SelectedIndex;
    StateIndex := Info.StateIndex;
    OverlayIndex := Info.OverlayIndex;
    Height := Info.Height;
    NewExpanded := Info.Expanded;
    ItemCount := Info.Count;
    SetLength(FText,Info.TextLen);
  end;
  if FText<>'' then
    Stream.Read(FText[1],length(FText));
  if Owner<>nil then begin
    for I := 0 to ItemCount - 1 do begin
      Node:=Owner.AddChild(Self, '');
      Node.ReadData(Stream, StreamVersion);
      Owner.Owner.Added(Node);
    end;
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

procedure TTreeNode.WriteData(Stream: TStream);
var
  i: integer;
  Info: TTreeNodeInfo;
begin
  Info.ImageIndex := ImageIndex;
  Info.SelectedIndex := SelectedIndex;
  Info.OverlayIndex := OverlayIndex;
  Info.StateIndex := StateIndex;
  Info.Height := FHeight;
  Info.Count := Count;
  Info.Expanded := Expanded;
  Info.TextLen := Length(Text);
  Stream.WriteBuffer(Info, SizeOf(TTreeNodeInfo));
  if Text<>'' then
    Stream.Write(FText[1],length(Text));
  for i := 0 to Count - 1 do
    Items[i].WriteData(Stream);
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

function TTreeNode.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TTreeNode.ConsistencyCheck;
var RealSubTreeCount: integer;
  i: integer;
  Node1: TTreeNode;
begin
  if FOwner<>nil then begin
  end;
  if FCapacity<0 then
    RaiseGDBException('');
  if FCapacity<FCount then
    RaiseGDBException('');
  if FCount<0 then
    RaiseGDBException('');
  if FHeight<0 then
    RaiseGDBException('');
  if (FItems<>nil) and (FCapacity<=0) then
    RaiseGDBException('');
  if (FCapacity>0) and (FItems=nil) then
    RaiseGDBException('');
  if (FNextBrother<>nil) and (FNextBrother.FPrevBrother<>Self) then
    RaiseGDBException('');
  if (FPrevBrother<>nil) and (FPrevBrother.FNextBrother<>Self) then
    RaiseGDBException('');
  if (FNextMultiSelected<>nil) and (FNextMultiSelected.FPrevMultiSelected<>Self) then
    RaiseGDBException('');
  if (FPrevMultiSelected<>nil) and (FPrevMultiSelected.FNextMultiSelected<>Self) then
    RaiseGDBException('');
  if MultiSelected then begin
    Node1:=TreeView.GetFirstMultiSelected;
    while (Node1<>nil) and (Node1<>Self) do Node1:=Node1.FNextMultiSelected;
    if Node1=nil then
      RaiseGDBException('');
  end;
  if Selected and (TreeView<>nil) and (tvoAllowMultiselect in TreeView.Options)
  and (not MultiSelected) then
    RaiseGDBException('');// selected, but not multiselected

  // check children
  RealSubTreeCount:=1;
  for i:=0 to FCount-1 do begin
    if (Items[i]=nil) then RaiseGDBException('');
    Node1:=Items[i];
    if Node1.FParent<>Self then RaiseGDBException('');
    if (i=0) and (Node1.FPrevBrother<>nil) then
      RaiseGDBException('');
    if (i>0) and (Node1.FPrevBrother=nil) then
      RaiseGDBException('');
    if (i>0) and (Node1.FPrevBrother<>Items[i-1]) then
      RaiseGDBException('');
    if (i<FCount-1) and (Node1.FNextBrother=nil) then
      RaiseGDBException('');
    if (i<FCount-1) and (Node1.FNextBrother<>Items[i+1]) then
      RaiseGDBException('');
    if (i=FCount-1) and (Node1.FNextBrother<>nil) then
      RaiseGDBException('');
    if Node1.FIndex<>i then
      RaiseGDBException('');
    Node1.ConsistencyCheck;
    inc(RealSubTreeCount,Node1.SubTreeCount);
  end;
  if FParent<>nil then begin
    if FParent.IndexOf(Self)<0 then RaiseGDBException('');
  end;
  if RealSubTreeCount<>SubTreeCount then RaiseGDBException('');
  if FTop<0 then RaiseGDBException('');
  // check for circles
  if FNextBrother=Self then RaiseGDBException('');
  if FPrevBrother=Self then RaiseGDBException('');
  if FParent=Self then RaiseGDBException('');
  Node1:=FParent;
  while Node1<>nil do begin
    if (Node1=Self) then RaiseGDBException('');
    Node1:=Node1.FParent;
  end;
end;

procedure TTreeNode.WriteDebugReport(const Prefix: string; Recurse: boolean);
var i: integer;
begin
  DbgOut('%s%s.WriteDebugReport Self=%p',[Prefix, ClassName, Pointer(Self)]);
  ConsistencyCheck;
  DebugLn(' Text=',Text);
  if Recurse then begin
    for i:=0 to FCount-1 do
      Items[i].WriteDebugReport(Prefix+'  ',true);
  end;
end;


{ TTreeNodes }

constructor TTreeNodes.Create(AnOwner: TCustomTreeView);
begin
  inherited Create;
  FSelection := TFPList.Create;
  FOwner := AnOwner;
end;

destructor TTreeNodes.Destroy;
begin
  Clear;
  FreeThenNil(FSelection);
  inherited Destroy;
end;

function TTreeNodes.GetCount: Integer;
begin
  Result:=FCount;
  //if Owner.HandleAllocated then Result := TreeView_GetCount(Handle)
  //else Result := 0;
end;

function TTreeNodes.GetOwner: TPersistent;
begin
  Result := FOwner;
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
  Node.Delete;
  if (FUpdateCount=0) and (Owner<>nil) then
    Owner.Invalidate;
end;

procedure TTreeNodes.Clear;
var
  Node: TTreeNode;
begin
  BeginUpdate;
  ClearCache;
  Node := GetLastNode;
  if Assigned(Node) then
  begin
    while Assigned(Node) do
    begin
      Node.Delete;
      Node := GetLastNode;
    end;
    if (FUpdateCount = 0) and Assigned(Owner) then
      Owner.Invalidate;
  end;
  FSelection.Clear;
  if (FOwner <> nil) then
    FOwner.GetAccessibleObject().ClearChildAccessibleObjects();
  EndUpdate;
end;

procedure TTreeNodes.ClearMultiSelection(ClearSelected: boolean = false);
var
  ANode, OldNode: TTreeNode;
begin
  if Assigned(Owner) then Owner.LockSelectionChangeEvent;
  try
    ANode := FFirstMultiSelected;
    while Assigned(ANode) do
    begin
      OldNode := ANode;
      ANode := ANode.GetNextMultiSelected;
      OldNode.MultiSelected := false;
    end;
    if ClearSelected then
      Owner.Selected := nil;
  finally
    if Assigned(Owner) then Owner.UnlockSelectionChangeEvent;
  end;
end;

procedure TTreeNodes.SelectOnlyThis(Node: TTreeNode);
begin
  if Owner<>nil then Owner.LockSelectionChangeEvent;
  try
    ClearMultiSelection(true);
    Node.Selected:=true;
  finally
    if Owner<>nil then Owner.UnlockSelectionChangeEvent;
  end;
end;

function TTreeNodes.IsMultiSelection: boolean;
begin
  Result:=(FFirstMultiSelected<>nil)
          and (FFirstMultiSelected.GetNextMultiSelected<>nil);
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

function TTreeNodes.AddNode(Node: TTreeNode; Relative: TTreeNode;
  const S: string; Ptr: Pointer; Method: TNodeAttachMode): TTreeNode;
var
   AddMode: TAddMode;
begin
  if (Relative=nil) and not (Method in [naAdd,naAddFirst]) then
    TreeNodeError('TTreeNode.AddNode Relative=nil');
  if Method=naInsertBehind then begin    // convert naInsertBehind
    if Relative.GetNextSibling=nil then
      Method:=naAdd
    else begin
      Method:=naInsert;
      Relative:=Relative.GetNextSibling;
    end;
  end;
  if (Relative <> nil) and (Method in [naAdd, naAddFirst]) then
    Relative := Relative.Parent;
  // Convert TNodeAttachMode to TAddMode
  case Method of
    naAddFirst,naAddChildFirst: AddMode := taAddFirst;
    naInsert: AddMode := taInsert;
  else
    AddMode:=taAdd;
  end;
  fNewNodeToBeAdded := Node;
  Result := InternalAddObject(Relative, S, Ptr, AddMode);
end;

procedure TTreeNodes.SelectionsChanged(ANode: TTreeNode; const AIsSelected: Boolean);
begin
  if ANode <> nil then
  begin
    if AIsSelected then
      FSelection.Add(ANode)
    else
      FSelection.Remove(ANode);
  end;
end;

function TTreeNodes.GetSelections(const AIndex: Integer): TTreeNode;
begin
  if (AIndex < 0) or (AIndex > FSelection.Count - 1) then
    TreeNodeError('TTreeNodes.GetSelections Index '+IntToStr(AIndex)
      +' out of bounds (Count='+IntToStr(FSelection.Count)+')');
  Result := TTreeNode(FSelection.Items[AIndex]);
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

function TTreeNodes.InsertBehind(PrevNode: TTreeNode; const S: string): TTreeNode;
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

procedure TTreeNodes.SortTopLevelNodes(SortProc: TTreeNodeCompare);
begin
  Sort(FTopLvlItems, FTopLvlCount, SortProc, true);
end;

function TTreeNodes.InternalAddObject(Node: TTreeNode; const S: string;
  Data: Pointer; AddMode: TAddMode): TTreeNode;
{
  TAddMode = (taAddFirst, taAdd, taInsert);

  taAdd:      add Result as last child of Node
  taAddFirst: add Result as first child of Node
  taInsert:   add Result in front of Node
}
var
  ok: boolean;
  // Item: HTreeItem;
  lAccessibleObject: TLazAccessibleObject;
begin
  if Owner=nil then
    TreeNodeError('TTreeNodes.InternalAddObject Owner=nil');
  {$IFDEF TREEVIEW_DEBUG}
  write('[TTreeNodes.InternalAddObject] Node=',Node<>nil,' S=',S,
        ' AddMode=',AddModeStr(AddMode));
  if Node<>nil then
    DbgOut(' Node.Text=',Node.Text);
  DebugLn('');
  {$ENDIF}
  Result := fNewNodeToBeAdded; // Used by AddNode to pass an existing node.
  if Result = Nil then
    Result := Owner.CreateNode;
  fNewNodeToBeAdded := nil;
  ok:=false;
  try
    Result.Data := Data;
    Result.Text := S;
    // move node in tree (tree of TTreeNode)
    Result.InternalMove(Node,AddMode);
    if (Owner<>nil) and Owner.AutoExpand and (Result.Parent<>nil) then
      Result.Parent.Expanded:=true;
    if (Owner<>nil) and (not (csReading in Owner.ComponentState)) then
      Owner.Added(Result);
    if (FUpdateCount=0) and (Owner<>nil) then
      Owner.Invalidate;
    ok:=true;
    if ok and (Owner<>nil) then
    begin
      lAccessibleObject := FOwner.GetAccessibleObject().AddChildAccessibleObject();
      lAccessibleObject.AccessibleDescription := S;
      lAccessibleObject.AccessibleRole := larTreeItem;
      lAccessibleObject.DataObject := Result;
    end;
  finally
    // this construction creates nicer exception output
    if not ok then
      Result.Free;
  end;
end;

function TTreeNodes.GetFirstNode: TTreeNode;
begin
  if Assigned(FTopLvlItems) then
    Result := FTopLvlItems[0]
  else
    Result := nil;
  //Result := GetNode(TreeView_GetRoot(Handle));
end;

function TTreeNodes.GetFirstVisibleNode: TTreeNode;
var
  Node: TTreeNode;
  i: Integer;
begin
  Result := nil;
  if Assigned(FTopLvlItems) then
    for i := 0 to FTopLvlCount-1 do begin
      Node := FTopLvlItems[i];
      if Node.Visible then begin
        Result := Node;
        Break;
      end;
    end;
end;

function TTreeNodes.GetLastNode: TTreeNode;
begin
  if Assigned(FTopLvlItems) then
    Result := FTopLvlItems[FTopLvlCount - 1]
  else
    Result := nil;
end;

function TTreeNodes.GetLastVisibleNode: TTreeNode;
var
  Node: TTreeNode;
  i: Integer;
begin
  Result := nil;
  if Assigned(FTopLvlItems) then
    for i := FTopLvlCount-1 downto 0 do begin
      Node := FTopLvlItems[i];
      if Node.Visible then begin
        Result := Node;
        Break;
      end;
    end;
end;

function TTreeNodes.GetLastSubNode: TTreeNode;
// absolute last node
var
  Node: TTreeNode;
begin
  Result := GetLastNode;
  if Assigned(Result) then
  begin
    Node := Result.GetLastSubChild;
    if Assigned(Node) then Result := Node;
  end;
end;

function TTreeNodes.GetLastExpandedSubNode: TTreeNode;
// absolute last expanded node
var
  Node: TTreeNode;
begin
  Result := GetLastNode;
  while Assigned(Result) and (Result.Expanded) do
  begin
    Node := Result.GetLastChild;
    if Assigned(Node) then
      Result := Node
    else
      exit;
  end;
end;

function TTreeNodes.FindTopLvlNode(const NodeText: string): TTreeNode;
begin
  Result := GetFirstNode;
  while Assigned(Result) and (Result.Text <> NodeText) do
    Result := Result.GetNextSibling;
end;

function TTreeNodes.FindNodeWithText(const NodeText: string): TTreeNode;
begin
  Result := GetFirstNode;
  while Assigned(Result) and (Result.Text <> NodeText) do
    Result := Result.GetNext;
end;

function TTreeNodes.FindNodeWithData(const NodeData: Pointer): TTreeNode;
begin
  Result := GetFirstNode;
  while Assigned(Result) and (Result.Data <> NodeData) do
    Result := Result.GetNext;
end;

function TTreeNodes.GetNodeFromIndex(Index: Integer): TTreeNode;
// find node with absolute index in ALL nodes (even collapsed)

  procedure RaiseIndexOutOfBounds;
  begin
    TreeNodeError('TTreeNodes.GetNodeFromIndex Index '+IntToStr(Index)
           +' out of bounds (Count='+IntToStr(FCount)+')');
  end;
  
  procedure RaiseSubTreeCount0;
  begin
    TreeNodeError(
      'TTreeNodes.GetNodeFromIndex: Consistency error - SubTreeCount=0');
  end;
  
  procedure RaiseSubTreeCountTooBig;
  begin
    TreeNodeError(
      'TTreeNodes.GetNodeFromIndex: Consistency error - invalid SubTreeCount');
  end;
  
  procedure RaiseCountTooBig;
  begin
    TreeNodeError(
      'TTreeNodes.GetNodeFromIndex: Consistency Error - Count too big');
  end;

var
  I, J: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexOutOfBounds;
  if (FNodeCache.CacheNode <> nil) and (Abs(FNodeCache.CacheIndex - Index) <= 1)
  then begin
    with FNodeCache do
    begin
      if Index = CacheIndex then Result := CacheNode
      else if Index < CacheIndex then Result := CacheNode.GetPrev
      else Result := CacheNode.GetNext;
    end;
  end
  else if Index>Count-5 then begin
    // optimization for the last nodes
    Result:=GetLastSubNode;
    i:=Count-1;
    while (Index<i) do begin
      Result:=Result.GetPrev;
      dec(i);
    end;
  end
  else begin
    Result := GetFirstNode;
    I:=0;
    while (Result<>nil) and (Index>I) do begin
      Repeat
        // calculate the absolute index of the next sibling
        J:=I+Result.FSubTreeCount;
        if J=I then RaiseSubTreeCount0;
        if J<=Index then begin
          // Index > absolute index of next sibling -> search in next sibling
          Result:=Result.GetNextSibling;
          I:=J;
        end else
          break;
      until false;
      if (Result<>nil) and (Index>I) then begin
        // Index is somewhere in subtree of Result
        Result:=Result.GetFirstChild;
        if Result=nil then RaiseSubTreeCountTooBig;
        inc(I);
      end;
    end;
  end;
  if Result = nil then RaiseCountTooBig;
  FNodeCache.CacheNode := Result;
  FNodeCache.CacheIndex := Index;
end;

function TTreeNodes.GetSelectionCount: Cardinal;
begin
  Result := Cardinal(FSelection.Count);
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

function TTreeNodes.GetEnumerator: TTreeNodesEnumerator;
begin
  Result := TTreeNodesEnumerator.Create(Self);
end;

procedure TTreeNodes.GrowTopLvlItems;
begin
  if FTopLvlItems<>nil then begin
    FTopLvlCapacity:=FTopLvlCapacity shl 1;
    ReAllocMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
  end else begin
    FTopLvlCapacity:=MinNodeCapacity;
    GetMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
  end;
  //debugln('TTreeNodes.GrowTopLvlItems END FTopLvlCapacity=',FTopLvlCapacity,' FTopLvlCount=',FTopLvlCount,' ',FTopLvlItems<>nil);
end;

function TTreeNodes.GetTopLvlItems(Index: integer): TTreeNode;
begin
  Result:=FTopLvlItems[Index];
end;

procedure TTreeNodes.ShrinkTopLvlItems;
var
  OldCapacity: LongInt;
begin
  if FTopLvlItems<>nil then begin
    OldCapacity:=FTopLvlCapacity;
    FTopLvlCapacity:=FTopLvlCapacity shr 1;
    if FTopLvlCapacity<FTopLvlCount then FTopLvlCapacity:=FTopLvlCount;
    if (FTopLvlCapacity<MinNodeCapacity) then begin
      if (FTopLvlCount>0) then
        FTopLvlCapacity:=MinNodeCapacity
      else
        FTopLvlCapacity:=0;
    end;
    if OldCapacity=FTopLvlCapacity then exit;
    //debugln('TTreeNodes.ShrinkTopLvlItems A FTopLvlCapacity=',FTopLvlCapacity,' FTopLvlCount=',FTopLvlCount,' ',FTopLvlItems<>nil);
    ReAllocMem(FTopLvlItems,SizeOf(Pointer)*FTopLvlCapacity);
    //debugln('TTreeNodes.ShrinkTopLvlItems B FTopLvlCapacity=',FTopLvlCapacity,' FTopLvlCount=',FTopLvlCount,' ',FTopLvlItems<>nil);
  end else begin
    if (FTopLvlCapacity>0) then
      TreeNodeError('TTreeNodes.ShrinkTopLvlItems FTopLvlCapacity>0');
  end;
end;

function TTreeNodes.IndexOfTopLvlItem(Node: TTreeNode): integer;
begin
  if (Node<>nil) and (Node.Owner=Self) then
    Result:=Node.FIndex
  else
    Result:=-1;
end;

procedure TTreeNodes.MoveTopLvlNode(TopLvlFromIndex, TopLvlToIndex: integer;
  Node: TTreeNode);
// TopLvlFromIndex = -1 and is insert
// TopLvlToIndex = -1 is remove
var i: integer;
begin
  {$IFDEF TREEVIEW_DEBUG}
  DebugLn('[TTreeNodes.MoveTopLvlNode] TopLvlFromIndex=',TopLvlFromIndex,
  ' TopLvlToIndex=',TopLvlToIndex,' Node.Text=',Node.Text);
  {$ENDIF}
  if TopLvlFromIndex=TopLvlToIndex then exit;
  if (TopLvlFromIndex>=FTopLvlCount) then
    TreeNodeError('TTreeNodes.MoveTopLvlNode TopLvlFromIndex>FTopLvlCount');
  if (TopLvlToIndex>FTopLvlCount) then
    TreeNodeError('TTreeNodes.MoveTopLvlNode TopLvlFromIndex>FTopLvlCount');
  if (TopLvlFromIndex>=0) then begin
    Node:=FTopLvlItems[TopLvlFromIndex];
    if (TopLvlToIndex>=0) then begin
      // move node
      if TopLvlFromIndex<TopLvlToIndex then begin
        // move forward
        for i:=TopLvlFromIndex to TopLvlToIndex-1 do begin
          FTopLvlItems[i]:=FTopLvlItems[i+1];
          FTopLvlItems[i].FIndex:=i;
        end;
      end else begin
        // move backward
        for i:=TopLvlToIndex downto TopLvlFromIndex+1 do begin
          FTopLvlItems[i]:=FTopLvlItems[i-1];
          FTopLvlItems[i].FIndex:=i;
        end;
      end;
      FTopLvlItems[TopLvlToIndex]:=Node;
      FTopLvlItems[TopLvlToIndex].FIndex:=TopLvlToIndex;
    end else begin
      // remove node
      if FTopLvlItems<>nil then begin
        for i:=TopLvlFromIndex to FTopLvlCount-2 do begin
          FTopLvlItems[i]:=FTopLvlItems[i+1];
          FTopLvlItems[i].FIndex:=i;
        end;
      end;
      Dec(FTopLvlCount);
      if FTopLvlCount<0 then
        TreeNodeError('TTreeNodes.MoveTopLvlNode FTopLvlCount<0');
      if FTopLvlCount<(FTopLvlCapacity shr 2) then ShrinkTopLvlItems;
    end;
  end else begin
    if (TopLvlToIndex>=0) then begin
      if Node=nil then
        TreeNodeError('TTreeNodes.MoveTopLvlNode inserting nil');
      // insert node
      if (FTopLvlCount=FTopLvlCapacity) then GrowTopLvlItems;
      inc(FTopLvlCount);
      if FTopLvlItems<>nil then begin
        for i:=FTopLvlCount-1 downto TopLvlToIndex+1 do begin
          FTopLvlItems[i]:=FTopLvlItems[i-1];
          FTopLvlItems[i].FIndex:=i;
        end;
        FTopLvlItems[TopLvlToIndex]:=Node;
        FTopLvlItems[TopLvlToIndex].FIndex:=TopLvlToIndex;
      end;
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
        Result := not Item[I].IsEqual(Nodes[I]);
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
  DelphiNodeInfo: TDelphiNodeInfo;
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
      Add(nil, '').ReadData(Stream, StreamVersion);
  end else begin
    // delphi stream
    NewCount:=MagicNumber;
    for I := 0 to NewCount - 1 do
      Add(nil, '').ReadDelphiData(Stream, @DelphiNodeInfo);
  end;
end;

procedure TTreeNodes.WriteData(Stream: TStream);
var
  ANode: TTreeNode;
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
    ANode.WriteData(Stream);
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

procedure TTreeNodes.ConsistencyCheck;
var Node: TTreeNode;
  RealCount, i: integer;
  OldCache: TNodeCache;
begin
  if FUpdateCount<0 then
    RaiseGDBException('FUpdateCount<0');
  RealCount:=0;
  Node:=GetFirstNode;
  while Node<>nil do begin
    Node.ConsistencyCheck;
    inc(RealCount,Node.SubTreeCount);
    //DebugLn(' ConsistencyCheck: B  ',RealCount,',',Node.SubTreeCount);
    Node:=Node.FNextBrother;
  end;
  //DebugLn(' ConsistencyCheck: B  ',RealCount,',',FCount);
  if RealCount<>FCount then
    RaiseGDBException('RealCount<>FCount');
  if (FTopLvlCapacity<=0) and (FTopLvlItems<>nil) then
    RaiseGDBException('');
  if (FTopLvlCapacity>0) and (FTopLvlItems=nil) then
    RaiseGDBException('');
  if FTopLvlCapacity<FTopLvlCount then
    RaiseGDBException('');
  if (FTopLvlCount<0) then
    RaiseGDBException('');
  for i:=0 to FTopLvlCount-1 do begin
    Node:=FTopLvlItems[i];
    if (i=0) and (Node.FPrevBrother<>nil) then
      RaiseGDBException('');
    if (i>0) and (Node.FPrevBrother<>FTopLvlItems[i-1]) then
      RaiseGDBException('');
    if (i<FTopLvlCount-1) and (Node.FNextBrother<>FTopLvlItems[i+1])
    then begin
      DebugLn(' CONSISTENCY i=%d FTopLvlCount=%d FTopLvlItems[i]=%p FTopLvlItems[i].FNextBrother=%p FTopLvlItems[i+1]=%p',
             [i, FTopLvlCount, Pointer(Node), Pointer(Node.FNextBrother), Pointer(FTopLvlItems[i+1])]);
      RaiseGDBException('');
    end;
    if (i=FTopLvlCount-1) and (Node.FNextBrother<>nil) then
      RaiseGDBException('');
    if Node.FIndex<>i then
      RaiseGDBException('');
  end;
  if FNodeCache.CacheNode<>nil then begin
    OldCache:=FNodeCache;
    ClearCache;
    if GetNodeFromIndex(OldCache.CacheIndex)<>OldCache.CacheNode then
      RaiseGDBException('');
  end;
end;

procedure TTreeNodes.WriteDebugReport(const Prefix: string; AllNodes: boolean);
var Node: TTreeNode;
begin
  DbgOut('%s%s.WriteDebugReport Self=%p', [Prefix, ClassName, Pointer(Self)]);
  ConsistencyCheck;
  DebugLn('');
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
  for I := 0 to Level - 1 do
    Result := Result + TabChar;
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
  DebugLn('%sTTreeStrings.WriteDebugReport Self=%p Consistency=%d', [Prefix, Pointer(Self), ConsistencyCheck]);
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
  FBackgroundColor := clWindow;
  FDefItemHeight := 20;
  FExpandSignType := tvestTheme;
  UpdateExpandSignSize;
  FTreeNodes := TTreeNodes.Create(Self);
  BorderStyle := bsSingle;
  BorderWidth := 0;
  FMultiSelectStyle := DefaultMultiSelectStyle;
  FOptions := DefaultTreeViewOptions;
  Items.KeepCollapsedNodes:=KeepCollapsedNodes;
  FScrollBars:=ssBoth;
  FDragImage := TDragImageList.CreateSize(32, 32);
  FIndent:=15;
  FChangeTimer := TTimer.Create(Self);
  FChangeTimer.Enabled := False;
  FChangeTimer.Interval := 1;
  FChangeTimer.OnTimer := @OnChangeTimer;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FSelectedColor:=clHighlight;
  fSeparatorColor:=clGray;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := @ImageListChange;
  FStates:=[tvsMaxLvlNeedsUpdate,tvsMaxRightNeedsUpdate,tvsScrollbarChanged];
  FTreeLineColor := clWindowFrame;
  FTreeLinePenStyle := psPattern;
  FExpandSignColor := clWindowFrame;
  SetAccesibilityFields('A tree of items', '', larTreeView);
end;

destructor TCustomTreeView.Destroy;
begin
  LockSelectionChangeEvent; // prevent change event during destroying
  
  Images:=nil;
  FreeThenNil(FTreeNodes);
  FreeThenNil(FSaveItems);
  FreeThenNil(FDragImage);
  FreeThenNil(FImageChangeLink);
  FreeThenNil(FStateChangeLink);
  inherited Destroy;
end;

procedure TCustomTreeView.CreateWnd;
begin
  Exclude(FStates,tvsStateChanging);
  inherited CreateWnd;
end;

procedure TCustomTreeView.InitializeWnd;
begin
  inherited InitializeWnd;
  UpdateDefaultItemHeight;
end;

procedure TCustomTreeView.Invalidate;
begin
  if tvsPainting in FStates then exit;
  inherited Invalidate;
end;

procedure TCustomTreeView.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

procedure TCustomTreeView.DestroyWnd;
begin
  Include(FStates, tvsStateChanging);
  inherited DestroyWnd;
  if Canvas <> nil then
    TControlCanvas(Canvas).FreeHandle;
  FLastHorzScrollInfo.cbSize := 0;
  FLastVertScrollInfo.cbSize := 0;
end;

procedure TCustomTreeView.BeginAutoDrag;
begin
  BeginDrag(False);
end;

procedure TCustomTreeView.BeginEditing(ANode: TTreeNode);
var
  ARect: TRect;
begin
  //DebugLn(['TCustomTreeView.BeginEditing tvsIsEditing=',tvsIsEditing in FStates,' Selected=',Selected<>nil]);
  if (tvsIsEditing in FStates) or (ANode=nil) then exit;
  if (not CanEdit(ANode)) or (not ANode.FVisible) then exit;
  // if we are asked to edit another node while one is already being edited then
  // stop editing that node
  if FEditingItem <> nil then
    EndEditing;
  FEditingItem := ANode;
  // make node visible (this will cancel editing, so call this first)
  EnsureNodeIsVisible(ANode);

  Include(FStates,tvsIsEditing);
  if FEditor=nil then begin
    FEditor:=TEdit.Create(Self);
    FEditor.OnEditingDone:=@EditorEditingDone;
    FEditor.OnKeyDown:=@EditorKeyDown;
  end;
  ARect:=Rect(Max(BorderWidth,ANode.DisplayTextLeft),ANode.Top-ScrolledTop,
              ClientWidth-BorderWidth,ANode.Bottom-ScrolledTop);
  FEditor.BoundsRect:=ARect;
  FEditor.AnchorParallel(akLeft,ARect.Left,Self);
  FEditor.AnchorParallel(akRight,BorderWidth,Self);
  FEditor.Visible:=true;
  FEditor.Parent:=Self;
  FEditor.Text:=ANode.Text;
  FEditor.SelectAll;
  FEditor.SetFocus;
end;

procedure TCustomTreeView.BeginUpdate;
begin
  Items.BeginUpdate;
  LockSelectionChangeEvent;
end;

procedure TCustomTreeView.EndUpdate;
begin
  UnlockSelectionChangeEvent;
  if Items.FUpdateCount<=0 then RaiseGDBException('TCustomTreeView.EndUpdate');
  Items.EndUpdate;
  if Items.FUpdateCount=0 then begin
    // ToDo: only refresh if something changed
    UpdateScrollBars;
    Invalidate;
  end;
end;

function TCustomTreeView.AlphaSort: Boolean;
begin
  Result := CustomSort(nil);
end;

function TCustomTreeView.CustomSort(SortProc: TTreeNodeCompare): Boolean;
var Node: TTreeNode;
begin
  Result := False;
  if FTreeNodes.Count>0 then begin
    BeginUpdate;
    if not assigned(SortProc) then SortProc := @DefaultTreeViewSort;
    FTreeNodes.SortTopLevelNodes(SortProc);

    Node := FTreeNodes.GetFirstNode;
    while Node <> nil do begin
      if (Node.GetFirstChild<>nil) then Node.CustomSort(SortProc);
      Node := Node.GetNext;
    end;
    Items.ClearCache;
    FStates:= FStates+[tvsTopsNeedsUpdate, tvsTopItemNeedsUpdate,
                       tvsBottomItemNeedsUpdate,tvsScrollbarChanged];
    Invalidate;
    EndUpdate;
  end;
end;

function TCustomTreeView.DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
begin
  if Assigned(OnCompare) then begin
    Result:=0;
    OnCompare(Node1.TreeView,Node1, Node2, Result);
  end else
    Result := AnsiCompareStr(Node1.Text,Node2.Text);
end;

procedure TCustomTreeView.SetAutoExpand(Value: Boolean);
begin
  if AutoExpand <> Value then begin
    if Value then
      Include(FOptions,tvoAutoExpand)
    else
      Exclude(FOptions,tvoAutoExpand);
  end;
end;

procedure TCustomTreeView.SetHotTrack(Value: Boolean);
begin
  if HotTrack <> Value then begin
    if Value then
      Include(FOptions,tvoHotTrack)
    else
      Exclude(FOptions,tvoHotTrack);
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
  end;
end;

procedure TCustomTreeView.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    Include(FStates,tvsScrollbarChanged);
    UpdateScrollBars;
  end;
end;

procedure TCustomTreeView.SetScrolledLeft(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
  if AValue>GetMaxScrollLeft then AValue:=GetMaxScrollLeft;
  if AValue=FScrolledLeft then exit;
  EndEditing(true);
  FScrolledLeft:=AValue;
  Include(FStates,tvsScrollbarChanged);
  Invalidate;
end;

procedure TCustomTreeView.SetScrolledTop(AValue: integer);
begin
  if FScrolledTop=AValue then exit;
  if AValue<0 then AValue:=0;
  if AValue>GetMaxScrollTop then AValue:=GetMaxScrollTop;
  if AValue=FScrolledTop then exit;
  EndEditing(true);
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

procedure TCustomTreeView.Paint;
begin
  DoPaint;
end;

procedure TCustomTreeView.SetDragMode(Value: TDragMode);
begin
  inherited SetDragMode(Value);
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
      if Selected<>nil then
        Selected.MultiSelected:=true;
    end else begin
      Items.ClearMultiSelection;
    end;
  end;
  if tvoAutoItemHeight in ChangedOptions then
    UpdateDefaultItemHeight;
  if ([tvoHideSelection,tvoReadOnly,tvoShowButtons,tvoShowRoot,tvoShowLines]
    * ChangedOptions)<>[]
  then
    Invalidate;
end;

procedure TCustomTreeView.UpdateDefaultItemHeight;
var
  NewDefItemHeight: Integer;
begin
  if (tvoAutoItemHeight in FOptions)
  and HandleAllocated and Canvas.HandleAllocated then begin
    NewDefItemHeight:=Canvas.TextHeight(TVAutoHeightString)+2; // +2 for border
    if NewDefItemHeight<2 then NewDefItemHeight:=2;
    if (Images<>nil) and (Images.Height>NewDefItemHeight) then
      NewDefItemHeight:=Images.Height;
    if (StateImages<>nil) and (StateImages.Height>NewDefItemHeight) then
      NewDefItemHeight:=StateImages.Height;
    if Odd(NewDefItemHeight) then Inc(NewDefItemHeight);
    if NewDefItemHeight<>FDefItemHeight then begin
      FDefItemHeight:=NewDefItemHeight;
      FStates:=FStates+[tvsTopsNeedsUpdate,tvsTopItemNeedsUpdate,
                        tvsBottomItemNeedsUpdate];
      Invalidate;
    end;
  end;
end;

procedure TCustomTreeView.UpdateAllTops;
var
  CurTop: integer;

  procedure CalculateTops(Node: TTreeNode);
  begin
    while Node<>nil do begin
      if Node.FVisible then begin
        Node.fTop:=CurTop;
        inc(CurTop, Node.Height);
        if Node.Expanded then
          CalculateTops(Node.GetFirstChild);
      end;
      Node:=Node.GetNextSibling;
    end;
  end;

begin
  if not (tvsTopsNeedsUpdate in FStates) then exit;
  CurTop:=0;
  CalculateTops(Items.GetFirstVisibleNode);
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
const
  LargeItemCount = 100;
  ReservedWidth = 100;
var
  Node: TTreeNode;
  i: integer;
  FMaxTextLen: Integer;
  Cnt: Integer;
begin
  if not (tvsMaxRightNeedsUpdate in FStates) then exit;
  FMaxRight := 0;
  FMaxTextLen := 0;
  Node := Items.GetFirstNode;
  Cnt := 0;
  while Node <> nil do
  begin
    if not Node.AreParentsExpanded then
    begin
      Node := Node.GetNext;
      Continue;
    end;
    inc(Cnt);
    if (Cnt < LargeItemCount) then
    begin
      i := Node.DisplayTextRight + ScrolledLeft + Indent div 2;
    end else
    begin
      // computing DisplayTextRight is too expensive when the tree
      // has hundreds of nodes
      // => use a heuristic
      if length(Node.Text) > FMaxTextLen then
        i := Node.DisplayTextRight + ScrolledLeft + ReservedWidth
      else
        i := FMaxRight;
    end;
    if FMaxRight < i then
    begin
      FMaxRight := i;
      FMaxTextLen := length(Node.Text);
    end;
    Node := Node.GetNext;
  end;
  Exclude(FStates, tvsMaxRightNeedsUpdate);
  Include(FStates, tvsScrollbarChanged);
end;

procedure TCustomTreeView.UpdateTopItem;
begin
  //DebugLn('TCustomTreeView.UpdateTopItem tvsTopItemNeedsUpdate in FStates=',tvsTopItemNeedsUpdate in FStates);
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
  end;
end;

procedure TCustomTreeView.SetSeparatorColor(const AValue: TColor);
begin
  if fSeparatorColor=AValue then exit;
  fSeparatorColor:=AValue;
  if tvoShowSeparators in Options then
    Invalidate;
end;

procedure TCustomTreeView.SetShowButton(Value: Boolean);
begin
  if ShowButtons <> Value then begin
    if Value then
      Include(FOptions,tvoShowButtons)
    else
      Exclude(FOptions,tvoShowButtons);
    Invalidate;
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

procedure TCustomTreeView.SetMultiSelect(const AValue: Boolean);
begin
  if MultiSelect <> AValue then
  begin
    if AValue then
      Include(FOptions,tvoAllowMultiselect)
    else
      Exclude(FOptions,tvoAllowMultiselect);
  end;
end;

procedure TCustomTreeView.SetMultiSelectStyle(const AValue: TMultiSelectStyle);
begin
  if FMultiSelectStyle=AValue then exit;
  FMultiSelectStyle:=AValue;
  // there must be at least one multiselectstyle according to docs
  // http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/ComCtrls_TCustomTreeView_MultiSelectStyle.html
  if FMultiSelectStyle = [] then
    FMultiSelectStyle := DefaultMultiSelectStyle;
end;

procedure TCustomTreeView.SetReadOnly(Value: Boolean);
begin
  if ReadOnly = Value then exit;
  if Value then
    Include(FOptions,tvoReadOnly)
  else
    Exclude(FOptions,tvoReadOnly);
  if not Value then EndEditing;
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
             -(ClientHeight-ScrollBarWidth)+2*integer(BorderWidth);
    //DebugLn('>>> ',LastVisibleNode.Text,' ',Result);
    if Result<0 then Result:=0;
  end;
end;

function TCustomTreeView.GetNodeAtInternalY(Y: Integer): TTreeNode;
// search in all expanded nodes for the node at the absolute coordinate Y
var
  i: integer;
begin
  i := IndexOfNodeAtTop(Items.FTopLvlItems, Items.FTopLvlCount, Y);
  if i >= 0 then
  begin
    Result := Items.FTopLvlItems[i];
    while Result.Visible and Result.Expanded do
    begin
      i := IndexOfNodeAtTop(Result.FItems, Result.FCount, Y);
      if i >= 0 then
        Result := Result.Items[i]
      else
        break;
    end;
  end
  else
    Result := nil;
end;

function TCustomTreeView.GetNodeAtY(Y: Integer): TTreeNode;
// search in all expanded nodes for the node at the screen coordinate Y
begin
  Result := nil;
  if (Y >= BorderWidth) and (Y < (ClientHeight - ScrollBarWidth) - BorderWidth) then
  begin
    inc(Y, FScrolledTop - BorderWidth);
    Result := GetNodeAtInternalY(Y);
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
begin
  Result := nil;
  if (X >= BorderWidth) and (X < ClientWidth - BorderWidth) then
  begin
    Result := GetNodeAtY(Y);
    if (Result <> nil) and (X < Result.DisplayExpandSignLeft) then
      Result := nil;
  end;
end;

procedure TCustomTreeView.GetInsertMarkAt(X, Y: Integer;
  out AnInsertMarkNode: TTreeNode; out AnInsertMarkType: TTreeViewInsertMarkType
  );
var
  ANode: TTreeNode;
  NodeRect: TRect;
  NodeMidY: integer;
begin
  AnInsertMarkNode:=nil;
  AnInsertMarkType:=tvimNone;
  if Y<0 then Y:=0;
  if Y>=ClientHeight then Y:=ClientHeight-1;
  ANode:=GetNodeAtY(Y);
  if ANode<>nil then
  begin
    NodeRect:=ANode.DisplayRect(false);
    NodeMidY:=(NodeRect.Top+NodeRect.Bottom) div 2;
    AnInsertMarkNode:=ANode;
    if (X>AnInsertMarkNode.DisplayExpandSignRight) then
      if Y>=NodeMidY then begin
        // insert as first child of pointed node
        AnInsertMarkType:=tvimAsFirstChild;
      end else begin
        // insert as previous sibling of pointed node
        AnInsertMarkType:=tvimAsPrevSibling;
    end else begin
      if Y>=NodeMidY then begin
        if ANode.Expanded and ANode.HasChildren then begin
          // insert as first child of pointed node
          AnInsertMarkType:=tvimAsFirstChild;
        end else begin
          // insert as next sibling of pointed node
          AnInsertMarkType:=tvimAsNextSibling;
        end;
      end else begin
        // insert as previous sibling of pointed node
        AnInsertMarkType:=tvimAsPrevSibling;
      end;
    end;
  end else begin
    // insert behind all nodes
    ANode:=Items.GetLastExpandedSubNode;
    if ANode<>nil then begin
     AnInsertMarkNode:=ANode;
     if X>AnInsertMarkNode.DisplayExpandSignRight then
       // insert as first child of last visible node
       AnInsertMarkType:=tvimAsFirstChild
     else
       // insert as next sibling of last visible node
       AnInsertMarkType:=tvimAsNextSibling;
    end else begin
      // insert as new root
      AnInsertMarkNode:=nil;
      AnInsertMarkType:=tvimAsFirstChild;
    end;
  end;

  // normalize (try to replace tvimAsPrevSibling)
  if (AnInsertMarkType=tvimAsPrevSibling) and (AnInsertMarkNode<>nil) then begin
    if (AnInsertMarkNode.GetPrevSibling<>nil) then begin
      if (AnInsertMarkNode.GetPrevSibling.Expanded=false)
      and (AnInsertMarkNode.GetPrevSibling.IsVisible) then begin
        AnInsertMarkNode:=AnInsertMarkNode.GetPrevSibling;
        AnInsertMarkType:=tvimAsNextSibling;
      end;
    end else if (AnInsertMarkNode.Parent<>nil)
    and (AnInsertMarkNode.Parent.IsVisible) then begin
      AnInsertMarkNode:=AnInsertMarkNode.Parent;
      AnInsertMarkType:=tvimAsFirstChild;
    end;
  end;
end;

procedure TCustomTreeView.SetInsertMark(AnInsertMarkNode: TTreeNode;
  AnInsertMarkType: TTreeViewInsertMarkType);
begin
  InsertMarkNode:=AnInsertMarkNode;
  InsertMarkType:=AnInsertMarkType;
end;

procedure TCustomTreeView.SetInsertMarkAt(X, Y: integer);
var
  AnInsertMarkNode: TTreeNode;
  AnInsertMarkType: TTreeViewInsertMarkType;
begin
  GetInsertMarkAt(X,Y,AnInsertMarkNode,AnInsertMarkType);
  SetInsertMark(AnInsertMarkNode,AnInsertMarkType);
end;

function TCustomTreeView.GetHitTestInfoAt(X, Y: Integer): THitTests;
var Node: TTreeNode;
begin
  Result := [];
  if (X>=0) and (X<ClientWidth) and (Y>=0) and (Y<(ClientHeight-ScrollBarWidth))
  then begin
    Node:=GetNodeAtY(Y);
    if Node<>nil then begin
      Include(Result,htOnItem);
      if X<Node.DisplayExpandSignLeft then
        Include(Result,htOnIndent)
      else if X<Node.DisplayIconLeft then
        Include(Result,htOnButton)
      else if X<Node.DisplayStateIconLeft then
        Include(Result,htOnIcon)
      else if X<Node.DisplayTextLeft then
        Include(Result,htOnStateIcon)
      else if X<Node.DisplayTextRight then
        Include(Result,htOnLabel);
    end else
      Include(Result,htNowhere);
  end;
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
  Result:=(ANode<>nil) and (ANode.Visible) and (ANode.AreParentsExpanded);
  //DebugLn('[TCustomTreeView.IsNodeVisible] A Node=',DbgS(ANode),
  //' ANode.AreParentsExpanded=',ANode.AreParentsExpanded);
  if Result then begin
    //DebugLn('[TCustomTreeView.IsNodeVisible] B Node=',DbgS(ANode),
    //  ' ',dbgs(FScrolledTop)+'>=',dbgs(ANode.Top+ANode.Height)+' or =',dbgs(FScrolledTop),'+'+dbgs(ClientHeight)+'<',dbgs(ANode.Top));
    if (FScrolledTop>=ANode.Top+ANode.Height)
    or (FScrolledTop+(ClientHeight-ScrollBarWidth)-2*BorderWidth<ANode.Top)
    then
      Result:=false;
  end;
  //DebugLn('[TCustomTreeView.IsNodeVisible] END Node=',DbgS(ANode),
  //' Node.Text=',ANode.Text,' Visible=',Result);
end;

function TCustomTreeView.IsNodeHeightFullVisible(ANode: TTreeNode): Boolean;
begin
  Result:=(ANode<>nil) and (ANode.AreParentsExpanded);
  //DebugLn('[TCustomTreeView.IsNodeVisible] A Node=',DbgS(ANode),
  //' ANode.AreParentsExpanded=',ANode.AreParentsExpanded);
  if Result then begin
    //DebugLn('[TCustomTreeView.IsNodeVisible] B Node=',DbgS(ANode),
    //' ',FScrolledTop,'>=',ANode.Top,'+',ANode.Height,' or ',FScrolledTop,'+',ClientHeight,'<',ANode.Top);
    if (FScrolledTop>ANode.Top)
    or (FScrolledTop+(ClientHeight-ScrollBarWidth)-2*BorderWidth
      <ANode.Top+ANode.Height)
    then
      Result:=false;
  end;
  //DebugLn('[TCustomTreeView.IsNodeVisible] END Node=',DbgS(ANode),
  //' Node.Text=',ANode.Text,' Visible=',Result);
end;

procedure TCustomTreeView.KeyDown(var Key: Word; Shift: TShiftState);
const
  EditKey = VK_F2;
  EditKeyShift = [];
var
  I: Integer;
  lNode, tempNode: TTreeNode;
begin
  inherited KeyDown(Key, Shift);

  if (tvoAllowMultiSelect in FOptions) and (ssShift in Shift) then
    lNode := FTreeNodes.FLastMultiSelected
  else
    lNode := Selected;
    
  case Key of

  VK_DOWN:
  begin
    if lNode <> nil then 
      lNode := lNode.GetNextExpanded
    else if Items.Count > 0 then
      lNode := FTreeNodes.GetFirstNode;
    Key:=VK_UNKNOWN;
  end;

  VK_UP:
  begin
    if lNode <> nil then
      lNode := lNode.GetPrevExpanded
    else if Items.Count > 0 then
      lNode := Items.GetLastExpandedSubNode;
    Key:=VK_UNKNOWN;
  end;

  VK_LEFT:
    if lNode <> nil then
    begin
      if lNode.Expanded then
        lNode.Expanded := False
      else
      if lNode.Parent <> nil then
        lNode := lNode.Parent;
      Key:=VK_UNKNOWN;
    end;

  VK_RIGHT:
    if lNode <> nil then
    begin
      if lNode.Expanded then
        lNode := lNode.GetNextExpanded
      else
        lNode.Expanded := True;
      Key:=VK_UNKNOWN;
    end;
    
  VK_ADD:
    if lNode <> nil then 
      lNode.Expanded := True;
    
  VK_SUBTRACT:
    if lNode <> nil then 
      lNode.Expanded := False;

  VK_HOME:
    begin
      tempNode := Items.GetFirstVisibleNode;
      if tempNode<>nil then
        lNode := tempNode;
    end;

  VK_END:
    begin
      tempNode := Items.GetLastExpandedSubNode;
      if tempNode<>nil then
        lNode := tempNode;
    end;

  VK_PRIOR: // Page Up
    if lNode <> nil then
    begin
      I := Pred(ClientHeight div DefaultItemHeight);

      while (I > 0) do
        if lNode.GetPrevExpanded <> nil then
        begin
          lNode := lNode.GetPrevExpanded;
          Dec(I);
        end
        else Break;

    end;
    
  VK_NEXT: // Page Down
    if lNode <> nil then
    begin
      I := Pred(ClientHeight div DefaultItemHeight);

      while (I > 0) do
        if lNode.GetNextExpanded <> nil then
        begin
          lNode := lNode.GetNextExpanded;
          Dec(I);
        end
        else Break;

    end;

  else
    if (Key=EditKey) and (Shift=EditKeyShift) and (not ReadOnly) then
      BeginEditing(Selected);
    lNode := nil; { No change in selection }
  end;

  if lNode <> nil then
    if tvoAllowMultiSelect in FOptions then
    begin
      if ([ssCtrl,ssShift] * Shift) <> [] then
        lNode.MultiSelectGroup
      else begin
        FTreeNodes.SelectOnlyThis(lNode);
      end;
    end else
      Selected := lNode;
end;

procedure TCustomTreeView.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then FullExpand;
  UpdateDefaultItemHeight;
end;

function TCustomTreeView.GetTopItem: TTreeNode;
begin
  if HandleAllocated then
  begin
    UpdateTopItem;
    Result := FTopItem;
  end
  else
    Result := FTopItem;
end;

procedure TCustomTreeView.SetTopItem(Value: TTreeNode);
begin
  if HandleAllocated and (Value <> nil) then
  begin
    Value.MakeVisible;
    ScrolledTop:=Value.Top;
  end
  else
    FTopItem := Value;
end;

procedure TCustomTreeView.OnChangeTimer(Sender: TObject);
begin
  FChangeTimer.Enabled := False;
  //debugln('TCustomTreeView.OnChangeTimer');
  Change(FSelectedNode);
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

  if not HandleAllocated or (Items.FUpdateCount>0) then
    exit;

  if ScrolledLeft>GetMaxScrollLeft then ScrolledLeft:=GetMaxScrollLeft;
  if ScrolledTop>GetMaxScrollTop then ScrolledTop:=GetMaxScrollTop;
  Exclude(FStates,tvsScrollbarChanged);

  if fScrollBars in [ssBoth, ssHorizontal, ssAutoBoth, ssAutoHorizontal] then
  begin
    // horizontal scrollbar
    FillChar(ScrollInfo,SizeOf(ScrollInfo),0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPage := Max(1,(ClientWidth-ScrollBarWidth)-2*BorderWidth);
    ScrollInfo.nMax := Max(1,GetMaxScrollLeft+integer(ScrollInfo.nPage)-1);
    ScrollInfo.nPos := Max(FScrolledLeft,0);
    //DebugLn(['TCustomTreeView.UpdateScrollbars nPage=',ScrollInfo.nPage,',nMax=',ScrollInfo.nMax,' fScrollBars=',ord(fScrollBars)]);
    if not CompareMem(@ScrollInfo,@FLastHorzScrollInfo,SizeOf(TScrollInfo))
    then begin
      if (fScrollBars in [ssAutoBoth, ssAutoHorizontal])
      and (ScrollInfo.nPage>=cardinal(ScrollInfo.nMax)) then begin
        FLastHorzScrollInfo.cbSize:=0;
        ShowScrollBar(Handle, SB_HORZ, false);
      end else begin
        FLastHorzScrollInfo:=ScrollInfo;
        ShowScrollBar(Handle, SB_HORZ, true);
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, true);
      end;
    end;
    //DebugLn('>>>>>>>>>> [TCustomTreeView.UpdateScrollbars] nMin=',ScrollInfo.nMin,
    //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
    //' nPos=',ScrollInfo.nPos,' GetMaxScrollLeft=',GetMaxScrollLeft,
    //' ClientW=',ClientWidth,
    //' MaxRight=',FMaxRight
    //);
  end else begin
    FLastHorzScrollInfo.cbSize:=0;
    ShowScrollBar(Handle,SB_HORZ,false);
  end;

  if fScrollBars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical] then begin
    // vertical scrollbar
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPage := Max(1,(ClientHeight-ScrollBarWidth)-FDefItemHeight);
    ScrollInfo.nMax := Max(1,GetMaxScrollTop+integer(ScrollInfo.nPage)-1);
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nPos := Max(0,FScrolledTop);
    if not CompareMem(@ScrollInfo,@FLastVertScrollInfo,SizeOf(TScrollInfo))
    then begin
      if (fScrollBars in [ssAutoBoth, ssAutoVertical])
      and (ScrollInfo.nPage>=cardinal(ScrollInfo.nMax)) then begin
        FLastVertScrollInfo.cbSize:=0;
        ShowScrollBar(Handle, SB_VERT, false);
      end else begin
        FLastVertScrollInfo:=ScrollInfo;
        ShowScrollBar(Handle, SB_VERT, true);
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
      end;
    end;
    //DebugLn('>>>>>>>>>> [TCustomTreeView.UpdateScrollbars] Vert On nMin=',dbgs(ScrollInfo.nMin),
    //' nMax=',dbgs(ScrollInfo.nMax),' nPage=',dbgs(ScrollInfo.nPage),
    //' nPos=',dbgs(ScrollInfo.nPos),' GetMaxScrollTop=',dbgs(GetMaxScrollTop));
  end else begin
    //DebugLn('>>>>>>>>>> [TCustomTreeView.UpdateScrollbars] Vert Off ');
    FLastVertScrollInfo.cbSize:=0;
    ShowScrollBar(Handle,SB_VERT,false);
  end;
end;

function TCustomTreeView.GetSelection: TTreeNode;
begin
  if RightClickSelect and Assigned(FRClickNode) then
    Result := FRClickNode
  else
    Result := FSelectedNode;
end;

function TCustomTreeView.GetSelectionCount: Cardinal;
begin
  Result := Items.SelectionCount;
end;

function TCustomTreeView.GetSelections(AIndex: Integer): TTreeNode;
begin
  if (AIndex >= 0) and (AIndex < Items.SelectionCount) then
    Result := Items.GetSelections(AIndex)
  else
    Result := nil;
end;

procedure TCustomTreeView.SetSelection(Value: TTreeNode);
var
  OldNode: TTreeNode;
begin
  if FSelectedNode = Value then Exit;
  if not CanChange(FSelectedNode) then
    exit;
  EndEditing(true); // end editing before FSelectedNode change
  OldNode := FSelectedNode;
  FSelectedNode := Value;
  if Assigned(OldNode) then
    OldNode.Selected := False;
  if Assigned(Value) then
  begin
    Value.Selected := True;
    Value.MakeVisible;
  end;
  InternalSelectionChanged;
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

procedure TCustomTreeView.UpdateExpandSignSize;
var
  Details: TThemedElementDetails;
begin
  if ExpandSignType = tvestTheme then
  begin
    Details := ThemeServices.GetElementDetails(ttGlyphOpened);
    FExpandSignSize := ThemeServices.GetDetailSize(Details).cx;
  end
  else
    FExpandSignSize := 9;
end;

procedure TCustomTreeView.SetExpandSignType(Value: TTreeViewExpandSignType);
begin
  if Value <> FExpandSignType then
  begin
    FExpandSignType := Value;
    UpdateExpandSignSize;
    Invalidate;
  end;
end;

procedure TCustomTreeView.SetDefaultItemHeight(Value: integer);
begin
  if (tvoAutoItemHeight in FOptions) and (not (csLoading in ComponentState))
  then exit;
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

function TCustomTreeView.GetDropTarget: TTreeNode;
begin
  if HandleAllocated then
  begin
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

function TCustomTreeView.GetMultiSelect: Boolean;
begin
  Result := (tvoAllowMultiSelect in FOptions);
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

function TCustomTreeView.IsEditing: Boolean;
begin
  Result:=tvsIsEditing in FStates;
end;

function TCustomTreeView.GetDragImages: TDragImageList;
begin
  if FDragImage.Count > 0 then
    Result := FDragImage
  else
    Result := nil;
end;

procedure TCustomTreeView.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);
end;

procedure TCustomTreeView.UpdateInsertMark(X,Y: integer);
begin
  if (tvoAutoInsertMark in Options) and (not (csDesigning in ComponentState))
  then
    SetInsertMarkAt(X,Y)
  else
    SetInsertMark(nil,tvimNone);
end;

procedure TCustomTreeView.DoSelectionChanged;
var
  lAccessibleObject: TLazAccessibleObject;
  lSelection: TTreeNode;
begin
  // Update the accessibility information
  lAccessibleObject := GetAccessibleObject();
  lSelection := Self.Selected;
  if lSelection = nil then lAccessibleObject.SelectedText := ''
  else lAccessibleObject.SelectedText := lSelection.Text;
  lAccessibleObject.SetAccesibilityFields(lAccessibleObject.AccessibleDescription,
    lSelection.Text, lAccessibleObject.AccessibleRole);
  lAccessibleObject.SendNotification(lanSelectedTextChanged);

  if Assigned(OnSelectionChanged) then OnSelectionChanged(Self);
end;

function TCustomTreeView.IsInsertMarkVisible: boolean;
begin
  Result:=(FInsertMarkType<>tvimNone) and (FInsertMarkNode<>nil)
           and (FInsertMarkNode.IsVisible);
end;

procedure TCustomTreeView.DoStartDrag(var DragObject: TDragObject);
var
  P: TPoint;
begin
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.DoStartDrag A ',Name,':',ClassName);
  {$ENDIF}
  inherited DoStartDrag(DragObject);
  FLastDropTarget := nil;
  if FDragNode = nil then begin
    GetCursorPos(P);
    with ScreenToClient(P) do FDragNode := GetNodeAt(X, Y);
    {$IFDEF VerboseDrag}
    if FDragNode<>nil then
      DebugLn('TCustomTreeView.DoStartDrag DragNode=',FDragNode.Text)
    else
      DebugLn('TCustomTreeView.DoStartDrag DragNode=nil');
    {$ENDIF}
  end;
end;

procedure TCustomTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.DoEndDrag A ',Name,':',ClassName);
  {$ENDIF}
  inherited DoEndDrag(Target, X, Y);
  FLastDropTarget := nil;
end;

function TCustomTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    ScrolledTop := ScrolledTop -
              (WheelDelta * Mouse.WheelScrollLines * DefaultItemHeight) div 120;
    Result := true;
  end;
end;

function TCustomTreeView.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT;
begin
  Result:=inherited;
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.DoDragMsg ',Name,':',ClassName,' ',IntToStr(ord(ADragMessage)));
  {$ENDIF}
  case ADragMessage of
    {dmDragMove:
      begin
        P:=ScreenToClient(Pos);
        DoDragOver(Source, P.X, P.Y, AMessage.Result <> 0);
      end;}
    dmDragLeave:
      begin
        ADragObject.HideDragImage;
        FLastDropTarget := DropTarget;
        DropTarget := nil;
        ADragObject.ShowDragImage;
      end;
    dmDragDrop: FLastDropTarget := nil;
  end;
end;

procedure TCustomTreeView.DragOver(Source: TObject; X,Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  inherited DragOver(Source,X,Y,State,Accept);
  Node := GetNodeAt(X, Y);
  {$IFDEF VerboseDrag}
  DebugLn(['TCustomTreeView.DragOver ',Name,':',ClassName,' ',Node<>nil,' ',Node <> DropTarget,' ',Node = FLastDropTarget]);
  DebugLn(['TCustomTreeView.DragOver Source ',Source,':',Source.ClassName]);
  {$ENDIF}
  if (Node <> nil)
  and ((Node <> DropTarget) or (Node = FLastDropTarget)) then
  begin
    FLastDropTarget := nil;
    Node.DropTarget := True;
  end;
end;

procedure TCustomTreeView.DoPaint;
var
  a,HalfBorderWidth:integer;
  SpaceRect, DrawRect: TRect;
  Node: TTreeNode;
  InsertMarkRect: TRect;
begin
  if [tvsUpdating,tvsPainting] * FStates <> [] then Exit;
  Include(FStates, tvsPainting);
  try
    if (tvoAutoItemHeight in fOptions) then
      UpdateDefaultItemHeight;
    UpdateScrollbars;
    with Canvas do
    begin
      if Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) then
      begin
        DrawRect := ClientRect;
        if not CustomDraw(DrawRect, cdPrePaint) then exit;
      end;
      // draw nodes
      Node := TopItem;
      //write('[TCustomTreeView.DoPaint] A Node=',DbgS(Node));
      //if Node<>nil then DebugLn(' Node.Text=',Node.Text) else DebugLn('');
      while Node <> nil do
      begin
        if Node.Visible then
          DoPaintNode(Node);
        Node := Node.GetNextVisible;
        //write('[TCustomTreeView.DoPaint] B Node=',DbgS(Node));
        //if Node<>nil then DebugLn(' Node.Text=',Node.Text) else DebugLn('');
      end;
      SpaceRect := Rect(BorderWidth, BorderWidth,
                        (ClientWidth - ScrollBarWidth) - BorderWidth,
                        (ClientHeight - ScrollBarWidth) - BorderWidth);
      // draw insert mark for new root node
      if (InsertMarkType = tvimAsFirstChild) and (Items.Count = 0) then
      begin
        Pen.Color := FTreeLineColor;
        Brush.Color := FSelectedColor;
        InsertMarkRect := SpaceRect;
        InsertMarkRect.Bottom := InsertMarkRect.Top + 2;
        Rectangle(InsertMarkRect);
        SpaceRect.Top := InsertMarkRect.Bottom;
      end;
      // draw unused space below nodes
      Node := BottomItem;
      if Node <> nil then
        SpaceRect.Top := Node.Top + Node.Height - FScrolledTop + BorderWidth;
      //if Node<>nil then DebugLn('BottomItem=',BottomItem.text) else DebugLn('NO BOTTOMITEM!!!!!!!!!');
      // TWinControl(Parent).InvalidateRect(Self,SpaceRect,true);
      if (FBackgroundColor <> clNone) and (SpaceRect.Top < SpaceRect.Bottom) then
      begin
        //DebugLn('  SpaceRect=',SpaceRect.Left,',',SpaceRect.Top,',',SpaceRect.Right,',',SpaceRect.Bottom);
        Brush.Color := FBackgroundColor;
        FillRect(SpaceRect);
      end;
      // draw border
      HalfBorderWidth := BorderWidth shr 1;
      Pen.Color := clGray;
      for a := 0 to BorderWidth - 1 do
      begin
        if a = HalfBorderWidth then
          Pen.Color := clBlack;
        MoveTo(a, (ClientHeight-ScrollBarWidth) - 1 - a);
        LineTo(a, a);
        LineTo((ClientWidth - ScrollBarWidth) - 1 - a, a);
      end;
      Pen.Color := clWhite;
      for a := 0 to BorderWidth - 1 do
      begin
        if a = HalfBorderWidth then
          Pen.Color := clLtGray;
        MoveTo((ClientWidth - ScrollBarWidth) - 1 - a, a);
        LineTo((ClientWidth - ScrollBarWidth) - 1 - a, (ClientHeight - ScrollBarWidth) - 1 - a);
        LineTo(a, (ClientHeight-ScrollBarWidth) - 1 - a);
      end;
      if Assigned(FOnCustomDraw) or Assigned(FOnAdvancedCustomDraw) then
      begin
        DrawRect := ClientRect;
        if not CustomDraw(DrawRect, cdPostPaint) then exit;
      end;
    end;
  finally
    Exclude(FStates, tvsPainting);
  end;
end;

procedure TCustomTreeView.DoPaintNode(Node: TTreeNode);
var
  NodeRect: TRect;
  VertMid: integer;
  NodeSelected, HasExpandSign: boolean;

  function InvertColor(AColor: TColor): TColor;
  var Red, Green, Blue: integer;
  begin
    if AColor<>clHighlight then begin
      Result:=clWhite;
      Red:=(AColor shr 16) and $ff;
      Green:=(AColor shr 8) and $ff;
      Blue:=AColor and $ff;
      if Red+Green+Blue>$180 then
        Result:=clBlack;
      //DebugLn(['[TCustomTreeView.DoPaintNode.InvertColor] Result=',Result,' ',Red,',',Green,',',Blue]);
    end
    else
      Result := clHighlightText;
  end;

  procedure DrawVertLine(X, Y1, Y2: Integer);
  begin
    if Y1 > Y2 then
      Exit;
    if TreeLinePenStyle = psPattern then
    begin
      // TODO: implement psPattern support in the LCL
      while Y1 < Y2 do
      begin
        Canvas.Pixels[X, Y1] := TreeLineColor;
        inc(Y1, 2);
      end;
    end
    else
    begin
      Canvas.MoveTo(X, Y1);
      Canvas.LineTo(X, Y2);
    end;
  end;

  procedure DrawHorzLine(Y, X1, X2: Integer);
  begin
    if X1 > X2 then
      Exit;
    if TreeLinePenStyle = psPattern then
    begin
      // TODO: implement psPattern support in the LCL
      while X1 < X2 do
      begin
        Canvas.Pixels[X1, Y] := TreeLineColor;
        inc(X1, 2);
      end;
    end
    else
    begin
      Canvas.MoveTo(X1, Y);
      Canvas.LineTo(X2, Y);
    end;
  end;

  function DrawTreeLines(CurNode: TTreeNode): integer;
  // paints tree lines, returns indent
  var
    CurMid: integer;
  begin
    if (CurNode <> nil) and ((tvoShowRoot in Options) or (CurNode.Parent<>nil)) then
    begin
      Result := DrawTreeLines(CurNode.Parent);
      if ShowLines then
      begin
        CurMid := Result + (Indent shr 1);
        if CurNode = Node then
        begin
          // draw horizontal line
          if HasExpandSign then
            DrawHorzLine(VertMid, CurMid + FExpandSignSize div 2, Result + Indent)
          else
            DrawHorzLine(VertMid, CurMid, Result + Indent);
        end;

        if (CurNode.GetNextVisibleSibling <> nil) then
        begin
          // draw vertical line to next brother
          if (CurNode = Node) and HasExpandSign then
          begin
            if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
              DrawVertLine(CurMid, VertMid + FExpandSignSize div 2, NodeRect.Bottom)
            else
            begin
              DrawVertLine(CurMid, NodeRect.Top, Max(NodeRect.Top, VertMid - FExpandSignSize div 2));
              DrawVertLine(CurMid, VertMid + FExpandSignSize div 2, NodeRect.Bottom - 1);
            end;
          end
          else
          if (Node.Parent = nil) and (Node.GetPrevSibling = nil) then
            DrawVertLine(CurMid, VertMid, NodeRect.Bottom)
          else
            DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom);
        end else
        if (CurNode = Node) then
        begin
          // draw vertical line from top to horizontal line
          if HasExpandSign then
          begin
            if ((InsertMarkNode = Node) and (InsertMarkType = tvimAsNextSibling)) then
            begin
              DrawVertLine(CurMid, NodeRect.Top, Max(NodeRect.Top, VertMid - FExpandSignSize div 2));
              DrawVertLine(CurMid, VertMid + FExpandSignSize div 2, NodeRect.Bottom - 1);
            end
            else
              DrawVertLine(CurMid, NodeRect.Top, VertMid - FExpandSignSize div 2);
          end
          else
          if ((InsertMarkNode = Node) and (InsertMarkType = tvimAsNextSibling)) then
            DrawVertLine(CurMid, NodeRect.Top, NodeRect.Bottom - 1)
          else
            DrawVertLine(CurMid, NodeRect.Top, VertMid);
        end;
      end;
      inc(Result, Indent);
    end else
    begin
      Result := BorderWidth - FScrolledLeft;
      if CurNode <> nil then // indent first level of tree with ShowRoot = false a bit
        inc(Result, Indent shr 2);
    end;
  end;

  procedure DrawExpandSign(MidX, MidY: integer; CollapseSign: boolean);
  const
    PlusMinusDetail: array[Boolean {Hot}, Boolean {Expanded}] of TThemedTreeview =
    (
      (ttGlyphClosed, ttGlyphOpened),
      (ttHotGlyphClosed, ttHotGlyphOpened)
    );
  var
    HalfSize, ALeft, ATop, ARight, ABottom: integer;
    Points: PPoint;
    Details: TThemedElementDetails;
    R: TRect;
  begin
    with Canvas do
    begin
      Pen.Color := FExpandSignColor;
      Pen.Style := psSolid;
      HalfSize := FExpandSignSize shr 1;
      if ((FExpandSignSize and 1) = 0) then
        dec(HalfSize);
      ALeft := MidX - HalfSize;
      ATop := MidY - HalfSize;
      ARight := ALeft + FExpandSignSize;
      ABottom := ATop + FExpandSignSize;
      case ExpandSignType of
      tvestTheme:
        begin
          // draw a themed expand sign. Todo: track hot
          R := Rect(ALeft, ATop, ARight + 1, ABottom + 1);
          Details := ThemeServices.GetElementDetails(PlusMinusDetail[False, CollapseSign]);
          ThemeServices.DrawElement(Canvas.Handle, Details, R, nil);
        end;
      tvestPlusMinus:
        begin
          // draw a plus or a minus sign
          R := Rect(ALeft, ATop, ARight, ABottom);
          Rectangle(R);
          MoveTo(R.Left + 2, MidY);
          LineTo(R.Right - 2, MidY);
          if not CollapseSign then
          begin
            MoveTo(MidX, R.Top + 2);
            LineTo(MidX, R.Bottom - 2);
	  end;
        end;
      tvestArrow:
        begin
          // draw an arrow. down for collapse and right for expand
          R := Rect(ALeft, ATop, ARight, ABottom);
          GetMem(Points, SizeOf(TPoint) * 3);
          if CollapseSign then
          begin
            // draw an arrow down
            Points[0] := Point(R.Left, MidY);
            Points[1] := Point(R.Right - 1, MidY);
            Points[2] := Point(MidX, R.Bottom - 1);
          end else
          begin
            // draw an arrow right
            Points[0] := Point(MidX - 1, ATop);
            Points[1] := Point(R.Right - 2, MidY);
            Points[2] := Point(MidX - 1, R.Bottom - 1);
          end;
          Polygon(Points, 3, False);
          FreeMem(Points);
        end;
      end;
    end;
  end;

  procedure DrawInsertMark;
  var
    InsertMarkRect: TRect;
    x: Integer;
  begin
    case InsertMarkType of

    tvimAsFirstChild:
      if InsertMarkNode=Node then begin
        // draw insert mark for new first child
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          // Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignRight+Indent div 2;
          MoveTo(x,NodeRect.Bottom-3);
          LineTo(x,NodeRect.Bottom-2);
          x:=Node.DisplayExpandSignRight+Indent;
          LineTo(x,NodeRect.Bottom-2);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color:=TreeLineColor;
          Brush.Color:=FSelectedColor;
          InsertMarkRect:=Rect(x,NodeRect.Bottom-3,
                               NodeRect.Right,NodeRect.Bottom-1);
          Rectangle(InsertMarkRect);
        end;
      end;

    tvimAsPrevSibling:
      if InsertMarkNode=Node then begin
        // draw insert mark for new previous sibling
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          //Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignLeft+Indent div 2;
          MoveTo(x,NodeRect.Top+1);
          x:=Node.DisplayExpandSignRight;
          LineTo(x,NodeRect.Top+1);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color:=TreeLineColor;
          Brush.Color:=FSelectedColor;
          InsertMarkRect:=Rect(x,NodeRect.Top,
                               NodeRect.Right,NodeRect.Top+2);
          Rectangle(InsertMarkRect);
        end;
      end;

    tvimAsNextSibling:
      if InsertMarkNode=Node then begin
        // draw insert mark for new next sibling
        with Canvas do begin
          // draw virtual tree line
          Pen.Color:=TreeLineColor;
          //Pen.Style:=TreeLinePenStyle; ToDo: not yet implemented in all widgetsets
          x:=Node.DisplayExpandSignLeft+Indent div 2;
          MoveTo(x,NodeRect.Bottom-3);
          LineTo(x,NodeRect.Bottom-2);
          x:=Node.DisplayExpandSignRight;
          LineTo(x,NodeRect.Bottom-2);
          Pen.Style:=psSolid;

          // draw virtual rectangle
          Pen.Color:=TreeLineColor;
          Brush.Color:=FSelectedColor;
          InsertMarkRect:=Rect(x,NodeRect.Bottom-3,
                               NodeRect.Right,NodeRect.Bottom-1);
          Rectangle(InsertMarkRect);
        end;
      end;

    end;
  end;

  procedure DrawBackground(IsSelected: Boolean; ARect: TRect);
  var
    Details: TThemedElementDetails;
    CurBackgroundColor: TColor;
  begin
    if (tvoRowSelect in Options) and IsSelected then
      if tvoThemedDraw in Options then
      begin
        if Focused then
          Details := ThemeServices.GetElementDetails(ttItemSelected)
        else
          Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
        if ThemeServices.HasTransparentParts(Details) then
        begin
          Canvas.Brush.Color := BackgroundColor;
          Canvas.FillRect(ARect);
        end;
        ThemeServices.DrawElement(Canvas.Handle, Details, ARect, nil);
        Exit;
      end
      else
        CurBackgroundColor := FSelectedColor
    else
      CurBackgroundColor := FBackgroundColor;
    if CurBackgroundColor <> clNone then
    begin
      Canvas.Brush.Color := CurBackgroundColor;
      Canvas.FillRect(ARect);
    end;
  end;

  procedure DrawNodeText(IsSelected: Boolean; NodeRect: TRect; AText: String);
  var
    Details: TThemedElementDetails;
  begin
    if IsSelected then
    begin
      if Focused then
        Details := ThemeServices.GetElementDetails(ttItemSelected)
      else
        Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
      if not (tvoRowSelect in Options) then
        if (tvoThemedDraw in Options) then
          ThemeServices.DrawElement(Canvas.Handle, Details, NodeRect, nil)
        else
        begin
          Canvas.Brush.Color := FSelectedColor;
          Canvas.Font.Color := InvertColor(Brush.Color);
          Canvas.FillRect(NodeRect);
        end
      else
      if not (tvoThemedDraw in Options) then
        Canvas.Font.Color := Font.Color;
    end
    else
      Details := ThemeServices.GetElementDetails(ttItemNormal);

    if (tvoThemedDraw in Options) then
      ThemeServices.DrawText(Canvas, Details, AText, NodeRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX, 0)
    else
      DrawText(Canvas.Handle, PChar(AText), -1, NodeRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
  end;


var
  x, ImgIndex: integer;
  CurTextRect: TRect;
  DrawState: TCustomDrawState;
  PaintImages: boolean;
begin
  NodeRect := Node.DisplayRect(False);
  if (NodeRect.Bottom < 0) or (NodeRect.Top >= (ClientHeight - ScrollBarWidth)) then
    Exit;
  NodeSelected := (Node.Selected) or (Node.MultiSelected);
  Canvas.Font.Color := Font.Color;
  if Assigned(OnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) then
  begin
    DrawState := [];
    if NodeSelected then
      Include(DrawState, cdsSelected);
    if Node.Focused then
      Include(DrawState, cdsFocused);
    if Node.MultiSelected then
      Include(DrawState, cdsMarked);
    if not CustomDrawItem(Node, DrawState, cdPrePaint, PaintImages) then Exit;
  end
  else
    PaintImages := True;

  VertMid := (NodeRect.Top + NodeRect.Bottom) div 2;
  HasExpandSign := ShowButtons and Node.HasChildren and ((tvoShowRoot in Options) or (Node.Parent <> nil));
  //DebugLn(['[TCustomTreeView.DoPaintNode] Node=',DbgS(Node),' Node.Text=',Node.Text,' NodeRect=',NodeRect.Left,',',NodeRect.Top,',',NodeRect.Right,',',NodeRect.Bottom,' VertMid=',VertMid]);
  with Canvas do
  begin
    // draw background
    DrawBackground(NodeSelected, NodeRect);

    // draw tree lines
    Pen.Color := TreeLineColor;
    Pen.Style := TreeLinePenStyle;
    x := DrawTreeLines(Node);
    Pen.Style := psSolid;

    // draw expand sign
    if HasExpandSign then
      DrawExpandSign(x - Indent + (Indent shr 1), VertMid, Node.Expanded);

    // draw icon
    if (Images <> nil) and PaintImages then
    begin
      if FSelectedNode <> Node then
      begin
      	GetImageIndex(Node);
        ImgIndex := Node.ImageIndex
      end
      else
      begin
      	GetSelectedIndex(Node);
        ImgIndex := Node.SelectedIndex;
      end;
      if (ImgIndex >= 0) and (ImgIndex < Images.Count) then
        Images.Draw(Canvas, x + 1, (NodeRect.Top + NodeRect.Bottom - Images.Height) div 2,
            ImgIndex, Node.FNodeEffect);
      inc(x, Images.Width + 2);
    end;

    // draw state icon
    if (StateImages <> nil) and PaintImages then
    begin
      if (Node.StateIndex >= 0) and (Node.StateIndex < StateImages.Count) then
        StateImages.Draw(Canvas, x + 1, (NodeRect.Top + NodeRect.Bottom - StateImages.Height) div 2,
          Node.StateIndex, True);
      inc(x, StateImages.Width + 2);
    end;

    // draw text
    if Node.Text <> '' then
    begin
      CurTextRect := NodeRect;
      CurTextRect.Left := x;
      CurTextRect.Right := x + TextWidth(Node.Text) + Indent div 2;
      DrawNodeText(NodeSelected, CurTextRect, Node.Text);
    end;

    // draw separator
    if (tvoShowSeparators in FOptions) then
    begin
      Pen.Color:=SeparatorColor;
      MoveTo(NodeRect.Left,NodeRect.Bottom-1);
      LineTo(NodeRect.Right,NodeRect.Bottom-1);
    end;

    // draw insert mark
    DrawInsertMark;
  end;
  if Assigned(OnCustomDrawItem) or Assigned(FOnAdvancedCustomDrawItem) then
  begin
    DrawState:=[];
    if Node.Selected then
      Include(DrawState,cdsSelected);
    if Node.Focused then
      Include(DrawState,cdsFocused);
    if Node.MultiSelected then
      Include(DrawState,cdsMarked);
    if not CustomDrawItem(Node,DrawState,cdPostPaint,PaintImages) then exit;
  end
  else
    PaintImages := true;
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
  if Assigned(Node) and Assigned(FOnChanging) then FOnChanging(Self, Node, Result);
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

procedure TCustomTreeView.EndEditing(Cancel: boolean);
var
  NewText: String;
  Node: TTreeNode;
begin
  //DebugLn(['TCustomTreeView.EndEditing ',DbgSName(Self),' ',tvsIsEditing in FStates,' ',DbgSName(FEditor)]);
  if not (tvsIsEditing in FStates) then exit;
  Exclude(FStates,tvsIsEditing);
  if FEditor<>nil then begin
    // get new value fom edit control and hide it
    NewText:='';
    if not Cancel then
      NewText:=FEditor.Text;
    FEditor.Parent:=nil;

    // commit new value
    if not Cancel then begin
      Node:=FEditingItem;
      if (Node<>nil) then begin
        if Assigned(OnEdited) then
          OnEdited(Self,Node,NewText);
        Node.Text:=NewText;
      end;
    end;
    if Assigned(FOnEditingEnd) then FOnEditingEnd(Self, FEditingItem, Cancel);
  end;
  FEditingItem := nil;
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
var
  NewNodeClass: TTreeNodeClass;
begin
  Result := nil;
  if Assigned(FOnCustomCreateItem) then
    FOnCustomCreateItem(Self, Result);
  if Result = nil then
  begin
    NewNodeClass:=TTreeNode;
    if Assigned(OnCreateNodeClass) then
      OnCreateNodeClass(Self,NewNodeClass);
    Result := NewNodeClass.Create(Items);
  end;
end;

procedure TCustomTreeView.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CursorNode: TTreeNode;
  LogicalX: Integer;
begin
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.MouseDown A ',Name,':',ClassName,' ');
  {$ENDIF}
  fMouseDownPos := Point(X,Y);
  Exclude(FStates,tvsEditOnMouseUp);
  if Button=mbRight then begin
    if RightClickSelect then begin
      Selected:=GetNodeAt(X,Y);
    end;
  end;

  if not Focused and CanFocus then
    SetFocus;

  inherited MouseDown(Button, Shift, X, Y);

  CursorNode := GetNodeAt(X, Y);
  LogicalX:=X;
  if ([ssDouble, ssTriple, ssQuad] * Shift) = [] then
  begin
    if (Button = mbLeft) and (CursorNode <> nil) then
    begin
      if CursorNode.HasChildren and ShowButtons and
         (LogicalX >= CursorNode.DisplayExpandSignLeft) and
         (LogicalX < CursorNode.DisplayExpandSignRight) then
      begin
        // mousedown occured on expand sign -> expand/collapse
        CursorNode.Expanded:=not CursorNode.Expanded;
      end else
      if LogicalX >= CursorNode.DisplayIconLeft then
      begin
        // mousedown occured in text or icon
        // -> select node and begin drag operation
        {$IFDEF VerboseDrag}
        DebugLn(['TCustomTreeView.MouseDown In Text ',Name,':',ClassName,' MouseCapture=',MouseCapture]);
        {$ENDIF}
        if (Selected = CursorNode) and (LogicalX >= CursorNode.DisplayTextLeft) then
          Include(FStates, tvsEditOnMouseUp);
        if not (tvoAllowMultiselect in Options) then
          Selected := CursorNode
        else
        begin
          if (ssShift in Shift) then
          begin
            Exclude(FStates,tvsEditOnMouseUp);
            LockSelectionChangeEvent;
            try
              Items.ClearMultiSelection;
              CursorNode.MultiSelectGroup;
            finally
              UnlockSelectionChangeEvent;
            end;
          end
          else
          if (ssCtrl in Shift) then
          begin
            Exclude(FStates,tvsEditOnMouseUp);
            CursorNode.MultiSelected:=not CursorNode.MultiSelected;
          end
          else
          begin
            if (Selected <> CursorNode) or Items.IsMultiSelection then
              Items.SelectOnlyThis(CursorNode);
          end;
        end;
      end;
    end;
  end else
  if not (tvoNoDoubleClickExpand in Options) and (ssDouble in Shift)
     and (Button = mbLeft) and (CursorNode<>nil) then
    CursorNode.Expanded := not CursorNode.Expanded;
end;

procedure TCustomTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, x, y);
  if (tvoAutoInsertMark in FOptions) then
    UpdateInsertMark(X,Y);
end;

procedure TCustomTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
    exit;
  if Button=mbLeft then
    MouseCapture := False;
  if (Button=mbLeft)
  and (FStates * [tvsDblClicked, tvsTripleClicked, tvsQuadClicked] = [])
  then begin
    //AquirePrimarySelection;
    if (tvsEditOnMouseUp in FStates) and (not ReadOnly)
    and (abs(fMouseDownPos.X-X)+abs(fMouseDownPos.Y-Y)<10)
    and (GetNodeAt(fMouseDownPos.X,fMouseDownPos.Y)=GetNodeAt(X,Y)) then
      BeginEditing(Selected);
  end;
  FStates:=FStates-[tvsDblClicked,tvsTripleClicked,tvsQuadClicked,tvsEditOnMouseUp];
end;

procedure TCustomTreeView.Notification(AComponent: TComponent; Operation: TOperation);
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
    if DefaultItemHeight<Images.Height+2 then
      DefaultItemHeight:=Images.Height+2;
  end;
  Invalidate;
end;

procedure TCustomTreeView.SetInsertMarkNode(const AValue: TTreeNode);
var
  InvalidateNeeded: Boolean;
begin
  if FInsertMarkNode=AValue then exit;
  InvalidateNeeded:=IsInsertMarkVisible;
  FInsertMarkNode:=AValue;
  InvalidateNeeded:=InvalidateNeeded or IsInsertMarkVisible;
  if InvalidateNeeded then Invalidate;
end;

procedure TCustomTreeView.SetInsertMarkType(
  const AValue: TTreeViewInsertMarkType);
var
  InvalidateNeeded: Boolean;
begin
  if FInsertMarkType=AValue then exit;
  InvalidateNeeded:=IsInsertMarkVisible;
  FInsertMarkType:=AValue;
  InvalidateNeeded:=InvalidateNeeded or IsInsertMarkVisible;
  if InvalidateNeeded then Invalidate;
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
    if DefaultItemHeight<StateImages.Height+2 then
      DefaultItemHeight:=StateImages.Height+2;
  end;
  Invalidate;
end;

procedure TCustomTreeView.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(UTF8ToSys(FileName), fmOpenRead or fmShareDenyWrite);
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
  Stream := TFileStream.Create(UTF8ToSys(FileName), fmCreate);
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

procedure TCustomTreeView.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrolledTop := 0;
    SB_BOTTOM:     ScrolledTop := GetMaxScrollTop;
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefItemHeight;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefItemHeight;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + (ClientHeight-ScrollBarWidth)
                                     - FDefItemHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - (ClientHeight-ScrollBarWidth)
                                     + FDefItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledTop := Msg.Pos;

    SB_ENDSCROLL: ; // Ends scrolling
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
      
    SB_ENDSCROLL: ;// Ends scrolling
  end;
end;

procedure TCustomTreeView.WMLButtonDown(var AMessage: TLMLButtonDown);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.WMLButtonDown A ',Name,':',ClassName,' ');
  {$ENDIF}
  Exclude(FStates,tvsDragged);
  inherited WMLButtonDown(AMessage);
  {$IFDEF VerboseDrag}
  DebugLn('TCustomTreeView.WMLButtonDown END ',Name,':',ClassName,' ');
  {$ENDIF}
end;

procedure TCustomTreeView.WMNotify(var AMessage: TLMNotify);
begin
  inherited WMNotify(AMessage);
end;

procedure TCustomTreeView.WMSetFocus(var Message: TLMSetFocus);
begin
  Invalidate;
  inherited;
end;

procedure TCustomTreeView.WMKillFocus(var Message: TLMKillFocus);
begin
  Invalidate;
  inherited;
end;

procedure TCustomTreeView.Resize;
begin
  FStates:=FStates+[tvsScrollbarChanged,
                    tvsBottomItemNeedsUpdate];
  inherited Resize;
end;

function TCustomTreeView.GetSelectedChildAccessibleObject: TLazAccessibleObject;
var
  lNode: TTreeNode;
begin
  Result := nil;
  lNode := GetSelection();
  if lNode = nil then Exit;
  Result := FAccessibleObject.GetChildAccessibleObjectWithDataObject(lNode);
end;

function TCustomTreeView.GetChildAccessibleObjectAtPos(APos: TPoint): TLazAccessibleObject;
var
  lNode: TTreeNode;
begin
  Result := nil;
  lNode := GetNodeAt(APos.X, APos.Y);
  //if lNode = nil then DebugLn('[TCustomTreeView.GetChildAccessibleObjectAtPos] lNode=nil')
  //else DebugLn('[TCustomTreeView.GetChildAccessibleObjectAtPos] lNode=' + lNode.Text);
  if lNode = nil then Exit;
  Result := FAccessibleObject.GetChildAccessibleObjectWithDataObject(lNode);
end;

procedure TCustomTreeView.InternalSelectionChanged;
begin
  if FSelectionChangeEventLock > 0 then
    Include(FStates, tvsSelectionChanged)
  else
  begin
    Exclude(FStates, tvsSelectionChanged);
    DoSelectionChanged;
    FChangeTimer.Enabled := False;
    FChangeTimer.Enabled := True;
    //debugln('TCustomTreeView.InternalSelectionChanged');
  end;
end;

class procedure TCustomTreeView.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomTreeView;
end;

class function TCustomTreeView.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 121;
  Result.CY := 97;
end;

procedure TCustomTreeView.Added(Node: TTreeNode);
begin
  if Assigned(OnAddition) then OnAddition(Self,Node);
end;

{ CustomDraw support }

procedure TCustomTreeView.EditorEditingDone(Sender: TObject);
var
  WasFocused: Boolean;
begin
  WasFocused := (FEditor<>nil) and FEditor.Focused;
  EndEditing;
  if WasFocused then
    SetFocus;
end;

procedure TCustomTreeView.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  WasFocused: Boolean;
begin
  if (Key = VK_ESCAPE) or (Key = VK_RETURN) then
  begin
    WasFocused := Assigned(FEditor) and FEditor.Focused;
    EndEditing(Key = VK_ESCAPE);
    if WasFocused then
      SetFocus;
    Key := 0; // key was handled
  end;
end;

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

procedure TCustomTreeView.ConsistencyCheck;
var OldMaxRight, OldLastTop, OldMaxLvl: integer;
  OldTopItem, OldBottomItem: TTreeNode;
begin
  if Canvas=nil then
    RaiseGDBException('Canvas=nil');
  if (FExpandSignSize<0) then
    RaiseGDBException('FExpandSignSize='+IntToStr(FExpandSignSize));
  if FDefItemHeight<0 then
    RaiseGDBException('FDefItemHeight='+IntToStr(FDefItemHeight));
  if FIndent<0 then
    RaiseGDBException('FIndent='+IntToStr(FIndent));
  if FMaxRight<0 then
    RaiseGDBException('FMaxRight='+IntToStr(FMaxRight));
  if FTreeNodes=nil then
    RaiseGDBException('FTreeNodes=nil');
  FTreeNodes.ConsistencyCheck;
  if Items.FUpdateCount<0 then
    RaiseGDBException('FUpdateCount='+IntToStr(Items.FUpdateCount));
  if (not (tvsTopsNeedsUpdate in FStates)) then begin
    if Items.GetLastSubNode<>nil then begin
      OldLastTop:=Items.GetLastSubNode.Top;
      Include(FStates,tvsTopsNeedsUpdate);
      UpdateAllTops;
      if OldLastTop<>Items.GetLastSubNode.Top then
        RaiseGDBException('OldLastTop='+DbgS(OldLastTop)
        +'<>Items.GetLastSubNode.Top='+DbgS(Items.GetLastSubNode.Top));
    end;
  end;
  if not (tvsMaxRightNeedsUpdate in FStates) then begin
    OldMaxRight:=FMaxRight;
    Include(FStates,tvsMaxRightNeedsUpdate);
    UpdateMaxRight;
    if OldMaxRight<>FMaxRight then
      RaiseGDBException('OldMaxRight<>FMaxRight');
  end;
  if not (tvsMaxLvlNeedsUpdate in FStates) then begin
    OldMaxLvl:=FMaxLvl;
    Include(FStates,tvsMaxLvlNeedsUpdate);
    UpdateMaxLvl;
    if OldMaxLvl<>FMaxLvl then
      RaiseGDBException('OldMaxLvl<>FMaxLvl');
  end;
  if (tvsIsEditing in FStates) and (FSelectedNode=nil) then
    RaiseGDBException('');
  if (FSelectedNode<>nil) then begin
    if not FSelectedNode.IsVisible then
      RaiseGDBException('not FSelectedNode.IsVisible');
  end;
  if not (tvsTopItemNeedsUpdate in FStates) then begin
    OldTopItem:=FTopItem;
    UpdateTopItem;
    if FTopItem<>OldTopItem then
      RaiseGDBException('FTopItem<>OldTopItem');
  end;
  if not (tvsBottomItemNeedsUpdate in FStates) then begin
    OldBottomItem:=FBottomItem;
    UpdateBottomItem;
    if FBottomItem<>OldBottomItem then
      RaiseGDBException('FBottomItem<>OldBottomItem');
  end;
end;

procedure TCustomTreeView.WriteDebugReport(const Prefix: string;
  AllNodes: boolean);
begin
  DbgOut('%s%s.WriteDebugReport Self=%p', [Prefix, ClassName, Pointer(Self)]);
  ConsistencyCheck;
  DebugLn('');
  if AllNodes then begin
    Items.WriteDebugReport(Prefix+'  ',true);
  end;
end;

procedure TCustomTreeView.LockSelectionChangeEvent;
begin
  inc(FSelectionChangeEventLock);
end;

procedure TCustomTreeView.UnlockSelectionChangeEvent;
begin
  dec(FSelectionChangeEventLock);
  if FSelectionChangeEventLock<0 then
    RaiseGDBException('TCustomTreeView.UnlockSelectionChangeEvent');
  if (FSelectionChangeEventLock=0)
  and (tvsSelectionChanged in FStates) then
    InternalSelectionChanged;
end;

function TCustomTreeView.GetFirstMultiSelected: TTreeNode;
begin
  Result := Items.FFirstMultiSelected;
end;

function TCustomTreeView.GetLastMultiSelected: TTreeNode;
begin
  Result := Items.FLastMultiSelected;
end;

function TCustomTreeView.SelectionVisible: boolean;
var
  ANode: TTreeNode;
begin
  ANode:=GetFirstMultiSelected;
  if (ANode<>nil) and (ANode.GetNextMultiSelected<>nil) then begin
    // 2 or more elements => a real multi selection =>
    // is visible if even one of its nodes is partly visible
    while (ANode<>nil) do begin
      if ANode.IsVisible then begin
        Result:=true;
        exit;
      end;
      ANode:=ANode.GetNextMultiSelected;
    end;
    Result:=false;
  end else begin
    if ANode=nil then
      ANode:=Selected;
    Result:=(ANode<>nil) and (ANode.IsFullHeightVisible);
  end;
end;

procedure TCustomTreeView.MakeSelectionVisible;
var
  ANode: TTreeNode;
begin
  if SelectionVisible then exit;
  ANode:=GetFirstMultiSelected;
  if (ANode=nil) then
    ANode:=Selected;
  if ANode=nil then exit;
  ANode.MakeVisible;
end;

procedure TCustomTreeView.ClearInvisibleSelection;
var
  ANode: TTreeNode;
begin
  if tvoAllowMultiSelect in FOptions then begin
    ANode := FTreeNodes.FLastMultiSelected;
    Items.ClearMultiSelection(True);       // Now clears all multi-selections
  end
  else begin
    ANode := Selected;            // Clear a single selection only if not visible
    if Assigned(ANode) and not ANode.Visible then
      ANode.Selected:=False;       // Selected := nil;
  end;
end;

procedure TCustomTreeView.MoveToNextNode;
var
  ANode: TTreeNode;
begin
  if tvoAllowMultiSelect in FOptions then
    ANode := FTreeNodes.FLastMultiSelected
  else
    ANode := Selected;
  if ANode <> nil then
    ANode := ANode.GetNextExpanded
  else if Items.Count > 0 then
    ANode := FTreeNodes.GetFirstVisibleNode;
  if ANode <> nil then
    if tvoAllowMultiSelect in FOptions then
      FTreeNodes.SelectOnlyThis(ANode)
    else
      Selected := ANode;
end;

procedure TCustomTreeView.MoveToPrevNode;
var
  ANode: TTreeNode;
begin
  if tvoAllowMultiSelect in FOptions then
    ANode := FTreeNodes.FLastMultiSelected
  else
    ANode := Selected;
  if ANode <> nil then
    ANode := ANode.GetPrevExpanded
  else if Items.Count > 0 then
    ANode := Items.GetLastExpandedSubNode;
  if ANode <> nil then
    if tvoAllowMultiSelect in FOptions then
      FTreeNodes.SelectOnlyThis(ANode)
    else
      Selected := ANode;
end;

// back to comctrls.pp
