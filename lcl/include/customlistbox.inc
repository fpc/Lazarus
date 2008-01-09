{%MainUnit ../stdctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{ if not HandleAllocated then
    FItems contains a TExtendedStringList
  else
    FItems contains an interface specific TStrings descendent
}

type
  TCustomListBoxItemRecord = record
    TheObject: TObject;
    Selected: Boolean;
  end;
  PCustomListBoxItemRecord = ^TCustomListBoxItemRecord;

{------------------------------------------------------------------------------
 procedure TCustomListBox.AssignCacheToItemData
------------------------------------------------------------------------------}
procedure TCustomListBox.AssignCacheToItemData(const AIndex: Integer;
  const AData: Pointer);
begin
  if PCustomListBoxItemRecord(AData)^.Selected or (FItemIndex = AIndex) then
  begin
    LockSelectionChange;
    SendItemSelected(AIndex, True);
    UnlockSelectionChange;
  end;
end;

procedure TCustomListBox.BeginAutoDrag;
begin
  BeginDrag(False);
end;

procedure TCustomListBox.Loaded;
begin
  inherited Loaded;
  if HandleAllocated then begin
    LockSelectionChange;
    SendItemIndex;
    UnlockSelectionChange;
  end;
end;

{------------------------------------------------------------------------------
 procedure TCustomListBox.AssignItemDataToCache
------------------------------------------------------------------------------}
procedure TCustomListBox.AssignItemDataToCache(const AIndex: Integer;
  const AData: Pointer);
begin
  PCustomListBoxItemRecord(AData)^.Selected := Selected[AIndex];
end;

{------------------------------------------------------------------------------
 procedure TCustomListBox.InitializeWnd
------------------------------------------------------------------------------}
procedure TCustomListBox.InitializeWnd;
var
  NewStrings : TStrings;
  i, cnt: integer;
  OldItems: TExtendedStringList;
begin
  LockSelectionChange;
  //DebugLn('[TCustomListBox.InitializeWnd] A ',FItems.ClassName);
  inherited InitializeWnd;
  //DebugLn('[TCustomListBox.InitializeWnd] B ',FItems.ClassName);
  // create
  TWSCustomListBoxClass(WidgetSetClass).SetBorder(Self);

  // fetch the interface item list
  NewStrings := TWSCustomListBoxClass(WidgetSetClass).GetStrings(Self);
  // copy the items (text+objects)
  NewStrings.Assign(Items);
  OldItems := FItems as TExtendedStringList;

  // new item list is the interface item list
  FItems:= NewStrings;
  FCacheValid := False;

  SendItemIndex;

  // copy items attributes
  cnt := OldItems.Count;
  for i:=0 to cnt-1 do
    AssignCacheToItemData(i, OldItems.Records[i]);

  // free old items
  OldItems.Free;
  UnlockSelectionChange;
  //DebugLn('[TCustomListBox.InitializeWnd] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.FinalizeWnd                                       }
{------------------------------------------------------------------------------}
procedure TCustomListBox.FinalizeWnd;
var
  NewStrings : TExtendedStringList;
  i, Cnt: integer;
begin
  LockSelectionChange;
  
  // save ItemIndex on destroy handle
  if ([csDestroying,csLoading]*ComponentState=[]) then
    GetItemIndex;
  //DebugLn('[TCustomListBox.FinalizeWnd] A ',FItems.ClassName);
  // create internal item list
  if Assigned(FItems) then begin;
    NewStrings:= TExtendedStringList.Create(GetCachedDataSize);

    // copy items (text+objects) from the interface items list
    NewStrings.Assign(Items);
    // copy items attributes
    Cnt:=Items.Count;
    for i:=0 to Cnt-1 do
      AssignItemDataToCache(i, NewStrings.Records[i]);

    // free the interface items list
    FItems.Free;
    // new item list is the internal item list
    FItems:= NewStrings;
    FCacheValid := True;
    //DebugLn('[TCustomListBox.FinalizeWnd] B ',FItems.ClassName);
  end;
  inherited FinalizeWnd;
  //DebugLn('[TCustomListBox.FinalizeWnd] END ',FItems.ClassName);
  UnlockSelectionChange;
end;

class function TCustomListBox.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=100;
  Result.Y:=80;
end;

{------------------------------------------------------------------------------
 procedure TCustomListBox.UpdateSelectionMode
------------------------------------------------------------------------------}
procedure TCustomListBox.UpdateSelectionMode;
begin
  if not HandleAllocated then exit;
  LockSelectionChange;
  TWSCustomListBoxClass(WidgetSetClass).SetSelectionMode(Self, 
    ExtendedSelect, MultiSelect);
  UnlockSelectionChange;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetTopIndex: Integer;
------------------------------------------------------------------------------}
function TCustomListBox.GetTopIndex: Integer;
begin
  if HandleAllocated then
    FTopIndex := TWSCustomListBoxClass(WidgetSetClass).GetTopIndex(Self);
  Result := FTopIndex;
end;

procedure TCustomListBox.RaiseIndexOutOfBounds(AIndex: integer);
begin
  Exception.CreateFmt(rsIndexOutOfBounds, [ClassName, AIndex, FItems.Count-1]);
end;

procedure TCustomListBox.SetColumns(const AValue: Integer);
begin
  if (FColumns = AValue) or (AValue < 0) then
    exit;
  FColumns := AValue;
  if HandleAllocated then
    TWSCustomListBoxClass(WidgetSetClass).SetColumnCount(Self, FColumns);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetCount: Integer;
------------------------------------------------------------------------------}
function TCustomListBox.GetCount: Integer;
begin
  Result := Items.Count;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.SetTopIndex(const AValue: Integer);
------------------------------------------------------------------------------}
procedure TCustomListBox.SetTopIndex(const AValue: Integer);
begin
  // don't check if changed. If the item is only partly visible, the message
  // will make it complete visible.
  FTopIndex:=AValue;
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
    TWSCustomListBoxClass(WidgetSetClass).SetTopIndex(Self, AValue);
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.UpdateSorted;
------------------------------------------------------------------------------}
procedure TCustomListBox.UpdateSorted;
begin
  if not HandleAllocated then exit;
  LockSelectionChange;
  TWSCustomListBoxClass(WidgetSetClass).SetSorted(Self, FItems, FSorted);
  UnlockSelectionChange;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.LMDrawListItem(var TheMessage: TLMDrawListItem);

  Handler for custom drawing items.
 ------------------------------------------------------------------------------}
procedure TCustomListBox.LMDrawListItem(var TheMessage: TLMDrawListItem);
begin
  with TheMessage.DrawListItemStruct^ do
  begin
    FCanvas.Handle := DC;
    if Font<>nil then
      FCanvas.Font := Font;
    if Brush<>nil then
      FCanvas.Brush := Brush;
    if (ItemID >= 0) and (odSelected in ItemState) then
    begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end else begin
      FCanvas.Brush.Color:=clWindow;
      FCanvas.Font.Color:=clWindowText;
    end;
    //DebugLn('TCustomListBox.LMDrawListItem ',DbgSName(Self));
    DrawItem(ItemID, Area, ItemState);
    if odFocused in ItemState then
      {DrawFocusRect(hDC, rcItem)};
    FCanvas.Handle := 0;
  end;
end;

procedure TCustomListBox.LMMeasureItem(var TheMessage: TLMMeasureItem);
var
  AHeight: Integer;
begin
  with TheMessage.MeasureItemStruct^ do 
  begin
    if Self.ItemHeight <> 0 then
      AHeight := Self.ItemHeight
    else
      AHeight := ItemHeight;
    MeasureItem(ItemId, AHeight);
    if AHeight > 0 then
      ItemHeight := AHeight;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.LMSelChange(var TheMessage);
------------------------------------------------------------------------------}
procedure TCustomListBox.LMSelChange(var TheMessage);
begin
  //debugln('TCustomListBox.LMSelChange ',DbgSName(Self),' ',dbgs(ItemIndex));
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  //debugln('TCustomListBox.LMSelChange ',Name,':',ClassName,' ItemIndex=',dbgs(ItemIndex),' FLockSelectionChange=',dbgs(FLockSelectionChange));
  if FLockSelectionChange=0 then
    EditingDone;
  DoSelectionChange(FLockSelectionChange=0);
end;

procedure TCustomListBox.WMLButtonUp(var Message: TLMLButtonUp);
begin
  // prevent Click to be called twice when using selchange as click
  if ClickOnSelChange and FClickTriggeredBySelectionChange then
    Exclude(FControlState, csClicked);
  //debugln('TCustomListBox.WMLButtonDown ',DbgSName(Self),' ',dbgs(ItemIndex));
  inherited WMLButtonUp(Message);
  // reset flag
  FClickTriggeredBySelectionChange:=false;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.SendItemSelected(Index: integer; IsSelected: boolean);

  Tell the interface whether an item is selected.
------------------------------------------------------------------------------}
procedure TCustomListBox.SendItemSelected(Index: integer; IsSelected: boolean);
begin
  if HandleAllocated then
    TWSCustomListBoxClass(WidgetSetClass).SelectItem(Self, Index, IsSelected);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetExtendedSelect                                   }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetExtendedSelect(Val : boolean);
begin
  if Val <> FExtendedSelect then begin
    FExtendedSelect:= Val;
    UpdateSelectionMode;
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetMultiSelect                                      }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetMultiSelect(Val : boolean);
begin
  if Val <> FMultiSelect then begin
    FMultiSelect:= Val;
    UpdateSelectionMode;
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetSelected                                         }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetSelected(Index : integer; Val : boolean);
begin
  CheckIndex(Index);

  if not MultiSelect then
  begin
    if Val then
      ItemIndex := Index
    else
    if Index = ItemIndex then
      ItemIndex := -1;
  end else
  begin
    if HandleAllocated
    then SendItemSelected(Index, Val)
    else PCustomListBoxItemRecord(GetCachedData(Index))^.Selected := Val;
  end;
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelected                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelected(Index : integer) : boolean;
begin
  CheckIndex(Index);
  if HandleAllocated then
    Result:= TWSCustomListBoxClass(WidgetSetClass).GetSelected(Self, Index)
  else
    Result:= PCustomListBoxItemRecord(GetCachedData(Index))^.Selected;
  //debugln('TCustomListBox.GetSelected A ',DbgSName(Self),' Index=',dbgs(Index),' Selected=',dbgs(Result));
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelCount                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelCount : integer;
begin
  if HandleAllocated then
    Result := TWSCustomListBoxClass(WidgetSetClass).GetSelCount(Self)
  else
    Result := 0;
end;

function TCustomListBox.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

procedure TCustomListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value >= 0) then begin
    FItemHeight := Value;
    if (not HandleAllocated) or (csLoading in ComponentState) then exit;
    // TODO: remove RecreateWnd
    RecreateWnd(Self);
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetSorted                                          }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    UpdateSorted;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.SetStyle
------------------------------------------------------------------------------}
procedure TCustomListBox.SetStyle(Val : TListBoxStyle);
begin
  if Val <> FStyle then begin
    FStyle:= Val;
    if HandleAllocated then
      TWSCustomListBoxClass(WidgetSetClass).SetStyle(Self);
  end;
end;

procedure TCustomListBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
begin
  //DebugLn('TCustomListBox.DrawItem ',DbgSName(Self));
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, ARect, State)
  else if not (odPainted in State) then
  begin
    FCanvas.FillRect(ARect);
    if (Index>=0) and (Index < Items.Count) then begin
      OldBrushStyle := FCanvas.Brush.Style;
      FCanvas.Brush.Style := bsClear;

      OldTextStyle := FCanvas.TextStyle;
      NewTextStyle := OldTextStyle;
      NewTextStyle.Layout := tlCenter;
      FCanvas.TextStyle := NewTextStyle;

      FCanvas.TextRect(ARect, ARect.Left+2, ARect.Top, Items[Index]);
      FCanvas.Brush.Style := OldBrushStyle;
      FCanvas.TextStyle := OldTextStyle;
    end;
  end;
end;

procedure TCustomListBox.DoSelectionChange(User: Boolean);
begin
  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self,User);
  if User and ClickOnSelChange then begin
    Click;
    // set flag, that we triggered a Click, so that a possible MouseClick will
    // not trigger it again
    FClickTriggeredBySelectionChange:=true;
  end;
end;

procedure TCustomListBox.SendItemIndex;
begin
  TWSCustomListBoxClass(WidgetSetClass).SetItemIndex(Self, FItemIndex);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetCachedData
------------------------------------------------------------------------------}
function TCustomListBox.GetCachedData(const AIndex: Integer): Pointer;
begin
  if not FCacheValid then
    raise EInvalidOperation.Create('Reading form invalid cache');
  Result := TExtendedStringList(FItems).Records[AIndex];
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetCachedDataSize

  Returns the amount of data needed when the widged isn't realized in the
  interface
------------------------------------------------------------------------------}
function TCustomListBox.GetCachedDataSize: Integer;
begin
  Result := SizeOf(TCustomListBoxItemRecord);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.SetItems
------------------------------------------------------------------------------}
procedure TCustomListBox.SetItems(Value : TStrings);
begin
  if (Value <> FItems) then begin
//DebugLn('[TCustomListBox.SetItems] A FItems=',FItems.ClassName,' Value=',Value.ClassName);
    LockSelectionChange;
    FItems.Assign(Value);
    UnlockSelectionChange;
  end;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.Create
------------------------------------------------------------------------------}
constructor TCustomListBox.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
  LockSelectionChange;
  fCompStyle := csListBox;
  BorderStyle:= bsSingle;
  FItems := TExtendedStringList.Create(GetCachedDataSize);
  FCacheValid := True;
  FClickOnSelChange:= True;
  FItemIndex:=-1;
  FExtendedSelect := true;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  ParentColor := false;
  TabStop := true;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  UnlockSelectionChange;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.Destroy
------------------------------------------------------------------------------}
destructor TCustomListBox.Destroy;
begin
  Destroying;
  DestroyWnd;
  FreeAndNil(FCanvas);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TCustomListBox.GetItemIndex : integer;
begin
//DebugLn('[TCustomListBox.GetItemIndex] A ',FItems.ClassName);
  if HandleAllocated then 
  begin
    Result := TWSCustomListBoxClass(WidgetSetClass).GetItemIndex(Self);
    FItemIndex:=Result;
  end else
    Result:=FItemIndex;
//DebugLn('[TCustomListBox.GetItemIndex] END ');
end;

procedure TCustomListBox.SetItemIndex(AIndex : integer);
begin
  if (AIndex >= FItems.Count) then
    RaiseIndexOutOfBounds(AIndex);
  if AIndex<0 then AIndex:=-1;
//DebugLn('[TCustomListBox.SetItemIndex] A ',FItems.ClassName,' ',dbgs(AIndex));
  FItemIndex:=AIndex;
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
    SendItemIndex;
  DoSelectionChange(false);
//DebugLn('[TCustomListBox.SetItemIndex] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.CheckIndex
------------------------------------------------------------------------------}
procedure TCustomListBox.CheckIndex(const AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Items.Count) then
    RaiseIndexOutOfBounds(AIndex);
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.Clear

  Delete all items.
------------------------------------------------------------------------------}
procedure TCustomListBox.Clear;
begin
  FItems.Clear;
end;

procedure TCustomListBox.ClearSelection;
var
  i: integer;
begin
  if MultiSelect then
    for i := 0 to Items.Count - 1 do
      Selected[i] := False
  else
    ItemIndex := -1; // no need to traverse all items - look at SetSelected
end;

procedure TCustomListBox.LockSelectionChange;
begin
  inc(FLockSelectionChange);
end;

procedure TCustomListBox.UnlockSelectionChange;
begin
  dec(FLockSelectionChange);
end;

procedure TCustomListBox.Click;
begin
  inherited Click;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer);

 ------------------------------------------------------------------------------}
procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer);
begin
  if Assigned(OnMeasureItem) then
    OnMeasureItem(Self, Index, TheHeight);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetIndexAtY(Y: integer): integer;

  Returns item index at y coordinate (including scrolling)
------------------------------------------------------------------------------}
function TCustomListBox.GetIndexAtY(Y: integer): integer;
begin
  Result:=-1;
  if (not HandleAllocated) then exit;
  Result := TWSCustomListBoxClass(WidgetSetClass).GetIndexAtY(Self, Y);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetSelectedText: string;

  Returns Text of all selected items, separated by LineEnding
------------------------------------------------------------------------------}
function TCustomListBox.GetSelectedText: string;
var
  i: Integer;
begin
  Result:='';
  if ItemIndex<0 then exit;
  for i:=0 to Items.Count-1 do
    if Selected[i] then Result:=Result+Items[i]+LineEnding;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean
    ): Integer;

  Returns item index at y coordinate (including scrolling)
------------------------------------------------------------------------------}
function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean
  ): Integer;
begin
  Result:=GetIndexAtY(Pos.Y);
  if Existing then begin
    if Result>=Items.Count then Result:=-1;
  end else begin
    if (Result<0) and (Result>Items.Count) and PtInRect(ClientRect,Pos) then
      Result:=Items.Count;
  end;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.ItemRect(Index: Integer): TRect;

  Returns coordinates of an item (including scrolling)
  Special: If Index=Count the rectangle is guessed (like VCL).
------------------------------------------------------------------------------}
function TCustomListBox.ItemRect(Index: Integer): TRect;
begin
  if (Index>=0) and (Index<Items.Count) then
  begin
    TWSCustomListBoxClass(WidgetSetClass).GetItemRect(Self, Index, Result);
  end else
  if (Index=Items.Count) and (Index>0) then
  begin
    TWSCustomListBoxClass(WidgetSetClass).GetItemRect(Self, Index - 1, Result);
    OffsetRect(Result,0,Result.Bottom-Result.Top);
  end else
  begin
    FillChar(Result,SizeOf(Result),0);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.ItemVisible(Index: Integer): boolean;

  Returns true if Item is partially visible.
------------------------------------------------------------------------------}
function TCustomListBox.ItemVisible(Index: Integer): boolean;
var
  ARect: TRect;
begin
  Result:=false;
  if (Index<0) or (Index>=Items.Count) then exit;
  if not TWSCustomListBoxClass(WidgetSetClass).GetItemRect(Self, Index, ARect) then
    exit;
  if (ARect.Bottom<0) or (ARect.Top>ClientHeight) then
    exit;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TCustomListBox.ItemFullyVisible(Index: Integer): boolean;

  Returns true if Item is fully visible.
------------------------------------------------------------------------------}
function TCustomListBox.ItemFullyVisible(Index: Integer): boolean;
var
  ARect: TRect;
begin
  Result:=false;
  if (Index<0) or (Index>=Items.Count) then exit;
  if not TWSCustomListBoxClass(WidgetSetClass).GetItemRect(Self, Index, ARect) then
    exit;
  if (ARect.Top<0) or (ARect.Bottom>ClientHeight) then
    exit;
  Result:=true;
end;

procedure TCustomListBox.MakeCurrentVisible;
var
  i: Integer;
begin
  i:=ItemIndex;
  if (i<0) or (i>=Items.Count) then exit;
  TopIndex:=ItemIndex;
end;


// back to stdctrls.pp
