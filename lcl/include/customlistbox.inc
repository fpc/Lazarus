// included by stdctrls.pp
{
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
}
{ if not HandleAllocated then
    FItems contains a TExtendedStringList
  else
    FItems contains an interface specific TStrings descendent
}

type
  TCustomListBoxItemFlag = (clbiSelected);
  TCustomListBoxItemFlags = set of TCustomListBoxItemFlag;
  
  TCustomListBoxItemRecord = record
    TheObject: TObject;
    Flags: TCustomListBoxItemFlags;
  end;
  PCustomListBoxItemRecord = ^TCustomListBoxItemRecord;


function GetListBoxItemRecord(ListBoxInternalItems: TStrings;
  Index: integer): PCustomListBoxItemRecord;
begin
  Result:=PCustomListBoxItemRecord(
                      TExtendedStringList(ListBoxInternalItems).Records[Index]);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.CreateHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.CreateHandle;
var
  NewStrings : TStrings;
  i: integer;
begin
  //writeln('[TCustomListBox.CreateHandle] A ',FItems.ClassName);
  inherited CreateHandle;
  //writeln('[TCustomListBox.CreateHandle] B ',FItems.ClassName);
  // create
  CNSendMessage(LM_SETBORDER, Self, nil);
  UpdateSelectionMode;
  UpdateSorted;

  // fetch the interface item list
  NewStrings:= TStrings(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
  // copy the items (text+objects)
  NewStrings.Assign(Items);
  // copy items attributes
  for i:=0 to Items.Count-1 do begin
    if clbiSelected in GetListBoxItemRecord(FItems,i)^.Flags then
      SendItemSelected(i,True);
  end;
  // free old items
  FItems.Free;
  
  // new item list is the interface item list
  FItems:= NewStrings;

  CNSendMessage(LM_SETITEMINDEX, Self, Pointer(FItemIndex));
  //writeln('[TCustomListBox.CreateHandle] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.DestroyHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.DestroyHandle;
var
  NewStrings : TStrings;
  i, Cnt: integer;
begin
  //writeln('[TCustomListBox.DestroyHandle] A ',FItems.ClassName);
  // create internal item list
  NewStrings:= TExtendedStringList.Create(SizeOf(TCustomListBoxItemRecord));
  // copy items (text+objects) from the interface items list
  NewStrings.Assign(Items);
  // copy items attributes
  Cnt:=Items.Count;
  for i:=0 to Cnt-1 do begin
    if Selected[i] then
      Include(GetListBoxItemRecord(NewStrings,i)^.Flags,clbiSelected);
  end;
  // free the interface items list
  FItems.Free;
  // new item list is the internal item list
  FItems:= NewStrings;
  //writeln('[TCustomListBox.DestroyHandle] B ',FItems.ClassName);
  inherited DestroyHandle;
  //writeln('[TCustomListBox.DestroyHandle] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetBorderStyle                                      }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetBorderStyle(Val : TBorderStyle);
begin
  if FBorderStyle <> Val then begin
    FBorderStyle:= Val;
    if HandleAllocated then CNSendMessage(LM_SETBORDER, Self, nil);
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.UpdateSelectionMode                                 }
{------------------------------------------------------------------------------}
procedure TCustomListBox.UpdateSelectionMode;
var
  Msg : TLMSetSelMode;
begin
  if not HandleAllocated then exit;
  Msg.ExtendedSelect:= ExtendedSelect;
  Msg.MultiSelect:= MultiSelect;
  CNSendMessage(LM_SETSELMODE, Self, @Msg);
end;

{------------------------------------------------------------------------------
  function TCustomListBox.GetTopIndex: Integer;
------------------------------------------------------------------------------}
function TCustomListBox.GetTopIndex: Integer;
begin
  if HandleAllocated then
    FTopIndex:=CNSendMessage(LB_GETTOPINDEX, Self, nil);
  Result := FTopIndex;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.SetTopIndex(const AValue: Integer);
------------------------------------------------------------------------------}
procedure TCustomListBox.SetTopIndex(const AValue: Integer);
begin
  if FTopIndex=AValue then exit;
  FTopIndex:=AValue;
  if HandleAllocated then
    CNSendMessage(LB_SETTOPINDEX, Self, Pointer(FTopIndex));
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.UpdateSorted;
------------------------------------------------------------------------------}
procedure TCustomListBox.UpdateSorted;
var AMessage : TLMSort;
begin
  if not HandleAllocated then exit;
  with AMessage do begin
    Msg:= LM_SORT;
    List:= TObject(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
    IsSorted:= FSorted;
  end;
  CNSendMessage(LM_SORT, Self, @AMessage);
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
    end;
    DrawItem(ItemID, Area, ItemState);
    if odFocused in ItemState then
      {DrawFocusRect(hDC, rcItem)};
    FCanvas.Handle := 0;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomListBox.SendItemSelected(Index: integer; IsSelected: boolean);

  Tell the interface whether an item is selected.
------------------------------------------------------------------------------}
procedure TCustomListBox.SendItemSelected(Index: integer; IsSelected: boolean);
var
  Msg : TLMSetSel;
begin
  if HandleAllocated then begin
    Msg.Index:= Index;
    Msg.Selected:= IsSelected;
    CNSendMessage(LM_SETSEL, Self, @Msg);
  end;
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
  if (Index < 0) or (Index >= Items.Count) then
    raise Exception.Create('List index out of bounds');
  //writeln('TCustomListBox.SetSelected A ',Items.Count);
  if HandleAllocated then begin
    //writeln('TCustomListBox.SetSelected B ',Items.Count);
    SendItemSelected(Index,Val);
    //writeln('TCustomListBox.SetSelected END ',Items.Count);
  end else begin
    if Val then
      Include(GetListBoxItemRecord(FItems,Index)^.Flags,clbiSelected)
    else
      Exclude(GetListBoxItemRecord(FItems,Index)^.Flags,clbiSelected)
  end;
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelected                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelected(Index : integer) : boolean;
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise Exception.Create('TCustomListBox.GetSelected: index '+IntToStr(Index)
      +' out of bound. Count='+IntToStr(Items.Count));
  if HandleAllocated then
    Result:= (CNSendMessage(LM_GETSEL, Self, @Index) >= 0)
  else
    Result:=clbiSelected in GetListBoxItemRecord(FItems,Index)^.Flags;
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelCount                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelCount : integer;
begin
  if HandleAllocated then
    Result:= CNSendMessage(LM_GETSELCOUNT, Self, nil)
  else
    Result:=0;
end;

function TCustomListBox.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

procedure TCustomListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
    FItemHeight := Value;
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

{------------------------------------------------------------------------------}
{  procedure TCustomListBox.SetStyle                                }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetStyle(Val : TListBoxStyle);
begin
  if Val <> FStyle then begin
    FStyle:= Val;
  end;
end;

procedure TCustomListBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
{var
  Flags: Longint;
  Data: String;}
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, ARect, State)
  else if not (odPainted in State) then
  begin
    FCanvas.FillRect(ARect);
    if (Index>=0) and (Index < Items.Count) then
    begin
      {Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      Data := '';
      if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
        Data := DoGetData(Index)
      else
        Data := Items[Index];
      DrawText(FCanvas.Handle, PChar(Data), Length(Data), Rect, Flags);}
    end;
  end;
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.SetItems                                   }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetItems(Value : TStrings);
begin
  if (Value <> FItems) then begin
//writeln('[TCustomListBox.SetItems] A FItems=',FItems.ClassName,' Value=',Value.ClassName);
    FItems.Assign(Value);
  end;
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.Create                                      }
{------------------------------------------------------------------------------}
constructor TCustomListBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csListBox;

  FBorderStyle:= bsSingle;
  FItems := TExtendedStringList.Create(SizeOf(TCustomListBoxItemRecord));
  FItemIndex:=-1;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  SetBounds(1, 1, 100, 25);
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.Destroy                                      }
{------------------------------------------------------------------------------}
destructor TCustomListBox.Destroy;
begin
  FCanvas.Free;
  FCanvas:=nil;
  inherited Destroy;
  FItems.Free;
end;

function TCustomListBox.GetItemIndex : integer;
begin
//writeln('[TCustomListBox.GetItemIndex] A ',FItems.ClassName);
  if HandleAllocated then begin
    Result:= CNSendMessage(LM_GETITEMINDEX, Self, nil);
    FItemIndex:=Result;
  end else
    Result:=FItemIndex;
//writeln('[TCustomListBox.GetItemIndex] END ');
end;

procedure TCustomListBox.SetItemIndex(Val : integer);
begin
  if (Val < 0) or (Val >= FItems.Count) then raise Exception.Create('Out of bounds');
//writeln('[TCustomListBox.SetItemIndex] A ',FItems.ClassName,' ',Val);
  FItemIndex:=Val;
  if HandleAllocated then
    CNSendMessage(LM_SETITEMINDEX, Self, Pointer(Val));
//writeln('[TCustomListBox.SetItemIndex] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{  procedure TCustomListBox.Clear                                            }
{------------------------------------------------------------------------------}
procedure TCustomListBox.Clear;
begin
  FItems.Clear;
end;


// back to stdctrls.pp
