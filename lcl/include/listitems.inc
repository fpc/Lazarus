{%MainUnit ../comctrls.pp}
{ $Id$

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
}

{------------------------------------------------------------------------------}
{   TListItems Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TListItems.Create(AOwner : TCustomListView);
begin
  Inherited Create;
  FItems := TList.Create;
  FOwner := AOwner;
  FCacheIndex := -1;
end;

{------------------------------------------------------------------------------}
{   TListItems GetCount                                                        }
{------------------------------------------------------------------------------}
function TListItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TListItems.SetCount(const ACount: Integer); 
begin
  if not FOwner.OwnerData then Exit;
  FItems.Count := ACount;
  if WSUpdateAllowed then 
    WSSetItemsCount(FItems.Count);
  // reset ownerdata 
  if (FOwner.FOwnerDataItem.Index >= ACount) then
    FOwner.FOwnerDataItem.SetDataIndex(-1);
  // invalidate selection
  FOwner.InvalidateSelected;
end;

{------------------------------------------------------------------------------}
{   TListItems GetItem                                                         }
{------------------------------------------------------------------------------}
function TListItems.GetItem(const AIndex: Integer): TListItem;
begin
  if not FOwner.OwnerData then
  begin
    if (FCacheIndex <> -1) and (FCacheIndex = AIndex)
    then begin
      Result := FCacheItem;
      Exit;
    end;
    
    if FItems.Count - 1 < AIndex
    then Result := nil
    else begin
      Result := TListItem(FItems.Items[AIndex]);
      FCacheItem := Result;
      FCacheIndex := AIndex;
    end;
  end 
  else 
  begin
    if (AIndex >= FItems.Count) then
    begin
      Result := nil;
      Exit;
    end;
    FOwner.FOwnerDataItem.SetDataIndex(AIndex);
    Result := FOwner.FOwnerDataItem;
  end;
end;

function TListItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{------------------------------------------------------------------------------}
{   TListItems SetItem                                                         }
{------------------------------------------------------------------------------}
procedure TListItems.SetItem(const AIndex: Integer; const AValue: TListItem);
var
  OldItem: TListItem;
begin
  if FItems.Count - 1 < AIndex then Exit;
  
  OldItem := GetItem(AIndex);
  if OldItem = AValue then Exit;
  
  FItems.Items[AIndex] := AValue;
  FCacheIndex := AIndex;
  FCacheItem := AValue;

  if WSUpdateAllowed then
    WSUpdateItem(AIndex, AValue);

  if AValue.WSUpdateAllowed
  then begin
    AValue.WSUpdateText;
    AValue.WSUpdateImages;
    AValue.WSUpdateChecked;
    AValue.WSUpdateState;
  end;
end;

{------------------------------------------------------------------------------}
{   TListItems Add                                                             }
{------------------------------------------------------------------------------}
function TListItems.Add: TListItem;
begin
  Result := TListItem.Create(self);
  AddItem(Result);
end;

{------------------------------------------------------------------------------}
{   TListItems AddItem                                                         }
{------------------------------------------------------------------------------}
procedure TListItems.AddItem(AItem: TListItem);
begin
  FCacheIndex := FItems.Add(AItem);
  FCacheItem := AItem;

  if WSUpdateAllowed
  then WSCreateCacheItem;

  //Notify parent TListView that something was added.
  if FOwner <> nil
  then FOwner.ItemInserted(AItem);
end;

procedure TListItems.BeginUpdate;
begin
  Owner.BeginUpdate;
end;

{------------------------------------------------------------------------------
   TListItems Clear
------------------------------------------------------------------------------}
procedure TListItems.Clear;
var
  i : Integer;
begin
  if not Owner.OwnerData then 
    while Count > 0 do Delete(Count-1)
  else 
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      TListItem(FItems[i]).Free;
      FItems[i]:=nil;
    end;
    Count := 0;
  end;
end;

{------------------------------------------------------------------------------}
{   TListItems Delete                                                          }
{------------------------------------------------------------------------------}
procedure TListItems.Delete(const AIndex: Integer);
begin
  // Don't use GetItem, it updates the cache, which then will become invalid
  TListItem(FItems.Items[AIndex]).Delete;
end;

procedure TListItems.EndUpdate;
begin
  Owner.EndUpdate;
end;

{------------------------------------------------------------------------------}
{   TListItems IntfCreateItem                                                  }
{------------------------------------------------------------------------------}
procedure TListItems.WSCreateCacheItem;
begin
  TWSCustomListViewClass(FOwner.WidgetSetClass).ItemInsert(FOwner, FCacheIndex, FCacheItem);
  Include(FCacheItem.FFlags, lifCreated);
  FCacheItem.WSUpdateText;
  FCacheItem.WSUpdateImages;
  FCacheItem.WSUpdateChecked;
  if not (lisfWSItemsCreated in FFlags) then
    FCacheItem.WSSetState;
  FCacheItem.WSUpdateState;
end;

{------------------------------------------------------------------------------}
{   TListItems IntfCreateItems                                                 }
{------------------------------------------------------------------------------}
procedure TListItems.WSCreateItems;
var
  n: integer;
begin
  for n := 0 to FItems.Count - 1 do
  begin
    FCacheItem := TListItem(FItems[n]);
    FCacheIndex := n;
    WSCreateCacheItem;
  end;
  Include(FFlags, lisfWSItemsCreated);
end;

procedure TListItems.DoFinalizeWnd;
begin
  Exclude(FFlags, lisfWSItemsCreated);
end;

{------------------------------------------------------------------------------}
{   TListItems IntfUpdateAllowed                                               }
{------------------------------------------------------------------------------}
function TListItems.WSUpdateAllowed: Boolean;
begin
  Result := (FOwner <> nil)
    and FOwner.HandleAllocated
    and not (csDestroying in FOwner.ComponentState);
end;

procedure TListItems.WSUpdateItem(const AIndex: Integer; const AValue: TListItem);
begin
  TWSCustomListViewClass(FOwner.WidgetSetClass).ItemUpdate(FOwner, AIndex, AValue);
end;

procedure TListItems.WSSetItemsCount(const ACount: Integer); 
begin
  TWSCustomListViewClass(FOwner.WidgetSetClass).SetItemsCount(FOwner, ACount);
end;

{------------------------------------------------------------------------------}
{   TListItems ItemDeleted                                                     }
{------------------------------------------------------------------------------}
procedure TListItems.ItemDestroying(const AItem: TListItem);
var
  idx: Integer;
begin
  // Don't use IndexOf, it updates the cache, which then will become invalid
  //DebugLn('TListItems.ItemDestroying ',dbgs(AItem));
  if  (FCacheIndex <> -1)
  and (FCacheItem = AItem)
  then idx := FCacheIndex
  else idx := FItems.IndexOf(AItem);

  if idx = -1 then Exit; //?????

  if FOwner <> nil
  then begin
    FOwner.ItemDeleted(AItem);

    if (FOwner.HandleAllocated)
    and (lifCreated in AItem.FFlags)
    then begin
      Exclude(AItem.FFlags, lifCreated);
      TWSCustomListViewClass(FOwner.WidgetSetClass).ItemDelete(FOwner, idx);
    end;
  end;

  if FCacheIndex = idx 
  then FCacheIndex := -1;
  FItems.Delete(idx);
end;

{------------------------------------------------------------------------------
   TListItems IndexOf
------------------------------------------------------------------------------}
function TListItems.IndexOf(const AItem: TListItem): Integer;
begin
  if  (FCacheIndex <> -1) 
  and (FCacheItem = AItem)
  then begin
    Result := FCacheIndex;
    Exit;
  end;
  
  Result := FItems.IndexOf(AItem);
  if Result = -1 then Exit;
  
  FCacheIndex := Result;
  FCacheItem := AItem;
end;

{------------------------------------------------------------------------------
   TListItems Insert
------------------------------------------------------------------------------}
function TListItems.Insert(const AIndex: Integer): TListItem;
begin
  Result := TListItem.Create(self);
  InsertItem(Result, AIndex);
end;

{------------------------------------------------------------------------------
   TListItems InsertItem
------------------------------------------------------------------------------}
procedure TListItems.InsertItem(AItem: TListItem; const AIndex: Integer);
begin
  FItems.Insert(AIndex, AItem);
  FCacheIndex := AIndex;
  FCacheItem := AItem;

  if WSUpdateAllowed
  then WSCreateCacheItem;

  //Notify parent TListView that something was added.
  if FOwner <> nil
  then FOwner.ItemInserted(AItem);
end;

{------------------------------------------------------------------------------}
{   TListItems Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TListItems.Destroy;
begin
  if not FOwner.FOwnerData then  
  begin
    // to call Self.Clear instead?
    FCacheIndex := 0;
    while FItems.Count > 0 do
    begin
      FCacheItem := TListItem(FItems[0]);
      FCacheItem.Free;
    end;
  end;
  FCacheIndex := -1;
  FreeAndNil(FItems);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TListItems FindCaption                                                     }
{------------------------------------------------------------------------------}
function TListItems.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean; PartStart: Boolean): TListItem;
var
  I: Integer;
  CaptionFound, AllChecked: Boolean;
begin
  result := nil;
  if (Count = 0) or (StartIndex >= Count) or (not Inclusive and (count = 1)) then Exit;
  CaptionFound := False;
  AllChecked := False;
  if Inclusive then
    I := StartIndex
  else begin
    I := succ(StartIndex);
    if I >= Count then I := 0;
  end;
  if Wrap then Wrap := (StartIndex <> 0);
  repeat
    if Partial then begin
      if PartStart then
        CaptionFound := pos(Value, Item[I].Caption) = 1
      else
        CaptionFound := pos(Value, Item[I].Caption) <> 0;
    end else
      CaptionFound := Value = Item[I].Caption;
    if not CaptionFound then begin
      Inc(I);
      if Wrap then begin
        if I = Count then
          I := 0
        else
          if I = StartIndex then
            AllChecked := True;
      end else begin
        if I = Count then AllChecked := True;
      end;
    end;
  until CaptionFound or AllChecked;
  if CaptionFound then result := Item[I];
end;


{------------------------------------------------------------------------------}
{   TListItems FindData                                                        }
{------------------------------------------------------------------------------}
function TListItems.FindData(const AData: Pointer): TListItem;
var
  n: Integer;
begin
  if  (FCacheIndex <> -1) 
  and (FCacheItem <> nil) 
  and (FCacheItem.Data = AData)
  then begin
    Result := FCacheItem;
    Exit;
  end;
  
  for n := 0 to FItems.Count - 1 do
  begin
    Result := TListItem(FItems[n]);
    if Result.Data = AData 
    then begin
      FCacheIndex := n;
      FCacheItem := Result;
      Exit;
    end;
  end;          
  
  Result := nil;
end;

function TListItems.GetEnumerator: TListItemsEnumerator;
begin
  Result := TListItemsEnumerator.Create(Self);
end;

{------------------------------------------------------------------------------}
{   TListItems DefineProperties                                                }
{------------------------------------------------------------------------------}
procedure TListItems.DefineProperties(Filer: TFiler);

  function WriteItems: Boolean;
  var
    I: Integer;
    Items: TListItems;
  begin
    Items := TListItems(Filer.Ancestor);
    if not Assigned(Items) then
      Result := Count > 0
    else if (Items.Count <> Count) then
             Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not Item[I].IsEqual(Items[I]);
        if Result then Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', @ReadData, nil, false);
  Filer.DefineBinaryProperty('LazData', @ReadLazData, @WriteLazData, WriteItems);
end;

type
  TItemHeader = record // packing is not needed (and not wanted since it controls also how this record is stored)
    Size, Count: Integer;
    Items: record end;
  end;
  // for reading Delphi compatible TListItem.Data
  TItemInfo = record // packing is not needed (and not wanted since it controls also how this record is stored)
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Integer; // pointer field on 32 bits computers
  end;
  // Lazarus does not store ListItem.Data field, because a pointer field cannot
  // be made 64 bits safe
  TLazItemInfo = record // packing is not needed and not wanted
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
  end;

procedure TListItems.ReadData(Stream: TStream);
  function ReadString: String;
  var
    Len: Byte;
  begin
    Len := Stream.ReadByte;
    SetLength(Result, Len);
    Stream.ReadBuffer(Result[1], Len);
  end;

var
  I, J: Integer;
  ItemInfo: TItemInfo;
  ListItem: TListItem;
  Size, ItemCount, SubCount: Integer;
  StartPos: Int64;
begin
  Clear;
  StartPos := Stream.Position;
  Size := LEtoN(Integer(Stream.ReadDWord));
  ItemCount := LEtoN(Integer(Stream.ReadDWord));
  Owner.BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
    begin
      Stream.ReadBuffer(ItemInfo, SizeOf(ItemInfo));
      ListItem := Add;
      ListItem.Caption := ReadString;
      ListItem.ImageIndex := LEtoN(ItemInfo.ImageIndex);
      // ListItem.StateIndex := LEtoN(ItemInfo.StateIndex);
      // ListItem.OverlayIndex := LEtoN(ItemInfo.OverlayIndex);

      //TODO: check if we need to stream a data pointer
      ListItem.Data := Pointer(LEtoN(PtrInt(ItemInfo.Data)));
      
      SubCount := LEtoN(ItemInfo.SubItemCount);
      for J := 0 to SubCount - 1 do
      begin
        ListItem.SubItems.Add(ReadString);
      end;
    end;
    
    //read subitem images
    if Stream.Position < StartPos + Size
    then begin
      for I := 0 to Count - 1 do
      begin
        ListItem := Item[I];
        if ListItem.FSubItems = nil then Continue;

        for J := 0 to ListItem.SubItems.Count - 1 do
          ListItem.SubItemImages[J] := LEtoN(Integer(Stream.ReadDWord));
      end;
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TListItems.ReadLazData(Stream: TStream);
var
  I, J: Integer;
  ItemInfo: TLazItemInfo;
  ListItem: TListItem;
  Size, ItemCount, SubCount: Integer;
  StartPos: Int64;
begin
  Clear;
  StartPos := Stream.Position;
  Size := LEtoN(Integer(Stream.ReadDWord));
  ItemCount := LEtoN(Integer(Stream.ReadDWord));
  Owner.BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
    begin
      Stream.ReadBuffer(ItemInfo, SizeOf(ItemInfo));
      ListItem := Add;
      ListItem.Caption := Stream.ReadAnsiString;
      ListItem.ImageIndex := LEtoN(ItemInfo.ImageIndex);
      // ListItem.StateIndex := LEtoN(ItemInfo.StateIndex);
      // ListItem.OverlayIndex := LEtoN(ItemInfo.OverlayIndex);

      SubCount := LEtoN(ItemInfo.SubItemCount);
      for J := 0 to SubCount - 1 do
      begin
        ListItem.SubItems.Add(Stream.ReadAnsiString);
      end;
    end;

    //read subitem images
    if Stream.Position < StartPos + Size
    then begin
      for I := 0 to Count - 1 do
      begin
        ListItem := Item[I];
        if ListItem.FSubItems = nil then Continue;

        for J := 0 to ListItem.SubItems.Count - 1 do
          ListItem.SubItemImages[J] := LEtoN(Integer(Stream.ReadDWord));
      end;
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TListItems.WriteLazData(Stream: TStream);
var
  I, J, Size, L : Integer;
  ItemHeader    : TItemHeader;
  ItemInfo      : TLazItemInfo;
  ListItem      : TListItem;

begin
  Size := SizeOf(ItemHeader);
  for I := 0 to Count - 1 do
  begin
    L := Length(Item[I].Caption) + 4;
    for J := 0 to Item[I].SubItems.Count - 1 do
    begin
      Inc(L, Length(Item[I].SubItems[J]) + 4);
      Inc(L, SizeOf(DWORD));
    end;
    Inc(Size, SizeOf(TLazItemInfo) + L);
  end;

  ItemHeader.Size := NtoLE(Size);
  ItemHeader.Count := NtoLE(Count);
  Stream.WriteBuffer(ItemHeader, SizeOf(ItemHeader));
  
  for I := 0 to Count - 1 do
  begin
    ListItem := Item[I];

    ItemInfo.ImageIndex := NtoLE(ListItem.ImageIndex);
    ItemInfo.StateIndex := NtoLE(Integer(-1)) {StateIndex};
    ItemInfo.OverlayIndex := NtoLE(Integer(-1)) {OverlayIndex};
    // don't acces SubItems directly, they will be created
    if ListItem.FSubItems = nil
    then ItemInfo.SubItemCount := 0
    else ItemInfo.SubItemCount := NtoLE(ListItem.SubItems.Count);

    Stream.WriteBuffer(ItemInfo, SizeOf(ItemInfo));

    // Write the strings
    Stream.WriteAnsiString(ListItem.Caption);
    
    for J := 0 to ItemInfo.SubItemCount - 1 do
    begin
      Stream.WriteAnsiString(ListItem.SubItems[J]);
    end;
  end;
  
  //write SubItem images.
  for I := 0 to Count - 1 do
  begin
    ListItem := Item[I];
    // do not force subitem creation
    if ListItem.FSubItems = nil then Continue;
    for J := 0 to  ListItem.SubItems.Count - 1 do
    begin
      Stream.WriteDWord(DWord(ListItem.SubItemImages[J]));
    end;
  end;
end;
