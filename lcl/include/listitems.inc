{%MainUnit ../comctrls.pp}
{ $Id$

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
function TListItems.GetCount : Integer;
begin
  Result:=FItems.Count;
end;

{------------------------------------------------------------------------------}
{   TListItems GetItem                                                         }
{------------------------------------------------------------------------------}
function TListItems.GetItem(const AIndex: Integer): TListItem;
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
  
  if AValue.IntfUpdateAllowed
  then begin
    AValue.IntfUpdateText;
    AValue.IntfUpdateImages;
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

  if IntfUpdateAllowed
  then IntfCreateCacheItem;

  //Notify parent TListView that something was added.
  if FOwner <> nil
  then FOwner.ItemInserted(AItem);
end;

{------------------------------------------------------------------------------
   TListItems Clear
------------------------------------------------------------------------------}
procedure TListItems.Clear;
begin
  while Count > 0 do Delete(Count-1);
end;

{------------------------------------------------------------------------------}
{   TListItems Delete                                                          }
{------------------------------------------------------------------------------}
procedure TListItems.Delete(const AIndex: Integer);
begin
  // Don't use GetItem, it updates the cache, which then will become invalid
  TListItem(FItems.Items[AIndex]).Delete;
end;

{------------------------------------------------------------------------------}
{   TListItems IntfCreateItem                                                  }
{------------------------------------------------------------------------------}
procedure TListItems.IntfCreateCacheItem;
begin
  TWSCustomListViewClass(FOwner.WidgetSetClass).ItemInsert(FOwner, FCacheIndex, FCacheItem);
  Include(FCacheItem.FFlags, lifCreated);
  FCacheItem.IntfUpdateText;
  FCacheItem.IntfUpdateImages;
end;

{------------------------------------------------------------------------------}
{   TListItems IntfCreateItems                                                 }
{------------------------------------------------------------------------------}
procedure TListItems.IntfCreateItems;
var
  n: integer;
begin
  for n := 0 to FItems.Count - 1 do
  begin
    FCacheItem := TListItem(FItems[n]);
    FCacheIndex := n;
    IntfCreateCacheItem;
  end;
end;

{------------------------------------------------------------------------------}
{   TListItems IntfUpdateAllowed                                               }
{------------------------------------------------------------------------------}
function TListItems.IntfUpdateAllowed: Boolean;
begin
  Result := (FOwner <> nil)
    and FOwner.HandleAllocated
    and not (csDestroying in FOwner.ComponentState);
end;

{------------------------------------------------------------------------------}
{   TListItems ItemDeleted                                                     }
{------------------------------------------------------------------------------}
procedure TListItems.ItemDestroying(const AItem: TListItem);
var
  idx: Integer;
begin
  // Don't use IndexOf, it updates the cache, which then will become invalid
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

{------------------------------------------------------------------------------}
{   TListItems IndexOf                                                         }
{------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------}
{   TListItems Insert                                                          }
{------------------------------------------------------------------------------}
function TListItems.Insert(const AIndex: Integer): TListItem;
begin
  Result := TListItem.Create(self);
  InsertItem(Result, AIndex);
end;

{------------------------------------------------------------------------------}
{   TListItems InsertItem                                                      }
{------------------------------------------------------------------------------}
procedure TListItems.InsertItem(AItem: TListItem; const AIndex: Integer);
begin
  FItems.Insert(AIndex, AItem);
  FCacheIndex := AIndex;
  FCacheItem := AItem;

  if IntfUpdateAllowed
  then IntfCreateCacheItem;

  //Notify parent TListView that something was added.
  if FOwner <> nil
  then FOwner.ItemInserted(AItem);
end;

{------------------------------------------------------------------------------}
{   TListItems Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TListItems.Destroy;
begin
  FCacheIndex := 0;
  while FItems.Count > 0 do
  begin
    FCacheItem := TListItem(FItems[0]);
    FCacheItem.Free;
  end;
  FCacheIndex := -1;
  FreeAndNil(FItems);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TListItems FindCaption                                                     }
{------------------------------------------------------------------------------}
function TListItems.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean): TListItem;
var
  I: Integer;
  CaptionFound, AllChecked: Boolean;
begin
  result := nil;
  if (Count = 0) or (StartIndex > Count) or (not Inclusive and (count = 1)) then Exit;
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
    if not AllChecked then begin
      if Partial then CaptionFound := pos(Value, Item[I].Caption) > 0
                 else CaptionFound := Value = Item[I].Caption;
    end;
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
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData, WriteItems);
end;

type
  PItemHeader = ^TItemHeader;
  TItemHeader = record // packing is not needed (and not wanted since it controls also how this record is stored)
    Size, Count: Integer;
    Items: record end;
  end;
  PItemInfo = ^TItemInfo;
  TItemInfo = record // packing is not needed (and not wanted since it controls also how this record is stored)
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Pointer;
    //Caption: string[255]; // all stings follow here
  end;
  ShortStr = string[255];
  PShortStr = ^ShortStr;
  PInteger  = ^Integer;
  
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
  Size := Stream.ReadDWord;
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

procedure TListItems.WriteData(Stream: TStream);
  function GetLength(const S: string): Integer;
  begin
    Result := Length(S);
    if Result > 255 then Result := 255;
  end;
  
  procedure WriteString(const S: String);
  var
    Len: Integer;
  begin
    Len := Length(S);
    if Len > 255 then Len := 255;
    Stream.WriteByte(Len);
    Stream.WriteBuffer(S[1], Len);
  end;

var
  I, J, Size, L : Integer;
  ItemHeader    : TItemHeader;
  ItemInfo      : TItemInfo;
  ListItem      : TListItem;

begin
  Size := SizeOf(ItemHeader);
  for I := 0 to Count - 1 do
  begin
    L := GetLength(Item[I].Caption) + 1;
    for J := 0 to Item[I].SubItems.Count - 1 do
    begin
      Inc(L, GetLength(Item[I].SubItems[J]) + 1);
      Inc(L, SizeOf(Integer));
    end;
    Inc(Size, SizeOf(TItemInfo) - 255 + L);
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
    // TODO: check this
    // Stream pointers ???
    ItemInfo.Data := Pointer(NtoLE(PtrInt(ListItem.Data)));
    Stream.WriteBuffer(ItemInfo, SizeOf(ItemInfo));

    // Write the strings
    WriteString(ListItem.Caption);
    
    for J := 0 to ItemInfo.SubItemCount - 1 do
    begin
      WriteString(ListItem.SubItems[J]);
    end;
  end;
  
  //write SubItem images.
  for I := 0 to Count - 1 do
  begin
    ListItem := Item[I];
    // dont force subitem creation
    if ListItem.FSubItems = nil then Continue;
    for J := 0 to  ListItem.SubItems.Count - 1 do
    begin
      Stream.WriteDWord(DWord(ListItem.SubItemImages[J]));
    end;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.29  2005/04/21 16:15:52  mattias
  added TListView.FindCaption  from Matthijs Willemstein

  Revision 1.28  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.27  2005/01/16 23:01:07  marc
  * Rewrite of streamreading/writing to be sparc compatible
    (no unaligned read/writes)

  Revision 1.26  2004/11/13 17:22:15  marc
  + Added generic endian functions

  Revision 1.25  2004/11/13 15:53:14  marc
  * Fixed endianess

  Revision 1.24  2004/08/15 17:00:58  mattias
  improved DefineProperties to read/write endian independent

  Revision 1.23  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.22  2004/06/28 23:16:24  mattias
  added TListView.AddItems  from Andrew Haines

  Revision 1.21  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.20  2004/05/18 23:10:41  marc
  * Started to move TListview to the WS interface

  Revision 1.19  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.18  2004/03/18 22:35:52  mattias
  improved TCustomListView.ItemAdded with an Index param  from Andrew

  Revision 1.17  2003/02/24 19:00:42  mattias
  added TlistView.SubItem improvements from Olivier Guilbaud

  Revision 1.16  2003/02/18 23:22:56  mattias
  added listview items property editor

  Revision 1.15  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.14  2002/11/13 18:21:04  lazarus
  MG: added TListItems.Clear

  Revision 1.13  2002/09/14 14:47:41  lazarus
  MG: fixed icons

  Revision 1.12  2002/09/14 08:38:06  lazarus
  MG: added TListView notification from Vincent

  Revision 1.11  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.10  2002/03/24 16:38:01  lazarus
  MWE:
    * Fixed bug on ListItems.Delete

  Revision 1.9  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

}
