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
  Result:=nil;
  if (FItems.Count-1 < AIndex) then Exit;
  Result := TListItem(FItems.Items[AIndex]);
end;

{------------------------------------------------------------------------------}
{   TListItems SetItem                                                         }
{------------------------------------------------------------------------------}
procedure TListItems.SetItem(const AIndex: Integer; const AValue: TListItem);
begin
  if FItems.Count-1 < AIndex then Exit;
  FItems.Items[AIndex] := AValue;
  FOwner.ItemChanged(AIndex);
end;

{------------------------------------------------------------------------------}
{   TListItems Add                                                             }
{------------------------------------------------------------------------------}
function TListItems.Add:TListItem;
begin
  Result := TListItem.Create(self);
  FItems.Add(Result);
  //Notify parent TListView that something was added.
  FOwner.ItemAdded;
end;

{------------------------------------------------------------------------------
   TListItems Delete
------------------------------------------------------------------------------}
procedure TListItems.Clear;
begin
  while Count>0 do Delete(Count-1);
end;

{------------------------------------------------------------------------------}
{   TListItems Delete                                                          }
{------------------------------------------------------------------------------}
procedure TListItems.Delete(const AIndex: Integer);
begin
  Item[AIndex].Delete;
end;

{------------------------------------------------------------------------------}
{   TListItems ItemDeleted                                                     }
{------------------------------------------------------------------------------}
procedure TListItems.ItemDeleted(const AItem: TListItem);
var
  idx: Integer;
begin
  idx := FItems.IndexOf(AItem);
  if assigned(FOwner)
    then FOwner.ItemDeleted(idx);
  FItems.Remove(AItem);
end;

{------------------------------------------------------------------------------}
{   TListItems Insert                                                          }
{------------------------------------------------------------------------------}
function TListItems.Insert(const AIndex: Integer): TListItem;
begin
  // ToDo
  Result:=nil;
end;

{------------------------------------------------------------------------------}
{   TListItems Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TListItems.Destroy;
begin
  while FItems.Count>0 do
    TListItem(FItems[0]).Free;
  FreeThenNil(FItems);
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TListItems FindData                                                     }
{------------------------------------------------------------------------------}
function TListItems.FindData(const AData: Pointer): TListItem;
var
  n: Integer;
begin
  for n := 0 to FItems.Count - 1 do
  begin
    Result := TListItem(FItems[n]);
    if Result.Data = AData then Exit;
  end;          
  
  Result := nil;
end;

{------------------------------------------------------------------------------}
{   TListItems ItemChanged                                                     }
{------------------------------------------------------------------------------}
Procedure TListItems.ItemChanged(Sender : TObject);  //called by the onchange of the tstringlist in TListItem
var
  Index : Integer;
begin
//  Writeln('ITEM CHANGED');
  Index := FItems.IndexOf(TListItem(sender));
//  Writeln('Calling Item Changed with Index ',Index);
  if Index > -1 then
  FOwner.ItemChanged(Index);
end;

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
  Filer.DefineBinaryProperty('Data', @ReadData, @WriteData,WriteItems);
end;

type
  PItemHeader = ^TItemHeader;
  TItemHeader = packed record
    Size, Count: Integer;
    Items: record end;
  end;
  PItemInfo = ^TItemInfo;
  TItemInfo = packed record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Pointer;
    Caption: string[255];
  end;
  ShortStr = string[255];
  PShortStr = ^ShortStr;

procedure TListItems.ReadData(Stream: TStream);
var
  I, J, Size, L, Len: Integer;
  ItemHeader        : PItemHeader;
  ItemInfo          : PItemInfo;
  PStr              : PShortStr;
  Flag              : Boolean;
begin
  Clear;
  Flag:=False;
  Stream.ReadBuffer(Size, SizeOf(Integer));
  ItemHeader := AllocMem(Size);
  Owner.BeginUpdate;
  try
    Stream.ReadBuffer(ItemHeader^.Count, Size - SizeOf(Integer));
    ItemInfo := @ItemHeader^.Items;
    PStr := nil;
    for I := 0 to ItemHeader^.Count - 1 do
    begin
      with Add do
      begin
        Flag:=True;
        Caption := ItemInfo^.Caption;
        ImageIndex := ItemInfo^.ImageIndex;
        Data := ItemInfo^.Data;
        PStr := @ItemInfo^.Caption;
        Inc(Integer(PStr), Length(PStr^) + 1);
        Len := 0;
        for J := 0 to ItemInfo^.SubItemCount - 1 do
        begin
          SubItems.Add(PStr^);
          L := Length(PStr^);
          Inc(Len, L + 1);
          Inc(Integer(PStr), L + 1);
        end;
      end;
      Inc(Integer(ItemInfo), SizeOf(TItemInfo)-255+Length(ItemInfo^.Caption)+Len);
    end;
  finally
    FreeMem(ItemHeader, Size);
    Owner.EndUpdate;

    if Flag then
       Owner.ItemAdded;
  end;
end;

procedure TListItems.WriteData(Stream: TStream);
var
  I, J, Size, L, Len: Integer;
  ItemHeader: PItemHeader;
  ItemInfo: PItemInfo;
  PStr: PShortStr;

  function GetLength(const S: string): Integer;
  begin
    Result := Length(S);
    if Result > 255 then Result := 255;
  end;

begin
  Size := SizeOf(TItemHeader);
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
  ItemHeader := AllocMem(Size);
  try
    ItemHeader^.Size := Size;
    ItemHeader^.Count := Count;
    ItemInfo := @ItemHeader^.Items;
    PStr := nil;
    for I := 0 to Count - 1 do
    begin
      with Item[I] do
      begin
        ItemInfo^.Caption := Caption;
        ItemInfo^.ImageIndex := ImageIndex;
        ItemInfo^.OverlayIndex := -1 {OverlayIndex};
        ItemInfo^.StateIndex := -1 {StateIndex};
        ItemInfo^.Data := Data;
        ItemInfo^.SubItemCount := SubItems.Count;
        PStr := @ItemInfo^.Caption;
        Inc(Integer(PStr), Length(ItemInfo^.Caption) + 1);
        Len := 0;
        for J := 0 to SubItems.Count - 1 do
        begin
          PStr^ := SubItems[J];
          L := Length(PStr^);
          Inc(Len, L + 1);
          Inc(Integer(PStr), L + 1);
        end;
      end;
      Inc(Integer(ItemInfo), SizeOf(TItemInfo) - 255 +
        Length(ItemInfo^.Caption) + Len);
    end;
    Stream.WriteBuffer(ItemHeader^, Size);
  finally
    FreeMem(ItemHeader, Size);
  end;
end;

{ =============================================================================

  $Log$
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
