{------------------------------------------------------------------------------}
{   TListItems Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TListItems.Create(AOwner : TCustomListView);
begin
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
function TListItems.GetItem(Index : Integer): TListItem;
begin
  Result:=nil;
  if (FItems.Count-1 <Index) then Exit;
  Result := TListItem(FItems.Items[Index]);
end;

{------------------------------------------------------------------------------}
{   TListItems SetItem                                                         }
{------------------------------------------------------------------------------}
procedure TListItems.SetItem(Index : Integer; Value : TListItem);
begin
  if FItems.Count-1 < Index then Exit;
  FItems.Items[Index] := Value;
  FOwner.ItemChanged(Index);
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

{------------------------------------------------------------------------------}
{   TListItems Delete                                                          }
{------------------------------------------------------------------------------}
procedure TListItems.Delete(Index : Integer);
begin
  if (FItems.Items[index] <> nil) then
  TListItem(FItems.Items[Index]).Delete;
  FOwner.ItemDeleted(Index);
end;

{------------------------------------------------------------------------------}
{   TListItems Insert                                                          }
{------------------------------------------------------------------------------}
function TListItems.Insert(Index : Integer) : TListItem;
begin
  // ToDo
  Result:=nil;
end;

{------------------------------------------------------------------------------}
{   TListItems Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TListItems.Destroy;
begin
end;

{------------------------------------------------------------------------------}
{   TListItems ItemChanged                                                     }
{------------------------------------------------------------------------------}
Procedure TListItems.ItemChanged(Sender : TObject);  //called by the onchange of the tstringlist in TListItem
var
  Index : Integer;
begin
  Writeln('ITEM CHANGED');
  Index := FItems.IndexOf(TListItem(sender));
  if Index > -1 then
  FOwner.ItemChanged(Index);
end;

