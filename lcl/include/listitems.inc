{ $Id$}

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

{------------------------------------------------------------------------------}
{   TListItems Delete                                                          }
{------------------------------------------------------------------------------}
procedure TListItems.Delete(const AIndex: Integer);
begin
  if (FItems.Items[Aindex] <> nil) 
  then Item[AIndex].Delete;
  FItems.Delete(AIndex);
  FOwner.ItemDeleted(AIndex);
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
  FItems.Free;
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

{ =============================================================================

  $Log$
  Revision 1.9  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

}
