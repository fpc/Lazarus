{------------------------------------------------------------------------------}
{   TListItem Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TListItem.Create(AOwner : TListItems);
begin
  FOwner := AOwner;
  SubItems := TStringList.Create;
  TStringList(SubItems).OnChange := @FOwner.ItemChanged;
end;

{------------------------------------------------------------------------------}
{   TListItem SetCaption                                                       }
{------------------------------------------------------------------------------}
procedure TListItem.SetCaption(const Value : String);
begin
  FCaption := Value;
  FOwner.ItemChanged(self);
end;

{------------------------------------------------------------------------------}
{   TListItem GetIndex                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetIndex : Integer;
begin
  if Assigned(FOwner) then
    Result := FOwner.FItems.IndexOf(self)
    else
    Result := -1;
end;

{------------------------------------------------------------------------------}
{   TListItem SetSubItems                                                      }
{------------------------------------------------------------------------------}
procedure TListItem.SetSubItems(Value : TStrings);
begin
  SubItems.Assign(Value);
  //call itemchanged here r does the onchange event fire for each?
end;

{------------------------------------------------------------------------------}
{   TListItem Delete                                                           }
{------------------------------------------------------------------------------}
procedure TListItem.Delete;
begin
  free;
end;

{------------------------------------------------------------------------------}
{   TListItem Destructor                                                       }
{------------------------------------------------------------------------------}
destructor TListItem.Destroy;
begin
 SubItems.Free;
 Inherited Destroy;
end;


