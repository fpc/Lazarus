{ $Id$}

{------------------------------------------------------------------------------}
{   TListItem Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TListItem.Create(AOwner : TListItems);
begin
  Inherited Create;
  FOwner := AOwner;
  FSubItems := TStringList.Create;
  TStringList(FSubItems).OnChange := @ItemChanged;
  FDestroying := False;
end;

{------------------------------------------------------------------------------}
{   TListItem SetCaption                                                       }
{------------------------------------------------------------------------------}
procedure TListItem.SetCaption(const AValue : String);
begin
  FCaption := AValue;
  FOwner.ItemChanged(self);
end;

{------------------------------------------------------------------------------}
{   TListItem SetData                                                       }
{------------------------------------------------------------------------------}
procedure TListItem.SetData(const AValue: Pointer);
begin
  FData := AValue;
  // TODO: Sort
end;

{------------------------------------------------------------------------------}
{   TListItem GetIndex                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetIndex : Integer;
begin
  if Assigned(FOwner) 
  then Result := FOwner.FItems.IndexOf(self)
  else Result := -1;
end;

{------------------------------------------------------------------------------}
{   TListItem SetSubItems                                                      }
{------------------------------------------------------------------------------}
{procedure TListItem.SetSubItems(Value : TStrings);
begin
  SubItems.Assign(Value);
  //call itemchanged here r does the onchange event fire for each?
end;
 }
{------------------------------------------------------------------------------}
{   TListItem Delete                                                           }
{------------------------------------------------------------------------------}
procedure TListItem.Delete;
begin
  if not FDestroying then Free;
end;

{------------------------------------------------------------------------------}
{   TListItem ItemChanged                                                      }
{------------------------------------------------------------------------------}
procedure TListItem.ItemChanged(sender : TObject);
begin
  FOwner.ItemChanged(self);
end;

{------------------------------------------------------------------------------}
{   TListItem Destructor                                                       }
{------------------------------------------------------------------------------}
destructor TListItem.Destroy;
begin
  FDestroying := True;
  if FOwner <> nil
  then FOwner.ItemDeleted(Self);
  
  SubItems.Free;    
  inherited Destroy;
end;

procedure TListItem.SetImageIndex(const AValue: Integer);
begin
  if AValue <> FImageIndex then
    Begin
      FImageIndex := AValue;
      ItemChanged(self);
    end;
end;

{ =============================================================================

  $Log$
  Revision 1.8  2002/03/24 16:38:01  lazarus
  MWE:
    * Fixed bug on ListItems.Delete

  Revision 1.7  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

}
