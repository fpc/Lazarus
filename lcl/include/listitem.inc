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
  
  FreeThenNil(FSubItems);
  inherited Destroy;
end;

procedure TListItem.SetImageIndex(const AValue: Integer);
begin
  if AValue <> FImageIndex then
    Begin
      FImageIndex := AValue;
      ItemChanged(Self);
    end;
end;

{------------------------------------------------------------------------------}
{   TListItem GetState                                                         }
{------------------------------------------------------------------------------}
function TListItem.GetState(const AIndex: Integer): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------}
{   TListItem SetState                                                         }
{------------------------------------------------------------------------------}
procedure TListItem.SetState(const AIndex: Integer; const AState: Boolean);
begin
end;

{ =============================================================================

  $Log$
  Revision 1.11  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.10  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.9  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.8  2002/03/24 16:38:01  lazarus
  MWE:
    * Fixed bug on ListItems.Delete

  Revision 1.7  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

}
