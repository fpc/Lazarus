{------------------------------------------------------------------------------}
{   TListColumns                                                               }
{------------------------------------------------------------------------------}
function TListColumns.Add: TListColumn;
Begin
  Result := TListColumn(inherited Add);
end;

constructor TListColumns.Create(AOwner: TCustomListView);
begin
  FOwner := AOwner;
  inherited Create(TListColumn);
end;

function TListColumns.GetItem(const AIndex: Integer): TListColumn;
begin
  Result := TListColumn(inherited GetItem(AIndex));
end;

procedure TListColumns.SetItem(const AIndex: Integer; const AValue: TListColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TListColumns.Update(AItem: TCollectionItem);
begin
//TODO: Optimize implementation by invoking individual upadates instead of 
//      recreating window
  if FOwner <>nil
  then FOwner.ColumnsChanged;
end;

{ =============================================================================

  $Log$
  Revision 1.1  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}

