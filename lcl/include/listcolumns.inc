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

procedure TListColumns.Update(AnItem: TCollectionItem);
begin
//TODO: Optimize implementation by invoking individual upadates instead of 
//      recreating window
  if FOwner <>nil
  then FOwner.ColumnsChanged;
end;

{ =============================================================================

  $Log$
  Revision 1.4  2002/10/09 11:46:05  lazarus
  MG: fixed loading TListView from stream

  Revision 1.3  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.1  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}

