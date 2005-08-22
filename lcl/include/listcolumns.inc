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
{   TListColumns                                                               }
{------------------------------------------------------------------------------}
function TListColumns.Add: TListColumn;
begin
  Result := TListColumn(inherited Add);
  if (Owner<>nil)
  and ([csDesigning,csLoading,csReading]*Owner.ComponentState=[csDesigning])
  then
    OwnerFormDesignerModified(Owner);
end;

procedure TListColumns.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TListColumns.EndUpdate;
begin
  if FUpdateCount<=0 then
    RaiseGDBException('TListColumns.EndUpdate');
  dec(FUpdateCount);
  if (FUpdateCount=0) and FNeedsUpdate then Update(FItemNeedsUpdate);
end;

procedure TListColumns.Assign(Source: TPersistent);
var
  I: Integer;
  NewColumn: TListColumn;
begin
  if (Source=nil) or (Source=Self) then exit;
  BeginUpdate;
  //inherited Assign(Source);

  If Source is TCollection then begin
    Clear;
    // workaround for compiler bug: Add.Assign calls 2 times Add
    For I:=0 To TCollection(Source).Count-1 do begin
      NewColumn:=Add;
      NewColumn.Assign(TCollection(Source).Items[I]);
    end;
  end
  else
    Inherited Assign(Source);

  EndUpdate;
  if (Owner<>nil)
  and ([csDesigning,csLoading,csReading]*Owner.ComponentState=[csDesigning])
  then
    OwnerFormDesignerModified(Owner);
end;

constructor TListColumns.Create(TheOwner: TCustomListView);
begin
  FOwner := TheOwner;
  inherited Create(TListColumn);
end;

destructor TListColumns.Destroy;
begin
  BeginUpdate;
  inherited Destroy;
  EndUpdate;
end;

function TListColumns.GetItem(const AIndex: Integer): TListColumn;
begin
  Result := TListColumn(inherited GetItem(AIndex));
end;

procedure TListColumns.IntfCreateColumns;
var
  n: Integer;
begin
  for n :=0 to Count - 1 do
    GetItem(n).IntfCreateColumn;
end;

procedure TListColumns.SetItem(const AIndex: Integer; const AValue: TListColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

// included by comctrls.pp

{
  $Log$
  Revision 1.10  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.9  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.8  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.7  2004/03/06 18:44:06  mattias
  workaround for fpc bug 2859

  Revision 1.6  2003/12/26 15:23:30  mattias
  started message editor and fixed some range checks

  Revision 1.5  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

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

