{%MainUnit ../comctrls.pp}

{ $Id$

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  {$IFNDEF VER2_0}
  inherited;
  {$ENDIF}
  inc(FUpdateCount);
end;

procedure TListColumns.EndUpdate;
begin
  {$IFNDEF VER2_0}
  inherited;
  {$ENDIF}
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

procedure TListColumns.WSCreateColumns;
var
  n: Integer;
begin
  for n :=0 to Count - 1 do
    GetItem(n).WSCreateColumn;
end;

procedure TListColumns.SetItem(const AIndex: Integer; const AValue: TListColumn);
begin
  inherited SetItem(AIndex, AValue);
end;

// included by comctrls.pp

