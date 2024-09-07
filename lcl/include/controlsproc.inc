{%MainUnit ../controls.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
procedure ListAdd(var List : TFPList; Item: Pointer); inline;
begin
  if List = nil then
    List := TFPList.Create;
  List.Add(Item);
end;

procedure ListInsert(var List : TFPList; Index : Longint; Item: Pointer); inline;
begin
  if List = nil then
    List := TFPList.Create;
  List.Insert(Index, Item);
end;

function ListIndexOf(var List : TFPList; Item: Pointer) : Longint; inline;
begin
  if assigned(List) then
    Result := List.IndexOf(Item)
  else
    Result := -1;
end;

function ListCount(List : TFPList) : Longint; inline;
begin
  if assigned(List) then
    Result := List.Count
  else
    Result := 0;
end;

procedure ListRemove(var List : TFPList; Item: Pointer); inline;
begin
  if assigned(List) then
  begin
    List.Remove(Item);
    if List.Count = 0 then
      FreeAndNil(List);
  end;
end;

procedure ListDelete(var List : TFPList; Index: integer); inline;
begin
  if assigned(List) then
  begin
    List.Delete(Index);
    if List.Count = 0 then
      FreeAndNil(List);
  end;
end;

// included by controls.pp
