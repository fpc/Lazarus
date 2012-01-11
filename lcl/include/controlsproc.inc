{%MainUnit ../controls.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
procedure ListAdd(var List : TList; Item: Pointer);
begin
  if List = nil then
    List := TList.Create;
  List.Add(Item);
end;

procedure ListInsert(var List : TList; Index : Longint; Item: Pointer);
begin
  if List = nil then
    List := TList.Create;
  List.Insert(Index, Item);
end;

function ListIndexOf(var List : TList; Item: Pointer) : Longint;
begin
  Result := -1;
  if List <> nil then
    Result := List.IndexOf(Item);
end;

function ListCount(List : TList) : Longint;
begin
  Result := 0;
  if List <> nil then
    Result := List.Count;
end;

procedure ListRemove(var List : TList; Item: Pointer);
begin
  if List = nil then
    Exit;
  List.Remove(Item);
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;

procedure ListDelete(var List : TList; Index: integer);
begin
  if List = nil then
    Exit;
  List.Delete(Index);
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;

procedure ListAdd(var List : TFPList; Item: Pointer);
begin
  if List = nil then
    List := TFPList.Create;
  List.Add(Item);
end;

procedure ListInsert(var List : TFPList; Index : Longint; Item: Pointer);
begin
  if List = nil then
    List := TFPList.Create;
  List.Insert(Index, Item);
end;

function ListIndexOf(var List : TFPList; Item: Pointer) : Longint;
begin
  Result := -1;
  if List <> nil then
    Result := List.IndexOf(Item);
end;

function ListCount(List : TFPList) : Longint;
begin
  Result := 0;
  if List <> nil then
    Result := List.Count;
end;

procedure ListRemove(var List : TFPList; Item: Pointer);
begin
  if List = nil then
    Exit;
  List.Remove(Item);
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;

procedure ListDelete(var List : TFPList; Index: integer);
begin
  if List = nil then
    Exit;
  List.Delete(Index);
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;

// included by controls.pp
