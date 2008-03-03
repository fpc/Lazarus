{%MainUnit ../controls.pp}

{
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
procedure ListAdd(var List : TList; Item: Pointer);
Begin
  if List = nil then List := TList.Create;
  List.Add(Item);
End;

procedure ListInsert(var List : TList; Index : Longint; Item: Pointer);
Begin
  if List = nil then List := TList.Create;
  List.Insert(Index, Item);
End;

function ListIndexOf(var List : TList; Item: Pointer) : Longint;
Begin
  Result := -1;
  if List <> nil then Result := List.IndexOf(Item);
End;

function ListCount(List : TList) : Longint;
Begin
  Result := 0;
  if List <> nil then Result := List.Count;
End;

procedure ListRemove(var List : TList; Item: Pointer);
Begin
  if List=nil then exit;
  List.Remove(Item);
  if List.Count = 0 then begin
    List.Free;
    List := nil;
  End;
End;

procedure ListDelete(var List : TList; Index: integer);
Begin
  if List=nil then exit;
  List.Delete(Index);
  if List.Count = 0 then begin
    List.Free;
    List := nil;
  End;
End;

procedure ListAdd(var List : TFPList; Item: Pointer);
Begin
  if List = nil then List := TFPList.Create;
  List.Add(Item);
End;

procedure ListInsert(var List : TFPList; Index : Longint; Item: Pointer);
Begin
  if List = nil then List := TFPList.Create;
  List.Insert(Index, Item);
End;

function ListIndexOf(var List : TFPList; Item: Pointer) : Longint;
Begin
  Result := -1;
  if List <> nil then Result := List.IndexOf(Item);
End;

function ListCount(List : TFPList) : Longint;
Begin
  Result := 0;
  if List <> nil then Result := List.Count;
End;

procedure ListRemove(var List : TFPList; Item: Pointer);
Begin
  if List=nil then exit;
  List.Remove(Item);
  if List.Count = 0 then begin
    List.Free;
    List := nil;
  End;
End;

procedure ListDelete(var List : TFPList; Index: integer);
Begin
  if List=nil then exit;
  List.Delete(Index);
  if List.Count = 0 then begin
    List.Free;
    List := nil;
  End;
End;
// included by controls.pp
