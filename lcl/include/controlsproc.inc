{
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
Procedure ListAdd(var List : TList; Item: Pointer);
Begin
  if List = nil then List := TList.Create;
  List.Add(Item);
End;

Procedure ListInsert(var List : TList; Index : Longint; Item: Pointer);
Begin
  if List = nil then List := TList.Create;
  List.Insert(Index, Item);
End;

Function ListIndexOf(var List : TList; Item: Pointer) : Longint;
Begin
  Result := -1;
  if List = nil then exit;
  Result := List.IndexOf(Item);
End;

Function ListCount(List : TList) : Longint;
Begin
  Result := 0;
  if List = nil then exit;
  Result := List.Count;
End;

Procedure ListRemove(var List : TList; Item: Pointer);
Begin
  if List=nil then exit;
  List.Remove(Item);
  if List.Count = 0 then
	Begin
	List.Free;
	List := nil;
	End;

End;
