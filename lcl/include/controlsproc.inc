Procedure ListAdd(var List : TList; Item: Pointer);
Begin
  if List = nil then List := TList.Create;
  List.Add(Item);
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
