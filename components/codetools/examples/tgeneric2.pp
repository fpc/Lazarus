program TGeneric2;

{$mode objfpc}

type
   TList =generic(T) class(TObject)
     data : T;
     procedure Add(item: T);
   end;

procedure TList.Add(item: T);
var
  i : integer;
begin
  { The next line should fail for TList(string) }
  i:=item;
  data:=item;
end;

type
  TMyStringList = specialize TList(string);

var
  slist : TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.Add('Test');
  writeln(slist.data);
end.
