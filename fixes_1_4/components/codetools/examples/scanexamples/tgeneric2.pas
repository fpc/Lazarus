program TGeneric2;

{$mode objfpc}{$H+}

type

  { TMyList }

  generic TMyList<T> = class(TObject)
  type public
    TItem = record
      Value: T;
    end;
  type private
    PValue = ^T;
  var public
    Data : T;
  public
    Cache: T;
    procedure SetData(item: T);
  end;

procedure TMyList.SetData(item: T);
var
  i : integer;
begin
  {$IFDEF TestWrongType}
  { The next line should fail for TList(string) }
  i:=item;
  {$ENDIF}
  data:=item;
end;

type
  TMyStringList = specialize TMyList<string>;

var
  slist : TMyStringList;
begin
  slist := TMyStringList.Create;
  slist.SetData('Test');
  writeln(slist.data);
end.
