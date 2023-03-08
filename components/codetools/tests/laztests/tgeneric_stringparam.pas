program tgeneric_stringparam;
{$mode objfpc}
type

  TStringHelper = type helper for string
    procedure Helping;
  end;

  generic TGen<T> = class
    function Foo: T;
    procedure bar(param: T);
  end;

  TDataItem = class
    Name, Address: String;
  end;

  TMyData = specialize TGen<TDataItem>;
  TMyString = specialize TGen<string>;

procedure TStringHelper.Helping;
begin
end;

function TGen.Foo: T;
begin
end;

procedure TGen.bar(param: T);
begin
end;

var
  MyData: TMyData;
  MyString: TMyString;

begin
  MyData.Foo.{completion:Name,Address,!Helping} ;
  MyString.Foo.{completion:Helping,!Name,!Address} ;

  d1{guesstype:TDataItem} := MyData.Foo;
  s1{guesstype:string} := MyString.Foo;

  MyData.bar(x);
  MyString.bar(y);
end.


