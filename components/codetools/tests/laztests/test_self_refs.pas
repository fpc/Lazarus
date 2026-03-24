unit test_self_refs;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
interface
uses
  Classes, SysUtils;


var Result: ShortInt;
    Self: integer;
type T1 = record
  result: integer;
  self: byte;
  function GetSelf: integer;
end;

implementation




type TBird = class
  type T3 = record
    Result: integer;
  end;
public
  Result: boolean;
  aa:integer;
  function Fly: word;
end;


function T1.GetSelf: Integer;
begin
  Result:=Self.self;
end;

operator + (count : integer; Bird : TBird) :  boolean;
begin
  Result:= Bird.Result and Odd(count);
  case Result of
  true: Result:=false;
  else
    Result:=not odd(count);
  end;
end;

function TBird.Fly: word;
var loc: T1;
    loci: T3;
begin
  with loc do begin
    Result:=11;
    self{findrefs:5,55;3,57;3,68}.Result:=true;
  end;
  Self.aa:=1;
  Fly:=1;
  Result:=2;
  aa:=3;
  Inc(Result,aa);
  Result:=Fly-1;
  with loci do begin
    Result:=33;
  end;
  test_self_refs.Self:=111;
  test_self_refs.Result:=1;
  self.Result:=true;
end;

function ExternalDotResult: word;
  var c: T1;
begin
  Result:=1;
  with c do
  begin
    Result:=11;
    self:=123;
  end;
  with c do Result:=111;
  Result:= c.Result - Result;
  test_self_refs.Result:=1;
end;

function LocalDotResult: word;
type T2 = record
  Result: integer;
end;
  var c: T2;
begin

  Result:=1;
  LocalDotResult:=2;
  with c do Result:=22;
  with c do
  begin
    Result:=222;
  end;
  writeln(Result);
end;

end.
