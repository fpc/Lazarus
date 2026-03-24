unit test_result_refs;

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
  Result{findrefs:3,41;8,42;9,43;5,45} := Bird.Result and Odd(count);
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
    Result{findrefs:3,13;5,54;5,77;13,80;14,81}:=11;
    self.Result:=true;
  end;
  Self.aa:=1;
  Fly:=1;
  Result{findrefs:3,59;7,61;3,62}:=2;
  aa:=3;
  Inc(Result,aa);
  Result:=Fly-1;
  with loci do begin
    Result:=33;
  end;
  test_result_refs.Self:=111;
  test_result_refs.Result:=1;
  self.Result:=true;
end;

function ExternalDotResult: word;
  var c: T1;
begin
  Result{findrefs:3,74;3,81;23,81}:=1;
  with c do
  begin
    Result:=11;
    self:=123;
  end;
  with c do Result:=111;
  Result:= c.Result - Result;
  test_result_refs.Result:=1;
end;

function LocalDotResult: word;
type T2 = record
  Result{findrefs:3,87;13,94;5,97}: integer;
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
