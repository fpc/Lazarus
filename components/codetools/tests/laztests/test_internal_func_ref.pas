unit test_internal_func_ref;
{$mode objfpc}
interface
function test{findrefs:10,4;10,9;5,12;3,16;13,16;3,34;3,35}: boolean;
function test(a: integer): boolean;
function test(a,b: integer): boolean;

implementation
function test: boolean;
  procedure aa;
  begin
    test:=true;
  end;
begin
  aa;
  test:=not test;
end;

function test(a: integer): boolean;
begin
  test:=a>0;
  if test then
    test:= not test;
end;

function test(a,b: integer): boolean;
begin
  test:=a>b;
  if test then
    test:= not test;
end;

initialization
  test;
  test();
  test(1);
  test{findrefs:10,6;10,26;3,28;6,29;5,30;16,30;3,37}(1,2);
end.
