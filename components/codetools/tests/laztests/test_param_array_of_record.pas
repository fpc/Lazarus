unit test_param_array_of_record;

{$mode ObjFPC}{$H+}

interface

type
  T = record
    a: byte;
  end;

implementation

function x(a: byte): T;
begin
  result.a:=a;
end;

procedure Foo(s: integer); overload;
begin
end;

procedure Foo(s: integer; const b: array of T); overload;
begin
end;

procedure Foo(s: string; c: byte; const b: array of T); overload;
begin
end;


begin
  Foo(1);
  Foo{findrefs:11,23;3,34}(1, [x(1), x(2)]);
  Foo{findrefs:11,27;3,35}('abc', 1, [x(1), x(2)]);
end.

