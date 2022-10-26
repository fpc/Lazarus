program bug39897;
{$mode objfpc}{$H+}
uses
  Classes;

const
  b = $1;
  a = ((b)+1);
procedure foo(x,y: byte);
begin
end;
procedure foo(x,y: word);
begin
end;


begin
  foo{declaration:foo}(b, a);
end.

