unit unit_gen1;
{$mode objfpc}

interface

generic function Log<T>: integer; overload;
function Log1: integer; overload;

implementation

// Code starts at line 100
























































































// Must not have code before line 100



function Log1: integer;
begin
  WriteLn('xxx'); // TEST_BREAKPOINT=BrkUnit1Log
end;

generic function Log<T>: integer;
begin
  WriteLn('xxx'); // TEST_BREAKPOINT=BrkUnit1GenLog
end;

end.
