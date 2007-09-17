unit OverloadedFunction; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function test_function(a: Pointer; b: integer): pointer; overload;
function test_function(a: Pointer = nil; b: integer = 1; c: string =
''): pointer; overload;

implementation

function test_function(a: Pointer; b: integer): pointer;
begin

end;

function test_function(a: Pointer; b: integer; c: string): pointer;
begin

end;

procedure DoSomething;
begin
  test_function(nil);
end;

end.

