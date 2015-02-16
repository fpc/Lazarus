unit InitVars1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
implementation

procedure TestInteger(var i: integer);
begin
  writeln(i);
end;

procedure DoSomething;
var
  i: integer;
begin
  TestInteger(i);
end;

end.

