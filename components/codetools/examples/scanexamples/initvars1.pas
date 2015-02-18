unit InitVars1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
implementation

procedure TestInteger(i: integer);
begin
  writeln(i);
end;

procedure DoSomething;
var
  i: integer;
begin
  TestInteger(i);
end;

procedure TestLots({%H-}s: string; {%H-}c: char; {%H-}p: pointer;
  {%H-}SignalState: TSignalState; {%H-}ShiftState: TShiftState);
begin

end;

procedure DoLots;
var
  s: string;
  c: char;
  p: Pointer;
  SignalState: TSignalState;
  ShiftState: TShiftState;
begin
  TestLots(s,c,p,SignalState,ShiftState);
end;

end.

