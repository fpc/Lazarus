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
  {%H-}SignalState: TSignalState; {%H-}ShiftState: TShiftState;
  {%H-}HelpCtx: THelpContext; {%H-}Event: TNotifyEvent; {%H-}E: EStreamError;
  {%H-}pc: PChar);
begin

end;

procedure DoLots;
var
  s: string;
  c: char;
  p: Pointer;
  SignalState: TSignalState;
  ShiftState: TShiftState;
  HelpCtx: THelpContext;
  Event: TNotifyEvent;
  E: EStreamError;
  pc: PChar;
begin
  TestLots(s,c,p,SignalState,ShiftState,HelpCtx,Event,E,pc);
end;

function Func1: integer;
begin
  TestInteger(Func1);
  TestInteger(Result);
end;

end.

