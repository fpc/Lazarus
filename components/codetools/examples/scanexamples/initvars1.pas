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
  {%H-}HelpCtx: THelpContext; {%H-}Event: TNotifyEvent; {%H-}E: EStreamError);
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
begin
  TestLots(s,c,p,SignalState,ShiftState,HelpCtx,Event,E);
end;

end.

