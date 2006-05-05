{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Procedure: DeliverMessage
  Params:    Message: thje message to process
  Returns:   True if handled

  Generic function whih calls the WindowProc if defined, otherwise the 
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(const Target: Pointer; var Message): Boolean;
begin
{
  if TObject(Target) is TControl then
  begin
    TControl(Target).WindowProc(TLMessage(Message));
  end
  else
  begin
    TObject(Target).Dispatch(TLMessage(Message));
  end;
  Result := TLMessage(Message).Result = 0;}
end;



{*************************************************************}
{            callback routines                                }
{*************************************************************}

procedure QTMousePressedEvent(qwid,button,x,y,state: integer);cdecl;
{var
  MessE : TLMMouseEvent;
  Data: pointer;
}
begin
{  Data := GetData(qwid);

  EventTrace('Mouse button Press', data);
     MessE.Button := button;
    case button of
      1 : MessE.Msg := LM_LBUTTONDOWN;
      2 : MessE.Msg := LM_MBUTTONDOWN;
      3 : MessE.Msg := LM_RBUTTONDOWN;
      else MessE.Msg := LM_NULL;
    end;
   // MessE.WheelDelta := 1;
    //MessE.State := state;
    MessE.X := RoundToInt(x);
    MessE.Y := RoundToInt(y);


    if MessE.Msg <> LM_NULL then
      DeliverMessage(Data, MessE);
}
end;

{*
procedure QTMousePressedEvent(qwid,button,x,y,state: integer);cdecl;
begin


DebugLn('fired press event widget' + IntToStr(qwid));
DebugLn('mouse button=' + IntToStr(button));
DebugLn('mouse x=' + IntToStr(x));
DebugLn('mouse y=' + IntToStr(y));
DebugLn('mouse state=' + IntToStr(state));
end; *}