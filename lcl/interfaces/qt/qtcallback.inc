{------------------------------------------------------------------------------
  Procedure: DeliverMessage
  Params:    Message: thje message to process
  Returns:   True if handled

  Generic function whih calls the WindowProc if defined, otherwise the 
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(const Target: Pointer; var Message): Boolean;
begin

  if TObject(Target) is TControl then
  begin
    TControl(Target).WindowProc(TLMessage(Message));
  end
  else
  begin
    TObject(Target).Dispatch(TLMessage(Message));
  end;
  Result := TLMessage(Message).Result = 0;
end;



{*************************************************************}
{            callback routines                                }
{*************************************************************}

procedure QTMousePressedEvent(qwid,button,x,y,state: integer);cdecl;
var
  MessI : TLMMouse;
  MessE : TLMMouseEvent;
  Data: pointer;

begin
  Data := GetData(qwid);

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
    MessE.X := Trunc(x);
    MessE.Y := Trunc(y);


    if MessE.Msg <> LM_NULL then
      DeliverMessage(Data, MessE);

end;

{*
procedure QTMousePressedEvent(qwid,button,x,y,state: integer);cdecl;
begin


writeln('fired press event widget' + IntToStr(qwid));
writeln('mouse button=' + IntToStr(button));
writeln('mouse x=' + IntToStr(x));
writeln('mouse y=' + IntToStr(y));
writeln('mouse state=' + IntToStr(state));
end; *}
