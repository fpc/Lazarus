(******************************************************************************
                                   TButton
 ******************************************************************************)

{------------------------------------------------------------------------------}
{       TButton Constructor                                                    }
{------------------------------------------------------------------------------}

constructor TButton.Create(AOwner: TComponent);
begin

  Inherited Create(AOwner);
  {set the component style to csButton}
  fCompStyle := csButton;

  {set default alignment}
  Align := alNone;

  {setup default sizes}

  SetBounds(1, 1, 50, 25);


end;

{------------------------------------------------------------------------------
  Method:  TButton.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TButton.CreateWnd;
begin
  inherited CreateWnd;
end;

{------------------------------------------------------------------------------
  Method:  TButton.SetDefault
  Params:  Value
  Returns: Nothing

  
 ------------------------------------------------------------------------------}
procedure TButton.SetDefault(Value : Boolean);
begin
  if FDefault = Value then Exit;

  FDefault := Value;
  CNSendMessage(LM_BTNDEFAULT_CHANGED,Self,nil);
End;

{------------------------------------------------------------------------------
  Method:  TButton.Click
  Params:  None
  Returns: Nothing

  Handles the event that the button is clicked
 ------------------------------------------------------------------------------}
procedure TButton.Click;  
var
  Form : TCustomForm;
Begin
  if ModalResult <> mrNone
  then begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.ModalResult := ModalResult;
  end;
  inherited Click;
end;

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.2  2000/06/13 20:50:42  lazarus
  MWE:
    - Started to remove obsolete/dead code/messages

  HJO:
    * Fixed messages in showmodal of 2nd form
    * Fixed modal result for button


}
