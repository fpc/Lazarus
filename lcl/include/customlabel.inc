{******************************************************************************
                                TCustomLabel
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetLayout
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetLayout(Value : TTextLayout);
begin
   if FLayout <> Value then
   begin
     FLayout := Value;
     CNSendMessage(LM_SETPROPERTIES,Self,nil);
   end;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetAlignment
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetAlignment(Value : TAlignment);
begin
   if fAlignment <> value then
     Begin
      fAlignment := value;
      handleneeded;
      invalidate;
      CNSendMessage(LM_SETPROPERTIES,Self,nil);
     end;

end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetWordWrap
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetWordWrap(Value : Boolean);
Begin
   if fWordWrap <> value then
     Begin
      fWordWrap := value;
      HandleNeeded;
      invalidate;
      CNSendMessage(LM_SETPROPERTIES,Self,nil);
     end;

end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.GetLabelText
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCustomLabel.GetLabelText: String;
begin
   GetLabelText := Caption;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csLabel;
  FLayout := tlBottom;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];

  SetBounds(0, 0, 65, 17);
end;

{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.3  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment


}
