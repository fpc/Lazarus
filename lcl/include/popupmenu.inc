{******************************************************************************
                                  TPopupMenu
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TPopupMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TPopupMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;


{
  $Log$
  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  1999/11/05 00:34:11  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.2  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

  Revision 1.1  1999/10/27 00:31:07  lazarus
  MWE: Added for compatibility and future use

}

