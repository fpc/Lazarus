{******************************************************************************
                                  TMainMenu
 ******************************************************************************}
{------------------------------------------------------------------------------
  Method: TMainMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMainMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csMenuBar;
end;


{ =============================================================================

  $Log$
  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.2  1999/11/05 00:34:11  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.1  1999/10/27 00:30:51  lazarus
  MWE: Added for compatibility and future use

}

