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
  FCompStyle := csPopupMenu; 
  FAutoPopup := True;
end;

Procedure TPopupMenu.PopUp(X,Y : Integer);
var
  P : TPoint;
begin
  P.X := X;
  P.Y := Y;
  HandleNeeded;
 InterfaceObject.IntSendMessage3(LM_POPUPSHOW, Self, @P);
end;


{
  $Log$
  Revision 1.3  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.2  2000/12/22 19:55:38  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

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

