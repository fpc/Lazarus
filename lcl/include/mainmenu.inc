// included by menus.pp
{******************************************************************************
                                  TMainMenu
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method: TMainMenu.ItemChanged
  Params:  none
  Returns: Nothing

  Called whenever
 ------------------------------------------------------------------------------}
procedure TMainMenu.ItemChanged;
begin
  {MenuChanged(nil, nil, False);
  if FWindowHandle <> 0 then
    SendMessage(FWindowHandle, CM_MENUCHANGED, 0, 0);}
end;

{------------------------------------------------------------------------------
  Method: TMainMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMainMenu.Create(AOwner : TComponent);
begin
  FCompStyle := csMainMenu;
  inherited Create(AOwner);
end;

// included by menus.pp

{ =============================================================================

  $Log$
  Revision 1.5  2002/11/12 10:16:17  lazarus
  MG: fixed TMainMenu creation

  Revision 1.4  2002/08/07 09:55:30  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.3  2002/05/10 06:05:53  lazarus
  MG: changed license to LGPL

  Revision 1.2  2001/04/13 17:56:17  lazarus
  MWE:
  * Moved menubar outside clientarea
  * Played a bit with the IDE layout
  * Moved the creation of the toolbarspeedbuttons to a separate function

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

