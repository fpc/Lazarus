{******************************************************************************
                                  TPopupMenu
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
  procedure TPopupMenu.DoPopup(Sender: TObject);

  Creates the popup window and shows it.
 ------------------------------------------------------------------------------}
procedure TPopupMenu.DoPopup(Sender: TObject);
begin
  if Assigned(FOnPopup) then FOnPopup(Sender);
end;

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

destructor TPopupMenu.Destroy;
begin
  Close;
  inherited Destroy;
end;

Procedure TPopupMenu.PopUp(X,Y : Integer);
begin
  if ActivePopupMenu<>nil then ActivePopupMenu.Close;
  FPopupPoint := Point(X, Y);
  ReleaseCapture;
  DoPopup(Self);
  if Items.Count=0 then exit;
  ActivePopupMenu:=Self;
  HandleNeeded;
  SendMsgToInterface(LM_POPUPSHOW, Self, @FPopupPoint);
end;

procedure TPopupMenu.Close;
begin
  if HandleAllocated then FItems.DestroyHandle;
  if ActivePopupMenu=Self then begin
    DoClose;
    ActivePopupMenu:=nil;
  end;
end;

procedure TPopupMenu.DoClose;
begin
  if Assigned(OnClose) then OnClose(Self);
end;


{
  $Log$
  Revision 1.9  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.8  2003/05/12 13:40:50  mattias
  fixed clsing popupmenu on showmodal

  Revision 1.7  2003/04/06 22:39:47  mattias
  implemented searching packages

  Revision 1.6  2002/10/26 15:15:48  lazarus
  MG: broke LCL<->interface circles

  Revision 1.5  2002/08/17 07:57:05  lazarus
  MG: added TPopupMenu.OnPopup and SourceEditor PopupMenu checks

  Revision 1.4  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

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

