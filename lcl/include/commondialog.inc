{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TCommonDialog
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
  Method: TCommonDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCommonDialog.Create (TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

function TCommonDialog.Execute: boolean;
begin
  FUserChoice := mrNone;
  Handle := TWSCommonDialogClass(WidgetSetClass).CreateHandle(Self);
  Result:= DoExecute;
  Close;
end;

procedure TCommonDialog.Close;
begin
  DoClose;
  TWSCommonDialogClass(WidgetSetClass).DestroyHandle(Self);
  FHandle := 0;
end;

procedure TCommonDialog.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TCommonDialog.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(Self);
end;

function TCommonDialog.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

procedure TCommonDialog.SetHandle(const AValue: THandle);
begin
  FHandle:=AValue;
end;

procedure TCommonDialog.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
end;

procedure TCommonDialog.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

function TCommonDialog.DoExecute : boolean;
var CanClose: boolean;
begin
  if Assigned(FOnShow) then FOnShow(Self);
  TWSCommonDialogClass(WidgetSetClass).ShowModal(Self);
  repeat
    if (FUserChoice <> mrNone) and (Handle<>0)
    and (OnCanClose<>nil) then begin
      CanClose:=true;
      OnCanClose(Self,CanClose);
      if not CanClose then FUserChoice:=mrNone;
    end;
    if FUserChoice <> mrNone then
      break;
    { win32 widgetset dialogs use their own message loop,
      so only FUserChoice may have been set already }
    Application.HandleMessage;
  until false;
  Result := (FUserChoice = mrOk);
  if Assigned(FOnClose) then FOnClose(Self);
end;

{
  $Log$
  Revision 1.20  2005/07/04 13:49:18  micha
  fix bug 936: waits for event after dialog

  Revision 1.19  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.18  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.17  2004/09/18 12:43:15  micha
  convert LM_DESTROY message to interface methods

  Revision 1.16  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.15  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.14  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.13  2004/04/03 13:10:20  mattias
  fixed TCalendarDialog.DialogTitle

  Revision 1.12  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.10  2003/09/04 10:51:30  mattias
  fixed default size of preview widget

  Revision 1.9  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.8  2003/03/29 19:15:30  mattias
  fixed untransienting

  Revision 1.7  2003/03/17 13:00:35  mattias
  improved but not fixed transient windows

  Revision 1.6  2003/03/15 09:42:49  mattias
  fixed transient windows

  Revision 1.5  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.4  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.3  2001/12/11 14:36:41  lazarus
  MG: started multiselection for TOpenDialog

  Revision 1.2  2001/08/05 10:14:50  lazarus
  MG: removed double props in OI, small bugfixes

  Revision 1.1  2000/08/10 11:01:06  lazarus
  Adding commondialog.pp and removing customdialog.inc

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.8  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.7  2000/03/24 14:40:41  lazarus
  A little polishing and bug fixing.

  Revision 1.6  2000/02/28 19:16:03  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.5  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.4  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.3  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick


}
