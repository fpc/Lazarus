// included by extctrls.pp
{******************************************************************************
                                     TPage
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
  TPage AddControl
------------------------------------------------------------------------------}
procedure TPage.AddControl;
begin
  // nothing yet
end;

{------------------------------------------------------------------------------
  TPage Constructor
 ------------------------------------------------------------------------------}
constructor TPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  fImageIndex:=-1;
  
  {set the default height and width}

  if (AOwner<>nil) and (AOwner is TControl) then begin
    Width:=(TControl(AOwner).ClientWidth);
    Height:=(TControl(AOwner).ClientHeight);
  end else begin
    Width:=120;
    Height:=100;
  end;

  {create the control}
  fCompStyle := csPage;

  Visible := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  { The alignment is performed by the notebook component }

  Caption := '';
end;

{------------------------------------------------------------------------------
  method TPage SetImageIndex
  Params: const AValue: integer
  Result: none
  
  Set the image index of the image shown in the tabs.
 ------------------------------------------------------------------------------}
procedure TPage.SetImageIndex(const AValue: integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_NB_UpdateTab, Self, nil);
end;

{------------------------------------------------------------------------------
  TPage Paint
 ------------------------------------------------------------------------------}
{procedure TPage.Paint;
begin
end;}

{------------------------------------------------------------------------------
  TPage WMPaint
  Params: a TLMPaint message
 ------------------------------------------------------------------------------}
procedure TPage.WMPaint(var Msg: TLMPaint);
var Notebook: TNoteBook;
begin
  if (Parent is TNoteBook) then begin
    NoteBook:=TNoteBook(Parent);
    if (NoteBook.PageIndex>=0) and (NoteBook.Page[NoteBook.PageIndex]=Self) then
      inherited WMPaint(Msg);
  end else
    inherited WMPaint(Msg);
end;

{------------------------------------------------------------------------------
  procedure TPage.SetParent(AParent: TWinControl);
  
  Set parent wincontrol.
 ------------------------------------------------------------------------------}
procedure TPage.SetParent(AParent: TWinControl);
var
  OldParent: TWinControl;
  ParentNotebook: TCustomNotebook;
  i: integer;
begin
  OldParent:=Parent;
  if (OldParent<>AParent) and (OldParent<>nil)
  and (OldParent is TCustomNotebook) then begin
    // remove from old pagelist
    ParentNotebook:=TCustomNotebook(OldParent);
    i:=PageIndex;
    if i>=0 then
      TNBPages(ParentNotebook.fAccess).RemovePage(i);
  end;

  inherited SetParent(AParent);
  
  if (OldParent<>AParent) and (Parent<>nil)
  and (Parent is TCustomNotebook) then begin
    // add to new pagelist
    ParentNotebook:=TCustomNotebook(Parent);
    if ParentNotebook.PageList.IndexOf(Self)<0 then
      TNBPages(ParentNotebook.fAccess).InsertPage(ParentNotebook.PageCount,Self);
  end;
end;

{------------------------------------------------------------------------------
  TPage Destroy
 ------------------------------------------------------------------------------}
destructor TPage.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TPage AdjustClientRect
  Params: Rect
 ------------------------------------------------------------------------------}
procedure TPage.AdjustClientRect(var ARect: TRect);
begin
  ARect:=GetClientRect;
end;

{------------------------------------------------------------------------------
  function TPage.PageIndex: integer;
  
  Returns the index of the page in the notebook.
 ------------------------------------------------------------------------------}
function TPage.PageIndex: integer;
begin
  if (Parent<>nil) and (Parent is TCustomNotebook) then
    Result:=TCustomNotebook(Parent).PageList.IndexOf(Self);
end;

function TPage.CanTab: boolean;
begin
  Result:=false;
end;

// included by extctrls.pp
{
  $Log$
  Revision 1.13  2003/06/10 17:23:35  mattias
  implemented tabstop

  Revision 1.12  2002/12/22 22:42:55  mattias
  custom controls now support child wincontrols

  Revision 1.11  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.10  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.9  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.8  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.7  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.6  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.5  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.4  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.3  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.2  2001/01/12 18:27:32  lazarus
  Streaming additions by MAttias
  Shane

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.13  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.12  2000/02/25 19:28:34  lazarus
  Played with TNotebook to see why it crashes when I add a tab and the tnotebook is showing.  Havn't figured it out
  Shane

  Revision 1.11  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.10  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.9  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.8  1999/10/05 02:40:22  lazarus
  Cleaned up the code.       CAW

  Revision 1.7  1999/09/30 21:59:03  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.6  1999/09/22 20:07:16  lazarus
  *** empty log message ***

  Revision 1.4  1999/09/21 23:46:55  lazarus
  *** empty log message ***

  Revision 1.3  1999/08/03 06:34:26  lazarus
  Added cvs logging to bottom of unit

}

