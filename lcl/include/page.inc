{******************************************************************************
                                     TPage
 ******************************************************************************}
{------------------------------------------------------------------------------}
{  TPage AddControl                                                         }
{------------------------------------------------------------------------------}
procedure TPage.AddControl;
begin
  // nothing yet
  // put here the call to LM_AddPage
end;

{------------------------------------------------------------------------------
  TPage Constructor
 ------------------------------------------------------------------------------}
constructor TPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {set the default height and widths}

  {create the control}
  fCompStyle := csPage;

  Visible := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  { The alignment is performed by the notebook component }

  Caption := '';
end;

{------------------------------------------------------------------------------
  Method:  TPage.AttachSignals
  Params:  None
  Returns: Nothing

  Attaches signals.
 ------------------------------------------------------------------------------}
procedure TPage.AttachSignals; 
begin
  inherited AttachSignals;

  Assert(False, 'Trace:[TPage.AttachSignals] Assign Callback Signals');  
  SetCallback(LM_Paint);
end;

{------------------------------------------------------------------------------
  TPage ReadState
 ------------------------------------------------------------------------------}
procedure TPage.ReadState(Reader: TAbstractReader);
begin
   inherited ReadState(Reader);

  if Reader.Parent is TNoteBook then begin
    with TNBPages(TNoteBook(Reader.Parent).fAccess) do
      InsertPage(Count,Self);
  end;

end;

{------------------------------------------------------------------------------
  TPage Paint
 ------------------------------------------------------------------------------}
procedure TPage.Paint;
begin
  { Nothing to do here yet }
end;

{------------------------------------------------------------------------------
  TPage WMPaint
  Params: a TLMPaint message
 ------------------------------------------------------------------------------}
procedure TPage.WMPaint(var Msg: TLMPaint);
var Notebook: TNoteBook;
begin
  if (Parent is TNoteBook) then begin
    NoteBook:=TNoteBook(Parent);
    if NoteBook.Page[NoteBook.PageIndex]=Self then
      inherited WMPaint(Msg);
  end else
    inherited WMPaint(Msg);
end;

{------------------------------------------------------------------------------
  TPage Destroy
 ------------------------------------------------------------------------------}
destructor TPage.Destroy;
begin
   inherited Destroy;
end;

{
  $Log$
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

