// included by extctrls.pp

{******************************************************************************
                                   TNBPages
 ******************************************************************************}

{ $DEFINE NOTEBOOK_DEBUG}

{------------------------------------------------------------------------------
  TNBPages Constructor
 ------------------------------------------------------------------------------}
constructor TNBPages.Create(thePageList: TList; theNotebook: TCustomNotebook);
begin
  inherited Create;
  { Create the page list and a notebook }
  fPageList := thePageList;
  fNotebook := theNotebook;
end;

{------------------------------------------------------------------------------
  TNBPages Get
 ------------------------------------------------------------------------------}
function TNBPages.Get(Index: Integer): String;
begin
//writeln('TNBPages.Get Index=',Index);
  Result := TPage(fPageList[Index]).Caption;
end;

{------------------------------------------------------------------------------
  TNBPages GetCount
 ------------------------------------------------------------------------------}
function TNBPages.GetCount: Integer;
begin
  Result := fPageList.Count;
end;

{------------------------------------------------------------------------------
  TNBPages GetObject
 ------------------------------------------------------------------------------}
function TNBPages.GetObject(Index: Integer): TObject;
begin
  Result := TPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TNBPages Put
 ------------------------------------------------------------------------------}
procedure TNBPages.Put(Index: Integer; const S: String);
var
  Msg: TLMNotebookEvent;
begin
  TPage(fPageList[Index]).Caption := S;
  TPage(fPageList[Index]).Name := S;

  if FNoteBook.HandleAllocated 
  then begin
    Msg.Parent := fNotebook;
    Msg.Child := TPage(fPageList[Index]);
    Msg.fCompStyle := fNotebook.fCompStyle;
    Msg.Str := S;
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TNBPages.Put] A ',Index,' ',S);
{$ENDIF}
    CNSendMessage(LM_SETTEXT, fNotebook, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TNBPages.Put] B ');
{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  TNBPages Clear
 ------------------------------------------------------------------------------}
procedure TNBPages.Clear;
var
  i: Integer;
begin
  for i := 0 to fPageList.Count - 1 do
    TPage(fPageList[I]).Free;
  fPageList.Clear;
end;

{------------------------------------------------------------------------------
  TNBPages Delete
 ------------------------------------------------------------------------------}
procedure TNBPages.Delete(Index: Integer);
var
  Msg: TLMNotebookEvent;
  NewPageIndex: integer;
begin
  // Make sure Index is in the range of valid pages to delete 
{$IFDEF NOTEBOOK_DEBUG}
//writeln('TNBPages.Delete A Index=',Index);
writeln('TNBPages.Delete B Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
{$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    // If that page is showing, then show the next page before deleting it
    NewPageIndex:=fNoteBook.PageIndex;
    if (Index = fNoteBook.PageIndex) then begin
      if NewPageIndex<fPageList.Count-1 then
        // switch current page to next (right) page
        inc(NewPageIndex)
      else if fPageList.Count>1 then
        // switch to previous (left) page
        dec(NewPageIndex)
      else
        // deleting last page
        // TODO: delete contents but not last page
        exit;
    end;
    fNoteBook.PageIndex:=NewPageIndex;
    if FNoteBook.HandleAllocated then begin
      Msg.Parent := fNotebook;
      Msg.fCompStyle := fNotebook.fCompStyle;
      Msg.Page := Index;
      CNSendMessage(LM_REMOVEPAGE, fNotebook, @Msg);
    end;
    TPage(fPageList[Index]).Free;
    fPageList.Delete(Index);
    if NewPageIndex>=Index then
      fNoteBook.PageIndex:=NewPageIndex-1;
  end;
{$IFDEF NOTEBOOK_DEBUG}
writeln('TNBPages.Delete END Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
{$ENDIF}
end;


{------------------------------------------------------------------------------
  TNBPages Insert
 ------------------------------------------------------------------------------}
procedure TNBPages.Insert(Index: Integer; const S: String);
var
  tmpPage: TPage;
begin
  tmpPage := TPage.Create(fNotebook);
  with tmpPage do
  begin
    Name    := S;
    Parent := fNotebook;
    Caption := S;
    Visible := true;
  end;

  InsertPage(Index,tmpPage);
end;

{------------------------------------------------------------------------------
  TNBPages InsertPage
 ------------------------------------------------------------------------------}
procedure TNBPages.InsertPage(Index:integer; APage: TPage);
var
  Msg: TLMNotebookEvent;
begin
  fPageList.Insert(Index,APage);

  if FNoteBook.HandleAllocated
  then begin
    Msg.Parent := TControl(fNotebook);
    Msg.Child := APage;
    Msg.fCompStyle := fNotebook.fCompStyle;
    Msg.Page := Index;

{$IFDEF NOTEBOOK_DEBUG}
writeln('[TNBPages.InsertPage] A ',Index);
{$ENDIF}
    CNSendMessage(LM_ADDPAGE, fNotebook, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TNBPages.InsertPage] B ');
{$ENDIF}

    fNoteBook.PageIndex := Index;
  end;
end;

{------------------------------------------------------------------------------
  TNBPages Move
 ------------------------------------------------------------------------------}
procedure TNBPages.Move(CurIndex, NewIndex: Integer);
// ToDo
//var
// theObject: TObject;
begin
  //fPageList.Move(CurIndex, NewIndex);
  //theObject := fPageList[CurIndex];
  //fPageList[CurIndex] := fPageList[NewIndex];
  //fPageList[NewIndex] := theObject;
  //MoveThePage(CurIndex, NewIndex);
  { Still need to implement }
end;


{******************************************************************************
                                TCustomNotebook
 ******************************************************************************}
{------------------------------------------------------------------------------
  TCustomNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TCustomNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {create the control}
  fCompStyle := csNoteBook;

  fPageList := TList.Create;

  fAccess := TNBPages.Create(fPageList, Self);
  fPageIndex := -1;
  { Create the first notebook page }
  fAccess.Add('Default');

  fPageIndex := 0;
  //Exclude(fComponentStyle, csInheritable);

  TabPosition := tpTop;
  ShowTabs := False;
end;

{------------------------------------------------------------------------------
  Method:  TCustomNotebook.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CreateWnd;
var
  n: Integer;
  Msg: TLMNotebookEvent;
begin
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.CreateWnd] A');
{$ENDIF}
  inherited CreateWnd;
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.CreateWnd] B');
{$ENDIF}

  Assert(False, 'Trace:[TCustomNotebook.CreateWnd] add pages');
  for n := 0 to FPageList.Count -1 do begin
    // this is workaround til visible=true is default in TControl
    TControl(FPageList[n]).Visible:=true;
    
    Msg.Parent := Self;
    Msg.Child := TControl(FPageList[n]);
    Msg.fCompStyle := FCompStyle;
    Msg.Page := n;
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.CreateWnd] C ',n);
{$ENDIF}
    CNSendMessage(LM_ADDPAGE, Self, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.CreateWnd] D');
{$ENDIF}
  end;

  SetShowTabs(FShowTabs);
  SetTabPosition(fTabPosition);
  SetPageIndex(FPageIndex);

{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.CreateWnd] END');
{$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TCustomNotebook.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomNotebook.Destroy;
begin
  fAccess.Free;
  fAccess:=nil;
  fPageList.Free;
  fPageList:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetActivePage
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetActivePage: String;
begin
  Result := TPage(fPageList[PageIndex]).Caption;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetActivePage
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetActivePage(const Value: String);
var
  i: Integer;
begin
  for i := 0 to fPageList.Count - 1 do
  begin
    if TPage(fPageList[i]).Caption = Value then
    begin
      SetPageIndex(i);
      Break;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetPageIndex(Value: Integer);
var
  Msg: TLMNotebookEvent;
begin
  fPageIndex := Value;
  if HandleAllocated then begin
    Msg.Parent := Self;
    Msg.fCompStyle := fCompStyle;
    Msg.Page := Value;
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetPageIndex] A');
{$ENDIF}
    CNSendMessage(LM_SETITEMINDEX, Self, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetPageIndex] B');
{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPageIndex
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPageIndex: Integer;
//var
//  Msg: TLMNotebookEvent;
begin
//we don't have to query the contol.  FPageindex should track this along with the pagechanged handler.
{  if HandleAllocated
  then begin
    Msg.Parent := Self;
    Msg.fCompStyle := fCompStyle;
    CNSendMessage(LM_GETITEMINDEX, Self, @Msg);
    fPageIndex := Msg.Page;
  end;}

  Result := fPageIndex;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPageCount
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPageCount: Integer;
begin
  Result := fPageList.Count;
end;


{------------------------------------------------------------------------------
  TCustomNotebook SetPages
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetPages(Value: TStrings);
begin
  FAccess.Assign(Value);
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPage
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPage(aIndex: Integer): TPage;
begin
  Result := TPage(fPageList.Items[aIndex]);
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetShowTabs
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetShowTabs(Value: Boolean);
var
  Msg: TLMNotebookEvent;
begin
  fShowTabs := Value;
  if HandleAllocated
  then begin
    Msg.Parent := Self;
    Msg.fCompStyle := fCompStyle;
    Msg.ShowTabs := fShowTabs;
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetShowTabs] A');
{$ENDIF}
    CNSendMessage(LM_SHOWTABS, Self, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetShowTabs] B');
{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetTabPosition
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetTabPosition(tabPos: TTabPosition);
var
  Msg: TLMNotebookEvent;
begin
  fTabPosition := tabPos;
  if HandleAllocated
  then begin
    Msg.Parent := Self;
    Msg.fCompStyle := fCompStyle;
    Msg.TabPosition := @fTabPosition;
    //InterfaceObject.IntCNSendMessage2(LM_SETTABPOSITION, Self, nil, @fTabPosition);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetTabPosition] A');
{$ENDIF}
    CNSendMessage(LM_SETTABPOSITION, Self, @Msg);
{$IFDEF NOTEBOOK_DEBUG}
writeln('[TCustomNotebook.SetTabPosition] B');
{$ENDIF}
  end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook CreateParams
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CreateParams(var Params: TCreateParams);
begin
  { This function exists for Delphi compatibility only. May be necessary
    when the Windows API is implemented }
  inherited CreateParams(Params);
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetChildOwner
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetChildOwner: TComponent;
begin
  Result := Self;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetChildren
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: LongInt;
begin
  for i := 0 to fPageList.Count - 1 do
    Proc(TControl(fPageList[i]));
end;

{------------------------------------------------------------------------------
  TCustomNotebook ReadState
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.ReadState(Reader: TAbstractReader);
begin
  fAccess.Clear;
  inherited ReadState(Reader);
end;

{------------------------------------------------------------------------------
  TCustomNotebook ShowControl
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.ShowControl(AControl: TControl);
var
  i: LongInt;
begin
  //inherited ShowControl(AControl);
  { Find a child control that matches the one passed in and display
    the page that contains that control. This method is necessary
    for compatibility with Delphi }
  for i := 0 to fPageList.Count - 1 do begin
    if TControl(fPageList[i]) = AControl then begin
      PageIndex := i;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook Change
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.Change;
Begin
  if Assigned(fOnPageChanged) then fOnPageChanged(self);
end;

{------------------------------------------------------------------------------
  TCustomNotebook CNNotify
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CNNotify(var Message : TLMNotify);
Begin
  with Message do
    Case NMHdr^.code of
      TCN_SELCHANGE:
        Begin
          //set the page from the NMHDR^.idfrom
          FPageIndex := NMHDR^.idfrom;
          Change;
        end;
    else
      begin
        writeln('');
        writeln('[TCustomNotebook.CNNotify]');
        writeln('');
      end;
    end;
end;

{------------------------------------------------------------------------------
  TCustomNotebook InternalSetMultiLine
 ------------------------------------------------------------------------------}
{function TCustomNotebook.InternalSetMultiLine(Value: boolean): boolean;
begin
  Result := FMultiLine <> Value;
  if Result then begin
    if not Value and ((TabPosition = tpLeft) or (TabPosition = tpRight)) then
      raise Exception.Create(
        'TCustomNotebook.InternalSetMultiLine: Tab must be multiline');
    FMultiLine := Value;
    //if not Value then FScrollOpposite := False;
  end;
end;}

{------------------------------------------------------------------------------
  TCustomNotebook SetMultiLine
 ------------------------------------------------------------------------------}
{procedure TCustomNotebook.SetMultiLine(Value: boolean);
begin
  if InternalSetMultiLine(Value) then RecreateWnd;
end;}



{ =============================================================================

  $Log$
  Revision 1.14  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.13  2002/02/24 20:51:24  lazarus
  Improved TSpeedButton (Glyph, Spacing, Margin, drawing)
  Added PageCount to TNotebook
  Optimized component selection buttons a bit.

  Revision 1.12  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.11  2001/11/05 18:18:19  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.10  2001/10/16 10:51:10  lazarus
  MG: added clicked event to TButton, MessageDialog reacts to return key

  Revision 1.9  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.8  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.7  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.5  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.4  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.3  2001/01/12 18:27:32  lazarus
  Streaming additions by MAttias
  Shane

  Revision 1.2  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.4  2000/06/29 18:08:56  lazarus
  Shane
    Looking for the editor problem I made a few changes.  I changed everything back to the original though.

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.16  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.15  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.14  2000/02/26 00:09:06  lazarus
  MWE:
    Temorary removed focus on insert of a new page

  Revision 1.13  2000/02/25 19:28:34  lazarus
  Played with TNotebook to see why it crashes when I add a tab and the tnotebook is showing.  Havn't figured it out
  Shane

  Revision 1.12  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.11  2000/01/04 21:00:34  lazarus
  *** empty log message ***

  Revision 1.10  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.9  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.8  1999/10/22 21:01:50  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.7  1999/10/04 23:40:33  lazarus
  Implemented the GetChildren and ShowControl methods. GetChildren needs
  some more work. See the comments for a description.            CAW

  Revision 1.6  1999/09/30 21:59:01  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.5  1999/09/22 20:07:14  lazarus
  *** empty log message ***

  Revision 1.3  1999/09/21 23:46:53  lazarus
  *** empty log message ***

  Revision 1.2  1999/09/13 03:25:00  lazarus
  Modified to utilize the new gtkint IntCNSendMessage3 function.       caw

  Revision 1.1  1999/08/04 05:24:21  lazarus
  Added new TCustomNotebook component.        CAW

}

