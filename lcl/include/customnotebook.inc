// included by extctrls.pp

{******************************************************************************
                                   TNBPages
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
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TNBPages.Get Index out of bounds');
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
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TNBPages.GetObject Index out of bounds');
  Result := TPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TNBPages Put
 ------------------------------------------------------------------------------}
procedure TNBPages.Put(Index: Integer; const S: String);
var
  Msg: TLMNotebookEvent;
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TNBPages.Put Index out of bounds');
  TPage(fPageList[Index]).Caption := S;

  if (FNoteBook.HandleAllocated)
  and (not (csLoading in FNoteBook.ComponentState))
  then begin
    Msg.Parent := fNotebook;
    Msg.Child := TPage(fPageList[Index]);
    Msg.fCompStyle := fNotebook.fCompStyle;
    Msg.Str := S;
    {$IFDEF NOTEBOOK_DEBUG}
    writeln('[TNBPages.Put] A ',fNoteBook.Name,' ',Index,' ',S);
    {$ENDIF}
    CNSendMessage(LM_SetLabel, fNotebook, @Msg);
    {$IFDEF NOTEBOOK_DEBUG}
    writeln('[TNBPages.Put] B ',fNoteBook.Name);
    {$ENDIF}
  end;
end;

procedure TNBPages.RemovePage(Index: integer);
var
  Msg: TLMNotebookEvent;
  NewPageIndex: integer;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.RemovePage A ',fNoteBook.Name,' Index=',Index,
    ' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    if not (csLoading in fNoteBook.ComponentState) then begin
      // If that page is showing, then show the next page before deleting it
      NewPageIndex:=fNoteBook.PageIndex;
      if (Index = fNoteBook.PageIndex) then begin
        if NewPageIndex<fPageList.Count-1 then
          // switch current page to next (right) page
          inc(NewPageIndex)
        else if fPageList.Count>0 then
          // switch to previous (left) page
          dec(NewPageIndex)
        else
          // deleting last page
          NewPageIndex:=-1;
          
        if NewPageIndex>=0 then
          fNoteBook.PageIndex:=NewPageIndex;
      end;
    end;
    if (FNoteBook.HandleAllocated) and (TPage(fPageList[Index]).HandleAllocated)
    then begin
      Msg.Parent := fNotebook;
      Msg.fCompStyle := fNotebook.fCompStyle;
      Msg.Page := Index;
      CNSendMessage(LM_REMOVEPAGE, fNotebook, @Msg);
    end;
    fPageList.Delete(Index);
    if not (csLoading in fNoteBook.ComponentState) then begin
      if NewPageIndex>=Index then
        fNoteBook.PageIndex:=NewPageIndex-1;
    end;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.RemovePage END ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages Clear
 ------------------------------------------------------------------------------}
procedure TNBPages.Clear;
begin
  while fPageList.Count>0 do
    Delete(fPageList.Count-1);
end;

{------------------------------------------------------------------------------
  TNBPages Delete
 ------------------------------------------------------------------------------}
procedure TNBPages.Delete(Index: Integer);
var
  APage: TPage;
begin
  // Make sure Index is in the range of valid pages to delete 
  {$IFDEF NOTEBOOK_DEBUG}
  //writeln('TNBPages.Delete A Index=',Index);
  writeln('TNBPages.Delete B ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    APage:=TPage(fPageList[Index]);
    RemovePage(Index);
    APage.Free;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.Delete END ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages Insert
 ------------------------------------------------------------------------------}
procedure TNBPages.Insert(Index: Integer; const S: String);
var
  tmpPage: TPage;
  NewOwner: TComponent;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.Insert A ',fNoteBook.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
  NewOwner:=fNotebook.Owner;
  if NewOwner=nil then
    NewOwner:=fNotebook;
  tmpPage := TPage.Create(fNotebook.Owner);
  with tmpPage do
  begin
    Caption := S;
    Visible := true;
  end;

  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.Insert B ',fNoteBook.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
  InsertPage(Index,tmpPage);
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.Insert END ',fNoteBook.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages InsertPage
 ------------------------------------------------------------------------------}
procedure TNBPages.InsertPage(Index:integer; APage: TPage);
var
  Msg: TLMNotebookEvent;
  NewZPosition: integer;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.InsertPage A ',fNoteBook.Name,' Index=',Index,' Name=',APage.Name,' Caption=',APage.Caption);
  {$ENDIF}
  if Index<fPageList.Count then
    NewZPosition:=fNoteBook.GetControlIndex(TPage(fPageList[Index]))
  else
    NewZPosition:=-1;
  fPageList.Insert(Index,APage);
  APage.Parent := fNotebook;
  if NewZPosition>=0 then
    fNoteBook.SetControlIndex(APage,NewZPosition);
  APage.Visible:=true;
  fNoteBook.UpdateDesignerFlags(Index);

  if FNoteBook.HandleAllocated
  and (not (csLoading in FNoteBook.ComponentState))
  then begin
    Msg.Parent := TControl(fNotebook);
    Msg.Child := APage;
    Msg.fCompStyle := fNotebook.fCompStyle;
    Msg.Page := Index;

    CNSendMessage(LM_ADDPAGE, fNotebook, @Msg);
    Include(APage.FFlags,pfAdded);
    if fNoteBook.PageIndex = Index then
      fNoteBook.DoSendPageIndex
    else
      fNoteBook.PageIndex := Index;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TNBPages.InsertPage END ',fNoteBook.Name,' Index=',Index,' Name=',APage.Name,' Caption=',APage.Caption);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages Move
 ------------------------------------------------------------------------------}
procedure TNBPages.Move(CurIndex, NewIndex: Integer);
var
  APage: TPage;
  Msg: TLMNotebookEvent;
  NewControlIndex, NewPageIndex: integer;
begin
  if CurIndex=NewIndex then exit;
  
  APage:=TPage(fPageList[CurIndex]);
  
  // calculate new control index (i.e. ZOrderPosition)
  if NewIndex>=fPageList.Count-1 then
    NewControlIndex:=fNoteBook.ControlCount-1
  else
    NewControlIndex:=fNoteBook.GetControlIndex(TPage(fPageList[NewIndex]));
    
  // calculate new PageIndex
  if fNoteBook.PageIndex=CurIndex then
    NewPageIndex:=NewIndex
  else if fNoteBook.PageIndex>CurIndex then begin
    if fNoteBook.PageIndex<=NewIndex then
      NewPageIndex:=fNoteBook.PageIndex-1;
  end else begin
    if fNoteBook.PageIndex>=NewIndex then
      NewPageIndex:=fNoteBook.PageIndex+1;
  end;

  // move Page in fPageList
  fPageList.Move(CurIndex, NewIndex);
  
  // move in wincontrol list
  fNoteBook.SetControlIndex(APage,NewControlIndex);

  // move Page in notebook handle
  if FNoteBook.HandleAllocated
  and (not (csLoading in FNoteBook.ComponentState))
  then begin
    Msg.Parent := TControl(fNotebook);
    Msg.Child := APage;
    Msg.fCompStyle := fNotebook.fCompStyle;
    Msg.Page := NewIndex;

    CNSendMessage(LM_MOVEPAGE, fNotebook, @Msg);
  end;

  // update PageIndex
  fNoteBook.PageIndex:=NewPageIndex;
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

  TabPosition := tpTop;
  ShowTabs := True;
end;

{------------------------------------------------------------------------------
  Method:  TCustomNotebook.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CreateWnd;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TCustomNotebook.CreateWnd ',Name,':',ClassName,' HandleAllocated=',HandleAllocated);
  {$ENDIF}
  inherited CreateWnd;
  DoCreateWnd;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoCreateWnd;

  Creates the handles for the pages and updates the notebook handle.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoCreateWnd;
var
  i: Integer;
  Msg: TLMNotebookEvent;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('TCustomNotebook.DoCreateWnd ',Name,':',ClassName,' HandleAllocated=',HandleAllocated);
  {$ENDIF}
  fAddingPages:=true;
  for i := 0 to FPageList.Count -1 do begin
    {$IFDEF NOTEBOOK_DEBUG}
    writeln('TCustomNotebook.DoCreateWnd ',Name,':',ClassName,' ',Page[i].Caption,' ',not (pfAdded in Page[i].Flags));
    {$ENDIF}
    if not (pfAdded in Page[i].Flags) then begin
      Msg.Parent := Self;
      Msg.Child := TControl(FPageList[i]);
      Msg.fCompStyle := FCompStyle;
      Msg.Page := i;
      CNSendMessage(LM_ADDPAGE, Self, @Msg);
      Include(Page[i].FFlags,pfAdded);
    end;
  end;
  fAddingPages:=false;

  DoSendShowTabs;
  DoSendTabPosition;
  DoSendPageIndex;
end;

{------------------------------------------------------------------------------
  Method: TCustomNotebook.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomNotebook.Destroy;
begin
  Pages.Clear;
  fAccess.Free;
  fAccess:=nil;
  fPageList.Free;
  fPageList:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  function TCustomNotebook.TabIndexAtClientPos(ClientPos: TPoint): integer;
  
  Returns the index of the page of the tab at the client position.
  For example:
    Index:=NoteBook1.PageIndexAtClientPos(
               NoteBook1.ScreenToClient(Mouse.CursorPos));
 ------------------------------------------------------------------------------}
function TCustomNotebook.TabIndexAtClientPos(ClientPos: TPoint): integer;
begin
  if HandleAllocated then
    Result:=LCLLinux.GetNotebookTabIndexAtPos(Handle,ClientPos)
  else
    Result:=-1;
end;

{------------------------------------------------------------------------------
  method TCustomNotebook DoCloseTabClicked
  Params: APage: TPage
  Result: none
  
  Called whenever the user closes the tab.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoCloseTabClicked(APage: TPage);
begin
  if Assigned(OnCloseTabClicked) then OnCloseTabClicked(APage);
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetActivePage
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetActivePage: String;
begin
  if (PageIndex>=0) and (PageIndex<PageCount) then
    Result := TPage(fPageList[PageIndex]).Caption
  else
    Result:='';
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

procedure TCustomNotebook.SetImages(const AValue: TImageList);
begin
  if FImages=AValue then exit;
  FImages:=AValue;
  UpdateTabProperties;
end;

procedure TCustomNotebook.SetOptions(const AValue: TNoteBookOptions);
var ChangedOptions: TNoteBookOptions;
begin
  if FOptions=AValue then exit;
  ChangedOptions:=(FOptions-AValue)+(AValue-FOptions);
  FOptions:=AValue;
  if nboShowCloseButtons in ChangedOptions then
    UpdateTabProperties;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetPageIndex(Value: Integer);
begin
  if fPageIndex = Value then exit;
  fPageIndex := Value;
  UpdateAllDesignerFlags;
  DoSendPageIndex;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPageIndex
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPageIndex: Integer;
begin
  Result := fPageIndex;
end;

{------------------------------------------------------------------------------
  function TCustomNotebook.IsStoredActivePage: boolean;
 ------------------------------------------------------------------------------}
function TCustomNotebook.IsStoredActivePage: boolean;
begin
  Result:=false;
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
  if (aIndex<0) or (aIndex>=fPageList.Count) then
    RaiseGDBException('TCustomNotebook.GetPage Index out of bounds');
  Result := TPage(fPageList.Items[aIndex]);
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetShowTabs
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetShowTabs(AValue: Boolean);
begin
  if fShowTabs=AValue then exit;
  fShowTabs := AValue;
  DoSendShowTabs;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetTabPosition
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetTabPosition(tabPos: TTabPosition);
begin
  if fTabPosition = tabPos then exit;
  fTabPosition := tabPos;
  DoSendTabPosition;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.UpdateAllDesignerFlags;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i:=0 to PageCount-1 do
    UpdateDesignerFlags(i);
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.UpdateDesignerFlags(APageIndex: integer);
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.UpdateDesignerFlags(APageIndex: integer);
begin
  if APageIndex<>fPageIndex then
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle+[csNoDesignVisible]
  else
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle-[csNoDesignVisible];
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
procedure TCustomNotebook.ShowControl(APage: TControl);
var
  i: LongInt;
begin
  //inherited ShowControl(AControl);
  { Find a child control that matches the one passed in and display
    the page that contains that control. This method is necessary
    for compatibility with Delphi }
  for i := 0 to fPageList.Count - 1 do begin
    if TControl(fPageList[i]) = APage then begin
      PageIndex := i;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  method TCustomNotebook UpdateTabProperties
  Params: none
  Result: none
  
  Tells the interface to update all tabs.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.UpdateTabProperties;
var i: integer;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  for i := 0 to PageCount - 1 do
    CNSendMessage(LM_NB_UpdateTab, Page[i], nil);
end;

{------------------------------------------------------------------------------
  TCustomNotebook Change
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.Change;
Begin
  if Assigned(fOnPageChanged) then fOnPageChanged(self);
end;

procedure TCustomNotebook.Loaded;
begin
  inherited Loaded;
  if HandleAllocated then DoCreateWnd;
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
          if (not (csLoading in ComponentState))
          and (not fAddingPages) then begin
            FPageIndex := NMHDR^.idfrom;
            if FPageIndex>=PageCount then
              FPageIndex:=-1;
            UpdateAllDesignerFlags;
            Change;
            if csDesigning in ComponentState then
              OwnerFormDesignerModified(Self);
          end;
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
  procedure TCustomNotebook.DoSendPageIndex;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendPageIndex;
var
  Msg: TLMNotebookEvent;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  Msg.Parent := Self;
  Msg.fCompStyle := fCompStyle;
  Msg.Page := fPageIndex;
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('[TCustomNotebook.DoSendPageIndex] A ',Name,' PageIndex=',fPageIndex);
  {$ENDIF}
  CNSendMessage(LM_SETITEMINDEX, Self, @Msg);
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('[TCustomNotebook.DoSendPageIndex] B');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendShowTabs;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendShowTabs;
var
  Msg: TLMNotebookEvent;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  Msg.Parent := Self;
  Msg.fCompStyle := fCompStyle;
  Msg.ShowTabs := fShowTabs;
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('[TCustomNotebook.DoSendShowTabs] A ',Name);
  {$ENDIF}
  CNSendMessage(LM_SHOWTABS, Self, @Msg);
  {$IFDEF NOTEBOOK_DEBUG}
  writeln('[TCustomNotebook.DoSendShowTabs] B ',Name);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendTabPosition;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendTabPosition;
var
  Msg: TLMNotebookEvent;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  Msg.Parent := Self;
  Msg.fCompStyle := fCompStyle;
  Msg.TabPosition := @fTabPosition;
  CNSendMessage(LM_SetTabPosition, Self, @Msg);
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
  Revision 1.29  2002/12/16 09:17:20  mattias
  notebook pageindex can be set to -1, if interface supports it

  Revision 1.28  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.27  2002/11/18 17:06:29  mattias
  improved designer rubberband

  Revision 1.26  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.25  2002/11/09 15:02:07  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.24  2002/10/26 15:56:45  lazarus
  MG: fixed changing notebook pageindex at designtime

  Revision 1.23  2002/10/24 08:56:30  lazarus
  MG: fixed TnoteBook AddPage and double creation of MeinMenu

  Revision 1.22  2002/09/05 13:21:12  lazarus
  MG: fixed removing notebook

  Revision 1.21  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.20  2002/09/02 20:05:44  lazarus
  MG: fixed GetActivePage

  Revision 1.19  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.18  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.17  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.16  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.15  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

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

