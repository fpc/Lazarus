{%MainUnit ../extctrls.pp}

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
  // Create the page list and a notebook
  fPageList := thePageList;
  fNotebook := theNotebook;
end;

{------------------------------------------------------------------------------
  TNBPages Get
 ------------------------------------------------------------------------------}
function TNBPages.Get(Index: Integer): String;
begin
//DebugLn('TNBPages.Get Index=',Index);
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TNBPages.Get Index out of bounds');
  Result := TCustomPage(fPageList[Index]).Caption;
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
  Result := TCustomPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TNBPages Put
 ------------------------------------------------------------------------------}
procedure TNBPages.Put(Index: Integer; const S: String);
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TNBPages.Put Index out of bounds');
  
  TCustomPage(fPageList[Index]).Caption := S;
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
  APage: TCustomPage;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('TNBPages.Delete A Index=',Index);
  DebugLn('TNBPages.Delete B ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    APage:=TCustomPage(fPageList[Index]);
    // delete handle
    APage.Parent:=nil;
    // free the page
    APage.Free;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TNBPages.Delete END ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages Insert
 ------------------------------------------------------------------------------}
procedure TNBPages.Insert(Index: Integer; const S: String);
var
  NewPage: TCustomPage;
  NewOwner: TComponent;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TNBPages.Insert A ',FNoteBook.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
  NewOwner:=FNotebook.Owner;
  if NewOwner=nil then
    NewOwner:=FNotebook;
  NewPage := FNotebook.PageClass.Create(NewOwner);
  with NewPage do
    Caption := S;

  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TNBPages.Insert B ',FNotebook;.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
  FNoteBook.InsertPage(NewPage,Index);
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TNBPages.Insert END ',FNotebook;.Name,' Index=',Index,' S="',S,'"');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TNBPages Move
 ------------------------------------------------------------------------------}
procedure TNBPages.Move(CurIndex, NewIndex: Integer);
var
  APage: TCustomPage;
  NewControlIndex, NewPageIndex: integer;
begin
  if CurIndex=NewIndex then exit;
  NewPageIndex:=NewIndex;

  APage:=TCustomPage(fPageList[CurIndex]);

  // calculate new control index (i.e. ZOrderPosition)
  if NewIndex>=fPageList.Count-1 then
    NewControlIndex:=fNoteBook.ControlCount-1
  else
    NewControlIndex:=fNoteBook.GetControlIndex(TCustomPage(fPageList[NewIndex]));

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
  FNotebook.WSMovePage(APage, NewIndex);

  // update PageIndex
  fNoteBook.PageIndex:=NewPageIndex;
end;


{******************************************************************************
                                TCustomNotebook
 ******************************************************************************}
{------------------------------------------------------------------------------
  TCustomNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TCustomNotebook.Create(TheOwner: TComponent);
begin
  if PageClass=nil then
    RaiseGDBException('');
  inherited Create(TheOwner);

  fCompStyle := csNoteBook;

  fPageList := TList.Create;

  fAccess := TNBPages.Create(fPageList, Self);
  fPageIndex := -1;
  FLoadedPageIndex:=-1;

  ControlStyle := [csAcceptsControls];
  TabPosition := tpTop;
  TabStop := true;
  ShowTabs := True;
  SetInitialBounds(0,0,200,200);
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
  DebugLn('TCustomNotebook.CreateWnd ',Name,':',ClassName,' HandleAllocated=',HandleAllocated);
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
  lPage: TCustomPage;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TCustomNotebook.DoCreateWnd ',Name,':',ClassName,' HandleAllocated=',HandleAllocated);
  {$ENDIF}
  fAddingPages:=true;
  for i := 0 to FPageList.Count -1 do begin
    {$IFDEF NOTEBOOK_DEBUG}
    DebugLn('TCustomNotebook.DoCreateWnd ',Name,':',ClassName,' ',Page[i].Caption,' ',not (pfAdded in Page[i].Flags));
    {$ENDIF}
    lPage := Page[i];
    if not (pfAdded in lPage.Flags) then begin
      TWSCustomNotebookClass(WidgetSetClass).AddPage(Self, lPage, i);
      Include(lPage.FFlags,pfAdded);
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
  FreeAndNil(FAccess);
  FreeAndNil(FPageList);
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
    Result:=LCLIntf.GetNotebookTabIndexAtPos(Handle,ClientPos)
  else
    Result:=-1;
end;

function TCustomNotebook.CanTab: boolean;
begin
  Result:=false;
end;

function TCustomNotebook.GetImageIndex(ThePageIndex: Integer): Integer;
var
  APage: TCustomPage;
begin
  APage:=Page[ThePageIndex];
  if APage<>nil then
    Result:=APage.ImageIndex
  else
    Result:=-1;
  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self,ThePageIndex,Result);
end;

function TCustomNotebook.IndexOf(APage: TCustomPage): integer;
begin
  Result:=FPageList.IndexOf(APage);
end;

function TCustomNotebook.CustomPage(Index: integer): TCustomPage;
begin
  Result:=GetPage(Index);
end;

function TCustomNotebook.CanChangePageIndex: boolean;
begin
  Result:=true;
  if Assigned(OnChanging) then OnChanging(Self,Result);
end;

function TCustomNotebook.GetMinimumTabWidth: integer;
begin
  Result:=TWSCustomNotebookClass(WidgetSetClass).GetNotebookMinTabWidth(Self);
  //debugln('TCustomNotebook.GetMinimumTabWidth A ',dbgs(Result));
end;

function TCustomNotebook.GetMinimumTabHeight: integer;
begin
  Result:=TWSCustomNotebookClass(WidgetSetClass).GetNotebookMinTabHeight(Self);
  //debugln('TCustomNotebook.GetMinimumTabHeight A ',dbgs(Result));
end;

{------------------------------------------------------------------------------
  method TCustomNotebook DoCloseTabClicked
  Params: APage: TCustomPage
  Result: none

  Called whenever the user closes the tab.
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoCloseTabClicked(APage: TCustomPage);
begin
  if Assigned(OnCloseTabClicked) then OnCloseTabClicked(APage);
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetActivePage
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetActivePage: String;
begin
  if (PageIndex>=0) and (PageIndex<PageCount) then
    Result := TCustomPage(fPageList[PageIndex]).Caption
  else
    Result:='';
end;

{------------------------------------------------------------------------------
  function TCustomNotebook.GetActivePageComponent: TCustomPage;
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetActivePageComponent: TCustomPage;
var
  i: Integer;
begin
  i:=PageIndex;
  if (i>=0) and (i<PageCount) then
    Result:=Page[i]
  else
    Result:=nil;
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
    if TCustomPage(fPageList[i]).Caption = Value then
    begin
      SetPageIndex(i);
      Break;
    end;
  end;
end;

procedure TCustomNotebook.SetActivePageComponent(const AValue: TCustomPage);
begin
  PageIndex:=fPageList.IndexOf(AValue);
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
procedure TCustomNotebook.SetPageIndex(AValue: Integer);
begin
  if (csLoading in ComponentState) then FLoadedPageIndex:=AValue;
  //debugln('TCustomNotebook.SetPageIndex A ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated));
  if (AValue < 0) or (AValue >= PageCount) then exit;
  if fPageIndex = AValue then exit;
  if not CanChangePageIndex then exit;
  //debugln('TCustomNotebook.SetPageIndex B ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated));
  fPageIndex := AValue;
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

procedure TCustomNotebook.InsertPage(APage: TCustomPage; Index: Integer);
var
  NewZPosition: integer;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TCustomNotebook.InsertPage A ',Name,' Index=',Index,' Name=',
    APage.Name,' Caption=',APage.Caption);
  {$ENDIF}
  if Index<FPageList.Count then
    NewZPosition:=GetControlIndex(TCustomPage(fPageList[Index]))
  else
    NewZPosition:=-1;
  FPageList.Insert(Index,APage);
  APage.Parent := Self;
  if NewZPosition>=0 then
    SetControlIndex(APage,NewZPosition);
  if PageIndex = -1 then
    FPageIndex := Index;

{$ifndef WIN32}
  // TODO: remove when gtk widgetset fixed to show tabpage tab upon
  //   AddPage, instead of needing TabPage.Visible := true
  APage.Visible := true;
{$endif}

  UpdateDesignerFlags(Index);

  if HandleAllocated and (not (csLoading in ComponentState)) then begin
    TWSCustomNotebookClass(WidgetSetClass).AddPage(Self, APage, Index);
    Include(APage.FFlags, pfAdded);
    if PageIndex = Index then
      DoSendPageIndex;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TCustomNotebook.InsertPage END ',Name,' Index=',
    Index,' Name=',APage.Name,' Caption=',APage.Caption);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TCustomNotebook MoveTab
 ------------------------------------------------------------------------------}
procedure TCustomNoteBook.MoveTab(Sender: TObject; NewIndex: Integer);
begin
  if (Sender <> nil) and (NewIndex < PageCount) then begin
    TNBPages(fAccess).Move(TCustomPage(Sender).PageIndex,NewIndex);
  end
  else ; //raise exception?
end;

procedure TCustomNotebook.WSMovePage(APage: TCustomPage; NewIndex: Integer);
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    TWSCustomNotebookClass(WidgetSetClass).MovePage(Self, APage, NewIndex);
end;

procedure TCustomNotebook.RemovePage(Index: Integer);
var
  NewPageIndex: integer;
  APage: TCustomPage;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TCustomNotebook.RemovePage A ',Name,' Index=',Index,
    ' FPageList.Count=',FPageList.Count,' PageIndex=',PageIndex);
  {$ENDIF}
  if (Index >= 0) and
     (Index < FPageList.Count) then
  begin
    APage:=TCustomPage(fPageList[Index]);
    NewPageIndex:=PageIndex;
    if not (csLoading in ComponentState) then begin
      // If that page is showing, then show the next page before deleting it
      if (Index = PageIndex) then begin
        if NewPageIndex<FPageList.Count-1 then
          // switch current page to next (right) page
          inc(NewPageIndex)
        else if FPageList.Count>0 then
          // switch to previous (left) page
          dec(NewPageIndex)
        else
          // deleting last page
          NewPageIndex:=-1;

        if NewPageIndex>=0 then
          PageIndex:=NewPageIndex;
      end;
    end;
    if HandleAllocated and APage.HandleAllocated then begin
      TWSCustomNotebookClass(WidgetSetClass).RemovePage(Self, Index);
    end;
    FPageList.Delete(Index);
    APage.Parent:=nil;
    if fPageIndex >= Index then
      Dec(fPageIndex);
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('TCustomNotebook.RemovePage END ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex);
  {$ENDIF}
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
procedure TCustomNotebook.SetPages(AValue: TStrings);
begin
  FAccess.Assign(AValue);
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPage
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPage(aIndex: Integer): TCustomPage;
begin
  if (aIndex<0) or (aIndex>=fPageList.Count) then
    RaiseGDBException('TCustomNotebook.GeTCustomPage Index out of bounds');
  Result := TCustomPage(fPageList.Items[aIndex]);
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
    TWSCustomPageClass(Page[i].WidgetSetClass).UpdateProperties(Page[i]);
end;

function TCustomNotebook.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and (ChildClass.InheritsFrom(PageClass));
end;

{------------------------------------------------------------------------------
  TCustomNotebook Change
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.Change;
Begin
  fPageIndexOnLastChange:=fPageIndex;
  ShowCurrentPage;
  if ([csLoading,csDestroying]*ComponentState=[])
  and (not fAddingPages) then begin
    if Assigned(fOnPageChanged) then fOnPageChanged(Self);
  end;
end;

procedure TCustomNotebook.Loaded;
begin
  inherited Loaded;
  if FLoadedPageIndex>=0 then PageIndex:=FLoadedPageIndex;
  FLoadedPageIndex:=-1;
  fPageIndexOnLastChange:=PageIndex;
  if HandleAllocated then DoCreateWnd;
end;

{------------------------------------------------------------------------------
  TCustomNotebook CNNotify
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CNNotify(var Message: TLMNotify);
Begin
  with Message do
    Case NMHdr^.code of
      TCN_SELCHANGE:
        begin
          // set the page from the NMHDR^.idfrom
          if (not fAddingPages) then begin
            FPageIndex := NMHDR^.idfrom;
            if FPageIndex>=PageCount then
              FPageIndex:=-1;
            //debugln('TCustomNotebook.CNNotify A fPageIndex=',fPageIndex,' FLoadedPageIndex=',FLoadedPageIndex);
            UpdateAllDesignerFlags;
            if ([csLoading,csDestroying]*ComponentState=[]) then begin
              if fPageIndexOnLastChange<>fPageIndex then
                Change;
              if csDesigning in ComponentState then
                OwnerFormDesignerModified(Self);
            end;
          end;
        end;
      TCN_SELCHANGING:
        begin
          if CanChangePageIndex then
            Result := 0
          else
            Result := 1;
          //debugln('TCustomNotebook.CNNotify TCN_SELCHANGING Result=',dbgs(Result));
        end;
    else
      begin
        {$IFDEF NOTEBOOK_DEBUG}
        DebugLn('[TCustomNotebook.CNNotify] unhandled NMHdr code:', NMHdr^.code);
        {$ENDIF}
      end;
    end;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendPageIndex

  Makes sure Visible = true for page which has index FPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.ShowCurrentPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
    Page[FPageIndex].Visible := true;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendPageIndex;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendPageIndex;
begin
  //DebugLn('[TCustomNotebook.DoSendPageIndex] A ',Name,' PageIndex=',dbgs(fPageIndex),' ',dbgs(csLoading in ComponentState),' ',dbgs(HandleAllocated));
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('[TCustomNotebook.DoSendPageIndex] B ',Name,' PageIndex=',dbgs(fPageIndex));
  {$ENDIF}
  ShowCurrentPage;
  TWSCustomNotebookClass(WidgetSetClass).SetPageIndex(Self, FPageIndex);
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('[TCustomNotebook.DoSendPageIndex] END ',dbgs(FPageIndex));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendShowTabs;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendShowTabs;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('[TCustomNotebook.DoSendShowTabs] A ',Name);
  {$ENDIF}
  TWSCustomNotebookClass(WidgetSetClass).ShowTabs(Self, FShowTabs);
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('[TCustomNotebook.DoSendShowTabs] B ',Name);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendTabPosition;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendTabPosition;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  TWSCustomNotebookClass(WidgetSetClass).SetTabPosition(Self, FTabPosition);
end;


{ =============================================================================

  $Log$
  Revision 1.68  2005/01/03 11:13:41  micha
  fix repeatingly setting pageindex 0,1,0,1,.. when removing more pages ... note: needs implementation of visible list

  Revision 1.67  2005/01/01 11:59:58  mattias
  fixed collecting values

  Revision 1.66  2004/10/23 14:49:38  micha
  optimize: only create tabpage contents when accessed, not immediately upon creation

  Revision 1.65  2004/10/15 12:04:09  mattias
  calling updating notebook tab after realize, needed for close btns

  Revision 1.64  2004/09/24 20:23:33  vincents
  fixed fpc 1.0.x compilation

  Revision 1.63  2004/09/24 19:02:38  micha
  convert LM_MOVEPAGE message to interface method

  Revision 1.62  2004/09/24 18:00:51  micha
  convert LM_NB_UPDATETAB message to interface method

  Revision 1.61  2004/09/23 16:37:13  vincents
  fix compilation

  Revision 1.60  2004/09/23 14:54:55  vincents
  moved widgetset related code from TNBPages to TCustomNotebook

  Revision 1.59  2004/09/15 17:21:22  micha
  convert LM_GETITEMINDEX and LM_SETITEMINDEX messages to interface methods

  Revision 1.58  2004/09/14 12:45:29  micha
  convert LM_SETTABPOSITION message to interface method

  Revision 1.57  2004/09/13 19:57:30  micha
  convert LM_SHOWTABS message to interface method

  Revision 1.56  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.55  2004/09/10 16:28:50  mattias
  implemented very rudimentary TTabControl

  Revision 1.54  2004/09/10 09:43:12  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.53  2004/07/17 15:08:36  mattias
  fixed tab for TPanel and TPage

  Revision 1.52  2004/06/29 08:03:08  micha
  fix showtabs for win32 interface

  Revision 1.51  2004/06/24 17:45:33  mattias
  fixed TMenuItem.GetIconSize

  Revision 1.50  2004/06/20 20:25:47  micha
  fix tabbing to next control to skip invisible notebook pages

  Revision 1.49  2004/06/01 09:58:35  mattias
  implemented setting TCustomPage.PageIndex  from Andrew Haines

  Revision 1.48  2004/05/25 21:50:32  mattias
  TCustomNotebook now allows pageclass descendents

  Revision 1.47  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.46  2004/04/11 11:30:39  vincents
  Reduced output, fixes bug 220

  Revision 1.45  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.44  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.42  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.41  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.40  2003/12/23 18:15:37  mattias
  fixed TNoteBook.OnPageChanged for user PageIndex

  Revision 1.39  2003/12/23 14:52:11  mattias
  TNoteBook.PageChanged not during loading

  Revision 1.38  2003/11/25 21:48:29  micha
  clearer notebook error message

  Revision 1.37  2003/11/10 19:05:44  mattias
  fixed some bugs by Andreas

  Revision 1.36  2003/09/20 15:24:54  mattias
  implemented TPageControl and TTabSheet

  Revision 1.35  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.34  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.33  2003/09/17 15:26:41  mattias
  fixed removing TCustomPage

  Revision 1.32  2003/08/12 14:02:54  mattias
  fixed keypress/keyup, createcaret on synedit focus

  Revision 1.31  2003/06/10 17:23:35  mattias
  implemented tabstop

  Revision 1.30  2003/04/22 13:27:10  mattias
  implemented installing components in component palette

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
  MG: TNoteBook now starts with no Page and TCustomPage has no auto names

  Revision 1.18  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.17  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.16  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.15  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TCustomPage

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
       (affects TCustomPage & TCustomNotebook)
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
       * changed fixed widged of TCustomPage. Code is still broken.
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

