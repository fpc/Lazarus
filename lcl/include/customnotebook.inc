{%MainUnit ../extctrls.pp}

{******************************************************************************
                                   TNBPages
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{off $DEFINE NOTEBOOK_DEBUG}

{------------------------------------------------------------------------------
  TNBPages Constructor
 ------------------------------------------------------------------------------}
constructor TNBPages.Create(thePageList: TListWithEvent;
  theNotebook: TCustomNotebook);
begin
  inherited Create;
  fPageList := thePageList;
  fPageList.OnChange:=@PageListChange;
  fNotebook := theNotebook;
end;

{------------------------------------------------------------------------------
  procedure TNBPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
 ------------------------------------------------------------------------------}
procedure TNBPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
var
  APage: TCustomPage;
begin
  if (AnAction=lnAdded) then begin
    APage:=TObject(Ptr) as TCustomPage;
    if not (pfInserting in APage.FFlags) then
      APage.Parent:=fNotebook;
  end;
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
  //debugln(['TNBPages.Put ',DbgSName(FNotebook),' ',Index,' S="',S,'"']);
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
  DebugLn(['TNBPages.Delete B ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex]);
  {$ENDIF}
  if (Index >= 0) and
     (Index < fPageList.Count) then
  begin
    APage := TCustomPage(fPageList[Index]);
    // delete handle
    APage.Parent := nil;
    // free the page
    Application.ReleaseComponent(APage);
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TNBPages.Delete END ',fNoteBook.Name,' Index=',Index,' fPageList.Count=',fPageList.Count,' fNoteBook.PageIndex=',fNoteBook.PageIndex]);
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
  DebugLn(['TNBPages.Insert A ',FNoteBook.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  NewOwner := FNotebook.Owner;
  if NewOwner = nil then
    NewOwner := FNotebook;
  NewPage := FNotebook.PageClass.Create(NewOwner);
  with NewPage do
    Caption := S;

  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TNBPages.Insert B ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
  {$ENDIF}
  FNoteBook.InsertPage(NewPage,Index);
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TNBPages.Insert END ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
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

   // move Page in notebook handle
  FNotebook.WSMovePage(APage, NewIndex);

  // move Page in fPageList
  fPageList.Move(CurIndex, NewIndex);

  // move in wincontrol list
  fNoteBook.SetControlIndex(APage,NewControlIndex);
  
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

  FPageList := TListWithEvent.Create;
  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @DoImageListChange;

  fAccess := TNBPages.Create(TListWithEvent(fPageList), Self);
  FPageIndex := -1;
  FLoadedPageIndex:=-1;
  FPageIndexOnLastShow:=-1;

  ControlStyle := []; // do not add csAcceptsControls
  TabPosition := tpTop;
  TabStop := true;
  ShowTabs := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
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
  DebugLn(['TCustomNotebook.CreateWnd ',dbgsName(Self),' HandleAllocated=',HandleAllocated]);
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
  DebugLn(['TCustomNotebook.DoCreateWnd ',dbgsName(Self),' HandleAllocated=',HandleAllocated]);
  {$ENDIF}
  DisableAlign;
  try
    FAddingPages := True;
    for i := 0 to FPageList.Count -1 do
    begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomNotebook.DoCreateWnd ',dbgsName(Self),' Page.Caption=',Page[i].Caption,' pfAdded=',pfAdded in Page[i].Flags]);
      {$ENDIF}
      lPage := Page[i];
      AddRemovePageHandle(lPage);
    end;
    FAddingPages := False;

    DoSendShowTabs;
    DoSendPageIndex;
    ReAlign;
  finally
    EnableAlign;
  end;
end;

procedure TCustomNotebook.DoChange;
begin
  if Assigned(OnPageChanged) then
    OnPageChanged(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomNotebook.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomNotebook.Destroy;
begin
  FImageListChangeLink.Free;
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
    Result:=TWSCustomNotebookClass(WidgetSetClass).GetTabIndexAtPos(Self, ClientPos)
  else
    Result:=-1;
end;

function TCustomNotebook.TabRect(AIndex: Integer): TRect;
begin
  if HandleAllocated then
    Result := TWSCustomNotebookClass(WidgetSetClass).GetTabRect(Self, AIndex)
  else
    Result := Rect(-1, -1, -1, -1);
end;

function TCustomNotebook.GetImageIndex(ThePageIndex: Integer): Integer;
var
  APage: TCustomPage;
begin
  APage := Page[ThePageIndex];
  if APage <> nil then
    Result := APage.ImageIndex
  else
    Result := -1;
  if Assigned(OnGetImageIndex) then
    OnGetImageIndex(Self, ThePageIndex, Result);
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
  Result := True;
  if ([csDesigning, csDestroying] * ComponentState = []) and Assigned(OnChanging) then
    OnChanging(Self, Result);
end;

function TCustomNotebook.GetMinimumTabWidth: integer;
begin
  Result := TWSCustomNotebookClass(WidgetSetClass).GetNotebookMinTabWidth(Self);
  //debugln('TCustomNotebook.GetMinimumTabWidth A ',dbgs(Result));
end;

function TCustomNotebook.GetMinimumTabHeight: integer;
begin
  Result := TWSCustomNotebookClass(WidgetSetClass).GetNotebookMinTabHeight(Self);
  //debugln('TCustomNotebook.GetMinimumTabHeight A ',dbgs(Result));
end;

function TCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=TWSCustomNotebookClass(WidgetSetClass).GetCapabilities;
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
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
    Result := TCustomPage(FPageList[FPageIndex]).Caption
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  function TCustomNotebook.GetActivePageComponent: TCustomPage;
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetActivePageComponent: TCustomPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
    Result := Page[FPageIndex]
  else
    Result := nil;
end;

function TCustomNotebook.GetMultiLine: Boolean;
begin
  Result := nboMultiLine in Options;
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetActivePage
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetActivePage(const Value: String);
var
  i: Integer;
begin
  for i := 0 to FPageList.Count - 1 do
  begin
    if TCustomPage(FPageList[i]).Caption = Value then
    begin
      SetPageIndex(i);
      Exit;
    end;
  end;
end;

procedure TCustomNotebook.SetActivePageComponent(const AValue: TCustomPage);
begin
  PageIndex := FPageList.IndexOf(AValue);
end;

procedure TCustomNotebook.SetImages(const AValue: TImageList);
begin
  if FImages = AValue then Exit;
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FImageListChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := AValue;
  if FImages <> nil then
  begin
    FImages.FreeNotification(Self);
    FImages.RegisterChanges(FImageListChangeLink);
  end;
  DoImageListChange(Self);
  UpdateTabProperties;
end;

procedure TCustomNotebook.SetOptions(const AValue: TNoteBookOptions);
var
  ChangedOptions: TNoteBookOptions;
begin
  if FOptions = AValue then Exit;
  ChangedOptions := (FOptions - AValue) + (AValue - FOptions);
  FOptions := AValue;
  if nboShowCloseButtons in ChangedOptions then
    UpdateTabProperties;
  if HandleAllocated then
    TWSCustomNotebookClass(WidgetSetClass).UpdateProperties(Self);
end;

{------------------------------------------------------------------------------
  TCustomNotebook SetPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.SetPageIndex(AValue: Integer);
begin
  if (csLoading in ComponentState) then FLoadedPageIndex := AValue;
  //debugln('TCustomNotebook.SetPageIndex A ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated),' ',dbgs(ComponentState));
  if (AValue < -1) or (AValue >= PageCount) then Exit;
  if FPageIndex = AValue then exit;
  if not CanChangePageIndex then exit;
  //debugln('TCustomNotebook.SetPageIndex B ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated));

  FPageIndex := AValue;
  UpdateAllDesignerFlags;
  DoSendPageIndex;
  if ([csDesigning, csLoading, csDestroying] * ComponentState = []) then
    DoChange;
end;

{------------------------------------------------------------------------------
  TCustomNotebook GetPageIndex
 ------------------------------------------------------------------------------}
function TCustomNotebook.GetPageIndex: Integer;
begin
  Result := FPageIndex;
end;

procedure TCustomNotebook.InsertPage(APage: TCustomPage; Index: Integer);
var
  NewZPosition: integer;
begin
  if FPageList.IndexOf(APage) >= 0 then Exit;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomNotebook.InsertPage A ',dbgsName(Self),' Index=',Index,' Name=',
    APage.Name,' Caption=',APage.Caption]);
  {$ENDIF}
  APage.DisableAlign;
  try
    if Index < FPageList.Count then
      NewZPosition := GetControlIndex(TCustomPage(fPageList[Index]))
    else
      NewZPosition := -1;
    Include(APage.FFlags, pfInserting);
    FPageList.Insert(Index, APage);
    Exclude(APage.FFlags, pfInserting);
    APage.Parent := Self;
    if NewZPosition >= 0 then
      SetControlIndex(APage, NewZPosition);
    if PageIndex = -1 then
      FPageIndex := Index;

    UpdateDesignerFlags(Index);

    if HandleAllocated and (not (csLoading in ComponentState)) then
    begin
      AddRemovePageHandle(APage);
      if PageIndex = Index then
        DoSendPageIndex;
    end;
  finally
    APage.EnableAlign;
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomNotebook.InsertPage END ',dbgsName(Self),' Index=',
    Index,' Name=',APage.Name,' Caption=',APage.Caption]);
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
  Change;
end;

procedure TCustomNotebook.SetMultiLine(const AValue: Boolean);
begin
  if AValue then
    Options := Options + [nboMultiLine]
  else
    Options := Options - [nboMultiLine];
end;

{------------------------------------------------------------------------------
  function TCustomNotebook.FindVisiblePage(Index: Integer): Integer;

  It tries to find the next (at right) visible page. If no one is found,
  it tries to to find the previous (at left) visible page.
  Returns -1 if there's no visible pages.
 ------------------------------------------------------------------------------}

function TCustomNotebook.FindVisiblePage(Index: Integer): Integer;
begin
  for Result := Index to FPageList.Count - 1 do
    if TCustomPage(FPageList[Result]).TabVisible then
      exit;
  // if arrived here no visible forward page was found, search backwards
  for Result := Index - 1 downto 0 do
    if TCustomPage(FPageList[Result]).TabVisible then
      exit;
  Result := -1;
end;

procedure TCustomNotebook.PageRemoved(Index: Integer);
var
  NewPageIndex: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    // if this page is showing, then show the next page before deleting it
    if Index = FPageIndex then 
    begin
      NewPageIndex := FindVisiblePage(Index);
      if NewPageIndex >= 0 then
        PageIndex := NewPageIndex
      else
        FPageIndex := NewPageIndex;
    end;
  end;
end;

procedure TCustomNotebook.WSMovePage(APage: TCustomPage; NewIndex: Integer);
var
  RealIndex: Integer;
  i: Integer;
begin
  //DebugLn(['TCustomNotebook.WSMovePage APage=',DbgSName(APage),' NewIndex=',NewIndex,' pfAdded=',pfAdded in APage.FFlags]);
  if HandleAllocated and (pfAdded in APage.FFlags) then begin
    RealIndex:=0;
    i:=0;
    repeat
      if (i=NewIndex) or (i=PageCount) then break;
      if pfAdded in Page[i].FFlags then inc(RealIndex);
      inc(i);
    until false;
    //DebugLn(['TCustomNotebook.WSMovePage APage=',DbgSName(APage),' NewIndex=',NewIndex,' RealIndex=',RealIndex]);
    TWSCustomNotebookClass(WidgetSetClass).MovePage(Self, APage, RealIndex);
  end;
end;

procedure TCustomNoteBook.AddRemovePageHandle(APage: TCustomPage);
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomNoteBook.AddRemovePageHandle'){$ENDIF};
  try
    if (not (csDestroying in APage.ComponentState))
    and (APage.TabVisible or (csDesigning in ComponentState)) then begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomNoteBook.AddRemovePageHandle ADD ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
      {$ENDIF}
      if (pfAdded in APage.FFlags) then exit;
      Include(APage.FFlags,pfAdding);
      TWSCustomNotebookClass(WidgetSetClass).AddPage(Self, APage, APage.VisibleIndex);
      APage.FFlags:=APage.FFlags+[pfAdded]-[pfAdding];
      APage.AdjustSize;
    end else begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomNoteBook.AddRemovePageHandle REMOVE ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
      {$ENDIF}
      if not (pfAdded in APage.FFlags) or (pfRemoving in APage.FFlags) then
        exit;
      APage.FFlags := APage.FFlags - [pfAdded] + [pfRemoving];
      TWSCustomNotebookClass(WidgetSetClass).RemovePage(Self, APage.VisibleIndex);
      if APage.HandleAllocated then
        APage.DestroyHandle;
      Exclude(APage.FFlags, pfRemoving);
    end;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomNoteBook.AddRemovePageHandle'){$ENDIF};
  end;
end;

procedure TCustomNotebook.RemovePage(Index: Integer);
var
  APage: TCustomPage;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomNotebook.RemovePage A ',dbgsName(Self),' Index=',Index,
    ' FPageList.Count=',FPageList.Count,' PageIndex=',PageIndex]);
  {$ENDIF}
  if (Index >= 0) and (Index < FPageList.Count) then
  begin
    APage:=TCustomPage(fPageList[Index]);
    APage.FTabVisible:=false;
    if HandleAllocated then
      AddRemovePageHandle(APage);
    PageRemoved(Index);
    FPageList.Delete(Index);
    APage.Parent:=nil;
    if FPageIndex >= Index then
      Dec(FPageIndex);
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomNotebook.RemovePage END ',dbgsName(Self),' Index=',Index,' fPageList.Count=',fPageList.Count,' PageIndex=',PageIndex]);
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
function TCustomNotebook.GetPage(AIndex: Integer): TCustomPage;
begin
  if (AIndex < 0) or (AIndex >= FPageList.Count) then
    RaiseGDBException('TCustomNotebook.GetCustomPage Index out of bounds');
  Result := TCustomPage(FPageList.Items[AIndex]);
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

class procedure TCustomNotebook.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomNotebook;
end;

{------------------------------------------------------------------------------
  TCustomNotebook ReadState
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.ReadState(Reader: TReader);
begin
  // do not clear. Think about loading ancestor + loading descendant stream.
  // fAccess.Clear;
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
var
  i: integer;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  for i := 0 to PageCount - 1 do
    TWSCustomPageClass(Page[i].WidgetSetClass).UpdateProperties(Page[i]);
end;

function TCustomNotebook.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and (ChildClass.InheritsFrom(PageClass));
end;

class function TCustomNotebook.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 200;
end;

procedure TCustomNotebook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    SetImages(nil);
end;

{------------------------------------------------------------------------------
  TCustomNotebook Change
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.Change;
begin
  //DebugLn(['TCustomNotebook.Change ',DbgSName(Self),' fPageIndex=',fPageIndex]);
  ShowCurrentPage;
  FPageIndexOnLastChange := FPageIndex;
  if ([csLoading,csDestroying]*ComponentState=[]) and (not FAddingPages) then
    DoChange;
end;

procedure TCustomNotebook.Loaded;
begin
  inherited Loaded;
  if FLoadedPageIndex >= -1 then
    PageIndex := FLoadedPageIndex;
  FLoadedPageIndex := -1;
  //DebugLn(['TCustomNotebook.Loaded ',DbgSName(Self),' fPageIndex=',fPageIndex]);
  FPageIndexOnLastChange := PageIndex;
  FPageIndexOnLastShow := FPageIndexOnLastChange;
  if HandleAllocated then DoCreateWnd;
end;

function TCustomNotebook.DialogChar(var Message: TLMKey): boolean;
var
  destPage: TCustomPage;
begin
  // broadcast only to active page
  Result := false;
  destPage := GetActivePageComponent;
  if destPage <> nil then
    Result := destPage.DialogChar(Message);
end;

{------------------------------------------------------------------------------
  TCustomNotebook CNNotify
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.CNNotify(var Message: TLMNotify);
var
  OldPageIndex: LongInt;
begin
  with Message do
    case NMHdr^.code of
      TCN_SELCHANGE:
        begin
          // set the page from the NMHDR^.idfrom
          if (not FAddingPages) and not
             (csDestroyingHandle in ControlState) then
          begin
            OldPageIndex := FPageIndex;
            FPageIndex := PtrInt(NMHDR^.idfrom);
            if FPageIndex >= PageCount then
              FPageIndex := -1;
            //debugln(['TCustomNotebook.CNNotify ',DbgSName(Self),' A Old=',OldPageIndex,' fPageIndex=',fPageIndex,' FLoadedPageIndex=',FLoadedPageIndex]);
            //if PageIndex>=0 then DebugLn(['TCustomNotebook.CNNotify Page=',DbgSName(Page[PageIndex]),' Visible=',Page[PageIndex].Visible]);
            UpdateAllDesignerFlags;
            if ([csLoading,csDestroying]*ComponentState=[]) then
            begin
              if OldPageIndex <> FPageIndex then
              begin
                if csDesigning in ComponentState then
                  OwnerFormDesignerModified(Self);
                //DebugLn(['TCustomNotebook.CNNotify ',DbgSName(Page[PageIndex]),' ',Page[PageIndex].Visible]);
                Change;
              end;
            end;
          end;
        end;
      TCN_SELCHANGING:
        begin
          if CanChangePageIndex and not
          (csDestroyingHandle in ControlState) then
            Result := 0
          else
            Result := 1;
          //debugln('TCustomNotebook.CNNotify TCN_SELCHANGING Result=',dbgs(Result));
        end;
    else
      begin
        {$IFDEF NOTEBOOK_DEBUG}
        DebugLn(['[TCustomNotebook.CNNotify] unhandled NMHdr code:', NMHdr^.code]);
        {$ENDIF}
      end;
    end;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.ShowCurrentPage

  Makes sure Visible = true for page which has index FPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.ShowCurrentPage;
var
  CurPage: TCustomPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
  begin
    CurPage := Page[FPageIndex];
    // first make the new page visible
    //DebugLn(['TCustomNotebook.ShowCurrentPage ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible]);
    CurPage.UpdateControlState;
    if CurPage.Visible then
    begin
      if FPageIndexOnLastShow <> FPageIndex then
      begin
        // some widgetsets like win32/64 do not send WM_SIZE messages for
        // hidden pages. Force resizing page (it is alClient).
        //DebugLn(['TCustomNotebook.ShowCurrentPage ',dbgsName(Self),' ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible,' BoundsRect=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CurPage.BoundsRect=',dbgs(CurPage.BoundsRect),' CurPage.ClientRect=',dbgs(CurPage.ClientRect)]);
        ReAlign;
        // TCustomPage.IsControlVisible is overriden
        // therefore AutoSizing of children was skipped => do it now
        CurPage.ReAlign;
      end;
    end else
    begin
      CurPage.Visible := true;
      //DebugLn(['TCustomNotebook.ShowCurrentPage CurPage.AutoSizeDelayed=',CurPage.AutoSizeDelayed,' ',dbgs(CurPage.ComponentState),' ',CurPage.HandleAllocated]);
    end;
    FPageIndexOnLastShow := FPageIndex;
    CurPage.DoShow;
  end;
  if (FPageIndexOnLastChange >= 0) and (FPageIndexOnLastChange < PageCount) and
     (FPageIndexOnLastChange <> FPageIndex) then
  begin
    // Page[FPageIndexOnLastChange].Visible := False; <-- this will be better,
    // but this does not work on gtk (tab hides too)
    Page[FPageIndexOnLastChange].DoHide;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomNotebook.DoSendPageIndex;
 ------------------------------------------------------------------------------}
procedure TCustomNotebook.DoSendPageIndex;
begin
  //DebugLn('[TCustomNotebook.DoSendPageIndex] A ',dbgsName(Self),' PageIndex=',dbgs(fPageIndex),' ',dbgs(csLoading in ComponentState),' ',dbgs(HandleAllocated));
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('[TCustomNotebook.DoSendPageIndex] B ',dbgsName(Self),' PageIndex=',dbgs(fPageIndex));
  {$ENDIF}
  ShowCurrentPage;
  FPageIndexOnLastChange := FPageIndex;
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
  DebugLn('[TCustomNotebook.DoSendShowTabs] A ',dbgsName(Self));
  {$ENDIF}
  TWSCustomNotebookClass(WidgetSetClass).ShowTabs(Self, FShowTabs);
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('[TCustomNotebook.DoSendShowTabs] B ',dbgsName(Self));
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

procedure TCustomNotebook.DoImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    TWSCustomNotebookClass(WidgetSetClass).SetImageList(Self, Images);
end;


