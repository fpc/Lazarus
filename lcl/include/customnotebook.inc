{%MainUnit ../comctrls.pp}

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
  theNotebook: TCustomTabControl);
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
                                TCustomTabControl
 ******************************************************************************}
{------------------------------------------------------------------------------
  TCustomTabControl Constructor
 ------------------------------------------------------------------------------}
constructor TCustomTabControl.Create(TheOwner: TComponent);
begin
  if PageClass=nil then PageClass := TCustomPage;

  inherited Create(TheOwner);

  fCompStyle := csNoteBook;

  if not FUnPaged then begin
    FPageList := TListWithEvent.Create;
    fAccess := TNBPages.Create(TListWithEvent(fPageList), Self);
  end;

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @DoImageListChange;

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
  Method:  TCustomTabControl.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.CreateWnd;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.CreateWnd ',dbgsName(Self),' HandleAllocated=',HandleAllocated]);
  {$ENDIF}
  inherited CreateWnd;
  DoCreateWnd;
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.DoCreateWnd;

  Creates the handles for the pages and updates the notebook handle.
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.DoCreateWnd;
var
  i: Integer;
  lPage: TCustomPage;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.DoCreateWnd ',dbgsName(Self),' HandleAllocated=',HandleAllocated]);
  {$ENDIF}
  DisableAlign;
  try
    FAddingPages := True;
    for i := 0 to PageCount -1 do
    begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomTabControl.DoCreateWnd ',dbgsName(Self),' Page.Caption=',Page[i].Caption,' pfAdded=',pfAdded in Page[i].Flags]);
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

procedure TCustomTabControl.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomTabControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomTabControl.Destroy;
begin
  FImageListChangeLink.Free;
  Pages.Clear;
  FreeAndNil(FAccess);
  FreeAndNil(fPageList);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.TabIndexAtClientPos(ClientPos: TPoint): integer;

  Returns the index of the page of the tab at the client position.
  For example:
    Index:=NoteBook1.PageIndexAtClientPos(
               NoteBook1.ScreenToClient(Mouse.CursorPos));
 ------------------------------------------------------------------------------}
function TCustomTabControl.TabIndexAtClientPos(ClientPos: TPoint): integer;
var
  i, VisiblePageInd: Integer;
begin
  if HandleAllocated then begin
    Result:=TWSCustomTabControlClass(WidgetSetClass).GetTabIndexAtPos(Self, ClientPos);
    VisiblePageInd:=-1;
    for i:=0 to PageCount-1 do begin
      if Page[i].TabVisible then
        Inc(VisiblePageInd)
      else begin
        if VisiblePageInd < Result then
          Inc(Result)
        else
          Break;
      end;
    end;
  end
  else
    Result:=-1;
end;

function TCustomTabControl.TabRect(AIndex: Integer): TRect;
begin
  if HandleAllocated then
    Result := TWSCustomTabControlClass(WidgetSetClass).GetTabRect(Self, AIndex)
  else
    Result := Rect(-1, -1, -1, -1);
end;

function TCustomTabControl.GetImageIndex(ThePageIndex: Integer): Integer;
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

function TCustomTabControl.IndexOf(APage: TPersistent): integer;
begin
  if FUnPaged then
    Result := -1
  else
    Result:=FPageList.IndexOf(APage);
end;

function TCustomTabControl.CustomPage(Index: integer): TCustomPage;
begin
  Result:=GetPage(Index);
end;

function TCustomTabControl.CanChangePageIndex: boolean;
begin
  Result := CanChange;
end;

function TCustomTabControl.CanChange: Boolean;
begin
  Result := True;
  if ([csDesigning, csDestroying] * ComponentState = []) and Assigned(OnChanging) then
    OnChanging(Self, Result);
end;

function TCustomTabControl.GetMinimumTabWidth: integer;
begin
  Result := TWSCustomTabControlClass(WidgetSetClass).GetNotebookMinTabWidth(Self);
  //debugln('TCustomTabControl.GetMinimumTabWidth A ',dbgs(Result));
end;

function TCustomTabControl.GetMinimumTabHeight: integer;
begin
  Result := TWSCustomTabControlClass(WidgetSetClass).GetNotebookMinTabHeight(Self);
  //debugln('TCustomTabControl.GetMinimumTabHeight A ',dbgs(Result));
end;

function TCustomTabControl.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=TWSCustomTabControlClass(WidgetSetClass).GetCapabilities;
end;

function TCustomTabControl.PageToTabIndex(AIndex: integer): integer;
var
  i: integer;
begin
(* Map LCL Page into widgetset Tab index.
  Taken from TWin32WSCustomNotebook.GetPageRealIndex (modified)
*)
  if (AIndex < 0) or (AIndex >= PageCount) then
    exit(-1);

  Result := AIndex;
  if csDesigning in ComponentState then
    exit; //all pages are visible
  for i := 0 to AIndex - 1 do begin
    if not Page[i].TabVisible then
      dec(Result); //exclude invisible page
  end;
end;

function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result := TabIndexAtClientPos(Point(X, Y));
end;

function TCustomTabControl.TabToPageIndex(AIndex: integer): integer;
var
  I: integer;
begin
(* Map widgetset Tab index into LCL Page index.
  Taken from win32 NotebookPageRealToLCLIndex
*)
  Result := AIndex;
  if FUnPaged or (csDesigning in ComponentState) then
    exit; //all pages are visible
  I := 0;
  while (I < PageCount) and (I <= Result) do
  begin
    if not Page[I].TabVisible then
      Inc(Result); //insert invisible page
    Inc(I);
  end;
end;


{------------------------------------------------------------------------------
  method TCustomTabControl DoCloseTabClicked
  Params: APage: TCustomPage
  Result: none

  Called whenever the user closes the tab.
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.DoCloseTabClicked(APage: TCustomPage);
begin
  if Assigned(OnCloseTabClicked) then OnCloseTabClicked(APage);
end;

{------------------------------------------------------------------------------
  TCustomTabControl GetActivePage
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetActivePage: String;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
    Result := Page[FPageIndex].Caption
  else
    Result := '';
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.GetActivePageComponent: TCustomPage;
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetActivePageComponent: TCustomPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
    Result := Page[FPageIndex]
  else
    Result := nil;
end;

function TCustomTabControl.GetDisplayRect: TRect;
begin
  Result := GetClientRect; //???
end;

function TCustomTabControl.GetMultiLine: Boolean;
begin
  Result := nboMultiLine in Options;
end;

{------------------------------------------------------------------------------
  TCustomTabControl SetActivePage
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetActivePage(const Value: String);
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
  begin
    if Page[i].Caption = Value then
    begin
      SetPageIndex(i);
      Exit;
    end;
  end;
end;

procedure TCustomTabControl.SetActivePageComponent(const AValue: TCustomPage);
begin
  if not FUnPaged then
    PageIndex := FPageList.IndexOf(AValue);
end;

procedure TCustomTabControl.SetImages(const AValue: TCustomImageList);
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

procedure TCustomTabControl.SetOptions(const AValue: TCTabControlOptions);
var
  ChangedOptions: TCTabControlOptions;
begin
  if FOptions = AValue then Exit;
  ChangedOptions := (FOptions - AValue) + (AValue - FOptions);
  FOptions := AValue;
  if nboShowCloseButtons in ChangedOptions then
    UpdateTabProperties;
  if HandleAllocated then
    TWSCustomTabControlClass(WidgetSetClass).UpdateProperties(Self);
end;

{------------------------------------------------------------------------------
  TCustomTabControl SetPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetPageIndex(AValue: Integer);
begin
  if (csLoading in ComponentState) then FLoadedPageIndex := AValue;
  //debugln('TCustomTabControl.SetPageIndex A ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated),' ',dbgs(ComponentState));
  if (AValue < -1) or (AValue >= PageCount) then Exit;
  if FPageIndex = AValue then exit;
  if not CanChangePageIndex then exit;
  //debugln('TCustomTabControl.SetPageIndex B ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated));

  FPageIndex := AValue;
  if not FUnPaged then
    UpdateAllDesignerFlags;
  DoSendPageIndex;
  if ([csDesigning, csLoading, csDestroying] * ComponentState = []) then
    DoChange;
end;

{$IFDEF old}
{------------------------------------------------------------------------------
  TCustomTabControl GetPageIndex
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetPageIndex: Integer;
begin
  Result := FPageIndex;
end;
{$ELSE}
//if override is required, make virtual first!
{$ENDIF}

procedure TCustomTabControl.InsertPage(APage: TCustomPage; Index: Integer);
var
  NewZPosition: integer;
begin
  if FUnPaged or (IndexOf(APage) >= 0) then Exit;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.InsertPage A ',dbgsName(Self),' Index=',Index,' Name=',
    APage.Name,' Caption=',APage.Caption]);
  {$ENDIF}
  APage.DisableAlign;
  try
    if Index < PageCount then
      NewZPosition := GetControlIndex(Page[Index])
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
  DebugLn(['TCustomTabControl.InsertPage END ',dbgsName(Self),' Index=',
    Index,' Name=',APage.Name,' Caption=',APage.Caption]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TCustomTabControl MoveTab
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.MoveTab(Sender: TObject; NewIndex: Integer);
begin
  if (Sender <> nil) and (NewIndex < PageCount) then begin
    TNBPages(fAccess).Move(TCustomPage(Sender).PageIndex,NewIndex);
  end
  else ; //raise exception?
  Change;
end;

procedure TCustomTabControl.SetMultiLine(const AValue: Boolean);
begin
  if AValue then
    Options := Options + [nboMultiLine]
  else
    Options := Options - [nboMultiLine];
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.FindVisiblePage(Index: Integer): Integer;

  It tries to find the next (at right) visible page. If no one is found,
  it tries to to find the previous (at left) visible page.
  Returns -1 if there's no visible pages.
 ------------------------------------------------------------------------------}

function TCustomTabControl.FindVisiblePage(Index: Integer): Integer;
begin
  for Result := Index to PageCount - 1 do
    if Page[Result].TabVisible then
      exit;
  // if arrived here no visible forward page was found, search backwards
  for Result := Index - 1 downto 0 do
    if Page[Result].TabVisible then
      exit;
  Result := -1;
end;

procedure TCustomTabControl.PageRemoved(Index: Integer);
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

procedure TCustomTabControl.WSMovePage(APage: TCustomPage; NewIndex: Integer);
var
  RealIndex: Integer;
  i: Integer;
begin
  //DebugLn(['TCustomTabControl.WSMovePage APage=',DbgSName(APage),' NewIndex=',NewIndex,' pfAdded=',pfAdded in APage.FFlags]);
  if HandleAllocated and (pfAdded in APage.FFlags) then begin
    RealIndex:=0;
    i:=0;
    repeat
      if (i=NewIndex) or (i=PageCount) then break;
      if pfAdded in Page[i].FFlags then inc(RealIndex);
      inc(i);
    until false;
    //DebugLn(['TCustomTabControl.WSMovePage APage=',DbgSName(APage),' NewIndex=',NewIndex,' RealIndex=',RealIndex]);
    TWSCustomTabControlClass(WidgetSetClass).MovePage(Self, APage, RealIndex);
  end;
end;

procedure TCustomTabControl.AddRemovePageHandle(APage: TCustomPage);
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomTabControl.AddRemovePageHandle'){$ENDIF};
  try
    if (not (csDestroying in APage.ComponentState))
    and (APage.TabVisible or (csDesigning in ComponentState)) then begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomTabControl.AddRemovePageHandle ADD ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
      {$ENDIF}
      if (pfAdded in APage.FFlags) then exit;
      Include(APage.FFlags,pfAdding);
      TWSCustomTabControlClass(WidgetSetClass).AddPage(Self, APage, APage.VisibleIndex);
      APage.FFlags:=APage.FFlags+[pfAdded]-[pfAdding];
      APage.AdjustSize;
    end else begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomTabControl.AddRemovePageHandle REMOVE ',DbgSName(APage),' pfAdded=',pfAdded in APage.FFlags]);
      {$ENDIF}
      if not (pfAdded in APage.FFlags) or (pfRemoving in APage.FFlags) then
        exit;
      APage.FFlags := APage.FFlags - [pfAdded] + [pfRemoving];
      TWSCustomTabControlClass(WidgetSetClass).RemovePage(Self, APage.VisibleIndex);
      if APage.HandleAllocated then
        APage.DestroyHandle;
      Exclude(APage.FFlags, pfRemoving);
    end;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomTabControl.AddRemovePageHandle'){$ENDIF};
  end;
end;

procedure TCustomTabControl.RemovePage(Index: Integer);
var
  APage: TCustomPage;
begin
  // Make sure Index is in the range of valid pages to delete
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.RemovePage A ',dbgsName(Self),' Index=',Index,
    ' FPageList.Count=',PageCount,' PageIndex=',PageIndex]);
  {$ENDIF}
  if not FUnPaged and (Index >= 0) and (Index < PageCount) then
  begin
    APage:=Page[Index];
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
  DebugLn(['TCustomTabControl.RemovePage END ',dbgsName(Self),' Index=',Index,' fPageList.Count=',fPageList.Count,' PageIndex=',PageIndex]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.IsStoredActivePage: boolean;
 ------------------------------------------------------------------------------}
function TCustomTabControl.IsStoredActivePage: boolean;
begin
  Result:=false;
end;

procedure TCustomTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (nboKeyboardTabSwitch in Options) and (Key = VK_TAB) and (PageCount > 0) then 
  begin
    if Shift = [ssCtrl] then 
    begin
      PageIndex := (PageIndex + 1) mod PageCount;
      Exit;
    end
    else if Shift = [ssCtrl, ssShift] then 
    begin
      PageIndex := (PageIndex + PageCount - 1) mod PageCount;
      Exit;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

{------------------------------------------------------------------------------
  TCustomTabControl GetPageCount
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetPageCount: Integer;
begin
  if FUnPaged then
    Result := FAccess.Count
  else
    Result := fPageList.Count;
end;

{------------------------------------------------------------------------------
  TCustomTabControl SetPages
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetPages(AValue: TStrings);
begin
  FAccess.Assign(AValue);
end;

{------------------------------------------------------------------------------
  TCustomTabControl GetPage
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetPage(AIndex: Integer): TCustomPage;
begin
  if (AIndex < 0) or (AIndex >= PageCount) then
    RaiseGDBException('TCustomTabControl.GetCustomPage Index out of bounds');
  Result := TCustomPage(FPageList.Items[AIndex]);
end;

{------------------------------------------------------------------------------
  TCustomTabControl SetShowTabs
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetShowTabs(AValue: Boolean);
begin
  if fShowTabs=AValue then exit;
  fShowTabs := AValue;
  DoSendShowTabs;
end;

{------------------------------------------------------------------------------
  TCustomTabControl SetTabPosition
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetTabPosition(tabPos: TTabPosition);
begin
  if fTabPosition = tabPos then exit;
  fTabPosition := tabPos;
  DoSendTabPosition;
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.UpdateAllDesignerFlags;
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  if FUnPaged then
    exit;
  for i:=0 to PageCount-1 do
    UpdateDesignerFlags(i);
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.UpdateDesignerFlags(APageIndex: integer);
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.UpdateDesignerFlags(APageIndex: integer);
begin
  if APageIndex<>fPageIndex then
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle+[csNoDesignVisible]
  else
    Page[APageIndex].ControlStyle:=
      Page[APageIndex].ControlStyle-[csNoDesignVisible];
end;

class procedure TCustomTabControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomTabControl();
end;

{------------------------------------------------------------------------------
  TCustomTabControl ReadState
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.ReadState(Reader: TReader);
begin
  // do not clear. Think about loading ancestor + loading descendant stream.
  // fAccess.Clear;
  inherited ReadState(Reader);
end;

{------------------------------------------------------------------------------
  TCustomTabControl ShowControl
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.ShowControl(APage: TControl);
var
  i: LongInt;
begin
  //inherited ShowControl(AControl);
  { Find a child control that matches the one passed in and display
    the page that contains that control. This method is necessary
    for compatibility with Delphi }
  for i := 0 to PageCount - 1 do begin
    if Page[i] = APage then begin
      PageIndex := i;
      Exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  method TCustomTabControl UpdateTabProperties
  Params: none
  Result: none

  Tells the interface to update all tabs.
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.UpdateTabProperties;
var
  i: integer;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  for i := 0 to PageCount - 1 do
    TWSCustomPageClass(Page[i].WidgetSetClass).UpdateProperties(Page[i]);
end;

class function TCustomTabControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 200;
end;

procedure TCustomTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    SetImages(nil);
end;

{------------------------------------------------------------------------------
  TCustomTabControl Change
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.Change;
begin
  //DebugLn(['TCustomTabControl.Change ',DbgSName(Self),' fPageIndex=',fPageIndex]);
  ShowCurrentPage;
  FPageIndexOnLastChange := FPageIndex;
  if ([csLoading,csDestroying]*ComponentState=[]) and (not FAddingPages) then
    DoChange;
end;

procedure TCustomTabControl.Loaded;
begin
  inherited Loaded;
  if FLoadedPageIndex >= -1 then
    PageIndex := FLoadedPageIndex;
  FLoadedPageIndex := -1;
  //DebugLn(['TCustomTabControl.Loaded ',DbgSName(Self),' fPageIndex=',fPageIndex]);
  FPageIndexOnLastChange := PageIndex;
  FPageIndexOnLastShow := FPageIndexOnLastChange;
  if HandleAllocated then DoCreateWnd;
end;

function TCustomTabControl.DialogChar(var Message: TLMKey): boolean;
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
  TCustomTabControl CNNotify
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.CNNotify(var Message: TLMNotify);
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
            //debugln(['TCustomTabControl.CNNotify ',DbgSName(Self),' A Old=',OldPageIndex,' fPageIndex=',fPageIndex,' FLoadedPageIndex=',FLoadedPageIndex]);
            //if PageIndex>=0 then DebugLn(['TCustomTabControl.CNNotify Page=',DbgSName(Page[PageIndex]),' Visible=',Page[PageIndex].Visible]);
            UpdateAllDesignerFlags;
            if ([csLoading,csDestroying]*ComponentState=[]) then
            begin
              if OldPageIndex <> FPageIndex then
              begin
                if csDesigning in ComponentState then
                  OwnerFormDesignerModified(Self);
                //DebugLn(['TCustomTabControl.CNNotify ',DbgSName(Page[PageIndex]),' ',Page[PageIndex].Visible]);
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
          //debugln('TCustomTabControl.CNNotify TCN_SELCHANGING Result=',dbgs(Result));
        end;
    else
      begin
        {$IFDEF NOTEBOOK_DEBUG}
        DebugLn(['[TCustomTabControl.CNNotify] unhandled NMHdr code:', NMHdr^.code]);
        {$ENDIF}
      end;
    end;
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.ShowCurrentPage

  Makes sure Visible = true for page which has index FPageIndex
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.ShowCurrentPage;
var
  CurPage: TCustomPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
  begin
    CurPage := Page[FPageIndex];
    // first make the new page visible
    //DebugLn(['TCustomTabControl.ShowCurrentPage ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible]);
    CurPage.UpdateControlState;
    if CurPage.Visible then
    begin
      if FPageIndexOnLastShow <> FPageIndex then
      begin
        // some widgetsets like win32/64 do not send WM_SIZE messages for
        // hidden pages. Force resizing page (it is alClient).
        //DebugLn(['TCustomTabControl.ShowCurrentPage ',dbgsName(Self),' ',DbgSName(CurPage),' CurPage.Visible=',CurPage.Visible,' BoundsRect=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect),' CurPage.BoundsRect=',dbgs(CurPage.BoundsRect),' CurPage.ClientRect=',dbgs(CurPage.ClientRect)]);
        ReAlign;
        // TCustomPage.IsControlVisible is overriden
        // therefore AutoSizing of children was skipped => do it now
        CurPage.ReAlign;
      end;
    end else
    begin
      CurPage.Visible := true;
      //DebugLn(['TCustomTabControl.ShowCurrentPage CurPage.AutoSizeDelayed=',CurPage.AutoSizeDelayed,' ',dbgs(CurPage.ComponentState),' ',CurPage.HandleAllocated]);
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
  procedure TCustomTabControl.DoSendPageIndex;
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.DoSendPageIndex;
begin
  //DebugLn('[TCustomTabControl.DoSendPageIndex] A ',dbgsName(Self),' PageIndex=',dbgs(fPageIndex),' ',dbgs(csLoading in ComponentState),' ',dbgs(HandleAllocated));
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('[TCustomTabControl.DoSendPageIndex] B ',dbgsName(Self),' PageIndex=',dbgs(fPageIndex));
  {$ENDIF}
  ShowCurrentPage;
  FPageIndexOnLastChange := FPageIndex;
  TWSCustomTabControlClass(WidgetSetClass).SetPageIndex(Self, FPageIndex);
  {$IFDEF NOTEBOOK_DEBUG}
  //DebugLn('[TCustomTabControl.DoSendPageIndex] END ',dbgs(FPageIndex));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.DoSendShowTabs;
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.DoSendShowTabs;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('[TCustomTabControl.DoSendShowTabs] A ',dbgsName(Self));
  {$ENDIF}
  TWSCustomTabControlClass(WidgetSetClass).ShowTabs(Self, FShowTabs);
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn('[TCustomTabControl.DoSendShowTabs] B ',dbgsName(Self));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.DoSendTabPosition;
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.DoSendTabPosition;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  TWSCustomTabControlClass(WidgetSetClass).SetTabPosition(Self, FTabPosition);
end;

procedure TCustomTabControl.DoImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    TWSCustomTabControlClass(WidgetSetClass).SetImageList(Self, Images);
end;


