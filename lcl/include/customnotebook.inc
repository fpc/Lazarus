{%MainUnit ../comctrls.pp}

{******************************************************************************
                                   TNBPages
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{off $DEFINE NOTEBOOK_DEBUG}

const
  TabControlOptionStr: Array[TCTabControlOption] of String = (
      'nboShowCloseButtons', 'nboMultiLine', 'nboHidePageListPopup',
      'nboKeyboardTabSwitch', 'nboShowAddTabButton', 'nboDoChangeOnSetIndex'
    );

function DbgS(Opt: TCTabControlOptions): String; overload;
var
  O: TCTabControlOption;
begin
  Result := '';
  for O in Opt do Result := Result +  TabControlOptionStr[O] + ',';
  if (Length(Result) > 0) then System.Delete(Result, Length(Result), 1);
  Result := '[' + Result + ']';
end;

{------------------------------------------------------------------------------
  TNBPages Constructor
 ------------------------------------------------------------------------------}
constructor TNBPages.Create(theNotebook: TCustomTabControl);
begin
  inherited Create(theNotebook);
  FPageList := TListWithEvent.Create;
  FPageList.OnChange:=@PageListChange;
  FNotebook := theNotebook;
end;

destructor TNBPages.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPageList);
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
      APage.Parent:=FNotebook;
  end;
end;

{------------------------------------------------------------------------------
  TNBPages Get
 ------------------------------------------------------------------------------}
function TNBPages.Get(Index: Integer): String;
begin
  //DebugLn('TNBPages.Get Index=',Index);
  if (Index<0) or (Index>=FPageList.Count) then
    RaiseGDBException('TNBPages.Get Index out of bounds');
  Result := TCustomPage(FPageList[Index]).Caption;
end;

{------------------------------------------------------------------------------
  TNBPages GetCount
 ------------------------------------------------------------------------------}
function TNBPages.GetCount: Integer;
begin
  Result := FPageList.Count;
end;

{------------------------------------------------------------------------------
  TNBPages GetObject
 ------------------------------------------------------------------------------}
function TNBPages.GetObject(Index: Integer): TObject;
begin
  if (Index<0) or (Index>=FPageList.Count) then
    RaiseGDBException('TNBPages.GetObject Index out of bounds');
  Result := TObject(FPageList[Index]);
end;

{------------------------------------------------------------------------------
  TNBPages Put
 ------------------------------------------------------------------------------}
procedure TNBPages.Put(Index: Integer; const S: String);
begin
  if (Index<0) or (Index>=FPageList.Count) then
    RaiseGDBException('TNBPages.Put Index out of bounds');
  //debugln(['TNBPages.Put ',DbgSName(FNotebook),' ',Index,' S="',S,'"']);
  TCustomPage(FPageList[Index]).Caption := S;
end;

function TNBPages.IndexOfPage(const AnObject: TPersistent): Integer;
begin
  Result := FPageList.IndexOf(AnObject);
end;

procedure TNBPages.InsertPage(Index: Integer; const APage: TCustomPage);
begin
  FPageList.Insert(Index, APage);
end;

procedure TNBPages.DeletePage(Index: Integer);
begin
  FPageList.Delete(Index);
end;

function TNBPages.GetPage(Index: Integer): TCustomPage;
begin
  Result := TCustomPage(GetObject(Index));
end;

{------------------------------------------------------------------------------
  TNBPages Clear
 ------------------------------------------------------------------------------}
procedure TNBPages.Clear;
begin
  while FPageList.Count>0 do
    Delete(FPageList.Count-1);
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
  DebugLn(['TNBPages.Delete B ',FNotebook.Name,' Index=',Index,' FPageList.Count=',FPageList.Count,' FNotebook.PageIndex=',FNotebook.PageIndex]);
  {$ENDIF}
  if (Index >= 0) and
     (Index < FPageList.Count) then
  begin
    APage := TCustomPage(FPageList[Index]);
    // delete handle
    APage.Parent := nil;
    // free the page
    Application.ReleaseComponent(APage);
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TNBPages.Delete END ',FNotebook.Name,' Index=',Index,' FPageList.Count=',FPageList.Count,' FNotebook.PageIndex=',FNotebook.PageIndex]);
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
  DebugLn(['TNBPages.Insert A ',FNotebook.Name,' Index=',Index,' S="',S,'"']);
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
  FNotebook.InsertPage(NewPage,Index);
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
  NewControlIndex: integer;
  ActivePageIndex: Integer;
  ActivePage: TCustomPage;
begin
  if CurIndex = NewIndex then Exit;
  ActivePageIndex := FNotebook.PageIndex;
  if (FNotebook.PageIndex >= 0) and (FNotebook.PageIndex < Count) then
    ActivePage := GetPage(ActivePageIndex)
  else
    ActivePage := nil;
  //NewPageIndex := NewIndex;
  APage := TCustomPage(FPageList[CurIndex]);

  // calculate new control index (i.e. ZOrderPosition)
  if NewIndex >= FPageList.Count - 1 then
    NewControlIndex := FNotebook.ControlCount-1
  else
    NewControlIndex := FNotebook.GetControlIndex(TCustomPage(FPageList[NewIndex]));

  FNotebook.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TNBPages.Move'){$ENDIF};
  try
     // move Page in notebook handle
    FNotebook.WSMovePage(APage, NewIndex);

    // move Page in FPageList
    FPageList.Move(CurIndex, NewIndex);

    // move in wincontrol list
    FNotebook.SetControlIndex(APage, NewControlIndex);

    // update PageIndex
    if ActivePage <> nil then
      FNotebook.InternalSetPageIndex(IndexOfPage(ActivePage))
    else // Can not restore an invalid page index.
    if FNotebook.PageIndex >= 0 then // keep if -1
      FNotebook.PageIndex := NewIndex;
  finally
    FNotebook.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TNBPages.Move'){$ENDIF};
  end;
end;

{ TNBNoPages }

function TNBNoPages.Get(Index: Integer): String;
begin
  Result := '';
end;

function TNBNoPages.GetCount: Integer;
begin
  Result := 0;
end;

function TNBNoPages.IndexOfPage(const AnObject: TPersistent): Integer;
begin
  Result := -1;
end;

function TNBNoPages.GetPage(Index: Integer): TCustomPage;
begin
  Result := nil;
end;

{******************************************************************************
                                TCustomTabControl
 ******************************************************************************}
{------------------------------------------------------------------------------
  TCustomTabControl Constructor
 ------------------------------------------------------------------------------}
constructor TCustomTabControl.Create(TheOwner: TComponent);
begin
  if PageClass=nil then PageClass := GetPageClass;

  inherited Create(TheOwner);

  fCompStyle := csNoteBook;

  FAccess := GetListClass.Create(Self);

  FImageListChangeLink := TChangeLink.Create;
  FImageListChangeLink.OnChange := @DoImageListChange;
  FImageListChangeLink.OnDestroyResolutionHandle := @DoImageListDestroyResolutionHandle;

  FPageIndex := -1;

  ControlStyle := []; // do not add csAcceptsControls
  TabPosition := tpTop;
  TabStop := true;
  ShowTabs := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  // Accessibility
  AccessibleDescription := rsTCustomTabControlAccessibilityDescription;
  AccessibleRole := larTabControl;
end;

{------------------------------------------------------------------------------
  Method:  TCustomTabControl.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.CreateWnd;
var
  i: Integer;
  lPage: TCustomPage;
begin
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.CreateWnd ',dbgsName(Self),' HandleAllocated=',HandleAllocated]);
  {$ENDIF}
  inherited CreateWnd;
  DisableAlign;
  try
    FAddingPages := True;
    for i := 0 to PageCount -1 do
    begin
      {$IFDEF NOTEBOOK_DEBUG}
      DebugLn(['TCustomTabControl.CreateWnd ',dbgsName(Self),' Page.Caption=',Page[i].Caption,' pfAdded=',pfAdded in Page[i].Flags]);
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

procedure TCustomTabControl.Loaded;
begin
  inherited Loaded;
  if HandleAllocated then
    DoSendPageIndex;
end;

procedure TCustomTabControl.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCustomTabControl.InitializeWnd;
begin
  inherited InitializeWnd;
  //DebugLn(['TCustomTabControl.InitializeWnd ',DbgSName(Self),' fPageIndex=',fPageIndex]);
  FPageIndexOnLastChange := PageIndex;
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
  Application.RemoveAsyncCalls(Self);
  inherited Destroy;
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
  Result:=TNBPages(FAccess).IndexOfPage(APage);
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

function TCustomTabControl.GetCapabilities: TCTabControlCapabilities;
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

  // it is possible to show pages without visible tabs, but then, no tab index
  // can be sendet back, issue #21723
  if not Page[AIndex].TabVisible then
    exit(-1);

  for i := 0 to AIndex - 1 do begin
    if not Page[i].TabVisible then
      dec(Result); //exclude invisible page
  end;
end;

function TCustomTabControl.TabToPageIndex(AIndex: integer): integer;
var
  I: integer;
begin
(* Map widgetset Tab index into LCL Page index.
  Taken from win32 NotebookPageRealToLCLIndex
*)
  Result := AIndex;
  if (csDesigning in ComponentState) then
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
  PageIndex := IndexOf(AValue); // -1 for unpaged
end;

procedure TCustomTabControl.SetImageListAsync(Data: PtrInt);
begin
  DoImageListChange(Self);
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

procedure TCustomTabControl.SetImagesWidth(const aImagesWidth: Integer);
begin
  if FImagesWidth = aImagesWidth then Exit;
  FImagesWidth := aImagesWidth;
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
  if (AValue < -1) or (AValue >= PageCount) then Exit;
  //debugln('TCustomTabControl.SetPageIndex A ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated),' ',dbgs(ComponentState));
  if FPageIndex = AValue then exit;
  if not CanChangePageIndex then exit;
  //debugln('TCustomTabControl.SetPageIndex B ',dbgsName(Self),' AValue=',dbgs(AValue),' fPageIndex=',dbgs(fPageIndex),' PageCount=',dbgs(PageCount),' HandleAllocated=',dbgs(HandleAllocated));

  InternalSetPageIndex(AValue);

  //debugln(['TCustomTabControl.SetPageIndex C ',dbgsName(Self)]);
  //debugln(['  FOptions = ',DbgS(Foptions)]);

  if ([csDesigning, csLoading, csDestroying] * ComponentState = []) and (nboDoChangeOnSetIndex in Options) then
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
  // only called from TNBPages, but not TNBNoPages
  // Also called from TCustomPage.SetParent
  if (IndexOf(APage) >= 0) then Exit;
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
    TNBPages(FAccess).InsertPage(Index, APage);
    Exclude(APage.FFlags, pfInserting);
    APage.Parent := Self; // will recursively call
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
  if Assigned(Sender) and (NewIndex < PageCount) then
  begin
    TNBPages(fAccess).Move(TCustomPage(Sender).PageIndex, NewIndex);
    Change;
  end;
end;

procedure TCustomTabControl.SetMultiLine(const AValue: Boolean);
begin
  if AValue then
    Options := Options + [nboMultiLine]
  else
    Options := Options - [nboMultiLine];
end;

procedure TCustomTabControl.SetStyle(AValue: TTabStyle);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;
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
  if HandleAllocated and (pfAdded in APage.FFlags) then
  begin
    RealIndex := 0;
    i := 0;
    repeat
      if (i = NewIndex) or (i = PageCount) then break;
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
    ' FAccess.Count=',PageCount,' PageIndex=',PageIndex]);
  {$ENDIF}
  if (Index >= 0) and (Index < PageCount) then
  begin
    APage:=Page[Index];
    APage.FTabVisible:=false;
    if HandleAllocated then
      AddRemovePageHandle(APage);
    PageRemoved(Index);
    TNBPages(FAccess).DeletePage(Index);
    APage.Parent:=nil;
    if FPageIndex >= Index then
      Dec(FPageIndex);
  end;
  {$IFDEF NOTEBOOK_DEBUG}
  DebugLn(['TCustomTabControl.RemovePage END ',dbgsName(Self),' Index=',Index,' FAccess.Count=',FAccess.Count,' PageIndex=',PageIndex]);
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
      Key := 0;
      PageIndex := (PageIndex + 1) mod PageCount;
      Exit;
    end
    else if Shift = [ssCtrl, ssShift] then 
    begin
      Key := 0;
      PageIndex := (PageIndex + PageCount - 1) mod PageCount;
      Exit;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

{------------------------------------------------------------------------------
  TCustomTabControl GetPageCount
 ------------------------------------------------------------------------------}
function TCustomTabControl.GetPageCount: integer;
begin
  Result := FAccess.Count
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
  Result := TNBPages(FAccess).GetPage(AIndex);
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
  TCustomTabControl SetTabHeight
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetTabHeight(AValue: Smallint);
begin
  if FTabHeight = AValue then Exit;
  if not (nbcTabsSizeable in GetCapabilities) then Exit;
  FTabHeight := AValue;
  DoSendTabSize;
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
  TCustomTabControl SetTabWidth
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.SetTabWidth(AValue: Smallint);
begin
  if FTabWidth = AValue then Exit;
  if not (nbcTabsSizeable in GetCapabilities) then Exit;
  FTabWidth := AValue;
  DoSendTabSize;
end;

{------------------------------------------------------------------------------
  procedure TCustomTabControl.UpdateAllDesignerFlags;
 ------------------------------------------------------------------------------}
procedure TCustomTabControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
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

function TCustomTabControl.GetPageClass: TCustomPageClass;
begin
  Result := TCustomPage;
end;

function TCustomTabControl.GetListClass: TNBBasePagesClass;
begin
  Result := TNBPages;
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
  { Find a child control that matches the one passed in and display
    the page that contains that control. This method is necessary
    for compatibility with Delphi }
  for i := 0 to PageCount - 1 do begin
    if Page[i] = APage then begin
      PageIndex := i;
      Exit;
    end;
  end;
  inherited ShowControl(APage);
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;

  Returns the index of the visible tab at the client position.
 ------------------------------------------------------------------------------}
function TCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result := IndexOfTabAt(Point(X, Y));
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.IndexOfTabAt(P: TPoint): Integer;

  Returns the index of the visible tab at the client position.
  For example:
    Index:=NoteBook1.IndexOfTabAt(
               NoteBook1.ScreenToClient(Mouse.CursorPos));
 ------------------------------------------------------------------------------}
function TCustomTabControl.IndexOfTabAt(P: TPoint): Integer;
begin
  if HandleAllocated then
    Result := TWSCustomTabControlClass(WidgetSetClass).GetTabIndexAtPos(Self, P)
  else
    Result := -1;
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.IndexOfPageAt(X, Y: Integer): Integer;

  Returns the index of the page at the client position.
 ------------------------------------------------------------------------------}
function TCustomTabControl.IndexOfPageAt(X, Y: Integer): Integer;
begin
  Result := IndexOfPageAt(Point(X, Y));
end;

{------------------------------------------------------------------------------
  function TCustomTabControl.IndexOfPageAt(X, Y: Integer): Integer;

  Returns the index of the page at the client position.
  For example:
    Index:=NoteBook1.IndexOfPageAt(
               NoteBook1.ScreenToClient(Mouse.CursorPos));
 ------------------------------------------------------------------------------}
function TCustomTabControl.IndexOfPageAt(P: TPoint): Integer;
begin
  Result := IndexOfTabAt(P);
  if Result <> -1 then
    Result := TabToPageIndex(Result);
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

procedure TCustomTabControl.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion,
  AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    if Assigned(Images) then
      DoImageListChange(Self);
  end;
end;

procedure TCustomTabControl.InternalSetPageIndex(AValue: Integer);
begin
  FPageIndex := AValue;
  UpdateAllDesignerFlags;
  DoSendPageIndex;
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

  function HasFocusedControl(APage: TCustomPage): Boolean;
  var
    i: Integer;
    lForm: TCustomForm;
  begin
    Result := False;
    lForm := GetParentForm(APage);
    if not Assigned(lForm) then Exit;
    for i := 0 to APage.ControlCount - 1 do
      if APage.Controls[i] = lForm.ActiveControl then
        Exit(True);
  end;

var
  CurPage: TCustomPage;
begin
  CurPage := nil;
  if (FPageIndex >= 0) and (FPageIndex < PageCount) then
  begin
    CurPage := Page[FPageIndex];
    CurPage.Visible := True;
    //DebugLn(['TCustomTabControl.ShowCurrentPage CurPage.AutoSizeDelayed=',CurPage.AutoSizeDelayed,' ',dbgs(CurPage.ComponentState),' ',CurPage.HandleAllocated]);
  end;
  if (FPageIndexOnLastChange >= 0) and (FPageIndexOnLastChange < PageCount) and
     (FPageIndexOnLastChange <> FPageIndex) then
  begin
    if Assigned(CurPage) and HasFocusedControl(Page[FPageIndexOnLastChange]) then
      CurPage.SetFocus;
    Page[FPageIndexOnLastChange].Visible := False;
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

procedure TCustomTabControl.DoSendTabSize;
begin
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  TWSCustomTabControlClass(WidgetSetClass).SetTabSize(Self, FTabWidth, FTabHeight);
  DoSendTabPosition;
  Invalidate;
end;

procedure TCustomTabControl.DoImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    TWSCustomTabControlClass(WidgetSetClass).SetImageList(Self, Images.ResolutionForPPI[ImagesWidth, Font.PixelsPerInch, 1].Resolution); // to-do: support scaling factor
end;

procedure TCustomTabControl.DoImageListDestroyResolutionHandle(
  Sender: TCustomImageList; AWidth: Integer; AReferenceHandle: TLCLHandle);
begin
  TWSCustomTabControlClass(WidgetSetClass).SetImageList(Self, nil);
  Application.QueueAsyncCall(@SetImageListAsync, 0);
end;

