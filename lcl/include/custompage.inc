{%MainUnit ../extctrls.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  TCustomPage Constructor
 ------------------------------------------------------------------------------}
constructor TCustomPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fImageIndex:=-1;
  fCompStyle := csPage;
  fTabVisible := True;
  ControlStyle := ControlStyle + [csAcceptsControls,csDesignFixedBounds,csNoDesignVisible];

  // height and width depends on parent, align to client rect
  Align := alClient;
  Caption := '';
  Visible := false;
end;

{------------------------------------------------------------------------------
  method TCustomPage SetImageIndex
  Params: const AValue: integer
  Result: none

  Set the image index of the image shown in the tabs.
 ------------------------------------------------------------------------------}
procedure TCustomPage.SetImageIndex(const AValue: integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if not HandleAllocated or (csLoading in ComponentState) then exit;
  TWSCustomPageClass(WidgetSetClass).UpdateProperties(Self);
end;

function TCustomPage.GetTabVisible: Boolean;
begin
  Result := fTabVisible;
end;

procedure TCustomPage.SetTabVisible(const AValue: Boolean);
begin
  if AValue = FTabVisible then exit;
  FTabVisible := AValue;
 
 if csDesigning in ComponentState then
    Exit;

  if Parent.HandleAllocated then
  begin
    TCustomNotebook(Parent).AddRemovePageHandle(Self);
    if FTabVisible then
    begin
      // check if there was no visible tab
      if TCustomNotebook(Parent).PageIndex = -1 then
        TCustomNotebook(Parent).PageIndex:=PageIndex;
    end
    else
      // Check if the page is active and set a new pageindex
      TCustomNotebook(Parent).PageRemoved(PageIndex);
  end;
end;

{------------------------------------------------------------------------------
  TCustomPage WMPaint
  Params: a TLMPaint message
 ------------------------------------------------------------------------------}
procedure TCustomPage.WMPaint(var Msg: TLMPaint);
var Notebook: TCustomNoteBook;
begin
  if (Parent<>nil) and (Parent is TCustomNoteBook) then begin
    NoteBook:=TCustomNoteBook(Parent);
    if (NoteBook.PageIndex>=0) and (NoteBook.Page[NoteBook.PageIndex]=Self) then
      inherited WMPaint(Msg);
  end else
    inherited WMPaint(Msg);
end;

{------------------------------------------------------------------------------
  procedure TCustomPage.SetParent(AParent: TWinControl);

  Set parent wincontrol.
 ------------------------------------------------------------------------------}
procedure TCustomPage.SetParent(AParent: TWinControl);
var
  OldParent: TWinControl;
  ParentNotebook: TCustomNotebook;
  i: integer;
begin
  if AParent=Parent then exit;
  CheckNewParent(AParent);
  OldParent:=Parent;
  if (OldParent<>AParent) and (OldParent<>nil)
  and (OldParent is TCustomNotebook)
  and (not (pfRemoving in FFlags)) then begin
    // remove from old pagelist
    ParentNotebook := TCustomNotebook(OldParent);
    i := PageIndex;
    if i >= 0 then
      ParentNotebook.RemovePage(i);
  end;

  inherited SetParent(AParent);

  if (OldParent<>AParent) and (Parent<>nil)
  and (Parent is TCustomNotebook) then begin
    // add to new pagelist
    ParentNotebook:=TCustomNotebook(Parent);
    i:=ParentNotebook.PageList.IndexOf(Self);
    if i>=0 then
      ParentNotebook.PageList.Delete(i)
    else
      i:=ParentNotebook.PageCount;
    ParentNotebook.InsertPage(Self,i);
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomPage.CMHitTest(var Message: TLMNCHITTEST);
 ------------------------------------------------------------------------------}
procedure TCustomPage.CMHitTest(var Message: TLMNCHITTEST);
begin
  if (Parent<>nil) and (Parent is TCustomNotebook)
  and (TCustomNotebook(Parent).ActivePageComponent<>Self) then
    Message.Result:=0 // no hit
  else
    inherited CMHitTest(Message);
  {DebugLn('TCustomPage.CMHitTest A ',Name,' ',(Parent<>nil),' ',
    (Parent is TCustomNotebook),' ',
    (TCustomNotebook(Parent).ActivePageComponent<>Self),
    ' Message.Result=',Message.Result);}
end;

{------------------------------------------------------------------------------
  TCustomPage AdjustClientRect
  Params: Rect
 ------------------------------------------------------------------------------}
procedure TCustomPage.AdjustClientRect(var ARect: TRect);
begin
  ARect:=GetClientRect;
end;

{------------------------------------------------------------------------------
  function TCustomPage.PageIndex: integer;

  Returns the index of the page in the notebook.
 ------------------------------------------------------------------------------}
function TCustomPage.GetPageIndex: integer;
begin
  if (Parent<>nil) and (Parent is TCustomNotebook) then
    Result:=TCustomNotebook(Parent).PageList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TCustomPage.SetPageIndex(AValue: Integer);
begin
  if (Parent<>nil) and (Parent is TCustomNotebook) then begin
    TCustomNotebook(Parent).MoveTab(Self,AValue);
  end;
  //DebugLn('TCustomPage.SetPageIndex Old=',dbgs(PageIndex),' New=',dbgs(AValue));
end;

function TCustomPage.DialogChar(var Message: TLMKey): boolean;
begin
  Result := false;
  if (not (csDesigning in ComponentState))
  and IsAccel(Message.CharCode, Caption) and TabVisible then begin
    Result := true;
    if (Parent<>nil) and (Parent is TCustomNotebook) then
      TCustomNotebook(Parent).PageIndex:=PageIndex;
  end else
    Result:=inherited DialogChar(Message);
end;

procedure TCustomPage.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TCustomPage.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TCustomPage.DestroyHandle;
begin
  inherited DestroyHandle;
  Exclude(FFlags,pfAdded);
end;

function TCustomPage.IsControlVisible: Boolean;
begin
  if Parent is TCustomNotebook then
  begin
    Result := (PageIndex = TCustomNotebook(Parent).PageIndex);
    //if Name='FormEditorPage' then DebugLn(['TCustomPage.IsControlVisible PageIndex=',PageIndex,' Parent.PageIndex=',TCustomNotebook(Parent).PageIndex]);
  end else begin
    Result := inherited IsControlVisible;
  end;
end;

function TCustomPage.VisibleIndex: integer;
// returns the visible index, as if TabVisible=true
var
  List: TList;
  i: Integer;
begin
  if (Parent<>nil) and (Parent is TCustomNotebook) then begin
    Result:=0;
    List:=TCustomNotebook(Parent).PageList;
    i:=0;
    repeat
      if i=List.Count then exit(-1);
      if (TObject(List[i])=Self) then exit;
      if TCustomPage(List[i]).TabVisible or (csDesigning in ComponentState)
      then inc(Result);
      inc(i);
    until false;
  end else
    Result := -1;
end;

{------------------------------------------------------------------------------
  function TCustomPage.CanTab: boolean;
 ------------------------------------------------------------------------------}
function TCustomPage.CanTab: boolean;
begin
  Result:=false;
end;

function TCustomPage.AutoSizeDelayed: boolean;
begin
  Result:=(not (pfAdded in FFlags)) or (inherited AutoSizeDelayed);
end;

// included by extctrls.pp
