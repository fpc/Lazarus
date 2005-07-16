{%MainUnit ../forms.pp}

{******************************************************************************
                               TCustomForm
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

{ $DEFINE CHECK_POSITION}

{ TCustomForm }

{------------------------------------------------------------------------------}
{       TCustomForm ClientWndProc                                              }
{------------------------------------------------------------------------------}
Procedure TCustomForm.ClientWndProc(var Message: TLMessage);

  procedure CallDefault;
    begin
      {
      with Message do
      Result := CallWindowProc(FDefClientProc, ClientHandle, Msg, wParam, lParam);
     }
    end;

begin
  with Message do
    case Msg of
      LM_NCHITTEST:
        begin
          CallDefault;
          if Result = HTCLIENT then Result := HTTRANSPARENT;
        end;
      LM_ERASEBKGND:
        begin
          // Not sure if this will work real good.
          //Canvas.FillRect(ClientRect);
          Result := 1;
        end;
    else
      CallDefault;
    end;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.CloseModal;
 ------------------------------------------------------------------------------}
procedure TCustomForm.CloseModal;
var
  CloseAction: TCloseAction;
begin
  try
    CloseAction := caNone;
    if CloseQuery then
    begin
      CloseAction := caHide;
      DoClose(CloseAction);
    end;
    case CloseAction of
      caNone: ModalResult := 0;
      caFree: Release;
    end;
    if CloseAction <> caNone then
      TWSCustomFormClass(WidgetSetClass).CloseModal(Self);
  except
    ModalResult := 0;
    Application.HandleException(Self);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.BeforeDestruction
  Params:  None
  Returns: Nothing

  Gets called before the destruction of the object
 ------------------------------------------------------------------------------}
procedure TCustomForm.BeforeDestruction;
begin
  Destroying;
  Screen.FSaveFocusedList.Remove(Self);
  RemoveFixupReferences(Self, '');
  //if FOleForm <> nil then FOleForm.OnDestroy;
  if FormStyle <> fsMDIChild then Hide;
  DoDestroy;
  inherited BeforeDestruction;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomForm.Destroy;
var
  HandlerType: TFormHandlerType;
begin
  //DebugLn('[TCustomForm.Destroy] A ',Name,':',ClassName);
  if not (csDestroying in ComponentState) then ;//GlobalNameSpace.BeginWrite;
  try
    // ------
    // Temp hack to get Beforedestruction called
    // FPC1.0.x doesn't call itself before destruction
    {$IFDEF VER1_0}BeforeDestruction;{$ENDIF}
    // ------
    FreeThenNil(FMenu);
    FreeThenNil(FIcon);
    Screen.RemoveForm(Self);
    FreeThenNil(FActionLists);
    for HandlerType:=Low(FFormHandlers) to High(FFormHandlers) do
      FreeThenNil(FFormHandlers[HandlerType]);
    //DebugLn('[TCustomForm.Destroy] B ',Name,':',ClassName);
    inherited Destroy;
    //DebugLn('[TCustomForm.Destroy] END ',Name,':',ClassName);
  finally
    //GlobalNameSpace.EndWrite;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.FocusControl
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
Procedure TCustomForm.FocusControl(WinControl : TWinControl);
var
  WasActive: Boolean;
begin
  WasActive := FActive;
  SetActiveControl(WinControl);
  if not WasActive then SetFocus;
End;

{------------------------------------------------------------------------------
  Method: TCustomForm.Notification
 ------------------------------------------------------------------------------}
Procedure TCustomForm.Notification(AComponent: TComponent;
  Operation: TOperation);
Begin
  inherited Notification(AComponent,Operation);

  case Operation of

  opInsert:
    begin
      if AComponent is TCustomActionList then begin
        if FActionLists=nil then FActionLists:=TList.Create;
        FActionLists.Add(AComponent);
      end
      else if not (csLoading in ComponentState) and (Menu = nil)
      and (AComponent.Owner=Self) and (AComponent is TMainMenu) then
        Menu:=TMainMenu(AComponent);
    end;

  opRemove:
    begin
      if (FActionLists<>nil) and (AComponent is TCustomActionList) then
        FActionLists.Remove(AComponent)
      else
      begin
        if Menu = AComponent then Menu := nil;
        //if WindowMenu = AComponent then WindowMenu := nil;
        //if ObjectMenuItem = AComponent then ObjectMenuItem := nil;
      end;
      if FActiveControl=AComponent then begin
        {$IFDEF VerboseFocus}
        debugln('TCustomForm.Notification opRemove FActiveControl=',DbgSName(AComponent));
        {$ENDIF}
        FActiveControl:=nil;
      end;
      if AComponent=FDefaultControl then
        FDefaultControl:=nil;
      if AComponent=FCancelControl then
        FCancelControl:=nil;
    end;
  end;
  if FDesigner<>nil then FDesigner.Notification(AComponent,Operation);
End;

{------------------------------------------------------------------------------
  Method: TCustomForm.IconChanged
 ------------------------------------------------------------------------------}
procedure TCustomForm.IconChanged(Sender: TObject);
begin
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetIcon(Self, GetIconHandle);
end;

{------------------------------------------------------------------------------
  function TCustomForm.IsKeyPreviewStored: boolean;
 ------------------------------------------------------------------------------}
function TCustomForm.IsKeyPreviewStored: boolean;
begin
  Result:=FKeyPreview=true;
end;

function TCustomForm.IsShowInTaskBarStored: boolean;
begin
  Result := FShowInTaskBar <> GetShowInTaskBarDefault;
end;

function TCustomForm.GetShowInTaskBarDefault: boolean;
begin
  Result := (Application = nil) or (Application.MainForm = Self);
end;

procedure TCustomForm.SetCancelControl(NewControl: TControl);
var
  OldCancelControl: TControl;
begin
  if NewControl <> FCancelControl then
  begin
    OldCancelControl:=FCancelControl;
    FCancelControl := NewControl;
    // notify old control
    if OldCancelControl<>nil then
      OldCancelControl.UpdateRolesForForm;
    // notify new control
    if FCancelControl<>nil then
      FCancelControl.UpdateRolesForForm;
  end;
end;

procedure TCustomForm.SetDefaultControl(NewControl: TControl);
var
  OldDefaultControl: TControl;
begin
  if NewControl <> FDefaultControl then
  begin
    OldDefaultControl:=FDefaultControl;
    FDefaultControl := NewControl;
    // notify old control
    if OldDefaultControl<>nil then
      OldDefaultControl.UpdateRolesForForm;
    // notify new control
    if FDefaultControl<>nil then
      FDefaultControl.UpdateRolesForForm;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.SetIcon
  Params: the new icon
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetIcon(AValue: TIcon);
begin
  if FIcon=nil then begin
    FIcon:=TIcon.Create;
    FIcon.OnChange := @IconChanged;
  end;
  FIcon.Assign(AValue);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.SetModalResult(const AValue: TModalResult);
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetModalResult(const AValue: TModalResult);
begin
  if FModalResult=AValue then exit;
  FModalResult:=AValue;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.IsIconStored
  Returns: if form icon should be stored in the stream
 ------------------------------------------------------------------------------}
function TCustomForm.IsIconStored: Boolean;
begin
  Result := IsForm and (Icon<>nil);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.GetIconHandle
  Returns: handle of form icon
 ------------------------------------------------------------------------------}
function TCustomForm.GetIconHandle: HICON;
begin
  //DebugLn('[TCustomForm.GetIconHandle] ',ClassName,' ',FIcon<>nil);
  if (FIcon<>nil) and not Icon.Empty then
    Result := FIcon.Handle
  else
    Result := Application.GetIconHandle;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.SetFocus
 ------------------------------------------------------------------------------}
Procedure TCustomForm.SetFocus;

  procedure RaiseCannotFocus;
  var
    s: String;
  begin
    s:='[TCustomForm.SetFocus] '+Name+':'+ClassName+' '+rsCanNotFocus;
    {$IFDEF VerboseFocus}
    RaiseGDBException(s);
    {$ELSE}
    raise EInvalidOperation.Create(s);
    {$ENDIF}
  end;

Begin
  {$IFDEF VerboseFocus}
  DebugLn('TCustomForm.SetFocus ',Name,':',ClassName);
  {$ENDIF}
  if not FActive then
  begin
    if not (Visible and Enabled) then
      RaiseCannotFocus;
    SetWindowFocus;
  end;
end;

{------------------------------------------------------------------------------
       TCustomForm SetVisible
------------------------------------------------------------------------------}
Procedure TCustomForm.SetVisible(Value : boolean);
Begin
  if (Value=(fsVisible in FFormState)) and (Visible=Value) then exit;
  //DebugLn('[TCustomForm.SetVisible] START ',Name,':',ClassName,' Old=',Visible,' New=',Value,' ',(fsCreating in FFormState),' ',FormUpdating);
  if Value then
    Include(FFormState, fsVisible)
  else
    Exclude(FFormState, fsVisible);
  //DebugLn('TCustomForm.SetVisible ',Name,':',ClassName,' FormUpdating=',FormUpdating,' fsCreating=',fsCreating in FFormState);
  if (fsCreating in FFormState) {or FormUpdating} then
    // will be done when finished loading
  else
  begin
    inherited Visible:=Value;
  end;
  //DebugLn('[TCustomForm.SetVisible] END ',Name,':',ClassName,' ',Value,' ',(fsCreating in FFormState),' ',FormUpdating,' ',Visible);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.SetWindowFocus;
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetWindowFocus;
var
  NewFocusControl: TWinControl;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  if (FActiveControl <> nil) and (FDesigner = nil) then
    NewFocusControl := FActiveControl
  else
    NewFocusControl := Self;
  {$IFDEF VerboseFocus}
  DebugLn('TCustomForm.SetWindowFocus ',Name,':',Classname ,
    ' NewFocusControl=',NewFocusControl.Name,':',NewFocusControl.ClassName,
    ' HndAlloc=',dbgs(NewFocusControl.HandleAllocated));
  {$ENDIF}
  if (not NewFocusControl.HandleAllocated)
  or (not NewFocusControl.Visible)
  or (not NewFocusControl.Enabled) then
    exit;
  LCLIntf.SetFocus(NewFocusControl.Handle);
  if GetFocus = NewFocusControl.Handle then
    NewFocusControl.Perform(CM_UIACTIVATE, 0, 0);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMShowWindow
  Params:   Msg: The showwindow message
  Returns:  nothing

  ShowWindow event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMShowWindow(var message: TLMShowWindow);

  function FindFirstControl: TWinControl;
  var
    List: TList;
    I: Integer;
  begin
    List := TList.Create;
    Result := nil;
    try
      GetTabOrderList(List);
      for I := 0 to List.Count - 1 do
      begin
        if TObject(List.Items[0]) is TWinControl then
        begin
          Result := TWinControl(List.Items[0]);
          exit;
        end;
      end;
    finally
      List.Free;
    end;
  end;

begin
  {$IFDEF VerboseFocus}
  DbgOut('TCustomForm.WMShowWindow A ',Name,':'+ClassName+' fsShowing='+dbgs(fsShowing in FFormState)+' Msg.Show='+dbgs(Message.Show));
  if FActiveControl<>nil then begin
    DbgOut(' FActiveControl=',FActiveControl.Name,':',FActiveControl.ClassName,' HandleAllocated=',dbgs(FActiveControl.HandleAllocated));
  end else begin
    DbgOut(' FActiveControl=nil');
  end;
  DebugLn('');
  {$ENDIF}
  if (fsShowing in FFormState) then exit;
  Include(FFormState, fsShowing);
  try
    // only fire event if reason is not some other window hide/showing etc.
    if Message.Status = 0 then
    begin
      if Message.Show then begin
        if FActiveControl = nil then begin
          FActiveControl := FindFirstControl;
          {$IFDEF VerboseFocus}
          DebugLn('TCustomForm.WMShowWindow Set FActiveControl := FindFirstControl = ',DbgSName(FActiveControl));
          {$ENDIF}
        end;
        if ([csLoading,csDestroying]*ComponentState=[])
        and (FActiveControl<>nil) and FActiveControl.HandleAllocated
        and FActiveControl.Visible and FActiveControl.Enabled then begin
          {$IFDEF VerboseFocus}
          DebugLn('TCustomForm.WMShowWindow SetFocus ',DbgSName(FActiveControl));
          {$ENDIF}
          LCLIntf.SetFocus(FActiveControl.Handle);
        end;
        DoShow;
      end else begin
        DoHide;
      end;
    end;
  finally
    Exclude(FFormState, fsShowing);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMActivate
  Params:   Msg: When the form is Activated
  Returns:  nothing

  Activate event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMActivate(var Message : TLMActivate);
begin
  {$IFDEF VerboseFocus}
  DebugLn('TCustomForm.WMActivate A ',Name,':',ClassName,' Msg.Active=',dbgs(Message.Active));
  {$ENDIF}
  if (FormStyle <> fsMDIForm) or (csDesigning in ComponentState) then
    SetActive(Message.Active {<> WA_INACTIVE});
  FActive:=true;
  Activate;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMDeactivate
  Params: Msg: When the form is deactivated (loses focus within application)
  Returns: nothing

  Form deactivation (losing focus within application)  event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMDeactivate(var Message : TLMActivate);
begin
  FActive:=false;
  Deactivate;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Activate
  Params: none
  Returns:  nothing

  Activation form methode event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.Activate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.ActiveChanged;
 ------------------------------------------------------------------------------}
procedure TCustomForm.ActiveChanged;
begin

end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Deactivate
  Params: none
  Returns: nothing

  Form deactivation (losing focus within application)  event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.Deactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMPaint(var Message: TLMPaint);
begin
  //DebugLn('[TCustomForm.WMPaint] ',Name,':',ClassName);
  inherited WMPaint(Message);
  //DebugLn('[TCustomForm.WMPaint] END ',Name,':',ClassName);
end;


{------------------------------------------------------------------------------
  Method: TCustomForm.WMSize
  Params:   Msg: The Size message
  Returns:  nothing

  Resize event handler.
 ------------------------------------------------------------------------------}
procedure TCustomForm.WMSize(var Message: TLMSize);
var
  OldState: TWindowState;
begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TCustomForm.WMSize] ',DbgSName(Self),' Message.SizeType=',dbgs(Message.SizeType),' Message.Width=',dbgs(Message.Width),' Message.Height=',dbgs(Message.Height));
  {$ENDIF}
  Assert(False, 'Trace:WMSIZE in TCustomForm');
  if not (csDesigning in ComponentState) then begin
    OldState:=FWindowState;
    Case (Message.SizeType and not SIZE_SourceIsInterface) of
      SIZENORMAL : 
        if Showing then
          FWindowState := wsNormal;
      SIZEICONIC : 
      begin
        if Showing then
          FWindowState := wsMinimized;
        if Application.MainForm = Self then
          Application.Minimize;
      end;
      SIZEFULLSCREEN: 
        if Showing then
          FWindowState := wsMaximized;
    end;
    if OldState<>FWindowState then begin
      //if Assigned(OnWindowStateChanged) then OnWindowStateChanged(Self);
    end;
  end;

  inherited WMSize(Message);

  if (Message.SizeType and not Size_SourceIsInterface) = Size_Restored then
  begin
    FRestoredLeft := Left;
    FRestoredTop := Top;
    FRestoredWidth := Width;
    FRestoredHeight := Height;
    //DebugLn('[TCustomForm.WMSize] saving restored bounds ',DbgSName(Self),' ',dbgs(FRestoredWidth),'x',dbgs(FRestoredHeight));
  end;
End;

procedure TCustomForm.AddHandler(HandlerType: TFormHandlerType;
  const Handler: TMethod; AsLast: Boolean);
begin
  if Handler.Code=nil then RaiseGDBException('TCustomForm.AddHandler');
  if FFormHandlers[HandlerType]=nil then
    FFormHandlers[HandlerType]:=TMethodList.Create;
  FFormHandlers[HandlerType].Add(Handler,AsLast);
end;

procedure TCustomForm.RemoveHandler(HandlerType: TFormHandlerType;
  const Handler: TMethod);
begin
  FFormHandlers[HandlerType].Remove(Handler);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DefocusControl
  Params:   Control: the control which is to be defocused
            Removing: is it to be defocused because it is being removed?
  Returns:  nothing

  Updates ActiveControl if it is to be defocused
 ------------------------------------------------------------------------------}
procedure TCustomForm.DefocusControl(Control: TWinControl; Removing: Boolean);
begin
  if Control.ContainsControl(FActiveControl) then begin
    {$IFDEF VerboseFocus}
    debugln('TCustomForm.DefocusControl Control=',DbgSName(Control),' FActiveControl=',DbgSName(FActiveControl));
    {$ENDIF}
    ActiveControl := nil;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoCreate
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoCreate;
begin
  LockRealizeBounds;
  if Assigned(FOnCreate) then FOnCreate(Self);
  UnlockRealizeBounds;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoClose
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoClose(var CloseAction: TCloseAction);
var
  i: LongInt;
begin
  if Assigned(FOnClose) then FOnClose(Self, CloseAction);
  i:=FFormHandlers[fhtClose].Count;
  while FFormHandlers[fhtClose].NextDownIndex(i) do
    TCloseEvent(FFormHandlers[fhtClose][i])(Self,CloseAction);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoDestroy
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoDestroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.SetActive(AValue: Boolean);
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  //if FActiveOleControl <> nil then
  //  FActiveOleControl.Perform(CM_DOCWINDOWACTIVATE, WParam(Ord(Value)), 0);
  if FActive then
  begin
    if (ActiveControl = nil) and not (csDesigning in ComponentState) then
      ActiveControl := FindNextControl(nil, True, True, False);
    //MergeMenu(True);
    SetWindowFocus;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoHide
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.DoShow
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.EndFormUpdate;
 ------------------------------------------------------------------------------}
procedure TCustomForm.EndFormUpdate;
begin
  dec(FFormUpdateCount);
  if FFormUpdateCount=0 then begin
    Visible:=(fsVisible in FFormState);
  end;
end;

{------------------------------------------------------------------------------
  function TCustomForm.FormUpdating: boolean;
 ------------------------------------------------------------------------------}
function TCustomForm.FormUpdating: boolean;
begin
  Result:=FFormUpdateCount>0;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.GetChildren
  Params:   Proc - see fcl/inc/writer.inc
            Root
  Returns:  nothing

  Adds component to children list which have no parent.
    (TWinControl only lists components with parents)
 ------------------------------------------------------------------------------}
procedure TCustomForm.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do begin
      OwnedComponent := Components[I];
      if OwnedComponent.HasParent = False
        then Proc(OwnedComponent);
    end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.PaintWindow
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
Procedure TCustomForm.PaintWindow(DC : Hdc);
begin
  // FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    //DebugLn('[TCustomForm.PaintWindow] ',ClassName,' DC=',DbgS(DC,8),'  ',DbgS(FCanvas.Handle,8));
    try
      if FDesigner <> nil then FDesigner.PaintGrid else Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    // FCanvas.Unlock;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomForm.RequestAlign
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
Procedure TCustomForm.RequestAlign;
Begin
  if Parent = nil then begin
    //Screen.AlignForm(Self);
  end
  else
  inherited RequestAlign;
end;

{------------------------------------------------------------------------------
       TCustomForm SetDesigner
------------------------------------------------------------------------------}
Procedure TCustomForm.SetDesigner(Value : TIDesigner);
Begin
  FDesigner := Value;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.SetZOrder(Topmost: Boolean);
------------------------------------------------------------------------------}
procedure TCustomForm.SetZOrder(Topmost: Boolean);
begin
  if Parent=nil then begin
    if TopMost and HandleAllocated then begin
      //TODO: call TWSCustomFormClass(Widgetset).SetZORder.
      BringWindowToTop(Handle);
    end;
    exit;
  end;
  inherited SetZOrder(Topmost);
end;

procedure TCustomForm.SetParent(NewParent: TWinControl);
begin
  if Parent=NewParent then exit;
  if HandleAllocated then DestroyHandle;
  inherited SetParent(NewParent);
  if (Parent=nil) and Visible then
    HandleNeeded;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.VisibleChanging;
------------------------------------------------------------------------------}
procedure TCustomForm.VisibleChanging;
begin
  //if (FormStyle = fsMDIChild) and Visible then
  //  raise EInvalidOperation.Create(SMDIChildNotVisible);
end;

{------------------------------------------------------------------------------
       TCustomForm WndProc
------------------------------------------------------------------------------}
procedure TCustomForm.WndProc(Var TheMessage : TLMessage);
var
  FocusHandle : HWND;
  MenuItem : TMenuItem;
begin
  with TheMessage do
  case Msg of
    LM_ACTIVATE, LM_SETFOCUS, LM_KILLFOCUS:
      begin
        if not FocusMessages then Exit;
        if (Msg = LM_SetFocus) and not (csDesigning in ComponentState)
        then begin
          FocusHandle := 0;
          
          if FormStyle = fsMDIFORM
          then begin
            // ToDo
          end
          else begin
            if (FActiveControl <> nil) and (FActiveControl <> Self)
            and FActiveControl.Visible and FActiveControl.Enabled
            and ([csLoading,csDestroying]*ComponentState=[])
            and not FActiveControl.ParentDestroyingHandle
            then
              // get or create handle of FActiveControl
              FocusHandle := FActiveControl.Handle;
          end;
          
          TheMessage.Result:=0;
          if FocusHandle <> 0
          then begin
            {$IFDEF VerboseFocus}
            DebugLn('[TCustomForm.WndProc] ',Name,':',ClassName);
            {$ENDIF}
            LCLIntf.SetFocus(FocusHandle);
            Exit;
          end;
        end;
      end;
    CM_EXIT:
      begin
        //TODO: deal with docking                if HostDockSite <> nil then DeActivate;
      end;
    CM_ENTER:
      begin
        //TODO: Deal with docking                if HostDockSite <> nil then Activate;
      end;
    LM_WINDOWPOSCHANGING:
      if ([csLoading, csDesigning] * ComponentState = [csLoading])
      then begin
        if  (Position in [poDefault, poDefaultPosOnly])
        and (WindowState <> wsMaximized)
        then
          with PWindowPos(TheMessage.lParam)^ do
            flags := flags or SWP_NOMOVE;

        if  (Position in [poDefault, poDefaultSizeOnly])
        and (BorderStyle in [bsSizeable, bsSizeToolWin])
        then
          with PWindowPos(TheMessage.lParam)^ do
            flags := flags or SWP_NOSIZE;
      end;
    LM_DRAWITEM:
      with PDrawItemStruct(TheMessage.LParam)^ do
      begin
        if (CtlType = ODT_MENU) and Assigned(Menu)
        then begin
          MenuItem := Menu.FindItem(itemID, fkCommand);
          if MenuItem <> nil
          then begin
            Exit;
          end;
        end;
      end;

    end;
  inherited WndProc(TheMessage);
end;

function TCustomForm.VisibleIsStored: boolean;
begin
  Result:=Visible;
end;

function TCustomForm.ColorIsStored: boolean;
begin
  Result:=(Color<>clBtnFace);
end;

procedure TCustomForm.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  if WindowState=wsNormal then begin
    FRestoredLeft:=Left;
    FRestoredTop:=Top;
    FRestoredWidth:=Width;
    FRestoredHeight:=Height;
  end;
end;

procedure TCustomForm.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  if (NewDockSite<>HostDockSite) then begin

  end;
  inherited DoDock(NewDockSite, ARect);
end;

function TCustomForm.GetFloating: Boolean;
begin
  Result := (HostDockSite = nil) and (FloatingDockSiteClass = ClassType);
end;

procedure TCustomForm.CMActionExecute(var Message: TLMessage);
begin
  if DoExecuteAction(TBasicAction(Message.LParam)) then
    Message.Result := 1;
end;

procedure TCustomForm.CMActionUpdate(var Message: TLMessage);
begin
  if DoUpdateAction(TBasicAction(Message.LParam)) then
    Message.Result := 1;
end;

function TCustomForm.DoExecuteAction(ExeAction: TBasicAction): boolean;

  function ProcessExecute(Control: TControl): Boolean;
  begin
    Result := (Control <> nil) and
      Control.ExecuteAction(ExeAction);
  end;

  function TraverseClients(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible and ProcessExecute(Control)
        or (Control is TWinControl) and TraverseClients(TWinControl(Control))
        then begin
          Result := True;
          exit;
        end;
      end;
    Result := False;
  end;

begin
  Result := false;
  if (csDesigning in ComponentState) or (not Showing) then exit;
  if ProcessExecute(ActiveControl) or ProcessExecute(Self)
  or TraverseClients(Self) then
    Result := true;
end;

function TCustomForm.DoUpdateAction(TheAction: TBasicAction): boolean;

  function ProcessUpdate(Control: TControl): Boolean;
  begin
    Result := (Control <> nil) and
      Control.UpdateAction(TheAction);
  end;

  function TraverseClients(Container: TWinControl): Boolean;
  var
    I: Integer;
    Control: TControl;
  begin
    if Container.Showing then
      for I := 0 to Container.ControlCount - 1 do
      begin
        Control := Container.Controls[I];
        if Control.Visible and ProcessUpdate(Control)
        or (Control is TWinControl) and TraverseClients(TWinControl(Control))
        then begin
          Result := True;
          exit;
        end;
      end;
    Result := False;
  end;

begin
  Result:=false;
  if (csDesigning in ComponentState) or not Showing then exit;
  // Find a target for given Command (Message.LParam).
  if ProcessUpdate(ActiveControl) or ProcessUpdate(Self)
  or TraverseClients(Self) then
    Result:=true;
end;

procedure TCustomForm.UpdateActions;

  procedure RecursiveInitiate(Container: TWinControl);
  var
    i: Integer;
    CurControl: TControl;
  begin
    if not Container.Showing then exit;
    for i := 0 to Container.ControlCount - 1 do begin
      CurControl := Container.Controls[i];
      if (csActionClient in CurControl.ControlStyle)
      and CurControl.Visible then
        CurControl.InitiateAction;
      if CurControl is TWinControl then
        RecursiveInitiate(TWinControl(CurControl));
    end;
  end;

var
  I: Integer;
begin
  if (csDesigning in ComponentState) or (not Showing) then exit;
  // update this form
  InitiateAction;
  // update main menu's top-most items
  if Menu <> nil then
    for I := 0 to Menu.Items.Count - 1 do
      with Menu.Items[I] do
        if Visible then InitiateAction;
  // update all controls
  RecursiveInitiate(Self);
end;

{------------------------------------------------------------------------------
       TCustomForm SetMenu
------------------------------------------------------------------------------}
Procedure TCustomForm.SetMenu(Value : TMainMenu);
var
  I: Integer;
begin
  if FMenu=Value then exit;
  if Value <> nil then
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I].Menu = Value) and (Screen.Forms[I] <> Self) then
        raise EInvalidOperation.CreateFmt(sDuplicateMenus, [Value.Name]);

  if FMenu<>nil then FMenu.Parent:=nil;

  if (csDestroying in ComponentState) or
    ((Value <> nil) and (csDestroying in Value.ComponentState))
  then
    Value := nil;

  FMenu := Value;
  if FMenu<>nil then begin
    FMenu.Parent:=Self;
    if HandleAllocated then
    begin
      FMenu.HandleNeeded;
      InterfaceObject.AttachMenuToWindow(FMenu);
    end;
  end;
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetBorderIcons                                             }
{------------------------------------------------------------------------------}
procedure TCustomForm.SetBorderIcons(NewIcons: TBorderIcons);
begin
  if FBorderIcons = NewIcons then exit;
  FBorderIcons := NewIcons;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetBorderIcons(Self, NewIcons);
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetFormBorderStyle                                         }
{------------------------------------------------------------------------------}
procedure TCustomForm.SetFormBorderStyle(NewStyle: TFormBorderStyle);
begin
  if FFormBorderStyle = NewStyle then exit;
  //TODO: Finish SETBORDERSTYLE
  FFormBorderStyle := NewStyle;
  Include(FFormState,fsBorderStyleChanged);
  // ToDo: implement it.
  // We can not use inherited SetBorderStyle(NewStyle),
  // because TBorderSTyle <> TFormBorderSTyle;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetFormBorderStyle(Self, NewStyle);
end;

{------------------------------------------------------------------------------}
{       TCustomForm UpdateWindowState                                          }
{------------------------------------------------------------------------------}
Procedure TCustomForm.UpdateWindowState;
Begin
//TODO: Finish UpdateWindowState
  Assert(False, 'Trace:TODO: [TCustomForm.UpdateWindowState]');
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetWindowState                                             }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetWindowState(Value : TWindowState);
const
  ShowCommands: array[TWindowState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
begin
  if FWindowState <> Value then
  begin
    FWindowState := Value;
    if not (csDesigning in ComponentState) and Showing then
      ShowWindow(Handle, ShowCommands[Value]);
  end;
end;

procedure TCustomForm.SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  prevWindowState: TWindowState;
begin
  // temporarily go to normal window state to store restored bounds
  prevWindowState := WindowState;
  WindowState := wsNormal;
  SetBounds(ALeft, ATop, AWidth, AHeight);
  WindowState := prevWindowState;
end;

{------------------------------------------------------------------------------}
{       TCustomForm SetActiveControl                                                      }
{------------------------------------------------------------------------------}
Procedure TCustomForm.SetActiveControl(AWinControl: TWinControl);
Begin
  if FActiveControl <> AWinControl then
  begin
    if not
      ((AWinControl = nil)
      or (AWinControl <> Self) and (GetParentForm(AWinControl) = Self)
        and ((csLoading in ComponentState) or not (Visible and Enabled)
          or AWinControl.CanFocus))
    then
      RaiseGDBException(SCannotFocus);
      // EInvalidOperation.Create(SCannotFocus);
    {$IFDEF VerboseFocus}
    DbgOut('TCustomForm.SetActiveControl ',Name,':',ClassName,' FActive=',DbgS(FActive));
    if FActiveControl<>nil then
      DebugLn(' OldActiveControl=',DbgSName(FActiveControl))
    else
      DebugLn(' OldActiveControl=nil');
    if AWinControl<>nil then
      DebugLn(' NewActiveControl=',DbgSName(AWinControl))
    else
      DebugLn(' NewActiveControl=nil');
    {$ENDIF}
    FActiveControl := AWinControl;
    if ([csLoading,csDestroying]*ComponentState=[]) then begin
      if FActive then SetWindowFocus;
      ActiveChanged;
    end;
  end;
end;

procedure TCustomForm.SetActiveDefaultControl(AControl: TControl);
var
  lPrevControl: TControl;
begin
  if AControl = FActiveDefaultControl then exit;
  lPrevControl := FActiveDefaultControl;
  FActiveDefaultControl := AControl;

  if ((lPrevControl = nil) <> (AControl = nil))
    and (FDefaultControl <> nil) then
    FDefaultControl.ActiveDefaultControlChanged(AControl);
end;

{------------------------------------------------------------------------------
       TCustomForm SetFormStyle
------------------------------------------------------------------------------}
Procedure TCustomForm.SetFormStyle(Value : TFormStyle);
var
  OldFormStyle: TFormStyle;
Begin
  if FFormStyle = Value then exit;
  if (Value in [fsMDIChild, fsMDIForm]) then
    raise Exception.Create('TCustomForm.SetFormStyle MDI forms are not implemented yet');
  OldFormStyle:=FFormStyle;
  FFormStyle := Value;
  Include(FFormState,fsFormStyleChanged);
  if FFormStyle=fsSplash then begin
    BorderStyle:=bsNone;
  end else if OldFormStyle=fsSplash then begin
    BorderStyle:=bsSizeable;
  end;
end;

{------------------------------------------------------------------------------
       TCustomForm SetPosition
------------------------------------------------------------------------------}
procedure TCustomForm.SetPosition(Value : TPosition);
begin
  if Value <> FPosition then begin
    FPosition := Value;
    UpdateControlState;
  end;
end;

procedure TCustomForm.SetShowInTaskbar(Value: Boolean);
begin
  if Value = FShowInTaskbar then exit;
  FShowInTaskbar := Value;
  TWSCustomFormClass(WidgetSetClass).SetShowInTaskbar(Self, Value);
end;

{------------------------------------------------------------------------------
       TCustomForm Constructor
------------------------------------------------------------------------------}
constructor TCustomForm.Create(AOwner : TComponent);
begin
  //DebugLn('[TCustomForm.Create] A Class=',Classname);
  FShowInTaskbar := GetShowInTaskBarDefault;
  BeginFormUpdate;
  try
    CreateNew(AOwner, 1);
    //DebugLn('[TCustomForm.Create] B Class=',Classname);
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        //DebugLn('[TCustomForm.Create] C Class=',Classname);
        if not InitResourceComponent(Self, TForm) then begin
          //DebugLn('[TCustomForm.Create] Resource '''+ClassName+''' not found');
          //DebugLn('This is for information purposes only.   This is not critical at this time.');
          // MG:  Ignoring is best at the moment. (Delphi raises an exception.)
        end;
        //DebugLn('[TCustomForm.Create] D Class=',Classname);
        DoCreate;
        //DebugLn('[TCustomForm.Create] E Class=',Classname);
      finally
        Exclude(FFormState, fsCreating);
      end;
    end;
  finally
    EndFormUpdate;
  end;
  //DebugLn('[TCustomForm.Create] END Class=',Classname);
end;

{------------------------------------------------------------------------------
  constructor TCustomForm.CreateNew(AOwner: TComponent; Num : Integer);
------------------------------------------------------------------------------}
constructor TCustomForm.CreateNew(AOwner: TComponent; Num : Integer);
Begin
  Include(FFormState,fsFirstShow);
  //DebugLn('[TCustomForm.CreateNew] Class=',Classname);
  BeginFormUpdate;
  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  // set border style before handle is allocated
  if not (fsBorderStyleChanged in FFormState) then
    FFormBorderStyle:= bsSizeable;
  // set form style before handle is allocated
  if not (fsFormStyleChanged in FFormState) then
    FFormStyle:= fsNormal;

  inherited Create(AOwner);
  Visible := False;
  fCompStyle:= csForm;

  FMenu	:= nil;

  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
                                  csClickEvents, csSetCaption, csDoubleClicks];
  SetInitialBounds(0,0,320,240);
  ParentColor := False;
  ParentFont := False;
  Ctl3D := True;
  //  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FWindowState := wsNormal;
  //  FDefaultMonitor := dmActiveForm;
  FIcon := TIcon.Create;
  //  FInCMParentBiDiModeChanged := False;
  FKeyPreview :=  False;
  Color := clBtnFace;
  //  FPixelsPerInch := Screen.PixelsPerInch;
  //  FPrintScale := poProportional;
  FloatingDockSiteClass := TWinControlClass(ClassType);
  Screen.AddForm(Self);
  EndFormUpdate;
End;

{------------------------------------------------------------------------------
  TCustomForm CreateParams
------------------------------------------------------------------------------}
procedure TCustomForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if (Parent = nil) {and (ParentWindow = 0)} then
    begin
//    WndParent := Application.Handle;
      { TODO : No application handle }
      Style := Style and not Cardinal(WS_GROUP or WS_TABSTOP);
      if Parent=nil then
        Style := Style and not Cardinal(WS_CHILD);
    end;
  end;
end;


{------------------------------------------------------------------------------}
{       TCustomForm Method Close                                               }
{------------------------------------------------------------------------------}
Procedure TCustomForm.Close;
var
  CloseAction: TCloseAction;
begin
  if fsModal in FFormState then
    ModalResult := mrCancel
  else begin
    if CloseQuery then
    begin
      if FormStyle = fsMDIChild then begin
        //if biMinimize in BorderIcons then
        //  CloseAction := caMinimize
        //else
          CloseAction := caNone;
      end else begin
        CloseAction := caHide;
      end;
      DoClose(CloseAction);
      if CloseAction <> caNone then begin
        if Application.MainForm = Self then Application.Terminate
        else if CloseAction = caHide then Hide
        else if CloseAction = caMinimize then WindowState := wsMinimized
        else Release;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.Release;
------------------------------------------------------------------------------}
procedure TCustomForm.Release;
begin
  if Application<>nil then
    Application.ReleaseComponent(Self)
  else
    Free;
end;

{------------------------------------------------------------------------------
       TCustomForm Method CloseQuery
------------------------------------------------------------------------------}
function TCustomForm.CloseQuery: boolean;
//var i : integer;
begin
  { Query children forms whether we can close }
  if FormStyle = fsMDIForm then begin
  {  for i:= 0 to MDIChildCount - 1 do begin
      if not MDIChildren[i].CloseQuery then begin
        Result:= false;
	Exit;
       end;
    end;}
  end;
  Result := true;
  if Assigned(FOnCloseQuery) then FOnCloseQuery(Self, Result);
end;

{------------------------------------------------------------------------------
       TCustomForm Method WMCloseQuery
------------------------------------------------------------------------------}
procedure TCustomForm.WMCloseQuery(var Message: TLMessage);
begin
  Close;
  // Always return 0, because we destroy the window ourselves
  Message.Result:= 0;
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method Hide                                                }
{------------------------------------------------------------------------------}
procedure TCustomForm.Hide;
begin
  If (fsModal in FormState) and (ModalResult=0) then
    ModalResult := mrCancel;
  Visible := False;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.Show;
------------------------------------------------------------------------------}
procedure TCustomForm.Show;
begin
  Visible:=true;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.ShowOnTop;
------------------------------------------------------------------------------}
procedure TCustomForm.ShowOnTop;
begin
  Show;
  BringToFront;
  //DebugLn('TCustomForm.ShowOnTop ',Name,':',ClassName,' ',Visible,' ',HandleAllocated,' ',csDesigning in ComponentState);
end;

function TCustomForm.AutoSizeDelayed: boolean;
begin
  Result:=// no autosize during loading or destruction
          ([csLoading,csDestroying]*ComponentState<>[])
          // no handle means not visible
          or (not HandleAllocated)
          // during handle creation no autosize
          or (wcfCreatingChildHandles in FWinControlFlags)
          // no autosize for invisible forms
          or (not Visible)
          // if there is a parent, ask it
          or ((Parent<>nil) and Parent.AutoSizeDelayed);
end;

{------------------------------------------------------------------------------
       TCustomForm Method IsForm
------------------------------------------------------------------------------}
function TCustomForm.IsForm: Boolean;
begin
  //TODO:
  Result := True;
end;

{------------------------------------------------------------------------------
  function TCustomForm.IsHelpFileStored: boolean;
------------------------------------------------------------------------------}
function TCustomForm.IsHelpFileStored: boolean;
begin
  Result:=FHelpFile<>'';
end;

{------------------------------------------------------------------------------
  TCustomForm Method SetFocusedControl
  
  Switch focus.
------------------------------------------------------------------------------}
function TCustomForm.SetFocusedControl(Control: TWinControl): Boolean;
var
  ParentForm: TCustomForm;
begin
  Result := False;
  
  if (csDestroying in Control.ComponentState) then exit;
  
  if (Parent<>nil) then begin
    ParentForm:=GetParentForm(Self);
    if ParentForm<>nil then
      ParentForm.SetFocusedControl(Control);
    exit;
  end;

  // update FActiveControl
  if (FDesigner = nil) and (not (csLoading in ComponentState)) then
    if Control <> Self then
      FActiveControl := Control
    else
      FActiveControl := nil;

  // update Screen object
  Screen.FActiveControl := Control;
  Screen.FActiveCustomForm := Self;
  Screen.MoveFormToFocusFront(Self);
  if Self is TForm then
    Screen.FActiveForm := TForm(Self)
  else
    Screen.FActiveForm := nil;

  {$IFDEF VerboseFocus}
  DbgOut('TCustomForm.SetFocusedControl Self=',DbgSName(Self));
  DbgOut(' Control=',DbgSName(Control),' Control.HandleAllocated=',dbgs(Control.HandleAllocated));
  DebugLn();
  {$ENDIF}

  Result:=true;

  if not (csFocusing in Control.ControlState) then begin
    // prevent looping
    Control.ControlState := Control.ControlState + [csFocusing];
    try
      // change focus
      
    finally
      Control.ControlState := Control.ControlState - [csFocusing];
    end;
  end;
  
  {
  Inc(FocusCount);

  // prevent looping
  if (csFocusing in Control.ControlState) then exit;
  Control.ControlState := Control.ControlState + [csFocusing];
  try

    if Screen.FFocusedForm <> Self then
    begin
      if Screen.FFocusedForm <> nil then
      begin
        FocusHandle := Screen.FFocusedForm.Handle;
        Screen.FFocusedForm := nil;
        if not SendFocusMessage(FocusHandle, CM_DEACTIVATE) then Exit;
      end;
      Screen.FFocusedForm := Self;
      if not SendFocusMessage(Handle, CM_ACTIVATE) then Exit;
    end;
    if FFocusedWinControl = nil then FFocusedWinControl := Self;
    if FFocusedWinControl <> Control then
    begin
      while (FFocusedWinControl <> nil) and not
        FFocusedWinControl.ContainsControl(Control) do
      begin
        FocusHandle := FFocusedWinControl.Handle;
        FFocusedWinControl := FFocusedWinControl.Parent;
        if not SendFocusMessage(FocusHandle, CM_EXIT) then Exit;
      end;
      while FFocusedControl <> Control do
      begin
        TempControl := Control;
        while TempControl.Parent <> FFocusedControl do
          TempControl := TempControl.Parent;
        FFocusedControl := TempControl;
        if not SendFocusMessage(TempControl.Handle, CM_ENTER) then Exit;
      end;
      TempControl := Control.Parent;
      while TempControl <> nil do
      begin
        if TempControl is TScrollingWinControl then
          TScrollingWinControl(TempControl).AutoScrollInView(Control);
        TempControl := TempControl.Parent;
      end;
      Perform(CM_FOCUSCHANGED, 0, LParam(Control));
      if (FActiveOleControl <> nil) and (FActiveOleControl <> Control) then
        FActiveOleControl.Perform(CM_UIDEACTIVATE, 0, 0);
    end;
  finally
    Control.ControlState := Control.ControlState - [csFocusing];
  end;
  Screen.UpdateLastActive;
  Result := True;
  }
end;

{------------------------------------------------------------------------------}
{       TCustomForm Method WantChildKey            }
{------------------------------------------------------------------------------}
function TCustomForm.WantChildKey(Child : TControl;
  var Message : TLMessage):Boolean;
begin
  Result := False;
end;

function TCustomForm.IsShortCut(var Message: TLMKey): boolean;
var
  I: integer;
begin
  Result := false;
  if Assigned(FOnShortcut) then
    FOnShortcut(Message, Result);
  if Result then 
    exit;
  if Assigned(FMenu) then
  begin
    Result := FMenu.IsShortCut(Message);
    if Result then exit;
  end;
  if Assigned(FActionLists) then
  begin
    for I := 0 to FActionLists.Count - 1 do
    begin
      Result := TCustomActionList(FActionLists.Items[I]).IsShortCut(Message);
      if Result then exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomForm.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomForm.CreateWnd;
var
  DC: HDC;
  ParentForm: TCustomForm;
begin
  //DebugLn('TCustomForm.CreateWnd START ',ClassName);
  FFormState:=FFormState-[fsBorderStyleChanged,fsFormStyleChanged];
  inherited CreateWnd;
  
  if Parent=nil then begin
    TWSCustomFormClass(WidgetSetClass).SetIcon(Self, GetIconHandle);
    DC:=GetDC(Handle);
    FPixelsPerInch:=GetDeviceCaps(DC,LOGPIXELSX);
    ReleaseDC(Handle,DC);
  end else begin
    ParentForm:=GetParentForm(Self);
    if ParentForm<>nil then begin
      FPixelsPerInch:=ParentForm.PixelsPerInch;
    end;
  end;

  Assert(False, 'Trace:[TCustomForm.CreateWnd] FMenu.HandleNeeded');
  if FMenu <> nil then
  begin
    FMenu.HandleNeeded;
    InterfaceObject.AttachMenuToWindow(FMenu);
  end;

  // activate focus if visible
  if Visible then begin
    if (FActiveControl<>nil) and FActiveControl.HandleAllocated
    and FActiveControl.Visible and FActiveControl.Enabled
    and ([csLoading,csDestroying]*ComponentState=[]) then begin
      {$IFDEF VerboseFocus}
      DebugLn('TCustomForm.CreateWnd A ',FActiveControl.Name,':',FActiveControl.ClassName);
      {$ENDIF}
      LCLIntf.SetFocus(FActiveControl.Handle);
    end;
  end;
  //DebugLn('TCustomForm.CreateWnd END ',ClassName);
end;

procedure TCustomForm.DestroyWnd;
begin
  if FMenu <> nil then
  begin
    FMenu.DestroyHandle;
  end;
  inherited DestroyWnd;
end;

procedure TCustomForm.Loaded;
var
  Control: TWinControl;
begin
  inherited Loaded;
  if FMenu<>nil then
    FMenu.HandleNeeded;
  if ActiveControl <> nil then
  begin
    Control := ActiveControl;
    {$IFDEF VerboseFocus}
    Debugln('TCustomForm.Loaded Self=',DbgSName(Self),' FActiveControl=',DbgSName(FActiveControl));
    {$ENDIF}
    FActiveControl := nil;
    if Control.CanFocus then SetActiveControl(Control);
  end;
  //DebugLn('TCustomForm.Loaded ',Name,':',ClassName,' ',FormUpdating,' ',fsCreating in FFormState,' ',Visible,' ',fsVisible in FormState);
  if fsVisible in FormState then
    Visible:=true;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.BeginFormUpdate;
  
  Called after all childs handles are created.
 ------------------------------------------------------------------------------}
procedure TCustomForm.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if Parent=nil then
    ParentFormHandleInitialized;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.BeginFormUpdate;
 ------------------------------------------------------------------------------}
procedure TCustomForm.BeginFormUpdate;
begin
  inc(FFormUpdateCount);
end;

{------------------------------------------------------------------------------
  Method:  TCustomForm.UpdateShowing
  Params:  None
  Returns: Nothing

  Here the initial form left and top are determined.
 ------------------------------------------------------------------------------}
procedure TCustomForm.UpdateShowing;
var
  X, Y : integer;
begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TCustomForm.UpdateShowing] A  Class=',Name,':',Classname,' Pos=',DbgS(Left),',',DbgS(Top),' Visible=',DbgS(Visible));
  {$ENDIF}
  { If the the form is about to show, calculate its metrics }
  if Visible then begin
    if Parent=nil then begin
      // first make sure X and Y are assigned
      X := Left;
      Y := Top;

      if  (Position = poMainFormCenter)
      and (FormStyle = fsMDIChild)
      and (Self <> Application.Mainform)
      then begin
        X:= (Application.Mainform.ClientWidth - Width) div 2;
        Y:= (Application.Mainform.ClientHeight - Height) div 2;
      end
      else begin
        case Position of
        //TODO:poDefault, poDefaultPosOnly, poDefaultSizeOnly
        poScreenCenter, poDesktopCenter :
          begin
            X:= (Screen.Width - Width) div 2;
            Y:= (Screen.Height - Height) div 2;
          end;
        poMainFormCenter :
          if (Self <> Application.MainForm) then begin
            X:= ((Application.MainForm.Width - Width) div 2) + Application.MainForm.Left;
            Y:= ((Application.MainForm.Height - Height) div 2) + Application.MainForm.Top;
          end;
        poOwnerFormCenter :
          if (Owner is TCustomForm) then begin
            X:= ((TCustomForm(Owner).Width - Width) div 2) + TCustomForm(Owner).Left;
            Y:= ((TCustomForm(Owner).Height - Height) div 2) + TCustomForm(Owner).Top;
          end;
        end;
      end;
      if X < 0 then X := 0;
      if Y < 0 then Y := 0;
      SetBounds(X, Y, Width, Height);
    end;
    
    if (fsFirstShow in FFormState) then begin
      Exclude(FFormState,fsFirstShow);
      DoFirstShow;
    end;
  end;
  {$IFDEF CHECK_POSITION}
  DebugLn('[TCustomForm.UpdateShowing] B  ',Name,':',Classname,' Pos=',DbgS(Left),',',DbgS(Top));
  {$ENDIF}
  inherited UpdateShowing;
  {$IFDEF CHECK_POSITION}
  DebugLn('[TCustomForm.UpdateShowing] END  ',Name,':',Classname,' Pos=',DbgS(Left),',',DbgS(Top));
  {$ENDIF}
end;

procedure TCustomForm.DoFirstShow;
var
  i: LongInt;
begin
  i:=FFormHandlers[fhtFirstShow].Count;
  while FFormHandlers[fhtFirstShow].NextDownIndex(i) do
    TNotifyEvent(FFormHandlers[fhtFirstShow][i])(Self);
end;

{------------------------------------------------------------------------------}
{  TCustomForm ShowModal                                                       }
{------------------------------------------------------------------------------}
Function TCustomForm.ShowModal: Integer;

  procedure RaiseShowModalImpossible;
  begin
    DebugLn('TCustomForm.ShowModal Visible=',dbgs(Visible),' Enabled=',dbgs(Enabled),
      ' fsModal=',dbgs(fsModal in FFormState),' MDIChild=',dbgs(FormStyle = fsMDIChild));
    raise EInvalidOperation.Create('TCustomForm.ShowModal impossible ');
  end;

var
  //WindowList: Pointer;
  SaveFocusCount: Integer;
  //SaveCursor: TCursor;
  //SaveCount: Integer;
  ActiveWindow: HWnd;
begin
  if Self=nil then raise
    EInvalidOperation.Create('TCustomForm.ShowModal Self=nil');
  if Application.Terminated then
    ModalResult:=0;
  // cancel drags
  CancelDrag;
  // close popupmenus
  if ActivePopupMenu<>nil then ActivePopupMenu.Close;
  //DebugLn('[TCustomForm.ShowModal] START ',Classname);
  if Visible or not Enabled or (fsModal in FFormState)
  or (FormStyle = fsMDIChild) then
    RaiseShowModalImpossible;
  // Kill capture when opening another dialog
  if GetCapture <> 0 then SendMessage(GetCapture,LM_CANCELMODE,0,0);
  ReleaseCapture;

  Include(FFormState, fsModal);
  ActiveWindow := GetActiveWindow;
  SaveFocusCount := FocusCount;
  Screen.FSaveFocusedList.Insert(0, Screen.FFocusedForm);
  Screen.FFocusedForm := Self;
  Screen.MoveFormToFocusFront(Self);
  Screen.MoveFormToZFront(Self);
  //SaveCursor := Screen.Cursor;
  //Screen.Cursor := crDefault;
  //SaveCount := Screen.FCursorCount;
  //WindowList := DisableTaskWindows(0);
  ModalResult := 0;

  try
    Show;
    try
      TWSCustomFormClass(WidgetSetClass).ShowModal(Self);
      repeat
        { Delphi calls Application.HandleMessage
          But HandleMessage processes all pending events and then calls idle,
          which will wait for new messages. Under Win32 there is always a next
          message, so it works there. The LCL is OS independent, and so it uses
          a better way: }
        InterfaceObject.HandleEvents; // process all events
        if Application.Terminated then
          ModalResult := mrCancel;
        if ModalResult <> 0 then begin
          CloseModal;
          if ModalResult<>0 then break;
        end;
        Application.Idle;
      until false;

      Result := ModalResult;
      if HandleAllocated and (GetActiveWindow <> Handle) then
        ActiveWindow := 0;
    finally
      Hide;
      // free handles to save resources and to reduce overhead in the interfaces
      // for bookkeeping changing between Show and ShowModal.
      // (e.g.: the gtk interface creates some specials on ShowModal, so the
      //  combination ShowModal, Close, Show makes problems.)
      DestroyHandle;
    end;
  finally
    {if Screen.FCursorCount = SaveCount then
      Screen.Cursor := SaveCursor
    else
      Screen.Cursor := crDefault;
    EnableTaskWindows(WindowList);}
    if Screen.FSaveFocusedList.Count > 0 then
    begin
      Screen.FFocusedForm := TCustomForm(Screen.FSaveFocusedList.First);
      Screen.FSaveFocusedList.Remove(Screen.FFocusedForm);
    end else
      Screen.FFocusedForm := nil;
    if ActiveWindow <> 0 then SetActiveWindow(ActiveWindow);
    FocusCount := SaveFocusCount;
    Exclude(FFormState, fsModal);
  end;
end;

function TCustomForm.GetRolesForControl(AControl: TControl
  ): TControlRolesForForm;
begin
  Result:=[];
  if DefaultControl=AControl then Include(Result,crffDefault);
  if CancelControl=AControl then Include(Result,crffCancel);
end;

procedure TCustomForm.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TFormHandlerType;
begin
  inherited RemoveAllHandlersOfObject(AnObject);
  for HandlerType:=Low(TFormHandlerType) to High(TFormHandlerType) do
    FFormHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TCustomForm.AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;
  AsLast: Boolean);
begin
  AddHandler(fhtFirstShow,TMethod(OnFirstShowHandler),AsLast);
end;

procedure TCustomForm.RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);
begin
  RemoveHandler(fhtFirstShow,TMethod(OnFirstShowHandler));
end;

procedure TCustomForm.AddHandlerClose(OnCloseHandler: TCloseEvent;
  AsLast: Boolean);
begin
  AddHandler(fhtClose,TMethod(OnCloseHandler),AsLast);
end;

procedure TCustomForm.RemoveHandlerClose(OnCloseHandler: TCloseEvent);
begin
  RemoveHandler(fhtClose,TMethod(OnCloseHandler));
end;

procedure TCustomForm.Dock(NewDockSite: TWinControl; ARect: TRect);
begin
  inherited Dock(NewDockSite, ARect);
end;

//==============================================================================
{$IFNDEF VER1_0}

{ TFormPropertyStorage }

procedure TFormPropertyStorage.FormFirstShow(Sender: TObject);
begin
  if Sender=nil then ;
  Restore;
end;

procedure TFormPropertyStorage.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Sender=nil then ;
  Save;
end;

constructor TFormPropertyStorage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if Owner is TCustomForm then begin
    TCustomForm(Owner).AddHandlerFirstShow(@FormFirstShow,true);
    TCustomForm(Owner).AddHandlerClose(@FormClose,true);
  end;
end;

destructor TFormPropertyStorage.Destroy;
begin
  if Owner is TControl then
    TControl(Owner).RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;
{$ENDIF not VER1_0}


// included by forms.pp

{ =============================================================================

  $Log$
  Revision 1.188  2005/07/16 00:08:26  marc
  * Reimplemented ZOrder
  + Added IDE option to move a control one forward/back
  * Fixed IDE control selection
  - Removed some IFDEF VER 1_0
  + Added some inline

  Revision 1.187  2005/06/30 19:39:04  mattias
  added AutoSizeDelayed check for controls without form parents

  Revision 1.186  2005/06/02 20:31:19  micha
  fix lm_showwindow handling to not fire onshow event in case of restore and unzoom (fixes bug 928)

  Revision 1.185  2005/06/02 12:11:54  micha
  need to recreate form window when changing border flags
  implement lock-flag when destroying handle, do not focus control that is destroying handles

  Revision 1.184  2005/05/03 14:56:33  vincents
  fixed getting the size of a minimized window (bug 886)

  Revision 1.183  2005/04/17 18:41:15  micha
  implement active default control switching
  pressing return key executes active default control action

  Revision 1.182  2005/03/26 14:01:18  micha
  save BorderIcons setting internally

  Revision 1.181  2005/03/25 08:58:11  micha
  implement ShowInTaskBar for win32 intf

  Revision 1.180  2005/03/19 23:36:26  mattias
  implemented TCustomForm.ShowInTaskBar for gtk1+2  from Andrew Haines

  Revision 1.179  2005/03/07 21:59:44  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.178  2005/03/04 13:50:08  mattias
  fixed Arc and changed x,y to Left,Top to make meaning more clear

  Revision 1.177  2005/02/05 13:33:05  mattias
  implemented gtkwidgetset.IsWindowEnabled

  Revision 1.176  2005/02/03 15:10:23  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.175  2005/01/27 10:10:25  mattias
  added TTreeNode.GetParentNodeOfAbsoluteLevel  from Sergio

  Revision 1.174  2005/01/26 23:23:11  mattias
  added error when setting FormStyle to MDI

  Revision 1.173  2005/01/25 20:07:03  mattias
  fixed updating restored form bounds for forms without handle

  Revision 1.172  2005/01/21 22:08:11  micha
  implement restored size, let lazarus ide store restored size

  Revision 1.171  2005/01/18 18:46:59  mattias
  improved invert assignment tool by multilines   from Andrew Haines

  Revision 1.170  2005/01/04 11:26:26  micha
  let canfocus imply that setfocus can be called

  Revision 1.169  2004/12/27 00:06:44  mattias
  added DestroyHandle after ShowModal to save resources

  Revision 1.168  2004/12/16 14:29:51  micha
  fix showmodal to remember disabled windows (fixes bug 478, and more)

  Revision 1.167  2004/12/05 13:25:47  mattias
  destroying clean up

  Revision 1.166  2004/12/05 13:20:29  mattias
  destroying TMenu Handle when Set Parent=nil

  Revision 1.165  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.164  2004/11/10 20:53:18  vincents
  Destroy menu handle, when destroying form handle.

  Revision 1.163  2004/11/10 18:23:56  mattias
  impementing changing a TLabel.Font properties Size, Height, Name, Style - set only at Handle creation time

  Revision 1.162  2004/11/05 22:08:53  mattias
  implemented auto sizing: child to parent sizing

  Revision 1.161  2004/10/25 14:35:13  micha
  fix bordericons initialization
  fix setting tab page caption (win32)

  Revision 1.160  2004/10/24 18:54:57  micha
  add TCustomForm.BorderIcons (delphi compat)
  tweak win32 window dialog flags, to create better dialog look

  Revision 1.159  2004/09/23 09:00:42  vincents
  fix type cast of WidgetSetClass

  Revision 1.158  2004/09/17 20:30:13  vincents
  replaced write by DbgOut

  Revision 1.157  2004/09/15 07:57:59  micha
  convert LM_SETFORMICON message to interface method

  Revision 1.156  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.155  2004/08/30 10:49:20  mattias
  fixed focus catch for combobox csDropDownList

  Revision 1.154  2004/08/27 08:55:22  micha
  implement tapplication.minimize for win32, stub for gtk

  Revision 1.153  2004/08/25 17:59:06  micha
  upon form show, if no activecontrol, then focus first control in tab order

  Revision 1.152  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.151  2004/08/18 14:24:55  mattias
  implemented TCustomForm.Release

  Revision 1.150  2004/08/16 22:09:18  mattias
  started TCustomDockForm

  Revision 1.149  2004/08/15 14:39:36  mattias
  implemented platform independent binary object streamer

  Revision 1.148  2004/08/09 21:12:43  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.147  2004/08/08 20:51:15  mattias
  replaced TDBEdit.WMKillFocus by EditingDone, Change Class basically working

  Revision 1.146  2004/07/25 01:04:45  mattias
  TXMLPropStorage basically working

  Revision 1.145  2004/07/11 13:03:54  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.144  2004/07/07 15:31:47  micha
  fix code editor restoring when maximized upon ctrl+shift+up

  Revision 1.143  2004/07/04 20:07:08  micha
  form notifies control of new role

  Revision 1.142  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TButton class in TCustomForm

  Revision 1.141  2004/06/30 11:07:20  micha
  implement return key clicks default button; escape key clicks cancel button

  Revision 1.140  2004/05/22 11:06:27  mattias
  fixed grids SetBorderStyle

  Revision 1.139  2004/05/21 18:34:44  mattias
  readded protected TWinControl.BorderStyle

  Revision 1.138  2004/05/21 18:20:11  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.137  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.136  2004/05/21 09:03:55  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.135  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.134  2004/04/23 11:18:28  mattias
  fixed unsetting csFocusing

  Revision 1.133  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.132  2004/03/30 19:08:29  mattias
  test for destroying controls, so they do not get focus

  Revision 1.131  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.130  2004/03/06 14:39:50  micha
  although loading, attach menu to form if handle already created, if this is not wanted, then prevent creation of handle somehow
  fixes bug #197

  Revision 1.129  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.128  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.126  2004/02/10 02:00:13  mattias
  activated Idle actions

  Revision 1.125  2004/02/02 17:39:10  mattias
  added TActionList - actions need testing

  Revision 1.124  2004/02/02 16:59:28  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.123  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.122  2003/12/23 16:50:45  micha
  fix defocus control when destroying it

  Revision 1.121  2003/12/18 08:50:13  micha
  attachmenutowindow cleanup

  Revision 1.120  2003/12/18 08:15:25  micha
  setmenu fix, now correctly (sigh)

  Revision 1.119  2003/12/18 08:00:37  micha
  setmenu fix, notify interface (from darek)

  Revision 1.118  2003/11/23 00:28:51  mattias
  fixed closing IDE while debugging

  Revision 1.117  2003/11/17 23:09:39  mattias
  started PixelsPerInch

  Revision 1.116  2003/10/07 14:54:59  mattias
  moved some lazarus resource code to LResources.pp

  Revision 1.115  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.114  2003/09/13 10:04:35  mattias
  fixed ColorIsStored

  Revision 1.113  2003/09/06 18:38:06  mattias
  fixed recreating handle after showmodal

  Revision 1.112  2003/08/31 17:30:49  mattias
  fixed TControl painting for win32

  Revision 1.111  2003/08/28 12:08:30  mattias
  fixed register color prop edit

  Revision 1.110  2003/08/27 09:20:44  mattias
  added TFrame definition, no implementation

  Revision 1.109  2003/08/12 21:35:11  mattias
  TApplication now descends from TCustomApplication

  Revision 1.108  2003/07/01 13:49:36  mattias
  clean up

  Revision 1.107  2003/06/30 07:00:18  mattias
  activated EraseBckGrd messages in doublebuffer WMPaint section

  Revision 1.106  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.105  2003/06/16 23:12:59  mattias
  fixed TCustomForm.ShowModal when Self=nil

  Revision 1.104  2003/06/16 22:47:19  mattias
  fixed keeping TForm.Visible=false

  Revision 1.103  2003/06/13 14:38:01  mattias
  fixed using streamed clientwith/height for child anchors

  Revision 1.102  2003/06/13 06:05:49  mattias
  started context diff

  Revision 1.101  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.100  2003/06/01 21:09:09  mattias
  implemented datamodules

  Revision 1.99  2003/05/24 08:51:41  mattias
  implemented designer close query

  Revision 1.98  2003/05/12 13:40:50  mattias
  fixed clsing popupmenu on showmodal

  Revision 1.97  2003/04/20 16:32:58  mattias
  published keypreview

  Revision 1.96  2003/04/16 22:11:35  mattias
  fixed codetools Makefile, fixed default prop not found error

  Revision 1.95  2003/04/16 17:20:24  mattias
  implemented package check broken dependency on compile

  Revision 1.94  2003/04/11 21:21:34  mattias
  implemented closing unneeded package

  Revision 1.93  2003/04/11 09:32:20  mattias
  added some help stuff

  Revision 1.92  2003/03/25 10:45:40  mattias
  reduced focus handling and improved focus setting

  Revision 1.91  2003/03/18 13:04:25  mattias
  improved focus debugging output

  Revision 1.90  2003/03/17 13:54:34  mattias
  fixed setting activecontrol after createwnd

  Revision 1.89  2003/03/13 10:11:41  mattias
  fixed TControl.Show in design mode

  Revision 1.88  2003/03/11 22:56:41  mattias
  added visiblechanging

  Revision 1.87  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.86  2003/02/28 19:54:05  mattias
  added ShowWindow

  Revision 1.85  2003/02/04 11:44:13  mattias
  fixed modified and loading xpms for button glyphs

  Revision 1.84  2003/01/06 12:00:16  mattias
  implemented fsStayOnTop+bsNone for forms under gtk (useful for splash)

  Revision 1.83  2003/01/04 12:06:53  mattias
  fixed TCustomform.BringToFront

  Revision 1.82  2002/12/29 11:10:45  mattias
  fixed form FActive, cleanups

  Revision 1.81  2002/12/28 21:06:37  mattias
  fixed TCustomForm.WMCloseQuery

  Revision 1.80  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.79  2002/12/28 11:29:47  mattias
  xmlcfg deletion, focus fixes

  Revision 1.78  2002/12/25 14:21:28  mattias
  fixed setting activecontrol to nil when removing component

  Revision 1.77  2002/12/25 11:53:47  mattias
  Button.Default now sets focus

  Revision 1.76  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.75  2002/12/03 17:40:37  mattias
  fixed deleting lookup form when form is deleted

  Revision 1.74  2002/11/30 11:24:05  mattias
  removed unused TCustomForm.WMDestroy

  Revision 1.73  2002/11/30 08:35:42  mattias
  TCustomForm.WMDestroy does not Free anymore

  Revision 1.72  2002/11/29 15:14:47  mattias
  replaced many invalidates by invalidaterect

  Revision 1.71  2002/11/12 16:18:46  lazarus
  MG fixed hidden component page

  Revision 1.70  2002/11/09 18:13:33  lazarus
  MG: fixed gdkwindow checks

  Revision 1.69  2002/11/06 17:46:36  lazarus
  MG: reduced showing forms during creation

  Revision 1.68  2002/10/28 18:17:02  lazarus
  MG: impoved focussing, unfocussing on destroy and fixed unit search

  Revision 1.67  2002/10/27 15:46:58  lazarus
  MWE:
    * Moved call to BeforeDestruction to CustomForm
    - Removed form.inc

  Revision 1.66  2002/10/27 11:51:35  lazarus
  MG: fixed memleaks

  Revision 1.65  2002/10/24 09:37:39  lazarus
  MG: broke menus.pp <-> controls.pp circle

  Revision 1.64  2002/10/23 20:47:26  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.63  2002/10/22 18:54:56  lazarus
  MG: fixed menu streaming

  Revision 1.62  2002/10/22 13:01:20  lazarus
  MG: fixed setting modalresult on hide

  Revision 1.61  2002/10/06 17:55:45  lazarus
  MG: JITForms now sets csDesigning before creation

  Revision 1.60  2002/09/29 15:08:38  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.59  2002/09/16 16:18:50  lazarus
  MG: fixed mem leak in TPixmap

  Revision 1.58  2002/09/09 14:01:05  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.57  2002/09/09 06:27:06  lazarus

  Form deactivation fixes.

  Revision 1.56  2002/09/03 20:02:01  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.55  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.54  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.53  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.52  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.51  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.50  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.49  2002/07/05 09:09:20  lazarus
  MG: fixed TCustomForm.ShowModal reacting to ModalResult

  Revision 1.48  2002/06/19 19:46:09  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.47  2002/05/30 21:17:27  lazarus
  lcl/controls.pp

  Revision 1.46  2002/05/15 05:58:17  lazarus
  MG: added TMainMenu.Parent

  Revision 1.45  2002/05/13 15:26:13  lazarus
  MG: fixed form positioning when show, hide, show

  Revision 1.44  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.43  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.42  2002/04/27 18:56:50  lazarus
  MG: started component renaming

  Revision 1.41  2002/04/27 15:35:50  lazarus
  MG: fixed window shrinking

  Revision 1.40  2002/04/26 15:31:06  lazarus
  MG: made ShowModal more dlephi compatible

  Revision 1.39  2002/04/26 12:26:50  lazarus
  MG: improved clean up

  Revision 1.38  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.37  2002/03/18 11:44:41  lazarus
  MG: TForm.Position will now considered before creating form on 0,0

  Revision 1.36  2002/03/16 21:40:55  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.35  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.34  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.33  2001/12/28 15:12:02  lazarus
  MG: LM_SIZE and LM_MOVE messages are now send directly, not queued

  Revision 1.32  2001/12/20 14:41:20  lazarus
  Fixed setfocus for TComboBox and TMemo
  Shane

  Revision 1.31  2001/12/19 10:59:12  lazarus
  MG: changes for fpc 1.1

  Revision 1.30  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.29  2001/10/16 20:01:28  lazarus
  MG: removed splashform fix, because of the unpredictable side effects

  Revision 1.28  2001/10/10 17:55:04  lazarus
  MG: fixed caret lost, gtk cleanup, bracket lvls, bookmark saving

  Revision 1.27  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.26  2001/10/03 17:34:26  lazarus
  MG: activated TCustomForm.OnCreate event

  Revision 1.24  2001/07/10 13:25:49  lazarus
  MG: repaints reduced

  Revision 1.22  2001/06/28 18:15:03  lazarus
  MG: bugfixes for destroying controls

  Revision 1.21  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.20  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.19  2001/05/31 13:57:28  lazarus
  MG: added environment option OpenLastProjectAtStart

  Revision 1.18  2001/03/31 13:35:23  lazarus
  MG: added non-visual-component code to IDE and LCL

  Revision 1.17  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.15  2001/03/19 14:41:56  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.13  2001/02/28 13:17:33  lazarus
  Added some debug code for the top,left reporting problem.
  Shane

  Revision 1.12  2001/02/06 20:59:17  lazarus
  Trying to get the last control of the last form focused when a dialog closes.
  Still working on it.
  Shane

  Revision 1.11  2001/02/02 20:13:39  lazarus
  Codecompletion changes.
  Added code to Uniteditor for code completion.

  Also, added code to gtkobject.inc so forms now get keypress events.
  Shane

  Revision 1.10  2001/02/01 16:45:20  lazarus
  Started the code completion.
  Shane

  Revision 1.9  2001/01/12 18:46:50  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.8  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.7  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.6  2000/11/30 21:43:38  lazarus
  Changed TDesigner.  It's now notified when a control is added to it's CustomForm.
  It's created in main.pp when New Form is selected.

  Shane

  Revision 1.5  2000/11/21 17:33:37  lazarus
  Added TCustomForm.Notification so the TDesigner is notified of actions.

  Added more code for getting info via RTTI
  Shane

  Revision 1.4  2000/08/14 12:31:12  lazarus
  Minor modifications for SynEdit .
  Shane

  Revision 1.3  2000/08/09 14:15:04  lazarus
  Changed the TCUstomForm create function.  I am getting it ready to read the resources to auto-create the controls...

  Anslo changes TScreen.AddForm and TScreen.RemoveForm.  They were being passed TFOrm's instead of TCustomForms.
  Shane

  Revision 1.2  2000/07/23 19:01:33  lazarus
  menus will be destroyed now, stoppok

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

}
