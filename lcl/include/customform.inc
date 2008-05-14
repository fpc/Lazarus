{%MainUnit ../forms.pp}

{******************************************************************************
                               TCustomForm
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
    { do not call widgetset CloseModal here, but in ShowModal to
      guarantee execution of it }
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
  GlobalNameSpace.BeginWrite;
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
  if not (csDestroying in ComponentState) then GlobalNameSpace.BeginWrite;
  try
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
    GlobalNameSpace.EndWrite;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.FocusControl
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomForm.FocusControl(WinControl : TWinControl);
var
  WasActive: Boolean;
begin
  WasActive := FActive;
  SetActiveControl(WinControl);
  if not WasActive then
    SetFocus;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.Notification
 ------------------------------------------------------------------------------}
procedure TCustomForm.Notification(AComponent: TComponent;
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
    // maybe active default control changed
    if FActiveDefaultControl = nil then
    begin
      if OldDefaultControl <> nil then
        OldDefaultControl.ActiveDefaultControlChanged(nil);
      if FDefaultControl <> nil then
        FDefaultControl.ActiveDefaultControlChanged(nil);
    end;
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
  if (FIcon<>nil) and (not Icon.Empty) then
    Result := FIcon.Handle
  else
    Result := Application.GetIconHandle;
end;

{------------------------------------------------------------------------------
  Method: TCustomForm.SetFocus
 ------------------------------------------------------------------------------}
procedure TCustomForm.SetFocus;

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

begin
  {$IFDEF VerboseFocus}
  DebugLn('TCustomForm.SetFocus ',Name,':',ClassName);
  {$ENDIF}
  if not FActive then
  begin
    if not (IsControlVisible and Enabled) then
      RaiseCannotFocus;
    SetWindowFocus;
  end;
end;

{------------------------------------------------------------------------------
       TCustomForm SetVisible
------------------------------------------------------------------------------}
procedure TCustomForm.SetVisible(Value : boolean);
begin
  if (Value=(fsVisible in FFormState)) and (Visible=Value) then exit;
  //DebugLn(['[TCustomForm.SetVisible] START ',Name,':',ClassName,' Old=',Visible,' New=',Value,' ',(fsCreating in FFormState)]);
  if Value then
    Include(FFormState, fsVisible)
  else
    Exclude(FFormState, fsVisible);
  //DebugLn(['TCustomForm.SetVisible ',Name,':',ClassName,' fsCreating=',fsCreating in FFormState]);
  if (fsCreating in FFormState) {or FormUpdating} then
    // will be done when finished loading
  else
  begin
    inherited SetVisible(Value);
  end;
  //DebugLn(['[TCustomForm.SetVisible] END ',Name,':',ClassName,' ',Value,' ',(fsCreating in FFormState),' ',Visible]);
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
  or (not NewFocusControl.IsVisible)
  or (not NewFocusControl.Enabled) then
    exit;
  //DebugLn(['TCustomForm.SetWindowFocus ',DbgSName(Self),' NewFocusControl',DbgSName(NewFocusControl)]);
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
var
  NewFocusControl: TWinControl;
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
        if (ActiveControl = nil) and (not (csDesigning in ComponentState))
        and (Parent=nil) then begin
          // automatically choose a control to focus
          {$IFDEF VerboseFocus}
          DebugLn('TCustomForm.WMShowWindow ',DbgSName(Self),' Set ActiveControl := ',DbgSName(FindDefaultForActiveControl));
          {$ENDIF}
          ActiveControl := FindDefaultForActiveControl;
        end;
        if ([csLoading,csDestroying]*ComponentState=[])
        and (Parent=nil) then begin
          NewFocusControl:=FActiveControl;
          if (NewFocusControl=nil)
          or (not NewFocusControl.CanFocus)
          then
            NewFocusControl:=Self;
          if NewFocusControl.HandleAllocated and NewFocusControl.IsVisible
          then begin
            {$IFDEF VerboseFocus}
            DebugLn('TCustomForm.WMShowWindow ',DbgSName(Self),' SetFocus NewFocusControl=',DbgSName(NewFocusControl),' FActiveControl=',DbgSName(FActiveControl));
            {$ENDIF}
            LCLIntf.SetFocus(NewFocusControl.Handle);
          end;
        end;
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
    SetActive(Message.Active);

  Activate;
  if Application<>nil then Application.Activate;
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
  if Application<>nil then Application.Deactivate;
  Deactivate;
end;

procedure TCustomForm.CMShowingChanged(var Message: TLMessage);
begin
  if Showing then
    DoShow
  else
    DoHide;
  inherited;
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
  Include(FFormState,fsDisableAutoSize);
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
      if Assigned(OnWindowStateChange) then
        OnWindowStateChange(Self);
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

procedure TCustomForm.CMBiDiModeChanged(var Message: TLMessage);
var
  i:Integer;
  lMessage:TLMessage;
begin
  inherited;
  //send CM_PARENTBIDIMODECHANGED to All Component owned by Form
{ This way is usefull for other TMenu components that need BidiMode of form changed
  Like as TToolbar }
  lMessage.msg := CM_PARENTBIDIMODECHANGED;
  lMessage.wParam := 0;
  lMessage.lParam := 0;
  lMessage.Result := 0;
  for i := 0 to ComponentCount - 1 do
  begin
    if not (Components[i] is TCustomControl) then//TCustomControl already has this notification
      Components[i].Dispatch(lMessage);
//the old way
//    if Components[i] is TMenu then
//      TMenu(Components[i]).ParentBiDiModeChanged;
  end;
end;

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

function TCustomForm.FindDefaultForActiveControl: TWinControl;
begin
  Result:=FindNextControl(nil, True, True, False)
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
  FFormHandlers[fhtCreate].CallNotifyEvents(Self);
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
  //DebugLn('TCustomForm.DoClose ',DbgSName(Self),' ',dbgs(ord(CloseAction)));
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
  if FActive then
  begin
    if (ActiveControl = nil) and not (csDesigning in ComponentState) then
      ActiveControl := FindDefaultForActiveControl;
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
    FormEndUpdated;
    Visible:=(fsVisible in FFormState);
  end;
end;

procedure TCustomForm.EnsureVisible(AMoveToTop: boolean = true);
var
  newLeft, newTop: integer;
begin
  newLeft := Left;
  newTop := Top;
  if newLeft + (Width div 2) > Screen.Width then
    newLeft := Screen.Width - Width;
  if newLeft < 0 then
    newLeft := 0;
  if newTop + (Height div 2) + 24 > Screen.Height then
    newTop := Screen.Height - Height - 24;
  if newTop < 0 then
    newTop := 0;
  SetBounds(newLeft, newTop, Width, Height);
  if AMoveToTop then 
    ShowOnTop
  else
    Show;
end;

{------------------------------------------------------------------------------
  function TCustomForm.FormIsUpdating: boolean;
 ------------------------------------------------------------------------------}
function TCustomForm.FormIsUpdating: boolean;
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
procedure TCustomForm.PaintWindow(DC : Hdc);
begin
  // Canvas.Lock;
  try
    Canvas.Handle := DC;
    //DebugLn('[TCustomForm.PaintWindow] ',ClassName,' DC=',DbgS(DC,8),'  ',DbgS(FCanvas.Handle,8));
    try
      if FDesigner <> nil then FDesigner.PaintGrid else Paint;
    finally
      Canvas.Handle := 0;
    end;
  finally
    // Canvas.Unlock;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomForm.RequestAlign
  Params:   none
  Returns:  nothing

  Calls user handler
 ------------------------------------------------------------------------------}
procedure TCustomForm.RequestAlign;
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
procedure TCustomForm.SetDesigner(Value : TIDesigner);
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
      if (Screen.GetCurrentModalForm<>nil)
      and (Screen.GetCurrentModalForm<>Self) then exit;
      //TODO: call TWSCustomFormClass(Widgetset).SetZORder.
      Screen.MoveFormToZFront(Self);
      SetForegroundWindow(Handle);
    end;
  end else
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
  inherited VisibleChanging;
end;

{------------------------------------------------------------------------------
       TCustomForm WndProc
------------------------------------------------------------------------------}
procedure TCustomForm.WndProc(var TheMessage : TLMessage);

 {-----------------------------------------------------------------------
   Return if the control contain a form
  -----------------------------------------------------------------------}
  function ContainsForm(Control : TWinControl) : Boolean;
  var
    I     : Integer;
    Found : Boolean;
  begin
    Found := False;
    if Control <> Nil then
    begin
      I := 1;
      while (I <= Control.ControlCount) And (Not Found) do
      begin
        if (Control.Controls[I-1] Is TCustomForm)
        then
          Found := True
        else
          If (Control.Controls[I-1] Is TWinControl)
          then
            Found := ContainsForm(Control.Controls[I-1] As TWinControl);
        Inc(I);
      end;
    end;
    Result := Found;
  end;

var
  FocusHandle : HWND;
  MenuItem : TMenuItem;
begin
  with TheMessage do
  case Msg of
    LM_ACTIVATE, LM_SETFOCUS, LM_KILLFOCUS:
      begin
        if (Msg = LM_SETFOCUS) and not (csDesigning in ComponentState) then
        begin
          //DebugLn(['TCustomForm.WndProc ',DbgSName(Self),' FActiveControl=',DbgSName(FActiveControl)]);
          FocusHandle := 0;

          if (ActiveControl = nil) and (not (csDesigning in ComponentState))
          and (Parent=nil) then begin
            // automatically choose a control to focus
            {$IFDEF VerboseFocus}
            DebugLn('TCustomForm.WndProc ',DbgSName(Self),' Set ActiveControl := ',DbgSName(FindDefaultForActiveControl));
            {$ENDIF}
            ActiveControl := FindDefaultForActiveControl;
          end;

          if FormStyle = fsMDIFORM then
          begin
            // ToDo
          end
          else
          begin
            if (FActiveControl <> nil) and (FActiveControl <> Self)
            and FActiveControl.IsVisible and FActiveControl.Enabled
            and ([csLoading,csDestroying]*ComponentState=[])
            and not FActiveControl.ParentDestroyingHandle
            then begin
              // get or create handle of FActiveControl
              FocusHandle := FActiveControl.Handle;
              //debugln('TCustomForm.WndProc A ',DbgSName(Self),' FActiveControl=',DbgSName(FActiveControl),' FocusHandle=',dbgs(FocusHandle));
            end;
          end;

          TheMessage.Result:=0;
          if FocusHandle <> 0
          then begin
            // redirect focus to child
            {$IFDEF VerboseFocus}
            DebugLn('[TCustomForm.WndProc] ',Name,':',ClassName,' FActiveControl=',DbgSName(FActiveControl));
            {$ENDIF}
            LCLIntf.SetFocus(FocusHandle);
            if not ContainsForm(Self) then exit;
          end;
        end;
      end;
    CM_EXIT:
      begin
        if HostDockSite <> nil then DeActivate;
      end;
    CM_ENTER:
      begin
        if HostDockSite <> nil then Activate;
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

procedure TCustomForm.DoSendBoundsToInterface;
begin
  inherited DoSendBoundsToInterface;
  if WindowState=wsNormal then begin
    FRestoredLeft:=Left;
    FRestoredTop:=Top;
    FRestoredWidth:=Width;
    FRestoredHeight:=Height;
  end;
end;

procedure TCustomForm.DoAutoSize;
begin
  //DebugLn(['TCustomForm.DoAutoSize ',DbgSName(Self),' ',WindowState=wsNormal,' ',fsDisableAutoSize in FFormState,' ',dbgs(BoundsRect),' ',dbgs(ClientRect)]);
  if (WindowState=wsNormal) and (not (fsDisableAutoSize in FFormState)) then
    inherited DoAutoSize;
end;

procedure TCustomForm.SetAutoSize(const Value: Boolean);
begin
  if Value=AutoSize then exit;
  if Value=true then
    Exclude(FFormState,fsDisableAutoSize);
  inherited SetAutoSize(Value);
end;

class function TCustomForm.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=320;
  Result.Y:=240;
end;

procedure TCustomForm.DoDock(NewDockSite: TWinControl; var ARect: TRect);
//Save or restore the borderstyle
begin
  if (NewDockSite <> HostDockSite) and ((NewDockSite = nil) or Floating) then
  begin
    if NewDockSite = nil then begin
      //Restore the form borderstyle
      BorderStyle := FOldBorderStyle;
      // Note: changing the Align property must be done by the dock manager, not by default
    end else begin
      //Save the borderstyle & set new bordertype
      FOldBorderStyle := BorderStyle;
      BorderStyle := bsNone;
      // Note: changing the Align property must be done by the dock manager, not by default
    end;
  end;
  inherited DoDock(NewDockSite, ARect);
end;

function TCustomForm.GetFloating: Boolean;
begin
  Result := (HostDockSite = nil) and (FloatingDockSiteClass = ClassType);
end;

function TCustomForm.GetDefaultDockCaption: String;
begin
  Result := Caption;
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
  function DoExecuteActionInChildControls(ParentControl: TWinControl;
    AnAction: TBasicAction) : boolean;
  var
    i: integer;
    ChildControl: TControl;
  begin
    Result := true;
    for i := 0 to ParentControl.ControlCount-1 do
    begin
      ChildControl := ParentControl.Controls[i];
      if ChildControl.Visible then
      begin
        if ChildControl.ExecuteAction(AnAction)
          then exit;
        if (ChildControl is TWinControl) and
           DoExecuteActionInChildControls(TWinControl(ChildControl), AnAction)
          then exit;
      end;
    end;
    Result := false;
  end;
  
begin
  // don't execute action while designing or when form is not visible
  if (csDesigning in ComponentState) or not Visible then
    exit(false);

  // assume it gets handled somewhere
  Result := true;
  if assigned(ActiveControl) and ActiveControl.ExecuteAction(ExeAction)
    then exit;
    
  if ExecuteAction(ExeAction)
    then exit;
    
  if DoExecuteActionInChildControls(Self, ExeAction)
    then exit;

  // not handled anywhere, return false
  Result := false;
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
    //DebugLn(['RecursiveInitiate ',DbgSName(Container)]);
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
      with Menu.Items[I] do begin
        //DebugLn(['TCustomForm.UpdateActions ',Name,' Visible=',Visible]);
        if Visible then InitiateAction;
      end;
  // update all controls
  RecursiveInitiate(Self);
end;

{------------------------------------------------------------------------------
       TCustomForm SetMenu
------------------------------------------------------------------------------}
procedure TCustomForm.SetMenu(Value : TMainMenu);
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
      WidgetSet.AttachMenuToWindow(FMenu);
    end;
  end;
end;

{------------------------------------------------------------------------------
       TCustomForm SetBorderIcons
------------------------------------------------------------------------------}
procedure TCustomForm.SetBorderIcons(NewIcons: TBorderIcons);
begin
  if FBorderIcons = NewIcons then exit;
  FBorderIcons := NewIcons;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetBorderIcons(Self, NewIcons);
end;

{------------------------------------------------------------------------------
       TCustomForm SetFormBorderStyle
------------------------------------------------------------------------------}
procedure TCustomForm.SetFormBorderStyle(NewStyle: TFormBorderStyle);
var
  AdaptBorderIcons: boolean;
begin
  if FFormBorderStyle = NewStyle then exit;
  //TODO: Finish SETBORDERSTYLE
  AdaptBorderIcons := not (csLoading in ComponentState) and
                          (BorderIcons=DefaultBorderIcons[FFormBorderStyle]);
  FFormBorderStyle := NewStyle;
  
  // if Form had default border icons before change, it should keep the default
  if AdaptBorderIcons then
    BorderIcons := DefaultBorderIcons[FFormBorderStyle];
    
  Include(FFormState,fsBorderStyleChanged);
  // ToDo: implement it.
  // We can not use inherited SetBorderStyle(NewStyle),
  // because TBorderStyle <> TFormBorderStyle;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetFormBorderStyle(Self, NewStyle);
end;

{------------------------------------------------------------------------------
       TCustomForm UpdateWindowState
------------------------------------------------------------------------------}
procedure TCustomForm.UpdateWindowState;
Begin

//TODO: Finish UpdateWindowState
  Assert(False, 'Trace:TODO: [TCustomForm.UpdateWindowState]');
end;

{------------------------------------------------------------------------------
       TCustomForm SetWindowState
------------------------------------------------------------------------------}
procedure TCustomForm.SetWindowState(Value : TWindowState);
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

{------------------------------------------------------------------------------
       TCustomForm SetActiveControl
------------------------------------------------------------------------------}
procedure TCustomForm.SetActiveControl(AWinControl: TWinControl);
begin
  if FActiveControl <> AWinControl then
  begin
    if (AWinControl<>nil) then 
    begin
      if (AWinControl = Self) or 
         (GetParentForm(AWinControl) <> Self) or 
         not ((csLoading in ComponentState) or AWinControl.CanFocus) then 
      begin
        DebugLn('TCustomForm.SetActiveControl ',DbgSName(Self),' AWinControl=',DbgSName(AWinControl),' GetParentForm(AWinControl)=',
                DbgSName(GetParentForm(AWinControl)),' csLoading=',dbgs(csLoading in ComponentState),' AWinControl.CanFocus=',
                dbgs((AWinControl<>nil) and AWinControl.CanFocus),' IsControlVisible=',dbgs((AWinControl<>nil) and AWinControl.IsControlVisible),
                ' Enabled=',dbgs((AWinControl<>nil) and AWinControl.Enabled));
        {$IFDEF VerboseFocus}
        RaiseGDBException(SCannotFocus);
        {$ELSE}
        raise EInvalidOperation.Create(SCannotFocus);
        {$ENDIF}
      end;
    end;

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
    if ([csLoading, csDestroying] * ComponentState = []) then
    begin
      if FActive then
        SetWindowFocus;
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

  // notify previous active default control that he has lost "default-ness"
  if lPrevControl <> nil then
    lPrevControl.ActiveDefaultControlChanged(AControl);
  // notify default control that it may become/lost active default again
  if (FDefaultControl <> nil) and (FDefaultControl <> lPrevControl) then
    FDefaultControl.ActiveDefaultControlChanged(AControl);
end;

procedure TCustomForm.SetAllowDropFiles(const AValue: Boolean);
begin
  if AValue = FAllowDropFiles then Exit;
  FAllowDropFiles := AValue;
  
  if HandleAllocated and not (csDesigning in ComponentState) then
    TWSCustomFormClass(WidgetSetClass).SetAllowDropFiles(Self, AValue);
end;

{------------------------------------------------------------------------------
       TCustomForm SetFormStyle
------------------------------------------------------------------------------}
procedure TCustomForm.SetFormStyle(Value : TFormStyle);
var
  OldFormStyle: TFormStyle;
Begin
  if FFormStyle = Value then
    exit;
  OldFormStyle := FFormStyle;
  FFormStyle := Value;
  Include(FFormState, fsFormStyleChanged);

  if FFormStyle = fsSplash then
    BorderStyle := bsNone
  else
  if OldFormStyle = fsSplash then
    BorderStyle := bsSizeable;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetFormStyle(Self, Value);
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

procedure TCustomForm.SetShowInTaskbar(Value: TShowInTaskbar);
begin
  if Value = FShowInTaskbar then exit;
  FShowInTaskbar := Value;
  if HandleAllocated then
    TWSCustomFormClass(WidgetSetClass).SetShowInTaskbar(Self, Value);
end;

{------------------------------------------------------------------------------
       TCustomForm Constructor
------------------------------------------------------------------------------}
constructor TCustomForm.Create(AOwner : TComponent);
begin
  //DebugLn('[TCustomForm.Create] A Class=',Classname);
  FShowInTaskbar := stDefault;
  FAllowDropFiles := False;

  GlobalNameSpace.BeginWrite;
  try
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
  finally
    GlobalNameSpace.EndWrite;
  end;
  //DebugLn('[TCustomForm.Create] END Class=',Classname);
end;

{------------------------------------------------------------------------------
  constructor TCustomForm.CreateNew(AOwner: TComponent; Num : Integer);
------------------------------------------------------------------------------}
constructor TCustomForm.CreateNew(AOwner: TComponent; Num : Integer);
begin
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

  FMenu:= nil;

  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
                                  csClickEvents, csSetCaption, csDoubleClicks];
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ParentColor := False;
  ParentFont := False;
  Ctl3D := True;
  FWindowState := wsNormal;
  FIcon := TIcon.Create;
  FKeyPreview :=  False;
  Color := clBtnFace;
  FloatingDockSiteClass := TWinControlClass(ClassType);
  Screen.AddForm(Self);
  EndFormUpdate;
  FAllowDropFiles := False;
End;

{------------------------------------------------------------------------------
  TCustomForm CreateParams
------------------------------------------------------------------------------}
procedure TCustomForm.CreateParams(var Params : TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if (Parent = nil) then
    begin
      Style := Style and not Cardinal(WS_GROUP or WS_TABSTOP);
      if Parent = nil then
        Style := Style and not Cardinal(WS_CHILD);
    end;
  end;
end;


{------------------------------------------------------------------------------
       TCustomForm Method Close
------------------------------------------------------------------------------}
procedure TCustomForm.Close;
var
  CloseAction: TCloseAction;
  IsMainForm: Boolean;
begin
  if fsModal in FFormState then
    ModalResult := mrCancel
  else
  begin
    if CloseQuery then
    begin
      // IsMainForm flag set if we are closeing MainForm or its parent
      IsMainForm := (Application.MainForm = Self) or (Self.IsParentOf(Application.MainForm));
      // Prepare default close action
      if FormStyle = fsMDIChild then
      begin
        CloseAction := caNone;
        // TODO: mdi logic
      end
      else
      begin
        if IsMainForm then
          CloseAction := caFree
        else
          CloseAction := caHide;
      end;
      // call even handler - maybe use want to modify it
      DoClose(CloseAction);
      // execute action according to close action
      case CloseAction of
        caHide: Hide;
        caMinimize: WindowState := wsMinimized;
        caFree:
          begin
            // if form is MainForm, then terminate the application
            // the owner of the MainForm is the application,
            // so the Application will take care of free-ing the form
            // and Release is not necessary
            if IsMainForm then
              Application.Terminate
            else
              Release;
          end;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.Release;
------------------------------------------------------------------------------}
procedure TCustomForm.Release;
begin
  if Application <> nil then
    Application.ReleaseComponent(Self)
  else
    Free;
end;

{------------------------------------------------------------------------------
       TCustomForm Method CloseQuery
------------------------------------------------------------------------------}
function TCustomForm.CloseQuery: boolean;
begin
  if FormStyle = fsMDIForm then
  begin
    // Query children forms whether we can close
    // TODO: mdi logic
  end;
  Result := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, Result);
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

{------------------------------------------------------------------------------
       TCustomForm Method Hide
------------------------------------------------------------------------------}
procedure TCustomForm.Hide;
begin
  if (fsModal in FormState) and (ModalResult = 0) then
    ModalResult := mrCancel;
  Visible := False;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.Show;
------------------------------------------------------------------------------}
procedure TCustomForm.Show;
begin
  Visible := True;
  BringToFront;
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.ShowOnTop;
------------------------------------------------------------------------------}
procedure TCustomForm.ShowOnTop;
begin
  Visible := True;
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  BringToFront;
  //DebugLn('TCustomForm.ShowOnTop ',Name,':',ClassName,' ',Visible,' ',HandleAllocated,' ',csDesigning in ComponentState);
end;

function TCustomForm.NeedParentForAutoSize: Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
       TCustomForm Method IsForm
------------------------------------------------------------------------------}
function TCustomForm.IsForm: Boolean;
begin
  Result := True;
end;

function TCustomForm.GetPixelsPerInch: Longint;
var
  ParentForm: TCustomForm;
  DC: HDC;
begin
  if FPixelsPerInch=0 then begin
    if Parent<>nil then begin
      ParentForm:=GetParentForm(Self);
      if ParentForm<>nil then begin
        FPixelsPerInch:=ParentForm.PixelsPerInch;
      end;
    end;

    if FPixelsPerInch<=0 then begin
      if HandleAllocated then begin
        DC:=GetDC(Handle);
        FPixelsPerInch:=GetDeviceCaps(DC,LOGPIXELSX);
        ReleaseDC(Handle,DC);
      end else begin
        FPixelsPerInch:=Screen.PixelsPerInch;
      end;
    end;
  end;
  Result:=FPixelsPerInch;
end;

function TCustomForm.GetRestoredLeft: integer;
begin
  if WindowState=wsNormal then
    Result := Left
  else
    Result := FRestoredLeft;
end;

function TCustomForm.GetRestoredTop: integer;
begin
  if WindowState=wsNormal then
    Result := Top
  else
    Result := FRestoredTop;
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
  CurControl: TWinControl;
begin
  Result := False;
  if (Control<>nil)
  and (csDestroying in Control.ComponentState) then exit;

  if (Parent<>nil) then begin
    // delegate to topmost form
    ParentForm:=GetParentForm(Self);
    if ParentForm<>nil then
      ParentForm.SetFocusedControl(Control);
    exit;
  end;

  // update FActiveControl
  if (FDesigner = nil) and (not (csLoading in ComponentState)) then
  begin
    if Control <> Self then
      FActiveControl := Control
    else
      FActiveControl := nil;
  end;

  // update Screen object
  Screen.FActiveControl := Control;
  if Control<>nil then begin
    Screen.FActiveCustomForm := Self;
    Screen.MoveFormToFocusFront(Self);
    if Self is TForm then
      Screen.FActiveForm := TForm(Self)
    else
      Screen.FActiveForm := nil;
  end;

  {$IFDEF VerboseFocus}
  DbgOut('TCustomForm.SetFocusedControl Self=',DbgSName(Self));
  if Control<>nil then
    DbgOut(' Control=',DbgSName(Control),' Control.HandleAllocated=',dbgs(Control.HandleAllocated));
  DebugLn();
  {$ENDIF}

  Result:=true;

  if (Control<>nil) and (not (csFocusing in Control.ControlState)) then begin
    // prevent looping
    // update ActiveControls of all parent forms
    CurControl:=Control.Parent;
    while CurControl<>nil do begin
      if CurControl is TCustomForm then
        TCustomForm(CurControl).FActiveControl:=Control;
      CurControl:=CurControl.Parent;
    end;
  end;
end;

{------------------------------------------------------------------------------
       TCustomForm Method WantChildKey
------------------------------------------------------------------------------}
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
  begin
    FOnShortcut(Message, Result);
    if Result then exit;
  end;
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
  Method:  TCustomForm.IntfDropFiles
  Params:  FileNames - Dropped files

  Invokes OnDropFilesEvent of the form.
  This function is called by the interface.
 ------------------------------------------------------------------------------}
procedure TCustomForm.IntfDropFiles(const FileNames: array of String);
begin
  if Assigned(FOnDropFiles) then FOnDropFiles(Self, FileNames);
end;

{------------------------------------------------------------------------------
  procedure TCustomForm.IntfHelp(AComponent: TComponent);

  Show help for control or menu item.
  This function is called by the interface.
------------------------------------------------------------------------------}
procedure TCustomForm.IntfHelp(AComponent: TComponent);
begin
  if csDesigning in ComponentState then exit;
  
  if AComponent is TControl then begin
    TControl(AComponent).ShowHelp;
  end else begin
    DebugLn('TCustomForm.IntfHelp TODO help for ',DbgSName(AComponent));
  end;
end;

function TCustomForm.GetFormImage: TBitmap;
var
  ARect: TRect;
begin
  Result := TBitmap.Create;
  try
    Result.Width := ClientWidth;
    Result.Height := ClientHeight;
    LCLIntf.GetWindowRect(Handle, ARect);
    with GetClientOrigin do
      PaintTo(Result.Canvas, ARect.Left - X, ARect.Top - Y);
  except
    Result.Free;
    raise;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomForm.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomForm.CreateWnd;
begin
  //DebugLn('TCustomForm.CreateWnd START ',ClassName);
  FFormState:=FFormState-[fsBorderStyleChanged,fsFormStyleChanged];
  inherited CreateWnd;

  FPixelsPerInch:=0;

  Assert(False, 'Trace:[TCustomForm.CreateWnd] FMenu.HandleNeeded');
  if FMenu <> nil then
  begin
    FMenu.HandleNeeded;
    WidgetSet.AttachMenuToWindow(FMenu);
  end;

  // activate focus if visible
  if Visible then begin
    if (ActiveControl = nil) and (not (csDesigning in ComponentState))
    and (Parent=nil) then begin
      // automatically choose a control to focus
      {$IFDEF VerboseFocus}
      DebugLn('TCustomForm.CreateWnd ',DbgSName(Self),' Set ActiveControl := ',DbgSName(FindDefaultForActiveControl));
      {$ENDIF}
      ActiveControl := FindDefaultForActiveControl;
    end;
    if (Parent=nil)
    and (FActiveControl<>nil) and FActiveControl.HandleAllocated
    and FActiveControl.CanFocus
    and ([csLoading,csDestroying,csDesigning]*ComponentState=[]) then begin
      {$IFDEF VerboseFocus}
      DebugLn('TCustomForm.CreateWnd A ',DbgSName(Self),' FActiveControl=',DbgSName(FActiveControl));
      {$ENDIF}
      LCLIntf.SetFocus(FActiveControl.Handle);
    end;
  end;
  
  // set allow drop files
  TWSCustomFormClass(WidgetSetClass).SetAllowDropFiles(Self, FAllowDropFiles);
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
  DisableAlign;
  try
    inherited Loaded;
  finally
    EnableAlign;
  end;
  if (ActiveControl <> nil) and (Parent=nil) then
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
  if csLoading in ComponentState then exit;
  {$IFDEF CHECK_POSITION}
  DebugLn('[TCustomForm.UpdateShowing] A  ',DbgSName(Self),' Pos=',DbgS(Left),',',DbgS(Top),' Visible=',DbgS(Visible));
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
        X := (Application.Mainform.ClientWidth - Width) div 2;
        Y := (Application.Mainform.ClientHeight - Height) div 2;
      end
      else begin
        case Position of
        //TODO:poDefault, poDefaultPosOnly, poDefaultSizeOnly
        poDesktopCenter :
          begin
            X := (Screen.DesktopWidth - Width) div 2;
            Y := (Screen.DesktopHeight - Height) div 2;
          end;
        poScreenCenter :
          begin
            X := (Screen.Width - Width) div 2;
            Y := (Screen.Height - Height) div 2;
          end;
        poMainFormCenter :
          if (Self <> Application.MainForm) then begin
            X := ((Application.MainForm.Width - Width) div 2) + Application.MainForm.Left;
            Y := ((Application.MainForm.Height - Height) div 2) + Application.MainForm.Top;
          end;
        poOwnerFormCenter :
          if (Owner is TCustomForm) then begin
            X := ((TCustomForm(Owner).Width - Width) div 2) + TCustomForm(Owner).Left;
            Y := ((TCustomForm(Owner).Height - Height) div 2) + TCustomForm(Owner).Top;
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
begin
  FFormHandlers[fhtFirstShow].CallNotifyEvents(Self);
end;

{------------------------------------------------------------------------------
  TCustomForm ShowModal
------------------------------------------------------------------------------}
function TCustomForm.ShowModal: Integer;

  procedure RaiseShowModalImpossible;
  begin
    DebugLn('TCustomForm.ShowModal Visible=',dbgs(Visible),' Enabled=',dbgs(Enabled),
      ' fsModal=',dbgs(fsModal in FFormState),' MDIChild=',dbgs(FormStyle = fsMDIChild));
    raise EInvalidOperation.Create('TCustomForm.ShowModal impossible ');
  end;

var
  //WindowList: Pointer;
  SavedFocusState: TFocusState;
  //SaveCursor: TCursor;
  //SaveCount: Integer;
  ActiveWindow: HWnd;
begin
  if Self=nil then raise
    EInvalidOperation.Create('TCustomForm.ShowModal Self=nil');
  if Application.Terminated then
    ModalResult:=0;
  // cancel drags
  DragManager.DragStop(false);
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
  SavedFocusState := SaveFocusState;
  Screen.FSaveFocusedList.Insert(0, Screen.FFocusedForm);
  Screen.FFocusedForm := Self;
  Screen.MoveFormToFocusFront(Self);
  Screen.MoveFormToZFront(Self);
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
        try
          WidgetSet.AppProcessMessages; // process all events
        except
          on E: Exception do Application.HandleException(E);
        end;
        if Application.Terminated then
          ModalResult := mrCancel;
        if ModalResult <> 0 then begin
          CloseModal;
          if ModalResult<>0 then break;
        end;
        Application.Idle(true);
      until false;

      Result := ModalResult;
      if HandleAllocated and (GetActiveWindow <> Handle) then
        ActiveWindow := 0;
    finally
      { guarantee execution of widgetset CloseModal }
      TWSCustomFormClass(WidgetSetClass).CloseModal(Self);
      Hide;
      // free handles to save resources and to reduce overhead in the interfaces
      // for bookkeeping changing between Show and ShowModal.
      // (e.g.: the gtk interface creates some specials on ShowModal, so the
      //  combination ShowModal, Close, Show makes problems.)
      DestroyHandle;
    end;
  finally
    if Screen.FSaveFocusedList.Count > 0 then
    begin
      Screen.FFocusedForm := TCustomForm(Screen.FSaveFocusedList.First);
      Screen.FSaveFocusedList.Remove(Screen.FFocusedForm);
    end else
      Screen.FFocusedForm := nil;
    Exclude(FFormState, fsModal);
    RestoreFocusState(SavedFocusState);
    //DebugLn('TCustomForm.ShowModal ',dbgs(ActiveWindow));
    if ActiveWindow <> 0 then
      SetActiveWindow(ActiveWindow);
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

procedure TCustomForm.AddHandlerCreate(OnCreateHandler: TNotifyEvent;
  AsLast: Boolean);
begin
  AddHandler(fhtCreate,TMethod(OnCreateHandler),AsLast);
end;

procedure TCustomForm.RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);
begin
  RemoveHandler(fhtCreate,TMethod(OnCreateHandler));
end;

procedure TCustomForm.Dock(NewDockSite: TWinControl; ARect: TRect);
begin
  inherited Dock(NewDockSite, ARect);
end;

//==============================================================================

{ TForm }

function TForm.LCLVersionIsStored: boolean;
begin
  Result:=Parent=nil;
end;

procedure TForm.CreateWnd;
begin
  if (Application<>nil) then
    Application.UpdateMainForm(TForm(Self));
  inherited CreateWnd;
end;

procedure TForm.Loaded;
begin
  inherited Loaded;
  FLCLVersion:=lcl_version;
end;

constructor TForm.Create(TheOwner: TComponent);
begin
  FLCLVersion:=lcl_version;
  inherited Create(TheOwner);
end;

//==============================================================================

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
