{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TCommonDialog
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{------------------------------------------------------------------------------
  Method: TCommonDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCommonDialog.Create (TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTitle := DefaultTitle;
end;

function TCommonDialog.Execute: boolean;
var
  DisabledList: TList;
  SavedFocusState: TFocusState;
begin
  SavedFocusState := SaveFocusState;
  Application.ModalStarted;
  try
    DisabledList := Screen.DisableForms(Screen.ActiveForm);
    try
      FUserChoice := mrNone;
      ResetShowCloseFlags;
      FWSEventCapabilities := TWSCommonDialogClass(WidgetSetClass).QueryWSEventCapabilities(Self);
      Handle := TWSCommonDialogClass(WidgetSetClass).CreateHandle(Self);
      Result:= DoExecute;
      Close;
    finally
      Screen.EnableForms(DisabledList);
      RestoreFocusState(SavedFocusState);
      if (Screen.ActiveControl<>nil) and (Screen.ActiveControl.HandleAllocated)
      and (GetFocus<>Screen.ActiveControl.Handle) then
        SetFocus(Screen.ActiveControl.Handle); // must restore focus after Screen.EnableForms
    end;
  finally
    Application.ModalFinished;
  end;
end;

procedure TCommonDialog.Close;
begin
  if HandleAllocated and not FClosing then begin
    FClosing := true;
    if (not FDoCloseCalled) and (not (cdecWSPerformsDoClose in FWSEventCapabilities)) then
      DoClose;
    TWSCommonDialogClass(WidgetSetClass).DestroyHandle(Self);
    FHandle := 0;
    FClosing := false;
  end;
end;

procedure TCommonDialog.DoShow;
begin
  if FDoShowCalled then Exit;
  FDoShowCalled := True;
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TCommonDialog.DoCanClose(var CanClose: Boolean);
begin
  FDoCanCloseCalled := True;
  if Assigned(FOnCanClose) and (not (cdecWSNOCanCloseSupport in FWSEventCapabilities)) then
    OnCanClose(Self, CanClose);
end;

procedure TCommonDialog.DoClose;
begin
  if FDoCloseCalled then Exit;
  FDoCloseCalled := True;
  if Assigned(FOnClose) then FOnClose(Self);
end;

function TCommonDialog.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

procedure TCommonDialog.SetHandle(const AValue: THandle);
begin
  FHandle:=AValue;
end;

function TCommonDialog.IsTitleStored: boolean;
begin
  result := FTitle<>DefaultTitle;
end;

class procedure TCommonDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCommonDialog;
  RegisterPropertyToSkip(TCommonDialog, 'Width', 'Property streamed in older Lazarus revision','');
  RegisterPropertyToSkip(TCommonDialog, 'Height', 'Property streamed in older Lazarus revision','');
end;

procedure TCommonDialog.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
end;

procedure TCommonDialog.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

procedure TCommonDialog.ResetShowCloseFlags;
begin
  FDoShowCalled := False;
  FDoCanCloseCalled := False;
  FDoCloseCalled := False;
end;

function TCommonDialog.DoExecute : boolean;
var
  CanClose: boolean;
begin
  {
    Various widgetsets may or may not call DoShow, DoCanClose or DoClose from within
    the WS implementation.
    If the WS calls any of these, we assume that we should NOT call them from here.
    Checking for FDoShowCalled (etc) alone is not enough, since it may very well be that
    the WS wants to (deiberately) call the methos at a later point in time.
  }
  {$ifdef DebugCommonDialogEvents}
  debugln(['TCommonDialog.DoExecute A']);
  {$endif}
  if (not FDoShowCalled) and (not (cdecWSPerformsDoShow in FWSEventCapabilities)) then
  begin
    {$ifdef DebugCommonDialogEvents}
    debugln(['TCommonDialog.DoExecute calling DoShow']);
    {$endif}
    DoShow;
  end;
  {$ifdef DebugCommonDialogEvents}
  debugln(['TCommonDialog.DoExecute before WS_ShowModal']);
  {$endif}
  TWSCommonDialogClass(WidgetSetClass).ShowModal(Self);
  {$ifdef DebugCommonDialogEvents}
  debugln(['TCommonDialog.DoExecute after WS_ShowModal, FCanCloseCalled=',FDoCanCloseCalled,' FUserChoice=',ModalResultStr[FUserChoice]]);
  {$endif}
  // can close was called from widgetset loop
  if (not FDoCanCloseCalled) and ((FWSEventCapabilities * [cdecWSPerformsDoCanClose,cdecWSNOCanCloseSupport]) = []) then
  begin
    repeat
      {$ifdef DebugCommonDialogEvents}
      debugln(['TCommonDialog.DoExecute, FUserChoice=',ModalResultStr[FUserChoice],' Handle=',Handle]);
      {$endif}
      if (FUserChoice <> mrNone) and (Handle<>0) then
      begin
        CanClose := True;
        {$ifdef DebugCommonDialogEvents}
        debugln(['TCommonDialog.DoExecute calling DoCanClose']);
        {$endif}
        DoCanClose(CanClose);
        if not CanClose then
          FUserChoice:=mrNone;
        {$ifdef DebugCommonDialogEvents}
        debugln(['TCommonDialog.DoExecute after calling DoCanClose: CanClose=',CanClose,' FUserChoice=',ModalResultStr[FUserChoice]]);
        {$endif}
      end;
      if FUserChoice <> mrNone then
        break;
      { win32 widgetset dialogs use their own message loop,
        so only FUserChoice may have been set already }
      Application.HandleMessage;
    until false;
  end;
  Result := (FUserChoice = mrOk);
  {$ifdef DebugCommonDialogEvents}
  debugln(['TCommonDialog.DoExecute End']);
  {$endif}
end;

function TCommonDialog.DefaultTitle: string;
begin
  Result := '';
end;

function TCommonDialog.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCommonDialog.GetWidth: Integer;
begin
  Result := FWidth;
end;
