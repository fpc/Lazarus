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
    DoClose;
    TWSCommonDialogClass(WidgetSetClass).DestroyHandle(Self);
    FHandle := 0;
    FClosing := false;
  end;
end;

procedure TCommonDialog.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TCommonDialog.DoCanClose(var CanClose: Boolean);
begin
  FCanCloseCalled := True;
  if Assigned(FOnCanClose) then
    OnCanClose(Self, CanClose);
end;

procedure TCommonDialog.DoClose;
begin
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

function TCommonDialog.DoExecute : boolean;
var
  CanClose: boolean;
begin
  FCanCloseCalled := False;
  if Assigned(FOnShow) then
    FOnShow(Self);
  TWSCommonDialogClass(WidgetSetClass).ShowModal(Self);
  {$ifdef DebugCommonDialogEvents}
  if (Self is TFileDialog) then debugln(['TCommonDialog.DoExecute, FCanCloseCalled=',FCanCloseCalled,' FUserChoice=',ModalResultStr[FUserChoice]]);
  {$endif}
  // can close was called from widgetset loop
  if not FCanCloseCalled then
  begin
    repeat
      {$ifdef DebugCommonDialogEvents}
      if (Self is TFileDialog) then debugln(['TCommonDialog.DoExecute, FUserChoice=',ModalResultStr[FUserChoice],' Handle=',Handle]);
      {$endif}
      if (FUserChoice <> mrNone) and (Handle<>0) then
      begin
        CanClose := True;
        {$ifdef DebugCommonDialogEvents}
        if (Self is TFileDialog) then debugln(['TCommonDialog.DoExecute calling DoCanClose']);
        {$endif}
        DoCanClose(CanClose);
        if not CanClose then
          FUserChoice:=mrNone;
        {$ifdef DebugCommonDialogEvents}
        if (Self is TFileDialog) then debugln(['TCommonDialog.DoExecute after calling DoCanClose: CanClose=',CanClose,' FUserChoice=',ModalResultStr[FUserChoice]]);
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
