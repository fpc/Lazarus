{%MainUnit ../forms.pp}
{******************************************************************************
                          TApplicationProperties
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

Procedure TApplicationProperties.SetCaptureExceptions(Const AValue : boolean);
begin
  FCaptureExceptions := AValue;

  If not (csDesigning in ComponentState) then
    Application.CaptureExceptions := AValue;
end;

Procedure TApplicationProperties.SetHelpFile(Const AValue : string);
begin
  FHelpFile := AValue;

  If not (csDesigning in ComponentState) then
    Application.HelpFile := AValue;
end;

Procedure TApplicationProperties.SetHint(Const AValue : string);
begin
  FHint := AValue;

  If not (csDesigning in ComponentState) then
    Application.Hint := AValue;
end;

Procedure TApplicationProperties.SetHintColor(Const AValue : TColor);
begin
  FHintColor := AValue;

  If not (csDesigning in ComponentState) then
    Application.HintColor := AValue;
end;

Procedure TApplicationProperties.SetHintHidePause(Const AValue : Integer);
begin
  FHintHidePause := AValue;

  If not (csDesigning in ComponentState) then
    Application.HintHidePause := AValue;
end;

Procedure TApplicationProperties.SetHintPause(Const AValue : Integer);
begin
  FHintPause := AValue;

  If not (csDesigning in ComponentState) then
    Application.HintPause := AValue;
end;

Procedure TApplicationProperties.SetHintShortCuts(Const AValue : Boolean);
begin
  FHintShortCuts := AValue;

  If not (csDesigning in ComponentState) then
    Application.HintShortCuts := AValue;
end;

Procedure TApplicationProperties.SetHintShortPause(Const AValue : Integer);
begin
  FHintShortPause := AValue;

  If not (csDesigning in ComponentState) then
    Application.HintShortPause := AValue;
end;

Procedure TApplicationProperties.SetShowHint(Const AValue : Boolean);
begin
  FShowHint := AValue;

  If not (csDesigning in ComponentState) then
    Application.ShowHint := AValue;
end;

procedure TApplicationProperties.SetShowMainForm(const AValue: Boolean);
begin
  FShowMainForm:=AValue;

  if not (csDesigning in ComponentState) then
    Application.ShowMainForm := AValue;
end;

Procedure TApplicationProperties.SetTitle(Const AValue : String);
begin
  FTitle := AValue;

  If not (csDesigning in ComponentState) then
    Application.Title := AValue;
end;

Procedure TApplicationProperties.SetOnException(Const AValue : TExceptionEvent);
begin
  if (TMethod(FOnException).Data=TMethod(AValue).Data)
  and (TMethod(FOnException).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnException) then
    Application.RemoveOnExceptionHandler(FOnException);
  FOnException := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnException) then
    Application.AddOnExceptionHandler(FOnException);
end;

Procedure TApplicationProperties.SetOnIdle(Const AValue : TIdleEvent);
begin
  if (TMethod(FOnIdle).Data=TMethod(AValue).Data)
  and (TMethod(FOnIdle).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnIdle) then
    Application.RemoveOnIdleHandler(FOnIdle);
  FOnIdle := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnIdle) then
    Application.AddOnIdleHandler(FOnIdle);
end;

Procedure TApplicationProperties.SetOnIdleEnd(Const AValue : TNotifyEvent);
begin
  if (TMethod(FOnIdleEnd).Data=TMethod(AValue).Data)
  and (TMethod(FOnIdleEnd).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnIdleEnd) then
    Application.RemoveOnIdleEndHandler(FOnIdleEnd);
  FOnIdleEnd := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnIdleEnd) then
    Application.AddOnIdleEndHandler(FOnIdleEnd);
end;


Procedure TApplicationProperties.SetOnEndSession(Const AValue : TNotifyEvent);
begin
  if (TMethod(FOnEndSession).Data=TMethod(AValue).Data)
  and (TMethod(FOnEndSession).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnEndSession) then
    Application.RemoveOnEndSessionHandler(FOnEndSession);
  FOnEndSession := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnEndSession) then
    Application.AddOnEndSessionHandler(FOnEndSession);
end;


Procedure TApplicationProperties.SetOnQueryEndSession(Const AValue : TQueryEndSessionEvent);
begin
  if (TMethod(FOnQueryEndSession).Data=TMethod(AValue).Data)
  and (TMethod(FOnQueryEndSession).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnQueryEndSession) then
    Application.RemoveOnQueryEndSessionHandler(FOnQueryEndSession);
  FOnQueryEndSession := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnQueryEndSession) then
    Application.AddOnQueryEndSessionHandler(FOnQueryEndSession);
end;

procedure TApplicationProperties.SetOnMinimize(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnMinimize).Data=TMethod(AValue).Data)
  and (TMethod(FOnMinimize).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnMinimize) then
    Application.RemoveOnMinimizeHandler(FOnMinimize);
  FOnMinimize := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnMinimize) then
    Application.AddOnMinimizeHandler(FOnMinimize);
end;

procedure TApplicationProperties.SetOnRestore(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnRestore).Data=TMethod(AValue).Data)
  and (TMethod(FOnRestore).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnRestore) then
    Application.RemoveOnRestoreHandler(FOnRestore);
  FOnRestore := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnRestore) then
    Application.AddOnRestoreHandler(FOnRestore);
end;

Procedure TApplicationProperties.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  if (TMethod(FOnDropFiles).Data=TMethod(AValue).Data)
  and (TMethod(FOnDropFiles).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnDropFiles) then
    Application.RemoveOnDropFilesHandler(FOnDropFiles);
  FOnDropFiles := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnDropFiles) then
    Application.AddOnDropFilesHandler(FOnDropFiles);
end;

Procedure TApplicationProperties.SetOnHelp(Const AValue : THelpEvent);
begin
  if (TMethod(FOnHelp).Data=TMethod(AValue).Data)
  and (TMethod(FOnHelp).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnHelp) then
    Application.RemoveOnHelpHandler(FOnHelp);
  FOnHelp := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnHelp) then
    Application.AddOnHelpHandler(FOnHelp);
end;

Procedure TApplicationProperties.SetOnHint(Const AValue : TNotifyEvent);
begin
  if (TMethod(FOnHint).Data=TMethod(AValue).Data)
  and (TMethod(FOnHint).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnHint) then
    Application.RemoveOnHintHandler(FOnHint);
  FOnHint := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnHint) then
    Application.AddOnHintHandler(FOnHint);
end;

Procedure TApplicationProperties.SetOnShowHint(Const AValue : TShowHintEvent);
begin
  if (TMethod(FOnShowHint).Data=TMethod(AValue).Data)
  and (TMethod(FOnShowHint).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnShowHint) then
    Application.RemoveOnShowHintHandler(FOnShowHint);
  FOnShowHint := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnShowHint) then
    Application.AddOnShowHintHandler(FOnShowHint);
end;

Procedure TApplicationProperties.SetOnUserInput(Const AValue : TOnUserInputEvent);
begin
  if (TMethod(FOnUserInput).Data=TMethod(AValue).Data)
  and (TMethod(FOnUserInput).Code=TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState)
  and Assigned(FOnUserInput) then
    Application.RemoveOnUserInputHandler(FOnUserInput);
  FOnUserInput := AValue;
  if not (csDesigning in ComponentState)
  and Assigned(FOnUserInput) then
    Application.AddOnUserInputHandler(FOnUserInput);
end;

constructor TApplicationProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (csDesigning in ComponentState) then begin
    FCaptureExceptions:=true;
    FHintColor := DefHintColor;
    FHintPause := DefHintPause;
    FHintShortCuts := True;
    FHintShortPause := DefHintShortPause;
    FHintHidePause := DefHintHidePause;
    FShowHint := true;
  end
  else begin
    FCaptureExceptions := Application.CaptureExceptions;
    FHelpFile := Application.HelpFile;
    FHint := Application.Hint;
    FHintColor := Application.HintColor;
    FHintHidePause := Application.HintHidePause;
    FHintPause := Application.HintPause;
    FHintShortCuts := Application.HintShortCuts;
    FHintShortPause := Application.HintShortPause;
    FShowHint := Application.ShowHint;
    FTitle := Application.Title;
  end;
  FShowMainForm := true;

  FOnException := nil;
  FOnIdle := nil;
  FOnIdleEnd := nil;
  FOnHelp := nil;
  FOnHint := nil;
  FOnShowHint := nil;
  FOnUserInput := nil;
  FOnEndSession := nil;
  FOnQueryEndSession := nil;
end;

destructor TApplicationProperties.Destroy;
begin
  If not (csDesigning in ComponentState) then begin
    Application.RemoveOnExceptionHandler(FOnException);
    Application.RemoveOnIdleHandler(FOnIdle);
    Application.RemoveOnIdleEndHandler(FOnIdleEnd);
    Application.RemoveOnEndSessionHandler(FOnEndSession);
    Application.RemoveOnQueryEndSessionHandler(FOnQueryEndSession);
    Application.RemoveOnMinimizeHandler(FOnMinimize);
    Application.RemoveOnRestoreHandler(FOnRestore);
    Application.RemoveOnDropFilesHandler(FOnDropFiles);
    Application.RemoveOnHelpHandler(FOnHelp);
    Application.RemoveOnHintHandler(FOnHint);
    Application.RemoveOnShowHintHandler(FOnShowHint);
    Application.RemoveOnUserInputHandler(FOnUserInput);
  end;
//AG Application.RemoveAllHandlersOfObject(Self); "Self" is wrong, because Events is owner's methods
  inherited Destroy;
end;

// included by forms.pp
