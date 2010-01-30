{%MainUnit ../forms.pp}
{******************************************************************************
                          TApplicationProperties
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

procedure TApplicationProperties.SetOnModalBegin(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnModalBegin).Data = TMethod(AValue).Data) and
     (TMethod(FOnModalBegin).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnModalBegin) then
    Application.RemoveOnModalBeginHandler(FOnModalBegin);
  FOnModalBegin := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnModalBegin) then
    Application.AddOnModalBeginHandler(FOnModalBegin);
end;

procedure TApplicationProperties.SetOnModalEnd(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnModalEnd).Data = TMethod(AValue).Data) and
     (TMethod(FOnModalEnd).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnModalEnd) then
    Application.RemoveOnModalEndHandler(FOnModalEnd);
  FOnModalEnd := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnModalEnd) then
    Application.AddOnModalEndHandler(FOnModalEnd);
end;

procedure TApplicationProperties.SetCaptureExceptions(const AValue : boolean);
begin
  FCaptureExceptions := AValue;

  if not (csDesigning in ComponentState) then
    Application.CaptureExceptions := AValue;
end;

procedure TApplicationProperties.SetHelpFile(const AValue : string);
begin
  FHelpFile := AValue;

  if not (csDesigning in ComponentState) then
    Application.HelpFile := AValue;
end;

procedure TApplicationProperties.SetHint(const AValue : string);
begin
  FHint := AValue;

  if not (csDesigning in ComponentState) then
    Application.Hint := AValue;
end;

procedure TApplicationProperties.SetHintColor(const AValue : TColor);
begin
  FHintColor := AValue;

  if not (csDesigning in ComponentState) then
    Application.HintColor := AValue;
end;

procedure TApplicationProperties.SetHintHidePause(const AValue : Integer);
begin
  FHintHidePause := AValue;

  if not (csDesigning in ComponentState) then
    Application.HintHidePause := AValue;
end;

procedure TApplicationProperties.SetHintPause(const AValue : Integer);
begin
  FHintPause := AValue;

  if not (csDesigning in ComponentState) then
    Application.HintPause := AValue;
end;

procedure TApplicationProperties.SetHintShortCuts(const AValue : Boolean);
begin
  FHintShortCuts := AValue;

  if not (csDesigning in ComponentState) then
    Application.HintShortCuts := AValue;
end;

procedure TApplicationProperties.SetHintShortPause(const AValue : Integer);
begin
  FHintShortPause := AValue;

  if not (csDesigning in ComponentState) then
    Application.HintShortPause := AValue;
end;

procedure TApplicationProperties.SetShowHint(const AValue : Boolean);
begin
  FShowHint := AValue;

  if not (csDesigning in ComponentState) then
    Application.ShowHint := AValue;
end;

procedure TApplicationProperties.SetShowMainForm(const AValue: Boolean);
begin
  FShowMainForm:=AValue;

  if not (csDesigning in ComponentState) then
    Application.ShowMainForm := AValue;
end;

procedure TApplicationProperties.SetShowButtonGlyphs(const AValue: TApplicationShowGlyphs);
begin
  FShowButtonGlyphs := AValue;

  if not (csDesigning in ComponentState) then
    Application.ShowButtonGlyphs := AValue;
end;

procedure TApplicationProperties.SetShowMenuGlyphs(const AValue: TApplicationShowGlyphs);
begin
  FShowMenuGlyphs := AValue;

  if not (csDesigning in ComponentState) then
    Application.ShowMenuGlyphs := AValue;
end;

procedure TApplicationProperties.SetTitle(const AValue : String);
begin
  FTitle := AValue;

  if not (csDesigning in ComponentState) then
    Application.Title := AValue;
end;

procedure TApplicationProperties.SetOnException(const AValue : TExceptionEvent);
begin
  if (TMethod(FOnException).Data = TMethod(AValue).Data) and
     (TMethod(FOnException).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnException) then
    Application.RemoveOnExceptionHandler(FOnException);
  FOnException := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnException) then
    Application.AddOnExceptionHandler(FOnException);
end;

procedure TApplicationProperties.SetOnIdle(const AValue : TIdleEvent);
begin
  if (TMethod(FOnIdle).Data = TMethod(AValue).Data) and
     (TMethod(FOnIdle).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnIdle) then
    Application.RemoveOnIdleHandler(FOnIdle);
  FOnIdle := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnIdle) then
    Application.AddOnIdleHandler(FOnIdle);
end;

procedure TApplicationProperties.SetOnIdleEnd(const AValue : TNotifyEvent);
begin
  if (TMethod(FOnIdleEnd).Data = TMethod(AValue).Data) and
     (TMethod(FOnIdleEnd).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnIdleEnd) then
    Application.RemoveOnIdleEndHandler(FOnIdleEnd);
  FOnIdleEnd := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnIdleEnd) then
    Application.AddOnIdleEndHandler(FOnIdleEnd);
end;


procedure TApplicationProperties.SetOnEndSession(const AValue : TNotifyEvent);
begin
  if (TMethod(FOnEndSession).Data = TMethod(AValue).Data) and
     (TMethod(FOnEndSession).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnEndSession) then
    Application.RemoveOnEndSessionHandler(FOnEndSession);
  FOnEndSession := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnEndSession) then
    Application.AddOnEndSessionHandler(FOnEndSession);
end;


procedure TApplicationProperties.SetOnQueryEndSession(const AValue : TQueryEndSessionEvent);
begin
  if (TMethod(FOnQueryEndSession).Data = TMethod(AValue).Data) and
     (TMethod(FOnQueryEndSession).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnQueryEndSession) then
    Application.RemoveOnQueryEndSessionHandler(FOnQueryEndSession);
  FOnQueryEndSession := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnQueryEndSession) then
    Application.AddOnQueryEndSessionHandler(FOnQueryEndSession);
end;

procedure TApplicationProperties.SetOnMinimize(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnMinimize).Data = TMethod(AValue).Data) and
     (TMethod(FOnMinimize).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnMinimize) then
    Application.RemoveOnMinimizeHandler(FOnMinimize);
  FOnMinimize := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnMinimize) then
    Application.AddOnMinimizeHandler(FOnMinimize);
end;

procedure TApplicationProperties.SetOnRestore(const AValue: TNotifyEvent);
begin
  if (TMethod(FOnRestore).Data = TMethod(AValue).Data) and
     (TMethod(FOnRestore).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnRestore) then
    Application.RemoveOnRestoreHandler(FOnRestore);
  FOnRestore := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnRestore) then
    Application.AddOnRestoreHandler(FOnRestore);
end;

procedure TApplicationProperties.SetOnDropFiles(const AValue: TDropFilesEvent);
begin
  if (TMethod(FOnDropFiles).Data = TMethod(AValue).Data) and
     (TMethod(FOnDropFiles).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnDropFiles) then
    Application.RemoveOnDropFilesHandler(FOnDropFiles);
  FOnDropFiles := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnDropFiles) then
    Application.AddOnDropFilesHandler(FOnDropFiles);
end;

procedure TApplicationProperties.SetOnHelp(const AValue : THelpEvent);
begin
  if (TMethod(FOnHelp).Data = TMethod(AValue).Data) and
     (TMethod(FOnHelp).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnHelp) then
    Application.RemoveOnHelpHandler(FOnHelp);
  FOnHelp := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnHelp) then
    Application.AddOnHelpHandler(FOnHelp);
end;

procedure TApplicationProperties.SetOnHint(const AValue : TNotifyEvent);
begin
  if (TMethod(FOnHint).Data = TMethod(AValue).Data) and
     (TMethod(FOnHint).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnHint) then
    Application.RemoveOnHintHandler(FOnHint);
  FOnHint := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnHint) then
    Application.AddOnHintHandler(FOnHint);
end;

procedure TApplicationProperties.SetOnShowHint(const AValue : TShowHintEvent);
begin
  if (TMethod(FOnShowHint).Data = TMethod(AValue).Data) and
     (TMethod(FOnShowHint).Code = TMethod(AValue).Code) then Exit;
  if not (csDesigning in ComponentState) and Assigned(FOnShowHint) then
    Application.RemoveOnShowHintHandler(FOnShowHint);
  FOnShowHint := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnShowHint) then
    Application.AddOnShowHintHandler(FOnShowHint);
end;

procedure TApplicationProperties.SetOnUserInput(const AValue : TOnUserInputEvent);
begin
  if (TMethod(FOnUserInput).Data = TMethod(AValue).Data) and
     (TMethod(FOnUserInput).Code = TMethod(AValue).Code) then exit;
  if not (csDesigning in ComponentState) and Assigned(FOnUserInput) then
    Application.RemoveOnUserInputHandler(FOnUserInput);
  FOnUserInput := AValue;
  if not (csDesigning in ComponentState) and Assigned(FOnUserInput) then
    Application.AddOnUserInputHandler(FOnUserInput);
end;

constructor TApplicationProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (csDesigning in ComponentState) then
  begin
    FCaptureExceptions := True;
    FHintColor := DefHintColor;
    FHintPause := DefHintPause;
    FHintShortCuts := True;
    FHintShortPause := DefHintShortPause;
    FHintHidePause := DefHintHidePause;
    FShowButtonGlyphs := sbgAlways;
    FShowMenuGlyphs := sbgAlways;
    FShowHint := True;
  end
  else
  begin
    FCaptureExceptions := Application.CaptureExceptions;
    FHelpFile := Application.HelpFile;
    FHint := Application.Hint;
    FHintColor := Application.HintColor;
    FHintHidePause := Application.HintHidePause;
    FHintPause := Application.HintPause;
    FHintShortCuts := Application.HintShortCuts;
    FHintShortPause := Application.HintShortPause;
    FShowButtonGlyphs := Application.ShowButtonGlyphs;
    FShowMenuGlyphs := Application.ShowMenuGlyphs;
    FShowHint := Application.ShowHint;
    FTitle := Application.Title;
  end;
  FShowMainForm := True;

  FOnException := nil;
  FOnIdle := nil;
  FOnIdleEnd := nil;
  FOnHelp := nil;
  FOnHint := nil;
  FOnShowHint := nil;
  FOnUserInput := nil;
  FOnEndSession := nil;
  FOnQueryEndSession := nil;
  FOnMinimize := nil;
  FOnModalBegin := nil;
  FOnModalEnd := nil;
  FOnRestore := nil;
  FOnDropFiles := nil;
end;

destructor TApplicationProperties.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Application.RemoveOnExceptionHandler(FOnException);
    Application.RemoveOnIdleHandler(FOnIdle);
    Application.RemoveOnIdleEndHandler(FOnIdleEnd);
    Application.RemoveOnEndSessionHandler(FOnEndSession);
    Application.RemoveOnQueryEndSessionHandler(FOnQueryEndSession);
    Application.RemoveOnMinimizeHandler(FOnMinimize);
    Application.RemoveOnModalBeginHandler(FOnModalBegin);
    Application.RemoveOnModalEndHandler(FOnModalEnd);
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
