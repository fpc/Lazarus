{%MainUnit ../dialogs.pp}

{ TTaskDialogButtonsEnumerator }

constructor TTaskDialogButtonsEnumerator.Create(ACollection: TTaskDialogButtons
  );
begin
  FCollection := ACollection;
  FIndex := -1;
end;

function TTaskDialogButtonsEnumerator.GetCurrent: TTaskDialogBaseButtonItem;
begin
  Result := FCollection[FIndex];
end;

function TTaskDialogButtonsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TTaskDialogButtons }

function TTaskDialogButtons.Add: TTaskDialogBaseButtonItem;
begin
  Result := TTaskDialogBaseButtonItem(inherited Add);
end;

function TTaskDialogButtons.FindButton(AModalResult: TModalResult
  ): TTaskDialogBaseButtonItem;
begin
  for Result in Self do
    if Result.ModalResult = AModalResult then
      Exit;

  Result := nil;
end;

function TTaskDialogButtons.GetEnumerator: TTaskDialogButtonsEnumerator;
begin
  Result := TTaskDialogButtonsEnumerator.Create(Self);
end;

function TTaskDialogButtons.GetItem(Index: Integer): TTaskDialogBaseButtonItem;
begin
  Result := TTaskDialogBaseButtonItem(inherited GetItem(Index));
end;

procedure TTaskDialogButtons.SetDefaultButton(
  const Value: TTaskDialogBaseButtonItem);
begin
  if Value <> FDefaultButton then
    FDefaultButton := Value;
end;

procedure TTaskDialogButtons.SetItem(Index: Integer;
  const Value: TTaskDialogBaseButtonItem);
begin
  inherited SetItem(Index, Value);
end;

{ TTaskDialogProgressBar }

procedure TTaskDialogProgressBar.SetMarqueeSpeed(AValue: Cardinal);
begin
  if FMarqueeSpeed = AValue then Exit;
  FMarqueeSpeed := AValue;
  if (tfShowMarqueeProgressBar in Dlg.Flags) and (Dlg.Handle <> 0) then
    SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_MARQUEE, WPARAM(TRUE), LPARAM(FMarqueeSpeed));
end;

procedure TTaskDialogProgressBar.SetMax(AValue: Integer);
begin
  if (FMax = AValue) then Exit;
  SetRange(FMin, AValue);
end;

procedure TTaskDialogProgressBar.SetMin(AValue: Integer);
begin
  if FMin = AValue then Exit;
  SetRange(AValue, FMax);
end;

procedure TTaskDialogProgressBar.SetRange(AMin, AMax: Integer);
var
  Res: LRESULT;
  Err: Integer;
begin
  if (AMin = FMin) and (AMax = FMax) then
    Exit;
  FMin := AMin;
  FMax := AMax;
  //several sources indicate that FMax must be <= High(Word)
  //testing reveals that FMin must be >= 0 (Vista+ native dialog)
  FMax := EnsureRange(FMax, Integer(0), High(Word));
  FMin := EnsureRange(FMin, Integer(0), High(Word));
  if (FMin > FMax) then
    FMin := FMax;
  FPosition := EnsureRange(FPosition, FMin, FMax); //the actual progressbar in the dialog will adjust this as well
  if (tfShowProgressBar in Dlg.Flags) and (Dlg.Handle <> 0) then
  begin
    Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_RANGE, 0, MAKELPARAM(Word(FMin), Word(FMax)));   //Zero indicates failure
    debugln(['TTaskDialogProgressBar.SetRange: TDM_SET_PROGRESS_BAR_RANGE, FMin=',FMin,', FMax=',FMax,', MAKELPARAM=',MAKELPARAM(Word(FMin), Word(FMax)),': LResult=',PtrInt(Res)]);
    if (Res = 0) then
    begin
      Err := GetLastOSError;
      debugln(['Err=',Err,': ',SysErrorMessage(Err)]);
    end
    else
    begin
      debugln(['Old Min=',LoWord(Res),', Old Max=',HiWord(Res)]);
    end;
  end;
end;

procedure TTaskDialogProgressBar.SetPosition(AValue: Integer);
var
  Res: LRESULT;
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
  if (FPosition < FMin) then
    FPosition := FMIn
  else
    if (FPosition > FMax) then
      FPosition := FMax;
  if (tfShowProgressBar in Dlg.Flags) and (Dlg.Handle <> 0) then
  begin
    Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_POS, WPARAM(FPosition), 0);
    debugln(['TTaskDialogProgressBar.SetPosition: old Position=',PtrInt(Res)]);
  end;
end;

procedure TTaskDialogProgressBar.SetState(AValue: TProgressBarState);
var
  Res: LRESULT;
  Err: Integer;
begin
  if FState = AValue then Exit;
  FState := AValue;
  if (tfShowProgressBar in Dlg.Flags) and (Dlg.Handle <> 0) then
  begin
    Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_STATE, WPARAM(ProgressBarStateValues[FState]), 0);
    debugln(['TTaskDialogProgressBar.SetState: LResult=',PtrInt(Res)]);  //Zero iindicates failure
    if (Res = 0) then
    begin
      Err := GetLastOSError;
      debugln(['Err=',Err,': ',SysErrorMessage(Err)]);
    end;
  end;
end;

constructor TTaskDialogProgressBar.Create(ADialog: TCustomTaskDialog);
begin
  inherited Create;
  Dlg := ADialog;
  FMin := PB_DEFMIN;
  FMax := PB_DEFMAX;
  FMarqueeSpeed := 0;
end;

procedure TTaskDialogProgressBar.Initialize;
//this should be invoked as soon as the dialog has been instatiated (so Dlg.Handle <> 0),
//so probably in Dlg.DoOnConstructed??
var
  Res: LRESULT;
  Err: Integer;
  IsMarquee: BOOL;
begin
  //ToDo:
  //  send TDM_SET_MARQUEE_PROGRESS_BAR to Dlg (differentiates between "normal" and marguee)
  //  send range, position and state to dialog.
  if (Dlg.Handle <> 0) and (Dlg.Flags * [tfShowProgressBar,tfShowMarqueeProgressBar] <> []) then
  begin
    IsMarquee := (tfShowMarqueeProgressBar in Dlg.Flags);
    debugln(['TTaskDialogProgressBar.Initialize: IsMarquee=',IsMarquee]);
    //wParam = 0 (nonMarque) wParam != 0 (Marquee), lParam must be 0
    //if Both Flags are set, Windows seems to intepret this a Marquee
    Res := SendMessage(Dlg.Handle, TDM_SET_MARQUEE_PROGRESS_BAR, WPARAM(IsMarquee), 0);  //Result is ignored
    debugln(['TTaskDialogProgressBar.Initialize: TDM_SET_MARQUEE_PROGRESS_BAR: LResult=',PtrInt(Res)]);

    if not IsMarquee then
    begin
      //wParam must be 0, lParam = MAKELPARAM(nMinRange, nMaxRange)
      Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_RANGE, 0, MAKELPARAM(Word(FMin), Word(FMax)));   //Zero indicates failure
      debugln(['TTaskDialogProgressBar.Initialize: TDM_SET_PROGRESS_BAR_RANGE (FMin=,',FMin,', FMax=',FMax,'): LResult=',PtrInt(Res)]);
      if (Res = 0) then
      begin
        Err := GetLastOSError;
        debugln(['Err=',Err,': ',SysErrorMessage(Err)]);
      end
      else
      begin
        debugln(['Old Min=',LoWord(Res),', Old Max=',HiWord(Res)]);
      end;

      //wParam = new position, lParam must be 0, return value is previous position
      Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_POS, WPARAM(FPosition), 0);
      debugln(['TTaskDialogProgressBar.Initialize: TDM_SET_PROGRESS_BAR_POS: LResult=',PtrInt(Res)]);

    end//Not IsMarquee
    else
    begin //IsMarquee
      // wParam = False (stop marquee), wParam = True (start marquee), lparam = speed (milliseconds between repaints) // Return value is ignored
      SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_MARQUEE, WPARAM(TRUE), LPARAM(FMarqueeSpeed));
    end;

    //wParam = new progress state, lParam must be 0
    Res := SendMessage(Dlg.Handle, TDM_SET_PROGRESS_BAR_STATE, WPARAM(ProgressBarStateValues[FState]), 0);
    debugln(['TTaskDialogProgressBar.Initialize: TDM_SET_PROGRESS_BAR_STATE: LResult=',PtrInt(Res)]);  //Zero iindicates failure
    if (Res = 0) then
    begin
      Err := GetLastOSError;
      debugln(['Err=',Err,': ',SysErrorMessage(Err)]);
    end;

  end;
end;

{ TTaskDialogRadioButtonItem }

constructor TTaskDialogRadioButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  Caption := 'RadioButton'+IntToStr(ID+1);
end;

{ TTaskDialogButtonItem }

constructor TTaskDialogButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  Caption := 'Button'+IntToStr(ID+1);
end;

{ TCustomTaskDialog }

constructor TCustomTaskDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FButtons := TTaskDialogButtons.Create(Self, TTaskDialogButtonItem);
  FRadioButtons := TTaskDialogButtons.Create(Self, TTaskDialogRadioButtonItem);

  FCommonButtons := [tcbOk, tcbCancel];
  FDefaultButton := tcbOk;
  FFlags := [tfAllowDialogCancellation];
  FFooterIcon := tdiNone;
  FMainIcon := tdiInformation;

  FQueryChoices := TStringList.Create;

  FCustomFooterIcon := TIcon.Create;
  FCustomMainIcon := TIcon.Create;
  FProgressBar := TTaskDialogProgressBar.Create(Self);

  FHandle := 0;
end;

function TCustomTaskDialog.ButtonIDToModalResult(const AButtonID: Integer
  ): TModalResult;
begin
  if AButtonID<TaskDialogFirstButtonIndex then
    begin
      case AButtonID of
        IDOK: Result := mrOK;
        IDCANCEL: Result := mrCancel;
        IDABORT: Result := mrAbort;
        IDRETRY: Result := mrRetry;
        IDIGNORE: Result := mrIgnore;
        IDYES: Result := mrYes;
        IDNO: Result := mrNo;
        IDCLOSE: Result := mrClose;
        else Result := AButtonID
      end;
    end
  else if (AButtonID-TaskDialogFirstButtonIndex<Buttons.Count) then
    Result := Buttons[AButtonID-TaskDialogFirstButtonIndex].ModalResult
  else
    Result := mrNone;
end;

class procedure TCustomTaskDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterTaskDialog;
end;


destructor TCustomTaskDialog.Destroy;
begin
  FButtons.Free;
  FRadioButtons.Free;
  FQueryChoices.Free;
  FCustomFooterIcon.Free;
  FCustomMainIcon.Free;
  FProgressBar.Free;
  inherited Destroy;
end;

function TCustomTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
var
  DefRB, DefBtn: TModalResult;
  B: TTaskDialogBaseButtonItem;
  ButtonID, ARadioRes: Integer;
const
  TD_BTNMOD: array[TTaskDialogCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);
begin
  FQueryResult := '';
  ButtonID := TWSTaskDialogClass(WidgetSetClass).Execute(Self, ParentWnd, ARadioRes);
  FModalResult := ButtonIDToModalResult(ButtonID);
  //Note: this behaviour is inconsistent with all other dialogs, where a ModalResult of mrCancel would give a return value of False.
  //It also does not conform to Delhi's offial documentation
  //(https://docwiki.embarcadero.com/Libraries/Alexandria/en/Vcl.Dialogs.TCustomTaskDialog.Execute)
  //But it seems that Delphi in fact does the same: it will always return True, as long as we succeed in showing the dialog.
  Result := (ButtonID >= 0);
  SetRadioButtonFromRadioIndex(aRadioRes);
  //debugln(['TWSTaskDialogClass(WidgetSetClass).Execute(Self)=',ButtonID,', Result=',Result]);
  //debugln([' ButtonID=',ButtonID]);
  //debugln([' FModalResult=',FModalResult]);
  //debugln([' VerifyChecked=',tfVerificationFlagChecked in FFlags]);
  //debugln([' ARadioRes=',ARadioRes]);
  //debugln([' Assigned(FRadioButton)=',Assigned(FRadioButton)]);
  //debugln([' QueryItemIndex=',QueryItemIndex]);
  //debugln([' QueryResult=',QueryResult]);
end;

procedure TCustomTaskDialog.DoOnButtonClicked(AModalResult: Integer;
  var ACanClose: Boolean);
begin
  if Assigned(FOnButtonClicked) then
    FOnButtonClicked(Self, AModalResult, ACanClose);
end;

procedure TCustomTaskDialog.DoOnRadioButtonClicked(ButtonID: Integer);
begin
  if Assigned(FOnRadioButtonClicked) then
  begin
    SetRadioButtonFromRadioIndex(ButtonID);
    FOnRadioButtonClicked(Self);
  end;
end;

procedure TCustomTaskDialog.DoOnDialogConstructed;
begin
  if Assigned(FOnDialogConstructed) then
    FOnDialogConstructed(Self);
  FProgressBar.Initialize;
end;

procedure TCustomTaskDialog.DoOnDialogCreated;
begin
  if Assigned(FOnDialogCreated) then
    FOnDialogCreated(Self);
end;

procedure TCustomTaskDialog.DoOnDialogDestroyed;
begin
  if Assigned(FOnDialogDestroyed) then
    FOnDialogDestroyed(Self);
end;

procedure TCustomTaskDialog.DoOnExpandButtonClicked(Expanded: Boolean);
begin
  FExpanded := Expanded;
  if Assigned(FOnExpanded) then
    FOnExpanded(Self)
end;

procedure TCustomTaskDialog.DoOnTimer(TickCount: Cardinal; var Reset: Boolean);
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self, TickCount, Reset);
end;

procedure TCustomTaskDialog.DoOnVerificationClicked(Checked: Boolean);
begin
  if Checked then
    Include(FFlags, tfVerificationFlagChecked)
  else
    Exclude(FFlags, tfVerificationFlagChecked);
  if Assigned(FOnVerificationClicked) then
    FOnVerificationClicked(Self);
end;

procedure TCustomTaskDialog.DoOnHelp;
begin
  if Assigned(FonHelp) then
    FOnHelp(Self);
end;

procedure TCustomTaskDialog.DoOnHyperlinkClicked(const AURL: string);
begin
  FURL := AURL;
  if Assigned(FOnHyperlinkClicked) then
    FOnHyperlinkClicked(Self);
end;

procedure TCustomTaskDialog.DoOnNavigated;
begin
  if Assigned(FOnNavigated) then
    FOnNavigated(Self);
end;

procedure TCustomTaskDialog.InternalSetDialogHandle(AHandle: THandle);
begin
  FHandle := AHandle;
end;

procedure TCustomTaskDialog.SetRadioButtonFromRadioIndex(AIndex: Integer);
begin
  if (AIndex >= TaskDialogFirstRadioButtonIndex) and (AIndex-TaskDialogFirstRadioButtonIndex < RadioButtons.Count) then
    FRadioButton := RadioButtons[AIndex-TaskDialogFirstRadioButtonIndex] as TTaskDialogRadioButtonItem
  else
    FRadioButton := nil;
end;


function TCustomTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FModalResult := 0;
  Result := DoExecute(ParentWnd);
end;

function TCustomTaskDialog.Execute: Boolean;
var
  AParentWnd: HWND;
begin
  //See: https://docwiki.embarcadero.com/Libraries/Alexandria/en/Vcl.Dialogs.TCustomTaskDialog.Execute
  //If no handle is supplied, then the handle of the active form should be used.
  //For that Delphi uses Application.ActiveFormHandle (the docs say so).
  //Currently TApplication does not have a ActiveFormHandle property, so we use our own code for that.
  if Assigned(Screen.ActiveCustomForm) then
    AParentWnd := Screen.ActiveCustomForm.Handle
  else
    AParentWnd := 0;
  Result := Execute(AParentWnd);
end;

procedure TCustomTaskDialog.SetButtons(const Value: TTaskDialogButtons);
begin
  if FButtons=Value then Exit;
  FButtons.Assign(Value);
end;

function TCustomTaskDialog.IsCustomFooterIconStored: Boolean;
begin
  Result := Assigned(FCustomFooterIcon) and not FCustomFooterIcon.Empty;
end;

function TCustomTaskDialog.IsCustomMainIconStored: Boolean;
begin
  Result := Assigned(FCustomMainIcon) and not FCustomMainIcon.Empty;
end;

procedure TCustomTaskDialog.SetCustomFooterIcon(AValue: TIcon);
begin
  FCustomFooterIcon.Assign(AValue);
end;

procedure TCustomTaskDialog.SetCustomMainIcon(AValue: TIcon);
begin
  FCustomMainIcon.Assign(AValue);
end;

function DbgS(aFlag: TTaskDialogFlag): String; overload;
begin
  WriteStr(Result, aFlag);
end;

function DbgS(Flags: Dialogs.TTaskDialogFlags): String; overload;
var
  aFlag: Dialogs.TTaskDialogFlag;
begin
  Result := '[';
  for aFlag in Dialogs.TTaskDialogFlag do
    if (aFlag in Flags) then
      Result := Result + DbgS(aFlag) + ',';
  if Result[Length(Result)] = ',' then
    SetLength(Result, Length(Result)-1);
  Result := Result + ']';
end;

procedure TCustomTaskDialog.SetFlags(AValue: TTaskDialogFlags);
begin
  if FFlags = AValue then Exit;
  if (AValue * [tfQuery, tfSimpleQuery] = [tfQuery, tfSimpleQuery]) then
  begin
    if (tfQuery in FFlags) then
      Exclude(AValue, tfQuery)
    else
      Exclude(AValue, tfSimpleQuery);
  end;
  FFlags := AValue;
end;

procedure TCustomTaskDialog.SetQueryChoices(AValue: TStrings);
begin
  FQueryChoices.Assign(AValue);
end;

procedure TCustomTaskDialog.SetRadioButtons(const Value: TTaskDialogButtons);
begin
  if FRadioButtons=Value then Exit;
  FRadioButtons.Assign(Value);
end;

{ TTaskDialogBaseButtonItem }

constructor TTaskDialogBaseButtonItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FClient := Collection.Owner as TCustomTaskDialog;
  FModalResult := TaskDialogFirstButtonIndex + ID;
end;

function TTaskDialogBaseButtonItem.GetDefault: Boolean;
begin
  Result := TaskButtonCollection.DefaultButton = Self;
end;

function TTaskDialogBaseButtonItem.GetDisplayName: TTranslateString;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TTaskDialogBaseButtonItem.SetCaption(const ACaption: TTranslateString);
begin
  if FCaption = ACaption then Exit;
  FCaption := ACaption;
end;

procedure TTaskDialogBaseButtonItem.SetDefault(const Value: Boolean);
begin
  if Value then
    TaskButtonCollection.DefaultButton := Self
  else if TTaskDialogButtons(Collection).DefaultButton = Self then
    TaskButtonCollection.DefaultButton := nil;
end;

function TTaskDialogBaseButtonItem.TaskButtonCollection: TTaskDialogButtons;
begin
  Result := TTaskDialogButtons(Collection);
end;
