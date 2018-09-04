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
end;

function TCustomTaskDialog.ButtonIDToModalResult(const AButtonID: Integer
  ): TModalResult;
begin
  if AButtonID<100 then
    Result := AButtonID
  else if (AButtonID-100<Buttons.Count) then
    Result := Buttons[AButtonID-100].ModalResult
  else
    Result := mrNone;
end;

destructor TCustomTaskDialog.Destroy;
begin
  FButtons.Free;
  FRadioButtons.Free;

  inherited Destroy;
end;

function TCustomTaskDialog.DoExecute(ParentWnd: HWND): Boolean;
  function TD_COMMONBUTTONS(const aButtons: TTaskDialogCommonButtons): LCLTaskDialog.TCommonButtons;
  begin
    Result := [];
    if tcbOk in aButtons then
      Result := Result + [cbOK];
    if tcbYes in aButtons then
      Result := Result + [cbYes];
    if tcbNo in aButtons then
      Result := Result + [cbNo];
    if tcbCancel in aButtons then
      Result := Result + [cbCancel];
    if tcbRetry in aButtons then
      Result := Result + [cbRetry];
    if tcbClose in aButtons then
      Result := Result + [cbClose];
  end;

  function TD_FLAGS(const aTaskFlags: TTaskDialogFlags): LCLTaskDialog.TTaskDialogFlags;
  begin
    Result := [];
    if tfEnableHyperlinks in aTaskFlags then
      Result := Result + [tdfEnableHyperlinks];
    if tfUseHiconMain in aTaskFlags then
      Result := Result + [tdfUseHIconMain];
    if tfUseHiconFooter in aTaskFlags then
      Result := Result + [tdfUseHIconFooter];
    if tfAllowDialogCancellation in aTaskFlags then
      Result := Result + [tdfAllowDialogCancellation];
    if tfUseCommandLinks in aTaskFlags then
      Result := Result + [tdfUseCommandLinks];
    if tfUseCommandLinksNoIcon in aTaskFlags then
      Result := Result + [tdfUseCommandLinksNoIcon];
    if tfExpandFooterArea in aTaskFlags then
      Result := Result + [tdfExpandFooterArea];
    if tfExpandedByDefault in aTaskFlags then
      Result := Result + [tdfExpandByDefault];
    if tfVerificationFlagChecked in aTaskFlags then
      Result := Result + [tdfVerificationFlagChecked];
    if tfShowProgressBar in aTaskFlags then
      Result := Result + [tdfShowProgressBar];
    if tfShowMarqueeProgressBar in aTaskFlags then
      Result := Result + [tdfShowMarqueeProgressBar];
    if tfCallbackTimer in aTaskFlags then
      Result := Result + [tdfCallbackTimer];
    if tfPositionRelativeToWindow in aTaskFlags then
      Result := Result + [tdfPositionRelativeToWindow];
    if tfRtlLayout in aTaskFlags then
      Result := Result + [tdfRtlLayout];
    if tfNoDefaultRadioButton in aTaskFlags then
      Result := Result + [tdfNoDefaultRadioButton];
    if tfCanBeMinimized in aTaskFlags then
      Result := Result + [tdfCanBeMinimized];
  end;

  function TF_DIALOGICON(const aIcon: TTaskDialogIcon): LCLTaskDialog.TTaskDialogIcon;
  begin
    case aIcon of
      tdiWarning: Result := LCLTaskDialog.TTaskDialogIcon.tiWarning;
      tdiError: Result := LCLTaskDialog.TTaskDialogIcon.tiError;
      tdiInformation: Result := LCLTaskDialog.TTaskDialogIcon.tiInformation;
      tdiShield: Result := LCLTaskDialog.TTaskDialogIcon.tiShield;
      tdiQuestion: Result := LCLTaskDialog.TTaskDialogIcon.tiQuestion;
    else
      Result := LCLTaskDialog.TTaskDialogIcon.tiBlank;
    end;
  end;

  function TF_FOOTERICON(const aIcon: TTaskDialogIcon): LCLTaskDialog.TTaskDialogFooterIcon;
  begin
    case aIcon of
      tdiWarning: Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiWarning;
      tdiError: Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiError;
      tdiInformation: Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiInformation;
      tdiShield: Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiShield;
      tdiQuestion: Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiQuestion;
    else
      Result := LCLTaskDialog.TTaskDialogFooterIcon.tfiBlank;
    end;
  end;
var
  TaskDlg: LCLTaskDialog.TTaskDialog;
  DefRB, DefBtn: TModalResult;
  B: TTaskDialogBaseButtonItem;
  ButtonID: Integer;
const
  TD_BTNMOD: array[TTaskDialogCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);
begin
  FillChar(TaskDlg, SizeOf(LCLTaskDialog.TTaskDialog), 0);

  if RadioButtons.DefaultButton<> nil then
    DefRB := RadioButtons.DefaultButton.ModalResult
  else
    DefRB := 0;
  if Buttons.DefaultButton<>nil then
    DefBtn := Buttons.DefaultButton.ModalResult
  else
    DefBtn := TD_BTNMOD[DefaultButton];

  for B in Buttons do
    TaskDlg.Buttons := TaskDlg.Buttons + B.Caption + #10;
  for B in RadioButtons do
    TaskDlg.Radios := TaskDlg.Radios + B.Caption + #10;

  TaskDlg.Title := Caption;
  TaskDlg.Inst := Title;
  TaskDlg.Content := Text;
  TaskDlg.InfoCollapse := ExpandButtonCaption;
  TaskDlg.Info := ExpandedText;
  TaskDlg.Footer := FooterText;
  TaskDlg.Verify := VerificationText;

  ButtonID := TaskDlg.Execute(TD_COMMONBUTTONS(CommonButtons), DefBtn, TD_FLAGS(Flags), TF_DIALOGICON(MainIcon), TF_FOOTERICON(FooterIcon),
    DefRB, 0, ParentWnd, False, False, @DoOnButtonClickedHandler);
  Result := ButtonID>=0;
  FModalResult := ButtonIDToModalResult(ButtonID);

  if (TaskDlg.RadioRes>=200) and (TaskDlg.RadioRes-200<RadioButtons.Count) then
    FRadioButton := RadioButtons[TaskDlg.RadioRes-200] as TTaskDialogRadioButtonItem
  else
    FRadioButton := nil;
  if TaskDlg.VerifyChecked then
    Include(FFlags, tfVerificationFlagChecked)
  else
    Exclude(FFlags, tfVerificationFlagChecked)
end;

procedure TCustomTaskDialog.DoOnButtonClicked(AModalResult: Integer;
  var ACanClose: Boolean);
begin
  if Assigned(FOnButtonClicked) then
    FOnButtonClicked(Self, AModalResult, ACanClose);
end;

procedure TCustomTaskDialog.DoOnButtonClickedHandler(Sender: PTaskDialog;
  AButtonID: integer; var ACanClose: Boolean);
begin
  DoOnButtonClicked(ButtonIDToModalResult(AButtonID), ACanClose)
end;

function TCustomTaskDialog.Execute(ParentWnd: HWND): Boolean;
begin
  FModalResult := 0;
  Result := DoExecute(ParentWnd);
end;

function TCustomTaskDialog.Execute: Boolean;
begin
  Result := Execute(0);
end;

procedure TCustomTaskDialog.SetButtons(const Value: TTaskDialogButtons);
begin
  if FButtons=Value then Exit;
  FButtons.Assign(Value);
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
  FModalResult := 100 + ID;
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
