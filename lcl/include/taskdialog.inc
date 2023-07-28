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

  FQueryChoices := TStringList.Create;
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
  if (ARadioRes >= TaskDialogFirstRadioButtonIndex) and (aRadioRes-TaskDialogFirstRadioButtonIndex < RadioButtons.Count) then
    FRadioButton := RadioButtons[ARadioRes-TaskDialogFirstRadioButtonIndex] as TTaskDialogRadioButtonItem
  else
    FRadioButton := nil;
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
