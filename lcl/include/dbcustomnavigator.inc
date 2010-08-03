{%MainUnit ../dbctrls.pp}
{
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

{ TDBNavButton }

destructor TDBNavButton.Destroy;
begin
  inherited Destroy;
end;

{ TDBNavDataLink }

procedure TDBNavDataLink.EditingChanged;
begin
  if FNavigator<>nil then FNavigator.EditingChanged;
end;

procedure TDBNavDataLink.DataSetChanged;
begin
  if FNavigator<>nil then FNavigator.DataChanged;
end;

procedure TDBNavDataLink.ActiveChanged;
begin
  if FNavigator<>nil then FNavigator.ActiveChanged;
end;

constructor TDBNavDataLink.Create(TheNavigator: TDBCustomNavigator);
begin
  inherited Create;
  FNavigator := TheNavigator;
  VisualControl := True;
end;

destructor TDBNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

{ TDBCustomNavigator }

procedure TDBCustomNavigator.DefaultHintsChanged(Sender: TObject);
var
  OldNotify: TNotifyEvent;
begin
  if FDefaultHints.Text = FOriginalHints then
    exit;
  
  OldNotify := TStringList(FHints).OnChange;
  TStringlist(FHints).OnChange := nil;
  TStringList(FDefaultHints).OnChange := nil;
  try
    FHints.Assign(FDefaultHints);
    FDefaultHints.Clear;
    UpdateHints;
  finally
    TStringlist(FHints).OnChange := OldNotify;
  end;
  
end;

function TDBCustomNavigator.GetDataSource: TDataSource;
begin
  Result:=FDataLink.DataSource;
end;

function TDBCustomNavigator.GetHints: TStrings;
begin
  if ([csDesigning,csWriting,csReading]*ComponentState=[csDesigning])
  and (FHints.Count=0) then
    Result:=FDefaultHints
  else
    Result:=FHints;
end;

procedure TDBCustomNavigator.SetDataSource(const AValue: TDataSource);
begin
  if AValue=DataSource then exit;
  ChangeDataSource(Self,FDataLink,AValue);
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TDBCustomNavigator.SetDirection(const AValue: TDBNavButtonDirection);
begin
  if FDirection=AValue then exit;
  FDirection:=AValue;
  if not (csLoading in ComponentState) then
    UpdateButtons;
end;

procedure TDBCustomNavigator.SetFlat(const AValue: Boolean);
var
  CurButton: TDBNavButtonType;
begin
  if FFlat=AValue then exit;
  FFlat:=AValue;
  for CurButton:=Low(Buttons) to High(Buttons) do
    Buttons[CurButton].Flat:=AValue;
end;

procedure TDBCustomNavigator.SetHints(const AValue: TStrings);
begin
  if (AValue=FDefaultHints) then exit;
  if (AValue.Text=FDefaultHints.Text) then
    FHints.Clear
  else
    FHints.Assign(AValue);
end;

procedure TDBCustomNavigator.SetShowButtonHints(const AValue: boolean);
begin
  if FShowButtonHints=AValue then exit;
  FShowButtonHints:=AValue;
  if not (csLoading in ComponentState) then
    UpdateHints;
end;

procedure TDBCustomNavigator.SetVisibleButtons(const AValue: TDBNavButtonSet);
var
  CurButton: TDBNavButtonType;
begin
  if FVisibleButtons=AValue then exit;
  FVisibleButtons:=AValue;
  for CurButton:=Low(Buttons) to High(Buttons) do
    Buttons[CurButton].Visible:=CurButton in FVisibleButtons;
  if not (csLoading in ComponentState) then
    UpdateButtons;
end;

procedure TDBCustomNavigator.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TDBCustomNavigator.DataChanged;
var
  PriorEnable, NextEnable: Boolean;
begin
  PriorEnable:=Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  NextEnable:=Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled:=PriorEnable;
  Buttons[nbPrior].Enabled:=PriorEnable;
  Buttons[nbNext].Enabled:=NextEnable;
  Buttons[nbLast].Enabled:=NextEnable;
  Buttons[nbDelete].Enabled:=Enabled and FDataLink.Active
                    and FDataLink.DataSet.CanModify
                    and (not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF));
end;

procedure TDBCustomNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify:=Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled:=CanModify;
  Buttons[nbEdit].Enabled:=CanModify and not FDataLink.Editing;
  Buttons[nbPost].Enabled:=CanModify and FDataLink.Editing;
  Buttons[nbCancel].Enabled:=CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled:=CanModify;
end;

procedure TDBCustomNavigator.ActiveChanged;
var
  CurButton: TDBNavButtonType;
begin
  if not (Enabled and FDataLink.Active) then begin
    for CurButton:=Low(Buttons) to High(Buttons) do
      Buttons[CurButton].Enabled:=False
  end else begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDBCustomNavigator.Loaded;
begin
  inherited Loaded;
  UpdateButtons;
  UpdateHints;
  ActiveChanged;
end;

procedure TDBCustomNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then begin
    if (FDataLink<>nil) and (AComponent=DataSource) then
      DataSource:=nil;
  end;
end;

procedure TDBCustomNavigator.UpdateButtons;
var
  CurButtonType: TDBNavButtonType;
  CurButton: TDBNavButton;
begin
  if FUpdateButtonsLock>0
  then begin
    FUpdateButtonsNeeded:=true;
    exit;
  end;

  DisableAlign;
  FUpdateButtonsNeeded:=false;
  if Direction=nbdHorizontal then
    ChildSizing.Layout:=cclLeftToRightThenTopToBottom
  else
    ChildSizing.Layout:=cclTopToBottomThenLeftToRight;

  for CurButtonType:=Low(Buttons) to High(Buttons) do
  begin
    // create/get button
    if Buttons[CurButtonType]=nil
    then begin
      CurButton:=TDBNavButton.Create(Self);
      CurButton.Name:=DBNavButtonResourceName[CurButtonType];
      Buttons[CurButtonType]:=CurButton;
      if CurButtonType in [nbPrior,nbNext] then
        CurButton.NavStyle:=CurButton.NavStyle+[nsAllowTimer];
      CurButton.LoadGlyphFromLazarusResource(DBNavButtonResourceName[CurButtonType]);
      CurButton.NumGlyphs:=1;
      CurButton.Parent:=Self;
      CurButton.OnClick:=@ButtonClickHandler;
      CurButton.ControlStyle := CurButton.ControlStyle + [csNoDesignSelectable];
    end else
      CurButton:=Buttons[CurButtonType];

    // update button properties
    CurButton.Flat:=Flat;
    CurButton.Index:=CurButtonType;
    CurButton.Visible:=CurButtonType in FVisibleButtons;
  end;
  EnableAlign;
  ActiveChanged;
end;

procedure TDBCustomNavigator.UpdateHints;
var
  CurButton: TDBNavButtonType;
  DBNavButtonDefaultHint: array[TDBNavButtonType] of string;

  procedure AssignHintsCaptions;
  begin
    DBNavButtonDefaultHint[nbFirst] := rsFirstRecordHint;
    DBNavButtonDefaultHint[nbPrior] := rsPriorRecordHint;
    DBNavButtonDefaultHint[nbNext] := rsNextRecordHint;
    DBNavButtonDefaultHint[nbLast] := rsLastRecordHint;
    DBNavButtonDefaultHint[nbInsert] := rsInsertRecordHint;
    DBNavButtonDefaultHint[nbDelete] := rsDeleteRecordHint;
    DBNavButtonDefaultHint[nbEdit] := rsEditRecordHint;
    DBNavButtonDefaultHint[nbPost] := rsPostRecordHint;
    DBNavButtonDefaultHint[nbCancel] := rsCancelRecordHint;
    DBNavButtonDefaultHint[nbRefresh] := rsRefreshRecordsHint;
  end;

begin
  if (FDefaultHints.Count = 0) then
  begin
    TStringList(FDefaultHints).OnChange := nil;
    AssignHintsCaptions;
    for CurButton := Low(Buttons) to High(Buttons) do
      FDefaultHints.Add(DBNavButtonDefaultHint[CurButton]);
    FOriginalHints := FDefaultHints.Text;
    TStringList(FDefaultHints).OnChange := @DefaultHintsChanged;
  end;
  for CurButton := Low(Buttons) to High(Buttons) do
  begin
    if FHints.Count > Ord(CurButton) then
      Buttons[CurButton].Hint := FHints[Ord(CurButton)]
    else
      Buttons[CurButton].Hint := FDefaultHints[Ord(CurButton)];
    Buttons[CurButton].ShowHint := ShowButtonHints;
  end;
end;

procedure TDBCustomNavigator.HintsChanged(Sender: TObject);
begin
  UpdateHints;
end;

procedure TDBCustomNavigator.ButtonClickHandler(Sender: TObject);
begin
  BtnClick(TDBNavButton(Sender).Index);
end;

class function TDBCustomNavigator.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 241;
  Result.CY := 25;
end;

procedure TDBCustomNavigator.BeginUpdateButtons;
begin
  inc(FUpdateButtonsLock);
end;

procedure TDBCustomNavigator.EndUpdateButtons;
begin
  dec(FUpdateButtonsLock);
  if (FUpdateButtonsLock<0) then
    RaiseGDBException('TDBCustomNavigator.EndUpdateButtons');
  if (FUpdateButtonsLock=0) then begin
    if FUpdateButtonsNeeded then
      UpdateButtons;
  end;
end;

procedure TDBCustomNavigator.SetEnabled(Value: Boolean);
begin
  if Value<>Enabled then begin
    inherited SetEnabled(Value);
    if not (csLoading in ComponentState) then
      UpdateButtons;
  end;
end;

constructor TDBCustomNavigator.Create(TheOwner: TComponent);
begin
  BeginUpdateButtons;
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csAcceptsControls,csSetCaption]+[csOpaque];
  FDirection:=nbdHorizontal;
  ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine:=100;
  ChildSizing.EnlargeHorizontal:=crsScaleChilds;
  ChildSizing.ShrinkHorizontal:=crsScaleChilds;
  ChildSizing.EnlargeVertical:=crsScaleChilds;
  ChildSizing.ShrinkVertical:=crsScaleChilds;
  FDataLink:=TDBNavDataLink.Create(Self);
  FVisibleButtons:=DefaultDBNavigatorButtons;
  FHints:=TStringList.Create;
  FShowButtonHints:=true;
  TStringList(FHints).OnChange:=@HintsChanged;
  FDefaultHints:=TStringList.Create;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  FConfirmDelete:=True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  UpdateButtons;
  EndUpdateButtons;
  UpdateHints;
end;

destructor TDBCustomNavigator.Destroy;
begin
  FDataLink.Free;
  FDataLink:=nil;
  FHints.Free;
  FHints:=nil;
  FDefaultHints.Free;
  FDefaultHints:=nil;
  inherited Destroy;
end;

procedure TDBCustomNavigator.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource<>nil) and (DataSource.State<>dsInactive) then begin
    if not (csDesigning in ComponentState) and Assigned(BeforeAction) then
      BeforeAction(Self,Index);
    with DataSource.DataSet do begin
      case Index of
      nbPrior:  Prior;
      nbNext:   Next;
      nbFirst:  First;
      nbLast:   Last;
      nbInsert: Insert;
      nbEdit:   Edit;
      nbCancel: Cancel;
      nbPost:   Post;
      nbRefresh: Refresh;
      nbDelete:
        if (not ConfirmDelete)
        or (MessageDlg(rsDeleteRecord, mtConfirmation, mbOKCancel, 0 )<>
          mrCancel)
        then
          Delete;
      end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(OnClick) then
    OnClick(Self,Index);
end;

function TDBCustomNavigator.VisibleButtonCount: integer;
var
  CurButton: TDBNavButtonType;
begin
  Result:=0;
  for CurButton:=Low(Buttons) to High(Buttons) do
    if CurButton in FVisibleButtons then
      inc(Result);
end;

// included by dbctrls.pas
