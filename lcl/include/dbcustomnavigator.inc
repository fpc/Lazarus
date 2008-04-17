{%MainUnit ../dbctrls.pp}
{
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
  FDataLink.DataSource:=AValue;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if AValue <> nil then
    AValue.FreeNotification(Self);
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
  ButtonCount: Integer;
  ButtonNumber: Integer;
  ButtonStartPos: Integer;
  ButtonEndPos: Integer;
  ButtonSpace: Integer;
  X: Integer;
  Y: Integer;
  W: Integer;
  H: Integer;
begin
  if FUpdateButtonsLock>0
  then begin
    FUpdateButtonsNeeded:=true;
    exit;
  end;

  FUpdateButtonsNeeded:=false;
  ButtonCount:=VisibleButtonCount;
  if Direction=nbdHorizontal then
    ButtonSpace:=ClientWidth
  else
    ButtonSpace:=ClientHeight;

  ButtonNumber:=1;
  ButtonEndPos:=0;
  ButtonStartPos:=ButtonEndPos;
  for CurButtonType:=Low(Buttons) to High(Buttons) do
  begin
    // calculate button bounds
    if ButtonCount>0 then
      ButtonEndPos:=(ButtonSpace*ButtonNumber) div ButtonCount
    else
      ButtonEndPos:=ButtonStartPos+1;
    if Direction=nbdHorizontal then begin
      X:=ButtonStartPos;
      Y:=0;
      W:=ButtonEndPos-ButtonStartPos;
      H:=ClientHeight;
    end else begin
      X:=0;
      Y:=ButtonStartPos;
      W:=ClientWidth;
      H:=ButtonEndPos-ButtonStartPos;
    end;
    
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
    end else
      CurButton:=Buttons[CurButtonType];

    // update button properties
    CurButton.Flat:=Flat;
    CurButton.Enabled:=Enabled;
    CurButton.Index:=CurButtonType;
    CurButton.SetBounds(X,Y,W,H);
    CurButton.Visible:=CurButtonType in FVisibleButtons;
    CurButton.OnClick:=@ButtonClickHandler;
    CurButton.Parent:=Self;
    CurButton.ControlStyle := CurButton.ControlStyle + [csNoDesignSelectable];
    if CurButton.Visible then begin
      inc(ButtonNumber);
      ButtonStartPos:=ButtonEndPos;
    end;
  end;
end;

procedure TDBCustomNavigator.UpdateHints;

  function GetDBNavButtonHint(CurButton: TDBNavButtonType): string;
  begin
    case CurButton of
    // ToDo: convert to resourcestrings
    nbFirst:   Result:='First';
    nbPrior:   Result:='Prior';
    nbNext:    Result:='Next';
    nbLast:    Result:='Last';
    nbInsert:  Result:='Insert';
    nbDelete:  Result:='Delete';
    nbEdit:    Result:='Edit';
    nbPost:    Result:='Post';
    nbCancel:  Result:='Cancel';
    nbRefresh: Result:='Refresh';
    else
      RaiseGDBException('TDBCustomNavigator.InitHints');
    end;
  end;

var
  CurButton: TDBNavButtonType;
  i: Integer;
begin
  if (FDefaultHints.Count=0) then begin
    TStringList(FDefaultHints).OnChange:=nil;
    for CurButton:=Low(Buttons) to High(Buttons) do
      FDefaultHints.Add(GetDBNavButtonHint(CurButton));
    FOriginalHints := FDefaultHints.Text;
    TStringList(FDefaultHints).OnChange:=@DefaultHintsChanged;
  end;
  i:=0;
  for CurButton:=Low(Buttons) to High(Buttons) do begin
    if FHints.Count>i then
      Buttons[CurButton].Hint:=FHints[i]
    else
      Buttons[CurButton].Hint:=FDefaultHints[i];
    Buttons[CurButton].ShowHint:=ShowButtonHints;
    inc(i);
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

procedure TDBCustomNavigator.DoOnResize;
begin
  inherited DoOnResize;
  if (not (csLoading in ComponentState)) and (Buttons[nbFirst]<>nil) then
    UpdateButtons;
end;

class function TDBCustomNavigator.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=241;
  Result.Y:=25;
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
  FDataLink:=TDBNavDataLink.Create(Self);
  FVisibleButtons:=DefaultDBNavigatorButtons;
  FHints:=TStringList.Create;
  FShowButtonHints:=true;
  TStringList(FHints).OnChange:=@HintsChanged;
  FDefaultHints:=TStringList.Create;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  FConfirmDelete:=True;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
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
