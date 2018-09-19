{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TFindDialog
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


type
  { TFindDialogForm }

  TFindDialogForm = class(TForm)
  // Same order as in TFindDialog
    BtnPanel: TPanel;
    CancelButton: TButton;
    CaseSensitiveCheckBox: TCheckBox;
    DirectionRadioGroup: TRadioGroup;
    EditFind: TEdit;
    EntireScopeCheckBox: TCheckBox;
    FindButton: TButton;
    FindLabel: TLabel;
    FlagsPanel: TPanel;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;

    procedure EditFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FButtonsBottom: Boolean;
  public
    constructor CreateNew(aOwner: TComponent; Options: TFindOptions); reintroduce;
    procedure SetLayout (Options: TFindOptions);
  end;

procedure TFindDialogForm.EditFindChange(Sender: TObject);
begin
  FindButton.Enabled:=EditFind.Text<>'';
end;

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  EditFindChange(nil);
end;

constructor TFindDialogForm.CreateNew(aOwner: TComponent; Options: TFindOptions
  );
var
  Layout: boolean; // true = Buttons at bottom
begin
  inherited CreateNew(aOwner);
  Layout := frButtonsAtBottom in Options;

  Left := 417;
  Top := 333;

  if Layout then begin
    Height := 166;
    Width := 415;
    ClientHeight := 166;
    ClientWidth := 415;
    Constraints.MinHeight := 166;
    Constraints.MaxHeight := 166; // fixed height
    Constraints.MinWidth := 415;
  end
  else begin
    Height := 136;
    Width := 480;
    ClientHeight := 140;
    ClientWidth := 480;
    Constraints.MinHeight := 136;
    Constraints.MinWidth := 480;
  end;

  AutoSize := True;
  BorderIcons := [biSystemMenu, biHelp];
  Caption := 'Find';
  //OnCreate := @FormCreate;
  //Position := poMainFormCenter;

  LCLVersion := '1.3';

  FindLabel := TLabel.Create(Self);
  with FindLabel do begin
    Left := 6;
    Height := 17;
    Top := 15;
    Width := 77;
    BorderSpacing.Right := 2;
    Caption := 'Text to find';
    FocusControl := EditFind;
    ParentColor := False;
    Parent := Self;
  end;
  EditFind := TEdit.Create(Self);
  with EditFind do begin
    Left := 85;
    Height := 27;
    Top := 10;
    Width := 303;
    TabOrder := 0;
    Text := 'EditFind';
    Parent := Self;
  end;
  DirectionRadioGroup := TRadioGroup.Create(Self);
  with DirectionRadioGroup do begin
    Left := 155;
    Height := 45;
    Top := 49;
    Width := 227;
    AutoFill := True;
    AutoSize := True;
    BorderSpacing.Left := 12;
    //BorderSpacing.Around := 0;
    Caption := 'Direction';
    ChildSizing.LeftRightSpacing := 6;
    ChildSizing.TopBottomSpacing := 6;
    ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
    ChildSizing.EnlargeVertical := crsHomogenousChildResize;
    ChildSizing.ShrinkHorizontal := crsScaleChilds;
    ChildSizing.ShrinkVertical := crsScaleChilds;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.ControlsPerLine := 2;
    ClientHeight := 48;
    ClientWidth := 217;
    Columns := 2;
    Constraints.MaxHeight := 60;
    Items.Add('Forward');
    Items.Add('Backward');
    TabOrder := 1;
    Parent := Self;
  end;
  FlagsPanel := TPanel.Create(Self);
  With FlagsPanel do begin
    Left := 6;
    Height := 82;
    Top := 43;
    Width := 143;
    AutoSize := True;
    BorderSpacing.Around := 6;
    BevelOuter := bvNone;
    ChildSizing.VerticalSpacing := 6;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.ControlsPerLine := 1;
    ClientHeight := 82;
    ClientWidth := 143;
    TabOrder := 2;
    Parent := Self;
  end;
  // Flags Panel Objects
    WholeWordsOnlyCheckBox := TCheckBox.Create(Self);
    with WholeWordsOnlyCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 0;
      Width := 142;
      Caption := 'Whole words only';
      TabOrder := 0;
      Parent := FlagsPanel;
    end;
    CaseSensitiveCheckBox := TCheckBox.Create(Self);
    with CaseSensitiveCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 29;
      Width := 142;
      Caption := 'Case sensitive';
      TabOrder := 1;
      Parent := FlagsPanel;
    end;
    EntireScopeCheckBox := TCheckBox.Create(Self);
    with EntireScopeCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 58;
      Width := 142;
      Caption := 'Search entire file';
      TabOrder := 2;
      Parent := FlagsPanel;
    end;
  //End of Flags Panel objects
  BtnPanel := TPanel.Create(Self);
  with BtnPanel do begin
    Left := 388;
    Height := 132;
    Top := 0;
    Width := 92;
    Align := alRight;
    AutoSize := True;
    BevelOuter := bvNone;
    ChildSizing.LeftRightSpacing := 6;
    ChildSizing.TopBottomSpacing := 6;
    ChildSizing.VerticalSpacing := 6;
    ClientHeight := 132;
    ClientWidth := 92;
    TabOrder := 3;
    Parent := Self;
  end;
  // BtnPanel Objects
    FindButton := TButton.Create(Self);
    with FindButton do begin
      Left := 6;
      Height := 26;
      Top := 6;
      Width := 80;
      AutoSize := True;
      Caption := 'Find more';
      Constraints.MinWidth := 80;
      Default := True;
      TabOrder := 0;
      Parent := BtnPanel;
    end;
    CancelButton := TButton.Create(Self);
    with CancelButton do begin
      Left := 6;
      Height := 26;
      Top := 64;
      Width := 80;
      AutoSize := True;
      BorderSpacing.Bottom := 10;
      Cancel := True;
      Caption := 'Cancel';
      Constraints.MinWidth := 80;
      ModalResult := 2;
      TabOrder := 1;
      Parent := BtnPanel;
    end;
    HelpButton := TButton.Create(Self);
    with HelpButton do begin
      Left := 6;
      Height := 26;
      Top := 100;
      Width := 80;
      AutoSize := True;
      Caption := 'Help';
      Constraints.MinWidth := 80;
      TabOrder := 2;
      Parent := BtnPanel;
    end;

  EditFind.OnChange := @EditFindChange;
  SetLayout(Options);
end;

procedure TFindDialogForm.SetLayout(Options: TFindOptions);
begin
  FButtonsBottom := frButtonsAtBottom in Options;
  if FButtonsBottom then begin
    Height := 180;
    Width := 447;
    ClientHeight := 180;
    ClientWidth := 447;
    AutoSize := False;
    with BtnPanel do begin
      Left := 0;
      Height := 38;
      Top := 131;
      Width := 447;
      ClientHeight := 38;
      ClientWidth := 447;
      Align := alBottom;
      AutoSize := True;
    end;
    // Anchors
    FindLabel.Anchors := [akTop, akLeft];
    FindLabel.AnchorSideTop.Control := EditFind;
    FindLabel.AnchorSideTop.Side := asrCenter;

    EditFind.Width := 362;
    EditFind.Anchors := [akTop, akLeft, akRight];
    EditFind.AnchorSideLeft.Control := FindLabel;
    EditFind.AnchorSideLeft.Side := asrBottom;
    EditFind.AnchorSideRight.Control := TControl(Owner);
    EditFind.AnchorSideRight.Side := asrBottom;


    DirectionRadioGroup.Anchors := [akTop, akLeft];
    DirectionRadioGroup.AnchorSideLeft.Control := FlagsPanel;
    DirectionRadioGroup.AnchorSideLeft.Side := asrBottom;
    DirectionRadioGroup.AnchorSideTop.Control := FlagsPanel;

    FlagsPanel.Anchors := [akTop, akLeft];
    FlagsPanel.AnchorSideLeft.Control := TControl(Owner);
    FlagsPanel.AnchorSideTop.Control := EditFind;
    FlagsPanel.AnchorSideTop.Side := asrBottom;

    FindButton.Anchors := [akRight, akBottom];
    FindButton.AnchorSideRight.Control := BtnPanel;
    FindButton.AnchorSideRight.Side := asrBottom;
    FindButton.AnchorSideBottom.Control := BtnPanel;
    FindButton.AnchorSideBottom.Side := asrBottom;

    CancelButton.Left := 265;
    CancelButton.Top := 6;
    CancelButton.BorderSpacing.Bottom := 0;
    CancelButton.Anchors := [akRight, akBottom];
    CancelButton.AnchorSideRight.Control := FindButton;
    //CancelButton.AnchorSideRight.Side := asrBottom;
    CancelButton.AnchorSideBottom.Control := BtnPanel;
    CancelButton.AnchorSideBottom.Side := asrBottom;
    CancelButton.BorderSpacing.Right := 32;

    HelpButton.Anchors := [akLeft, akBottom];
    HelpButton.AnchorSideLeft.Control := BtnPanel;
    HelpButton.AnchorSideBottom.Control := BtnPanel;
    HelpButton.AnchorSideBottom.Side := asrBottom;
    end
  else begin
    with BtnPanel do begin
      Height := 132;
      Top := 0;
      Width := 92;
      Align := alRight;
    end;
    // Anchors
    FindLabel.AnchorSideTop.Control := EditFind;
    FindLabel.AnchorSideTop.Side := asrCenter;

    EditFind.Anchors := [akTop, akLeft, akRight];
    EditFind.AnchorSideLeft.Control := FindLabel;
    EditFind.AnchorSideLeft.Side := asrBottom;
    EditFind.AnchorSideRight.Control := BtnPanel;

    DirectionRadioGroup.Anchors := [akTop, akLeft, akRight, akBottom];
    DirectionRadioGroup.AnchorSideLeft.Control := FlagsPanel;
    DirectionRadioGroup.AnchorSideLeft.Side := asrBottom;
    DirectionRadioGroup.AnchorSideTop.Control := FlagsPanel;
    DirectionRadioGroup.AnchorSideRight.Control := BtnPanel;
    DirectionRadioGroup.AnchorSideBottom.Control := FlagsPanel;
    DirectionRadioGroup.AnchorSideBottom.Side := asrBottom;

    FlagsPanel.AnchorSideLeft.Control := TControl(Owner);
    FlagsPanel.AnchorSideTop.Control := EditFind;
    FlagsPanel.AnchorSideTop.Side := asrBottom;

    FindButton.Anchors := [akTop, akRight];
    FindButton.AnchorSideTop.Control := BtnPanel;
    FindButton.AnchorSideRight.Control := BtnPanel;
    FindButton.AnchorSideRight.Side := asrBottom;
    FindButton.AnchorSideBottom.Control := CancelButton;

    CancelButton.Anchors := [akRight, akBottom];
    CancelButton.AnchorSideTop.Side := asrBottom;
    CancelButton.AnchorSideRight.Control := BtnPanel;
    CancelButton.AnchorSideRight.Side := asrBottom;
    CancelButton.AnchorSideBottom.Control := HelpButton;

    HelpButton.Anchors := [akLeft, akRight, akBottom];
    HelpButton.AnchorSideLeft.Control := BtnPanel;
    HelpButton.AnchorSideRight.Control := BtnPanel;
    HelpButton.AnchorSideBottom.Control := BtnPanel;
    HelpButton.AnchorSideBottom.Side := asrBottom;
  end;
end;

{ TFindDialog }

function TFindDialog.GetReplaceText: string;
begin
  Result:=FReplaceText;
end;

procedure TFindDialog.SetReplaceText(const AValue: string);
begin
  if FReplaceText = AValue then exit;
  FReplaceText:=AValue;
end;

procedure TFindDialog.FindClick(Sender: TObject);
begin
  GetFormValues;
  FOptions := FOptions - [frReplace, frReplaceAll] + [frFindNext];
  Find;
end;

procedure TFindDialog.HelpClick(Sender: TObject);
begin
  Help;
end;

procedure TFindDialog.CancelClick(Sender: TObject);
begin
  CloseDialog;
end;

function TFindDialog.GetHeight: Integer;
begin
  if Assigned(FFindForm) then
    Result := FFindForm.Height
  else
    Result := inherited GetHeight;
end;

function TFindDialog.GetWidth: Integer;
begin
  if Assigned(FFindForm) then
    Result := FFindForm.Width
  else
  Result := inherited GetWidth;
end;

procedure TFindDialog.DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  if csDesigning in ComponentState then exit;
  FFormTop := FFindForm.Top;
  FFormLeft := FFindForm.Left;
  if Assigned(OnClose) then OnClose(Self);
end;

procedure TFindDialog.DoShowForm(Sender: TObject);
begin
  if csDesigning in ComponentState then exit;
  if Assigned(OnShow) then OnShow(Self);
end;

function TFindDialog.GetFindText: string;
begin
  Result:=FFindText;
end;

function TFindDialog.GetLeft: Integer;
begin
  if Assigned(FFindForm) then
    FFormLeft := FFindForm.Left;
  Result:=FFormLeft;
end;

function TFindDialog.GetPosition: TPoint;
begin
  Result:=Point(GetLeft, GetTop);
end;

function TFindDialog.GetTop: Integer;
begin
  if Assigned(FFindForm) then
    FFormTop := FFindForm.Top;
  Result:=FFormTop;
end;

procedure TFindDialog.SetFindText(const AValue: string);
begin
  if FFindText = AValue then exit;
  FFindText:=AValue;
  If Assigned(FFindForm) then
    TFindDialogForm(FFindForm).EditFind.Text := AValue;
end;

procedure TFindDialog.SetLeft(const AValue: Integer);
begin
  if FFormLeft = AValue then Exit;
  FFormLeft:=AValue;
  if Assigned(FFindForm) then FFindForm.Left :=FFormLeft;
end;

procedure TFindDialog.SetOptions(AValue: TFindOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  If Assigned(FFindForm) then begin
    TFindDialogForm(FFindForm).EntireScopeCheckBox.Checked    := (frEntireScope in FOptions);
    TFindDialogForm(FFindForm).WholeWordsOnlyCheckBox.Checked := (frWholeWord in FOptions);
    TFindDialogForm(FFindForm).CaseSensitiveCheckBox.checked  := (frMatchCase in FOptions);
  end;
end;

procedure TFindDialog.SetPosition(const AValue: TPoint);
begin
  if (GetLeft <> AValue.x) then Left := AValue.x;
  If (GetTop <> AValue.y) then  Top := AValue.y;
end;

procedure TFindDialog.SetTop(const AValue: Integer);
begin
  if FFormTop = AValue then exit;
  FFormTop:=AValue;
  if Assigned(FFindForm) then FFindForm.Top :=FFormTop;
end;

procedure TFindDialog.Find;
begin
  if Assigned(FOnFind) then
    FOnFind(Self);
end;

procedure TFindDialog.Help;
begin
  if Assigned(FOnHelpClicked) then
    FOnHelpClicked(Self);
end;

procedure TFindDialog.Replace;
begin
  if Assigned(FOnReplace) then
    FOnReplace(Self);
end;

function TFindDialog.CreateForm: TForm;
begin
  // do not use Self as Owner, otherwise as desgntime this will not work
  //debugln(['TFindDialog.CreateForm: Left=',Left,', Top=',Top,', Position=',dbgs(position),', FFormLeft=',FFormLeft,', FFormTop=',FFormTop]);
  Result := TFindDialogForm.CreateNew(nil,Options);
  with TFindDialogForm(Result) do
  begin
    FindButton.Caption := rsFind;
    CancelButton.Caption := rsMbCancel;
    HelpButton.Caption := rsHelp;
    WholeWordsOnlyCheckBox.Caption := rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption := rsCaseSensitive;
    EntireScopeCheckBox.Caption := rsEntireScope;
    FindLabel.Caption := rsText;
    DirectionRadioGroup.Caption := rsDirection;
    DirectionRadioGroup.Items[0] := rsForward;
    DirectionRadioGroup.Items[1] := rsBackward;
    //Setting up button eventhandlers
    FindButton.OnClick := @FindClick;
    HelpButton.OnClick := @HelpClick;
    CancelButton.OnClick := @CancelClick;
    PopupMode := pmAuto;
    // Init local property with default value
    Result.Top := FFormTop;
    Result.Left := FFormLeft;
  end;
end;

procedure TFindDialog.SetFormValues;
var
  Dlg: TFindDialogForm;
begin
  Dlg:=TFindDialogForm(FFindForm);
  Dlg.EditFind.Text:=FFindText;
  Dlg.EditFind.SelectAll;
  Dlg.ActiveControl:=Dlg.EditFind;

  Dlg.WholeWordsOnlyCheckBox.Checked:=frWholeWord in Options;
  Dlg.CaseSensitiveCheckBox.Checked:=frMatchCase in Options;
  Dlg.EntireScopeCheckBox.Checked:=frEntireScope in Options;
  Dlg.DirectionRadioGroup.ItemIndex:=ord(not(frDown in Options));

  Dlg.WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  Dlg.CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  Dlg.DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  Dlg.WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  Dlg.CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  Dlg.DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  Dlg.HelpButton.Visible:=(frShowHelp in Options);
  Dlg.EntireScopeCheckBox.Visible := not (frHideEntireScope in Options);
end;

procedure TFindDialog.GetFormValues;
var
  Dlg: TFindDialogForm;
begin
  Dlg:=TFindDialogForm(FFindForm);
  if Dlg.DirectionRadioGroup.ItemIndex = 0 then
    FOptions:=FOptions + [frDown]
  else
    FOptions:=FOptions - [frDown];
  if Dlg.WholeWordsOnlyCheckBox.Checked then
    FOptions:=FOptions + [frWholeWord]
  else
    FOptions:=FOptions - [frWholeWord];
  if Dlg.CaseSensitiveCheckBox.Checked then
    FOptions:=FOptions + [frMatchCase]
  else
    FOptions:=FOptions - [frMatchCase];
  if Dlg.EntireScopeCheckBox.Checked then
    FOptions:=FOptions + [frEntireScope]
  else
    FOptions:=FOptions - [frEntireScope];
  FFindText := Dlg.EditFind.Text;
end;

procedure TFindDialog.CalcPosition(aForm:Tform);
begin
  //debugln(['TFindDialog.CalcPosition: Left=',Left,', Top=',Top,', Position=',dbgs(position),', FFormLeft=',FFormLeft,', FFormTop=',FFormTop]);
  if (csDesigning in ComponentState) then
  begin
    aForm.Position := poScreenCenter;
    Exit;
  end;
  if (FFormLeft < 0) and (FFormTop < 0) then
  begin
    aForm.Position := poMainFormCenter;
    FFormTop := aForm.Top;
    FFormLeft := aForm.Left;
  end;
end;


constructor TFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions:=[frDown];
  FFormLeft := -1;
  FFormTop := -1;
end;

destructor TFindDialog.Destroy;
begin
  FreeAndNil(FFindForm);
  inherited Destroy;
end;

procedure TFindDialog.CloseDialog;
begin
  if Assigned(FFindForm) then
    FFindForm.Close;
end;

function TFindDialog.Execute: Boolean;
begin
  Result := False;
  if not Assigned(FFindForm) then
    FFindForm:=CreateForm;
    
  if Assigned(FFindForm) then begin
    CalcPosition(FFindForm);

    SetFormValues;
    FFindForm.OnClose:=@DoCloseForm;
    FFindForm.OnShow := @DoShowForm;
    FFindForm.HelpContext:=HelpContext;
    FFindForm.Caption:=Title;
    FFindForm.Show;
    FHeight := FFindForm.Height;
    FWidth := FFindForm.Width;
    Result := true;
  end;
end;

function TFindDialog.DefaultTitle: string;
begin
  Result := rsFind;
end;

