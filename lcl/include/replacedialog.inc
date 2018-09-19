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

{ TReplaceDialogForm }
  TReplaceDialogForm = class(TForm)
    // Same order as in TReplaceDialog
	PanelButtons: TPanel;
	CancelButton: TButton;
	CaseSensitiveCheckBox: TCheckBox;
	DirectionRadioGroup: TRadioGroup;
    EditFind: TEdit;
	EntireScopeCheckBox: TCheckBox;
	FindMoreButton: TButton;
	TextLabel: TLabel;
	FlagPanel: TPanel;
	HelpButton: TButton;
	WholeWordsOnlyCheckBox: TCheckBox;

    EditReplace: TEdit;
    PromptOnReplaceCheckBox: TCheckBox;
    ReplaceAllButton: TButton;
    ReplaceButton: TButton;
    ReplaceLabel: TLabel;
  private
    FButtonsBottom: Boolean;
  public
    { public declarations }
    constructor CreateNew(aOwner: TComponent; Options: TFindOptions); reintroduce;
    procedure SetLayout (Options: TFindOptions);
  end;

constructor TReplaceDialogForm.CreateNew(aOwner: TComponent;
  Options: TFindOptions);
var
  Layout: boolean; // true = Buttons at bottom
begin
  inherited CreateNew(aOwner);
  Layout := frButtonsAtBottom in Options;
  if Layout then begin
    Left := 415;
    Height := 220;
    Top := 391;
    Width := 530;
    ClientHeight := 220;
    ClientWidth := 530;
    Constraints.MinHeight := 220;
    Constraints.MinWidth := 530;
    end
  else begin
    Left := 415;
    Height := 184;
    Top := 333;
    Width := 480;
    ClientHeight := 140;
    ClientWidth := 480;
    Constraints.MinHeight := 184;
    Constraints.MinWidth := 480;
  end;
  ActiveControl := EditFind;
  AutoSize := True;
  BorderIcons := [biSystemMenu, biHelp];
  Caption := 'Replace Text';
  //Position := poMainFormCenter;
  LCLVersion := '1.3';

  TextLabel := TLabel.Create(Self);
  with TextLabel do begin
    Left := 4;
    Height := 17;
    Top := 14;
    Width := 77;
    BorderSpacing.Left := 4;
    BorderSpacing.Top := 14;
    Caption := 'Text to find';
    FocusControl := EditFind;
    ParentColor := False;
    Parent := Self;
  end;
  ReplaceLabel := TLabel.Create(Self);
  with ReplaceLabel do begin
    Left := 4;
    Height := 17;
    Top := 45;
    Width := 87;
    BorderSpacing.Top := 14;
    Caption := 'Replace with';
    FocusControl := EditReplace;
    ParentColor := False;
    Parent := Self;
  end;
  EditFind := TEdit.Create(Self);
  with EditFind do begin
    Left := 89;
    Height := 27;
    Top := 9;
    Width := 288;
    BorderSpacing.Left := 8;
    TabOrder := 0;
    Text := 'EditFind';
    Parent := Self;
  end;
  EditReplace := TEdit.Create(Self);
  with EditReplace do begin
    Left := 99;
    Height := 27;
    Top := 40;
    Width := 278;
    BorderSpacing.Left := 8;
    TabOrder := 1;
    Text := 'EditReplace';
    Parent := Self;
  end;
  DirectionRadioGroup := TRadioGroup.Create(Self);
  with DirectionRadioGroup do begin
    Left := 155;
    Height := 60;
    Top := 62;
    Width := 222;
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
    ClientHeight := 38;
    ClientWidth := 212;
    Columns := 2;
    Constraints.MaxHeight := 60;
    Items.Add('Forward');
    Items.Add('Backward');
    TabOrder := 2;
    Parent := Self;
  end;
  FlagPanel := TPanel.Create(Self);
  With FlagPanel do begin
    Left := 4;
    Height := 114;
    Top := 62;
    Width := 151;
    AutoSize := True;
    BorderSpacing.Around := 6;
    BevelOuter := bvNone;
    //ChildSizing.VerticalSpacing := 6;
    //ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    //ChildSizing.ControlsPerLine := 1;
    ClientHeight := 114;
    ClientWidth := 151;
    Constraints.MinHeight := 114;
    TabOrder := 3;
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
      Parent := FlagPanel;
    end;
    CaseSensitiveCheckBox := TCheckBox.Create(Self);
    with CaseSensitiveCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 29;
      Width := 142;
      Caption := 'Case sensitive';
      TabOrder := 1;
      Parent := FlagPanel;
    end;
    EntireScopeCheckBox := TCheckBox.Create(Self);
    with EntireScopeCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 58;
      Width := 142;
      Caption := 'Search entire file';
      TabOrder := 2;
      Parent := FlagPanel;
    end;
    PromptOnReplaceCheckBox := TCheckBox.Create(Self);
    with PromptOnReplaceCheckBox do begin
      Left := 0;
      Height := 23;
      Top := 89;
      Width := 151;
      Caption := 'Prompt on replace';
      TabOrder := 3;
      Parent := FlagPanel;
    end;
  //End of Flags Panel objects
  PanelButtons := TPanel.Create(Self);
  with PanelButtons do begin
    if Layout then begin
      Left := 0;
      Height := 29;
      Top := 191;
      Width := 530;
      Align := alBottom;
      BevelOuter := bvNone;
      ClientHeight := 29;
      ClientWidth := 530;
      TabOrder := 4;
      end
    else begin
      Left := 388;
      Height := 132;
      Top := 0;
      Width := 92;
      Align := alRight;
      BevelOuter := bvNone;
      ChildSizing.LeftRightSpacing := 6;
      ChildSizing.TopBottomSpacing := 6;
      ChildSizing.VerticalSpacing := 6;
      ClientHeight := 132;
      ClientWidth := 92;
      TabOrder := 3;
      end;
    Parent := Self;
    end;
  // PanelButtons Objects
    FindMoreButton := TButton.Create(Self);
    with FindMoreButton do begin
      Tag := 1;
      Left := 12;
      Height := 26;
      Top := 10;
      Width := 91;
      AutoSize := True;
      Caption := 'Find more';
      Constraints.MinWidth := 91;
      Default := True;
      TabOrder := 0;
      Parent := PanelButtons;
    end;
    ReplaceButton := TButton.Create(Self);
    with ReplaceButton do begin
      Tag := 2;
      Left := 12;
      Height := 26;
      Top := 46;
      Width := 91;
      AutoSize := True;
      Caption := 'Replace';
      Constraints.MinWidth := 91;
      Default := True;
      TabOrder := 2;
      Parent := PanelButtons;
    end;
    ReplaceAllButton := TButton.Create(Self);
    with ReplaceAllButton do begin
      Tag := 3;
      Left := 12;
      Height := 26;
      Top := 82;
      Width := 91;
      AutoSize := True;
      Caption := 'Replace all';
      Constraints.MinWidth := 91;
      Default := True;
      TabOrder := 2;
      Parent := PanelButtons;
    end;
    CancelButton := TButton.Create(Self);
    with CancelButton do begin
      Left := 12;
      Height := 26;
      Top := 122;
      Width := 91;
      AutoSize := True;
      Cancel := True;
      Caption := 'Cancel';
      Constraints.MinWidth := 91;
      ModalResult := 2;
      TabOrder := 1;
      Parent := PanelButtons;
    end;
    HelpButton := TButton.Create(Self);
    with HelpButton do begin
      Left := 12;
      Height := 26;
      Top := 158;
      Width := 91;
      AutoSize := True;
      Caption := 'Help';
      Constraints.MinWidth := 91;
      TabOrder := 3;
      Parent := PanelButtons;
    end;

    //EditFind.OnChange := @EditFindChange;
    SetLayout(Options);
end;

procedure TReplaceDialogForm.SetLayout(Options: TFindOptions);
var
  dlgWidth: Integer;
begin
  FButtonsBottom := frButtonsAtBottom in Options;
  if FButtonsBottom then begin
    if frShowHelp in Options then dlgWidth := 630
    else dlgWidth := 530;
    Height := 220;
    Width := dlgWidth;
    ClientHeight := 220;
    ClientWidth := dlgWidth;
    AutoSize := True;
    with PanelButtons do begin
      Left := 0;
      Height := 29;
      Top := 191;
      Width := dlgWidth;
      ClientHeight := 29;
      ClientWidth := dlgWidth;
      Align := alBottom;
      end;
    // Anchors
    TextLabel.AnchorSideLeft.Control := TControl(Owner);
    TextLabel.AnchorSideTop.Control := TControl(Owner);
    //TextLabel.Anchors := [akTop, akLeft];

    ReplaceLabel.AnchorSideLeft.Control := TextLabel;
    ReplaceLabel.AnchorSideTop.Control := TextLabel;
    ReplaceLabel.AnchorSideTop.Side := asrBottom;
    //ReplaceLabel.Anchors := [akTop, akLeft];

    EditFind.Width := 425;
    EditFind.AnchorSideLeft.Control := TextLabel;
    EditFind.AnchorSideLeft.Side := asrBottom;
    EditFind.AnchorSideTop.Control := TextLabel;
    EditFind.AnchorSideTop.Side := asrCenter;
    EditFind.AnchorSideRight.Control := TControl(Owner);
    EditFind.AnchorSideRight.Side := asrBottom;
    EditFind.Anchors := [akTop, akLeft, akRight];

    EditReplace.Width := 416;
    EditReplace.AnchorSideLeft.Control := ReplaceLabel;
    EditReplace.AnchorSideLeft.Side := asrBottom;
    EditReplace.AnchorSideTop.Control := ReplaceLabel;
    EditReplace.AnchorSideTop.Side := asrCenter;
    EditReplace.AnchorSideRight.Control := TControl(Owner);
    EditReplace.AnchorSideRight.Side := asrBottom;
    EditReplace.Anchors := [akTop, akLeft, akRight];

    //DirectionRadioGroup.Anchors := [akTop, akLeft];
    DirectionRadioGroup.AnchorSideLeft.Control := FlagPanel;
    DirectionRadioGroup.AnchorSideLeft.Side := asrBottom;
    DirectionRadioGroup.AnchorSideTop.Control := FlagPanel;

    FlagPanel.AnchorSideLeft.Control := TextLabel;
    FlagPanel.AnchorSideTop.Control := EditReplace;
    FlagPanel.AnchorSideTop.Side := asrBottom;
    FlagPanel.AnchorSideBottom.Side := asrBottom;

    FindMoreButton.Top := 0;
    FindMoreButton.Left := 439;
    FindMoreButton.AnchorSideTop.Control := PanelButtons;
    FindMoreButton.AnchorSideRight.Control := PanelButtons;
    FindMoreButton.AnchorSideRight.Side := asrBottom;
    FindMoreButton.Anchors := [akTop, akRight];
    FindMoreButton.BorderSpacing.Right := 6;

    ReplaceButton.Top := 0;
    ReplaceButton.Left := 309;
    ReplaceButton.BorderSpacing.Left := 12;
    ReplaceButton.AnchorSideLeft.Control := ReplaceAllButton;
    ReplaceButton.AnchorSideLeft.Side := asrBottom;
    ReplaceButton.AnchorSideTop.Control := PanelButtons;
    ReplaceButton.AnchorSideRight.Side := asrBottom;
    //ReplaceButton.Anchors := [akLeft,akTop];

    ReplaceAllButton.Top := 0;
    ReplaceAllButton.Left := 206;
    ReplaceAllButton.BorderSpacing.Left := 12;
    ReplaceAllButton.AnchorSideLeft.Control := CancelButton;
    ReplaceAllButton.AnchorSideLeft.Side := asrBottom;
    ReplaceAllButton.AnchorSideTop.Control := PanelButtons;
    ReplaceAllButton.AnchorSideRight.Side := asrBottom;
    //ReplaceAllButton.Anchors := [akLeft,akTop];

    CancelButton.Top := 0;
    CancelButton.Left := 103;
    CancelButton.BorderSpacing.Left := 12;
    CancelButton.AnchorSideLeft.Control := HelpButton;
    CancelButton.AnchorSideLeft.Side := asrBottom;
    CancelButton.AnchorSideTop.Control := PanelButtons;
    CancelButton.AnchorSideRight.Side := asrBottom;
    //CancelButton.Anchors := [akLeft,akTop];

    HelpButton.AnchorSideLeft.Control := PanelButtons;
    HelpButton.AnchorSideTop.Control := PanelButtons;
    HelpButton.AnchorSideRight.Control := PanelButtons;
    HelpButton.AnchorSideRight.Side := asrBottom;
    //HelpButton.Anchors := [akLeft,akRight,akTop];
    end
  else begin
    with PanelButtons do begin
      Height := 184;
      Top := 0;
      Width := 103;
      AutoSize := True;
      Align := alRight;
    end;
    // Anchors
    TextLabel.AnchorSideTop.Control := TControl(Owner);
    TextLabel.AnchorSideLeft.Control := TControl(Owner);

    ReplaceLabel.AnchorSideLeft.Control := TextLabel;
    ReplaceLabel.AnchorSideTop.Control := TextLabel;
    ReplaceLabel.AnchorSideTop.Side := asrBottom;

    EditFind.Anchors := [akTop, akLeft, akRight];
    EditFind.AnchorSideLeft.Control := TextLabel;
    EditFind.AnchorSideLeft.Side := asrBottom;
    EditFind.AnchorSideTop.Control := TextLabel;
    EditFind.AnchorSideTop.Side := asrCenter;
    EditFind.AnchorSideRight.Control := PanelButtons;

    EditReplace.Anchors := [akTop, akLeft, akRight];
    EditReplace.AnchorSideLeft.Control := ReplaceLabel;
    EditReplace.AnchorSideLeft.Side := asrBottom;
    EditReplace.AnchorSideTop.Control := ReplaceLabel;
    EditReplace.AnchorSideTop.Side := asrCenter;
    EditReplace.AnchorSideRight.Control := PanelButtons;

    DirectionRadioGroup.Anchors := [akTop, akLeft, akRight, akBottom];
    DirectionRadioGroup.AnchorSideLeft.Control := FlagPanel;
    DirectionRadioGroup.AnchorSideLeft.Side := asrBottom;
    DirectionRadioGroup.AnchorSideTop.Control := FlagPanel;
    DirectionRadioGroup.AnchorSideRight.Control := PanelButtons;
    DirectionRadioGroup.AnchorSideBottom.Control := FlagPanel;
    DirectionRadioGroup.AnchorSideBottom.Side := asrBottom;

    FlagPanel.AnchorSideLeft.Control := TextLabel;
    FlagPanel.AnchorSideTop.Control := ReplaceLabel;
    FlagPanel.AnchorSideTop.Side := asrBottom;

    FindMoreButton.BorderSpacing.Top := 10;
    FindMoreButton.Anchors := [akTop, akRight];
    FindMoreButton.AnchorSideTop.Control := PanelButtons;
    FindMoreButton.AnchorSideRight.Control := PanelButtons;
    FindMoreButton.AnchorSideRight.Side := asrBottom;

    ReplaceButton.BorderSpacing.Top := 10;
    ReplaceButton.AnchorSideLeft.Side := asrBottom;
    ReplaceButton.AnchorSideTop.Control := FindMoreButton;
    ReplaceButton.AnchorSideTop.Side := asrBottom;
    ReplaceButton.AnchorSideRight.Control := PanelButtons;
    ReplaceButton.AnchorSideRight.Side := asrBottom;
    ReplaceButton.Anchors := [akTop, akRight];

    ReplaceAllButton.BorderSpacing.Top := 10;
    ReplaceAllButton.AnchorSideLeft.Side := asrBottom;
    ReplaceAllButton.AnchorSideTop.Control := ReplaceButton;
    ReplaceAllButton.AnchorSideTop.Side := asrBottom;
    ReplaceAllButton.AnchorSideRight.Control := PanelButtons;
    ReplaceAllButton.AnchorSideRight.Side := asrBottom;
    ReplaceAllButton.Anchors := [akTop, akRight];

    CancelButton.BorderSpacing.Bottom := 10;
    CancelButton.AnchorSideLeft.Side := asrBottom;
    CancelButton.AnchorSideTop.Side := asrBottom;
    CancelButton.AnchorSideRight.Control := PanelButtons;
    CancelButton.AnchorSideRight.Side := asrBottom;
    CancelButton.AnchorSideBottom.Control := HelpButton;
    CancelButton.Anchors := [akRight, akBottom];

    HelpButton.AnchorSideLeft.Control := PanelButtons;
    HelpButton.AnchorSideRight.Control := PanelButtons;
    HelpButton.AnchorSideRight.Side := asrBottom;
    HelpButton.AnchorSideBottom.Control := PanelButtons;
    HelpButton.AnchorSideBottom.Side := asrBottom;
    HelpButton.Anchors := [akLeft, akRight, akBottom];
  end;
end;

{ TReplaceDialog }

procedure TReplaceDialog.ReplaceClick(Sender: TObject);
begin
  GetFormValues;
  //Delphi does it this way
  FOptions := FOptions + [frReplace] - [frReplaceAll, frFindNext];
  Replace;
end;

procedure TReplaceDialog.ReplaceAllClick(Sender: TObject);
begin
  GetFormValues;
  //Delphi does it this way
  FOptions := FOptions + [frReplaceAll] - [frFindNext, frReplace];
  Replace;
end;

function TReplaceDialog.CreateForm: TForm;
begin
  Result := TReplaceDialogForm.CreateNew(Self,Options);
  with TReplaceDialogForm(Result) do
  begin
    FindMoreButton.Caption := rsFindMore;
    ReplaceButton.Caption := rsReplace;
    ReplaceAllButton.Caption := rsReplaceAll;
    CancelButton.Caption := rsMbCancel;
    HelpButton.Caption := rsHelp;
    WholeWordsOnlyCheckBox.Caption := rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption := rsCaseSensitive;
    EntireScopeCheckBox.Caption := rsEntireScope;
    PromptOnReplaceCheckBox.Caption := rsPromptOnReplace;
    TextLabel.Caption := rsText;
    ReplaceLabel.Caption := rsReplace;
    DirectionRadioGroup.Caption := rsDirection;
    DirectionRadioGroup.Items[0] := rsForward;
    DirectionRadioGroup.Items[1] := rsBackward;

    //Setting up button eventhandlers
    FindMoreButton.OnClick := @FindClick;
    ReplaceButton.OnClick := @ReplaceClick;
    ReplaceAllButton.OnClick := @ReplaceAllClick;
    CancelButton.OnClick := @CancelClick;
    HelpButton.OnClick := @HelpClick;
    PopupMode := pmAuto;
    // Init local property with default value
    Result.Top := FFormTop;
    Result.Left := FFormLeft;
  end;
end;

procedure TReplaceDialog.SetFormValues;
var
  dlg: TReplaceDialogForm;
begin
  dlg:=TReplaceDialogForm(FFindForm);
  dlg.EditFind.Text:=FFindText;
  Dlg.EditFind.SelectAll;
  Dlg.ActiveControl:=Dlg.EditFind;

  dlg.EditReplace.Text:=FReplaceText;

  dlg.WholeWordsOnlyCheckBox.Checked:=frWholeWord in Options;
  Dlg.EntireScopeCheckBox.Checked:=frEntireScope in Options;
  dlg.CaseSensitiveCheckBox.Checked:=frMatchCase in Options;
  dlg.PromptOnReplaceCheckBox.Checked := frPromptOnReplace in Options;
  Dlg.DirectionRadioGroup.ItemIndex:=ord(not(frDown in Options));

  dlg.WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  dlg.DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  dlg.WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  dlg.DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  dlg.HelpButton.Visible:=(frShowHelp in Options);
  Dlg.EntireScopeCheckBox.Visible := not (frHideEntireScope in Options);
  dlg.PromptOnReplaceCheckBox.Visible := not (frHidePromptOnReplace in Options);
end;

procedure TReplaceDialog.GetFormValues;
var
  Dlg: TReplaceDialogForm;
begin
  Dlg:=TReplaceDialogForm(FFindForm);
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
  if Dlg.PromptOnReplaceCheckBox.Checked then
    FOptions := FOptions + [frPromptOnReplace]
  else
    FOptions := FOptions - [frPromptOnReplace];
  FFindText := Dlg.EditFind.Text;
  FReplaceText := Dlg.EditReplace.Text;
end;

constructor TReplaceDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions += [frReplace, frReplaceAll, frHidePromptOnreplace];
end;

function TReplaceDialog.DefaultTitle: string; 
begin
  Result := rsReplace;
end;

