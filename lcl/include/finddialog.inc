{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TFindDialog
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


type
  { TFindDialogForm }

  TFindDialogForm = class(TForm)
    FindButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    CaseSensitiveCheckBox: TCheckBox;
    EditFind: TEdit;
    FindLabel: TLabel;
    DirectionRadioGroup: TRadioGroup;
    procedure EditFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

procedure TFindDialogForm.EditFindChange(Sender: TObject);
begin
  FindButton.Enabled:=EditFind.Text<>'';
end;

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  EditFindChange(nil);
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

procedure TFindDialog.UpdatePosition;
begin
  if Assigned(FFindForm) then
  begin
    FFindForm.Top:=FFormTop;
    FFindForm.Left:=FFormLeft;
  end;
end;

procedure TFindDialog.DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TFindDialogForm(Sender) do
  begin
    if ModalResult <> mrCancel then
    begin
      FOptions:=FOptions + [frFindNext];
      FFindText:=EditFind.Text;
      if DirectionRadioGroup.ItemIndex = 0 then
        FOptions:=FOptions + [frDown]
      else
        FOptions:=FOptions - [frDown];

      if WholeWordsOnlyCheckBox.Checked then
        FOptions:=FOptions + [frWholeWord]
      else
        FOptions:=FOptions - [frWholeWord];

      if CaseSensitiveCheckBox.Checked then
        FOptions:=FOptions + [frMatchCase]
      else
        FOptions:=FOptions - [frMatchCase];
    end
    else
      FOptions:=FOptions - [frFindNext];
  end;
end;

function TFindDialog.GetFindText: string;
begin
  Result:=FFindText;
end;

function TFindDialog.GetLeft: Integer;
begin
  Result:=FFormLeft;
end;

function TFindDialog.GetPosition: TPoint;
begin
  Result:=Point(FFormLeft, FFormTop);
end;

function TFindDialog.GetTop: Integer;
begin
  Result:=FFormTop;
end;

procedure TFindDialog.SetFindText(const AValue: string);
begin
  if FFindText = AValue then exit;
  FFindText:=AValue;
end;

procedure TFindDialog.SetLeft(const AValue: Integer);
begin
  if FFormLeft <> AValue then
  begin
    FFormLeft:=AValue;
    UpdatePosition;
  end;
end;

procedure TFindDialog.SetPosition(const AValue: TPoint);
begin
  if (FFormLeft<>AValue.x) or (FFormTop<>AValue.y) then
  begin;
    FFormLeft:=AValue.x;
    FFormTop:=AValue.y;
    UpdatePosition;
  end;
end;

procedure TFindDialog.SetTop(const AValue: Integer);
begin
  if FFormTop <> AValue then
  begin
    FFormTop:=AValue;
    UpdatePosition;
  end;
end;

procedure TFindDialog.Find;
begin
  if Assigned(FOnFind) then
    FOnFind(Self);
end;

procedure TFindDialog.Replace;
begin
  if Assigned(FOnReplace) then
    FOnReplace(Self);
end;

function TFindDialog.CreateForm: TForm;
begin
  Result:=TFindDialogForm.Create(Self);
  with TFindDialogForm(Result) do begin
    FindButton.Caption:=rsFind;
    CancelButton.Caption:=rsMbCancel;
    HelpButton.Caption:=ifsVK_HELP;
    WholeWordsOnlyCheckBox.Caption:=rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption:=rsCaseSensitive;
    FindLabel.Caption:=rsText;
    DirectionRadioGroup.Caption:=rsDirection;
    DirectionRadioGroup.Items[0]:=rsForward;
    DirectionRadioGroup.Items[1]:=rsBackward;
  end;
end;

procedure TFindDialog.UpdateValues;
begin
  TFindDialogForm(FFindForm).EditFind.Text:=FFindText;

  TFindDialogForm(FFindForm).WholeWordsOnlyCheckBox.Checked:=frWholeWord in Options;
  TFindDialogForm(FFindForm).CaseSensitiveCheckBox.Checked:=frMatchCase in Options;
  TFindDialogForm(FFindForm).DirectionRadioGroup.ItemIndex:=ord(frDown in Options);

  TFindDialogForm(FFindForm).WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  TFindDialogForm(FFindForm).CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  TFindDialogForm(FFindForm).DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  TFindDialogForm(FFindForm).WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  TFindDialogForm(FFindForm).CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  TFindDialogForm(FFindForm).DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  TFindDialogForm(FFindForm).HelpButton.Visible:=(frShowHelp in Options);
end;

constructor TFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions:=[frDown];
end;

destructor TFindDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TFindDialog.CloseDialog;
begin
  if Assigned(FFindForm) then
    FFindForm.Close;
end;

function TFindDialog.Execute: Boolean;
begin
  FFindForm:=CreateForm;
  if Assigned(FFindForm) then
  try
    UpdateValues;
    FFindForm.OnClose:=@DoCloseForm;
    FFindForm.HelpContext:=HelpContext;
    FFindForm.Caption:=Title;
    Result:=FFindForm.ShowModal=mrOk;
  finally
    FreeAndNil(FFindForm);
  end;
end;


