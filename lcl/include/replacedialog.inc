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

{ TReplaceDialogForm }

  TReplaceDialogForm = class(TForm)
    FindMoreButton: TButton;
    ReplaceButton: TButton;
    ReplaceAllButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    CaseSensitiveCheckBox: TCheckBox;
    EditFind: TEdit;
    EditReplace: TEdit;
    TextLabel: TLabel;
    ReplaceLabel: TLabel;
    DirectionRadioGroup: TRadioGroup;
    procedure btnReplaceAllClick(Sender: TObject);
  private
    ReplOwner: TReplaceDialog;
  public
  end;

procedure TReplaceDialogForm.btnReplaceAllClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1:ReplOwner.FOptions:=ReplOwner.FOptions + [frFindNext];
    2:ReplOwner.FOptions:=ReplOwner.FOptions + [frReplace];
    3:ReplOwner.FOptions:=ReplOwner.FOptions + [frReplaceAll];
  end;
  if DirectionRadioGroup.ItemIndex = 0 then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frDown]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frDown];

  if WholeWordsOnlyCheckBox.Checked then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frWholeWord]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frWholeWord];

  if CaseSensitiveCheckBox.Checked then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frMatchCase]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frMatchCase];
end;


{ TReplaceDialog }

procedure TReplaceDialog.DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TReplaceDialogForm(Sender) do
  begin
    if ModalResult <> mrCancel then
    begin
      FFindText:=EditFind.Text;
      FReplaceText:=EditReplace.Text;
    end
    else
      Options:=FOptions - [frFindNext];
  end;
end;

function TReplaceDialog.CreateForm: TForm;
begin
  Result:=TReplaceDialogForm.Create(nil);
  with TReplaceDialogForm(Result) do begin
    FindMoreButton.Caption:=rsFindMore;
    ReplaceButton.Caption:=rsReplace;
    ReplaceAllButton.Caption:=rsReplaceAll;
    CancelButton.Caption:=rsMbCancel;
    HelpButton.Caption:=ifsVK_HELP;
    WholeWordsOnlyCheckBox.Caption:=rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption:=rsCaseSensitive;
    TextLabel.Caption:=rsText;
    ReplaceLabel.Caption:=rsReplace;
    DirectionRadioGroup.Caption:=rsDirection;
    DirectionRadioGroup.Items[0]:=rsForward;
    DirectionRadioGroup.Items[1]:=rsBackward;
  end;
  TReplaceDialogForm(Result).ReplOwner:=Self;
end;

procedure TReplaceDialog.UpdateValues;
var
  dlg: TReplaceDialogForm;
begin
  dlg:=TReplaceDialogForm(FFindForm);
  dlg.EditFind.Text:=FFindText;
  dlg.EditReplace.Text:=FReplaceText;

  dlg.WholeWordsOnlyCheckBox.Checked:=frWholeWord in Options;
  dlg.CaseSensitiveCheckBox.Checked:=frMatchCase in Options;
  dlg.DirectionRadioGroup.ItemIndex:=ord(frDown in Options);

  dlg.WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  dlg.DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  dlg.WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  dlg.DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  dlg.HelpButton.Visible:=(frShowHelp in Options);
end;

constructor TReplaceDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options:=Options + [frReplace, frReplaceAll];
end;


