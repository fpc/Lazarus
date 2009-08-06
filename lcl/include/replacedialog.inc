{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TFindDialog
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





type

{ TReplaceDialogForm }

  TReplaceDialogForm = class(TForm)
    EntireScopeCheckBox: TCheckBox;
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
  private
  public
    { public declarations }
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
  Result:=TReplaceDialogForm.Create(nil);
  with TReplaceDialogForm(Result) do begin
    FindMoreButton.Caption:=rsFindMore;
    ReplaceButton.Caption:=rsReplace;
    ReplaceAllButton.Caption:=rsReplaceAll;
    CancelButton.Caption:=rsMbCancel;
    HelpButton.Caption:=ifsVK_HELP;
    WholeWordsOnlyCheckBox.Caption:=rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption:=rsCaseSensitive;
    EntireScopeCheckBox.Caption:=rsEntireScope;
    TextLabel.Caption:=rsText;
    ReplaceLabel.Caption:=rsReplace;
    DirectionRadioGroup.Caption:=rsDirection;
    DirectionRadioGroup.Items[0]:=rsForward;
    DirectionRadioGroup.Items[1]:=rsBackward;

    //Setting up button eventhandlers
    FindMoreButton.OnClick := @FindClick;
    ReplaceButton.OnClick := @ReplaceClick;
    ReplaceAllButton.OnClick := @ReplaceAllClick;
    CancelButton.OnClick := @CancelClick;
    HelpButton.OnClick := @HelpClick;
    //Once the issues concerning FormStyle = fsStayOnTop are resolved we can implement this
    //so that the form should stay on top of the application (not on top of everything)
    //See bug #8471 in Mantis
    //FormStyle := fsStayOnTop;
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
  Dlg.DirectionRadioGroup.ItemIndex:=ord(not(frDown in Options));

  dlg.WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  dlg.DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  dlg.WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  dlg.CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  dlg.DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  dlg.HelpButton.Visible:=(frShowHelp in Options);
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
  FFindText := Dlg.EditFind.Text;
  FReplaceText := Dlg.EditReplace.Text;
end;

constructor TReplaceDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options:=Options + [frReplace, frReplaceAll];
end;


