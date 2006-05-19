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
    Button1: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    Button4: TButton;
    btnHelp: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    EditFind: TEdit;
    EditReplace: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
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
  if RadioGroup1.ItemIndex = 0 then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frDown]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frDown];

  if CheckBox1.Checked then
    ReplOwner.FOptions:=ReplOwner.FOptions + [frWholeWord]
  else
    ReplOwner.FOptions:=ReplOwner.FOptions - [frWholeWord];

  if CheckBox2.Checked then
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
  Result:=TReplaceDialogForm.Create(Application);
  TReplaceDialogForm(Result).ReplOwner:=Self;
end;

procedure TReplaceDialog.UpdateValues;
begin
  TReplaceDialogForm(FFindForm).EditFind.Text:=FFindText;
  TReplaceDialogForm(FFindForm).EditReplace.Text:=FReplaceText;

  TReplaceDialogForm(FFindForm).CheckBox1.Checked:=frWholeWord in Options;
  TReplaceDialogForm(FFindForm).CheckBox2.Checked:=frMatchCase in Options;
  TReplaceDialogForm(FFindForm).RadioGroup1.ItemIndex:=ord(frDown in Options);

  TReplaceDialogForm(FFindForm).CheckBox1.Enabled:=not (frDisableWholeWord in Options);
  TReplaceDialogForm(FFindForm).CheckBox2.Enabled:=not (frDisableMatchCase in Options);
  TReplaceDialogForm(FFindForm).RadioGroup1.Enabled:=not (frDisableUpDown in Options);

  TReplaceDialogForm(FFindForm).CheckBox1.Visible:=not (frHideWholeWord in Options);
  TReplaceDialogForm(FFindForm).CheckBox2.Visible:=not (frHideMatchCase in Options);
  TReplaceDialogForm(FFindForm).RadioGroup1.Visible:=not (frHideUpDown in Options);
  TReplaceDialogForm(FFindForm).btnHelp.Visible:=(frShowHelp in Options);
end;

constructor TReplaceDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options:=Options + [frReplace, frReplaceAll];
end;

