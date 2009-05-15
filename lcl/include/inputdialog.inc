{%MainUnit ../dialogs.pp}
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

function ShowInputDialog(const InputCaption, InputPrompt : String;
  MaskInput : Boolean; var Value : String) : Boolean;
var
  Form : TForm;
  Prompt : TLabel;
  Edit : TEdit;
  MinEditWidth: integer;
begin
  Result := False;
  Form := TForm.CreateNew(nil, 0);
  with Form do
  begin
    BorderStyle := bsDialog;
    Caption := InputCaption;
    Prompt := TLabel.Create(Form);
    with Prompt do
    begin
      Parent := Form;
      Caption := InputPrompt;
      Align := alTop;
      Visible := True;
      AutoSize := True;
    end;
    Position := poScreenCenter;
    Edit := TEdit.Create(Form);
    with Edit do
    begin
      Parent := Form;
      Align := alTop;
      AnchorToNeighbour(akTop, 6, Prompt);
      MinEditWidth := max(260, Screen.Width div 4);
      Constraints.MinWidth := MinEditWidth;
      Text := Value;
      TabStop := True;
      Visible := True;
      if MaskInput then
      begin 
        EchoMode := emPassword; 
        PasswordChar:='*'; 
      end else 
      begin 
        EchoMode := emNormal;
        PasswordChar:=#0; 
      end; 
      TabOrder := 0;
    end;

    with TButtonPanel.Create(Form) do
    begin
      Parent := Form;
      ShowBevel := False;
      ShowButtons := [pbOK, pbCancel];
      Align := alTop;
      AnchorToNeighbour(akTop, 0, Edit);
      Visible := True;
    end;

    ChildSizing.TopBottomSpacing := 6;
    ChildSizing.LeftRightSpacing := 6;
    AutoSize := True;

    // upon show, the edit control will be focused for editing, because it's
    // the first in the tab order
    if ShowModal = mrOk then
    begin
      Value := Edit.Text;
      Result := True;
    end;
    Form.Free;
  end;
end;

// included by dialogs.pp
