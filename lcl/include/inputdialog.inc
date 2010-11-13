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

  function ActiveMonitor: TMonitor; inline;
  begin
    if Screen.ActiveCustomForm <> nil then
      Result := Screen.ActiveCustomForm.Monitor
    else
    if Application.MainForm <> nil then
      Result := Application.MainForm.Monitor
    else
      Result := Screen.PrimaryMonitor;
  end;

var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  MinEditWidth: integer;
  AMonitor: TMonitor;
begin
  Result := False;
  Form := TForm(TForm.NewInstance);
  Form.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowInputDialog'){$ENDIF};
  Form.CreateNew(nil, 0);
  with Form do
  begin
    PopupMode := pmAuto;
    BorderStyle := bsDialog;
    Caption := InputCaption;
    Position := poScreenCenter;
    Prompt := TLabel.Create(Form);
    with Prompt do
    begin
      Parent := Form;
      Caption := InputPrompt;
      Align := alTop;
      AutoSize := True;
    end;
    Edit := TEdit.Create(Form);
    with Edit do
    begin
      Parent := Form;
      Top := Prompt.Height;
      Align := alTop;
      BorderSpacing.Top := 3;
      AMonitor := ActiveMonitor;
      // check that edit is smaller than our monitor, it must be smaller at least
      // by 6 * 2 pixels (spacing from window borders) + window border
      MinEditWidth := Min(AMonitor.Width - 20, Max(260, AMonitor.Width div 4));
      Constraints.MinWidth := MinEditWidth;
      Text := Value;
      TabStop := True;
      if MaskInput then
      begin 
        EchoMode := emPassword; 
        PasswordChar := '*';
      end else 
      begin 
        EchoMode := emNormal;
        PasswordChar := #0;
      end; 
      TabOrder := 0;
    end;

    with TButtonPanel.Create(Form) do
    begin
      Top := Edit.Top + Edit.Height;
      Parent := Form;
      ShowBevel := False;
      ShowButtons := [pbOK, pbCancel];
      Align := alTop;
    end;

    ChildSizing.TopBottomSpacing := 6;
    ChildSizing.LeftRightSpacing := 6;
    AutoSize := True;

    // upon show, the edit control will be focused for editing, because it's
    // the first in the tab order
    Form.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowInputDialog'){$ENDIF};
    if ShowModal = mrOk then
    begin
      Value := Edit.Text;
      Result := True;
    end;
    Form.Free;
  end;
end;

// included by dialogs.pp
