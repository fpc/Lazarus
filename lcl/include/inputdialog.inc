{%MainUnit ../dialogs.pp}
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

Function ShowInputDialog(const InputCaption, InputPrompt : String;
  MaskInput : Boolean; var Value : String) : Boolean;
Const
  AVGBuffer : PChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890()|_ ';
var
  Form : TForm;
  Prompt : TLabel;
  Edit : TEdit;
  ButtonTop, ButtonWidth, ButtonHeight : Integer;
  Avg : TPoint;
begin
  Result := False;
  Form := TForm.CreateNew(nil, 0);
  With Form do begin
    BorderStyle := bsDialog;
    Caption := InputCaption;
    Prompt := TLabel.Create(Form);
    With Prompt do begin
      Parent := Form;
      Caption := InputPrompt;
      Visible := True;
    end;
    SelectObject(Canvas.Handle, GetStockObject(DEFAULT_GUI_FONT));
    GetTextExtentPoint(Canvas.Handle,AVGBuffer,StrLen(AVGBuffer),TSize(AVG));
    AVG.X := AVG.X div 52;
    Position := poScreenCenter;
    Prompt.Left := (7*AVG.X) div 4;
    Prompt.Top := (8*AVG.Y) div 8;
    Prompt.Width := AVG.X * Length(InputPrompt) + 1;
    ClientWidth := (172*AVG.X) div 4;
    ClientHeight := (58*AVG.Y) div 8;
    ButtonTop := (39*AVG.Y) div 8;
    ButtonWidth := (50*AVG.X) div 4;
    ButtonHeight := 2*AVG.Y;
    Edit := TEdit.Create(Form);
    With Edit do begin
      Parent := Form;
      Left := Prompt.Left;
      Top := (19*AVG.Y) div 8;
      Width := (160*AVG.X) div 4;
      Height := (7*AVG.Y) div 4;
      Text := Value;
      TabStop := True;
      Visible := True;
      If MaskInput then
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
    With TBitBtn.Create(Form) do begin
      Parent := Form;
      Kind := bkOk;
      Default := True;
      ModalResult := mrOk;
      Left := (37*AVG.X) div 4;
      Top := ButtonTop;
      Height := ButtonHeight;
      Width := ButtonWidth;
      TabStop := True;
      Visible := True;
      TabOrder := 1;
    end;
    With TBitBtn.Create(Form) do begin
      Parent := Form;
      Kind := bkCancel;
      Cancel := True;
      Left := (92*AVG.X) div 4;
      Top := ButtonTop;
      Height := ButtonHeight;
      Width := ButtonWidth;
      TabStop := True;
      Visible := True;
      TabOrder := 2;
    end;

    // upon show, the edit control will be focused for editing, because it's
    // the first in the tab order
    If ShowModal = mrOk then
    begin
      Value := Edit.Text;
      Result := True;
    end;
    Form.Free;
  end;
end;

// included by dialogs.pp
