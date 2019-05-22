{%MainUnit ../dialogs.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function _InputQueryActiveMonitor: TMonitor;
begin
  if Screen.ActiveCustomForm <> nil then
    Result := Screen.ActiveCustomForm.Monitor
  else
  if Application.MainForm <> nil then
    Result := Application.MainForm.Monitor
  else
    Result := Screen.PrimaryMonitor;
end;

function DefaultInputDialog(const InputCaption, InputPrompt : String;
  MaskInput : Boolean; var Value : String) : Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  MinEditWidth, NSpacing: integer;
  AMonitor: TMonitor;
begin
  Result := False;
  Form := TForm(TForm.NewInstance);
  Form.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowInputDialog'){$ENDIF};
  Form.CreateNew(nil, 0);
  with Form do
  try
    PopupMode := pmAuto;
    BorderStyle := bsDialog;
    Caption := InputCaption;
    Position := poScreenCenter;
    NSpacing := Scale96ToScreen(6);
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
      BorderSpacing.Top := NSpacing div 2;
      AMonitor := _InputQueryActiveMonitor;
      // check that edit is smaller than our monitor, it must be smaller at least
      // by 6 * 2 pixels (spacing from window borders) + window border
      MinEditWidth := Min(AMonitor.Width - 20,
        Max(Scale96ToScreen(cInputQueryEditSizePixels),
            AMonitor.Width * cInputQueryEditSizePercents div 100));
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

    ChildSizing.TopBottomSpacing := NSpacing;
    ChildSizing.LeftRightSpacing := NSpacing;
    AutoSize := True;

    // upon show, the edit control will be focused for editing, because it's
    // the first in the tab order
    Form.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('ShowInputDialog'){$ENDIF};
    if ShowModal = mrOk then
    begin
      Value := Edit.Text;
      Result := True;
    end;
  finally
    Form.Free;
  end;
end;

function DoInputCombo(const ACaption, APrompt: string; const AList: TStrings; AllowInput : Boolean; Out ASelected : Integer) : String;
const
  CBStyles : array[Boolean] of TComboBoxStyle = (csDropDownList,csDropDown);
var
  W,I,Sep,Margin: Integer;
  Frm: TForm;
  CBSelect : TComboBox;
  LPrompt: TLabel;
  BP: TButtonPanel;
begin
  Margin:=24;
  Sep:=8;
  Result:='';
  ASelected:=-1;
  Frm:=TForm.Create(Application);
  try
    // Determine needed width
    W:=frm.Canvas.TextWidth(APrompt);
    W:=Max(W,frm.Canvas.TextWidth(ACaption));
    for I:=0 to AList.Count-1 do
      W:=Max(W,frm.Canvas.TextWidth(AList[i]+'WWW')); // WWW is just some extra.
    frm.BorderStyle:=bsDialog;
    frm.Caption:=ACaption;
    frm.ClientWidth:=W+2*Margin;
    frm.Position:=poScreenCenter;
    // Prompt
    LPrompt:=TLabel.Create(frm);
    LPrompt.Parent:=frm;
    LPrompt.Caption:=APrompt;
    LPrompt.SetBounds(Margin,Margin,Frm.ClientWidth-2*Margin,frm.Canvas.TextHeight(APrompt));
    LPrompt.WordWrap:=True;
    LPrompt.AutoSize:=False;
    // Selection combobox
    CBSelect:=TComboBox.Create(Frm);
    CBSelect.Parent:=Frm;
    CBSelect.Style:=CBStyles[AllowInput];
    CBSelect.Items.Assign(AList);
    CBSelect.ItemIndex:=-1;
    CBSelect.Left:=Margin;
    CBSelect.Top:=LPrompt.Top + LPrompt.Height + Sep;
    CBSelect.Width:=Frm.ClientWidth-2*Margin;
    // Buttons
    BP:=TButtonPanel.Create(Frm);
    BP.Parent:=Frm;
    BP.ShowButtons:=[pbOK,pbCancel];
    Frm.ClientHeight:=LPrompt.Height+CBSelect.Height+BP.Height+2*Sep+Margin;
    if (Frm.ShowModal=mrOk) then
    begin
      Result:=CBSelect.Text;
      ASelected:=CBSelect.ItemIndex;
    end;
  finally
    FreeAndNil(Frm);
  end;
end;

function InputCombo(const ACaption, APrompt: string; const AList: TStrings
  ): Integer;
begin
  DoInputCombo(ACaption,APrompt,AList,False,Result);
end;

function InputCombo(const ACaption, APrompt: string;
  const AList: array of String): Integer;
var
  L : TStrings;
  S : String;
begin
  L:=TStringList.Create;
  try
    for S in AList do
      L.Add(S);
    Result:=InputCombo(ACaption,APrompt,L);
  finally
    L.Free;
  end;
end;

function InputComboEx(const ACaption, APrompt: string; const AList: TStrings;
  AllowCustomText: Boolean): String;
var
  D: Integer;
begin
  Result:=DoInputCombo(ACaption,APrompt,AList,AllowCustomText,D);
end;

function InputComboEx(const ACaption, APrompt: string;
  const AList: array of String; AllowCustomText: Boolean): String;
var
  L: TStrings;
  S: String;
begin
  L:=TstringList.Create;
  try
    for S in AList do
      L.Add(S);
    Result:=InputComboEx(ACaption,APrompt,L,AllowCustomText);
  finally
    L.Free;
  end;
end;

// included by dialogs.pp
