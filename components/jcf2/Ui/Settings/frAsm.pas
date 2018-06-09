unit frAsm;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frAsm,.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  SysUtils, StdCtrls, ExtCtrls, Spin,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfAsm }

  TfAsm = class(TAbstractIDEOptionsEditor)
    rgCaps: TRadioGroup;
    gbStatementIndent: TGroupBox;
    edtStatementIndent: TSpinEdit;
    cbStatementIndent: TCheckBox;
    gbBreaksAfterLabel: TGroupBox;
    cbBreaksAfterLabelEnabled: TCheckBox;
    edtBreaksAfterLabel: TSpinEdit;
    Label7: TLabel;
    Label1: TLabel;
    gbParamsIndent: TGroupBox;
    Label2: TLabel;
    edtParamsIndent: TSpinEdit;
    cbParamsIndent: TCheckBox;
    procedure FrameResize(Sender:TObject);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  { jcf }
  JcfSettings, jcfuiconsts,
  { local }
  SettingsTypes, SetAsm;

procedure TfAsm.FrameResize(Sender:TObject);
begin
  rgCaps.Width := (Width-18) div 2;
end;

function TfAsm.GetTitle: String;
begin
  Result := lisAsmAsm;
end;

procedure TfAsm.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbStatementIndent.Caption := lisAsmStatementIndents;
  cbStatementIndent.Caption := lisAsmEnabled;
  Label1.Caption := lisAsmIndent;

  gbParamsIndent.Caption := lisAsmParamsIndents;
  cbParamsIndent.Caption := lisAsmENabled2;
  Label2.Caption := lisAsmIndent;

  rgCaps.Caption := lisAsmCapitalization;
  rgCaps.Items[0] := lisObfsAllCapitals;
  rgCaps.Items[1] := lisObfsAllLowerCase;
  rgCaps.Items[2] := lisObfsMixedCase;
  rgCaps.Items[3] := lisObfsLeaveAlone;

  gbBreaksAfterLabel.Caption := lisAsmBreaksAfterLabel;
  cbBreaksAfterLabelEnabled.Caption := lisAsmEnAbled3;
  Label7.Caption := lisAsmNUmberOfBreaks;
end;

procedure TfAsm.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.SetAsm do
  begin
    cbStatementIndent.Checked := StatementIndentEnabled;
    edtStatementIndent.Value := StatementIndent;

    cbParamsIndent.Checked := ParamsIndentEnabled;
    edtParamsIndent.Value := ParamsIndent;

    cbBreaksAfterLabelEnabled.Checked := BreaksAfterLabelEnabled;
    edtBreaksAfterLabel.Value := BreaksAfterLabel;

    rgCaps.ItemIndex := Ord(Capitalisation);
  end;
end;

procedure TfAsm.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.SetAsm do
  begin
    StatementIndentEnabled := cbStatementIndent.Checked;
    StatementIndent := edtStatementIndent.Value;

    ParamsIndentEnabled := cbParamsIndent.Checked;
    ParamsIndent := edtParamsIndent.Value;

    BreaksAfterLabelEnabled := cbBreaksAfterLabelEnabled.Checked;
    BreaksAfterLabel := edtBreaksAfterLabel.Value;

    Capitalisation := TCapitalisationType(rgCaps.ItemIndex);
  end;
end;

class function TfAsm.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfAsm, JCFOptionAsm, JCFOptionClarify);
end.
