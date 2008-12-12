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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, 
  { local}
  frmBaseSettingsFrame, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfAsm = class(TfrSettingsFrame)
    rgCaps: TRadioGroup;
    gbStatementIndent: TGroupBox;
    edtStatementIndent: TJvValidateEdit;
    cbStatementIndent: TCheckBox;
    gbBreaksAfterLabel: TGroupBox;
    cbBreaksAfterLabelEnabled: TCheckBox;
    edtBreaksAfterLabel: TJvValidateEdit;
    Label7: TLabel;
    Label1: TLabel;
    gbParamsIndent: TGroupBox;
    Label2: TLabel;
    edtParamsIndent: TJvValidateEdit;
    cbParamsIndent: TCheckBox;
  private
  public
    procedure Read; override;
    procedure Write; override;

  end;

implementation

uses
  { jcf }
  JcfHelp, JcfSettings,
  { local }
  Capitalisation, SettingsTypes, SetAsm;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TfAsm.Read;
begin

  with FormatSettings.SetAsm do
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

procedure TfAsm.Write;
begin

  with FormatSettings.SetAsm do
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

end.
