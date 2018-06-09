{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frObfuscateSettings.pas, released April 2000.
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

unit frObfuscateSettings;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type
  { TfObfuscateSettings }

  TfObfuscateSettings = class(TAbstractIDEOptionsEditor)
    cbRemoveWhiteSpace: TCheckBox;
    cbRemoveComments: TCheckBox;
    rgObfuscateCaps: TRadioGroup;
    cbRebreak: TCheckBox;
    cbRemoveIndent: TCheckBox;
    cbEnabled: TCheckBox;
    procedure cbEnabledChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  JcfSettings, SettingsTypes, jcfuiconsts;

{ TfObfuscateSettings }

procedure TfObfuscateSettings.cbEnabledChange(Sender: TObject);
var
  b: Boolean;
begin
  b := (Sender as TCheckBox).Checked;
  rgObfuscateCaps.Enabled := b;
  cbRemoveWhiteSpace.Enabled := b;
  cbRemoveComments.Enabled := b;
  cbRemoveIndent.Enabled := b;
  cbRebreak.Enabled := b;
end;

constructor TfObfuscateSettings.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_OBFUSCATE_SETTINGS;
end;

function TfObfuscateSettings.GetTitle: String;
begin
  Result := lisObfsObfuscate;
end;

procedure TfObfuscateSettings.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnabled.Caption := lisObfsObfuscateMode;

  rgObfuscateCaps.Caption := lisObfsObfuscateWordCaps;
  rgObfuscateCaps.Items[0] := lisObfsAllCapitals;
  rgObfuscateCaps.Items[1] := lisObfsAllLowerCase;
  rgObfuscateCaps.Items[2] := lisObfsMixedCase;
  rgObfuscateCaps.Items[3] := lisObfsLeaveAlone;

  cbRemoveWhiteSpace.Caption := lisObfsRemoveWhiteSpace;
  cbRemoveComments.Caption := lisObfsRemoveComments;
  cbRemoveIndent.Caption := lisObfsRemoveIndent;
  cbRebreak.Caption := lisObfsRebreakLines;
  cbEnabledChange(cbEnabled);
end;

procedure TfObfuscateSettings.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Obfuscate do
  begin
    cbEnabled.Checked      := Enabled;
    rgObfuscateCaps.ItemIndex := Ord(Caps);
    cbRemoveWhiteSpace.Checked := RemoveWhiteSpace;
    cbRemoveComments.Checked := RemoveComments;
    cbRemoveIndent.Checked := RemoveIndent;
    cbRebreak.Checked      := RebreakLines;
  end;
end;

procedure TfObfuscateSettings.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Obfuscate do
  begin
    Enabled := cbEnabled.Checked;
    Caps    := TCapitalisationType(rgObfuscateCaps.ItemIndex);
    RemoveWhiteSpace := cbRemoveWhiteSpace.Checked;
    RemoveComments := cbRemoveComments.Checked;
    RemoveIndent := CbRemoveIndent.Checked;
    RebreakLines := cbRebreak.Checked;
  end;
end;

class function TfObfuscateSettings.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfObfuscateSettings, JCFOptionObfuscate);
end.
