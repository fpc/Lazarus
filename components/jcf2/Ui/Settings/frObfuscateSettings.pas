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
  { delphi }
  Classes, Controls, Forms,
  StdCtrls, ExtCtrls,
  { local }
  frmBaseSettingsFrame;

type
  TfObfuscateSettings = class(TfrSettingsFrame)
    cbRemoveWhiteSpace: TCheckBox;
    cbRemoveComments: TCheckBox;
    rgObfuscateCaps: TRadioGroup;
    cbRebreak: TCheckBox;
    cbRemoveIndent: TCheckBox;
    cbEnabled: TCheckBox;
  private

  public
    constructor Create(AOwner: TComponent); override;

    procedure Read; override;
    procedure Write; override;

  end;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

uses JcfSettings, SettingsTypes, JcfHelp, SetObfuscate;

{ TfObfuscateSettings }

constructor TfObfuscateSettings.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_OBFUSCATE_SETTINGS;
end;

procedure TfObfuscateSettings.Read;
begin
  with FormatSettings.Obfuscate do
  begin
    cbEnabled.Checked      := Enabled;
    rgObfuscateCaps.ItemIndex := Ord(Caps);
    cbRemoveWhiteSpace.Checked := RemoveWhiteSpace;
    cbRemoveComments.Checked := RemoveComments;
    cbRemoveIndent.Checked := RemoveIndent;
    cbRebreak.Checked      := RebreakLines;
  end;
end;

procedure TfObfuscateSettings.Write;
begin
  with FormatSettings.Obfuscate do
  begin
    Enabled := cbEnabled.Checked;
    Caps    := TCapitalisationType(rgObfuscateCaps.ItemIndex);
    RemoveWhiteSpace := cbRemoveWhiteSpace.Checked;
    RemoveComments := cbRemoveComments.Checked;
    RemoveIndent := CbRemoveIndent.Checked;
    RebreakLines := cbRebreak.Checked;
  end;
end;

end.
