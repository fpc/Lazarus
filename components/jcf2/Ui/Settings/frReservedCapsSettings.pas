{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frCapsSettings.pas, released April 2000.
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

unit frReservedCapsSettings;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfrReservedCapsSettings }

  TfrReservedCapsSettings = class(TAbstractIDEOptionsEditor)
    cbEnable: TCheckBox;
    rgReservedWords: TRadioGroup;
    rgOperators: TRadioGroup;
    rgTypes: TRadioGroup;
    rgConstants: TRadioGroup;
    rgDirectives: TRadioGroup;
    procedure cbEnableClick(Sender: TObject);
    procedure FrameResize(Sender:TObject);
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
  SettingsTypes, JcfSettings, jcfuiconsts;

constructor TfrReservedCapsSettings.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_CAPITALISATION;
end;

function TfrReservedCapsSettings.GetTitle: String;
begin
  Result := lisCapsCapitalisation;
end;

procedure TfrReservedCapsSettings.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnable.Caption := lisCapsEnableCapitalisationFixing;
  rgReservedWords.Caption := lisCapsReservedWords;
  rgReservedWords.Items[0] := lisObfsAllCapitals;
  rgReservedWords.Items[1] := lisObfsAllLowerCase;
  rgReservedWords.Items[2] := lisObfsMixedCase;
  rgReservedWords.Items[3] := lisObfsLeaveAlone;

  rgOperators.Caption := lisCapsOperators;
  rgOperators.Items[0] := lisObfsAllCapitals;
  rgOperators.Items[1] := lisObfsAllLowerCase;
  rgOperators.Items[2] := lisObfsMixedCase;
  rgOperators.Items[3] := lisObfsLeaveAlone;

  rgDirectives.Caption := lisCapsDirectives;
  rgDirectives.Items[0] := lisObfsAllCapitals;
  rgDirectives.Items[1] := lisObfsAllLowerCase;
  rgDirectives.Items[2] := lisObfsMixedCase;
  rgDirectives.Items[3] := lisObfsLeaveAlone;

  rgConstants.Caption := lisCapsConstants;
  rgConstants.Items[0] := lisObfsAllCapitals;
  rgConstants.Items[1] := lisObfsAllLowerCase;
  rgConstants.Items[2] := lisObfsMixedCase;
  rgConstants.Items[3] := lisObfsLeaveAlone;

  rgTypes.Caption := lisCapsTypes;
  rgTypes.Items[0] := lisObfsAllCapitals;
  rgTypes.Items[1] := lisObfsAllLowerCase;
  rgTypes.Items[2] := lisObfsMixedCase;
  rgTypes.Items[3] := lisObfsLeaveAlone;
end;

procedure TfrReservedCapsSettings.cbEnableClick(Sender: TObject);
begin
  rgReservedWords.Enabled := cbEnable.Checked;
  rgConstants.Enabled := cbEnable.Checked;
  rgDirectives.Enabled := cbEnable.Checked;
  rgOperators.Enabled := cbEnable.Checked;
  rgTypes.Enabled := cbEnable.Checked;
end;

procedure TfrReservedCapsSettings.FrameResize(Sender:TObject);
begin
  rgOperators.Width := (Width-18) div 2;
end;

procedure TfrReservedCapsSettings.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  cbEnable.Checked := FormattingSettings.Caps.Enabled;

  with FormattingSettings.Caps do
  begin
    rgReservedWords.ItemIndex := Ord(ReservedWords);
    rgConstants.ItemIndex := Ord(Constants);
    rgDirectives.ItemIndex := Ord(Directives);
    rgOperators.ItemIndex := Ord(Operators);
    rgTypes.ItemIndex := Ord(Types);
  end;
end;

procedure TfrReservedCapsSettings.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  FormattingSettings.Caps.Enabled := cbEnable.Checked;

  with FormattingSettings.Caps do
  begin
    ReservedWords := TCapitalisationType(rgReservedWords.ItemIndex);
    Constants := TCapitalisationType(rgConstants.ItemIndex);
    Directives := TCapitalisationType(rgDirectives.ItemIndex);
    Operators := TCapitalisationType(rgOperators.ItemIndex);
    Types := TCapitalisationType(rgTypes.ItemIndex);
  end;
end;

class function TfrReservedCapsSettings.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfrReservedCapsSettings, JCFOptionObjectPascal, JCFOptionClarify);
end.
