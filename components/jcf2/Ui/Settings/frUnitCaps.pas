unit frUnitCaps;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frUnitCaps.pas
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
  Classes, Controls, Forms, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfrUnitNameCaps }

  TfrUnitNameCaps = class(TAbstractIDEOptionsEditor)
    mWords: TMemo;
    cbEnableAnyWords: TCheckBox;
    Label1: TLabel;
    procedure FrameResize(Sender: TObject);
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
  JcfSettings, jcfuiconsts;

constructor TfrUnitNameCaps.Create(AOwner: TComponent);
begin
  inherited;

end;

function TfrUnitNameCaps.GetTitle: String;
begin
  Result := lisCapsUnitNamesUnitNames;
end;

procedure TfrUnitNameCaps.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnableAnyWords.Caption := lisCapsAnyWordEnable;
  Label1.Caption := lisCapsUnitNamesSetCapitalisationOnTheseUnitNames;
end;

procedure TfrUnitNameCaps.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.UnitNameCaps do
  begin
    cbEnableAnyWords.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;

end;

procedure TfrUnitNameCaps.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.UnitNameCaps do
  begin
    Enabled := cbEnableAnyWords.Checked;
    Words.Assign(mWords.Lines);
  end;

end;

class function TfrUnitNameCaps.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfrUnitNameCaps.FrameResize(Sender: TObject);
begin
  mWords.Height := ClientHeight -
    (cbEnableAnyWords.Top + cbEnableAnyWords.Height + GUI_PAD);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfrUnitNameCaps, JCFOptionUnitName, JCFOptionObjectPascal);
end.
