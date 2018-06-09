{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frIdentifierCapsSettings.pas, released June 2005.
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

unit frNotIdentifierCapsSettings;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfNotIdentifierCapsSettings }

  TfNotIdentifierCapsSettings = class(TAbstractIDEOptionsEditor)
    Label1: TLabel;
    cbEnableAnyWords: TCheckBox;
    mWords: TMemo;
    procedure cbEnableAnyWordsClick(Sender: TObject);
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

constructor TfNotIdentifierCapsSettings.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_CAPITALISATION;
end;

function TfNotIdentifierCapsSettings.GetTitle: String;
begin
  Result := lisCapsNotIdentifiersNotIdentifiers;
end;

procedure TfNotIdentifierCapsSettings.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnableAnyWords.Caption := lisCapsAnyWordEnable;
  Label1.Caption := lisCapsNotIdentifiersSetCapitalisationOnTheseNonIdentifiers;
end;


procedure TfNotIdentifierCapsSettings.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.NotIdentifierCaps do
  begin
    cbEnableAnyWords.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;
end;

procedure TfNotIdentifierCapsSettings.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.NotIdentifierCaps do
  begin
    Enabled := cbEnableAnyWords.Checked;
    Words.Assign(mWords.Lines);
  end;
end;

class function TfNotIdentifierCapsSettings.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfNotIdentifierCapsSettings.cbEnableAnyWordsClick(Sender: TObject);
begin
  mWords.Enabled := cbEnableAnyWords.Checked;
end;

procedure TfNotIdentifierCapsSettings.FrameResize(Sender: TObject);
begin
  mWords.Height := ClientHeight -
    (cbEnableAnyWords.Top + cbEnableAnyWords.Height + GUI_PAD);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfNotIdentifierCapsSettings, JCFOptionNotIdentifiers, JCFOptionObjectPascal);
end.
