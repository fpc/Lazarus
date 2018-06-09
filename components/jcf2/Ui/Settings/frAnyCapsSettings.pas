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

unit frAnyCapsSettings;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfrAnyCapsSettings }

  TfrAnyCapsSettings = class(TAbstractIDEOptionsEditor)
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

constructor TfrAnyCapsSettings.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_CAPITALISATION;
end;

function TfrAnyCapsSettings.GetTitle: String;
begin
  Result := lisCapsAnyWordAnyWord;
end;

procedure TfrAnyCapsSettings.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnableAnyWords.Caption := lisCapsAnyWordEnable;
  Label1.Caption := lisCapsAnyWordSetCapitalisationOnTheseWords;
end;


procedure TfrAnyCapsSettings.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.SpecificWordCaps do
  begin
    cbEnableAnyWords.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;
end;

procedure TfrAnyCapsSettings.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.SpecificWordCaps do
  begin
    Enabled := cbEnableAnyWords.Checked;
    Words.Assign(mWords.Lines);
  end;
end;

class function TfrAnyCapsSettings.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfrAnyCapsSettings.cbEnableAnyWordsClick(Sender: TObject);
begin
  mWords.Enabled := cbEnableAnyWords.Checked;
end;

procedure TfrAnyCapsSettings.FrameResize(Sender: TObject);
begin
  mWords.Height := ClientHeight -
    (cbEnableAnyWords.Top + cbEnableAnyWords.Height + GUI_PAD);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfrAnyCapsSettings, JCFOptionAnyWord, JCFOptionObjectPascal);
end.
