{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frReplace.pas, released April 2000.
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

unit frReplace;

{$I JcfGlobal.inc}

interface

uses
  Classes, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfReplace }

  TfReplace = class(TAbstractIDEOptionsEditor)
    cbEnable: TCheckBox;
    mWords: TMemo;
    lblWordList: TLabel;
    procedure cbEnableClick(Sender: TObject);
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

{ TfReplace }

constructor TfReplace.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_FIND_AND_REPLACE;
end;

function TfReplace.GetTitle: String;
begin
  Result := lisFindReplaceFindAndReplace;
end;

procedure TfReplace.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnable.Caption := lisFindReplaceEnableFindAndReplace;
  lblWordList.Caption := lisFindReplaceWordList;
end;

procedure TfReplace.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Replace do
  begin
    cbEnable.Checked := Enabled;
    mWords.Lines.Assign(Words);
  end;
  cbEnableClick(nil);
end;

procedure TfReplace.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Replace do
  begin
    Enabled := cbEnable.Checked;
    Words.Assign(mWords.Lines);
    SplitWords;
  end;
end;

class function TfReplace.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfReplace.cbEnableClick(Sender: TObject);
begin
  mWords.Enabled := cbEnable.Checked;
end;

procedure TfReplace.FrameResize(Sender: TObject);
const
  PAD = 4;
begin
  mWords.Height := ClientHeight - (lblWordList.Top + lblWordList.Height + PAD);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfReplace, JCFOptionFindAndReplace, JCFOptionClarify);
end.
