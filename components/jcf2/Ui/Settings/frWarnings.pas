unit frWarnings;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frWarnings.pas, released June 2005.
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
  StdCtrls, Classes,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfWarnings }

  TfWarnings = class(TAbstractIDEOptionsEditor)
    cbWarningsOn: TCheckBox;
    cbWarnUnusedParams: TCheckBox;
    mIgnoreUnusedParams: TMemo;
    Label1: TLabel;
    procedure cbWarningsOnChange(Sender: TObject);
    procedure cbWarnUnusedParamsChange(Sender: TObject);
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

constructor TfWarnings.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_WARNINGS;
end;

function TfWarnings.GetTitle: String;
begin
  Result := lisWarningsWarnings;
end;

procedure TfWarnings.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbWarningsOn.Caption := lisWarningsWarningsOn;
  cbWarnUnusedParams.Caption := lisWarningsWarnAboutUnusedParameters;
  Label1.Caption := lisWarningsIgnoreUnusedParametersNamed;
  cbWarningsOnChange(cbWarningsOn);
end;

procedure TfWarnings.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Clarify do
  begin
    cbWarningsOn.Checked := Warnings;
    cbWarnUnusedParams.Checked  := WarnUnusedParams;
    mIgnoreUnusedParams.Lines.Assign(IgnoreUnusedParams);
  end;
end;

procedure TfWarnings.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Clarify do
  begin
    Warnings := cbWarningsOn.Checked;
    WarnUnusedParams  := cbWarnUnusedParams.Checked;
    IgnoreUnusedParams.Assign(mIgnoreUnusedParams.Lines);
    IgnoreUnusedParams.Sort;
  end;
end;

class function TfWarnings.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfWarnings.FrameResize(Sender: TObject);
const
  PAD = 6;
begin
  inherited;
  mIgnoreUnusedParams.Height := ClientHeight - (mIgnoreUnusedParams.Top + PAD);
end;

procedure TfWarnings.cbWarningsOnChange(Sender: TObject);
var
  b: Boolean;
begin
  b := (Sender as TCheckBox).Checked;
  cbWarnUnusedParams.Enabled := b;
  mIgnoreUnusedParams.Enabled := b and cbWarnUnusedParams.Checked;
end;

procedure TfWarnings.cbWarnUnusedParamsChange(Sender: TObject);
var
  b: Boolean;
begin
  b := (Sender as TCheckBox).Checked;
  mIgnoreUnusedParams.Enabled := b;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfWarnings, JCFOptionWarnings, JCFOptionClarify);
end.
