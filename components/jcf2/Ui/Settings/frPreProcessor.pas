unit frPreProcessor;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frPreProcessor.pas.
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

{ preprocessor symbols }

uses
  Classes, StdCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfPreProcessor }

  TfPreProcessor = class(TAbstractIDEOptionsEditor)
    mSymbols: TMemo;
    cbEnable: TCheckBox;
    lblSymbols: TLabel;
    mOptions: TMemo;
    lblCompilerOptions: TLabel;
    procedure cbEnableChange(Sender: TObject);
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

constructor TfPreProcessor.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY;
end;

function TfPreProcessor.GetTitle: String;
begin
  Result := lisPrpPreProcessor;
end;

procedure TfPreProcessor.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbEnable.Caption := lisPrpEnablePreprocessorParsing;
  lblSymbols.Caption := lisPrpSymbolsDefinedForConditionalCompilation;
  lblCompilerOptions.Caption := lisPrpCompilerOptionsDefinedForConditionalCompilation;
  cbEnableChange(cbEnable);
end;

procedure TfPreProcessor.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.PreProcessor do
  begin
    cbEnable.Checked := Enabled;
    mSymbols.Lines.Assign(DefinedSymbols);
    mOptions.Lines.Assign(DefinedOptions);
  end;
end;

procedure TfPreProcessor.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.PreProcessor do
  begin
    Enabled := cbEnable.Checked;
    DefinedSymbols.Assign(mSymbols.Lines);
    DefinedOptions.Assign(mOptions.Lines);
  end;
end;

class function TfPreProcessor.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
end;

procedure TfPreProcessor.FrameResize(Sender: TObject);
begin
  mSymbols.Height := (mSymbols.Height+mOptions.Height) div 2;
end;

procedure TfPreProcessor.cbEnableChange(Sender: TObject);
var
  b: Boolean;
begin
  b := (Sender as TCheckBox).Checked;
  mSymbols.Enabled := b;
  mOptions.Enabled := b;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfPreProcessor, JCFOptionPreProcessor);
end.
