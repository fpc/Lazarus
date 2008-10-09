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
  Classes, Controls, Forms,
  StdCtrls, JvMemo,
   { local}frmBaseSettingsFrame, JvExStdCtrls;

type
  TfPreProcessor = class(TfrSettingsFrame)
    mSymbols: TJvMemo;
    cbEnable: TCheckBox;
    lblSymbols: TLabel;
    mOptions: TJvMemo;
    lblCompilerOptions: TLabel;
    procedure FrameResize(Sender: TObject);
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

uses JcfHelp, JcfSettings;

constructor TfPreProcessor.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY;
end;

procedure TfPreProcessor.Read;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    cbEnable.Checked := Enabled;
    mSymbols.Lines.Assign(DefinedSymbols);
    mOptions.Lines.Assign(DefinedOptions);
  end;

end;

procedure TfPreProcessor.Write;
begin
  inherited;
  with FormatSettings.PreProcessor do
  begin
    Enabled := cbEnable.Checked;
    DefinedSymbols.Assign(mSymbols.Lines);
    DefinedOptions.Assign(mOptions.Lines);
  end;
end;

procedure TfPreProcessor.FrameResize(Sender: TObject);
var
  liClientHeight: integer;
begin
  liClientHeight := ClientHeight -
    (cbEnable.Top + cbEnable.Height +
    lblCompilerOptions.Height + lblSymbols.Height +
    (GUI_PAD * 3));

  mSymbols.Height := (liClientHeight div 2);
  mSymbols.Left   := 0;
  mSymbols.Width  := ClientWidth;

  lblCompilerOptions.Top := mSymbols.Top + mSymbols.Height + GUI_PAD;
  mOptions.Top    := lblCompilerOptions.Top + lblCompilerOptions.Height + GUI_PAD;
  mOptions.Height := ClientHeight - mOptions.Top;
  mOptions.Left   := 0;
  mOptions.Width  := ClientWidth;

end;


end.
