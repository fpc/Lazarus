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
  { delphi }
  StdCtrls, Classes, Controls, Forms,
  { local }
  frmBaseSettingsFrame;

type
  TfWarnings = class(TfrSettingsFrame)
    cbWarningsOn: TCheckBox;
    cbWarnUnusedParams: TCheckBox;
    mIgnoreUnusedParams: TMemo;
    Label1: TLabel;
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

constructor TfWarnings.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_WARNINGS;
end;

procedure TfWarnings.Read;
begin
  with FormatSettings.Clarify do
  begin
    cbWarningsOn.Checked := Warnings;
    cbWarnUnusedParams.Checked  := WarnUnusedParams;
    mIgnoreUnusedParams.Lines.Assign(IgnoreUnusedParams);

  end;
end;

procedure TfWarnings.Write;
begin
  with FormatSettings.Clarify do
  begin
    Warnings := cbWarningsOn.Checked;
    WarnUnusedParams  := cbWarnUnusedParams.Checked;
    IgnoreUnusedParams.Assign(mIgnoreUnusedParams.Lines);
    IgnoreUnusedParams.Sort;
  end;
end;

procedure TfWarnings.FrameResize(Sender: TObject);
const
  PAD = 6;
begin
  inherited;

  mIgnoreUnusedParams.Height := ClientHeight - (mIgnoreUnusedParams.Top + PAD);
end;

end.
