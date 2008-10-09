{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frClarifyCaseBlocks.pas, released June 2004.
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

unit frClarifyCaseBlocks;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Classes, Controls, Forms,
  StdCtrls, ExtCtrls,
  { local}
  frmBaseSettingsFrame;

type
  TfClarifyCaseBlocks = class(TfrSettingsFrame)
    rgLabelBegin: TRadioGroup;
    rgLabel: TRadioGroup;
    Label1: TLabel;
    rgCaseLabel: TRadioGroup;
    rgElseCase: TRadioGroup;
    rgCaseBegin: TRadioGroup;
    rgCaseElseBegin: TRadioGroup;
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

uses JcfSettings, SettingsTypes, JcfHelp;

constructor TfClarifyCaseBlocks.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_BLOCKS;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyCaseBlocks.Read;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    rgLabelBegin.ItemIndex := Ord(LabelBeginStyle);
    rgLabel.ItemIndex      := Ord(LabelStyle);

    rgCaseLabel.ItemIndex  := Ord(CaseLabelStyle);
    rgCaseBegin.ItemIndex  := Ord(CaseBeginStyle);
    rgElseCase.ItemIndex := Ord(CaseElseStyle);
    rgCaseElseBegin.ItemIndex := Ord(CaseElseBeginStyle);
  end;
end;

procedure TfClarifyCaseBlocks.Write;
begin
  with FormatSettings.Returns do
  begin
    { block styles }
    LabelBeginStyle := TTriOptionStyle(rgLabelBegin.ItemIndex);
    LabelStyle      := TTriOptionStyle(rgLabel.ItemIndex);

    CaseLabelStyle := TTriOptionStyle(rgCaseLabel.ItemIndex);
    CaseBeginStyle := TTriOptionStyle(rgCaseBegin.ItemIndex);
    CaseElseStyle  := TTriOptionStyle(rgElseCase.ItemIndex);
    CaseElseBeginStyle  := TTriOptionStyle(rgCaseElseBegin.ItemIndex);
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

end.
