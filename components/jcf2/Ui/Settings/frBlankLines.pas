{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frBlankLines.pas, released Nov 2003.
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
unit frBlankLines;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Classes, Controls, Forms,
  StdCtrls,
  { JVCL }
  JvEdit, JvExStdCtrls, JvValidateEdit,
  { local}
  frmBaseSettingsFrame;


type
  TfBlankLines = class(TfrSettingsFrame)
    Label1: TLabel;
    eNumReturnsAfterFinalEnd: TJvValidateEdit;
    cbRemoveConsecutiveBlankLines: TCheckBox;
    edtMaxConsecutiveBlankLines: TJvValidateEdit;
    Label2: TLabel;
    gbRemoveBlankLines: TGroupBox;
    cbRemoveBlockBlankLines: TCheckBox;
    cbRemoveBlankLinesAfterProcHeader: TCheckBox;
    cbRemoveVarBlankLines: TCheckBox;
    edtLinesBeforeProcedure: TJvValidateEdit;
    Label3: TLabel;
    Label4: TLabel;
    edtMaxBlankLinesInSection: TJvValidateEdit;
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

uses
  { delphi }
  Math,
  { local }
  JcfSettings, SetReturns, JcfHelp;

constructor TfBlankLines.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_BLANK_LINES;
end;

procedure TfBlankLines.Read;
begin
  with FormatSettings.Returns do
  begin
    cbRemoveVarBlankLines.Checked   := RemoveVarBlankLines;
    cbRemoveBlankLinesAfterProcHeader.Checked := RemoveProcHeaderBlankLines;
    cbRemoveBlockBlankLines.Checked := RemoveBlockBlankLines;

    eNumReturnsAfterFinalEnd.Value := NumReturnsAfterFinalEnd;

    cbRemoveConsecutiveBlankLines.Checked := RemoveConsecutiveBlankLines;
    edtMaxConsecutiveBlankLines.Value     := MaxConsecutiveBlankLines;
    edtMaxBlankLinesInSection.Value := MaxBlankLinesInSection;

    edtLinesBeforeProcedure.Value := LinesBeforeProcedure;
  end;
end;

procedure TfBlankLines.Write;
begin
  with FormatSettings.Returns do
  begin
    RemoveVarBlankLines   := cbRemoveVarBlankLines.Checked;
    RemoveProcHeaderBlankLines := cbRemoveBlankLinesAfterProcHeader.Checked;
    RemoveBlockBlankLines := cbRemoveBlockBlankLines.Checked;

    NumReturnsAfterFinalEnd := eNumReturnsAfterFinalEnd.Value;

    RemoveConsecutiveBlankLines := cbRemoveConsecutiveBlankLines.Checked;
    // this value is always at least 2
    MaxConsecutiveBlankLines    := Max(edtMaxConsecutiveBlankLines.Value, 2);
    MaxBlankLinesInSection := edtMaxBlankLinesInSection.Value;


    LinesBeforeProcedure := edtLinesBeforeProcedure.Value;
  end;
end;

end.
