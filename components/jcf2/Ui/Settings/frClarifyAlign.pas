{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is frClarify.pas, released April 2000.
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

unit frClarifyAlign;

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
  TfClarifyAlign = class(TfrSettingsFrame)
    cbInterfaceOnly: TCheckBox;
    edtMaxVariance: TJvValidateEdit;
    edtMaxColumn: TJvValidateEdit;
    edtMinColumn: TJvValidateEdit;
    Label6: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    gbWhat: TGroupBox;
    cbAlignAsign: TCheckBox;
    cbAlignConst: TCheckBox;
    cbAlignVar: TCheckBox;
    cbAlignTypedef: TCheckBox;
    cbAlignComment: TCheckBox;
    Label1: TLabel;
    eMaxUnaligned: TJvValidateEdit;
    cbAlignField: TCheckBox;
    edtMaxVarianceInterface: TJvValidateEdit;
    Label2: TLabel;
    procedure edtMinColumnExit(Sender: TObject);
    procedure edtMaxColumnExit(Sender: TObject);
  private
    procedure CheckMax;
    procedure CheckMin;

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

uses JcfSettings, JcfHelp, SetAlign;

constructor TfClarifyAlign.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_ALIGN;
end;


{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyAlign.CheckMin;
begin
  if (edtMaxColumn = nil) or (edtMaxColumn = nil) then
    exit;

  if edtMaxColumn.Value < edtMinColumn.Value then
    edtMaxColumn.Value := edtMinColumn.Value;
end;

procedure TfClarifyAlign.CheckMax;
begin
  if (edtMaxColumn = nil) or (edtMaxColumn = nil) then
    exit;

  if edtMaxColumn.Value < edtMinColumn.Value then
    edtMinColumn.Value := edtMaxColumn.Value;
end;

procedure TfClarifyAlign.Read;
begin
  with FormatSettings.Align do
  begin
    cbAlignAsign.Checked   := AlignAssign;
    cbAlignConst.Checked   := AlignConst;
    cbAlignVar.Checked     := AlignVar;
    cbAlignTypedef.Checked := AlignTypeDef;
    cbAlignComment.Checked := AlignComment;
    cbAlignField.Checked := AlignField;

    cbInterfaceOnly.Checked := InterfaceOnly;

    edtMinColumn.Value   := MinColumn;
    edtMaxColumn.Value   := MaxColumn;
    edtMaxVariance.Value := MaxVariance;
    edtMaxVarianceInterface.Value := MaxVarianceInterface;
    eMaxUnaligned.Value  := MaxUnalignedStatements;
  end;
end;

procedure TfClarifyAlign.Write;
begin
  with FormatSettings.Align do
  begin
    AlignAssign  := cbAlignAsign.Checked;
    AlignConst   := cbAlignConst.Checked;
    AlignVar     := cbAlignVar.Checked;
    AlignTypeDef := cbAlignTypedef.Checked;
    AlignComment := cbAlignComment.Checked;
    AlignField := cbAlignField.Checked;

    InterfaceOnly := cbInterfaceOnly.Checked;

    MinColumn   := edtMinColumn.Value;
    MaxColumn   := edtMaxColumn.Value;
    MaxVariance := edtMaxVariance.Value;
    MaxVarianceInterface := edtMaxVarianceInterface.Value;
    MaxUnalignedStatements := eMaxUnaligned.Value;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifyAlign.edtMinColumnExit(Sender: TObject);
begin
  CheckMin;
end;

procedure TfClarifyAlign.edtMaxColumnExit(Sender: TObject);
begin
  inherited;
  CheckMax;
end;

end.
