unit frClarifyIndent;

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
  TfClarifyIndent = class(TfrSettingsFrame)
    Label2: TLabel;
    edtIndentSpaces: TJvValidateEdit;
    gbOptions: TGroupBox;
    cbIndentBeginEnd: TCheckBox;
    eIndentBeginEndSpaces: TJvValidateEdit;
    cbHasFirstLevelIndent: TCheckBox;
    eFirstLevelIndent: TJvValidateEdit;
    cbKeepWithInProc: TCheckBox;
    cbKeepWithInGlobals: TCheckBox;
    cbKeepWithInClassDef: TCheckBox;
    cbKeepWithElsewhere: TCheckBox;
    cbIndentIfElse: TCheckBox;
    cbIndentCaseElse: TCheckBox;
    cbIndentLibraryProcs: TCheckBox;
    cbIndentProcedureBody: TCheckBox;
    cbIndentNestedTypes: TCheckBox;
    cbIndentVarAndConstInClass: TCheckBox;
    procedure cbIndentBeginEndClick(Sender: TObject);
    procedure cbHasFirstLevelIndentClick(Sender: TObject);
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

uses JcfHelp, JcfSettings, SetIndent;

constructor TfClarifyIndent.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_INDENTATION;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyIndent.Read;
begin
  with FormatSettings.Indent do
  begin
    edtIndentSpaces.Value    := IndentSpaces;
    cbIndentBeginEnd.Checked := IndentBeginEnd;
    eIndentBeginEndSpaces.Value := IndentBeginEndSpaces;

    cbIndentLibraryProcs.Checked := IndentLibraryProcs;

    cbHasFirstLevelIndent.Checked := HasFirstLevelIndent;
    eFirstLevelIndent.Value := FirstLevelIndent;

    cbKeepWithInProc.Checked     := KeepCommentsWithCodeInProcs;
    cbKeepWithInGlobals.Checked  := KeepCommentsWithCodeInGlobals;
    cbKeepWithInClassDef.Checked := KeepCommentsWithCodeInClassDef;
    cbKeepWithElsewhere.Checked  := KeepCommentsWithCodeElsewhere;
    cbIndentIfElse.Checked := IndentElse;
    cbIndentCaseElse.Checked := IndentCaseElse;
    cbIndentProcedureBody.Checked := IndentProcedureBody;

    cbIndentNestedTypes.Checked := IndentNestedTypes;
    cbIndentVarAndConstInClass.Checked := IndentVarAndConstInClass;
  end;

  cbIndentBeginEndClick(nil);
  cbHasFirstLevelIndentClick(nil);
end;

procedure TfClarifyIndent.Write;
begin

  with FormatSettings.Indent do
  begin
    IndentSpaces   := edtIndentSpaces.Value;
    IndentBeginEnd := cbIndentBeginEnd.Checked;
    IndentBeginEndSpaces := eIndentBeginEndSpaces.Value;

    IndentLibraryProcs := cbIndentLibraryProcs.Checked;

    HasFirstLevelIndent := cbHasFirstLevelIndent.Checked;
    FirstLevelIndent    := eFirstLevelIndent.Value;

    KeepCommentsWithCodeInProcs    := cbKeepWithInProc.Checked;
    KeepCommentsWithCodeInGlobals  := cbKeepWithInGlobals.Checked;
    KeepCommentsWithCodeInClassDef := cbKeepWithInClassDef.Checked;
    KeepCommentsWithCodeElsewhere  := cbKeepWithElsewhere.Checked;
    IndentElse := cbIndentIfElse.Checked;
    IndentCaseElse := cbIndentCaseElse.Checked;
    IndentProcedureBody := cbIndentProcedureBody.Checked;

    IndentNestedTypes := cbIndentNestedTypes.Checked;
    IndentVarAndConstInClass := cbIndentVarAndConstInClass.Checked;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifyIndent.cbIndentBeginEndClick(Sender: TObject);
begin
  eIndentBeginEndSpaces.Enabled := cbIndentBeginEnd.Checked;
end;

procedure TfClarifyIndent.cbHasFirstLevelIndentClick(Sender: TObject);
begin
  eFirstLevelIndent.Enabled := cbHasFirstLevelIndent.Checked;
end;

end.
