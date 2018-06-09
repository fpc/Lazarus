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
  Classes, StdCtrls, Spin,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifyIndent }

  TfClarifyIndent = class(TAbstractIDEOptionsEditor)
    Label2: TLabel;
    edtIndentSpaces: TSpinEdit;
    gbOptions: TGroupBox;
    cbIndentBeginEnd: TCheckBox;
    eIndentBeginEndSpaces: TSpinEdit;
    cbHasFirstLevelIndent: TCheckBox;
    eFirstLevelIndent: TSpinEdit;
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

constructor TfClarifyIndent.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_INDENTATION;
end;

function TfClarifyIndent.GetTitle: String;
begin
  Result := lisIndentIndentation;
end;

procedure TfClarifyIndent.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  Label2.Caption := lisIndentBlockIndentationSpaces;
  gbOptions.Caption := lisIndentOptions;
  cbIndentBeginEnd.Caption := lisIndentExtraIndentForBeginEnd;
  cbHasFirstLevelIndent.Caption := lisIndentDifferentIndentForFirstLevel;
  cbKeepWithInProc.Caption := lisIndentKeepSingleLineCommentsWithCodeInProcs;
  cbKeepWithInGlobals.Caption :=
    lisIndentKeepSingleLineCommentsWithCodeInGlobals;
  cbKeepWithInClassDef.Caption :=
    lisIndentKeepSingleLineCommentsWithCodeInClassDefs;
  cbKeepWithElsewhere.Caption :=
    lisIndentKeepSingleLineCommentsWithCodeElsewhere;
  cbIndentIfElse.Caption := lisIndentExtraIndentForIfElseBlocks;
  cbIndentCaseElse.Caption := lisIndentExtraIndentForCaseElseBlocks;
  cbIndentLibraryProcs.Caption := lisIndentIndentForProceduresInLibrary;
  cbIndentProcedureBody.Caption := lisIndentIndentForProcedureBody;
  cbIndentNestedTypes.Caption := lisIndentIndentNestedTypes;
  cbIndentVarAndConstInClass.Caption := lisIndentIndentVarAndConstInClass;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifyIndent.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Indent do
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

procedure TfClarifyIndent.WriteSettings(AOptions: TAbstractIDEOptions);
begin

  with FormattingSettings.Indent do
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

class function TfClarifyIndent.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
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

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarifyIndent, JCFOptionIndentation, JCFOptionClarify);
end.
