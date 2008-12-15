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

unit frClarifySpaces;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Classes, Controls, Forms,
  StdCtrls,
  { JVCL }
  JvEdit, JvExStdCtrls, JvValidateEdit,
  { local}
  frmBaseSettingsFrame, ExtCtrls;

type
  TfClarifySpaces = class(TfrSettingsFrame)
    cbFixSpacing: TCheckBox;
    cbSpaceClassHeritage: TCheckBox;
    gbColon: TGroupBox;
    lblSpaceBeforeColonVar: TLabel;
    lblSpacesBeforeColonClassVar: TLabel;
    lblSpaceBeforeColonFn: TLabel;
    lblSpaceBeforeColonParam: TLabel;
    eSpaceBeforeColonVar: TJvValidateEdit;
    eSpaceBeforeColonParam: TJvValidateEdit;
    eSpaceBeforeColonFn: TJvValidateEdit;
    eSpacesBeforeColonClassVar: TJvValidateEdit;
    gbTabs: TGroupBox;
    cbTabsToSpaces: TCheckBox;
    cbSpacesToTabs: TCheckBox;
    Label1: TLabel;
    edtSpacesPerTab: TJvValidateEdit;
    Label3: TLabel;
    edtSpacesForTab: TJvValidateEdit;
    eSpacesBeforeCaseLabel: TJvValidateEdit;
    eSpacesBeforeLabel: TJvValidateEdit;
    lblSpacesBeforeCaseLabel: TLabel;
    lbSpacesBeforeLabel: TLabel;
    cbMaxSpaces: TCheckBox;
    edtMaxSpacesInCode: TJvValidateEdit;
    rgOperators: TRadioGroup;
    GroupBoxInsertSpaceBeforeBracket: TGroupBox;
    cbInsertSpaceBeforeBracketinFunctionDeclaration: TCheckBox;
    cbInsertSpaceBeforeBracketinFunctionCall: TCheckBox;
    cbBeforeOpenSquareBracketInExpression: TCheckBox;
    GroupBoxSpacesInsideBrackets: TGroupBox;
    CheckBoxInsertSpaceBeforeEnd: TCheckBox;
    cbInsertSpaceAfterOpen: TCheckBox;
    eSpacesBeforeColonGeneric: TJvValidateEdit;
    lblSpacesBeforeColonGeneric: TLabel;
    eSpaceBeforeColonConst: TJvValidateEdit;
    lblSpaceBeforeColonConst: TLabel;
    eSpacesBeforeColonRecordField: TJvValidateEdit;
    lblSpacesBeforeColonRecordField: TLabel;
    cbMoveSpacesToBeforeColon: TCheckBox;
    procedure cbTabsToSpacesClick(Sender: TObject);
    procedure cbSpacesToTabsClick(Sender: TObject);
    procedure cbMaxSpacesClick(Sender: TObject);
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

uses JcfHelp, JcfSettings, SetSpaces, SettingsTypes;

constructor TfClarifySpaces.Create(AOwner: TComponent);
begin
  inherited;
  fiHelpContext := HELP_CLARIFY_SPACES;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifySpaces.Read;
begin
  with FormatSettings.Spaces do
  begin
    cbTabsToSpaces.Checked := TabsToSpaces;
    cbSpacesToTabs.Checked := SpacesToTabs;
    edtSpacesPerTab.Value  := SpacesPerTab;
    edtSpacesForTab.Value  := SpacesForTab;

    cbFixSpacing.Checked := FixSpacing;

    cbSpaceClassHeritage.Checked := SpaceBeforeClassHeritage;

    eSpaceBeforeColonVar.Value   := SpacesBeforeColonVar;
    eSpaceBeforeColonConst.Value :=  SpacesBeforeColonConst;
    eSpaceBeforeColonParam.Value := SpacesBeforeColonParam;
    eSpaceBeforeColonFn.Value    := SpacesBeforeColonFn;
    eSpacesBeforeColonClassVar.Value := SpacesBeforeColonClassVar;

    eSpacesBeforeColonRecordField.Value := SpacesBeforeColonRecordField;
    eSpacesBeforeCaseLabel.Value := SpacesBeforeColonCaseLabel;
    eSpacesBeforeLabel.Value     := SpacesBeforeColonLabel;
    eSpacesBeforeColonGeneric.Value := SpacesBeforeColonInGeneric;

    cbMaxSpaces.Checked      := UseMaxSpacesInCode;
    edtMaxSpacesInCode.Value := MaxSpacesInCode;

    rgOperators.ItemIndex := Ord(SpaceForOperator);

    cbInsertSpaceBeforeBracketinFunctionDeclaration.Checked := SpaceBeforeOpenBracketsInFunctionDeclaration;
    cbInsertSpaceBeforeBracketinFunctionCall.Checked := SpaceBeforeOpenBracketsInFunctionCall;
    cbBeforeOpenSquareBracketInExpression.Checked := SpaceBeforeOpenSquareBracketsInExpression;

    cbInsertSpaceAfterOpen.Checked := SpaceAfterOpenBrackets;
    CheckBoxInsertSpaceBeforeEnd.Checked := SpaceBeforeCloseBrackets;

    cbMoveSpacesToBeforeColon.Checked := MoveSpaceToBeforeColon;
  end;

  cbTabsToSpacesClick(nil);
  cbSpacesToTabsClick(nil);
  cbMaxSpacesClick(nil);
end;

procedure TfClarifySpaces.Write;
begin
  with FormatSettings.Spaces do
  begin
    TabsToSpaces := cbTabsToSpaces.Checked;
    SpacesToTabs := cbSpacesToTabs.Checked;

    SpacesPerTab := edtSpacesPerTab.Value;
    SpacesForTab := edtSpacesForTab.Value;

    FixSpacing := cbFixSpacing.Checked;

    SpaceBeforeClassHeritage := cbSpaceClassHeritage.Checked;

    SpacesBeforeColonVar   := eSpaceBeforeColonVar.Value;
    SpacesBeforeColonConst := eSpaceBeforeColonConst.Value;


    SpacesBeforeColonParam := eSpaceBeforeColonParam.Value;
    SpacesBeforeColonFn    := eSpaceBeforeColonFn.Value;
    SpacesBeforeColonClassVar := eSpacesBeforeColonClassVar.Value;

    SpacesBeforeColonRecordField := eSpacesBeforeColonRecordField.Value;
    SpacesBeforeColonCaseLabel := eSpacesBeforeCaseLabel.Value;
    SpacesBeforeColonLabel := eSpacesBeforeLabel.Value;
    SpacesBeforeColonInGeneric := eSpacesBeforeColonGeneric.Value;

    UseMaxSpacesInCode := cbMaxSpaces.Checked;
    MaxSpacesInCode    := edtMaxSpacesInCode.Value;

    SpaceForOperator := TTriOptionStyle(rgOperators.ItemIndex);

    SpaceBeforeOpenBracketsInFunctionDeclaration := cbInsertSpaceBeforeBracketinFunctionDeclaration.Checked;
    SpaceBeforeOpenBracketsInFunctionCall := cbInsertSpaceBeforeBracketinFunctionCall.Checked;
    SpaceBeforeOpenSquareBracketsInExpression := cbBeforeOpenSquareBracketInExpression.Checked;

    SpaceAfterOpenBrackets := cbInsertSpaceAfterOpen.Checked;
    SpaceBeforeCloseBrackets := CheckBoxInsertSpaceBeforeEnd.Checked;

    MoveSpaceToBeforeColon := cbMoveSpacesToBeforeColon.Checked;
  end;
end;

{-------------------------------------------------------------------------------
  event handlers }

procedure TfClarifySpaces.cbTabsToSpacesClick(Sender: TObject);
begin
  edtSpacesPerTab.Enabled := cbTabsToSpaces.Checked;
end;

procedure TfClarifySpaces.cbSpacesToTabsClick(Sender: TObject);
begin
  edtSpacesForTab.Enabled := cbSpacesToTabs.Checked;
end;

procedure TfClarifySpaces.cbMaxSpacesClick(Sender: TObject);
begin
  edtMaxSpacesInCode.Enabled := cbMaxSpaces.Checked;
end;

end.
