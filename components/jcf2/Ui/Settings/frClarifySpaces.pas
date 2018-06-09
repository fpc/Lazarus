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
  Classes, StdCtrls, ExtCtrls, Spin,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifySpaces }

  TfClarifySpaces = class(TAbstractIDEOptionsEditor)
    cbFixSpacing: TCheckBox;
    cbSpaceClassHeritage: TCheckBox;
    gbColon: TGroupBox;
    lblSpaceBeforeColonVar: TLabel;
    lblSpacesBeforeColonClassVar: TLabel;
    lblSpaceBeforeColonFn: TLabel;
    lblSpaceBeforeColonParam: TLabel;
    eSpaceBeforeColonVar: TSpinEdit;
    eSpaceBeforeColonParam: TSpinEdit;
    eSpaceBeforeColonFn: TSpinEdit;
    eSpacesBeforeColonClassVar: TSpinEdit;
    gbTabs: TGroupBox;
    cbTabsToSpaces: TCheckBox;
    cbSpacesToTabs: TCheckBox;
    Label1: TLabel;
    edtSpacesPerTab: TSpinEdit;
    Label3: TLabel;
    edtSpacesForTab: TSpinEdit;
    eSpacesBeforeCaseLabel: TSpinEdit;
    eSpacesBeforeLabel: TSpinEdit;
    lblSpacesBeforeCaseLabel: TLabel;
    lbSpacesBeforeLabel: TLabel;
    cbMaxSpaces: TCheckBox;
    edtMaxSpacesInCode: TSpinEdit;
    rgOperators: TRadioGroup;
    GroupBoxInsertSpaceBeforeBracket: TGroupBox;
    cbInsertSpaceBeforeBracketinFunctionDeclaration: TCheckBox;
    cbInsertSpaceBeforeBracketinFunctionCall: TCheckBox;
    cbBeforeOpenSquareBracketInExpression: TCheckBox;
    GroupBoxSpacesInsideBrackets: TGroupBox;
    CheckBoxInsertSpaceBeforeEnd: TCheckBox;
    cbInsertSpaceAfterOpen: TCheckBox;
    eSpacesBeforeColonGeneric: TSpinEdit;
    lblSpacesBeforeColonGeneric: TLabel;
    eSpaceBeforeColonConst: TSpinEdit;
    lblSpaceBeforeColonConst: TLabel;
    eSpacesBeforeColonRecordField: TSpinEdit;
    lblSpacesBeforeColonRecordField: TLabel;
    cbMoveSpacesToBeforeColon: TCheckBox;
    procedure cbTabsToSpacesClick(Sender: TObject);
    procedure cbSpacesToTabsClick(Sender: TObject);
    procedure cbMaxSpacesClick(Sender: TObject);
    procedure FrameResize(Sender:TObject);
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
  JcfSettings, SettingsTypes, jcfuiconsts;

constructor TfClarifySpaces.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_SPACES;
end;

function TfClarifySpaces.GetTitle: String;
begin
  Result := lisSpacesSpaces;
end;

procedure TfClarifySpaces.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  cbFixSpacing.Caption := lisSpacesFixSpacing;
  cbSpaceClassHeritage.Caption := lisSpacesSpaceBeforeClassHeritage;

  gbColon.Caption := lisSpacesSpacesBeforeColonIn;
  lblSpaceBeforeColonVar.Caption := lisSpacesVarDeclarations;
  lblSpaceBeforeColonConst.Caption := lisSpacesConstDeclarations;
  lblSpaceBeforeColonParam.Caption := lisSpacesProcedureParameters;
  lblSpaceBeforeColonFn.Caption := lisSpacesFunctionReturnTypes;
  lblSpacesBeforeColonClassVar.Caption := lisSpacesClassVariables;
  lblSpacesBeforeColonRecordField.Caption := lisSpacesRecordFields;
  lblSpacesBeforeCaseLabel.Caption := lisSpacesCaseLAbel;
  lbSpacesBeforeLabel.Caption := lisSpacesLabel;
  lblSpacesBeforeColonGeneric.Caption := lisSpacesInGeneric;

  rgOperators.Caption := lisSpacesSpacesAroundOperators;
  rgOperators.Items[0] := lisSpacesAlways;
  rgOperators.Items[1] := lisSpacesLeaveAsIs;
  rgOperators.Items[2] := lisSpacesNever;

  GroupBoxInsertSpaceBeforeBracket.Caption := lisSpacesInsertSpaceBeforeBracket;
  cbInsertSpaceBeforeBracketinFunctionDeclaration.Caption :=
    lisSpacesInFunctionDeclaration;
  cbInsertSpaceBeforeBracketinFunctionCall.Caption := lisSpacesInFunctionCall;
  cbBeforeOpenSquareBracketInExpression.Caption := lisSpacesBeforeInExpression;

  GroupBoxSpacesInsideBrackets.Caption := lisSpacesInsertSpaceInsideBrackets;
  cbInsertSpaceAfterOpen.Caption := lisSpacesAfterOpen;
  CheckBoxInsertSpaceBeforeEnd.Caption := lisSpacesBeforeEnd;

  cbMoveSpacesToBeforeColon.Caption := lisSpacesMoveSpacesToBeforeColon;

  gbTabs.Caption := lisSpacesTabCharacters;
  cbTabsToSpaces.Caption := lisSpacesTurnTabsToSpaces;
  Label1.Caption := lisSpacesSpacesPerTab;
  cbSpacesToTabs.Caption := lisSpacesTurnSpacesToTabs;
  Label3.Caption := lisSpacesSpacesForTab;

  cbMaxSpaces.Caption := lisSpacesMaxSpacesInCode;
end;

{-------------------------------------------------------------------------------
  worker procs }

procedure TfClarifySpaces.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Spaces do
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

procedure TfClarifySpaces.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Spaces do
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

class function TfClarifySpaces.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFormattingSettings;
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

procedure TfClarifySpaces.FrameResize(Sender:TObject);
begin
  gbColon.Width:=trunc(Width*0.5);
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfClarifySpaces, JCFOptionSpaces, JCFOptionClarify);
end.
