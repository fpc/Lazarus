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

{$mode delphi}

interface

uses
  Classes, StdCtrls, ExtCtrls, Spin,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfClarifySpaces }

  TfClarifySpaces = class(TAbstractIDEOptionsEditor)
    cbAfterAssign: TComboBox;
    cbAfterSemicolon: TComboBox;
    cbAfterComma: TComboBox;
    cbAfterColon: TComboBox;
    cbBeforeSemicolon: TComboBox;
    cbBeforeComma: TComboBox;
    cbBeforeColon: TComboBox;
    cbFixSpacing: TCheckBox;
    cbMoveSpacesToBeforeColon: TCheckBox;
    cbSpacesAroundOperators: TComboBox;
    cbSpaceClassHeritage: TCheckBox;
    cbBeforeAssign: TComboBox;
    gbColon: TGroupBox;
    lbAfterAssign: TLabel;
    lbAfterSemicolon: TLabel;
    lbAfterComma: TLabel;
    lbAfterColon: TLabel;
    lbBeforeSemicolon: TLabel;
    lbBeforeComma: TLabel;
    lbBeforeColon: TLabel;
    lbSpacesAroundOperators: TLabel;
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
    lbBeforeAssign: TLabel;
    lbSpacesBeforeLabel: TLabel;
    cbMaxSpaces: TCheckBox;
    edtMaxSpacesInCode: TSpinEdit;
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
  JcfSettings, SettingsTypes, JcfUIConsts, JcfIdeRegister;

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

  lbSpacesAroundOperators.Caption := lisSpacesSpacesAroundOperators;
  cbSpacesAroundOperators.Items[0] := lisSpacesAlways;
  cbSpacesAroundOperators.Items[1] := lisSpacesLeaveAsIs;
  cbSpacesAroundOperators.Items[2] := lisSpacesNever;

  GroupBoxInsertSpaceBeforeBracket.Caption := lisSpacesInsertSpaceBeforeBracket;
  cbInsertSpaceBeforeBracketinFunctionDeclaration.Caption :=
    lisSpacesInFunctionDeclaration;
  cbInsertSpaceBeforeBracketinFunctionCall.Caption := lisSpacesInFunctionCall;
  cbBeforeOpenSquareBracketInExpression.Caption := lisSpacesBeforeInExpression;

  GroupBoxSpacesInsideBrackets.Caption := lisSpacesInsertSpaceInsideBrackets;
  cbInsertSpaceAfterOpen.Caption := lisSpacesAfterOpen;
  CheckBoxInsertSpaceBeforeEnd.Caption := lisSpacesBeforeEnd;

  gbTabs.Caption := lisSpacesTabCharacters;
  cbTabsToSpaces.Caption := lisSpacesTurnTabsToSpaces;
  Label1.Caption := lisSpacesSpacesPerTab;
  cbSpacesToTabs.Caption := lisSpacesTurnSpacesToTabs;
  Label3.Caption := lisSpacesSpacesForTab;

  cbMaxSpaces.Caption := lisSpacesMaxSpacesInCode;

  lbBeforeAssign.Caption := lisSpacesSpacesBeforeAssign;
  cbBeforeAssign.Items[0] := lisSpacesAlways;
  cbBeforeAssign.Items[1] := lisSpacesLeaveAsIs;
  cbBeforeAssign.Items[2] := lisSpacesNever;

  lbAfterAssign.Caption := lisSpacesSpacesAfterAssign;
  cbAfterAssign.Items[0] := lisSpacesAlways;
  cbAfterAssign.Items[1] := lisSpacesLeaveAsIs;
  cbAfterAssign.Items[2] := lisSpacesNever;

  lbBeforeComma.Caption := lisSpacesSpacesBeforeComma;
  cbBeforeComma.Items[0] := lisSpacesAlways;
  cbBeforeComma.Items[1] := lisSpacesLeaveAsIs;
  cbBeforeComma.Items[2] := lisSpacesNever;

  lbAfterComma.Caption := lisSpacesSpacesAfterComma;
  cbAfterComma.Items[0] := lisSpacesAlways;
  cbAfterComma.Items[1] := lisSpacesLeaveAsIs;
  cbAfterComma.Items[2] := lisSpacesNever;

  lbBeforeColon.Caption := lisSpacesSpacesBeforeColon;
  cbBeforeColon.Items[0] := lisSpacesAlways;
  cbBeforeColon.Items[1] := lisSpacesLeaveAsIs;
  cbBeforeColon.Items[2] := lisSpacesNever;

  lbAfterColon.Caption := lisSpacesSpacesAfterColon;
  cbAfterColon.Items[0] := lisSpacesAlways;
  cbAfterColon.Items[1] := lisSpacesLeaveAsIs;
  cbAfterColon.Items[2] := lisSpacesNever;

  lbBeforeSemicolon.Caption := lisSpacesSpacesBeforeSemicolon;
  cbBeforeSemicolon.Items[0] := lisSpacesAlways;
  cbBeforeSemicolon.Items[1] := lisSpacesLeaveAsIs;
  cbBeforeSemicolon.Items[2] := lisSpacesNever;

  lbAfterSemicolon.Caption := lisSpacesSpacesAfterSemicolon;
  cbAfterSemicolon.Items[0] := lisSpacesAlways;
  cbAfterSemicolon.Items[1] := lisSpacesLeaveAsIs;
  cbAfterSemicolon.Items[2] := lisSpacesNever;

  JCFOptionsFrameDialogs[JCFOptionSpaces] := Self;
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

    cbSpacesAroundOperators.ItemIndex := Ord(SpaceForOperator);
    cbBeforeAssign.ItemIndex := Ord(SpaceBeforeAssign);
    cbAfterAssign.ItemIndex := Ord(SpaceAfterAssign);
    cbBeforeComma.ItemIndex := Ord(SpaceBeforeComma);
    cbAfterComma.ItemIndex := Ord(SpaceAfterComma);
    cbBeforeColon.ItemIndex := Ord(SpaceBeforeColon);
    cbAfterColon.ItemIndex := Ord(SpaceAfterColon);
    cbBeforeSemicolon.ItemIndex := Ord(SpaceBeforeSemicolon);
    cbAfterSemicolon.ItemIndex := Ord(SpaceAfterSemicolon);

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

    SpaceForOperator := TTriOptionStyle(cbSpacesAroundOperators.ItemIndex);
    SpaceBeforeAssign := TTriOptionStyle(cbBeforeAssign.ItemIndex);
    SpaceAfterAssign := TTriOptionStyle(cbAfterAssign.ItemIndex);
    SpaceBeforeComma := TTriOptionStyle(cbBeforeComma.ItemIndex);
    SpaceAfterComma := TTriOptionStyle(cbAfterComma.ItemIndex);
    SpaceBeforeColon := TTriOptionStyle(cbBeforeColon.ItemIndex);
    SpaceAfterColon := TTriOptionStyle(cbAfterColon.ItemIndex);
    SpaceBeforeSemicolon := TTriOptionStyle(cbBeforeSemicolon.ItemIndex);
    SpaceAfterSemicolon := TTriOptionStyle(cbAfterSemicolon.ItemIndex);


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
  Result := TIDEFormattingSettings;
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
