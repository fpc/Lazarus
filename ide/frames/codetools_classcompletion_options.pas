{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit codetools_classcompletion_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, ExtCtrls, StdCtrls, Dialogs,
  // CodeTools
  SourceChanger,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  CodeToolsOptions, LazarusIDEStrConsts;

type

  { TCodetoolsClassCompletionOptionsFrame }

  TCodetoolsClassCompletionOptionsFrame = class(TAbstractIDEOptionsEditor)
    ClassPartInsertPolicyComboBox: TComboBox;
    ClassPartInsertPolicyLabel: TLabel;
    InsertPoliciesPanel: TPanel;
    MethodDefaultSectionComboBox: TComboBox;
    MethodDefaultSectionLabel: TLabel;
    MethodInsertPolicyComboBox: TComboBox;
    MethodInsertPolicyLabel: TLabel;
    SetPropertyVariableIsPrefixCheckBox: TCheckBox;
    SetPropertyVariableUseConstCheckBox: TCheckBox;
    ClassHeaderCommentsCheckBox: TCheckBox;
    ClassImplementationCommentsCheckBox: TCheckBox;
    MixMethodsAndPropertiesCheckBox: TCheckBox;
    PropPrefixesPanel: TPanel;
    PrivateVariablePrefixEdit: TEdit;
    PrivateVariablePrefixLabel: TLabel;
    PropertyCompletionCheckBox: TCheckBox;
    PropertyCompletionGroupBox: TGroupBox;
    PropertyReadIdentPrefixEdit: TEdit;
    PropertyReadIdentPrefixLabel: TLabel;
    PropertyStoredIdentPostfixEdit: TEdit;
    PropertyStoredIdentPostfixLabel: TLabel;
    PropertyWriteIdentPrefixEdit: TEdit;
    PropertyWriteIdentPrefixLabel: TLabel;
    SetPropertyVariablenameEdit: TEdit;
    SetPropertyVariablenameLabel: TLabel;
    UpdateAllMethodSignaturesCheckBox: TCheckBox;
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodetoolsClassCompletionOptionsFrame }

function TCodetoolsClassCompletionOptionsFrame.GetTitle: String;
begin
  Result := lisClassCompletion;
end;

procedure TCodetoolsClassCompletionOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
var
  s: String;
  ics: TInsertClassSection;
begin
  ClassPartInsertPolicyLabel.Caption:=dlgInsertClassParts;
  ClassPartInsertPolicyLabel.Hint:=
    lisNewMethodsAndMembersAreInsertedAlphabeticallyOrAdd;
  ClassPartInsertPolicyComboBox.Hint:=ClassPartInsertPolicyLabel.Hint;
  ClassPartInsertPolicyComboBox.Items.Text:=
    dlgAlphabetically+LineEnding+
    dlgCDTLast;

  MethodInsertPolicyLabel.Caption:=dlgInsertMethods;
  MethodInsertPolicyLabel.Hint:=
    lisNewMethodImplementationsAreInsertedBetweenExisting;
  MethodInsertPolicyComboBox.Hint:=MethodInsertPolicyLabel.Hint;
  MethodInsertPolicyComboBox.Items.Text:=
    dlgAlphabetically+LineEnding+
    dlgCDTLast+LineEnding+
    dlgCDTClassOrder;

  MethodDefaultSectionLabel.Caption:=lisDefaultSectionOfMethods;
  MethodDefaultSectionLabel.Hint:=
    lisDefaultClassVisibilitySectionOfNewMethodsForExampl;
  MethodDefaultSectionComboBox.Hint:=MethodDefaultSectionLabel.Hint;
  s:='';
  for ics in TInsertClassSection do
    s:=s+InsertClassSectionNames[ics]+LineEnding;
  MethodDefaultSectionComboBox.Items.Text:=s;

  MixMethodsAndPropertiesCheckBox.Caption:=dlgMixMethodsAndProperties;
  UpdateAllMethodSignaturesCheckBox.Caption:=lisCTOUpdateAllMethodSignatures;
  ClassHeaderCommentsCheckBox.Caption:=lisHeaderCommentForClass;
  ClassImplementationCommentsCheckBox.Caption:=lisImplementationCommentForClass;

  PropertyCompletionGroupBox.Caption:=dlgPropertyCompletion;
  PropertyCompletionCheckBox.Caption:=dlgCompleteProperties;
  PropertyReadIdentPrefixLabel.Caption:=dlgCDTReadPrefix;
  PropertyWriteIdentPrefixLabel.Caption:=dlgCDTWritePrefix;
  PropertyStoredIdentPostfixLabel.Caption:=dlgCDTStoredPostfix;
  PrivateVariablePrefixLabel.Caption:=dlgCDTVariablePrefix;
  SetPropertyVariablenameLabel.Caption:=dlgSetPropertyVariable;
  SetPropertyVariablenameLabel.Hint:=dlgSetPropertyVariableHint;
  SetPropertyVariablenameEdit.Hint:=SetPropertyVariablenameLabel.Hint;
  SetPropertyVariableIsPrefixCheckBox.Caption:=dlgSetPropertyVariableIsPrefix;
  SetPropertyVariableIsPrefixCheckBox.Hint:=dlgSetPropertyVariableIsPrefixHint;
  SetPropertyVariableUseConstCheckBox.Caption:=dlgSetPropertyVariableUseConst;
  SetPropertyVariableUseConstCheckBox.Hint:=dlgSetPropertyVariableUseConstHint;
end;

procedure TCodetoolsClassCompletionOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case ClassPartInsertPolicy of
      cpipAlphabetically:
        ClassPartInsertPolicyComboBox.ItemIndex:=0;
    else
      // cpipLast
      ClassPartInsertPolicyComboBox.ItemIndex:=1;
    end;

    MixMethodsAndPropertiesCheckBox.Checked := MixMethodsAndProperties;
    UpdateAllMethodSignaturesCheckBox.Checked:=UpdateAllMethodSignatures;
    ClassHeaderCommentsCheckBox.Checked := ClassHeaderComments;
    ClassImplementationCommentsCheckBox.Checked := ClassImplementationComments;
    case MethodInsertPolicy of
      mipAlphabetically:
        MethodInsertPolicyComboBox.ItemIndex:=0;
      mipLast:
        MethodInsertPolicyComboBox.ItemIndex:=1;
    else
      // mipClassOrder
      MethodInsertPolicyComboBox.ItemIndex:=2;
    end;

    MethodDefaultSectionComboBox.ItemIndex:=ord(MethodDefaultSection);

    PropertyCompletionCheckBox.Checked := CompleteProperties;
    PropertyReadIdentPrefixEdit.Text := PropertyReadIdentPrefix;
    PropertyWriteIdentPrefixEdit.Text := PropertyWriteIdentPrefix;
    PropertyStoredIdentPostfixEdit.Text := PropertyStoredIdentPostfix;
    PrivateVariablePrefixEdit.Text := PrivateVariablePrefix;
    SetPropertyVariablenameEdit.Text := SetPropertyVariablename;
    SetPropertyVariableIsPrefixCheckBox.Checked := SetPropertyVariableIsPrefix;
    SetPropertyVariableUseConstCheckBox.Checked := SetPropertyVariableUseConst;
  end;
end;

procedure TCodetoolsClassCompletionOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodetoolsOptions do
  begin
    case ClassPartInsertPolicyComboBox.ItemIndex of
      0: ClassPartInsertPolicy := cpipAlphabetically;
      1: ClassPartInsertPolicy := cpipLast;
    end;

    MixMethodsAndProperties := MixMethodsAndPropertiesCheckBox.Checked;
    UpdateAllMethodSignatures:=UpdateAllMethodSignaturesCheckBox.Checked;
    ClassHeaderComments := ClassHeaderCommentsCheckBox.Checked;
    ClassImplementationComments := ClassImplementationCommentsCheckBox.Checked;

    case MethodInsertPolicyComboBox.ItemIndex of
      0: MethodInsertPolicy := mipAlphabetically;
      1: MethodInsertPolicy := mipLast;
      2: MethodInsertPolicy := mipClassOrder;
    end;

    if MethodDefaultSectionComboBox.ItemIndex>=0 then
      MethodDefaultSection:=TInsertClassSection(MethodDefaultSectionComboBox.ItemIndex);

    CompleteProperties:=PropertyCompletionCheckBox.Checked;
    PropertyReadIdentPrefix :=
      ReadIdentifier(PropertyReadIdentPrefixEdit.Text,'Get');
    PropertyWriteIdentPrefix :=
      ReadIdentifier(PropertyWriteIdentPrefixEdit.Text,'Set');
    PropertyStoredIdentPostfix :=
      ReadIdentifier(PropertyStoredIdentPostfixEdit.Text,'IsStored');
    PrivateVariablePrefix :=
      ReadIdentifier(PrivateVariablePrefixEdit.Text,'F');
    SetPropertyVariablename :=
      ReadIdentifier(SetPropertyVariablenameEdit.Text,'AValue');
    SetPropertyVariableIsPrefix := SetPropertyVariableIsPrefixCheckBox.Checked;
    SetPropertyVariableUseConst := SetPropertyVariableUseConstCheckBox.Checked;
  end;
end;

class function TCodetoolsClassCompletionOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsClassCompletionOptionsFrame, CdtOptionsClassCompletion);
end.

