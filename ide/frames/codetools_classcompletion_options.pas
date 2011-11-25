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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit codetools_classcompletion_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, ExtCtrls, StdCtrls,
  SourceChanger, CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TCodetoolsClassCompletionOptionsFrame }

  TCodetoolsClassCompletionOptionsFrame = class(TAbstractIDEOptionsEditor)
    ClassHeaderCommentsCheckBox: TCheckBox;
    ClassImplementationCommentsCheckBox: TCheckBox;
    ClassPartInsertPolicyRadioGroup: TRadioGroup;
    MethodInsertPolicyRadioGroup: TRadioGroup;
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
  private
    fLoaded: Boolean;
    FSaved: Boolean;
    { private declarations }
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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
begin
  with ClassPartInsertPolicyRadioGroup do begin
    Caption:=dlgClassInsertPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgAlphabetically);
      Add(dlgCDTLast);
      EndUpdate;
    end;
  end;

  with MixMethodsAndPropertiesCheckBox do
    Caption:=dlgMixMethodsAndProperties;

  with MethodInsertPolicyRadioGroup do begin
    Caption:=dlgMethodInsPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgAlphabetically);
      Add(dlgCDTLast);
      Add(dlgCDTClassOrder);
      EndUpdate;
    end;
  end;

  ClassHeaderCommentsCheckBox.Caption:=lisHeaderCommentForClass;
  ClassImplementationCommentsCheckBox.Caption:=lisImplementationCommentForClass;
  PropertyCompletionGroupBox.Caption:=dlgPropertyCompletion;
  PropertyCompletionCheckBox.Caption:=dlgCompleteProperties;
  PropertyReadIdentPrefixLabel.Caption:=dlgCDTReadPrefix;
  PropertyWriteIdentPrefixLabel.Caption:=dlgCDTWritePrefix;
  PropertyStoredIdentPostfixLabel.Caption:=dlgCDTStoredPostfix;
  PrivateVariablePrefixLabel.Caption:=dlgCDTVariablePrefix;
  SetPropertyVariablenameLabel.Caption:=dlgSetPropertyVariable;
end;

procedure TCodetoolsClassCompletionOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  if fLoaded then exit;
  fLoaded:=true;

  with AOptions as TCodetoolsOptions do
  begin
    case ClassPartInsertPolicy of
      cpipAlphabetically:
        ClassPartInsertPolicyRadioGroup.ItemIndex:=0;
    else
      // cpipLast
      ClassPartInsertPolicyRadioGroup.ItemIndex:=1;
    end;

    MixMethodsAndPropertiesCheckBox.Checked := MixMethodsAndProperties;

    ClassHeaderCommentsCheckBox.Checked := ClassHeaderComments;
    ClassImplementationCommentsCheckBox.Checked := ClassImplementationComments;
    case MethodInsertPolicy of
      mipAlphabetically:
        MethodInsertPolicyRadioGroup.ItemIndex:=0;
      mipLast:
        MethodInsertPolicyRadioGroup.ItemIndex:=1;
    else
      // mipClassOrder
      MethodInsertPolicyRadioGroup.ItemIndex:=2;
    end;

    PropertyCompletionCheckBox.Checked := CompleteProperties;
    PropertyReadIdentPrefixEdit.Text := PropertyReadIdentPrefix;
    PropertyWriteIdentPrefixEdit.Text := PropertyWriteIdentPrefix;
    PropertyStoredIdentPostfixEdit.Text := PropertyStoredIdentPostfix;
    PrivateVariablePrefixEdit.Text := PrivateVariablePrefix;
    SetPropertyVariablenameEdit.Text := SetPropertyVariablename;
  end;
end;

procedure TCodetoolsClassCompletionOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
  with AOptions as TCodetoolsOptions do
  begin
    case ClassPartInsertPolicyRadioGroup.ItemIndex of
      0: ClassPartInsertPolicy := cpipAlphabetically;
      1: ClassPartInsertPolicy := cpipLast;
    end;

    MixMethodsAndProperties := MixMethodsAndPropertiesCheckBox.Checked;

    ClassHeaderComments := ClassHeaderCommentsCheckBox.Checked;
    ClassImplementationComments := ClassImplementationCommentsCheckBox.Checked;

    case MethodInsertPolicyRadioGroup.ItemIndex of
      0: MethodInsertPolicy := mipAlphabetically;
      1: MethodInsertPolicy := mipLast;
      2: MethodInsertPolicy := mipClassOrder;
    end;

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
  end;
end;

class function TCodetoolsClassCompletionOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsClassCompletionOptionsFrame, CdtOptionsClassCompletion);
end.

