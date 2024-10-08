unit frTransform;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is frTransform.pas.
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

{$mode delphi}

interface

uses
  Classes, StdCtrls, ExtCtrls,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TfTransform }

  TfTransform = class(TAbstractIDEOptionsEditor)
    btnAdvancedSettings: TButton;
    cbBlockEndSemicolons: TCheckBox;
    lbWarningSortUsesClauses: TLabel;
    rbBeginEnd: TRadioGroup;
    bgSortUses: TGroupBox;
    cbSortInterfaceUses: TCheckBox;
    cbSortImplementationUses: TCheckBox;
    cbBreakUsesSortOnComment: TCheckBox;
    cbBreakUsesSortOnReturn: TCheckBox;
    rgUsesSortOrder: TRadioGroup;
    cbNoComments: TCheckBox;
    cbSortProgramUses: TCheckBox;
    procedure btnAdvancedSettingsClick(Sender: TObject);
    procedure ShowAdvancedSettings(AVisible: boolean);
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
  SettingsTypes, JcfSettings, SetTransform, JcfUIConsts, JcfIdeRegister;

procedure TfTransform.btnAdvancedSettingsClick(Sender: TObject);
begin
  ShowAdvancedSettings(True);
  btnAdvancedSettings.Enabled := False;
end;

procedure TfTransform.ShowAdvancedSettings(AVisible: boolean);
begin
  lbWarningSortUsesClauses.Visible := AVisible;
  bgSortUses.Visible := AVisible;
  rgUsesSortOrder.Visible := AVisible;
end;

constructor TfTransform.Create(AOwner: TComponent);
begin
  inherited;
  //fiHelpContext := HELP_CLARIFY_TRANSFORM;
end;

function TfTransform.GetTitle: String;
begin
  Result := lisTransformTransform;
end;

procedure TfTransform.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  rbBeginEnd.Caption := lisTransformAddOrRemoveBeginAndEndFromSingleStatements;
  rbBeginEnd.Items[0] := lisTransformAddBeginAndEndToSingleStatements;
  rbBeginEnd.Items[1] := lisTransformLeaveBeginAndEndAsIs;
  rbBeginEnd.Items[2] := lisTransformRemoveBeginAndEndFromAroundSingleStatements;

  cbBlockEndSemicolons.Caption :=
    lisTransformPutSemicolonsAfterLastStatementInABlock;

  bgSortUses.Caption := lisTransformSortUsesClauses;
  cbSortInterfaceUses.Caption := lisTransformSortINterfaceUses;
  cbSortImplementationUses.Caption := lisTransformSortIMplementationUses;
  cbSortProgramUses.Caption := lisTransformSortProgramUses;
  cbBreakUsesSortOnReturn.Caption := lisTransformBreakOnReturn;
  cbBreakUsesSortOnComment.Caption := lisTransformBreakOnComment;
  cbNoComments.Caption := lisTransformOnlyWithNoComments;

  rgUsesSortOrder.Caption := lisTransformUsesSortOrder;
  rgUsesSortOrder.Items[0] := lisTransformAlphabetic;
  rgUsesSortOrder.Items[1] := lisTransformReverseAlphabetic;
  rgUsesSortOrder.Items[2] := lisTransformShortestToLongest;
  rgUsesSortOrder.Items[3] := lisTransformLongestToShortest;
  lbWarningSortUsesClauses.Caption := lisTransformWarningSortUsesClauses;
  btnAdvancedSettings.Caption := lisTransformAdvancedSettings;

  if FormattingSettings.Transform.SortProgramUses or
     FormattingSettings.Transform.SortInterfaceUses or
     FormattingSettings.Transform.SortImplementationUses then
  begin
    ShowAdvancedSettings(True);
    btnAdvancedSettings.Enabled := False;
  end;
end;

procedure TfTransform.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Transform do
  begin
    rbBeginEnd.ItemIndex := Ord(BeginEndStyle);
    cbBlockEndSemicolons.Checked := AddBlockEndSemicolon;

    cbSortInterfaceUses.Checked := SortInterfaceUses;
    cbSortImplementationUses.Checked := SortImplementationUses;
    cbSortProgramUses.Checked := SortProgramUses;
    cbBreakUsesSortOnReturn.Checked := BreakUsesSortOnReturn;
    cbBreakUsesSortOnComment.Checked := BreakUsesSortOnComment;

    rgUsesSortOrder.ItemIndex := Ord(UsesSortOrder);

    cbNoComments.Checked := SortUsesNoComments;
  end;

end;

procedure TfTransform.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with FormattingSettings.Transform do
  begin
    BeginEndStyle := TTriOptionStyle(rbBeginEnd.ItemIndex);
    AddBlockEndSemicolon := cbBlockEndSemicolons.Checked;

    SortInterfaceUses := cbSortInterfaceUses.Checked;
    SortImplementationUses := cbSortImplementationUses.Checked;
    SortProgramUses := cbSortProgramUses.Checked;

    BreakUsesSortOnReturn := cbBreakUsesSortOnReturn.Checked;
    BreakUsesSortOnComment := cbBreakUsesSortOnComment.Checked;

    UsesSortOrder := TUsesSortOrder(rgUsesSortOrder.ItemIndex);

    SortUsesNoComments := cbNoComments.Checked;
  end;

end;

class function TfTransform.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TIDEFormattingSettings;
end;

initialization
  RegisterIDEOptionsEditor(JCFOptionsGroup, TfTransform, JCFOptionTransform, JCFOptionClarify);
end.
