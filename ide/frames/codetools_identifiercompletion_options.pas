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
unit codetools_identifiercompletion_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls,
  CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf, DividerBevel;

type

  { TCodetoolsIndentifierCompletionOptionsFrame }

  TCodetoolsIndentifierCompletionOptionsFrame = class(TAbstractIDEOptionsEditor)
    ICAddDoCheckBox: TCheckBox;
    ICAutoAddParameterBracketsCheckBox: TCheckBox;
    ICMiscDividerBevel: TDividerBevel;
    ICOpenDividerBevel: TDividerBevel;
    ICAutoStartAfterPointCheckBox: TCheckBox;
    ICAddAssignOperatorCheckBox: TCheckBox;
    ICAddSemicolonCheckBox: TCheckBox;
    ICAddDividerBevel: TDividerBevel;
    ICReplaceCheckBox: TCheckBox;
    ICJumpToErrorCheckBox: TCheckBox;
    ICShowHelpCheckBox: TCheckBox;
    ICAutoUseSingleIdent: TCheckBox;
    ICSortDividerBevel: TDividerBevel;
    ICSortForHistoryCheckBox: TCheckBox;
    ICSortForScopeCheckBox: TCheckBox;
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

{ TCodetoolsIndentifierCompletionOptionsFrame }

function TCodetoolsIndentifierCompletionOptionsFrame.GetTitle: String;
begin
  Result := dlgIdentifierCompletion;
end;

procedure TCodetoolsIndentifierCompletionOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  ICOpenDividerBevel.Caption:=lisIdCOpening;
  ICAutoStartAfterPointCheckBox.Caption:=lisAutomaticallyInvokeAfterPoint;
  ICAutoUseSingleIdent.Caption:=lisAutomaticallyUseSinglePossibleIdent;
  ICAutoUseSingleIdent.Hint:=
    lisWhenThereIsOnlyOnePossibleCompletionItemUseItImmed;
  ICShowHelpCheckBox.Caption:=lisShowHelp;
  ICShowHelpCheckBox.Hint:=lisBestViewedByInstallingAHTMLControlLikeTurbopowerip;

  ICAddDividerBevel.Caption:=lisIdCAddition;
  ICAddSemicolonCheckBox.Caption:=dlgAddSemicolon;
  ICAddAssignOperatorCheckBox.Caption:=dlgAddAssignmentOperator;
  ICAddDoCheckBox.Caption:=lisAddKeywordDo;
  ICAutoAddParameterBracketsCheckBox.Caption:=lisAddParameterBrackets;

  ICSortDividerBevel.Caption:=lisSorting;
  ICSortForHistoryCheckBox.Caption:=lisShowRecentlyUsedIdentifiersAtTop;
  ICSortForScopeCheckBox.Caption:=lisSortForScope;
  ICSortForScopeCheckBox.Hint:=lisForExampleShowAtTopTheLocalVariablesThenTheMembers;

  ICMiscDividerBevel.Caption:=dlgEnvMisc;
  ICReplaceCheckBox.Caption:=lisReplaceWholeIdentifier;
  ICReplaceCheckBox.Hint:=lisEnableReplaceWholeIdentifierDisableReplacePrefix;
  ICJumpToErrorCheckBox.Caption:=lisJumpToError;
  ICJumpToErrorCheckBox.Hint:=lisJumpToErrorAtIdentifierCompletion;
end;

procedure TCodetoolsIndentifierCompletionOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    ICAddSemicolonCheckBox.Checked := IdentComplAddSemicolon;
    ICAddAssignOperatorCheckBox.Checked := IdentComplAddAssignOperator;
    ICAddDoCheckBox.Checked := IdentComplAddDo;
    ICAutoStartAfterPointCheckBox.Checked := IdentComplAutoStartAfterPoint;
    ICAutoUseSingleIdent.Checked := IdentComplAutoUseSingleIdent;
    ICAutoAddParameterBracketsCheckBox.Checked:=IdentComplAddParameterBrackets;
    ICReplaceCheckBox.Checked:=IdentComplReplaceIdentifier;
    ICJumpToErrorCheckBox.Checked:=IdentComplJumpToError;
    ICShowHelpCheckBox.Checked:=IdentComplShowHelp;
    ICSortForHistoryCheckBox.Checked:=IdentComplSortForHistory;
    ICSortForScopeCheckBox.Checked:=IdentComplSortForScope;
  end;
end;

procedure TCodetoolsIndentifierCompletionOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    IdentComplAddSemicolon := ICAddSemicolonCheckBox.Checked;
    IdentComplAddAssignOperator := ICAddAssignOperatorCheckBox.Checked;
    IdentComplAddDo := ICAddDoCheckBox.Checked;
    IdentComplAutoStartAfterPoint := ICAutoStartAfterPointCheckBox.Checked;
    IdentComplAutoUseSingleIdent := ICAutoUseSingleIdent.Checked;
    IdentComplAddParameterBrackets:=ICAutoAddParameterBracketsCheckBox.Checked;
    IdentComplReplaceIdentifier:=ICReplaceCheckBox.Checked;
    IdentComplJumpToError:=ICJumpToErrorCheckBox.Checked;
    IdentComplShowHelp:=ICShowHelpCheckBox.Checked;
    IdentComplSortForHistory:=ICSortForHistoryCheckBox.Checked;
    IdentComplSortForScope:=ICSortForScopeCheckBox.Checked;
  end;
end;

class function TCodetoolsIndentifierCompletionOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsIndentifierCompletionOptionsFrame, CdtOptionsIdentCompletion);
end.

