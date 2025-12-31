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
unit editor_codetools_options;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  // LCL
  StdCtrls, ComCtrls, ExtCtrls,
  // SynEdit
  SynCompletion, SynPluginAutoBraces,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, DividerBevel,
  IdeDebuggerStringConstants,
  // IDE
  EditorOptions, LazarusIDEStrConsts;

type
  { TEditorCodetoolsOptionsFrame }

  TEditorCodetoolsOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoCompleteBlockCheckBox: TCheckBox;
    AutoDelayLabel: TLabel;
    AutoHDelayLabel: TLabel;
    AutoHintDelayTrackBar: TTrackBar;
    AutoDisplayFuncProtoCheckBox: TCheckBox;
    AutoHintDelayLabel: TLabel;
    cbAutoBraceOpen: TCheckBox;
    cbAutoBraceClose: TCheckBox;
    DbgToolTipAutoCastClass: TCheckBox;
    CompletionDropDownHintLabel: TLabel;
    CompletionDropDownHint: TComboBox;
    CompletionDropDownDelayLabel: TLabel;
    AutoCompletionDelayTrackBar: TTrackBar;
    CompletionDropDownLabel: TLabel;
    CompletionDropDownHintTrackBar: TTrackBar;
    AutoToolTipExprEvalCheckBox: TCheckBox;
    AutoCompletionDelayLabel: TLabel;
    DbgToolTipUseConverter: TCheckBox;
    AutoBracketBevel: TDividerBevel;
    edAutoBraceOpenFilter: TEdit;
    edAutoBraceCloseFilter: TEdit;
    lblAutoBraceOpenFilter: TLabel;
    lblAutoBraceCloseFilter: TLabel;
    Panel1: TPanel;
    ToolTipBevel: TBevel;
    AutoToolTipSymbToolsCheckBox: TCheckBox;
    AutoRemoveEmptyMethodsOnSave: TCheckBox;
    procedure AutoCompletionDelayTrackBarChange(Sender: TObject);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorCodetoolsOptionsFrame }

procedure TEditorCodetoolsOptionsFrame.AutoCompletionDelayTrackBarChange(Sender: TObject);
begin
  AutoDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', AutoCompletionDelayTrackBar.Position/1000)]);
  AutoHDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', AutoHintDelayTrackBar.Position/1000)]);
  CompletionDropDownDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', CompletionDropDownHintTrackBar.Position/1000)]);
end;

function TEditorCodetoolsOptionsFrame.GetTitle: String;
begin
  Result := lisAutomaticFeatures;
end;

procedure TEditorCodetoolsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  AutoRemoveEmptyMethodsOnSave.Caption := dlgAutoRemoveEmptyMethods;
  AutoToolTipSymbToolsCheckBox.Caption := lisShowDeclarationHints;
  AutoToolTipExprEvalCheckBox.Caption := lisShowValueHintsWhileDebugging;
  DbgToolTipAutoCastClass.Caption := lisDebugHintAutoTypeCastClass;
  DbgToolTipUseConverter.Caption := dsrEvalUseDebugConverter;
  AutoCompleteBlockCheckBox.Caption := dlgEdCompleteBlocks;
  AutoDisplayFuncProtoCheckBox.Caption := dlgAutoDisplayFuncProto;

  AutoCompletionDelayLabel.Caption:=lisDelayForCompletionBox;
  AutoHintDelayLabel.Caption:=lisDelayForHints;
  CompletionDropDownLabel.Caption := lisDelayForCompletionLongLineHint;
  CompletionDropDownHintLabel.Caption := lisCompletionLongLineHintType;
  CompletionDropDownHint.Clear;
  CompletionDropDownHint.Items.Add(lisCompletionLongLineHintTypeNone);
  CompletionDropDownHint.Items.Add(lisCompletionLongLineHintTypeRightOnly);
  CompletionDropDownHint.Items.Add(lisCompletionLongLineHintTypeLittleLeft);
  CompletionDropDownHint.Items.Add(lisCompletionLongLineHintTypeFullLeft);

  cbAutoBraceOpen.Caption := dlgOptAutoBraceOpen;
  cbAutoBraceClose.Caption := dlgOptAutoBraceClose;
  lblAutoBraceOpenFilter.Caption := dlgOptAutoBraceFilter;
  lblAutoBraceCloseFilter.Caption := dlgOptAutoBraceFilter;
  edAutoBraceOpenFilter.Hint := dlgOptAutoBraceFilterHint;
  edAutoBraceCloseFilter.Hint := dlgOptAutoBraceFilterHint;
end;

procedure TEditorCodetoolsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    AutoCompleteBlockCheckBox.Checked := AutoBlockCompletion;
    AutoToolTipExprEvalCheckBox.Checked := AutoToolTipExprEval;
    AutoToolTipSymbToolsCheckBox.Checked := AutoToolTipSymbTools;
    DbgToolTipAutoCastClass.Checked := DbgHintAutoTypeCastClass;
    DbgToolTipUseConverter.Checked := DbgHintUseBackendDebugConverter;
    AutoCompletionDelayTrackBar.Position := AutoDelayInMSec;
    AutoHintDelayTrackBar.Position := AutoHintDelayInMSec;
    AutoRemoveEmptyMethodsOnSave.Checked := AutoRemoveEmptyMethods;
    AutoDisplayFuncProtoCheckBox.Checked := AutoDisplayFunctionPrototypes;

    CompletionDropDownHintTrackBar.Position := CompletionLongLineHintInMSec;
    CompletionDropDownHint.ItemIndex := ord(CompletionLongLineHintType);

    cbAutoBraceOpen.Checked := abInsertClose in AutoBraceModes;
    cbAutoBraceClose.Checked := abSkipClose in AutoBraceModes;
    edAutoBraceOpenFilter.Text := AutoBraceFilterOpen;
    edAutoBraceCloseFilter.Text := AutoBraceFilterClose;
  end;
  AutoCompletionDelayTrackBarChange(nil);
end;

procedure TEditorCodetoolsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  abm: TSynPluginAutoBraceModes;
begin
  with AOptions as TEditorOptions do
  begin
    AutoBlockCompletion := AutoCompleteBlockCheckBox.Checked;
    AutoToolTipExprEval := AutoToolTipExprEvalCheckBox.Checked;
    AutoToolTipSymbTools := AutoToolTipSymbToolsCheckBox.Checked;
    DbgHintAutoTypeCastClass := DbgToolTipAutoCastClass.Checked;
    DbgHintUseBackendDebugConverter := DbgToolTipUseConverter.Checked;
    AutoDelayInMSec := AutoCompletionDelayTrackBar.Position;
    AutoHintDelayInMSec := AutoHintDelayTrackBar.Position;
    AutoRemoveEmptyMethods := AutoRemoveEmptyMethodsOnSave.Checked;
    AutoDisplayFunctionPrototypes := AutoDisplayFuncProtoCheckBox.Checked;

    CompletionLongLineHintInMSec := CompletionDropDownHintTrackBar.Position;
    CompletionLongLineHintType := TSynCompletionLongHintType(CompletionDropDownHint.ItemIndex);

    abm := [];
    if cbAutoBraceOpen.Checked  then abm := abm + [abInsertClose];
    if cbAutoBraceClose.Checked then abm := abm + [abSkipClose];
    AutoBraceModes := abm;
    AutoBraceFilterOpen := edAutoBraceOpenFilter.Text;
    AutoBraceFilterClose := edAutoBraceCloseFilter.Text;
  end;
end;

class function TEditorCodetoolsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodetoolsOptionsFrame, EdtOptionsCodetools);
end.

