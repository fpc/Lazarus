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
  SynCompletion,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
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
    DbgToolTipAutoCastClass: TCheckBox;
    CompletionDropDownHintLabel: TLabel;
    CompletionDropDownHint: TComboBox;
    CompletionDropDownDelayLabel: TLabel;
    AutoCompletionDelayTrackBar: TTrackBar;
    CompletionDropDownLabel: TLabel;
    CompletionDropDownHintTrackBar: TTrackBar;
    AutoToolTipExprEvalCheckBox: TCheckBox;
    AutoCompletionDelayLabel: TLabel;
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
end;

procedure TEditorCodetoolsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    AutoCompleteBlockCheckBox.Checked := AutoBlockCompletion;
    AutoToolTipExprEvalCheckBox.Checked := AutoToolTipExprEval;
    AutoToolTipSymbToolsCheckBox.Checked := AutoToolTipSymbTools;
    DbgToolTipAutoCastClass.Checked := DbgHintAutoTypeCastClass;
    AutoCompletionDelayTrackBar.Position := AutoDelayInMSec;
    AutoHintDelayTrackBar.Position := AutoHintDelayInMSec;
    AutoRemoveEmptyMethodsOnSave.Checked := AutoRemoveEmptyMethods;
    AutoDisplayFuncProtoCheckBox.Checked := AutoDisplayFunctionPrototypes;

    CompletionDropDownHintTrackBar.Position := CompletionLongLineHintInMSec;
    CompletionDropDownHint.ItemIndex := ord(CompletionLongLineHintType);

  end;
  AutoCompletionDelayTrackBarChange(nil);
end;

procedure TEditorCodetoolsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    AutoBlockCompletion := AutoCompleteBlockCheckBox.Checked;
    AutoToolTipExprEval := AutoToolTipExprEvalCheckBox.Checked;
    AutoToolTipSymbTools := AutoToolTipSymbToolsCheckBox.Checked;
    DbgHintAutoTypeCastClass := DbgToolTipAutoCastClass.Checked;
    AutoDelayInMSec := AutoCompletionDelayTrackBar.Position;
    AutoHintDelayInMSec := AutoHintDelayTrackBar.Position;
    AutoRemoveEmptyMethods := AutoRemoveEmptyMethodsOnSave.Checked;
    AutoDisplayFunctionPrototypes := AutoDisplayFuncProtoCheckBox.Checked;

    CompletionLongLineHintInMSec := CompletionDropDownHintTrackBar.Position;
    CompletionLongLineHintType := TSynCompletionLongHintType(CompletionDropDownHint.ItemIndex);

  end;
end;

class function TEditorCodetoolsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodetoolsOptionsFrame, EdtOptionsCodetools);
end.

