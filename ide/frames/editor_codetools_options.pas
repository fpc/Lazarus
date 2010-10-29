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
unit editor_codetools_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ComCtrls, Graphics, sysutils,
  EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, Spin, ExtCtrls,
  SynEditMarkupBracket, editor_color_options, editor_general_options,
  SynEdit, SynCompletion, LCLType;

type
  { TEditorCodetoolsOptionsFrame }

  TEditorCodetoolsOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoCompleteBlockCheckBox: TCheckBox;
    AutoDelayLabel: TLabel;
    CompletionDropDownHintLabel: TLabel;
    CompletionDropDownHint: TComboBox;
    CompletionDropDownDelayLabel: TLabel;
    AutoDelayTrackBar: TTrackBar;
    CompletionDropDownLabel: TLabel;
    CompletionDropDownHintTrackBar: TTrackBar;
    AutoToolTipExprEvalCheckBox: TCheckBox;
    AutoHintAndCompletionDelayLabel: TLabel;
    ToolTipBevel: TBevel;
    AutoToolTipSymbToolsCheckBox: TCheckBox;
    AutoRemoveEmptyMethodsOnSave: TCheckBox;
    procedure AutoDelayTrackBarChange(Sender: TObject);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorCodetoolsOptionsFrame }

procedure TEditorCodetoolsOptionsFrame.AutoDelayTrackBarChange(Sender: TObject);
begin
  AutoDelayLabel.Caption := Format(dlgEdDelayInSec, [FormatFloat('0.00', AutoDelayTrackBar.Position/1000)]);
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
  AutoCompleteBlockCheckBox.Caption := dlgEdCompleteBlocks;

  AutoHintAndCompletionDelayLabel.Caption:=lisDelayForHintsAndCompletionBox;
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
    AutoDelayTrackBar.Position := AutoDelayInMSec;
    AutoRemoveEmptyMethodsOnSave.Checked := AutoRemoveEmptyMethods;

    CompletionDropDownHintTrackBar.Position := CompletionLongLineHintInMSec;
    CompletionDropDownHint.ItemIndex := ord(CompletionLongLineHintType);

  end;
  AutoDelayTrackBarChange(nil);
end;

procedure TEditorCodetoolsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    AutoBlockCompletion := AutoCompleteBlockCheckBox.Checked;
    AutoToolTipExprEval := AutoToolTipExprEvalCheckBox.Checked;
    AutoToolTipSymbTools := AutoToolTipSymbToolsCheckBox.Checked;
    AutoDelayInMSec := AutoDelayTrackBar.Position;
    AutoRemoveEmptyMethods := AutoRemoveEmptyMethodsOnSave.Checked;

    CompletionLongLineHintInMSec := CompletionDropDownHintTrackBar.Position;
    CompletionLongLineHintType :=  TSynCompletionLongHintType(CompletionDropDownHint.ItemIndex);

  end;
end;

class function TEditorCodetoolsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodetoolsOptionsFrame, EdtOptionsCodetools);
end.

