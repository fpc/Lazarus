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
    BracketCombo: TComboBox;
    BracketLabel: TLabel;
    BracketLink: TLabel;
    AutoHintAndCompletionDelayLabel: TLabel;
    MarkupColorLink: TLabel;
    MarkupWordBevel1: TBevel;
    MarkupWordGroupLabel: TLabel;
    MarkupWordBevel: TBevel;
    ToolTipBevel: TBevel;
    MarkupWordDelayLabel: TLabel;
    MarkupWordNoTimerCheckBox: TCheckBox;
    MarkupWordFullLenSpin: TSpinEdit;
    MarkupWordFullLenLabel: TLabel;
    MarkupWordNoKeyword: TCheckBox;
    MarkupWordTrim: TCheckBox;
    MarkupWordTimeTrackBar: TTrackBar;
    AutoToolTipSymbToolsCheckBox: TCheckBox;
    AutoRemoveEmptyMethodsOnSave: TCheckBox;
    MarkupBevel: TBevel;
    procedure AutoDelayTrackBarChange(Sender: TObject);
    procedure BracketComboChange(Sender: TObject);
    procedure BracketComboExit(Sender: TObject);
    procedure BracketComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BracketLinkClick(Sender: TObject);
    procedure BracketLinkMouseEnter(Sender: TObject);
    procedure BracketLinkMouseLeave(Sender: TObject);
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    procedure MarkupColorLinkClick(Sender: TObject);
  private
    { private declarations }
    FDialog: TAbstractOptionsEditorDialog;
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

procedure TEditorCodetoolsOptionsFrame.BracketComboChange(Sender: TObject);
begin
  if BracketCombo.Items.IndexOf(BracketCombo.Text) >= 0 then
    BracketComboExit(Sender);
end;

procedure TEditorCodetoolsOptionsFrame.AutoDelayTrackBarChange(Sender: TObject);
begin
  AutoDelayLabel.Caption := Format(dlgEdDelayInSec, [FormatFloat('0.00', AutoDelayTrackBar.Position/1000)]);
  MarkupWordDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', MarkupWordTimeTrackBar.Position/1000)]);
  CompletionDropDownDelayLabel.Caption :=
    Format(dlgEdDelayInSec, [FormatFloat('0.00', CompletionDropDownHintTrackBar.Position/1000)]);
end;

procedure TEditorCodetoolsOptionsFrame.BracketComboExit(Sender: TObject);
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        if BracketCombo.ItemIndex = 0 then
          PreviewEdits[a].Options := PreviewEdits[a].Options - [eoBracketHighlight]
        else
        begin
          PreviewEdits[a].Options := PreviewEdits[a].Options + [eoBracketHighlight];
          PreviewEdits[a].BracketHighlightStyle := TSynEditBracketHighlightStyle(BracketCombo.ItemIndex - 1);
        end;
      end;
end;

procedure TEditorCodetoolsOptionsFrame.BracketComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    BracketComboExit(Sender);
end;

procedure TEditorCodetoolsOptionsFrame.BracketLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  col.SelectAhaColor(ahaBracketMatch);
end;

procedure TEditorCodetoolsOptionsFrame.BracketLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TEditorCodetoolsOptionsFrame.BracketLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

function TEditorCodetoolsOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame; inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

procedure TEditorCodetoolsOptionsFrame.MarkupColorLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  col.SelectAhaColor(ahaHighlightWord);
end;

function TEditorCodetoolsOptionsFrame.GetTitle: String;
begin
  Result := lisAutomaticFeatures;
end;

procedure TEditorCodetoolsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;

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

  MarkupWordGroupLabel.Caption := dlgMarkupGroup;
  MarkupWordFullLenLabel.Caption := dlgMarkupWordFullLen;
  MarkupWordNoKeyword.Caption := dlgMarkupWordNoKeyword;
  MarkupWordTrim.Caption := dlgMarkupWordTrim;
  MarkupWordNoTimerCheckBox.Caption := dlgMarkupWordNoTimer;
  MarkupColorLink.Caption := dlgColorLink;

  BracketLabel.Caption := dlgBracketHighlight;
  BracketLink.Caption := dlgColorLink;
  BracketCombo.Items.Add(dlgNoBracketHighlight);
  BracketCombo.Items.Add(dlgHighlightLeftOfCursor);
  BracketCombo.Items.Add(dlgHighlightRightOfCursor);
  BracketCombo.Items.Add(gldHighlightBothSidesOfCursor);
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

    MarkupWordTimeTrackBar.Position := MarkupCurWordTime;
    MarkupWordFullLenSpin. Value := MarkupCurWordFullLen;
    MarkupWordNoKeyword.Checked := MarkupCurWordNoKeyword;
    MarkupWordTrim.Checked := MarkupCurWordTrim;
    MarkupWordNoTimerCheckBox.Checked := MarkupCurWordNoTimer;

    if eoBracketHighlight in SynEditOptions then
      BracketCombo.ItemIndex := Ord(BracketHighlightStyle) + 1
    else
      BracketCombo.ItemIndex := 0;
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
    CompletionLongLineHintType :=  TSynComletionLongHintType(CompletionDropDownHint.ItemIndex);

    MarkupCurWordTime := MarkupWordTimeTrackBar.Position;
    MarkupCurWordFullLen := MarkupWordFullLenSpin.Value;
    MarkupCurWordNoKeyword := MarkupWordNoKeyword.Checked;
    MarkupCurWordTrim := MarkupWordTrim.Checked;
    MarkupCurWordNoTimer := MarkupWordNoTimerCheckBox.Checked;

    if BracketCombo.ItemIndex = 0 then
      SynEditOptions := SynEditOptions - [eoBracketHighlight]
    else
    begin
      SynEditOptions := SynEditOptions + [eoBracketHighlight];
      BracketHighlightStyle := TSynEditBracketHighlightStyle(BracketCombo.ItemIndex - 1);
    end;

  end;
end;

class function TEditorCodetoolsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodetoolsOptionsFrame, EdtOptionsCodetools);
end.

