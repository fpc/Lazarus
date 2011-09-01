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
unit editor_display_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, StdCtrls,
  Spin, LCLType, SynEdit, SynEditMouseCmds, EditorOptions, LazarusIDEStrConsts,
  IDEOptionsIntf, editor_general_options, editor_color_options, IDEProcs,
  SynGutterLineNumber, SynGutter;

type
  { TEditorDisplayOptionsFrame }

  TEditorDisplayOptionsFrame = class(TAbstractIDEOptionsEditor)
    DisableAntialiasingCheckBox: TCheckBox;
    DisplayPreview: TSynEdit;
    EditorFontButton: TButton;
    EditorFontComboBox: TComboBox;
    EditorFontGroupBox: TGroupBox;
    EditorFontSizeSpinEdit: TSpinEdit;
    EditorFontSizeLabel: TLabel;
    ExtraCharSpacingComboBox: TComboBox;
    ExtraCharSpacingLabel: TLabel;
    ExtraLineSpacingComboBox: TComboBox;
    ExtraLineSpacingLabel: TLabel;
    GutterSeparatorIndexLabel: TLabel;
    MarginAndGutterGroupBox: TGroupBox;
    RightMarginColorLink: TLabel;
    RightMarginComboBox: TComboBox;
    RightMarginLabel: TLabel;
    ShowLineNumbersCheckBox: TCheckBox;
    ShowOnlyLineNumbersMultiplesOfLabel: TLabel;
    ShowOnlyLineNumbersMultiplesOfSpinEdit: TSpinEdit;
    GutterSeparatorIndexSpinBox: TSpinEdit;
    VisibleGutterCheckBox: TCheckBox;
    VisibleRightMarginCheckBox: TCheckBox;
    procedure EditorFontButtonClick(Sender: TObject);
    procedure EditorFontComboBoxEditingDone(Sender: TObject);
    procedure EditorFontSizeSpinEditChange(Sender: TObject);
    procedure ComboboxOnExit(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboboxOnChange(Sender: TObject);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure RightMarginColorLinkClick(Sender: TObject);
    procedure RightMarginColorLinkMouseEnter(Sender: TObject);
    procedure RightMarginColorLinkMouseLeave(Sender: TObject);
    procedure ShowLineNumbersCheckBoxClick(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FUpdatingFontSizeRange: Boolean;
    function FontSizeNegativeToPositive(NegativeSize: Integer): Integer;
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    procedure SetEditorFontSizeSpinEditValue(FontSize: Integer);

    procedure FontDialogApplyClicked(Sender: TObject);
    function DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf;

function TEditorDisplayOptionsFrame.FontSizeNegativeToPositive(NegativeSize: Integer): Integer;
var
  tm: TTextMetric;
begin
  DisplayPreview.Canvas.Font.Assign(DisplayPreview.Font);
  if LCLIntf.GetTextMetrics(DisplayPreview.Canvas.Handle, tm) then
    Result := -(NegativeSize + MulDiv(tm.tmInternalLeading, 72, DisplayPreview.Font.PixelsPerInch))
  else
    Result := -NegativeSize;
end;

procedure TEditorDisplayOptionsFrame.FontDialogApplyClicked(Sender: TObject);
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
        PreviewEdits[a].Font.Assign(TFontDialog(Sender).Font);

  SetComboBoxText(EditorFontComboBox, DisplayPreview.Font.Name,cstCaseInsensitive);
  SetEditorFontSizeSpinEditValue(DisplayPreview.Font.Size);
end;

function TEditorDisplayOptionsFrame.DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := true;
end;

procedure TEditorDisplayOptionsFrame.EditorFontButtonClick(Sender: TObject);
var
  FontDialog: TFontDialog;
  CurFontSize: Integer;
begin
  FontDialog := TFontDialog.Create(nil);
  try
    with FontDialog do
    begin
      Font.Name := EditorFontComboBox.Text;
      CurFontSize := EditorFontSizeSpinEdit.Value;
      if CurFontSize < 0 then
      begin
        CurFontSize := FontSizeNegativeToPositive(CurFontSize);
        RepairEditorFontSize(CurFontSize);
      end;
      Font.Size := CurFontSize;
      Options := Options + [fdApplyButton];
      OnApplyClicked := @FontDialogApplyClicked;
      if Execute then
        FontDialogApplyClicked(FontDialog);
    end;
  finally
    FontDialog.Free;
  end;
end;

procedure TEditorDisplayOptionsFrame.EditorFontComboBoxEditingDone(Sender: TObject);
var
  i: Integer;
begin
  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        PreviewEdits[i].Font.Name := EditorFontComboBox.Text;
end;

procedure TEditorDisplayOptionsFrame.EditorFontSizeSpinEditChange(Sender: TObject);
var
  NewVal, a: Integer;
begin
  NewVal := EditorFontSizeSpinEdit.Value;
  if (NewVal < 0) and (NewVal > -EditorOptionsMinimumFontSize) then
  begin
    // Skip to minimum positive value. Will trigger OnChange again.
    SetEditorFontSizeSpinEditValue(EditorOptionsMinimumFontSize);
  end
  else
  begin
    if (NewVal > 0) and not FUpdatingFontSizeRange then
      EditorFontSizeSpinEdit.MinValue := EditorOptionsMinimumFontSize;

    with GeneralPage do
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> nil then
          PreviewEdits[a].Font.Size := NewVal;
  end;
end;

procedure TEditorDisplayOptionsFrame.ComboboxOnExit(Sender: TObject);
var
  NewVal, a: Integer;
begin
  if Sender = ExtraCharSpacingComboBox then
  begin
    NewVal := StrToIntDef(ExtraCharSpacingComboBox.Text, DisplayPreview.ExtraCharSpacing);
    SetComboBoxText(ExtraCharSpacingComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].ExtraCharSpacing := NewVal;
  end
  else
  if Sender = ExtraLineSpacingComboBox then
  begin
    NewVal := StrToIntDef(ExtraLineSpacingComboBox.Text, DisplayPreview.ExtraLineSpacing);
    SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].ExtraLineSpacing := NewVal;
  end
  else
  if Sender = RightMarginComboBox then
  begin
    NewVal := StrToIntDef(RightMarginComboBox.Text, DisplayPreview.RightEdge);
    SetComboBoxText(RightMarginComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> nil then
        begin
          if VisibleRightMarginCheckBox.Checked then
            PreviewEdits[a].RightEdge := NewVal
          else
            PreviewEdits[a].RightEdge := 0;
        end;
  end;
end;

procedure TEditorDisplayOptionsFrame.ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorDisplayOptionsFrame.ComboboxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorDisplayOptionsFrame.GeneralCheckBoxOnChange(Sender: TObject);
var
  a: integer;
  AGeneralPage: TEditorGeneralOptionsFrame;
  Separator: TSynGutterSeparator;
begin
  AGeneralPage := GeneralPage;

  if AGeneralPage = nil then
    Exit;

  with AGeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        PreviewEdits[a].Gutter.Visible := VisibleGutterCheckBox.Checked;
        PreviewEdits[a].Gutter.LineNumberPart.Visible
          := ShowLineNumbersCheckBox.Checked;
        if Assigned(PreviewEdits[a].Gutter.Parts.ByClass[TSynGutterLineNumber, 0]) then
          TSynGutterLineNumber(PreviewEdits[a].Gutter.Parts.ByClass[TSynGutterLineNumber, 0])
            .ShowOnlyLineNumbersMultiplesOf := ShowOnlyLineNumbersMultiplesOfSpinEdit.Value;

        Separator := TSynGutterSeparator(PreviewEdits[a].Gutter.Parts.ByClass[TSynGutterSeparator, 0]);
        if Assigned(Separator) then
        begin
          Separator.Visible := GutterSeparatorIndexSpinBox.Value <> -1;
          if Separator.Visible then
            Separator.Index := GutterSeparatorIndexSpinBox.Value;
        end;
        if VisibleRightMarginCheckBox.Checked then
          PreviewEdits[a].RightEdge := StrToIntDef(RightMarginComboBox.Text, 80)
        else
          PreviewEdits[a].RightEdge := 0;
        if DisableAntialiasingCheckBox.Checked then
          PreviewEdits[a].Font.Quality := fqNonAntialiased
        else
          PreviewEdits[a].Font.Quality := fqDefault;
      end;
end;

procedure TEditorDisplayOptionsFrame.RightMarginColorLinkClick(Sender: TObject);
var
  col: TEditorColorOptionsFrame;
begin
  col := TEditorColorOptionsFrame(FDialog.FindEditor(TEditorColorOptionsFrame));
  if col = nil then exit;
  FDialog.OpenEditor(TEditorColorOptionsFrame);
  col.SelectAhaColor(ahaRightMargin);
end;

procedure TEditorDisplayOptionsFrame.RightMarginColorLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TEditorDisplayOptionsFrame.RightMarginColorLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TEditorDisplayOptionsFrame.ShowLineNumbersCheckBoxClick(Sender: TObject);
begin
  ShowOnlyLineNumbersMultiplesOfSpinEdit.Enabled := ShowLineNumbersCheckBox.Checked;
  ShowOnlyLineNumbersMultiplesOfLabel.Enabled := ShowLineNumbersCheckBox.Checked;
end;

function TEditorDisplayOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame; inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

procedure TEditorDisplayOptionsFrame.SetEditorFontSizeSpinEditValue(FontSize: Integer);
begin
  FUpdatingFontSizeRange := True;
  if FontSize < 0 then
    EditorFontSizeSpinEdit.MinValue := -EditorFontSizeSpinEdit.MaxValue
  else
    EditorFontSizeSpinEdit.MinValue := EditorOptionsMinimumFontSize;
  FUpdatingFontSizeRange := False;
  EditorFontSizeSpinEdit.Value := FontSize;
end;

function TEditorDisplayOptionsFrame.GetTitle: String;
begin
  Result := dlgEdDisplay;
end;

procedure TEditorDisplayOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Prevent the caret from moving
  DisplayPreview.RegisterMouseActionSearchHandler(@DoSynEditMouse);
  FDialog := ADialog;
  FUpdatingFontSizeRange := False;

  MarginAndGutterGroupBox.Caption := dlgMarginGutter;
  VisibleRightMarginCheckBox.Caption := dlgVisibleRightMargin;
  VisibleGutterCheckBox.Caption := dlgVisibleGutter;
  ShowLineNumbersCheckBox.Caption := dlgShowLineNumbers;
  ShowOnlyLineNumbersMultiplesOfLabel.Caption := lisEveryNThLineNumber;
  GutterSeparatorIndexLabel.Caption := dlgGutterSeparatorIndex;
  RightMarginLabel.Caption := dlgRightMargin;
  EditorFontGroupBox.Caption := dlgDefaultEditorFont;
  EditorFontSizeLabel.Caption := dlgEditorFontSize;
  ExtraCharSpacingLabel.Caption := dlgExtraCharSpacing;
  ExtraLineSpacingLabel.Caption := dlgExtraLineSpacing;
  DisableAntialiasingCheckBox.Caption := dlgDisableAntialiasing;
  RightMarginColorLink.Caption := dlgColorLink;

  with GeneralPage do
    AddPreviewEdit(DisplayPreview);
end;

procedure TEditorDisplayOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    // init the spin-edit first, since it does not trigger on change,
    // but is copied when checkboxes are initalized
    ShowOnlyLineNumbersMultiplesOfSpinEdit.Value := ShowOnlyLineNumbersMultiplesOf;
    GutterSeparatorIndexSpinBox.Value := GutterSeparatorIndex;
    VisibleRightMarginCheckBox.Checked := VisibleRightMargin;
    VisibleGutterCheckBox.Checked := VisibleGutter;
    ShowLineNumbersCheckBox.Checked := ShowLineNumbers;
    VisibleRightMarginCheckBox.Checked := VisibleRightMargin;
    SetComboBoxText(RightMarginComboBox, IntToStr(RightMargin),cstCaseInsensitive);
    SetComboBoxText(EditorFontComboBox, EditorFont,cstCaseInsensitive);
    SetEditorFontSizeSpinEditValue(EditorFontSize);
    SetComboBoxText(ExtraCharSpacingComboBox, IntToStr(ExtraCharSpacing),cstCaseInsensitive);
    SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(ExtraLineSpacing),cstCaseInsensitive);
    DisableAntialiasingCheckBox.Checked := DisableAntialiasing;
  end;

  ShowOnlyLineNumbersMultiplesOfLabel.Enabled := ShowLineNumbersCheckBox.Checked;
  ShowOnlyLineNumbersMultiplesOfSpinEdit.Enabled := ShowLineNumbersCheckBox.Checked;
end;

procedure TEditorDisplayOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    VisibleRightMargin := VisibleRightMarginCheckBox.Checked;
    VisibleGutter := VisibleGutterCheckBox.Checked;
    ShowLineNumbers := ShowLineNumbersCheckBox.Checked;
    ShowOnlyLineNumbersMultiplesOf := ShowOnlyLineNumbersMultiplesOfSpinEdit.Value;
    GutterSeparatorIndex := GutterSeparatorIndexSpinBox.Value;
    VisibleRightMargin := VisibleRightMarginCheckBox.Checked;
    RightMargin := StrToIntDef(RightMarginComboBox.Text, 80);
    EditorFont := EditorFontComboBox.Text;
    EditorFontSize := EditorFontSizeSpinEdit.Value;
    ExtraCharSpacing := StrToIntDef(ExtraCharSpacingComboBox.Text, ExtraCharSpacing);
    ExtraLineSpacing := StrToIntDef(ExtraLineSpacingComboBox.Text, ExtraLineSpacing);
    DisableAntialiasing := DisableAntialiasingCheckBox.Checked;
  end;
end;

class function TEditorDisplayOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorDisplayOptionsFrame, EdtOptionsDisplay);
end.

