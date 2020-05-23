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
unit editor_general_misc_options;

{$mode objfpc}{$H+}

{$IFDEF Windows}
  {$IFnDEF WithoutWinIME}
    {$DEFINE WinIME}
  {$ENDIF}
{$ENDIF}

interface

uses
  // LCL
  LCLProc, StdCtrls, ExtCtrls, Spin,
  // SynEdit
  SynEdit, SynEditTextTrimmer,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, DividerBevel, CheckBoxThemed,
  // IDE
  EditorOptions, LazarusIDEStrConsts, editor_general_options;

type
  { TEditorGeneralMiscOptionsFrame }

  TEditorGeneralMiscOptionsFrame = class(TAbstractIDEOptionsEditor)
    DBRigthScroll: TDividerBevel;
    DBLeftScroll: TDividerBevel;
    EditorTrimSpaceTypeComboBox: TComboBox;
    EditorOptionsGroupBox: TCheckGroup;
    EditorTrimSpaceTypeLabel: TLabel;
    lbLeftDist: TLabel;
    lbLeftDistP: TLabel;
    lbLeftAdd: TLabel;
    lbLeftAddP: TLabel;
    lbLeftAddMax: TLabel;
    lbRightDist: TLabel;
    lbRightDistP: TLabel;
    lbRightAdd: TLabel;
    lbRightAddP: TLabel;
    lbRightAddMax: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    edLeftDist: TSpinEdit;
    edLeftDistP: TSpinEdit;
    edLeftAdd: TSpinEdit;
    edLeftAddP: TSpinEdit;
    edLeftAddMax: TSpinEdit;
    edRightDist: TSpinEdit;
    edRightDistP: TSpinEdit;
    edRightAdd: TSpinEdit;
    edRightAddP: TSpinEdit;
    edRightAddMax: TSpinEdit;
    procedure EditorOptionsGroupBoxItemClick(Sender: TObject; {%H-}Index: integer);
  private
    FDialog: TAbstractOptionsEditorDialog;
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorGeneralMiscOptionsFrame }

function TEditorGeneralMiscOptionsFrame.GetTitle: String;
begin
  Result := dlgEdMisc;
end;

procedure TEditorGeneralMiscOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;
  EditorOptionsGroupBox.Caption := dlgEditorOptions;
  // Warning:
  // Only append new items at the end of list.
  // since revision 23597 the order of boxes is hardcoded in Read/WriteSettings
  with EditorOptionsGroupBox do
  begin
    // visual effects
    //Items.Add(dlgShowGutterHints);  // unimplemented
    Items.Add(lisShowSpecialCharacters);
    // spaces
    Items.Add(dlgTrimTrailingSpaces);
    // copying
    Items.Add(dlgFindTextatCursor);
    Items.Add(dlgCopyWordAtCursorOnCopyNone);
    Items.Add(dlgCopyPasteKeepFolds);
    {$IFDEF WinIME}
    Items.Add(dlgUseMinimumIme);
    {$ENDIF}
  end;
  EditorTrimSpaceTypeComboBox.Items.Add(dlgTrimSpaceTypeLeaveLine);
  EditorTrimSpaceTypeComboBox.Items.Add(dlgTrimSpaceTypeEditLine);
  EditorTrimSpaceTypeComboBox.Items.Add(dlgTrimSpaceTypeCaretMove);
  EditorTrimSpaceTypeComboBox.Items.Add(dlgTrimSpaceTypePosOnly);
  EditorTrimSpaceTypeLabel.Caption := dlgTrimSpaceTypeCaption;

  DBLeftScroll.Caption := lisDynPkgAutoScrollOnDeletePa;
  lbLeftDist.Caption   := lisDynPkgTriggerOnMinCharsVis;
  lbLeftDistP.Caption  := lisDynPkgTriggerOnMinCharsOfW;
  lbLeftAdd.Caption    := lisDynPkgAmountToScrollIn;
  lbLeftAddP.Caption   := lisDynPkgAmountToScrollIn2;
  lbLeftAddMax.Caption := lisDynPkgAmountToScrollInMax;
  DBRigthScroll.Caption := lisDynPkgAutoScrollOnTypePast;
  lbRightDist.Caption   := lisDynPkgTriggerOnMinCharsVis;
  lbRightDistP.Caption  := lisDynPkgTriggerOnMinCharsOfW;
  lbRightAdd.Caption    := lisDynPkgAmountToScrollIn;
  lbRightAddP.Caption   := lisDynPkgAmountToScrollIn2;
  lbRightAddMax.Caption := lisDynPkgAmountToScrollInMax;
end;

procedure TEditorGeneralMiscOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    with EditorOptionsGroupBox do
    begin
      Checked[0] := eoShowSpecialChars in SynEditOptions;
      Checked[1] := eoTrimTrailingSpaces in SynEditOptions;
      //Checked[Items.IndexOf(dlgShowGutterHints)] := ShowGutterHints;
      Checked[2] := FindTextAtCursor;
      Checked[3] := CopyWordAtCursorOnCopyNone;
      Checked[4] := eoFoldedCopyPaste in SynEditOptions2;
      {$IFDEF WinIME}
      Checked[5] := UseMinimumIme;
      {$ENDIF}

      with ScrollOnEditLeftOptions do begin
        edLeftDist.Value   := KeepBorderDistance;
        edLeftDistP.Value  := KeepBorderDistancePercent;
        edLeftAdd.Value    := ScrollExtraColumns;
        edLeftAddP.Value   := ScrollExtraPercent;
        edLeftAddMax.Value := ScrollExtraMax;
      end;
      with ScrollOnEditRightOptions do begin
        edRightDist.Value   := KeepBorderDistance;
        edRightDistP.Value  := KeepBorderDistancePercent;
        edRightAdd.Value    := ScrollExtraColumns;
        edRightAddP.Value   := ScrollExtraPercent;
        edRightAddMax.Value := ScrollExtraMax;
      end;
    end;
    EditorTrimSpaceTypeComboBox.ItemIndex := ord(TrimSpaceType);
  end;
end;

procedure TEditorGeneralMiscOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption); overload;
  begin
    if AValue then
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions + [AnOption]
    else
      TEditorOptions(AOptions).SynEditOptions := TEditorOptions(AOptions).SynEditOptions - [AnOption];
  end;

begin
  with AOptions as TEditorOptions do
  begin
    UpdateOptionFromBool(EditorOptionsGroupBox.Checked[0], eoShowSpecialChars);
    UpdateOptionFromBool(EditorOptionsGroupBox.Checked[1], eoTrimTrailingSpaces);
    //ShowGutterHints := CheckGroupItemChecked(EditorOptionsGroupBox, dlgShowGutterHints);
    FindTextAtCursor := EditorOptionsGroupBox.Checked[2];
    CopyWordAtCursorOnCopyNone := EditorOptionsGroupBox.Checked[3];
    if EditorOptionsGroupBox.Checked[4] then
      SynEditOptions2 := SynEditOptions2 + [eoFoldedCopyPaste]
    else
      SynEditOptions2 := SynEditOptions2 - [eoFoldedCopyPaste];
    TrimSpaceType := TSynEditStringTrimmingType(EditorTrimSpaceTypeComboBox.ItemIndex);
    {$IFDEF WinIME}
    UseMinimumIme := EditorOptionsGroupBox.Checked[5];
    {$ENDIF}

      with ScrollOnEditLeftOptions do begin
        KeepBorderDistance := edLeftDist.Value;
        KeepBorderDistancePercent := edLeftDistP.Value;
        ScrollExtraColumns := edLeftAdd.Value;
        ScrollExtraPercent := edLeftAddP.Value;
        ScrollExtraMax := edLeftAddMax.Value;
      end;
      with ScrollOnEditRightOptions do begin
        KeepBorderDistance := edRightDist.Value;
        KeepBorderDistancePercent := edRightDistP.Value;
        ScrollExtraColumns := edRightAdd.Value;
        ScrollExtraPercent := edRightAddP.Value;
        ScrollExtraMax := edRightAddMax.Value;
      end;
  end;
end;

class function TEditorGeneralMiscOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

procedure TEditorGeneralMiscOptionsFrame.EditorOptionsGroupBoxItemClick(
  Sender: TObject; Index: integer);

  procedure SetOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    i: LongInt;
    a: Integer;
  begin
    i := EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i < 0 then
      Exit;

    with GeneralPage do
      for a := Low(PreviewEdits) to High(PreviewEdits) do
      begin
        if PreviewEdits[a] <> nil then
          if EditorOptionsGroupBox.Checked[i] then
            PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
          else
            PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
      end;
  end;

begin
  SetOption(lisShowSpecialCharacters, eoShowSpecialChars);
  SetOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);
end;

function TEditorGeneralMiscOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame;
  inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorGeneralMiscOptionsFrame, EdtOptionsGeneralMisc, EdtOptionsGeneral);
end.

