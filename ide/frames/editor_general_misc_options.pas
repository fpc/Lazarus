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
unit editor_general_misc_options;

{$mode objfpc}{$H+}

interface

uses
  LCLProc, StdCtrls, SynEdit, ExtCtrls, ComCtrls, EditorOptions,
  LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf, editor_general_options,
  SynEditTextTrimmer;

type
  { TEditorGeneralMiscOptionsFrame }

  TEditorGeneralMiscOptionsFrame = class(TAbstractIDEOptionsEditor)
    EditorTrimSpaceTypeCheckBox: TComboBox;
    EditorOptionsGroupBox: TCheckGroup;
    EditorTabPositionCheckBox: TComboBox;
    EditorTrimSpaceTypeLabel: TLabel;
    EditorTabPositionLabel: TLabel;
    procedure EditorOptionsGroupBoxItemClick(Sender: TObject; Index: integer);
  private
    FDialog: TAbstractOptionsEditorDialog;
    fLoaded: Boolean;
    FSaved: Boolean;
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
  end;
  EditorTrimSpaceTypeCheckBox.Items.Add(dlgTrimSpaceTypeLeaveLine);
  EditorTrimSpaceTypeCheckBox.Items.Add(dlgTrimSpaceTypeEditLine);
  EditorTrimSpaceTypeCheckBox.Items.Add(dlgTrimSpaceTypeCaretMove);
  EditorTrimSpaceTypeCheckBox.Items.Add(dlgTrimSpaceTypePosOnly);
  EditorTrimSpaceTypeLabel.Caption := dlgTrimSpaceTypeCaption;
  EditorTabPositionCheckBox.Items.Add(dlgNotebookTabPosTop);
  EditorTabPositionCheckBox.Items.Add(dlgNotebookTabPosBottom);
  EditorTabPositionCheckBox.Items.Add(dlgNotebookTabPosLeft);
  EditorTabPositionCheckBox.Items.Add(dlgNotebookTabPosRight);
  EditorTabPositionLabel.Caption := dlgNotebookTabPos;
end;

procedure TEditorGeneralMiscOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
const
  TabPosToIndex : Array [TTabPosition] of Integer = (0, 1, 2, 3);
begin
  if fLoaded then exit;
  fLoaded:=true;
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
    end;
    EditorTrimSpaceTypeCheckBox.ItemIndex := ord(TrimSpaceType);
    EditorTabPositionCheckBox.ItemIndex := TabPosToIndex[TabPosition];
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

const
  TabIndexToPos : Array [0..3] of TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);
begin
  if FSaved then exit;
  FSaved:=true;
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
    TrimSpaceType := TSynEditStringTrimmingType(EditorTrimSpaceTypeCheckBox.ItemIndex);
    TabPosition := TabIndexToPos[EditorTabPositionCheckBox.ItemIndex];
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

