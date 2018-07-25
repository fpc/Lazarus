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

  MultiPaste text - Lazarus addon

  Author:        Silvio Clecio  (https://github.com/silvioprog)
  Inspired by:   Delphi Multi Paste
  Last Modified: Fri Sep 30 15:22:18 EDT 2016

  Abstract:

  The MultiPaste dialog enables you to modify and paste into the
  Source Editor the text you copied to the clipboard. The MultiPaste feature
  helps you to work with SQL, HTML, JSON, formatted text, and any other text.

}

unit MultiPasteDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // LCL
  StdCtrls, ButtonPanel, Forms, Clipbrd,
  // SynEdit
  SynEdit,
  // IdeIntf
  IDEHelpIntf,
  // IDE
  InputHistory, IDEProcs, LazarusIDEStrConsts;

const
  hlFormatPasteTxtBefore = 'FormatPasteTxtBefore';
  hlFormatPasteTxtAfter = 'FormatPasteTxtAfter';

type

  { TMultiPasteDialog }

  TMultiPasteDialog = class(TForm)
    BottomButtonPanel: TButtonPanel;
    PreviewSynEdit: TSynEdit;
    TrimClipbrdContentsCheckBox: TCheckBox;
    EscQuotesCheckBox: TCheckBox;
    EscQuotesStyleComboBox: TComboBox;
    TxtAfterLinesComboBox: TComboBox;
    TxtBeforeLinesComboBox: TComboBox;
    TxtBeforeLinesLabel: TLabel;
    TxtAfterLinesLabel: TLabel;
    PasteOptsGroupBox: TGroupBox;
    PreviewGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContent: TStringList;
  protected
    procedure DoWatch(Sender: TObject); virtual;
    procedure DoUpdatePreview; virtual;
    function DoFormatContent(const AContent: string): string; virtual;
    procedure DoEscQuotesCheckBoxChange(Sender: TObject); virtual;
    procedure DoHelpButtonClick(Sender: TObject); virtual;
  public
    property Content: TStringList read FContent;
  end;

implementation

{$R *.lfm}

procedure TMultiPasteDialog.FormCreate(Sender: TObject);
var
  List: THistoryList;
begin
  FContent := TStringList.Create;
  {$IF FPC_FULLVERSION >= 30101}
  FContent.SkipLastLineBreak := True;
  {$ENDIF}

  OnShow := @DoWatch;
  OnActivate := @DoWatch;
  TxtBeforeLinesComboBox.OnChange := @DoWatch;
  TxtAfterLinesComboBox.OnChange := @DoWatch;
  EscQuotesCheckBox.OnChange := @DoEscQuotesCheckBoxChange;
  EscQuotesStyleComboBox.OnChange := @DoWatch;
  TrimClipbrdContentsCheckBox.OnChange := @DoWatch;
  BottomButtonPanel.HelpButton.OnClick := @DoHelpButtonClick;

  List:=InputHistories.HistoryLists.GetList(hlFormatPasteTxtBefore,true,rltCaseSensitive);
  List.AppendEntry('Add(''');
  TxtBeforeLinesComboBox.Items.Assign(List);
  TxtBeforeLinesComboBox.Text:=List[0];

  List:=InputHistories.HistoryLists.GetList(hlFormatPasteTxtAfter,true,rltCaseSensitive);
  List.AppendEntry(''');');
  TxtAfterLinesComboBox.Items.Assign(List);
  TxtAfterLinesComboBox.Text:=List[0];

  Caption := lismpMultiPaste;
  PasteOptsGroupBox.Caption := lismpPasteOptions;
  TxtBeforeLinesLabel.Caption := lismpTextBeforeEachLine;
  TxtAfterLinesLabel.Caption := lismpTextAfterEachLine;
  EscQuotesCheckBox.Caption := lismpEscapeQuotes;
  EscQuotesStyleComboBox.Items[0] := lismpPascalStyle;
  EscQuotesStyleComboBox.Items[1] := lismpCStyle;
  TrimClipbrdContentsCheckBox.Caption := lismpTrimClipboardContents;
  PreviewGroupBox.Caption := lismpPreview;
end;

procedure TMultiPasteDialog.FormDestroy(Sender: TObject);
begin
  TxtBeforeLinesComboBox.AddHistoryItem(TxtBeforeLinesComboBox.Text,20,true,false);
  InputHistories.HistoryLists.GetList(hlFormatPasteTxtBefore,true,rltCaseSensitive)
    .Assign(TxtBeforeLinesComboBox.Items);

  TxtAfterLinesComboBox.AddHistoryItem(TxtAfterLinesComboBox.Text,20,true,false);
  InputHistories.HistoryLists.GetList(hlFormatPasteTxtAfter,true,rltCaseSensitive)
    .Assign(TxtAfterLinesComboBox.Items);

  FreeAndNil(FContent);
end;

procedure TMultiPasteDialog.DoWatch(Sender: TObject);
begin
  DoUpdatePreview;
end;

procedure TMultiPasteDialog.DoUpdatePreview;
begin
  if Clipboard.HasFormat(CF_TEXT) then
    PreviewSynEdit.Lines.Text := DoFormatContent(Clipboard.AsText)
  else
    PreviewSynEdit.Clear;
end;

function TMultiPasteDialog.DoFormatContent(const AContent: string): string;
var
  I: Integer;
  S: string;
begin
  FContent.Text := AContent;
  for I := 0 to Pred(FContent.Count) do
  begin
    S := FContent[I];
    if TrimClipbrdContentsCheckBox.Checked then
      S := Trim(S);
    if EscQuotesCheckBox.Checked then
      case EscQuotesStyleComboBox.ItemIndex of
        0: S := StringReplace(S, '''', '''''', [rfReplaceAll]);
        1: S := StringReplace(S, '"', '\"', [rfReplaceAll]);
      end;
    FContent[I] := Concat(TxtBeforeLinesComboBox.Text, S, TxtAfterLinesComboBox.Text);
  end;
  Result := FContent.Text;
end;

procedure TMultiPasteDialog.DoEscQuotesCheckBoxChange(Sender: TObject);
begin
  DoWatch(Sender);
  EscQuotesStyleComboBox.Enabled := EscQuotesCheckBox.Checked;
end;

procedure TMultiPasteDialog.DoHelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

end.

