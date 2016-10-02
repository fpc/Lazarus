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
  SysUtils, Classes, StdCtrls, ButtonPanel, Forms, Clipbrd, IDEHelpIntf, SynEdit;

type
  TMultiPasteDialog = class(TForm)
    BottomButtonPanel: TButtonPanel;
    PreviewSynEdit: TSynEdit;
    TrimClipbrdContentsCheckBox: TCheckBox;
    EscQuotesCheckBox: TCheckBox;
    EscQuotesStyleComboBox: TComboBox;
    TxtBeforeLinesEdit: TEdit;
    TxtAfterLinesEdit: TEdit;
    TxtBeforeLinesLabel: TLabel;
    TxtAfterLinesLabel: TLabel;
    PasteOptsGroupBox: TGroupBox;
    PreviewGroupBox: TGroupBox;
  private
    FContent: TStringList;
  protected
    procedure DoWatch(Sender: TObject); virtual;
    procedure DoUpdatePreview; virtual;
    function DoFormatContent(const AContent: string): string; virtual;
    procedure DoEscQuotesCheckBoxChange(Sender: TObject); virtual;
    procedure DoHelpButtonClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Content: TStringList read FContent;
  end;

implementation

{$R *.lfm}

constructor TMultiPasteDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContent := TStringList.Create;
  FContent.SkipLastLineBreak := True;
  OnShow := @DoWatch;
  OnActivate := @DoWatch;
  TxtBeforeLinesEdit.OnChange := @DoWatch;
  TxtAfterLinesEdit.OnChange := @DoWatch;
  EscQuotesCheckBox.OnChange := @DoEscQuotesCheckBoxChange;
  EscQuotesStyleComboBox.OnChange := @DoWatch;
  TrimClipbrdContentsCheckBox.OnChange := @DoWatch;
  BottomButtonPanel.HelpButton.OnClick := @DoHelpButtonClick;
end;

destructor TMultiPasteDialog.Destroy;
begin
  FContent.Free;
  inherited Destroy;
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
    FContent[I] := Concat(TxtBeforeLinesEdit.Text, S, TxtAfterLinesEdit.Text);
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

