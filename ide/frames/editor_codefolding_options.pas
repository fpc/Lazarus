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
unit editor_codefolding_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ExtCtrls, Graphics,
  LCLType, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, Controls,
  SynEditHighlighter, SynEditHighlighterFoldBase, Spin, ComCtrls, ColorBox, CheckLst, Buttons;

type

  { TEditorCodefoldingOptionsFrame }

  TEditorCodefoldingOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    FoldConfigCheckListBox: TCheckListBox;
    FoldConfPanel: TPanel;
    chkCodeFoldingEnabled: TCheckBox;
    LanguageLabel: TLabel;
    LanguageComboBox: TComboBox;
    procedure FoldConfigCheckListBoxClickCheck(Sender: TObject);
    procedure FoldConfigCheckListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure LanguageComboBoxExit(Sender: TObject);
    procedure LanguageComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FHighlighters: array[TLazSyntaxHighlighter] of TSrcIDEHighlighter;
    FCurHighlighter: TSrcIDEHighlighter;
    FCurFoldInfo: TEditorOptionsFoldRecord;
  protected
    function GetHighlighter(SynType: TLazSyntaxHighlighter;
      CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    procedure ClearHighlighters;
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TEditorCodefoldingOptionsFrame }

procedure TEditorCodefoldingOptionsFrame.LanguageComboBoxChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorCodefoldingOptionsFrame.LanguageComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorCodefoldingOptionsFrame.LanguageComboBoxExit(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
  tp: TLazSyntaxHighlighter;
  i: Integer;
begin
  tp := EditorOpts.HighlighterList
          [EditorOpts.HighlighterList.FindByName(ComboBox.Text)].TheType;
  FCurHighlighter := GetHighlighter(tp, True);
  FCurFoldInfo := EditorOptionsFoldDefaults[tp];

  FoldConfigCheckListBox.Clear;
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;

  for i := 0 to FCurFoldInfo.Count - 1 do begin
    FoldConfigCheckListBox.Items.add(FCurFoldInfo.Info^[i].Name);
    FoldConfigCheckListBox.Checked[i] :=
      TSynCustomFoldHighlighter(FCurHighlighter).FoldConfig[FCurFoldInfo.Info^[i].Index];
  end;
end;

procedure TEditorCodefoldingOptionsFrame.FoldConfigCheckListBoxClickCheck(Sender: TObject);
var
  i: Integer;
begin
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  for i := 0 to FCurFoldInfo.Count - 1 do
    TSynCustomFoldHighlighter(FCurHighlighter).FoldConfig[FCurFoldInfo.Info^[i].Index]
      := FoldConfigCheckListBox.Checked[i];
end;

procedure TEditorCodefoldingOptionsFrame.FoldConfigCheckListBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FoldConfigCheckListBoxClickCheck(Sender);
end;

function TEditorCodefoldingOptionsFrame.GetHighlighter(SynType: TLazSyntaxHighlighter;
  CreateIfNotExists: Boolean): TSrcIDEHighlighter;
var
  SynClass: TCustomSynClass;
begin
  Result := FHighlighters[SynType];
  if (Result <> nil) or not(CreateIfNotExists) then exit;

  SynClass := LazSyntaxHighlighterClasses[SynType];
  Result := SynClass.Create(nil);
  FHighlighters[SynType] := Result;
  EditorOpts.ReadHighlighterFoldSettings(Result);
end;

procedure TEditorCodefoldingOptionsFrame.ClearHighlighters;
var
  i: TLazSyntaxHighlighter;
begin
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    FreeAndNil(FHighlighters[i]);
end;

destructor TEditorCodefoldingOptionsFrame.Destroy;
begin
  ClearHighlighters;
  inherited Destroy;
end;

function TEditorCodefoldingOptionsFrame.GetTitle: String;
begin
  Result := dlgUseCodeFolding;
end;

procedure TEditorCodefoldingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  chkCodeFoldingEnabled.Caption := dlgUseCodeFolding;
  LanguageLabel.Caption := dlgLang;
end;

procedure TEditorCodefoldingOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  i: Integer;
  rf: TEditorOptionsFoldRecord;
begin
  with AOptions as TEditorOptions do
  begin
    chkCodeFoldingEnabled.Checked := UseCodeFolding;

    with LanguageComboBox.Items do begin
      BeginUpdate;
      for i := 0 to EditorOpts.HighlighterList.Count - 1 do begin
        rf := EditorOptionsFoldDefaults[HighlighterList[i].TheType];
        if (rf.Count > 0) then
          Add(HighlighterList[i].SynClass.GetLanguageName);
      end;
      EndUpdate;
    end;
    LanguageComboBox.ItemIndex := 0;
    LanguageComboBoxExit(LanguageComboBox);
  end;
end;

procedure TEditorCodefoldingOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  i: TLazSyntaxHighlighter;
begin
  with AOptions as TEditorOptions do
  begin
    UseCodeFolding := chkCodeFoldingEnabled.Checked;
    for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do begin
      if assigned(FHighlighters[i]) then
         WriteHighlighterFoldSettings(FHighlighters[i]);
    end;
  end;
end;

class function TEditorCodefoldingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  {$I editor_codefolding_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodefoldingOptionsFrame, EdtOptionsCodeFolding);
end.

