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
unit editor_codefolding_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, StdCtrls, ExtCtrls, Graphics, CheckLst,
  // IdeIntf
  EditorSyntaxHighlighterDef, IDEOptionsIntf, IDEOptEditorIntf,
  // SynEdit
  SynEditHighlighterFoldBase,
  // IDE
  EditorOptions, LazarusIDEStrConsts;

type

  { TEditorCodefoldingOptionsFrame }

  TEditorCodefoldingOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    chkPopOrder: TCheckBox;
    FoldConfigCheckListBox: TCheckListBox;
    FoldConfPanel: TPanel;
    chkCodeFoldingEnabled: TCheckBox;
    LanguageLabel: TLabel;
    LanguageComboBox: TComboBox;
    pnlFoldHide: TPanel;
    chkFold: TRadioButton;
    chkHide: TRadioButton;
    chkBoth: TRadioButton;
    procedure chkCodeFoldingEnabledChange(Sender: TObject);
    procedure chkFoldChange(Sender: TObject);
    procedure FoldConfigCheckListBoxClick(Sender: TObject);
    procedure FoldConfigCheckListBoxClickCheck(Sender: TObject);
    procedure FoldConfigCheckListBoxKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure LanguageComboBoxExit(Sender: TObject);
    procedure LanguageComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FHighlighters: array[TLazSyntaxHighlighter] of TSrcIDEHighlighter;
    FCurHighlighter: TSrcIDEHighlighter;
    FCurFoldInfo: TEditorOptionsFoldRecord;
    FModeLock: Boolean;
    procedure UpdateFoldHideRadio;
  protected
    procedure ClearHighlighters;
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    function GetHighlighter(SynType: TLazSyntaxHighlighter;
      CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

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
  i, j: Integer;
  Hl: TSynCustomFoldHighlighter;
begin
  tp := EditorOpts.HighlighterList
          [EditorOpts.HighlighterList.FindByName(ComboBox.Text)].TheType;
  FCurHighlighter := GetHighlighter(tp, True);
  FCurFoldInfo := EditorOptionsFoldDefaults[tp];

  FoldConfigCheckListBox.Clear;
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);

  for i := 0 to FCurFoldInfo.Count - 1 do begin
    if Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].SupportedModes * [fmFold, fmHide] <> [] then begin
      j := FoldConfigCheckListBox.Items.Add(FCurFoldInfo.Info^[i].Name);
      FoldConfigCheckListBox.Checked[j] :=
        Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].Enabled and
        (Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].Modes * [fmFold, fmHide] <> []);
      FoldConfigCheckListBox.Items.Objects[j] := TObject({%H-}Pointer(PtrUInt(i)));
    end;
  end;
end;

procedure TEditorCodefoldingOptionsFrame.FoldConfigCheckListBoxClickCheck(Sender: TObject);
var
  i, j: Integer;
  Hl: TSynCustomFoldHighlighter;
begin
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);
  j := 0;
  for i := 0 to FCurFoldInfo.Count - 1 do begin
    if Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].SupportedModes * [fmFold, fmHide] <> [] then begin
      Hl.FoldConfig[FCurFoldInfo.Info^[i].Index].Enabled
        := FoldConfigCheckListBox.Checked[j];
      inc(j);
    end;
  end;

  UpdateFoldHideRadio;
end;

procedure TEditorCodefoldingOptionsFrame.UpdateFoldHideRadio;
var
  i: LongInt;
  AvailModes, Modes: TSynCustomFoldConfigModes;
  Hl: TSynCustomFoldHighlighter;
begin
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);
  FModeLock := True;
  i := FoldConfigCheckListBox.ItemIndex;
  if i >= 0 then
    i := PtrUInt(FoldConfigCheckListBox.Items.Objects[i]);
  AvailModes := [];
  Modes := [fmFold];
  if i >= 0 then begin
    i := FCurFoldInfo.Info^[i].Index;
    AvailModes := Hl.FoldConfig[i].SupportedModes * [fmFold, fmHide];
    Modes := Hl.FoldConfig[i].Modes * [fmFold, fmHide];
  end;
  chkFold.Checked := Modes = [fmFold];
  chkHide.Checked := Modes = [fmHide];
  chkBoth.Checked := Modes = [fmFold, fmHide];
  pnlFoldHide.Enabled := AvailModes = [fmFold, fmHide];
  FModeLock := False;
end;

procedure TEditorCodefoldingOptionsFrame.FoldConfigCheckListBoxClick(Sender: TObject);
begin
  UpdateFoldHideRadio;
end;

procedure TEditorCodefoldingOptionsFrame.chkFoldChange(Sender: TObject);
var
  Hl: TSynCustomFoldHighlighter;
  Modes: TSynCustomFoldConfigModes;
  i: LongInt;
begin
  if FModeLock then exit;
  if not (assigned(FCurHighlighter) and
         (FCurHighlighter is TSynCustomFoldHighlighter)) then exit;
  i := FoldConfigCheckListBox.ItemIndex;
  if i < 0 then exit;
  i := PtrUInt(FoldConfigCheckListBox.Items.Objects[i]);
  i := FCurFoldInfo.Info^[i].Index;
  Hl := TSynCustomFoldHighlighter(FCurHighlighter);
  Modes := [fmFold];
  if chkHide.Checked then Modes := [fmHide];
  if chkBoth.Checked then Modes := [fmFold, fmHide];
  Hl.FoldConfig[i].Modes := (Hl.FoldConfig[i].Modes - [fmFold, fmHide]) + Modes;
end;

procedure TEditorCodefoldingOptionsFrame.chkCodeFoldingEnabledChange(Sender: TObject);
var
  b: Boolean;
begin
  b := (Sender as TCheckBox).Checked;
  LanguageComboBox.Enabled := b;
  FoldConfigCheckListBox.Enabled := b;
  chkPopOrder.Enabled := b;
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
  Result.BeginUpdate;
  EditorOpts.ReadHighlighterFoldSettings(Result, True);
  result.EndUpdate;
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
  chkFold.Caption := dlgCodeFoldEnableFold;
  chkHide.Caption := dlgCodeFoldEnableHide;
  chkBoth.Caption := dlgCodeFoldEnableBoth;
  chkPopOrder.Caption := dlgCodeFoldPopUpOrder;
  LanguageLabel.Caption := dlgLang;
  FModeLock := False;
  chkCodeFoldingEnabledChange(chkCodeFoldingEnabled);
end;

procedure TEditorCodefoldingOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  i: Integer;
  rf: TEditorOptionsFoldRecord;
begin
  FModeLock := False;
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
    chkPopOrder.Checked := ReverseFoldPopUpOrder;
  end;
  UpdateFoldHideRadio;
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
    ReverseFoldPopUpOrder := chkPopOrder.Checked;
  end;
end;

class function TEditorCodefoldingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodefoldingOptionsFrame, EdtOptionsCodeFolding);
end.

