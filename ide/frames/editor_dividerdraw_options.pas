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
unit editor_dividerdraw_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  StdCtrls, ExtCtrls, Graphics, Spin, ColorBox, CheckLst, LCLType,
  // IdeIntf
  EditorSyntaxHighlighterDef, IDEOptionsIntf, IDEOptEditorIntf,
  // SynEdit
  SynEditHighlighter,
  // IDE
  EditorOptions, LazarusIDEStrConsts;

type

  { TEditorDividerDrawOptionsFrame }

  TEditorDividerDrawOptionsFrame = class(TAbstractIDEOptionsEditor)
    DividerConfigListBox: TCheckListBox;
    LanguageLabel: TLabel;
    TopLvlPanel: TPanel;
    TopLvlColorCheckBox: TCheckBox;
    NestLvlColorCheckBox: TCheckBox;
    TopLvlColorBox: TColorBox;
    NestLvlColorBox: TColorBox;
    DividerSpinLabel: TLabel;
    TopLvlColorLabel: TLabel;
    NestLvlColorLabel: TLabel;
    LanguageComboBox: TComboBox;
    DividerConfPanel: TPanel;
    DividerSpinPanel: TPanel;
    DividerSpinEdit: TSpinEdit;
    NestLvlPanel: TPanel;
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure LanguageComboBoxExit(Sender: TObject);
    procedure LanguageComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DividerConfigListBoxKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure DividerConfigListBoxClick(Sender: TObject);
    procedure DividerSpinEditChange(Sender: TObject);
    procedure NestLvlColorBoxChange(Sender: TObject);
    procedure NestLvlColorCheckBoxChange(Sender: TObject);
    procedure TopLvlColorBoxChange(Sender: TObject);
    procedure TopLvlColorCheckBoxChange(Sender: TObject);
  private
    FHighlighters: array[TLazSyntaxHighlighter] of TSrcIDEHighlighter;
    FCurHighlighter: TSrcIDEHighlighter;
    FCurDividerConf: TSynDividerDrawConfig;
    FCurDivInfo: TEditorOptionsDividerRecord;
  protected
    function GetHighlighter(SynType: TLazSyntaxHighlighter;
      CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    procedure ClearHighlighters;
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorDividerDrawOptionsFrame }

procedure TEditorDividerDrawOptionsFrame.LanguageComboBoxChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorDividerDrawOptionsFrame.LanguageComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    LanguageComboBoxExit(Sender);
end;

procedure TEditorDividerDrawOptionsFrame.LanguageComboBoxExit(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
  tp: TLazSyntaxHighlighter;
  i: Integer;
begin
  tp := EditorOpts.HighlighterList
          [EditorOpts.HighlighterList.FindByName(ComboBox.Text)].TheType;
  FCurHighlighter := GetHighlighter(tp, True);
  FCurDivInfo := EditorOptionsDividerDefaults[tp];

  DividerConfigListBox.Clear;
  for i := 0 to FCurDivInfo.Count - 1 do begin
    DividerConfigListBox.Items.add(FCurDivInfo.Info^[i].Name);
    DividerConfigListBox.Checked[i] :=
      FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth > 0;
  end;
  DividerConfigListBox.ItemIndex := 0;
  DividerConfigListBoxClick(Sender);
end;

procedure TEditorDividerDrawOptionsFrame.DividerConfigListBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  DividerConfigListBoxClick(Sender);
end;

procedure TEditorDividerDrawOptionsFrame.DividerConfigListBoxClick(Sender: TObject);
var
  i: LongInt;
  NewDiv: TSynDividerDrawConfig;
  b: Boolean;
begin
  if not assigned(FCurHighlighter) then exit;
  for i := 0 to FCurDivInfo.Count - 1 do begin
    if DividerConfigListBox.Checked[i] then begin
      if FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth = 0 then begin
        if FCurDivInfo.Info^[i].MaxLevel > 0 then
          FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth := FCurDivInfo.Info^[i].MaxLevel
        else
          FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth := 1;
      end;
    end else begin
      if FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth > 0 then
        FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth := 0;
    end;
  end;

  i := DividerConfigListBox.ItemIndex;
  if (i < 0) or (i >= FCurDivInfo.Count) then exit;
  NewDiv := FCurHighlighter.DividerDrawConfig[i];
  FCurDividerConf := nil;

  b := FCurDivInfo.Info^[i].BoolOpt;
  DividerSpinPanel.Visible := not b;
  NestLvlPanel.Visible := not b;
  DividerSpinEdit.Value := NewDiv.MaxDrawDepth;

  TopLvlColorBox.Selected := NewDiv.TopColor;
  TopLvlColorBox.Tag := TopLvlColorBox.Selected;
  TopLvlColorCheckBox.Checked := NewDiv.TopColor = clDefault;
  TopLvlPanel.Enabled := NewDiv.MaxDrawDepth > 0;

  NestLvlColorBox.Selected := NewDiv.NestColor;
  NestLvlColorBox.Tag := NestLvlColorBox.Selected;
  NestLvlColorCheckBox.Checked := NewDiv.NestColor = clDefault;
  NestLvlPanel.Enabled := NewDiv.MaxDrawDepth > 1;

  FCurDividerConf := NewDiv;
end;

procedure TEditorDividerDrawOptionsFrame.DividerSpinEditChange(Sender: TObject);
var
  Spin: TSpinEdit absolute Sender;
  i: Integer;
begin
  if not assigned(FCurDividerConf) then exit;
  FCurDividerConf.MaxDrawDepth := Spin.Value;
  TopLvlPanel.Enabled := FCurDividerConf.MaxDrawDepth > 0;
  NestLvlPanel.Enabled := FCurDividerConf.MaxDrawDepth > 1;
  for i := 0 to DividerConfigListBox.Count - 1 do
    DividerConfigListBox.Checked[i] :=
      FCurHighlighter.DividerDrawConfig[i].MaxDrawDepth > 0;
end;

procedure TEditorDividerDrawOptionsFrame.NestLvlColorBoxChange(Sender: TObject);
begin
  if not assigned(FCurDividerConf) then exit;
  FCurDividerConf.NestColor := NestLvlColorBox.Selected;
  if NestLvlColorBox.Selected <> clDefault then
    NestLvlColorBox.Tag := NestLvlColorBox.Selected;
  NestLvlColorCheckBox.Checked := NestLvlColorBox.Selected = clDefault;
end;

procedure TEditorDividerDrawOptionsFrame.NestLvlColorCheckBoxChange(Sender: TObject);
begin
  if not assigned(FCurDividerConf) then exit;
  if NestLvlColorCheckBox.Checked then begin
    FCurDividerConf.NestColor := clDefault;
    if NestLvlColorBox.Selected <> clDefault then
      NestLvlColorBox.Selected := clDefault;
  end else begin
    FCurDividerConf.NestColor := NestLvlColorBox.Tag;
    if NestLvlColorBox.Selected <> NestLvlColorBox.Tag then
      NestLvlColorBox.Selected := NestLvlColorBox.Tag;
  end;
end;

procedure TEditorDividerDrawOptionsFrame.TopLvlColorBoxChange(Sender: TObject);
begin
  if not assigned(FCurDividerConf) then exit;
  FCurDividerConf.TopColor := TopLvlColorBox.Selected;
  if TopLvlColorBox.Selected <> clDefault then
    TopLvlColorBox.Tag := TopLvlColorBox.Selected;
  TopLvlColorCheckBox.Checked := TopLvlColorBox.Selected = clDefault;
end;

procedure TEditorDividerDrawOptionsFrame.TopLvlColorCheckBoxChange(Sender: TObject);
begin
  if not assigned(FCurDividerConf) then exit;
  if TopLvlColorCheckBox.Checked then begin
    FCurDividerConf.TopColor := clDefault;
    if TopLvlColorBox.Selected <> clDefault then
      TopLvlColorBox.Selected := clDefault;
  end else begin
    FCurDividerConf.TopColor := TopLvlColorBox.Tag;
    if TopLvlColorBox.Selected <> TopLvlColorBox.Tag then
      TopLvlColorBox.Selected := TopLvlColorBox.Tag;
  end;
end;

function TEditorDividerDrawOptionsFrame.GetHighlighter(SynType: TLazSyntaxHighlighter;
  CreateIfNotExists: Boolean): TSrcIDEHighlighter;
var
  SynClass: TCustomSynClass;
begin
  Result := FHighlighters[SynType];
  if (Result <> nil) or not(CreateIfNotExists) then exit;

  SynClass := LazSyntaxHighlighterClasses[SynType];
  Result := SynClass.Create(nil);
  FHighlighters[SynType] := Result;
  EditorOpts.ReadHighlighterDivDrawSettings(Result);
end;

procedure TEditorDividerDrawOptionsFrame.ClearHighlighters;
var
  i: TLazSyntaxHighlighter;
begin
  for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do
    FreeAndNil(FHighlighters[i]);
end;

destructor TEditorDividerDrawOptionsFrame.Destroy;
begin
  ClearHighlighters;
  inherited Destroy;
end;

function TEditorDividerDrawOptionsFrame.GetTitle: String;
begin
  Result := dlgUseDividerDraw;
end;

procedure TEditorDividerDrawOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  LanguageLabel.Caption := dlgLang;
  DividerSpinEdit.Hint := lis0No1DrawDividerLinesOnlyForTopLevel2DrawLinesForFi;
  DividerSpinLabel.Caption := dlgDividerDrawDepth;
  TopLvlColorLabel.Caption := dlgDividerTopColor;
  TopLvlColorCheckBox.Caption := dlgDividerColorDefault;
  NestLvlColorLabel.Caption := dlgDividerNestColor;
  NestLvlColorCheckBox.Caption := dlgDividerColorDefault;
end;

procedure TEditorDividerDrawOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  i: Integer;
  rd: TEditorOptionsDividerRecord;
begin
  with AOptions as TEditorOptions do
  begin
    with LanguageComboBox.Items do begin
      BeginUpdate;
      for i := 0 to EditorOpts.HighlighterList.Count - 1 do begin
        rd := EditorOptionsDividerDefaults[HighlighterList[i].TheType];
        if (rd.Count > 0) then
          Add(HighlighterList[i].SynClass.GetLanguageName);
      end;
      EndUpdate;
    end;
    LanguageComboBox.ItemIndex := 0;
    LanguageComboBoxExit(LanguageComboBox);
  end;
end;

procedure TEditorDividerDrawOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  i: TLazSyntaxHighlighter;
begin
  with AOptions as TEditorOptions do
  begin
    for i := low(TLazSyntaxHighlighter) to high(TLazSyntaxHighlighter) do begin
      if assigned(FHighlighters[i]) then
         WriteHighlighterDivDrawSettings(FHighlighters[i]);
    end;
  end;
end;

class function TEditorDividerDrawOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorDividerDrawOptionsFrame,
                           EdtOptionsDrawDivider);
end.

