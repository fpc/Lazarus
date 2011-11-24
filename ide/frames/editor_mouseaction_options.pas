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
unit editor_mouseaction_options;

{$mode objfpc}{$H+}

interface

uses
  math, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, SynEdit,
  StdCtrls, ExtCtrls, Classes, LCLProc, editor_mouseaction_options_advanced, Controls, Forms;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    BottomDivider: TBevel;
    chkPredefinedScheme: TCheckBox;
    CtrLLeftLabel: TLabel;
    CtrlLeftRadio1: TRadioButton;
    CtrlLeftRadio2: TRadioButton;
    CtrlLeftRadio3: TRadioButton;
    DiffLabel: TLabel;
    dropUserSchemes: TComboBox;
    GenericDividerLabel: TLabel;
    GenericDividerLeft: TBevel;
    GenericDividerRight: TBevel;
    GutterDividerLabel: TLabel;
    GutterDividerLeft: TBevel;
    GutterDividerRight: TBevel;
    GutterLeftRadio1: TRadioButton;
    GutterLeftRadio2: TRadioButton;
    HideMouseCheckBox: TCheckBox;
    MiddleBtnLabel: TLabel;
    pnlBottom: TPanel;
    PanelGutter: TPanel;
    PanelTextCheckBox: TPanel;
    PanelTextCtrlLeft: TPanel;
    PanelTextMiddle: TPanel;
    pnlAllGutter: TPanel;
    pnlAllText: TPanel;
    pnlUserSchemes: TPanel;
    ResetAllButton: TButton;
    ResetGutterButton: TButton;
    ResetTextButton: TButton;
    RightMoveCaret: TCheckBox;
    ScrollBox1: TScrollBox;
    TextAltMode: TCheckBox;
    TextDividerLabel: TLabel;
    TextDividerLeft: TBevel;
    TextDividerRight: TBevel;
    TextDoubleSelLine: TCheckBox;
    TextDrag: TCheckBox;
    TextMidRadio1: TRadioButton;
    TextMidRadio2: TRadioButton;
    TextMidRadio3: TRadioButton;
    RadioGroup1: TRadioGroup;
    TextLeft: TCheckGroup;
    TextMiddle: TRadioGroup;
    GutterLeft: TRadioGroup;
    WarnLabel: TLabel;
    procedure CheckOrRadioChange(Sender: TObject);
    procedure chkPredefinedSchemeChange(Sender: TObject);
    procedure dropUserSchemesChange(Sender: TObject);
    procedure dropUserSchemesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ResetGutterButtonClick(Sender: TObject);
    procedure ResetTextButtonClick(Sender: TObject);
    procedure ResetAllButtonClick(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    fLoaded: Boolean;
    FOptions: TAbstractIDEOptions;
    FSaved: Boolean;
    FTempMouseSettings: TEditorMouseOptions;
    FInClickHandler: Integer;
    procedure UpdateButtons;
    function  IsUserSchemeChanged: Boolean;
    function  IsTextSettingsChanged: Boolean;
    function  IsGutterSettingsChanged: Boolean;
    procedure SaveUserScheme;
    procedure SaveTextSettings;
    procedure SaveGutterSettings;
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    class function DefaultCollapseChildNodes: Boolean; override;
  end;

implementation

{$R *.lfm}

{ TEditorMouseOptionsFrame }

procedure TEditorMouseOptionsFrame.CheckOrRadioChange(Sender: TObject);
begin
  if FInClickHandler > 0 then exit;
  Inc(FInClickHandler);
  try
    if FTempMouseSettings.IsPresetEqualToMouseActions then begin
      // write settings to conf (and reset conf to settings)
      SaveGutterSettings;
      SaveTextSettings;
    end;
    UpdateButtons;
  finally
    Dec(FInClickHandler);
  end;
end;

procedure TEditorMouseOptionsFrame.UpdateButtons;
begin
    if FTempMouseSettings.IsPresetEqualToMouseActions then begin
      ResetTextButton.Visible   := False;
      ResetGutterButton.Visible := False;
      ResetAllButton.Visible    := False;
      WarnLabel.Visible := False;
      DiffLabel.Visible := False;
      BottomDivider.Visible := False;
    end
    else begin
      ResetTextButton.Visible   := (FTempMouseSettings.SelectedUserScheme = '') and IsTextSettingsChanged;
      ResetGutterButton.Visible := (FTempMouseSettings.SelectedUserScheme = '') and IsGutterSettingsChanged;
      ResetAllButton.Visible    := True; // ResetTextButton.Enabled or ResetGutterButton.Enabled;
      WarnLabel.Visible := (IsTextSettingsChanged or IsGutterSettingsChanged) and
                           ( (FTempMouseSettings.SelectedUserScheme = '') or
                             IsUserSchemeChanged );
      DiffLabel.Visible := (not WarnLabel.Visible);
      BottomDivider.Visible := True;
    end;
end;

procedure TEditorMouseOptionsFrame.dropUserSchemesChange(Sender: TObject);
begin
  if Sender <> nil then begin;
    chkPredefinedScheme.Checked := dropUserSchemes.ItemIndex > 0;
    if dropUserSchemes.ItemIndex > 0 then
      dropUserSchemes.tag := dropUserSchemes.ItemIndex;
  end;
  pnlAllGutter.Enabled := dropUserSchemes.ItemIndex = 0;
  pnlAllText.Enabled   := dropUserSchemes.ItemIndex = 0;
  if FTempMouseSettings.IsPresetEqualToMouseActions then
    SaveUserScheme;
  UpdateButtons;
end;

procedure TEditorMouseOptionsFrame.dropUserSchemesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  dropUserSchemesChange(Sender);
end;

procedure TEditorMouseOptionsFrame.chkPredefinedSchemeChange(Sender: TObject);
begin
  if chkPredefinedScheme.Checked then
    dropUserSchemes.ItemIndex := Max(dropUserSchemes.Tag, 1)
  else
    dropUserSchemes.ItemIndex := 0;
  dropUserSchemesChange(nil);
end;

procedure TEditorMouseOptionsFrame.ResetGutterButtonClick(Sender: TObject);
begin
  SaveGutterSettings;
  UpdateButtons;
end;

procedure TEditorMouseOptionsFrame.ResetTextButtonClick(Sender: TObject);
begin
  SaveTextSettings;
  UpdateButtons;
end;

procedure TEditorMouseOptionsFrame.ResetAllButtonClick(Sender: TObject);
begin
  SaveGutterSettings;
  SaveTextSettings;
  SaveUserScheme; // must be last
  UpdateButtons;
end;

function TEditorMouseOptionsFrame.IsUserSchemeChanged: Boolean;
begin
  Result := FTempMouseSettings.SelectedUserSchemeIndex <>
    PtrInt(dropUserSchemes.Items.Objects[dropUserSchemes.ItemIndex]);
end;

function TEditorMouseOptionsFrame.IsTextSettingsChanged: Boolean;
begin
  Result := not (
    (FTempMouseSettings.AltColumnMode = TextAltMode.Checked) and
    (FTempMouseSettings.TextDrag = TextDrag.Checked) and
    (FTempMouseSettings.TextRightMoveCaret = RightMoveCaret.Checked) and
    (FTempMouseSettings.TextDoubleSelLine = TextDoubleSelLine.Checked) and
    ( (TextMidRadio1.Checked and (FTempMouseSettings.TextMiddleClick = moTMPaste)) or
      (TextMidRadio2.Checked and (FTempMouseSettings.TextMiddleClick = moTMIgnore)) or
      (TextMidRadio3.Checked and (FTempMouseSettings.TextMiddleClick = moTMDeclarationJump))
    ) and
    ( (CtrlLeftRadio1.Checked and (FTempMouseSettings.TextCtrlLeftClick = moTCLJump)) or
      (CtrlLeftRadio2.Checked and (FTempMouseSettings.TextCtrlLeftClick = moTCLNone)) or
      (CtrlLeftRadio3.Checked and (FTempMouseSettings.TextCtrlLeftClick = moTCLJumpOrBlock))
    )
  );
end;

function TEditorMouseOptionsFrame.IsGutterSettingsChanged: Boolean;
begin
  Result := not (
    ( (GutterLeftRadio1.Checked and (FTempMouseSettings.GutterLeft = moGLDownClick)) or
      (GutterLeftRadio2.Checked and (FTempMouseSettings.GutterLeft = moglUpClickAndSelect))
    )
  );
end;

procedure TEditorMouseOptionsFrame.SaveUserScheme;
var
  i: Integer;
begin
  i := PtrInt(dropUserSchemes.Items.Objects[dropUserSchemes.ItemIndex]);
  FTempMouseSettings.SelectedUserSchemeIndex := i;
  if i >= 0 then
    FTempMouseSettings.ResetToUserScheme
  else begin
    FTempMouseSettings.ResetTextToDefault;
    FTempMouseSettings.ResetGutterToDefault;
  end;
  if FDialog.FindEditor(TEditorMouseOptionsAdvFrame) <> nil then
    TEditorMouseOptionsAdvFrame(FDialog.FindEditor(TEditorMouseOptionsAdvFrame)).RefreshSettings;
end;

procedure TEditorMouseOptionsFrame.SaveTextSettings;
begin
  FTempMouseSettings.AltColumnMode := TextAltMode.Checked;
  FTempMouseSettings.TextDrag := TextDrag.Checked;
  FTempMouseSettings.TextRightMoveCaret := RightMoveCaret.Checked;
  FTempMouseSettings.TextDoubleSelLine := TextDoubleSelLine.Checked;

  if TextMidRadio3.Checked then
    FTempMouseSettings.TextMiddleClick := moTMDeclarationJump
  else if TextMidRadio2.Checked then
    FTempMouseSettings.TextMiddleClick := moTMIgnore
  else
    FTempMouseSettings.TextMiddleClick := moTMPaste;

  if CtrlLeftRadio3.Checked then
    FTempMouseSettings.TextCtrlLeftClick := moTCLJumpOrBlock
  else if CtrlLeftRadio2.Checked then
    FTempMouseSettings.TextCtrlLeftClick := moTCLNone
  else
    FTempMouseSettings.TextCtrlLeftClick := moTCLJump;

  FTempMouseSettings.ResetTextToDefault;
  if FDialog.FindEditor(TEditorMouseOptionsAdvFrame) <> nil then
    TEditorMouseOptionsAdvFrame(FDialog.FindEditor(TEditorMouseOptionsAdvFrame)).RefreshSettings;
end;

procedure TEditorMouseOptionsFrame.SaveGutterSettings;
begin
  if GutterLeftRadio2.Checked then
    FTempMouseSettings.GutterLeft := moglUpClickAndSelect
  else
    FTempMouseSettings.GutterLeft := moGLDownClick;
  FTempMouseSettings.ResetGutterToDefault;
  if FDialog.FindEditor(TEditorMouseOptionsAdvFrame) <> nil then
    TEditorMouseOptionsAdvFrame(FDialog.FindEditor(TEditorMouseOptionsAdvFrame)).RefreshSettings;
end;

procedure TEditorMouseOptionsFrame.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Value and (FTempMouseSettings <> nil) then
    UpdateButtons;
end;

function TEditorMouseOptionsFrame.GetTitle: String;
begin
  Result := dlgMouseOptions;
end;

procedure TEditorMouseOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;
  chkPredefinedScheme.Caption := dlfMousePredefinedScheme;
  GenericDividerLabel.Caption := dlfMouseSimpleGenericSect;
  GutterDividerLabel.Caption := dlfMouseSimpleGutterSect;
  GutterLeftRadio1.Caption := dlfMouseSimpleGutterLeftDown;
  GutterLeftRadio2.Caption := dlfMouseSimpleGutterLeftUp;
  TextDividerLabel.Caption := dlfMouseSimpleTextSect;
  TextAltMode.Caption := dlfMouseSimpleTextSectAlt;
  TextDrag.Caption := dlfMouseSimpleTextSectDrag;
  TextDoubleSelLine.Caption := dlfMouseSimpleTextSectDoubleSelLine;
  RightMoveCaret.Caption := dlfMouseSimpleRightMoveCaret;
  MiddleBtnLabel.Caption := dlfMouseSimpleTextSectMidLabel;
  TextMidRadio1.Caption := dlfMouseSimpleTextSectMidPaste;
  TextMidRadio2.Caption := dlfMouseSimpleTextSectMidNone;
  TextMidRadio3.Caption := dlfMouseSimpleTextSectMidGoto;
  CtrLLeftLabel.Caption := dlfMouseSimpleTextSectCtrlLeftLabel;
  CtrlLeftRadio1.Caption := dlfMouseSimpleTextSectCtrlLeftRJump;
  CtrlLeftRadio2.Caption := dlfMouseSimpleTextSectCtrlLeftRNone;
  CtrlLeftRadio3.Caption := dlfMouseSimpleTextSectCtrlLeftRJumpOrBlock;
  WarnLabel.Caption := dlfMouseSimpleWarning;
  DiffLabel.Caption := dlfMouseSimpleDiff;
  ResetAllButton.Caption := dlfMouseResetAll;
  ResetGutterButton.Caption := dlfMouseResetGutter;
  ResetTextButton.Caption := dlfMouseResetText;
  HideMouseCheckBox.Caption := dlgAutoHideCursor;
end;

procedure TEditorMouseOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  if fLoaded then exit;
  fLoaded:=true;
  Inc(FInClickHandler);
  FOptions := AOptions;
  FTempMouseSettings := TEditorOptions(AOptions).TempMouseSettings;
  FTempMouseSettings.Assign(TEditorOptions(AOptions).UserMouseSettings);

  pnlUserSchemes.Visible := FTempMouseSettings.UserSchemeCount > 0;
  dropUserSchemes.Clear;
  for i := 0 to FTempMouseSettings.UserSchemeCount - 1 do begin
    dropUserSchemes.Items.AddObject(FTempMouseSettings.UserSchemeNames[i],
                                    TObject(PtrUInt(i)) );
  end;
  dropUserSchemes.Sorted := True;
  dropUserSchemes.Sorted := False;
  dropUserSchemes.Items.InsertObject(0, dlfNoPredefinedScheme, TObject(PtrUInt(-1)));
  dropUserSchemes.ItemIndex := dropUserSchemes.Items.IndexOfObject
    ( TObject(PtrUInt(FTempMouseSettings.SelectedUserSchemeIndex)) );
  dropUserSchemesChange(Self);

  case FTempMouseSettings.GutterLeft of
    moGLDownClick: GutterLeftRadio1.Checked := True;
    moglUpClickAndSelect: GutterLeftRadio2.Checked := True;
  end;
  TextAltMode.Checked := FTempMouseSettings.AltColumnMode;
  TextDrag.Checked    := FTempMouseSettings.TextDrag;
  RightMoveCaret.Checked := FTempMouseSettings.TextRightMoveCaret;
  TextDoubleSelLine.Checked := FTempMouseSettings.TextDoubleSelLine;
  case FTempMouseSettings.TextMiddleClick of
    moTMPaste: TextMidRadio1.Checked := True;
    moTMIgnore: TextMidRadio2.Checked := True;
    moTMDeclarationJump: TextMidRadio3.Checked := True;
  end;
  case FTempMouseSettings.TextCtrlLeftClick of
    moTCLJump: CtrlLeftRadio1.Checked := True;
    moTCLNone: CtrlLeftRadio2.Checked := True;
    moTCLJumpOrBlock: CtrlLeftRadio3.Checked := True;
  end;
  Dec(FInClickHandler);
  UpdateButtons;

  HideMouseCheckBox.Checked := eoAutoHideCursor in TEditorOptions(AOptions).SynEditOptions2;
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
  TEditorOptions(AOptions).UserMouseSettings.Assign(FTempMouseSettings);
  with TEditorOptions(AOptions) do begin
    if HideMouseCheckBox.Checked then
      SynEditOptions2 := SynEditOptions2 + [eoAutoHideCursor]
    else
      SynEditOptions2 := SynEditOptions2 - [eoAutoHideCursor]
  end;
end;

class function TEditorMouseOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

class function TEditorMouseOptionsFrame.DefaultCollapseChildNodes: Boolean;
begin
  Result := True;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorMouseOptionsFrame, EdtOptionsMouse);
end.

