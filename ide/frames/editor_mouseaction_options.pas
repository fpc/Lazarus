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
unit editor_mouseaction_options;

{$mode objfpc}{$H+}

interface

uses
  sysutils, math,
  // LCL
  LCLProc, StdCtrls, ExtCtrls, Classes, Forms, ComCtrls,
  // LazControls
  DividerBevel,
  // SynEdit
  SynEdit,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  EditorOptions, LazarusIDEStrConsts, editor_mouseaction_options_advanced;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkGutterTextLines: TCheckBox;
    dropWheelHoriz: TComboBox;
    dropWheelAltHoriz: TComboBox;
    dropWheelAltCtrlHoriz: TComboBox;
    dropWheelCtrlHoriz: TComboBox;
    dropWheelShiftHoriz: TComboBox;
    dropWheelShiftAltHoriz: TComboBox;
    dropWheelShiftAltCtrlHoriz: TComboBox;
    dropWheelShiftCtrlHoriz: TComboBox;
    GutterLeftRadio3: TRadioButton;
    lblWheel: TLabel;
    lblWheelHoriz: TLabel;
    lblWheelAlt: TLabel;
    lblWheelAltHoriz: TLabel;
    lblWheelAltCtrlHoriz: TLabel;
    lblWheelCtrl: TLabel;
    lblWheelCtrlHoriz: TLabel;
    lblWheelShift: TLabel;
    lblWheelAltCtrl: TLabel;
    lblWheelShiftHoriz: TLabel;
    lblWheelShiftAlt: TLabel;
    lblWheelShiftAltHoriz: TLabel;
    lblWheelShiftAltCtrlHoriz: TLabel;
    lblWheelShiftCtrl: TLabel;
    lblWheelShiftAltCtrl: TLabel;
    dropWheel: TComboBox;
    dropWheelAlt: TComboBox;
    dropWheelCtrl: TComboBox;
    dropWheelShift: TComboBox;
    dropWheelAltCtrl: TComboBox;
    dropWheelShiftAlt: TComboBox;
    dropWheelShiftCtrl: TComboBox;
    dropWheelShiftAltCtrl: TComboBox;
    lblMiddle: TLabel;
    lblMiddleAlt: TLabel;
    lblMiddleCtrl: TLabel;
    lblMiddleShift: TLabel;
    lblMiddleAltCtrl: TLabel;
    lblMiddleShiftAlt: TLabel;
    lblMiddleShiftCtrl: TLabel;
    lblMiddleShiftAltCtrl: TLabel;
    dropMiddle: TComboBox;
    dropMiddleAlt: TComboBox;
    dropMiddleCtrl: TComboBox;
    dropMiddleShift: TComboBox;
    dropMiddleAltCtrl: TComboBox;
    dropMiddleShiftAlt: TComboBox;
    dropMiddleShiftCtrl: TComboBox;
    dropMiddleShiftAltCtrl: TComboBox;
    lblRight: TLabel;
    lblRightAlt: TLabel;
    lblRightCtrl: TLabel;
    lblRightShift: TLabel;
    lblRightAltCtrl: TLabel;
    lblRightShiftAlt: TLabel;
    lblRightShiftCtrl: TLabel;
    lblRightShiftAltCtrl: TLabel;
    dropRight: TComboBox;
    dropRightAlt: TComboBox;
    dropRightCtrl: TComboBox;
    dropRightShift: TComboBox;
    dropRightAltCtrl: TComboBox;
    dropRightShiftAlt: TComboBox;
    dropRightShiftCtrl: TComboBox;
    dropRightShiftAltCtrl: TComboBox;
    lblExtra1: TLabel;
    lblExtra1Alt: TLabel;
    lblExtra1Ctrl: TLabel;
    lblExtra1Shift: TLabel;
    lblExtra1AltCtrl: TLabel;
    lblExtra1ShiftAlt: TLabel;
    lblExtra1ShiftCtrl: TLabel;
    lblExtra1ShiftAltCtrl: TLabel;
    dropExtra1: TComboBox;
    dropExtra1Alt: TComboBox;
    dropExtra1Ctrl: TComboBox;
    dropExtra1Shift: TComboBox;
    dropExtra1AltCtrl: TComboBox;
    dropExtra1ShiftAlt: TComboBox;
    dropExtra1ShiftCtrl: TComboBox;
    dropExtra1ShiftAltCtrl: TComboBox;
    lblExtra2: TLabel;
    lblExtra2Alt: TLabel;
    lblExtra2Ctrl: TLabel;
    lblExtra2Shift: TLabel;
    lblExtra2AltCtrl: TLabel;
    lblExtra2ShiftAlt: TLabel;
    lblExtra2ShiftCtrl: TLabel;
    lblExtra2ShiftAltCtrl: TLabel;
    dropExtra2: TComboBox;
    dropExtra2Alt: TComboBox;
    dropExtra2Ctrl: TComboBox;
    dropExtra2Shift: TComboBox;
    dropExtra2AltCtrl: TComboBox;
    dropExtra2ShiftAlt: TComboBox;
    dropExtra2ShiftCtrl: TComboBox;
    dropExtra2ShiftAltCtrl: TComboBox;
    lblLeftDouble: TLabel;
    lblLeftTriple: TLabel;
    lblLeftQuad: TLabel;
    lblLeftDoubleShift: TLabel;
    lblLeftDoubleAlt: TLabel;
    lblLeftDoubleCtrl: TLabel;
    dropLeftDouble: TComboBox;
    dropLeftTriple: TComboBox;
    dropLeftQuad: TComboBox;
    dropLeftShiftDouble: TComboBox;
    dropLeftAltDouble: TComboBox;
    dropLeftCtrlDouble: TComboBox;
    lblWheelShiftCtrlHoriz: TLabel;
    PageHorizWheel: TPage;
    PageExtra2: TPage;
    PageExtra1: TPage;
    PageRight: TPage;
    ScrollBoxExtra2: TScrollBox;
    ScrollBoxExtra1: TScrollBox;
    ScrollBoxRight: TScrollBox;
    ScrollBoxWheelHoriz: TScrollBox;
    ShiftLeftLabel: TLabel;
    AltCtrlLeftLabel: TLabel;
    CtrLLeftLabel: TLabel;
    ShiftCtrlLeftLabel: TLabel;
    ShiftAltLeftLabel: TLabel;
    ShiftAltCtrlLeftLabel: TLabel;
    BottomDivider: TBevel;
    chkPredefinedScheme: TCheckBox;
    AltLeftLabel: TLabel;
    dropAltLeft: TComboBox;
    dropAltCtrlLeft: TComboBox;
    dropShiftAltLeft: TComboBox;
    dropShiftCtrlLeft: TComboBox;
    dropShiftAltCtrlLeft: TComboBox;
    dropShiftLeft: TComboBox;
    PageLeftDbl: TPage;
    PageLeftMod: TPage;
    ScrollBoxMiddle: TScrollBox;
    ScrollBoxWheel: TScrollBox;
    ScrollBoxLeftMod: TScrollBox;
    ScrollBoxLeftDbl: TScrollBox;
    TextDividerLabel: TDividerBevel;
    GutterDividerLabel: TDividerBevel;
    GenericDividerLabel: TDividerBevel;
    dropCtrlLeft: TComboBox;
    DiffLabel: TLabel;
    dropUserSchemes: TComboBox;
    GutterLeftRadio1: TRadioButton;
    GutterLeftRadio2: TRadioButton;
    HideMouseCheckBox: TCheckBox;
    Notebook1: TNotebook;
    PageMiddle: TPage;
    PageWheel: TPage;
    ToolBar1: TToolBar;
    ToolBtnRight: TToolButton;
    ToolBtnExtra1: TToolButton;
    ToolBtnExtra2: TToolButton;
    ToolBtnMiddle: TToolButton;
    ToolBtnWheel: TToolButton;
    ToolBtnLeftMod: TToolButton;
    ToolBtnLeftMulti: TToolButton;
    pnlBottom: TPanel;
    PanelGutter: TPanel;
    PanelTextCheckBox: TPanel;
    pnlAllGutter: TPanel;
    pnlAllText: TPanel;
    pnlUserSchemes: TPanel;
    ResetAllButton: TButton;
    ResetGutterButton: TButton;
    ResetTextButton: TButton;
    RightMoveCaret: TCheckBox;
    TextDrag: TCheckBox;
    RadioGroup1: TRadioGroup;
    TextLeft: TCheckGroup;
    TextMiddle: TRadioGroup;
    GutterLeft: TRadioGroup;
    ToolBtnHorizWheel: TToolButton;
    WarnLabel: TLabel;
    procedure CheckOrRadioChange(Sender: TObject);
    procedure chkPredefinedSchemeChange(Sender: TObject);
    procedure dropUserSchemesChange(Sender: TObject);
    procedure dropUserSchemesKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure ResetGutterButtonClick(Sender: TObject);
    procedure ResetTextButtonClick(Sender: TObject);
    procedure ResetAllButtonClick(Sender: TObject);
    procedure ToolBtnMiddleClick(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FOptions: TAbstractIDEOptions;
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
    function IdxToDoubleMouseOptButtonAction(AIdx: integer): TMouseOptButtonAction;
    procedure CheckForShiftChange(Sender: TObject);
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
  if Sender <> nil then
    CheckForShiftChange(Sender);
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

procedure TEditorMouseOptionsFrame.ToolBtnMiddleClick(Sender: TObject);
begin
  if not(Sender is TToolButton) then exit;
  Notebook1.PageIndex := TToolButton(Sender).Tag;
end;

function TEditorMouseOptionsFrame.IsUserSchemeChanged: Boolean;
begin
  Result := FTempMouseSettings.SelectedUserSchemeIndex <>
    PtrInt(dropUserSchemes.Items.Objects[dropUserSchemes.ItemIndex]);
end;

function TEditorMouseOptionsFrame.IsTextSettingsChanged: Boolean;
begin
  Result := not (
    (FTempMouseSettings.TextDrag      = TextDrag.Checked) and
    (FTempMouseSettings.TextRightMoveCaret = RightMoveCaret.Checked) and

    (FTempMouseSettings.TextAltLeftClick          = TMouseOptButtonAction(dropAltLeft.ItemIndex)) and
    (FTempMouseSettings.TextCtrlLeftClick         = TMouseOptButtonAction(dropCtrlLeft.ItemIndex)) and
    (FTempMouseSettings.TextAltCtrlLeftClick      = TMouseOptButtonAction(dropAltCtrlLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftLeftClick        = TMouseOptButtonAction(dropShiftLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltLeftClick     = TMouseOptButtonAction(dropShiftAltLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlLeftClick    = TMouseOptButtonAction(dropShiftCtrlLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltCtrlLeftClick = TMouseOptButtonAction(dropShiftAltCtrlLeft.ItemIndex)) and

    (FTempMouseSettings.TextDoubleLeftClick      = IdxToDoubleMouseOptButtonAction(dropLeftDouble.ItemIndex)) and
    (FTempMouseSettings.TextTripleLeftClick      = IdxToDoubleMouseOptButtonAction(dropLeftTriple.ItemIndex)) and
    (FTempMouseSettings.TextQuadLeftClick        = IdxToDoubleMouseOptButtonAction(dropLeftQuad.ItemIndex)) and
    (FTempMouseSettings.TextShiftDoubleLeftClick = IdxToDoubleMouseOptButtonAction(dropLeftShiftDouble.ItemIndex)) and
    (FTempMouseSettings.TextAltDoubleLeftClick   = IdxToDoubleMouseOptButtonAction(dropLeftAltDouble.ItemIndex)) and
    (FTempMouseSettings.TextCtrlDoubleLeftClick  = IdxToDoubleMouseOptButtonAction(dropLeftCtrlDouble.ItemIndex)) and

    (FTempMouseSettings.TextMiddleClick      = TMouseOptButtonAction(dropMiddle.ItemIndex)) and
    (FTempMouseSettings.TextShiftMiddleClick = TMouseOptButtonAction(dropMiddleShift.ItemIndex)) and
    (FTempMouseSettings.TextAltMiddleClick   = TMouseOptButtonAction(dropMiddleAlt.ItemIndex)) and
    (FTempMouseSettings.TextCtrlMiddleClick  = TMouseOptButtonAction(dropMiddleCtrl.ItemIndex)) and
    (FTempMouseSettings.TextAltCtrlMiddleClick      = TMouseOptButtonAction(dropMiddleAltCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltMiddleClick     = TMouseOptButtonAction(dropMiddleShiftAlt.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlMiddleClick    = TMouseOptButtonAction(dropMiddleShiftCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltCtrlMiddleClick = TMouseOptButtonAction(dropMiddleShiftAltCtrl.ItemIndex)) and

    (FTempMouseSettings.TextRightClick      = TMouseOptButtonAction(dropRight.ItemIndex)) and
    (FTempMouseSettings.TextShiftRightClick = TMouseOptButtonAction(dropRightShift.ItemIndex)) and
    (FTempMouseSettings.TextAltRightClick   = TMouseOptButtonAction(dropRightAlt.ItemIndex)) and
    (FTempMouseSettings.TextCtrlRightClick  = TMouseOptButtonAction(dropRightCtrl.ItemIndex)) and
    (FTempMouseSettings.TextAltCtrlRightClick      = TMouseOptButtonAction(dropRightAltCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltRightClick     = TMouseOptButtonAction(dropRightShiftAlt.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlRightClick    = TMouseOptButtonAction(dropRightShiftCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltCtrlRightClick = TMouseOptButtonAction(dropRightShiftAltCtrl.ItemIndex)) and

    (FTempMouseSettings.TextExtra1Click      = TMouseOptButtonAction(dropExtra1.ItemIndex)) and
    (FTempMouseSettings.TextShiftExtra1Click = TMouseOptButtonAction(dropExtra1Shift.ItemIndex)) and
    (FTempMouseSettings.TextAltExtra1Click   = TMouseOptButtonAction(dropExtra1Alt.ItemIndex)) and
    (FTempMouseSettings.TextCtrlExtra1Click  = TMouseOptButtonAction(dropExtra1Ctrl.ItemIndex)) and
    (FTempMouseSettings.TextAltCtrlExtra1Click      = TMouseOptButtonAction(dropExtra1AltCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltExtra1Click     = TMouseOptButtonAction(dropExtra1ShiftAlt.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlExtra1Click    = TMouseOptButtonAction(dropExtra1ShiftCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltCtrlExtra1Click = TMouseOptButtonAction(dropExtra1ShiftAltCtrl.ItemIndex)) and

    (FTempMouseSettings.TextExtra2Click      = TMouseOptButtonAction(dropExtra2.ItemIndex)) and
    (FTempMouseSettings.TextShiftExtra2Click = TMouseOptButtonAction(dropExtra2Shift.ItemIndex)) and
    (FTempMouseSettings.TextAltExtra2Click   = TMouseOptButtonAction(dropExtra2Alt.ItemIndex)) and
    (FTempMouseSettings.TextCtrlExtra2Click  = TMouseOptButtonAction(dropExtra2Ctrl.ItemIndex)) and
    (FTempMouseSettings.TextAltCtrlExtra2Click      = TMouseOptButtonAction(dropExtra2AltCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltExtra2Click     = TMouseOptButtonAction(dropExtra2ShiftAlt.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlExtra2Click    = TMouseOptButtonAction(dropExtra2ShiftCtrl.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltCtrlExtra2Click = TMouseOptButtonAction(dropExtra2ShiftAltCtrl.ItemIndex)) and

    (FTempMouseSettings.Wheel      = TMouseOptWheelAction(dropWheel.ItemIndex)) and
    (FTempMouseSettings.CtrlWheel  = TMouseOptWheelAction(dropWheelCtrl.ItemIndex)) and
    (FTempMouseSettings.AltWheel   = TMouseOptWheelAction(dropWheelAlt.ItemIndex)) and
    (FTempMouseSettings.ShiftWheel = TMouseOptWheelAction(dropWheelShift.ItemIndex)) and
    (FTempMouseSettings.AltCtrlWheel        = TMouseOptWheelAction(dropWheelAltCtrl.ItemIndex)) and
    (FTempMouseSettings.ShiftAltWheel       = TMouseOptWheelAction(dropWheelShiftAlt.ItemIndex)) and
    (FTempMouseSettings.ShiftCtrlWheel      = TMouseOptWheelAction(dropWheelShiftCtrl.ItemIndex)) and
    (FTempMouseSettings.ShiftAltCtrlWheel   = TMouseOptWheelAction(dropWheelShiftAltCtrl.ItemIndex)) and

    (FTempMouseSettings.HorizWheel      = TMouseOptWheelAction(dropWheelHoriz.ItemIndex)) and
    (FTempMouseSettings.CtrlHorizWheel  = TMouseOptWheelAction(dropWheelCtrlHoriz.ItemIndex)) and
    (FTempMouseSettings.AltHorizWheel   = TMouseOptWheelAction(dropWheelAltHoriz.ItemIndex)) and
    (FTempMouseSettings.ShiftHorizWheel = TMouseOptWheelAction(dropWheelShiftHoriz.ItemIndex)) and
    (FTempMouseSettings.AltCtrlHorizWheel        = TMouseOptWheelAction(dropWheelAltCtrlHoriz.ItemIndex)) and
    (FTempMouseSettings.ShiftAltHorizWheel       = TMouseOptWheelAction(dropWheelShiftAltHoriz.ItemIndex)) and
    (FTempMouseSettings.ShiftCtrlHorizWheel      = TMouseOptWheelAction(dropWheelShiftCtrlHoriz.ItemIndex)) and
    (FTempMouseSettings.ShiftAltCtrlHorizWheel   = TMouseOptWheelAction(dropWheelShiftAltCtrlHoriz.ItemIndex))
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
  FTempMouseSettings.TextDrag := TextDrag.Checked;
  FTempMouseSettings.TextRightMoveCaret := RightMoveCaret.Checked;

  FTempMouseSettings.TextAltLeftClick          := TMouseOptButtonAction(dropAltLeft.ItemIndex);
  FTempMouseSettings.TextCtrlLeftClick         := TMouseOptButtonAction(dropCtrlLeft.ItemIndex);
  FTempMouseSettings.TextAltCtrlLeftClick      := TMouseOptButtonAction(dropAltCtrlLeft.ItemIndex);
  FTempMouseSettings.TextShiftLeftClick        := TMouseOptButtonAction(dropShiftLeft.ItemIndex);
  FTempMouseSettings.TextShiftAltLeftClick     := TMouseOptButtonAction(dropShiftAltLeft.ItemIndex);
  FTempMouseSettings.TextShiftCtrlLeftClick    := TMouseOptButtonAction(dropShiftCtrlLeft.ItemIndex);
  FTempMouseSettings.TextShiftAltCtrlLeftClick := TMouseOptButtonAction(dropShiftAltCtrlLeft.ItemIndex);

  FTempMouseSettings.TextDoubleLeftClick      := IdxToDoubleMouseOptButtonAction(dropLeftDouble.ItemIndex);
  FTempMouseSettings.TextTripleLeftClick      := IdxToDoubleMouseOptButtonAction(dropLeftTriple.ItemIndex);
  FTempMouseSettings.TextQuadLeftClick        := IdxToDoubleMouseOptButtonAction(dropLeftQuad.ItemIndex);
  FTempMouseSettings.TextShiftDoubleLeftClick := IdxToDoubleMouseOptButtonAction(dropLeftShiftDouble.ItemIndex);
  FTempMouseSettings.TextAltDoubleLeftClick   := IdxToDoubleMouseOptButtonAction(dropLeftAltDouble.ItemIndex);
  FTempMouseSettings.TextCtrlDoubleLeftClick  := IdxToDoubleMouseOptButtonAction(dropLeftCtrlDouble.ItemIndex);

  FTempMouseSettings.TextMiddleClick      := TMouseOptButtonAction(dropMiddle.ItemIndex);
  FTempMouseSettings.TextShiftMiddleClick := TMouseOptButtonAction(dropMiddleShift.ItemIndex);
  FTempMouseSettings.TextAltMiddleClick   := TMouseOptButtonAction(dropMiddleAlt.ItemIndex);
  FTempMouseSettings.TextCtrlMiddleClick  := TMouseOptButtonAction(dropMiddleCtrl.ItemIndex);
  FTempMouseSettings.TextAltCtrlMiddleClick      := TMouseOptButtonAction(dropMiddleAltCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltMiddleClick     := TMouseOptButtonAction(dropMiddleShiftAlt.ItemIndex);
  FTempMouseSettings.TextShiftCtrlMiddleClick    := TMouseOptButtonAction(dropMiddleShiftCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltCtrlMiddleClick := TMouseOptButtonAction(dropMiddleShiftAltCtrl.ItemIndex);

  FTempMouseSettings.TextRightClick      := TMouseOptButtonAction(dropRight.ItemIndex);
  FTempMouseSettings.TextShiftRightClick := TMouseOptButtonAction(dropRightShift.ItemIndex);
  FTempMouseSettings.TextAltRightClick   := TMouseOptButtonAction(dropRightAlt.ItemIndex);
  FTempMouseSettings.TextCtrlRightClick  := TMouseOptButtonAction(dropRightCtrl.ItemIndex);
  FTempMouseSettings.TextAltCtrlRightClick      := TMouseOptButtonAction(dropRightAltCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltRightClick     := TMouseOptButtonAction(dropRightShiftAlt.ItemIndex);
  FTempMouseSettings.TextShiftCtrlRightClick    := TMouseOptButtonAction(dropRightShiftCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltCtrlRightClick := TMouseOptButtonAction(dropRightShiftAltCtrl.ItemIndex);

  FTempMouseSettings.TextExtra1Click      := TMouseOptButtonAction(dropExtra1.ItemIndex);
  FTempMouseSettings.TextShiftExtra1Click := TMouseOptButtonAction(dropExtra1Shift.ItemIndex);
  FTempMouseSettings.TextAltExtra1Click   := TMouseOptButtonAction(dropExtra1Alt.ItemIndex);
  FTempMouseSettings.TextCtrlExtra1Click  := TMouseOptButtonAction(dropExtra1Ctrl.ItemIndex);
  FTempMouseSettings.TextAltCtrlExtra1Click      := TMouseOptButtonAction(dropExtra1AltCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltExtra1Click     := TMouseOptButtonAction(dropExtra1ShiftAlt.ItemIndex);
  FTempMouseSettings.TextShiftCtrlExtra1Click    := TMouseOptButtonAction(dropExtra1ShiftCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltCtrlExtra1Click := TMouseOptButtonAction(dropExtra1ShiftAltCtrl.ItemIndex);

  FTempMouseSettings.TextExtra2Click      := TMouseOptButtonAction(dropExtra2.ItemIndex);
  FTempMouseSettings.TextShiftExtra2Click := TMouseOptButtonAction(dropExtra2Shift.ItemIndex);
  FTempMouseSettings.TextAltExtra2Click   := TMouseOptButtonAction(dropExtra2Alt.ItemIndex);
  FTempMouseSettings.TextCtrlExtra2Click  := TMouseOptButtonAction(dropExtra2Ctrl.ItemIndex);
  FTempMouseSettings.TextAltCtrlExtra2Click      := TMouseOptButtonAction(dropExtra2AltCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltExtra2Click     := TMouseOptButtonAction(dropExtra2ShiftAlt.ItemIndex);
  FTempMouseSettings.TextShiftCtrlExtra2Click    := TMouseOptButtonAction(dropExtra2ShiftCtrl.ItemIndex);
  FTempMouseSettings.TextShiftAltCtrlExtra2Click := TMouseOptButtonAction(dropExtra2ShiftAltCtrl.ItemIndex);

  FTempMouseSettings.Wheel               := TMouseOptWheelAction(dropWheel.ItemIndex);
  FTempMouseSettings.CtrlWheel           := TMouseOptWheelAction(dropWheelCtrl.ItemIndex);
  FTempMouseSettings.AltWheel            := TMouseOptWheelAction(dropWheelAlt.ItemIndex);
  FTempMouseSettings.ShiftWheel          := TMouseOptWheelAction(dropWheelShift.ItemIndex);
  FTempMouseSettings.AltCtrlWheel        := TMouseOptWheelAction(dropWheelAltCtrl.ItemIndex);
  FTempMouseSettings.ShiftAltWheel       := TMouseOptWheelAction(dropWheelShiftAlt.ItemIndex);
  FTempMouseSettings.ShiftCtrlWheel      := TMouseOptWheelAction(dropWheelShiftCtrl.ItemIndex);
  FTempMouseSettings.ShiftAltCtrlWheel   := TMouseOptWheelAction(dropWheelShiftAltCtrl.ItemIndex);

  FTempMouseSettings.HorizWheel               := TMouseOptWheelAction(dropWheelHoriz.ItemIndex);
  FTempMouseSettings.CtrlHorizWheel           := TMouseOptWheelAction(dropWheelCtrlHoriz.ItemIndex);
  FTempMouseSettings.AltHorizWheel            := TMouseOptWheelAction(dropWheelAltHoriz.ItemIndex);
  FTempMouseSettings.ShiftHorizWheel          := TMouseOptWheelAction(dropWheelShiftHoriz.ItemIndex);
  FTempMouseSettings.AltCtrlHorizWheel        := TMouseOptWheelAction(dropWheelAltCtrlHoriz.ItemIndex);
  FTempMouseSettings.ShiftAltHorizWheel       := TMouseOptWheelAction(dropWheelShiftAltHoriz.ItemIndex);
  FTempMouseSettings.ShiftCtrlHorizWheel      := TMouseOptWheelAction(dropWheelShiftCtrlHoriz.ItemIndex);
  FTempMouseSettings.ShiftAltCtrlHorizWheel   := TMouseOptWheelAction(dropWheelShiftAltCtrlHoriz.ItemIndex);

  FTempMouseSettings.ResetTextToDefault;
  if FDialog.FindEditor(TEditorMouseOptionsAdvFrame) <> nil then
    TEditorMouseOptionsAdvFrame(FDialog.FindEditor(TEditorMouseOptionsAdvFrame)).RefreshSettings;
end;

procedure TEditorMouseOptionsFrame.SaveGutterSettings;
begin
  if GutterLeftRadio2.Checked then
    FTempMouseSettings.GutterLeft := moglUpClickAndSelect
  else
  if GutterLeftRadio3.Checked then
    FTempMouseSettings.GutterLeft := moglUpClickAndSelectRighHalf
  else
    FTempMouseSettings.GutterLeft := moGLDownClick;
  FTempMouseSettings.SelectOnLineNumbers := chkGutterTextLines.Checked;
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

function TEditorMouseOptionsFrame.IdxToDoubleMouseOptButtonAction(AIdx: integer): TMouseOptButtonAction;
begin
  if AIdx <> 0 then AIdx := AIdx + 3;
  Result := TMouseOptButtonAction(AIdx);
end;

procedure TEditorMouseOptionsFrame.CheckForShiftChange(Sender: TObject);
begin
  if (Sender = nil) then begin
    dropShiftLeft.Items[0] := Format(dlfMouseSimpleButtonSelContinuePlain, [dlfMouseSimpleButtonSelect]);
  end;
  if (Sender = dropAltLeft) or (Sender = nil) then begin
    if TMouseOptButtonAction(dropAltLeft.ItemIndex) in [mbaSelect..mbaSelectLine]
    then dropShiftAltLeft.Items[0] := Format(dlfMouseSimpleButtonSelContinue,
                                             [dropAltLeft.Items[dropAltLeft.ItemIndex], AltLeftLabel.Caption])
    else dropShiftAltLeft.Items[0] := dlfMouseSimpleButtonNothing;
  end;
  if (Sender = dropCtrlLeft) or (Sender = nil) then begin
    if TMouseOptButtonAction(dropCtrlLeft.ItemIndex) in [mbaSelect..mbaSelectLine]
    then dropShiftCtrlLeft.Items[0] := Format(dlfMouseSimpleButtonSelContinue,
                                             [dropCtrlLeft.Items[dropCtrlLeft.ItemIndex], CtrLLeftLabel.Caption])
    else dropShiftCtrlLeft.Items[0] := dlfMouseSimpleButtonNothing;
  end;
  if (Sender = dropAltCtrlLeft) or (Sender = nil) then begin
    if TMouseOptButtonAction(dropAltCtrlLeft.ItemIndex) in [mbaSelect..mbaSelectLine]
    then dropShiftAltCtrlLeft.Items[0] := Format(dlfMouseSimpleButtonSelContinue,
                                             [dropAltCtrlLeft.Items[dropAltCtrlLeft.ItemIndex], AltCtrlLeftLabel.Caption])
    else dropShiftAltCtrlLeft.Items[0] := dlfMouseSimpleButtonNothing;
  end;
end;

function TEditorMouseOptionsFrame.GetTitle: String;
begin
  Result := dlgMouseOptions;
end;

procedure TEditorMouseOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);

  procedure SetupButtonCombo(ACombo: TComboBox; ASkipSel: Boolean = False);
  begin
    // must be in the order in which the mba* are declared
    ACombo.Clear;
    ACombo.Items.Add(dlfMouseSimpleButtonNothing);           // mbaNone,
    if not ASkipSel then begin
      ACombo.Items.Add(dlfMouseSimpleButtonSelect);            // mbaSelect,
      ACombo.Items.Add(dlfMouseSimpleButtonSelectColumn);      // mbaSelectColumn,
      ACombo.Items.Add(dlfMouseSimpleButtonSelectLine);        // mbaSelectLine,
    end;
    //ACombo.Items.Add(dlfMouseSimpleButtonSelectByToken);     // mbaSelectTokens,
    ACombo.Items.Add(dlfMouseSimpleButtonSelectByWord);      // mbaSelectWords,
    //ACombo.Items.Add(dlfMouseSimpleButtonSelectByLine);      // mbaSelectLines,
    ACombo.Items.Add(dlfMouseSimpleButtonSetWord);           // mbaSelectSetWord,
    ACombo.Items.Add(dlfMouseSimpleButtonSetLineSmart);      // mbaSelectSetLineSmart,
    ACombo.Items.Add(dlfMouseSimpleButtonSetLineFull);       // mbaSelectSetLineFull,
    ACombo.Items.Add(dlfMouseSimpleButtonSetPara);           // mbaSelectSetPara,
    ACombo.Items.Add(dlfMouseSimpleButtonPaste);             // mbaPaste,
    ACombo.Items.Add(dlfMouseSimpleButtonDeclaration);       // mbaDeclarationJump,
    ACombo.Items.Add(dlfMouseSimpleButtonDeclarationBlock);  // mbaDeclarationOrBlockJump,
    ACombo.Items.Add(dlfMouseSimpleButtonAddHistoryPoint);   // mbaAddHistoryPoint,
    ACombo.Items.Add(dlfMouseSimpleButtonHistBack);          // mbaHistoryBack,
    ACombo.Items.Add(dlfMouseSimpleButtonHistForw);          // mbaHistoryForw,
    ACombo.Items.Add(dlfMouseSimpleButtonSetFreeBookmark);   // mbaSetFreeBookmark,
    ACombo.Items.Add(dlfMouseSimpleButtonZoomReset);         // mbaZoomReset
    ACombo.Items.Add(dlfMouseSimpleButtonContextMenu);       // mbaContextMenu
    ACombo.Items.Add(dlfMouseSimpleButtonContextMenuDbg);    // mbaContextMenuDebug;
    ACombo.Items.Add(dlfMouseSimpleButtonContextMenuTab);    // mbaContextMenuTab;
    ACombo.Items.Add(dlfMouseSimpleButtonMultiCaretToggle);  // mbaMultiCaretToggle;
  end;

  procedure SetupWheelCombo(ACombo: TComboBox);
  begin
    ACombo.Clear;
    ACombo.Items.Add(dlfMouseSimpleWheelNothing);        //mwaNone,
    ACombo.Items.Add(dlfMouseSimpleWheelSrollDef);       //mwaScroll,
    ACombo.Items.Add(dlfMouseSimpleWheelSrollLine);      //mwaScrollSingleLine,
    ACombo.Items.Add(dlfMouseSimpleWheelSrollPage);      //mwaScrollPage,
    ACombo.Items.Add(dlfMouseSimpleWheelSrollPageLess);  //mwaScrollPageLessOne,
    ACombo.Items.Add(dlfMouseSimpleWheelSrollPageHalf);  //mwaScrollHalfPage,
    ACombo.Items.Add(dlfMouseSimpleWheelHSrollDef);      //mwaScrollHoriz,
    ACombo.Items.Add(dlfMouseSimpleWheelHSrollLine);     //mwaScrollHorizSingleLine,
    ACombo.Items.Add(dlfMouseSimpleWheelHSrollPage);     //mwaScrollHorizPage,
    ACombo.Items.Add(dlfMouseSimpleWheelHSrollPageLess); //mwaScrollHorizPageLessOne
    ACombo.Items.Add(dlfMouseSimpleWheelHSrollPageHalf); //mwaScrollHorizHalfPage
    ACombo.Items.Add(dlfMouseSimpleWheelZoom);
  end;

begin
  FDialog := ADialog;
  chkPredefinedScheme.Caption := dlfMousePredefinedScheme;
  GenericDividerLabel.Caption := dlfMouseSimpleGenericSect;

  GutterDividerLabel.Caption := dlfMouseSimpleGutterSect;
  GutterLeftRadio1.Caption := dlfMouseSimpleGutterLeftDown;
  GutterLeftRadio2.Caption := dlfMouseSimpleGutterLeftUp;
  GutterLeftRadio3.Caption := dlfMouseSimpleGutterLeftUpRight;
  chkGutterTextLines.Caption := dlfMouseSimpleGutterLines;

  TextDividerLabel.Caption := dlfMouseSimpleTextSect;
  TextDrag.Caption := dlfMouseSimpleTextSectDrag;
  RightMoveCaret.Caption := dlfMouseSimpleRightMoveCaret;

  ToolBtnLeftMod.Caption := dlfMouseSimpleTextSectPageLMod;
  ToolBtnLeftMulti.Caption := dlfMouseSimpleTextSectPageLMulti;
  ToolBtnMiddle.Caption := dlfMouseSimpleTextSectPageBtn;
  ToolBtnWheel.Caption := dlfMouseSimpleTextSectPageWheel;
  ToolBtnHorizWheel.Caption := dlfMouseSimpleTextSectPageHorizWheel;
  ToolBtnRight.Caption := dlfMouseSimpleTextSectPageRight;
  ToolBtnExtra1.Caption := dlfMouseSimpleTextSectPageExtra1;
  ToolBtnExtra2.Caption := dlfMouseSimpleTextSectPageExtra2;

    // left multi click
  lblLeftDouble.Caption        := dlfMouseSimpleTextSectLDoubleLabel;
  lblLeftTriple.Caption        := dlfMouseSimpleTextSectLTripleLabel;
  lblLeftQuad.Caption          := dlfMouseSimpleTextSectLQuadLabel;
  lblLeftDoubleShift.Caption   := dlfMouseSimpleTextSectLDoubleShiftLabel;
  lblLeftDoubleCtrl.Caption    := dlfMouseSimpleTextSectLDoubleCtrlLabel;
  lblLeftDoubleAlt.Caption     := dlfMouseSimpleTextSectLDoubleAltLabel;
    // left + modifier click
  AltLeftLabel.Caption          := dlfMouseSimpleTextSectAltLabel;
  CtrLLeftLabel.Caption         := dlfMouseSimpleTextSectCtrlLabel;
  AltCtrlLeftLabel.Caption      := dlfMouseSimpleTextSectAltCtrlLabel;
  ShiftLeftLabel.Caption        := dlfMouseSimpleTextSectShiftLabel;
  ShiftAltLeftLabel.Caption     := dlfMouseSimpleTextSectShiftAltLabel;
  ShiftCtrlLeftLabel.Caption    := dlfMouseSimpleTextSectShiftCtrlLabel;
  ShiftAltCtrlLeftLabel.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // middle click
  lblMiddle.Caption     := dlfMouseSimpleTextSectMidLabel;
  lblMiddleShift.Caption   := dlfMouseSimpleTextSectShiftLabel;
  lblMiddleAlt.Caption  := dlfMouseSimpleTextSectAltLabel;
  lblMiddleCtrl.Caption := dlfMouseSimpleTextSectCtrlLabel;
  lblMiddleAltCtrl.Caption      := dlfMouseSimpleTextSectAltCtrlLabel;
  lblMiddleShiftAlt.Caption     := dlfMouseSimpleTextSectShiftAltLabel;
  lblMiddleShiftCtrl.Caption    := dlfMouseSimpleTextSectShiftCtrlLabel;
  lblMiddleShiftAltCtrl.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // Right click
  lblRight.Caption     := dlfMouseSimpleTextSectRightLabel;
  lblRightShift.Caption   := dlfMouseSimpleTextSectShiftLabel;
  lblRightAlt.Caption  := dlfMouseSimpleTextSectAltLabel;
  lblRightCtrl.Caption := dlfMouseSimpleTextSectCtrlLabel;
  lblRightAltCtrl.Caption      := dlfMouseSimpleTextSectAltCtrlLabel;
  lblRightShiftAlt.Caption     := dlfMouseSimpleTextSectShiftAltLabel;
  lblRightShiftCtrl.Caption    := dlfMouseSimpleTextSectShiftCtrlLabel;
  lblRightShiftAltCtrl.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // Extra1 click
  lblExtra1.Caption     := dlfMouseSimpleTextSectExtra1Label;
  lblExtra1Shift.Caption   := dlfMouseSimpleTextSectShiftLabel;
  lblExtra1Alt.Caption  := dlfMouseSimpleTextSectAltLabel;
  lblExtra1Ctrl.Caption := dlfMouseSimpleTextSectCtrlLabel;
  lblExtra1AltCtrl.Caption      := dlfMouseSimpleTextSectAltCtrlLabel;
  lblExtra1ShiftAlt.Caption     := dlfMouseSimpleTextSectShiftAltLabel;
  lblExtra1ShiftCtrl.Caption    := dlfMouseSimpleTextSectShiftCtrlLabel;
  lblExtra1ShiftAltCtrl.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // Extra2 click
  lblExtra2.Caption     := dlfMouseSimpleTextSectExtra2Label;
  lblExtra2Shift.Caption   := dlfMouseSimpleTextSectShiftLabel;
  lblExtra2Alt.Caption  := dlfMouseSimpleTextSectAltLabel;
  lblExtra2Ctrl.Caption := dlfMouseSimpleTextSectCtrlLabel;
  lblExtra2AltCtrl.Caption      := dlfMouseSimpleTextSectAltCtrlLabel;
  lblExtra2ShiftAlt.Caption     := dlfMouseSimpleTextSectShiftAltLabel;
  lblExtra2ShiftCtrl.Caption    := dlfMouseSimpleTextSectShiftCtrlLabel;
  lblExtra2ShiftAltCtrl.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // wheel
  lblWheel.Caption      := dlfMouseSimpleTextSectWheelLabel;
  lblWheelCtrl.Caption  := dlfMouseSimpleTextSectCtrlWheelLabel;
  lblWheelAlt.Caption   := dlfMouseSimpleTextSectAltWheelLabel;
  lblWheelShift.Caption := dlfMouseSimpleTextShiftSectWheelLabel;
  lblWheelAltCtrl.Caption      := dlfMouseSimpleTextSectAltCtrlWheelLabel;
  lblWheelShiftAlt.Caption     := dlfMouseSimpleTextSectShiftAltWheelLabel;
  lblWheelShiftCtrl.Caption    := dlfMouseSimpleTextSectShiftCtrlWheelLabel;
  lblWheelShiftAltCtrl.Caption := dlfMouseSimpleTextSectShiftAltCtrlWheelLabel;
    // Horiz wheel
  lblWheelHoriz.Caption      := dlfMouseSimpleTextSectWheelLabel;
  lblWheelCtrlHoriz.Caption  := dlfMouseSimpleTextSectCtrlWheelLabel;
  lblWheelAltHoriz.Caption   := dlfMouseSimpleTextSectAltWheelLabel;
  lblWheelShiftHoriz.Caption := dlfMouseSimpleTextShiftSectWheelLabel;
  lblWheelAltCtrlHoriz.Caption      := dlfMouseSimpleTextSectAltCtrlWheelLabel;
  lblWheelShiftAltHoriz.Caption     := dlfMouseSimpleTextSectShiftAltWheelLabel;
  lblWheelShiftCtrlHoriz.Caption    := dlfMouseSimpleTextSectShiftCtrlWheelLabel;
  lblWheelShiftAltCtrlHoriz.Caption := dlfMouseSimpleTextSectShiftAltCtrlWheelLabel;

    // left multi click
  SetupButtonCombo(dropLeftDouble, True);
  SetupButtonCombo(dropLeftTriple, True);
  SetupButtonCombo(dropLeftQuad, True);
  SetupButtonCombo(dropLeftShiftDouble, True);
  SetupButtonCombo(dropLeftAltDouble, True);
  SetupButtonCombo(dropLeftCtrlDouble, True);
    // left + modifier click
  SetupButtonCombo(dropShiftLeft);
  SetupButtonCombo(dropAltLeft);
  SetupButtonCombo(dropCtrlLeft);
  SetupButtonCombo(dropAltCtrlLeft);
  SetupButtonCombo(dropShiftAltLeft);
  SetupButtonCombo(dropShiftCtrlLeft);
  SetupButtonCombo(dropShiftAltCtrlLeft);
    // middle click
  SetupButtonCombo(dropMiddle);
  SetupButtonCombo(dropMiddleShift);
  SetupButtonCombo(dropMiddleAlt);
  SetupButtonCombo(dropMiddleCtrl);
  SetupButtonCombo(dropMiddleAltCtrl);
  SetupButtonCombo(dropMiddleShiftCtrl);
  SetupButtonCombo(dropMiddleShiftAlt);
  SetupButtonCombo(dropMiddleShiftAltCtrl);
    // Right click
  SetupButtonCombo(dropRight);
  SetupButtonCombo(dropRightShift);
  SetupButtonCombo(dropRightAlt);
  SetupButtonCombo(dropRightCtrl);
  SetupButtonCombo(dropRightAltCtrl);
  SetupButtonCombo(dropRightShiftCtrl);
  SetupButtonCombo(dropRightShiftAlt);
  SetupButtonCombo(dropRightShiftAltCtrl);
    // Extra1 click
  SetupButtonCombo(dropExtra1);
  SetupButtonCombo(dropExtra1Shift);
  SetupButtonCombo(dropExtra1Alt);
  SetupButtonCombo(dropExtra1Ctrl);
  SetupButtonCombo(dropExtra1AltCtrl);
  SetupButtonCombo(dropExtra1ShiftCtrl);
  SetupButtonCombo(dropExtra1ShiftAlt);
  SetupButtonCombo(dropExtra1ShiftAltCtrl);
    // extra2 click
  SetupButtonCombo(dropExtra2);
  SetupButtonCombo(dropExtra2Shift);
  SetupButtonCombo(dropExtra2Alt);
  SetupButtonCombo(dropExtra2Ctrl);
  SetupButtonCombo(dropExtra2AltCtrl);
  SetupButtonCombo(dropExtra2ShiftCtrl);
  SetupButtonCombo(dropExtra2ShiftAlt);
  SetupButtonCombo(dropExtra2ShiftAltCtrl);
    // wheel
  SetupWheelCombo(dropWheel);
  SetupWheelCombo(dropWheelCtrl);
  SetupWheelCombo(dropWheelAlt);
  SetupWheelCombo(dropWheelShift);
  SetupWheelCombo(dropWheelAltCtrl);
  SetupWheelCombo(dropWheelShiftAlt);
  SetupWheelCombo(dropWheelShiftCtrl);
  SetupWheelCombo(dropWheelShiftAltCtrl);
    // Horiz wheel
  SetupWheelCombo(dropWheelHoriz);
  SetupWheelCombo(dropWheelCtrlHoriz);
  SetupWheelCombo(dropWheelAltHoriz);
  SetupWheelCombo(dropWheelShiftHoriz);
  SetupWheelCombo(dropWheelAltCtrlHoriz);
  SetupWheelCombo(dropWheelShiftAltHoriz);
  SetupWheelCombo(dropWheelShiftCtrlHoriz);
  SetupWheelCombo(dropWheelShiftAltCtrlHoriz);

  WarnLabel.Caption := dlfMouseSimpleWarning;
  DiffLabel.Caption := dlfMouseSimpleDiff;
  ResetAllButton.Caption := dlfMouseResetAll;
  ResetGutterButton.Caption := dlfMouseResetGutter;
  ResetTextButton.Caption := dlfMouseResetText;
  HideMouseCheckBox.Caption := dlgAutoHideCursor;
end;

procedure TEditorMouseOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
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
    moglUpClickAndSelectRighHalf: GutterLeftRadio3.Checked := True;
  end;
  chkGutterTextLines.Checked := FTempMouseSettings.SelectOnLineNumbers;
  TextDrag.Checked    := FTempMouseSettings.TextDrag;
  RightMoveCaret.Checked := FTempMouseSettings.TextRightMoveCaret;

  dropAltLeft.ItemIndex          := ord(FTempMouseSettings.TextAltLeftClick);
  dropCtrlLeft.ItemIndex         := ord(FTempMouseSettings.TextCtrlLeftClick);
  dropAltCtrlLeft.ItemIndex      := ord(FTempMouseSettings.TextAltCtrlLeftClick);
  dropShiftLeft.ItemIndex        := ord(FTempMouseSettings.TextShiftLeftClick);
  dropShiftAltLeft.ItemIndex     := ord(FTempMouseSettings.TextShiftAltLeftClick);
  dropShiftCtrlLeft.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlLeftClick);
  dropShiftAltCtrlLeft.ItemIndex := ord(FTempMouseSettings.TextShiftAltCtrlLeftClick);

  // 1,2&3 are mouse selection, and not avail for double clicks
  dropLeftDouble.ItemIndex      := Max(ord(FTempMouseSettings.TextDoubleLeftClick)-3,0);
  dropLeftTriple.ItemIndex      := Max(ord(FTempMouseSettings.TextTripleLeftClick)-3,0);
  dropLeftQuad.ItemIndex        := Max(ord(FTempMouseSettings.TextQuadLeftClick)-3,0);
  dropLeftShiftDouble.ItemIndex := Max(ord(FTempMouseSettings.TextShiftDoubleLeftClick)-3,0);
  dropLeftAltDouble.ItemIndex   := Max(ord(FTempMouseSettings.TextAltDoubleLeftClick)-3,0);
  dropLeftCtrlDouble.ItemIndex  := Max(ord(FTempMouseSettings.TextCtrlDoubleLeftClick)-3,0);

  dropMiddle.ItemIndex      := ord(FTempMouseSettings.TextMiddleClick);
  dropMiddleShift.ItemIndex := ord(FTempMouseSettings.TextShiftMiddleClick);
  dropMiddleAlt.ItemIndex   := ord(FTempMouseSettings.TextAltMiddleClick);
  dropMiddleCtrl.ItemIndex  := ord(FTempMouseSettings.TextCtrlMiddleClick);
  dropMiddleAltCtrl.ItemIndex      := ord(FTempMouseSettings.TextAltCtrlMiddleClick);
  dropMiddleShiftCtrl.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlMiddleClick);
  dropMiddleShiftAlt.ItemIndex     := ord(FTempMouseSettings.TextShiftAltMiddleClick);
  dropMiddleShiftAltCtrl.ItemIndex := ord(FTempMouseSettings.TextShiftAltCtrlMiddleClick);

  dropRight.ItemIndex      := ord(FTempMouseSettings.TextRightClick);
  dropRightShift.ItemIndex := ord(FTempMouseSettings.TextShiftRightClick);
  dropRightAlt.ItemIndex   := ord(FTempMouseSettings.TextAltRightClick);
  dropRightCtrl.ItemIndex  := ord(FTempMouseSettings.TextCtrlRightClick);
  dropRightAltCtrl.ItemIndex      := ord(FTempMouseSettings.TextAltCtrlRightClick);
  dropRightShiftCtrl.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlRightClick);
  dropRightShiftAlt.ItemIndex     := ord(FTempMouseSettings.TextShiftAltRightClick);
  dropRightShiftAltCtrl.ItemIndex := ord(FTempMouseSettings.TextShiftAltCtrlRightClick);

  dropExtra1.ItemIndex      := ord(FTempMouseSettings.TextExtra1Click);
  dropExtra1Shift.ItemIndex := ord(FTempMouseSettings.TextShiftExtra1Click);
  dropExtra1Alt.ItemIndex   := ord(FTempMouseSettings.TextAltExtra1Click);
  dropExtra1Ctrl.ItemIndex  := ord(FTempMouseSettings.TextCtrlExtra1Click);
  dropExtra1AltCtrl.ItemIndex      := ord(FTempMouseSettings.TextAltCtrlExtra1Click);
  dropExtra1ShiftCtrl.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlExtra1Click);
  dropExtra1ShiftAlt.ItemIndex     := ord(FTempMouseSettings.TextShiftAltExtra1Click);
  dropExtra1ShiftAltCtrl.ItemIndex := ord(FTempMouseSettings.TextShiftAltCtrlExtra1Click);

  dropExtra2.ItemIndex      := ord(FTempMouseSettings.TextExtra2Click);
  dropExtra2Shift.ItemIndex := ord(FTempMouseSettings.TextShiftExtra2Click);
  dropExtra2Alt.ItemIndex   := ord(FTempMouseSettings.TextAltExtra2Click);
  dropExtra2Ctrl.ItemIndex  := ord(FTempMouseSettings.TextCtrlExtra2Click);
  dropExtra2AltCtrl.ItemIndex      := ord(FTempMouseSettings.TextAltCtrlExtra2Click);
  dropExtra2ShiftCtrl.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlExtra2Click);
  dropExtra2ShiftAlt.ItemIndex     := ord(FTempMouseSettings.TextShiftAltExtra2Click);
  dropExtra2ShiftAltCtrl.ItemIndex := ord(FTempMouseSettings.TextShiftAltCtrlExtra2Click);

  dropWheel.ItemIndex      := ord(FTempMouseSettings.Wheel);
  dropWheelCtrl.ItemIndex  := ord(FTempMouseSettings.CtrlWheel);
  dropWheelAlt.ItemIndex   := ord(FTempMouseSettings.AltWheel);
  dropWheelShift.ItemIndex := ord(FTempMouseSettings.ShiftWheel);
  dropWheelAltCtrl.ItemIndex      := ord(FTempMouseSettings.AltWheel);
  dropWheelShiftAlt.ItemIndex     := ord(FTempMouseSettings.ShiftAltWheel);
  dropWheelShiftCtrl.ItemIndex    := ord(FTempMouseSettings.ShiftCtrlWheel);
  dropWheelShiftAltCtrl.ItemIndex := ord(FTempMouseSettings.ShiftAltCtrlWheel);

  dropWheelHoriz.ItemIndex      := ord(FTempMouseSettings.HorizWheel);
  dropWheelCtrlHoriz.ItemIndex  := ord(FTempMouseSettings.CtrlHorizWheel);
  dropWheelAltHoriz.ItemIndex   := ord(FTempMouseSettings.AltHorizWheel);
  dropWheelShiftHoriz.ItemIndex := ord(FTempMouseSettings.ShiftHorizWheel);
  dropWheelAltCtrlHoriz.ItemIndex      := ord(FTempMouseSettings.AltHorizWheel);
  dropWheelShiftAltHoriz.ItemIndex     := ord(FTempMouseSettings.ShiftAltHorizWheel);
  dropWheelShiftCtrlHoriz.ItemIndex    := ord(FTempMouseSettings.ShiftCtrlHorizWheel);
  dropWheelShiftAltCtrlHoriz.ItemIndex := ord(FTempMouseSettings.ShiftAltCtrlHorizWheel);

  Dec(FInClickHandler);
  UpdateButtons;

  HideMouseCheckBox.Checked := eoAutoHideCursor in TEditorOptions(AOptions).SynEditOptions2;
  CheckForShiftChange(nil);
end;

procedure TEditorMouseOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
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

