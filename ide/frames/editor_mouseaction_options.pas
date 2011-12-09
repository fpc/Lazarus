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
  math, sysutils, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, SynEdit, DividerBevel, StdCtrls,
  ExtCtrls, Classes, LCLProc, editor_mouseaction_options_advanced, Controls, Forms, ComCtrls;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    dropShiftMiddle: TComboBox;
    lblLeftDouble: TLabel;
    lblLeftTripple: TLabel;
    lblLeftQuad: TLabel;
    lblLeftDoubleShift: TLabel;
    lblLeftDoubleAlt: TLabel;
    lblLeftDoubleCtrl: TLabel;
    dropLeftDouble: TComboBox;
    dropLeftTripple: TComboBox;
    dropLeftQuad: TComboBox;
    dropLeftShiftDouble: TComboBox;
    dropLeftAltDouble: TComboBox;
    dropLeftCtrlDouble: TComboBox;
    ShiftMiddleLabel: TLabel;
    ShiftLeftLabel: TLabel;
    AltCtrlLeftLabel: TLabel;
    ShiftCtrlLeftLabel: TLabel;
    ShiftAltLeftLabel: TLabel;
    ShiftCtrlAltLeftLabel: TLabel;
    BottomDivider: TBevel;
    chkPredefinedScheme: TCheckBox;
    AltLeftLabel: TLabel;
    AltMiddleBtnLabel: TLabel;
    dropAltLeft: TComboBox;
    dropAltCtrlLeft: TComboBox;
    dropShiftAltLeft: TComboBox;
    dropShiftCtrlLeft: TComboBox;
    dropShiftAltCtrlLeft: TComboBox;
    dropShiftLeft: TComboBox;
    dropAltMiddle: TComboBox;
    PageLeftDbl: TPage;
    PageLeftMod: TPage;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    ScrollBox4: TScrollBox;
    TextDividerLabel: TDividerBevel;
    GutterDividerLabel: TDividerBevel;
    GenericDividerLabel: TDividerBevel;
    dropMiddle: TComboBox;
    dropCtrlLeft: TComboBox;
    CtrLLeftLabel: TLabel;
    DiffLabel: TLabel;
    dropCtrlMiddle: TComboBox;
    dropWheel: TComboBox;
    dropCtrlWheel: TComboBox;
    dropAltWheel: TComboBox;
    dropShiftWheel: TComboBox;
    dropUserSchemes: TComboBox;
    GutterLeftRadio1: TRadioButton;
    GutterLeftRadio2: TRadioButton;
    HideMouseCheckBox: TCheckBox;
    MiddleBtnLabel: TLabel;
    CtrlMiddleBtnLabel: TLabel;
    Notebook1: TNotebook;
    PageMiddle: TPage;
    PageWheel: TPage;
    ToolBar1: TToolBar;
    ToolButtonMiddle: TToolButton;
    ToolBtnWheel: TToolButton;
    ToolButtonLeftMod: TToolButton;
    ToolButtonLeftMulti: TToolButton;
    WheelBtnLabel: TLabel;
    CtrlWheelBtnLabel: TLabel;
    AltWheelBtnLabel: TLabel;
    ShiftWheelBtnLabel: TLabel;
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
    WarnLabel: TLabel;
    procedure CheckOrRadioChange(Sender: TObject);
    procedure chkPredefinedSchemeChange(Sender: TObject);
    procedure dropUserSchemesChange(Sender: TObject);
    procedure dropUserSchemesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ResetGutterButtonClick(Sender: TObject);
    procedure ResetTextButtonClick(Sender: TObject);
    procedure ResetAllButtonClick(Sender: TObject);
    procedure ToolButtonMiddleClick(Sender: TObject);
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

procedure TEditorMouseOptionsFrame.ToolButtonMiddleClick(Sender: TObject);
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
    (FTempMouseSettings.TextCtrlAltLeftClick      = TMouseOptButtonAction(dropAltCtrlLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftLeftClick        = TMouseOptButtonAction(dropShiftLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftAltLeftClick     = TMouseOptButtonAction(dropShiftAltLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlLeftClick    = TMouseOptButtonAction(dropShiftCtrlLeft.ItemIndex)) and
    (FTempMouseSettings.TextShiftCtrlAltLeftClick = TMouseOptButtonAction(dropShiftAltCtrlLeft.ItemIndex)) and

    (FTempMouseSettings.TextDoubleLeftClick      = IdxToDoubleMouseOptButtonAction(dropLeftDouble.ItemIndex)) and
    (FTempMouseSettings.TextTrippleLeftClick     = IdxToDoubleMouseOptButtonAction(dropLeftTripple.ItemIndex)) and
    (FTempMouseSettings.TextQuadLeftClick        = IdxToDoubleMouseOptButtonAction(dropLeftQuad.ItemIndex)) and
    (FTempMouseSettings.TextShiftDoubleLeftClick = IdxToDoubleMouseOptButtonAction(dropLeftShiftDouble.ItemIndex)) and
    (FTempMouseSettings.TextAltDoubleLeftClick   = IdxToDoubleMouseOptButtonAction(dropLeftAltDouble.ItemIndex)) and
    (FTempMouseSettings.TextCtrlDoubleLeftClick  = IdxToDoubleMouseOptButtonAction(dropLeftCtrlDouble.ItemIndex)) and

    (FTempMouseSettings.TextMiddleClick      = TMouseOptButtonAction(dropMiddle.ItemIndex)) and
    (FTempMouseSettings.TextShiftMiddleClick = TMouseOptButtonAction(dropShiftMiddle.ItemIndex)) and
    (FTempMouseSettings.TextAltMiddleClick   = TMouseOptButtonAction(dropAltMiddle.ItemIndex)) and
    (FTempMouseSettings.TextCtrlMiddleClick  = TMouseOptButtonAction(dropCtrlMiddle.ItemIndex)) and

    (FTempMouseSettings.Wheel      = TMouseOptWheelAction(dropWheel.ItemIndex)) and
    (FTempMouseSettings.CtrlWheel  = TMouseOptWheelAction(dropCtrlWheel.ItemIndex)) and
    (FTempMouseSettings.AltWheel   = TMouseOptWheelAction(dropAltWheel.ItemIndex)) and
    (FTempMouseSettings.ShiftWheel = TMouseOptWheelAction(dropShiftWheel.ItemIndex))
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
  FTempMouseSettings.TextCtrlAltLeftClick      := TMouseOptButtonAction(dropAltCtrlLeft.ItemIndex);
  FTempMouseSettings.TextShiftLeftClick        := TMouseOptButtonAction(dropShiftLeft.ItemIndex);
  FTempMouseSettings.TextShiftAltLeftClick     := TMouseOptButtonAction(dropShiftAltLeft.ItemIndex);
  FTempMouseSettings.TextShiftCtrlLeftClick    := TMouseOptButtonAction(dropShiftCtrlLeft.ItemIndex);
  FTempMouseSettings.TextShiftCtrlAltLeftClick := TMouseOptButtonAction(dropShiftAltCtrlLeft.ItemIndex);

  FTempMouseSettings.TextDoubleLeftClick      := IdxToDoubleMouseOptButtonAction(dropLeftDouble.ItemIndex);
  FTempMouseSettings.TextTrippleLeftClick     := IdxToDoubleMouseOptButtonAction(dropLeftTripple.ItemIndex);
  FTempMouseSettings.TextQuadLeftClick        := IdxToDoubleMouseOptButtonAction(dropLeftQuad.ItemIndex);
  FTempMouseSettings.TextShiftDoubleLeftClick := IdxToDoubleMouseOptButtonAction(dropLeftShiftDouble.ItemIndex);
  FTempMouseSettings.TextAltDoubleLeftClick   := IdxToDoubleMouseOptButtonAction(dropLeftAltDouble.ItemIndex);
  FTempMouseSettings.TextCtrlDoubleLeftClick  := IdxToDoubleMouseOptButtonAction(dropLeftCtrlDouble.ItemIndex);

  FTempMouseSettings.TextMiddleClick      := TMouseOptButtonAction(dropMiddle.ItemIndex);
  FTempMouseSettings.TextShiftMiddleClick := TMouseOptButtonAction(dropShiftMiddle.ItemIndex);
  FTempMouseSettings.TextAltMiddleClick   := TMouseOptButtonAction(dropAltMiddle.ItemIndex);
  FTempMouseSettings.TextCtrlMiddleClick  := TMouseOptButtonAction(dropCtrlMiddle.ItemIndex);

  FTempMouseSettings.Wheel               := TMouseOptWheelAction(dropWheel.ItemIndex);
  FTempMouseSettings.CtrlWheel           := TMouseOptWheelAction(dropCtrlWheel.ItemIndex);
  FTempMouseSettings.AltWheel            := TMouseOptWheelAction(dropAltWheel.ItemIndex);
  FTempMouseSettings.ShiftWheel          := TMouseOptWheelAction(dropShiftWheel.ItemIndex);

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

  TextDividerLabel.Caption := dlfMouseSimpleTextSect;
  TextDrag.Caption := dlfMouseSimpleTextSectDrag;
  RightMoveCaret.Caption := dlfMouseSimpleRightMoveCaret;

  ToolButtonLeftMod.Caption := dlfMouseSimpleTextSectPageLMod;
  ToolButtonLeftMulti.Caption := dlfMouseSimpleTextSectPageLMulti;
  ToolButtonMiddle.Caption := dlfMouseSimpleTextSectPageBtn;
  ToolBtnWheel.Caption := dlfMouseSimpleTextSectPageWheel;
  ToolBar1.AnchorSide[akTop].Side := asrBottom;

    // left multi click
  lblLeftDouble.Caption        := dlfMouseSimpleTextSectLDoubleLabel;
  lblLeftTripple.Caption       := dlfMouseSimpleTextSectLTrippleLabel;
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
  ShiftCtrlAltLeftLabel.Caption := dlfMouseSimpleTextSectShiftAltCtrlLabel;
    // middle click
  MiddleBtnLabel.Caption     := dlfMouseSimpleTextSectMidLabel;
  ShiftMiddleLabel.Caption   := dlfMouseSimpleTextSectShiftLabel;
  AltMiddleBtnLabel.Caption  := dlfMouseSimpleTextSectAltLabel;
  CtrlMiddleBtnLabel.Caption := dlfMouseSimpleTextSectCtrlLabel;
    // wheel
  WheelBtnLabel.Caption      := dlfMouseSimpleTextSectWheelLabel;
  CtrlWheelBtnLabel.Caption  := dlfMouseSimpleTextSectCtrlWheelLabel;
  AltWheelBtnLabel.Caption   := dlfMouseSimpleTextSectAltWheelLabel;
  ShiftWheelBtnLabel.Caption := dlfMouseSimpleTextShiftSectWheelLabel;

    // left multi click
  SetupButtonCombo(dropLeftDouble, True);
  SetupButtonCombo(dropLeftTripple, True);
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
  SetupButtonCombo(dropShiftMiddle);
  SetupButtonCombo(dropAltMiddle);
  SetupButtonCombo(dropCtrlMiddle);
    // wheel
  SetupWheelCombo(dropWheel);
  SetupWheelCombo(dropCtrlWheel);
  SetupWheelCombo(dropAltWheel);
  SetupWheelCombo(dropShiftWheel);

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
  TextDrag.Checked    := FTempMouseSettings.TextDrag;
  RightMoveCaret.Checked := FTempMouseSettings.TextRightMoveCaret;

  dropAltLeft.ItemIndex          := ord(FTempMouseSettings.TextAltLeftClick);
  dropCtrlLeft.ItemIndex         := ord(FTempMouseSettings.TextCtrlLeftClick);
  dropAltCtrlLeft.ItemIndex      := ord(FTempMouseSettings.TextCtrlAltLeftClick);
  dropShiftLeft.ItemIndex        := ord(FTempMouseSettings.TextShiftLeftClick);
  dropShiftAltLeft.ItemIndex     := ord(FTempMouseSettings.TextShiftAltLeftClick);
  dropShiftCtrlLeft.ItemIndex    := ord(FTempMouseSettings.TextShiftCtrlLeftClick);
  dropShiftAltCtrlLeft.ItemIndex := ord(FTempMouseSettings.TextShiftCtrlAltLeftClick);

  // 1,2&3 are mouse selection, and not avail for double clicks
  dropLeftDouble.ItemIndex      := Max(ord(FTempMouseSettings.TextDoubleLeftClick)-3,0);
  dropLeftTripple.ItemIndex     := Max(ord(FTempMouseSettings.TextTrippleLeftClick)-3,0);
  dropLeftQuad.ItemIndex        := Max(ord(FTempMouseSettings.TextQuadLeftClick)-3,0);
  dropLeftShiftDouble.ItemIndex := Max(ord(FTempMouseSettings.TextShiftDoubleLeftClick)-3,0);
  dropLeftAltDouble.ItemIndex   := Max(ord(FTempMouseSettings.TextAltDoubleLeftClick)-3,0);
  dropLeftCtrlDouble.ItemIndex  := Max(ord(FTempMouseSettings.TextCtrlDoubleLeftClick)-3,0);

  dropMiddle.ItemIndex      := ord(FTempMouseSettings.TextMiddleClick);
  dropShiftMiddle.ItemIndex := ord(FTempMouseSettings.TextShiftMiddleClick);
  dropAltMiddle.ItemIndex   := ord(FTempMouseSettings.TextAltMiddleClick);
  dropCtrlMiddle.ItemIndex  := ord(FTempMouseSettings.TextCtrlMiddleClick);

  dropWheel.ItemIndex      := ord(FTempMouseSettings.Wheel);
  dropCtrlWheel.ItemIndex  := ord(FTempMouseSettings.CtrlWheel);
  dropAltWheel.ItemIndex   := ord(FTempMouseSettings.AltWheel);
  dropShiftWheel.ItemIndex := ord(FTempMouseSettings.ShiftWheel);

  Dec(FInClickHandler);
  UpdateButtons;

  HideMouseCheckBox.Checked := eoAutoHideCursor in TEditorOptions(AOptions).SynEditOptions2;
  CheckForShiftChange(nil);
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
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

