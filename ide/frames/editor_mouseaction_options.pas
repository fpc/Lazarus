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
  LResources, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, SynEdit,
  StdCtrls, ExtCtrls, Classes, LCLProc, editor_mouseaction_options_advanced;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    HideMouseCheckBox: TCheckBox;
    DiffLabel: TLabel;
    GenericDividerLeft: TBevel;
    GenericDividerLabel: TLabel;
    GenericDividerRight: TBevel;
    TextDoubleSelLine: TCheckBox;
    CtrLLeftLabel: TLabel;
    MiddleBtnLabel: TLabel;
    PanelTextCtrlLeft: TPanel;
    CtrlLeftRadio1: TRadioButton;
    CtrlLeftRadio2: TRadioButton;
    CtrlLeftRadio3: TRadioButton;
    RightMoveCaret: TCheckBox;
    TextDrag : TCheckBox;
    PanelTextCheckBox : TPanel;
    TextAltMode: TCheckBox;
    WarnLabel: TLabel;
    ResetTextButton: TButton;
    ResetGutterButton: TButton;
    PanelGutter: TPanel;
    PanelTextMiddle: TPanel;
    TextMidRadio1: TRadioButton;
    TextMidRadio2: TRadioButton;
    TextMidRadio3: TRadioButton;
    GutterDividerLabel: TLabel;
    BottomDivider: TBevel;
    GutterLeftRadio1: TRadioButton;
    GutterLeftRadio2: TRadioButton;
    TextDividerLabel: TLabel;
    GutterDividerLeft: TBevel;
    TextDividerLeft: TBevel;
    GutterDividerRight: TBevel;
    TextDividerRight: TBevel;
    RadioGroup1: TRadioGroup;
    TextLeft: TCheckGroup;
    TextMiddle: TRadioGroup;
    GutterLeft: TRadioGroup;
    ResetAllButton: TButton;
    procedure ResetGutterButtonClick(Sender: TObject);
    procedure ResetAllButtonClick(Sender: TObject);
    procedure ResetTextButtonClick(Sender: TObject);
    procedure CheckOrRadioChange(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FOptions: TAbstractIDEOptions;
    FTempMouseSettings: TEditorMouseOptions;
    FInClickHandler: Integer;
    function IsTextSettingsChanged: Boolean;
    function IsGutterSettingsChanged: Boolean;
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
  end;

implementation

{ TEditorMouseOptionsFrame }

procedure TEditorMouseOptionsFrame.CheckOrRadioChange(Sender: TObject);
var
  MouseDiff: Boolean;
begin
  if FInClickHandler > 0 then exit;
  Inc(FInClickHandler);
  try
    MouseDiff := not FTempMouseSettings.IsPresetEqualToMouseActions;
    if not MouseDiff then begin
      ResetAllButtonClick(nil);
      ResetTextButton.Visible   := False;
      ResetGutterButton.Visible := False;
      ResetAllButton.Visible    := False;
      WarnLabel.Visible := False;
      DiffLabel.Visible := False;
      exit;
    end;
    ResetTextButton.Visible   := True; // MouseDiff or IsTextSettingsChanged;
    ResetGutterButton.Visible := True; // MouseDiff or IsGutterSettingsChanged;
    ResetAllButton.Visible    := True; // ResetTextButton.Enabled or ResetGutterButton.Enabled;
    WarnLabel.Visible := IsTextSettingsChanged or IsGutterSettingsChanged;
    DiffLabel.Visible := (not WarnLabel.Visible) and MouseDiff;
  finally
    Dec(FInClickHandler);
  end;
end;

procedure TEditorMouseOptionsFrame.ResetGutterButtonClick(Sender: TObject);
begin
  if GutterLeftRadio2.Checked then
    FTempMouseSettings.GutterLeft := moglUpClickAndSelect
  else
    FTempMouseSettings.GutterLeft := moGLDownClick;
  FTempMouseSettings.ResetGutterToDefault;
  if FDialog.FindEditor(TEditorMouseOptionsAdvFrame) <> nil then
    TEditorMouseOptionsAdvFrame(FDialog.FindEditor(TEditorMouseOptionsAdvFrame)).RefreshSettings;
  CheckOrRadioChange(nil);
end;

procedure TEditorMouseOptionsFrame.ResetTextButtonClick(Sender: TObject);
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
  CheckOrRadioChange(nil);
end;

procedure TEditorMouseOptionsFrame.ResetAllButtonClick(Sender: TObject);
begin
  ResetTextButtonClick(nil);
  ResetGutterButtonClick(nil);
end;

function TEditorMouseOptionsFrame.IsGutterSettingsChanged: Boolean;
begin
  Result := not (
    ( (GutterLeftRadio1.Checked and (FTempMouseSettings.GutterLeft = moGLDownClick)) or
      (GutterLeftRadio2.Checked and (FTempMouseSettings.GutterLeft = moglUpClickAndSelect))
    )
  );

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

procedure TEditorMouseOptionsFrame.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Value and (FTempMouseSettings <> nil) then
    CheckOrRadioChange(nil);
end;

function TEditorMouseOptionsFrame.GetTitle: String;
begin
  Result := dlgMouseOptions;
end;

procedure TEditorMouseOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;
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
begin
  Inc(FInClickHandler);
  FOptions := AOptions;
  FTempMouseSettings := TEditorOptions(AOptions).TempMouseSettings;
  FTempMouseSettings.Read;
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
  CheckOrRadioChange(nil);

  HideMouseCheckBox.Checked := eoAutoHideCursor in TEditorOptions(AOptions).SynEditOptions2;
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  FTempMouseSettings.WriteBack;
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

initialization
  {$I editor_mouseaction_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorMouseOptionsFrame, EdtOptionsMouse);
end.

