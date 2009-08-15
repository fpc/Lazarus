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
  LResources, EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf,
  StdCtrls, ExtCtrls, Classes, LCLProc, editor_mouseaction_options_advanced;

type

  { TEditorMouseOptionsFrame }

  TEditorMouseOptionsFrame = class(TAbstractIDEOptionsEditor)
    DiffLabel: TLabel;
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
    function IsTextSettingsChanged: Boolean;
    function IsGutterSettingsChanged: Boolean;
    function IsEqualToMouseActions: Boolean;
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
  MouseDiff := not IsEqualToMouseActions;
  ResetTextButton.Enabled   := MouseDiff or IsTextSettingsChanged;
  ResetGutterButton.Enabled := MouseDiff or IsGutterSettingsChanged;
  ResetAllButton.Enabled    := ResetTextButton.Enabled or ResetGutterButton.Enabled;
  WarnLabel.Visible := IsTextSettingsChanged or IsGutterSettingsChanged;
  DiffLabel.Visible := (not WarnLabel.Visible) and MouseDiff;
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

function TEditorMouseOptionsFrame.IsTextSettingsChanged: Boolean;
begin
  Result := not (
    ( (GutterLeftRadio1.Checked and (FTempMouseSettings.GutterLeft = moGLDownClick)) or
      (GutterLeftRadio2.Checked and (FTempMouseSettings.GutterLeft = moglUpClickAndSelect))
    )
  );

end;

function TEditorMouseOptionsFrame.IsGutterSettingsChanged: Boolean;
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

function TEditorMouseOptionsFrame.IsEqualToMouseActions: Boolean;
var
  Current: TEditorMouseOptions;
begin
  Current := TEditorMouseOptions.Create(nil);
  Current.Assign(FTempMouseSettings);
  Current.ResetTextToDefault;
  Current.ResetGutterToDefault;
  Result :=
    Current.MainActions.Equals(FTempMouseSettings.MainActions) and
    Current.SelActions.Equals (FTempMouseSettings.SelActions) and
    Current.GutterActions.Equals       (FTempMouseSettings.GutterActions) and
    Current.GutterActionsFold.Equals   (FTempMouseSettings.GutterActionsFold) and
    Current.GutterActionsFoldCol.Equals(FTempMouseSettings.GutterActionsFoldCol) and
    Current.GutterActionsFoldExp.Equals(FTempMouseSettings.GutterActionsFoldExp) and
    Current.GutterActionsLines.Equals(FTempMouseSettings.GutterActionsLines);
  Current.Free;
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
end;

procedure TEditorMouseOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
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
  CheckOrRadioChange(nil);
end;

procedure TEditorMouseOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  FTempMouseSettings.WriteBack;
end;

class function TEditorMouseOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  {$I editor_mouseaction_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorMouseOptionsFrame, EdtOptionsMouse);
end.

