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
unit window_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls, ExtCtrls,
  Spin, EnvironmentOpts, LazarusIDEStrConsts, IDEOptionDefs, ObjectInspector,
  IDEOptionsIntf;

type
  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    Bevel2: TBevel;
    GetWindowPositionButton: TButton;
    ApplyButton: TButton;
    HideMessagesIconsCheckBox: TCheckBox;
    LeftLabel: TLabel;
    DockedRadioButton: TRadioButton;
    TopLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    lblWindowCaption: TLabel;
    LeftEdit: TSpinEdit;
    TopEdit: TSpinEdit;
    WidthEdit: TSpinEdit;
    HeightEdit: TSpinEdit;
    UseWindowManagerSettingRadioButton: TRadioButton;
    DefaultRadioButton: TRadioButton;
    RestoreWindowGeometryRadioButton: TRadioButton;
    CustomPositionRadioButton: TRadioButton;
    WindowPositionsGroupBox: TGroupBox;
    HideIDEOnRunCheckBox: TCheckBox;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    WindowPositionsListBox: TListBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure GetWindowPositionButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TIDEWindowLayoutList;
    FLayout: TIDEWindowLayout;
    function GetPlacementRadioButtons(APlacement: TIDEWindowPlacement): TRadioButton;
    procedure SetLayout(const AValue: TIDEWindowLayout);
    procedure SetWindowPositionsItem(Index: integer);
    procedure SaveLayout;
    function GetCaptionFor(AWindow: TNonModalIDEWindow): String;
    property Layout: TIDEWindowLayout read FLayout write SetLayout;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TWindowOptionsFrame }

function TWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgWindow;
end;

procedure TWindowOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Window: TNonModalIDEWindow;
begin
  // windows
  MinimizeAllOnMinimizeMainCheckBox.Caption := dlgMinimizeAllOnMinimizeMain;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;
  HideMessagesIconsCheckBox.Caption := dlgHideMessagesIcons;

  // Window Positions
  WindowPositionsGroupBox.Caption := dlgWinPos;
  with WindowPositionsListBox.Items do
  begin
    BeginUpdate;
    for Window := Succ(Low(TNonModalIDEWindow)) to High(TNonModalIDEWindow) do
      Add(GetCaptionFor(Window));
    Add(dlgObjInsp);
    EndUpdate;
  end;

  LeftLabel.Caption := dlgLeftPos;
  TopLabel.Caption := dlgTopPos;
  WidthLabel.Caption := dlgWidthPos;
  HeightLabel.Caption := DlgHeightPos;
  ApplyButton.Caption := dlgButApply;
  GetWindowPositionButton.Caption := dlgGetPosition;

  UseWindowManagerSettingRadioButton.Caption := rsiwpUseWindowManagerSetting;
  DefaultRadioButton.Caption := rsiwpDefault;
  RestoreWindowGeometryRadioButton.Caption := rsiwpRestoreWindowGeometry;
  DockedRadioButton.Caption := rsiwpDocked;
  CustomPositionRadioButton.Caption := rsiwpCustomPosition;
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    FLayouts := IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked := MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    HideMessagesIconsCheckBox.Checked := HideMessagesIcons;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    SaveLayout;
    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
    HideMessagesIcons:=HideMessagesIconsCheckBox.Checked;
  end;
end;

function TWindowOptionsFrame.GetPlacementRadioButtons(APlacement: TIDEWindowPlacement): TRadioButton;
begin
  case APlacement of
    iwpRestoreWindowGeometry:   Result := RestoreWindowGeometryRadioButton;
    iwpDefault:                 Result := DefaultRadioButton;
    iwpCustomPosition:          Result := CustomPositionRadioButton;
    iwpUseWindowManagerSetting: Result := UseWindowManagerSettingRadioButton;
    iwpDocked:                  Result := DockedRadioButton;
  else
    Result := nil;
  end;
end;

procedure TWindowOptionsFrame.SetLayout(const AValue: TIDEWindowLayout);
var
  APlacement: TIDEWindowPlacement;
  RadioButton: TRadioButton;
begin
  FLayout := AValue;
  if Layout=nil then Exit;

  for APlacement := Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    RadioButton := GetPlacementRadioButtons(APlacement);
    if APlacement in Layout.WindowPlacementsAllowed then
    begin
      RadioButton.Enabled := True;
      RadioButton.Checked := (APlacement = Layout.WindowPlacement);

      if APlacement = iwpCustomPosition then
      begin
        // custom window position
        if Layout.CustomCoordinatesAreValid then
        begin
          LeftEdit.Value := Layout.Left;
          TopEdit.Value := Layout.Top;
          WidthEdit.Value := Layout.Width;
          HeightEdit.Value := Layout.Height;
        end
        else
        if Layout.Form <> nil then
        begin
          LeftEdit.Value := Layout.Form.Left;
          TopEdit.Value := Layout.Form.Top;
          WidthEdit.Value := Layout.Form.Width;
          HeightEdit.Value := Layout.Form.Height;
        end
        else
        begin
          LeftEdit.Value := 0;
          TopEdit.Value := 0;
          WidthEdit.Value := 0;
          HeightEdit.Value := 0;
        end;
      end;
    end
    else
    if RadioButton <> nil then
      RadioButton.Enabled := False;
  end;

  GetWindowPositionButton.Enabled :=
    (iwpCustomPosition in Layout.WindowPlacementsAllowed) and
    (Layout.Form <> nil);
end;

procedure TWindowOptionsFrame.WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TWindowOptionsFrame.ApplyButtonClick(Sender: TObject);
begin
  SaveLayout;
  Layout.Apply;
end;

procedure TWindowOptionsFrame.GetWindowPositionButtonClick(Sender: TObject);
begin
  if Layout.Form <> nil then
  begin
    LeftEdit.Value := Layout.Form.Left;
    TopEdit.Value := Layout.Form.Top;
    WidthEdit.Value := Layout.Form.Width;
    HeightEdit.Value := Layout.Form.Height;
  end;
end;

procedure TWindowOptionsFrame.SetWindowPositionsItem(Index: integer);
begin
  SaveLayout;
  WindowPositionsListBox.ItemIndex := Index;

  if Index < Ord(High(TNonModalIDEWindow)) then
    Layout := FLayouts.ItemByEnum(TNonModalIDEWindow(Index + 1))
  else
  begin
    case Index - Ord(High(TNonModalIDEWindow)) of
      0: Layout := FLayouts.ItemByFormID(DefaultObjectInspectorName);
    end;
  end;

  if Index >= 0 then
    lblWindowCaption.Caption := WindowPositionsListBox.Items[Index];
end;

procedure TWindowOptionsFrame.SaveLayout;
var
  APlacement: TIDEWindowPlacement;
  ARadioButton: TRadioButton;
begin
  if Layout = nil then
   Exit;

  for APlacement := Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    if APlacement in Layout.WindowPlacementsAllowed then
    begin
      ARadioButton := GetPlacementRadioButtons(APlacement);
      if (ARadioButton <> nil) and ARadioButton.Enabled and ARadioButton.Checked then
        Layout.WindowPlacement := APlacement;
      if APlacement = iwpCustomPosition then
      begin
        Layout.Left := LeftEdit.Value;
        Layout.Top := TopEdit.Value;
        Layout.Width := WidthEdit.Value;
        Layout.Height := HeightEdit.Value;
      end;
    end;
  end;
end;

function TWindowOptionsFrame.GetCaptionFor(AWindow: TNonModalIDEWindow): String;
begin
  case AWindow of
    nmiwMainIDEName: Result := dlgMainMenu;
    nmiwSourceNoteBookName: Result := dlgSrcEdit;
    nmiwMessagesViewName: Result := dlgMsgs;
    nmiwCodeExplorerName: Result := lisCodeExplorer;
    nmiwFPDocEditorName: Result := lisCodeHelpMainFormCaption;
    nmiwPkgGraphExplorer: Result := lisMenuPackageGraph;
    nmiwProjectInspector: Result := lisMenuProjectInspector;
    nmiwUnitDependenciesName: Result := dlgUnitDepCaption;
    nmiwDbgOutput: Result := lisMenuViewDebugOutput;
    nmiwBreakPoints: Result := lisMenuViewBreakPoints;
    nmiwWatches: Result := liswlWatchList;
    nmiwLocals: Result := lisLocals;
    nmiwCallStack: Result := lisMenuViewCallStack;
    nmiwEvaluate: Result := lisKMEvaluateModify;
    nmiwRegisters: Result := lisRegisters;
    nmiwAssembler: Result := lisMenuViewAssembler;
    nmiwSearchResultsViewName: Result := lisMenuViewSearchResults;
    nmiwAnchorEditor: Result := lisMenuViewAnchorEditor;
    nmiwCodeBrowser: Result := lisCodeBrowser;
    nmiwIssueBrowser: Result := lisMenuViewRestrictionBrowser;
    nmiwJumpHistory: Result := lisMenuViewJumpHistory;
    nmiwInspect: Result := lisInspectDialog;
  else
    Result := NonModalIDEWindowNames[AWindow];
  end;
end;

class function TWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  {$I window_options.lrs}
  RegisterIDEOptionsEditor(GroupEnvironment, TWindowOptionsFrame, EnvOptionsWindow);
end.

