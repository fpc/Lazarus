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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls,
  EnvironmentOpts, LazarusIDEStrConsts, IDEOptionDefs, ObjectInspector, IDEOptionsIntf;

type

  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    HideIDEOnRunCheckBox: TCheckBox;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TIDEWindowLayoutList;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;
    procedure SetWindowPositionsItem(Index: integer);
    function GetCaptionFor(AWindow: TNonModalIDEWindow): String;
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
  WindowPositionsBox := TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do
  begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
    BorderSpacing.Around:=6;
    Align:=alBottom;
    AnchorToNeighbour(akTop,6,WindowPositionsListBox);
  end;
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
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    WindowPositionsBox.Save;
    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
  end;
end;

procedure TWindowOptionsFrame.WindowPositionsListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TWindowOptionsFrame.SetWindowPositionsItem(Index: integer);
begin
  if WindowPositionsBox.Layout <> nil then
    WindowPositionsBox.Save;
  WindowPositionsListBox.ItemIndex := Index;
  if Index < Ord(High(TNonModalIDEWindow)) then
    WindowPositionsBox.Layout := FLayouts.ItemByEnum(TNonModalIDEWindow(Index + 1))
  else
  begin
    case Index - Ord(High(TNonModalIDEWindow)) of
      0: WindowPositionsBox.Layout := FLayouts.ItemByFormID(DefaultObjectInspectorName);
    end;
  end;
  if Index >= 0 then
    WindowPositionsBox.Caption := WindowPositionsListBox.Items[Index];
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

