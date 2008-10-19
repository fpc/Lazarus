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
unit options_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls,
  EnvironmentOpts, LazarusIDEStrConsts, IDEOptionDefs, ObjectInspector;

type

  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractOptionsFrame)
    HideIDEOnRunCheckBox: TCheckBox;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TIDEWindowLayoutList;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;
    procedure SetWindowPositionsItem(Index: integer);
  public
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup; override;
    procedure ReadSettings(AOptions: TEnvironmentOptions); override;
    procedure WriteSettings(AOptions: TEnvironmentOptions); override;
  end;

implementation

{ TWindowOptionsFrame }

function TWindowOptionsFrame.Check: Boolean;
begin
  Result := True;
end;

function TWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgWindow;
end;

procedure TWindowOptionsFrame.Setup;
begin
  // windows
  MinimizeAllOnMinimizeMainCheckBox.Caption := dlgMinimizeAllOnMinimizeMain;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;

  // Window Positions
  WindowPositionsGroupBox.Caption := dlgWinPos;
  with WindowPositionsListBox.Items do
  begin
    BeginUpdate;
    Add(dlgMainMenu);
    Add(dlgSrcEdit);
    Add(dlgMsgs);
    Add(dlgObjInsp);
    Add(lisMenuProjectInspector);
    Add(lisCodeExplorer);
    Add(lisMenuPackageGraph);
    Add(dlgUnitDepCaption);
    Add(lisMenuFPDocEditor);
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

procedure TWindowOptionsFrame.ReadSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
  begin
    FLayouts := IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked := MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TEnvironmentOptions);
begin
  with AOptions do
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
  case Index of
    0: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwMainIDEName);
    1: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwSourceNoteBookName);
    2: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwMessagesViewName);
    3: WindowPositionsBox.Layout := FLayouts.ItemByFormID(DefaultObjectInspectorName);
    4: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwProjectInspector);
    5: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwCodeExplorerName);
    6: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwPkgGraphExplorer);
    7: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwUnitDependenciesName);
    8: WindowPositionsBox.Layout := FLayouts.ItemByEnum(nmiwFPDocEditorName);
  end;
  if Index >= 0 then
    WindowPositionsBox.Caption:=WindowPositionsListBox.Items[Index];
end;

initialization
  {$I options_window.lrs}

end.

