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
 Author: Balazs Szekely
 Abstract:
  Frame for toolbar options.
}
unit toolbar_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, ExtCtrls, StdCtrls,
  IDEOptionsIntf, LazarusIDEStrConsts, EnvironmentOpts, GraphType;

type

  { TToolbarOptionsFrame }

  TToolbarOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbToolbarsVisibleCheckBox: TCheckBox;
    cbHighlightCheckBox: TCheckBox;
    cbRaisedCheckBox: TCheckBox;
    cbStandardVisibleCheckBox: TCheckBox;
    cbViewDebugVisibleCheckBox: TCheckBox;
    gbToolbarOptions: TGroupBox;
    procedure cbToolbarsVisibleCheckBoxClick(Sender: TObject);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation
uses MainBar;
{$R *.lfm}

{ TToolbarOptionsFrame }

procedure TToolbarOptionsFrame.cbToolbarsVisibleCheckBoxClick(
  Sender: TObject);
begin
  cbStandardVisibleCheckBox.Enabled := cbToolbarsVisibleCheckBox.Checked;
  cbViewDebugVisibleCheckBox.Enabled := cbToolbarsVisibleCheckBox.Checked;
end;

function TToolbarOptionsFrame.GetTitle: String;
begin
  Result := lisToolbarOptions;
end;

procedure TToolbarOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  gbToolbarOptions.Caption := lisToolbarOptions;
  cbToolbarsVisibleCheckBox.Caption := lisToolbarOptionsToolbarsVisible;
  cbStandardVisibleCheckBox.Caption := lisToolbarOptionsStandardVisible;
  cbViewDebugVisibleCheckBox.Caption := lisToolbarOptionsViewDebugVisible;
  cbHighlightCheckBox.Caption := lisToolbarOptionsHighLight;
  cbRaisedCheckBox.Caption := lisToolbarOptionsRaise;
end;

procedure TToolbarOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    cbToolbarsVisibleCheckBox.Checked := ToolbarVisible;
    cbStandardVisibleCheckBox.Enabled := cbToolbarsVisibleCheckBox.Checked;
    cbStandardVisibleCheckBox.Checked := ToolBarStandardVisible;
    cbViewDebugVisibleCheckBox.Enabled := cbToolbarsVisibleCheckBox.Checked;
    cbViewDebugVisibleCheckBox.Checked := ToolBarViewDebugVisible;
    cbHighlightCheckBox.Checked := ToolBarHighlight;
    cbRaisedCheckBox.Checked := ToolBarRaised;
  end;
end;

procedure TToolbarOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    //toolbar general visibility
    ToolbarVisible := cbToolbarsVisibleCheckBox.Checked;
    MainIDEBar.itmViewIDESpeedButtons.Checked := ToolbarVisible;
    MainIDEBar.pnlSpeedButtons.Visible := ToolbarVisible;

    MainIDEBar.ToolBarsVisibleCount := 0;
    //standard
    ToolBarStandardVisible := cbStandardVisibleCheckBox.Checked;
    MainIDEBar.pnStandard.Visible := ToolBarStandardVisible;
    if ToolBarStandardVisible then
      MainIDEBar.ToolBarsVisibleCount := MainIDEBar.ToolBarsVisibleCount + 1;
    //viewDebug
    ToolBarViewDebugVisible := cbViewDebugVisibleCheckBox.Checked;
    MainIDEBar.pnViewDebug.Visible := ToolBarViewDebugVisible;
    if ToolBarViewDebugVisible then
      MainIDEBar.ToolBarsVisibleCount := MainIDEBar.ToolBarsVisibleCount + 1;
    //highlight
    ToolBarHighlight := cbHighlightCheckBox.Checked;
    MainIDEBar.tbStandard.Flat := not ToolBarHighlight;
    MainIDEBar.tbViewDebug.Flat := not ToolBarHighlight;
    //raised
    ToolBarRaised := cbRaisedCheckBox.Checked;
    if ToolBarRaised then
    begin
      MainIDEBar.pnStandard.BevelOuter := bvRaised;
      MainIDEBar.pnViewDebug.BevelOuter := bvRaised;
    end
    else
    begin
      MainIDEBar.pnStandard.BevelOuter := bvNone;
      MainIDEBar.pnViewDebug.BevelOuter := bvNone;
    end;
  end;
  MainIDEBar.SetToolbarsPositions;
end;

class function TToolbarOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TToolbarOptionsFrame, EnvOptionsToolbar);

end.

