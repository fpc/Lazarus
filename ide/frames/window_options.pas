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
unit window_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, StdCtrls, InterfaceBase,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  EnvironmentOpts, LazarusIDEStrConsts;

type
  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoAdjustIDEHeightFullCompPalCheckBox: TCheckBox;
    lblShowingWindows: TDividerBevel;
    NameForDesignedFormList: TCheckBox;
    AutoAdjustIDEHeightCheckBox: TCheckBox;
    TitleIncludesBuildMode: TCheckBox;
    HideIDEOnRunCheckBox: TCheckBox;
    SingleTaskBarButtonCheckBox: TCheckBox;
    TitleStartsWithProjectCheckBox: TCheckBox;
    ProjectDirInIdeTitleCheckBox: TCheckBox;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TWindowOptionsFrame }

function TWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgWindow;
end;

procedure TWindowOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // windows
  lblShowingWindows.Caption := dlgShowingWindows;
  SingleTaskBarButtonCheckBox.Caption := dlgSingleTaskBarButton;
  SingleTaskBarButtonCheckBox.Enabled :=
    WidgetSet.GetLCLCapability(lcNeedMininimizeAppWithMainForm) = LCL_CAPABILITY_YES;
  SingleTaskBarButtonCheckBox.Hint:=lisShowOnlyOneButtonInTheTaskbarForTheWholeIDEInstead;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;
  HideIDEOnRunCheckBox.Hint := dlgHideIDEOnRunHint;
  TitleStartsWithProjectCheckBox.Caption:=lisIDETitleStartsWithProjectName;
  TitleStartsWithProjectCheckBox.Hint:=lisTitleInTaskbarShowsForExampleProject1LpiLazarus;
  TitleIncludesBuildMode.Caption:=lisIDETitleShowsBuildMode;
  TitleIncludesBuildMode.Hint:=lisBuildModeInTitleInExample;
  NameForDesignedFormList.Caption:=lisWindowMenuWithNameForDesignedForm;
  NameForDesignedFormList.Hint:=lisWindowMenuWithNameForDesignedFormHint;
  AutoAdjustIDEHeightCheckBox.Caption:=lisAutoAdjustIDEHeight;
  AutoAdjustIDEHeightCheckBox.Hint:=lisAutoAdjustIDEHeightHint;
  AutoAdjustIDEHeightFullCompPalCheckBox.Caption:=lisAutoAdjustIDEHeightFullComponentPalette;
  AutoAdjustIDEHeightFullCompPalCheckBox.Hint:=lisAutoAdjustIDEHeightFullComponentPaletteHint;
  ProjectDirInIdeTitleCheckBox.Caption:=lisIDETitleShowsProjectDir;
  ProjectDirInIdeTitleCheckBox.Hint:=lisProjectDirectoryIsShowedInIdeTitleBar;
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TEnvironmentOptions).Desktop do
  begin
    // window minimizing and hiding
    SingleTaskBarButtonCheckBox.Checked := SingleTaskBarButton;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    TitleStartsWithProjectCheckBox.Checked:=IDETitleStartsWithProject;
    TitleIncludesBuildMode.Checked:=IDETitleIncludesBuildMode;
    NameForDesignedFormList.Checked:=IDENameForDesignedFormList;
    AutoAdjustIDEHeightCheckBox.Checked:=AutoAdjustIDEHeight;
    AutoAdjustIDEHeightFullCompPalCheckBox.Checked:=AutoAdjustIDEHeightFullCompPal;
    ProjectDirInIdeTitleCheckBox.Checked:=IDEProjectDirectoryInIdeTitle;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TEnvironmentOptions).Desktop do
  begin
    // window minimizing
    SingleTaskBarButton := SingleTaskBarButtonCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
    IDETitleStartsWithProject:=TitleStartsWithProjectCheckBox.Checked;
    IDETitleIncludesBuildMode := TitleIncludesBuildMode.Checked;
    IDENameForDesignedFormList := NameForDesignedFormList.Checked;
    AutoAdjustIDEHeight := AutoAdjustIDEHeightCheckBox.Checked;
    AutoAdjustIDEHeightFullCompPal := AutoAdjustIDEHeightFullCompPalCheckBox.Checked;
    IDEProjectDirectoryInIdeTitle:=ProjectDirInIdeTitleCheckBox.Checked;
  end;
end;

class function TWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TWindowOptionsFrame, EnvOptionsWindow);
end.

