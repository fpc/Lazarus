{**********************************************************************
          Copyright (c) PilotLogic Software House
                   All rights reserved

 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

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
 ***************************************************************************}

unit embedded_designer_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  FileUtil, Forms, Controls, Graphics, StdCtrls, ColorBox, Dialogs,
  //IDE intf
  IDEOptionsIntf, SrcEditorIntf, IDEOptEditorIntf, IDEWindowIntf,
  //IDE
  LazarusIDEStrConsts, EnvironmentOpts, IDEDialogs;

type

  { TIDEEmbeddedDesignerSettingsFrame }

  TIDEEmbeddedDesignerSettingsFrame = class(TAbstractIDEOptionsEditor)
    ButtonSetDafault: TButton;
    grOptions: TGroupBox;
    UseDesignerBackColorClrBox: TColorBox;
    UseDesignerBackColorText: TLabel;
    UseEmbeddedDesignerCheckBox: TCheckBox;
    UseEmbeddedScreenPreviewCheckBox: TCheckBox;
    UseInfopanelColorCheckText: TLabel;
    UseInfopanelColorClrBox: TColorBox;
    UseSreenPreviewBackColorClrBox: TColorBox;
    UseSreenPreviewBackColorText: TLabel;
    procedure ButtonSetDafaultClick(Sender: TObject);
    procedure UseEmbeddedDesignerCheckBoxClick(Sender: TObject);
  private
    FOldUseEmbeddedDesigner: Boolean;
    procedure EnableDisableControls(const AEnable: Boolean);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function TIDEEmbeddedDesignerSettingsFrame.GetTitle: String;
begin
  Result := lisEmbDsgn_Title;
end;

procedure TIDEEmbeddedDesignerSettingsFrame.ButtonSetDafaultClick(Sender: TObject);
begin
  UseEmbeddedDesignerCheckBox.Checked := False;
  UseEmbeddedScreenPreviewCheckBox.Checked := True;
  UseDesignerBackColorClrBox.Selected := clWhite;
  UseSreenPreviewBackColorClrBox.Selected := $00A56E3A;
  UseInfopanelColorClrBox.Selected := $00EAFFEF;
  EnableDisableControls(UseEmbeddedDesignerCheckBox.Checked);
end;

procedure TIDEEmbeddedDesignerSettingsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  UseEmbeddedDesignerCheckBox.Caption := lisEmbDsgn_EnableEmbeddedDesigner;
  grOptions.Caption := lisEmbDsgn_Options;
  UseEmbeddedScreenPreviewCheckBox.Caption := lisEmbDsgn_ShowScreenPreview;
  UseDesignerBackColorText.Caption := lisEmbDsgn_BackColor;
  UseSreenPreviewBackColorText.Caption := lisEmbDsgn_SreenPreviewBackColor;
  UseInfopanelColorCheckText.Caption := lisEmbDsgn_InfopanelColor;
  ButtonSetDafault.Caption := lisEmbDsgn_SetDefault;
end;

procedure TIDEEmbeddedDesignerSettingsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TEnvironmentOptions) do
  begin
    UseEmbeddedDesignerCheckBox.Checked := UseEmbeddedDesigner;
    UseEmbeddedScreenPreviewCheckBox.Checked := UseEmbeddedScreenPreview;
    UseDesignerBackColorClrBox.Selected := UseEmbeddedDesignerBackColor;
    UseSreenPreviewBackColorClrBox.Selected := UseEmbeddedSreenPreviewBackColor;
    UseInfopanelColorClrBox.Selected := UseEmbeddedInfopanelColor;
  end;
  EnableDisableControls(UseEmbeddedDesignerCheckBox.Checked);
  FOldUseEmbeddedDesigner := (AOptions as TEnvironmentOptions).UseEmbeddedDesigner;
end;

procedure TIDEEmbeddedDesignerSettingsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TEnvironmentOptions) do
  begin
    UseEmbeddedDesigner := UseEmbeddedDesignerCheckBox.Checked;
    UseEmbeddedScreenPreview := UseEmbeddedScreenPreviewCheckBox.Checked;
    UseEmbeddedDesignerBackColor := UseDesignerBackColorClrBox.Selected;
    UseEmbeddedSreenPreviewBackColor:= UseSreenPreviewBackColorClrBox.Selected;
    UseEmbeddedInfopanelColor := UseInfopanelColorClrBox.Selected;
  end;
  if Assigned(SourceEditorManagerIntf) then
    SourceEditorManagerIntf.UpdateEmbedFormDsgSettings;
  if FOldUseEmbeddedDesigner <> (AOptions as TEnvironmentOptions).UseEmbeddedDesigner then
    IDEMessageDialog(lisInformation, lisEmbDsgn_RestartIDEText, mtInformation, [mbOk]);
end;

procedure TIDEEmbeddedDesignerSettingsFrame.EnableDisableControls(
  const AEnable: Boolean);
begin
  UseDesignerBackColorText.Enabled := AEnable;
  UseDesignerBackColorClrBox.Enabled := AEnable;
  UseSreenPreviewBackColorText.Enabled := AEnable;
  UseSreenPreviewBackColorClrBox.Enabled := AEnable;
  UseInfopanelColorCheckText.Enabled := AEnable;
  UseInfopanelColorClrBox.Enabled := AEnable;
  UseEmbeddedScreenPreviewCheckBox.Enabled := AEnable;
end;

procedure TIDEEmbeddedDesignerSettingsFrame.UseEmbeddedDesignerCheckBoxClick(
  Sender: TObject);
begin
  EnableDisableControls(UseEmbeddedDesignerCheckBox.Checked);
end;

class function TIDEEmbeddedDesignerSettingsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TIDEEmbeddedDesignerSettingsFrame, EnvOptionsEmbeddedDsg);

end.

