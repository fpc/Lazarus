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
 Author: Balázs Székely
 Abstract:
   Frame for IDE Coolbar options.
}
unit idecoolbar_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, ExtCtrls, ComCtrls, Buttons, Controls, StdCtrls, Spin,
  // LazControls
  DividerBevel,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, MainBar, EnvironmentOpts, IdeCoolbarData;

type

  { TIdeCoolbarOptionsFrame }

  TIdeCoolbarOptionsFrame = class(TAbstractIDEOptionsEditor)
    bAdd: TBitBtn;
    bDefaultGeneral: TBitBtn;
    bConfig: TBitBtn;
    bDefaultToolbar: TBitBtn;
    bDelete: TBitBtn;
    cbGrabStyle: TComboBox;
    cbBorderStyle: TComboBox;
    cbCoolBarVisible: TCheckBox;
    Coolbar: TCoolBar;
    gbGrabStyle: TGroupBox;
    gbBorderStyle: TGroupBox;
    imButtons: TImageList;
    dbAddConfigDelete: TDividerBevel;
    dbGeneralSettings: TDividerBevel;
    lblNoAutoSaveActiveDesktop: TLabel;
    pnTopCenterLabel: TLabel;
    lbGrabWidth: TLabel;
    lbCoolBarWidth: TLabel;
    pnTop: TPanel;
    pnBottom: TPanel;
    pnButtons: TPanel;
    sbCoolBar: TScrollBox;
    spGrabWidth: TSpinEdit;
    spCoolBarWidth: TSpinEdit;
    tmWait: TTimer;
    procedure bAddClick(Sender: TObject);
    procedure bConfigClick(Sender: TObject);
    procedure bDefaultGeneralClick(Sender: TObject);
    procedure bDefaultToolbarClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure cbBorderStyleChange(Sender: TObject);
    procedure cbGrabStyleChange(Sender: TObject);
    procedure cbCoolBarVisibleClick(Sender: TObject);
    procedure CoolbarChange(Sender: TObject);
    procedure CoolBarMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: integer);
    procedure CoolbarResize(Sender: TObject);
    procedure spCoolBarWidthChange(Sender: TObject);
    procedure spGrabWidthChange(Sender: TObject);
    procedure tmWaitTimer(Sender: TObject);
  private
    FTempCoolBar: TIDECoolBar;
    FTempCoolBarOptions: TIDECoolBarOptions;
    // Used for assigning and testing the default configuration.
    FDefaultOptions: TDefaultCoolBarOptions;
    procedure EnableDisableGeneralButtons;
    procedure EnableDisableToolbarButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;


implementation

{$R *.lfm}

{ TIdeCoolbarOptionsFrame }

constructor TIdeCoolbarOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTempCoolBar := TIDEcoolBar.Create(Coolbar);
  FTempCoolBarOptions := TIDECoolBarOptions.Create;
  FDefaultOptions := TDefaultCoolBarOptions.Create;
end;

destructor TIdeCoolbarOptionsFrame.Destroy;
begin
  FreeAndNil(FDefaultOptions);
  FreeAndNil(FTempCoolBarOptions);
  FreeAndNil(FTempCoolBar);
  inherited Destroy;
end;

function TIdeCoolbarOptionsFrame.GetTitle: string;
begin
  Result := lisCoolbarOptions;
end;

procedure TIdeCoolbarOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  dbGeneralSettings.Caption := lisCoolbarGeneralSettings;
  cbCoolBarVisible.Caption := lisCoolbarVisible;
  lbCoolBarWidth.Caption := lisCoolbarWidth;
  gbGrabStyle.Caption := lisCoolbarGrabStyle;
  cbGrabStyle.Items.Strings[0] := lisCoolbarGrabStyleItem0;
  cbGrabStyle.Items.Strings[1] := lisCoolbarGrabStyleItem1;
  cbGrabStyle.Items.Strings[2] := lisCoolbarGrabStyleItem2;
  cbGrabStyle.Items.Strings[3] := lisCoolbarGrabStyleItem3;
  cbGrabStyle.Items.Strings[4] := lisCoolbarGrabStyleItem4;
  cbGrabStyle.Items.Strings[5] := lisCoolbarGrabStyleItem5;
  lbGrabWidth.Caption := lisCoolbarGrabWidth;
  gbBorderStyle.Caption := lisCoolbarBorderStyle;
  cbBorderStyle.Items.Strings[0] := lisCoolbarBorderStyleItem0;
  cbBorderStyle.Items.Strings[1] := lisCoolbarBorderStyleItem1;
  bDefaultGeneral.Caption := lisCoolbarRestoreDefaults;
  IDEImages.AssignImage(bDefaultGeneral, 'restore_default');

  dbAddConfigDelete.Caption := lisCoolbarAddConfigDelete;
  bAdd.Caption := lisBtnAdd;
  IDEImages.AssignImage(bAdd, 'laz_add');
  bConfig.Caption := lisCoolbarConfigure;
  IDEImages.AssignImage(bConfig, 'preferences');
  bDelete.Caption := lisBtnDelete;
  IDEImages.AssignImage(bDelete, 'laz_delete');
  bDefaultToolbar.Caption := lisCoolbarRestoreDefaults;
  IDEImages.AssignImage(bDefaultToolbar, 'restore_defaults');
  lblNoAutoSaveActiveDesktop.Caption := lisNoAutoSaveActiveDesktop;
end;

procedure TIdeCoolbarOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TIDECoolBarOptions;
begin
  Opts := (AOptions as TEnvironmentOptions).Desktop.IDECoolBarOptions;
  cbCoolBarVisible.Checked := Opts.Visible;
  FTempCoolBar.IsVisible := Opts.Visible;

  spCoolBarWidth.Value := Opts.Width;
  FTempCoolBar.Width := Opts.Width;

  if not (Opts.GrabStyle in [0..5]) then
    Opts.GrabStyle := 1;
  cbGrabStyle.ItemIndex := Opts.GrabStyle;
  FTempCoolBar.Coolbar.GrabStyle := TGrabStyle(Opts.GrabStyle);

  if not (Opts.GrabWidth in [1..50]) then
    Opts.GrabWidth := 5;
  spGrabWidth.Value := Opts.GrabWidth;
  FTempCoolBar.Coolbar.GrabWidth := Opts.GrabWidth;

  if not (Opts.BorderStyle in [0..1]) then
    Opts.BorderStyle := 1;
  cbBorderStyle.ItemIndex := Opts.BorderStyle;
  FTempCoolBar.Coolbar.BandBorderStyle := TBorderStyle(Opts.BorderStyle);
  EnableDisableGeneralButtons;
  lblNoAutoSaveActiveDesktop.Visible := not EnvironmentOptions.AutoSaveActiveDesktop;

  // ToDo: More tests?
  if Opts.ToolBars.Count = 0 then
    FTempCoolBar.CopyFromOptions(FDefaultOptions)
  else
    FTempCoolBar.CopyFromOptions(Opts);

  FTempCoolBar.PopulateToolBar;
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TIDECoolBarOptions;
begin
  Opts := (AOptions as TEnvironmentOptions).Desktop.IDECoolBarOptions;
  FTempCoolBar.CopyFromRealCoolbar(Coolbar);
  FTempCoolBar.CopyToOptions(Opts);
  Opts.Visible := cbCoolBarVisible.Checked;
  Opts.Width := FTempCoolBar.Width;
  Opts.GrabStyle := cbGrabStyle.ItemIndex;
  Opts.GrabWidth := spGrabWidth.Value;
  Opts.BorderStyle := cbBorderStyle.ItemIndex;
  MainIDEBar.RefreshCoolbar;
  MainIDEBar.SetMainIDEHeight;
end;

class function TIdeCoolbarOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

procedure TIdeCoolbarOptionsFrame.CoolBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FTempCoolBar.SelectBandAtXY(X, Y);
end;

procedure TIdeCoolbarOptionsFrame.CoolbarResize(Sender: TObject);
begin
  if tmWait.Enabled then
    Exit;
  tmWait.Enabled := True;
end;

procedure TIdeCoolbarOptionsFrame.spCoolBarWidthChange(Sender: TObject);
begin
  FTempCoolBar.Width := spCoolBarWidth.Value;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.tmWaitTimer(Sender: TObject);
begin
  FTempCoolBar.Coolbar.AutosizeBands;
  tmWait.Enabled := False;
end;

procedure TIdeCoolbarOptionsFrame.spGrabWidthChange(Sender: TObject);
begin
  FTempCoolBar.CoolBar.GrabWidth := TSpinEdit(Sender).Value;
  FTempCoolBar.CoolBar.AutosizeBands;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.cbGrabStyleChange(Sender: TObject);
begin
  FTempCoolBar.CoolBar.GrabStyle := TGrabStyle(TComboBox(Sender).ItemIndex);
  FTempCoolBar.CoolBar.AutosizeBands;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.cbCoolBarVisibleClick(Sender: TObject);
begin
  FTempCoolBar.IsVisible := cbCoolBarVisible.Checked;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.CoolbarChange(Sender: TObject);
begin
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.cbBorderStyleChange(Sender: TObject);
begin
  FTempCoolBar.Coolbar.BandBorderStyle := TBorderStyle(TComboBox(Sender).ItemIndex);
  FTempCoolBar.Coolbar.AutosizeBands;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.EnableDisableGeneralButtons;
begin
  bDefaultGeneral.Enabled := not FTempCoolBar.IsDefaultCoolbar;
end;

procedure TIdeCoolbarOptionsFrame.EnableDisableToolbarButtons;
var
  IsSel: Boolean;
begin
  IsSel := FTempCoolBar.GetSelectedBand > -1;
  bConfig.Enabled := IsSel;
  bDelete.Enabled := IsSel;
  bDefaultToolbar.Enabled := not FTempCoolBar.IsDefaultToolbar;
end;

procedure TIdeCoolbarOptionsFrame.bAddClick(Sender: TObject);
begin
  FTempCoolBar.AddExtra;
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.bConfigClick(Sender: TObject);
begin
  FTempCoolBar.Config;
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.bDeleteClick(Sender: TObject);
begin
  FTempCoolBar.Delete;
  // CoolBar Band gets deleted at the same go with FTempCoolBar item.
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.bDefaultGeneralClick(Sender: TObject);
begin
  cbCoolBarVisible.Checked := True;
  FTempCoolBar.IsVisible := True;
  spCoolBarWidth.Value := 230;
  FTempCoolBar.Width := 230;
  cbGrabStyle.ItemIndex := 1;
  spGrabWidth.Value := 5;
  BiDiMode := bdLeftToRight;
  cbBorderStyle.ItemIndex := 1;
  FTempCoolBar.SetCoolBarDefaults;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.bDefaultToolbarClick(Sender: TObject);
begin
  FTempCoolBar.SetToolBarDefaults;
  FTempCoolBar.PopulateToolBar;
  EnableDisableToolbarButtons;
end;


initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TIdeCoolbarOptionsFrame, EnvOptionsToolbar);

end.

