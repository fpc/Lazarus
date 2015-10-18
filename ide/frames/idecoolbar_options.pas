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
 Author: Balázs Székely
 Abstract:
   Frame for IDE Coolbar options.
}
unit idecoolbar_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, ComCtrls, Buttons, Controls, Menus,
  Graphics, Dialogs, StdCtrls, DividerBevel, Spin,
  LazarusIDEStrConsts, IDEOptionsIntf, EnvironmentOpts, IdeCoolbarData;

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
    procedure SelectBand(const ID: integer);
    function GetSelectedBand: Integer;
    procedure ToolBarClick(Sender: TObject);
    procedure PopulateToolBar;
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

uses MainBar, ToolbarConfig;

{$R *.lfm}

{ TIdeCoolbarOptionsFrame }

function TIdeCoolbarOptionsFrame.GetTitle: string;
begin
  Result := lisCoolbarOptions;
end;

procedure TIdeCoolbarOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  dbAddConfigDelete.Caption := lisCoolbarAddConfigDelete;
  dbGeneralSettings.Caption := lisCoolbarGeneralSettings;
  bDefaultToolbar.Caption := lisCoolbarRestoreDefaults;
  bAdd.Caption := lisBtnAdd;
  bConfig.Caption := lisCoolbarConfigure;
  bDelete.Caption := lisBtnDelete;
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
  Coolbar.GrabStyle := TGrabStyle(Opts.GrabStyle);

  if not (Opts.GrabWidth in [1..50]) then
    Opts.GrabWidth := 5;
  spGrabWidth.Value := Opts.GrabWidth;
  Coolbar.GrabWidth := Opts.GrabWidth;

  if not (Opts.BorderStyle in [0..1]) then
    Opts.BorderStyle := 1;
  cbBorderStyle.ItemIndex := Opts.BorderStyle;
  Coolbar.BandBorderStyle := TBorderStyle(Opts.BorderStyle);
  EnableDisableGeneralButtons;

  // ToDo: More tests?
  if Opts.ToolBars.Count = 0 then
    FTempCoolBar.CopyFromOptions(FDefaultOptions)
  else
    FTempCoolBar.CopyFromOptions(Opts);

  PopulateToolBar;
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
var
  ABand: integer;
  AGrabber: boolean;
begin
  CoolBar.MouseToBandPos(X, Y, ABand, AGrabber);
  if ABand < 0 then
    Exit;
  if CoolBar.Bands.Items[ABand].Color <> clHighlight then
    SelectBand(ABand);
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
  Coolbar.AutosizeBands;
  tmWait.Enabled := False;
end;

procedure TIdeCoolbarOptionsFrame.spGrabWidthChange(Sender: TObject);
begin
  CoolBar.GrabWidth := TSpinEdit(Sender).Value;
  CoolBar.AutosizeBands;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.cbGrabStyleChange(Sender: TObject);
begin
  CoolBar.GrabStyle := TGrabStyle(TComboBox(Sender).ItemIndex);
  CoolBar.AutosizeBands;
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
  Coolbar.BandBorderStyle := TBorderStyle(TComboBox(Sender).ItemIndex);
  Coolbar.AutosizeBands;
  EnableDisableGeneralButtons;
end;

procedure TIdeCoolbarOptionsFrame.SelectBand(const ID: integer);
var
  I: integer;
begin
  Coolbar.Color := clDefault;
  for I := 0 to CoolBar.Bands.Count - 1 do
  begin
    if I <> ID then
    begin
      CoolBar.Bands.Items[I].Color := clDefault;
      CoolBar.Bands.Items[I].Control.Color := clDefault;
    end
    else
    begin
      CoolBar.Bands.Items[I].Color := clHighlight;
      CoolBar.Bands.Items[I].Control.Color := clHighLight;
    end;
  end;
end;

function TIdeCoolbarOptionsFrame.GetSelectedBand: Integer;
var
  I: Integer;
begin
  Result := -1;
  if CoolBar.Bands.Count = 0 then
    Exit;
  for I := 0 to CoolBar.Bands.Count - 1 do
  begin
    if CoolBar.Bands.Items[I].Color = clHighlight then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TIdeCoolbarOptionsFrame.ToolBarClick(Sender: TObject);
var
  CoolBand: TCoolBand;
begin
  CoolBand := Coolbar.Bands.FindBand(Sender as TToolBar);
  if CoolBand <> nil then
    SelectBand(CoolBand.Index);
end;

procedure TIdeCoolbarOptionsFrame.EnableDisableGeneralButtons;
begin
  bDefaultGeneral.Enabled := not FTempCoolBar.IsDefaultCoolbar;
end;

procedure TIdeCoolbarOptionsFrame.EnableDisableToolbarButtons;
var
  I: Integer;
  Selected: Boolean;
begin
  Selected := False;
  for I := 0 to Coolbar.Bands.Count - 1 do
  begin
    if Coolbar.Bands[I].Color = clHighlight then
    begin
      Selected := True;
      Break;
    end;
  end;
  bConfig.Enabled := Selected;
  bDelete.Enabled := Selected;
  bDefaultToolbar.Enabled := not FTempCoolBar.IsDefaultToolbar;
end;

procedure TIdeCoolbarOptionsFrame.PopulateToolBar;
var
  CoolBand: TCoolBand;
  I: Integer;
begin
  CoolBar.Bands.Clear;
  for I := 0 to FTempCoolBar.ToolBars.Count - 1 do
  begin
    CoolBand := CoolBar.Bands.Add;
    CoolBand.Break := FTempCoolBar.ToolBars[I].CurrentOptions.Break;
    CoolBand.Control := FTempCoolBar.ToolBars[I].Toolbar;
    FTempCoolBar.ToolBars[I].Toolbar.Enabled := False;
    CoolBand.Visible := True;
    CoolBand.MinWidth := 25;
    CoolBand.MinHeight := 22;
    CoolBand.FixedSize := True;
    FTempCoolBar.ToolBars[I].UseCurrentOptions;
  end;
  if CoolBar.Bands.Count > 0 then
    SelectBand(0);
  Coolbar.AutosizeBands;
  EnableDisableToolbarButtons;
end;

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

procedure TIdeCoolbarOptionsFrame.bAddClick(Sender: TObject);
var
  CoolBand: TCoolBand;
  IDEToolbar: TIDEToolBar;
begin
  IDEToolbar := FTempCoolBar.Add;
  IDEToolbar.CurrentOptions.Break := False;
  IDEToolbar.OnToolBarClick := @ToolBarClick;

  CoolBand := CoolBar.Bands.Add;
  CoolBand.Break := True;
  CoolBand.Control := IDEToolbar.Toolbar;
  IDEToolbar.Toolbar.Enabled := False;
  CoolBand.Visible := True;
  CoolBand.MinWidth := 25;
  CoolBand.MinHeight := 22;
  CoolBand.FixedSize := True;
  SelectBand(CoolBand.Index);
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.bConfigClick(Sender: TObject);
var
  ToConfig: Integer;
  ToolBar: TToolBar;
begin
  ToConfig := GetSelectedBand;
  if ToConfig = -1 then
  begin
    MessageDlg(lisCoolbarSelectToolBar, mtInformation, [mbOk], 0);
    Exit;
  end;
  ToolBar := (Coolbar.Bands.Items[ToConfig].Control as TToolBar);
  if ToolBar <> nil then
  begin
    ToConfig := FTempCoolBar.FindByToolBar(ToolBar);
    if (ToConfig <> -1)
    and (ShowToolBarConfig(FTempCoolBar.ToolBars[ToConfig].CurrentOptions.ButtonNames) = mrOK)
    then
      FTempCoolBar.ToolBars[ToConfig].UseCurrentOptions;
  end;
  Coolbar.AutosizeBands;
  EnableDisableToolbarButtons;
end;

procedure TIdeCoolbarOptionsFrame.bDeleteClick(Sender: TObject);
var
  I: integer;
  ToDelete: integer;
begin
  if Coolbar.Bands.Count = 1 then
  begin
    MessageDlg(lisCoolbarDeleteWarning, mtInformation, [mbOk], 0);
    Exit;
  end;
  ToDelete := GetSelectedBand;
  if ToDelete > -1 then
  begin
    if MessageDlg(lisCoolbarDeleteToolBar, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if ToDelete + 1 < CoolBar.Bands.Count then
        SelectBand(ToDelete + 1)
      else if ToDelete - 1 >= 0 then
        SelectBand(ToDelete - 1);
      I := FTempCoolBar.FindByToolBar((CoolBar.Bands.Items[ToDelete].Control as TToolBar));
      if I <> -1 then
        FTempCoolBar.ToolBars.Delete(I);
      CoolBar.Bands.Delete(ToDelete);
    end;
  end;
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
  PopulateToolBar;
end;


initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TIdeCoolbarOptionsFrame, EnvOptionsToolbar);

end.

