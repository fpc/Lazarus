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
 Author: Juha Manninen
 Abstract:
   Frame for editortoolbar options.
}
unit editortoolbar_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, ExtCtrls, Buttons, Controls, StdCtrls,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, EditorToolbarStatic, ToolbarConfig;

type

  { TEditorToolbarOptionsFrame }

  TEditorToolbarOptionsFrame = class(TAbstractIDEOptionsEditor)
    bConfig: TBitBtn;
    bDefaultToolbar: TBitBtn;
    cbCoolBarVisible: TCheckBox;
    cbPos: TComboBox;
    imButtons: TImageList;
    dbGeneralSettings: TDividerBevel;
    lblNoAutoSaveActiveDesktop: TLabel;
    lblpos: TLabel;
    pnTopCenterLabel: TLabel;
    pnTop: TPanel;
    procedure bConfigClick(Sender: TObject);
    procedure bDefaultToolbarClick(Sender: TObject);
    procedure cbCoolBarVisibleClick(Sender: TObject);
    procedure cbPosChange(Sender: TObject);
  private
    FLocalOptions: TEditorToolBarOptions;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

var
  sPosValues: array[0..3] of string = ('Top','Bottom','Right','Left');
  sLocalizedPosValues: array[0..3] of string;


implementation

{$R *.lfm}

function IndexFromEnglish (AValue: string): Integer;
var
  i: Integer;
begin
  for i := 0 to 3 do
    if AValue = sPosValues[i] then
      exit(i);
  Result := 0; // default is Top
end;

{ TEditorToolbarOptionsFrame }

function TEditorToolbarOptionsFrame.GetTitle: string;
begin
  Result := lisEditorToolbar;
end;

procedure TEditorToolbarOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  i: Integer;
begin
  sLocalizedPosValues[0] := lisTop;
  sLocalizedPosValues[1] := lisBottom;
  sLocalizedPosValues[2] := lisRight;
  sLocalizedPosValues[3] := lisLeft;
  for i := 0 to high(sLocalizedPosValues) do
    cbPos.Items[i] := sLocalizedPosValues[i]; // localized
  cbPos.Caption := cbPos.Items[cbPos.ItemIndex];
  lblNoAutoSaveActiveDesktop.Caption := lisNoAutoSaveActiveDesktop;

  dbGeneralSettings.Caption := lisEditorToolbarSettings; // ToDo: Will be removed ...
  cbCoolBarVisible.Caption := lisEditorToolbarVisible;
  lblpos.Caption := lisPosition;
  bDefaultToolbar.Caption := lisCmpRestoreDefaults;
  IDEImages.AssignImage(bDefaultToolbar, 'restore_defaults');
  bConfig.Caption := lisCoolbarConfigure;
  IDEImages.AssignImage(bConfig, 'preferences');
end;

procedure TEditorToolbarOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TEditorToolBarOptions;
begin
  Opts := (AOptions as TEnvironmentOptions).Desktop.EditorToolBarOptions;
  cbCoolBarVisible.Checked := Opts.Visible;
  cbPos.ItemIndex := IndexFromEnglish(Opts.Position);
  lblNoAutoSaveActiveDesktop.Visible := not EnvironmentOptions.AutoSaveActiveDesktop;
  // Disable controls when toolbar is hidden.
  cbPos.Enabled := Opts.Visible;
  bConfig.Enabled := Opts.Visible;
  bDefaultToolbar.Enabled := Opts.Visible;
  // Copy from environment options to local options.
  FLocalOptions.Assign(Opts);
end;

procedure TEditorToolbarOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TEditorToolBarOptions;
begin
  Opts := (AOptions as TEnvironmentOptions).Desktop.EditorToolBarOptions;
  Opts.Assign(FLocalOptions);
end;

class function TEditorToolbarOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

procedure TEditorToolbarOptionsFrame.cbCoolBarVisibleClick(Sender: TObject);
var
  chk: Boolean;
begin
  chk := (Sender as TCheckBox).Checked;
  FLocalOptions.Visible := chk;
  // Disable controls when toolbar is hidden.
  cbPos.Enabled := chk;
  bConfig.Enabled := chk;
  bDefaultToolbar.Enabled := chk;
end;

procedure TEditorToolbarOptionsFrame.cbPosChange(Sender: TObject);
begin
  DebugLn(['TEditorToolbarOptionsFrame.cbPosChange: cbPos.ItemIndex=', cbPos.ItemIndex]);
  if cbPos.ItemIndex >= 0 then
    FLocalOptions.Position := sPosValues[cbPos.ItemIndex];
end;

constructor TEditorToolbarOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocalOptions := TEditorToolBarOptions.Create;
end;

destructor TEditorToolbarOptionsFrame.Destroy;
begin
  FreeAndNil(FLocalOptions);
  inherited Destroy;
end;

procedure TEditorToolbarOptionsFrame.bConfigClick(Sender: TObject);
begin
  if ShowToolBarConfig(FLocalOptions.ButtonNames) = mrOK then
    ;  // Do Nothing, changed options were copied.
end;

procedure TEditorToolbarOptionsFrame.bDefaultToolbarClick(Sender: TObject);
begin
  FLocalOptions.CreateDefaults;
end;


initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TEditorToolbarOptionsFrame, EnvOptionsEditorToolbar);

end.

