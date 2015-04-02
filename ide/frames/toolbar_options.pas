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
  Frame for toolbar options.
}
unit toolbar_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, ComCtrls, Buttons, Controls, Menus,
  Graphics, Dialogs, StdCtrls, DividerBevel, Spin,
  LazarusIDEStrConsts, IDEOptionsIntf, EnvironmentOpts, ToolbarData;

type

  { TToolbarOptionsFrame }

  TToolbarOptionsFrame = class(TAbstractIDEOptionsEditor)
    bAdd: TBitBtn;
    bDefaultGeneral: TBitBtn;
    bConfig: TBitBtn;
    bDefaultToolbar: TBitBtn;
    bDelete: TBitBtn;
    cbGrabStyle: TComboBox;
    cbBorderStyle: TComboBox;
    cbToolBarVisible: TCheckBox;
    Coolbar: TCoolBar;
    gbGrabStyle: TGroupBox;
    gbBorderStyle: TGroupBox;
    imButtons: TImageList;
    dbAddConfigDelete: TDividerBevel;
    dbGeneralSettings: TDividerBevel;
    lbGrabWidth: TLabel;
    pnTop: TPanel;
    pnBottom: TPanel;
    pnButtons: TPanel;
    sbCoolBar: TScrollBox;
    spGrabWidth: TSpinEdit;
    tmWait: TTimer;
    procedure bAddClick(Sender: TObject);
    procedure bConfigClick(Sender: TObject);
    procedure bDefaultGeneralClick(Sender: TObject);
    procedure bDefaultToolbarClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure cbBorderStyleChange(Sender: TObject);
    procedure cbGrabStyleChange(Sender: TObject);
    procedure cbToolBarVisibleClick(Sender: TObject);
    procedure CoolBarMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: integer);
    procedure CoolbarResize(Sender: TObject);
    procedure spGrabWidthChange(Sender: TObject);
    procedure tmWaitTimer(Sender: TObject);
  private
    FTempCoolBar: TIDECoolBar;
    procedure SelectBand(const ID: integer);
    function GetSelectedBand: Integer;
    procedure ToolBarClick(Sender: TObject);
    procedure PopulateToolBar;
    procedure EnableDisableButtons(const bType: Integer);
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

{ TToolbarOptionsFrame }

function TToolbarOptionsFrame.GetTitle: string;
begin
  Result := lisCoolbarOptions;
end;

procedure TToolbarOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  dbAddConfigDelete.Caption := lisCoolbarAddConfigDelete;
  dbGeneralSettings.Caption := lisCoolbarGeneralSettings;
  bDefaultToolbar.Caption := lisCoolbarRestoreDefaults;
  bAdd.Caption := lisBtnAdd;
  bConfig.Caption := lisCoolbarConfigure;
  bDelete.Caption := lisBtnDelete;
  cbToolBarVisible.Caption := lisCoolbarVisible;
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

procedure TToolbarOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  I, J: integer;
  IDEToolBar: TIDEToolBar;
begin
  with (AOptions as TEnvironmentOptions).IDECoolBarOptions do
  begin
     //read toolbar settings
    cbToolBarVisible.Checked := IDECoolBarVisible;

    if not (IDECoolBarGrabStyle in [0..5]) then
      IDECoolBarGrabStyle := 1;
    cbGrabStyle.ItemIndex := IDECoolBarGrabStyle;
    Coolbar.GrabStyle := TGrabStyle(IDECoolBarGrabStyle);

    if not (IDECoolBarGrabWidth in [1..50]) then
      IDECoolBarGrabWidth := 5;
    spGrabWidth.Value := IDECoolBarGrabWidth;
    Coolbar.GrabWidth := IDECoolBarGrabWidth;

    if not (IDECoolBarBorderStyle in [0..1]) then
      IDECoolBarBorderStyle := 1;
    cbBorderStyle.ItemIndex := IDECoolBarBorderStyle;
    Coolbar.BandBorderStyle := TBorderStyle(IDECoolBarBorderStyle);
    EnableDisableButtons(0);

    //read toolbars
    FTempCoolBar.ToolBars.Clear;
    for I := 0 to IDECoolBar.ToolBars.Count - 1  do
    begin
      IDEToolBar := FTempCoolBar.Add;
      IDEToolBar.Position := IDECoolBar.ToolBars[I].Position;
      IDEToolBar.Break := IDECoolBar.ToolBars[I].Break;
      for J := 0 to IDECoolBar.ToolBars[I].ButtonNames.Count - 1 do
        IDEToolBar.ButtonNames.Add(IDECoolBar.ToolBars[I].ButtonNames.Strings[J]);
    end;
    FTempCoolBar.Sort;

    PopulateToolBar;
  end;
end;

procedure TToolbarOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  I, J: integer;
  IDEToolBar: TIDEToolBar;
  ToolBar: TToolBar;
begin
  with (AOptions as TEnvironmentOptions).IDECoolBarOptions do
  begin
    //write toolbar settings
    IDECoolBarVisible := cbToolBarVisible.Checked;
    IDECoolBarGrabStyle := cbGrabStyle.ItemIndex;
    IDECoolBarGrabWidth := spGrabWidth.Value;
    IDECoolBarBorderStyle := cbBorderStyle.ItemIndex;

    //write toolbars
    for I := 0 to Coolbar.Bands.Count - 1 do
    begin
      if Coolbar.Bands[I].Control = nil then
        Continue;
      ToolBar := (Coolbar.Bands[I].Control as TToolBar);
      J := FTempCoolBar.FindByToolBar(ToolBar);
      if J <> -1 then
      begin
        FTempCoolBar.ToolBars[J].Position := Coolbar.Bands[I].Index;
        FTempCoolBar.ToolBars[J].Break := Coolbar.Bands[I].Break;
      end;
    end;
    FTempCoolBar.Sort;

    IDECoolBar.ToolBars.Clear;
    for I := 0 to FTempCoolBar.ToolBars.Count - 1  do
    begin
      IDEToolBar := IDECoolBar.Add;
      IDEToolBar.Position := FTempCoolBar.ToolBars[I].Position;
      IDEToolBar.Break := FTempCoolBar.ToolBars[I].Break;
      for J := 0 to FTempCoolBar.ToolBars[I].ButtonNames.Count - 1 do
        IDEToolBar.ButtonNames.Add(FTempCoolBar.ToolBars[I].ButtonNames.Strings[J]);
    end;
  end;
  MainIDEBar.RefreshCoolbar;
end;

class function TToolbarOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

procedure TToolbarOptionsFrame.CoolBarMouseDown(Sender: TObject;
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

procedure TToolbarOptionsFrame.CoolbarResize(Sender: TObject);
begin
  if tmWait.Enabled then
    Exit;
  tmWait.Enabled := True;
end;

procedure TToolbarOptionsFrame.tmWaitTimer(Sender: TObject);
begin
  Coolbar.AutosizeBands;
  tmWait.Enabled := False;
end;

procedure TToolbarOptionsFrame.spGrabWidthChange(Sender: TObject);
begin
  CoolBar.GrabWidth := TSpinEdit(Sender).Value;
  CoolBar.AutosizeBands;
  EnableDisableButtons(0);
end;

procedure TToolbarOptionsFrame.cbGrabStyleChange(Sender: TObject);
begin
  CoolBar.GrabStyle := TGrabStyle(TComboBox(Sender).ItemIndex);
  CoolBar.AutosizeBands;
  EnableDisableButtons(0);
end;

procedure TToolbarOptionsFrame.cbToolBarVisibleClick(Sender: TObject);
begin
  EnableDisableButtons(0);
end;

procedure TToolbarOptionsFrame.cbBorderStyleChange(Sender: TObject);
begin
  Coolbar.BandBorderStyle := TBorderStyle(TComboBox(Sender).ItemIndex);
  Coolbar.AutosizeBands;
  EnableDisableButtons(0);
end;

procedure TToolbarOptionsFrame.SelectBand(const ID: integer);
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

function TToolbarOptionsFrame.GetSelectedBand: Integer;
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

procedure TToolbarOptionsFrame.ToolBarClick(Sender: TObject);
var
  CoolBand: TCoolBand;
begin
  CoolBand := Coolbar.Bands.FindBand(Sender as TToolBar);
  if CoolBand <> nil then
    SelectBand(CoolBand.Index);
end;

procedure TToolbarOptionsFrame.EnableDisableButtons(const bType: Integer);
  function IsDefaultGeneralEnabled: Boolean;
  begin
    Result := (not cbToolBarVisible.Checked) or (cbGrabStyle.ItemIndex <> 1) or
              (spGrabWidth.Value <> 5) or (cbBorderStyle.ItemIndex <> 1);
  end;

  function IsDefaultToolbarEnabled: Boolean;
  var
    IDEToolBar0: TIDEToolBar;
    IDEToolBar1: TIDEToolBar;
  begin
    Result := True;
    if FTempCoolBar.ToolBars.Count <> 2 then
      Exit;
    if (FTempCoolBar.ToolBars[0].ButtonNames.Count <> 8) or (FTempCoolBar.ToolBars[1].ButtonNames.Count <> 10) then
      Exit;
    IDEToolBar0 := FTempCoolBar.ToolBars[0];
    IDEToolBar1 := FTempCoolBar.ToolBars[1];
    Result := (IDEToolBar0.ButtonNames[0] <> 'IDEMainMenu/File/itmFileNew/itmFileNewForm') or
              (IDEToolBar0.ButtonNames[1] <> 'IDEMainMenu/File/itmFileNew/itmFileNewUnit') or
              (IDEToolBar0.ButtonNames[2] <> '---------------') or
              (IDEToolBar0.ButtonNames[3] <> 'IDEMainMenu/File/itmFileOpenSave/itmFileOpen') or
              (IDEToolBar0.ButtonNames[4] <> 'IDEMainMenu/File/itmFileOpenSave/itmFileSave') or
              (IDEToolBar0.ButtonNames[5] <> 'IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll') or
              (IDEToolBar0.ButtonNames[6] <> '---------------') or
              (IDEToolBar0.ButtonNames[7] <> 'IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit') or

              (IDEToolBar1.ButtonNames[0] <> 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits') or
              (IDEToolBar1.ButtonNames[1] <> 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms') or
              (IDEToolBar1.ButtonNames[2] <> '---------------') or
              (IDEToolBar1.ButtonNames[3] <> 'IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode') or
              (IDEToolBar1.ButtonNames[4] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuRun') or
              (IDEToolBar1.ButtonNames[5] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuPause') or
              (IDEToolBar1.ButtonNames[6] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuStop') or
              (IDEToolBar1.ButtonNames[7] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver') or
              (IDEToolBar1.ButtonNames[8] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto') or
              (IDEToolBar1.ButtonNames[9] <> 'IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
  end;

var
  I: Integer;
  Selected: Boolean;
begin
  case bType of
    0: begin //general settings
         bDefaultGeneral.Enabled := IsDefaultGeneralEnabled;
       end;
    1: begin //toolbar settings
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
         bDefaultToolbar.Enabled := IsDefaultToolbarEnabled;
       end;
  end;
end;

procedure TToolbarOptionsFrame.PopulateToolBar;
var
  CoolBand: TCoolBand;
  I, J: Integer;
begin
  CoolBar.Bands.Clear;
  for I := 0 to FTempCoolBar.ToolBars.Count - 1 do
  begin
    CoolBand := CoolBar.Bands.Add;
    CoolBand.Break := FTempCoolBar.ToolBars[I].Break;
    CoolBand.Control := FTempCoolBar.ToolBars[I].Toolbar;
    FTempCoolBar.ToolBars[I].Toolbar.Enabled := False;
    CoolBand.Visible := True;
    CoolBand.MinWidth := 25;
    CoolBand.MinHeight := 22;
    CoolBand.FixedSize := True;
    FTempCoolBar.ToolBars[I].ClearToolbar;
    for J := 0 to FTempCoolBar.ToolBars[I].ButtonNames.Count - 1 do
      FTempCoolBar.ToolBars[I].AddCustomItems(J);
  end;
  if CoolBar.Bands.Count > 0 then
    SelectBand(0);
  Coolbar.AutosizeBands;
  EnableDisableButtons(1);
end;

constructor TToolbarOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTempCoolBar := TIDEcoolBar.Create(Nil);
end;

destructor TToolbarOptionsFrame.Destroy;
begin
  FreeAndNil(FTempCoolBar);
  inherited Destroy;
end;

procedure TToolbarOptionsFrame.bAddClick(Sender: TObject);
var
  CoolBand: TCoolBand;
  IDEToolbar: TIDEToolBar;
begin
  IDEToolbar := FTempCoolBar.Add;
  IDEToolbar.Break := False;
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
  EnableDisableButtons(1);
end;

procedure TToolbarOptionsFrame.bConfigClick(Sender: TObject);
var
  ToConfig: Integer;
  ToolBar: TToolBar;
  I: Integer;
begin
  ToConfig := GetSelectedBand;
  if ToConfig = -1 then
  begin
    MessageDlg(lisCoolbarSelectToolBar, mtInformation, [mbOk], 0);
    Exit;
  end;
  fToolBarConfig := TToolBarConfig.Create(Self);
  try
    ToolBar := (Coolbar.Bands.Items[ToConfig].Control as TToolBar);
    if ToolBar <> nil then
    begin
      ToConfig := FTempCoolBar.FindByToolBar(ToolBar);
      if ToConfig <> -1 then
      begin
        fToolBarConfig.LoadSettings(FTempCoolBar.ToolBars[ToConfig].ButtonNames);
        if fToolBarConfig.ShowModal  = mrOK then
        begin
          FTempCoolBar.ToolBars[ToConfig].ClearToolbar;
          fToolBarConfig.SaveSettings(FTempCoolBar.ToolBars[ToConfig].ButtonNames);
          for I := 0 to FTempCoolBar.ToolBars[ToConfig].ButtonNames.Count - 1 do
            FTempCoolBar.ToolBars[ToConfig].AddCustomItems(I);
        end;
      end;
    end;
  finally
    fToolBarConfig.Free;
  end;
  Coolbar.AutosizeBands;
  EnableDisableButtons(1);
end;

procedure TToolbarOptionsFrame.bDeleteClick(Sender: TObject);
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
  EnableDisableButtons(1);
end;

procedure TToolbarOptionsFrame.bDefaultGeneralClick(Sender: TObject);
begin
  cbGrabStyle.ItemIndex := 1;
  spGrabWidth.Value := 5;
  BiDiMode := bdLeftToRight;
  cbBorderStyle.ItemIndex := 1;
  Coolbar.GrabStyle := TGrabStyle(1);
  Coolbar.GrabWidth := 5;
  Coolbar.BandBorderStyle := bsSingle;
  EnableDisableButtons(0);
end;

procedure TToolbarOptionsFrame.bDefaultToolbarClick(Sender: TObject);
var
  IDEToolBar: TIDEToolBar;
begin
  FTempCoolBar.ToolBars.Clear;
  //standard
  IDEToolBar := FTempCoolBar.Add;
  IDEToolBar.Position := 0;
  IDEToolBar.Break := False;
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileNew/itmFileNewForm');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileNew/itmFileNewUnit');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileOpen');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileSave');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/File/itmFileOpenSave/itmFileSaveAll');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/View/itmViewMainWindows/itmViewToggleFormUnit');

  //debug
  IDEToolBar := FTempCoolBar.Add;
  IDEToolBar.Position := 1;
  IDEToolBar.Break := True;
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewUnits');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectViewForms');
  IDEToolBar.ButtonNames.Add('---------------');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Project/itmProjectAddRemoveSection/itmProjectBuildMode');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuRun');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuPause');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStop');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOver');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepInto');
  IDEToolBar.ButtonNames.Add('IDEMainMenu/Run/itmRunnning/itmRunMenuStepOut');
  PopulateToolBar;
end;


initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TToolbarOptionsFrame, EnvOptionsToolbar);

end.

