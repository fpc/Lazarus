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
  Forms, StdCtrls, InterfaceBase, ExtCtrls, Controls,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazStringUtils,
  // IdeConfig
  EnvironmentOpts, LazConf, TransferMacros,
  // IDE
  LazarusIDEStrConsts, EnvGuiOptions;

type
  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    AutoAdjustIDEHeightFullCompPalCheckBox: TCheckBox;
    bvWindowTitle: TDividerBevel;
    EdTitleBar: TComboBox;
    lbTitlePreviewHeader: TLabel;
    lbTitlePreview: TLabel;
    lblTitleBar: TLabel;
    pnlTitlePreview: TPanel;
    lblShowingWindows: TDividerBevel;
    NameForDesignedFormListCheckBox: TCheckBox;
    AutoAdjustIDEHeightCheckBox: TCheckBox;
    HideIDEOnRunCheckBox: TCheckBox;
    SingleTaskBarButtonCheckBox: TCheckBox;
    TitleStartsWithProjectCheckBox: TCheckBox;
    procedure EdTitleBarChange(Sender: TObject);
    procedure TitleStartsWithProjectCheckBoxChange(Sender: TObject);
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

procedure TWindowOptionsFrame.EdTitleBarChange(Sender: TObject);
  function AddToCaption(const CurrentCaption, CaptAddition: string): String;
  begin
    if TitleStartsWithProjectCheckBox.Checked then
      Result := CaptAddition + ' - ' + CurrentCaption
    else
      Result := CurrentCaption + ' - ' + CaptAddition;
  end;

var
  rev, NewCaption: String;
  CustomCaption: TCaption;
  OldMarkUnhandledMacros: Boolean;
begin
  if GlobalMacroList = nil then
    exit;

  rev := LazarusRevisionStr;
  if IsNumeric(rev) then
    NewCaption := Format(lisLazarusEditorV + ' r%s',
                         [LazarusVersionStr, rev])
  else
    NewCaption := Format(lisLazarusEditorV, [LazarusVersionStr]);

  CustomCaption := EdTitleBar.Text;
  if CustomCaption <> '' then begin
    OldMarkUnhandledMacros := GlobalMacroList.MarkUnhandledMacros;
    GlobalMacroList.MarkUnhandledMacros := false;
    GlobalMacroList.SubstituteStr(CustomCaption, 0, 0, True);
    if CustomCaption <> '' then begin
      NewCaption := AddToCaption(NewCaption, CustomCaption);
    end;
    GlobalMacroList.MarkUnhandledMacros := OldMarkUnhandledMacros;
  end;

  lbTitlePreview.Caption := NewCaption;
end;

procedure TWindowOptionsFrame.TitleStartsWithProjectCheckBoxChange(Sender: TObject);
begin
  EdTitleBarChange(nil);
end;

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
  bvWindowTitle.Caption:=lisIDETitleOptions;
  lblTitleBar.Caption:=lisIDETitleCustom;
  EdTitleBar.Hint := lisIDECaptionCustomHint;
  NameForDesignedFormListCheckBox.Caption:=lisWindowMenuWithNameForDesignedForm;
  NameForDesignedFormListCheckBox.Hint:=lisWindowMenuWithNameForDesignedFormHint;
  AutoAdjustIDEHeightCheckBox.Caption:=lisAutoAdjustIDEHeight;
  AutoAdjustIDEHeightCheckBox.Hint:=lisAutoAdjustIDEHeightHint;
  AutoAdjustIDEHeightFullCompPalCheckBox.Caption:=lisAutoAdjustIDEHeightFullComponentPalette;
  AutoAdjustIDEHeightFullCompPalCheckBox.Hint:=lisAutoAdjustIDEHeightFullComponentPaletteHint;
  lbTitlePreviewHeader.Caption := dlgWRDPreview;
  lbTitlePreviewHeader.Visible := GlobalMacroList <> nil;
  pnlTitlePreview.Visible := GlobalMacroList <> nil;

  EdTitleBar.AddItem('$project(TitleNew)', nil);
  EdTitleBar.AddItem('$project(TitleNew) $EncloseBracket($project(infodir))', nil);
  EdTitleBar.AddItem('$(BuildModeCaption)', nil);
  EdTitleBar.AddItem('$project(TitleNew) $EncloseBracket($project(infodir)) $(BuildModeCaption)', nil);
  EdTitleBar.AddItem('$(FPCTarget)', nil);
  EdTitleBar.AddItem('$TargetCPU(Param)-$TargetOS(Param)-$SubTarget(Param)', nil);
  EdTitleBar.AddItem('$(LCLWidgetType)', nil);
  EdTitleBar.AddItem('$(FPCVer)', nil);
  EdTitleBar.AddItem('$project(TitleNew) $EncloseBracket($project(infodir)) $(BuildModeCaption) $(LCLWidgetType) $(FPCTarget)', nil);
  EdTitleBar.AddItem('$(EdFile)', nil);
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  EnvOpt: TEnvironmentOptions;
  EnvGui: TIDESubOptions;
begin
  EnvOpt := AOptions as TEnvironmentOptions;
  EnvGui := EnvOpt.GetSubConfigObj(TEnvGuiOptions);
  with (EnvGui as TEnvGuiOptions).Desktop do
  begin
    // window minimizing and hiding
    SingleTaskBarButtonCheckBox.Checked := SingleTaskBarButton;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    TitleStartsWithProjectCheckBox.Checked := IDETitleStartsWithProject;
    EdTitleBar.Text := IDETitleBarCustomText;
    NameForDesignedFormListCheckBox.Checked := IDENameForDesignedFormList;
    AutoAdjustIDEHeightCheckBox.Checked := AutoAdjustIDEHeight;
    AutoAdjustIDEHeightFullCompPalCheckBox.Checked := AutoAdjustIDEHeightFullCompPal;
  end;
  EdTitleBarChange(nil);
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  EnvOpt: TEnvironmentOptions;
  EnvGui: TIDESubOptions;
begin
  EnvOpt := AOptions as TEnvironmentOptions;
  EnvGui := EnvOpt.GetSubConfigObj(TEnvGuiOptions);
  with (EnvGui as TEnvGuiOptions).Desktop do
  begin
    // window minimizing
    SingleTaskBarButton := SingleTaskBarButtonCheckBox.Checked;
    HideIDEOnRun := HideIDEOnRunCheckBox.Checked;
    IDETitleStartsWithProject := TitleStartsWithProjectCheckBox.Checked;
    IDETitleBarCustomText := EdTitleBar.Text;
    IDENameForDesignedFormList := NameForDesignedFormListCheckBox.Checked;
    AutoAdjustIDEHeight := AutoAdjustIDEHeightCheckBox.Checked;
    AutoAdjustIDEHeightFullCompPal := AutoAdjustIDEHeightFullCompPalCheckBox.Checked;
  end;
end;

class function TWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TWindowOptionsFrame, EnvOptionsWindow);
end.

