{ $Id$ }
{               ----------------------------------------------
                 watchproperydlg.pp  -  property editor for 
                                        watches
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the watch property dialog.


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

unit WatchPropertyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Extctrls, ButtonPanel,
  // IdeIntf
  IDEHelpIntf, IdeIntfStrConsts, IdeDebuggerWatchValueIntf,
  // IdeConfig
  //EnvironmentOpts,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerIntf
  LazDebuggerIntf,
  // IdeDebugger
  Debugger, IdeDebuggerOpts, BaseDebugManager, IdeDebuggerStringConstants,
  EnvDebuggerOptions, DisplayFormatConfigFrame;

type

  { TWatchPropertyDlg }

  TWatchPropertyDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    chkAllowFunc: TCheckBox;
    chkAllowFuncThreads: TCheckBox;
    chkEnabled: TCheckBox;
    chkUseInstanceClass: TCheckBox;
    DisplayFormatFrame1: TDisplayFormatFrame;
    dropFpDbgConv: TComboBox;
    lblFpDbgConv: TLabel;
    lblDigits: TLabel;
    lblExpression: TLabel;
    lblRepCount: TLabel;
    PanelTop: TPanel;
    txtDigits: TEdit;
    txtExpression: TEdit;
    txtRepCount: TEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkAllowFuncChange(Sender: TObject);
    procedure txtExpressionChange(Sender: TObject);
  private
    FWatch: TIdeWatch;
  public
    constructor Create(AOWner: TComponent; const AWatch: TIdeWatch; const AWatchExpression: String = ''; AResDataType: TWatchResultDataKind = rdkUnknown); overload;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TWatchPropertyDlg }

procedure TWatchPropertyDlg.btnOKClick(Sender: TObject);
begin
  if txtExpression.Text = '' then
    exit;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  try
    if FWatch = nil
    then begin
      FWatch := DebugBoss.Watches.CurrentWatches.Add(txtExpression.Text);
    end
    else begin
      FWatch.Expression := txtExpression.Text;
    end;

    FWatch.DisplayFormat := DisplayFormatFrame1.DisplayFormat;

    FWatch.EvaluateFlags := [];
    if chkUseInstanceClass.Checked
    then FWatch.EvaluateFlags := FWatch.EvaluateFlags + [defClassAutoCast];
    if chkAllowFunc.Checked
    then FWatch.EvaluateFlags := FWatch.EvaluateFlags + [defAllowFunctionCall];
    if chkAllowFuncThreads.Checked
    then FWatch.EvaluateFlags := FWatch.EvaluateFlags + [defFunctionCallRunAllThreads];
    FWatch.RepeatCount := StrToIntDef(txtRepCount.Text, 0);

    if dropFpDbgConv.ItemIndex = 0 then
      FWatch.DbgBackendConverter := nil
    else
    if dropFpDbgConv.ItemIndex = 1 then
      FWatch.EvaluateFlags := FWatch.EvaluateFlags + [defSkipValConv]
    else
    if dropFpDbgConv.ItemIndex - 2 < DebuggerOptions.BackendConverterConfig.Count then begin
      FWatch.DbgBackendConverter := DebuggerOptions.BackendConverterConfig.IdeItems[dropFpDbgConv.ItemIndex - 2];
    end
    else begin
      if ProjectValueConverterSelectorList <> nil then
        FWatch.DbgBackendConverter := ProjectValueConverterSelectorList.IdeItems[dropFpDbgConv.ItemIndex - 2 - DebuggerOptions.BackendConverterConfig.Count];
    end;

    FWatch.Enabled := chkEnabled.Checked;
  finally
    DebugBoss.Watches.CurrentWatches.EndUpdate;
  end;
end;

procedure TWatchPropertyDlg.chkAllowFuncChange(Sender: TObject);
begin
  chkAllowFuncThreads.Enabled := EnvironmentDebugOpts.DebuggerAllowFunctionCalls and
    (dfEvalFunctionCalls in DebugBoss.DebuggerClass.SupportedFeatures) and
    (chkAllowFunc.Checked);
end;

procedure TWatchPropertyDlg.txtExpressionChange(Sender: TObject);
begin
  ButtonPanel.OKButton.Enabled := txtExpression.Text <> '';
end;

procedure TWatchPropertyDlg.btnHelpClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

constructor TWatchPropertyDlg.Create(AOWner: TComponent; const AWatch: TIdeWatch;
  const AWatchExpression: String; AResDataType: TWatchResultDataKind);
var
  i, i2: Integer;
begin
  FWatch := AWatch;
  inherited Create(AOwner);
  DisplayFormatFrame1.Setup;
  if FWatch = nil
  then begin
    chkEnabled.Checked := True;
    txtExpression.Text := AWatchExpression;
    DisplayFormatFrame1.DisplayFormat := DefaultWatchDisplayFormat;
    chkUseInstanceClass.Checked := EnvironmentDebugOpts.DebuggerAutoSetInstanceFromClass;
    txtRepCount.Text := '0';
  end
  else begin
    txtExpression.Text          := FWatch.Expression;
    chkEnabled.Checked          := FWatch.Enabled;
    DisplayFormatFrame1.DisplayFormat := FWatch.DisplayFormat;
    chkUseInstanceClass.Checked := defClassAutoCast in FWatch.EvaluateFlags;
    chkAllowFunc.Checked        := defAllowFunctionCall in FWatch.EvaluateFlags;
    chkAllowFuncThreads.Checked := defFunctionCallRunAllThreads in FWatch.EvaluateFlags;
    txtRepCount.Text            := IntToStr(FWatch.RepeatCount);
  end;
  DisplayFormatFrame1.CurrentResDataType := AResDataType;
  DisplayFormatFrame1.SelectDefaultButton;
  txtExpressionChange(nil);

  lblDigits.Enabled := False;
  txtDigits.Enabled := False;
  chkAllowFunc.Enabled := EnvironmentDebugOpts.DebuggerAllowFunctionCalls and
    (dfEvalFunctionCalls in DebugBoss.DebuggerClass.SupportedFeatures);
  chkAllowFuncThreads.Enabled := EnvironmentDebugOpts.DebuggerAllowFunctionCalls and
    (dfEvalFunctionCalls in DebugBoss.DebuggerClass.SupportedFeatures) and
    (chkAllowFunc.Checked);

  Caption:= lisWatchPropert;
  lblExpression.Caption:= lisExpression;
  lblRepCount.Caption:= lisRepeatCount;
  lblDigits.Caption:= lisDigits;
  chkEnabled.Caption:= lisEnabled;
  chkAllowFunc.Caption:= lisAllowFunctio;
  chkAllowFuncThreads.Caption := drsRunAllThreadsWhileEvaluat;
  chkUseInstanceClass.Caption := drsUseInstanceClassType;

  lblFpDbgConv.Caption := dlgBackendConvOptDebugConverter;
  dropFpDbgConv.AddItem(dlgBackendConvOptDefault, nil);
  dropFpDbgConv.AddItem(dlgBackendConvOptDisabled, nil);
  for i := 0 to DebuggerOptions.BackendConverterConfig.Count - 1 do
    dropFpDbgConv.AddItem(DebuggerOptions.BackendConverterConfig.IdeItems[i].Name, nil);
  i2 := dropFpDbgConv.Items.Count;
  if (ProjectValueConverterSelectorList <> nil) then
    for i := 0 to ProjectValueConverterSelectorList.Count - 1 do
      dropFpDbgConv.AddItem(ProjectValueConverterSelectorList.IdeItems[i].Name, nil);

  dropFpDbgConv.ItemIndex := 0;
  if AWatch <> nil then begin
    if defSkipValConv in AWatch.EvaluateFlags then begin
      dropFpDbgConv.ItemIndex := 1;
    end
    else
    if AWatch.DbgBackendConverter <> nil then begin
      i := DebuggerOptions.BackendConverterConfig.IndexOf(AWatch.DbgBackendConverter);
      if i >= 0 then begin
        dropFpDbgConv.ItemIndex := i + 2;
      end
      else begin
        i := ProjectValueConverterSelectorList.IndexOf(AWatch.DbgBackendConverter);
        if i >= 0 then
          dropFpDbgConv.ItemIndex := i2 + i;
      end;
    end;
  end;

  ButtonPanel.OKButton.Caption:=lisBtnOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
end;

destructor TWatchPropertyDlg.Destroy;
begin
  inherited;
end;

end.

