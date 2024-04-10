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
  Debugger, IdeDebuggerOpts, BaseDebugManager, IdeDebuggerStringConstants, EnvDebuggerOptions,
  ProjectDebugLink, IdeDebuggerBackendValueConv, IdeDebuggerValueFormatter,
  DisplayFormatConfigFrame;

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
    dropValFormatter: TComboBox;
    lblValFormatter: TLabel;
    Panel2: TPanel;
    Spacer1: TLabel;
    lblFpDbgConv: TLabel;
    lblExpression: TLabel;
    lblRepCount: TLabel;
    Panel1: TPanel;
    PanelTop: TPanel;
    Spacer2: TLabel;
    txtExpression: TEdit;
    txtRepCount: TEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure chkAllowFuncChange(Sender: TObject);
    procedure txtExpressionChange(Sender: TObject);
  private
    FMode: (wpmWatch, wpmDispFormat);
    FWatch: TIdeWatch;
    FDisplayFormat: TWatchDisplayFormat;
  public
    constructor Create(AOWner: TComponent; const AWatch: TIdeWatch; const AWatchExpression: String = ''; AResDataType: TWatchResultDataKind = rdkUnknown); overload;
    constructor Create(AOWner: TComponent; const ADisplayFormat: TWatchDisplayFormat; AResDataType: TWatchResultDataKind; AShowMemDump: boolean = False); overload;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TWatchPropertyDlg }

procedure TWatchPropertyDlg.btnOKClick(Sender: TObject);
var
  Conv: TIdeDbgValueConvertSelector;
  VFormatter: TIdeDbgValueFormatterSelector;
begin
  if FMode = wpmDispFormat then begin
    FDisplayFormat := DisplayFormatFrame1.DisplayFormat;
    exit;
  end;

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
    else begin
      Conv := TIdeDbgValueConvertSelector(dropFpDbgConv.Items.Objects[dropFpDbgConv.ItemIndex]);
      if (DebuggerOptions.BackendConverterConfig.IndexOf(Conv) < 0) and
         (DbgProjectLink.BackendConverterConfig.IndexOf(Conv) < 0)
      then
        Conv := nil;
      FWatch.DbgBackendConverter := Conv;
    end;

    if dropValFormatter.ItemIndex = 0 then
      FWatch.DbgValueFormatter := nil
    else
    if dropValFormatter.ItemIndex = 1 then
      FWatch.EvaluateFlags := FWatch.EvaluateFlags + [defSkipValueFormatter]
    else begin
      VFormatter := TIdeDbgValueFormatterSelector(dropValFormatter.Items.Objects[dropValFormatter.ItemIndex]);
      if (DebuggerOptions.ValueFormatterConfig.IndexOf(VFormatter) < 0) and
         (DbgProjectLink.ValueFormatterConfig.IndexOf(VFormatter) < 0)
      then
        VFormatter := nil;
      FWatch.DbgValueFormatter := VFormatter;
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
  i: Integer;
begin
  FMode := wpmWatch;
  FWatch := AWatch;
  inherited Create(AOwner);
  PanelTop.Visible := True;
  ButtonPanel.HelpButton.Visible := True;
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.BeginUdpate;
  try
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
  finally
    DisplayFormatFrame1.EndUdpate;
  end;
  txtExpressionChange(nil);

  chkAllowFunc.Enabled := EnvironmentDebugOpts.DebuggerAllowFunctionCalls and
    (dfEvalFunctionCalls in DebugBoss.DebuggerClass.SupportedFeatures);
  chkAllowFuncThreads.Enabled := EnvironmentDebugOpts.DebuggerAllowFunctionCalls and
    (dfEvalFunctionCalls in DebugBoss.DebuggerClass.SupportedFeatures) and
    (chkAllowFunc.Checked);

  Caption:= lisWatchPropert;
  lblExpression.Caption:= lisExpression;
  lblRepCount.Caption:= lisRepeatCount;
  chkEnabled.Caption:= lisEnabled;
  chkAllowFunc.Caption:= lisAllowFunctio;
  chkAllowFuncThreads.Caption := drsRunAllThreadsWhileEvaluat;
  chkUseInstanceClass.Caption := drsUseInstanceClassType;

  lblFpDbgConv.Caption := dlgBackendConvOptDebugConverter;
  dropFpDbgConv.AddItem(dlgBackendConvOptDefault, nil);
  dropFpDbgConv.AddItem(dlgBackendConvOptDisabled, nil);
  for i := 0 to DebuggerOptions.BackendConverterConfig.Count - 1 do
    dropFpDbgConv.AddItem(DebuggerOptions.BackendConverterConfig.Items[i].Name, DebuggerOptions.BackendConverterConfig.Items[i]);
  for i := 0 to DbgProjectLink.BackendConverterConfig.Count - 1 do
    dropFpDbgConv.AddItem(DbgProjectLink.BackendConverterConfig.Items[i].Name, DbgProjectLink.BackendConverterConfig.Items[i]);

  dropFpDbgConv.ItemIndex := 0;
  if AWatch <> nil then begin
    if defSkipValConv in AWatch.EvaluateFlags then begin
      dropFpDbgConv.ItemIndex := 1;
    end
    else
    if AWatch.DbgBackendConverter <> nil then begin
      i := dropFpDbgConv.Items.IndexOfObject(AWatch.DbgBackendConverter);
      assert(i > 0, 'TWatchPropertyDlg.Create: i > 0');
      if i > 0 then
        dropFpDbgConv.ItemIndex := i;
    end;
  end;

  lblValFormatter.Caption := dlgVarFormatterDebugOptions;
  dropValFormatter.AddItem(dlgBackendConvOptDefault, nil);
  dropValFormatter.AddItem(dlgBackendConvOptDisabled, nil);
  for i := 0 to DebuggerOptions.ValueFormatterConfig.Count - 1 do
    dropValFormatter.AddItem(DebuggerOptions.ValueFormatterConfig.Items[i].Name, DebuggerOptions.ValueFormatterConfig.Items[i]);
  for i := 0 to DbgProjectLink.ValueFormatterConfig.Count - 1 do
    dropValFormatter.AddItem(DbgProjectLink.ValueFormatterConfig.Items[i].Name, DbgProjectLink.ValueFormatterConfig.Items[i]);

  dropValFormatter.ItemIndex := 0;
  if AWatch <> nil then begin
    if defSkipValueFormatter in AWatch.EvaluateFlags then begin
      dropValFormatter.ItemIndex := 1;
    end
    else
    if AWatch.DbgValueFormatter <> nil then begin
      i := dropValFormatter.Items.IndexOfObject(AWatch.DbgValueFormatter);
      assert(i > 0, 'TWatchPropertyDlg.Create: i > 0');
      if i > 0 then
        dropValFormatter.ItemIndex := i;
    end;
  end;

  ButtonPanel.OKButton.Caption:=lisBtnOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
end;

constructor TWatchPropertyDlg.Create(AOWner: TComponent;
  const ADisplayFormat: TWatchDisplayFormat; AResDataType: TWatchResultDataKind;
  AShowMemDump: boolean);
begin
  FMode := wpmDispFormat;
  inherited Create(AOWner);
  PanelTop.Visible := False;
  ButtonPanel.HelpButton.Visible := False;
  FDisplayFormat := ADisplayFormat;

  Caption:= dlgDisplayFormatDebugOptions;
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowMemDump := AShowMemDump;
  DisplayFormatFrame1.BeginUdpate;
  DisplayFormatFrame1.DisplayFormat := FDisplayFormat;
  DisplayFormatFrame1.CurrentResDataType := AResDataType;
  DisplayFormatFrame1.SelectDefaultButton;
  DisplayFormatFrame1.EndUdpate;
end;

destructor TWatchPropertyDlg.Destroy;
begin
  inherited;
end;

end.

