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
  Forms, StdCtrls, Extctrls, ButtonPanel, Spin,
  // IdeIntf
  IdeIntfStrConsts, IdeDebuggerWatchValueIntf,
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
    txtRepCount: TSpinEdit;
    procedure btnOKClick(Sender: TObject);
    procedure chkAllowFuncChange(Sender: TObject);
    procedure txtExpressionChange(Sender: TObject);
    procedure txtRepCountChange(Sender: TObject);
  private
    FMode: (wpmWatch, wpmMultiWatch, wpmDispFormat);
    FWatch: TIdeWatch;
    FWatches: array of TIdeWatch;
    FDisplayFormat: TWatchDisplayFormat;

    procedure InitCaptions(AnIncludeUnknown: Boolean);
    procedure InitBtnCaptions;
    function  ValBackConvIndex(const AWatch: TIdeWatch; AnIncludeUnknown: Boolean): integer;
    function  ValFormatterIndex(const AWatch: TIdeWatch; AnIncludeUnknown: Boolean): integer;
    procedure  ValBackConvToWatch(AWatch: TIdeWatch; AnIncludeUnknown: Boolean);
    procedure  ValFormatterToWach(AWatch: TIdeWatch; AnIncludeUnknown: Boolean);
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AnOWner: TComponent; const AWatch: TIdeWatch; const AWatchExpression: String = ''; AResDataType: TWatchResultDataKind = rdkUnknown); overload;
    constructor Create(AnOWner: TComponent; const AWatches: array of TIdeWatch); overload;
    constructor Create(AnOWner: TComponent; const ADisplayFormat: TWatchDisplayFormat; AResDataType: TWatchResultDataKind; AShowMemDump: boolean = False; AShowArrayNav: boolean = False); overload;
    property DisplayFormat: TWatchDisplayFormat read FDisplayFormat;
  end;

implementation

{$R *.lfm}

{ TWatchPropertyDlg }

procedure TWatchPropertyDlg.btnOKClick(Sender: TObject);
var
  Idx: Integer;
begin
  case FMode of
    wpmWatch: begin
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

        ValBackConvToWatch(FWatch, False);
        ValFormatterToWach(FWatch, False);

        FWatch.Enabled := chkEnabled.Checked;
      finally
        DebugBoss.Watches.CurrentWatches.EndUpdate;
      end;
    end;

    wpmMultiWatch: begin
      DebugBoss.Watches.CurrentWatches.BeginUpdate;
      try
        for Idx := 0 to Length(FWatches) - 1 do begin
          FWatches[Idx].DisplayFormat := DisplayFormatFrame1.DisplayFormats[Idx];

          case chkUseInstanceClass.State of
            cbUnchecked: FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags - [defClassAutoCast];
            cbChecked:   FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags + [defClassAutoCast];
          end;
          case chkAllowFunc.State of
            cbUnchecked: FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags - [defAllowFunctionCall];
            cbChecked:   FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags + [defAllowFunctionCall];
          end;
          case chkAllowFuncThreads.State of
            cbUnchecked: FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags - [defFunctionCallRunAllThreads];
            cbChecked:   FWatches[Idx].EvaluateFlags := FWatches[Idx].EvaluateFlags + [defFunctionCallRunAllThreads];
          end;

          if txtRepCount.Tag = 0 then
            FWatches[Idx].RepeatCount := txtRepCount.Value;

          ValBackConvToWatch(FWatches[Idx], False);
          ValFormatterToWach(FWatches[Idx], False);

          case chkEnabled.State of
            cbUnchecked: FWatches[Idx].Enabled := False;
            cbChecked:   FWatches[Idx].Enabled := True;
          end;
      end;
      finally
        DebugBoss.Watches.CurrentWatches.EndUpdate;
      end;
    end;

    wpmDispFormat: begin
      FDisplayFormat := DisplayFormatFrame1.DisplayFormat;
      exit;
    end;
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

procedure TWatchPropertyDlg.txtRepCountChange(Sender: TObject);
begin
  txtRepCount.Tag := 0;
end;

procedure TWatchPropertyDlg.InitCaptions(AnIncludeUnknown: Boolean);
var
  i: Integer;
begin
  Caption:= lisWatchPropert;

  lblExpression.Caption:= lisExpression;
  lblRepCount.Caption:= lisRepeatCount;
  chkEnabled.Caption:= lisEnabled;
  chkAllowFunc.Caption:= lisAllowFunctio;
  chkAllowFuncThreads.Caption := drsRunAllThreadsWhileEvaluat;
  chkUseInstanceClass.Caption := drsUseInstanceClassType;

  lblFpDbgConv.Caption := dlgBackendConvOptDebugConverter;
  if AnIncludeUnknown then
    dropFpDbgConv.AddItem(dlgWatchPropertyUnknown, nil);
  dropFpDbgConv.AddItem(dlgBackendConvOptDefault, nil);
  dropFpDbgConv.AddItem(dlgBackendConvOptDisabled, nil);
  for i := 0 to DebuggerOptions.BackendConverterConfig.Count - 1 do
    dropFpDbgConv.AddItem(DebuggerOptions.BackendConverterConfig.Items[i].Name, DebuggerOptions.BackendConverterConfig.Items[i]);
  for i := 0 to DbgProjectLink.BackendConverterConfig.Count - 1 do
    dropFpDbgConv.AddItem(DbgProjectLink.BackendConverterConfig.Items[i].Name, DbgProjectLink.BackendConverterConfig.Items[i]);


  lblValFormatter.Caption := dlgVarFormatterDebugOptions;
  if AnIncludeUnknown then
    dropValFormatter.AddItem(dlgWatchPropertyUnknown, nil);
  dropValFormatter.AddItem(dlgBackendConvOptDefault, nil);
  dropValFormatter.AddItem(dlgBackendConvOptDisabled, nil);
  for i := 0 to DebuggerOptions.ValueFormatterConfig.Count - 1 do
    dropValFormatter.AddItem(DebuggerOptions.ValueFormatterConfig.Items[i].Name, DebuggerOptions.ValueFormatterConfig.Items[i]);
  for i := 0 to DbgProjectLink.ValueFormatterConfig.Count - 1 do
    dropValFormatter.AddItem(DbgProjectLink.ValueFormatterConfig.Items[i].Name, DbgProjectLink.ValueFormatterConfig.Items[i]);

  dropValFormatter.ItemIndex := 0;

  InitBtnCaptions;
end;

procedure TWatchPropertyDlg.InitBtnCaptions;
begin
  ButtonPanel.OKButton.Caption:=lisBtnOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
end;

function TWatchPropertyDlg.ValBackConvIndex(const AWatch: TIdeWatch; AnIncludeUnknown: Boolean
  ): integer;
var
  i: Integer;
begin
  Result := 0;
  if AnIncludeUnknown then
    Result := 1;
  if AWatch <> nil then begin
    if defSkipValConv in AWatch.EvaluateFlags then begin
      Result := Result + 1;
    end
    else
    if AWatch.DbgBackendConverter <> nil then begin
      i := dropFpDbgConv.Items.IndexOfObject(AWatch.DbgBackendConverter);
      assert(i > 0, 'TWatchPropertyDlg.Create: i > 0');
      if i > 0 then
        Result := i;
    end;
  end;
end;

function TWatchPropertyDlg.ValFormatterIndex(const AWatch: TIdeWatch; AnIncludeUnknown: Boolean
  ): integer;
var
  i: Integer;
begin
  Result := 0;
  if AnIncludeUnknown then
    Result := 1;
  if AWatch <> nil then begin
    if defSkipValueFormatter in AWatch.EvaluateFlags then begin
      Result := Result + 1;
    end
    else
    if AWatch.DbgValueFormatter <> nil then begin
      i := dropValFormatter.Items.IndexOfObject(AWatch.DbgValueFormatter);
      assert(i > 0, 'TWatchPropertyDlg.Create: i > 0');
      if i > 0 then
        Result := i;
    end;
  end;
end;

procedure TWatchPropertyDlg.ValBackConvToWatch(AWatch: TIdeWatch; AnIncludeUnknown: Boolean);
var
  Offs: Integer;
  Conv: TIdeDbgValueConvertSelector;
begin
  Offs := 0;
  if AnIncludeUnknown then begin
    if dropFpDbgConv.ItemIndex = 0 then
      exit;
    Offs := 1;
  end;
  if dropFpDbgConv.ItemIndex = Offs then
    AWatch.DbgBackendConverter := nil
  else
  if dropFpDbgConv.ItemIndex = Offs+1 then
    AWatch.EvaluateFlags := AWatch.EvaluateFlags + [defSkipValConv]
  else begin
    Conv := TIdeDbgValueConvertSelector(dropFpDbgConv.Items.Objects[dropFpDbgConv.ItemIndex]);
    if (DebuggerOptions.BackendConverterConfig.IndexOf(Conv) < 0) and
       (DbgProjectLink.BackendConverterConfig.IndexOf(Conv) < 0)
    then
      Conv := nil;
    AWatch.DbgBackendConverter := Conv;
  end;
end;

procedure TWatchPropertyDlg.ValFormatterToWach(AWatch: TIdeWatch; AnIncludeUnknown: Boolean);
var
  Offs: Integer;
  VFormatter: TIdeDbgValueFormatterSelector;
begin
  Offs := 0;
  if AnIncludeUnknown then begin
    if dropFpDbgConv.ItemIndex = 0 then
      exit;
    Offs := 1;
  end;

  if dropValFormatter.ItemIndex = Offs then
    AWatch.DbgValueFormatter := nil
  else
  if dropValFormatter.ItemIndex = Offs+1 then
    AWatch.EvaluateFlags := AWatch.EvaluateFlags + [defSkipValueFormatter]
  else begin
    VFormatter := TIdeDbgValueFormatterSelector(dropValFormatter.Items.Objects[dropValFormatter.ItemIndex]);
    if (DebuggerOptions.ValueFormatterConfig.IndexOf(VFormatter) < 0) and
       (DbgProjectLink.ValueFormatterConfig.IndexOf(VFormatter) < 0)
    then
      VFormatter := nil;
    AWatch.DbgValueFormatter := VFormatter;
  end;
end;

procedure TWatchPropertyDlg.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  // Issue #40875
  if HandleAllocated then
    if txtRepCount.Tag = 2 then begin
      txtRepCount.Handle;
      txtRepCount.Text := '';
      txtRepCount.Tag := 2;
    end;
end;

constructor TWatchPropertyDlg.Create(AnOWner: TComponent; const AWatch: TIdeWatch;
  const AWatchExpression: String; AResDataType: TWatchResultDataKind);
begin
  FMode := wpmWatch;
  FWatch := AWatch;
  inherited Create(AnOwner);
  PanelTop.Visible := True;
  ButtonPanel.HelpButton.Visible := True;
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.BeginUdpate;
  DisplayFormatFrame1.ShowArrayNavBarOpts := True;
  DisplayFormatFrame1.ShowMemDump := True;
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

  InitCaptions(False);

  dropFpDbgConv.ItemIndex := ValBackConvIndex(AWatch, False);
  dropValFormatter.ItemIndex := ValFormatterIndex(AWatch, False);
end;

constructor TWatchPropertyDlg.Create(AnOWner: TComponent; const AWatches: array of TIdeWatch);
var
  Idx: Integer;
  OptEnabled, OptUseInstanceClass, OptAllowFunc, OptFuncThreads: TBoolSet;
  OptRepeat, OptBackend, OptValFormatter: integer;
begin
  FMode := wpmMultiWatch;
  inherited Create(AnOWner);
  PanelTop.Visible := True;
  txtExpression.Text    := '';
  txtExpression.Enabled := False;
  InitCaptions(True);

  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowArrayNavBarOpts := True;
  DisplayFormatFrame1.ShowMemDump := True;
  DisplayFormatFrame1.BeginUdpate;
  try
    OptEnabled          := [];
    OptUseInstanceClass := [];
    OptAllowFunc        := [];
    OptFuncThreads      := [];
    OptRepeat           := MULTIOPT_INT_UNK;
    OptBackend          := MULTIOPT_INT_UNK;
    OptValFormatter     := MULTIOPT_INT_UNK;

    DisplayFormatFrame1.DisplayFormatCount := Length(AWatches);
    SetLength(FWatches, Length(AWatches));
    for Idx := 0 to Length(AWatches) - 1 do begin
      DisplayFormatFrame1.DisplayFormats[Idx] := AWatches[Idx].DisplayFormat;
      FWatches[Idx] := AWatches[Idx];
      include(OptEnabled, AWatches[Idx].Enabled);
      include(OptUseInstanceClass, defClassAutoCast             in AWatches[Idx].EvaluateFlags);
      include(OptAllowFunc,        defAllowFunctionCall         in AWatches[Idx].EvaluateFlags);
      include(OptFuncThreads,      defFunctionCallRunAllThreads in AWatches[Idx].EvaluateFlags);
      UpdateIntSetting(OptRepeat,       AWatches[Idx].RepeatCount);
      UpdateIntSetting(OptBackend,      ValBackConvIndex(AWatches[Idx], True));
      UpdateIntSetting(OptValFormatter, ValFormatterIndex(AWatches[Idx], True));
    end;
    chkEnabled.State          := BoolsetToCBState(OptEnabled, False);
    chkUseInstanceClass.State := BoolsetToCBState(OptUseInstanceClass, False);
    chkAllowFunc.State        := BoolsetToCBState(OptAllowFunc, False);
    chkAllowFuncThreads.State := BoolsetToCBState(OptFuncThreads, False);
    IntToSpinEdit(txtRepCount, OptRepeat);
    if OptBackend < 0 then OptBackend := 0;
    if OptValFormatter < 0 then OptValFormatter := 0;
    dropFpDbgConv.ItemIndex    := OptBackend;
    dropValFormatter.ItemIndex := OptValFormatter;

    DisplayFormatFrame1.CurrentResDataType := rdkUnknown;
    DisplayFormatFrame1.SelectDefaultButton;
  finally
    DisplayFormatFrame1.EndUdpate;
  end;
end;

constructor TWatchPropertyDlg.Create(AnOWner: TComponent;
  const ADisplayFormat: TWatchDisplayFormat; AResDataType: TWatchResultDataKind;
  AShowMemDump: boolean; AShowArrayNav: boolean);
begin
  FMode := wpmDispFormat;
  inherited Create(AnOWner);
  PanelTop.Visible := False;
  ButtonPanel.HelpButton.Visible := False;
  FDisplayFormat := ADisplayFormat;

  Caption:= dlgDisplayFormatDebugOptions;
  InitBtnCaptions;
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowArrayNavBarOpts := AShowArrayNav;
  DisplayFormatFrame1.ShowMemDump := AShowMemDump;
  DisplayFormatFrame1.BeginUdpate;
  DisplayFormatFrame1.DisplayFormat := FDisplayFormat;
  DisplayFormatFrame1.CurrentResDataType := AResDataType;
  DisplayFormatFrame1.SelectDefaultButton;
  DisplayFormatFrame1.EndUdpate;
end;

end.

