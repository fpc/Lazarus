unit DisplayFormatDefaultsConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, StdCtrls, DisplayFormatConfigFrame,
  IdeDebuggerDisplayFormats, IdeDebuggerStringConstants;

type

  { TDisplayFormatDefaultsConfigFrame }

  TDisplayFormatDefaultsConfigFrame = class(TFrame)
    DisplayFormatFrame1: TDisplayFormatFrame;
    lblProjectText: TLabel;
    lblChangingDescr: TLabel;
    btnGlobal: TSpeedButton;
    btnHint: TSpeedButton;
    btnWatches: TSpeedButton;
    btnLocals: TSpeedButton;
    btnInspect: TSpeedButton;
    btnEvalMod: TSpeedButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure btnGlobalClick(Sender: TObject);
    procedure btnTargetClick(Sender: TObject);
  private
    FDisplayFormatConfig: TDisplayFormatConfig;
    FButtonStates: array [TDisplayFormatTarget] of boolean;
    FShowProjectInfo: boolean;
    procedure SetDisplayFormatConfig(AValue: TDisplayFormatConfig);
    procedure SaveButtonStates;
    procedure ApplyConfigs;
    procedure RetrieveConfigs;
    procedure SetShowProjectInfo(AValue: boolean);

  public
    procedure Setup;
    procedure SaveConfig;
    property DisplayFormatConfig: TDisplayFormatConfig read FDisplayFormatConfig write SetDisplayFormatConfig;
    property ShowProjectInfo: boolean read FShowProjectInfo write SetShowProjectInfo;
  end;

implementation

{$R *.lfm}

{ TDisplayFormatDefaultsConfigFrame }

procedure TDisplayFormatDefaultsConfigFrame.btnGlobalClick(Sender: TObject);
begin
  RetrieveConfigs;
  btnGlobal.Down  := True;
  btnHint.Down    := False;
  btnWatches.Down := False;
  btnLocals.Down  := False;
  btnInspect.Down := False;
  btnEvalMod.Down := False;
  lblChangingDescr.Caption := DispFormatOptChangingDescrAll;
  SaveButtonStates;
  ApplyConfigs;
end;

procedure TDisplayFormatDefaultsConfigFrame.btnTargetClick(Sender: TObject);
begin
  RetrieveConfigs;
  btnGlobal.Down  := False;
  if (not(ssShift in GetKeyShiftState)) or
     (not(btnHint.Down or btnWatches.Down or btnLocals.Down or btnInspect.Down or btnEvalMod.Down))
  then begin
    btnHint.Down    := Sender = btnHint;
    btnWatches.Down := Sender = btnWatches;
    btnLocals.Down  := Sender = btnLocals;
    btnInspect.Down := Sender = btnInspect;
    btnEvalMod.Down := Sender = btnEvalMod;
  end;
  lblChangingDescr.Caption := DispFormatOptChangingDescrSome;
  SaveButtonStates;
  ApplyConfigs;
end;

procedure TDisplayFormatDefaultsConfigFrame.SetDisplayFormatConfig(AValue: TDisplayFormatConfig);
begin
  if FDisplayFormatConfig = AValue then Exit;
  FDisplayFormatConfig := AValue;
  SaveButtonStates;
  ApplyConfigs;
end;

procedure TDisplayFormatDefaultsConfigFrame.SaveButtonStates;
begin
  FButtonStates[dtfGlobal]  := btnGlobal.Down;
  FButtonStates[dtfHint]    := btnHint.Down;
  FButtonStates[dtfWatches] := btnWatches.Down;
  FButtonStates[dtfLocals]  := btnLocals.Down;
  FButtonStates[dtfInspect] := btnInspect.Down;
  FButtonStates[dtfEvalMod] := btnEvalMod.Down;
end;

procedure TDisplayFormatDefaultsConfigFrame.ApplyConfigs;
var
  j: Integer;
  t: TDisplayFormatTarget;
begin
  j := 0;
  for t in TDisplayFormatTarget do if FButtonStates[t] then inc(j);
  DisplayFormatFrame1.DisplayFormatCount := j;
  j := 0;
  for t in TDisplayFormatTarget do
    if FButtonStates[t] then begin
      DisplayFormatFrame1.DisplayFormats[j] := DisplayFormatConfig.DefaultDisplayFormats[t];
      inc(j);
    end;
end;

procedure TDisplayFormatDefaultsConfigFrame.RetrieveConfigs;
var
  j: Integer;
  t: TDisplayFormatTarget;
begin
  j := 0;
  for t in TDisplayFormatTarget do
    if FButtonStates[t] then begin
      DisplayFormatConfig.DefaultDisplayFormats[t] := DisplayFormatFrame1.DisplayFormats[j];
      inc(j);
      if j >= DisplayFormatFrame1.DisplayFormatCount then
        break;
    end;
end;

procedure TDisplayFormatDefaultsConfigFrame.SetShowProjectInfo(AValue: boolean);
begin
  if FShowProjectInfo = AValue then Exit;
  FShowProjectInfo := AValue;
  lblProjectText.Visible := AValue;
end;

procedure TDisplayFormatDefaultsConfigFrame.Setup;
begin
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowCurrent := False;
  DisplayFormatFrame1.ShowMemDump := False;
  DisplayFormatFrame1.SelectDefaultButton;

  btnGlobal.Down  := True;
  btnHint.Down    := False;
  btnWatches.Down := False;
  btnLocals.Down  := False;
  btnInspect.Down := False;
  btnEvalMod.Down := False;

  btnGlobal.Caption  := DispFormatTargetGlobal;
  btnHint.Caption    := DispFormatTargetHint;
  btnWatches.Caption := DispFormatTargetWatches;
  btnLocals.Caption  := DispFormatTargetLocals;
  btnInspect.Caption := DispFormatTargetInspect;
  btnEvalMod.Caption := DispFormatTargetEvalMod;

  lblChangingDescr.Caption := DispFormatOptChangingDescrAll;
  lblProjectText.Caption   := DispFormatOptProjectText;
end;

procedure TDisplayFormatDefaultsConfigFrame.SaveConfig;
begin
  RetrieveConfigs;
end;

end.

