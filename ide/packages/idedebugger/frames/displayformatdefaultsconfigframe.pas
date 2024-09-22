unit DisplayFormatDefaultsConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, StdCtrls, ExtCtrls,
  IdeDebuggerWatchValueIntf, DisplayFormatConfigFrame,
  IdeDebuggerDisplayFormats, IdeDebuggerStringConstants;

type

  { TDisplayFormatDefaultsConfigFrame }

  TDisplayFormatDefaultsConfigFrame = class(TFrame)
    btnPreDefaults: TButton;
    btnPresets: TSpeedButton;
    btnPreNew: TButton;
    btnPreDel: TButton;
    btnPreUp: TButton;
    btnPreDown: TButton;
    DisplayFormatFrame1: TDisplayFormatFrame;
    edPreName: TEdit;
    lblPreName: TLabel;
    lblProjectText: TLabel;
    lblChangingDescr: TLabel;
    btnGlobal: TSpeedButton;
    btnHint: TSpeedButton;
    btnWatches: TSpeedButton;
    btnLocals: TSpeedButton;
    btnInspect: TSpeedButton;
    btnEvalMod: TSpeedButton;
    lbPresets: TListBox;
    Panel1: TPanel;
    pnlPresetOpts: TPanel;
    pnlPresetList: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure btnGlobalClick(Sender: TObject);
    procedure btnPreDelClick(Sender: TObject);
    procedure btnPreDefaultsClick(Sender: TObject);
    procedure btnPreDownClick(Sender: TObject);
    procedure btnPreNewClick(Sender: TObject);
    procedure btnPresetsClick(Sender: TObject);
    procedure btnPreUpClick(Sender: TObject);
    procedure btnTargetClick(Sender: TObject);
    procedure edPreNameEditingDone(Sender: TObject);
    procedure lbPresetsClick(Sender: TObject);
  private
    FDisplayFormatConfig: TDisplayFormatConfig;
    FButtonStates: array [TDisplayFormatTarget] of boolean;
    FButtonStatePreset: boolean;
    FCurrentPresetIdx: integer;
    FShowOverrideChecks: boolean;
    FShowProjectInfo: boolean;
    function UniqueName(AName: String; AnIgnoreIdx: Integer): String;
    procedure ShowPresetConf(AShow: boolean);
    procedure FillPresetList;
    procedure SetDisplayFormatConfig(AValue: TDisplayFormatConfig);
    procedure SaveButtonStates;
    procedure ApplyConfigs;
    procedure RetrieveConfigs;
    procedure SetShowOverrideChecks(AValue: boolean);
    procedure SetShowProjectInfo(AValue: boolean);

  public
    constructor Create(TheOwner: TComponent); override;
    procedure Setup;
    procedure SaveConfig;
    property DisplayFormatConfig: TDisplayFormatConfig read FDisplayFormatConfig write SetDisplayFormatConfig;
    property ShowProjectInfo: boolean read FShowProjectInfo write SetShowProjectInfo;
    property ShowOverrideChecks: boolean read FShowOverrideChecks write SetShowOverrideChecks;
  end;

implementation

{$R *.lfm}

{ TDisplayFormatDefaultsConfigFrame }

procedure TDisplayFormatDefaultsConfigFrame.btnGlobalClick(Sender: TObject);
begin
  ShowPresetConf(False);
  RetrieveConfigs;
  btnGlobal.Down  := True;
  btnHint.Down    := False;
  btnWatches.Down := False;
  btnLocals.Down  := False;
  btnInspect.Down := False;
  btnEvalMod.Down := False;
  DisplayFormatFrame1.ShowArrayNavBarOpts := True;
  lblChangingDescr.Caption := DispFormatOptChangingDescrAll;
  SaveButtonStates;
  ApplyConfigs;
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPreDelClick(Sender: TObject);
var
  i: Integer;
begin
  FCurrentPresetIdx := -1;
  i := lbPresets.ItemIndex;
  if (i < 0) or (i >= FDisplayFormatConfig.DisplayFormatPresetCount) then
    exit;
  FDisplayFormatConfig.DisplayFormatPresetList.Delete(i);
  FillPresetList;
  if i > 0 then
    dec(i);
  if i < lbPresets.Count then
    lbPresets.ItemIndex := i;
  lbPresetsClick(nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPreDefaultsClick(Sender: TObject);
var
  i, NewIdx: Integer;
  p: TWatchDisplayFormatPreset;
begin
  RetrieveConfigs;
  if FDisplayFormatConfig.DisplayFormatPresetList.Defaults = nil then
    exit;
  NewIdx := -1;
  for i := 0 to FDisplayFormatConfig.DisplayFormatPresetList.Defaults.Count - 1 do begin
    p := FDisplayFormatConfig.DisplayFormatPresetList.Defaults[i];
    if FDisplayFormatConfig.DisplayFormatPresetList.IndexOfFormat(p.DisplayFormat) < 0 then begin
      p.Name := UniqueName(p.Name, -1);
      if NewIdx < 0 then
        NewIdx := FDisplayFormatConfig.AddDisplayFormatPreset(p)
      else
        FDisplayFormatConfig.AddDisplayFormatPreset(p);
    end;
  end;

  if NewIdx < 0 then
    NewIdx := lbPresets.ItemIndex;
  FillPresetList;
  lbPresets.ItemIndex := NewIdx;
  lbPresetsClick(nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPreDownClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbPresets.ItemIndex;
  if (i < 0) or (i > FDisplayFormatConfig.DisplayFormatPresetCount-2) then
    exit;

  RetrieveConfigs;
  FDisplayFormatConfig.DisplayFormatPresetList.Move(i, i+1);
  inc(i);
  FillPresetList;
  lbPresets.ItemIndex := i;
  lbPresetsClick(nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPreNewClick(Sender: TObject);
var
  i: Integer;
  d: TWatchDisplayFormatPreset;
begin
  RetrieveConfigs;

  d.Init;
  i := FDisplayFormatConfig.AddDisplayFormatPreset(d);
  FillPresetList;
  lbPresets.ItemIndex := i;
  lbPresetsClick(nil);
  edPreName.SetFocus;
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPresetsClick(Sender: TObject);
begin
  ShowPresetConf(True);
  btnGlobal.Down  := False;
  btnHint.Down    := False;
  btnWatches.Down := False;
  btnLocals.Down  := False;
  btnInspect.Down := False;
  btnEvalMod.Down := False;

  DisplayFormatFrame1.ShowArrayNavBarOpts := True;
  DisplayFormatFrame1.ShowOverrideChecks := True;

  lblChangingDescr.Caption := DispFormatOptChangingDescrPreset;

  lbPresetsClick(nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.btnPreUpClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbPresets.ItemIndex;
  if i < 1 then
    exit;

  RetrieveConfigs;
  FDisplayFormatConfig.DisplayFormatPresetList.Move(i, i-1);
  dec(i);
  FillPresetList;
  lbPresets.ItemIndex := i;
  lbPresetsClick(nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.btnTargetClick(Sender: TObject);
begin
  ShowPresetConf(False);
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
  DisplayFormatFrame1.ShowArrayNavBarOpts := btnWatches.Down or btnLocals.Down or btnGlobal.Down;
  lblChangingDescr.Caption := DispFormatOptChangingDescrSome;
  SaveButtonStates;
  ApplyConfigs;
end;

procedure TDisplayFormatDefaultsConfigFrame.edPreNameEditingDone(Sender: TObject
  );
var
  i: Integer;
begin
  i := FCurrentPresetIdx;
  RetrieveConfigs;
  FCurrentPresetIdx := i;
end;

procedure TDisplayFormatDefaultsConfigFrame.lbPresetsClick(Sender: TObject);
var
  d: TWatchDisplayFormatPreset;
begin
  if FCurrentPresetIdx >= 0 then begin
    if FCurrentPresetIdx = lbPresets.ItemIndex then
      exit;
    RetrieveConfigs;
  end;

  SaveButtonStates;

  btnPreDel.Enabled  := (lbPresets.Count > 0) and (lbPresets.ItemIndex >= 0);
  btnPreUp.Enabled   := (lbPresets.Count > 0) and (lbPresets.ItemIndex > 0);
  btnPreDown.Enabled := (lbPresets.Count > 0) and (lbPresets.ItemIndex >= 0) and (lbPresets.ItemIndex < lbPresets.Count - 1);
  btnPreDefaults.Enabled := not FDisplayFormatConfig.DisplayFormatPresetList.HasAllPresets;

  if (lbPresets.Count = 0) or (lbPresets.ItemIndex < 0) or
     (lbPresets.ItemIndex >= FDisplayFormatConfig.DisplayFormatPresetCount)
  then begin
    DisplayFormatFrame1.Enabled := False;
    DisplayFormatFrame1.DisplayFormat := DefaultWatchDisplayFormat;
    pnlPresetOpts.Enabled := False;
    exit;
  end;

  pnlPresetOpts.Enabled := True;
  DisplayFormatFrame1.Enabled := True;

  FCurrentPresetIdx := lbPresets.ItemIndex;
  d := FDisplayFormatConfig.DisplayFormatPresets[FCurrentPresetIdx];
  edPreName.Text := d.Name;
  DisplayFormatFrame1.DisplayFormat := d.DisplayFormat;
end;

function TDisplayFormatDefaultsConfigFrame.UniqueName(AName: String; AnIgnoreIdx: Integer): String;
var
  i, j: Integer;
begin
  Result := AName;
  i := FDisplayFormatConfig.DisplayFormatPresetList.IndexOfName(Result);
  if (Result = '') or ( (i >= 0) and (i <> AnIgnoreIdx)) then begin
    if Result <> '' then
      Result := Result + '-';
    j := 1;
    while (i >= 0) and (i <> AnIgnoreIdx) do begin
      inc(j);
      i := FDisplayFormatConfig.DisplayFormatPresetList.IndexOfName(Result + IntToStr(j));
    end;
    Result := Result + IntToStr(j);
  end;
end;

procedure TDisplayFormatDefaultsConfigFrame.ShowPresetConf(AShow: boolean);
begin
  pnlPresetList.Visible := AShow;
  Splitter1.Visible     := AShow;
  pnlPresetOpts.Visible := AShow;
  btnPresets.Down       := AShow;
  lblProjectText.Visible := not AShow;
  if not AShow then
    DisplayFormatFrame1.Enabled := True;
end;

procedure TDisplayFormatDefaultsConfigFrame.FillPresetList;
var
  i: Integer;
begin
  lbPresets.Clear;
  for i := 0 to FDisplayFormatConfig.DisplayFormatPresetCount - 1 do
    lbPresets.AddItem(FDisplayFormatConfig.DisplayFormatPresets[i].Name, nil);
end;

procedure TDisplayFormatDefaultsConfigFrame.SetDisplayFormatConfig(AValue: TDisplayFormatConfig);
begin
  if FDisplayFormatConfig = AValue then Exit;
  FDisplayFormatConfig := AValue;
  btnPreDefaults.Visible := FDisplayFormatConfig.DisplayFormatPresetList.HasPresetsList;
  btnPreDefaults.Enabled := not FDisplayFormatConfig.DisplayFormatPresetList.HasAllPresets;
  FillPresetList;
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
  FButtonStatePreset := btnPresets.Down;
end;

procedure TDisplayFormatDefaultsConfigFrame.ApplyConfigs;
var
  j: Integer;
  t: TDisplayFormatTarget;
begin
  if FButtonStates[dtfGlobal] then
    DisplayFormatFrame1.ShowOverrideChecks := FShowOverrideChecks
  else
    DisplayFormatFrame1.ShowOverrideChecks := True;

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
  j, i: Integer;
  t: TDisplayFormatTarget;
  n: string;
  d: TWatchDisplayFormatPreset;
begin
  if FButtonStatePreset then begin
    if FCurrentPresetIdx >= 0 then begin
      n := edPreName.Text;
      n := UniqueName(n, FCurrentPresetIdx);
      d.Name := n;
      d.DisplayFormat := DisplayFormatFrame1.DisplayFormat;
      FDisplayFormatConfig.DisplayFormatPresets[FCurrentPresetIdx] := d;
      lbPresets.Items[FCurrentPresetIdx] := n;
    end;
    FCurrentPresetIdx := -1;
    exit;
  end;

  j := 0;
  for t in TDisplayFormatTarget do
    if FButtonStates[t] then begin
      DisplayFormatConfig.DefaultDisplayFormats[t] := DisplayFormatFrame1.DisplayFormats[j];
      inc(j);
      if j >= DisplayFormatFrame1.DisplayFormatCount then
        break;
    end;
end;

procedure TDisplayFormatDefaultsConfigFrame.SetShowOverrideChecks(AValue: boolean);
begin
  if FShowOverrideChecks = AValue then Exit;
  FShowOverrideChecks := AValue;
  if FButtonStates[dtfGlobal] then
    DisplayFormatFrame1.ShowOverrideChecks := AValue;
end;

procedure TDisplayFormatDefaultsConfigFrame.SetShowProjectInfo(AValue: boolean);
begin
  if FShowProjectInfo = AValue then Exit;
  FShowProjectInfo := AValue;
  lblProjectText.Visible := AValue;
end;

constructor TDisplayFormatDefaultsConfigFrame.Create(TheOwner: TComponent);
begin
  FShowOverrideChecks := True;
  FCurrentPresetIdx := -1;
  inherited Create(TheOwner);
end;

procedure TDisplayFormatDefaultsConfigFrame.Setup;
begin
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowCurrent := False;
  DisplayFormatFrame1.ShowMemDump := False;
  DisplayFormatFrame1.SelectDefaultButton;
  DisplayFormatFrame1.ShowExtraSettings := True;
  DisplayFormatFrame1.ShowArrayNavBarOpts := True;
  DisplayFormatFrame1.ShowFullAddressInMulti := True;
  DisplayFormatFrame1.ShowOverrideChecks := FShowOverrideChecks;

  btnGlobal.Down  := True;
  btnHint.Down    := False;
  btnWatches.Down := False;
  btnLocals.Down  := False;
  btnInspect.Down := False;
  btnEvalMod.Down := False;
  btnPresets.Down := False;

  btnGlobal.Caption  := DispFormatTargetGlobal;
  btnHint.Caption    := DispFormatTargetHint;
  btnWatches.Caption := DispFormatTargetWatches;
  btnLocals.Caption  := DispFormatTargetLocals;
  btnInspect.Caption := DispFormatTargetInspect;
  btnEvalMod.Caption := DispFormatTargetEvalMod;

  btnPresets.Caption := DispFormatTargetPreset;
  lblPreName.Caption := DispFormatPresetName;
  btnPreNew.Caption  := DispFormatPresetNew;
  btnPreDel.Caption  := DispFormatPresetDel;
  btnPreDefaults.Caption:= DispFormatPresetDefaults;
  btnPreUp.Caption   := DispFormatPresetUp;
  btnPreDown.Caption := DispFormatPresetDown;

  ShowPresetConf(False);
  lblChangingDescr.Caption := DispFormatOptChangingDescrAll;
  lblProjectText.Caption   := DispFormatOptProjectText;
end;

procedure TDisplayFormatDefaultsConfigFrame.SaveConfig;
begin
  RetrieveConfigs;
end;

end.

