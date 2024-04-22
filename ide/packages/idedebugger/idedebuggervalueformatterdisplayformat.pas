unit IdeDebuggerValueFormatterDisplayFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ExtCtrls, StdCtrls, DisplayFormatConfigFrame,
  IdeDebuggerDisplayFormats,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // LazUtils
  Laz2_XMLCfg,
  // IdeDebugger
  IdeDebuggerStringConstants;

type

  { TIdeDebuggerValueFormatterDisplayFormatFrame }

  TIdeDebuggerValueFormatterDisplayFormatFrame = class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    cbOverrideWatch: TCheckBox;
    DisplayFormatFrame1: TDisplayFormatFrame;
    Panel1: TPanel;
  protected
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TIdeDbgValueFormatterDisplayFormat }

  TIdeDbgValueFormatterDisplayFormat = class(
    specialize TLazDbgIdeValueFormatterGeneric<TObject>,
    ILazDbgIdeValueFormatterConfigStorageIntf
  )
  private
    FDisplayFormat: TWatchDisplayFormat;
    FOverrideWatch: boolean;
  protected
    procedure Init; override;
    procedure Assign(AnOther: TObject); override;
  public
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override; experimental;

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
//    function SupportedDataKinds: TWatchResultDataKinds; override;

    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
  published
    property OverrideWatch: boolean read FOverrideWatch write FOverrideWatch;
  end;

  TIdeDbgValueFormatterRegistryDisplayFormat =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterDisplayFormat, TIdeDebuggerValueFormatterDisplayFormatFrame>;

implementation

{$R *.lfm}

{ TIdeDebuggerValueFormatterDisplayFormatFrame }

procedure TIdeDebuggerValueFormatterDisplayFormatFrame.ReadFrom(
  AFormatter: ILazDbgIdeValueFormatterIntf);
var
  vf: TIdeDbgValueFormatterDisplayFormat;
begin
  vf := TIdeDbgValueFormatterDisplayFormat(AFormatter.GetObject);
  cbOverrideWatch.Checked := vf.OverrideWatch;
  DisplayFormatFrame1.DisplayFormat := vf.FDisplayFormat;
  DisplayFormatFrame1.SelectDefaultButton;
end;

function TIdeDebuggerValueFormatterDisplayFormatFrame.WriteTo(
  AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
var
  vf: TIdeDbgValueFormatterDisplayFormat;
  df: TWatchDisplayFormat;
begin
  vf := TIdeDbgValueFormatterDisplayFormat(AFormatter.GetObject);
  df := DisplayFormatFrame1.DisplayFormat;
  Result := (vf.FDisplayFormat <> df) or
            (vf.OverrideWatch <> cbOverrideWatch.Checked);

  vf.OverrideWatch := cbOverrideWatch.Checked;
  vf.FDisplayFormat := df;
end;

constructor TIdeDebuggerValueFormatterDisplayFormatFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DisplayFormatFrame1.Setup;
  DisplayFormatFrame1.ShowCurrent := False;
  DisplayFormatFrame1.ShowAll := True;
  DisplayFormatFrame1.AllowMultiTabs := True;
  DisplayFormatFrame1.ShowExtraSettings := False;
  DisplayFormatFrame1.ShowMemDump := False;
  DisplayFormatFrame1.ShowOverrideChecks := True;

  cbOverrideWatch.Caption := 'Override watch settings';
end;

{ TIdeDbgValueFormatterDisplayFormat }

procedure TIdeDbgValueFormatterDisplayFormat.Init;
begin
  FDisplayFormat := DefaultWatchDisplayFormat;
  FOverrideWatch := False;
end;

procedure TIdeDbgValueFormatterDisplayFormat.Assign(AnOther: TObject);
var
  f: TIdeDbgValueFormatterDisplayFormat;
begin
  inherited Assign(AnOther);
  if AnOther is TIdeDbgValueFormatterDisplayFormat then begin
    f := AnOther as TIdeDbgValueFormatterDisplayFormat;

    FDisplayFormat     := f.FDisplayFormat;
    FOverrideWatch     := f.FOverrideWatch;
  end;
end;

class function TIdeDbgValueFormatterDisplayFormat.GetRegisteredDisplayName: String;
begin
  Result := optDispGutterCustomDisplayformat;
end;

function TIdeDbgValueFormatterDisplayFormat.FormatValue(AWatchValue: IWatchResultDataIntf;
  ADisplayFormat: TWatchDisplayFormat; AWatchResultPrinter: IWatchResultPrinter; out
  APrintedValue: String): Boolean;
var
  d: TWatchDisplayFormat;
begin
  Result := True;
  if FOverrideWatch then begin
    d := FDisplayFormat;
    d.CopyInheritedFrom(ADisplayFormat);
  end
  else begin
    d := ADisplayFormat;
    d.CopyInheritedFrom(FDisplayFormat);
  end;
  APrintedValue := AWatchResultPrinter.PrintWatchValue(AWatchValue, d, [wpfUseCurrentValueFormatterList]);
end;

function TIdeDbgValueFormatterDisplayFormat.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffValueData, vffSkipOnRecursion, vffPreventOrigValue];
end;

procedure TIdeDbgValueFormatterDisplayFormat.LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
begin
  LoadDisplayFormatFromXMLConfig(AConfig, APath + 'DispFormat/', FDisplayFormat);
end;

procedure TIdeDbgValueFormatterDisplayFormat.SaveDataToXMLConfig(const AConfig: TRttiXMLConfig;
  const APath: string);
begin
  SaveDisplayFormatToXMLConfig(AConfig, APath + 'DispFormat/', FDisplayFormat);
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryDisplayFormat);

end.

