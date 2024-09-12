unit IdeDebuggerValueFormatterArrayOfCharToString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  StdCtrls, Forms,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerUtils;

type

  { TIdeDebuggerValueFormatterCharArrayToStringFrame }

  TIdeDebuggerValueFormatterCharArrayToStringFrame = class(TFrame, ILazDbgIdeValueFormatterSettingsFrameIntf)
    cbStopAtNull: TCheckBox;
  protected
    procedure ReadFrom(AFormatter: ILazDbgIdeValueFormatterIntf);
    function  WriteTo(AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TIdeDbgValueFormatterCharArrayToString }

  TIdeDbgValueFormatterCharArrayToString = class(specialize TLazDbgIdeValueFormatterGeneric<TObject>)
  private
    FStopAtNull: Boolean;
  protected
    procedure Init; override;
    procedure Assign(AnOther: TObject); override;
  public
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      const ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override;
    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
    function SupportedDataKinds: TWatchResultDataKinds; override;
  published
    property StopAtNull: Boolean read FStopAtNull write FStopAtNull;
  end;

  TIdeDbgValueFormatterRegistryCharArrayToString =
    specialize TLazDbgIdeValueFormatterFrameRegistryEntryGeneric<TIdeDbgValueFormatterCharArrayToString, TIdeDebuggerValueFormatterCharArrayToStringFrame>;


implementation

{$R *.lfm}

{ TIdeDebuggerValueFormatterCharArrayToStringFrame }

procedure TIdeDebuggerValueFormatterCharArrayToStringFrame.ReadFrom(
  AFormatter: ILazDbgIdeValueFormatterIntf);
var
  f: TIdeDbgValueFormatterCharArrayToString;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterCharArrayToString;
  cbStopAtNull.Checked := f.StopAtNull;
end;

function TIdeDebuggerValueFormatterCharArrayToStringFrame.WriteTo(
  AFormatter: ILazDbgIdeValueFormatterIntf): Boolean;
var
  f: TIdeDbgValueFormatterCharArrayToString;
begin
  f := AFormatter.GetObject as TIdeDbgValueFormatterCharArrayToString;

  Result := (f.StopAtNull <> cbStopAtNull.Checked);
  f.StopAtNull := cbStopAtNull.Checked;
end;

constructor TIdeDebuggerValueFormatterCharArrayToStringFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  cbStopAtNull.Caption := ValFormatterCharArrayToStringStopNull;
end;

{ TIdeDbgValueFormatterCharArrayToString }

procedure TIdeDbgValueFormatterCharArrayToString.Init;
begin
  inherited Init;
  FStopAtNull := False;
end;

procedure TIdeDbgValueFormatterCharArrayToString.Assign(AnOther: TObject);
var
  f: TIdeDbgValueFormatterCharArrayToString;
begin
  inherited Assign(AnOther);

  if AnOther is TIdeDbgValueFormatterCharArrayToString then begin
    f := AnOther as TIdeDbgValueFormatterCharArrayToString;

    FStopAtNull     := f.FStopAtNull;
  end;
end;

class function TIdeDbgValueFormatterCharArrayToString.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterCharArrayToStringName;
end;

function TIdeDbgValueFormatterCharArrayToString.FormatValue(
  AWatchValue: IWatchResultDataIntf; const ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
var
  Cnt, i: Integer;
  AElem: IWatchResultDataIntf;
  AWideValue: WideString;
  p: PChar;
  pw: PWideChar;
begin
  Result := (AWatchValue.ValueKind in [rdkArray]);
  if not Result then
    exit;

  Cnt := AWatchValue.Count;
  if Cnt = 0 then
    exit(False);

  AWatchValue.SetSelectedIndex(0);
  AElem := AWatchValue.SelectedEntry;

  Result := AElem.ValueKind = rdkChar;
  if not Result then
    exit;

  case AElem.ByteSize of
    1: begin
      SetLength(APrintedValue, Cnt);
      p := @APrintedValue[1];
      i := 0;
      while i < Cnt do begin
        AWatchValue.SetSelectedIndex(i);
        AElem := AWatchValue.SelectedEntry;
        p^ := AElem.AsString[1];
        if StopAtNull and (p^ = #0) then
          break;
        inc(p);
        inc(i);
      end;
      if i < Cnt then
        SetLength(APrintedValue, i);

      APrintedValue := QuoteText(APrintedValue);
    end;
    2: begin
      SetLength(AWideValue, Cnt);
      pw := @AWideValue[1];
      i := 0;
      while i < Cnt do begin
        AWatchValue.SetSelectedIndex(i);
        AElem := AWatchValue.SelectedEntry;
        pw^ := AElem.AsWideString[1];
        if StopAtNull and (pw^ = #0) then
          break;
        inc(pw);
        inc(i);
      end;
      if i < Cnt then
        SetLength(AWideValue, i);

      APrintedValue := QuoteWideText(AWideValue);
    end;
    else
      Result := False;
  end;
end;

function TIdeDbgValueFormatterCharArrayToString.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffValueData];
end;

function TIdeDbgValueFormatterCharArrayToString.SupportedDataKinds: TWatchResultDataKinds;
begin
  Result := [rdkArray];
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryCharArrayToString);

end.

