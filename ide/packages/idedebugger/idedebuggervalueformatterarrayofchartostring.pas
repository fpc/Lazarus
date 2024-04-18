unit IdeDebuggerValueFormatterArrayOfCharToString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // IdeIntf
  IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDebuggerUtils;

type

  { TIdeDbgValueFormatterCharArrayToString }

  TIdeDbgValueFormatterCharArrayToString = class(specialize TLazDbgIdeValueFormatterGeneric<TObject>)
  public
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override;
    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
    function SupportedDataKinds: TWatchResultDataKinds; override;
  end;

  TIdeDbgValueFormatterRegistryCharArrayToString =
    specialize TLazDbgIdeValueFormatterRegistryEntryGeneric<TIdeDbgValueFormatterCharArrayToString>;


implementation

{ TIdeDbgValueFormatterCharArrayToString }

class function TIdeDbgValueFormatterCharArrayToString.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterCharArrayToStringName;
end;

function TIdeDbgValueFormatterCharArrayToString.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
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
      p^ := AElem.AsString[1];

      i := 1;
      while i < Cnt do begin
        AWatchValue.SetSelectedIndex(i);
        AElem := AWatchValue.SelectedEntry;
        inc(p);
        p^ := AElem.AsString[1];
        inc(i);
      end;

      APrintedValue := QuoteText(APrintedValue);
    end;
    2: begin
      SetLength(AWideValue, Cnt);
      pw := @AWideValue[1];
      pw^ := AElem.AsWideString[1];

      i := 1;
      while i < Cnt do begin
        AWatchValue.SetSelectedIndex(i);
        AElem := AWatchValue.SelectedEntry;
        inc(pw);
        pw^ := AElem.AsString[1];
        inc(i);
      end;

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

