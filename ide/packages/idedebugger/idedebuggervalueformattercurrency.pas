unit IdeDebuggerValueFormatterCurrency;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math,
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfBaseTypes,
  // LazDebuggerIntf
  LazDebuggerIntf,
  // IdeIntf
   LazIDEIntf, IdeDebuggerValueFormatterIntf, IdeDebuggerWatchValueIntf,
  // IdeDebugger
  IdeDebuggerOpts, IdeDebuggerValueFormatter, IdeDebuggerStringConstants;

type

  { TIdeDbgValueFormatterCurrency }

  TIdeDbgValueFormatterCurrency = class(specialize TLazDbgIdeValueFormatterGeneric<TObject>)
  public
    class function GetRegisteredDisplayName: String;
    function FormatValue(AWatchValue: IWatchResultDataIntf;
      ADisplayFormat: TWatchDisplayFormat;
      AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String
      ): Boolean; override; experimental;
    function FormatValue(aDBGType: TDBGType;
                         aValue: string;
                         ADisplayFormat: TWatchDisplayFormat;
                         out APrintedValue: String
                        ): boolean; override; deprecated 'For values from older backends only - to be removed as backends are upgraded';

    function SupportedFeatures: TLazDbgIdeValFormatterFeatures; override;
  end;
  TIdeDbgValueFormatterRegistryCurrency =
    specialize TLazDbgIdeValueFormatterRegistryEntryGeneric<TIdeDbgValueFormatterCurrency>;


implementation

{ TIdeDbgValueFormatterCurrency }

class function TIdeDbgValueFormatterCurrency.GetRegisteredDisplayName: String;
begin
  Result := ValFormatterCurrencyName;
end;

function TIdeDbgValueFormatterCurrency.FormatValue(
  AWatchValue: IWatchResultDataIntf; ADisplayFormat: TWatchDisplayFormat;
  AWatchResultPrinter: IWatchResultPrinter; out APrintedValue: String): Boolean;
begin
  Result := (AWatchValue.ValueKind in [rdkUnsignedNumVal, rdkSignedNumVal]);
  if not Result then
    exit;

  case AWatchValue.ValueKind of
    rdkUnsignedNumVal: APrintedValue := IntToStr(AWatchValue.AsQWord);
    rdkSignedNumVal:   APrintedValue := IntToStr(AWatchValue.AsInt64);
  end;

  if APrintedValue[1] in ['-','+'] then begin
    while length(APrintedValue) < 5 do
      insert('0', APrintedValue, 2);
  end
  else begin
    while length(APrintedValue) < 4 do
      APrintedValue := '0' + APrintedValue;
  end;

  Insert('.', APrintedValue, Length(APrintedValue)-3);
end;

function TIdeDbgValueFormatterCurrency.FormatValue(aDBGType: TDBGType;
  aValue: string; ADisplayFormat: TWatchDisplayFormat; out APrintedValue: String
  ): boolean;
var
  i: int64;
  u: QWord;
begin
  APrintedValue := trim(aValue);
  Result := (APrintedValue <> '') and ( TryStrToInt64(APrintedValue, i) or TryStrToQWord(APrintedValue, u) );
  if not Result then
    exit;

  if APrintedValue[1] in ['-','+'] then begin
    while length(APrintedValue) < 5 do
      insert('0', APrintedValue, 2);
  end
  else begin
    while length(APrintedValue) < 4 do
      APrintedValue := '0' + APrintedValue;
  end;

  Insert('.', APrintedValue, Length(APrintedValue)-3);
end;

function TIdeDbgValueFormatterCurrency.SupportedFeatures: TLazDbgIdeValFormatterFeatures;
begin
  Result := [vffFormatValue, vffFormatOldValue, vffValueData];
end;

initialization
  ValueFormatterRegistry.Add(TIdeDbgValueFormatterRegistryCurrency);

end.

