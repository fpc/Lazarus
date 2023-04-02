unit FpDebugFpcConvVariantNormalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FpDebugValueConvertors, FpDebugDebuggerBase,
  FpDebugStringConstants, LazDebuggerValueConverter, LazDebuggerIntf, FpDbgInfo,
  FpDbgUtil, FpErrorMessages, FpDbgDwarf, FpdMemoryTools, DbgIntfBaseTypes;

type

  (**** Display variant as single/simple value ****)


  { TFpDbgValueConverterVariantNormalizer }

  TFpDbgValueConverterVariantNormalizer = class(TFpDbgValueConverter)
  protected
    //function GetSettingsFrame: ILazDbgValueConverterSettingsFrameIntf; override;  // TConverterSettingsFrameBase
  public
    class function GetName: String; override;
    function GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass; override;
    function NeedConversionLimit: Boolean; override;
    function CanHandleValue(ASourceValue: TFpValue; AnFpDebugger: TFpDebugDebuggerBase): Boolean; override;
    function ConvertValue(ASourceValue: TFpValue;
                          AnFpDebugger: TFpDebugDebuggerBase;
                          AnExpressionScope: TFpDbgSymbolScope;
                          var AnResData: IDbgWatchDataIntf
                         ): TFpValue; override;
  end;

  { TFpDbgValueConverterVariantNormalizerRegistryEntry }

  TFpDbgValueConverterVariantNormalizerRegistryEntry = class(TFpDbgValueConverterRegistryEntry)
  public
    class function GetConvertorClass: TClass; override;
  end;


implementation

{ TFpDbgValueConverterVariantNormalizer }

class function TFpDbgValueConverterVariantNormalizer.GetName: String;
begin
  Result := drsConverterNormalizeVariant;
end;

function TFpDbgValueConverterVariantNormalizer.GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass;
begin
  Result := TFpDbgValueConverterVariantNormalizerRegistryEntry;
end;

function TFpDbgValueConverterVariantNormalizer.NeedConversionLimit: Boolean;
begin
  Result := False;
end;

function TFpDbgValueConverterVariantNormalizer.CanHandleValue(
  ASourceValue: TFpValue; AnFpDebugger: TFpDebugDebuggerBase): Boolean;
var
  m, m2: TFpValue;
begin
  Result := (ASourceValue.Kind = skRecord) and (AnFpDebugger.DbgController.CurrentProcess <> nil);
  if (not Result) then
    exit;

  if AnFpDebugger.DbgController.CurrentProcess.Mode = dm32 then
    Result := (ASourceValue.DataSize.Size = 16)
  else
    Result := (ASourceValue.DataSize.Size = 24);

  if (not Result) then
    exit;

  if (ASourceValue.MemberCount = 1) then begin
    m := ASourceValue.Member[0];
    Result := (m <> nil);
    if not Result then begin
      exit;
      m.ReleaseReference;
    end;

    m2 := m.Member[-1];
    m.ReleaseReference;
    Result := (m2 <> nil) and (m2.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> []);
    m2.ReleaseReference;

    exit;
  end
  else
  begin
    m := ASourceValue.MemberByName['vtype'];
    Result := (m <> nil);
    m.ReleaseReference;
    exit;
  end;

  Result := False;
end;

function TFpDbgValueConverterVariantNormalizer.ConvertValue(
  ASourceValue: TFpValue; AnFpDebugger: TFpDebugDebuggerBase;
  AnExpressionScope: TFpDbgSymbolScope; var AnResData: IDbgWatchDataIntf
  ): TFpValue;

  procedure ReturnSourceValue;
  begin
    Result.ReleaseReference;
    ASourceValue.AddReference;
    Result := ASourceValue;
  end;

  procedure ReturnNil;
  begin
    Result.ReleaseReference;
    Result := nil;
  end;

  procedure HandleAnsiString;
  var
    Addr: TFpDbgMemLocation;
    sz: Integer;
    SLen: Int64;
    Str: string;
  begin
    if (Result = nil) then begin
      ReturnSourceValue;
      exit;
    end;
    if (Result.Kind <> skPointer) then
      exit;

    Addr := Result.DerefAddress;
    if not IsTargetNotNil(Addr) then
      exit;

    sz := AnExpressionScope.LocationContext.SizeOfAddress;
    Addr.Address := Addr.Address - sz;
    AnExpressionScope.LocationContext.ReadSignedInt(Addr, SizeVal(sz), SLen);
    if IsError(AnExpressionScope.LocationContext.LastMemError) then begin
      AnResData.CreateError('<Error reading string>');
      ReturnNil;
      exit;
    end;

    Addr.Address := Addr.Address + sz;
    if SLen > AnExpressionScope.LocationContext.MemManager.MemLimits.MaxStringLen then
      SLen := AnExpressionScope.LocationContext.MemManager.MemLimits.MaxStringLen;
    SetLength(Str, SLen);
    if SLen > 0 then
      AnExpressionScope.LocationContext.ReadMemory(Addr, SizeVal(SLen), @Str[1]);
    if IsError(AnExpressionScope.LocationContext.LastMemError) then
      AnResData.CreateError('<Error reading string>')
    else
      AnResData.CreateString(Str);
    ReturnNil;
  end;

var
  m, m2, vtype: TFpValue;
  discr: Int64;
  i: Integer;
  t: TFpSymbol;
  OldResData: IDbgWatchDataIntf;
begin
  Result := nil;

  OldResData := AnResData;
  AnResData := AnResData.CreateVariantValue();
  t := ASourceValue.TypeInfo;
  if (t <> nil) then
    OldResData.SetTypeName(t.Name);


  if ASourceValue.MemberCount = 1 then begin
    // dwarf 3
    m := ASourceValue.Member[0];
    if m = nil then begin
      ReturnSourceValue;
      exit;
    end;

    vtype := m.Member[-1];
    if vtype = nil then begin
      m.ReleaseReference;
      ReturnSourceValue;
      exit;
    end;

    discr := vtype.AsInteger;
    try
      for i := 0 to m.MemberCount - 1 do begin
        m2 := m.Member[i];
        try
          if (m2 <> nil) and
             (m2.DbgSymbol is TFpSymbolDwarfTypeVariant) and
             (TFpSymbolDwarfTypeVariant(m2.DbgSymbol).MatchesDiscr(discr)) and
             (m2.MemberCount = 1)
          then begin
            Result := m2.Member[0];

            case discr of
              0: if Result = nil then AnResData.CreatePrePrinted('<Empty>');
              1: if Result = nil then AnResData.CreatePrePrinted('<Null>');
              256: HandleAnsiString;
              otherwise
                if Result = nil then ReturnSourceValue;
            end;

            exit;
          end;
        finally
          m2.ReleaseReference;
        end;
      end;
    finally
      m.ReleaseReference;
      vtype.ReleaseReference;
    end;
  end
  else begin
    // dwarf 2
    vtype := ASourceValue.MemberByName['vtype'];
    try
      i := vtype.AsInteger;
      case i of
           0: AnResData.CreatePrePrinted('<Empty>');
           1: AnResData.CreatePrePrinted('<Null>');
           2: Result := ASourceValue.MemberByName['VSMALLINT'];
           3: Result := ASourceValue.MemberByName['VINTEGER'];
           4: Result := ASourceValue.MemberByName['VSINGLE'];
           5: Result := ASourceValue.MemberByName['VDOUBLE'];
           6: Result := ASourceValue.MemberByName['VCURRENCY'];
           7: Result := ASourceValue.MemberByName['VDATE'];
           8: Result := ASourceValue.MemberByName['VOLESTR'];
           9: Result := ASourceValue.MemberByName['VDISPATCH'];
          10: Result := ASourceValue.MemberByName['VERROR'];
          11: Result := ASourceValue.MemberByName['VBOOLEAN'];
          //12: varVariant
          13: Result := ASourceValue.MemberByName['VUNKNOWN'];
          //14: varDecimal
          //15??
          16: Result := ASourceValue.MemberByName['VSHORTINT'];
          17: Result := ASourceValue.MemberByName['VBYTE'];
          18: Result := ASourceValue.MemberByName['VWORD'];
          19: Result := ASourceValue.MemberByName['VLONGWORD'];
          20: Result := ASourceValue.MemberByName['VINT64'];
          21: Result := ASourceValue.MemberByName['VQWORD'];
          22: Result := ASourceValue.MemberByName['VWORD'];
          //
          36: Result := ASourceValue.MemberByName['VRECORD'];
          //72
         256: Result := ASourceValue.MemberByName['VSTRING'];
         257: Result := ASourceValue.MemberByName['VANY'];
         258: Result := ASourceValue.MemberByName['VUSTRING'];
         //258: varUString
        8192: Result := ASourceValue.MemberByName['VARRAY'];
       16384: Result := ASourceValue.MemberByName['VPOINTER'];
      end;

      if Result = nil then begin
        ReturnSourceValue;
      end
      else
      case i of
        256: begin
            HandleAnsiString;
            exit;
          end;
      end;
    finally
      vtype.ReleaseReference;
    end;
    exit;
  end;

  if Result = nil then
    ReturnSourceValue;
end;

{ TFpDbgValueConverterVariantNormalizerRegistryEntry }

class function TFpDbgValueConverterVariantNormalizerRegistryEntry.GetConvertorClass: TClass;
begin
  Result := TFpDbgValueConverterVariantNormalizer;
end;

initialization
  ValueConverterRegistry.Add(TFpDbgValueConverterVariantNormalizerRegistryEntry);

end.

