unit IdeDebuggerWatchResult;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerUtils, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes, LazUTF8, Laz2_XMLCfg, StrUtils;

type

  TWatchResultDataKind = (
    rdkUnknown,
    rdkError, rdkPrePrinted,
    rdkString, rdkWideString,
    rdkSignedNumVal, rdkUnsignedNumVal, rdkPointerVal, rdkFloatVal
  );

  TWatchResultData = class;

  { TWatchResultValue }

  TWatchResultValue = object
  protected
    function GetAsString: String; inline;
    function GetAsWideString: WideString; inline;
    function GetAsQWord: QWord; inline;
    function GetAsInt64: Int64; inline;
    function GetAsFloat: Extended; inline;
    function GetByteSize: Integer; inline;
    function GetFloatPrecission: TLzDbgFloatPrecission; inline;
    function GetDerefData: TWatchResultData; inline;

    procedure AfterAssign;
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValueTextBase }

  TWatchResultValueTextBase = object(TWatchResultValue)
  private
    FText: String;
  protected
    property GetAsString: String read FText;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValuePrePrinted }

  TWatchResultValuePrePrinted = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkPrePrinted;
  end;

  { TWatchResultValueString }

  TWatchResultValueString = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkString;
  end;

  { TWatchResultValueWideString }

  TWatchResultValueWideString = object(TWatchResultValue)
  protected const
    VKind = rdkWideString;
  private
    FWideText: WideString;
  protected
    property GetAsWideString: WideString read FWideText;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValueOrdNumBase }

  TWatchResultValueOrdNumBase = object(TWatchResultValue)
  private
    FNumValue: QWord;
  protected
    property GetAsQWord: QWord read FNumValue;
    function GetAsInt64: Int64; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultTypeOrdNum }

  TWatchResultTypeOrdNum = object(TWatchResultValue)
  private
    FNumByteSize: Integer; // SmallInt
  protected
    property GetByteSize: Integer read FNumByteSize;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValueSignedNum }

  TWatchResultValueSignedNum = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkSignedNumVal;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValueUnsignedNum }

  TWatchResultValueUnsignedNum = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkUnsignedNumVal;
  protected
    function GetAsString: String; inline;
  end;

  { TWatchResultValuePointer }

  TWatchResultValuePointer = object(TWatchResultValueOrdNumBase)
  protected const
    VKind = rdkPointerVal;
  private
    FDerefData: TWatchResultData;
  protected
    function GetAsString: String; inline;
    property GetDerefData: TWatchResultData read FDerefData;
    procedure AfterAssign;
    procedure DoFree;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValueFloat }

  TWatchResultValueFloat = object(TWatchResultValue)
  protected const
    VKind = rdkFloatVal;
  private
    FFloatValue: Extended;
  protected
    property GetAsFloat: Extended read FFloatValue;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultTypeFloat }

  TWatchResultTypeFloat = object(TWatchResultValue)
  private
    FFloatPrecission: TLzDbgFloatPrecission;
  protected
    property FloatPrecission: TLzDbgFloatPrecission read FFloatPrecission;
    function GetAsString: String; inline;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string);
  end;

  { TWatchResultValueError }

  TWatchResultValueError = object(TWatchResultValueTextBase)
  protected const
    VKind = rdkError;
  end;

  TWatchResultDataClassID = (
    wdPrePrint,  // TWatchResultDataPrePrinted
    wdString,    // TWatchResultDataString
    wdWString,   // TWatchResultDataWideString
    wdSNum,      // TWatchResultDataSignedNum
    wdUNum,      // TWatchResultDataUnSignedNum
    wdPtr,       // TWatchResultDataPointer
    wdFloat,     // TWatchResultDataFloat
    wdErr        // TWatchResultDataError
  );

  { TWatchResultData }

  TWatchResultData = class // (TRefCountedObject)
  private
    FTypeName: String;
  //  ValidData: TWatchValueDataFlags;
  //  Addr: TDbgPtr;
  // MemDump
    function GetClassID: TWatchResultDataClassID; virtual; //abstract;
  protected
    function GetValueKind: TWatchResultDataKind; virtual; //abstract;
    function GetAsString: String; virtual; abstract;
    function GetAsWideString: WideString; virtual; abstract;
    function GetAsQWord: QWord; virtual; abstract;
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsFloat: Extended; virtual; abstract;
    function GetByteSize: Integer; virtual; abstract;
    function GetFloatPrecission: TLzDbgFloatPrecission; virtual; abstract;
    function GetDerefData: TWatchResultData; virtual; abstract;
  public
    class function CreateFromXMLConfig(const AConfig: TXMLConfig; const APath: string): TWatchResultData;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string); virtual;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string); virtual;
    procedure Assign(ASource: TWatchResultData); virtual;
    function  CreateCopy: TWatchResultData;

    procedure SetTypeName(ATypeName: String);

    property ValueKind: TWatchResultDataKind read GetValueKind;
    property TypeName: String read FTypeName;

    property AsString: String read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    property AsQWord: QWord read GetAsQWord;
    property AsInt64: Int64 read GetAsInt64;
    property AsFloat: Extended read GetAsFloat;

    property ByteSize: Integer read GetByteSize;
    property FloatPrecission: TLzDbgFloatPrecission read GetFloatPrecission;
    property DerefData: TWatchResultData read GetDerefData;
  end;

  TWatchResultDataClass = class of TWatchResultData;

  { TGenericWatchResultData }

  generic TGenericWatchResultData<_DATA> = class(TWatchResultData)
  private
    FData: _DATA;
  protected
    function GetValueKind: TWatchResultDataKind; override;
    function GetAsString: String; override;
    function GetAsWideString: WideString; override;
    function GetAsQWord: QWord; override;
    function GetAsInt64: Int64; override;
    function GetAsFloat: Extended; override;
    function GetDerefData: TWatchResultData; override;

    function GetByteSize: Integer; override;
    function GetFloatPrecission: TLzDbgFloatPrecission; override;
  public
    destructor Destroy; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string); override;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string); override;
    procedure Assign(ASource: TWatchResultData); override;
  end;

  { TGenericWatchResultDataWithType }

  generic TGenericWatchResultDataWithType<_DATA, _TYPE> = class(specialize TGenericWatchResultData<_DATA>)
  private
    FType: _TYPE;
  protected
    function GetByteSize: Integer; override;
    function GetFloatPrecission: TLzDbgFloatPrecission; override;
  public
    destructor Destroy; override;
    procedure LoadDataFromXMLConfig(const AConfig: TXMLConfig; const APath: string); override;
    procedure SaveDataToXMLConfig(const AConfig: TXMLConfig; const APath: string); override;
    procedure Assign(ASource: TWatchResultData); override;
  end;

  { TWatchResultDataPrePrinted }

  TWatchResultDataPrePrinted = class(specialize TGenericWatchResultData<TWatchResultValuePrePrinted>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(APrintedVal: String);
  end;

  { TWatchResultDataString }

  TWatchResultDataString = class(specialize TGenericWatchResultData<TWatchResultValueString>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AStringVal: String);
  end;

  { TWatchResultDataWideString }

  TWatchResultDataWideString = class(specialize TGenericWatchResultData<TWatchResultValueWideString>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AStringVal: WideString);
  end;

  { TWatchResultDataSignedNum }

  TWatchResultDataSignedNum = class(specialize TGenericWatchResultDataWithType<TWatchResultValueSignedNum, TWatchResultTypeOrdNum>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: Int64; AByteSize: Integer = 0);
  end;

  { TWatchResultDataUnSignedNum }

  TWatchResultDataUnSignedNum = class(specialize TGenericWatchResultDataWithType<TWatchResultValueUnsignedNum, TWatchResultTypeOrdNum>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(ANumValue: QWord; AByteSize: Integer = 0);
  end;

  { TWatchResultDataPointer }

  TWatchResultDataPointer = class(specialize TGenericWatchResultData<TWatchResultValuePointer>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    procedure SetDerefData(ADerefData: TWatchResultData);
  public
    constructor Create(AnAddr: TDBGPtr);
  end;

  { TWatchResultDataFloat }

  TWatchResultDataFloat = class(specialize TGenericWatchResultDataWithType<TWatchResultValueFloat, TWatchResultTypeFloat>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(AFloatValue: Extended; APrecission: TLzDbgFloatPrecission);
  end;

  { TWatchResultDataError }

  TWatchResultDataError = class(specialize TGenericWatchResultData<TWatchResultValueError>)
  private
    function GetClassID: TWatchResultDataClassID; override;
  public
    constructor Create(APrintedVal: String);
  end;


function PrintWatchValue(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat): String;

implementation

function PrintWatchValueEx(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

  function PrintNumber(ANumValue: TWatchResultData; AByteSize: Integer; AnIsPointer: Boolean; ADispFormat: TWatchDisplayFormat): String;
  var
    num: QWord;
    n, i, j: Integer;
  begin
    case ADispFormat of
      //wdfString: // get pchar(num)^ ?
      wdfChar: begin
        num := ANumValue.AsQWord;
        Result := '';
        while num <> 0 do begin
          Result := chr(num and $ff) + Result;
          num := num >> 8;
        end;
        if Result <> '' then begin
          i := 1;
          while i <= length(Result) do begin
            j := UTF8CodepointStrictSize(@Result[i]);
            if j = 0 then begin
              Result := copy(Result, 1, i-1) + '''#$'+ IntToHex(byte(Result[i]), 2) + '''' + copy(Result, i + 6, 99);
              inc(i, 6);
            end
            else
              inc(i, j);
          end;
          Result := '''' + Result + '''';
        end
        else
          Result := '#$00';
      end;
      wdfUnsigned: begin
        Result := IntToStr(ANumValue.AsQWord)
      end;
      wdfHex: begin
        n := HexDigicCount(ANumValue.AsQWord, AByteSize, AnIsPointer);
        Result := '$'+IntToHex(ANumValue.AsQWord, n);
      end;
      wdfBinary: begin
        n := HexDigicCount(ANumValue.AsQWord, AByteSize, AnIsPointer);
        Result := '%'+IntToBin(ANumValue.AsInt64, n*4);
      end;
      wdfPointer: begin
        n := HexDigicCount(ANumValue.AsQWord, AByteSize, True);
        Result := '$'+IntToHex(ANumValue.AsQWord, n);
      end;
      else begin // wdfDecimal
        Result := IntToStr(ANumValue.AsInt64);
      end;
    end;
  end;

var
  PointerValue: TWatchResultDataPointer absolute AResValue;
  ResTypeName: String;
  PtrDeref: TWatchResultData;
begin
  inc(ANestLvl);
  Result := '';
  case AResValue.ValueKind of
    rdkError:
      Result := 'Error: ' + AResValue.AsString;
    rdkPrePrinted: begin
      Result := AResValue.AsString;
    end;
    rdkSignedNumVal,
    rdkUnsignedNumVal: begin
      if (ADispFormat = wdfPointer) and (AResValue.AsQWord = 0) then begin
        Result := 'nil';
      end
      else begin
        if (AResValue.ValueKind = rdkUnsignedNumVal) and (ADispFormat = wdfDecimal) then
          ADispFormat := wdfUnsigned
        else
        if not (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary, wdfPointer]) then begin
          //wdfDefault, wdfStructure, wdfChar, wdfString, wdfFloat
          if AResValue.ValueKind = rdkUnsignedNumVal then
            ADispFormat := wdfUnsigned
          else
            ADispFormat := wdfDecimal;
        end;

        Result := PrintNumber(AResValue, AResValue.ByteSize, False, ADispFormat);
      end;
    end;
    rdkPointerVal: begin
      ResTypeName := '';
      if (ADispFormat = wdfStructure) or
         ((ADispFormat = wdfDefault) and (PointerValue.DerefData = nil))
      then
        ResTypeName := AResValue.TypeName;

      if (ADispFormat in [wdfDefault, wdfStructure, wdfPointer]) and (AResValue.AsQWord = 0)
      then begin
        Result := 'nil';
      end
      else begin
        if not (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary, wdfPointer]) then
          //wdfDefault, wdfStructure, wdfChar, wdfString, wdfFloat
          ADispFormat := wdfPointer;

        Result := PrintNumber(AResValue, 0, True, ADispFormat);
      end;

      if ResTypeName <> '' then
        Result := ResTypeName + '(' + Result + ')';

      PtrDeref :=  PointerValue.DerefData;
      if PtrDeref <> nil then begin
        while (PtrDeref.ValueKind = rdkPointerVal) and (PtrDeref.DerefData <> nil) do begin
          Result := Result + '^';
          PtrDeref :=  PtrDeref.DerefData;
        end;
        Result := Result + '^: ' + PrintWatchValueEx(PointerValue.DerefData, wdfDefault, ANestLvl);
      end;
    end;
    rdkFloatVal: begin
      case AResValue.FloatPrecission of
        dfpSingle:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral,  8, 0);
        dfpDouble:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 12, 0);
        dfpExtended: Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 15, 0);
      end;
    end;
    rdkString:     Result := QuoteText(AResValue.AsString);
    rdkWideString: Result := QuoteWideText(AResValue.AsWideString);
  end;
end;

function PrintWatchValue(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat): String;
begin
  Result := PrintWatchValueEx(AResValue, ADispFormat, -1);
end;

const
  WatchResNameToClass: array [TWatchResultDataClassID] of TWatchResultDataClass = (
    TWatchResultDataPrePrinted,    // wdPrePrint
    TWatchResultDataString,        // wdString
    TWatchResultDataWideString,    // wdWString
    TWatchResultDataSignedNum,     // wdSNum
    TWatchResultDataUnSignedNum,   // wdUNum
    TWatchResultDataPointer,       // wdPtr
    TWatchResultDataFloat,         // wdFloat
    TWatchResultDataError          // wdErr
  );

{ TWatchResultValue }

function TWatchResultValue.GetAsString: String;
begin
  Result := '';
end;

function TWatchResultValue.GetAsWideString: WideString;
begin
  Result := '';
end;

function TWatchResultValue.GetAsQWord: QWord;
begin
  Result := 0;
end;

function TWatchResultValue.GetAsInt64: Int64;
begin
  Result := 0;
end;

function TWatchResultValue.GetAsFloat: Extended;
begin
  Result := 0;
end;

function TWatchResultValue.GetByteSize: Integer;
begin
  Result := 0;
end;

function TWatchResultValue.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := dfpSingle;
end;

function TWatchResultValue.GetDerefData: TWatchResultData;
begin
  Result := nil;
end;

procedure TWatchResultValue.AfterAssign;
begin
  //
end;

procedure TWatchResultValue.DoFree;
begin
  //
end;

procedure TWatchResultValue.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  //
end;

procedure TWatchResultValue.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  //
end;

{ TWatchResultValueTextBase }

procedure TWatchResultValueTextBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FText := AConfig.GetValue(APath + 'Value', '');
end;

procedure TWatchResultValueTextBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetValue(APath + 'Value', FText);
end;

{ TWatchResultValueWideString }

function TWatchResultValueWideString.GetAsString: String;
begin
  Result := FWideText;
end;

procedure TWatchResultValueWideString.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FWideText := AConfig.GetValue(APath + 'Value', '');
end;

procedure TWatchResultValueWideString.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetValue(APath + 'Value', FWideText);
end;

{ TWatchResultValueOrdNumBase }

function TWatchResultValueOrdNumBase.GetAsInt64: Int64;
begin
  Result := Int64(FNumValue);
end;

procedure TWatchResultValueOrdNumBase.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FNumValue := QWord(AConfig.GetValue(APath + 'Value', int64(0)));
end;

procedure TWatchResultValueOrdNumBase.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetValue(APath + 'Value', Int64(FNumValue));
end;

{ TWatchResultTypeOrdNum }

procedure TWatchResultTypeOrdNum.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  FNumByteSize := AConfig.GetValue(APath + 'Value', 0);
end;

procedure TWatchResultTypeOrdNum.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetDeleteValue(APath + 'ByteSize', FNumByteSize, 0);
end;

{ TWatchResultValueSignedNum }

function TWatchResultValueSignedNum.GetAsString: String;
begin
  Result := IntToStr(Int64(FNumValue));
end;

{ TWatchResultValueUnsignedNum }

function TWatchResultValueUnsignedNum.GetAsString: String;
begin
  Result := IntToStr(QWord(FNumValue))
end;

{ TWatchResultValuePointer }

function TWatchResultValuePointer.GetAsString: String;
begin
  Result := '$'+IntToHex(QWord(FNumValue), HexDigicCount(FNumValue, 0, True));
  if FDerefData <> nil then
    Result := Result + '^: ' + FDerefData.AsString;
end;

procedure TWatchResultValuePointer.AfterAssign;
begin
  FDerefData := FDerefData.CreateCopy;
end;

procedure TWatchResultValuePointer.DoFree;
begin
  FDerefData.Free;
end;

procedure TWatchResultValuePointer.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  if AConfig.HasPath(APath + 'Deref', False) then
    FDerefData := TWatchResultData.CreateFromXMLConfig(AConfig, APath + 'Deref/');
end;

procedure TWatchResultValuePointer.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  if FDerefData <> nil then
    FDerefData.SaveDataToXMLConfig(AConfig, APath + 'Deref/')
  else
    AConfig.DeletePath(APath + 'Deref');
end;

{ TWatchResultValueFloat }

function TWatchResultValueFloat.GetAsString: String;
begin
  Result := FloatToStr(FFloatValue);
end;

procedure TWatchResultValueFloat.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FFloatValue := AConfig.GetExtendedValue(APath + 'Value', 0);
end;

procedure TWatchResultValueFloat.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetExtendedValue(APath + 'Value', FFloatValue);
end;

{ TWatchResultTypeFloat }

function TWatchResultTypeFloat.GetAsString: String;
begin
  WriteStr(Result, FFloatPrecission);
end;

procedure TWatchResultTypeFloat.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  AConfig.GetValue(APath + 'Prec', int64(ord(dfpSingle)), FFloatPrecission, TypeInfo(TLzDbgFloatPrecission));
end;

procedure TWatchResultTypeFloat.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  AConfig.SetDeleteValue(APath + 'Prec', FFloatPrecission, ord(dfpSingle), TypeInfo(TLzDbgFloatPrecission));
end;

{ TWatchResultData }

function TWatchResultData.GetValueKind: TWatchResultDataKind;
begin
  Result := rdkUnknown;
end;

function TWatchResultData.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPrePrint;
end;

class function TWatchResultData.CreateFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string): TWatchResultData;
var
  AnId: TWatchResultDataClassID;
begin
  Result := nil;
  try
    AConfig.GetValue(APath + 'CID', Int64(ord(wdPrePrint)), AnId, TypeInfo(TWatchResultDataClassID));
    Result := WatchResNameToClass[AnId].Create;
    Result.LoadDataFromXMLConfig(AConfig, APath);
  except
    Result := TWatchResultDataError.Create('Error: Failed to load from XML'); // TODO: create a class, that will not overwrite the broken xml
  end;
end;

procedure TWatchResultData.LoadDataFromXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  FTypeName := AConfig.GetValue(APath + 'TypeName', '');
end;

procedure TWatchResultData.SaveDataToXMLConfig(const AConfig: TXMLConfig;
  const APath: string);
begin
  AConfig.SetDeleteValue(APath + 'CID', GetClassID, int64(ord(wdPrePrint)), TypeInfo(TWatchResultDataClassID));
  AConfig.SetDeleteValue(APath + 'TypeName', FTypeName, '');
end;

procedure TWatchResultData.Assign(ASource: TWatchResultData);
begin
  FTypeName := ASource.FTypeName;
end;

function TWatchResultData.CreateCopy: TWatchResultData;
begin
  if Self = nil then
    exit(nil);
  Result := TWatchResultData(ClassType.Create);
  Result.Assign(Self);
end;

procedure TWatchResultData.SetTypeName(ATypeName: String);
begin
  FTypeName := ATypeName;
end;

{ TGenericWatchResultData }

function TGenericWatchResultData.GetValueKind: TWatchResultDataKind;
begin
  Result := FData.VKind;
end;

function TGenericWatchResultData.GetAsString: String;
begin
  Result := FData.GetAsString;
end;

function TGenericWatchResultData.GetAsWideString: WideString;
begin
  Result := FData.GetAsWideString;
end;

function TGenericWatchResultData.GetAsQWord: QWord;
begin
  Result := FData.GetAsQWord;
end;

function TGenericWatchResultData.GetAsInt64: Int64;
begin
  Result := FData.GetAsInt64;
end;

function TGenericWatchResultData.GetAsFloat: Extended;
begin
  Result := FData.GetAsFloat;
end;

function TGenericWatchResultData.GetDerefData: TWatchResultData;
begin
  Result := FData.GetDerefData;
end;

function TGenericWatchResultData.GetByteSize: Integer;
begin
  Result := FData.GetByteSize;
end;

function TGenericWatchResultData.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := FData.GetFloatPrecission;
end;

destructor TGenericWatchResultData.Destroy;
begin
  FData.DoFree;
  inherited Destroy;
end;

procedure TGenericWatchResultData.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FData.LoadDataFromXMLConfig(AConfig, APath);
end;

procedure TGenericWatchResultData.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  FData.SaveDataToXMLConfig(AConfig, APath);
end;

procedure TGenericWatchResultData.Assign(ASource: TWatchResultData);
var
  Src: TGenericWatchResultData absolute ASource;
begin
  inherited Assign(ASource);
  if not (ASource is TGenericWatchResultData) then
    exit;
  FData := Src.FData;
  FData.AfterAssign;
end;

{ TGenericWatchResultDataWithType }

function TGenericWatchResultDataWithType.GetByteSize: Integer;
begin
  Result := FType.GetByteSize;
end;

function TGenericWatchResultDataWithType.GetFloatPrecission: TLzDbgFloatPrecission;
begin
  Result := FType.GetFloatPrecission;
end;

destructor TGenericWatchResultDataWithType.Destroy;
begin
  FType.DoFree;
  inherited Destroy;
end;

procedure TGenericWatchResultDataWithType.LoadDataFromXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited LoadDataFromXMLConfig(AConfig, APath);
  FType.LoadDataFromXMLConfig(AConfig, APath);
end;

procedure TGenericWatchResultDataWithType.SaveDataToXMLConfig(
  const AConfig: TXMLConfig; const APath: string);
begin
  inherited SaveDataToXMLConfig(AConfig, APath);
  FType.SaveDataToXMLConfig(AConfig, APath);
end;

procedure TGenericWatchResultDataWithType.Assign(ASource: TWatchResultData);
var
  Src: TGenericWatchResultDataWithType absolute ASource;
begin
  inherited Assign(ASource);
  if not (ASource is TGenericWatchResultDataWithType) then
    exit;
  FType := Src.FType;
  FType.AfterAssign;
end;

{ TWatchResultDataPrePrinted }

function TWatchResultDataPrePrinted.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPrePrint;
end;

constructor TWatchResultDataPrePrinted.Create(APrintedVal: String);
begin
  inherited Create;
  FData.FText := APrintedVal;
end;

{ TWatchResultDataString }

function TWatchResultDataString.GetClassID: TWatchResultDataClassID;
begin
  Result := wdString;
end;

constructor TWatchResultDataString.Create(AStringVal: String);
begin
  inherited Create;
  FData.FText := AStringVal;
end;

{ TWatchResultDataWideString }

function TWatchResultDataWideString.GetClassID: TWatchResultDataClassID;
begin
  Result := wdWString;
end;

constructor TWatchResultDataWideString.Create(AStringVal: WideString);
begin
  inherited Create;
  FData.FWideText := AStringVal;
end;

{ TWatchResultDataSignedNum }

function TWatchResultDataSignedNum.GetClassID: TWatchResultDataClassID;
begin
  Result := wdSNum;
end;

constructor TWatchResultDataSignedNum.Create(ANumValue: Int64;
  AByteSize: Integer);
begin
  inherited Create();
  FData.FNumValue := QWord(ANumValue);
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataUnSignedNum }

function TWatchResultDataUnSignedNum.GetClassID: TWatchResultDataClassID;
begin
  Result := wdUNum;
end;

constructor TWatchResultDataUnSignedNum.Create(ANumValue: QWord;
  AByteSize: Integer);
begin
  inherited Create();
  FData.FNumValue := QWord(ANumValue);
  FType.FNumByteSize := AByteSize;
end;

{ TWatchResultDataPointer }

function TWatchResultDataPointer.GetClassID: TWatchResultDataClassID;
begin
  Result := wdPtr;
end;

procedure TWatchResultDataPointer.SetDerefData(ADerefData: TWatchResultData);
begin
  FData.FDerefData := ADerefData;
end;

constructor TWatchResultDataPointer.Create(AnAddr: TDbgPtr);
begin
  inherited Create();
  FData.FNumValue := QWord(AnAddr);
end;


{ TWatchResultDataFloat }

function TWatchResultDataFloat.GetClassID: TWatchResultDataClassID;
begin
  Result := wdFloat;
end;

constructor TWatchResultDataFloat.Create(AFloatValue: Extended;
  APrecission: TLzDbgFloatPrecission);
begin
  inherited Create;
  FData.FFloatValue := AFloatValue;
  FType.FFloatPrecission := APrecission;
end;

{ TWatchResultDataError }

function TWatchResultDataError.GetClassID: TWatchResultDataClassID;
begin
  Result := wdErr;
end;

constructor TWatchResultDataError.Create(APrintedVal: String);
begin
  inherited Create;
  FData.FText := APrintedVal;
end;

end.

