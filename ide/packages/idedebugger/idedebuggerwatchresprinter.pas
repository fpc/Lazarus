unit IdeDebuggerWatchResPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, IdeDebuggerWatchResult, IdeDebuggerUtils,
  LazDebuggerIntf, LazUTF8, IdeDebuggerWatchValueIntf, StrUtils;

type

  TResolvedDisplayFormat = record
    NumBaseFormat:           TValueDisplayFormatBase;
    NumSignFormat:           TValueDisplayFormatSign;
    NumCharFormat:           TValueDisplayFormatNumChar;
    EnumFormat:              TValueDisplayFormatEnum; // Also bool,char
    FloatFormat:             TValueDisplayFormatFloat;
    StructFormat:            TValueDisplayFormatStruct;
    StructAddrFormat:        TValueDisplayFormatStructAddr;
    PointerFormat:           TValueDisplayFormatPointer;
    PointerDerefFormat:      TValueDisplayFormatPointerDeref;
  end;

  { TDisplayFormatResolver }

  TDisplayFormatResolver = class
  private
    function DoResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
      const AResValue: TWatchResultData
    ): TResolvedDisplayFormat; inline;
    procedure ResolveDefaults(var AResolved: TResolvedDisplayFormat; const ADefaults: TResolvedDisplayFormat); inline;
  public
    function ResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
      const AResValue: TWatchResultData
    ): TResolvedDisplayFormat;
  end;

  TWatchResultPrinterFormatFlag = (
    rpfIndent,           // use Indent. Only when MultiLine
    rpfMultiLine,
    rpfClearMultiLine    // clean up pre-printed data
  );
  TWatchResultPrinterFormatFlags = set of TWatchResultPrinterFormatFlag;

  { TWatchResultPrinter }

  TWatchResultPrinter = class(TObject, IWatchResultPrinter)
  private
    FFormatFlags: TWatchResultPrinterFormatFlags;
    FLineSeparator: String;
    FTargetAddressSize: integer;
  protected const
    MAX_ALLOWED_NEST_LVL = 100;
  protected
    function PrintNumber(AUnsignedValue: QWord; ASignedValue: Int64;
                         AByteSize: Integer;
                         const AResolvedFormat: TResolvedDisplayFormat
                        ): String;
    function PrintArray(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
    function PrintStruct(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
    function PrintConverted(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
    function PrintProc(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

    function PrintWatchValueEx(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
  public
    constructor Create;
    function PrintWatchValue(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat): String;
    function PrintWatchValue(AResValue: IWatchResultDataIntf; const ADispFormat: TWatchDisplayFormat): String;

    property FormatFlags: TWatchResultPrinterFormatFlags read FFormatFlags write FFormatFlags;
    property TargetAddressSize: integer read FTargetAddressSize write FTargetAddressSize;
  end;

const
  {$WRITEABLECONST OFF}
  FormatBoolToEnum: array [TValueDisplayFormatBool] of TValueDisplayFormatEnum = (vdfEnumDefault, vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd);
  FormatCharToEnum: array [TValueDisplayFormatChar] of TValueDisplayFormatEnum = (vdfEnumDefault, vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd);
  FormatEnumToBool: array [TValueDisplayFormatEnum] of TValueDisplayFormatBool = (vdfBoolDefault, vdfBoolName, vdfBoolOrd, vdfBoolNameAndOrd);
  FormatEnumToChar: array [TValueDisplayFormatEnum] of TValueDisplayFormatChar = (vdfCharDefault, vdfCharLetter, vdfCharOrd, vdfCharLetterAndOrd);

var
  ValueFormatResolver: TDisplayFormatResolver;

implementation

const
  {$WRITEABLECONST OFF}
  (* NO vdf...default in the below *)
  SignedNumDefaults: TResolvedDisplayFormat = (
    NumBaseFormat:      vdfBaseDecimal;
    NumSignFormat:      vdfSignDefault;       // EXCEPTION: final format depends on NumBaseFormat (which may have been explicitly set)
    NumCharFormat:      vdfNumCharOff;
    EnumFormat:         vdfEnumName; // Also bool,char
    FloatFormat:        vdfFloatPoint;
    StructFormat:       vdfStructFields;
    StructAddrFormat:   vdfStructAddressOff;
    PointerFormat:      vdfPointerAddress;
    PointerDerefFormat: vdfPointerDerefOn;
  );
  UnsignedNumDefaults: TResolvedDisplayFormat = (
    NumBaseFormat:      vdfBaseDecimal;
    NumSignFormat:      vdfSignUnsigned;
    NumCharFormat:      vdfNumCharOff;
    EnumFormat:         vdfEnumName; // Also bool,char
    FloatFormat:        vdfFloatPoint;
    StructFormat:       vdfStructFields;
    StructAddrFormat:   vdfStructAddressOff;
    PointerFormat:      vdfPointerAddress;
    PointerDerefFormat: vdfPointerDerefOn;
  );
  HexPointerDefaults: TResolvedDisplayFormat = (
    NumBaseFormat:      vdfBasePointer;  // display 0 as nil
    NumSignFormat:      vdfSignUnsigned;
    NumCharFormat:      vdfNumCharOff;
    EnumFormat:         vdfEnumName; // Also bool,char
    FloatFormat:        vdfFloatPoint;
    StructFormat:       vdfStructFields;
    StructAddrFormat:   vdfStructAddressOff;
    PointerFormat:      vdfPointerAddress;
    PointerDerefFormat: vdfPointerDerefOn;
  );
  BoolAndNumNumDefaults: TResolvedDisplayFormat = (
    NumBaseFormat:      vdfBaseDecimal;
    NumSignFormat:      vdfSignUnsigned;
    NumCharFormat:      vdfNumCharOff;
    EnumFormat:         vdfEnumNameAndOrd; // For bools with value not in 0,1
    FloatFormat:        vdfFloatPoint;
    StructFormat:       vdfStructFields;
    StructAddrFormat:   vdfStructAddressOff;
    PointerFormat:      vdfPointerAddress;
    PointerDerefFormat: vdfPointerDerefOn;
  );
  EnumValDefaults: TResolvedDisplayFormat = (
    NumBaseFormat:      vdfBaseDecimal;
    NumSignFormat:      vdfSignUnsigned;
    NumCharFormat:      vdfNumCharOff;
    EnumFormat:         vdfEnumNameAndOrd;
    FloatFormat:        vdfFloatPoint;
    StructFormat:       vdfStructFields;
    StructAddrFormat:   vdfStructAddressOff;
    PointerFormat:      vdfPointerAddress;
    PointerDerefFormat: vdfPointerDerefOn;
  );

function Dec64ToNumb(N: QWord; Len, Base: Byte): string; overload;
var
  C: Integer;
  Number: QWord;
begin
  if N=0 then
    Result:='0'
  else
  begin
    Number:=N;
    Result:='';
    while Number>0 do begin
      C := Number mod Base;
      if C>9 then
        C:=C+55
      else
        C:=C+48;
      Result:=Chr(C)+Result;
      Number:=Number div Base;
    end;
  end;
  if (Result<>'') then
    Result:=AddChar('0',Result,Len);
end;

function Dec64ToNumb(N: Int64; Len, Base: Byte): string; inline; overload;
begin
  Result := Dec64ToNumb(QWord(N), Len, Base);
end;


{ TDisplayFormatResolver }

function TDisplayFormatResolver.DoResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
  const AResValue: TWatchResultData): TResolvedDisplayFormat;
begin
  Result.NumBaseFormat         := ADispFormat.NumBaseFormat;
  Result.NumSignFormat         := ADispFormat.NumSignFormat;
  Result.NumCharFormat      := vdfNumCharOff;  // Must be off for all non-num, so PrintNumber does not add chars to random types
  Result.EnumFormat         := ADispFormat.EnumFormat;
  Result.FloatFormat        := ADispFormat.FloatFormat;
  Result.StructFormat       := ADispFormat.StructFormat;
  Result.StructAddrFormat   := ADispFormat.StructAddrFormat;
  Result.PointerFormat      := ADispFormat.PointerFormat;
  Result.PointerDerefFormat := ADispFormat.PointerDerefFormat;

  case AResValue.ValueKind of
    rdkSignedNumVal, rdkUnsignedNumVal:
      Result.NumCharFormat := ADispFormat.NumCharFormat;
    rdkEnum, rdkEnumVal, rdkSet: begin
      Result.NumBaseFormat := ADispFormat.EnumBaseFormat;
      Result.NumSignFormat := ADispFormat.EnumSignFormat;
    end;
    rdkBool: begin
      Result.EnumFormat := FormatBoolToEnum[ADispFormat.BoolFormat];
      Result.NumBaseFormat := ADispFormat.BoolBaseFormat;
      Result.NumSignFormat := ADispFormat.BoolSignFormat;
    end;
    rdkChar: begin
      Result.EnumFormat := FormatCharToEnum[ADispFormat.CharFormat];
      Result.NumBaseFormat := ADispFormat.CharBaseFormat;
      Result.NumSignFormat := ADispFormat.CharSignFormat;
    end;

    rdkStruct: begin
      Result.NumBaseFormat := ADispFormat.StructPointerBaseFormat;
      Result.NumSignFormat := ADispFormat.StructPointerSignFormat;
      Result.PointerFormat := ADispFormat.StructPointerFormat;
    end;

    rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef,
    rdkPointerVal: begin
      Result.NumBaseFormat := ADispFormat.PointerBaseFormat;
      Result.NumSignFormat := ADispFormat.PointerSignFormat;
    end;
  end;
end;

procedure TDisplayFormatResolver.ResolveDefaults(var AResolved: TResolvedDisplayFormat;
  const ADefaults: TResolvedDisplayFormat);
begin
  if AResolved.NumBaseFormat      = vdfBaseDefault          then AResolved.NumBaseFormat         := ADefaults.NumBaseFormat;
  if AResolved.NumSignFormat      = vdfSignDefault          then AResolved.NumSignFormat         := ADefaults.NumSignFormat;
  if AResolved.NumCharFormat      = vdfNumCharDefault       then AResolved.NumCharFormat      := ADefaults.NumCharFormat;
  if AResolved.EnumFormat         = vdfEnumDefault          then AResolved.EnumFormat         := ADefaults.EnumFormat;
  if AResolved.FloatFormat        = vdfFloatDefault         then AResolved.FloatFormat        := ADefaults.FloatFormat;
  if AResolved.StructFormat       = vdfStructDefault        then AResolved.StructFormat       := ADefaults.StructFormat;
  if AResolved.StructAddrFormat   = vdfStructAddressDefault then AResolved.StructAddrFormat   := ADefaults.StructAddrFormat;
  if AResolved.PointerFormat      = vdfPointerDefault       then AResolved.PointerFormat      := ADefaults.PointerFormat;
  if AResolved.PointerDerefFormat = vdfPointerDerefDefault  then AResolved.PointerDerefFormat := ADefaults.PointerDerefFormat;
end;

function TDisplayFormatResolver.ResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
  const AResValue: TWatchResultData): TResolvedDisplayFormat;
var
  Base: TValueDisplayFormatBase;
begin
  Result := DoResolveDispFormat(ADispFormat, AResValue);

  case AResValue.ValueKind of
    rdkSignedNumVal:             ResolveDefaults(Result, SignedNumDefaults);
    rdkUnsignedNumVal:           ResolveDefaults(Result, UnsignedNumDefaults);
    rdkEnumVal:                  ResolveDefaults(Result, EnumValDefaults);
    rdkEnum, rdkSet:             ResolveDefaults(Result, UnsignedNumDefaults);
    rdkBool: if AResValue.AsQWord > 1 then
        ResolveDefaults(Result, BoolAndNumNumDefaults)
      else
        ResolveDefaults(Result, UnsignedNumDefaults);
    rdkChar:           ResolveDefaults(Result, UnsignedNumDefaults);
    rdkFloatVal:       ResolveDefaults(Result, UnsignedNumDefaults);
    rdkStruct:         ResolveDefaults(Result, HexPointerDefaults);
    rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef,
    rdkPointerVal:     ResolveDefaults(Result, HexPointerDefaults);
    else               ResolveDefaults(Result, HexPointerDefaults);
  end;

  if Result.NumSignFormat = vdfSignDefault then begin
    if Result.NumBaseFormat = vdfBaseDecimal then
      Result.NumSignFormat := vdfSignSigned
    else
      Result.NumSignFormat := vdfSignUnsigned;
  end;
end;

{ TWatchResultPrinter }

function TWatchResultPrinter.PrintNumber(AUnsignedValue: QWord; ASignedValue: Int64;
  AByteSize: Integer; const AResolvedFormat: TResolvedDisplayFormat): String;
var
  s: String;
begin
  Result := '';
  if AResolvedFormat.NumCharFormat <> vdfNumCharOnlyUnicode then
  case AResolvedFormat.NumBaseFormat of
    vdfBaseDecimal: case AResolvedFormat.NumSignFormat of
      vdfSignSigned:   Result := IntToStr(ASignedValue);
      vdfSignUnsigned: Result := IntToStr(AUnsignedValue);
    end;
    vdfBaseHex: begin
      case AResolvedFormat.NumSignFormat of
        vdfSignSigned: if (ASignedValue < 0) then
                         Result := '-$'+IntToHex(abs(ASignedValue), AByteSize * 2)
                       else
                         Result :=  '$'+IntToHex(AUnsignedValue, AByteSize * 2);
        vdfSignUnsigned: Result :=  '$'+IntToHex(AUnsignedValue, AByteSize * 2);
      end;
    end;
    vdfBaseOct: begin
      case AResolvedFormat.NumSignFormat of
        vdfSignSigned: if (ASignedValue < 0) then
                         Result := '-&'+Dec64ToNumb(abs(ASignedValue), 0, 8)
                       else
                         Result :=  '&'+Dec64ToNumb(AUnsignedValue, 0, 8);
        vdfSignUnsigned: Result :=  '&'+Dec64ToNumb(AUnsignedValue, 0, 8);
      end;
    end;
    vdfBaseBin: begin
      case AResolvedFormat.NumSignFormat of
        vdfSignSigned: if (ASignedValue < 0) then
                         Result := '-&'+Dec64ToNumb(abs(ASignedValue), AByteSize * 8, 2)
                       else
                         Result :=  '&'+Dec64ToNumb(AUnsignedValue, AByteSize * 8, 2);
        vdfSignUnsigned: Result :=  '&'+Dec64ToNumb(AUnsignedValue, AByteSize * 8, 2);
      end;
    end;
    vdfBasePointer: begin
      if AUnsignedValue = 0 then
        Result := 'nil'
      else
        Result := '$'+IntToHex(AUnsignedValue, AByteSize * 2);
    end;
    else begin // should never happen
      Result := IntToStr(AUnsignedValue);
    end;
  end;

  case AResolvedFormat.NumCharFormat of
    vdfNumCharOff: ;
    vdfNumCharOnlyUnicode,
    vdfNumCharOrdAndUnicode: begin
      s := '';
      if Result <> '' then s := ' = ';
      if AUnsignedValue <= 31 then
        Result := Result + s + '#'+IntToStr(AUnsignedValue)
      else
      if AUnsignedValue <= high(Cardinal) then
        Result := Result + s + UnicodeToUTF8(AUnsignedValue)
      else
      if AResolvedFormat.NumCharFormat = vdfNumCharOnlyUnicode then
        Result := IntToStr(AUnsignedValue);
    end;
  end;
end;

function TWatchResultPrinter.PrintArray(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
var
  i: Integer;
  sep, tn: String;
begin
  if (AResValue.ArrayType = datDynArray) then begin
    tn := AResValue.TypeName;
    if (AResValue.Count = 0) and (AResValue.DataAddress = 0) then begin
      if (ADispFormat.StructFormat = vdfStructFull) then
        Result := AResValue.TypeName + '(nil)'
      else
        Result := 'nil';
      exit;
    end;

    if (ADispFormat.StructAddrFormat = vdfStructAddressOnly) then begin
      Result := '$'+IntToHex(AResValue.DataAddress, HexDigicCount(AResValue.DataAddress, 4, True));

      if tn <> '' then
        Result := tn + '(' + Result + ')';
      exit;
    end;
  end;

  if ANestLvl = 0 then
    sep := ',' + FLineSeparator
  else
    sep := ', ';

  Result := '';
  for i := 0 to AResValue.Count - 1 do begin
    if Result <> '' then
      Result := Result + sep;
    AResValue.SetSelectedIndex(i);
    Result := Result + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
    if Length(Result) > 1000*1000 div Max(1, ANestLvl*4) then begin
      Result := Result + sep +'...';
      break;
    end;
  end;
  if AResValue.Count < AResValue.ArrayLength then
    Result := Result + sep +'...';
  Result := '(' + Result +')';
end;

function TWatchResultPrinter.PrintStruct(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
const
  VisibilityNames: array [TLzDbgFieldVisibility] of string = (
    '', 'private', 'protected', 'public', 'published'
  );
var
  Resolved: TResolvedDisplayFormat;
  FldInfo: TWatchResultDataFieldInfo;
  FldOwner: TWatchResultData;
  vis, indent, sep, tn, Header: String;
  InclVisSect: Boolean;
begin
  Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
  Result := '';

  tn := AResValue.TypeName;
  if (AResValue.StructType in [dstClass, dstInterface])
  then begin
    if (AResValue.DataAddress = 0) then begin
      Result := PrintNumber(0, 0, FTargetAddressSize, Resolved);
      if (Resolved.PointerFormat = vdfPointerTypedAddress) and (tn <> '') then
        Result := tn + '(' + Result + ')';
      exit;
    end;

    if (Resolved.StructAddrFormat <> vdfStructAddressOff) or (AResValue.FieldCount = 0)
    then begin
      Result := PrintNumber(AResValue.DataAddress, AResValue.DataAddress, FTargetAddressSize, Resolved);
      if (Resolved.PointerFormat = vdfPointerTypedAddress) and (tn <> '') then
        Result := tn + '(' + Result + ')';

      if (Resolved.StructAddrFormat = vdfStructAddressOnly) or (AResValue.FieldCount = 0) then
        exit;
    end;
  end;
  Header := Result;
  Result := '';

  if FFormatFlags * [rpfIndent, rpfMultiLine] = [rpfIndent, rpfMultiLine] then
    indent := StringOfChar(' ', (ANestLvl+1)*2) // TODO: first line should only be indented, if it starts on new line...
  else
    indent := '';
  if ANestLvl < 2 then
    sep := FLineSeparator
  else
    sep := ' ';

  InclVisSect := (Resolved.StructFormat = vdfStructFull) and (AResValue.StructType in [dstClass, dstObject]);
  FldOwner := nil;
  vis := '';
  for FldInfo in AResValue do begin
    if FldOwner <> FldInfo.Owner then begin
      FldOwner := FldInfo.Owner;
      vis := '';

      if (Resolved.StructFormat = vdfStructFull) and (FldOwner <> nil) and (FldOwner.DirectFieldCount > 0) and
         (AResValue.StructType in [dstClass, dstInterface, dstObject]) // record has no inheritance
      then begin
        if (Length(Result) > 0) then
          Result := Result + sep;
        Result := Result + indent + '{' + FldOwner.TypeName + '}';
      end;
    end;

    if InclVisSect and (vis <> VisibilityNames[FldInfo.FieldVisibility]) then begin
      vis := VisibilityNames[FldInfo.FieldVisibility];
      if (Length(Result) > 0) then
        Result := Result + sep;
      Result := Result + indent + vis;
    end;

    if (Length(Result) > 0) then
      Result := Result + sep;

    if Resolved.StructFormat <> vdfStructValOnly then
      Result := Result + indent + FldInfo.FieldName + ': '
    else
      Result := Result + indent;
    Result := Result + PrintWatchValueEx(FldInfo.Field, ADispFormat, ANestLvl) + ';';

    if Length(Result) > 1000*1000 div Max(1, ANestLvl*4) then begin
      Result := Result + sep +'...';
      break;
    end;
  end;

  if Result = '' then
    Result := '()'
  else begin
    if indent = '' then
      Result := '(' + Result + ')'
    else
    begin
      UniqueString(Result);
      Result[ANestLvl*2+1] := '(';
      Result[Length(Result)] := ')';
      //Delete(Result, Length(Result), 1)
      //Result := Result + sep + ')';
    end;
  end;

  tn := AResValue.TypeName;
  if (tn <> '') and (ANestLvl=0) and
     ( (Resolved.StructFormat = vdfStructFull) or
       (AResValue.StructType in [dstClass, dstInterface])
     )
  then
    Result := tn + Result;

  if Header <> '' then
    Result := Header + ': ' + Result;
end;

function TWatchResultPrinter.PrintConverted(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
begin
  if AResValue.FieldCount = 0 then
    exit('Error: No result');

  if (AResValue.FieldCount = 1) or
     ( (AResValue.Fields[0].Field <> nil) and
       ((AResValue.Fields[0].Field.ValueKind <> rdkError))
     )
  then begin
    Result := PrintWatchValueEx(AResValue.Fields[0].Field, ADispFormat, ANestLvl);
    exit;
  end;

  if (AResValue.FieldCount > 1) then begin
    Result := PrintWatchValueEx(AResValue.Fields[1].Field, ADispFormat, ANestLvl);
    if (AResValue.Fields[0].Field = nil) or
       (AResValue.Fields[0].Field.ValueKind <> rdkError) or
       (AResValue.Fields[0].Field.AsString <> '')
    then
    Result := Result + ' { '
      + PrintWatchValueEx(AResValue.Fields[0].Field, ADispFormat, ANestLvl)
      + ' }';
    exit;
  end;

  Result := 'Error: No result';
end;

function TWatchResultPrinter.PrintProc(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
var
  Resolved: TResolvedDisplayFormat;
  s: String;
begin
  Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

  if AResValue.AsQWord = 0 then
    Result := 'nil'
  else
    Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, TargetAddressSize, Resolved);

  if AResValue.AsString <> '' then
    Result := Result + ' = ' + AResValue.AsString;

  if ANestLvl > 0 then begin
    s := AResValue.TypeName;
  end
  else begin
    s := AResValue.AsDesc;
    if s = '' then
      s := AResValue.TypeName;
  end;

  if s <> '' then
    if AResValue.ValueKind in [rdkFunctionRef, rdkProcedureRef] then
      Result := Result + ': '+s
    else
      Result := s + ' AT ' +Result;
end;

function TWatchResultPrinter.PrintWatchValueEx(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

  function PrintChar: String;
  var
    Resolved: TResolvedDisplayFormat;
    s: String;
  begin
    Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    result := '';
    s := '';
    if Resolved.EnumFormat <> vdfEnumOrd then begin
      s := ' = ';
      case AResValue.ByteSize of
         //1: Result := QuoteText(SysToUTF8(char(Byte(AResValue.AsQWord))));
         1: Result := QuoteText(char(Byte(AResValue.AsQWord)));
         2: Result := QuoteWideText(WideChar(Word(AResValue.AsQWord)));
         else
           if Resolved.EnumFormat <> vdfEnumName then
             Result := '#' + IntToStr(AResValue.AsQWord)
           else
             Result := '#' + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved);
      end;
    end;

    if Resolved.EnumFormat <> vdfEnumName then begin
      Result := Result + s + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved);
    end;
  end;

  function PrintBool: String;
  var
    Resolved: TResolvedDisplayFormat;
    c: QWord;
    s: String;
  begin
    Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    c := AResValue.AsQWord;
    result := '';
    if Resolved.EnumFormat <> vdfEnumOrd then begin
      if c = 0 then
        Result := 'False'
      else
        Result := 'True';
    end;

    if Resolved.EnumFormat <> vdfEnumName then begin
      s := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved);
      if Result <> '' then
        Result := Result + '(' + s + ')'
      else
        Result := s;
    end;
  end;

  function PrintEnum: String;
  var
    Resolved: TResolvedDisplayFormat;
    s: String;
  begin
    Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    result := '';
    s := '';
    if Resolved.EnumFormat <> vdfEnumOrd then begin
      Result := AResValue.AsString;
      s := ' = ';
    end;

    if Resolved.EnumFormat <> vdfEnumName then begin
      Result := Result + s + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved);
    end;
  end;

  function PrintSet: String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to AResValue.Count - 1 do
      Result := Result + ',' + AResValue.ElementName[i];
    if Result = '' then
      Result := '[]'
    else begin
      Result[1] := '[';
      Result := Result + ']'
    end;
  end;

var
  PointerValue: TWatchResultDataPointer absolute AResValue;
  ResTypeName: String;
  PtrDeref, PtrDeref2: TWatchResultData;
  Resolved: TResolvedDisplayFormat;
  n: Integer;
begin
  inc(ANestLvl);
  if ANestLvl > MAX_ALLOWED_NEST_LVL then
    exit('...');
  if AResValue = nil then
    exit('???');

  Result := '';
  case AResValue.ValueKind of
    rdkError:
      begin
      Result := 'Error: ' + AResValue.AsString;
      if rpfClearMultiLine in FFormatFlags then
        Result := ClearMultiline(Result);
    end;
    rdkUnknown:
      Result := 'Error: Unknown';
    rdkPrePrinted: begin
      Result := AResValue.AsString;
      if rpfClearMultiLine in FFormatFlags then
        Result := ClearMultiline(Result);
    end;
    rdkSignedNumVal,
    rdkUnsignedNumVal:
      Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize,
                            ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue));
    rdkPointerVal: begin
      Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
      Result := '';

      PtrDeref :=  PointerValue.DerefData;
      if (Resolved.PointerDerefFormat <> vdfPointerDerefOnly) or (PtrDeref = nil) then begin
        n := AResValue.ByteSize;
        if n = 0 then n := FTargetAddressSize;
        Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, n, Resolved);
        if Resolved.PointerFormat = vdfPointerTypedAddress then begin
          ResTypeName := AResValue.TypeName;
          if (ResTypeName = '') and (PtrDeref <> nil) then begin
            ResTypeName := PtrDeref.TypeName;
            if ResTypeName <> '' then
              ResTypeName := '^'+ResTypeName;
          end;
          if ResTypeName <> '' then
            Result := ResTypeName + '(' + Result + ')';
        end;
      end;

      if (Resolved.PointerDerefFormat <> vdfPointerDerefOff) and (PtrDeref <> nil) then begin
        while (PtrDeref.ValueKind = rdkPointerVal) and (PtrDeref.DerefData <> nil) do begin
          PtrDeref2 := PtrDeref;
          Result := Result + '^';
          PtrDeref :=  PtrDeref.DerefData;
        end;
        if PtrDeref <> nil then
          Result := Result + '^: ' + PrintWatchValueEx(PtrDeref, ADispFormat, ANestLvl)
        else
          Result := Result + ': ' + PrintWatchValueEx(PtrDeref2, ADispFormat, ANestLvl);
      end;
    end;
    rdkFloatVal: begin
      Resolved := ValueFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
      if Resolved.FloatFormat = vdfFloatScientific then
        case AResValue.FloatPrecission of
          dfpSingle:   Result := FloatToStrF(AResValue.AsFloat, ffExponent,  9, 0);
          dfpDouble:   Result := FloatToStrF(AResValue.AsFloat, ffExponent, 17, 0);
          dfpExtended: Result := FloatToStrF(AResValue.AsFloat, ffExponent, 21, 0);
        end
      else
        case AResValue.FloatPrecission of
          dfpSingle:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral,  9, 0);
          dfpDouble:   Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 17, 0);
          dfpExtended: Result := FloatToStrF(AResValue.AsFloat, ffGeneral, 21, 0);
        end;
    end;
    rdkChar:       Result := PrintChar;
    rdkString:     Result := QuoteText(AResValue.AsString);
    rdkWideString: Result := QuoteWideText(AResValue.AsWideString);
    rdkBool:       Result := PrintBool;
    rdkEnum, rdkEnumVal:
                   Result := PrintEnum;
    rdkSet:        Result := PrintSet;
    rdkPCharOrString: begin
      AResValue.SetSelectedIndex(0); // pchar res
      Result := 'PChar: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
      AResValue.SetSelectedIndex(1); // string res
      Result := Result + FLineSeparator
              + 'String: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
    end;
    rdkArray:  Result := PrintArray(AResValue, ADispFormat, ANestLvl);
    rdkStruct: Result := PrintStruct(AResValue, ADispFormat, ANestLvl);
    rdkConvertRes: Result := PrintConverted(AResValue, ADispFormat, ANestLvl);
    rdkFunction,
    rdkProcedure,
    rdkFunctionRef,
    rdkProcedureRef: Result := PrintProc(AResValue, ADispFormat, ANestLvl);
    rdkVariant: Result := PrintWatchValueEx(AResValue.DerefData, ADispFormat, ANestLvl);
  end;
end;

constructor TWatchResultPrinter.Create;
begin
  FFormatFlags := [rpfMultiLine, rpfIndent];
  FTargetAddressSize := SizeOf(Pointer); // TODO: ask debugger
end;

function TWatchResultPrinter.PrintWatchValue(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat): String;
begin
  if rpfMultiLine in FFormatFlags then
    FLineSeparator := LineEnding
  else
    FLineSeparator := ' ';

  Result := PrintWatchValueEx(AResValue, ADispFormat, -1);
end;

function TWatchResultPrinter.PrintWatchValue(AResValue: IWatchResultDataIntf;
  const ADispFormat: TWatchDisplayFormat): String;
begin
  Result := PrintWatchValue(TWatchResultData(AResValue.GetInternalObject), ADispFormat);
end;

initialization
  ValueFormatResolver := TDisplayFormatResolver.Create;
finalization
  ValueFormatResolver.Free;
end.

