unit IdeDebuggerWatchResPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, IdeDebuggerWatchResult, IdeDebuggerUtils,
  LazDebuggerIntf, LazUTF8, StrUtils;

type

  { TWatchResultPrinter }

  TWatchResultPrinter = class
  protected const
    MAX_ALLOWED_NEST_LVL = 100;
  protected
    function PrintNumber(ANumValue: TWatchResultData; AnIsPointer: Boolean; ADispFormat: TWatchDisplayFormat): String;
    function PrintArray(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
    function PrintStruct(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
    function PrintProc(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

    function PrintWatchValueEx(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
  public
    function PrintWatchValue(AResValue: TWatchResultData; ADispFormat: TWatchDisplayFormat): String;
  end;

implementation

{ TWatchResultPrinter }

function TWatchResultPrinter.PrintNumber(ANumValue: TWatchResultData;
  AnIsPointer: Boolean; ADispFormat: TWatchDisplayFormat): String;
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
      n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, AnIsPointer);
      Result := '$'+IntToHex(ANumValue.AsQWord, n);
    end;
    wdfBinary: begin
      n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, AnIsPointer);
      Result := '%'+IntToBin(Int64(ANumValue.AsQWord), n*4); // Don't get any extra leading 1
    end;
    wdfPointer: begin
      n := HexDigicCount(ANumValue.AsQWord, ANumValue.ByteSize, True);
      Result := '$'+IntToHex(ANumValue.AsQWord, n);
    end;
    else begin // wdfDecimal
      Result := IntToStr(ANumValue.AsInt64);
    end;
  end;
end;

function TWatchResultPrinter.PrintArray(AResValue: TWatchResultData;
  ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
var
  i: Integer;
  sep, tn: String;
begin
  if (AResValue.ArrayType = datDynArray) then begin
    tn := AResValue.TypeName;
    if (AResValue.Count = 0) and (AResValue.DataAddress = 0) then begin
      if (ADispFormat = wdfStructure) then
        Result := AResValue.TypeName + '(nil)'
      else
        Result := 'nil';
      exit;
    end;

    if (ADispFormat = wdfPointer) then begin
      Result := '$'+IntToHex(AResValue.DataAddress, HexDigicCount(AResValue.DataAddress, 4, True));

      if tn <> '' then
        Result := tn + '(' + Result + ')';
      exit;
    end;
  end;

  if ANestLvl = 0 then
    sep := ',' + LineEnding
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
  ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
const
  VisibilityNames: array [TLzDbgFieldVisibility] of string = (
    '', 'private', 'protected', 'public', 'published'
  );
var
  i: Integer;
  FldInfo: TWatchResultDataFieldInfo;
  FldOwner: TWatchResultData;
  vis, indent, sep, tn: String;
begin
  Result := '';

  if (AResValue.ValueKind = rdkStruct) and
     (AResValue.StructType = dstInternal)
  then begin
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

    exit('Error: No result');
  end;

  if (AResValue.StructType in [dstClass, dstInterface])
  then begin
    tn := AResValue.TypeName;
    if (AResValue.DataAddress = 0) and (tn <> '') then begin
      if (ADispFormat = wdfStructure) then
        Result := AResValue.TypeName + '(nil)'
      else
        Result := 'nil';
      exit;
    end;

    if (ADispFormat = wdfPointer) or (AResValue.FieldCount = 0)
    then begin
      Result := '$'+IntToHex(AResValue.DataAddress, HexDigicCount(AResValue.DataAddress, 4, True));

      if tn <> '' then
        Result := tn + '(' + Result + ')';
      exit;
    end;
  end;

  indent := StringOfChar(' ', (ANestLvl+1)*2); // TODO: first line should only be indented, if it starts on new line...
  if ANestLvl < 2 then
    sep := LineEnding
  else
    sep := ' ';

  FldOwner := nil;
  vis := '';
  for i := 0 to AResValue.FieldCount - 1 do begin
    FldInfo := AResValue.Fields[i];

    if FldOwner <> FldInfo.Owner then begin
      FldOwner := FldInfo.Owner;
      vis := '';

      if (ADispFormat = wdfStructure) and (FldOwner <> nil) and (FldOwner.DirectFieldCount > 0) and
         (AResValue.StructType in [dstClass, dstInterface, dstObject]) // record has no inheritance
      then begin
        if (Length(Result) > 0) then
          Result := Result + sep;
        Result := Result + indent + '{' + FldOwner.TypeName + '}';
      end;
    end;

    if (ADispFormat = wdfStructure) and (AResValue.StructType in [dstClass, dstObject]) then begin
      if vis <> VisibilityNames[FldInfo.FieldVisibility] then begin
        vis := VisibilityNames[FldInfo.FieldVisibility];
        if (Length(Result) > 0) then
          Result := Result + sep;
        Result := Result + indent + vis;
      end;
    end;

    if (Length(Result) > 0) then
      Result := Result + sep;
    Result := Result + indent + FldInfo.FieldName + ': ' +
      PrintWatchValueEx(FldInfo.Field, wdfDefault, ANestLvl) + ';';
    if Length(Result) > 1000*1000 div Max(1, ANestLvl*4) then begin
      Result := Result + sep +'...';
      break;
    end;
  end;

  if Result = '' then
    Result := '()'
  else begin
    Result[ANestLvl*2+1] := '(';
    Result[Length(Result)] := ')';
    //Delete(Result, Length(Result), 1)
    //Result := Result + sep + ')';
  end;

  tn := AResValue.TypeName;
  if (tn <> '') and (ANestLvl=0) and
     ( (ADispFormat = wdfStructure) or
       (AResValue.StructType in [dstClass, dstInterface])
     )
  then
    Result := tn + Result;
end;

function TWatchResultPrinter.PrintProc(AResValue: TWatchResultData;
  ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;
var
  s: String;
begin
  if AResValue.AsQWord = 0 then
    Result := 'nil'
  else
    Result := PrintNumber(AResValue, True, wdfHex);

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
  ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

  function PrintChar: String;
  begin
    if ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary] then begin
      Result := '#' + PrintNumber(AResValue, False, ADispFormat);
      exit;
    end;
    case AResValue.ByteSize of
       //1: Result := QuoteText(SysToUTF8(char(Byte(AResValue.AsQWord))));
       1: Result := QuoteText(char(Byte(AResValue.AsQWord)));
       2: Result := QuoteWideText(WideChar(Word(AResValue.AsQWord)));
       else Result := '#' + PrintNumber(AResValue, False, wdfDecimal);
    end;
  end;

  function PrintBool: String;
  var
    c: QWord;
  begin
    c := AResValue.AsQWord;
    if c = 0 then
      Result := 'False'
    else
      Result := 'True';

    if (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary]) then
      Result := Result + '(' + PrintNumber(AResValue, False, ADispFormat) + ')'
    else
    if (c > 1) then
      Result := Result + '(' + PrintNumber(AResValue, False, wdfDecimal) + ')';
  end;

  function PrintEnum: String;
  begin
    if (ADispFormat = wdfDefault) and (AResValue.ValueKind = rdkEnumVal) then
      ADispFormat := wdfStructure;
    case ADispFormat of
      wdfStructure:
        Result := AResValue.AsString + ' = ' +  PrintNumber(AResValue, False, wdfDecimal);
      wdfUnsigned,
      wdfDecimal,
      wdfHex,
      wdfBinary:
        Result := PrintNumber(AResValue, False, ADispFormat);
      else
        Result := AResValue.AsString;
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
  PtrDeref: TWatchResultData;
begin
  inc(ANestLvl);
  if ANestLvl > MAX_ALLOWED_NEST_LVL then
    exit('...');
  if AResValue = nil then
    exit('???');

  Result := '';
  case AResValue.ValueKind of
    rdkError:
      Result := 'Error: ' + AResValue.AsString;
    rdkUnknown:
      Result := 'Error: Unknown';
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

        Result := PrintNumber(AResValue, False, ADispFormat);
      end;
    end;
    rdkPointerVal: begin
      PtrDeref :=  PointerValue.DerefData;
      ResTypeName := '';
      if (ADispFormat = wdfStructure) or
         ((ADispFormat = wdfDefault) and (PointerValue.DerefData = nil))
      then begin
        ResTypeName := AResValue.TypeName;
        if (ResTypeName = '') and (PtrDeref <> nil) then begin
          ResTypeName := PtrDeref.TypeName;
          if ResTypeName <> '' then
            ResTypeName := '^'+ResTypeName;
        end;
      end;

      if (ADispFormat in [wdfDefault, wdfStructure, wdfPointer]) and (AResValue.AsQWord = 0)
      then begin
        Result := 'nil';
      end
      else begin
        if not (ADispFormat in [wdfDecimal, wdfUnsigned, wdfHex, wdfBinary, wdfPointer]) then
          //wdfDefault, wdfStructure, wdfChar, wdfString, wdfFloat
          ADispFormat := wdfPointer;

        Result := PrintNumber(AResValue, True, ADispFormat);
      end;

      if ResTypeName <> '' then
        Result := ResTypeName + '(' + Result + ')';

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
      Result := Result + LineEnding
              + 'String: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl);
    end;
    rdkArray:  Result := PrintArray(AResValue, ADispFormat, ANestLvl);
    rdkStruct: Result := PrintStruct(AResValue, ADispFormat, ANestLvl);
    rdkFunction,
    rdkProcedure,
    rdkFunctionRef,
    rdkProcedureRef: Result := PrintProc(AResValue, ADispFormat, ANestLvl);
  end;
end;

function TWatchResultPrinter.PrintWatchValue(AResValue: TWatchResultData;
  ADispFormat: TWatchDisplayFormat): String;
begin
  Result := PrintWatchValueEx(AResValue, ADispFormat, -1);
end;

end.

