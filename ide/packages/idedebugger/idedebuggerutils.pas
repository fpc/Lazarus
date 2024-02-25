unit IdeDebuggerUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerStringConstants, IdeDebuggerWatchValueIntf;

function HexDigicCount(ANum: QWord; AByteSize: Integer = 0; AForceAddr: Boolean = False): integer;
function QuoteText(AText: Utf8String): UTf8String;
function QuoteWideText(AText: WideString): WideString;
function ClearMultiline(const AValue: ansistring): ansistring;

(* GetExpressionForArrayElement
  If "AnArrayExpression" returns an array, get a new Expression that returns
  the element "[AnIndex]"
  In case "AnArrayExpression" is an array slice, the new "AnIndex" has to
  replace the slice-range.
  E.g. Entry [15] from "Foo[11..19]" must result in "Foo[15]"

  By default "AnIndex" has the lower slice-value as index.
  That is
  - For a slice "Foo[11..19]" (with type TFoo = array [0..100])
  - The slice would defined as "Slice[11..19]"
    The Index "11" returns Foo[11]
*)
function GetExpressionForArrayElement(AnArrayExpression: AnsiString; AnIndex: String): AnsiString; overload;
function GetExpressionForArrayElement(AnArrayExpression: AnsiString; AnIndex: Int64): AnsiString; overload;

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormat: TValueDisplayFormat): string;
function DisplayFormatGroupName(ADispFormatGroup: TValueDisplayFormatGroup): string;

function DisplayFormatCount(ADispFormats: TValueDisplayFormats): integer;
function DisplayFormatMask(ADispFormatGroups: TValueDisplayFormatGroups): TValueDisplayFormats;

const
  {$WRITEABLECONST OFF}
  DataKindToDisplayFormatGroups: array [TWatchResultDataKind] of TValueDisplayFormatGroups = (
    [],                                                 // rdkUnknown
    [],                                                 // rdkError
    [],                                                 // rdkPrePrinted
    [{pointer}],                                        // rdkString
    [],                                                 // rdkWideString
    [vdfgChar, vdfgBase, vdfgSign],                     // rdkChar
    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkSignedNumVal
    [vdfgBase, vdfgSign, vdfgNumChar],                  // rdkUnsignedNumVal
    [vdfgPointer, vdfgPointerDeref],                    // rdkPointerVal
    [vdfgFloat],                                        // rdkFloatVal
    [vdfgBool, vdfgBase, vdfgSign],                     // rdkBool
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnum
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkEnumVal
    [vdfgEnum, vdfgBase, vdfgSign],                     // rdkSet
    [],                                                 // rdkVariant
    [],                                                 // rdkPCharOrString
    [],                                                 // rdkArray
    [vdfgStruct, vdfgStructAddress, vdfgPointer],       // rdkStruct
    [],                                                 // rdkConvertRes
    [],                                                 // rdkFunction
    [],                                                 // rdkProcedure
    [],                                                 // rdkFunctionRef
    []                                                  // rdkProcedureRe
  );

implementation

function HexDigicCount(ANum: QWord; AByteSize: Integer = 0; AForceAddr: Boolean = False): integer;
begin
  if (ANum > high(DWord)) then
    Result := 16
  else
  if (ANum > high(Word)) then
    Result := 8
  else
  if (ANum > high(Byte)) then
    Result := 4
  else
    Result := 2;

  if AByteSize*2 > Result then
    Result := AByteSize*2;

  if AForceAddr then begin
     // Fallback / TODO: Use Target-AddrSize
    if Result < SizeOf(Pointer)*2 then
      Result := 16;
  end;
end;

function QuoteWideText(AText: WideString): WideString;
const
  HEXCHR: array [0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  Len: Integer;
  c: WideChar;
  RPos, SPos, SEnd, QPos: PWideChar;
begin
  if AText = '' then
    exit('''''');

  Len := Length(AText);

  SetLength(Result, Len * 4); // This is the maximal length result can get
  RPos := @Result[1];
  SPos := @AText[1];
  SEnd := PWideChar(@AText[Len]) + 1;

  repeat
    RPos^ := ''''; inc(RPos);
    QPos := RPos;


    repeat
      c := SPos^;
      case c of
        #0..#31, #127, #$80..#$9F:
          break;
        '''': begin
          RPos^ := c; inc(RPos);
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        else begin
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
      end;

      c := SPos^;
    until False;

    if RPos = QPos then
      dec(RPos)
    else begin
      RPos^ := ''''; inc(RPos);
    end;

    repeat
      c := SPos^;
      if (c = #0) and (SPos >= SEnd) then begin
        // END OF TEXT
        Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
        SetLength(Result, RPos - PWideChar(@Result[1]));
        exit;
      end;

      RPos^ := '#'; inc(RPos);
      RPos^ := '$'; inc(RPos);
      RPos^ := HEXCHR[Byte(c) >> 4]; inc(RPos);
      RPos^ := HEXCHR[Byte(c) and 15]; inc(RPos);
      inc(SPos);
      c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$9F]);

  until False;
end;

function QuoteText(AText: Utf8String): UTf8String;
// TODO: process large text in chunks to avoid allocating huge memory
const
  HEXCHR: array [0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  Len: Integer;
  c: Char;
  RPos, SPos, SEnd, QPos: PChar;
begin
  if AText = '' then
    exit('''''');

  Len := Length(AText);

  SetLength(Result, Len * 4); // This is the maximal length result can get
  RPos := @Result[1];
  SPos := @AText[1];
  SEnd := PChar(@AText[Len]) + 1;

  repeat
    RPos^ := ''''; inc(RPos);
    QPos := RPos;


    repeat
      c := SPos^;
      case c of
        '''': begin
          RPos^ := c; inc(RPos);
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        #32..Pred(''''), Succ('''')..#126: begin
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        #192..#223: begin
          if ((Byte(SPos[1]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          inc(SPos, 2);
        end;
        #224..#239: begin
          if ((Byte(SPos[1]) and $C0) <> $80) or ((Byte(SPos[2]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          RPos^ := SPos[2]; inc(RPos);
          inc(SPos, 3);
        end;
        #240..#247: begin
          if ((Byte(SPos[1]) and $C0) <> $80) or ((Byte(SPos[2]) and $C0) <> $80) or
             ((Byte(SPos[3]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          RPos^ := SPos[2]; inc(RPos);
          RPos^ := SPos[3]; inc(RPos);
          inc(SPos, 4);
        end;
        #0: begin
          if (SPos < SEnd) then
            break; // need escaping

          // END OF TEXT
          RPos^ := ''''; inc(RPos);
          Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
          SetLength(Result, RPos - @Result[1]);
          exit;
        end;
        else
          break; // need escaping
      end;

      c := SPos^;
    until False;

    if RPos = QPos then
      dec(RPos)
    else begin
      RPos^ := ''''; inc(RPos);
    end;

    repeat
      c := SPos^;
      if (c = #0) and (SPos >= SEnd) then begin
        // END OF TEXT
        Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
        SetLength(Result, RPos - @Result[1]);
        exit;
      end;

      RPos^ := '#'; inc(RPos);
      RPos^ := '$'; inc(RPos);
      RPos^ := HEXCHR[Byte(c) >> 4]; inc(RPos);
      RPos^ := HEXCHR[Byte(c) and 15]; inc(RPos);
      inc(SPos);
      c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$BF]);

  until False;
end;

function ClearMultiline(const AValue: ansistring): ansistring;
var
  j: SizeInt;
  ow: SizeInt;
  NewLine: Boolean;
begin
  ow:=0;
  SetLength(Result{%H-},Length(AValue));
  NewLine:=true;
  for j := 1 to Length(AValue) do begin
    if (AValue[j]=#13) or (AValue[j]=#10) then begin
      NewLine:=true;
      inc(ow);
      Result[ow]:=#32; // insert one space instead of new line
    end
    else if Avalue[j] in [#9,#32] then begin
      if not NewLine then begin // strip leading spaces after new line
        inc(ow);
        Result[ow]:=#32;
      end;
    end else begin
      inc(ow);
      Result[ow]:=AValue[j];
      NewLine:=false;
    end;
  end;
  If ow>255 then begin
    //Limit watch to 255 chars in length
    Result:=Copy(Result,1,252)+'...';
  end else begin
    SetLength(Result,ow);
  end;
end;

function GetExpressionForArrayElement(AnArrayExpression: AnsiString;
  AnIndex: String): AnsiString;
var
  s, e, p, p2: PChar;
  MaybeBeforeRange, InString, FoundDotDot: Boolean;
  RStart, InRndBracket, InSqrBracket: Integer;
begin
  Result := AnArrayExpression + '[' + AnIndex + ']';
  if AnArrayExpression = '' then
    exit;

  s := @AnArrayExpression[1];
  p := s;
  e := @AnArrayExpression[Length(AnArrayExpression)];
  MaybeBeforeRange := False;
  InString := False;
  dec(p);
  while p < e do begin
    inc(p);
    if p^ = '''' then begin
      InString := not InString;
      MaybeBeforeRange := True; // sub-range of string
      Continue;
    end;
    if InString then
      Continue;

    if p^ in ['@', '.', '+', '-', '*', '/', '(', ',', '=', '<', '>', '#', '$', '%', '&', '!'] then
      MaybeBeforeRange := False  // after operator. A [1..5] would be a set of byte

    else
    if (p - s >= 2) and
       (p[-2] in [#1..#32]) and (p[-1] in ['i', 'I']) and
       (p^ in ['n', 'N']) and  (p[1] in [#1..#32])
    then
      MaybeBeforeRange := False  // after IN operator. A [1..5] would be a set of byte

    else
    if p^ in ['a'..'z', 'A'..'Z', '_', ')', ']'] then
      MaybeBeforeRange := True  // after identifier, or after ")" or "]"

    else
    if (p^ = '[') and MaybeBeforeRange then begin
      // maybe found first range
      p2 := nil;
      RStart := p - s; // Length of substring before "["


      // check if this is a slice
      InRndBracket := 0;
      InSqrBracket := 0;
      FoundDotDot := False;
      while p < e do begin
        inc(p);
        if p^ = '''' then begin
          InString := not InString;
          Continue;
        end;
        if InString then
          Continue;
        if (p^ = '.') and (p[1] = '.') then begin FoundDotDot := True; inc(p); end
        else if (p^ = '(') then inc(InRndBracket)
        else if (p^ = ')') then begin
          dec(InRndBracket);
          if InRndBracket < 0 then
            break; // something wrong.
        end
        else if (p^ = '[') then begin
          inc(InSqrBracket);
          if p2 = nil then p2 := p; // continue outer loop from here
        end
        else if (p^ = ']') then begin
          dec(InSqrBracket);
          if InSqrBracket < 0 then begin
            if (not FoundDotDot) or (InRndBracket <> 0) then
              break; // not a range, continue outer loop
            // Found
            Result := copy(AnArrayExpression, 1 , RStart+1) +
              AnIndex +
              copy(AnArrayExpression, p-s + 1, Length(AnArrayExpression));
            exit;
          end;
        end;
      end;


      MaybeBeforeRange := p2 = nil;  // after "]"
      if p2 <> nil then
        p := p2; // continue after first "[" in above loop
    end;
  end;
end;

function GetExpressionForArrayElement(AnArrayExpression: AnsiString;
  AnIndex: Int64): AnsiString;
begin
  Result := GetExpressionForArrayElement(AnArrayExpression, IntToStr(AnIndex));
end;

function DisplayFormatName(ADispFormat: TValueDisplayFormat): string;
begin
  Result := '?';
  WriteStr(Result, ADispFormat);
  case ADispFormat of
    vdfBaseDefault:          Result := DispFormatBaseDefault;
    vdfBaseDecimal:          Result := DispFormatBaseDecimal;
    vdfBaseHex:              Result := DispFormatBaseHex;
    vdfBaseOct:              Result := DispFormatBaseOct;
    vdfBaseBin:              Result := DispFormatBaseBin;
    vdfBasePointer:          Result := DispFormatBasePointer;
    vdfSignDefault:          Result := DispFormatSignDefault;
    vdfSignSigned:           Result := DispFormatSignSigned;
    vdfSignUnsigned:         Result := DispFormatSignUnsigned;
    vdfNumCharDefault:       Result := DispFormatNumCharDefault;
    vdfNumCharOff:           Result := DispFormatNumCharOff;
    vdfNumCharOrdAndUnicode: Result := DispFormatNumCharOrdAndUnicode;
    vdfNumCharOnlyUnicode:   Result := DispFormatNumCharOnlyUnicode;
    vdfEnumDefault:          Result := DispFormatEnumDefault;
    vdfEnumName:             Result := DispFormatEnumName;
    vdfEnumOrd:              Result := DispFormatEnumOrd;
    vdfEnumNameAndOrd:       Result := DispFormatEnumNameAndOrd;
    vdfBoolDefault:          Result := DispFormatBoolDefault;
    vdfBoolName:             Result := DispFormatBoolName;
    vdfBoolOrd:              Result := DispFormatBoolOrd;
    vdfBoolNameAndOrd:       Result := DispFormatBoolNameAndOrd;
    vdfCharDefault:          Result := DispFormatCharDefault;
    vdfCharLetter:           Result := DispFormatCharLetter;
    vdfCharOrd:              Result := DispFormatCharOrd;
    vdfCharLetterAndOrd:     Result := DispFormatCharLetterAndOrd;
    vdfFloatDefault:         Result := DispFormatFloatDefault;
    vdfFloatPoint:           Result := DispFormatFloatPoint;
    vdfFloatScientific:      Result := DispFormatFloatScientific;
    vdfStructDefault:        Result := DispFormatStructDefault;
    vdfStructValOnly:        Result := DispFormatStructValOnly;
    vdfStructFields:         Result := DispFormatStructFields;
    vdfStructFull:           Result := DispFormatStructFull;
    vdfStructAddressDefault: Result := DispFormatStructAddressDefault;
    vdfStructAddressOff:     Result := DispFormatStructAddressOff;
    vdfStructAddressOn:      Result := DispFormatStructAddressOn;
    vdfStructAddressOnly:    Result := DispFormatStructAddressOnly;
    vdfPointerDefault:       Result := DispFormatPointerDefault;
    vdfPointerAddress:       Result := DispFormatPointerAddress;
    vdfPointerTypedAddress:  Result := DispFormatPointerTypedAddress;
    vdfPointerDerefDefault:  Result := DispFormatPointerDerefDefault;
    vdfPointerDerefOff:      Result := DispFormatPointerDerefOff;
    vdfPointerDerefOn:       Result := DispFormatPointerDerefOn;
    vdfPointerDerefOnly:     Result := DispFormatPointerDerefOnly;
    vdfCategoryData:         Result := DispFormatCategoryData;
    vdfCategoryMemDump:      Result := DispFormatCategoryMemDump;
  end;
end;

function DisplayFormatGroupName(ADispFormat: TValueDisplayFormat): string;
begin
  Result := DisplayFormatGroupName(ValueDisplayFormatGroupMap[ADispFormat]);
end;

function DisplayFormatGroupName(ADispFormatGroup: TValueDisplayFormatGroup): string;
begin
  case ADispFormatGroup of
    vdfgBase:          Result := DispFormatGroupBase;
    vdfgSign:          Result := DispFormatGroupSign;
    vdfgNumChar:       Result := DispFormatGroupNumChar;
    vdfgEnum:          Result := DispFormatGroupEnum;
    vdfgBool:          Result := DispFormatGroupBool;
    vdfgChar:          Result := DispFormatGroupChar;
    vdfgFloat:         Result := DispFormatGroupFloat;
    vdfgStruct:        Result := DispFormatGroupStruct;
    vdfgStructAddress: Result := DispFormatGroupStructAddress;
    vdfgPointer:       Result := DispFormatGroupPointer;
    vdfgPointerDeref:  Result := DispFormatGroupPointerDeref;
    vdfgCategory:      Result := DispFormatGroupCategory;
    else Result := '?';
  end;
end;

function DisplayFormatCount(ADispFormats: TValueDisplayFormats): integer;
var
  d: TValueDisplayFormat;
begin
  Result := 0;
  for d := low(TValueDisplayFormat) to high(TValueDisplayFormat) do
    if d in ADispFormats then
      inc(Result);
end;

function DisplayFormatMask(ADispFormatGroups: TValueDisplayFormatGroups): TValueDisplayFormats;
var
  g: TValueDisplayFormatGroup;
begin
  Result := [];
  for g := low(TValueDisplayFormatGroup) to high(TValueDisplayFormatGroup) do
    if g in ADispFormatGroups then
      Result := Result + ValueDisplayFormatMaskMap[g];
end;

end.

