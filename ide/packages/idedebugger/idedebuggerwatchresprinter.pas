unit IdeDebuggerWatchResPrinter;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, IdeDebuggerWatchResult, IdeDebuggerUtils, IdeDebuggerDisplayFormats,
  IdeDebuggerBase, IdeDebuggerStringConstants, IdeDebuggerValueFormatter, LazDebuggerIntf, LazUTF8,
  IdeDebuggerWatchValueIntf, StrUtils, LazDebuggerUtils;

type

  { TResolvedDisplayFormatNum }

  TResolvedDisplayFormatNum = record
    UseInherited:          boolean;
    Visible:               boolean;
    BaseFormat:            TValueDisplayFormatBase;
    SignFormat:            TValueDisplayFormatSign;
    MinDigits:             Integer;
    SeparatorDec:          boolean;
    SeparatorHexBin:       TValueDisplayFormatHexSeperator;
    class operator = (a,b: TResolvedDisplayFormatNum): boolean;
    class operator := (a: TWatchDisplayFormatNum): TResolvedDisplayFormatNum;
    class operator := (a: TWatchDisplayFormatNum2): TResolvedDisplayFormatNum;
  end;

  TResolvedDisplayFormat = record
    Num1:    TResolvedDisplayFormatNum;
    Num2:    TResolvedDisplayFormatNum; // Enum, Addr, ... // using "visible"
    Enum:    TWatchDisplayFormatEnum;
    Bool:    TWatchDisplayFormatBool;
    Char:    TWatchDisplayFormatChar;
    Float:   TWatchDisplayFormatFloat;
    Struct:  TWatchDisplayFormatStruct;  // Ignore Addr part
    Pointer: TWatchDisplayFormatPointer; // Ignore Addr part
    Address:               TWatchDisplayFormatAddr;
  end;

  { TDisplayFormatResolver }

  TDisplayFormatResolver = class
  private
    FFallBackFormats: TWatchDisplayFormatList;
  public
    constructor Create;
    destructor Destroy; override;
    function ResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
      const AResValue: TWatchResultData
    ): TResolvedDisplayFormat;
    function ResolveMultiLine(const ADispFormat: TWatchDisplayFormat): TWatchDisplayFormatMultiline;
    function ResolveArrayNavBar(const ADispFormat: TWatchDisplayFormat): TWatchDisplayFormatArrayNav;
    function ResolveArrayLen(const ADispFormat: TWatchDisplayFormat): TWatchDisplayFormatArrayLen;
    // Resolving from FallBackFormats[n] to FallBackFormats[0]
    // [0] must be the IDE global format, and will be used regardless of UseInherited
    property FallBackFormats: TWatchDisplayFormatList read FFallBackFormats;
  end;

  TWatchResultPrinterFormatFlag = (
    rpfIndent,           // use Indent. Only when MultiLine
    rpfMultiLine,
    rpfClearMultiLine,   // clean up pre-printed data
    rpfSkipValueFormatter
  );
  TWatchResultPrinterFormatFlags = set of TWatchResultPrinterFormatFlag;

  { TWatchResultPrinter }

  TWatchResultPrinter = class(TObject, IWatchResultPrinter)
  private
    FFormatFlags: TWatchResultPrinterFormatFlags;
    FLineSeparator: String;
    FOnlyValueFormatter: IIdeDbgValueFormatterIntf;
    FDefaultValueFormatter, FCurrentValueFormatter, FNextValueFormatter: IIdeDbgValueFormatterIntf;
    FWatchedVarName: String;
    FTargetAddressSize: integer;
    FDisplayFormatResolver: TDisplayFormatResolver;
    FNextCallIsValueFormatter: Boolean;
    FInValFormNestLevel: integer;
    FResValueInFormatter: TWatchResultData;
    FWatchedExprInFormatter: String;
    FIndentString: String;
    FHasLineBreak: Boolean;
    FElementCount, FCurrentMultilineLvl, FDeepestMultilineLvl, FDeepestArray: integer;
    FParentResValue, FCurrentResValue: TWatchResultData;
    FCurrentOuterMostArrayLvl, FOuterMostArrayCount, FCurrentArrayCombineLvl: integer;
    FCurrentArrayLenShown: boolean;
  protected const
    MAX_ALLOWED_NEST_LVL = 100;
  protected type
    TWatchResStoredSettings = record
      FFormatFlags: TWatchResultPrinterFormatFlags;
      FCurrentValueFormatter: IIdeDbgValueFormatterIntf;
      FResValueInFormatter: TWatchResultData;
    end;
  protected
    procedure StoreSetting(var AStorage: TWatchResStoredSettings); inline;
    procedure RestoreSetting(const AStorage: TWatchResStoredSettings); inline;
    function PrintNumber(AUnsignedValue: QWord; ASignedValue: Int64;
                         AByteSize: Integer;
                         const ANumFormat: TResolvedDisplayFormatNum;
                         PrintNil: Boolean = False
                        ): String;
    function PrintArray(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
    function PrintStruct(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
    function PrintConverted(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
    function PrintProc(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer): String;

    function PrintWatchValueEx(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
  public
    constructor Create;
    destructor Destroy; override;
    function PrintWatchValue(AResValue: TWatchResultData; const ADispFormat: TWatchDisplayFormat; const AWatchedExpr: String): String;
    function PrintWatchValueIntf(AResValue: IWatchResultDataIntf; const ADispFormat: TWatchDisplayFormat; AFlags: TWatchResultPrinterFlags = []): String;
    function IWatchResultPrinter.PrintWatchValue = PrintWatchValueIntf;

    property FormatFlags: TWatchResultPrinterFormatFlags read FFormatFlags write FFormatFlags;
    property OnlyValueFormatter: IIdeDbgValueFormatterIntf read FOnlyValueFormatter write FOnlyValueFormatter;

    property TargetAddressSize: integer read FTargetAddressSize write FTargetAddressSize;
    property DisplayFormatResolver: TDisplayFormatResolver read FDisplayFormatResolver;
  end;

const
  {$WRITEABLECONST OFF}
  FormatBoolToEnum: array [TValueDisplayFormatBool] of TValueDisplayFormatEnum = (vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd);
  FormatCharToEnum: array [TValueDisplayFormatChar] of TValueDisplayFormatEnum = (vdfEnumName, vdfEnumOrd, vdfEnumNameAndOrd);
  FormatEnumToBool: array [TValueDisplayFormatEnum] of TValueDisplayFormatBool = (vdfBoolName, vdfBoolOrd, vdfBoolNameAndOrd);
  FormatEnumToChar: array [TValueDisplayFormatEnum] of TValueDisplayFormatChar = (vdfCharLetter, vdfCharOrd, vdfCharLetterAndOrd);


implementation

const
  {$WRITEABLECONST OFF}
  (* NO vdf...default in the below *)
    DefaultEnumNum: TResolvedDisplayFormatNum = (
      UseInherited:         False;
      Visible:              False;
      BaseFormat:           vdfBaseDecimal;
      SignFormat:           vdfSignSigned;
      MinDigits:            0;
      SeparatorDec:         False;
      SeparatorHexBin:      vdfhsNone;
    );
    DefaultAddrNum: TResolvedDisplayFormatNum = (
      UseInherited:         False;
      Visible:              False;
      BaseFormat:           vdfBaseHex;
      SignFormat:           vdfSignUnsigned;
      MinDigits:            0;
      SeparatorDec:         False;
      SeparatorHexBin:      vdfhsNone;
    );

{ TResolvedDisplayFormatNum }

class operator TResolvedDisplayFormatNum. = (a, b: TResolvedDisplayFormatNum): boolean;
begin
  Result :=
    (a.UseInherited       = b.UseInherited) and
    (a.Visible            = b.Visible) and
    (a.BaseFormat         = b.BaseFormat) and
    (a.SignFormat         = b.SignFormat) and
    (a.MinDigits          = b.MinDigits) and
    (a.SeparatorDec       = b.SeparatorDec) and
    (a.SeparatorHexBin    = b.SeparatorHexBin)
  ;
end;

class operator TResolvedDisplayFormatNum. := (a: TWatchDisplayFormatNum
  ): TResolvedDisplayFormatNum;
begin
  Result.UseInherited       := a.UseInherited;
  Result.Visible            := False;
  Result.BaseFormat         := a.BaseFormat;
  Result.SignFormat         := a.SignFormat;
  Result.MinDigits          := a.MinDigits[a.BaseFormat];
  Result.SeparatorDec       := a.SeparatorDec;
  Result.SeparatorHexBin    := a.SeparatorHexBin;
end;

class operator TResolvedDisplayFormatNum. := (a: TWatchDisplayFormatNum2
  ): TResolvedDisplayFormatNum;
begin
  Result.UseInherited       := a.UseInherited;
  Result.Visible            := a.Visible;
  Result.BaseFormat         := a.BaseFormat;
  Result.SignFormat         := a.SignFormat;
  Result.MinDigits          := a.MinDigits[a.BaseFormat];
  Result.SeparatorDec       := a.SeparatorDec;
  Result.SeparatorHexBin    := a.SeparatorHexBin;
end;


{ TDisplayFormatResolver }

constructor TDisplayFormatResolver.Create;
begin
  inherited Create;
  FFallBackFormats := TWatchDisplayFormatList.Create;
end;

destructor TDisplayFormatResolver.Destroy;
begin
  inherited Destroy;
  FFallBackFormats.Free;
end;

function TDisplayFormatResolver.ResolveDispFormat(const ADispFormat: TWatchDisplayFormat;
  const AResValue: TWatchResultData): TResolvedDisplayFormat;

  procedure ResolveSign(var ASign: TValueDisplayFormatSign; ABase: TValueDisplayFormatBase);
  begin
    if ASign = vdfSignAuto then begin
      if ABase = vdfBaseDecimal then
        ASign := vdfSignSigned
      else
        ASign := vdfSignUnsigned;
    end;
  end;
  procedure ResolveSign(var ASign: TValueDisplayFormatSign; ADef: TValueDisplayFormatSign);
  begin
    if ASign = vdfSignAuto then
      ASign := ADef;
  end;
  procedure ResolveMinDigits(var AMinDigits: Integer; ABase: TValueDisplayFormatBase);
  begin
    if AMinDigits = 0 then begin
      if ABase = vdfBaseHex then
        AMinDigits := -1;
    end;
  end;
  procedure ResolveMinDigitsToFull(var AMinDigits: Integer);
  begin
    if AMinDigits = 0 then
      AMinDigits := -1;
  end;

var
  i: Integer;
  fbf: TWatchDisplayFormat;
begin

  case AResValue.ValueKind of
    rdkSignedNumVal, rdkUnsignedNumVal: begin
      Result.Num1 := ADispFormat.Num;
      Result.Num2 := ADispFormat.Num2;
      for i := Max(0, FFallBackFormats.Count -1) downto 0 do begin
        fbf := FFallBackFormats[i];
        if Result.Num1.UseInherited then Result.Num1 := fbf.Num;
        if Result.Num2.UseInherited then Result.Num2 := fbf.Num2;
        if not(Result.Num1.UseInherited or Result.Num2.UseInherited) then
          break;
      end;
      if AResValue.ValueKind = rdkUnsignedNumVal then begin
        ResolveSign(Result.Num1.SignFormat, TValueDisplayFormatSign(vdfSignUnsigned));
        ResolveSign(Result.Num2.SignFormat, TValueDisplayFormatSign(vdfSignUnsigned));
      end
      else begin
        ResolveSign(Result.Num1.SignFormat, Result.Num1.BaseFormat);
        ResolveSign(Result.Num2.SignFormat, Result.Num2.BaseFormat);
      end;
      ResolveMinDigits(Result.Num1.MinDigits, Result.Num1.BaseFormat);
      ResolveMinDigits(Result.Num2.MinDigits, Result.Num2.BaseFormat);
    end;

    rdkEnum, rdkSet: begin
      Result.Enum := ADispFormat.Enum;

      if Result.Enum.UseInherited then begin
        i := FFallBackFormats.Count -1;
        while (i >0) and (FFallBackFormats[i].Enum.UseInherited) do
          dec(i);
        Result.Enum  := FFallBackFormats[i].Enum;
      end;

      Result.Num2            := DefaultEnumNum;
      Result.Num2.Visible    := Result.Enum.MainFormat in [vdfEnumNameAndOrd, vdfEnumOrd];
      Result.Num2.BaseFormat := Result.Enum.BaseFormat;
      Result.Num2.SignFormat := Result.Enum.SignFormat;
      ResolveSign(Result.Num2.SignFormat, DefaultEnumNum.SignFormat);
    end;

    rdkEnumVal: begin
      Result.Enum := ADispFormat.Enum;

      if Result.Enum.UseInherited then begin
        i := FFallBackFormats.Count -1;
        while (i >0) and (FFallBackFormats[i].EnumVal.UseInherited) do
          dec(i);
        Result.Enum  := FFallBackFormats[i].EnumVal;
      end;

      Result.Num2            := DefaultEnumNum;
      Result.Num2.Visible    := Result.Enum.MainFormat in [vdfEnumNameAndOrd, vdfEnumOrd];
      Result.Num2.BaseFormat := Result.Enum.BaseFormat;
      Result.Num2.SignFormat := Result.Enum.SignFormat;
      ResolveSign(Result.Num2.SignFormat, DefaultEnumNum.SignFormat);
    end;

    rdkBool: begin
      Result.Bool := ADispFormat.Bool;

      if Result.Bool.UseInherited then begin
        i := FFallBackFormats.Count -1;
        while (i >0) and (FFallBackFormats[i].Bool.UseInherited) do
          dec(i);
        Result.Bool  := FFallBackFormats[i].Bool;
      end;

      Result.Num2            := DefaultEnumNum;
      Result.Num2.Visible    := Result.Bool.MainFormat in [vdfBoolNameAndOrd, vdfBoolOrd];
      Result.Num2.BaseFormat := Result.Bool.BaseFormat;
      Result.Num2.SignFormat := Result.Bool.SignFormat;
      ResolveSign(Result.Num2.SignFormat, DefaultEnumNum.SignFormat);
    end;

    rdkChar: begin
      Result.Char := ADispFormat.Char;

      if Result.Char.UseInherited then begin
        i := FFallBackFormats.Count -1;
        while (i >0) and (FFallBackFormats[i].Char.UseInherited) do
          dec(i);
        Result.Char  := FFallBackFormats[i].Char;
      end;

      Result.Num2            := DefaultEnumNum;
      Result.Num2.Visible    := Result.Char.MainFormat in [vdfCharLetterAndOrd, vdfCharOrd];
      Result.Num2.BaseFormat := Result.Char.BaseFormat;
      Result.Num2.SignFormat := Result.Char.SignFormat;
      ResolveSign(Result.Num2.SignFormat, DefaultEnumNum.SignFormat);
    end;

    rdkFloatVal: begin
      Result.Float := ADispFormat.Float;

      if Result.Float.UseInherited then begin
        i := FFallBackFormats.Count -1;
        while (i >0) and (FFallBackFormats[i].Float.UseInherited) do
          dec(i);
        Result.Float  := FFallBackFormats[i].Float;
      end;
    end;

    rdkStruct: begin
      Result.Struct := ADispFormat.Struct;
      Result.Address := ADispFormat.Struct.Address;
      for i := Max(0, FFallBackFormats.Count -1) downto 0 do begin
        fbf := FFallBackFormats[i];
        if Result.Struct.UseInherited  then Result.Struct  := fbf.Struct;
        if Result.Address.UseInherited then Result.Address := fbf.Struct.Address;
        if not(Result.Struct.UseInherited or Result.Struct.Address.UseInherited) then
          break;
      end;
      Result.Num2            := DefaultAddrNum;
      Result.Num2.Visible    := Result.Struct.ShowPointerFormat in [vdfStructPointerOn, vdfStructPointerOnly];
      Result.Num2.BaseFormat := Result.Struct.Address.BaseFormat;
      if Result.Struct.Address.Signed then
        Result.Num2.SignFormat := vdfSignSigned;
      ResolveSign(Result.Num2.SignFormat, DefaultAddrNum.SignFormat);
      if not Result.Address.NoLeadZero then
        ResolveMinDigits(Result.Num2.MinDigits, Result.Num2.BaseFormat);
    end;

    rdkFunction, rdkProcedure,
    rdkFunctionRef, rdkProcedureRef,
    rdkPointerVal: begin
      Result.Pointer := ADispFormat.Pointer;
      Result.Address := ADispFormat.Pointer.Address;
      for i := Max(0, FFallBackFormats.Count -1) downto 0 do begin
        fbf := FFallBackFormats[i];
        if Result.Pointer.UseInherited then Result.Pointer := fbf.Pointer;
        if Result.Address.UseInherited then Result.Address := fbf.Pointer.Address;
        if not(Result.Pointer.UseInherited or Result.Pointer.Address.UseInherited) then
          break;
      end;
      Result.Num2            := DefaultAddrNum;
      Result.Num2.Visible    := Result.Pointer.DerefFormat <> vdfPointerDerefOnly;
      Result.Num2.BaseFormat := Result.Pointer.Address.BaseFormat;
      if Result.Pointer.Address.Signed then
        Result.Num2.SignFormat := vdfSignSigned;
      ResolveSign(Result.Num2.SignFormat, DefaultAddrNum.SignFormat);
      if not Result.Address.NoLeadZero then
        ResolveMinDigits(Result.Num2.MinDigits, Result.Num2.BaseFormat);
    end;

    //rdkString, rdkWideString,
    //rdkVariant,
    //rdkPCharOrString,
    //rdkArray,
    //rdkConvertRes,
  end;
end;

function TDisplayFormatResolver.ResolveMultiLine(const ADispFormat: TWatchDisplayFormat
  ): TWatchDisplayFormatMultiline;
var
  i: Integer;
begin
  Result := ADispFormat.MultiLine;
  i := FFallBackFormats.Count-1;
  while (Result.UseInherited) and (i > 0) do begin
    Result := FFallBackFormats[i].MultiLine;
    dec(i);
  end;
  if (Result.UseInherited) then
    Result := DefaultWatchDisplayFormat.MultiLine;
end;

function TDisplayFormatResolver.ResolveArrayNavBar(const ADispFormat: TWatchDisplayFormat
  ): TWatchDisplayFormatArrayNav;
var
  i: Integer;
begin
  Result := ADispFormat.ArrayNavBar;
  i := FFallBackFormats.Count-1;
  while (Result.UseInherited) and (i > 0) do begin
    Result := FFallBackFormats[i].ArrayNavBar;
    dec(i);
  end;
  if (Result.UseInherited) then
    Result := DefaultWatchDisplayFormat.ArrayNavBar;
end;

function TDisplayFormatResolver.ResolveArrayLen(const ADispFormat: TWatchDisplayFormat
  ): TWatchDisplayFormatArrayLen;
var
  i: Integer;
begin
  Result := ADispFormat.ArrayLen;
  i := FFallBackFormats.Count-1;
  while (Result.UseInherited) and (i > 0) do begin
    Result := FFallBackFormats[i].ArrayLen;
    dec(i);
  end;
  if (Result.UseInherited) then
    Result := DefaultWatchDisplayFormat.ArrayLen;
end;

{ TWatchResultPrinter }

procedure TWatchResultPrinter.StoreSetting(var AStorage: TWatchResStoredSettings);
begin
  AStorage.FFormatFlags           := FFormatFlags;
  AStorage.FCurrentValueFormatter := FCurrentValueFormatter;
  AStorage.FResValueInFormatter   := FResValueInFormatter;
end;

procedure TWatchResultPrinter.RestoreSetting(const AStorage: TWatchResStoredSettings);
begin
  FFormatFlags           := AStorage.FFormatFlags;
  FCurrentValueFormatter := AStorage.FCurrentValueFormatter;
  FResValueInFormatter   := AStorage.FResValueInFormatter;
end;

function TWatchResultPrinter.PrintNumber(AUnsignedValue: QWord; ASignedValue: Int64;
  AByteSize: Integer; const ANumFormat: TResolvedDisplayFormatNum; PrintNil: Boolean): String;

  function PadNumber(ANum: String; ANewLen, ASepPos: Integer; ASep: Char): String;
  var
    i, j: Integer;
  begin
    Result := ANum;
    i := Length(Result);
    ANewLen := ANewLen + (ANewLen-1) div ASepPos;
    SetLength(Result, ANewLen);
    inc(ASepPos);
    j := 0;
    while (ANewLen > i) and (ANewLen > 0) do begin
      inc(j);
      if j = ASepPos then begin
        Result[ANewLen] := ASep;
        dec(ANewLen);
        j := 1;
      end;
      if i <= 0 then
        Result[ANewLen] := '0'
      else
        Result[ANewLen] := Result[i];
      dec(ANewLen);
      dec(i);
    end;
  end;

var
  s: String;
  i, d: Integer;
begin
  if PrintNil and (AUnsignedValue = 0) then
    exit('nil');
  Result := '';
  s := '';
  if (ANumFormat.SignFormat = vdfSignSigned) and (ASignedValue < 0) then
    s := '-';
  case ANumFormat.BaseFormat of
    vdfBaseDecimal: begin
        {$PUSH}{$Q-}// abs may overflow
        if ANumFormat.SignFormat = vdfSignSigned then Result := IntToStr(qword(abs(ASignedValue)))
        {$POP}
        else                                          Result := IntToStr(AUnsignedValue);
        d := ANumFormat.MinDigits;
        if d < 0 then case AByteSize of
          1:    d := 3;
          2:    d := 5;
          3..4: d := 10;
          else  d := 20;
        end;
        i := Length(Result);
        if d < i then
          d := i;

        if ANumFormat.SeparatorDec then
          Result := PadNumber(Result, d, 3, FormatSettings.ThousandSeparator)
        else
        if d > i then
          Result := StringOfChar('0', d - i) + Result;
        Result := s + Result;
      end;
    vdfBaseHex: begin
        s := s + '$';
        if ANumFormat.SignFormat = vdfSignSigned then Result := IntToHex(qword(abs(ASignedValue)), 1)
        else                                          Result := IntToHex(AUnsignedValue, 1);
        d := ANumFormat.MinDigits;
        if d < 0 then case AByteSize of
          1:    d := 2;
          2:    d := 4;
          3..4: d := 8;
          else  d := 16;
        end;
        i := Length(Result);
        if d < i then
          d := i;

        case ANumFormat.SeparatorHexBin of
          vdfhsNone: if d > i then Result := StringOfChar('0', d - i) + Result;
          vdfhsByte: Result := PadNumber(Result, d, 2, ' ');
          vdfhsWord: Result := PadNumber(Result, d, 4, ' ');
          vdfhsLong: Result := PadNumber(Result, d, 8, ' ');
        end;
        Result := s + Result;
      end;
    vdfBaseOct: begin
        s := s + '&';
        if ANumFormat.SignFormat = vdfSignSigned then Result := Dec64ToNumb(qword(abs(ASignedValue)), 0 , 8)
        else                                          Result := Dec64ToNumb(AUnsignedValue, 0 , 8);
        d := ANumFormat.MinDigits;
        if d < 0 then case AByteSize of
          1:    d :=  3;
          2:    d :=  6;
          3..4: d := 11;
          else  d := 22;
        end;
        i := Length(Result);
        if d < i then
          d := i;

        if d > i then Result := StringOfChar('0', d - i) + Result;
        Result := s + Result;
      end;
    vdfBaseBin: begin
        s := s + '%';
        if ANumFormat.SignFormat = vdfSignSigned then Result := Dec64ToNumb(qword(abs(ASignedValue)), 0 , 2)
        else                                          Result := Dec64ToNumb(AUnsignedValue, 0 , 2);
        d := ANumFormat.MinDigits;
        if d < 0 then case AByteSize of
          1:    d :=  8;
          2:    d := 16;
          3..4: d := 32;
          else  d := 64;
        end;
        i := Length(Result);
        if d < i then
          d := i;

        case ANumFormat.SeparatorHexBin of
          vdfhsNone: if d > i then Result := StringOfChar('0', d - i) + Result;
          vdfhsByte: Result := PadNumber(Result, d,  8, ' ');
          vdfhsWord: Result := PadNumber(Result, d, 16, ' ');
          vdfhsLong: Result := PadNumber(Result, d, 32, ' ');
        end;
        Result := s + Result;
    end;
    vdfBaseChar: begin
        if AUnsignedValue <= 31 then
          Result := '#'+IntToStr(AUnsignedValue)
        else
        if AUnsignedValue <= high(Cardinal) then
          Result := UnicodeToUTF8(AUnsignedValue)
        else
        //if ANumFormat.NumCharFormat = vdfNumCharOnlyUnicode then
          Result := IntToStr(AUnsignedValue);
    end;
    else begin // should never happen
      Result := IntToStr(AUnsignedValue);
    end;
  end;
end;

function TWatchResultPrinter.PrintArray(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
type
  TIntegerArray = array of integer;

  function CheckArrayIndexes(AnArrayResValue: TWatchResultData;
    var AKnownLengthList: TIntegerArray; var AKnownLengthDepth, AMaxAllEqualDepth: Integer;
    ACurrentDepth: integer; AnArrayTypes: TLzDbgArrayTypes;
    out AnHasNonStatNested: boolean): boolean;
  var
    r: TWatchResultData;
    Len, i, Cnt: Integer;
    NestedNonStatNested: Boolean;
  begin
    Result := False;
    AnHasNonStatNested := AnArrayResValue.ArrayType <> datStatArray;
    Len := AnArrayResValue.ArrayLength;

    if ACurrentDepth > AKnownLengthDepth then begin
      AKnownLengthDepth := ACurrentDepth;
      AKnownLengthList[ACurrentDepth] := Len;
    end
    else
    if AKnownLengthList[ACurrentDepth] <> Len then
      exit;

    Result := True;

    Cnt := AnArrayResValue.Count;
    if (Cnt <> Len) then begin
      if (AnArrayResValue.ArrayType <> datStatArray) then begin
        if ACurrentDepth < AMaxAllEqualDepth then
          AMaxAllEqualDepth := ACurrentDepth;
        exit;
      end;
      AnArrayTypes := [datStatArray]; // all nested must be stat, since we can't test to the end
    end;

    if ACurrentDepth = AMaxAllEqualDepth then
      exit;

    if (Len = 0) or (Cnt = 0)
       //( (Cnt = 0) and (ACurrentDepth = AKnownLengthDepth))
    then begin
      if ACurrentDepth < AMaxAllEqualDepth then
        AMaxAllEqualDepth := ACurrentDepth;
      exit;
    end;

    for i := 0 to Cnt - 1 do begin
      AnArrayResValue.SetSelectedIndex(i);
      r := AnArrayResValue.SelectedEntry;
      NestedNonStatNested := False;
      if (r.ValueKind <> rdkArray) or
         ( not(r.ArrayType in AnArrayTypes)) or
         not CheckArrayIndexes(AnArrayResValue.SelectedEntry, AKnownLengthList, AKnownLengthDepth,
                               AMaxAllEqualDepth, ACurrentDepth + 1, AnArrayTypes, NestedNonStatNested)
      then begin
        AnHasNonStatNested := AnHasNonStatNested or NestedNonStatNested;
        if ACurrentDepth < AMaxAllEqualDepth then
          AMaxAllEqualDepth := ACurrentDepth;
        exit;
      end;
      AnHasNonStatNested := AnHasNonStatNested or NestedNonStatNested;

     if not AnHasNonStatNested  then  // includes current is stat-array
        break; // they must all be the same
    end;
  end;

  procedure CheckArrayIndexes(var AnArrayResValue: TWatchResultData;
    out AKnownLengthList: TIntegerArray; out AnAllEqualDepth: Integer;
    AnArrayTypes: TLzDbgArrayTypes);
  var
    KnownLenDepth: Integer;
    dummy: boolean;
  begin
    SetLength(AKnownLengthList, 100);
    KnownLenDepth := -1;
    AnAllEqualDepth := 99;
    CheckArrayIndexes(AnArrayResValue, AKnownLengthList, KnownLenDepth,  AnAllEqualDepth, 0, AnArrayTypes, dummy);
    inc(AnAllEqualDepth);
  end;

var
  i, Cnt, Len, MaxLen: Integer;
  PrefixIdxCnt, OldCurrentOuterMostArrayLvl, OldOuterMostArrayCount,
    OldCurrentArrayCombineLvl, ElemCnt, HideLenEach, ForceSingleLineEach, SepLen: Integer;
  CurIndentString: String;
  tn, sep, sep2, LenStr: String;
  OldHasLineBreak, CutOff: Boolean;
  ShowLen, ShowCombined, ShowCombinedStatOnly, ShowCombinedDynOnly, OuterMostArray,
    CouldHide, OldCurrentArrayLenShown, LoopCurrentArrayLenShown, CouldSingleLine,
    ShowMultiLine: Boolean;
  MultiLine: TWatchDisplayFormatMultiline;
  Results: array of string;
  PrefixIdxList: TIntegerArray;
  LenPrefix: TWatchDisplayFormatArrayLen;
  ArrayTypes: TLzDbgArrayTypes;
  EntryVal: TWatchResultData;
begin
  inc(FCurrentMultilineLvl);
  if (ANestLvl > FDeepestArray) then
    FDeepestArray := ANestLvl;

  if (AResValue.ArrayType = datDynArray) then begin
    tn := AResValue.TypeName;
    if (AResValue.Count = 0) and (AResValue.DataAddress = 0) then begin
      if (ADispFormat.Struct.DataFormat = vdfStructFull) then
        Result := AResValue.TypeName + '(nil)'
      else
        Result := 'nil';
      exit;
    end;

    if (ADispFormat.Struct.ShowPointerFormat = vdfStructPointerOnly) then begin
      Result := '$'+IntToHex(AResValue.DataAddress, HexDigicCount(AResValue.DataAddress, 4, True));

      if tn <> '' then
        Result := tn + '(' + Result + ')';
      exit;
    end;
  end;

  CurIndentString := FIndentString;
  SepLen := 2; // ', '
  MultiLine := FDisplayFormatResolver.ResolveMultiLine(ADispFormat);
  ShowMultiLine := (rpfMultiLine in FFormatFlags) and (FCurrentMultilineLvl <= MultiLine.MaxMultiLineDepth);
  if ShowMultiLine then begin
    SepLen := 1 {,} + Length(FLineSeparator);
    if (rpfIndent in FFormatFlags) then begin
      SepLen := SepLen + Length(FIndentString);
      FIndentString := FIndentString + '  ';
    end;
    CouldSingleLine := MultiLine.ForceSingleLine;
  end
  else
    CouldSingleLine := False;

  ForceSingleLineEach := Max(1, MultiLine.ForceSingleLineThresholdEach);
  OldHasLineBreak := FHasLineBreak;
  FHasLineBreak := False;

  OldCurrentOuterMostArrayLvl := FCurrentOuterMostArrayLvl;
  OldOuterMostArrayCount := FOuterMostArrayCount;
  OldCurrentArrayCombineLvl := FCurrentArrayCombineLvl;
  OldCurrentArrayLenShown := FCurrentArrayLenShown;
  try
    if (FCurrentMultilineLvl > FDeepestMultilineLvl) and (AResValue.ArrayLength > 0) then
      FDeepestMultilineLvl := FCurrentMultilineLvl;
    OuterMostArray := (FParentResValue = nil) or (FParentResValue.ValueKind <> rdkArray);
    if OuterMostArray then begin
      FCurrentOuterMostArrayLvl := ANestLvl;
      inc(FOuterMostArrayCount);
      FCurrentArrayCombineLvl := 0;
      FCurrentArrayLenShown := False;
    end;

    LenPrefix := FDisplayFormatResolver.ResolveArrayLen(ADispFormat);
    ShowLen := LenPrefix.ShowLenPrefix and
               ( (FOuterMostArrayCount = 1) or LenPrefix.ShowLenPrefixEmbedded ) and
               (AResValue.ArrayLength > 0) and
               (ANestLvl <= FCurrentOuterMostArrayLvl + LenPrefix.LenPrefixMaxNest) and
               (ANestLvl >= FCurrentOuterMostArrayLvl + FCurrentArrayCombineLvl);
    if ShowLen then begin
      ShowCombinedDynOnly  := (LenPrefix.LenPrefixCombine = vdfatDyn) and (AResValue.ArrayType = datDynArray);
      ShowCombinedStatOnly := (LenPrefix.LenPrefixCombine = vdfatStat) and (AResValue.ArrayType = datStatArray);
      ShowCombined := OuterMostArray and (
        (LenPrefix.LenPrefixCombine = vdfatAll) or
        ShowCombinedStatOnly or ShowCombinedDynOnly
      );

      if ShowCombinedDynOnly then
        ArrayTypes := [datDynArray]
      else
      if ShowCombinedStatOnly then
        ArrayTypes := [datStatArray]
      else
        ArrayTypes := [Low(TLzDbgArrayType)..High(TLzDbgArrayType)];
    end;



    Cnt := AResValue.Count;
    CutOff := (Cnt < AResValue.ArrayLength);
    if CutOff then begin
      SetLength(Results, Cnt+1);
    end
    else
      SetLength(Results, Cnt);

    if ShowLen then begin
      if ShowCombined then begin
        CheckArrayIndexes(AResValue, PrefixIdxList, PrefixIdxCnt, ArrayTypes);
        FCurrentArrayCombineLvl := PrefixIdxCnt;
      end
      else begin
        SetLength(PrefixIdxList, 1);
        PrefixIdxList[0] := AResValue.ArrayLength;
        PrefixIdxCnt := 1;
      end;

      ShowLen := PrefixIdxCnt > 0;
      if ShowLen then begin
        LenStr := '';
        for i := 0 to PrefixIdxCnt - 2 do
          LenStr := LenStr + IntToStr(PrefixIdxList[i]) + ', ';
        LenStr := LenStr + IntToStr(PrefixIdxList[PrefixIdxCnt-1]);
        if PrefixIdxCnt > 1 then
          LenStr := '('+LenStr+')';
        LenStr := Format(drsLen2, [LenStr]);
      end;
    end;

    LoopCurrentArrayLenShown := FCurrentArrayLenShown;
    FCurrentArrayLenShown := False;
    CouldHide := LenPrefix.HideLen and (AResValue.ArrayLength <= LenPrefix.HideLenThresholdCnt);
    HideLenEach := Max(1, LenPrefix.HideLenThresholdEach);
    MaxLen := 1000*1000 div Max(1, ANestLvl*4);
    Len := 0;
    if Cnt > 0 then begin
      dec(FElementCount);
      for i := 0 to Cnt - 1 do begin
        AResValue.SetSelectedIndex(i);
        ElemCnt := FElementCount;
        EntryVal := AResValue.SelectedEntry;
        Results[i] := PrintWatchValueEx(EntryVal, ADispFormat, ANestLvl, AWatchedExpr);
        Len := Len + Length(Results[i]) + SepLen;

        if CouldHide and (
             ( (LenPrefix.HideLenThresholdLen > 0) and
               (Length(Results[i]) > LenPrefix.HideLenThresholdLen)
             ) or
             (FElementCount - ElemCnt > HideLenEach) or
             ( (LenPrefix.HideLenThresholdEach = 0) and (EntryVal.ValueKind in [rdkArray, rdkStruct]) )
           )
        then
          CouldHide := False;

        if CouldSingleLine and (
             ( (MultiLine.ForceSingleLineThresholdLen > 0) and
               (Length(Results[i]) > MultiLine.ForceSingleLineThresholdLen)
             ) or
             (FElementCount - ElemCnt > ForceSingleLineEach) or
             ( (MultiLine.ForceSingleLineThresholdEach = 0) and (EntryVal.ValueKind in [rdkArray, rdkStruct]) )
           )
        then
          CouldSingleLine := False;

        if Len > MaxLen then begin
          CutOff := True;
          Cnt := i+1;
          SetLength(Results, Cnt+1);
        end;
      end;

      if FCurrentArrayLenShown then
        CouldHide := False;
      FCurrentArrayLenShown := FCurrentArrayLenShown or LoopCurrentArrayLenShown;

      if FHasLineBreak or
         ( ( (ANestLvl < FDeepestArray) or (MultiLine.ForceSingleLineReverseDepth <= 1) ) and
           (AResValue.FieldCount > MultiLine.ForceSingleLineThresholdStructFld) ) or
         (Min(FDeepestMultilineLvl, MultiLine.MaxMultiLineDepth) - FCurrentMultilineLvl >= MultiLine.ForceSingleLineReverseDepth)
      then
        CouldSingleLine := False;

      if CutOff then begin
        Results[Cnt] := '...';
        inc(Cnt);
      end;

      if ShowMultiLine and not CouldSingleLine then begin
        if (rpfIndent in FFormatFlags) then begin
          sep := ',' + FLineSeparator + FIndentString;
          sep2 := FLineSeparator + FIndentString;
        end
        else begin
          sep := ',' + FLineSeparator;
          sep2 := FLineSeparator;
        end;
        if Cnt > 1 then
          FHasLineBreak := True;
      end
      else begin
        sep := ', ';
        sep2 := '';
      end;

      if (Cnt > 1) and (not CouldSingleLine) then
        Results[Cnt-1] := Results[Cnt-1] + sep2 +')'
      else
        Results[Cnt-1] := Results[Cnt-1] +')';

      if CouldHide and
         (ANestLvl - FCurrentOuterMostArrayLvl >= LenPrefix.HideLenKeepDepth)
      then
        ShowLen := False;
      if ShowLen then begin
        Results[0] := LenStr + sep2 + '(' + Results[0];
        FCurrentArrayLenShown := True;
      end
      else
        Results[0] := '(' + Results[0];

      Result := AnsiString.Join(sep, Results);
    end
    else begin
      if ShowLen then
        Result := LenStr + '()'
      else
        Result := '()';
    end;

    if OldHasLineBreak then
      FHasLineBreak := True;
  finally
    if FCurrentOuterMostArrayLvl = ANestLvl then
      FCurrentArrayLenShown := OldCurrentArrayLenShown;
    FCurrentOuterMostArrayLvl := OldCurrentOuterMostArrayLvl;
    FOuterMostArrayCount := OldOuterMostArrayCount;
    FCurrentArrayCombineLvl := OldCurrentArrayCombineLvl;
    FIndentString := CurIndentString;
  end;
end;

function TWatchResultPrinter.PrintStruct(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
const
  VisibilityNames: array [TLzDbgFieldVisibility] of string = (
    '', 'private', 'protected', 'public', 'published'
  );
var
  Resolved: TResolvedDisplayFormat;
  FldInfo: TWatchResultDataFieldInfo;
  FldOwner: TWatchResultData;
  vis, sep, tn, Header, we, CurIndentString: String;
  InclVisSect, OldHasLineBreak, CouldSingleLine, ShowMultiLine: Boolean;
  MultiLine: TWatchDisplayFormatMultiline;
  Results: array of string;
  FldIdx, Len, ForceSingleLineEach, ElemCnt, SepLen: Integer;
begin
  inc(FCurrentMultilineLvl);
  Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
  Result := '';

  tn := AResValue.TypeName;
  if (AResValue.StructType in [dstClass, dstInterface])
  then begin
    if (AResValue.DataAddress = 0) then begin
      //Result := PrintNumber(0, 0, FTargetAddressSize, Resolved.Num2);
      Result := 'nil';
      if (Resolved.Address.TypeFormat = vdfAddressTyped) and (tn <> '') then
        Result := tn + '(' + Result + ')';
      exit;
    end;

    if (Resolved.Struct.ShowPointerFormat <> vdfStructPointerOff) or (AResValue.FieldCount = 0)
    then begin
      // TODO: for 32 bit target, sign extend the 2nd argument
      Result := PrintNumber(AResValue.DataAddress, Int64(AResValue.DataAddress), FTargetAddressSize, Resolved.Num2, True);
      if (Resolved.Address.TypeFormat = vdfAddressTyped) and (tn <> '') then begin
        Result := tn + '(' + Result + ')';
        tn := '';
      end;

      if (Resolved.Struct.ShowPointerFormat = vdfStructPointerOnly) or (AResValue.FieldCount = 0) then
        exit;
    end;
  end;
  Header := Result;
  Result := '';

  if Header <> '' then
    Header := Header + ': ';
  if (Resolved.Struct.DataFormat <> vdfStructFull) and
     not(AResValue.StructType in [dstClass, dstInterface])
  then
    tn := '';
  if AResValue.FieldCount = 0 then begin
    Result := Header + tn + '()';
    exit;
  end;

  if (FCurrentMultilineLvl > FDeepestMultilineLvl) and (AResValue.FieldCount > 0) then
    FDeepestMultilineLvl := FCurrentMultilineLvl;

  CurIndentString := FIndentString;
  SepLen := 1; // ' ' space
  MultiLine := FDisplayFormatResolver.ResolveMultiLine(ADispFormat);
  ShowMultiLine := (rpfMultiLine in FFormatFlags) and (FCurrentMultilineLvl <= MultiLine.MaxMultiLineDepth);
  if ShowMultiLine then begin
    SepLen := Length(FLineSeparator);
    if (rpfIndent in FFormatFlags) then begin
      SepLen := SepLen + Length(FIndentString);
      FIndentString := FIndentString + '  ';
    end;
    CouldSingleLine := MultiLine.ForceSingleLine and (AResValue.FieldCount <= MultiLine.ForceSingleLineThresholdStructFld);
  end
  else
    CouldSingleLine := False;

  ForceSingleLineEach := Max(1, MultiLine.ForceSingleLineThresholdEach);
  OldHasLineBreak := FHasLineBreak;
  FHasLineBreak := False;

  we := AWatchedExpr + '.';
  InclVisSect := (Resolved.Struct.DataFormat = vdfStructFull) and (AResValue.StructType in [dstClass, dstObject]);
  FldOwner := nil;
  vis := '';
  dec(FElementCount);
  FldIdx := 1;
  if Resolved.Struct.DataFormat = vdfStructFull then
    inc(FldIdx);
  if InclVisSect then
    inc(FldIdx);
  SetLength(Results, AResValue.FieldCount * FldIdx+1);
  FldIdx := 0;
  if (Header <> '') or (tn <> '') then begin
    Results[FldIdx] := Header + tn + '(';
    inc(FldIdx);
  end;
  Len := 0;
  for FldInfo in AResValue do begin
    if Len > 1 + 1000*1000 div Max(1, ANestLvl*4) then begin
      Results[FldIdx] := '...';
      inc(FldIdx);
      break;
    end;

    if FldOwner <> FldInfo.Owner then begin
      FldOwner := FldInfo.Owner;
      vis := '';

      if (Resolved.Struct.DataFormat = vdfStructFull) and (FldOwner <> nil) and (FldOwner.DirectFieldCount > 0) and
         (AResValue.StructType in [dstClass, dstInterface, dstObject]) // record has no inheritance
      then begin
        Results[FldIdx] := '{' + FldOwner.TypeName + '}';
        Len := Len + Length(Results[FldIdx]) + SepLen;
        inc(FldIdx);
        inc(FElementCount);
      end;
    end;

    if InclVisSect and (vis <> VisibilityNames[FldInfo.FieldVisibility]) then begin
      vis := VisibilityNames[FldInfo.FieldVisibility];
      Results[FldIdx] := vis;
      Len := Len + Length(Results[FldIdx]) + SepLen;
      inc(FldIdx);
      inc(FElementCount);
    end;

    ElemCnt := FElementCount;
    Results[FldIdx] := PrintWatchValueEx(FldInfo.Field, ADispFormat, ANestLvl, we + UpperCase(FldInfo.FieldName)) + ';';
    if Resolved.Struct.DataFormat <> vdfStructValOnly then
      Results[FldIdx] := FldInfo.FieldName + ': ' + Results[FldIdx];
    Len := Len + Length(Results[FldIdx]) + SepLen;

    if CouldSingleLine and (
         ( (MultiLine.ForceSingleLineThresholdLen > 0) and
           (Length(Results[FldIdx]) > MultiLine.ForceSingleLineThresholdLen)
         ) or
         (FElementCount - ElemCnt > ForceSingleLineEach) or
         ( (MultiLine.ForceSingleLineThresholdEach = 0) and (FldInfo.Field.ValueKind in [rdkArray, rdkStruct]) )
       )
    then
      CouldSingleLine := False;

    inc(FldIdx);
  end;

  SetLength(Results, FldIdx);
  if FHasLineBreak or
     (Min(FDeepestMultilineLvl, MultiLine.MaxMultiLineDepth) - FCurrentMultilineLvl > MultiLine.ForceSingleLineReverseDepth)
  then
    CouldSingleLine := False;

  if ShowMultiLine and not CouldSingleLine then begin
    if (rpfIndent in FFormatFlags) then
      sep := FLineSeparator + FIndentString
    else
      sep := FLineSeparator;
  end
  else
    sep := ' ';


  dec(FldIdx);
  if FHasLineBreak then
    Results[FldIdx] := Results[FldIdx] + sep + ')'
  else
    Results[FldIdx] := Results[FldIdx] + ')';

  if not ((Header <> '') or (tn <> '')) then
    Results[0] := '(' + Results[0];

  Result := AnsiString.Join(sep, Results);

  if ((FldIdx > 0) and (Sep <> ' ')) or OldHasLineBreak then
    FHasLineBreak := True;
  FIndentString := CurIndentString;
end;

function TWatchResultPrinter.PrintConverted(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;
begin
  if AResValue.FieldCount = 0 then
    exit('Error: No result');

  if (AResValue.FieldCount = 1) or
     ( (AResValue.Fields[0].Field <> nil) and
       ((AResValue.Fields[0].Field.ValueKind <> rdkError))
     )
  then begin
    Result := PrintWatchValueEx(AResValue.Fields[0].Field, ADispFormat, ANestLvl, AWatchedExpr);
    exit;
  end;

  if (AResValue.FieldCount > 1) then begin
    Result := PrintWatchValueEx(AResValue.Fields[1].Field, ADispFormat, ANestLvl, AWatchedExpr);
    if (AResValue.Fields[0].Field = nil) or
       (AResValue.Fields[0].Field.ValueKind <> rdkError) or
       (AResValue.Fields[0].Field.AsString <> '')
    then
    Result := Result + ' { '
      + PrintWatchValueEx(AResValue.Fields[0].Field, ADispFormat, ANestLvl, AWatchedExpr)
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
  Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
  Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, TargetAddressSize, Resolved.Num2, True);

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
  const ADispFormat: TWatchDisplayFormat; ANestLvl: Integer; const AWatchedExpr: String): String;

  function PrintChar: String;
  var
    Resolved: TResolvedDisplayFormat;
    s: String;
  begin
    Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    result := '';
    s := '';
    if Resolved.Char.MainFormat <> vdfCharOrd then begin
      s := ' = ';
      case AResValue.ByteSize of
         //1: Result := QuoteText(SysToUTF8(char(Byte(AResValue.AsQWord))));
         1: Result := QuoteText(char(Byte(AResValue.AsQWord)));
         2: Result := QuoteWideText(WideChar(Word(AResValue.AsQWord)));
         else
           if Resolved.Char.MainFormat <> vdfCharLetter then
             Result := '#' + IntToStr(AResValue.AsQWord)
           else
             Result := '#' + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num2);
      end;
    end;

    if Resolved.Char.MainFormat <> vdfCharLetter then begin
      Result := Result + s + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num2);
    end;
  end;

  function PrintBool: String;
  var
    Resolved: TResolvedDisplayFormat;
    c: QWord;
    s: String;
  begin
    Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    c := AResValue.AsQWord;
    result := '';
    if Resolved.Bool.MainFormat <> vdfBoolOrd then begin
      if c = 0 then
        Result := 'False'
      else
        Result := 'True';
    end;

    if Resolved.Bool.MainFormat <> vdfBoolName then begin
      s := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num2);
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
    Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);

    result := '';
    s := '';
    if Resolved.Enum.MainFormat <> vdfEnumOrd then begin
      Result := AResValue.AsString;
      if Result = '' then begin
        Result := AResValue.TypeName;
        if Result <> ''
        then Result := Result + '(' + IntToStr(AResValue.AsInt64) + ')'
        else Result := IntToStr(AResValue.AsInt64);
      end;
      s := ' = ';
    end;

    if Resolved.Enum.MainFormat <> vdfEnumName then begin
      Result := Result + s + PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num2);
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
  PtrDeref, PtrDeref2, OldCurrentResValue, OldParentResValue: TWatchResultData;
  Resolved: TResolvedDisplayFormat;
  n, OldCurrentMultilineLvl: Integer;
  StoredSettings: TWatchResStoredSettings;
begin
  inc(ANestLvl);
  OldCurrentResValue := FCurrentResValue;
  OldParentResValue := FParentResValue;
  OldCurrentMultilineLvl := FCurrentMultilineLvl;
  FParentResValue := FCurrentResValue;
  FCurrentResValue := AResValue;
  inc(FElementCount);
  try
    if ANestLvl > MAX_ALLOWED_NEST_LVL then
      exit('...');
    if AResValue = nil then
      exit('???');

    if FCurrentValueFormatter <> nil then begin
      StoreSetting(StoredSettings);
      // for the next IWatchResultPrinter.FormatValue call
      FNextCallIsValueFormatter := True;
      FInValFormNestLevel  := ANestLvl;
      FResValueInFormatter := AResValue;
      FWatchedExprInFormatter := AWatchedExpr;
      //
      try
        if FCurrentValueFormatter.FormatValue(AResValue, ADispFormat, ANestLvl, Self, Result, FWatchedVarName, AWatchedExpr) then
          exit;
      finally
        FNextCallIsValueFormatter := False;
        RestoreSetting(StoredSettings);
      end;
    end
    else
      FCurrentValueFormatter := FNextValueFormatter;

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
      rdkUnsignedNumVal: begin
        Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
        Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num1);
        if Resolved.Num2.Visible then begin
          Result := Result +' = ' +
                    PrintNumber(AResValue.AsQWord, AResValue.AsInt64, AResValue.ByteSize, Resolved.Num2);
        end;
      end;
      rdkPointerVal: begin
        Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
        Result := '';

        PtrDeref :=  PointerValue.DerefData;
        if (Resolved.Pointer.DerefFormat = vdfPointerDerefOnly) then
          dec(FElementCount);
        if (Resolved.Pointer.DerefFormat <> vdfPointerDerefOnly) or (PtrDeref = nil) then begin
          n := AResValue.ByteSize;
          if n = 0 then n := FTargetAddressSize;
          Result := PrintNumber(AResValue.AsQWord, AResValue.AsInt64, n, Resolved.Num2, True);
          if Resolved.Pointer.Address.TypeFormat = vdfAddressTyped then begin
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

        if (Resolved.Pointer.DerefFormat <> vdfPointerDerefOff) and (PtrDeref <> nil) then begin
          while (PtrDeref.ValueKind = rdkPointerVal) and (PtrDeref.DerefData <> nil) do begin
            PtrDeref2 := PtrDeref;
            Result := Result + '^';
            PtrDeref :=  PtrDeref.DerefData;
          end;
          if PtrDeref <> nil then
            Result := Result + '^: ' + PrintWatchValueEx(PtrDeref, ADispFormat, ANestLvl, AWatchedExpr+'^')
          else
            Result := Result + ': ' + PrintWatchValueEx(PtrDeref2, ADispFormat, ANestLvl, AWatchedExpr+'^');
        end;
      end;
      rdkFloatVal: begin
        Resolved := DisplayFormatResolver.ResolveDispFormat(ADispFormat, AResValue);
        if Resolved.Float.NumFormat = vdfFloatScientific then
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
        Result := 'PChar: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl, AWatchedExpr);
        AResValue.SetSelectedIndex(1); // string res
        Result := Result + FLineSeparator
                + 'String: ' + PrintWatchValueEx(AResValue.SelectedEntry, ADispFormat, ANestLvl, AWatchedExpr);
      end;
      rdkArray:  Result := PrintArray(AResValue, ADispFormat, ANestLvl, AWatchedExpr);
      rdkStruct: Result := PrintStruct(AResValue, ADispFormat, ANestLvl, AWatchedExpr);
      rdkConvertRes: Result := PrintConverted(AResValue, ADispFormat, ANestLvl, AWatchedExpr);
      rdkFunction,
      rdkProcedure,
      rdkFunctionRef,
      rdkProcedureRef: Result := PrintProc(AResValue, ADispFormat, ANestLvl);
      rdkVariant: Result := PrintWatchValueEx(AResValue.DerefData, ADispFormat, ANestLvl, AWatchedExpr);
    end;
  finally
    FCurrentResValue := OldCurrentResValue;
    FParentResValue := OldParentResValue;
    FCurrentMultilineLvl := OldCurrentMultilineLvl;
  end;
end;

constructor TWatchResultPrinter.Create;
begin
  FFormatFlags := [rpfMultiLine, rpfIndent];
  FCurrentMultilineLvl    := 0;
  FIndentString := '';
  FTargetAddressSize := SizeOf(Pointer); // TODO: ask debugger
  FDisplayFormatResolver := TDisplayFormatResolver.Create;
end;

destructor TWatchResultPrinter.Destroy;
begin
  inherited Destroy;
  FDisplayFormatResolver.Free;
end;

function TWatchResultPrinter.PrintWatchValue(AResValue: TWatchResultData;
  const ADispFormat: TWatchDisplayFormat; const AWatchedExpr: String): String;
begin
  FNextValueFormatter := nil;
  if FOnlyValueFormatter <> nil then
    FDefaultValueFormatter := FOnlyValueFormatter
  else
    FDefaultValueFormatter := GlobalValueFormatterSelectorList;

  if rpfSkipValueFormatter in FormatFlags then
    FCurrentValueFormatter := nil
  else
    FCurrentValueFormatter := FDefaultValueFormatter;

  if rpfMultiLine in FFormatFlags then
    FLineSeparator := LineEnding
  else
    FLineSeparator := ' ';

  FWatchedVarName := UpperCase(AWatchedExpr);
  FParentResValue := nil;
  FCurrentResValue := nil;
  FElementCount := 0;
  FCurrentMultilineLvl := 0;
  FDeepestMultilineLvl := 0;
  FDeepestArray := 0;
  Result := PrintWatchValueEx(AResValue, ADispFormat, -1, FWatchedVarName);
end;

function TWatchResultPrinter.PrintWatchValueIntf(AResValue: IWatchResultDataIntf;
  const ADispFormat: TWatchDisplayFormat; AFlags: TWatchResultPrinterFlags): String;
var
  AResValObj: TWatchResultData;
  IncLvl: Integer;
begin
  AResValObj := TWatchResultData(AResValue.GetInternalObject);
  FNextValueFormatter := nil;
  if wpfUseDefaultValueFormatterList in AFlags then
    FNextValueFormatter := FCurrentValueFormatter;
  if wpfUseCurrentValueFormatterList in AFlags then
    FNextValueFormatter := FDefaultValueFormatter;

  FCurrentValueFormatter := nil;

  IncLvl := 0;
  if wpfResValueIsNestedValue in AFlags then begin
    if AResValObj <> FResValueInFormatter then
      FCurrentValueFormatter := FNextValueFormatter;
    IncLvl := 1; // Incrementing the level protects against endless loop
  end;

  if FNextCallIsValueFormatter then begin
    FNextCallIsValueFormatter := False;
    Result := PrintWatchValueEx(AResValObj, ADispFormat, FInValFormNestLevel - 1 + IncLvl, FWatchedExprInFormatter); // This will increase it by one, compared to the value given to the formatter
  end
  else begin
    // TOOD: full init? Or Call PrintWatchValueEx ?
    // TODO: inc level/count of iterations
    FParentResValue := nil;
    FCurrentResValue := nil;
    FElementCount := 0;
    FCurrentMultilineLvl := 0;
    FDeepestMultilineLvl := 0;
    FDeepestArray := 0;
    Result := PrintWatchValueEx(AResValObj, ADispFormat, -1, FWatchedExprInFormatter);
  end;
end;

end.

