unit FpErrorMessages;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  SysUtils, variants, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif};

type
   TFpErrorCode = Integer;

resourcestring
  // menu caption from LazDebuggerFpGdbmi package
  fpgdbmiDisplayGDBInsteadOfFpDebugWatches = 'Display GDB instead of FpDebug '
    +'Watches';

  // %0:s is always linebreak
  MsgfpErrAnyError                           = '%1:s';
  MsgfpErrSymbolNotFound_p                   = 'Identifier not found: "%1:s"%2:s';
  MsgfpErrNoMemberWithName                   = 'Member not found: %1:s';
  MsgfpErrorNotAStructure                    = 'Cannot get member "%1:s" from non-structured type: %2:s';
  MsgfpErrorBadFloatSize                     = 'Unsupported float value: Unknown precision';
  MsgfpErrAddressIsNil                       = 'Cannot access data, Address is NIL';
  MsgfpErrIndexOutOfRange                    = 'Index out of range: %1:d';
  MsgfpErrTypeNotIndexable                   = 'The value has no index';
  MsgfpErrExpectedOrdinalVal_p               = 'Expected an ordinal value, but found ''%1:s''%2:s';
  MsgfpErrCannotCastToPointer_p              = 'Can''t cast value to pointer ''%1:s''%2:s';
  MsgfpErrCannotDeref_p                      = 'Can''t dereference expression ''%1:s''%2:s';

  MsgfpInternalErr                           = 'Internal error: %1:s';

  // 100 memreader error
  MsgfpInternalErrfpErrFailedReadMem         = 'Internal error: Failed to read data from memory';
  MsgfpInternalErrCanNotReadInvalidMem       = 'Internal error: Missing data location';
  MsgfpErrReadMemSizeLimit                   = 'Memory read size exceeds limit';
  MsgfpErrCanNotReadMemAtAddr                = 'Failed to read Mem at Address $%1:x';
  MsgfpErrFailedReadRegiseter                = 'Failed to read data from register';
  MsgfpErrFailedWriteMem                     = 'Failed to write data';
  MsgfpInternalErrCanNotWriteInvalidMem      = 'Internal error writing data: Missing data location';
  MsgfpErrCanNotWriteMemAtAddr               = 'Failed to write Mem at Address $%1:x';

  MsgfpErrPasParserEmptyExpression           = 'Empty expression';
  MsgfpErrPasParserUnexpectedEndOfExpression = 'Unexpected end of expression after ''%1:s''';
  MsgfpErrPasParserInvalidExpression         = 'Invalid Expression';
  MsgfpErrPasParserUnexpectedToken_p         = 'Unexpected token ''%1:s''%2:s';
  MsgfpErrPasParserUnknownIntrinsic_p        = 'Intrinsic function ''%1:s'' not found%2:s';

  MsgfpErrPasParserMissingOpenBracket_p      = 'No opening bracket for ''%1:s'' found%2:s';
  MsgfpErrPasParserWrongOpenBracket_p        = 'Mismatched opening bracket (''%3:s'' at pos %2:d) for ''%1:s'' found%4:s';
  MsgfpErrPasParserMissingIndexExpression    = 'Expected Expression but found closing bracket: ''%1:s'' at pos %2:d';
  MsgfpErrPasParserMissingExprAfterComma     = 'Expected Expression after Comma, but found closing bracket %1:s at pos %2:d';
  MsgfpErrPasParserIndexError_Wrapper        = 'Index error at pos %2:d for ''%1:s''[]: %3:s';
  MsgfpErrPasParserUnterminatedString_p      = 'String not terminated%1:s';
  MsgfpErrPasParserExpectedNumber_p          = 'Expected Number, but found ''%1:s''%2:s';

  MsgfpErrTypeHasNoIndex                     = 'Cannot access indexed element in expression %1:s';
  MsgfpErrChangeVariableNotSupported         = 'Changing the value of this variable is not supported';

  (* Any resourcestring endig in *_p may or may not have one of the below included.
     The value may also be an empty string instead.
  *)
  MsgfpErrPasParser_AtStart                  = ' at start of expression';
  MsgfpErrPasParser_PositionAfter            = ' at pos %1:d after ''%2:s''';
  MsgfpErrPasParser_Position                 = ' at pos %1:d';

  // 200 LocationParser
  MsgfpErrLocationParser                     = 'Internal Error: Cannot calculate location.';
  MsgfpErrLocationParserMemRead              = '%1:s (while calculating location)';          // Pass on nested error
  MsgfpErrLocationParserInit                 = 'Internal Error: Cannot calculate location (Init).';
  MsgfpErrLocationParserMinStack             = 'Not enough elements on stack.';             // internally used
  MsgfpErrLocationParserNoAddressOnStack     = 'Not an address on stack';           // internally used

  // 10000 Process/Control errors
  MsgfpErrCreateProcess = 'Failed to start process "%1:s".%0:sError message: %2:d "%3:s".%0:s%4:s';
  MsgfpErrAttachProcess = 'Failed to attach to process "%1:s".%0:sError message: %2:d "%3:s".%0:s%4:s';

const
  fpErrNoError        = TFpErrorCode(0); // not an error
  fpErrAnyError       = TFpErrorCode(1);

  fpErrSymbolNotFound_p                = TFpErrorCode( 2);
  fpErrNoMemberWithName                = TFpErrorCode( 3);
  fpErrorNotAStructure                 = TFpErrorCode( 4);
  fpErrorBadFloatSize                  = TFpErrorCode( 5);
  fpErrAddressIsNil                    = TFpErrorCode( 6);
  fpErrIndexOutOfRange                 = TFpErrorCode( 7);
  fpErrTypeNotIndexable                = TFpErrorCode( 8);
  fpErrExpectedOrdinalVal_p            = TFpErrorCode( 9);
  fpErrCannotCastToPointer_p           = TFpErrorCode(10);
  fpErrCannotDeref_p                   = TFpErrorCode(11);

  fpInternalErr                        = TFpErrorCode( 99);

  // 100 memreader error
  fpInternalErrFailedReadMem           = TFpErrorCode(100);
  fpInternalErrCanNotReadInvalidMem    = TFpErrorCode(101);
  fpErrReadMemSizeLimit                = TFpErrorCode(102);
  fpErrCanNotReadMemAtAddr             = TFpErrorCode(103);
  fpErrFailedReadRegister              = TFpErrorCode(104);
  fpInternalErrCanNotWriteInvalidMem   = TFpErrorCode(105);
  fpErrFailedWriteMem                  = TFpErrorCode(106);
  fpErrCanNotWriteMemAtAddr            = TFpErrorCode(107);

  // 200 LocationParser
  fpErrLocationParser                  = TFpErrorCode(200);
  fpErrLocationParserMemRead           = TFpErrorCode(201);
  fpErrLocationParserInit              = TFpErrorCode(202);
  fpErrLocationParserMinStack          = TFpErrorCode(203);
  fpErrLocationParserNoAddressOnStack  = TFpErrorCode(204);

  // 500 parser
  fpErrPasParserEmptyExpression           = TFpErrorCode(500);
  fpErrPasParserUnexpectedEndOfExpression = TFpErrorCode(501);
  fpErrPasParserInvalidExpression         = TFpErrorCode(502);
  fpErrPasParserUnexpectedToken_p         = TFpErrorCode(503);
  fpErrPasParserUnknownIntrinsic_p        = TFpErrorCode(504);

  fpErrPasParserMissingOpenBracket_p      = TFpErrorCode(530);
  fpErrPasParserWrongOpenBracket_p        = TFpErrorCode(531);
  fpErrPasParserMissingIndexExpression    = TFpErrorCode(532);
  fpErrPasParserMissingExprAfterComma     = TFpErrorCode(533);
  fpErrPasParserIndexError_Wrapper        = TFpErrorCode(534);
  fpErrPasParserUnterminatedString_p      = TFpErrorCode(535);
  fpErrPasParserExpectedNumber_p          = TFpErrorCode(536);

  fpErrPasParser_AtStart                  = TFpErrorCode(595);
  fpErrPasParser_PositionAfter            = TFpErrorCode(596);
  fpErrPasParser_Position                 = TFpErrorCode(597);

  fpErrTypeHasNoIndex                     = TFpErrorCode(1540);
  fpErrChangeVariableNotSupported         = TFpErrorCode(1541);

  // 10000 Process/Control errors
  fpErrCreateProcess                  = TFpErrorCode(10000);
  fpErrAttachProcess                  = TFpErrorCode(10001);

type

  TFpError = array of record
    ErrorCode: TFpErrorCode;
    ErrorData: Array of TVarRec;
    ErrorData2: Array of
      record
        ansi: Ansistring;
        wide: widestring;
        uni: unicodestring;
        vari: variant;
        case integer of
          1: (ext: Extended);
          2: (cur: Currency);
          3: (short: shortstring);
          4: (i64: int64);
          5: (qw: QWord);
      end;
  end;

  TFpErrorTextLookup = function(AnErrorCode: TFpErrorCode; out AnErrorText: AnsiString): Boolean;

  { TFpErrorHandler }

  TFpErrorHandler = class
  private
    FOnErrorTextLookup: TFpErrorTextLookup;
  protected
    function GetErrorRawString(AnErrorCode: TFpErrorCode): string;
  public
    function CreateError(AnErrorCode: TFpErrorCode; AData: array of const): TFpError;
    function CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError; AData: array of const): TFpError;
    function ErrorAsString(AnError: TFpError): string; virtual;
    function ErrorAsString(AnErrorCode: TFpErrorCode; AData: array of const): string; virtual;
  public
    property OnErrorTextLookup: TFpErrorTextLookup read FOnErrorTextLookup write FOnErrorTextLookup;
  end;

function GetFpErrorHandler: TFpErrorHandler;
procedure SetFpErrorHandler(AHandler: TFpErrorHandler);

property ErrorHandler: TFpErrorHandler read GetFpErrorHandler write SetFpErrorHandler;

function IsError(AnError: TFpError): Boolean; inline;
function ErrorCode(AnError: TFpError): TFpErrorCode;  inline;
function NoError: TFpError;  inline;
function CreateError(AnErrorCode: TFpErrorCode): TFpError; inline;
function CreateError(AnErrorCode: TFpErrorCode; AData: array of const): TFpError; inline;
function CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError; AData: array of const): TFpError; inline;
function CreateError(AnErrorCode: TFpErrorCode; AData: array of const; AnError: TFpError): TFpError; inline;

function dbgs(AnError: TFpError): string; overload;

implementation

var TheErrorHandler: TFpErrorHandler = nil;

function GetFpErrorHandler: TFpErrorHandler;
begin
  if TheErrorHandler = nil then
    TheErrorHandler := TFpErrorHandler.Create;
  Result := TheErrorHandler;
end;

procedure SetFpErrorHandler(AHandler: TFpErrorHandler);
begin
  FreeAndNil(TheErrorHandler);
  TheErrorHandler := AHandler;
end;

function IsError(AnError: TFpError): Boolean;
begin
  Result := (length(AnError) > 0) and (AnError[0].ErrorCode <> 0);
end;

function ErrorCode(AnError: TFpError): TFpErrorCode;
begin
  if length(AnError) > 0 then
    Result := AnError[0].ErrorCode
  else
    Result := fpErrNoError; // 0
end;

function NoError: TFpError;
begin
  Result:= nil;
end;

function CreateError(AnErrorCode: TFpErrorCode): TFpError;
begin
  Result := ErrorHandler.CreateError(AnErrorCode, []);
end;

function CreateError(AnErrorCode: TFpErrorCode; AData: array of const): TFpError;
begin
  Result := ErrorHandler.CreateError(AnErrorCode, AData);
end;

function CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError;
  AData: array of const): TFpError;
begin
  Result := ErrorHandler.CreateError(AnErrorCode, AnError, AData);
end;

function CreateError(AnErrorCode: TFpErrorCode; AData: array of const;
  AnError: TFpError): TFpError;
begin
  Result := ErrorHandler.CreateError(AnErrorCode, AnError, AData);
end;

function dbgs(AnError: TFpError): string;
begin
  if IsError(AnError) then
    Result := '[['+ GetFpErrorHandler.ErrorAsString(AnError) +']]'
  else
    Result := '[[no err]]';
end;

{ TFpErrorHandler }

function TFpErrorHandler.GetErrorRawString(AnErrorCode: TFpErrorCode): string;
begin
  Result := '';
  if FOnErrorTextLookup <> nil then
    if FOnErrorTextLookup(AnErrorCode, Result) then
      exit;

  case AnErrorCode of
    fpErrAnyError:         Result := MsgfpErrAnyError;
    fpErrAddressIsNil:     Result := MsgfpErrAddressIsNil;
    fpErrSymbolNotFound_p: Result := MsgfpErrSymbolNotFound_p;
    fpErrNoMemberWithName: Result := MsgfpErrNoMemberWithName;
    fpErrorNotAStructure:  Result := MsgfpErrorNotAStructure;
    fpErrorBadFloatSize:   Result := MsgfpErrorBadFloatSize;
    fpErrIndexOutOfRange:          Result := MsgfpErrIndexOutOfRange;
    fpErrTypeNotIndexable:         Result := MsgfpErrTypeNotIndexable;
    fpErrExpectedOrdinalVal_p:     Result := MsgfpErrExpectedOrdinalVal_p;
    fpErrCannotCastToPointer_p:    Result := MsgfpErrCannotCastToPointer_p;
    fpErrCannotDeref_p:            Result := MsgfpErrCannotDeref_p;

    fpErrPasParserEmptyExpression:           Result := MsgfpErrPasParserEmptyExpression;
    fpErrPasParserUnexpectedEndOfExpression: Result := MsgfpErrPasParserUnexpectedEndOfExpression;
    fpErrPasParserInvalidExpression:         Result := MsgfpErrPasParserInvalidExpression;
    fpErrPasParserUnexpectedToken_p:         Result := MsgfpErrPasParserUnexpectedToken_p;
    fpErrPasParserUnknownIntrinsic_p:        Result := MsgfpErrPasParserUnknownIntrinsic_p;

    fpErrPasParserMissingOpenBracket_p:      Result := MsgfpErrPasParserMissingOpenBracket_p;
    fpErrPasParserWrongOpenBracket_p:        Result := MsgfpErrPasParserWrongOpenBracket_p;
    fpErrPasParserMissingIndexExpression:    Result := MsgfpErrPasParserMissingIndexExpression;
    fpErrPasParserMissingExprAfterComma:     Result := MsgfpErrPasParserMissingExprAfterComma;
    fpErrPasParserIndexError_Wrapper:        Result := MsgfpErrPasParserIndexError_Wrapper;
    fpErrPasParserUnterminatedString_p:      Result := MsgfpErrPasParserUnterminatedString_p;
    fpErrPasParserExpectedNumber_p:          Result := MsgfpErrPasParserExpectedNumber_p;

    fpErrPasParser_AtStart:                  Result := MsgfpErrPasParser_AtStart;
    fpErrPasParser_PositionAfter:            Result := MsgfpErrPasParser_PositionAfter;
    fpErrPasParser_Position:                 Result := MsgfpErrPasParser_Position;

    fpErrTypeHasNoIndex:                     Result := MsgfpErrTypeHasNoIndex;
    fpErrChangeVariableNotSupported:         Result := MsgfpErrChangeVariableNotSupported;

    fpInternalErr:                     Result := MsgfpInternalErr;
    fpInternalErrCanNotReadInvalidMem: Result := MsgfpInternalErrCanNotReadInvalidMem;
    fpErrReadMemSizeLimit:             Result := MsgfpErrReadMemSizeLimit;
    fpInternalErrFailedReadMem:        Result := MsgfpInternalErrfpErrFailedReadMem;
    fpErrCanNotReadMemAtAddr:          Result := MsgfpErrCanNotReadMemAtAddr;
    fpErrFailedReadRegister:           Result := MsgfpErrFailedReadRegiseter;
    fpInternalErrCanNotWriteInvalidMem:Result := MsgfpInternalErrCanNotWriteInvalidMem;
    fpErrFailedWriteMem:               Result := MsgfpErrFailedWriteMem;
    fpErrCanNotWriteMemAtAddr:         Result := MsgfpErrCanNotWriteMemAtAddr;

    fpErrLocationParser:                 Result := MsgfpErrLocationParser;
    fpErrLocationParserMemRead:          Result := MsgfpErrLocationParserMemRead;
    fpErrLocationParserInit:             Result := MsgfpErrLocationParserInit;
    fpErrLocationParserMinStack:         Result := MsgfpErrLocationParserMinStack;
    fpErrLocationParserNoAddressOnStack: Result := MsgfpErrLocationParserNoAddressOnStack;

    fpErrCreateProcess:                  Result := MsgfpErrCreateProcess;
    fpErrAttachProcess:                  Result := MsgfpErrAttachProcess;
  end;
end;

function TFpErrorHandler.CreateError(AnErrorCode: TFpErrorCode;
  AData: array of const): TFpError;
var
  i: Integer;
begin
  SetLength(Result, 1);
  Result[0].ErrorCode := AnErrorCode;
  SetLength(Result[0].ErrorData, Length(AData));
  SetLength(Result[0].ErrorData2, Length(AData));
  for i := low(AData) to high(AData) do begin
    Result[0].ErrorData[i] := AData[i];
    case  AData[i].VType of
       vtExtended      : begin
           Result[0].ErrorData2[i].ext := AData[i].VExtended^;
           Result[0].ErrorData[i].VExtended := @Result[0].ErrorData2[i].ext;
         end;
       vtString        : begin
           Result[0].ErrorData2[i].short := AData[i].VString^;
           Result[0].ErrorData[i].VString := @Result[0].ErrorData2[i].short;
         end;
       vtAnsiString    : begin
           Result[0].ErrorData2[i].ansi := Ansistring(AData[i].VAnsiString);
           Result[0].ErrorData[i].VAnsiString := Pointer(Result[0].ErrorData2[i].ansi);
         end;
       vtCurrency      : begin
           Result[0].ErrorData2[i].cur := AData[i].VCurrency^;
           Result[0].ErrorData[i].VCurrency := @Result[0].ErrorData2[i].cur;
         end;
       vtVariant       : begin
           Result[0].ErrorData2[i].vari := AData[i].VVariant^;
           Result[0].ErrorData[i].VVariant := @Result[0].ErrorData2[i].vari;
         end;
       vtWideString    : begin
           Result[0].ErrorData2[i].wide := WideString(AData[i].VWideString);
           Result[0].ErrorData[i].VWideString := Pointer(Result[0].ErrorData2[i].wide);
         end;
       vtInt64         : begin
           Result[0].ErrorData2[i].i64 := AData[i].VInt64^;
           Result[0].ErrorData[i].VInt64 := @Result[0].ErrorData2[i].i64;
         end;
       vtUnicodeString : begin
           Result[0].ErrorData2[i].uni := unicodestring(AData[i].VUnicodeString);
           Result[0].ErrorData[i].VUnicodeString := pointer(Result[0].ErrorData2[i].uni);
         end;
       vtQWord         : begin
           Result[0].ErrorData2[i].qw := AData[i].VQWord^;
           Result[0].ErrorData[i].VQWord := @Result[0].ErrorData2[i].qw;
         end;
    end;
  end;
end;

function TFpErrorHandler.CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError;
  AData: array of const): TFpError;
var
  i: Integer;
begin
  Result := CreateError(AnErrorCode, AData);
  SetLength(Result, Length(AnError) + 1);
  for i := 0 to Length(AnError) - 1 do
    Result[i+1] := AnError[i];
end;

function TFpErrorHandler.ErrorAsString(AnError: TFpError): string;
var
  RealData: Array of TVarRec;
  i, l: Integer;
  s: String;
begin
  i := Length(AnError) - 1;
  Result := '';
  while i >= 0 do begin
    RealData := AnError[i].ErrorData;
    l := Length(RealData);
    SetLength(RealData, l + 1);
    s := Result;
    UniqueString(s);
    RealData[l].VAnsiString := pointer(s);
    RealData[l].VType := vtAnsiString;
    // to do : Errorcode may be mapped, if required by outer error
    Result := ErrorAsString(AnError[i].ErrorCode, RealData);
    dec(i);
  end;
end;

function TFpErrorHandler.ErrorAsString(AnErrorCode: TFpErrorCode;
  AData: array of const): string;
var
  RealData: Array of TVarRec;
  i: Integer;
  s: String;
begin
  Result := '';
  if AnErrorCode = fpErrNoError then exit;
  SetLength(RealData, Length(AData) + 1);
  s := LineEnding;
  RealData[0].VAnsiString := Pointer(s); // first arg is always line end
  RealData[0].VType := vtAnsiString;
  for i := 0 to Length(AData) - 1 do
    RealData[i + 1] := AData[i];
  s := GetErrorRawString(AnErrorCode);
  if s = '' then s := 'Internal Error: ' + IntToStr(AnErrorCode);
  try
    Result := Format(s, RealData);
  except
    Result := 'Internal Error: "' + s + '"';
  end;
end;

finalization
  FreeAndNil(TheErrorHandler);

end.

