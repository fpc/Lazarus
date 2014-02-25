unit FpErrorMessages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, variants, LazLoggerBase;

type
   TFpErrorCode = Integer;

resourcestring
  // %0:s is always linebreak
  MsgfpErrAnyError         = '%1:s';
  MsgfpErrSymbolNotFound   = 'Identifier not found: "%1:s"';
  MsgfpErrNoMemberWithName = 'Member not found: %1:s';
  // 100 memreader error
  MsgfpErrfpErrFailedReadMem   = 'Failed to read data from target mem';
  MsgfpErrCanNotReadInvalidMem = 'Failed to read data from invalid location';
  MsgfpErrCanNotReadMemAtAddr  = 'Failed to read Mem at Address %1:u';
  // 200 LocationParser
  MsgfpErrLocationParser = 'Internal Error: Can not calculate location.';
  MsgfpErrLocationParserMemRead = '%1:s (while calculating location)';          // Pass on nested error
  MsgfpErrLocationParserInit = 'Internal Error: Can not calculate location. (Init)';
  MsgfpErrLocationParserMinStack = 'Not enough elements on stack.';             // internally used
  MsgfpErrLocationParserNoAddressOnStack = 'Not an address on stack';           // internally used

const
  fpErrNoError        = TFpErrorCode(0); // not an error
  fpErrAnyError       = TFpErrorCode(1);

  fpErrSymbolNotFound   = TFpErrorCode(2);
  fpErrNoMemberWithName = TFpErrorCode(3);

  // 100 memreader error
  fpErrFailedReadMem        = TFpErrorCode(100);
  fpErrCanNotReadInvalidMem = TFpErrorCode(101);
  fpErrCanNotReadMemAtAddr  = TFpErrorCode(102);

  // 200 LocationParser
  fpErrLocationParser                 = TFpErrorCode(200);
  fpErrLocationParserMemRead          = TFpErrorCode(201);
  fpErrLocationParserInit             = TFpErrorCode(202);
  fpErrLocationParserMinStack         = TFpErrorCode(203);
  fpErrLocationParserNoAddressOnStack = TFpErrorCode(204);
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

  { TFpErrorHandler }

  TFpErrorHandler = class
  protected
    function GetErrorRawString(AnErrorCode: TFpErrorCode): string;
  public
    function CreateError(AnErrorCode: TFpErrorCode; AData: array of const): TFpError;
    function CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError; AData: array of const): TFpError;
    function ErrorAsString(AnError: TFpError): string;
    function ErrorAsString(AnErrorCode: TFpErrorCode; AData: array of const): string;
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

{ TFpErrorHandler }

function TFpErrorHandler.GetErrorRawString(AnErrorCode: TFpErrorCode): string;
begin
  case AnErrorCode of
    fpErrAnyError:         Result := MsgfpErrAnyError;
    fpErrSymbolNotFound:   Result := MsgfpErrSymbolNotFound;
    fpErrNoMemberWithName: Result := MsgfpErrNoMemberWithName;

    fpErrCanNotReadInvalidMem: Result := MsgfpErrCanNotReadInvalidMem;
    fpErrCanNotReadMemAtAddr: Result := MsgfpErrCanNotReadMemAtAddr;
    fpErrFailedReadMem: Result := MsgfpErrfpErrFailedReadMem;

    fpErrLocationParser: Result := MsgfpErrLocationParser;
    fpErrLocationParserMemRead: Result := MsgfpErrLocationParserMemRead;
    fpErrLocationParserInit: Result := MsgfpErrLocationParserInit;
    fpErrLocationParserMinStack: Result := MsgfpErrLocationParserMinStack;
    fpErrLocationParserNoAddressOnStack: Result := MsgfpErrLocationParserNoAddressOnStack;
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
  i, j: Integer;
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
  for i := 0 to Length(AData) - 1 do
    RealData[i + 1] := AData[i];
  s := GetErrorRawString(AnErrorCode);
  if s = '' then s := 'Internal Error: ' + IntToStr(AnErrorCode);
  Result := Format(s, RealData);
end;

finalization
  FreeAndNil(TheErrorHandler);

end.

