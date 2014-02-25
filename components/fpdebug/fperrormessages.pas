unit FpErrorMessages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLoggerBase;

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
  fpErrLocationParser         = TFpErrorCode(200);
  fpErrLocationParserMemRead  = TFpErrorCode(201);
  fpErrLocationParserMinStack = TFpErrorCode(202);
  fpErrLocationParserNoAddressOnStack = TFpErrorCode(203);
type

  TFpError = array of record
    ErrorCode: TFpErrorCode;
    ErrorData: Array of TVarRec;
    ErrorData2: Array of String;
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

property FpErrorHandler: TFpErrorHandler read GetFpErrorHandler write SetFpErrorHandler;

function IsFpError(AnError: TFpError): Boolean; inline;
function FpErrorCode(AnError: TFpError): TFpErrorCode;  inline;
function FpErrorNone: TFpError;  inline;
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

function IsFpError(AnError: TFpError): Boolean;
begin
  Result := (length(AnError) > 0) and (AnError[0].ErrorCode <> 0);
end;

function FpErrorCode(AnError: TFpError): TFpErrorCode;
begin
  if length(AnError) > 0 then
    Result := AnError[0].ErrorCode
  else
    Result := fpErrNoError; // 0
end;

function FpErrorNone: TFpError;
begin
  Result:= nil;
end;

function CreateError(AnErrorCode: TFpErrorCode): TFpError;
begin
  Result := FpErrorHandler.CreateError(AnErrorCode, []);
end;

function CreateError(AnErrorCode: TFpErrorCode; AData: array of const): TFpError;
begin
  Result := FpErrorHandler.CreateError(AnErrorCode, AData);
end;

function CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError;
  AData: array of const): TFpError;
begin
  Result := FpErrorHandler.CreateError(AnErrorCode, AnError, AData);
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
    if AData[i].VType = vtAnsiString then begin
      Result[0].ErrorData2[i] := AnsiString(AData[i].VAnsiString);
      Result[0].ErrorData[i].VAnsiString := Pointer(Result[0].ErrorData2[i]);
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

