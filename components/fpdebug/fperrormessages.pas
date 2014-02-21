unit FpErrorMessages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLoggerBase;

type
   TFpErrorCode = Integer;

resourcestring
  // %0:s is always linebreak
  MsgfpErrAnyError       = '%1:s';
  MsgfpErrSymbolNotFound = 'Identifier not found: "%1:s"';
  MsgfpErrNoMemberWithName = 'Member not found: %1:s';

const
  fpErrAnyError       = TFpErrorCode(1);
  fpErrSymbolNotFound = TFpErrorCode(2);
  fpErrNoMemberWithName = TFpErrorCode(3);

type

  TFpError = record
    ErrorCode: TFpErrorCode;
    ErrorData: Array of TVarRec;
    ErrorData2: Array of String;
  end;

  { TErrorHandler }

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

function IsFpError(AnError: TFpError): Boolean;
function FpErrorNone: TFpError;

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
  Result := AnError.ErrorCode <> 0;
end;

function FpErrorNone: TFpError;
begin
  Result.ErrorCode := 0;
end;

{ TFpErrorHandler }

function TFpErrorHandler.GetErrorRawString(AnErrorCode: TFpErrorCode): string;
begin
  case AnErrorCode of
    fpErrAnyError:         Result := MsgfpErrAnyError;
    fpErrSymbolNotFound:   Result := MsgfpErrSymbolNotFound;
    fpErrNoMemberWithName: Result := MsgfpErrNoMemberWithName;
  end;
end;

function TFpErrorHandler.CreateError(AnErrorCode: TFpErrorCode;
  AData: array of const): TFpError;
var
  i: Integer;
begin
  Result.ErrorCode := AnErrorCode;
  SetLength(Result.ErrorData, Length(AData));
  SetLength(Result.ErrorData2, Length(AData));
  for i := low(AData) to high(AData) do begin
    Result.ErrorData[i] := AData[i];
    if AData[i].VType = vtAnsiString then begin
      Result.ErrorData2[i] := AnsiString(AData[i].VAnsiString);
      Result.ErrorData[i].VAnsiString := Pointer(Result.ErrorData2[i]);
    end;
  end;
end;

function TFpErrorHandler.CreateError(AnErrorCode: TFpErrorCode; AnError: TFpError;
  AData: array of const): TFpError;
var
  i, j: Integer;
begin
  Result.ErrorCode := AnErrorCode;
  j := Length(AnError.ErrorData);
  SetLength(Result.ErrorData, Length(AData) + j);
  SetLength(Result.ErrorData2, Length(AData) + j);
  for i := 0 to j - 1 do begin
    Result.ErrorData2[i] := AnError.ErrorData2[i];
    Result.ErrorData[i] := AnError.ErrorData[i];
  end;
  for i := low(AData) to high(AData) do begin
    Result.ErrorData[j+i] := AData[i];
    if AData[i].VType = vtAnsiString then begin
      Result.ErrorData2[j+i] := AnsiString(AData[i].VAnsiString);
      Result.ErrorData[j+i].VAnsiString := Pointer(Result.ErrorData2[j+i]);
    end;
  end;
end;

function TFpErrorHandler.ErrorAsString(AnError: TFpError): string;
var
  RealData: Array of TVarRec;
  i: Integer;
begin
  Result := ErrorAsString(AnError.ErrorCode, AnError.ErrorData);
end;

function TFpErrorHandler.ErrorAsString(AnErrorCode: TFpErrorCode;
  AData: array of const): string;
var
  RealData: Array of TVarRec;
  i: Integer;
  s: String;
begin
  Result := '';
  if AnErrorCode = 0 then exit;
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

