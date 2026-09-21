{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains CSS scanner and tokenizer

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSScanner;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpCss.Tree;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, fpCSSTree;
{$ENDIF FPC_DOTTEDUNITS}

Type
  TCSSToken =  (
    ctkUNKNOWN,
    ctkEOF,
    ctkINVALID,
    ctkWHITESPACE,
    ctkCOMMENT,
    ctkSEMICOLON,
    ctkLPARENTHESIS, // round bracket
    ctkRPARENTHESIS,
    ctkLBRACE, // curly bracket
    ctkRBRACE,
    ctkLBRACKET, // edged bracket
    ctkRBRACKET,
    ctkCOMMA,
    ctkEQUALS,
    ctkAND,
    ctkTILDE,
    ctkTILDEEQUAL,
    ctkPLUS,
    ctkCOLON,
    ctkDOUBLECOLON,
    ctkDOT,
    ctkDIV,
    ctkGT,
    ctkGE,
    ctkLT,
    ctkLE,
    ctkPERCENTAGE,
    ctkMINUS,
    ctkSTAR,
    ctkSTAREQUAL,
    ctkINTEGER,
    ctkFLOAT,
    ctkHASH,
    ctkSTRING,
    ctkIDENTIFIER,
    ctkATKEYWORD,
    ctkURL,
    ctkBADURL,
    ctkIMPORTANT,
    ctkCLASSNAME,
    ctkFUNCTION,
    ctkPSEUDO,
    ctkPSEUDOFUNCTION,
    ctkSQUARED,
    ctkSQUAREDEQUAL,
    ctkUNICODERANGE,
    ctkPIPE,
    ctkPIPEEQUAL,
    ctkDOLLAR,
    ctkDOLLAREQUAL
   );
  TCSSTokens = Set of TCSSToken;

resourcestring
  SErrInvalidCharacter = 'Invalid character ''%s''';
  SErrOpenString = 'String exceeds end of line';
  SErrIncludeFileNotFound = 'Could not find include file ''%s''';
  SInvalidHexadecimalNumber = 'Invalid decimal number';
  SErrUnknownCharacter = 'Unknown character: %s';

Type
  ECSSScanner = Class(ECSSException);

  TCSSLineReader = class
  protected
    // 0-based byte offset of the next character to be read (i.e. the start of
    // the line returned by the next ReadLine call).
    function GetPosition: Integer; virtual; abstract;
  public
    function IsEOF: Boolean; virtual; abstract;
    function ReadLine: TCSSString; virtual; abstract;
    property Position: Integer read GetPosition;
  end;

  { TCSSStreamLineReader }

  TCSSStreamLineReader = class(TCSSLineReader)
  private
    FStream : TStream;
    Buffer : Array[0..1024] of Byte;
    FBufPos,
    FBufLen : Integer;
    FBufStart : Integer; // 0-based stream offset of Buffer[0]
    procedure FillBuffer;
  protected
    function GetPosition: Integer; override;
  public
    Constructor Create(AStream : TStream);
    function IsEOF: Boolean; override;
    function ReadLine: TCSSString; override;
  end;

  TCSSFileLineReader = class(TCSSLineReader)
  private
    FTextFile: Text;
    FileOpened: Boolean;
    FPosition: Integer; // 0-based offset of the start of the next line
  protected
    function GetPosition: Integer; override;
  public
    constructor Create(const AFilename: TCSSString);
    destructor Destroy; override;
    function IsEOF: Boolean; override;
    function ReadLine: TCSSString; override;
  end;

  { TCSSScanner }

  TCSSScannerOption = (csoExtendedIdentifiers,csoReturnComments,csoReturnWhiteSpace,csoDisablePseudo);
  TCSSScannerOptions = set of TCSSScannerOption;
  TCSSScannerWarnEvent = function(Sender: TObject; Msg: TCSSString; aRow, aCol: integer
    ): boolean of object; // returns true to continue, false to raise an exception

  // A line as delivered by the line reader, together with its stream offset.
  TCSSScannerLine = record
    Line: TCSSString;
    LineStart: Integer; // 0-based stream offset of the start of Line
  end;
  TCSSScannerLines = array of TCSSScannerLine;

  // Result of TCSSScanner.SearchStatementEndInCurLine
  TCSSStatementEnd = (
    cseUnknown, // the rest of the current line does not tell
    cseLBrace,  // found a '{', i.e. the statement is a rule
    cseEnd      // found a ';' or '}', i.e. the statement is a declaration
    );

  // Complete scanner position, see TCSSScanner.SaveState.
  TCSSScannerState = record
    Line: TCSSString;
    TokenOfs: Integer; // offset of TokenStr within Line, -1 if TokenStr=nil
    Row, LineStart: Integer;
    Token: TCSSToken;
    TokenString: TCSSString;
    TokenRow, TokenCol, TokenPos: Integer;
    PrevToken: TCSSToken;
    Options: TCSSScannerOptions;
  end;

  TCSSScanner = class
  private
    FPreviousToken: TCSSToken;
    FOnWarn: TCSSScannerWarnEvent;
    FOptions: TCSSScannerOptions;
    FSourceFile: TCSSLineReader;
    FSourceFilename: TCSSString;
    FCurRow: Integer;
    FCurLineStart: Integer; // 0-based stream offset of the start of FCurLine
    FCurTokenRow: Integer;
    FCurTokenColumn: Integer;
    FCurTokenPos: Integer;
    FCurToken: TCSSToken;
    FCurTokenString: TCSSString;
    FCurLine: TCSSString;
    TokenStr: PCSSChar;
    FSourceStream : TStream;
    FOwnSourceFile : Boolean;
    // The line reader cannot seek. To allow RestoreState to rewind across line
    // boundaries the lines read since SaveState are recorded and served again.
    FRecordLines: Boolean;
    FRecordedLines: TCSSScannerLines; // lines fetched since SaveState
    FPendingLines: TCSSScannerLines;  // lines to serve before reading the source
    FPendingLinePos: Integer;
    function DoHash: TCSSToken;
    function DoIdentifierLike : TCSSToken;
    function DoInvalidChars : TCSSToken;
    function DoMultiLineComment: TCSSToken;
    function CommentDiv: TCSSToken;
    function DoNumericLiteral: TCSSToken;
    function DoSingleLineComment: TCSSToken;
    function DoStringLiteral: TCSSToken;
    function DoStringEscape: TCSSToken;
    function DoWhiteSpace: TCSSToken;
    function EatBadURL: TCSSToken;
    Function DoUnicodeRange : TCSSTOKEN;
    function FetchLine: Boolean;
    function GetCurColumn: Integer;
    function GetOption(anOption: TCSSScannerOption): Boolean;
    function ReadUnicodeEscape: WideChar;
    procedure SetOption(anOption: TCSSScannerOption; const AValue: Boolean);
    class function UnknownCharToStr(C: TCSSChar): TCSSString;
  protected
    procedure DoError(const Msg: TCSSString; Args: array of const); overload;
    procedure DoError(const Msg: TCSSString); overload;
    function DoFetchToken: TCSSToken; virtual;
  public
    constructor Create(ALineReader: TCSSLineReader);
    constructor Create(AStream : TStream);
    destructor Destroy; override;
    procedure OpenFile(const AFilename: TCSSString);
    Function FetchToken: TCSSToken;
    function IsUTF8BOM: boolean;
    function SearchStatementEndInCurLine: TCSSStatementEnd;
    // Speculative scanning: SaveState remembers the current position and starts
    // recording lines, RestoreState rewinds to it, DropState commits.
    function SaveState: TCSSScannerState;
    procedure RestoreState(const aState: TCSSScannerState);
    procedure DropState;
    Property ReturnComments : Boolean Index csoReturnComments Read GetOption Write SetOption;
    Property ReturnWhiteSpace : Boolean Index csoReturnWhiteSpace Read GetOption Write SetOption;
    Property Options : TCSSScannerOptions Read FOptions Write FOptions;
    property SourceFile: TCSSLineReader read FSourceFile;
    property CurFilename: TCSSString read FSourceFilename;
    property CurLine: TCSSString read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;
    property CurTokenRow: Integer read FCurTokenRow; // row of the start of the current token
    property CurTokenColumn: Integer read FCurTokenColumn; // column of the start of the current token
    property CurTokenPos: Integer read FCurTokenPos; // 0-based stream position of the start of the current token
    property CurToken: TCSSToken read FCurToken;
    property CurTokenString: TCSSString read FCurTokenString;
    property PreviousToken: TCSSToken read FPreviousToken;
    property DisablePseudo : Boolean Index csoDisablePseudo Read GetOption Write SetOption;
    property OnWarn: TCSSScannerWarnEvent read FOnWarn write FOnWarn; // if not set raise ECSSScanner
  end;

function SafeFormat(const Fmt: TCSSString; const Args: array of const): TCSSString;
// The text of the bytes of a stylesheet, which are read as UTF-8.
function CSSStringOfBytes(const aBytes: RawByteString): TCSSString;
// The bytes of the text of a stylesheet, written as UTF-8.
function CSSBytesOfString(const aText: TCSSString): RawByteString;

implementation

Const
  Alpha = ['A'..'Z','a'..'z'];
  Num   = ['0'..'9'];
  AlNum = Alpha+Num;
  AlNumIden = AlNum+['-'];
  WhiteSpace = [' ',#9];

type
  TMessageArgs = array of TCSSString;

procedure CreateMsgArgs(var MsgArgs: TMessageArgs; const Args: array of const);
var
  i: Integer;
  A : AnsiString;
  U : UnicodeString;
  {$ifdef pas2js}
  v: jsvalue;
  {$endif}
begin
  SetLength(MsgArgs, High(Args)-Low(Args)+1);
  for i:=Low(Args) to High(Args) do
    {$ifdef pas2js}
    begin
    v:=Args[i];
    if isBoolean(v) then
      MsgArgs[i] := BoolToStr(Boolean(v))
    else if isString(v) then
      MsgArgs[i] := String(v)
    else if isNumber(v) then
      begin
      if IsInteger(v) then
        MsgArgs[i] := str(NativeInt(v))
      else
        MsgArgs[i] := str(double(v));
      end
    else
      MsgArgs[i]:='';
    end;
    {$else}
    case Args[i].VType of
      vtInteger:      MsgArgs[i] := IntToStr(Args[i].VInteger);
      vtBoolean:      MsgArgs[i] := BoolToStr(Args[i].VBoolean);
      vtChar:         MsgArgs[i] := Args[i].VChar;
      {$ifndef FPUNONE}
      vtExtended:     ; //  Args[i].VExtended^;
      {$ENDIF}
      vtString:       MsgArgs[i] := Args[i].VString^;
      vtPointer:      ; //  Args[i].VPointer;
      vtPChar:        MsgArgs[i] := Args[i].VPChar;
      vtObject:       ; //  Args[i].VObject;
      vtClass:        ; //  Args[i].VClass;
      vtWideChar:
        begin
        U:=Args[i].VWideChar;
        MsgArgs[i] := String(U);
        end;
      vtPWideChar:
        begin
        U:=Args[i].VPWideChar;
        MsgArgs[i] := String(U);
        end;
      vtAnsiString:
        begin
        A:=AnsiString(Args[i].VAnsiString);
        MsgArgs[i]:=A;
        end;
      vtCurrency:     ; //  Args[i].VCurrency^);
      vtVariant:      ; //  Args[i].VVariant^);
      vtInterface:    ; //  Args[i].VInterface^);
      vtWidestring:
        begin
        U:=WideString(Args[i].VWideString);
        MsgArgs[i] := String(U);
        end;
      vtInt64:        MsgArgs[i] := IntToStr(Args[i].VInt64^);
      vtQWord:        MsgArgs[i] := IntToStr(Args[i].VQWord^);
      vtUnicodeString:
        begin
        U:=UnicodeString(Args[i].VUnicodeString);
        MsgArgs[i] := String(U);
        end;
    end;
    {$endif}
end;

function CSSStringOfBytes(const aBytes: RawByteString): TCSSString;

begin
{$IF SIZEOF(CHAR)=1}
  Result:=aBytes;
{$ELSE}
  Result:=UTF8Decode(aBytes);
{$ENDIF}
end;


function CSSBytesOfString(const aText: TCSSString): RawByteString;

begin
{$IF SIZEOF(CHAR)=1}
  Result:=aText;
{$ELSE}
  Result:=UTF8Encode(aText);
{$ENDIF}
end;


function SafeFormat(const Fmt: TCSSString;
  const Args: array of const): TCSSString;
var
  MsgArgs: TMessageArgs;
  i: Integer;
begin
  try
    Result:=Format(Fmt,Args);
  except
    Result:='';
    MsgArgs:=nil;
    CreateMsgArgs(MsgArgs,Args);
    for i:=0 to length(MsgArgs)-1 do
      begin
      if i>0 then
        Result:=Result+',';
      Result:=Result+MsgArgs[i];
      end;
    Result:='{'+Fmt+'}['+Result+']';
  end;
end;

constructor TCSSFileLineReader.Create(const AFilename: TCSSString);
begin
  inherited Create;
  Assign(FTextFile, AFilename);
  Reset(FTextFile);
  FileOpened := true;
end;

destructor TCSSFileLineReader.Destroy;
begin
  if FileOpened then
    Close(FTextFile);
  inherited Destroy;
end;

function TCSSFileLineReader.IsEOF: Boolean;
begin
  Result := EOF(FTextFile);
end;

function TCSSFileLineReader.GetPosition: Integer;
begin
  Result:=FPosition;
end;

function TCSSFileLineReader.ReadLine: TCSSString;
begin
  ReadLn(FTextFile, Result);
  // ReadLn strips the line ending, so the exact ending length is unknown here.
  // Assume the platform line ending; byte-exact positions require the stream reader.
  Inc(FPosition,Length(Result)+Length(LineEnding));
end;

constructor TCSSScanner.Create(ALineReader: TCSSLineReader);
begin
  inherited Create;
  FSourceFile := ALineReader;
end;

constructor TCSSScanner.Create(AStream: TStream);
begin
  FSourceStream:=AStream;
  FOwnSourceFile:=True;
  Create(TCSSStreamLineReader.Create(AStream));
end;

destructor TCSSScanner.Destroy;
begin
  If FOwnSourceFile then
    FreeAndNil(FSourceFile);
  inherited Destroy;
end;

procedure TCSSScanner.OpenFile(const AFilename: TCSSString);
begin
  FSourceFile := TCSSFileLineReader.Create(AFilename);
  FSourceFilename := AFilename;
end;

function TCSSScanner.FetchLine: Boolean;
var
  l: Integer;
begin
  if FPendingLinePos<Length(FPendingLines) then
    begin
    // a line read during a speculative scan, served again after RestoreState
    FCurLineStart := FPendingLines[FPendingLinePos].LineStart;
    FCurLine := FPendingLines[FPendingLinePos].Line;
    Inc(FPendingLinePos);
    if FPendingLinePos=Length(FPendingLines) then
      begin
      FPendingLines := nil;
      FPendingLinePos := 0;
      end;
    end
  else if FSourceFile.IsEOF then
  begin
    FCurLine := '';
    TokenStr := nil;
    Result := false;
    exit;
  end else
  begin
    // Capture the stream offset before reading, so it points at the line start.
    FCurLineStart := FSourceFile.Position;
    FCurLine := FSourceFile.ReadLine;
  end;
  if FRecordLines then
    begin
    l := Length(FRecordedLines);
    SetLength(FRecordedLines,l+1);
    FRecordedLines[l].Line := FCurLine;
    FRecordedLines[l].LineStart := FCurLineStart;
    end;
  TokenStr := PCSSChar(CurLine);
  Result := true;
  Inc(FCurRow);
end;

function TCSSScanner.SearchStatementEndInCurLine: TCSSStatementEnd;
// Read the rest of the current line, without fetching any token, and search for
// the end of the statement which starts with the current token:
// a '{' means the statement is a rule, a ';' or '}' means it is a declaration.
// '{' within round or edged brackets are skipped.
// The result is cseUnknown if the line ends first, or if a comment or a string
// might continue on the next line.
var
  p: PCSSChar;
  Depth: Integer;
  Delim: TCSSChar;
  First: Boolean;
begin
  Result:=cseUnknown;
  p:=TokenStr;
  if p=nil then exit;
  Depth:=0;
  First:=true;
  repeat
    case p[0] of
    #0:
      exit; // end of line
    ' ',#9:
      ; // whitespace, the statement continues
    '''','"':
      begin
      // a string, note that it cannot span lines
      Delim:=p[0];
      Inc(p);
      while p[0]<>Delim do
        begin
        if p[0]=#0 then exit; // unterminated, let the scanner complain
        if p[0]='\' then
          begin
          Inc(p);
          if p[0]=#0 then exit;
          end;
        Inc(p);
        end;
      First:=false;
      end;
    '/':
      case p[1] of
      '/':
        exit; // a comment till the end of the line
      '*':
        begin
        // a comment, which can span lines
        Inc(p,2);
        while not ((p[0]='*') and (p[1]='/')) do
          begin
          if p[0]=#0 then exit; // it continues on the next line
          Inc(p);
          end;
        Inc(p); // the '/' is skipped below
        end
      else
        First:=false;
      end;
    '\':
      begin
      // an escaped character
      Inc(p);
      if p[0]=#0 then exit;
      First:=false;
      end;
    ':':
      begin
      if First and ((csoDisablePseudo in FOptions)
          or not ((p[1]=':') or (p[1] in AlNumIden))) then
        // a plain ':' right behind the first token, e.g. 'color: red',
        // in other words: not a pseudo class or element like 'div:hover'
        exit(cseEnd);
      First:=false;
      end;
    '(','[':
      begin
      Inc(Depth);
      First:=false;
      end;
    ')',']':
      begin
      if Depth=0 then exit(cseEnd);
      Dec(Depth);
      First:=false;
      end;
    '{':
      begin
      if Depth=0 then exit(cseLBrace);
      First:=false;
      end;
    ';','}':
      begin
      if Depth=0 then exit(cseEnd);
      First:=false;
      end;
    else
      First:=false;
    end;
    Inc(p);
  until false;
end;

function TCSSScanner.SaveState: TCSSScannerState;
begin
  Result.Line := FCurLine;
  if TokenStr=nil then
    Result.TokenOfs := -1
  else if Length(FCurLine)=0 then
    Result.TokenOfs := 0 // see GetCurColumn: PCSSChar of an empty string
  else
    Result.TokenOfs := TokenStr - PCSSChar(FCurLine);
  Result.Row := FCurRow;
  Result.LineStart := FCurLineStart;
  Result.Token := FCurToken;
  Result.TokenString := FCurTokenString;
  Result.TokenRow := FCurTokenRow;
  Result.TokenCol := FCurTokenColumn;
  Result.TokenPos := FCurTokenPos;
  Result.PrevToken := FPreviousToken;
  Result.Options := FOptions;
  FRecordedLines := nil;
  FRecordLines := true;
end;

procedure TCSSScanner.RestoreState(const aState: TCSSScannerState);
var
  i, Cnt: Integer;
  NewPending: TCSSScannerLines;
begin
  FRecordLines := false;
  // The lines fetched since SaveState must be served again, followed by the
  // lines which were still pending at that time.
  NewPending := nil;
  Cnt := Length(FPendingLines)-FPendingLinePos;
  SetLength(NewPending,Length(FRecordedLines)+Cnt);
  for i := 0 to Length(FRecordedLines)-1 do
    NewPending[i] := FRecordedLines[i];
  for i := 0 to Cnt-1 do
    NewPending[Length(FRecordedLines)+i] := FPendingLines[FPendingLinePos+i];
  FPendingLines := NewPending;
  FPendingLinePos := 0;
  FRecordedLines := nil;

  FCurLine := aState.Line;
  if aState.TokenOfs<0 then
    TokenStr := nil
  else if Length(FCurLine)=0 then
    TokenStr := PCSSChar(FCurLine)
  else
    TokenStr := PCSSChar(FCurLine)+aState.TokenOfs;
  FCurRow := aState.Row;
  FCurLineStart := aState.LineStart;
  FCurToken := aState.Token;
  FCurTokenString := aState.TokenString;
  FCurTokenRow := aState.TokenRow;
  FCurTokenColumn := aState.TokenCol;
  FCurTokenPos := aState.TokenPos;
  FPreviousToken := aState.PrevToken;
  FOptions := aState.Options;
end;

procedure TCSSScanner.DropState;
begin
  FRecordLines := false;
  FRecordedLines := nil;
end;

function TCSSScanner.DoWhiteSpace : TCSSToken;

begin
  Result:=ctkWhitespace;
  repeat
    Inc(TokenStr);
    if TokenStr[0] = #0 then
      if not FetchLine then
       begin
       FCurToken := Result;
       exit;
       end;
  until not (TokenStr[0] in [#9, ' ']);
end;

function TCSSScanner.DoSingleLineComment : TCSSToken;

Var
  TokenStart : PCSSChar;
  Len : Integer;

begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  while TokenStr[0] <> #0 do
     Inc(TokenStr);
  Len:=TokenStr-TokenStart;
  SetLength(FCurTokenString, Len);
  if (Len>0) then
    Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));
  Result := ctkComment;
end;

function TCSSScanner.DoMultiLineComment : TCSSToken;

Var
  TokenStart : PCSSChar;
  Len,OLen : Integer;
  PrevToken : TCSSChar;

begin
  Inc(TokenStr);
  TokenStart := TokenStr;
  FCurTokenString := '';
  OLen:= 0;
  PrevToken:=#0;
  while Not ((TokenStr[0]='/') and (PrevToken='*')) do
    begin
    if (TokenStr[0]=#0) then
      begin
      Len:=TokenStr-TokenStart+1;
      SetLength(FCurTokenString,OLen+Len);
      if Len>1 then
        Move(TokenStart^,FCurTokenString[OLen+1],(Len-1)*SizeOf(TCSSChar));
      Inc(OLen,Len);
      FCurTokenString[OLen]:=#10;
      if not FetchLine then
        begin
        Result := ctkEOF;
        FCurToken := Result;
        exit;
        end;
      TokenStart := TokenStr;
      PrevToken:=#0;
      end
    else
      begin
      PrevToken:=TokenStr[0];
      Inc(TokenStr);
      end;
    end;
  Len:=TokenStr-TokenStart-1; // -1 for *
  SetLength(FCurTokenString, Olen+Len);
  if (Len>0) then
    Move(TokenStart^, FCurTokenString[Olen + 1], Len*SizeOf(TCSSChar));
  Inc(TokenStr);
  Result := ctkComment;
end;

function TCSSScanner.CommentDiv : TCSSToken;

begin
  FCurTokenString := '';
  Inc(TokenStr);
  if (TokenStr[0] = '/') then       // Single-line comment
    Result:=DoSingleLineComment
  else if (TokenStr[0]='*') then
    Result:=DoMultiLineComment
  else
    Result:=ctkDiv;
end;

function TCSSScanner.ReadUnicodeEscape: WideChar;

const
  Hex = ['0'..'9','A'..'F','a'..'f' ];

Var
  S : TCSSString;
  I : Integer;
  HaveHex : Boolean;

begin
  S:='';
  I:=1;
  Repeat
    S:=S+Upcase(TokenStr[0]);
    HaveHex:=TokenStr[1] in Hex;
    if HaveHex then
      Inc(TokenStr);
    Inc(I);
  Until (I>4) or not HaveHex;
  // Takes care of conversion... This needs improvement !!
  Result:=WideChar(StrToInt('$'+S));
end;

procedure TCSSScanner.SetOption(anOption: TCSSScannerOption; const AValue: Boolean
  );
begin
  if AValue then
    Include(FOptions,anOption)
  else
    Exclude(FOptions,anOption);
end;

function TCSSScanner.DoStringLiteral: TCSSToken;

Var
  Delim : TCSSChar;
  TokenStart : PCSSChar;
  Len,OLen: Integer;
  S : TCSSString;

begin
  Delim:=TokenStr[0];
  Inc(TokenStr);
  TokenStart := TokenStr;
  OLen := 0;
  FCurTokenString := '';
  while not (TokenStr[0] in [#0,Delim]) do
    begin
    if (TokenStr[0]='\') then
      begin
      // Save length
      Len := TokenStr - TokenStart;
      Inc(TokenStr);
      // Read escaped token
      Case TokenStr[0] of
        '"' : S:='"';
        'a'..'f',
        'A'..'F',
        '0'..'9':
              begin
              S:=UTF8Encode(ReadUniCodeEscape);
              end;
        #0  :
          begin
          DoError(SErrOpenString);
          exit(ctkINVALID);
          end
      else
        DoError(SErrInvalidCharacter, [TokenStr[0]]);
        S:='';
      end;
      SetLength(FCurTokenString, OLen + Len+1+Length(S));
      if Len > 0 then
        begin
        Move(TokenStart^, FCurTokenString[OLen + 1], Len*SizeOf(TCSSChar));
        Inc(OLen, Len);
        end;
      if S>'' then
        begin
        Move(S[1],FCurTokenString[OLen + 1],Length(S)*SizeOf(TCSSChar));
        Inc(OLen, Length(S));
        end;
      TokenStart := TokenStr+1;
      end;
    if TokenStr[0] = #0 then
      begin
      DoError(SErrOpenString);
      exit(ctkINVALID);
      end;
    Inc(TokenStr);
    end;
  if TokenStr[0] = #0 then
    begin
    DoError(SErrOpenString);
    exit(ctkINVALID);
    end;
  Len := TokenStr - TokenStart;
  SetLength(FCurTokenString, OLen + Len);
  if Len > 0 then
    Move(TokenStart^, FCurTokenString[OLen+1], Len*SizeOf(TCSSChar));
  Inc(TokenStr);
  Result := ctkSTRING;
end;

function TCSSScanner.DoStringEscape: TCSSToken;
var
  TokenStart: PCSSChar;
  Len: Integer;
begin
  Inc(TokenStr); // skip \
  TokenStart := TokenStr;
  while TokenStr[0] in Num do
    inc(TokenStr);
  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
    Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));
  Result:=ctkString;
  FCurTokenString:=TCSSChar(StrToInt(FCurTokenString));
end;

function TCSSScanner.DoNumericLiteral: TCSSToken;
// number: 1, 0.2, .3, 4.01, 0.0, +0.0, -0.0, .50, 2e3, -6.7E-2
const
  NumEnd = [#0..#31,' ',';','{','}',
    ',',
    ')', // e.g. calc(3*4)
    '*','/', // e.g. 3*4, note that + and - require whitespace
    'a'..'z','A'..'Z' // e.g. 3px
    ];

  procedure Skip;
  begin
    while not (TokenStr^ in NumEnd) do inc(TokenStr);
    Result:=ctkUNKNOWN;
  end;

Var
  TokenStart : PCSSChar;
  Len : Integer;
  HasNumber: Boolean;

begin
  Result := ctkINTEGER;
  TokenStart := TokenStr;
  case TokenStr^ of
  '-': inc(TokenStr);
  '+': if csoReturnWhiteSpace in Options then inc(TokenStr);
  end;
  HasNumber:=false;
  if TokenStr^ in Num then
    begin
    // read significand
    HasNumber:=true;
    repeat
      inc(TokenStr);
    until not (TokenStr^ in Num);
    end;
  if TokenStr^='.' then
    begin
    // read fraction
    inc(TokenStr);
    if TokenStr^ in Num then
      begin
      Result := ctkFLOAT;
      HasNumber:=true;
      repeat
        inc(TokenStr);
      until not (TokenStr^ in Num)
      end;
    end;
  if not HasNumber then
    begin
    Skip;
    exit;
    end;
  if (TokenStr^ in ['e','E']) and not (TokenStr[1] in Alpha) then
    begin
    // read exponent
    Result := ctkFLOAT;
    inc(TokenStr);
    if TokenStr^ in ['-','+'] then
      inc(TokenStr);
    if not (TokenStr^ in Num) then
      begin
      Skip;
      exit;
      end;
    repeat
      inc(TokenStr);
    until not (TokenStr^ in Num)
    end;

  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
    Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));
end;

function TCSSScanner.DoHash :TCSSToken;

Var
  TokenStart : PCSSChar;
  Len : Integer;

begin
  Result := ctkHASH;
  TokenStart := TokenStr;
  Inc(TokenStr);
  while (TokenStr[0]<>'#') and (TokenStr[0] in AlNumIden) do
    inc(TokenStr);
  Len:=TokenStr-TokenStart;
  Setlength(FCurTokenString, Len);
  if (Len>0) then
  Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));
end;


function TCSSScanner.EatBadURL: TCSSToken;

var
  TokenStart : PCSSChar;
  C : AnsiChar;
  len,oldlen : integer;

begin
  Result:=ctkURL;
  While not (TokenStr[0] in [#0,')']) do
    begin
    TokenStart:=TokenStr;
    While not (TokenStr[0] in [#0,')']) do
      begin
      C:=TokenStr[0];
      if (Ord(C)<=Ord(' ')) or (Ord(C)>127) then
        Result:=ctkBADURL;
      inc(TokenStr);
      end;
    Len:=TokenStr-TokenStart;
    oldLen:=Length(FCurTokenString);
    Setlength(FCurTokenString, OldLen+Len);
    if (Len>0) then
      Move(TokenStart^,FCurTokenString[OldLen+1],Len*SizeOf(TCSSChar));
    if TokenStr[0]=#0 then
      if not FetchLine then
        Exit(ctkEOF);
    end;
end;

function TCSSScanner.DoUnicodeRange: TCSSTOKEN;
Var
  TokenStart: PCSSChar;
  Len : Integer;
  Tokens : Set of TCSSChar;

begin
  Tokens:= ['A'..'F', 'a'..'f', '0'..'9', '-'];
  Result:=ctkUNICODERANGE;
  TokenStart := TokenStr;
  Inc(TokenStr,2); // U+
  repeat
    if (TokenStr[0]='-') then
      Tokens:=Tokens-['-'];
    Inc(TokenStr);
    //If (TokenStr[0]='\') and (TokenStr[1]='u') then
  until not (TokenStr[0] in Tokens);
  Len:=(TokenStr-TokenStart);
  SetLength(FCurTokenString,Len);
  if Len > 0 then
    Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));

end;

class function TCSSScanner.UnknownCharToStr(C: TCSSChar): TCSSString;

begin
  if C=#0 then
    Result:='EOF'
  else if (C in WhiteSpace) then
    Result:='#'+IntToStr(Ord(C))
  else
    Result:='"'+C+'"';
end;

function TCSSScanner.DoIdentifierLike : TCSSToken;

Var
  TokenStart: PCSSChar;
  Len,oLen : Integer;
  IsEscape,IsAt, IsPseudo, IsFunc : Boolean;

begin
  Result:=ctkIDENTIFIER;
  TokenStart := TokenStr;
  IsPseudo:=False;
  IsAt:=TokenStr[0]='@';
  IsFunc:=false;
  For Len:=1 to 2 do
    if TokenStr[0]=':' then
      begin
      IsPseudo:=True;
      Inc(TokenStr);
      end;
  Repeat
    if not (TokenStr[0]='\') then
      repeat
        Inc(TokenStr);
        //If (TokenStr[0]='\') and (TokenStr[1]='u') then
      until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_','-']);
    IsEscape:=TokenStr[0]='\';
    if IsEscape then
      begin
      if ((TokenStr[0] in WhiteSpace) or (TokenStr[0]=#0))  then
        DoError(SErrUnknownCharacter ,[UnknownCharToStr(TokenStr[0])])
      end
    else if not IsAt then
      begin
      IsFunc:=TokenStr[0]='(';
      if IsFunc then
        Inc(TokenStr);
      end;
    Len:=(TokenStr-TokenStart);
    oLen:=Length(FCurTokenString);
    SetLength(FCurTokenString,Olen+Len);
    if Len > 0 then
      Move(TokenStart^,FCurTokenString[Olen+1],Len*SizeOf(TCSSChar));
    if IsEscape then
      Inc(TokenStr);
    TokenStart := TokenStr;
  until Not IsEscape;
  // Some specials
  if (CurTokenString[1]='.') and not IsFunc then
    Result:=ctkCLASSNAME
  else if isAt then
    Result:=ctkATKEYWORD
  else if CurTokenString='!important' then
    Result:=ctkIMPORTANT
  else if CurTokenString='!' then
    begin
    if (TokenStr^=' ')
    and CompareMem(TokenStr+1,PCSSChar('important'),9*SizeOf(TCSSChar))
    and (TokenStr[10] in [' ',';']) then
      begin
      inc(TokenStr,10);
      FCurTokenString:='!important';
      Result:=ctkIMPORTANT;
      end;
    end
  else if (CurTokenString='url(') then
    begin
    If TokenStr[0] in ['"',''''] then
      begin
      Result:=DoStringLiteral;
      if Result<>ctkSTRING then
        exit;
      Result:=ctkURL;
      end
    else
      begin
      // The token of a url is the address alone, which is what the
      // quoted form above leaves behind. What stands in FCurTokenString
      // here is the "url(" that opened it.
      FCurTokenString:='';
      Result:=EatBadURL;
      end;
    If (result<>ctkEOF) and (TokenStr[0] in [')']) then
      Inc(TokenStr);
    end
  else if IsPseudo then
    begin
    if IsFunc then
      Result:=ctkPSEUDOFUNCTION
    else
      Result:=ctkPSEUDO;
    end
  else if IsFunc then
    Result:=ctkFUNCTION;
end;

function TCSSScanner.DoInvalidChars: TCSSToken;
var
  TokenStart: PCSSChar;
  Len: SizeUInt;
begin
  Result:=ctkUNKNOWN;
  TokenStart := TokenStr;
  repeat
    //writeln('TCSSScanner.DoInvalidChars ',hexstr(ord(TokenStr^),2));
    Inc(TokenStr);
  until (TokenStr[0] in [#0,#9,#10,#13,#32..#127]);
  Len:=TokenStr-TokenStart;
  SetLength(FCurTokenString,Len);
  if Len > 0 then
    Move(TokenStart^,FCurTokenString[1],Len*SizeOf(TCSSChar));
end;

function TCSSScanner.FetchToken: TCSSToken;

var
  CanStop : Boolean;

begin
  Repeat
    Result:=DoFetchToken;
    if (Result=ctkUNKNOWN) and IsUTF8BOM then
      CanStop:=false
    else
      CanStop:=(Not (Result in [ctkComment,ctkWhiteSpace]))
             or ((ReturnComments and (Result=ctkComment))
                  or
                 (ReturnWhiteSpace and (Result=ctkWhiteSpace))
                )
  Until CanStop;
end;

function TCSSScanner.IsUTF8BOM: boolean;
begin
{$IF SIZEOF(CHAR)=1}
  Result:=(length(FCurTokenString)=3)
      and (FCurTokenString[1]=#$EF)
      and (FCurTokenString[2]=#$BB)
      and (FCurTokenString[3]=#$BF);
{$ELSE}
  // The reader gives the three bytes as one character.
  Result:=(length(FCurTokenString)=1) and (FCurTokenString[1]=#$FEFF);
{$ENDIF}
end;

function TCSSScanner.DoFetchToken: TCSSToken;


  Procedure CharToken(aToken : TCSSToken);

  begin
    FCurTokenString:=TokenStr[0];
    Inc(TokenStr);
    Result:=aToken;
  end;

  Procedure TwoCharsToken(aToken : TCSSToken);

  begin
    FCurTokenString:=TokenStr[0]+TokenStr[1];
    Inc(TokenStr,2);
    Result:=aToken;
  end;

begin
  FPreviousToken:=FCurToken;
  if TokenStr = nil then
    begin
    if not FetchLine then
      begin
      Result := ctkEOF;
      FCurToken := Result;
      exit;
      end;
    end;
  FCurTokenRow:=FCurRow;
  if (TokenStr=Nil) or (Length(CurLine)=0) then
    FCurTokenColumn:=0
  else
    FCurTokenColumn:=TokenStr - PCSSChar(CurLine);
  FCurTokenPos:=FCurLineStart+FCurTokenColumn;
  FCurTokenString := '';
  case TokenStr[0] of
    #0:         // EOL
      begin
      FetchLine;
      Result := ctkWhitespace;
      end;
    '''','"':
      Result:=DoStringLiteral;
    '/' :
      Result:=CommentDiv;
    #9, ' ':
      Result:=DoWhiteSpace;
    '#':
      Result:=DoHash;
    '\':
      begin
      if TokenStr[1] in ['0'..'9'] then
        Result:=DoStringEscape
      else
        begin
        if (TokenStr[1] in WhiteSpace) or (TokenStr[1]=#0) then
          DoError(SErrUnknownCharacter ,[UnknownCharToStr(TokenStr[1])])
        else
          Result:=DoIdentifierLike
        end;
      end;
    '0'..'9':
      Result:=DoNumericLiteral;
    '&': CharToken(ctkAnd);
    '{': CharToken(ctkLBRACE);
    '}': CharToken(ctkRBRACE);
    '*': if TokenStr[1]='=' then
           TwoCharsToken(ctkSTAREQUAL)
         else if (csoExtendedIdentifiers in Options) and (TokenStr[1] in AlNumIden) then
           Result:=DoIdentifierLike
         else
           CharToken(ctkSTAR);
    '^':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkSQUAREDEQUAL)
      else
        CharToken(ctkSQUARED);
    ',': CharToken(ctkCOMMA);
    '~':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkTILDEEQUAL)
      else
        CharToken(ctkTILDE);
    '|':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkPIPEEQUAL)
      else
        CharToken(ctkPIPE);
    '$':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkDOLLAREQUAL)
      else
        CharToken(ctkDOLLAR);
    ';': CharToken(ctkSEMICOLON);
    '@': Result:=DoIdentifierLike;
    ':':
      begin
      if csoDisablePseudo in Options then
        CharToken(ctkCOLON)
      else if (TokenStr[1]=':') then
        TwoCharsToken(ctkDoubleCOLON)
      else if (TokenStr[1] in AlNumIden) then
        Result:=DoIdentifierLike
      else
        CharToken(ctkCOLON);
      end;
    '.':
      begin
      if TokenStr[1] in Num then
        Result:=DoNumericLiteral  // e.g. .1 = 0.1
      else if TokenStr[1] in Alpha then
        Result:=DoIdentifierLike
      else
        CharToken(ctkDOT);
      end;
    '>':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkGE)
      else
        CharToken(ctkGT);
    '<':
      if TokenStr[1]='=' then
        TwoCharsToken(ctkLE)
      else
        CharToken(ctkLT);
    '(': CharToken(ctkLPARENTHESIS);
    ')': CharToken(ctkRPARENTHESIS);
    '[': CharToken(ctkLBRACKET);
    ']': CharToken(ctkRBRACKET);
    '=': CharToken(ctkEQUALS);
    '-':
      case TokenStr[1] of
      '0'..'9':
        Result:=DoNumericLiteral;
      '.':
        if TokenStr[2] in Num then
          Result:=DoNumericLiteral
        else
          CharToken(ctkMINUS);
      #9,#10,#13,' ',#0:
        CharToken(ctkMINUS);
      else
        Result:=DoIdentifierLike;
      end;
    '+':
      CharToken(ctkPLUS);
    '%': CharToken(ctkPERCENTAGE);
    '_','!',
    'a'..'z',
    'A'..'Z':
       begin
       if (TokenStr[0] in ['u','U']) and (TokenStr[1]='+') then
         Result:=DoUnicodeRange
       else
         Result:=DoIdentifierLike;
       end;
  else
    //writeln('TCSSScanner.DoFetchToken ',Ord(TokenStr[0]));
    Result:=DoInvalidChars;
  end; // Case
end;

procedure TCSSScanner.DoError(const Msg: TCSSString; Args: array of const);
begin
  DoError(Format(Msg,Args));
end;

procedure TCSSScanner.DoError(const Msg: TCSSString);

Var
  S : TCSSString;

begin
  if Assigned(OnWarn) then
    begin
    if OnWarn(Self,Msg,CurRow,CurColumn) then
      exit;
    end;
  S:=Format('Error at (%d,%d): ',[CurRow,CurColumn])+Msg;
  Raise ECSSScanner.Create(S);
end;

function TCSSScanner.GetCurColumn: Integer;
begin
  if (TokenStr=Nil) or (Length(CurLine)=0) then
    Result:=0
  else
    Result := TokenStr - PCSSChar(CurLine);
end;

function TCSSScanner.GetOption(anOption: TCSSScannerOption): Boolean;
begin
  Result:=anOption in Options;
end;

{ TCSSStreamLineReader }

constructor TCSSStreamLineReader.Create(AStream: TStream);
begin
  FStream:=AStream;
  FBufPos:=0;
  FBufLen:=0;
  FBufStart:=0;
end;

function TCSSStreamLineReader.GetPosition: Integer;
begin
  Result:=FBufStart+FBufPos;
end;

function TCSSStreamLineReader.IsEOF: Boolean;
begin
  Result:=(FBufPos>=FBufLen);
  If Result then
    begin
    FillBuffer;
    Result:=(FBufLen=0);
    end;
end;

procedure TCSSStreamLineReader.FillBuffer;

begin
  Inc(FBufStart,FBufLen); // account for the bytes of the buffer we are replacing
  FBufLen:=FStream.Read(Buffer,SizeOf(Buffer)-1);
  Buffer[FBufLen]:=0;
  FBufPos:=0;
end;

function TCSSStreamLineReader.ReadLine: TCSSString;

Var
  FPos,OLen,Len: Integer;
  PRun : PByte;
  // The bytes of the line, before they are read as UTF-8.
  Bytes : RawByteString;

begin
  Bytes:='';
  FPos:=FBufPos;
  Repeat
    PRun:=@Buffer[FBufPos];
    While (FBufPos<FBufLen) and Not (PRun^ in [10,13]) do
      begin
      Inc(PRun);
      Inc(FBufPos);
      end;
    If (FBufPos=FBufLen) then
      begin
      Len:=FBufPos-FPos;
      If (Len>0) then
        begin
        Olen:=Length(Bytes);
        SetLength(Bytes,OLen+Len);
        Move(Buffer[FPos],Bytes[OLen+1],Len);
        end;
      FillBuffer;
      FPos:=FBufPos;
      end;
  until (FBufPos=FBufLen) or (PRun^ in [10,13]);
  Len:=FBufPos-FPos;
  If (Len>0) then
    begin
    Olen:=Length(Bytes);
    SetLength(Bytes,OLen+Len);
    Move(Buffer[FPos],Bytes[OLen+1],Len)
    end;
  Result:=CSSStringOfBytes(Bytes);
  If (PRun^ in [10,13]) and (FBufPos<FBufLen) then
    begin
    Inc(FBufPos);
    // Check #13#10
    If (PRun^=13) then
      begin
      If (FBufPos=FBufLen) then
        FillBuffer;
      If (FBufPos<FBufLen) and (Buffer[FBufpos]=10) then
        Inc(FBufPos);
      end;
    end;
end;

end.

