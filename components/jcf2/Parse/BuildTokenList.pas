{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is BuildTokenList.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit BuildTokenList;

{ AFS 29 Nov 1999
 converts the input string of chars into a list of tokens
 This is the lexical analysis phase of the parsing

 2014.11.02 ~bk Added lexing of binary constants (ex. -> const a=%101001;)
 2017.05.17 ~pktv Added lexing of octal constants (ex. -> const a=&777;)
}

{$mode delphi}

interface

uses
  SysUtils, StrUtils,
  { local }
  Tokens, SourceToken, SourceTokenList, JcfUiTools;

type

  TBuildTokenListFlag=(btlOnlyDirectives);
  TBuildTokenListFlags = set of TBuildTokenListFlag;

  { TBuildTokenList }

  TBuildTokenList = class(TObject)
  private
    { property implementation }
    fsSourceCode: String;
    fsFileName: string;

    { woker procs }
    fiCurrentIndex: integer;

    procedure SetSourceCode(const Value: String);

    function Current: Char;
    function CurrentChars(const piCount: integer): String;
    function ForwardChar(const piOffset: integer): Char;
    function ForwardChars(const piOffset, piCount: integer): String;
    procedure Consume(const piCount: integer = 1);
    procedure UndoConsume(const piCount: integer = 1);
    function EndOfFile: boolean;
    function EndOfFileAfter(const piChars: integer): boolean;

    { implementation of GetNextToken }
    function TryReturn(const pcToken: TSourceToken): boolean;

    function TryCurlyComment(const pcToken: TSourceToken): boolean;
    function TrySlashComment(const pcToken: TSourceToken): boolean;
    function TryBracketStarComment(const pcToken: TSourceToken): boolean;

    function TryWhiteSpace(const pcToken: TSourceToken): boolean;
    function TryLiteralString(const pcToken: TSourceToken;
      const pcDelimiter: Char): boolean;
    function TryMultiLineLiteralString(const pcToken: TSourceToken): boolean;

    function TryNumber(const pcToken: TSourceToken): boolean;
    function TryHexNumber(const pcToken: TSourceToken): boolean;
    function TryBinNumber(const pcToken: TSourceToken): boolean; // ~bk 14.11.01
    function TryOctNumber(const pcToken: TSourceToken): boolean; // ~pktv 17.05.19

    function TryDots(const pcToken: TSourceToken): boolean;

    function TryAssign(const pcToken: TSourceToken): boolean;

    function TrySingleCharToken(const pcToken: TSourceToken): boolean;

    function TryPunctuation(const pcToken: TSourceToken): boolean;
    function TryWord(const pcToken: TSourceToken): boolean;

    function GetNextToken: TSourceToken;

  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildTokenList(ASourceTokenList:TSourceTokenList;AFlags:TBuildTokenListFlags=[]);

    property SourceCode: String read fsSourceCode write SetSourceCode;
    property FileName: string read fsFileName write fsFileName;
  end;


implementation

uses
  { local }
  JcfStringUtils, JcfRegistrySettings, ParseError, jcfbaseConsts, FormatFlags;

const
  CurlyLeft =  '{'; //widechar(123);
  CurlyRight = '}'; //widechar(125);

{$push}{$warn 5024 off}
function CheckMultiByte(const pcChar: char): boolean;
begin
  Result := False;
//  if GetRegSettings.CheckMultiByteChars then
//    Result := IsMultiByte(pcChar);  //IsMultiByte(pcChar)-->Result := IsDBCSLeadByte(Byte(pcChar));
end;
{$pop}

function CharIsOctDigit(const c: Char): Boolean;
const
  OctDigits: set of Char = [ '0', '1', '2', '3', '4', '5', '6', '7'];
begin
  Result := (c in OctDigits);
end;

// 123_456 Ok  _123_456 Bad
function CharIsDigitSeparator(c: Char; aIsFirstDigit: Boolean): Boolean;
begin
  result:= (not aIsFirstDigit) and (c='_');
end;

{ TBuildTokenList }

constructor TBuildTokenList.Create;
begin
  inherited;
  SourceCode := '';
end;

destructor TBuildTokenList.Destroy;
begin
  inherited;
end;

procedure TBuildTokenList.SetSourceCode(const Value: String);
begin
  fsSourceCode := Value;
  // reset the index
  fiCurrentIndex := 1;
end;

function TBuildTokenList.GetNextToken: TSourceToken;
var
  lcNewToken: TSourceToken;

  procedure DoAllTheTries;
  begin
    { first look for return }
    if TryReturn(lcNewToken) then
      exit;
    { comments }
    if TryCurlyComment(lcNewToken) then
      exit;
    if TrySlashComment(lcNewToken) then
      exit;
    if TryBracketStarComment(lcNewToken) then
      exit;
    { the rest }
    if TryWhiteSpace(lcNewToken) then
      exit;
    if TryMultiLineLiteralString(lcNewToken) then
      exit;
    if TryLiteralString(lcNewToken, NativeSingleQuote) then
      exit;
    if TryLiteralString(lcNewToken, NativeDoubleQuote) then
      exit;

    if TryWord(lcNewToken) then
      exit;
    if TryNumber(lcNewToken) then
      exit;
    if TryHexNumber(lcNewToken) then
      exit;
    if TryBinNumber(lcNewToken) then // ~bk 2014.11.01
      exit;
    if TryOctNumber(lcNewToken) then // ~pktv 2017.05.19
      exit;

    if TryDots(lcNewToken) then
      exit;

    { attempt assign before colon }
    if TryAssign(lcNewToken) then
      exit;

    if TryPunctuation(lcNewToken) then
      exit;

    if TrySingleCharToken(lcNewToken) then
      exit;

    { default }
    lcNewToken.TokenType  := ttUnknown;
    lcNewToken.SourceCode := Current;
    Consume(1);
  end;

begin
  if EndOfFile then
    Result := nil
  else
  begin
    lcNewToken := TSourceToken.Create;
    lcNewToken.FileName := FileName;
    try
      DoAllTheTries;
      lcNewToken.WordType := WordTypeOfToken(lcNewToken.TokenType);
    except
      lcNewToken.Free;
      raise;
    end;
    Result := lcNewToken;
  end;
end;

{-------------------------------------------------------------------------------
  worker fns for GetNextComment }

function TBuildTokenList.TryBracketStarComment(const pcToken: TSourceToken): boolean;
var
  liCommentLength, lNestedDepth: integer;
  bPossiblyImbalanced: Boolean;
  liCommentStart, liLine, liCol:integer;

  procedure MoveToCommentEnd;
  var
    sCurrentStr: string;
  begin
    { comment is ended by *) or by EOF (bad source) }
    while True do
    begin
      if EndOfFileAfter(liCommentLength) then
      begin
        if bPossiblyImbalanced then
        begin
          // $ (US): 2021-06-28 15:48:59 $
          //  Although it is not a parse error, but I do not want to introduce
          //  another exception class.
          FindLineCol(fsSourceCode, liCommentStart, liLine, liCol);
          pcToken.XPosition := liCol;
          pcToken.YPosition := liLine;
          raise TEParseError.Create(lisMsgUnableToRecoverImbalancedBracketStarComment, pcToken);
        end else
        begin
          bPossiblyImbalanced := True;
        end;
        break;
      end;

      if CheckMultiByte(ForwardChar(liCommentLength)) then
      begin
        liCommentLength := liCommentLength + 2;
        continue;
      end;

      sCurrentStr := ForwardChars(liCommentLength, 2);
      if '(*' = sCurrentStr then
      begin
        Inc(lNestedDepth);
      end else if '*)' = sCurrentStr then
      begin
        Dec(lNestedDepth);
        if (lNestedDepth = 0) or bPossiblyImbalanced then
        begin
          break;
        end;
      end;

      inc(liCommentLength);
    end;

    // include the comment end
    if not EndOfFileAfter(liCommentLength) and (ForwardChars(liCommentLength, 2) = '*)') then
      inc(liCommentLength, 2);
  end;


begin
  Result := False;
  if not (Current = '(') then
    exit;


  if CurrentChars(2) <> '(*' then
    exit;

  liCommentStart := fiCurrentIndex;
  lNestedDepth := 1;
  { if the comment starts with (*) that is not the end of the comment }
  liCommentLength := 2;

  MoveToCommentEnd;
  if bPossiblyImbalanced then
  begin
    lNestedDepth := 1;
    liCommentLength := 2;
    MoveToCommentEnd;
  end;

  pcToken.TokenType := ttComment;
  pcToken.CommentStyle := eBracketStar;
  pcToken.SourceCode := CurrentChars(liCommentLength);
  Consume(liCommentLength);

  Result := True;
end;

function TBuildTokenList.TryCurlyComment(const pcToken: TSourceToken): boolean;
var
  liCommentLength, lNestedDepth: integer;
  bPossiblyImbalanced: Boolean;
  liCommentStart, liLine, liCol:integer;

  procedure MoveToCommentEnd;
  var
    lForwardChar: char;
  begin
    { comment is ended by (close-curly AND lNestedDepth=0) or by EOF (bad source) }
    while True do
    begin
      if EndOfFileAfter(liCommentLength) then
      begin
        if bPossiblyImbalanced then
        begin
          // $ (US): 2021-06-28 15:48:59 $
          //  Although it is not a parse error, but I do not want to introduce
          //  another exception class.
          FindLineCol(fsSourceCode, liCommentStart, liLine, liCol);
          pcToken.XPosition := liCol;
          pcToken.YPosition := liLine;
          raise TEParseError.Create(lisMsgUnableToRecoverImbalancedCurlyComment, pcToken);
        end else
        begin
          bPossiblyImbalanced := True;
        end;
        break;
      end;
      lForwardChar:=ForwardChar(liCommentLength);
      if CheckMultiByte(lForwardChar) then
      begin
        Inc(liCommentLength, 2);
        continue;
      end;
      Inc(liCommentLength);
      if lForwardChar = CurlyLeft then
        Inc(lNestedDepth)
      else if lForwardChar = CurlyRight then begin
        Dec(lNestedDepth);
        if (lNestedDepth = 0) or bPossiblyImbalanced then
          break;
      end;
    end;
  end;

var
  lLen, lPos: integer;
begin
  Result := False;
  if Current <> '{' then
    exit;

  liCommentStart := fiCurrentIndex;
  bPossiblyImbalanced := False;

  pcToken.TokenType  := ttComment;
  lNestedDepth := 1;
  liCommentLength := 1;

  { compiler directive are the comments with a $ just after the open-curly
    this is always the case }
  if ForwardChar(1) = '$' then
    pcToken.CommentStyle := eCompilerDirective
  else
    pcToken.CommentStyle := eCurlyBrace;

  MoveToCommentEnd;
  if bPossiblyImbalanced then
  begin
    lNestedDepth := 1;
    liCommentLength := 1;
    MoveToCommentEnd;
  end;
  pcToken.SourceCode := CurrentChars(liCommentLength);
  Consume(liCommentLength);

  if pcToken.CommentStyle=eCompilerDirective then  // {$I %XXX%} {$include %XXX%}
  begin
    lPos:=0;
    lLen:=length(pcToken.SourceCode);
    if AnsiStartsText('{$I', pcToken.SourceCode) then
      if AnsiStartsText('{$INCLUDE', pcToken.SourceCode) then
        lPos := 10
      else
        lPos := 4;
    if (lPos>0) and (lPos<lLen) and (CharIsWhiteSpace(pcToken.SourceCode[lPos])) then
    begin
      while (lPos<=lLen) and CharIsWhiteSpace(pcToken.SourceCode[lPos]) do
        Inc(lPos);
      if (lPos<lLen) and (pcToken.SourceCode[lPos]='%') then
      begin
        pcToken.CommentStyle := eNotAComment;
        pcToken.WordType:=wtIdentifier;
        pcToken.TokenType:=ttIdentifier;
      end;
    end;
  end;

  Result := True;
end;

function TBuildTokenList.TrySlashComment(const pcToken: TSourceToken): boolean;
var
  liCommentLength: integer;

  procedure MoveToCommentEnd;
  begin
    { comment is ended by return or by EOF (bad source) }
    while True do
    begin
      if EndOfFileAfter(liCommentLength) then
        break;

      if CheckMultiByte(ForwardChar(liCommentLength)) then
      begin
        liCommentLength := liCommentLength + 2;
        continue;
      end;

      if CharIsReturn(ForwardChar(liCommentLength)) then
        break;

      inc(liCommentLength);
    end;
  end;

begin
  Result := False;
  if Current <> '/' then
    exit;

  { until end of line or file }
  if CurrentChars(2) <> '//' then
    exit;

  liCommentLength := 2;

  MoveToCommentEnd;

  pcToken.TokenType := ttComment;
  pcToken.CommentStyle := eDoubleSlash;
  pcToken.SourceCode := CurrentChars(liCommentLength);
  Consume(liCommentLength);

  Result := True;
end;


function TBuildTokenList.TryReturn(const pcToken: TSourceToken): boolean;
var
  chNext: Char;
begin
  Result := False;
  if not CharIsReturn(Current) then
    exit;

  Result := True;
  pcToken.TokenType  := ttReturn;
  pcToken.SourceCode := Current;
  Consume;
  if fiCurrentIndex > Length(fsSourceCode) then
    exit;

  { concat the next return char if it is not the same
    This will recognise <cr><lf> or <lf><cr>, but not <cr><cr> }
  chNext := Current;
  if CharIsReturn(chNext) and (chNext <> pcToken.SourceCode[1]) then
  begin
    pcToken.SourceCode := pcToken.SourceCode + chNext;
    Consume;
  end;
end;

{ delphi 12 multiline string }
function TBuildTokenList.TryMultiLineLiteralString(const pcToken: TSourceToken): boolean;
var
  liCount,liAux:integer;
  liCountEnd:integer;
  liLiteralStart, liLine, liCol, liPos:integer;
begin
  Result := False;
  liLiteralStart := fiCurrentIndex;
  liCount:=0;
  while ForwardChar(liCount)=NativeSingleQuote do
    Inc(liCount);

  if (liCount>=3) and Odd(liCount) then
  begin
    liAux := liCount + 1;
    while ForwardChar(liAux) in NativeTabSpace do
      inc(liAux);
    if not CharIsReturn(ForwardChar(liAux)) then
      exit(False);
    Result := True;
    { read the opening '''  odd number  of single quotes }
    pcToken.SourceCode := pcToken.SourceCode + CurrentChars(liCount);
    Consume(liCount);
    { read until the close ''' }
    repeat
      if Current = #0 then
      begin
        FindLineCol(fsSourceCode, liLiteralStart, liLine, liCol);
        pcToken.XPosition := liCol;
        pcToken.YPosition := liLine;
        liPos := 1;
        raise TEParseError.Create(Format(lisMsgUnterminatedString,[ExtractSubStr(pcToken.SourceCode,liPos,[#13,#10])]),pcToken);
      end;

      if (Current = NativeSingleQuote) then
      begin
        liCountEnd:=0;
        while ForwardChar(liCountEnd)=NativeSingleQuote do
          Inc(liCountEnd);
        pcToken.SourceCode := pcToken.SourceCode + CurrentChars(liCountEnd);
        Consume(liCountEnd);
        if liCountEnd=liCount then
           break;
      end
      else
      begin
        { normal char, read it }
        pcToken.SourceCode := pcToken.SourceCode + Current;
        Consume;
      end;

    until False;

    pcToken.TokenType := ttQuotedLiteralString;
  end;
end;


{ complexities like 'Hello'#32'World' and #$12'Foo' are assemlbed in the parser }
function TBuildTokenList.TryLiteralString(const pcToken: TSourceToken;
  const pcDelimiter: Char): boolean;
var
  liLiteralStart, liLine, liCol: integer;
begin
  Result := False;

  liLiteralStart := fiCurrentIndex;
  if Current = pcDelimiter then
  begin
    Result := True;
    { read the opening ' }
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;

    { read until the close ' }
    repeat
      if Current = #0 then
        break;
      if CharIsReturn(Current) then
      begin
        FindLineCol(fsSourceCode, liLiteralStart, liLine, liCol);
        pcToken.XPosition := liCol;
        pcToken.YPosition := liLine;
        raise TEParseError.Create(Format(lisMsgUnterminatedString,[pcToken.SourceCode]),pcToken);
      end;
      { two quotes in a row are still part of the string }
      if (Current = pcDelimiter) then
      begin
        { two consecutive quote chars inside string, read them }
        if (ForwardChar(1) = pcDelimiter) then
        begin
          pcToken.SourceCode := pcToken.SourceCode + CurrentChars(2);
          Consume(2);
        end
        else
        begin
          { single quote char ends string }
          pcToken.SourceCode := pcToken.SourceCode + Current;
          Consume;
          break;
        end
      end
      else
      begin
        { normal char, read it }
        pcToken.SourceCode := pcToken.SourceCode + Current;
        Consume;
      end;

    until False;

    pcToken.TokenType := ttQuotedLiteralString;
  end;
end;

function TBuildTokenList.TryWord(const pcToken: TSourceToken): boolean;

begin
  Result := False;

  // support reserved words as identifiers
  // example.
  // var &type:integer;
  if Current='&' then
  begin
    if CharIsOctDigit(ForwardChar(1)) then
      Exit;
    pcToken.SourceCode := Current;
    Consume;
    if not CharIsWordChar(Current) then
    begin
      pcToken.TokenType := ttAmpersand;
      exit;
    end;
  end
  else
  begin
    if not CharIsWordChar(Current) then
      exit;
    pcToken.SourceCode := Current;
    Consume;
  end;

  { concat any subsequent word chars }
  while CharIsWordChar(Current) or CharIsDigit(Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  { try to recognise the word as built in }
  pcToken.TokenType := TypeOfToken(pcToken.SourceCode);
  if pcToken.TokenType = ttUnknown then
    pcToken.TokenType := ttIdentifier
  else if pcToken.TokenType = ttNot then    // maybe "not in" delphi 13+ operator
  begin
    if ForwardChar(0) = ' ' then  //I guess it only allow ONE space in between. ???
    begin
      if (UpCase(ForwardChar(1)) = 'I') and
         (UpCase(ForwardChar(2)) = 'N') and
         (not (CharIsWordChar(ForwardChar(3)) or CharIsDigit(ForwardChar(3)))) then
      begin
        pcToken.TokenType := ttIn;
        pcToken.SourceCode := pcToken.SourceCode + ForwardChars(0,3);
        Consume(3);
      end;
    end;
  end
  else if pcToken.TokenType = ttIs then     // maybe "is not" delphi 13+ operator
  begin
    if ForwardChar(0) = ' ' then  //I guess it only allow ONE space in between. ???
    begin
      if (UpCase(ForwardChar(1)) = 'N') and
         (UpCase(ForwardChar(2)) = 'O') and
         (UpCase(ForwardChar(3)) = 'T') and
         (not (CharIsWordChar(ForwardChar(4)) or CharIsDigit(ForwardChar(4)))) then
      begin
        pcToken.SourceCode := pcToken.SourceCode + ForwardChars(0,4);
        Consume(4);
      end;
    end;
  end;
  Result := True;
end;

function TBuildTokenList.TryWhiteSpace(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  if not CharIsWhiteSpaceNoReturn(Current) then
    exit;

  pcToken.TokenType  := ttWhiteSpace;
  pcToken.SourceCode := Current;
  Consume;

  { concat any subsequent return chars }
  while CharIsWhiteSpaceNoReturn(Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  Result := True;
end;

function TBuildTokenList.TryAssign(const pcToken: TSourceToken): boolean;
var
  TwoChars: String;
begin
  Result := False;

  if not (CharInSet(Char(Current), [':', '+', '-', '*', '/'])) then
    exit;

  TwoChars := CurrentChars(2);

  if TwoChars = ':=' then
    pcToken.TokenType := ttAssign
  else
  if TwoChars = '+=' then
    pcToken.TokenType := ttPlusAssign
  else
  if TwoChars = '-=' then
    pcToken.TokenType := ttMinusAssign
  else
  if TwoChars = '*=' then
    pcToken.TokenType := ttTimesAssign
  else
  if TwoChars = '/=' then
    pcToken.TokenType := ttFloatDivAssign
  else
    exit;

  pcToken.SourceCode := TwoChars;
  Consume(2);

  Result := True;
end;

function TBuildTokenList.TryNumber(const pcToken: TSourceToken): boolean;
var
  lbHasDecimalSep, lbFirstDigit: boolean;
begin
  Result := False;

  { recognise a number -
   they don't start with a '.' but may contain one

   a minus sign in front is considered unary operator not part of the number
   this is bourne out by the compiler considering
    '- 0.3' and -0.3' to be the same value
    and -.3 is not legal at all }

  { first one must be a digit }
  if not CharIsDigit(Current) then
    exit;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Current;
  Consume;
  lbHasDecimalSep := False;

  { concat any subsequent number chars
    only one decimal seperator allowed

    also NB that two dots can be array range, as in
    var foo = array[1..100] of integer;
    ie one dat = decimal
    two dots = end of number
  }
  lbFirstDigit := False;
  while CharIsDigit(Current) or (Current = '.') or CharIsDigitSeparator(Current,lbFirstDigit) do
  begin
    lbFirstDigit := False;
    // have we got to the dot?
    if (Current = '.') then
    begin
      if CurrentChars(2) = '..' then
        break;
      if lbHasDecimalSep then
        break;                   // oops! a second one
      lbHasDecimalSep := True;
      lbFirstDigit := True;
    end;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  { scientific notation suffix, eg 3e2 = 30, 2.1e-3 = 0.0021 }

  { check for a trailing 'e' }
  if CharInSet(Current, ['e', 'E']) then
  begin
    // sci notation mode
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;

    // can be a minus or plus here
    if CharInSet(Current, ['-', '+']) then
    begin
      pcToken.SourceCode := pcToken.SourceCode + Current;
      Consume;
    end;

    { exponent must be integer }
    lbFirstDigit := True;
    while CharIsDigit(Current) or CharIsDigitSeparator(Current,lbFirstDigit) do
    begin
      lbFirstDigit := False;
      pcToken.SourceCode := pcToken.SourceCode + Current;
      Consume;
    end;
  end;

  Result := True;
end;

{ NB: do not localise '.' with DecimalSeperator
  Delphi source code does *not* change for this }
function TBuildTokenList.TryHexNumber(const pcToken: TSourceToken): boolean;
var
  lbHasDecimalSep, lbFirstDigit: boolean;
begin
  Result := False;

  { starts with a $ }
  if Current <> '$' then
    exit;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Current;
  Consume;
  lbHasDecimalSep := False;

  { concat any subsequent number chars }
  lbFirstDigit := True;
  while CharIsHexDigitDot(Current) or CharIsDigitSeparator(Current,lbFirstDigit) do
  begin
    lbFirstDigit := False;
    // have we got to the dot?
    if (Current = '.') then
    begin
      if CurrentChars(2) = '..' then
        break;
      if lbHasDecimalSep then
        break;                   // oops! a second one
      lbHasDecimalSep := True;
      lbFirstDigit := True;
    end;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;
  // msdos i8086 segment:offset
  // $SSSS:$OOOO
  if (not lbHasDecimalSep) and (Current=':') and (ForwardChar(1)='$') then
  begin
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
    lbFirstDigit := True;
    while (Current in NativeHexDigits) or CharIsDigitSeparator(Current,lbFirstDigit) do
    begin
      lbFirstDigit := False;
      pcToken.SourceCode := pcToken.SourceCode + Current;
      Consume;
    end;
  end;

  Result := True;
end;

{ Bin numbers are prefixed with % }
function TBuildTokenList.TryBinNumber(const pcToken: TSourceToken): boolean;
var
  lbFirstDigit: boolean;
begin
  Result := False;

  { starts with a % }
  if Current <> '%' then
    exit;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Current;
  Consume;

  { concat any subsequent binary chars }
  lbFirstDigit := True;
  while CharIsBinDigit(Current) or CharIsDigitSeparator(Current,lbFirstDigit) do
  begin
    lbFirstDigit := False;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  Result := True;
end;

{ ~pktb 2017.05.19 - Oct numbers are prefixed with & }
function TBuildTokenList.TryOctNumber(const pcToken: TSourceToken): boolean;
var
  lbFirstDigit: boolean;
begin
  Result := False;

  { starts with a & }
  if Current <> '&' then
    exit;

  //ISN'T A Octal Number.
  if not CharIsOctDigit(ForwardChar(1)) then
  begin
    pcToken.TokenType  := ttAmpersand;
    pcToken.SourceCode := Current;
    Consume;
    result:=true;
    exit;
  end;

  pcToken.TokenType  := ttNumber;
  pcToken.SourceCode := Current;
  Consume;

  { concat any subsequent binary chars }
  lbFirstDigit := True;
  while CharIsOctDigit(Current) or CharIsDigitSeparator(Current,lbFirstDigit) do
  begin
    lbFirstDigit := False;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  Result := True;
end;

{ try the range '..' operator and object access  '.' operator }
function TBuildTokenList.TryDots(const pcToken: TSourceToken): boolean;
begin
  Result := False;

  if Current <> '.' then
    exit;

  pcToken.SourceCode := Current;
  Consume;

  if Current = '.' then
  begin
    pcToken.TokenType  := ttDoubleDot;
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end
  else
  begin
    pcToken.TokenType := ttDot;
  end;

  Result := True;
end;

function TBuildTokenList.TryPunctuation(const pcToken: TSourceToken): boolean;


  function FollowsPunctuation(const chLast, ch: Char): boolean;
  const
    { These have meanings on thier own and should not be recognised as part of the punc.
     e.g '=(' is not a punctation symbol, but 2 of them ( for e.g. in const a=(3);
     simlarly ');' is 2 puncs }
    UnitaryPunctuation: set of AnsiChar = [
      NativeSingleQuote, '"', '(', ')', '[', ']', '{',
      '#', '$', '_', ';', '@', '^', ','];

   { These can't have anything following them:
    for e.g, catch the case if a=-1 then ...
      where '=-1' should be read as '=' '-1' not '=-' '1'
      Nothing legitimately comes after '=' AFAIK
      also a:=a*-1;
      q:=q--1; // q equals q minus minus-one. It sucks but it compiles so it must be parsed
      etc }
    SingleChars: set of AnsiChar = ['=', '+', '-', '/', '\'];

  begin
    Result := False;

    if CharInSet(chLast, UnitaryPunctuation) or CharInSet(ch, UnitaryPunctuation) then
      exit;

    if CharInSet(chLast, SingleChars) then
      exit;

    { '<' or '<' can only be followed by '<', '>' or '='.
     Beware of "if x<-1"
     }
    if CharInSet(chLast, ['<', '>']) and not CharInSet(ch, ['<', '>', '=']) then
      exit;

    // ':' can be followed by '=' only
    if (chLast = ':') and (ch <> '=') then
      exit;

    // * can be followed by another *
    if (chLast = '*') and (ch <> '*') then
      exit;

    // "<<" is the start of two nested generics,
    // likewise '>>' is not an operator, it is two "end-of-generic" signs in sucession
    if (chLast = '<') and (ch = '<') then
      exit(True);    // <<
    if (chLast = '>') and (ch = '>') then
      exit(True);    // >>

    Result := CharIsPuncChar(ch);
  end;

var
  leWordType:  TWordType;
  leTokenType: TTokenType;
  lcLast:      Char;
begin
  Result := False;
  if not CharIsPuncChar(Current) then
    exit;

  pcToken.TokenType := ttPunctuation;
  lcLast := Current;
  pcToken.SourceCode := lcLast;
  Consume;

  { concat any subsequent punc chars }
  while FollowsPunctuation(lcLast, Current) do
  begin
    lcLast := Current;
    pcToken.SourceCode := pcToken.SourceCode + lcLast;
    Consume;
  end;

  if length(pcToken.SourceCode) > 2 then  // nested generic    specialize TC1<TC2<TC3<integer>>>=record  end;
  begin
    // only consume the first >
    UndoConsume(Length(pcToken.SourceCode) - 1);
    pcToken.SourceCode := pcToken.SourceCode[1];
  end;
  { try to recognise the punctuation as an operator }
  TypeOfToken(pcToken.SourceCode, leWordType, leTokenType);
  if leTokenType <> ttUnknown then
  begin
    pcToken.TokenType := leTokenType;
  end;

  Result := True;
end;

function TBuildTokenList.TrySingleCharToken(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  pcToken.TokenType := TypeOfToken(Current);
  if pcToken.TokenType <> ttUnknown then
  begin
    pcToken.SourceCode := Current;
    Consume;
    Result := True;
  end;
end;

procedure TBuildTokenList.BuildTokenList(ASourceTokenList:TSourceTokenList;AFlags:TBuildTokenListFlags=[]);
const
  UPDATE_INTERVAL = 4096; // big increments here, this goes faster than parsing
var
  lcNew:     TSourceToken;
  liCounter: integer;
  lbIncludeToken: boolean;
  liParseDisabledCount: integer;
  liParseDisabledCountRaw: integer;   // needed to emit warning.
  lsError: string;
  leFlags: TFormatFlags;
  lbOn: boolean;
begin
  Assert(SourceCode <> '');
  liCounter := 0;
  liParseDisabledCount := 0;
  liParseDisabledCountRaw := 0;

  while not EndOfFile do
  begin
    lbIncludeToken := True;
    lcNew := nil;
    try
      lcNew := GetNextToken;
      if lcNew<>nil then
      begin
        if (lcNew.TokenType=ttComment) then
        begin
          if ReadCommentJcfFlags(lcNew.SourceCode, lsError, leFlags, lbOn) then
          begin
            if eParse in leFlags then
            begin
              if not lbOn then // //jcf:parse=off
              begin
                Inc(liParseDisabledCount);
                Inc(liParseDisabledCountRaw);
              end
              else
              begin
                if (liParseDisabledCount > 0) then // //jcf:parse=on
                  Dec(liParseDisabledCount);
                Dec(liParseDisabledCountRaw);
              end;
            end;
          end;
        end
        else
        begin
          if (liParseDisabledCount > 0) and lcNew.IsSolid then
            lcNew.TokenType := ttComment;
        end;
        if btlOnlyDirectives in AFlags then
        begin
          if not ((lcNew.TokenType=ttComment) and (lcNew.CommentStyle=eCompilerDirective)) then
            lbIncludeToken := False;
        end;
        if lbIncludeToken then
          ASourceTokenList.Add(lcNew)
        else
          lcNew.Free;
        lcNew := nil;
      end;
      Inc(liCounter);
      GetUI.UpdateGUI(liCounter, UPDATE_INTERVAL);
    except
      lcNew.Free;
      raise;
    end;
  end;
  if liParseDisabledCountRaw <> 0 then
    raise EBuildTokenListWarning.Create(lisMsgImbalancedParseDisable);
end;

function TBuildTokenList.Current: Char;
begin
  if fiCurrentIndex<=Length(fsSourceCode) then
    Result := fsSourceCode[fiCurrentIndex]
  else
    Result := #0;
end;

function TBuildTokenList.CurrentChars(const piCount: integer): String;
begin
  Result := Copy(fsSourceCode, fiCurrentIndex, piCount);
end;

function TBuildTokenList.ForwardChar(const piOffset: integer): Char;
begin
  if (fiCurrentIndex + piOffset) <= Length(fsSourceCode) then
    Result := fsSourceCode[fiCurrentIndex + piOffset]
  else
    Result := #0;
end;

function TBuildTokenList.ForwardChars(const piOffset, piCount: integer): String;
begin
  Result := Copy(fsSourceCode, fiCurrentIndex + piOffset, piCount);
end;


procedure TBuildTokenList.Consume(const piCount: integer);
begin
  inc(fiCurrentIndex, piCount);
end;

procedure TBuildTokenList.UndoConsume(const piCount: integer);
begin
  dec(fiCurrentIndex, piCount);
end;

function TBuildTokenList.EndOfFile: boolean;
begin
  Result := fiCurrentIndex > Length(fsSourceCode);
end;

function TBuildTokenList.EndOfFileAfter(const piChars: integer): boolean;
begin
  Result := (fiCurrentIndex + piChars) > Length(fsSourceCode);
end;

end.
