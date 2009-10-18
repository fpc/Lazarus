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
}

{$I JcfGlobal.inc}

interface

uses
  { local }
  Tokens, SourceToken, SourceTokenList;

type

  TBuildTokenList = class(TObject)
  private
    { property implementation }
    fsSourceCode: WideString;
    fsFileName: string;

    { woker procs }
    fiCurrentIndex: integer;

    procedure SetSourceCode(const Value: WideString);

    function Current: WideChar;
    function CurrentChars(const piCount: integer): WideString;
    function ForwardChar(const piOffset: integer): WideChar;
    function ForwardChars(const piOffset, piCount: integer): WideString;
    procedure Consume(const piCount: integer = 1);
    function EndOfFile: boolean;
    function EndOfFileAfter(const piChars: integer): boolean;

      { implementation of GetNextToken }
    function TryReturn(const pcToken: TSourceToken): boolean;

    function TryCurlyComment(const pcToken: TSourceToken): boolean;
    function TrySlashComment(const pcToken: TSourceToken): boolean;
    function TryBracketStarComment(const pcToken: TSourceToken): boolean;

    function TryWhiteSpace(const pcToken: TSourceToken): boolean;
    function TryLiteralString(const pcToken: TSourceToken;
      const pcDelimiter: WideChar): boolean;

    function TryNumber(const pcToken: TSourceToken): boolean;
    function TryHexNumber(const pcToken: TSourceToken): boolean;

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

    function BuildTokenList: TSourceTokenList;

    property SourceCode: WideString read fsSourceCode write SetSourceCode;
    property FileName: string read fsFileName write fsFileName;
  end;


implementation

uses
 { delphi }
 Forms, SysUtils,
 { local }
 JcfStringUtils, JcfSystemUtils,
 JcfUnicode,
 JcfRegistrySettings;

function CheckMultiByte(const pcChar: WideChar): Boolean;
begin
  Result := False;

  if GetRegSettings.CheckMultiByteChars then
    Result := IsMultiByte(pcChar);
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

procedure TBuildTokenList.SetSourceCode(const Value: WideString);
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
    DoAllTheTries;

    lcNewToken.WordType := WordTypeOfToken(lcNewToken.TokenType);
    Result := lcNewToken;
  end;
end;

{-------------------------------------------------------------------------------
  worker fns for GetNextComment }

function TBuildTokenList.TryBracketStarComment(const pcToken: TSourceToken): boolean;
var
  liCommentLength: integer;

  procedure MoveToCommentEnd;
  begin
    { comment is ended by *) or by EOF (bad source) }
    while True do
    begin
      if EndOfFileAfter(liCommentLength) then
        break;

      if CheckMultiByte(ForwardChar(liCommentLength)) then
      begin
        liCommentLength := liCommentLength + 2;
        continue;
      end;

      if ForwardChars(liCommentLength, 2) = '*)' then
        break;

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

  { if the comment starts with (*) that is not the end of the comment }
  liCommentLength := 2;

  MoveToCommentEnd;

  pcToken.TokenType := ttComment;
  pcToken.CommentStyle := eBracketStar;
  pcToken.SourceCode := CurrentChars(liCommentLength);
  Consume(liCommentLength);
  
  Result := True;
end;

function TBuildTokenList.TryCurlyComment(const pcToken: TSourceToken): boolean;
var
  liCommentLength: integer;

  procedure MoveToCommentEnd;
  begin
    { comment is ended by close-curly or by EOF (bad source) }
    while True do
    begin
      if EndOfFileAfter(liCommentLength) then
        break;

      if CheckMultiByte(ForwardChar(liCommentLength)) then
      begin
        liCommentLength := liCommentLength + 2;
        continue;
      end;

      if ForwardChar(liCommentLength) = '}' then
        break;

      inc(liCommentLength);
    end;

    { include the closing brace }
    if not EndOfFileAfter(liCommentLength) and (ForwardChars(liCommentLength, 1) = '}') then
      inc(liCommentLength);
  end;

begin
  Result := False;
  if Current <> '{' then
    exit;

  pcToken.TokenType  := ttComment;
  liCommentLength := 1;

  { compiler directive are the comments with a $ just after the open-curly
    this is always the case }
  if ForwardChar(1) = '$' then
    pcToken.CommentStyle := eCompilerDirective
  else
    pcToken.CommentStyle := eCurlyBrace;

  MoveToCommentEnd;

  pcToken.SourceCode := CurrentChars(liCommentLength);
  Consume(liCommentLength);
  
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

      if WideCharIsReturn(ForwardChar(liCommentLength)) then
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
  chNext: WideChar;
begin
  Result := False;
  if not WideCharIsReturn(Current) then
    exit;

  pcToken.TokenType  := ttReturn;
  pcToken.SourceCode := Current;
  Consume;

  { concat the next return char if it is not the same
    This will recognise <cr><lf> or <lf><cr>, but not <cr><cr> }

  chNext := Current;
  if WideCharIsReturn(chNext) and (chNext <> pcToken.SourceCode[1]) then
  begin
    pcToken.SourceCode := pcToken.SourceCode + chNext;
    Consume;
  end;
  Result := True;
end;

{ complexities like 'Hello'#32'World' and #$12'Foo' are assemlbed in the parser }
function TBuildTokenList.TryLiteralString(const pcToken: TSourceToken;
  const pcDelimiter: WideChar): boolean;
begin
  Result := False;

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
      if WideCharIsReturn(Current) then
        Raise Exception.Create('Unterminated string: ' + pcToken.SourceCode);

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

  if not WideCharIsWordChar(Current) then
    exit;

  pcToken.SourceCode := Current;
  Consume;

  { concat any subsequent word chars }
  while WideCharIsWordChar(Current) or WideCharIsDigit(Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  { try to recognise the word as built in }
  pcToken.TokenType := TypeOfToken(pcToken.SourceCode);
  if pcToken.TokenType = ttUnknown then
    pcToken.TokenType := ttIdentifier;

  Result := True;
end;

function TBuildTokenList.TryWhiteSpace(const pcToken: TSourceToken): boolean;
begin
  Result := False;
  if not WideCharIsWhiteSpaceNoReturn(Current) then
    exit;

  pcToken.TokenType  := ttWhiteSpace;
  pcToken.SourceCode := Current;
  Consume;

  { concat any subsequent return chars }
  while WideCharIsWhiteSpaceNoReturn(Current) do
  begin
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  Result := True;
end;

function TBuildTokenList.TryAssign(const pcToken: TSourceToken): boolean;
var
  TwoChars: WideString;
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
  lbHasDecimalSep: boolean;
begin
  Result := False;

  { recognise a number -
   they don't start with a '.' but may contain one

   a minus sign in front is considered unary operator not part of the number
   this is bourne out by the compiler considering
    '- 0.3' and -0.3' to be the same value
    and -.3 is not legal at all }

  { first one must be a digit }
  if not WideCharIsDigit(Current) then
    exit;

  if (Current = '.') or (Current = '-') then
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
  while WideCharIsDigit(Current) or (Current = '.') do
  begin
    // have we got to the dot?
    if (Current = '.') then
    begin
      if CurrentChars(2) = '..' then
        break;

      if lbHasDecimalSep then
        // oops! a second one
        break
      else
        lbHasDecimalSep := True;
    end;

    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;
  end;

  { scientific notation suffix, eg 3e2 = 30, 2.1e-3 = 0.0021 }

  { check for a trailing 'e' }
  if WideCharInSet(Current, ['e', 'E']) then
  begin
    // sci notation mode
    pcToken.SourceCode := pcToken.SourceCode + Current;
    Consume;

    // can be a minus or plus here
    if WideCharInSet(Current, ['-', '+']) then
    begin
      pcToken.SourceCode := pcToken.SourceCode + Current;
      Consume;
    end;

    { exponent must be integer }
    while WideCharIsDigit(Current) do
    begin
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
  lbHasDecimalSep: boolean;
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
  while WideCharIsHexDigitDot(Current) do
  begin
    // have we got to the dot?
    if (Current = '.') then
    begin
      if CurrentChars(2) = '..' then
        break;

      if lbHasDecimalSep then
        // oops! a second one
        break
      else
        lbHasDecimalSep := True;
    end;

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


  function FollowsPunctuation(const chLast, ch: WideChar): boolean;
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

    if WideCharInSet(chLast, UnitaryPunctuation) or WideCharInSet(ch, UnitaryPunctuation) then
      exit;

    if WideCharInSet(chLast, SingleChars) then
      exit;

    { '<' or '<' can only be followed by '<', '>' or '='.
     Beware of "if x<-1"
     }
    if WideCharInSet(chLast, ['<', '>']) and not WideCharInSet(ch, ['<', '>', '=']) then
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
      exit;
    if (chLast = '>') and (ch = '>') then
      exit;


    Result := WideCharIsPuncChar(ch);
  end;

var
  leWordType:  TWordType;
  leTokenType: TTokenType;
  lcLast:      WideChar;
begin
  Result := False;

  if not WideCharIsPuncChar(Current) then
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

function TBuildTokenList.BuildTokenList: TSourceTokenList;
const
  UPDATE_INTERVAL = 4096; // big increments here, this goes faster than parsing
var
  lcList:    TSourceTokenList;
  lcNew:     TSourceToken;
  {$IFNDEF COMMAND_LINE}
  liCounter: integer;
  {$ENDIF}
begin
  Assert(SourceCode <> '');

  {$IFNDEF COMMAND_LINE}
  liCounter := 0;
  {$ENDIF}
  lcList    := TSourceTokenList.Create;

  while not EndOfFile do
  begin
    lcNew := GetNextToken;
    lcList.Add(lcNew);

    {$IFNDEF COMMAND_LINE}
    Inc(liCounter);
    if (liCounter mod UPDATE_INTERVAL) = 0 then
       Application.ProcessMessages;
    {$ENDIF}
  end;

  Result := lcList;
end;

function TBuildTokenList.Current: WideChar;
begin
  Result := fsSourceCode[fiCurrentIndex];
end;

function TBuildTokenList.CurrentChars(const piCount: integer): WideString;
begin
  Result := Copy(fsSourceCode, fiCurrentIndex, piCount);
end;

function TBuildTokenList.ForwardChar(const piOffset: integer): WideChar;
begin
  Result := fsSourceCode[fiCurrentIndex + piOffset];
end;

function TBuildTokenList.ForwardChars(const piOffset, piCount: integer): WideString;
begin
  Result := Copy(fsSourceCode, fiCurrentIndex + piOffset, piCount);
end;


procedure TBuildTokenList.Consume(const piCount: integer);
begin
  inc(fiCurrentIndex, piCount);
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
