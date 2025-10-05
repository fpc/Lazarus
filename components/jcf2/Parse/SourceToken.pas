{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Token.pas, released April 2000.
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

unit SourceToken;

{ Created AFS 29 Nov 1999
  Token  - element of source code text }

{$mode delphi}

interface

uses
  SysUtils,
  { local }
  Tokens, ParseTreeNode;

type

  TSourceToken = class(TParseTreeNode)
  private
    { property implementation }
    fsSourceCode: String;
    feTokenType: TTokenType;
    feWordType: TWordType;
    feCommentStyle: TCommentStyle;

    fsFileName: string;
    fiXPosition, fiYPosition: integer;
    fiSolidTokenOnLineIndex: integer;
    fbPreprocessedOut: boolean;

    fePreprocessorSymbol: TPreProcessorSymbolType;
    fsPreProcessorText: String;

  protected
  public
    constructor Create;

    function Describe: string; override;
    function DescribePosition: string;

    function IsSolid: boolean;
    function SourceLine: string;

    function HasChildNode(const peTokens: TTokenTypeSet): boolean; override;
    function HasChildNode(const peTokens: TTokenTypeSet;
      const {%H-}piMaxDepth: integer): boolean; override;

    function SolidChildCount: integer; override;
    function FirstSolidLeaf: TParseTreeNode; override;
    function IsLeaf: boolean; override;

    { navigating the source tree as if it was a list }
    function NextToken: TSourceToken;
    function PriorToken: TSourceToken;
    function NextSolidToken: TSourceToken;
    function PriorSolidToken: TSourceToken;
    function NextSolidTokenType: TTokenType;
    function PriorSolidTokenType: TTokenType;
    function NextTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;
    function PriorTokenWithExclusions(const peExclusions: TTokenTypeSet): TSourceToken;
    function NextTokenTypeWithExclusions(const peExclusions: TTokenTypeSet): TTokenType;
    function PriorTokenTypeWithExclusions(const peExclusions: TTokenTypeSet): TTokenType;

    procedure GeneratePreProcessorData;

    property TokenType: TTokenType Read feTokenType Write feTokenType;
    property WordType: TWordType Read feWordType Write feWordType;

    property SourceCode: String Read fsSourceCode Write fsSourceCode;
    property CommentStyle: TCommentStyle Read feCommentStyle Write feCommentStyle;

    property FileName: string read fsFileName write fsFileName;
    property XPosition: integer Read fiXPosition Write fiXPosition;
    property YPosition: integer Read fiYPosition Write fiYPosition;
    property SolidTokenOnLineIndex: integer
      Read fiSolidTokenOnLineIndex Write fiSolidTokenOnLineIndex;

    property PreprocessorSymbol: TPreProcessorSymbolType Read fePreprocessorSymbol;
    property PreProcessorText: String Read fsPreProcessorText;

    property PreprocessedOut: boolean Read fbPreprocessedOut Write fbPreprocessedOut;
  end;

  TSourceTokenProcedure = procedure(const pt: TSourceToken) of object;

implementation

uses
  jcfbaseConsts;

{-------------------------------------------------------------------------------
 TSourceToken }

constructor TSourceToken.Create;
begin
  inherited;

  feTokenType  := ttUnknown;
  fsSourceCode := '';

  feWordType     := wtNotAWord;
  feCommentStyle := eNotAComment;

  fiXPosition := -1;
  fiYPosition := -1;
  fiSolidTokenOnLineIndex := -1;

  fePreprocessorSymbol := ppNone;
  fsPreProcessorText   := '';
  fbPreprocessedOut    := False;
end;

function TSourceToken.Describe: string;
const
  StructuredTokens: TTokenTypeSet =
    [ttComment, ttNumber, ttQuotedLiteralString, ttUnknown, ttPunctuation, ttIdentifier];
begin
  if TokenType = ttIdentifier then
    Result := SourceCode
  // is, is not, in, not in,  ternary operator, ...
  else if TokenType in [ttIs, ttIn, ttMultiWordOperator] then
    Result := UpperCase(SourceCode)
  else
  begin
    Result := TokenTypeToString(TokenType);
    if (TokenType in StructuredTokens) then
      Result := Result + ' ' + SourceCode;
  end;
end;

function TSourceToken.DescribePosition: string;
begin
  Result := '';

  if YPosition > 0 then
  begin
    Result := Format(lisMsgOnLine, [YPosition]);

    if XPosition > 0 then
      Result := Format(lisMsgPosition, [Result, XPosition]);
  end;
end;

function TSourceToken.HasChildNode(const peTokens: TTokenTypeSet): boolean;
begin
  Result := (TokenType in peTokens);
end;

function TSourceToken.HasChildNode(const peTokens: TTokenTypeSet;
  const piMaxDepth: integer): boolean;
begin
  Result := (TokenType in peTokens);
end;

function TSourceToken.IsSolid: boolean;
begin
  Result := not (TokenType in NotSolidTokens);
end;


function TSourceToken.NextToken: TSourceToken;
var
  lcTemp: TParseTreeNode;
begin
  Result := nil;
  lcTemp := NextLeafNode;
  if lcTemp = nil then
    exit;

  Assert(lcTemp is TSourceToken, 'Next leaf is not token at ' + Describe);
  Result := TSourceToken(lcTemp);
end;

function TSourceToken.PriorToken: TSourceToken;
var
  lcTemp: TParseTreeNode;
begin
  Result := nil;
  lcTemp := PriorLeafNode;
  if lcTemp = nil then
    exit;

  Assert(lcTemp is TSourceToken, 'prior leaf is not token at ' + Describe);
  Result := TSourceToken(lcTemp);
end;

function TSourceToken.NextSolidToken: TSourceToken;
begin
  Result := NextToken;

  while (Result <> nil) and ( not Result.IsSolid) do
    Result := Result.NextToken;
end;

function TSourceToken.NextTokenWithExclusions(
  const peExclusions: TTokenTypeSet): TSourceToken;
begin
  Result := NextToken;

  while (Result <> nil) and (Result.TokenType in peExclusions) do
    Result := Result.NextToken;
end;

function TSourceToken.PriorTokenWithExclusions(
  const peExclusions: TTokenTypeSet): TSourceToken;
begin
  Result := PriorToken;

  while (Result <> nil) and (Result.TokenType in peExclusions) do
    Result := Result.PriorToken;
end;


function TSourceToken.PriorSolidToken: TSourceToken;
begin
  Result := PriorToken;

  while (Result <> nil) and ( not Result.IsSolid) do
    Result := Result.PriorToken;
end;

function TSourceToken.NextSolidTokenType: TTokenType;
var
  lc: TSourceToken;
begin
  Result := ttUnknown;
  lc := NextSolidToken;
  if lc <> nil then
    Result := lc.TokenType;
end;

function TSourceToken.PriorSolidTokenType: TTokenType;
var
  lc: TSourceToken;
begin
  Result := ttUnknown;
  lc := PriorSolidToken;
  if lc <> nil then
    Result := lc.TokenType;
end;

function TSourceToken.NextTokenTypeWithExclusions(const peExclusions: TTokenTypeSet): TTokenType;
var
  lc: TSourceToken;
begin
  Result := ttUnknown;
  lc := NextTokenWithExclusions(peExclusions);
  if lc <> nil then
    Result := lc.TokenType;
end;

function TSourceToken.PriorTokenTypeWithExclusions(const peExclusions: TTokenTypeSet): TTokenType;
var
  lc: TSourceToken;
begin
  Result := ttUnknown;
  lc := PriorTokenWithExclusions(peExclusions);
  if lc <> nil then
    Result := lc.TokenType;
end;



function TSourceToken.SolidChildCount: integer;
begin
  if IsSolid then
    Result := 1
  else
    Result := 0;
end;

function TSourceToken.SourceLine: string;
var
  lcLineToken: TSourceToken;
begin
  // find the return at the start of the line
  // or nil for start of first line
  lcLineToken := self;

  while lcLineToken.PriorToken <> nil do
  begin
    if lcLineToken.PriorToken.TokenType = ttReturn then
      break
    else
      lcLineToken := lcLineToken.PriorToken;
  end;

  // walk to the end of the line
  Result := '';
  while (lcLineToken <> nil) and (lcLineToken.TokenType <> ttReturn) do
  begin
    Result := Result + lcLineToken.SourceCode;
    lcLineToken := lcLineToken.NextToken;
  end;
end;

function TSourceToken.FirstSolidLeaf: TParseTreeNode;
begin
  if IsSolid then
    Result := self
  else
    Result := nil;
end;

function TSourceToken.IsLeaf: boolean;
begin
  Result := True;
end;

procedure TSourceToken.GeneratePreProcessorData;
begin
  GetPreprocessorSymbolData(SourceCode, fePreprocessorSymbol, fsPreProcessorText);
end;

end.
