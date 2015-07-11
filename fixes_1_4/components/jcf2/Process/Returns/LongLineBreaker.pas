unit LongLineBreaker;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is LongLineBreaker, released May 2003.
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

{$I JcfGlobal.inc}

interface

{ AFS 10 March 2003
  With the Indenter, this is the other big & inportant process.
  Its job is to break long lines.
  This is more complex than most.
  If the line is too long, then the entire line is inspected
  and each token thereon is given a score
  The token with the 'best' (lowest? Highest?) score is where
  (before? after?) the line will be broken 
}

uses SwitchableVisitor, IntList, SourceTokenList;


type
  TLongLineBreaker = class(TSwitchableVisitor)
  private
    fcScores: TIntList;
    fcTokens: TSourceTokenList;

    procedure FixPos;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils,
  SourceToken, FormatFlags, JcfSettings, SetReturns,
  TokenUtils, JcfMiscFunctions, Tokens, ParseTreeNodeType;

function IsLineBreaker(const pcToken: TSourceToken): boolean;
begin
  Result := (pcToken.TokenType in [ttReturn, ttConditionalCompilationRemoved]) or
    IsMultiLineComment(pcToken);
end;

function EndsFunctionReturnType(const pt: TSourceToken): boolean;
var
  lcPrev: TSourceToken;
begin
  Result := False;

  if pt.TokenType <> ttSemicolon then
    exit;

  lcPrev := pt.PriorToken;
  Result := lcPrev.HasParentNode(nFunctionReturnType, 2);
end;

function PositionScore(const piIndex, piIndexOfFirstSolidToken, piPos: integer): integer;
const
  NOGO_PLACE   = -100; // the pits
  PLATEAU      = 100;  // the baseline
  PAST_END     = 0;
  ROOFSLOPE    = 5; // the plateau slopes a bit up to a point /\
  INCREASE_TO_RIGHT_FACTOR = 15; // and it also slopes to the right
  WIDTH_SCORE_FACTOR = 5;
  TO_FAR_SCORE_FACTOR = 10;
  FAR_TO_FAR_SCORE_FACTOR = 0.3;
  FIRST_TOKENS = 3;
var
  liEffectiveWidth: integer;
  liEffectivePos: integer;
  liMidPoint: integer;
  liOneThirdPoint, liThreeQuarterPoint: integer;
  liClose:    integer;
  liOverFlow: integer;
  fUnderflow: double;
begin
  if piIndex < piIndexOfFirstSolidToken then
  begin
    Result := NOGO_PLACE;
    exit;
  end;

  { middle of the actual line (ie from first token pos to max length) }
  liEffectiveWidth := FormatSettings.Returns.MaxLineLength - piIndexOfFirstSolidToken;
  liMidPoint      := (liEffectiveWidth div 2) + piIndexOfFirstSolidToken;
  liOneThirdPoint := (liEffectiveWidth div 3) + piIndexOfFirstSolidToken;

  if piPos < liOneThirdPoint then
  begin
    { slope up evenly over the first third
      want a fn that at piIndexOfFirstSolidToken is NOGO_PLACE
      and slopes up evenly to PLATEAU at liMidPoint }

    { this is the distance from the first tokne  }
    liEffectivePos := piPos - piIndexOfFirstSolidToken;

    Result := NOGO_PLACE + ((PLATEAU - NOGO_PLACE) * liEffectivePos * 3) div
      liEffectiveWidth;
  end
  else if piPos < FormatSettings.Returns.MaxLineLength then
  begin
    { relatively flat plateau, slight bump in the middle }
    liThreeQuarterPoint := (liEffectiveWidth * 3 div 4) + piIndexOfFirstSolidToken;
    { how close to it }
    liClose := liMidPoint - abs(piPos - liThreeQuarterPoint) + 1;
   // Assert(liClose >= 0, 'Close is neg: ' + IntToStr(liClose));

    Result := PLATEAU + (liClose * 2 div ROOFSLOPE);
  end
  else
  begin
    { past the end}
    liOverFlow := piPos - FormatSettings.Returns.MaxLineLength;
    Result     := PLATEAU - (liOverFlow * TO_FAR_SCORE_FACTOR);
    if Result < PAST_END then
    begin
      fUnderflow := PAST_END - Result;
      { must make is slightly lower the further we go -
      otherwise the last pos is found in lines that are far too long with no good place to break
      eg lines that start with a very long text string }
      Result     := PAST_END - Round(fUnderflow * FAR_TO_FAR_SCORE_FACTOR);
    end;

    { normally a place to break before the end will be found,
      but if the first possible place to break is past the end position,
      don't consider it a bad place because any break past the end is better than none }
    if piIndex <= FIRST_TOKENS then
      Result := Result + PLATEAU;
  end;

  { general slope up to the right
    this results in the RHS slope of the plateau being favoured over the left one }
  if Result > NOGO_PLACE then
    Result := Result + (piPos div INCREASE_TO_RIGHT_FACTOR);
end;

function BracketScore(const pcToken: TSourceToken): integer;
const
  BRACKET_SCALE = -8;
begin
  { less of a good place if it is in brackets }
  Result := (RoundBracketLevel(pcToken) + SquareBracketLevel(pcToken)) * BRACKET_SCALE;
end;


{ experimental - score for line breaking based on the parse tree
  The idea is that higher up the tree is going to be a better place to break
  as it represents a natural break in the program flow

  larger number are better so invert

function TreeScore(const pcToken: TSourceToken): integer;
const
  DEPTH_FACTOR = 5;
  FIRST_CHILD_FACTOR = 20;
begin
  Result := - (pcToken.Level * DEPTH_FACTOR);
  if pcToken.IndexOfSelf = 0 then
    Result := Result + FIRST_CHILD_FACTOR;
end;
}


{ want to capture the effect that that breaking the line near the end
  (ie with few chars to go) is bad.
  e.g. max allowed line length is 100, actual line is 102 chars long
  breaking at char 101, just before the semicolon, would really suck

  Have used a factor that breaking 15 or more chars from the end
  incurs no penalty, Penalty is not linear:
  It is insignificant for 15, 14, chars from end,
  getting sizeable from 10 down to 5 spaces to the end
  and very large for 1,2 chars to end

  The function that I have used is Ceil((x ^ 2) / 2)
  where x is (15 - <spaces to end>)
}
function NearEndScore(const piSpacesToEnd: integer; const pcNext: TSourceToken): integer;
const
  TAIL_SIZE: integer = 15;

  penalties: array[1..15] of integer = (
    113, 98, 85, 72, 61,
    50, 41, 32, 25, 18,
    13, 8, 5, 2, 1);
var
  lsMessage: string;
begin
  if piSpacesToEnd < 0 then
  begin
    lsMessage := 'Spaces to end is ' + IntToStr(piSpacesToEnd) +
      ' on ' + pcNext.Describe + ' at ' + pcNext.DescribePosition +
      ' line is ' + NativeLineBreak + pcNext.SourceLine;

    Raise Exception.Create(lsMessage);
  end;

  if piSpacesToEnd > TAIL_SIZE then
  begin
    Result := 0;
  end
  else
  begin
    Result := penalties[piSpacesToEnd];
  end;
end;

{ scoring - based on the current token,
  score how aestetically pleasing a line break after this token would be }
procedure ScoreToken(const pcToken: TSourceToken;
  out piScoreBefore, piScoreAfter: integer);
const
  BAD4 = -40;
  BAD3 = -30;
  BAD2 = -20;
  BAD1 = -10;

  HALF_GOOD = 5;

  GOOD1 = 10;
  GOOD2 = 20;
  GOOD3 = 30;
  GOOD4 = 40;
  GOOD5 = 50;

var
  lcPrev, lcNext: TSourceToken;
begin
  Assert(pcToken <> nil);
  piScoreBefore := 0;
  piScoreAfter  := 0;

  if pcToken.TokenType in Operators then
  begin
    { good to break after an operator (except unary operators)
    bad to break just before one }
    if IsUnaryOperator(pcToken) then
    begin
      { do not break between unary operator and operand }
      piScoreAfter := BAD4;
    end
    else
    begin
      if (pcToken.TokenType = ttEquals) and pcToken.HasParentNode(nConstDecl) then
      begin
        { '=' in a const def is like a ':='}
        piScoreAfter  := GOOD4;
        piScoreBefore := BAD2;
      end
      else
      begin
        piScoreAfter  := GOOD1;
        piScoreBefore := BAD2;
      end;
    end;
  end
  else
  begin
    // process everyone else

    case pcToken.TokenType of
      { bad to break just before or after a dot.
        However if you must pick, break after it  }
      ttDot:
      begin
        piScoreBefore := BAD2;
        piScoreAfter  := BAD1;
      end;
      { it is better to break after a colon than before }
      ttColon:
      begin
        piScoreBefore := BAD2;
        piScoreAfter  := HALF_GOOD;
      end;
      ttOpenBracket:
      begin
        { if this is a fn def or call, break after. }
        if IsActualParamOpenBracket(pcToken) or IsFormalParamOpenBracket(pcToken) then
        begin
          piScoreBefore := BAD2;
          piScoreAfter  := GOOD1;
        end
        { If it not a fn call but is in an expr then break before }
        else if pcToken.HasParentNode(nExpression) then
        begin
          piScoreBefore := GOOD1;
          piScoreAfter  := BAD1;
        end
        else
        begin
          // class defs and stuph Break after
          piScoreBefore := BAD2;
          piScoreAfter  := GOOD1;
        end;

      end;
      { or just before close brackets -
        better to break after these }
      ttCloseBracket, ttCloseSquareBracket:
      begin
        piScoreBefore := BAD2;
        piScoreAfter  := GOOD1;

        if pcToken.HasParentNode(nExpression) then
        begin
          lcNext := pcToken.NextSolidToken;
          if lcNext <> nil then
          begin
            if lcNext.TokenType in Operators then
            begin
               { operator next? want to break after it instead of after the ')'
                (for e.g after the + in 'a := (x - y) + z; }
              piScoreAfter := BAD1;
            end
            else if (lcNext.TokenType in [ttCloseBracket, ttCloseSquareBracket]) then
            begin
              { more close brackets coming? break after them instead }
              piScoreAfter := BAD2;
            end
            else
            begin
              { no operator next, no bracket next.
                Is this the last of multiple brackets ? }
              lcPrev := pcToken.PriorSolidToken;
              if (lcPrev.TokenType in [ttCloseBracket, ttCloseSquareBracket]) then
                piScoreAfter := GOOD3;
            end;
          end;
        end;
      end;
      { break after the semicolon is awesome, before is terrible}
      ttSemiColon:
      begin
        if EndsFunctionReturnType(pcToken) then
        begin
          // if this semicolon ends the function return type, even better
          piScoreBefore := BAD3;
          piScoreAfter  := GOOD5;
        end
        else
        begin
          piScoreBefore := BAD2;
          piScoreAfter  := GOOD4;
        end;
      end;
      { It is good to break after := not before }
      ttAssign, ttPlusAssign, ttMinusAssign, ttTimesAssign, ttFloatDivAssign:
      begin
        piScoreBefore := BAD2;
        piScoreAfter  := GOOD3;
      end;
      ttComma:
      begin
        { just not on to break before comma
          breaking after comma is better, but not great
         Have found aplace where in formal params, breaking after commas
         is not as good value as in fn call actual params }
        piScoreBefore := Bad4;
        if pcToken.HasParentNode(nFormalParams) then
          piScoreAfter := HALF_GOOD
        else
          piScoreAfter := GOOD1;

      end;
      { break before white Space, not after }
      ttWhiteSpace:
      begin
        piScoreBefore := GOOD1;
        piScoreAfter  := BAD1;
      end;

      { words }
      { good to break after if <exp> then, not before
       likewise case <exp> of and while <exp> dp }
      ttThen, ttOf, ttDo:
      begin
        piScoreBefore := BAD2;
        piScoreAfter  := GOOD4;
      end;
      { in the unlikely event that one of these is embedded in a long line }
      ttBegin, ttEnd:
      begin
        // good to break before, even better to break after
        piScoreBefore := GOOD1;
        piScoreAfter  := GOOD4;
      end;
      ttConst:
      begin
        piScoreBefore := BAD1;
        piScoreAfter  := GOOD1;
      end;
    end;
  end;

  { rules for properties }
  if pcToken.HasParentNode(nProperty) then
  begin
      { in a property def, good to break before 'read', Ok to break before 'Write'
        bad to break just after then }
    case pcToken.TokenType of
      ttRead:
      begin
        if pcToken.HasParentNode(nProperty) then
        begin
          piScoreBefore := GOOD4;
          piScoreAfter  := BAD2;
        end;
      end;
      ttWrite, ttImplements:
      begin
        if pcToken.HasParentNode(nProperty) then
        begin
          piScoreBefore := GOOD3;
          piScoreAfter  := BAD2;
        end;
      end;
      ttExternal:
      begin
        // the function directive external is followed by text
        if pcToken.HasParentNode(nExternalDirective) then
        begin
          piScoreBefore := HALF_GOOD;
          piScoreAfter  := BAD1;
        end;
      end;
      ttDefault:
      begin
        { default attr in a property is a bad thing to break before }
        if pcToken.HasParentNode(nProperty) then
        begin
          piScoreBefore := BAD1;
          piScoreAfter  := GOOD1;
        end;

      end;
    end;

    if pcToken.HasParentNode(nPropertyParameterList) and
      (pcToken.TokenType = ttConst) then
    begin
      { breaking before const not after }
      piScoreBefore := GOOD3;
      piScoreAfter  := BAD2;
    end;
  end;

  { slightly different rules for procedure params }
  if InFormalParams(pcToken) then
  begin
    case pcToken.TokenType of
      ttArray, ttOf:
      begin
        piScoreBefore := BAD1;
        piScoreAfter  := BAD1;
      end;
      ttConst, ttVar, ttOut:
      begin
          { breaking in these would be breaking inside a parameter}
        piScoreBefore := GOOD3;
        piScoreAfter  := BAD2;
      end;
    end;
  end
end;


{ TLongLineBreaker }

constructor TLongLineBreaker.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eLineBreaking];
  fcScores    := TIntList.Create;
  fcTokens    := TSourceTokenList.Create;
end;

destructor TLongLineBreaker.Destroy;
begin
  FreeAndNil(fcScores);
  FreeAndNil(fcTokens);
  inherited;
end;

procedure TLongLineBreaker.FixPos;
var
  liLoop, liPos: integer;
  lt: TSourceToken;
  lbStarted: boolean;
begin
  liPos     := 1; // XPos is indexed from 1
  lbStarted := False;

  // look through the token list and reset X pos of all tokens after the inserted returns
  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lt := fcTokens.SourceTokens[liLoop];

    if lbStarted then
      lt.XPosition := liPos;

    case lt.TokenType of
      ttReturn:
      begin
        liPos     := 1;
        lbStarted := True;
      end;
      ttComment:
      begin
        if Pos(WideString(NativeLineBreak), lt.SourceCode) <= 0 then
          liPos := liPos + Length(lt.SourceCode)
        else
          liPos := LastLineLength(lt.SourceCode);
      end
      else
        liPos := liPos + Length(lt.SourceCode);
    end;
  end;
end;


function TLongLineBreaker.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
const
  GOOD_BREAK_THRESHHOLD = 50;
  ANY_BREAK_THRESHHOLD  = -10;
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
  liInitWidth, liTotalWidth, liTempWidth: integer;
  liIndexOfFirstSolidToken: integer;
  liLoop: integer;
  liScoreBefore, liScoreAfter: integer;
  liPlaceToBreak: integer;
  lcBreakToken, lcNewToken: TSourceToken;
begin
  Result := False;

  lcSourceToken := TSourceToken(pcNode);

  // don't break lines in dpr program uses clause
  if lcSourceToken.HasParentNode(nUses) and lcSourceToken.HasParentNode(nProgram) then
    exit;

  { line can start with a return or with a multiline comment }
  if not IsLineBreaker(lcSourceToken) or (lcSourceToken.TokenType = ttConditionalCompilationRemoved) then
    exit;

  // read until the next return
  lcNext := lcSourceToken.NextToken;
  liIndexOfFirstSolidToken := -1;

  if lcSourceToken.TokenType = ttReturn then
    liInitWidth := 0
  else
    liInitWidth := LastLineLength(lcSourceToken.SourceCode);

  liTotalWidth := liInitWidth;

  fcTokens.Clear;

  while (lcNext <> nil) and ( not IsLineBreaker(lcNext)) do
  begin
    fcTokens.Add(lcNext);

    { record which token starts the line's solid text - don't want to break before it }
    if (lcNext.TokenType <> ttWhiteSpace) and (liIndexOfFirstSolidToken = -1) then
      liIndexOfFirstSolidToken := fcTokens.Count - 1;

    liTotalWidth := liTotalWidth + Length(lcNext.SourceCode);

    lcNext := lcNext.NextToken;
  end;

  // check for trailing conditional compilation removed
  while (lcNext <> nil) and (not IsLineBreaker(lcNext) or (lcNext.TokenType = ttConditionalCompilationRemoved)) do
  begin
    liTotalWidth := liTotalWidth + Length(lcNext.SourceCode);
    lcNext := lcNext.NextToken;
  end;


  // EOF or blank line means no linebreaking to do 
  if (lcNext = nil) or (lcNext = lcSourceToken) then
    exit;

  { must be solid stuff on the line }
  if liIndexOfFirstSolidToken < 0 then
    exit;

  { if the line does not run on, exit now }
  if liTotalWidth < FormatSettings.Returns.MaxLineLength then
    exit;

  { right, the line is too long.
    Score each token to find the best place to break
    This is a bunch of heuristics to produce a reasonably aesthetic result
    The obvious heuristics are:
     - it is better to break near the specified max line length (and bad to break near the line start)
     - it is better to break outside of brackets
     - it is good to break after operators like '+' 'or' ',' (and bad to break before them)
  }

  { -------------
    scoring }

  { Set up scores - first the basics just for position on the line }
  fcScores.Clear;

  (* better coded this way
  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcNext := fcTokens.SourceTokens[liLoop];
    liWidth := liWidth + Length(lcNext.SourceCode);

    { thse scores are simply property of one token }
    liScoreAfter := PositionScore(liLoop, liIndexOfFirstSolidToken, liWidth) +
      TreeScore(lcNext) + BracketScore(lcNext);
    fcScores.Add(liScoreAfter);
  end;  *)

  // easier to debug *this* way
  liTempWidth := liInitWidth;
  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcNext      := fcTokens.SourceTokens[liLoop];
    liTempWidth := liTempWidth + Length(lcNext.SourceCode);

    { these scores are simply property of one token }
    liScoreAfter := PositionScore(liLoop, liIndexOfFirstSolidToken, liTempWidth);
    fcScores.Add(liScoreAfter);
  end;

  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcNext := fcTokens.SourceTokens[liLoop];

    { !!! debug temp
    if liTotalWidth - (lcNext.XPosition - 1) < 0 then
    begin
      Self := Self;
    end;
    }

    // xpos is indexed from 1
    liScoreAfter := NearEndScore(liTotalWidth - (lcNext.XPosition - 2), lcNext);
    // subtract this one - bad to break near end
    fcScores.Items[liLoop] := fcScores.Items[liLoop] - liScoreAfter;
  end;

  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcNext := fcTokens.SourceTokens[liLoop];
    liScoreAfter := BracketScore(lcNext);
    fcScores.Items[liLoop] := fcScores.Items[liLoop] + liScoreAfter;
  end;


  { modify the weights based on the particular source code.
    This is what will make it work -
   it is better to break line at some syntax than at other }
  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcNext := fcTokens.SourceTokens[liLoop];

    ScoreToken(lcNext, liScoreBefore, liScoreAfter);

    if liLoop > 0 then
      fcScores.Items[liLoop - 1] := fcScores.Items[liLoop - 1] + liScoreBefore;
    fcScores.Items[liLoop] := fcScores.Items[liLoop] + liScoreAfter;
  end;

  { Where shall we break, if anywhere? }
  liPlaceToBreak := fcScores.IndexOfMax;

  { ignore the error conditions
   - is the break place before the first non-space token? }
  if (liPlaceToBreak < liIndexOfFirstSolidToken) then
    exit;
  { - is it at the end of the line already, just before the existing return?}
  if (liPlaceToBreak >= (fcTokens.Count - 1)) then
    exit;

  { best breakpoint is not good enough? }
  if FormatSettings.Returns.RebreakLines = rbOnlyIfGood then
  begin
    if fcScores.Items[liPlaceToBreak] < GOOD_BREAK_THRESHHOLD then
      exit;
  end
  else
  begin
    Assert(FormatSettings.Returns.RebreakLines = rbUsually,
      'bad rebreak setting of ' + IntToStr(Ord(FormatSettings.Returns.RebreakLines)));
    if fcScores.Items[liPlaceToBreak] < ANY_BREAK_THRESHHOLD then
      exit;
  end;


  { check if the program has made a bad decision,
    e.g. the only thing on the line is a *really* long string constant and it's semicolon
    The program must break because the line is too long,
    so only place it can break lines is before the semicolon }
  lcBreakToken := fcTokens.SourceTokens[liPlaceToBreak];

  { go break! }
  lcNewToken := InsertReturnAfter(lcBreakToken);
  fcTokens.Insert(liPlaceToBreak + 1, lcNewToken);

  { the tokens in the buffer past liPlaceToBreak now have the wrong Xpos }
  FixPos;
end;

function TLongLineBreaker.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Returns.RebreakLines <> rbOff;
end;

(*
  //TEST code used to get a graph of the position scoring function into excel

var
  liLoop, liScore: integer;
  lsData: string;
const
  INDEX_OF_FIRST = 10;
  MAX_TOKENS = 100;
initialization
  Settings.Returns.MaxLineLength  := MAX_TOKENS;

  { debug temp to graph the pos fn }
  for liLoop := 0 to (MAX_TOKENS * 3 div  2) do
  begin
    liScore := PositionScore(liLoop, INDEX_OF_FIRST, liLoop);

    lsData := lsData + IntToStr(liScore) + ', ';
  end;

  StringToFile('C:\temp\posvalues.txt', lsData)

*)


end.
