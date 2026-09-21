{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains a CSS parser

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpCSSParser;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}
{$IF FPC_FULLVERSION>30300}
{$WARN 6060 off} // Case statement does not handle all possible cases
{$WARN 6058 off} // Call to subroutine "$1" marked as inline is not inlined
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.TypInfo, System.Classes, System.SysUtils, FpCss.Tree, FpCss.Scanner;
{$ELSE FPC_DOTTEDUNITS}
uses
  TypInfo, Classes, SysUtils, fpCSSTree, fpCSSScanner;
{$ENDIF FPC_DOTTEDUNITS}

Type	
  ECSSParser = Class(ECSSException);

  // Complete parser position, see TCSSParser.SaveState.
  TCSSParserState = record
    ScannerState: TCSSScannerState;
    Previous, Current: TCSSToken;
    CurrentTokenString: TCSSString;
    PeekToken: TCSSToken;
    PeekTokenString: TCSSString;
    CurrentTokenRow, CurrentTokenCol, CurrentTokenPos: Integer;
    PeekTokenRow, PeekTokenCol, PeekTokenPos: Integer;
    CurrentTokenEndRow, CurrentTokenEndCol: Integer;
    PeekTokenEndRow, PeekTokenEndCol: Integer;
    CurSigTokenEndRow, CurSigTokenEndCol: Integer;
    PrevSigTokenEndRow, PrevSigTokenEndCol: Integer;
  end;

  { TCSSParser }

  TCSSParser = class(TObject)
  private
    FInput : TStream;
    FScanner: TCSSScanner;
    FPrevious : TCSSToken;
    FCurrent : TCSSToken;
    FCurrentTokenString : TCSSString;
    FPeekToken : TCSSToken;
    FPeekTokenString : TCSSString;
    // start location (row,col of the first character, 0-based stream position) of tokens.
    // Note: the scanner only knows the last fetched token, which is the peeked
    // one while a peek is pending, so the parser has to remember these.
    FCurrentTokenRow, FCurrentTokenCol, FCurrentTokenPos : Integer; // current token
    FPeekTokenRow, FPeekTokenCol, FPeekTokenPos : Integer;          // peeked token
    // end location (row,col right after the last character) of tokens.
    FCurrentTokenEndRow, FCurrentTokenEndCol : Integer; // current token
    FPeekTokenEndRow, FPeekTokenEndCol : Integer;       // peeked token
    // end of the current/previous significant token (skipping whitespace and comments),
    // used to determine where a value ends without counting trailing whitespace.
    FCurSigTokenEndRow, FCurSigTokenEndCol : Integer;
    FPrevSigTokenEndRow, FPrevSigTokenEndCol : Integer;
    FFreeScanner : Boolean;
    FRuleLevel : Integer;
    FInvalidDeclarationValue : Boolean;
    function GetAtEOF: Boolean;
    function GetCurSource: TCSSString;
    Function GetCurLine : Integer;
    Function GetCurPos : Integer;
  protected
    function CreateElement(aClass: TCSSElementClass): TCSSElement; virtual;
    class function GetAppendElement(aList: TCSSListElement): TCSSElement;
    Procedure DoWarn(const Msg : TCSSString); virtual;
    Procedure DoWarn(const Fmt : TCSSString; const Args : Array of const);
    Procedure DoWarnExpectedButGot(const Expected: string);
    Procedure DoError(const Msg : TCSSString); virtual;
    Procedure DoError(const Fmt : TCSSString; const Args : Array of const);
    Procedure DoErrorExpectedButGot(const Expected: string);
    Procedure Consume(aToken : TCSSToken); virtual;
    // Close a block: an unclosed block at end of input is auto closed, as required by the CSS syntax spec.
    Procedure ConsumeRBrace;
    Procedure SkipWhiteSpace;
    Procedure SkipRule;
    function ParseComponentValueList(AllowRules: Boolean=True): TCSSElement; virtual;
    function ParseComponentValue: TCSSElement; virtual;
    function ParseExpression: TCSSElement; virtual;
    function ParseRule: TCSSRuleElement; virtual;
    function ParseAtRulePrelude: TCSSAtRuleElement; virtual;
    function ParseAtUnknownRule(aSkipDeclarations : Boolean = False): TCSSElement; virtual;
    function ParseAtNestedRule: TCSSAtRuleElement; virtual;
    function ParseAtMediaRulePrelude: TCSSAtRuleElement; virtual;
    function ParseAtMediaRule: TCSSAtRuleElement; virtual;
    function ParseAtSimpleRule(aSkipDeclarations : Boolean = False): TCSSAtRuleElement; virtual;
    function IsAtKeyframesKeyword(const aKeyword : TCSSString): Boolean; virtual;
    function ParseAtKeyframesRule: TCSSAtRuleElement; virtual;
    function ParseKeyframeSelector: TCSSElement; virtual;
    function ParseMediaCondition(TopLvl: boolean): TCSSElement; virtual;
    function ParseMediaBracket: TCSSElement; virtual;
    function ParseRuleList(aStopOn : TCSStoken = ctkEOF;
                           aSkipDeclarations : Boolean = False): TCSSElement; virtual;
    function ParseSelector: TCSSElement; virtual;
    function ParseAttributeSelector: TCSSElement; virtual;
    function ParseWQName: TCSSElement;
    function ParseDeclaration(aIsAt : Boolean = false): TCSSDeclarationElement; virtual;
    function ParseCall(aName: TCSSString; IsSelector: boolean): TCSSCallElement; virtual;
    procedure ParseSelectorCommaList(aCall: TCSSCallElement); virtual;
    procedure ParseRelationalSelectorCommaList(aCall: TCSSCallElement); virtual;
    procedure ParseNthChildParams(aCall: TCSSCallElement); virtual;
    function ParseUnary: TCSSElement; virtual;
    function ParseUnit: TCSSUnit; virtual;
    function ParseIdentifier : TCSSIdentifierElement; virtual;
    function ParseHashIdentifier : TCSSHashIdentifierElement; virtual;
    function ParseClassName : TCSSClassNameElement; virtual;
    function ParseParenthesis: TCSSElement; virtual;
    function ParsePseudoClass: TCSSElement; virtual;
    function ParsePseudoElement: TCSSElement; virtual;
    function ParseRuleBody(aRule: TCSSRuleElement; aIsAt : Boolean = False;
                           aSkipDeclarations : Boolean = False) : integer; virtual;
    function CurrentStartsNestedRule : Boolean; virtual;
    function CurrentStartsDeclaration : Boolean; virtual;
    procedure SkipInvalidDeclaration(aIsAt : Boolean = False); virtual;
    function LookAheadIsNestedRule : Boolean;
    function ParseInteger: TCSSElement; virtual;
    function ParseFloat: TCSSElement; virtual;
    function ParseString: TCSSElement; virtual;
    function ParseHashValue: TCSSElement; virtual;
    Function ParseUnicodeRange : TCSSElement; virtual;
    function ParseArray(aPrefix: TCSSElement; AllowRules: boolean): TCSSElement; virtual;
    function ParseURL: TCSSElement; virtual;
    function ParseInvalidToken: TCSSElement; virtual;
    Property CurrentSource : TCSSString Read GetCurSource;
    Property CurrentLine : Integer Read GetCurLine;
    Property CurrentPos : Integer Read GetCurPos;
  Public
    CSSArrayElementClass: TCSSArrayElementClass;
    CSSAtRuleElementClass: TCSSAtRuleElementClass;
    CSSBinaryElementClass: TCSSBinaryElementClass;
    CSSCallElementClass: TCSSCallElementClass;
    CSSClassNameElementClass: TCSSClassNameElementClass;
    CSSCompoundElementClass: TCSSCompoundElementClass;
    CSSDeclarationElementClass: TCSSDeclarationElementClass;
    CSSFloatElementClass: TCSSFloatElementClass;
    CSSHashIdentifierElementClass: TCSSHashIdentifierElementClass;
    CSSIdentifierElementClass: TCSSIdentifierElementClass;
    CSSIntegerElementClass: TCSSIntegerElementClass;
    CSSListElementClass: TCSSListElementClass;
    CSSParenthesisElementClass: TCSSParenthesisElementClass;
    CSSPseudoClassElementClass: TCSSPseudoClassElementClass;
    CSSRuleElementClass: TCSSRuleElementClass;
    CSSStringElementClass: TCSSStringElementClass;
    CSSHashValueElementClass: TCSSHashValueElementClass;
    CSSUnaryElementClass: TCSSUnaryElementClass;
    CSSUnicodeRangeElementClass: TCSSUnicodeRangeElementClass;
    CSSURLElementClass: TCSSURLElementClass;
    Constructor Create(AInput: TStream; ExtraScannerOptions : TCSSScannerOptions = []); overload; // AInput is not freed
    Constructor Create(AScanner : TCSSScanner); virtual; overload;
    Destructor Destroy; override;
    Function Parse : TCSSElement;
    Function ParseInline : TCSSElement;
    Property CurrentToken : TCSSToken Read FCurrent;
    Property CurrentTokenString : TCSSString Read FCurrentTokenString;
    Property PreviousToken : TCSSToken Read FPrevious;
    Function GetNextToken : TCSSToken;
    Function PeekNextToken : TCSSToken;
    // Speculative scanning: SaveState remembers the current position,
    // RestoreState rewinds to it, DropState commits.
    function SaveState : TCSSParserState;
    procedure RestoreState(const aState : TCSSParserState);
    procedure DropState;
    Property Scanner : TCSSScanner Read FScanner;
    Property atEOF : Boolean Read GetAtEOF;
  end;

Function TokenToBinaryOperation(aToken : TCSSToken) : TCSSBinaryOperation;
Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;
Function IsValidCSSAttributeName(const aName: TCSSString): boolean;
Function CSSIsKeyframesAtKeyword(const aKeyword: TCSSString): Boolean;

implementation

Resourcestring
  SBinaryInvalidToken = 'Invalid token for binary operation: %s';
  SUnaryInvalidToken = 'Invalid token for unary operation: %s';
  SErrFileSource = 'Error: file "%s" line %d, pos %d: ';
  SErrSource = 'Error: line %d, pos %d: ';
  SErrUnexpectedToken = 'Unexpected token: Got %s (as string: "%s"), expected: %s ';
  SErrInvalidFloat = 'Invalid float: %s';
  SErrUnexpectedEndOfFile = 'Unexpected EOF while scanning function args: %s';

Function TokenToBinaryOperation(aToken : TCSSToken) : TCSSBinaryOperation;

begin
  Case aToken of
    ctkEquals : Result:=boEquals;
    ctkPlus : Result:=boPlus;
    ctkMinus:  Result:=boMinus;
    ctkAnd : result:=boAnd;
    ctkGE : Result:=boGE;
    ctkGT : Result:=boGT;
    ctkLE : Result:=boLE;
    ctkLT : Result:=boLT;
    ctkDIV : Result:=boDIV;
    ctkStar : Result:=boStar;
    ctkSTAREQUAL : Result:=boStarEqual;
    ctkTilde : Result:=boTilde;
    ctkTILDEEQUAL : Result:=boTildeEqual;
    ctkSquared : Result:=boSquared;
    ctkSQUAREDEQUAL : Result:=boSquaredEqual;
    ctkPIPE : Result:=boPipe;
    ctkPIPEEQUAL : Result:=boPipeEqual;
    ctkDOLLAR : Result:=boDollar;
    ctkDOLLAREQUAL : Result:=boDollarEqual;
    ctkColon : Result:=boCOLON;
    ctkDoubleColon : Result:=boDoubleColon;
  else
    Raise ECSSParser.CreateFmt(SBinaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
    // Result:=boEquals;
  end;
end;

Function TokenToUnaryOperation(aToken : TCSSToken) : TCSSUnaryOperation;

begin
  Case aToken of
    ctkDOUBLECOLON: Result:=uoDoubleColon;
    ctkMinus: Result:=uoMinus;
    ctkPlus: Result:=uoPlus;
    ctkDiv: Result:=uoDiv;
    ctkGT: Result:=uoGT;
    ctkTILDE: Result:=uoTilde;
  else
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[GetEnumName(TypeInfo(aToken),Ord(aToken))]);
  end;
end;

function IsValidCSSAttributeName(const aName: TCSSString): boolean;
var
  p, StartP: PCSSChar;
begin
  if aName='' then exit(false);
  StartP:=PCSSChar(aName);
  p:=StartP;
  if p^='-' then
  begin
    inc(p);
    if p^='-' then
      inc(p);
    if not (p^ in ['A'..'Z','a'..'z']) then
      exit;
    inc(p);
  end;
  while p^ in ['A'..'Z','a'..'z','_','-'] do inc(p);
  Result:=p=StartP+length(aName);
end;

function CSSIsKeyframesAtKeyword(const aKeyword: TCSSString): Boolean;
// true for '@keyframes' and its vendor prefixed forms, e.g. '@-webkit-keyframes'

var
  s : TCSSString;
  p : Integer;

begin
  Result:=false;
  s:=lowercase(aKeyword);
  if (s='') or (s[1]<>'@') then exit;
  Delete(s,1,1);
  if (s<>'') and (s[1]='-') then
    begin
    // skip the vendor prefix, e.g. '-webkit-'
    p:=Pos('-',s,2);
    if p<1 then exit;
    Delete(s,1,p);
    end;
  Result:=s='keyframes';
end;

{ TCSSParser }

function TCSSParser.GetAtEOF: Boolean;
begin
  Result:=(CurrentToken=ctkEOF);
end;

procedure TCSSParser.DoError(const Msg: TCSSString);
Var
  ErrAt : TCSSString;

begin
  If Assigned(FScanner) then
    If FScanner.CurFilename<>'' then
      ErrAt:=SafeFormat(SErrFileSource,[FScanner.CurFileName,FScanner.CurRow,FScanner.CurColumn])
    else
      ErrAt:=SafeFormat(SErrSource,[FScanner.Currow,FScanner.CurColumn]);
  Raise ECSSParser.Create(ErrAt+Msg)
end;

procedure TCSSParser.DoError(const Fmt: TCSSString; const Args: array of const);
begin
  DoError(SafeFormat(Fmt,Args));
end;

procedure TCSSParser.DoErrorExpectedButGot(const Expected: string);
begin
  DoError(SErrUnexpectedToken ,[
           GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
           CurrentTokenString,
           Expected
           ]);
end;

procedure TCSSParser.Consume(aToken: TCSSToken);
begin
  if CurrentToken<>aToken then
    DoError(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             GetEnumName(TypeInfo(TCSSToken),Ord(aToken))
             ]);
  GetNextToken;
end;

procedure TCSSParser.ConsumeRBrace;
begin
  if CurrentToken=ctkRBRACE then
    GetNextToken
  else if CurrentToken<>ctkEOF then
    DoWarnExpectedButGot('}');
end;

procedure TCSSParser.SkipWhiteSpace;
begin
  while CurrentToken=ctkWHITESPACE do
    GetNextToken;
end;

procedure TCSSParser.SkipRule;
var
  Lvl: Integer;
begin
{$ifdef VerboseCSSParser}
  Writeln('Skip rule');
{$endif}
  // skip selectors
  While Not (CurrentToken in [ctkEOF,ctkLBRACE,ctkSEMICOLON]) do
    GetNextToken;
  if (CurrentToken=ctkLBRACE) then
    begin
    // skip the block, including nested blocks
    Lvl:=1;
    repeat
      GetNextToken;
      case CurrentToken of
      ctkEOF:
        break;
      ctkLBRACE:
        inc(Lvl);
      ctkRBRACE:
        begin
        dec(Lvl);
        if Lvl=0 then
          begin
          GetNextToken;
          break;
          end;
        end;
      end;
    until false;
    end;
{$ifdef VerboseCSSParser}  Writeln('Done Skip rule '); {$endif}
end;

function TCSSParser.GetCurSource: TCSSString;
begin
  If Assigned(FScanner) then
    Result:=FScanner.CurFileName
  else
    Result:='';
end;

function TCSSParser.GetCurLine: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurRow
  else
    Result:=0;
end;

function TCSSParser.GetCurPos: Integer;
begin
  if Assigned(FScanner) then
    Result:=FScanner.CurColumn
  else
    Result:=0;
end;

procedure TCSSParser.DoWarn(const Msg: TCSSString);
begin
  if Assigned(Scanner.OnWarn) then
    begin
    if Scanner.OnWarn(Self,Msg,Scanner.CurRow,Scanner.CurColumn) then
      exit;
    end;
  DoError(Msg);
end;

procedure TCSSParser.DoWarn(const Fmt: TCSSString; const Args: array of const);
begin
  DoWarn(SafeFormat(Fmt,Args));
end;

procedure TCSSParser.DoWarnExpectedButGot(const Expected: string);
begin
  DoWarn(SErrUnexpectedToken ,[
           GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
           CurrentTokenString,
           Expected
           ]);
end;

constructor TCSSParser.Create(AInput: TStream; ExtraScannerOptions : TCSSScannerOptions = []);
begin
  FInput:=AInput;
  Create(TCSSScanner.Create(FInput));
  FScanner.Options:=FScanner.Options+ExtraScannerOptions;
  FFreeScanner:=True;
end;

constructor TCSSParser.Create(AScanner: TCSSScanner);
begin
  FCurrent:=ctkUNKNOWN;
  FPeekToken:=ctkUNKNOWN;
  FPeekTokenString:='';
  FScanner:=aScanner;
  CSSArrayElementClass:=TCSSArrayElement;
  CSSAtRuleElementClass:=TCSSAtRuleElement;
  CSSBinaryElementClass:=TCSSBinaryElement;
  CSSCallElementClass:=TCSSCallElement;
  CSSClassNameElementClass:=TCSSClassNameElement;
  CSSCompoundElementClass:=TCSSCompoundElement;
  CSSDeclarationElementClass:=TCSSDeclarationElement;
  CSSFloatElementClass:=TCSSFloatElement;
  CSSHashIdentifierElementClass:=TCSSHashIdentifierElement;
  CSSIdentifierElementClass:=TCSSIdentifierElement;
  CSSIntegerElementClass:=TCSSIntegerElement;
  CSSListElementClass:=TCSSListElement;
  CSSParenthesisElementClass:=TCSSParenthesisElement;
  CSSPseudoClassElementClass:=TCSSPseudoClassElement;
  CSSRuleElementClass:=TCSSRuleElement;
  CSSStringElementClass:=TCSSStringElement;
  CSSHashValueElementClass:=TCSSHashValueElement;
  CSSUnaryElementClass:=TCSSUnaryElement;
  CSSUnicodeRangeElementClass:=TCSSUnicodeRangeElement;
  CSSURLElementClass:=TCSSURLElement;
end;

destructor TCSSParser.Destroy;
begin
  if FFreeScanner then
    FreeAndNil(FScanner);
  inherited Destroy;
end;

class function TCSSParser.GetAppendElement(aList: TCSSListElement): TCSSElement;

begin
  Case aList.ChildCount of
    0 : Result:=Nil;
    1 : Result:=aList.ExtractElement(0);
  else
    Result:=aList;
  end;
  if Result<>aList then
    aList.Free;
end;

function TCSSParser.ParseAtRulePrelude: TCSSAtRuleElement;
// read the at-keyword and the selectors/condition up to the '{', ';' or EOF

Var
  aSel : TCSSElement;
  Term : TCSSTokens;
  aList : TCSSListElement;

begin
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  Result:=TCSSAtRuleElement(CreateElement(CSSAtRuleElementClass));
  Result.AtKeyWord:=CurrentTokenString;
  GetNextToken;
  aList:=nil;
  try
    aList:=TCSSListElement(CreateElement(CSSListElementClass));
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseComponentValue;
      aList.AddChild(aSel);
      if CurrentToken=ctkCOMMA then
        begin
        GetNextToken;
        Result.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(CSSListElementClass));
        end;
      end;
    Result.AddSelector(GetAppendElement(aList));
    aList:=nil;
  finally
    aList.Free;
  end;
end;

function TCSSParser.ParseAtUnknownRule(aSkipDeclarations : Boolean): TCSSElement;
// read unknown at-rule, its block contains only rules

Var
  aRule : TCSSAtRuleElement;
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}

begin
  Result:=nil;
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @ rule');
{$endif}
  aRule:=ParseAtRulePrelude;
  try
    if (CurrentToken=ctkLBRACE) then
      begin
      GetNextToken;
      aRule.AddChild(ParseRuleList(ctkRBRACE,aSkipDeclarations));
      ConsumeRBrace;
      end;
    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseAtNestedRule: TCSSAtRuleElement;
// read an at-rule nested inside a style rule, e.g.
//   .foo { @supports (display:grid) { display:grid; } }
// Its block can contain declarations as well as nested rules.

Var
  aRule : TCSSAtRuleElement;
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}

begin
  Result:=nil;
  if IsAtKeyframesKeyword(CurrentTokenString) then
    // a nested @keyframes is read like a top level one, e.g.
    //   div { @keyframes fade { from{ opacity:0; } to{ opacity:1; } } }
    // Note: ParseAtKeyframesRule does its own FRuleLevel bookkeeping.
    Exit(ParseAtKeyframesRule);
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse nested @ rule');
{$endif}
  if lowercase(CurrentTokenString)='@media' then
    // a nested @media has the same conditions as a top level @media
    aRule:=ParseAtMediaRulePrelude
  else
    aRule:=ParseAtRulePrelude;
  try
    if (CurrentToken=ctkLBRACE) then
      begin
      GetNextToken;
      ParseRuleBody(aRule);
      ConsumeRBrace;
      end;
    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse nested @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseAtMediaRulePrelude: TCSSAtRuleElement;
// read the @media keyword and the media conditions up to the '{', ';' or EOF

Var
  Term : TCSSTokens;
  aToken: TCSSToken;
  aList : TCSSListElement;
  OldOptions: TCSSScannerOptions;
begin
  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  Result:=TCSSAtRuleElement(CreateElement(CSSAtRuleElementClass));
  Result.AtKeyWord:=CurrentTokenString;
  aList:=nil;
  OldOptions:=Scanner.Options;
  try
    // A media query has no pseudo classes, so that 'max-width:100px' gives a
    // ctkCOLON instead of a ctkPSEUDO ':100px'. Whitespace around the ':' of a
    // media feature is optional.
    Scanner.DisablePseudo:=True;
    GetNextToken;
    aList:=TCSSListElement(CreateElement(CSSListElementClass));
    While Not (CurrentToken in Term) do
      begin
      aToken:=CurrentToken;
      //  writeln('TCSSParser.ParseAtMediaRulePrelude Token=',CurrentToken);
      case aToken of
      ctkIDENTIFIER:
        aList.AddChild(ParseMediaCondition(true));
      ctkLPARENTHESIS:
        aList.AddChild(ParseMediaBracket);
      else
        DoWarnExpectedButGot('identifier');
        SkipRule;
      end;
      if CurrentToken=ctkCOMMA then
        begin
        GetNextToken;
        Result.AddSelector(GetAppendElement(aList));
        aList:=TCSSListElement(CreateElement(CSSListElementClass));
        end;
      end;
    Result.AddSelector(GetAppendElement(aList));
    aList:=nil;
  finally
    Scanner.Options:=OldOptions;
    aList.Free;
  end;
end;

function TCSSParser.ParseAtMediaRule: TCSSAtRuleElement;

Var
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}
  aRule : TCSSAtRuleElement;
  Term : TCSSTokens;
  El: TCSSElement;
begin
  Result:=nil;
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @media rule');
{$endif}
  aRule:=ParseAtMediaRulePrelude;
  try
    if (CurrentToken=ctkLBRACE) then
      begin
      GetNextToken;
      Term:=[ctkEOF,ctkRBRACE];
      While not (CurrentToken in Term) do
        begin
        if CurrentStartsDeclaration then
          // a declaration directly in a @media block: only rules allowed
          SkipInvalidDeclaration
        else
          begin
          El:=ParseExpression;
          if El is TCSSRuleElement then
            aRule.AddNestedRule(TCSSRuleElement(El))
          else
            aRule.AddChild(ParseExpression);
          end;
        if CurrentToken=ctkSEMICOLON then
          GetNextToken;
        end;
      ConsumeRBrace;
      end;
    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseAtSimpleRule(aSkipDeclarations : Boolean): TCSSAtRuleElement;
var
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}
  aRule: TCSSAtRuleElement;
begin
  Result:=nil;
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @font-face rule');
{$endif}
  aRule:=TCSSAtRuleElement(CreateElement(CSSAtRuleElementClass));
  try
    aRule.AtKeyWord:=CurrentTokenString;
    GetNextToken;

    // read {
    repeat
      case CurrentToken of
      ctkEOF:
        begin
          DoWarnExpectedButGot('{');
          exit;
        end;
      ctkRBRACE, ctkLPARENTHESIS, ctkRPARENTHESIS, ctkLBRACKET,ctkRBRACKET, ctkSEMICOLON:
        begin
        DoWarnExpectedButGot('{');
        Result:=aRule;
        aRule:=nil;
        exit;
        end;
      ctkLBRACE:
        break;
      end;
    until false;
    GetNextToken;

    // read declarations
    ParseRuleBody(aRule,false,aSkipDeclarations);
    if CurrentToken=ctkRBRACE then
      GetNextToken;

    Result:=aRule;
    aRule:=nil;
    {$ifdef VerboseCSSParser}  Writeln('Done Parse @ rule ',aAt); {$endif}
    Inc(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.IsAtKeyframesKeyword(const aKeyword: TCSSString): Boolean;
// true for '@keyframes' and its vendor prefixed forms, e.g. '@-webkit-keyframes'

begin
  Result:=CSSIsKeyframesAtKeyword(aKeyword);
end;

function TCSSParser.ParseAtKeyframesRule: TCSSAtRuleElement;
// read '@keyframes name { <keyframe selectors> { declarations } ... }', e.g.
//   @keyframes fade { from { opacity: 0; } 50%, 75% { opacity: 0.5; } to { opacity: 1; } }
// The name becomes the selector of the at-rule, each keyframe becomes a nested rule.

Var
  {$ifdef VerboseCSSParser}
  aAt : TCSSString;
  {$endif}
  aRule : TCSSAtRuleElement;
  aKeyframe : TCSSRuleElement;
  aSel : TCSSElement;
  Valid : Boolean;

begin
  Result:=nil;
  Inc(FRuleLevel);
{$ifdef VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse @keyframes rule');
{$endif}
  aRule:=TCSSAtRuleElement(CreateElement(CSSAtRuleElementClass));
  try
    aRule.AtKeyWord:=CurrentTokenString;
    GetNextToken;

    // read the name of the animation
    case CurrentToken of
    ctkIDENTIFIER:
      aRule.AddSelector(ParseIdentifier);
    ctkSTRING:
      aRule.AddSelector(ParseString);
    else
      DoWarnExpectedButGot('identifier');
    end;

    if CurrentToken<>ctkLBRACE then
      begin
      if CurrentToken<>ctkEOF then
        begin
        DoWarnExpectedButGot('{');
        SkipRule;
        end;
      Result:=aRule;
      aRule:=nil;
      exit;
      end;
    GetNextToken;

    // read the keyframes
    While Not (CurrentToken in [ctkEOF,ctkRBRACE]) do
      begin
      if CurrentToken=ctkSEMICOLON then
        begin
        GetNextToken;
        continue;
        end;
      aKeyframe:=TCSSRuleElement(CreateElement(CSSRuleElementClass));
      try
        // read the keyframe selectors, e.g. 'from', 'to', '0%, 50%'
        Valid:=true;
        While Not (CurrentToken in [ctkEOF,ctkLBRACE,ctkRBRACE]) do
          begin
          aSel:=ParseKeyframeSelector;
          if aSel=nil then
            begin
            Valid:=false;
            break;
            end;
          aKeyframe.AddSelector(aSel);
          if CurrentToken<>ctkCOMMA then
            break;
          GetNextToken;
          end;

        if not Valid then
          // an invalid keyframe selector: skip the whole keyframe
          SkipRule
        else
          begin
          if CurrentToken=ctkLBRACE then
            begin
            GetNextToken;
            // a keyframe contains only declarations
            ParseRuleBody(aKeyframe);
            ConsumeRBrace;
            end;
          aRule.AddNestedRule(aKeyframe);
          aKeyframe:=nil;
          end;
      finally
        aKeyframe.Free;
      end;
      end;
    ConsumeRBrace;

    Result:=aRule;
    aRule:=nil;
{$ifdef VerboseCSSParser}  Writeln('Done Parse @keyframes rule ',aAt); {$endif}
    Dec(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseKeyframeSelector: TCSSElement;
// read a single keyframe selector: 'from', 'to' or a percentage

begin
  Result:=nil;
  case CurrentToken of
  ctkIDENTIFIER:
    begin
    case lowercase(CurrentTokenString) of
    'from','to': ;
    else
      DoWarnExpectedButGot('from or to');
    end;
    Result:=ParseIdentifier;
    end;
  ctkINTEGER:
    begin
    Result:=ParseInteger;
    if (Result is TCSSIntegerElement) and (TCSSIntegerElement(Result).Units<>cuPercent) then
      DoWarnExpectedButGot('percentage');
    end;
  ctkFLOAT:
    begin
    Result:=ParseFloat;
    if (Result is TCSSFloatElement) and (TCSSFloatElement(Result).Units<>cuPercent) then
      DoWarnExpectedButGot('percentage');
    end;
  else
    DoWarnExpectedButGot('percentage');
  end;
end;

function TCSSParser.ParseMediaCondition(TopLvl: boolean): TCSSElement;
// for example:
//   (color)
//   (color: #fff)
//   (30em <= width)
//   (30em >= width > 20em)
//   (aspect-ratio < 3/2)
//   (not(MediaCondition))
//   (not print)
//   (print or screen)
//   ((print))
//   ((print) and not screen)

  function ReadBinRightRatio(Bin: TCSSBinaryElement; Num: TCSSElement): boolean;
  var
    Sub: TCSSBinaryElement;
  begin
    Result:=false;
    // ratio value N/M
    GetNextToken; // consume '/'
    Sub:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
    Sub.Operation:=boDIV;
    Bin.Right:=Sub;
    Sub.Left:=Num;
    if CurrentToken=ctkINTEGER then
      Sub.Right:=ParseInteger
    else if CurrentToken=ctkFLOAT then
      Sub.Right:=ParseFloat
    else
      begin
      DoWarnExpectedButGot('integer');
      exit;
      end;
    Result:=Sub.Right<>nil;
  end;

var
  El, Sub: TCSSElement;
  Bin: TCSSBinaryElement;
  List, AndOrList: TCSSListElement;
  aToken: TCSSToken;
  IsAnd: boolean;
  Term: TCSSTokens;
begin
  Result:=nil;
  Term:=[ctkRPARENTHESIS,ctkSEMICOLON,ctkRBRACKET,ctkRBRACE,ctkLBRACE,ctkCOMMA];
  {$IFDEF VerboseCSSParser}
  writeln('TCSSParser.ParseMediaCondition START ',CurrentToken);
  {$ENDIF}

  El:=nil;
  Bin:=nil;
  List:=nil;
  AndOrList:=nil;
  try
    IsAnd:=false;
    repeat
      case CurrentToken of
      ctkIDENTIFIER:
        begin
        El:=ParseIdentifier;
        if (TCSSIdentifierElement(El).Value='not')
            or (TopLvl and (TCSSIdentifierElement(El).Value='only')) then
          begin
          if CurrentToken=ctkLPARENTHESIS then
            begin
            // not (mediacondition) ...
            List:=TCSSListElement(CreateElement(CSSListElementClass));
            List.AddChild(El);
            El:=nil;
            Sub:=ParseMediaBracket();
            if Sub=nil then
              exit;
            List.AddChild(Sub);

            El:=List;
            List:=nil;
            end
          else if CurrentToken=ctkIDENTIFIER then
            begin
            // not identifier ...
            List:=TCSSListElement(CreateElement(CSSListElementClass));
            List.AddChild(El);
            El:=nil;
            List.AddChild(ParseIdentifier);
            El:=List;
            List:=nil;
            end
          else
            begin
            DoWarnExpectedButGot('identifier');
            exit;
            end;
          end
        else if CurrentToken=ctkCOLON then
          begin
          // (mediaproperty: value)
          Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
          Bin.Operation:=boColon;
          Bin.Left:=El;
          El:=nil;
          GetNextToken;
          Bin.Right:=ParseComponentValue;
          if Bin.Right=nil then
            exit;
          El:=Bin;
          Bin:=nil;
          end;
        end;
      ctkSTRING:
        El:=ParseString;
      ctkINTEGER:
        El:=ParseInteger;
      ctkFLOAT:
        El:=ParseFloat;
      ctkLPARENTHESIS:
        El:=ParseMediaBracket();
      else
        DoWarnExpectedButGot('identifier');
        exit;
      end;
      if El=nil then exit;

      aToken:=CurrentToken;
      if AndOrList<>nil then
        begin
        AndOrList.AddChild(El);
        El:=nil;
        if aToken=ctkIDENTIFIER then
          begin
          if (CurrentTokenString='and') then
            begin
            if not IsAnd then
              begin
              // mixing "and" and "or" is not allowed
              DoWarnExpectedButGot('or');
              exit;
              end;
            end
          else if (CurrentTokenString='or') then
            begin
            if IsAnd then
              begin
              // mixing "and" and "or" is not allowed
              DoWarnExpectedButGot('or');
              exit;
              end;
            end
          else
            begin
            if TopLvl then
              DoWarnExpectedButGot('{')
            else
              DoWarnExpectedButGot(')');
            exit;
            end;
          AndOrList.AddChild(ParseIdentifier);
          end
        else if aToken in Term then
          begin
          Result:=AndOrList;
          AndOrList:=nil;
          exit;
          end
        else
          begin
          if TopLvl then
            DoWarnExpectedButGot('{')
          else
            DoWarnExpectedButGot(')');
          exit;
          end;
        end
      else if aToken=ctkIDENTIFIER then
        begin
        if (CurrentTokenString='and') then
          begin
          // "and" list
          IsAnd:=true;
          AndOrList:=TCSSListElement(CreateElement(CSSListElementClass));
          AndOrList.AddChild(El);
          El:=nil;
          end
        else if (CurrentTokenString='or') then
          begin
          // "or" list
          IsAnd:=false;
          AndOrList:=TCSSListElement(CreateElement(CSSListElementClass));
          AndOrList.AddChild(El);
          El:=nil;
          end
        else
          break;
        AndOrList.AddChild(ParseIdentifier);
        end
      else
        break;
    until false;

    // read binaryoperator operand til bracket close
    repeat
      aToken:=CurrentToken;
      {$IFDEF VerboseCSSParser}
      writeln('TCSSParser.ParseMediaCondition NEXT ',CurrentToken);
      {$ENDIF}
      if aToken in Term then
        begin
        Result:=El;
        El:=nil;
        break;
        end;
      case aToken of
      ctkEQUALS,
      ctkGE,ctkGT,ctkLE,ctkLT:
        begin
        Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
        Bin.Left:=El;
        El:=nil;
        Bin.Operation:=TokenToBinaryOperation(aToken);
        GetNextToken;
        end;
      else
        if TopLvl then
          DoWarnExpectedButGot('{')
        else
          DoWarnExpectedButGot(')');
        exit;
      end;

      case CurrentToken of
      ctkIDENTIFIER:
        Bin.Right:=ParseIdentifier;
      ctkSTRING:
        Bin.Right:=ParseString;
      ctkINTEGER:
        begin
        Sub:=ParseInteger;
        if (Sub<>nil) and (CurrentToken=ctkDIV) then
          begin
          if not ReadBinRightRatio(Bin,Sub) then exit;
          Sub:=nil;
          end
        else
          begin
          Bin.Right:=Sub;
          Sub:=nil;
          end;
        end;
      ctkFLOAT:
        begin
        Sub:=ParseFloat;
        if (Sub<>nil) and (CurrentToken=ctkDIV) then
          begin
          if not ReadBinRightRatio(Bin,Sub) then exit;
          Sub:=nil;
          end
        else
          begin
          Bin.Right:=Sub;
          Sub:=nil;
          end;
        end;
      else
        DoWarnExpectedButGot('identifier');
        exit;
      end;
      if Bin.Right=nil then
        exit;
      El:=Bin;
      Bin:=nil;
    until false;

  finally
    AndOrList.Free;
    List.Free;
    Bin.Free;
    El.Free;
  end;

  {$IFDEF VerboseCSSParser}
  writeln('TCSSParser.ParseMediaCondition END');
  {$ENDIF}
end;

function TCSSParser.ParseMediaBracket: TCSSElement;
begin
  Consume(ctkLPARENTHESIS);
  Result:=ParseMediaCondition(false);
  if CurrentToken=ctkRPARENTHESIS then
    GetNextToken
  else
    begin
    Result.Free;
    Result:=nil;
    DoWarnExpectedButGot(')');
    end;
end;

function TCSSParser.ParseExpression: TCSSElement;

Const
  RuleTokens =
       [ctkIDENTIFIER,ctkCLASSNAME,ctkHASH,ctkINTEGER,
        ctkPSEUDO,ctkPSEUDOFUNCTION,
        ctkCOLON,ctkDOUBLECOLON,ctkSTAR,ctkTILDE,ctkLBRACKET,ctkDOT,ctkPERCENTAGE];

begin
  if CurrentToken in RuleTokens then
    Result:=ParseRule
  else if CurrentToken=ctkATKEYWORD then
    case lowercase(CurrentTokenString) of
    '@media': Result:=ParseAtMediaRule;
    '@font-face',
    '@page': Result:=ParseAtSimpleRule;
    // a top level @starting-style contains only rules, no declarations
    '@starting-style': Result:=ParseAtSimpleRule(true);
    else
      if IsAtKeyframesKeyword(CurrentTokenString) then
        // @keyframes and its vendor prefixed forms
        Result:=ParseAtKeyframesRule
      else
        // e.g. @supports: only rules are allowed in the block of a top level at-rule
        Result:=ParseAtUnknownRule(true);
    end
  else
    Result:=ParseComponentValueList;
end;

function TCSSParser.ParseRuleList(aStopOn : TCSStoken = ctkEOF;
  aSkipDeclarations : Boolean = False): TCSSElement;

Var
  aList : TCSSCompoundElement;
  aEl : TCSSElement;
  Terms : TCSSTokens;
begin
  Terms:=[ctkEOF,aStopOn];
  aList:=TCSSCompoundElement(CreateElement(CSSCompoundElementClass));
  Try
    While not (CurrentToken in Terms) do
      begin
      if aSkipDeclarations and CurrentStartsDeclaration then
        // a declaration directly in this at-rule block: only rules allowed
        SkipInvalidDeclaration
      else
        begin
        aEl:=ParseExpression;
        aList.AddChild(aEl);
        end;
      if CurrentToken=ctkSEMICOLON then
        GetNextToken;
      end;
    Result:=aList;
    aList:=nil;
  finally
    aList.Free;
  end;
end;

function TCSSParser.Parse: TCSSElement;
begin
  FPrevious:=ctkUNKNOWN;
  GetNextToken;
  if CurrentToken=ctkLBRACE then
    Result:=ParseRule
  else
    Result:=ParseRuleList;
end;

function TCSSParser.ParseInline: TCSSElement;
var
  aRule: TCSSRuleElement;
begin
  FPrevious:=ctkUNKNOWN;
  GetNextToken;
  aRule:=TCSSRuleElement(CreateElement(CSSRuleElementClass));
  try
    ParseRuleBody(aRule);
    Result:=aRule;
    aRule:=nil;
  finally
    aRule.Free;
  end;
end;

function TCSSParser.GetNextToken: TCSSToken;
begin
  FPrevious:=FCurrent;
  If (FPeekToken<>ctkUNKNOWN) then
    begin
    FCurrent:=FPeekToken;
    FCurrentTokenString:=FPeekTokenString;
    FCurrentTokenRow:=FPeekTokenRow;
    FCurrentTokenCol:=FPeekTokenCol;
    FCurrentTokenPos:=FPeekTokenPos;
    FCurrentTokenEndRow:=FPeekTokenEndRow;
    FCurrentTokenEndCol:=FPeekTokenEndCol;
    FPeekToken:=ctkUNKNOWN;
    FPeekTokenString:='';
    end
  else
    begin
    FCurrent:=FScanner.FetchToken;
    FCurrentTokenString:=FScanner.CurTokenString;
    FCurrentTokenRow:=FScanner.CurTokenRow;
    FCurrentTokenCol:=FScanner.CurTokenColumn;
    FCurrentTokenPos:=FScanner.CurTokenPos;
    FCurrentTokenEndRow:=FScanner.CurRow;
    FCurrentTokenEndCol:=FScanner.CurColumn;
    end;
  if not (FCurrent in [ctkWhitespace,ctkComment]) then
    begin
    // remember the end of the significant token before this one
    FPrevSigTokenEndRow:=FCurSigTokenEndRow;
    FPrevSigTokenEndCol:=FCurSigTokenEndCol;
    FCurSigTokenEndRow:=FCurrentTokenEndRow;
    FCurSigTokenEndCol:=FCurrentTokenEndCol;
    end;
  Result:=FCurrent;
  {$ifdef VerboseCSSParser}
     Writeln('GetNextToken returns ',
       GetEnumName(TypeInfo(TCSSToken),Ord(FCurrent)),
       '(String: "',FCurrentTokenString,'")',
       ' at (',FScanner.CurRow,',',FScanner.CurColumn,'): ',
       FSCanner.CurLine);
  {$endif VerboseCSSParser}
end;

function TCSSParser.PeekNextToken: TCSSToken;
begin
  If (FPeekToken=ctkUNKNOWN) then
    begin
    FPeekToken:=FScanner.FetchToken;
    FPeekTokenString:=FScanner.CurTokenString;
    FPeekTokenRow:=FScanner.CurTokenRow;
    FPeekTokenCol:=FScanner.CurTokenColumn;
    FPeekTokenPos:=FScanner.CurTokenPos;
    FPeekTokenEndRow:=FScanner.CurRow;
    FPeekTokenEndCol:=FScanner.CurColumn;
    end;
  {$ifdef VerboseCSSParser}Writeln('PeekNextToken : ',GetEnumName(TypeInfo(TCSSToken),Ord(FPeekToken)), ' As TCSSString: ',FPeekTokenString);{$endif VerboseCSSParser}
  Result:=FPeekToken;
end;

function TCSSParser.SaveState: TCSSParserState;
begin
  Result.ScannerState:=FScanner.SaveState;
  Result.Previous:=FPrevious;
  Result.Current:=FCurrent;
  Result.CurrentTokenString:=FCurrentTokenString;
  Result.PeekToken:=FPeekToken;
  Result.PeekTokenString:=FPeekTokenString;
  Result.CurrentTokenRow:=FCurrentTokenRow;
  Result.CurrentTokenCol:=FCurrentTokenCol;
  Result.CurrentTokenPos:=FCurrentTokenPos;
  Result.PeekTokenRow:=FPeekTokenRow;
  Result.PeekTokenCol:=FPeekTokenCol;
  Result.PeekTokenPos:=FPeekTokenPos;
  Result.CurrentTokenEndRow:=FCurrentTokenEndRow;
  Result.CurrentTokenEndCol:=FCurrentTokenEndCol;
  Result.PeekTokenEndRow:=FPeekTokenEndRow;
  Result.PeekTokenEndCol:=FPeekTokenEndCol;
  Result.CurSigTokenEndRow:=FCurSigTokenEndRow;
  Result.CurSigTokenEndCol:=FCurSigTokenEndCol;
  Result.PrevSigTokenEndRow:=FPrevSigTokenEndRow;
  Result.PrevSigTokenEndCol:=FPrevSigTokenEndCol;
end;

procedure TCSSParser.RestoreState(const aState: TCSSParserState);
begin
  FScanner.RestoreState(aState.ScannerState);
  FPrevious:=aState.Previous;
  FCurrent:=aState.Current;
  FCurrentTokenString:=aState.CurrentTokenString;
  FPeekToken:=aState.PeekToken;
  FPeekTokenString:=aState.PeekTokenString;
  FCurrentTokenRow:=aState.CurrentTokenRow;
  FCurrentTokenCol:=aState.CurrentTokenCol;
  FCurrentTokenPos:=aState.CurrentTokenPos;
  FPeekTokenRow:=aState.PeekTokenRow;
  FPeekTokenCol:=aState.PeekTokenCol;
  FPeekTokenPos:=aState.PeekTokenPos;
  FCurrentTokenEndRow:=aState.CurrentTokenEndRow;
  FCurrentTokenEndCol:=aState.CurrentTokenEndCol;
  FPeekTokenEndRow:=aState.PeekTokenEndRow;
  FPeekTokenEndCol:=aState.PeekTokenEndCol;
  FCurSigTokenEndRow:=aState.CurSigTokenEndRow;
  FCurSigTokenEndCol:=aState.CurSigTokenEndCol;
  FPrevSigTokenEndRow:=aState.PrevSigTokenEndRow;
  FPrevSigTokenEndCol:=aState.PrevSigTokenEndCol;
end;

procedure TCSSParser.DropState;
begin
  FScanner.DropState;
end;

function TCSSParser.ParseUnit : TCSSUnit;

var
  aName: TCSSString;
  U: TCSSUnit;
begin
  Result:=cuNone;
  case CurrentToken of
  ctkPERCENTAGE:
    begin
    Result:=cuPercent;
    GetNextToken;
    end;
  ctkIDENTIFIER:
    begin
    // match the whole unit name, not just a prefix, otherwise a short name eats
    // every longer identifier starting with it, e.g. 'in' would match 'index'
    aName:=CurrentTokenString;
    for U:=Succ(cuNone) to High(TCSSUnit) do
      if CSSUnitNames[U]=aName then
        begin
        Result:=U;
        GetNextToken;
        break;
        end;
    end;
  ctkWHITESPACE:
    GetNextToken;
  end;
end;

function TCSSParser.CreateElement(aClass : TCSSElementClass): TCSSElement;

begin
  // Use the start of the current token, so the element position points at the
  // first character of the token (e.g. the start of a declaration's attribute name).
  if Assigned(FScanner) then
    begin
    // Note: FScanner knows only the last fetched token, which is the peeked one
    // while a peek is pending, so use the position remembered by GetNextToken.
    Result:=aClass.Create(CurrentSource,FCurrentTokenRow,FCurrentTokenCol);
    Result.SourcePos:=FCurrentTokenPos;
    end
  else
    Result:=aClass.Create(CurrentSource,CurrentLine,CurrentPos);
end;

function TCSSParser.ParseIdentifier: TCSSIdentifierElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  Result:=TCSSIdentifierElement(CreateElement(CSSIdentifierElementClass));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseHashIdentifier: TCSSHashIdentifierElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  system.delete(aValue,1,1);
  Result:=TCSSHashIdentifierElement(CreateElement(CSSHashIdentifierElementClass));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseClassName: TCSSClassNameElement;

Var
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  system.delete(aValue,1,1);
  Result:=TCSSClassNameElement(CreateElement(CSSClassNameElementClass));
  Result.Value:=aValue;
  GetNextToken;
end;

function TCSSParser.ParseInteger: TCSSElement;

Var
  aCode, aValue : Integer;
  aInt : TCSSIntegerElement;
  OldReturnWhiteSpace: Boolean;

begin
  Val(CurrentTokenString,aValue,aCode);
  if aCode<>0 then
    begin
    DoWarn(SErrInvalidFloat,[CurrentTokenString]);
    GetNextToken;
    exit(nil);
    end;
  aInt:=TCSSIntegerElement(CreateElement(CSSIntegerElementClass));
  OldReturnWhiteSpace:=Scanner.ReturnWhiteSpace;
  try
    aInt.Value:=aValue;
    Scanner.ReturnWhiteSpace:=true;
    Consume(ctkINTEGER);
    aInt.Units:=ParseUnit;
    Result:=aInt;
    aInt:=nil;
  finally
    aInt.Free;
    Scanner.ReturnWhiteSpace:=OldReturnWhiteSpace;
    SkipWhiteSpace;
  end;
end;

function TCSSParser.ParseFloat: TCSSElement;
Var
  aCode : Integer;
  aValue : Double;
  aFloat : TCSSFloatElement;
  OldReturnWhiteSpace: Boolean;

begin
  Val(CurrentTokenString,aValue,aCode);
  if aCode<>0 then
    begin
    DoWarn(SErrInvalidFloat,[CurrentTokenString]);
    GetNextToken;
    exit(nil);
    end;
  aFloat:=TCSSFloatElement(CreateElement(CSSFloatElementClass));
  OldReturnWhiteSpace:=Scanner.ReturnWhiteSpace;
  try
    aFloat.Value:=aValue;
    Scanner.ReturnWhiteSpace:=true;
    Consume(ctkFloat);
    aFloat.Units:=ParseUnit;
    if CurrentToken=ctkWHITESPACE then
      GetNextToken;
    Result:=aFloat;
    aFloat:=nil;
  finally
    Scanner.ReturnWhiteSpace:=OldReturnWhiteSpace;
    aFloat.Free;
  end;
end;


function TCSSParser.ParseParenthesis: TCSSElement;

var
  aParen : TCSSParenthesisElement;
  aList: TCSSElement;
begin
  // Create the parenthesis node while the '(' is current, so it points at the opening bracket.
  aParen:=TCSSParenthesisElement(CreateElement(CSSParenthesisElementClass));
  try
    Consume(ctkLPARENTHESIS);
    if CurrentToken in [ctkEOF, ctkSEMICOLON, ctkRBRACE] then
      begin
      FInvalidDeclarationValue:=True;
      DoWarn(SErrUnexpectedEndOfFile,['(']);
      Result:=aParen;
      aParen:=nil;
      exit;
      end;
    aList:=ParseComponentValueList;
    if CurrentToken<>ctkRPARENTHESIS then
      begin
      FInvalidDeclarationValue:=True;
      DoWarn(SErrUnexpectedEndOfFile,['(']);
      end
    else
      GetNextToken;
    if Assigned(aList) then
      aParen.AddChild(aList);
    Result:=aParen;
    aParen:=nil;
  finally
    aParen.Free;
  end;
end;

function TCSSParser.ParseURL: TCSSElement;

Var
  aURL : TCSSURLElement;

begin
  aURL:=TCSSURLElement(CreateElement(CSSURLElementClass));
  try
    aURL.Value:=CurrentTokenString;
    Consume(ctkURL);
    Result:=aURL;
    aURL:=nil;
  finally
    aURL.Free;
  end;
end;

function TCSSParser.ParseInvalidToken: TCSSElement;
begin
  Result:=TCSSElement(CreateElement(TCSSElement));
  GetNextToken;
end;

function TCSSParser.ParsePseudoClass: TCSSElement;

Var
  aPseudo : TCSSPseudoClassElement;
  aValue : TCSSString;

begin
  aValue:=CurrentTokenString;
  aPseudo:=TCSSPseudoClassElement(CreateElement(CSSPseudoClassElementClass));
  try
    Consume(ctkPseudo);
    aPseudo.Value:=aValue;
    Result:=aPseudo;
    aPseudo:=nil;
  finally
    aPseudo.Free;
  end;
end;

function TCSSParser.ParsePseudoElement: TCSSElement;
begin
  if CurrentToken<>ctkDOUBLECOLON then
    raise ECSSParser.Create('20250224201230');
  GetNextToken;
  case CurrentToken of
  ctkIDENTIFIER: Result:=ParseIdentifier;
  ctkFUNCTION: Result:=ParseCall('',false);
  else
    DoWarnExpectedButGot('pseudo element name');
    Result:=nil;
  end;
end;

function TCSSParser.LookAheadIsNestedRule: Boolean;
// The current token starts a statement of a rule body, which can be a
// declaration "color:red;" or a nested rule "div:hover{}".
// Scan forward until { or ; and rewind:
// Note: the scanned tokens cannot be buffered and reused, because the scanner
// options change how the source is tokenized. For example ParseDeclaration sets
// DisablePseudo, so that 'width:10px' gives a ctkCOLON instead of a ctkPSEUDO.
// That is why everything is scanned again after RestoreState.
var
  State: TCSSParserState;
  Depth: Integer;
  aToken: TCSSToken;
  First: Boolean;
begin
  Result:=false;
  if FPeekToken=ctkUNKNOWN then
    // Most statements end on the line they start, so try the cheap way first:
    // read the rest of the current line, without fetching tokens and without
    // having to rewind.
    case FScanner.SearchStatementEndInCurLine of
    cseLBrace: exit(true);
    cseEnd: exit(false);
    end;

  State:=SaveState;
  try
    Depth:=0;
    First:=true;
    repeat
      aToken:=GetNextToken;
      if First then
        begin
        First:=false;
        if aToken=ctkCOLON then
          // e.g. 'color: red'. Note: 'div:hover' gives a single ctkPSEUDO token,
          // because the scanner option DisablePseudo is not set here.
          break;
        end;
      case aToken of
      ctkEOF:
        break;
      ctkLPARENTHESIS,ctkLBRACKET,
      ctkFUNCTION,ctkPSEUDOFUNCTION: // these tokens contain the opening '('
        Inc(Depth);
      ctkRPARENTHESIS,ctkRBRACKET:
        begin
        if Depth=0 then break;
        Dec(Depth);
        end;
      ctkLBRACE:
        begin
        Result:=Depth=0;
        break;
        end;
      ctkSEMICOLON,ctkRBRACE:
        if Depth=0 then break;
      end;
    until false;
  finally
    RestoreState(State);
  end;
end;

function TCSSParser.CurrentStartsNestedRule: Boolean;

Const
  NestedRuleTokens: TCSSTokens = [ctkAND, ctkCLASSNAME, ctkHASH, ctkPSEUDO,
                                  ctkPSEUDOFUNCTION, ctkLBRACKET, ctkDOUBLECOLON,
                                  ctkPLUS, ctkGT, ctkTILDE];
  // These start a type or universal selector, but a declaration as well.
  AmbiguousTokens: TCSSTokens = [ctkIDENTIFIER, ctkSTAR];

begin
  if CurrentToken in NestedRuleTokens then
    exit(true);
  if not (CurrentToken in AmbiguousTokens) then
    exit(false);
  if (CurrentToken=ctkIDENTIFIER) and (Copy(CurrentTokenString,1,2)='--') then
    exit(false); // a custom property, its value may contain a block
  Result:=LookAheadIsNestedRule;
end;

function TCSSParser.CurrentStartsDeclaration: Boolean;
// Whether the current token starts a declaration instead of a rule.
// Only an identifier can start a declaration, e.g. 'color:red' or '--foo:red'.
begin
  Result:=(CurrentToken=ctkIDENTIFIER) and not CurrentStartsNestedRule;
end;

procedure TCSSParser.SkipInvalidDeclaration(aIsAt: Boolean);
// Skip a declaration in a block where only rules are allowed, e.g. the body of
// a top level @media, @supports or @starting-style.
var
  aDecl : TCSSElement;
begin
  DoWarnExpectedButGot('selector');
  aDecl:=ParseDeclaration(aIsAt);
  if aDecl=nil then
    // skip invalid
    while not (CurrentToken in [ctkEOF,ctkSEMICOLON,ctkRBRACE]) do
      GetNextToken
  else
    aDecl.Free;
end;

function TCSSParser.ParseRuleBody(aRule: TCSSRuleElement; aIsAt: Boolean = false;
  aSkipDeclarations: Boolean = false): integer;

Var
  aDecl : TCSSElement;
  aNestedRule: TCSSRuleElement;

begin
  aDecl:=nil;
  While Not (CurrentToken in [ctkEOF,ctkRBRACE]) do
    begin
    While CurrentToken in [ctkSEMICOLON,ctkUNKNOWN] do
      GetNextToken;
    if (CurrentToken in [ctkEOF,ctkRBRACE]) then
      break;
    if CurrentToken=ctkATKEYWORD then
      begin
      // an at-rule nested in a style rule, e.g. @supports or @starting-style,
      // can contain declarations as well as nested rules
      aNestedRule:=ParseAtNestedRule;
      if aNestedRule<>nil then
        aRule.AddNestedRule(aNestedRule);
      end
    else if CurrentStartsNestedRule then
      begin
      aNestedRule:=ParseRule;
      if aNestedRule<>nil then
        aRule.AddNestedRule(aNestedRule);
      end
    else if aSkipDeclarations then
      // e.g. a declaration directly in a top level @starting-style: only rules allowed
      SkipInvalidDeclaration(aIsAt)
    else
      begin
      aDecl:=ParseDeclaration(aIsAt);
      if aDecl=nil then
        begin
        // skip invalid
        while not (CurrentToken in [ctkEOF,ctkSEMICOLON,ctkRBRACE]) do
          GetNextToken;
        end
      else if aRule.NestedRuleCount=0 then
        aRule.AddChild(aDecl)
      else
        begin
        // declarations behind nested rules are added to a special nested rule
        aNestedRule:=aRule.NestedRules[aRule.NestedRuleCount-1];
        if (aNestedRule.SelectorCount>0) or (aNestedRule is TCSSAtRuleElement) then
          begin
          // add special nested rule
          aNestedRule:=TCSSRuleElement(CreateElement(CSSRuleElementClass));
          aRule.AddNestedRule(aNestedRule);
          end;
        aNestedRule.AddChild(aDecl);
        end;
      end;
    end;
  Result:=aRule.ChildCount;
end;

function TCSSParser.ParseRule: TCSSRuleElement;

Var
  aRule : TCSSRuleElement;
  aSel : TCSSElement;
  Term : TCSSTokens;
{$IFDEF VerboseCSSParser}
  aAt : TCSSString;
{$ENDIF}

begin
  Result:=nil;
  Inc(FRuleLevel);
{$IFDEF VerboseCSSParser}
  aAt:=Format(' Level %d at (%d:%d)',[FRuleLevel,CurrentLine,CurrentPos]);
  Writeln('Parse rule.: ',aAt);
{$ENDIF}
  case CurrentToken of
  ctkEOF: exit;
  ctkSEMICOLON:
    begin
    Result:=TCSSRuleElement(CreateElement(CSSRuleElementClass));
    exit;
    end;
  end;

  Term:=[ctkLBRACE,ctkEOF,ctkSEMICOLON];
  aRule:=TCSSRuleElement(CreateElement(CSSRuleElementClass));
  try
    While Not (CurrentToken in Term) do
      begin
      aSel:=ParseSelector;
      if aSel=nil then
        begin
        SkipRule;
        exit;
        end;
      aRule.AddSelector(aSel);
      if CurrentToken=ctkCOMMA then
        GetNextToken;
      end;
    // Note: no selectors is allowed
    if (CurrentToken=ctkLBRACE) then
      begin
      GetNextToken;
      ParseRuleBody(aRule);
      ConsumeRBrace;
      end;
    Result:=aRule;
    aRule:=nil;
    {$IFDEF VerboseCSSParser}
    Writeln('Rule started at ',aAt,' done');
    {$endif}
    Dec(FRuleLevel);
  finally
    aRule.Free;
  end;
end;

function TCSSParser.ParseUnary: TCSSElement;

var
  Un : TCSSUnaryElement;
  Op : TCSSUnaryOperation;
  El: TCSSElement;
  aRow, aCol, aPos : Integer;
  aFileName : TCSSString;

begin
  Result:=nil;
  if not (CurrentToken in [ctkDOUBLECOLON, ctkMinus, ctkPlus, ctkDiv, ctkGT, ctkTILDE]) then
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,[CurrentTokenString]);
  op:=TokenToUnaryOperation(CurrentToken);
  // Remember the operator location, so the element position points at the start of the unary value.
  aRow:=FScanner.CurTokenRow;
  aCol:=FScanner.CurTokenColumn;
  aPos:=FScanner.CurTokenPos;
  aFileName:=CurrentSource;
  GetNextToken;
  if CurrentToken=ctkWHITESPACE then
    Raise ECSSParser.CreateFmt(SUnaryInvalidToken,['white space']);
  El:=ParseComponentValue;

  Un:=TCSSUnaryElement(CreateElement(CSSUnaryElementClass));
  Un.SetLocation(aRow,aCol,aFileName);
  Un.SourcePos:=aPos;
  Un.Operation:=op;
  Un.Right:=El;
  Result:=Un;
end;

function TCSSParser.ParseComponentValueList(AllowRules : Boolean = True): TCSSElement;

Const
  TermSeps = [ctkEquals,ctkPlus,ctkMinus,ctkAnd,ctkLT,ctkDIV,
              ctkStar,ctkTilde,ctkColon, ctkDoubleColon,
              ctkSquared,ctkGT, ctkPIPE, ctkDOLLAR];
  ListTerms = [ctkEOF,ctkLBRACE,ctkATKEYWORD,ctkComma];

  function DoBinary(var aLeft : TCSSElement) : TCSSElement;
  var
    Bin : TCSSBinaryElement;
  begin
    Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
    try
      Bin.Left:=ALeft;
      aLeft:=Nil;
      Bin.Operation:=TokenToBinaryOperation(CurrentToken);
      GetNextToken;
      Bin.Right:=ParseComponentValue;
      if Bin.Right=nil then
        DoWarn(SErrUnexpectedToken ,[
               GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
               CurrentTokenString,
               'value'
               ]);
      Result:=Bin;
      Bin:=nil;
    finally
      Bin.Free;
    end;
  end;

Var
  List : TCSSListElement;
  aFactor : TCSSelement;

begin
  aFactor:=Nil;
  List:=TCSSListElement(CreateElement(CSSListElementClass));
  try
    if AllowRules and (CurrentToken in [ctkLBRACE,ctkATKEYWORD]) then
      begin
      if CurrentToken=ctkATKEYWORD then
        aFactor:=ParseAtUnknownRule
      else
        aFactor:=ParseRule;
      end
    else
      aFactor:=ParseComponentValue;
    if aFactor=nil then
      begin
      DoWarn(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             'value'
             ]);
      GetNextToken;
      Result:=GetAppendElement(List);
      List:=nil;
      exit;
      end;
    While Assigned(aFactor) do
      begin
      While CurrentToken in TermSeps do
        aFactor:=DoBinary(aFactor);
      List.AddChild(aFactor);
      aFactor:=Nil;
      if not (CurrentToken in ListTerms) then
        aFactor:=ParseComponentValue;
      end;
    Result:=GetAppendElement(List);
    List:=nil;
  finally
    List.Free;
    aFactor.Free;
  end;
end;


function TCSSParser.ParseComponentValue: TCSSElement;

Const
  FinalTokens =
     [ctkLPARENTHESIS,ctkURL,ctkColon,ctkLBRACE, ctkLBRACKET,
      ctkDOUBLECOLON,ctkMinus,ctkPlus,ctkDiv,ctkSTAR,ctkTILDE];

var
  aToken : TCSSToken;

begin
  aToken:=CurrentToken;
  if aToken=ctkUNKNOWN then
    begin
    DoWarn('invalid');
    repeat
      GetNextToken;
    until CurrentToken<>ctkUNKNOWN;
    aToken:=CurrentToken;
    end;
  Case aToken of
    ctkEOF: exit(nil);
    ctkLPARENTHESIS: Result:=ParseParenthesis;
    ctkURL: Result:=ParseURL;
    ctkPSEUDO: Result:=ParsePseudoClass;
    ctkLBRACE: Result:=ParseRule;
    ctkLBRACKET: Result:=ParseArray(Nil,false);
    ctkMinus,
    ctkPlus,
    ctkDiv,
    ctkGT,
    ctkTilde: Result:=ParseUnary;
    ctkUnicodeRange: Result:=ParseUnicodeRange;
    ctkSTRING: Result:=ParseString;
    ctkHASH: Result:=ParseHashValue;
    ctkINTEGER: Result:=ParseInteger;
    ctkFloat : Result:=ParseFloat;
    ctkPSEUDOFUNCTION,
    ctkFUNCTION : Result:=ParseCall('',false);
    ctkSTAR: Result:=ParseInvalidToken;
    ctkIDENTIFIER,ctkPERCENTAGE: Result:=ParseIdentifier;
    ctkCLASSNAME : Result:=ParseClassName;
  else
    Result:=nil;
  end;
  if (aToken in FinalTokens) or (PreviousToken=ctkWHITESPACE) then
    exit;
  if (CurrentToken=ctkLBRACKET) then
    Result:=ParseArray(Result,false);
end;

function TCSSParser.ParseSelector: TCSSElement;

  function ParseBinaryPseudoElement(var El: TCSSElement): boolean;
  var
    Bin: TCSSBinaryElement;
  begin
    Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
    Bin.Left:=El;
    El:=Bin;
    Bin.Operation:=boDoubleColon;
    Bin.Right:=ParsePseudoElement;
    Result:=Bin.Right<>nil;
  end;

  function ParseUnaryPseudoElement: TCSSElement;
  var
    Un: TCSSUnaryElement;
  begin
    Un:=TCSSUnaryElement(CreateElement(CSSUnaryElementClass));
    Result:=Un;
    Un.Operation:=uoDoubleColon;
    Un.Right:=ParsePseudoElement;
  end;

  function ParseSub: TCSSElement;
  var
    Un: TCSSUnaryElement;
    Sub: TCSSElement;
    aOperation: TCSSUnaryOperation;
  begin
    Result:=nil;
    Case CurrentToken of
      ctkAND, // the & of a nested rule
      ctkSTAR,
      ctkIDENTIFIER : Result:=ParseIdentifier;
      ctkHASH : Result:=ParseHashIdentifier;
      ctkCLASSNAME : Result:=ParseClassName;
      ctkLBRACKET: Result:=ParseAttributeSelector;
      ctkPSEUDO: Result:=ParsePseudoClass;
      ctkPSEUDOFUNCTION: Result:=ParseCall('',true);
      ctkDOUBLECOLON: Result:=ParseUnaryPseudoElement;
      ctkPLUS, ctkGT, ctkTILDE:
        begin
        aOperation:=TokenToUnaryOperation(CurrentToken);
        GetNextToken;
        SkipWhiteSpace;
        Sub:=ParseSub();
        if Sub=nil then exit;
        Un:=TCSSUnaryElement(CreateElement(CSSUnaryElementClass));
        Un.Operation:=aOperation;
        Un.Right:=Sub;
        Result:=Un;
        end;
    else
      DoWarn(SErrUnexpectedToken ,[
               GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
               CurrentTokenString,
               'selector'
               ]);
      case CurrentToken of
      ctkINTEGER: Result:=ParseInteger;
      ctkFLOAT: Result:=ParseFloat;
      else Result:=ParseInvalidToken;
      end;
    end;
  end;

var
  ok, OldReturnWhiteSpace: Boolean;
  Bin: TCSSBinaryElement;
  El, Sub: TCSSElement;
  List: TCSSListElement;
begin
  Result:=nil;
  if CurrentToken in [ctkLBRACE,ctkRBRACE,ctkRPARENTHESIS,ctkEOF] then
    exit;
  El:=nil;
  Bin:=nil;
  List:=nil;
  ok:=false;
  //writeln('TCSSParser.ParseSelector START ',CurrentToken);
  OldReturnWhiteSpace:=Scanner.ReturnWhiteSpace;
  Scanner.ReturnWhiteSpace:=true; // needed for the descendant operator - a whitespace
  try
    repeat
      {$IFDEF VerboseCSSParser}
      writeln('TCSSParser.ParseSelector LIST START ',CurrentToken,' ',CurrentTokenString);
      {$ENDIF}
      // read list
      List:=nil;
      El:=ParseSub;
      {$IFDEF VerboseCSSParser}
      writeln('TCSSParser.ParseSelector LIST NEXT ',CurrentToken,' ',CurrentTokenString,' El=',GetCSSObj(El));
      {$ENDIF}
      if El=nil then
        exit;

      while CurrentToken in [ctkSTAR,ctkHASH,ctkIDENTIFIER,ctkCLASSNAME,ctkLBRACKET,ctkPSEUDO,ctkPSEUDOFUNCTION] do
        begin
        if List=nil then
          begin
          List:=TCSSListElement(CreateElement(CSSListElementClass));
          List.AddChild(El);
          El:=List;
          end;
        Sub:=ParseSub;
        if Sub=nil then break;
        List.AddChild(Sub);
        end;
      List:=nil;

      // read postfix pseudo elements
      while CurrentToken=ctkDOUBLECOLON do
        if not ParseBinaryPseudoElement(El) then break;

      // use element
      if Bin<>nil then
        Bin.Right:=El
      else
        Result:=El;
      El:=nil;

      SkipWhiteSpace;
      {$IFDEF VerboseCSSParser}
      writeln('TCSSParser.ParseSelector LIST END ',CurrentToken,' ',CurrentTokenString);
      {$ENDIF}

      case CurrentToken of
      ctkLBRACE,ctkRBRACE,ctkRBRACKET,ctkRPARENTHESIS,ctkEOF,ctkSEMICOLON,ctkCOMMA:
        break;
      ctkGT,ctkPLUS,ctkTILDE,ctkPIPE:
        begin
        // combinator
        Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
        Bin.Left:=Result;
        Result:=Bin;
        Bin.Operation:=TokenToBinaryOperation(CurrentToken);
        GetNextToken;
        SkipWhiteSpace;
        end;
      ctkSTAR,ctkHASH,ctkIDENTIFIER,ctkCLASSNAME,ctkLBRACKET,ctkPSEUDO,ctkPSEUDOFUNCTION,ctkAND:
        begin
        // descendant combinator
        Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
        Bin.Left:=Result;
        Result:=Bin;
        Bin.Operation:=boWhiteSpace;
        end;
      else
        break;
      end;
    until false;
    ok:=true;
  finally
    Scanner.ReturnWhiteSpace:=OldReturnWhiteSpace;
    if not ok then
      begin
      if Result=Bin then Bin:=nil;
      if El=List then List:=nil;
      if Result=El then El:=nil;
      Result.Free;
      El.Free;
      List.Free;
      Bin.Free;
      end;
  end;
  SkipWhiteSpace;
end;

function TCSSParser.ParseAttributeSelector: TCSSElement;

Var
  aEl : TCSSElement;
  aArray : TCSSArrayElement;
  Bin: TCSSBinaryElement;
  StrEl: TCSSStringElement;
  aToken: TCSSToken;

begin
  Result:=Nil;
  aArray:=TCSSArrayElement(CreateElement(CSSArrayElementClass));
  try
    Consume(ctkLBRACKET);
    SkipWhiteSpace;
    if CurrentToken<>ctkIDENTIFIER then
      begin
      DoWarnExpectedButGot('identifier');
      Result:=aArray;
      aArray:=nil;
      exit;
      end;
    aEl:=ParseWQName;
    SkipWhiteSpace;
    aToken:=CurrentToken;
    case aToken of
    ctkEQUALS,ctkTILDEEQUAL,ctkPIPEEQUAL,ctkSQUAREDEQUAL,ctkDOLLAREQUAL,ctkSTAREQUAL:
      begin
      // parse attr-matcher
      Bin:=TCSSBinaryElement(CreateElement(CSSBinaryElementClass));
      aArray.AddChild(Bin);
      Bin.Left:=aEl;
      Bin.Operation:=TokenToBinaryOperation(aToken);
      GetNextToken;
      SkipWhiteSpace;
      // parse value
      case CurrentToken of
      ctkIDENTIFIER:
        Bin.Right:=ParseIdentifier;
      ctkSTRING:
        begin
        StrEl:=TCSSStringElement(CreateElement(CSSStringElementClass));
        StrEl.Value:=CurrentTokenString;
        Bin.Right:=StrEl;
        GetNextToken;
        end;
      ctkINTEGER:
        Bin.Right:=ParseInteger;
      ctkFLOAT:
        Bin.Right:=ParseFloat;
      else
        DoWarn(SErrUnexpectedToken ,[
                 GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
                 CurrentTokenString,
                 'attribute value'
                 ]);
      end;
      end;
    else
      aArray.AddChild(aEl);
    end;
    SkipWhiteSpace;
    while CurrentToken=ctkIDENTIFIER do
      begin
      // attribute modifier
      // with CSS 5 there is only i and s, but for future compatibility read all
      aArray.AddChild(ParseIdentifier);
      SkipWhiteSpace;
      end;
    if CurrentToken=ctkRBRACKET then
      GetNextToken
    else
      DoWarnExpectedButGot(']');

    Result:=aArray;
    aArray:=nil;
  finally
    aArray.Free;
  end;
end;

function TCSSParser.ParseWQName: TCSSElement;
begin
  if CurrentToken<>ctkIDENTIFIER then
    begin
    DoWarn(SErrUnexpectedToken ,[
             GetEnumName(TypeInfo(TCSSToken),Ord(CurrentToken)),
             CurrentTokenString,
             'identifier'
             ]);
    Result:=nil;
    exit;
    end;
  Result:=ParseIdentifier;
  // todo: parse optional ns-prefix
end;

function TCSSParser.ParseDeclaration(aIsAt: Boolean = false): TCSSDeclarationElement;

Var
  aDecl : TCSSDeclarationElement;
  aKey,aValue : TCSSElement;
  aList : TCSSListElement;
  OldOptions: TCSSScannerOptions;

begin
  aList:=nil;
  FInvalidDeclarationValue:=False;
  OldOptions:=Scanner.Options;
  aDecl:=TCSSDeclarationElement(CreateElement(CSSDeclarationElementClass));
  try
    // read attribute names
    Scanner.DisablePseudo:=True;
    aKey:=ParseComponentValue;
    aDecl.AddKey(aKey);
    if aIsAt then
      begin
      While (CurrentToken=ctkCOMMA) do
        begin
        while (CurrentToken=ctkCOMMA) do
          GetNextToken;
        aKey:=ParseComponentValue;
        aDecl.AddKey(aKey);
        end;
      end;
    if Not aIsAt then
      begin
      if CurrentToken<>ctkCOLON then
        begin
        DoWarnExpectedButGot(':');
        Result:=nil;
        exit;
        end;
      aDecl.Colon:=True;
      GetNextToken;
      end
    else
      begin
      aDecl.Colon:=CurrentToken=ctkColon;
      if aDecl.Colon then
        GetNextToken
      end;
    aValue:=ParseComponentValue;
    aList:=TCSSListElement(CreateElement(CSSListElementClass));
    aList.AddChild(aValue);
    if aDecl.Colon then
      begin
      // read attribute value
      // + and - must be enclosed in whitespace, +3 and -4 are values
      While not (CurrentToken in [ctkEOF,ctkSemicolon,ctkRBRACE,ctkImportant]) do
        begin
        While CurrentToken=ctkCOMMA do
          begin
          GetNextToken;
          aDecl.AddChild(GetAppendElement(aList));
          aList:=TCSSListElement(CreateElement(CSSListElementClass));
          end;
        aValue:=ParseComponentValue;
        if aValue=nil then break;
        aList.AddChild(aValue);
        end;
      // Store the end of the value: the significant token before the current
      // terminator (';', '}', EOF or !important), skipping trailing whitespace.
      aDecl.EndRow:=FPrevSigTokenEndRow;
      aDecl.EndCol:=FPrevSigTokenEndCol;
      if CurrentToken=ctkImportant then
        begin
        GetNextToken;
        aDecl.IsImportant:=True;
        end;
      end
    else
      begin
      // Store the end of the value.
      aDecl.EndRow:=FPrevSigTokenEndRow;
      aDecl.EndCol:=FPrevSigTokenEndCol;
      end;
    if FInvalidDeclarationValue then
      begin
      Result:=nil;
      exit;
      end;
    aDecl.AddChild(GetAppendElement(aList));
    aList:=nil;
    Result:=aDecl;
    aDecl:=nil;
  finally
    Scanner.Options:=OldOptions;
    aDecl.Free;
    aList.Free;
  end;
end;

function TCSSParser.ParseCall(aName: TCSSString; IsSelector: boolean
  ): TCSSCallElement;
var
  aCall : TCSSCallElement;
  l : Integer;
  aValue: TCSSElement;
  aList: TCSSListElement;
begin
  aCall:=TCSSCallElement(CreateElement(CSSCallElementClass));
  try
    if (aName='') then
      aName:=CurrentTokenString;
    L:=Length(aName);
    if (L>0) and (aName[L]='(') then
      aName:=Copy(aName,1,L-1);
    aCall.Name:=aName;
    if IsSelector and (CurrentToken=ctkPSEUDOFUNCTION) then
      begin
      GetNextToken;
      SkipWhiteSpace;
      case aName of
      ':not',':is',':where':
        ParseSelectorCommaList(aCall);
      ':has':
        ParseRelationalSelectorCommaList(aCall);
      ':nth-child',':nth-last-child',':nth-of-type',':nth-last-of-type':
        ParseNthChildParams(aCall);
      end;
      end
    else begin
      Consume(ctkFUNCTION);
    end;
    // Call argument list can be empty: mask()
    While not (CurrentToken in [ctkRPARENTHESIS,ctkEOF,ctkSEMICOLON,ctkRBRACE]) do
      begin
      aValue:=ParseComponentValue;
      if aValue=nil then
        begin
        aValue:=TCSSElement(CreateElement(TCSSElement));
        GetNextToken;
        end;
      if (CurrentToken in [ctkCOMMA,ctkRPARENTHESIS,ctkEOF,ctkSEMICOLON,ctkRBRACE]) then
        begin
        aCall.AddArg(aValue);
        if CurrentToken=ctkCOMMA then
          GetNextToken;
        end
      else
        begin
        // e.g. repeat(5, 1em 2em)  the "1em 2em" is one arg
        aList:=TCSSListElement(CreateElement(CSSListElementClass));
        aList.AddChild(aValue);
        aCall.AddArg(aList);
        repeat
          aValue:=ParseComponentValue;
          if aValue=nil then
            begin
            aValue:=TCSSElement(CreateElement(TCSSElement));
            GetNextToken;
            end;
          aList.AddChild(aValue);
        until CurrentToken in [ctkCOMMA,ctkRPARENTHESIS,ctkEOF,ctkSEMICOLON,ctkRBRACE];
        if CurrentToken=ctkCOMMA then
          GetNextToken;
        end;
      end;
    if CurrentToken<>ctkRPARENTHESIS then
      begin
      FInvalidDeclarationValue:=True;
      DoWarn(SErrUnexpectedEndOfFile,[aName]);
      end
    else
      GetNextToken;
    Result:=aCall;
    aCall:=nil;
  finally
    aCall.Free;
  end;
end;

procedure TCSSParser.ParseSelectorCommaList(aCall: TCSSCallElement);
var
  El: TCSSElement;
begin
  while not (CurrentToken in [ctkEOF,ctkRBRACKET,ctkRBRACE,ctkRPARENTHESIS]) do
    begin
    El:=ParseSelector;
    if El=nil then exit;
    aCall.AddArg(El);
    if CurrentToken<>ctkCOMMA then
      exit;
    GetNextToken;
    SkipWhiteSpace;
  end;
end;

procedure TCSSParser.ParseRelationalSelectorCommaList(aCall: TCSSCallElement);
var
  El: TCSSElement;
  aToken: TCSSToken;
  IsUnary: Boolean;
  Unary: TCSSUnaryElement;
begin
  while not (CurrentToken in [ctkEOF,ctkRBRACKET,ctkRBRACE,ctkRPARENTHESIS]) do
    begin
    IsUnary:=false;
    aToken:=CurrentToken;
    if aToken in [ctkGT,ctkPLUS,ctkTILDE] then
      begin
      IsUnary:=true;
      GetNextToken;
      end;
    El:=ParseSelector;
    if El=nil then exit;
    if IsUnary then
      begin
      Unary:=TCSSUnaryElement(CreateElement(CSSUnaryElementClass));
      aCall.AddArg(Unary);
      Unary.Right:=El;
      Unary.Operation:=TokenToUnaryOperation(aToken);
      end
    else
      aCall.AddArg(El);
    if CurrentToken<>ctkCOMMA then
      exit;
    GetNextToken;
  end;
end;

procedure TCSSParser.ParseNthChildParams(aCall: TCSSCallElement);
// Examples:
// odd
// even
//  n
//  +n
// -2n
// 2n+1
//  even of :not(:hidden)
// 2n+1 of [:not(display=none)]
var
  aUnary: TCSSUnaryElement;
  IdentEl: TCSSIdentifierElement;
begin
  case CurrentToken of
  ctkIDENTIFIER:
    case lowercase(CurrentTokenString) of
    'odd','even','n':
      aCall.AddArg(ParseIdentifier);
    '-n':
      begin
        aUnary:=TCSSUnaryElement(CreateElement(CSSUnaryElementClass));
        aCall.AddArg(aUnary);
        aUnary.Operation:=uoMinus;
        IdentEl:=TCSSIdentifierElement(CreateElement(CSSIdentifierElementClass));
        aUnary.Right:=IdentEl;
        IdentEl.Value:='n';
        GetNextToken;
      end;
    else
      DoWarnExpectedButGot('An+B');
      aCall.AddArg(ParseIdentifier);
      exit;
    end;
  ctkINTEGER:
    begin
    aCall.AddArg(ParseInteger);
    // optional 'n': with it An+B, without it a plain integer B (a=0), e.g. nth-child(2)
    if (CurrentToken=ctkIDENTIFIER) then
      begin
      if (lowercase(CurrentTokenString)<>'n') then
        begin
        DoWarnExpectedButGot('An+B');
        exit;
        end;
      aCall.AddArg(ParseIdentifier);
      end;
    end;
  else
    DoWarnExpectedButGot('An+B');
    exit;
  end;

  if CurrentToken in [ctkMINUS,ctkPLUS] then
    aCall.AddArg(ParseUnary);
  if (CurrentToken=ctkIDENTIFIER) and SameText(CurrentTokenString,'of') then
    begin
    aCall.AddArg(ParseIdentifier);
    SkipWhiteSpace;
    aCall.AddArg(ParseSelector);
    SkipWhiteSpace;
    end;
end;

function TCSSParser.ParseString: TCSSElement;
var
  aStr: TCSSStringElement;
  aValue: TCSSString;
begin
  aValue:=CurrentTokenString;
  aStr:=TCSSStringElement(CreateElement(CSSStringElementClass));
  try
    aStr.Value:=aValue;
    Consume(ctkSTRING);
    Result:=aStr;
    aStr:=nil;
  finally
    aStr.Free;
  end;
end;

function TCSSParser.ParseHashValue: TCSSElement;
var
  aHash: TCSSHashValueElement;
  aValue: TCSSString;
begin
  aValue:=CurrentTokenString;
  system.delete(aValue,1,1);
  aHash:=TCSSHashValueElement(CreateElement(CSSHashValueElementClass));
  try
    aHash.Value:=aValue;
    Consume(ctkHASH); // e.g. #rrggbb
    Result:=aHash;
    aHash:=nil;
  finally
    aHash.Free;
  end;
end;

function TCSSParser.ParseUnicodeRange: TCSSElement;
Var
  aValue : TCSSString;
  aRange : TCSSUnicodeRangeElement;

begin
  aValue:=CurrentTokenString;
  aRange:=TCSSUnicodeRangeElement(CreateElement(CSSUnicodeRangeElementClass));
  try
    Consume(ctkUnicodeRange);
    aRange.Value:=aValue;
    Result:=aRange;
    aRange:=nil;
  finally
    aRange.Free;
  end;
end;

function TCSSParser.ParseArray(aPrefix: TCSSElement; AllowRules: boolean): TCSSElement;

Var
  aEl : TCSSElement;
  aArray : TCSSArrayElement;

begin
  Result:=Nil;
  aArray:=TCSSArrayElement(CreateElement(CSSArrayElementClass));
  try
    aArray.Prefix:=aPrefix;
    Consume(ctkLBRACKET);
    if CurrentToken in [ctkEOF, ctkSEMICOLON, ctkRBRACE] then
      begin
      FInvalidDeclarationValue:=True;
      DoWarn(SErrUnexpectedEndOfFile,['[']);
      Result:=aArray;
      aArray:=nil;
      exit;
      end;
    While not (CurrentToken in [ctkRBRACKET, ctkEOF, ctkSEMICOLON, ctkRBRACE]) do
      begin
      aEl:=ParseComponentValueList(AllowRules);
      aArray.AddChild(aEl);
      end;
    if CurrentToken=ctkRBRACKET then
      begin
      GetNextToken;
      Result:=aArray;
      aArray:=nil;
      end
    else
      begin
      FInvalidDeclarationValue:=True;
      DoWarn(SErrUnexpectedEndOfFile,['[']);
      Result:=aArray;
      aArray:=nil;
      end;
  finally
    aArray.Free;
  end;
end;

end.

