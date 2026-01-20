{
 *****************************************************************************
  This file is part of the SynEditSpellChecker package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit SynSpellCheckWordBreaker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazEditTypes, LazEditMiscProcs, SynSpellDictionary, LazClasses, LazUTF8;

type

  { TSynSpellWordBreaker }

  TSynSpellWordBreaker = class
  strict private
    FLineLen: integer;
    FLineStr: string;
    FLine: PChar;
    FOnChanged: TNotifyEvent;
    FRunPos: IntPos;
  protected
    procedure DoChanged;
    function DoNextWord(ASearchStartPos: IntIdx; out AFoundPos: IntIdx; out AFoundLen: integer): Boolean; virtual; abstract;

    property Line: PChar read FLine;
    property LineLen: integer read FLineLen;
  public
    function GetReady: boolean; virtual;
    function IsReady: boolean; virtual;
    procedure SetLine(const ALine: String);
    procedure SetLine(ALine: PChar; ALineLen: Integer);
    function NextWord(out AFoundPos: IntPos; out AFoundLen: integer): Boolean;
    function WordAt(ASearchStartPos: IntPos; out AFoundPos: IntPos; out AFoundLen: integer): Boolean;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;
  TSynSpellWordBreakerClass = class of TSynSpellWordBreaker;

  { TSynSpellWordBreakerSimpleUtf8 }

  TSynSpellWordBreakerSimpleUtf8 = class(TSynSpellWordBreaker)
  private
    FSpecialLetters: String;
    procedure SetSpecialLetters(AValue: String);
  protected
    function NextWordBegin(AStartX: integer): integer;
    function CurrentWordEnd(AStartX: integer): integer;
    function DoNextWord(ASearchStartPos: IntIdx; out AFoundPos: IntIdx; out AFoundLen: integer
      ): Boolean; override;
  public
    property SpecialLetters: String read FSpecialLetters write SetSpecialLetters;
  end;

  TSynSpellWordErrorState = (
    esOk,
    esWrong,
    esIgnored  // not checked, assumed ok
  );

  {$TYPEINFO ON}
  { TSynSpellWordChecker }

  TSynSpellWordChecker = class
  strict private
    FLineStr: string;
    FOnChanged: TNotifyEvent;
    FWordLen: integer;
    FWord: PChar;
    FNextSearchPos, FNextPartPos: IntIdx;
    FNextPartLen: integer;
    FNextPartErr: TSynSpellWordErrorState;
    FNextPartResult: Boolean;
    FReachedEnd: boolean;
    FSearchPos, FErrorPos: IntIdx;
    FErrorLen: integer;
    FSynSpellDictionary: TSynSpellDictionary;
    procedure DoDictChanged(Sender: TObject);
    procedure SetSynSpellDictionary(AValue: TSynSpellDictionary);
  protected
    procedure DoChanged;
    procedure InitWord; virtual;
    function DoNextWordBreak(
      var ASearchStartPos: IntIdx;
      out AFoundPos: IntIdx; out AFoundLen: integer;
      out AnErrorState: TSynSpellWordErrorState
    ): boolean; // True: more word-parts available
    virtual; abstract;
    function StartScanWord(out HasError: Boolean): Boolean;
    function ScanWord(out HasError: Boolean): Boolean;

    property Word: PChar read FWord;
    property WordLen: integer read FWordLen;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(AnOther: TSynSpellWordChecker); virtual;
    function GetReady: boolean; virtual;
    function IsReady: boolean; virtual;
    procedure SetWord(const ALine: String; AWordStart: IntPos; AWordLen: Integer);
    procedure SetWord(AWord: PChar; AWordLen: Integer);

    function CheckWord(const ALine: String; AWordStart: IntPos; AWordLen: Integer): boolean;
    function CheckWord(AWord: PChar; AWordLen: Integer): boolean;
    // GetNextError: True = Has more errors
    function GetNextError(out AFoundPos: IntIdx; out AFoundLen: integer; ACombineAdjacent: boolean): boolean;

    function GetBoundaryAt(ASearchPos: IntIdx; out AFoundPos: IntIdx; out AFoundLen: integer): boolean;

    function GetPartSuggestions(AStartPos: IntIdx; ALen: integer; AMaxSuggestions: integer = MaxInt): TStringArray; virtual;
    function GetWordSuggestions(AStartPos: IntIdx; ALen: integer; AMaxSuggestions: integer = MaxInt): TStringArray; virtual;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property SynSpellDictionary: TSynSpellDictionary read FSynSpellDictionary write SetSynSpellDictionary;
  end;
  TSynSpellWordCheckerClass = class of TSynSpellWordChecker;

  { TSynSpellWordCheckerSimple - Check full words }

  TSynSpellWordCheckerSimple = class(TSynSpellWordChecker)
  protected
    function DoNextWordBreak(var ASearchStartPos: IntIdx; out AFoundPos: IntIdx; out
      AFoundLen: integer; out AnErrorState: TSynSpellWordErrorState): boolean; override;
  end;


  TSynSpellWordCheckerSourceCodeOption = (
    // whole word options
    coAllowWordWithXCaps,           // Allow WOrld as World
    coAllowIgnoreWordWithXCaps,     // Allow CAt to be ignored like Cat
    coAllowMultiTokenWord,         // Allow ReStoRation as word (otherwise must be matched as tokens)
    coTreatPrefixedWordAsToken,    // AnOption, after removing "An", "Option" will be treated as token
                                   // affects IgnoreLen and Allow...XCaps
    // Token options
    coAllowTokenWithXCaps,        // Allow HelloWOrld as HelloWorld
    coAllowIgnoreTokenWithXCaps,  // Allow XYz in FooXYz or XYzFoo to be ignored like Xyz or xyz

    // Prefix options
    coIgnoredUpperStartMustMatchCase, // the letter after an ignored upper must be an upper letter
    coIgnoredLowerStartMustMatchCase,
    coIgnoreUpperArticle,              // Ignore A/An/AN
    coIgnoreUpperArticleMustMatchCase,
    coIgnoreLowerArticle,              // Ignore a/an
    coIgnoreLowerArticleMustMatchCase
  );
  TSynSpellWordCheckerSourceCodeOptions = set of TSynSpellWordCheckerSourceCodeOption;

  { TSynSpellWordCheckerSourceCode }

  (* TSynSpellWordCheckerSourceCode
     ** Words may pass as
     - WORD:  the entire word
     - TOKEN: List of tokens, all tokens must pass.
              Words are divided into tokens at EACH change from lower to UPPER
       e.g.: FooBarABC
       =>    Foo Bar ABC
     - PART-TOKEN: If TOKEN does not pass, it may be further divided
     -- MIXED-PART (divided BEFORE the last upper)
        LEAD + Mixed: Upper LEAD, and Mixed (Lower with one capital)
        e.g.: FOOBar
        =>    FOO Bar
     -- CASE-PART (divided AFTER the last upper)
        UPPER and LOWER: change from upper to lower
        e.g.: NEVERagain
        => never again

     TOKEN are allowed a single upper letter at their start.
     TOKEN with XCaps (multiple upper letters at start) can be
     - coAllowIgnoreTokenWithXCaps: ignored if they are short
     - coAllowTokenWithXCaps: checked in the dictionary
     Otherwise, they must match as their respective parts

     ** Ignoring short words/tokens/parts
     - Each Word, Token, Part can be ignored if it is shorter (or equal length) than
       the configured threshold.
     - Short parts are ONLY ignored if the other part of the token has a required
       minimum length.
     - Short parts are ONLY ignored if the other part is in the dictionary
       (only one of the two parts can be ignored / otherwise it would be same as
        ignoring a longer TOKEN)
     - A short part at the very start is *NOT* ignored, if
       - it is in "IgnoreLettersAtStart"
       - AND has a "...MustMatchCase" requirement, which is *NOT* fulfilled.

     - First token and ignoring short parts:
       - The very first TOKEN may be all lowercase, then
         it is not broken into parts
         it is ignored according to "IgnoreLowerStart"




     ** Prefix letters / A/An article
     May be removed from
     - The whole word:
       *IF* the remaining whole word is at least "IgnoreAtStartMinRemainderLen" long.
     - The first sub-token:
       *IF* the remaining sub-token is at least "IgnoreAtStartMinRemainderLen" long.
       (In that case the remaining word is also long enough and will also be tested)


  *)

  TSynSpellWordCheckerSourceCode = class(TSynSpellWordChecker)
  public type

    { TSynSpellWCheckerSrcPartConstraints }

    TSynSpellWCheckerSrcPartConstraints = class
    private
      FMinLen: integer;
      FIgnoreLen: integer;
      FIgnoreRemainderLen: integer;
      FOwner: TSynSpellWordCheckerSourceCode;
      procedure SetIgnoreLen(AValue: integer);
      procedure SetIgnoreRemainderLen(AValue: integer);
      procedure SetMinLen(AValue: integer);
    public
      constructor Create(AnOwner: TSynSpellWordCheckerSourceCode);
      procedure Assign(AnOther: TSynSpellWCheckerSrcPartConstraints);
      function HasLen(AStart, AnEnd: IntIdx): Boolean;
      function CanIgnore(AStart, AnEnd, AnOtherEnd: IntIdx): Boolean;
      procedure Init(AMinLen, AnIgnoreLen, AnIgnoreRemainderLen: integer);
    published
      property MinLen: integer read FMinLen write SetMinLen default 0;
      property IgnoreLen: integer read FIgnoreLen write SetIgnoreLen default 3;
      property IgnoreRemainderLen: integer read FIgnoreRemainderLen write SetIgnoreRemainderLen default 5; // required len of opposite part
    end;

    // 2nd part of token
    TSynSpellWCheckerSrcPartConstraints2 = class(TSynSpellWCheckerSrcPartConstraints)
    public
      function CanIgnore(AStart, AnEnd, AnOtherStart: IntIdx): Boolean; reintroduce;
    end;
  public const
    DEFAULT_OPTIONS = TSynSpellWordCheckerSourceCodeOptions([
      coAllowIgnoreWordWithXCaps,
      coAllowMultiTokenWord,
      coAllowIgnoreTokenWithXCaps,
      coIgnoredUpperStartMustMatchCase, // the letter after an ignored upper must be an upper letter
      coIgnoreUpperArticle,              // Ignore A/An/AN
      coIgnoreUpperArticleMustMatchCase,
      coIgnoreLowerArticle               // Ignore a/an
    ]);
  private
    FIgnoreShortWord, FIgnoreShortToken, FIgnoreLowerStart: integer;
    FLeadConstr: TSynSpellWCheckerSrcPartConstraints;
    FMixedConstr: TSynSpellWCheckerSrcPartConstraints2;
    FUpperConstr: TSynSpellWCheckerSrcPartConstraints;
    FLowerConstr: TSynSpellWCheckerSrcPartConstraints2;

    FIgnoreAtStartMinRemainderLen: integer;
    FIgnoreLettersAtStart: String;
    FIgnoreLettersAtStartSet: set of char;
    FOptions: TSynSpellWordCheckerSourceCodeOptions;

    FLowStart, FLowEnd: IntIdx;
    FLowErrState: TSynSpellWordErrorState;
    FSpecialLowerLetters: String;
    FSpecialUpperLetters: String;

    procedure SetIgnoreAtStartMinRemainderLen(AValue: integer);
    procedure SetIgnoreLettersAtStart(AValue: String);
    procedure SetIgnoreLowerStart(AValue: integer);
    procedure SetIgnoreShortToken(AValue: integer);
    procedure SetIgnoreShortWord(AValue: integer);
    procedure SetOptions(AValue: TSynSpellWordCheckerSourceCodeOptions);
    procedure SetPartLeadConstraints(AValue: TSynSpellWCheckerSrcPartConstraints);
    procedure SetPartMixedConstraints(AValue: TSynSpellWCheckerSrcPartConstraints2);
    procedure SetPartUpperConstraints(AValue: TSynSpellWCheckerSrcPartConstraints);
    procedure SetPartLowerConstraints(AValue: TSynSpellWCheckerSrcPartConstraints2);
    procedure SetSpecialLowerLetters(AValue: String);
    procedure SetSpecialUpperLetters(AValue: String);

    function CheckSuggestion(const ASuggestion: String): boolean;
    function CurrentLowerEnd(AStartX: integer): integer;
    function CurrentUpperEnd(AStartX: integer): integer;
    function IsLower(AStartX: integer): Boolean;
    function IsLower(AStartX: integer; out AByteLen: integer): Boolean;
    function IsUpper(AStartX: integer): Boolean;
    function IsUpper(AStartX: integer; out AByteLen: integer): Boolean;
  protected
    procedure InitWord; override;
    function DoNextWordBreak(var ASearchStartPos: IntIdx; out AFoundPos: IntIdx; out
      AFoundLen: integer; out AnErrorState: TSynSpellWordErrorState): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(AnOther: TSynSpellWordChecker); override;

    function GetPartSuggestions(AStartPos: IntIdx; ALen: integer; AMaxSuggestions: integer = MaxInt
      ): TStringArray; override;

    procedure SetAllPartMinLen(AMinLen: integer);
    procedure SetAllPartIgnoreLen(AnIgnoreLen: integer);
    procedure SetAllPartIgnoreRequiredOther(AMinRequiredOther: integer);

  published
    property SpecialUpperLetters: String read FSpecialUpperLetters write SetSpecialUpperLetters;
    property SpecialLowerLetters: String read FSpecialLowerLetters write SetSpecialLowerLetters;

    property IgnoreShortWord: integer  read FIgnoreShortWord  write SetIgnoreShortWord  default 3;
    property IgnoreShortToken: integer read FIgnoreShortToken write SetIgnoreShortToken default 3;
    property IgnoreLowerStart: integer read FIgnoreLowerStart write SetIgnoreLowerStart default 4;

    property PartLeadConstraints: TSynSpellWCheckerSrcPartConstraints read FLeadConstr write SetPartLeadConstraints;
    property PartMixedConstraints: TSynSpellWCheckerSrcPartConstraints2 read FMixedConstr write SetPartMixedConstraints;
    property PartUpperConstraints: TSynSpellWCheckerSrcPartConstraints read FUpperConstr write SetPartUpperConstraints;
    property PartLowerConstraints: TSynSpellWCheckerSrcPartConstraints2 read FLowerConstr write SetPartLowerConstraints;

    property Options: TSynSpellWordCheckerSourceCodeOptions read FOptions write SetOptions default DEFAULT_OPTIONS;
    property IgnoreLettersAtStart: String read FIgnoreLettersAtStart write SetIgnoreLettersAtStart;
    property IgnoreAtStartMinRemainderLen: integer read FIgnoreAtStartMinRemainderLen write SetIgnoreAtStartMinRemainderLen default 2;
  end;

implementation

function IsUtf8CharFromList(const ALine: PChar; ALineLen: integer; X: Integer; out ByteLen: integer; const AnUtf8Chars: String): Boolean;
var
  x2: Integer;
  s: string;
begin
  x2 := x;
  if (x2 < ALineLen) and not (ALine[x2] in [$80..#$BF]) then
    inc(x2);
  while (x2 < ALineLen) and (ALine[x2] in [$80..#$BF]) do
    inc(x2);
  ByteLen := x2 - X;
  SetString(s, @ALine[x], ByteLen);
  Result := (x2 > x) and (pos(s, AnUtf8Chars) > 0);
end;

function IsUtf8CharFromList(const ALine: PChar; ALineLen: integer; X: Integer; const AnUtf8Chars: String): Boolean;
var
  L: integer;
begin
  Result := IsUtf8CharFromList(ALine, ALineLen, X, L, AnUtf8Chars);
end;




{ TSynSpellWordBreaker }

procedure TSynSpellWordBreaker.DoChanged;
begin
  if FOnChanged <> nil then
    FOnChanged(Self);
end;

function TSynSpellWordBreaker.GetReady: boolean;
begin
  Result := IsReady;
end;

function TSynSpellWordBreaker.IsReady: boolean;
begin
  Result := True;
end;

procedure TSynSpellWordBreaker.SetLine(const ALine: String);
begin
  FLineStr := ALine;
  FLine := PChar(FLineStr);
  FLineLen := Length(FLineStr);
  FRunPos := 0;
end;

procedure TSynSpellWordBreaker.SetLine(ALine: PChar; ALineLen: Integer);
begin
  FLine := ALine;
  FLineLen := ALineLen;
  FRunPos := 0;
end;

function TSynSpellWordBreaker.NextWord(out AFoundPos: IntPos; out AFoundLen: integer): Boolean;
begin
  if FRunPos >= FLineLen then
    exit(False);
  Result := DoNextWord(FRunPos, FRunPos, AFoundLen);
  AFoundPos := ToPos(FRunPos);
  if Result then
    FRunPos := FRunPos + AFoundLen;
end;

function TSynSpellWordBreaker.WordAt(ASearchStartPos: IntPos; out AFoundPos: IntPos; out
  AFoundLen: integer): Boolean;
var
  r: IntIdx;
begin
  r := 0;
  ASearchStartPos := ToIdx(ASearchStartPos);
  while DoNextWord(r, r, AFoundLen) do begin
    if (r >= ASearchStartPos) then
      break;
    Result := (r + AFoundLen > ASearchStartPos);
    if Result then begin
      AFoundPos := ToPos(r);
      exit;
    end;
    r := r + AFoundLen;
  end;
  Result := False;
end;

{ TSynSpellWordBreakerSimpleUtf8 }

procedure TSynSpellWordBreakerSimpleUtf8.SetSpecialLetters(AValue: String);
begin
  if FSpecialLetters = AValue then Exit;
  FSpecialLetters := AValue;
  DoChanged;
end;

function TSynSpellWordBreakerSimpleUtf8.NextWordBegin(AStartX: integer): integer;
begin
  Result := AStartX;
  while (Result < LineLen) and
        not ( (Line[Result] in ['A'..'Z', 'a'..'z'])
              or
              ( (ord(Line[Result]) >= $80) and IsUtf8CharFromList(Line, LineLen, Result, FSpecialLetters) )
            )
  do
    inc(Result);
end;

function TSynSpellWordBreakerSimpleUtf8.CurrentWordEnd(AStartX: integer): integer;
begin
  Result := AStartX;
  while (Result < LineLen) and
        ( (Line[Result] in ['A'..'Z', 'a'..'z', #$80..#$BF])
          or
          ( (ord(Line[Result]) >= $80) and IsUtf8CharFromList(Line, LineLen, Result, FSpecialLetters) )
        )
  do
    inc(Result);
end;

function TSynSpellWordBreakerSimpleUtf8.DoNextWord(ASearchStartPos: IntIdx; out
  AFoundPos: IntIdx; out AFoundLen: integer): Boolean;
begin
  Result := ASearchStartPos < LineLen;
  if not Result then
    exit;

  AFoundPos := NextWordBegin(ASearchStartPos);
  Result := AFoundPos < LineLen;
  if not Result then
    exit;

  AFoundLen := CurrentWordEnd(AFoundPos) - AFoundPos;
  Result := AFoundLen > 0;
end;

{ TSynSpellWordChecker }

procedure TSynSpellWordChecker.SetSynSpellDictionary(AValue: TSynSpellDictionary);
begin
  if FSynSpellDictionary = AValue then Exit;
  if FSynSpellDictionary <> nil then begin
    FSynSpellDictionary.UnregisterChangedHandler(@DoDictChanged);
    FSynSpellDictionary.ReleaseReference;
  end;
  FSynSpellDictionary := AValue;
  if FSynSpellDictionary <> nil then begin
    FSynSpellDictionary.AddReference;
    FSynSpellDictionary.RegisterChangedHandler(@DoDictChanged);
  end;
  DoChanged;
end;

procedure TSynSpellWordChecker.DoDictChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TSynSpellWordChecker.DoChanged;
begin
  if FOnChanged <> nil then
    FOnChanged(Self);
end;

procedure TSynSpellWordChecker.InitWord;
begin
  //
end;

function TSynSpellWordChecker.StartScanWord(out HasError: Boolean): Boolean;
begin
  FSearchPos := 0;
  InitWord;
  if WordLen = 0 then begin
    Result := False;
    HasError := False;
    exit;
  end;

  FNextSearchPos := FSearchPos;
  FNextPartResult := DoNextWordBreak(FNextSearchPos, FNextPartPos, FNextPartLen, FNextPartErr);
  Result := ScanWord(HasError);
end;

function TSynSpellWordChecker.ScanWord(out HasError: Boolean): Boolean;
var
  IgnPos: IntIdx;
  ErrEndPos: Integer;
begin
  IgnPos := -1;
  FErrorPos := -1;

  while FErrorPos < 0 do begin
    case FNextPartErr of
      esOk: begin
          IgnPos := -1;
        end;
      esWrong: begin
          FErrorPos := FSearchPos;
          //if IgnPos >= 0
          //then FErrorPos := IgnPos
          //else FErrorPos := FNextPartPos;
          ErrEndPos := FNextPartPos + FNextPartLen;
        end;
      esIgnored: begin
          IgnPos := FSearchPos;  // any skipped part counts as ignored
        end;
    end;

    Result := FNextPartResult;
    if not Result then
      break;

    FSearchPos := FNextSearchPos;
    FNextPartResult := DoNextWordBreak(FNextSearchPos, FNextPartPos, FNextPartLen, FNextPartErr);
  end;

  //IgnPos := -1;
  while FNextPartResult do begin // scan to next ok (or next error) to include ignored parts
    case FNextPartErr of
      esOk: begin
          //ErrEndPos := FSearchPos;
          break;
        end;
      esWrong: begin
          ErrEndPos := FSearchPos;
          break;
        end;
      esIgnored: begin
          //IgnPos := p;
          ErrEndPos := FNextPartPos + FNextPartLen;
        end;
    end;

    FSearchPos := FNextSearchPos;
    FNextPartResult := DoNextWordBreak(FNextSearchPos, FNextPartPos, FNextPartLen, FNextPartErr);
  end;

  HasError := FErrorPos >= 0;
  if HasError then
    FErrorLen := ErrEndPos - FErrorPos;
end;

constructor TSynSpellWordChecker.Create;
begin
  inherited Create;
end;

destructor TSynSpellWordChecker.Destroy;
begin
  inherited Destroy;
  if FSynSpellDictionary <> nil then begin
    FSynSpellDictionary.UnregisterChangedHandler(@DoDictChanged);
    FSynSpellDictionary.ReleaseReference;
    FSynSpellDictionary := nil;
  end;
end;

procedure TSynSpellWordChecker.Assign(AnOther: TSynSpellWordChecker);
begin
  //
end;

function TSynSpellWordChecker.GetReady: boolean;
begin
  if FSynSpellDictionary <> nil then
    FSynSpellDictionary.Load;
  Result := IsReady;
end;

function TSynSpellWordChecker.IsReady: boolean;
begin
  Result := (FSynSpellDictionary <> nil) and FSynSpellDictionary.IsLoaded;
end;

procedure TSynSpellWordChecker.SetWord(const ALine: String; AWordStart: IntPos; AWordLen: Integer
  );
begin
  FLineStr := ALine;
  SetWord(@FLineStr[AWordStart], AWordLen);
end;

procedure TSynSpellWordChecker.SetWord(AWord: PChar; AWordLen: Integer);
begin
  FWord := AWord;
  FWordLen := AWordLen;
end;

function TSynSpellWordChecker.CheckWord(const ALine: String; AWordStart: IntPos;
  AWordLen: Integer): boolean;
begin
  FLineStr := ALine;
  Result := CheckWord(@FLineStr[AWordStart], AWordLen);
end;

function TSynSpellWordChecker.CheckWord(AWord: PChar; AWordLen: Integer): boolean;
begin
  SetWord(AWord, AWordLen);
  FReachedEnd := not StartScanWord(Result);
  Result := not Result;
end;

function TSynSpellWordChecker.GetNextError(out AFoundPos: IntIdx; out AFoundLen: integer;
  ACombineAdjacent: boolean): boolean;
begin
  AFoundPos := FErrorPos;
  AFoundLen := FErrorLen;

  if FReachedEnd then begin
    Result := False;
  end
  else begin
    FReachedEnd := not ScanWord(Result);

    if ACombineAdjacent then begin
      while Result and (FErrorPos = AFoundPos+AFoundLen) do begin
        AFoundLen := FErrorPos + FErrorLen - AFoundPos;
        if FReachedEnd then
          break;
        FReachedEnd := not ScanWord(Result);
      end;
    end;
  end;
end;

function TSynSpellWordChecker.GetBoundaryAt(ASearchPos: IntIdx; out AFoundPos: IntIdx; out
  AFoundLen: integer): boolean;
var
  SrcPos, LstPos: IntIdx;
  e: TSynSpellWordErrorState;
begin
  SrcPos := 0;
  Result := DoNextWordBreak(SrcPos, AFoundPos, AFoundLen, e);
  repeat
    LstPos := SrcPos;
    if (ASearchPos >= AFoundPos) and (ASearchPos < AFoundPos+AFoundLen) then
      exit(True);
    if (ASearchPos >= LstPos) and (ASearchPos < SrcPos) then begin
      AFoundPos := LstPos;
      AFoundLen := SrcPos - LstPos;
      exit(True);
    end;

    if ASearchPos < SrcPos then
      exit(False);

    if not Result then break;
    Result := DoNextWordBreak(SrcPos, AFoundPos, AFoundLen, e);
  until False;
  Result := False;
end;

function TSynSpellWordChecker.GetPartSuggestions(AStartPos: IntIdx; ALen: integer;
  AMaxSuggestions: integer): TStringArray;
begin
  Result := FSynSpellDictionary.GetSuggestions(Word+AStartPos, ALen, AMaxSuggestions);
end;

function TSynSpellWordChecker.GetWordSuggestions(AStartPos: IntIdx; ALen: integer;
  AMaxSuggestions: integer): TStringArray;
var
  s1, s2: string;
  i: Integer;
begin
  Result := GetPartSuggestions(AStartPos, ALen, AMaxSuggestions);
  SetString(s1, Word, AStartPos);
  SetString(s2, Word+AStartPos+ALen, WordLen-AStartPos-ALen);
  for i := 0 to Length(Result) - 1 do
    Result[i] := s1 + Result[i] + s2;
end;

{ TSynSpellWordCheckerSimple }

function TSynSpellWordCheckerSimple.DoNextWordBreak(var ASearchStartPos: IntIdx; out
  AFoundPos: IntIdx; out AFoundLen: integer; out AnErrorState: TSynSpellWordErrorState): boolean;
begin
  Result := False;
  AFoundPos := 0;
  AFoundLen := WordLen;
  if SynSpellDictionary.CheckWord(Word, WordLen)
  then AnErrorState := esOk
  else AnErrorState := esWrong;
end;

{ TSynSpellWordCheckerSourceCode }

function TSynSpellWordCheckerSourceCode.CheckSuggestion(const ASuggestion: String): boolean;
var
  i: Integer;
begin
  Result := False;
  i := 1;
  while (i <= Length(ASuggestion)) and
        ( (ASuggestion[i] in ['A'..'Z', 'a'..'z', #$80..#$BF]) or
          ( (ASuggestion[i] in [#$C0..#$FF]) and
            ( IsUtf8CharFromList(Word, WordLen, i-1, FSpecialLowerLetters) or
              IsUtf8CharFromList(Word, WordLen, i-1, FSpecialUpperLetters)
            )
          )
        )
  do
    inc(i);

  Result := i > Length(ASuggestion);
end;

function TSynSpellWordCheckerSourceCode.CurrentLowerEnd(AStartX: integer): integer;
begin
  Result := AStartX;
  while (Result < WordLen) and
        ( (Word[Result] in ['a'..'z', #$80..#$BF])
          or
          ( (ord(Word[Result]) >= $80) and IsUtf8CharFromList(Word, WordLen, Result, FSpecialLowerLetters) )
        )
  do
    inc(Result);
end;

function TSynSpellWordCheckerSourceCode.CurrentUpperEnd(AStartX: integer): integer;
begin
  Result := AStartX;
  while (Result < WordLen) and
        ( (Word[Result] in ['A'..'Z', #$80..#$BF])
          or
          ( (ord(Word[Result]) >= $80) and IsUtf8CharFromList(Word, WordLen, Result, FSpecialUpperLetters) )
        )
  do
    inc(Result);
end;

function TSynSpellWordCheckerSourceCode.IsLower(AStartX: integer): Boolean;
begin
  Result := (Word[AStartX] in ['a'..'z']) or
            ( (ord(Word[AStartX]) >= $80) and IsUtf8CharFromList(Word, WordLen, AStartX, FSpecialLowerLetters) );
end;

function TSynSpellWordCheckerSourceCode.IsLower(AStartX: integer; out AByteLen: integer): Boolean;
begin
  AByteLen := 1;
  Result := (Word[AStartX] in ['a'..'z']) or
            ( (ord(Word[AStartX]) >= $80) and IsUtf8CharFromList(Word, WordLen, AStartX, AByteLen, FSpecialLowerLetters) );
end;

function TSynSpellWordCheckerSourceCode.IsUpper(AStartX: integer): Boolean;
begin
  Result := (Word[AStartX] in ['A'..'Z']) or
            ( (ord(Word[AStartX]) >= $80) and IsUtf8CharFromList(Word, WordLen, AStartX, FSpecialUpperLetters) );
end;

function TSynSpellWordCheckerSourceCode.IsUpper(AStartX: integer; out AByteLen: integer): Boolean;
begin
  AByteLen := 1;
  Result := (Word[AStartX] in ['A'..'Z']) or
            ( (ord(Word[AStartX]) >= $80) and IsUtf8CharFromList(Word, WordLen, AStartX, AByteLen, FSpecialUpperLetters) );
end;

procedure TSynSpellWordCheckerSourceCode.SetIgnoreAtStartMinRemainderLen(AValue: integer);
begin
  if FIgnoreAtStartMinRemainderLen = AValue then Exit;
  FIgnoreAtStartMinRemainderLen := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetIgnoreLettersAtStart(AValue: String);
var
  c: Char;
begin
  if FIgnoreLettersAtStart = AValue then Exit;
  FIgnoreLettersAtStart := AValue;
  FIgnoreLettersAtStartSet := [];
  for c in FIgnoreLettersAtStart do
    Include(FIgnoreLettersAtStartSet, c);
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetIgnoreLowerStart(AValue: integer);
begin
  if FIgnoreLowerStart = AValue then Exit;
  FIgnoreLowerStart := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetIgnoreShortToken(AValue: integer);
begin
  if FIgnoreShortToken = AValue then Exit;
  FIgnoreShortToken := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetIgnoreShortWord(AValue: integer);
begin
  if FIgnoreShortWord = AValue then Exit;
  FIgnoreShortWord := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetOptions(AValue: TSynSpellWordCheckerSourceCodeOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetPartLeadConstraints(
  AValue: TSynSpellWCheckerSrcPartConstraints);
begin
  FLeadConstr.Assign(AValue);
end;

procedure TSynSpellWordCheckerSourceCode.SetPartLowerConstraints(
  AValue: TSynSpellWCheckerSrcPartConstraints2);
begin
  FLowerConstr.Assign(AValue);
end;

procedure TSynSpellWordCheckerSourceCode.SetPartMixedConstraints(
  AValue: TSynSpellWCheckerSrcPartConstraints2);
begin
  FMixedConstr.Assign(AValue);
end;

procedure TSynSpellWordCheckerSourceCode.SetPartUpperConstraints(
  AValue: TSynSpellWCheckerSrcPartConstraints);
begin
  FUpperConstr.Assign(AValue);
end;

procedure TSynSpellWordCheckerSourceCode.SetSpecialLowerLetters(AValue: String);
begin
  if FSpecialLowerLetters = AValue then Exit;
  FSpecialLowerLetters := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.SetSpecialUpperLetters(AValue: String);
begin
  if FSpecialUpperLetters = AValue then Exit;
  FSpecialUpperLetters := AValue;
  DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.InitWord;
begin
  FLowErrState := esIgnored;
end;

function TSynSpellWordCheckerSourceCode.DoNextWordBreak(var ASearchStartPos: IntIdx; out
  AFoundPos: IntIdx; out AFoundLen: integer; out AnErrorState: TSynSpellWordErrorState): boolean;

  procedure SetReturnValues(AnErr: TSynSpellWordErrorState; APos, AnEnd: IntIdx; ANextSearch: IntIdx = -1); inline;
  begin
    AnErrorState := AnErr;
    AFoundPos := APos;
    AFoundLen := AnEnd - APos;
    if ANextSearch < 0 then
      ASearchStartPos := AnEnd
    else
      ASearchStartPos := ANextSearch;
  end;

  var
    LastSpellState: TSynSpellWordErrorState;

  function CheckSpelling(AStart, AnEnd: IntIdx; AnIgnoreLen: integer; AnAllowXCaps, AnAllowIgnoreWithXCaps: Boolean): boolean;
  var
    CLen: SizeInt;
    l: integer;
  begin
    LastSpellState := esIgnored;
    CLen := UTF8CodepointCount(@Word[AStart], AnEnd - AStart);
    if CLen <= AnIgnoreLen then begin
      if (CLen <= 1) or AnAllowIgnoreWithXCaps or IsLower(AStart, l) then
        exit(True);
      if IsLower(AStart + l) then
        exit(True);

      // Has XCaps (multiple upper char at start)
      LastSpellState := esWrong;
      if not AnAllowXCaps then
        exit(False);
    end
    else
    if (not AnAllowXCaps) and (IsUpper(AStart, l)) then begin
      LastSpellState := esWrong;
      if not IsLower(AStart + l) then
        exit(False); // Has XCaps
    end;

    LastSpellState := esWrong;
    Result := SynSpellDictionary.CheckWord(@Word[AStart], AnEnd-AStart);
    if Result then
      LastSpellState := esOk;
  end;


var
  UpStart, UpEnd, LeadEnd, LowEnd: IntIdx;
  IgnAtStart, IgnAtStart2, IgnAtStart2Tmp, i: integer;
  AWordIgnoreLen: Integer;
  AWordAllowXCaps, AWordAllowIgnoreXCaps: Boolean;
  NoIgnoreAllowed, IgnForWholeWordOnly: Boolean;
  WordCodePointLen: SizeInt;
  Ok1, Ok2: Boolean;
  UpOk, LowOk, LeadOk, MixOk, IsUp: Boolean;
begin
  case FLowErrState of
    esOk, esWrong: begin
        Result := FLowEnd < WordLen;
        SetReturnValues(FLowErrState, FLowStart, FLowEnd);
        FLowErrState := esIgnored;
        exit;
      end;
  end;
  FLowErrState := esIgnored;


  UpStart := ASearchStartPos;
  UpEnd   := CurrentUpperEnd(UpStart);
  if UpEnd = WordLen
  then LowEnd := WordLen
  else LowEnd := CurrentLowerEnd(UpEnd);

  if LowEnd = ASearchStartPos then begin
    // internal error
    SetReturnValues(esWrong, ASearchStartPos, WordLen);
    Result := False;
    exit;
  end;

  Result := (LowEnd < WordLen);


  IgnAtStart := 0;
  IgnAtStart2 := 0;
  NoIgnoreAllowed := False;
  if ASearchStartPos = 0 then begin
    // check entire word
    AWordIgnoreLen       := IgnoreShortWord;
    AWordAllowXCaps       := (coAllowWordWithXCaps in Options) or (UpEnd = LowEnd);
    AWordAllowIgnoreXCaps := (coAllowIgnoreWordWithXCaps in Options) or (UpEnd = LowEnd);
    if ( (LowEnd = WordLen) or (coAllowMultiTokenWord in Options) ) and
       CheckSpelling(0, WordLen, AWordIgnoreLen, AWordAllowXCaps, AWordAllowIgnoreXCaps)
    then begin
      assert(LastSpellState in [esOk, esIgnored], 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: LastSpellState in [esOk, esIgnored]');
      SetReturnValues(LastSpellState, 0, WordLen);
      Result := False;
      exit;
    end;

    // remove leading letter like T or F
    WordCodePointLen := UTF8CodepointCount(Word, LowEnd);
    IgnForWholeWordOnly := (WordCodePointLen-1 < FIgnoreAtStartMinRemainderLen) or (WordCodePointLen < 2);
    if IgnForWholeWordOnly then
      WordCodePointLen := UTF8CodepointCount(Word, WordLen);
    if (WordCodePointLen >= 2) and (WordCodePointLen-1 >= FIgnoreAtStartMinRemainderLen) then begin
      i := 1;
      if Word[0] in FIgnoreLettersAtStartSet then begin
        repeat // allow early break out
          if Word[0] >= #$80 then begin
            if not IsUtf8CharFromList(Word, WordLen, 0, i, FIgnoreLettersAtStart) then
              break;
            IsUp := IsUpper(0);
          end
          else
            IsUp := Word[0] in ['A'..'Z'];

          if IsUp and (coIgnoredUpperStartMustMatchCase in FOptions) and not IsUpper(i)
          then begin
            NoIgnoreAllowed := True;
            break;
          end
          else
          if (not IsUp) and (coIgnoredLowerStartMustMatchCase in FOptions) and not IsLower(i)
          then begin
            NoIgnoreAllowed := True;
            break;
          end;

          IgnAtStart := i;
        until true;
      end;


      if WordLen >= 2 then begin
        IgnAtStart2Tmp := 0;
        if (coIgnoreLowerArticle in FOptions) and (Word[0] = 'a') then begin
          if (Word[1] = 'n') and (WordLen >= 3) and
             (Word[2] in ['H','h', 'A','a', 'E','e', 'I','i', 'O','o', 'U','u', 'Y','y' {, #$80..$FF}]) and
             (WordCodePointLen-2 >= FIgnoreAtStartMinRemainderLen)
          then begin
            IgnAtStart2 := 2;
            IgnAtStart2Tmp := 1; // Could be "anothing"
          end
          else
          if (not (Word[1] in ['A','a', 'E','e', 'I','i', 'O','o', 'U','u', 'Y','y'])) then
            IgnAtStart2 := 1;

          if (IgnAtStart2 > 0) and (coIgnoreLowerArticleMustMatchCase in FOptions) and not IsLower(IgnAtStart2) then begin
            IgnAtStart2 := 0;
            NoIgnoreAllowed := True;
          end;
        end;
        if (coIgnoreUpperArticle in FOptions) and (Word[0] = 'A') then begin
          if (Word[1] in ['n', 'N']) and (WordLen >= 3) and
             (Word[2] in ['H','h', 'A','a', 'E','e', 'I','i', 'O','o', 'U','u', 'Y','y' {, #$80..$FF}]) and
             (WordCodePointLen-2 >= FIgnoreAtStartMinRemainderLen)
          then begin
            IgnAtStart2 := 2;
            if (Word[1] = 'N') or not (coIgnoreUpperArticleMustMatchCase in FOptions) then
              IgnAtStart2Tmp := 1; // Could be "ANothing"
          end
          else
          if (not (Word[1] in ['A','a', 'E','e', 'I','i', 'O','o', 'U','u', 'Y','y'])) then
            IgnAtStart2 := 1;

          if (IgnAtStart2 > 0) and (coIgnoreUpperArticleMustMatchCase in FOptions) and not IsUpper(IgnAtStart2) then begin
            IgnAtStart2 := 0;
            NoIgnoreAllowed := True;
          end;
        end;

        if IgnAtStart = 0 then
          IgnAtStart := IgnAtStart2Tmp;

        if IgnAtStart = 0 then begin
          IgnAtStart := IgnAtStart2;
          IgnAtStart2 := 0;
        end;
      end;

      if (IgnAtStart > 0) or (IgnAtStart2 > 0) then
        NoIgnoreAllowed := False;
    end;


    // check entire word, but without prefix
    if IgnAtStart > 0 then begin
      if coTreatPrefixedWordAsToken in Options then begin
        AWordIgnoreLen       := IgnoreShortToken;
        AWordAllowXCaps       := (coAllowTokenWithXCaps in Options) or (UpEnd = LowEnd);
        AWordAllowIgnoreXCaps := (coAllowIgnoreTokenWithXCaps in Options) or (UpEnd = LowEnd);
      end;

      i := LowEnd;
      if i = IgnAtStart then begin
        i := CurrentUpperEnd(i);
        i := CurrentLowerEnd(i);
      end;
      if ( (i = WordLen) or (coAllowMultiTokenWord in Options) ) and
         CheckSpelling(IgnAtStart, WordLen, AWordIgnoreLen, AWordAllowXCaps, AWordAllowIgnoreXCaps) then begin
        assert(LastSpellState in [esOk, esIgnored], 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: LastSpellState in [esOk, esIgnored]');
        SetReturnValues(LastSpellState, IgnAtStart, WordLen);
        Result := False;
        exit;
      end;

      if IgnAtStart2 > 0 then begin
        i := LowEnd;
        if i = IgnAtStart2 then begin
          i := CurrentUpperEnd(i);
          i := CurrentLowerEnd(i);
        end;
        if ( (i = WordLen) or (coAllowMultiTokenWord in Options) ) and
           CheckSpelling(IgnAtStart2, WordLen, AWordIgnoreLen, AWordAllowXCaps, AWordAllowIgnoreXCaps) then begin
          assert(LastSpellState in [esOk, esIgnored], 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: LastSpellState in [esOk, esIgnored]');
          SetReturnValues(LastSpellState, IgnAtStart2, WordLen);
          Result := False;
          exit;
        end;
      end;
    end;

    if IgnForWholeWordOnly then begin
      IgnAtStart := 0;
      IgnAtStart2 := 0;
    end;
  end;



  // TODO: check ignores first? Especially when IgnAtStart > 0
  UpOk   := False;
  LowOk  := False;
  LeadOk := False;
  MixOk  := False;
  LeadEnd := UpEnd - 1;
  while (LeadEnd > UpStart) and (Word[LeadEnd] in [#$80..#$BF]) do
    dec(LeadEnd);
  repeat
    // check entire token
    i := IgnoreShortToken;
    if (UpStart = 0) and   // prefix still part of it
       (UpEnd = UpStart)  // no upper for very first token
    then
      i := IgnoreLowerStart;
    if NoIgnoreAllowed then i := 0;
    if ((ASearchStartPos <> 0) or (LowEnd <> WordLen)) and // already done the whole word (with/without prefix)
       CheckSpelling(UpStart, LowEnd, i,
         (coAllowTokenWithXCaps in Options) or (UpEnd = LowEnd),
         (coAllowIgnoreTokenWithXCaps in Options) or (UpEnd = LowEnd))
    then begin
      assert(LastSpellState in [esOk, esIgnored], 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: LastSpellState in [esOk, esIgnored]');
      SetReturnValues(LastSpellState, UpStart, LowEnd);
      exit;
    end;

    (* Try to split and test separate upper and lower part / requires both parts to exist *)

    if FUpperConstr.HasLen(UpStart, UpEnd) and FLowerConstr.HasLen(UpEnd, LowEnd)
    then begin
      if FUpperConstr.CanIgnore(UpStart, UpEnd, LowEnd) and (not NoIgnoreAllowed) then begin
        Ok2 := SynSpellDictionary.CheckWord(@Word[UpEnd], LowEnd-UpEnd);
        Ok1  := Ok2 or SynSpellDictionary.CheckWord(@Word[UpStart], UpEnd-UpStart);
        if FLowerConstr.CanIgnore(UpEnd, LowEnd, UpStart) then // both can ignore, at least one must be in dictionary
          Ok2 := Ok2 or Ok1;
      end
      else begin
        Ok1  := SynSpellDictionary.CheckWord(@Word[UpStart], UpEnd-UpStart);
        if FLowerConstr.CanIgnore(UpEnd, LowEnd, UpStart) then
          Ok2 := Ok1 or SynSpellDictionary.CheckWord(@Word[UpEnd], LowEnd-UpEnd) // TODO: defer eval
        else
          Ok2 := SynSpellDictionary.CheckWord(@Word[UpEnd], LowEnd-UpEnd);
      end;
      if (not Ok2) and (UpEnd - UpStart = UTF8CodepointSize(@Word[UpStart])) then
        Ok1 := False; // don't report a single char as valid for a part token
      UpOk  := UpOk  or Ok1;
      LowOk := LowOk or Ok2;

      if Ok1 and Ok2 // One of them must have been done by the dictionary
      then begin
        // UPPER and lower are 2 words
        SetReturnValues(esOk, UpStart, LowEnd);
        exit;
      end;
    end;

    // try with the last upper attributed to the lower
    if (LeadEnd-UpStart > 1) and
       FLeadConstr.HasLen(UpStart, LeadEnd) and FMixedConstr.HasLen(LeadEnd, LowEnd)
    then begin
      if FLeadConstr.CanIgnore(UpStart, LeadEnd, LowEnd) and (not NoIgnoreAllowed) then begin
        Ok2 := SynSpellDictionary.CheckWord(@Word[LeadEnd], LowEnd-LeadEnd);
        Ok1  := Ok2 or SynSpellDictionary.CheckWord(@Word[UpStart], LeadEnd-UpStart);
        if FMixedConstr.CanIgnore(LeadEnd, LowEnd, UpStart) then // both can ignore, at least one must be in dictionary
          Ok2 := Ok2 or Ok1;
      end
      else begin
        Ok1  := SynSpellDictionary.CheckWord(@Word[UpStart], LeadEnd-UpStart);
        if FMixedConstr.CanIgnore(LeadEnd, LowEnd, UpStart) then
          Ok2 := Ok1 or SynSpellDictionary.CheckWord(@Word[LeadEnd], LowEnd-LeadEnd) // TODO: defer eval
        else
          Ok2 := SynSpellDictionary.CheckWord(@Word[LeadEnd], LowEnd-LeadEnd);
      end;
      if (not Ok2) and (LeadEnd - UpStart = UTF8CodepointSize(@Word[UpStart])) then
        Ok1 := False; // don't report a single char as valid for a part token
      LeadOk := LeadOk or Ok1;
      MixOk  := MixOk  or Ok2;

      if Ok1 and Ok2 // One of them must have been done by the dictionary
      then begin
        // UPPER and lower are 2 words
        SetReturnValues(esOk, UpStart, LowEnd);
        exit;
      end;
    end;

    if (IgnAtStart > 0) then begin
      UpStart := IgnAtStart;
      IgnAtStart := IgnAtStart2;
      IgnAtStart2 := 0;
      Continue;
    end;
    break;
  until False;

  // At least upper OR lower is an error
  assert(not(UpOk and LowOk), 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: not(UpOk and LowOk)');
  assert(not(LeadOk and MixOk), 'TSynSpellWordCheckerSourceCode.DoNextWordBreak: not(LeadOk and MixOk)');

  //TODO: restore FUpStart to before IgnAtStart was applied;
  FLowEnd := LowEnd;
  if MixOk then begin
    SetReturnValues(esWrong, UpStart, LeadEnd);
    FLowErrState := esOk;
    FLowStart := LeadEnd;
    Result := True;
  end
  else
  if LowOk then begin
    SetReturnValues(esWrong, UpStart, UpEnd);
    FLowErrState := esOk;
    FLowStart := UpEnd;
    Result := True;
  end
  else
  if UpOk then begin
    SetReturnValues(esOk, UpStart, UpEnd);
    FLowErrState := esWrong;
    FLowStart := UpEnd;
    Result := True;
  end
  else
  if LeadOk then begin
    SetReturnValues(esOk, UpStart, LeadEnd);
    FLowErrState := esWrong;
    FLowStart := LeadEnd;
    Result := True;
  end
  else begin
    SetReturnValues(esWrong, ASearchStartPos, LowEnd);
  end;
end;

constructor TSynSpellWordCheckerSourceCode.Create;
begin
  FLeadConstr  := TSynSpellWCheckerSrcPartConstraints.Create(Self);
  FMixedConstr := TSynSpellWCheckerSrcPartConstraints2.Create(Self);
  FUpperConstr := TSynSpellWCheckerSrcPartConstraints.Create(Self);
  FLowerConstr := TSynSpellWCheckerSrcPartConstraints2.Create(Self);
  inherited Create;
  FIgnoreShortWord := 3;
  FIgnoreShortToken := 3;
  FIgnoreLowerStart := 4;
  FIgnoreAtStartMinRemainderLen := 2;
  IgnoreLettersAtStart := 'TtFf';
  FOptions := DEFAULT_OPTIONS;
end;

destructor TSynSpellWordCheckerSourceCode.Destroy;
begin
  inherited Destroy;
  FLeadConstr.Free;
  FLowerConstr.Free;
  FMixedConstr.Free;
  FUpperConstr.Free;
end;

procedure TSynSpellWordCheckerSourceCode.Assign(AnOther: TSynSpellWordChecker);
var
  TheOther: TSynSpellWordCheckerSourceCode absolute AnOther;
begin
  inherited Assign(AnOther);
  if not (AnOther is TSynSpellWordCheckerSourceCode) then
    exit;

  FLeadConstr.Assign(TheOther.FLeadConstr);
  FMixedConstr.Assign(TheOther.FMixedConstr);
  FUpperConstr.Assign(TheOther.FUpperConstr);
  FLowerConstr.Assign(TheOther.FLowerConstr);

  FIgnoreShortWord  := TheOther.FIgnoreShortWord;
  FIgnoreShortToken := TheOther.FIgnoreShortToken;
  FIgnoreLowerStart := TheOther.FIgnoreLowerStart;

  FIgnoreAtStartMinRemainderLen := TheOther.FIgnoreAtStartMinRemainderLen;
  FIgnoreLettersAtStart         := TheOther.FIgnoreLettersAtStart;
  FIgnoreLettersAtStartSet      := TheOther.FIgnoreLettersAtStartSet;

  FOptions := TheOther.FOptions;

  FSpecialLowerLetters := TheOther.FSpecialLowerLetters;
  FSpecialUpperLetters := TheOther.FSpecialUpperLetters;

  DoChanged;
end;

function TSynSpellWordCheckerSourceCode.GetPartSuggestions(AStartPos: IntIdx; ALen: integer;
  AMaxSuggestions: integer): TStringArray;
begin
  Result := SynSpellDictionary.GetSuggestions(Word+AStartPos, ALen, AMaxSuggestions, @CheckSuggestion);
end;

procedure TSynSpellWordCheckerSourceCode.SetAllPartMinLen(AMinLen: integer);
begin
  // BeginUpdate
  FLeadConstr.MinLen  := AMinLen;
  FLowerConstr.MinLen := AMinLen;
  FMixedConstr.MinLen := AMinLen;
  FUpperConstr.MinLen := AMinLen;
end;

procedure TSynSpellWordCheckerSourceCode.SetAllPartIgnoreLen(AnIgnoreLen: integer);
begin
  // BeginUpdate
  FLeadConstr.IgnoreLen  := AnIgnoreLen;
  FLowerConstr.IgnoreLen := AnIgnoreLen;
  FMixedConstr.IgnoreLen := AnIgnoreLen;
  FUpperConstr.IgnoreLen := AnIgnoreLen;
end;

procedure TSynSpellWordCheckerSourceCode.SetAllPartIgnoreRequiredOther(AMinRequiredOther: integer);
begin
  // BeginUpdate
  FLeadConstr.IgnoreRemainderLen  := AMinRequiredOther;
  FLowerConstr.IgnoreRemainderLen := AMinRequiredOther;
  FMixedConstr.IgnoreRemainderLen := AMinRequiredOther;
  FUpperConstr.IgnoreRemainderLen := AMinRequiredOther;
end;

{ TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints }

procedure TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.SetIgnoreLen
  (AValue: integer);
begin
  if FIgnoreLen = AValue then Exit;
  FIgnoreLen := AValue;
  FOwner.DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.SetIgnoreRemainderLen
  (AValue: integer);
begin
  if FIgnoreRemainderLen = AValue then Exit;
  FIgnoreRemainderLen := AValue;
  FOwner.DoChanged;
end;

procedure TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.SetMinLen(
  AValue: integer);
begin
  if FMinLen = AValue then Exit;
  FMinLen := AValue;
  FOwner.DoChanged;
end;

constructor TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.Create(
  AnOwner: TSynSpellWordCheckerSourceCode);
begin
  FOwner := AnOwner;
  inherited Create;
  FIgnoreLen := 3;
  FIgnoreRemainderLen := 5;
end;

procedure TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.Assign(
  AnOther: TSynSpellWCheckerSrcPartConstraints);
begin
  FMinLen             := AnOther.FMinLen;
  FIgnoreLen          := AnOther.FIgnoreLen;
  FIgnoreRemainderLen := AnOther.FIgnoreRemainderLen;
  FOwner.DoChanged;
end;

function TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.HasLen(AStart,
  AnEnd: IntIdx): Boolean;
begin
  Result := (AnEnd - AStart >= FMinLen) and
            (AnEnd - AStart >  0);
end;

function TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.CanIgnore(AStart,
  AnEnd, AnOtherEnd: IntIdx): Boolean;
begin
  Result := (AnEnd - AStart <= FIgnoreLen) and
            (AnOtherEnd - AnEnd >= FIgnoreRemainderLen);
end;

procedure TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints.Init(AMinLen,
  AnIgnoreLen, AnIgnoreRemainderLen: integer);
begin
  FMinLen := AMinLen;
  FIgnoreLen := AnIgnoreLen;
  FIgnoreRemainderLen := AnIgnoreRemainderLen;
  FOwner.DoChanged;
end;

{ TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints2 }

function TSynSpellWordCheckerSourceCode.TSynSpellWCheckerSrcPartConstraints2.CanIgnore(AStart,
  AnEnd, AnOtherStart: IntIdx): Boolean;
begin
  Result := (AnEnd - AStart <= FIgnoreLen) and
            (AStart - AnOtherStart >= FIgnoreRemainderLen);
end;

end.

