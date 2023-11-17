{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
 *****************************************************************************

  Written by Martin Friebe 2023
}

unit TextMateGrammar;

{$mode objfpc}{$H+}
{ $DEFINE TMATE_MATCHING}

interface

uses
  Classes, SysUtils, fgl, fpjson, jsonparser, jsonscanner, Math,
  // LazUtils
  Laz2_DOM, Laz2_XMLRead, PList2JSon,
  //LazLoggerBase,
  LazLoggerDummy, LazClasses,
  // LazEdit
  xregexpr;

type

  { TTextMateGrammarException }

  TTextMateGrammarException = class(Exception)
  private
    FLocation: String;
  public
    constructor Create(const msg: string);
    constructor Create(const msg, JsKey: string);
    constructor Create(const msg: string; JsIndex: integer);
    function Copy: TTextMateGrammarException;
    function AddLocation(JsKey: string): TTextMateGrammarException;
    function AddLocation(JsIndex: integer): TTextMateGrammarException;
    function ErrorLocation: String;
    function FullError: String;
  end;

  TTextMatePattern = class;
  TTextMatePatternClass = class of TTextMatePattern;
  TTextMatePatternList  = specialize TFPGObjectList<TTextMatePattern>;
  TTextMatePatternMap   = specialize TFPGMapObject<string, TTextMatePattern>;
  TTextMatePatternArray = array of TTextMatePattern;
  TTextMatePatternBaseNested = class;
  TTextMateGrammar = class;

  TSynAttributeInfo = record
    TokId: Integer;
    TokObject: TObject;
  end;

  TPopulateAttributeInfoProc = procedure (Sender: TTextMateGrammar; APattern: TTextMatePattern;
    AContextName: String; var AnAttribInfo: TSynAttributeInfo) of object;
  TCheckAttributeInfoProc = procedure (Sender: TTextMatePattern;
    const AnAttribInfo: TSynAttributeInfo;
    out AnUseId, AnUseObject: Boolean) of object;
  TGetIncludedGrammar = function(Sender: TTextMateGrammar; AScopeName: string): TTextMateGrammar of object;


  TTextMateFoundCaptureBounds = record
    Start, Len: integer;
  end;
  TTextMateFoundCaptureBoundsArray = array of TTextMateFoundCaptureBounds;

  TTextMateMatchInfo = record
    Start: Integer;
    Pattern: TTextMatePattern;
    Index: Integer;
  end;

  TTextMatePatternStateFlag = (
    psfNestedinCapture,
    psfMatchBeginInitialized,
    psfMatchBeginDone,
    psfMatchEndInitialized,
    psfWhileInBeginLine,
    psfWhileDone
  );
  TTextMatePatternStateFlags = set of TTextMatePatternStateFlag;

  { TTextMatePatternStateEntry }

  TTextMatePatternStateEntry = object
    Pattern: TTextMatePattern;
    PatternIndex: integer;
    SubTextBeginPos, SubTextLen: integer; // begin is always 1 // if otherwise then ATextOffset needs adjustment
    CurrentTokenAttribInfo: TSynAttributeInfo;
    Flags: TTextMatePatternStateFlags;
    FoundCaptures: TTextMateFoundCaptureBoundsArray;

    CachedMatchInfo: TTextMateMatchInfo;
    CachedCaptureLen: Integer;

    procedure SetCache(AMatchInfo: TTextMateMatchInfo; ACaptureLen: Integer);
    function  GetCache(out AMatchInfo: TTextMateMatchInfo; out ACaptureLen: Integer): boolean;
    procedure ClearCache;
  end;
  PTextMatePatternStateEntry = ^TTextMatePatternStateEntry;
  TTextMatePatternStateArray = array of TTextMatePatternStateEntry;

  { TTextMatePatternState }

  TTextMatePatternState = object
  private
    function GetEntryP(ADepth: integer): PTextMatePatternStateEntry; inline;
    function GetParent(ADepth: integer): TTextMatePattern;
    function GetPattern(ADepth: integer): TTextMatePattern; inline;
  public
    StateList: TTextMatePatternStateArray;
    StateIdx: integer;

    procedure ValidateParentChain(AGrammar: TTextMateGrammar);
    procedure InitForDepth(ANewDepth: integer);
    procedure Add(const AMatchInfo: TTextMateMatchInfo; const ASubTextBeginPos, ASubTextLen: integer; const AFlags: TTextMatePatternStateFlags = []);
    procedure Pop;
    procedure ClearRecurseData;
    procedure CallParentNextToken(const AText: String; ACurTokenPos: integer;
      var ANextTokenPos: integer; // May be set by caller, so it can be kept unchanged if StateIdx = 0
      AnInitInfoOnly: Boolean = False);
    property Parent[ADepth: integer]: TTextMatePattern read GetParent;
    property Pattern[ADepth: integer]: TTextMatePattern read GetPattern;
    property EntryP[ADepth: integer]: PTextMatePatternStateEntry read GetEntryP; // valid only until next add/pop
  end;

  { TTextMatePattern }

  TTextMatePattern = class
  private
    FMainIndex: integer; // used as RANGE in SynEdit
    FComment: String;
    FName: String;
    FDebugName: String;
    FAttribInfo: TSynAttributeInfo; // Info for SynEdit
  protected
    // detect recursions that do not move forward
    FDeepestRecurseTextAtOffset: Integer;
    FDeepestRecurseWasInit: Boolean;
    function IsEndlessRecursion(ACurTokenPos: integer; AnInitInfoOnly: Boolean): boolean;
    procedure ClearDeepestRecurseData; virtual;

  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); virtual;
    procedure InitStateAfterAdded(AStateEntry: PTextMatePatternStateEntry); virtual;
    procedure GetCurrentTokenInfo(const AStates: TTextMatePatternState;
      const ACheckProc: TCheckAttributeInfoProc;
      out ATokenKind: integer; out AnAttr: TObject;
      ADepth: Integer
      ); virtual;
    procedure InitFoundCaptures(AStateEntryP: PTextMatePatternStateEntry; ARegEx: TRegExpr);

    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; virtual;

  protected
    function  GetForwardTarget: TTextMatePatternBaseNested; virtual;
    procedure FlattenNested(AGrammar: TTextMateGrammar; ARemoveMissingIncludes: boolean); virtual;
    class function GetCopyFor(AnOther:TTextMatePattern; AnIndexOffset: Integer; ANewList: TTextMatePatternList): TTextMatePattern;
    procedure CopyFrom(AnOther:TTextMatePattern; AnIndexOffset: Integer; ANewList: TTextMatePatternList); virtual;
    procedure DoInitRegex(var ARegEx: TRegExpr; const AText: string; AnErrKey: String);
  public
    procedure InitRegEx; virtual;
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; const AText: String);
    procedure GetCurrentTokenInfo(const AStates: TTextMatePatternState;
      const ACheckProc: TCheckAttributeInfoProc;
      out ATokenKind: integer; out AnAttr: TObject);

    procedure NextToken(var AStates: TTextMatePatternState; const AText: String;
      ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); virtual;
  public
    function DebugName: string; virtual;
    function DebugDump(AnIndent: Integer = 2; AnIncludeNested: Boolean = True; APrefix: string = ''): string; virtual;

    property Index: integer read FMainIndex;
    property Name: String read FName;
    property Comment: String read FComment;
    property AttribInfo: TSynAttributeInfo read FAttribInfo;
  end;

  { TTextMatePatternBaseNested }

  TTextMatePatternBaseNested = class(TTextMatePattern)
  private
    FParent: TTextMatePattern; // TTextMatePatternState.Parent takes precedence
    FRecurseMatchPos: integer;
    Patterns, UnFlatPatterns: TTextMatePatternArray;
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState;
      AParent: TTextMatePattern; const AText: String; ADepth: Integer); override;
    function FindPatternForNextMatchPos( // left-most matchg pos
      const AText: String; ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out AMatchInfo: TTextMateMatchInfo;
      AMatchMustStartBefore: integer = 0): boolean;
    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; override;

    function GetForwardTarget: TTextMatePatternBaseNested; override;
    procedure FlattenNested(AGrammar: TTextMateGrammar; ARemoveMissingIncludes: boolean); override;
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  public
    procedure NextToken(var AStates: TTextMatePatternState; const AText: String;
      ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); override;

  public
    function DebugDump(AnIndent: Integer = 2; AnIncludeNested: Boolean = True; APrefix: string = ''): string; override;
  end;

  { TTextMatePatternNested }

  TTextMatePatternNested = class(TTextMatePatternBaseNested)
  end;

  { TTextMatePatternNestedList }

  TTextMatePatternNestedList = class(TTextMatePatternNested)
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar;
      var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    procedure FlattenNested(AGrammar: TTextMateGrammar; ARemoveMissingIncludes: boolean); override;
  end;

  //TTextMatePatternInclude = class(TTextMatePatternNestedList)
  TTextMatePatternInclude = class(TTextMatePattern)
  protected
    FSourceScope, FSourceKey: String;
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  protected
    function GetFirstMatchPos(const AText: String;
      const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry; out APattern: TTextMatePattern;
      out AFoundStartPos: integer; AMatchMustStartBefore: integer = 0
      ): Boolean; override;
  end;

  { TTextMatePatternCapture }

  TTextMatePatternCapture = class(TTextMatePatternBaseNested)
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    procedure InitStateAfterAdded(AStateEntry: PTextMatePatternStateEntry); override;
  end;

  TTextMatePatternCaptureArray = array of TTextMatePatternCapture;

  { TTextMatePatternCaptures }

  TTextMatePatternCaptures = object
    CaptureArray: TTextMatePatternCaptureArray; // may contain nil entries
    function FindCaptureForNextMatchPos( // left-most matchg pos
      ATextStartOffset: integer;
      const AFoundCapturePosList: TTextMateFoundCaptureBoundsArray;
      out ACaptureInfo: TTextMateMatchInfo): boolean;
    procedure CopyFrom(AnOther: TTextMatePatternCaptures; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList);
  end;

  { TTextMatePatternMatch }

  TTextMatePatternMatch = class(TTextMatePattern)
  private
    FRegExMatch: TRegExpr;
    Match: String;
    Captures: TTextMatePatternCaptures;
  protected
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; override;
  public
    destructor Destroy; override;
    procedure InitRegEx; override;
    procedure NextToken(var AStates: TTextMatePatternState; const AText: String;
      ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); override;
  end;

  { TTextMatePatternBeginEnd }

  TTextMatePatternBeginEnd = class(TTextMatePatternBaseNested)
  private
    FRegExMatchBegin, FRegExMatchEnd: TRegExpr;
    FRecursiveFirstMatchPos: integer;
    MatchBegin, MatchEnd: String;
    ContentName: String;
    ContentAttribInfo: TSynAttributeInfo;
    EndPatternLast: Boolean;
    Captures, CapturesBegin, CapturesEnd: TTextMatePatternCaptures;
  protected
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; override;
  public
    destructor Destroy; override;
    procedure InitRegEx; override;
    procedure NextToken(var AStates: TTextMatePatternState; const AText: String;
      ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); override;
  end;

  { TTextMatePatternBeginWhile }

  TTextMatePatternBeginWhile = class(TTextMatePatternBaseNested)
  private
    FRegExMatchBegin, FRegExMatchWhile: TRegExpr;
    MatchBegin, MatchWhile: String;
    ContentName: String;
    ContentAttribInfo: TSynAttributeInfo;
    Captures, CapturesBegin, CapturesWhile: TTextMatePatternCaptures;
  protected
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  protected
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    procedure InitStateAfterAdded(AStateEntry: PTextMatePatternStateEntry); override;
    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; override;
  public
    destructor Destroy; override;
    procedure InitRegEx; override;
    procedure NextToken(var AStates: TTextMatePatternState;
      const AText: String; ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); override;
  end;

  { TTextMatePatternRoot }

  TTextMatePatternRoot = class(TTextMatePatternNested)
  protected
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
  public
    ScopeName: String;
  end;

  { TTextMatePatternForwarder }

  TTextMatePatternForwarder = class(TTextMatePatternBaseNested)
  private
    FForwardTo: TTextMatePatternBaseNested;
  protected
    procedure CopyFrom(AnOther: TTextMatePattern; AnIndexOffset: Integer;
      ANewList: TTextMatePatternList); override;
  protected
    procedure ClearDeepestRecurseData; override;
    function  GetForwardTarget: TTextMatePatternBaseNested; override;
    procedure InitStates(const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState; AParent: TTextMatePattern;
      const AText: String; ADepth: Integer); override;
    function GetFirstMatchPos(const AText: String; const ATextStartOffset: integer;
      AStateEntryP: PTextMatePatternStateEntry;
      out APattern: TTextMatePattern; out AFoundStartPos: integer;
      AMatchMustStartBefore: integer = 0): Boolean; override;
  public
    procedure NextToken(var AStates: TTextMatePatternState;
      const AText: String; ACurTokenPos: integer; out ANextTokenPos: integer;
      AnInitInfoOnly: Boolean = False); override;
    property ForwardTo: TTextMatePatternBaseNested read FForwardTo;

    function DebugDump(AnIndent: Integer = 2; AnIncludeNested: Boolean = True; APrefix: string = ''): string; override;
  end;

  { TTextMateGrammar }

  TTextMateGrammar = class(TFreeNotifyingObject)
  private type
    TOtherGrammarMap = specialize TFPGMapObject<string, TTextMateGrammar>;
  private
    FOnCheckAttributeInfo: TCheckAttributeInfoProc;
    FOnPopulateAttributeInfo: TPopulateAttributeInfoProc;
    FParserError: String;
    FSampleText: String;
    FSampleTextFile: String;
    FLangName: string;

    FRegExMatchFoldBegin, FRegExMatchFoldEnd: TRegExpr;
    MatchFoldBegin, MatchFoldEnd: String;


    FMainPatternCount: integer;
    FMainPatternList: TTextMatePatternList;
    FPatternRepo: TTextMatePatternMap;
    FRootPattern: TTextMatePatternRoot;
    FTheEmptyPattern: TTextMatePattern;
    FOtherGrammars: TOtherGrammarMap;


    procedure ClearAttributeInfo(var AnAttribInfo: TSynAttributeInfo);
    function  CreatePatternObject(AClass: TTextMatePatternClass): TTextMatePattern;
    procedure DoOtherGrammarFreed(Sender: TObject);

    function  GetJson(AGrammarDef: String): TJSONObject;
    procedure ParseLanguage(ALangDef: TJSONData);
    function  IsInclude(APatternJson: TJSONObject): boolean;
    function  IncludeName(APatternJson: TJSONObject; out AnOtherScopeName: string): string;
    function  ResolveInclude(APatternJson: TJSONObject): TTextMatePattern;
    procedure CopyPatterns(AnOtherGrammar: TTextMateGrammar; AnIncludeName: String);
    function  CreatePattern(AParent: TTextMatePattern; APatternJson: TJSONObject; AllowPatternOnly: Boolean = False): TTextMatePattern;
    procedure ReadPattern(APattern: TTextMatePattern; APatternJson: TJSONObject);
    function  ParsePattern(AParent: TTextMatePattern; APatternJson: TJSONObject): TTextMatePattern;
    procedure ParsePatterns(AParent: TTextMatePattern; var APatternArray: TTextMatePatternArray; APatterns: TJSONArray);

  private
    FCurrentPattern: TTextMatePattern;
    FCurrentState: TTextMatePatternState;
    FLanguageScopeName: String;
    FLineText: String;
    FCurrentTokenPos, FNextTokenPos: Integer;
    FMissingIncludes: String;
    FCurrentTokenKind: integer;
    FCurrentAttrib: TObject;
    FOnGetIncludedGrammar: TGetIncludedGrammar;
    function GetCurrentPatternIndex: Integer;
    function GetCurrentTokenLen: integer; inline;

  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseGrammar(AGrammarDef: String);
    // ResolveExternalIncludes: Call only what all other grammars have done the initial ParseGrammar
    procedure ResolveExternalIncludes;
    procedure ClearGrammar;
    function DebugDump(AnIndent: Integer = 2; AnIncludeNested: Boolean = True): string;

    procedure SetLine(const AText: String; const AnInitialPatternIndex: Integer);
    procedure First;
    procedure Next;
    function  IsAtEol: boolean;
    procedure NextToEol;
    function  IsFoldBegin: boolean;
    function  IsFoldEnd: boolean;

    property CurrentTokenPos: integer read FCurrentTokenPos;
    property NextTokenPos: integer read FNextTokenPos;
    property CurrentTokenLen: integer read GetCurrentTokenLen;
    property CurrentTokenKind: integer read FCurrentTokenKind;
    property CurrentAttrib: TObject read FCurrentAttrib;
    property CurrentPatternIndex: Integer read GetCurrentPatternIndex;

    property CurrentState: TTextMatePatternState read FCurrentState;

    property LanguageName: String read FLangName write FLangName;
    property LanguageScopeName: String read FLanguageScopeName;
    property SampleText: String read FSampleText write FSampleText;
    property SampleTextFile: String read FSampleTextFile;
    property ParserError: String read FParserError;
    property MissingIncludes: String read FMissingIncludes;

    property RootPattern: TTextMatePatternRoot read FRootPattern;
    property MainPatternList: TTextMatePatternList read FMainPatternList;

    property OnPopulateAttributeInfo: TPopulateAttributeInfoProc read FOnPopulateAttributeInfo write FOnPopulateAttributeInfo;
    property OnCheckAttributeInfo: TCheckAttributeInfoProc read FOnCheckAttributeInfo write FOnCheckAttributeInfo;
    property OnGetIncludedGrammar: TGetIncludedGrammar read FOnGetIncludedGrammar write FOnGetIncludedGrammar;
  end;

implementation

function LeftmostFoundPattern(var Info1: TTextMateMatchInfo; const Info2: TTextMateMatchInfo): boolean; inline;
begin
  Result := Info1.Index >= 0;
  if Info2.Index >= 0 then begin
    if (not Result) or (Info2.Start < Info1.Start) then begin
      Info1 := Info2;
      Result := Info1.Index >= 0;
    end;
  end;
end;

procedure AssertJsObject(AJSon: TJSONData); inline;
begin
  if (AJSon = nil) or (not (AJSon is TJSONObject)) then
    raise TTextMateGrammarException.Create('Expected Js-Object');
end;

procedure AssertJsArray(AJSon: TJSONData); inline;
begin
  if (AJSon = nil) or (not (AJSon is TJSONArray)) then
    raise TTextMateGrammarException.Create('Expected Js-Array');
end;

function jsAsDict(AJSon: TJSONData): TJSONObject; inline;
begin
  if AJSon = nil then exit(nil);
  if not (AJSon is TJSONObject) then
    raise TTextMateGrammarException.Create('Expected Js-Object');
  Result := TJSONObject(AJSon);
end;

function jsAsArray(AJSon: TJSONData): TJSONArray; inline;
begin
  if AJSon = nil then exit(nil);
  if not (AJSon is TJSONArray) then
    raise TTextMateGrammarException.Create('Expected Js-Array');
  Result := TJSONArray(AJSon);
end;

function jsKeyAsDict(AJSon: TJSONData; AKey: String): TJSONObject;
begin
  AssertJsObject(AJSon);
  Result := jsAsDict(TJSONObject(AJSon)[AKey]);
end;

function jsKeyAsArray(AJSon: TJSONData; AKey: String): TJSONArray;
begin
  AssertJsObject(AJSon);
  Result := jsAsArray(TJSONObject(AJSon)[AKey]);
end;

function jsIndexAsDict(AJSon: TJSONData; AnIdx: Integer): TJSONObject;
begin
  AssertJsArray(AJSon);
  Result := jsAsDict(TJSONArray(AJSon)[AnIdx]);
end;

function jsKeyAsString(AJSon: TJSONData; AKey: String; AllowNil: Boolean = True): string;
var
  R: TJSONObject;
  R2: TJSONData;
begin
  AssertJsObject(AJSon);
  R := TJSONObject(AJSon);
  if AllowNil and (R.IndexOfName(AKey) < 0) then
    exit('');
  R2 := R[AKey];
  if not (R2 is TJSONString) then
    raise TTextMateGrammarException.Create('Expected String', AKey);
  Result := R2.AsString;
end;

function jsKeyAsNumber(AJSon: TJSONData; AKey: String; AllowNil: Boolean = True): integer;
var
  R: TJSONObject;
  R2: TJSONData;
begin
  AssertJsObject(AJSon);
  R := TJSONObject(AJSon);
  if AllowNil and (R.IndexOfName(AKey) < 0) then
    exit(0);
  R2 := R[AKey];
  if not (R2 is TJSONIntegerNumber) then
    raise TTextMateGrammarException.Create('Expected Number', AKey);
  Result := R2.AsInteger;
end;

{ TTextMateGrammarException }

constructor TTextMateGrammarException.Create(const msg: string);
begin
  inherited Create(msg);
end;

constructor TTextMateGrammarException.Create(const msg, JsKey: string);
begin
  inherited Create(msg);
  FLocation := '.' + JsKey;
end;

constructor TTextMateGrammarException.Create(const msg: string; JsIndex: integer);
begin
  inherited Create(msg);
  FLocation := '[' + IntToStr(JsIndex) + ']';
end;

function TTextMateGrammarException.Copy: TTextMateGrammarException;
begin
  Result := TTextMateGrammarException.Create(Message);
  Result.FLocation := FLocation;
end;

function TTextMateGrammarException.AddLocation(JsKey: string
  ): TTextMateGrammarException;
begin
  FLocation := '.' + JsKey + FLocation;
  Result := Self;
end;

function TTextMateGrammarException.AddLocation(JsIndex: integer
  ): TTextMateGrammarException;
begin
  FLocation := '[' + IntToStr(JsIndex) + ']' + FLocation;
  Result := Self;
end;

function TTextMateGrammarException.ErrorLocation: String;
begin
  Result := FLocation;
  if (Result <> '') and (Result[1] = '.') then
    Delete(Result, 1, 1);
end;

function TTextMateGrammarException.FullError: String;
begin
  Result := Message + ' at ' + ErrorLocation;
end;

{ TTextMatePatternStateEntry }

procedure TTextMatePatternStateEntry.SetCache(AMatchInfo: TTextMateMatchInfo;
  ACaptureLen: Integer);
begin
  {$IFDEF TMATE_MATCHING} debugln(['SET cache >>> Pos:', AMatchInfo.Start, ' Idx:', AMatchInfo.Index, ' C-Len:', ACaptureLen]); {$ENDIF}
  CachedMatchInfo  := AMatchInfo;
  CachedCaptureLen := ACaptureLen;
end;

function TTextMatePatternStateEntry.GetCache(out
  AMatchInfo: TTextMateMatchInfo; out ACaptureLen: Integer): boolean;
begin
  Result := CachedMatchInfo.Start > 0;
  {$IFDEF TMATE_MATCHING}debugln(Result, ['Got cache at ', CachedMatchInfo.Start]);{$ENDIF}
  AMatchInfo  := CachedMatchInfo;
  ACaptureLen := CachedCaptureLen;
  CachedMatchInfo.Start  := 0;
end;

procedure TTextMatePatternStateEntry.ClearCache;
begin
  CachedMatchInfo.Start  := 0;
end;

{ TTextMatePatternState }

function TTextMatePatternState.GetEntryP(ADepth: integer
  ): PTextMatePatternStateEntry;
begin
  Result := @StateList[StateIdx - ADepth];
end;

function TTextMatePatternState.GetParent(ADepth: integer
  ): TTextMatePattern;
begin
  if ADepth < StateIdx then
    Result := StateList[StateIdx - (ADepth + 1)].Pattern
  else
    Result := nil;
end;

function TTextMatePatternState.GetPattern(ADepth: integer): TTextMatePattern;
begin
  Result := StateList[StateIdx - ADepth].Pattern;
end;

procedure TTextMatePatternState.ValidateParentChain(AGrammar: TTextMateGrammar);
var
  i, j: Integer;
  NewFwrd: TTextMatePatternForwarder;
  Par, Cur, Cur2: TTextMatePatternBaseNested;
begin
  for i := 1 to StateIdx do begin
    assert(StateList[i].Pattern is TTextMatePatternBaseNested, 'TTextMatePatternState.ValidateParentChain: StateList[i].Pattern is TTextMatePatternBaseNested');
    Cur := TTextMatePatternBaseNested(StateList[i].Pattern);
    if Cur.FParent = StateList[i-1].Pattern then
      Continue;
    if Cur.FParent = nil then begin
      Cur.FParent := StateList[i-1].Pattern;
      Continue;
    end;

    Par := TTextMatePatternBaseNested(StateList[i-1].Pattern);
    Cur2 := Cur.GetForwardTarget;

    NewFwrd := TTextMatePatternForwarder(AGrammar.CreatePatternObject(TTextMatePatternForwarder));
    SetLength(NewFwrd.Patterns, Length(Cur2.Patterns));
    for j := 0 to Length(NewFwrd.Patterns) - 1 do
      NewFwrd.Patterns[j] := Cur2.Patterns[j];

    NewFwrd.FParent := Par;
    NewFwrd.FForwardTo := Cur2;
    StateList[i].Pattern := NewFwrd;

    j := StateList[i].PatternIndex;
    assert((j>=0) and (j < length(Par.Patterns)), 'TTextMatePatternState.ValidateParentChain: (j>=0) and (j < length(Par.Patterns))');
    assert(Par.Patterns[j].GetForwardTarget = Cur2, 'TTextMatePatternState.ValidateParentChain: Par.Patterns[j].GetForwardTarget = Cur2');
    Par.Patterns[j] := NewFwrd;
  end;
end;

procedure TTextMatePatternState.InitForDepth(ANewDepth: integer);
begin
  if (Length(StateList) < ANewDepth) or (Length(StateList) > ANewDepth + 256) then
    SetLength(StateList, ANewDepth + 16);
  StateIdx := ANewDepth - 1; // for the top caller
end;

procedure TTextMatePatternState.Add(const AMatchInfo: TTextMateMatchInfo;
  const ASubTextBeginPos, ASubTextLen: integer;
  const AFlags: TTextMatePatternStateFlags);
begin
  {$IFDEF TMATE_MATCHING}
  DebugLn(['TTextMatePatternState.Add >>> OLD idx=', StateIdx, ' ADD ',ASubTextBeginPos,'..',ASubTextLen]);
  {$ENDIF}
  inc(StateIdx);
  if StateIdx >= Length(StateList) then
    SetLength(StateList, StateIdx + 16);
  StateList[StateIdx].Pattern := AMatchInfo.Pattern;
  StateList[StateIdx].PatternIndex := AMatchInfo.Index;
  StateList[StateIdx].SubTextBeginPos := ASubTextBeginPos;
  StateList[StateIdx].SubTextLen := ASubTextLen;
  StateList[StateIdx].CurrentTokenAttribInfo.TokId := -1;
  StateList[StateIdx].CurrentTokenAttribInfo.TokObject := nil;
  StateList[StateIdx].Flags := AFlags;
  StateList[StateIdx].ClearCache;
  AMatchInfo.Pattern.InitStateAfterAdded(@StateList[StateIdx]);
end;

procedure TTextMatePatternState.Pop;
begin
  StateList[StateIdx].Pattern.ClearDeepestRecurseData;
  dec(StateIdx);
end;

procedure TTextMatePatternState.ClearRecurseData;
var
  i: Integer;
begin
  for i := 0 to min(StateIdx, High(StateList)) do begin
    StateList[i].ClearCache;
    StateList[i].Pattern.ClearDeepestRecurseData;
  end;
end;

procedure TTextMatePatternState.CallParentNextToken(const AText: String;
  ACurTokenPos: integer; var ANextTokenPos: integer; AnInitInfoOnly: Boolean);
var
  st: PTextMatePatternStateEntry;
begin
  {$IFDEF TMATE_MATCHING}
  debugln(['CallParentNextToken <<< for "'+StateList[StateIdx].Pattern.ClassName+'"  | Depth', StateIdx, ', CurPos:',ACurTokenPos, ', NExtPos ', ANextTokenPos, ' ',AnInitInfoOnly]);
  {$ENDIF}
  if StateIdx = 0 then
    exit;
  Pop;
  st := @StateList[StateIdx];
  st^.Pattern.NextToken(Self, AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
end;

{ TTextMatePattern }

procedure TTextMatePattern.InitRegEx;
begin
  //
end;

procedure TTextMatePattern.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; const AText: String);
begin
  InitStates(AGrammar, AStates, nil, AText, 0);
end;

procedure TTextMatePattern.GetCurrentTokenInfo(
  const AStates: TTextMatePatternState;
  const ACheckProc: TCheckAttributeInfoProc; out ATokenKind: integer; out
  AnAttr: TObject);
begin
  GetCurrentTokenInfo(AStates, ACheckProc, ATokenKind, AnAttr, 0);
end;

procedure TTextMatePattern.NextToken(var AStates: TTextMatePatternState;
  const AText: String; ACurTokenPos: integer; out ANextTokenPos: integer;
  AnInitInfoOnly: Boolean);
begin
  ANextTokenPos := Length(AText);
  assert(False, 'TTextMatePattern.NextToken: False');
end;

function TTextMatePattern.DebugName: string;
begin
  Result := FName;
  If FDebugName <> '' then
    Result := '#' + FDebugName + ': ' + Result;

  if Result = '' then
    Result := ClassName;
  if FComment <> '' then
    Result := Result + ' // ' + FComment;
end;

function TTextMatePattern.DebugDump(AnIndent: Integer;
  AnIncludeNested: Boolean; APrefix: string): string;
begin
  Result := StringOfChar(' ', AnIndent) + APrefix
          + '[' + IntToStr(FMainIndex) + '] ' + DebugName + LineEnding;
end;

function TTextMatePattern.IsEndlessRecursion(ACurTokenPos: integer;
  AnInitInfoOnly: Boolean): boolean;
begin
  Result :=
     (FDeepestRecurseWasInit and AnInitInfoOnly) or
     (ACurTokenPos < FDeepestRecurseTextAtOffset) or
     ((not FDeepestRecurseWasInit) and (ACurTokenPos = FDeepestRecurseTextAtOffset))
     ;
  FDeepestRecurseWasInit := False;
  if not Result then
    FDeepestRecurseTextAtOffset := ACurTokenPos
  {$IFDEF TMATE_MATCHING} else DebugLn(['TTextMatePatternRoot.NextToken: recursion did not advance']) {$ENDIF}
  ;
end;

procedure TTextMatePattern.ClearDeepestRecurseData;
begin
  FDeepestRecurseTextAtOffset := 0;
  FDeepestRecurseWasInit := False;
end;

procedure TTextMatePattern.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  assert(False, 'TTextMatePattern.InitStates: not TTextMatePatternBaseNested');
end;

procedure TTextMatePattern.GetCurrentTokenInfo(
  const AStates: TTextMatePatternState;
  const ACheckProc: TCheckAttributeInfoProc; out ATokenKind: integer; out
  AnAttr: TObject; ADepth: Integer);
var
  st: PTextMatePatternStateEntry;
  UseId, UseObj: Boolean;
  AParent: TTextMatePattern;
  ParTokenKind: integer;
  ParAttr: TObject;
begin
  st := AStates.EntryP[ADepth];

  ACheckProc(Self, st^.CurrentTokenAttribInfo, UseId, UseObj);
  ATokenKind :=  st^.CurrentTokenAttribInfo.TokId;
  AnAttr :=  st^.CurrentTokenAttribInfo.TokObject;
  if UseId and UseObj then
    exit;

  AParent := AStates.Parent[ADepth];
  if AParent <> nil then begin
    AParent.GetCurrentTokenInfo(AStates, ACheckProc, ParTokenKind, ParAttr, ADepth + 1);
    if not UseId then
      ATokenKind := ParTokenKind;
    if not UseObj then
      AnAttr := ParAttr;
  end;
end;

procedure TTextMatePattern.InitStateAfterAdded(
  AStateEntry: PTextMatePatternStateEntry);
begin
  //
end;

procedure TTextMatePattern.InitFoundCaptures(
  AStateEntryP: PTextMatePatternStateEntry; ARegEx: TRegExpr);
var
  c, i: Integer;
begin
  c := ARegEx.SubExprMatchCount;
  assert(c>=0, 'TTextMatePatternBeginEnd.NextToken: c>=0');
  SetLength(AStateEntryP^.FoundCaptures, c + 1);
  for i := 0 to c do begin
    AStateEntryP^.FoundCaptures[i].Start := ARegEx.MatchPos[i];
    AStateEntryP^.FoundCaptures[i].Len   := ARegEx.MatchLen[i];
  end;
end;

function TTextMatePattern.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
begin
  Result := False;
  assert(False);
end;

function TTextMatePattern.GetForwardTarget: TTextMatePatternBaseNested;
begin
  assert(False, 'TTextMatePattern.GetForwardTarget: False');
  Result := nil;
  //Result := Self;
end;

procedure TTextMatePattern.FlattenNested(AGrammar: TTextMateGrammar;
  ARemoveMissingIncludes: boolean);
begin
  //
end;

class function TTextMatePattern.GetCopyFor(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList): TTextMatePattern;
begin
  Result := nil;
  if AnOther = nil then
    exit;
  Result := ANewList[AnOther.Index + AnIndexOffset];
end;

procedure TTextMatePattern.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
begin
  FComment    := AnOther.FComment;
  FName       := AnOther.FName;
  FDebugName  := AnOther.FDebugName;
  FAttribInfo := AnOther.FAttribInfo;
end;

procedure TTextMatePattern.DoInitRegex(var ARegEx: TRegExpr;
  const AText: string; AnErrKey: String);
var
  err: Integer;
begin
  ARegEx.Free;
  ARegEx := TRegExpr.Create(AText);
  ARegEx.RaiseForRuntimeError := False;
  ARegEx.AllowBraceWithoutMin := True;
  ARegEx.AllowLiteralBraceWithoutRange := True;
  //ARegEx.AllowUnsafeLookBehind := True;
  try
    ARegEx.Compile;
  except
    on E: Exception do raise TTextMateGrammarException.Create(E.Message, AnErrKey);
  end;
  err := ARegEx.LastError;
  if err <> 0 then
    raise TTextMateGrammarException.Create(ARegEx.ErrorMsg(err), AnErrKey);
end;

procedure TTextMatePatternBaseNested.InitStates(
  const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState;
  AParent: TTextMatePattern; const AText: String; ADepth: Integer);
begin
  if AParent = nil then
    AParent := FParent;
  AParent.InitStates(AGrammar, AStates, nil, AText, ADepth + 1);

  AStates.StateList[AStates.StateIdx - ADepth].Pattern := self;
  AStates.StateList[AStates.StateIdx - ADepth].SubTextBeginPos := 1;
  AStates.StateList[AStates.StateIdx - ADepth].SubTextLen := Length(AText);
  AStates.StateList[AStates.StateIdx - ADepth].CurrentTokenAttribInfo.TokId := -1;
  AStates.StateList[AStates.StateIdx - ADepth].CurrentTokenAttribInfo.TokObject := nil;
  AStates.StateList[AStates.StateIdx - ADepth].Flags := [];
  AStates.StateList[AStates.StateIdx - ADepth].ClearCache;
  FDeepestRecurseTextAtOffset := 0;
end;

function TTextMatePatternBaseNested.FindPatternForNextMatchPos(
  const AText: String; ATextStartOffset: integer;
  AStateEntryP: PTextMatePatternStateEntry; out AMatchInfo: TTextMateMatchInfo;
  AMatchMustStartBefore: integer): boolean;
var
  i, FndStartPos, OldRecurseMatchPos: integer;
  FndPattern: TTextMatePattern;
begin
  Result := False;

  AMatchInfo.Index := -1;
  if ATextStartOffset > AStateEntryP^.SubTextLen then
    exit;

  if ATextStartOffset = FRecurseMatchPos then begin
    {$IFDEF TMATE_MATCHING} debugln(['FindPatternForNextMatchPos - Recurse at ', ATextStartOffset, ' -- ', DebugName]); {$ENDIF}
    exit;
  end;
  OldRecurseMatchPos := FRecurseMatchPos;
  FRecurseMatchPos := ATextStartOffset;

  AMatchInfo.Start := high(AMatchInfo.Start);
  for i := 0 to Length(Patterns) - 1 do begin
    if not Patterns[i].GetFirstMatchPos(AText, ATextStartOffset, AStateEntryP, FndPattern, FndStartPos, AMatchMustStartBefore) then
      continue;
    Result := True;

    if FndStartPos >= AMatchInfo.Start then
      continue;

    if (FndStartPos < AMatchMustStartBefore) or (AMatchMustStartBefore < 1) then
      AMatchMustStartBefore := FndStartPos;

    AMatchInfo.Start := FndStartPos;
    AMatchInfo.Pattern := FndPattern;
    AMatchInfo.Index := i;

    if AMatchInfo.Start = 1 then
      Break;
  end;
  FRecurseMatchPos := OldRecurseMatchPos;
end;

{ TTextMatePatternBaseNested }

function TTextMatePatternBaseNested.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
var
  AMatchInfo: TTextMateMatchInfo;
begin
  assert(AStateEntryP^.Pattern is TTextMatePatternBaseNested, 'TTextMatePatternBaseNested.GetFirstMatchPos: AStateEntryP^.Pattern is TTextMatePatternBaseNested');
  Result := TTextMatePatternBaseNested(AStateEntryP^.Pattern).FindPatternForNextMatchPos
    (AText, ATextStartOffset, AStateEntryP, AMatchInfo, AMatchMustStartBefore);
  AFoundStartPos := AMatchInfo.Start;
  APattern := AMatchInfo.Pattern;
end;

function TTextMatePatternBaseNested.GetForwardTarget: TTextMatePatternBaseNested;
begin
  Result := Self;
end;

procedure TTextMatePatternBaseNested.FlattenNested(AGrammar: TTextMateGrammar;
  ARemoveMissingIncludes: boolean);
var
  NewPtnList: TTextMatePatternArray;
  NewPtnIdx: integer;

  procedure InsertInto(ASrcList: TTextMatePatternArray);
  var
    i, j: Integer;
    PNest: TTextMatePatternNested;
    PIncl: TTextMatePatternInclude;
    Src: TTextMatePattern;
    k: String;
  begin
    SetLength(NewPtnList, Length(NewPtnList) + Length(ASrcList) - 1);
    for i := 0 to Length(ASrcList) - 1 do begin
      Src := ASrcList[i];
      PNest := nil;
      if (Src is TTextMatePatternInclude) then begin
        j := 0;
        repeat
          PIncl := TTextMatePatternInclude(Src);
          if (PIncl.FSourceScope = '$base') or (PIncl.FSourceScope = AGrammar.FLanguageScopeName) then
            k := PIncl.FSourceKey
          else
            k := PIncl.FSourceScope + '#' + PIncl.FSourceKey;

          if k = '' then begin
            Src := AGrammar.FRootPattern;
            Break;
          end;
          if AGrammar.FPatternRepo.IndexOf(k) >= 0 then begin
            Src := AGrammar.FPatternRepo[k];
            if Src is TTextMatePatternInclude then begin
              inc(j);
              if j > AGrammar.MainPatternList.Count then
                raise TTextMateGrammarException.Create('Invalid nested pattern');
              continue;
            end;
          end
          else begin
            if (ARemoveMissingIncludes) then begin
              Src := nil;
              if (AGrammar.FOtherGrammars.IndexOf(PIncl.FSourceScope) >= 0) and
                 (AGrammar.FOtherGrammars[PIncl.FSourceScope] <> nil)
              then begin
                if AGrammar.FMissingIncludes <> '' then AGrammar.FMissingIncludes := AGrammar.FMissingIncludes + ', ';
                AGrammar.FMissingIncludes := AGrammar.FMissingIncludes + k;
              end;
            end;
          end;
          break;
        until False;
      end;
      if Src is TTextMatePatternNested then begin
        PNest := TTextMatePatternNested(Src);
      end;

      if PNest <> nil then begin
        if PNest.FParent = Self then
          raise TTextMateGrammarException.Create('Invalid nested pattern');
        PNest.FParent := Self;
        InsertInto(PNest.Patterns);
        PNest.FParent := nil;
      end
      else
      if Src <> nil then begin
        NewPtnList[NewPtnIdx] := Src;
        inc(NewPtnIdx);
      end;
    end;
  end;

begin
  if UnFlatPatterns = nil then
    UnFlatPatterns := Patterns;
  SetLength(NewPtnList, 1);
  NewPtnIdx := 0;
  InsertInto(UnFlatPatterns);
  SetLength(NewPtnList, NewPtnIdx);
  Patterns := NewPtnList
end;

procedure TTextMatePatternBaseNested.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternBaseNested absolute AnOther;
  i: Integer;
  Src: TTextMatePatternArray;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  FParent := GetCopyFor(TheOther.FParent, AnIndexOffset, ANewList);
  Src := TTextMatePatternBaseNested(AnOther).UnFlatPatterns;
  if Src = nil then
    Src := TTextMatePatternBaseNested(AnOther).Patterns;
  SetLength(Patterns, Length(Src));
  for i := 0 to Length(Src) - 1 do
    Patterns[i] :=  GetCopyFor(Src[i], AnIndexOffset, ANewList);
end;

procedure TTextMatePatternBaseNested.NextToken(
  var AStates: TTextMatePatternState; const AText: String;
  ACurTokenPos: integer; out ANextTokenPos: integer; AnInitInfoOnly: Boolean);
var
  FndMatchInfo: TTextMateMatchInfo;
  st: PTextMatePatternStateEntry;
begin
  {$IFDEF TMATE_MATCHING}
  debuglnEnter(['> '+ClassName+'.NextToken ',DebugName,' --- TkPos:', ACurTokenPos, ' ',dbgs(AnInitInfoOnly), '  txt-len:',AStates.EntryP[0]^.SubTextLen,'/',length(AText) ]); try
  {$ENDIF}
  if IsEndlessRecursion(ACurTokenPos, AnInitInfoOnly) then begin
    if AnInitInfoOnly
    then ANextTokenPos := ACurTokenPos
    else AStates.CallParentNextToken(AText, Length(AText) + 1, ANextTokenPos, False);
    exit;
  end;

  st := AStates.EntryP[0];
  st^.CurrentTokenAttribInfo := FAttribInfo;

  if AnInitInfoOnly
  then ANextTokenPos := ACurTokenPos
  else ANextTokenPos := st^.SubTextLen + 1;

  if ACurTokenPos > st^.SubTextLen then begin // can happen in captures, as text is not full line text
    assert(ACurTokenPos=st^.SubTextLen+1, 'TTextMatePatternBaseNested.NextToken: ACurTokenPos=Length(AText)+1');
    {$IFDEF TMATE_MATCHING} debugln(['EXIT - because curpos ALREADY was TOO FAR']); {$ENDIF}
    AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
    exit;
  end;

  if not TTextMatePatternBaseNested(st^.Pattern).FindPatternForNextMatchPos
         (AText, ACurTokenPos, st, FndMatchInfo)
     or
     (AnInitInfoOnly and (FndMatchInfo.Start > ACurTokenPos))
  then begin
    {$IFDEF TMATE_MATCHING} debugln([''+ClassName+'.NextToken  << no match or init ', FndMatchInfo.Start]); {$ENDIF}
    if AnInitInfoOnly then begin
      // TODO, cache if AnInitInfoOnly
      FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
    end
    else
    if (self is TTextMatePatternCapture) then // ANextTokenPos is end of catpured text + 1
      AStates.CallParentNextToken(AText, ANextTokenPos, ANextTokenPos, True);

    exit;
  end;

  AStates.Add(FndMatchInfo, 1, st^.SubTextLen, st^.Flags * [psfNestedinCapture]);
  FndMatchInfo.Pattern.NextToken(AStates, AText, FndMatchInfo.Start, ANextTokenPos, True);
  assert((ANextTokenPos=FndMatchInfo.Start) or (not AnInitInfoOnly), 'TTextMatePatternBaseNested.NextToken: (ANextTokenPos=FndMatchInfo.Start) or (not AnInitInfoOnly)');
  {$IFDEF TMATE_MATCHING}
  finally debuglnExit(['< '+ClassName+'.NextToken  << ',ANextTokenPos  ]); end;
  {$ENDIF}
end;

function TTextMatePatternBaseNested.DebugDump(AnIndent: Integer;
  AnIncludeNested: Boolean; APrefix: string): string;
var
  i: Integer;
  l: TTextMatePatternArray;
begin
  Result := inherited DebugDump(AnIndent, AnIncludeNested, APrefix);
  if AnIncludeNested then begin
    l := Patterns;
    Patterns := nil;
    for i := 0 to Length(l) - 1 do
      Result := Result + l[i].DebugDump(AnIndent + 2, AnIncludeNested, APrefix);
    Patterns := l;
  end;
end;

{ TTextMatePatternNestedList }

procedure TTextMatePatternNestedList.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  assert(False, 'TTextMatePatternNestedList.InitStates: False');
end;

procedure TTextMatePatternNestedList.FlattenNested(AGrammar: TTextMateGrammar;
  ARemoveMissingIncludes: boolean);
begin
  //
end;

{ TTextMatePatternInclude }

procedure TTextMatePatternInclude.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternInclude absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  FSourceScope := TheOther.FSourceScope;
  FSourceKey   := TheOther.FSourceKey;
end;

function TTextMatePatternInclude.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
begin
  Result := False;
end;

{ TTextMatePatternCapture }

procedure TTextMatePatternCapture.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  assert(False, 'TTextMatePatternCapture.InitStates: can not be on range stack');
end;

procedure TTextMatePatternCapture.InitStateAfterAdded(
  AStateEntry: PTextMatePatternStateEntry);
begin
  inherited InitStateAfterAdded(AStateEntry);
  Include(AStateEntry^.Flags, psfNestedinCapture)
end;

{ TTextMatePatternCaptures }

function TTextMatePatternCaptures.FindCaptureForNextMatchPos(
  ATextStartOffset: integer;
  const AFoundCapturePosList: TTextMateFoundCaptureBoundsArray; out
  ACaptureInfo: TTextMateMatchInfo): boolean;
var
  i: integer;
begin
  Result := False;
  ACaptureInfo.Index := -1;

  ACaptureInfo.Start := high(ACaptureInfo.Start);
  for i := 0 to min(Length(CaptureArray), Length(AFoundCapturePosList)) - 1 do begin
    if (CaptureArray[i] = nil) or
       (AFoundCapturePosList[i].Start < ATextStartOffset) or (AFoundCapturePosList[i].Len <= 0)
    then
      continue;
    Result := True;

    if AFoundCapturePosList[i].Start >= ACaptureInfo.Start then
      continue;

    ACaptureInfo.Start := AFoundCapturePosList[i].Start;
    ACaptureInfo.Pattern := CaptureArray[i];
    ACaptureInfo.Index := i;

    if ACaptureInfo.Start = 1 then
      Break;
  end;
  {$IFDEF TMATE_MATCHING}
  debugln(['TTextMatePatternCaptures.FindCaptureForNextMatchPos ',dbgs(Result), ' pos ', ACaptureInfo.Start, ' idx ',ACaptureInfo.Index]);
  {$ENDIF}
end;

procedure TTextMatePatternCaptures.CopyFrom(AnOther: TTextMatePatternCaptures;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  i: Integer;
begin
  SetLength(CaptureArray, Length(AnOther.CaptureArray));
  for i := 0 to Length(AnOther.CaptureArray) - 1 do
    CaptureArray[i] :=  TTextMatePatternCapture(TTextMatePattern.GetCopyFor(AnOther.CaptureArray[i], AnIndexOffset, ANewList));

end;

{ TTextMatePatternMatch }

procedure TTextMatePatternMatch.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternMatch absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  Captures.CopyFrom(TheOther.Captures, AnIndexOffset, ANewList);
  Match := TheOther.Match;
  DoInitRegex(FRegExMatch, Match, 'match');
end;

procedure TTextMatePatternMatch.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  assert(False, 'Single match can not be on range stack');
end;

function TTextMatePatternMatch.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
begin
  FRegExMatch.SetInputSubString(AText, AStateEntryP^.SubTextBeginPos, AStateEntryP^.SubTextLen);

  Result := FRegExMatch.ExecPos(ATextStartOffset, AMatchMustStartBefore) and (FRegExMatch.MatchLen[0] > 0);
  if Result then
    AFoundStartPos := FRegExMatch.MatchPos[0];
  APattern := Self;
end;

destructor TTextMatePatternMatch.Destroy;
begin
  FreeAndNil(FRegExMatch);
  inherited Destroy;
end;

procedure TTextMatePatternMatch.InitRegEx;
begin
  DoInitRegex(FRegExMatch, Match, 'match');
end;

procedure TTextMatePatternMatch.NextToken(var AStates: TTextMatePatternState;
  const AText: String; ACurTokenPos: integer; out ANextTokenPos: integer;
  AnInitInfoOnly: Boolean);
var
  st: PTextMatePatternStateEntry;
  CaptTextLen, MatchEndPos: Integer;
  r: Boolean;
  FndCapture: TTextMateMatchInfo;
begin
  {$IFDEF TMATE_MATCHING}
  debuglnEnter(['> TTextMatePatternMatch.NextToken ',DebugName,' --- TkPos:', ACurTokenPos, ' ',dbgs(AnInitInfoOnly), '  txt-len:',AStates.EntryP[0]^.SubTextLen,'/',length(AText) ]); try
  {$ENDIF}
  if IsEndlessRecursion(ACurTokenPos, AnInitInfoOnly) then begin
    if AnInitInfoOnly
    then ANextTokenPos := ACurTokenPos
    else AStates.CallParentNextToken(AText, Length(AText) + 1, ANextTokenPos, False);
    exit;
  end;

  st := AStates.EntryP[0];
  st^.CurrentTokenAttribInfo := FAttribInfo;

  if not (psfMatchBeginInitialized in st^.Flags) then begin
    if FRegExMatch.MatchPos[0] <> ACurTokenPos then begin
      // In case the pattern was called, since the parent did match it
      GetFirstMatchPos(AText, ACurTokenPos, st, FndCapture.Pattern, FndCapture.Start);
      assert(FRegExMatch.MatchPos[0]=ACurTokenPos, 'TTextMatePatternBeginEnd.NextToken: FRegExMatch.MatchPos[0]=ACurTokenPos');
    end;
    InitFoundCaptures(st, FRegExMatch);
    Include(st^.Flags, psfMatchBeginInitialized);
  end;

  MatchEndPos := st^.FoundCaptures[0].Start + st^.FoundCaptures[0].Len;

  if (ACurTokenPos >= MatchEndPos) or
     (ACurTokenPos > st^.SubTextLen)
  then begin
    assert(ACurTokenPos = MatchEndPos, 'TTextMatePatternMatch.NextToken: ACurTokenPos = MatchEndPos');
    {$IFDEF TMATE_MATCHING}debugln(['EXIT - because curpos ALREADY was TOO FAR']); {$ENDIF}
    AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
    exit;
  end;

  r := st^.GetCache(FndCapture, CaptTextLen);
  if not r then begin
    CaptTextLen := MatchEndPos-1; // TODO: cache
    if ACurTokenPos < MatchEndPos then begin
      r := Captures.FindCaptureForNextMatchPos(ACurTokenPos, st^.FoundCaptures, FndCapture);
      if r then
        CaptTextLen := FndCapture.Start + st^.FoundCaptures[FndCapture.Index].Len - 1;
    end;
  end;

  if AnInitInfoOnly
  then ANextTokenPos := ACurTokenPos
  else ANextTokenPos := CaptTextLen + 1;

  if (not r) or (AnInitInfoOnly and (FndCapture.Start > ACurTokenPos))
  then begin
    if AnInitInfoOnly then begin
      if r then
        st^.SetCache(FndCapture, CaptTextLen);
      FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
    end
    else begin
      assert(ANextTokenPos >= MatchEndPos, 'TTextMatePatternMatch.NextToken: ANextTokenPos >= MatchEndPos');
      AStates.CallParentNextToken(AText, MatchEndPos, ANextTokenPos, True);
    end;
    exit;
  end;

  assert((FndCapture.Pattern <> nil) and (CaptTextLen > 0), 'TTextMatePatternMatch.NextToken: (FndCapture.Pattern <> nil) and (CaptTextLen <> '')');
  AStates.Add(FndCapture, 1, CaptTextLen);
  FndCapture.Pattern.NextToken(AStates, AText, FndCapture.Start, ANextTokenPos, True);
  assert((ANextTokenPos=FndCapture.Start) or (not AnInitInfoOnly), 'TTextMatePatternRoot.NextToken: (ANextTokenPos=FndCapture.Bounds.Start) or (not AnInitInfoOnly)');
  {$IFDEF TMATE_MATCHING}
  finally debuglnExit(['< TTextMatePatternMatch.NextToken ' ]); end;
  {$ENDIF}
end;

{ TTextMatePatternBeginEnd }

procedure TTextMatePatternBeginEnd.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternBeginEnd absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  ContentName := TheOther.ContentName;
  ContentAttribInfo := TheOther.ContentAttribInfo;
  EndPatternLast := TheOther.EndPatternLast;
  Captures.CopyFrom(TheOther.Captures, AnIndexOffset, ANewList);
  CapturesBegin.CopyFrom(TheOther.CapturesBegin, AnIndexOffset, ANewList);
  CapturesEnd.CopyFrom(TheOther.CapturesEnd, AnIndexOffset, ANewList);
  MatchBegin := TheOther.MatchBegin;
  MatchEnd := TheOther.MatchEnd;
  DoInitRegex(FRegExMatchBegin, MatchBegin, 'matchBegin');
  DoInitRegex(FRegExMatchEnd, MatchEnd, 'matchEnd');
end;

procedure TTextMatePatternBeginEnd.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  inherited InitStates(AGrammar, AStates, AParent, AText, ADepth);

  AStates.StateList[AStates.StateIdx - ADepth].CurrentTokenAttribInfo := ContentAttribInfo;
  AStates.StateList[AStates.StateIdx - ADepth].Flags := [psfMatchBeginDone];
end;

function TTextMatePatternBeginEnd.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
var
  AMatchInfo: TTextMateMatchInfo;
begin
  if ATextStartOffset = FRecursiveFirstMatchPos then
    exit(False);

  FRegExMatchBegin.SetInputSubString(AText, AStateEntryP^.SubTextBeginPos, AStateEntryP^.SubTextLen);
  Result := FRegExMatchBegin.ExecPos(ATextStartOffset, AMatchMustStartBefore);
  if Result then begin
    AFoundStartPos := FRegExMatchBegin.MatchPos[0];
    APattern := Self;
    // check for zero length
    if FRegExMatchBegin.MatchLen[0] <= 0 then begin
      FRegExMatchEnd.SetInputSubString(AText, AStateEntryP^.SubTextBeginPos, AStateEntryP^.SubTextLen);
      if FRegExMatchEnd.ExecPos(ATextStartOffset) and
         (FRegExMatchEnd.MatchPos[0] = ATextStartOffset) and (FRegExMatchEnd.MatchLen[0] <=0)
      then begin
        if EndPatternLast then begin
          FRecursiveFirstMatchPos := ATextStartOffset;
          try
            if TTextMatePatternBaseNested(AStateEntryP^.Pattern).FindPatternForNextMatchPos(AText, ATextStartOffset, AStateEntryP, AMatchInfo) and
               (AMatchInfo.Start = ATextStartOffset)
            then
              Result := False;
          finally
            FRecursiveFirstMatchPos := 0;
          end;
        end
        else
          Result := False;
      end;
    end;
  end;
end;

destructor TTextMatePatternBeginEnd.Destroy;
begin
  FreeAndNil(FRegExMatchBegin);
  FreeAndNil(FRegExMatchEnd);
  inherited Destroy;
end;

procedure TTextMatePatternBeginEnd.InitRegEx;
begin
  DoInitRegex(FRegExMatchBegin, MatchBegin, 'matchBegin');
  DoInitRegex(FRegExMatchEnd,   MatchEnd, 'matchEnd');
end;

procedure TTextMatePatternBeginEnd.NextToken(
  var AStates: TTextMatePatternState; const AText: String;
  ACurTokenPos: integer; out ANextTokenPos: integer; AnInitInfoOnly: Boolean);
var
  st: PTextMatePatternStateEntry;
  FndCapture, FndCapture2: TTextMateMatchInfo;
  r: Boolean;
  BodyPos, EndPos, EndEnd: Integer;
  CaptTextLen: Integer;
begin
  {$IFDEF TMATE_MATCHING}
  debuglnEnter(['> TTextMatePatternBeginEnd.NextToken ',DebugName,' --- TkPos:', ACurTokenPos, ' ',dbgs(AnInitInfoOnly), '  txt-len:',AStates.EntryP[0]^.SubTextLen,'/',length(AText) ]); try
  {$ENDIF}
  if IsEndlessRecursion(ACurTokenPos, AnInitInfoOnly) then begin
    if AnInitInfoOnly
    then ANextTokenPos := ACurTokenPos
    else AStates.CallParentNextToken(AText, Length(AText) + 1, ANextTokenPos, False);
    exit;
  end;

  st := AStates.EntryP[0];
  st^.CurrentTokenAttribInfo := FAttribInfo;

  BodyPos := 0;

  r := st^.GetCache(FndCapture, CaptTextLen);
  assert(not(r and AnInitInfoOnly), 'TTextMatePatternBeginEnd.NextToken: not(r and AnInitInfoOnly)');

  (*  Check matchBegin  *)
  if (not (psfMatchBeginDone in st^.Flags)) then begin
    {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken >> beginMatch']); {$ENDIF}
    if not (psfMatchBeginInitialized in st^.Flags) then begin
      if FRegExMatchBegin.MatchPos[0] <> ACurTokenPos then begin
        // In case the pattern was called, since the parent did match it
        GetFirstMatchPos(AText, ACurTokenPos, st, FndCapture2.Pattern, FndCapture2.Start);
        assert(FRegExMatchBegin.MatchPos[0]=ACurTokenPos, 'TTextMatePatternBeginEnd.NextToken: FRegExMatchBegin.MatchPos[0]=ACurTokenPos');
      end;
      InitFoundCaptures(st, FRegExMatchBegin);
      Include(st^.Flags, psfMatchBeginInitialized);
    end;

    BodyPos := st^.FoundCaptures[0].Start + st^.FoundCaptures[0].Len;
    if (ACurTokenPos < BodyPos) and not r then begin
      CaptTextLen := BodyPos - 1; // TODO: cache
      r := CapturesBegin.FindCaptureForNextMatchPos(ACurTokenPos, st^.FoundCaptures, FndCapture);
      if Captures.FindCaptureForNextMatchPos(ACurTokenPos, st^.FoundCaptures, FndCapture2) then
        r := LeftmostFoundPattern(FndCapture, FndCapture2);
      if r then
        CaptTextLen := FndCapture.Start + st^.FoundCaptures[FndCapture.Index].Len - 1;

      if AnInitInfoOnly and ( (not r) or (FndCapture.Start > ACurTokenPos) ) then begin
        if r then
          st^.SetCache(FndCapture, CaptTextLen);
        ANextTokenPos := ACurTokenPos;
        FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
        {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken exit inside MatchBegin ',BodyPos  ]); {$ENDIF}
        exit;
      end;
    end;
  end;

  (*  Check body / nested patterns  *)
  if (not r) and (BodyPos <= st^.SubTextLen) then begin
    Include(st^.Flags, psfMatchBeginDone);
    CaptTextLen := st^.SubTextLen;
    if BodyPos < ACurTokenPos then
      BodyPos := ACurTokenPos;

    if not (psfMatchEndInitialized in st^.Flags) then begin
      if ContentAttribInfo.TokId >= 0 then
        st^.CurrentTokenAttribInfo := ContentAttribInfo;
      assert(st^.Pattern is TTextMatePatternBaseNested, 'TTextMatePatternBeginEnd.NextToken: st^.Pattern is TTextMatePatternBaseNested');
      r := TTextMatePatternBaseNested(st^.Pattern).FindPatternForNextMatchPos(AText, BodyPos, st, FndCapture);
      {$IFDEF TMATE_MATCHING} debugln(r, ['TTextMatePatternBeginEnd.NextToken  FOUND BODY PTRN ', FndCapture.Start]); {$ENDIF}

      (*  Check matchEnd  *)
      // TODO: \1 to refer to begin pattern
      FRegExMatchEnd.SetInputSubString(AText, st^.SubTextBeginPos, st^.SubTextLen);
      if FRegExMatchEnd.ExecPos(BodyPos) then begin
        EndPos := FRegExMatchEnd.MatchPos[0];
        {$IFDEF TMATE_MATCHING} debugln(r, ['TTextMatePatternBeginEnd.NextToken  FOUND END PTRN ', EndPos]); {$ENDIF}

        if (not r) or (FndCapture.Start > EndPos) or
           ( (not EndPatternLast) and (FndCapture.Start >= EndPos) )
        then begin
          InitFoundCaptures(st, FRegExMatchEnd);
          Include(st^.Flags, psfMatchEndInitialized);
        end;
      end;
    end
    else
    // End pattern was found as next, there are no more "Patterns" from the content
    if (ContentAttribInfo.TokId >= 0) and (ACurTokenPos < st^.FoundCaptures[0].Start) then
      st^.CurrentTokenAttribInfo := ContentAttribInfo;
  end;

  (*  Check matchEnd captures  *)
  if psfMatchEndInitialized in st^.Flags then begin
    EndPos := st^.FoundCaptures[0].Start;
    EndEnd := EndPos + st^.FoundCaptures[0].Len;
    if ACurTokenPos > EndPos then
      EndPos := ACurTokenPos;
    {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken  END ',EndPos,'-',EndEnd]); {$ENDIF}

    if ACurTokenPos >= EndEnd then begin
      {$IFDEF TMATE_MATCHING} debugln(['EXIT - because curpos ALREADY was TOO FAR']); {$ENDIF}
      AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
        exit;
    end;

    r := CapturesEnd.FindCaptureForNextMatchPos(EndPos, st^.FoundCaptures, FndCapture);
    if Captures.FindCaptureForNextMatchPos(EndPos, st^.FoundCaptures, FndCapture2)
    then
      r := LeftmostFoundPattern(FndCapture, FndCapture2);
    if r then
      CaptTextLen := FndCapture.Start + st^.FoundCaptures[FndCapture.Index].Len - 1;
  end;


  if (ContentAttribInfo.TokId >= 0) and (ContentAttribInfo.TokId <> FAttribInfo.TokId) and
     ( (not (psfMatchEndInitialized in st^.Flags)) or (EndPos > BodyPos) ) and  // Not yet in matchEnd regex
     not AnInitInfoOnly
  then begin
    if (ACurTokenPos < BodyPos) and
       ( (not r) or (FndCapture.Start > BodyPos) )
    then begin
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken exit after MatchBegin ',BodyPos  ]); {$ENDIF}
      ANextTokenPos := BodyPos;
      if (psfNestedinCapture in st^.Flags) and (ANextTokenPos > st^.SubTextLen) then
        AStates.CallParentNextToken(AText, ANextTokenPos, ANextTokenPos, True);
      exit;
    end;

    if (ACurTokenPos < EndPos) and (psfMatchEndInitialized in st^.Flags) and
       ( (not r) or (FndCapture.Start > EndPos) )
    then begin
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken  exit before matchEnd', EndPos]); {$ENDIF}
      ANextTokenPos := EndPos;
      st^.CurrentTokenAttribInfo := FAttribInfo;
      if (psfNestedinCapture in st^.Flags) and (ANextTokenPos > st^.SubTextLen) then
        AStates.CallParentNextToken(AText, ANextTokenPos, ANextTokenPos, True);
      exit;
    end;
  end;

  if AnInitInfoOnly
  then ANextTokenPos := ACurTokenPos
  else ANextTokenPos := st^.SubTextLen+ 1;

  if (psfNestedinCapture in st^.Flags) and (ACurTokenPos > st^.SubTextLen) then begin
    assert(ACurTokenPos=st^.SubTextLen+1, 'TTextMatePatternBeginEnd.NextToken: ACurTokenPos=st^.SubTextLen+1');
    {$IFDEF TMATE_MATCHING} debugln(['EXIT - because curpos ALREADY was TOO FAR']); {$ENDIF}
    AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
    exit;
  end;

  if (psfMatchEndInitialized in st^.Flags) and (ANextTokenPos >= EndPos) then
    st^.CurrentTokenAttribInfo := FAttribInfo;

  if (not r) or (AnInitInfoOnly and (FndCapture.Start > ACurTokenPos))
  then begin
    if AnInitInfoOnly then begin
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken exit for init']); {$ENDIF}
      if r then
        st^.SetCache(FndCapture, CaptTextLen);
      FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
    end
    else
    if (psfMatchEndInitialized in st^.Flags) then begin
      assert(ANextTokenPos >= EndEnd, 'TTextMatePatternBeginEnd.NextToken: ANextTokenPos >= EndEnd');
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken  EXIT GOT TO END ', ANextTokenPos, ' ' ,EndEnd]); {$ENDIF}
      AStates.CallParentNextToken(AText, EndEnd, ANextTokenPos, True);
    end
    else
    if (psfNestedinCapture in st^.Flags) and (ANextTokenPos > st^.SubTextLen)then begin
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken  EXIT GOT TO LENGTH ', ANextTokenPos, ' ' ,EndEnd]); {$ENDIF}
      AStates.CallParentNextToken(AText, ANextTokenPos, ANextTokenPos, True);
    end;
    exit;
  end;

  AStates.Add(FndCapture, 1, CaptTextLen);
  FndCapture.Pattern.NextToken(AStates, AText, FndCapture.Start, ANextTokenPos, True);
  assert((ANextTokenPos=FndCapture.Start) or (not AnInitInfoOnly), 'TTextMatePatternRoot.NextToken: (ANextTokenPos=FndCapture.Bounds.Start) or (not AnInitInfoOnly)');

  {$IFDEF TMATE_MATCHING}
  finally debuglnExit(['< TTextMatePatternBeginEnd.NextToken << ',ANextTokenPos ]); end;
  {$ENDIF}
end;

{ TTextMatePatternBeginWhile }

procedure TTextMatePatternBeginWhile.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternBeginWhile absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  ContentName := TheOther.ContentName;
  ContentAttribInfo := TheOther.ContentAttribInfo;
  Captures.CopyFrom(TheOther.Captures, AnIndexOffset, ANewList);
  CapturesBegin.CopyFrom(TheOther.CapturesBegin, AnIndexOffset, ANewList);
  CapturesWhile.CopyFrom(TheOther.CapturesWhile, AnIndexOffset, ANewList);
  MatchBegin := TheOther.MatchBegin;
  MatchWhile := TheOther.MatchWhile;
  DoInitRegex(FRegExMatchBegin, MatchBegin, 'matchBegin');
  DoInitRegex(FRegExMatchWhile, MatchWhile, 'MatchWhile');
end;

procedure TTextMatePatternBeginWhile.InitStates(
  const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState;
  AParent: TTextMatePattern; const AText: String; ADepth: Integer);
begin
  inherited InitStates(AGrammar, AStates, AParent, AText, ADepth);

  AStates.StateList[AStates.StateIdx - ADepth].CurrentTokenAttribInfo := ContentAttribInfo;
end;

procedure TTextMatePatternBeginWhile.InitStateAfterAdded(
  AStateEntry: PTextMatePatternStateEntry);
begin
  inherited InitStateAfterAdded(AStateEntry);
  Include(AStateEntry^.Flags, psfWhileInBeginLine);
end;

function TTextMatePatternBeginWhile.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
begin
  FRegExMatchBegin.SetInputSubString(AText, AStateEntryP^.SubTextBeginPos, AStateEntryP^.SubTextLen);
  Result := FRegExMatchBegin.ExecPos(ATextStartOffset, AMatchMustStartBefore);
  if Result then
    AFoundStartPos := FRegExMatchBegin.MatchPos[0];
  APattern := Self;
end;

destructor TTextMatePatternBeginWhile.Destroy;
begin
  FreeAndNil(FRegExMatchBegin);
  FreeAndNil(FRegExMatchWhile);
  inherited Destroy;
end;

procedure TTextMatePatternBeginWhile.InitRegEx;
begin
  DoInitRegex(FRegExMatchBegin, MatchBegin, 'matchBegin');
  DoInitRegex(FRegExMatchWhile, MatchWhile, 'matchWhile');
end;

procedure TTextMatePatternBeginWhile.NextToken(
  var AStates: TTextMatePatternState; const AText: String;
  ACurTokenPos: integer; out ANextTokenPos: integer; AnInitInfoOnly: Boolean);
var
  st: PTextMatePatternStateEntry;
  BodyPos, CaptTextLen: Integer;
  MatchCaptures: TTextMatePatternCaptures;
  FndCapture, FndCapture2: TTextMateMatchInfo;
  r: Boolean;
begin
  {$IFDEF TMATE_MATCHING}
  debuglnEnter(['> TTextMatePatternBeginWhile.NextToken ',DebugName,' --- TkPos:', ACurTokenPos, ' ',dbgs(AnInitInfoOnly), '  txt-len:',AStates.EntryP[0]^.SubTextLen,'/',length(AText) ]); try
  {$ENDIF}
  if IsEndlessRecursion(ACurTokenPos, AnInitInfoOnly) then begin
    if AnInitInfoOnly
    then ANextTokenPos := ACurTokenPos
    else AStates.CallParentNextToken(AText, Length(AText) + 1, ANextTokenPos, False);
    exit;
  end;

  st := AStates.EntryP[0];
  st^.CurrentTokenAttribInfo := FAttribInfo;

  if (psfNestedinCapture in st^.Flags) and (ACurTokenPos > st^.SubTextLen) then begin
    AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
    exit;
  end;

  if (psfWhileInBeginLine in st^.Flags) then begin
    if not (psfMatchBeginInitialized in st^.Flags) then begin
      if FRegExMatchBegin.MatchPos[0] <> ACurTokenPos then begin
        // In case the pattern was called, since the parent did match it
        GetFirstMatchPos(AText, ACurTokenPos, st, FndCapture2.Pattern, FndCapture2.Start);
        assert(FRegExMatchBegin.MatchPos[0]=ACurTokenPos, 'TTextMatePatternBeginEnd.NextToken: FRegExMatchBegin.MatchPos[0]=ACurTokenPos');
      end;
      InitFoundCaptures(st, FRegExMatchBegin);
      Include(st^.Flags, psfMatchBeginInitialized);
    end;

    MatchCaptures := CapturesBegin;
  end
  else begin
    // Continuation line
    if not (psfWhileDone in st^.Flags) then begin
      Include(st^.Flags, psfWhileDone);
      FRegExMatchWhile.SetInputSubString(AText, st^.SubTextBeginPos, st^.SubTextLen);
      if not FRegExMatchWhile.ExecPos(ACurTokenPos) then begin
        AStates.CallParentNextToken(AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
        exit;
      end;

      InitFoundCaptures(st, FRegExMatchWhile);
      Include(st^.Flags, psfMatchBeginInitialized);
    end;

    MatchCaptures := CapturesWhile;
  end;


  if (not (psfMatchBeginDone in st^.Flags)) then begin
    BodyPos := st^.FoundCaptures[0].Start + st^.FoundCaptures[0].Len;
    if (ACurTokenPos < BodyPos) then begin
      CaptTextLen := BodyPos - 1; // TODO: cache
      r := MatchCaptures.FindCaptureForNextMatchPos(ACurTokenPos, st^.FoundCaptures, FndCapture);
      if Captures.FindCaptureForNextMatchPos(ACurTokenPos, st^.FoundCaptures, FndCapture2) then
        r := LeftmostFoundPattern(FndCapture, FndCapture2);
      if r then
        CaptTextLen := FndCapture.Start + st^.FoundCaptures[FndCapture.Index].Len - 1;

      if AnInitInfoOnly and ( (not r) or (FndCapture.Start > ACurTokenPos) ) then begin
        //if r then
        //  st^.SetCache(FndCapture, CaptTextLen);
        ANextTokenPos := ACurTokenPos;
        FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
        {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken exit inside MatchBegin ',BodyPos  ]); {$ENDIF}
        exit;
      end;
    end;
  end;

  if (not r) and (ACurTokenPos >=  BodyPos) then begin
    Include(st^.Flags, psfMatchBeginDone);
    if ContentAttribInfo.TokId >= 0 then
      st^.CurrentTokenAttribInfo := ContentAttribInfo;
  end;


  if AnInitInfoOnly
  then ANextTokenPos := ACurTokenPos
  else ANextTokenPos := st^.SubTextLen+ 1;

  if not r then begin
    if AnInitInfoOnly then begin
      FDeepestRecurseWasInit := True; // Next call must be NOT AnInitInfoOnly
    end
    else
    if (psfNestedinCapture in st^.Flags) and (ANextTokenPos > st^.SubTextLen)then begin
      {$IFDEF TMATE_MATCHING} debugln(['TTextMatePatternBeginEnd.NextToken  EXIT GOT TO LENGTH ', ANextTokenPos, ' ' ,EndEnd]); {$ENDIF}
      AStates.CallParentNextToken(AText, ANextTokenPos, ANextTokenPos, True);
    end;
    exit;
  end;

  AStates.Add(FndCapture, 1, CaptTextLen);
  FndCapture.Pattern.NextToken(AStates, AText, FndCapture.Start, ANextTokenPos, True);

  {$IFDEF TMATE_MATCHING}
  finally debuglnExit(['< TTextMatePatternBeginWhile.NextToken ' ]); end;
  {$ENDIF}
end;

{ TTextMatePatternRoot }

procedure TTextMatePatternRoot.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternRoot absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  ScopeName := TheOther.ScopeName;
end;

procedure TTextMatePatternRoot.InitStates(const AGrammar: TTextMateGrammar;
  var AStates: TTextMatePatternState; AParent: TTextMatePattern;
  const AText: String; ADepth: Integer);
begin
  AStates.InitForDepth(ADepth + 1);
  AStates.StateList[AStates.StateIdx - ADepth].Pattern := self;
  AStates.StateList[AStates.StateIdx - ADepth].SubTextBeginPos := 1;
  AStates.StateList[AStates.StateIdx - ADepth].SubTextLen := Length(AText);
  AStates.StateList[AStates.StateIdx - ADepth].CurrentTokenAttribInfo := FAttribInfo;
  AStates.StateList[AStates.StateIdx - ADepth].Flags := [];
  AStates.StateList[AStates.StateIdx - ADepth].Flags := [];
  AStates.StateList[AStates.StateIdx - ADepth].ClearCache;
  FDeepestRecurseTextAtOffset := 0;
end;

{ TTextMatePatternForwarder }

function TTextMatePatternForwarder.GetForwardTarget: TTextMatePatternBaseNested;
begin
  Result := FForwardTo.GetForwardTarget;
end;

procedure TTextMatePatternForwarder.InitStates(
  const AGrammar: TTextMateGrammar; var AStates: TTextMatePatternState;
  AParent: TTextMatePattern; const AText: String; ADepth: Integer);
begin
  // Call with own FParent
  FForwardTo.InitStates(AGrammar, AStates, FParent, AText, ADepth);

  AStates.StateList[AStates.StateIdx - ADepth].Pattern := Self;
end;

function TTextMatePatternForwarder.GetFirstMatchPos(const AText: String;
  const ATextStartOffset: integer; AStateEntryP: PTextMatePatternStateEntry;
  out APattern: TTextMatePattern; out AFoundStartPos: integer;
  AMatchMustStartBefore: integer): Boolean;
begin
  Result := FForwardTo.GetFirstMatchPos(AText, ATextStartOffset, AStateEntryP,
    APattern, AFoundStartPos, AMatchMustStartBefore);
  if APattern = FForwardTo then
    APattern := Self;
end;

procedure TTextMatePatternForwarder.CopyFrom(AnOther: TTextMatePattern;
  AnIndexOffset: Integer; ANewList: TTextMatePatternList);
var
  TheOther: TTextMatePatternForwarder absolute AnOther;
begin
  inherited CopyFrom(AnOther, AnIndexOffset, ANewList);
  FForwardTo := TTextMatePatternForwarder(GetCopyFor(TheOther.FForwardTo, AnIndexOffset, ANewList));
end;

procedure TTextMatePatternForwarder.ClearDeepestRecurseData;
begin
  inherited ClearDeepestRecurseData;
  FForwardTo.ClearDeepestRecurseData;
end;

procedure TTextMatePatternForwarder.NextToken(
  var AStates: TTextMatePatternState; const AText: String;
  ACurTokenPos: integer; out ANextTokenPos: integer; AnInitInfoOnly: Boolean);
begin
  FForwardTo.NextToken(AStates, AText, ACurTokenPos, ANextTokenPos, AnInitInfoOnly);
end;

function TTextMatePatternForwarder.DebugDump(AnIndent: Integer;
  AnIncludeNested: Boolean; APrefix: string): string;
var
  i: Integer;
begin
  Result := FForwardTo.DebugDump(AnIndent, False, '[' + IntToStr(FMainIndex) + ']=>' + APrefix);
  if AnIncludeNested then
    for i := 0 to Length(Patterns) - 1 do
      Result := Result + Patterns[i].DebugDump(AnIndent + 2, AnIncludeNested, APrefix);
end;

{ TTextMateGrammar }

procedure TTextMateGrammar.ClearAttributeInfo(
  var AnAttribInfo: TSynAttributeInfo);
begin
  AnAttribInfo.TokId := -1;
  AnAttribInfo.TokObject := nil;
end;

function TTextMateGrammar.CreatePatternObject(
  AClass: TTextMatePatternClass): TTextMatePattern;
begin
  if FMainPatternList.Count = FMainPatternList.Capacity then
    FMainPatternList.Capacity := FMainPatternList.Capacity +
    Min(Max(250, FMainPatternList.Capacity shr 2), 8000);
  Result := AClass.Create;
  Result.FMainIndex := FMainPatternList.Add(Result);
end;

procedure TTextMateGrammar.DoOtherGrammarFreed(Sender: TObject);
var
  i: Integer;
begin
  ClearGrammar;
  i := FOtherGrammars.IndexOfData(TTextMateGrammar(Sender));
  if i >= 0 then
    FOtherGrammars.Delete(i);
end;

function TTextMateGrammar.GetJson(AGrammarDef: String): TJSONObject;
var
  i: Integer;
  R: TJSONData;
  stream: TStringStream;
  AXml: TXMLDocument;
  jparser: TJSONParser;
begin
  Result := nil;
  R := nil;
  FParserError := 'Unknown Error';
  if AGrammarDef = '' then
    exit;

  try
    i := 1;
    while AGrammarDef[i] in [' ', #9] do inc(i);
    if AGrammarDef[i] = '<' then begin
      stream := nil;
      AXml := nil;
      try
        stream := TStringStream.Create(AGrammarDef);
        ReadXMLFile(AXml, stream);
        R := PListXml2Json(AXml);
      finally
        FreeAndNil(AXml);
        FreeAndNil(stream);
      end;
    end
    else begin
      jparser := TJSONParser.Create(AGrammarDef, [joUTF8,joComments,joIgnoreTrailingComma
          {$IF FPC_VERSION >= 030202} , joBOMCheck {$ENDIF}
          {,joIgnoreDuplicates}]);
      try
        R := jparser.Parse;
      finally
        jparser.Free;
      end;
    end;
  except
    on E: Exception do begin
      R := nil;
      FParserError := 'Error: ' + E.message;
    end;
  end;

  if R is TJSONObject then
    Result := TJSONObject(R);
  if Result <> nil then
    FParserError := '';
end;

procedure TTextMateGrammar.ParseLanguage(ALangDef: TJSONData);
begin
  //
end;

function TTextMateGrammar.IsInclude(APatternJson: TJSONObject): boolean;
begin
  Result := APatternJson.IndexOfName('include') >= 0;
end;

function TTextMateGrammar.IncludeName(APatternJson: TJSONObject; out
  AnOtherScopeName: string): string;
var
  j: TJSONData;
  i: SizeInt;
begin
  j := APatternJson['include'];
  if not (j is TJSONString) then
    raise TTextMateGrammarException.Create('include is not string', 'include');
  Result := TJSONString(APatternJson['include']).AsString;

  i := pos('#', Result);
  if i < 1 then i := Length(Result)+1;
  AnOtherScopeName := copy(Result, 1, i-1);
  system.Delete(Result, 1, i);
  if (Result = '') and (AnOtherScopeName = '') then
    raise TTextMateGrammarException.Create('invalid include without ref', 'include');
  if AnOtherScopeName = '$self' then
    AnOtherScopeName := '';
  if (AnOtherScopeName <> '') and (FOtherGrammars.IndexOf(AnOtherScopeName) < 0) then
    FOtherGrammars.Add(AnOtherScopeName);
end;

function TTextMateGrammar.ResolveInclude(APatternJson: TJSONObject
  ): TTextMatePattern;
var
  n, OtherScopeName: String;
begin
  Result := nil;
  n := IncludeName(APatternJson, OtherScopeName);
  if OtherScopeName <> '' then begin
    Result := CreatePatternObject(TTextMatePatternInclude);
    TTextMatePatternInclude(Result).FSourceScope := OtherScopeName;
    TTextMatePatternInclude(Result).FSourceKey := n;
    exit
  end;

  if n = '' then begin
    Result := FRootPattern;
  end
  else
  begin
    if FPatternRepo.IndexOf(n) >= 0 then
      Result := FPatternRepo[n];
  end;
  if Result = nil then begin
    debugln('unknown include ' + n);

    if FTheEmptyPattern = nil then
      FTheEmptyPattern := TTextMatePatternNestedList.Create;
    Result := FTheEmptyPattern;
    if FMissingIncludes <> '' then FMissingIncludes := FMissingIncludes + ', ';
    FMissingIncludes := FMissingIncludes + n;
  end;
end;

procedure TTextMateGrammar.CopyPatterns(AnOtherGrammar: TTextMateGrammar;
  AnIncludeName: String);
var
  InsPos, i: Integer;
  k: String;
  o: TTextMatePattern;
begin
  InsPos := FMainPatternList.Count;
  for i := 0 to AnOtherGrammar.FMainPatternCount - 1 do
    CreatePatternObject(TTextMatePatternClass(AnOtherGrammar.FMainPatternList[i].ClassType));

  for i := 0 to AnOtherGrammar.FMainPatternCount - 1 do
    FMainPatternList[InsPos + i].CopyFrom(AnOtherGrammar.FMainPatternList[i], InsPos, FMainPatternList);

  FPatternRepo.Add(AnIncludeName+'#', AnOtherGrammar.RootPattern);
  for i := 0 to AnOtherGrammar.FPatternRepo.Count - 1 do begin
    k := AnOtherGrammar.FPatternRepo.Keys[i];
    if pos('#', k) > 0 then
      continue;
    o := TTextMatePattern.GetCopyFor(AnOtherGrammar.FPatternRepo.Data[i], InsPos, FMainPatternList);
    FPatternRepo.Add(AnIncludeName+'#'+k, o);
  end;
end;

function TTextMateGrammar.CreatePattern(AParent: TTextMatePattern;
  APatternJson: TJSONObject; AllowPatternOnly: Boolean): TTextMatePattern;
begin
  if APatternJson.IndexOfName('while') >= 0 then begin
    if APatternJson.IndexOfName('begin') < 0 then
      raise TTextMateGrammarException.Create('invalid pattern - missing begin for while');
    Result := CreatePatternObject(TTextMatePatternBeginWhile);
  end
  else
  if APatternJson.IndexOfName('end') >= 0 then begin
    if APatternJson.IndexOfName('begin') < 0 then
      raise TTextMateGrammarException.Create('invalid pattern - missing begin for end');
    Result := CreatePatternObject(TTextMatePatternBeginEnd);
  end
  else
  if APatternJson.IndexOfName('match') >= 0 then begin
    Result := CreatePatternObject(TTextMatePatternMatch);
  end
  else
  if AllowPatternOnly and (APatternJson.IndexOfName('patterns') >= 0) then begin
    Result := CreatePatternObject(TTextMatePatternNestedList);
  end
  else
    raise TTextMateGrammarException.Create('invalid pattern');
end;

procedure TTextMateGrammar.ReadPattern(APattern: TTextMatePattern;
  APatternJson: TJSONObject);

  // Each capture must have a () in the reg-ex, therefore the max idx is length(regex)/2
  procedure ReadCaptures(AParent: TTextMatePattern; var ACapturesArray: TTextMatePatternCaptureArray;
    AnOwner: TJSONObject; AKey: String; AMaxIndex: integer);
  var
    cnt, i, j: Integer;
    CaptObjJson, CaptItemJson: TJSONObject;
  begin
    CaptObjJson := jsKeyAsDict(AnOwner, AKey);
    //inc(FParsingCaptures);

    cnt := AMaxIndex div 2 + 1;
    SetLength(ACapturesArray, cnt);
    j := -1;
    for i := 0 to cnt-1 do begin
      try
//        ACapturesArray[i] := nil;
        if CaptObjJson.IndexOfName(IntToStr(i)) < 0 then
          continue;
        CaptItemJson :=  jsKeyAsDict(CaptObjJson, IntToStr(i));
        if i > j then
          j := i;
        ACapturesArray[i] := TTextMatePatternCapture(CreatePatternObject(TTextMatePatternCapture));
        ACapturesArray[i].FName := jsKeyAsString(CaptItemJson, 'name');
        if assigned(FOnPopulateAttributeInfo) and (ACapturesArray[i].FName <> '') then
          FOnPopulateAttributeInfo(Self, ACapturesArray[i], ACapturesArray[i].FName, ACapturesArray[i].FAttribInfo)
        else
          ClearAttributeInfo(ACapturesArray[i].FAttribInfo);
        if CaptItemJson.IndexOfName('patterns') >= 0 then
          ParsePatterns(AParent, ACapturesArray[i].Patterns, jsKeyAsArray(CaptItemJson, 'patterns'));
      except
        on E: TTextMateGrammarException do raise E.Copy.AddLocation(i).AddLocation(AKey);
        on E: Exception do raise TTextMateGrammarException.Create(E.Message, i).AddLocation(AKey);
      end;
    end;
    SetLength(ACapturesArray, j+1);
    //dec(FParsingCaptures);
  end;

var
  ptrnBW: TTextMatePatternBeginWhile absolute APattern;
  ptrnBE: TTextMatePatternBeginEnd   absolute APattern;
  ptrnM:  TTextMatePatternMatch      absolute APattern;
begin
  if APattern is TTextMatePatternBeginWhile then begin
    ptrnBW.MatchBegin  := jsKeyAsString(APatternJson, 'begin');
    ptrnBW.MatchWhile  := jsKeyAsString(APatternJson, 'while');
    ptrnBW.ContentName := jsKeyAsString(APatternJson, 'contentName');
    if assigned(FOnPopulateAttributeInfo) and (ptrnBW.ContentName <> '') then
      FOnPopulateAttributeInfo(Self, ptrnBW, ptrnBW.ContentName, ptrnBW.ContentAttribInfo)
    else
      ClearAttributeInfo(ptrnBW.ContentAttribInfo);
    if APatternJson.IndexOfName('captures') >= 0 then
      ReadCaptures(APattern, ptrnBW.Captures.CaptureArray, APatternJson, 'captures', max(Length(ptrnBW.MatchBegin), Length(ptrnBW.MatchWhile)));
    if APatternJson.IndexOfName('beginCaptures') >= 0 then
      ReadCaptures(APattern, ptrnBW.CapturesBegin.CaptureArray, APatternJson, 'beginCaptures', Length(ptrnBW.MatchBegin));
    if APatternJson.IndexOfName('whileCaptures') >= 0 then
      ReadCaptures(APattern, ptrnBW.CapturesWhile.CaptureArray, APatternJson, 'whileCaptures', Length(ptrnBW.MatchWhile));
  end
  else
  if APattern is TTextMatePatternBeginEnd then begin
    ptrnBE.MatchBegin     := jsKeyAsString(APatternJson, 'begin');
    ptrnBE.MatchEnd       := jsKeyAsString(APatternJson, 'end');
    ptrnBE.ContentName    := jsKeyAsString(APatternJson, 'contentName');
    if assigned(FOnPopulateAttributeInfo) and (ptrnBE.ContentName <> '') then
      FOnPopulateAttributeInfo(Self, ptrnBE, ptrnBE.ContentName, ptrnBE.ContentAttribInfo)
    else
      ClearAttributeInfo(ptrnBE.ContentAttribInfo);
    ptrnBE.EndPatternLast := jsKeyAsNumber(APatternJson, 'applyEndPatternLast') = 1;
    if APatternJson.IndexOfName('captures') >= 0 then
      ReadCaptures(APattern, ptrnBE.Captures.CaptureArray, APatternJson, 'captures', max(Length(ptrnBE.MatchBegin), Length(ptrnBE.MatchEnd)));
    if APatternJson.IndexOfName('beginCaptures') >= 0 then
      ReadCaptures(APattern, ptrnBE.CapturesBegin.CaptureArray, APatternJson, 'beginCaptures', Length(ptrnBE.MatchBegin));
    if APatternJson.IndexOfName('endCaptures') >= 0 then
      ReadCaptures(APattern, ptrnBE.CapturesEnd.CaptureArray, APatternJson, 'endCaptures', Length(ptrnBE.MatchEnd));
  end
  else
  if APattern is TTextMatePatternMatch then begin
    ptrnM.Match  := jsKeyAsString(APatternJson, 'match');
    if APatternJson.IndexOfName('captures') >= 0 then
      ReadCaptures(APattern, ptrnM.Captures.CaptureArray, APatternJson, 'captures', Length(ptrnM.Match));
  end
  else
  if APattern is TTextMatePatternNestedList then begin
    // no exception
  end
  else
    raise TTextMateGrammarException.Create('invalid pattern');

  if APattern is TTextMatePatternBaseNested then begin
    try
      if APatternJson.IndexOfName('patterns') >= 0 then
        ParsePatterns(APattern, ptrnBE.Patterns, jsKeyAsArray(APatternJson, 'patterns'));
    except
      on E: TTextMateGrammarException do raise E.Copy.AddLocation('patterns');
      on E: Exception do raise TTextMateGrammarException.Create(E.Message, 'patterns');
    end;
  end;

  APattern.FComment := jsKeyAsString(APatternJson, 'Fcomment');
  APattern.FName    := jsKeyAsString(APatternJson, 'name');
  if assigned(FOnPopulateAttributeInfo) and (APattern.FName <> '') then
    FOnPopulateAttributeInfo(Self, APattern, APattern.FName, APattern.FAttribInfo)
  else
    ClearAttributeInfo(APattern.FAttribInfo);

  APattern.InitRegEx;
end;

function TTextMateGrammar.ParsePattern(AParent: TTextMatePattern;
  APatternJson: TJSONObject): TTextMatePattern;
begin
  if IsInclude(APatternJson) then begin
    Result := ResolveInclude(APatternJson);
    exit;
  end;

  Result := CreatePattern(AParent, APatternJson, True);
  ReadPattern(Result, APatternJson);
end;

procedure TTextMateGrammar.ParsePatterns(AParent: TTextMatePattern;
  var APatternArray: TTextMatePatternArray; APatterns: TJSONArray);
var
  i, j: Integer;
  PtrnJson: TJSONObject;
begin
  SetLength(APatternArray, APatterns.Count);
  j := -1;
  for i := 0 to APatterns.Count -1 do begin
    try
      PtrnJson :=  jsIndexAsDict(APatterns, i);
      if jsKeyAsNumber(PtrnJson, 'disabled') = 1 then
        continue;
      inc(j);

      APatternArray[j] := ParsePattern(AParent, PtrnJson);
    except
      on E: TTextMateGrammarException do raise E.Copy.AddLocation(i);
      on E: Exception do raise TTextMateGrammarException.Create(E.Message, i);
    end;
  end;
  SetLength(APatternArray, j+1);
end;

function TTextMateGrammar.GetCurrentTokenLen: integer;
begin
  Result := FNextTokenPos - FCurrentTokenPos;
end;

function TTextMateGrammar.GetCurrentPatternIndex: Integer;
begin
  if FCurrentState.StateIdx < 0 then
    exit(-1);
  FCurrentState.ValidateParentChain(Self);
  FCurrentPattern := FCurrentState.Pattern[0];
  Result := FCurrentPattern.FMainIndex;
end;

constructor TTextMateGrammar.Create;
begin
  FMainPatternList := TTextMatePatternList.Create(True);
  FPatternRepo := TTextMatePatternMap.Create(False);
  FCurrentState.InitForDepth(0);
  FOtherGrammars := TOtherGrammarMap.Create(False);
end;

destructor TTextMateGrammar.Destroy;
var
  i: Integer;
begin
  for i := 0 to FOtherGrammars.Count-1 do
    if FOtherGrammars.Data[i] <> nil then
      FOtherGrammars.Data[i].RemoveFreeNotification(@DoOtherGrammarFreed);
  inherited Destroy;

  ClearGrammar;
  FMainPatternList.Free;
  FPatternRepo.Free;
  FTheEmptyPattern.Free;
  FOtherGrammars.Free;
  FRegExMatchFoldBegin.Free;
  FRegExMatchFoldEnd.Free;
end;

procedure TTextMateGrammar.ParseGrammar(AGrammarDef: String);
type
  TIndexNameMap = specialize TFPGMap<string, string>;
var
  JSonDef, RepoJson, EntryJSon: TJSONObject;
  i, j: Integer;
  EntryName, InclName, SourceName: String;
  p: TTextMatePattern;
  InclList: TIndexNameMap;
begin
  ClearGrammar;
  FRootPattern := TTextMatePatternRoot(CreatePatternObject(TTextMatePatternRoot));
  if assigned(FOnPopulateAttributeInfo) then
    FOnPopulateAttributeInfo(Self, FRootPattern, 'Default-text', FRootPattern.FAttribInfo); // dash in name

  FParserError := 'Unknown Error';
  JSonDef := nil;
  try
    JSonDef := GetJson(AGrammarDef);
    if JSonDef = nil then
      exit;

    FSampleText       := jsKeyAsString(JSonDef, 'sampleText');
    FSampleTextFile   := jsKeyAsString(JSonDef, 'sampleTextFile');

    FRootPattern.FName     := jsKeyAsString(JSonDef, 'name');
    FRootPattern.ScopeName := jsKeyAsString(JSonDef, 'scopeName');


    MatchFoldBegin := jsKeyAsString(JSonDef, 'foldingStartMarker');
    MatchFoldEnd := jsKeyAsString(JSonDef, 'foldingStopMarker');
    if MatchFoldBegin <> '' then
      FRootPattern.DoInitRegex(FRegExMatchFoldBegin, MatchFoldBegin, 'foldingStartMarker');
    if MatchFoldEnd <> '' then
      FRootPattern.DoInitRegex(FRegExMatchFoldEnd, MatchFoldEnd, 'foldingStopMarker');

    // language file?
    if JSonDef.IndexOfName('contributes') >= 0 then begin
      try
        ParseLanguage(JSonDef['contributes']);
      except
        on E: TTextMateGrammarException do raise E.Copy.AddLocation('contributes');
        on E: Exception do raise TTextMateGrammarException.Create(E.Message, 'contributes');
      end;
      exit;
    end;

    if JSonDef.IndexOfName('repository') >= 0 then begin
      RepoJson := jsKeyAsDict(JSonDef, 'repository');
      InclList := TIndexNameMap.Create;
      try
        try
          for i := 0 to RepoJson.Count - 1 do begin
            EntryName := RepoJson.Names[i];
            EntryJSon := jsKeyAsDict(RepoJson, EntryName);

            if not IsInclude(EntryJSon) then begin
              try
                FPatternRepo.Add(EntryName, CreatePattern(nil, EntryJSon, True));
              except
                on E: TTextMateGrammarException do raise E.Copy.AddLocation(EntryName);
                on E: Exception do raise TTextMateGrammarException.Create(E.Message, EntryName);
              end;
              continue;
            end;

            // include
            InclName := IncludeName(EntryJSon, SourceName);
            if SourceName <> '' then begin
              try
                FPatternRepo.Add(EntryName, ResolveInclude(EntryJSon));
              except
                on E: TTextMateGrammarException do raise E.Copy.AddLocation(EntryName);
                on E: Exception do raise TTextMateGrammarException.Create(E.Message, EntryName);
              end;
              continue;
            end;

            // # other entry from repo
            if FPatternRepo.IndexOf(InclName) >=0 then begin
              p := FPatternRepo[InclName];
              if p <> nil then begin
                FPatternRepo.Add(EntryName, p);
                continue;
              end;
            end;

            InclList.Add(EntryName, InclName);
          end;

          j := InclList.Count;
          while j > 0 do begin
            for i := InclList.Count - 1 downto 0 do begin
              EntryName := InclList.Keys[i];
              InclName  := InclList.Data[i];
              if FPatternRepo.IndexOf(InclName) >=0 then begin
                p := FPatternRepo[InclName];
                if p <> nil then begin
                  FPatternRepo.Add(EntryName, p);
                  InclList.Delete(i);
                  continue;
                end;
              end;
            end;
            if j = InclList.Count then
              raise Exception.Create('circular includes');
            j := InclList.Count;
          end;

        finally
          InclList.Free;
        end;

        try
          for i := 0 to RepoJson.Count - 1 do begin
            EntryName := RepoJson.Names[i];
            EntryJSon := jsKeyAsDict(RepoJson, EntryName);
            if IsInclude(EntryJSon) then
              continue;
            p := FPatternRepo[EntryName];
            p.FDebugName := EntryName;
            ReadPattern(p, jsKeyAsDict(RepoJson, EntryName));
          end;
        except
          on E: TTextMateGrammarException do raise E.Copy.AddLocation(EntryName);
          on E: Exception do raise TTextMateGrammarException.Create(E.Message, EntryName);
        end;

      except
        on E: TTextMateGrammarException do raise E.Copy.AddLocation('repository');
        on E: Exception do raise TTextMateGrammarException.Create(E.Message, 'repository');
      end;
    end;


    try
      if JSonDef.IndexOfName('patterns') >= 0 then
        ParsePatterns(FRootPattern, FRootPattern.Patterns, jsKeyAsArray(JSonDef, 'patterns'));
    except
      on E: TTextMateGrammarException do raise E.Copy.AddLocation('patterns');
      on E: Exception do raise TTextMateGrammarException.Create(E.Message, 'patterns');
    end;

    for i := 0 to FMainPatternList.Count - 1 do
      FMainPatternList[i].FlattenNested(Self, False);
    FParserError := '';
  except
    on E: TTextMateGrammarException do begin
      FParserError := 'Error: ' + E.FullError;
    end;
    on E: Exception do begin
      FParserError := 'Error: ' + E.message;
    end;
  end;
  if (FRootPattern <> nil) then begin
    FLangName := FRootPattern.FName;
    FLanguageScopeName := FRootPattern.ScopeName;
    FMainPatternCount := FMainPatternList.Count;
  end;
  if FParserError <> '' then
    ClearGrammar;

  JSonDef.Free;
end;

procedure TTextMateGrammar.ResolveExternalIncludes;
var
  o: TTextMateGrammar;
  i: Integer;
begin
  if (OnGetIncludedGrammar = nil) or (FOtherGrammars.Count = 0) then
    exit;

  try
    for i := 0 to FOtherGrammars.Count - 1 do begin
      o := FOnGetIncludedGrammar(Self, FOtherGrammars.Keys[i]);
      if o = nil then begin
        if FMissingIncludes <> '' then FMissingIncludes := FMissingIncludes + ', ';
        FMissingIncludes := FMissingIncludes + FOtherGrammars.Keys[i];
        continue;
      end;
      o.AddFreeNotification(@DoOtherGrammarFreed);
      FOtherGrammars.Data[i] := o;
      CopyPatterns(o, FOtherGrammars.Keys[i]);
    end;

    for i := 0 to FMainPatternList.Count - 1 do
      FMainPatternList[i].FlattenNested(Self, True);
  except
    on E: TTextMateGrammarException do begin
      FParserError := 'Error: ' + E.FullError;
    end;
    on E: Exception do begin
      FParserError := 'Error: ' + E.message;
    end;
  end;
end;

procedure TTextMateGrammar.ClearGrammar;
begin
  FCurrentState.InitForDepth(0);
  FRootPattern := nil;
  FMainPatternList.Clear;
  FMainPatternCount := 0;
  FPatternRepo.Clear;
  FMissingIncludes := '';
end;

function TTextMateGrammar.DebugDump(AnIndent: Integer; AnIncludeNested: Boolean
  ): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FPatternRepo.Count - 1 do
    Result := Result + StringOfChar(' ', AnIndent) + '### ' + FPatternRepo.Keys[i] + LineEnding
            + FPatternRepo.Data[i].DebugDump(AnIndent + 2, AnIncludeNested);
  if Result <> '' then
    Result := StringOfChar(' ', AnIndent) + '===== REPO =====' + LineEnding
            + Result
            + StringOfChar(' ', AnIndent) + '===== ROOT =====' + LineEnding;
  Result := Result + FRootPattern.DebugDump(AnIndent, AnIncludeNested);
end;

procedure TTextMateGrammar.SetLine(const AText: String;
  const AnInitialPatternIndex: Integer);
begin
  FLineText := AText;
  if (AnInitialPatternIndex < 0) or (AnInitialPatternIndex >= FMainPatternList.Count) then
    FCurrentPattern := FRootPattern
  else
    FCurrentPattern := FMainPatternList[AnInitialPatternIndex];

  FCurrentState.ClearRecurseData; // the previous state
  FCurrentPattern.InitStates(Self, FCurrentState, AText);
  FCurrentTokenPos := 1;
  FNextTokenPos := 1;
end;

procedure TTextMateGrammar.First;
begin
  if FCurrentPattern = nil then
    exit;

  FCurrentPattern.NextToken(FCurrentState, FLineText, 1, FNextTokenPos, True);
  FCurrentPattern := FCurrentState.Pattern[0];
  assert(FNextTokenPos=1, 'TTextMateGrammar.Next: FNextTokenPos=1');

  if FLineText <> '' then
    Next;
end;

procedure TTextMateGrammar.Next;
begin
  if FCurrentPattern = nil then
    exit;

  FCurrentPattern.GetCurrentTokenInfo(FCurrentState, FOnCheckAttributeInfo, FCurrentTokenKind, FCurrentAttrib);

  FCurrentTokenPos := FNextTokenPos;
  if FCurrentTokenPos > Length(FLineText) then
    exit;

  FCurrentPattern.NextToken(FCurrentState, FLineText, FCurrentTokenPos, FNextTokenPos);
  if FNextTokenPos < 0 then FNextTokenPos := Length(FLineText) + 1;
  FCurrentPattern := FCurrentState.Pattern[0];
  //assert((FNextTokenPos > FCurrentTokenPos) or (FLineText=''), 'TTextMateGrammar.Next: (FNextTokenPos > FCurrentTokenPos) or (FLineText='')');
  assert((FNextTokenPos >= FCurrentTokenPos) or (FLineText=''), 'TTextMateGrammar.Next: (FNextTokenPos > FCurrentTokenPos) or (FLineText='')');
  assert(FCurrentPattern<>nil, 'TTextMateGrammar.Next: FCurrentPattern<>nil');

  if FNextTokenPos <= FCurrentTokenPos then FNextTokenPos := Length(FLineText) + 1; // error, skip line
end;

function TTextMateGrammar.IsAtEol: boolean;
begin
  Result := FCurrentTokenPos > Length(FLineText);
end;

procedure TTextMateGrammar.NextToEol;
var
  c, n: integer;
begin
  c := 0;
  FCurrentState.Pattern[0].NextToken(FCurrentState, FLineText, 1, n);
  while (c < n) and (n <= Length(FLineText)) do begin
    c := n;
    FCurrentState.Pattern[0].NextToken(FCurrentState, FLineText, c, n);
  end;
  FCurrentPattern := FCurrentState.Pattern[0];
  FCurrentTokenPos := Length(FLineText) + 1;
end;

function TTextMateGrammar.IsFoldBegin: boolean;
begin
  if MatchFoldBegin = '' then
    exit(false);
  try
    FRegExMatchFoldBegin.InputString := FLineText;
    Result := FRegExMatchFoldBegin.Exec;
  except
    Result := False;
  end;
end;

function TTextMateGrammar.IsFoldEnd: boolean;
begin
  if MatchFoldEnd = '' then
    exit(false);
  try
    FRegExMatchFoldEnd.InputString := FLineText;
    Result := FRegExMatchFoldEnd.Exec;
  except
    Result := False;
  end;
end;

end.

