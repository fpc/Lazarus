{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

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
unit LazEditMatchingBracketUtils;

{$mode objfpc}{$H+}
{$WARN 3018 off : Constructor should be public}
interface

uses
  LazEditTypes, LazEditHighlighter, LazEditASyncRunner, LazEditMiscProcs, SysUtils, Classes;

type

  TLazEditBracketSearchDirection = (
    bsdRightOrPartRight,  // bracket is after LogX or partly after (LogX in middle of bracket)
    bsdRight,             // bracket starts at LogX
    bsdLeft,              // bracket ends at LogX
    bsdRightThenLeft,     // Prefer bracket starts at LogX (or includes LogX), fallback to bracket ends at LogX
    bsdLeftThenRight      // Prefer bracket ends at LogX, fallback to bracket starts at LogX (or includes LogX)
  );

  TLazEditBracketInfo = record
    BracketLogStartX, BracketLogLength: Integer;
    BracketFlags: TLazEditBracketInfoFlags;
    BracketNestLevel: integer; // used as counter variable if bfUnknownNestLevel is set
    BracketKind: integer;      // e.g. ([{' ...
    BracketContext: integer;   // if the same bracket can be in diff context (e.g, "(" in code, string or comment)
    InternalInfo: PtrUInt;     // for the HL internal use
  end;


  TCharset=set of char;
  TLazEditBracketCharsFilter = (bcfOnlyOpen, bcfOnlyClose);
  TLazEditBracketCharsFilters = set of TLazEditBracketCharsFilter;


  { TBracketFinderAsync }

  TBracketFinderAsync = object // class
  public type
    TBracketFinderAsyncDoneEvent = procedure(ASender: TBracketFinderAsync) of object;
  private
    FCallBack: TBracketFinderAsyncDoneEvent;
    FEditor:TLazEditEditorId;
    FCloseBracketFound: boolean;
    FCloseBracketPos: TLogTokenPos;
    FHardBottomLineLimit: integer;
    FHardTopLineLimit: integer;
    FHighlighter: TLazEditCustomHighlighter;
    FLines: TLazEditStringsBase;
    FOpenBracketIsOpening, FSearchingForward: boolean;
    FAsyncSched: (asNone, asFrwrd, asBackwrd);
    FTaskPriorities: TTLazEditTaskPriorities;
    FNoCtxTopLineLimit, FNoCtxBottomLineLimit: integer;

    FStartPos: TPoint;
    FOpenBracketPos: TLogTokenPos;
    FCurYIdx, FCurXPos: integer;
    FOpenBracketInfo, FSearchBracketInfo: TLazEditBracketInfo;

    procedure DoCallback;
    procedure DoBackwardSearch(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId;
      AMaxTime: integer; var AData: Pointer; var ADone: boolean;
      var APriorities: TTLazEditTaskPriorities);
    procedure DoForwardSearch(AnEditorId: TLazEditEditorId; ATaskId: TLazEditTaskId;
      AMaxTime: integer; var AData: Pointer; var ADone: boolean;
      var APriorities: TTLazEditTaskPriorities);
  public
    procedure Create;
    procedure Destroy;
    function Run(AnEditor:TLazEditEditorId;
      ALogicalStartPos: TPoint; SearchSide: TLazEditBracketSearchDirection;
      ACallBack: TBracketFinderAsyncDoneEvent;
      AnHighlighter: TLazEditCustomHighlighter; ALines: TLazEditStringsBase = nil;
      ATaskPriorities: TTLazEditTaskPriorities = [];
      ANoCtxTopLineLimit: integer = -1;
      ANoCtxBottomLineLimit: integer = -1
    ): Boolean;
    function Init(AnEditor:TLazEditEditorId;
      ALogicalStartPos: TPoint; SearchSide: TLazEditBracketSearchDirection;
      ACallBack: TBracketFinderAsyncDoneEvent;
      AnHighlighter: TLazEditCustomHighlighter; ALines: TLazEditStringsBase = nil;
      ATaskPriorities: TTLazEditTaskPriorities = [];
      ANoCtxTopLineLimit: integer = -1;
      ANoCtxBottomLineLimit: integer = -1
    ): boolean;
    function Exec: Boolean;
    procedure Cancel;

    property NoCtxTopLineLimit:    integer read FNoCtxTopLineLimit    write FNoCtxTopLineLimit;
    property NoCtxBottomLineLimit: integer read FNoCtxBottomLineLimit write FNoCtxTopLineLimit;
    property HardTopLineLimit:     integer read FHardTopLineLimit    write FHardTopLineLimit;
    property HardBottomLineLimit:  integer read FHardBottomLineLimit write FHardBottomLineLimit;

    property OpenBracketPos: TLogTokenPos read FOpenBracketPos;
    // OpenBracketIsOpening: for uniform brackets this can be adjusted after CreateIdle
    property OpenBracketIsOpening: boolean read FOpenBracketIsOpening write FOpenBracketIsOpening; // do forward search
    property CloseBracketPos: TLogTokenPos read FCloseBracketPos;
    property CloseBracketFound: boolean read FCloseBracketFound;
    property OpenBracketInfo: TLazEditBracketInfo read FOpenBracketInfo;
  end;

(* - AnHighlighter may be nil, and then ALines must be given.
   - If AnHighlighter <> nil, and ALines <> nil then CurrentLines will be set to ALines
*)

function GetBracketInfoAt(ALineIdx: TLineIdx; ALogX: IntPos;
  AnHighlighter: TLazEditCustomHighlighter;
  out AFoundBracketInfo: TLazEditBracketInfo;
  ASearchDirection: TLazEditBracketSearchDirection = bsdRightOrPartRight;
  ALines: TLazEditStringsBase = nil
): boolean;

function FindBracketPos(ALineIdx: TLineIdx; ALogStartX: IntPos; ASearchBackward: Boolean;
  AnHighlighter: TLazEditCustomHighlighter;
  var ASearchBracketInfo: TLazEditBracketInfo; // will adjust BracketNestLevel, if depth is not known
  out AFoundLogPos: IntPos;
  ALines: TLazEditStringsBase = nil
): boolean;

function GetBracketTokenCount(AnHighlighter: TLazEditCustomHighlighter ): integer;
function GetBracketToken(AKind: integer; AnOpen: Boolean; AnHighlighter: TLazEditCustomHighlighter ): string;
function GetBracketTokenIndex(AToken: String; AnOpen: Boolean; AnHighlighter: TLazEditCustomHighlighter ): integer;
function GetBracketCharSet(AnHighlighter: TLazEditCustomHighlighter; AFilter: TLazEditBracketCharsFilters = []): TCharset;


implementation

const
  BRACKET_KIND_TOKEN_COUNT = 5;
  BRACKET_KIND_TOKEN_QUOTE_START = 3;
  BRACKET_KIND_TOKENS: array [Boolean, 0..BRACKET_KIND_TOKEN_COUNT-1] of string =
    ( (')', ']', '}', '"', ''''),
      ('(', '[', '{', '"', '''')
    );

function GetBracketInfoAt(ALineIdx: TLineIdx; ALogX: IntPos;
  AnHighlighter: TLazEditCustomHighlighter;
  out AFoundBracketInfo: TLazEditBracketInfo;
  ASearchDirection: TLazEditBracketSearchDirection = bsdRightOrPartRight;
  ALines: TLazEditStringsBase = nil
): boolean;
var
  Line, BracketToken, PrevBracketToken: String;
  BracketKindCount, BracketIdx: Integer;
  BracketOpen, HasTxtToken: Boolean;
  x1, x2: IntPos;
begin
  Result := False;
  AFoundBracketInfo := default(TLazEditBracketInfo);

  if ALines <> nil
  then Line := ALines[ALineIdx]
  else Line := AnHighlighter.CurrentLines[ALineIdx];

  if AnHighlighter = nil then begin
    BracketKindCount := BRACKET_KIND_TOKEN_COUNT;
    AFoundBracketInfo.BracketFlags := [bfNoLanguageContext];
  end
  else
    BracketKindCount := AnHighlighter.BracketKindCount;

  BracketToken := '';
  HasTxtToken := True;
  x2 := ALogX;
  if ASearchDirection = bsdLeft then  x2 := ALogX-1;
  for BracketIdx := 0 to BracketKindCount - 1 do
  for BracketOpen := False to true do begin
    PrevBracketToken := BracketToken;
    if AnHighlighter = nil then
      BracketToken := BRACKET_KIND_TOKENS[BracketOpen, BracketIdx]
    else
      BracketToken := AnHighlighter.BracketKinds[BracketIdx, BracketOpen];

    // if closing equal open, and the token does not appear in the line, then skip
    if (BracketToken = PrevBracketToken) and (not HasTxtToken) then
      continue;

    case ASearchDirection of
      bsdRightOrPartRight: x1 := ALogX-length(BracketToken)+1;
      bsdRight:            x1 := ALogX;
      bsdLeft:             x1 := ALogX-length(BracketToken);
      bsdRightThenLeft:  begin
                           x1 := ALogX;
                           x2 := ALogX;
                         end;
      bsdLeftThenRight:    x1 := ALogX-length(BracketToken);
    end;

    if x1 < 1 then x1 := 1;
    if x2 > Length(Line) then x2 := Length(Line);

    HasTxtToken := False;
    repeat
      while (x1 <= x2 ) and (Line[x1] <> BracketToken[1]) do
        inc(x1);
      if x1 <= x2 then begin
        HasTxtToken := True;

        if strlcomp(pchar(BracketToken), @Line[x1], Length(BracketToken)) = 0 then begin
          if (AnHighlighter <> nil) and (ALines <> nil) then
            AnHighlighter.CurrentLines := ALines;
          AFoundBracketInfo.BracketLogStartX := x1;
          AFoundBracketInfo.BracketLogLength := Length(BracketToken);
          AFoundBracketInfo.BracketKind      := BracketIdx;
          if BracketOpen
          then Include(AFoundBracketInfo.BracketFlags, bfOpen)
          else Exclude(AFoundBracketInfo.BracketFlags, bfOpen);

          if (AnHighlighter = nil) then begin
            // found
            if BracketIdx >= BRACKET_KIND_TOKEN_QUOTE_START then
              AFoundBracketInfo.BracketFlags := AFoundBracketInfo.BracketFlags + [bfUniform, bfNotNestable, bfSingleLine]
            else
              AFoundBracketInfo.BracketFlags := AFoundBracketInfo.BracketFlags + [bfUnknownNestLevel];
            Result := True;
            exit;
          end
          else
          if AnHighlighter.GetBracketContextAt(ALineIdx,
              AFoundBracketInfo.BracketLogStartX,
              AFoundBracketInfo.BracketLogLength,
              AFoundBracketInfo.BracketKind,
              AFoundBracketInfo.BracketFlags,
              AFoundBracketInfo.BracketContext,
              AFoundBracketInfo.BracketNestLevel,
              AFoundBracketInfo.InternalInfo
             )
          then begin
            // found
            Result := True;
            exit;
          end;
        end;
      end;

      inc(x1);
      if x1 <= x2 then
        continue;

      if (ASearchDirection = bsdRightThenLeft) and (x2 = ALogX) then begin
        x1 := ALogX-length(BracketToken);
        x2 := ALogX-1;
        continue;
      end;

      break;
    until False;
  end;
end;

function FindBracketPos(ALineIdx: TLineIdx; ALogStartX: IntPos; ASearchBackward: Boolean;
  AnHighlighter: TLazEditCustomHighlighter;
  var ASearchBracketInfo: TLazEditBracketInfo; // will adjust BracketNestLevel, if depth is not known
  out AFoundLogPos: IntPos;
  ALines: TLazEditStringsBase = nil
): boolean;

  var
    Line: String;
    LogX, FndLvl: integer;

  function IsMatchingBracketAtLogX(const B: string; AReverseOpening: Boolean = False): boolean; inline;
  // Will set outer FndLvl
  var
    l: integer;
    FndCtx: Integer;
    Flags, Flags2: TLazEditBracketInfoFlags;
  begin
    l := Length(B);
    Result := (strlcomp(@Line[LogX], @B[1], Length(B)) = 0);
    if (not Result) or (AnHighlighter = nil) then
      exit;
    if (ALines <> nil) then
      AnHighlighter.CurrentLines := ALines;
    Flags := ASearchBracketInfo.BracketFlags;
    if AReverseOpening and not(bfUniform in Flags) then begin
      if bfOpen in Flags
      then exclude(Flags, bfOpen)
      else include(Flags, bfOpen);
    end;
    Flags2 := Flags;
    Result := AnHighlighter.GetBracketContextAt(ALineIdx, LogX, l,
                                  ASearchBracketInfo.BracketKind,
                                  Flags, FndCtx, FndLvl,
                                  ASearchBracketInfo.InternalInfo)
              and
              (Flags * [bfOpen, bfUniform, bfUnknownNestLevel] = Flags2 * [bfOpen, bfUniform, bfUnknownNestLevel]) and
              (FndCtx = ASearchBracketInfo.BracketContext);
  end;

var
  SearchBracket, SearchAntiBracket: String;
  IsOpening, SeenLowerNest: Boolean;
  ALogXMax, CacheIdx, CacheAntiBrkCount: Integer;
  Cache: array of Integer;
begin
  Result := False;
  AFoundLogPos := -1;
  FndLvl := 0;
  if ALines <> nil
  then Line := ALines[ALineIdx]
  else Line := AnHighlighter.CurrentLines[ALineIdx];

  IsOpening := bfOpen in ASearchBracketInfo.BracketFlags;
  if AnHighlighter <> nil
  then SearchBracket := AnHighlighter.BracketKinds[ASearchBracketInfo.BracketKind, IsOpening]
  else SearchBracket := BRACKET_KIND_TOKENS[IsOpening, ASearchBracketInfo.BracketKind];
  ALogXMax := Length(Line) - Length(SearchBracket) + 1;

  if ASearchBackward then begin
    if (ALogStartX < 1) then
      ALogStartX := Length(Line)  - Length(SearchBracket) + 1;
    if ALogXMax > ALogStartX then
      ALogXMax :=  ALogStartX;
  end;

  if (bfNotNestable in ASearchBracketInfo.BracketFlags) or
     not(bfUnknownNestLevel in ASearchBracketInfo.BracketFlags)
  then begin
    // NestLevel is known or ignored
    if ASearchBackward or (ALogStartX < 1) then
      LogX := 1
    else
      LogX := ALogStartX;
    SeenLowerNest := False;
    repeat
      while (LogX <= ALogXMax) and (Line[LogX] <> SearchBracket[1]) do
        inc(LogX);
      if LogX > ALogXMax then
        break;

      if IsMatchingBracketAtLogX(SearchBracket) then begin
        if (bfNotNestable in ASearchBracketInfo.BracketFlags) or
           (FndLvl = ASearchBracketInfo.BracketNestLevel)
        then begin
          AFoundLogPos := LogX;
          Result := True;
          if not ASearchBackward then
            exit;
          // else keep searching for the right most bracket that matches
        end
        else
        // if bracket has lowel level, then stop
        if (not (bfNotNestable in ASearchBracketInfo.BracketFlags)) and
           (FndLvl < ASearchBracketInfo.BracketNestLevel)
        then begin
          if (not ASearchBackward) then begin
            ASearchBracketInfo.BracketFlags := ASearchBracketInfo.BracketFlags + [bfForceStopSearch];
            exit;
          end;
          SeenLowerNest := True;
        end;
      end;

      inc(LogX);
    until LogX > ALogXMax;
    if (not Result) and  SeenLowerNest then begin
      ASearchBracketInfo.BracketFlags := ASearchBracketInfo.BracketFlags + [bfForceStopSearch];
      exit;
    end;
  end

  else begin
    // NestLevel not known
    if AnHighlighter <> nil
    then SearchAntiBracket := AnHighlighter.BracketKinds[ASearchBracketInfo.BracketKind, not IsOpening]
    else SearchAntiBracket := BRACKET_KIND_TOKENS[not IsOpening, ASearchBracketInfo.BracketKind];

    if ASearchBackward then begin
      // Backwards, build cache
      LogX := 1;
      Cache := nil;
      CacheIdx := -1;
      CacheAntiBrkCount := 0;
      repeat
        while (LogX <= ALogXMax) and
              (Line[LogX] <> SearchBracket[1]) and (Line[LogX] <> SearchAntiBracket[1])
        do
          inc(LogX);
        if LogX > ALogXMax then
          break;

        if IsMatchingBracketAtLogX(SearchBracket) then begin
          inc(CacheIdx);
          if CacheIdx >= Length(Cache) then
            SetLength(Cache, CacheIdx + 32);
          Cache[CacheIdx] := LogX;
        end
        else
        if IsMatchingBracketAtLogX(SearchAntiBracket, True) then begin
          if (CacheIdx >= 0) then
            dec(CacheIdx)
          else
            inc(CacheAntiBrkCount);
        end;

        inc(LogX);
      until LogX > ALogXMax;

      if ASearchBracketInfo.BracketNestLevel <= CacheIdx then begin
        AFoundLogPos := Cache[CacheIdx-ASearchBracketInfo.BracketNestLevel];
        Result := True;
        exit;
      end;
      ASearchBracketInfo.BracketNestLevel := ASearchBracketInfo.BracketNestLevel - (CacheIdx+1) + CacheAntiBrkCount;
    end

    else begin
      // Forward
      LogX := ALogStartX;
      repeat
        while (LogX <= ALogXMax) and
              (Line[LogX] <> SearchBracket[1]) and (Line[LogX] <> SearchAntiBracket[1])
        do
          inc(LogX);
        if LogX > ALogXMax then
          break;

        if IsMatchingBracketAtLogX(SearchBracket) then begin
          if ASearchBracketInfo.BracketNestLevel = 0 then begin
            AFoundLogPos := LogX;
            Result := True;
            exit;
          end
          else
            dec(ASearchBracketInfo.BracketNestLevel);
        end
        else
        if IsMatchingBracketAtLogX(SearchAntiBracket, True) then begin
            inc(ASearchBracketInfo.BracketNestLevel);
        end;

        inc(LogX);
      until LogX > ALogXMax;
    end;
  end;
end;

function GetBracketTokenCount(AnHighlighter: TLazEditCustomHighlighter): integer;
begin
  if AnHighlighter = nil then
    Result := BRACKET_KIND_TOKEN_COUNT
  else
    Result := AnHighlighter.BracketKindCount;
end;

function GetBracketToken(AKind: integer; AnOpen: Boolean; AnHighlighter: TLazEditCustomHighlighter
  ): string;
begin
  if AnHighlighter = nil then
    Result := BRACKET_KIND_TOKENS[AnOpen, AKind]
  else
    Result := AnHighlighter.BracketKinds[AKind, AnOpen];
end;

function GetBracketTokenIndex(AToken: String; AnOpen: Boolean;
  AnHighlighter: TLazEditCustomHighlighter): integer;
begin
  Result := GetBracketTokenCount(AnHighlighter) - 1;
  while Result >= 0 do begin
    if GetBracketToken(Result, AnOpen, AnHighlighter) = AToken then
      exit;
    dec(Result);
  end;
end;

function GetBracketCharSet(AnHighlighter: TLazEditCustomHighlighter;
  AFilter: TLazEditBracketCharsFilters): TCharset;
  procedure AddC(s: string);
  var
    j: Integer;
  begin
    for j := 1 to Length(s) do Include(Result, s[j]);
  end;
var
  i: Integer;
begin
  Result := Default(TCharset);
  if AnHighlighter <> nil then begin
    for i := 0 to AnHighlighter.BracketKindCount - 1 do begin
      if not (bcfOnlyClose in AFilter) then
        AddC(AnHighlighter.BracketKinds[i, True]);
      if not (bcfOnlyOpen in AFilter) then
        AddC(AnHighlighter.BracketKinds[i, False]);
    end;
  end
  else begin
    for i := 0 to BRACKET_KIND_TOKEN_COUNT - 1 do begin
      if not (bcfOnlyClose in AFilter) then
        AddC(BRACKET_KIND_TOKENS[True, i]);
      if not (bcfOnlyOpen in AFilter) then
        AddC(BRACKET_KIND_TOKENS[False, i]);
    end;
  end;
end;

{ TBracketFinderAsync }

procedure TBracketFinderAsync.DoCallback;
begin
  if FCallBack <> nil then
    FCallBack(Self);
end;

procedure TBracketFinderAsync.DoBackwardSearch(AnEditorId: TLazEditEditorId;
  ATaskId: TLazEditTaskId; AMaxTime: integer; var AData: Pointer; var ADone: boolean;
  var APriorities: TTLazEditTaskPriorities);
var
  EndTime: QWord;
  FoundPosX: IntPos;
begin
  ADone := False;
  EndTime := GetTickCount64 + AMaxTime;
  while FCurYIdx >= 0 do begin
    if FindBracketPos(FCurYIdx, FCurXPos, True, FHighlighter, FSearchBracketInfo, FoundPosX, FLines) then begin
      ADone := True;
      FCloseBracketFound := True;
      FCloseBracketPos.Y := ToPos(FCurYIdx);
      FCloseBracketPos.X := FoundPosX;
      FCloseBracketPos.Len := FSearchBracketInfo.BracketLogLength;
      DoCallback;
      exit;
    end;
    dec(FCurYIdx);
    FCurXPos := -1;
    if ( (bfNoLanguageContext in FOpenBracketInfo.BracketFlags) and
         (FNoCtxTopLineLimit >= 0) and (FCurYIdx < FNoCtxTopLineLimit)
       ) or
       ( (FHardTopLineLimit >= 0) and (FCurYIdx < FHardTopLineLimit) ) or
       (bfSingleLine in FOpenBracketInfo.BracketFlags) or
       (bfForceStopSearch in FOpenBracketInfo.BracketFlags)
    then begin
      ADone := True;
      DoCallback;
      exit;
    end;
    if ((FCurYIdx and 15) = 0) and (FCallBack <> nil) and (GetTickCount64 > EndTime) then
      exit;
  end;
  ADone := True;
  DoCallback;
end;

procedure TBracketFinderAsync.DoForwardSearch(AnEditorId: TLazEditEditorId;
  ATaskId: TLazEditTaskId; AMaxTime: integer; var AData: Pointer; var ADone: boolean;
  var APriorities: TTLazEditTaskPriorities);
var
  c: Integer;
  EndTime: QWord;
  FoundPosX: IntPos;
begin
  ADone := False;
  EndTime := GetTickCount64 + AMaxTime;
  if FLines <> nil then c := FLines.Count
  else c := FHighlighter.CurrentLines.Count;

  while FCurYIdx < c do begin
    if FindBracketPos(FCurYIdx, FCurXPos, False, FHighlighter, FSearchBracketInfo, FoundPosX, FLines) then begin
      ADone := True;
      FCloseBracketFound := True;
      FCloseBracketPos.Y := ToPos(FCurYIdx);
      FCloseBracketPos.X := FoundPosX;
      FCloseBracketPos.Len := FSearchBracketInfo.BracketLogLength;
      DoCallback;
      exit;
    end;
    inc(FCurYIdx);
    FCurXPos := 1;
    if ( (bfNoLanguageContext in FOpenBracketInfo.BracketFlags) and
         (FNoCtxBottomLineLimit >= 0) and (FCurYIdx > FNoCtxBottomLineLimit)
       ) or
       ( (FHardBottomLineLimit >= 0) and (FCurYIdx > FHardBottomLineLimit) ) or
       (bfSingleLine in FOpenBracketInfo.BracketFlags) or
       (bfForceStopSearch in FOpenBracketInfo.BracketFlags)
    then begin
      ADone := True;
      DoCallback;
      exit;
    end;
    if ((FCurYIdx and 15) = 0) and (FCallBack <> nil) and (GetTickCount64 > EndTime) then
      exit;
  end;
  ADone := True;
  DoCallback;
end;

procedure TBracketFinderAsync.Create;
begin
  FAsyncSched := asNone;
  FCloseBracketFound := False;
  FOpenBracketPos.Y  := -1;
end;

procedure TBracketFinderAsync.Destroy;
begin
  Cancel;
end;

function TBracketFinderAsync.Run(AnEditor: TLazEditEditorId; ALogicalStartPos: TPoint;
  SearchSide: TLazEditBracketSearchDirection; ACallBack: TBracketFinderAsyncDoneEvent;
  AnHighlighter: TLazEditCustomHighlighter; ALines: TLazEditStringsBase;
  ATaskPriorities: TTLazEditTaskPriorities; ANoCtxTopLineLimit: integer;
  ANoCtxBottomLineLimit: integer): Boolean;
begin
  Result := Init(AnEditor, ALogicalStartPos, SearchSide, ACallBack, AnHighlighter,
      ALines, ATaskPriorities, ANoCtxTopLineLimit, ANoCtxBottomLineLimit);

  if Result then begin
    Result := Exec;
    if FCallBack <> nil then Result := True;
  end;
end;

function TBracketFinderAsync.Init(AnEditor: TLazEditEditorId; ALogicalStartPos: TPoint;
  SearchSide: TLazEditBracketSearchDirection; ACallBack: TBracketFinderAsyncDoneEvent;
  AnHighlighter: TLazEditCustomHighlighter; ALines: TLazEditStringsBase;
  ATaskPriorities: TTLazEditTaskPriorities; ANoCtxTopLineLimit: integer;
  ANoCtxBottomLineLimit: integer): boolean;
var
  FndOpenBracketInfo: TLazEditBracketInfo;
begin
  Result := False;
  FCloseBracketFound := False;
  FOpenBracketPos.Y  := -1;

  if (not GetBracketInfoAt(ToIdx(ALogicalStartPos.Y), ALogicalStartPos.X,
        AnHighlighter, FndOpenBracketInfo, SearchSide, ALines)) or
     (bfUnmatched in FndOpenBracketInfo.BracketFlags)
  then
    exit;

  Result := True;

  FStartPos := ALogicalStartPos;
  FOpenBracketPos.Y   := ALogicalStartPos.Y;
  FOpenBracketPos.X   := FndOpenBracketInfo.BracketLogStartX;
  FOpenBracketPos.Len := FndOpenBracketInfo.BracketLogLength;
  FOpenBracketInfo := FndOpenBracketInfo;
  FSearchBracketInfo := FndOpenBracketInfo;

  FEditor               := AnEditor;
  FCallBack             := ACallBack;
  FHighlighter          := AnHighlighter;
  FLines                := ALines;
  FTaskPriorities       := ATaskPriorities;
  FNoCtxTopLineLimit    := ToIdx(ANoCtxTopLineLimit);
  FNoCtxBottomLineLimit := ToIdx(ANoCtxBottomLineLimit);
  FHardTopLineLimit     := -1;
  FHardBottomLineLimit  := -1;

  FCloseBracketFound := False;

  if bfUniform in FOpenBracketInfo.BracketFlags then
    FOpenBracketIsOpening := ALogicalStartPos.X > FndOpenBracketInfo.BracketLogStartX // decide if to search forward/backward
  else
    FOpenBracketIsOpening := bfOpen in FndOpenBracketInfo.BracketFlags;
end;

function TBracketFinderAsync.Exec: Boolean;
var
  ADone: Boolean;
  d: Pointer;
begin
  Cancel;
  FCloseBracketFound := False;
  if FOpenBracketPos.y < 0 then
    exit(False);

  Exclude(FSearchBracketInfo.BracketFlags, bfOpen);
  ADone := False;
  FCurYIdx := ToIdx(FStartPos.Y);
  d := nil;
  if FOpenBracketIsOpening then begin
    // find the closing bracket
    FSearchingForward := True;

    FCurXPos := FSearchBracketInfo.BracketLogStartX + FSearchBracketInfo.BracketLogLength;
    DoForwardSearch(FEditor, nil, 10, d, ADone, FTaskPriorities);
    if not ADone then begin
      FAsyncSched := asFrwrd;
      GlobalASyncRunner.AddOrReplaceTask(@DoForwardSearch, FEditor, FTaskPriorities);
    end;
  end
  else begin
    if not (bfUniform in FSearchBracketInfo.BracketFlags) then
      Include(FSearchBracketInfo.BracketFlags, bfOpen); // find the opening bracket
    FSearchingForward := False;

    FCurXPos := FSearchBracketInfo.BracketLogStartX - 1;
    if FCurXPos < 1 then begin
      dec(FCurYIdx);
      FCurXPos := -1;
    end;
    DoBackwardSearch(FEditor, nil, 10, d, ADone, FTaskPriorities);
    if not ADone then begin
      FAsyncSched := asBackwrd;
      GlobalASyncRunner.AddOrReplaceTask(@DoBackwardSearch, FEditor, FTaskPriorities);
    end;
  end;
  Result := FCloseBracketFound;
end;

procedure TBracketFinderAsync.Cancel;
begin
  case FAsyncSched of
    asFrwrd:   GlobalASyncRunner.RemoveTask(@DoForwardSearch, FEditor);
    asBackwrd: GlobalASyncRunner.RemoveTask(@DoBackwardSearch, FEditor);
  end;
  FAsyncSched := asNone;
end;

end.

