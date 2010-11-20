{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit SynEditMarkupHighAll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, SynEditMarkup, SynEditTypes, SynEditSearch,
  SynEditMiscClasses, Controls, LCLProc, SynEditHighlighter, SynEditPointClasses,
  SynEditMiscProcs;

type

(*
  TSynMarkupHighAllLineState = (hlsValid, hlsInvalid);
  
  TSynMarkupHighAllLine = Record
    MatchIndex : Integer;
    State : TSynMarkupHighAllLineState;
  end;
  
  { TSynMarkupHighAllLines }

  TSynMarkupHighAllLines = class(TObject)
  private
    FCount, FCapacity : Integer;
    FLines : Array of TSynMarkupHighAllLine;
    function GetLine(const Index : Integer) : TSynMarkupHighAllLine;
    procedure SetCount(const AValue : Integer);
    procedure SetLine(const Index : Integer; const AValue : TSynMarkupHighAllLine);
  public
    constructor Create;
    Function MaybeReduceCapacity : Boolean;
    property Count : Integer read FCount write SetCount;
    property Line [const Index : Integer] : TSynMarkupHighAllLine read GetLine write SetLine; default;
  end;
*)

  { TSynMarkupHighAllMatch }
  TSynMarkupHighAllMatch = Record
    StartPoint, EndPoint : TPoint;
  end;

  { TSynMarkupHighAllMatchList }
  TSynMarkupHighAllMatchList = class(TObject)
  private
    FCount, FCapacity : Integer;
    FMatches : Array of TSynMarkupHighAllMatch;
    
    function GetEndPoint(const Index : Integer) : TPoint;
    function GetPoint(const Index : Integer) : TPoint;
    function GetPointCount : Integer;
    function GetStartPoint(const Index : Integer) : TPoint;
    procedure SetCount(const AValue : Integer);
    function  GetMatch(const Index : Integer) : TSynMarkupHighAllMatch;
    procedure SetEndPoint(const Index : Integer; const AValue : TPoint);
    procedure SetMatch(const Index : Integer; const AValue : TSynMarkupHighAllMatch);
    procedure SetStartPoint(const Index : Integer; const AValue : TPoint);
  public
    constructor Create;
    Function MaybeReduceCapacity : Boolean;
    property Count : Integer read FCount write SetCount;
    property Match [const Index : Integer] : TSynMarkupHighAllMatch read GetMatch write SetMatch; default;
    property StartPoint [const Index : Integer] : TPoint read GetStartPoint write SetStartPoint;
    property EndPoint   [const Index : Integer] : TPoint read GetEndPoint write SetEndPoint;
    property PointCount : Integer read GetPointCount;
    property Point      [const Index : Integer] : TPoint read GetPoint;
  end;
  
  
  { TSynEditMarkupHighlightAll }

  TSynEditMarkupHighlightAll = class(TSynEditMarkup)
  private
    fSearchString : string;            // for highlighting all occurences of a word/term
    fSearchOptions: TSynSearchOptions;
    fSearch: TSynEditSearch;

    fStartPoint : TPoint;        // First found position, before TopLine of visible area
    fMatches : TSynMarkupHighAllMatchList;
    fHasInvalidLines : Boolean;
    FHideSingleMatch: Boolean;

    Procedure FindStartPoint;
    Procedure FindInitialize(Backward: Boolean);
    function GetMatchCount: Integer;
    Procedure ValidateMatches(RePaint: Boolean = True; KeepStartPoint: Boolean = False);
    procedure SetSearchOptions(const AValue : TSynSearchOptions);
  protected
    procedure SetSearchString(const AValue : String); virtual;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoTextChanged(StartLine, EndLine: Integer); override;
    property  HideSingleMatch: Boolean read FHideSingleMatch write FHideSingleMatch;
    property MatchCount: Integer read GetMatchCount;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;

    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;

    Procedure Invalidate(RePaint: Boolean = True);
    Procedure SendLineInvalidation;

    property SearchString : String read fSearchString write SetSearchString;
    property SearchOptions : TSynSearchOptions read fSearchOptions write SetSearchOptions;
  end;

  { TSynEditMarkupHighlightAllCaret }

  TSynEditMarkupHighlightAllCaret = class(TSynEditMarkupHighlightAll)
  private
    FTimer: TTimer;
    FTrim: Boolean;
    FWaitTime: Integer;
    FFullWord: Boolean;
    FFullWordMaxLen: Integer;
    FIgnoreKeywords: Boolean;
    FSelection: TSynEditSelection;
    FHighlighter: TSynCustomHighlighter;
    FLowBound, FUpBound, FOldLowBound, FOldUpBound: TPoint;
    FToggledWord: String;
    FToggledOption: TSynSearchOptions;
    FStateChanged: Boolean;
    procedure SetFullWord(const AValue: Boolean);
    procedure SetFullWordMaxLen(const AValue: Integer);
    procedure SetHighlighter(const AValue: TSynCustomHighlighter);
    procedure SetIgnoreKeywords(const AValue: Boolean);
    procedure SetSelection(const AValue: TSynEditSelection);
    procedure SetTrim(const AValue: Boolean);
    procedure SetWaitTime(const AValue: Integer);
  protected
    procedure SetSearchString(const AValue : String); override;
    procedure SelectionChanged(Sender: TObject);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure RestartTimer;
    procedure ScrollTimerHandler(Sender: TObject);
    function  GetCurrentText: String;
    function  GetCurrentOption: TSynSearchOptions;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure CheckState; // Todo need a global lock, including the markup
    procedure ToggleCurrentWord;
    property  WaitTime: Integer read FWaitTime write SetWaitTime;
    property  Trim: Boolean read FTrim write SetTrim;
    property  FullWord: Boolean read FFullWord write SetFullWord;
    property  FullWordMaxLen: Integer read FFullWordMaxLen write SetFullWordMaxLen;
    property  IgnoreKeywords: Boolean read FIgnoreKeywords write SetIgnoreKeywords;
    property  Highlighter: TSynCustomHighlighter
      read FHighlighter write SetHighlighter;
    property  Selection: TSynEditSelection write SetSelection;
  end;

implementation
uses SynEdit;

{ TSynMarkupHighAllLines }

(*
constructor TSynMarkupHighAllLines.Create;
begin
  FCount := 0;
  FCapacity := 128;
  SetLength(FLines, FCapacity);
end;

function TSynMarkupHighAllLines.MaybeReduceCapacity : Boolean;
begin
  if not( (FCapacity > 128) and (FCapacity > FCount*4) )
  then exit(False);

  FCapacity := FCapacity div 2;
  SetLength(FLines, FCapacity);
  result := true;
end;

procedure TSynMarkupHighAllLines.SetCount(const AValue : Integer);

begin
  if FCount=AValue then exit;
  FCount:=AValue;

  if MaybeReduceCapacity
  then exit;
  if (FCapacity <= FCount)
  then FCapacity := FCapacity * 2
  else exit;
  SetLength(FLines, FCapacity);
end;

function TSynMarkupHighAllLines.GetLine(const Index : Integer) : TSynMarkupHighAllLine;
begin
  Result := FLines[Index];
end;

procedure TSynMarkupHighAllLines.SetLine(const Index : Integer; const AValue : TSynMarkupHighAllLine);
begin
//  if Index = Count  then Count := Count + 1; // AutoIncrease
  FLines[Index] := AValue;
end;
*)

{ TSynMarkupHighAllPosList }

constructor TSynMarkupHighAllMatchList.Create;
begin
  FCount := 0;
  FCapacity := 256;
  SetLength(FMatches, FCapacity);
end;

function TSynMarkupHighAllMatchList.MaybeReduceCapacity : Boolean;
begin
  if not( (FCapacity > 512) and (FCapacity > FCount*4) )
  then exit(False);

  FCapacity := FCapacity div 2;
  SetLength(FMatches, FCapacity);
  result := true;
end;

procedure TSynMarkupHighAllMatchList.SetCount(const AValue : Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;

  if MaybeReduceCapacity
  then exit;
  if (FCapacity <= FCount)
  then FCapacity := FCapacity * 2
  else exit;
  SetLength(FMatches, FCapacity);
end;

function TSynMarkupHighAllMatchList.GetPointCount : Integer;
begin
  result := Count << 1;
end;

function TSynMarkupHighAllMatchList.GetPoint(const Index : Integer) : TPoint;
begin
  if (Index and 1) = 0
  then Result := FMatches[Index>>1].StartPoint
  else Result := FMatches[Index>>1].EndPoint
end;

function TSynMarkupHighAllMatchList.GetStartPoint(const Index : Integer) : TPoint;
begin
  Result := FMatches[Index].StartPoint;
end;

procedure TSynMarkupHighAllMatchList.SetStartPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index].StartPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetEndPoint(const Index : Integer) : TPoint;
begin
  Result := FMatches[Index].EndPoint;
end;

procedure TSynMarkupHighAllMatchList.SetEndPoint(const Index : Integer; const AValue : TPoint);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index].EndPoint := AValue;
end;

function TSynMarkupHighAllMatchList.GetMatch(const index : Integer) : TSynMarkupHighAllMatch;
begin
  Result := FMatches[Index];
end;

procedure TSynMarkupHighAllMatchList.SetMatch(const index : Integer; const AValue : TSynMarkupHighAllMatch);
begin
  if Index = Count
  then Count := Count + 1; // AutoIncrease
  FMatches[Index] := AValue;
end;


{ TSynEditMarkupHighlightAll }

constructor TSynEditMarkupHighlightAll.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  fStartPoint.y := -1;
  fSearch := TSynEditSearch.Create;
  fMatches := TSynMarkupHighAllMatchList.Create;
  fSearchString:='';
  fHasInvalidLines:=True;
  FHideSingleMatch := False;
end;

destructor TSynEditMarkupHighlightAll.Destroy;
begin
  FreeAndNil(fSearch);
  FreeAndNil(fMatches);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAll.SetSearchOptions(const AValue : TSynSearchOptions);
begin
  if fSearchOptions = AValue then exit;
  fSearchOptions := AValue;
  Invalidate;
  ValidateMatches;
end;

procedure TSynEditMarkupHighlightAll.SetSearchString(const AValue : String);
begin
  if fSearchString = AValue then exit;
  fSearchString := AValue;
  Invalidate;
  ValidateMatches; // bad if options and string search at the same time *and* string is <> ''
end;

procedure TSynEditMarkupHighlightAll.DoTopLineChanged(OldTopLine : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  Invalidate(False);
  ValidateMatches(False, (fStartPoint.y < TopLine) and (fStartPoint.y >= 0));
end;

procedure TSynEditMarkupHighlightAll.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
  // {TODO: Only do a partial search on the new area}
  Invalidate(False);;
  ValidateMatches(False, True);
end;

procedure TSynEditMarkupHighlightAll.DoMarkupChanged(AMarkup : TSynSelectedColor);
begin
  Invalidate; {TODO: only redraw, no search}
  ValidateMatches;
end;

procedure TSynEditMarkupHighlightAll.FindInitialize(Backward : Boolean);
begin
  fSearch.Pattern   := fSearchString;
  fSearch.Sensitive := ssoMatchCase in fSearchOptions;
  fSearch.Whole     := ssoWholeWord in fSearchOptions;
  fSearch.RegularExpressions := ssoRegExpr in fSearchOptions;
  fSearch.RegExprMultiLine   := ssoRegExprMultiLine in fSearchOptions;
  fSearch.Backwards := Backward;
end;

function TSynEditMarkupHighlightAll.GetMatchCount: Integer;
begin
  Result := fMatches.Count;
end;

procedure TSynEditMarkupHighlightAll.FindStartPoint;
var
  ptStart, ptEnd, ptFoundStart, ptFoundEnd: TPoint;
begin
  if fSearchString = '' then exit;

  FindInitialize(true);
  ptStart := Point(1, Max(1, TopLine-100));
  ptEnd.Y := TopLine;
  ptEnd.X := 1;
  if fSearch.FindNextOne(Lines, ptStart, ptEnd, ptFoundStart, ptFoundEnd)
  then fStartPoint := ptFoundStart
  else fStartPoint := ptEnd;
end;

procedure TSynEditMarkupHighlightAll.ValidateMatches(RePaint: Boolean = True;
  KeepStartPoint: Boolean = False);
var
  LastLine : Integer;
  Pos : Integer;
  ptStart, ptEnd, ptFoundStart, ptFoundEnd: TPoint;
begin
  if (fSearchString = '') or (not MarkupInfo.IsEnabled) then begin
    fMatches.Count := 0;
    exit;
  end;

  if not fHasInvalidLines
  then exit;
  fHasInvalidLines := false;

  LastLine := ScreenRowToRow(LinesInWindow);

  { TODO: need a list of invalidated lines, so we can keep valid matches }
  if not KeepStartPoint then
    FindStartPoint;
  ptStart := fStartPoint;
  Pos := 0;

  FindInitialize(false);
  ptEnd.Y:= min(ScreenRowToRow(LinesInWindow)+100, Lines.Count);
  ptEnd.X:= Length(Lines[ptEnd.y - 1]);

  While (true) do begin
    if not fSearch.FindNextOne(Lines, ptStart, ptEnd, ptFoundStart, ptFoundEnd)
    then break;
    ptStart := ptFoundEnd;

    fMatches.StartPoint[Pos] := LogicalToPhysicalPos(ptFoundStart);
    fMatches.EndPoint[Pos]:= LogicalToPhysicalPos(ptFoundEnd);

    if ptFoundStart.Y > LastLine
    then break;
    inc(Pos);
  end;

  fMatches.Count := Pos;
  fMatches.MaybeReduceCapacity;
  if RePaint and ( not(HideSingleMatch) or (fMatches.Count > 1) ) then
    SendLineInvalidation;
end;

procedure TSynEditMarkupHighlightAll.DoTextChanged(StartLine, EndLine: Integer);
begin
  if (fSearchString = '') then exit;
  Invalidate;
  ValidateMatches;
end;

procedure TSynEditMarkupHighlightAll.Invalidate(RePaint: Boolean = True);
begin
  if RePaint then
    SendLineInvalidation;
  fHasInvalidLines := True;
  fMatches.Count := 0;
  if SearchString = '' then fMatches.MaybeReduceCapacity;
end;

procedure TSynEditMarkupHighlightAll.SendLineInvalidation;
var
  Pos: Integer;
begin
  // Inform SynEdit which lines need repainting
  Pos := 0;
  while (Pos < fMatches.PointCount)
  do begin
    // first block, or new-end bigger last-end
    if ( (Pos = 0) or (fMatches.Point[Pos+1].y > fMatches.Point[Pos-1].y) )
    then
      InvalidateSynLines(fMatches.Point[Pos].y, fMatches.Point[Pos+1].y);
    inc(pos,2);
  end;
end;

function TSynEditMarkupHighlightAll.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
var
  Pos: Integer;
begin
  result := nil;
  if (fSearchString = '') then
  exit;

  if HideSingleMatch and (fMatches.Count <= 1) then exit;

  Pos:= 0;
  while (Pos < fMatches.PointCount)
  and ( (fMatches.Point[Pos].y < aRow)
       or ((fMatches.Point[Pos].y = aRow) and (fMatches.Point[Pos].x <= aCol)) )
  do inc(Pos);

  if Pos = fMatches.PointCount // last point was EndPoint => no markup
  then exit;
  
  if (Pos and 1)=0 // the next Point is a StartPoint => Outside Match
  then exit;
  
  //debugLN('+++>MARUP *ON* ',dbgs(@result),' / ',dbgs(ARow), ' at index ', dbgs(Pos));
  if fMatches.Point[Pos-1].y < aRow then
    MarkupInfo.StartX := -1
  else
    MarkupInfo.StartX := fMatches.Point[Pos-1].x;
  if fMatches.Point[Pos].y > aRow then
    MarkupInfo.EndX := -1
  else
    MarkupInfo.EndX := fMatches.Point[Pos].x-1;
  result := MarkupInfo;
end;

function TSynEditMarkupHighlightAll.GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer;
var
  Pos: Integer;
begin
  result := -1;
  if (fSearchString = '') then
  exit;

  if HideSingleMatch and (fMatches.Count <= 1) then exit;

  Pos:= 0;
  while (Pos < fMatches.PointCount)
  and ( (fMatches.Point[Pos].y < aRow)
       or ((fMatches.Point[Pos].y = aRow) and (fMatches.Point[Pos].x <= aCol)) )
  do inc(Pos);

  // what is ifthe next is an END, with a start at the same pos?

  if Pos = fMatches.PointCount
  then exit;
  
  if fMatches.Point[Pos].y <> aRow
  then exit;

  Result := fMatches.Point[Pos].x;
  //debugLN('--->NEXT POS ',dbgs(result),' / ',dbgs(ARow), ' at index ', dbgs(Pos));
end;

{ TSynEditMarkupHighlightAllCaret }

procedure TSynEditMarkupHighlightAllCaret.SetWaitTime(const AValue: Integer);
begin
  if FWaitTime = AValue then exit;
  FWaitTime := AValue;
  FTimer.Interval := FWaitTime;
  if FWaitTime = 0 then
    SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.SetSearchString(const AValue: String);
begin
  inherited SetSearchString(AValue);
  if AValue = '' then
    FLowBound.X := -1;
  FOldLowBound := FLowBound;
  FOldUpBound := FUpBound;
end;

procedure TSynEditMarkupHighlightAllCaret.SetFullWord(const AValue: Boolean);
begin
  if FFullWord = AValue then exit;
  FFullWord := AValue;
  SearchOptions := GetCurrentOption;
end;

procedure TSynEditMarkupHighlightAllCaret.SetFullWordMaxLen(const AValue: Integer);
begin
  if FFullWordMaxLen = AValue then exit;
  FFullWordMaxLen := AValue;
  SearchOptions := GetCurrentOption;
end;

procedure TSynEditMarkupHighlightAllCaret.SetHighlighter(const AValue: TSynCustomHighlighter);
begin
  if FHighlighter = AValue then exit;
  FHighlighter := AValue;
  ScrollTimerHandler(self);
end;

procedure TSynEditMarkupHighlightAllCaret.SetIgnoreKeywords(const AValue: Boolean);
begin
  if FIgnoreKeywords = AValue then exit;
  FIgnoreKeywords := AValue;
  ScrollTimerHandler(self);
end;

procedure TSynEditMarkupHighlightAllCaret.SetSelection(const AValue: TSynEditSelection);
begin
  if Assigned(FSelection) then
    FSelection.RemoveChangeHandler({$IFDEF FPC}@{$ENDIF}SelectionChanged);
  FSelection := AValue;
  if Assigned(FSelection) then
    FSelection.AddChangeHandler({$IFDEF FPC}@{$ENDIF}SelectionChanged);
end;

procedure TSynEditMarkupHighlightAllCaret.SetTrim(const AValue: Boolean);
begin
  if FTrim = AValue then exit;
  FTrim := AValue;
  ScrollTimerHandler(self);
end;

procedure TSynEditMarkupHighlightAllCaret.CheckState;
var
  t: String;
begin
  if (not FStateChanged) or (Caret = nil) then
    exit;
  FStateChanged := False;
  t := GetCurrentText;
  if (SearchString = t) and (SearchOptions = GetCurrentOption) then begin
    SearchString := t; // Update old bounds
    exit;
  end;
  if (SearchString <> '') and
     ( ((CompareCarets(FLowBound, FOldLowBound) = 0) and
       (CompareCarets(Caret.LineBytePos, FUpBound) >= 0) and (MatchCount > 1) )
      OR ((CompareCarets(FUpBound, FOldUpBound) = 0) and
       (CompareCarets(Caret.LineBytePos, FLowBound) <= 0) and (MatchCount > 1) )
     ) then begin
    ScrollTimerHandler(self);
    exit;
  end;

  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.SelectionChanged(Sender: TObject);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoCaretChanged(Sender: TObject);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoTextChanged(StartLine, EndLine: Integer);
begin
  FStateChanged := True; // Something changed, paint will be called
  inherited;
end;

procedure TSynEditMarkupHighlightAllCaret.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.RestartTimer;
begin
  FTimer.Enabled := False;
  if (MarkupInfo.IsEnabled) and (FWaitTime > 0) then
    FTimer.Enabled := True;
end;

procedure TSynEditMarkupHighlightAllCaret.ScrollTimerHandler(Sender: TObject);
begin
  FTimer.Enabled := False;
  if (SearchString = GetCurrentText) and (SearchOptions = GetCurrentOption) then
    exit;
  SearchString := ''; // prevent double update
  SearchOptions := GetCurrentOption;
  SearchString := GetCurrentText;
end;

function TSynEditMarkupHighlightAllCaret.GetCurrentText: String;
  function TrimS(s: String): String;
  var
    i: Integer;
  begin
    i := 1;
    while (i <= length(s)) and (s[i] in [#1..#32]) do inc(i);
    Result := copy(s, i, MaxInt);
    i := length(Result);
    while (i > 0) and (Result[i] in [#1..#32]) do dec(i);
    Result := copy(Result, 1, i);
  end;
var
  LowBnd, UpBnd: TPoint;
  i: integer;
begin
  if Caret = nil then
    exit('');
  if FToggledWord <> '' then
    exit(FToggledWord);
  If TSynEdit(SynEdit).SelAvail then begin
    LowBnd := TSynEdit(SynEdit).BlockBegin;
    UpBnd := TSynEdit(SynEdit).BlockEnd;
    i := UpBnd.y - LowBnd.y + 1;
    if (i > LowBnd.y) and (i > Lines.Count - UpBnd.y) then
      exit('');
    if FTrim then
      Result := TrimS(TSynEdit(SynEdit).SelText)
    else
      Result := TSynEdit(SynEdit).SelText;
    if TrimS(Result) = '' then Result := '';
    FLowBound := LowBnd;
    FUpBound := UpBnd;
  end else begin
    Result :=  TSynEdit(SynEdit).GetWordAtRowCol(Caret.LineBytePos);
    if FIgnoreKeywords and assigned(FHighlighter)
       and FHighlighter.IsKeyword(Result) then
      Result := '';
    FLowBound.Y := Caret.LinePos;
    FUpBound.Y := Caret.LinePos;
    TSynEdit(SynEdit).GetWordBoundsAtRowCol(Caret.LineBytePos, FLowBound.X, FUpBound.X);
  end;
end;

function TSynEditMarkupHighlightAllCaret.GetCurrentOption: TSynSearchOptions;
begin
  if FToggledWord <> '' then
    exit(FToggledOption);
  If TSynEdit(SynEdit).SelAvail or not(FFullWord) then
    Result := []
  else
    if (FFullWordMaxLen >0) and (UTF8Length(GetCurrentText) > FFullWordMaxLen) then
      Result := []
    else
      Result := [ssoWholeWord];
end;

constructor TSynEditMarkupHighlightAllCaret.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);
  FStateChanged := False;
  MarkupInfo.Clear;
  HideSingleMatch := True;
  FFullWord := False;
  FWaitTime := 1500;
  FTrim := True;
  FLowBound := Point(-1, -1);
  FUpBound := Point(-1, -1);
  FOldLowBound := Point(-1, -1);
  FOldUpBound := Point(-1, -1);
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := FWaitTime;
  FTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;
end;

destructor TSynEditMarkupHighlightAllCaret.Destroy;
begin
  if Assigned(FSelection) then
    FSelection.RemoveChangeHandler({$IFDEF FPC}@{$ENDIF}SelectionChanged);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TSynEditMarkupHighlightAllCaret.ToggleCurrentWord;
var
  s: String;
begin
  if FToggledWord = '' then begin
    FToggledWord := GetCurrentText;
    FToggledOption :=  GetCurrentOption;
  end else begin
    s := FToggledWord;
    FToggledWord := '';
    if GetCurrentText <> s then begin
      FToggledWord := GetCurrentText;
      FToggledOption :=  GetCurrentOption;
    end;
  end;
  SearchString := FToggledWord;
  SearchOptions := GetCurrentOption;
  if FToggledWord = '' then begin
    RestartTimer;
  end else begin
    ScrollTimerHandler(self);
  end;
end;

end.

