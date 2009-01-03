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
  SynEditMiscClasses, Controls, LCLProc;

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
    fInvalidating, fHasInvalidLines : Boolean;
    FHideSingleMatch: Boolean;

    Procedure FindStartPoint;
    Procedure FindInitialize(Backward: Boolean);
    Procedure ValidateMatches;
    procedure SetSearchOptions(const AValue : TSynSearchOptions);
    procedure SetSearchString(const AValue : String);
  protected
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoTextChanged(StartLine, EndLine: Integer); override;
    property  HideSingleMatch: Boolean read FHideSingleMatch write FHideSingleMatch;
  public
    constructor Create(ASynEdit : TCustomControl);
    destructor Destroy; override;

    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;

    Procedure Invalidate;
    Procedure SendLineInvalidation;

    property SearchString : String read fSearchString write SetSearchString;
    property SearchOptions : TSynSearchOptions read fSearchOptions write SetSearchOptions;
  end;

  { TSynEditMarkupHighlightAllCaret }

  TSynEditMarkupHighlightAllCaret = class(TSynEditMarkupHighlightAll)
  private
    FTimer: TTimer;
    FWaitTime: Integer;
    procedure SetWaitTime(const AValue: Integer);
  protected
    procedure DoCaretChanged(OldCaret : TPoint); override;
    procedure DoTextChanged(StartLine, EndLine : Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure RestartTimer;
    procedure ScrollTimerHandler(Sender: TObject);
    function  GetCurrentText: String;
  public
    constructor Create(ASynEdit : TCustomControl);
    destructor Destroy; override;
    property  WaitTime: Integer read FWaitTime write SetWaitTime;
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

constructor TSynEditMarkupHighlightAll.Create(ASynEdit : TCustomControl);
begin
  inherited Create(ASynEdit);
  fSearch := TSynEditSearch.Create;
  fMatches := TSynMarkupHighAllMatchList.Create;
  fSearchString:='';
  fInvalidating := false;
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
  // {TODO: XXX}
  // Need indication if Bitmap was scrooled => Scrolled part does not need invalidateLines
  Invalidate;
  ValidateMatches;
end;

procedure TSynEditMarkupHighlightAll.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
  // {TODO: XXX}
  Invalidate;
  ValidateMatches;
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

procedure TSynEditMarkupHighlightAll.FindStartPoint;
var
  ptStart, ptEnd, ptFoundStart, ptFoundEnd: TPoint;
begin
  if fSearchString = '' then exit;

  FindInitialize(true);
  ptStart := Point(1, 1);
  ptEnd.Y := TopLine;
  ptEnd.X := 1;
  if fSearch.FindNextOne(Lines, ptStart, ptEnd, ptFoundStart, ptFoundEnd)
  then fStartPoint := ptFoundStart
  else fStartPoint := ptEnd;
end;

procedure TSynEditMarkupHighlightAll.ValidateMatches;
var
  LastLine : Integer;
  Pos : Integer;
  ptStart, ptEnd, ptFoundStart, ptFoundEnd: TPoint;
begin
  if (fSearchString = '') then begin
    fMatches.Count := 0;
    exit;
  end;

  if not fHasInvalidLines
  then exit;
  fHasInvalidLines := false;

  LastLine := ScreenRowToRow(LinesInWindow);

  { TODO: need a list of invalidated lines, so we can keep valid matches }
  FindStartPoint;
  ptStart := fStartPoint;
  Pos := 0;

  FindInitialize(false);
  ptEnd.Y:= Lines.Count;
  ptEnd.X:= Length(Lines[ptEnd.y - 1]);

  While (true) do begin
    if not fSearch.FindNextOne(Lines, ptStart, ptEnd, ptFoundStart, ptFoundEnd)
    then break;
    if ptFoundStart.Y > LastLine
    then break;
    ptStart := ptFoundEnd;

    { TODO: skip if all folded }
    fMatches.StartPoint[Pos] := LogicalToPhysicalPos(ptFoundStart);
    fMatches.EndPoint[Pos]:= LogicalToPhysicalPos(ptFoundEnd);
    inc(Pos);
  end;

  fMatches.Count := Pos;
  fMatches.MaybeReduceCapacity;
  if not(HideSingleMatch) or (fMatches.Count > 1) then
    SendLineInvalidation;
end;

procedure TSynEditMarkupHighlightAll.DoTextChanged(StartLine, EndLine: Integer);
var
  Pos: Integer;
begin
  if (fSearchString = '') or fInvalidating
  then exit;

  fInvalidating := True;
  // Invalidate neighbouring lines, which have an overlapping match
  Pos := 0;
  while (Pos < fMatches.Count)
  and (fMatches.EndPoint[Pos].y < StartLine)
  do inc(Pos);
  // curent Match ends in aFirstCodeLine or later
  if fMatches.StartPoint[Pos].y < StartLine
  then InvalidateSynLines(fMatches.StartPoint[Pos].y, StartLine - 1);

  while (Pos < fMatches.Count)
  and (fMatches.EndPoint[Pos].y = EndLine )
  do inc(Pos);
  // curent Match ends after aLastCodeLine
  if fMatches.StartPoint[Pos].y <= EndLine
  then InvalidateSynLines(EndLine + 1, fMatches.EndPoint[Pos].y);

  fInvalidating := False;
  fHasInvalidLines := true;
end;

procedure TSynEditMarkupHighlightAll.Invalidate;
begin
  SendLineInvalidation;
  fHasInvalidLines := True;
  fMatches.Count := 0;
  if SearchString = '' then fMatches.MaybeReduceCapacity;
end;

procedure TSynEditMarkupHighlightAll.SendLineInvalidation;
var
  Pos: Integer;
begin
  fInvalidating := True;
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

  fInvalidating := False;
end;

function TSynEditMarkupHighlightAll.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
var
  Pos: Integer;
begin
  result := nil;
  if (fSearchString = '') then
  exit;

  ValidateMatches;
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

  ValidateMatches;
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
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.DoCaretChanged(OldCaret: TPoint);
begin
  if SearchString = GetCurrentText then exit;
  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.DoTextChanged(StartLine, EndLine: Integer);
begin
  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  SearchString := '';
  RestartTimer;
end;

procedure TSynEditMarkupHighlightAllCaret.RestartTimer;
begin
  FTimer.Enabled := False;
  if MarkupInfo.IsEnabled then
    FTimer.Enabled := True;
end;

procedure TSynEditMarkupHighlightAllCaret.ScrollTimerHandler(Sender: TObject);
begin
  SearchString := GetCurrentText;
end;

function TSynEditMarkupHighlightAllCaret.GetCurrentText: String;
begin
  If TCustomSynEdit(SynEdit).SelAvail then
    Result := TCustomSynEdit(SynEdit).SelText
  else
    Result :=  TCustomSynEdit(SynEdit).GetWordAtRowCol
      (TCustomSynEdit(SynEdit).PhysicalToLogicalPos(TCustomSynEdit(SynEdit).CaretXY));
end;

constructor TSynEditMarkupHighlightAllCaret.Create(ASynEdit: TCustomControl);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Clear;
  HideSingleMatch := True;
  FWaitTime := 1500;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := FWaitTime;
  FTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;
end;

destructor TSynEditMarkupHighlightAllCaret.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

end.

