unit SynEditMarkupHighAll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditMarkup, SynEditTypes, SynEditSearch,
  SynEditMiscClasses, Controls;//, LCLProc;

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

    Procedure FindStartPoint;
    Procedure FindInitialize(Backward: Boolean);
    Procedure ValidateMatches;
    procedure SetSearchOptions(const AValue : TSynSearchOptions);
    procedure SetSearchString(const AValue : String);
  protected
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
  public
    constructor Create(ASynEdit : TCustomControl);
    destructor Destroy; override;

    Function GetMarkupAttributeAtRowCol(aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(aRow, aCol : Integer) : Integer; override;

    Procedure InvalidateScreenLines(aFirstCodeLine, aLastCodeLine: Integer);
    Procedure InvalidateLines(aFirstCodeLine, aLastCodeLine: Integer);
    Procedure Invalidate;
    Procedure SendLineInvalidation;

    property SearchString : String read fSearchString write SetSearchString;
    property SearchOptions : TSynSearchOptions read fSearchOptions write SetSearchOptions;
  end;
  
  
implementation

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
    fMatches.StartPoint[Pos] := ptFoundStart;
    fMatches.EndPoint[Pos]:= ptFoundEnd;
    inc(Pos);
  end;

  fMatches.Count := Pos;
  fMatches.MaybeReduceCapacity;
  SendLineInvalidation;
end;

procedure TSynEditMarkupHighlightAll.InvalidateScreenLines(aFirstCodeLine, aLastCodeLine : Integer);
begin
  InvalidateLines(ScreenRowToRow(aFirstCodeLine), ScreenRowToRow(aLastCodeLine));
end;

procedure TSynEditMarkupHighlightAll.InvalidateLines(aFirstCodeLine, aLastCodeLine : Integer);
var
  Pos: Integer;
begin
  if (fSearchString = '') or fInvalidating
  then exit;

  fInvalidating := True;
  // Invalidate neighbouring lines, which have an overlapping match
  Pos := 0;
  while (Pos < fMatches.Count)
  and (fMatches.EndPoint[Pos].y < aFirstCodeLine )
  do inc(Pos);
  // curent Match ends in aFirstCodeLine or later
  if fMatches.StartPoint[Pos].y < aFirstCodeLine
  then InvalidateSynLines(fMatches.StartPoint[Pos].y, aFirstCodeLine - 1);

  while (Pos < fMatches.Count)
  and (fMatches.EndPoint[Pos].y = aLastCodeLine )
  do inc(Pos);
  // curent Match ends after aLastCodeLine
  if fMatches.StartPoint[Pos].y <= aLastCodeLine
  then InvalidateSynLines(aLastCodeLine + 1, fMatches.EndPoint[Pos].y);

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
    if ( (Pos = 0) or (fMatches.Point[Pos].y <> fMatches.Point[Pos-1].y) )
    then InvalidateSynLines(fMatches.Point[Pos].y, fMatches.Point[Pos].y);
    inc(pos);
  end;

  fInvalidating := False;
end;

function TSynEditMarkupHighlightAll.GetMarkupAttributeAtRowCol(aRow, aCol : Integer) : TSynSelectedColor;
var
  Pos: Integer;
begin
  result := nil;
  if (fSearchString = '') then
  exit;

  ValidateMatches;
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
  result := MarkupInfo;
end;

function TSynEditMarkupHighlightAll.GetNextMarkupColAfterRowCol(aRow, aCol : Integer) : Integer;
var
  Pos: Integer;
begin
  result := -1;
  if (fSearchString = '') then
  exit;

  ValidateMatches;
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

end.

