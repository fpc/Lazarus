unit TALinePatterns;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpImage, fpCanvas;

type
  TLinePattern = array of LongWord;  // even index = length of line, odd = length of space

  TLinePatternPainter = class
  private
    FPenStyle: TFPPenStyle;
    FPenWidth: Integer;
    FPattern: TLinePattern;
    FElementIndex: Integer;
    FLeftOverLength: Integer;
  protected
    FCurrentX, FCurrentY: Integer;
    procedure DoDrawOrMoveTo(X, Y: Integer);
    procedure DoLineTo(X, Y: Integer); virtual; abstract;
    procedure DoMoveTo(X, Y: Integer); virtual; abstract;
    procedure NextPatternElement;
    function PatternPoint(P1, P2: TPoint; AFraction: Double): TPoint;
    procedure SetPenStyle(AValue: TFPPenStyle); virtual;
    procedure SetPenWidth(AValue: Integer); virtual;
    procedure PreparePattern(APenStyle: TFPPenStyle; APenWidth: Integer);
    property PenStyle: TFPPenStyle read FPenStyle write SetPenStyle;
    property PenWidth: Integer read FPenWidth write SetPenWidth;
  public
    constructor Create;
    function GetPattern: TLinePattern;
    procedure Line(P1, P2: TPoint);
    procedure Line(X1, Y1, X2, Y2: Integer);
    procedure LineTo(P: TPoint);
    procedure LineTo(X, Y: Integer);
    procedure MoveTo(P: TPoint);
    procedure MoveTo(X, Y: Integer);
    procedure PolyLine(const APoints: Array of TPoint);
    procedure Polyline(const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure Prepare(APenStyle: TFPPenStyle; APenWidth: Integer);
    procedure ResetPattern;
    procedure SetPattern(const APattern: TLinePattern);
  end;

implementation

constructor TLinePatternPainter.Create;
begin
  inherited;
  FPenWidth := 1;
end;

procedure TLinePatternPainter.DoDrawOrMoveTo(X, Y: Integer);
begin
  if odd(FElementIndex) then
    DoMoveTo(X, Y)
  else
    DoLineTo(X, Y);
end;

function TLinePatternPainter.GetPattern: TLinePattern;
begin
  SetLength(Result, Length(FPattern));
  Move(FPattern[0], Result[0], Length(FPattern) * SizeOf(LongWord));
end;

procedure TLinePatternPainter.Line(X1, Y1, X2, Y2: Integer);
begin
  Line(Point(X1, Y1), Point(X2, Y2));
end;

{ Draws a line from P1 to P2 in the pattern defined by FPenStyle.
  Pattern elements reaching beyond the limiting points are remembered and
  used when the next Line call is made. }
procedure TLinePatternPainter.Line(P1, P2: TPoint);
var
  totalLength: Integer;
  drawnLength: Integer;
  P: TPoint;
  done: Boolean;
begin
  // Handle trivial cases
  if FPenStyle = psClear then
    exit;

  if (P1 = P2) then
    exit;

  if (FPenStyle = psSolid) or (FPenStyle = psInsideFrame) then
  begin
    DoMoveTo(P1.X, P1.Y);
    DoLineTo(P2.X, P2.Y);
    exit;
  end;

  // Total length of the line beteween P1 and P2
  totalLength := round(sqrt(sqr(P2.X - P1.X) + sqr(P2.Y - P1.Y)));

  // Move to start point
  MoveTo(P1.X, P1.Y);

  // Draw left-over of the pattern from previous "line" call
  if FLeftOverLength > 0 then
  begin
    if FLeftOverLength > totalLength then
    begin
      P := P2;
      DoDrawOrMoveTo(P.X, P.Y);
      FLeftOverLength := FLeftOverLength - totalLength;
      exit;
    end else
    begin
      P := PatternPoint(P1, P2, FLeftOverLength/totalLength);
      DoDrawOrMoveTo(P.X, P.Y);
      drawnLength := FLeftOverLength;
      FLeftOverLength := 0;
      NextPatternElement;
    end;
  end else
    drawnLength := 0;

  // Draw patterned line to (X2, Y2)
  done := false;
  repeat
    drawnLength := drawnLength + FPattern[FElementIndex];
    if drawnLength <= totalLength then
    begin
      P := PatternPoint(P1, P2, drawnLength/totalLength);
      DoDrawOrMoveTo(P.X, P.Y);
      NextPatternElement;
      FLeftOverLength := 0;
    end else
    begin
      P := P2;
      DoDrawOrMoveTo(P.X, P.Y);
      FLeftOverLength := drawnLength - totalLength;
      done := true;
    end;
  until done;
end;

procedure TLinePatternPainter.LineTo(P: TPoint);
begin
  LineTo(P.X, P.Y);
end;

procedure TLinePatternPainter.LineTo(X, Y: Integer);
begin
  Line(FCurrentX, FCurrentY, X, Y);
  FCurrentX := X;
  FCurrentY := Y;
end;

procedure TLinePatternPainter.MoveTo(P: TPoint);
begin
  MoveTo(P.X, P.Y);
end;

procedure TLinePatternPainter.MoveTo(X, Y: Integer);
begin
  FCurrentX := X;
  FCurrentY := Y;
  DoMoveTo(X, Y);
end;

procedure TLinePatternPainter.NextPatternElement;
begin
  inc(FElementIndex);
  if FElementIndex = Length(FPattern) then
    FElementIndex := 0;
end;

procedure TLinePatternPainter.PolyLine(const APoints: Array of TPoint);
begin
  PolyLine(APoints, 0, Length(APoints));
end;

procedure TLinePatternPainter.PolyLine(const APoints: Array of TPoint;
  AStartIndex, ANumPts: Integer);
var
  i: Integer;
begin
  MoveTo(APoints[AStartIndex].X, APoints[AStartIndex].Y);
  for i := AStartIndex + 1 to AStartIndex + ANumPts - 1 do
    LineTo(APoints[i].X, APoints[i].Y);
end;

procedure TLinePatternPainter.Prepare(APenStyle: TFPPenStyle;
  APenWidth: Integer);
begin
  FPenWidth := APenWidth;
  PenStyle := APenStyle;
  ResetPattern;
end;

{ Note: Dots in the pattern should be given a length 1 because then it is not
  scaled by the multiplier. }
procedure TLinePatternPainter.PreparePattern(APenStyle: TFPPenStyle;
  APenWidth: Integer);
const
  DASH_PATTERN: array[0..1] of LongWord = (12, 6);
  DOT_PATTERN: array[0..1] of LongWord = (1, 4);
  DASH_DOT_PATTERN: array[0..3] of LongWord = (12, 6, 1, 6);
  DASH_DOT_DOT_PATTERN: array[0..5] of LongWord = (12, 6, 1, 6, 1, 6);
var
  i: Integer;
begin
  case APenStyle of
    psSolid, psClear, psInsideFrame:
      exit;
    psDash:
      begin
        SetLength(FPattern, Length(DASH_PATTERN));
        Move(DASH_PATTERN, FPattern[0], Sizeof(DASH_PATTERN));
      end;
    psDot:
      begin
        SetLength(FPattern, Length(DOT_PATTERN));
        Move(DOT_PATTERN, FPattern[0], SizeOf(DOT_PATTERN));
      end;
    psDashDot:
      begin
        SetLength(FPattern, Length(DASH_DOT_PATTERN));
        Move(DASH_DOT_PATTERN, FPattern[0], SizeOf(DASH_DOT_PATTERN));
      end;
    psDashDotDot:
      begin
        SetLength(FPattern, Length(DASH_DOT_DOT_PATTERN));
        Move(DASH_DOT_DOT_PATTERN, FPattern[0], SizeOf(DASH_DOT_DOT_PATTERN));
      end;
    psPattern:
      ;
  end;

  // Adjust pattern lengths to pen width
  for i := 0 to High(FPattern) do
  begin
    if odd(i) then
      FPattern[i] := FPattern[i] + APenWidth - 1     // Prevent spaces from closing
    else
    if (FPattern[i] = 1) and (APenWidth = 1) then    // Make dots a bit larger for thinnest lines
      FPattern[i] := 2;
  end;
end;

function TLinePatternPainter.PatternPoint(P1, P2: TPoint; AFraction: Double): TPoint;
begin
  Result.X := P1.X + round((P2.X - P1.X) * AFraction);
  Result.Y := P1.Y + round((P2.Y - P1.Y) * AFraction);
end;

procedure TLinePatternPainter.ResetPattern;
begin
  FElementIndex := 0;
  FLeftOverLength := 0;
  PreparePattern(FPenStyle, FPenWidth);
end;

procedure TLinePatternPainter.SetPattern(const APattern: TLinePattern);
begin
  SetLength(FPattern, Length(APattern));
  Move(APattern[0], FPattern[0], Length(APattern) * SizeOf(LongWord));
  FPenStyle := psPattern;
  ResetPattern;
end;

procedure TLinePatternPainter.SetPenStyle(AValue: TFPPenStyle);
begin
  FPenStyle := AValue;
  ResetPattern;
end;

procedure TLinePatternPainter.SetPenWidth(AValue: Integer);
begin
  FPenWidth := AValue;
  ResetPattern;
end;

end.

