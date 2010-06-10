{ agg_lcl.pas }
unit
 Agg_LCL;

{$mode objfpc}{$H+}

interface

uses
 sysutils,
 {$IFDEF AGG_WINDOWS}
 Windows ,
 {$ENDIF}
 Classes ,Graphics, LCLProc, types, IntfGraphics, GraphType, FPimage, FPCanvas,
 {$IFDEF LCLGtk2}
 pango, LCLType, Gtk2Proc, Gtk2Def, gtk2,
 {$ENDIF}
 agg_arc, GraphMath, agg_fpimage, agg_basics;

type

  { TAggLCLImage }

  TAggLCLImage = class(TAggFPImage)
  private
    FIntfImg: TLazIntfImage;
  protected
    procedure ReallocData; override;
  public
    constructor Create(AWidth, AHeight: integer); override;
    destructor Destroy; override;
    property IntfImg: TLazIntfImage read FIntfImg;
  end;

  { TAggLCLBrush }

  TAggLCLBrush = class(TAggFPBrush)
  private
    FColor: TColor;
  protected
    procedure SetColor(const AValue: TColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
  public
    property Color: TColor read FColor write SetColor;
  end;

  { TAggLCLPen }

  TAggLCLPen = class(TAggFPPen)
  private
    FColor: TColor;
  protected
    procedure SetColor(const AValue: TColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
  public
    property Color: TColor read FColor write SetColor;
  end;

  { TAggLCLFont }

  TAggLCLFont = class(TAggFPFont)
  private
    FColor: TColor;
    FPixelsPerInch: Integer;
  protected
    procedure SetColor(const AValue: TColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
  public
    constructor Create; override;
    function AggHeightToSize(const h: double): double; override;
    function SizeToAggHeight(const s: double): double; override;
    {$IFDEF LCLGtk2}
    procedure LoadViaPango;
    {$ENDIF}
    property Color: TColor read FColor write SetColor;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
  end;

  { TAggLCLCanvas }

  TAggLCLCanvas = class(TAggFPCanvas)
  private
    FALBrush: TAggLCLBrush;
    FALFont: TAggLCLFont;
    FALImage: TAggLCLImage;
    FALPen: TAggLCLPen;
    FLockInitialzed: boolean;
    FLock: TRTLCriticalSection;// FLock is initialized on demand
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    function DoCreateDefaultBrush: TFPCustomBrush; override;
    function DoCreateDefaultFont: TFPCustomFont; override;
    function DoCreateDefaultPen: TFPCustomPen; override;
    function DoCreateDefaultImage: TAggFPImage; override;
    procedure DoLockCanvas; override;
    procedure DoUnlockCanvas; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearSettings; override;

    property Pen: TAggLCLPen read FALPen;
    property Brush: TAggLCLBrush read FALBrush;
    property Font: TAggLCLFont read FALFont;
    property Image: TAggLCLImage read FALImage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;

    procedure Lock; virtual;
    function TryLock: Boolean;
    procedure Unlock; virtual;
    procedure Changing; virtual;
    procedure Changed; virtual;

    // extra drawing methods (there are more in the ancestor TFPCustomCanvas)
    procedure Arc(ALeft, ATop, ARight, ABottom, StartAngle, AngleLength: Integer); virtual;
    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual;
    procedure Chord(ALeft, ATop, ARight, ABottom, StartAngle, AngleLength: Integer); virtual;
    procedure Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual;
    function LCLAngleToAggAngle(const angle: double): double;

    procedure FillRect(const ARect: TRect); virtual; // no border
    procedure FillRect(X1,Y1,X2,Y2: Integer);        // no border

    procedure Frame(const ARect: TRect); virtual; // border using pen
    procedure Frame(X1,Y1,X2,Y2: Integer);        // border using pen

    procedure GradientFill(ARect: TRect; AStart, AStop: TColor; ADirection: TGradientDirection);

    procedure RadialPie(x1, y1, x2, y2,
                        StartAngle16Deg, Angle16DegLength: Integer); virtual;
    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False); virtual;
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);// ToDo: winding
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False); virtual;// ToDo: winding
    procedure PolyLine(const Points: array of TPoint;
                       StartIndex: Integer = 0;
                       NumPts: Integer = -1);
    procedure PolyLine(Points: PPoint; NumPts: Integer); virtual;

    procedure RoundRect(X1, Y1, X2, Y2: Integer; DX,DY: Integer); virtual;
    procedure RoundRect(const aRect: TRect; DX,DY: Integer);

    procedure TextRect(const ARect: TRect; X, Y: integer; const Text: string);
    function TextExtent(const Text: string): TSize; virtual;
    function TextHeight(const Text: string): Integer; virtual;
    function TextWidth(const Text: string): Integer; virtual;
    procedure AggTextOut(const x, y: double; str: AnsiString;
               roundOff: boolean=false;
               const ddx: double=0.0; const ddy: double=0.0); override;
  end;

procedure InitAggPasRawImageDescriptor(APixelFormat: TAggFPImgPixelFormat;
                       AWidth, AHeight: cardinal; out Desc: TRawImageDescription);
function AggToLCLColor(const c: TAggColor): TColor;
function LCLToAggColor(const c: TColor): TAggColor;
function dbgs(const c: TAggColor): string; overload;

implementation

procedure InitAggPasRawImageDescriptor(APixelFormat: TAggFPImgPixelFormat;
  AWidth, AHeight: cardinal; out Desc: TRawImageDescription);
begin
  FillByte(Desc, SizeOf(Desc), 0);
  with Desc do begin
    Format := ricfRGBA;
    if APixelFormat=afpimRGB24 then
      Depth := 24 // used bits per pixel
    else
      Depth := 32; // used bits per pixel
    Width := AWidth;
    Height := AHeight;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    BitsPerPixel := Depth; // bits per pixel. can be greater than Depth.
    LineEnd := rileByteBoundary;
    RedPrec := 8; // red precision. bits for red
    RedShift := 0;
    GreenPrec := 8;
    GreenShift := 8; // bitshift. Direction: from least to most significant
    BluePrec := 8;
    BlueShift := 16;
    if APixelFormat=afpimRGBA32 then begin
      AlphaPrec := 8;
      AlphaShift := 24;
    end;
  end;
end;

function AggToLCLColor(const c: TAggColor): TColor;
begin
  if c.a<>0 then
    Result:=RGBToColor(c.r,c.g,c.b)
  else
    Result:=clNone;
end;

function LCLToAggColor(const c: TColor): TAggColor;
begin
  Result.a:=255;
  RedGreenBlue(ColorToRGB(c),Result.r,Result.g,Result.b);
end;

function dbgs(const c: TAggColor): string; overload;
begin
  Result:='r='+IntToStr(c.r)+',g='+IntToStr(c.g)+',b='+IntToStr(c.b)+',a='+IntToStr(c.a);
end;

{ TAggLCLCanvas }

function TAggLCLCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TAggLCLBrush.Create;
end;

function TAggLCLCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result:=TAggLCLFont.Create;
end;

function TAggLCLCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TAggLCLPen.Create;
end;

function TAggLCLCanvas.DoCreateDefaultImage: TAggFPImage;
begin
  Result:=TAggLCLImage.Create(0,0);
end;

procedure TAggLCLCanvas.DoLockCanvas;
begin
  if not FLockInitialzed then begin
    InitCriticalSection(FLock);
    FLockInitialzed:=true;
  end;
  EnterCriticalsection(FLock);
  inherited DoLockCanvas;
end;

procedure TAggLCLCanvas.DoUnlockCanvas;
begin
  LeaveCriticalsection(FLock);
  inherited DoUnlockCanvas;
end;

constructor TAggLCLCanvas.Create;
begin
  inherited Create;
  FALFont := TAggLCLFont(inherited Font);
  FALPen := TAggLCLPen(inherited Pen);
  FALBrush := TAggLCLBrush(inherited Brush);
  FALImage := TAggLCLImage(inherited Image);
end;

destructor TAggLCLCanvas.Destroy;
begin
  if FLockInitialzed then begin
    DoneCriticalsection(FLock);
    FLockInitialzed:=false;
  end;
  inherited Destroy;
  // set resources to nil, so that dangling pointers are spotted early
  FALBrush:=nil;
  FALPen:=nil;
  FALFont:=nil;
  FALImage:=nil;
end;

procedure TAggLCLCanvas.ClearSettings;
begin
  inherited ClearSettings;
end;

procedure TAggLCLCanvas.Lock;
begin
  LockCanvas;
end;

function TAggLCLCanvas.TryLock: Boolean;
begin
  Result := not Locked;
  if Result then
    Lock;
end;

procedure TAggLCLCanvas.Unlock;
begin
  UnlockCanvas;
end;

procedure TAggLCLCanvas.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TAggLCLCanvas.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAggLCLCanvas.Arc(ALeft, ATop, ARight, ABottom,
  StartAngle, AngleLength: Integer);
{ Use Arc to draw an elliptically curved line with the current Pen.
  The angles angle1 and angle2 are 1/16th of a degree. For example, a full
  circle equals 5760 (16*360). Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.
}
var
  cx, cy, rx, ry, start, endangle, h: double;
begin
  if AngleLength=0 then exit;
  cx:=double(ALeft+ARight)/2+0.5;
  cy:=double(ATop+ABottom)/2+0.5;
  rx:=double(ARight-ALeft)/2;
  ry:=double(ABottom-ATop)/2;
  // counter clockwise to clockwise
  start:=LCLAngleToAggAngle(StartAngle+AngleLength);
  endangle:=LCLAngleToAggAngle(StartAngle);
  if AngleLength<0 then begin
    h:=start;
    start:=endangle;
    endangle:=h;
  end;
  AggArc(cx,cy,rx,ry,start,endangle);
end;

procedure TAggLCLCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
var
  StartAngle: Extended;
  AngleLength: Extended;
  cx, cy, rx, ry, start, endangle, h: double;
begin
  Coords2Angles(ALeft, ATop, ARight-ALeft, ABottom-ATop, SX, SY, EX, EY,
                StartAngle, AngleLength);
  if AngleLength=0 then exit;
  cx:=double(ALeft+ARight)/2;
  cy:=double(ATop+ABottom)/2;
  rx:=double(ARight-ALeft)/2;
  ry:=double(ABottom-ATop)/2;
  // counter clockwise to clockwise
  start:=LCLAngleToAggAngle(StartAngle+AngleLength);
  endangle:=LCLAngleToAggAngle(StartAngle);
  if AngleLength<0 then begin
    h:=start;
    start:=endangle;
    endangle:=h;
  end;
  AggArc(cx,cy,rx,ry,start,endangle);
end;

procedure TAggLCLCanvas.Chord(ALeft, ATop, ARight, ABottom, StartAngle,
  AngleLength: Integer);
{ Same as Arc, but closed and filled with Brush.
}
var
  cx, cy, rx, ry, start, endangle, h: double;
  ar : agg_arc.arc;
begin
  if AngleLength=0 then exit;
  cx:=double(ALeft+ARight)/2+0.5;
  cy:=double(ATop+ABottom)/2+0.5;
  rx:=double(ARight-ALeft)/2;
  ry:=double(ABottom-ATop)/2;
  // counter clockwise to clockwise
  start:=LCLAngleToAggAngle(StartAngle+AngleLength);
  endangle:=LCLAngleToAggAngle(StartAngle);
  if AngleLength<0 then begin
    h:=start;
    start:=endangle;
    endangle:=h;
  end;

  Path.m_path.remove_all;

  ar.Construct(cx ,cy ,rx ,ry ,endangle ,start ,false );

  Path.m_path.add_path(@ar ,0 ,false );
  AggClosePolygon;

  AggDrawPath(AGG_FillAndStroke);
end;

procedure TAggLCLCanvas.Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX,
  EY: Integer);
var
  StartAngle: Extended;
  AngleLength: Extended;
  cx, cy, rx, ry, start, endangle: double;
  ar : agg_arc.arc;
begin
  Coords2Angles(ALeft, ATop, ARight-ALeft, ABottom-ATop, SX, SY, EX, EY,
                StartAngle, AngleLength);
  if AngleLength=0 then exit;
  cx:=double(ALeft+ARight)/2+0.5;
  cy:=double(ATop+ABottom)/2+0.5;
  rx:=double(ARight-ALeft)/2;
  ry:=double(ABottom-ATop)/2;
  start:=LCLAngleToAggAngle(StartAngle+AngleLength);
  endangle:=LCLAngleToAggAngle(StartAngle);
  Path.m_path.remove_all;

  ar.Construct(cx ,cy ,rx ,ry ,endangle ,start ,AngleLength<0 );

  Path.m_path.add_path(@ar ,0 ,false );
  AggClosePolygon;

  AggDrawPath(AGG_FillAndStroke);
end;

function TAggLCLCanvas.LCLAngleToAggAngle(const angle: double): double;
// both: 0 = 3'o clock
// LCL: counter clockwise, Agg: clockwise
// full circle: LCL = 5760, Agg = 2*pi
begin
  Result:=2*pi* (1-(angle / 5760));
end;

procedure TAggLCLCanvas.FillRect(const ARect: TRect);
begin
  Fillrect(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

procedure TAggLCLCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(X1+0.5,Y1+0.5);
  Path.m_path.line_to(X2+0.5,Y1+0.5);
  Path.m_path.line_to(X2+0.5,Y2+0.5);
  Path.m_path.line_to(X1+0.5,Y2+0.5);
  AggClosePolygon;
  AggDrawPath(AGG_FillOnly);
end;

procedure TAggLCLCanvas.AggTextOut(const x, y: double; str: AnsiString;
  roundOff: boolean; const ddx: double; const ddy: double);
begin
  inherited AggTextOut(x-0.5, y+Font.Size+0.5, str, roundOff, ddx, ddy);
end;

procedure TAggLCLCanvas.Frame(const ARect: TRect);
begin
  Frame(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
end;

procedure TAggLCLCanvas.Frame(X1, Y1, X2, Y2: Integer);
begin
  Path.m_path.remove_all;
  Path.m_path.move_to(X1+0.5,Y1+0.5);
  Path.m_path.line_to(X2+0.5,Y1+0.5);
  Path.m_path.line_to(X2+0.5,Y2+0.5);
  Path.m_path.line_to(X1+0.5,Y2+0.5);
  AggClosePolygon;
  AggDrawPath(AGG_StrokeOnly);
end;

procedure TAggLCLCanvas.GradientFill(ARect: TRect; AStart, AStop: TColor;
  ADirection: TGradientDirection);
var
  x1,y1,x2,y2: double;
begin
  x1:=ARect.Left+0.5;
  y1:=ARect.Top+0.5;
  x2:=ARect.Right+0.5;
  y2:=ARect.Bottom+0.5;
  if ADirection=gdVertical then
    AggFillLinearGradient(x1,y1,x1,y2,LCLToAggColor(AStart),LCLToAggColor(AStop))
  else
    AggFillLinearGradient(x1,y1,x2,y1,LCLToAggColor(AStart),LCLToAggColor(AStop));
  Path.m_path.remove_all;
  Path.m_path.move_to(x1,y1);
  Path.m_path.line_to(x2,y1);
  Path.m_path.line_to(x2,y2);
  Path.m_path.line_to(x1,y2);
  AggClosePolygon;
  AggDrawPath(AGG_FillOnly);
  m_fillGradientFlag:=AGG_Solid;
end;

procedure TAggLCLCanvas.RadialPie(x1, y1, x2, y2, StartAngle16Deg,
  Angle16DegLength: Integer);
{ Use RadialPie to draw a filled pie-shaped wedge on the canvas.
  The angles StartAngle16Deg and EndAngle16Deg are 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).
  Positive values of Angle and AngleLength mean
  counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position.
}
var
  cx, cy, rx, ry: double;
  a, da, startangle, endangle: Double;
begin
  if Angle16DegLength=0 then exit;
  Path.m_path.remove_all;
  cx:=double(x1+x2)/2+0.5;
  cy:=double(y1+y2)/2+0.5;
  rx:=double(x2-x1)/2;
  ry:=double(y2-y1)/2;
  da:=PI/16;
  startangle:=LCLAngleToAggAngle(StartAngle16Deg+Angle16DegLength);
  endangle:=LCLAngleToAggAngle(StartAngle16deg);
  if startangle>endangle then begin
    a:=startangle;
    startangle:=endangle;
    endangle:=a;
  end;

  Path.m_path.move_to(cx,cy);
  a:=startangle;
  while a<endangle do begin
    Path.m_path.line_to(cx+rx*cos(a),cy+ry*sin(a));
    a:=a+da;
  end;
  Path.m_path.line_to(cx+rx*cos(endangle),cy+ry*sin(endangle));

  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke);
end;

procedure TAggLCLCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX,
  StartY, EndX, EndY: Integer);
{ Use Pie to draw a filled Pie-shaped wedge on the canvas. The pie is part of
  an ellipse between the points EllipseX1, EllipseY1, EllipseX2, EllipseY2.
  The values StartX, StartY and EndX, EndY represent the starting and ending
  radial-points between which the Bounding-Arc is drawn.
}
var
  cx, cy, rx, ry: double;
  StartAngle16deg, AngleLength16deg: extended;
  a, da, startangle, endangle: Double;
begin
  Coords2Angles(EllipseX1, EllipseY1, EllipseX2-EllipseX1, EllipseY2-EllipseY1,
                StartX, StartY, EndX, EndY,
                StartAngle16deg, AngleLength16deg);
  if AngleLength16deg<=0 then exit;

  Path.m_path.remove_all;
  cx:=double(EllipseX1+EllipseX2)/2+0.5;
  cy:=double(EllipseY1+EllipseY2)/2+0.5;
  rx:=double(EllipseX2-EllipseX1)/2;
  ry:=double(EllipseY2-EllipseY1)/2;
  da:=PI/16;
  startangle:=LCLAngleToAggAngle(StartAngle16deg+AngleLength16deg);
  endangle:=LCLAngleToAggAngle(StartAngle16deg);
  if startangle>endangle then begin
    a:=startangle;
    startangle:=endangle;
    endangle:=a;
  end;

  Path.m_path.move_to(cx,cy);
  a:=startangle;
  while a<endangle do begin
    Path.m_path.line_to(cx+rx*cos(a),cy+ry*sin(a));
    a:=a+da;
  end;
  Path.m_path.line_to(cx+rx*cos(endangle),cy+ry*sin(endangle));

  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke);
end;

procedure TAggLCLCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean; Continuous: boolean);
{ Use Polybezier to draw cubic Bézier curves. The first curve is drawn from the
  first point to the fourth point with the second and third points being the
  control points. If the Continuous flag is TRUE then each subsequent curve
  requires three more points, using the end-point of the previous Curve as its
  starting point, the first and second points being used as its control points,
  and the third point its end-point. If the continous flag is set to FALSE,
  then each subsequent Curve requires 4 additional points, which are used
  exactly as in the first curve. Any additonal points which do not add up to
  a full bezier(4 for Continuous, 3 otherwise) are ignored. There must be at
  least 4 points for an drawing to occur. If the Filled Flag is set to TRUE
  then the resulting Poly-Bézier will be drawn as a Polygon.
}
var
  LinePoints: PPoint;
  LinePointCount: integer;
begin
  if NumPts<4 then exit;
  LinePoints:=nil;
  LinePointCount:=0;
  PolyBezier2Polyline(Points,NumPts,LinePoints,LinePointCount,Continuous);
  if LinePointCount>1 then begin
    if Filled then
      Polygon(LinePoints,LinePointCount)
    else
      Polyline(LinePoints,LinePointCount);
  end;
  ReAllocMem(LinePoints,0);
end;

procedure TAggLCLCanvas.PolyBezier(const Points: array of TPoint;
  Filled: boolean; Continuous: boolean);
begin
  if length(Points)<2 then exit;
  PolyBezier(@Points[0],length(Points),Filled,Continuous);
end;

procedure TAggLCLCanvas.Polygon(const Points: array of TPoint;
  Winding: Boolean; StartIndex: Integer; NumPts: Integer);
begin
  if NumPts=0 then exit;
  if StartIndex<low(Points) then exit;
  if StartIndex>=high(Points) then exit;
  if (NumPts<0) or (StartIndex+NumPts-1>high(Points)) then
    NumPts:=High(Points)-StartIndex+1;
  Polygon(@Points[StartIndex],NumPts,Winding);
end;

procedure TAggLCLCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
var
  i: Integer;
begin
  if NumPts<=1 then exit;
  Path.m_path.remove_all;
  i:=0;
  Path.m_path.move_to(points[i].x+0.5 ,points[i].y+0.5 );
  inc(i);
  while i<NumPts do begin
    Path.m_path.line_to(points[i].x+0.5,points[i].y+0.5);
    inc(i);
  end;
  AggClosePolygon;
  AggDrawPath(AGG_FillOnly);
end;

procedure TAggLCLCanvas.PolyLine(const Points: array of TPoint;
  StartIndex: Integer; NumPts: Integer);
{ Use Polyline to connect a set of points on the canvas. If you specify only two
  points, Polyline draws a single line.
  The Points parameter is an array of points to be connected.
  StartIndex identifies the first point in the array to use.
  NumPts indicates the number of points to use. If NumPts is -1 (the default),
  PolyLine uses all the points from StartIndex to the end of the array.
  Calling the MoveTo function with the value of the first point, and then
  repeatedly calling LineTo with all subsequent points will draw the same image
  on the canvas. However, unlike LineTo, Polyline does not change the value of
  PenPos.
}
begin
  if NumPts=0 then exit;
  if StartIndex<low(Points) then exit;
  if StartIndex>=high(Points) then exit;
  if (NumPts<0) or (StartIndex+NumPts-1>high(Points)) then
    NumPts:=High(Points)-StartIndex+1;
  PolyLine(@Points[StartIndex],NumPts);
end;

procedure TAggLCLCanvas.PolyLine(Points: PPoint; NumPts: Integer);
{ Use Polyline to connect a set of points on the canvas. If you specify only two
  points, Polyline draws a single line.
  The Points parameter is an array of points to be connected.
}
var
  i: Integer;
begin
  if NumPts<=1 then exit;
  Path.m_path.remove_all;
  i:=0;
  Path.m_path.move_to(points[i].x+0.5 ,points[i].y+0.5 );
  inc(i);
  while i<NumPts do begin
    Path.m_path.line_to(points[i].x+0.5,points[i].y+0.5);
    inc(i);
  end;
  AggDrawPath(AGG_StrokeOnly);
end;

procedure TAggLCLCanvas.RoundRect(X1, Y1, X2, Y2: Integer; DX, DY: Integer);
{ Draw a filled rectangle but with round edges.
  The edges are like ellipse with Size of RX,RY
}
var
  rx: double;
  ry: double;
  da: double;

  procedure AddArc(cx, cy, startangle, endangle: double);
  var
    a: Double;
  begin
    a:=startangle;
    while a<endangle do begin
      Path.m_path.line_to(cx+rx*cos(a),cy+ry*sin(a));
      a:=a+da;
    end;
  end;

begin
  Path.m_path.remove_all;
  rx:=double(DX)/2;
  ry:=double(DY)/2;
  da:=PI/16;

  Path.m_path.move_to(X1+0.5,Y1+ry+0.5);
  AddArc(X1+rx+0.5,Y1+ry+0.5,PI,PI*1.5);
  AddArc(X2-rx+0.5,Y1+ry+0.5,PI*1.5,PI*2);
  AddArc(X2-rx+0.5,Y2-ry+0.5,0,PI*0.5);
  AddArc(X1+rx+0.5,Y2-ry+0.5,PI*0.5,PI);

  AggClosePolygon;
  AggDrawPath(AGG_FillAndStroke);
end;

procedure TAggLCLCanvas.RoundRect(const aRect: TRect; DX, DY: Integer);
begin
  RoundRect(aRect.Left,aRect.Top,aRect.Right,aRect.Bottom,DX,DY);
end;

procedure TAggLCLCanvas.TextRect(const ARect: TRect; X, Y: integer;
  const Text: string);
var
  OldClipBox: rect_d;
begin
  OldClipBox:=AggGetClipBox;
  TextOut(x,y,Text);
  AggSetClipBox(OldClipBox.x1,OldClipBox.y1,OldClipBox.x2,OldClipBox.y2);
end;

function TAggLCLCanvas.TextExtent(const Text: string): TSize;
begin
  GetTextSize(Text,Result.cx,Result.cy);
end;

function TAggLCLCanvas.TextHeight(const Text: string): Integer;
begin
  Result:=GetTextHeight(Text);
end;

function TAggLCLCanvas.TextWidth(const Text: string): Integer;
begin
  Result:=GetTextWidth(Text);
end;

{ TAggLCLBrush }

procedure TAggLCLBrush.SetColor(const AValue: TColor);
begin
  FPColor:=TColorToFPColor(AValue);
end;

procedure TAggLCLBrush.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FColor:=FPColorToTColor(FPColor);
end;

{ TAggLCLPen }

procedure TAggLCLPen.SetColor(const AValue: TColor);
begin
  FPColor:=TColorToFPColor(AValue);
end;

procedure TAggLCLPen.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FColor:=FPColorToTColor(FPColor);
end;

{ TAggLCLFont }

procedure TAggLCLFont.SetColor(const AValue: TColor);
begin
  FPColor:=TColorToFPColor(AValue);
end;

procedure TAggLCLFont.SetFPColor(const AValue: TFPColor);
begin
  if FPColor=AValue then exit;
  inherited SetFPColor(AValue);
  FColor:=FPColorToTColor(FPColor);
end;

constructor TAggLCLFont.Create;
begin
  FPixelsPerInch := ScreenInfo.PixelsPerInchY;
  inherited Create;
end;

function TAggLCLFont.AggHeightToSize(const h: double): double;
begin
  Result:=h*72 / FPixelsPerInch;
end;

function TAggLCLFont.SizeToAggHeight(const s: double): double;
begin
  Result:=s*FPixelsPerInch / 72;
end;

{$IFDEF LCLGtk2}
procedure TAggLCLFont.LoadViaPango;
var
  FullString: String;
  PangoDesc: PPangoFontDescription;
  PangoLayout: PPangoLayout;
begin
  FullString := Name+' '+IntToStr(Size);
  PangoDesc := pango_font_description_from_string(PChar(FullString));
  if Bold then
    pango_font_description_set_weight(PangoDesc, FW_BOLD);
  if Italic then
    pango_font_description_set_style(PangoDesc, PANGO_STYLE_ITALIC);
  pango_font_description_set_size(PangoDesc, Size*PANGO_SCALE);
  PangoLayout:=gtk_widget_create_pango_layout(GetStyleWidget(lgsdefault), nil);
  pango_layout_set_font_description(PangoLayout,PangoDesc);

end;
{$ENDIF}

{ TAggLCLImage }

procedure TAggLCLImage.ReallocData;
var
  ARawImage: TRawImage;
begin
  inherited ReallocData;

  // create FIntfImg
  if FIntfImg=nil then
    FIntfImg:=TLazIntfImage.Create(0,0);

  FillByte(ARawImage, SizeOf(ARawImage), 0);
  InitAggPasRawImageDescriptor(PixelFormat,Width,Height,ARawImage.Description);
  ARawImage.Data:=Data;
  ARawImage.DataSize:=DataSize;

  FIntfImg.SetRawImage(ARawImage,false);
end;

constructor TAggLCLImage.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
end;

destructor TAggLCLImage.Destroy;
begin
  FreeAndNil(FIntfImg);
  inherited Destroy;
end;

end.

