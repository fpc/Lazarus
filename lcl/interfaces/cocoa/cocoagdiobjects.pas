unit CocoaGDIObjects;
//todo: Remove MacOSAll unit to prevent Carbon framework linking.
//todo: Remove HIShape usage used in TCocoaRegion.

interface

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  MacOSAll, // for CGContextRef
  CocoaAll, CocoaUtils,
  Classes, Types;

type
  { TCocoaGDIObject }

  TCocoaGDIObject = class(TObject)
  public
    RefCount: Integer;
    procedure AddRef;
    procedure Release;
  end;

  TCocoaRegionType = (crt_Empty, crt_Rectangle, crt_Complex);
  TCocoaCombine = (cc_And, cc_Xor, cc_Or, cc_Diff, cc_Copy);

  { TCocoaRegion }

  //todo: Remove HIShape usage. HIShape is legacy
  TCocoaRegion = class(TCocoaGDIObject)
  private
    FShape: HIShapeRef;
  public
    constructor Create;
    constructor Create(const X1, Y1, X2, Y2: Integer);
    constructor Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
    destructor Destroy; override;

    procedure Apply(cg: CGContextRef);
    function GetBounds: TRect;
    function GetType: TCocoaRegionType;
    function ContainsPoint(const P: TPoint): Boolean;
    procedure SetShape(AShape: HIShapeRef);
    function CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): Boolean;
  public
    property Shape: HIShapeRef read FShape write SetShape;
  end;

  { TCocoaBrush }

  TCocoaBrush = class(TCocoaGDIObject)
    R,G,B : Single;
    procedure Apply(cg: CGContextRef);
  end;

  { TCocoaPen }

  TCocoaPen = class(TCocoaGDIObject)
  public
    Style : Integer;
    Width : Integer;
    R,G,B : Single;
    procedure Apply(cg: CGContextRef);
    constructor Create;
  end;

  { TCocoaFont }

  TCocoaFontStyle = set of (cfs_Bold, cfs_Italic, cfs_Underline, cfs_Strikeout);

  TCocoaFont = class(TCocoaGDIObject)
    Name  : AnsiString;
    Size  : Integer;
    Style : TCocoaFontStyle;
    Antialiased: Boolean;
  end;

  { TCocoaBitmap }

  TCocoaBitmap = class(TCocoaGDIObject);

  { TCocoaTextLayout }

  TCocoaTextLayout = class(TObject)
  public
    constructor Create; virtual;
    procedure SetFont(AFont: TCocoaFont); virtual; abstract;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer); virtual; abstract;
    function GetSize: TSize; virtual; abstract;

    procedure Draw(cg: CGContextRef; X, Y: Integer; DX: PInteger); virtual; abstract;
  end;
  TCocoaTextLayoutClass = class of TCocoaTextLayout;

  { TCocoaContext }

  TCocoaContext = class(TObject)
  private
    fText    : TCocoaTextLayout;
    fBrush   : TCocoaBrush;
    fPen     : TCocoaPen;
    fFont    : TCocoaFont;
    fRegion  : TCocoaRegion;
    fBitmap  : TCocoaBitmap;
    procedure SetBitmap(const AValue: TCocoaBitmap);
    procedure SetBrush(const AValue: TCocoaBrush);
    procedure SetFont(const AValue: TCocoaFont);
    procedure SetPen(const AValue: TCocoaPen);
    procedure SetRegion(const AValue: TCocoaRegion);
  protected
    ContextSize : TSize;
  public
    ctx      : NSGraphicsContext;
    PenPos   : TPoint;
    Stack    : Integer;
    TR,TG,TB : Single;
    constructor Create;
    destructor Destroy; override;
    function InitDraw(width, height: Integer): Boolean;
    procedure MoveTo(x,y: Integer);
    procedure LineTo(x,y: Integer);
    procedure Polygon(const Points: array of TPoint; NumPts: Integer; Winding: boolean);
    procedure Polyline(const Points: array of TPoint; NumPts: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure TextOut(X,Y: Integer; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger);
    procedure SetOrigin(X,Y: Integer);
    procedure GetOrigin(var X,Y: Integer);
    function CGContext: CGContextRef; virtual;
    property Brush: TCocoaBrush read fBrush write SetBrush;
    property Pen: TCocoaPen read fPen write SetPen;
    property Font: TCocoaFont read fFont write SetFont;
    property Region: TCocoaRegion read fRegion write SetRegion;
    property Bitmap: TCocoaBitmap read fBitmap write SetBitmap;
  end;

var
  TextLayoutClass  : TCocoaTextLayoutClass = nil;

implementation

{ TCocoaContext }

function TCocoaContext.CGContext:CGContextRef;
begin
  Result:=CGContextRef(ctx.graphicsPort);
end;

procedure TCocoaContext.SetBitmap(const AValue: TCocoaBitmap);
begin
  fBitmap:=AValue;
end;

procedure TCocoaContext.SetBrush(const AValue: TCocoaBrush);
begin
  fBrush:=AValue;
  if Assigned(fBrush) then fBrush.Apply(CGContext);
end;

procedure TCocoaContext.SetFont(const AValue: TCocoaFont);
begin
  fFont:=AValue;
end;

procedure TCocoaContext.SetPen(const AValue: TCocoaPen);
begin
  fPen:=AValue;
  if Assigned(fPen) then fPen.Apply(CGContext);
end;

procedure TCocoaContext.SetRegion(const AValue: TCocoaRegion);
begin
  fRegion:=AValue;
end;

constructor TCocoaContext.Create;
begin
  fText:=TextLayoutClass.Create;
end;

destructor TCocoaContext.Destroy;
begin
  fText.Free;
  inherited Destroy;
end;

function TCocoaContext.InitDraw(width,height:Integer): Boolean;
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  Result:=Assigned(cg);
  if not Result then Exit;

  ContextSize.cx:=width;
  ContextSize.cy:=height;

  CGContextTranslateCTM(cg, 0, height);
  CGContextScaleCTM(cg, 1, -1);
  PenPos.x:=0;
  PenPos.y:=0;
end;

procedure TCocoaContext.MoveTo(x,y:Integer);
begin
  PenPos.x:=x;
  PenPos.y:=y;
end;

procedure TCocoaContext.LineTo(x,y:Integer);
var
  cg  : CGContextRef;
  p   : array [0..1] of CGPoint;
  deltaX, deltaY, absDeltaX, absDeltaY: Integer;
  clipDeltaX, clipDeltaY: Float32;
  tx,ty:Float32;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  deltaX := X - PenPos.x;
  deltaY := Y - PenPos.y;
  if (deltaX=0) and (deltaY=0) then Exit;

  absDeltaX := Abs(deltaX);
  absDeltaY := Abs(deltaY);
  if (absDeltaX<=1) and (absDeltaY<=1) then
  begin
    // special case for 1-pixel lines
    tx := PenPos.x + 0.55;
    ty := PenPos.y + 0.55;
  end
  else
  begin
    // exclude the last pixel from the line
    if absDeltaX > absDeltaY then
    begin
      if deltaX > 0 then clipDeltaX := -1.0 else clipDeltaX := 1.0;
      clipDeltaY := clipDeltaX * deltaY / deltaX;
    end
    else
    begin
      if deltaY > 0 then clipDeltaY := -1.0 else clipDeltaY := 1.0;
      clipDeltaX := clipDeltaY * deltaX / deltaY;
    end;
    tx := X + clipDeltaX + 0.5;
    ty := Y + clipDeltaY + 0.5;
  end;

  p[0].x:=PenPos.X+0.5;
  p[0].y:=PenPos.Y+0.5;
  p[1].x:=tx;
  p[1].y:=ty;

  CGContextBeginPath(cg);
  CGContextAddLines(cg, @p, 2);
  CGContextStrokePath(cg);

  PenPos.x := X;
  PenPos.y := Y;
end;

procedure CGContextAddLCLPoints(cg: CGContextRef; const Points: array of TPoint;NumPts:Integer);
var
  cp  : array of CGPoint;
  i   : Integer;
begin
  SetLength(cp, NumPts);
  for i:=0 to NumPts-1 do begin
    cp[i].x:=Points[i].X+0.5;
    cp[i].y:=Points[i].Y+0.5;
  end;
  CGContextAddLines(cg, @cp[0], NumPts);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; x1, y1, x2, y2: Integer); overload;
var
  r  : CGRect;
begin
  r.origin.x:=x1+0.5;
  r.origin.y:=y1+0.5;
  r.size.width:=x2-x1-1;
  r.size.height:=y2-y1-1;
  CGContextAddRect(cg, r);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; const R: TRect); overload;
begin
  CGContextAddLCLRect(cg, r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TCocoaContext.Polygon(const Points:array of TPoint;NumPts:Integer;
  Winding:boolean);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) or (NumPts<=0) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLPoints(cg, Points, NumPts);
  CGContextClosePath(cg);

  if Winding then
    CGContextDrawPath(cg, kCGPathFillStroke)
  else
    CGContextDrawPath(cg, kCGPathEOFillStroke);
end;

procedure TCocoaContext.Polyline(const Points: array of TPoint; NumPts: Integer);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) or (NumPts<=0) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLPoints(cg, Points, NumPts);
  CGContextStrokePath(cg);
end;

procedure TCocoaContext.Rectangle(X1,Y1,X2,Y2:Integer;FillRect:Boolean; UseBrush: TCocoaBrush);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLRect(cg, X1,Y1,X2,Y2);
  if FillRect then begin
    //using the brush
    if Assigned(UseBrush) then UseBrush.Apply(cg);
    CGContextFillPath(cg);
    //restore the brush
    if Assigned(UseBrush) and Assigned(fBrush) then fBrush.Apply(cg);
  end else
    CGContextStrokePath(cg);
end;

procedure TCocoaContext.Ellipse(X1,Y1,X2,Y2:Integer);
var
  cg : CGContextRef;
  r  : CGRect;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  r.origin.x:=x1+0.5;
  r.origin.y:=y1+0.5;
  r.size.width:=x2-x1-1;
  r.size.height:=y2-y1-1;
  CGContextBeginPath(CGContext);
  CGContextAddEllipseInRect(CGContext, R);
  CGContextDrawPath(CGContext, kCGPathFillStroke);
end;

procedure TCocoaContext.TextOut(X,Y:Integer;UTF8Chars:PChar;Count:Integer;
  CharsDelta:PInteger);
var
  cg      : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  CGContextScaleCTM(cg, 1, -1);
  CGContextTranslateCTM(cg, 0, -ContextSize.cy);

  CGContextSetRGBFillColor(cg, TR, TG, TB, 1);
  fText.SetText(UTF8Chars, Count);
  fText.Draw(cg, X, ContextSize.cy-Y, CharsDelta);

  if Assigned(fBrush) then fBrush.Apply(cg);

  CGContextTranslateCTM(cg, 0, ContextSize.cy);
  CGContextScaleCTM(cg, 1, -1);
end;

procedure TCocoaContext.SetOrigin(X,Y:Integer);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  if Assigned(cg) then CGContextTranslateCTM(cg, X, Y);
end;

procedure TCocoaContext.GetOrigin(var X,Y: Integer);
var
  cg  : CGContextRef;
  t   : CGAffineTransform;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  t:=CGContextGetCTM(cg);
  X := Round(t.tx);
  Y := ContextSize.cy - Round(t.ty);
end;


{ TCocoaRegion }

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create

  Creates a new empty Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create;
begin
  inherited Create;

  FShape := HIShapeCreateEmpty;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create
  Params:  X1, Y1, X2, Y2 - Region bounding rectangle

  Creates a new rectangular Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create(const X1, Y1, X2, Y2: Integer);
begin
  inherited Create;
  FShape := HIShapeCreateWithRect(GetCGRect(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode

  Creates a new polygonal Cocoa region from the specified points
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
var
  Bounds: TRect;
  Context: CGContextRef;
  W, H: Integer;
  Data: Pointer;
  PData: PByte;
  P: PPoint;
  I: Integer;
  X, Y, SX: Integer;
  LC, C: Byte;
  //Line: String;

  function GetPolygonBounds: TRect;
  var
    I: Integer;
  begin
    P := Points;
    Result := Classes.Rect(P^.X, P^.Y, P^.X, P^.Y);
    for I := 1 to NumPts - 1 do
    begin
      Inc(P);
      if P^.X < Result.Left then Result.Left := P^.X;
      if P^.X > Result.Right then Result.Right := P^.X;
      if P^.Y < Result.Top then Result.Top := P^.Y;
      if P^.Y > Result.Bottom then Result.Bottom := P^.Y;
    end;
  end;

  procedure AddPart(X1, X2, Y: Integer);
  var
    R: HIShapeRef;
  begin
    //DebugLn('AddPart:' + DbgS(X1) + ' - ' + DbgS(X2) + ', ' + DbgS(Y));

    R := HIShapeCreateWithRect(GetCGRect(X1, Y, X2, Y + 1));
    HIShapeUnion(FShape, R, FShape);
    CFRelease(R);
  end;

begin
  inherited Create;

(*
  The passed polygon is drawed into grayscale context, the region is constructed
  per rows from rectangles of drawed polygon parts.
  *)

  FShape := HIShapeCreateMutable;

  if (NumPts <= 2) or (Points = nil) then Exit;
  Bounds := GetPolygonBounds;
  W := Bounds.Right - Bounds.Left + 2;
  H := Bounds.Bottom - Bounds.Top + 2;

  if (W <= 0) or (H <= 0) then Exit;

  System.GetMem(Data, W * H);
  System.FillChar(Data^, W * H, 0); // clear bitmap context data to black
  try
    Context := CGBitmapContextCreate(Data, W, H, 8, W, CGColorSpaceCreateDeviceGray,
      kCGImageAlphaNone);
    try
      CGContextSetShouldAntialias(Context, 0); // disable anti-aliasing
      CGContextSetGrayFillColor(Context, 1.0, 1.0); // draw white polygon

      P := Points;
      CGContextBeginPath(Context);
      CGContextMoveToPoint(Context, P^.X, P^.Y);

      for I := 1 to NumPts - 1 do
      begin
        Inc(P);
        CGContextAddLineToPoint(Context, P^.X, P^.Y);
      end;

      CGContextClosePath(Context);

      if isAlter then
        CGContextEOFillPath(Context)
      else
        CGContextFillPath(Context);

      //SetLength(Line, W);

      PData := Data;
      for Y := 0 to Pred(H) do
      begin
        LC := 0; // edge is black
        for X := 0 to Pred(W) do
        begin
          C := PData^;
          //Line[X + 1] := Chr(Ord('0') + C div 255);

          if (C = $FF) and (LC = 0) then
            SX := X; // start of painted row part
          if (C = 0) and (LC = $FF) then
            // end of painted row part (SX, X)
            AddPart(SX, X,  Pred(H) - Y);

          LC := C;
          Inc(PData);
        end;
        //DebugLn(DbgS(Pred(H) - Y) + ':' + Line);
      end;

    finally
      CGContextRelease(Context);
    end;
  finally
    System.FreeMem(Data);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Destroy

  Destroys Cocoa region
 ------------------------------------------------------------------------------}
destructor TCocoaRegion.Destroy;
begin
  CFRelease(FShape);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Apply
  Params:  ADC - Context to apply to

  Applies region to the specified context
  Note: Clipping region is only reducing
 ------------------------------------------------------------------------------}
procedure TCocoaRegion.Apply(cg: CGContextRef);
begin
  if not Assigned(cg) then Exit;
  if HIShapeIsEmpty(FShape) or (HIShapeReplacePathInCGContext(FShape, cg)<>noErr) then
    Exit;
  CGContextClip(cg);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.GetBounds
  Returns: The bounding box of Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.GetBounds: TRect;
var
  R: HIRect;
begin
  if HIShapeGetBounds(FShape, R) = nil then begin
    System.FillChar(Result, sizeof(Result), 0);
    Exit;
  end;

  Result := CGRectToRect(R);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.GetType
  Returns: The type of Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.GetType: TCocoaRegionType;
begin
  if not Assigned(FShape) or HIShapeIsEmpty(FShape) then
    Result := crt_Empty
  else if HIShapeIsRectangular(FShape) then
    Result := crt_Rectangle
  else
    Result := crt_Complex;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.ContainsPoint
  Params:  P - Point
  Returns: If the specified point lies in Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.ContainsPoint(const P: TPoint): Boolean;
var
  cp : CGPoint;
begin
  cp.x:=P.x+0.5;
  cp.y:=P.y+0.5;
  Result := HIShapeContainsPoint(FShape, cp);
end;

procedure TCocoaRegion.SetShape(AShape: HIShapeRef);
begin
  if Assigned(FShape) then CFRelease(FShape);
  FShape := AShape;
end;

function TCocoaRegion.CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): Boolean;
var
  sh1, sh2: HIShapeRef;
const
  MinCoord=-35000;
  MaxSize=65000;
begin
  Result:=Assigned(ARegion);
  if not Assigned(ARegion) then Exit;

  if (CombineMode in [cc_AND, cc_OR, cc_XOR]) and HIShapeIsEmpty(FShape) then
    CombineMode := cc_COPY;

  case CombineMode of
    cc_AND: Shape:=HIShapeCreateIntersection(FShape, ARegion.Shape);
    cc_XOR:
    begin
      sh1 := HIShapeCreateUnion(FShape, ARegion.Shape);
      sh2 := HIShapeCreateIntersection(FShape, ARegion.Shape);
      Shape  := HIShapeCreateDifference(sh1, sh2);
      CFRelease(sh1); CFRelease(sh2);
    end;
    cc_OR:   Shape:=HIShapeCreateUnion(FShape, ARegion.Shape);
    cc_DIFF:
    begin
      if HIShapeIsEmpty(FShape) then
        {HIShapeCreateDifference doesn't work properly if original shape is empty}
        {to simulate "emptieness" very big shape is created }
        Shape:=HIShapeCreateWithRect(GetCGRect(MinCoord,MinCoord,MaxSize,MaxSize)); // create clip nothing.

      Shape:=HIShapeCreateDifference(FShape, ARegion.Shape);
    end;
    cc_COPY: Shape:=HIShapeCreateCopy(ARegion.Shape);
  else
    Result := false;
  end;
end;

{ TCocoaPen }

procedure TCocoaPen.Apply(cg:CGContextRef);
begin
  if not Assigned(cg) then Exit;
  CGContextSetRGBStrokeColor(cg, r, g, b, 1);
  CGContextSetLineWidth(cg, Width);
  //todo: style
end;

constructor TCocoaPen.Create;
begin
  inherited Create;
  Width:=1;
end;

{ TCocoaBrush }

procedure TCocoaBrush.Apply(cg:CGContextRef);
begin
  CGContextSetRGBFillColor(cg, R,G,B, 1);
end;

{ TCocoaTextLayout }

constructor TCocoaTextLayout.Create;
begin
  inherited Create;
end;

{ TCocoaGDIObject }

procedure TCocoaGDIObject.AddRef;
begin
  if RefCount>=0 then inc(RefCount);
end;

procedure TCocoaGDIObject.Release;
begin
  if RefCount>0 then Dec(RefCount)
  else if RefCount=0 then Free;
end;

end.
