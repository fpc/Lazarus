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

  TCocoaGDIObject = class(TObject);

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

  end;

  { TCocoaPen }

  TCocoaPen = class(TCocoaGDIObject);

  { TCocoaContext }

  TCocoaContext = class(TObject)
  public
    ctx    : NSGraphicsContext;
    PenPos : TPoint;
    function InitDraw(width, height: Integer): Boolean;
    procedure MoveTo(x,y: Integer);
    procedure LineTo(x,y: Integer);
    function CGContext: CGContextRef; virtual;
  end;

implementation

{ TCocoaContext }

function TCocoaContext.CGContext:CGContextRef;
begin
  Result:=CGContextRef(ctx.graphicsPort);
end;

function TCocoaContext.InitDraw(width,height:Integer): Boolean;
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  Result:=Assigned(cg);
  if not Result then Exit;

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

  CGContextAddLines(cg, @p, 2);
  CGContextStrokePath(cg);

  PenPos.x := X;
  PenPos.y := Y;
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


end.
