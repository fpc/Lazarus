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
 Classes ,Graphics, LCLProc, IntfGraphics, GraphType, FPimage, FPCanvas,
 agg_arc, GraphMath, agg_fpimage;

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
    procedure AggTextOut(const x, y: double; str: AnsiString;
               roundOff: boolean=false;
               const ddx: double=0.0; const ddy: double=0.0); override;

    procedure Frame(const ARect: TRect); virtual; // ToDo: border using brush
    procedure Frame(X1,Y1,X2,Y2: Integer);        // ToDo: border using brush

    procedure GradientFill(ARect: TRect; AStart, AStop: TColor; ADirection: TGradientDirection);
  end;

procedure InitAggPasRawImageDescriptor(APixelFormat: TAggFPImgPixelFormat;
                       AWidth, AHeight: cardinal; out Desc: TRawImageDescription);
function AggToLCLColor(const c: TAggColor): TColor;
function LCLToAggColor(const c: TColor): TAggColor;

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
  Result:=RGBToColor(c.r,c.g,c.b);
end;

function LCLToAggColor(const c: TColor): TAggColor;
begin
  Result.a:=0;
  RedGreenBlue(ColorToRGB(c),Result.r,Result.g,Result.b);
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
  cx, cy, rx, ry, start, endangle, h: double;
  ar : agg_arc.arc;
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
  Path.m_path.remove_all;

  ar.Construct(cx ,cy ,rx ,ry ,endangle ,start ,false );

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
  Path.m_path.move_to(X1,Y1);
  Path.m_path.line_to(X2,Y1);
  Path.m_path.line_to(X2,Y2);
  Path.m_path.line_to(X1,Y2);
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
begin
  raise Exception.Create('TAggLCLCanvas.GradientFill ToDo');
  AggFillLinearGradient(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom,
    LCLToAggColor(AStart),LCLToAggColor(AStop));
  FillRect(ARect);
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

