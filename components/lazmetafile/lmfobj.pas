{ Metafile objects, to be used by TlmfImage and TlmfCanvas }

unit lmfObj;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types,
  FPImage, Graphics,
  LCLType, LCLIntf,
  lmf;

type
  TlmfObject = class(TComponent)
  public
    procedure Action(fImage: TlmfImage; ACanvas:TCanvas); virtual; abstract;
  end;

  TlmfAnchor = class(TlmfObject)
  private
    fPos:TPoint;
  public
    constructor Create(Ax,Ay:integer);virtual;
  published
    property px:integer read fPos.x write fpos.x;
    property py:integer read fPos.y write fpos.y;
  end;

  TlmfMoveTo = class(TlmfAnchor)
  public
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
  end;

  TlmfLineTo = class(TlmfAnchor)
  public
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas); override;
  end;

  TlmfLine = class(TlmfAnchor)
  private
    fEndPos:TPoint;
  public
    constructor Create(x1,y1,x2,y2:integer);overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property px1:integer read fEndPos.x write fEndpos.x;
    property py1:integer read fEndPos.y write fEndpos.y;
  end;

  TlmfText = class(TlmfAnchor)
  private
    fText: string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(x,y:integer; const AText:string);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Text:string read fText write fText;
  end;

  TlmfTextInRect = class(TlmfText)
  private
    fRect: TRect;
    fStyle: TTextStyle;
    procedure ReadTextStyle(Reader: TReader);
    procedure WriteTextStyle(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(const ARect: TRect; x, y: Integer; const AText: String;
      const AStyle: TTextStyle); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    property TextStyle: TTextStyle read fStyle write fStyle;
  end;

  TlmfColor=class(TlmfAnchor)
  private
    fColor: TFPColor;
  public
    constructor Create(x,y:integer; AColor:TfpColor);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property r:word read fColor.red write fColor.red;
    property g:word read fColor.green write fColor.green;
    property b:word read fColor.blue write fColor.blue;
    property a:word read fColor.alpha write fColor.alpha;
  end;

  TlmfClip = class(TlmfObject)
  private
    fClip:TRect;
  public
    constructor Create(AClip: TRect); virtual; overload;
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
    property Clip: TRect read FClip write fClip;
  published
    property Left:integer read fClip.Left write fClip.Left;
    property Top:integer read fClip.Top write fClip.Top;
    property Right:integer read fClip.Right write fClip.Right;
    property Bottom:integer read fClip.Bottom write fClip.Bottom;
  end;

  TlmfRect=class(TlmfClip)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas);override;
  end;

  TlmfFillRect=class(TlmfRect)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfFrame = class(TlmfRect)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfFrameRect = class(TlmfRect)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfFrame3D = class(TlmfRect)
  private
    fTopColor: TColor;
    fBottomColor: TColor;
    fFrameWidth: Integer;
  public
    constructor Create(ARect:TRect; ATopColor, ABottomColor: TColor; AFrameWidth: Integer); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property TopColor: TColor read fTopColor write fTopColor;
    property BottomColor: TColor read fBottomColor write fBottomColor;
    property FrameWidth: Integer read fFrameWidth write fFrameWidth;
  end;

  TlmfRoundRect = class(TlmfRect)
  private
    frx, fry: Integer;
  public
    constructor Create(ARect: TRect; ARx, ARy: Integer); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Rx: Integer read frx write frx;
    property Ry: Integer read fry write fry;
  end;

  TlmfGradientFill = class(TlmfClip)
  private
    fStartColor: TColor;
    fEndColor: TColor;
    fDirection: TGradientDirection;
  public
    constructor Create(ARect: TRect; AStartColor, AEndColor: TColor; ADirection: TGradientDirection); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property StartColor: TColor read fStartColor write fStartColor;
    property EndColor: TColor read fEndColor write fEndColor;
    property Direction: TGradientDirection read fDirection write fDirection;
  end;

  TlmfEllipse=class(TlmfClip)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfArc = class(TlmfEllipse)
  private
    fStartPt: TPoint;
    fEndPt: TPoint;
  public
    constructor Create(ARect: TRect; AStartPt, AEndPt: TPoint); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property StartPtX: Integer read fStartPt.X write fStartPt.X;
    property StartPtY: Integer read fStartPt.Y write fStartPt.Y;
    property EndPtX: Integer read fEndPt.X write fEndPt.X;
    property EndPtY: Integer read fEndPt.Y write fEndPt.Y;
  end;

  TlmfChord = class(TlmfArc)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfPie = class(TlmfArc)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfFont=class(TlmfObject)
  private
    fFont: TFont;
    fHeight, fRotation: integer;
//    fName:string;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Font:TFont read fFont write fFont;
    property Height:integer read fHeight write fHeight;
    property Rotation:integer read fRotation write fRotation;
  end;

  TlmfBrush=class(TlmfObject)
  private
    fBrush:TBrush;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Brush:TBrush read fBrush write fBrush;
  end;

  TlmfPen=class(TlmfObject)
  private
    fPen:TPen;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Pen:TPen read fPen write fPen;
  end;

  TlmfGraph=class(TlmfClip)
  private
    fGraph:TPicture;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
  published
    property Graph:TPicture read fGraph write fGraph;
  end;

  TlmfPolyline=class(TlmfRect)
  private
    pts:array of TPoint;
  protected
    procedure StorePoints(AStream:TStream);virtual;
    procedure LoadPoints(AStream:TStream);virtual;
    procedure DefineProperties(Afiler:TFiler);override;
  public
    constructor Create(Points:PPoint; NumPts:integer); overload;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
  end;

  TlmfPolygon=class(TlmfPolyline)
  private
    fWinding:boolean;
  public
    constructor Create(Points: PPoint; NumPts: integer; Winding: boolean = false); overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas); override;
  published
    property Winding:boolean read fWinding write fWinding;
  end;


implementation

{ LMF object }

{ TlmfAnchor }

constructor TlmfAnchor.Create(Ax,Ay:integer);
begin
  inherited Create(nil);
  fPos.X:=Ax;
  fPos.Y:=Ay;
end;


{ TlmfMoveTo}

procedure TlmfMoveTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.MoveTo(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y));
end;


{ TlmfLineTo }

procedure TlmfLineTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.LineTo(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y));
end;


{ TlmfLine }

constructor TlmfLine.Create(x1,y1,x2,y2:integer);
begin
  inherited Create(x1,y1);
  fEndPos.X:=x2;
  fEndPos.Y:=y2;
end;

procedure TlmfLine.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Line(
    fImage.ScaleX(fPos.X),
    fImage.ScaleY(fPos.Y),
    fImage.ScaleX(fEndPos.X),
    fImage.ScaleY(fEndPos.Y));
end;


constructor TlmfText.Create(x,y:integer; const AText:string);
begin
  inherited Create(x,y);
  fText:=AText;
end;

procedure TlmfText.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  fnt:TFont;
  ofh:Hfont;
begin
(*	if (fRotation<>0) then
  begin
  	fnt:=CreateOrtFont(round(fImage.ky*fHeight),fRotation div 10,ACanvas.Font.PixelsPerInch);
    Acanvas.Font.Assign(fnt);
    Acanvas.Font.Name:='Arial';
	  {$message 'This is font-selection workaround'}
  	ofh:=SelectObject(ACanvas.Handle,fnt.Handle);
    ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
    ofh:=SelectObject(ACanvas.Handle,ofh);
    fnt.Free;
  end
  else
  begin
	  ACanvas.Font.Height:=round(fImage.ky*fHeight);
  	ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
  end;*)
  ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
end;

procedure TlmfText.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;


{ TlmfTextInRect }

constructor TlmfTextInRect.Create(const ARect: TRect; x, y: Integer;
  const AText: String; const AStyle: TTextStyle);
begin
  inherited Create(x, y, AText);
  fRect := ARect;
  fStyle := AStyle;
end;

procedure TlmfTextInRect.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  R: TRect;
begin
  R := Rect(
    fImage.ScaleX(fRect.Left),
    fImage.ScaleY(fRect.Top),
    fImage.ScaleX(fRect.Right),
    fImage.ScaleY(fRect.bottom)
  );
  ACanvas.TextRect(R, fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y), fText, fStyle);
end;

procedure TlmfTextInRect.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('TextStyle', @ReadTextStyle, @WriteTextStyle, true);
end;

procedure TlmfTextInRect.ReadTextStyle(Reader: TReader);
begin
  Reader.Read(fStyle, SizeOf(fStyle));
end;

procedure TlmfTextInRect.WriteTextStyle(Writer: TWriter);
begin
  Writer.Write(fStyle, SizeOf(fStyle));
end;


// pixel mode
constructor TlmfColor.Create(x,y:integer; AColor:TfpColor);
begin
  inherited Create(x,y);
  fColor:=AColor;
end;

procedure TlmfColor.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Colors[fImage.ScaleX(fpos.x), fImage.ScaleY(fpos.y)] := fColor;
end;

// cliprect
constructor TlmfClip.Create(AClip:TRect);
begin
  inherited Create(nil);
  fClip:=AClip;
end;

procedure TlmfClip.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  newClip:TRect;
begin
  // reset the clipping
  if (fClip.Left=0) and (fClip.Top=0) and (fClip.Right=MaxInt) and (fClip.Bottom=MaxInt) then
  begin
    // this clip rect have not to scale
    ACanvas.ClipRect:=fClip; // actually does clipping through virtualization
    SelectClipRgn(ACanvas.Handle,0)
  end
  else
  begin
    newClip:=Rect(
      fImage.ScaleX(fClip.Left),
      fImage.ScaleY(fClip.Top),
      fImage.Scalex(fClip.Right),
      fImage.ScaleY(fClip.Bottom)
    );

    ACanvas.ClipRect:=newClip; // actually does nothing

    // this is real clipping
    lclintf.IntersectClipRect(ACanvas.Handle,
    	newClip.Left,newClip.Top,newClip.Right,newClip.Bottom);
  end;
end;

// rectangle
procedure TlmfRect.Action(fImage:TlmfImage; ACanvas:TCanvas);
begin
 // ACanvas.Brush.Style:=bsClear;
  ACanvas.Rectangle(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;

procedure TlmfFillRect.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.FillRect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;


{ TlmfFrame }

procedure TlmfFrame.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Frame(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;


{ TlmfFrame }

procedure TlmfFrameRect.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.FrameRect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;


{ TlmfRoundRect }

constructor TlmfRoundRect.Create(ARect: TRect; ARx, ARy: Integer);
begin
  inherited Create(ARect);
  frx := ARx;
  fry := ARy;
end;

procedure TlmfRoundRect.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.RoundRect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.ScaleX(fClip.Right),
    fImage.ScaleY(fClip.Bottom),
    frx, fry
  );
end;


{ TlmfFrame3d }

constructor TlmfFrame3d.Create(ARect: TRect; ATopColor, ABottomColor: TColor;
  AFrameWidth: Integer);
begin
  inherited Create(ARect);
  fTopColor := ATopColor;
  fBottomColor := ABottomColor;
  fFrameWidth := AFrameWidth;
end;

procedure TlmfFrame3d.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  xL, xR, yT, yB: Integer;
  W, wFrame, i : Integer;
begin
  xL := fImage.ScaleX(Left);
  xR := fImage.ScaleX(Right);
  yT := fImage.ScaleY(Top);
  yB := fImage.ScaleY(Bottom);

  if yB - yT > xR - xL then
  begin
    W := xR - xL + 1;
    wFrame := fImage.ScaleSizeX(fFrameWidth);
  end else
  begin
    W := yB - yT + 1;
    wFrame := fImage.ScaleSizeY(fFrameWidth);
  end;

  if wFrame > W then
    W := W-1
  else
    W := wFrame;

  for i := 1 to W do
  begin
    ACanvas.Pen.Color := fTopColor;
    ACanvas.MoveTo(xL, yB-1);
    ACanvas.LineTo(xL, yT);
    ACanvas.LineTo(xR-1, yT);

    ACanvas.Pen.Color := fBottomColor;
    ACanvas.LineTo(xR-1, yB-1);
    ACanvas.LineTo(xL, yB-1);

    inc(xL);
    inc(yT);
    dec(xR);
    dec(yB);
  end;
end;


{ TlmfGradientFill }

constructor TlmfGradientFill.Create(ARect: TRect; AStartColor, AEndColor: TColor;
  ADirection: TGradientDirection);
begin
  inherited Create(ARect);
  fStartColor := ColorToRGB(AStartColor);
  fEndColor := ColorToRGB(AEndColor);
  fDirection := ADirection;
end;

procedure TlmfGradientFill.Action(fImage: TlmfImage; ACanvas: TCanvas);

  function InterpolateColor(C1, C2: TColor; x, Total: Integer): TColor;
  var
    f1, f2: Double;
  begin
    f2 := x / Total;
    f1 := 1.0 - f2;
    TRgbQuad(Result).rgbRed := round(TRgbQuad(C1).rgbRed * f1 + TRgbQuad(C2).rgbRed * f2);
    TRgbQuad(Result).rgbGreen := round(TRgbQuad(C1).rgbGreen * f1 + TRgbQuad(C2).rgbGreen * f2);
    TRgbQuad(Result).rgbBlue := round(TRgbQuad(C1).rgbBlue * f1 + TRgbQuad(C2).rgbBlue * f2);
    TRgbQuad(Result).rgbReserved := round(TRgbQuad(C1).rgbReserved * f1 + TRgbQuad(C2).rgbReserved * f2);
  end;

var
  x, y, i, n: Integer;
  xL, xR, yT, yB: Integer;
  oldPenStyle: TPenStyle;
  oldPenWidth: Integer;
  oldPenColor: TColor;
begin
  oldPenStyle := ACanvas.Pen.Style;
  oldPenWidth := ACanvas.Pen.Width;
  oldPenColor := ACanvas.Pen.Color;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;

  xL := fImage.ScaleX(Left);
  xR := fImage.ScaleX(Right);
  yT := fImage.ScaleY(Top);
  yB := fImage.ScaleY(Bottom);
  if fDirection = gdVertical then
  begin
    n := yB - yT;
    if n = 0 then
      exit;
    i := 0;
    for y := yT to yB - 1 do
    begin
      ACanvas.Pen.Color := InterpolateColor(fStartColor, fEndColor, i, n);
      ACanvas.Line(xL, y, xR - 1, y);
      inc(i);
    end;
  end else
  begin
    n := xR - xL;
    if n = 0 then
      exit;
    i := 0;
    for x := xL to xR - 1 do
    begin
      ACanvas.Pen.Color := InterpolateColor(fStartColor, fEndColor, i, n);
      ACanvas.Line(x, yT, x, yB - 1);
      inc(i);
    end;
  end;

  ACanvas.Pen.Style := oldPenStyle;
  ACanvas.Pen.Width := oldPenWidth;
  ACanvas.Pen.Color := oldPenColor;
end;


{ TlmfEllipse }

procedure TlmfEllipse.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Ellipse(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.Scalex(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );
end;


{ TlmfArc }

constructor TlmfArc.Create(ARect: TRect; AStartPt, AEndPt: TPoint);
begin
  inherited Create(ARect);
  fStartPt := AStartPt;
  fEndPt := AEndPt;
end;

procedure TlmfArc.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Arc(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y),
    fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y)
  );
end;


{ TlmfChord }

procedure TlmfChord.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Chord(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y),
    fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y)
  );
end;


{ TlmfPie }

procedure TlmfPie.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Pie(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y),
    fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y)
  );
end;


{ TlmfFont }

constructor TlmfFont.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fFont := TFont.Create;
end;

destructor TlmfFont.Destroy;
begin
  fFont.Free;
  inherited Destroy;
end;

procedure TlmfFont.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  AFont: TFont;
  rot, ht: integer;
//  ofh: Hfont;
begin
  rot:=fRotation;//TRotFont(fFont).Rotation;

  Acanvas.Font.Assign(fFont);
  ht := abs(fImage.ScaleSizeY(fHeight));
  if ht <= 0 then ht := 1;
  ACanvas.Font.Height := -ht;
  ACanvas.Font.Orientation := rot;
end;


{ TlmfBrush }

constructor TlmfBrush.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fBrush := TBrush.Create;
end;

destructor TlmfBrush.Destroy;
begin
  fBrush.Free;
  inherited Destroy;
end;

procedure TlmfBrush.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.Brush.Assign(fBrush);
end;


{ TlmfPen }

constructor TlmfPen.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fPen := TPen.Create;
end;

destructor TlmfPen.Destroy;
begin
  fPen.Free;
  inherited Destroy;
end;

procedure TlmfPen.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(fPen);
  ACanvas.Pen.Width := fImage.ScaleSizeY(fPen.Width);
end;


{ TlmfGraph }

constructor TlmfGraph.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fGraph := TPicture.Create;
end;

destructor TlmfGraph.Destroy;
begin
  fGraph.Free;
  inherited Destroy;
end;

procedure TlmfGraph.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  ACanvas.StretchDraw(
    Rect(
      fImage.ScaleX(fClip.Left),
      fImage.ScaleY(fClip.Top),
      fImage.ScaleX(fClip.Right),
      fImage.ScaleY(fClip.Bottom)
    ),
    fGraph.Graphic
  );
end;


{ TlmfPolyLine }

constructor TlmfPolyLine.Create(Points:PPoint;NumPts:integer);
begin
  inherited Create(nil);
  Setlength(pts,numPts);
  System.Move(Points^,pts[0],NumPts*sizeof(pts[0]));
end;

destructor TlmfPolyLine.Destroy;
begin
  Setlength(pts,0);
  inherited Destroy;
end;

procedure TlmfPolyLine.StorePoints(AStream:TStream);
var
  len:longint;
begin
  len:=length(pts);
  AStream.Write(len,sizeof(len));
  if len>0 then
    AStream.Write(pts[0],len*sizeof(pts[0]));
end;

procedure TlmfPolyLine.LoadPoints(AStream:TStream);
var
  len:longint;
begin
  Setlength(pts,0);
  if AStream.Read(len,sizeof(len))=sizeof(len) then
    if len>0 then
    begin
      setlength(pts,len);
      AStream.Read(pts[0],len*sizeof(pts[0]));
    end;
end;

procedure TlmfPolyLine.DefineProperties(Afiler:TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Points', @LoadPoints, @StorePoints, Length(pts) > 0);
end;

procedure TlmfPolyLine.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  i:longint;
  npts:array of TPoint;
begin
  setlength(npts,length(pts));
  for i:=0 to high(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polyline(npts);
end;


{ TlmfPolygon }

constructor TlmfPolygon.Create(Points: PPoint; NumPts: integer;
  Winding: boolean = false);
begin
  inherited Create(Points, NumPts);
  fWinding := Winding;
end;

procedure TlmfPolygon.Action(fImage:TlmfImage;ACanvas:TCanvas);
var
  i:longint;
  npts:array of TPoint;
begin
  Setlength(npts, Length(pts));
  for i:=0 to High(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polygon(npts,fWinding,0,length(npts));
end;


initialization
  RegisterClasses([TlmfAnchor,
    TlmfMoveTo, TlmfLineTo, TlmfLine, TlmfText, TlmfTextInRect, TlmfColor,
    TlmfClip, TlmfRect, TlmfFillRect, TlmfFrame, TLmfFrameRect, TlmfRoundRect,
    TlmfEllipse, TlmfArc, TlmfChord, TlmfPie,
    TlmfGraph, TlmfPolyLine, TlmfPolygon,
    TlmfFont, TlmfBrush, TlmfPen
  ]);

end.

