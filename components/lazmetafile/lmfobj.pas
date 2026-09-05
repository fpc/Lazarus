{ Metafile objects, to be used by TlmfImage and TlmfCanvas }

unit lmfObj;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math,
  FPImage, Graphics, //GraphMath,
  LCLType, LCLIntf, LConvEncoding,
  lmf, lmfWMF;

type
  TPointArray = array of TPoint;

  TlmfObject = class(TComponent)
  public
    procedure Action(fImage: TlmfImage; ACanvas:TCanvas); virtual; abstract;
  end;

  TlmfBkColor = class(TlmfObject)
  private
    fColor: TColor;
  public
    constructor Create(AColor: TColor); virtual; reintroduce;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Color: TColor read fColor write fColor;
  end;

  TlmfBkMode = class(TlmfObject)
  private
    fMode: Word;
  public
    constructor Create(AMode: Word); virtual; reintroduce;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Mode: Word read fMode write fMode;
  end;

  TlmfAnchor = class(TlmfObject)
  private
    fPos:TPoint;
  public
    constructor Create(Ax,Ay:integer); virtual; reintroduce;
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
  public
    constructor Create(x, y: integer; const AText: string); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Text: string read fText write fText;
  end;

  TlmfTextInRect = class(TlmfText)
  private
    fRect: TRect;
    fStyle: TTextStyle;
    function GetAlignment: TAlignment;
    function GetClipping: Boolean;
    function GetLayout: TTextLayout;
    function GetOpaque: Boolean;
    function GetSingleLine: Boolean;
    function GetWordBreak: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetClipping(AValue: Boolean);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetOpaque(AValue: Boolean);
    procedure SetSingleLine(AValue: Boolean);
    procedure SetWordBreak(AValue: Boolean);
    procedure ReadTextStyle(Reader: TReader);
    procedure WriteTextStyle(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(const ARect: TRect; x, y: Integer; const AText: String;
      const AStyle: TTextStyle); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    property TextStyle: TTextStyle read fStyle write fStyle;
  published
    property Left: Integer read fRect.Left write fRect.Left;
    property Top: Integer read fRect.Top write fRect.Top;
    property Right: Integer read fRect.Right write fRect.Right;
    property Bottom: Integer read fRect.Bottom write fRect.Bottom;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Clipping: Boolean read GetClipping write SetClipping default false;
    property Layout: TTextLayout read GetLayout write SetLayout default tlTop;
    property Opaque: Boolean read GetOpaque write SetOpaque default false;
    property SingleLine: Boolean read GetSingleLine write SetSingleLine default false;
    property WordBreak: Boolean read GetWordBreak write SetWordBreak default false;
  end;

  TlmfTextColor = class(TlmfObject)
  private
    fColor: TColor;
  public
    constructor Create(AColor: TColor); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Color: TColor read FColor write FColor;
  end;

  TlmfColor = class(TlmfAnchor)
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

  TlmfRect = class(TlmfClip)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas);override;
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

  TlmfFloodFill = class(TlmfAnchor)
  private
    fFillColor: TColor;
    fFillStyle: TFillStyle;
  public
    constructor Create(AX, AY: integer; AFillColor: TColor; AFillStyle: TFillStyle); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property FillColor: TColor read fFillColor write fFillColor;
    property FillStyle: TFillStyle read fFillStyle write fFillStyle;
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

  TlmfEllipse = class(TlmfClip)
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
    fHeight: integer;
    function GetRotation: Integer;
    procedure SetRotation(AValue: Integer);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Font: TFont read fFont write fFont;
    property Height: integer read fHeight write fHeight;
    property Rotation: integer read GetRotation write SetRotation;
  end;

  TlmfBrush=class(TlmfObject)
  private
    fBrush: TBrush;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action({%H-}fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Brush: TBrush read fBrush write fBrush;
  end;

  TlmfPen=class(TlmfObject)
  private
    fPen: TPen;
  public
    constructor Create(AnOwner:TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Pen: TPen read fPen write fPen;
  end;

  TlmfSelectObject = class(TlmfObject)
  private
    fCurrObj: TlmfObject;
  public
    constructor Create(ACurrObj: TlmfObject); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  end;

  TlmfPicture = class(TlmfClip)
  private
    fPicture: TPicture;
    fPixelsPerInch: Integer;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas);override;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
  published
    property Picture: TPicture read fPicture write fPicture;
  end;

  TlmfPolyline=class(TlmfRect)
  private
    pts: TPointArray;
  protected
    procedure StorePoints(AStream:TStream);virtual;
    procedure LoadPoints(AStream:TStream);virtual;
    procedure DefineProperties(Afiler:TFiler);override;
  public
    constructor Create(APoints:PPoint; NumPts:integer); overload;
    destructor Destroy;override;
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
    property Points: TPointArray read pts write pts;
  end;

  TlmfPolygon=class(TlmfPolyline)
  private
    fWinding: boolean;
    fBorderPts: Integer;
  public
    constructor Create(APoints: PPoint; ANumPts: integer; AWinding: boolean = false;
      ABorderPts: Integer = -1); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Winding:boolean read fWinding write fWinding;
  end;


implementation

{ TlmfBkColor (Text background color) }

constructor TlmfBkColor.Create(AColor: TColor);
begin
  inherited Create(nil);
  fColor := AColor;
end;

procedure TlmfBkColor.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  SetBkColor(ACanvas.Handle, fColor);
end;


{ TlmfBkMode (Text background transparent or opaque) }

constructor TlmfBkMode.Create(AMode: Word);
begin
  inherited Create(nil);
  if not (AMode in [TRANSPARENT, OPAQUE]) then
    raise Exception.Create('Illegal BkMode value');
  fMode := AMode;
end;

procedure TlmfBkMode.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  SetBkMode(ACanvas.Handle, fMode);
end;


{ TlmfAnchor }

constructor TlmfAnchor.Create(Ax,Ay:integer);
begin
  inherited Create(nil);
  fPos.X := Ax;
  fPos.Y := Ay;
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

procedure TlmfLine.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Line(
    fImage.ScaleX(fPos.X),
    fImage.ScaleY(fPos.Y),
    fImage.ScaleX(fEndPos.X),
    fImage.ScaleY(fEndPos.Y));
end;


{ TlmfText }

constructor TlmfText.Create(x,y:integer; const AText:string);
begin
  inherited Create(x,y);
  fText:=AText;
end;

procedure TlmfText.Action(fImage:TlmfImage;ACanvas:TCanvas);
{
var
  fnt:TFont;
  ofh:Hfont;
}
begin
{
  if (fRotation<>0) then
  begin
    fnt:=CreateOrtFont(round(fImage.ky*fHeight),fRotation div 10,ACanvas.Font.PixelsPerInch);
    Acanvas.Font.Assign(fnt);
    Acanvas.Font.Name:='Arial';
      // $message 'This is font-selection workaround'
    ofh:=SelectObject(ACanvas.Handle,fnt.Handle);
    ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
    ofh:=SelectObject(ACanvas.Handle,ofh);
    fnt.Free;
  end
  else
  begin
    ACanvas.Font.Height:=round(fImage.ky*fHeight);
    ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
  end;
}
  ACanvas.TextOut(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y),fText);
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
  if fImage.YAxisDown then
    ACanvas.TextRect(R, R.Left, R.Top, fText, fStyle)
  else
    ACanvas.TextRect(R, R.Left, R.Bottom, fText, fStyle);
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

function TlmfTextInRect.GetAlignment: TAlignment;
begin
  Result := fStyle.Alignment;
end;

function TlmfTextInRect.GetClipping: Boolean;
begin
  Result := fStyle.Clipping;
end;

function TlmfTextInRect.GetLayout: TTextLayout;
begin
  Result := fStyle.Layout;
end;

function TlmfTextInRect.GetOpaque: Boolean;
begin
  Result := fStyle.Opaque;
end;

function TlmfTextInRect.GetSingleLine: Boolean;
begin
  Result := fStyle.SingleLine;
end;

function TlmfTextInRect.GetWordBreak: Boolean;
begin
  Result := fStyle.WordBreak;
end;

procedure TlmfTextInRect.SetAlignment(AValue: TAlignment);
begin
  fStyle.Alignment := AValue;
end;

procedure TlmfTextInRect.SetClipping(AValue: Boolean);
begin
  fStyle.Clipping := AValue;
end;

procedure TlmfTextInRect.SetLayout(AValue: TTextLayout);
begin
  fStyle.Layout := AValue;
end;

procedure TlmfTextInRect.SetOpaque(AValue: Boolean);
begin
  fStyle.Opaque := AValue;
end;

procedure TlmfTextInRect.SetSingleLine(AValue: Boolean);
begin
  fStyle.SingleLine := AValue;
end;

procedure TlmfTextInRect.SetWordBreak(AValue: Boolean);
begin
  fStyle.WordBreak := AValue;
end;


{ TlmfTextColor

  Text color normally is included in the font. But WMF has a separate record
  for it. To simplify reading, a TlmfTextColor class has been added. }
constructor TlmfTextColor.Create(AColor: TColor);
begin
  inherited Create(nil);
  FColor := AColor;
end;

procedure TlmfTextColor.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Font.Color := FColor;
end;


{ TlmfColor (pixel mode) }

constructor TlmfColor.Create(x,y:integer; AColor:TfpColor);
begin
  inherited Create(x,y);
  fColor := AColor;
end;

procedure TlmfColor.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Colors[fImage.ScaleX(fpos.x), fImage.ScaleY(fpos.y)] := fColor;
end;


{ TlmfClip (cliprect) }

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


{ TlmfRect (rectangle) }

procedure TlmfRect.Action(fImage:TlmfImage; ACanvas:TCanvas);
begin
  ACanvas.Rectangle(
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
    FImage.ScaleSizeX(frx),
    FImage.ScaleSizeY(fry)
  );
end;


{ TlmfFloodFill }

constructor TlmfFloodFill.Create(AX, AY: Integer; AFillColor: TColor;
  AFillStyle: TFillStyle);
begin
  inherited Create(AX, AY);
  fFillColor := AFillColor;
  fFillStyle := AFillStyle;
end;

procedure TlmfFloodFill.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.FloodFill(fImage.ScaleX(pX), fImage.ScaleY(pY), fFillColor, fFillStyle)
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
  if fImage.YAxisDown then
  begin
    yT := fImage.ScaleY(Top);
    yB := fImage.ScaleY(Bottom);
  end else
  begin
    yT := fImage.ScaleY(Bottom);
    yB := fImage.ScaleY(Top);
  end;
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
var
  ptStart, ptEnd: TPoint;
begin
  if fImage.YAxisDown then begin
    ptStart := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
    ptEnd := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
  end else
  begin
    ptStart := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
    ptEnd := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
  end;
  ACanvas.Arc(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    ptStart.X, ptStart.Y,
    ptEnd.X, ptEnd.Y
  );
end;


{ TlmfChord }

procedure TlmfChord.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  ptStart, ptEnd: TPoint;
begin
  if fImage.YAxisDown then begin
    ptStart := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
    ptEnd := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
  end else
  begin
    ptStart := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
    ptEnd := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
  end;

  ACanvas.Chord(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    ptStart.X, ptStart.Y,
    ptEnd.X, ptEnd.Y
  );
end;


{ TlmfPie }

procedure TlmfPie.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  ptStart, ptEnd: TPoint;
begin
  if fImage.YAxisDown then begin
    ptStart := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
    ptEnd := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
  end else
  begin
    ptStart := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
    ptEnd := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
  end;
  ACanvas.Pie(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    ptStart.X, ptStart.Y,
    ptEnd.X, ptEnd.Y
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
  ht: integer;
begin
  ACanvas.Font.Assign(fFont);
  ht := abs(fImage.ScaleSizeY(fHeight));
  if ht <= 0 then ht := 1;
  ACanvas.Font.Height := -ht;
end;

function TlmfFont.GetRotation: Integer;
begin
  Result := fFont.Orientation;
end;

procedure TlmfFont.SetRotation(AValue: Integer);
begin
  fFont.Orientation := AValue;
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

procedure TlmfBrush.Action(fImage: TlmfImage; ACanvas: TCanvas);
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


{ TlmfSelectObject }

constructor TlmfSelectObject.Create(ACurrObj: TlmfObject);
begin
  inherited Create(nil);
  FCurrObj := ACurrObj;
end;

procedure TlmfSelectObject.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  ht: Integer;
begin
  if FCurrObj is TlmfBrush then
    ACanvas.Brush.Assign(TlmfBrush(FCurrObj).Brush)
  else
  if FCurrObj is TlmfPen then
  begin
    ACanvas.Pen.Assign(TlmfPen(FCurrObj).Pen);
    ACanvas.Pen.Width := fImage.ScaleSizeY(TlmfPen(FCurrObj).Pen.Width);
  end else
  if FCurrObj is TlmfFont then
  begin
    ACanvas.Font.Assign(TlmfFont(FCurrObj).Font);
    ht := abs(fImage.ScaleSizeY(TlmfFont(FCurrObj).Height));
    if ht <= 0 then ht := 1;
    ACanvas.Font.Height := -ht;
  end;
end;


{ TlmfPicture }

constructor TlmfPicture.Create(AnOwner:TComponent);
begin
  inherited Create(AnOwner);
  fPicture := TPicture.Create;
  fPixelsPerInch := 96;  // needs to be updated when image is read
end;

destructor TlmfPicture.Destroy;
begin
  fPicture.Free;
  inherited Destroy;
end;

procedure TlmfPicture.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.StretchDraw(
    Rect(
      fImage.ScaleX(fClip.Left),
      fImage.ScaleY(fClip.Top),
      fImage.ScaleX(fClip.Right),
      fImage.ScaleY(fClip.Bottom)
    ),
    fPicture.Graphic
  );
end;


{ TlmfPolyLine }

constructor TlmfPolyLine.Create(APoints: PPoint; NumPts: integer);
begin
  inherited Create(nil);
  Setlength(pts, numPts);
  System.Move(APoints^, pts[0], NumPts*SizeOf(pts[0]));
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
  len:longint = 0;
begin
  Setlength(pts,0);
  if AStream.Read(len,sizeof(len))=sizeof(len) then
    if len > 0 then
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
  i: Longint;
  npts: array of TPoint = nil;
begin
  SetLength(npts, Length(pts));
  for i:=0 to high(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polyline(npts);
end;


{ TlmfPolygon }

{ Covers also the case of multiple polygons; in this case ABorderPts is the
  number of "real" polygon points without the "retreat" points needed to close
  the overall shape properly.
  See https://wiki.freepascal.org/Developing_with_Graphics#Polygon_with_a_hole
}
constructor TlmfPolygon.Create(APoints: PPoint; ANumPts: integer;
  AWinding: boolean = false; ABorderPts: Integer = -1);
begin
  inherited Create(APoints, ANumPts);
  fWinding := AWinding;
  fBorderPts := ABorderPts;
end;

procedure TlmfPolygon.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  i: longint;
  npts: array of TPoint = nil;
  ps: TPenStyle;
begin
  if fBorderPts > -1 then
  begin
    // Poly-Polygon
    ps := ACanvas.Pen.Style;
    ACanvas.Pen.Style := psClear;
  end;

  Setlength(npts, Length(pts));
  for i:=0 to High(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polygon(npts,fWinding,0,length(npts));

  if fBorderPts > -1 then
  begin
    ACanvas.Pen.Style := ps;
    ACanvas.PolyLine(@pts[0], FBorderPts);
  end;
end;


initialization
  RegisterClasses([TlmfAnchor,
    TlmfMoveTo, TlmfLineTo, TlmfLine,
    TlmfText, TlmfTextInRect,
    TlmfClip, TlmfRect, TlmfRoundRect, TlmfEllipse,
    TlmfArc, TlmfChord, TlmfPie,
    TlmfPicture, TlmfPolyLine, TlmfPolygon,
    TlmfFloodFill, TlmfGradientFill,
    TlmfBkMode, TlmfBkColor, TlmfTextColor, TlmfColor,
    TlmfFont, TlmfBrush, TlmfPen,
    TlmfSelectObject
  ]);

end.

