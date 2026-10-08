{ Metafile objects, to be used by TlmfImage and TlmfCanvas }

unit lmfObj;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, Contnrs,
  FPImage, Graphics, GraphMath,
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
    property Direction: TGradientDirection read fDirection write fDirection;
    property StartColor: TColor read fStartColor write fStartColor;
    property EndColor: TColor read fEndColor write fEndColor;
  end;

  TlmfTriVertexArray = array of TTriVertex;
  TlmfGradientRectArray = array of TGradientRect;
  TlmfGradientTriangleArray = array of TGradientTriangle;
  TlmfIndexArray = array of array[0..2] of Integer;

  TlmfMultiGradientFill = class(TlmfObject)
  private
    fVertices: TlmfTriVertexArray;
    fRectangles: TlmfGradientRectArray;
    fTriangles: TlmfGradientTriangleArray;
    fDirection: Integer;
  protected
    procedure DefineProperties(AFiler: TFiler); override;
    procedure LoadRectangles(AStream: TStream); virtual;
    procedure LoadTriangles(AStream: TStream); virtual;
    procedure LoadVertices(AStream: TStream); virtual;
    procedure StoreRectangles(AStream: TStream); virtual;
    procedure StoreTriangles(AStream: TStream); virtual;
    procedure StoreVertices(AStream: TStream); virtual;
  public
    constructor Create(AFirstVertex: PTriVertex; ANumVertices: Integer;
      AFirstRect: PGradientRect; ANumRects: Integer; ADirection: Integer); overload;
    constructor Create(AFirstVertex: PTriVertex; ANumVertices: Integer;
      AFirstTriangle: PGradientTriangle; ANumTriangles: Integer); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    property Vertices: TlmfTriVertexArray read FVertices write FVertices;
    property Rectangles: TlmfGradientRectArray read fRectangles write fRectangles;
    property Triangles: TlmfGradientTriangleArray read fTriangles write fTriangles;
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

  TlmfArcTo = class(TlmfArc)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
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
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property Pen: TPen read fPen write fPen;
  end;

  TlmfPenMode = class(TlmfObject)
  private
    fMode: TPenMode;
  public
    constructor Create(AMode: TPenMode); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property PenMode: TPenMode read fMode write fMode;
  end;

  TlmfCopyMode = class(tlmfObject)
  private
    fMode: TCopyMode;
  public
    constructor Create(AMode: TCopyMode); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property CopyMode: TCopyMode read fMode write fMode;
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
    fTransparentColor: TColor;
    fSrcRect: TRect;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    property PixelsPerInch: Integer read fPixelsPerInch write fPixelsPerInch;
    property SrcRect: TRect read fSrcRect write fSrcRect;
  published
    property Picture: TPicture read fPicture write fPicture;
    property SrcLeft: Integer read fSrcRect.Left write fSrcRect.Left;
    property SrcTop: Integer read fSrcRect.Top write fSrcRect.Top;
    property SrcRight: Integer read fSrcRect.Right write fSrcRect.Right;
    property SrcBottom: Integer read fSrcRect.Bottom write fSrcRect.Bottom;
    property TransparentColor: TColor read fTransparentColor write fTransparentColor;
  end;

  TlmfBasicPolyLine = class(TlmfRect)
  private
    FPoints: TPointArray;
    FStartsAtPenPos: Boolean;
  protected
    procedure StorePoints(AStream: TStream);virtual;
    procedure LoadPoints(AStream: TStream);virtual;
    procedure DefineProperties(AFiler: TFiler);override;
    property StartsAtPenPos: Boolean read FStartsAtPenPos write FStartsAtPenPos;
  public
    constructor Create(APoints: PPoint; NumPts: integer); overload;
    destructor Destroy; override;
    property Points: TPointArray read FPoints write FPoints;
  end;

  TlmfPolyline = class(TlmfBasicPolyLine)
  public
    procedure Action(fImage:TlmfImage; ACanvas:TCanvas); override;
  published
    property StartsAtPenPos;
  end;

  TlmfPolyBezier = class(TlmfPolyLine)
  private
    fFilled: Boolean;
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property StartsAtPenPos;
    property Filled: Boolean read fFilled write fFilled;
  end;

  TlmfPolygon = class(TlmfBasicPolyline)
  private
    fWinding: boolean;
    fBorderPoints: Integer;
  public
    constructor Create(APoints: PPoint; ANumPts: integer; AWinding: boolean = false;
      ABorderPts: Integer = -1); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
  published
    property BorderPoints: Integer read fBorderPoints write fBorderPoints;
    property Winding: boolean read fWinding write fWinding;
  end;

  TlmfPathItem = class
  end;

  TlmfPathPoint = class(TlmfPathItem)
  private
    FPt: TPoint;
  public
    constructor Create(APt: TPoint);
    property Pt: TPoint read FPt;
  end;

  TlmfPathPoints = class(TlmfPathPoint)
  private
    FPoints: TPointArray;
  public
    constructor Create(APoints: PPoint; ANumPts: Integer);
    property Points: TPointArray read FPoints;
  end;

  TlmfPathMoveTo = class(TlmfPathPoint);
  TlmfPathLineTo = class(TlmfPathPoint);
  TlmfPathPolyBezier = class(TlmfPathPoints);
  TlmfPathPolyBezierTo = class(TlmfPathPoints);
  TlmfPathPolyLine = class(TlmfPathPoints);
  TlmfPathPolyLineTo = class(TlmfPathPoints);
  TlmfPathPolygon = class(TlmfPathPoints);
  TlmfPathEnd = class(TlmfPathItem);
  TlmfPathClose = class(TlmfPathItem);

  TlmfFillStrokeMode = (fsmFill, fsmStroke, fsmFillStroke);

  TlmfPath = class(TlmfClip)
  private
    FList: TFPObjectList;
    FFillStrokeMode: TlmfFillStrokeMode;
    FStartPt: Integer;
    FPolyFillMode: Integer;
  public
    constructor Create(AClip: TRect); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure AddMoveTo(APt: TPoint);
    procedure AddLineTo(APt: TPoint);
    procedure AddPolyBezier(APoints: PPoint; ANumPts: Integer);
    procedure AddPolyBezierTo(APoints: PPoint; ANumPts: Integer);
    procedure AddPolyLine(APoints: PPoint; ANumPts: Integer);
    procedure AddPolyLineTo(APoints: PPoint; ANumPts: Integer);
    procedure AddPolygon(APoints: PPoint; ANumPts: Integer);
    procedure AbortPath;
    procedure BeginPath;
    procedure ClosePath;
    procedure EndPath;
    procedure FlattenPath;
    procedure WidenPath;

    procedure FillPath;
    procedure StrokeAndFillPath;
    procedure StrokePath;
  published
    property FillStrokeMode: TlmfFillStrokeMode read FFillStrokeMode write FFillStrokeMode;
    property PolyFillMode: Integer read FPolyFillMode write FPolyFillMode;
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
  SetBkMode(ACanvas.Handle, fImage.BkMode);
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
  SetBkMode(ACanvas.Handle, fImage.BkMode);
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
  P: TPoint;
begin
  SetBkMode(ACanvas.Handle, fImage.BkMode);
  P := Point(fImage.ScaleX(px), fImage.ScaleY(py));
  if fRect = Rect(0, 0, -1, -1) then
    R := Rect(P.X, P.Y, P.X, P.Y)
  else
    R := Rect(
      fImage.ScaleX(fRect.Left),
      fImage.ScaleY(fRect.Top),
      fImage.ScaleX(fRect.Right),
      fImage.ScaleY(fRect.Bottom)
    );
//  if fImage.YAxisDown then
//    ACanvas.TextRect(R, px, py, fText, fStyle)
//  else
  ACanvas.TextRect(R, P.X, P.Y, fText, fStyle);
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
  SetBkMode(ACanvas.Handle, fImage.BkMode);
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
  SetBkMode(ACanvas.Handle, fImage.BkMode);
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
  fStartColor := AStartColor;
  fEndColor := AEndColor;
  fDirection := ADirection;
end;

procedure TlmfGradientFill.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  R: TRect;
begin
  R.Left := fImage.ScaleX(Left);
  R.Top := fImage.ScaleY(Top);
  R.Right := fImage.ScaleX(Right);
  R.Bottom := fImage.ScaleY(Bottom);
  ACanvas.GradientFill(R, ColorToRGB(fStartColor), ColorToRGB(fEndColor), fDirection);
end;


{ TlmfMultiGradientFill }

constructor TlmfMultiGradientFill.Create(AFirstVertex: PTriVertex; ANumVertices: Integer;
  AFirstRect: PGradientRect; ANumRects: Integer; ADirection: Integer);
var
  i: Integer;
begin
  inherited Create(nil);
  if not (fDirection in [GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V]) then
    raise ElmfReader.CreateFmt('Gradient direction mode %d not supported.', [ADirection]);

  fDirection := ADirection;
  SetLength(fVertices, ANumVertices);
  Move(AFirstVertex^, fVertices[0], SizeOf(TTriVertex) * ANumVertices);

  SetLength(fRectangles, ANumRects);
  Move(AFirstRect^, fRectangles[0], SizeOf(TGradientRect) * ANumRects);
end;

constructor TlmfMultiGradientFill.Create(AFirstVertex: PTriVertex; ANumVertices: Integer;
  AFirstTriangle: PGradientTriangle; ANumTriangles: Integer);
begin
  inherited Create(nil);

  fDirection := GRADIENT_FILL_TRIANGLE;
  SetLength(fVertices, ANumVertices);
  Move(AFirstVertex^, fVertices[0], SizeOf(TTriVertex) * ANumVertices);

  SetLength(fTriangles, ANumTriangles);
  Move(AFirstTriangle^, fTriangles[0], SizeOf(TGradientTriangle) * ANumTriangles);
end;

procedure TlmfMultiGradientFill.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  scaledVertices: array of TTriVertex;
  i: Integer;
begin
  SetLength(scaledVertices, Length(fVertices));
  for i := 0 to High(fVertices) do
  begin
    scaledVertices[i] := fVertices[i];
    scaledVertices[i].X := fImage.ScaleX(fVertices[i].X);
    scaledvertices[i].Y := fImage.ScaleY(fVertices[i].Y);
  end;

  case fDirection of
    GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V:
      GradientFill(ACanvas.Handle,
        @scaledVertices[0], Length(scaledVertices),
        @fRectangles[0], Length(fRectangles),
        fDirection);
    GRADIENT_FILL_TRIANGLE:
      GradientFill(ACanvas.Handle,
        @scaledVertices[0], Length(scaledVertices),
        @fTriangles[0], Length(fTriangles),
        fDirection);
  end;
end;

procedure TlmfMultiGradientFill.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Vertices', @LoadVertices, @StoreVertices, Length(fVertices) > 0);
  AFiler.DefineBinaryProperty('Rectangles', @LoadRectangles, @StoreRectangles, Length(fRectangles) > 0);
  AFiler.DefineBinaryProperty('Triangles', @LoadTriangles, @StoreTriangles, Length(fTriangles) > 0);
end;

procedure TlmfMultiGradientFill.LoadRectangles(AStream: TStream);
var
  len: longint = 0;
begin
  Setlength(fRectangles, 0);
  if AStream.Read(len, SizeOf(len)) = SizeOf(len) then
    if len > 0 then
    begin
      SetLength(fRectangles, len);
      AStream.Read(fRectangles[0], len*SizeOf(fRectangles[0]));
    end;
end;

procedure TlmfMultiGradientFill.LoadTriangles(AStream: TStream);
var
  len: longint = 0;
begin
  Setlength(fTriangles, 0);
  if AStream.Read(len, SizeOf(len)) = SizeOf(len) then
    if len > 0 then
    begin
      SetLength(fTriangles, len);
      AStream.Read(fTriangles[0], len*SizeOf(fTriangles[0]));
    end;
end;

procedure TlmfMultiGradientFill.LoadVertices(AStream: TStream);
var
  len: longint = 0;
begin
  Setlength(fVertices, 0);
  if AStream.Read(len, SizeOf(len)) = SizeOf(len) then
    if len > 0 then
    begin
      SetLength(fVertices, len);
      AStream.Read(fVertices[0], len*SizeOf(fVertices[0]));
    end;
end;

procedure TlmfMultiGradientFill.StoreRectangles(AStream: TStream);
var
  len: longint;
begin
  len := Length(fRectangles);
  AStream.Write(len, sizeof(len));
  if len > 0 then
    AStream.Write(fRectangles[0], len*SizeOf(fRectangles[0]));
end;

procedure TlmfMultiGradientFill.StoreTriangles(AStream: TStream);
var
  len: longint;
begin
  len := Length(fTriangles);
  AStream.Write(len, sizeof(len));
  if len > 0 then
    AStream.Write(fTriangles[0], len*SizeOf(fTriangles[0]));
end;

procedure TlmfMultiGradientFill.StoreVertices(AStream: TStream);
var
  len: longint;
begin
  len := Length(fVertices);
  AStream.Write(len, sizeof(len));
  if len > 0 then
    AStream.Write(fVertices[0], len*SizeOf(fVertices[0]));
end;


{ TlmfEllipse }

procedure TlmfEllipse.Action(fImage:TlmfImage;ACanvas:TCanvas);
begin
  SetBkMode(ACanvas.Handle, fImage.BkMode);
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


{ TlmfArcTo }

procedure TlmfArcTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  ptStart, ptEnd: TPoint;
begin
  ptStart := ACanvas.PenPos;
  if fImage.YAxisDown then begin
    ptStart := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
    ptEnd := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
  end else
  begin
    ptStart := Point(fImage.ScaleX(fEndPt.X), fImage.ScaleY(fEndPt.Y));
    ptEnd := Point(fImage.ScaleX(fStartPt.X), fImage.ScaleY(fStartPt.Y));
  end;
  ACanvas.ArcTo(
    fImage.ScaleX(fClip.Left), fImage.ScaleY(fClip.Top), fImage.ScaleX(fClip.Right), fImage.ScaleY(fClip.Bottom),
    ptStart.X, ptStart.Y,
    ptEnd.X, ptEnd.Y
  );

  ptEnd := ACanvas.PenPos;
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

  SetBkMode(ACanvas.Handle, fImage.BkMode);
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

  SetBkMode(ACanvas.Handle, fImage.BkMode);
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


{ TlmfPenMode }

constructor TlmfPenMode.Create(AMode: TPenMode);
begin
  inherited Create(nil);
  fMode := AMode;
end;

procedure TlmfPenMode.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.Pen.Mode := fMode;
end;


{ TlmfCopyMode }

constructor TlmfCopyMode.Create(AMode: TCopyMode);
begin
  inherited Create(nil);
  fMode := AMode;
end;

procedure TlmfCopyMode.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.CopyMode := fMode;
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
  fTransparentColor := clNone;   // clNone --> ignore
  fSrcRect := Rect(0, 0, -1, -1);  // -1 mean: full size
end;

destructor TlmfPicture.Destroy;
begin
  fPicture.Free;
  inherited Destroy;
end;

procedure TlmfPicture.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  destRect: TRect;
  R: TRect;
  bmpRect: TRect;
  bmp: TBitmap;
begin
  if (fTransparentColor <> clNone) and (FPicture.Bitmap.PixelFormat <> pf32Bit) then
    FPicture.Bitmap.TransparentColor := fTransparentColor;

  destRect := Rect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.ScaleX(fClip.Right),
    fImage.ScaleY(fClip.Bottom)
  );

  if (fSrcRect = Rect(0, 0, -1, -1)) or (fSrcRect = Rect(0, 0, fPicture.Width, fPicture.Height)) then
    ACanvas.StretchDraw(destRect, fPicture.Graphic)
  else
  begin
    bmp := TBitmap.Create;
    try
      bmp.SetSize(abs(fSrcRect.Width), abs(fSrcRect.Height));
      bmp.Canvas.Draw(-fSrcRect.Left, -fSrcRect.Top, fPicture.Bitmap);
      ACanvas.StretchDraw(destRect, bmp);
    finally
      bmp.Free;
    end;
  end;
end;


{ TlmfBasicPolyLine }

constructor TlmfBasicPolyLine.Create(APoints: PPoint; NumPts: integer);
begin
  inherited Create(nil);
  SetLength(fPoints, numPts);
  System.Move(APoints^, fPoints[0], NumPts*SizeOf(fPoints[0]));
end;

destructor TlmfBasicPolyLine.Destroy;
begin
  Setlength(fPoints, 0);
  inherited Destroy;
end;

procedure TlmfBasicPolyLine.StorePoints(AStream: TStream);
var
  len: longint;
begin
  len := Length(fPoints);
  AStream.Write(len, sizeof(len));
  if len > 0 then
    AStream.Write(fPoints[0], len*SizeOf(fPoints[0]));
end;

procedure TlmfBasicPolyLine.LoadPoints(AStream:TStream);
var
  len: longint = 0;
begin
  Setlength(fPoints, 0);
  if AStream.Read(len, SizeOf(len)) = SizeOf(len) then
    if len > 0 then
    begin
      SetLength(fPoints, len);
      AStream.Read(fPoints[0], len*SizeOf(fPoints[0]));
    end;
end;

procedure TlmfBasicPolyLine.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineBinaryProperty('Points', @LoadPoints, @StorePoints, Length(fPoints) > 0);
end;


{ TlmfPolyLine }

procedure TlmfPolyLine.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  i, j: Longint;
  P: TPointArray = nil;
begin
  SetBkMode(ACanvas.Handle, fImage.BkMode);

  if StartsAtPenPos then
  begin
    SetLength(P, Length(Points) + 1);
    P[0] := ACanvas.PenPos;
    j := 1;
  end else
  begin
    SetLength(P, Length(Points));
    j := 0;
  end;
  for i:=0 to High(Points) do
  begin
    P[j].X := fImage.ScaleX(Points[i].x);
    P[j].Y := fImage.ScaleY(Points[i].y);
    inc(j);
  end;
  ACanvas.Polyline(P);
end;


{ TlmfPolyBezier }

procedure TlmfPolyBezier.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  i, j: Longint;
  P: array of TPoint = nil;
begin
  SetBkMode(ACanvas.Handle, fImage.BkMode);

  if FStartsAtPenPos then
  begin
    SetLength(P, Length(Points) + 1);
    P[0] := ACanvas.PenPos;
    j := 1;
  end else
  begin
    SetLength(P, Length(Points));
    j := 0;
  end;
  for i:=0 to high(Points) do
  begin
    P[j].x := fImage.ScaleX(Points[i].x);
    P[j].y := fImage.ScaleY(Points[i].y);
    inc(j);
  end;
  ACanvas.PolyBezier(P, fFilled);
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
  fBorderPoints := ABorderPts;
end;

procedure TlmfPolygon.Action(fImage: TlmfImage; ACanvas: TCanvas);
var
  i: longint;
  P: TPointArray = nil;
  ps: TPenStyle;
begin
  SetBkMode(ACanvas.Handle, fImage.BkMode);

  if fBorderPoints > -1 then
  begin
    // Poly-Polygon: fill only, border will be drawn at end
    ps := ACanvas.Pen.Style;
    ACanvas.Pen.Style := psClear;
  end;

  Setlength(P, Length(Points));
  for i:=0 to High(Points) do
  begin
    P[i].x := fImage.ScaleX(Points[i].x);
    P[i].y := fImage.ScaleY(Points[i].y);
  end;
  ACanvas.Polygon(P, fWinding, 0, Length(P));

  if fBorderPoints > -1 then
  begin
    // Poly-Polygon: draw border
    ACanvas.Pen.Style := ps;
    ACanvas.PolyLine(@P[0], FBorderPoints);
  end;
end;


{ TlmfPathPoint }

constructor TlmfPathPoint.Create(APt: TPoint);
begin
  inherited Create;
  FPt := APt;
end;


{ TlmfPathPoints }

constructor TlmfPathPoints.Create(APoints: PPoint; ANumPts: Integer);
begin
  inherited Create(APoints[0]);
  SetLength(FPoints, ANumPts);
  Move(APoints^, FPoints[0], ANumPts * SizeOf(TPoint));
end;


{ TlmfPath }

constructor TlmfPath.Create(AClip: TRect);
begin
  inherited Create(AClip);
  FList := TFPObjectList.Create;
end;

destructor TlmfPath.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TlmfPath.Action(fImage: TlmfImage; ACanvas: TCanvas);
const
  BLOCK_SIZE = 1024;
var
  pts: TPointArray = nil;
  nPts: Integer = 0;
  i, j, k: Integer;
  item: TObject;
  oldPenStyle: TPenStyle;
  B: TBezier;
  bezPts: PPoint = nil;
  nBezPts: Integer = 0;
  startPt: Integer = 0;
begin
  for i := 0 to FList.Count-1 do
  begin
    item := FList[i];
    { PathEnd }
    if (item is TlmfPathEnd) then
    begin
      SetBkMode(ACanvas.Handle, fImage.BkMode);
      SetLength(pts, nPts);
      case FFillStrokeMode of
        fsmFill:
          begin
            oldPenStyle := ACanvas.Pen.Style;
            ACanvas.Pen.Style := psClear;
            ACanvas.Polygon(pts, FPolyFillMode = WINDING);
            ACanvas.Pen.Style := oldPenStyle;
          end;
        fsmStroke:
          ACanvas.PolyLine(pts);
        fsmFillStroke:
          begin
            ACanvas.Polygon(pts, FPolyFillMode = WINDING);
            ACanvas.PolyLine(pts);
          end;
      end;
      exit;
    end
    else
    { Close Path }  // closes current polygon --> can be called several times per path. Not sure if this is correct...
    if (item is TlmfPathClose) then
    begin
      if Length(pts) mod BLOCK_SIZE = 0 then
        SetLength(pts, Length(pts) + BLOCK_SIZE);
      pts[nPts] := pts[startPt];
      inc(nPts);
    end
    else
    { MoveTo }
    if (item is TlmfPathMoveTo) then
    begin
      if Length(pts) > 0 then
      begin
        SetLength(pts, nPts);
        ACanvas.Polyline(pts);
      end;
      SetLength(pts, BLOCK_SIZE);
      pts[0].X := fImage.ScaleX(TLmfPathMoveTo(item).Pt.X);
      pts[0].Y := fImage.ScaleY(TLmfPathMoveTo(item).Pt.Y);
      ACanvas.MoveTo(pts[0]);
      nPts := 1;
    end
    else
    { LineTo }
    if item is TlmfPathLineTo then
    begin
      if Length(pts) mod BLOCK_SIZE = 0 then
        SetLength(pts, Length(pts) + BLOCK_SIZE);
      pts[nPts].X := fImage.ScaleX(TlmfPathLineTo(item).Pt.X);
      pts[nPts].Y := fImage.ScaleY(TlmfPathLineTo(item).Pt.Y);
      ACanvas.MoveTo(pts[npts]);
      inc(nPts);
    end
    else
    { Polygon, PolyLine, PolyLineTo }
    if (item is TlmfPathPolygon) or (item is TlmfPathPolyLine) or (item is TlmfPathPolyLineTo) then
    begin
      if nPts + Length(TlmfPathPoints(item).Points) >= Length(pts) then
        SetLength(pts, nPts + Length(TlmfPathPoints(item).Points) + 1);  // +1 for start pt of PolyLineTo
      startPt := nPts;
      if (item is TlmfPathPolyLineTo) then
      begin
        pts[npts] := ACanvas.PenPos;
        inc(npts);
      end;
      for j := 0 to Length(TlmfPathPoints(item).Points)-1 do
      begin
        pts[nPts].X := fImage.ScaleX(TlmfPathPoints(item).Points[j].X);
        pts[nPts].Y := fImage.ScaleY(TlmfPathPoints(item).Points[j].Y);
        inc(nPts);
      end;
      // Non-closed shapes must be closed in case of filling modes
      if not (pts[0] = pts[nPts-1]) and (FFillStrokeMode <> fsmStroke) and
        ((item is TlmfPathPolyLine) or (item is TlmfPathPolyLineTo)) then
      begin
        pts[nPts] := pts[0];
        inc(nPts);
        if (item is TlmfPathPolyLineTo) then
          ACanvas.MoveTo(pts[npts-1]);
      end;
    end
    else
    { PolyBezier, PolyBezierTo --> convert to polyline }
    if (item is TlmfPathPolyBezier) or (item is TlmfPathPolyBezierTo) then
    begin
      j := 0;
      if (item is TlmfPathPolyBezierTo) then
      begin
        if (nPts > 0) then
          B[0] := pts[nPts-1]
        else
          B[0] := ACanvas.PenPos;
      end else
      begin
        B[0].X := fImage.ScaleX(TlmfPathPolyBezier(item).Points[0].X);
        B[0].Y := fImage.ScaleY(TlmfPathPolyBezier(item).Points[0].Y);
        j := 1;
      end;
      while (j < Length(TlmfPathPolyBezier(item).Points)) do
      begin
        B[1].X := fImage.ScaleX(TlmfPathPolyBezier(item).Points[j].X);
        B[1].Y := fImage.ScaleY(TlmfPathPolyBezier(item).Points[j].Y);
        B[2].X := fImage.ScaleX(TlmfPathPolyBezier(item).Points[j+1].X);
        B[2].Y := fImage.ScaleY(TlmfPathPolyBezier(item).Points[j+1].Y);
        B[3].X := fImage.ScaleX(TlmfPathPolyBezier(item).Points[j+2].X);
        B[3].Y := fImage.ScaleY(TlmfPathPolyBezier(item).Points[j+2].Y);
        Bezier2PolyLine(B, bezPts, nBezPts);
        if nPts + Length(TlmfPathPolygon(item).Points) >= Length(pts) then
          SetLength(pts, nPts + nBezPts);
        for k := 0 to nBezPts-1 do
        begin
          pts[nPts] := bezPts^;
          inc(bezPts);
          inc(nPts);
        end;
        FreeMem(bezPts, 0);
        bezPts := nil;
        B[0] := B[3];
        inc(j, 3);
      end;
    end;
  end;
end;

procedure TlmfPath.AddLineTo(APt: TPoint);
var
  item: TlmfPathLineTo;
begin
  item := TlmfPathLineTo.Create(APt);
  FList.Add(item);
end;

procedure TlmfPath.AddMoveTo(APt: TPoint);
var
  item: TlmfPathMoveTo;
begin
  item := TlmfPathMoveTo.Create(APt);
  FList.Add(item);
end;

procedure TlmfPath.AddPolyBezier(APoints: PPoint; ANumPts: Integer);
var
  item: TlmfPathPolyBezier;
begin
  item := TlmfPathPolyBezier.Create(APoints, ANumPts);
  FList.Add(item);
end;

procedure TlmfPath.AddPolyBezierTo(APoints: PPoint; ANumPts: Integer);
var
  item: TlmfPathPolyBezierTo;
begin
  item := TlmfPathPolyBezierTo.Create(APoints, ANumPts);
  FList.Add(item);
end;

procedure TlmfPath.AddPolyLine(APoints: PPoint; ANumPts: Integer);
var
  item: TlmfPathPolyLine;
begin
  item := TlmfPathPolyLine.Create(APoints, ANumPts);
  FList.Add(item);
end;

procedure TlmfPath.AddPolyLineTo(APoints: PPoint; ANumPts: Integer);
var
  item: TlmfPathPolyLineTo;
begin
  item := TlmfPathPolyLineTo.Create(APoints, ANumPts);
  FList.Add(item);
end;

procedure TlmfPath.AddPolygon(APoints: PPoint; ANumPts: Integer);
var
  item: TlmfPathPolygon;
begin
  item := TlmfPathPolygon.Create(APoints, ANumPts);
  FList.Add(item);
end;

procedure TlmfPath.AbortPath;
begin
  FList.Clear;
end;

procedure TlmfPath.BeginPath;
begin
  FList.Clear;
end;

procedure TlmfPath.EndPath;
begin
  FList.Add(TlmfPathEnd.Create);
end;

procedure TlmfPath.FlattenPath;
begin
  // Ignored for the moment. Later, when Bezier is implemented, must replace
  // Bezier segments by straight lines.
end;

procedure TlmfPath.ClosePath;
begin
  FList.Add(TlmfPathClose.Create);
end;

procedure TlmfPath.FillPath;
begin
  FFillStrokeMode := fsmFill;
end;

procedure TlmfPath.StrokeAndFillPath;
begin
  FFillStrokeMode := fsmFillStroke;
end;

procedure TlmfPath.StrokePath;
begin
  FFillStrokeMode := fsmStroke;
end;

procedure TlmfPath.WidenPath;
begin
  // to do...
end;

initialization
  RegisterClasses([TlmfAnchor,
    TlmfMoveTo, TlmfLineTo, TlmfLine,
    TlmfText, TlmfTextInRect,
    TlmfClip, TlmfRect, TlmfRoundRect, TlmfEllipse,
    TlmfArc, TlmfArcTo, TlmfChord, TlmfPie,
    TlmfPicture, TlmfPolyLine, TlmfPolygon, TlmfPolyBezier,
    TlmfFloodFill, TlmfGradientFill,
    TlmfBkMode, TlmfBkColor, TlmfTextColor, TlmfColor,
    TlmfFont, TlmfBrush, TlmfPen, TlmfPenMode, TlmfCopyMode,
    TlmfPath,
    TlmfSelectObject
  ]);

end.

