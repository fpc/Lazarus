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
  TlmfObject = class(TComponent)
  public
    procedure Action(fImage: TlmfImage; ACanvas:TCanvas); virtual; abstract;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); virtual;
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
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfLineTo = class(TlmfAnchor)
  public
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfLine = class(TlmfAnchor)
  private
    fEndPos:TPoint;
  public
    constructor Create(x1,y1,x2,y2:integer);overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  published
    property px1:integer read fEndPos.x write fEndpos.x;
    property py1:integer read fEndPos.y write fEndpos.y;
  end;

  TlmfText = class(TlmfAnchor)
  private
    fText: string;
  public
    constructor Create(x,y:integer; const AText:string);overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas);override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
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
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
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
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfFrameRect = class(TlmfRect)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas);override;
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
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
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

  TlmfEllipse = class(TlmfClip)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfArc = class(TlmfEllipse)
  private
    fStartPt: TPoint;
    fEndPt: TPoint;
  public
    constructor Create(ARect: TRect; AStartPt, AEndPt: TPoint); overload;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  published
    property StartPtX: Integer read fStartPt.X write fStartPt.X;
    property StartPtY: Integer read fStartPt.Y write fStartPt.Y;
    property EndPtX: Integer read fEndPt.X write fEndPt.X;
    property EndPtY: Integer read fEndPt.Y write fEndPt.Y;
  end;

  TlmfChord = class(TlmfArc)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfPie = class(TlmfArc)
  public
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  end;

  TlmfFont=class(TlmfObject)
  private
    fFont: TFont;
    fHeight, fRotation: integer;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  published
    property Font: TFont read fFont write fFont;
    property Height: integer read fHeight write fHeight;
    property Rotation: integer read fRotation write fRotation;
  end;

  TlmfBrush=class(TlmfObject)
  private
    fBrush: TBrush;
  public
    constructor Create(AnOwner:TComponent);override;
    destructor Destroy;override;
    procedure Action({%H-}fImage: TlmfImage; ACanvas: TCanvas);override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream); override;
  published
    property Brush:TBrush read fBrush write fBrush;
  end;

  TlmfPen=class(TlmfObject)
  private
    fPen: TPen;
  public
    constructor Create(AnOwner:TComponent); override;
    destructor Destroy; override;
    procedure Action(fImage: TlmfImage; ACanvas: TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
      AStream: TStream); override;
  published
    property Pen: TPen read fPen write fPen;
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
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
      AStream: TStream); override;
  end;

  TlmfPolygon=class(TlmfPolyline)
  private
    fWinding: boolean;
  public
    constructor Create(Points: PPoint; NumPts: integer; Winding: boolean = false); overload;
    procedure Action(fImage:TlmfImage;ACanvas:TCanvas); override;
    procedure WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
      AStream: TStream); override;
  published
    property Winding:boolean read fWinding write fWinding;
  end;


implementation

{ LMF object }

procedure TlmfObject.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
begin
  // to be overridden by descendants
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

procedure TlmfMoveTo.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  rec: TWMFPointRecord;
begin
  rec.X := AWriter.ScaleX(fPos.X);
  rec.Y := AWriter.ScaleY(fPos.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_MOVETO, rec, SizeOf(TWMFPointRecord));
end;


{ TlmfLineTo }

procedure TlmfLineTo.Action(fImage: TlmfImage; ACanvas: TCanvas);
begin
  ACanvas.LineTo(fImage.ScaleX(fPos.X), fImage.ScaleY(fPos.Y));
end;

procedure TlmfLineTo.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  rec: TWMFPointRecord;
begin
  rec.X := AWriter.ScaleX(fPos.X);
  rec.Y := AWriter.ScaleY(fPos.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_LINETO, rec, SizeOf(TWMFPointRecord));
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

procedure TlmfLine.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  rec: TWMFLineRecord;
begin
  rec.NumPts := 2;
  rec.P1.X := AWriter.ScaleX(fPos.X);
  rec.P1.Y := AWriter.ScaleY(fPos.Y);
  rec.P2.X := AWriter.ScaleX(fEndPos.X);
  rec.P2.Y := AWriter.ScaleY(fEndPos.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_POLYLINE, rec, SizeOf(TWMFLineRecord));
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
  ACanvas.TextOut(fImage.ScaleX(fPos.X),fImage.ScaleY(fPos.Y),fText);
end;

procedure TlmfText.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
// text record
//  - StringLength (word)
//  - String (variable length)
//  - YStart (word)
//  - XStart (word)
var
  ptRec: TWMFPointRecord;
  len: Word;
  s: String;
begin
  if fText = '' then
    exit;

  s := fText;
  len := Length(s);
  if odd(len) then     // String length must be even
  begin
    s := s + #0;
    inc(len);
  end;

  // Record header
  AWriter.WriteWMFRecord(AStream, META_TEXTOUT, len + 3*SizeOf(word));
  // String length
  AWriter.WriteWMFParams(AStream, len, SizeOf(word));
  // String
  AWriter.WriteWMFParams(AStream, s[1], len);
  // String position
  ptRec.X := AWriter.ScaleX(fPos.X);
  ptRec.Y := AWriter.ScaleY(fPos.Y);
  AWriter.WriteWMFParams(AStream, ptRec, SizeOf(TWMFPointRecord));
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

procedure TlmfTextInRect.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  rec: TWMFExtTextOutRecord;
  R: packed array[0..3] of SmallInt;
  strLen, adjLen: Word;
  s: AnsiString;
  n, nR: Integer;
  P: Int64;
begin
  if fText = '' then
    exit;

  s := UTF8ToISO_8859_1(fText);
  strLen := Length(s);
  adjLen := strLen;
  if odd(strLen) then   // String length must be even
  begin
    s := s + #0;
    inc(adjLen);
  end;

  n := SizeOf(TWMFExtTextOutRecord) + adjLen;

  rec := Default(TWMFExtTextOutRecord);
  rec.X := AWriter.ScaleX(fPos.X);
  rec.Y := AWriter.ScaleY(fPos.Y);
  rec.Len := strLen;
  if fStyle.Opaque then rec.Options := rec.Options or ETO_OPAQUE;
  if fStyle.Clipping then rec.Options := rec.Options or ETO_CLIPPED;
  if fStyle.RightToLeft then rec.Options := rec.Options or ETO_RTLREADING;
  if (rec.Options and (ETO_OPAQUE or ETO_CLIPPED) <> 0) then
  begin
    R[0] := AWriter.ScaleX(fRect.Left);
    R[1] := AWriter.ScaleY(fRect.Top);
    R[2] := AWriter.ScaleX(fRect.Right);
    R[3] := AWriter.ScaleY(fRect.Bottom);
    nR := SizeOf(R);
  end else
    nR := 0;
  AWriter.WriteWMFRecord(AStream, META_EXTTEXTOUT, rec, n + nR);
  AStream.Position := AStream.Position - nR - adjLen;
  if nR > 0 then
    AStream.WriteBuffer(R, nR);
  AStream.WriteBuffer(s[1], adjLen);
end;


{ TlmfColor (pixel mode) }

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

procedure TlmfRect.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFRectRecord;
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_RECTANGLE, rec, SizeOf(TWMFRectRecord));
end;


{ TlmfFrameRect (rectangle drawn with brush settings) }

procedure TlmfFrameRect.Action(fImage:TlmfImage; ACanvas:TCanvas);
begin
  ACanvas.FrameRect(
    fImage.ScaleX(fClip.Left),
    fImage.ScaleY(fClip.Top),
    fImage.ScaleX(fClip.Right),
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

procedure TlmfRoundRect.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFRoundRectRecord;
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);
  rec.RX := AWriter.ScaleX(frx);
  rec.RY := AWriter.ScaleY(fry);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_ROUNDRECT, rec, SizeOf(TWMFRoundRectRecord));
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

procedure TlmfEllipse.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFRectRecord;
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_ELLIPSE, rec, SizeOf(TWMFRectRecord));
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

procedure TlmfArc.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFArcRecord;
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);
  rec.XStartArc := AWriter.ScaleX(fStartPt.X);
  rec.YStartArc := AWriter.ScaleY(fStartPt.Y);
  rec.XEndArc := AWriter.ScaleX(fEndPt.X);
  rec.YEndArc := AWriter.ScaleY(fEndPt.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_ARC, rec, SizeOf(TWMFArcRecord));
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

procedure TlmfChord.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFArcRecord;  // same structure for both arc, chord and pie
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);
  rec.XStartArc := AWriter.ScaleX(fStartPt.X);
  rec.YStartArc := AWriter.ScaleY(fStartPt.Y);
  rec.XEndArc := AWriter.ScaleX(fEndPt.X);
  rec.YEndArc := AWriter.ScaleY(fEndPt.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_CHORD, rec, SizeOf(TWMFArcRecord));
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

procedure TlmfPie.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFArcRecord;  // same structure for both arc, chord and pie
begin
  rec.Left := AWriter.ScaleX(fClip.Left);
  rec.Top := AWriter.ScaleY(fClip.Top);
  rec.Right := AWriter.ScaleX(fClip.Right);
  rec.Bottom := AWriter.ScaleY(fClip.Bottom);
  rec.XStartArc := AWriter.ScaleX(fStartPt.X);
  rec.YStartArc := AWriter.ScaleY(fStartPt.Y);
  rec.XEndArc := AWriter.ScaleX(fEndPt.X);
  rec.YEndArc := AWriter.ScaleY(fEndPt.Y);

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_PIE, rec, SizeOf(TWMFArcRecord));
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
  //AFont: TFont;
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

procedure TlmfFont.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
const
  ZERO_OR_ONE: array[boolean] of byte = (0, 1);
var
  rec: TWMFFontRecord;
  colorRec: TWMFColorRecord;
  fntName: String;
  idx, n: Integer;
  idxObj: Word;
begin
  idx := AWriter.FindInObjTable(Self);
  if idx = -1 then
  begin
    rec := Default(TWMFFontRecord);

    fntName := UTF8ToISO_8859_1(fFont.Name) + #0;
    if odd(Length(fntName)) then
      fntName := fntName + #0;
    if Length(fntName) > 32 then begin
      SetLength(fntName, 32);
      fntName[32] := #0;
    end;

    rec.Height := abs(AWriter.ScaleSizeY(fHeight));
    rec.Width := 0;
    rec.Orientation := round(fFont.Orientation * 10);
    rec.Escapement := round(fFont.Orientation * 10); // 0;
      // strange: must use "Escapement" here, not "Orientation".
      // Otherwise MS software will not show the rotated font.
    rec.Weight := IfThen(fsBold in fFont.Style, 700, 400);
    rec.Italic := ZERO_OR_ONE[fsItalic in fFont.Style];
    rec.Underline := ZERO_OR_ONE[fsUnderline in fFont.Style];
    rec.Strikeout := ZERO_OR_ONE[fsStrikeOut in fFont.Style];
    rec.Charset := DEFAULT_CHARSET;
    rec.OutPrecision := 0;  // default
    rec.ClipPrecision := 0; // default
    rec.Quality := 0; // default
    rec.PitchAndFamily := 0;  // don't care / default
    Move(fntName[1], rec.FaceName[0], Length(fntName));
    // Write wmf record
    AWriter.WriteWMFRecord(AStream, META_CREATEFONTINDIRECT, rec, SizeOf(TWMFFontRecord));
    idx := AWriter.AddToObjTable(Self);
  end;
  // Find the (existing or newly created) font in the WMFObjTable and
  // write its index to the SelectObject WMF record:
  idxObj := word(idx);
  AWriter.WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));

  // Write text color
  colorRec.ColorRED := Red(fFont.Color);
  colorRec.ColorGREEN := Green(fFont.Color);
  colorRec.ColorBLUE := Blue(fFont.Color);
  colorRec.Reserved := 0;
  AWriter.WriteWMFRecord(AStream, META_SETTEXTCOLOR, colorRec, SizeOf(TWMFColorRecord));

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

procedure TlmfBrush.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFBrushRecord;
  idx: Integer;
  idxObj: Word;
begin
  idx := AWriter.FindInObjTable(Self);
  if idx = -1 then
  begin
    rec := Default(TWMFBrushRecord);
    case fBrush.Style of
      bsClear      : rec.Style := BS_NULL;
      bsSolid      : rec.Style := BS_SOLID;
      bsHorizontal : begin rec.Style := BS_HATCHED; rec.Hatch := HS_HORIZONTAL; end;
      bsVertical   : begin rec.Style := BS_HATCHED; rec.Hatch := HS_VERTICAL; end;
      bsFDiagonal  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_FDIAGONAL; end;
      bsBDiagonal  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_BDIAGONAL; end;
      bsCross      : begin rec.Style := BS_HATCHED; rec.Hatch := HS_CROSS; end;
      bsDiagCross  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_DIAGCROSS; end;
      else           rec.Style := BS_SOLID;
    end;
    rec.ColorRED := Red(fBrush.Color);
    rec.ColorGREEN := Green(fBrush.Color);
    rec.ColorBLUE := Blue(fBrush.Color);
    rec.Reserved := 0;
    idx := AWriter.AddToObjTable(Self);
    AWriter.WriteWMFRecord(AStream, META_CREATEBRUSHINDIRECT, rec, SizeOf(rec));
  end;
  // Find the (existing or newly created) brush in the WMFObjTable and
  // write its index to the SelectObject WMF record:
  idxObj := word(idx);
  AWriter.WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));
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

procedure TlmfPen.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter; AStream: TStream);
var
  rec: TWMFPenRecord;
  idx: Integer;
  idxObj: Word;
begin
  // Searches the object list for the first usage of this pen and returns its index
  idx := AWriter.FindInObjTable(Self);; //fImage.List.FindPen(fPen);

  // This pen is used here for the first time --> createpen record
  if idx = -1 then //ComponentIndex then
  begin
    case fPen.Style of
      psDash       : rec.Style := PS_DASH;
      psDot        : rec.Style := PS_DOT;
      psDashDot    : rec.Style := PS_DASHDOT;
      psDashDotDot : rec.Style := PS_DASHDOTDOT;
      psClear      : rec.Style := PS_NULL;
      psInsideFrame: rec.Style := PS_INSIDEFRAME;
      else           rec.Style := PS_SOLID;
    end;
    if fPen.Cosmetic then
      rec.Style := rec.Style or PS_COSMETIC;
    case fPen.JoinStyle of
      pjsRound: rec.Style := rec.Style or PS_JOIN_ROUND;
      pjsBevel: rec.Style := rec.Style or PS_JOIN_BEVEL;
      pjsMiter: rec.Style := rec.Style or PS_JOIN_MITER;
    end;
    case fPen.EndCap of
      pecRound: rec.Style := rec.Style or PS_ENDCAP_ROUND;
      pecSquare: rec.Style := rec.Style or PS_ENDCAP_SQUARE;
      pecFlat: rec.Style := rec.Style or PS_ENDCAP_FLAT;
    end;
    rec.Width := AWriter.ScaleSizeX(fPen.Width);
    rec.Ignored1 := 0;
    rec.ColorRED := Red(fPen.Color);
    rec.ColorGREEN := Green(fPen.Color);
    rec.ColorBLUE := Blue(fPen.Color);
    rec.Ignored2 := 0;
    AWriter.WriteWMFRecord(AStream, META_CREATEPENINDIRECT, rec, SizeOf(rec));
    idx := AWriter.AddToObjTable(Self);
  end;
  // Find the (existing or newly created) brush in the WMFObjTable and
  // write its index to the SelectObject WMF record.
  idxObj := word(idx);
  AWriter.WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));
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

procedure TlmfPolyLine.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  numPts: Word;
  recPts: packed array of TWMFPointXYRecord = nil;
  i: Integer;
begin
  numPts := Length(pts);
  SetLength(recPts, numPts);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := AWriter.ScaleX(pts[i].X);
    recPts[i].Y := AWriter.ScaleY(pts[i].Y);
  end;

  // WMF record header + parameters
  AWriter.WriteWMFRecord(AStream, META_POLYLINE, SizeOf(word) + numPts * SizeOf(TWMFPointXYRecord));
  AWriter.WriteWMFParams(AStream, numPts, SizeOf(Word));
  AWriter.WriteWMFParams(AStream, recPts[0], numPts * SizeOf(TWMFPointXYRecord));
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
  i: longint;
  npts: array of TPoint = nil;
begin
  Setlength(npts, Length(pts));
  for i:=0 to High(pts) do
  begin
    npts[i].x:=fImage.ScaleX(pts[i].x);
    npts[i].y:=fImage.ScaleY(pts[i].y);
  end;
  ACanvas.Polygon(npts,fWinding,0,length(npts));
end;

procedure TlmfPolygon.WriteWMFRecord(fImage: TlmfImage; AWriter: TlmfWriter;
  AStream: TStream);
var
  numPts: Word;
  recPts: packed array of TWMFPointXYRecord = nil;
  fillModeRec: TWMFSetPolyFillModeRecord;
  i: Integer;
begin
  numPts := Length(pts);
  SetLength(recPts, numPts);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := AWriter.ScaleX(pts[i].X);
    recPts[i].Y := AWriter.ScaleY(pts[i].Y);
  end;

  fillModeRec.PolyFillMode := IfThen(fWinding, LCLType.WINDING, LCLType.ALTERNATE);
  fillModeRec.Reserved := 0;
  AWriter.WriteWMFRecord(AStream, META_SETPOLYFILLMODE, fillModeRec, SizeOf(TWmfSetPolyFillModeRecord));
  AWriter.WriteWMFRecord(AStream, META_POLYGON, SizeOf(word) + numPts * SizeOf(TWMFPointXYRecord));
  AWriter.WriteWMFParams(AStream, numPts, SizeOf(Word));
  AWriter.WriteWMFParams(AStream, recPts[0], numPts * SizeOf(TWMFPointXYRecord));
end;


initialization
  RegisterClasses([TlmfAnchor,
    TlmfMoveTo, TlmfLineTo, TlmfLine, TlmfText, TlmfTextInRect, TlmfColor,
    TlmfClip, TlmfRect, TlmfRoundRect, TlmfEllipse, TlmfArc, TlmfChord, TlmfPie,
    TlmfGraph, TlmfPolyLine, TlmfPolygon,
    TlmfFont, TlmfBrush, TlmfPen
  ]);

end.

