unit lmfCanvas;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Types, FPImage, FPCanvas,
  LCLType, LCLIntf, GraphType, GraphMath, IntfGraphics, Graphics,
  lmf;

type
  TlmfCanvas = class(TCanvas)
  private
    fClipRect: TRect;
    fState: TCanvasState;
    fImage: TlmfImage;
  private
    function GetCopyMode: TCopyMode;
    procedure SetCopyMode(AValue: TCopyMode);
  protected
    procedure CreateFont;override;
    procedure CreateBrush;override;
    procedure CreatePen;override;
    function DoCreateDefaultFont : TFPCustomFont; override;
    function DoCreateDefaultPen : TFPCustomPen; override;
    function DoCreateDefaultBrush : TFPCustomBrush; override;
    procedure DoGetTextSize (text:string; var w,h:integer);override;
    function  DoAllowBrush (ABrush : TFPCustomBrush) : boolean; override;
    procedure DoMoveTo(x, y: integer); override;
    procedure DoLineTo(x, y: integer); override;
    procedure DoLine(x1, y1, x2, y2: integer); override;
    //  procedure DoEllipseFill (const Bounds:TRect); override;
    procedure DoEllipse (const Bounds:TRect); override;
//    procedure DoRectangleFill (Const Bounds:TRect); override;
    procedure SetPixel(X,Y: Integer; Value: TColor); override;
    procedure SetColor (x,y:integer; const Value:TFPColor); override;
    function  GetColor (x,y:integer) : TFPColor; override;
    procedure SetClipRect(const AValue: TRect); override;
    function GetClipRect:TRect; override;
    procedure RequiredState(ReqState: TCanvasState); override;

  public
    constructor Create(Almf: TlmfImage);

    procedure Draw(X, Y: Integer; SrcGraphic: TGraphic); override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;

    procedure Ellipse (x1,y1,x2,y2:integer); override; overload;

    procedure FillRect(const ARect: TRect); override; overload;
    procedure Frame(const ARect: TRect); override; overload;
    procedure FrameRect(const ARect: TRect); override; overload;
    procedure Frame3D(var ARect: TRect; TopColor, BottomColor: TColor; const FrameWidth: Integer); overload;
    procedure Frame3D(var ARect: TRect; const FrameWidth: integer; const Style: TGraphicsBevelCut); override; overload;
    procedure Rectangle(X1,Y1,X2,Y2: Integer); override; overload; // already in fpcanvas
    procedure RoundRect(X1, Y1, X2, Y2, Rx, Ry: Integer); override; overload;

    procedure PolyBezier(Points: PPoint; NumPts: Integer; Filled: Boolean = false;
      Continuous: Boolean = true); override;
    procedure PolyLine(Points: PPoint; NumPts: Integer); override;
    procedure Polygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); override; overload;

    procedure FloodFill(x, y: Integer; AFillColor: TColor; AFillStyle: TFillStyle); override;
    procedure GradientFill(ARect: TRect; AStartColor, AEndColor: TColor; ADirection: TGradientDirection); override;

    procedure TextOut(x, y: integer; const AText: string); override; // already in fpcanvas
    function  TextExtent(const Text: string): TSize; override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string; const Style: TTextStyle); override;

    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override; overload;
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override; overload;
    procedure ArcTo(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override; overload;
    procedure Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override; overload;
    procedure Chord(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override; overload;
    procedure Pie(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override;
    procedure RadialPie(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override;

    procedure SetBkColor(AColor: TColor);
    procedure SetBkMode(AMode: Word);  // 1 = TRANSPARENT, 2 = OPAQUE

    property CopyMode: TCopyMode read GetCopyMode write SetCopyMode;
  end;


implementation

uses
  lmfObj;

{  TlmfCanvas }

constructor TlmfCanvas.Create(Almf:TlmfImage);
begin
  fImage:=Almf;
  inherited Create;
end;

procedure TlmfCanvas.RequiredState(ReqState: TCanvasState);
var
  Needed: TCanvasState;
begin
  Needed := ReqState - fState;
  if Needed <> [] then
  begin
    if csHandleValid in Needed then
    begin
      RealizeAntialiasing;
      Include(FState, csHandleValid);
    end;
    if csFontValid in Needed then
      CreateFont;
    if csPenValid in Needed then
    begin
      CreatePen;
      if Pen.Style in [psDash, psDot, psDashDot, psDashDotDot]
        then Include(Needed, csBrushValid);
    end;
    if csBrushValid in Needed then
      CreateBrush;
  end;
end;


// workaround

function TlmfCanvas.DoCreateDefaultFont: TFPCustomFont;
begin
  Result := TFont.Create;
  Result.Name := 'Sans';
  Result.Size := 10;
  TFont(Result).Orientation := 0;
end;

function TlmfCanvas.DoCreateDefaultPen: TFPCustomPen;
begin
  Result:=TPen.Create;
  TPen(Result).Color:=clBlack;
  Tpen(Result).Style:=psSolid;
end;

function TlmfCanvas.DoCreateDefaultBrush: TFPCustomBrush;
begin
  Result:=TBrush.Create;
  Result.Style:=bsClear;
  Tbrush(Result).Color:=clNone;
end;

procedure TlmfCanvas.DoMoveTo(x, y: integer);
var
  item: TlmfMoveTo;
begin
  item := TlmfMoveTo.Create(x,y);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.DoLineTo(x, y: integer);
var
  item: TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLineTo.Create(x,y);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.DoLine(x1, y1, x2, y2: integer);
var
  item:TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLine.Create(x1,y1,x2,y2);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.DoEllipse(const Bounds:TRect);
var
  item:TlmfEllipse;
begin
  RequiredState([csPenValid, csBrushValid]);
  item := TlmfEllipse.Create(Bounds);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.TextOut(x, y: integer; const AText: string);
var
  item: TlmfText;
begin
  RequiredState([csFontValid, csBrushValid]);
  item := TlmfText.Create(x, y, AText);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string;
  const Style: TTextStyle);
var
  item:TlmfText;
begin
  RequiredState([csFontValid,csBrushValid]);
  item:=TlmfTextInRect.Create(ARect, x, y, Text, Style);
  fImage.List.InsertComponent(item);
end;

function UniqueColor(ABitmap: TCustomBitmap): TColor;
var
  img: TLazIntfImage;
  x, y: Integer;
  found: Boolean;
  testColor: TFPColor;
begin
  found := false;
  img := ABitmap.CreateIntfImage;
  try
    repeat
      Result := RGBToColor(Random(255), Random(255), Random(255));;
      testColor := TColorToFPColor(Result);
      found := false;
      for y := 0 to img.Height-1 do
      begin
        for x := 0 to img.Width-1 do
          if img.Colors[x, y] = testColor then
          begin
            found := true;
            break;
          end;
        if found then break;
      end;
    until not found;
  finally
    img.Free;
  end;
end;

procedure TlmfCanvas.Draw(X, Y: Integer; SrcGraphic: TGraphic);
var
  R: TRect;
  ppi: Integer = 96;  // Pixels per inch of SrcGraphic -- to do: should be extracted from file
begin
  R.Left := X;
  R.Top := Y;
  R.Right := round(X + SrcGraphic.Width / ppi * fImage.LogUnitsPerInch);
  R.Bottom := round(Y + SrcGraphic.Height / ppi * fImage.LogUnitsPerInch);
  StretchDraw(R, SrcGraphic);
end;

procedure TlmfCanvas.StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic);
var
  item: TlmfPicture;
  bmp: TBitmap = nil;
begin
  if (SrcGraphic = nil) or SrcGraphic.Empty then
    exit;
  if (SrcGraphic is TCustomBitmap) and (TCustomBitmap(SrcGraphic).PixelFormat = pf32bit) then
  begin
    // Convert to 24 bpp bitmap and switch to mask-transparency because wmf
    // does not support alpha channel.
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Transparent := true;
    bmp.TransparentColor := UniqueColor(TCustomBitmap(SrcGraphic));
    bmp.SetSize(SrcGraphic.Width, SrcGraphic.Height);
    bmp.Canvas.Brush.Color := bmp.TransparentColor;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    bmp.Canvas.Draw(0, 0, SrcGraphic);
  end;

  item := TlmfPicture.Create(nil);
  fImage.List.InsertComponent(item);
  if Assigned(bmp) then
    item.Picture.Assign(bmp)
  else
    item.Picture.Assign(SrcGraphic);
  item.Clip := DestRect;

  bmp.Free;
end;

procedure TlmfCanvas.SetColor(x,y:integer; const Value:TFPColor);
var
  item:TlmfAnchor;
begin
  item := TlmfColor.Create(x,y,Value);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.SetPixel(X,Y: Integer; Value: TColor);
begin
  SetColor(x,y,TColorToFPColor(Value));
end;

function TlmfCanvas.GetColor(x,y:integer): TFPColor;
begin
  Result.Alpha := 0;
  Result.Red := 0;
  Result.Green := 0;
  Result.Blue := 0;
end;

procedure TlmfCanvas.SetCopyMode(AValue: TCopyMode);
var
  item: TlmfObject;
begin
  item := TlmfCopyMode.Create(AValue);
  fImage.List.InsertComponent(item);
  inherited CopyMode := AValue;
end;

function TlmfCanvas.GetCopyMode: TCopyMode;
begin
  Result := inherited CopyMode;
end;

procedure TlmfCanvas.SetClipRect(const AValue: TRect);
var
  item: TlmfObject;
begin
  inherited SetClipRect(AValue);
  fClipRect := AValue;
  item := TlmfClip.Create(AValue);
  fImage.List.InsertComponent(item);
end;

function TlmfCanvas.GetClipRect:TRect;
begin
  Result := fClipRect;
end;

procedure TlmfCanvas.Rectangle(X1,Y1,X2,Y2: Integer);
var
  item:TlmfObject;
begin
  RequiredState([csPenValid, csBrushValid]); // this adds TlmfPen and TlmfBrush
  item := TlmfRect.Create(Rect(x1,y1,x2,y2));
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.RoundRect(X1, Y1, X2, Y2, Rx, Ry: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csPenValid, csBrushValid]);
  item := TlmfRoundRect.Create(Rect(X1, Y1, X2, Y2), Rx, Ry);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.FillRect(const ARect: TRect);
var
  item: TlmfObject;
  ps: TPenStyle;
begin
  ps := Pen.Style;
  Pen.Style := psClear;

  RequiredState([csBrushValid, csPenValid]);
  item := TlmfRect.Create(ARect);
  fImage.List.InsertComponent(item);

  Pen.Style := ps;
end;

{ Draws a frame using the pen settings. }
procedure TlmfCanvas.Frame(const ARect: TRect);
var
  item: TlmfObject;
  bs: TBrushStyle;
begin
  bs := Brush.Style;
  Brush.Style := bsClear;

  RequiredState([csPenValid, csBrushValid]);
  item := TlmfRect.Create(ARect);
  fImage.List.InsertComponent(item);

  Brush.Style := bs;
end;

{ Draws a frame using the brush settings. BUT: This implementation ignores the
  brush style (is always bsSolid, other styles don't make sense anyway) }
procedure TlmfCanvas.FrameRect(const ARect: TRect);
var
  ps: TPenStyle;
  pc: TColor;
begin
  pc := Brush.Color;
  ps := Pen.Style;

  Pen.Color := Brush.Color;
  Pen.Style := psSolid;
  Frame(ARect);

  Pen.Style := ps;
  Pen.Color := pc;
end;

procedure TlmfCanvas.Frame3D(var ARect: TRect; TopColor, BottomColor: TColor;
  const Framewidth: Integer);
var
  item: TlmfObject;
  savedPenStyle: TPenStyle;
  savedBrushStyle: TBrushStyle;
  savedBrushColor: TColor;
  pTL: array[0..6] of TPoint;
  pBR: array[0..6] of TPoint;
begin
  savedPenStyle := Pen.Style;
  savedBrushStyle := Brush.Style;
  savedBrushColor := Brush.Color;

  Brush.Style := bsSolid;
  Pen.Style := psClear;

  // Top/left edges, drawn as polygon
  Brush.Color := ColorToRGB(TopColor);
  pTL[0] := ARect.TopLeft;
  pTL[1] := Point(ARect.Right, ARect.Top);
  pTL[2] := Point(ARect.Right + FrameWidth, ARect.Top - FrameWidth);
  pTL[3] := Point(ARect.Left - FrameWidth, pTL[2].Y);
  pTL[4] := Point(pTL[3].X, ARect.Bottom + FrameWidth);
  pTL[5] := Point(ARect.Left, ARect.Bottom);
  Polygon(@pTL[0], 6);

  // Bottom/right edges, drawn as polygon
  Brush.Color := ColorToRGB(BottomColor);
  pBR[0] := pTL[5];
  pBR[1] := Point(ARect.Right, ARect.Bottom);
  pBR[2] := pTL[1];
  pBR[3] := pTL[2];
  pBR[4] := Point(ARect.Right + FrameWidth, ARect.Bottom + FrameWidth);
  pBR[5] := pTL[4];
  Polygon(@pBR[0], 6);

  Brush.Color := savedBrushColor;
  Brush.Style := savedBrushStyle;
  Pen.Style := savedPenStyle;
end;

procedure TlmfCanvas.Frame3D(var ARect: TRect; const FrameWidth: integer;
  const Style: TGraphicsBevelCut);
begin
  case Style of
    bvNone: ;
    bvLowered: Frame3D(ARect, cl3dShadow, cl3dLight, FrameWidth);
    bvRaised: Frame3D(ARect, cl3dLight, cl3dShadow, FrameWidth);
    bvSpace: ;
  end;
end;

function  TlmfCanvas.DoAllowBrush (ABrush: TFPCustomBrush): boolean;
begin
  Result:=true;
end;

procedure TlmfCanvas.CreateFont;
var
  item:TlmfFont;
begin
  item:=TlmfFont.Create(nil);
  item.Rotation:=TFont(Font).Orientation;
  item.Font.Assign(Font);
  item.Height:=Font.Height;
  item.Rotation:=TFont(item.Font).Orientation;
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.CreateBrush;
var
  item:TlmfBrush;
begin
  item:=TlmfBrush.Create(nil);
  item.Brush.Assign(Brush);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.CreatePen;
var
  item:TlmfPen;
begin
  item:=TlmfPen.Create(nil);
  item.Pen.Assign(Pen);
  fImage.List.InsertComponent(item);
end;

type
  TSafeObject=class(TObject)
    Font:TFont;
    ext:TSize;
    str:string;
    procedure MeasureExtent;
  end;

  procedure TSafeObject.MeasureExtent;
  var
    dc: HDC;
    ofh: HFONT;
  begin
    dc := CreateCompatibleDC(0);
    try
      try
        ofh := SelectObject(dc, Font.Handle);
        GetTextExtentPoint(dc, PChar(str), Length(str), ext);
        SelectObject(dc,ofh);
      finally
        DeleteDC(dc);
      end
    except
    //  writeln('wrong string:',str);
    end;
  end;

function TlmfCanvas.TextExtent(const Text: string): TSize;
var
  so:TSafeObject;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text = '' then
    exit;

  RequiredState([csHandleValid,csFontValid]);
  so:=TSafeObject.Create;
  try
    so.Font:=Self.Font;
    so.Str:=Text;
    TThread.Synchronize(nil, so.MeasureExtent);
    Result:=so.ext;
  finally
    so.Free;
  end;
end;

procedure TlmfCanvas.DoGetTextSize (text:string; var w,h:integer);
var
  sz: TSize;
begin
  sz := TextExtent(Text);
  w := sz.cx;
  h := sz.cy;
end;

procedure TlmfCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: Boolean = false; Continuous: Boolean = true);
var
  item: TlmfPolyBezier;
begin
  Changing;
  if Filled then
    RequiredState([csHandleValid, csPenValid, csBrushValid])
  else
    RequiredState([csHandleValid, csPenValid]);
  item := TlmfPolyBezier.Create(Points, NumPts);
  item.Clip := Self.ClipRect;
  item.Filled := Filled;
  fImage.List.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  item: TlmfPolyLine;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  item := TlmfPolyline.Create(Points,NumPts);
  item.Clip := Self.ClipRect;
  fImage.List.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean = False);
var
  item: TlmfPolygon;
begin
  if NumPts <= 0 then exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  item := TlmfPolygon.Create(Points, NumPts, Winding);
  item.Clip := Self.ClipRect;
  fImage.List.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Ellipse (x1,y1,x2,y2:integer);
begin
  DoEllipse(Rect(x1,y1,x2,y2));
end;

procedure TlmfCanvas.Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csPenValid]);
  item := TlmfArc.Create(Rect(ALeft, ATop, ARight, ABottom), Point(SX, SY), Point(EX, EY));
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
var
  SX: Integer = 0;
  SY: Integer = 0;
  EX: Integer = 0;
  EY: Integer = 0;
begin
  Angles2Coords(
    ALeft, ATop, abs(ARight-ALeft), abs(ABottom-ATop),
    Angle16Deg, Angle16DegLength,
    SX, SY, EX, EY
  );
  Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY);
end;

procedure TlmfCanvas.ArcTo(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csPenValid]);
  item := TlmfArcTo.Create(Rect(ALeft, ATop, ARight, ABottom), Point(SX, SY), Point(EX, EY));
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]);
  item := TlmfChord.Create(Rect(ALeft, ATop, ARight, ABottom), Point(SX, SY), Point(EX, EY));
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.Chord(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
var
  SX: Integer = 0;
  SY: Integer = 0;
  EX: Integer = 0;
  EY: Integer = 0;
begin
  Angles2Coords(
    ALeft, ATop, abs(ARight-ALeft), abs(ABottom-ATop),
    Angle16Deg, Angle16DegLength,
    SX, SY, EX, EY
  );
  Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY);
end;


{ TlmfPie }

procedure TlmfCanvas.Pie(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]);
  item := TlmfPie.Create(Rect(ALeft, ATop, ARight, ABottom), Point(SX, SY), Point(EX, EY));
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.RadialPie(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
var
  SX: Integer = 0;
  SY: Integer = 0;
  EX: Integer = 0;
  EY: Integer = 0;
begin
  Angles2Coords(
    ALeft, ATop, abs(ARight-ALeft), abs(ABottom-ATop),
    Angle16Deg, Angle16DegLength,
    SX, SY, EX, EY
  );
  Pie(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY);
end;

procedure TlmfCanvas.FloodFill(x, y: Integer; AFillColor: TColor; AFillStyle: TFillStyle);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfFloodFill.Create(x, y, AFillColor, AFillStyle);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.GradientFill(ARect: TRect;
  AStartColor, AEndColor: TColor; ADirection: TGradientDirection);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]);
  item := TlmfGradientFill.Create(ARect, AStartColor, AEndColor, ADirection);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.SetBkColor(AColor: TColor);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfBkColor.Create(AColor);
  fImage.List.InsertComponent(item);
end;

procedure TlmfCanvas.SetBkMode(AMode: Word);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfBkMode.Create(AMode);
  fImage.List.InsertComponent(item);
end;


end.

