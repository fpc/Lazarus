unit lmf experimental;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Types,
  LCLType, LCLIntf,
  FPCanvas, FPImage, GraphMath, GraphType, IntfGraphics, Graphics, syncobjs;

const
  PTS_PER_INCH = 72;          // 1 pt = 1/72 inch
  TWIPS_PER_INCH = 1440;      // 1 twip = 1/20 pt = 1/1440 inch

type
  // Exception types
  ElmfImage = class(Exception);
  ElmfReader = class(ElmfImage);
  ElmfWriter = class(ElmfImage);

  // forward declarations
  TlmfImage = class;
  TlmfList = class;

  // abstract reader/writer classes
  TlmfReader = class
  public
    procedure ReadFromStream(AStream: TStream; AImage: TlmfImage); virtual; abstract;
  end;

  TlmfWriter = class
  public
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); virtual; abstract;
  end;

  { TlmfImage }

  TlmfImage = class(TGraphic)
  private
    forgX,forgY,
    fWidth,fHeight:integer;
    kx,ky:double;
    fList: TlmfList;
    fCrs:TCriticalSection;
    fEnhanced: Boolean;
  private
    function GetLogUnitsPerInch: Integer;
    procedure SetLogUnitsPerInch(AValue: Integer);
  protected
    procedure AssignTo(Dest:TPersistent);override;
    function GetWidth:integer;override;
    procedure SetWidth(AVal:integer);override;
    function GetHeight:integer;override;
    procedure SetHeight(AVal:integer);override;
    function GetEmpty:boolean;override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
      //procedure Erase;override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function ScaleSizeX(ax: Integer): Integer;
    function ScaleSizeY(ay: Integer): Integer;
    function ScaleX(ax: Integer): Integer;
    function ScaleY(ay:Integer): Integer;

    procedure SaveToLMFFile(AFileName: String);
    procedure SaveToLMFStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromLMFFile(AFileName: String);
    procedure LoadFromLMFStream(AStream: TStream; IsEnhanced: Boolean);
    procedure LoadFromStream(Stream: TStream); override;

    property Enhanced: Boolean read FEnhanced write FEnhanced;  // Write WMF or EMF stream
    property List: TlmfList read fList;

    property LogUnitsPerInch: Integer read GetLogUnitsPerInch write SetLogUnitsPerInch;
  end;

  TlmfList = class(TComponent)
  private
    fWidth, fHeight:integer;
    fLogUnitsPerInch: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; {%H-}Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property Width:integer read fWidth write fWidth;
    property Height:integer read fHeight write fHeight;
    property LogUnitsPerInch: Integer read FLogUnitsPerInch write FLogUnitsPerInch;
  end;

  TlmfCanvas = class(TCanvas)
  private
    fClipRect: TRect;
    fState: TCanvasState;
    fImage: TlmfImage;
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

    procedure Polyline(Points: PPoint; NumPts: Integer);override;
    procedure Polygon(Points: PPoint; NumPts: Integer;  Winding: boolean = False); override;

    procedure GradientFill(const ARect: TRect; AStartColor, AEndColor: TColor; ADirection: TGradientDirection);

    procedure TextOut(x, y: integer; const AText: string); override; // already in fpcanvas
    function  TextExtent(const Text: string): TSize; override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string; const Style: TTextStyle); override;

    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override; overload;
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override; overload;
    procedure Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override; overload;
    procedure Chord(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override; overload;
    procedure Pie(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); override;
    procedure RadialPie(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override;

    procedure SetBkColor(AColor: TColor);
    procedure SetBkMode(AMode: Word);  // 1 = TRANSPARENT, 2 = OPAQUE
  end;


implementation

uses
  lmfObj, lmfWMFWrite, lmfWMFRead;

constructor TlmfImage.Create;
begin
  inherited Create;
  fCrs:=syncobjs.TCriticalSection.Create;
  fList:=TlmfList.Create(nil);
end;

destructor TlmfImage.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
  fCrs.Free;
end;

procedure TlmfImage.AssignTo(Dest:TPersistent);
var
  mf:TMemoryStream;
begin
  if Dest is TlmfImage then
  begin
    mf := TMemoryStream.Create;
    try
      mf.WriteComponent(fList);
      mf.Position := 0;
      mf.ReadComponent(TlmfImage(Dest).fList);
      TlmfImage(Dest).fWidth:=fWidth;
      TlmfImage(Dest).fHeight:=fHeight;
      TlmfImage(Dest).LogUnitsPerInch := LogUnitsPerInch;
    finally
      mf.Free;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TlmfImage.Clear;
begin
  fList.DestroyComponents;
end;

function TlmfImage.GetLogUnitsPerInch: Integer;
begin
  Result := fList.LogUnitsPerInch;
end;

procedure TlmfImage.SetLogUnitsPerInch(AValue: Integer);
begin
  fList.LogUnitsPerInch := AValue;
end;

function TlmfImage.GetWidth: integer;
begin
  Result := flist.fWidth;
end;

procedure TlmfImage.SetWidth(AVal: integer);
begin
  if (AVal=fWidth) then exit;
  fWidth:=AVal;
  fList.fWidth:=fWidth;
  Self.Modified:=true;
end;

function TlmfImage.GetHeight:integer;
begin
  Result:=fList.fHeight;
end;

function TlmfImage.ScaleSizeX(ax: Integer): Integer;
begin
  Result := round(ax * kx);
end;

function TlmfImage.ScaleSizeY(ay: Integer): Integer;
begin
  Result := round(ay * ky);
end;

function TlmfImage.ScaleX(ax: Integer):integer;
begin
  Result := fOrgX + trunc(ax * kx);
  //if Result>Width then Result:=width;
end;

function TlmfImage.ScaleY(ay: Integer):integer;
begin
  Result := fOrgY + trunc(ay * ky);
  //if Result>height then Result:=height;
end;

procedure TlmfImage.SetHeight(AVal:integer);
begin
  if (AVal=fHeight) then exit;
  fHeight:=AVal;
  fList.fHeight:=fHeight;
  Modified:=true;
end;

function TlmfImage.GetEmpty:boolean;
begin
  Result:=Assigned(fList) and (fList.ComponentCount>0);
end;

procedure TlmfImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  i:integer;
  item: TlmfObject;
  bkMode: Word;
begin
  fCrs.Acquire;
  try
    bkMode := 1;  // Transparent
    fOrgX:=Rect.Left;
    fOrgY:=Rect.Top;
    kx:=(Rect.Right-Rect.Left)/Width;
    ky:=(Rect.Bottom-Rect.Top)/Height;
    ACanvas.MoveTo(ScaleX(Rect.Left), ScaleY(Rect.Top));
    for i:=0 to fList.ComponentCount-1 do
    begin
      item := TlmfObject(fList.Components[i]);
      // It seems that SetBkMode must be executed immediately before a command
      // which depends on it (text, patterned line) is drawn.
      if (item is TlmfBkMode) then
        bkMode := TlmfBkMode(item).Mode
      else
      if (item is TlmfLine) or (item is TlmfLineTo) or (item is TlmfText) then
        SetBkMode(ACanvas.Handle, bkMode);
      item.Action(Self, ACanvas);
    end;
  finally
    kx:=1;
    ky:=1;
    fCrs.Release;
  end;
end;

function TlmfImage.GetTransparent: Boolean;
begin
  Result:=true; // assume it is always
end;

procedure TlmfImage.SetTransparent(Value: Boolean);
begin
  // nothing to do
end;

procedure TlmfImage.SaveToStream(Stream: TStream);
begin
  Stream.WriteComponent(fList);
end;

procedure TlmfImage.SaveToLMFFile(AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    SaveToLMFStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TlmfImage.SaveToLMFStream(Stream: TStream);
var
  writer: TlmfWriter;
begin
  if FEnhanced then
    //writer := TEMFWriter.Create  // to be completed...
  else
    writer := TWMFWriter.Create;
  try
    writer.WriteToStream(Stream, self);
  finally
    writer.Free;
  end;
end;

procedure TlmfImage.LoadFromLMFFile(AFileName: String);
var
  stream: TFileStream;
  isWMF: Boolean;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromLMFStream(stream, false);
  finally
    stream.Free;
  end;
end;

procedure TlmfImage.LoadFromLMFStream(AStream: TStream; IsEnhanced: Boolean);
var
  reader: TlmfReader;
begin
  if IsEnhanced then
    //reader := TEMFReader.Create  // to be completed...
  else
    reader := TlmfWMFReader.Create;
  try
    reader.ReadFromStream(AStream, self);
  finally
    reader.Free;
  end;
end;

procedure TlmfImage.LoadFromStream(Stream: TStream);
begin
  Stream.ReadComponent(fList);
end;


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
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoLineTo(x, y: integer);
var
  item: TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLineTo.Create(x,y);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoLine(x1, y1, x2, y2: integer);
var
  item:TlmfAnchor;
begin
  RequiredState([csPenValid]);
  item := TlmfLine.Create(x1,y1,x2,y2);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.DoEllipse(const Bounds:TRect);
var
  item:TlmfEllipse;
begin
  RequiredState([csPenValid, csBrushValid]);
  item := TlmfEllipse.Create(Bounds);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.TextOut(x, y: integer; const AText: string);
var
  item: TlmfText;
begin
  RequiredState([csFontValid, csBrushValid]);
  item := TlmfText.Create(x, y, AText);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.TextRect(ARect: TRect; X, Y: integer; const Text: string;
  const Style: TTextStyle);
var
  item:TlmfText;
begin
  RequiredState([csFontValid,csBrushValid]);
  item:=TlmfTextInRect.Create(ARect, x, y, Text, Style);
  fImage.fList.InsertComponent(item);
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
  fImage.fList.InsertComponent(item);
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
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.SetPixel(X,Y: Integer; Value: TColor);
begin
  SetColor(x,y,TColorToFPColor(Value));
end;

function TlmfCanvas.GetColor(x,y:integer): TFPColor;
begin
  Result.alpha:=0;
  Result.red:=0;
  Result.green:=0;
  Result.blue:=0;
end;

procedure TlmfCanvas.SetClipRect(const AValue: TRect);
var
  item:TlmfObject;
begin
  inherited SetClipRect(AValue);
  fClipRect:=AValue;
  item:=TlmfClip.Create(AValue);
  fImage.fList.InsertComponent(item);
end;

function TlmfCanvas.GetClipRect:TRect;
begin
  Result:=fClipRect;
end;

procedure TlmfCanvas.Rectangle(X1,Y1,X2,Y2: Integer);
var
  item:TlmfObject;
begin
  RequiredState([csPenValid, csBrushValid]); // this adds TlmfPen and TlmfBrush
  item := TlmfRect.Create(Rect(x1,y1,x2,y2));
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.RoundRect(X1, Y1, X2, Y2, Rx, Ry: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csPenValid, csBrushValid]);
  item := TlmfRoundRect.Create(Rect(X1, Y1, X2, Y2), Rx, Ry);
  fImage.fList.InsertComponent(item);
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
  fImage.fList.InsertComponent(item);

  Pen.Style := ps;
end;

procedure TlmfCanvas.Frame(const ARect: TRect);
var
  item: TlmfObject;
  bs: TBrushStyle;
begin
  bs := Brush.Style;
  Brush.Style := bsClear;

  RequiredState([csPenValid, csBrushValid]);
  item := TlmfRect.Create(ARect);
  fImage.fList.InsertComponent(item);

  Brush.Style := bs;
end;

procedure TlmfCanvas.FrameRect(const ARect: TRect);
var
  item: TlmfObject;
  ps: TPenStyle;
begin
  ps := Pen.Style;
  Pen.Style := psClear;

  RequiredState([csPenValid, csBrushValid]);
  item := TlmfFrameRect.Create(ARect);
  fImage.fList.InsertComponent(item);

  Pen.Style := ps;
end;

procedure TlmfCanvas.Frame3D(var ARect: TRect; TopColor, BottomColor: TColor;
  const Framewidth: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csPenValid]);
  item := TlmfFrame3d.Create(ARect, TopColor, BottomColor, FrameWidth);
  fImage.fList.InsertComponent(item);
  InflateRect(ARect, FrameWidth, FrameWidth);
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

(*
procedure TlmfCanvas.DoRectangleFill(const Bounds:TRect);
var
  item:TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]); // this adds TlmfBrush and TlmfPen
  item := TlmfRect.Create(nil);
  fImage.fList.InsertComponent(item);
end;
*)
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
//  item.Name:=Font.Name;
  item.Rotation:=TFont(item.Font).Orientation;

  //writems('Created font "%s" size=%d rot=%d',[item.Font.Name,item.Font.Size,TrotFont(item.Font).Rotation]);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.CreateBrush;
var
  item:TlmfBrush;
begin
  item:=TlmfBrush.Create(nil);
  item.Brush.Assign(Brush);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.CreatePen;
var
  item:TlmfPen;
begin
  item:=TlmfPen.Create(nil);
  item.Pen.Assign(Pen);
  fImage.fList.InsertComponent(item);
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
    dc:HDC;
    ofh:HFONT;
  begin
    dc:=CreateCompatibleDC(0);
    try
      try
        ofh:=SelectObject(dc,Font.Handle);
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
  if Text='' then exit;
  RequiredState([csHandleValid,csFontValid]);
  so:=TSafeObject.Create;
  try
    so.Font:=Self.Font;
    so.Str:=Text;
    TThread.Synchronize(nil,so.MeasureExtent);
    Result:=so.ext;
  finally
    so.Free;
  end;
end;

procedure TlmfCanvas.DoGetTextSize (text:string; var w,h:integer);
var
  sz:TSize;
begin
  sz:=TextExtent(Text);
  w:=sz.cx;
  h:=sz.cy;
end;

procedure TlmfCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  item:TlmfPolyLine;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  item:=TlmfPolyline.Create(Points,NumPts);
  item.Clip := Self.ClipRect;
  fImage.fList.InsertComponent(item);
  Changed;
end;

procedure TlmfCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean = False);
var
  item:TlmfPolygon;
begin
  if NumPts <= 0 then exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  item := TlmfPolygon.Create(Points, NumPts, Winding);
  item.Clip := Self.ClipRect;
  fImage.fList.InsertComponent(item);
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
  fImage.fList.InsertComponent(item);
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

procedure TlmfCanvas.Chord(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]);
  item := TlmfChord.Create(Rect(ALeft, ATop, ARight, ABottom), Point(SX, SY), Point(EX, EY));
  fImage.fList.InsertComponent(item);
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
  fImage.fList.InsertComponent(item);
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

procedure TlmfCanvas.GradientFill(const ARect: TRect;
  AStartColor, AEndColor: TColor; ADirection: TGradientDirection);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid, csPenValid]);
  item := TlmfGradientFill.Create(ARect, AStartColor, AEndColor, ADirection);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.SetBkColor(AColor: TColor);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfBkColor.Create(AColor);
  fImage.fList.InsertComponent(item);
end;

procedure TlmfCanvas.SetBkMode(AMode: Word);
var
  item: TlmfObject;
begin
  RequiredState([csBrushValid]);
  item := TlmfBkMode.Create(AMode);
  fImage.fList.InsertComponent(item);
end;


{ LMF list }

constructor TlmfList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogUnitsPerInch := TWIPS_PER_INCH;
end;

procedure TlmfList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i:integer;
begin
  for i:=0 to ComponentCount-1 do
    Proc(Components[i]);
end;

function TlmfList.GetChildOwner: TComponent;
begin
  Result:=self;
end;


initialization
  RegisterClasses([TlmfList]);

end.

