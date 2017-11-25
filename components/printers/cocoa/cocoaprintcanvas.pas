{
 *****************************************************************************
  This file is part of the Printer4Lazarus package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 custom-page in cocoa example:
  https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/Printing/osxp_pagination/osxp_pagination.html
}
unit cocoaprintcanvas;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // fpc
  MacOSAll, CocoaAll, Classes, SysUtils, fpcanvas, Contnrs, Types,
  // lcl-widgetset
  CocoaUtils, CocoaGDIObjects,
  // lcl
  Printers, LCLType, Graphics;

type

  { TCanvasOperation }

  TCanvasOperation = class(TObject)
  public
    procedure DrawTo(const ADest: TCocoaContext); virtual; abstract;
  end;

  { TCanvasOperation_Pen }

  TCanvasOperation_Pen = class(TCanvasOperation)
  public
    Pen_Color: TColor; // default clBlack;
    Pen_Cosmetic: Boolean; // default True;
    Pen_Style: TFPPenStyle;
    Pen_Width: Integer;
    Pen_Mode: TFPPenMode;
    Pen_EndCap: TFPPenEndCap;
    Pen_JoinStyle: TFPPenJoinStyle;
    //
    DisablePen: Boolean; // for descendents to disable pen setting in DrawTo
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  { TCanvasOperation_Pen_Brush }

  TCanvasOperation_Pen_Brush = class(TCanvasOperation_Pen)
  public
    Brush_Color: TColor; // default clBlack;
    Brush_Style: TFPBrushStyle;
    Brush_Pattern: TBrushPattern;
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  TCanvasOperation_Font = class(TCanvasOperation_Pen_Brush)
  public
    Font_Name: string;
    Font_Size: integer;
    Font_Bold: boolean;
    Font_Italic: boolean;
    Font_Underline: boolean;
    Font_StrikeThrough: boolean;
    Font_Orientation: Integer;
    //
    Font_Pitch: TFontPitch;
    Font_Quality: TFontQuality;
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  { TCanvasOperation_Ellipse }

  TCanvasOperation_Ellipse = class(TCanvasOperation_Pen_Brush)
  public
    X1, Y1, X2, Y2: Integer;
    constructor Create(AX1, AY1, AX2, AY2: Integer);
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  TCanvasOperation_Rectangle = class(TCanvasOperation_Pen_Brush)
  public
    X1, Y1, X2, Y2: Integer;
    constructor Create(AX1, AY1, AX2, AY2: Integer);
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  TCanvasOperation_TextOut = class(TCanvasOperation_Font)
  public
    X, Y: Integer;
    Text: string;
    constructor Create(AX, AY: Integer; AText: string);
    procedure DrawTo(const ADest: TCocoaContext); override;
  end;

  { TCocoaPrinterCanvas }

  TCocoaPrinterCanvas = class(TFilePrinterCanvas)
  private
    FLazClipRect: TRect;
    FRecordingPages: TFPObjectList; // list of 1 (FCurRecording: TFPObjectList) per page
    FCurRecording: TFPObjectList; // this is the current recording
    function SetCurPage(APageNum: Integer): Boolean;
    function GetOperationCount: Integer;
    function GetOperation(ANum: Integer): TCanvasOperation;
    procedure SetPenProperties(ADest: TCanvasOperation_Pen);
    procedure SetBrushProperties(ADest: TCanvasOperation_Pen_Brush);
    procedure SetFontProperties(ADest: TCanvasOperation_Font);
    procedure SetPenAndBrush(ADest: TCanvasOperation_Pen_Brush);
    procedure SetFontAndBrush(ADest: TCanvasOperation_Font);
  protected
    ScaleX, ScaleY, FontScale: Double;
    procedure SetLazClipRect(r: TRect);
    procedure DoLineTo(X1,Y1: Integer); override;
    procedure DoMoveTo({%H-}x, {%H-}y: integer); override;
    procedure SetHandle(NewHandle: HDC); override;
    function GetClipRect: TRect; override;
    procedure SetClipRect(const ARect: TRect); override;
    function GetClipping: Boolean; override;
    procedure SetClipping(const AValue: boolean); override;
    //
    procedure CreateBrush; override;
    procedure CreateFont; override;
    procedure CreateHandle; override;
    procedure CreatePen; override;
    procedure CreateRegion; override;
    procedure RealizeAntialiasing; override;
    procedure DestroyHandle;
  public
    SurfaceXDPI, SurfaceYDPI: Integer;
    constructor Create(APrinter : TPrinter); override;
    constructor Create; overload;
    destructor Destroy; override;
    procedure BeginDoc; override;
    procedure EndDoc; override;
    procedure NewPage; override;
    procedure FillRect(const ARect: TRect); override;
    procedure Rectangle(X1,Y1,X2,Y2: Integer); override;
    procedure Polyline(Points: PPoint; NumPts: Integer); override;
    procedure Polygon(Points: PPoint; NumPts: Integer; {%H-}Winding: boolean = False); override;
    procedure FrameRect(const ARect: TRect); override;
    procedure Frame(const ARect: TRect); override;
    procedure RoundRect(X1, Y1, X2, Y2: Integer; RX, RY: Integer); override;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
    procedure Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer); override;
    procedure Arc(ALeft, ATop, ARight, ABottom, StX, StY, EX, EY: Integer); override;
    procedure Chord(X1, Y1, X2, Y2, Angle1, Angle2: Integer); override;
    procedure Chord(X1, Y1, X2, Y2, StX, StY, EX, EY: Integer); override;
    procedure Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX, StartY, EndX, EndY: Integer); override;
    procedure RadialPie(Left, Top, Right, Bottom, Angle1, Angle2: Integer); override;
    procedure PolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean = False; Continuous: boolean = False); override;
    procedure TextOut(X,Y: Integer; const Text: String); override;
    procedure TextRect(ARect: TRect; X1, Y1: integer; const Text: string; const Style: TTextStyle); override;
    function TextExtent(const Text: string): TSize; override;
    function GetTextMetrics(out M: TLCLTextMetric): boolean; override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;
    procedure SetPixel(X,Y: Integer; Value: TColor); override;
  public
    procedure DrawRecording(const ADest: NSView; dirtyRect: NSRect; APageNum: Integer);
  end;

implementation

{ TCanvasOperation_Pen }

procedure TCanvasOperation_Pen.DrawTo(const ADest: TCocoaContext);
var
  lCocoaPen: TCocoaPen;
begin
  if DisablePen then Exit;

  lCocoaPen := TCocoaPen.Create(Pen_Color, Pen_Style, Pen_Cosmetic,
    Pen_Width, Pen_Mode, Pen_EndCap, Pen_JoinStyle);
  lCocoaPen.Apply(ADest);
  lCocoaPen.Free;
end;

{ TCanvasOperation_Pen_Brush }

procedure TCanvasOperation_Pen_Brush.DrawTo(const ADest: TCocoaContext);
var
  lCocoaBrush: TCocoaBrush;
begin
  inherited DrawTo(ADest);
  lCocoaBrush := TCocoaBrush.Create(Brush_Color, Brush_Style, Brush_Pattern);
  lCocoaBrush.Apply(ADest);
  lCocoaBrush.Free;
end;

{ TCanvasOperation_Font }

procedure TCanvasOperation_Font.DrawTo(const ADest: TCocoaContext);
const
  LF_BOOL: array[Boolean] of Byte = (0, 255);
  LF_WEIGHT: array[Boolean] of Integer = (FW_NORMAL, FW_BOLD);
  LF_QUALITY: array[TFontQuality] of Integer = (DEFAULT_QUALITY,
    DRAFT_QUALITY, PROOF_QUALITY, NONANTIALIASED_QUALITY, ANTIALIASED_QUALITY,
    CLEARTYPE_QUALITY, CLEARTYPE_NATURAL_QUALITY);
var
  lLogFont: TLogFont;
  lCocoaFont: TCocoaFont;
begin
  lLogFont.lfHeight := Font_Size;
  lLogFont.lfWidth := 0;
  lLogFont.lfEscapement := Font_Orientation;
  lLogFont.lfOrientation := Font_Orientation;
  lLogFont.lfWeight := LF_WEIGHT[Font_Bold];
  lLogFont.lfItalic := LF_BOOL[Font_Italic];
  lLogFont.lfUnderline := LF_BOOL[Font_Underline];
  lLogFont.lfStrikeOut := LF_BOOL[Font_StrikeThrough];
  lLogFont.lfCharSet := DEFAULT_CHARSET;
  lLogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  lLogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  lLogFont.lfQuality := LF_QUALITY[Font_Quality];
  case Font_Pitch of
    fpVariable: lLogFont.lfPitchAndFamily := VARIABLE_PITCH;
    fpFixed:    lLogFont.lfPitchAndFamily := FIXED_PITCH;
  else
    lLogFont.lfPitchAndFamily := DEFAULT_PITCH;
  end;

  lLogFont.lfFaceName := Font_Name;

  {lCocoaFont := TCocoaFont.Create(lLogFont, Font_Name);
  ADest.Font.Free;
  ADest.Font := lCocoaFont;}

  inherited DrawTo(ADest);
end;

{ TCanvasOperation_Ellipse }

constructor TCanvasOperation_Ellipse.Create(AX1, AY1, AX2, AY2: Integer);
begin
  inherited Create;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
end;

procedure TCanvasOperation_Ellipse.DrawTo(const ADest: TCocoaContext);
begin
  inherited DrawTo(ADest); // apply pen and brush
  ADest.Ellipse(X1, Y1, X2, Y2);
end;

{ TCanvasOperation_Rectangle }

constructor TCanvasOperation_Rectangle.Create(AX1, AY1, AX2, AY2: Integer);
begin
  inherited Create;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
end;

procedure TCanvasOperation_Rectangle.DrawTo(const ADest: TCocoaContext);
begin
  inherited DrawTo(ADest); // apply pen and brush
  ADest.Rectangle(X1, Y1, X2, Y2, True, nil);
end;

{ TCanvasOperation_TextOut }

constructor TCanvasOperation_TextOut.Create(AX, AY: Integer; AText: string);
begin
  inherited Create;
  DisablePen := True;
  X := AX;
  Y := AY;
  Text := AText;
end;

procedure TCanvasOperation_TextOut.DrawTo(const ADest: TCocoaContext);
begin
  inherited DrawTo(ADest); // apply pen and brush
  ADest.TextOut(X, Y, 0, nil, PChar(Text), Length(Text), nil);
end;

{ TCocoaPrinterCanvas }

function TCocoaPrinterCanvas.SetCurPage(APageNum: Integer): Boolean;
begin
  Result := False;
  if (APageNum < 0) or (APageNum >= FRecordingPages.Count) then Exit;
  FCurRecording := TFPObjectList(FRecordingPages.Items[APageNum]);
  Result := True;
end;

function TCocoaPrinterCanvas.GetOperationCount: Integer;
begin
  Result := FCurRecording.Count;
end;

function TCocoaPrinterCanvas.GetOperation(ANum: Integer): TCanvasOperation;
begin
  Result := TCanvasOperation(FCurRecording.Items[ANum]);
end;

procedure TCocoaPrinterCanvas.SetPenProperties(ADest: TCanvasOperation_Pen);
begin
  ADest.Pen_Color := Pen.Color;
  ADest.Pen_Cosmetic := Pen.Cosmetic;
  ADest.Pen_Style := Pen.Style;
  ADest.Pen_Width := Pen.Width;
  ADest.Pen_Mode := Pen.Mode;
  ADest.Pen_EndCap := Pen.EndCap;
  ADest.Pen_JoinStyle := Pen.JoinStyle;
end;

procedure TCocoaPrinterCanvas.SetBrushProperties(ADest: TCanvasOperation_Pen_Brush);
begin
  ADest.Brush_Color := Brush.Color;
  ADest.Brush_Style := Brush.Style;
  ADest.Brush_Pattern := Brush.Pattern;
end;

procedure TCocoaPrinterCanvas.SetFontProperties(ADest: TCanvasOperation_Font);
begin
end;

procedure TCocoaPrinterCanvas.SetPenAndBrush(ADest: TCanvasOperation_Pen_Brush);
begin
  SetPenProperties(ADest);
  SetBrushProperties(ADest);
end;

procedure TCocoaPrinterCanvas.SetFontAndBrush(ADest: TCanvasOperation_Font);
begin
  SetBrushProperties(ADest);
  SetFontProperties(ADest);
end;

procedure TCocoaPrinterCanvas.DoLineTo(X1, Y1: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid]);
  SetPenProperties;
  cairo_move_to(cr, SX(PenPos.X), SY(PenPos.Y));
  cairo_line_to(cr, SX(X1), SY(Y1));
  SetInternalPenPos(Point(X1,Y1));
  StrokeOnly;
  Changed;}
end;

procedure TCocoaPrinterCanvas.DoMoveTo(x, y: integer);
begin
  // should not call inherited DoMoveTo which would end calling
  // interface MoveToEx which breaks things for Qt
end;

procedure TCocoaPrinterCanvas.SetHandle(NewHandle: HDC);
begin
  inherited SetHandle(NewHandle);
end;

procedure TCocoaPrinterCanvas.BeginDoc;
begin
  inherited BeginDoc;
  if assigned(printer) then
    FLazClipRect:=printer.PaperSize.PaperRect.WorkRect;
end;

procedure TCocoaPrinterCanvas.EndDoc;
begin
  inherited EndDoc;
  {cairo_show_page(cr);
  FLazClipRect := Rect(0, 0, 0, 0);
  //if caller is printer, then at the end destroy cairo handles (flush output)
  //and establishes CreateCairoHandle call on the next print}
  Handle := 0;
end;

procedure TCocoaPrinterCanvas.NewPage;
begin
  inherited NewPage;
  //cairo_show_page(cr);
end;

procedure TCocoaPrinterCanvas.CreateBrush;
begin
end;

procedure TCocoaPrinterCanvas.CreateFont;
begin
end;

procedure TCocoaPrinterCanvas.CreateHandle;
begin
  ScaleX := SurfaceXDPI/XDPI;
  ScaleY := SurfaceYDPI/YDPI;

  if FRecordingPages = nil then
  begin
    FCurRecording := TFPObjectList.Create;
    FRecordingPages := TFPObjectList.Create;
    FRecordingPages.Add(FCurRecording);
  end;
  Handle := HWND(FRecordingPages);
end;

procedure TCocoaPrinterCanvas.CreatePen;
begin
end;

procedure TCocoaPrinterCanvas.CreateRegion;
begin
end;

procedure TCocoaPrinterCanvas.RealizeAntialiasing;
begin
end;

procedure TCocoaPrinterCanvas.DestroyHandle;
var
  i: Integer;
begin
  for i := 0 to FRecordingPages.Count-1 do
    FRecordingPages.Items[i].Free;
  FCurRecording := nil;
end;

function TCocoaPrinterCanvas.GetClipRect: TRect;
var
  x1,y1,x2,y2: double;
begin
  RequiredState([csHandleValid]);

  {// it doesn't matter what the clip is in use, default or user
  // this returns always the current clip

  result.Left:=round(x1/ScaleX);
  result.Top:=round(y1/ScaleY);
  result.Right:=round(x2/ScaleX);
  result.Bottom:=round(y2/ScaleY);}
end;

procedure TCocoaPrinterCanvas.SetClipRect(const ARect: TRect);
begin
  {RequiredState([csHandleValid]);
  if FUserClipRect=nil then
    New(FUserClipRect);

  fUserClipRect^.x := SX(ARect.Left);
  fUserClipRect^.y := SY(ARect.Top);
  fUserClipRect^.width := SX2(ARect.Right-ARect.Left);
  fUserClipRect^.height:= SY2(ARect.Bottom-ARect.Top);
  }
end;

function TCocoaPrinterCanvas.GetClipping: Boolean;
begin
  //result := (fUserClipRect<>nil);
end;

procedure TCocoaPrinterCanvas.SetClipping(const AValue: boolean);
begin
  RequiredState([csHandleValid]);
  {cairo_reset_clip(cr);

  if not AValue then begin
    // free user cliprect if exists
    if fUserClipRect<>nil then
      Dispose(fUserClipRect);
    fUserClipRect := nil;
  end
  else begin
    if fUserClipRect<>nil then
    begin
      with fUserClipRect^ do
      begin
        cairo_rectangle(cr, x, y, width, height);
        cairo_clip(cr);
      end;
    end else
      ; // cairo_reset_clip always clip
  end;}
end;

procedure TCocoaPrinterCanvas.SetLazClipRect(r: TRect);
begin
  FLazClipRect := r;
end;

constructor TCocoaPrinterCanvas.Create(APrinter: TPrinter);
begin
  inherited Create(APrinter);
  ScaleX := 1;
  ScaleY := 1;
  FontScale := 1;
  SurfaceXDPI := 72;
  SurfaceYDPI := 72;
  XDPI := SurfaceXDPI;
  YDPI := SurfaceXDPI;

  CreateHandle;
end;

constructor TCocoaPrinterCanvas.Create;
begin
  Create(nil);
end;

destructor TCocoaPrinterCanvas.Destroy;
begin
  DestroyHandle;

  inherited Destroy;
end;

procedure TCocoaPrinterCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
var
  lRect: TCanvasOperation_Rectangle;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  lRect := TCanvasOperation_Rectangle.Create(X1, Y1, X2, Y2);
  SetPenAndBrush(lRect);
  FCurRecording.Add(lRect);
  Changed;
end;

//1 point rectangle in _Brush_ color
procedure TCocoaPrinterCanvas.FrameRect(const ARect: TRect);
begin
  {Changing;
  RequiredState([csHandleValid, csBrushValid]);
  cairo_rectangle(cr, SX(ARect.Left), SY(ARect.Top), SX2(ARect.Right-ARect.Left), SY2(ARect.Bottom-ARect.Top));
  SetSourceColor(Brush.Color);
  cairo_set_line_width(cr, 1);
  cairo_stroke(cr); //Don't touch
  Changed;}
end;

procedure TCocoaPrinterCanvas.Frame(const ARect: TRect);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid]);
  cairo_rectangle(cr, SX(ARect.Left), SY(ARect.Top), SX2(ARect.Right-ARect.Left), SY2(ARect.Bottom-ARect.Top));
  cairo_set_line_width(cr, 1);
  SetSourceColor(Pen.Color);
  cairo_stroke(cr); //Don't touch
  Changed;}
end;

procedure TCocoaPrinterCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX, RY: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  cairo_move_to(cr, SX(X1+RX), SY(Y1));
  cairo_line_to(cr, SX(X2-RX), SY(Y1));
  EllipseArcPath(X2-RX, Y1+RY, RX, RY, -PI/2, 0, True, True);
  cairo_line_to(cr, SX(X2), SY(Y2-RY));
  EllipseArcPath(X2-RX, Y2-RY, RX, RY, 0, PI/2, True, True);
  cairo_line_to(cr, SX(X1+RX), SY(Y2));
  EllipseArcPath(X1+RX, Y2-RY, RX, RY, PI/2, PI, True, True);
  cairo_line_to(cr, SX(X1), SY(Y1+RX));
  EllipseArcPath(X1+RX, Y1+RY, RX, RY, PI, PI*1.5, True, True);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
var
  lEllipse: TCanvasOperation_Ellipse;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  lEllipse := TCanvasOperation_Ellipse.Create(X1, Y1, X2, Y2);
  SetPenAndBrush(lEllipse);
  FCurRecording.Add(lEllipse);
  Changed;
end;

procedure TCocoaPrinterCanvas.Arc(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid]);
  ArcPath(ALeft, ATop, ARight, ABottom, Angle16Deg, Angle16DegLength);
  StrokeOnly;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Chord(X1, Y1, X2, Y2, Angle1, Angle2: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  ArcPath(X1, Y1, X2, Y2, Angle1, Angle2);
  cairo_close_path(cr);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.RadialPie(Left, Top, Right, Bottom, Angle1, Angle2: Integer);
{var
  cx, cy: double;}
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  ArcPath(Left, Top, Right, Bottom, Angle1, Angle2);
  cx := (Right+Left)/2;
  cy := (Bottom+Top)/2;
  cairo_line_to(cr, SX(cx), SY(cy));
  cairo_close_path(cr);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Arc(ALeft, ATop, ARight, ABottom, StX, StY, EX, EY: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid]);
  ArcPath(ALeft, ATop, ARight, ABottom, StX, StY, EX, EY);
  StrokeOnly;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Chord(X1, Y1, X2, Y2, StX, StY, EX, EY: Integer);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  ArcPath(X1, Y1, X2, Y2, StX, StY, EX, EY);
  cairo_close_path(cr);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
  StartX, StartY, EndX, EndY: Integer);
var
  cx, cy: double;
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  ArcPath(EllipseX1, EllipseY1, EllipseX2, EllipseY2, StartX, StartY, EndX, EndY);
  cx := (EllipseX2+EllipseX1)/2;
  cy := (EllipseY2+EllipseY1)/2;
  cairo_line_to(cr, SX(cx), SY(cy));
  cairo_close_path(cr);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.PolyBezier(Points: PPoint; NumPts: Integer; Filled: boolean; Continuous: boolean);
begin
end;

procedure TCocoaPrinterCanvas.TextOut(X, Y: Integer; const Text: String);
var
  lText: TCanvasOperation_TextOut;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  lText := TCanvasOperation_TextOut.Create(X, Y, Text);
  SetFontAndBrush(lText);
  FCurRecording.Add(lText);
  Changed;
end;

procedure TCocoaPrinterCanvas.TextRect(ARect: TRect; X1, Y1: integer; const Text: string; const Style: TTextStyle);
var
  s: string;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Changed;
end;

function TCocoaPrinterCanvas.TextExtent(const Text: string): TSize;
begin
  RequiredState([csHandleValid, csFontValid]);
  //SelectFont;
end;

function TCocoaPrinterCanvas.GetTextMetrics(out M: TLCLTextMetric): boolean;
begin
  {RequiredState([csHandleValid, csFontValid]);
  SelectFont;
  cairo_font_extents(cr, @e); //transformation matrix is here ignored
  FillChar(M{%H-}, SizeOf(M), 0);
  M.Ascender := Round(e.ascent/ScaleY);
  M.Descender := Round(e.descent/ScaleY);
  M.Height := Round(e.height/ScaleY);
  Result := True;}
end;

procedure TCocoaPrinterCanvas.StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic);
begin
end;

procedure TCocoaPrinterCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  {Changing;
  RequiredState([csHandleValid, csPenValid]);
  SetSourceColor(Value);
  cairo_rectangle(cr, SX(X), SY(Y), 1, 1);
  cairo_fill(cr);
  Changed;}
end;

procedure TCocoaPrinterCanvas.Polyline(Points: PPoint; NumPts: Integer);
begin
  {if NumPts <= 0 then
    Exit;
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  PolylinePath(Points, NumPts);
  StrokeOnly;
  Changed;}
end;

procedure TCocoaPrinterCanvas.Polygon(Points: PPoint; NumPts: Integer; Winding: boolean);
begin
  {if NumPts <= 0 then
    Exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  PolylinePath(Points, NumPts);
  cairo_close_path(cr);
  FillAndStroke;
  Changed;}
end;

procedure TCocoaPrinterCanvas.FillRect(const ARect: TRect);
begin
  {Changing;
  RequiredState([csHandleValid, csBrushValid]);
  cairo_rectangle(cr, SX(ARect.Left), SY(ARect.Top), SX2(ARect.Right-ARect.Left), SY2(ARect.Bottom-ARect.Top));
  FillOnly;
  Changed;}
end;

procedure TCocoaPrinterCanvas.DrawRecording(const ADest: NSView; dirtyRect: NSRect; APageNum: Integer);
var
  i: Integer;
  lCurOperation: TCanvasOperation;
  lCocoaContext: TCocoaContext;
begin
  if not SetCurPage(APageNum) then Exit;
  lCocoaContext := TCocoaContext.Create(NSGraphicsContext.currentContext);
  for i := 0 to GetOperationCount()-1 do
  begin
    lCurOperation := GetOperation(i);
    lCurOperation.DrawTo(lCocoaContext);
  end;
  lCocoaContext.Free;
end;

end.

