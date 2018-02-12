{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin, Werner Pamler

  Notes:
  - This unit is not "used" by the TAChart package. In order to find it the
    unit should be copied to the project folder or specified with its path
    in the uses clause (see demo project).

  - If define CHARTGL_USE_LAZFREETYPE is activated in the package options then
    the LazFreeType library is used for rendering text. If not, the GLUT library
    is used instead. Note that GLUT is not available on every system.
}

unit TADrawerOpenGL;

{$H+}

interface

{$DEFINE CHARTGL_USE_LAZFREETYPE}

uses
  Classes, SysUtils, FPCanvas, FPImage,
  TAChartUtils, TADrawUtils;

type
  { TOpenGLDrawer }

  TOpenGLDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FBrushColor: TFPColor;
    FFontColor: TFPColor;
    FPenColor: TFPColor;
    FPenStyle: TFPPenStyle;
    FPenWidth: Integer;
    FFontName: String;
    FFontSize: Integer;
    FFontStyle: TChartFontStyles;
    FFontAngle: Double;  // in degrees
    FPos: TPoint;
    procedure ChartGLColor(AColor: TFPColor);
    procedure ChartGLPenStyle(APenStyle: TFPPenStyle);
    procedure InternalPolyline(
      const APoints: array of TPoint; AStartIndex, ANumPts, AMode: Integer);
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    constructor Create;
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetFontAngle: Double; override;
    function GetFontColor: TFPColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TChartFontStyles; override;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override;
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor); override;
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
    procedure SetTransparency(ATransparency: TChartTransparency);
  end;


implementation

uses
  GL, GLu, FileUtil,
  Math,
 {$IFDEF CHARTGL_USE_LAZFREETYPE}
  LazFileUtils,
  EasyLazFreeType, LazFreeTypeFPImageDrawer, LazFreeTypeFontCollection, TAFonts,
 {$ELSE}
  Glut,
 {$ENDIF}
  TAGeometry;

{$IFDEF CHARTGL_USE_LAZFREETYPE}
type
  TTextureCacheItem = class
    TextureID: Gluint;
    TextWidth: Integer;
    TextHeight: Integer;
  end;

  TGLFreeTypeHelper = class
  private
    FFont: TFreeTypeFont;
    FImg: TFPMemoryImage;
    FDrawer: TFPImageFreeTypeDrawer;
    FTextureCache: TStringList;
  protected
    function BuildTextureName(AText: String): String;
    procedure CreateTexture(AText: String; out ATextWidth, ATextHeight,
      ATextureWidth, ATextureHeight: Integer; out ATextureID: GLuint);
    function FindTexture(AText: String; out ATextWidth, ATextHeight,
      ATextureWidth, ATextureHeight: Integer): GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RenderText(AText: String; Alignments: TFreeTypeAlignments);
    procedure SetFont(AFontName: String; AFontSize: Integer;
      ABold: Boolean = false; AItalic: Boolean = false;
      AUnderline: Boolean = false; AStrikethrough: Boolean = false);
    procedure TextExtent(AText: String; out AWidth, AHeight: Integer);
  end;

var
  GLFreeTypeHelper: TGLFreeTypeHelper = nil;

function NextPowerOf2(n: Integer): Integer;
begin
  Result := 1;
  while Result < n do
    Result := Result * 2;
end;


{ TGLFreeTypeHelper }

constructor TGLFreeTypeHelper.Create;
begin
  FImg := TFPMemoryImage.Create(8, 8);  // dummy size, will be updated when needed
  FDrawer := TFPImageFreeTypeDrawer.Create(FImg);
  FTextureCache := TStringList.Create;
  FTextureCache.Sorted := true;
end;

destructor TGLFreeTypeHelper.Destroy;
var
  i: Integer;
  item: TTextureCacheItem;
begin
  for i:=0 to FTextureCache.Count-1 do begin
    item := TTextureCacheItem(FTextureCache.Objects[i]);
    glDeleteTextures(1, @item.TextureID);
    item.Free;
  end;
  FTextureCache.Free;
  if FFont <> nil then FFont.Free;
  FDrawer.Free;
  FImg.Free;
  inherited;
end;

{ The texture items are stored in the FTextureCache list and can be identified
  by means of their name which is composed of the text and font parameters.
  The name of the texture items is calculated here. }
function TGLFreeTypeHelper.BuildTextureName(AText: String): String;
begin
  Result := Format('%s|%s|%d|%s', [
    AText, FFont.Family, round(FFont.SizeInPoints*100), FFont.StyleAsString
  ]);
end;

procedure TGLFreeTypeHelper.CreateTexture(AText: String; out ATextWidth, ATextHeight,
  ATextureWidth, ATextureHeight: Integer; out ATextureID: GLuint);
var
  expanded_data: packed array of byte;
  i, j: Integer;
  c: TFPColor;
begin
  if FFont = nil then
    raise Exception.Create('No font selected.');

  ATextWidth := round(FFont.TextWidth(AText));
  ATextHeight := round(FFont.TextHeight(AText));
  ATextureWidth := NextPowerOf2(ATextWidth);
  ATextureHeight := NextPowerOf2(ATextHeight);

  FImg.SetSize(ATextureWidth, ATextureHeight);
  FDrawer.FillPixels(colTransparent);
  FDrawer.DrawText(AText, FFont, 0,0, colRed, [ftaLeft, ftaTop]);

  SetLength(expanded_data, 2*ATextureWidth * ATextureHeight);
  for j:=0 to ATextureHeight-1 do
    for i:=0 to ATextureWidth-1 do
    begin
      expanded_data[2*(i + j*ATextureWidth)] := 255;     // Luminosity
      if (i > ATextWidth) or (j > ATextHeight) then
        expanded_data[2*(i + j*ATextureWidth) + 1] := 0  // Alpha
      else begin
        c := FImg.Colors[i,j];
        expanded_data[2*(i + j*ATextureWidth) + 1] := FImg.Colors[i, j].Alpha shr 8;
      end;
    end;

  // Set up texture parameters
  glGenTextures(1, @ATextureID);
  glBindTexture(GL_TEXTURE_2D, ATextureID);

  // Create the texture
  // Note that we are using GL_LUMINANCE_ALPHA to indicate that we are using
  // two-channel data
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ATextureWidth, ATextureHeight, 0,
    GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @expanded_data[0]);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
end;

{ Finds the texture id for the given text. Returns the texture id and the
  size of text and texture. Note that the texture size must be a power of 2 and
  thus can be different from the text size. }
function TGLFreeTypeHelper.FindTexture(AText: String;
  out ATextWidth, ATextHeight, ATextureWidth, ATextureHeight: Integer): GLuint;
var
  idx: Integer;
  item: TTextureCacheItem;
  txname: String;
begin
  txname := BuildTextureName(AText);
  idx := FTextureCache.IndexOf(txname);
  if idx = -1 then begin
    CreateTexture(AText, ATextWidth, ATextHeight, ATextureWidth, ATextureHeight, Result);
    item := TTextureCacheItem.Create;
    item.TextureID := Result;
    item.TextWidth := ATextWidth;
    item.TextHeight := ATextHeight;
    FTextureCache.AddObject(txname, item);
  end else begin
    item := TTextureCacheItem(FTextureCache.Objects[idx]);
    result := item.TextureID;
    ATextWidth := item.TextWidth;
    ATextHeight := item.TextHeight;
    ATextureWidth := NextPowerOf2(ATextWidth);
    ATextureHeight := NextPowerOf2(ATextHeight);
  end;
end;

procedure TGLFreeTypeHelper.RenderText(AText: String; Alignments: TFreeTypeAlignments);
var
  textureID: GLuint;
  w, h: Integer;
  w2, h2: Integer;
  sx, sy: Double;
  dx, dy: Integer;
begin
  textureID := FindTexture(AText, w, h, w2, h2);
  sx := w / w2;
  sy := h / h2;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
    // Note: We don't support ftaJustify)
    if (ftaCenter in Alignments) then dx := -w div 2
    else if (ftaRight in ALignments) then dx := -w
    else dx := 0;

    if (ftaVerticalCenter in Alignments) then dy := -h div 2
    else if (ftaBottom in Alignments) then dy := -h
    else if (ftaBaseline in Alignments) then dy := - h + round(FFont.Descent)
    else dy := 0;

    glTranslatef(dx, dy, 0);
    glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, textureID);
      glBegin(GL_QUADS);
        glTexCoord2f(0.0, sy);  glVertex2f(0, h);
        glTexCoord2f(sx, sy);   glVertex2f(w, h);
        glTexCoord2f(sx, 0.0);  glVertex2f(w, 0);
        glTexCoord2f(0.0, 0.0); glVertex2f(0, 0);
      glEnd();
    glDisable(GL_TEXTURE_2D);
  glPopMatrix;
end;

procedure TGLFreeTypeHelper.SetFont(AFontName: String; AFontSize: Integer;
  ABold: Boolean = false; AItalic: Boolean = false;
  AUnderline: Boolean = false; AStrikethrough: Boolean = false);
var
  style: TFreeTypeStyles;
begin
  if GLFreeTypeHelper = nil then
    raise Exception.Create('InitFonts has not been called.');

  style := [];
  if ABold then Include(style, ftsBold);
  if AItalic then Include(style, ftsItalic);

  // Create a new font if not yet loaded
  if (FFont = nil) or (FFont.Family <> AFontName) or (FFont.Style <> style) then
  begin
    FreeAndNil(FFont);
    FFont := LoadFont(AFontName, style);
    if FFont = nil then
      raise Exception.CreateFmt('Font "%s" not found.', [AFontName]);
  end;

  // Set the requested font attributes.
  FFont.SizeInPoints := AFontSize;
  FFont.UnderlineDecoration := AUnderline;
  FFont.StrikeoutDecoration := AStrikethrough;
  FFont.Hinted := true;
  FFont.Quality := grqHighQuality;
  //FFont.ClearType := true;
end;

{ Returns the width and height of the specified text. If the text already has
  been handled with the same font parameters it is stored in the FTextureCache
  list. If not, the size is determined from the font. }
procedure TGLFreeTypeHelper.TextExtent(AText: String; out AWidth, AHeight: Integer);
var
  txname: String;
  idx: Integer;
  item: TTextureCacheItem;
  textureID: Gluint;
  w2, h2: Integer;
begin
  txname := BuildTextureName(AText);
  idx := FTextureCache.IndexOf(txname);
  if idx = -1 then begin
    CreateTexture(AText, AWidth, AHeight, w2, h2, textureID);
    item := TTextureCacheItem.Create;
    item.TextureID := textureID;
    item.TextWidth := AWidth;
    item.TextHeight := AHeight;
    idx := FTextureCache.AddObject(txname, item);
  end;

  item := TTextureCacheItem(FTextureCache.Objects[idx]);
  AWidth := item.TextWidth;
  AHeight := item.TextHeight;
end;

{$ENDIF}


{ TOpenGLDrawer }

constructor TOpenGLDrawer.Create;
{$IFDEF CHARTGL_USE_LAZFREETYPE}
begin
  inherited;
  InitFonts;
  if GLFreeTypeHelper = nil then
    GLFreeTypeHelper := TGLFreeTypeHelper.Create;
end;
{$ELSE}
var
  CmdCount : Integer;
  Cmd : Array of Pchar;
  I: Integer;
begin
  CmdCount := Paramcount+1;
  SetLength(Cmd,CmdCount);
  for I := 0 to CmdCount - 1 do
     Cmd[I] := PChar(ParamStr(I));
  glutInit (@CmdCount,@Cmd);
end;
{$ENDIF}

procedure TOpenGLDrawer.AddToFontOrientation(ADelta: Integer);
begin
  FFontAngle := FFontAngle + ADelta / ORIENTATION_UNITS_PER_DEG;
end;

procedure TOpenGLDrawer.ChartGLColor(AColor: TFPColor);
begin
  with AColor do
    glColor4us(red, green, blue, (255 - FTransparency) shl 8);
end;

procedure TOpenGLDrawer.ChartGLPenStyle(APenStyle: TFPPenStyle);
var
  pattern: Word;
begin
  case APenStyle of
    psClear      : pattern := %0000000000000000;
    psDot        : pattern := %0011001100110011;
    psDash       : pattern := %0000000011111111;
    psDashDot    : pattern := %0001100011111111;
    psDashDotDot : pattern := %0001101100111111;
    else
      glDisable(GL_LINE_STIPPLE);   // --> psSolid
      exit;
      // psPattern will render as psSolid because there are differences in
      // implementations between fpc and lcl.
      // psInsideFrame will render as psSolid - I don't know what this is...
  end;
  glLineStipple(1, pattern);
  glEnable(GL_LINE_STIPPLE);
end;

procedure TOpenGLDrawer.ClippingStart(const AClipRect: TRect);
type
  TGLClipPlaneEqn = record A, B, C, D: GLdouble; end;
var
  cp: TGLClipPlaneEqn;
begin
  cp.A := 1.0;
  cp.D := -AClipRect.Left;
  glClipPlane(GL_CLIP_PLANE0, @cp);
  cp.A := -1.0;
  cp.D := AClipRect.Right;
  glClipPlane(GL_CLIP_PLANE1, @cp);
  cp.A := 0.0;
  cp.B := 1.0;
  cp.D := -AClipRect.Top;
  glClipPlane(GL_CLIP_PLANE2, @cp);
  cp.B := -1.0;
  cp.D := AClipRect.Bottom;
  glClipPlane(GL_CLIP_PLANE3, @cp);
  ClippingStart;
end;

procedure TOpenGLDrawer.ClippingStart;
begin
  glEnable(GL_CLIP_PLANE0);
  glEnable(GL_CLIP_PLANE1);
  glEnable(GL_CLIP_PLANE2);
  glEnable(GL_CLIP_PLANE3);
end;

procedure TOpenGLDrawer.ClippingStop;
begin
  glDisable(GL_CLIP_PLANE0);
  glDisable(GL_CLIP_PLANE1);
  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
end;

procedure TOpenGLDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  p: TPointArray;
begin
  p := TesselateEllipse(Rect(AX1, AY1, AX2, AY2), 4);
  Polygon(p, 0, Length(p));
end;

procedure TOpenGLDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  ChartGLColor(FBrushColor);
  glRecti(AX1, AY1, AX2, AY2);
end;

function TOpenGLDrawer.GetBrushColor: TChartColor;
begin
  Result := FPColorToChartColor(FBrushColor);
end;

function TOpenGLDrawer.GetFontAngle: Double;
begin
  {$IFDEF CHARTGL_USE_LAZFREETYPE}
  Result := DegToRad(FFontAngle);
  {$ELSE}
  Result := 0.0;
  {$ENDIF}
end;

function TOpenGLDrawer.GetFontColor: TFPColor;
begin
  Result := FFontColor;
end;

function TOpenGLDrawer.GetFontName: String;
begin
  Result := FFontName;
end;

function TOpenGLDrawer.GetFontSize: Integer;
begin
  Result := IFThen(FFontSize = 0, DEFAULT_FONT_SIZE, FFontSize);
end;

function TOpenGLDrawer.GetFontStyle: TChartFontStyles;
begin
  Result := FFontStyle;
end;

procedure TOpenGLDrawer.InternalPolyline(
  const APoints: array of TPoint; AStartIndex, ANumPts, AMode: Integer);
var
  i: Integer;
begin
  if FPenStyle = psClear then exit;
  ChartGLColor(FPenColor);
  glBegin(AMode);
  for i := AStartIndex to AStartIndex + ANumPts - 1 do
    glVertex2iv(@APoints[i]);
  glEnd();
end;

procedure TOpenGLDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  if FPenStyle = psClear then exit;
  glBegin(GL_LINES);
  ChartGLColor(FPenColor);
  glVertex2i(AX1, AY1);
  glVertex2i(AX2, AY2);
  glEnd();
end;

procedure TOpenGLDrawer.Line(const AP1, AP2: TPoint);
begin
  Line(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

procedure TOpenGLDrawer.LineTo(AX, AY: Integer);
begin
  Line(FPos.X, FPos.Y, AX, AY);
end;

procedure TOpenGLDrawer.MoveTo(AX, AY: Integer);
begin
  FPos := Point(AX, AY);
end;

procedure TOpenGLDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
var
  i: Integer;
begin
  glBegin(GL_POLYGON);
  ChartGLColor(FBrushColor);
  for i := AStartIndex to AStartIndex + ANumPts - 1 do
    glVertex2iv(@APoints[i]);
  glEnd();
  InternalPolyline(APoints, AStartIndex, ANumPts, GL_LINE_LOOP);
end;

procedure TOpenGLDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  InternalPolyline(APoints, AStartIndex, ANumPts, GL_LINE_STRIP);
end;

procedure TOpenGLDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPenWidth := 1;
  FPenColor := FChartColorToFPColorFunc(AColor);
  FPenStyle := psSolid;
end;

procedure TOpenGLDrawer.PutPixel(AX, AY: Integer; AColor: TChartColor);
begin
  ChartGLColor(FChartColorToFPColorFunc(AColor));
  glBegin(GL_POINTS);
  glVertex2i(AX, AY);
  glEnd;
end;

procedure TOpenGLDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
var
  e: TEllipse;
  p: TPointArray;
begin
  e.InitBoundingBox(AX1, AY1, AX2, AY2);
  p := e.TesselateRadialPie(
    Deg16ToRad(AStartAngle16Deg), Deg16ToRad(AAngleLength16Deg), 4);
  Polygon(p, 0, Length(p));
end;

procedure TOpenGLDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  ChartGLColor(FBrushColor);
  glRecti(AX1, AY1, AX2, AY2);
  if FPenStyle = psClear then exit;
  ChartGLColor(FPenColor);
  glBegin(GL_LINE_LOOP);
  glVertex2i(AX1, AY1);
  glVertex2i(AX2, AY1);
  glVertex2i(AX2, AY2);
  glVertex2i(AX1, AY2);
  glEnd();
end;

procedure TOpenGLDrawer.Rectangle(const ARect: TRect);
begin
  Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TOpenGLDrawer.ResetFont;
begin
end;

procedure TOpenGLDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
begin
  case AValue of
    amOn: begin
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
    end;
    amOff: begin
      glDisable(GL_LINE_SMOOTH);
      glDisable(GL_POLYGON_SMOOTH);
    end;
  end;
end;

procedure TOpenGLDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  FBrushColor := ABrush.FPColor;
end;

procedure TOpenGLDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(AColor);
end;

procedure TOpenGLDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  SetBrushColor(AColor);
  Unused(AStyle);
end;

procedure TOpenGLDrawer.SetFont(AFont: TFPCustomFont);
begin
  FFontName := AFont.Name;
  if SameText(FFontName, 'default') then FFontName := 'Arial';
  FFontSize := IfThen(AFont.Size = 0, DEFAULT_FONT_SIZE, AFont.Size);
  FFontStyle := [];
  if AFont.Bold then Include(FFontStyle, cfsBold);
  if AFont.Italic then Include(FFontStyle, cfsItalic);
  if AFont.Underline then Include(FFontStyle, cfsUnderline);
  if AFont.Strikethrough then Include(FFontStyle, cfsStrikeout);
  FFontColor := AFont.FPColor;

 {$IFDEF CHARTGL_USE_LAZFREETYPE}
  FFontAngle := FGetFontOrientationFunc(AFont) / ORIENTATION_UNITS_PER_DEG;
  GLFreeTypeHelper.SetFont(FFontName, FFontSize,
    AFont.Bold, AFont.Italic, AFont.Underline, AFont.Strikethrough);
 {$ENDIF}
end;

procedure TOpenGLDrawer.SetPen(APen: TFPCustomPen);
begin
  FPenWidth := APen.Width;
  FPenColor := APen.FPColor;
  FPenStyle := APen.Style;
  glLineWidth(FPenWidth);
  ChartGLPenStyle(FPenStyle);
end;

procedure TOpenGLDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FPenStyle := AStyle;
  FPenColor := FChartColorToFPColorFunc(AColor);
  ChartGLPenStyle(AStyle);
end;

procedure TOpenGLDrawer.SetTransparency(ATransparency: TChartTransparency);
begin
  inherited;
  if FTransparency > 0 then begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end
  else
    glDisable(GL_BLEND);
end;

{$IFDEF CHARTGL_USE_LAZFREETYPE}

function TOpenGLDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  GLFreeTypeHelper.TextExtent(AText, Result.X, Result.Y);
end;

procedure TOpenGLDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
begin
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  ChartGLColor(FFontColor);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glTranslatef(AX, AY, 0);
  glRotatef(-FFontAngle, 0, 0, 1);
  GLFreeTypeHelper.RenderText(AText, [ftaLeft, ftaTop]);
  glPopMatrix;
end;

{$ELSE}

function TOpenGLDrawer.SimpleTextExtent(const AText: String): TPoint;
const
  F_WIDTH = 8;
  F_HEIGHT = 13;
begin
  Result := Point(F_WIDTH * Length(AText), F_HEIGHT);
end;

procedure TOpenGLDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
const
  X_OFFSET = 0;
  Y_OFFSET = 10;
var
  i: Integer;
begin
  ChartGLColor(FFontColor);
  glRasterPos2i(AX + X_OFFSET, AY + Y_OFFSET);
  for i := 1 to Length(AText) do
    glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord(AText[i]));
end;
{$ENDIF}

initialization

finalization
 {$IFDEF CHARTGL_USE_LAZFREETYPE}
  FreeAndNil(GLFreeTypeHelper);
 {$ENDIF}

end.

