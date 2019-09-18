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
  Classes, SysUtils, FPImage, FPCanvas,
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
    function GetPenColor: TChartColor;
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
    procedure SetPenColor(AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
    procedure SetTransparency(ATransparency: TChartTransparency);
  end;


implementation

uses
  GL, GLu, FileUtil,
  Math,
 {$IFDEF CHARTGL_USE_LAZFREETYPE}
  EasyLazFreeType, TAOpenGL, TAFonts,
 {$ELSE}
  Glut,
 {$ENDIF}
  TAGeometry;


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

function TOpenGLDrawer.GetPenColor: TChartColor;
begin
  Result := FPColorToChartColor(FPenColor);
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

procedure TOpenGLDrawer.SetPenColor(AColor: TChartColor);
begin
  FPenColor := FChartColorToFPColorFunc(AColor);
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

