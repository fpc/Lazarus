unit TAOpenGL;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazFileUtils, FPCanvas, FPImage, gl,
  EasyLazFreeType, LazFreeTypeFPImageDrawer, LazFreeTypeFontCollection, TAFonts;

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


implementation

uses
  SysUtils;

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
        expanded_data[2*(i + j*ATextureWidth) + 1] := c.Alpha shr 8;
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

end.

