{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazFreeTypeFPImageDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EasyLazFreeType, FPimage;

type
  TLazIntfImageGetPixelAtProc = procedure(p: pointer; out Color: TFPColor);
  TLazIntfImageSetPixelAtProc = procedure(p: pointer; const Color: TFPColor);

  { TFPImageFreeTypeDrawer }

  TFPImageFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FColor: TFPColor;
    FCurX,FCurY: integer;
  protected
    FImage: TFPCustomImage;
    procedure MoveToPixel(x,y: integer); virtual;
    function GetCurrentColor: TFPColor; virtual;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); virtual;
    procedure MoveRight; virtual;
    function GetClipRect: TRect; virtual;
  protected
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    procedure InternalMergeColorOver(var merge: TFPColor; const c: TFPColor; calpha: word); inline;
    procedure MergeColorOver(var merge: TFPColor; const c: TFPColor); inline;
    procedure MergeColorOver(var merge: TFPColor; const c: TFPColor; ApplyOpacity: byte); inline;
    procedure DrawPixelAndMoveRight(const c: TFPColor);
    procedure DrawPixelAndMoveRight(const c: TFPColor; applyOpacity: byte);
    procedure ClearTypePixelAndMoveRight(Cr,Cg,Cb: byte; const Color: TFPColor);
    procedure UnclippedDrawPixel(x,y: integer; const c: TFPColor);
  public
    ClearTypeRGBOrder: boolean;
    constructor Create(AImage: TFPCustomImage); virtual;
    procedure DrawPixel(x,y: integer; const c: TFPColor);
    procedure ClearTypePixel(x,y: integer; Cr,Cg,Cb: byte; const Color: TFPColor);
    procedure SetVertLine(x,y1,y2: integer; const c: TFPColor);
    procedure DrawVertLine(x,y1,y2: integer; const c: TFPColor);
    procedure SetHorizLine(x1,y,x2: integer; const c: TFPColor);
    procedure DrawHorizLine(x1,y,x2: integer; const c: TFPColor);
    procedure FillPixels(const c: TFPColor);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override;
    destructor Destroy; override;
    property Image: TFPCustomImage read FImage;
  end;

  { TFPMemoryImageWithScanline }

  TFPMemoryImageWithScanline = class(TFPMemoryImage)
  protected
    function GetScanline(y: integer): PFPColor;
    procedure SetUsePalette ({%H-}Value:boolean);override;
  public
    property ScanLine[y: integer]: PFPColor read GetScanline;
  end;

  { TFPImageWithScanlineFreeTypeDrawer }

  TFPImageWithScanlineFreeTypeDrawer= class(TFPImageFreeTypeDrawer)
  protected
    FCurrentColor: PFPColor;
    procedure MoveToPixel(x,y: integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;
  public
    constructor Create(AImage: TFPCustomImage); override; //requires TFPMemoryImageWithScanline
  end;

implementation

{ TFPImageFreeTypeDrawer }

procedure TFPImageFreeTypeDrawer.MergeColorOver(var merge: TFPColor; const c: TFPColor);
begin
  InternalMergeColorOver(merge,c,c.alpha);
end;

procedure TFPImageFreeTypeDrawer.MergeColorOver(var merge: TFPColor;
  const c: TFPColor; ApplyOpacity: byte);
var
  calpha: longword;
begin
  calpha := c.alpha*applyOpacity div 255;
  InternalMergeColorOver(merge,c,calpha);
end;

procedure TFPImageFreeTypeDrawer.UnclippedDrawPixel(x, y: integer; const c: TFPColor);
var
  merge: TFPColor;
begin
  if c.alpha = 0 then exit;
  MoveToPixel(x,y);
  if c.alpha = $ffff then
    SetCurrentColorAndMoveRight(c)
  else
  begin
    merge := GetCurrentColor;
    MergeColorOver(merge,c);
    SetCurrentColorAndMoveRight(merge);
  end;
end;

procedure TFPImageFreeTypeDrawer.DrawPixelAndMoveRight(const c: TFPColor; applyOpacity: byte);
var
  merge: TFPColor;
  calpha: longword;
begin
  calpha := c.alpha*applyOpacity div 255;
  if calpha = 0 then
  begin
    MoveRight;
    exit;
  end;
  if calpha = $ffff then
    SetCurrentColorAndMoveRight(c)
  else
  begin
    merge := GetCurrentColor;
    InternalMergeColorOver(merge,c,calpha);
    SetCurrentColorAndMoveRight(merge);
  end;
end;

procedure TFPImageFreeTypeDrawer.DrawPixelAndMoveRight(const c: TFPColor);
var
  merge: TFPColor;
begin
  if (c.alpha = 0) then
  begin
    MoveRight;
    exit;
  end;
  if (c.alpha = $ffff) then
    SetCurrentColorAndMoveRight(c)
  else
  begin
    merge := GetCurrentColor;
    InternalMergeColorOver(merge,c,c.alpha);
    SetCurrentColorAndMoveRight(merge);
  end;
end;

procedure TFPImageFreeTypeDrawer.SetVertLine(x, y1, y2: integer;
  const c: TFPColor);
var y: integer;
begin
  with GetClipRect do
  begin
    if (x < Left) or (x >= Right) then exit;
    if (y1 > y2) then
    begin
      y := y1;
      y1:= y2;
      y2 := y;
    end;
    if y1 < Top then y1 := Top;
    if y2 >= Bottom then y2 := Bottom-1;
  end;
  for y := y1 to y2 do
  begin
    MoveToPixel(x,y1);
    SetCurrentColorAndMoveRight(c);
  end;
end;

procedure TFPImageFreeTypeDrawer.DrawVertLine(x, y1, y2: integer; const c: TFPColor
  );
var y: integer;
begin
  with GetClipRect do
  begin
    if (x < Left) or (x >= Right) then exit;
    if (y1 > y2) then
    begin
      y := y1;
      y1:= y2;
      y2 := y;
    end;
    if y1 < Top then y1 := Top;
    if y2 >= Bottom then y2 := Bottom-1;
  end;
  for y := y1 to y2 do
    UnclippedDrawPixel(x,y, c);
end;

procedure TFPImageFreeTypeDrawer.SetHorizLine(x1, y, x2: integer; const c: TFPColor);
var i: integer;
begin
  with GetClipRect do
  begin
    if (y < Top) or (y >= Bottom) then exit;
    if (x1 > x2) then
    begin
      i := x1;
      x1:= x2;
      x2 := i;
    end;
    if x1 < Left then x1 := Left;
    if x2 >= Right then x2 := Right-1;
  end;
  MoveToPixel(x1,y);
  i := x2-x1+1;
  while i > 0 do
  begin
    SetCurrentColorAndMoveRight(c);
    dec(i);
  end;
end;

procedure TFPImageFreeTypeDrawer.DrawHorizLine(x1, y, x2: integer;
  const c: TFPColor);
var i: integer;
begin
  with GetClipRect do
  begin
    if (y < Top) or (y >= Bottom) then exit;
    if (x1 > x2) then
    begin
      i := x1;
      x1:= x2;
      x2 := i;
    end;
    if x1 < Left then x1 := Left;
    if x2 >= Right then x2 := Right-1;
  end;
  MoveToPixel(x1,y);
  i := x2-x1+1;
  while i > 0 do
  begin
    DrawPixelAndMoveRight(c);
    dec(i);
  end;
end;

procedure TFPImageFreeTypeDrawer.FillPixels(const c: TFPColor);
var yb: integer;
begin
  with GetClipRect do
  begin
    for yb := Top to Bottom-1 do
      SetHorizLine(Left,yb,Right-1,c);
  end;
end;

procedure TFPImageFreeTypeDrawer.ClearTypePixel(x, y: integer; Cr, Cg, Cb: byte; const Color: TFPColor);
begin
  with GetClipRect do
    if (x < Left) or (y < Top) or (x >= Right) or (y >= Bottom) then exit;

  MoveToPixel(x,y);
  ClearTypePixelAndMoveRight(Cr,Cg,Cb,Color);
end;

procedure TFPImageFreeTypeDrawer.ClearTypePixelAndMoveRight(Cr, Cg, Cb: byte;
  const Color: TFPColor);
var merge,mergeClearType: TFPColor;
    acc: longword;
    keep,dont_keep: word;
begin
  Cr := Cr*(color.alpha+1) shr 16;
  Cg := Cg*(color.alpha+1) shr 16;
  Cb := Cb*(color.alpha+1) shr 16;
  acc := Cr+Cg+Cb;
  if acc = 0 then exit;

  merge := GetCurrentColor;
  mergeClearType.red := (merge.red * (not byte(Cr)) +
                color.red * Cr + 128) div 255;
  mergeClearType.green := (merge.green * (not byte(Cg)) +
                color.green * Cg + 128) div 255;
  mergeClearType.blue := (merge.blue * (not byte(Cb)) +
                color.blue * Cb + 128) div 255;
  mergeClearType.alpha := merge.alpha;

  if (mergeClearType.alpha = $ffff) then
    SetCurrentColorAndMoveRight(mergeClearType)
  else
  begin
    if Cg <> 0 then
      MergeColorOver(merge,color,Cg);
    dont_keep := mergeClearType.alpha shr 1;
    if dont_keep > 0 then
    begin
      keep := 32767 - dont_keep;
      merge.red := (merge.red * keep + mergeClearType.red * dont_keep) div 32767;
      merge.green := (merge.green * keep + mergeClearType.green * dont_keep) div 32767;
      merge.blue := (merge.blue * keep + mergeClearType.blue * dont_keep) div 32767;
      merge.alpha := mergeClearType.alpha + ((not mergeClearType.alpha)*merge.alpha div 65535);
    end;
    SetCurrentColorAndMoveRight(merge);
  end;
end;

procedure TFPImageFreeTypeDrawer.MoveToPixel(x, y: integer);
begin
  FCurX := x;
  FCurY := y;
end;

function TFPImageFreeTypeDrawer.GetCurrentColor: TFPColor;
begin
  result := FImage.Colors[FCurX,FCurY];
end;

procedure TFPImageFreeTypeDrawer.SetCurrentColorAndMoveRight(
  const AColor: TFPColor);
begin
  FImage.Colors[FCurX,FCurY] := AColor;
  Inc(FCurX);
end;

procedure TFPImageFreeTypeDrawer.MoveRight;
begin
  inc(FCurX);
end;

function TFPImageFreeTypeDrawer.GetClipRect: TRect;
begin
  result := rect(0,0,FImage.Width,FImage.Height);
end;

procedure TFPImageFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    c: TFPColor;
    tempValue: byte;
begin
  //ensure rendering in bounds
  with GetClipRect do
    if (y < Top) or (y >= Bottom) or (x < Left) or (x > Right-tx) then exit;

  c := FColor;
  psrc := pbyte(data);

  MoveToPixel(x,y);
  inc(psrc,tx);
  while tx > 0 do
  begin
    tempValue := (psrc-tx)^;
    if tempValue <> 0 then
      DrawPixelAndMoveRight(c,tempValue)
    else
      MoveRight;
    dec(tx);
  end;
end;

procedure TFPImageFreeTypeDrawer.RenderDirectlyClearType(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    Cr,Cg,Cb: byte;
begin
  //ClearType position in third of pixels horizontally (multiple of 3)
  x := x div 3;
  tx := tx div 3;
  //ensure rendering in bounds
  with GetClipRect do
    if (y < Top) or (y >= Bottom) or (x < Left) or (x > Right-tx) then exit;
  if tx=0 then exit;

  psrc := pbyte(data);
  Cr := (psrc^ + psrc^ + (psrc+1)^) div 3;
  Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
  if tx > 1 then
    Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
  else
    Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;

  MoveToPixel(x,y);
  if Cr+Cg+Cb <> 0 then
    ClearTypePixelAndMoveRight(Cr,Cg,Cb, FColor)
  else
    MoveRight;
  inc(psrc,3);
  for xb := 1 to tx-2 do
  begin
    Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
    Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
    if Cr+Cg+Cb <> 0 then
      ClearTypePixelAndMoveRight(Cr,Cg,Cb, FColor)
    else
      MoveRight;
    inc(psrc,3);
  end;
  if tx > 1 then
  begin
    Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
    Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    if Cr+Cg+Cb <> 0 then
      ClearTypePixelAndMoveRight(Cr,Cg,Cb, FColor);
  end;
end;

procedure TFPImageFreeTypeDrawer.InternalMergeColorOver(var merge: TFPColor;
  const c: TFPColor; calpha: word);
var
  a1f, a2f, a12, a12m: cardinal;
begin
  if calpha = 0 then exit;
  a12  := 65534 - ((not merge.alpha) * (not calpha) shr 16);
  a12m := a12 shr 1;

  a1f := merge.alpha * (not calpha) shr 16;
  a2f := calpha - (calpha shr 15);

  merge.red := (merge.red * a1f + c.red * a2f + a12m) div a12;
  merge.green := (merge.green * a1f + c.green * a2f + a12m) div a12;
  merge.blue := (merge.blue * a1f + c.blue * a2f + a12m) div a12;
  merge.alpha := a12 + (a12 shr 15);
end;

constructor TFPImageFreeTypeDrawer.Create(AImage: TFPCustomImage);
begin
  ClearTypeRGBOrder:= true;
  FImage := AImage;
end;

procedure TFPImageFreeTypeDrawer.DrawPixel(x, y: integer; const c: TFPColor
  );
begin
  with GetClipRect do
    if (x < Left) or (y < Top) or (x >= Right) or (y >= Bottom) then exit;
  UnclippedDrawPixel(x,y,c);
end;

procedure TFPImageFreeTypeDrawer.DrawText(AText: string; AFont: TFreeTypeRenderableFont; x, y: single;
  AColor: TFPColor);
begin
  FColor := AColor;
  if AFont.ClearType then
    AFont.RenderText(AText, x, y, GetClipRect, @RenderDirectlyClearType)
  else
    AFont.RenderText(AText, x, y, GetClipRect, @RenderDirectly);
end;

destructor TFPImageFreeTypeDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TFPImageFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var f: TFreeTypeFont;
begin
  if AFont is TFreeTypeFont then
  begin
    f := TFreeTypeFont(AFont);
    FColor := AColor;
    if AFont.ClearType then
      f.RenderGlyph(AGlyph, x, y, GetClipRect, @RenderDirectlyClearType)
    else
      f.RenderGlyph(AGlyph, x, y, GetClipRect, @RenderDirectly);
  end;
end;

{ TFPImageWithScanlineFreeTypeDrawer }

procedure TFPImageWithScanlineFreeTypeDrawer.MoveToPixel(x, y: integer);
begin
  FCurrentColor:= TFPMemoryImageWithScanline(FImage).ScanLine[y]+x;
end;

function TFPImageWithScanlineFreeTypeDrawer.GetCurrentColor: TFPColor;
begin
  result := FCurrentColor^;
end;

procedure TFPImageWithScanlineFreeTypeDrawer.SetCurrentColorAndMoveRight(
  const AColor: TFPColor);
begin
  FCurrentColor^ := AColor;
  inc(FCurrentColor);
end;

procedure TFPImageWithScanlineFreeTypeDrawer.MoveRight;
begin
  inc(FCurrentColor);
end;

function TFPImageWithScanlineFreeTypeDrawer.GetClipRect: TRect;
begin
  result := rect(0,0,FImage.Width,FImage.Height);
end;

constructor TFPImageWithScanlineFreeTypeDrawer.Create(AImage: TFPCustomImage);
begin
  inherited Create(AImage);
  if not (AImage is TFPMemoryImageWithScanline) then
    raise Exception.Create('Scanline not available');
end;

{ TFPMemoryImageWithScanline }

function TFPMemoryImageWithScanline.GetScanline(y: integer): PFPColor;
begin
  if (y < 0) or (y >= Height) then
    raise ERangeError.Create('Scanline out of bounds');
  result := PFPColor(FData)+(y*Width);
end;

procedure TFPMemoryImageWithScanline.SetUsePalette(Value: boolean);
begin
  if Value then
    raise Exception.Create('Palette not supported with scanlines');
end;

end.


