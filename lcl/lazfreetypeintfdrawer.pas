unit LazFreeTypeIntfDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, EasyLazFreeType, IntfGraphics, FPimage;

type
  { TIntfFreeTypeDrawer }

  TIntfFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FColor: TFPColor;
  protected
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    procedure MergeColorOver(var merge: TFPColor; const c: TFPColor); inline;
    procedure ClearTypePixel(x,y: integer; Cr,Cg,Cb: byte; Color: TFPColor);
  public
    Destination: TLazIntfImage;
    ClearTypeRGBOrder: boolean;
    constructor Create(ADestination: TLazIntfImage);
    procedure ClippedDrawPixel(x,y: integer; const c: TFPColor);
    procedure DrawPixel(x,y: integer; const c: TFPColor);
    procedure DrawVertLine(x,y1,y2: integer; const c: TFPColor);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor; AOpacity: Byte); override; overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload;
    destructor Destroy; override;
  end;

implementation

uses LCLType;

{ TIntfFreeTypeDrawer }

procedure TIntfFreeTypeDrawer.MergeColorOver(var merge: TFPColor; const c: TFPColor);
var
  a1f, a2f, a12, a12m: cardinal;
begin
  a12  := 65534 - ((not merge.alpha) * (not c.alpha) shr 16);
  a12m := a12 shr 1;

  a1f := merge.alpha * (not c.alpha) shr 16;
  a2f := c.alpha - (c.alpha shr 15);

  merge.red := (merge.red * a1f + c.red * a2f + a12m) div a12;
  merge.green := (merge.green * a1f + c.green * a2f + a12m) div a12;
  merge.blue := (merge.blue * a1f + c.blue * a2f + a12m) div a12;
  merge.alpha := a12 + (a12 shr 15);
end;

procedure TIntfFreeTypeDrawer.DrawPixel(x, y: integer; const c: TFPColor);
var
  merge: TFPColor;
begin
  if c.alpha = 0 then exit;
  if c.alpha = $ffff then
    Destination.Colors[x,y] := c
  else
  begin
    merge := Destination.Colors[x,y];
    MergeColorOver(merge,c);
    Destination.Colors[x,y] := merge;
  end;
end;

procedure TIntfFreeTypeDrawer.DrawVertLine(x, y1, y2: integer; const c: TFPColor
  );
var y: integer;
begin
  if (x < 0) or (x >= Destination.Width) then exit;
  if (y1 > y2) then
  begin
    y := y1;
    y1:= y2;
    y2 := y;
  end;
  if y1 < 0 then y1 := 0;
  if y2 >= Destination.Height then y2 := Destination.Height-1;
  for y := y1 to y2 do
    DrawPixel(x,y, c);
end;

procedure TIntfFreeTypeDrawer.ClearTypePixel(x, y: integer; Cr, Cg, Cb: byte; Color: TFPColor);
var merge,mergeClearType: TFPColor;
    acc: longword;
    keep,dont_keep: word;
begin
  Cr := Cr*color.alpha div 65535;
  Cg := Cg*color.alpha div 65535;
  Cb := Cb*color.alpha div 65535;
  acc := Cr+Cg+Cb;
  if acc = 0 then exit;

  merge := Destination.Colors[x,y];
  mergeClearType.red := (merge.red * (not byte(Cr)) +
                color.red * Cr + 128) div 255;
  mergeClearType.green := (merge.green * (not byte(Cg)) +
                color.green * Cg + 128) div 255;
  mergeClearType.blue := (merge.blue * (not byte(Cb)) +
                color.blue * Cb + 128) div 255;
  mergeClearType.alpha := merge.alpha;

  if (mergeClearType.alpha = $ffff) then
    Destination.Colors[x,y]:= mergeClearType
  else
  begin
    if Cg <> 0 then
    begin
      Color.alpha := Color.alpha*Cg div 255;
      MergeColorOver(merge,color);
    end;
    dont_keep := mergeClearType.alpha shr 1;
    if dont_keep > 0 then
    begin
      keep := 32767 - dont_keep;
      merge.red := (merge.red * keep + mergeClearType.red * dont_keep) div 32767;
      merge.green := (merge.green * keep + mergeClearType.green * dont_keep) div 32767;
      merge.blue := (merge.blue * keep + mergeClearType.blue * dont_keep) div 32767;
      merge.alpha := mergeClearType.alpha + ((not mergeClearType.alpha)*merge.alpha div 65535);
    end;
    Destination.Colors[x,y] := merge;
  end;
end;

procedure TIntfFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    c: TFPColor;
begin
  if Destination <> nil then
  begin
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;

    c := FColor;
    psrc := pbyte(data);
    while tx > 0 do
    begin
      c.alpha:= FColor.alpha * psrc^ div 255;
      DrawPixel(x,y,c);
      inc(psrc);
      inc(x);
      dec(tx);
    end;
  end;
end;

procedure TIntfFreeTypeDrawer.RenderDirectlyClearType(x, y, tx: integer; data: pointer);
var xb: integer;
    psrc: pbyte;
    Cr,Cg,Cb: byte;
begin
  if Destination <> nil then
  begin
    //ClearType position in third of pixels horizontally (multiple of 3)
    x := x div 3;
    tx := tx div 3;
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;
    if tx=0 then exit;

    psrc := pbyte(data);
    Cr := (psrc^ + psrc^ + (psrc+1)^) div 3;
    Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
    if tx > 1 then
      Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3
    else
      Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
    ClearTypePixel(x,y,Cr,Cg,Cb, FColor);
    inc(x);
    inc(psrc,3);
    for xb := 1 to tx-2 do
    begin
      Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
      ClearTypePixel(x,y,Cr,Cg,Cb, FColor);
      inc(x);
      inc(psrc,3);
    end;
    if tx > 1 then
    begin
      Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
      Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
      Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
      ClearTypePixel(x,y,Cr,Cg,Cb, FColor);
    end;
  end;
end;

constructor TIntfFreeTypeDrawer.Create(ADestination: TLazIntfImage);
begin
  Destination := ADestination;
  ClearTypeRGBOrder:= true;
end;

procedure TIntfFreeTypeDrawer.ClippedDrawPixel(x, y: integer; const c: TFPColor
  );
begin
  if (x < 0) or (y < 0) or (x >= Destination.Width) or (y >= Destination.Height) then exit;
  DrawPixel(x,y,c);
end;

procedure TIntfFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor; AOpacity: Byte);
var col: TFPColor;
begin
  col := AColor;
  col.alpha := AOpacity + (AOpacity shl 8);
  DrawText(AText, AFont, x,y, col);
end;

procedure TIntfFreeTypeDrawer.DrawText(AText: string; AFont: TFreeTypeRenderableFont; x, y: single;
  AColor: TFPColor);
begin
  FColor := AColor;
  if AFont.ClearType then
    AFont.RenderText(AText, x, y, rect(0,0,Destination.Width,Destination.Height), @RenderDirectlyClearType)
  else
    AFont.RenderText(AText, x, y, rect(0,0,Destination.Width,Destination.Height), @RenderDirectly);
end;

destructor TIntfFreeTypeDrawer.Destroy;
begin
  inherited Destroy;
end;

end.


