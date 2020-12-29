unit LazFreeTypeIntfDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPimage,
  // LazUtils
  GraphType, EasyLazFreeType,
  // LCL
  Graphics, IntfGraphics;

type
  TLazIntfImageGetPixelAtProc = procedure(p: pointer; out Color: TFPColor);
  TLazIntfImageSetPixelAtProc = procedure(p: pointer; const Color: TFPColor);
  TLazIntfHorizLineProc = procedure(x1,y,x2: integer; const Color: TFPColor) of object;

  { TIntfFreeTypeDrawer }

  TIntfFreeTypeDrawer = class(TFreeTypeDrawer)
  private
    FColor: TFPColor;
    FDestination: TLazIntfImage;
    FHasPixelAtProc: boolean;
    FGetPixelAtProc: TLazIntfImageGetPixelAtProc;
    FSetPixelAtProc: TLazIntfImageSetPixelAtProc;
    FPixelSizeInBytes: longword;
    FWidth, FHeight: integer;
    procedure SetDestination(AValue: TLazIntfImage);
  protected
    procedure RenderDirectly(x, y, tx: integer; data: pointer);
    procedure RenderDirectlyClearType(x, y, tx: integer; data: pointer);
    procedure InternalMergeColorOver(var merge: TFPColor; const c: TFPColor; calpha: word); inline;
    procedure MergeColorOver(var merge: TFPColor; const c: TFPColor); inline;
    procedure MergeColorOver(var merge: TFPColor; const c: TFPColor; ApplyOpacity: byte); inline;
    procedure DrawPixelAt(p: pointer; const c: TFPColor);
    procedure DrawPixelAt(p: pointer; const c: TFPColor; applyOpacity: byte);
    procedure ClearTypePixelAt(p: pointer; Cr,Cg,Cb: byte; const Color: TFPColor);
    function UnclippedGetPixelAddress(x, y: integer): pointer; inline;
    function ClippedGetPixelAddress(x, y: integer): pointer; inline;
  public
    ClearTypeRGBOrder: boolean;
    constructor Create(ADestination: TLazIntfImage);
    procedure ClippedDrawPixel(x,y: integer; const c: TFPColor);
    procedure UnclippedDrawPixel(x,y: integer; const c: TFPColor);
    procedure ClippedClearTypePixel(x,y: integer; Cr,Cg,Cb: byte; const Color: TFPColor);
    procedure UnclippedClearTypePixel(x,y: integer; Cr,Cg,Cb: byte; const Color: TFPColor);
    procedure DrawVertLine(x,y1,y2: integer; const c: TFPColor);
    procedure SetHorizLine(x1,y,x2: integer; const c: TFPColor);
    procedure DrawHorizLine(x1,y,x2: integer; const c: TFPColor);
    procedure FillRect(x,y,x2,y2: integer; const c: TFPColor; ASetPixels: boolean = True);
    procedure FillPixels(const c: TFPColor; ASetPixels: boolean = True);
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override; overload;
    procedure DrawGlyph(AGlyph: integer; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); override; overload;
    property Destination: TLazIntfImage read FDestination write SetDestination;
    destructor Destroy; override;
  end;

implementation

type
  PFPColorBytes = ^TFPColorBytes;
  TFPColorBytes = record
    {$ifdef ENDIAN_LITTLE}
    Rl, Rh, Gl, Gh, Bl, Bh, Al, Ah: Byte;
    {$else}
    Rh, Rl, Gh, Gl, Bh, Bl, Ah, Al: Byte;
    {$endif}
  end;

  PFourBytes = ^TFourBytes;
  TFourBytes = record
    B0, B1, B2, B3: Byte;
  end;

{ TIntfFreeTypeDrawer }

function TIntfFreeTypeDrawer.ClippedGetPixelAddress(x, y: integer): pointer;
begin
  if (x < 0) or (x >= Destination.Width) then
    raise FPImageException.CreateFmt(ErrorText[StrInvalidIndex],[ErrorText[StrImageX],x]);
  if (y < 0) or (y >= Destination.Height) then
    raise FPImageException.CreateFmt(ErrorText[StrInvalidIndex],[ErrorText[StrImageY],y]);

  result := pbyte(Destination.GetDataLineStart(y))+(x*FPixelSizeInBytes);
end;

procedure InternalGetPixelAtWithoutAlphaRGB(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B0;
    TFPColorBytes(color).Rl := B0;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B2;
    TFPColorBytes(color).Bl := B2;
    color.alpha := $ffff;
  end;
end;

procedure InternalSetPixelAtWithoutAlphaRGB(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B0 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B2 := TFPColorBytes(color).Bh;
  end;
end;

procedure InternalGetPixelAtWithoutAlphaBGR(p: pointer; out Color: TFPColor);
{$IFDEF CPUI386} assembler; {$ASMMODE INTEL}
asm
  mov cl, [eax+2]
  mov [edx], cl
  mov [edx+1], cl
  mov cl, [eax+1]
  mov [edx+2], cl
  mov [edx+3], cl
  mov cl, [eax]
  mov [edx+4], cl
  mov [edx+5], cl
  xor ecx, ecx
  not ecx
  mov [edx+6], cx
end;
{$ELSE}
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B2;
    TFPColorBytes(color).Rl := B2;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B0;
    TFPColorBytes(color).Bl := B0;
    color.alpha := $ffff;
  end;
end;
{$ENDIF}

procedure InternalSetPixelAtWithoutAlphaBGR(p: pointer; const Color: TFPColor);
{$IFDEF CPUI386} assembler; {$ASMMODE INTEL}
asm
  mov cl, [edx+1]
  mov [eax+2], cl
  mov cl, [edx+3]
  mov [eax+1], cl
  mov cl, [edx+5]
  mov [eax], cl
end;
{$ELSE}
begin
  with PFourBytes(p)^ do
  begin
    B2 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B0 := TFPColorBytes(color).Bh;
  end;
end;
{$ENDIF}

procedure InternalGetPixelAtWithAlphaRGBA(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B0;
    TFPColorBytes(color).Rl := B0;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B2;
    TFPColorBytes(color).Bl := B2;
    TFPColorBytes(color).Ah := B3;
    TFPColorBytes(color).Al := B3;
  end;
end;

procedure InternalSetPixelAtWithAlphaRGBA(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B0 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B2 := TFPColorBytes(color).Bh;
    B3 := TFPColorBytes(color).Ah;
  end;
end;

procedure InternalGetPixelAtWithAlphaBGRA(p: pointer; out Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    TFPColorBytes(color).Rh := B2;
    TFPColorBytes(color).Rl := B2;
    TFPColorBytes(color).Gh := B1;
    TFPColorBytes(color).Gl := B1;
    TFPColorBytes(color).Bh := B0;
    TFPColorBytes(color).Bl := B0;
    TFPColorBytes(color).Ah := B3;
    TFPColorBytes(color).Al := B3;
  end;
end;

procedure InternalSetPixelAtWithAlphaBGRA(p: pointer; const Color: TFPColor);
begin
  with PFourBytes(p)^ do
  begin
    B2 := TFPColorBytes(color).Rh;
    B1 := TFPColorBytes(color).Gh;
    B0 := TFPColorBytes(color).Bh;
    B3 := TFPColorBytes(color).Ah;
  end;
end;

procedure TIntfFreeTypeDrawer.MergeColorOver(var merge: TFPColor; const c: TFPColor);
begin
  InternalMergeColorOver(merge,c,c.alpha);
end;

procedure TIntfFreeTypeDrawer.MergeColorOver(var merge: TFPColor;
  const c: TFPColor; ApplyOpacity: byte);
var
  calpha: longword;
begin
  calpha := c.alpha*applyOpacity div 255;
  InternalMergeColorOver(merge,c,calpha);
end;

procedure TIntfFreeTypeDrawer.UnclippedDrawPixel(x, y: integer; const c: TFPColor);
var
  merge: TFPColor;
begin
  if c.alpha = 0 then exit;
  if FHasPixelAtProc then
    DrawPixelAt(UnclippedGetPixelAddress(x,y),c)
  else
  begin
    if c.alpha = $ffff then
      Destination.Colors[x,y] := c
    else
    begin
      merge := Destination.Colors[x,y];
      MergeColorOver(merge,c);
      Destination.Colors[x,y] := merge;
    end;
  end;
end;

procedure TIntfFreeTypeDrawer.DrawPixelAt(p: pointer; const c: TFPColor; applyOpacity: byte);
var
  merge: TFPColor;
  calpha: longword;
begin
  calpha := c.alpha*applyOpacity div 255;
  if calpha = 0 then exit;
  if calpha = $ffff then
    FSetPixelAtProc(p, c)
  else
  begin
    FGetPixelAtProc(p, merge);
    InternalMergeColorOver(merge,c,calpha);
    FSetPixelAtProc(p, merge);
  end;
end;

procedure TIntfFreeTypeDrawer.DrawPixelAt(p: pointer; const c: TFPColor);
var
  merge: TFPColor;
begin
  if (c.alpha = 0) then exit;
  if (c.alpha = $ffff) then
    FSetPixelAtProc(p, c)
  else
  begin
    FGetPixelAtProc(p, merge);
    MergeColorOver(merge,c);
    FSetPixelAtProc(p, merge);
  end;
end;

procedure TIntfFreeTypeDrawer.ClippedClearTypePixel(x, y: integer; Cr, Cg,
  Cb: byte; const Color: TFPColor);
begin
  if (x < 0) or (y < 0) or (x >= Destination.Width) or (y >= Destination.Height) then exit;
  UnclippedClearTypePixel(x,y,Cr,Cg,Cb,Color);
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
    UnclippedDrawPixel(x,y, c);
end;

procedure TIntfFreeTypeDrawer.SetHorizLine(x1, y, x2: integer; const c: TFPColor);
var i: integer;
  pdest: pbyte;
  step: longword;
begin
  if (y < 0) or (y >= Destination.Height) then exit;
  if (x1 > x2) then
  begin
    i := x1;
    x1:= x2;
    x2 := i;
  end;
  if x1 < 0 then x1 := 0;
  if x2 >= Destination.Width then x2 := Destination.Width-1;
  if FHasPixelAtProc then
  begin
    pdest := UnclippedGetPixelAddress(x1,y);
    step := FPixelSizeInBytes;
    i := x2-x1+1;
    while i > 0 do
    begin
      FSetPixelAtProc(pdest,c);
      inc(pdest,step);
      dec(i);
    end;
  end else
    for i := x1 to x2 do
      Destination.Colors[i,y] := c;
end;

procedure TIntfFreeTypeDrawer.DrawHorizLine(x1, y, x2: integer;
  const c: TFPColor);
var i: integer;
  pdest: pbyte;
  step: longword;
begin
  if (y < 0) or (y >= Destination.Height) then exit;
  if (x1 > x2) then
  begin
    i := x1;
    x1:= x2;
    x2 := i;
  end;
  if x1 < 0 then x1 := 0;
  if x2 >= Destination.Width then x2 := Destination.Width-1;
  if FHasPixelAtProc then
  begin
    pdest := UnclippedGetPixelAddress(x1,y);
    step := FPixelSizeInBytes;
    i := x2-x1+1;
    while i > 0 do
    begin
      DrawPixelAt(pdest,c);
      inc(pdest,step);
      dec(i);
    end;
  end else
    for i := x1 to x2 do
      UnclippedDrawPixel(i,y,c);
end;

procedure TIntfFreeTypeDrawer.FillRect(x, y, x2, y2: integer;
  const c: TFPColor; ASetPixels: boolean);
var yb,xb: integer;
  HorizLineProc: TLazIntfHorizLineProc;
begin
  if x2 < x then
  begin
    xb:= x;
    x := x2;
    x2 := xb;
  end;
  if x < 0 then x := 0;
  if x2 > Destination.Width then x2 := Destination.Width;
  if (x >= Destination.Width) or (x2 <= 0) then exit;
  if y2 < y then
  begin
    yb := y;
    y := y2;
    y2 := yb;
  end;
  if y < 0 then y := 0;
  if y2 > Destination.Height then y2 := Destination.Height;
  if ASetPixels then HorizLineProc := @SetHorizLine else HorizLineProc := @DrawHorizLine;
  for yb := y to y2-1 do
    HorizLineProc(x,yb,x2-1,c);
end;

procedure TIntfFreeTypeDrawer.FillPixels(const c: TFPColor; ASetPixels: boolean = True);
var yb: integer;
  HorizLineProc: TLazIntfHorizLineProc;
begin
  if ASetPixels then HorizLineProc := @SetHorizLine else HorizLineProc := @DrawHorizLine;
  for yb := 0 to Destination.Height-1 do
    HorizLineProc(0,yb,Destination.Width-1,c);
end;

procedure TIntfFreeTypeDrawer.UnclippedClearTypePixel(x, y: integer; Cr, Cg, Cb: byte; const Color: TFPColor);
var merge,mergeClearType: TFPColor;
    acc: longword;
    keep,dont_keep: word;
begin
  Cr := Cr*(color.alpha+1) shr 16;
  Cg := Cg*(color.alpha+1) shr 16;
  Cb := Cb*(color.alpha+1) shr 16;
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
    Destination.Colors[x,y] := merge;
  end;
end;

procedure TIntfFreeTypeDrawer.ClearTypePixelAt(p: pointer; Cr, Cg, Cb: byte;
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

  FGetPixelAtProc(p, merge);
  mergeClearType.red := (merge.red * (not byte(Cr)) +
                color.red * Cr + 128) div 255;
  mergeClearType.green := (merge.green * (not byte(Cg)) +
                color.green * Cg + 128) div 255;
  mergeClearType.blue := (merge.blue * (not byte(Cb)) +
                color.blue * Cb + 128) div 255;
  mergeClearType.alpha := merge.alpha;

  if (mergeClearType.alpha = $ffff) then
    FSetPixelAtProc(p, mergeClearType)
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
    FSetPixelAtProc(p, merge);
  end;
end;

function TIntfFreeTypeDrawer.UnclippedGetPixelAddress(x, y: integer): pointer;
begin
  result := pbyte(Destination.GetDataLineStart(y))+(x*FPixelSizeInBytes);
end;

procedure TIntfFreeTypeDrawer.SetDestination(AValue: TLazIntfImage);
var CanBeOptimized: boolean;
    RedShiftInBytes,GreenShiftInBytes,BlueShiftInBytes,AlphaShiftInBytes: integer;
begin
  if FDestination=AValue then Exit;
  FDestination := AValue;

  FGetPixelAtProc := nil;
  FSetPixelAtProc := nil;

  if FDestination = nil then
  begin
    FWidth := 0;
    FHeight := 0;
  end else
  begin
    FWidth := FDestination.Width;
    FHeight := FDestination.Height;

    with Destination.DataDescription do
      CanBeOptimized := (BitsPerPixel and 7 = 0) and
      (Format = ricfRGBA) and (RedPrec = 8) and (GreenPrec = 8) and (BluePrec = 8) and
      (RedShift and 7 = 0) and (GreenPrec and 7 = 0) and (BluePrec and 7 = 0) and
      (((AlphaPrec = 8) and (AlphaShift and 7 = 0)) or (AlphaPrec = 0));

    if CanBeOptimized then
    begin
      FPixelSizeInBytes := Destination.DataDescription.BitsPerPixel div 8;

      RedShiftInBytes := Destination.DataDescription.RedShift div 8;
      GreenShiftInBytes := Destination.DataDescription.GreenShift div 8;
      BlueShiftInBytes := Destination.DataDescription.BlueShift div 8;
      AlphaShiftInBytes := Destination.DataDescription.AlphaShift div 8;

      if Destination.DataDescription.ByteOrder = riboMSBFirst then
      begin
        RedShiftInBytes := FPixelSizeInBytes-1 - RedShiftInBytes;
        GreenShiftInBytes := FPixelSizeInBytes-1 - GreenShiftInBytes;
        BlueShiftInBytes := FPixelSizeInBytes-1 - BlueShiftInBytes;
        AlphaShiftInBytes := FPixelSizeInBytes-1 - AlphaShiftInBytes;
      end;

      if Destination.DataDescription.AlphaPrec = 0 then
      begin
        if (RedShiftInBytes = 0) and (GreenShiftInBytes = 1) and
           (BlueShiftInBytes = 2) then
        begin
          FGetPixelAtProc := @InternalGetPixelAtWithoutAlphaRGB;
          FSetPixelAtProc := @InternalSetPixelAtWithoutAlphaRGB;
        end else
        if (RedShiftInBytes = 2) and (GreenShiftInBytes = 1) and
           (BlueShiftInBytes = 0) then
        begin
          FGetPixelAtProc := @InternalGetPixelAtWithoutAlphaBGR;
          FSetPixelAtProc := @InternalSetPixelAtWithoutAlphaBGR;
        end;
      end else
      begin
        if (RedShiftInBytes = 0) and (GreenShiftInBytes = 1) and
           (BlueShiftInBytes = 2) then
        begin
          FGetPixelAtProc := @InternalGetPixelAtWithAlphaRGBA;
          FSetPixelAtProc := @InternalSetPixelAtWithAlphaRGBA;
        end else
        if (RedShiftInBytes = 2) and (GreenShiftInBytes = 1) and
           (BlueShiftInBytes = 0) then
        begin
          FGetPixelAtProc := @InternalGetPixelAtWithAlphaBGRA;
          FSetPixelAtProc := @InternalSetPixelAtWithAlphaBGRA;
        end;
      end;
    end;
  end;

  FHasPixelAtProc := (FGetPixelAtProc<>nil) and (FSetPixelAtProc <> nil);
end;

procedure TIntfFreeTypeDrawer.RenderDirectly( x,y,tx: integer;
                          data: pointer );
var psrc: pbyte;
    c: TFPColor;
    pdest: pbyte;
    step: longword;
    tempValue: byte;
begin
  if Destination <> nil then
  begin
    //ensure rendering in bounds
    if (y < 0) or (y >= Destination.height) or (x < 0) or (x > Destination.width-tx) then exit;

    c := FColor;
    psrc := pbyte(data);

    if FHasPixelAtProc then
    begin
      step := FPixelSizeInBytes;
      pdest := UnclippedGetPixelAddress(x,y);
      inc(psrc,tx);
      while tx > 0 do
      begin
        tempValue := (psrc-tx)^;
        if tempValue <> 0 then
          DrawPixelAt(pdest,c,tempValue);
        inc(pdest,step);
        dec(tx);
      end;
    end else
    while tx > 0 do
    begin
      tempValue := psrc^;
      if tempValue <> 0 then
      begin
        c.alpha:= FColor.alpha * tempValue div 255;
        UnclippedDrawPixel(x,y,c);
      end;
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
    pdest: pbyte;
    step: longword;
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

    if FHasPixelAtProc then
    begin
      step := FPixelSizeInBytes;
      pdest := UnclippedGetPixelAddress(x,y);
      if Cr+Cg+Cb <> 0 then
        ClearTypePixelAt(pdest,Cr,Cg,Cb, FColor);
      inc(pdest,step);
      inc(psrc,3);
      for xb := 1 to tx-2 do
      begin
        Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
        Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
        Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
        if Cr+Cg+Cb <> 0 then
          ClearTypePixelAt(pdest,Cr,Cg,Cb, FColor);
        inc(pdest,step);
        inc(psrc,3);
      end;
      if tx > 1 then
      begin
        Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
        Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
        Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
        if Cr+Cg+Cb <> 0 then
          ClearTypePixelAt(pdest,Cr,Cg,Cb, FColor);
      end;
    end else
    begin
      if Cr+Cg+Cb <> 0 then
        UnclippedClearTypePixel(x,y,Cr,Cg,Cb, FColor);
      inc(x);
      inc(psrc,3);
      for xb := 1 to tx-2 do
      begin
        Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
        Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
        Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+3)^) div 3;
        if Cr+Cg+Cb <> 0 then
          UnclippedClearTypePixel(x,y,Cr,Cg,Cb, FColor);
        inc(x);
        inc(psrc,3);
      end;
      if tx > 1 then
      begin
        Cr := ((psrc-1)^+ psrc^ + (psrc+1)^) div 3;
        Cg := (psrc^+ (psrc+1)^ + (psrc+2)^) div 3;
        Cb := ((psrc+1)^ + (psrc+2)^ + (psrc+2)^) div 3;
        if Cr+Cg+Cb <> 0 then
          UnclippedClearTypePixel(x,y,Cr,Cg,Cb, FColor);
      end;
    end;
  end;
end;

procedure TIntfFreeTypeDrawer.InternalMergeColorOver(var merge: TFPColor;
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

constructor TIntfFreeTypeDrawer.Create(ADestination: TLazIntfImage);
begin
  Destination := ADestination;
  ClearTypeRGBOrder:= true;
end;

procedure TIntfFreeTypeDrawer.ClippedDrawPixel(x, y: integer; const c: TFPColor
  );
begin
  if (x < 0) or (y < 0) or (x >= Destination.Width) or (y >= Destination.Height) then exit;
  UnclippedDrawPixel(x,y,c);
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

procedure TIntfFreeTypeDrawer.DrawGlyph(AGlyph: integer;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
var f: TFreeTypeFont;
begin
  if AFont is TFreeTypeFont then
  begin
    f := TFreeTypeFont(AFont);
    FColor := AColor;
    if AFont.ClearType then
      f.RenderGlyph(AGlyph, x, y, rect(0,0,Destination.Width,Destination.Height), @RenderDirectlyClearType)
    else
      f.RenderGlyph(AGlyph, x, y, rect(0,0,Destination.Width,Destination.Height), @RenderDirectly);
  end;
end;

destructor TIntfFreeTypeDrawer.Destroy;
begin
  inherited Destroy;
end;

end.


