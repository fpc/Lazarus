unit customdrawn_windows7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math,
  { Graphics }
  LResources, Graphics, FPCanvas, FPImage, IntfGraphics, lazcanvas,
  { Custom Drawn }
  customdrawn_common, customdrawndrawers;

type
  TBitmapArray = array of TBitmap;
  TBitmapArrays = array of TBitmapArray;

  { TSliceScaling }

  TSliceScaling = class
  private
    BorWidth: integer;
    BorHeight: integer;
    FUseNativeStretch: boolean;
    procedure SetFUseNativeStretch(AValue: boolean);
    function InternalGetBitmapElements(bmpArrayStates: TBitmapArray;
      Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
  protected
    function GetBitmap(Filename: string; Number: integer; ALoadFromResource: Boolean = False): TBitmapArray;
    function GetBitmapPart(Source: TBitmap;
      BorderWidth, BorderHeight: integer): TBitmapArray;
    function GetBitmapElements(Filename: string;
      Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
    function GetBitmapElementsWithResource(AResourceName: string;
      Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
    function DrawBitmapNonNativeStretch(Source: TBitmapArray;
      DestWidth, DestHeight, BorderWidth, BorderHeight: integer): TBitmap;
    function DrawBitmapNativeStretch(Source: TBitmapArray;
      DestWidth, DestHeight, BorderWidth, BorderHeight: integer): TBitmap;
  public
    bmpArrays: TBitmapArrays;
    constructor Create(Filename: string;
      BorderWidth, BorderHeight, NumberOfItems: integer);
    constructor CreateWithResource(AResourceName: string;
      BorderWidth, BorderHeight, NumberOfItems: integer);
    destructor Destroy; override;
    function Draw(Width, Height, ItemIndex: integer): TBitmap;
    procedure Draw(Dest: TCanvas; Left, Top, Width, Height, ItemIndex: integer);
  public
    property UseNativeStretch: boolean read FUseNativeStretch
      write SetFUseNativeStretch default False;
    property BorderWidth: integer read BorWidth;
    property BorderHeight: integer read BorHeight;
  end;

  { TCDWin7 }

  TCDWin7 = class(TCDDrawerCommon)
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
  end;

var
  win7button: TSliceScaling;

implementation

{$R customdrawnimages/windows7.res}

{ TSliceScaling }

procedure TSliceScaling.SetFUseNativeStretch(AValue: boolean);
begin
  if FUseNativeStretch = AValue then
    Exit;
  FUseNativeStretch := AValue;
end;

function TSliceScaling.InternalGetBitmapElements(bmpArrayStates: TBitmapArray;
  Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
var
  bmpArrayParts: TBitmapArrays;
  i, tempWidth, tempHeight: integer;
begin
  if (BorderWidth * 2 > bmpArrayStates[0].Width) or (BorderWidth < 0) then
    tempWidth := Trunc(bmpArrayStates[0].Width div 2)
  else
    tempWidth := BorderWidth;

  if (BorderHeight * 2 > bmpArrayStates[0].Height) or (BorderHeight < 0) then
    tempHeight := Trunc(bmpArrayStates[0].Height div 2)
  else
    tempHeight := BorderHeight;

  SetLength(bmpArrayParts, Number, 9);
  for i := Low(bmpArrayStates) to High(bmpArrayStates) do
  begin
    bmpArrayParts[i] := GetBitmapPart(bmpArrayStates[i], tempWidth, tempHeight);
    bmpArrayStates[i].Free;
    bmpArrayStates[i] := nil;
  end;

  bmpArrayStates := nil;

  Result := bmpArrayParts;
end;

function TSliceScaling.GetBitmap(Filename: string; Number: integer; ALoadFromResource: Boolean = False): TBitmapArray;
var
  bmpArray: TBitmapArray;
  temp: TBitmap;
  i: integer;
  s: TSize;
begin
  temp := TBitmap.Create;
  if ALoadFromResource then temp.LoadFromResourceName(HInstance, Filename)
  else temp.LoadFromFile(Filename);
  s.cx := temp.Width;
  s.cy := temp.Height div Number;

  SetLength(bmpArray, Number);
  Result := bmpArray;

  for i := Low(bmpArray) to High(bmpArray) do
  begin
    bmpArray[i] := TBitmap.Create;
    bmpArray[i].Width := s.cx;
    bmpArray[i].Height := s.cy;
    bmpArray[i].Canvas.Draw(0, -s.cy * i, temp);
  end;

  temp.Free;
end;

function TSliceScaling.GetBitmapPart(Source: TBitmap;
  BorderWidth, BorderHeight: integer): TBitmapArray;
var
  bmpArray: TBitmapArray;
begin
  SetLength(bmpArray, 9);
  Result := bmpArray;

  { Top Left }
  bmpArray[0] := TBitmap.Create;
  bmpArray[0].Width := BorderWidth;
  bmpArray[0].Height := BorderHeight;
  bmpArray[0].Canvas.Draw(0, 0, Source);

  { Top Right }
  bmpArray[1] := TBitmap.Create;
  bmpArray[1].Width := BorderWidth;
  bmpArray[1].Height := BorderHeight;
  bmpArray[1].Canvas.Draw(BorderWidth - Source.Width, 0, Source);

  { Bottom Left }
  bmpArray[2] := TBitmap.Create;
  bmpArray[2].Width := BorderWidth;
  bmpArray[2].Height := BorderHeight;
  bmpArray[2].Canvas.Draw(0, BorderHeight - Source.Height, Source);

  { Bottom Right }
  bmpArray[3] := TBitmap.Create;
  bmpArray[3].Width := BorderWidth;
  bmpArray[3].Height := BorderHeight;
  bmpArray[3].Canvas.Draw(BorderWidth - Source.Width, BorderHeight -
    Source.Height, Source);

  { Center }
  bmpArray[4] := TBitmap.Create;
  bmpArray[4].Width := Source.Width - BorderWidth * 2;
  bmpArray[4].Height := Source.Height - BorderHeight * 2;
  bmpArray[4].Canvas.Draw(-BorderWidth, -BorderHeight, Source);

  { Top }
  bmpArray[5] := TBitmap.Create;
  bmpArray[5].Width := Source.Width - BorderWidth * 2;
  bmpArray[5].Height := BorderHeight;
  bmpArray[5].Canvas.Draw(-BorderWidth, 0, Source);

  { Left }
  bmpArray[6] := TBitmap.Create;
  bmpArray[6].Width := BorderWidth;
  bmpArray[6].Height := Source.Height - BorderHeight * 2;
  bmpArray[6].Canvas.Draw(0, -BorderHeight, Source);

  { Right }
  bmpArray[7] := TBitmap.Create;
  bmpArray[7].Width := BorderWidth;
  bmpArray[7].Height := Source.Height - BorderHeight * 2;
  bmpArray[7].Canvas.Draw(BorderWidth - Source.Width, -BorderHeight, Source);

  { Bottom }
  bmpArray[8] := TBitmap.Create;
  bmpArray[8].Width := Source.Width - BorderWidth * 2;
  bmpArray[8].Height := BorderHeight;
  bmpArray[8].Canvas.Draw(-BorderWidth, BorderHeight - Source.Height, Source);
end;

function TSliceScaling.GetBitmapElements(Filename: string;
  Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
var
  bmpArrayStates: TBitmapArray;
begin
  bmpArrayStates := GetBitmap(Filename, Number);
  Result := InternalGetBitmapElements(bmpArrayStates, Number, BorderWidth, BorderHeight);
end;

function TSliceScaling.GetBitmapElementsWithResource(AResourceName: string;
  Number, BorderWidth, BorderHeight: integer): TBitmapArrays;
var
  bmpArrayStates: TBitmapArray;
begin
  bmpArrayStates := GetBitmap(AResourceName, Number, True);
  Result := InternalGetBitmapElements(bmpArrayStates, Number, BorderWidth, BorderHeight);
end;

function TSliceScaling.DrawBitmapNonNativeStretch(Source: TBitmapArray;
  DestWidth, DestHeight, BorderWidth, BorderHeight: integer): TBitmap;

  procedure StretchDrawBitmapToBitmap(SourceBitmap, DestBitmap: TBitmap;
    DestWidth, DestHeight: integer);
  var
    DestIntfImage, SourceIntfImage: TLazIntfImage;
    DestCanvas: TLazCanvas;
  begin
    // Prepare the destination
    DestIntfImage := TLazIntfImage.Create(0, 0);
    DestIntfImage.LoadFromBitmap(DestBitmap.Handle, 0);

    DestCanvas := TLazCanvas.Create(DestIntfImage);

    //Prepare the source
    SourceIntfImage := TLazIntfImage.Create(0, 0);
    SourceIntfImage.LoadFromBitmap(SourceBitmap.Handle, 0);

    // Execute the stretch draw via TFPSharpInterpolation
    DestCanvas.Interpolation := TFPSharpInterpolation.Create;
    DestCanvas.StretchDraw(0, 0, DestWidth, DestHeight, SourceIntfImage);

    // Reload the image into the TBitmap
    DestBitmap.LoadFromIntfImage(DestIntfImage);

    SourceIntfImage.Free;
    DestCanvas.Interpolation.Free;
    DestCanvas.Free;
    DestIntfImage.Free;
  end;

  procedure DrawStretch(Source, Dest: TBitmap; x, y, w, h: integer);
  var
    temp: TBitmap;
  begin
    if (Source.Width <> w) or (Source.Height <> h) then
    begin
      temp := TBitmap.Create;
      temp.Width := w;
      temp.Height := h;
      StretchDrawBitmapToBitmap(Source, temp, w, h);
      dest.Canvas.Draw(x, y, temp);
      temp.Free;
    end
    else
      dest.Canvas.Draw(x, y, Source);
  end;

  procedure DrawEachPart(Source: TBitmapArray; dest: TBitmap;
    DestWidth, DestHeight, BorderWidth, BorderHeight: integer);
  begin
    // center
    if (DestWidth > BorderWidth * 2) and (DestHeight > BorderHeight * 2) then
      DrawStretch(Source[4], dest, BorderWidth, BorderHeight, DestWidth -
        2 * BorderWidth, DestHeight - 2 * BorderHeight);
    //top
    DrawStretch(Source[5], dest, BorderWidth, 0, DestWidth - 2 * BorderWidth,
      BorderHeight);
    //left
    DrawStretch(Source[6], dest, 0, BorderHeight, BorderWidth, DestHeight -
      2 * BorderHeight);
    //right
    DrawStretch(Source[7], dest, DestWidth - BorderWidth, BorderHeight,
      BorderWidth, DestHeight - 2 * BorderHeight);
    //bottom
    DrawStretch(Source[8], dest, BorderWidth, DestHeight - BorderHeight,
      DestWidth - 2 * BorderWidth, BorderHeight);
    //top left
    DrawStretch(Source[0], dest, 0, 0, BorderWidth, BorderHeight);
    //top right
    DrawStretch(Source[1], dest, DestWidth - BorderWidth, 0, BorderWidth, BorderHeight);
    //bottom left
    DrawStretch(Source[2], dest, 0, DestHeight - BorderHeight, BorderWidth,
      BorderHeight);
    //bottom right
    DrawStretch(Source[3], dest, DestWidth - BorderWidth, DestHeight -
      BorderHeight, BorderWidth, BorderHeight);
  end;

var
  temp: TBitmap;
  tempWidth, tempHeight: integer;
begin
  if (BorderWidth < 1) or (BorderHeight < 1) then
  begin
    Result := TBitmap.Create;
    Result.Width := DestWidth;
    Result.Height := DestHeight;
    StretchDrawBitmapToBitmap(Source[4], Result, DestWidth, DestHeight);
    Result.Transparent := True;
    Result.TransparentColor := clFuchsia;
    exit;
  end;

  if DestWidth < BorderWidth * 2 then
    tempWidth := BorderWidth * 2
  else
    tempWidth := DestWidth;

  if DestHeight < BorderHeight * 2 then
    tempHeight := DestHeight * 2
  else
    tempHeight := DestHeight;

  temp := TBitmap.Create;
  temp.Width := tempWidth;
  temp.Height := tempHeight;
  temp.Transparent := True;
  temp.TransparentColor := clFuchsia;
  DrawEachPart(Source, temp, tempWidth, tempHeight, BorderWidth, BorderHeight);

  if (tempWidth <> DestWidth) or (tempHeight <> DestHeight) then
  begin
    Result := TBitmap.Create;
    Result.Width := DestWidth;
    Result.Height := DestHeight;
    StretchDrawBitmapToBitmap(temp, Result, DestWidth, DestHeight);
    Result.Transparent := True;
    Result.TransparentColor := clFuchsia;
    temp.Free;
  end
  else
    Result := temp;
end;

function TSliceScaling.DrawBitmapNativeStretch(Source: TBitmapArray;
  DestWidth, DestHeight, BorderWidth, BorderHeight: integer): TBitmap;

  procedure DrawEachPart(Source: TBitmapArray; dest: TBitmap;
    DestWidth, DestHeight, BorderWidth, BorderHeight: integer);
  begin
    //center
    dest.Canvas.StretchDraw(Rect(BorderWidth, BorderHeight, DestWidth -
      BorderWidth, DestHeight - BorderHeight), Source[4]);
    //top
    dest.Canvas.StretchDraw(Rect(BorderWidth, 0, DestWidth - BorderWidth, BorderHeight),
      Source[5]);
    //left
    dest.Canvas.StretchDraw(Rect(0, BorderHeight, BorderWidth, DestHeight -
      BorderHeight),
      Source[6]);
    //right
    dest.Canvas.StretchDraw(Rect(DestWidth - BorderWidth, BorderHeight,
      DestWidth, DestHeight - BorderHeight), Source[7]);
    //bottom
    dest.Canvas.StretchDraw(Rect(BorderWidth, DestHeight - BorderHeight,
      DestWidth - BorderWidth, DestHeight), Source[8]);
    //top left
    dest.Canvas.StretchDraw(Rect(0, 0, BorderWidth, BorderHeight), Source[0]);
    //top right
    dest.Canvas.StretchDraw(Rect(DestWidth - BorderWidth, 0, DestWidth, BorderHeight),
      Source[1]);
    //bottom left
    dest.Canvas.StretchDraw(Rect(0, DestHeight - BorderHeight, BorderWidth, DestHeight),
      Source[2]);
    //bottom right
    dest.Canvas.StretchDraw(Rect(DestWidth - BorderWidth, DestHeight -
      BorderHeight, DestWidth, DestHeight), Source[3]);
  end;

var
  dest: TBitmap;
begin
  dest := TBitmap.Create;
  dest.Transparent := True;
  dest.TransparentColor := clFuchsia;
  dest.Width := DestWidth;
  dest.Height := DestHeight;
  Result := dest;

  DrawEachPart(Source, dest, DestWidth, DestHeight, BorderWidth, BorderHeight);
end;

constructor TSliceScaling.Create(Filename: string;
  BorderWidth, BorderHeight, NumberOfItems: integer);
begin
  FUseNativeStretch := False;
  BorWidth := BorderWidth;
  BorHeight := BorderHeight;
  BmpArrays := GetBitmapElements(Filename, NumberOfItems, BorderWidth, BorderHeight);
  inherited Create;
end;

constructor TSliceScaling.CreateWithResource(AResourceName: string;
  BorderWidth, BorderHeight, NumberOfItems: integer);
begin
  FUseNativeStretch := False;
  BorWidth := BorderWidth;
  BorHeight := BorderHeight;
  BmpArrays := GetBitmapElementsWithResource(AResourceName, NumberOfItems, BorderWidth, BorderHeight);
  inherited Create;
end;

destructor TSliceScaling.Destroy;
var
  i, j: integer;
begin
  for i := Low(bmpArrays) to High(BMPArrays) do
  begin
    for  j := Low(BMPArrays[i]) to High(BMPArrays[i]) do
    begin
      BMPArrays[i, j].Free;
      BMPArrays[i, j] := nil;
    end;
    BMPArrays[i] := nil;
  end;
  inherited Destroy;
end;

function TSliceScaling.Draw(Width, Height, ItemIndex: integer): TBitmap;
begin
  if UseNativeStretch then
    Result := DrawBitmapNativeStretch(bmpArrays[ItemIndex], Width,
      Height, BorWidth, BorHeight)
  else
    Result := DrawBitmapNonNativeStretch(bmpArrays[ItemIndex], Width,
      Height, BorWidth, BorHeight);
end;

procedure TSliceScaling.Draw(Dest: TCanvas;
  Left, Top, Width, Height, ItemIndex: integer);
var
  temp: TBitmap;
begin
  if UseNativeStretch then
    temp := DrawBitmapNativeStretch(bmpArrays[ItemIndex], Width,
      Height, BorWidth, BorHeight)
  else
    temp := DrawBitmapNonNativeStretch(bmpArrays[ItemIndex], Width,
      Height, BorWidth, BorHeight);

  Dest.Draw(Left, Top, temp);
  temp.Free;
end;

{ TCDWin7 }

procedure TCDWin7.DrawButton(ADest: TFPCustomCanvas; ADestPos: TPoint; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Str: string;
  lGlyphLeftSpacing: integer = 0;
  lTextOutPos: TPoint;
  lGlyphCaptionHeight: integer;
  ItemIndex: integer;
begin
  // ItemIndex
  if csfEnabled in AState then
  begin
    ItemIndex := 0;
    if csfHasFocus in AState then
      ItemIndex := 4;
    if csfMouseOver in AState then
      ItemIndex := 1;
    if csfSunken in AState then
      ItemIndex := 2;
  end
  else
    ItemIndex := 3;

  // Draw ItemIndex
  win7button.Draw(ADest as TCanvas, 0, 0, ASize.cx, ASize.cy, ItemIndex);

  // Position calculations
  if ADest is TCanvas then
  begin
    ADest.Font.Assign(AStateEx.Font);
    Str := AStateEx.Caption;
    lGlyphCaptionHeight := Max(TCanvas(ADest).TextHeight(Str), AStateEx.Glyph.Height);
    lTextOutPos.X := (ASize.cx - TCanvas(ADest).TextWidth(Str) -
      AStateEx.Glyph.Width) div 2;
    lTextOutPos.Y := (ASize.cy - lGlyphCaptionHeight) div 2;
    lTextOutPos.X := Max(lTextOutPos.X, 5);
    lTextOutPos.Y := Max(lTextOutPos.Y, 5);

    // Button glyph
    if not AStateEx.Glyph.Empty then
    begin
      TCanvas(ADest).Draw(lTextOutPos.X, lTextOutPos.Y, AStateEx.Glyph);
      lGlyphLeftSpacing := AStateEx.Glyph.Width + 5;
    end;

    // Button text
    lTextOutPos.X := lTextOutPos.X + lGlyphLeftSpacing;
    lTextOutPos.Y := (ASize.cy - TCanvas(ADest).TextHeight(Str)) div 2;
    ADest.Brush.Style := bsClear;
    ADest.Pen.Style := psSolid;
    if csfEnabled in AState then
    begin
      ADest.TextOut(lTextOutPos.X, lTextOutPos.Y, Str);
    end
    else
    begin
      // The disabled text is composed by a white shadow under it and a grey text
      TCanvas(ADest).Font.Color := clWhite;
      Inc(lTextOutPos.X);
      Inc(lTextOutPos.Y);
      TCanvas(ADest).TextOut(lTextOutPos.X, lTextOutPos.Y, Str);

      TCanvas(ADest).Font.Color := clSilver;
      Dec(lTextOutPos.X);
      Dec(lTextOutPos.Y);
      ADest.TextOut(lTextOutPos.X, lTextOutPos.Y, Str);
    end;
  end;
end;

initialization
  RegisterDrawer(TCDWin7.Create, dsWindows7);
  win7button := TSliceScaling.CreateWithResource('windows7_button', 6, 6, 6);
  win7button.UseNativeStretch := False;

finalization
  win7button.Free;

end.
