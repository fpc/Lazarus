unit TestCustomDrawnStretchMaskBlt;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Types, fpcunit, TestGlobals, FPImage, GraphType, IntfGraphics,
  LazCanvas, LCLType, LCLIntf, CustomDrawnInt, CustomDrawnProc;

type
  TTestCustomDrawnStretchMaskBlt = class(TTestCase)
  published
    procedure AppliesMonochromeMask;
    procedure BlendsSourceAlpha;
    procedure BlendsSourceAlphaWithBaseWindowOrg;
    procedure CopiesOverlappingCanvasPixels;
    procedure CopiesOverlappingCanvasPixelsWithBaseWindowOrg;
  end;

implementation

function NewImage(AWidth, AHeight: Integer): TLazIntfImage;
var
  Desc: TRawImageDescription;
begin
  Desc.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
  Result := TLazIntfImage.Create(0, 0);
  Result.DataDescription := Desc;
end;

function NewMaskImage(AWidth, AHeight: Integer): TLazIntfImage;
var
  Desc: TRawImageDescription;
begin
  Desc.Init_BPP1(AWidth, AHeight);
  Result := TLazIntfImage.Create(0, 0);
  Result.DataDescription := Desc;
  Result.FillPixels(colBlack);
end;

function NewCanvas(AImage: TLazIntfImage): TLazCanvas;
begin
  Result := TLazCanvas.Create(AImage);
  Result.ImageFormat := clfBGRA32;
end;

procedure TTestCustomDrawnStretchMaskBlt.BlendsSourceAlphaWithBaseWindowOrg;
var
  SrcImage, DstImage: TLazIntfImage;
  SrcCanvas, DstCanvas: TLazCanvas;
  C: TFPColor;
  Success: Boolean;
begin
  SrcImage := NewImage(1, 1);
  DstImage := NewImage(3, 1);
  SrcCanvas := nil;
  DstCanvas := nil;
  try
    SrcImage.Colors[0, 0] := FPColor($FFFF, 0, 0, $8000);
    DstImage.Colors[0, 0] := colYellow;
    DstImage.Colors[1, 0] := colGreen;
    DstImage.Colors[2, 0] := colBlue;
    SrcCanvas := NewCanvas(SrcImage);
    DstCanvas := NewCanvas(DstImage);
    DstCanvas.BaseWindowOrg := Point(2, 0);
    DstCanvas.WindowOrg := Point(0, 0);
    Success := CDWidgetSet.StretchMaskBlt(HDC(DstCanvas), 0, 0, 1, 1,
      HDC(SrcCanvas), 0, 0, 1, 1, 0, 0, 0, SRCCOPY);
    C := DstImage.Colors[0, 0];
    AssertEquals('base origin leaves earlier destination pixel unchanged',
      colYellow.Red, C.Red);
    C := DstImage.Colors[2, 0];
    AssertTrue('alpha blend reads destination at total window origin',
      Abs(Integer(C.Red) - 32768) < 256);
    AssertTrue('alpha blend retains blue at total window origin',
      Abs(Integer(C.Blue) - 32767) < 256);
    AssertTrue('operation succeeded', Success);
  finally
    DstCanvas.Free;
    SrcCanvas.Free;
    SrcImage.Free;
    DstImage.Free;
  end;
end;

procedure TTestCustomDrawnStretchMaskBlt.AppliesMonochromeMask;
var
  SrcImage, DstImage: TLazIntfImage;
  MaskBitmap: TCDBitmap;
  SrcCanvas, DstCanvas: TLazCanvas;
  C: TFPColor;
  Success: Boolean;
begin
  SrcImage := NewImage(2, 1);
  DstImage := NewImage(2, 1);
  MaskBitmap := TCDBitmap.Create;
  SrcCanvas := nil;
  DstCanvas := nil;
  try
    SrcImage.FillPixels(colRed);
    DstImage.FillPixels(colBlue);
    MaskBitmap.Image := NewMaskImage(2, 1);
    MaskBitmap.Image.Colors[1, 0] := colWhite;
    SrcCanvas := NewCanvas(SrcImage);
    DstCanvas := NewCanvas(DstImage);
    Success := CDWidgetSet.StretchMaskBlt(HDC(DstCanvas), 0, 0, 2, 1,
      HDC(SrcCanvas), 0, 0, 2, 1, HBITMAP(MaskBitmap), 0, 0, SRCCOPY);
    C := DstImage.Colors[0, 0];
    AssertEquals('unmasked source pixel copied', colRed.Red, C.Red);
    C := DstImage.Colors[1, 0];
    AssertEquals('masked destination pixel preserved', colBlue.Blue, C.Blue);
    AssertTrue('operation succeeded', Success);
  finally
    DstCanvas.Free;
    SrcCanvas.Free;
    MaskBitmap.Free;
    SrcImage.Free;
    DstImage.Free;
  end;
end;

procedure TTestCustomDrawnStretchMaskBlt.BlendsSourceAlpha;
var
  SrcImage, DstImage: TLazIntfImage;
  SrcCanvas, DstCanvas: TLazCanvas;
  C: TFPColor;
  Success: Boolean;
begin
  SrcImage := NewImage(1, 1);
  DstImage := NewImage(1, 1);
  SrcCanvas := nil;
  DstCanvas := nil;
  try
    SrcImage.FillPixels(colBlack);
    SrcImage.Colors[0, 0] := FPColor($FFFF, 0, 0, $8000);
    DstImage.FillPixels(colBlue);
    SrcCanvas := NewCanvas(SrcImage);
    DstCanvas := NewCanvas(DstImage);
    Success := CDWidgetSet.StretchMaskBlt(HDC(DstCanvas), 0, 0, 1, 1,
      HDC(SrcCanvas), 0, 0, 1, 1, 0, 0, 0, SRCCOPY);
    C := DstImage.Colors[0, 0];
    AssertTrue('half-alpha red is blended', Abs(Integer(C.Red) - 32768) < 256);
    AssertTrue('half-alpha blue is retained', Abs(Integer(C.Blue) - 32767) < 256);
    AssertEquals('blended output is opaque', alphaOpaque, C.Alpha);
    AssertTrue('operation succeeded', Success);
  finally
    DstCanvas.Free;
    SrcCanvas.Free;
    SrcImage.Free;
    DstImage.Free;
  end;
end;

procedure TTestCustomDrawnStretchMaskBlt.CopiesOverlappingCanvasPixels;
var
  Image: TLazIntfImage;
  MaskBitmap: TCDBitmap;
  Canvas: TLazCanvas;
  C: TFPColor;
  Success: Boolean;
begin
  Image := NewImage(3, 3);
  MaskBitmap := TCDBitmap.Create;
  Canvas := nil;
  try
    Image.FillPixels(colBlack);
    Image.Colors[0, 0] := colRed;
    Image.Colors[1, 0] := colGreen;
    Image.Colors[2, 0] := colBlue;
    Image.Colors[0, 1] := colYellow;
    Image.Colors[1, 1] := colWhite;
    Image.Colors[2, 1] := colBlack;
    MaskBitmap.Image := NewMaskImage(3, 2);
    Canvas := NewCanvas(Image);
    Success := CDWidgetSet.StretchMaskBlt(HDC(Canvas), 0, 1, 3, 2,
      HDC(Canvas), 0, 0, 3, 2, HBITMAP(MaskBitmap), 0, 0, SRCCOPY);
    C := Image.Colors[0, 2];
    AssertEquals('overlap reads original second source row', colYellow.Red, C.Red);
    AssertEquals('overlap preserves second source row green channel', colYellow.Green, C.Green);
    AssertTrue('operation succeeded', Success);
  finally
    Canvas.Free;
    MaskBitmap.Free;
    Image.Free;
  end;
end;

procedure TTestCustomDrawnStretchMaskBlt.CopiesOverlappingCanvasPixelsWithBaseWindowOrg;
var
  Image: TLazIntfImage;
  MaskBitmap: TCDBitmap;
  Canvas: TLazCanvas;
  C: TFPColor;
  Success: Boolean;
begin
  Image := NewImage(7, 1);
  MaskBitmap := TCDBitmap.Create;
  Canvas := nil;
  try
    Image.Colors[0, 0] := colBlack;
    Image.Colors[1, 0] := colBlue;
    Image.Colors[2, 0] := colGreen;
    Image.Colors[3, 0] := colWhite;
    Image.Colors[4, 0] := colRed;
    Image.Colors[5, 0] := colYellow;
    Image.Colors[6, 0] := colFuchsia;
    MaskBitmap.Image := NewMaskImage(2, 1);
    Canvas := NewCanvas(Image);
    Canvas.BaseWindowOrg := Point(5, 0);
    Canvas.WindowOrg := Point(0, 0);
    Success := CDWidgetSet.StretchMaskBlt(HDC(Canvas), 0, 0, 2, 1,
      HDC(Canvas), 4, 0, 2, 1, HBITMAP(MaskBitmap), 0, 0, SRCCOPY);
    C := Image.Colors[5, 0];
    AssertEquals('first overlapping source pixel copied', colRed.Red, C.Red);
    C := Image.Colors[6, 0];
    AssertEquals('overlap reads source pixel before base-origin write',
      colYellow.Red, C.Red);
    AssertEquals('overlap preserves source pixel green channel',
      colYellow.Green, C.Green);
    AssertTrue('operation succeeded', Success);
  finally
    Canvas.Free;
    MaskBitmap.Free;
    Image.Free;
  end;
end;

initialization
  AddToLCLTestSuite(TTestCustomDrawnStretchMaskBlt);

end.
