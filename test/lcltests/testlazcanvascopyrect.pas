{ Regression test for clipping in TLazCanvas.CanvasCopyRect.
  See COPYING.modifiedLGPL.txt for the license and linking exception. }

unit TestLazCanvasCopyRect;

{$mode objfpc}{$H+}

interface

uses
  Types, fpcunit, TestGlobals, FPImage, GraphType, IntfGraphics, LazCanvas;

type
  TTestLazCanvasCopyRect = class(TTestCase)
  private
    FDestImage: TLazIntfImage;
    FSourceImage: TLazIntfImage;
    FDestCanvas: TLazCanvas;
    FSourceCanvas: TLazCanvas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Clipping;
  end;

implementation

procedure TTestLazCanvasCopyRect.SetUp;
var
  Desc: TRawImageDescription;
begin
  Desc.Init_BPP32_A8R8G8B8_BIO_TTB(8, 8);
  FDestImage := TLazIntfImage.Create(0, 0);
  FDestImage.DataDescription := Desc;
  FDestImage.FillPixels(colBlack);
  FSourceImage := TLazIntfImage.Create(0, 0);
  FSourceImage.DataDescription := Desc;
  FSourceImage.FillPixels(colRed);
  FDestCanvas := TLazCanvas.Create(FDestImage);
  FSourceCanvas := TLazCanvas.Create(FSourceImage);
  FDestCanvas.ImageFormat := clfBGRA32;
  FSourceCanvas.ImageFormat := clfBGRA32;
end;

procedure TTestLazCanvasCopyRect.TearDown;
begin
  FSourceCanvas.Free;
  FDestCanvas.Free;
  FSourceImage.Free;
  FDestImage.Free;
end;

procedure TTestLazCanvasCopyRect.Clipping;
begin
  FDestCanvas.ClipRect := Rect(4, 4, 7, 7);
  FDestCanvas.Clipping := True;
  FDestCanvas.CanvasCopyRect(FSourceCanvas, 0, 0, 0, 0, 8, 8);
  AssertEquals('copied pixel inside clip', $FFFF, FDestImage.Colors[5, 5].Red);
  AssertEquals('copied pixel outside clip', 0, FDestImage.Colors[1, 1].Red);
end;

initialization
  AddToLCLTestSuite(TTestLazCanvasCopyRect);

end.
