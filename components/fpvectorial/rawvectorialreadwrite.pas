{
A very simple raw image format

AUTHORS: Felipe Monteiro de Carvalho

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details
}
unit rawvectorialreadwrite;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, dateutils,
  fpcanvas, fpimage,
  intfgraphics, graphtype,
  //avisozlib,
  fpvectorial;

type

  { TvRAWVectorialReader }

  TvRAWVectorialReader = class(TvCustomVectorialReader)
  public
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

var
  RAW_IMAGE_WIDTH, RAW_IMAGE_HEIGHT: Integer;

implementation

{ TvRAWVectorialReader }

procedure TvRAWVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  lPage: TvVectorialPage;
  lRasterImage: TvRasterImage;
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  x, y: Integer;
  lColor: TFPColor;
begin
  // create a TLazIntfImage with 32 bits per pixel, alpha 8bit, red 8 bit, green 8bit, blue 8bit,
  // Bits In Order: bit 0 is pixel 0, Top To Bottom: line 0 is top
  lRawImage.Init;
  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(RAW_IMAGE_WIDTH, RAW_IMAGE_HEIGHT);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0,0);

  AImage.SetRawImage(lRawImage);

  for x := 0 to RAW_IMAGE_WIDTH - 1 do
    for y := 0 to RAW_IMAGE_HEIGHT - 1 do
    begin
      lColor.Red := AStream.ReadByte() * $FF;
      AImage.Colors[x, y] := lColor;
    end;

  lPage := AData.AddPage();
  lRasterImage := TvRasterImage.Create(nil);
  lRasterImage.RasterImage := AImage;
  lPage.AddEntity(lRasterImage);
end;

initialization

  RAW_IMAGE_WIDTH := 1024;
  RAW_IMAGE_HEIGHT := 1024;
  RegisterVectorialReader(TvRAWVectorialReader, vfRAW);

end.

