{
lazvectorialreader.pas

Reads geospatial data encoded in the ASPRS LASer (LAS) file format version 1.3
With compression

LAZ file format specification obtained from:

http://laszip.org/
http://www.cs.unc.edu/~isenburg/lastools/download/laszip.pdf

Example file:

www.cs.unc.edu/~isenburg/lastools/download/data/xyzrgb_manuscript.laz

All data is in little-endian format.

AUTHORS: Felipe Monteiro de Carvalho

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details
}
unit lazvectorialreader;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define FPVECTORIALDEBUG_LAS}

interface

uses
  Classes, SysUtils, dateutils,
  fpcanvas, fpimage,
  //avisozlib,
  fpvectorial, lasvectorialreader;

type
  { TvLAZVectorialReader }

  TvLAZVectorialReader = class(TvLASVectorialReader)
  public
    { General reading methods }
    //procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

(*procedure TvLAZVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  lPage: TvVectorialPage;
  lRecord0: TLASPointDataRecordFormat0;
  lRecord1: TLASPointDataRecordFormat1;
  lFirstPoint: Boolean = True;
  lPoint: TvPoint;
  lClassification: Integer = -1;
  lColor: TFPColor;
  lPointsCounter: Integer = 0;
begin
  // Clear and add the first page
  AData.Clear;
  lPage := AData.AddPage();

(*  // First read the header like if it was for LAS 1.0,
  // this will tell us the real version so then we read it again
  InitialPos := AStream.Position;
  AStream.Read(PublicHeaderBlock_1_0, SizeOf(TLASPublicHeaderBlock_1_0));
  {$ifdef FPVECTORIALDEBUG_LAS}
  DebugOutPublicHeaderBlock();
  {$endif}

  // First check the signature
  if PublicHeaderBlock_1_0.FileSignatureLASF <> 'LASF' then
    raise Exception.Create('[TvLASVectorialReader.ReadFromStream] Invalid file signoture while reading LAS file');

  lPage.MinX := PublicHeaderBlock_1_0.MinX;
  lPage.MinY := PublicHeaderBlock_1_0.MinY;
  lPage.MinZ := PublicHeaderBlock_1_0.MinZ;
  lPage.MaxX := PublicHeaderBlock_1_0.MaxX;
  lPage.MaxY := PublicHeaderBlock_1_0.MaxY;
  lPage.MaxZ := PublicHeaderBlock_1_0.MaxZ;

  // In LAS 1.3+ read the header extension
  // ToDo

  PositionAfterPublicHeader := AStream.Position;

  // Read the variable length records
  ReadVariableLengthRecords(AStream);

  // Read the point data
  AStream.Position := InitialPos + PublicHeaderBlock_1_0.OffsetToPointData;
  while AStream.Position < AStream.Size do
  begin
    // Send a progress event every 1k points
    Inc(lPointsCounter);
    if lPointsCounter mod 1000 = 0 then
      DoProgress(Round(AStream.Position * 100 / AStream.Size), AData);

    // hack to cut las files: if lPage.GetEntitiesCount = 100000 then Exit;
    case PublicHeaderBlock_1_0.PointDataFormatID of
    0:
    begin
      AStream.ReadBuffer(lRecord0, SizeOf(TLASPointDataRecordFormat0));
      lClassification := lRecord0.Classification;
      lPoint := lPage.AddPoint(lRecord0.X, lRecord0.Y, lRecord0.Z);
    end;
    1:
    begin
      AStream.ReadBuffer(lRecord1, SizeOf(TLASPointDataRecordFormat1));
      lClassification := lRecord1.Classification;
      lPoint := lPage.AddPoint(lRecord1.X, lRecord1.Y, lRecord1.Z);
    end;
    end;

    // Correct the min and max
    if lFirstPoint then
    begin
      lPage.MinX := lPoint.X;
      lPage.MinY := lPoint.Y;
      lPage.MinZ := lPoint.Z;
      lPage.MaxX := lPoint.X;
      lPage.MaxY := lPoint.Y;
      lPage.MaxZ := lPoint.Z;

      lFirstPoint := False;
    end
    else
    begin
      if lPage.MinX > lPoint.X then lPage.MinX := lPoint.X;
      if lPage.MinY > lPoint.Y then lPage.MinY := lPoint.Y;
      if lPage.MinZ > lPoint.Z then lPage.MinZ := lPoint.Z;
      if lPage.MaxX < lPoint.X then lPage.MaxX := lPoint.X;
      if lPage.MaxY < lPoint.Y then lPage.MaxY := lPoint.Y;
      if lPage.MaxZ < lPoint.Z then lPage.MaxZ := lPoint.Z;
    end;

    // Set a color if desired
    case lClassification of
    // Table 4.9 - ASPRS Standard LIDAR Point Classes
    // Classification Value
    // 0 Created, never classified
    // 1 Unclassified1
    // 2 Ground
    2: lColor := colMaroon;
    // 3 Low Vegetation
    3: lColor := colGreen;
    // 4 Medium Vegetation
    4: lColor := colGreen;
    // 5 High Vegetation
    5: lColor := colDkGreen;
    // 6 Building
    6: lColor := colGray;
    // 7 Low Point (noise)
    // 8 Model Key-point (mass point)
    // 9 Water
    9: lColor := colBlue;
    else
      lColor := colBlack;
    end;

    lPoint.Pen.Color := lColor;
  end;
end;*)

initialization

//  RegisterVectorialReader(TvLAZVectorialReader, vfLAZ);
//  RegisterVectorialWriter(TvLAZVectorialWriter, vfLAZ);

end.

