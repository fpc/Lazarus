{
lasvectorialreader.pas

Reads geospatial data encoded in the ASPRS LASer (LAS) file format version 1.3

LAS file format specification obtained from:

http://www.asprs.org/a/society/committees/standards/lidar_exchange_format.html
LAS 1.3 r11

All data is in little-endian format.

AUTHORS: Felipe Monteiro de Carvalho

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details
}
unit lasvectorialreader;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define FPVECTORIALDEBUG_LAS}

interface

uses
  Classes, SysUtils,
  //avisozlib,
  fpvectorial;

type
  // LAS data types introduced in LAS 1.0
  laschar = AnsiChar; // or ShortInt
  lasuchar = Byte;
  lasshort = Smallint;
  lasushort = Word;
  laslong = Integer;
  lasulong = Cardinal;
  lasdouble = double;
  //
  laslonglong = Int64;
  lasulonglong = QWord;

  // PUBLIC HEADER BLOCK version 1.0
  TLASPublicHeaderBlock_1_0 = packed record
    FileSignatureLASF: array[0..3] of laschar;
    FileSourceID: lasushort;                    // Reserved in LAS 1.0
    GlobalEncoding: lasushort;                  // Reserved in LAS 1.0
    ProjectIDGUIDdata1: lasulong;               // Optional
    ProjectIDGUIDdata2: lasushort;              // Optional
    ProjectIDGUIDdata3: lasushort;              // Optional
    ProjectIDGUIDdata4: array[0..7] of lasuchar;// Optional
    VersionMajor: lasuchar;
    VersionMinor: lasuchar;
    SystemIdentifier: array[0..31] of laschar;
    GeneratingSoftware: array[0..31] of laschar;
    FileCreationDayofYear: lasushort;           // Name in LAS 1.0 -> FlightDateJulian, but same meaning
    FileCreationYear: lasushort;                // Name in LAS 1.0 -> Year, but same meaning
    HeaderSize: lasushort;                      // Name in LAS 1.0 -> OffsetToData
    OffsetToPointData: lasulong;
    NumberofVariableLengthRecords: lasulong;
    PointDataFormatID: lasuchar; // (0-99 for spec)
    PointDataRecordLength: lasushort;
    Numberofpointrecords: lasulong;
    Numberofpointsbyreturn: array[0..4] of lasulong;
    Xscalefactor: lasdouble;
    Yscalefactor: lasdouble;
    Zscalefactor: lasdouble;

    Xoffset: lasdouble;
    Yoffset: lasdouble;
    Zoffset: lasdouble;
    MaxX: lasdouble;
    MinX: lasdouble;
    MaxY: lasdouble;
    MinY: lasdouble;
    MaxZ: lasdouble;
    MinZ: lasdouble;
  end;

  // PUBLIC HEADER BLOCK Extension in Version 1.3
  TLASPublicHeaderBlock_1_3_Extension = packed record
    StartofWaveformDataPacket: lasulonglong;
  end;

  TLASVariableLengthRecord = packed record
    RecordSignatureAABB: lasushort;
    UserID: array[0..15] of laschar;
    RecordID: lasushort;
    RecordLengthAfterHeader: lasushort;
    Description: array[0..31] of laschar;
  end;

  TLASPointDataRecordFormat0 = packed record
    X: laslong;
    Y: laslong;
    Z: laslong;
    Intensity: lasushort;
    Flags: Byte;
    Classification: lasuchar;
    ScanAngleRank: lasuchar; // (-90 to +90) - is the left side
    FileMarker: lasuchar;
    UserBitField: lasushort;
  end;

  TLASPointDataRecordFormat1 = packed record
    X: laslong;
    Y: laslong;
    Z: laslong;
    Intensity: lasushort;
    Flags: Byte;
    Classification: lasuchar;
    ScanAngleRank: lasuchar; // (-90 to +90) - is the left side
    FileMarker: lasuchar;
    UserBitField: lasushort;
    GPSTime: lasdouble;
  end;

  { TvLASVectorialReader }

  TvLASVectorialReader = class(TvCustomVectorialReader)
  private
    // Stream position information
    InitialPos, PositionAfterPublicHeader: Int64;
    {$ifdef FPVECTORIALDEBUG_LAS}
    procedure DebugOutPublicHeaderBlock();
    {$endif}
    procedure ReadVariableLengthRecords(AStream: TStream);
  public
    // Public Header
    PublicHeaderBlock_1_0: TLASPublicHeaderBlock_1_0;
    PublicHeaderBlock_1_3_Extension: TLASPublicHeaderBlock_1_3_Extension;
    // Variable Length Records
    VariableLengthRecords: array of TLASVariableLengthRecord;
    // Point Data
    PointsFormat0: array of TLASPointDataRecordFormat0;
    PointsFormat1: array of TLASPointDataRecordFormat1;
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{$ifdef FPVECTORIALDEBUG_LAS}
procedure TvLASVectorialReader.DebugOutPublicHeaderBlock;
begin
  WriteLn(Format('FileSignatureLASF = %s = %x %x %x %x',
    [string(PublicHeaderBlock_1_0.FileSignatureLASF),
     Ord(PublicHeaderBlock_1_0.FileSignatureLASF[0]),
     Ord(PublicHeaderBlock_1_0.FileSignatureLASF[1]),
     Ord(PublicHeaderBlock_1_0.FileSignatureLASF[2]),
     Ord(PublicHeaderBlock_1_0.FileSignatureLASF[3])]));
  WriteLn(Format('FileSourceID = $%x', [PublicHeaderBlock_1_0.FileSourceID]));
  WriteLn(Format('GlobalEncoding = $%x', [PublicHeaderBlock_1_0.GlobalEncoding]));
  WriteLn(Format('ProjectIDGUIDdata1 = $%x', [PublicHeaderBlock_1_0.ProjectIDGUIDdata1]));
  WriteLn(Format('ProjectIDGUIDdata2 = $%x', [PublicHeaderBlock_1_0.ProjectIDGUIDdata2]));
  WriteLn(Format('ProjectIDGUIDdata3 = $%x', [PublicHeaderBlock_1_0.ProjectIDGUIDdata3]));
//  WriteLn(Format('ProjectIDGUIDdata4 = %x', [ProjectIDGUIDdata2]));
  WriteLn(Format('VersionMajor = %d', [PublicHeaderBlock_1_0.VersionMajor]));
  WriteLn(Format('VersionMinor = %d', [PublicHeaderBlock_1_0.VersionMinor]));
  WriteLn(Format('SystemIdentifier = %s', [PublicHeaderBlock_1_0.SystemIdentifier]));
  WriteLn(Format('GeneratingSoftware = %s', [PublicHeaderBlock_1_0.GeneratingSoftware]));
  WriteLn(Format('FileCreationDayofYear = %d', [PublicHeaderBlock_1_0.FileCreationDayofYear]));
  WriteLn(Format('FileCreationYear = %d', [PublicHeaderBlock_1_0.FileCreationYear]));
  WriteLn(Format('HeaderSize = $%x', [PublicHeaderBlock_1_0.HeaderSize]));
  WriteLn(Format('OffsetToPointData = $%x', [PublicHeaderBlock_1_0.OffsetToPointData]));
  WriteLn(Format('NumberofVariableLengthRecords = $%x', [PublicHeaderBlock_1_0.NumberofVariableLengthRecords]));
  WriteLn(Format('PointDataFormatID = $%x', [PublicHeaderBlock_1_0.PointDataFormatID]));
  WriteLn(Format('PointDataRecordLength = $%x', [PublicHeaderBlock_1_0.PointDataRecordLength]));
  WriteLn(Format('Numberofpointrecords = $%x', [PublicHeaderBlock_1_0.Numberofpointrecords]));
  WriteLn(Format('Numberofpointsbyreturn = %x %x %x %x %x',
    [PublicHeaderBlock_1_0.Numberofpointsbyreturn[0],
     PublicHeaderBlock_1_0.Numberofpointsbyreturn[1],
     PublicHeaderBlock_1_0.Numberofpointsbyreturn[2],
     PublicHeaderBlock_1_0.Numberofpointsbyreturn[3],
     PublicHeaderBlock_1_0.Numberofpointsbyreturn[4]
     ]));
  WriteLn(Format('Xscalefactor = %f', [PublicHeaderBlock_1_0.Xscalefactor]));
  WriteLn(Format('Yscalefactor = %f', [PublicHeaderBlock_1_0.Yscalefactor]));
  WriteLn(Format('Zscalefactor = %f', [PublicHeaderBlock_1_0.Zscalefactor]));
  WriteLn(Format('Xoffset = %f', [PublicHeaderBlock_1_0.Xoffset]));
  WriteLn(Format('Yoffset = %f', [PublicHeaderBlock_1_0.Yoffset]));
  WriteLn(Format('Zoffset = %f', [PublicHeaderBlock_1_0.Zoffset]));
  WriteLn(Format('MaxX = %f', [PublicHeaderBlock_1_0.MaxX]));
  WriteLn(Format('MinX = %f', [PublicHeaderBlock_1_0.MinX]));
  WriteLn(Format('MaxY = %f', [PublicHeaderBlock_1_0.MaxY]));
  WriteLn(Format('MinY = %f', [PublicHeaderBlock_1_0.MinY]));
  WriteLn(Format('MaxZ = %f', [PublicHeaderBlock_1_0.MaxZ]));
  WriteLn(Format('MinZ = %f', [PublicHeaderBlock_1_0.MinZ]));
  WriteLn('');
  WriteLn(Format('LAS 1.0 header size = $%x', [SizeOf(TLASPublicHeaderBlock_1_0)]));
  WriteLn('');
end;
{$endif}

procedure TvLASVectorialReader.ReadVariableLengthRecords(AStream: TStream);
var
  i: Integer;
  NextPosition: Int64;
begin
  NextPosition := PositionAfterPublicHeader;

  SetLength(VariableLengthRecords, PublicHeaderBlock_1_0.NumberofVariableLengthRecords);
  for i := 0 to PublicHeaderBlock_1_0.NumberofVariableLengthRecords - 1 do
  begin
    AStream.Position := NextPosition;
    {$ifdef FPVECTORIALDEBUG_LAS}
    WriteLn(Format('Variable Length Record #%d Position = $%x', [i, NextPosition]));
    WriteLn('');
    {$endif}
    AStream.Read(VariableLengthRecords[i], SizeOf(TLASVariableLengthRecord));
    NextPosition := AStream.Position+VariableLengthRecords[i].RecordLengthAfterHeader;
    {$ifdef FPVECTORIALDEBUG_LAS}
    WriteLn(Format('RecordSignatureAABB = $%x', [VariableLengthRecords[i].RecordSignatureAABB]));
    WriteLn(Format('UserID = %s', [VariableLengthRecords[i].UserID]));
    WriteLn(Format('RecordID = $%x', [VariableLengthRecords[i].RecordID]));
    WriteLn(Format('RecordLengthAfterHeader = $%x', [VariableLengthRecords[i].RecordLengthAfterHeader]));
    WriteLn(Format('Description = %s', [VariableLengthRecords[i].Description]));
    WriteLn('');
    {$endif}
  end;
end;

procedure TvLASVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  lPage: TvVectorialPage;
  lRecord0: TLASPointDataRecordFormat0;
  lRecord1: TLASPointDataRecordFormat1;
begin
  // Clear and add the first page
  AData.Clear;
  lPage := AData.AddPage();

  // First read the header like if it was for LAS 1.0,
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
  case PublicHeaderBlock_1_0.PointDataFormatID of
    0:
    begin
      while AStream.Position < AStream.Size do
      begin
        AStream.ReadBuffer(lRecord0, SizeOf(TLASPointDataRecordFormat0));
        lPage.AddPoint(lRecord0.X, lRecord0.Y, lRecord0.Z);
      end;
    end;
    1:
    begin
      while AStream.Position < AStream.Size do
      begin
        AStream.ReadBuffer(lRecord1, SizeOf(TLASPointDataRecordFormat1));
        lPage.AddPoint(lRecord1.X, lRecord1.Y, lRecord1.Z);
      end;
    end;
  end;
end;

initialization

  RegisterVectorialReader(TvLASVectorialReader, vfLAS);

end.

