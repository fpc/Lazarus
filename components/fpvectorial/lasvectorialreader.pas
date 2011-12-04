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
  // LAS data types
  laschar = Char; // or ShortInt
  lasuchar = Byte;
  lasshort = Smallint;
  lasushort = Word;
  laslong = Integer;
  lasulong = Cardinal;
  laslonglong = Int64;
  lasulonglong = QWord;
  lasdouble = double;

  // PUBLIC HEADER BLOCK
  TLASPublicHeaderBlock = packed record
    FileSignatureLASF: array[0..3] of laschar;
    FileSourceID: lasushort;
    GlobalEncoding: lasushort;
    ProjectIDGUIDdata1: lasulong;               // Optional
    ProjectIDGUIDdata2: lasushort;              // Optional
    ProjectIDGUIDdata3: lasushort;              // Optional
    ProjectIDGUIDdata4: array[0..7] of lasuchar;// Optional
    VersionMajor: lasuchar;
    VersionMinor: lasuchar;
    SystemIdentifier: array[0..31] of laschar;
    GeneratingSoftware: array[0..31] of laschar;
    FileCreationDayofYear: lasushort;
    FileCreationYear: lasushort;
{Number of Variable Length Records
Point Data Format ID (0-99 for spec)
Point Data Record Length
Number of point records
Number of points by return
X scale factor
Y scale factor
Z scale factor
X offset
Y offset
Z offset
Max X
Min X
Max Y
Min Y
Max Z
Min Z
Start of Waveform Data Packet Record
Any field in the Public Header Block that is not required and is not used must be zero filled.}
  end;

  { TvLASVectorialReader }

  TvLASVectorialReader = class(TvCustomVectorialReader)
  private
    {$ifdef FPVECTORIALDEBUG_LAS}
    procedure DebugOutPublicHeaderBlock();
    {$endif}
  public
    PublicHeaderBlock: TLASPublicHeaderBlock;
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{$ifdef FPVECTORIALDEBUG_LAS}
procedure TvLASVectorialReader.DebugOutPublicHeaderBlock;
begin
  WriteLn(Format('FileSignatureLASF = %s = %x %x %x %x',
    [PublicHeaderBlock.FileSignatureLASF,
    PublicHeaderBlock.FileSignatureLASF[0],
    PublicHeaderBlock.FileSignatureLASF[1],
    PublicHeaderBlock.FileSignatureLASF[2],
    PublicHeaderBlock.FileSignatureLASF[3]]));
  WriteLn(Format('FileSourceID = %x', [PublicHeaderBlock.FileSourceID]));
  WriteLn(Format('GlobalEncoding = %x', [PublicHeaderBlock.GlobalEncoding]));
  WriteLn(Format('ProjectIDGUIDdata1 = %x', [PublicHeaderBlock.ProjectIDGUIDdata1]));
  WriteLn(Format('ProjectIDGUIDdata2 = %x', [PublicHeaderBlock.ProjectIDGUIDdata2]));
  WriteLn(Format('ProjectIDGUIDdata3 = %x', [PublicHeaderBlock.ProjectIDGUIDdata3]));
//  WriteLn(Format('ProjectIDGUIDdata2 = %x', [ProjectIDGUIDdata2]));
  WriteLn(Format('VersionMajor = %x', [PublicHeaderBlock.VersionMajor]));
  WriteLn(Format('VersionMinor = %x', [PublicHeaderBlock.VersionMinor]));
  WriteLn(Format('SystemIdentifier = %s', [PublicHeaderBlock.SystemIdentifier]));
  WriteLn(Format('GeneratingSoftware = %s', [PublicHeaderBlock.GeneratingSoftware]));
  WriteLn(Format('FileCreationDayofYear = %x', [PublicHeaderBlock.FileCreationDayofYear]));
  WriteLn(Format('FileCreationYear = %x', [PublicHeaderBlock.FileCreationYear]));
end;
{$endif}

procedure TvLASVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  AStream.Read(PublicHeaderBlock, SizeOf(TLASPublicHeaderBlock));
  {$ifdef FPVECTORIALDEBUG_LAS}
  DebugOutPublicHeaderBlock();
  {$endif}
end;

initialization

  RegisterVectorialReader(TvLASVectorialReader, vfLAS);

end.

