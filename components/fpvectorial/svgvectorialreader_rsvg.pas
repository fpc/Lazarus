{
Reads an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

SVG rendering system using an external dependency: librsvg which uses cairo

This provides a better rendering, but has the downside of the external dependency
}
unit svgvectorialreader_rsvg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, contnrs, process, utf8Process,
  laz2_xmlread, laz2_xmlwrite, laz2_dom,
  // image data formats
  fpreadpng,
  fpvectorial, fpvutils, lazutf8, TypInfo,
  fileutil, lazfileutils;

type
  { TvSVGVectorialReader_RSVG }

  TvSVGVectorialReader_RSVG = class(TvCustomVectorialReader)
  private
    FRSVG_CMD, FTmpFile_Path: string;
    function ExecuteRSVG(AUsePipes: Boolean = False): TvRasterImage;
    procedure AddRasterImageToDoc(AData: TvVectorialDocument; AImage: TvRasterImage);
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
    procedure ReadFromXML(Doc: TXMLDocument; AData: TvVectorialDocument); override;
  end;

var
  gSVGVecReader_RSVG_Path: string = '';

implementation

{ TvSVGVectorialReader_RSVG }

function TvSVGVectorialReader_RSVG.ExecuteRSVG(AUsePipes: Boolean): TvRasterImage;
const
  BUF_SIZE = 1024;
var
  AProcess    : TProcessUTF8;
  OutputStream: TStream = nil;
  lImageReader: TFPReaderPNG = nil;
  BytesRead   : longint;
  Buffer      : array[1..BUF_SIZE] of byte;
  lOutputTmpFileName: String;
begin
  Result := TvRasterImage.Create(nil);
  AProcess := TProcessUTF8.Create(nil);
  try
    if gSVGVecReader_RSVG_Path <> '' then FRSVG_CMD := gSVGVecReader_RSVG_Path;

    if not FileExistsUTF8(FRSVG_CMD) then
      raise Exception.Create('TvSVGVectorialReader_RSVG: rsvg-convert.exe not found: ' + FRSVG_CMD);

    AProcess.Executable := FRSVG_CMD;
    //AProcess.Parameters.Add('-f png');
    if not AUsePipes then
    begin
      lOutputTmpFileName := FTmpFile_Path+'.o';
      // Needs to be .Add twice because otherwise TProcess.Execute
      // will think it is a path that needs quoting
      AProcess.Parameters.Add('-o');
      AProcess.Parameters.Add(lOutputTmpFileName);
      AProcess.Options := [poNoConsole, powaitonexit];
    end
    else
    begin
      AProcess.Options := [poUsePipes, poNoConsole];
    end;
    AProcess.Parameters.Add(FTmpFile_Path);
    AProcess.Execute;

    if AUsePipes then
    begin
      OutputStream := TMemoryStream.Create;
      // All generated output from AProcess is read in a loop until no more data is available
      repeat
        BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
        OutputStream.Write(Buffer, BytesRead)
      until BytesRead = 0;  // Stop if no more data is available
    end
    else
    begin
      OutputStream := TFileStream.Create(lOutputTmpFileName, fmOpenRead);
    end;

    // Now that all data has been read it can be used; for example to save it to a file on disk
    OutputStream.Position := 0;
    lImageReader := TFPReaderPNG.Create;
    Result.CreateImageFromStream(OutputStream, lImageReader);

    Result.Width := Result.RasterImage.Width;
    Result.Height := Result.RasterImage.Height;
  finally
    // Clean up
    AProcess.Free;
    if OutputStream <> nil then OutputStream.Free;
    if lImageReader <> nil then lImageReader.Free;
  end;
end;

procedure TvSVGVectorialReader_RSVG.AddRasterImageToDoc(
  AData: TvVectorialDocument; AImage: TvRasterImage);
var
  lPage: TvVectorialPage;
begin
  lPage := AData.AddPage(True);
  lPage.Width := AImage.Width;
  lPage.Height := AImage.Height;
  lPage.AddEntity(AImage);
end;

constructor TvSVGVectorialReader_RSVG.Create;
begin
  inherited Create;

  // rsvg-convert.exe doesnt work without .svg extension
  FTmpFile_Path := SysUtils.GetTempFileName('', 'fpvectorial_')+'.svg';

  {$ifdef Windows}
  FRSVG_CMD := ExtractFilePath(paramstr(0))+'rsvg-convert.exe';
  {$endif}
  {$if defined(Unix) and not defined(darwin)}
  FRSVG_CMD := '/usr/bin/rsvg-convert';
  {$endif}
end;

destructor TvSVGVectorialReader_RSVG.Destroy;
begin
  inherited Destroy;
end;

procedure TvSVGVectorialReader_RSVG.ReadFromFile(AFileName: string;
  AData: TvVectorialDocument);
var
  lRasterImage: TvRasterImage;
begin
  CopyFile(AFileName, FTmpFile_Path);
  lRasterImage := ExecuteRSVG();
  AddRasterImageToDoc(AData, lRasterImage);
end;

procedure TvSVGVectorialReader_RSVG.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument = nil;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);
    ReadFromXML(Doc, AData);
  finally
    // finally, free the document
    Doc.Free;
  end;
end;

procedure TvSVGVectorialReader_RSVG.ReadFromXML(Doc: TXMLDocument;
  AData: TvVectorialDocument);
var
  lRasterImage: TvRasterImage;
begin
  WriteXMLFile(Doc, FTmpFile_Path);
  lRasterImage := ExecuteRSVG();
  AddRasterImageToDoc(AData, lRasterImage);
end;

initialization

  RegisterVectorialReader(TvSVGVectorialReader_RSVG, vfSVG);

end.

