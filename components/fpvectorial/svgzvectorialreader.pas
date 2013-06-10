unit svgzvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  zstream,
  svgvectorialreader, fpvectorial, fpvutils, lazutf8;

type
  { TvSVGZVectorialReader }

  TvSVGZVectorialReader = class(TvSVGVectorialReader)
  public
    { General reading methods }
    procedure InflateGZ(AGZFilename: string; ADest: TStream);
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvSVGZVectorialReader }

procedure TvSVGZVectorialReader.InflateGZ(AGZFilename: string; ADest: TStream);
var
  GZStream: TGZFileStream;
  chunk:string;
  cnt:integer;
const
  CHUNKSIZE=4096;
begin
  GZStream := TGZFileStream.Create(AGZFilename, gzopenread);
  try
    setlength(chunk,CHUNKSIZE);
    repeat
      cnt := GZStream.read(chunk[1],CHUNKSIZE);
      if cnt<CHUNKSIZE then
        setlength(chunk,cnt);
      ADest.Write(chunk[1], Length(chunk));
    until cnt<CHUNKSIZE;
  finally
    GZStream.Free;
  end;
end;

procedure TvSVGZVectorialReader.ReadFromFile(AFileName: string;
  AData: TvVectorialDocument);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    InflateGZ(AFileName, DataStream);
    DataStream.Position := 0;
    ReadFromStream(DataStream, AData);
    //DataStream.SaveToFile('/tmp/foo.svg');
  finally
    DataStream.Free;
  end;
end;

procedure TvSVGZVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  inherited ReadFromStream(AStream, AData);
end;

initialization

  RegisterVectorialReader(TvSVGZVectorialReader, vfSVGZ);

end.

