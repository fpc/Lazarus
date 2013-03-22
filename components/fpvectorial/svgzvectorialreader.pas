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
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvSVGZVectorialReader }

procedure TvSVGZVectorialReader.ReadFromFile(AFileName: string;
  AData: TvVectorialDocument);
var
  FileStream: TGZFileStream;
begin
  FileStream := TGZFileStream.Create(AFileName, gzopenread);
  try
    ReadFromStream(FileStream, AData);
  finally
    FileStream.Free;
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

