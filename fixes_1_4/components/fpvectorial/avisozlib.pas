unit avisozlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paszlib;

type
  Decode = class
  public
    procedure CHECK_ERR(err: Integer; msg: String);
     procedure EXIT_ERR(const msg: String);
     function test_inflate(compr: Pointer; comprLen : LongInt;
               uncompr: Pointer; uncomprLen : LongInt): PChar;
     constructor Create();
    end;

implementation

procedure Decode.CHECK_ERR(err: Integer; msg: String);
begin
  if err <> Z_OK then
  begin
    raise Exception.Create('ERROR: ' + msg);
    Halt(1);
  end;
end;

procedure Decode.EXIT_ERR(const msg: String);
begin
  raise Exception.Create('ERROR: ' + msg);
  Halt(1);
end;

function Decode.test_inflate(compr: Pointer; comprLen : LongInt;
                       uncompr: Pointer; uncomprLen : LongInt): PChar;
var err: Integer;
    d_stream: TZStream; // decompression stream
begin
  StrCopy(PChar(uncompr), 'garbage');

  d_stream.next_in := compr;
  d_stream.avail_in := 0;
  d_stream.next_out := uncompr;

  err := inflateInit(d_stream);
  CHECK_ERR(err, 'inflateInit');

  while (d_stream.total_out < uncomprLen) and
        (d_stream.total_in < comprLen) do
  begin
    d_stream.avail_out := 1; // force small buffers
    d_stream.avail_in := 1;
    err := inflate(d_stream, Z_NO_FLUSH);
    if err = Z_STREAM_END then
      break;
    CHECK_ERR(err, 'inflate');
  end;

  err := inflateEnd(d_stream);
  CHECK_ERR(err, 'inflateEnd');

  Result:=PChar(uncompr);
end;

constructor Decode.Create();
begin
  inherited Create;
end;

end.

