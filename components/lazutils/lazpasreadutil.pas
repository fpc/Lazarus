{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Helper functions for component serialized Pascal.

  Author: Mattias Gaertner
}
unit LazPasReadUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes;

{ ExecCustomCSP: Call Instance.DefineProperties with a list of properties in
  TBinaryObjectWriter format. This function is used by the auto generated
  Pascal of TCompWriterPas for custom DefineProperties. }
procedure ExecCustomCSP(Instance: TPersistent; const Data: array of string);

implementation

type

  { TCSPReader }

  TCSPReader = class(TReader)
  public
    procedure ReadProperties(Instance: TPersistent);
  end;

{ TCSPReader }

procedure TCSPReader.ReadProperties(Instance: TPersistent);
begin
  while not EndOfList do
    ReadProperty(Instance);
end;

procedure ExecCustomCSP(Instance: TPersistent; const Data: array of string);
var
  MemStream: TMemoryStream;
  i: Integer;
  s: String;
  Reader: TCSPReader;
begin
  MemStream:=TMemoryStream.Create;
  Reader:=nil;
  try
    for i:=low(Data) to High(Data) do
    begin
      s:=Data[i];
      MemStream.Write(s[1],length(s));
    end;
    MemStream.Position:=0;
    Reader:=TCSPReader.Create(MemStream,1024);
    Reader.ReadProperties(Instance);
  finally
    Reader.Free;
    MemStream.Free;
  end;
end;

end.

