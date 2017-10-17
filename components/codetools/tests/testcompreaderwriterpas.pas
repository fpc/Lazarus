unit TestCompReaderWriterPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, fpcunit, testregistry,
  CodeToolManager, CodeCache, LinkScanner,
  TestStdCodetools;

type

  { TTestCompReaderWriterPas }

  TTestCompReaderWriterPas = class(TCustomTestCTStdCodetools)
  published
    procedure TestWriteProperties;
  end;

implementation

{ TTestCompReaderWriterPas }

procedure TTestCompReaderWriterPas.TestWriteProperties;
begin

end;

initialization
  RegisterTest(TTestCompReaderWriterPas);
end.

