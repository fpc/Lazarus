unit TestOutputLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger;

function GetTestLogger: TLazLoggerFile; inline;

property TestLogger: TLazLoggerFile read GetTestLogger;

implementation

var TheLogger: TLazLoggerFile;

function GetTestLogger: TLazLoggerFile;
begin
  if TheLogger = nil then begin
    TheLogger := TLazLoggerFile.Create;
    TLazLoggerFile(TheLogger).Assign(DebugLogger);
    TheLogger.OnDbgOut := nil;
    TheLogger.OnDebugLn := nil;
    TheLogger.Init;
  end;
  Result := TheLogger;
end;

end.

