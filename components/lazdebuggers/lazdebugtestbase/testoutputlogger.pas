(* This file is part of the TestSuite for debuggers in Lazarus IDE

  This file is licensed

  modified LGPL-2 (version 2 or later version).

  "modified" as documented for the LCL:
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
*)
unit TestOutputLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, LazClasses;

function GetTestLogger: TLazLoggerFile; inline;

property TestLogger: TLazLoggerFile read GetTestLogger;

implementation

var TheLogger: TLazLoggerFile;

function GetTestLogger: TLazLoggerFile;
begin
  if TheLogger = nil then begin
    TheLogger := TLazLoggerFile.Create;
    TheLogger.AddReference;
//    TLazLoggerFile(TheLogger).Assign(DebugLogger);
    TheLogger.OnDbgOut := nil;
    TheLogger.OnDebugLn := nil;
    TheLogger.Init;
  end;
  Result := TheLogger;
end;

finalization
  ReleaseRefAndNil(TheLogger);

end.

