{
Reads AvisoCNC G-Code

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit avisocncgcodereader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial;

type

  { Used by tcutils.SeparateString }
  T10Strings = array[0..9] of shortstring;

  { TvAvisoCNCGCodeReader }

  TvAvisoCNCGCodeReader = class(TvCustomVectorialReader)
  private
    LastX, LastY, LastZ: Double;
    function  SeparateString(AString: string; ASeparator: Char): T10Strings;
    procedure ReadString(AStr: string; AData: TvVectorialPage);
    function  GetCoordinate(AStr: shortstring): Integer;
    function  GetCoordinateValue(AStr: shortstring): Double;
  public
    { General reading methods }
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

const
  { Coordinate constants }

  INT_COORDINATE_NONE = 0;
  INT_COORDINATE_X = 1;
  INT_COORDINATE_Y = 2;
  INT_COORDINATE_Z = 3;

  { GCode constants }

  STR_GCODE_LINEAR_MOVE = 'G01';
  STR_GCODE_STEPPER_MOVE = 'S01';
  STR_GCODE_2DBEZIER_MOVE = 'B02';
  STR_GCODE_3DBEZIER_MOVE = 'B03';
  STR_GCODE_DRILL_UP = 'P01';
  STR_GCODE_DRILL_DOWN = 'P02';

{ TvAvisoCNCGCodeReader }

{@@
  Reads a string and separates it in substring
  using ASeparator to delimite them.

  Limits:

  Number of substrings: 10 (indexed 0 to 9)
  Length of each substring: 255 (they are shortstrings)
}
function TvAvisoCNCGCodeReader.SeparateString(AString: string; ASeparator: Char): T10Strings;
var
  i, CurrentPart: Integer;
begin
  CurrentPart := 0;

  { Clears the result }
  for i := 0 to 9 do Result[i] := '';

  { Iterates througth the string, filling strings }
  for i := 1 to Length(AString) do
  begin
    if Copy(AString, i, 1) = ASeparator then
    begin
      Inc(CurrentPart);

      { Verifies if the string capacity wasn't exceeded }
      if CurrentPart > 9 then Exit;
    end
    else
      Result[CurrentPart] := Result[CurrentPart] + Copy(AString, i, 1);
  end;
end;

procedure TvAvisoCNCGCodeReader.ReadString(AStr: string;
  AData: TvVectorialPage);
var
  AParams: T10Strings;
  DestX, DestY, DestZ: Double;
  i: Integer;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn('TvAvisoCNCGCodeReader.ReadString ', AStr);
  {$endif}
  AParams := SeparateString(AStr, ' ');

  {
    Format may be:
    G01 X3
    G01 X3 Y4
    G01 X3 Y4 Z2
  }
  if AParams[0] = STR_GCODE_DRILL_UP then
  begin
    AData.AddLineToPath(LastX, LastY, 0);
    LastZ := 0;
  end
  else if AParams[0] = STR_GCODE_DRILL_DOWN then
  begin
    AData.AddLineToPath(LastX, LastY, 50);
    LastZ := 50;
  end
  else if AParams[0] = STR_GCODE_LINEAR_MOVE then
  begin
    DestX := LastX;
    DestY := LastY;
    DestZ := LastZ;

    for i := 1 to 3 do
    begin
      case GetCoordinate(AParams[i]) of
      INT_COORDINATE_X: DestX := GetCoordinateValue(AParams[i]);
      INT_COORDINATE_Y: DestY := GetCoordinateValue(AParams[i]);
      INT_COORDINATE_Z: DestZ := GetCoordinateValue(AParams[i]);
      else
        // error
      end;
    end;

    AData.AddLineToPath(DestX, DestY, DestZ);

    LastX := DestX;
    LastY := DestY;
    LastZ := DestZ;
  end
  else if AParams[0] = STR_GCODE_2DBEZIER_MOVE then
  begin
    AData.AddBezierToPath(
      GetCoordinateValue(AParams[1]),
      GetCoordinateValue(AParams[2]),
      GetCoordinateValue(AParams[3]),
      GetCoordinateValue(AParams[4]),
      GetCoordinateValue(AParams[5]),
      GetCoordinateValue(AParams[6])
      );

    LastX := GetCoordinateValue(AParams[5]);
    LastY := GetCoordinateValue(AParams[6]);
  end
  else if AParams[0] = STR_GCODE_3DBEZIER_MOVE then
  begin
    AData.AddBezierToPath(
      GetCoordinateValue(AParams[1]),
      GetCoordinateValue(AParams[2]),
      GetCoordinateValue(AParams[3]),
      GetCoordinateValue(AParams[4]),
      GetCoordinateValue(AParams[5]),
      GetCoordinateValue(AParams[6]),
      GetCoordinateValue(AParams[7]),
      GetCoordinateValue(AParams[8]),
      GetCoordinateValue(AParams[9])
      );

    LastX := GetCoordinateValue(AParams[7]);
    LastY := GetCoordinateValue(AParams[8]);
    LastZ := GetCoordinateValue(AParams[9]);
  end;
  {else
  begin
     Ignore any of these codes:

      STR_GCODE_STEPPER_MOVE

      and anything else
  end;}
end;

function TvAvisoCNCGCodeReader.GetCoordinate(AStr: shortstring): Integer;
begin
  Result := INT_COORDINATE_NONE;

  if AStr = '' then Exit
  else if AStr[1] = 'X' then Result := INT_COORDINATE_X
  else if AStr[1] = 'Y' then Result := INT_COORDINATE_Y
  else if AStr[1] = 'Z' then Result := INT_COORDINATE_Z;
end;

function TvAvisoCNCGCodeReader.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;

  if Length(AStr) <= 1 then Exit;

  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1));
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}
procedure TvAvisoCNCGCodeReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i: Integer;
  FirstPage: TvVectorialPage;
begin
  {$ifdef FPVECTORIALDEBUG}
  WriteLn('TvAvisoCNCGCodeReader.ReadFromStrings AStrings = ', PtrInt(AStrings), ' AData = ', PtrInt(AData));
  {$endif}

  FirstPage := AData.AddPage();
  FirstPage.StartPath(0, 0);

  for i := 0 to AStrings.Count - 1 do
    ReadString(AStrings.Strings[i], FirstPage);

  {$ifdef FPVECTORIALDEBUG}
  WriteLn('AData.EndPath');
  {$endif}
  FirstPage.EndPath();
end;

initialization

  RegisterVectorialReader(TvAvisoCNCGCodeReader, vfGCodeAvisoCNCPrototipoV5);

end.

