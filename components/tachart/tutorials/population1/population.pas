unit population;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPopulationRecord = record
    Age: Integer;
    Total: Double;
    Male: Double;
    Female: Double;
    Ratio: Double;
  end;

type
  TPopulationArray = array of TPopulationRecord;

procedure LoadPopulationData(const AFileName: String; var Data: TPopulationArray);


implementation

procedure LoadPopulationData(const AFileName: String; var Data: TPopulationArray);

  function StripThousandSep(const s: String): String;
  var
    i: Integer;
  begin
    Result := s;
    for i:=Length(Result) downto 1 do
      if Result[i] = ',' then
        Delete(Result, i, 1);
  end;

var
  List1, List2: TStringList;
  i, j, n: Integer;
  s: String;
  ds: char;
begin
  ds := FormatSettings.DecimalSeparator;
  List1 := TStringList.Create;
  try
    List1.LoadFromFile(AFileName);
    n := List1.Count;
    SetLength(Data, n-2);
    FormatSettings.DecimalSeparator := '.';
    List2 := TStringList.Create;
    try
      List2.Delimiter := #9;
      List2.StrictDelimiter := true;
      j := 0;
      for i:=2 to n-1 do begin
        List2.DelimitedText := List1[i];
        s := List1[i];
        with Data[j] do begin
          if i < n-1 then
            Age := StrToInt(trim(List2[0]))
          else
            Age := 100;
          Total := StrToFloat(StripThousandSep(trim(List2[1])));
          Male := StrToFloat(StripThousandSep(trim(List2[2])));
          Female := StrToFloat(StripThousandSep(trim(List2[3])));
          Ratio := StrToFloat(trim(List2[4]));
        end;
        inc(j);
      end;
    finally
      List2.Free;
    end;
  finally
    FormatSettings.DecimalSeparator := ds;
    List1.Free;
  end;
end;

end.