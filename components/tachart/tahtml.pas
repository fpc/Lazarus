unit TAHtml;

{$MODE ObjFPC}{$H+}

interface

uses
  fpimage, Classes;

function ReplaceHTMLEntities(const AText: String): String;
function HTMLToFontSize(AText: String): Integer;
function HTMLToFPColor(const AText: String): TFPColor;


implementation

uses
  SysUtils, htmldefs, LazUTF8,
  TAChartUtils;

function ReplaceHTMLEntities(const AText: String): String;
var
  i: Integer;
  s: WideString;
  wch: WideChar = #0;
begin
  Result := '';
  i := 1;
  while (i <= Length(AText)) do
  begin
    case AText[i] of
      '&': begin
             s := '';
             inc(i);
             while (i <= Length(AText)) and (AText[i] <> ';') do begin
               s := s + WideChar(AText[i]);
               inc(i);
             end;
             if ResolveHTMLEntityReference(s, wch) then
               Result := Result + UnicodeToUTF8(cardinal(wch));
           end;
      else Result := Result + AText[i];
    end;
    inc(i);
  end;
end;

function HTMLToFPColor(const AText: String): TFPColor;
var
  i: Integer;
  len: Integer;
begin
  Result := colBlack;
  // AText is already upper-cased by the calling routine.
  case AText of
    'AQUA'   : Result := colAqua;
    'BLACK'  : Result := colBlack;
    'BLUE'   : Result := colBlue;
    'CYAN'   : Result := colCyan;
    'FUCHSIA': Result := colFuchsia;
    'GRAY'   : Result := colGray;
    'GREY'   : Result := colGray;
    'GREEN'  : Result := colGreen;
    'LIME'   : Result := colLime;
    'MAGENTA': Result := colMagenta;
    'MAROON' : Result := colMaroon;
    'NAVY'   : Result := colNavy;
    'OLIVE'  : Result := colOlive;
    'PURPLE' : Result := colPurple;
    'RED'    : Result := colRed;
    'SILVER' : Result := colSilver;
    'TEAL'   : Result := colTeal;
    'WHITE'  : Result := colWhite;
    'YELLOW' : Result := colYellow;
    else       if (pos('#', AText) = 1) then begin
                 len := Length(AText);
                 if not (len in [7, 4]) then
                   exit;
                 for i:=2 to len do
                   if not (AText[i] in ['0'..'9', 'A'..'F']) then
                     exit;
                 if len = 7 then begin
                   Result.Red := StrToInt('$' + copy(AText, 2, 2)) shl 8;
                   Result.Green := StrToInt('$' + copy(AText, 4, 2)) shl 8;
                   Result.Blue := StrToInt('$' + copy(AText, 6, 2)) shl 8;
                 end else
                 if len = 4 then begin
                   Result.Red := StrToInt('$' + AText[2] + AText[2]) shl 8;
                   Result.Green := StrToInt('$' + AText[3] + AText[3]) shl 8;
                   Result.Blue := StrToInt('$' + AText[4] + AText[4]) shl 8;
                 end;
               end;
  end;
end;

function HTMLToFontSize(AText: String): Integer;
begin
  case AText of
    'X-SMALL',  '1' : Result := 7;
    'SMALL',    '2' : Result := 10;
    'MEDIUM',   '3' : Result := 12;
    'LARGE',    '4' : Result := 14;
    'X-LARGE',  '5' : Result := 18;
    'XX-LARGE', '6' : Result := 24;
  else
    if Pos('PT', AText) = Length(AText)-1 then
      Result := StrToInt(Copy(AText, 1, Length(AText) - 2))
    else
    if Pos('PX', AText) = Length(AText)-1 then
    begin
      Result := StrToInt(Copy(AText, 1, Length(AText) - 2));
      Result := Result * 72 div 96;  // Assuming a 96 ppi screen here!
    end else
      Result := 9;
  end;
end;

end.

