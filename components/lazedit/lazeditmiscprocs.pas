{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
 *****************************************************************************

  Written by Martin Friebe
}
unit LazEditMiscProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function IsCombiningCodePoint(const AChar: PChar): Boolean;

function IsSpaceChar(AText: PChar): Boolean; inline;
function CountLeadSpace(AText: PChar): integer; inline;
function CountLeadWhiteSpace(AText: PChar): integer; inline;
function CountBackwardWhiteSpace(AText: PChar; AStart: Integer): integer; inline;
function CountLeadWhiteSpace(AText: PChar; out AnHasTab: boolean): integer; inline;

function CountChars(AText: PChar; AByteLen: integer): integer;
function CountBytes(AText: PChar; ACharLen: Integer; AMaxBytes: integer = high(Integer)): integer;

Operator =  (P1, P2 : TPoint) : Boolean; inline;
Operator <  (P1, P2 : TPoint) : Boolean; inline;
Operator <= (P1, P2 : TPoint) : Boolean; inline;
Operator >  (P1, P2 : TPoint) : Boolean; inline;
Operator >= (P1, P2 : TPoint) : Boolean; inline;
function Min(P1, P2 : TPoint) : TPoint; inline; overload;
function Max(P1, P2 : TPoint) : TPoint; inline; overload;

implementation

function IsCombiningCodePoint(const AChar: PChar): Boolean;
begin
  Result := (
   ( (AChar[0] = #$CC) ) or                                                       // Combining Diacritical Marks (belongs to previos char) 0300-036F
   ( (AChar[0] = #$CD) and (AChar[1] in [#$80..#$AF]) ) or                        // Combining Diacritical Marks
   ( (AChar[0] = #$D8) and (AChar[1] in [#$90..#$9A]) ) or                        // Arabic 0610 (d890)..061A (d89a)
   ( (AChar[0] = #$D9) and (AChar[1] in [#$8b..#$9f, #$B0]) ) or                  // Arabic 064B (d98b)..065F (d99f) // 0670 (d9b0)
   ( (AChar[0] = #$DB) and (AChar[1] in [#$96..#$9C, #$9F..#$A4, #$A7..#$A8, #$AA..#$AD]) ) or // Arabic 06D6 (db96)..  .. ..06EA (dbaa)
   ( (AChar[0] = #$E0) and (AChar[1] = #$A3) and (AChar[2] in [#$A4..#$BE]) ) or  // Arabic 08E4 (e0a3a4) ..08FE (e0a3be)
   ( (AChar[0] = #$E1) and (AChar[1] = #$B7) ) or                                 // Combining Diacritical Marks Supplement 1DC0-1DFF
   ( (AChar[0] = #$E2) and (AChar[1] = #$83) and (AChar[2] in [#$90..#$FF]) ) or  // Combining Diacritical Marks for Symbols 20D0-20FF
   ( (AChar[0] = #$EF) and (AChar[1] = #$B8) and (AChar[2] in [#$A0..#$AF]) )     // Combining half Marks FE20-FE2F
  );
end;

function IsSpaceChar(AText: PChar): Boolean;
begin
  Result := (AText^ = ' ') and not IsCombiningCodePoint(AText);
end;

function CountLeadSpace(AText: PChar): integer;
var
  Run : PChar;
begin
  Run := AText;
  while (Run^ in [' ']) do
    Inc(Run);
  Result := Run - AText;
  if (Result > 0) and IsCombiningCodePoint(Run) then
    dec(Result);
end;

function CountLeadWhiteSpace(AText: PChar): integer;
var
  Run : PChar;
begin
  Run := AText;
  while (Run^ in [' ', #9]) do
    Inc(Run);
  Result := Run - AText;
  if (Result > 0) and IsCombiningCodePoint(Run) then
    dec(Result);
end;

function CountBackwardWhiteSpace(AText: PChar; AStart: Integer): integer;
var
  Run : PChar;
begin
  Run := AText+AStart-1;
  while (Run >=AText) and (Run^ in [' ', #9]) do
    Dec(Run);
  Result := AText + AStart - 1 - Run;
end;

function CountLeadWhiteSpace(AText: PChar; out AnHasTab: boolean): integer;
var
  Run : PChar;
begin
  Run := AText;
  while (Run^ = ' ') do
    Inc(Run);
  AnHasTab := Run^ = #9;
  while (Run^ in [' ', #9]) do
    Inc(Run);
  Result := Run - AText;
  if (Result > 0) and IsCombiningCodePoint(Run) then
    dec(Result);
end;

function CountChars(AText: PChar; AByteLen: integer): integer;
var
  b: Byte;
begin
  Result := 0;
  while AByteLen > 0 do begin
    b := Byte(AText^);
    if (b < 128) or
       ((b >= 192) and not IsCombiningCodePoint(AText))
    then
      inc(Result);
    inc(AText);
    dec(AByteLen);
  end;
end;

function CountBytes(AText: PChar; ACharLen: Integer; AMaxBytes: integer): integer;
var
  b: Byte;
begin
  Result := 0;
  while AMaxBytes > 0 do begin
    b := Byte(AText^);
    if b = 0 then
      exit;
    if (b < 128) or
       ((b >= 192) and not IsCombiningCodePoint(AText))
    then begin
      if ACharLen = 0 then
        exit;
      dec(ACharLen);
    end;
    inc(AText);
    inc(Result);
    dec(AMaxBytes);
  end;
end;

Operator = (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y = P2.Y) and (P1.X = P2.X);
end;

Operator < (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y < P2.Y) or ( (P1.Y = P2.Y) and (P1.X < P2.X) );
end;

Operator <= (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y < P2.Y) or ( (P1.Y = P2.Y) and (P1.X <= P2.X) );
end;

Operator > (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y > P2.Y) or ( (P1.Y = P2.Y) and (P1.X > P2.X) );
end;

Operator >= (P1, P2 : TPoint) : Boolean;
begin
  Result := (P1.Y > P2.Y) or ( (P1.Y = P2.Y) and (P1.X >= P2.X) );
end;

function Min(P1, P2: TPoint): TPoint;
begin
  if P1 < P2
  then Result := P1
  else Result := P2;
end;

function Max(P1, P2: TPoint): TPoint;
begin
  if P1 > P2
  then Result := P1
  else Result := P2;
end;

end.

