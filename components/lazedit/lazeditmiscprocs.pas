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
  (* according to UnicodeData.txt 16.0
  *)
  Result := False;
  case AChar[0] of
    #$CC:
      Result := True;
    #$CD:
      if (AChar[1] <= #$AF) then Result := True;                              // 0340..036F
    #$D2:
      if (AChar[1] in [#$83..#$87]) then Result := True;                      // 0483..0487
    #$D6:
      if (AChar[1] in [#$91..#$BD, #$BF]) then Result := True;                // 0591..05BD, 05BF
    #$D7:
      if (AChar[1] in [#$81..#$82, #$84..#$85, #$87]) then Result := True;    // 05C1..05C2, 05C4..05C5, 05C7
    #$D8:
      if (AChar[1] in [#$90..#$9A]) then Result := True;                      // 0610..061A
    #$D9:
      if (AChar[1] in [#$8B..#$9F, #$B0]) then Result := True;                // 064B..065F, 0670
    #$DB:
      if (AChar[1] in [#$96..#$9C, #$9F..#$A4, #$A7..#$A8, #$AA..#$AD]) then Result := True;   // 06D6..06DC, 06DF..06E4, 06E7..06E8, 06EA..06ED
    #$DC:
      if (AChar[1] in [#$91, #$B0..#$BF]) then Result := True;                // 0711, 0730..073F
    #$DD:
      if (AChar[1] <= #$8A) then Result := True;                              // 0740..074A
    #$DF:
      if (AChar[1] in [#$AB..#$B3, #$BD]) then Result := True;                // 07EB..07F3, 07FD
    #$E0:
      case AChar[1] of
        #$A0:
          if (AChar[2] in [#$96..#$99, #$9B..#$A3, #$A5..#$A7, #$A9..#$AD]) then Result := True;   // 0816..0819, 081B..0823, 0825..0827, 0829..082D
        #$A1:
          if (AChar[2] in [#$99..#$9B]) then Result := True;                  // 0859..085B
        #$A2:
          if (AChar[2] in [#$97..#$9F]) then Result := True;                  // 0897..089F
        #$A3:
          if (AChar[2] in [#$8A..#$A1, #$A3..#$BF]) then Result := True;      // 08CA..08E1, 08E3..08FF
        #$A4:
          if (AChar[2] in [#$BC]) then Result := True;                        // 093C
        #$A5:
          if (AChar[2] in [#$8D, #$91..#$94]) then Result := True;            // 094D, 0951..0954
        #$A6:
          if (AChar[2] in [#$BC]) then Result := True;                        // 09BC
        #$A7:
          if (AChar[2] in [#$8D, #$BE]) then Result := True;                  // 09CD, 09FE
        #$A8:
          if (AChar[2] in [#$BC]) then Result := True;                        // 0A3C
        #$A9:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0A4D
        #$AA:
          if (AChar[2] in [#$BC]) then Result := True;                        // 0ABC
        #$AB:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0ACD
        #$AC:
          if (AChar[2] in [#$BC]) then Result := True;                        // 0B3C
        #$AD:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0B4D
        #$AF:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0BCD
        #$B0:
          if (AChar[2] in [#$BC]) then Result := True;                        // 0C3C
        #$B1:
          if (AChar[2] in [#$8D, #$95..#$96]) then Result := True;            // 0C4D, 0C55..0C56
        #$B2:
          if (AChar[2] in [#$BC]) then Result := True;                        // 0CBC
        #$B3:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0CCD
        #$B4:
          if (AChar[2] in [#$BB..#$BC]) then Result := True;                  // 0D3B..0D3C
        #$B5:
          if (AChar[2] in [#$8D]) then Result := True;                        // 0D4D
        #$B7:
          if (AChar[2] in [#$8A]) then Result := True;                        // 0DCA
        #$B8:
          if (AChar[2] in [#$B8..#$BA]) then Result := True;                  // 0E38..0E3A
        #$B9:
          if (AChar[2] in [#$88..#$8B]) then Result := True;                  // 0E48..0E4B
        #$BA:
          if (AChar[2] in [#$B8..#$BA]) then Result := True;                  // 0EB8..0EBA
        #$BB:
          if (AChar[2] in [#$88..#$8B]) then Result := True;                  // 0EC8..0ECB
        #$BC:
          if (AChar[2] in [#$98..#$99, #$B5, #$B7, #$B9]) then Result := True;   // 0F18..0F19, 0F35, 0F37, 0F39
        #$BD:
          if (AChar[2] in [#$B1..#$B2, #$B4, #$BA..#$BD]) then Result := True;   // 0F71..0F72, 0F74, 0F7A..0F7D
        #$BE:
          if (AChar[2] in [#$80, #$82..#$84, #$86..#$87]) then Result := True;   // 0F80, 0F82..0F84, 0F86..0F87
        #$BF:
          if (AChar[2] in [#$86]) then Result := True;                        // 0FC6
      end;
    #$E1:
      case AChar[1] of
        #$80:
          if (AChar[2] in [#$B7, #$B9..#$BA]) then Result := True;            // 1037, 1039..103A
        #$82:
          if (AChar[2] in [#$8D]) then Result := True;                        // 108D
        #$8D:
          if (AChar[2] in [#$9D..#$9F]) then Result := True;                  // 135D..135F
        #$9C:
          if (AChar[2] in [#$94..#$95, #$B4]) then Result := True;            // 1714..1715, 1734
        #$9F:
          if (AChar[2] in [#$92, #$9D]) then Result := True;                  // 17D2, 17DD
        #$A2:
          if (AChar[2] in [#$A9]) then Result := True;                        // 18A9
        #$A4:
          if (AChar[2] in [#$B9..#$BB]) then Result := True;                  // 1939..193B
        #$A8:
          if (AChar[2] in [#$97..#$98]) then Result := True;                  // 1A17..1A18
        #$A9:
          if (AChar[2] in [#$A0, #$B5..#$BC, #$BF]) then Result := True;      // 1A60, 1A75..1A7C, 1A7F
        #$AA:
          if (AChar[2] >= #$B0) then Result := True;                          // 1AB0..1ABF
        #$AB:
          Result := True;
        #$AC:
          if (AChar[2] in [#$B4]) then Result := True;                        // 1B34
        #$AD:
          if (AChar[2] in [#$84, #$AB..#$B3]) then Result := True;            // 1B44, 1B6B..1B73
        #$AE:
          if (AChar[2] in [#$AA..#$AB]) then Result := True;                  // 1BAA..1BAB
        #$AF:
          if (AChar[2] in [#$A6, #$B2..#$B3]) then Result := True;            // 1BE6, 1BF2..1BF3
        #$B0:
          if (AChar[2] in [#$B7]) then Result := True;                        // 1C37
        #$B3:
          if (AChar[2] in [#$90..#$92, #$94..#$A0, #$A2..#$A8, #$AD, #$B4, #$B8..#$B9]) then Result := True;   // 1CD0..1CD2, 1CD4..1CE0, 1CE2..1CE8, 1CED, 1CF4, 1CF8..1CF9
        #$B7:
          Result := True;                                                    // 1DC0..1DFF
      end;
    #$E2:
      case AChar[1] of
        #$83:
          if (AChar[2] in [#$90..#$BF]) then Result := True;                  // 20D0..20FF
        #$B3:
          if (AChar[2] in [#$AF..#$B1]) then Result := True;                  // 2CEF..2CF1
        #$B5:
          if (AChar[2] in [#$BF]) then Result := True;                        // 2D7F
        #$B7:
          if (AChar[2] in [#$A0..#$BF]) then Result := True;                  // 2DE0..2DFF
      end;
    #$E3:
      case AChar[1] of
        #$80:
          if (AChar[2] in [#$AA..#$AF]) then Result := True;                  // 302A..302F
        #$82:
          if (AChar[2] in [#$99..#$9A]) then Result := True;                  // 3099..309A
      end;
    #$EA:
      case AChar[1] of
        #$99:
          if (AChar[2] in [#$AF, #$B4..#$BD]) then Result := True;            // A66F, A674..A67D
        #$9A:
          if (AChar[2] in [#$9E..#$9F]) then Result := True;                  // A69E..A69F
        #$9B:
          if (AChar[2] in [#$B0..#$B1]) then Result := True;                  // A6F0..A6F1
        #$A0:
          if (AChar[2] in [#$86, #$AC]) then Result := True;                  // A806, A82C
        #$A3:
          if (AChar[2] in [#$84, #$A0..#$B1]) then Result := True;            // A8C4, A8E0..A8F1
        #$A4:
          if (AChar[2] in [#$AB..#$AD]) then Result := True;                  // A92B..A92D
        #$A5:
          if (AChar[2] in [#$93]) then Result := True;                        // A953
        #$A6:
          if (AChar[2] in [#$B3]) then Result := True;                        // A9B3
        #$A7:
          if (AChar[2] in [#$80]) then Result := True;                        // A9C0
        #$AA:
          if (AChar[2] in [#$B0, #$B2..#$B4, #$B7..#$B8, #$BE..#$BF]) then Result := True;   // AAB0, AAB2..AAB4, AAB7..AAB8, AABE..AABF
        #$AB:
          if (AChar[2] in [#$81, #$B6]) then Result := True;                  // AAC1, AAF6
        #$AF:
          if (AChar[2] in [#$AD]) then Result := True;                        // ABED
      end;
    #$EF:
      case AChar[1] of
        #$AC:
          if (AChar[2] in [#$9E]) then Result := True;                        // FB1E
        #$B8:
          if (AChar[2] in [#$A0..#$AF]) then Result := True;                  // FE20..FE2F
      end;
    #$F0:
      case AChar[1] of
        #$90:
          case AChar[2] of
            #$87:
              if (AChar[3] in [#$BD]) then Result := True;                    // 101FD
            #$8B:
              if (AChar[3] in [#$A0]) then Result := True;                    // 102E0
            #$8D:
              if (AChar[3] in [#$B6..#$BA]) then Result := True;              // 10376..1037A
            #$A8:
              if (AChar[3] in [#$8D, #$8F, #$B8..#$BA, #$BF]) then Result := True;   // 10A0D, 10A0F, 10A38..10A3A, 10A3F
            #$AB:
              if (AChar[3] in [#$A5..#$A6]) then Result := True;              // 10AE5..10AE6
            #$B4:
              if (AChar[3] in [#$A4..#$A7]) then Result := True;              // 10D24..10D27
            #$B5:
              if (AChar[3] in [#$A9..#$AD]) then Result := True;              // 10D69..10D6D
            #$BA:
              if (AChar[3] in [#$AB..#$AC]) then Result := True;              // 10EAB..10EAC
            #$BB:
              if (AChar[3] in [#$BD..#$BF]) then Result := True;              // 10EFD..10EFF
            #$BD:
              if (AChar[3] in [#$86..#$90]) then Result := True;              // 10F46..10F50
            #$BE:
              if (AChar[3] in [#$82..#$85]) then Result := True;              // 10F82..10F85
          end;
        #$91:
          case AChar[2] of
            #$81:
              if (AChar[3] in [#$86, #$B0, #$BF]) then Result := True;        // 11046, 11070, 1107F
            #$82:
              if (AChar[3] in [#$B9..#$BA]) then Result := True;              // 110B9..110BA
            #$84:
              if (AChar[3] in [#$80..#$82, #$B3..#$B4]) then Result := True;   // 11100..11102, 11133..11134
            #$85:
              if (AChar[3] in [#$B3]) then Result := True;                    // 11173
            #$87:
              if (AChar[3] in [#$80, #$8A]) then Result := True;              // 111C0, 111CA
            #$88:
              if (AChar[3] in [#$B5..#$B6]) then Result := True;              // 11235..11236
            #$8B:
              if (AChar[3] in [#$A9..#$AA]) then Result := True;              // 112E9..112EA
            #$8C:
              if (AChar[3] in [#$BB..#$BC]) then Result := True;              // 1133B..1133C
            #$8D:
              if (AChar[3] in [#$8D, #$A6..#$AC, #$B0..#$B4]) then Result := True;   // 1134D, 11366..1136C, 11370..11374
            #$8F:
              if (AChar[3] in [#$8E..#$90]) then Result := True;              // 113CE..113D0
            #$91:
              if (AChar[3] in [#$82, #$86, #$9E]) then Result := True;        // 11442, 11446, 1145E
            #$93:
              if (AChar[3] in [#$82..#$83]) then Result := True;              // 114C2..114C3
            #$96:
              if (AChar[3] >= #$BF) then Result := True;                      // 115BF
            #$97:
              if (AChar[3] <= #$80) then Result := True;                      // 115C0
            #$98:
              if (AChar[3] in [#$BF]) then Result := True;                    // 1163F
            #$9A:
              if (AChar[3] in [#$B6..#$B7]) then Result := True;              // 116B6..116B7
            #$9C:
              if (AChar[3] in [#$AB]) then Result := True;                    // 1172B
            #$A0:
              if (AChar[3] in [#$B9..#$BA]) then Result := True;              // 11839..1183A
            #$A4:
              if (AChar[3] in [#$BD..#$BE]) then Result := True;              // 1193D..1193E
            #$A5:
              if (AChar[3] in [#$83]) then Result := True;                    // 11943
            #$A7:
              if (AChar[3] in [#$A0]) then Result := True;                    // 119E0
            #$A8:
              if (AChar[3] in [#$B4]) then Result := True;                    // 11A34
            #$A9:
              if (AChar[3] in [#$87]) then Result := True;                    // 11A47
            #$AA:
              if (AChar[3] in [#$99]) then Result := True;                    // 11A99
            #$B0:
              if (AChar[3] in [#$BF]) then Result := True;                    // 11C3F
            #$B5:
              if (AChar[3] in [#$82, #$84..#$85]) then Result := True;        // 11D42, 11D44..11D45
            #$B6:
              if (AChar[3] in [#$97]) then Result := True;                    // 11D97
            #$BD:
              if (AChar[3] in [#$81..#$82]) then Result := True;              // 11F41..11F42
          end;
        #$96:
          case AChar[2] of
            #$84:
              if (AChar[3] in [#$AF]) then Result := True;                    // 1612F
            #$AB:
              if (AChar[3] in [#$B0..#$B4]) then Result := True;              // 16AF0..16AF4
            #$AC:
              if (AChar[3] in [#$B0..#$B6]) then Result := True;              // 16B30..16B36
            #$BF:
              if (AChar[3] in [#$B0..#$B1]) then Result := True;              // 16FF0..16FF1
          end;
        #$9B:
          case AChar[2] of
            #$B2:
              if (AChar[3] in [#$9E]) then Result := True;                    // 1BC9E
          end;
        #$9D:
          case AChar[2] of
            #$85:
              if (AChar[3] in [#$A5..#$A9, #$AD..#$B2, #$BB..#$BF]) then Result := True;   // 1D165..1D169, 1D16D..1D172, 1D17B..1D17F
            #$86:
              if (AChar[3] in [#$80..#$82, #$85..#$8B, #$AA..#$AD]) then Result := True;   // 1D180..1D182, 1D185..1D18B, 1D1AA..1D1AD
            #$89:
              if (AChar[3] in [#$82..#$84]) then Result := True;              // 1D242..1D244
          end;
        #$9E:
          case AChar[2] of
            #$80:
              if (AChar[3] in [#$80..#$86, #$88..#$98, #$9B..#$A1, #$A3..#$A4, #$A6..#$AA]) then Result := True;   // 1E000..1E006, 1E008..1E018, 1E01B..1E021, 1E023..1E024, 1E026..1E02A
            #$82:
              if (AChar[3] in [#$8F]) then Result := True;                    // 1E08F
            #$84:
              if (AChar[3] in [#$B0..#$B6]) then Result := True;              // 1E130..1E136
            #$8A:
              if (AChar[3] in [#$AE]) then Result := True;                    // 1E2AE
            #$8B:
              if (AChar[3] in [#$AC..#$AF]) then Result := True;              // 1E2EC..1E2EF
            #$93:
              if (AChar[3] in [#$AC..#$AF]) then Result := True;              // 1E4EC..1E4EF
            #$97:
              if (AChar[3] in [#$AE..#$AF]) then Result := True;              // 1E5EE..1E5EF
            #$A3:
              if (AChar[3] in [#$90..#$96]) then Result := True;              // 1E8D0..1E8D6
            #$A5:
              if (AChar[3] in [#$84..#$8A]) then Result := True;              // 1E944..1E94A
          end;
        #$9F:
          case AChar[2] of
            #$8F:
              if (AChar[3] in [#$BB..#$BF]) then Result := True;              // 1F3FB..1F3FF // EMOJI modifier
          end;
      end;
  end;
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

