{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
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

function ToIdx(APos: Integer): Integer; inline;
function ToPos(AIdx: Integer): Integer; inline;
function YToIdx(APointWithYPos: TPoint): TPoint; inline;
function YToPos(APointWithYIdx: TPoint): TPoint; inline;

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
      if (AChar[1] <= #$AF) then Result := True;                              // 0300..036F 0340..036F
    #$D2:
      if (AChar[1] in [#$83..#$87]) then Result := True;                      // 0483..0487 0483..0487
    #$D6:
      if (AChar[1] in [#$91..#$BD, #$BF]) then Result := True;                // 0591..05BD 0591..05BD, 05BF, 05BF
    #$D7:
      if (AChar[1] in [#$81..#$82, #$84..#$85, #$87..#$89]) then Result := True;   // 05C1..05C2 05C1..05C2, 05C4..05C5, 05C4..05C5, 05C7..05C9, 05C7..05C9
    #$D8:
      if (AChar[1] in [#$90..#$9A]) then Result := True;                      // 0610..061A 0610..061A
    #$D9:
      if (AChar[1] in [#$8B..#$9F, #$B0]) then Result := True;                // 064B..065F 064B..065F, 0670, 0670
    #$DB:
      if (AChar[1] in [#$96..#$9C, #$9F..#$A4, #$A7..#$A8, #$AA..#$AD]) then Result := True;   // 06D6..06DC 06D6..06DC, 06DF..06E4, 06DF..06E4, 06E7..06E8, 06E7..06E8, 06EA..06ED, 06EA..06ED
    #$DC:
      if (AChar[1] in [#$91, #$B0..#$BF]) then Result := True;                // 0711 0711, 0730..073F, 0730..074A
    #$DD:
      if (AChar[1] <= #$8A) then Result := True;                              // 0730..074A 0740..074A
    #$DE:
      if (AChar[1] in [#$A6..#$B0]) then Result := True;                      // 07A6..07B0 07A6..07B0
    #$DF:
      if (AChar[1] in [#$AB..#$B3, #$BD]) then Result := True;                // 07EB..07F3 07EB..07F3, 07FD, 07FD
    #$E0:
      case AChar[1] of
        #$A0:
          if (AChar[2] in [#$96..#$99, #$9B..#$A3, #$A5..#$A7, #$A9..#$AD]) then Result := True;   // 0816..0819 0816..0819, 081B..0823, 081B..0823, 0825..0827, 0825..0827, 0829..082D, 0829..082D
        #$A1:
          if (AChar[2] in [#$99..#$9B]) then Result := True;                  // 0859..085B 0859..085B
        #$A2:
          if (AChar[2] in [#$97..#$9F]) then Result := True;                  // 0897..089F 0897..089F
        #$A3:
          if (AChar[2] in [#$8A..#$A1, #$A3..#$BF]) then Result := True;      // 08CA..08E1 08CA..08E1, 08E3..08FF, 08E3..0902
        #$A4:
          if (AChar[2] in [#$80..#$82, #$BA, #$BC]) then Result := True;      // 08E3..0902 0900..0902, 093A, 093A, 093C, 093C
        #$A5:
          if (AChar[2] in [#$81..#$88, #$8D, #$91..#$97, #$A2..#$A3]) then Result := True;   // 0941..0948 0941..0948, 094D, 094D, 0951..0957, 0951..0957, 0962..0963, 0962..0963
        #$A6:
          if (AChar[2] in [#$81, #$BC]) then Result := True;                  // 0981 0981, 09BC, 09BC
        #$A7:
          if (AChar[2] in [#$81..#$84, #$8D, #$A2..#$A3, #$BE]) then Result := True;   // 09C1..09C4 09C1..09C4, 09CD, 09CD, 09E2..09E3, 09E2..09E3, 09FE, 09FE
        #$A8:
          if (AChar[2] in [#$81..#$82, #$BC]) then Result := True;            // 0A01..0A02 0A01..0A02, 0A3C, 0A3C
        #$A9:
          if (AChar[2] in [#$81..#$82, #$87..#$88, #$8B..#$8D, #$91, #$B0..#$B1, #$B5]) then Result := True;   // 0A41..0A42 0A41..0A42, 0A47..0A48, 0A47..0A48, 0A4B..0A4D, 0A4B..0A4D, 0A51, 0A51, 0A70..0A71, 0A70..0A71, 0A75, 0A75
        #$AA:
          if (AChar[2] in [#$81..#$82, #$BC]) then Result := True;            // 0A81..0A82 0A81..0A82, 0ABC, 0ABC
        #$AB:
          if (AChar[2] in [#$81..#$85, #$87..#$88, #$8D, #$A2..#$A3, #$BA..#$BF]) then Result := True;   // 0AC1..0AC5 0AC1..0AC5, 0AC7..0AC8, 0AC7..0AC8, 0ACD, 0ACD, 0AE2..0AE3, 0AE2..0AE3, 0AFA..0AFF, 0AFA..0AFF
        #$AC:
          if (AChar[2] in [#$81, #$BC, #$BF]) then Result := True;            // 0B01 0B01, 0B3C, 0B3C, 0B3F, 0B3F
        #$AD:
          if (AChar[2] in [#$81..#$84, #$8D, #$93..#$96, #$A2..#$A3]) then Result := True;   // 0B41..0B44 0B41..0B44, 0B4D, 0B4D, 0B53..0B56, 0B53..0B56, 0B62..0B63, 0B62..0B63
        #$AE:
          if (AChar[2] in [#$82]) then Result := True;                        // 0B82 0B82
        #$AF:
          if (AChar[2] in [#$80, #$8D]) then Result := True;                  // 0BC0 0BC0, 0BCD, 0BCD
        #$B0:
          if (AChar[2] in [#$80, #$84, #$BC, #$BE..#$BF]) then Result := True;   // 0C00 0C00, 0C04, 0C04, 0C3C, 0C3C, 0C3E..0C3F, 0C3E..0C40
        #$B1:
          if (AChar[2] in [#$80, #$86..#$88, #$8A..#$8D, #$95..#$96, #$A2..#$A3]) then Result := True;   // 0C3E..0C40 0C40, 0C46..0C48, 0C46..0C48, 0C4A..0C4D, 0C4A..0C4D, 0C55..0C56, 0C55..0C56, 0C62..0C63, 0C62..0C63
        #$B2:
          if (AChar[2] in [#$81, #$BC, #$BF]) then Result := True;            // 0C81 0C81, 0CBC, 0CBC, 0CBF, 0CBF
        #$B3:
          if (AChar[2] in [#$86, #$8C..#$8D, #$A2..#$A3]) then Result := True;   // 0CC6 0CC6, 0CCC..0CCD, 0CCC..0CCD, 0CE2..0CE3, 0CE2..0CE3
        #$B4:
          if (AChar[2] in [#$80..#$81, #$BB..#$BC]) then Result := True;      // 0D00..0D01 0D00..0D01, 0D3B..0D3C, 0D3B..0D3C
        #$B5:
          if (AChar[2] in [#$81..#$84, #$8D, #$A2..#$A3]) then Result := True;   // 0D41..0D44 0D41..0D44, 0D4D, 0D4D, 0D62..0D63, 0D62..0D63
        #$B6:
          if (AChar[2] in [#$81]) then Result := True;                        // 0D81 0D81
        #$B7:
          if (AChar[2] in [#$8A, #$92..#$94, #$96]) then Result := True;      // 0DCA 0DCA, 0DD2..0DD4, 0DD2..0DD4, 0DD6, 0DD6
        #$B8:
          if (AChar[2] in [#$B1, #$B4..#$BA]) then Result := True;            // 0E31 0E31, 0E34..0E3A, 0E34..0E3A
        #$B9:
          if (AChar[2] in [#$87..#$8E]) then Result := True;                  // 0E47..0E4E 0E47..0E4E
        #$BA:
          if (AChar[2] in [#$B1, #$B4..#$BC]) then Result := True;            // 0EB1 0EB1, 0EB4..0EBC, 0EB4..0EBC
        #$BB:
          if (AChar[2] in [#$88..#$8E]) then Result := True;                  // 0EC8..0ECE 0EC8..0ECE
        #$BC:
          if (AChar[2] in [#$98..#$99, #$B5, #$B7, #$B9]) then Result := True;   // 0F18..0F19 0F18..0F19, 0F35, 0F35, 0F37, 0F37, 0F39, 0F39
        #$BD:
          if (AChar[2] in [#$B1..#$BE]) then Result := True;                  // 0F71..0F7E 0F71..0F7E
        #$BE:
          if (AChar[2] in [#$80..#$84, #$86..#$87, #$8D..#$97, #$99..#$BC]) then Result := True;   // 0F80..0F84 0F80..0F84, 0F86..0F87, 0F86..0F87, 0F8D..0F97, 0F8D..0F97, 0F99..0FBC, 0F99..0FBC
        #$BF:
          if (AChar[2] in [#$86]) then Result := True;                        // 0FC6 0FC6
      end;
    #$E1:
      case AChar[1] of
        #$80:
          if (AChar[2] in [#$AD..#$B0, #$B2..#$B7, #$B9..#$BA, #$BD..#$BE]) then Result := True;   // 102D..1030 102D..1030, 1032..1037, 1032..1037, 1039..103A, 1039..103A, 103D..103E, 103D..103E
        #$81:
          if (AChar[2] in [#$98..#$99, #$9E..#$A0, #$B1..#$B4]) then Result := True;   // 1058..1059 1058..1059, 105E..1060, 105E..1060, 1071..1074, 1071..1074
        #$82:
          if (AChar[2] in [#$82, #$85..#$86, #$8D, #$9D]) then Result := True;   // 1082 1082, 1085..1086, 1085..1086, 108D, 108D, 109D, 109D
        #$8D:
          if (AChar[2] in [#$9D..#$9F]) then Result := True;                  // 135D..135F 135D..135F
        #$9C:
          if (AChar[2] in [#$92..#$95, #$B2..#$B4]) then Result := True;      // 1712..1715 1712..1715, 1732..1734, 1732..1734
        #$9D:
          if (AChar[2] in [#$92..#$93, #$B2..#$B3]) then Result := True;      // 1752..1753 1752..1753, 1772..1773, 1772..1773
        #$9E:
          if (AChar[2] in [#$B4..#$B5, #$B7..#$BD]) then Result := True;      // 17B4..17B5 17B4..17B5, 17B7..17BD, 17B7..17BD
        #$9F:
          if (AChar[2] in [#$86, #$89..#$93, #$9D]) then Result := True;      // 17C6 17C6, 17C9..17D3, 17C9..17D3, 17DD, 17DD
        #$A0:
          if (AChar[2] in [#$8B..#$8D, #$8F]) then Result := True;            // 180B..180D 180B..180D, 180F, 180F
        #$A2:
          if (AChar[2] in [#$85..#$86, #$A9]) then Result := True;            // 1885..1886 1885..1886, 18A9, 18A9
        #$A4:
          if (AChar[2] in [#$A0..#$A2, #$A7..#$A8, #$B2, #$B9..#$BB]) then Result := True;   // 1920..1922 1920..1922, 1927..1928, 1927..1928, 1932, 1932, 1939..193B, 1939..193B
        #$A8:
          if (AChar[2] in [#$97..#$98, #$9B]) then Result := True;            // 1A17..1A18 1A17..1A18, 1A1B, 1A1B
        #$A9:
          if (AChar[2] in [#$96, #$98..#$9E, #$A0, #$A2, #$A5..#$AC, #$B3..#$BC, #$BF]) then Result := True;   // 1A56 1A56, 1A58..1A5E, 1A58..1A5E, 1A60, 1A60, 1A62, 1A62, 1A65..1A6C, 1A65..1A6C, 1A73..1A7C, 1A73..1A7C, 1A7F, 1A7F
        #$AA:
          if (AChar[2] >= #$B0) then Result := True;                          // 1AB0..1B03 1AB0..1ABF
        #$AB:
           Result := True;
        #$AC:
          if (AChar[2] in [#$80..#$83, #$B4, #$B6..#$BA, #$BC]) then Result := True;   // 1AB0..1B03 1B00..1B03, 1B34, 1B34, 1B36..1B3A, 1B36..1B3A, 1B3C, 1B3C
        #$AD:
          if (AChar[2] in [#$82, #$84, #$AB..#$B3]) then Result := True;      // 1B42 1B42, 1B44, 1B44, 1B6B..1B73, 1B6B..1B73
        #$AE:
          if (AChar[2] in [#$80..#$81, #$A2..#$A5, #$A8..#$AD]) then Result := True;   // 1B80..1B81 1B80..1B81, 1BA2..1BA5, 1BA2..1BA5, 1BA8..1BAD, 1BA8..1BAD
        #$AF:
          if (AChar[2] in [#$A6, #$A8..#$A9, #$AD, #$AF..#$B3]) then Result := True;   // 1BE6 1BE6, 1BE8..1BE9, 1BE8..1BE9, 1BED, 1BED, 1BEF..1BF3, 1BEF..1BF3
        #$B0:
          if (AChar[2] in [#$AC..#$B3, #$B6..#$B7]) then Result := True;      // 1C2C..1C33 1C2C..1C33, 1C36..1C37, 1C36..1C37
        #$B3:
          if (AChar[2] in [#$90..#$92, #$94..#$A0, #$A2..#$A8, #$AD, #$B4, #$B8..#$B9]) then Result := True;   // 1CD0..1CD2 1CD0..1CD2, 1CD4..1CE0, 1CD4..1CE0, 1CE2..1CE8, 1CE2..1CE8, 1CED, 1CED, 1CF4, 1CF4, 1CF8..1CF9, 1CF8..1CF9
        #$B7:
           Result := True;
      end;
    #$E2:
      case AChar[1] of
        #$83:
          if (AChar[2] >= #$90) then Result := True;                          // 20D0..20FF 20D0..20FF
        #$B3:
          if (AChar[2] in [#$AF..#$B1]) then Result := True;                  // 2CEF..2CF1 2CEF..2CF1
        #$B5:
          if (AChar[2] >= #$BF) then Result := True;                          // 2D7F 2D7F
        #$B7:
          if (AChar[2] >= #$A0) then Result := True;                          // 2DE0..2DFF 2DE0..2DFF
      end;
    #$E3:
      case AChar[1] of
        #$80:
          if (AChar[2] in [#$AA..#$AF]) then Result := True;                  // 302A..302F 302A..302F
        #$82:
          if (AChar[2] in [#$99..#$9A]) then Result := True;                  // 3099..309A 3099..309A
      end;
    #$EA:
      case AChar[1] of
        #$99:
          if (AChar[2] in [#$AF, #$B4..#$BD]) then Result := True;            // A66F A66F, A674..A67D, A674..A67D
        #$9A:
          if (AChar[2] in [#$9E..#$9F]) then Result := True;                  // A69E..A69F A69E..A69F
        #$9B:
          if (AChar[2] in [#$B0..#$B1]) then Result := True;                  // A6F0..A6F1 A6F0..A6F1
        #$A0:
          if (AChar[2] in [#$82, #$86, #$8B, #$A5..#$A6, #$AC]) then Result := True;   // A802 A802, A806, A806, A80B, A80B, A825..A826, A825..A826, A82C, A82C
        #$A3:
          if (AChar[2] in [#$84..#$85, #$A0..#$B1, #$BF]) then Result := True;   // A8C4..A8C5 A8C4..A8C5, A8E0..A8F1, A8E0..A8F1, A8FF, A8FF
        #$A4:
          if (AChar[2] in [#$A6..#$AD]) then Result := True;                  // A926..A92D A926..A92D
        #$A5:
          if (AChar[2] in [#$87..#$91, #$93]) then Result := True;            // A947..A951 A947..A951, A953, A953
        #$A6:
          if (AChar[2] in [#$80..#$82, #$B3, #$B6..#$B9, #$BC..#$BD]) then Result := True;   // A980..A982 A980..A982, A9B3, A9B3, A9B6..A9B9, A9B6..A9B9, A9BC..A9BD, A9BC..A9BD
        #$A7:
          if (AChar[2] in [#$80, #$A5]) then Result := True;                  // A9C0 A9C0, A9E5, A9E5
        #$A8:
          if (AChar[2] in [#$A9..#$AE, #$B1..#$B2, #$B5..#$B6]) then Result := True;   // AA29..AA2E AA29..AA2E, AA31..AA32, AA31..AA32, AA35..AA36, AA35..AA36
        #$A9:
          if (AChar[2] in [#$83, #$8C, #$BC]) then Result := True;            // AA43 AA43, AA4C, AA4C, AA7C, AA7C
        #$AA:
          if (AChar[2] in [#$B0, #$B2..#$B4, #$B7..#$B8, #$BE..#$BF]) then Result := True;   // AAB0 AAB0, AAB2..AAB4, AAB2..AAB4, AAB7..AAB8, AAB7..AAB8, AABE..AABF, AABE..AABF
        #$AB:
          if (AChar[2] in [#$81, #$AC..#$AD, #$B6]) then Result := True;      // AAC1 AAC1, AAEC..AAED, AAEC..AAED, AAF6, AAF6
        #$AF:
          if (AChar[2] in [#$A5, #$A8, #$AD]) then Result := True;            // ABE5 ABE5, ABE8, ABE8, ABED, ABED
      end;
    #$EF:
      case AChar[1] of
        #$AC:
          if (AChar[2] in [#$9E]) then Result := True;                        // FB1E FB1E
        #$B8:
          if (AChar[2] in [#$80..#$8F, #$A0..#$AF]) then Result := True;      // FE00..FE0F FE00..FE0F, FE20..FE2F, FE20..FE2F
      end;
    #$F0:
      case AChar[1] of
        #$90:
          case AChar[2] of
            #$87:
              if (AChar[3] in [#$BD]) then Result := True;                    // 101FD 101FD
            #$8B:
              if (AChar[3] in [#$A0]) then Result := True;                    // 102E0 102E0
            #$8D:
              if (AChar[3] in [#$B6..#$BA]) then Result := True;              // 10376..1037A 10376..1037A
            #$A8:
              if (AChar[3] in [#$81..#$83, #$85..#$86, #$8C..#$8F, #$B8..#$BA, #$BF]) then Result := True;   // 10A01..10A03 10A01..10A03, 10A05..10A06, 10A05..10A06, 10A0C..10A0F, 10A0C..10A0F, 10A38..10A3A, 10A38..10A3A, 10A3F, 10A3F
            #$AB:
              if (AChar[3] in [#$A5..#$A6]) then Result := True;              // 10AE5..10AE6 10AE5..10AE6
            #$B4:
              if (AChar[3] in [#$A4..#$A7]) then Result := True;              // 10D24..10D27 10D24..10D27
            #$B5:
              if (AChar[3] in [#$A9..#$AD]) then Result := True;              // 10D69..10D6D 10D69..10D6D
            #$BA:
              if (AChar[3] in [#$AB..#$AC]) then Result := True;              // 10EAB..10EAC 10EAB..10EAC
            #$BB:
              if (AChar[3] in [#$8B..#$8F, #$B0..#$BF]) then Result := True;   // 10ECB..10ECF 10ECB..10ECF, 10EF0..10EFF, 10EF0..10EFF
            #$BD:
              if (AChar[3] in [#$86..#$90]) then Result := True;              // 10F46..10F50 10F46..10F50
            #$BE:
              if (AChar[3] in [#$82..#$85]) then Result := True;              // 10F82..10F85 10F82..10F85
          end;
        #$91:
          case AChar[2] of
            #$80:
              if (AChar[3] in [#$81, #$B8..#$BF]) then Result := True;        // 11001 11001, 11038..1103F, 11038..11046
            #$81:
              if (AChar[3] in [#$80..#$86, #$B0, #$B3..#$B4, #$BF]) then Result := True;   // 11038..11046 11040..11046, 11070, 11070, 11073..11074, 11073..11074, 1107F, 1107F..11081
            #$82:
              if (AChar[3] in [#$80..#$81, #$B3..#$B6, #$B9..#$BA]) then Result := True;   // 1107F..11081 11080..11081, 110B3..110B6, 110B3..110B6, 110B9..110BA, 110B9..110BA
            #$83:
              if (AChar[3] in [#$82]) then Result := True;                    // 110C2 110C2
            #$84:
              if (AChar[3] in [#$80..#$82, #$A7..#$AB, #$AD..#$B4]) then Result := True;   // 11100..11102 11100..11102, 11127..1112B, 11127..1112B, 1112D..11134, 1112D..11134
            #$85:
              if (AChar[3] in [#$B3]) then Result := True;                    // 11173 11173
            #$86:
              if (AChar[3] in [#$80..#$81, #$B6..#$BE]) then Result := True;   // 11180..11181 11180..11181, 111B6..111BE, 111B6..111BE
            #$87:
              if (AChar[3] in [#$80, #$89..#$8C, #$8F]) then Result := True;   // 111C0 111C0, 111C9..111CC, 111C9..111CC, 111CF, 111CF
            #$88:
              if (AChar[3] in [#$AF..#$B1, #$B4..#$B7, #$BE]) then Result := True;   // 1122F..11231 1122F..11231, 11234..11237, 11234..11237, 1123E, 1123E
            #$89:
              if (AChar[3] in [#$81]) then Result := True;                    // 11241 11241
            #$8B:
              if (AChar[3] in [#$9F, #$A3..#$AA]) then Result := True;        // 112DF 112DF, 112E3..112EA, 112E3..112EA
            #$8C:
              if (AChar[3] in [#$80..#$81, #$BB..#$BC]) then Result := True;   // 11300..11301 11300..11301, 1133B..1133C, 1133B..1133C
            #$8D:
              if (AChar[3] in [#$80, #$8D, #$A6..#$AC, #$B0..#$B4]) then Result := True;   // 11340 11340, 1134D, 1134D, 11366..1136C, 11366..1136C, 11370..11374, 11370..11374
            #$8E:
              if (AChar[3] >= #$BB) then Result := True;                      // 113BB..113C0 113BB..113BF
            #$8F:
              if (AChar[3] in [#$80, #$8E..#$90, #$92, #$A1..#$A2]) then Result := True;   // 113BB..113C0 113C0, 113CE..113D0, 113CE..113D0, 113D2, 113D2, 113E1..113E2, 113E1..113E2
            #$90:
              if (AChar[3] >= #$B8) then Result := True;                      // 11438..1143F 11438..1143F
            #$91:
              if (AChar[3] in [#$82..#$84, #$86, #$9E]) then Result := True;   // 11442..11444 11442..11444, 11446, 11446, 1145E, 1145E
            #$92:
              if (AChar[3] in [#$B3..#$B8, #$BA, #$BF]) then Result := True;   // 114B3..114B8 114B3..114B8, 114BA, 114BA, 114BF, 114BF..114C0
            #$93:
              if (AChar[3] in [#$80, #$82..#$83]) then Result := True;        // 114BF..114C0 114C0, 114C2..114C3, 114C2..114C3
            #$96:
              if (AChar[3] in [#$B2..#$B5, #$BC..#$BD, #$BF]) then Result := True;   // 115B2..115B5 115B2..115B5, 115BC..115BD, 115BC..115BD, 115BF, 115BF..115C0
            #$97:
              if (AChar[3] in [#$80, #$9C..#$9D]) then Result := True;        // 115BF..115C0 115C0, 115DC..115DD, 115DC..115DD
            #$98:
              if (AChar[3] in [#$B3..#$BA, #$BD, #$BF]) then Result := True;   // 11633..1163A 11633..1163A, 1163D, 1163D, 1163F, 1163F..11640
            #$99:
              if (AChar[3] <= #$80) then Result := True;                      // 1163F..11640 11640
            #$9A:
              if (AChar[3] in [#$AB, #$AD, #$B0..#$B7]) then Result := True;   // 116AB 116AB, 116AD, 116AD, 116B0..116B7, 116B0..116B7
            #$9C:
              if (AChar[3] in [#$9D, #$9F, #$A2..#$A5, #$A7..#$AB]) then Result := True;   // 1171D 1171D, 1171F, 1171F, 11722..11725, 11722..11725, 11727..1172B, 11727..1172B
            #$A0:
              if (AChar[3] in [#$AF..#$B7, #$B9..#$BA]) then Result := True;   // 1182F..11837 1182F..11837, 11839..1183A, 11839..1183A
            #$A4:
              if (AChar[3] in [#$BB..#$BE]) then Result := True;              // 1193B..1193E 1193B..1193E
            #$A5:
              if (AChar[3] in [#$83]) then Result := True;                    // 11943 11943
            #$A7:
              if (AChar[3] in [#$94..#$97, #$9A..#$9B, #$A0]) then Result := True;   // 119D4..119D7 119D4..119D7, 119DA..119DB, 119DA..119DB, 119E0, 119E0
            #$A8:
              if (AChar[3] in [#$81..#$8A, #$B3..#$B8, #$BB..#$BE]) then Result := True;   // 11A01..11A0A 11A01..11A0A, 11A33..11A38, 11A33..11A38, 11A3B..11A3E, 11A3B..11A3E
            #$A9:
              if (AChar[3] in [#$87, #$91..#$96, #$99..#$9B]) then Result := True;   // 11A47 11A47, 11A51..11A56, 11A51..11A56, 11A59..11A5B, 11A59..11A5B
            #$AA:
              if (AChar[3] in [#$8A..#$96, #$98..#$99]) then Result := True;   // 11A8A..11A96 11A8A..11A96, 11A98..11A99, 11A98..11A99
            #$AD:
              if (AChar[3] in [#$A0, #$A2..#$A4, #$A6]) then Result := True;   // 11B60 11B60, 11B62..11B64, 11B62..11B64, 11B66, 11B66
            #$B0:
              if (AChar[3] in [#$B0..#$B6, #$B8..#$BD, #$BF]) then Result := True;   // 11C30..11C36 11C30..11C36, 11C38..11C3D, 11C38..11C3D, 11C3F, 11C3F
            #$B2:
              if (AChar[3] in [#$92..#$A7, #$AA..#$B0, #$B2..#$B3, #$B5..#$B6]) then Result := True;   // 11C92..11CA7 11C92..11CA7, 11CAA..11CB0, 11CAA..11CB0, 11CB2..11CB3, 11CB2..11CB3, 11CB5..11CB6, 11CB5..11CB6
            #$B4:
              if (AChar[3] in [#$B1..#$B6, #$BA, #$BC..#$BD, #$BF]) then Result := True;   // 11D31..11D36 11D31..11D36, 11D3A, 11D3A, 11D3C..11D3D, 11D3C..11D3D, 11D3F, 11D3F..11D45
            #$B5:
              if (AChar[3] in [#$80..#$85, #$87]) then Result := True;        // 11D3F..11D45 11D40..11D45, 11D47, 11D47
            #$B6:
              if (AChar[3] in [#$90..#$91, #$95, #$97]) then Result := True;   // 11D90..11D91 11D90..11D91, 11D95, 11D95, 11D97, 11D97
            #$B7:
              if (AChar[3] in [#$B0]) then Result := True;                    // 11DF0 11DF0
            #$BB:
              if (AChar[3] in [#$B3..#$B4]) then Result := True;              // 11EF3..11EF4 11EF3..11EF4
            #$BC:
              if (AChar[3] in [#$80..#$81, #$B6..#$BA]) then Result := True;   // 11F00..11F01 11F00..11F01, 11F36..11F3A, 11F36..11F3A
            #$BD:
              if (AChar[3] in [#$80..#$82, #$9A]) then Result := True;        // 11F40..11F42 11F40..11F42, 11F5A, 11F5A
          end;
        #$93:
          case AChar[2] of
            #$91:
              if (AChar[3] in [#$80, #$87..#$95]) then Result := True;        // 13440 13440, 13447..13455, 13447..13455
          end;
        #$96:
          case AChar[2] of
            #$84:
              if (AChar[3] in [#$9E..#$A9, #$AD..#$AF]) then Result := True;   // 1611E..16129 1611E..16129, 1612D..1612F, 1612D..1612F
            #$AB:
              if (AChar[3] in [#$B0..#$B4]) then Result := True;              // 16AF0..16AF4 16AF0..16AF4
            #$AC:
              if (AChar[3] in [#$B0..#$B6]) then Result := True;              // 16B30..16B36 16B30..16B36
            #$BD:
              if (AChar[3] in [#$8F]) then Result := True;                    // 16F4F 16F4F
            #$BE:
              if (AChar[3] in [#$8F..#$92]) then Result := True;              // 16F8F..16F92 16F8F..16F92
            #$BF:
              if (AChar[3] in [#$A4, #$B0..#$B1]) then Result := True;        // 16FE4 16FE4, 16FF0..16FF1, 16FF0..16FF1
          end;
        #$9B:
          case AChar[2] of
            #$B2:
              if (AChar[3] in [#$9D..#$9E]) then Result := True;              // 1BC9D..1BC9E 1BC9D..1BC9E
          end;
        #$9C:
          case AChar[2] of
            #$BC:
              if (AChar[3] in [#$80..#$AD, #$B0..#$BF]) then Result := True;   // 1CF00..1CF2D 1CF00..1CF2D, 1CF30..1CF3F, 1CF30..1CF46
            #$BD:
              if (AChar[3] <= #$86) then Result := True;                      // 1CF30..1CF46 1CF40..1CF46
          end;
        #$9D:
          case AChar[2] of
            #$84:
              if (AChar[3] in [#$A7..#$A8]) then Result := True;              // 1D127..1D128 1D127..1D128
            #$85:
              if (AChar[3] in [#$A5..#$A9, #$AD..#$B2, #$BB..#$BF]) then Result := True;   // 1D165..1D169 1D165..1D169, 1D16D..1D172, 1D16D..1D172, 1D17B..1D17F, 1D17B..1D182
            #$86:
              if (AChar[3] in [#$80..#$82, #$85..#$8B, #$AA..#$AD]) then Result := True;   // 1D17B..1D182 1D180..1D182, 1D185..1D18B, 1D185..1D18B, 1D1AA..1D1AD, 1D1AA..1D1AD
            #$89:
              if (AChar[3] in [#$82..#$84, #$90..#$92, #$9B..#$9C, #$9F]) then Result := True;   // 1D242..1D244 1D242..1D244, 1D250..1D252, 1D250..1D252, 1D25B..1D25C, 1D25B..1D25C, 1D25F, 1D25F
            #$8A:
              if (AChar[3] <= #$81) then Result := True;                      // 1D280..1D281 1D280..1D281
            #$A8:
              if (AChar[3] in [#$80..#$B6, #$BB..#$BF]) then Result := True;   // 1DA00..1DA36 1DA00..1DA36, 1DA3B..1DA3F, 1DA3B..1DA6C
            #$A9:
              if (AChar[3] in [#$80..#$AC, #$B5]) then Result := True;        // 1DA3B..1DA6C 1DA40..1DA6C, 1DA75, 1DA75
            #$AA:
              if (AChar[3] in [#$84, #$9B..#$9F, #$A1..#$AF]) then Result := True;   // 1DA84 1DA84, 1DA9B..1DA9F, 1DA9B..1DA9F, 1DAA1..1DAAF, 1DAA1..1DAAF
          end;
        #$9E:
          case AChar[2] of
            #$80:
              if (AChar[3] in [#$80..#$86, #$88..#$98, #$9B..#$A1, #$A3..#$A4, #$A6..#$AA]) then Result := True;   // 1E000..1E006 1E000..1E006, 1E008..1E018, 1E008..1E018, 1E01B..1E021, 1E01B..1E021, 1E023..1E024, 1E023..1E024, 1E026..1E02A, 1E026..1E02A
            #$82:
              if (AChar[3] in [#$8F]) then Result := True;                    // 1E08F 1E08F
            #$84:
              if (AChar[3] in [#$B0..#$B6]) then Result := True;              // 1E130..1E136 1E130..1E136
            #$8A:
              if (AChar[3] in [#$AE]) then Result := True;                    // 1E2AE 1E2AE
            #$8B:
              if (AChar[3] in [#$AC..#$AF]) then Result := True;              // 1E2EC..1E2EF 1E2EC..1E2EF
            #$93:
              if (AChar[3] in [#$AC..#$AF]) then Result := True;              // 1E4EC..1E4EF 1E4EC..1E4EF
            #$97:
              if (AChar[3] in [#$AE..#$AF]) then Result := True;              // 1E5EE..1E5EF 1E5EE..1E5EF
            #$9B:
              if (AChar[3] in [#$A3, #$A6, #$AE..#$AF, #$B5]) then Result := True;   // 1E6E3 1E6E3, 1E6E6, 1E6E6, 1E6EE..1E6EF, 1E6EE..1E6EF, 1E6F5, 1E6F5
            #$A3:
              if (AChar[3] in [#$90..#$96]) then Result := True;              // 1E8D0..1E8D6 1E8D0..1E8D6
            #$A5:
              if (AChar[3] in [#$84..#$8A]) then Result := True;              // 1E944..1E94A 1E944..1E94A
          end;
        #$9F:
          case AChar[2] of
            #$8F:
              if (AChar[3] >= #$BB) then Result := True;                      // 1F3FB..1F3FF 1F3FB..1F3FF
          end;
      end;
    #$F3:
      case AChar[1] of
        #$A0:
          case AChar[2] of
            #$84..#$86:
               Result := True;
            #$87:
              if (AChar[3] <= #$AF) then Result := True;                      // E0100..E01EF E01C0..E01EF
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

function ToIdx(APos: Integer): Integer; inline;
begin
  Result := APos - 1;
end;

function ToPos(AIdx: Integer): Integer; inline;
begin
  Result := AIdx + 1;
end;

function YToIdx(APointWithYPos: TPoint): TPoint; inline;
begin
  Result := APointWithYPos;
  dec(Result.Y);
end;

function YToPos(APointWithYIdx: TPoint): TPoint; inline;
begin
  Result := APointWithYIdx;
  inc(Result.Y);
end;

end.

