{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

(*
 visit the following URL for more information
 http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
 http://unicode.org/reports/tr11/
*)

unit SynEditTextDoubleWidthChars;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, SynEditTextBase;

type

  { SynEditTextDoubleWidthChars }

  SynEditStringDoubleWidthChars = class(TSynEditStringsLinked)
  protected
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  end;


implementation

{ SynEditTextDoubleWidthChars }

procedure SynEditStringDoubleWidthChars.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
var
  i: Integer;
begin
  inherited DoGetPhysicalCharWidths(Line, LineLen, Index, PWidths);
  if not IsUtf8 then
    exit;

  dec(Line);
  dec(PWidths);
  for i := 0 to LineLen - 1 do begin
    inc(Line);
    inc(PWidths);
    if Line^ < #$e1 then continue;
    if PWidths^ = 0 then continue;
    case Line^ of
      #$e1:
        case Line[1] of
          #$84:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$85:
            if (Line[2] <= #$9f) then PWidths^ := 2;
        end;
      #$e2:
        case Line[1] of
          #$8c:
            if (Line[2] = #$a9) or (Line[2] = #$aa) then PWidths^ := 2;
          #$ba:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$bb..#$ff:
            PWidths^ := 2;
        end;
      #$e3:
        case Line[1] of
          #$81:
            if (Line[2] >= #$81) then PWidths^ := 2;
          #$82..#$8e:
            PWidths^ := 2;
          #$8f:
            if (Line[2] <= #$bf) then PWidths^ := 2;
          #$90:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$91..#$FF:
            PWidths^ := 2;
        end;
      #$e4:
        case Line[1] of
          #$00..#$b5:
            PWidths^ := 2;
          #$b6:
            if (Line[2] <= #$b5) then PWidths^ := 2;
          #$b8:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$b9..#$ff:
            PWidths^ := 2;
        end;
      #$e5..#$e8:
        PWidths^ := 2;
      #$e9:
        if (Line[1] <= #$bf) or (Line[2] <= #$83) then PWidths^ := 2;
      #$ea:
        case Line[1] of
          #$80, #$b0:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$81..#$92, #$b1..#$ff:
            PWidths^ := 2;
          #$93:
            if (Line[2] <= #$86) then PWidths^ := 2;
        end;
      #$eb..#$ec:
        PWidths^ := 2;
      #$ed:
        if (Line[1] <= #$9e) or (Line[2] <= #$a3) then PWidths^ := 2;

      #$ef:
        case Line[1] of
          #$a4:
            if (Line[2] >= #$80) then PWidths^ := 2;
          #$a5..#$aa:
            PWidths^ := 2;
          #$ab:
            if (Line[2] <= #$99) then PWidths^ := 2;
          #$b8:
            if (Line[2] in [#$90..#$99,#$b0..#$ff]) then PWidths^ := 2;
          #$b9:
            if (Line[2] <= #$ab) then PWidths^ := 2;
          #$bc:
            if (Line[2] >= #$81) then PWidths^ := 2;
          #$bd:
            if (Line[2] <= #$a0) then PWidths^ := 2;
          #$bf:
            if (Line[2] >= #$a0) and (Line[2] <= #$a6) then PWidths^ := 2;
        end;
      #$f0:
        case Line[1] of
          #$a0, #$b0:
            case Line[2] of
              #$80:
                if (Line[3] >= #$80) then PWidths^ := 2;
              #$81..#$ff:
                PWidths^ := 2;
            end;
          #$a1..#$ae, #$b1..#$be:
            PWidths^ := 2;
          #$af, #$bf:
            case Line[2] of
              #$00..#$be:
                PWidths^ := 2;
              #$bf:
                if (Line[3] <= #$bd) then PWidths^ := 2;
            end;
        end
    end;
  end
end;

(* Ranges that are FullWidth char

 1100  e1 84 80  ..  115F  e1 85 9f
 2329  e2 8c a9  ..  232A  e2 8c aa
 2E80  e2 ba 80  ..  303E  e3 80 be
 3041  e3 81 81  ..  33FF  e3 8f bf
 3400  e3 90 80  ..  4DB5  e4 b6 b5
 4E00  e4 b8 80  ..  9FC3  e9 bf 83
 A000  ea 80 80  ..  A4C6  ea 93 86
 AC00  ea b0 80  ..  D7A3  ed 9e a3
 F900  ef a4 80  ..  FAD9  ef ab 99
 FE10  ef b8 90  ..  FE19  ef b8 99
 FE30  ef b8 b0  ..  FE6B  ef b9 ab
 FF01  ef bc 81  ..  FF60  ef bd a0
 FFE0  ef bf a0  ..  FFE6  ef bf a6
20000  f0 a0 80 80  .. 2FFFD f0 af bf bd
30000  f0 b0 80 80  .. 3FFFD f0 bf bf bd

*)
end.

