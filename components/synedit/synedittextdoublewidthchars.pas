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
  public
    function GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths; override;
  end;


implementation

{ SynEditTextDoubleWidthChars }

function SynEditStringDoubleWidthChars.GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths;
var
  i: Integer;
begin
  Result := inherited GetPhysicalCharWidths(Line, Index);
  if not IsUtf8 then
    exit;

  for i := 0 to length(Line) -1 do begin
    if Result[i] = 0 then continue;
    case Line[i+1] of
      #$e1:
        case Line[i+2] of
          #$84:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$85:
            if (Line[i+3] <= #$9f) then Result[i] := 2;
        end;
      #$e2:
        case Line[i+2] of
          #$8c:
            if (Line[i+3] = #$a9) or (Line[i+3] = #$aa) then Result[i] := 2;
          #$ba:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$bb..#$ff:
            Result[i] := 2;
        end;
      #$e3:
        case Line[i+2] of
          #$81:
            if (Line[i+3] >= #$81) then Result[i] := 2;
          #$82..#$8e:
            Result[i] := 2;
          #$8f:
            if (Line[i+3] <= #$bf) then Result[i] := 2;
          #$90:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$91..#$FF:
            Result[i] := 2;
        end;
      #$e4:
        case Line[i+2] of
          #$00..#$b5:
            Result[i] := 2;
          #$b6:
            if (Line[i+3] <= #$b5) then Result[i] := 2;
          #$b8:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$b9..#$ff:
            Result[i] := 2;
        end;
      #$e5..#$e8:
        Result[i] := 2;
      #$e9:
        if (Line[i+2] <= #$bf) or (Line[i+3] <= #$83) then Result[i] := 2;
      #$ea:
        case Line[i+2] of
          #$80, #$b0:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$81..#$92, #$b1..#$ff:
            Result[i] := 2;
          #$93:
            if (Line[i+3] <= #$86) then Result[i] := 2;
        end;
      #$eb..#$ec:
        Result[i] := 2;
      #$ed:
        if (Line[i+2] <= #$9e) or (Line[i+3] <= #$a3) then Result[i] := 2;

      #$ef:
        case Line[i+2] of
          #$a4:
            if (Line[i+3] >= #$80) then Result[i] := 2;
          #$a5..#$aa:
            Result[i] := 2;
          #$ab:
            if (Line[i+3] <= #$99) then Result[i] := 2;
          #$b8:
            if (Line[i+3] in [#$90..#$99,#$b0..#$ff]) then Result[i] := 2;
          #$b9:
            if (Line[i+3] <= #$ab) then Result[i] := 2;
          #$bc:
            if (Line[i+3] >= #$81) then Result[i] := 2;
          #$bd:
            if (Line[i+3] <= #$a0) then Result[i] := 2;
          #$bf:
            if (Line[i+3] >= #$a0) and (Line[i+3] <= #$a6) then Result[i] := 2;
        end;
      #$f0:
        case Line[i+2] of
          #$a0, #$b0:
            case Line[i+3] of
              #$80:
                if (Line[i+4] >= #$80) then Result[i] := 2;
              #$81..#$ff:
                Result[i] := 2;
            end;
          #$a1..#$ae, #$b1..#$be:
            Result[i] := 2;
          #$af, #$bf:
            case Line[i+3] of
              #$00..#$be:
                Result[i] := 2;
              #$bf:
                if (Line[i+4] <= #$bd) then Result[i] := 2;
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

