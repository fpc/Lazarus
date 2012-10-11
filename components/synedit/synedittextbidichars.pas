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

unit SynEditTextBidiChars;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, LazSynEditText;

type

  { TSynEditStringBidiChars }

  TSynEditStringBidiChars = class(TSynEditStringsLinked)
  protected
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  end;


implementation

{ TSynEditStringBidiChars }

procedure TSynEditStringBidiChars.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
var
  i: Integer;
  lbidi, bidi: (bLtr, bRtl, bWeak);
  WeakStart: PPhysicalCharWidth;
begin
  inherited DoGetPhysicalCharWidths(Line, LineLen, Index, PWidths);
  if not IsUtf8 then
    exit;

  dec(Line);
  dec(PWidths);
  lbidi := bLtr;
  WeakStart := nil;
  for i := 0 to LineLen - 1 do begin
    inc(Line);
    inc(PWidths);
    if PWidths^ = 0 then continue;
    bidi := bLtr;
    case Line^ of
      #$09, //Segment_Separator
      #$20, // White_Space
      #$21..#$22, #$26..#$2A, // Other_Neutral
      #$2B, #$2D,  // EN (European Seperator)
      #$23..#$25,  // European Terminator
      #$2C, #$2E, #$3A, // Common Separator
      #$3B..#$40, #$5B..#$60, #$7B..#$7E // Other_Neutral
      :
          bidi := bWeak;
      #$30..#$39:
          bidi := bWeak; // EN (European Number)
      #$C2: case Line[1] of
          #$B2..#$B3, #$B9:
            bidi := bWeak; // EN (European Seperator)
        end;
      #$D6:
          if (Line[1] >= #$90) then bidi := bRtl;
      #$D7..#$DA, #$DC..#$df:
          bidi := bRtl;
      #$DB: case Line[1] of
          #$B0..#$B9:
            bidi := bWeak; // EN (European Seperator)
          else
            bidi := bRtl;
        end;
      #$E0:
          if (Line[1] <= #$A2) then bidi := bRtl
          else
          if (Line[1] = #$A3) and (Line[2] <= #$BF) then bidi := bRtl;
      #$E2: case Line[1] of
          #$81: if (Line[2] in [#$B0, #$B4..#$B9]) then bidi := bWeak; // EN (European Seperator)
          #$82: if (Line[2] in [#$80..#$89]) then bidi := bWeak; // EN (European Seperator)
          #$92: if (Line[2] in [#$88..#$9B]) then bidi := bWeak; // EN (European Seperator)
        end;
      #$EF: case Line[1] of
          #$AC:       if  (Line[2] >= #$9D) then bidi := bRtl;
          #$AD..#$B6: bidi := bRtl;
          #$B7:       if (Line[2] in [#$80..#$8F, #$B0..#$bF]) then bidi := bRtl;
          #$B9:       if (Line[2] >= #$B0) then bidi := bRtl;
          #$BA:       bidi := bRtl;
          #$BB:       if (Line[2] <= #$BF) then bidi := bRtl;
          #$BC: if (Line[2] in [#$90..#$99]) then bidi := bWeak; // EN (European Seperator)
        end;
      #$F0: case Line[1] of
          #$90:       if (Line[2] >= #$A0) then bidi := bRtl;
          #$91..#$9C, #$9E: bidi := bRtl;
          #$9D: if (Line[2] = #$9F) and (Line[3] >= #$8E)
                then bidi := bWeak // EN (European Seperator)
                else bidi := bRtl;
          #$9F: if (Line[2] = #$84) and (Line[3] <= #$8A) then bidi := bWeak; // EN (European Seperator)
        end;
    end;

    case bidi of
      bRtl: begin
          PWidths^ := PWidths^ or PCWFlagRTL;
          lbidi := bRtl;
          WeakStart := nil;
        end;
      bLtr: begin
          if (WeakStart <> nil) and (lbidi = bRtl) then begin
            while WeakStart < PWidths do begin
              WeakStart^ := WeakStart^ and (not PCWFlagRTL);
              inc(WeakStart);
            end;
          end;
          lbidi := bLtr;
          WeakStart := nil;
        end;
      bWeak: begin
          if WeakStart = nil then WeakStart := PWidths;
          if lbidi = bRtl then
            PWidths^ := PWidths^ or PCWFlagRTL;
        end;
    end;

  end;

  if (WeakStart <> nil) and (lbidi = bRtl) then begin
    while WeakStart <= PWidths do begin
      WeakStart^ := WeakStart^ and (not PCWFlagRTL);
      inc(WeakStart);
    end;
  end;

end;

(*
RTL strong
200F (e2808f)           ; R # Cf (c38f)        RIGHT-TO-LEFT MARK

0590 (d690)   - 08FF (e0a3bf)
FB1D (efac9d) - FDCF (efb78f)
FDF0 (efb7b0) - FDFF (efb7bf)
FE70 (efb9b0) - FEFF (efbbbf)
00010800 (f090a080) - 0001EFFF (f09ebfbf)

TODO WEAK
  EN (European Seperator)
European Terminator
*)
end.

