{  $Id$  }
{
 /***************************************************************************
   diffpatch.pas - functions to extract differences between texts
                   (diffs, patches) and apply them (patching).

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

}
unit DiffPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TTextDiffFlag = (
    tdfIgnoreSpaceCharAmount,
    tdfIgnoreSpaceChars,
    tdfIgnoreEmptyLineChanges,
    tdfIgnoreCase
  );
  TTextDiffFlags = set of TTextDiffFlag;

procedure CreateTextDiff(Text1, Text2, DiffText: TStrings;
  Flags: TTextDiffFlags);


implementation


const
  IsSpaceChars, UpperCaseChars: array[char] of boolean;


function IsEmptyLine(const s: string; Flags: TTextDiffFlags): boolean;
var i: integer;
begin
  if ([tdfIgnoreSpaceCharAmount,tdfIgnoreSpaceChars]*Flags)<>[] then begin
    Result:=true;
    for i:=1 to length(s) do begin
      if not IsSpaceChars[s[i]] then begin
        Result:=false;
        exit;
      end;
    end;
  end else begin
    Result:=(s='');
  end;
end;

function LinesAreEqual(const Line1, Line2: string; Flags: TTextDiffFlags
  ): boolean;
var Pos1, Pos2, Len1, Len2: integer;
begin
  if ([tdfIgnoreSpaceCharAmount,tdfIgnoreSpaceChars]*Flags)<>[] then begin
    // completely ignore space chars
    Result:=true;
    Len1:=length(Line1);
    Len2:=length(Line2);
    Pos1:=1;
    Pos2:=1;
    while (Pos1<=Len1) and (Pos2<=Len2) do begin
      if ((not (tdfIgnoreCase in Flags))
        and (Line1[Pos1]=Line2[Pos2]))
      or ((tdfIgnoreCase in Flags)
        and (UpperCaseChars[Line1[Pos1]]=UpperCaseChars[Line2[Pos2]]))
      then begin
        // both chars are the same
        inc(Pos1);
        inc(Pos2);
        continue;
      end else begin
        // there is a difference
        if (tdfIgnoreSpaceChars in Flags) then begin
          if IsSpaceChars[Line1[Pos1]]
          or IsSpaceChars[Line2[Pos2]]
          then begin
            // skip spaces
            while (Pos1<=Len1) and IsSpaceChars[Line1[Pos1]] do inc(Pos1);
            while (Pos2<=Len2) and IsSpaceChars[Line2[Pos2]] do inc(Pos2);
          end else begin
            Result:=false;
          end;
        end else begin
          xxx
        end;
      end;
    end;
  end else begin
    if (tdfIgnoreCase in Flags) then begin
      Result:=(AnsiCompareText(Line1,Line2)=0);
    end else begin
      Result:=(AnsiCompareStr(Line1,Line2)=0);
    end;
  end;
end;

procedure CreateTextDiff(Text1, Text2, DiffText: TStrings;
  Flags: TTextDiffFlags);
var Cnt1, Cnt2, Line1, Line2: integer;
begin
  DiffText.Clear;
  Cnt1:=Text1.Count;
  Cnt2:=Text2.Count;
  Line1:=1;
  Line2:=1;
  // read empty lines
  if (tdfIgnoreEmptyLineChanges in Flags) then begin
    while (Line1<Cnt1) and (IsEmptyLine(Text1[Line1],Flags)) do
      inc(Line1);
    while (Line2<Cnt2) and (IsEmptyLine(Text2[Line2],Flags)) do
      inc(Line2);
  end;
  // read lines that are equal
  while (Line1<Cnt1) and (Line2<Cnt2)
  and LinesAreEqual(Text1[Line1],Text2[Line2],Flags) do begin
    inc(Line1);
    inc(Line2);
  end;
end;

procedure InternalInit;
var c: char;
begin
  for c:=Low(char) to High(char) do begin
    IsSpaceChars[c]:=c in [' ',#9,#10,#13];
    UpperCaseChars[c]:=upcase(c);
  end;
end;

initialization
  InternalInit;

finalization


end.

