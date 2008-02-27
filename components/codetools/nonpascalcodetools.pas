{
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

  Abstract:
    Basic code functions for other languages than pascal.
}
unit NonPascalCodeTools;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, AVL_Tree, KeywordFuncLists, FileProcs;

function CompareCIdentifiers(Identifier1, Identifier2: PChar): integer;
procedure ReadTilCLineEnd(const Source: string;
   var Position: integer);
function ReadTilCBracketClose(const Source: string;
   var Position: integer): boolean;
procedure ReadNextCAtom(const Source: string;
   var Position: integer; out AtomStart: integer);
procedure ReadRawNextCAtom(const Source: string;
   var Position: integer; out AtomStart: integer);


implementation

function CompareCIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (Identifier1[0]=Identifier2[0]) do begin
        if (IsIdentChar[Identifier1[0]]) then begin
          inc(Identifier1);
          inc(Identifier2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsIdentChar[Identifier1[0]]) then begin
        if (IsIdentChar[Identifier2[0]]) then begin
          if Identifier1[0]>Identifier2[0] then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsIdentChar[Identifier2[0]]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Identifier2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

procedure ReadTilCLineEnd(const Source: string; var Position: integer);
var
  Len: Integer;
  AtomStart: Integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Len:=length(Source);
  if Position>Len then exit;
  AtomStart:=Position;
  repeat
    ReadRawNextCAtom(Source,Position,AtomStart);
  until (Position>Len) or (Source[Position] in [#10,#13]);
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function ReadTilCBracketClose(const Source: string; var Position: integer
  ): boolean;
// Position must start on a bracket
// at end Position will be right behind closing bracket
var
  Len: Integer;
  CloseBracket: Char;
  AtomStart: LongInt;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Result:=false;
  Len:=length(Source);
  if Position>Len then exit;
  case Source[Position] of
  '{': CloseBracket:='}';
  '[': CloseBracket:=']';
  '(': CloseBracket:=')';
  '<': CloseBracket:='>';
  else
    exit;
  end;
  inc(Position);
  AtomStart:=Position;
  repeat
    ReadRawNextCAtom(Source,Position,AtomStart);
    if AtomStart>Len then exit;
    case Source[AtomStart] of
    '{','(','[':
      // skip nested bracketss
      begin
        Position:=AtomStart;
        if not ReadTilCBracketClose(Source,Position) then exit;
      end;
    else
      if Source[AtomStart]=CloseBracket then exit(true);
    end;
  until false;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

procedure ReadNextCAtom(const Source: string; var Position: integer; out
  AtomStart: integer);
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  repeat
    ReadRawNextCAtom(Source,Position,AtomStart);
    if AtomStart>length(Source) then exit;
    case Source[AtomStart] of
    '#':
      // skip directive
      ReadTilCLineEnd(Source,Position);
    #0..#32:
      // skip space
    else
      // found normal C token
      exit;
    end;
  until false;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

procedure ReadRawNextCAtom(const Source: string; var Position: integer;
  out AtomStart: integer);
var
  Len:integer;
  c1,c2:char;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Len:=length(Source);
  // read til next atom
  while (Position<=Len) do begin
    case Source[Position] of
     #0..#9,#11,#12,#14..#32:  // spaces and special characters
      begin
        inc(Position);
      end;
     '\': // backslash
      if (Position<Len) and (Source[Position+1] in [#10,#13]) then begin
        inc(Position,2);
        if (Position<=Len) and (Source[Position] in [#10,#13])
        and (Source[Position-1]<>Source[Position]) then begin
          inc(Position);
        end;
      end;
     '/':  // comment or division
      if (Position<Len) then begin
        if (Source[Position+1]='/') then begin
          // comment start -> read til line end
          inc(Position);
          while (Position<=Len) do begin
            case Source[Position] of
            #10,#13: break;
            '\':
              begin
                inc(Position);
                if (Position<=Len) then begin
                  inc(Position);
                  if (Position<=Len) and (Source[Position-1] in [#10,#13])
                  and (Source[Position] in [#10,#13])
                  and (Source[Position-1]<>Source[Position]) then begin
                    inc(Position);
                  end;
                end;
              end;
            else inc(Position);
            end;
          end;
        end else if (Source[Position+1]='*') then begin
          // comment start -> read */
          inc(Position);
          while (Position<=Len) do begin
            if (Source[Position]='*')
            and (Position<Len)
            and (Source[Position+1]='/') then begin
              inc(Position,2);
              break;
            end;
            inc(Position);
          end;
        end else
          break;
      end else
        break;
     '(':  // comment or bracket
      if (Position<Len) and (Source[Position]='*') then begin
        // comment start -> read til comment end
        inc(Position,2);
        while true do begin
          case Source[Position] of
          #0:  if Position>Len then break;
          '*':
            if (Source[Position+1]=')') then begin
              inc(Position,2);
              break;
            end;
          end;
          inc(Position);
        end;
      end else
        // round bracket open
        break;
    else
      break;
    end;
  end;
  // read atom
  AtomStart:=Position;
  if Position<=Len then begin
    c1:=Source[Position];
    case c1 of
     'A'..'Z','a'..'z','_':
      begin
        // identifier
        inc(Position);
        while (Position<=Len) and (IsIdentChar[Source[Position]]) do
          inc(Position);
      end;
     '0'..'9': // number
      begin
        inc(Position);
        // read numbers
        while (Position<=Len) and (Source[Position] in ['0'..'9']) do
          inc(Position);
        if (Position<Len) and (Source[Position]='.')
        and (Source[Position+1]<>'.') then begin
          // real type number
          inc(Position);
          while (Position<=Len) and (Source[Position] in ['0'..'9']) do
            inc(Position);
          if (Position<=Len) and (Source[Position] in ['e','E']) then begin
            // read exponent
            inc(Position);
            if (Position<=Len) and (Source[Position]='-') then inc(Position);
            while (Position<=Len) and (Source[Position] in ['0'..'9']) do
              inc(Position);
          end;
        end;
      end;
     '"':  // string constant
      begin
        while (Position<=Len) do begin
          case (Source[Position]) of
          '"':
            begin
              inc(Position);
              while (Position<=Len)
              and (Source[Position]<>'"') do
                inc(Position);
              inc(Position);
            end;
          else
            break;
          end;
        end;
      end;
     '''': // char constant
       begin
         inc(Position);
         if (Position<=Len) then begin
           if Source[Position]='\' then
             inc(Position);
           inc(Position);
           if (Position<=Len) and (Source[Position]='''') then begin
             inc(Position);
           end;
         end;
       end;
     '$':  // hex constant
      begin
        inc(Position);
        while (Position<=Len)
        and (IsHexNumberChar[Source[Position]]) do
          inc(Position);
      end;
    else
      inc(Position);
      if Position<=Len then begin
        c2:=Source[Position];
        // test for double char operators :=, +=, -=, /=, *=, <>, <=, >=, **, ^^
        if ((c2='=') and  (IsEqualOperatorStartChar[c1]))
        or ((c1='<') and (c2='>'))
        or ((c1='.') and (c2='.'))
        or ((c1='*') and (c2='*'))
        or ((c1='^') and (c2='^'))
        then
          inc(Position);
      end;
    end;
  end;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

end.

