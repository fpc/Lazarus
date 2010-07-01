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

// C
function CompareCIdentifiers(Identifier1, Identifier2: PChar): integer;
procedure ReadTilCLineEnd(const Source: string;
   var Position: integer);
function ReadTilCBracketClose(const Source: string;
   var Position: integer): boolean;
procedure ReadNextCAtom(const Source: string;
   var Position: integer; out AtomStart: integer);
procedure ReadRawNextCAtom(const Source: string;
   var Position: integer; out AtomStart: integer);
function IsCDecimalNumber(const Source: string; Position: integer): boolean;
function IsCHexNumber(const Source: string; Position: integer): boolean;
function IsCOctalNumber(const Source: string; Position: integer): boolean;
function ExtractCCode(const Source: string;
                      StartPos: integer = 1; EndPos: integer = -1): string;

function CConstantToInt64(const s: string; out i: int64): boolean;


// Makefile
function ExtractCodeFromMakefile(const Source: string): string;


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
  while (AtomStart<=Len) and (not (Source[AtomStart] in [#10,#13])) do
    ReadRawNextCAtom(Source,Position,AtomStart);
  Position:=AtomStart;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function ReadTilCBracketClose(const Source: string; var Position: integer
  ): boolean;
// Position must start on a bracket
// at end Position will be right behind closing bracket
// if no closing bracket found then Position will be on the starting position
var
  Len: Integer;
  CloseBracket: Char;
  AtomStart: LongInt;
  StartPos: LongInt;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Result:=false;
  Len:=length(Source);
  if Position>Len then exit;
  StartPos:=Position;
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
    if AtomStart>Len then begin
      Position:=StartPos;
      exit;
    end;
    case Source[AtomStart] of
    '{','(','[':
      // skip nested bracketss
      begin
        Position:=AtomStart;
        if not ReadTilCBracketClose(Source,Position) then
          exit;
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
      #$EF:
        if (Source[Position+1]=#$BB)
        and (Source[Position+2]=#$BF) then begin
          // skip UTF BOM
          inc(Position,3);
        end else begin
          break;
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
      if (c1='0') and (Source[Position+1]='x') then begin
        inc(Position);
        // hex number
        repeat
          inc(Position);
        until (Position>Len) or (not IsHexNumberChar[Source[Position]]);
      end else if (c1='0') and (Source[Position+1] in ['0'..'7']) then begin
        // octal number
        repeat
          inc(Position);
        until (Position>Len) or (not (Source[Position] in ['0'..'7']));
      end else begin
        inc(Position);
        // read number
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
          if (Source[Position]='"') then
          begin
            inc(Position);
            while (Position<=Len)
            and (Source[Position]<>'"') do
              inc(Position);
            inc(Position);
          end else
            break;
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
    else
      inc(Position);
      if Position<=Len then begin
        c2:=Source[Position];
        // test for double char operators
        if ((c1=#13) and (c2=#10))
        or ((c2='=') and (c1 in ['=','!','<','>','+','-','*','/','&','|']))
        or ((c1=':') and (c2=':'))
        or ((c1='|') and (c2='|'))
        or ((c1='&') and (c2='&'))
        or ((c1='+') and (c2='+'))
        or ((c1='-') and (c2='-'))
        or ((c1='-') and (c2='>'))
        or ((c1='>') and (c2='>'))
        or ((c1='<') and (c2='<'))
        then
          inc(Position)
        else if ((c1='.') and (c2='.') and (Source[Position+1]='.')) then
          inc(Position,2);
      end;
    end;
  end;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function IsCDecimalNumber(const Source: string; Position: integer): boolean;
var
  l: Integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Result:=false;
  l:=length(Source);
  if (Position<1) or (Position>l) or (not IsNumberChar[Source[Position]])
  then exit;
  // check octal and hex number
  if (Source[Position]='0') and (Source[Position+1] in ['x','0'..'9'])
  then exit;
  // check float
  inc(Position);
  while (Position<=l) and (IsNumberChar[Source[Position]]) do
    inc(Position);
  if Source[Position]='.' then exit;
  Result:=true;
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function IsCHexNumber(const Source: string; Position: integer): boolean;
begin
  Result:=(Position>=1) and (Position<length(Source))
       and (Source[Position]='0') and (Source[Position+1]='x');
end;

function IsCOctalNumber(const Source: string; Position: integer): boolean;
begin
  Result:=(Position>=1) and (Position<length(Source))
       and (Source[Position]='0') and (Source[Position+1] in ['0'..'7']);
end;

function ExtractCCode(const Source: string; StartPos: integer;
  EndPos: integer): string;
var
  DstPos: Integer;
  SrcPos: Integer;
  SrcLen: Integer;
  AtomStart: integer;
begin
  {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
  {$R-}
  Result:=Source;
  DstPos:=1;
  SrcPos:=StartPos;
  SrcLen:=length(Source);
  if EndPos<1 then EndPos:=SrcLen+1;
  if EndPos>SrcLen then EndPos:=SrcLen+1;
  if SrcPos<EndPos then begin
    repeat
      ReadRawNextCAtom(Source,SrcPos,AtomStart);
      if AtomStart>=EndPos then break;
      if not (Source[AtomStart] in [#10,#13]) then begin
        if IsIdentChar[Source[AtomStart]]
        and (DstPos>1) and IsIdentChar[Result[DstPos-1]] then begin
          // space needed between words/numbers
          Result[DstPos]:=' ';
          inc(DstPos);
        end;
        // copy word
        while AtomStart<SrcPos do begin
          Result[DstPos]:=Source[AtomStart];
          inc(AtomStart);
          inc(DstPos);
        end;
      end;
    until false;
  end;
  if DstPos>length(Result)+1 then begin
    DebugLn(['ExtractCCode Source="',Source,'"']);
    raise Exception.Create('');
  end;
  SetLength(Result,DstPos-1);
  {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
end;

function ExtractCodeFromMakefile(const Source: string): string;
// remove comments, empty lines, double spaces, replace newline chars with #10

  procedure Run(var NewSrc: string; out NewLength: integer);
  var
    SrcLen: Integer;
    SrcPos: Integer;
    DestPos: Integer;
    LastChar: Char;
    LineEndPos: LongInt;
    EndPos: LongInt;
    IsEmptyLine: Boolean;
    CommentStartPos: Integer;
  begin
    SrcPos:=1;
    SrcLen:=length(Source);
    DestPos:=1;
    while SrcPos<=SrcLen do begin
      // check if line is empty
      LineEndPos:=SrcPos;
      IsEmptyLine:=true;
      CommentStartPos:=0;
      while (LineEndPos<=SrcLen) do begin
        case Source[LineEndPos] of
        #10,#13: break;
        ' ',#9:  ;
        '#':     if (CommentStartPos<1) then CommentStartPos:=LineEndPos;
        else
          if IsEmptyLine and (CommentStartPos<1) then
            IsEmptyLine:=false;
        end;
        inc(LineEndPos);
      end;
      //DebugLn(['Run SrcPos=',SrcPos,' LineEndPos=',LineEndPos,' Line="',dbgstr(copy(Source,SrcPos,LineEndPos-SrcPos)),'" IsEmpty=',IsEmptyLine]);
      
      // copy line content
      if not IsEmptyLine then begin
        LastChar:=#0;
        if Source[SrcPos]=#9 then begin
          // first character is tab
          LastChar:=#9;
          if NewSrc<>'' then
            NewSrc[DestPos]:=LastChar;
          inc(DestPos);
          inc(SrcPos);
        end;
        EndPos:=LineEndPos;
        if CommentStartPos>0 then
          EndPos:=CommentStartPos;
        while SrcPos<EndPos do begin
          if (not (Source[SrcPos] in [' ',#9]))
          or (not (LastChar in [' ',#9])) then begin
            LastChar:=Source[SrcPos];
            if NewSrc<>'' then
              NewSrc[DestPos]:=LastChar;
            inc(DestPos);
          end;
          inc(SrcPos);
        end;
        if NewSrc<>'' then
          NewSrc[DestPos]:=#10;
        inc(DestPos);
      end;

      // next line
      SrcPos:=LineEndPos+1;
      if (SrcPos<=SrcLen) and (Source[SrcPos] in [#10,#13])
      and (Source[SrcPos]<>Source[SrcPos-1]) then
        inc(SrcPos);
    end;
    NewLength:=DestPos-1;
  end;

var
  NewLength: integer;
begin
  //DebugLn(['ExtractCodeFromMakefile START ',Result]);
  Result:='';
  Run(Result,NewLength);
  SetLength(Result,NewLength);
  Run(Result,NewLength);
  //DebugLn(['ExtractCodeFromMakefile END ',Result]);
end;

function CConstantToInt64(const s: string; out i: int64): boolean;
var
  p: Integer;
  l: Integer;
begin
  i:=0;
  Result:=false;
  if s='' then exit;
  l:=length(s);
  try
    if s='0' then begin
      Result:=true;
    end else if (s[1]='0') and (s[2] in ['0'..'7']) then begin
      // octal
      p:=2;
      while (p<=l) and (s[p] in ['0'..'7']) do begin
        i:=i*8+ord(s[p])-ord('0');
        dec(p);
      end;
      Result:=p>l;
    end else if (s[1]='0') and (s[2]='x') then begin
      // hex
      p:=3;
      while (p<=l) and (IsHexNumberChar[s[p]]) do begin
        i:=i*16;
        case s[p] of
        '0'..'9': i:=i*16+ord(s[p])-ord('0');
        'a'..'f': i:=i*16+ord(s[p])-ord('a');
        'A'..'F': i:=i*16+ord(s[p])-ord('A');
        else break;
        end;
        dec(p);
      end;
      Result:=p>l;
    end else begin
      // decimal
      p:=1;
      while (p<=l) and (s[p] in ['0'..'9']) do begin
        i:=i*10+ord(s[p])-ord('0');
        dec(p);
      end;
      Result:=p>l;
    end;
  except
  end;
end;

end.

