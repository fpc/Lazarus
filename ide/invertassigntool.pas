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

  Author: Sérgio Marcelo S. Gomes <smace at smace.com.br>
  Modified by Andrew Haines

  Abstract: Invert Assignment Code.

  Example: AValue := BValue  ->  BValue := AValue;
           AValue := True    ->  AValue := False;

}
unit InvertAssignTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function InvertAssignment(ALines:TStrings):TStrings;

implementation


function GetIndent(ALine: String):Integer;
begin
  Result :=Length(Aline) - Length(TrimLeft(ALine));
end;

procedure DivideLines(Lines: TStrings; var PreList, AList, BList, PostList: TStrings);
var
X: Integer;
ALine: String;
EqPos: Integer;
SemiPos: Integer;
WordBeforeEqPos: Integer;
TrueFalse: String;
        function FindWordBeforeEquals(ALine: String): Integer;
        var
        X: Integer;
        fPos: Integer;
        begin
          Result := 0;
          fPos := Pos(':=', ALine);
          if fPos > 0 then begin
            ALine := Trim(Copy(ALine,1,fPos-1));
            for X := Length(ALine) downto 1 do begin
              if ALine[X] = ' ' then begin
                Result := X+1;
                Exit;
              end;
            end;
          end;
        end;
begin
  AList.Clear;
  BList.Clear;
  for X := 0 to Lines.Count-1 do begin
    ALine := Trim(Lines.Strings[X]);

    EqPos := Pos(':=', ALine);
    SemiPos := Pos(';', ALine);
    WordBeforeEqPos := FindWordBeforeEquals(ALine);
    
    if (EqPos > 0) and (SemiPos > 0) then begin
      Alist.Add(Trim(Copy(ALine, WordBeforeEqPos+Ord(WordBeforeEqPos=0), EqPos - (WordBeforeEqPos+Ord(WordBeforeEqPos=0)))));
      BList.Add(Trim(Copy(ALine, EqPos + 2, (SemiPos-1) -(EqPos+1))));
      PreList.Add(Trim(Copy(ALine,1, WordBeforeEqPos-1)));
      PostList.Add(Trim(Copy(ALine, SemiPos, Length(ALine)-(SemiPos-1))));
      if Length(PreList.Strings[X]) > 0 then
        PreList.Strings[X] := PreList.Strings[X] + ' ';
    end
    else begin  // not a valid line
      PreList.Add('');
      AList.Add(ALine);
      Blist.Add('');
      PostList.Add('');
    end;
    // Check if is being assigned true or false
    if CompareText(BList.Strings[X], 'True') = 0 then begin
      TrueFalse := AList.Strings[X];
      AList.Strings[X] := 'False';
      BList.Strings[X] := TrueFalse;
    end;
    if CompareText(BList.Strings[X], 'False') = 0 then begin
      TrueFalse := AList.Strings[X];
      AList.Strings[X] := 'True';
      BList.Strings[X] := TrueFalse;
    end;
  end;

end;

function InvertLine(PreVar, VarA, VarB, PostVar: String; LineStart, EqualPosition: Integer): String;
var
fLength: Integer;
X: Integer;

begin
  Result := '';

  for X := 1 to LineStart do begin
    Result := Result + ' ';
  end;

  if Length(Trim(VarB)) = 0 then begin // is not a line with a ':='
    Result := Result + VarA;
    Exit;
  end;

  Result := Result + PreVar + VarB;

  fLength := Length(Trim(Result));
  if fLength < EqualPosition then begin
    for X := fLength+1 to EqualPosition do begin
      Result := Result + ' ';
    end;
  end;
  Result := Result + ' := ' + VarA + PostVar;
end;

function IsAWholeLine(ALine: String): Boolean;
begin
  // This function is useful for when the text is put back
  // in the synedit, things like this don't happen:
  // begin
  //   if CallSomeFunction > 0
  //   then
  //     DoThis
  //   else
  //     Exit;
  // end;
  //
  // Would otherwise become this
  //
  // begin if CallSomeFunction > 0 then DoThis else exit;
  // end;
  Result := False;
  ALine := LowerCase(ALine);
  if (Pos(';', ALine) > 0)
  or (Pos('if ', ALine) > 0)
  or (Pos('begin', ALine) > 0)
  or (Pos('end', ALine) > 0)
  or (Pos('then', ALine) > 0)
  or (Pos('else', ALine) > 0)
  or (Pos('and', ALine) > 0)
  or (Pos('or', ALine) > 0)
  or (Pos('//', ALine) > 0)
  then Result := True;
end;


// This function inverts all Assignments operation.
// like valuea := valueb; to valueb := valuea;
// or valuea := False; to valuea := True;
function InvertAssignment(ALines:TStrings):TStrings;
var
Lines: TStringList;
PreList,
AList,
BList,
PostList: TStrings;
Indents: PInteger;
X, Y: Integer;
EqPos: Integer;
ALine: String;
begin
  if ALines.Count = 0 then begin
    Result := ALines;
    Exit;
  end;
  
  Lines := TStringList.Create;

  GetMem(Indents,SizeOf(Integer)*ALines.Count);

  // Put a line on multiple lines, on one line
  ALine := '';
  for X := 0 to ALines.Count-1 do begin
    ALine := ALine + ALines.Strings[X];
    if IsAWholeLine(ALine) then begin
      Indents[Lines.Add(ALine)] := GetIndent(ALine);
      ALine := '';
    end;
  end;
  
  // exited the loop without finding the end of a line
  if Length(ALine) > 0 then begin
    X := Lines.Add(ALine);
    Indents[X] := GetIndent(ALine);
  end;
  
  ALines.Clear;
  
  PreList := TStringList.Create;
  AList := TStringList.Create;
  BList := TStringList.Create;
  PostList := TStringList.Create;
  
  DivideLines(Lines, PreList, AList, BList, PostList);
  Lines.Free;
  
  //Find where the ':=' should be
  EqPos := 0;
  for X := 0 to BList.Count-1 do begin
    Y := Length(BList.Strings[X]);
    if Y > EqPos then EqPos := Y;
  end;

  for X := 0 to AList.Count-1 do begin
    ALines.Add(InvertLine(PreList.Strings[X],
                            Alist.Strings[X],
                            BList.Strings[X],
                         PostList.Strings[X],
                                 Indents[X],
                                     EqPos));
  end;
  PreList.Free;
  AList.Free;
  BList.Free;
  PostList.Free;
  ReAllocMem(Indents,0);

  Result := ALines;
  // TODO: How do you stop this from adding a new line at the end of the last item
end;
//////////////////////////////////////////////////////////////////////


end.

