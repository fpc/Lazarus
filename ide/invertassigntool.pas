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

  Abstract: Invert Assignment Code.

  Example: AValue := BValue  ->  BValue := AValue;
  
  To-Do:
    - Redo everything. It was baldy coded.
      I am one of those stupid Drag and Drop programmers ;-)
    - Add blank spaces at left here and Keep identation..
    - Add multi-lines support.
      // like: xx :=
      //         ola;

}
unit InvertAssignTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function InvertAssignment(ALines:TStrings):TStrings;

implementation

// This function inverts all Assignments operation.
// like valuea := valueb; to valueb := valuea;
function InvertAssignment(ALines:TStrings):TStrings;
var iLeft, iSize : integer; //for identation purpose only

    function clearline(s:String):String;
    //Removing all ";" of the line.
    begin
      Result := trim(s);
      repeat
        if length(Result)= 0 then exit;
        if Result[length(Result)] = ';' then
          Result := Copy(Result,0, length(Result)-1);
        Result := TrimRight(Result);
      until Result[length(Result)] <> ';';
    end;

  function InvertAssignLine(s:String):String;
  //it Inverts one Single Line

    function AddSpaces(iChar:Integer):String;
    var i : integer;
    begin
      Result := '';
      for i := 0 to iChar-1 do  Result := Result+' ';
    end;

  var iPos: integer;
      s_Right, s_Left, s_Comment : String;
  begin

    if trim(s) = '' then
    begin
      Result := '';
      exit;
    end;

    // Extrating Comments
    iPos := Pos('//',s);
    if iPos > 0 then
    begin
      s_Comment := Copy(s,iPos,Length(s)-iPos+1);
      s         := Copy(s,0,iPos-1);
    end;

    iPos := Pos('{',s);
    if iPos > 0 then
    begin
      s_Comment := s_Comment+Copy(s,iPos,Length(s)-iPos+1);
      s         := trim( Copy(s,0,iPos-1) );
    end;

    if Pos(':=',s) > 0 then
    begin
      if s_Comment <> '' then s_Comment := ' '+s_Comment;

      // Inverting
      iPos    := Pos(':=', s );
      s_Right := ClearLine( Copy(s,iPos+2, Length(s)-iPos) );
      s_Left  := trim( Copy(s,0,iPos-1) );

      Result := AddSpaces(iLeft) + s_Right + AddSpaces(iSize-length(s_Right))+' := '+s_Left+';'+s_Comment;

   end else Result := s+s_Comment; //Does not exist assignment

  end; //function end

var
  s : String;
  i, iPos : integer;

begin

  Result := TStringList.Create;

  // Let's find the Bigest Words at Right for Identation
  iSize := 0;
  for i := 0 to ALines.Count-1 do
  begin
    s    := trim(ALines[i]);
    iPos := Pos(':=', s );
    iPos := length( ClearLine( Copy(s,iPos+2, Length(s)-iPos) ) );
    if iPos > iSize then iSize := iPos;
  end;

  iLeft := length(ALines[0]) - length(TrimLeft(ALines[0]));

  // We must get a valid line to invert and call InvertLine function.
  for i := 0 to ALines.Count-1 do
  begin
    Result.Add( InvertAssignLine( trim(ALines[i]) ) );
  end;

end;
//////////////////////////////////////////////////////////////////////


end.

