{ $Id$ }
{                   -------------------------------------------
                     dbgutils.pp  -  Debugger utility routines
                    -------------------------------------------

 @created(Sun Apr 28st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains a collection of debugger support routines.

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
}
unit DBGUtils;

{$mode objfpc}{$H+}

interface 

function GetLine(var ABuffer: String): String;
function StripLN(const ALine: String): String;
function GetPart(const ASkipTo, AnEnd: String; var ASource: String): String;
function ConvertToCString(const AText: String): String;

const
{$IFDEF WIN32}
  LINE_END = #13#10;
{$ELSE}
  LINE_END = #10;
{$ENDIF}

implementation

function GetLine(var ABuffer: String): String;
var
  idx: Integer;
begin
  idx := Pos(#10, ABuffer);
  if idx = 0
  then Result := ''
  else begin
    Result := Copy(ABuffer, 1, idx);
    Delete(ABuffer, 1, idx);
  end;
end;

function StripLN(const ALine: String): String;
var
  idx: Integer;
begin
  idx := Pos(#10, ALine);
  if idx = 0
  then begin
    idx := Pos(#13, ALine);
    if idx = 0
    then begin
      Result := ALine;
      Exit;
    end;
  end
  else begin
    if (idx > 1)
    and (ALine[idx - 1] = #13)
    then Dec(idx);
  end;
  Result := Copy(ALine, 1, idx - 1);
end;           

function GetPart(const ASkipTo, AnEnd: String; var ASource: String): String;
var
  idx: Integer;
begin                  
  if ASkipTo <> ''
  then begin
    idx := Pos(ASkipTo, ASource);
    if idx = 0 
    then begin
      Result := '';
      Exit;
    end;
    Delete(ASource, 1, idx + Length(ASkipTo) - 1);
  end;
  if AnEnd = ''
  then idx := 0
  else idx := Pos(AnEnd, ASource);
  if idx = 0 
  then begin
    Result := ASource;
    ASource := '';
  end
  else begin
    Result := Copy(ASource, 1, idx - 1);
    Delete(ASource, 1, idx - 1);
  end;
end;

function ConvertToCString(const AText: String): String;
var
  n: Integer;
begin
  Result := AText;
  n := 1;
  while n <= Length(Result) do
  begin
    case Result[n] of
      '''': begin
        if (n < Length(Result))
        and (Result[n + 1] = '''')
        then Delete(Result, n, 1)
        else Result[n] := '"';
      end;
      '"': begin
        Insert('"', Result, n);
        Inc(n);
      end;
    end;
    Inc(n);
  end;
end;

end.
{ =============================================================================
  $Log$
  Revision 1.3  2003/05/22 23:08:19  marc
  MWE: = Moved and renamed debuggerforms so that they can be
         modified by the ide
       + Added some parsing to evaluate complex expressions
         not understood by the debugger

  Revision 1.2  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.1  2002/04/30 15:57:39  lazarus
  MWE:
    + Added callstack object and dialog
    + Added checks to see if debugger = nil
    + Added dbgutils

}
