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
    Demo for automatic indentation.
}
program AutoIndent;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig, FileProcs,
  CodeToolsStructs, CodeToolManager, CodeCache, CodeBeautifier;

var
  Code: TCodeBuffer;
  Filename: String;
  Y: LongInt;
  X: LongInt;
  p: integer;
  Indentation: TFABIndentationPolicy;
begin
  if Paramcount>0 then begin
    if Paramcount<>3 then begin
      writeln('Usage: '+ParamStrUTF8(0)+' filename line column');
      exit;
    end;
    Filename:=ExpandFileNameUTF8(ParamStrUTF8(1));
    Y:=StrToInt(ParamStrUTF8(2));
    X:=StrToInt(ParamStrUTF8(3));
  end else begin
    Filename:=ExpandFileNameUTF8('scanexamples/indentation.pas');
    X:=3;
    Y:=74;
  end;

  // load the example unit
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('unable to read '+Filename);

  Code.LineColToPosition(Y,X,p);
  if p<1 then begin
    writeln('ERROR: invalid position: X=',X,' Y=',Y,' in ',Code.Filename);
    exit;
  end;
  if CodeToolBoss.Indenter.GetIndent(Code.Source,p,true,true,Indentation) then begin
    writeln('Indent=',Indentation.Indent);
  end else begin
    writeln('Error: GetIndent failed');
  end;
end.

