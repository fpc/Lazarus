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
    Simple demonstrating, how to setup the codetools, FPC and Lazarus Source
    directory to find a declaration.
}
program FindUnusedUnits;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates,
  CodeToolsConfig, SimpleUnit1;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  Filename: String;
  Units: TStringList;
begin
  if (ParamCount>=1) and (Paramcount<3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename>');
  end;

  CodeToolBoss.SimpleInit(ConfigFilename);
  Filename:=ExpandFileName('scanexamples/unusedunits1.pas');

  if (ParamCount>=1) then begin
    Filename:=ExpandFileName(ParamStr(1));
  end;

  // Step 1: load the file
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // Step 2: find declaration
  writeln('Filename: ',Code.Filename);
  Units:=TStringList.Create;
  if CodeToolBoss.FindUnusedUnits(Code,Units) then
  begin
    writeln(Units.Text);
  end else begin
    writeln('CodeToolBoss.FindUnusedUnits failed: ',CodeToolBoss.ErrorMessage);
  end;
  Units.Free;
end.

