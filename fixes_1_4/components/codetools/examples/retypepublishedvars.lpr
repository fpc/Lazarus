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
    Demonstrates how to change the types of the variables of a class.
}
program retypepublishedvars;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates,
  CodeAtom, CodeToolsConfig, CodeToolsStructs, PascalParserTool;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  Filename: String;
  ListOfTypes: TStringToStringTree;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename.pas>');
  end;

  try
    CodeToolBoss.SimpleInit(ConfigFilename);
    
    Filename:=ExpandFileName('scanexamples'+PathDelim+'publishedvars.pas');
    
    if (ParamCount>=3) then begin
      Filename:=ExpandFileName(ParamStr(1));
    end;

    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    // complete code
    ListOfTypes:=TStringToStringTree.Create(false);
    ListOfTypes['TExButton']:='TButton';
    ListOfTypes['TExEdit']:='TEdit';
    if CodeToolBoss.RetypeClassVariables(Code,'TForm1',ListOfTypes,true)
    then begin
      writeln('Sucess:');
      writeln('=========================');
      writeln(Code.Source);
      writeln('=========================');
    end else begin
      writeln('RetypeClassVariables failed: ',CodeToolBoss.ErrorMessage);
    end;
  except
    on E: Exception do begin
      writeln('EXCEPTION: '+E.Message);
    end;
  end;
end.

