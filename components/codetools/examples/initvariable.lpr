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
    Simple demonstration, how to setup the codetools to insert a simple
    Pascal statement to initialize a variable.
}
program initvariable;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, contnrs, CodeCache, CodeToolManager, DefineTemplates,
  FileProcs, CodeToolsConfig, CodeToolsStructs, CodeCompletionTool,
  StdCodeTools, initvars1;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  Filename: String;
  Statements: TStrings;
  InsertPositions: TObjectList;
  InsertPosDesc: TInsertStatementPosDescription;
  i: Integer;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;

  try
    CodeToolBoss.SimpleInit(ConfigFilename);
    
    X:=15;
    Y:=21;
    Filename:=ExpandFileName('scanexamples'+PathDelim+'initvars1.pas');
    
    if (ParamCount>=3) then begin
      Filename:=ExpandFileName(ParamStr(1));
      X:=StrToInt(ParamStr(2));
      Y:=StrToInt(ParamStr(3));
    end;

    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    // get possible initializations
    Statements:=nil;
    InsertPositions:=nil;
    try
      if not CodeToolBoss.GetPossibleInitsForVariable(Code,X,Y,Statements,
        InsertPositions)
      then begin
        writeln('CodeToolBoss.GetPossibleInitsForVariable failed');
        exit;
      end;
      writeln('Possible initialization statements:');
      writeln(Statements.Text);
      writeln('Possible initialization positions:');
      for i:=0 to InsertPositions.Count-1 do begin
        InsertPosDesc:=TInsertStatementPosDescription(InsertPositions[i]);
        with InsertPosDesc do begin
          writeln(CodeXYPos.Code.Filename,'(',CodeXYPos.Y,',',CodeXYPos.X,'): ',Description);
        end;
      end;

      // insert the first statement at the first position
      InsertPosDesc:=TInsertStatementPosDescription(InsertPositions[0]);
      if not CodeToolBoss.InsertStatements(InsertPosDesc,Statements[0]) then begin
        writeln('CodeToolBoss.InsertStatements failed');
        exit;
      end;
      writeln('New source (not saved to disk):');
      writeln(Code.Source);

    finally
      Statements.Free;
      InsertPositions.Free;
    end;
  except
    on E: Exception do begin
      writeln('EXCEPTION: '+E.Message);
    end;
  end;
end.

