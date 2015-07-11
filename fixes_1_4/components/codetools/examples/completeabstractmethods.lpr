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
    Demonstration, how to setup the codetools, FPC and Lazarus Source
    directory to find abstract methods not yet implemented in a class.
}
program CompleteAbstractMethods;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, CodeTree, CodeAtom,
  AbstractClass1;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  Filename: String;
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodeXYPos: TCodeXYPosition;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;

  // setup the Options
  ListOfPCodeXYPosition:=nil;
  try
    CodeToolBoss.SimpleInit(ConfigFilename);

    // Example: complete identifier s,
    //          by adding a local variable declaration (var s: string)
    Filename:=GetCurrentDir+'/scanexamples/abstractclass1.pas';
    X:=3;
    Y:=18;
    if (ParamCount>=3) then begin
      Filename:=ExpandFileName(ParamStr(1));
      X:=StrToInt(ParamStr(2));
      Y:=StrToInt(ParamStr(3));
    end;

    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    // find abstract methods
    if CodeToolBoss.FindAbstractMethods(Code,X,Y,ListOfPCodeXYPosition,true)
    then begin
      writeln('FindAbstractMethods succeeded: ');
      if ListOfPCodeXYPosition<>nil then begin
        for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
          CodeXYPos:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
          writeln(i,' ',CodeXYPos.Code.Filename,'(',CodeXYPos.Y,',',CodeXYPos.X,')');
        end;
      end;
    end else begin
      writeln('FindAbstractMethods failed: ',CodeToolBoss.ErrorMessage);
    end;
    
    if CodeToolBoss.AddMethods(Code,X,Y,1,ListOfPCodeXYPosition,true,
      NewCode,NewX,NewY,NewTopLine)
    then begin
      writeln('AddMethods succeeded: ',NewCode.Filename,' (',NewY,',',NewX,') ');
      writeln(Code.Source);
    end else begin
      writeln('AddMethods failed: ',CodeToolBoss.ErrorMessage);
    end;
  except
    on E: Exception do begin
      writeln(E.Message);
    end;
  end;
  CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
end.

