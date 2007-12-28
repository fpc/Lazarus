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
    directory to complete code.
}
program CodeCompletion;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates,
  CodeToolsConfig;

const
  ConfigFilename = 'codetools.config';
var
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  TopLine: Integer;
  Filename: String;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;

  try
    CodeToolBoss.SimpleInit(ConfigFilename);
    
    // Example: complete identifier s,
    //          by adding a local variable declaration (var s: string)
    X:=3;
    Y:=41;
    TopLine:=20;
    Filename:=ExpandFileName('scanexamples'+PathDelim+'completion1.pas');
    
    if (ParamCount>=3) then begin
      Filename:=ExpandFileName(ParamStr(1));
      X:=StrToInt(ParamStr(2));
      Y:=StrToInt(ParamStr(3));
    end;

    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    // complete code
    if CodeToolBoss.CompleteCode(Code,X,Y,TopLine,NewCode,NewX,NewY,NewTopLine)
    then begin
      writeln('Code completed: ',NewCode.Filename,' Line=',NewY,' Column=',NewX);
      writeln(Code.Source);
    end else begin
      writeln('Code completion failed: ',CodeToolBoss.ErrorMessage);
    end;
  except
    on E: Exception do begin
      writeln('EXCEPTION: '+E.Message);
    end;
  end;
end.

