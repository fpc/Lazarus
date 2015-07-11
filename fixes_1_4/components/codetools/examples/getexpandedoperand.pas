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
program getexpandedoperand;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates, FileProcs,
  CodeToolsConfig, SimpleUnit1, getterexample1;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  Filename: String;
  Line: String;
  Operand: string;
begin
  Filename:='scanexamples/getterexample1.pas';
  X:=14;
  Y:=56;
  if (ParamCount>=1) and (Paramcount<3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
    writeln('  ',ParamStr(0),' ',Filename,' ',X,' ',Y);
  end;

  CodeToolBoss.SimpleInit(ConfigFilename);

  if (ParamCount=3) then begin
    Filename:=ParamStr(1);
    X:=StrToInt(ParamStr(2));
    Y:=StrToInt(ParamStr(3));
  end;
  Filename:=TrimAndExpandFilename(Filename);
  writeln('File: ',Filename,' Line=',Y,' Column=',X);

  try
    // Step 1: load the file
    Code:=CodeToolBoss.LoadFile(Filename,false,false);
    if Code=nil then
      raise Exception.Create('loading failed '+Filename);

    Line:=Code.GetLine(Y-1);
    writeln('Line ',Y,': ',copy(Line,1,X-1),'|',copy(Line,X,length(Line)));

    // Step 2: find declaration
    if CodeToolBoss.GetExpandedOperand(Code,X,Y,Operand,false) then
    begin
      writeln('Operand: ',Operand);
    end else begin
      if CodeToolBoss.ErrorMessage<>'' then
        writeln('Parse error: ',CodeToolBoss.ErrorMessage)
      else
        writeln('Declaration not found');
    end;
  except
    on E: Exception do begin
      writeln('Error: ',E.Message);
    end;
  end;
end.

