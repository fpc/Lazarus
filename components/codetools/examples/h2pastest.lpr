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
    Demonstration of how to convert c header files to pascal interfaces.
    
  Usage:
    h2pastest [filename.h]
}
program H2PasTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs,
  CodeAtom, CodeTree, NonPascalCodeTools, CCodeParserTool,
  H2PasTool, ctypes;
  
const
  ConfigFilename = 'codetools.config';
var
  Filename: String;
  CCode, PasCode: TCodeBuffer;
  Tool: TH2PasTool;
  Caret: TCodeXYPosition;
  OutputFilename: String;
  CCodeTool: TCCodeParserTool;
begin
  try
    CodeToolBoss.SimpleInit(ConfigFilename);
    Filename:=GetCurrentDir+'/scanexamples/test.h';
    if ParamCount=1 then
      Filename:=ParamStr(1);

    // Step 1: load the file
    CCode:=CodeToolBoss.LoadFile(Filename,false,false);
    if CCode=nil then
      raise Exception.Create('loading failed '+Filename);
    // Step 2: create the output file
    OutputFilename:='h2pasoutput.pas';
    PasCode:=CodeToolBoss.CreateFile(OutputFilename);
    if PasCode=nil then
      raise Exception.Create('creating failed '+OutputFilename);

    Tool:=TH2PasTool.Create;
    Tool.SourceName:=ExtractFileNameOnly(PasCode.Filename);
    Tool.Convert(CCode,PasCode);
    //Tool.WriteDebugReport;
    Tool.WriteH2PNodeReport;
    writeln;
    writeln('=============================================');
    writeln(PasCode.Source);
    Tool.Free;
  except
    on E: ECCodeParserException do begin
      CCodeTool:=ECCodeParserException(E).Sender;
      CCodeTool.CleanPosToCaret(CCodeTool.LastErrorReportPos,Caret);
      writeln(Caret.Code.Filename+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')'+' Error: '+E.Message);
    end;
    on E: Exception do begin
      writeln(E.Message);
    end;
  end;
end.

