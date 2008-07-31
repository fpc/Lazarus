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
  H2PasTool;
  
const
  ConfigFilename = 'codetools.config';
var
  Filename: String;
  CCode, PasCode: TCodeBuffer;
  Tool: TH2PasTool;
  Caret: TCodeXYPosition;
  OutputFilename: String;
  CCodeTool: TCCodeParserTool;
  i: Integer;
  Param: String;
  Filenames: TStringList;
  Src: String;
begin
  try
    CodeToolBoss.SimpleInit(ConfigFilename);

    Tool:=TH2PasTool.Create;
    Filenames:=TStringList.Create;
    OutputFilename:=CleanAndExpandFilename(AppendPathDelim(GetCurrentDir)+'h2pasoutput.pas');
    for i:=1 to Paramcount do begin
      Param:=ParamStr(i);
      if copy(Param,1,2)='-d' then
        Tool.Defines.Add(copy(Param,3,255),'')
      else if copy(Param,1,2)='-u' then
        Tool.Undefines.Add(copy(Param,3,255),'')
      else if copy(Param,1,2)='-o' then
        OutputFilename:=CleanAndExpandFilename(Param)
      else if copy(Param,1,1)='-' then begin
        writeln('Usage: ',ParamStr(0),' [-d<definesymbol>]... [-u<undefinesymbol>]... <header filename1> ... -o<Outputfilename>');
      end else begin
        Filename:=CleanAndExpandFilename(Param);
        Filenames.Add(Filename);
      end;
    end;

    // for demonstration purpose run the tool on the example file
    if Filenames.Count=0 then
      Filenames.Add(CleanAndExpandFilename(GetCurrentDir+'/scanexamples/test.h'));

    // Step 1: load all input filenames
    Src:='';
    for i:=0 to Filenames.Count-1 do begin
      Filename:=Filenames[i];
      CCode:=CodeToolBoss.LoadFile(Filename,false,false);
      if CCode=nil then
        raise Exception.Create('loading failed '+Filename);
      Tool.UndefineEnclosingIFNDEF(CCode);
      if Src<>'' then
        Src:=Src+LineEnding;
      Src:=Src+CCode.Source;
    end;

    // Step 2: create a temporary file
    Filename:='h2pasoutput.pas';
    CCode:=CodeToolBoss.CreateTempFile(Filename);
    if CCode=nil then
      raise Exception.Create('failed creating temporary file '+Filename);
    CCode.Source:=Src;
    // Step 3: create the output file
    PasCode:=CodeToolBoss.CreateFile(OutputFilename);
    if PasCode=nil then
      raise Exception.Create('creating failed '+OutputFilename);

    // Step 4: convert
    Tool.SourceName:=ExtractFileNameOnly(PasCode.Filename);
    Tool.Convert(CCode,PasCode);
    //Tool.WriteDebugReport;
    Tool.WriteH2PNodeReport;
    Tool.WriteH2PDirectivesNodeReport;
    writeln;
    writeln('=============================================');
    writeln(PasCode.Source);

    // clean up
    CCode.ReleaseRefCount;
    Tool.Free;
    Filenames.Free;

    // Step 5: write unit
    if PasCode.Save then
      writeln('Wrote ',PasCode.Filename)
    else
      writeln('Failed writing ',PasCode.Filename);
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

