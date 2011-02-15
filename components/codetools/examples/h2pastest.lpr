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
  Merger: TCHeaderFileMerger;
  MergeAll: Boolean;
  MergeFlags: TCHFileMergeFlags;
  MacroName: String;
  p: LongInt;
  MacroValue: String;
begin
  Merger:=nil;
  try
    Tool:=TH2PasTool.Create;
    Merger:=TCHeaderFileMerger.Create;
    Filenames:=TStringList.Create;
    OutputFilename:=CleanAndExpandFilename(AppendPathDelim(GetCurrentDir)+'h2pasoutput.pas');
    MergeAll:=false;
    for i:=1 to Paramcount do begin
      Param:=ParamStr(i);
      if copy(Param,1,2)='-d' then begin
        MacroName:=copy(Param,3,length(Param));
        MacroValue:='';
        p:=Pos('=',MacroName);
        if p>0 then begin
          MacroValue:=copy(MacroName,p+1,length(MacroName));
          MacroName:=copy(MacroName,1,p-1);
        end;
        MacroName:=copy(MacroName,1,255);
        if not IsValidIdent(MacroName) then begin
          writeln('invalid macro name "',MacroName,'"');
          Halt;
        end;
        Merger.Macros[MacroName]:=MacroValue;
        Tool.Defines.Add(MacroName,MacroValue);
      end
      else if copy(Param,1,2)='-u' then begin
        MacroName:=copy(Param,3,255);
        if not IsValidIdent(MacroName) then begin
          writeln('invalid macro name "',MacroName,'"');
          Halt;
        end;
        Merger.Macros[MacroName]:='';
        Tool.Undefines.Add(MacroName,'');
      end
      else if copy(Param,1,2)='-o' then
        OutputFilename:=CleanAndExpandFilename(Param)
      else if Param='--merge-all' then
        MergeAll:=true
      else if copy(Param,1,1)='-' then begin
        writeln('Usage: ',ParamStr(0),' [--merge-all] [-d<definesymbol>]... [-u<undefinesymbol>]... <main header filename> <sub header file> ... -o<Outputfilename>');
        writeln();
        writeln('  Scans for include directives and inserts given sub include files.');
        writeln('');
        writeln('  --merge-all');
        writeln('    Merge all given files. Normally only files needed by the main header files are merged.');
        Halt;
      end else begin
        Filename:=CleanAndExpandFilename(Param);
        Filenames.Add(Filename);
      end;
    end;

    CodeToolBoss.SimpleInit(ConfigFilename);

    // for demonstration purpose run the tool on the example file
    if Filenames.Count=0 then
      Filenames.Add(CleanAndExpandFilename(GetCurrentDir+'/scanexamples/test.h'));

    // Step 1: load all input files
    MergeFlags:=[];
    if MergeAll then Include(MergeFlags,chfmfAll);
    Merger.Merge(Filenames,CodeToolBoss.SourceCache,MergeFlags);
    //Merger.WriteDebugReport;
    Src:=Merger.CombinedSource.Source;
    {writeln;
    writeln('======Combined c header files================');
    writeln(Src);
    writeln('=============================================');}

    // ToDo:
    //  Tool.UndefineEnclosingIFNDEF(CCode);

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

    FreeAndNil(Merger);
  except
    on E: ECCodeParserException do begin
      CCodeTool:=ECCodeParserException(E).Sender;
      CCodeTool.CleanPosToCaret(CCodeTool.LastErrorReportPos,Caret);
      writeln('Combined source: ',
         Caret.Code.Filename+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')',
         ' ',dbgstr(copy(CCodeTool.Src,CCodeTool.LastErrorReportPos-40,40)),'|',
             dbgstr(copy(CCodeTool.Src,CCodeTool.LastErrorReportPos,40)));
      if Merger<>nil then begin
        Merger.MergedPosToOriginal(Caret.X,Caret.Y,Caret.Code,Caret.X,Caret.Y);
      end;
      writeln('Origin: ',Caret.Code.Filename+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')'+' Error: '+E.Message);
    end;
    on E: Exception do begin
      writeln(E.Message);
    end;
  end;
end.

