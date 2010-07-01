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
    Simple demonstration, how to setup the codetools, FPC and Lazarus Source
    directory to remove empty methods.
}
program RemoveEmptyMethods;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, DefineTemplates, FileProcs,
  CodeAtom, CodeToolsConfig, CodeToolsStructs, PascalParserTool,
  EmptyMethods1;

const
  ConfigFilename = 'codetools.config';
var
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
  Filename: String;
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  P: PCodeXYPosition;
  All: boolean;
  Sections: TPascalClassSections;
  RemovedProcHeads: TStrings;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
  end;

  try
    CodeToolBoss.SimpleInit(ConfigFilename);
    
    X:=10;
    Y:=22;
    Filename:=ExpandFileName('scanexamples'+PathDelim+'emptymethods1.pas');
    
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
    ListOfPCodeXYPosition:=TFPList.Create;
    Sections:=[pcsPublished,pcsPrivate,pcsProtected,pcsPublic];
    if CodeToolBoss.FindEmptyMethods(Code,'',X,Y,Sections,ListOfPCodeXYPosition,All)
    then begin
      writeln('Found ',ListOfPCodeXYPosition.Count,' empty methods (All=',All,'):');
      for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
        P:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
        writeln(i,' ',DbgsCXY(P^));
      end;
      if CodeToolBoss.RemoveEmptyMethods(Code,'',X,Y,Sections,All,[],RemovedProcHeads)
      then begin
        writeln('Empty methods removed:');
        if RemovedProcHeads<>nil then
          writeln(RemovedProcHeads.Text);
        writeln('=========================');
        writeln(Code.Source);
        writeln('=========================');
      end else begin
        writeln('RemoveEmptyMethods failed: ',CodeToolBoss.ErrorMessage);
      end;
      RemovedProcHeads.Free;
    end else begin
      writeln('FindEmptyMethods failed: ',CodeToolBoss.ErrorMessage);
    end;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  except
    on E: Exception do begin
      writeln('EXCEPTION: '+E.Message);
    end;
  end;
end.

