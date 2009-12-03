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
    List fpc units.
}
program FPCUnitLinks;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, DefineTemplates, CodeToolsConfig, FileProcs,
  CodeToolsStructs, CodeToolManager, CodeCache, CodeBeautifier;

procedure CollectUnits(Dir: string; List: TStringList);
var
  FileInfo: TSearchRec;
begin
  if FindFirstUTF8(Dir+FileMask,faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;

      if FilenameIsPascalUnit(FileInfo.Name,false) then begin
        List.Add(Dir+FileInfo.Name);
      end else if (FileInfo.Attr and faDirectory)>0 then begin
        CollectUnits(Dir+FileInfo.Name+PathDelim,List);
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

var
  FPCSrcDir: String;
  Files: TStringList;
begin
  if Paramcount<>1 then begin
    writeln('Usage: '+ParamStrUTF8(0)+' fpc-source-directory');
    exit;
  end;
  FPCSrcDir:=AppendPathDelim(ExpandFileNameUTF8(ParamStrUTF8(1)));
  Files:=TStringList.Create;
  CollectUnits(FPCSrcDir,Files);
  writeln(Files.Count);
  Files.Free;
end.

