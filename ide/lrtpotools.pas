{ Copyright (C) 2004 V.I.Volchenko, Lazarus and FreePascal Developers Teams

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
}
unit LrtPoTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Forms, FileUtil, StringHashList,
  DialogProcs;

function AddFiles2Po(Files: TStrings; const POFilename: string): TModalResult;

implementation

uses Translations;

function FindAllTranslatedPoFiles(const Filename: string): TStringList;
var
  Path: String;
  Name: String;
  NameOnly: String;
  Ext: String;
  FileInfo: TSearchRec;
  CurExt: String;
begin
  Result:=TStringList.Create;
  Path:=ExtractFilePath(Filename);
  Name:=ExtractFilename(Filename);
  Ext:=ExtractFileExt(Filename);
  NameOnly:=LeftStr(Name,length(Name)-length(Ext));
  if SysUtils.FindFirst(Path+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      or (CompareFilenames(FileInfo.Name,Name)=0) then continue;
      CurExt:=ExtractFileExt(FileInfo.Name);
      if (CompareFilenames(CurExt,'.po')<>0)
      or (CompareFilenames(LeftStr(FileInfo.Name,length(NameOnly)),NameOnly)<>0)
      then
        continue;
      Result.Add(Path+FileInfo.Name);
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function AddFiles2Po(Files: TStrings; const POFilename: string): TModalResult;
var
  InputLines: TStringList;
  Filename: string;
  BasePoFile, POFile: TPoFile;
  i: Integer;
begin
  if (Files=nil) or (Files.Count=0) then exit(mrOk);
  
  InputLines := TStringList.Create;
  try
    // Read base po items
    if FileExists(POFilename) then
      BasePOFile := TPOFile.Create(POFilename, true)
    else
      BasePOFile := TPOFile.Create;
    BasePOFile.Tag:=1;

    // Update po file with lrt or/and rst files
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      if (CompareFileExt(Filename,'.lrt')=0)
      or (CompareFileExt(Filename,'.rst')=0) then begin
        //DebugLn(['AddFiles2Po Filename="',Filename,'"']);
        InputLines.Clear;
        Result:=LoadStringListFromFile(Filename, 'Update PO file '+POFilename,
                                       InputLines);
        if Result <> mrOK then Exit;
        if CompareFileExt(Filename,'.lrt')=0 then
          BasePOFile.UpdateStrings(InputLines, stLrt)
        else
          BasePOFile.UpdateStrings(InputLines, stRst);
      end;
    end;
    BasePOFile.SaveToFile(POFilename);
  
    // Update translated PO files
    InputLines.Free;
    InputLines := FindAllTranslatedPoFiles(POFilename);
    for i:=0 to InputLines.Count-1 do begin
      POFile := TPOFile.Create(InputLines[i], true);
      try
        POFile.Tag:=1;
        POFile.UpdateTranslation(BasePOFile);
        POFile.SaveToFile(InputLines[i]);
      finally
        POFile.Free;
      end;
    end;
      
  finally
    InputLines.Free;
    BasePOFile.Free;
  end;
end;

end.

