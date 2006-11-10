{  $Id$  }
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

  Author: Vincent Snijders

  Name:
       svn2revisioninc - creates an include file with the revision number

  Synopsis:
       svn2revisioninc sourcedir revision.inc

  Description:
       svn2revisioninc creates an include file with the current revision number.

       If the source directory contains a .svn subdirectory, it tries to execute
       svnversion to get the revision number.
       If that fails - for example, because it can't find svnversion - it opens
       .svn/entries to get the revision number of the source directory.

       If it can't find revision information, it checks whether the revision.inc
       exists. If it exists and seems to be created with svn2revisioninc, it
       will leave the file as is. Otherwise it will create a new revision.inc,
       indicating that the revision number is unknown.

}
program Svn2RevisionInc;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Process, FileUtil, Dom, XmlRead;
  
var
  SourceDirectory,
  RevisionIncFileName: string;
  RevisionIncDirName: string;
  RevisionStr: string = 'Unknown';
  
const
  RevisionIncComment = '// Created by Svn2RevisionInc';
  ConstStart = 'const RevisionStr = ''';
  
function FindRevision: boolean;
var
  SvnDir: string;
  function GetRevisionFromSvnVersion : boolean;
  var
    SvnVersionProcess: TProcess;
    Buffer: string;
    n: LongInt;
  begin
    Result:=false;
    SvnVersionProcess := TProcess.Create(nil);
    try
      with SvnVersionProcess do begin
        CommandLine := 'svnversion -n "'+SourceDirectory+'"';
        Options := [poUsePipes, poWaitOnExit];
        try
          Execute;
          SetLength(Buffer, 80);
          n:=OutPut.Read(Buffer[1], 80);
          RevisionStr := Copy(Buffer, 1, n);
          Result:=true;
          writeln('Retrieved revision with svnversion.');
        except
        // ignore error, default result is false
        end;
      end;
    finally
      SvnVersionProcess.Free;
    end;
  end;

  function GetRevisionFromEntriesTxt: boolean;
  var
    EntriesFileName: string;
    EntriesText: Text;
    line: string;
  begin
    Result:=false;
    EntriesFileName:=AppendPathDelim(SourceDirectory)+'.svn'+PathDelim+'entries';
    if FileExists(EntriesFileName) then begin
       try
         AssignFile(EntriesText, EntriesFileName);
         Reset(EntriesText);
         try
         // skip three lines
           Readln(EntriesText, line);
           if line<>'<?xml version="1.0" encoding="utf-8"?>' then begin
             Readln(EntriesText);
             Readln(EntriesText);
             Readln(EntriesText, RevisionStr);
             Result := true;
           end;
         finally
           CloseFile(EntriesText);
         end;
       except
         // ignore error, default result is false
       end;
    end;
  end;

  function GetRevisionFromEntriesXml: boolean;
  var
    EntriesFileName: string;
    EntriesDoc: TXMLDocument;
    EntryNode: TDomNode;
  begin
    Result:=false;
    EntriesFileName:=AppendPathDelim(SourceDirectory)+'.svn'+PathDelim+'entries';
    if FileExists(EntriesFileName) then begin
       try
         ReadXMLFile(EntriesDoc, EntriesFileName);
         try
           EntryNode := EntriesDoc.FirstChild.FirstChild;
           while not Result and (EntryNode<>nil) do begin
             if EntryNode.Attributes.GetNamedItem('name').NodeValue='' then begin
               RevisionStr:=EntryNode.Attributes.GetNamedItem('revision').NodeValue;
               Result:=true;
             end;
             EntryNode := EntryNode.NextSibling;
           end;
         finally
           EntriesDoc.Free;
         end;
       except
         // ignore error, default result is false
       end;
    end;
  end;

begin
  Result:=false;
  SvnDir:= AppendPathDelim(SourceDirectory)+'.svn';
  if DirectoryExists(SvnDir) then
    Result := GetRevisionFromSvnVersion or GetRevisionFromEntriesTxt
      or GetRevisionFromEntriesXml;
end;

function IsValidRevisionInc: boolean;
var
  Lines: TStringList;
begin
  Result:=false;
  if FileExists(RevisionIncFileName) then begin
    Lines := TStringList.Create;
    Lines.LoadFromFile(RevisionIncFileName);
    if (Lines.Count=2) and
      (Lines[0]=RevisionIncComment) and
      (copy(Lines[1], 1, length(ConstStart))=ConstStart) then
      Result:=true;
  end;
end;

procedure WriteRevisionInc;
var
  RevisionIncText: Text;
begin
  AssignFile(RevisionIncText, RevisionIncFileName);
  Rewrite(RevisionIncText);
  writeln(RevisionIncText, RevisionIncComment);
  writeln(RevisionIncText, ConstStart, RevisionStr, ''';');
  CloseFile(RevisionIncText);
  writeln(format('Created %s for revision: %s',
    [RevisionIncFileName, RevisionStr]));
end;

function ParamsValid: boolean;
begin
  Result := false;
  if ParamCount<>2 then exit;
  SourceDirectory:=ParamStr(1);
  if not DirectoryExists(SourceDirectory) then begin
    writeln('Error: Source directory "', SourceDirectory, '" doesn''t exist.');
    exit;
  end;
  RevisionIncFileName:=ParamStr(2);
  RevisionIncDirName:=ExtractFilePath(RevisionIncFileName);
  if not DirectoryExists(RevisionIncDirName) then begin
    writeln('Error: Target Directory "', RevisionIncDirName, '" doesn''t exist.');
    exit;
  end;
  Result := true;
end;

function CanCreateRevisionInc: boolean;
begin
  if (FileExists(RevisionIncFileName)) then
    Result:= FileIsWritable(RevisionIncFileName)
  else
    Result := DirectoryIsWritable(RevisionIncDirName);
end;

begin
  if not ParamsValid then begin
    writeln('Usage: ',ExtractFileName(ParamStr(0)),' sourcedir revision.inc');
    halt(1);
  end;
  if not CanCreateRevisionInc then exit;
  if FindRevision or not IsValidRevisionInc then
    WriteRevisionInc;
end.

