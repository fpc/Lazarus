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
  Classes, CustApp, SysUtils, Process, FileUtil, Dom, XmlRead, GetOpts;

type

  { TSvn2RevisionApplication }

  TSvn2RevisionApplication = class(TCustomApplication)
  private
    SourceDirectory,
    RevisionIncFileName: string;
    RevisionIncDirName: string;
    RevisionStr: string;
    ConstName: string;
    Verbose: boolean;

    function FindRevision: boolean;
    function IsValidRevisionInc: boolean;
    procedure WriteRevisionInc;
    function ParamsValid: boolean;
    procedure ShowHelp;
    function CanCreateRevisionInc: boolean;
    function ConstStart: string;
    procedure Show(msg: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
  end;

var
  Application: TSvn2RevisionApplication = nil;

const
  RevisionIncComment = '// Created by Svn2RevisionInc';

function TSvn2RevisionApplication.FindRevision: boolean;
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
        CommandLine := 'svnversion -n "' + SourceDirectory + '"';
        Options := [poUsePipes, poWaitOnExit];
        try
          Execute;
          SetLength(Buffer, 80);
          n:=OutPut.Read(Buffer[1], 80);
          RevisionStr := Copy(Buffer, 1, n);
          
          SetLength(Buffer, 1024);
          n:=Stderr.Read(Buffer[1], 1024);

          Result:=true;
          Show('Retrieved revision with svnversion.');
          Show('');
          Show('svnversion output:');
          Show(Copy(Buffer, 1, n));
          Show('');
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

constructor TSvn2RevisionApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  
  RevisionStr := 'Unknown';
end;

destructor TSvn2RevisionApplication.Destroy;
begin
  inherited Destroy;
end;

function TSvn2RevisionApplication.IsValidRevisionInc: boolean;
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

procedure TSvn2RevisionApplication.WriteRevisionInc;
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

procedure TSvn2RevisionApplication.ShowHelp;
begin
  writeln(ParamStr(0), ' [Options]');
  writeln;
  writeln('Options:');
  writeln(' -D<path>      Directory to get SVN version from (default current dir)');
  writeln(' -O<file>      Output file (default revision.inc)');
  writeln(' -C<name>      Name of constant (default RevisionStr)');
  writeln(' -v            Be more verbose');
  writeln(' -h            This help screen');
  halt(1);
end;

function TSvn2RevisionApplication.ParamsValid: boolean;
var
  ch: char;
begin
  Result := False;

  //reset
  SourceDirectory:=ExtractFilePath(ParamStr(0));
  Verbose := False;
  RevisionIncFileName := ExpandFileName('revision.inc');
  ConstName := 'RevisionStr';

  //parse options
  repeat
    ch:=Getopt('D:O:C:vh?');
    Case ch of
      'D' : SourceDirectory := OptArg;
      'O' : RevisionIncFileName := ExpandFileName(OptArg);
      'C' : ConstName := OptArg;
      'v' : Verbose := True;
      'h' : ShowHelp;
      '?' : ShowHelp;

      EndOfOptions : break;
    end;
  until False;

  //show options
  Show('SourceDirectory:     ' + SourceDirectory);
  Show('RevisionIncFileName: ' + RevisionIncFileName);
  Show('ConstName:           ' + ConstName);
  Show('');

  //checks
  if not DirectoryExists(SourceDirectory) then
  begin
    writeln('Error: Source directory "', SourceDirectory, '" doesn''t exist.');
    exit;
  end;
  
  RevisionIncDirName:=ExtractFilePath(RevisionIncFileName);
  if not DirectoryExists(RevisionIncDirName) then begin
    writeln('Error: Target Directory "', RevisionIncDirName, '" doesn''t exist.');
    exit;
  end;
  
  if ConstName[1] in ['0'..'9'] then
  begin
    writeln('Error: Invalid constant name ', ConstName, '.');
    exit;
  end;
  
  Result := True;
end;

function TSvn2RevisionApplication.CanCreateRevisionInc: boolean;
begin
  if (FileExists(RevisionIncFileName)) then
    Result:= FileIsWritable(RevisionIncFileName)
  else
    Result := DirectoryIsWritable(RevisionIncDirName);
end;

function TSvn2RevisionApplication.ConstStart: string;
begin
  Result := Format('const %s = ''', [ConstName]);
end;

procedure TSvn2RevisionApplication.Show(msg: string);
begin
  if Verbose then
    Writeln(msg);
end;

procedure TSvn2RevisionApplication.Run;
begin
  if not ParamsValid then
    ShowHelp;

  if not CanCreateRevisionInc then exit;
  
  if FindRevision or not IsValidRevisionInc then
    WriteRevisionInc;
end;

begin
  Application := TSvn2RevisionApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
