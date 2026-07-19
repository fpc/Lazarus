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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Vincent Snijders

  Name:
       svn2revisioninc - creates an include file with the revision number

  Synopsis:
       svn2revisioninc sourcedir revision.inc

  Description:
       svn2revisioninc creates an include file with the current revision number
       coming from a version control repository.

       This tool supports Subversion (svn), git copies from and Mercurial (hg) repositories.

       1. If the source directory contains a .svn subdirectory, it tries to
       execute svnversion to get the revision number.
       If that fails - for example, because it can't find svnversion - it opens
       .svn/entries to get the revision number of the source directory.

       If it can't find revision information, it checks whether revision.inc
       exists. If it exists and seems to be created with svn2revisioninc, it
       will leave the file as is. Otherwise it will create a new revision.inc,
       indicating that the revision number is unknown.

       2. If the source directory doesn't contain a .svn subdirectory, it
       searches for a .git directory. If it exists, it tries to execute git to
       get the revision number.

       3. If the source directory doesn't contain a .svn or .git subdirectory,
       it tries to execute hg to get the revision id.
       Not checking for the .hg subdirectory allows getting the hg revision id
       even in subdirectories.
       Support for svn repos converted to hg with hgsubversion.
}
program Svn2RevisionInc;

{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>30200}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$ENDIF}
uses
  Classes, CustApp, SysUtils, Process, Dom, XmlRead, StrUtils,
  // LazUtils
  LazFileUtils, LazUTF8, LazLogger, LazVCSUtils;

type

  { TSvn2RevisionApplication }

  TSvn2RevisionApplication = class(TCustomApplication)
  private
    Scout: TVCSScout;
    SourceDirectory,
    RevisionIncFileName: string;
    RevisionIncDirName: string;
    ConstName: string;
    Verbose: boolean;
    UseStdOut: boolean;

    function IsValidRevisionInc: boolean;
    procedure WriteRevisionInc;
    function ParamsValid: boolean;
    procedure ShowHelp;
    function CanCreateRevisionInc: boolean;
    function ConstStart: string;
    procedure Show(Msg: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Run;
  end;

var
  Application: TSvn2RevisionApplication = nil;

const
  RevisionIncComment = '// Created by Svn2RevisionInc';

constructor TSvn2RevisionApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Scout:=TVCSScout.Create(Self);
  Scout.OnShow:=@Show;
  Scout.RevisionStr := 'Unknown';
end;

destructor TSvn2RevisionApplication.Destroy;
begin
  inherited Destroy;
end;

function TSvn2RevisionApplication.IsValidRevisionInc: boolean;
var
  Lines: TStringList;
begin
  Result := FileExistsUTF8(RevisionIncFileName);
  if Result then 
  begin
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(RevisionIncFileName);
      Result := (Lines.Count = 2) and
        (Lines[0] = RevisionIncComment) and
        (Copy(Lines[1], 1, Length(ConstStart)) = ConstStart);
    finally
      Lines.Free;
    end;
  end;
end;

procedure TSvn2RevisionApplication.WriteRevisionInc;
var
  RevisionIncText: Text;
begin
  AssignFile(RevisionIncText, RevisionIncFileName);
  Rewrite(RevisionIncText);
  writeln(RevisionIncText, RevisionIncComment);
  writeln(RevisionIncText, ConstStart, Scout.RevisionStr, ''';');
  CloseFile(RevisionIncText);
  DebugLn(format('Created %s for revision: %s', [RevisionIncFileName, Scout.RevisionStr]));
end;

procedure TSvn2RevisionApplication.ShowHelp;
  function ExtractFileBaseName(FileName: string): string;
  begin
    Result := ChangeFileExt(ExtractFileName(FileName), '');
  end;
begin
  debugln;
  debugln(ParamStrUTF8(0));
  debugln;
  debugln(ExtractFileBaseName(ParamStrUTF8(0)), ' <repository path> <output file> [Options]');
  debugln('or');
  debugln(ExtractFileBaseName(ParamStrUTF8(0)), ' [Options] <repository path> <output file>');
  debugln('or');
  debugln(ExtractFileBaseName(ParamStrUTF8(0)), ' <repository path> [Options] <output file>');
  debugln;
  debugln('Options:');
  debugln(' --o                  Output file');
  debugln(' --c=<name>           Name of constant (default RevisionStr)');
  debugln(' --s                  write revision to stdout, do not create inc file');
  debugln(' --v                  Be more verbose');
  debugln(' --h                  This help screen');
  debugln;
  debugln('Note: <repository path> default current directory');
  debugln('      <output file> default revision.inc');
  debugln('      --o overrides <output file>');
  debugln;
  debugln('      1st switchless parameter = <repository path>');
  debugln('      2nd switchless parameter = <output file>');
  halt(1);
end;

function TSvn2RevisionApplication.ParamsValid: boolean;
var
  i: integer;
  index: integer;
begin
  Result := False;

  //reset
  Verbose := False;
  ConstName := 'RevisionStr';
  SourceDirectory:=ChompPathDelim(ExtractFilePath(ParamStrUTF8(0)));
  RevisionIncFileName := ExpandFileNameUTF8('revision.inc');

  //find switchless parameters
  index := 1;
  for i := 1 to ParamCount do
  begin
    if Copy(ParamStrUTF8(i),1,1) <> '-' then
    begin
      case index of
        1: SourceDirectory:=ChompPathDelim(ParamStrUTF8(i));
        2: RevisionIncFileName := ExpandFileNameUTF8(ParamStrUTF8(i));
      end;
      Inc(index);
    end;
  end;

  //parse options
  if HasOption('h', 'help') or HasOption('?') then
    ShowHelp;

  if HasOption('v') then
    Verbose := True;

  if HasOption('s') then
    UseStdOut := True;

  if HasOption('c') then
    ConstName := GetOptionValue('c');

  if HasOption('o') then
    RevisionIncFileName := GetOptionValue('o');

  //show options
  Show('SourceDirectory:     ' + SourceDirectory);
  Show('RevisionIncFileName: ' + RevisionIncFileName);
  Show('ConstName:           ' + ConstName);
  Show('');

  //checks
  if not DirectoryExistsUTF8(SourceDirectory) then
  begin
    debugln('Error: Source directory "', SourceDirectory, '" doesn''t exist.');
    exit;
  end;

  RevisionIncDirName:=ExtractFilePath(ExpandFileNameUTF8(RevisionIncFileName));
  if (not UseStdOut) and (not DirectoryExistsUTF8(RevisionIncDirName)) then begin
    debugln('Error: Target Directory "', RevisionIncDirName, '" doesn''t exist.');
    exit;
  end;

  if ConstName[1] in ['0'..'9'] then
  begin
    debugln('Error: Invalid constant name ', ConstName, '.');
    exit;
  end;

  if not (Scout.GitInPath or Scout.SvnInPath or Scout.HgInPath) then
    debugln('Warning: Version control client not in path.');

  Result := True;
end;

function TSvn2RevisionApplication.CanCreateRevisionInc: boolean;
begin
  if (FileExistsUTF8(RevisionIncFileName)) then
    Result:= FileIsWritable(RevisionIncFileName)
  else
    Result := DirectoryIsWritable(RevisionIncDirName);
end;

function TSvn2RevisionApplication.ConstStart: string;
begin
  Result := Format('const %s = ''', [ConstName]);
end;

procedure TSvn2RevisionApplication.Show(Msg: string);
begin
  if Verbose then
    debugln(Msg);
end;

procedure TSvn2RevisionApplication.Run;
begin
  if not ParamsValid then
    ShowHelp;

  if not CanCreateRevisionInc then exit;

  if UseStdOut then begin
    if Scout.FindRevision then
      debugln(Scout.RevisionStr);
  end
  else if Scout.FindRevision or not IsValidRevisionInc then
    WriteRevisionInc;
end;

begin
  Application := TSvn2RevisionApplication.Create(nil);
  Application.Run;
  Application.Free;
end.
