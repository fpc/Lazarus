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
       cvsexportlocal - creates a directory structure like 'cvs export' does.

  Synopsis:
       cvsexportlocal sourcedirectory destinationdirectory

  Description:
       cvsexportlocal assumes source directory is a local copy of a cvs
          directory. It looks in the CVS subdirectory to see, what files and
          directories are part of CVS and copies them recursively to the
          destination directory.
}
program cvsexportlocal;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FileUtil;
  
var
  InputDir: string;
  OutputDir: string;

type
  TCvsDirectory = class
  private
    FDirectories: TStrings;
    FDirectory: string;
    FFiles: TStrings;
    procedure LoadEntries;
  public
    constructor Create(ADirectory: string);
    destructor Destroy; override;
    property Files : TStrings read FFiles;
    property Directories: TStrings read FDirectories;
  end;

{ TCvsDirectory }

constructor TCvsDirectory.Create(ADirectory: string);
begin
  FDirectory:= ADirectory;
  FFiles:= TStringList.Create;
  FDirectories := TStringList.Create;
  LoadEntries;
end;

destructor TCvsDirectory.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FDirectories);
end;

procedure TCvsDirectory.LoadEntries;
var
  EntriesPath: string;
  Entries: TStrings;
  i: integer;
  Name: string;

  function IsDirectoryEntry(i: integer): boolean;
  begin
    result := (Length(Entries[i])>0) and (Entries[i][1]='D')
  end;

  function GetNameFromEntry(i: integer): string;
  var
    FirstSlashPos: integer;
    SecondSlashPos: integer;
    Entry: string;
  begin
    Entry := Entries[i];
    FirstSlashPos := Pos('/',Entry);
    SecondSlashPos := Pos('/',
      Copy(Entry,FirstSlashPos+1,Length(Entry)-FirstSlashPos));
    Result := Copy(Entry, FirstSlashPos + 1, SecondSlashPos-1);
  end;
begin
  EntriesPath := AppendPathDelim(FDirectory)+'CVS'+DirectorySeparator+'Entries';
  Entries := TStringList.Create;
  Entries.LoadFromFile(EntriesPath);
  for i := 0 to Entries.Count-1 do begin
    Name := GetNameFromEntry(i);
    if Length(Name)>0 then
      if IsDirectoryEntry(i) then
        FDirectories.Add(Name)
      else
        FFiles.Add(Name);
  end;
  Entries.Free;
end;

procedure Init;
begin
  if ParamCount<2 then begin
    writeln('Usage:');
    writeln('  cvsexportlocal <sourcedirectory> <destinationdirectory>');
    Halt;
  end;
  InputDir := ExpandFileName(ParamStr(1));
  OutputDir := ExpandFileName(ParamStr(2));
end;

procedure CopyCvsDirectory(const SourceDir, DestinationDir: string);
var
  CvsDirectory: TCvsDirectory;
  i: Integer;

  procedure CopyCvsFile(const FileName: string);
  var
    SourceFileName: string;
    DestinationFileName: string;
  begin
    SourceFileName := AppendPathDelim(SourceDir)+FileName;
    DestinationFileName := AppendPathDelim(DestinationDir)+FileName;
    CopyFile(SourceFileName, DestinationFileName, true);
  end;
begin
  if DirectoryExists(DestinationDir) then
  begin
    writeln(format('Output directory %s exists. It will be deleted',
      [DestinationDir]));
    DeleteDirectory(DestinationDir, false);
  end;
  ForceDirectory(DestinationDir);

  CvsDirectory := TCvsDirectory.Create(SourceDir);
  try
    for i:= 0 to CvsDirectory.Files.Count-1 do
      CopyCvsFile(CvsDirectory.Files[i]);
    for i:= 0 to CvsDirectory.Directories.Count-1 do
      CopyCvsDirectory(AppendPathDelim(SourceDir)+CvsDirectory.Directories[i],
        AppendPathDelim(DestinationDir)+CvsDirectory.Directories[i]);
  finally
    CvsDirectory.Free;
  end;
end;

procedure Done;
begin
end;

begin
  Init;
  CopyCvsDirectory(InputDir, OutputDir);
  Done;
end.

