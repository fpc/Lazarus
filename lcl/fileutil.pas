{
 /***************************************************************************
                               fileutil.pas
                               -----------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit FileUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLStrConsts;

// file attributes and states
function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFilenames(const Filename1, Filename2: string;
                          ResolveLinks: boolean): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer;
function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string):boolean;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
procedure CheckIfFileIsSymlink(const AFilename: string);
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function FileIsExecutable(const AFilename: string): boolean;
function FileIsSymlink(const AFilename: string): boolean;
function GetFileDescription(const AFilename: string): string;
function ReadAllLinks(const Filename: string;
                      ExceptionOnError: boolean): string;

// directories
function DirPathExists(const FileName: String): Boolean;
function ForceDirectory(DirectoryName: string): boolean;
function DeleteDirectory(const DirectoryName: string;
  OnlyChilds: boolean): boolean;
function ProgramDirectory: string;
function DirectoryIsWritable(const DirectoryName: string): boolean;

// filename parts
function ExtractFileNameOnly(const AFilename: string): string;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer;
function FilenameIsPascalUnit(const Filename: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;
function FileIsInPath(const Filename, Path: string): boolean;
function FileIsInDirectory(const Filename, Directory: string): boolean;
function FileInFilenameMasks(const Filename, Masks: string): boolean;

// file search
type
  TSearchFileInPathFlag = (
    sffDontSearchInBasePath,
    sffSearchLoUpCase
    );
  TSearchFileInPathFlags = set of TSearchFileInPathFlag;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string;
function GetAllFilesMask: string;

// file actions
function ReadFileToString(const Filename: string): string;
function CopyFile(const SrcFilename, DestFilename: string): boolean;
function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean): boolean;
function GetTempFilename(const Path, Prefix: string): string;

implementation

uses
{$IFDEF win32}
  Dos;
{$ELSE}
  {$IFDEF Ver1_0}Linux{$ELSE}Unix,BaseUnix{$ENDIF};
{$ENDIF}

var
  UpChars: array[char] of char;


{$I fileutil.inc}

procedure InternalInit;
var
  c: char;
begin
  for c:=Low(char) to High(char) do begin
    UpChars[c]:=upcase(c);
  end;
end;

initialization
  InternalInit;

end.

{
  $Log$
  Revision 1.5  2005/01/12 23:58:31  mattias
  improved project: recognizing if filename was fixed before pathdelim changed

  Revision 1.4  2004/12/18 23:46:16  mattias
  added resurce strings for gtk file dlg

  Revision 1.3  2004/12/04 01:17:41  mattias
  implemented Target Directory for IDE

  Revision 1.2  2004/09/27 22:16:04  vincents
  renamed filectrl.inc to fileutil.inc

}

