{
 /***************************************************************************
                               filectrl.pp
                               -----------
                             Component Library File Controls
                   Initial Revision  : Sun Apr 23 18:30:00 PDT 2000


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

{
@author(DirectoryExists - Curtis White <cwhite@aracnet.com>)                       
@created(23-Apr-2000)
@lastmod(23-Apr-2000)

This unit contains file and directory controls and supporting handling functions. 
} 

unit FileCtrl;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils;

// file attributes and states
function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFilenames(const Filename1, Filename2: string;
                          ResolveLinks: boolean): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer;
function FilenameIsAbsolute(TheFilename: string):boolean;
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

// filename parts
function ExtractFileNameOnly(const AFilename: string): string;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;
function FileIsInPath(const Filename, Path: string): boolean;

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


{$I filectrl.inc}

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

finalization

end.

{
  $Log$
  Revision 1.21  2004/01/23 19:36:49  mattias
  fixed searching dir in searchpath under win32

  Revision 1.20  2003/12/21 13:58:06  mattias
  renamed DirectoryExists to DirPathExists to reduce ambigiousity

  Revision 1.19  2003/10/31 14:25:59  mazen
  * Fixing VER1_1 compile problem to allow using 1.1 compiler
  * Most of oldlinux unit calls are now in BaseUnix unit with prefix Fp

  Revision 1.18  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.17  2003/08/13 16:18:58  mattias
  started check compiler options

  Revision 1.16  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.15  2003/03/28 23:03:38  mattias
  started TMemoScrollbar

  Revision 1.14  2003/03/26 11:39:08  mattias
  fixed rtl include path

  Revision 1.13  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.12  2003/02/20 11:03:20  mattias
  save as of project files now starts in project dierctory

  Revision 1.11  2003/02/07 17:49:21  mattias
  added ReadAllLinks

  Revision 1.10  2003/01/17 16:28:42  mattias
  updated translation files

  Revision 1.9  2002/12/23 13:20:46  mattias
  fixed backuping symlinks

  Revision 1.8  2002/12/23 10:12:40  mattias
  added symlink test and unit types

  Revision 1.7  2002/12/17 19:49:34  mattias
  finished publish project

  Revision 1.6  2002/12/12 17:47:44  mattias
  new constants for compatibility

  Revision 1.5  2002/12/09 16:48:36  mattias
  added basic file handling functions to filectrl

  Revision 1.4  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.3  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.2  2001/06/15 10:31:05  lazarus
  MG: set longstrings as default

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.2  2000/05/09 00:00:33  lazarus
  Updated my email address in the documentation to the current one. Also
  removed email references in comments that were not @author comments to
  fix problems with the documentation produced by pasdoc.           CAW

  Revision 1.1  2000/04/24 05:03:25  lazarus
  Added filectrl unit for DirectoryExists function.      CAW


}

