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
function FilenameIsAbsolute(TheFilename: string):boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function FileIsExecutable(const AFilename: string): boolean;
function GetFileDescription(const AFilename: string): string;

// directories
function DirectoryExists(const FileName: String): Boolean;
function ForceDirectory(DirectoryName: string): boolean;
function DeleteDirectory(const DirectoryName: string;
  OnlyChilds: boolean): boolean;

// filename parts
function ExtractFileNameOnly(const AFilename: string): string;
function CompareFileExt(const Filename, Ext: string;
  CaseSensitive: boolean): integer;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;

// file search
function SearchFileInPath(const Filename, BasePath, SearchPath,
                          Delimiter: string; SearchLoUpCase: boolean): string;

// file read
function ReadFileToString(const Filename: string): string;

implementation

{$IFNDEF win32}
uses
  {$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF};
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

