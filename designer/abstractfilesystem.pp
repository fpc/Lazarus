      {
 /***************************************************************************
                               AbstractFileSystem.pp
                             -------------------

 ***************************************************************************/

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
unit AbstractFileSystem;

{$mode objfpc}

interface

uses
  classes;

type
 TFilename : String;

{  TAbstractFileSystem
   FileAge -- Returns the date/time of the file in a longint.

   GetFileStream -- Creates and returns a TStream for the Filename
                    passed into it.

   RenameFile -- Renames a file.  Returns TRUE is successful

   IsReadOnly -- Returns TRUE if file is READONLY.

   DeleteFile -- Returns TRUE if successful;

   FileExists -- Returns TRUE if the file exists

   GetBackupFileName -- Returns a string which represents the backup name for
                        the filename passed into the function.  It uses the
                        name passed into it to calculate the backup name.
}

  TAbstractFileSystem = class
   public
    Function FileAge(const Filename : TFilename) : Longint; virtual; abstract;
    Function GetFileStream(const Filename : TFilename; Mode : Integer): TStream; virtual; abstract;
    Function RenameFile(const Oldname, Newname : TFilename): Boolean; virtual; abstract;
    Function IsReadOnly(const Filename : TFilename): Boolean; virtual; abstract;
    Function DeleteFile(const Filename : TFilename): Boolean; virtual; abstract;
    Function FileExists(const Filename : TFilename) : Boolean; virtual; abstract
    Function GetBackupFileName(const Filename : TFilename): TFilename; virtual; abstract;
  end;


implementation

end.
