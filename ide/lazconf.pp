{
 /***************************************************************************
                               lazconf.pp
                             -------------------
                           Lazarus Config Functions
                   Initial Revision  : Tue Apr 18 22:10:00 CET 2000

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

{
@author(Config Path Functions - Curtis White <cwhite@aracnet.com>)
@created(18-Apr-2000)
@lastmod(18-Apr-2000)

This unit contains functions to manage OS specific configuration path
information from within Lazarus.
}
unit LazConf;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes, FileCtrl, IDEProcs;

  { Config Path Functions }

  { The primary config path is the local or user specific path.
    If the primary config path does not exists, it will automatically be
    created.
    The secondary config path is for templates. The IDE will never write to it.
    If a config file is not found in the primary config file, Lazarus will
    copy the template file from the secondary config file. If there is no
    template file, the IDE will create a default file.
  }
  function GetPrimaryConfigPath: String;
  function GetSecondaryConfigPath: String;
  procedure CreatePrimaryConfigPath;
  procedure SetPrimaryConfigPath(const NewValue: String);
  procedure SetSecondaryConfigPath(const NewValue: String);
  procedure CopySecondaryConfigFile(const AFilename: String);
  
  function FindDefaultCompilerPath: string;
  function FindDefaultMakePath: string;
  function FindDefaultFPCSrcDirectory: string;
  function CheckFPCSourceDir(const ADirectory: string): boolean;

  function CreateCompilerTestPascalFilename: string;

implementation


{$I lazconf.inc}

function CheckFPCSourceDir(const ADirectory: string): boolean;
var
  Dir: String;
begin
  Result:=false;
  if DirectoryExists(ADirectory) then begin
    Dir:=AppendPathDelim(ADirectory);
    Result:=DirectoryExists(Dir+'fcl')
        and DirectoryExists(Dir+'rtl')
        and DirectoryExists(Dir+'packages');
  end;
end;

function FindDefaultFPCSrcDirectory: string;
var
  i: integer;
begin
  for i:=Low(DefaultFPCSrcDirs) to High(DefaultFPCSrcDirs) do begin
    Result:=DefaultFPCSrcDirs[i];
    if CheckFPCSourceDir(Result) then exit;
  end;
  Result:='';
end;

initialization
  InternalInit;

end.

{
  $Log$
  Revision 1.12  2003/02/07 18:46:35  mattias
  resolving lazarus directory even if started with search path

  Revision 1.11  2003/02/06 20:46:51  mattias
  default fpc src dirs and clean ups

  Revision 1.10  2002/12/23 13:20:45  mattias
  fixed backuping symlinks

  Revision 1.9  2002/12/20 11:08:47  mattias
  method resolution clause, class ancestor find declaration, 1.1. makros

  Revision 1.8  2002/07/01 05:53:31  lazarus
  MG: improved default make path for build lazarus

  Revision 1.7  2002/07/01 05:11:34  lazarus
  MG: improved default path to lazarus and ppc386

  Revision 1.6  2002/05/10 06:57:42  lazarus
  MG: updated licenses

  Revision 1.5  2002/03/22 17:36:09  lazarus
  MG: added include link history

  Revision 1.4  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.3  2001/05/27 11:52:00  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.2  2001/02/06 13:55:23  lazarus
  Changed the files from mode delphi to mode objfpc
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

  Revision 1.1  2000/04/25 01:24:35  lazarus
  Adding lazconf.pp interface file.                              CAW


}

