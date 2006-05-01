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

  Author: Michael Van Canneyt
}
unit LazDEOpts;

{$mode objfpc}{$H+}

Interface

uses SysUtils,IniFiles;

Var
  SkipEmptyNodes   : Boolean;  
  ConfirmDelete    : Boolean;
  CreateBackup     : Boolean;
  ShowHelpHints    : Boolean;
  MaxRecentUsed    : Integer;
  BackupExtension  : String;  
  DefaultExtension : String;
  CmdMakeSkel      : String;
  cmdFPDoc         : String;
  
Const
  ImgIndxNew        = 0;
  ImgIndxEdited     = 1;
  ImgIndxModified   = 2;
  imgIndxFinished   = 3;
  
Procedure LoadOptions;
Procedure SaveOptions;
Function  GetOptionFileName : String;

Implementation

Const
  DefFilename         = 'fpde.ini';
  SecPrefs            = 'Preferences';
  KeySkipEmptyNodes   = 'SkipEmptyNodes';
  KeyConfirmDelete    = 'ConfirmDelete';
  KeyCreateBackup     = 'CreateBackup';
  KeyBackupExtension  = 'BackupExtension';  
  KeyDefaultExtension = 'DefaultExtension';
  //KeyMaxRecentUsed    = 'MaxMRUitems';
  KeyCmdMakeSkel      = 'makeskel';
  KeyCmdFpdoc         = 'fpdoc';
  KeyShowHints        = 'ShowHints';

{$ifndef MSWindows}
Function GetOptionFileName : String;

Const
  fpdedir = '.fpde';

Var
  HomeDir : String;

begin
  HomeDir:=GetEnvironmentVariable('HOME');
  If (HomeDir<>'') then
    begin
    HomeDir:=IncludeTrailingPathDelimiter(HomeDir)+fpdedir;
    If not DirectoryExists(HomeDir) then
      If Not CreateDir(HomeDir) then
        HomeDir:=''
      else
        HomeDir:=HomeDir;  
    end;
  Result:=IncludeTrailingPathDelimiter(HomeDir)+DefFileName;
end;

{$else}

Function GetOptionFileName : String;

begin
  Result:=ExtractFilePath(Paramstr(0))+DefFileName;  
end;
{$endif}

Procedure LoadOptions;

begin
  With TInifile.Create(GetOptionFileName) do
    Try
      SkipEmptyNodes:=ReadBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      ConfirmDelete:=ReadBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      CreateBackup:=ReadBool(SecPrefs,KeyCreateBackup,CreateBackup);
      ShowHelpHints:=ReadBool(SecPrefs,KeyShowHints,ShowHelpHints);
      BackupExtension:=ReadString(SecPrefs,KeyBackupExtension,BackupExtension);
      DefaultExtension:=ReadString(SecPrefs,KeyDefaultExtension,DefaultExtension);
      CmdMakeSkel:=ReadString(SecPrefs,KeyCmdMakeSkel,cmdMakeSkel);
      Cmdfpdoc:=ReadString(SecPrefs,KeyCmdfpdoc,cmdfpdoc);
    finally
      Free;
    end;
end;

Procedure SaveOptions;

begin
  With TInifile.Create(GetOptionFileName) do
    Try
      WriteBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      WriteBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      WriteBool(SecPrefs,KeyCreateBackup,CreateBackup);
      WriteBool(SecPrefs,KeyShowHints,ShowHelpHints);
      WriteString(SecPrefs,KeyBackupExtension,BackupExtension);
      WriteString(SecPrefs,KeyDefaultExtension,DefaultExtension);
      WriteString(SecPrefs,KeyCmdMakeSkel,cmdMakeSkel);
      WriteString(SecPrefs,KeyCmdfpdoc,cmdfpdoc);
      UpdateFile;
    finally
      Free;
    end;
end;

Initialization
  SkipEmptyNodes   := True;
  ConfirmDelete    := True;
  CreateBackup     := True;
  BackupExtension  := '.~xml';
  DefaultExtension := '.xml';
  MaxRecentUSed    := 10;
  CmdMakeSkel      := 'makeskel';
  cmdfpdoc         := 'fpdoc';
  ShowHelpHints    := true;
end.
