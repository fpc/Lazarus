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

uses SysUtils, IniFiles, FileUtil;

Var
  SkipEmptyNodes   : Boolean;  
  ConfirmDelete    : Boolean;
  CreateBackup     : Boolean;
  ShowHelpHints    : Boolean;
  StartMaximized   : Boolean;
  ReopenLast       : Boolean;
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
uses gettext, translations;

const
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
  HomeDir:=GetEnvironmentVariableUTF8('HOME');
  If (HomeDir<>'') then
    begin
    HomeDir:=IncludeTrailingPathDelimiter(HomeDir)+fpdedir;
    If not DirectoryExistsUTF8(HomeDir) then
      If Not CreateDirUTF8(HomeDir) then
        HomeDir:=''
      else
        HomeDir:=HomeDir;  
    end;
  Result:=IncludeTrailingPathDelimiter(HomeDir)+DefFileName;
end;

{$else}

Function GetOptionFileName : String;

begin
  Result:=ExtractFilePath(ParamStrUTF8(0))+DefFileName;  
end;
{$endif}

Procedure LoadOptions;

begin
  With TInifile.Create(UTF8ToSys(GetOptionFileName)) do
    Try
      SkipEmptyNodes:=ReadBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      ConfirmDelete:=ReadBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      CreateBackup:=ReadBool(SecPrefs,KeyCreateBackup,CreateBackup);
      ShowHelpHints:=ReadBool(SecPrefs,KeyShowHints,ShowHelpHints);
      StartMaximized := ReadBool(SecPrefs, 'StartMaximized', StartMaximized);
      ReopenLast := ReadBool(SecPrefs, 'ReopenLast', ReopenLast);
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
  With TInifile.Create(UTF8ToSys(GetOptionFileName)) do
    Try
      WriteBool(SecPrefs,KeySkipEmptyNodes,SkipEmptyNodes);
      WriteBool(SecPrefs,KeyConfirmDelete,ConfirmDelete);
      WriteBool(SecPrefs,KeyCreateBackup,CreateBackup);
      WriteBool(SecPrefs,KeyShowHints,ShowHelpHints);
      WriteBool(SecPrefs, 'StartMaximized', StartMaximized);
      WriteBool(SecPrefs, 'ReopenLast', ReopenLast);
      WriteString(SecPrefs,KeyBackupExtension,BackupExtension);
      WriteString(SecPrefs,KeyDefaultExtension,DefaultExtension);
      WriteString(SecPrefs,KeyCmdMakeSkel,cmdMakeSkel);
      WriteString(SecPrefs,KeyCmdfpdoc,cmdfpdoc);
      UpdateFile;
    finally
      Free;
    end;
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang, S: String;
begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  S:=AppendPathDelim(AppendPathDelim(ExtractFileDir(ParamStr(0))) + 'languages');
  TranslateUnitResourceStrings('LazDEMsg',S+'lazde.%s.po', Lang,FallbackLang);
end;

Initialization
  SkipEmptyNodes   := True;
  ConfirmDelete    := True;
  CreateBackup     := True;
  StartMaximized   := true;
  ReopenLast       := true;
  BackupExtension  := '.~xml';
  DefaultExtension := '.xml';
  MaxRecentUSed    := 10;
  CmdMakeSkel      := 'makeskel';
  cmdfpdoc         := 'fpdoc';
  ShowHelpHints    := true;

  TranslateResStrings;
end.
