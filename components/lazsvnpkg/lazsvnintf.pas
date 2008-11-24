{ Copyright (C) 2008 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit LazSVNIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLtype, LResources, ProjectIntf, LCLProc;

procedure ProcSVNLog(Sender: TObject);
procedure ProcSVNCommit(Sender: TObject);
procedure ProcSVNUpdate(Sender: TObject);
procedure ProcSVNDiff(Sender: TObject);
procedure ProcSVNSettings(Sender: TObject);
procedure Register;

implementation

uses
  MenuIntf, IDECommands, Controls, Forms, Dialogs,
  SVNLogForm, SVNUpdateForm, SVNDiffForm, SVNStatusForm, LazIDEIntf, SVNClasses,
  SVNAddProjectForm, SrcEditorIntf;

var
  CmdSVNLog : TIDECommand;
  CmdSVNCommit : TIDECommand;
  CmdSVNUpdate : TIDECommand;
  CmdSVNDiff : TIDECommand;
  CmdSVNSettings : TIDECommand;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  mnuSVNMain : TIDEMenuSection;
  mnuSVNSection : TIDEMenuSection;
begin
  Key:=IDEShortCut(VK_UNKNOWN,[],VK_UNKNOWN,[]);

  {$ifndef USECustomCategory}
    Cat:=IDECommandList.CreateCategory(nil, 'SVN', rsSVNTools,
      IDECmdScopeSrcEditOnly);
  {$else}
    cat:=nil;
  {$endif}

  CmdSVNLog:=RegisterIDECommand(Cat, 'SVNLog', 'SVN log', Key, nil, @ProcSVNLog);
  CmdSVNCommit:=RegisterIDECommand(Cat, 'SVNCommit', 'SVN commit', Key, nil, @ProcSVNCommit);
  CmdSVNUpdate:=RegisterIDECommand(Cat, 'SVNUpdate', 'SVN update', Key, nil, @ProcSVNUpdate);
  CmdSVNDiff:=RegisterIDECommand(Cat, 'SVNDiff', 'SVN diff', Key, nil, @ProcSVNDiff);
  CmdSVNSettings:=RegisterIDECommand(Cat, 'SVNSettings', 'SVN settings', Key, nil, @ProcSVNSettings);

  {$note add menu_svn bitmap in the main menu}
  mnuSVNMain := RegisterIDEMenuSection(itmCustomTools, 'SVN');
  mnuSVNSection:=RegisterIDESubMenu(mnuSVNMain, 'SVN', 'SVN', nil, nil);
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNLog', rsShowLog, nil, nil,
    CmdSVNLog, 'menu_svn_log');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNCommit', rsCommit, nil, nil,
    CmdSVNCommit, 'menu_svn_commit');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNUpdate', rsUpdate, nil, nil,
    CmdSVNUpdate, 'menu_svn_update');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNDiff', rsShowDiff, nil, nil,
    CmdSVNDiff, 'menu_svn_diff');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNSettings', rsSettings, nil, nil,
    CmdSVNSettings, 'menu_environment_options');
end;

procedure ProcSVNLog(Sender: TObject);
var
  Repo: string;
  IsActive: boolean;
  sBool: string;
begin
  If Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    Repo := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];
    sBool := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE];
    if sBool <> '' then
      IsActive := StrToBool(sBool)
    else
      IsActive := False;

    if IsActive and (Repo <> '') then
      ShowSVNLogFrm(Repo)
    else
      ShowMessage(rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst);
  end;
end;

procedure ProcSVNCommit(Sender: TObject);
var
  Repo: string;
  IsActive: boolean;
  sBool: string;
begin
  If Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    Repo := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];
    sBool := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE];
    if sBool <> '' then
      IsActive := StrToBool(sBool)
    else
      IsActive := False;

    if IsActive and (Repo <> '') then
      ShowSVNStatusFrm(Repo)
    else
      ShowMessage(rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst);
  end;
end;

procedure ProcSVNUpdate(Sender: TObject);
var
  Repo: string;
  IsActive: boolean;
  sBool: string;
begin
  If Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    Repo := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];
    sBool := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE];
    if sBool <> '' then
      IsActive := StrToBool(sBool)
    else
      IsActive := False;

    debugln('ProcSVNUpdate repo='+Repo+' isactive='+sBool);

    if IsActive and (Repo <> '') then
      ShowSVNUpdateFrm(Repo)
    else
      ShowMessage(rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst);
  end;
end;

procedure ProcSVNDiff(Sender: TObject);
var
  Repo: string;
  IsActive: boolean;
  SrcFile: string;
  sBool: string;
begin
  If Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    Repo := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_REPOSITORY];
    sBool := LazarusIDE.ActiveProject.CustomSessionData.Values[SVN_ACTIVE];
    if sBool <> '' then
      IsActive := StrToBool(sBool)
    else
      IsActive := False;

    if IsActive and (Repo <> '') then
    begin
      SrcFile := SourceEditorWindow.ActiveEditor.FileName;

      if LazarusIDE.ActiveProject.FindFile(SrcFile, [pfsfOnlyEditorFiles]).IsPartOfProject then
        ShowSVNDiffFrm('-r PREV', '"' + SrcFile + '"')
      else
        ShowMessage(rsSourceFileDoesNotBelongToTheProjectPleaseAddFirst);
    end
    else
      ShowMessage(rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst);
  end;
end;

procedure ProcSVNSettings(Sender: TObject);
begin
  ShowSVNAddProjectFrm;
end;

initialization
  {$I lazsvnpkg_images.lrs}

end.
