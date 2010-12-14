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
  SysUtils, LCLtype, LResources, LCLProc;

procedure ProcSVNLog(Sender: TObject);
procedure ProcSVNCommit(Sender: TObject);
procedure ProcSVNUpdate(Sender: TObject);
procedure ProcSVNDiff(Sender: TObject);
procedure ProcSVNDiffPrev(Sender: TObject);
procedure ProcSVNDiffHead(Sender: TObject);
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
  CmdSVNDiffPrev : TIDECommand;
  CmdSVNDiffHead : TIDECommand;
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

  CmdSVNLog:=RegisterIDECommand(Cat, 'SVNLog', rsShowLog, Key, nil, @ProcSVNLog);
  CmdSVNCommit:=RegisterIDECommand(Cat, 'SVNCommit', rsCommit, Key, nil, @ProcSVNCommit);
  CmdSVNUpdate:=RegisterIDECommand(Cat, 'SVNUpdate', rsUpdate, Key, nil, @ProcSVNUpdate);
  CmdSVNDiff:=RegisterIDECommand(Cat, 'SVNDiff', rsShowDiffBase, Key, nil, @ProcSVNDiff);
  CmdSVNDiffPrev:=RegisterIDECommand(Cat, 'SVNDiffPrev', rsShowDiffPrev, Key, nil, @ProcSVNDiffPrev);
  CmdSVNDiffHead:=RegisterIDECommand(Cat, 'SVNDiffHead', rsShowDiffHead, Key, nil, @ProcSVNDiffHead);
  CmdSVNSettings:=RegisterIDECommand(Cat, 'SVNSettings', rsSVNSettings, Key, nil, @ProcSVNSettings);

  mnuSVNMain := RegisterIDEMenuSection(mnuTools, 'SVN');
  mnuSVNSection:=RegisterIDESubMenu(mnuSVNMain, 'SVN', 'SVN', nil, nil, 'menu_svn');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNLog', rsShowLog, nil, nil,
    CmdSVNLog, 'menu_svn_log');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNCommit', rsCommit, nil, nil,
    CmdSVNCommit, 'menu_svn_commit');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNUpdate', rsUpdate, nil, nil,
    CmdSVNUpdate, 'menu_svn_update');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNDiff', rsShowDiffBase, nil, nil,
    CmdSVNDiff, 'menu_svn_diff');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNDiffPrev', rsShowDiffPrev, nil, nil,
    CmdSVNDiffPrev, 'menu_svn_diff');
  RegisterIDEMenuCommand(mnuSVNSection, 'SVNDiffHead', rsShowDiffHead, nil, nil,
    CmdSVNDiffHead, 'menu_svn_diff');
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

procedure DoSVNDiff(ASwitches: String);
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
      SrcFile := SourceEditorManagerIntf.ActiveEditor.FileName;
      ShowSVNDiffFrm(ASwitches, SrcFile);
    end
    else
      ShowMessage(rsProjectIsNotActiveInSVNSettingsPleaseActivateFirst);
  end;
end;

procedure ProcSvnDiff(Sender: TObject);
begin
  DoSvnDiff('-r BASE');
end;

procedure ProcSvnDiffHead(Sender: TObject);
begin
  DoSvnDiff('-r HEAD');
end;

procedure ProcSvnDiffPrev(Sender: TObject);
begin
  DoSvnDiff('-r PREV');
end;

procedure ProcSVNSettings(Sender: TObject);
begin
  ShowSVNAddProjectFrm;
end;

initialization
  {$I lazsvnpkg_images.lrs}

end.
