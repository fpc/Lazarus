{  $Id$  }
{
 /***************************************************************************
                            pkgmanager.pas
                            --------------


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

  Author: Mattias Gaertner

  Abstract:
    TPkgManager is the class for the global PkgBoss variable, which controls
    the whole package system in the IDE.
}
unit PkgManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, LCLProc, Forms, COntrols, KeyMapping, EnvironmentOpts,
  UComponentManMain, PackageEditor, PackageDefs, PackageLinks, PackageSystem,
  ComponentReg, OpenInstalledPkgDlg,
  BasePkgManager, MainBar;

type
  TPkgManager = class(TBasePkgManager)
    function OnPackageEditorOpenPackage(Sender: TObject; APackage: TLazPackage
      ): TModalResult;
    procedure mnuConfigCustomCompsClicked(Sender: TObject);
    procedure mnuOpenInstalledPckClicked(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;

    procedure LoadInstalledPackages; override;

    function ShowConfigureCustomComponents: TModalResult; override;
    function DoNewPackage: TModalResult; override;
    function DoShowOpenInstalledPckDlg: TModalResult; override;
    function DoOpenPackage(APackage: TLazPackage): TModalResult; override;
  end;

implementation

{ TPkgManager }

function TPkgManager.OnPackageEditorOpenPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackage(APackage);
end;

procedure TPkgManager.mnuConfigCustomCompsClicked(Sender: TObject);
begin
  ShowConfigureCustomComponents;
end;

procedure TPkgManager.mnuOpenInstalledPckClicked(Sender: TObject);
begin
  DoShowOpenInstalledPckDlg;
end;

constructor TPkgManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEComponentPalette:=TIDEComponentPalette.Create;
  
  PkgLinks:=TPackageLinks.Create;
  
  PackageGraph:=TLazPackageGraph.Create;
  
  PackageEditors:=TPackageEditors.Create;
  PackageEditors.OnOpenFile:=@MainIDE.DoOpenMacroFile;
  PackageEditors.OnOpenPackage:=@OnPackageEditorOpenPackage;
end;

destructor TPkgManager.Destroy;
begin
  FreeThenNil(PackageEditors);
  FreeThenNil(PackageGraph);
  FreeThenNil(PkgLinks);
  FreeThenNil(IDEComponentPalette);
  inherited Destroy;
end;

procedure TPkgManager.ConnectMainBarEvents;
begin
  with MainIDE do begin
    itmCompsConfigCustomComps.OnClick :=@mnuConfigCustomCompsClicked;
    itmOpenInstalledPkg.OnClick :=@mnuOpenInstalledPckClicked;
  end;
end;

procedure TPkgManager.ConnectSourceNotebookEvents;
begin

end;

procedure TPkgManager.SetupMainBarShortCuts;
begin

end;

procedure TPkgManager.LoadInstalledPackages;
begin
  // base packages
  PackageGraph.AddStaticBasePackages;
  PackageGraph.RegisterStaticPackages;
  // custom packages
  // ToDo
end;

function TPkgManager.ShowConfigureCustomComponents: TModalResult;
begin
  Result:=ShowConfigureCustomComponentDlg(EnvironmentOptions.LazarusDirectory);
end;

function TPkgManager.DoNewPackage: TModalResult;
var
  NewPackage: TLazPackage;
  CurEditor: TPackageEditorForm;
begin
  Result:=mrCancel;
  // create a new package
  NewPackage:=PackageGraph.NewPackage('NewPackage');
  // open a package editor
  CurEditor:=PackageEditors.OpenEditor(NewPackage);
  CurEditor.Show;
  Result:=mrOk;
end;

function TPkgManager.DoShowOpenInstalledPckDlg: TModalResult;
var
  APackage: TLazPackage;
begin
  Result:=ShowOpenInstalledPkgDlg(APackage);
  if (Result<>mrOk) then exit;
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.DoOpenPackage(APackage: TLazPackage): TModalResult;
var
  CurEditor: TPackageEditorForm;
begin
  Result:=mrCancel;
  // open a package editor
  CurEditor:=PackageEditors.OpenEditor(APackage);
  CurEditor.Show;
  Result:=mrOk;
end;

end.

