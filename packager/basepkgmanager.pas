{  $Id$  }
{
 /***************************************************************************
                            basepkgmanager.pas
                            ------------------


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
    TBasePkgManager is the base class for TPkgManager, which controls the whole
    package system in the IDE. The base class is mostly abstract.
}
unit BasePkgManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms, PackageDefs, ComponentReg, CompilerOptions, Project;

type
  TPkgSaveFlag = (
    psfSaveAs,
    psfAskBeforeSaving
    );
  TPkgSaveFlags = set of TPkgSaveFlag;
  
  TPkgOpenFlag = (
    pofAddToRecent,
    pofRevert
    );
  TPkgOpenFlags = set of TPkgOpenFlag;

  TPkgCompileFlag = (
    pcfCleanCompile,  // append -B to the compiler options
    pcfDoNotCompileDependencies,
    pcfCompileDependenciesClean,
    pcfOnlyIfNeeded,
    pcfDoNotSaveEditorFiles
    );
  TPkgCompileFlags = set of TPkgCompileFlag;

  TBasePkgManager = class(TComponent)
  public
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure ConnectSourceNotebookEvents; virtual; abstract;
    procedure SetupMainBarShortCuts; virtual; abstract;
    procedure SetRecentPackagesMenu; virtual; abstract;
    procedure SaveSettings; virtual; abstract;

    function GetDefaultSaveDirectoryForFile(const Filename: string): string; virtual; abstract;

    procedure LoadInstalledPackages; virtual; abstract;
    procedure UpdateVisibleComponentPalette; virtual; abstract;

    function OpenProjectDependencies(AProject: TProject;
                       ReportMissing: boolean): TModalResult; virtual; abstract;
    procedure AddDefaultDependencies(AProject: TProject); virtual; abstract;
    procedure AddProjectDependency(AProject: TProject; APackage: TLazPackage); virtual; abstract;
    procedure AddProjectRegCompDependency(AProject: TProject;
                          ARegisteredComponent: TRegisteredComponent); virtual; abstract;
    procedure AddProjectLCLDependency(AProject: TProject); virtual; abstract;

    function ShowConfigureCustomComponents: TModalResult; virtual; abstract;
    function DoNewPackage: TModalResult; virtual; abstract;
    function DoShowOpenInstalledPckDlg: TModalResult; virtual; abstract;
    function DoOpenPackage(APackage: TLazPackage): TModalResult; virtual; abstract;
    function DoOpenPackageFile(AFilename: string;
                         Flags: TPkgOpenFlags): TModalResult; virtual; abstract;
    function DoSavePackage(APackage: TLazPackage;
                          Flags: TPkgSaveFlags): TModalResult; virtual; abstract;
    function DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult; virtual; abstract;
    function DoClosePackageEditor(APackage: TLazPackage): TModalResult; virtual; abstract;
    function DoCloseAllPackageEditors: TModalResult; virtual; abstract;
    procedure DoShowPackageGraphPathList(PathList: TList); virtual; abstract;
    function DoCompileProjectDependencies(AProject: TProject;
                      Flags: TPkgCompileFlags): TModalResult; virtual; abstract;
    function DoCompilePackage(APackage: TLazPackage;
                      Globals: TGlobalCompilerOptions;
                      Flags: TPkgCompileFlags): TModalResult; virtual; abstract;
    function DoSavePackageMainSource(APackage: TLazPackage;
                      Flags: TPkgCompileFlags): TModalResult; virtual; abstract;
    function OnRenameFile(const OldFilename,
                          NewFilename: string): TModalResult; virtual; abstract;
    function FindIncludeFileInProjectDependencies(Project1: TProject;
                          const Filename: string): string; virtual; abstract;

    function OnProjectInspectorOpen(Sender: TObject): boolean; virtual; abstract;
    function DoCompileAutoInstallPackages(Flags: TPkgCompileFlags
                                          ): TModalResult; virtual; abstract;
    function DoSaveAutoInstallConfig: TModalResult; virtual; abstract;
    function DoGetIDEInstallPackageOptions(
                           var InheritedOptionStrings: TInheritedCompOptsStrings
                           ): string; virtual; abstract;
  end;

var
  PkgBoss: TBasePkgManager;
  
const
  PkgSaveFlagNames: array[TPkgSaveFlag] of string = (
    'psfSaveAs',
    'psfAskBeforeSaving'
    );

  PkgOpenFlagNames: array[TPkgOpenFlag] of string = (
    'pofAddToRecent',
    'pofRevert'
    );

  PkgCompileFlagNames: array[TPkgCompileFlag] of string = (
    'pcfCleanCompile',
    'pcfDoNotCompileDependencies',
    'pcfCompileDependenciesClean',
    'pcfOnlyIfNeeded',
    'pcfAutomatic'
    );

function PkgSaveFlagsToString(Flags: TPkgSaveFlags): string;
function PkgOpenFlagsToString(Flags: TPkgOpenFlags): string;
function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;

implementation

function PkgSaveFlagsToString(Flags: TPkgSaveFlags): string;
var
  f: TPkgSaveFlag;
begin
  Result:='';
  for f:=Low(TPkgSaveFlag) to High(TPkgSaveFlag) do begin
    if not (f in Flags) then continue;
    if Result<>'' then Result:=Result+',';
    Result:=Result+PkgSaveFlagNames[f];
  end;
  Result:='['+Result+']';
end;

function PkgOpenFlagsToString(Flags: TPkgOpenFlags): string;
var
  f: TPkgOpenFlag;
begin
  Result:='';
  for f:=Low(TPkgOpenFlag) to High(TPkgOpenFlag) do begin
    if not (f in Flags) then continue;
    if Result<>'' then Result:=Result+',';
    Result:=Result+PkgOpenFlagNames[f];
  end;
  Result:='['+Result+']';
end;

function PkgCompileFlagsToString(Flags: TPkgCompileFlags): string;
var
  f: TPkgCompileFlag;
begin
  Result:='';
  for f:=Low(TPkgCompileFlag) to High(TPkgCompileFlag) do begin
    if not (f in Flags) then continue;
    if Result<>'' then Result:=Result+',';
    Result:=Result+PkgCompileFlagNames[f];
  end;
  Result:='['+Result+']';
end;

initialization
  PkgBoss:=nil;

end.

