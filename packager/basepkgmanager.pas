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
  TypInfo, Classes, SysUtils, Forms, FileUtil, LCLProc,
  LazIDEIntf, PackageIntf, MenuIntf,
  LazarusIDEStrConsts, EnvironmentOpts,
  PackageDefs, PackageSystem, ComponentReg, CompilerOptions, Project;

type
  { TBasePkgManager }

  TBasePkgManager = class(TPackageEditingInterface)
  public
    // initialization and menu
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure ConnectSourceNotebookEvents; virtual; abstract;
    procedure SetupMainBarShortCuts; virtual; abstract;
    procedure SetRecentPackagesMenu; virtual; abstract;
    procedure SaveSettings; virtual; abstract;
    procedure UpdateVisibleComponentPalette; virtual; abstract;
    procedure ProcessCommand(Command: word; var Handled: boolean); virtual; abstract;
    procedure OnSourceEditorPopupMenu(const AddMenuItemProc: TAddMenuItemProc); virtual; abstract;
    procedure TranslateResourceStrings; virtual; abstract;

    // files
    function GetDefaultSaveDirectoryForFile(const Filename: string): string; virtual; abstract;
    function OnRenameFile(const OldFilename, NewFilename: string;
                          IsPartOfProject: boolean): TModalResult; virtual; abstract;
    function FindIncludeFileInProjectDependencies(Project1: TProject;
                          const Filename: string): string; virtual; abstract;
    function AddDependencyToOwners(OwnerList: TFPList; APackage: TLazPackage;
                   OnlyTestIfPossible: boolean = false): TModalResult; virtual; abstract;
    function SearchFile(const AFilename: string;
                        SearchFlags: TSearchIDEFileFlags;
                        InObject: TObject): TPkgFile; virtual; abstract;
    function SearchUnitInDesigntimePackages(const AnUnitName: string;
                                InObject: TObject): TPkgFile; virtual; abstract;
    function AddDependencyToUnitOwners(const OwnedFilename,
                     RequiredUnitname: string): TModalResult; virtual; abstract;
    procedure GetPackagesChangedOnDisk(var ListOfPackages: TFPList); virtual; abstract;
    function RevertPackages(APackageList: TFPList): TModalResult; virtual; abstract;

    // project
    function OpenProjectDependencies(AProject: TProject;
                       ReportMissing: boolean): TModalResult; virtual; abstract;
    procedure AddDefaultDependencies(AProject: TProject); virtual; abstract;
    function AddProjectDependency(AProject: TProject; APackage: TLazPackage;
                                  OnlyTestIfPossible: boolean = false): TModalResult; virtual; abstract;
    function AddProjectDependency(AProject: TProject;
                                  ADependency: TPkgDependency): TModalResult; virtual; abstract;
    procedure AddProjectRegCompDependency(AProject: TProject;
                          ARegisteredComponent: TRegisteredComponent); virtual; abstract;
    procedure AddProjectLCLDependency(AProject: TProject); virtual; abstract;
    function AddProjectDependencies(AProject: TProject; const Packages: string;
                                  OnlyTestIfPossible: boolean = false): TModalResult; virtual; abstract;
    function CheckProjectHasInstalledPackages(AProject: TProject; Interactive: boolean): TModalResult; virtual; abstract;
    function CanOpenDesignerForm(AnUnitInfo: TUnitInfo; 
                                 Interactive: boolean): TModalResult; virtual; abstract;
    function OnProjectInspectorOpen(Sender: TObject): boolean; virtual; abstract;
    function OnProjectInspectorAddDependency(Sender: TObject;
                  ADependency: TPkgDependency): TModalResult; virtual; abstract;
    function OnProjectInspectorRemoveDependency(Sender: TObject;
                  ADependency: TPkgDependency): TModalResult; virtual; abstract;
    function OnProjectInspectorReAddDependency(Sender: TObject;
                  ADependency: TPkgDependency): TModalResult; virtual; abstract;

    // package editors
    function DoNewPackage: TModalResult; virtual; abstract;
    function DoOpenPackage(APackage: TLazPackage; Flags: TPkgOpenFlags;
                           ShowAbort: boolean): TModalResult; virtual; abstract;
    function DoSavePackage(APackage: TLazPackage;
                          Flags: TPkgSaveFlags): TModalResult; virtual; abstract;

    function DoClosePackageEditor(APackage: TLazPackage): TModalResult; virtual; abstract;
    function DoCloseAllPackageEditors: TModalResult; virtual; abstract;
    function AddPackageDependency(APackage: TLazPackage; const ReqPackage: string;
                                  OnlyTestIfPossible: boolean = false): TModalResult; virtual; abstract;
    procedure OpenHiddenModifiedPackages; virtual; abstract;

    // package graph
    procedure DoShowPackageGraphPathList(PathList: TFPList); virtual; abstract;
    procedure RebuildDefineTemplates; virtual; abstract;
    procedure LazarusSrcDirChanged; virtual; abstract;

    // package compilation
    function DoCompileProjectDependencies(AProject: TProject;
                      Flags: TPkgCompileFlags): TModalResult; virtual; abstract;
    function DoCompilePackage(APackage: TLazPackage; Flags: TPkgCompileFlags;
                              ShowAbort: boolean;
                              Globals: TGlobalCompilerOptions = nil): TModalResult; virtual; abstract;

    // package installation
    procedure LoadInstalledPackages; virtual; abstract;
    function DoShowOpenInstalledPckDlg: TModalResult; virtual; abstract;
    function ShowConfigureCustomComponents: TModalResult; virtual; abstract;
    function DoCompileAutoInstallPackages(Flags: TPkgCompileFlags;
                                          OnlyBase: boolean): TModalResult; virtual; abstract;
    function DoSaveAutoInstallConfig: TModalResult; virtual; abstract;

    // components
    function GetUsableComponentUnits(CurRoot: TPersistent): TFPList; virtual; abstract; // list of TUnitInfo
    procedure IterateComponentNames(CurRoot: TPersistent; TypeData: PTypeData;
                                    Proc: TGetStrProc); virtual; abstract;
    function FindUsableComponent(CurRoot: TPersistent;
                  const ComponentPath: string): TComponent; virtual; abstract;
    function FindReferencedRootComponent(CurRoot: TPersistent; 
         const ComponentName: string): TComponent; virtual; abstract;
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
    'pofRevert',
    'pofConvertMacros',
    'pofMultiOpen',
    'pofDoNotOpenEditor'
    );

function PkgSaveFlagsToString(Flags: TPkgSaveFlags): string;
function PkgOpenFlagsToString(Flags: TPkgOpenFlags): string;

procedure GetDescriptionOfDependencyOwner(Dependency: TPkgDependency;
                                          out Description: string);
procedure GetDirectoryOfDependencyOwner(Dependency: TPkgDependency;
                                        out Directory: string);


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

procedure GetDescriptionOfDependencyOwner(Dependency: TPkgDependency;
  out Description: string);
var
  DepOwner: TObject;
begin
  DepOwner:=Dependency.Owner;
  if (DepOwner<>nil) then begin
    if DepOwner is TLazPackage then begin
      Description:=Format(lisPkgMangPackage, [TLazPackage(DepOwner).IDAsString]
        );
    end else if DepOwner is TProject then begin
      Description:=Format(lisPkgMangProject, [ExtractFileNameOnly(TProject(
        DepOwner).ProjectInfoFile)]);
    end else if (DepOwner=PkgBoss) or (DepOwner=PackageGraph) then begin
      Description:=lisPkgMangLazarus;
    end else begin
      Description:=dbgsName(DepOwner)
    end;
  end else begin
    Description:=Format(lisPkgMangDependencyWithoutOwner, [Dependency.AsString]
      );
  end;
end;

procedure GetDirectoryOfDependencyOwner(Dependency: TPkgDependency;
  out Directory: string);
var
  DepOwner: TObject;
begin
  DepOwner:=Dependency.Owner;
  if (DepOwner<>nil) then begin
    if DepOwner is TLazPackage then begin
      Directory:=TLazPackage(DepOwner).Directory;
    end else if DepOwner is TProject then begin
      Directory:=TProject(DepOwner).ProjectDirectory;
    end else if DepOwner=PkgBoss then begin
      Directory:=EnvironmentOptions.LazarusDirectory;
    end else begin
      Directory:=''
    end;
  end else begin
    Directory:=''
  end;
end;

{ TBasePkgManager }

constructor TBasePkgManager.Create(TheOwner: TComponent);
begin
  PackageEditingInterface:=Self;
  inherited Create(TheOwner);
end;

destructor TBasePkgManager.Destroy;
begin
  inherited Destroy;
  PackageEditingInterface:=nil;
end;

initialization
  PkgBoss:=nil;

end.

