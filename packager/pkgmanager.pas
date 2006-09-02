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
  // FCL, LCL
  Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, Dialogs, Menus,
  // codetools
  CodeToolManager, CodeCache, BasicCodeTools, DefineTemplates,
  AVL_Tree, Laz_XMLCfg,
  // IDE Interface
  IDEExternToolIntf, NewItemIntf, ProjectIntf, PackageIntf, MenuIntf,
  MacroIntf, LazIDEIntf,
  // IDE
  LazConf, LazarusIDEStrConsts, IDEProcs, ObjectLists, DialogProcs, IDECommands,
  EnvironmentOpts, MiscOptions, InputHistory, ProjectDefs, Project,
  ComponentReg, UComponentManMain, PackageEditor, AddToPackageDlg, PackageDefs,
  PackageLinks, PackageSystem, OpenInstalledPkgDlg, PkgGraphExplorer,
  BrokenDependenciesDlg, CompilerOptions, ExtToolEditDlg,
  MsgView, BuildLazDialog, NewDialog,
  ProjectInspector, ComponentPalette, UnitEditor, AddFileToAPackageDlg,
  LazarusPackageIntf, PublishProjectDlg, InstallPkgSetDlg,
  // bosses
  BasePkgManager,
  MainBar, MainIntf, MainBase;

type
  TPkgUninstallFlag = (
    puifDoNotConfirm,
    puifDoNotBuildIDE
    );

  TPkgUninstallFlags = set of TPkgUninstallFlag;

  { TPkgManager }

  TPkgManager = class(TBasePkgManager)
    // events - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // package editor
    function OnPackageEditorCompilePackage(Sender: TObject;
                          APackage: TLazPackage;
                          CompileClean, CompileRequired: boolean): TModalResult;
    function OnPackageEditorCreateMakefile(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    function OnPackageEditorCreateFile(Sender: TObject;
                                       Params: TAddToPkgResult): TModalResult;
    function OnPackageEditorDeleteAmbiguousFiles(Sender: TObject;
      APackage: TLazPackage; const Filename: string): TModalResult;
    function OnPackageEditorAddToProject(Sender: TObject; APackage: TLazPackage;
                                     OnlyTestIfPossible: boolean): TModalResult;
    function OnPackageEditorInstallPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    function OnPackageEditorPublishPackage(Sender: TObject;
      APackage: TLazPackage): TModalResult;
    function OnPackageEditorRevertPackage(Sender: TObject; APackage: TLazPackage
      ): TModalResult;
    function OnPackageEditorUninstallPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    function OnPackageEditorOpenPkgFile(Sender: TObject; PkgFile: TPkgFile
                                        ): TModalResult;
    function OnPackageEditorOpenPackage(Sender: TObject; APackage: TLazPackage
                                        ): TModalResult;
    function OnPackageEditorSavePackage(Sender: TObject; APackage: TLazPackage;
                                        SaveAs: boolean): TModalResult;
    function OnPackageEditorViewPkgSourcePackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    procedure OnPackageEditorFreeEditor(APackage: TLazPackage);
    procedure OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
                              const AFilename: string; var TheUnitName: string;
                              var HasRegisterProc: boolean);
    procedure OnPackageEditorImExportCompilerOptions(Sender: TObject);
    // package graph
    function PackageGraphExplorerOpenPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    function PackageGraphExplorerOpenProject(Sender: TObject;
                                             AProject: TProject): TModalResult;
    function PackageGraphExplorerUninstallPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    procedure PackageGraphAddPackage(Pkg: TLazPackage);
    procedure PackageGraphBeginUpdate(Sender: TObject);
    procedure PackageGraphChangePackageName(APackage: TLazPackage;
                                            const OldName: string);
    procedure PackageGraphDeletePackage(APackage: TLazPackage);
    procedure PackageGraphDependencyModified(ADependency: TPkgDependency);
    procedure PackageGraphEndUpdate(Sender: TObject; GraphChanged: boolean);
    procedure PackageGraphFindFPCUnit(const UnitName, Directory: string;
                                      var Filename: string);

    // menu
    procedure MainIDEitmPkgOpenPackageFileClick(Sender: TObject);
    procedure MainIDEitmPkgPkgGraphClick(Sender: TObject);
    procedure MainIDEitmPkgEditInstallPkgsClick(Sender: TObject);
    procedure MainIDEitmPkgAddCurUnitToPkgClick(Sender: TObject);
    procedure MainIDEitmPkgOpenPackageOfCurUnitClicked(Sender: TObject);
    procedure MainIDEitmConfigCustomCompsClicked(Sender: TObject);
    procedure MainIDEitmOpenRecentPackageClicked(Sender: TObject);
    procedure MainIDEitmPkgOpenPackageClicked(Sender: TObject);

    // component palette
    procedure IDEComponentPaletteEndUpdate(Sender: TObject;
                                           PaletteChanged: boolean);
    procedure IDEComponentPaletteOpenPackage(Sender: TObject);
    procedure IDEComponentPaletteOpenUnit(Sender: TObject);

    // LCL
    procedure OnApplicationIdle(Sender: TObject);

    // package links
    function PkgLinksDependencyOwnerGetPkgFilename(PkgLinks: TPackageLinks;
      Dependency: TPkgDependency): boolean;

    // misc
    procedure GetDependencyOwnerDescription(Dependency: TPkgDependency;
                                            var Description: string);
    procedure GetDependencyOwnerDirectory(Dependency: TPkgDependency;
                                          var Directory: string);
    procedure GetWritablePkgOutputDirectory(APackage: TLazPackage;
                                            var AnOutDirectory: string);
    procedure OnCheckInstallPackageList(PkgIDList: TList; var Ok: boolean);
    function LoadDependencyList(FirstDependency: TPkgDependency): TModalResult;
    procedure OnOpenPackageForCurrentSrcEditFile(Sender: TObject);
  private
    FirstAutoInstallDependency: TPkgDependency;
    // helper functions
    function DoShowSavePackageAsDialog(APackage: TLazPackage): TModalResult;
    function DoWriteMakefile(APackage: TLazPackage): TModalResult;
    function CompileRequiredPackages(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency;
                                 Globals: TGlobalCompilerOptions;
                                 Policies: TPackageUpdatePolicies): TModalResult;
    function CheckPackageGraphForCompilation(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency;
                                 const Directory: string): TModalResult;
    function DoPreparePackageOutputDirectory(APackage: TLazPackage;
                                             CleanUp: boolean): TModalResult;
    function DoSavePackageCompiledState(APackage: TLazPackage;
                  const CompilerFilename, CompilerParams: string): TModalResult;
    function DoLoadPackageCompiledState(APackage: TLazPackage;
                                        IgnoreErrors: boolean): TModalResult;
    function CheckIfPackageNeedsCompilation(APackage: TLazPackage;
                            const CompilerFilename, CompilerParams,
                            SrcFilename: string): TModalResult;
    function CheckAmbiguousPackageUnits(APackage: TLazPackage): TModalResult;
    function MacroFunctionPkgSrcPath(Data: Pointer): boolean;
    function MacroFunctionPkgUnitPath(Data: Pointer): boolean;
    function MacroFunctionPkgIncPath(Data: Pointer): boolean;
    function DoGetUnitRegisterInfo(const AFilename: string;
                          var TheUnitName: string; var HasRegisterProc: boolean;
                          IgnoreErrors: boolean): TModalResult;
    procedure SaveAutoInstallDependencies(SetWithStaticPcksFlagForIDE: boolean);
    procedure LoadStaticBasePackages;
    procedure LoadStaticCustomPackages;
    function LoadInstalledPackage(const PackageName: string;
                                  AddToAutoInstall: boolean): TLazPackage;
    procedure LoadAutoInstallPackages;
    procedure SortAutoInstallDependencies;
    procedure AddUnitToProjectMainUsesSection(AProject: TProject;
                                    const AnUnitName, AnUnitInFilename: string);
                                    
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // initialization and menu
    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;
    procedure SetRecentPackagesMenu; override;
    procedure AddFileToRecentPackages(const Filename: string);
    procedure SaveSettings; override;
    procedure UpdateVisibleComponentPalette; override;
    procedure ProcessCommand(Command: word; var Handled: boolean); override;
    procedure OnSourceEditorPopupMenu(const AddMenuItemProc: TAddMenuItemProc); override;

    // files
    function GetDefaultSaveDirectoryForFile(const Filename: string): string; override;
    function GetPublishPackageDir(APackage: TLazPackage): string;
    function OnRenameFile(const OldFilename, NewFilename: string;
                          IsPartOfProject: boolean): TModalResult; override;
    function FindIncludeFileInProjectDependencies(Project1: TProject;
                          const Filename: string): string; override;
    function AddUnitDependenciesForComponentClasses(const UnitFilename: string;
                         ComponentClassnames: TStrings): TModalResult; override;
    function GetMissingDependenciesForUnit(const UnitFilename: string;
                         ComponentClassnames: TStrings;
                         var List: TObjectArray): TModalResult;
    function GetOwnersOfUnit(const UnitFilename: string): TList; override;
    procedure ExtendOwnerListWithUsedByOwners(OwnerList: TList); override;
    function GetSourceFilesOfOwners(OwnerList: TList): TStrings; override;
    function GetPackageOfCurrentSourceEditor: TPkgFile;
    function DoOpenPkgFile(PkgFile: TPkgFile): TModalResult;
    function FindVirtualUnitSource(PkgFile: TPkgFile): string;
    function SearchFile(const AFilename: string;
                        SearchFlags: TSearchIDEFileFlags;
                        InObject: TObject): TPkgFile; override;

    // package graph
    function AddPackageToGraph(APackage: TLazPackage; Replace: boolean): TModalResult;
    function DoShowPackageGraph: TModalResult;
    procedure DoShowPackageGraphPathList(PathList: TList); override;
    function ShowBrokenDependenciesReport(Dependencies: TList): TModalResult;
    procedure RebuildDefineTemplates; override;

    // project
    function OpenProjectDependencies(AProject: TProject;
                                ReportMissing: boolean): TModalResult; override;
    function CheckProjectHasInstalledPackages(AProject: TProject): TModalResult; override;
    function CanOpenDesignerForm(AnUnitInfo: TUnitInfo): TModalResult; override;
    procedure AddDefaultDependencies(AProject: TProject); override;
    function AddProjectDependency(AProject: TProject; APackage: TLazPackage;
                                  OnlyTestIfPossible: boolean = false): TModalResult; override;
    function AddProjectDependency(AProject: TProject;
                                  ADependency: TPkgDependency): TModalResult;
    procedure AddProjectRegCompDependency(AProject: TProject;
                          ARegisteredComponent: TRegisteredComponent); override;
    procedure AddProjectLCLDependency(AProject: TProject); override;
    function OnProjectInspectorOpen(Sender: TObject): boolean; override;
    function OnProjectInspectorAddDependency(Sender: TObject;
                           ADependency: TPkgDependency): TModalResult; override;
    function OnProjectInspectorRemoveDependency(Sender: TObject;
                           ADependency: TPkgDependency): TModalResult; override;
    function OnProjectInspectorReAddDependency(Sender: TObject;
                           ADependency: TPkgDependency): TModalResult; override;

    // package editors
    function DoNewPackage: TModalResult; override;
    function DoShowOpenInstalledPckDlg: TModalResult; override;
    function DoOpenPackage(APackage: TLazPackage): TModalResult; override;
    function DoOpenPackageFile(AFilename: string;
                         Flags: TPkgOpenFlags): TModalResult; override;
    function DoSavePackage(APackage: TLazPackage;
                           Flags: TPkgSaveFlags): TModalResult; override;
    function DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult; override;
    function DoClosePackageEditor(APackage: TLazPackage): TModalResult; override;
    function DoCloseAllPackageEditors: TModalResult; override;
    function DoAddActiveUnitToAPackage: TModalResult;
    function WarnAboutMissingPackageFiles(APackage: TLazPackage): TModalResult;

    // package compilation
    function DoCompileProjectDependencies(AProject: TProject;
                               Flags: TPkgCompileFlags): TModalResult; override;
    function DoCompilePackage(APackage: TLazPackage; Flags: TPkgCompileFlags;
                              Globals: TGlobalCompilerOptions = nil): TModalResult; override;
    function DoSavePackageMainSource(APackage: TLazPackage;
                              Flags: TPkgCompileFlags): TModalResult; override;
    function DoCreatePackageMakefile(APackage: TLazPackage): TModalResult;
    function DoCheckIfDependenciesNeedCompilation(DependencyOwner: TObject;
                            StateFileAge: longint): TModalResult; override;

    // package installation
    procedure LoadInstalledPackages; override;
    procedure UnloadInstalledPackages;
    function ShowConfigureCustomComponents: TModalResult; override;
    function DoInstallPackage(APackage: TLazPackage): TModalResult;
    function DoUninstallPackage(APackage: TLazPackage;
                                Flags: TPkgUninstallFlags): TModalResult;
    function DoOpenPackageSource(APackage: TLazPackage): TModalResult;
    function DoCompileAutoInstallPackages(Flags: TPkgCompileFlags
                                          ): TModalResult; override;
    function DoSaveAutoInstallConfig: TModalResult; override;
    function DoGetIDEInstallPackageOptions(
                           var InheritedOptionStrings: TInheritedCompOptsStrings
                           ): string; override;
    function DoPublishPackage(APackage: TLazPackage; Flags: TPkgSaveFlags;
                              ShowDialog: boolean): TModalResult;
  end;


  { TLazPackageDescriptors }

  TLazPackageDescriptors = class(TPackageDescriptors)
  private
    fDestroying: boolean;
    fItems: TList; // list of TProjectDescriptor
  protected
    function GetItems(Index: integer): TPackageDescriptor; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; override;
    function GetUniqueName(const Name: string): string; override;
    function IndexOf(const Name: string): integer; override;
    function FindByName(const Name: string): TPackageDescriptor; override;
    procedure RegisterDescriptor(Descriptor: TPackageDescriptor); override;
    procedure UnregisterDescriptor(Descriptor: TPackageDescriptor); override;
    procedure AddDefaultPackageDescriptors;
  public
    property Items[Index: integer]: TPackageDescriptor read GetItems; default;
  end;
  
  
  { TPackageDescriptorStd }
  
  TPackageDescriptorStd = class(TPackageDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

var
  LazPackageDescriptors: TLazPackageDescriptors;

implementation

{ TPkgManager }

procedure TPkgManager.MainIDEitmPkgOpenPackageFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
  I: Integer;
  OpenFlags: TPkgOpenFlags;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenPackageFile;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    OpenDialog.Filter:=lisLazarusPackage+' (*.lpk)|*.lpk'
                     +'|'+dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask;
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      OpenFlags:=[pofAddToRecent];
      For I := 0 to OpenDialog.Files.Count-1 do
        Begin
          AFilename:=CleanAndExpandFilename(OpenDialog.Files.Strings[i]);
          if DoOpenPackageFile(AFilename,OpenFlags)=mrAbort then begin
            break;
          end;
        end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TPkgManager.MainIDEitmPkgPkgGraphClick(Sender: TObject);
begin
  DoShowPackageGraph;
end;

procedure TPkgManager.MainIDEitmPkgEditInstallPkgsClick(Sender: TObject);

  function CreateChangeReport(OldDependencyList,
    NewDependencyList: TPkgDependency): string;
  var
    CurDependency: TPkgDependency;
    OldDependency: TPkgDependency;
    NewDependency: TPkgDependency;
  begin
    Result:='';

    // list all packages, that will be installed
    CurDependency:=NewDependencyList;
    while CurDependency<>nil do begin
      Result:=Result+CurDependency.AsString;
      OldDependency:=FindDependencyByNameInList(OldDependencyList,pdlRequires,
                                                CurDependency.PackageName);
      if OldDependency<>nil then begin
        // stay installed
        if CurDependency.AsString<>OldDependency.AsString then
          Result:=Result+' (old: '+OldDependency.AsString+')';
      end else begin
        // newly installed
        Result:=Result+' (new)';
      end;
      Result:=Result+#13;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;

    // list all packages, that will be removed
    CurDependency:=OldDependencyList;
    while CurDependency<>nil do begin
      NewDependency:=FindDependencyByNameInList(NewDependencyList,pdlRequires,
                                                CurDependency.PackageName);
      if NewDependency=nil then begin
        // this package will be removed
        Result:=Result+CurDependency.AsString+' (remove)'#13;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
  end;

var
  RebuildIDE: Boolean;
  PkgIDList: TList;
  NewFirstAutoInstallDependency: TPkgDependency;
  BuildIDEFlags: TBuildLazarusFlags;
  ok: boolean;
  Report: String;
  PkgList: TList;
  RequiredPackage: TLazPackage;
  i: Integer;
  CurDependency: TPkgDependency;
begin
  RebuildIDE:=false;
  PkgIDList:=nil;
  NewFirstAutoInstallDependency:=nil;
  PkgList:=nil;
  try
    if ShowEditInstallPkgsDialog(FirstAutoInstallDependency,
      @OnCheckInstallPackageList,PkgIDList,RebuildIDE)<>mrOk
    then exit;
    
    OnCheckInstallPackageList(PkgIDList,ok);
    if not ok then exit;

    // create new auto install dependency PkgIDList
    ListPkgIDToDependencyList(PkgIDList,NewFirstAutoInstallDependency,
      pdlRequires,Self,true);

    PackageGraph.SortDependencyListTopologically(NewFirstAutoInstallDependency,
                                                 false);

    // tell the user, which packages will stay, which will be removed and
    // which will be newly installed
    Report:=CreateChangeReport(FirstAutoInstallDependency,
                               NewFirstAutoInstallDependency);
    if MessageDlg('Confirm new package set for the IDE',
      'This will happen:'#13#13
      +Report+#13'Continue?',mtConfirmation,[mbYes,mbNo],0)<>mrYes then exit;

    // try to commit changes -> replace install list
    PackageGraph.BeginUpdate(true);
    try
      // get all required packages
      debugln('TPkgManager.MainIDEitmPkgEditInstallPkgsClick GetAllRequiredPackages for ',DependencyListAsString(NewFirstAutoInstallDependency,pdlRequires));
      if LoadDependencyList(NewFirstAutoInstallDependency)<>mrOk then exit;
      PackageGraph.GetAllRequiredPackages(NewFirstAutoInstallDependency,PkgList);
      
      // mark packages for installation
      debugln('TPkgManager.MainIDEitmPkgEditInstallPkgsClick mark packages for installation');
      for i:=0 to PkgList.Count-1 do begin
        RequiredPackage:=TLazPackage(PkgList[i]);
        if RequiredPackage.AutoInstall=pitNope then begin
          RequiredPackage.AutoInstall:=pitStatic;
        end;
      end;

      // mark packages for uninstall
      debugln('TPkgManager.MainIDEitmPkgEditInstallPkgsClick mark packages for uninstall');
      CurDependency:=FirstAutoInstallDependency;
      while CurDependency<>nil do begin
        if CurDependency.RequiredPackage<>nil then
          CurDependency.RequiredPackage.AutoInstall:=pitNope;
        CurDependency:=CurDependency.NextRequiresDependency;
      end;

      // replace install list
      debugln('TPkgManager.MainIDEitmPkgEditInstallPkgsClick replace install list');
      FreeDependencyList(FirstAutoInstallDependency,pdlRequires);
      FirstAutoInstallDependency:=NewFirstAutoInstallDependency;
      NewFirstAutoInstallDependency:=nil;
    finally
      PackageGraph.EndUpdate;
    end;

    // save package list
    debugln('TPkgManager.MainIDEitmPkgEditInstallPkgsClick save package list');
    SortAutoInstallDependencies;
    SaveAutoInstallDependencies(true);

    // save IDE build configs, so user can build IDE on command line
    BuildIDEFlags:=[blfWithStaticPackages,blfDontClean,blfOnlyIDE];
    if MainIDE.DoSaveBuildIDEConfigs(BuildIDEFlags)<>mrOk then exit;

    if RebuildIDE then begin
      // rebuild Lazarus
      if MainIDE.DoBuildLazarus(BuildIDEFlags)<>mrOk then exit;
    end;

  finally
    if PkgIDList<>nil then FreeListObjects(PkgIDList,true);
    FreeDependencyList(NewFirstAutoInstallDependency,pdlRequires);
    PkgList.Free;
  end;
end;

procedure TPkgManager.IDEComponentPaletteEndUpdate(Sender: TObject;
  PaletteChanged: boolean);
begin
  UpdateVisibleComponentPalette;
end;

procedure TPkgManager.IDEComponentPaletteOpenPackage(Sender: TObject);
begin
  if (Sender=nil) or (not (Sender is TLazPackage)) then exit;
  DoOpenPackage(TLazPackage(Sender));
end;

procedure TPkgManager.IDEComponentPaletteOpenUnit(Sender: TObject);
var
  PkgComponent: TPkgComponent;
  PkgFile: TPkgFile;
  Filename: String;
begin
  if (Sender=nil) then exit;
  if (Sender is TPkgFile) then
    DoOpenPkgFile(TPkgFile(Sender))
  else if (Sender is TPkgComponent) then begin
    PkgComponent:=TPkgComponent(Sender);
    PkgFile:=PkgComponent.PkgFile;
    if PkgFile=nil then exit;
    Filename:='';
    if PkgFile.FileType=pftVirtualUnit then
      Filename:=FindVirtualUnitSource(PkgFile);
    if Filename='' then
      Filename:=PkgFile.Filename;
    MainIDE.DoOpenFileAndJumpToIdentifier(
      Filename,PkgComponent.ComponentClass.ClassName,
      -1, // open page somewhere
      [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros]);
  end;
end;

procedure TPkgManager.GetDependencyOwnerDescription(
  Dependency: TPkgDependency; var Description: string);
begin
  GetDescriptionOfDependencyOwner(Dependency,Description);
end;

procedure TPkgManager.GetDependencyOwnerDirectory(Dependency: TPkgDependency;
  var Directory: string);
begin
  GetDirectoryOfDependencyOwner(Dependency,Directory);
end;

procedure TPkgManager.GetWritablePkgOutputDirectory(APackage: TLazPackage;
  var AnOutDirectory: string);
var
  NewOutDir: String;
begin
  if DirectoryIsWritableCached(AnOutDirectory) then exit;

  ForceDirectory(AnOutDirectory);
  InvalidateFileStateCache;
  if DirectoryIsWritableCached(AnOutDirectory) then exit;
  //debugln('TPkgManager.GetWritablePkgOutputDirectory AnOutDirectory=',AnOutDirectory,' ',dbgs(DirectoryIsWritable(AnOutDirectory)));
  
  // output directory is not writable
  // -> redirect to config directory
  NewOutDir:=SetDirSeparators('/$(TargetCPU)-$(TargetOS)');
  IDEMacros.SubstituteMacros(NewOutDir);
  NewOutDir:=TrimFilename(GetPrimaryConfigPath+PathDelim+'lib'+PathDelim
                          +APackage.Name+NewOutDir);
  AnOutDirectory:=NewOutDir;
  debugln('TPkgManager.GetWritablePkgOutputDirectory APackage=',APackage.IDAsString,' AnOutDirectory="',AnOutDirectory,'"');
end;

procedure TPkgManager.OnCheckInstallPackageList(PkgIDList: TList;
  var Ok: boolean);
var
  NewFirstAutoInstallDependency: TPkgDependency;
  PkgList: TList;
  i: Integer;
  APackage: TLazPackage;
begin
  Ok:=false;
  try
    // create new auto install dependency PkgIDList
    ListPkgIDToDependencyList(PkgIDList,NewFirstAutoInstallDependency,
      pdlRequires,Self,true);

    // get all required packages
    if LoadDependencyList(NewFirstAutoInstallDependency)<>mrOk then exit;
    PackageGraph.GetAllRequiredPackages(NewFirstAutoInstallDependency,PkgList);

    // check if any package is a runtime package, that is not needed
    for i:=0 to PkgList.Count-1 do begin
      APackage:=TLazPackage(PkgList[i]);
      if (APackage.PackageType=lptRunTime)
      and (APackage.FirstUsedByDependency=nil) then begin
        // this is a runtime only package, not needed by any other package
        if MessageDlg(lisPkgMangPackageIsNoDesigntimePackage,
          Format(lisPkgMangThePackageIsARuntimeOnlyPackageRuntimeOnlyPackages, [
            APackage.IDAsString, #13]),
          mtWarning,[mbIgnore,mbCancel],0)<>mrIgnore
        then
          exit;
      end;
    end;

    // try save all modified packages
    for i:=0 to PkgList.Count-1 do begin
      APackage:=TLazPackage(PkgList[i]);
      if (not APackage.AutoCreated)
      and (APackage.IsVirtual or APackage.Modified) then begin
        if DoSavePackage(APackage,[])<>mrOk then exit;
      end;
    end;

    Ok:=true;
  finally
    FreeDependencyList(NewFirstAutoInstallDependency,pdlRequires);
    PkgList.Free;
  end;
end;

function TPkgManager.LoadDependencyList(FirstDependency: TPkgDependency
  ): TModalResult;
var
  CurDependency: TPkgDependency;
  OpenResult: TLoadPackageResult;
begin
  Result:=mrCancel;
  // load all packages
  CurDependency:=FirstDependency;
  while CurDependency<>nil do begin
    OpenResult:=PackageGraph.OpenDependency(CurDependency);
    if OpenResult<>lprSuccess then begin
      MessageDlg('Error',
        'Unable to load package "'+CurDependency.AsString+'"',
        mtError,[mbCancel],0);
      exit;
    end;
    CurDependency:=CurDependency.NextRequiresDependency;
  end;
  Result:=mrOk;
end;

procedure TPkgManager.OnOpenPackageForCurrentSrcEditFile(Sender: TObject);
var
  PkgFile: TPkgFile;
begin
  PkgFile:=GetPackageOfCurrentSourceEditor;
  if PkgFile<>nil then
    DoOpenPackage(PkgFile.LazPackage);
end;

procedure TPkgManager.MainIDEitmPkgAddCurUnitToPkgClick(Sender: TObject);
begin
  DoAddActiveUnitToAPackage;
end;

procedure TPkgManager.MainIDEitmPkgOpenPackageOfCurUnitClicked(Sender: TObject);
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  PkgFile: TPkgFile;
  Filename: String;
begin
  MainIDE.GetCurrentUnitInfo(ActiveSourceEditor,ActiveUnitInfo);
  if ActiveSourceEditor=nil then exit;

  Filename:=ActiveUnitInfo.Filename;

  PkgFile:=PackageGraph.FindFileInAllPackages(Filename,false,true,
                                            not ActiveUnitInfo.IsPartOfProject);
  if PkgFile=nil then begin
    MessageDlg(lisProjAddPackageNotFound,
      lisPkgThisFileIsNotInAnyLoadedPackage, mtInformation,
      [mbCancel],0);
    exit;
  end;
  DoOpenPackageFile(PkgFile.LazPackage.Filename,[pofAddToRecent])
end;

function TPkgManager.OnPackageEditorCompilePackage(Sender: TObject;
  APackage: TLazPackage; CompileClean, CompileRequired: boolean): TModalResult;
var
  Flags: TPkgCompileFlags;
  Globals: TGlobalCompilerOptions;
begin
  Flags:=[];
  if CompileClean then Include(Flags,pcfCleanCompile);
  if CompileRequired then Include(Flags,pcfCompileDependenciesClean);
  if Project1<>nil then
    Globals:=Project1.CompilerOptions.Globals
  else
    Globals:=nil;
  //debugln('TPkgManager.OnPackageEditorCompilePackage OS=',Globals.TargetOS);
  Result:=DoCompilePackage(APackage,Flags,Globals);
end;

function TPkgManager.OnPackageEditorCreateMakefile(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoCreatePackageMakefile(APackage);
end;

function TPkgManager.OnPackageEditorCreateFile(Sender: TObject;
  Params: TAddToPkgResult): TModalResult;
var
  LE: String;
  UsesLine: String;
  NewSource: String;
begin
  Result:=mrCancel;
  // create sourcecode
  LE:=LineEnding;
  UsesLine:='Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs';
  if (System.Pos(Params.UsedUnitname,UsesLine)<1)
  and (Params.UsedUnitname<>'') then
    UsesLine:=UsesLine+', '+Params.UsedUnitname;
  NewSource:=
     'unit '+Params.UnitName+';'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+UsesLine+';'+LE
    +LE
    +'type'+LE
    +'  '+Params.NewClassName+' = class('+Params.AncestorType+')'+LE
    +'  private'+LE
    +'    { Private declarations }'+LE
    +'  protected'+LE
    +'    { Protected declarations }'+LE
    +'  public'+LE
    +'    { Public declarations }'+LE
    +'  published'+LE
    +'    { Published declarations }'+LE
    +'  end;'+LE
    +LE
    +'procedure Register;'+LE
    +LE
    +'implementation'+LE
    +LE
    +'procedure Register;'+LE
    +'begin'+LE
    +'  RegisterComponents('''+Params.PageName+''',['+Params.NewClassName+']);'+LE
    +'end;'+LE
    +LE
    +'end.'+LE;

  Result:=MainIDE.DoNewEditorFile(FileDescriptorUnit,
     Params.UnitFilename,NewSource,
     [nfOpenInEditor,nfIsNotPartOfProject,nfSave,nfAddToRecent]);
end;

function TPkgManager.OnPackageEditorDeleteAmbiguousFiles(Sender: TObject;
  APackage: TLazPackage; const Filename: string): TModalResult;
begin
  Result:=MainIDE.DoDeleteAmbiguousFiles(Filename);
end;

function TPkgManager.OnPackageEditorAddToProject(Sender: TObject;
  APackage: TLazPackage; OnlyTestIfPossible: boolean): TModalResult;
begin
  Result:=AddProjectDependency(Project1,APackage,OnlyTestIfPossible);
end;

function TPkgManager.OnPackageEditorInstallPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoInstallPackage(APackage);
end;

function TPkgManager.OnPackageEditorPublishPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoPublishPackage(APackage,[],true);
end;

function TPkgManager.OnPackageEditorRevertPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  if APackage.AutoCreated or (not FilenameIsAbsolute(APackage.Filename))
  or (not FileExists(APackage.Filename)) then
    exit;
  Result:=DoOpenPackageFile(APackage.Filename,[pofRevert]);
end;

function TPkgManager.OnPackageEditorUninstallPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoUninstallPackage(APackage,[]);
end;

function TPkgManager.OnPackageEditorOpenPkgFile(Sender: TObject;
  PkgFile: TPkgFile): TModalResult;
begin
  Result:=DoOpenPkgFile(PkgFile);
end;

procedure TPkgManager.OnPackageEditorFreeEditor(APackage: TLazPackage);
begin
  APackage.Editor:=nil;
  PackageGraph.ClosePackage(APackage);
end;

procedure TPkgManager.OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
  const AFilename: string; var TheUnitName: string; var HasRegisterProc: boolean
  );
begin
  DoGetUnitRegisterInfo(AFilename,TheUnitName,HasRegisterProc,true);
end;

procedure TPkgManager.OnPackageEditorImExportCompilerOptions(Sender: TObject);
begin
  MainIDE.DoImExportCompilerOptions(Sender);
end;

function TPkgManager.OnPackageEditorOpenPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.OnPackageEditorSavePackage(Sender: TObject;
  APackage: TLazPackage; SaveAs: boolean): TModalResult;
begin
  if SaveAs then
    Result:=DoSavePackage(APackage,[psfSaveAs])
  else
    Result:=DoSavePackage(APackage,[]);
end;

function TPkgManager.OnPackageEditorViewPkgSourcePackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackageSource(APackage);
end;

procedure TPkgManager.PackageGraphBeginUpdate(Sender: TObject);
begin
  if PackageGraphExplorer<>nil then PackageGraphExplorer.BeginUpdate;
end;

procedure TPkgManager.PackageGraphChangePackageName(APackage: TLazPackage;
  const OldName: string);
begin
  if PackageGraphExplorer<>nil then
    PackageGraphExplorer.UpdatePackageName(APackage,OldName);
end;

procedure TPkgManager.PackageGraphDeletePackage(APackage: TLazPackage);
begin
  if APackage.Editor<>nil then begin
    APackage.Editor.Hide;
    APackage.Editor.Free;
  end;
end;

procedure TPkgManager.PackageGraphDependencyModified(ADependency: TPkgDependency
  );
var
  DepOwner: TObject;
begin
  DepOwner:=ADependency.Owner;
  if DepOwner is TLazPackage then
    TLazPackage(DepOwner).Modified:=true
  else if DepOwner is TProject then
    TProject(DepOwner).Modified:=true;
end;

function TPkgManager.PackageGraphExplorerOpenPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.PackageGraphExplorerOpenProject(Sender: TObject;
  AProject: TProject): TModalResult;
begin
  if AProject<>Project1 then exit;
  Result:=MainIDE.DoShowProjectInspector;
end;

procedure TPkgManager.PackageGraphAddPackage(Pkg: TLazPackage);
begin
  if FileExists(Pkg.FileName) then PkgLinks.AddUserLink(Pkg);
  if PackageGraphExplorer<>nil then
    PackageGraphExplorer.UpdatePackageAdded(Pkg);
end;

procedure TPkgManager.PackageGraphEndUpdate(Sender: TObject;
  GraphChanged: boolean);
begin
  if GraphChanged then IncreaseCompilerGraphStamp;
  if PackageGraphExplorer<>nil then begin
    if GraphChanged then PackageGraphExplorer.UpdateAll;
    PackageGraphExplorer.EndUpdate;
  end;
  if GraphChanged then begin
    if PackageEditors<>nil then
      PackageEditors.UpdateAllEditors;
    if ProjInspector<>nil then
      ProjInspector.UpdateItems;
  end;
end;

procedure TPkgManager.PackageGraphFindFPCUnit(const UnitName,
  Directory: string; var Filename: string);
begin
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseGDBException(Directory);
  //DebugLn('TPkgManager.PackageGraphFindFPCUnit "',Directory,'"');
  Filename:=CodeToolBoss.DirectoryCachePool.FindUnitInUnitLinks(Directory,
                                                                UnitName);
end;

function TPkgManager.PackageGraphExplorerUninstallPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoUninstallPackage(APackage,[]);
end;

function TPkgManager.PkgLinksDependencyOwnerGetPkgFilename(
  PkgLinks: TPackageLinks; Dependency: TPkgDependency): boolean;
begin
  Result:=false;
  // TODO search in Project/Package history list of dependencies
  
end;

procedure TPkgManager.MainIDEitmConfigCustomCompsClicked(Sender: TObject);
begin
  ShowConfigureCustomComponents;
end;

procedure TPkgManager.MainIDEitmPkgOpenPackageClicked(Sender: TObject);
begin
  DoShowOpenInstalledPckDlg;
end;

procedure TPkgManager.MainIDEitmOpenRecentPackageClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentPackagesMenu;
    MainIDE.SaveEnvironment;
  end;

var
  AFilename: string;
begin
  AFileName:=ExpandFilename((Sender as TIDEMenuItem).Caption);
  if DoOpenPackageFile(AFilename,[pofAddToRecent])=mrOk then begin
    UpdateEnvironment;
  end else begin
    // open failed
    if not FileExists(AFilename) then begin
      // file does not exist -> delete it from recent file list
      RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
      UpdateEnvironment;
    end;
  end;
end;

procedure TPkgManager.OnApplicationIdle(Sender: TObject);
begin
  if (Screen.ActiveCustomForm<>nil)
  and (fsModal in Screen.ActiveCustomForm.FormState) then exit;
  if PackageGraph = nil then Exit;
  PackageGraph.CloseUnneededPackages;
end;

function TPkgManager.DoShowSavePackageAsDialog(
  APackage: TLazPackage): TModalResult;
var
  OldPkgFilename: String;
  SaveDialog: TSaveDialog;
  NewFileName: String;
  NewPkgName: String;
  ConflictPkg: TLazPackage;
  PkgFile: TPkgFile;
  LowerFilename: String;
  BrokenDependencies: TList;
  RenameDependencies: Boolean;
  OldPkgName: String;
  
  procedure RenamePackageInProject;
  var
    AProject: TProject;
    OldUnitName: String;
    NewUnitName: String;
  begin
    AProject:=Project1;
    if (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags)
    and (AProject.MainUnitInfo<>nil) then begin
      OldUnitName:=OldPkgName;
      NewUnitName:=APackage.Name;
      if (OldUnitName<>NewUnitName) then begin
        MainIDEInterface.SaveSourceEditorChangesToCodeCache(-1);
        if CodeToolBoss.RenameUsedUnit(
          AProject.MainUnitInfo.Source,OldUnitName,NewUnitName,'')
        then
          AProject.MainUnitInfo.Modified:=true;
      end;
    end;
  end;
  
begin
  OldPkgFilename:=APackage.Filename;
  OldPkgName:=APackage.Name;

  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=Format(lisPkgMangSavePackageLpk, [APackage.IDAsString]);
    if APackage.HasDirectory then
      SaveDialog.InitialDir:=APackage.Directory;

    // build a nice package filename suggestion
    NewFileName:=APackage.Name+'.lpk';
    SaveDialog.FileName:=NewFileName;

    repeat
      Result:=mrCancel;

      if not SaveDialog.Execute then begin
        // user cancels
        Result:=mrCancel;
        exit;
      end;
      NewFileName:=CleanAndExpandFilename(SaveDialog.Filename);
      NewPkgName:=ExtractFileNameOnly(NewFilename);
      
      // check file extension
      if ExtractFileExt(NewFilename)='' then begin
        // append extension
        NewFileName:=NewFileName+'.lpk';
      end else if ExtractFileExt(NewFilename)<>'.lpk' then begin
        Result:=MessageDlg(lisPkgMangInvalidPackageFileExtension,
          lisPkgMangPackagesMustHaveTheExtensionLpk,
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;

      // check filename
      if (NewPkgName='') or (not IsValidIdent(NewPkgName)) then begin
        Result:=MessageDlg(lisPkgMangInvalidPackageName,
          Format(lisPkgMangThePackageNameIsNotAValidPackageNamePleaseChooseAn, [
            '"', NewPkgName, '"', #13]),
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;

      // apply naming conventions
      
      if lowercase(NewPkgName) <> NewPkgName then
      begin
        LowerFilename:=ExtractFilePath(NewFilename)+lowercase(ExtractFileName(NewFilename));
        if EnvironmentOptions.CharcaseFileAction = ccfaAsk then
        begin
          if MessageDlg(lisPkgMangRenameFileLowercase,
            Format(lisPkgMangShouldTheFileRenamedLowercaseTo, [#13, '"',
              LowerFilename, '"']),
            mtConfirmation,[mbYes,mbNo],0)=mrYes
          then
            NewFileName:=LowerFilename;
        end
        else
        begin
          if EnvironmentOptions.CharcaseFileAction = ccfaAutoRename then NewFileName:=LowerFilename;
        end;
      end;

      // check package name conflict
      ConflictPkg:=PackageGraph.FindAPackageWithName(NewPkgName,APackage);
      if ConflictPkg<>nil then begin
        Result:=MessageDlg(lisPkgMangPackageNameAlreadyExists,
          Format(lisPkgMangThereIsAlreadyAnotherPackageWithTheName, ['"',
            NewPkgName, '"', #13, '"', ConflictPkg.IDAsString, '"', #13, '"',
            ConflictPkg.Filename, '"']),
          mtInformation,[mbRetry,mbAbort,mbIgnore],0);
        if Result=mrAbort then exit;
        if Result<>mrIgnore then continue; // try again
      end;
      
      // check file name conflict with project
      if Project1.ProjectUnitWithFilename(NewFilename)<>nil then begin
        Result:=MessageDlg(lisPkgMangFilenameIsUsedByProject,
          Format(lisPkgMangTheFileNameIsPartOfTheCurrentProject, ['"',
            NewFilename, '"', #13]),
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;
      
      // check file name conflicts with files in other packages
      PkgFile:=PackageGraph.FindFileInAllPackages(NewFilename,true,true,false);
      if PkgFile<>nil then begin
        Result:=MessageDlg(lisPkgMangFilenameIsUsedByOtherPackage,
          Format(lisPkgMangTheFileNameIsUsedByThePackageInFile, ['"',
            NewFilename, '"', #13, '"', PkgFile.LazPackage.IDAsString, '"',
            #13, '"', PkgFile.LazPackage.Filename, '"']),
          mtWarning,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;
      
      // check for broken dependencies
      BrokenDependencies:=PackageGraph.GetBrokenDependenciesWhenChangingPkgID(
        APackage,NewPkgName,APackage.Version);
      RenameDependencies:=false;
      try
        if BrokenDependencies.Count>0 then begin
          Result:=ShowBrokenDependencies(BrokenDependencies,
                                         DefaultBrokenDepButtons);
          if Result=mrAbort then exit;
          if Result=mrRetry then continue;
          if Result=mrYes then RenameDependencies:=true;
        end;
      finally
        BrokenDependencies.Free;
      end;
      
      // check existing file
      if (CompareFilenames(NewFileName,OldPkgFilename)<>0)
      and FileExists(NewFileName) then begin
        Result:=MessageDlg(lisPkgMangReplaceFile,
          Format(lisPkgMangReplaceExistingFile, ['"', NewFilename, '"']),
          mtConfirmation,[mbOk,mbCancel],0);
        if Result<>mrOk then exit;
      end;
      
      // check if new file is read/writable
      Result:=CheckCreatingFile(NewFileName,true);
      if Result=mrAbort then exit;

    until Result<>mrRetry;
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;
  
  // set filename
  APackage.Filename:=NewFilename;
  
  // rename package
  PackageGraph.ChangePackageID(APackage,NewPkgName,APackage.Version,
                               RenameDependencies);
  SaveAutoInstallDependencies(false);
  RenamePackageInProject;

  // clean up old package file to reduce ambiguousities
  if FileExists(OldPkgFilename)
  and (CompareFilenames(OldPkgFilename,NewFilename)<>0) then begin
    if MessageDlg(lisPkgMangDeleteOldPackageFile,
      Format(lisPkgMangDeleteOldPackageFile2, ['"', OldPkgFilename, '"']),
      mtConfirmation,[mbOk,mbCancel],0)=mrOk
    then begin
      if DeleteFile(OldPkgFilename) then begin
        RemoveFromRecentList(OldPkgFilename,
                             EnvironmentOptions.RecentPackageFiles);
      end else begin
        MessageDlg(lisPkgMangDeleteFailed,
          Format(lisPkgMangUnableToDeleteFile, ['"', OldPkgFilename, '"']),
            mtError, [mbOk], 0);
      end;
    end;
  end;

  // success
  Result:=mrOk;
end;

function TPkgManager.DoWriteMakefile(APackage: TLazPackage): TModalResult;
var
  PathDelimNeedsReplace: Boolean;

  procedure Replace(var s: string; const SearchTxt, ReplaceTxt: string);
  var
    p: LongInt;
  begin
    repeat
      p:=Pos(SearchTxt,s);
      if p<=1 then break;
      s:=copy(s,1,p-1)+ReplaceTxt+copy(s,p+length(SearchTxt),length(s));
    until false;
  end;
  
  function ConvertPIMacrosToMakefileMacros(const s: string): string;
  begin
    Result:=s;
    Replace(Result,'%(','$(');
  end;

  function ConvertLazarusToMakefileSearchPath(const s: string): string;
  begin
    Result:=ConvertPIMacrosToMakefileMacros(s);
    Result:=CreateRelativeSearchPath(TrimSearchPath(Result,''),APackage.Directory);
    Replace(Result,';',' ');
    if PathDelimNeedsReplace then
      Replace(Result,PathDelim,'/');
  end;

  function ConvertLazarusToMakefileDirectory(const s: string): string;
  begin
    Result:=ConvertPIMacrosToMakefileMacros(s);
    Result:=CreateRelativePath(TrimFilename(Result),APackage.Directory);
    if PathDelimNeedsReplace then
      Replace(Result,PathDelim,'/');
    // trim trailing PathDelim, as windows does not like it
    Result:=ChompPathDelim(Result);
  end;
  
  function ConvertLazarusOptionsToMakefileOptions(const s: string): string;
  begin
    Result:=ConvertPIMacrosToMakefileMacros(s);
    if PathDelimNeedsReplace then
      Replace(Result,PathDelim,'/');
  end;

var
  s: String;
  e: string;
  SrcFilename: String;
  MainUnitName: String;
  MakefileFPCFilename: String;
  UnitOutputPath: String;
  UnitPath: String;
  FPCMakeTool: TExternalToolOptions;
  CodeBuffer: TCodeBuffer;
  MainSrcFile: String;
  CustomOptions: String;
begin
  Result:=mrCancel;
  PathDelimNeedsReplace:=PathDelim<>'/';

  SrcFilename:=APackage.GetSrcFilename;
  MainUnitName:=lowercase(ExtractFileNameOnly((SrcFilename)));
  UnitPath:=APackage.CompilerOptions.GetUnitPath(true,
                                                 coptParsedPlatformIndependent);
  UnitOutputPath:=APackage.CompilerOptions.GetUnitOutPath(true,
                                                 coptParsedPlatformIndependent);
  CustomOptions:=APackage.CompilerOptions.GetCustomOptions(
                                                 coptParsedPlatformIndependent);
  if APackage.CompilerOptions.Delphi2Extensions then
    CustomOptions:=CustomOptions+' -S2';
  if APackage.CompilerOptions.UseLineInfoUnit then
    CustomOptions:=CustomOptions+' -gl';
  // TODO: other options

  //DebugLn('TPkgManager.DoWriteMakefile ',APackage.Name,' makefile UnitPath="',UnitPath,'"');
  UnitPath:=ConvertLazarusToMakefileSearchPath(UnitPath);
  // remove path delimiter at the end, or else it will fail on windows
  UnitOutputPath:=ConvertLazarusToMakefileDirectory(
                                                ChompPathDelim(UnitOutputPath));
  MainSrcFile:=CreateRelativePath(SrcFilename,APackage.Directory);
  CustomOptions:=ConvertLazarusOptionsToMakefileOptions(CustomOptions);
  

  e:=LineEnding;
  s:='';
  s:=s+'#   Makefile.fpc for '+APackage.IDAsString+e;
  s:=s+''+e;
  s:=s+'[package]'+e;
  s:=s+'name='+lowercase(APackage.Name)+e;
  s:=s+'version='+APackage.Version.AsString+e;
  s:=s+''+e;
  s:=s+'[compiler]'+e;
  s:=s+'unittargetdir='+UnitOutputPath+e;
  s:=s+'unitdir='+UnitPath+e;
  s:=s+'options='+CustomOptions+e; // ToDo do the other options
  s:=s+''+e;
  s:=s+'[target]'+e;
  s:=s+'units='+MainSrcFile+e;
  //s:=s+'implicitunits=syntextdrawer'+e; // TODO list all unit names
  s:=s+''+e;
  s:=s+'[clean]'+e;
  s:=s+'files=$(wildcard $(COMPILER_UNITTARGETDIR)/*$(OEXT)) \'+e;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*$(PPUEXT)) \'+e;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*$(RSTEXT)) \'+e;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*.compiled) \'+e;
  s:=s+'      $(wildcard *$(OEXT)) $(wildcard *$(PPUEXT)) $(wildcard *$(RSTEXT))'+e;
  s:=s+'[prerules]'+e;
  s:=s+'# LCL Platform'+e;
  s:=s+'ifndef LCL_PLATFORM'+e;
  s:=s+'ifeq ($(OS_TARGET),win32)'+e;
  s:=s+'LCL_PLATFORM=win32'+e;
  s:=s+'else'+e;
  s:=s+'LCL_PLATFORM=gtk'+e;
  s:=s+'endif'+e;
  s:=s+'endif'+e;
  s:=s+'export LCL_PLATFORM'+e;

  s:=s+''+e;
  s:=s+'[rules]'+e;
  s:=s+'.PHONY: cleartarget all'+e;
  s:=s+''+e;
  s:=s+'cleartarget:'+e;
  s:=s+'        -$(DEL) $(COMPILER_UNITTARGETDIR)/'+MainUnitName+'$(PPUEXT)'+e;
  s:=s+''+e;
  s:=s+'all: cleartarget $(COMPILER_UNITTARGETDIR) '+MainUnitName+'$(PPUEXT)'+e;
  
  //DebugLn('TPkgManager.DoWriteMakefile [',s,']');

  MakefileFPCFilename:=AppendPathDelim(APackage.Directory)+'Makefile.fpc';
  
  CodeBuffer:=CodeToolBoss.LoadFile(MakefileFPCFilename,true,true);
  if CodeBuffer=nil then begin
    CodeBuffer:=CodeToolBoss.CreateFile(MakefileFPCFilename);
  end;
  
  if CompareTextIgnoringSpace(CodeBuffer.Source,s,false)=0 then begin
    // Makefile.fpc not changed
    Result:=mrOk;
    exit;
  end;
  CodeBuffer.Source:=s;

  //debugln('TPkgManager.DoWriteMakefile MakefileFPCFilename="',MakefileFPCFilename,'"');
  Result:=MainIDE.DoSaveCodeBufferToFile(CodeBuffer,MakefileFPCFilename,false);
  if Result<>mrOk then exit;
  
  // call fpcmake to create the Makefile
  FPCMakeTool:=TExternalToolOptions.Create;
  try
    FPCMakeTool.Title:='Creating Makefile for package '+APackage.IDAsString;
    FPCMakeTool.WorkingDirectory:=APackage.Directory;
    FPCMakeTool.Filename:=FindFPCTool('fpcmake'+GetExecutableExt,
                                      EnvironmentOptions.CompilerFilename);
    FPCMakeTool.CmdLineParams:='-q -TAll';
    FPCMakeTool.EnvironmentOverrides.Add(
                               'FPCDIR='+EnvironmentOptions.FPCSourceDirectory);

    // clear old errors
    SourceNotebook.ClearErrorLines;

    // compile package
    Result:=LazarusIDE.RunExternalTool(FPCMakeTool);
    if Result<>mrOk then begin
      Result:=MessageDlg('fpcmake failed',
        'Calling '+FPCMakeTool.Filename+' to create Makefile from '
        +MakefileFPCFilename+' failed.',
        mtError,[mbCancel],0);
      exit;
    end;
  finally
    // clean up
    FPCMakeTool.Free;
  end;
  
  Result:=mrOk;
end;

function TPkgManager.CompileRequiredPackages(APackage: TLazPackage;
  FirstDependency: TPkgDependency; Globals: TGlobalCompilerOptions;
  Policies: TPackageUpdatePolicies): TModalResult;
var
  AutoPackages: TList;
  i: Integer;
begin
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CompileRequiredPackages A ');
  {$ENDIF}
  AutoPackages:=PackageGraph.GetAutoCompilationOrder(APackage,FirstDependency,
                                                     Policies);
  if AutoPackages<>nil then begin
    //DebugLn('TPkgManager.CompileRequiredPackages B Count=',IntToStr(AutoPackages.Count));
    try
      i:=0;
      while i<AutoPackages.Count do begin
        Result:=DoCompilePackage(TLazPackage(AutoPackages[i]),
                      [pcfDoNotCompileDependencies,pcfOnlyIfNeeded,
                       pcfDoNotSaveEditorFiles],Globals);
        if Result<>mrOk then exit;
        inc(i);
      end;
    finally
      AutoPackages.Free;
    end;
  end;
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CompileRequiredPackages END ');
  {$ENDIF}
  Result:=mrOk;
end;

function TPkgManager.CheckPackageGraphForCompilation(APackage: TLazPackage;
  FirstDependency: TPkgDependency; const Directory: string): TModalResult;
var
  PathList: TList;
  Dependency: TPkgDependency;
  PkgFile1,PkgFile2: TPkgFile;
  ConflictPkg: TLazPackage;
  s: String;
begin
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CheckPackageGraphForCompilation A');
  {$ENDIF}
  PathList:=nil;
  try
    // check for unsaved packages
    PathList:=PackageGraph.FindUnsavedDependencyPath(APackage,FirstDependency);
    if PathList<>nil then begin
      DoShowPackageGraphPathList(PathList);
      Result:=MessageDlg(lisPkgMangUnsavedPackage,
        lisPkgMangThereIsAnUnsavedPackageInTheRequiredPackages,
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;

    // check for broken dependencies
    PathList:=PackageGraph.FindBrokenDependencyPath(APackage,FirstDependency);
    if PathList<>nil then begin
      if (PathList.Count=1) then begin
        Dependency:=TPkgDependency(PathList[0]);
        if Dependency is TPkgDependency then begin
          // check if project
          if Dependency.Owner is TProject then begin
            MainIDE.DoShowProjectInspector;
            Result:=MessageDlg(lisPkgMangBrokenDependency,
              Format(lisPkgMangTheProjectRequiresThePackageButItWasNotFound, [
                '"', Dependency.AsString, '"', #13]),
              mtError,[mbCancel,mbAbort],0);
            exit;
          end;
        end;
      end;
      DoShowPackageGraphPathList(PathList);
      Result:=MessageDlg(lisPkgMangBrokenDependency,
        lisPkgMangARequiredPackagesWasNotFound,
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;

    // check for circle dependencies
    PathList:=PackageGraph.FindCircleDependencyPath(APackage,FirstDependency);
    if PathList<>nil then begin
      DoShowPackageGraphPathList(PathList);
      Result:=MessageDlg(lisPkgMangCircleInPackageDependencies,
        lisPkgMangThereIsACircleInTheRequiredPackages,
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;

    // check for ambiguous units between packages
    if PackageGraph.FindAmbiguousUnits(APackage,FirstDependency,
      PkgFile1,PkgFile2,ConflictPkg)
    then begin
      if (PkgFile1<>nil) and (PkgFile2<>nil) then begin
        s:=Format(lisPkgMangThereAreTwoUnitsWithTheSameName1From2From, [#13,
          #13, '"', PkgFile1.Filename, '"', PkgFile1.LazPackage.IDAsString,
          #13, '"', PkgFile2.Filename, '"', PkgFile2.LazPackage.IDAsString,
          #13, #13]);
      end else if (PkgFile1<>nil) and (ConflictPkg<>nil) then begin
        s:=Format(lisPkgMangThereIsAUnitWithTheSameNameAsAPackage1From2, [#13,
          #13, '"', PkgFile1.Filename, '"', PkgFile1.LazPackage.IDAsString,
          #13, '"', ConflictPkg.IDAsString, #13, #13]);
      end else
        s:='Internal inconsistency FindAmbiguousUnits: '
          +'Please report this bug and how you got here.'#13;
      Result:=MessageDlg(lisPkgMangAmbiguousUnitsFound, Format(
        lisPkgMangBothPackagesAreConnectedThisMeansEitherOnePackageU, [s]),
          mtError,[mbCancel,mbAbort],0);
      exit;
    end;

    // check for ambiguous units between packages
    if PackageGraph.FindFPCConflictUnit(APackage,FirstDependency,Directory,
      @PackageGraphFindFPCUnit,PkgFile1,ConflictPkg)
    then begin
      if (PkgFile1<>nil) then begin
        s:=Format(lisPkgMangThereIsAFPCUnitWithTheSameNameFrom, [#13, #13, '"',
          PkgFile1.Filename, '"', PkgFile1.LazPackage.IDAsString, #13, #13]);
      end else if (PkgFile1<>nil) and (ConflictPkg<>nil) then begin
        s:=Format(lisPkgMangThereIsAFPCUnitWithTheSameNameAsAPackage, [#13,
          #13, '"', ConflictPkg.IDAsString, #13, #13]);
      end else
        s:='Internal inconsistency FindFPCConflictUnits: '
          +'Please report this bug and how you got here.'#13;
      Result:=MessageDlg(lisPkgMangAmbiguousUnitsFound, s,
          mtError,[mbCancel,mbAbort],0);
      exit;
    end;

  finally
    PathList.Free;
  end;
  
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CheckPackageGraphForCompilation END');
  {$ENDIF}
  Result:=mrOk;
end;

function TPkgManager.DoSavePackageCompiledState(APackage: TLazPackage;
  const CompilerFilename, CompilerParams: string): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  CompilerFileDate: Integer;
begin
  StateFile:=APackage.GetStateFilename;
  try
    CompilerFileDate:=FileAge(CompilerFilename);
    XMLConfig:=TXMLConfig.CreateClean(StateFile);
    try
      XMLConfig.SetValue('Compiler/Value',CompilerFilename);
      XMLConfig.SetValue('Compiler/Date',CompilerFileDate);
      XMLConfig.SetValue('Params/Value',CompilerParams);
      InvalidateFileStateCache;
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
    APackage.LastCompilerFilename:=CompilerFilename;
    APackage.LastCompilerFileDate:=CompilerFileDate;
    APackage.LastCompilerParams:=CompilerParams;
    APackage.StateFileDate:=FileAge(StateFile);
    APackage.Flags:=APackage.Flags+[lpfStateFileLoaded];
  except
    on E: Exception do begin
      Result:=MessageDlg(lisPkgMangErrorWritingFile,
        Format(lisPkgMangUnableToWriteStateFileOfPackageError, ['"', StateFile,
          '"', #13, APackage.IDAsString, #13, E.Message]),
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;

  Result:=MainIDE.DoDeleteAmbiguousFiles(StateFile);
  if Result<>mrOk then exit;
end;

function TPkgManager.DoLoadPackageCompiledState(APackage: TLazPackage;
  IgnoreErrors: boolean): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  StateFileAge: Integer;
begin
  StateFile:=APackage.GetStateFilename;
  if not FileExists(StateFile) then begin
    DebugLn('TPkgManager.DoLoadPackageCompiledState Statefile not found: ',StateFile);
    APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
    Result:=mrOk;
    exit;
  end;

  // read the state file
  StateFileAge:=FileAge(StateFile);
  if (not (lpfStateFileLoaded in APackage.Flags))
  or (APackage.StateFileDate<>StateFileAge) then begin
    APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
    try
      XMLConfig:=TXMLConfig.Create(StateFile);
      try
        APackage.LastCompilerFilename:=XMLConfig.GetValue('Compiler/Value','');
        APackage.LastCompilerFileDate:=XMLConfig.GetValue('Compiler/Date',0);
        APackage.LastCompilerParams:=XMLConfig.GetValue('Params/Value','');
      finally
        XMLConfig.Free;
      end;
      APackage.StateFileDate:=StateFileAge;
    except
      on E: Exception do begin
        if IgnoreErrors then begin
          Result:=mrOk;
        end else begin
          Result:=MessageDlg(lisPkgMangErrorReadingFile,
            Format(lisPkgMangUnableToReadStateFileOfPackageError, ['"',
              StateFile, '"', #13, APackage.IDAsString, #13, E.Message]),
            mtError,[mbCancel,mbAbort],0);
        end;
        exit;
      end;
    end;
    APackage.Flags:=APackage.Flags+[lpfStateFileLoaded];
  end;
  
  Result:=mrOk;
end;

function TPkgManager.DoPreparePackageOutputDirectory(APackage: TLazPackage;
  CleanUp: boolean): TModalResult;
var
  OutputDir: String;
  StateFile: String;
  PkgSrcDir: String;
  i: Integer;
  CurFile: TPkgFile;
  CompiledUnitExt: String;
  FPCVersion, FPCRelease, FPCPatch: integer;
  OutputFileName: String;
begin
  OutputDir:=APackage.GetOutputDirectory;
  StateFile:=APackage.GetStateFilename;
  PkgSrcDir:=ExtractFilePath(APackage.GetSrcFilename);

  // create the output directory
  if not ForceDirectory(OutputDir) then begin
    Result:=MessageDlg(lisPkgMangUnableToCreateDirectory,
      Format(lisPkgMangUnableToCreateOutputDirectoryForPackage, ['"',
        OutputDir, '"', #13, APackage.IDAsString]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // delete old Compile State file
  if FileExists(StateFile) and not DeleteFile(StateFile) then begin
    Result:=MessageDlg(lisPkgMangUnableToDeleteFilename,
      Format(lisPkgMangUnableToDeleteOldStateFileForPackage, ['"', StateFile,
        '"', #13, APackage.IDAsString]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
  
  // create the package src directory
  if not ForceDirectory(PkgSrcDir) then begin
    Result:=MessageDlg(lisPkgMangUnableToCreateDirectory,
      Format(lisPkgMangUnableToCreatePackageSourceDirectoryForPackage, ['"',
        PkgSrcDir, '"', #13, APackage.IDAsString]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  
  // clean up if wanted
  if CleanUp then begin
    CodeToolBoss.GetFPCVersionForDirectory(PkgSrcDir,
                                           FPCVersion,FPCRelease,FPCPatch);
    if FPCPatch=0 then ;
    CompiledUnitExt:=GetDefaultCompiledUnitExt(FPCVersion,FPCRelease);
    for i:=0 to APackage.FileCount-1 do begin
      CurFile:=APackage.Files[i];
      if not (CurFile.FileType in PkgFileUnitTypes) then continue;
      OutputFileName:=AppendPathDelim(OutputDir)+CurFile.UnitName+CompiledUnitExt;
      Result:=DeleteFileInteractive(OutputFileName,[mbIgnore,mbAbort]);
      if Result in [mrCancel,mrAbort] then exit;
    end;
  end;

  Result:=mrOk;
end;

function TPkgManager.CheckIfPackageNeedsCompilation(APackage: TLazPackage;
  const CompilerFilename, CompilerParams, SrcFilename: string): TModalResult;
var
  StateFilename: String;
  StateFileAge: Integer;
  i: Integer;
  CurFile: TPkgFile;
begin
  Result:=mrYes;
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CheckIfPackageNeedsCompilation A ',APackage.IDAsString);
  {$ENDIF}

  // check state file
  StateFilename:=APackage.GetStateFilename;
  Result:=DoLoadPackageCompiledState(APackage,false);
  if Result<>mrOk then exit;
  if not (lpfStateFileLoaded in APackage.Flags) then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  No state file for ',APackage.IDAsString);
    Result:=mrYes;
    exit;
  end;

  StateFileAge:=FileAge(StateFilename);

  // check main source file
  if FileExists(SrcFilename) and (StateFileAge<FileAge(SrcFilename)) then
  begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  SrcFile outdated ',APackage.IDAsString);
    Result:=mrYes;
    exit;
  end;

  // check all required packages
  Result:=DoCheckIfDependenciesNeedCompilation(APackage,StateFileAge);
  if Result<>mrNo then exit;

  Result:=mrYes;

  // check compiler and params
  if CompilerFilename<>APackage.LastCompilerFilename then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  Compiler filename changed for ',APackage.IDAsString);
    DebugLn('  Old="',APackage.LastCompilerFilename,'"');
    DebugLn('  Now="',CompilerFilename,'"');
    exit;
  end;
  if not FileExists(CompilerFilename) then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  Compiler filename not found for ',APackage.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit;
  end;
  if FileAge(CompilerFilename)<>APackage.LastCompilerFileDate then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  Compiler file changed for ',APackage.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit;
  end;
  if CompilerParams<>APackage.LastCompilerParams then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  Compiler params changed for ',APackage.IDAsString);
    DebugLn('  Old="',APackage.LastCompilerParams,'"');
    DebugLn('  Now="',CompilerParams,'"');
    exit;
  end;
  
  // check package files
  if StateFileAge<FileAge(APackage.Filename) then begin
    DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  StateFile older than lpk ',APackage.IDAsString);
    exit;
  end;
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    //writeln('TPkgManager.CheckIfPackageNeedsCompilation  CurFile.Filename="',CurFile.Filename,'" ',FileExists(CurFile.Filename),' ',StateFileAge<FileAge(CurFile.Filename));
    if FileExists(CurFile.Filename)
    and (StateFileAge<FileAge(CurFile.Filename)) then begin
      DebugLn('TPkgManager.CheckIfPackageNeedsCompilation  Src has changed ',APackage.IDAsString,' ',CurFile.Filename);
      exit;
    end;
  end;

  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.CheckIfPackageNeedsCompilation END ',APackage.IDAsString);
  {$ENDIF}
  Result:=mrNo;
end;

function TPkgManager.DoCheckIfDependenciesNeedCompilation(
  DependencyOwner: TObject; StateFileAge: longint): TModalResult;
  
  function GetOwnerID: string;
  begin
    if DependencyOwner is TLazPackageID then
      Result:=TLazPackageID(DependencyOwner).IDAsString
    else if DependencyOwner is TProject then
      Result:=TProject(DependencyOwner).IDAsString
    else
      Result:=dbgsName(DependencyOwner);
  end;
  
var
  Dependency: TPkgDependency;
  RequiredPackage: TLazPackage;
  OtherStateFile: String;
begin
  if DependencyOwner is TLazPackage then
    Dependency:=TLazPackage(DependencyOwner).FirstRequiredDependency
  else if DependencyOwner is TProject then
    Dependency:=TProject(DependencyOwner).FirstRequiredDependency
  else begin
    Result:=mrNo;
    exit;
  end;
    
  while Dependency<>nil do begin
    if (Dependency.LoadPackageResult=lprSuccess) then begin
      RequiredPackage:=Dependency.RequiredPackage;
      // check compile state file of required package
      if not RequiredPackage.AutoCreated then begin
        Result:=DoLoadPackageCompiledState(RequiredPackage,false);
        if Result<>mrOk then exit;
        Result:=mrYes;
        if not (lpfStateFileLoaded in RequiredPackage.Flags) then begin
          DebugLn('TPkgManager.CheckIfDependenciesNeedCompilation  No state file for ',RequiredPackage.IDAsString);
          exit;
        end;
        if StateFileAge<RequiredPackage.StateFileDate then begin
          DebugLn('TPkgManager.CheckIfDependenciesNeedCompilation  Required ',
            RequiredPackage.IDAsString,' State file is newer than ',
            'State file ',GetOwnerID);
          exit;
        end;
      end;
      // check output state file of required package
      if RequiredPackage.OutputStateFile<>'' then begin
        OtherStateFile:=RequiredPackage.OutputStateFile;
        IDEMacros.SubstituteMacros(OtherStateFile);
        if FileExists(OtherStateFile)
        and (FileAge(OtherStateFile)>StateFileAge) then begin
          DebugLn('TPkgManager.CheckIfDependenciesNeedCompilation  Required ',
            RequiredPackage.IDAsString,' OtherState file "',OtherStateFile,'"'
            ,' is newer than State file ',GetOwnerID);
          Result:=mrYes;
          exit;
        end;
      end;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
  Result:=mrNo;
end;

function TPkgManager.CheckAmbiguousPackageUnits(APackage: TLazPackage
  ): TModalResult;
var
  i: Integer;
  CurFile: TPkgFile;
  CurUnitName: String;
  SrcDirs: String;
  PkgDir: String;
  PkgOutputDir: String;
  YesToAll: Boolean;

  function CheckFile(const ShortFilename: string): TModalResult;
  var
    AmbiguousFilename: String;
    SearchFlags: TSearchFileInPathFlags;
  begin
    Result:=mrOk;
    SearchFlags:=[];
    if CompareFilenames(PkgDir,PkgOutputDir)=0 then
      Include(SearchFlags,sffDontSearchInBasePath);
    repeat
      AmbiguousFilename:=SearchFileInPath(ShortFilename,PkgDir,SrcDirs,';',
                                          SearchFlags);
      if (AmbiguousFilename='') then exit;
      if not YesToAll then
        Result:=MessageDlg(lisAmbiguousUnitFound,
          Format(lisTheFileWasFoundInOneOfTheSourceDirectoriesOfThePac, ['"',
            AmbiguousFilename, '"', #13, APackage.IDAsString, #13, #13]),
          mtWarning,[mbYes,mbYesToAll,mbNo,mbAbort],0)
      else
        Result:=mrYesToAll;
      if Result=mrNo then
        Result:=mrOk;
      if Result in [mrYes,mrYesToAll] then begin
        YesToAll:=Result=mrYesToAll;
        if (not DeleteFile(AmbiguousFilename))
        and (MessageDlg(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed,
          ['"', AmbiguousFilename, '"']), mtError, [mbIgnore, mbCancel], 0)
          <>mrIgnore) then
        begin
          Result:=mrCancel;
          exit;
        end;
        Result:=mrOk;
      end else
        break;
    until false;
  end;
  
begin
  Result:=mrOk;
  YesToAll:=False;
  // search in every source directory for compiled versions of the units
  // A source directory is a directory with a used unit and it is not the output
  // directory
  SrcDirs:=APackage.GetSourceDirs(true,true);
  PkgOutputDir:=AppendPathDelim(APackage.GetOutputDirectory);
  SrcDirs:=RemoveSearchPaths(SrcDirs,PkgOutputDir);
  if SrcDirs='' then exit;
  PkgDir:=AppendPathDelim(APackage.Directory);
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    if CurFile.FileType<>pftUnit then continue;
    CurUnitName:=lowercase(CurFile.UnitName);
    if CurUnitName='' then continue;
    Result:=CheckFile(CurUnitName+'.ppu');
    if Result<>mrOk then exit;
    Result:=CheckFile(CurUnitName+'.ppw');
    if Result<>mrOk then exit;
    Result:=CheckFile(CurUnitName+'.ppl');
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

function TPkgManager.MacroFunctionPkgSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.SourceDirectories.CreateSearchPathFromAllFiles;
      Result:=true;
    end else begin
      DebugLn('WARNING: TPkgManager.MacroFunctionPkgSrcPath unknown package id: ',FuncData^.Param);
    end;
    //if AnsiCompareText(APackage.IDAsString,'uni_avglvltree')=0 then begin
    //debugln('TPkgManager.MacroFunctionPkgSrcPath PkgID=',FuncData^.Param,' ',dbgs(APackage<>nil),' FuncData^.Result="',FuncData^.Result,'"');
    {if APackage<>nil then begin
      with APackage.SourceDirectories.CreateFileList do begin
        debugln(Text);
        Free;
      end;
    end;}
    //end;
  end else begin
    DebugLn('WARNING: TPkgManager.MacroFunctionPkgSrcPath invalid package id: ',FuncData^.Param);
  end;
  PkgID.Free;
end;

function TPkgManager.MacroFunctionPkgUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.GetUnitPath(false);
      Result:=true;
    end else begin
      DebugLn('WARNING: TPkgManager.MacroFunctionPkgUnitPath unknown package id: ',FuncData^.Param);
    end;
  end else begin
    DebugLn('WARNING: TPkgManager.MacroFunctionPkgUnitPath invalid package id: ',FuncData^.Param);
  end;
  PkgID.Free;
end;

function TPkgManager.MacroFunctionPkgIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.GetIncludePath(false);
      Result:=true;
    end;
  end;
  PkgID.Free;
end;

function TPkgManager.DoGetUnitRegisterInfo(const AFilename: string;
  var TheUnitName: string; var HasRegisterProc: boolean; IgnoreErrors: boolean
  ): TModalResult;
  
  function ErrorsHandled: boolean;
  begin
    if (CodeToolBoss.ErrorMessage='') or IgnoreErrors then exit;
    MainIDE.DoJumpToCodeToolBossError;
    Result:=false;
  end;
  
var
  ExpFilename: String;
  CodeBuffer: TCodeBuffer;
begin
  Result:=mrCancel;
  ExpFilename:=CleanAndExpandFilename(AFilename);
  // create default values
  TheUnitName:='';
  HasRegisterProc:=false;
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);
  CodeBuffer:=CodeToolBoss.LoadFile(ExpFilename,true,false);
  if CodeBuffer<>nil then begin
    TheUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
    if not ErrorsHandled then exit;
    CodeToolBoss.HasInterfaceRegisterProc(CodeBuffer,HasRegisterProc);
    if not ErrorsHandled then exit;
  end;
  if TheUnitName='' then
    TheUnitName:=ExtractFileNameOnly(ExpFilename);
  Result:=mrOk;
end;

procedure TPkgManager.SaveAutoInstallDependencies(
  SetWithStaticPcksFlagForIDE: boolean);
var
  Dependency: TPkgDependency;
  sl: TStringList;
begin
  if SetWithStaticPcksFlagForIDE then begin
    MiscellaneousOptions.BuildLazOpts.WithStaticPackages:=true;
  end;

  sl:=TStringList.Create;
  Dependency:=FirstAutoInstallDependency;
  while Dependency<>nil do begin
    if (Dependency.LoadPackageResult=lprSuccess)
    and (not Dependency.RequiredPackage.AutoCreated)
    and (not Dependency.RequiredPackage.Missing) then begin
      if sl.IndexOf(Dependency.PackageName)<0 then begin
        sl.Add(Dependency.PackageName);
        DebugLn('TPkgManager.SaveAutoInstallDependencies A ',Dependency.PackageName);
      end;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
  MiscellaneousOptions.BuildLazOpts.StaticAutoInstallPackages.Assign(sl);
  MiscellaneousOptions.Save;
  sl.Free;
end;

procedure TPkgManager.LoadStaticBasePackages;
var
  i: Integer;
  BasePackage: TLazPackage;
  Dependency: TPkgDependency;
begin
  // create static base packages
  PackageGraph.AddStaticBasePackages;

  // add them to auto install list
  for i:=0 to PackageGraph.LazarusBasePackages.Count-1 do begin
    BasePackage:=TLazPackage(PackageGraph.LazarusBasePackages[i]);
    Dependency:=BasePackage.CreateDependencyWithOwner(Self);
    PackageGraph.OpenDependency(Dependency);
    Dependency.AddToList(FirstAutoInstallDependency,pdlRequires)
  end;
  SortAutoInstallDependencies;

  // register them
  PackageGraph.RegisterStaticBasePackages;
end;

procedure TPkgManager.LoadStaticCustomPackages;
var
  StaticPackages: TList;
  StaticPackage: PRegisteredPackage;
  i: Integer;
  APackage: TLazPackage;
begin
  StaticPackages:=LazarusPackageIntf.RegisteredPackages;
  if StaticPackages=nil then exit;
  for i:=0 to StaticPackages.Count-1 do begin
    StaticPackage:=PRegisteredPackage(StaticPackages[i]);
    
    // check package name
    if (StaticPackage^.Name='') or (not IsValidIdent(StaticPackage^.Name))
    then begin
      DebugLn('TPkgManager.LoadStaticCustomPackages Invalid Package Name: "',
        BinaryStrToText(StaticPackage^.Name),'"');
      continue;
    end;
    
    // check register procedure
    if (StaticPackage^.RegisterProc=nil) then begin
      DebugLn('TPkgManager.LoadStaticCustomPackages',
        ' Package "',StaticPackage^.Name,'" has no register procedure.');
      continue;
    end;
    
    // load package
    APackage:=LoadInstalledPackage(StaticPackage^.Name,
                                   {$IFDEF BigIDE}True{$ELSE}False{$ENDIF});
    
    // register
    PackageGraph.RegisterStaticPackage(APackage,StaticPackage^.RegisterProc);
  end;
  SortAutoInstallDependencies;
  ClearRegisteredPackages;
end;

function TPkgManager.LoadInstalledPackage(const PackageName: string;
  AddToAutoInstall: boolean): TLazPackage;
var
  NewDependency: TPkgDependency;
  PackageList: TStringList;
begin
  //DebugLn('TPkgManager.LoadInstalledPackage PackageName="',PackageName,'"');
  NewDependency:=TPkgDependency.Create;
  NewDependency.Owner:=Self;
  NewDependency.PackageName:=PackageName;
  PackageGraph.OpenInstalledDependency(NewDependency,pitStatic);
  Result:=NewDependency.RequiredPackage;
  if AddToAutoInstall and (Result<>nil) then begin
    NewDependency.AddToList(FirstAutoInstallDependency,pdlRequires);
    PackageList:=MiscellaneousOptions.BuildLazOpts.StaticAutoInstallPackages;
    if PackageList.IndexOf(NewDependency.PackageName)<0 then
      PackageList.Add(NewDependency.PackageName);
    NewDependency.RequiredPackage.AutoInstall:=pitStatic;
  end else begin
    NewDependency.Free;
  end;
end;

procedure TPkgManager.LoadAutoInstallPackages;
var
  PkgList: TStringList;
  i: Integer;
  PackageName: string;
  Dependency: TPkgDependency;
begin
  PkgList:=MiscellaneousOptions.BuildLazOpts.StaticAutoInstallPackages;
  
  for i:=0 to PkgList.Count-1 do begin
    PackageName:=PkgList[i];
    if (PackageName='') or (not IsValidIdent(PackageName)) then continue;
    Dependency:=FindDependencyByNameInList(FirstAutoInstallDependency,
                                           pdlRequires,PackageName);
    //DebugLn('TPkgManager.LoadAutoInstallPackages ',dbgs(Dependency),' ',PackageName);
    if Dependency<>nil then continue;
    Dependency:=TPkgDependency.Create;
    Dependency.Owner:=Self;
    Dependency.PackageName:=PackageName;
    Dependency.AddToList(FirstAutoInstallDependency,pdlRequires);
    if PackageGraph.OpenDependency(Dependency)<>lprSuccess then begin
      MessageDlg(lisPkgMangUnableToLoadPackage,
        Format(lisPkgMangUnableToOpenThePackage, ['"', PackageName, '"', #13]),
        mtWarning,[mbOk],0);
      continue;
    end;
    if not Dependency.RequiredPackage.Missing then
      Dependency.RequiredPackage.AutoInstall:=pitStatic;
  end;
  SortAutoInstallDependencies;
end;

procedure TPkgManager.SortAutoInstallDependencies;
begin
  // sort install dependencies, so that lower packages come first
  PackageGraph.SortDependencyListTopologically(FirstAutoInstallDependency,
                                               false);
end;

procedure TPkgManager.AddUnitToProjectMainUsesSection(AProject: TProject;
  const AnUnitName, AnUnitInFilename: string);
begin
  // add unit to project main source file
  if (pfMainUnitHasUsesSectionForAllUnits in AProject.Flags)
  and (AProject.MainUnitInfo<>nil) then begin
    debugln('TPkgManager.AddUnitToProjectMainUsesSection B ',AnUnitName);
    if (AnUnitName<>'') then begin
      MainIDEInterface.SaveSourceEditorChangesToCodeCache(-1);
      if CodeToolBoss.AddUnitToMainUsesSection(
        AProject.MainUnitInfo.Source,AnUnitName,AnUnitInFilename)
      then
        AProject.MainUnitInfo.Modified:=true;
    end;
  end;
end;

constructor TPkgManager.Create(TheOwner: TComponent);
var
  CompPalette: TComponentPalette;
begin
  inherited Create(TheOwner);
  OnGetDependencyOwnerDescription:=@GetDependencyOwnerDescription;
  OnGetDependencyOwnerDirectory:=@GetDependencyOwnerDirectory;
  OnGetWritablePkgOutputDirectory:=@GetWritablePkgOutputDirectory;

  // componentpalette
  IDEComponentPalette:=TComponentPalette.Create;
  CompPalette:=TComponentPalette(IDEComponentPalette);
  if CompPalette=nil then ;
  CompPalette.OnEndUpdate:=@IDEComponentPaletteEndUpdate;
  CompPalette.OnOpenPackage:=@IDEComponentPaletteOpenPackage;
  CompPalette.OnOpenUnit:=@IDEComponentPaletteOpenUnit;

  // package links
  PkgLinks:=TPackageLinks.Create;
  PkgLinks.UpdateAll;
  PkgLinks.DependencyOwnerGetPkgFilename:=@PkgLinksDependencyOwnerGetPkgFilename;

  // package graph
  PackageGraph:=TLazPackageGraph.Create;
  PackageGraph.OnChangePackageName:=@PackageGraphChangePackageName;
  PackageGraph.OnAddPackage:=@PackageGraphAddPackage;
  PackageGraph.OnDeletePackage:=@PackageGraphDeletePackage;
  PackageGraph.OnDependencyModified:=@PackageGraphDependencyModified;
  PackageGraph.OnBeginUpdate:=@PackageGraphBeginUpdate;
  PackageGraph.OnEndUpdate:=@PackageGraphEndUpdate;

  // package editors
  PackageEditors:=TPackageEditors.Create;
  PackageEditors.OnOpenFile:=@MainIDE.DoOpenMacroFile;
  PackageEditors.OnOpenPkgFile:=@OnPackageEditorOpenPkgFile;
  PackageEditors.OnOpenPackage:=@OnPackageEditorOpenPackage;
  PackageEditors.OnCreateNewFile:=@OnPackageEditorCreateFile;
  PackageEditors.OnGetIDEFileInfo:=@MainIDE.GetIDEFileState;
  PackageEditors.OnGetUnitRegisterInfo:=@OnPackageEditorGetUnitRegisterInfo;
  PackageEditors.OnFreeEditor:=@OnPackageEditorFreeEditor;
  PackageEditors.OnSavePackage:=@OnPackageEditorSavePackage;
  PackageEditors.OnRevertPackage:=@OnPackageEditorRevertPackage;
  PackageEditors.OnPublishPackage:=@OnPackageEditorPublishPackage;
  PackageEditors.OnCompilePackage:=@OnPackageEditorCompilePackage;
  PackageEditors.OnAddToProject:=@OnPackageEditorAddToProject;
  PackageEditors.OnInstallPackage:=@OnPackageEditorInstallPackage;
  PackageEditors.OnUninstallPackage:=@OnPackageEditorUninstallPackage;
  PackageEditors.OnViewPackageSource:=@OnPackageEditorViewPkgSourcePackage;
  PackageEditors.OnDeleteAmbiguousFiles:=@OnPackageEditorDeleteAmbiguousFiles;
  PackageEditors.OnImExportCompilerOptions:=@OnPackageEditorImExportCompilerOptions;
  PackageEditors.OnCreateMakefile:=@OnPackageEditorCreateMakefile;

  // package macros
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGSRCPATH',nil,@MacroFunctionPkgSrcPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGUNITPATH',nil,@MacroFunctionPkgUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGINCPATH',nil,@MacroFunctionPkgIncPath);
    
  LazPackageDescriptors:=TLazPackageDescriptors.Create;
  LazPackageDescriptors.AddDefaultPackageDescriptors;

  // idle handler
  Application.AddOnIdleHandler(@OnApplicationIdle,true);
end;

destructor TPkgManager.Destroy;
var
  Dependency: TPkgDependency;
begin
  FreeThenNil(LazPackageDescriptors);
  while FirstAutoInstallDependency<>nil do begin
    Dependency:=FirstAutoInstallDependency;
    Dependency.RequiredPackage:=nil;
    Dependency.RemoveFromList(FirstAutoInstallDependency,pdlRequires);
    Dependency.Free;
  end;
  FreeThenNil(PackageGraphExplorer);
  FreeThenNil(PackageEditors);
  FreeThenNil(PackageGraph);
  FreeThenNil(PkgLinks);
  FreeThenNil(IDEComponentPalette);
  FreeThenNil(PackageDependencies);
  inherited Destroy;
end;

procedure TPkgManager.ConnectMainBarEvents;
begin
  with MainIDEBar do begin
    itmPkgOpenPackage.OnClick :=@MainIDEitmPkgOpenPackageClicked;
    itmPkgOpenPackageFile.OnClick:=@MainIDEitmPkgOpenPackageFileClick;
    itmPkgOpenPackageOfCurUnit.OnClick :=@MainIDEitmPkgOpenPackageOfCurUnitClicked;
    itmPkgAddCurUnitToPkg.OnClick:=@MainIDEitmPkgAddCurUnitToPkgClick;
    itmPkgPkgGraph.OnClick:=@MainIDEitmPkgPkgGraphClick;
    itmPkgEditInstallPkgs.OnClick:=@MainIDEitmPkgEditInstallPkgsClick;
    {$IFDEF CustomIDEComps}
    itmCompsConfigCustomComps.OnClick :=@MainIDEitmConfigCustomCompsClicked;
    {$ENDIF}
  end;
  
  SetRecentPackagesMenu;
end;

procedure TPkgManager.ConnectSourceNotebookEvents;
begin

end;

procedure TPkgManager.SetupMainBarShortCuts;
begin

end;

procedure TPkgManager.SetRecentPackagesMenu;
begin
  MainIDE.SetRecentSubMenu(itmPkgOpenRecent,
     EnvironmentOptions.RecentPackageFiles,@MainIDEitmOpenRecentPackageClicked);
end;

procedure TPkgManager.AddFileToRecentPackages(const Filename: string);
begin
  AddToRecentList(Filename,EnvironmentOptions.RecentPackageFiles,
                  EnvironmentOptions.MaxRecentPackageFiles);
  SetRecentPackagesMenu;
  MainIDE.SaveEnvironment;
end;

procedure TPkgManager.SaveSettings;
begin
  PackageEditors.SaveLayouts;
end;

function TPkgManager.GetDefaultSaveDirectoryForFile(const Filename: string
  ): string;
var
  APackage: TLazPackage;
  PkgFile: TPkgFile;
begin
  Result:='';
  PkgFile:=PackageGraph.FindFileInAllPackages(Filename,false,true,true);
  if PkgFile=nil then exit;
  APackage:=PkgFile.LazPackage;
  if APackage.AutoCreated or (not APackage.HasDirectory) then exit;
  Result:=APackage.Directory;
end;

function TPkgManager.GetPublishPackageDir(APackage: TLazPackage): string;
begin
  Result:=APackage.PublishOptions.DestinationDirectory;
  if IDEMacros.SubstituteMacros(Result) then begin
    if FilenameIsAbsolute(Result) then begin
      Result:=AppendPathDelim(TrimFilename(Result));
    end else begin
      Result:='';
    end;
  end else begin
    Result:='';
  end;
end;

procedure TPkgManager.LoadInstalledPackages;
begin
  IDEComponentPalette.BeginUpdate(true);
  try
    LoadStaticBasePackages;
    LoadStaticCustomPackages;

    LoadAutoInstallPackages;
  finally
    IDEComponentPalette.EndUpdate;
  end;
end;

procedure TPkgManager.UnloadInstalledPackages;
var
  Dependency: TPkgDependency;
begin
  // break and free auto installed packages
  while FirstAutoInstallDependency<>nil do begin
    Dependency:=FirstAutoInstallDependency;
    Dependency.RequiredPackage:=nil;
    Dependency.RemoveFromList(FirstAutoInstallDependency,pdlRequires);
    Dependency.Free;
  end;
end;

procedure TPkgManager.UpdateVisibleComponentPalette;
begin
  TComponentPalette(IDEComponentPalette).NoteBook:=MainIDEBar.ComponentNotebook;
  TComponentPalette(IDEComponentPalette).UpdateNoteBookButtons;
end;

procedure TPkgManager.ProcessCommand(Command: word; var Handled: boolean);
begin
  Handled:=true;
  case Command of
  ecOpenPackage: MainIDEitmPkgOpenPackageClicked(Self);
  ecOpenPackageFile: MainIDEitmPkgOpenPackageFileClick(Self);
  ecOpenPackageOfCurUnit: MainIDEitmPkgOpenPackageOfCurUnitClicked(Self);
  ecAddCurUnitToPkg: MainIDEitmPkgAddCurUnitToPkgClick(Self);
  ecPackageGraph: MainIDEitmPkgPkgGraphClick(Self);
  ecEditInstallPkgs: MainIDEitmPkgEditInstallPkgsClick(Self);
  else
    Handled:=false;
  end;
end;

procedure TPkgManager.OnSourceEditorPopupMenu(
  const AddMenuItemProc: TAddMenuItemProc);
var
  PkgFile: TPkgFile;
begin
  PkgFile:=GetPackageOfCurrentSourceEditor;
  //debugln('TPkgManager.OnSourceEditorPopupMenu ',dbgsName(PkgFile));
  if PkgFile<>nil then
    AddMenuItemProc('Open package '+PkgFile.LazPackage.Name,true,
                    @OnOpenPackageForCurrentSrcEditFile);
end;

function TPkgManager.AddPackageToGraph(APackage: TLazPackage;
  Replace: boolean): TModalResult;
var
  ConflictPkg: TLazPackage;
begin
  // check Package Name
  if (APackage.Name='') or (not IsValidIdent(APackage.Name)) then begin
    Result:=MessageDlg(lisPkgMangInvalidPackageName2,
      Format(lisPkgMangThePackageNameOfTheFileIsInvalid, ['"', APackage.Name,
        '"', #13, '"', APackage.Filename, '"']),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // check if Package with same name is already loaded
  ConflictPkg:=PackageGraph.FindAPackageWithName(APackage.Name,nil);
  if ConflictPkg<>nil then begin
    if not PackageGraph.PackageCanBeReplaced(ConflictPkg,APackage) then begin
      Result:=MessageDlg(lisPkgMangPackageConflicts,
        Format(lisPkgMangThereIsAlreadyAPackageLoadedFromFile, ['"',
          ConflictPkg.IDAsString, '"', #13, '"', ConflictPkg.Filename, '"',
          #13, #13]),
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
    
    if ConflictPkg.Modified and (not ConflictPkg.ReadOnly) then begin
      Result:=MessageDlg(lisPkgMangSavePackage,
        Format(lisPkgMangLoadingPackageWillReplacePackage, [
          APackage.IDAsString, ConflictPkg.IDAsString, #13,
          ConflictPkg.Filename, #13, #13, #13, ConflictPkg.Filename]),
        mtConfirmation,[mbYes,mbNo,mbCancel,mbAbort],0);
      if Result=mrNo then Result:=mrOk;
      if Result=mrYes then begin
        Result:=DoSavePackage(ConflictPkg,[]);
      end;
      if Result<>mrOk then exit;
    end;
    
    // replace package
    PackageGraph.ReplacePackage(ConflictPkg,APackage);
  end else begin
    // add to graph
    PackageGraph.AddPackage(APackage);
  end;

  // save package file links
  PkgLinks.SaveUserLinks;

  Result:=mrOk;
end;

function TPkgManager.OpenProjectDependencies(AProject: TProject;
  ReportMissing: boolean): TModalResult;
var
  BrokenDependencies: TList;
begin
  PackageGraph.OpenRequiredDependencyList(AProject.FirstRequiredDependency);
  if ReportMissing then begin
    BrokenDependencies:=PackageGraph.FindAllBrokenDependencies(nil,
                                             AProject.FirstRequiredDependency);
    if BrokenDependencies<>nil then begin
      Result:=ShowBrokenDependenciesReport(BrokenDependencies);
      BrokenDependencies.Free;
    end;
  end else
    Result:=mrOk;
end;

procedure TPkgManager.AddDefaultDependencies(AProject: TProject);
begin
  OpenProjectDependencies(AProject,true);
end;

function TPkgManager.AddProjectDependency(AProject: TProject;
  APackage: TLazPackage; OnlyTestIfPossible: boolean): TModalResult;
var
  NewDependency: TPkgDependency;
begin
  Result:=mrCancel;
  // check if the dependency is already there
  if FindDependencyByNameInList(AProject.FirstRequiredDependency,pdlRequires,
    APackage.Name)<>nil
  then begin
    // package already there
    Result:=mrCancel;
    exit;
  end;
  if OnlyTestIfPossible then
    exit(mrOk);
  // add a dependency for the package to the project
  NewDependency:=APackage.CreateDependencyWithOwner(AProject);
  Result:=AddProjectDependency(AProject,NewDependency);
end;

function TPkgManager.AddProjectDependency(AProject: TProject;
  ADependency: TPkgDependency): TModalResult;
begin
  Result:=mrOk;
  AProject.AddRequiredDependency(ADependency);
  PackageGraph.OpenDependency(ADependency);
  if (ADependency.RequiredPackage<>nil)
  and (not ADependency.RequiredPackage.AutoCreated) then begin
    AddUnitToProjectMainUsesSection(AProject,ADependency.PackageName,'');
  end;
end;

procedure TPkgManager.AddProjectRegCompDependency(AProject: TProject;
  ARegisteredComponent: TRegisteredComponent);
var
  PkgFile: TPkgFile;
begin
  if not (ARegisteredComponent is TPkgComponent) then exit;
  PkgFile:=TPkgComponent(ARegisteredComponent).PkgFile;
  if (PkgFile=nil) or (PkgFile.LazPackage=nil) then exit;
  AddProjectDependency(AProject,PkgFile.LazPackage);
end;

procedure TPkgManager.AddProjectLCLDependency(AProject: TProject);
begin
  AddProjectDependency(AProject,PackageGraph.LCLPackage);
end;

function TPkgManager.CheckProjectHasInstalledPackages(AProject: TProject
  ): TModalResult;
var
  MissingUnits: TList;
  i: Integer;
  PkgFile: TPkgFile;
  Msg: String;
begin
  Result:=mrOk;
  MissingUnits:=PackageGraph.FindNotInstalledRegisterUnits(nil,
                                              AProject.FirstRequiredDependency);
  if MissingUnits<>nil then begin
    Msg:=Format(lisProbablyYouNeedToInstallSomePackagesForBeforeConti, [#13,
      #13, #13, #13, #13, #13, #13, #13, #13]);
    for i:=0 to MissingUnits.Count-1 do begin
      PkgFile:=TPkgFile(MissingUnits[i]);
      Msg:=Msg+' unit '+PkgFile.UnitName
              +' in package '+PkgFile.LazPackage.IDAsString+#13;
    end;
    Result:=MessageDlg(lisPackageNeedsInstallation,
      Msg,mtWarning,[mbIgnore,mbCancel],0);
    if Result<>mrIgnore then
      AProject.AutoOpenDesignerFormsDisabled:=true;
    MissingUnits.Free;
  end;
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
  // create a new package with standard dependencies
  NewPackage:=PackageGraph.CreateNewPackage(lisPkgMangNewPackage);
  PackageGraph.AddDependencyToPackage(NewPackage,
                PackageGraph.FCLPackage.CreateDependencyWithOwner(NewPackage));
  NewPackage.Modified:=false;

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
  // open a package editor
  CurEditor:=PackageEditors.OpenEditor(APackage);
  CurEditor.ShowOnTop;
  Result:=mrOk;
end;

function TPkgManager.DoOpenPackageFile(AFilename: string; Flags: TPkgOpenFlags
  ): TModalResult;
var
  APackage: TLazPackage;
  XMLConfig: TXMLConfig;
  AlternativePkgName: String;
begin
  AFilename:=CleanAndExpandFilename(AFilename);
  
  // check file extension
  if CompareFileExt(AFilename,'.lpk',false)<>0 then begin
    Result:=MessageDlg(lisPkgMangInvalidFileExtension,
      Format(lisPkgMangTheFileIsNotALazarusPackage, ['"', AFilename, '"']),
      mtError,[mbCancel,mbAbort],0);
    RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
    SetRecentPackagesMenu;
    exit;
  end;
  
  // check filename
  AlternativePkgName:=ExtractFileNameOnly(AFilename);
  if (AlternativePkgName='') or (not IsValidIdent(AlternativePkgName)) then
  begin
    Result:=MessageDlg(lisPkgMangInvalidPackageFilename,
      Format(lisPkgMangThePackageFileNameInIsNotAValidLazarusPackageName, ['"',
        AlternativePkgName, '"', #13, '"', AFilename, '"']),
      mtError,[mbCancel,mbAbort],0);
    RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
    SetRecentPackagesMenu;
    exit;
  end;

  // add to recent packages
  if pofAddToRecent in Flags then begin
    AddToRecentList(AFilename,EnvironmentOptions.RecentPackageFiles,
                    EnvironmentOptions.MaxRecentPackageFiles);
    SetRecentPackagesMenu;
  end;

  // check if package is already loaded
  APackage:=PackageGraph.FindPackageWithFilename(AFilename,true);
  if (APackage=nil) or (pofRevert in Flags) then begin
    // package not yet loaded
    
    if not FileExists(AFilename) then begin
      MessageDlg(lisFileNotFound,
        Format(lisPkgMangFileNotFound, ['"', AFilename, '"']),
        mtError,[mbCancel],0);
      RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
      SetRecentPackagesMenu;
      Result:=mrCancel;
      exit;
    end;

    // create a new package
    Result:=mrCancel;
    APackage:=TLazPackage.Create;
    try

      // load the package file
      try
        XMLConfig:=TXMLConfig.Create(AFilename);
        try
          APackage.Filename:=AFilename;
          APackage.LoadFromXMLConfig(XMLConfig,'Package/');
        finally
          XMLConfig.Free;
        end;
      except
        on E: Exception do begin
          Result:=MessageDlg(lisPkgMangErrorReadingPackage,
            Format(lisPkgUnableToReadPackageFileError, ['"', AFilename, '"',
              #13, E.Message]),
            mtError,[mbAbort,mbCancel],0);
          exit;
        end;
      end;

      // newly loaded is not modified
      APackage.Modified:=false;

      // check if package name and file name correspond
      if (SysUtils.CompareText(AlternativePkgName,APackage.Name)<>0) then begin
        Result:=MessageDlg(lisPkgMangFilenameDiffersFromPackagename,
          Format(lisPkgMangTheFilenameDoesNotCorrespondToThePackage, ['"',
            ExtractFileName(AFilename), '"', '"', APackage.Name, '"', #13, '"',
            AlternativePkgName, '"']),
          mtConfirmation,[mbYes,mbCancel,mbAbort],0);
        if Result<>mrYes then exit;
        APackage.Name:=AlternativePkgName;
      end;
      
      // integrate it into the graph
      Result:=AddPackageToGraph(APackage,pofRevert in Flags);
    finally
      if Result<>mrOk then APackage.Free;
    end;
  end;

  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.DoSavePackage(APackage: TLazPackage;
  Flags: TPkgSaveFlags): TModalResult;
var
  XMLConfig: TXMLConfig;
  PkgLink: TPackageLink;
begin
  // do not save during compilation
  if not (MainIDE.ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  
  if APackage.IsVirtual then Include(Flags,psfSaveAs);

  // check if package needs saving
  if (not (psfSaveAs in Flags))
  and (not APackage.ReadOnly) and (not APackage.Modified)
  and FileExists(APackage.Filename) then begin
    Result:=mrOk;
    exit;
  end;

  // ask user if package should be saved
  if psfAskBeforeSaving in Flags then begin
    Result:=MessageDlg(lisPkgMangSavePackage2,
               Format(lisPkgMangPackageChangedSave, ['"', APackage.IDAsString,
                 '"']),
               mtConfirmation,[mbYes,mbNo,mbAbort],0);
    if (Result=mrNo) then Result:=mrIgnore;
    if Result<>mrYes then exit;
  end;
  
  // warn about missing files
  Result:=WarnAboutMissingPackageFiles(APackage);
  if Result<>mrOk then exit;

  // save editor files to codetools
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);

  // save package
  if (psfSaveAs in Flags) then begin
    Result:=DoShowSavePackageAsDialog(APackage);
    if Result<>mrOk then exit;
  end;
  
  // backup old file
  Result:=MainIDE.DoBackupFile(APackage.Filename,true);
  if Result=mrAbort then exit;

  // delete ambiguous files
  Result:=MainIDE.DoDeleteAmbiguousFiles(APackage.Filename);
  if Result=mrAbort then exit;

  // save
  try
    XMLConfig:=TXMLConfig.CreateClean(APackage.Filename);
    try
      XMLConfig.Clear;
      APackage.SaveToXMLConfig(XMLConfig,'Package/');
      InvalidateFileStateCache;
      XMLConfig.Flush;
      PkgLink:=PkgLinks.AddUserLink(APackage);
      if PkgLink<>nil then begin
        PkgLink.FileDate:=FileDateToDateTime(FileAge(APackage.Filename));
        PkgLink.FileDateValid:=true;
        PkgLinks.SaveUserLinks;
      end;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      Result:=MessageDlg(lisPkgMangErrorWritingPackage,
        Format(lisPkgMangUnableToWritePackageToFileError, ['"',
          APackage.IDAsString, '"', #13, '"', APackage.Filename, '"', #13,
          E.Message]),
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;

  // success
  APackage.Modified:=false;
  // add to recent
  if (psfSaveAs in Flags) then begin
    AddFileToRecentPackages(APackage.Filename);
  end;

  if APackage.Editor<>nil then APackage.Editor.UpdateAll;
  Result:=mrOk;
end;

function TPkgManager.DoShowPackageGraph: TModalResult;
begin
  if PackageGraphExplorer=nil then begin
    PackageGraphExplorer:=TPkgGraphExplorerDlg.Create(Application);
    PackageGraphExplorer.OnOpenPackage:=@PackageGraphExplorerOpenPackage;
    PackageGraphExplorer.OnOpenProject:=@PackageGraphExplorerOpenProject;
    PackageGraphExplorer.OnUninstallPackage:=@PackageGraphExplorerUninstallPackage;
  end;
  PackageGraphExplorer.ShowOnTop;
  Result:=mrOk;
end;

function TPkgManager.DoCloseAllPackageEditors: TModalResult;
var
  APackage: TLazPackage;
begin
  while PackageEditors.Count>0 do begin
    APackage:=PackageEditors.Editors[PackageEditors.Count-1].LazPackage;
    Result:=DoClosePackageEditor(APackage);
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

procedure TPkgManager.DoShowPackageGraphPathList(PathList: TList);
begin
  if DoShowPackageGraph<>mrOk then exit;
  PackageGraphExplorer.ShowPath(PathList);
end;

function TPkgManager.ShowBrokenDependenciesReport(Dependencies: TList
  ): TModalResult;
var
  Msg: String;
  i: Integer;
  ADependency: TPkgDependency;
begin
  Result:=mrOk;
  if (Dependencies=nil) or (Dependencies.Count=0) then exit;
  if Dependencies.Count=1 then
    Msg:=lisPkgMangTheFollowingPackageFailedToLoad
  else
    Msg:=lisPkgMangTheFollowingPackagesFailedToLoad;
  Msg:=Msg+#13#13;
  for i:=0 to Dependencies.Count-1 do begin
    ADependency:=TPkgDependency(Dependencies[i]);
    Msg:=Msg+ADependency.AsString+#13;
  end;
  
  // give some hints
  ADependency:=TPkgDependency(Dependencies[0]);
  if (ADependency.Owner is TProject) then begin
    // broken dependency used by project -> show project inspector
    if ADependency.Owner=Project1 then begin
      Result:=MainIDE.DoShowProjectInspector;
      Msg:=Format(lisSeeProjectProjectInspector, [Msg]);
    end;
  end;
  
  Result:=MessageDlg(lisMissingPackages, Msg, mtError, [mbOk], 0);
end;

procedure TPkgManager.RebuildDefineTemplates;
begin
  PackageGraph.RebuildDefineTemplates;
end;

function TPkgManager.DoCompileProjectDependencies(AProject: TProject;
  Flags: TPkgCompileFlags): TModalResult;
begin
  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    Result:=CheckPackageGraphForCompilation(nil,
                                            AProject.FirstRequiredDependency,
                                            AProject.ProjectDirectory);
    if Result<>mrOk then exit;
  end;
  
  // save all open files
  if not (pcfDoNotSaveEditorFiles in Flags) then begin
    Result:=MainIDE.DoSaveForBuild;
    if Result<>mrOk then exit;
  end;

  PackageGraph.BeginUpdate(false);
  try
    // automatically compile required packages
    if not (pcfDoNotCompileDependencies in Flags) then begin
      Result:=CompileRequiredPackages(nil,AProject.FirstRequiredDependency,
                                      AProject.CompilerOptions.Globals,
                                      [pupAsNeeded]);
      if Result<>mrOk then exit;
    end;
  finally
    PackageGraph.EndUpdate;
  end;
  
  Result:=mrOk;
end;

function TPkgManager.DoCompilePackage(APackage: TLazPackage;
  Flags: TPkgCompileFlags; Globals: TGlobalCompilerOptions): TModalResult;
var
  PkgCompileTool: TExternalToolOptions;
  CompilerFilename: String;
  CompilerParams: String;
  EffektiveCompilerParams: String;
  SrcFilename: String;
  CompilePolicies: TPackageUpdatePolicies;
begin
  Result:=mrCancel;
  
  DebugLn('TPkgManager.DoCompilePackage A ',APackage.IDAsString,' Flags=',PkgCompileFlagsToString(Flags));
  
  if APackage.AutoCreated then exit;

  Result:=MainIDE.PrepareForCompile;
  if Result<>mrOk then exit;

  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    Result:=CheckPackageGraphForCompilation(APackage,nil,APackage.Directory);
    if Result<>mrOk then exit;
  end;
  
  // save all open files
  {$IFDEF VerboseSaveForBuild}
  DebugLn('TPkgManager.DoCompilePackage  ',APackage.IDAsString,' Flags=',PkgCompileFlagsToString(Flags));
  {$ENDIF}
  if not (pcfDoNotSaveEditorFiles in Flags) then begin
    Result:=MainIDE.DoSaveForBuild;
    if Result<>mrOk then exit;
  end;
  
  Result:=WarnAboutMissingPackageFiles(APackage);
  if Result<>mrOk then exit;

  PackageGraph.BeginUpdate(false);
  try
    // automatically compile required packages
    if not (pcfDoNotCompileDependencies in Flags) then begin
      CompilePolicies:=[pupAsNeeded];
      if pcfCompileDependenciesClean in Flags then
        Include(CompilePolicies,pupOnRebuildingAll);
      Result:=CompileRequiredPackages(APackage,nil,Globals,CompilePolicies);
      if Result<>mrOk then exit;
    end;

    SrcFilename:=APackage.GetSrcFilename;
    CompilerFilename:=APackage.GetCompilerFilename;
    CompilerParams:=APackage.CompilerOptions.MakeOptionsString(Globals,
                               APackage.CompilerOptions.DefaultMakeOptionsFlags)
                    +' '+CreateRelativePath(SrcFilename,APackage.Directory);

    // check if compilation is neccessary
    if (pcfOnlyIfNeeded in Flags) then begin
      Result:=CheckIfPackageNeedsCompilation(APackage,
                                             CompilerFilename,CompilerParams,
                                             SrcFilename);
      if Result=mrNo then begin
        Result:=mrOk;
        exit;
      end;
      if Result<>mrYes then exit;
    end;
    
    // auto increase version
    // ToDo
    
    MessagesView.BeginBlock;
    try
      Result:=DoPreparePackageOutputDirectory(APackage,pcfCleanCompile in Flags);
      if Result<>mrOk then begin
        DebugLn('TPkgManager.DoCompilePackage DoPreparePackageOutputDirectory failed');
        exit;
      end;

      // create package main source file
      Result:=DoSavePackageMainSource(APackage,Flags);
      if Result<>mrOk then begin
        DebugLn('TPkgManager.DoCompilePackage DoSavePackageMainSource failed');
        exit;
      end;

      // check ambiguous units
      Result:=CheckAmbiguousPackageUnits(APackage);
      if Result<>mrOk then begin
        DebugLn('TPkgManager.DoCompilePackage CheckAmbiguousPackageUnits failed');
        exit;
      end;
      
      // create Makefile
      if (pcfCreateMakefile in Flags)
      or (APackage.CompilerOptions.CreateMakefileOnBuild) then begin
        Result:=DoWriteMakefile(APackage);
        if Result<>mrOk then begin
          DebugLn('TPkgManager.DoCompilePackage DoWriteMakefile failed');
          exit;
        end;
      end;

      // run compilation tool 'Before'
      if not (pcfDoNotCompilePackage in Flags) then begin
        Result:=MainIDE.DoExecuteCompilationTool(
          APackage.CompilerOptions.ExecuteBefore,
          APackage.Directory,'Executing command before');
        if Result<>mrOk then exit;
      end;

      // create external tool to run the compiler
      DebugLn('TPkgManager.DoCompilePackage Compiler="',CompilerFilename,'"');
      DebugLn('TPkgManager.DoCompilePackage Params="',CompilerParams,'"');
      DebugLn('TPkgManager.DoCompilePackage WorkingDir="',APackage.Directory,'"');

      if (not APackage.CompilerOptions.SkipCompiler)
      and (not (pcfDoNotCompilePackage in Flags)) then begin
        // check compiler filename
        try
          CheckIfFileIsExecutable(CompilerFilename);
        except
          on e: Exception do begin
            Result:=MessageDlg(lisPkgManginvalidCompilerFilename,
              Format(lisPkgMangTheCompilerFileForPackageIsNotAValidExecutable, [
                APackage.IDAsString, #13, E.Message]),
              mtError,[mbCancel,mbAbort],0);
            exit;
          end;
        end;

        // change compiler parameters for compiling clean
        EffektiveCompilerParams:=CompilerParams;
        if pcfCleanCompile in Flags then begin
          if EffektiveCompilerParams<>'' then
            EffektiveCompilerParams:='-B '+EffektiveCompilerParams
          else
            EffektiveCompilerParams:='-B';
        end;

        PkgCompileTool:=TExternalToolOptions.Create;
        try
          PkgCompileTool.Title:='Compiling package '+APackage.IDAsString;
          PkgCompileTool.ScanOutputForFPCMessages:=true;
          PkgCompileTool.ScanOutputForMakeMessages:=true;
          PkgCompileTool.WorkingDirectory:=APackage.Directory;
          PkgCompileTool.Filename:=CompilerFilename;
          PkgCompileTool.CmdLineParams:=EffektiveCompilerParams;

          // clear old errors
          SourceNotebook.ClearErrorLines;

          // compile package
          Result:=EnvironmentOptions.ExternalTools.Run(PkgCompileTool,
                                MainIDE.MacroList,nil,APackage.CompilerOptions);
          if Result<>mrOk then exit;
          // compilation succeded -> write state file
          Result:=DoSavePackageCompiledState(APackage,
                                             CompilerFilename,CompilerParams);
          if Result<>mrOk then exit;
        finally
          // clean up
          PkgCompileTool.Free;
        end;
      end;

      // run compilation tool 'After'
      if not (pcfDoNotCompilePackage in Flags) then begin
        Result:=MainIDE.DoExecuteCompilationTool(
          APackage.CompilerOptions.ExecuteAfter,
          APackage.Directory,'Executing command after');
        if Result<>mrOk then exit;
      end;
    finally
      MessagesView.EndBlock;

      if Result<>mrOk then begin
        if (APackage.AutoInstall<>pitNope) and (APackage.Installed=pitNope) then
        begin
          // package was tried to install, but failed
          // -> ask user if the package should be removed from the installation
          // list
          if MessageDlg(lisInstallationFailed,
            Format(
              lisPkgMangThePackageFailedToCompileRemoveItFromTheInstallati, [
              '"', APackage.IDAsString, '"', #13]), mtConfirmation,
            [mbYes,mbIgnore],0)=mrYes then
          begin
            DoUninstallPackage(APackage,[puifDoNotConfirm,puifDoNotBuildIDE]);
          end;
        end;
      end;
    end;

  finally
    if not (pcfDoNotSaveEditorFiles in Flags) then begin
      // check for changed files on disk
      MainIDE.DoCheckFilesOnDisk;
    end;

    PackageGraph.EndUpdate;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoSavePackageMainSource(APackage: TLazPackage;
  Flags: TPkgCompileFlags): TModalResult;
var
  SrcFilename: String;
  UsedUnits: String;
  Src: String;
  i: Integer;
  e: String;
  CurFile: TPkgFile;
  CodeBuffer: TCodeBuffer;
  CurUnitName: String;
  RegistrationCode: String;
  HeaderSrc: String;
  OutputDir: String;
  OldShortenSrc: String;
  NeedsRegisterProcCall: boolean;
  CurSrcUnitName: String;
  NewShortenSrc: String;
begin
  {$IFDEF VerbosePkgCompile}
  writeln('TPkgManager.DoSavePackageMainSource A');
  {$ENDIF}
  // check if package is ready for saving
  OutputDir:=APackage.GetOutputDirectory;
  if not DirPathExists(OutputDir) then begin
    Result:=MessageDlg(lisEnvOptDlgDirectoryNotFound,
      Format(lisPkgMangPackageHasNoValidOutputDirectory, ['"',
        APackage.IDAsString, '"', #13, '"', OutputDir, '"']),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  SrcFilename:=APackage.GetSrcFilename;

  // delete ambiguous files
  Result:=MainIDE.DoDeleteAmbiguousFiles(SrcFilename);
  if Result=mrAbort then begin
    DebugLn('TPkgManager.DoSavePackageMainSource DoDeleteAmbiguousFiles failed');
    exit;
  end;

  // collect unitnames
  e:=LineEnding;
  UsedUnits:='';
  RegistrationCode:='';
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    // update unitname
    if FilenameIsPascalUnit(CurFile.Filename)
    and (CurFile.FileType in PkgFileUnitTypes) then begin
      CurUnitName:=ExtractFileNameOnly(CurFile.Filename);
      
      if CurUnitName=lowercase(CurUnitName) then begin
        // the filename is all lowercase, so we can use the nicer unitname from
        // the source.

        CodeBuffer:=CodeToolBoss.LoadFile(CurFile.Filename,false,false);
        if CodeBuffer<>nil then begin
          // if the unit is edited, the unitname is probably already cached
          CurSrcUnitName:=CodeToolBoss.GetCachedSourceName(CodeBuffer);
          // if not then parse it
          if SysUtils.CompareText(CurSrcUnitName,CurUnitName)<>0 then
            CurSrcUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
          // if it makes sense, update unitname
          if SysUtils.CompareText(CurSrcUnitName,CurFile.UnitName)=0 then
            CurFile.UnitName:=CurSrcUnitName;
        end;
        if SysUtils.CompareText(CurUnitName,CurFile.UnitName)=0 then
          CurUnitName:=CurFile.UnitName
        else
          CurFile.UnitName:=CurUnitName;
      end;

      if (CurUnitName<>'') and IsValidIdent(CurUnitName) then begin
        NeedsRegisterProcCall:=CurFile.HasRegisterProc
          and (APackage.PackageType in [lptDesignTime,lptRunAndDesignTime]);
        if NeedsRegisterProcCall or CurFile.AddToUsesPkgSection then begin
          if UsedUnits<>'' then
            UsedUnits:=UsedUnits+', ';
          UsedUnits:=UsedUnits+CurUnitName;
        end;
        if NeedsRegisterProcCall then begin
          RegistrationCode:=RegistrationCode+
            '  RegisterUnit('''+CurUnitName+''',@'+CurUnitName+'.Register);'+e;
        end;
      end else begin
        MessagesView.AddMsg('WARNING: unit name invalid '+CurFile.Filename
           +', package='+APackage.IDAsString,
           APackage.Directory,-1);
      end;
    end;
  end;
  // append registration code only for design time packages
  if (APackage.PackageType in [lptDesignTime,lptRunAndDesignTime]) then begin
    RegistrationCode:=
      'procedure Register;'+e
      +'begin'+e
      +RegistrationCode
      +'end;'+e
      +e
      +'initialization'+e
      +'  RegisterPackage('''+APackage.Name+''',@Register);'
      +e;
    if UsedUnits<>'' then UsedUnits:=UsedUnits+', ';
    UsedUnits:=UsedUnits+'LazarusPackageIntf';
  end;

  // create source
  HeaderSrc:=lisPkgMangThisSourceIsOnlyUsedToCompileAndInstallThePackage;
  HeaderSrc:= '{ '
           +lisPkgMangThisFileWasAutomaticallyCreatedByLazarusDoNotEdit+e
           +lisPkgMangThisSourceIsOnlyUsedToCompileAndInstallThePackage+e
           +' }'+e+e;
  Src:='unit '+APackage.Name+';'+e
      +e
      +'interface'+e
      +e;
  if UsedUnits<>'' then
    Src:=Src
      +'uses'+e
      +'  '+UsedUnits+';'+e
      +e;
  Src:=Src
      +'implementation'+e
      +e
      +RegistrationCode
      +'end.'+e;
  Src:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                                                       BeautifyStatement(Src,0);
  Src:=HeaderSrc+Src;

  // check if old code is already uptodate
  Result:=LoadCodeBuffer(CodeBuffer,SrcFilename,[lbfQuiet,lbfCheckIfText,
                                      lbfUpdateFromDisk,lbfCreateClearOnError]);
  if Result<>mrOk then begin
    DebugLn('TPkgManager.DoSavePackageMainSource LoadCodeBuffer ',SrcFilename,' failed');
    exit;
  end;
  OldShortenSrc:=CodeToolBoss.ExtractCodeWithoutComments(CodeBuffer);
  NewShortenSrc:=CleanCodeFromComments(Src,
                CodeToolBoss.GetNestedCommentsFlagForFile(CodeBuffer.Filename));
  if CompareTextIgnoringSpace(OldShortenSrc,NewShortenSrc,true)=0 then begin
    Result:=mrOk;
    exit;
  end;
  if OldShortenSrc<>NewShortenSrc then begin
    DebugLn('TPkgManager.DoSavePackageMainSource Src changed ',dbgs(length(OldShortenSrc)),' ',dbgs(length(NewShortenSrc)));
  end;

  // save source
  Result:=MainIDE.DoSaveStringToFile(SrcFilename, Src,
    lisPkgMangpackageMainSourceFile);
  if Result<>mrOk then begin
    DebugLn('TPkgManager.DoSavePackageMainSource DoSaveStringToFile ',SrcFilename,' failed');
    exit;
  end;

  Result:=mrOk;
end;

function TPkgManager.DoCreatePackageMakefile(APackage: TLazPackage
  ): TModalResult;
begin
  Result:=DoCompilePackage(APackage,[pcfDoNotCompileDependencies,
                           pcfDoNotCompilePackage,pcfCreateMakefile],nil);
end;

function TPkgManager.OnRenameFile(const OldFilename, NewFilename: string;
  IsPartOfProject: boolean): TModalResult;
var
  OldPackage: TLazPackage;
  OldPkgFile: TPkgFile;
  NewPkgFile: TPkgFile;
begin
  Result:=mrOk;
  if (OldFilename=NewFilename) then
    exit;
  debugln('TPkgManager.OnRenameFile A OldFilename="',OldFilename,'" New="',NewFilename,'"');
  OldPkgFile:=PackageGraph.FindFileInAllPackages(OldFilename,false,true,
                                                 not IsPartOfProject);
  if (OldPkgFile=nil) or (OldPkgFile.LazPackage.ReadOnly) then
    exit;
  OldPackage:=OldPkgFile.LazPackage;
  debugln('TPkgManager.OnRenameFile A OldPackage="',OldPackage.Name);
  NewPkgFile:=PackageGraph.FindFileInAllPackages(NewFilename,false,true,false);
  if (NewPkgFile<>nil) and (OldPackage<>NewPkgFile.LazPackage) then exit;

  Result:=MessageDlg(lisPkgMangRenameFileInPackage,
    Format(lisPkgMangThePackageOwnsTheFileShouldTheFileBeRenamed, [
      OldPackage.IDAsString, #13, '"', OldFilename, '"', #13]),
    mtConfirmation,[mbYes,mbNo,mbAbort],0);
  if Result=mrNo then begin
    Result:=mrOk;
    exit;
  end;
  if Result<>mrYes then exit;
  
  OldPkgFile.Filename:=NewFilename;
  if OldPackage.Editor<>nil then OldPackage.Editor.UpdateAll;
  OldPackage.Modified:=true;

  Result:=mrOk;
end;

{------------------------------------------------------------------------------
  function TPkgManager.FindIncludeFileInProjectDependencies(Project1: TProject;
    const Filename: string): string;
    
  Search filename in the include paths of all required packages
------------------------------------------------------------------------------}
function TPkgManager.FindIncludeFileInProjectDependencies(Project1: TProject;
  const Filename: string): string;
var
  APackage: TLazPackage;
  IncPath: String;
  PkgList: Tlist;
  i: Integer;
begin
  Result:='';
  if FilenameIsAbsolute(Filename) then begin
    Result:=Filename;
    exit;
  end;
  PkgList:=nil;
  PackageGraph.GetAllRequiredPackages(Project1.FirstRequiredDependency,PkgList);
  if PkgList=nil then exit;
  try
    for i:=0 to PkgList.Count-1 do begin
      APackage:=TLazPackage(PkgList[i]);
      IncPath:=APackage.CompilerOptions.GetIncludePath(false);
      Result:=SearchFileInPath(Filename,APackage.Directory,IncPath,';',[]);
      if Result<>'' then exit;
    end;
  finally
    PkgList.Free;
  end;
end;

function TPkgManager.AddUnitDependenciesForComponentClasses(
  const UnitFilename: string; ComponentClassnames: TStrings): TModalResult;
var
  UnitBuf: TCodeBuffer;
  UnitNames: TStringList;
  Packages: TList;
  MissingDependencies: TObjectArray;
  
  function LoadAndParseUnitBuf: TModalResult;
  begin
    if not CodeToolBoss.GatherExternalChanges then begin
      Result:=mrCancel;
      MainIDE.DoJumpToCodeToolBossError;
      exit;
    end;
    UnitBuf:=CodeToolBoss.LoadFile(UnitFilename,false,false);
    if UnitBuf=nil then begin
      Result:=MessageDlg('Error loading file',
        'Loading '+UnitFilename+' failed.',
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
    Result:=mrOk;
  end;

  function CollectNeededUnitnamesAndPackages: TModalResult;
  var
    i: Integer;
    RegComp: TRegisteredComponent;
    NewUnitName: String;
    PkgFile: TPkgFile;
  begin
    for i:=0 to ComponentClassnames.Count-1 do begin
      RegComp:=IDEComponentPalette.FindComponent(ComponentClassnames[i]);
      if (RegComp<>nil) then begin
        NewUnitName:=RegComp.GetUnitName;
        if (NewUnitName<>'') and (UnitNames.IndexOf(NewUnitName)<0) then
          UnitNames.Add(NewUnitName);
        if (RegComp is TPkgComponent) then begin
          PkgFile:=TPkgComponent(RegComp).PkgFile;
          if (PkgFile<>nil) and (PkgFile.LazPackage<>nil)
          and (Packages.IndexOf(PkgFile.LazPackage)<0) then
            Packages.Add(PkgFile.LazPackage);
        end;
      end;
    end;
    Result:=mrOk;
  end;

  function RemoveExistingUnitnames: TModalResult;
  var
    ImplementationUsesSection: TStringList;
    MainUsesSection: TStringList;
    j: LongInt;
    i: Integer;
  begin
    Result:=LoadAndParseUnitBuf;
    if Result<>mrOk then exit;
    if not CodeToolBoss.FindUsedUnitNames(UnitBuf,MainUsesSection,
      ImplementationUsesSection)
    then begin
      MainIDE.DoJumpToCodeToolBossError;
      exit;
    end;
    for i:=0 to MainUsesSection.Count-1 do begin
      j:=UnitNames.IndexOf(MainUsesSection[i]);
      if j>=0 then UnitNames.Delete(j);
    end;
    MainUsesSection.Free;
    ImplementationUsesSection.Free;
    Result:=mrOk;
  end;
  
  function AskUser: TModalResult;
  var
    UsesAdditions: String;
    UnitOwner: TObject;
    RequiredPackage: TLazPackage;
    i: Integer;
    PackageAdditions: String;
    Msg: String;
  begin
    UsesAdditions:='';
    for i:=0 to UnitNames.Count-1 do begin
      if UsesAdditions<>'' then UsesAdditions:=UsesAdditions+', ';
      UsesAdditions:=UsesAdditions+UnitNames[i];
    end;
    DebugLn('TPkgManager.AddUnitDependenciesForComponentClasses UsesAdditions=',UsesAdditions);
    PackageAdditions:='';
    if MissingDependencies<>nil then begin
      for i:=0 to MissingDependencies.Count-1 do begin
        UnitOwner:=TObject(MissingDependencies[i]);
        RequiredPackage:=TLazPackage(MissingDependencies.Objects[i]);
        if UnitOwner is TProject then begin
          PackageAdditions:=Format(
            lisPkgMangAddingNewDependencyForProjectPackage, [PackageAdditions,
            TProject(UnitOwner).Title, RequiredPackage.Name, #13#13]);
        end else if UnitOwner is TLazPackage then begin
          PackageAdditions:=Format(
            lisPkgMangAddingNewDependencyForPackagePackage, [PackageAdditions,
            TLazPackage(UnitOwner).Name, RequiredPackage.Name, #13#13]);
        end;
      end;
    end;
    DebugLn('TPkgManager.AddUnitDependenciesForComponentClasses PackageAdditions=',PackageAdditions);
    Msg:='';
    if UsesAdditions<>'' then begin
      Msg:=Format(lisPkgMangTheFollowingUnitsWillBeAddedToTheUsesSectionOf, [
        Msg, #13, UnitFilename, #13, UsesAdditions, #13#13]);
    end;
    if PackageAdditions<>'' then begin
      Msg:=Msg+PackageAdditions;
    end;
    if Msg<>'' then begin
      Result:=MessageDlg(lisConfirmChanges,
        Msg,mtConfirmation,[mbOk,mbAbort],0);
      exit;
    end;
    Result:=mrOk;
  end;
  
  function AddDependencies: TModalResult;
  var
    i: Integer;
    UnitOwner: TObject;
    RequiredPackage: TLazPackage;
  begin
    if MissingDependencies<>nil then begin
      for i:=0 to MissingDependencies.Count-1 do begin
        UnitOwner:=TObject(MissingDependencies[i]);
        RequiredPackage:=TLazPackage(MissingDependencies.Objects[i]);
        if UnitOwner is TProject then begin
          DebugLn('TPkgManager.AddUnitDependenciesForComponentClasses Adding Project Dependency ',TProject(UnitOwner).Title,' -> ',RequiredPackage.Name);
          AddProjectDependency(TProject(UnitOwner),RequiredPackage);
        end else if UnitOwner is TLazPackage then begin
          DebugLn('TPkgManager.AddUnitDependenciesForComponentClasses Adding Package Dependency ',TLazPackage(UnitOwner).Name,' -> ',RequiredPackage.Name);
          PackageGraph.AddDependencyToPackage(TLazPackage(UnitOwner),
                                              RequiredPackage);
        end;
      end;
    end;
    Result:=mrOk;
  end;

  function AddUsedUnits: TModalResult;
  var
    i: Integer;
  begin
    Result:=LoadAndParseUnitBuf;
    if Result<>mrOk then exit;
    for i:=0 to UnitNames.Count-1 do begin
      DebugLn('TPkgManager.AddUnitDependenciesForComponentClasses Extending Uses ',UnitBuf.Filename,' ',UnitNames[i]);
      if not CodeToolBoss.AddUnitToMainUsesSection(UnitBuf,UnitNames[i],'') then
        MainIDE.DoJumpToCodeToolBossError;
    end;
    Result:=mrOk;
  end;

begin
  Result:=mrCancel;
  UnitNames:=TStringList.Create;
  Packages:=TList.Create;
  MissingDependencies:=nil;
  try
  
    Result:=CollectNeededUnitnamesAndPackages;
    if Result<>mrOk then exit;
    
    Result:=RemoveExistingUnitnames;
    if Result<>mrOk then exit;
    
    Result:=GetMissingDependenciesForUnit(UnitFilename,ComponentClassnames,
                                          MissingDependencies);
    if Result<>mrOk then exit;
    if (UnitNames.Count=0)
    and ((MissingDependencies=nil) or (MissingDependencies.Count=0)) then begin
      // no change needed
      Result:=mrOk;
      exit;
    end;

    Result:=AskUser;
    if Result<>mrOk then exit;
    
    Result:=AddDependencies;
    if Result<>mrOk then exit;

    Result:=AddUsedUnits;
    if Result<>mrOk then exit;

    Result:=mrOk;
  finally
    UnitNames.Free;
    Packages.Free;
    MissingDependencies.Free;
  end;
end;

function TPkgManager.GetMissingDependenciesForUnit(
  const UnitFilename: string; ComponentClassnames: TStrings;
  var List: TObjectArray): TModalResult;
// returns a list of packages needed to use the Component in the unit
var
  UnitOwners: TList;
  UnitOwner: TObject;
  FirstDependency: TPkgDependency;
  CurClassID: Integer;
  CurOwnerID: Integer;
  CurCompClass: string;
  CurRegisteredComponent: TRegisteredComponent;
  PkgFile: TPkgFile;
  RequiredPackage: TLazPackage;
begin
  Result:=mrCancel;
  List:=nil;
  UnitOwners:=GetOwnersOfUnit(UnitFilename);
  if (UnitOwners<>nil) then begin
    for CurOwnerID:=0 to UnitOwners.Count-1 do begin
      UnitOwner:=TObject(UnitOwners[CurOwnerID]);
      if UnitOwner is TProject then
        FirstDependency:=TProject(UnitOwner).FirstRequiredDependency
      else if UnitOwner is TLazPackage then
        FirstDependency:=TLazPackage(UnitOwner).FirstRequiredDependency
      else
        FirstDependency:=nil;
      for CurClassID:=0 to ComponentClassnames.Count-1 do begin
        CurCompClass:=ComponentClassnames[CurClassID];
        CurRegisteredComponent:=IDEComponentPalette.FindComponent(CurCompClass);
        if CurRegisteredComponent is TPkgComponent then begin
          PkgFile:=TPkgComponent(CurRegisteredComponent).PkgFile;
          if PkgFile<>nil then begin
            RequiredPackage:=PkgFile.LazPackage;
            if (RequiredPackage<>nil)
            and (RequiredPackage<>UnitOwner)
            and (FindCompatibleDependencyInList(FirstDependency,pdlRequires,
              RequiredPackage)=nil)
            then begin
              if List=nil then List:=TObjectArray.Create;
              List.AddObject(UnitOwner,RequiredPackage);
              //writeln('TPkgManager.GetMissingDependenciesForUnit A ',UnitOwner.ClassName,' ',RequiredPackage.Name);
              //if TObject(List[List.Count-1])<>UnitOwner then RaiseException('A');
              //if TObject(List.Objects[List.Count-1])<>RequiredPackage then RaiseException('B');
            end;
          end;
        end;
      end;
    end;
    UnitOwners.Free;
  end;
  Result:=mrOk;
end;

function TPkgManager.GetOwnersOfUnit(const UnitFilename: string): TList;
var
  PkgFile: TPkgFile;
begin
  Result:=TList.Create;
  // check if unit is part of project
  if Project1<>nil then begin
    if Project1.UnitInfoWithFilename(UnitFilename,
      [pfsfResolveFileLinks,pfsfOnlyProjectFiles])<>nil
    then
      Result.Add(Project1);
  end;
  // find all packages owning file
  PkgFile:=PackageGraph.FindFileInAllPackages(UnitFilename,false,true,true);
  if (PkgFile<>nil) and (PkgFile.LazPackage<>nil) then
    Result.Add(PkgFile.LazPackage);
  if Result.Count=0 then
    FreeThenNil(Result);
end;

procedure TPkgManager.ExtendOwnerListWithUsedByOwners(OwnerList: TList);
// use items (packages and projects) in OwnerList as leaves and create the
// list of all packages and projects using them.
// The result will be the topologically sorted list of projects and packages
// using the projects/packages in OwnerList, beginning with the top levels.
var
  AddedNonPackages: TList;

  procedure AddUsedByOwners(ADependenyOwner: TObject);
  var
    LazPackage: TLazPackage;
    Dependency: TPkgDependency;
  begin
    if ADependenyOwner is TProject then begin
      if AddedNonPackages.IndexOf(ADependenyOwner)>=0 then exit;
      AddedNonPackages.Add(ADependenyOwner);
      OwnerList.Add(ADependenyOwner);
    end else if ADependenyOwner is TLazPackage then begin
      LazPackage:=TLazPackage(ADependenyOwner);
      if lpfVisited in LazPackage.Flags then exit;
      LazPackage.Flags:=LazPackage.Flags+[lpfVisited];
      Dependency:=LazPackage.FirstUsedByDependency;
      while Dependency<>nil do begin
        AddUsedByOwners(Dependency.Owner);
        Dependency:=Dependency.NextUsedByDependency;
      end;
      OwnerList.Add(LazPackage);
    end;
  end;
  
var
  i: Integer;
  OldOwnerList: TList;
begin
  OldOwnerList:=TList.Create;
  for i:=0 to OwnerList.Count-1 do
    OldOwnerList.Add(OwnerList[i]);
  OwnerList.Clear;
  AddedNonPackages:=TList.Create;
  PackageGraph.MarkAllPackagesAsNotVisited;
  for i:=0 to OldOwnerList.Count-1 do
    AddUsedByOwners(TObject(OldOwnerList[i]));
  OldOwnerList.Free;
end;

function TPkgManager.GetSourceFilesOfOwners(OwnerList: TList): TStrings;

  procedure AddFile(TheOwner: TObject; const Filename: string);
  begin
    if Result=nil then Result:=TStringList.Create;
    Result.AddObject(Filename,TheOwner);
  end;

var
  CurOwner: TObject;
  CurPackage: TLazPackage;
  CurPkgFile: TPkgFile;
  CurProject: TProject;
  CurUnit: TUnitInfo;
  i: Integer;
  j: Integer;
begin
  Result:=nil;
  if OwnerList=nil then exit;
  for i:=0 to OwnerList.Count-1 do begin
    CurOwner:=TObject(OwnerList[i]);
    if CurOwner is TLazPackage then begin
      CurPackage:=TLazPackage(CurOwner);
      for j:=0 to CurPackage.FileCount-1 do begin
        CurPkgFile:=CurPackage.Files[j];
        if CurPkgFile.FileType in PkgFileUnitTypes then
          AddFile(CurOwner,CurPkgFile.Filename);
      end;
    end else if CurOwner is TProject then begin
      CurProject:=TProject(CurOwner);
      CurUnit:=CurProject.FirstPartOfProject;
      while CurUnit<>nil do begin
        if FilenameIsPascalSource(CurUnit.Filename) then
          AddFile(CurOwner,CurUnit.Filename);
        CurUnit:=CurUnit.NextPartOfProject;
      end;
    end;
  end;
end;

function TPkgManager.GetPackageOfCurrentSourceEditor: TPkgFile;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=SourceNotebook.GetActiveSE;
  if SrcEdit<>nil then begin
    //debugln('TPkgManager.GetPackageOfCurrentSourceEditor ',SrcEdit.Filename);
    Result:=SearchFile(SrcEdit.Filename,[],nil);
  end else
    SrcEdit:=nil;
end;

function TPkgManager.DoOpenPkgFile(PkgFile: TPkgFile): TModalResult;
var
  Filename: String;
begin
  if (PkgFile.FileType=pftVirtualUnit) then begin
    Filename:=FindVirtualUnitSource(PkgFile);
    if Filename<>'' then begin
      Result:=MainIDE.DoOpenEditorFile(Filename,-1,
                                  [ofOnlyIfExists,ofAddToRecent,ofRegularFile]);
      exit;
    end;
  end;
  Result:=MainIDE.DoOpenMacroFile(Self,PkgFile.Filename);
end;

function TPkgManager.FindVirtualUnitSource(PkgFile: TPkgFile): string;
begin
  Result:='';
  if (PkgFile.FileType=pftVirtualUnit)
  and (PkgFile.LazPackage<>nil)
  and (not FileExists(PkgFile.Filename)) then begin
    Result:=MainIDE.FindSourceFile(PkgFile.GetShortFilename(false),
                                     PkgFile.LazPackage.Directory,[]);
  end;
end;

function TPkgManager.SearchFile(const AFilename: string;
  SearchFlags: TSearchIDEFileFlags; InObject: TObject): TPkgFile;
var
  i: Integer;
  APackage: TLazPackage;
  CurFilename: String;
begin
  if InObject is TLazPackage then begin
    APackage:=TLazPackage(InObject);
    CurFilename:=AFilename;
    APackage.ShortenFilename(CurFilename,true);
    Result:=APackage.SearchFile(CurFilename,SearchFlags);
    if Result<>nil then exit;
  end;
  if not (siffDoNotCheckAllPackages in SearchFlags) then begin
    for i:=0 to PackageGraph.Count-1 do begin
      APackage:=PackageGraph[i];
      CurFilename:=AFilename;
      APackage.ShortenFilename(CurFilename,true);
      Result:=APackage.SearchFile(CurFilename,SearchFlags);
      //debugln('TPkgManager.SearchFile Pkg=',APackage.Filename,' CurFilename="',CurFilename,'"');
      if Result<>nil then exit;
    end;
  end;
  Result:=nil;
end;

function TPkgManager.DoAddActiveUnitToAPackage: TModalResult;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  PkgFile: TPkgFile;
  Filename: String;
  TheUnitName: String;
  HasRegisterProc: Boolean;
begin
  MainIDE.GetCurrentUnitInfo(ActiveSourceEditor,ActiveUnitInfo);
  if ActiveSourceEditor=nil then exit;
  
  Filename:=ActiveUnitInfo.Filename;
  
  // check if filename is absolute
  if ActiveUnitInfo.IsVirtual or (not FileExists(Filename)) then begin
    Result:=MessageDlg(lisPkgMangFileNotSaved,
      lisPkgMangPleaseSaveTheFileBeforeAddingItToAPackage,
      mtWarning,[mbCancel],0);
    exit;
  end;
  
  // check if file is part of project
  if ActiveUnitInfo.IsPartOfProject then begin
    Result:=MessageDlg(lisPkgMangFileIsInProject,
      Format(lisPkgMangWarningTheFileBelongsToTheCurrentProject, ['"',
        Filename, '"', #13])
      ,mtWarning,[mbIgnore,mbCancel,mbAbort],0);
    if Result<>mrIgnore then exit;
  end;
  
  // check if file is already in a package
  PkgFile:=PackageGraph.FindFileInAllPackages(Filename,false,true,true);
  if PkgFile<>nil then begin
    Result:=MessageDlg(lisPkgMangFileIsAlreadyInPackage,
      Format(lisPkgMangTheFileIsAlreadyInThePackage, ['"', Filename, '"', #13,
        PkgFile.LazPackage.IDAsString]),
      mtWarning,[mbIgnore,mbCancel,mbAbort],0);
    if Result<>mrIgnore then exit;
  end;
  
  TheUnitName:='';
  HasRegisterProc:=false;
  if FilenameIsPascalUnit(Filename) then begin
    Result:=DoGetUnitRegisterInfo(Filename,TheUnitName,HasRegisterProc,false);
    if Result<>mrOk then exit;
  end;
  
  Result:=ShowAddFileToAPackageDlg(Filename,TheUnitName,HasRegisterProc);
end;

function TPkgManager.WarnAboutMissingPackageFiles(APackage: TLazPackage
  ): TModalResult;
var
  i: Integer;
  AFile: TPkgFile;
  AFilename: String;
begin
  Result:=mrOk;
  for i:=0 to APackage.FileCount-1 do begin
    AFile:=APackage.Files[i];
    if AFile.FileType=pftVirtualUnit then continue;
    AFilename:=AFile.Filename;
    if System.Pos('$(',AFilename)>0 then begin
      // filename contains macros -> skip
    end;
    if (not APackage.IsVirtual) and FilenameIsAbsolute(AFilename) then
      APackage.LongenFilename(AFilename);
    if FilenameIsAbsolute(AFilename) then begin
      if not FileExistsCached(AFilename) then begin
        if not APackage.IsVirtual then
          AFilename:=CreateRelativePath(AFilename,APackage.Directory);
        Result:=QuestionDlg(lisPkgMangPackageFileMissing,
          Format(lisPkgMangTheFileOfPackageIsMissing, ['"', AFilename, '"',
            #13, APackage.IDAsString]),
          mtWarning,[mrIgnore,mrAbort],0);
        if Result<>mrAbort then
          Result:=mrOk;
        // one warning is enough
        exit;
      end;
    end else begin
      if not APackage.IsVirtual then begin
        // an unsaved file
        Result:=QuestionDlg(lisPkgMangPackageFileNotSaved,
          Format(lisPkgMangTheFileOfPackageNeedsToBeSavedFirst, ['"',
            AFilename, '"', #13, APackage.IDAsString]),
          mtWarning, [mrIgnore, lisPkgMangIgnoreAndSavePackageNow, mrAbort], 0
            );
        if Result<>mrAbort then
          Result:=mrOk;
      end;
    end;
  end;
end;

function TPkgManager.DoInstallPackage(APackage: TLazPackage): TModalResult;
var
  PkgList: TList;
  
  function GetPkgListIndex(APackage: TLazPackage): integer;
  begin
    Result:=PkgList.Count-1;
    while (Result>=0) and (TLazPackage(PkgList[Result])<>APackage) do
      dec(Result);
  end;
  
var
  Dependency: TPkgDependency;
  i: Integer;
  s: String;
  NeedSaving: Boolean;
  RequiredPackage: TLazPackage;
  BuildIDEFlags: TBuildLazarusFlags;
  Msg: string;
begin
  PackageGraph.BeginUpdate(true);
  PkgList:=nil;
  try
    // check if package is designtime package
    if APackage.PackageType=lptRunTime then begin
      Result:=MessageDlg(lisPkgMangPackageIsNoDesigntimePackage,
        Format(lisPkgMangThePackageIsARuntimeOnlyPackageRuntimeOnlyPackages, [
          APackage.IDAsString, #13]),
        mtError,[mbIgnore,mbAbort],0);
      if Result<>mrIgnore then exit;
    end;
  
    // save package
    if APackage.IsVirtual or APackage.Modified then begin
      Result:=DoSavePackage(APackage,[]);
      if Result<>mrOk then exit;
    end;

    // check consistency
    Result:=CheckPackageGraphForCompilation(APackage,nil,
                                           EnvironmentOptions.LazarusDirectory);
    if Result<>mrOk then exit;
    
    // get all required packages, which will also be auto installed
    APackage.GetAllRequiredPackages(PkgList);
    if PkgList=nil then PkgList:=TList.Create;
    
    // remove packages already marked for installation
    for i:=PkgList.Count-1 downto 0 do begin
      RequiredPackage:=TLazPackage(PkgList[i]);
      if (RequiredPackage.AutoInstall<>pitNope) then
        PkgList.Delete(i);
    end;
    
    // now PkgList contains only the required packages that were added to the
    // list of installation packages
    // => show the user the list
    if PkgList.Count>0 then begin
      s:='';
      for i:=0 to PkgList.Count-1 do begin
        RequiredPackage:=TLazPackage(PkgList[i]);
        s:=s+RequiredPackage.IDAsString+#13;
      end;
      if PkgList.Count=0 then
        Msg:=Format(
          lisPkgMangInstallingThePackageWillAutomaticallyInstallThePac, [
          APackage.IDAsString])
      else
        Msg:=Format(
          lisPkgMangInstallingThePackageWillAutomaticallyInstallThePac2, [
          APackage.IDAsString]);
      Result:=MessageDlg(lisPkgMangAutomaticallyInstalledPackages,
        Msg+#13+s,mtConfirmation,[mbOk,mbCancel,mbAbort],0);
      if Result<>mrOk then exit;
    end;

    // add packages to auto installed packages
    if GetPkgListIndex(APackage)<0 then
      PkgList.Add(APackage);
    NeedSaving:=false;
    for i:=0 to PkgList.Count-1 do begin
      RequiredPackage:=TLazPackage(PkgList[i]);
      if RequiredPackage.AutoInstall=pitNope then begin
        RequiredPackage.AutoInstall:=pitStatic;
        Dependency:=RequiredPackage.CreateDependencyWithOwner(Self);
        Dependency.AddToList(FirstAutoInstallDependency,pdlRequires);
        PackageGraph.OpenDependency(Dependency);
        NeedSaving:=true;
      end;
    end;
  finally
    PackageGraph.EndUpdate;
    PkgList.Free;
  end;

  if NeedSaving then begin
    SortAutoInstallDependencies;
    SaveAutoInstallDependencies(true);
  end;

  // save IDE build configs, so user can build IDE on command line
  BuildIDEFlags:=[blfWithStaticPackages,blfDontClean,blfOnlyIDE];
  Result:=MainIDE.DoSaveBuildIDEConfigs(BuildIDEFlags);
  if Result<>mrOk then exit;

  // ask user to rebuilt Lazarus now
  Result:=MessageDlg(lisPkgMangRebuildLazarus,
    Format(lisPkgMangThePackageWasMarkedForInstallationCurrentlyLazarus, [
      '"', APackage.IDAsString, '"', #13, #13, #13]),
    mtConfirmation,[mbYes,mbNo],0);
  if Result=mrNo then begin
    Result:=mrOk;
    exit;
  end;
  
  // rebuild Lazarus
  Result:=MainIDE.DoBuildLazarus(BuildIDEFlags);
  if Result<>mrOk then exit;

  Result:=mrOk;
end;

function TPkgManager.DoUninstallPackage(APackage: TLazPackage;
  Flags: TPkgUninstallFlags): TModalResult;
var
  DependencyPath: TList;
  ParentPackage: TLazPackage;
  Dependency: TPkgDependency;
  BuildIDEFlags: TBuildLazarusFlags;
begin
  if (APackage.Installed=pitNope) and (APackage.AutoInstall=pitNope) then exit;
  
  // check if package is required by auto install package
  DependencyPath:=PackageGraph.FindAutoInstallDependencyPath(APackage);
  if DependencyPath<>nil then begin
    DoShowPackageGraphPathList(DependencyPath);
    ParentPackage:=TLazPackage(DependencyPath[0]);
    Result:=MessageDlg(lisPkgMangPackageIsRequired,
      Format(lisPkgMangThePackageIsRequiredByWhichIsMarkedForInstallation, [
        APackage.IDAsString, ParentPackage.IDAsString, #13]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // confirm uninstall package
  if not (puifDoNotConfirm in Flags) then begin
    Result:=MessageDlg(lisPkgMangUninstallPackage,
      Format(lisPkgMangUninstallPackage2, [APackage.IDAsString]),
      mtConfirmation,[mbYes,mbCancel,mbAbort],0);
    if Result<>mrYes then exit;
  end;
  
  PackageGraph.BeginUpdate(true);
  try
    // save package
    if APackage.IsVirtual or APackage.Modified then begin
      Result:=DoSavePackage(APackage,[]);
      if Result<>mrOk then exit;
    end;

    // remove package from auto installed packages
    if APackage.AutoInstall<>pitNope then begin
      APackage.AutoInstall:=pitNope;
      Dependency:=FindCompatibleDependencyInList(FirstAutoInstallDependency,
                                                 pdlRequires,APackage);
      if Dependency<>nil then begin
        Dependency.RemoveFromList(FirstAutoInstallDependency,pdlRequires);
        Dependency.Free;
        SortAutoInstallDependencies;
      end;
      SaveAutoInstallDependencies(true);
    end;

    // save IDE build configs, so user can build IDE on command line
    BuildIDEFlags:=[blfWithStaticPackages,blfDontClean,blfOnlyIDE];
    Result:=MainIDE.DoSaveBuildIDEConfigs(BuildIDEFlags);
    if Result<>mrOk then exit;

    if not (puifDoNotBuildIDE in Flags) then begin
      // ask user to rebuilt Lazarus now
      Result:=MessageDlg(lisPkgMangRebuildLazarus,
        Format(lisPkgMangThePackageWasMarkedCurrentlyLazarus, ['"',
          APackage.IDAsString, '"', #13, #13, #13]),
        mtConfirmation,[mbYes,mbNo],0);
      if Result=mrNo then begin
        Result:=mrOk;
        exit;
      end;

      // rebuild Lazarus
      Result:=MainIDE.DoBuildLazarus(BuildIDEFlags);
      if Result<>mrOk then exit;
    end;
  finally
    PackageGraph.EndUpdate;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoOpenPackageSource(APackage: TLazPackage): TModalResult;
var
  Filename: String;
begin
  Result:=mrCancel;
  if APackage.IsVirtual then begin
    MessageDlg(lisPkgMangThisIsAVirtualPackageItHasNoSourceYetPleaseSaveThe,
      mtError, [mbCancel], 0);
    exit;
  end;
  Filename:=APackage.GetSrcFilename;
  if (not FilenameIsAbsolute(Filename)) or (not FileExists(Filename)) then begin
    MessageDlg(lisPkgMangPleaseSaveThePackageFirst, mtError, [mbCancel], 0);
    exit;
  end;
  Result:=MainIDE.DoOpenEditorFile(Filename,-1,[ofRegularFile]);
end;

function TPkgManager.DoCompileAutoInstallPackages(
  Flags: TPkgCompileFlags): TModalResult;
var
  Dependency: TPkgDependency;
  OldDependency: TPkgDependency;
begin
  PackageGraph.BeginUpdate(false);
  try
    Dependency:=FirstAutoInstallDependency;
    while Dependency<>nil do begin
      OldDependency:=Dependency;
      Dependency:=Dependency.NextRequiresDependency;
      if OldDependency.LoadPackageResult<>lprSuccess then begin
        Result:=MessageDlg(lisProjAddPackageNotFound,
          Format(lisPkgMangThePackageIsMarkedForInstallationButCanNotBeFound, [
            '"', OldDependency.AsString, '"', #13]),
          mtError,[mbYes,mbNo,mbAbort],0);
        if Result=mrNo then Result:=mrCancel;
        if Result<>mrYes then exit;
        OldDependency.RemoveFromList(FirstAutoInstallDependency,pdlRequires);
        OldDependency.Free;
        SaveAutoInstallDependencies(true);
      end;
    end;
    
    // check consistency
    Result:=CheckPackageGraphForCompilation(nil,FirstAutoInstallDependency,
                                           EnvironmentOptions.LazarusDirectory);
    if Result<>mrOk then exit;

    // save all open files
    if not (pcfDoNotSaveEditorFiles in Flags) then begin
      Result:=MainIDE.DoSaveForBuild;
      if Result<>mrOk then exit;
    end;
    
    // compile all auto install dependencies
    Result:=CompileRequiredPackages(nil,FirstAutoInstallDependency,
                       MiscellaneousOptions.BuildLazOpts.Globals,[pupAsNeeded]);
    if Result<>mrOk then exit;
    
  finally
    PackageGraph.EndUpdate;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoSaveAutoInstallConfig: TModalResult;
var
  ConfigDir: String;
  StaticPackagesInc: String;
  StaticPckIncludeFile: String;
  Dependency: TPkgDependency;
  TargetDir: String;
begin
  ConfigDir:=AppendPathDelim(GetPrimaryConfigPath);
  
  // create auto install package list for the Lazarus uses section
  StaticPackagesInc:='';
  Dependency:=FirstAutoInstallDependency;
  while Dependency<>nil do begin
    if (not Dependency.RequiredPackage.AutoCreated) then
      StaticPackagesInc:=StaticPackagesInc+Dependency.PackageName+','+LineEnding;
    Dependency:=Dependency.NextRequiresDependency;
  end;
  StaticPckIncludeFile:=ConfigDir+'staticpackages.inc';
  Result:=MainIDE.DoSaveStringToFile(StaticPckIncludeFile,StaticPackagesInc,
                                     lisPkgMangstaticPackagesConfigFile);
  if Result<>mrOk then exit;

  TargetDir:=MiscellaneousOptions.BuildLazOpts.TargetDirectory;
  IDEMacros.SubstituteMacros(TargetDir);
  if not ForceDirectory(TargetDir) then begin
    Result:=MessageDlg(lisPkgMangUnableToCreateDirectory,
      Format(lisPkgMangUnableToCreateTargetDirectoryForLazarus, [#13, '"',
        TargetDir, '"', #13]),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  Result:=mrOk;
end;

function TPkgManager.DoGetIDEInstallPackageOptions(
  var InheritedOptionStrings: TInheritedCompOptsStrings): string;
  
  procedure AddOption(const s: string);
  begin
    if s='' then exit;
    if Result='' then
      Result:=s
    else
      Result:=Result+' '+s;
  end;
  
var
  PkgList: TList;
  AddOptionsList: TList;
  ConfigDir: String;
begin
  Result:='';
  if not Assigned(OnGetAllRequiredPackages) then exit;
  
  // get all required packages
  PkgList:=nil;
  OnGetAllRequiredPackages(FirstAutoInstallDependency,PkgList);
  if PkgList=nil then exit;
  // get all usage options
  AddOptionsList:=GetUsageOptionsList(PkgList);
  PkgList.Free;
  if AddOptionsList<>nil then begin
    // combine options of same type
    GatherInheritedOptions(AddOptionsList,coptParsed,InheritedOptionStrings);
    AddOptionsList.Free;
  end;

  // convert options to compiler parameters
  Result:=InheritedOptionsToCompilerParameters(InheritedOptionStrings,[]);
  
  // add activate-static-packages option
  AddOption('-dAddStaticPkgs');
  
  // add include path to config directory
  ConfigDir:=AppendPathDelim(GetPrimaryConfigPath);
  AddOption(PrepareCmdLineOption('-Fi'+ConfigDir));
end;

function TPkgManager.DoPublishPackage(APackage: TLazPackage;
  Flags: TPkgSaveFlags; ShowDialog: boolean): TModalResult;
begin
  // show the publish dialog
  if ShowDialog then begin
    Result:=ShowPublishProjectDialog(APackage.PublishOptions);
    if Result<>mrOk then exit;
  end;

  // save package
  Result:=DoSavePackage(APackage,Flags);
  if Result<>mrOk then exit;

  // publish package
  Result:=MainIDE.DoPublishModule(APackage.PublishOptions,APackage.Directory,
                                  GetPublishPackageDir(APackage));
end;

function TPkgManager.OnProjectInspectorOpen(Sender: TObject): boolean;
var
  Dependency: TPkgDependency;
begin
  Result:=false;
  if (Sender=nil) or (not (Sender is TProjectInspectorForm)) then exit;
  Dependency:=TProjectInspectorForm(Sender).GetSelectedDependency;
  if Dependency=nil then exit;
  // user has selected a dependency -> open package
  Result:=true;
  if PackageGraph.OpenDependency(Dependency)<>lprSuccess then
    exit;
  DoOpenPackage(Dependency.RequiredPackage);
end;

function TPkgManager.OnProjectInspectorAddDependency(Sender: TObject;
  ADependency: TPkgDependency): TModalResult;
begin
  Result:=AddProjectDependency(Project1,ADependency);
end;

function TPkgManager.OnProjectInspectorRemoveDependency(Sender: TObject;
  ADependency: TPkgDependency): TModalResult;
var
  ShortUnitName: String;
  Dummy: Boolean;
begin
  Result:=mrOk;
  Project1.RemoveRequiredDependency(ADependency);
  //debugln('TPkgManager.OnProjectInspectorRemoveDependency A');
  if (Project1.MainUnitID>=0)
  and (pfMainUnitHasUsesSectionForAllUnits in Project1.Flags)
  then begin
    MainIDEInterface.SaveSourceEditorChangesToCodeCache(-1);
    ShortUnitName:=ADependency.PackageName;
    //debugln('TPkgManager.OnProjectInspectorRemoveDependency B ShortUnitName="',ShortUnitName,'"');
    if (ShortUnitName<>'') then begin
      Dummy:=CodeToolBoss.RemoveUnitFromAllUsesSections(
                                    Project1.MainUnitInfo.Source,ShortUnitName);
      if Dummy then
        Project1.MainUnitInfo.Modified:=true
      else begin
        MainIDEInterface.DoJumpToCodeToolBossError;
        Result:=mrCancel;
        exit;
      end;
    end;
  end;
end;

function TPkgManager.OnProjectInspectorReAddDependency(Sender: TObject;
  ADependency: TPkgDependency): TModalResult;
begin
  Result:=mrOk;
  Project1.ReaddRemovedDependency(ADependency);
  PackageGraph.OpenDependency(ADependency);
  if (ADependency.RequiredPackage<>nil)
  and (not ADependency.RequiredPackage.AutoCreated) then begin
    AddUnitToProjectMainUsesSection(Project1,ADependency.PackageName,'');
  end;
end;

function TPkgManager.CanOpenDesignerForm(AnUnitInfo: TUnitInfo): TModalResult;
var
  AProject: TProject;
begin
  Result:=mrCancel;
  if AnUnitInfo=nil then exit;
  AProject:=AnUnitInfo.Project;
  if AProject=nil then exit;
  Result:=CheckProjectHasInstalledPackages(AProject);
end;

function TPkgManager.DoClosePackageEditor(APackage: TLazPackage): TModalResult;
begin
  if APackage.Editor<>nil then begin
    APackage.Editor.Free;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult;
var
  AllSaved: Boolean;
  i: Integer;
  CurPackage: TLazPackage;
begin
  try
    repeat
      AllSaved:=true;
      i:=0;
      while i<PackageGraph.Count do begin
        CurPackage:=PackageGraph[i];
        if CurPackage.Modified and (not CurPackage.ReadOnly)
        and (not (lpfSkipSaving in CurPackage.Flags)) then begin
          Result:=DoSavePackage(CurPackage,Flags);
          if Result=mrIgnore then begin
            CurPackage.Flags:=CurPackage.Flags+[lpfSkipSaving];
            Result:=mrOk;
          end;
          if Result<>mrOk then exit;
          AllSaved:=false;
        end;
        inc(i);
      end;
    until AllSaved;
  finally
    // clear all lpfSkipSaving flags
    for i:=0 to PackageGraph.Count-1 do begin
      CurPackage:=PackageGraph[i];
      CurPackage.Flags:=CurPackage.Flags-[lpfSkipSaving];
    end;
  end;
  Result:=mrOk;
end;

{ TLazPackageDescriptors }

function TLazPackageDescriptors.GetItems(Index: integer): TPackageDescriptor;
begin
  Result:=TPackageDescriptor(FItems[Index]);
end;

constructor TLazPackageDescriptors.Create;
begin
  PackageDescriptors:=Self;
  FItems:=TList.Create;
end;

destructor TLazPackageDescriptors.Destroy;
var
  i: Integer;
begin
  fDestroying:=true;
  for i:=Count-1 downto 0 do Items[i].Release;
  FItems.Free;
  FItems:=nil;
  PackageDescriptors:=nil;
  inherited Destroy;
end;

function TLazPackageDescriptors.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazPackageDescriptors.GetUniqueName(const Name: string): string;
var
  i: Integer;
begin
  Result:=Name;
  if IndexOf(Result)<0 then exit;
  i:=0;
  repeat
    inc(i);
    Result:=Name+IntToStr(i);
  until IndexOf(Result)<0;
end;

function TLazPackageDescriptors.IndexOf(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Name,Items[Result].Name)<>0) do
    dec(Result);
end;

function TLazPackageDescriptors.FindByName(const Name: string
  ): TPackageDescriptor;
var
  i: LongInt;
begin
  i:=IndexOf(Name);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TLazPackageDescriptors.RegisterDescriptor(
  Descriptor: TPackageDescriptor);
begin
  if Descriptor.Name='' then
    raise Exception.Create('TLazPackageDescriptors.RegisterDescriptor Descriptor.Name empty');
  Descriptor.Name:=GetUniqueName(Descriptor.Name);
  FItems.Add(Descriptor);
end;

procedure TLazPackageDescriptors.UnregisterDescriptor(
  Descriptor: TPackageDescriptor);
var
  i: LongInt;
begin
  if fDestroying then exit;
  i:=FItems.IndexOf(Descriptor);
  if i<0 then
    raise Exception.Create('TLazPackageDescriptors.UnregisterDescriptor');
  FItems.Delete(i);
  Descriptor.Release;
end;

procedure TLazPackageDescriptors.AddDefaultPackageDescriptors;
begin
  NewIDEItems.Add(TNewLazIDEItemCategoryPackage.Create(PkgDescGroupName));
  RegisterPackageDescriptor(TPackageDescriptorStd.Create);
end;

{ TPackageDescriptorStd }

constructor TPackageDescriptorStd.Create;
begin
  inherited Create;
  Name:=PkgDescNameStandard;
end;

function TPackageDescriptorStd.GetLocalizedName: string;
begin
  Result:='Package';
end;

function TPackageDescriptorStd.GetLocalizedDescription: string;
begin
  Result:=Format(lisNewDlgCreateANewStandardPackageAPackageIsACollectionOfUn,
                 [#13]);
end;

end.

