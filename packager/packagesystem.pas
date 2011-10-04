{  $Id$  }
{
 /***************************************************************************
                            packagesystem.pas
                            -----------------


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
    The package registration.
}
unit PackageSystem;

{$mode objfpc}{$H+}

interface

{off $DEFINE IDE_MEM_CHECK}

{$DEFINE StopOnRegError}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  // FPC + LCL
  Classes, SysUtils, FileProcs, FileUtil, LCLProc, Forms, Controls, Dialogs,
  InterfaceBase,
  // codetools
  AVL_Tree, Laz_XMLCfg, DefineTemplates, CodeCache,
  BasicCodeTools, CodeToolsStructs, NonPascalCodeTools, SourceChanger,
  CodeToolManager,
  // IDEIntf,
  SrcEditorIntf, IDEExternToolIntf, IDEDialogs, IDEMsgIntf, PackageIntf,
  CompOptsIntf, LazIDEIntf,
  // package registration
  LazarusPackageIntf,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, IDEProcs, LazConf, TransferMacros,
  DialogProcs, IDETranslations, CompilerOptions, PackageLinks, PackageDefs,
  ComponentReg, ProjectIntf,
  FCLLaz, AllLCLUnits, allsynedit, LazControls;
  
type
  TFindPackageFlag = (
    fpfSearchInInstalledPckgs,
    fpfSearchInAutoInstallPckgs,
    fpfSearchInPckgsWithEditor,
    fpfSearchInLoadedPkgs,
    fpfSearchInPkgLinks,
    fpfPkgLinkMustExist,  // check if .lpk file exists
    fpfIgnoreVersion
    );
  TFindPackageFlags = set of TFindPackageFlag;
  
const
  fpfSearchEverywhere =
    [fpfSearchInInstalledPckgs,fpfSearchInAutoInstallPckgs,
     fpfSearchInPckgsWithEditor,fpfSearchInPkgLinks,fpfSearchInLoadedPkgs];
  fpfSearchAllExisting = fpfSearchEverywhere+[fpfPkgLinkMustExist];

  LCLCompPriority: TComponentPriority = (Category: cpBase; Level: 10);
  FCLCompPriority: TComponentPriority = (Category: cpBase; Level: 9);
  IDEIntfCompPriority: TComponentPriority = (Category: cpBase; Level: 8);

type
  TPkgUninstallFlag = (
    puifDoNotConfirm,
    puifDoNotBuildIDE
    );

  TPkgUninstallFlags = set of TPkgUninstallFlag;

  TPkgAddedEvent = procedure(APackage: TLazPackage) of object;
  TPkgDeleteEvent = procedure(APackage: TLazPackage) of object;
  TPkgUninstall = function(APackage: TLazPackage;
                    Flags: TPkgUninstallFlags; ShowAbort: boolean): TModalResult of object;
  TPkgTranslate = procedure(APackage: TLazPackage) of object;
  TDependencyModifiedEvent = procedure(ADependency: TPkgDependency) of object;
  TEndUpdateEvent = procedure(Sender: TObject; GraphChanged: boolean) of object;
  TFindFPCUnitEvent = procedure(const AUnitName, Directory: string;
                                var Filename: string) of object;
  TPkgDeleteAmbiguousFiles = function(const Filename: string): TModalResult of object;

  { TLazPackageGraph }

  TLazPackageGraph = class
  private
    FAbortRegistration: boolean;
    fChanged: boolean;
    FCodeToolsPackage: TLazPackage;
    FDefaultPackage: TLazPackage;
    FErrorMsg: string;
    FFCLPackage: TLazPackage;
    FIDEIntfPackage: TLazPackage;
    FItems: TFPList;   // unsorted list of TLazPackage
    FLazarusBasePackages: TFPList;
    FLCLBasePackage: TLazPackage;
    FLCLPackage: TLazPackage;
    FOnAddPackage: TPkgAddedEvent;
    FOnBeginUpdate: TNotifyEvent;
    FOnChangePackageName: TPkgChangeNameEvent;
    FOnDeleteAmbiguousFiles: TPkgDeleteAmbiguousFiles;
    FOnDeletePackage: TPkgDeleteEvent;
    FOnDependencyModified: TDependencyModifiedEvent;
    FOnEndUpdate: TEndUpdateEvent;
    FOnTranslatePackage: TPkgTranslate;
    FOnUninstallPackage: TPkgUninstall;
    FQuietRegistration: boolean;
    FRegistrationFile: TPkgFile;
    FRegistrationPackage: TLazPackage;
    FRegistrationUnitName: string;
    FSynEditPackage: TLazPackage;
    FLazControlsPackage: TLazPackage;
    FTree: TAVLTree; // sorted tree of TLazPackage
    FUpdateLock: integer;
    function CreateFCLPackage: TLazPackage;
    function CreateLCLBasePackage: TLazPackage;
    function CreateLCLPackage: TLazPackage;
    function CreateSynEditPackage: TLazPackage;
    function CreateLazControlsPackage: TLazPackage;
    function CreateCodeToolsPackage: TLazPackage;
    function CreateIDEIntfPackage: TLazPackage;
    function CreateDefaultPackage: TLazPackage;
    function CreateLazarusBasePackage(PkgName: string): TLazPackage;
    function GetCount: Integer;
    function GetPackages(Index: integer): TLazPackage;
    procedure DoDependencyChanged(Dependency: TPkgDependency);
    procedure SetRegistrationPackage(const AValue: TLazPackage);
    procedure UpdateBrokenDependenciesToPackage(APackage: TLazPackage);
    function OpenDependencyWithPackageLink(Dependency: TPkgDependency;
                       PkgLink: TPackageLink; ShowAbort: boolean): TModalResult;
    function DeleteAmbiguousFiles(const Filename: string): TModalResult;
    procedure AddMessage(const Msg, Directory: string);
    function OutputDirectoryIsWritable(APackage: TLazPackage; Directory: string;
                                       Verbose: boolean): boolean;
    function CheckIfCurPkgOutDirNeedsCompile(APackage: TLazPackage;
                    const CompilerFilename, CompilerParams, SrcFilename: string;
                    CheckDependencies, SkipDesignTimePackages: boolean;
                    out NeedBuildAllFlag, ConfigChanged, DependenciesChanged: boolean): TModalResult;
    procedure InvalidateStateFile(APackage: TLazPackage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: integer);
    function Count: integer; // number of Packages
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    function Updating: boolean;
    procedure RebuildDefineTemplates;
    function MacroFunctionPkgDir(const s: string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFunctionPkgSrcPath(const s: string; const Data: PtrInt;
                                     var Abort: boolean): string;
    function MacroFunctionPkgUnitPath(const s: string; const Data: PtrInt;
                                      var Abort: boolean): string;
    function MacroFunctionPkgIncPath(const s: string; const Data: PtrInt;
                                     var Abort: boolean): string;
    function MacroFunctionCTPkgDir(Data: Pointer): boolean;
    function MacroFunctionCTPkgSrcPath(Data: Pointer): boolean;
    function MacroFunctionCTPkgUnitPath(Data: Pointer): boolean;
    function MacroFunctionCTPkgIncPath(Data: Pointer): boolean;
    function MacroFunctionCTPkgName(Data: Pointer): boolean;
    function GetPackageFromMacroParameter(const TheID: string;
                                          out APackage: TLazPackage): boolean;
  public
    // searching
    function CheckIfPackageCanBeClosed(APackage: TLazPackage): boolean;
    function CreateUniquePkgName(const Prefix: string;
                                 IgnorePackage: TLazPackage): string;
    function CreateUniqueUnitName(const Prefix: string): string;
    function DependencyExists(Dependency: TPkgDependency;
                              Flags: TFindPackageFlags): boolean;
    function FindAPackageWithName(const PkgName: string;
                                  IgnorePackage: TLazPackage): TLazPackage;
    function FindBrokenDependencyPath(APackage: TLazPackage;
                                      FirstDependency: TPkgDependency): TFPList;
    function FindAllBrokenDependencies(APackage: TLazPackage;
                                        FirstDependency: TPkgDependency): TFPList;
    function FindCircleDependencyPath(APackage: TLazPackage;
                                      FirstDependency: TPkgDependency): TFPList;
    function FindPkgOutputInFPCSearchPath(APackage: TLazPackage;
                                      FirstDependency: TPkgDependency): TFPList; // find a package with auto compile and output dir is in FPC default search path
    function FindUnsavedDependencyPath(APackage: TLazPackage;
                                       FirstDependency: TPkgDependency): TFPList;
    function FindNotInstalledRegisterUnits(APackage: TLazPackage;
                                        FirstDependency: TPkgDependency): TFPList;
    function FindAutoInstallDependencyPath(ChildPackage: TLazPackage): TFPList;
    function FindAmbiguousUnits(APackage: TLazPackage;
                                FirstDependency: TPkgDependency;
                                var File1, File2: TPkgFile;
                                var ConflictPkg: TLazPackage): boolean;
    function FindFPCConflictUnit(APackage: TLazPackage;
                                FirstDependency: TPkgDependency;
                                const Directory: string;
                                OnFindFPCUnit: TFindFPCUnitEvent;
                                var File1: TPkgFile;
                                var ConflictPkg: TLazPackage): boolean;
    function FindFileInAllPackages(const TheFilename: string;
                                IgnoreDeleted, FindNewFile: boolean): TPkgFile;
    procedure FindPossibleOwnersOfUnit(const TheFilename: string;
                                       OwnerList: TFPList);
    function FindLowestPkgNodeByName(const PkgName: string): TAVLTreeNode;
    function FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
    function FindNodeOfDependency(Dependency: TPkgDependency;
                                  Flags: TFindPackageFlags): TAVLTreeNode;
    function FindOpenPackage(Dependency: TPkgDependency;
                             Flags: TFindPackageFlags): TLazPackage;
    function FindPackageWithFilename(const TheFilename: string): TLazPackage;
    function FindPackageWithID(PkgID: TLazPackageID): TLazPackage;
    function FindPackageWithIDMask(PkgIDMask: TLazPackageID): TLazPackage;
    function FindPackageProvidingName(FirstDependency: TPkgDependency;
                 const Name: string): TLazPackage;
    function FindDependencyRecursively(FirstDependency: TPkgDependency;
                                       PkgID: TLazPackageID): TPkgDependency;
    function FindDependencyRecursively(FirstDependency: TPkgDependency;
                                       const PkgName: string): TPkgDependency;
    function FindConflictRecursively(FirstDependency: TPkgDependency;
                                     PkgID: TLazPackageID): TPkgDependency;
    function FindUnit(StartPackage: TLazPackage; const TheUnitName: string;
                      WithRequiredPackages, IgnoreDeleted: boolean): TPkgFile;
    function FindUnitInAllPackages(const TheUnitName: string;
                                   IgnoreDeleted: boolean): TPkgFile;
    function PackageCanBeReplaced(OldPackage, NewPackage: TLazPackage): boolean;
    function PackageIsNeeded(APackage: TLazPackage): boolean;
    function PackageNameExists(const PkgName: string;
                               IgnorePackage: TLazPackage): boolean;
    procedure GetAllRequiredPackages(FirstDependency: TPkgDependency;
                                     out List: TFPList); // for single search use FindDependencyRecursively
    procedure GetConnectionsTree(FirstDependency: TPkgDependency;
                                 var PkgList: TFPList; var Tree: TPkgPairTree);
    function GetAutoCompilationOrder(APackage: TLazPackage;
                                     FirstDependency: TPkgDependency;
                                     SkipDesignTimePackages: boolean;
                                     Policies: TPackageUpdatePolicies): TFPList;
    function GetBrokenDependenciesWhenChangingPkgID(APackage: TLazPackage;
                         const NewName: string; NewVersion: TPkgVersion): TFPList;
    procedure GetPackagesChangedOnDisk(var ListOfPackages: TFPList);
    procedure CalculateTopologicalLevels;
    procedure SortDependencyListTopologically(
                   var FirstDependency: TPkgDependency; TopLevelFirst: boolean);
    procedure IterateAllComponentClasses(Event: TIterateComponentClassesEvent);
    procedure IterateComponentClasses(APackage: TLazPackage;
                               Event: TIterateComponentClassesEvent;
                               WithUsedPackages, WithRequiredPackages: boolean);
    procedure IteratePackages(Flags: TFindPackageFlags;
                              Event: TIteratePackagesEvent);
    procedure IteratePackagesSorted(Flags: TFindPackageFlags;
                                    Event: TIteratePackagesEvent);
    procedure MarkAllPackagesAsNotVisited;
    procedure MarkAllDependencies(MarkPackages: boolean;
                            AddMarkerFlags, RemoveMarkerFlags: TPkgMarkerFlags);
    procedure MarkAllRequiredPackages(FirstDependency: TPkgDependency);
    procedure MarkNeededPackages;
    procedure ConsistencyCheck;
  public
    // packages handling
    function CreateNewPackage(const Prefix: string): TLazPackage;
    procedure AddPackage(APackage: TLazPackage);
    procedure ReplacePackage(OldPackage, NewPackage: TLazPackage);
    procedure ClosePackage(APackage: TLazPackage);
    procedure CloseUnneededPackages;
    procedure ChangePackageID(APackage: TLazPackage;
                              const NewName: string; NewVersion: TPkgVersion;
                              RenameDependencies, RenameMacros: boolean);
    function SavePackageCompiledState(APackage: TLazPackage;
                  const CompilerFilename, CompilerParams: string;
                  Complete, MainPPUExists, ShowAbort: boolean): TModalResult;
    function LoadPackageCompiledState(APackage: TLazPackage;
                                IgnoreErrors, ShowAbort: boolean): TModalResult;
    function CheckCompileNeedDueToFPCUnits(TheOwner: TObject;
                          StateFileAge: longint): boolean;
    function CheckCompileNeedDueToDependencies(TheOwner: TObject;
                          FirstDependency: TPkgDependency;
                          SkipDesignTimePackages: boolean; StateFileAge: longint
                          ): TModalResult;
    function CheckIfPackageNeedsCompilation(APackage: TLazPackage;
                    const CompilerFilename, CompilerParams, SrcFilename: string;
                    SkipDesignTimePackages: boolean;
                    out NeedBuildAllFlag: boolean): TModalResult;
    function PreparePackageOutputDirectory(APackage: TLazPackage;
                                           CleanUp: boolean): TModalResult;
    function GetFallbackOutputDir(APackage: TLazPackage): string;
    function CheckAmbiguousPackageUnits(APackage: TLazPackage): TModalResult;
    function SavePackageMainSource(APackage: TLazPackage;
                     Flags: TPkgCompileFlags; ShowAbort: boolean): TModalResult;
    function CompileRequiredPackages(APackage: TLazPackage;
                                FirstDependency: TPkgDependency;
                                SkipDesignTimePackages: boolean;
                                Policies: TPackageUpdatePolicies): TModalResult;
    function CompilePackage(APackage: TLazPackage; Flags: TPkgCompileFlags;
                            ShowAbort: boolean): TModalResult;
    function ConvertPackageRSTFiles(APackage: TLazPackage): TModalResult;
    function WriteMakeFile(APackage: TLazPackage): TModalResult;
  public
    // installed packages
    FirstAutoInstallDependency: TPkgDependency;
    procedure LoadStaticBasePackages;
    procedure LoadAutoInstallPackages(PkgList: TStringList);
    procedure SortAutoInstallDependencies;
    function GetIDEInstallPackageOptions(FirstDependency: TPkgDependency;
                 var InheritedOptionStrings: TInheritedCompOptsStrings): string;
    function SaveAutoInstallConfig: TModalResult;// for the uses section
    function IsStaticBasePackage(PackageName: string): boolean;
    procedure FreeAutoInstallDependencies;
  public
    // registration
    procedure RegisterUnitHandler(const TheUnitName: string;
                                  RegisterProc: TRegisterProc);
    procedure RegisterComponentsHandler(const Page: string;
                                    ComponentClasses: array of TComponentClass);
    procedure RegistrationError(const Msg: string);
    procedure RegisterStaticBasePackages;
    procedure RegisterStaticPackage(APackage: TLazPackage;
                                    RegisterProc: TRegisterProc);
    procedure RegisterDefaultPackageComponent(const Page, AUnitName: ShortString;
                                              ComponentClass: TComponentClass);
    procedure CallRegisterProc(RegisterProc: TRegisterProc);
  public
    // dependency handling
    procedure AddDependencyToPackage(APackage: TLazPackage;
                                     Dependency: TPkgDependency);
    procedure AddDependencyToPackage(APackage, RequiredPackage: TLazPackage);
    procedure RemoveDependencyFromPackage(APackage: TLazPackage;
                         Dependency: TPkgDependency; AddToRemovedList: boolean);
    procedure ChangeDependency(Dependency, NewDependency: TPkgDependency);
    function OpenDependency(Dependency: TPkgDependency;
                            ShowAbort: boolean): TLoadPackageResult;
    procedure OpenInstalledDependency(Dependency: TPkgDependency;
                          InstallType: TPackageInstallType; var Quiet: boolean);
    procedure OpenRequiredDependencyList(FirstDependency: TPkgDependency);
    procedure MoveRequiredDependencyUp(ADependency: TPkgDependency);
    procedure MoveRequiredDependencyDown(ADependency: TPkgDependency);
  public
    // properties
    property AbortRegistration: boolean read FAbortRegistration
                                        write FAbortRegistration;
    property QuietRegistration: boolean read FQuietRegistration
                                        write FQuietRegistration;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;

    property FCLPackage: TLazPackage read FFCLPackage;
    property LCLBasePackage: TLazPackage read FLCLBasePackage;
    property LCLPackage: TLazPackage read FLCLPackage;
    property SynEditPackage: TLazPackage read FSynEditPackage;
    property LazControlsPackage: TLazPackage read FLazControlsPackage;
    property CodeToolsPackage: TLazPackage read FCodeToolsPackage;
    property IDEIntfPackage: TLazPackage read FIDEIntfPackage;
    property LazarusBasePackages: TFPList read FLazarusBasePackages;
    property DefaultPackage: TLazPackage read FDefaultPackage;// fall back package for buggy/obsoleted stuff

    property OnAddPackage: TPkgAddedEvent read FOnAddPackage write FOnAddPackage;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnChangePackageName: TPkgChangeNameEvent read FOnChangePackageName
                                                     write FOnChangePackageName;
    property OnDependencyModified: TDependencyModifiedEvent
                         read FOnDependencyModified write FOnDependencyModified;
    property OnDeletePackage: TPkgDeleteEvent read FOnDeletePackage
                                              write FOnDeletePackage;
    property OnEndUpdate: TEndUpdateEvent read FOnEndUpdate write FOnEndUpdate;
    property OnDeleteAmbiguousFiles: TPkgDeleteAmbiguousFiles
                     read FOnDeleteAmbiguousFiles write FOnDeleteAmbiguousFiles;
    property OnTranslatePackage: TPkgTranslate read FOnTranslatePackage
                                                   write FOnTranslatePackage;
    property OnUninstallPackage: TPkgUninstall read FOnUninstallPackage
                                               write FOnUninstallPackage;
    property Packages[Index: integer]: TLazPackage read GetPackages; default; // see Count for the number
    property RegistrationFile: TPkgFile read FRegistrationFile;
    property RegistrationPackage: TLazPackage read FRegistrationPackage
                                              write SetRegistrationPackage;
    property RegistrationUnitName: string read FRegistrationUnitName;
    property UpdateLock: integer read FUpdateLock;
  end;
  
var
  PackageGraph: TLazPackageGraph = nil;

function ExtractFPCParamsForBuildAll(const CompParams: string): string;
function ExtractSearchPathsFromFPCParams(const CompParams: string;
  CreateReduced: boolean = false;
  BaseDir: string = ''; MakeRelative: boolean = false): TStringList;

implementation

procedure RegisterCustomIDEComponent(const Page, AUnitName: ShortString;
  ComponentClass: TComponentClass);
begin
  PackageGraph.RegisterDefaultPackageComponent(Page,AUnitName,ComponentClass);
end;

procedure RegisterComponentsGlobalHandler(const Page: string;
  ComponentClasses: array of TComponentClass);
begin
  PackageGraph.RegisterComponentsHandler(Page,ComponentClasses);
end;

procedure RegisterNoIconGlobalHandler(
  ComponentClasses: array of TComponentClass);
begin
  PackageGraph.RegisterComponentsHandler('',ComponentClasses);
end;

function ExtractFPCParamsForBuildAll(const CompParams: string): string;
{ Some compiler flags require a clean build -B, because the compiler
  does not recompile/update some ppu itself.
  Remove all flags that do not require build all:
  -l -F -B -e -i -o -s -v }
var
  EndPos: Integer;
  StartPos: integer;
begin
  Result:=CompParams;
  EndPos:=1;
  while ReadNextFPCParameter(Result,EndPos,StartPos) do begin
    if (Result[StartPos]='-') and (StartPos<length(Result)) then begin
      case Result[StartPos+1] of
      'l','F','B','e','i','o','s','v':
        begin
          while (StartPos>1) and (Result[StartPos-1] in [' ',#9]) do
            dec(StartPos);
          //DebugLn(['TLazPackageGraph.ExtractFPCParamsForBuildAll Removing: ',copy(Result,StartPos,EndPos-StartPos)]);
          while (EndPos<=length(Result)) and (Result[EndPos] in [' ',#9]) do
            inc(EndPos);
          System.Delete(Result,StartPos,EndPos-StartPos);
          EndPos:=StartPos;
        end;
      end;
    end;
  end;
end;

function ExtractSearchPathsFromFPCParams(const CompParams: string;
  CreateReduced: boolean; BaseDir: string; MakeRelative: boolean): TStringList;
var
  AllPaths: TStringList;
  EndPos: Integer;
  StartPos: integer;
  Path: String;
  Reduced: String;
  i: Integer;

  procedure AddSearchPath(Typ: string);
  begin
    AllPaths.Values[Typ]:=MergeSearchPaths(AllPaths.Values[Typ],Path);
  end;

begin
  Result:=TStringList.Create;
  Reduced:=CompParams;
  AllPaths:=Result;
  EndPos:=1;
  while ReadNextFPCParameter(Reduced,EndPos,StartPos) do begin
    if (Reduced[StartPos]='-') and (StartPos<length(Reduced)) then begin
      case Reduced[StartPos+1] of
      'F':
        if StartPos<length(Reduced)-1 then begin
          Path:=copy(Reduced,StartPos+3,EndPos-StartPos-3);
          if (Path<>'') and (Path[1] in ['''','"']) then
            Path:=AnsiDequotedStr(Path,Path[1]);
          case Reduced[StartPos+2] of
          'u': AddSearchPath('UnitPath');
          'U': AllPaths.Values['UnitOutputDir']:=Path;
          'i': AddSearchPath('IncPath');
          'o': AddSearchPath('ObjectPath');
          'l': AddSearchPath('LibPath');
          end;
          while (EndPos<=length(Reduced)) and (Reduced[EndPos] in [' ',#9]) do
            inc(EndPos);
          System.Delete(Reduced,StartPos,EndPos-StartPos);
          EndPos:=StartPos;
        end;
      end;
    end;
  end;
  if BaseDir<>'' then begin
    for i:=0 to AllPaths.Count-1 do begin
      Path:=AllPaths.ValueFromIndex[i];
      if MakeRelative then
        AllPaths[i]:=AllPaths.Names[i]+'='+CreateRelativeSearchPath(Path,BaseDir)
      else
        AllPaths[i]:=AllPaths.Names[i]+'='+CreateAbsoluteSearchPath(Path,BaseDir);
    end;
  end;
  if CreateReduced then
    AllPaths.Values['Reduced']:=Reduced;
end;

{ TLazPackageGraph }

procedure TLazPackageGraph.DoDependencyChanged(Dependency: TPkgDependency);
begin
  fChanged:=true;
  if Assigned(OnDependencyModified) then OnDependencyModified(Dependency);
end;

function TLazPackageGraph.GetPackages(Index: integer): TLazPackage;
begin
  Result:=TLazPackage(FItems[Index]);
end;

procedure TLazPackageGraph.SetRegistrationPackage(const AValue: TLazPackage);
begin
  if FRegistrationPackage=AValue then exit;
  FRegistrationPackage:=AValue;
  AbortRegistration:=false;
  LazarusPackageIntf.RegisterUnitProc:=@RegisterUnitHandler;
  RegisterComponentsProc:=@RegisterComponentsGlobalHandler;
  RegisterNoIconProc:=@RegisterNoIconGlobalHandler;
end;

procedure TLazPackageGraph.UpdateBrokenDependenciesToPackage(
  APackage: TLazPackage);
var
  ANode: TAVLTreeNode;
  Dependency: TPkgDependency;
begin
  BeginUpdate(false);
  ANode:=FindLowestPkgDependencyNodeWithName(APackage.Name);
  while ANode<>nil do begin
    Dependency:=TPkgDependency(ANode.Data);
    if (Dependency.LoadPackageResult<>lprSuccess)
    and Dependency.IsCompatible(APackage) then begin
      Dependency.LoadPackageResult:=lprUndefined;
      OpenDependency(Dependency,false);
    end;
    ANode:=FindNextPkgDependencyNodeWithSameName(ANode);
  end;
  EndUpdate;
end;

function TLazPackageGraph.OpenDependencyWithPackageLink(
  Dependency: TPkgDependency; PkgLink: TPackageLink;
  ShowAbort: boolean): TModalResult;
var
  AFilename: String;
  NewPackage: TLazPackage;
  XMLConfig: TXMLConfig;
  Code: TCodeBuffer;
  OldPackage: TLazPackage;
begin
  NewPackage:=nil;
  XMLConfig:=nil;
  BeginUpdate(false);
  try
    AFilename:=PkgLink.GetEffectiveFilename;
    if not FileExistsUTF8(AFilename) then begin
      DebugLn('invalid Package Link: file "'+AFilename+'" does not exist.');
      PkgLink.FileDateValid:=false;
      exit(mrCancel);
    end;
    try
      PkgLink.FileDate:=FileDateToDateTime(FileAgeUTF8(AFilename));
      PkgLink.FileDateValid:=true;
      XMLConfig:=TXMLConfig.Create(nil);
      NewPackage:=TLazPackage.Create;
      NewPackage.Filename:=AFilename;
      Result:=LoadXMLConfigFromCodeBuffer(AFilename,XMLConfig,
                         Code,[lbfUpdateFromDisk,lbfRevert],ShowAbort);
      if Result<>mrOk then exit;
      NewPackage.LoadFromXMLConfig(XMLConfig,'Package/');
      NewPackage.LPKSource:=Code;
    except
      on E: Exception do begin
        DebugLn('unable to read file "'+AFilename+'" ',E.Message);
        Result:=mrCancel;
        exit;
      end;
    end;
    if not NewPackage.MakeSense then begin
      DebugLn('invalid Package file "'+AFilename+'".');
      exit(mrCancel);
    end;
    if SysUtils.CompareText(PkgLink.Name,NewPackage.Name)<>0 then exit;
    // ok
    Result:=mrOk;
    Dependency.RequiredPackage:=NewPackage;
    Dependency.LoadPackageResult:=lprSuccess;
    OldPackage:=FindAPackageWithName(NewPackage.Name,NewPackage);
    if OldPackage=nil then
      AddPackage(NewPackage)
    else
      ReplacePackage(OldPackage,NewPackage);
  finally
    if Result<>mrOk then
      NewPackage.Free;
    EndUpdate;
    FreeAndNil(XMLConfig);
  end;
end;

function TLazPackageGraph.DeleteAmbiguousFiles(const Filename: string
  ): TModalResult;
begin
  if Assigned(OnDeleteAmbiguousFiles) then
    Result:=OnDeleteAmbiguousFiles(Filename)
  else
    Result:=mrOk;
end;

procedure TLazPackageGraph.AddMessage(const Msg, Directory: string);
begin
  if Assigned(IDEMessagesWindow) then
    IDEMessagesWindow.AddMsg(Msg, Directory,-1)
  else
    DebugLn(['TLazPackageGraph.AddMessage Msg="',Msg,'" Directory="',Directory,'"']);
end;

function TLazPackageGraph.OutputDirectoryIsWritable(APackage: TLazPackage;
  Directory: string; Verbose: boolean): boolean;
begin
  Result:=false;
  //debugln(['TLazPackageGraph.OutputDirectoryIsWritable ',Directory]);
  if not FilenameIsAbsolute(Directory) then
    exit;
  Directory:=ChompPathDelim(Directory);
  if not DirPathExistsCached(Directory) then begin
    // the directory does not exist => try creating it
    if not ForceDirectoriesUTF8(Directory) then begin
      if Verbose then begin
        IDEMessageDialog(lisPkgMangUnableToCreateDirectory,
          Format(lisPkgMangUnableToCreateOutputDirectoryForPackage, ['"',
            Directory, '"', #13, APackage.IDAsString]),
          mtError,[mbCancel]);
      end;
      debugln(['TLazPackageGraph.OutputDirectoryIsWritable unable to create directory "',Directory,'"']);
      exit;
    end;
    Result:=true;
  end else
    Result:=DirectoryIsWritableCached(Directory);
end;

constructor TLazPackageGraph.Create;
begin
  OnGetAllRequiredPackages:=@GetAllRequiredPackages;
  FTree:=TAVLTree.Create(@CompareLazPackageID);
  FItems:=TFPList.Create;
  FLazarusBasePackages:=TFPList.Create;
  if GlobalMacroList<>nil then begin
    GlobalMacroList.Add(TTransferMacro.Create('PkgDir','',
      lisPkgMacroPackageDirectoryParameterIsPackageID, @MacroFunctionPkgDir, []));
    GlobalMacroList.Add(TTransferMacro.Create('PkgSrcPath','',
      lisPkgMacroPackageSourceSearchPathParameterIsPackageID,
      @MacroFunctionPkgSrcPath,[]));
    GlobalMacroList.Add(TTransferMacro.Create('PkgUnitPath','',
      lisPkgMacroPackageUnitSearchPathParameterIsPackageID,
      @MacroFunctionPkgUnitPath,[]));
    GlobalMacroList.Add(TTransferMacro.Create('PkgIncPath','',
      lisPkgMacroPackageIncludeFilesSearchPathParameterIsPackageID,
      @MacroFunctionPkgIncPath,[]));
  end;
end;

destructor TLazPackageGraph.Destroy;
begin
  if LazarusPackageIntf.RegisterUnitProc=@RegisterUnitHandler then
    LazarusPackageIntf.RegisterUnitProc:=nil;
  if RegisterComponentsProc=@RegisterComponentsGlobalHandler then
    RegisterComponentsProc:=nil;
  if RegisterNoIconProc=@RegisterNoIconGlobalHandler then
    RegisterNoIconProc:=nil;
  if OnGetAllRequiredPackages=@GetAllRequiredPackages then
    OnGetAllRequiredPackages:=nil;
  Clear;
  FreeAndNil(FDefaultPackage);
  FreeAndNil(FLazarusBasePackages);
  FreeAndNil(FItems);
  FreeAndNil(FTree);
  inherited Destroy;
end;

procedure TLazPackageGraph.Clear;
var
  i: Integer;
begin
  FLazarusBasePackages.Clear;
  for i:=FItems.Count-1 downto 0 do Delete(i);
end;

procedure TLazPackageGraph.Delete(Index: integer);
var
  CurPkg: TLazPackage;
begin
  BeginUpdate(true);
  CurPkg:=Packages[Index];
  CurPkg.Flags:=CurPkg.Flags+[lpfDestroying];
  CurPkg.DefineTemplates.Active:=false;

  if Assigned(OnDeletePackage) then OnDeletePackage(CurPkg);

  if CurPkg=FCLPackage then
    FFCLPackage:=nil
  else if CurPkg=LCLBasePackage then
    FLCLBasePackage:=nil
  else if CurPkg=LCLPackage then
    FLCLPackage:=nil
  else if CurPkg=IDEIntfPackage then
    FIDEIntfPackage:=nil
  else if CurPkg=SynEditPackage then
    FSynEditPackage:=nil
  else if CurPkg=LazControlsPackage then
    FLazControlsPackage:=nil
  else if CurPkg=CodeToolsPackage then
    FCodeToolsPackage:=nil;
  FLazarusBasePackages.Remove(CurPkg);

  FItems.Delete(Index);
  FTree.Remove(CurPkg);
  CurPkg.Free;
  EndUpdate;
end;

function TLazPackageGraph.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TLazPackageGraph.BeginUpdate(Change: boolean);
begin
  inc(FUpdateLock);
  if FUpdateLock=1 then begin
    fChanged:=Change;
    if Assigned(OnBeginUpdate) then OnBeginUpdate(Self);
  end else
    fChanged:=fChanged or Change;
end;

procedure TLazPackageGraph.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseException('TLazPackageGraph.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if Assigned(OnEndUpdate) then OnEndUpdate(Self,fChanged);
  end;
end;

function TLazPackageGraph.Updating: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TLazPackageGraph.RebuildDefineTemplates;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Packages[i].DefineTemplates.AllChanged;
end;

function TLazPackageGraph.MacroFunctionPkgDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  APackage: TLazPackage;
begin
  if GetPackageFromMacroParameter(s,APackage) then
    Result:=APackage.Directory
  else
    Result:='';
end;

function TLazPackageGraph.MacroFunctionPkgSrcPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  APackage: TLazPackage;
begin
  if GetPackageFromMacroParameter(s,APackage) then
    Result:=APackage.SourceDirectories.CreateSearchPathFromAllFiles
  else
    Result:='';
end;

function TLazPackageGraph.MacroFunctionPkgUnitPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  APackage: TLazPackage;
begin
  if GetPackageFromMacroParameter(s,APackage) then
    Result:=APackage.GetUnitPath(false)
  else
    Result:='';
end;

function TLazPackageGraph.MacroFunctionPkgIncPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
var
  APackage: TLazPackage;
begin
  if GetPackageFromMacroParameter(s,APackage) then
    Result:=APackage.GetIncludePath(false)
  else
    Result:='';
end;

function TLazPackageGraph.MacroFunctionCTPkgDir(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=GetPackageFromMacroParameter(FuncData^.Param,APackage);
  if Result then
    FuncData^.Result:=APackage.Directory;
end;

function TLazPackageGraph.MacroFunctionCTPkgSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=GetPackageFromMacroParameter(FuncData^.Param,APackage);
  if Result then
    FuncData^.Result:=APackage.GetUnitPath(false)+';'+APackage.GetSrcPath(false);
end;

function TLazPackageGraph.MacroFunctionCTPkgUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=GetPackageFromMacroParameter(FuncData^.Param,APackage);
  if Result then
    FuncData^.Result:=APackage.GetUnitPath(false);
end;

function TLazPackageGraph.MacroFunctionCTPkgIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  Result:=GetPackageFromMacroParameter(FuncData^.Param,APackage);
  if Result then
    FuncData^.Result:=APackage.GetIncludePath(false);
end;

function TLazPackageGraph.MacroFunctionCTPkgName(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=GetIdentifier(PChar(FuncData^.Param));
  Result:=true;
end;

function TLazPackageGraph.GetPackageFromMacroParameter(const TheID: string;
  out APackage: TLazPackage): boolean;
var
  PkgID: TLazPackageID;
begin
  PkgID:=TLazPackageID.Create;
  if PkgID.StringToID(TheID) then begin
    APackage:=FindPackageWithIDMask(PkgID);
    if APackage=nil then begin
      DebugLn('WARNING: TLazPackageGraph.GetPackageFromMacroParameter unknown package id "',TheID,'" PkgID.IDAsString="',PkgID.IDAsString,'"');
    end;
  end else begin
    APackage:=nil;
    DebugLn('WARNING: TLazPackageGraph.GetPackageFromMacroParameter invalid package id "',TheID,'"');
  end;
  PkgID.Free;
  Result:=APackage<>nil;
end;

function TLazPackageGraph.FindLowestPkgNodeByName(const PkgName: string
  ): TAVLTreeNode;
var
  PriorNode: TAVLTreeNode;
begin
  Result:=nil;
  if PkgName='' then exit;
  Result:=FTree.FindKey(PChar(PkgName),@CompareNameWithPackageID);
  while Result<>nil do begin
    PriorNode:=FTree.FindPrecessor(Result);
    if (PriorNode=nil)
    or (SysUtils.CompareText(PkgName,TLazPackage(PriorNode.Data).Name)<>0) then
      break;
    Result:=PriorNode;
  end;
end;

function TLazPackageGraph.FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
var
  NextNode: TAVLTreeNode;
begin
  Result:=nil;
  if ANode=nil then exit;
  NextNode:=FTree.FindSuccessor(ANode);
  if (NextNode=nil)
  or (SysUtils.CompareText(TLazPackage(ANode.Data).Name,
                      TLazPackage(NextNode.Data).Name)<>0)
  then exit;
  Result:=NextNode;
end;

function TLazPackageGraph.FindNodeOfDependency(Dependency: TPkgDependency;
  Flags: TFindPackageFlags): TAVLTreeNode;
var
  CurPkg: TLazPackage;
begin
  // search in all packages with the same name
  Result:=FindLowestPkgNodeByName(Dependency.PackageName);
  while Result<>nil do begin
    CurPkg:=TLazPackage(Result.Data);
    // check version
    if (not (fpfIgnoreVersion in Flags))
    and (not Dependency.IsCompatible(CurPkg)) then begin
      Result:=FindNextSameName(Result);
      continue;
    end;
    // check loaded packages
    if (fpfSearchInLoadedPkgs in Flags) then exit;
    // check installed packages
    if (fpfSearchInInstalledPckgs in Flags)
    and (CurPkg.Installed<>pitNope) then exit;
    // check autoinstall packages
    if (fpfSearchInAutoInstallPckgs in Flags)
    and (CurPkg.AutoInstall<>pitNope) then exit;
    // check packages with opened editor
    if (fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil) then exit;
    // search next package node with same name
    Result:=FindNextSameName(Result);
  end;
end;

function TLazPackageGraph.FindOpenPackage(Dependency: TPkgDependency;
  Flags: TFindPackageFlags): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FindNodeOfDependency(Dependency,Flags);
  if ANode<>nil then
    Result:=TLazPackage(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageGraph.FindAPackageWithName(const PkgName: string;
  IgnorePackage: TLazPackage): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  Result:=nil;
  ANode:=FindLowestPkgNodeByName(PkgName);
  if ANode<>nil then begin
    Result:=TLazPackage(ANode.Data);
    if Result=IgnorePackage then begin
      Result:=nil;
      ANode:=FindNextSameName(ANode);
      if ANode<>nil then
        Result:=TLazPackage(ANode.Data);
    end;
  end;
end;

function TLazPackageGraph.FindPackageWithID(PkgID: TLazPackageID): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FTree.Find(PkgID);
  if ANode<>nil then
    Result:=TLazPackage(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageGraph.FindPackageWithIDMask(PkgIDMask: TLazPackageID
  ): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FTree.FindKey(PkgIDMask,@ComparePkgIDMaskWithPackageID);
  if ANode<>nil then
    Result:=TLazPackage(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageGraph.FindPackageProvidingName(
  FirstDependency: TPkgDependency;
  const Name: string): TLazPackage;
  
  function Search(ADependency: TPkgDependency; out Found: TLazPackage
    ): boolean;
  begin
    Found:=nil;
    while ADependency<>nil do begin
      Found:=ADependency.RequiredPackage;
      //DebugLn(['Search ',Found.Name,' ',Found.ProvidesPackage(Name),' "',Found.Provides.Text,'"']);
      if (Found<>nil) and (not (lpfVisited in Found.Flags)) then begin
        Found.Flags:=Found.Flags+[lpfVisited];
        if Found.ProvidesPackage(Name)
        or Search(Found.FirstRequiredDependency,Found) then
          exit(true);
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
    Found:=nil;
    Result:=false;
  end;
  
begin
  MarkAllPackagesAsNotVisited;
  Search(FirstDependency,Result);
end;

function TLazPackageGraph.FindDependencyRecursively(
  FirstDependency: TPkgDependency; PkgID: TLazPackageID): TPkgDependency;
// returns one compatible dependency for PkgID

  function Find(CurDependency: TPkgDependency): TPkgDependency;
  var
    RequiredPackage: TLazPackage;
  begin
    while CurDependency<>nil do begin
      if CurDependency.IsCompatible(PkgID) then begin
        Result:=CurDependency;
        exit;
      end;
      if CurDependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=CurDependency.RequiredPackage;
        if (not (lpfVisited in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          Result:=Find(RequiredPackage.FirstRequiredDependency);
          if Result<>nil then exit;
        end;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    Result:=nil;
  end;

begin
  MarkAllPackagesAsNotVisited;
  Result:=Find(FirstDependency);
end;

function TLazPackageGraph.FindDependencyRecursively(
  FirstDependency: TPkgDependency; const PkgName: string): TPkgDependency;
// returns one compatible dependency for PkgName

  function Find(CurDependency: TPkgDependency): TPkgDependency;
  var
    RequiredPackage: TLazPackage;
  begin
    while CurDependency<>nil do begin
      if SysUtils.CompareText(CurDependency.PackageName,PkgName)=0 then begin
        Result:=CurDependency;
        exit;
      end;
      if CurDependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=CurDependency.RequiredPackage;
        if (not (lpfVisited in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          Result:=Find(RequiredPackage.FirstRequiredDependency);
          if Result<>nil then exit;
        end;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    Result:=nil;
  end;

begin
  MarkAllPackagesAsNotVisited;
  Result:=Find(FirstDependency);
end;

function TLazPackageGraph.FindConflictRecursively(
  FirstDependency: TPkgDependency; PkgID: TLazPackageID): TPkgDependency;
// returns one conflicting dependency for PkgID

  function Find(CurDependency: TPkgDependency): TPkgDependency;
  var
    RequiredPackage: TLazPackage;
  begin
    while CurDependency<>nil do begin
      if (SysUtils.CompareText(CurDependency.PackageName,PkgID.Name)=0)
      and (not CurDependency.IsCompatible(PkgID)) then begin
        Result:=CurDependency;
        exit;
      end;
      if CurDependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=CurDependency.RequiredPackage;
        if (not (lpfVisited in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          Result:=Find(RequiredPackage.FirstRequiredDependency);
          if Result<>nil then exit;
        end;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    Result:=nil;
  end;

begin
  MarkAllPackagesAsNotVisited;
  Result:=Find(FirstDependency);
end;

function TLazPackageGraph.FindUnit(StartPackage: TLazPackage;
  const TheUnitName: string;
  WithRequiredPackages, IgnoreDeleted: boolean): TPkgFile;
var
  ADependency: TPkgDependency;
  ARequiredPackage: TLazPackage;
begin
  Result:=StartPackage.FindUnit(TheUnitName,IgnoreDeleted);
  if Result<>nil then exit;
  // search also in all required packages
  if WithRequiredPackages then begin
    ADependency:=StartPackage.FirstRequiredDependency;
    while ADependency<>nil do begin
      ARequiredPackage:=FindOpenPackage(ADependency,[fpfSearchInInstalledPckgs]);
      if ARequiredPackage<>nil then begin
        Result:=ARequiredPackage.FindUnit(TheUnitName,IgnoreDeleted);
        if Result<>nil then exit;
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
  end;
end;

function TLazPackageGraph.FindUnitInAllPackages(
  const TheUnitName: string; IgnoreDeleted: boolean): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Result:=FindUnit(Packages[i],TheUnitName,false,IgnoreDeleted);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TLazPackageGraph.FindFileInAllPackages(const TheFilename: string;
  IgnoreDeleted, FindNewFile: boolean): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Result:=Packages[i].FindPkgFile(TheFilename,IgnoreDeleted,
                                    FindNewFile);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

procedure TLazPackageGraph.FindPossibleOwnersOfUnit(const TheFilename: string;
  OwnerList: TFPList);
var
  Cnt: Integer;
  i: Integer;
  APackage: TLazPackage;
  SrcDir: String;

  function SrcDirInPath(Dirs: String): boolean;
  begin
    Result:=FindPathInSearchPath(PChar(SrcDir),length(SrcDir),
                                 PChar(Dirs),length(Dirs))<>nil;
  end;

begin
  if not FilenameIsAbsolute(TheFilename) then exit;
  Cnt:=Count;
  SrcDir:=ExtractFilePath(TheFilename);
  for i:=0 to Cnt-1 do begin
    APackage:=Packages[i];
    if APackage.IsVirtual and (not APackage.AutoCreated) then continue;
    // source directories + unit path without inherited paths + base directory + output directory
    if SrcDirInPath(APackage.CompilerOptions.GetParsedPath(pcosUnitPath,icoNone,false))
    or SrcDirInPath(APackage.SourceDirectories.CreateSearchPathFromAllFiles)
    or SrcDirInPath(APackage.GetOutputDirectory)
    or SrcDirInPath(APackage.Directory)
    then
      OwnerList.Add(APackage);
  end;
end;

function TLazPackageGraph.FindPackageWithFilename(const TheFilename: string
  ): TLazPackage;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Result:=Packages[i];
    if Result.IsVirtual then continue;
    if CompareFilenames(TheFilename,Result.Filename)=0 then
      exit;
  end;
  Result:=nil;
end;

function TLazPackageGraph.CreateUniqueUnitName(const Prefix: string): string;
begin
  Result:=Prefix;
  while FindUnitInAllPackages(Result,false)<>nil do
    Result:=CreateNextIdentifier(Result);
end;

function TLazPackageGraph.PackageNameExists(const PkgName: string;
  IgnorePackage: TLazPackage): boolean;
var
  ANode: TAVLTreeNode;
begin
  Result:=false;
  if PkgName<>'' then begin
    ANode:=FindLowestPkgNodeByName(PkgName);
    if (ANode<>nil) and (IgnorePackage=TLazPackage(ANode.Data)) then
      ANode:=FindNextSameName(ANode);
    Result:=ANode<>nil;
  end;
end;

function TLazPackageGraph.DependencyExists(Dependency: TPkgDependency;
  Flags: TFindPackageFlags): boolean;
begin
  Result:=true;
  if FindNodeOfDependency(Dependency,Flags)<>nil then exit;
  if FindAPackageWithName(Dependency.PackageName,nil)=nil then begin
    // no package with same name open
    // -> try package links
    if fpfSearchInPkgLinks in Flags then
      if PkgLinks.FindLinkWithDependency(Dependency)<>nil then exit;
  end else begin
    // there is already a package with this name open, but the wrong version
  end;
  Result:=false;
end;

function TLazPackageGraph.CreateUniquePkgName(const Prefix: string;
  IgnorePackage: TLazPackage): string;
var
  i: Integer;
begin
  // try Prefix alone
  if not PackageNameExists(Prefix,IgnorePackage) then begin
    Result:=Prefix;
  end else begin
    // try Prefix + number
    i:=1;
    while PackageNameExists(Prefix+IntToStr(i),IgnorePackage) do inc(i);
    Result:=Prefix+IntToStr(i);
  end;
end;

function TLazPackageGraph.CreateNewPackage(const Prefix: string): TLazPackage;
begin
  BeginUpdate(true);
  Result:=TLazPackage.Create;
  Result.Name:=CreateUniquePkgName(Prefix,nil);
  AddPackage(Result);
  EndUpdate;
end;

procedure TLazPackageGraph.ConsistencyCheck;
begin
  CheckList(FItems,true,true,true);
end;

procedure TLazPackageGraph.RegisterUnitHandler(const TheUnitName: string;
  RegisterProc: TRegisterProc);
begin
  if AbortRegistration then exit;

  ErrorMsg:='';
  FRegistrationFile:=nil;
  FRegistrationUnitName:='';

  // check package
  if FRegistrationPackage=nil then begin
    RegistrationError('Unit: '+TheUnitName);
    exit;
  end;
  try
    // check unitname
    FRegistrationUnitName:=TheUnitName;
    if not IsValidIdent(FRegistrationUnitName) then begin
      RegistrationError(Format(lisPkgSysInvalidUnitname, [FRegistrationUnitName]
        ));
      exit;
    end;
    // check unit file
    FRegistrationFile:=FindUnit(FRegistrationPackage,FRegistrationUnitName,true,true);
    if FRegistrationFile=nil then begin
      if not (FRegistrationPackage.Missing) then begin
        // lpk exists, but file is missing => warn
        FRegistrationFile:=
          FRegistrationPackage.FindUnit(FRegistrationUnitName,false);
        if FRegistrationFile=nil then begin
          RegistrationError(Format(
            lisPkgSysUnitWasNotFoundInTheLpkFileProbablyThisLpkFileWasN, [
            FRegistrationUnitName, #13]));
        end else begin
          if not (pffReportedAsRemoved in FRegistrationFile.Flags) then begin
            RegistrationError(
              Format(lisPkgSysUnitWasRemovedFromPackageLpk, [
                FRegistrationUnitName]));
            FRegistrationFile.Flags:=FRegistrationFile.Flags+[pffReportedAsRemoved];
          end;
        end;
      end;
      exit;
    end;
    CallRegisterProc(RegisterProc);
    // clean up
  finally
    FRegistrationUnitName:='';
    FRegistrationFile:=nil;
  end;
end;

procedure TLazPackageGraph.RegisterComponentsHandler(const Page: string;
  ComponentClasses: array of TComponentClass);
var
  i: integer;
  CurComponent: TComponentClass;
  NewPkgComponent: TPkgComponent;
  CurClassname: string;
begin
  {$IFDEF IDE_MEM_CHECK}
  CheckHeap('TLazPackageGraph.RegisterComponentsHandler Page='+Page);
  {$ENDIF}
  if AbortRegistration or (Low(ComponentClasses)>High(ComponentClasses)) then
    exit;

  ErrorMsg:='';

  // check package
  if FRegistrationPackage=nil then begin
    RegistrationError('');
    exit;
  end;
  // check unit file
  if FRegistrationFile=nil then begin
    RegistrationError(lisPkgSysCanNotRegisterComponentsWithoutUnit);
    exit;
  end;
  // register components
  for i:=Low(ComponentClasses) to High(ComponentClasses) do begin
    CurComponent:=ComponentClasses[i];
    if (CurComponent=nil) then continue;
    {$IFNDEF StopOnRegError}
    try
    {$ENDIF}
      CurClassname:=CurComponent.Classname;
      if not IsValidIdent(CurClassname) then begin
        RegistrationError(lisPkgSysInvalidComponentClass);
        continue;
      end;
    {$IFNDEF StopOnRegError}
    except
      on E: Exception do begin
        RegistrationError(E.Message);
        continue;
      end;
    end;
    {$ENDIF}
    if (IDEComponentPalette<>nil)
    and (IDEComponentPalette.FindComponent(CurClassname)<>nil) then begin
      RegistrationError(
        Format(lisPkgSysComponentClassAlreadyDefined, ['"',
          CurComponent.ClassName, '"']));
    end;
    if AbortRegistration then exit;
    // add the component to the package owning the file
    // (e.g. a designtime package can register units of a runtime packages)
    NewPkgComponent:=
      FRegistrationFile.LazPackage.AddComponent(FRegistrationFile,Page,CurComponent);
    //debugln('TLazPackageGraph.RegisterComponentsHandler Page="',Page,'" CurComponent=',CurComponent.ClassName,' FRegistrationFile=',FRegistrationFile.Filename);
    if IDEComponentPalette<>nil then
      IDEComponentPalette.AddComponent(NewPkgComponent);
  end;
end;

procedure TLazPackageGraph.RegistrationError(const Msg: string);
var
  DlgResult: Integer;
  IgnoreAll: Integer;
begin
  // create nice and useful error message

  // current registration package
  if FRegistrationPackage=nil then begin
    ErrorMsg:=lisPkgSysRegisterUnitWasCalledButNoPackageIsRegistering;
  end else begin
    ErrorMsg:='Package: "'+FRegistrationPackage.IDAsString+'"';
    // current unitname
    if FRegistrationUnitName<>'' then
      ErrorMsg:=Format(lisPkgSysUnitName, [ErrorMsg, #13, '"',
        FRegistrationUnitName, '"']);
    // current file
    if FRegistrationFile<>nil then
      ErrorMsg:=Format(lisPkgSysFileName, [ErrorMsg, #13, '"',
        FRegistrationFile.Filename, '"']);
  end;
  // append message
  if Msg<>'' then
    ErrorMsg:=ErrorMsg+#13#13+Msg;
  debugln(['TLazPackageGraph.RegistrationError ',dbgstr(ErrorMsg)]);

  if AbortRegistration or QuietRegistration then exit;

  // tell user
  IgnoreAll:=mrLast+1;
  DlgResult:=IDEQuestionDialog(lisPkgSysPackageRegistrationError,
               ErrorMsg, mtError, [mrIgnore, IgnoreAll, lisIgnoreAll, mrAbort]);
  if DlgResult=IgnoreAll then
    QuietRegistration:=true;
  if DlgResult=mrAbort then
    AbortRegistration:=true;
end;

function TLazPackageGraph.CreateFCLPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='FCL';
    Filename:=SetDirSeparators('$(FPCSrcDir)/');
    Version.SetValues(1,0,0,0);
    Author:='FPC team';
    License:='LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysTheFCLFreePascalComponentLibraryProvidesTheBase;
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    Translated:=SystemLanguageID1;
    AddToProjectUsesSection:=false;

    // add lazarus registration unit path
    UsageOptions.UnitPath:=SetDirSeparators(
      '$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)');

    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // add registering units
    AddFile(SetDirSeparators('packages/fcl-db/src/base/db.pas'),'DB',pftUnit,[],cpBase);
    AddFile(SetDirSeparators('packages/fcl-process/src/process.pp'),'Process',pftUnit,[],cpBase);
    AddFile(SetDirSeparators('packages/fcl-process/src/simpleipc.pp'),'SimpleIPC',pftUnit,[],cpBase);
    AddFile(SetDirSeparators('packages/fcl-xml/src/xmlconf.pp'),'XMLConf',pftUnit,[],cpBase);
    AddFile(SetDirSeparators('packages/fcl-base/src/eventlog.pp'),'EventLog',pftUnit,[],cpBase);

    SetAllComponentPriorities(FCLCompPriority);

    // use the packager/units/lazaruspackageintf.o file as indicator,
    // if FCL has been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)/lazaruspackageintf.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateLCLBasePackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='LCLBase';
    Filename:=SetDirSeparators('$(LazarusDir)/lcl');
    Version.SetValues(1,0,0,0);
    Author:='Lazarus';
    License:='modified LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysTheLCLLazarusComponentLibraryContainsAllBase;
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.OtherUnitFiles:='$(LazarusDir)/lcl;$(LazarusDir)/lcl/widgetset/';
    CompilerOptions.UnitOutputDirectory:='$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/';
    POOutputDirectory:='languages';
    Translated:=SystemLanguageID1;
    LazDocPaths:=SetDirSeparators('$(LazarusDir)/docs/xml/lcl');
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(FCLPackage.CreateDependencyWithOwner(Result,true));

    // register files
    {$I pkgfileslcl.inc}

    SetAllComponentPriorities(LCLCompPriority);

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
       '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)');
    // add include path
    CompilerOptions.IncludePath:=SetDirSeparators(
      '$(LazarusDir)/lcl/include');
    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // use the lcl/units/$(TargetCPU)-$(TargetOS)/alllclunits.o
    // file as indicator, if LCL has been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/alllclunits.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateLCLPackage: TLazPackage;
var
  Macro: TLazBuildMacro;
  lp: TLCLPlatform;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='LCL';
    Filename:=SetDirSeparators('$(LazarusDir)/lcl/interfaces');
    Version.SetValues(1,0,0,0);
    Author:='Lazarus';
    License:='modified LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysTheLCLLazarusComponentLibraryContainsAllBase;
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.OtherUnitFiles:='$(LazarusDir)/lcl/interfaces'
                             +';$(LazarusDir)/lcl/interfaces/($LCLWidgetType);';
    CompilerOptions.UnitOutputDirectory:='$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgeType)';
    POOutputDirectory:='languages';
    Translated:=SystemLanguageID1;
    LazDocPaths:=SetDirSeparators('$(LazarusDir)/docs/xml/lcl');
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(LCLBasePackage.CreateDependencyWithOwner(Result,true));

    // add issues files
    AddFile('interfaces/carbon/issues.xml','carbon-issues.xml',pftIssues,[],cpBase);
    AddFile('interfaces/win32/issues.xml','win32-issues.xml',pftIssues,[],cpBase);
    AddFile('interfaces/gtk/issues.xml','gtk-issues.xml',pftIssues,[],cpBase);
    AddFile('interfaces/gtk2/issues.xml','gtk2-issues.xml',pftIssues,[],cpBase);
    AddFile('interfaces/qt/issues.xml','qt-issues.xml',pftIssues,[],cpBase);

    SetAllComponentPriorities(LCLCompPriority);

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
       '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)');
    UsageOptions.CustomOptions:='-dLCL -dLCL$(LCLWidgetType)';
    // add include path
    CompilerOptions.IncludePath:=SetDirSeparators(
      '$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)');
    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';
    // build macro: LCLWidgetType
    Macro:=CompilerOptions.BuildMacros.Add('LCLWidgetType');
    for lp:=low(TLCLPlatform) to high(TLCLPlatform) do
      Macro.Values.Add(LCLPlatformDirNames[lp]);
    // build macro: fpGUIPlatform
    Macro:=CompilerOptions.BuildMacros.Add('fpGUIPlatform');
    Macro.Values.Add('gdi');
    Macro.Values.Add('x11');
    // conditionals
    CompilerOptions.Conditionals:=
       '// LCLWidgetType'+LineEnding
      +'if undefined(LCLWidgetType) then begin'+LineEnding
      +'  if (TargetOS=''win32'') or (TargetOS=''win64'') then'+LineEnding
      +'    LCLWidgetType := ''win32'''+LineEnding
      +'  else if TargetOS=''wince'' then'+LineEnding
      +'    LCLWidgetType := ''wince'''+LineEnding
      +'  else if TargetOS=''darwin'' then'+LineEnding
      +'    LCLWidgetType := ''carbon'''+LineEnding
      +'  else'+LineEnding
      +'    LCLWidgetType := ''gtk2'';'+LineEnding
      +'end;'+LineEnding
      +''+LineEnding
      +'// widget set specific options'+LineEnding
      +'base := LCLWidgetType+''/'';'+LineEnding
      +'if LCLWidgetType=''gtk'' then'+LineEnding
      +'  CustomOptions := ''-dgtk1'''+LineEnding
      +'else if LCLWidgetType=''carbon'' then begin'+LineEnding
      +'  CustomOptions := ''-dcarbon'';'+LineEnding
      +'  UnitPath := base+''objc;'''+LineEnding
      +'             +base+''pascocoa/appkit;'''+LineEnding
      +'             +base+''pascocoa/foundation'';'+LineEnding
      +'  IncPath := UnitPath;'+LineEnding
      +'end else if LCLWidgetType=''wince'' then begin'+LineEnding
      +'  CustomOptions := ''-dDisableChecks'';'+LineEnding
      +'end else if LCLWidgetType=''fpgui'' then begin'+LineEnding
      +'  if undefined(fpGUIPlatform) then begin'+LineEnding
      +'    if SrcOS=''win32'' then'+LineEnding
      +'      fpGUIPlatform := ''gdi'''+LineEnding
      +'    else'+LineEnding
      +'      fpGUIPlatform := ''x11'';'+LineEnding
      +'  end;'+LineEnding
      +'  CustomOptions := '' -dfpgui''+fpGUIPlatform;'+LineEnding
      +'  UnitPath := base+''gui;'''+LineEnding
      +'             +base+''corelib;'''+LineEnding
      +'             +base+''corelib/''+fpGUIPlatform;'+LineEnding
      +'  IncPath := UnitPath;'+LineEnding
      +'end;'+LineEnding
      +''+LineEnding
      +'// linker options'+LineEnding
      +'if TargetOS=''darwin'' then begin'+LineEnding
      +'  if LCLWidgetType=''gtk'' then'+LineEnding
      +'    UsageLibraryPath := ''/usr/X11R6/lib;/sw/lib'''+LineEnding
      +'  else if LCLWidgetType=''gtk2'' then'+LineEnding
      +'    UsageLibraryPath := ''/usr/X11R6/lib;/sw/lib;/sw/lib/pango-ft219/lib'''+LineEnding
      +'  else if LCLWidgetType=''carbon'' then begin'+LineEnding
      +'    UsageLinkerOptions := ''-framework Carbon'''+LineEnding
      +'      +'' -framework OpenGL'''+LineEnding
      +'      +'' -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib'';'+LineEnding
      +'  end else if LCLWidgetType=''cocoa'' then'+LineEnding
      +'    UsageLinkerOptions := ''-framework Cocoa'';'+LineEnding
      +'end else if TargetOS=''solaris'' then begin'+LineEnding
      +'  UsageLibraryPath:=''/usr/X11R6/lib'';'+LineEnding
      +'end;'+LineEnding
      +'';

    // use the lcl/units/$(TargetCPU)-$(TargetOS)/alllclunits.o
    // file as indicator, if LCL has been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/alllclunits.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateSynEditPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='SynEdit';
    Filename:=SetDirSeparators('$(LazarusDir)/components/synedit/');
    Version.SetValues(1,0,0,0);
    Author:='SynEdit - http://sourceforge.net/projects/synedit/';
    License:='LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysSynEditTheEditorComponentUsedByLazarus;
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    POOutputDirectory:='languages';
    Translated:=SystemLanguageID1;
    LazDocPaths:=SetDirSeparators('$(LazarusDir)/components/synedit/docs/xml');
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(LCLPackage.CreateDependencyWithOwner(Result,true));
    AddRequiredDependency(IDEIntfPackage.CreateDependencyWithOwner(Result,true));

    // add units
    AddFile('synedit.pp','SynEdit',pftUnit,[],cpBase);
    AddFile('synedit.inc','',pftInclude,[],cpBase);
    AddFile('syneditkeycmds.pp','SynEditKeyCmds',pftUnit,[],cpBase);
    AddFile('syneditmousecmds.pp','SynEditMouseCmds',pftUnit,[],cpBase);
    AddFile('syncompletion.pas','SynCompletion',pftUnit,[],cpBase);
    AddFile('syneditautocomplete.pp','SynEditAutoComplete',pftUnit,[],cpBase);
    AddFile('synmacrorecorder.pas','SynMacroRecorder',pftUnit,[],cpBase);
    AddFile('synmemo.pas','SynMemo',pftUnit,[],cpBase);
    AddFile('syneditsearch.pp','SynEditSearch',pftUnit,[],cpBase);
    AddFile('syneditplugins.pas','SynEditPlugins',pftUnit,[],cpBase);
    AddFile('syneditregexsearch.pas','SynEditRegExSearch',pftUnit,[],cpBase);
    AddFile('synedittypes.pp','SynEditTypes',pftUnit,[],cpBase);
    AddFile('syneditstrconst.pp','SynEditStrConst',pftUnit,[],cpBase);
    AddFile('syneditexport.pas','SynEditExport',pftUnit,[],cpBase);
    AddFile('synexporthtml.pas','SynExportHTML',pftUnit,[],cpBase);
    AddFile('syneditmiscclasses.pp','SynEditMiscClasses',pftUnit,[],cpBase);
    AddFile('syneditmiscprocs.pp','SynEditMiscProcs',pftUnit,[],cpBase);
    AddFile('synbeautifier.pas','SynBeautifier',pftUnit,[],cpBase);
    AddFile('synbeautifierpas.pas','SynBeautifierPas',pftUnit,[],cpBase);
    AddFile('syneditmarks.pp','SynEditMarks',pftUnit,[],cpBase);
    AddFile('synregexpr.pas','SynRegExpr',pftUnit,[],cpBase);
    AddFile('syntextdrawer.pp','SynTextDrawer',pftUnit,[],cpBase);
    AddFile('syneditpointclasses.pas','SynEditPointClasses',pftUnit,[],cpBase);

    AddFile('syneditlines.pp','SynEditLines',pftUnit,[],cpBase);
    AddFile('synedittextbase.pas','SynEditTextBase',pftUnit,[],cpBase);
    AddFile('synedittextbuffer.pp','SynEditTextBuffer',pftUnit,[],cpBase);
    AddFile('synedittextdoublewidthchars.pas','SynEditTextDoubleWidthChars',pftUnit,[],cpBase);
    AddFile('synedittexttabexpander.pas','SynEditTextTabExpander',pftUnit,[],cpBase);
    AddFile('synedittexttrimmer.pas','SynEditTextTrimmer',pftUnit,[],cpBase);
    AddFile('syneditfoldedview.pp','SynEditTextTrimmer',pftUnit,[],cpBase);

    AddFile('syneditmarkup.pp','SynEditMarkup',pftUnit,[],cpBase);
    AddFile('syneditmarkupctrlmouselink.pp','SynEditMarkupCtrlMouseLink',pftUnit,[],cpBase);
    AddFile('syneditmarkupselection.pp','SynEditMarkupSelection',pftUnit,[],cpBase);
    AddFile('syneditmarkupspecialline.pp','SynEditMarkupSpecialLine',pftUnit,[],cpBase);
    AddFile('syneditmarkupwordgroup.pp','SynEditMarkupWordGroup',pftUnit,[],cpBase);
    AddFile('syneditmarkupbracket.pp','SynEditMarkupBracket',pftUnit,[],cpBase);
    AddFile('syneditmarkuphighall.pp','SynEditMarkupHighAll',pftUnit,[],cpBase);

    AddFile('synedithighlighter.pp','SynEditHighlighter',pftUnit,[],cpBase);
    AddFile('synedithighlighterfoldbase.pp','SynEditHighlighterFoldBase',pftUnit,[],cpBase);
    AddFile('synedithighlighterxmlbase.pas','SynEditHighlighterXMLBase',pftUnit,[],cpBase);
    AddFile('synhighlighterpas.pp','SynHighlighterPas',pftUnit,[],cpBase);
    AddFile('synhighlightercpp.pp','SynHighlighterCPP',pftUnit,[],cpBase);
    AddFile('synhighlighterjava.pas','SynHighlighterJava',pftUnit,[],cpBase);
    AddFile('synhighlighterperl.pas','SynHighlighterPerl',pftUnit,[],cpBase);
    AddFile('synhighlighterhtml.pp','SynHighlighterHTML',pftUnit,[],cpBase);
    AddFile('synhighlighterxml.pas','SynHighlighterXML',pftUnit,[],cpBase);
    AddFile('synhighlighterlfm.pas','SynHighlighterLFM',pftUnit,[],cpBase);
    AddFile('synhighlighterdiff.pas','SynHighlighterDiff',pftUnit,[],cpBase);
    AddFile('synhighlighterunixshellscript.pas','SynHighlighterUNIXShellScript',
                                                             pftUnit,[],cpBase);
    AddFile('synhighlightermulti.pas','SynHighlighterMulti',pftUnit,[],cpBase);
    AddFile('synhighlightercss.pas','SynHighlighterCss',pftUnit,[],cpBase);
    AddFile('synhighlighterphp.pas','SynHighlighterPHP',pftUnit,[],cpBase);
    AddFile('synhighlightertex.pas','SynHighlighterTeX',pftUnit,[],cpBase);
    AddFile('synhighlightersql.pas','SynHighlighterSQL',pftUnit,[],cpBase);
    AddFile('synhighlighterpython.pas','SynHighlighterPython',pftUnit,[],cpBase);
    AddFile('synhighlightervb.pas','SynHighlighterVB',pftUnit,[],cpBase);
    AddFile('synhighlighterany.pas','SynHighlighterAny',pftUnit,[],cpBase);
    AddFile('synhighlighterhashentries.pas', 'SynHighlighterHashEntries', pftUnit,[], cpBase);
    AddFile('synhighlighterjscript.pas', 'SynHighlighterJScript', pftUnit,[], cpBase);
    AddFile('synhighlighterposition.pas', 'TSynPositionHighlighter', pftUnit,[], cpBase);
    AddFile('synhighlighterini.pas', 'SynHighlighterBat', pftUnit,[], cpBase);
    AddFile('synhighlighterbat.pas', 'SynHighlighterIni', pftUnit,[], cpBase);

    AddFile('syngutter.pas','SynGutter',pftUnit,[],cpBase);
    AddFile('syngutterbase.pp','SynGutterBase',pftUnit,[],cpBase);
    AddFile('syngutterchanges.pas','SynGutterChanges',pftUnit,[],cpBase);
    AddFile('synguttercodefolding.pas','SynGutterCodeFolding',pftUnit,[],cpBase);
    AddFile('syngutterlinenumber.pas','SynGutterLineNumber',pftUnit,[],cpBase);
    AddFile('synguttermarks.pas','SynGutterMarks',pftUnit,[],cpBase);

    AddFile('synpluginsyncronizededitbase.pp','SynPluginSyncronizedEditBase',pftUnit,[],cpBase);
    AddFile('synpluginsyncroedit.pp','SynPluginSyncroEdit',pftUnit,[],cpBase);
    AddFile('synplugintemplateedit.pp','SynPluginTemplateEdit',pftUnit,[],cpBase);

    AddFile('syneditlazdsgn.pas','SynEditLazDsgn',pftUnit,[],cpBase);
    AddFile('syndesignstringconstants.pas','SynDesignStringConstants',pftUnit,[],cpBase);
    AddFile('synpropertyeditobjectlist.pas','SynPropertyEditObjectList',pftUnit,[],cpBase);

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
                     '$(LazarusDir)/components/synedit/units/$(TargetCPU)-$(TargetOS)');
    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // use the components/units/..../allsyneditunits.o file as indicator,
    // if synedit has been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/components/synedit/units/$(TargetCPU)-$(TargetOS)/allsyneditunits.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateLazControlsPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='LazControls';
    Filename:=SetDirSeparators('$(LazarusDir)/components/lazcontrols/lazcontrols.lpk');
    Version.SetValues(0,0,0,0);
    Author:='Martin Friebe';
    License:='modified LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:='LCL controls for the Lazarus IDE';
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    POOutputDirectory:='languages';
    LazDocPaths:='docs';
    Translated:=SystemLanguageID1;
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(LCLPackage.CreateDependencyWithOwner(Result,true));

    // add units
    AddFile('dividerbevel.pas','DividerBevel',pftUnit,[],cpBase);

    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
           '$(LazarusDir)/components/lazcontrols/lib/$(TargetCPU)-$(TargetOS)');

    // use the components/lazcontrols/lib/..../lazcontrols.o file as indicator,
    // if lazcontrols have been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/components/lazcontrols/lib/$(TargetCPU)-$(TargetOS)/lazcontrols.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateCodeToolsPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='CodeTools';
    Filename:=SetDirSeparators('$(LazarusDir)/components/codetools/codetools.lpk');
    Version.SetValues(1,0,1,0);
    Author:='Mattias Gaertner';
    License:='GPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysCodeToolsToolsAndFunctionsToParseBrowseAndEditPasc;
    PackageType:=lptRunAndDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    POOutputDirectory:='languages';
    LazDocPaths:='docs';
    Translated:=SystemLanguageID1;
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(FCLPackage.CreateDependencyWithOwner(Result,true));

    // add units
    AddFile('basiccodetools.pas','BasicCodeTools',pftUnit,[],cpBase);
    AddFile('ccodeparsertool.pas','CCodeParserTool',pftUnit,[],cpBase);
    AddFile('codeatom.pas','CodeAtom',pftUnit,[],cpBase);
    AddFile('codebeautifier.pas','CodeBeautifier',pftUnit,[],cpBase);
    AddFile('codecache.pas','CodeCache',pftUnit,[],cpBase);
    AddFile('codecompletiontool.pas','CodeCompletionTool',pftUnit,[],cpBase);
    AddFile('codetemplatestool.pas','CodeTemplatesTool',pftUnit,[],cpBase);
    AddFile('codetoolmanager.pas','CodeToolManager',pftUnit,[],cpBase);
    AddFile('codetoolmemmanager.pas','CodeToolMemManager',pftUnit,[],cpBase);
    AddFile('codetoolsconfig.pas','CodeToolsConfig',pftUnit,[],cpBase);
    AddFile('codetoolsstrconsts.pas','CodeToolsStrConsts',pftUnit,[],cpBase);
    AddFile('codetoolsstructs.pas','CodeToolsStructs',pftUnit,[],cpBase);
    AddFile('codetree.pas','CodeTree',pftUnit,[],cpBase);
    AddFile('customcodetool.pas','CustomCodeTool',pftUnit,[],cpBase);
    AddFile('definetemplates.pas','DefineTemplates',pftUnit,[],cpBase);
    AddFile('directorycacher.pas','DirectoryCacher',pftUnit,[],cpBase);
    AddFile('eventcodetool.pas','EventCodeTool',pftUnit,[],cpBase);
    AddFile('expreval.pas','ExprEval',pftUnit,[],cpBase);
    AddFile('extractproctool.pas','ExtractProctool',pftUnit,[],cpBase);
    AddFile('fileprocs.pas','FileProcs',pftUnit,[],cpBase);
    AddFile('finddeclarationcache.pas','FindDeclarationCache',pftUnit,[],cpBase);
    AddFile('finddeclarationtool.pas','FindDeclarationTool',pftUnit,[],cpBase);
    AddFile('identcompletiontool.pas','IdentCompletionTool',pftUnit,[],cpBase);
    AddFile('keywordfunclists.pas','KeywordFuncLists',pftUnit,[],cpBase);
    AddFile('laz_dom.pas','Laz_DOM',pftUnit,[],cpBase);
    AddFile('laz_xmlcfg.pas','Laz_XMLCfg',pftUnit,[],cpBase);
    AddFile('laz_xmlread.pas','Laz_XMLRead',pftUnit,[],cpBase);
    AddFile('laz_xmlstreaming.pas','Laz_XMLStreaming',pftUnit,[],cpBase);
    AddFile('laz_xmlwrite.pas','Laz_XMLWrite',pftUnit,[],cpBase);
    AddFile('lfmtrees.pas','LFMTrees',pftUnit,[],cpBase);
    AddFile('linkscanner.pas','LinkScanner',pftUnit,[],cpBase);
    AddFile('memcheck.pas','MemCheck',pftUnit,[],cpBase);
    AddFile('methodjumptool.pas','MethodJumpTool',pftUnit,[],cpBase);
    AddFile('multikeywordlisttool.pas','MultiKeywordListTool',pftUnit,[],cpBase);
    AddFile('pascalparsertool.pas','PascalParserTool',pftUnit,[],cpBase);
    AddFile('pascalreadertool.pas','PascalReaderTool',pftUnit,[],cpBase);
    AddFile('resourcecodetool.pas','ResourceCodeTool',pftUnit,[],cpBase);
    AddFile('sourcechanger.pas','SourceChanger',pftUnit,[],cpBase);
    AddFile('sourcelog.pas','SourceLog',pftUnit,[],cpBase);
    AddFile('stdcodetools.pas','StdCodeTools',pftUnit,[],cpBase);

    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
           '$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)');

    // use the components/units/..../allcodetoolsunits.o file as indicator,
    // if codetools have been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)/allcodetoolsunits.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateIDEIntfPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='IDEIntf';
    Filename:=SetDirSeparators('$(LazarusDir)/ideintf/');
    Version.SetValues(1,0,0,0);
    Author:='Lazarus';
    License:='LGPL-2';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:='IDEIntf - the interface units for the IDE';
    PackageType:=lptDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    POOutputDirectory:='languages';
    Translated:=SystemLanguageID1;
    LazDocPaths:='docs';
    EnableI18N:=true;
    AddToProjectUsesSection:=false;

    // add requirements
    AddRequiredDependency(LCLPackage.CreateDependencyWithOwner(Result,true));

    // add units
    AddFile('actionseditor.pas','ActionsEditor',pftUnit,[],cpBase);
    AddFile('columndlg.pp','ColumnDlg',pftUnit,[],cpBase);
    AddFile('componenteditors.pas','ComponentEditors',pftUnit,[],cpBase);
    AddFile('componentreg.pas','ComponentReg',pftUnit,[],cpBase);
    AddFile('componenttreeview.pas','ComponentTreeview',pftUnit,[],cpBase);
    AddFile('baseideintf.pas','BaseIDEIntf',pftUnit,[],cpBase);
    AddFile('dbpropedits.pas','DBPropEdits',pftUnit,[],cpBase);
    AddFile('fieldseditor.pas','FieldsEditor',pftUnit,[],cpBase);
    AddFile('formeditingintf.pas','FormEditingIntf',pftUnit,[],cpBase);
    AddFile('frmselectprops.pas','FrmSelectProps',pftUnit,[],cpBase);
    AddFile('graphpropedits.pas','GraphPropEdits',pftUnit,[],cpBase);
    AddFile('helpfpdoc.pas','HelpFPDoc',pftUnit,[],cpBase);
    AddFile('idecommands.pas','IDECommands',pftUnit,[],cpBase);
    AddFile('ideimagesintf.pas','IDECommands',pftUnit,[],cpBase);
    AddFile('ideoptionsintf.pas','IDECommands',pftUnit,[],cpBase);
    AddFile('idewindowintf.pas','IDEWindowIntf',pftUnit,[pffHasRegisterProc],cpBase);
    AddFile('imagelisteditor.pp','ImageListEditor',pftUnit,[],cpBase);
    AddFile('lazideintf.pas','LazIDEIntf',pftUnit,[],cpBase);
    AddFile('listviewpropedit.pp','ListViewPropEdit',pftUnit,[],cpBase);
    AddFile('newitemintf.pas','NewItemIntf',pftUnit,[],cpBase);
    AddFile('macrointf.pas','MacroIntf',pftUnit,[],cpBase);
    AddFile('menuintf.pas','MenuIntf',pftUnit,[],cpBase);
    AddFile('objectinspector.pp','ObjectInspector',pftUnit,[],cpBase);
    AddFile('objinspstrconsts.pas','ObjInspStrConsts',pftUnit,[],cpBase);
    AddFile('packageintf.pas','PackageIntf',pftUnit,[],cpBase);
    AddFile('projectintf.pas','ProjectIntf',pftUnit,[],cpBase);
    AddFile('propedits.pp','PropEdits',pftUnit,[],cpBase);
    AddFile('srceditorintf.pas','SrcEditorIntf',pftUnit,[],cpBase);
    AddFile('texttools.pas','TextTools',pftUnit,[],cpBase);

    SetAllComponentPriorities(IDEIntfCompPriority);

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators(
      '$(LazarusDir)/ideintf/units/$(TargetCPU)-$(TargetOS)');

    CompilerOptions.CustomOptions:='$(IDEBuildOptions)';

    // use the ideintf/units/$(TargetCPU)/$(TargetOS)/allideintf.o file
    // as indicator, if ideintf has been recompiled
    OutputStateFile:=SetDirSeparators(
      '$(LazarusDir)/ideintf/units/$(TargetCPU)-$(TargetOS)/allideintf.o');

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateDefaultPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='DefaultPackage';
    Filename:=SetDirSeparators('$(LazarusDir)/components/custom/');
    Version.SetValues(1,0,1,1);
    Author:='Anonymous';
    AutoInstall:=pitStatic;
    AutoUpdate:=pupManually;
    Description:=lisPkgSysThisIsTheDefaultPackageUsedOnlyForComponents;
    PackageType:=lptDesignTime;
    Installed:=pitStatic;
    CompilerOptions.UnitOutputDirectory:='';
    Translated:=SystemLanguageID1;

    // add unit paths
    UsageOptions.UnitPath:=SetDirSeparators('$(LazarusDir)/components/custom');

    // add requirements
    AddRequiredDependency(LCLPackage.CreateDependencyWithOwner(Result));
    AddRequiredDependency(SynEditPackage.CreateDependencyWithOwner(Result));

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateLazarusBasePackage(PkgName: string
  ): TLazPackage;
begin
  PkgName:=lowercase(PkgName);
  if PkgName='fcl' then Result:=CreateFCLPackage
  else if PkgName='lclbase' then Result:=CreateLCLBasePackage
  else if PkgName='lcl' then Result:=CreateLCLPackage
  else if PkgName='ideintf' then Result:=CreateIDEIntfPackage
  else if PkgName='synedit' then Result:=CreateSynEditPackage
  else if PkgName='codetools' then Result:=CreateCodeToolsPackage
  else if PkgName='lazcontrols' then Result:=CreateLazControlsPackage
  else RaiseGDBException('');
end;

function TLazPackageGraph.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

procedure TLazPackageGraph.AddPackage(APackage: TLazPackage);

  procedure SetBasePackage(var BasePackage: TLazPackage);
  begin
    if BasePackage=APackage then exit;
    if BasePackage<>nil then
      RaiseGDBException('TLazPackageGraph.AddPackage Pkg='+APackage.IDAsString+' conflicts with existing base package');
    BasePackage:=APackage;
  end;

var
  Dependency: TPkgDependency;
begin
  BeginUpdate(true);
  FTree.Add(APackage);
  FItems.Add(APackage);

  if IsStaticBasePackage(APackage.Name) then begin
    APackage.Installed:=pitStatic;
    APackage.AutoInstall:=pitStatic;
    if SysUtils.CompareText(APackage.Name,'FCL')=0 then begin
      SetBasePackage(FFCLPackage);
      APackage.SetAllComponentPriorities(FCLCompPriority);
    end
    else if SysUtils.CompareText(APackage.Name,'LCLBase')=0 then begin
      SetBasePackage(FLCLBasePackage);
      APackage.SetAllComponentPriorities(LCLCompPriority);
    end
    else if SysUtils.CompareText(APackage.Name,'LCL')=0 then begin
      SetBasePackage(FLCLPackage);
      APackage.SetAllComponentPriorities(LCLCompPriority);
    end
    else if SysUtils.CompareText(APackage.Name,'IDEIntf')=0 then begin
      SetBasePackage(FIDEIntfPackage);
      APackage.SetAllComponentPriorities(IDEIntfCompPriority);
    end
    else if SysUtils.CompareText(APackage.Name,'SynEdit')=0 then
      SetBasePackage(FSynEditPackage)
    else if SysUtils.CompareText(APackage.Name,'LazControls')=0 then
      SetBasePackage(FLazControlsPackage)
    else if SysUtils.CompareText(APackage.Name,'CodeTools')=0 then
      SetBasePackage(FCodeToolsPackage);
    if FLazarusBasePackages.IndexOf(APackage)<0 then
      FLazarusBasePackages.Add(APackage);
  end;

  // open all required dependencies
  Dependency:=APackage.FirstRequiredDependency;
  while Dependency<>nil do begin
    OpenDependency(Dependency,false);
    Dependency:=Dependency.NextRequiresDependency;
  end;
  
  // update all missing dependencies
  UpdateBrokenDependenciesToPackage(APackage);
  
  // activate define templates
  APackage.DefineTemplates.Active:=true;

  if Assigned(OnAddPackage) then OnAddPackage(APackage);
  EndUpdate;
end;

procedure TLazPackageGraph.ReplacePackage(OldPackage, NewPackage: TLazPackage);

  procedure MoveInstalledComponents(OldPkgFile: TPkgFile);
  var
    NewPkgFile: TPkgFile;
    OldUnitName: String;
    PkgComponent: TPkgComponent;
  begin
    if (OldPkgFile.ComponentCount>0) then begin
      OldUnitName:=OldPkgFile.Unit_Name;
      if OldUnitName='' then RaiseException('MoveInstalledComponents');
      NewPkgFile:=NewPackage.FindUnit(OldUnitName,false);
      if NewPkgFile=nil then begin
        NewPkgFile:=NewPackage.AddRemovedFile(OldPkgFile.Filename,OldUnitName,
                                        OldPkgFile.FileType,OldPkgFile.Flags,
                                        OldPkgFile.ComponentPriority.Category);
      end;
      while OldPkgFile.ComponentCount>0 do begin
        PkgComponent:=OldPkgFile.Components[0];
        PkgComponent.PkgFile:=NewPkgFile;
      end;
    end;
  end;

var
  OldInstalled: TPackageInstallType;
  OldAutoInstall: TPackageInstallType;
  OldEditor: TBasePackageEditor;
  i: Integer;
begin
  BeginUpdate(true);
  // save flags
  OldInstalled:=OldPackage.Installed;
  OldAutoInstall:=OldPackage.AutoInstall;
  OldEditor:=OldPackage.Editor;
  if OldEditor<>nil then begin
    OldEditor.LazPackage:=nil;
  end;
  // migrate components
  for i:=0 to OldPackage.FileCount-1 do
    MoveInstalledComponents(OldPackage.Files[i]);
  for i:=0 to OldPackage.RemovedFilesCount-1 do
    MoveInstalledComponents(OldPackage.RemovedFiles[i]);
  // delete old package
  Delete(fItems.IndexOf(OldPackage));
  // restore flags
  NewPackage.Installed:=OldInstalled;
  NewPackage.AutoInstall:=OldAutoInstall;
  // add package to graph
  AddPackage(NewPackage);
  if OldEditor<>nil then begin
    OldEditor.LazPackage:=NewPackage;
  end;
  EndUpdate;
end;

procedure TLazPackageGraph.LoadStaticBasePackages;

  procedure LoadLazarusBasePackage(PkgName: string);
  var
    Dependency: TPkgDependency;
    Quiet: Boolean;
  begin
    if FindDependencyByNameInList(FirstAutoInstallDependency,pdlRequires,
      PkgName)<>nil
    then
      exit;
    Dependency:=TPkgDependency.Create;
    Dependency.Owner:=Self;
    Dependency.PackageName:=PkgName;
    Dependency.AddToList(FirstAutoInstallDependency,pdlRequires);
    Quiet:=false;
    OpenInstalledDependency(Dependency,pitStatic,Quiet);
  end;

begin
  LoadLazarusBasePackage('FCL');
  LoadLazarusBasePackage('LCLBase');
  LoadLazarusBasePackage('LCL');
  LoadLazarusBasePackage('IDEIntf');
  LoadLazarusBasePackage('SynEdit');
  LoadLazarusBasePackage('CodeTools');
  LoadLazarusBasePackage('LazControls');
  // the default package will be added on demand
  if FDefaultPackage=nil then
    FDefaultPackage:=CreateDefaultPackage;

  SortAutoInstallDependencies;

  // register them
  RegisterStaticBasePackages;
end;

procedure TLazPackageGraph.LoadAutoInstallPackages(PkgList: TStringList);
var
  i: Integer;
  PackageName: string;
  Dependency: TPkgDependency;
begin
  for i:=0 to PkgList.Count-1 do begin
    PackageName:=PkgList[i];
    if (PackageName='') or (not IsValidIdent(PackageName)) then continue;
    Dependency:=FindDependencyByNameInList(FirstAutoInstallDependency,
                                           pdlRequires,PackageName);
    //DebugLn('TLazPackageGraph.LoadAutoInstallPackages ',dbgs(Dependency),' ',PackageName);
    if Dependency<>nil then continue;
    Dependency:=TPkgDependency.Create;
    Dependency.Owner:=Self;
    Dependency.PackageName:=PackageName;
    Dependency.AddToList(FirstAutoInstallDependency,pdlRequires);
    if OpenDependency(Dependency,false)<>lprSuccess then begin
      IDEMessageDialog(lisPkgMangUnableToLoadPackage,
        Format(lisPkgMangUnableToOpenThePackage, ['"', PackageName, '"', #13]),
        mtWarning,[mbOk]);
      continue;
    end;
    if not Dependency.RequiredPackage.Missing then
      Dependency.RequiredPackage.AutoInstall:=pitStatic;
  end;
  SortAutoInstallDependencies;
end;

procedure TLazPackageGraph.SortAutoInstallDependencies;
begin
  // sort install dependencies, so that lower packages come first
  SortDependencyListTopologically(PackageGraph.FirstAutoInstallDependency,
                                               false);
end;

function TLazPackageGraph.GetIDEInstallPackageOptions(
  FirstDependency: TPkgDependency;
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
  PkgList: TFPList;
  AddOptionsList: TFPList;
  ConfigDir: String;
begin
  Result:='';
  if not Assigned(OnGetAllRequiredPackages) then exit;

  // get all required packages
  PkgList:=nil;
  OnGetAllRequiredPackages(FirstDependency,PkgList);
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
  AddOption(PrepareCmdLineOption('-Fi'+UTF8ToSys(ConfigDir)));
end;

function TLazPackageGraph.SaveAutoInstallConfig: TModalResult;
var
  ConfigDir: String;
  StaticPackagesInc: String;
  StaticPckIncludeFile: String;
  PkgList: TFPList;
  APackage: TLazPackage;
  i: Integer;
begin
  ConfigDir:=AppendPathDelim(GetPrimaryConfigPath);

  // create auto install package list for the Lazarus uses section
  PkgList:=nil;
  try
    GetAllRequiredPackages(FirstAutoInstallDependency,PkgList);
    StaticPackagesInc:='';
    if PkgList<>nil then begin
      for i:=0 to PkgList.Count-1 do begin
        APackage:=TLazPackage(PkgList[i]);
        if (APackage=nil) or APackage.AutoCreated
        or IsStaticBasePackage(APackage.Name)
        or (APackage.PackageType=lptRunTime)
        then continue;
        StaticPackagesInc:=StaticPackagesInc
            +ExtractFileNameOnly(APackage.GetCompileSourceFilename)
            +','+LineEnding;
      end;
    end;
  finally
    PkgList.Free;
  end;
  StaticPckIncludeFile:=ConfigDir+'staticpackages.inc';
  Result:=SaveStringToFile(StaticPckIncludeFile,StaticPackagesInc,[],
                           lisPkgMangstaticPackagesConfigFile);
end;

function TLazPackageGraph.IsStaticBasePackage(PackageName: string
  ): boolean;
begin
  PackageName:=lowercase(PackageName);
  Result:=(PackageName='fcl')
       or (PackageName='lclbase')
       or (PackageName='lcl')
       or (PackageName='synedit')
       or (PackageName='ideintf')
       or (PackageName='codetools')
       or (PackageName='lazcontrols');
end;

procedure TLazPackageGraph.FreeAutoInstallDependencies;
var
  Dependency: TPkgDependency;
begin
  while Assigned(PackageGraph.FirstAutoInstallDependency) do
  begin
    Dependency:=PackageGraph.FirstAutoInstallDependency;
    Dependency.RequiredPackage:=nil;
    Dependency.RemoveFromList(PackageGraph.FirstAutoInstallDependency,pdlRequires);
    Dependency.Free;
  end;
end;

procedure TLazPackageGraph.ClosePackage(APackage: TLazPackage);
begin
  if (lpfDestroying in APackage.Flags) or PackageIsNeeded(APackage) then exit;
  CloseUnneededPackages;
end;

procedure TLazPackageGraph.MarkNeededPackages;
var
  i: Integer;
  Pkg: TLazPackage;
  PkgStack: PLazPackage;
  StackPtr: Integer;
  RequiredPackage: TLazPackage;
  Dependency: TPkgDependency;
begin
  if Count=0 then exit;
  // create stack
  GetMem(PkgStack,SizeOf(Pointer)*Count);
  StackPtr:=0;
  // put all needed packages on stack and set lpfNeeded
  for i:=0 to FItems.Count-1 do begin
    Pkg:=TLazPackage(FItems[i]);
    if PackageIsNeeded(Pkg) then begin
      Pkg.Flags:=Pkg.Flags+[lpfNeeded];
      PkgStack[StackPtr]:=Pkg;
      inc(StackPtr);
    end else
      Pkg.Flags:=Pkg.Flags-[lpfNeeded];
  end;
  // mark all needed packages
  while StackPtr>0 do begin
    // get needed package from stack
    dec(StackPtr);
    Pkg:=PkgStack[StackPtr];
    // put all required packages on stack
    Dependency:=Pkg.FirstRequiredDependency;
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=Dependency.RequiredPackage;
        if (not (lpfNeeded in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfNeeded];
          PkgStack[StackPtr]:=RequiredPackage;
          inc(StackPtr);
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
  // clean up
  FreeMem(PkgStack);
end;

function TLazPackageGraph.FindBrokenDependencyPath(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TFPList;
// returns the first broken dependency (broken = not loaded)
// the first items are TLazPackage, the last item is a TPkgDependency
  
  procedure FindBroken(Dependency: TPkgDependency; var PathList: TFPList);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        // dependency ok
        RequiredPackage:=Dependency.RequiredPackage;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          FindBroken(RequiredPackage.FirstRequiredDependency,PathList);
          if PathList<>nil then begin
            // broken dependency found
            // -> add current package to list
            PathList.Insert(0,RequiredPackage);
            exit;
          end;
        end;
      end else begin
        // broken dependency found
        PathList:=TFPList.Create;
        PathList.Add(Dependency);
        exit;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
  
begin
  Result:=nil;
  if (Count=0) then exit;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  FindBroken(FirstDependency,Result);
  if (Result<>nil) and (APackage<>nil) then
    Result.Insert(0,APackage);
end;

function TLazPackageGraph.FindAllBrokenDependencies(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TFPList;
// returns the list of broken dependencies (TPkgDependency)

  procedure FindBroken(Dependency: TPkgDependency; var DepList: TFPList);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        // dependency ok
        RequiredPackage:=Dependency.RequiredPackage;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          FindBroken(RequiredPackage.FirstRequiredDependency,DepList);
        end;
      end else begin
        // broken dependency found
        if (DepList=nil) or (DepList.IndexOf(Dependency)<0) then begin
          if DepList=nil then
            DepList:=TFPList.Create;
          DepList.Add(Dependency);
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

begin
  Result:=nil;
  if (Count=0) then exit;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  FindBroken(FirstDependency,Result);
end;

function TLazPackageGraph.FindCircleDependencyPath(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TFPList;

  procedure FindCircle(Dependency: TPkgDependency; var PathList: TFPList);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        // dependency ok
        RequiredPackage:=Dependency.RequiredPackage;
        if lpfCircle in RequiredPackage.Flags then begin
          // circle detected
          PathList:=TFPList.Create;
          PathList.Add(RequiredPackage);
          exit;
        end;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited,lpfCircle];
          FindCircle(RequiredPackage.FirstRequiredDependency,PathList);
          if PathList<>nil then begin
            // circle detected
            // -> add current package to list
            PathList.Insert(0,RequiredPackage);
            exit;
          end;
          RequiredPackage.Flags:=RequiredPackage.Flags-[lpfCircle];
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

var
  i: Integer;
  Pkg: TLazPackage;
begin
  Result:=nil;
  if (Count=0) then exit;
  // mark all packages as not visited and circle free
  for i:=FItems.Count-1 downto 0 do begin
    Pkg:=TLazPackage(FItems[i]);
    Pkg.Flags:=Pkg.Flags-[lpfVisited,lpfCircle];
  end;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  FindCircle(FirstDependency,Result);
  if (Result<>nil) and (APackage<>nil) then
    Result.Insert(0,APackage);
end;

function TLazPackageGraph.FindPkgOutputInFPCSearchPath(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TFPList;
var
  CfgCache: TFPCTargetConfigCache;

  function CheckPkg(Pkg: TLazPackage; var PathList: TFPList): boolean;
  var
    OutputDir: String;
    i: Integer;
    Dir: String;
  begin
    Result:=true;
    if (Pkg=nil) then exit;
    Pkg.Flags:=Pkg.Flags+[lpfVisited];
    if (Pkg.FirstRequiredDependency=nil)
    or Pkg.IsVirtual or (Pkg.AutoUpdate<>pupAsNeeded) then exit;
    // this package is compiled automatically and has dependencies
    OutputDir:=ChompPathDelim(Pkg.GetOutputDirectory);
    if OutputDir='' then exit;
    for i:=0 to CfgCache.UnitPaths.Count-1 do begin
      Dir:=ChompPathDelim(CfgCache.UnitPaths[i]);
      if CompareFilenames(Dir,OutputDir)=0 then begin
        // this package changes the units in the default FPC search path
        // => a circle, because the dependencies use FPC search path too
        Result:=false;
        PathList:=TFPList.Create;
        PathList.Add(Pkg);
        exit;
      end;
    end;
  end;

  procedure CheckDependencyList(Dependency: TPkgDependency; var PathList: TFPList);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=Dependency.RequiredPackage;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          if CheckPkg(RequiredPackage,PathList) then exit;
          CheckDependencyList(RequiredPackage.FirstRequiredDependency,PathList);
          if PathList<>nil then begin
            // circle detected
            // -> add current package to list
            PathList.Insert(0,RequiredPackage);
            exit;
          end;
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

var
  UnitSet: TFPCUnitSetCache;
begin
  Result:=nil;
  MarkAllPackagesAsNotVisited;
  UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
  if UnitSet=nil then exit;
  CfgCache:=UnitSet.GetConfigCache(false);
  if (CfgCache=nil) or (CfgCache.UnitPaths=nil) then exit;
  if APackage<>nil then begin
    if not CheckPkg(APackage,Result) then exit;
    if FirstDependency=nil then
      FirstDependency:=APackage.FirstRequiredDependency;
  end;
  CheckDependencyList(FirstDependency,Result);
end;

function TLazPackageGraph.FindUnsavedDependencyPath(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TFPList;

  procedure FindUnsaved(Dependency: TPkgDependency; var PathList: TFPList);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        // dependency ok
        RequiredPackage:=Dependency.RequiredPackage;
        if RequiredPackage.Modified then begin
          // unsaved package detected
          PathList:=TFPList.Create;
          PathList.Add(RequiredPackage);
          exit;
        end;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          FindUnsaved(RequiredPackage.FirstRequiredDependency,PathList);
          if PathList<>nil then begin
            // unsaved package detected
            // -> add current package to list
            PathList.Insert(0,RequiredPackage);
            exit;
          end;
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

begin
  Result:=nil;
  if (Count=0) or (APackage=nil) then exit;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  FindUnsaved(FirstDependency,Result);
  if (Result<>nil) and (APackage<>nil) then
    Result.Insert(0,APackage);
end;

function TLazPackageGraph.FindNotInstalledRegisterUnits(
  APackage: TLazPackage; FirstDependency: TPkgDependency): TFPList;
// returns the list of required units (TPkgFile) with a Register procedure,
// that are not installed in the IDE

  procedure FindNotInstalledRegisterUnit(Dependency: TPkgDependency;
    var UnitList: TFPList);
  var
    RequiredPackage: TLazPackage;
    i: Integer;
    APkgFile: TPkgFile;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        // dependency ok
        RequiredPackage:=Dependency.RequiredPackage;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          if RequiredPackage.Installed=pitNope then begin
            // package not installed
            for i:=0 to RequiredPackage.FileCount-1 do begin
              APkgFile:=RequiredPackage.Files[i];
              if APkgFile.HasRegisterProc then begin
                // unit with register procedure -> add
                if UnitList=nil then
                  UnitList:=TFPList.Create;
                UnitList.Add(APkgFile);
              end;
            end;
          end;
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          FindNotInstalledRegisterUnit(RequiredPackage.FirstRequiredDependency,UnitList);
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

begin
  Result:=nil;
  if (Count=0) then exit;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  FindNotInstalledRegisterUnit(FirstDependency,Result);
end;

function TLazPackageGraph.FindAutoInstallDependencyPath(
  ChildPackage: TLazPackage): TFPList;
  
  procedure FindAutoInstallParent(APackage: TLazPackage);
  var
    ParentPackage: TLazPackage;
    Dependency: TPkgDependency;
  begin
    Dependency:=APackage.FirstUsedByDependency;
    while Dependency<>nil do begin
      if Dependency.Owner is TLazPackage then begin
        ParentPackage:=TLazPackage(Dependency.Owner);
        if not (lpfVisited in ParentPackage.Flags) then begin
          ParentPackage.Flags:=ParentPackage.Flags+[lpfVisited];
          if ParentPackage.AutoInstall<>pitNope then begin
            // auto install parent found
            if Result=nil then Result:=TFPList.Create;
            Result.Add(ParentPackage);
            Result.Add(APackage);
            exit;
          end;
          FindAutoInstallParent(ParentPackage);
          if Result<>nil then begin
            // build path
            Result.Add(APackage);
            exit;
          end;
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

begin
  Result:=nil;
  MarkAllPackagesAsNotVisited;
  ChildPackage.Flags:=ChildPackage.Flags+[lpfVisited];
  FindAutoInstallParent(ChildPackage);
end;

function TLazPackageGraph.FindAmbiguousUnits(APackage: TLazPackage;
  FirstDependency: TPkgDependency; var File1, File2: TPkgFile;
  var ConflictPkg: TLazPackage): boolean;
// check if two connected packages have units with the same name
// Connected means here: a Package1 is directly required by a Package2
// or: a Package1 and a Package2 are directly required by a Package3
// returns true, if ambiguous units found
// There can either be a conflict between two files (File1,File2)
// or between a file and a package (File1,ConflictPkg)
const
  FileTypes = PkgFileUnitTypes-[pftVirtualUnit];
var
  PackageTreeOfUnitTrees: TAVLTree; // tree of TPkgUnitsTree
  
  function GetUnitsTreeOfPackage(Pkg: TLazPackage): TPkgUnitsTree;
  var
    ANode: TAVLTreeNode;
    PkgFile: TPkgFile;
    i: Integer;
  begin
    // for first time: create PackageTreeOfUnitTrees
    if PackageTreeOfUnitTrees=nil then
      PackageTreeOfUnitTrees:=TAVLTree.Create(TListSortCompare(@CompareUnitsTree));
    // search UnitsTree for package
    ANode:=PackageTreeOfUnitTrees.FindKey(Pkg, TListSortCompare(@ComparePackageWithUnitsTree));
    if ANode<>nil then begin
      Result:=TPkgUnitsTree(ANode.Data);
      exit;
    end;
    // first time: create tree of units for Pkg
    Result:=TPkgUnitsTree.Create(Pkg);
    PackageTreeOfUnitTrees.Add(Result);
    for i:=0 to Pkg.FileCount-1 do begin
      PkgFile:=Pkg.Files[i];
      if (PkgFile.FileType in FileTypes) and (PkgFile.Unit_Name<>'') then
        Result.Add(PkgFile);
    end;
  end;
  
  function FindAmbiguousUnitsBetween2Packages(Pkg1,Pkg2: TLazPackage): boolean;
  var
    i: Integer;
    PkgFile1: TPkgFile;
    PkgFile2: TPkgFile;
    UnitsTreeOfPkg2: TPkgUnitsTree;
  begin
    Result:=false;
    if Pkg1=Pkg2 then exit;
    if (Pkg1.FileCount=0) or (Pkg2.FileCount=0) then exit;
    UnitsTreeOfPkg2:=GetUnitsTreeOfPackage(Pkg2);
    // check if a unit of Pkg2 has the same name as Pkg1
    PkgFile2:=UnitsTreeOfPkg2.FindPkgFileWithUnitName(Pkg1.Name);
    if PkgFile2<>nil then begin
      File1:=PkgFile2;
      ConflictPkg:=Pkg1;
      Result:=true;
      exit;
    end;
    for i:=0 to Pkg1.FileCount-1 do begin
      PkgFile1:=Pkg1.Files[i];
      if (PkgFile1.FileType in FileTypes)
      and (PkgFile1.Unit_Name<>'') then begin
        // check if a unit of Pkg1 exists in Pkg2
        PkgFile2:=UnitsTreeOfPkg2.FindPkgFileWithUnitName(PkgFile1.Unit_Name);
        if PkgFile2<>nil then begin
          File1:=PkgFile1;
          File2:=PkgFile2;
          Result:=true;
          exit;
        end;
        // check if a unit of Pkg1 has the same name as Pkg2
        if SysUtils.CompareText(PkgFile1.Unit_Name,Pkg2.Name)=0 then begin
          File1:=PkgFile1;
          ConflictPkg:=Pkg2;
          Result:=true;
          exit;
        end;
      end;
    end;
  end;

var
  PkgList: TFPList;
  ConnectionsTree: TPkgPairTree;
  ANode: TAVLTreeNode;
  Pair: TPkgPair;
begin
  Result:=false;
  if APackage<>nil then begin
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  File1:=nil;
  File2:=nil;
  ConflictPkg:=nil;
  ConnectionsTree:=nil;
  PkgList:=nil;
  PackageTreeOfUnitTrees:=nil;
  GetConnectionsTree(FirstDependency,PkgList,ConnectionsTree);
  try
    if ConnectionsTree=nil then exit;
    ANode:=ConnectionsTree.FindLowest;
    while ANode<>nil do begin
      Pair:=TPkgPair(ANode.Data);
      Result:=FindAmbiguousUnitsBetween2Packages(Pair.Package1,Pair.Package2);
      if Result then exit;
      ANode:=ConnectionsTree.FindSuccessor(ANode);
    end;
  finally
    if PackageTreeOfUnitTrees<>nil then begin
      PackageTreeOfUnitTrees.FreeAndClear;
      PackageTreeOfUnitTrees.Free;
    end;
    ConnectionsTree.Free;
    PkgList.Free;
  end;
  Result:=false;
end;

function TLazPackageGraph.FindFPCConflictUnit(APackage: TLazPackage;
  FirstDependency: TPkgDependency; const Directory: string;
  OnFindFPCUnit: TFindFPCUnitEvent;
  var File1: TPkgFile; var ConflictPkg: TLazPackage): boolean;
  
  function CheckUnitName(const AnUnitName: string): boolean;
  var Filename: string;
  begin
    Result:=false;
    if AnUnitName='' then exit;
    Filename:='';
    OnFindFPCUnit(AnUnitName,Directory,Filename);
    Result:=Filename<>'';
  end;
  
  function CheckDependencyList(ADependency: TPkgDependency): boolean; forward;

  function CheckPackage(Pkg1: TLazPackage): boolean;
  var
    Cnt: Integer;
    i: Integer;
    CurFile: TPkgFile;
  begin
    Result:=false;
    if (Pkg1=nil) or (lpfVisited in Pkg1.Flags)
    or (Pkg1=FFCLPackage) or (Pkg1=FLCLBasePackage) or (Pkg1=FLCLPackage) then exit;
    Pkg1.Flags:=Pkg1.Flags+[lpfVisited];
    Result:=CheckUnitName(Pkg1.Name);
    if Result then begin
      ConflictPkg:=Pkg1;
      exit;
    end;
    Cnt:=Pkg1.FileCount;
    for i:=0 to Cnt-1 do begin
      CurFile:=Pkg1.Files[i];
      if (CurFile.FileType in PkgFileRealUnitTypes)
      and (pffAddToPkgUsesSection in CurFile.Flags) then begin
        Result:=CheckUnitName(CurFile.Unit_Name);
        if Result then begin
          File1:=CurFile;
          exit;
        end;
      end;
    end;
    Result:=CheckDependencyList(Pkg1.FirstRequiredDependency);
  end;
  
  function CheckDependencyList(ADependency: TPkgDependency): boolean;
  begin
    Result:=false;
    while ADependency<>nil do begin
      Result:=CheckPackage(ADependency.RequiredPackage);
      if Result then exit;
      ADependency:=ADependency.NextDependency[pdlRequires];
    end;
  end;

begin
  Result:=false;
  if (Directory<>'') and not FilenameIsAbsolute(Directory) then
    RaiseGDBException(Directory);
  File1:=nil;
  ConflictPkg:=nil;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then
    Result:=CheckPackage(APackage)
  else
    Result:=CheckDependencyList(FirstDependency);
end;

function TLazPackageGraph.GetAutoCompilationOrder(APackage: TLazPackage;
  FirstDependency: TPkgDependency; SkipDesignTimePackages: boolean;
  Policies: TPackageUpdatePolicies): TFPList;
// Returns all required auto update packages, including indirect requirements.
// The packages will be in topological order, with the package that should be
// compiled first at the end.

  procedure GetTopologicalOrder(Dependency: TPkgDependency);
  var
    RequiredPackage: TLazPackage;
  begin
    while Dependency<>nil do begin
      if Dependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=Dependency.RequiredPackage;
        if not (lpfVisited in RequiredPackage.Flags) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          if RequiredPackage.AutoUpdate in Policies then begin
            // add first all needed packages
            GetTopologicalOrder(RequiredPackage.FirstRequiredDependency);
            // then add this package
            if (not SkipDesignTimePackages)
            or (RequiredPackage.PackageType<>lptDesignTime) then begin
              if Result=nil then Result:=TFPList.Create;
              Result.Add(RequiredPackage);
            end;
          end;
        end;
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
  
begin
  Result:=nil;
  MarkAllPackagesAsNotVisited;
  if APackage<>nil then begin
    APackage.Flags:=APackage.Flags+[lpfVisited];
    FirstDependency:=APackage.FirstRequiredDependency;
  end;
  GetTopologicalOrder(FirstDependency);
end;

procedure TLazPackageGraph.MarkAllPackagesAsNotVisited;
var
  i: Integer;
  Pkg: TLazPackage;
begin
  // mark all packages as not visited
  for i:=FItems.Count-1 downto 0 do begin
    Pkg:=TLazPackage(FItems[i]);
    Pkg.Flags:=Pkg.Flags-[lpfVisited];
  end;
end;

procedure TLazPackageGraph.MarkAllDependencies(
  MarkPackages: boolean; AddMarkerFlags, RemoveMarkerFlags: TPkgMarkerFlags);
var
  i: Integer;
  Pkg: TLazPackage;
  Dependency: TPkgDependency;
begin
  // mark all dependencies of all packages as not visited
  for i:=FItems.Count-1 downto 0 do begin
    Pkg:=TLazPackage(FItems[i]);
    if MarkPackages then
      Pkg.Flags:=Pkg.Flags-[lpfVisited];
    Dependency:=Pkg.FirstRequiredDependency;
    while Dependency<>nil do begin
      Dependency.MarkerFlags:=
                        Dependency.MarkerFlags+AddMarkerFlags-RemoveMarkerFlags;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
end;

procedure TLazPackageGraph.MarkAllRequiredPackages(
  FirstDependency: TPkgDependency);
var
  Dependency: TPkgDependency;
  RequiredPackage: TLazPackage;
begin
  Dependency:=FirstDependency;
  while Dependency<>nil do begin
    if Dependency.LoadPackageResult=lprSuccess then begin
      RequiredPackage:=Dependency.RequiredPackage;
      if not (lpfVisited in RequiredPackage.Flags) then begin
        RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
        MarkAllRequiredPackages(RequiredPackage.FirstRequiredDependency);
      end;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
end;

procedure TLazPackageGraph.CloseUnneededPackages;
var
  i: Integer;
begin
  BeginUpdate(false);
  MarkNeededPackages;
  for i:=FItems.Count-1 downto 0 do begin
    if not (lpfNeeded in Packages[i].Flags) then Delete(i);
  end;
  EndUpdate;
end;

procedure TLazPackageGraph.ChangePackageID(APackage: TLazPackage;
  const NewName: string; NewVersion: TPkgVersion; RenameDependencies,
  RenameMacros: boolean);
var
  Dependency: TPkgDependency;
  NextDependency: TPkgDependency;
  OldPkgName: String;
  i: Integer;
  Macro: TLazBuildMacro;
  RenamedMacros: TStringList;
  OldMacroName: String;
  BaseCompOpts: TBaseCompilerOptions;
begin
  OldPkgName:=APackage.Name;
  if (OldPkgName=NewName) and (APackage.Version.Compare(NewVersion)=0) then
    exit; // fit exactly

  BeginUpdate(true);

  if RenameMacros then
  begin
    // rename macros
    RenamedMacros:=TStringList.Create;
    try
      for i:=0 to APackage.CompilerOptions.BuildMacros.Count-1 do
      begin
        Macro:=APackage.CompilerOptions.BuildMacros[i];
        if SysUtils.CompareText(OldPkgName,copy(Macro.Identifier,1,length(OldPkgName)))=0
        then begin
          OldMacroName:=Macro.Identifier;
          RenamedMacros.Add(OldMacroName);
          Macro.Identifier:=NewName+copy(OldMacroName,length(OldPkgName)+1,256);
          BaseCompOpts:=TBaseCompilerOptions(APackage.CompilerOptions);
          BaseCompOpts.RenameMacro(OldMacroName,Macro.Identifier,true);
        end;
      end;
    finally
      RenamedMacros.Free;
    end;
  end;

  // cut or fix all dependencies, that became incompatible
  Dependency:=APackage.FirstUsedByDependency;
  while Dependency<>nil do begin
    NextDependency:=Dependency.NextUsedByDependency;
    if not Dependency.IsCompatible(NewName,NewVersion) then begin
      if RenameDependencies then begin
        Dependency.MakeCompatible(NewName,NewVersion);
        if Assigned(OnDependencyModified) then OnDependencyModified(Dependency);
      end else begin
        // remove dependency from the used-by list of the required package
        Dependency.RequiredPackage:=nil;
      end;
    end;
    Dependency:=NextDependency;
  end;

  // change ID
  FTree.Remove(APackage);
  APackage.ChangeID(NewName,NewVersion);
  FTree.Add(APackage);

  // update old broken dependencies
  UpdateBrokenDependenciesToPackage(APackage);

  if Assigned(OnChangePackageName) then
    OnChangePackageName(APackage,OldPkgName);

  // no try-finally needed, because above fails only for fatal reasons
  EndUpdate;
end;

function TLazPackageGraph.SavePackageCompiledState(APackage: TLazPackage;
  const CompilerFilename, CompilerParams: string; Complete, MainPPUExists,
  ShowAbort: boolean): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  CompilerFileDate: Integer;
  o: TPkgOutputDir;
  stats: PPkgLastCompileStats;
begin
  Result:=mrCancel;
  StateFile:=APackage.GetStateFilename;
  try
    CompilerFileDate:=FileAgeCached(CompilerFilename);

    o:=APackage.GetOutputDirType;
    stats:=@APackage.LastCompile[o];
    stats^.CompilerFilename:=CompilerFilename;
    stats^.CompilerFileDate:=CompilerFileDate;
    stats^.Params:=CompilerParams;
    stats^.Complete:=Complete;
    stats^.ViaMakefile:=false;

    XMLConfig:=TXMLConfig.CreateClean(StateFile);
    try
      XMLConfig.SetValue('Compiler/Value',CompilerFilename);
      XMLConfig.SetValue('Compiler/Date',CompilerFileDate);
      XMLConfig.SetValue('Params/Value',CompilerParams);
      XMLConfig.SetDeleteValue('Complete/Value',Complete,true);
      XMLConfig.SetDeleteValue('Complete/MainPPUExists',MainPPUExists,true);
      InvalidateFileStateCache;
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
    stats^.StateFileName:=StateFile;
    stats^.StateFileDate:=FileAgeCached(StateFile);
    APackage.LastCompile[o].StateFileLoaded:=true;
  except
    on E: Exception do begin
      Result:=IDEMessageDialogAb(lisPkgMangErrorWritingFile,
        Format(lisPkgMangUnableToWriteStateFileOfPackageError, ['"', StateFile,
          '"', #13, APackage.IDAsString, #13, E.Message]),
        mtError,[mbCancel],ShowAbort);
      exit;
    end;
  end;

  Result:=mrOk;
end;

function TLazPackageGraph.LoadPackageCompiledState(APackage: TLazPackage;
  IgnoreErrors, ShowAbort: boolean): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  StateFileAge: Integer;
  stats: PPkgLastCompileStats;
  o: TPkgOutputDir;
begin
  o:=APackage.GetOutputDirType;
  stats:=@APackage.LastCompile[o];
  StateFile:=APackage.GetStateFilename;
  if not FileExistsCached(StateFile) then begin
    //DebugLn('TLazPackageGraph.LoadPackageCompiledState Statefile not found: ',StateFile);
    stats^.StateFileLoaded:=false;
    Result:=mrOk;
    exit;
  end;

  // read the state file
  StateFileAge:=FileAgeCached(StateFile);
  if (not stats^.StateFileLoaded)
  or (stats^.StateFileDate<>StateFileAge)
  or (stats^.StateFileName<>StateFile) then begin
    stats^.StateFileLoaded:=false;
    try
      XMLConfig:=TXMLConfig.Create(StateFile);
      try
        stats^.CompilerFilename:=XMLConfig.GetValue('Compiler/Value','');
        stats^.CompilerFileDate:=XMLConfig.GetValue('Compiler/Date',0);
        stats^.Params:=XMLConfig.GetValue('Params/Value','');
        stats^.Complete:=XMLConfig.GetValue('Complete/Value',true);
        stats^.MainPPUExists:=XMLConfig.GetValue('Complete/MainPPUExists',true);
        stats^.ViaMakefile:=XMLConfig.GetValue('Makefile/Value',false);
      finally
        XMLConfig.Free;
      end;
      stats^.StateFileName:=StateFile;
      stats^.StateFileDate:=StateFileAge;
      stats^.StateFileLoaded:=true;
    except
      on E: Exception do begin
        if IgnoreErrors then begin
          Result:=mrOk;
        end else begin
          Result:=IDEMessageDialogAb(lisPkgMangErrorReadingFile,
            Format(lisPkgMangUnableToReadStateFileOfPackageError, ['"',
              StateFile, '"', #13, APackage.IDAsString, #13, E.Message]),
            mtError,[mbCancel],ShowAbort);
        end;
        exit;
      end;
    end;
  end;

  Result:=mrOk;
end;

function TLazPackageGraph.CheckCompileNeedDueToFPCUnits(TheOwner: TObject;
  StateFileAge: longint): boolean;
var
  AProject: TLazProject;
  Pkg: TLazPackage;
  ID: String;
  Dir: string;
  UnitSetID: String;
  HasChanged: boolean;
  Cache: TFPCUnitSetCache;
  CfgCache: TFPCTargetConfigCache;
  Node: TAVLTreeNode;
  Item: PStringToStringTreeItem;
  Filename: String;
begin
  Result:=false;
  if TheOwner=nil then exit;
  if TheOwner is TLazPackage then begin
    Pkg:=TLazPackage(TheOwner);
    if Pkg.IsVirtual then exit;
    Dir:=Pkg.DirectoryExpanded;
    ID:=Pkg.Name;
  end else if TheOwner is TLazProject then begin
    AProject:=TLazProject(TheOwner);
    Dir:=ExtractFilePath(AProject.ProjectInfoFile);
    ID:=ExtractFileName(AProject.ProjectInfoFile);
  end else
    exit;
  if (Dir='') or (not FilenameIsAbsolute(Dir)) then
    exit;
  UnitSetID:=CodeToolBoss.GetUnitSetIDForDirectory(Dir);
  if UnitSetID='' then exit;
  Cache:=CodeToolBoss.FPCDefinesCache.FindUnitSetWithID(UnitSetID,HasChanged,false);
  if Cache=nil then exit;
  CfgCache:=Cache.GetConfigCache(false);
  if CfgCache=nil then exit;
  if CfgCache.Units=nil then exit;
  Node:=CfgCache.Units.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringTreeItem(Node.Data);
    Filename:=Item^.Value;
    if FileAgeCached(Filename)>StateFileAge then begin
      debugln(['TLazPackageGraph.CheckCompileNeedDueToFPCUnits FPC unit "',Filename,'" is newer than state file of ',ID]);
      exit(true);
    end;
    Node:=CfgCache.Units.Tree.FindSuccessor(Node);
  end;
end;

function TLazPackageGraph.CheckCompileNeedDueToDependencies(TheOwner: TObject;
  FirstDependency: TPkgDependency; SkipDesignTimePackages: boolean;
  StateFileAge: longint): TModalResult;

  function GetOwnerID: string;
  begin
    OnGetDependencyOwnerDescription(FirstDependency,Result);
  end;

var
  Dependency: TPkgDependency;
  RequiredPackage: TLazPackage;
  OtherStateFile: String;
  o: TPkgOutputDir;
begin
  Dependency:=FirstDependency;
  if Dependency=nil then begin
    // no dependencies
    // => check FPC units
    if CheckCompileNeedDueToFPCUnits(TheOwner,StateFileAge) then
      exit(mrYes);
    Result:=mrNo;
    exit;
  end;

  while Dependency<>nil do begin
    if (Dependency.LoadPackageResult=lprSuccess) then begin
      RequiredPackage:=Dependency.RequiredPackage;
      if SkipDesignTimePackages and (RequiredPackage.PackageType=lptDesignTime)
      then begin
        // skip
      end else begin
        // check compile state file of required package
        if not RequiredPackage.AutoCreated then begin
          Result:=LoadPackageCompiledState(RequiredPackage,false,true);
          if Result<>mrOk then exit;
          Result:=mrYes;
          o:=RequiredPackage.GetOutputDirType;
          if not RequiredPackage.LastCompile[o].StateFileLoaded then begin
            DebugLn('TPkgManager.CheckCompileNeedDueToDependencies  No state file for ',RequiredPackage.IDAsString);
            exit;
          end;
          if StateFileAge<RequiredPackage.LastCompile[o].StateFileDate then begin
            DebugLn('TPkgManager.CheckCompileNeedDueToDependencies ',
              ' State file of ',RequiredPackage.IDAsString,' is newer than state file of ',GetOwnerID);
            exit;
          end;
        end;
        // check output state file of required package
        if RequiredPackage.OutputStateFile<>'' then begin
          OtherStateFile:=RequiredPackage.OutputStateFile;
          GlobalMacroList.SubstituteStr(OtherStateFile);
          if not FilenameIsAbsolute(OtherStateFile) then
            OtherStateFile:=AppendPathDelim(RequiredPackage.Directory)+OtherStateFile;
          if FilenameIsAbsolute(OtherStateFile)
          and FileExistsCached(OtherStateFile)
          and (FileAgeCached(OtherStateFile)>StateFileAge) then begin
            DebugLn('TPkgManager.CheckCompileNeedDueToDependencies ',
              ' OtherState of ',RequiredPackage.IDAsString,' file "',OtherStateFile,'" (',
                FileAgeToStr(FileAgeCached(OtherStateFile)),')'
              ,' is newer than State file ',GetOwnerID,'(',FileAgeToStr(StateFileAge),')');
            Result:=mrYes;
            exit;
          end;
        end;
      end;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
  Result:=mrNo;
end;

function TLazPackageGraph.CheckIfPackageNeedsCompilation(APackage: TLazPackage;
  const CompilerFilename, CompilerParams, SrcFilename: string;
  SkipDesignTimePackages: boolean; out NeedBuildAllFlag: boolean): TModalResult;
var
  OutputDir: String;
  NewOutputDir: String;
  ConfigChanged: boolean;
  DependenciesChanged: boolean;
  DefResult: TModalResult;
  OldNeedBuildAllFlag: Boolean;
  OldOverride: String;
begin
  Result:=mrYes;
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.CheckIfPackageNeedsCompilation A ',APackage.IDAsString);
  {$ENDIF}
  NeedBuildAllFlag:=false;

  if APackage.AutoUpdate=pupManually then exit(mrNo);

  // check the current output directory
  Result:=CheckIfCurPkgOutDirNeedsCompile(APackage,
             CompilerFilename,CompilerParams,SrcFilename,
             true,SkipDesignTimePackages,
             NeedBuildAllFlag,ConfigChanged,DependenciesChanged);
  if Result=mrNo then exit; // the current output is valid

  // the current output directory needs compilation
  if APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride='' then
  begin
    // the last compile was put to the normal/default output directory
    OutputDir:=APackage.GetOutputDirectory(false);
    if OutputDirectoryIsWritable(APackage,OutputDir,false) then
    begin
      // the normal output directory is writable => keep using it
      exit;
    end;
    debugln(['TLazPackageGraph.CheckIfPackageNeedsCompilation normal output dir is not writable: ',OutputDir]);
    // the normal output directory is not writable
    // => try the fallback directory
    NewOutputDir:=GetFallbackOutputDir(APackage);
    if (NewOutputDir=OutputDir) or (NewOutputDir='') then exit;
    APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride:=NewOutputDir;
    Result:=CheckIfCurPkgOutDirNeedsCompile(APackage,
               CompilerFilename,CompilerParams,SrcFilename,
               true,SkipDesignTimePackages,
               NeedBuildAllFlag,ConfigChanged,DependenciesChanged);
  end else begin
    // the last compile was put to the fallback output directory
    if not ConfigChanged then begin
      // some source files have changed, not the compiler parameters
      // => keep using the fallback directory
      exit;
    end;
    if DependenciesChanged then begin
      // dependencies have changed
      // => switching to the not writable default output directory is not possible
      // => keep using the fallback directory
      exit;
    end;
    // maybe the user switched the settings back to default
    // => try using the default output directory
    OldOverride:=APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride;
    APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride:='';
    OldNeedBuildAllFlag:=NeedBuildAllFlag;
    DefResult:=CheckIfCurPkgOutDirNeedsCompile(APackage,
               CompilerFilename,CompilerParams,SrcFilename,
               true,SkipDesignTimePackages,
               NeedBuildAllFlag,ConfigChanged,DependenciesChanged);
    if DefResult=mrNo then begin
      // switching back to the not writable output directory requires no compile
      debugln(['TLazPackageGraph.CheckIfPackageNeedsCompilation switching back to the normal output directory: ',APackage.GetOutputDirectory]);
      exit(mrNo);
    end;
    // neither the default nor the fallback is valid
    // => switch back to the fallback
    APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride:=OldOverride;
    NeedBuildAllFlag:=OldNeedBuildAllFlag;
  end;
end;

function TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile(
  APackage: TLazPackage; const CompilerFilename, CompilerParams,
  SrcFilename: string;
  CheckDependencies, SkipDesignTimePackages: boolean;
  out NeedBuildAllFlag,
  ConfigChanged, DependenciesChanged: boolean): TModalResult;
// returns: mrYes, mrNo, mrCancel, mrAbort
var
  StateFilename: String;
  StateFileAge: Integer;
  i: Integer;
  CurFile: TPkgFile;
  LastParams: String;
  LastPaths: TStringList;
  CurPaths: TStringList;
  OldValue: string;
  NewValue: string;
  o: TPkgOutputDir;
  Stats: PPkgLastCompileStats;
  SrcPPUFile: String;
begin
  Result:=mrYes;
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile A ',APackage.IDAsString);
  {$ENDIF}
  NeedBuildAllFlag:=false;
  ConfigChanged:=false;
  DependenciesChanged:=false;
  
  if APackage.AutoUpdate=pupManually then exit(mrNo);

  o:=APackage.GetOutputDirType;
  Stats:=@APackage.LastCompile[o];
  //debugln(['TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Last="',ExtractCompilerParamsForBuildAll(APackage.LastCompilerParams),'" Now="',ExtractCompilerParamsForBuildAll(CompilerParams),'"']);
  if (Stats^.CompilerFilename<>CompilerFilename)
  or (ExtractFPCParamsForBuildAll(Stats^.Params)
      <>ExtractFPCParamsForBuildAll(CompilerParams))
  or ((Stats^.CompilerFileDate>0)
      and FileExistsCached(CompilerFilename)
      and (FileAgeCached(CompilerFilename)<>Stats^.CompilerFileDate))
  then begin
    NeedBuildAllFlag:=true;
    ConfigChanged:=true;
  end;

  // check state file
  StateFilename:=APackage.GetStateFilename;
  Result:=LoadPackageCompiledState(APackage,false,true);
  if Result<>mrOk then exit;
  if not Stats^.StateFileLoaded then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  No state file for ',APackage.IDAsString);
    ConfigChanged:=true;
    exit(mrYes);
  end;

  StateFileAge:=FileAgeUTF8(StateFilename);

  // check compiler and params
  LastParams:=APackage.GetLastCompilerParams(o);
  if Stats^.ViaMakefile then begin
    // the package was compiled via Makefile
    CurPaths:=nil;
    LastPaths:=nil;
    try
      CurPaths:=ExtractSearchPathsFromFPCParams(CompilerParams,true);
      LastPaths:=ExtractSearchPathsFromFPCParams(LastParams,true);
      //debugln(['TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile CompilerParams="',CompilerParams,'" UnitPaths="',CurPaths.Values['UnitPath'],'"']);
      // compare custom options
      OldValue:=LastPaths.Values['Reduced'];
      NewValue:=CurPaths.Values['Reduced'];
      if NewValue<>OldValue then begin
        DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler custom params changed for ',APackage.IDAsString);
        DebugLn('  Old="',OldValue,'"');
        DebugLn('  Now="',NewValue,'"');
        ConfigChanged:=true;
        exit(mrYes);
      end;
      // compare unit paths
      OldValue:=TrimSearchPath(LastPaths.Values['UnitPath'],APackage.Directory,true);
      NewValue:=TrimSearchPath(CurPaths.Values['UnitPath'],APackage.Directory,true);
      if NewValue<>OldValue then begin
        DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler unit paths changed for ',APackage.IDAsString);
        DebugLn('  Old="',OldValue,'"');
        DebugLn('  Now="',NewValue,'"');
        ConfigChanged:=true;
        exit(mrYes);
      end;
      // compare include paths
      OldValue:=TrimSearchPath(LastPaths.Values['IncPath'],APackage.Directory,true);
      NewValue:=TrimSearchPath(CurPaths.Values['IncPath'],APackage.Directory,true);
      if NewValue<>OldValue then begin
        DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler include paths changed for ',APackage.IDAsString);
        DebugLn('  Old="',OldValue,'"');
        DebugLn('  Now="',NewValue,'"');
        ConfigChanged:=true;
        exit(mrYes);
      end;
    finally
      CurPaths.Free;
      LastPaths.Free;
    end;
  end else if CompilerParams<>LastParams then begin
    // package was compiled by Lazarus
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler params changed for ',APackage.IDAsString);
    DebugLn('  Old="',LastParams,'"');
    DebugLn('  Now="',CompilerParams,'"');
    ConfigChanged:=true;
    exit(mrYes);
  end;
  if (not Stats^.ViaMakefile)
  and (CompilerFilename<>Stats^.CompilerFilename) then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler filename changed for ',APackage.IDAsString);
    DebugLn('  Old="',Stats^.CompilerFilename,'"');
    DebugLn('  Now="',CompilerFilename,'"');
    exit(mrYes);
  end;
  if not FileExistsCached(CompilerFilename) then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler filename not found for ',APackage.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit(mrYes);
  end;
  if (not Stats^.ViaMakefile)
  and (FileAgeCached(CompilerFilename)<>Stats^.CompilerFileDate) then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compiler file changed for ',APackage.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit(mrYes);
  end;

  // check main source file
  if (SrcFilename<>'') then
  begin
    if (not FileExistsCached(SrcFilename)) or (StateFileAge<FileAgeUTF8(SrcFilename))
    then begin
      DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  SrcFile outdated of ',APackage.IDAsString,': ',SrcFilename);
      exit(mrYes);
    end;
    // check main source ppu file
    if Stats^.MainPPUExists then begin
      SrcPPUFile:=APackage.GetSrcPPUFilename;
      if not FileExistsCached(SrcPPUFile) then begin
        DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  main ppu file missing of ',APackage.IDAsString,': ',SrcPPUFile);
        exit(mrYes);
      end;
    end;
  end;


  //debugln(['TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile ',APackage.Name,' Last="',APackage.LastCompilerParams,'" Now="',CompilerParams,'"']);

  // compiler and parameters are the same
  // quick compile is possible
  NeedBuildAllFlag:=false;

  if not Stats^.Complete then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Compile was incomplete for ',APackage.IDAsString);
    exit(mrYes);
  end;

  if CheckDependencies then begin
    // check all required packages
    Result:=CheckCompileNeedDueToDependencies(APackage,
          APackage.FirstRequiredDependency,SkipDesignTimePackages,StateFileAge);
    if Result<>mrNo then begin
      DependenciesChanged:=true;
      exit;
    end;
  end;

  // check package files
  if StateFileAge<FileAgeCached(APackage.Filename) then begin
    DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  StateFile older than lpk ',APackage.IDAsString);
    exit(mrYes);
  end;
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    //debugln(['TLazPackageGraph.CheckIfPackageNeedsCompilation  CurFile.Filename="',CurFile.Filename,'" Exists=',FileExistsUTF8(CurFile.Filename),' NewerThanStateFile=',StateFileAge<FileAgeCached(CurFile.Filename)]);
    if FileExistsCached(CurFile.Filename)
    and (StateFileAge<FileAgeCached(CurFile.Filename)) then begin
      DebugLn('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile  Src has changed ',APackage.IDAsString,' ',CurFile.Filename);
      exit(mrYes);
    end;
  end;
  
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.CheckIfCurPkgOutDirNeedsCompile END ',APackage.IDAsString);
  {$ENDIF}
  Result:=mrNo;
end;

procedure TLazPackageGraph.InvalidateStateFile(APackage: TLazPackage);
begin
  APackage.LastCompile[APackage.GetOutputDirType].StateFileLoaded:=false
end;

function TLazPackageGraph.CompileRequiredPackages(APackage: TLazPackage;
  FirstDependency: TPkgDependency; SkipDesignTimePackages: boolean;
  Policies: TPackageUpdatePolicies): TModalResult;
var
  AutoPackages: TFPList;
  i: Integer;
begin
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.CompileRequiredPackages A ');
  {$ENDIF}
  AutoPackages:=PackageGraph.GetAutoCompilationOrder(APackage,
                               FirstDependency,SkipDesignTimePackages,Policies);
  if AutoPackages<>nil then begin
    //DebugLn('TLazPackageGraph.CompileRequiredPackages B Count=',IntToStr(AutoPackages.Count));
    try
      i:=0;
      while i<AutoPackages.Count do begin
        Result:=CompilePackage(TLazPackage(AutoPackages[i]),
                               [pcfDoNotCompileDependencies,pcfOnlyIfNeeded,
                                pcfDoNotSaveEditorFiles],false);
        if Result<>mrOk then exit;
        inc(i);
      end;
    finally
      AutoPackages.Free;
    end;
  end;
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.CompileRequiredPackages END ');
  {$ENDIF}
  Result:=mrOk;
end;

function TLazPackageGraph.CompilePackage(APackage: TLazPackage;
  Flags: TPkgCompileFlags; ShowAbort: boolean): TModalResult;

  function GetIgnoreIdentifier: string;
  begin
    Result:='install_package_compile_failed:'+APackage.Filename;
  end;

  function GetCompilerParams: string;
  begin
    Result:=APackage.CompilerOptions.MakeOptionsString(
            APackage.CompilerOptions.DefaultMakeOptionsFlags+[ccloAbsolutePaths])
            +' '+CreateRelativePath(APackage.GetSrcFilename,APackage.Directory);
  end;

var
  PkgCompileTool: TIDEExternalToolOptions;
  CompilerFilename: String;
  CompilerParams: String;
  SrcFilename: String;
  EffectiveCompilerParams: String;
  CompilePolicies: TPackageUpdatePolicies;
  BlockBegan: Boolean;
  NeedBuildAllFlag: Boolean;
  CompileResult, MsgResult: TModalResult;
  SrcPPUFile: String;
  SrcPPUFileExists: Boolean;
begin
  Result:=mrCancel;

  //DebugLn('TLazPackageGraph.CompilePackage A ',APackage.IDAsString,' Flags=',PkgCompileFlagsToString(Flags));

  if APackage.AutoCreated then begin
    DebugLn(['TLazPackageGraph.CompilePackage failed because autocreated: ',APackage.IDAsString]);
    exit;
  end;

  BeginUpdate(false);
  try
    // automatically compile required packages
    if not (pcfDoNotCompileDependencies in Flags) then begin
      CompilePolicies:=[pupAsNeeded];
      if pcfCompileDependenciesClean in Flags then
        Include(CompilePolicies,pupOnRebuildingAll);
      Result:=CompileRequiredPackages(APackage,nil,
                            pcfSkipDesignTimePackages in Flags,CompilePolicies);
      if Result<>mrOk then begin
        DebugLn(['TLazPackageGraph.CompilePackage CompileRequiredPackages failed: ',APackage.IDAsString]);
        exit;
      end;
    end;

    SrcFilename:=APackage.GetSrcFilename;
    CompilerFilename:=APackage.GetCompilerFilename;
    // Note: use absolute paths, because some external tools resolve symlinked directories
    CompilerParams:=GetCompilerParams;
    //DebugLn(['TLazPackageGraph.CompilePackage SrcFilename="',SrcFilename,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"']);

    // check if compilation is needed and if a clean build is needed
    Result:=CheckIfPackageNeedsCompilation(APackage,
                          CompilerFilename,CompilerParams,
                          SrcFilename,pcfSkipDesignTimePackages in Flags,
                          NeedBuildAllFlag);
    if (pcfOnlyIfNeeded in Flags) then begin
      if Result=mrNo then begin
        //DebugLn(['TLazPackageGraph.CompilePackage ',APackage.IDAsString,' does not need compilation.']);
        Result:=mrOk;
        exit;
      end;
      if Result<>mrYes then begin
        DebugLn(['TLazPackageGraph.CompilePackage CheckIfPackageNeedsCompilation failed: ',APackage.IDAsString]);
        exit;
      end;
    end;

    BlockBegan:=IDEMessagesWindow<>nil;
    if BlockBegan then
      IDEMessagesWindow.BeginBlock;
    try
      if (LazarusIDE<>nil) then
        LazarusIDE.MainBarSubTitle:=APackage.Name;
      // auto increase version
      // ToDo

      Result:=PreparePackageOutputDirectory(APackage,pcfCleanCompile in Flags);
      if Result<>mrOk then begin
        DebugLn('TLazPackageGraph.CompilePackage PreparePackageOutputDirectory failed: ',APackage.IDAsString);
        exit;
      end;
      // maybe output directory changed: update parameters
      CompilerParams:=GetCompilerParams;

      // create package main source file
      Result:=SavePackageMainSource(APackage,Flags,ShowAbort);
      if Result<>mrOk then begin
        DebugLn('TLazPackageGraph.CompilePackage SavePackageMainSource failed: ',APackage.IDAsString);
        exit;
      end;

      // check ambiguous units
      Result:=CheckAmbiguousPackageUnits(APackage);
      if Result<>mrOk then begin
        DebugLn('TLazPackageGraph.CompilePackage CheckAmbiguousPackageUnits failed: ',APackage.IDAsString);
        exit;
      end;

      // create Makefile
      if ((pcfCreateMakefile in Flags)
      or (APackage.CompilerOptions.CreateMakefileOnBuild)) then begin
        Result:=WriteMakeFile(APackage);
        if Result<>mrOk then begin
          DebugLn('TLazPackageGraph.CompilePackage DoWriteMakefile failed: ',APackage.IDAsString);
          exit;
        end;
      end;

      // run compilation tool 'Before'
      if not (pcfDoNotCompilePackage in Flags) then begin
        Result:=APackage.CompilerOptions.ExecuteBefore.Execute(
                                 APackage.Directory,'Executing command before');
        if Result<>mrOk then begin
          DebugLn(['TLazPackageGraph.CompilePackage ExecuteBefore failed: ',APackage.IDAsString]);
          exit;
        end;
      end;

      // create external tool to run the compiler
      //DebugLn('TPkgManager.DoCompilePackage Compiler="',CompilerFilename,'"');
      //DebugLn('TPkgManager.DoCompilePackage Params="',CompilerParams,'"');
      //DebugLn('TPkgManager.DoCompilePackage WorkingDir="',APackage.Directory,'"');

      if (not APackage.CompilerOptions.SkipCompiler)
      and (not (pcfDoNotCompilePackage in Flags)) then begin
        // check compiler filename
        try
          CheckIfFileIsExecutable(CompilerFilename);
        except
          on e: Exception do begin
            DebugLn(['TLazPackageGraph.CompilePackage ',APackage.IDAsString,' ',e.Message]);
            Result:=IDEMessageDialog(lisPkgManginvalidCompilerFilename,
              Format(lisPkgMangTheCompilerFileForPackageIsNotAValidExecutable, [
                APackage.IDAsString, #13, E.Message]),
              mtError,[mbCancel,mbAbort]);
            exit;
          end;
        end;

        // change compiler parameters for compiling clean
        EffectiveCompilerParams:=CompilerParams;
        if (pcfCleanCompile in Flags) or NeedBuildAllFlag then begin
          if EffectiveCompilerParams<>'' then
            EffectiveCompilerParams:='-B '+EffectiveCompilerParams
          else
            EffectiveCompilerParams:='-B';
        end;

        PkgCompileTool:=TIDEExternalToolOptions.Create;
        try
          PkgCompileTool.Title:=Format(lisPkgMangCompilingPackage, [APackage.IDAsString]);
          PkgCompileTool.ScanOutputForFPCMessages:=true;
          PkgCompileTool.ScanOutputForMakeMessages:=true;
          PkgCompileTool.WorkingDirectory:=APackage.Directory;
          PkgCompileTool.Filename:=CompilerFilename;
          PkgCompileTool.CmdLineParams:=EffectiveCompilerParams;

          // clear old errors
          if SourceEditorManagerIntf<>nil then
            SourceEditorManagerIntf.ClearErrorLines;

          // compile package
          CompileResult:=RunCompilerWithOptions(PkgCompileTool,APackage.CompilerOptions);
          // check if main ppu file was created
          SrcPPUFile:=APackage.GetSrcPPUFilename;
          SrcPPUFileExists:=(SrcPPUFile<>'') and FileExistsUTF8(SrcPPUFile);
          // write state file
          Result:=SavePackageCompiledState(APackage,
                                      CompilerFilename,CompilerParams,
                                      CompileResult=mrOk,SrcPPUFileExists,true);
          if Result<>mrOk then begin
            DebugLn(['TLazPackageGraph.CompilePackage SavePackageCompiledState failed: ',APackage.IDAsString]);
            exit;
          end;
          Result:=CompileResult;
          if Result<>mrOk then exit;
        finally
          // clean up
          PkgCompileTool.Free;
        end;
      end;

      // update .po files
      if (APackage.POOutputDirectory<>'') then begin
        Result:=ConvertPackageRSTFiles(APackage);
        if Result<>mrOk then begin
          IDEMessagesWindow.AddMsg(Format(
            lisPkgMangErrorUpdatingPoFilesFailedForPackage, [APackage.IDAsString
            ]), APackage.Directory, -1);
          DebugLn('TLazPackageGraph.CompilePackage ConvertPackageRSTFiles failed: ',APackage.IDAsString);
          exit;
        end;
      end;

      // run compilation tool 'After'
      if not (pcfDoNotCompilePackage in Flags) then begin
        Result:=APackage.CompilerOptions.ExecuteAfter.Execute(
                                  APackage.Directory,'Executing command after');
        if Result<>mrOk then begin
          IDEMessagesWindow.AddMsg(Format(
            lisIDEInfoErrorRunningCompileAfterToolFailedForPackage, [APackage.
            IDAsString]), APackage.Directory, -1);
          DebugLn(['TLazPackageGraph.CompilePackage ExecuteAfter failed: ',APackage.IDAsString]);
          exit;
        end;
      end;
      Result:=mrOk;
    finally
      if (LazarusIDE<>nil) then
        LazarusIDE.MainBarSubTitle:='';
      if BlockBegan and (IDEMessagesWindow<>nil) then
        IDEMessagesWindow.EndBlock;
      if Result<>mrOk then begin
        if (APackage.AutoInstall<>pitNope)
        and (OnUninstallPackage<>nil)
        and (not IsStaticBasePackage(APackage.Name))
        and (IgnoreQuestions<>nil)
        and (IgnoreQuestions.Find(GetIgnoreIdentifier)=nil)
        then begin
          // a package needed for installation failed to compile
          // -> ask user if the package should be removed from the installation
          // list
          MsgResult:=IDEQuestionDialog(lisInstallationFailed,
            Format(lisPkgMangThePackageFailedToCompileRemoveItFromTheInstallati,
              ['"', APackage.IDAsString, '"', #13]), mtConfirmation,
              [mrYes, lisRemoveFromInstallList, mrIgnore, lisKeepInInstallList
                ]);
          if MsgResult=mrIgnore then
            IgnoreQuestions.Add(GetIgnoreIdentifier,iiid24H)
          else if MsgResult=mrYes then
          begin
            Result:=OnUninstallPackage(APackage,
              [puifDoNotConfirm,puifDoNotBuildIDE],true);
          end;
        end;
      end;
    end;
  finally
    PackageGraph.EndUpdate;
  end;
end;

function TLazPackageGraph.ConvertPackageRSTFiles(APackage: TLazPackage
  ): TModalResult;
var
  PkgOutputDirectory: String;
  POOutputDirectory: String;
begin
  Result:=mrOK;
  if (APackage.POOutputDirectory='') then exit;// nothing to do
  POOutputDirectory:=AppendPathDelim(APackage.GetPOOutDirectory);

  // create output directory if not exists
  if not DirectoryExistsUTF8(POOutputDirectory) then begin
    Result:=ForceDirectoryInteractive(POOutputDirectory,[mbRetry,mbIgnore]);
    if Result<>mrOk then begin
      if Result=mrIgnore then Result:=mrOk;
      DebugLn(['TLazPackageGraph.ConvertPackageRSTFiles unable to create directory ',POOutputDirectory]);
      exit;
    end;
  end;
  
  // find all .rst files in package output directory
  if not DirectoryIsWritableCached(POOutputDirectory) then begin
    // this package is read only
    DebugLn(['TLazPackageGraph.ConvertPackageRSTFiles skipping read only directory '+POOutputDirectory]);
    exit(mrOK);
  end;

  PkgOutputDirectory:=AppendPathDelim(APackage.GetOutputDirectory);
  if not ConvertRSTFiles(PkgOutputDirectory,POOutputDirectory, APackage.Name+'.po') then begin
    DebugLn(['TLazPackageGraph.ConvertPackageRSTFiles FAILED: PkgOutputDirectory=',PkgOutputDirectory,' RSTOutputDirectory=',POOutputDirectory]);
    exit(mrCancel);
  end;
  Result:=mrOK;
end;

function TLazPackageGraph.WriteMakeFile(APackage: TLazPackage): TModalResult;
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
  FPCMakeTool: TIDEExternalToolOptions;
  CodeBuffer: TCodeBuffer;
  MainSrcFile: String;
  CustomOptions: String;
  IncPath: String;
  MakefileCompiledFilename: String;
  XMLConfig: TXMLConfig;
  OtherOptions: String;
begin
  Result:=mrCancel;
  PathDelimNeedsReplace:=PathDelim<>'/';

  if not DirectoryIsWritableCached(APackage.Directory) then begin
    // the Makefile.fpc is only needed for custom building
    // if the package directory is not writable, then the user does not want to
    // custom build
    // => silently skip
    DebugLn(['TPkgManager.DoWriteMakefile Skipping, because package directory is not writable: ',APackage.Directory]);
    Result:=mrOk;
    exit;
  end;
  MakefileFPCFilename:=AppendPathDelim(APackage.Directory)+'Makefile.fpc';
  MakefileCompiledFilename:=AppendPathDelim(APackage.Directory)+'Makefile.compiled';

  SrcFilename:=APackage.GetSrcFilename;
  MainUnitName:=lowercase(ExtractFileNameOnly((SrcFilename)));
  UnitPath:=APackage.CompilerOptions.GetUnitPath(true,
                                                 coptParsedPlatformIndependent);
  IncPath:=APackage.CompilerOptions.GetIncludePath(true,
                                                 coptParsedPlatformIndependent,false);
  UnitOutputPath:=APackage.CompilerOptions.GetUnitOutPath(true,
                                                 coptParsedPlatformIndependent);
  CustomOptions:=APackage.CompilerOptions.GetCustomOptions(
                                                 coptParsedPlatformIndependent);
  OtherOptions:=APackage.CompilerOptions.MakeOptionsString(
                              [ccloDoNotAppendOutFileOption,ccloNoMacroParams]);

  try
    XMLConfig:=TXMLConfig.Create(MakefileCompiledFilename);
    try
      XMLConfig.SetValue('Makefile/Value',True);
      s:=OtherOptions;
      if UnitPath<>'' then
        s:=s+' -Fu'+SwitchPathDelims(UnitPath,pdsUnix);
      if IncPath<>'' then
        s:=s+' -Fi'+SwitchPathDelims(IncPath,pdsUnix);
      if CustomOptions<>'' then
        s:=s+' '+CustomOptions;
      s:=s+' '+SwitchPathDelims(CreateRelativePath(APackage.GetSrcFilename,APackage.Directory),pdsUnix);

      //debugln(['TLazPackageGraph.WriteMakeFile IncPath="',IncPath,'" UnitPath="',UnitPath,'" Custom="',CustomOptions,'" Out="',UnitOutputPath,'"']);
      XMLConfig.SetValue('Params/Value',s);
      if XMLConfig.Modified then begin
        InvalidateFileStateCache;
        XMLConfig.Flush;
      end;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      Result:=IDEMessageDialog(lisPkgMangErrorWritingFile,
        Format(lisPkgMangUnableToWriteStateFileOfPackageError, ['"', MakefileCompiledFilename,
          '"', #13, APackage.IDAsString, #13, E.Message]),
        mtError,[mbCancel],'');
      exit;
    end;
  end;


  //DebugLn('TPkgManager.DoWriteMakefile ',APackage.Name,' makefile UnitPath="',UnitPath,'"');
  UnitPath:=ConvertLazarusToMakefileSearchPath(UnitPath);
  IncPath:=ConvertLazarusToMakefileSearchPath(IncPath);
  // remove path delimiter at the end, or else it will fail on windows
  UnitOutputPath:=ConvertLazarusToMakefileDirectory(
                                                ChompPathDelim(UnitOutputPath));
  MainSrcFile:=CreateRelativePath(SrcFilename,APackage.Directory);
  CustomOptions:=ConvertLazarusOptionsToMakefileOptions(CustomOptions);
  OtherOptions:=ConvertLazarusOptionsToMakefileOptions(OtherOptions);
  if CustomOptions<>'' then
    if OtherOptions<>'' then
      OtherOptions:=OtherOptions+' '+CustomOptions
    else
      OtherOptions:=CustomOptions;

  e:=LineEnding;
  s:='';
  s:=s+'#   File generated automatically by Lazarus Package Manager'+e;
  s:=s+'#'+e;
  s:=s+'#   Makefile.fpc for '+APackage.IDAsString+e;
  s:=s+'#'+e;
  s:=s+'#   This file was generated on '+DateToStr(Now)+''+e;
  s:=s+''+e;
  s:=s+'[package]'+e;
  s:=s+'name='+lowercase(APackage.Name)+e;
  s:=s+'version='+APackage.Version.AsString+e;
  s:=s+''+e;
  s:=s+'[compiler]'+e;
  s:=s+'unittargetdir='+UnitOutputPath+e;
  if UnitPath<>'' then
    s:=s+'unitdir='+UnitPath+e;
  if IncPath<>'' then
    s:=s+'includedir='+IncPath+e;
  s:=s+'options='+OtherOptions+e; // ToDo do the other options
  s:=s+''+e;
  s:=s+'[target]'+e;
  s:=s+'units='+MainSrcFile+e;
  //s:=s+'implicitunits=syntextdrawer'+e; // TODO list all unit names
  s:=s+''+e;
  s:=s+'[clean]'+e;
  s:=s+'files=$(wildcard $(COMPILER_UNITTARGETDIR)/*$(OEXT)) \'+e;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*$(PPUEXT)) \'+e;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*$(RSTEXT)) \'+e;
  if (TrimFilename(UnitOutputPath)<>'') and (TrimFilename(UnitOutputPath)<>'.')
  then begin
    s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*.lfm) \'+e;
    s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*.res) \'+e;
  end;
  s:=s+'      $(wildcard $(COMPILER_UNITTARGETDIR)/*.compiled) \'+e;
  s:=s+'      $(wildcard *$(OEXT)) $(wildcard *$(PPUEXT)) $(wildcard *$(RSTEXT))'+e;
  s:=s+'[prerules]'+e;
  s:=s+'# LCL Platform'+e;
  s:=s+'ifndef LCL_PLATFORM'+e;
  s:=s+'ifeq ($(OS_TARGET),win32)'+e;
  s:=s+'LCL_PLATFORM=win32'+e;
  s:=s+'else'+e;
  s:=s+'ifeq ($(OS_TARGET),win64)'+e;
  s:=s+'LCL_PLATFORM=win32'+e;
  s:=s+'else'+e;
  s:=s+'ifeq ($(OS_TARGET),darwin)'+e;
  s:=s+'LCL_PLATFORM=carbon'+e;
  s:=s+'else'+e;
  s:=s+'LCL_PLATFORM=gtk2'+e;
  s:=s+'endif'+e;
  s:=s+'endif'+e;
  s:=s+'endif'+e;
  s:=s+'endif'+e;
  s:=s+'export LCL_PLATFORM'+e;

  s:=s+''+e;
  s:=s+'[rules]'+e;
  s:=s+'.PHONY: cleartarget compiled all'+e;
  s:=s+''+e;
  s:=s+'cleartarget:'+e;
  s:=s+'        -$(DEL) $(COMPILER_UNITTARGETDIR)/'+MainUnitName+'$(PPUEXT)'+e;
  s:=s+''+e;
  s:=s+'compiled:'+e;
  s:=s+'        $(COPY) Makefile.compiled $(COMPILER_UNITTARGETDIR)/'+APackage.Name+'.compiled'+e;
  s:=s+''+e;
  s:=s+'all: cleartarget $(COMPILER_UNITTARGETDIR) '+MainUnitName+'$(PPUEXT) compiled'+e;

  //DebugLn('TPkgManager.DoWriteMakefile [',s,']');

  CodeBuffer:=CodeToolBoss.LoadFile(MakefileFPCFilename,true,true);
  if CodeBuffer=nil then begin
    CodeBuffer:=CodeToolBoss.CreateFile(MakefileFPCFilename);
    if CodeBuffer=nil then begin
      if not DirectoryIsWritableCached(ExtractFilePath(MakefileFPCFilename))
      then begin
        // the package source is read only => ignore
        exit(mrOk);
      end;
      debugln(['TLazPackageGraph.WriteMakeFile unable to create file '+MakefileFPCFilename]);
      exit(mrCancel);
    end;
  end;

  if ExtractCodeFromMakefile(CodeBuffer.Source)=ExtractCodeFromMakefile(s)
  then begin
    // Makefile.fpc not changed
    Result:=mrOk;
    exit;
  end;
  CodeBuffer.Source:=s;

  //debugln('TPkgManager.DoWriteMakefile MakefileFPCFilename="',MakefileFPCFilename,'"');
  Result:=SaveCodeBufferToFile(CodeBuffer,MakefileFPCFilename);
  if Result<>mrOk then begin
    if not DirectoryIsWritableCached(ExtractFilePath(MakefileFPCFilename)) then
    begin
      // the package source is read only => no problem
      Result:=mrOk;
    end;
    exit;
  end;

  // call fpcmake to create the Makefile
  FPCMakeTool:=TIDEExternalToolOptions.Create;
  try
    FPCMakeTool.Title:=Format(lisIDEInfoCreatingMakefileForPackage, [APackage.
      IDAsString]);
    FPCMakeTool.WorkingDirectory:=APackage.Directory;
    FPCMakeTool.Filename:=FindFPCTool('fpcmake'+GetExecutableExt,
                                      EnvironmentOptions.GetCompilerFilename);
    FPCMakeTool.CmdLineParams:='-q -TAll';
    FPCMakeTool.EnvironmentOverrides.Add(
                            'FPCDIR='+EnvironmentOptions.GetFPCSourceDirectory);

    // clear old errors
    if SourceEditorManagerIntf<>nil then
      SourceEditorManagerIntf.ClearErrorLines;

    // compile package
    Result:=RunExternalTool(FPCMakeTool);
    if Result<>mrOk then begin
      Result:=IDEMessageDialog(lisFpcmakeFailed,
        Format(lisCallingToCreateMakefileFromFailed, [FPCMakeTool.Filename,
          MakefileFPCFilename]),
        mtError,[mbCancel]);
      exit;
    end;
  finally
    // clean up
    FPCMakeTool.Free;
  end;

  Result:=mrOk;
end;

function TLazPackageGraph.PreparePackageOutputDirectory(APackage: TLazPackage;
  CleanUp: boolean): TModalResult;
var
  OutputDir: String;
  StateFile: String;
  PkgSrcDir: String;
  i: Integer;
  CurFile: TPkgFile;
  OutputFileName: String;
  NewOutputDir: String;
begin
  // get output directory
  OutputDir:=APackage.GetOutputDirectory;
  //debugln(['TLazPackageGraph.PreparePackageOutputDirectory OutputDir="',OutputDir,'"']);

  if not OutputDirectoryIsWritable(APackage,OutputDir,false) then
  begin
    // the normal output directory is not writable
    // => use the fallback directory
    NewOutputDir:=GetFallbackOutputDir(APackage);
    if (NewOutputDir=OutputDir) or (NewOutputDir='') then begin
      debugln(['TLazPackageGraph.PreparePackageOutputDirectory failed to create writable directory: ',OutputDir]);
      exit(mrCancel);
    end;
    APackage.CompilerOptions.ParsedOpts.OutputDirectoryOverride:=NewOutputDir;
    OutputDir:=APackage.GetOutputDirectory;
    if not OutputDirectoryIsWritable(APackage,OutputDir,true) then
    begin
      debugln(['TLazPackageGraph.PreparePackageOutputDirectory failed to create writable directory: ',OutputDir]);
      Result:=mrCancel;
    end;
  end;

  StateFile:=APackage.GetStateFilename;
  PkgSrcDir:=ExtractFilePath(APackage.GetSrcFilename);

  // delete old Compile State file
  if FileExistsUTF8(StateFile) and not DeleteFileUTF8(StateFile) then begin
    Result:=IDEMessageDialog(lisPkgMangUnableToDeleteFilename,
      Format(lisPkgMangUnableToDeleteOldStateFileForPackage, ['"', StateFile,
        '"', #13, APackage.IDAsString]),
      mtError,[mbCancel,mbAbort]);
    exit;
  end;
  InvalidateFileStateCache(StateFile);
  InvalidateStateFile(APackage);

  // create the package src directory
  if not ForceDirectoriesUTF8(PkgSrcDir) then begin
    Result:=IDEMessageDialog(lisPkgMangUnableToCreateDirectory,
      Format(lisPkgMangUnableToCreatePackageSourceDirectoryForPackage, ['"',
        PkgSrcDir, '"', #13, APackage.IDAsString]),
      mtError,[mbCancel,mbAbort]);
    exit;
  end;

  // clean up if wanted
  if CleanUp then begin
    for i:=0 to APackage.FileCount-1 do begin
      CurFile:=APackage.Files[i];
      if not (CurFile.FileType in PkgFileUnitTypes) then continue;
      OutputFileName:=AppendPathDelim(OutputDir)+CurFile.Unit_Name+'.ppu';
      Result:=DeleteFileInteractive(OutputFileName,[mbIgnore,mbAbort]);
      if Result in [mrCancel,mrAbort] then exit;
    end;
  end;

  Result:=mrOk;
end;

function TLazPackageGraph.GetFallbackOutputDir(APackage: TLazPackage): string;
var
  Dir: String;
begin
  // use the default output directory, if it is relative
  // (this way the fallback creates the same amount of target directories)
  Dir:=APackage.CompilerOptions.ParsedOpts.UnparsedValues[pcosOutputDir];
  Dir:=APackage.SubstitutePkgMacros(Dir,false);
  GlobalMacroList.SubstituteStr(Dir);
  if FilenameIsAbsolute(Dir) then begin
    // it is not relative => create a default one
    Dir:='$(TargetOS)-$(TargetCPU)';
  end;
  Dir:='$(FallbackOutputRoot)'+PathDelim+APackage.Name+PathDelim+Dir;
  GlobalMacroList.SubstituteStr(Dir);
  debugln(['TLazPackageGraph.GetFallbackOutputDir  ',APackage.Name,': ',Dir]);
  Result:=Dir;
end;

function TLazPackageGraph.CheckAmbiguousPackageUnits(APackage: TLazPackage
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
        Result:=IDEMessageDialog(lisAmbiguousUnitFound,
          Format(lisTheFileWasFoundInOneOfTheSourceDirectoriesOfThePac, ['"',
            AmbiguousFilename, '"', #13, APackage.IDAsString, #13, #13]),
          mtWarning,[mbYes,mbYesToAll,mbNo,mbAbort])
      else
        Result:=mrYesToAll;
      if Result=mrNo then
        Result:=mrOk;
      if Result in [mrYes,mrYesToAll] then begin
        YesToAll:=Result=mrYesToAll;
        if (not DeleteFileUTF8(AmbiguousFilename))
        and (IDEMessageDialog(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed,
          ['"', AmbiguousFilename, '"']), mtError, [mbIgnore, mbCancel])
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
    CurUnitName:=lowercase(CurFile.Unit_Name);
    if CurUnitName='' then continue;
    Result:=CheckFile(CurUnitName+'.ppu');
    if Result<>mrOk then exit;
    Result:=CheckFile(CurUnitName+'.ppl');
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

function TLazPackageGraph.SavePackageMainSource(APackage: TLazPackage;
  Flags: TPkgCompileFlags; ShowAbort: boolean): TModalResult;
var
  PkgUnitName, SrcFilename, UsedUnits, Src: String;
  i: Integer;
  e: String;
  CurFile: TPkgFile;
  CodeBuffer: TCodeBuffer;
  CurUnitName: String;
  RegistrationCode: String;
  HeaderSrc: String;
  OldShortenSrc: String;
  NeedsRegisterProcCall: boolean;
  CurSrcUnitName: String;
  NewShortenSrc: String;
  BeautifyCodeOptions: TBeautifyCodeOptions;
  AddedUnitNames: TStringToStringTree;

  procedure UseUnit(AnUnitName: string);
  begin
    if AddedUnitNames.Contains(AnUnitName) then exit;
    AddedUnitNames.Add(AnUnitName,'');
    if UsedUnits<>'' then
      UsedUnits:=UsedUnits+', ';
    UsedUnits:=UsedUnits+AnUnitName;
  end;

begin
  {$IFDEF VerbosePkgCompile}
  debugln('TLazPackageGraph.SavePackageMainSource A');
  {$ENDIF}
  SrcFilename:=APackage.GetSrcFilename;

  // delete ambiguous files
  Result:=DeleteAmbiguousFiles(SrcFilename);
  if Result=mrAbort then begin
    DebugLn('TLazPackageGraph.SavePackageMainSource DoDeleteAmbiguousFiles failed');
    exit;
  end;

  // collect unitnames
  e:=LineEnding;
  UsedUnits:='';
  RegistrationCode:='';
  AddedUnitNames:=TStringToStringTree.Create(false);
  try
    for i:=0 to APackage.FileCount-1 do begin
      CurFile:=APackage.Files[i];
      if CurFile.FileType=pftMainUnit then continue;
      // update unitname
      if FilenameIsPascalUnit(CurFile.Filename)
      and (CurFile.FileType in PkgFileUnitTypes) then begin
        NeedsRegisterProcCall:=CurFile.HasRegisterProc
          and (APackage.PackageType in [lptDesignTime,lptRunAndDesignTime]);

        CurUnitName:=ExtractFileNameOnly(CurFile.Filename);

        if not (NeedsRegisterProcCall or CurFile.AddToUsesPkgSection) then
          continue;

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
            if SysUtils.CompareText(CurSrcUnitName,CurFile.Unit_Name)=0 then
              CurFile.Unit_Name:=CurSrcUnitName;
          end;
          if SysUtils.CompareText(CurUnitName,CurFile.Unit_Name)=0 then
            CurUnitName:=CurFile.Unit_Name
          else
            CurFile.Unit_Name:=CurUnitName;
        end;

        if (CurUnitName='') or (not IsValidIdent(CurUnitName)) then begin
          AddMessage(Format(lisIDEInfoWARNINGUnitNameInvalidPackage, [CurFile.
            Filename, APackage.IDAsString]),
             APackage.Directory);
          continue;
        end;

        UseUnit(CurUnitName);
        if NeedsRegisterProcCall then begin
          RegistrationCode:=RegistrationCode+
            '  RegisterUnit('''+CurUnitName+''',@'+CurUnitName+'.Register);'+e;
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
      UseUnit('LazarusPackageIntf');
    end;

  finally
    AddedUnitNames.Free;
  end;
  // create source
  BeautifyCodeOptions:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions;
  // keep in english to avoid svn updates
  HeaderSrc:= '{ This file was automatically created by Lazarus. Do not edit!'+e
           +'  This source is only used to compile and install the package.'+e
           +' }'+e+e;
  // leave the unit case the same as the package name (e.g: package name LazReport, unit name lazreport)
  PkgUnitName := ExtractFileNameOnly(SrcFilename);
  if AnsiSameText(APackage.Name, PkgUnitName) then
    PkgUnitName := APackage.Name;  
  Src:='unit '+ PkgUnitName +';'+e
      +e
      +'interface'+e
      +e;
  Src:=BeautifyCodeOptions.BeautifyStatement(Src,0);
  Src:=HeaderSrc+Src;
  if UsedUnits<>'' then
    Src:=Src
      +'uses'+e
      +BreakString(GetIndentStr(BeautifyCodeOptions.Indent)+UsedUnits+';',
                   BeautifyCodeOptions.LineLength,BeautifyCodeOptions.Indent)+e
      +e;
  Src:=Src+BeautifyCodeOptions.BeautifyStatement(
       'implementation'+e
      +e
      +RegistrationCode
      +'end.'+e,0);

  // check if old code is already uptodate
  Result:=LoadCodeBuffer(CodeBuffer,SrcFilename,[lbfQuiet,lbfCheckIfText,
                            lbfUpdateFromDisk,lbfCreateClearOnError],ShowAbort);
  if Result<>mrOk then begin
    DebugLn('TLazPackageGraph.SavePackageMainSource LoadCodeBuffer ',SrcFilename,' failed');
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
    DebugLn('TLazPackageGraph.SavePackageMainSource Src changed ',dbgs(length(OldShortenSrc)),' ',dbgs(length(NewShortenSrc)));
  end;

  // save source
  Result:=SaveStringToFile(SrcFilename,Src,[],lisPkgMangpackageMainSourceFile);
  if Result<>mrOk then begin
    DebugLn('TLazPackageGraph.SavePackageMainSource SaveStringToFile ',SrcFilename,' failed');
    exit;
  end;

  Result:=mrOk;
end;

function TLazPackageGraph.GetBrokenDependenciesWhenChangingPkgID(
  APackage: TLazPackage; const NewName: string; NewVersion: TPkgVersion
    ): TFPList;
var
  Dependency: TPkgDependency;
begin
  Result:=TFPList.Create;
  // find all dependencies, that will become incompatible
  Dependency:=APackage.FirstUsedByDependency;
  while Dependency<>nil do begin
    if not Dependency.IsCompatible(NewName,NewVersion) then
      Result.Add(Dependency);
    Dependency:=Dependency.NextUsedByDependency;
  end;
end;

procedure TLazPackageGraph.GetPackagesChangedOnDisk(
  var ListOfPackages: TFPList);
// if package source is changed in IDE (codetools)
// then changes on disk are ignored
var
  APackage: TLazPackage;
  i: Integer;
begin
  MarkNeededPackages;
  for i:=FItems.Count-1 downto 0 do begin
    APackage:=TLazPackage(FItems[i]);
    if (not (lpfNeeded in APackage.Flags))
    or APackage.ReadOnly or APackage.Modified
    or (APackage.LPKSource=nil) then
      continue;
    if (not APackage.LPKSource.FileNeedsUpdate) then
      continue;
    if ListOfPackages=nil then
      ListOfPackages:=TFPList.Create;
    ListOfPackages.Add(APackage);
  end;
end;

procedure TLazPackageGraph.CalculateTopologicalLevels;

  procedure GetTopologicalOrder(CurDependency: TPkgDependency;
    out MaxChildLevel: integer);
  var
    RequiredPackage: TLazPackage;
    CurMaxChildLevel: integer;
  begin
    MaxChildLevel:=0;
    while CurDependency<>nil do begin
      if CurDependency.LoadPackageResult=lprSuccess then begin
        RequiredPackage:=CurDependency.RequiredPackage;
        if (not (lpfVisited in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          GetTopologicalOrder(RequiredPackage.FirstRequiredDependency,
                              CurMaxChildLevel);
          RequiredPackage.TopologicalLevel:=CurMaxChildLevel+1;
        end;
        if RequiredPackage.TopologicalLevel>MaxChildLevel then
          MaxChildLevel:=RequiredPackage.TopologicalLevel;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
  end;

var
  i: Integer;
  Pkg: TLazPackage;
  CurMaxChildLevel: integer;
begin
  for i:=FItems.Count-1 downto 0 do begin
    Pkg:=TLazPackage(FItems[i]);
    Pkg.Flags:=Pkg.Flags-[lpfVisited];
    Pkg.TopologicalLevel:=0;
  end;
  for i:=FItems.Count-1 downto 0 do begin
    Pkg:=TLazPackage(FItems[i]);
    GetTopologicalOrder(Pkg.FirstRequiredDependency,CurMaxChildLevel);
    Pkg.TopologicalLevel:=CurMaxChildLevel+1;
  end;
end;

procedure TLazPackageGraph.SortDependencyListTopologically(
  var FirstDependency: TPkgDependency; TopLevelFirst: boolean);
// Sort dependency list topologically.
// If TopLevelFirst is true then packages that needs others come first
var
  Dependency: TPkgDependency;
  BucketStarts: PInteger;
  MaxLvl: Integer;
  BucketCount: Integer;
  DependencyCount: Integer;
  Dependencies: PPkgDependency;
  i: Integer;
  j: Integer;
  CurLvl: LongInt;
begin
  CalculateTopologicalLevels;
  
  // Bucket sort dependencies
  MaxLvl:=0;
  Dependency:=FirstDependency;
  DependencyCount:=0;
  while Dependency<>nil do begin
    if Dependency.RequiredPackage<>nil then begin
      if MaxLvl<Dependency.RequiredPackage.TopologicalLevel then
        MaxLvl:=Dependency.RequiredPackage.TopologicalLevel;
    end;
    Dependency:=Dependency.NextRequiresDependency;
    inc(DependencyCount);
  end;
  if (MaxLvl=0) or (DependencyCount<=1) then exit;
  
  //debugln('TLazPackageGraph.SortDependencyListTopologically A MaxLvl=',dbgs(MaxLvl),' ',dbgs(DependencyCount));
  // compute BucketStarts
  BucketCount:=MaxLvl+1;
  GetMem(BucketStarts,SizeOf(Integer)*BucketCount);
  FillChar(BucketStarts^,SizeOf(Integer)*BucketCount,0);
  Dependency:=FirstDependency;
  while Dependency<>nil do begin
    if Dependency.RequiredPackage<>nil then
      CurLvl:=Dependency.RequiredPackage.TopologicalLevel
    else
      CurLvl:=0;
    if CurLvl+1<BucketCount then
      inc(BucketStarts[CurLvl+1]);
    Dependency:=Dependency.NextRequiresDependency;
  end;
  for i:=2 to MaxLvl do
    BucketStarts[i]:=BucketStarts[i]+BucketStarts[i-1];
  BucketStarts[0]:=0;

  // put Dependencies into buckets
  GetMem(Dependencies,SizeOf(Pointer)*DependencyCount);
  FillChar(Dependencies^,SizeOf(Pointer)*DependencyCount,0);
  Dependency:=FirstDependency;
  while Dependency<>nil do begin
    if Dependency.RequiredPackage<>nil then
      CurLvl:=Dependency.RequiredPackage.TopologicalLevel
    else
      CurLvl:=0;
    if Dependencies[BucketStarts[CurLvl]]<>nil then
      RaiseException('');
    Dependencies[BucketStarts[CurLvl]]:=Dependency;
    inc(BucketStarts[CurLvl]);
    Dependency:=Dependency.NextRequiresDependency;
  end;

  // optional: reverse order
  if TopLevelFirst then begin
    i:=0;
    j:=DependencyCount-1;
    while (i<j) do begin
      Dependency:=Dependencies[i];
      Dependencies[i]:=Dependencies[j];
      Dependencies[j]:=Dependency;
      inc(i);
      dec(j);
    end;
  end;

  // commit order
  FirstDependency:=Dependencies[0];
  for i:=0 to DependencyCount-1 do begin
    Dependency:=Dependencies[i];
    //debugln('TLazPackageGraph.SortDependencyListTopologically A ',Dependency.AsString);
    if i=0 then
      Dependency.PrevDependency[pdlRequires]:=nil
    else
      Dependency.PrevDependency[pdlRequires]:=Dependencies[i-1];
    if i=DependencyCount-1 then
      Dependency.NextDependency[pdlRequires]:=nil
    else
      Dependency.NextDependency[pdlRequires]:=Dependencies[i+1];
  end;

  // clean up
  FreeMem(BucketStarts);
  FreeMem(Dependencies);
end;

function TLazPackageGraph.CheckIfPackageCanBeClosed(APackage: TLazPackage
  ): boolean;
begin
  MarkNeededPackages;
  Result:=lpfNeeded in APackage.Flags;
end;

function TLazPackageGraph.PackageIsNeeded(APackage: TLazPackage): boolean;
// check if package is currently in use (installed, autoinstall, editor open,
// or used by a needed dependency)
// !!! it does not check if any needed package needs this package
begin
  Result:=true;
  // check if package is open, installed or will be installed
  if (APackage.Installed<>pitNope) or (APackage.AutoInstall<>pitNope)
  or ((APackage.Editor<>nil) and (APackage.Editor.Visible))
  or (APackage.HoldPackageCount>0) then
  begin
    exit;
  end;
  Result:=false;
end;

function TLazPackageGraph.PackageCanBeReplaced(
  OldPackage, NewPackage: TLazPackage): boolean;
begin
  if SysUtils.CompareText(OldPackage.Name,NewPackage.Name)<>0 then
    RaiseException('TLazPackageGraph.PackageCanBeReplaced');

  Result:=true;
end;

procedure TLazPackageGraph.RegisterStaticBasePackages;
begin
  BeginUpdate(true);
  
  // register IDE built-in packages (Note: codetools do not need this)
  if Assigned(OnTranslatePackage) then OnTranslatePackage(CodeToolsPackage);

  // register custom IDE components
  RegistrationPackage:=DefaultPackage;
  if IDEComponentPalette<>nil then
    IDEComponentPalette.RegisterCustomIDEComponents(@RegisterCustomIDEComponent);
  if DefaultPackage.FileCount=0 then begin
    FreeThenNil(FDefaultPackage);
  end else begin
    DefaultPackage.Name:=CreateUniquePkgName('DefaultPackage',DefaultPackage);
    AddPackage(DefaultPackage);
  end;
  RegistrationPackage:=nil;

  EndUpdate;
end;

procedure TLazPackageGraph.RegisterStaticPackage(APackage: TLazPackage;
  RegisterProc: TRegisterProc);
begin
  if AbortRegistration then exit;
  //DebugLn(['TLazPackageGraph.RegisterStaticPackage ',APackage.IDAsString]);
  RegistrationPackage:=APackage;
  if Assigned(OnTranslatePackage) then
    OnTranslatePackage(APackage);
  CallRegisterProc(RegisterProc);
  APackage.Registered:=true;
  RegistrationPackage:=nil;
end;

procedure TLazPackageGraph.RegisterDefaultPackageComponent(const Page,
  AUnitName: ShortString; ComponentClass: TComponentClass);
var
  PkgFile: TPkgFile;
  NewPkgFilename: String;
begin
  PkgFile:=FDefaultPackage.FindUnit(AUnitName,true);
  if PkgFile=nil then begin
    NewPkgFilename:=AUnitName+'.pas';
    PkgFile:=FDefaultPackage.AddFile(NewPkgFilename,AUnitName,pftUnit,[],
                                     cpOptional);
  end;
  FRegistrationFile:=PkgFile;
  RegisterComponentsHandler(Page,[ComponentClass]);
end;

procedure TLazPackageGraph.CallRegisterProc(RegisterProc: TRegisterProc);
begin
  if AbortRegistration then exit;

  // check registration procedure
  if RegisterProc=nil then begin
    RegistrationError(lisPkgSysRegisterProcedureIsNil);
    exit;
  end;
  {$IFNDEF StopOnRegError}
  try
  {$ENDIF}
    // call the registration procedure
    RegisterProc();
  {$IFNDEF StopOnRegError}
  except
    on E: Exception do begin
      RegistrationError(E.Message);
    end;
  end;
  {$ENDIF}
end;

procedure TLazPackageGraph.AddDependencyToPackage(APackage: TLazPackage;
  Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  APackage.AddRequiredDependency(Dependency);
  Dependency.LoadPackageResult:=lprUndefined;
  OpenDependency(Dependency,false);
  EndUpdate;
end;

procedure TLazPackageGraph.AddDependencyToPackage(APackage,
  RequiredPackage: TLazPackage);
var
  NewDependency: TPkgDependency;
begin
  NewDependency:=TPkgDependency.Create;
  NewDependency.PackageName:=RequiredPackage.Name;
  AddDependencyToPackage(APackage,NewDependency);
end;

procedure TLazPackageGraph.RemoveDependencyFromPackage(APackage: TLazPackage;
  Dependency: TPkgDependency; AddToRemovedList: boolean);
begin
  BeginUpdate(true);
  if AddToRemovedList then
    APackage.RemoveRequiredDependency(Dependency)
  else
    APackage.DeleteRequiredDependency(Dependency);
  IncreaseBuildMacroChangeStamp;
  EndUpdate;
end;

procedure TLazPackageGraph.ChangeDependency(Dependency,
  NewDependency: TPkgDependency);
begin
  if Dependency.Compare(NewDependency)=0 then exit;
  BeginUpdate(true);
  Dependency.Assign(NewDependency);
  Dependency.LoadPackageResult:=lprUndefined;
  IncreaseBuildMacroChangeStamp;
  OpenDependency(Dependency,false);
  DoDependencyChanged(Dependency);
  EndUpdate;
end;

function TLazPackageGraph.OpenDependency(Dependency: TPkgDependency;
  ShowAbort: boolean): TLoadPackageResult;

  procedure OpenFile(AFilename: string);
  var
    PkgLink: TPackageLink;
  begin
    PkgLink:=PkgLinks.AddUserLink(AFilename,Dependency.PackageName);
    if (PkgLink<>nil) then begin
      PkgLink.Reference;
      if OpenDependencyWithPackageLink(Dependency,PkgLink,false)<>mrOk then
        PkgLinks.RemoveLink(PkgLink);
      PkgLink.Release;
    end;
  end;

var
  ANode: TAVLTreeNode;
  CurDir: String;
  AFilename: String;
  MsgResult: TModalResult;
  APackage: TLazPackage;
  PreferredFilename: string;
  PkgLink: TPackageLink;
begin
  if Dependency.LoadPackageResult=lprUndefined then begin
    //debugln(['TLazPackageGraph.OpenDependency ',Dependency.PackageName,' ',Dependency.DefaultFilename,' Prefer=',Dependency.PreferDefaultFilename]);
    BeginUpdate(false);
    // search compatible package in opened packages
    ANode:=FindNodeOfDependency(Dependency,fpfSearchEverywhere);
    if (ANode<>nil) then begin
      // there is already a package that fits name and version
      APackage:=TLazPackage(ANode.Data);
      Dependency.RequiredPackage:=APackage;
      Dependency.LoadPackageResult:=lprSuccess;
    end;
    // load preferred package
    if (Dependency.DefaultFilename<>'') and Dependency.PreferDefaultFilename
    then begin
      PreferredFilename:=Dependency.FindDefaultFilename;
      //debugln(['TLazPackageGraph.OpenDependency checking preferred Prefer=',PreferredFilename]);
      if (PreferredFilename<>'')
      and ((Dependency.RequiredPackage=nil)
        or ((Dependency.RequiredPackage.FindUsedByDepPrefer(Dependency)=nil)
            and (CompareFilenames(PreferredFilename,Dependency.RequiredPackage.Filename)<>0)))
      then begin
        OpenFile(PreferredFilename);
      end;
    end;
    if Dependency.LoadPackageResult=lprUndefined then begin
      // no compatible package yet open
      Dependency.RequiredPackage:=nil;
      Dependency.LoadPackageResult:=lprNotFound;
      APackage:=FindAPackageWithName(Dependency.PackageName,nil);
      if APackage=nil then begin
        // no package with same name open
        // -> try package links
        repeat
          PkgLink:=PkgLinks.FindLinkWithDependency(Dependency);
          if (PkgLink=nil) then break;
          PkgLink.Reference;
          try
            MsgResult:=OpenDependencyWithPackageLink(Dependency,PkgLink,ShowAbort);
            if MsgResult=mrOk then break;
            PkgLinks.RemoveLink(PkgLink);
          finally
            PkgLink.Release;
          end;
        until MsgResult=mrAbort;
        // try defaultfilename
        if (Dependency.LoadPackageResult=lprNotFound)
        and (Dependency.DefaultFilename<>'') then begin
          AFilename:=Dependency.FindDefaultFilename;
          if AFilename<>'' then begin
            OpenFile(AFilename);
          end;
        end;
        // try in owner directory (some projects put all their packages into
        //   one directory)
        if Dependency.LoadPackageResult=lprNotFound then begin
          CurDir:=GetDependencyOwnerDirectory(Dependency);
          if (CurDir<>'') then begin
            AFilename:=FindDiskFileCaseInsensitive(
                         AppendPathDelim(CurDir)+Dependency.PackageName+'.lpk');
            if FileExistsCached(AFilename) then begin
              OpenFile(AFilename);
            end;
          end;
        end;
      end else begin
        // there is already a package with this name, but wrong version open
        // -> unable to load this dependency due to conflict
        debugln(['TLazPackageGraph.OpenDependency another package with wrong version is already open: Dependency=',Dependency.AsString,' Pkg=',APackage.IDAsString]);
        Dependency.LoadPackageResult:=lprLoadError;
      end;
    end;
    fChanged:=true;
    IncreaseBuildMacroChangeStamp;
    EndUpdate;
  end;
  Result:=Dependency.LoadPackageResult;
end;

procedure TLazPackageGraph.OpenInstalledDependency(Dependency: TPkgDependency;
  InstallType: TPackageInstallType; var Quiet: boolean);
var
  BrokenPackage: TLazPackage;
  CurResult: TModalResult;
  BasePackage: TLazPackage;
begin
  OpenDependency(Dependency,false);
  if Dependency.LoadPackageResult<>lprSuccess then begin
    // a valid lpk file of the installed package can not be found
    if IsStaticBasePackage(Dependency.PackageName) then begin
      // this is one of the Lazarus base packages
      // auto create the built in version
      BasePackage:=CreateLazarusBasePackage(Dependency.PackageName);
      if BasePackage<>nil then begin
        AddPackage(BasePackage);
        //DebugLn('TLazPackageGraph.OpenInstalledDependency lpk not found using built-in ',BasePackage.IDAsString,' ',dbgs(ord(BasePackage.AutoInstall)));
        if not Quiet then begin
          // don't bother the user
        end;
      end;
    end else begin
      // -> create a broken package
      BrokenPackage:=TLazPackage.Create;
      with BrokenPackage do begin
        BeginUpdate;
        Missing:=true;
        AutoCreated:=true;
        Name:=Dependency.PackageName;
        Filename:='';
        Version.SetValues(0,0,0,0);
        Author:='?';
        License:='?';
        AutoUpdate:=pupManually;
        Description:=lisPkgSysThisPackageIsInstalledButTheLpkFileWasNotFound;
        PackageType:=lptDesignTime;
        Installed:=pitStatic;
        AutoInstall:=pitNope;
        CompilerOptions.UnitOutputDirectory:='';

        // add lazarus registration unit path
        UsageOptions.UnitPath:='';

        Modified:=false;
        EndUpdate;
      end;
      AddPackage(BrokenPackage);
      DebugLn('TLazPackageGraph.OpenInstalledDependency ',BrokenPackage.IDAsString,' ',dbgs(ord(BrokenPackage.AutoInstall)));
      if (not Quiet) and DirPathExistsCached(PkgLinks.GetGlobalLinkDirectory)
      then begin
        // tell the user
        CurResult:=QuestionDlg(lisPkgSysPackageFileNotFound,
          Format(lisPkgSysThePackageIsInstalledButNoValidPackageFileWasFound, ['"',
            BrokenPackage.Name, '"', #13]),
          mtError,[mrOk,mrYesToAll,'Skip these warnings'],0);
        if CurResult=mrYesToAll then
          Quiet:=true;
      end;
    end;

    // open it
    if OpenDependency(Dependency,false)<>lprSuccess then
      RaiseException('TLazPackageGraph.OpenInstalledDependency');
  end;
  Dependency.RequiredPackage.Installed:=InstallType;
end;

procedure TLazPackageGraph.OpenRequiredDependencyList(
  FirstDependency: TPkgDependency);
var
  Dependency: TPkgDependency;
begin
  Dependency:=FirstDependency;
  while Dependency<>nil do begin
    OpenDependency(Dependency,false);
    Dependency:=Dependency.NextRequiresDependency;
  end;
end;

procedure TLazPackageGraph.MoveRequiredDependencyUp(
  ADependency: TPkgDependency);
begin
  if (ADependency=nil) or (ADependency.Removed) or (ADependency.Owner=nil)
  or (ADependency.PrevRequiresDependency=nil)
  or (not (ADependency.Owner is TLazPackage))
  then exit;
  BeginUpdate(true);
  TLazPackage(ADependency.Owner).MoveRequiredDependencyUp(ADependency);
  EndUpdate;
end;

procedure TLazPackageGraph.MoveRequiredDependencyDown(
  ADependency: TPkgDependency);
begin
  if (ADependency=nil) or (ADependency.Removed) or (ADependency.Owner=nil)
  or (ADependency.NextRequiresDependency=nil)
  or (not (ADependency.Owner is TLazPackage))
  then exit;
  BeginUpdate(true);
  TLazPackage(ADependency.Owner).MoveRequiredDependencyDown(ADependency);
  EndUpdate;
end;

procedure TLazPackageGraph.IterateComponentClasses(APackage: TLazPackage;
  Event: TIterateComponentClassesEvent; WithUsedPackages,
  WithRequiredPackages: boolean);
var
  ARequiredPackage: TLazPackage;
  ADependency: TPkgDependency;
begin
  APackage.IterateComponentClasses(Event,WithUsedPackages);
  // iterate through all required packages
  if WithRequiredPackages then begin
    ADependency:=APackage.FirstRequiredDependency;
    while ADependency<>nil do begin
      ARequiredPackage:=FindOpenPackage(ADependency,[fpfSearchInInstalledPckgs]);
      if ARequiredPackage<>nil then begin
        ARequiredPackage.IterateComponentClasses(Event,false);
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
  end;
end;

procedure TLazPackageGraph.IterateAllComponentClasses(
  Event: TIterateComponentClassesEvent);
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do
    IterateComponentClasses(Packages[i],Event,false,false);
end;

procedure TLazPackageGraph.IteratePackages(Flags: TFindPackageFlags;
  Event: TIteratePackagesEvent);
var
  CurPkg: TLazPackage;
  i: Integer;
begin
  // iterate opened packages
  for i:=0 to FItems.Count-1 do begin
    CurPkg:=Packages[i];
    // check installed packages
    if ((fpfSearchInInstalledPckgs in Flags) and (CurPkg.Installed<>pitNope))
    // check autoinstall packages
    or ((fpfSearchInAutoInstallPckgs in Flags) and (CurPkg.AutoInstall<>pitNope))
    // check packages with opened editor
    or ((fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil))
    then begin
      Event(CurPkg);
    end;
  end;
  // iterate in package links
  if (fpfSearchInPkgLinks in Flags) then begin
    PkgLinks.IteratePackages(fpfPkgLinkMustExist in Flags,Event);
  end;
end;

procedure TLazPackageGraph.IteratePackagesSorted(Flags: TFindPackageFlags;
  Event: TIteratePackagesEvent);
var
  ANode: TAVLTreeNode;
  CurPkg: TLazPackage;
begin
  ANode:=FTree.FindLowest;
  while ANode<>nil do begin
    CurPkg:=TLazPackage(ANode.Data);
    // check installed packages
    if ((fpfSearchInInstalledPckgs in Flags) and (CurPkg.Installed<>pitNope))
    // check autoinstall packages
    or ((fpfSearchInAutoInstallPckgs in Flags) and (CurPkg.AutoInstall<>pitNope))
    // check packages with opened editor
    or ((fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil))
    then
      Event(CurPkg);
    ANode:=FTree.FindSuccessor(ANode);
  end;
end;

procedure TLazPackageGraph.GetAllRequiredPackages(
  FirstDependency: TPkgDependency; out List: TFPList);
// returns packages in topological order, beginning with the top level package

  procedure GetTopologicalOrder(CurDependency: TPkgDependency);
  var
    RequiredPackage: TLazPackage;
  begin
    while CurDependency<>nil do begin
      //debugln('TLazPackageGraph.GetAllRequiredPackages A ',CurDependency.AsString,' ',dbgs(ord(CurDependency.LoadPackageResult)),' ',dbgs(ord(lprSuccess)));
      if CurDependency.LoadPackageResult=lprSuccess then begin
        //debugln('TLazPackageGraph.GetAllRequiredPackages B ',CurDependency.AsString);
        RequiredPackage:=CurDependency.RequiredPackage;
        if (not (lpfVisited in RequiredPackage.Flags)) then begin
          RequiredPackage.Flags:=RequiredPackage.Flags+[lpfVisited];
          GetTopologicalOrder(RequiredPackage.FirstRequiredDependency);
          // add package to list
          if List=nil then List:=TFPList.Create;
          List.Add(RequiredPackage);
        end;
      end;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
  end;

var
  i: Integer;
  j: Integer;
begin
  List:=nil;
  MarkAllPackagesAsNotVisited;
  // create topological list, beginning with the leaves
  GetTopologicalOrder(FirstDependency);
  // reverse list order
  if List<>nil then begin
    i:=0;
    j:=List.Count-1;
    while i<j do begin
      List.Exchange(i,j);
      inc(i);
      dec(j);
    end;
  end;
end;

procedure TLazPackageGraph.GetConnectionsTree(FirstDependency: TPkgDependency;
  var PkgList: TFPList; var Tree: TPkgPairTree);
  
  procedure AddConnection(Pkg1, Pkg2: TLazPackage);
  begin
    if Pkg1=Pkg2 then exit;
    if Tree=nil then
      Tree:=TPkgPairTree.Create;
    Tree.AddPairIfNotExists(Pkg1,Pkg2);
  end;
  
  procedure AddConnections(StartDependency: TPkgDependency);
  // add every connection between owner and required package
  // and between two children
  var
    OwnerPackage: TLazPackage;
    Dependency1: TPkgDependency;
    Dependency2: TPkgDependency;
    Pkg1: TLazPackage;
    Pkg2: TLazPackage;
  begin
    if StartDependency=nil then exit;
    if (StartDependency.Owner is TLazPackage) then
      OwnerPackage:=TLazPackage(StartDependency.Owner)
    else
      OwnerPackage:=nil;
    Dependency1:=StartDependency;
    while Dependency1<>nil do begin
      Pkg1:=Dependency1.RequiredPackage;
      if Pkg1<>nil then begin
        // add connection between owner and required package
        if OwnerPackage<>nil then
          AddConnection(OwnerPackage,Pkg1);
        // add connections between any two direct required packages
        Dependency2:=StartDependency;
        while Dependency2<>nil do begin
          Pkg2:=Dependency2.RequiredPackage;
          if Pkg2<>nil then
            AddConnection(Pkg1,Pkg2);
          Dependency2:=Dependency2.NextDependency[pdlRequires];
        end;
      end;
      Dependency1:=Dependency1.NextDependency[pdlRequires];
    end;
  end;
  
var
  i: Integer;
  Pkg: TLazPackage;
begin
  if Tree<>nil then Tree.FreeAndClear;
  GetAllRequiredPackages(FirstDependency,PkgList);
  if PkgList=nil then exit;
  AddConnections(FirstDependency);
  for i:=0 to PkgList.Count-1 do begin
    Pkg:=TLazPackage(PkgList[i]);
    AddConnections(Pkg.FirstRequiredDependency);
  end;
end;

initialization
  PackageGraph:=nil;

end.


