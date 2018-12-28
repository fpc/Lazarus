{
 /***************************************************************************
                            packagedefs.pas
                            ---------------


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Classes for packages and dependencies.
}
unit PackageDefs;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, SysUtils, contnrs, typinfo, Laz_AVL_Tree,
  // LCL
  LCLType, LResources, Graphics, Controls, Forms, Dialogs,
  // Codetools
  FileProcs, LazConfigStorage, BasicCodeTools, DefineTemplates, CodeToolManager,
  CodeCache, CodeToolsCfgScript, CodeToolsStructs,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache, LazUTF8, LazTracer, LazUtilities,
  Laz2_XMLCfg, AvgLvlTree,
  // IDEIntf
  PropEdits, LazIDEIntf, MacroIntf, MacroDefIntf, IDEOptionsIntf, IDEOptEditorIntf,
  PackageDependencyIntf, PackageIntf, IDEDialogs, ComponentReg, IDEImagesIntf,
  // IDE
  EditDefineTree, CompilerOptions, CompOptsModes, IDEOptionDefs, ProjPackCommon,
  LazarusIDEStrConsts, IDEProcs, TransferMacros, FileReferenceList,
  PublishModule, ImgList, FppkgHelper, FppkgIntf;

type
  TLazPackage = class;
  TPkgFile = class;
  TBasePackageEditor = class;
  TPkgDependency = class;

  TPackageUpdatePolicy = (
    pupManually,
    pupOnRebuildingAll,
    pupAsNeeded
    );
  TPackageUpdatePolicies = set of TPackageUpdatePolicy;

  TGetAllRequiredPackagesEvent =
    procedure(APackage: TLazPackage; // if not nil then ignore FirstDependency and do not add APackage to Result
              FirstDependency: TPkgDependency;
              out List, FPMakeList: TFPList;
              Flags: TPkgIntfRequiredFlags = [];
              MinPolicy: TPackageUpdatePolicy = low(TPackageUpdatePolicy)) of object;
  TGetDependencyOwnerDescription =
    procedure(Dependency: TPkgDependency; out Description: string) of object;
  TGetDependencyOwnerDirectory =
    procedure(Dependency: TPkgDependency; out Directory: string) of object;
  TGetWritablePkgOutputDirectory =
    procedure(APackage: TLazPackage; var AnOutDirectory: string) of object;


  { TPkgComponent }
  
  TPkgComponent = class(TRegisteredComponent)
  private
    FPkgFile: TPkgFile;
    procedure SetPkgFile(const AValue: TPkgFile);
  public
    constructor Create(ThePkgFile: TPkgFile; TheComponentClass: TComponentClass;
                       const ThePageName: string);
    destructor Destroy; override;
    function GetUnitName: string; override;
    function GetPriority: TComponentPriority; override;
    procedure ConsistencyCheck; override;
    function ImageIndex: TImageIndex;
    class function Images: TCustomImageList;
    function HasIcon: boolean;
    function CanBeCreatedInDesigner: boolean; override;
  public
    property PkgFile: TPkgFile read FPkgFile write SetPkgFile;
  end;

  { TPkgFile }

type
  TPFComponentBaseClass = (
    pfcbcNone,      // unknown
    pfcbcForm,      // is TForm
    pfcbcFrame,     // is TFrame
    pfcbcDataModule // is TDataModule
    );
    
const
  PFComponentBaseClassNames: array[TPFComponentBaseClass] of string = (
    'None',
    'Form',
    'Frame',
    'DataModule'
    );
    
function StrToComponentBaseClass(const s: string): TPFComponentBaseClass;
function GetComponentBaseClass(aClass: TClass): TPFComponentBaseClass;

type
  TPkgFileFlag = (
    pffHasRegisterProc,  // file is unit and has a 'register' procedure
    pffAddToPkgUsesSection,// unit is added to uses section
    pffReportedAsRemoved // file has been reported as removed
    );
  TPkgFileFlags = set of TPkgFileFlag;
  
  { TPkgFile }

  TPkgFile = class(TLazPackageFile)
  private
    FAutoReferenceSourceDir: boolean;
    FComponentPriority: TComponentPriority;
    FComponents: TFPList; // list of TPkgComponent
    FDirectory: string;
    FFlags: TPkgFileFlags;
    fFilename: string;
    fFullFilename: string;
    fFullFilenameStamp: integer;
    FPackage: TLazPackage;
    FResourceBaseClass: TPFComponentBaseClass;
    FSourceDirectoryReferenced: boolean;
    FSourceDirNeedReference: boolean;
    function GetAddToUsesPkgSection: boolean;
    function GetComponents(Index: integer): TPkgComponent;
    function GetHasRegisterProc: boolean;
    procedure SetAddToUsesPkgSection(const AValue: boolean);
    procedure SetAutoReferenceSourceDir(const AValue: boolean);
    procedure SetFlags(const AValue: TPkgFileFlags);
    procedure SetHasRegisterProc(const AValue: boolean);
    procedure UpdateUnitName;
    function GetComponentList: TFPList;
  protected
    function GetInUses: boolean; override;
    procedure SetInUses(AValue: boolean); override;
    function GetIDEPackage: TIDEPackage; override;
    function GetFilename: string; override;
    procedure SetFilename(const AValue: string); override;
    procedure SetRemoved(const AValue: boolean); override;
    procedure SetDisableI18NForLFM(AValue: boolean); override;
    procedure SetFileType(const AValue: TPkgFileType); override;
    procedure SetUnitName(const AValue: string); override;
  public
    constructor Create(ThePackage: TLazPackage);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer; AdjustPathDelims: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch);
    procedure ConsistencyCheck;
    function ComponentCount: integer;
    procedure AddPkgComponent(APkgComponent: TPkgComponent);
    procedure RemovePkgComponent(APkgComponent: TPkgComponent);
    function HasRegisteredPlugins: boolean;
    function MakeSense: boolean;
    procedure UpdateSourceDirectoryReference;
    function GetFullFilename: string; override;
    function GetShortFilename(UseUp: boolean): string; override;
    function GetResolvedFilename: string; // GetFullFilename + resolve symlinks
    function GetFileOwner: TObject; override;
    function GetFileOwnerName: string; override;
  public
    property AddToUsesPkgSection: boolean
                       read GetAddToUsesPkgSection write SetAddToUsesPkgSection;
    property AutoReferenceSourceDir: boolean read FAutoReferenceSourceDir
                                             write SetAutoReferenceSourceDir;
    property ResourceBaseClass: TPFComponentBaseClass read FResourceBaseClass
                                                      write FResourceBaseClass;
    property ComponentPriority: TComponentPriority read FComponentPriority
                                                   write FComponentPriority;
    property Components[Index: integer]: TPkgComponent read GetComponents;// registered components
    property Directory: string read FDirectory;
    property Flags: TPkgFileFlags read FFlags write SetFlags;
    property HasRegisterProc: boolean read GetHasRegisterProc write SetHasRegisterProc;
    property LazPackage: TLazPackage read FPackage;
    property SourceDirectoryReferenced: boolean read FSourceDirectoryReferenced;
  end;
  
  
  { TPkgUnitsTree - Tree of TPkgFile sorted for unitnames }
  
  TPkgUnitsTree = class(TAVLTree)
  private
    FLazPackage: TLazPackage;
  public
    function FindNodeWithUnitName(const AUnitName: string): TAVLTreeNode;
    function FindPkgFileWithUnitName(const AUnitName: string): TPkgFile;
    constructor Create(ThePackage: TLazPackage);
    property LazPackage: TLazPackage read FLazPackage write FLazPackage;
  end;
  
  
  { TPkgDependency }
  
  TPkgMarkerFlag = (
    pmfVisited,
    pmfMarked
    );
  TPkgMarkerFlags = set of TPkgMarkerFlag;
  
  TPkgDependencyList = (
    pdlRequires,
    pdlUsedBy
    );

  TPkgDependencyType = (
    pdtLazarus,
    pdtFPMake
    );

const
  PkgDependencyTypeNames: array[TPkgDependencyType] of string = (
    'Lazarus',
    'FPMake'
    );

type

  { TPkgDependency }

  TPkgDependency = class(TPkgDependencyID)
  private
    FDefaultFilename: string;
    FDependencyType: TPkgDependencyType;
    FHoldPackage: boolean;
    FMarkerFlags: TPKgMarkerFlags;
    FOwner: TObject;
    FPreferDefaultFilename: boolean;
    function GetRequiredPackage: TLazPackage;
    procedure SetHoldPackage(const AValue: boolean);
    procedure SetRequiredPackage(AValue: TLazPackage);
  protected
    procedure SetPackageName(const AValue: string); override;
  public
    NextDependency, PrevDependency: array[TPkgDependencyList] of TPkgDependency;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch);

    function Compare(Dependency2: TPkgDependency): integer;
    procedure Assign(Source: TPkgDependency);
    procedure Assign(Source: TLazPackageID);
    procedure ConsistencyCheck;
    function IsCompatible(Pkg: TLazPackageID): boolean; overload;
    procedure MakeCompatible(const PkgName: string; const Version: TPkgVersion);
    function AsString(WithOwner: boolean = false; WithDefaults: boolean = false): string;
    // API for iterating dependencies.
    function NextUsedByDependency: TPkgDependency; override;
    function PrevUsedByDependency: TPkgDependency; override;
    function NextRequiresDependency: TPkgDependency; override;
    function PrevRequiresDependency: TPkgDependency; override;
    // API for adding / removing dependencies, defined in base class.
    procedure AddUsedByDep(var FirstDependency: TPkgDependencyBase); override;
    procedure RemoveUsedByDep(var FirstDependency: TPkgDependencyBase); override;
    procedure AddRequiresDep(var FirstDependency: TPkgDependencyBase); override;
    procedure RemoveRequiresDep(var FirstDependency: TPkgDependencyBase); override;
    // API using ListType.
    procedure AddToList(var FirstDependency: TPkgDependency;
                        ListType: TPkgDependencyList);
    procedure AddToEndOfList(var LastDependency: TPkgDependency;
                             ListType: TPkgDependencyList);
    procedure RemoveFromList(var FirstDependency: TPkgDependency;
                             ListType: TPkgDependencyList);
    function MoveUpInList(var FirstDependency: TPkgDependency;
                          ListType: TPkgDependencyList): Boolean;
    function MoveDownInList(var FirstDependency: TPkgDependency;
      ListType: TPkgDependencyList): Boolean;
    function MakeFilenameRelativeToOwner(const AFilename: string): string;
    function FindDefaultFilename: string;
  public
    property Owner: TObject read FOwner write FOwner;// package or project or IDE
    property HoldPackage: boolean read FHoldPackage write SetHoldPackage;
    property MarkerFlags: TPKgMarkerFlags read FMarkerFlags write FMarkerFlags;
    property DefaultFilename: string read FDefaultFilename write FDefaultFilename;
    property PreferDefaultFilename: boolean read FPreferDefaultFilename write FPreferDefaultFilename;
    property RequiredPackage: TLazPackage read GetRequiredPackage write SetRequiredPackage;
    property DependencyType: TPkgDependencyType read FDependencyType write FDependencyType;
  end;
  PPkgDependency = ^TPkgDependency;


  { TPkgPair }

  TPkgPair = class
  public
    Package1: TLazPackage;
    Package2: TLazPackage;
    constructor Create(Pkg1, Pkg2: TLazPackage);
    function ComparePair(Pkg1, Pkg2: TLazPackage): integer;
    function Compare(PkgPair: TPkgPair): integer;
    function AsString: string;
  end;
  
  
  { TPkgPairTree - Tree of TPkgPair }
  
  TPkgPairTree = class(TAVLTree)
  public
    constructor Create;
    destructor Destroy; override;
    function FindPair(Pkg1, Pkg2: TLazPackage; IgnoreOrder: boolean): TPkgPair;
    function AddPair(Pkg1, Pkg2: TLazPackage): TPkgPair;
    function AddPairIfNotExists(Pkg1, Pkg2: TLazPackage): TPkgPair;
  end;


  { TPkgCompilerOptions }
  
  TPkgCompilerOptions = class(TBaseCompilerOptions)
  private
    FLazPackage: TLazPackage;
    FSkipCompiler: Boolean;
    procedure InvalidateOptions;
  protected
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetCustomOptions(const AValue: string); override;
    procedure SetIncludePaths(const AValue: string); override;
    procedure SetLibraryPaths(const AValue: string); override;
    procedure SetLinkerOptions(const AValue: string); override;
    procedure SetObjectPath(const AValue: string); override;
    procedure SetSrcPath(const AValue: string); override;
    procedure SetUnitPaths(const AValue: string); override;
    procedure SetUnitOutputDir(const AValue: string); override;
    procedure SetConditionals(AValue: string); override;
  public
    constructor Create(const AOwner: TObject); override;
    // IDE options
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function IsActive: boolean; override;
    procedure Clear; override;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList); override;
    function GetOwnerName: string; override;
    function GetDefaultMainSourceFileName: string; override;
    function CreateTargetFilename: string; override;
    function HasCompilerCommand: boolean; override;

    procedure LoadFromXMLConfig(AXMLConfig: TXMLConfig; const Path: string); override;
    procedure SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string); override;
    procedure Assign(Source: TPersistent); override;
    function CreateDiff(CompOpts: TBaseCompilerOptions;
                        Tool: TCompilerDiffTool = nil): boolean; override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property SkipCompiler: Boolean read FSkipCompiler write FSkipCompiler;
  end;
  
  
  { TPkgAdditionalCompilerOptions }
  
  TPkgAdditionalCompilerOptions = class(TAdditionalCompilerOptions)
  private
    FLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
  protected
    procedure SetCustomOptions(const AValue: string); override;
    procedure SetIncludePath(const AValue: string); override;
    procedure SetLibraryPath(const AValue: string); override;
    procedure SetLinkerOptions(const AValue: string); override;
    procedure SetObjectPath(const AValue: string); override;
    procedure SetUnitPath(const AValue: string); override;
    procedure SetSrcPath(const AValue: string); override;
  public
    constructor Create(ThePackage: TLazPackage);
    procedure AssignOptions(Source: TObject); override;
    function GetOwnerName: string; override;
    function GetBaseCompilerOptions: TBaseCompilerOptions; override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
  { TPublishPackageOptions }

  TPublishPackageOptions = class(TPublishModuleOptions)
  protected
    procedure DoOnModifyChange; override;
  public
    function GetDefaultDestinationDir: string; override;
  end;
  
  
  { TLazPackageDefineTemplates }
  
  TLazPackageDefineTemplates = class(TProjPackDefineTemplates)
  private
  protected
    procedure UpdateMain; override;
    procedure UpdateSrcDirIfDef; override;
    procedure UpdateSourceDirectories; override;
    procedure UpdateOutputDirectory; override;
    procedure UpdateDefinesForCustomDefines; override;
    procedure ClearFlags; override;
  public
    constructor Create(AOwner: IProjPack);
    destructor Destroy; override;
    procedure AllChanged; override;
  end;
  

  { TLazPackage }
  
  TLazPackageFlag = (
    lpfAutoIncrementVersionOnBuild, // increment version before
    lpfModified,       // package needs saving
    lpfNeeded,         // Set by PackageGraph, if package is in use
                       //   (for example because it is Installed or an Installed
                       //    package requires this package)
    lpfVisited,        // Used by the PackageGraph to avoid double checking
    lpfDestroying,     // set during destruction
    lpfLoading,        // set during loading
    lpfSkipSaving,     // Used by PkgBoss to skip saving
    lpfCycle,          // Used by the PackageGraph to mark cycles
    lpfNeedGroupCompile     // set during group compile, dependent packages need compile too
    );
  TLazPackageFlags = set of TLazPackageFlag;
  
const
  pupAllAuto = [pupAsNeeded,pupOnRebuildingAll];
  
type
  TPkgOutputDirWritable = (
    podwUnknown,
    podwWritable,
    podwNotWritable
    );
  TPkgLastCompileStats = record
    StateFileLoaded: boolean;
    StateFileName: string; // the .compiled file
    StateFileDate: longint;
    CompilerFilename: string; // path to used compiler
    CompilerFileDate: integer;
    Params: string;        // compiler parameters
    Complete: boolean;     // compilation was successful
    MainPPUExists: boolean; // main ppu file was there after compile
    ViaMakefile: boolean;  // compiled via make
    DirectoryWritable: TPkgOutputDirWritable;
  end;
  PPkgLastCompileStats = ^TPkgLastCompileStats;
  TPkgOutputDir = (
    podDefault,
    podFallback // used when podDefault is not writable
    );

  TIterateComponentClassesEvent = procedure(PkgComponent: TPkgComponent) of object;
  TPkgChangeNameEvent = procedure(Pkg: TLazPackage; const OldName: string) of object;

  { TPackageIDEOptions }

  TPackageIDEOptions = class(TAbstractPackageIDEOptions)
  private
    FPackage: TLazPackage;
  protected
    function GetPackage: TIDEPackage; override;
  public
    constructor Create(APackage: TLazPackage);
    destructor Destroy; override;
    class function GetInstance: TAbstractIDEOptions; override;
    class function GetGroupCaption: string; override;
    property Package: TLazPackage read FPackage;
  end;


  { TLazPackage }

  TLazPackage = class(TIDEPackage, IProjPack)
  private
    FAddToProjectUsesSection: boolean;
    FAuthor: string;
    FAutoUpdate: TPackageUpdatePolicy;
    FFPDocPackageName: string;
    FOnModifySilently: TNotifyEvent;
    FOptionsBackup: TLazPackage;
    FComponents: TFPList; // TFPList of TPkgComponent
    FDefineTemplates: TLazPackageDefineTemplates;
    FDescription: string;
    FDirectory: string;
    FDirectoryExpanded: string;
    FDirectoryExpandedChangeStamp: integer;
    FEnableI18N: boolean;
    FEnableI18NForLFM: boolean;
    FFileReadOnly: boolean;
    FFiles: TFPList; // TFPList of TPkgFile
    FFirstRemovedDependency: TPkgDependency;
    FFirstRequiredDependency: TPkgDependency;
    FFirstUsedByDependency: TPkgDependency;
    FFlags: TLazPackageFlags;
    FHasDirectory: boolean;
    FHasStaticDirectory: boolean;
    FHoldPackageCount: integer;
    FIconFile: string;
    FInstalled: TPackageInstallType;
    FFPDocPaths: string;
    FLicense: string;
    FLPKSource: TCodeBuffer;
    FLPKSourceChangeStep: integer;
    FMacros: TTransferMacroList;
    FMainUnit: TPkgFile;
    FMissing: boolean;
    FModifiedLock: integer;
    FOutputStateFile: string;
    FPackageEditor: TBasePackageEditor;
    FPOOutputDirectory: string;
    FProvides: TStrings;
    fPublishOptions: TPublishPackageOptions;
    FRegistered: boolean;
    FRemovedFiles: TFPList; // TFPList of TPkgFile
    FSourceDirectories: TFileReferenceList;
    FStorePathDelim: TPathDelimSwitch;
    FTopologicalLevel: integer;
    FTranslated: string;
    FUpdateLock: integer;
    FUsageOptions: TPkgAdditionalCompilerOptions;
    FUserIgnoreChangeStamp: integer;
    FUserReadOnly: boolean;
    function GetAutoIncrementVersionOnBuild: boolean;
    function GetCompilerOptions: TPkgCompilerOptions;
    function GetBaseCompilerOptions: TBaseCompilerOptions;
    function GetComponentCount: integer;
    function GetComponents(Index: integer): TPkgComponent;
    function GetRemovedFiles(Index: integer): TPkgFile;
    function GetFiles(Index: integer): TPkgFile;
    function GetIDEOptions: TPackageIDEOptions;
    function GetSourceDirectories: TFileReferenceList;
    procedure SetAddToProjectUsesSection(const AValue: boolean);
    procedure SetAuthor(const AValue: string);
    procedure SetAutoIncrementVersionOnBuild(const AValue: boolean);
    procedure SetAutoUpdate(const AValue: TPackageUpdatePolicy);
    procedure SetDescription(const AValue: string);
    procedure SetEnableI18NForLFM(AValue: boolean);
    procedure SetFileReadOnly(const AValue: boolean);
    procedure SetFlags(const AValue: TLazPackageFlags);
    procedure SetFPDocPackageName(AValue: string);
    procedure SetIconFile(const AValue: string);
    procedure SetInstalled(const AValue: TPackageInstallType);
    procedure SetFPDocPaths(const AValue: string);
    procedure SetLicense(const AValue: string);
    procedure SetLPKSource(const AValue: TCodeBuffer);
    procedure SetOutputStateFile(const AValue: string);
    procedure SetProvides(const AValue: TStrings);
    procedure SetPOOutputDirectory(const AValue: string);
    procedure SetEnableI18N(const AValue: boolean);
    procedure SetRegistered(const AValue: boolean);
    procedure SetPackageEditor(const AValue: TBasePackageEditor);
    procedure SetPackageType(const AValue: TLazPackageType);
    procedure SetStorePathDelim(const AValue: TPathDelimSwitch);
    procedure SetUserReadOnly(const AValue: boolean);
    procedure OnMacroListSubstitution({%H-}TheMacro: TTransferMacro;
      const MacroName: string; var s: string;
      const Data: PtrInt; var Handled, {%H-}Abort: boolean; {%H-}Depth: integer);
    procedure Clear;
    procedure UpdateSourceDirectories;
    procedure SourceDirectoriesChanged(Sender: TObject);
  protected
    function GetDirectory: string; override;
    function GetDefineTemplates: TProjPackDefineTemplates;
    function GetFileCount: integer; override;
    function GetPkgFiles(Index: integer): TLazPackageFile; override;
    function GetDirectoryExpanded: string; override;
    function GetModified: boolean; override;
    procedure SetFilename(const AValue: string); override;
    procedure SetModified(const AValue: boolean); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure VersionChanged(Sender: TObject); override;
    function GetRemovedCount: integer; override;
    function GetRemovedPkgFiles(Index: integer): TLazPackageFile; override;
    procedure SetAutoInstall(AValue: TPackageInstallType); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AssignOptions(Source: TPersistent); override;
    // IDE options
    procedure BackupOptions;
    procedure RestoreOptions;
    // modified
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LockModified;
    procedure UnlockModified;
    function ReadOnly: boolean; override;
    procedure ModifySilently; // Set Modified but do not trigger update of editors.
    // streaming
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToString(out s: string);
    // consistency
    procedure CheckInnerDependencies;
    function IsMakingSense: boolean;
    procedure ConsistencyCheck;
    // paths, define templates
    function ExtendUnitSearchPath(NewUnitPaths: string): boolean;
    function ExtendIncSearchPath(NewIncPaths: string): boolean;
    function IsVirtual: boolean; override;
    function HasDirectory: boolean; override;
    function HasStaticDirectory: boolean;
    function GetFullFilename(ResolveMacros: boolean): string;
    function GetResolvedFilename(ResolveMacros: boolean): string; // GetFullFilename + resolve symlinks
    function GetSourceDirs(WithPkgDir, WithoutOutputDir: boolean): string;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList);
    function GetOutputDirectory(UseOverride: boolean = true): string; // this can change before building, when default dir is readonly
    function HasSeparateOutputDirectory: boolean;
    function GetStateFilename(UseOverride: boolean = true): string;
    function GetCompileSourceFilename: string;// as GetSrcFilename without directory
    function GetSrcFilename: string;
    function GetSrcPPUFilename: string;
    function GetCompilerFilename: string;
    function GetPOOutDirectory: string;
    function GetUnitPath(RelativeToBaseDir: boolean): string;
    function GetIncludePath(RelativeToBaseDir: boolean): string;
    function GetSrcPath(RelativeToBaseDir: boolean): string;
    function GetFPDocPackageName: string;
    function GetLastCompilerParams(o: TPkgOutputDir): string;
    function NeedsDefineTemplates: boolean;
    function SubstitutePkgMacros(const s: string; PlatformIndependent: boolean): string;
    procedure WriteInheritedUnparsedOptions;
    function GetActiveBuildMethod: TBuildMethod;
    // files
    function IndexOfPkgFile(PkgFile: TPkgFile): integer;
    function SearchShortFilename(const ShortFilename: string;
                            SearchFlags: TSearchIDEFileFlags): TPkgFile;
    function SearchFilename(const AFilename: string;
                            SearchFlags: TSearchIDEFileFlags): TPkgFile;
    procedure ShortenFilename(var ExpandedFilename: string; UseUp: boolean);
    procedure LongenFilename(var AFilename: string);
    function FindPkgFile(const AFilename: string;
                         IgnoreRemoved, FindVirtualFile: boolean): TPkgFile;
    function FindUnitWithRegister(IgnorePkgFile: TPkgFile = nil): TPkgFile;
    function FindUnit(const TheUnitName: string): TPkgFile;
    function FindUnit(const TheUnitName: string; IgnoreRemoved: boolean): TPkgFile;
    function FindUnit(const TheUnitName: string; IgnoreRemoved: boolean;
                      IgnorePkgFile: TPkgFile): TPkgFile;
    function FindUsedUnit(TheUnitName: string; IgnorePkgFile: TPkgFile = nil): TPkgFile;
    function FindRemovedPkgFile(const AFilename: string): TPkgFile;
    function AddFile(const NewFilename, NewUnitName: string;
                     NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
                     CompPriorityCat: TComponentPriorityCategory): TPkgFile;
    function AddFileByName(aFilename: string;
                           var NewUnitPaths, NewIncPaths: String): Boolean;
    function AddRemovedFile(const NewFilename, NewUnitName: string;
                     NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
                     CompPriorityCat: TComponentPriorityCategory): TPkgFile;
    procedure DeleteFile(PkgFile: TPkgFile); // free TPkgFile
    procedure RemoveFileSilently(PkgFile: TPkgFile);
    procedure RemoveFile(PkgFile: TPkgFile); // move file to removed file list
    procedure UnremovePkgFile(PkgFile: TPkgFile); // move file back to file list
    // True if something changed. Param is ignored here, just to match with interface.
    function RemoveNonExistingFiles({%H-}RemoveFromUsesSection: boolean = true): boolean;
    function GetFileDialogInitialDir(const DefaultDirectory: string): string;
    procedure MoveFile(CurIndex, NewIndex: integer);
    procedure SortFiles;
    function FixFilesCaseSensitivity: boolean;
    function MainUnitHasPkgName: boolean;
    // required dependencies (plus removed required dependencies)
    function FindDependencyByName(const PackageName: string): TPkgDependency;
    function FindRemovedDependencyByName(const PkgName: string): TPkgDependency;
    function RequiredDepByIndex(Index: integer): TPkgDependency;
    function RemovedDepByIndex(Index: integer): TPkgDependency;
    procedure AddRequiredDependency(Dependency: TPkgDependency);
    procedure AddPackageDependency(const PackageName: string);
    procedure RemoveRequiredDepSilently(Dependency: TPkgDependency);
    procedure RemoveRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRemovedDependency(Dependency: TPkgDependency);
    procedure RemoveRemovedDependency(Dependency: TPkgDependency);
    function MoveRequiredDependencyUp(Dependency: TPkgDependency): Boolean;
    function MoveRequiredDependencyDown(Dependency: TPkgDependency): Boolean;
    function CreateDependencyWithOwner(NewOwner: TObject;
                               WithMinVersion: boolean = false): TPkgDependency;
    function Requires(APackage: TLazPackage): boolean;
    procedure GetAllRequiredPackages(var List, FPMakeList: TFPList; WithSelf: boolean;
      aFlags: TPkgIntfRequiredFlags = [];
      MinPolicy: TPackageUpdatePolicy = low(TPackageUpdatePolicy));
    // components
    function IndexOfPkgComponent(PkgComponent: TPkgComponent): integer;
    function AddComponent(PkgFile: TPkgFile; const Page: string;
                          TheComponentClass: TComponentClass): TPkgComponent;
    procedure AddPkgComponent(APkgComponent: TPkgComponent);
    procedure RemovePkgComponent(APkgComponent: TPkgComponent);
    procedure IterateComponentClasses(Event: TIterateComponentClassesEvent;
                                      WithUsedPackages: boolean);
    procedure SetAllComponentPriorities(const p: TComponentPriority);
    // used by dependencies
    procedure AddUsedByDependency(Dependency: TPkgDependencyBase); override;
    procedure RemoveUsedByDependency(Dependency: TPkgDependencyBase); override;
    function UsedByDepByIndex(Index: integer): TPkgDependency;
    function FindUsedByDepPrefer(Ignore: TPkgDependency): TPkgDependency;
    // provides
    function ProvidesPackage(const AName: string): boolean;
    // ID
    procedure ChangeID(const NewName: string; NewVersion: TPkgVersion);
  public
    LastCompile: array[TPkgOutputDir] of TPkgLastCompileStats;
    function GetOutputDirType: TPkgOutputDir;
  public
    property AddToProjectUsesSection: boolean read FAddToProjectUsesSection
                                              write SetAddToProjectUsesSection;
    property Author: string read FAuthor write SetAuthor;
    property AutoIncrementVersionOnBuild: boolean read GetAutoIncrementVersionOnBuild
                                                 write SetAutoIncrementVersionOnBuild;
    property AutoUpdate: TPackageUpdatePolicy read FAutoUpdate write SetAutoUpdate;
    property CompilerOptions: TPkgCompilerOptions read GetCompilerOptions;
    property ComponentCount: integer read GetComponentCount;
    property Components[Index: integer]: TPkgComponent read GetComponents;
    property DefineTemplates: TLazPackageDefineTemplates read FDefineTemplates
                                                         write FDefineTemplates;
    property Description: string read FDescription write SetDescription;
    property Editor: TBasePackageEditor read FPackageEditor write SetPackageEditor;
    property EnableI18N: Boolean read FEnableI18N write SetEnableI18N;
    property EnableI18NForLFM: boolean read FEnableI18NForLFM write SetEnableI18NForLFM;
    property FileReadOnly: boolean read FFileReadOnly write SetFileReadOnly;
    property Files[Index: integer]: TPkgFile read GetFiles;
    property FirstRemovedDependency: TPkgDependency read FFirstRemovedDependency;
    property FirstRequiredDependency: TPkgDependency read FFirstRequiredDependency;
    property FirstUsedByDependency: TPkgDependency read FFirstUsedByDependency;
    property Flags: TLazPackageFlags read FFlags write SetFlags;
    property HoldPackageCount: integer read FHoldPackageCount;
    property IconFile: string read FIconFile write SetIconFile;
    property IDEOptions: TPackageIDEOptions read GetIDEOptions;
    property Installed: TPackageInstallType read FInstalled write SetInstalled;
    property FPDocPaths: string read FFPDocPaths write SetFPDocPaths;
    property FPDocPackageName: string read FFPDocPackageName write SetFPDocPackageName;
    property License: string read FLicense write SetLicense;
    property LPKSource: TCodeBuffer read FLPKSource write SetLPKSource;// see Missing, can be nil when file on disk was removed or point to a different codebuffer during rename
    property LPKSourceChangeStep: integer read FLPKSourceChangeStep write FLPKSourceChangeStep;
    property Macros: TTransferMacroList read FMacros;
    property MainUnit: TPkgFile read FMainUnit;
    property Missing: boolean read FMissing write FMissing; // lpk is missing, Note: virtual packages can have Missing=false
    property OptionsBackup: TLazPackage read FOptionsBackup;
    property OutputStateFile: string read FOutputStateFile write SetOutputStateFile;
    property PackageType: TLazPackageType read FPackageType write SetPackageType;
    property POOutputDirectory: string read FPOOutputDirectory write SetPOOutputDirectory;
    property Provides: TStrings read FProvides write SetProvides;
    property PublishOptions: TPublishPackageOptions read fPublishOptions write fPublishOptions;
    property Registered: boolean read FRegistered write SetRegistered;
    property RemovedFiles[Index: integer]: TPkgFile read GetRemovedFiles;
    property SourceDirectories: TFileReferenceList read GetSourceDirectories;
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write SetStorePathDelim;
    property TopologicalLevel: integer read FTopologicalLevel write FTopologicalLevel;
    property Translated: string read FTranslated write FTranslated;
    property UsageOptions: TPkgAdditionalCompilerOptions read FUsageOptions;
    property UserReadOnly: boolean read FUserReadOnly write SetUserReadOnly;
    property UserIgnoreChangeStamp: integer read FUserIgnoreChangeStamp
                                            write FUserIgnoreChangeStamp;
    property OnModifySilently: TNotifyEvent read FOnModifySilently write FOnModifySilently;
  end;
  
  PLazPackage = ^TLazPackage;
  
  
  { TBasePackageEditor }
  
  TBasePackageEditor = class(TForm)
  protected
    function GetLazPackage: TLazPackage; virtual;
    procedure SetLazPackage(const AValue: TLazPackage); virtual; abstract;
  public
    procedure UpdateAll(Immediately: boolean = false); virtual; abstract;
    property LazPackage: TLazPackage read GetLazPackage write SetLazPackage;
  end;

const
  LazPkgXMLFileVersion = 4;
  
  AutoUpdateNames: array[TPackageUpdatePolicy] of string = (
    'Manually', 'OnRebuildingAll', 'AsNeeded');
    
var
  // All TPkgDependency are added to this AVL tree (sorted for names, not version!)
  PackageDependencies: TAVLTree = nil; // tree of TPkgDependency

  OnGetAllRequiredPackages: TGetAllRequiredPackagesEvent = nil;
  OnGetDependencyOwnerDescription: TGetDependencyOwnerDescription = nil;
  OnGetDependencyOwnerDirectory: TGetDependencyOwnerDirectory = nil;
  OnPackageFileLoaded: TNotifyEvent = nil;

function CompareLazPackageID(Data1, Data2: Pointer): integer;
function CompareNameWithPackageID(Key, Data: Pointer): integer;
function ComparePkgIDMaskWithPackageID(Key, Data: Pointer): integer;
function CompareLazPackageIDNames(Data1, Data2: Pointer): integer;
function CompareLazPackageTopologicallyAndName(Data1, Data2: Pointer): integer;
function CompareNameWithPkgDependency(Key, Data: Pointer): integer;
function ComparePkgDependencyNames(Data1, Data2: Pointer): integer;
function CompareUnitsTree(UnitTree1, UnitTree2: TPkgUnitsTree): integer;
function ComparePackageWithUnitsTree(Package: TLazPackage;
                                     UnitTree: TPkgUnitsTree): integer;
function ComparePkgFilesAlphabetically(PkgFile1, PkgFile2: TPkgFile): integer;

function GetUsageOptionsList(PackageList: TFPList): TFPList;

function GetPkgFileTypeLocalizedName(FileType: TPkgFileType): string;
function NameToAutoUpdatePolicy(const s: string): TPackageUpdatePolicy;
function FileNameToPkgFileType(AFilename: string): TPkgFileType;

procedure SortDependencyListAlphabetically(Dependencies: TFPList);
procedure LoadPkgDependencyList(XMLConfig: TXMLConfig; const ThePath: string;
  var First: TPkgDependency; ListType: TPkgDependencyList; Owner: TObject;
  HoldPackages, SortList: boolean);
procedure SavePkgDependencyList(XMLConfig: TXMLConfig; const ThePath: string;
  First: TPkgDependency; ListType: TPkgDependencyList;
  UsePathDelim: TPathDelimSwitch);
procedure ListPkgIDToDependencyList(ListOfTLazPackageID: TObjectList;
  var First: TPkgDependency; ListType: TPkgDependencyList; Owner: TObject;
  HoldPackages: boolean);
procedure DeleteDependencyInList(ADependency: TPkgDependency;
  var First: TPkgDependency; ListType: TPkgDependencyList);
procedure FreeDependencyList(var First: TPkgDependency;
  ListType: TPkgDependencyList);
function DependencyListAsString(First: TPkgDependency;
  ListType: TPkgDependencyList): string;

function FindDependencyByNameInList(First: TPkgDependency;
  ListType: TPkgDependencyList; const Name: string): TPkgDependency;
function FindCompatibleDependencyInList(First: TPkgDependency;
  ListType: TPkgDependencyList; ComparePackage: TLazPackageID): TPkgDependency;
function GetDependencyWithIndex(First: TPkgDependency;
  ListType: TPkgDependencyList; Index: integer): TPkgDependency;
function IndexOfDependencyInList(First: TPkgDependency;
  ListType: TPkgDependencyList; FindDependency: TPkgDependency): integer;
function GetFirstDependency(ListItem: TPkgDependency;
  ListType: TPkgDependencyList): TPkgDependency;

function FindLowestPkgDependencyWithName(const PkgName: string): TPkgDependency;
function FindLowestPkgDependencyNodeWithName(const PkgName: string): TAVLTreeNode;
function FindNextPkgDependencyNodeWithSameName(Node: TAVLTreeNode): TAVLTreeNode;

function GetDependencyOwnerAsString(Dependency: TPkgDependency): string;
function GetDependencyOwnerDirectory(Dependency: TPkgDependency): string;

procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig; const Path: string; FileVersion: integer);
procedure PkgVersionSaveToXMLConfig(Version: TPkgVersion; XMLConfig: TXMLConfig;
  const Path: string);
procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig);

var
  Package1: TLazPackage; // don't use it - only for options dialog

function dbgs(p: TPackageUpdatePolicy): string; overload;
function dbgs(p: TLazPackageType): string; overload;
function PackagePathToStr(PathList: TFPList): string;


implementation

function GetPkgFileTypeLocalizedName(FileType: TPkgFileType): string;
begin
  case FileType of
  pftUnit: Result:=lisUnit;
  pftVirtualUnit: Result:=lisPkgFileTypeVirtualUnit;
  pftMainUnit: Result:=lisPkgFileTypeMainUnit;
  pftLFM: Result:=lisPkgFileTypeLFM;
  pftLRS: Result:=lisPkgFileTypeLRS;
  pftInclude: Result:=lisPkgFileTypeInclude;
  pftIssues: Result:=lisPkgFileTypeIssues;
  pftText: Result:=lisPkgFileTypeText;
  pftBinary: Result:=lisPkgFileTypeBinary;
  else
    Result:='Unknown';
  end;
end;

function NameToAutoUpdatePolicy(const s: string): TPackageUpdatePolicy;
begin
  for Result:=Low(TPackageUpdatePolicy) to High(TPackageUpdatePolicy) do
    if SysUtils.CompareText(AutoUpdateNames[Result],s)=0 then exit;
  Result:=pupAsNeeded;
end;

function FileNameToPkgFileType(AFilename: string): TPkgFileType;
var
  Code: TCodeBuffer;
  SrcType: String;
  HasName: Boolean;
begin
  HasName:=ExtractFileNameOnly(AFilename)<>'';
  if HasName then begin
    if CompareFileExt(AFilename,'.lfm',true)=0 then
      exit(pftLFM)
    else if CompareFileExt(AFilename,'.lrs',true)=0 then
      exit(pftLRS)
    else if CompareFileExt(AFilename,'.inc',true)=0 then
      exit(pftInclude)
    else if CompareFileExt(AFilename,'.xml',true)=0 then
      exit(pftIssues)
    else if FilenameIsPascalUnit(AFilename) then begin
      Result:=pftUnit;
      AFilename:=CleanAndExpandFilename(AFilename);
      Code:=CodeToolBoss.LoadFile(aFilename,true,false);
      if Code<>nil then begin
        SrcType:=CodeToolBoss.GetSourceType(Code,false);
        if CompareText(SrcType,'unit')<>0 then
          Result:=pftInclude;
      end;
      exit;
    end;
  end;
  if FileIsText(AFilename) then
    Result:=pftText
  else
    Result:=pftBinary;
end;

procedure LoadPkgDependencyList(XMLConfig: TXMLConfig; const ThePath: string;
  var First: TPkgDependency; ListType: TPkgDependencyList; Owner: TObject;
  HoldPackages, SortList: boolean);
var
  i: Integer;
  PkgDependency: TPkgDependency;
  NewCount: Integer;
  List: TFPList;
  FileVersion: Integer;
  Last: TPkgDependency;
begin
  FileVersion:=XMLConfig.GetValue(ThePath+'Version',0);
  NewCount:=XMLConfig.GetValue(ThePath+'Count',0);
  List:=TFPList.Create;
  for i:=0 to NewCount-1 do begin
    PkgDependency:=TPkgDependency.Create;
    PkgDependency.LoadFromXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i+1)+'/',
                                    FileVersion);
    PkgDependency.HoldPackage:=HoldPackages;
    // IsMakingSense checks if the package-name is a valid identifier. This is
    // not applicable to FPMake-packages.
    if (PkgDependency.DependencyType=pdtFPMake) or PkgDependency.IsMakingSense then
      List.Add(PkgDependency)
    else
      PkgDependency.Free;
  end;
  if SortList then
    SortDependencyListAlphabetically(List);
  Last:=First;
  if Last<>nil then
    while Last.NextDependency[ListType]<>nil do
      Last:=Last.NextDependency[ListType];
  for i:=0 to List.Count-1 do begin
    PkgDependency:=TPkgDependency(List[i]);
    PkgDependency.AddToEndOfList(Last,ListType);
    if First=nil then
      First:=Last;
    PkgDependency.Owner:=Owner;
  end;
  List.Free;
end;

procedure SavePkgDependencyList(XMLConfig: TXMLConfig; const ThePath: string;
  First: TPkgDependency; ListType: TPkgDependencyList;
  UsePathDelim: TPathDelimSwitch);
var
  i: Integer;
  Dependency: TPkgDependency;
begin
  i:=0;
  Dependency:=First;
  while Dependency<>nil do begin
    inc(i);
    Dependency.SaveToXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i)+'/',UsePathDelim);
    Dependency:=Dependency.NextDependency[ListType];
  end;
  XMLConfig.SetDeleteValue(ThePath+'Count',i,0);
end;

procedure ListPkgIDToDependencyList(ListOfTLazPackageID: TObjectList;
  var First: TPkgDependency; ListType: TPkgDependencyList; Owner: TObject;
  HoldPackages: boolean);
var
  NewDependency: TPkgDependency;
  i: Integer;
  PkgID: TLazPackageID;
begin
  First:=nil;
  for i:=ListOfTLazPackageID.Count-1 downto 0 do begin
    PkgID:=TLazPackageID(ListOfTLazPackageID[i]);
    NewDependency:=TPkgDependency.Create;
    NewDependency.Assign(PkgID);
    NewDependency.Owner:=Owner;
    NewDependency.HoldPackage:=HoldPackages;
    NewDependency.AddToList(First,ListType);
  end;
end;

procedure DeleteDependencyInList(ADependency: TPkgDependency;
  var First: TPkgDependency; ListType: TPkgDependencyList);
var
  NextDependency, PrevDependency: TPkgDependency;
begin
  NextDependency := ADependency.NextDependency[ListType];
  PrevDependency := ADependency.PrevDependency[ListType];
  if First = ADependency then First := NextDependency;
  if Assigned(NextDependency) then
    NextDependency.PrevDependency[ListType] := PrevDependency;
  if Assigned(PrevDependency) then
    PrevDependency.NextDependency[ListType] := NextDependency;
  ADependency.Free;
end;

procedure FreeDependencyList(var First: TPkgDependency;
  ListType: TPkgDependencyList);
var
  NextDependency: TPkgDependency;
begin
  while First<>nil do begin
    NextDependency:=First.NextDependency[ListType];
    First.Free;
    First:=NextDependency;
  end;
end;

function DependencyListAsString(First: TPkgDependency;
  ListType: TPkgDependencyList): string;
begin
  Result:='';
  while First<>nil do begin
    Result:=Result+First.AsString+LineEnding;
    First:=First.NextDependency[ListType];
  end;
end;

procedure SortDependencyListAlphabetically(Dependencies: TFPList);
var
  Count: Integer;
  i, j: Integer;
  Dependency1: TPkgDependency;
  Dependency2: TPkgDependency;
  Sorted: Boolean;
begin
  if (Dependencies=nil) or (Dependencies.Count<2) then exit;
  // check if already sorted
  Count:=Dependencies.Count;
  Sorted:=true;
  for i:=0 to Count-2 do begin
    Dependency1:=TPkgDependency(Dependencies[i]);
    Dependency2:=TPkgDependency(Dependencies[i+1]);
    if Dependency1.Compare(Dependency2)>0 then begin
      Sorted:=false;
      break;
    end;
  end;
  if Sorted then exit;
  // bubble sort (slow, but dependency lists are normally sorted)
  for i:=0 to Count-2 do begin
    Dependency1:=TPkgDependency(Dependencies[i]);
    for j:=i+1 to Count-1 do begin
      Dependency2:=TPkgDependency(Dependencies[j]);
      if Dependency1.Compare(Dependency2)>0 then begin
        Dependencies.Exchange(i,j);
        Dependency1:=TPkgDependency(Dependencies[i]);
      end;
    end;
  end;
end;

function StrToComponentBaseClass(const s: string): TPFComponentBaseClass;
begin
  for Result:=low(TPFComponentBaseClass) to high(TPFComponentBaseClass) do
    if SysUtils.CompareText(PFComponentBaseClassNames[Result],s)=0 then exit;
  Result:=pfcbcNone;
end;

function GetComponentBaseClass(aClass: TClass): TPFComponentBaseClass;
begin
  Result:=pfcbcNone;
  if aClass=nil then exit;
  if aClass.InheritsFrom(TForm) then
    Result:=pfcbcForm
  else if aClass.InheritsFrom(TFrame) then
    Result:=pfcbcFrame
  else if aClass.InheritsFrom(TDataModule) then
    Result:=pfcbcDataModule;
end;

function CompareLazPackageID(Data1, Data2: Pointer): integer;
var
  Pkg1: TLazPackageID absolute Data1;
  Pkg2: TLazPackageID absolute Data2;
begin
  Result:=Pkg1.Compare(Pkg2);
end;

function CompareNameWithPackageID(Key, Data: Pointer): integer;
var
  Name: String;
  Pkg: TLazPackageID;
begin
  if Key<>nil then begin
    Name:=AnsiString(Key);
    Pkg:=TLazPackageID(Data);
    Result:=SysUtils.CompareText(Name,Pkg.Name);
  end else
    Result:=-1;
end;

function ComparePkgIDMaskWithPackageID(Key, Data: Pointer): integer;
var
  Pkg1: TLazPackageID absolute Key;
  Pkg2: TLazPackageID absolute Data;
begin
  Result:=Pkg1.CompareMask(Pkg2);
end;

function CompareLazPackageIDNames(Data1, Data2: Pointer): integer;
var
  Pkg1: TLazPackageID absolute Data1;
  Pkg2: TLazPackageID absolute Data2;
begin
  Result:=SysUtils.CompareText(Pkg1.Name,Pkg2.Name);
end;

function CompareLazPackageTopologicallyAndName(Data1, Data2: Pointer): integer;
var
  Pkg1: TLazPackage absolute Data1;
  Pkg2: TLazPackage absolute Data2;
begin
  Result:=Pkg1.TopologicalLevel-Pkg2.TopologicalLevel;
  if Result<>0 then exit;
  Result:=SysUtils.CompareText(Pkg1.Name,Pkg2.Name);
end;

function CompareNameWithPkgDependency(Key, Data: Pointer): integer;
var
  PkgName: String;
  Dependency: TPkgDependency absolute Data;
begin
  PkgName:=String(Key);
  Result:=SysUtils.CompareText(PkgName,Dependency.PackageName);
end;

function ComparePkgDependencyNames(Data1, Data2: Pointer): integer;
var
  Dependency1: TPkgDependency absolute Data1;
  Dependency2: TPkgDependency absolute Data2;
begin
  Result:=SysUtils.CompareText(Dependency1.PackageName,Dependency2.PackageName);
end;

function CompareUnitsTree(UnitTree1, UnitTree2: TPkgUnitsTree): integer;
begin
  Result:=UnitTree1.LazPackage.Compare(UnitTree2.LazPackage);
end;

function ComparePackageWithUnitsTree(Package: TLazPackage;
                                     UnitTree: TPkgUnitsTree): integer;
begin
  Result:=Package.Compare(UnitTree.LazPackage);
end;

function ComparePkgFilesAlphabetically(PkgFile1, PkgFile2: TPkgFile): integer;
var
  ShortFilename1: String;
  ShortFilename2: String;
  File1IsInMainDir: Boolean;
  File2IsInMainDir: Boolean;
begin
  ShortFilename1:=PkgFile1.GetShortFilename(true);
  ShortFilename2:=PkgFile2.GetShortFilename(true);
  // files in the main directory are higher
  File1IsInMainDir:=ExtractFilePath(ShortFilename1)='';
  File2IsInMainDir:=ExtractFilePath(ShortFilename2)='';
  if File1IsInMainDir xor File2IsInMainDir then begin
    if File1IsInMainDir then
      Result:=-1
    else
      Result:=1;
    exit;
  end;
  // compare short filenames without extension
  Result:=CompareFilenames(ChangeFileExt(ShortFilename1,''),
                           ChangeFileExt(ShortFilename2,''));
  if Result<>0 then exit;
  // if one is a unit, then it is higher
  if (PkgFile1.Unit_Name<>'') and (PkgFile2.Unit_Name='') then begin
    Result:=-1;
    exit;
  end else if (PkgFile1.Unit_Name='') and (PkgFile2.Unit_Name<>'') then begin
    Result:=1;
    exit;
  end;
  // compare short filenames with extension
  Result:=CompareFilenames(ShortFilename1,ShortFilename2);
  if Result<>0 then exit;
  // compare filenames
  Result:=CompareFilenames(PkgFile1.FileName,PkgFile2.FileName);
end;

function GetUsageOptionsList(PackageList: TFPList): TFPList;
// returns a list of TPkgAdditionalCompilerOptions
// from the list of TLazPackage
var
  Cnt: Integer;
  i: Integer;
begin
  if PackageList<>nil then begin
    Result:=TFPList.Create;
    Cnt:=PackageList.Count;
    for i:=0 to Cnt-1 do begin
      Result.Add(TLazPackage(PackageList[i]).UsageOptions);
    end;
  end else begin
    Result:=nil;
  end;
end;

function FindDependencyByNameInList(First: TPkgDependency;
  ListType: TPkgDependencyList; const Name: string): TPkgDependency;
begin
  Result:=First;
  while Result<>nil do begin
    if SysUtils.CompareText(Result.PackageName,Name)=0 then exit;
    Result:=Result.NextDependency[ListType];
  end;
end;

function FindCompatibleDependencyInList(First: TPkgDependency;
  ListType: TPkgDependencyList; ComparePackage: TLazPackageID): TPkgDependency;
begin
  Result:=First;
  while Result<>nil do begin
    if Result.IsCompatible(ComparePackage) then exit;
    Result:=Result.NextDependency[ListType];
  end;
end;

function GetDependencyWithIndex(First: TPkgDependency;
  ListType: TPkgDependencyList; Index: integer): TPkgDependency;
begin
  if Index<0 then RaiseGDBException('GetDependencyWithIndex');
  Result:=First;
  while (Result<>nil) and (Index>0) do begin
    Result:=Result.NextDependency[ListType];
    dec(Index);
  end;
end;

function FindLowestPkgDependencyNodeWithName(const PkgName: string): TAVLTreeNode;
begin
  Result:=nil;
  if PackageDependencies=nil then exit;
  Result:=PackageDependencies.FindLeftMostKey(PChar(PkgName),
                                              @CompareNameWithPkgDependency);
end;

function FindNextPkgDependencyNodeWithSameName(
  Node: TAVLTreeNode): TAVLTreeNode;
begin
  Result:=nil;
  if (Node=nil) or (PackageDependencies=nil) then exit;
  Result:=PackageDependencies.FindSuccessor(Node);
  if (Result<>nil)
  and (SysUtils.CompareText(TPkgDependency(Node.Data).PackageName,
                   TPkgDependency(Result.Data).PackageName)<>0)
  then
    Result:=nil;
end;

function GetDependencyOwnerAsString(Dependency: TPkgDependency): string;
begin
  Result := '';
  OnGetDependencyOwnerDescription(Dependency,Result);
end;

function GetDependencyOwnerDirectory(Dependency: TPkgDependency): string;
begin
  Result := '';
  OnGetDependencyOwnerDirectory(Dependency,Result);
end;

procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig; const Path: string; FileVersion: integer);
var
  NewMajor: Integer;
  NewMinor: Integer;
  NewRelease: Integer;
  NewBuild: Integer;
begin
  if FileVersion=1 then ;
  NewMajor:=Version.VersionBound(XMLConfig.GetValue(Path+'Major',0));
  NewMinor:=Version.VersionBound(XMLConfig.GetValue(Path+'Minor',0));
  NewRelease:=Version.VersionBound(XMLConfig.GetValue(Path+'Release',0));
  NewBuild:=Version.VersionBound(XMLConfig.GetValue(Path+'Build',0));
  Version.SetValues(NewMajor,NewMinor,NewRelease,NewBuild,pvtBuild);
end;

procedure PkgVersionSaveToXMLConfig(Version: TPkgVersion; XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Major',Version.Major,0);
  XMLConfig.SetDeleteValue(Path+'Minor',Version.Minor,0);
  XMLConfig.SetDeleteValue(Path+'Release',Version.Release,0);
  XMLConfig.SetDeleteValue(Path+'Build',Version.Build,0);
end;

procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig);
var
  Path: String;
  FileVersion: LongInt;
begin
  Path:='Package/';
  FileVersion:=XMLConfig.GetValue(Path+'Version',0);
  PkgVersionLoadFromXMLConfig(Version,XMLConfig,Path+'Version/',FileVersion);
end;

function dbgs(p: TPackageUpdatePolicy): string;
begin
  Result:=GetEnumName(TypeInfo(p),ord(p));
end;

function dbgs(p: TLazPackageType): string;
begin
  Result:=LazPackageTypeIdents[p];
end;

function PackagePathToStr(PathList: TFPList): string;
var
  i: Integer;
  Item: TObject;
  Dep: TPkgDependency;
begin
  Result:='';
  if PathList=nil then exit;
  for i:=0 to PathList.Count-1 do begin
    if i>0 then
      Result:=Result+' -> ';
    Item:=TObject(PathList[i]);
    if Item is TPkgDependency then begin
      Dep:=TPkgDependency(Item);
      Result:=Result+GetDependencyOwnerAsString(Dep);
      if i=PathList.Count-1 then
        Result:=Result+' -> '+Dep.AsString;
    end else if Item is TLazPackage then
      Result:=Result+TLazPackage(Item).Name
    else
      Result:=Result+DbgSName(Item);
  end;
end;

function IndexOfDependencyInList(First: TPkgDependency;
  ListType: TPkgDependencyList; FindDependency: TPkgDependency): integer;
var
  Dependency: TPkgDependency;
begin
  Result:=-1;
  Dependency:=First;
  while Dependency<>nil do begin
    inc(Result);
    if Dependency=FindDependency then exit;
    Dependency:=Dependency.NextDependency[ListType];
  end;
  Result:=-1;
end;

function GetFirstDependency(ListItem: TPkgDependency;
  ListType: TPkgDependencyList): TPkgDependency;
begin
  Result:=ListItem;
  if Result=nil then exit;
  while Result.PrevDependency[ListType]<>nil do
    Result:=Result.PrevDependency[ListType];
end;

function FindLowestPkgDependencyWithName(const PkgName: string): TPkgDependency;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FindLowestPkgDependencyNodeWithName(PkgName);
  if ANode<>nil then
    Result:=TPkgDependency(ANode.Data)
  else
    Result:=nil;
end;

{ TPkgFile }

procedure TPkgFile.SetFilename(const AValue: string);
var
  NewFilename: String;
  OldDirectory: String;
begin
  NewFilename:=AValue;
  ForcePathDelims(NewFilename);
  if Filename=NewFilename then exit;
  fFilename:=NewFilename;
  fFullFilenameStamp:=CTInvalidChangeStamp;
  OldDirectory:=FDirectory;
  FDirectory:=ExtractFilePath(Filename);
  if OldDirectory<>FDirectory then begin
    if FSourceDirNeedReference then begin
      LazPackage.SourceDirectories.RemoveFilename(OldDirectory);
      LazPackage.SourceDirectories.AddFilename(FDirectory);
    end;
  end;
  UpdateUnitName;
end;

function TPkgFile.GetHasRegisterProc: boolean;
begin
  Result:=pffHasRegisterProc in FFlags;
end;

procedure TPkgFile.SetAddToUsesPkgSection(const AValue: boolean);
begin
  if AddToUsesPkgSection=AValue then exit;
  if AValue then
    Include(FFlags,pffAddToPkgUsesSection)
  else
    Exclude(FFlags,pffAddToPkgUsesSection);
end;

procedure TPkgFile.SetAutoReferenceSourceDir(const AValue: boolean);
begin
  if FAutoReferenceSourceDir=AValue then exit;
  FAutoReferenceSourceDir:=AValue;
  if FSourceDirNeedReference then
    UpdateSourceDirectoryReference;
end;

procedure TPkgFile.SetRemoved(const AValue: boolean);
begin
  if Removed=AValue then exit;
  inherited SetRemoved(AValue);
  FSourceDirNeedReference:=(FileType in PkgFileRealUnitTypes) and not Removed;
  UpdateSourceDirectoryReference;
end;

procedure TPkgFile.SetDisableI18NForLFM(AValue: boolean);
begin
  if DisableI18NForLFM=AValue then exit;
  inherited SetDisableI18NForLFM(AValue);
  LazPackage.Modified:=true;
end;

function TPkgFile.GetComponents(Index: integer): TPkgComponent;
begin
  Result:=TPkgComponent(FComponents[Index]);
end;

function TPkgFile.GetAddToUsesPkgSection: boolean;
begin
  Result:=pffAddToPkgUsesSection in FFlags;
end;

procedure TPkgFile.SetFileType(const AValue: TPkgFileType);
begin
  if FileType=AValue then exit;
  if (LazPackage<>nil) and (LazPackage.MainUnit=Self) then
    LazPackage.FMainUnit:=nil;
  inherited SetFileType(AValue);
  FSourceDirNeedReference:=(FileType in PkgFileRealUnitTypes) and not Removed;
  UpdateSourceDirectoryReference;
  if (FileType=pftMainUnit) and (LazPackage<>nil)
  and (LazPackage.MainUnit<>Self) then begin
    if LazPackage.MainUnit<>nil then
      LazPackage.MainUnit.FileType:=pftUnit;
    LazPackage.FMainUnit:=Self;
  end;
end;

procedure TPkgFile.SetFlags(const AValue: TPkgFileFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

procedure TPkgFile.SetHasRegisterProc(const AValue: boolean);
begin
  if HasRegisterProc=AValue then exit;
  if AValue then
    Include(FFlags,pffHasRegisterProc)
  else
    Exclude(FFlags,pffHasRegisterProc);
end;

procedure TPkgFile.SetUnitName(const AValue: string);
begin
  if FUnitName=AValue then Exit;
  FUnitName:=AValue;
end;

procedure TPkgFile.UpdateUnitName;
var
  NewUnitName: String;
begin
  if FilenameIsPascalUnit(Filename) then begin
    NewUnitName:=ExtractFileNameOnly(Filename);
    if SysUtils.CompareText(NewUnitName,FUnitName)<>0 then
      FUnitName:=NewUnitName;
  end else
    FUnitName:='';
end;

function TPkgFile.GetComponentList: TFPList;
begin
  if FComponents=nil then FComponents:=TFPList.Create;
  Result:=FComponents;
end;

function TPkgFile.GetInUses: boolean;
begin
  Result:=pffAddToPkgUsesSection in FFlags;
end;

procedure TPkgFile.SetInUses(AValue: boolean);
begin
  if InUses=AValue then exit;
  if AValue then
    Include(FFlags,pffAddToPkgUsesSection)
  else
    Exclude(FFlags,pffAddToPkgUsesSection);
end;

function TPkgFile.GetIDEPackage: TIDEPackage;
begin
  Result:=FPackage;
end;

function TPkgFile.GetFilename: string;
begin
  Result:=fFilename;
end;

function TPkgFile.HasRegisteredPlugins: boolean;
begin
  Result:=ComponentCount>0;
end;

function TPkgFile.MakeSense: boolean;
begin
  Result:=Filename<>'';
end;

procedure TPkgFile.UpdateSourceDirectoryReference;
begin
  if (not AutoReferenceSourceDir) or (FPackage=nil) then exit;
  if FSourceDirNeedReference then begin
    if not SourceDirectoryReferenced then begin
      LazPackage.SourceDirectories.AddFilename(FDirectory);
      FSourceDirectoryReferenced:=true;
    end;
  end else begin
    if SourceDirectoryReferenced then begin
      LazPackage.SourceDirectories.RemoveFilename(FDirectory);
      FSourceDirectoryReferenced:=false;
    end;
  end;
end;

function TPkgFile.GetFullFilename: string;
begin
  if fFullFilenameStamp<>CompilerParseStamp then begin
    fFullFilename:=Filename;
    fFullFilenameStamp:=CompilerParseStamp;
    if LazPackage<>nil then begin
      LazPackage.SubstitutePkgMacros(fFullFilename,false);
      fFullFilename:=TrimFilename(fFullFilename);
      LazPackage.LongenFilename(fFullFilename);
    end
    else begin
      IDEMacros.SubstituteMacros(fFullFilename);
      fFullFilename:=TrimAndExpandFilename(fFullFilename);
    end;
  end;
  Result:=fFullFilename;
end;

constructor TPkgFile.Create(ThePackage: TLazPackage);
begin
  inherited Create;
  Clear;
  FPackage:=ThePackage;
  FComponentPriority:=ComponentPriorityNormal;
end;

destructor TPkgFile.Destroy;
begin
  FreeThenNil(FComponents);
  if (LazPackage<>nil) then begin
    if (LazPackage.MainUnit=Self) then
      LazPackage.FMainUnit:=nil;
    if (not (lpfDestroying in LazPackage.Flags)) then begin
      if Removed then
        LazPackage.FRemovedFiles.Remove(Self)
      else
        LazPackage.FFiles.Remove(Self);
    end;
  end;
  inherited Destroy;
end;

procedure TPkgFile.Clear;
begin
  AutoReferenceSourceDir:=false;
  if (LazPackage=nil) or (not (lpfDestroying in LazPackage.Flags)) then begin
    inherited SetRemoved(false);
    fFilename:='';
    FDirectory:='';
    FFlags:=[];
    inherited SetFileType(pftUnit);
    FSourceDirectoryReferenced:=false;
    FSourceDirNeedReference:=true;
  end;
  FreeThenNil(FComponents);
  if (LazPackage<>nil) and (LazPackage.MainUnit=Self) then
    LazPackage.FMainUnit:=nil;
end;

procedure TPkgFile.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  FileVersion: integer; AdjustPathDelims: boolean);
var
  AFilename: String;
  CaseInsensitiveUnitName: String;
  Config: TXMLOptionsStorage;
begin
  if FileVersion=1 then ;
  Clear;
  AFilename:=SwitchPathDelims(XMLConfig.GetValue(Path+'Filename/Value',''),
                              AdjustPathDelims);
  FPackage.LongenFilename(AFilename);
  Filename:=AFilename;
  FileType:=PkgFileTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',''));
  HasRegisterProc:=XMLConfig.GetValue(Path+'HasRegisterProc/Value',false);
  AddToUsesPkgSection:=XMLConfig.GetValue(Path+'AddToUsesPkgSection/Value',
                                          FileType in PkgFileUnitTypes);
  DisableI18NForLFM:=XMLConfig.GetValue(Path+'DisableI18NForLFM/Value',false);
  fUnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');
  if FileType in PkgFileUnitTypes then begin
    // make sure the unitname makes sense
    CaseInsensitiveUnitName:=ExtractFileNameOnly(Filename);
    if SysUtils.CompareText(fUnitName,CaseInsensitiveUnitName)<>0 then
      fUnitName:=CaseInsensitiveUnitName;
  end;
  FResourceBaseClass:=StrToComponentBaseClass(
                         XMLConfig.GetValue(Path+'ResourceBaseClass/Value',''));

  Config:=TXMLOptionsStorage.Create(XMLConfig);
  try
    TConfigMemStorage(CustomOptions).LoadFromConfig(Config,Path+'CustomOptions/');
  finally
    Config.Free;
  end;
end;

procedure TPkgFile.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch);
var
  TmpFilename: String;
  Config: TXMLOptionsStorage;
begin
  TmpFilename:=Filename;
  FPackage.ShortenFilename(TmpFilename,true);
  XMLConfig.SetDeleteValue(Path+'Filename/Value',
                           SwitchPathDelims(TmpFilename,UsePathDelim),'');
  XMLConfig.SetDeleteValue(Path+'Type/Value',PkgFileTypeIdents[FileType],
                           PkgFileTypeIdents[pftUnit]);
  XMLConfig.SetDeleteValue(Path+'HasRegisterProc/Value',HasRegisterProc,
                           false);
  XMLConfig.SetDeleteValue(Path+'AddToUsesPkgSection/Value',AddToUsesPkgSection,
                           FileType in PkgFileUnitTypes);
  XMLConfig.SetDeleteValue(Path+'DisableI18NForLFM/Value',DisableI18NForLFM,false);
  XMLConfig.SetDeleteValue(Path+'UnitName/Value',FUnitName,'');
  XMLConfig.SetDeleteValue(Path+'ResourceBaseClass/Value',
                           PFComponentBaseClassNames[FResourceBaseClass],
                           PFComponentBaseClassNames[pfcbcNone]);

  Config:=TXMLOptionsStorage.Create(XMLConfig);
  try
    TConfigMemStorage(CustomOptions).SaveToConfig(Config,Path+'CustomOptions/');
  finally
    Config.Free;
  end;
end;

procedure TPkgFile.ConsistencyCheck;
begin
  if FPackage=nil then
    RaiseGDBException('TPkgFile.ConsistencyCheck FPackage=nil');
  if Filename='' then
    RaiseGDBException('TPkgFile.ConsistencyCheck FFilename=""');
end;

function TPkgFile.GetShortFilename(UseUp: boolean): string;
begin
  Result:=GetFullFilename;
  LazPackage.ShortenFilename(Result,UseUp);
end;

function TPkgFile.ComponentCount: integer;
begin
  if FComponents<>nil then
    Result:=FComponents.Count
  else
    Result:=0;
end;

procedure TPkgFile.AddPkgComponent(APkgComponent: TPkgComponent);
begin
  if FComponents=nil then FComponents:=TFPList.Create;
  FComponents.Add(APkgComponent);
  if LazPackage<>nil then
    LazPackage.AddPkgComponent(APkgComponent);
end;

procedure TPkgFile.RemovePkgComponent(APkgComponent: TPkgComponent);
begin
  if FComponents<>nil then
    FComponents.Remove(APkgComponent);
  if LazPackage<>nil then
    LazPackage.RemovePkgComponent(APkgComponent);
end;

function TPkgFile.GetResolvedFilename: string;
begin
  Result:=GetPhysicalFilenameCached(GetFullFilename,false);
end;

function TPkgFile.GetFileOwner: TObject;
begin
  Result:=LazPackage;
end;

function TPkgFile.GetFileOwnerName: string;
begin
  if LazPackage<>nil then
    Result:=LazPackage.Name
  else
    Result:='';
end;

{ TPkgDependency }

procedure TPkgDependency.SetHoldPackage(const AValue: boolean);
begin
  if FHoldPackage=AValue then exit;
  FHoldPackage:=AValue;
  if RequiredPackage<>nil then begin
    if FHoldPackage then
      inc(RequiredPackage.FHoldPackageCount)
    else
      dec(RequiredPackage.FHoldPackageCount);
  end;
end;

function TPkgDependency.GetRequiredPackage: TLazPackage;
begin
  Result := TLazPackage(FRequiredPackage);
end;

procedure TPkgDependency.SetRequiredPackage(AValue: TLazPackage);
begin
  Assert((FDependencyType=pdtLazarus) or not assigned(AValue), 'Not possible to set a reference to a LazPackage into an FPMake-dependency');
  RequiredIDEPackage := AValue;
end;

procedure TPkgDependency.SetPackageName(const AValue: string);
begin
  if FPackageName=AValue then exit;
  if (PackageDependencies<>nil) and (FPackageName<>'') then
    AVLRemovePointer(PackageDependencies,Self);
  FPackageName:=AValue;
  if (PackageDependencies<>nil) and (FPackageName<>'') then
    PackageDependencies.Add(Self);
  FDefaultFilename:='';
end;

constructor TPkgDependency.Create;
begin
  inherited Create;
end;

destructor TPkgDependency.Destroy;
begin
  inherited Destroy;
end;

procedure TPkgDependency.Clear;
begin
  inherited Clear;
  FDefaultFilename:='';
  FPreferDefaultFilename:=false;
end;

procedure TPkgDependency.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; FileVersion: integer);
  
  function LoadFilename(const SubPath: string): string;
  var
    BaseDir: String;
  begin
    Result:=GetForcedPathDelims(XMLConfig.GetValue(Path+SubPath,''));
    if (Result<>'') and (Owner<>nil)
    and (not FilenameIsAbsolute(Result)) then begin
      BaseDir:=GetDependencyOwnerDirectory(Self);
      if BaseDir<>'' then
        Result:=TrimFilename(AppendPathDelim(BaseDir)+Result);
    end;
  end;
  
begin
  if FileVersion=1 then ;
  Clear;
  PackageName:=XMLConfig.GetValue(Path+'PackageName/Value','');
  if SameText(XMLConfig.GetValue(Path+'DependencyType/Value',''), PkgDependencyTypeNames[pdtFPMake]) then
    DependencyType:=pdtFPMake
  else
    DependencyType:=pdtLazarus;
  PkgVersionLoadFromXMLConfig(MaxVersion,XMLConfig,Path+'MaxVersion/',FileVersion);
  PkgVersionLoadFromXMLConfig(MinVersion,XMLConfig,Path+'MinVersion/',FileVersion);
  if XMLConfig.GetValue(Path+'MaxVersion/Valid',false) then
    Include(FFlags,pdfMaxVersion);
  if XMLConfig.GetValue(Path+'MinVersion/Valid',false) then
    Include(FFlags,pdfMinVersion);
  FDefaultFilename:=LoadFilename('DefaultFilename/Value');
  PreferDefaultFilename:=XMLConfig.GetValue(Path+'DefaultFilename/Prefer',false);
end;

procedure TPkgDependency.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
  
  procedure SaveFilename(const aPath: string; AFilename: string);
  var
    BaseDir: String;
  begin
    if (AFilename<>'')
    and (Owner<>nil) then begin
      BaseDir:=GetDependencyOwnerDirectory(Self);
      if BaseDir<>'' then
        AFilename:=CreateRelativePath(AFilename,BaseDir);
    end;
    XMLConfig.SetDeleteValue(Path+aPath,SwitchPathDelims(AFilename,UsePathDelim),'');
  end;
  
begin
  XMLConfig.SetDeleteValue(Path+'PackageName/Value',PackageName,'');
  PkgVersionSaveToXMLConfig(MaxVersion,XMLConfig,Path+'MaxVersion/');
  PkgVersionSaveToXMLConfig(MinVersion,XMLConfig,Path+'MinVersion/');
  XMLConfig.SetDeleteValue(Path+'MaxVersion/Valid',pdfMaxVersion in FFlags,false);
  XMLConfig.SetDeleteValue(Path+'MinVersion/Valid',pdfMinVersion in FFlags,false);
  SaveFilename('DefaultFilename/Value',FDefaultFilename);
  XMLConfig.SetDeleteValue(Path+'DefaultFilename/Prefer',PreferDefaultFilename,false);
  XMLConfig.SetDeleteValue(Path+'DependencyType/Value',PkgDependencyTypeNames[DependencyType],PkgDependencyTypeNames[pdtLazarus]);
end;

function TPkgDependency.Compare(Dependency2: TPkgDependency): integer;
begin
  Result:=SysUtils.CompareText(PackageName,Dependency2.PackageName);
  if Result<>0 then exit;
  Result:=MinVersion.Compare(Dependency2.MinVersion);
  if Result<>0 then exit;
  Result:=CompareBoolean(pdfMinVersion in Flags,
                         pdfMinVersion in Dependency2.Flags);
  if Result<>0 then exit;
  Result:=MaxVersion.Compare(Dependency2.MaxVersion);
  if Result<>0 then exit;
  Result:=CompareBoolean(pdfMaxVersion in Flags,
                         pdfMaxVersion in Dependency2.Flags);
end;

procedure TPkgDependency.Assign(Source: TPkgDependency);
begin
  PackageName:=Source.PackageName;
  Flags:=Source.Flags;
  MinVersion.Assign(Source.MinVersion);
  MaxVersion.Assign(Source.MaxVersion);
end;

procedure TPkgDependency.Assign(Source: TLazPackageID);
begin
  PackageName:=Source.Name;
  Flags:=[pdfMinVersion];
  MinVersion.Assign(Source.Version);
end;

procedure TPkgDependency.ConsistencyCheck;
begin

end;

function TPkgDependency.IsCompatible(Pkg: TLazPackageID): boolean;
begin
  Result:=IsCompatible(Pkg.Name,Pkg.Version);
end;

procedure TPkgDependency.MakeCompatible(const PkgName: string;
  const Version: TPkgVersion);
begin
  PackageName:=PkgName;
  if MinVersion.Compare(Version)>0 then MinVersion.Assign(Version);
  if MaxVersion.Compare(Version)<0 then MaxVersion.Assign(Version);
end;

function TPkgDependency.AsString(WithOwner: boolean; WithDefaults: boolean): string;
begin
  if Self=nil then
    exit('(nil)');
  Result:=FPackageName;
  if pdfMinVersion in FFlags then
    Result:=Result+' (>='+MinVersion.AsString+')';
  if pdfMaxVersion in FFlags then
    Result:=Result+' (<='+MaxVersion.AsString+')';
  if WithOwner and (Owner<>nil) then
    Result:=GetDependencyOwnerAsString(Self)+' uses '+Result;
  if WithDefaults then
  begin
    if DefaultFilename<>'' then begin
      Result+=', ';
      if PreferDefaultFilename then
        Result+='preferred'
      else
        Result+='default';
      Result+='="'+DefaultFilename+'"';
    end;
  end;
end;

function TPkgDependency.NextUsedByDependency: TPkgDependency;
begin
  Result:=NextDependency[pdlUsedBy];
end;

function TPkgDependency.PrevUsedByDependency: TPkgDependency;
begin
  Result:=PrevDependency[pdlUsedBy];
end;

function TPkgDependency.NextRequiresDependency: TPkgDependency;
begin
  Result:=NextDependency[pdlRequires];
end;

function TPkgDependency.PrevRequiresDependency: TPkgDependency;
begin
  Result:=PrevDependency[pdlRequires];
end;

procedure TPkgDependency.AddUsedByDep(var FirstDependency: TPkgDependencyBase);
begin
  AddToList(TPkgDependency(FirstDependency), pdlUsedBy);
end;

procedure TPkgDependency.RemoveUsedByDep(var FirstDependency: TPkgDependencyBase);
begin
  RemoveFromList(TPkgDependency(FirstDependency), pdlUsedBy);
end;

procedure TPkgDependency.AddRequiresDep(var FirstDependency: TPkgDependencyBase);
begin
  AddToList(TPkgDependency(FirstDependency), pdlRequires);
end;

procedure TPkgDependency.RemoveRequiresDep(var FirstDependency: TPkgDependencyBase);
begin
  RemoveFromList(TPkgDependency(FirstDependency), pdlRequires);
end;

procedure TPkgDependency.AddToList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList);
begin
  NextDependency[ListType]:=FirstDependency;
  FirstDependency:=Self;
  PrevDependency[ListType]:=nil;
  if NextDependency[ListType]<>nil then
    NextDependency[ListType].PrevDependency[ListType]:=Self;
end;

procedure TPkgDependency.AddToEndOfList(var LastDependency: TPkgDependency;
  ListType: TPkgDependencyList);
begin
  PrevDependency[ListType]:=LastDependency;
  LastDependency:=Self;
  NextDependency[ListType]:=nil;
  if PrevDependency[ListType]<>nil then
    PrevDependency[ListType].NextDependency[ListType]:=Self;
end;

procedure TPkgDependency.RemoveFromList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList);
begin
  if FirstDependency=Self then FirstDependency:=NextDependency[ListType];
  if NextDependency[ListType]<>nil then
    NextDependency[ListType].PrevDependency[ListType]:=PrevDependency[ListType];
  if PrevDependency[ListType]<>nil then
    PrevDependency[ListType].NextDependency[ListType]:=NextDependency[ListType];
  NextDependency[ListType]:=nil;
  PrevDependency[ListType]:=nil;
end;

function TPkgDependency.MoveUpInList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList): Boolean;
var
  OldPrev: TPkgDependency;
begin
  if (FirstDependency=Self) or (PrevDependency[ListType]=nil) then exit(False);
  OldPrev:=PrevDependency[ListType];
  if OldPrev.PrevDependency[ListType]<>nil then
    OldPrev.PrevDependency[ListType].NextDependency[ListType]:=Self;
  if NextDependency[ListType]<>nil then
    NextDependency[ListType].PrevDependency[ListType]:=OldPrev;
  OldPrev.NextDependency[ListType]:=NextDependency[ListType];
  PrevDependency[ListType]:=OldPrev.PrevDependency[ListType];
  NextDependency[ListType]:=OldPrev;
  OldPrev.PrevDependency[ListType]:=Self;
  if FirstDependency=OldPrev then FirstDependency:=Self;
  Result:=True;
end;

function TPkgDependency.MoveDownInList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList): Boolean;
var
  OldNext: TPkgDependency;
begin
  if (NextDependency[ListType]=nil) then exit(False);
  OldNext:=NextDependency[ListType];
  if OldNext.NextDependency[ListType]<>nil then
    OldNext.NextDependency[ListType].PrevDependency[ListType]:=Self;
  if PrevDependency[ListType]<>nil then
    PrevDependency[ListType].NextDependency[ListType]:=OldNext;
  OldNext.PrevDependency[ListType]:=PrevDependency[ListType];
  NextDependency[ListType]:=OldNext.NextDependency[ListType];
  PrevDependency[ListType]:=OldNext;
  OldNext.NextDependency[ListType]:=Self;
  if FirstDependency=Self then FirstDependency:=OldNext;
  Result:=True;
end;

function TPkgDependency.MakeFilenameRelativeToOwner(const AFilename: string): string;
var
  BaseDir: String;
begin
  Result:=AFilename;
  if (Result<>'')
  and (Owner<>nil) then begin
    BaseDir:=GetDependencyOwnerDirectory(Self);
    if BaseDir<>'' then
      Result:=CreateRelativePath(Result,BaseDir);
  end;
end;

function TPkgDependency.FindDefaultFilename: string;
var
  AFilename: String;
  CurDir: String;
begin
  Result:='';
  AFilename:=TrimFilename(DefaultFilename);
  if (CompareFileExt(AFilename,'lpk')<>0)
  or (SysUtils.CompareText(ExtractFileNameOnly(AFilename),PackageName)<>0) then
    exit;
  if not FilenameIsAbsolute(AFilename) then begin
    CurDir:=GetDependencyOwnerDirectory(Self);
    if (CurDir<>'') then
      AFilename:=AppendPathDelim(CurDir)+AFilename;
  end;
  if not FilenameIsAbsolute(AFilename) then exit;
  AFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(AFilename);
  if not FileExistsCached(AFilename) then exit;
  Result:=AFilename;
end;

{ TPackageIDEOptions }

function TPackageIDEOptions.GetPackage: TIDEPackage;
begin
  Result := FPackage;
end;

constructor TPackageIDEOptions.Create(APackage: TLazPackage);
begin
  inherited Create;
  FPackage := APackage;
end;

destructor TPackageIDEOptions.Destroy;
begin
  inherited Destroy;
end;

class function TPackageIDEOptions.GetInstance: TAbstractIDEOptions;
begin
  if Package1<>nil then
    Result := Package1.IDEOptions
  else
    Result := nil;
end;

class function TPackageIDEOptions.GetGroupCaption: string;
begin
  Result := lisPckOptsPackageOptions;
end;

{ TLazPackage }

procedure TLazPackage.OnMacroListSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s: string; const Data: PtrInt;
  var Handled, Abort: boolean; Depth: integer);
var
  Values: TCTCfgScriptVariables;
  Macro: PCTCfgScriptVariable;
var
  NewValue: String;
begin
  if Data=CompilerOptionMacroPlatformIndependent then
  begin
    NewValue:=GetMakefileMacroValue(MacroName);
    if NewValue<>'' then begin
      s:=NewValue;
      Handled:=true;
      exit;
    end;
  end;

  // check build macros
  if IsValidIdent(MacroName) then
  begin
    Values:=GetBuildMacroValues(CompilerOptions,true);
    if Values<>nil then begin
      Macro:=Values.GetVariable(PChar(MacroName));
      if Macro<>nil then
      begin
        s:=GetCTCSVariableAsString(Macro);
        //if MacroName='MyPackageOptions' then
        //  debugln(['TLazPackage.OnMacroListSubstitution Pkg=',Name,' Macro=',MacroName,' Value="',s,'"']);
        Handled:=true;
        exit;
      end;
    end;
  end;

  if s = '' then
  begin
  // check local macros
    if SysUtils.CompareText(MacroName,'PkgOutDir')=0 then begin
      Handled:=true;
      if Data=CompilerOptionMacroNormal then
        s:=GetOutputDirectory()
      else
        s:=CompilerOptions.ParsedOpts.GetParsedPIValue(pcosOutputDir);
      exit;
    end
    else if SysUtils.CompareText(MacroName,'PkgDir')=0 then begin
      Handled:=true;
      s:=FDirectory;
      exit;
    end
    else if SysUtils.CompareText(MacroName,'PkgName')=0 then begin
      Handled:=true;
      s:=Name;
      exit;
    end
    else if SysUtils.CompareText(MacroName,'PkgIncPath')=0 then begin
      Handled:=true;
      s:=GetIncludePath(false);
      exit;
    end
    else if SysUtils.CompareText(MacroName,'PkgSrcPath')=0 then begin
      Handled:=true;
      s:=SourceDirectories.CreateSearchPathFromAllFiles;
      exit;
    end
    else if SysUtils.CompareText(MacroName,'PkgUnitPath')=0 then begin
      Handled:=true;
      s:=GetUnitPath(false);
      exit;
    end;
  end;

  // check global macros
  GlobalMacroList.ExecuteMacro(MacroName,s,Data,Handled,Abort,Depth);
end;

procedure TLazPackage.SetUserReadOnly(const AValue: boolean);
begin
  if FUserReadOnly=AValue then exit;
  FUserReadOnly:=AValue;
end;

function TLazPackage.SubstitutePkgMacros(const s: string;
  PlatformIndependent: boolean): string;
begin
  Result:=s;
  if PlatformIndependent then
    FMacros.SubstituteStr(Result,CompilerOptionMacroPlatformIndependent)
  else
    FMacros.SubstituteStr(Result,CompilerOptionMacroNormal);
end;

procedure TLazPackage.WriteInheritedUnparsedOptions;
var
  OptionsList: TFPList;
  AddOptions: TAdditionalCompilerOptions;
  i: Integer;
begin
  OptionsList:=nil;
  CompilerOptions.GetInheritedCompilerOptions(OptionsList);
  if OptionsList<>nil then begin
    for i:=0 to OptionsList.Count-1 do begin
      AddOptions:=TAdditionalCompilerOptions(OptionsList[i]);
      if (not (AddOptions is TAdditionalCompilerOptions)) then continue;
      DebugLn('TLazPackage.WriteInheritedUnparsedOptions ',
        (AddOptions.Owner as TLazPackage).IDAsString,
        ' UnitPath="',AddOptions.GetOption(icoUnitPath),'"');
    end;
    OptionsList.Free;
  end;
end;

function TLazPackage.GetActiveBuildMethod: TBuildMethod;
begin
  Result:=BuildMethod;
  if Result=bmBoth then begin
    if Assigned(FppkgInterface) and FppkgInterface.UseFPMakeWhenPossible then
      Result:=bmFPMake
    else
      Result:=bmLazarus;
  end;
end;

function TLazPackage.GetAutoIncrementVersionOnBuild: boolean;
begin
  Result:=lpfAutoIncrementVersionOnBuild in FFlags;
end;

function TLazPackage.GetCompilerOptions: TPkgCompilerOptions;
begin
  Result := TPkgCompilerOptions(FLazCompilerOptions);
end;

function TLazPackage.GetBaseCompilerOptions: TBaseCompilerOptions;
// This satisfies the IProjPack interface requirement.
begin
  Result := TBaseCompilerOptions(FLazCompilerOptions);
end;

function TLazPackage.GetComponentCount: integer;
begin
  Result:=FComponents.Count;
end;

function TLazPackage.GetComponents(Index: integer): TPkgComponent;
begin
  Result:=TPkgComponent(FComponents[Index]);
end;

function TLazPackage.GetDirectoryExpanded: string;
begin
  if (FDirectoryExpandedChangeStamp<>CompilerParseStamp) then begin
    FDirectoryExpanded:=FDirectory;
    // use default macros (not package macros)
    if IDEMacros<>nil then
      IDEMacros.SubstituteMacros(FDirectoryExpanded);
    FDirectoryExpanded:=AppendPathDelim(TrimFilename(FDirectoryExpanded));
    FDirectoryExpandedChangeStamp:=CompilerParseStamp;
  end;
  Result:=FDirectoryExpanded;
end;

function TLazPackage.GetRemovedCount: integer;
begin
  Result:=FRemovedFiles.Count;
end;

function TLazPackage.GetRemovedPkgFiles(Index: integer): TLazPackageFile;
begin
  Result:=GetRemovedFiles(Index);
end;

procedure TLazPackage.AssignOptions(Source: TPersistent);
var
  aSource: TLazPackage;
begin
  inherited AssignOptions(Source);
  if Source is TLazPackage then
  begin
    aSource:=TLazPackage(Source);
    UserReadOnly:=aSource.UserReadOnly;
    Translated:=aSource.Translated;
    StorePathDelim:=aSource.StorePathDelim;
    // ToDo: PublishOptions.AssignOptions(aSource.PublishOptions);
    Provides.Assign(aSource.Provides);
    POOutputDirectory:=aSource.POOutputDirectory;
    PackageType:=aSource.PackageType;
    OutputStateFile:=aSource.OutputStateFile;
    License:=aSource.License;
    FPDocPaths:=aSource.FPDocPaths;
    FPDocPackageName:=aSource.FPDocPackageName;
    IconFile:=aSource.IconFile;
    UsageOptions.AssignOptions(aSource.UsageOptions);
    EnableI18N:=aSource.EnableI18N;
    EnableI18NForLFM:=aSource.EnableI18NForLFM;
    Description:=aSource.Description;
    AutoUpdate:=aSource.AutoUpdate;
    AutoIncrementVersionOnBuild:=aSource.AutoIncrementVersionOnBuild;
    Author:=aSource.Author;
    AddToProjectUsesSection:=aSource.AddToProjectUsesSection;
  end;
end;

function TLazPackage.GetRemovedFiles(Index: integer): TPkgFile;
begin
  If (Index >= 0) And (Index < FRemovedFiles.Count) Then
    Result:=TPkgFile(FRemovedFiles[Index])
  Else
    Result := NIL;
end;

function TLazPackage.GetDefineTemplates: TProjPackDefineTemplates;
begin
  Result:=FDefineTemplates;
end;

function TLazPackage.GetFileCount: integer;
begin
  Result:=FFiles.Count;
end;

function TLazPackage.GetPkgFiles(Index: integer): TLazPackageFile;
begin
  Result:=GetFiles(Index);
end;

function TLazPackage.GetFiles(Index: integer): TPkgFile;
begin
  Result:=TPkgFile(FFiles[Index]);
end;

function TLazPackage.GetIDEOptions: TPackageIDEOptions;
begin
  Result := TPackageIDEOptions(FIDEOptions);
end;

function TLazPackage.GetSourceDirectories: TFileReferenceList;
begin
  Result:=FSourceDirectories;
end;

function TLazPackage.GetModified: boolean;
begin
  Result:=(lpfModified in FFlags) or CompilerOptions.Modified;
end;

procedure TLazPackage.SetAddToProjectUsesSection(const AValue: boolean);
begin
  if FAddToProjectUsesSection=AValue then exit;
  FAddToProjectUsesSection:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetAuthor(const AValue: string);
begin
  if FAuthor=AValue then exit;
  FAuthor:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetAutoIncrementVersionOnBuild(const AValue: boolean);
begin
  if AutoIncrementVersionOnBuild=AValue then exit;
  if AValue then
    Include(FFlags,lpfAutoIncrementVersionOnBuild)
  else
    Exclude(FFlags,lpfAutoIncrementVersionOnBuild);
  Modified:=true;
end;

procedure TLazPackage.SetAutoInstall(AValue: TPackageInstallType);
begin
  if FAutoInstall=AValue then exit;
  FAutoInstall:=AValue;
end;

procedure TLazPackage.SetAutoUpdate(const AValue: TPackageUpdatePolicy);
begin
  if AValue=AutoUpdate then exit;
  FAutoUpdate:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetEnableI18NForLFM(AValue: boolean);
begin
  if FEnableI18NForLFM=AValue then Exit;
  FEnableI18NForLFM:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetFileReadOnly(const AValue: boolean);
begin
  if FFileReadOnly=AValue then exit;
  FFileReadOnly:=AValue;
end;

procedure TLazPackage.SetFilename(const AValue: string);
var
  NewFilename: String;
begin
  NewFilename:=AValue;
  ForcePathDelims(NewFilename);
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
  if (FFilename<>'') and (FFilename[length(FFilename)]=PathDelim) then
    FDirectory:=FFilename
  else
    FDirectory:=ExtractFilePath(FFilename);
  FDirectoryExpandedChangeStamp:=CTInvalidChangeStamp;
  FHasDirectory:=(FDirectory<>'') and (FDirectory[length(FDirectory)]=PathDelim);
  FHasStaticDirectory:=FHasDirectory and FilenameIsAbsolute(FDirectory);
  FUsageOptions.BaseDirectory:=FDirectory;
  CompilerOptions.BaseDirectory:=FDirectory;
  Modified:=true;
end;

procedure TLazPackage.SetFlags(const AValue: TLazPackageFlags);
var
  ChangedFlags: TLazPackageFlags;
begin
  if FFlags=AValue then exit;
  ChangedFlags:=(FFlags-AValue)+(AValue-FFlags);
  FFlags:=AValue;
  if ChangedFlags*[lpfAutoIncrementVersionOnBuild]<>[] then
    Modified:=true;
end;

procedure TLazPackage.SetFPDocPackageName(AValue: string);
begin
  if FFPDocPackageName=AValue then Exit;
  FFPDocPackageName:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetIconFile(const AValue: string);
begin
  if FIconFile=AValue then exit;
  FIconFile:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetInstalled(const AValue: TPackageInstallType);
begin
  if FInstalled=AValue then exit;
  FInstalled:=AValue;
end;

procedure TLazPackage.SetFPDocPaths(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimSearchPath(AValue,'');
  if FFPDocPaths=NewValue then exit;
  FFPDocPaths:=NewValue;
  Modified:=true;
end;

procedure TLazPackage.SetLicense(const AValue: string);
begin
  if FLicense=AValue then exit;
  FLicense:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetLPKSource(const AValue: TCodeBuffer);
begin
  if FLPKSource=AValue then exit;
  FLPKSource:=AValue;
  if LPKSource<>nil then
    FLPKSourceChangeStep:=LPKSource.ChangeStep;
  // do not change Filename here.
  // See TPkgManager.DoSavePackage and TPkgManager.DoOpenPackageFile
  // the LPKSource is the codebuffer last used during load/save, so it is not valid
  // for packages that were not yet loaded/saved or during renaming/loading/saving.
end;

procedure TLazPackage.SetOutputStateFile(const AValue: string);
var
  NewStateFile: String;
begin
  NewStateFile:=TrimFilename(AValue);
  if FOutputStateFile=NewStateFile then exit;
  FOutputStateFile:=NewStateFile;
end;

procedure TLazPackage.SetProvides(const AValue: TStrings);
begin
  if (AValue=FProvides) or (FProvides.Equals(AValue)) then exit;
  FProvides.Assign(AValue);
  Modified:=true;
end;

procedure TLazPackage.SetPOOutputDirectory(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FPOOutputDirectory=NewValue then exit;
  FPOOutputDirectory:=NewValue;
  Modified:=true;
end;

procedure TLazPackage.SetEnableI18N(const AValue: boolean);
begin
  if FEnableI18N=AValue then exit;
  FEnableI18N:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetRegistered(const AValue: boolean);
begin
  if FRegistered=AValue then exit;
  FRegistered:=AValue;
end;

procedure TLazPackage.ModifySilently;
begin
  if FModifiedLock>0 then exit;
  Include(FFlags,lpfModified);
  Exclude(FFlags,lpfSkipSaving);
  if FChangeStamp<High(FChangeStamp) then
    inc(FChangeStamp)
  else
    FChangeStamp:=low(FChangeStamp);
  if Assigned(FOnModifySilently) then
    FOnModifySilently(Self);
end;

procedure TLazPackage.SetModified(const AValue: boolean);
begin
  if AValue then begin
    if FModifiedLock>0 then exit;
    ModifySilently;
  end
  else begin
    FFlags:=FFlags-[lpfModified,lpfSkipSaving];
    PublishOptions.Modified:=false;
    CompilerOptions.Modified:=false;
  end;
  if Modified and (Editor<>nil) then
    Editor.UpdateAll(false);
end;

procedure TLazPackage.SetName(const NewName: TComponentName);
begin
  if Name=NewName then exit;
  inherited SetName(NewName);
  FDefineTemplates.IDChanged;
  Modified:=true;
end;

procedure TLazPackage.SetPackageEditor(const AValue: TBasePackageEditor);
begin
  if FPackageEditor=AValue then exit;
  FPackageEditor:=AValue;
end;

procedure TLazPackage.SetPackageType(const AValue: TLazPackageType);
begin
  if FPackageType=AValue then exit;
  FPackageType:=AValue;
  Modified:=true;
end;

procedure TLazPackage.SetStorePathDelim(const AValue: TPathDelimSwitch);
begin
  if FStorePathDelim=AValue then exit;
  FStorePathDelim:=AValue;
end;

constructor TLazPackage.Create;
begin
  inherited Create;
  FComponents:=TFPList.Create;
  FSourceDirectories:=TFileReferenceList.Create;
  FSourceDirectories.OnChanged:=@SourceDirectoriesChanged;
  FFiles:=TFPList.Create;
  FRemovedFiles:=TFPList.Create;
  FMacros:=TTransferMacroList.Create;
  FMacros.OnSubstitution:=@OnMacroListSubstitution;
  FIDEOptions:=TPackageIDEOptions.Create(Self);
  FLazCompilerOptions:=TPkgCompilerOptions.Create(Self);
  CompilerOptions.ParsedOpts.InvalidateParseOnChange:=true;
  CompilerOptions.ParsedOpts.OnLocalSubstitute:=@SubstitutePkgMacros;
  CompilerOptions.DefaultMakeOptionsFlags:=[ccloNoLinkerOpts];
  FUsageOptions:=TPkgAdditionalCompilerOptions.Create(Self);
  FUsageOptions.ParsedOpts.OnLocalSubstitute:=@SubstitutePkgMacros;
  FDefineTemplates:=TLazPackageDefineTemplates.Create(Self);
  fPublishOptions:=TPublishPackageOptions.Create(Self);
  FProvides:=TStringList.Create;
  Clear;
  FUsageOptions.ParsedOpts.InvalidateParseOnChange:=true;
end;

destructor TLazPackage.Destroy;
begin
  Include(FFlags,lpfDestroying);
  Clear;
  FreeAndNil(FOptionsBackup);
  FreeAndNil(fPublishOptions);
  FreeAndNil(FProvides);
  FreeAndNil(FDefineTemplates);
  FreeAndNil(FRemovedFiles);
  FreeAndNil(FFiles);
  FreeAndNil(FComponents);
  FreeAndNil(FLazCompilerOptions);
  FreeAndNil(FIDEOptions);
  FreeAndNil(FUsageOptions);
  FreeAndNil(FMacros);
  FreeAndNil(FSourceDirectories);
  inherited Destroy;
end;

procedure TLazPackage.BackupOptions;
begin
  if FOptionsBackup=nil then
    FOptionsBackup:=TLazPackage.Create;
  FOptionsBackup.AssignOptions(Self);
  FOptionsBackup.FFlags:=FOptionsBackup.FFlags-[lpfModified]+[lpfModified]*FFlags;
  FOptionsBackup.CompilerOptions.Modified:=CompilerOptions.Modified;
end;

procedure TLazPackage.RestoreOptions;
begin
  if FOptionsBackup=nil then exit;
  AssignOptions(FOptionsBackup);
  FFlags:=FFlags-[lpfModified]+[lpfModified]*FOptionsBackup.FFlags;
  CompilerOptions.Modified:=FOptionsBackup.CompilerOptions.Modified;
end;

procedure TLazPackage.BeginUpdate;
begin
  inc(FUpdateLock);
  FDefineTemplates.BeginUpdate;
  FSourceDirectories.BeginUpdate;
end;

procedure TLazPackage.EndUpdate;
begin
  if FUpdateLock=0 then RaiseGDBException('TLazPackage.EndUpdate');
  dec(FUpdateLock);
  FDefineTemplates.EndUpdate;
  FSourceDirectories.EndUpdate;
end;

procedure TLazPackage.Clear;
var
  i: Integer;
begin
  // break used-by dependencies
  while FFirstUsedByDependency<>nil do
    FFirstUsedByDependency.RequiredPackage:=nil;
  // break and free removed dependencies
  while FFirstRemovedDependency<>nil do
    DeleteRemovedDependency(FFirstRemovedDependency);
  // break and free required dependencies
  while FFirstRequiredDependency<>nil do
    DeleteRequiredDependency(FFirstRequiredDependency);
  if not (lpfDestroying in FFlags) then begin
    FAddToProjectUsesSection:=false;
    FAuthor:='';
    FAutoInstall:=pitNope;
    FComponents.Clear;
    CompilerOptions.Clear;
    FDescription:='';
    FDirectory:='';
    FDirectoryExpandedChangeStamp:=CTInvalidChangeStamp;
    FEnableI18N:=false;
    FEnableI18NForLFM:=false;
    FPOOutputDirectory:='';
    FHasDirectory:=false;
    FHasStaticDirectory:=false;
    FVersion.Clear;
    FFilename:='';
    FIconFile:='';
    FInstalled:=pitNope;
    Name:='';
    FPackageType:=lptRunAndDesignTime;
    FRegistered:=false;
    FFPDocPaths:='';
    FFPDocPackageName:='';
    ClearCustomOptions;
  end;
  for i:=FComponents.Count-1 downto 0 do Components[i].Free;
  for i:=FRemovedFiles.Count-1 downto 0 do RemovedFiles[i].Free;
  FRemovedFiles.Clear;
  for i:=FFiles.Count-1 downto 0 do Files[i].Free;
  FFiles.Clear;
  FUsageOptions.Clear;
  fPublishOptions.Clear;
  FProvides.Clear;
  UpdateSourceDirectories;
  // set some nice start values
  if not (lpfDestroying in FFlags) then begin
    FFlags:=[lpfAutoIncrementVersionOnBuild];
    FAutoUpdate:=pupAsNeeded;
    FLazCompilerOptions.UnitOutputDirectory:=
                           'lib'+PathDelim+'$(TargetCPU)-$(TargetOS)'+PathDelim;
    FUsageOptions.UnitPath:='$(PkgOutDir)';
  end else begin
    FFlags:=[lpfDestroying];
  end;
  FStorePathDelim:=pdsNone;
end;

//function DbgS(PkgFileType: TPkgFileType): string;
//begin
//  WriteStr(Result, PkgFileType);
//end;

procedure TLazPackage.UpdateSourceDirectories;
var
  Cnt: Integer;
  i: Integer;
  PkgFile: TPkgFile;
begin
  Cnt:=FFiles.Count;
  for i:=0 to Cnt-1 do begin
    PkgFile:=Files[i];
    PkgFile.FSourceDirectoryReferenced:=false;
  end;
  fSourceDirectories.Clear;
  for i:=0 to Cnt-1 do begin
    PkgFile:=Files[i];
    PkgFile.AutoReferenceSourceDir:=true;
    PkgFile.UpdateSourceDirectoryReference;
    //debugln('TLazPackage.UpdateSourceDirectories A ',PkgFile.Filename,' ',
    //  ' ',DbgS(PkgFile.FileType),' ',PkgFile.Removed,
    //  ' HasPkg=',dbgs(PkgFile.LazPackage=Self),
    //  ' Need=',PkgFile.FSourceDirNeedReference,
    //  ' Is=',PkgFile.FSourceDirectoryReferenced);
  end;
  //debugln('TLazPackage.UpdateSourceDirectories B ',IDAsString,' ',FFiles.Count,' "',fSourceDirectories.CreateSearchPathFromAllFiles,'"');
end;

procedure TLazPackage.VersionChanged(Sender: TObject);
begin
  inherited VersionChanged(Sender);
  FDefineTemplates.IDChanged;
  Modified:=true;
end;

procedure TLazPackage.SourceDirectoriesChanged(Sender: TObject);
begin
  FDefineTemplates.SourceDirectoriesChanged;
end;

function TLazPackage.GetDirectory: string;
begin
  Result:=FDirectory;
end;

procedure TLazPackage.LockModified;
begin
  inc(FModifiedLock);
end;

procedure TLazPackage.UnlockModified;
begin
  if FModifiedLock<=0 then
    RaiseGDBException('TLazPackage.UnlockModified');
  dec(FModifiedLock);
end;

function TLazPackage.ReadOnly: boolean;
begin
  Result:=UserReadOnly or FileReadOnly;
end;

procedure TLazPackage.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  FileVersion: integer;
  OldFilename: String;
  PathDelimChanged: boolean;
  Config: TXMLOptionsStorage;

  procedure LoadFiles(const ThePath: string; List: TFPList);
  var
    i: Integer;
    NewCount: Integer;
    PkgFile: TPkgFile;
  begin
    NewCount:=XMLConfig.GetValue(ThePath+'Count',0);
    for i:=0 to NewCount-1 do begin
      PkgFile:=TPkgFile.Create(Self);
      PkgFile.LoadFromXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i+1)+'/',
                                FileVersion,PathDelimChanged);
      if PkgFile.MakeSense then
        List.Add(PkgFile)
      else
        PkgFile.Free;
    end;
  end;
  
  procedure LoadFlags(const ThePath: string);
  begin
    if XMLConfig.GetValue(ThePath+'AutoIncrementVersionOnBuild/Value',true) then
      Include(FFlags,lpfAutoIncrementVersionOnBuild)
    else
      Exclude(FFlags,lpfAutoIncrementVersionOnBuild);
  end;

begin
  Flags:=Flags+[lpfLoading];
  FileVersion:=XMLConfig.GetValue(Path+'Version',0);
  OldFilename:=Filename;
  BeginUpdate;
  Clear;
  Filename:=OldFilename;
  LockModified;
  StorePathDelim:=CheckPathDelim(XMLConfig.GetValue(Path+'PathDelim/Value','/'),PathDelimChanged);
  Name:=XMLConfig.GetValue(Path+'Name/Value','');
  FPackageType:=LazPackageTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',
                                          LazPackageTypeIdents[lptRunTime]));
  FBuildMethod:=StringToBuildMethod(XMLConfig.GetValue(Path+'BuildMethod/Value',
                                    SBuildMethod[bmLazarus]));
  FAddToProjectUsesSection:=XMLConfig.GetValue(Path+'AddToProjectUsesSection/Value',
    FileVersion<4); // since version 4 the default is false
  FAuthor:=XMLConfig.GetValue(Path+'Author/Value','');
  FAutoUpdate:=NameToAutoUpdatePolicy(
                                XMLConfig.GetValue(Path+'AutoUpdate/Value',''));
  if FileVersion<2 then
    CompilerOptions.LoadFromXMLConfig(XMLConfig,'CompilerOptions/')
  else
    CompilerOptions.LoadFromXMLConfig(XMLConfig,Path+'CompilerOptions/');
  FDescription:=XMLConfig.GetValue(Path+'Description/Value','');
  FLicense:=XMLConfig.GetValue(Path+'License/Value','');
  PkgVersionLoadFromXMLConfig(FVersion,XMLConfig,Path+'Version/',FileVersion);
  FIconFile:=SwitchPathDelims(XMLConfig.GetValue(Path+'IconFile/Value',''),
                              PathDelimChanged);
  OutputStateFile:=SwitchPathDelims(
                            XMLConfig.GetValue(Path+'OutputStateFile/Value',''),
                            PathDelimChanged);
  FFPDocPaths:=SwitchPathDelims(XMLConfig.GetValue(Path+'LazDoc/Paths',''),
                            PathDelimChanged);
  FFPDocPackageName:=XMLConfig.GetValue(Path+'LazDoc/PackageName','');
  // i18n
  if FileVersion<3 then begin
    FPOOutputDirectory := SwitchPathDelims(
              xmlconfig.GetValue(Path+'RST/OutDir', ''),PathDelimChanged);
    EnableI18N := FPOOutputDirectory <> '';
  end else begin
    EnableI18N := xmlconfig.GetValue(Path+'i18n/EnableI18N/Value', False);
    FPOOutputDirectory := SwitchPathDelims(
             xmlconfig.GetValue(Path+'i18n/OutDir/Value', ''),PathDelimChanged);
  end;
  EnableI18NForLFM:=xmlconfig.GetValue(Path+'i18n/EnableI18NForLFM/Value', false);

  LoadFiles(Path+'Files/',FFiles);
  UpdateSourceDirectories;
  LoadFlags(Path);
  LoadPkgDependencyList(XMLConfig,Path+'RequiredPkgs/',
                        FFirstRequiredDependency,pdlRequires,Self,false,false);
  FUsageOptions.LoadFromXMLConfig(XMLConfig,Path+'UsageOptions/',
                                  PathDelimChanged);
  fPublishOptions.LoadFromXMLConfig(XMLConfig,Path+'PublishOptions/',
                                    PathDelimChanged);
  LoadStringList(XMLConfig,FProvides,Path+'Provides/');
  Config:=TXMLOptionsStorage.Create(XMLConfig);
  try
    TConfigMemStorage(CustomOptions).LoadFromConfig(Config,Path+'CustomOptions/');
  finally
    Config.Free;
  end;

  EndUpdate;
  Modified:=false;
  UnlockModified;
  Flags:=Flags-[lpfLoading];
end;

procedure TLazPackage.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var
  UsePathDelim: TPathDelimSwitch;
  Config: TXMLOptionsStorage;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveFiles(const ThePath: string; List: TFPList);
  var
    i: Integer;
    PkgFile: TPkgFile;
  begin
    XMLConfig.SetDeleteValue(ThePath+'Count',List.Count,0);
    for i:=0 to List.Count-1 do begin
      PkgFile:=TPkgFile(List[i]);
      PkgFile.SaveToXMLConfig(XMLConfig,ThePath+'Item'+IntToStr(i+1)+'/',UsePathDelim);
    end;
  end;
  
  procedure SaveFlags(const ThePath: string);
  begin
    XMLConfig.SetDeleteValue(ThePath+'AutoIncrementVersionOnBuild/Value',
      AutoIncrementVersionOnBuild,true);
  end;

begin
  UsePathDelim:=StorePathDelim;
  XMLConfig.SetValue(Path+'Version',LazPkgXMLFileVersion);
  XMLConfig.SetDeleteValue(Path+'PathDelim/Value',PathDelimSwitchToDelim[UsePathDelim],'/');
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  XMLConfig.SetDeleteValue(Path+'Type/Value',LazPackageTypeIdents[FPackageType],
                           LazPackageTypeIdents[lptRunTime]);
  XMLConfig.SetDeleteValue(Path+'BuildMethod/Value',SBuildMethod[FBuildMethod],
                           SBuildMethod[bmLazarus]);
  XMLConfig.SetDeleteValue(Path+'AddToProjectUsesSection/Value',
                           FAddToProjectUsesSection,false);
  XMLConfig.SetDeleteValue(Path+'Author/Value',FAuthor,'');
  XMLConfig.SetDeleteValue(Path+'AutoUpdate/Value',AutoUpdateNames[FAutoUpdate],
                           AutoUpdateNames[pupAsNeeded]);
  CompilerOptions.SaveToXMLConfig(XMLConfig,Path+'CompilerOptions/');
  XMLConfig.SetDeleteValue(Path+'Description/Value',FDescription,'');
  XMLConfig.SetDeleteValue(Path+'License/Value',FLicense,'');
  PkgVersionSaveToXMLConfig(FVersion,XMLConfig,Path+'Version/');
  SaveFiles(Path+'Files/',FFiles);
  SaveFlags(Path);
  XMLConfig.SetDeleteValue(Path+'IconFile/Value',f(FIconFile),'');
  XMLConfig.SetDeleteValue(Path+'OutputStateFile/Value',f(OutputStateFile),'');
  XMLConfig.SetDeleteValue(Path+'LazDoc/Paths',f(FFPDocPaths),'');
  XMLConfig.SetDeleteValue(Path+'LazDoc/PackageName',FFPDocPackageName,'');

  XMLConfig.SetDeleteValue(Path+'i18n/EnableI18N/Value', EnableI18N, false);
  XMLConfig.SetDeleteValue(Path+'i18n/OutDir/Value',f(FPOOutputDirectory), '');
  XMLConfig.SetDeleteValue(Path+'i18n/EnableI18NForLFM/Value', EnableI18NForLFM, false);

  SavePkgDependencyList(XMLConfig,Path+'RequiredPkgs/',
                        FFirstRequiredDependency,pdlRequires,UsePathDelim);
  FUsageOptions.SaveToXMLConfig(XMLConfig,Path+'UsageOptions/',UsePathDelim);
  fPublishOptions.SaveToXMLConfig(XMLConfig,Path+'PublishOptions/',UsePathDelim);
  SaveStringList(XMLConfig,FProvides,Path+'Provides/');
  Config:=TXMLOptionsStorage.Create(XMLConfig);
  try
    TConfigMemStorage(CustomOptions).SaveToConfig(Config,Path+'CustomOptions/');
  finally
    Config.Free;
  end;
  Modified:=false;
end;

procedure TLazPackage.SaveToString(out s: string);
var
  XMLConfig: TXMLConfig;
  ms: TMemoryStream;
begin
  s:='';
  XMLConfig:=TXMLConfig.Create(nil);
  ms:=TMemoryStream.Create;
  try
    XMLConfig.Clear;
    SaveToXMLConfig(XMLConfig,'Package/');
    XMLConfig.WriteToStream(ms);
    ms.Position:=0;
    SetLength(s,ms.Size);
    if s<>'' then
      ms.Read(s[1],length(s));
  finally
    XMLConfig.Free;
    ms.Free;
  end;
end;

function TLazPackage.IsVirtual: boolean;
begin
  Result:=not FilenameIsAbsolute(Filename);
end;

function TLazPackage.HasDirectory: boolean;
begin
  Result:=FHasDirectory;
end;

function TLazPackage.HasStaticDirectory: boolean;
begin
  Result:=FHasStaticDirectory;
end;

function TLazPackage.GetFullFilename(ResolveMacros: boolean): string;
begin
  Result:=FFilename;
  if ResolveMacros then
    GlobalMacroList.SubstituteStr(Result);
end;

procedure TLazPackage.CheckInnerDependencies;
begin
  // ToDo: make some checks like deactivating double requirements
end;

function TLazPackage.IsMakingSense: boolean;
begin
  Result:=IsValidPkgName(Name);
end;

procedure TLazPackage.ShortenFilename(var ExpandedFilename: string; UseUp: boolean);
var
  PkgDir: String;
  CurPath: String;
begin
  if (not HasDirectory) then exit;
  PkgDir:=DirectoryExpanded;
  if HasStaticDirectory and UseUp then
    ExpandedFilename:=CreateRelativePath(ExpandedFilename,PkgDir)
  else begin
    CurPath:=copy(ExtractFilePath(ExpandedFilename),1,length(PkgDir));
    if CompareFilenames(PkgDir,CurPath)=0 then begin
      ExpandedFilename:=copy(ExpandedFilename,length(CurPath)+1,
                             length(ExpandedFilename)-length(CurPath));
    end;
  end;
end;

procedure TLazPackage.LongenFilename(var AFilename: string);
begin
  if not HasDirectory then exit;
  if not FilenameIsAbsolute(AFilename) then
    AFilename:=TrimFilename(DirectoryExpanded+AFilename);
end;

function TLazPackage.GetResolvedFilename(ResolveMacros: boolean): string;
begin
  Result:=GetPhysicalFilenameCached(GetFullFilename(ResolveMacros),false);
end;

function TLazPackage.GetSourceDirs(WithPkgDir, WithoutOutputDir: boolean): string;
begin
  Result:=SourceDirectories.CreateSearchPathFromAllFiles;
  if WithPkgDir then
    Result:=MergeSearchPaths(Result,Directory);
  if WithoutOutputDir then
    Result:=RemoveSearchPaths(Result,GetOutputDirectory);
end;

procedure TLazPackage.IterateComponentClasses(Event: TIterateComponentClassesEvent;
  WithUsedPackages: boolean);
var
  Cnt: Integer;
  i: Integer;
  Dependency: TPkgDependency;
begin
  // iterate through components in this package
  Cnt:=ComponentCount;
  for i:=0 to Cnt-1 do Event(Components[i]);
  // iterate through all used/required packages
  if WithUsedPackages then begin
    Dependency:=FirstRequiredDependency;
    while Dependency<>nil do begin
      if Dependency.RequiredPackage<>nil then
        Dependency.RequiredPackage.IterateComponentClasses(Event,false);
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
end;

procedure TLazPackage.SetAllComponentPriorities(const p: TComponentPriority);
var
  i: Integer;
begin
  //debugln(['TLazPackage.SetAllComponentPriorities ',Name,' ',dbgs(p), ' FileCount=',FileCount]);
  for i:=0 to FileCount-1 do
    Files[i].ComponentPriority:=p;
end;

procedure TLazPackage.ConsistencyCheck;
begin
  CheckList(FRemovedFiles,true,true,true);
  CheckList(FFiles,true,true,true);
  CheckList(FComponents,true,true,true);
end;

function TLazPackage.ExtendUnitSearchPath(NewUnitPaths: string): boolean;
var
  CurUnitPaths: String;
  r: TModalResult;
begin
  Result:=True;
  CurUnitPaths:=CompilerOptions.ParsedOpts.GetParsedValue(pcosUnitPath);
  NewUnitPaths:=RemoveSearchPaths(NewUnitPaths,CurUnitPaths);
  if NewUnitPaths='' then Exit;
  NewUnitPaths:=CreateRelativeSearchPath(NewUnitPaths,Directory);
  if NewUnitPaths='.' then Exit;
  r:=IDEMessageDialog(lisExtendUnitPath,
        Format(lisExtendUnitSearchPathOfPackageWith, [Name, #13, NewUnitPaths]),
        mtConfirmation, [mbYes, mbNo, mbCancel]);
  case r of
    mrYes: CompilerOptions.MergeToUnitPaths(NewUnitPaths);
    mrNo: ;
  else exit(false);
  end;
end;

function TLazPackage.ExtendIncSearchPath(NewIncPaths: string): boolean;
var
  CurIncPaths: String;
  r: TModalResult;
begin
  Result:=True;
  CurIncPaths:=CompilerOptions.ParsedOpts.GetParsedValue(pcosIncludePath);
  NewIncPaths:=RemoveSearchPaths(NewIncPaths,CurIncPaths);
  if NewIncPaths='' then Exit;
  NewIncPaths:=CreateRelativeSearchPath(NewIncPaths,Directory);
  if NewIncPaths='.' then Exit;
  r:=IDEMessageDialog(lisExtendIncludePath,
      Format(lisExtendIncludeFileSearchPathOfPackageWith, [Name, #13, NewIncPaths]),
      mtConfirmation, [mbYes, mbNo, mbCancel]);
  case r of
  mrYes: CompilerOptions.MergeToIncludePaths(NewIncPaths);
  mrNo: ;
  else exit(false);
  end;
end;

function TLazPackage.IndexOfPkgComponent(PkgComponent: TPkgComponent): integer;
begin
  Result:=FComponents.IndexOf(PkgComponent);
end;

function TLazPackage.FindPkgFile(const AFilename: string; IgnoreRemoved,
  FindVirtualFile: boolean): TPkgFile;
var
  TheFilename: String;
  Cnt: Integer;
  i: Integer;
begin
  Result:=nil;
  
  TheFilename:=AFilename;
  
  Cnt:=FileCount;
  for i:=0 to Cnt-1 do begin
    Result:=Files[i];
    if (not FindVirtualFile) and (not FilenameIsAbsolute(Result.Filename)) then
      continue;
    if (CompareFilenames(Result.Filename,TheFilename)=0)
    or (CompareFilenames(Result.GetFullFilename,TheFilename)=0) then
      exit;
  end;
  if not IgnoreRemoved then begin
    Cnt:=RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      Result:=RemovedFiles[i];
      if (not FindVirtualFile) and (not FilenameIsAbsolute(Result.Filename)) then
        continue;
      if (CompareFilenames(Result.Filename,TheFilename)=0)
      or (CompareFilenames(Result.GetFullFilename,TheFilename)=0) then
        exit;
    end;
  end;
  Result:=nil;
end;

function TLazPackage.FindUnitWithRegister(IgnorePkgFile: TPkgFile): TPkgFile;
var
  Cnt: LongInt;
  i: Integer;
begin
  Cnt:=FileCount;
  for i:=0 to Cnt-1 do begin
    Result:=Files[i];
    if IgnorePkgFile=Result then continue;
    if not (Result.FileType in PkgFileRealUnitTypes) then continue;
    if Result.HasRegisterProc then exit;
  end;
  Result:=nil;
end;

function TLazPackage.FindUnit(const TheUnitName: string): TPkgFile;
begin
  Result:=FindUnit(TheUnitName,true);
end;

function TLazPackage.FindUnit(const TheUnitName: string;
  IgnoreRemoved: boolean): TPkgFile;
begin
  Result:=FindUnit(TheUnitName,IgnoreRemoved,nil);
end;

function TLazPackage.FindUnit(const TheUnitName: string;
  IgnoreRemoved: boolean; IgnorePkgFile: TPkgFile): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  if TheUnitName='' then exit(nil);
  Cnt:=FileCount;
  for i:=0 to Cnt-1 do begin
    Result:=Files[i];
    if IgnorePkgFile=Result then continue;
    if SysUtils.CompareText(Result.Unit_Name,TheUnitName)=0 then exit;
  end;
  if not IgnoreRemoved then begin
    Cnt:=RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      Result:=RemovedFiles[i];
      if IgnorePkgFile=Result then continue;
      if SysUtils.CompareText(Result.Unit_Name,TheUnitName)=0 then exit;
    end;
  end;
  Result:=nil;
end;

function TLazPackage.FindUsedUnit(TheUnitName: string; IgnorePkgFile: TPkgFile
  ): TPkgFile;
var
  i: Integer;
begin
  for i:=0 to FileCount-1 do begin
    Result:=Files[i];
    if IgnorePkgFile=Result then continue;
    if not Result.AddToUsesPkgSection then continue;
    if not (Result.FileType in PkgFileRealUnitTypes) then continue;
    if SysUtils.CompareText(Result.Unit_Name,TheUnitName)=0 then exit;
  end;
  Result:=nil;
end;

function TLazPackage.FindRemovedPkgFile(const AFilename: string): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=RemovedFilesCount;
  for i:=0 to Cnt-1 do begin
    Result:=RemovedFiles[i];
    if CompareFilenames(Result.Filename,AFilename)=0 then exit;
  end;
  Result:=nil;
end;

function TLazPackage.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=FindDependencyByNameInList(FFirstRequiredDependency,pdlRequires,PackageName);
end;

function TLazPackage.FindRemovedDependencyByName(const PkgName: string): TPkgDependency;
begin
  Result:=FindDependencyByNameInList(FFirstRemovedDependency,pdlRequires,PkgName);
end;

function TLazPackage.RequiredDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRequiredDependency,pdlRequires,Index);
end;

function TLazPackage.RemovedDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRemovedDependency,pdlRequires,Index);
end;

function TLazPackage.UsedByDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstUsedByDependency,pdlUsedBy,Index);
end;

function TLazPackage.FindUsedByDepPrefer(Ignore: TPkgDependency): TPkgDependency;
begin
  Result:=FFirstUsedByDependency;
  while (Result<>nil) do begin
    if Result.PreferDefaultFilename
    and (Result<>Ignore) then
      exit;
    Result:=Result.NextUsedByDependency;
  end;
end;

function TLazPackage.ProvidesPackage(const AName: string): boolean;
var
  i: Integer;
begin
  if AName='' then exit(false);
  for i:=0 to Provides.Count-1 do
    if SysUtils.CompareText(Provides[i],AName)=0 then begin
      //DebugLn(['TLazPackage.ProvidesPackage AName=',AName,' Provides[i]="',Provides[i],'"']);
      exit(true);
    end;
  Result:=false;
end;

function TLazPackage.AddFile(const NewFilename, NewUnitName: string;
  NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
  CompPriorityCat: TComponentPriorityCategory): TPkgFile;
var
  NewComponentPriority: TComponentPriority;
begin
  Result:=FindRemovedPkgFile(NewFilename);
  if Result=nil then begin
    Result:=TPkgFile.Create(Self);
  end else begin
    Result.AutoReferenceSourceDir:=false;
    FRemovedFiles.Remove(Result);
    Result.Removed:=false;
  end;
  with Result do begin
    Filename:=NewFilename;
    //debugln(['TLazPackage.AddFile Is=',Filename,' Should=',NewFilename]);
    Unit_Name:=NewUnitName;
    FileType:=NewFileType;
    Flags:=NewFlags;
    NewComponentPriority:=ComponentPriorityNormal;
    NewComponentPriority.Category:=CompPriorityCat;
    ComponentPriority:=NewComponentPriority;
    Removed:=false;
    AutoReferenceSourceDir:=true;
  end;
  FFiles.Add(Result);
  //debugln(['TLazPackage.AddFile Is=',Result.Filename,' Should=',NewFilename]);
  Modified:=true;
end;

function TLazPackage.AddFileByName(aFilename: string;
  var NewUnitPaths, NewIncPaths: String): Boolean;
var
  NewFileType: TPkgFileType;
  NewUnitName: String;
  HasRegister: Boolean;
  NewFlags: TPkgFileFlags;
  Code: TCodeBuffer;
  CurDir: String;
begin
  Result := True;
  aFilename:=CleanAndExpandFilename(aFileName);
  if not FileExistsUTF8(aFilename) then Exit(False);
  if DirPathExists(aFilename) then Exit(False);
  if FindPkgFile(aFilename,true,false)<>nil then Exit(False);
  NewFileType:=FileNameToPkgFileType(aFilename);
  NewFlags:=[];
  HasRegister:=false;
  NewUnitName:='';
  if (NewFileType=pftUnit) then begin
    Code:=CodeToolBoss.LoadFile(aFilename,true,false);
    NewUnitName:=CodeToolBoss.GetSourceName(Code,false);
    if NewUnitName='' then
      NewUnitName:=ExtractFileNameOnly(aFilename);
    if FindUsedUnit(NewUnitName)=nil then
      Include(NewFlags,pffAddToPkgUsesSection);
    CodeToolBoss.HasInterfaceRegisterProc(Code,HasRegister);
    if HasRegister then
      Include(NewFlags,pffHasRegisterProc);
  end;
  AddFile(aFilename,NewUnitName,NewFileType,NewFlags,cpNormal);
  CurDir:=ChompPathDelim(ExtractFilePath(aFilename));
  if NewFileType=pftUnit then
    NewUnitPaths:=MergeSearchPaths(NewUnitPaths,CurDir)
  else
    NewIncPaths:=MergeSearchPaths(NewIncPaths,CurDir);
end;

function TLazPackage.AddRemovedFile(const NewFilename, NewUnitName: string;
  NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
  CompPriorityCat: TComponentPriorityCategory): TPkgFile;
var
  NewComponentPriority: TComponentPriority;
begin
  Result:=FindRemovedPkgFile(NewFilename);
  if Result=nil then begin
    Result:=TPkgFile.Create(Self);
  end;
  with Result do begin
    AutoReferenceSourceDir:=false;
    Filename:=NewFilename;
    Unit_Name:=NewUnitName;
    FileType:=NewFileType;
    Flags:=NewFlags;
    NewComponentPriority:=ComponentPriorityNormal;
    NewComponentPriority.Category:=CompPriorityCat;
    ComponentPriority:=NewComponentPriority;
    Removed:=false;
    AutoReferenceSourceDir:=true;
  end;
  FRemovedFiles.Add(Result);
end;

procedure TLazPackage.DeleteFile(PkgFile: TPkgFile);
begin
  PkgFile.Free;
  Modified:=true
end;

procedure TLazPackage.RemoveFileSilently(PkgFile: TPkgFile);
// Remove a file without setting the Modified flag. Caller must take care of it.
begin
  FFiles.Remove(PkgFile);
  FRemovedFiles.Add(PkgFile);
  PkgFile.Removed:=true;
end;

procedure TLazPackage.RemoveFile(PkgFile: TPkgFile);
begin
  RemoveFileSilently(PkgFile);
  Modified:=true;
end;

procedure TLazPackage.UnremovePkgFile(PkgFile: TPkgFile);
begin
  FFiles.Add(PkgFile);
  FRemovedFiles.Remove(PkgFile);
  PkgFile.Removed:=false;
end;

function TLazPackage.RemoveNonExistingFiles(RemoveFromUsesSection: boolean): boolean;
// Param is ignored here, it is just to match with interface.
var
  i: Integer;
  AFilename: String;
begin
  Result:=false;
  i:=FileCount-1;
  while i>=0 do begin
    if i>=FileCount then continue;
    AFilename:=Files[i].GetResolvedFilename;
    if (AFilename='') or (not FileExistsCached(AFilename)) then
    begin
      RemoveFile(Files[i]);
      Result:=true;
    end;
    dec(i);
  end;
end;

function TLazPackage.GetFileDialogInitialDir(const DefaultDirectory: string): string;
begin
  Result:=AppendPathDelim(TrimFilename(DefaultDirectory));
  if (SourceDirectories.GetFileReference(Result)=nil)
  and DirPathExists(Directory) then
    Result:=Directory;
end;

procedure TLazPackage.MoveFile(CurIndex, NewIndex: integer);
begin
  if CurIndex=NewIndex then exit;
  FFiles.Move(CurIndex,NewIndex);
  Include(FFlags,lpfModified);
  if FChangeStamp<High(FChangeStamp) then
    inc(FChangeStamp)
  else
    FChangeStamp:=low(FChangeStamp);
end;

procedure TLazPackage.SortFiles;
var
  NewList: TFPList;
  Cnt: Integer;
  i: Integer;
begin
  if FileCount=0 then exit;
  NewList:=TFPList.Create;
  try
    Cnt:=FileCount;
    for i:=0 to Cnt-1 do NewList.Add(FFiles[i]);
    NewList.Sort(TListSortCompare(@ComparePkgFilesAlphabetically));
    i:=Cnt-1;
    while (i>=0) and (NewList[i]=FFiles[i]) do dec(i);
    if i<0 then exit;
    FFiles.Clear;
    for i:= 0 to Cnt-1 do FFiles.Add(NewList[i]);
    Modified:=true;
  finally
    NewList.Free;
  end;
end;

function TLazPackage.FixFilesCaseSensitivity: boolean;
var
  SrcDirs: TStringList;
  
  function IndexOfFileInStringList(List: TStringList;
    const Filename: string; OnlyExact: boolean): integer;
  begin
    // first search for exact match
    Result:=List.Count-1;
    while (Result>=0) do begin
      if (Filename=List[Result]) then exit;
      dec(Result);
    end;
    if OnlyExact then exit;
    // then search for case insensitive match
    Result:=List.Count-1;
    while (Result>=0) and (SysUtils.CompareText(Filename,List[Result])<>0) do
      dec(Result);
  end;

  function AddDirectoryListing(const ADirectory: string): TStringList;
  var
    SrcDirID: Integer;
    FileInfo: TSearchRec;
  begin
    if SrcDirs=nil then
      SrcDirs:=TStringList.Create;
    // search directory listing
    SrcDirID:=IndexOfFileInStringList(SrcDirs,ADirectory,true);
    if SrcDirID>=0 then begin
      Result:=TStringList(SrcDirs.Objects[SrcDirID]);
      exit;
    end;
    // create new directory listing
    Result:=TStringList.Create;
    if FindFirstUTF8(AppendPathDelim(ADirectory)+GetAllFilesMask,
                          faAnyFile,FileInfo)=0
    then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then continue;
        Result.Add(FileInfo.Name);
        //debugln('AddDirectoryListing ',FileInfo.Name);
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
    SrcDirs.AddObject(ADirectory,Result);
  end;

var
  Cnt: Integer;
  i: Integer;
  CurFile: TPkgFile;
  CurShortFilename: String;
  DirListID: LongInt;
  DirListing: TStringList;
  NewShortFilename: string;
  NewFilename: String;
  CurDir: String;
  AFilename: String;
begin
  Result:=false;
  Cnt:=FileCount;
  SrcDirs:=nil;
  try
    for i:=0 to Cnt-1 do begin
      CurFile:=Files[i];
      //debugln('TLazPackage.FixFilesCaseSensitivity A ',dbgs(i),' CurFile.Filename=',CurFile.Filename);
      AFilename:=CurFile.GetFullFilename;
      CurShortFilename:=ExtractFilename(AFilename);
      CurDir:=ExtractFilePath(AFilename);
      DirListing:=AddDirectoryListing(CurDir);
      DirListID:=IndexOfFileInStringList(DirListing,CurShortFilename,false);
      //debugln('TLazPackage.FixFilesCaseSensitivity B ',dbgs(i),' CurShortFilename=',CurShortFilename,' DirListID=',dbgs(DirListID));
      if DirListID<0 then continue;
      NewShortFilename:=DirListing[DirListID];
      //debugln('TLazPackage.FixFilesCaseSensitivity New ',dbgs(i),' NewShortFilename=',NewShortFilename);
      if CurShortFilename<>NewShortFilename then begin
        // case changes
        NewFilename:=AppendPathDelim(ExtractFilePath(CurFile.Filename))+NewShortFilename;
        //debugln('TLazPackage.FixFilesCaseSensitivity New ',dbgs(i),' NewFilename=',NewFilename);
        CurFile.Filename:=NewFilename;
        Result:=true;
      end;
    end;
    if Result then
      Modified:=true;
  finally
    if SrcDirs<>nil then begin
      for i:=0 to SrcDirs.Count-1 do
        SrcDirs.Objects[i].Free;
      SrcDirs.Free;
    end;
  end;
end;

function TLazPackage.MainUnitHasPkgName: boolean;
begin
  Result:=(MainUnit=nil) or (SysUtils.CompareText(MainUnit.Unit_Name,Name)=0);
end;

procedure TLazPackage.RemoveRemovedDependency(Dependency: TPkgDependency);
begin
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=false;
end;

procedure TLazPackage.AddRequiredDependency(Dependency: TPkgDependency);
begin
  Dependency.AddToList(FFirstRequiredDependency,pdlRequires);
  Dependency.Owner:=Self;
  Modified:=true;
end;

procedure TLazPackage.AddPackageDependency(const PackageName: string);
var
  Dependency: TPkgDependency;
begin
  if FindDependencyByName(PackageName)<>nil then exit;
  Dependency:=TPkgDependency.Create;
  Dependency.PackageName:=PackageName;
  AddRequiredDependency(Dependency);
end;

procedure TLazPackage.RemoveRequiredDepSilently(Dependency: TPkgDependency);
// Remove a dependency without setting the Modified flag. Caller must take care of it.
begin
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.RequiredPackage:=nil;
  Dependency.AddToList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=true;
end;

procedure TLazPackage.RemoveRequiredDependency(Dependency: TPkgDependency);
begin
  RemoveRequiredDepSilently(Dependency);
  Modified:=true;
end;

procedure TLazPackage.DeleteRequiredDependency(Dependency: TPkgDependency);
begin
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.Free;
end;

procedure TLazPackage.DeleteRemovedDependency(Dependency: TPkgDependency);
begin
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Free;
end;

function TLazPackage.MoveRequiredDependencyUp(Dependency: TPkgDependency): Boolean;
begin
  Result := Dependency.MoveUpInList(FFirstRequiredDependency,pdlRequires);
end;

function TLazPackage.MoveRequiredDependencyDown(Dependency: TPkgDependency): Boolean;
begin
  Result := Dependency.MoveDownInList(FFirstRequiredDependency,pdlRequires);
end;

function TLazPackage.CreateDependencyWithOwner(NewOwner: TObject;
  WithMinVersion: boolean): TPkgDependency;
begin
  Result:=TPkgDependency.Create;
  with Result do begin
    Owner:=NewOwner;
    PackageName:=Self.Name;
    if WithMinVersion then begin
      MinVersion.Assign(Version);
      Flags:=[pdfMinVersion];
    end;
  end;
end;

function TLazPackage.AddComponent(PkgFile: TPkgFile; const Page: string;
  TheComponentClass: TComponentClass): TPkgComponent;
begin
  Result:=TPkgComponent.Create(PkgFile,TheComponentClass,Page);
end;

procedure TLazPackage.AddPkgComponent(APkgComponent: TPkgComponent);
begin
  FComponents.Add(APkgComponent);
end;

procedure TLazPackage.RemovePkgComponent(APkgComponent: TPkgComponent);
begin
  FComponents.Remove(APkgComponent);
end;

function TLazPackage.Requires(APackage: TLazPackage): boolean;
begin
  Result:=FindCompatibleDependencyInList(FFirstRequiredDependency,pdlRequires,
                  APackage)<>nil;
end;

procedure TLazPackage.AddUsedByDependency(Dependency: TPkgDependencyBase);
begin
  Dependency.AddUsedByDep(TPkgDependencyBase(FFirstUsedByDependency));
  if TPkgDependency(Dependency).HoldPackage then
    inc(FHoldPackageCount);
end;

procedure TLazPackage.RemoveUsedByDependency(Dependency: TPkgDependencyBase);
begin
  Dependency.RemoveUsedByDep(TPkgDependencyBase(FFirstUsedByDependency));
  if TPkgDependency(Dependency).HoldPackage then
    dec(FHoldPackageCount);
end;

procedure TLazPackage.ChangeID(const NewName: string; NewVersion: TPkgVersion);
begin
  Version.Assign(NewVersion);
  Name:=NewName;
end;

function TLazPackage.GetFPDocPackageName: string;
begin
  if FPDocPackageName<>'' then
    Result:=FPDocPackageName
  else
    Result:=Name;
end;

function TLazPackage.GetOutputDirType: TPkgOutputDir;
begin
  if (CompilerOptions<>nil)
  and (CompilerOptions.ParsedOpts<>nil)
  and (CompilerOptions.ParsedOpts.OutputDirectoryOverride<>'') then
    Result:=podFallback
  else
    Result:=podDefault;
end;

procedure TLazPackage.GetAllRequiredPackages(var List, FPMakeList: TFPList;
  WithSelf: boolean; aFlags: TPkgIntfRequiredFlags;
  MinPolicy: TPackageUpdatePolicy);
begin
  if Assigned(OnGetAllRequiredPackages) then
    OnGetAllRequiredPackages(Self,FirstRequiredDependency,List,FPMakeList,aFlags,MinPolicy);
  if WithSelf then begin
    if List=nil then List:=TFPList.Create;
    if List.IndexOf(Self)<0 then
      List.Insert(0,Self);
  end else if List<>nil then begin
    List.Remove(Self);
    if List.Count=0 then FreeAndNil(List);
  end;
end;

procedure TLazPackage.GetInheritedCompilerOptions(var OptionsList: TFPList);
var
  PkgList: TFPList; // list of TLazPackage
  FPMakeList: TFPList;
begin
  PkgList:=nil;
  FPMakeList:=nil;
  GetAllRequiredPackages(PkgList,FPMakeList,false,[pirCompileOrder]);
  OptionsList:=GetUsageOptionsList(PkgList);
  PkgList.Free;
  FPMakeList.Free;
end;

function TLazPackage.GetCompileSourceFilename: string;
begin
  if MainUnit<>nil then
    Result:=ExtractFilename(MainUnit.GetFullFilename)
  else
    Result:=ChangeFileExt(ExtractFilename(Filename),'.pas');
end;

function TLazPackage.GetOutputDirectory(UseOverride: boolean = true): string;
begin
  if HasDirectory then begin
    if GetActiveBuildMethod = bmFPMake then begin
      Result :=TFppkgHelper.Instance.GetPackageUnitPath(name);
    end else begin
      Result:=CompilerOptions.ParsedOpts.GetParsedValue(pcosOutputDir,UseOverride);
    end;
  end else
    Result:='';
end;

function TLazPackage.HasSeparateOutputDirectory: boolean;
var
  VisitedPackages: TStringToStringTree;
  OutputDir: String;

  function CheckDependency(ADependency: TPkgDependency): boolean;
  var
    aPkg: TLazPackage;
    Dir: String;
    SrcPaths: String;
  begin
    Result:=false;
    while ADependency<>nil do begin
      if ADependency.RequiredPackage<>nil then begin
        aPkg:=ADependency.RequiredPackage;
        if not VisitedPackages.Contains(aPkg.Name) then begin
          VisitedPackages[aPkg.Name]:='1';
          // check recursively
          if not CheckDependency(aPkg.FirstRequiredDependency) then exit;
          // check if required package has the same output directory
          Dir:=aPkg.GetOutputDirectory;
          if CompareFilenames(Dir,OutputDir)=0 then exit;
          // check if output directory is a sour directory of a required package
          SrcPaths:=aPkg.SourceDirectories.CreateSearchPathFromAllFiles;
          if (SrcPaths<>'')
          and (FindPathInSearchPath(PChar(OutputDir),length(OutputDir),
                                    PChar(SrcPaths),length(SrcPaths))<>nil)
          then exit;
        end;
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
    Result:=true;
  end;

var
  SrcPaths: String;
begin
  Result:=false;
  if CompilerOptions.UnitOutputDirectory='' then exit;
  OutputDir:=CompilerOptions.ParsedOpts.GetParsedValue(pcosOutputDir,false);
  if OutputDir='' then exit;
  SrcPaths:=SourceDirectories.CreateSearchPathFromAllFiles;
  if SrcPaths='' then exit(true);
  if FindPathInSearchPath(PChar(OutputDir),length(OutputDir),PChar(SrcPaths),length(SrcPaths))<>nil
  then exit;
  // check used packages
  VisitedPackages:=TStringToStringTree.Create(false);
  try
    if not CheckDependency(FirstRequiredDependency) then exit;
  finally
    VisitedPackages.Free;
  end;
  Result:=true;
end;

function TLazPackage.GetStateFilename(UseOverride: boolean): string;
begin
  Result:=AppendPathDelim(GetOutputDirectory(UseOverride))+Name+'.compiled';
end;

function TLazPackage.GetSrcFilename: string;
begin
  if MainUnit<>nil then
    Result:=MainUnit.GetFullFilename
  else
    Result:=FDirectory+GetCompileSourceFilename;
end;

function TLazPackage.GetSrcPPUFilename: string;
begin
  Result:=AppendPathDelim(GetOutputDirectory)
          +ChangeFileExt(GetCompileSourceFilename,'.ppu');
end;

function TLazPackage.GetCompilerFilename: string;
begin
  Result:=CompilerOptions.ParsedOpts.GetParsedValue(pcosCompilerPath);
end;

function TLazPackage.GetPOOutDirectory: string;
begin
  Result:=TrimFilename(SubstitutePkgMacros(fPOOutputDirectory,false));
  LongenFilename(Result);
  IDEMacros.SubstituteMacros(Result);
  Result:=TrimFilename(Result);
end;

function TLazPackage.GetUnitPath(RelativeToBaseDir: boolean): string;
begin
  Result:=CompilerOptions.GetUnitPath(RelativeToBaseDir);
end;

function TLazPackage.GetIncludePath(RelativeToBaseDir: boolean): string;
begin
  Result:=CompilerOptions.GetIncludePath(RelativeToBaseDir);
end;

function TLazPackage.GetSrcPath(RelativeToBaseDir: boolean): string;
begin
  Result:=CompilerOptions.GetSrcPath(RelativeToBaseDir);
end;

function TLazPackage.GetLastCompilerParams(o: TPkgOutputDir): string;
begin
  Result:=LastCompile[o].Params;
  if LastCompile[o].ViaMakefile then begin
    Result:=StringReplace(Result,'$(CPU_TARGET)','$(TargetCPU)',[rfReplaceAll]);
    Result:=StringReplace(Result,'$(OS_TARGET)','$(TargetOS)',[rfReplaceAll]);
    Result:=StringReplace(Result,'$(LCL_PLATFORM)','$(LCLWidgetType)',[rfReplaceAll]);
    Result:=SubstitutePkgMacros(Result,false);
  end;
end;

function TLazPackage.NeedsDefineTemplates: boolean;
begin
  if IsVirtual or (lpfDestroying in Flags) or (Name='') then
    Result:=false
  else
    Result:=true;
end;

function TLazPackage.IndexOfPkgFile(PkgFile: TPkgFile): integer;
begin
  Result := FileCount - 1;
  if Result < 0 then
    Exit;
  while (Files[Result] <> PkgFile) do
  begin
    dec(Result);
    if Result < 0 then
      Exit;
  end;
end;

function TLazPackage.SearchShortFilename(const ShortFilename: string;
  SearchFlags: TSearchIDEFileFlags): TPkgFile;
var
  SearchedFilename: String;
  i: Integer;

  function FilenameFits(TheFilename: string): boolean;
  begin
    if siffIgnoreExtension in SearchFlags then
      TheFileName:=ExtractFileNameWithoutExt(TheFileName);
    //debugln('TLazPackage.SearchFile A ',SearchedFilename,' ',TheFilename);
    if siffCaseSensitive in SearchFlags then
      Result:=SearchedFilename=TheFilename
    else
      Result:=SysUtils.CompareText(SearchedFilename,TheFilename)=0;
  end;

begin
  SearchedFilename:=ShortFilename;
  if siffIgnoreExtension in SearchFlags then
    SearchedFilename:=ExtractFileNameWithoutExt(SearchedFilename);

  // search in files
  for i:=0 to FileCount-1 do begin
    Result:=Files[i];
    if FilenameFits(Result.GetShortFilename(true)) then exit;
  end;
  Result:=nil;
end;

function TLazPackage.SearchFilename(const AFilename: string;
  SearchFlags: TSearchIDEFileFlags): TPkgFile;
var
  SearchedFilename: String;
  i: Integer;

  function FilenameFits(TheFilename: string): boolean;
  begin
    if siffIgnoreExtension in SearchFlags then
      TheFileName:=ExtractFileNameWithoutExt(TheFileName);
    //debugln('TLazPackage.SearchFile A ',SearchedFilename,' ',TheFilename);
    if siffCaseSensitive in SearchFlags then
      Result:=SearchedFilename=TheFilename
    else
      Result:=SysUtils.CompareText(SearchedFilename,TheFilename)=0;
  end;

begin
  SearchedFilename:=AFilename;
  if siffIgnoreExtension in SearchFlags then
    SearchedFilename:=ExtractFileNameWithoutExt(SearchedFilename);

  // search in files
  for i:=0 to FileCount-1 do begin
    Result:=Files[i];
    if FilenameFits(Result.GetFullFilename) then exit;
  end;
  Result:=nil;
end;

{ TPkgComponent }

procedure TPkgComponent.SetPkgFile(const AValue: TPkgFile);
begin
  if FPkgFile=AValue then exit;
  if (FPkgFile<>nil) then PkgFile.RemovePkgComponent(Self);
  FPkgFile:=AValue;
  if (FPkgFile<>nil) then PkgFile.AddPkgComponent(Self);
end;

constructor TPkgComponent.Create(ThePkgFile: TPkgFile;
  TheComponentClass: TComponentClass; const ThePageName: string);
begin
  inherited Create(TheComponentClass,ThePageName);
  PkgFile:=ThePkgFile;
end;

destructor TPkgComponent.Destroy;
begin
  PkgFile:=nil;
  inherited Destroy;
end;

function TPkgComponent.GetUnitName: string;
var
  TIUnitName: String;
begin
  Result:=PkgFile.Unit_Name;
  // compare with RTTI unit name
  if ComponentClass<>nil then begin
    TIUnitName:=GetClassUnitName(ComponentClass);
    if SysUtils.CompareText(TIUnitName,Result)<>0 then
      Result:=TIUnitName;
  end;
end;

function TPkgComponent.GetPriority: TComponentPriority;
begin
  Result:=PkgFile.ComponentPriority;
end;

procedure TPkgComponent.ConsistencyCheck;
begin
  inherited ConsistencyCheck;
  if FPkgFile=nil then
    RaiseGDBException('TIDEComponent.ConsistencyCheck FPkgFile=nil');
  if FPkgFile.LazPackage=nil then
    RaiseGDBException('TIDEComponent.ConsistencyCheck FPkgFile.LazPackage=nil');
  if FPkgFile.LazPackage.IndexOfPkgComponent(Self)<0 then
    RaiseGDBException('TIDEComponent.ConsistencyCheck FPkgFile.LazPackage.IndexOfPkgComponent(Self)<0');
  if PkgFile.FComponents=nil then
    RaiseGDBException('TIDEComponent.ConsistencyCheck PkgFile.FComponents=nil');
  if PkgFile.FComponents.IndexOf(Self)<0 then
    RaiseGDBException('TIDEComponent.ConsistencyCheck PkgFile.FComponents.IndexOf(Self)<0');
end;

class function TPkgComponent.Images: TCustomImageList;
begin
  Result := IDEImages.Images_24;
end;

function TPkgComponent.HasIcon: boolean;
begin
  Result:=RealPage.PageName<>'';
end;

function TPkgComponent.ImageIndex: TImageIndex;
begin
  Result := IDEImages.GetImageIndex(ComponentClass.ClassName, 24);
  if Result=-1 then
    Result := IDEImages.GetImageIndex('default', 24);
end;

function TPkgComponent.CanBeCreatedInDesigner: boolean;
begin
  Result:=(not PkgFile.Removed);
end;

{ TPkgCompilerOptions }

procedure TPkgCompilerOptions.LoadFromXMLConfig(AXMLConfig: TXMLConfig;
  const Path: string);
begin
  inherited LoadFromXMLConfig(AXMLConfig,Path);

  FSkipCompiler := AXMLConfig.GetValue(Path+'SkipCompiler/Value', False);
end;

procedure TPkgCompilerOptions.SaveToXMLConfig(AXMLConfig: TXMLConfig; const Path: string);
begin
  inherited SaveToXMLConfig(AXMLConfig,Path);
  
  AXMLConfig.SetDeleteValue(Path+'SkipCompiler/Value', FSkipCompiler, False);
end;

procedure TPkgCompilerOptions.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
end;

procedure TPkgCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if CustomOptions=AValue then exit;
  InvalidateOptions;
  inherited SetCustomOptions(AValue);
  if LazPackage<>nil then
    LazPackage.DefineTemplates.CustomDefinesChanged;
end;

procedure TPkgCompilerOptions.SetIncludePaths(const AValue: string);
begin
  if IncludePath=AValue then exit;
  InvalidateOptions;
  inherited SetIncludePaths(AValue);
end;

procedure TPkgCompilerOptions.SetLibraryPaths(const AValue: string);
begin
  if Libraries=AValue then exit;
  InvalidateOptions;
  inherited SetLibraryPaths(AValue);
end;

procedure TPkgCompilerOptions.SetLinkerOptions(const AValue: string);
begin
  if LinkerOptions=AValue then exit;
  InvalidateOptions;
  inherited SetLinkerOptions(AValue);
end;

procedure TPkgCompilerOptions.SetObjectPath(const AValue: string);
begin
  if ObjectPath=AValue then exit;
  InvalidateOptions;
  inherited SetObjectPath(AValue);
end;

procedure TPkgCompilerOptions.SetSrcPath(const AValue: string);
begin
  if SrcPath=AValue then exit;
  InvalidateOptions;
  inherited SetSrcPath(AValue);
end;

procedure TPkgCompilerOptions.SetUnitPaths(const AValue: string);
begin
  if OtherUnitFiles=AValue then exit;
  InvalidateOptions;
  inherited SetUnitPaths(AValue);
end;

procedure TPkgCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if UnitOutputDirectory=AValue then exit;
  InvalidateOptions;
  inherited SetUnitOutputDir(AValue);
  if LazPackage<>nil then
    LazPackage.DefineTemplates.OutputDirectoryChanged;
end;

procedure TPkgCompilerOptions.SetConditionals(AValue: string);
begin
  AValue:=UTF8Trim(AValue,[]);
  if Conditionals=AValue then exit;
  InvalidateOptions;
  inherited SetConditionals(AValue);
end;

constructor TPkgCompilerOptions.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  if AOwner<>nil then
    FLazPackage := AOwner as TLazPackage;
  ParsedOpts.MacroValues.ProjValuesAvailable:=true;
end;

class function TPkgCompilerOptions.GetGroupCaption: string;
begin
  Result := dlgCompilerOptions;
end;

class function TPkgCompilerOptions.GetInstance: TAbstractIDEOptions;
begin
  if Package1<>nil then
    Result := Package1.CompilerOptions
  else
    Result := nil;
end;

function TPkgCompilerOptions.IsActive: boolean;
begin
  Result:=(LazPackage<>nil) and (LazPackage.CompilerOptions=Self);
end;

procedure TPkgCompilerOptions.Clear;
begin
  inherited Clear;
  FSkipCompiler:=false;
end;

procedure TPkgCompilerOptions.GetInheritedCompilerOptions(
  var OptionsList: TFPList);
begin
  if LazPackage<>nil then
    LazPackage.GetInheritedCompilerOptions(OptionsList);
end;

function TPkgCompilerOptions.GetOwnerName: string;
begin
  if LazPackage<>nil then
    Result:=LazPackage.IDAsString
  else
    Result:='';
end;

procedure TPkgCompilerOptions.InvalidateOptions;
begin
  if (LazPackage=nil) then exit;
  if LazPackage.UsageOptions=nil then RaiseGDBException('');
  if LazPackage.UsageOptions.ParsedOpts=nil then RaiseGDBException('');
  LazPackage.UsageOptions.ParsedOpts.InvalidateAll;
end;

function TPkgCompilerOptions.GetDefaultMainSourceFileName: string;
begin
  if LazPackage<>nil then
    Result:=LazPackage.GetCompileSourceFilename
  else
    Result:='';
  if Result='' then
    Result:=inherited GetDefaultMainSourceFileName;
end;

function TPkgCompilerOptions.CreateTargetFilename: string;
begin
  Result:='';
end;

function TPkgCompilerOptions.HasCompilerCommand: boolean;
begin
  Result:=(not SkipCompiler) and (CompilerPath<>'');
end;

procedure TPkgCompilerOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TPkgCompilerOptions
  then begin
    FSkipCompiler := TPkgCompilerOptions(Source).FSkipCompiler;
  end
  else begin
    FSkipCompiler := False;
  end;
end;

function TPkgCompilerOptions.CreateDiff(CompOpts: TBaseCompilerOptions;
  Tool: TCompilerDiffTool): boolean;
begin
  if (CompOpts is TPkgCompilerOptions) then begin
    Result:=Tool.AddDiff('SkipCompiler',FSkipCompiler,
                 TPkgCompilerOptions(CompOpts).FSkipCompiler);
  end else begin
    Result:=true;
    if Tool<>nil then Tool.Differ:=true;
  end;
  Result:=Result or inherited CreateDiff(CompOpts, Tool);
end;

{ TPkgAdditionalCompilerOptions }

procedure TPkgAdditionalCompilerOptions.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
end;

procedure TPkgAdditionalCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if AValue=CustomOptions then exit;
  inherited SetCustomOptions(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetIncludePath(const AValue: string);
begin
  if AValue=IncludePath then exit;
  inherited SetIncludePath(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetLibraryPath(const AValue: string);
begin
  if AValue=LibraryPath then exit;
  inherited SetLibraryPath(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetLinkerOptions(const AValue: string);
begin
  if AValue=LinkerOptions then exit;
  inherited SetLinkerOptions(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetObjectPath(const AValue: string);
begin
  if AValue=ObjectPath then exit;
  inherited SetObjectPath(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetUnitPath(const AValue: string);
begin
  if AValue=UnitPath then exit;
  inherited SetUnitPath(AValue);
  LazPackage.Modified:=true;
end;

procedure TPkgAdditionalCompilerOptions.SetSrcPath(const AValue: string);
begin
  if AValue=SrcPath then exit;
  inherited SetSrcPath(AValue);
  LazPackage.Modified:=true;
end;

constructor TPkgAdditionalCompilerOptions.Create(ThePackage: TLazPackage);
begin
  inherited Create(ThePackage);
  FLazPackage:=ThePackage;
end;

procedure TPkgAdditionalCompilerOptions.AssignOptions(Source: TObject);
begin
  inherited AssignOptions(Source);
  if Source is TPkgAdditionalCompilerOptions then begin
    //Src:=TPkgAdditionalCompilerOptions(Source);
    // nothing to do
  end;
end;

function TPkgAdditionalCompilerOptions.GetOwnerName: string;
begin
  Result:=LazPackage.IDAsString;
end;

function TPkgAdditionalCompilerOptions.
  GetBaseCompilerOptions: TBaseCompilerOptions;
begin
  Result:=LazPackage.CompilerOptions;
end;

{ TLazPackageDefineTemplates }

constructor TLazPackageDefineTemplates.Create(AOwner: IProjPack);
begin
  inherited Create(AOwner);
  Include(FFlags, ptfIsPackageTemplate);
  fLastSourceDirStamp:=CTInvalidChangeStamp;
end;

destructor TLazPackageDefineTemplates.Destroy;
begin
  inherited Destroy;
end;

procedure TLazPackageDefineTemplates.ClearFlags;
begin
  FFlags:=FFlags+[ptfIDChanged,ptfOutputDirChanged,ptfSourceDirsChanged,
                  ptfCustomDefinesChanged];
end;

procedure TLazPackageDefineTemplates.AllChanged;
begin
  IDChanged;
  UpdateSrcDirIfDef;// always create the SrcDirIfDef for IDE add-ons
  SourceDirectoriesChanged;
  CustomDefinesChanged;
  OutputDirectoryChanged;
end;

procedure TLazPackageDefineTemplates.UpdateMain;
begin
  if (not Owner.NeedsDefineTemplates) or (not Active) then exit;
  // update the package block define template (the container for all other
  // define templates of the package)
  if FMain=nil then begin
    FMain:=CreatePackageTemplateWithID(Owner.IDAsWord);
    FMain.SetDefineOwner(Owner as TLazPackage,false);
    FMain.SetFlags([dtfAutoGenerated],[],false);
  end else
    FMain.Name:=Owner.IDAsWord;
  // ClearCache is here unnessary, because it is only a block
end;

procedure TLazPackageDefineTemplates.UpdateSrcDirIfDef;
var
  Changed: Boolean;
  NewVariable: String;
  UnitPathDefTempl: TDefineTemplate;
  IncPathDefTempl: TDefineTemplate;
begin
  // create custom options
  // The custom options are enclosed by an IFDEF #PkgSrcMark<PckId> template.
  // Each source directory defines this variable, so that the settings can be
  // activated for each source directory by a simple DEFINE.
  if (FMain=nil) then UpdateMain;
  if FMain=nil then exit;
  if FSrcDirectories=nil then begin
    FSrcDirectories:=TDefineTemplate.Create('Source Directories',
      'Source Directories','','',
      da_Block);
    FMain.AddChild(FSrcDirectories);
  end;
  Changed:=false;
  if FSrcDirIf=nil then begin
    FSrcDirIf:=TDefineTemplate.Create('Source Directory Additions',
      'Additional defines for package source directories',
      '#PkgSrcMark'+Owner.IDAsWord, '', da_IfDef);
    FMain.AddChild(FSrcDirIf);

    // create unit path template for this directory
    UnitPathDefTempl:=TDefineTemplate.Create('UnitPath', lisPkgDefsUnitPath,
      '#UnitPath','$(#UnitPath);$PkgUnitPath('+Owner.IDAsString+')',
      da_Define);
    FSrcDirIf.AddChild(UnitPathDefTempl);
    // create include path template for this directory
    IncPathDefTempl:=TDefineTemplate.Create('IncPath','Include Path',
      '#IncPath','$(#IncPath);$PkgIncPath('+Owner.IDAsString+')',
      da_Define);
    FSrcDirIf.AddChild(IncPathDefTempl);

    Changed:=true;
  end else begin
    NewVariable:='#PkgSrcMark'+Owner.IDAsWord;
    if NewVariable<>FSrcDirIf.Variable then begin
      FSrcDirIf.Variable:=NewVariable;
      // unit path
      UnitPathDefTempl:=FSrcDirIf.FindChildByName('UnitPath');
      if UnitPathDefTempl<>nil then
        UnitPathDefTempl.Value:='$(#UnitPath);$PkgUnitPath('+Owner.IDAsString+')';
      // include path
      IncPathDefTempl:=FSrcDirIf.FindChildByName('IncPath');
      if IncPathDefTempl<>nil then
        IncPathDefTempl.Value:='$(#IncPath);$PkgIncPath('+Owner.IDAsString+')';

      Changed:=true;
    end;
  end;
  if Changed then
    CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.UpdateOutputDirectory;
var
  LazPackage: TLazPackage;
begin
  if FMain=nil then UpdateMain;
  if FMain=nil then exit;

  LazPackage := Owner as TLazPackage;
  if FOutputDir=nil then begin
    FOutputDir:=TDefineTemplate.Create(PkgOutputDirDefTemplName,
      lisPkgDefsOutputDirectory, '', LazPackage.GetOutputDirectory, da_Directory);
    FOutputDir.SetDefineOwner(LazPackage,false);
    FOutputDir.SetFlags([dtfAutoGenerated],[],false);
    DisableDefaultsInDirectories(FOutputDir,false);
    FMain.AddChild(FOutputDir);
  end else begin
    FOutputDir.Value:=LazPackage.GetOutputDirectory;
  end;

  if (FOutPutSrcPath=nil)
  or (fLastOutputDirSrcPathIDAsString<>Owner.IDAsString) then begin
    fLastOutputDirSrcPathIDAsString:=Owner.IDAsString;
    FOutputSrcPath:=TDefineTemplate.Create('CompiledSrcPath',
      lisPkgDefsCompiledSrcPathAddition, CompiledSrcPathMacroName,
      '$PkgSrcPath('+fLastOutputDirSrcPathIDAsString+');'+'$('+CompiledSrcPathMacroName+')',
      da_Define);
    FOutputSrcPath.SetDefineOwner(LazPackage,false);
    FOutputSrcPath.SetFlags([dtfAutoGenerated],[],false);
    CodeToolBoss.DefineTree.ReplaceChild(FOutputDir,FOutputSrcPath,
      FOutputSrcPath.Name);
  end;
end;

procedure TLazPackageDefineTemplates.UpdateSourceDirectories;
var
  i: Integer;
  SrcDirDefTempl, SrcDirMarkDefTempl: TDefineTemplate;
  IDHasChanged: Boolean;
  CurUnitPath, SrcDirs: String;
begin
  if (not Owner.NeedsDefineTemplates) or (not Active) then exit;

  // quick check if something has changed
  IDHasChanged:=fLastSourceDirsIDAsString<>Owner.IDAsString;
  CurUnitPath:=Owner.BaseCompilerOptions.ParsedOpts.GetParsedValue(pcosUnitPath);
  SrcDirs:=Owner.SourceDirectories.CreateSearchPathFromAllFiles;
  CurUnitPath:=TrimSearchPath(SrcDirs+';'+CurUnitPath+';.',
                              Owner.BaseCompilerOptions.BaseDirectory,true);

  if (fLastSourceDirectories<>nil)
  and (fLastSourceDirStamp=Owner.SourceDirectories.TimeStamp)
  and (not IDHasChanged)
  and (CurUnitPath=fLastUnitPath) then
    exit;
  //debugln(['TLazPackageDefineTemplates.UpdateSourceDirectories ',LazPackage.Name,' CurUnitPath=',CurUnitPath]);
  fLastSourceDirStamp:=Owner.SourceDirectories.TimeStamp;
  fLastSourceDirsIDAsString:=Owner.IDAsString;
  fLastUnitPath:=CurUnitPath;

  // clear old define templates
  if fLastSourceDirectories<>nil then begin
    for i:=0 to fLastSourceDirectories.Count-1 do begin
      SrcDirDefTempl:=TDefineTemplate(fLastSourceDirectories.Objects[i]);
      SrcDirDefTempl.Unbind;
      SrcDirDefTempl.Free;
    end;
    fLastSourceDirectories.Clear;
  end else
    fLastSourceDirectories:=TStringList.Create;

  // build source directory define templates
  FreeAndNil(fLastSourceDirectories);
  fLastSourceDirectories:=SearchPathToList(CurUnitPath);
  if (fLastSourceDirectories.Count>0)
  and ((FSrcDirIf=nil) or IDHasChanged) then
    UpdateSrcDirIfDef;
  for i:=0 to fLastSourceDirectories.Count-1 do begin
    // create directory template
    SrcDirDefTempl:=TDefineTemplate.Create('Source Directory '+IntToStr(i+1),
      fLastSourceDirectories[i],'',fLastSourceDirectories[i],da_Directory);
    DisableDefaultsInDirectories(SrcDirDefTempl,false);
    fLastSourceDirectories.Objects[i]:=SrcDirDefTempl;
    // add package source directory marker
    SrcDirMarkDefTempl:=TDefineTemplate.Create('PkgSrcDirMark',
      lisPkgDefsSrcDirMark,'#PkgSrcMark'+Owner.IDAsWord,'',da_Define);
    SrcDirDefTempl.AddChild(SrcDirMarkDefTempl);

    SrcDirDefTempl.SetDefineOwner(Owner as TLazPackage,false);
    SrcDirDefTempl.SetFlags([dtfAutoGenerated],[],false);
    // add directory
    FSrcDirectories.AddChild(SrcDirDefTempl);
  end;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.UpdateDefinesForCustomDefines;
var
  OptionsDefTempl: TDefineTemplate;
  NewCustomOptions: String;
begin
  if (not Owner.NeedsDefineTemplates) or (not Active) then exit;

  // check if something has changed
  NewCustomOptions:=Owner.BaseCompilerOptions.GetOptionsForCTDefines;
  if FLastCustomOptions=NewCustomOptions then exit;

  FLastCustomOptions:=NewCustomOptions;
  OptionsDefTempl:=CodeToolBoss.DefinePool.CreateFPCCommandLineDefines(
              'Custom Options', FLastCustomOptions, false, Owner as TLazPackage);
  if OptionsDefTempl=nil then begin
    // no custom options -> delete old template
    if FSrcDirIf<>nil then begin
      if FSrcDirIf.DeleteChild('Custom Options') then
        CodeToolBoss.DefineTree.ClearCache;
    end;
  end else begin
    UpdateSrcDirIfDef;
    FSrcDirIf.ReplaceChild(OptionsDefTempl);
    CodeToolBoss.DefineTree.ClearCache;
  end;
end;

{ TBasePackageEditor }

function TBasePackageEditor.GetLazPackage: TLazPackage;
begin
  Result:=nil;
end;

{ TPublishPackageOptions }

procedure TPublishPackageOptions.DoOnModifyChange;
begin
  if Modified then
    TLazPackage(Owner).Modified:=true;
end;

function TPublishPackageOptions.GetDefaultDestinationDir: string;
begin
  Result:='$(TestDir)/publishedpackage/';
end;

{ TPkgPairTree }

function ComparePkgPairs(Pair1, Pair2: Pointer): integer;
begin
  Result:=TPkgPair(Pair1).Compare(TPkgPair(Pair2));
end;

constructor TPkgPairTree.Create;
begin
  inherited Create(@ComparePkgPairs);
end;

destructor TPkgPairTree.Destroy;
begin
  FreeAndClear;
  inherited Destroy;
end;

function TPkgPairTree.FindPair(Pkg1, Pkg2: TLazPackage; IgnoreOrder: boolean): TPkgPair;
var
  Comp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=Root;
  while (ANode<>nil) do begin
    Result:=TPkgPair(ANode.Data);
    Comp:=Result.ComparePair(Pkg1,Pkg2);
    if Comp=0 then exit;
    if Comp>0 then begin
      ANode:=ANode.Left
    end else begin
      ANode:=ANode.Right
    end;
  end;
  if IgnoreOrder and (Pkg1<>Pkg2) then
    Result:=FindPair(Pkg2,Pkg1,false)
  else
    Result:=nil;
end;

function TPkgPairTree.AddPair(Pkg1, Pkg2: TLazPackage): TPkgPair;
begin
  Result:=TPkgPair.Create(Pkg1,Pkg2);
  Add(Result);
end;

function TPkgPairTree.AddPairIfNotExists(Pkg1, Pkg2: TLazPackage): TPkgPair;
begin
  Result:=FindPair(Pkg1,Pkg2,true);
  if Result=nil then
    Result:=AddPair(Pkg1,Pkg2);
end;

{ TPkgPair }

constructor TPkgPair.Create(Pkg1, Pkg2: TLazPackage);
begin
  Package1:=Pkg1;
  Package2:=Pkg2;
end;

function TPkgPair.ComparePair(Pkg1, Pkg2: TLazPackage): integer;
begin
  Result:=Package1.Compare(Pkg1);
  if Result=0 then
    Result:=Package2.Compare(Pkg2);
end;

function TPkgPair.Compare(PkgPair: TPkgPair): integer;
begin
  Result:=ComparePair(PkgPair.Package1,PkgPair.Package2);
end;

function TPkgPair.AsString: string;
begin
  Result:=Package1.IDAsString+' - '+Package2.IDAsString;
end;

{ TPkgUnitsTree }

function TPkgUnitsTree.FindNodeWithUnitName(const AUnitName: string): TAVLTreeNode;
var
  Comp: integer;
  PkgFile: TPkgFile;
begin
  Result:=Root;
  while (Result<>nil) do begin
    PkgFile:=TPkgFile(Result.Data);
    Comp:=SysUtils.CompareText(AUnitName,PkgFile.Unit_Name);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TPkgUnitsTree.FindPkgFileWithUnitName(const AUnitName: string): TPkgFile;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FindNodeWithUnitName(AUnitName);
  if ANode=nil then
    Result:=nil
  else
    Result:=TPkgFile(ANode.Data);
end;

function ComparePkgFilesUnitname(PkgFile1, PkgFile2: Pointer): integer;
begin
  Result := SysUtils.CompareText(
              TPkgFile(PkgFile1).Unit_Name,
              TPkgFile(PkgFile2).Unit_Name);
end;

constructor TPkgUnitsTree.Create(ThePackage: TLazPackage);
begin
  fLazPackage:=ThePackage;
  inherited Create(@ComparePkgFilesUnitname);
end;

initialization
  RegisterIDEOptionsGroup(GroupPackage, TPackageIDEOptions);
  RegisterIDEOptionsGroup(GroupPkgCompiler, TPkgCompilerOptions);
  PackageDependencies:=TAVLTree.Create(@ComparePkgDependencyNames);

finalization
  FreeThenNil(PackageDependencies);

end.

