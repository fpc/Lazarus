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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  What is a package:
  A lazarus package is a collection of units and components, containing
  information how they can be compiled and how they can be used by projects or
  other packages or the IDE. In contrary to Delphi, packages are not limited
  to libraries and they can be OS independent.
  (Delphi: a package is a specially compiled library used by applications,
  the IDE or both. Delphi packages require compiler magic, which fpc is not
  capable of at the moment and of course this magic is not OS independent.)

}
unit PackageDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, typinfo, LCLProc, LCLType, LResources, Graphics,
  Forms, FileProcs, FileUtil, AVL_Tree, LazConfigStorage, Laz_XMLCfg,
  BasicCodeTools, CodeToolsCfgScript, DefineTemplates, CodeToolManager,
  CodeCache, CodeToolsStructs,
  PropEdits, LazIDEIntf, MacroIntf, PackageIntf, IDEOptionsIntf,
  EditDefineTree, CompilerOptions, CompOptsModes, IDEOptionDefs, 
  LazarusIDEStrConsts, IDEProcs, ComponentReg,
  TransferMacros, FileReferenceList, PublishModule;

type
  TLazPackage = class;
  TPkgFile = class;
  TBasePackageEditor = class;
  TPkgDependency = class;

  TIteratePackagesEvent =
    procedure(APackage: TLazPackageID) of object;
  TGetAllRequiredPackagesEvent =
    procedure(FirstDependency: TPkgDependency; out List: TFPList) of object;
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
    FIcon: TCustomBitmap;
    FIconLoaded: boolean;
    procedure SetPkgFile(const AValue: TPkgFile);
  public
    constructor Create(ThePkgFile: TPkgFile; TheComponentClass: TComponentClass;
                       const ThePageName: string);
    destructor Destroy; override;
    function GetUnitName: string; override;
    function GetPriority: TComponentPriority; override;
    procedure ConsistencyCheck; override;
    function Icon: TCustomBitmap;
    function GetIconCopy: TCustomBitmap;
    function HasIcon: boolean;
    function CanBeCreatedInDesigner: boolean; override;
  public
    property PkgFile: TPkgFile read FPkgFile write SetPkgFile;
  end;

  { TPkgFile }

  TPkgFileType = (
    pftUnit,    // file is pascal unit
    pftVirtualUnit,// file is virtual pascal unit
    pftMainUnit, // file is the auto created main pascal unit
    pftLFM,     // lazarus form text file
    pftLRS,     // lazarus resource file
    pftInclude, // include file
    pftIssues,  // file is issues xml file
    pftText,    // file is text (e.g. copyright or install notes)
    pftBinary   // file is something else
    );
  TPkgFileTypes = set of TPkgFileType;
  
const
  PkgFileUnitTypes = [pftUnit,pftVirtualUnit,pftMainUnit];
  PkgFileRealUnitTypes = [pftUnit,pftMainUnit];
  
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
    FFileType: TPkgFileType;
    FFlags: TPkgFileFlags;
    fFullFilename: string;
    fFullFilenameStamp: integer;
    FPackage: TLazPackage;
    FResourceBaseClass: TPFComponentBaseClass;
    FSourceDirectoryReferenced: boolean;
    FSourceDirNeedReference: boolean;
    FUnitName: string;
    function GetAddToUsesPkgSection: boolean;
    function GetComponents(Index: integer): TPkgComponent;
    function GetHasRegisterProc: boolean;
    procedure SetAddToUsesPkgSection(const AValue: boolean);
    procedure SetAutoReferenceSourceDir(const AValue: boolean);
    procedure SetFileType(const AValue: TPkgFileType);
    procedure SetFlags(const AValue: TPkgFileFlags);
    procedure SetHasRegisterProc(const AValue: boolean);
    procedure UpdateUnitName;
    function GetComponentList: TFPList;
  protected
    function GetIDEPackage: TIDEPackage; override;
    procedure SetFilename(const AValue: string); override;
    procedure SetRemoved(const AValue: boolean); override;
  public
    constructor Create(ThePackage: TLazPackage);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      FileVersion: integer; AdjustPathDelims: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch);
    procedure ConsistencyCheck;
    function IsVirtual: boolean;
    function GetShortFilename(UseUp: boolean): string; override;
    function ComponentCount: integer;
    procedure AddPkgComponent(APkgComponent: TPkgComponent);
    procedure RemovePkgComponent(APkgComponent: TPkgComponent);
    function HasRegisteredPlugins: boolean;
    function MakeSense: boolean;
    procedure UpdateSourceDirectoryReference;
    function GetFullFilename: string; override;
    function GetResolvedFilename: string; // GetFullFilename + ReadAllLinks
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
    property FileType: TPkgFileType read FFileType write SetFileType;
    property Flags: TPkgFileFlags read FFlags write SetFlags;
    property HasRegisterProc: boolean
                               read GetHasRegisterProc write SetHasRegisterProc;
    property LazPackage: TLazPackage read FPackage;
    property SourceDirectoryReferenced: boolean read FSourceDirectoryReferenced;
    property Unit_Name: string read FUnitName write FUnitName;
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
  
  TPkgDependencyFlag = (
    pdfMinVersion, // >= MinVersion
    pdfMaxVersion // <= MaxVersion
    );
  TPkgDependencyFlags = set of TPkgDependencyFlag;
  
  TPkgMarkerFlag = (
    pmfVisited,
    pmfMarked
    );
  TPkgMarkerFlags = set of TPkgMarkerFlag;
  
  TLoadPackageResult = (
    lprUndefined,
    lprSuccess,
    lprNotFound,
    lprLoadError
    );

  TPkgDependencyList = (
    pdlRequires,
    pdlUsedBy
    );
  
  { TPkgDependency }

  TPkgDependency = class
  private
    FDefaultFilename: string;
    FFlags: TPkgDependencyFlags;
    FHoldPackage: boolean;
    FLoadPackageResult: TLoadPackageResult;
    FMarkerFlags: TPKgMarkerFlags;
    FOwner: TObject;
    FMaxVersion: TPkgVersion;
    FMinVersion: TPkgVersion;
    FPackageName: string;
    FPreferDefaultFilename: boolean;
    FRemoved: boolean;
    FRequiredPackage: TLazPackage;
    procedure SetFlags(const AValue: TPkgDependencyFlags);
    procedure SetHoldPackage(const AValue: boolean);
    procedure SetLoadPackageResult(const AValue: TLoadPackageResult);
    procedure SetMaxVersion(const AValue: TPkgVersion);
    procedure SetMinVersion(const AValue: TPkgVersion);
    procedure SetPackageName(const AValue: string);
    procedure SetRemoved(const AValue: boolean);
    procedure SetRequiredPackage(const AValue: TLazPackage);
  public
    NextDependency, PrevDependency: array[TPkgDependencyList] of TPkgDependency;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                FileVersion: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch);
    function MakeSense: boolean;
    function IsCompatible(const Version: TPkgVersion): boolean;
    function IsCompatible(const PkgName: string;
      const Version: TPkgVersion): boolean;
    function Compare(Dependency2: TPkgDependency): integer;
    procedure Assign(Source: TPkgDependency);
    procedure Assign(Source: TLazPackageID);
    procedure ConsistencyCheck;
    function IsCompatible(Pkg: TLazPackageID): boolean;
    procedure MakeCompatible(const PkgName: string; const Version: TPkgVersion);
    function AsString(WithOwner: boolean = false): string;
    function NextUsedByDependency: TPkgDependency;
    function PrevUsedByDependency: TPkgDependency;
    function NextRequiresDependency: TPkgDependency;
    function PrevRequiresDependency: TPkgDependency;
    procedure AddToList(var FirstDependency: TPkgDependency;
      ListType: TPkgDependencyList);
    procedure AddToEndOfList(var LastDependency: TPkgDependency;
      ListType: TPkgDependencyList);
    procedure RemoveFromList(var FirstDependency: TPkgDependency;
      ListType: TPkgDependencyList);
    procedure MoveUpInList(var FirstDependency: TPkgDependency;
      ListType: TPkgDependencyList);
    procedure MoveDownInList(var FirstDependency: TPkgDependency;
      ListType: TPkgDependencyList);
    function MakeFilenameRelativeToOwner(const AFilename: string): string;
    function FindDefaultFilename: string;
  public
    property PackageName: string read FPackageName write SetPackageName;
    property Flags: TPkgDependencyFlags read FFlags write SetFlags;
    property MinVersion: TPkgVersion read FMinVersion write SetMinVersion;
    property MaxVersion: TPkgVersion read FMaxVersion write SetMaxVersion;
    property Removed: boolean read FRemoved write SetRemoved;
    property Owner: TObject read FOwner write FOwner;// package or project or IDE
    property RequiredPackage: TLazPackage read FRequiredPackage write SetRequiredPackage;
    property LoadPackageResult: TLoadPackageResult read FLoadPackageResult write SetLoadPackageResult;
    property HoldPackage: boolean read FHoldPackage write SetHoldPackage;
    property MarkerFlags: TPKgMarkerFlags read FMarkerFlags write FMarkerFlags;
    property DefaultFilename: string read FDefaultFilename write FDefaultFilename;
    property PreferDefaultFilename: boolean read FPreferDefaultFilename write FPreferDefaultFilename;
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
    procedure SetConditionals(const AValue: string); override;
  public
    constructor Create(const AOwner: TObject); override;
    // IDE options
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function IsActive: boolean; override;
    procedure Clear; override;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList); override;
    function GetOwnerName: string; override;
    procedure InvalidateOptions;
    function GetDefaultMainSourceFileName: string; override;
    function CreateTargetFilename(const MainSourceFileName: string): string; override;

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
  private
    FLazPackage: TLazPackage;
  protected
    procedure DoOnModifyChange; override;
  public
    constructor Create(TheLazPackage: TLazPackage);
    function GetDefaultDestinationDir: string; override;
    property LazPackage: TLazPackage read FLazPackage;
  end;
  
  
  { TLazPackageDefineTemplates }
  
  TLazPkgDefineTemplatesFlag = (
    pdtIDChanged,
    pdtSourceDirsChanged,
    pdtOutputDirChanged,
    pdtCustomDefinesChanged
    );
  TLazPkgDefineTemplatesFlags = set of TLazPkgDefineTemplatesFlag;
  
  TLazPackageDefineTemplates = class
  private
    FActive: boolean;
    FSrcDirIfDef: TDefineTemplate;
    FFlags: TLazPkgDefineTemplatesFlags;
    fLastOutputDirSrcPathIDAsString: string;
    fLastSourceDirectories: TStringList;
    fLastSourceDirStamp: integer;
    fLastSourceDirsIDAsString: string;
    FLastCustomOptions: string;
    fLastUnitPath: string;
    FLazPackage: TLazPackage;
    FMain: TDefineTemplate;
    FOutputDir: TDefineTemplate;
    FOutPutSrcPath: TDefineTemplate;
    FSrcDirectories: TDefineTemplate;
    FUpdateLock: integer;
    procedure SetActive(const AValue: boolean);
    procedure UpdateMain;
    procedure UpdateSrcDirIfDef;
    procedure UpdateOutputDirectory;
    procedure UpdateSourceDirectories;
    procedure UpdateDefinesForCustomDefines;
  public
    constructor Create(OwnerPackage: TLazPackage);
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure PackageIDChanged;
    procedure SourceDirectoriesChanged;// a source directory was added/deleted
    procedure OutputDirectoryChanged;// the path or the defines of the output dir changed
    procedure CustomDefinesChanged;// the defines of the source dirs changed
    procedure AllChanged;
  public
    property LazPackage: TLazPackage read FLazPackage;
    property Main: TDefineTemplate read FMain;
    property SrcDirectories: TDefineTemplate read FSrcDirectories;
    property OutputDir: TDefineTemplate read FOutputDir;
    property OutPutSrcPath: TDefineTemplate read FOutPutSrcPath;
    property CustomDefines: TDefineTemplate read FSrcDirIfDef;
    property Active: boolean read FActive write SetActive;
  end;
  

  { TLazPackage }
  
  TLazPackageType = (
    lptRunTime,         // RunTime packages can't register anything in the IDE.
    lptDesignTime,      // DesignTime packages can register anything in the IDE
                        // and should not be compiled into projects.
                        // The IDE calls the 'register' procedures of each unit.
    lptRunAndDesignTime // RunAndDesignTime packages can do anything.
    );
    
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
    lpfCycle           // Used by the PackageGraph to mark cycles
    );
  TLazPackageFlags = set of TLazPackageFlag;
  
  TPackageInstallType = (
    pitNope,
    pitStatic,
    pitDynamic
    );
    
  TPackageUpdatePolicy = (
    pupManually,
    pupOnRebuildingAll,
    pupAsNeeded
    );
  TPackageUpdatePolicies = set of TPackageUpdatePolicy;

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

  TIterateComponentClassesEvent =
    procedure(PkgComponent: TPkgComponent) of object;
  TPkgChangeNameEvent = procedure(Pkg: TLazPackage;
                                  const OldName: string) of object;

  { TLazPackage }

  TLazPackage = class(TIDEPackage)
  private
    FAddToProjectUsesSection: boolean;
    FAuthor: string;
    FAutoCreated: boolean;
    FAutoInstall: TPackageInstallType;
    FAutoUpdate: TPackageUpdatePolicy;
    FOptionsBackup: TLazPackage;
    FCompilerOptions: TPkgCompilerOptions;
    FComponents: TFPList; // TFPList of TPkgComponent
    FDefineTemplates: TLazPackageDefineTemplates;
    FDescription: string;
    FDirectory: string;
    FDirectoryExpanded: string;
    FDirectoryExpandedChangeStamp: integer;
    FEnableI18N: boolean;
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
    FLazDocPaths: string;
    FLicense: string;
    FLPKSource: TCodeBuffer;
    FLPKSourceChangeStep: integer;
    FMacros: TTransferMacroList;
    FMainUnit: TPkgFile;
    FMissing: boolean;
    FModifiedLock: integer;
    FOutputStateFile: string;
    FPackageEditor: TBasePackageEditor;
    FPackageType: TLazPackageType;
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
    function GetComponentCount: integer;
    function GetComponents(Index: integer): TPkgComponent;
    function GetRemovedFiles(Index: integer): TPkgFile;
    function GetFiles(Index: integer): TPkgFile;
    procedure SetAddToProjectUsesSection(const AValue: boolean);
    procedure SetAuthor(const AValue: string);
    procedure SetAutoCreated(const AValue: boolean);
    procedure SetAutoIncrementVersionOnBuild(const AValue: boolean);
    procedure SetAutoInstall(const AValue: TPackageInstallType);
    procedure SetAutoUpdate(const AValue: TPackageUpdatePolicy);
    procedure SetDescription(const AValue: string);
    procedure SetFileReadOnly(const AValue: boolean);
    procedure SetFlags(const AValue: TLazPackageFlags);
    procedure SetIconFile(const AValue: string);
    procedure SetInstalled(const AValue: TPackageInstallType);
    procedure SetLazDocPaths(const AValue: string);
    procedure SetLicense(const AValue: string);
    procedure SetLPKSource(const AValue: TCodeBuffer);
    procedure SetLPKSourceChangeStep(const AValue: integer);
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
    function GetFileCount: integer; override;
    function GetPkgFiles(Index: integer): TLazPackageFile; override;
    function GetDirectoryExpanded: string; override;
    function GetModified: boolean; override;
    procedure SetFilename(const AValue: string); override;
    procedure SetModified(const AValue: boolean); override;
    procedure SetName(const AValue: string); override;
    procedure VersionChanged(Sender: TObject); override;
    function GetRemovedCount: integer; override;
    function GetRemovedPkgFiles(Index: integer): TLazPackageFile; override;
  public
    procedure AssignOptions(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    // IDE options
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure BackupOptions;
    procedure RestoreOptions;
    // modified
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LockModified;
    procedure UnlockModified;
    function ReadOnly: boolean; override;
    // streaming
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToString(out s: string);
    // consistency
    procedure CheckInnerDependencies;
    function MakeSense: boolean;
    procedure ConsistencyCheck;
    // paths, define templates
    function IsVirtual: boolean; override;
    function HasDirectory: boolean;
    function HasStaticDirectory: boolean;
    function GetFullFilename(ResolveMacros: boolean): string;
    function GetResolvedFilename(ResolveMacros: boolean): string; // GetFullFilename + ReadAllLinks
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
    function GetLastCompilerParams(o: TPkgOutputDir): string;
    function NeedsDefineTemplates: boolean;
    function SubstitutePkgMacros(const s: string;
                                 PlatformIndependent: boolean): string;
    procedure WriteInheritedUnparsedOptions;
    // files
    function IndexOfPkgFile(PkgFile: TPkgFile): integer;
    function SearchShortFilename(const ShortFilename: string;
                            SearchFlags: TSearchIDEFileFlags): TPkgFile;
    function SearchFilename(const AFilename: string;
                            SearchFlags: TSearchIDEFileFlags): TPkgFile;
    procedure ShortenFilename(var ExpandedFilename: string; UseUp: boolean);
    procedure LongenFilename(var AFilename: string);
    function FindPkgFile(const AFilename: string;
                         IgnoreRemoved, FindNewFile: boolean): TPkgFile;
    function FindUnitWithRegister(IgnorePkgFile: TPkgFile = nil): TPkgFile;
    function FindUnit(const TheUnitName: string): TPkgFile;
    function FindUnit(const TheUnitName: string; IgnoreRemoved: boolean): TPkgFile;
    function FindUnit(const TheUnitName: string; IgnoreRemoved: boolean;
                      IgnorePkgFile: TPkgFile): TPkgFile;
    function FindRemovedPkgFile(const AFilename: string): TPkgFile;
    function AddFile(const NewFilename, NewUnitName: string;
                     NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
                     CompPriorityCat: TComponentPriorityCategory): TPkgFile;
    function AddRemovedFile(const NewFilename, NewUnitName: string;
                     NewFileType: TPkgFileType; NewFlags: TPkgFileFlags;
                     CompPriorityCat: TComponentPriorityCategory): TPkgFile;
    procedure RemoveFile(PkgFile: TPkgFile); // move file to removed file list
    procedure UnremovePkgFile(PkgFile: TPkgFile); // move file back to file list
    function RemoveNonExistingFiles: boolean; // true if something changed
    function GetFileDialogInitialDir(const DefaultDirectory: string): string;
    procedure MoveFile(CurIndex, NewIndex: integer);
    procedure SortFiles;
    function FixFilesCaseSensitivity: boolean;
    function MainUnitHasPkgName: boolean;
    // required dependencies (plus removed required dependencies)
    function FindDependencyByName(const PkgName: string): TPkgDependency;
    function RequiredDepByIndex(Index: integer): TPkgDependency;
    function RemovedDepByIndex(Index: integer): TPkgDependency;
    procedure AddRequiredDependency(Dependency: TPkgDependency);
    procedure AddPackageDependency(const PkgName: string);
    procedure RemoveRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRemovedDependency(Dependency: TPkgDependency);
    procedure RemoveRemovedDependency(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyUp(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyDown(Dependency: TPkgDependency);
    function CreateDependencyWithOwner(NewOwner: TObject;
                               WithMinVersion: boolean = false): TPkgDependency;
    function Requires(APackage: TLazPackage): boolean;
    procedure GetAllRequiredPackages(var List: TFPList; WithSelf: boolean);
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
    procedure AddUsedByDependency(Dependency: TPkgDependency);
    procedure RemoveUsedByDependency(Dependency: TPkgDependency);
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
    property AutoCreated: boolean read FAutoCreated write SetAutoCreated; // do not save
    property AutoIncrementVersionOnBuild: boolean
                                           read GetAutoIncrementVersionOnBuild
                                           write SetAutoIncrementVersionOnBuild;
    property AutoInstall: TPackageInstallType read FAutoInstall
                                              write SetAutoInstall;
    property AutoUpdate: TPackageUpdatePolicy read FAutoUpdate
                                              write SetAutoUpdate;
    property CompilerOptions: TPkgCompilerOptions read FCompilerOptions;
    property ComponentCount: integer read GetComponentCount;
    property Components[Index: integer]: TPkgComponent read GetComponents;
    property DefineTemplates: TLazPackageDefineTemplates read FDefineTemplates
                                                         write FDefineTemplates;
    property Description: string read FDescription write SetDescription;
    property Directory: string read FDirectory; // the directory of the .lpk file with macros
    property Editor: TBasePackageEditor read FPackageEditor
                                        write SetPackageEditor;
    property EnableI18N: Boolean read FEnableI18N write SetEnableI18N;
    property FileReadOnly: boolean read FFileReadOnly write SetFileReadOnly;
    property Files[Index: integer]: TPkgFile read GetFiles;
    property FirstRemovedDependency: TPkgDependency
                                                   read FFirstRemovedDependency;
    property FirstRequiredDependency: TPkgDependency
                                                  read FFirstRequiredDependency;
    property FirstUsedByDependency: TPkgDependency read FFirstUsedByDependency;
    property Flags: TLazPackageFlags read FFlags write SetFlags;
    property HoldPackageCount: integer read FHoldPackageCount;
    property IconFile: string read FIconFile write SetIconFile;
    property Installed: TPackageInstallType read FInstalled write SetInstalled;
    property LazDocPaths: string read FLazDocPaths write SetLazDocPaths;
    property License: string read FLicense write SetLicense;
    property LPKSource: TCodeBuffer read FLPKSource write SetLPKSource;// can be nil when file on disk was removed
    property LPKSourceChangeStep: integer read FLPKSourceChangeStep write SetLPKSourceChangeStep;
    property Macros: TTransferMacroList read FMacros;
    property MainUnit: TPkgFile read FMainUnit;
    property Missing: boolean read FMissing write FMissing;
    property OptionsBackup: TLazPackage read FOptionsBackup;
    property OutputStateFile: string read FOutputStateFile write SetOutputStateFile;
    property PackageType: TLazPackageType read FPackageType
                                          write SetPackageType;
    property POOutputDirectory: string read FPOOutputDirectory
                                       write SetPOOutputDirectory;
    property Provides: TStrings read FProvides write SetProvides;
    property PublishOptions: TPublishPackageOptions
                                     read fPublishOptions write fPublishOptions;
    property Registered: boolean read FRegistered write SetRegistered;
    property RemovedFiles[Index: integer]: TPkgFile read GetRemovedFiles;
    property SourceDirectories: TFileReferenceList read FSourceDirectories;
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write SetStorePathDelim;
    property TopologicalLevel: integer read FTopologicalLevel write FTopologicalLevel;
    property Translated: string read FTranslated write FTranslated;
    property UsageOptions: TPkgAdditionalCompilerOptions read FUsageOptions;
    property UserReadOnly: boolean read FUserReadOnly write SetUserReadOnly;
    property UserIgnoreChangeStamp: integer read FUserIgnoreChangeStamp
                                            write FUserIgnoreChangeStamp;
  end;
  
  PLazPackage = ^TLazPackage;
  
  
  { TBasePackageEditor }
  
  TBasePackageEditor = class(TForm)
  protected
    function GetLazPackage: TLazPackage; virtual;
    procedure SetLazPackage(const AValue: TLazPackage); virtual; abstract;
  public
    procedure UpdateAll(Immediately: boolean); virtual; abstract;
    property LazPackage: TLazPackage read GetLazPackage write SetLazPackage;
  end;
  

const
  LazPkgXMLFileVersion = 4;
  
  PkgFileTypeIdents: array[TPkgFileType] of string = (
    'Unit', 'Virtual Unit', 'Main Unit',
    'LFM', 'LRS', 'Include', 'Issues', 'Text', 'Binary');
  LazPackageTypeIdents: array[TLazPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime');
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
function CompareNameWithPkgDependency(Key, Data: Pointer): integer;
function ComparePkgDependencyNames(Data1, Data2: Pointer): integer;
function CompareUnitsTree(UnitTree1, UnitTree2: TPkgUnitsTree): integer;
function ComparePackageWithUnitsTree(Package: TLazPackage;
                                     UnitTree: TPkgUnitsTree): integer;
function ComparePkgFilesAlphabetically(PkgFile1, PkgFile2: TPkgFile): integer;

function GetUsageOptionsList(PackageList: TFPList): TFPList;

function PkgFileTypeIdentToType(const s: string): TPkgFileType;
function LazPackageTypeIdentToType(const s: string): TLazPackageType;
function GetPkgFileTypeLocalizedName(FileType: TPkgFileType): string;
function NameToAutoUpdatePolicy(const s: string): TPackageUpdatePolicy;
function FileNameToPkgFileType(const AFilename: string): TPkgFileType;

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

function PackageFileNameIsValid(const AFilename: string): boolean;

procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig; const Path: string; FileVersion: integer);
procedure PkgVersionSaveToXMLConfig(Version: TPkgVersion; XMLConfig: TXMLConfig;
  const Path: string);
procedure PkgVersionLoadFromXMLConfig(Version: TPkgVersion;
  XMLConfig: TXMLConfig);

function IsValidUnitName(AUnitName: String): Boolean; inline;

var
  Package1: TLazPackage; // don't use it - only for options dialog

function dbgs(p: TPackageUpdatePolicy): string; overload;
function PackagePathToStr(PathList: TFPList): string;

implementation


function IsValidUnitName(AUnitName: String): Boolean;
begin
  Result := IsDottedIdentifier(AUnitName);
end;

function PkgFileTypeIdentToType(const s: string): TPkgFileType;
begin
  for Result:=Low(TPkgFileType) to High(TPkgFileType) do
    if CompareText(s,PkgFileTypeIdents[Result])=0 then exit;
  Result:=pftUnit;
end;

function LazPackageTypeIdentToType(const s: string): TLazPackageType;
begin
  for Result:=Low(TLazPackageType) to High(TLazPackageType) do
    if CompareText(s,LazPackageTypeIdents[Result])=0 then exit;
  Result:=lptRunTime;
end;

function GetPkgFileTypeLocalizedName(FileType: TPkgFileType): string;
begin
  case FileType of
  pftUnit: Result:=lisPkgFileTypeUnit;
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
    if CompareText(AutoUpdateNames[Result],s)=0 then exit;
  Result:=pupAsNeeded;
end;

function FileNameToPkgFileType(const AFilename: string): TPkgFileType;
begin
  if CompareFileExt(AFilename,'.lfm',true)=0 then
    Result:=pftLFM
  else if CompareFileExt(AFilename,'.lrs',true)=0 then
    Result:=pftLRS
  else if CompareFileExt(AFilename,'.inc',true)=0 then
    Result:=pftInclude
  else if FilenameIsPascalUnit(AFilename) then
    Result:=pftUnit
  else if CompareFileExt(AFilename,'.xml',true)=0 then
    Result:=pftIssues
  else if FileIsText(AFilename) then
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
    if PkgDependency.MakeSense then
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
  Pkg1: TLazPackageID;
  Pkg2: TLazPackageID;
begin
  Pkg1:=TLazPackageID(Data1);
  Pkg2:=TLazPackageID(Data2);
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
    Result:=CompareText(Name,Pkg.Name);
  end else
    Result:=-1;
end;

function ComparePkgIDMaskWithPackageID(Key, Data: Pointer): integer;
var
  Pkg1: TLazPackageID;
  Pkg2: TLazPackageID;
begin
  Pkg1:=TLazPackageID(Key);
  Pkg2:=TLazPackageID(Data);
  Result:=Pkg1.CompareMask(Pkg2);
end;

function CompareLazPackageIDNames(Data1, Data2: Pointer): integer;
var
  Pkg1: TLazPackageID;
  Pkg2: TLazPackageID;
begin
  Pkg1:=TLazPackageID(Data1);
  Pkg2:=TLazPackageID(Data2);
  Result:=CompareText(Pkg1.Name,Pkg2.Name);
end;

function CompareNameWithPkgDependency(Key, Data: Pointer): integer;
var
  PkgName: String;
  Dependency: TPkgDependency;
begin
  PkgName:=String(Key);
  Dependency:=TPkgDependency(Data);
  Result:=CompareText(PkgName,Dependency.PackageName);
end;

function ComparePkgDependencyNames(Data1, Data2: Pointer): integer;
var
  Dependency1: TPkgDependency;
  Dependency2: TPkgDependency;
begin
  Dependency1:=TPkgDependency(Data1);
  Dependency2:=TPkgDependency(Data2);
  Result:=CompareText(Dependency1.PackageName,Dependency2.PackageName);
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
    if CompareText(Result.PackageName,Name)=0 then exit;
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
  if Index<0 then RaiseException('GetDependencyWithIndex');
  Result:=First;
  while (Result<>nil) and (Index>0) do begin
    Result:=Result.NextDependency[ListType];
    dec(Index);
  end;
end;

function FindLowestPkgDependencyNodeWithName(const PkgName: string
  ): TAVLTreeNode;
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
  and (CompareText(TPkgDependency(Node.Data).PackageName,
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

function PackageFileNameIsValid(const AFilename: string): boolean;
var
  PkgName: String;
begin
  Result:=false;
  if CompareFileExt(AFilename,'.lpk',false)<>0 then exit;
  PkgName:=ExtractFileNameOnly(AFilename);
  if (PkgName='') or (not IsValidUnitName(PkgName)) then exit;
  Result:=true;
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
  DoDirSeparators(NewFilename);
  LazPackage.LongenFilename(NewFilename);
  if Filename=NewFilename then exit;
  inherited SetFilename(NewFilename);
  fFullFilenameStamp:=CompilerParseStamp;
  if fFullFilenameStamp=Low(fFullFilenameStamp) then
    fFullFilenameStamp:=High(fFullFilenameStamp)
  else
    dec(fFullFilenameStamp);
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
  if FFileType=AValue then exit;
  if (LazPackage<>nil) and (LazPackage.MainUnit=Self) then
    LazPackage.FMainUnit:=nil;
  FFileType:=AValue;
  FSourceDirNeedReference:=(FFileType in PkgFileRealUnitTypes) and not Removed;
  UpdateSourceDirectoryReference;
  if (FFileType=pftMainUnit) and (LazPackage<>nil)
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

procedure TPkgFile.UpdateUnitName;
var
  NewUnitName: String;
begin
  if FilenameIsPascalUnit(Filename) then begin
    NewUnitName:=ExtractFileNameOnly(Filename);
    if CompareText(NewUnitName,FUnitName)<>0 then
      FUnitName:=NewUnitName;
  end else
    FUnitName:='';
end;

function TPkgFile.GetComponentList: TFPList;
begin
  if FComponents=nil then FComponents:=TFPList.Create;
  Result:=FComponents;
end;

function TPkgFile.GetIDEPackage: TIDEPackage;
begin
  Result:=FPackage;
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
    if LazPackage<>nil then
      LazPackage.SubstitutePkgMacros(fFullFilename,false)
    else
      IDEMacros.SubstituteMacros(fFullFilename);
    fFullFilename:=TrimAndExpandFilename(fFullFilename);
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
  Clear;
  inherited Destroy;
end;

procedure TPkgFile.Clear;
begin
  AutoReferenceSourceDir:=false;
  inherited SetRemoved(false);
  inherited SetFilename('');
  FDirectory:='';
  FFlags:=[];
  FFileType:=pftUnit;
  FSourceDirectoryReferenced:=false;
  FSourceDirNeedReference:=true;
  FreeThenNil(FComponents);
  if (LazPackage<>nil) and (LazPackage.MainUnit=Self) then
    LazPackage.FMainUnit:=nil;
end;

procedure TPkgFile.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  FileVersion: integer; AdjustPathDelims: boolean);
var
  AFilename: String;
  CaseInsensitiveUnitName: String;
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
  fUnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');
  if FileType in PkgFileUnitTypes then begin
    // make sure the unitname makes sense
    CaseInsensitiveUnitName:=ExtractFileNameOnly(Filename);
    if CompareText(fUnitName,CaseInsensitiveUnitName)<>0 then
      fUnitName:=CaseInsensitiveUnitName;
  end;
  FResourceBaseClass:=StrToComponentBaseClass(
                         XMLConfig.GetValue(Path+'ResourceBaseClass/Value',''));
end;

procedure TPkgFile.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch);
var
  TmpFilename: String;
begin
  TmpFilename:=Filename;
  FPackage.ShortenFilename(TmpFilename,true);
  XMLConfig.SetDeleteValue(Path+'Filename/Value',
                           SwitchPathDelims(TmpFilename,UsePathDelim),'');
  XMLConfig.SetDeleteValue(Path+'HasRegisterProc/Value',HasRegisterProc,
                           false);
  XMLConfig.SetDeleteValue(Path+'AddToUsesPkgSection/Value',AddToUsesPkgSection,
                           FileType in PkgFileUnitTypes);
  XMLConfig.SetDeleteValue(Path+'Type/Value',PkgFileTypeIdents[FileType],
                           PkgFileTypeIdents[pftUnit]);
  XMLConfig.SetDeleteValue(Path+'UnitName/Value',FUnitName,'');
  XMLConfig.SetDeleteValue(Path+'ResourceBaseClass/Value',
                           PFComponentBaseClassNames[FResourceBaseClass],
                           PFComponentBaseClassNames[pfcbcNone]);
end;

procedure TPkgFile.ConsistencyCheck;
begin
  if FPackage=nil then
    RaiseGDBException('TPkgFile.ConsistencyCheck FPackage=nil');
  if Filename='' then
    RaiseGDBException('TPkgFile.ConsistencyCheck FFilename=""');
end;

function TPkgFile.IsVirtual: boolean;
begin
  Result:=not FilenameIsAbsolute(Filename);
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
  Result:=ReadAllLinks(GetFullFilename,false);
  if Result='' then Result:=Filename;
end;

{ TPkgDependency }

procedure TPkgDependency.SetFlags(const AValue: TPkgDependencyFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

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

procedure TPkgDependency.SetLoadPackageResult(const AValue: TLoadPackageResult);
begin
  if FLoadPackageResult=AValue then exit;
  FLoadPackageResult:=AValue;
end;

procedure TPkgDependency.SetMaxVersion(const AValue: TPkgVersion);
begin
  if FMaxVersion=AValue then exit;
  FMaxVersion:=AValue;
end;

procedure TPkgDependency.SetMinVersion(const AValue: TPkgVersion);
begin
  if FMinVersion=AValue then exit;
  FMinVersion:=AValue;
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

procedure TPkgDependency.SetRemoved(const AValue: boolean);
begin
  if FRemoved=AValue then exit;
  FRemoved:=AValue;
end;

procedure TPkgDependency.SetRequiredPackage(const AValue: TLazPackage);
begin
  if FRequiredPackage=AValue then exit;
  if FRequiredPackage<>nil then
    FRequiredPackage.RemoveUsedByDependency(Self);
  fLoadPackageResult:=lprUndefined;
  FRequiredPackage:=AValue;
  if FRequiredPackage<>nil then
    FRequiredPackage.AddUsedByDependency(Self);
end;

constructor TPkgDependency.Create;
begin
  MinVersion:=TPkgVersion.Create;
  MaxVersion:=TPkgVersion.Create;
  Clear;
end;

destructor TPkgDependency.Destroy;
begin
  RequiredPackage:=nil;
  PackageName:='';
  FreeAndNil(fMinVersion);
  FreeAndNil(fMaxVersion);
  inherited Destroy;
end;

procedure TPkgDependency.Clear;
begin
  RequiredPackage:=nil;
  PackageName:='';
  FRemoved:=false;
  FFlags:=[];
  FMaxVersion.Clear;
  FMinVersion.Clear;
  FDefaultFilename:='';
  FPreferDefaultFilename:=false;
end;

procedure TPkgDependency.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; FileVersion: integer);
  
  function LoadFilename(const SubPath: string): string;
  var
    BaseDir: String;
  begin
    Result:=SetDirSeparators(XMLConfig.GetValue(Path+SubPath,''));
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
end;

function TPkgDependency.MakeSense: boolean;
begin
  Result:=IsValidUnitName(PackageName);
  if Result
  and (pdfMinVersion in FFlags) and (pdfMaxVersion in FFlags)
  and (MinVersion.Compare(MaxVersion)>0) then
    Result:=false;
end;

function TPkgDependency.IsCompatible(const Version: TPkgVersion): boolean;
begin
  if ((pdfMinVersion in FFlags) and (MinVersion.Compare(Version)>0))
  or ((pdfMaxVersion in FFlags) and (MaxVersion.Compare(Version)<0)) then
    Result:=false
  else
    Result:=true;
end;

function TPkgDependency.IsCompatible(const PkgName: string;
  const Version: TPkgVersion): boolean;
begin
  Result:=(CompareText(PkgName,PackageName)=0) and IsCompatible(Version);
end;

function TPkgDependency.Compare(Dependency2: TPkgDependency): integer;
begin
  Result:=CompareText(PackageName,Dependency2.PackageName);
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

function TPkgDependency.AsString(WithOwner: boolean): string;
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

procedure TPkgDependency.MoveUpInList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList);
var
  OldPrev: TPkgDependency;
begin
  if (FirstDependency=Self) or (PrevDependency[ListType]=nil) then exit;
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
end;

procedure TPkgDependency.MoveDownInList(var FirstDependency: TPkgDependency;
  ListType: TPkgDependencyList);
var
  OldNext: TPkgDependency;
begin
  if (NextDependency[ListType]=nil) then exit;
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
end;

function TPkgDependency.MakeFilenameRelativeToOwner(const AFilename: string
  ): string;
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
  or (SysUtils.CompareText(ExtractFileNameOnly(AFilename),PackageName)<>0)
  then
    exit;
  if not FilenameIsAbsolute(AFilename) then begin
    CurDir:=GetDependencyOwnerDirectory(Self);
    if (CurDir<>'') then
      AFilename:=AppendPathDelim(CurDir)+AFilename;
  end;
  if not FilenameIsAbsolute(AFilename) then exit;
  AFilename:=FindDiskFileCaseInsensitive(AFilename);
  if not FileExistsCached(AFilename) then exit;
  Result:=AFilename;
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
  if (MacroName<>'') and IsValidIdent(MacroName) then
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

  // check local macros
  if CompareText(MacroName,'PkgOutDir')=0 then begin
    Handled:=true;
    if Data=CompilerOptionMacroNormal then
      s:=CompilerOptions.ParsedOpts.GetParsedValue(pcosOutputDir)
    else
      s:=CompilerOptions.ParsedOpts.GetParsedPIValue(pcosOutputDir);
    exit;
  end
  else if CompareText(MacroName,'PkgDir')=0 then begin
    Handled:=true;
    s:=FDirectory;
    exit;
  end
  else if CompareText(MacroName,'PkgName')=0 then begin
    Handled:=true;
    s:=Name;
    exit;
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

function TLazPackage.GetAutoIncrementVersionOnBuild: boolean;
begin
  Result:=lpfAutoIncrementVersionOnBuild in FFlags;
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
    LazDocPaths:=aSource.LazDocPaths;
    IconFile:=aSource.IconFile;
    UsageOptions.AssignOptions(aSource.UsageOptions);
    EnableI18N:=aSource.EnableI18N;
    Description:=aSource.Description;
    AutoUpdate:=aSource.AutoUpdate;
    AutoIncrementVersionOnBuild:=aSource.AutoIncrementVersionOnBuild;
    Author:=aSource.Author;
    AddToProjectUsesSection:=aSource.AddToProjectUsesSection;
  end;
end;

function TLazPackage.GetRemovedFiles(Index: integer): TPkgFile;
begin
  Result:=TPkgFile(FRemovedFiles[Index]);
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

procedure TLazPackage.SetAutoCreated(const AValue: boolean);
begin
  if FAutoCreated=AValue then exit;
  FAutoCreated:=AValue;
  if AutoCreated then UserReadOnly:=true;
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

procedure TLazPackage.SetAutoInstall(const AValue: TPackageInstallType);
begin
  if FAutoInstall=AValue then exit;
  FAutoInstall:=AValue;
  if AutoCreated and (FAutoInstall<>pitStatic) then
    DumpStack;
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
  DoDirSeparators(NewFilename);
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
  if (FFilename<>'') and (FFilename[length(FFilename)]=PathDelim) then
    FDirectory:=FFilename
  else
    FDirectory:=ExtractFilePath(FFilename);
  FDirectoryExpandedChangeStamp:=InvalidParseStamp;
  FHasDirectory:=(FDirectory<>'') and (FDirectory[length(FDirectory)]=PathDelim);
  FHasStaticDirectory:=FHasDirectory and FilenameIsAbsolute(FDirectory);
  FUsageOptions.BaseDirectory:=FDirectory;
  FCompilerOptions.BaseDirectory:=FDirectory;
  Modified:=true;
end;

procedure TLazPackage.SetFlags(const AValue: TLazPackageFlags);
var
  ChangedFlags: TLazPackageFlags;
begin
  if FFlags=AValue then exit;
  ChangedFlags:=FFlags+AValue-(FFlags*AValue);
  FFlags:=AValue;
  if ChangedFlags*[lpfAutoIncrementVersionOnBuild]<>[] then
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

procedure TLazPackage.SetLazDocPaths(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimSearchPath(AValue,'');
  if FLazDocPaths=NewValue then exit;
  FLazDocPaths:=NewValue;
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
  if FLPKSource<>nil then
    FLPKSourceChangeStep:=FLPKSource.ChangeStep;
  // do not change Filename here.
  // See TPkgManager.DoSavePackage and TPkgManager.DoOpenPackageFile
  // the LPKSource is the codebuffer last used during load/save, so it is not valid
  // for packages that were not yet loaded/saved or during renaming/loading/saving.
end;

procedure TLazPackage.SetLPKSourceChangeStep(const AValue: integer);
begin
  if FLPKSourceChangeStep=AValue then exit;
  FLPKSourceChangeStep:=AValue;
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

procedure TLazPackage.SetModified(const AValue: boolean);
var
  OldModified: Boolean;
begin
  OldModified:=Modified;
  if AValue and (FModifiedLock>0) then exit;
  if AValue then begin
    Include(FFlags,lpfModified);
    if FChangeStamp<High(FChangeStamp) then
      inc(FChangeStamp)
    else
      FChangeStamp:=low(FChangeStamp);
  end else
    Exclude(FFlags,lpfModified);
  Exclude(FFlags,lpfSkipSaving);
  if not AValue then
  begin
    PublishOptions.Modified:=false;
    CompilerOptions.Modified:=false;
  end;
  if (OldModified<>Modified) and (Editor<>nil) then
    Editor.UpdateAll(false);
end;

procedure TLazPackage.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  inherited SetName(AValue);
  FDefineTemplates.PackageIDChanged;
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
  FCompilerOptions:=TPkgCompilerOptions.Create(Self);
  FLazCompilerOptions:=FCompilerOptions;
  FCompilerOptions.ParsedOpts.InvalidateParseOnChange:=true;
  FCompilerOptions.ParsedOpts.OnLocalSubstitute:=@SubstitutePkgMacros;
  FCompilerOptions.DefaultMakeOptionsFlags:=[ccloNoLinkerOpts];
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
  FreeAndNil(FCompilerOptions);
  FreeAndNil(FUsageOptions);
  FreeAndNil(FMacros);
  FreeAndNil(FSourceDirectories);
  inherited Destroy;
end;

class function TLazPackage.GetGroupCaption: string;
begin
  Result := lisPckOptsPackageOptions;
end;

class function TLazPackage.GetInstance: TAbstractIDEOptions;
begin
  Result := Package1;
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
  if FUpdateLock=0 then RaiseException('TLazPackage.EndUpdate');
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
    FCompilerOptions.Clear;
    FDescription:='';
    FDirectory:='';
    FDirectoryExpandedChangeStamp:=InvalidParseStamp;
    FHasDirectory:=false;
    FHasStaticDirectory:=false;
    FVersion.Clear;
    FFilename:='';
    FIconFile:='';
    FInstalled:=pitNope;
    FName:='';
    FPackageType:=lptRunAndDesignTime;
    FRegistered:=false;
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
    fCompilerOptions.UnitOutputDirectory:=
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
  FDefineTemplates.PackageIDChanged;
  Modified:=true;
end;

procedure TLazPackage.SourceDirectoriesChanged(Sender: TObject);
begin
  FDefineTemplates.SourceDirectoriesChanged;
end;

procedure TLazPackage.LockModified;
begin
  inc(FModifiedLock);
end;

procedure TLazPackage.UnlockModified;
begin
  if FModifiedLock<=0 then
    RaiseException('TLazPackage.UnlockModified');
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
  FAddToProjectUsesSection:=XMLConfig.GetValue(Path+'AddToProjectUsesSection/Value',
    FileVersion<4); // since version 4 the default is false
  FAuthor:=XMLConfig.GetValue(Path+'Author/Value','');
  FAutoUpdate:=NameToAutoUpdatePolicy(
                                XMLConfig.GetValue(Path+'AutoUpdate/Value',''));
  FDescription:=XMLConfig.GetValue(Path+'Description/Value','');
  FLicense:=XMLConfig.GetValue(Path+'License/Value','');
  PkgVersionLoadFromXMLConfig(FVersion,XMLConfig,Path+'Version/',FileVersion);
  FIconFile:=SwitchPathDelims(XMLConfig.GetValue(Path+'IconFile/Value',''),
                              PathDelimChanged);
  OutputStateFile:=SwitchPathDelims(
                            XMLConfig.GetValue(Path+'OutputStateFile/Value',''),
                            PathDelimChanged);
  fLazDocPaths:=SwitchPathDelims(XMLConfig.GetValue(Path+'LazDoc/Paths',''),
                            PathDelimChanged);
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

  LoadFiles(Path+'Files/',FFiles);
  UpdateSourceDirectories;
  LoadFlags(Path);
  LoadPkgDependencyList(XMLConfig,Path+'RequiredPkgs/',
                        FFirstRequiredDependency,pdlRequires,Self,false,false);
  if FileVersion<2 then
    FCompilerOptions.LoadFromXMLConfig(XMLConfig,'CompilerOptions/')
  else
    FCompilerOptions.LoadFromXMLConfig(XMLConfig,Path+'CompilerOptions/');
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
  XMLConfig.SetDeleteValue(Path+'Name/Value',FName,'');
  XMLConfig.SetDeleteValue(Path+'AddToProjectUsesSection/Value',
                           FAddToProjectUsesSection,false);
  XMLConfig.SetDeleteValue(Path+'Author/Value',FAuthor,'');
  XMLConfig.SetDeleteValue(Path+'AutoUpdate/Value',AutoUpdateNames[FAutoUpdate],
                           AutoUpdateNames[pupAsNeeded]);
  FCompilerOptions.SaveToXMLConfig(XMLConfig,Path+'CompilerOptions/');
  XMLConfig.SetDeleteValue(Path+'Description/Value',FDescription,'');
  XMLConfig.SetDeleteValue(Path+'License/Value',FLicense,'');
  PkgVersionSaveToXMLConfig(FVersion,XMLConfig,Path+'Version/');
  SaveFiles(Path+'Files/',FFiles);
  SaveFlags(Path);
  XMLConfig.SetDeleteValue(Path+'IconFile/Value',f(FIconFile),'');
  XMLConfig.SetDeleteValue(Path+'Name/Value',FName,'');
  XMLConfig.SetDeleteValue(Path+'OutputStateFile/Value',f(OutputStateFile),'');
  XMLConfig.SetDeleteValue(Path+'LazDoc/Paths',f(FLazDocPaths),'');

  XMLConfig.SetDeleteValue(Path+'i18n/EnableI18N/Value', EnableI18N, false);
  XMLConfig.SetDeleteValue(Path+'i18n/OutDir/Value',f(FPOOutputDirectory), '');

  XMLConfig.SetDeleteValue(Path+'Type/Value',LazPackageTypeIdents[FPackageType],
                           LazPackageTypeIdents[lptRunTime]);
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
  Result:=(not FilenameIsAbsolute(Filename));
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

function TLazPackage.MakeSense: boolean;
begin
  Result:=false;
  if (Name='') or (not IsValidUnitName(Name)) then exit;
  Result:=true;
end;

procedure TLazPackage.ShortenFilename(var ExpandedFilename: string;
  UseUp: boolean);
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
var
  s: String;
begin
  Result:=GetFullFilename(ResolveMacros);
  s:=ReadAllLinks(Result,false);
  if s<>'' then Result:=s;
end;

function TLazPackage.GetSourceDirs(WithPkgDir, WithoutOutputDir: boolean
  ): string;
begin
  Result:=SourceDirectories.CreateSearchPathFromAllFiles;
  if WithPkgDir then
    Result:=MergeSearchPaths(Result,Directory);
  if WithoutOutputDir then
    Result:=RemoveSearchPaths(Result,GetOutputDirectory);
end;

procedure TLazPackage.IterateComponentClasses(
  Event: TIterateComponentClassesEvent;
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

function TLazPackage.IndexOfPkgComponent(PkgComponent: TPkgComponent): integer;
begin
  Result:=FComponents.IndexOf(PkgComponent);
end;

function TLazPackage.FindPkgFile(const AFilename: string;
  IgnoreRemoved, FindNewFile: boolean): TPkgFile;
var
  TheFilename: String;
  Cnt: Integer;
  i: Integer;
begin
  Result:=nil;
  
  TheFilename:=AFilename;
  
  if FindNewFile and (not FilenameIsAbsolute(TheFilename)) then begin
    // this is a virtual file, not yet saved
    // -> prepend Package Directory and check if it does not exists yet in
    // the package directory
    LongenFilename(TheFilename);
    if FileExistsUTF8(TheFilename) then begin
      // the file exists -> this virtual file does not belong to the package
      exit;
    end;
  end;
  
  Cnt:=FileCount;
  for i:=0 to Cnt-1 do begin
    Result:=Files[i];
    if CompareFilenames(Result.Filename,TheFilename)=0 then
      exit;
  end;
  if not IgnoreRemoved then begin
    Cnt:=RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      Result:=RemovedFiles[i];
      if CompareFilenames(Result.Filename,TheFilename)=0 then
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
    if CompareText(Result.Unit_Name,TheUnitName)=0 then exit;
  end;
  if not IgnoreRemoved then begin
    Cnt:=RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      Result:=RemovedFiles[i];
      if IgnorePkgFile=Result then continue;
      if CompareText(Result.Unit_Name,TheUnitName)=0 then exit;
    end;
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

function TLazPackage.FindDependencyByName(const PkgName: string
  ): TPkgDependency;
begin
  Result:=FindDependencyByNameInList(FFirstRequiredDependency,pdlRequires,
                                     PkgName);
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

function TLazPackage.FindUsedByDepPrefer(Ignore: TPkgDependency
  ): TPkgDependency;
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
  Modified:=true;
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

procedure TLazPackage.RemoveFile(PkgFile: TPkgFile);
begin
  FFiles.Remove(PkgFile);
  FRemovedFiles.Add(PkgFile);
  PkgFile.Removed:=true;
  Modified:=true;
end;

procedure TLazPackage.UnremovePkgFile(PkgFile: TPkgFile);
begin
  FFiles.Add(PkgFile);
  FRemovedFiles.Remove(PkgFile);
  PkgFile.Removed:=false;
end;

function TLazPackage.RemoveNonExistingFiles: boolean;
var
  i: Integer;
  AFilename: String;
begin
  Result:=false;
  i:=FileCount-1;
  while i>=0 do begin
    if i>=FileCount then continue;
    AFilename:=Files[i].GetResolvedFilename;
    if (AFilename='') or (not FileExistsCached(Files[i].Filename)) then
    begin
      RemoveFile(Files[i]);
      Result:=true;
    end;
    dec(i);
  end;
end;

function TLazPackage.GetFileDialogInitialDir(const DefaultDirectory: string
  ): string;
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
  Modified:=true;
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
    while (Result>=0) and (CompareText(Filename,List[Result])<>0) do
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
begin
  Result:=false;
  Cnt:=FileCount;
  SrcDirs:=nil;
  try
    for i:=0 to Cnt-1 do begin
      CurFile:=Files[i];
      CurDir:=CurFile.Directory;
      //debugln('TLazPackage.FixFilesCaseSensitivity A ',dbgs(i),' CurFile.Filename=',CurFile.Filename);
      DirListing:=AddDirectoryListing(CurDir);
      CurShortFilename:=ExtractFilename(CurFile.Filename);
      DirListID:=IndexOfFileInStringList(DirListing,CurShortFilename,false);
      //debugln('TLazPackage.FixFilesCaseSensitivity B ',dbgs(i),' CurShortFilename=',CurShortFilename,' DirListID=',dbgs(DirListID));
      if DirListID<0 then continue;
      NewShortFilename:=DirListing[DirListID];
      //debugln('TLazPackage.FixFilesCaseSensitivity New ',dbgs(i),' NewShortFilename=',NewShortFilename);
      if CurShortFilename<>NewShortFilename then begin
        // case changes
        NewFilename:=
            AppendPathDelim(ExtractFilePath(CurFile.Filename))+NewShortFilename;
        //debugln('TLazPackage.FixFilesCaseSensitivity New ',dbgs(i),' NewFilename=',NewFilename);
        CurFile.Filename:=NewFilename;
        Result:=true;
      end;
    end;
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

procedure TLazPackage.AddPackageDependency(const PkgName: string);
var
  Dependency: TPkgDependency;
begin
  if FindDependencyByName(PkgName)<>nil then exit;
  Dependency:=TPkgDependency.Create;
  Dependency.PackageName:=PkgName;
  AddRequiredDependency(Dependency);
end;

procedure TLazPackage.RemoveRequiredDependency(Dependency: TPkgDependency);
begin
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.RequiredPackage:=nil;
  Dependency.AddToList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=true;
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

procedure TLazPackage.MoveRequiredDependencyUp(Dependency: TPkgDependency);
begin
  Dependency.MoveUpInList(FFirstRequiredDependency,pdlRequires);
end;

procedure TLazPackage.MoveRequiredDependencyDown(Dependency: TPkgDependency);
begin
  Dependency.MoveDownInList(FFirstRequiredDependency,pdlRequires);
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

procedure TLazPackage.AddUsedByDependency(Dependency: TPkgDependency);
begin
  Dependency.AddToList(FFirstUsedByDependency,pdlUsedBy);
  if Dependency.HoldPackage then
    inc(FHoldPackageCount);
end;

procedure TLazPackage.RemoveUsedByDependency(Dependency: TPkgDependency);
begin
  Dependency.RemoveFromList(FFirstUsedByDependency,pdlUsedBy);
  if Dependency.HoldPackage then
    dec(FHoldPackageCount);
end;

procedure TLazPackage.ChangeID(const NewName: string; NewVersion: TPkgVersion);
begin
  Version.Assign(NewVersion);
  Name:=NewName;
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

procedure TLazPackage.GetAllRequiredPackages(var List: TFPList;
  WithSelf: boolean);
begin
  if Assigned(OnGetAllRequiredPackages) then
    OnGetAllRequiredPackages(FirstRequiredDependency,List);
  if WithSelf then begin
    if List=nil then List:=TFPList.Create;
    if List.IndexOf(Self)<0 then;
      List.Insert(0,Self);
  end else if List<>nil then begin
    List.Remove(Self);
    if List.Count=0 then FreeAndNil(List);
  end;
end;

procedure TLazPackage.GetInheritedCompilerOptions(var OptionsList: TFPList);
var
  PkgList: TFPList; // list of TLazPackage
begin
  PkgList:=nil;
  GetAllRequiredPackages(PkgList,false);
  OptionsList:=GetUsageOptionsList(PkgList);
  PkgList.Free;
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
    Result:=CompilerOptions.ParsedOpts.GetParsedValue(pcosOutputDir,UseOverride);
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
    Result:=StringReplace(Result,'%(CPU_TARGET)','$(TargetCPU)',[rfReplaceAll]);
    Result:=StringReplace(Result,'%(OS_TARGET)','$(TargetOS)',[rfReplaceAll]);
    Result:=StringReplace(Result,'%(LCL_PLATFORM)','$(LCLWidgetType)',[rfReplaceAll]);
    Result:=SubstitutePkgMacros(Result,false);
  end;
end;

function TLazPackage.NeedsDefineTemplates: boolean;
begin
  if IsVirtual or AutoCreated or (lpfDestroying in Flags) or (Name='') then
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
      Result:=CompareText(SearchedFilename,TheFilename)=0;
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
      Result:=CompareText(SearchedFilename,TheFilename)=0;
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
  if fIconLoaded then begin
    FIcon.Free;
    FIcon:=nil;
    fIconLoaded:=false;
  end;
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
    if CompareText(TIUnitName,Result)<>0 then
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

function TPkgComponent.Icon: TCustomBitmap;
begin
  if not fIconLoaded
  then begin
    fIcon:=GetIconCopy;
    fIconLoaded:=true;
  end;
  Result:=FIcon;
end;

function TPkgComponent.GetIconCopy: TCustomBitMap;
var
  ResHandle: TLResource;
  ResName: String;
begin
  Result := nil;
  ResName := ComponentClass.ClassName;
  // prevent raising exception and speedup a bit search/load
  ResHandle := LazarusResources.Find(ResName);
  if ResHandle <> nil then
    Result := CreateBitmapFromLazarusResource(ResHandle)
  else
  if FindResource(HInstance, PChar(ResName), PChar(RT_BITMAP)) <> 0 then
  begin
    Result := TBitmap.Create;
    Result.LoadFromResourceName(HInstance, ResName);
    Result.Transparent := True;
  end;

  if Result = nil then
    Result := CreateBitmapFromLazarusResource('default');
end;

function TPkgComponent.HasIcon: boolean;
begin
  Result:=Page.PageName<>'';
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

procedure TPkgCompilerOptions.SetConditionals(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=Trim(AValue);
  if Conditionals=NewValue then exit;
  InvalidateOptions;
  inherited SetConditionals(NewValue);
end;

constructor TPkgCompilerOptions.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  if AOwner<>nil then
    FLazPackage := AOwner as TLazPackage;
end;

class function TPkgCompilerOptions.GetGroupCaption: string;
begin
  Result := dlgCompilerOptions;
end;

class function TPkgCompilerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := Package1.CompilerOptions;
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
    Result:=LazPackage.IDAsString;
end;

procedure TPkgCompilerOptions.InvalidateOptions;
begin
  if (LazPackage=nil) then exit;
  if LazPackage.UsageOptions=nil then RaiseException('');
  if LazPackage.UsageOptions.ParsedOpts=nil then RaiseException('');
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

function TPkgCompilerOptions.CreateTargetFilename(
  const MainSourceFileName: string): string;
begin
  Result:='';
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

constructor TLazPackageDefineTemplates.Create(OwnerPackage: TLazPackage);
begin
  FLazPackage:=OwnerPackage;
  fLastSourceDirStamp:=CTInvalidChangeStamp;
end;

destructor TLazPackageDefineTemplates.Destroy;
begin
  Clear;
  fLastSourceDirectories.Free;
  inherited Destroy;
end;

procedure TLazPackageDefineTemplates.Clear;
begin
  if FMain<>nil then begin
    if (CodeToolBoss<>nil) then
      CodeToolBoss.DefineTree.RemoveDefineTemplate(FMain);
    FMain:=nil;
    FSrcDirIfDef:=nil;
    FSrcDirectories:=nil;
    FOutputDir:=nil;
    FOutPutSrcPath:=nil;
    fLastOutputDirSrcPathIDAsString:='';
    FLastCustomOptions:='';
    fLastUnitPath:='';
    fLastSourceDirsIDAsString:='';
    if fLastSourceDirectories<>nil then
      fLastSourceDirectories.Clear;
    FFlags:=FFlags+[pdtIDChanged,pdtOutputDirChanged,pdtSourceDirsChanged,
                    pdtCustomDefinesChanged];
  end;
end;

procedure TLazPackageDefineTemplates.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TLazPackageDefineTemplates.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TLazPackageDefineTemplates.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if pdtIDChanged in FFlags then PackageIDChanged;
    if pdtSourceDirsChanged in FFlags then SourceDirectoriesChanged;
    if pdtOutputDirChanged in FFlags then OutputDirectoryChanged;
    if pdtCustomDefinesChanged in FFlags then CustomDefinesChanged;
  end;
end;

procedure TLazPackageDefineTemplates.PackageIDChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,pdtIDChanged);
    exit;
  end;
  Exclude(FFlags,pdtIDChanged);
  UpdateMain;
  UpdateOutputDirectory;
  UpdateSourceDirectories;
  UpdateDefinesForCustomDefines;
end;

procedure TLazPackageDefineTemplates.SourceDirectoriesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,pdtSourceDirsChanged);
    exit;
  end;
  Exclude(FFlags,pdtSourceDirsChanged);
  UpdateSourceDirectories;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.OutputDirectoryChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,pdtOutputDirChanged);
    exit;
  end;
  Exclude(FFlags,pdtOutputDirChanged);
  UpdateOutputDirectory;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.CustomDefinesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,pdtCustomDefinesChanged);
    exit;
  end;
  Exclude(FFlags,pdtCustomDefinesChanged);
  UpdateDefinesForCustomDefines;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.AllChanged;
begin
  PackageIDChanged;
  UpdateSrcDirIfDef;// always create the SrcDirIfDef for IDE add-ons
  SourceDirectoriesChanged;
  OutputDirectoryChanged;
  CustomDefinesChanged;
end;

procedure TLazPackageDefineTemplates.UpdateMain;
begin
  if (not LazPackage.NeedsDefineTemplates) or (not Active) then exit;
  // update the package block define template (the container for all other
  // define templates of the package)
  if FMain=nil then begin
    FMain:=CreatePackageTemplateWithID(LazPackage.IDAsWord);
    FMain.SetDefineOwner(LazPackage,false);
    FMain.SetFlags([dtfAutoGenerated],[],false);
  end else
    FMain.Name:=LazPackage.IDAsWord;
  // ClearCache is here unnessary, because it is only a block
end;

procedure TLazPackageDefineTemplates.UpdateSrcDirIfDef;
var
  NewVariable: String;
  Changed: Boolean;
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
  if FSrcDirIfDef=nil then begin
    FSrcDirIfDef:=TDefineTemplate.Create('Source Directory Additions',
      'Additional defines for package source directories',
      '#PkgSrcMark'+LazPackage.IDAsWord,'',
      da_IfDef);
    FMain.AddChild(FSrcDirIfDef);

    // create unit path template for this directory
    UnitPathDefTempl:=TDefineTemplate.Create('UnitPath', lisPkgDefsUnitPath,
      '#UnitPath','$(#UnitPath);$PkgUnitPath('+LazPackage.IDAsString+')',
      da_Define);
    FSrcDirIfDef.AddChild(UnitPathDefTempl);
    // create include path template for this directory
    IncPathDefTempl:=TDefineTemplate.Create('IncPath','Include Path',
      '#IncPath','$(#IncPath);$PkgIncPath('+LazPackage.IDAsString+')',
      da_Define);
    FSrcDirIfDef.AddChild(IncPathDefTempl);

    Changed:=true;
  end else begin
    NewVariable:='#PkgSrcMark'+LazPackage.IDAsWord;
    if NewVariable<>FSrcDirIfDef.Variable then begin
      FSrcDirIfDef.Variable:=NewVariable;
      // unit path
      UnitPathDefTempl:=FSrcDirIfDef.FindChildByName('UnitPath');
      if UnitPathDefTempl<>nil then
        UnitPathDefTempl.Value:='$(#UnitPath);$PkgUnitPath('+LazPackage.IDAsString+')';
      // include path
      IncPathDefTempl:=FSrcDirIfDef.FindChildByName('IncPath');
      if IncPathDefTempl<>nil then
        IncPathDefTempl.Value:='$(#IncPath);$PkgIncPath('+LazPackage.IDAsString+')';

      Changed:=true;
    end;
  end;
  if Changed then
    CodeToolBoss.DefineTree.ClearCache;
end;

procedure TLazPackageDefineTemplates.SetActive(const AValue: boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if not FActive then Clear else AllChanged;
end;

procedure TLazPackageDefineTemplates.UpdateOutputDirectory;
begin
  if FMain=nil then UpdateMain;
  if FMain=nil then exit;

  if FOutputDir=nil then begin
    FOutputDir:=TDefineTemplate.Create(PkgOutputDirDefTemplName,
      lisPkgDefsOutputDirectory, '', LazPackage.GetOutputDirectory, da_Directory
        );
    FOutputDir.SetDefineOwner(LazPackage,false);
    FOutputDir.SetFlags([dtfAutoGenerated],[],false);
    DisableDefaultsInDirectories(FOutputDir,false);
    FMain.AddChild(FOutputDir);
  end else begin
    FOutputDir.Value:=LazPackage.GetOutputDirectory;
  end;

  if (FOutPutSrcPath=nil)
  or (fLastOutputDirSrcPathIDAsString<>LazPackage.IDAsString) then begin
    fLastOutputDirSrcPathIDAsString:=LazPackage.IDAsString;
    FOutputSrcPath:=TDefineTemplate.Create('CompiledSrcPath',
      lisPkgDefsCompiledSrcPathAddition, CompiledSrcPathMacroName,
      '$PkgSrcPath('+fLastOutputDirSrcPathIDAsString+');'
        +'$('+CompiledSrcPathMacroName+')',
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
  SrcDirDefTempl: TDefineTemplate;
  IDHasChanged: Boolean;
  SrcDirMarkDefTempl: TDefineTemplate;
  CurUnitPath: String;
  SrcDirs: String;
begin
  if (not LazPackage.NeedsDefineTemplates) or (not Active) then exit;

  // quick check if something has changed
  IDHasChanged:=fLastSourceDirsIDAsString<>LazPackage.IDAsString;
  CurUnitPath:=LazPackage.CompilerOptions.ParsedOpts.GetParsedValue(pcosUnitPath);
  SrcDirs:=LazPackage.SourceDirectories.CreateSearchPathFromAllFiles;
  CurUnitPath:=TrimSearchPath(SrcDirs+';'+CurUnitPath+';.',
                              LazPackage.CompilerOptions.BaseDirectory,true);

  if (fLastSourceDirectories<>nil)
  and (fLastSourceDirStamp=LazPackage.SourceDirectories.TimeStamp)
  and (not IDHasChanged)
  and (CurUnitPath=fLastUnitPath) then
    exit;
  //debugln(['TLazPackageDefineTemplates.UpdateSourceDirectories ',LazPackage.Name,' CurUnitPath=',CurUnitPath]);
  fLastSourceDirStamp:=LazPackage.SourceDirectories.TimeStamp;
  fLastSourceDirsIDAsString:=LazPackage.IDAsString;
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
  and ((FSrcDirIfDef=nil) or IDHasChanged) then
    UpdateSrcDirIfDef;
  for i:=0 to fLastSourceDirectories.Count-1 do begin
    // create directory template
    SrcDirDefTempl:=TDefineTemplate.Create('Source Directory '+IntToStr(i+1),
      fLastSourceDirectories[i],'',fLastSourceDirectories[i],da_Directory);
    DisableDefaultsInDirectories(SrcDirDefTempl,false);
    fLastSourceDirectories.Objects[i]:=SrcDirDefTempl;
    // add package source directory marker
    SrcDirMarkDefTempl:=TDefineTemplate.Create('PkgSrcDirMark',
      lisPkgDefsSrcDirMark,'#PkgSrcMark'+LazPackage.IDAsWord,'',da_Define);
    SrcDirDefTempl.AddChild(SrcDirMarkDefTempl);

    SrcDirDefTempl.SetDefineOwner(LazPackage,false);
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
  if (not LazPackage.NeedsDefineTemplates) or (not Active) then exit;

  // check if something has changed
  NewCustomOptions:=LazPackage.CompilerOptions.GetOptionsForCTDefines;
  if FLastCustomOptions=NewCustomOptions then exit;

  FLastCustomOptions:=NewCustomOptions;
  OptionsDefTempl:=CodeToolBoss.DefinePool.CreateFPCCommandLineDefines(
                          'Custom Options',FLastCustomOptions,false,LazPackage);
  if OptionsDefTempl=nil then begin
    // no custom options -> delete old template
    if FSrcDirIfDef<>nil then begin
      if FSrcDirIfDef.DeleteChild('Custom Options') then
        CodeToolBoss.DefineTree.ClearCache;
    end;
  end else begin
    UpdateSrcDirIfDef;
    FSrcDirIfDef.ReplaceChild(OptionsDefTempl);
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
  if Modified then LazPackage.Modified:=true;
end;

constructor TPublishPackageOptions.Create(TheLazPackage: TLazPackage);
begin
  FLazPackage:=TheLazPackage;
  inherited Create(FLazPackage);
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

function TPkgPairTree.FindPair(Pkg1, Pkg2: TLazPackage; IgnoreOrder: boolean
  ): TPkgPair;
var
  Comp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=Root;
  while (ANode<>nil) do begin
    Result:=TPkgPair(ANode.Data);
    Comp:=-Result.ComparePair(Pkg1,Pkg2);
    if Comp=0 then exit;
    if Comp<0 then begin
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

function TPkgUnitsTree.FindNodeWithUnitName(const AUnitName: string
  ): TAVLTreeNode;
var
  Comp: integer;
  PkgFile: TPkgFile;
begin
  Result:=Root;
  while (Result<>nil) do begin
    PkgFile:=TPkgFile(Result.Data);
    Comp:=CompareText(AUnitName,PkgFile.Unit_Name);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TPkgUnitsTree.FindPkgFileWithUnitName(const AUnitName: string
  ): TPkgFile;
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
  Result := CompareText(
              TPkgFile(PkgFile1).Unit_Name,
              TPkgFile(PkgFile2).Unit_Name);
end;

constructor TPkgUnitsTree.Create(ThePackage: TLazPackage);
begin
  fLazPackage:=ThePackage;
  inherited Create(@ComparePkgFilesUnitname);
end;

initialization
  RegisterIDEOptionsGroup(GroupPackage, TLazPackage);
  RegisterIDEOptionsGroup(GroupPkgCompiler, TPkgCompilerOptions);
  PackageDependencies:=TAVLTree.Create(@ComparePkgDependencyNames);

finalization
  FreeThenNil(PackageDependencies);

end.

