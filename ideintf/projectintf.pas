{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    IDE interface to the IDE projects.
}
unit ProjectIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Controls, Forms, AvgLvlTree,
  NewItemIntf, ObjInspStrConsts, IDEOptionsIntf;

const
  FileDescGroupName = 'File';
  FileDescNamePascalUnit = 'Unit';
  FileDescNameLCLForm = 'Form';
  FileDescNameDatamodule = 'Datamodule';
  FileDescNameFrame = 'Frame';
  FileDescNameText = 'Text';

  InheritedItemsGroupName = 'Inherited Items';
  FileDescNameLCLInheritedComponent = 'Inherited Component';

  ProjDescGroupName = 'Project';
  ProjDescNameApplication = 'Application';
  ProjDescNameProgram = 'Program';
  ProjDescNameConsoleApplication = 'Console application';
  ProjDescNameLibrary = 'Library';
  ProjDescNameCustomProgram = 'Custom Program';
  ProjDescNameEmpty = 'Empty';

type
  TResourceType = (
    rtLRS,   // lazarus resources
    rtRes    // fpc resources
  );

type

  { TLazBuildMacro
    Every package/project can define build macros. A build macro has a name,
    a description, a list of possible values and a default value.
    The default value can be an expression using other build macros.
    The IDE defines basic macros like TargetOS and TargetCPU.
    The LCL package defines the macro LCLWidgetType. }

  TLazBuildMacro = class
  protected
    FDefaultValue: string;
    FIdentifier: string;
    FDescription: string;
    FValueDescriptions: TStrings;
    FValues: TStrings;
    procedure SetIdentifier(const AValue: string); virtual; abstract;
    procedure SetDescription(const AValue: string); virtual; abstract;
    procedure SetValueDescriptions(const AValue: TStrings); virtual; abstract;
    procedure SetValues(const AValue: TStrings); virtual; abstract;
  public
    procedure Assign(Source: TLazBuildMacro); virtual; abstract;
    property Identifier: string read FIdentifier write SetIdentifier;
    property Description: string read FDescription write SetDescription;
    property Values: TStrings read FValues write SetValues;
    property ValueDescriptions: TStrings read FValueDescriptions write SetValueDescriptions;
  end;

  { TLazBuildMacros
    The list of build macros of a package/project.
    They are stored in the compiler options. }

  TLazBuildMacros = class
  private
    FOwner: TObject;
  protected
    function GetItems(Index: integer): TLazBuildMacro; virtual; abstract;
  public
    constructor Create(TheOwner: TObject); virtual;
    function Add(Identifier: string): TLazBuildMacro; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure Move(OldIndex, NewIndex: integer); virtual; abstract;
    function IndexOfIdentifier(Identifier: string): integer; virtual; abstract;
    function VarWithIdentifier(Identifier: string): TLazBuildMacro; virtual; abstract;
    function Count: integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    property Items[Index: integer]: TLazBuildMacro read GetItems; default;
    property Owner: TObject read FOwner;
  end;

  { TLazCompilerOptions }

  TCompilationExecutableType = (
    cetProgram,
    cetLibrary
    );

  TCompileReason = (
    crCompile,  // normal build current project/package
    crBuild,    // build all
    crRun       // quick build before run
    );
  TCompileReasons = set of TCompileReason;
const
  crAll = [crCompile, crBuild, crRun];

type
  { TLazCompilerOptions }

  TLazCompilerOptions = class(TAbstractIDEProjectOptions)
  private
    FOnModified: TNotifyEvent;
    fOwner: TObject;
    procedure SetLCLWidgetType(const AValue: string); virtual;
  protected
    FChangeStamp: int64;
    FSavedChangeStamp: int64;

    // Paths:

    // conditionals / build modes
    FConditionals: string;
    fBuildMacros: TLazBuildMacros;
    fLCLWidgetType: string;

    // Parsing:
    // assembler style
    fAssemblerStyle: Integer;

    // syntax options
    FSyntaxMode: string;
    fCStyleOp: Boolean;
    fIncludeAssertionCode: Boolean;
    fAllowLabel: Boolean;
    fUseAnsiStr: Boolean;
    fCPPInline: Boolean;
    fCMacros: Boolean;
    fInitConst: Boolean;
    fStaticKeyword: Boolean;

    // Code generation:
    fSmartLinkUnit: Boolean;
    fIOChecks: Boolean;
    fRangeChecks: Boolean;
    fOverflowChecks: Boolean;
    fStackChecks: Boolean;
    FEmulatedFloatOpcodes: boolean;
    fHeapSize: LongInt;
    fVerifyObjMethodCall: boolean;
    FSmallerCode: boolean;
    fTargetProc: string;
    fTargetCPU: string;
    fVarsInReg: Boolean;
    fUncertainOpt: Boolean;
    fOptLevel: Integer;
    fTargetOS: String;

    // Linking:
    fGenDebugInfo: Boolean;
    fUseLineInfoUnit: Boolean;
    FGenerateDwarf: Boolean;
    fUseHeaptrc: Boolean;
    fUseValgrind: Boolean;
    fGenGProfCode: Boolean;
    fStripSymbols: Boolean;
    fLinkSmart: Boolean;
    fPassLinkerOpt: Boolean;
    fLinkerOptions: String;
    FWin32GraphicApp: boolean;
    FExecutableType: TCompilationExecutableType;
    FUseExternalDbgSyms : Boolean;
    fTargetFilename: string;

    // Messages:
    fShowErrors: Boolean;
    fShowWarn: Boolean;
    fShowNotes: Boolean;
    fShowHints: Boolean;
    fShowGenInfo: Boolean;
    fShowLineNum: Boolean;
    fShowAll: Boolean;
    fShowAllProcsOnError: Boolean;
    fShowDebugInfo: Boolean;
    fShowUsedFiles: Boolean;
    fShowTriedFiles: Boolean;
    fShowDefMacros: Boolean;
    fShowCompProc: Boolean;
    fShowCond: Boolean;
    fShowExecInfo: Boolean;
    fShowNothing: Boolean;
    fShowSummary: Boolean;
    fShowHintsForUnusedUnitsInMainSrc: Boolean;
    fShowHintsForSenderNotUsed: Boolean;
    fWriteFPCLogo: Boolean;
    fStopAfterErrCount: integer;

    // Other:
    fDontUseConfigFile: Boolean;
    fCustomConfigFile: Boolean;
    fConfigFilePath: String;
  protected
    function GetCustomOptions: string; virtual; abstract;
    function GetDebugPath: string; virtual; abstract;
    function GetIncludePaths: String; virtual; abstract;
    function GetLibraryPaths: String; virtual; abstract;
    function GetModified: boolean; virtual;
    function GetObjectPath: string; virtual; abstract;
    function GetSrcPath: string; virtual; abstract;
    function GetUnitOutputDir: string; virtual; abstract;
    function GetUnitPaths: String; virtual; abstract;
    procedure SetCompilerPath(const AValue: String); virtual; abstract;
    procedure SetConditionals(const AValue: string); virtual; abstract;
    procedure SetCustomOptions(const AValue: string); virtual; abstract;
    procedure SetDebugPath(const AValue: string); virtual; abstract;
    procedure SetIncludePaths(const AValue: String); virtual; abstract;
    procedure SetLibraryPaths(const AValue: String); virtual; abstract;
    procedure SetLinkerOptions(const AValue: String); virtual; abstract;
    procedure SetModified(const AValue: boolean); virtual; abstract;
    procedure SetObjectPath(const AValue: string); virtual; abstract;
    procedure SetSrcPath(const AValue: string); virtual; abstract;
    procedure SetTargetCPU(const AValue: string); virtual; abstract;
    procedure SetTargetFilename(const AValue: String); virtual; abstract;
    procedure SetTargetOS(const AValue: string); virtual; abstract;
    procedure SetTargetProc(const AValue: string); virtual; abstract;
    procedure SetUnitOutputDir(const AValue: string); virtual; abstract;
    procedure SetUnitPaths(const AValue: String); virtual; abstract;
  public
    constructor Create(const TheOwner: TObject); virtual;
    function IsActive: boolean; virtual;
  public
    property Owner: TObject read fOwner write fOwner;
    property Modified: boolean read GetModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property ChangeStamp: int64 read FChangeStamp;
    procedure IncreaseChangeStamp;
    class function InvalidChangeStamp: int64;

    // search paths:
    property IncludePath: String read GetIncludePaths write SetIncludePaths;
    property Libraries: String read GetLibraryPaths write SetLibraryPaths;
    property OtherUnitFiles: String read GetUnitPaths write SetUnitPaths;
    property ObjectPath: string read GetObjectPath write SetObjectPath;
    property SrcPath: string read GetSrcPath write SetSrcPath;
    property DebugPath: string read GetDebugPath write SetDebugPath;
    property UnitOutputDirectory: string read GetUnitOutputDir write SetUnitOutputDir;

    // conditional / build modes
    property Conditionals: string read FConditionals write SetConditionals;
    property BuildMacros: TLazBuildMacros read fBuildMacros;
    // Beware: eventually LCLWidgetType will be replaced by a more generic solution
    property LCLWidgetType: string read fLCLWidgetType write SetLCLWidgetType;

    // target:
    property TargetFilename: String read fTargetFilename write SetTargetFilename;

    // parsing:
    property SyntaxMode: string read FSyntaxMode write fSyntaxMode;
    property AssemblerStyle: Integer read fAssemblerStyle write fAssemblerStyle;
    property CStyleOperators: Boolean read fCStyleOp write fCStyleOp;
    property IncludeAssertionCode: Boolean
                         read fIncludeAssertionCode write fIncludeAssertionCode;
    property AllowLabel: Boolean read fAllowLabel write fAllowLabel;
    property UseAnsiStrings: Boolean read fUseAnsiStr write fUseAnsiStr;
    property CPPInline: Boolean read fCPPInline write fCPPInline;
    property CStyleMacros: Boolean read fCMacros write fCMacros;
    property InitConstructor: Boolean read fInitConst write fInitConst;
    property StaticKeyword: Boolean read fStaticKeyword write fStaticKeyword;

    // code generation:
    property IOChecks: Boolean read fIOChecks write fIOChecks;
    property RangeChecks: Boolean read fRangeChecks write fRangeChecks;
    property OverflowChecks: Boolean read fOverflowChecks write fOverflowChecks;
    property StackChecks: Boolean read fStackChecks write fStackChecks;
    property SmartLinkUnit: Boolean read fSmartLinkUnit write fSmartLinkUnit;
    property EmulatedFloatOpcodes: boolean read FEmulatedFloatOpcodes
                                           write FEmulatedFloatOpcodes;
    property HeapSize: Integer read fHeapSize write fHeapSize;
    property VerifyObjMethodCall: boolean read FVerifyObjMethodCall
                                          write FVerifyObjMethodCall;
    property SmallerCode: boolean read FSmallerCode write FSmallerCode;
    property TargetCPU: string read fTargetCPU write SetTargetCPU; // general type
    property TargetProcessor: String read fTargetProc write SetTargetProc; // specific
    property TargetOS: string read fTargetOS write SetTargetOS;
    property VariablesInRegisters: Boolean read fVarsInReg write fVarsInReg;
    property UncertainOptimizations: Boolean read fUncertainOpt write fUncertainOpt;
    property OptimizationLevel: Integer read fOptLevel write fOptLevel;

    // linking:
    property GenerateDebugInfo: Boolean read fGenDebugInfo write fGenDebugInfo;
    property UseLineInfoUnit: Boolean read fUseLineInfoUnit write fUseLineInfoUnit;
    property GenerateDwarf: Boolean read FGenerateDwarf write FGenerateDwarf;
    property UseHeaptrc: Boolean read fUseHeaptrc write fUseHeaptrc;
    property UseValgrind: Boolean read fUseValgrind write fUseValgrind;
    property GenGProfCode: Boolean read fGenGProfCode write fGenGProfCode;
    property StripSymbols: Boolean read fStripSymbols write fStripSymbols;
    property LinkSmart: Boolean read fLinkSmart write fLinkSmart;
    property PassLinkerOptions: Boolean read fPassLinkerOpt write fPassLinkerOpt;
    property LinkerOptions: String read fLinkerOptions write SetLinkerOptions;
    property Win32GraphicApp: boolean read FWin32GraphicApp write FWin32GraphicApp;
    property ExecutableType: TCompilationExecutableType
                                     read FExecutableType write FExecutableType;
    property UseExternalDbgSyms: Boolean read FUseExternalDbgSyms write FUseExternalDbgSyms;

    // messages:
    property ShowErrors: Boolean read fShowErrors write fShowErrors;
    property ShowWarn: Boolean read fShowWarn write fShowWarn;
    property ShowNotes: Boolean read fShowNotes write fShowNotes;
    property ShowHints: Boolean read fShowHints write fShowHints;
    property ShowGenInfo: Boolean read fShowGenInfo write fShowGenInfo;
    property ShowLineNum: Boolean read fShowLineNum write fShowLineNum;
    property ShowAll: Boolean read fShowAll write fShowAll;
    property ShowAllProcsOnError: Boolean
      read fShowAllProcsOnError write fShowAllProcsOnError;
    property ShowDebugInfo: Boolean read fShowDebugInfo write fShowDebugInfo;
    property ShowUsedFiles: Boolean read fShowUsedFiles write fShowUsedFiles;
    property ShowTriedFiles: Boolean read fShowTriedFiles write fShowTriedFiles;
    property ShowDefMacros: Boolean read fShowDefMacros write fShowDefMacros;
    property ShowCompProc: Boolean read fShowCompProc write fShowCompProc;
    property ShowCond: Boolean read fShowCond write fShowCond;
    property ShowExecInfo: Boolean read fShowExecInfo write fShowExecInfo;
    property ShowNothing: Boolean read fShowNothing write fShowNothing;
    property ShowSummary: Boolean read FShowSummary write FShowSummary;
    property ShowHintsForUnusedUnitsInMainSrc: Boolean
      read fShowHintsForUnusedUnitsInMainSrc write fShowHintsForUnusedUnitsInMainSrc;
    property ShowHintsForSenderNotUsed: Boolean
      read fShowHintsForSenderNotUsed write fShowHintsForSenderNotUsed;
    property WriteFPCLogo: Boolean read fWriteFPCLogo write fWriteFPCLogo;
    property StopAfterErrCount: integer
      read fStopAfterErrCount write fStopAfterErrCount;

    // other
    property DontUseConfigFile: Boolean read fDontUseConfigFile
                                        write fDontUseConfigFile;
    property CustomConfigFile: Boolean read fCustomConfigFile
                                       write fCustomConfigFile;
    property ConfigFilePath: String read fConfigFilePath write fConfigFilePath;
    property CustomOptions: string read GetCustomOptions write SetCustomOptions;
  end;


  { TLazProjectFile }

  TLazProjectFile = class(TPersistent)
  private
    FCustomData: TStringToStringTree;
    FCustomSessionData: TStringToStringTree;
    FIsPartOfProject: boolean;
  protected
    function GetFilename: string; virtual; abstract;
    procedure SetFilename(const AValue: string); virtual; abstract;
    procedure SetIsPartOfProject(const AValue: boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSourceText(const SourceText: string); virtual; abstract;
    function GetSourceText: string; virtual; abstract;
    procedure ClearModifieds; virtual; abstract;
  public
    property IsPartOfProject: boolean read FIsPartOfProject
                                      write SetIsPartOfProject;
    property Filename: string read GetFilename write SetFilename;
    property CustomData: TStringToStringTree read FCustomData;
    property CustomSessionData: TStringToStringTree read FCustomSessionData;
  end;
  TLazProjectFileClass = class of TLazProjectFile;


  { TProjectFileDescriptor

    ResourceClass: When the IDE creates a new unit of this type the IDE will
                   create a direct descendant from this class.
                   You should also register this class, so that, when the IDE
                   opens a unit with such a type
                   (i.e. 'TMyResouceClass1 = class(TMyResouceClass)')
                   it creates the correct class type. Just call somewhere once
                   RegisterClass(ResourceClass);
                   }

  TProjectFileDescriptor = class(TPersistent)
  private
    FAddToProject: boolean;
    FBuildFileIfActive: boolean;
    FDefaultFileExt: string;
    FDefaultFilename: string;
    FDefaultResFileExt: string;
    FDefaultResourceName: string;
    FDefaultSourceName: string;
    FIsComponent: boolean;
    FIsPascalUnit: boolean;
    FName: string;
    FOwner: TObject;
    FReferenceCount: integer;
    FResourceClass: TPersistentClass;
    FRequiredPackages: string;
    FRunFileIfActive: boolean;
    FUseCreateFormStatements: boolean;
    FVisibleInNewDialog: boolean;
  protected
    procedure SetDefaultFileExt(const AValue: string); virtual;
    procedure SetDefaultFilename(const AValue: string); virtual;
    procedure SetDefaultResFileExt(const AValue: string); virtual;
    procedure SetDefaultSourceName(const AValue: string); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetOwner(const AValue: TObject); virtual;
    procedure SetRequiredPackages(const AValue: string); virtual;
    procedure SetResourceClass(const AValue: TPersistentClass); virtual;
  public
    constructor Create; virtual;
    function GetLocalizedName: string; virtual;
    function GetLocalizedDescription: string; virtual;
    function GetResourceSource(const ResourceName: string): string; virtual;
    procedure Release;
    procedure Reference;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; virtual;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); virtual;
  public
    property Owner: TObject read FOwner write SetOwner; // project, package or nil
    property Name: string read FName write SetName;
    property DefaultFilename: string read FDefaultFilename write SetDefaultFilename;
    property DefaultFileExt: string read FDefaultFileExt write SetDefaultFileExt;
    property DefaultSourceName: string read FDefaultSourceName write SetDefaultSourceName;
    property DefaultResFileExt: string read FDefaultResFileExt write SetDefaultResFileExt;
    property DefaultResourceName: string read FDefaultResourceName write FDefaultResourceName;
    property ResourceClass: TPersistentClass read FResourceClass write SetResourceClass;
    property RequiredPackages: string read FRequiredPackages write SetRequiredPackages; // package names separated by semicolon
    property IsComponent: boolean read FIsComponent;
    property UseCreateFormStatements: boolean read FUseCreateFormStatements write FUseCreateFormStatements;
    property VisibleInNewDialog: boolean read FVisibleInNewDialog write FVisibleInNewDialog;
    property IsPascalUnit: boolean read FIsPascalUnit write FIsPascalUnit;
    property AddToProject: boolean read FAddToProject write FAddToProject;// only if there is choice
    property BuildFileIfActive: boolean read FBuildFileIfActive write FBuildFileIfActive;
    property RunFileIfActive: boolean read FRunFileIfActive write FRunFileIfActive;
  end;
  TProjectFileDescriptorClass = class of TProjectFileDescriptor;


  { TNewItemProjectFile - a new item for project file descriptors }

  TNewItemProjectFile = class(TNewIDEItemTemplate)
  private
    FDescriptor: TProjectFileDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TProjectFileDescriptor read FDescriptor write FDescriptor;
  end;


  { TFileDescPascalUnit }

  TFileDescPascalUnit = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; virtual;
    function GetInterfaceUsesSection: string; virtual;
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string; virtual;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; virtual;
    class function CompilerOptionsToUnitDirectives(CompOpts: TLazCompilerOptions): string;
  end;


  { TFileDescPascalUnitWithResource }

  TFileDescPascalUnitWithResource = class(TFileDescPascalUnit)
  private
    FDeclareClassVariable: Boolean;
  protected
    function GetResourceType: TResourceType; virtual;
  public
    constructor Create; override;

    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; override;

    property DeclareClassVariable: Boolean read FDeclareClassVariable write FDeclareClassVariable;
  end;


  { TProjectFileDescriptors }

  TProjectFileDescriptors = class(TPersistent)
  protected
    function GetItems(Index: integer): TProjectFileDescriptor; virtual; abstract;
  public
    function Count: integer; virtual; abstract;
    function GetUniqueName(const Name: string): string; virtual; abstract;
    function IndexOf(const Name: string): integer; virtual; abstract;
    function IndexOf(FileDescriptor: TProjectFileDescriptor): integer; virtual; abstract;
    function FindByName(const Name: string): TProjectFileDescriptor; virtual; abstract;
    procedure RegisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); virtual; abstract;
    procedure UnregisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); virtual; abstract;
  public
    property Items[Index: integer]: TProjectFileDescriptor read GetItems; default;
  end;


var
  ProjectFileDescriptors: TProjectFileDescriptors; // will be set by the IDE

function FileDescriptorUnit: TProjectFileDescriptor;
function FileDescriptorForm: TProjectFileDescriptor;
function FileDescriptorDatamodule: TProjectFileDescriptor;
function FileDescriptorText: TProjectFileDescriptor;


type
  TLazProject = class;

  { TProjectDescriptor - Template for initializing new projects }

  TProjectFlag = (
    pfSaveClosedUnits,     // save info about closed files (not part of project)
    pfSaveOnlyProjectUnits, // save no info about foreign files (not part of project)
    pfMainUnitIsPascalSource,// main unit is pascal, even it does not end in .pas/.pp
    pfMainUnitHasUsesSectionForAllUnits,// add/remove pascal units to main uses section
    pfMainUnitHasCreateFormStatements,// add/remove Application.CreateForm statements
    pfMainUnitHasTitleStatement,// add/remove Application.Title:= statements
    pfRunnable, // project can be run
    pfAlwaysBuild, // skip IDE's smart check if compilation is needed and always compile
    pfLRSFilesInOutputDirectory, // put .lrs files in output directory
    pfUseDefaultCompilerOptions // load users default compiler options
    );
  TProjectFlags = set of TProjectFlag;

  TProjectSessionStorage = (
    pssInProjectInfo, // save session info in .lpi file
    pssInProjectDir, // save session info in .lps file in project directory
    pssInIDEConfig, // save session info in IDE config directory
    pssNone         // do not save any session info
    );
  TProjectSessionStorages = set of TProjectSessionStorage;

  { TProjectDescriptor
    - to show an option dialog to the user override the DoInitDescriptor
    - to initialize project compiler settings and paths override InitProject
    - to create files on creation override CreateStartFiles
  }

  TProjectDescriptor = class(TPersistent)
  private
    FDefaultExt: string;
    FFlags: TProjectFlags;
    FName: string;
    FReferenceCount: integer;
    FVisibleInNewDialog: boolean;
  protected
    procedure SetName(const AValue: string); virtual;
    procedure SetFlags(const AValue: TProjectFlags); virtual;
    function DoInitDescriptor: TModalResult; virtual;// put here option dialogs
  public
    constructor Create; virtual;
    function GetLocalizedName: string; virtual;
    function GetLocalizedDescription: string; virtual;
    procedure Release;
    procedure Reference;
    function InitDescriptor: TModalResult;
    function InitProject(AProject: TLazProject): TModalResult; virtual;
    function CreateStartFiles(AProject: TLazProject): TModalResult; virtual;
  public
    property Name: string read FName write SetName;
    property VisibleInNewDialog: boolean read FVisibleInNewDialog
                                         write FVisibleInNewDialog;
    property Flags: TProjectFlags read FFlags write SetFlags;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
  end;
  TProjectDescriptorClass = class of TProjectDescriptor;


  { TNewItemProject - a new item for project descriptors }

  TNewItemProject = class(TNewIDEItemTemplate)
  private
    FDescriptor: TProjectDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TProjectDescriptor read FDescriptor write FDescriptor;
  end;

  { TLazProject - interface class to a Lazarus project }

  TProjectFileSearchFlag = (
    pfsfResolveFileLinks,
    pfsfOnlyEditorFiles,
    pfsfOnlyVirtualFiles,
    pfsfOnlyProjectFiles
    );
  TProjectFileSearchFlags = set of TProjectFileSearchFlag;

  TProjectExecutableType = (
    petNone,
    petProgram,
    petLibrary,
    petPackage,
    petUnit
    );

  TLazProject = class(TAbstractIDEProjectOptions)
  private
    FCustomData: TStringToStringTree;
    FCustomSessionData: TStringToStringTree;
    FExecutableType: TProjectExecutableType;
    FLazCompilerOptions: TLazCompilerOptions;
    fModified: boolean;
    FProjectSessionFile: string;
    FSessionModified: boolean;
    FTitle: String;
    FSessionStorage: TProjectSessionStorage;
    FLazDocPaths: string;
    procedure SetLazDocPaths(const AValue: string);
  protected
    FFlags: TProjectFlags;
    procedure SetLazCompilerOptions(const AValue: TLazCompilerOptions);
    function GetMainFile: TLazProjectFile; virtual; abstract;
    function GetMainFileID: Integer; virtual; abstract;
    procedure SetMainFileID(const AValue: Integer); virtual; abstract;
    function GetFiles(Index: integer): TLazProjectFile; virtual; abstract;
    procedure SetTitle(const AValue: String); virtual;
    procedure SetFlags(const AValue: TProjectFlags); virtual;
    function GetModified: boolean; virtual;
    function GetProjectInfoFile: string; virtual; abstract;
    procedure SetProjectInfoFile(const NewFilename: string); virtual; abstract;
    procedure SetProjectSessionFile(const AValue: string); virtual;
    procedure SetSessionStorage(const AValue: TProjectSessionStorage); virtual;
    procedure SetModified(const AValue: boolean); virtual;
    procedure SetSessionModified(const AValue: boolean); virtual;
    procedure SetExecutableType(const AValue: TProjectExecutableType); virtual;
  public
    constructor Create(ProjectDescription: TProjectDescriptor); virtual;
    destructor Destroy; override;
    function CreateProjectFile(const Filename: string
                               ): TLazProjectFile; virtual; abstract;
    procedure AddFile(ProjectFile: TLazProjectFile;
                      AddToProjectUsesClause: boolean); virtual; abstract;
    procedure RemoveUnit(Index: integer; RemoveFromUsesSection: boolean = true); virtual; abstract;
    function GetFileCount: integer; virtual; abstract;
    procedure AddSrcPath(const SrcPathAddition: string); virtual; abstract;
    procedure AddPackageDependency(const PackageName: string); virtual; abstract;
    function ShortDescription: string;
    procedure ClearModifieds(ClearUnits: boolean);
    function FindFile(const AFilename: string;
                      SearchFlags: TProjectFileSearchFlags): TLazProjectFile; virtual; abstract;
    procedure UpdateExecutableType; virtual; abstract;
    procedure ShortenFilename(var AFilename: string); virtual; abstract;
    procedure LongenFilename(var AFilename: string); virtual; abstract;
  public
    property MainFileID: Integer read GetMainFileID write SetMainFileID;
    property Files[Index: integer]: TLazProjectFile read GetFiles;
    property FileCount: integer read GetFileCount;
    property MainFile: TLazProjectFile read GetMainFile;
    property Title: String read FTitle write SetTitle;
    property Flags: TProjectFlags read FFlags write SetFlags;
    property ExecutableType: TProjectExecutableType read FExecutableType
                 write SetExecutableType;// read from MainFile, not saved to lpi
    property LazCompilerOptions: TLazCompilerOptions read FLazCompilerOptions
                                                     write SetLazCompilerOptions;
    property ProjectInfoFile: string
                               read GetProjectInfoFile write SetProjectInfoFile;
    property ProjectSessionFile: string
                           read FProjectSessionFile write SetProjectSessionFile;
    property SessionStorage: TProjectSessionStorage read FSessionStorage
                                                    write SetSessionStorage;
    property Modified: boolean read GetModified
                       write SetModified; // project data (not units, session),
                                          // units have their own Modified
    property SessionModified: boolean read FSessionModified
                       write SetSessionModified;
                       // project session data (not units, data),
                       // units have their own SessionModified
    property LazDocPaths: string read FLazDocPaths write SetLazDocPaths;
    property CustomData: TStringToStringTree read FCustomData;
    property CustomSessionData: TStringToStringTree read FCustomSessionData;
  end;

  TLazProjectClass = class of TLazProject;


  { TProjectDescriptors }

  TProjectDescriptors = class(TPersistent)
  protected
    function GetItems(Index: integer): TProjectDescriptor; virtual; abstract;
  public
    function Count: integer; virtual; abstract;
    function GetUniqueName(const Name: string): string; virtual; abstract;
    function IndexOf(const Name: string): integer; virtual; abstract;
    function IndexOf(Descriptor: TProjectDescriptor): integer; virtual; abstract;
    function FindByName(const Name: string): TProjectDescriptor; virtual; abstract;
    procedure RegisterDescriptor(Descriptor: TProjectDescriptor); virtual; abstract;
    procedure UnregisterDescriptor(Descriptor: TProjectDescriptor); virtual; abstract;
  public
    property Items[Index: integer]: TProjectDescriptor read GetItems; default;
  end;
  TProjectDescriptorsClass = class of TProjectDescriptors;

var
  ProjectDescriptors: TProjectDescriptors; // will be set by the IDE

function ProjectDescriptorApplication: TProjectDescriptor;
function ProjectDescriptorProgram: TProjectDescriptor;
function ProjectDescriptorConsoleApplication: TProjectDescriptor;
function ProjectDescriptorLibrary: TProjectDescriptor;
function ProjectDescriptorCustomProgram: TProjectDescriptor;
function ProjectDescriptorEmptyProject: TProjectDescriptor;

const
  DefaultProjectFlags = [pfSaveClosedUnits,
                         pfMainUnitIsPascalSource,
                         pfMainUnitHasUsesSectionForAllUnits,
                         pfMainUnitHasCreateFormStatements,
                         pfMainUnitHasTitleStatement,
                         pfRunnable,
                         pfAlwaysBuild,
                         pfLRSFilesInOutputDirectory];
  ProjectFlagNames : array[TProjectFlag] of string = (
      'SaveClosedFiles',
      'SaveOnlyProjectUnits',
      'MainUnitIsPascalSource',
      'MainUnitHasUsesSectionForAllUnits',
      'MainUnitHasCreateFormStatements',
      'MainUnitHasTitleStatement',
      'Runnable',
      'AlwaysBuild',
      'LRSInOutputDirectory',
      'UseDefaultCompilerOptions'
    );

  ProjectSessionStorageNames: array[TProjectSessionStorage] of string = (
    'InProjectInfo',
    'InProjectDir',
    'InIDEConfig',
    'None'
    );

  CompilationExecutableTypeNames: array[TCompilationExecutableType] of string =(
    'Program',
    'Library'
    );


function ProjectFlagsToStr(Flags: TProjectFlags): string;
function StrToProjectSessionStorage(const s: string): TProjectSessionStorage;
function CompilationExecutableTypeNameToType(const s: string
                                             ): TCompilationExecutableType;

procedure RegisterProjectFileDescriptor(FileDesc: TProjectFileDescriptor);
procedure RegisterProjectDescriptor(ProjDesc: TProjectDescriptor);
procedure RegisterProjectFileDescriptor(FileDesc: TProjectFileDescriptor;
                       const ACategory : String;
                       DefaultCreateFlag: TNewIDEItemFlag = niifCopy;
                       const AllowedCreateFlags: TNewIDEItemFlags = [niifCopy]);
procedure RegisterProjectDescriptor(ProjDesc: TProjectDescriptor;
                       const ACategory : String;
                       DefaultCreateFlag: TNewIDEItemFlag = niifCopy;
                       const AllowedCreateFlags: TNewIDEItemFlags = [niifCopy]);


implementation

procedure RegisterProjectFileDescriptor(FileDesc: TProjectFileDescriptor);
begin
  RegisterProjectFileDescriptor(FileDesc,FileDescGroupName);
end;

procedure RegisterProjectFileDescriptor(FileDesc: TProjectFileDescriptor;
  const ACategory : String;
  DefaultCreateFlag: TNewIDEItemFlag; const AllowedCreateFlags: TNewIDEItemFlags);
var
  NewItemFile: TNewItemProjectFile;
begin
  ProjectFileDescriptors.RegisterFileDescriptor(FileDesc);
  if FileDesc.VisibleInNewDialog then begin
    NewItemFile:=TNewItemProjectFile.Create(FileDesc.Name,
                                          DefaultCreateFlag,AllowedCreateFlags);
    NewItemFile.Descriptor:=FileDesc;
    RegisterNewDialogItem(ACategory,NewItemFile);
  end;
end;

procedure RegisterProjectDescriptor(ProjDesc: TProjectDescriptor);
begin
  RegisterProjectDescriptor(ProjDesc,ProjDescGroupName);
end;

procedure RegisterProjectDescriptor(ProjDesc: TProjectDescriptor;
  const ACategory : String;
  DefaultCreateFlag: TNewIDEItemFlag; const AllowedCreateFlags: TNewIDEItemFlags);
var
  NewItemProject: TNewItemProject;
begin
  ProjectDescriptors.RegisterDescriptor(ProjDesc);
  if ProjDesc.VisibleInNewDialog then begin
    NewItemProject:=TNewItemProject.Create(ProjDesc.Name,
                                          DefaultCreateFlag,AllowedCreateFlags);
    NewItemProject.Descriptor:=ProjDesc;
    RegisterNewDialogItem(ACategory,NewItemProject);
  end;
end;

function FileDescriptorUnit: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNamePascalUnit);
end;

function FileDescriptorForm: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameLCLForm);
end;

function FileDescriptorDatamodule: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameDatamodule);
end;

function FileDescriptorText: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameText);
end;

function ProjectDescriptorApplication: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameApplication);
end;

function ProjectDescriptorProgram: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameProgram);
end;

function ProjectDescriptorConsoleApplication: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameConsoleApplication);
end;

function ProjectDescriptorLibrary: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameLibrary);
end;

function ProjectDescriptorCustomProgram: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameCustomProgram);
end;

function ProjectDescriptorEmptyProject: TProjectDescriptor;
begin
  Result:=ProjectDescriptors.FindByName(ProjDescNameEmpty);
end;

function ProjectFlagsToStr(Flags: TProjectFlags): string;
var f: TProjectFlag;
begin
  Result:='';
  for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
    if f in Flags then begin
      if Result='' then Result:=Result+',';
      Result:=Result+ProjectFlagNames[f];
    end;
  end;
end;

function StrToProjectSessionStorage(const s: string): TProjectSessionStorage;
begin
  for Result:=Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    if CompareText(s,ProjectSessionStorageNames[Result])=0 then exit;
  Result:=pssInProjectInfo;
end;

function CompilationExecutableTypeNameToType(const s: string
  ): TCompilationExecutableType;
begin
  for Result:=Low(TCompilationExecutableType) to High(TCompilationExecutableType)
  do if CompareText(s,CompilationExecutableTypeNames[Result])=0 then exit;
  Result:=cetProgram;
end;

{ TProjectFileDescriptor }

procedure TProjectFileDescriptor.SetResourceClass(
  const AValue: TPersistentClass);
begin
  if FResourceClass=AValue then exit;
  FResourceClass:=AValue;
  FIsComponent:=(FResourceClass<>nil)
                and (FResourceClass.InheritsFrom(TComponent));
  if FResourceClass=nil then
    FDefaultResourceName:=''
  else begin
    FDefaultResourceName:=
      copy(FResourceClass.ClassName,2,length(FResourceClass.ClassName)-1)+'1';
  end;
end;

procedure TProjectFileDescriptor.SetDefaultFileExt(const AValue: string);
begin
  if FDefaultFileExt=AValue then exit;
  FDefaultFileExt:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultResFileExt(const AValue: string);
begin
  if FDefaultResFileExt=AValue then exit;
  FDefaultResFileExt:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultSourceName(const AValue: string);
begin
  if FDefaultSourceName=AValue then exit;
  FDefaultSourceName:=AValue;
end;

procedure TProjectFileDescriptor.SetRequiredPackages(const AValue: string);
begin
  if FRequiredPackages=AValue then exit;
  FRequiredPackages:=AValue;
end;

procedure TProjectFileDescriptor.SetOwner(const AValue: TObject);
begin
  if FOwner=AValue then exit;
  FOwner:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultFilename(const AValue: string);
begin
  if FDefaultFilename=AValue then exit;
  FDefaultFilename:=AValue;
  DefaultFileExt:=ExtractFileExt(FDefaultFilename);
  FIsPascalUnit:=FilenameIsPascalUnit(DefaultFileExt);
end;

procedure TProjectFileDescriptor.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor TProjectFileDescriptor.Create;
begin
  FReferenceCount:=1;
  DefaultResFileExt:='.lrs';
  AddToProject:=true;
  VisibleInNewDialog:=true;
end;

function TProjectFileDescriptor.GetLocalizedName: string;
begin
  Result:=Name;
end;

function TProjectFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=GetLocalizedName;
end;

function TProjectFileDescriptor.GetResourceSource(const ResourceName: string): string;
// This function can override the automatic creation of the .lfm file source.
begin
  Result:=''; // if empty, the IDE will create the source automatically
end;

procedure TProjectFileDescriptor.Release;
begin
  //debugln('TProjectFileDescriptor.Release A ',Name,' ',dbgs(FReferenceCount));
  if FReferenceCount=0 then
    raise Exception.Create('');
  dec(FReferenceCount);
  if FReferenceCount=0 then Free;
end;

procedure TProjectFileDescriptor.Reference;
begin
  inc(FReferenceCount);
end;

function TProjectFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='';
end;

procedure TProjectFileDescriptor.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  if DefPasExt='' then exit;
  if FilenameIsPascalUnit(DefaultFileExt) then
    DefaultFileExt:=DefPasExt;
  if FilenameIsPascalUnit(DefaultFilename) then
    DefaultFilename:=ChangeFileExt(DefaultFilename,DefPasExt);
end;

{ TFileDescPascalUnit }

constructor TFileDescPascalUnit.Create;
begin
  inherited Create;
  Name:=FileDescNamePascalUnit;
  DefaultFilename:='unit.pas';
  DefaultSourceName:='Unit1';
  IsPascalUnit:=true;
end;

function TFileDescPascalUnit.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
var
  LE: string;
begin
  LE:=LineEnding;
  Result:=
     'unit '+SourceName+';'+LE
    +LE
    +GetUnitDirectives+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+GetInterfaceUsesSection+';'+LE
    +LE
    +GetInterfaceSource(Filename,SourceName,ResourceName)
    +'implementation'+LE
    +LE
    +GetImplementationSource(Filename,SourceName,ResourceName)
    +'end.'+LE
    +LE;
end;

function TFileDescPascalUnit.GetLocalizedName: string;
begin
  Result:=pirsUnit;
end;

function TFileDescPascalUnit.GetLocalizedDescription: string;
begin
  Result:=oisCreateANewPascalUnit;
end;

function TFileDescPascalUnit.GetUnitDirectives: string;
begin
  Result:='{$mode objfpc}{$H+}';
  if Owner is TLazProject then
    Result:=CompilerOptionsToUnitDirectives(TLazProject(Owner).LazCompilerOptions);
end;

function TFileDescPascalUnit.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils';
end;

function TFileDescPascalUnit.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='';
end;

function TFileDescPascalUnit.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:='';
end;

class function TFileDescPascalUnit.CompilerOptionsToUnitDirectives(
  CompOpts: TLazCompilerOptions): string;
var
  SyntaxMode: String;
begin
  Result:='{$mode objfpc}{$H+}';
  if CompOpts=nil then exit;
  SyntaxMode:=CompOpts.SyntaxMode;
  if SyntaxMode<>'' then begin
    Result:='{$mode '+lowercase(SyntaxMode)+'}';
    if CompOpts.UseAnsiStrings
    and (SysUtils.CompareText(SyntaxMode,'Delphi')<>0) then
      Result:=Result+'{$H+}';
  end;
end;

{ TFileDescPascalUnitWithResource }

function TFileDescPascalUnitWithResource.GetResourceType: TResourceType;
begin
  Result := rtRes;
end;

constructor TFileDescPascalUnitWithResource.Create;
begin
  inherited Create;
  FDeclareClassVariable := True;
end;

function TFileDescPascalUnitWithResource.GetInterfaceUsesSection: string;
begin
  Result := inherited GetInterfaceUsesSection + ', FileUtil';
  if GetResourceType = rtLRS then
    Result := Result +', LResources';
end;

function TFileDescPascalUnitWithResource.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
var
  LE: string;
begin
  LE:=LineEnding;
  Result:=
     'type'+LE
    +'  T'+ResourceName+' = class('+ResourceClass.ClassName+')'+LE
    +'  private'+LE
    +'    { private declarations }'+LE
    +'  public'+LE
    +'    { public declarations }'+LE
    +'  end;'+LE
    +LE;

  if DeclareClassVariable then
    Result := Result +
     'var'+LE
    +'  '+ResourceName+': T'+ResourceName+';'+LE
    +LE;
end;

function TFileDescPascalUnitWithResource.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  ResourceFilename: String;
  LE: String;
begin
  LE:=LineEnding;
  case GetResourceType of
    rtLRS:
      begin
        ResourceFilename:=TrimFilename(ExtractFilenameOnly(Filename)+DefaultResFileExt);
        Result:='initialization'+LE+'  {$I '+ResourceFilename+'}'+LE+LE;
      end;
    rtRes: Result := '{$R *.lfm}'+LE+LE;
  end;
end;

{ TProjectDescriptor }

procedure TProjectDescriptor.SetFlags(const AValue: TProjectFlags);
begin
  FFlags:=AValue;
end;

function TProjectDescriptor.DoInitDescriptor: TModalResult;
begin
  Result:=mrOk;
end;

procedure TProjectDescriptor.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

constructor TProjectDescriptor.Create;
begin
  FReferenceCount:=1;
  FFlags:=DefaultProjectFlags;
  fVisibleInNewDialog:=true;
  FDefaultExt:='.pas';
end;

function TProjectDescriptor.GetLocalizedName: string;
begin
  Result:=Name;
end;

function TProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:=GetLocalizedName;
end;

procedure TProjectDescriptor.Release;
begin
  //debugln('TProjectDescriptor.Release A ',Name,' ',dbgs(FReferenceCount));
  if FReferenceCount=0 then
    raise Exception.Create('');
  dec(FReferenceCount);
  if FReferenceCount=0 then Free;
end;

procedure TProjectDescriptor.Reference;
begin
  inc(FReferenceCount);
end;

function TProjectDescriptor.InitDescriptor: TModalResult;
begin
  Result:=DoInitDescriptor;
end;

function TProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  AProject.Title:='project1';
  AProject.Flags:=Flags;
  Result:=mrOk;
end;

function TProjectDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=mrOk;
end;

{ TLazProject }

procedure TLazProject.SetFlags(const AValue: TProjectFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

procedure TLazProject.SetSessionStorage(const AValue: TProjectSessionStorage);
begin
  if FSessionStorage=AValue then exit;
  FSessionStorage:=AValue;
  Modified:=true;
end;

procedure TLazProject.SetModified(const AValue: boolean);
begin
  if fModified=AValue then exit;
  fModified:=AValue;
end;

procedure TLazProject.SetSessionModified(const AValue: boolean);
begin
  if FSessionModified=AValue then exit;
  FSessionModified:=AValue;
end;

procedure TLazProject.SetProjectSessionFile(const AValue: string);
begin
  if FProjectSessionFile=AValue then exit;
  FProjectSessionFile:=AValue;
  SessionModified:=true;
end;

procedure TLazProject.SetLazDocPaths(const AValue: string);
begin
  if FLazDocPaths=AValue then exit;
  FLazDocPaths:=AValue;
  Modified:=true;
end;

function TLazProject.GetModified: boolean;
begin
  Result:=fModified;
end;

procedure TLazProject.SetExecutableType(const AValue: TProjectExecutableType);
begin
  if FExecutableType=AValue then exit;
  FExecutableType:=AValue;
  // not saved to lpi, so do not set Modified
end;

procedure TLazProject.SetLazCompilerOptions(const AValue: TLazCompilerOptions);
begin
  if FLazCompilerOptions=AValue then exit;
  FLazCompilerOptions:=AValue;
  Modified:=true;
end;

procedure TLazProject.SetTitle(const AValue: String);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  Modified:=true;
end;

constructor TLazProject.Create(ProjectDescription: TProjectDescriptor);
begin
  inherited Create;
  FSessionStorage:=pssInProjectInfo;
  FCustomData:=TStringToStringTree.Create(true);
  FCustomSessionData:=TStringToStringTree.Create(true);
end;

destructor TLazProject.Destroy;
begin
  FreeAndNil(FCustomData);
  FreeAndNil(FCustomSessionData);
  inherited Destroy;
end;

function TLazProject.ShortDescription: string;
begin
  if Title<>'' then
    Result:=Title
  else
    Result:=ExtractFileNameOnly(ProjectInfoFile);
end;

procedure TLazProject.ClearModifieds(ClearUnits: boolean);
var
  i: Integer;
begin
  Modified:=false;
  SessionModified:=false;
  if ClearUnits then
    for i:=0 to FileCount-1 do
      Files[i].ClearModifieds;
end;

{ TLazProjectFile }

procedure TLazProjectFile.SetIsPartOfProject(const AValue: boolean);
begin
  FIsPartOfProject:=AValue;
end;

constructor TLazProjectFile.Create;
begin
  FCustomData:=TStringToStringTree.Create(true);
  FCustomSessionData:=TStringToStringTree.Create(true);
end;

destructor TLazProjectFile.Destroy;
begin
  FreeAndNil(FCustomData);
  FreeAndNil(FCustomSessionData);
  inherited Destroy;
end;

{ TLazCompilerOptions }

procedure TLazCompilerOptions.SetLCLWidgetType(const AValue: string);
begin
  if AValue=LCLWidgetType then exit;
  fLCLWidgetType:=AValue;
  IncreaseChangeStamp;
end;

function TLazCompilerOptions.GetModified: boolean;
begin
  Result:=FSavedChangeStamp=FChangeStamp;
end;

constructor TLazCompilerOptions.Create(const TheOwner: TObject);
begin
  inherited Create;
  FChangeStamp:=InvalidChangeStamp;
  FOwner := TheOwner;
end;

function TLazCompilerOptions.IsActive: boolean;
begin
  Result:=false;
end;

procedure TLazCompilerOptions.IncreaseChangeStamp;
begin
  if fChangeStamp<High(ChangeStamp) then
    inc(fChangeStamp)
  else
    fChangeStamp:=Low(int64)+1;
end;

class function TLazCompilerOptions.InvalidChangeStamp: int64;
begin
  Result:=Low(int64);
end;

{ TNewItemProjectFile }

function TNewItemProjectFile.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemProjectFile.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemProjectFile.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemProjectFile then
    FDescriptor:=TNewItemProjectFile(Source).Descriptor;
end;

{ TNewItemProject }

function TNewItemProject.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemProject.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemProject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemProject then
    FDescriptor:=TNewItemProject(Source).Descriptor;
end;

{ TLazBuildMacros }

constructor TLazBuildMacros.Create(TheOwner: TObject);
begin
  FOwner:=TheOwner
end;

initialization
  ProjectFileDescriptors:=nil;

end.


