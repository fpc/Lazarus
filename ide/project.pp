{
 /***************************************************************************
                  project.pp  -  project utility class file
                  -----------------------------------------
          TProject is responsible for managing a complete project.


              Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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

}
unit Project;

{$mode objfpc}{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

{$I ide.inc}

{off $DEFINE VerboseProjectModified}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, TypInfo, FPCAdds, LCLProc, LCLIntf, LCLType, Forms,
  Controls, Dialogs, InterfaceBase,
  Laz_XMLCfg, ExprEval, FileUtil, DefineTemplates, CodeToolManager, CodeCache,
  // IDEIntf
  PropEdits, ProjectIntf, MacroIntf, LazIDEIntf,
  // IDE
  CompOptsModes, ProjectResources, LazConf, frmCustomApplicationOptions,
  LazarusIDEStrConsts, CompilerOptions,
  TransferMacros, EditorOptions, IDEProcs, RunParamsOpts, ProjectDefs,
  FileReferenceList, EditDefineTree, PackageDefs, PackageSystem;

type
  TUnitInfo = class;
  TProject = class;

  TOnFileBackup = function(const FileToBackup: string):TModalResult of object;
  TOnUnitNameChange = procedure(AnUnitInfo: TUnitInfo;
       const OldUnitName, NewUnitName: string;
       CheckIfAllowed: boolean;
       var Allowed: boolean) of object;
  TOnLoadProjectInfo = procedure(TheProject: TProject; XMLConfig: TXMLConfig;
                                 Merge: boolean) of object;
  TOnSaveProjectInfo = procedure(TheProject: TProject;
               XMLConfig: TXMLConfig; WriteFlags: TProjectWriteFlags) of object;
  TOnProjectGetTestDirectory = procedure(TheProject: TProject;
                                         out TestDir: string) of object;
  TOnChangeProjectInfoFile = procedure(TheProject: TProject) of object;

  TOnSaveUnitSessionInfoInfo = procedure(AUnitInfo: TUnitInfo) of object;
                                 
  TUnitInfoList = (
    uilPartOfProject,
    uilWithEditorIndex,
    uilWithComponent,
    uilLoaded,
    uilAutoRevertLocked
    );
    
  TUnitCompDependencyList = (
    ucdlRequires,
    ucdlUsedBy
    );
  TUnitCompDependencyType = (
    ucdtAncestor, // RequiresUnit is ancestor
    ucdtProperty, // a property references RequiresUnit's component or sub component
    ucdtOldProperty, // like ucdtProperty, but for the old state before the revert
    ucdtInlineClass // RequiresUnit is class of an inline component
    );
  TUnitCompDependencyTypes = set of TUnitCompDependencyType;

const
  AllUnitCompDependencyTypes = [low(TUnitCompDependencyType)..high(TUnitCompDependencyType)];

type

  { TUCDComponentProperty }

  TUCDComponentProperty = class
  public
    UsedByPropPath: string;
    RequiresPropPath: string;
    constructor Create(const SrcPath, DestPath: string);
  end;

  { TUnitComponentDependency }

  TUnitComponentDependency = class
  private
    FCompProps: TFPList;// list of TUCDComponentProperty
    FRequiresUnit: TUnitInfo;
    FTypes: TUnitCompDependencyTypes;
    FUsedByUnit: TUnitInfo;
    function GetCompPropCount: integer;
    function GetCompProps(Index: integer): TUCDComponentProperty;
    procedure SetRequiresUnit(const AValue: TUnitInfo);
    procedure SetTypes(const AValue: TUnitCompDependencyTypes);
    procedure SetUsedByUnit(const AValue: TUnitInfo);
  public
    NextDependency, PrevDependency:
                     array[TUnitCompDependencyList] of TUnitComponentDependency;
    constructor Create;
    destructor Destroy; override;
    procedure ClearComponentProperties;
    function NextUsedByDependency: TUnitComponentDependency;
    function PrevUsedByDependency: TUnitComponentDependency;
    function NextRequiresDependency: TUnitComponentDependency;
    function PrevRequiresDependency: TUnitComponentDependency;
    procedure AddToList(var FirstDependency: TUnitComponentDependency;
                        ListType: TUnitCompDependencyList);
    procedure RemoveFromList(var FirstDependency: TUnitComponentDependency;
                             ListType: TUnitCompDependencyList);
    property RequiresUnit: TUnitInfo read FRequiresUnit write SetRequiresUnit;
    property UsedByUnit: TUnitInfo read FUsedByUnit write SetUsedByUnit;
    property Types: TUnitCompDependencyTypes read FTypes write SetTypes;
    property CompPropCount: integer read GetCompPropCount;
    property CompProps[Index: integer]: TUCDComponentProperty read GetCompProps;
    function FindUsedByPropPath(const UsedByPropPath: string): TUCDComponentProperty;
    function SetUsedByPropPath(const UsedByPropPath, RequiresPropPath: string
                               ): TUCDComponentProperty;
    function CreatePropPath(AComponent: TComponent;
                            const PropName: string = ''): string;
  end;

  //---------------------------------------------------------------------------

  TUnitInfoFlag = (
    uifComponentUsedByDesigner,
    uifComponentIndirectlyUsedByDesigner,
    uifMarked
    );
  TUnitInfoFlags = set of TUnitInfoFlag;
  
  { TUnitInfo }

  TUnitInfo = class(TLazProjectFile)
  private
    FAutoReferenceSourceDir: boolean;
    fAutoRevertLockCount: integer;// =0 means, codetools can auto update from disk
    fBookmarks: TFileBookmarks;
    FBuildFileIfActive: boolean;
    fComponent: TComponent;
    FComponentState: TWindowState; // state of component when we save it
    FFoldState: String;
    FResourceBaseClass: TPFComponentBaseClass;
    fComponentName: string; { classname is always T<ComponentName>
         this attribute contains the component name,
         even if the unit is not loaded,
         or the designer form is not created.
         A component can be for example a TForm or a TDataModule }
    fComponentResourceName: string;
    FComponentLastBinStreamSize: TStreamSeekType;
    FComponentLastLFMStreamSize: TStreamSeekType;
    FComponentLastLRSStreamSize: TStreamSeekType;
    fCursorPos: TPoint;
    fCustomHighlighter: boolean; // do not change highlighter on file extension change
    FDirectives: TStrings;
    fEditorIndex: integer;
    fFileName: string;
    fFileReadOnly: Boolean;
    FFirstRequiredComponent: TUnitComponentDependency;
    FFirstUsedByComponent: TUnitComponentDependency;
    FFlags: TUnitInfoFlags;
    fHasResources: boolean; // source has resource file
    FIgnoreFileDateOnDiskValid: boolean;
    FIgnoreFileDateOnDisk: longint;
    fLoaded: Boolean;  // loaded in the source editor
    FLoadingComponent: boolean;
    fModified: boolean;
    fNext, fPrev: array[TUnitInfoList] of TUnitInfo;
    fOnFileBackup: TOnFileBackup;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
    FOnUnitNameChange: TOnUnitNameChange;
    FProject: TProject;
    FRevertLockCount: integer;// >0 means IDE is currently reverting this unit
    FRunFileIfActive: boolean;
    FSessionModified: boolean;
    fSource: TCodeBuffer;
    fSyntaxHighlighter: TLazSyntaxHighlighter;
    fTopLine: integer;
    fUnitName: String;
    fUsageCount: extended;
    fUserReadOnly:  Boolean;
    fSourceChangeStep: LongInt;
    FSourceDirectoryReferenced: boolean;
    FSourceDirNeedReference: boolean;
    fLastDirectoryReferenced: string;

    function GetHasResources:boolean;
    function GetModified: boolean;
    function GetNextAutoRevertLockedUnit: TUnitInfo;
    function GetNextLoadedUnit: TUnitInfo;
    function GetNextPartOfProject: TUnitInfo;
    function GetNextUnitWithComponent: TUnitInfo;
    function GetNextUnitWithEditorIndex: TUnitInfo;
    function GetPrevAutoRevertLockedUnit: TUnitInfo;
    function GetPrevLoadedUnit: TUnitInfo;
    function GetPrevPartOfProject: TUnitInfo;
    function GetPrevUnitWithComponent: TUnitInfo;
    function GetPrevUnitWithEditorIndex: TUnitInfo;
    procedure SetAutoReferenceSourceDir(const AValue: boolean);
    procedure SetBuildFileIfActive(const AValue: boolean);
    procedure SetDirectives(const AValue: TStrings);
    procedure SetEditorIndex(const AValue: integer);
    procedure SetFileReadOnly(const AValue: Boolean);
    procedure SetComponent(const AValue: TComponent);
    procedure SetLoaded(const AValue: Boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetProject(const AValue: TProject);
    procedure SetRunFileIfActive(const AValue: boolean);
    procedure SetSessionModified(const AValue: boolean);
    procedure SetSource(ABuffer: TCodeBuffer);
    procedure SetUnitName(const NewUnitName:string);
    procedure SetUserReadOnly(const NewValue: boolean);
  protected
    function GetFileName: string; override;
    procedure SetFilename(const AValue: string); override;
    procedure SetIsPartOfProject(const AValue: boolean); override;
    procedure UpdateList(ListType: TUnitInfoList; Add: boolean);
    procedure SetInternalFilename(const NewFilename: string);
  public
    constructor Create(ACodeBuffer: TCodeBuffer);
    destructor Destroy; override;

    function ChangedOnDisk(CompareOnlyLoadSaveTime: boolean): boolean;
    function IsAutoRevertLocked: boolean;
    function IsReverting: boolean;
    function IsMainUnit: boolean;
    function IsVirtual: boolean;
    function GetDirectory: string;
    function NeedsSaveToDisk: boolean;
    function ReadOnly: boolean;
    function ReadUnitSource(ReadUnitName,Revert:boolean): TModalResult;
    function ShortFilename: string;
    function WriteUnitSource: TModalResult;
    function WriteUnitSourceToFile(const AFileName: string): TModalResult;
    procedure Clear;
    procedure ClearModifieds; override;
    procedure ClearComponentDependencies;
    procedure WriteDebugReportUnitComponentDependencies(Prefix: string);
    procedure CreateStartCode(Descriptor: TProjectFileDescriptor;
                              const NewUnitName: string);
    procedure IgnoreCurrentFileDateOnDisk;
    procedure IncreaseAutoRevertLock;
    procedure DecreaseAutoRevertLock;
    function ParseUnitNameFromSource(TryCache: boolean): string;// fetch name fom source
    procedure ReadUnitNameFromSource(TryCache: boolean);// fetch unit name from source and update property UnitName
    function CreateUnitName: string;
    procedure ImproveUnitNameCache(const NewUnitName: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                Merge, IgnoreIsPartOfProject: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              SaveData, SaveSession: boolean;
                              UsePathDelim: TPathDelimSwitch);
    procedure UpdateUsageCount(Min, IfBelowThis, IncIfBelow: extended);
    procedure UpdateUsageCount(TheUsage: TUnitUsage; const Factor: extended);
    procedure UpdateSourceDirectoryReference;

    procedure SetSourceText(const SourceText: string); override;
    function GetSourceText: string; override;

    // component dependencies
    function AddRequiresComponentDependency(RequiredUnit: TUnitInfo;
                    Types: TUnitCompDependencyTypes
                    ): TUnitComponentDependency;
    procedure RemoveRequiresComponentDependency(RequiredUnit: TUnitInfo;
                    Types: TUnitCompDependencyTypes);
    function FindComponentDependency(RequiredUnit: TUnitInfo
                                     ): TUnitComponentDependency;
    function FindRequiredComponentDependency(MinTypes: TUnitCompDependencyTypes
                                     ): TUnitComponentDependency;
    function FindUsedByComponentDependency(MinTypes: TUnitCompDependencyTypes
                                     ): TUnitComponentDependency;
    function FindAncestorUnit: TUnitInfo;
    procedure ClearUnitComponentDependencies(
                     ClearTypes: TUnitCompDependencyTypes);
  public
    { Properties }
    // Unit lists
    property NextUnitWithEditorIndex: TUnitInfo read GetNextUnitWithEditorIndex;
    property PrevUnitWithEditorIndex: TUnitInfo read GetPrevUnitWithEditorIndex;
    property NextUnitWithComponent: TUnitInfo read GetNextUnitWithComponent;
    property PrevUnitWithComponent: TUnitInfo read GetPrevUnitWithComponent;
    property NextLoadedUnit: TUnitInfo read GetNextLoadedUnit;
    property PrevLoadedUnit: TUnitInfo read GetPrevLoadedUnit;
    property NextAutoRevertLockedUnit: TUnitInfo read GetNextAutoRevertLockedUnit;
    property PrevAutoRevertLockedUnit: TUnitInfo read GetPrevAutoRevertLockedUnit;
    property NextPartOfProject: TUnitInfo read GetNextPartOfProject;
    property PrevPartOfProject: TUnitInfo read GetPrevPartOfProject;
  public
    property Bookmarks: TFileBookmarks read FBookmarks write FBookmarks;
    property BuildFileIfActive: boolean read FBuildFileIfActive
                                        write SetBuildFileIfActive;
    property Component: TComponent read fComponent write SetComponent;
    property ComponentName: string read fComponentName write fComponentName;
    property ComponentResourceName: string read fComponentResourceName
                                           write fComponentResourceName;
    property ComponentState: TWindowState read FComponentState write FComponentState;
    property ResourceBaseClass: TPFComponentBaseClass read FResourceBaseClass
                                                      write FResourceBaseClass;
    property ComponentLastBinStreamSize: TStreamSeekType
             read FComponentLastBinStreamSize write FComponentLastBinStreamSize;
    property ComponentLastLRSStreamSize: TStreamSeekType
             read FComponentLastLRSStreamSize write FComponentLastLRSStreamSize;
    property ComponentLastLFMStreamSize: TStreamSeekType
             read FComponentLastLFMStreamSize write FComponentLastLFMStreamSize;
    property CursorPos: TPoint read fCursorPos write fCursorPos; // physical (screen) position
    property CustomHighlighter: boolean
                               read fCustomHighlighter write fCustomHighlighter;
    property Directives: TStrings read FDirectives write SetDirectives;
    property EditorIndex: integer read fEditorIndex write SetEditorIndex;
    property FileReadOnly: Boolean read fFileReadOnly write SetFileReadOnly;
    property FirstRequiredComponent: TUnitComponentDependency
                                                   read FFirstRequiredComponent;
    property FirstUsedByComponent: TUnitComponentDependency
                                                     read FFirstUsedByComponent;
    property Flags: TUnitInfoFlags read FFlags write FFlags;
    property HasResources: boolean read GetHasResources write fHasResources;
    property Loaded: Boolean read fLoaded write SetLoaded;
    property LoadingComponent: boolean read FLoadingComponent write FLoadingComponent;
    property Modified: boolean read GetModified write SetModified;// not Session data
    property SessionModified: boolean read FSessionModified write SetSessionModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnLoadSaveFilename: TOnLoadSaveFilename
                             read fOnLoadSaveFilename write fOnLoadSaveFilename;
    property OnUnitNameChange: TOnUnitNameChange
                                 read FOnUnitNameChange write FOnUnitNameChange;
    property Project: TProject read FProject write SetProject;
    property RunFileIfActive: boolean read FRunFileIfActive write SetRunFileIfActive;
    property Source: TCodeBuffer read fSource write SetSource;
    property FoldState: String read FFoldState write FFoldState;
    property SyntaxHighlighter: TLazSyntaxHighlighter
                               read fSyntaxHighlighter write fSyntaxHighlighter;
    property TopLine: integer read fTopLine write fTopLine;
    property AUnitName: String read fUnitName write SetUnitName;
    property UserReadOnly: Boolean read fUserReadOnly write SetUserReadOnly;
    property SourceDirectoryReferenced: boolean read FSourceDirectoryReferenced;
    property AutoReferenceSourceDir: boolean read FAutoReferenceSourceDir
                                             write SetAutoReferenceSourceDir;
  end;


  //---------------------------------------------------------------------------

  { TProjectCompilationToolOptions }

  TProjectCompilationToolOptions = class(TCompilationToolOptions)
  public
    CompileReasons: TCompileReasons;
    DefaultCompileReasons: TCompileReasons;
    procedure Clear; override;
    procedure CreateDiff(CompOpts: TCompilationToolOptions;
                         Tool: TCompilerDiffTool); override;
    procedure Assign(Src: TCompilationToolOptions); override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                DoSwitchPathDelims: boolean); override;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              UsePathDelim: TPathDelimSwitch); override;
  end;
  
  { TProjectCompilerOptions }

  TProjectCompilerOptions = class(TBaseCompilerOptions)
  private
    FBuildModes: TBuildModeGraph;
    FGlobals: TGlobalCompilerOptions;
    FOwnerProject: TProject;
    FCompileReasons: TCompileReasons;
  protected
    procedure LoadTheCompilerOptions(const APath: string); override;
    procedure SaveTheCompilerOptions(const APath: string); override;

    procedure SetTargetCPU(const AValue: string); override;
    procedure SetTargetOS(const AValue: string); override;
    procedure SetCustomOptions(const AValue: string); override;
    procedure SetIncludePaths(const AValue: string); override;
    procedure SetLibraryPaths(const AValue: string); override;
    procedure SetLinkerOptions(const AValue: string); override;
    procedure SetObjectPath(const AValue: string); override;
    procedure SetSrcPath(const AValue: string); override;
    procedure SetUnitPaths(const AValue: string); override;
    procedure SetUnitOutputDir(const AValue: string); override;
    procedure UpdateGlobals; virtual;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure Clear; override;
    function GetOwnerName: string; override;
    function GetDefaultMainSourceFileName: string; override;
    procedure GetInheritedCompilerOptions(var OptionsList: TFPList); override;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(CompOpts: TBaseCompilerOptions): boolean; override;
    procedure CreateDiff(CompOpts: TBaseCompilerOptions;
                         Tool: TCompilerDiffTool); override;
    procedure InvalidateOptions;
    function GetEffectiveLCLWidgetType: string; override;
  public
    property OwnerProject: TProject read FOwnerProject;
    property Project: TProject read FOwnerProject;
    property Globals: TGlobalCompilerOptions read FGlobals;
    property BuildModes: TBuildModeGraph read FBuildModes;
  published
    property CompileReasons: TCompileReasons read FCompileReasons write FCompileReasons;
  end;
  
  
  { TProjectDefineTemplates }

  TProjectDefineTemplatesFlag = (
    ptfFlagsChanged,
    ptfIDChanged,
    ptfSourceDirsChanged,
    ptfOutputDirChanged,
    ptfCustomDefinesChanged
    );
  TProjectDefineTemplatesFlags = set of TProjectDefineTemplatesFlag;

  TProjectDefineTemplates = class
  private
    FActive: boolean;
    FSrcDirectories: TDefineTemplate;
    FSrcDirIfDef: TDefineTemplate;
    FFlags: TProjectDefineTemplatesFlags;
    FMain: TDefineTemplate;
    FOutputDir: TDefineTemplate;
    FOutPutSrcPath: TDefineTemplate;
    FOwnerProject: TProject;
    FUpdateLock: integer;
    fLastSourceDirectories: TStringList;
    fLastOutputDirSrcPathIDAsString: string;
    fLastSourceDirsIDAsString: string;
    fLastSourceDirStamp: integer;
    FLastCustomOptions: string;
    fLastUnitPath: string;
    procedure SetActive(const AValue: boolean);
    procedure UpdateMain;
    procedure UpdateSrcDirIfDef;
    procedure UpdateDefinesForOutputDirectory;
    procedure UpdateSourceDirectories;
    procedure UpdateDefinesForCustomDefines;
  public
    constructor Create(OwnerProject: TProject);
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AllChanged;
    procedure ProjectIDChanged;
    procedure SourceDirectoriesChanged;// a source directory was added/deleted
    procedure CustomDefinesChanged;// the defines of the source dirs changed
    procedure OutputDirectoryChanged;// the path or the defines of the output dir changed
    procedure UpdateGlobalValues;
  public
    property Owner: TProject read FOwnerProject;
    property Project: TProject read FOwnerProject;
    property Main: TDefineTemplate read FMain;
    property SrcDirectories: TDefineTemplate read FSrcDirectories;
    property OutputDir: TDefineTemplate read FOutputDir;
    property OutPutSrcPath: TDefineTemplate read FOutPutSrcPath;
    property CustomDefines: TDefineTemplate read FSrcDirIfDef;
    property Active: boolean read FActive write SetActive;
  end;


  //----------------------------------------------------------------------------
  
  { TProjectApplicationDescriptor }

  TProjectApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectProgramDescriptor }

  TProjectProgramDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectConsoleApplicationDescriptor }

  TProjectConsoleApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectLibraryDescriptor }

  TProjectLibraryDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectManualProgramDescriptor }

  TProjectManualProgramDescriptor = class(TProjectDescriptor)
  private
    FAddMainSource: boolean;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property AddMainSource: boolean read FAddMainSource write FAddMainSource;
  end;

  { TProjectEmptyProgramDescriptor }

  TProjectEmptyProgramDescriptor = class(TProjectManualProgramDescriptor)
  public
    constructor Create; override;
  end;

  { TProject }
  
  TEndUpdateProjectEvent =
    procedure(Sender: TObject; ProjectChanged: boolean) of object;
    
  TLazProjectStateFlag = (
    lpsfStateFileLoaded,
    lpsfPropertyDependenciesChanged,
    lpsfDesignerChanged
    );
  TLazProjectStateFlags = set of TLazProjectStateFlag;
    
  { TProject }

  TProject = class(TLazProject)
  private
    fActiveEditorIndexAtStart: integer;
    FAutoCreateForms: boolean;
    FAutoOpenDesignerFormsDisabled: boolean;
    FBookmarks: TProjectBookmarkList;
    fChanged: boolean;
    FCompilerOptions: TProjectCompilerOptions;
    fCurStorePathDelim: TPathDelimSwitch; // used by OnLoadSaveFilename
    FDefineTemplates: TProjectDefineTemplates;
    fDestroying: boolean;
    FEnableI18N: boolean;
    fFirst: array[TUnitInfoList] of TUnitInfo;
    FFirstRemovedDependency: TPkgDependency;
    FFirstRequiredDependency: TPkgDependency;
    FJumpHistory: TProjectJumpHistory;
    FLastCompilerFileDate: integer;
    FLastCompilerFilename: string;
    FLastCompilerParams: string;
    fLastReadLPIFileDate: TDateTime;
    fLastReadLPIFilename: string;
    FLockUnitComponentDependencies: integer;
    FMainProject: boolean;
    fMainUnitID: Integer;
    FOnBeginUpdate: TNotifyEvent;
    FOnChangeProjectInfoFile: TOnChangeProjectInfoFile;
    FOnEndUpdate: TEndUpdateProjectEvent;
    fOnFileBackup: TOnFileBackup;
    FOnGetTestDirectory: TOnProjectGetTestDirectory;
    FOnLoadProjectInfo: TOnLoadProjectInfo;
    FOnSaveProjectInfo: TOnSaveProjectInfo;
    FOnSaveUnitSessionInfo: TOnSaveUnitSessionInfoInfo;
    fPathDelimChanged: boolean; // PathDelim in system and current config differ (see StorePathDelim and SessionStorePathDelim)
    FPOOutputDirectory: string;
    fProjectDirectory: string;
    fProjectDirectoryReferenced: string;
    fProjectInfoFile: String;  // the lpi filename
    FPublishOptions: TPublishProjectOptions;
    FResources: TProjectResources;
    FRevertLockCount: integer;
    FRunParameterOptions: TRunParamsOptions;
    FSessionStorePathDelim: TPathDelimSwitch;
    FSkipCheckLCLInterfaces: boolean;
    FSourceDirectories: TFileReferenceList;
    FStateFileDate: longint;
    FStateFlags: TLazProjectStateFlags;
    FStorePathDelim: TPathDelimSwitch;
    FTargetFileExt: String;
    FUnitList: TFPList;  // list of _all_ units (TUnitInfo)
    FUpdateLock: integer;
    FUseAppBundle: Boolean;
    function GetFirstAutoRevertLockedUnit: TUnitInfo;
    function GetFirstLoadedUnit: TUnitInfo;
    function GetFirstPartOfProject: TUnitInfo;
    function GetFirstUnitWithComponent: TUnitInfo;
    function GetFirstUnitWithEditorIndex: TUnitInfo;
    function GetMainFilename: String;
    function GetMainUnitInfo: TUnitInfo;
    function GetTargetFilename: string;
    function GetUnits(Index: integer): TUnitInfo;
    function JumpHistoryCheckPosition(
                                APosition:TProjectJumpHistoryPosition): boolean;
    function OnUnitFileBackup(const Filename: string): TModalResult;
    procedure OnLoadSaveFilename(var AFilename: string; Load: boolean);
    procedure OnUnitNameChange(AnUnitInfo: TUnitInfo;
                               const OldUnitName, NewUnitName: string;
                               CheckIfAllowed: boolean; var Allowed: boolean);
    procedure SetAutoOpenDesignerFormsDisabled(const AValue: boolean);
    procedure SetCompilerOptions(const AValue: TProjectCompilerOptions);
    procedure SetMainProject(const AValue: boolean);
    procedure SetSkipCheckLCLInterfaces(const AValue: boolean);
    procedure SetTargetFilename(const NewTargetFilename: string);
    procedure SetEnableI18N(const AValue: boolean);
    procedure SetPOOutputDirectory(const AValue: string);
    procedure SetMainUnitID(const AValue: Integer);
    procedure UpdateProjectDirectory;
    procedure UpdateSessionFilename;
    procedure UpdateSourceDirectories;
    procedure ClearSourceDirectories;
    procedure SourceDirectoriesChanged(Sender: TObject);
    procedure EmbeddedObjectModified(Sender: TObject);
    procedure FixEditorIndex;
  protected
    function GetMainFile: TLazProjectFile; override;
    function GetMainFileID: Integer; override;
    procedure SetMainFileID(const AValue: Integer); override;
    function GetFiles(Index: integer): TLazProjectFile; override;
    procedure SetFlags(const AValue: TProjectFlags); override;
    function GetProjectInfoFile: string; override;
    procedure SetProjectInfoFile(const NewFilename: string); override;
    procedure SetSessionStorage(const AValue: TProjectSessionStorage); override;
    procedure SetModified(const AValue: boolean); override;
    procedure SetSessionModified(const AValue: boolean); override;
    procedure SetExecutableType(const AValue: TProjectExecutableType); override;
  protected
    // special unit lists
    procedure AddToList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);
    procedure RemoveFromList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);

    procedure AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromComponentList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromPartOfProjectList(AnUnitInfo: TUnitInfo);
  public
    constructor Create(ProjectDescription: TProjectDescriptor); override;
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;
    procedure UnitModified(AnUnitInfo: TUnitInfo);
    function NeedsDefineTemplates: boolean;
    procedure BeginRevertUnit(AnUnitInfo: TUnitInfo);
    procedure EndRevertUnit(AnUnitInfo: TUnitInfo);
    function IsReverting(AnUnitInfo: TUnitInfo): boolean;

    // load/save
    function IsVirtual: boolean;
    function SomethingModified(CheckData, CheckSession: boolean): boolean;
    procedure MainSourceFilenameChanged;
    procedure GetUnitsChangedOnDisk(var AnUnitList: TFPList);
    function ReadProject(const NewProjectInfoFile: string): TModalResult;
    function WriteProject(ProjectWriteFlags: TProjectWriteFlags;
                          const OverrideProjectInfoFile: string): TModalResult;
    procedure UpdateExecutableType; override;

    // title
    function GetDefaultTitle: string;
    function TitleIsDefault(Fuzzy: boolean = false): boolean;
    function IDAsString: string;
    function IDAsWord: string;

    // units
    function UnitCount:integer;
    function GetFileCount: integer; override;
    function NewUniqueUnitName(const AnUnitName: string): string;
    function NewUniqueComponentName(const AComponentPrefix: string): string;
    function NewUniqueFilename(const Filename: string): string;
    procedure AddFile(ProjectFile: TLazProjectFile;
                      AddToProjectUsesClause: boolean); override;
    procedure RemoveUnit(Index: integer;
                         RemoveFromUsesSection: boolean = true); override;
    procedure RemoveNonExistingFiles(RemoveFromUsesSection: boolean = true);
    function CreateProjectFile(const Filename: string): TLazProjectFile; override;

    // search
    function IndexOf(AUnitInfo: TUnitInfo): integer;
    function IndexOfUnitWithName(const AnUnitName: string;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithComponent(AComponent: TComponent;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithComponentName(const AComponentName: string;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfFilename(const AFilename: string): integer;
    function IndexOfFilename(const AFilename: string;
                             SearchFlags: TProjectFileSearchFlags): integer;
    function ProjectUnitWithFilename(const AFilename: string): TUnitInfo;
    function ProjectUnitWithShortFilename(const ShortFilename: string): TUnitInfo;
    function ProjectUnitWithUnitname(const AnUnitName: string): TUnitInfo;
    function UnitWithEditorIndex(Index:integer): TUnitInfo;
    function UnitWithComponent(AComponent: TComponent): TUnitInfo;
    function UnitWithComponentClass(AClass: TComponentClass): TUnitInfo;
    function UnitWithComponentClassName(const AClassName: string): TUnitInfo;
    function UnitWithComponentName(AComponentName: String): TUnitInfo;
    function UnitComponentInheritingFrom(AClass: TComponentClass;
                                         Ignore: TUnitInfo): TUnitInfo;
    function UnitUsingComponentUnit(ComponentUnit: TUnitInfo;
                                    Types: TUnitCompDependencyTypes): TUnitInfo;
    function UnitComponentIsUsed(ComponentUnit: TUnitInfo;
                                 CheckHasDesigner: boolean): boolean;
    function UnitInfoWithFilename(const AFilename: string): TUnitInfo;
    function UnitInfoWithFilename(const AFilename: string;
                    SearchFlags: TProjectFileSearchFlags): TUnitInfo;
    function UnitWithUnitname(const AnUnitname: string): TUnitInfo;
    function SearchFile(const ShortFilename: string;
                        SearchFlags: TSearchIDEFileFlags): TUnitInfo;
    function FindFile(const AFilename: string;
                      SearchFlags: TProjectFileSearchFlags): TLazProjectFile; override;

    // units in editor
    procedure CloseEditorIndex(EditorIndex:integer);
    procedure InsertEditorIndex(EditorIndex:integer);
    procedure MoveEditorIndex(OldEditorIndex, NewEditorIndex: integer);

    // Application.CreateForm statements
    function AddCreateFormToProjectFile(const AClassName, AName:string):boolean;
    function RemoveCreateFormFromProjectFile(const AClassName,
                                                         AName: string):boolean;
    function FormIsCreatedInProjectFile(const AClassname, AName:string):boolean;
    
    // uses section
    function UnitIsUsed(const ShortUnitName:string):boolean;
    
    // resources
    function GetMainResourceFilename(AnUnitInfo: TUnitInfo): string;
    function GetResourceFile(AnUnitInfo: TUnitInfo; Index:integer):TCodeBuffer;

    // filenames and fileinfo
    function RemoveProjectPathFromFilename(const AFilename: string): string;
    function FileIsInProjectDir(const AFilename: string): boolean;
    procedure GetVirtualDefines(DefTree: TDefineTree; DirDef: TDirectoryDefines);
    function SearchFile(const Filename,SearchPaths,InitialDir:string):string;
    procedure ShortenFilename(var AFilename: string); override;
    procedure LongenFilename(var AFilename: string); override;

    // bookmarks
    procedure SetBookmark(AnUnitInfo: TUnitInfo; X,Y,ID: integer);
    procedure MergeBookmarks(AnUnitInfo: TUnitInfo);
    
    // package dependencies
    function FindDependencyByName(const PackageName: string): TPkgDependency;
    function RequiredDepByIndex(Index: integer): TPkgDependency;
    function RemovedDepByIndex(Index: integer): TPkgDependency;
    procedure AddRequiredDependency(Dependency: TPkgDependency);
    procedure RemoveRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRemovedDependency(Dependency: TPkgDependency);
    procedure RemoveRemovedDependency(Dependency: TPkgDependency);
    procedure ReaddRemovedDependency(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyUp(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyDown(Dependency: TPkgDependency);
    function Requires(APackage: TLazPackage; SearchRecursively: boolean): boolean;
    procedure GetAllRequiredPackages(var List: TFPList);
    procedure AddPackageDependency(const PackageName: string); override;
    
    // unit dependencies
    procedure LockUnitComponentDependencies;
    procedure UnlockUnitComponentDependencies;
    procedure UpdateUnitComponentDependencies;
    procedure InvalidateUnitComponentDesignerDependencies;
    procedure ClearUnitComponentDependencies(
                   ClearTypes: TUnitCompDependencyTypes);
    procedure FindUnitsUsingSubComponent(SubComponent: TComponent;
                     List: TFPList; IgnoreOwner: boolean);
    procedure WriteDebugReportUnitComponentDependencies(Prefix: string);

    // paths
    procedure AddSrcPath(const SrcPathAddition: string); override;
    function GetSourceDirs(WithProjectDir, WithoutOutputDir: boolean): string;
    function GetOutputDirectory: string;
    function GetCompilerFilename: string;
    function GetStateFilename: string;
    function GetTestDirectory: string;
    function GetCompileSourceFilename: string;
    procedure AutoAddOutputDirToIncPath;
    
    // state file
    function LoadStateFile(IgnoreErrors: boolean): TModalResult;
    function SaveStateFile(const CompilerFilename, CompilerParams: string
                           ): TModalResult;
                           
    // source editor
    procedure UpdateAllCustomHighlighter;
    procedure UpdateAllSyntaxHighlighter;
    
    // i18n
    function GetPOOutDirectory: string;
  public
    property ActiveEditorIndexAtStart: integer read fActiveEditorIndexAtStart
                                               write fActiveEditorIndexAtStart;
    property AutoCreateForms: boolean
                                   read FAutoCreateForms write FAutoCreateForms;
    property AutoOpenDesignerFormsDisabled: boolean
                                         read FAutoOpenDesignerFormsDisabled
                                         write SetAutoOpenDesignerFormsDisabled;
    property Bookmarks: TProjectBookmarkList read FBookmarks write FBookmarks;
    property SkipCheckLCLInterfaces: boolean read FSkipCheckLCLInterfaces
                                             write SetSkipCheckLCLInterfaces;
    property CompilerOptions: TProjectCompilerOptions
                                 read FCompilerOptions write SetCompilerOptions;
    property DefineTemplates: TProjectDefineTemplates read FDefineTemplates;
    property Destroying: boolean read fDestroying;
    property EnableI18N: boolean read FEnableI18N write SetEnableI18N;
    property FirstAutoRevertLockedUnit: TUnitInfo read GetFirstAutoRevertLockedUnit;
    property FirstLoadedUnit: TUnitInfo read GetFirstLoadedUnit;
    property FirstPartOfProject: TUnitInfo read GetFirstPartOfProject;
    property FirstRemovedDependency: TPkgDependency
                                                   read FFirstRemovedDependency;
    property FirstRequiredDependency: TPkgDependency
                                                  read FFirstRequiredDependency;
    property FirstUnitWithComponent: TUnitInfo read GetFirstUnitWithComponent;
    property FirstUnitWithEditorIndex: TUnitInfo read GetFirstUnitWithEditorIndex;
    property JumpHistory: TProjectJumpHistory
                                           read FJumpHistory write FJumpHistory;
    property LastCompilerFileDate: integer read FLastCompilerFileDate
                                          write FLastCompilerFileDate;
    property LastCompilerFilename: string read FLastCompilerFilename
                                          write FLastCompilerFilename;
    property LastCompilerParams: string read FLastCompilerParams
                                        write FLastCompilerParams;
    property MainFilename: String read GetMainFilename;
    property MainProject: boolean read FMainProject write SetMainProject;
    property MainUnitID: Integer read FMainUnitID write SetMainUnitID;
    property MainUnitInfo: TUnitInfo read GetMainUnitInfo;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnChangeProjectInfoFile: TOnChangeProjectInfoFile read FOnChangeProjectInfoFile
                                                 write FOnChangeProjectInfoFile;
    property OnEndUpdate: TEndUpdateProjectEvent read FOnEndUpdate write FOnEndUpdate;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnGetTestDirectory: TOnProjectGetTestDirectory
                             read FOnGetTestDirectory write FOnGetTestDirectory;
    property OnLoadProjectInfo: TOnLoadProjectInfo read FOnLoadProjectInfo
                                                   write FOnLoadProjectInfo;
    property OnSaveProjectInfo: TOnSaveProjectInfo read FOnSaveProjectInfo
                                                   write FOnSaveProjectInfo;
    property OnSaveUnitSessionInfo: TOnSaveUnitSessionInfoInfo
      read FOnSaveUnitSessionInfo write FOnSaveUnitSessionInfo;
    property POOutputDirectory: string read FPOOutputDirectory
                                       write SetPOOutputDirectory;
    property ProjectDirectory: string read fProjectDirectory;
    property ProjectInfoFile: string
                               read GetProjectInfoFile write SetProjectInfoFile;
    property PublishOptions: TPublishProjectOptions
                                     read FPublishOptions write FPublishOptions;
    property Resources: TProjectResources read FResources;

    property RunParameterOptions: TRunParamsOptions read FRunParameterOptions;
    property SourceDirectories: TFileReferenceList read FSourceDirectories;
    property StateFileDate: longint read FStateFileDate write FStateFileDate;
    property StateFlags: TLazProjectStateFlags read FStateFlags write FStateFlags;
    property SessionStorePathDelim: TPathDelimSwitch read FSessionStorePathDelim write FSessionStorePathDelim;
    property StorePathDelim: TPathDelimSwitch read FStorePathDelim write FStorePathDelim;
    property TargetFileExt: String read FTargetFileExt write FTargetFileExt;
    property TargetFilename: string
                                 read GetTargetFilename write SetTargetFilename;
    property Units[Index: integer]: TUnitInfo read GetUnits;
    property UpdateLock: integer read FUpdateLock;
    property UseAppBundle: Boolean read FUseAppBundle write FUseAppBundle;
  end;

  
const
  ResourceFileExt = '.lrs';

var
  Project1: TProject = nil;// the main project
  
const
  UnitInfoFlagNames: array[TUnitInfoFlag] of string = (
    'uifComponentUsedByDesigner',
    'uifComponentIndirectlyUsedByDesigner',
    'uifMarked'
    );

procedure AddCompileReasonsDiff(Tool: TCompilerDiffTool;
  const PropertyName: string; const Old, New: TCompileReasons);
function dbgs(aType: TUnitCompDependencyType): string; overload;
function dbgs(Types: TUnitCompDependencyTypes): string; overload;
function dbgs(Flag: TUnitInfoFlag): string; overload;
function dbgs(Flags: TUnitInfoFlags): string; overload;

function CompareUnitInfoWithEditorIndex(Unit1, Unit2: TUnitInfo): integer;


implementation

const
  ProjectInfoFileVersion = 7;

procedure AddCompileReasonsDiff(Tool: TCompilerDiffTool;
  const PropertyName: string; const Old, New: TCompileReasons);
begin
  if Old=New then exit;
  Tool.AddSetDiff(PropertyName,integer(Old),integer(New),
                  PString(@CompileReasonNames[Low(TCompileReasons)]));
end;

function dbgs(aType: TUnitCompDependencyType): string;
begin
  case aType of
  ucdtAncestor: Result:='Ancestor';
  ucdtProperty: Result:='Property';
  ucdtOldProperty: Result:='OldProperty';
  ucdtInlineClass: Result:='InlineClass';
  else Result:='?'
  end;
end;

function dbgs(Types: TUnitCompDependencyTypes): string;
var
  t: TUnitCompDependencyType;
begin
  Result:='';
  for t:=low(Types) to High(Types) do
    if t in Types then begin
      if Result<>'' then Result:=Result+';';
      Result:=Result+dbgs(t);
    end;
  Result:='['+Result+']';
end;

function dbgs(Flag: TUnitInfoFlag): string;
begin
  Result:=UnitInfoFlagNames[Flag];
end;

function dbgs(Flags: TUnitInfoFlags): string;
var
  f: TUnitInfoFlag;
begin
  Result:='';
  for f:=low(Flags) to High(Flags) do
    if f in Flags then begin
      if Result<>'' then Result:=Result+';';
      Result:=Result+dbgs(f);
    end;
  Result:='['+Result+']';
end;

function CompareUnitInfoWithEditorIndex(Unit1, Unit2: TUnitInfo): integer;
begin
  Result:=Unit1.EditorIndex-Unit2.EditorIndex;
end;

{------------------------------------------------------------------------------
  TUnitInfo Constructor
 ------------------------------------------------------------------------------}
constructor TUnitInfo.Create(ACodeBuffer: TCodeBuffer);
begin
  inherited Create;
  Assert(False, 'Trace:Project Unit Info Class Created');
  FBookmarks:=TFileBookmarks.Create;
  Clear;
  Source := ACodeBuffer;
  if Source=nil then
    FFileName:='';
end;

{------------------------------------------------------------------------------
  TUnitInfo Destructor
 ------------------------------------------------------------------------------}
destructor TUnitInfo.Destroy;
begin
  Component:=nil;
  Source:=nil;
  FreeAndNil(FBookmarks);
  Project:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TUnitInfo WriteUnitSource
 ------------------------------------------------------------------------------}
function TUnitInfo.WriteUnitSource: TModalResult;
var
  ACaption:string;
  AText:string;
begin
  if fSource=nil then begin
    Result:=mrOk;
    exit;
  end;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(Filename);
    if Result=mrAbort then exit;
  end;
  repeat
    if not fSource.Save then begin
      ACaption:='Write error';
      AText:='Unable to write file "'+Filename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end else begin
      Result:=mrOk;
      FIgnoreFileDateOnDiskValid:=true;
    end;
  until Result<>mrRetry;
  Result:=mrOk;
end;

function TUnitInfo.WriteUnitSourceToFile(const AFileName: string): TModalResult;
var
  ACaption:string;
  AText:string;
begin
  if fSource=nil then begin
    Result:=mrOk;
    exit;
  end;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(AFilename);
    if Result=mrAbort then exit;
  end;
  repeat
    if not fSource.SaveToFile(AFileName) then begin
      ACaption:='Write error';
      AText:='Unable to write file "'+AFilename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end else
      Result:=mrOk;
  until Result<>mrRetry;
  Result:=mrOk;
end;

{------------------------------------------------------------------------------
  TUnitInfo ReadUnitSource
 ------------------------------------------------------------------------------}
function TUnitInfo.ReadUnitSource(ReadUnitName,Revert:boolean): TModalResult;
var 
  ACaption:string;
  AText:string;
  NewSource: TCodeBuffer;
begin
  repeat
    NewSource:=CodeToolBoss.LoadFile(Filename,true,Revert);
    if NewSource=nil then begin
      ACaption:=lisCodeToolsDefsReadError;
      AText:=Format(lisUnableToReadFile2, ['"', Filename, '"']);
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result in [mrAbort,mrIgnore] then
        exit;
    end else begin
      Source:=NewSource;
      FIgnoreFileDateOnDiskValid:=true;
      Result:=mrOk;
    end;
  until Result<>mrRetry;
  if ReadUnitName then begin
    fUnitName:=CodeToolBoss.GetSourceName(fSource,false);
  end;
  Result:=mrOk;
end;

procedure TUnitInfo.ReadUnitNameFromSource(TryCache: boolean);
var
  NewUnitName: String;
begin
  NewUnitName:=ParseUnitNameFromSource(TryCache);
  if NewUnitName<>'' then
    fUnitName:=NewUnitName;
end;

function TUnitInfo.CreateUnitName: string;
begin
  Result:=AUnitName;
  if (Result='') and FilenameIsPascalSource(Filename) then
    Result:=ExtractFilenameOnly(Filename);
end;

procedure TUnitInfo.ImproveUnitNameCache(const NewUnitName: string);
begin
  if (fUnitName='') or (CompareText(fUnitName,NewUnitName)=0) then begin
    fUnitName:=NewUnitName;
  end;
end;

{------------------------------------------------------------------------------
  TUnitInfo Clear
 ------------------------------------------------------------------------------}
procedure TUnitInfo.Clear;
begin
  FBookmarks.Clear;
  FBuildFileIfActive:=false;
  fComponent := nil;
  fComponentName := '';
  fComponentResourceName := '';
  FComponentState := wsNormal;
  fCursorPos.X := -1;
  fCursorPos.Y := -1;
  fCustomHighlighter := false;
  fEditorIndex := -1;
  fFilename := '';
  fFileReadOnly := false;
  fHasResources := false;
  FIgnoreFileDateOnDiskValid := false;
  fAutoReferenceSourceDir := true;
  inherited SetIsPartOfProject(false);
  Modified := false;
  SessionModified := false;
  FRunFileIfActive:=false;
  fSyntaxHighlighter := lshText;
  fTopLine := -1;
  fUnitName := '';
  fUsageCount:=-1;
  fUserReadOnly := false;
  if fSource<>nil then fSource.Clear;
  Loaded := false;
  ClearComponentDependencies;
end;

procedure TUnitInfo.ClearModifieds;
begin
  Modified:=false;
  SessionModified:=false;
end;

procedure TUnitInfo.ClearComponentDependencies;
begin
  while FFirstRequiredComponent<>nil do FFirstRequiredComponent.Free;
  while FFirstUsedByComponent<>nil do FFirstUsedByComponent.Free;
end;

procedure TUnitInfo.WriteDebugReportUnitComponentDependencies(Prefix: string);
var
  Dependency: TUnitComponentDependency;
begin
  DebugLn([Prefix+'TUnitInfo.WriteDebugReportUnitComponentDependencies ',Filename,' ',dbgs(Flags)]);
  Dependency:=FirstRequiredComponent;
  if Dependency<>nil then begin
    DebugLn([Prefix+'  Requires:  >>> ']);
    while Dependency<>nil do begin
      DebugLn([Prefix+'    '+Dependency.RequiresUnit.Filename+' '+dbgs(Dependency.Types)]);
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
  Dependency:=FirstUsedByComponent;
  if Dependency<>nil then begin
    DebugLn([Prefix+'  UsedBy:    <<<']);
    while Dependency<>nil do begin
      DebugLn([Prefix+'    '+Dependency.UsedByUnit.Filename+' '+dbgs(Dependency.Types)]);
      Dependency:=Dependency.NextUsedByDependency;
    end;
  end;
end;


{------------------------------------------------------------------------------
  TUnitInfo SaveToXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  SaveData, SaveSession: boolean; UsePathDelim: TPathDelimSwitch);
var
  AFilename: String;
begin
  // global data
  AFilename:=Filename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename, False);
  XMLConfig.SetValue(Path+'Filename/Value',SwitchPathDelims(AFilename,UsePathDelim));

  if SaveData then
    XMLConfig.SetDeleteValue(Path+'IsPartOfProject/Value',IsPartOfProject,false);

  if SaveSession and Assigned(Project.OnSaveUnitSessionInfo) then
    Project.OnSaveUnitSessionInfo(Self);

  // context data (project/session)
  if (IsPartOfProject and SaveData)
  or ((not IsPartOfProject) and SaveSession)
  then begin
    XMLConfig.SetDeleteValue(Path+'ComponentName/Value',fComponentName,'');
    XMLConfig.SetDeleteValue(Path+'HasResources/Value',fHasResources,false);
    XMLConfig.SetDeleteValue(Path+'ResourceBaseClass/Value',
                             PFComponentBaseClassNames[FResourceBaseClass],
                             PFComponentBaseClassNames[pfcbcNone]);
    XMLConfig.SetDeleteValue(Path+'UnitName/Value',fUnitName,'');
    // save custom data
    SaveStringToStringTree(XMLConfig,CustomData,Path+'CustomData/');
  end;

  // session data
  if SaveSession then 
  begin
    XMLConfig.SetDeleteValue(Path+'ComponentState/Value',Ord(FComponentState),0);
    XMLConfig.SetDeleteValue(Path+'CursorPos/X',fCursorPos.X,-1);
    XMLConfig.SetDeleteValue(Path+'CursorPos/Y',fCursorPos.Y,-1);
    XMLConfig.SetDeleteValue(Path+'TopLine/Value',fTopLine,-1);
    XMLConfig.SetDeleteValue(Path+'EditorIndex/Value',fEditorIndex,-1);
    XMLConfig.SetDeleteValue(Path+'UsageCount/Value',RoundToInt(fUsageCount),-1);
    FBookmarks.SaveToXMLConfig(XMLConfig,Path+'Bookmarks/');
    XMLConfig.SetDeleteValue(Path+'Loaded/Value',fLoaded,false);
    XMLConfig.SetDeleteValue(Path+'ReadOnly/Value',fUserReadOnly,false);
    XMLConfig.SetDeleteValue(Path+'SyntaxHighlighter/Value',
                             LazSyntaxHighlighterNames[fSyntaxHighlighter],
                             LazSyntaxHighlighterNames[lshFreePascal]);
    XMLConfig.SetDeleteValue(Path+'BuildFileIfActive/Value',
                             FBuildFileIfActive,false);
    XMLConfig.SetDeleteValue(Path+'RunFileIfActive/Value',
                             FRunFileIfActive,false);
    XMLConfig.SetDeleteValue(Path+'FoldState/Value',FoldState, '');
    // save custom session data
    SaveStringToStringTree(XMLConfig,CustomSessionData,Path+'CustomSessionData/');
  end;
end;

{------------------------------------------------------------------------------
  TUnitInfo LoadFromXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  Merge, IgnoreIsPartOfProject: boolean);
var AFilename: string;
begin
  // project data
  if not Merge then begin
  
    AFilename:=XMLConfig.GetValue(Path+'Filename/Value','');
    if Assigned(fOnLoadSaveFilename) then
      fOnLoadSaveFilename(AFilename,true);
    fFilename:=AFilename;

    fComponentName:=XMLConfig.GetValue(Path+'ComponentName/Value','');
    if fComponentName='' then
      fComponentName:=XMLConfig.GetValue(Path+'FormName/Value','');
    FComponentState := TWindowState(XMLConfig.GetValue(Path+'ComponentState/Value',0));
    HasResources:=XMLConfig.GetValue(Path+'HasResources/Value',false);
    FResourceBaseClass:=StrToComponentBaseClass(
                         XMLConfig.GetValue(Path+'ResourceBaseClass/Value',''));
    if not IgnoreIsPartOfProject then
      IsPartOfProject:=XMLConfig.GetValue(Path+'IsPartOfProject/Value',false);
    AFilename:=XMLConfig.GetValue(Path+'ResourceFilename/Value','');
    if (AFilename<>'') and Assigned(fOnLoadSaveFilename) then
      fOnLoadSaveFilename(AFilename,true);
    if FilenameIsPascalSource(Filename) then
      fUnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');

    // save custom data
    LoadStringToStringTree(XMLConfig,CustomData,Path+'CustomData/');
  end;

  // session data
  CursorPos:=Point(XMLConfig.GetValue(Path+'CursorPos/X',-1),
                   XMLConfig.GetValue(Path+'CursorPos/Y',-1));
  EditorIndex:=XMLConfig.GetValue(Path+'EditorIndex/Value',-1);

  Loaded:=XMLConfig.GetValue(Path+'Loaded/Value',false);
  fUserReadOnly:=XMLConfig.GetValue(Path+'ReadOnly/Value',false);
  fSyntaxHighlighter:=StrToLazSyntaxHighlighter(XMLConfig.GetValue(
       Path+'SyntaxHighlighter/Value',''));
  fTopLine:=XMLConfig.GetValue(Path+'TopLine/Value',-1);
  FBuildFileIfActive:=XMLConfig.GetValue(Path+'BuildFileIfActive/Value',
                                         false);
  FRunFileIfActive:=XMLConfig.GetValue(Path+'RunFileIfActive/Value',false);
  FFoldState := XMLConfig.GetValue(Path+'FoldState/Value', '');
  fUsageCount:=XMLConfig.GetValue(Path+'UsageCount/Value',-1);
  if fUsageCount<1 then begin
    UpdateUsageCount(uuIsLoaded,1);
    if IsPartOfProject then
      UpdateUsageCount(uuIsPartOfProject,1);
  end;
  FBookmarks.LoadFromXMLConfig(XMLConfig,Path+'Bookmarks/');
  // load custom session data
  LoadStringToStringTree(XMLConfig,CustomSessionData,Path+'CustomSessionData/');
end;

function TUnitInfo.ParseUnitNameFromSource(TryCache: boolean): string;
begin
  Result:='';
  if TryCache then
    Result:=CodeToolBoss.GetCachedSourceName(Source);
  if Result='' then
    Result:=CodeToolBoss.GetSourceName(fSource,false);
  if Result='' then begin
    // unable to parse the source
    if FilenameIsPascalSource(Filename) then begin
      // use default: the filename
      Result:=ExtractFileNameOnly(Filename);
      if CompareText(Result,fUnitName)=0 then begin
        // the last stored unitname has the better case
        Result:=fUnitName;
      end;
    end;
  end;
end;

procedure TUnitInfo.SetUnitName(const NewUnitName:string);
var
  Allowed: boolean;
  OldUnitName: String;
begin
  if (fUnitName <> NewUnitName) and (NewUnitName <> '') then
  begin
    Allowed := true;
    OldUnitName := fUnitName;
    if OldUnitName = '' then
      OldUnitName := ExtractFileNameOnly(Filename);
    if Assigned(FOnUnitNameChange) then
      FOnUnitNameChange(Self, OldUnitName, NewUnitName, false, Allowed);
    // (ignore Allowed)
    if (fSource <> nil) then
    begin
      CodeToolBoss.RenameSource(fSource,NewUnitName);
    end;
    fUnitName := NewUnitName;
    Modified := true;
    if (Project <> nil) then Project.UnitModified(Self);
  end;
end;

procedure TUnitInfo.UpdateList(ListType: TUnitInfoList; Add: boolean);
begin
  if Project<>nil then begin
    if Add then
      Project.AddToList(Self,ListType)
    else
      Project.RemoveFromList(Self,ListType);
  end else begin
    fNext[ListType]:=nil;
    fPrev[ListType]:=nil;
  end;
end;

procedure TUnitInfo.SetInternalFilename(const NewFilename: string);
begin
  if fFileName=NewFilename then exit;
  //DebugLn('TUnitInfo.SetInternalFilename Old=',fFileName,' New=',NewFilename);
  
  // if directory changed then remove the old directory reference
  if SourceDirectoryReferenced
  and (Project<>nil)
  and (fLastDirectoryReferenced<>GetDirectory) then begin
    Project.SourceDirectories.RemoveFilename(fLastDirectoryReferenced);
    FSourceDirectoryReferenced:=false;
  end;
  
  fFileName:=NewFilename;
  UpdateSourceDirectoryReference;
end;

function TUnitInfo.GetFileName: string;
begin
  if fSource<>nil then
    Result:=fSource.Filename
  else
    Result:=fFileName;
end;

procedure TUnitInfo.SetFilename(const AValue: string);
begin
  if fSource<>nil then
    RaiseException('TUnitInfo.SetFilename Source<>nil')
  else
    SetInternalFilename(AValue);
end;

function TUnitInfo.IsVirtual: boolean;
begin
  if fSource<>nil then
    Result:=fSource.IsVirtual
  else
    Result:=(fFileName<>ExpandFileNameUTF8(fFileName));
end;

function TUnitInfo.GetDirectory: string;
begin
  if IsVirtual then begin
    if Project<>nil then
      Result:=Project.ProjectDirectory
    else
      Result:='';
  end else  begin
    Result:=ExtractFilePath(Filename);
  end;
end;

function TUnitInfo.IsMainUnit: boolean;
begin
  Result:=(Project<>nil) and (Project.MainUnitInfo=Self);
end;

procedure TUnitInfo.IncreaseAutoRevertLock;
begin
  inc(fAutoRevertLockCount);
  if (fAutoRevertLockCount=1) then begin
    // activate lock
    if (Source<>nil) then
      Source.LockAutoDiskRevert;
    if Project<>nil then
      Project.AddToOrRemoveFromAutoRevertLockedList(Self);
  end;
end;

procedure TUnitInfo.DecreaseAutoRevertLock;
begin
  dec(fAutoRevertLockCount);
  if (fAutoRevertLockCount=0) then begin
    // deactivate lock
    if (Source<>nil) then
      Source.LockAutoDiskRevert;
    if Project<>nil then
      Project.AddToOrRemoveFromAutoRevertLockedList(Self);
  end;
end;

function TUnitInfo.IsAutoRevertLocked: boolean;
begin
  Result:=fAutoRevertLockCount>0;
end;

function TUnitInfo.IsReverting: boolean;
begin
  Result:=FRevertLockCount>0;
end;

function TUnitInfo.ChangedOnDisk(CompareOnlyLoadSaveTime: boolean): boolean;
begin
  Result:=(Source<>nil) and (Source.FileOnDiskHasChanged);
  if Result
  and (not CompareOnlyLoadSaveTime)
  and FIgnoreFileDateOnDiskValid
  and (FIgnoreFileDateOnDisk=Source.FileDateOnDisk) then
    Result:=false;
  if (not IsVirtual) and FileExistsUTF8(Filename) then
    FileReadOnly:=not FileIsWritableCached(Filename)
  else
    FileReadOnly:=false;
end;

procedure TUnitInfo.IgnoreCurrentFileDateOnDisk;
begin
  if Source<>nil then begin
    FIgnoreFileDateOnDiskValid:=true;
    FIgnoreFileDateOnDisk:=Source.FileDateOnDisk;
  end
end;

function TUnitInfo.ShortFilename: string;
begin
  if Project<>nil then begin
    Result:=Project.RemoveProjectPathFromFilename(Filename);
  end else begin
    Result:=Filename;
  end;
end;

function TUnitInfo.NeedsSaveToDisk: boolean;
begin
  Result:=IsVirtual or Modified or ChangedOnDisk(true);
  //DebugLn(['TUnitInfo.NeedsSaveToDisk ',filename,' Result=',Result,' Modified=',Modified]);
  if not Result then begin
    if Source<>nil then
      Result:=Source.FileOnDiskNeedsUpdate
    else
      Result:=not FileExistsUTF8(Filename);
  end;
end;

procedure TUnitInfo.UpdateUsageCount(Min, IfBelowThis, IncIfBelow: extended);
begin
  if fUsageCount<IfBelowThis then fUsageCount:=fUsageCount+IncIfBelow;
  if fUsageCount<Min then fUsageCount:=Min;
end;

procedure TUnitInfo.UpdateUsageCount(TheUsage: TUnitUsage;
  const Factor: extended);
begin
  case TheUsage of
  uuIsPartOfProject: UpdateUsageCount(20,200,2*Factor);
  uuIsLoaded:        UpdateUsageCount(10,100,1*Factor);
  uuIsModified:      UpdateUsageCount(10,0,0);
  uuNotUsed:         fUsageCount:=fUsageCount-(Factor/5);
  end;
end;

procedure TUnitInfo.UpdateSourceDirectoryReference;
begin
  FSourceDirNeedReference:=IsPartOfProject and (FilenameIsPascalSource(Filename));
  if (not AutoReferenceSourceDir) or (FProject=nil) then exit;
  if FSourceDirNeedReference then begin
    if not SourceDirectoryReferenced then begin
      fLastDirectoryReferenced:=GetDirectory;
      //DebugLn('TUnitInfo.UpdateSourceDirectoryReference ADD File="',Filename,'" Project.SourceDirectories.TimeStamp=',dbgs(Project.SourceDirectories.TimeStamp));
      FSourceDirectoryReferenced:=true;
      Project.SourceDirectories.AddFilename(fLastDirectoryReferenced);
    end;
  end else begin
    if SourceDirectoryReferenced then begin
      //DebugLn('TUnitInfo.UpdateSourceDirectoryReference REMOVE File="',Filename,'" Project.SourceDirectories.TimeStamp=',dbgs(Project.SourceDirectories.TimeStamp));
      FSourceDirectoryReferenced:=false;
      Project.SourceDirectories.RemoveFilename(fLastDirectoryReferenced);
    end;
  end;
end;

procedure TUnitInfo.SetSourceText(const SourceText: string);
begin
  Source.Source:=SourceText;
end;

function TUnitInfo.GetSourceText: string;
begin
  Result:=Source.Source;
end;

function TUnitInfo.AddRequiresComponentDependency(RequiredUnit: TUnitInfo;
  Types: TUnitCompDependencyTypes): TUnitComponentDependency;
begin
  if RequiredUnit=nil then RaiseGDBException('inconsistency');
  if RequiredUnit=Self then RaiseGDBException('inconsistency');
  // search a dependency to this RequiredUnit
  Result:=FirstRequiredComponent;
  while Result<>nil do begin
    if Result.RequiresUnit=RequiredUnit then break;
    Result:=Result.NextRequiresDependency;
  end;
  // if none exists, then create one
  if Result=nil then begin
    Result:=TUnitComponentDependency.Create;
    Result.UsedByUnit:=Self;
    Result.RequiresUnit:=RequiredUnit;
  end;
  Result.Types:=Result.Types+Types;
end;

procedure TUnitInfo.RemoveRequiresComponentDependency(RequiredUnit: TUnitInfo;
  Types: TUnitCompDependencyTypes);
var
  Dependency: TUnitComponentDependency;
  NextDependency: TUnitComponentDependency;
begin
  Dependency:=FirstRequiredComponent;
  while Dependency<>nil do begin
    NextDependency:=Dependency.NextRequiresDependency;
    if (Dependency.RequiresUnit=RequiredUnit) then begin
      Dependency.Types:=Dependency.Types-Types;
      if Dependency.Types=[] then
        Dependency.Free;
    end;
    Dependency:=NextDependency;
  end;
end;

function TUnitInfo.FindComponentDependency(RequiredUnit: TUnitInfo
  ): TUnitComponentDependency;
begin
  Result:=FirstRequiredComponent;
  while Result<>nil do begin
    if Result.RequiresUnit=RequiredUnit then exit;
    Result:=Result.NextRequiresDependency;
  end;
end;

function TUnitInfo.FindRequiredComponentDependency(
  MinTypes: TUnitCompDependencyTypes): TUnitComponentDependency;
begin
  Result:=FirstRequiredComponent;
  while Result<>nil do begin
    if Result.Types*MinTypes=MinTypes then exit;
    Result:=Result.NextRequiresDependency;
  end;
end;

function TUnitInfo.FindUsedByComponentDependency(
  MinTypes: TUnitCompDependencyTypes): TUnitComponentDependency;
begin
  Result:=FirstUsedByComponent;
  while Result<>nil do begin
    if Result.Types*MinTypes=MinTypes then exit;
    Result:=Result.NextUsedByDependency;
  end;
end;

function TUnitInfo.FindAncestorUnit: TUnitInfo;
var
  Dependency: TUnitComponentDependency;
begin
  if Component<>nil then begin
    Dependency:=FirstRequiredComponent;
    while Dependency<>nil do begin
      Result:=Dependency.RequiresUnit;
      if (Result.Component<>nil)
      and (Component.ClassParent=Result.Component.ClassType) then
        exit;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;
  Result:=nil;
end;

procedure TUnitInfo.ClearUnitComponentDependencies(
  ClearTypes: TUnitCompDependencyTypes);
var
  Dep: TUnitComponentDependency;
  NextDep: TUnitComponentDependency;
begin
  Dep:=FirstRequiredComponent;
  while Dep<>nil do begin
    NextDep:=Dep.NextRequiresDependency;
    Dep.Types:=Dep.Types-ClearTypes;
    if Dep.Types=[] then
      Dep.Free;
    Dep:=NextDep;
  end;
end;

function TUnitInfo.ReadOnly: boolean;
begin
  Result:=UserReadOnly or FileReadOnly;
end;

procedure TUnitInfo.SetSource(ABuffer: TCodeBuffer);
begin
  if fSource=ABuffer then begin
    if fSource<>nil then
      fSourceChangeStep:=FSource.ChangeStep;
    exit;
  end;
  if (fSource<>nil) and IsAutoRevertLocked then
    fSource.UnlockAutoDiskRevert;
  fSource:=ABuffer;
  FIgnoreFileDateOnDiskValid:=false;
  if (fSource<>nil) then begin
    fSourceChangeStep:=FSource.ChangeStep;
    if IsAutoRevertLocked then
      fSource.LockAutoDiskRevert;
    SetInternalFilename(fSource.FileName);
    if (fProject<>nil) and (fProject.MainUnitInfo=Self) then
      fProject.MainSourceFilenameChanged;
  end;
end;

procedure TUnitInfo.SetUserReadOnly(const NewValue: boolean);
begin
  fUserReadOnly:=NewValue;
  if fSource<>nil then
    fSource.ReadOnly:=ReadOnly;
end;

procedure TUnitInfo.CreateStartCode(Descriptor: TProjectFileDescriptor;
  const NewUnitName: string);
var
  NewSource: string;
  
  function Beautified(const s: string): string;
  begin
    Result:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                  BeautifyStatement(s,0);
  end;
  
begin
  if fSource=nil then exit;
  NewSource:=Beautified(
                  Descriptor.CreateSource(Filename,NewUnitName,fComponentName));
  fSource.Source:=NewSource;
  Modified:=true;
end;

function TUnitInfo.GetHasResources:boolean;
begin
  Result:=fHasResources or (ComponentName<>'');
end;

function TUnitInfo.GetModified: boolean;
begin
  if (not fModified) and (Source<>nil) then
    fModified:=Source.ChangeStep<>fSourceChangeStep;
  Result:=fModified;
end;

function TUnitInfo.GetNextAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fNext[uilAutoRevertLocked];
end;

function TUnitInfo.GetNextLoadedUnit: TUnitInfo;
begin
  Result:=fNext[uilLoaded];
end;

function TUnitInfo.GetNextPartOfProject: TUnitInfo;
begin
  Result:=fNext[uilPartOfProject];
end;

function TUnitInfo.GetNextUnitWithComponent: TUnitInfo;
begin
  Result:=fNext[uilWithComponent];
end;

function TUnitInfo.GetNextUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fNext[uilWithEditorIndex];
end;

function TUnitInfo.GetPrevAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fPrev[uilAutoRevertLocked];
end;

function TUnitInfo.GetPrevLoadedUnit: TUnitInfo;
begin
  Result:=fPrev[uilLoaded];
end;

function TUnitInfo.GetPrevPartOfProject: TUnitInfo;
begin
  Result:=fPrev[uilPartOfProject];
end;

function TUnitInfo.GetPrevUnitWithComponent: TUnitInfo;
begin
  Result:=fPrev[uilWithComponent];
end;

function TUnitInfo.GetPrevUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fPrev[uilWithEditorIndex];
end;

procedure TUnitInfo.SetAutoReferenceSourceDir(const AValue: boolean);
begin
  if FAutoReferenceSourceDir=AValue then exit;
  FAutoReferenceSourceDir:=AValue;
  UpdateSourceDirectoryReference;
end;

procedure TUnitInfo.SetBuildFileIfActive(const AValue: boolean);
begin
  if FBuildFileIfActive=AValue then exit;
  FBuildFileIfActive:=AValue;
  SessionModified:=true;
end;

procedure TUnitInfo.SetDirectives(const AValue: TStrings);
begin
  if FDirectives=AValue then exit;
  FDirectives:=AValue;
end;

procedure TUnitInfo.SetEditorIndex(const AValue: integer);
begin
  if fEditorIndex=AValue then exit;
  fEditorIndex:=AValue;
  UpdateList(uilWithEditorIndex,fEditorIndex>=0);
  SessionModified:=true;
end;

procedure TUnitInfo.SetFileReadOnly(const AValue: Boolean);
begin
  if fFileReadOnly=AValue then exit;
  fFileReadOnly:=AValue;
  if fSource<>nil then
    fSource.ReadOnly:=ReadOnly;
  SessionModified:=true;
end;

procedure TUnitInfo.SetComponent(const AValue: TComponent);
begin
  if fComponent=AValue then exit;
  fComponent:=AValue;
  UpdateList(uilWithComponent,fComponent<>nil);
  if fComponent=nil then
    ClearComponentDependencies
  else
    FResourceBaseClass:=GetComponentBaseClass(fComponent.ClassType);
end;

procedure TUnitInfo.SetIsPartOfProject(const AValue: boolean);
begin
  if IsPartOfProject=AValue then exit;
  if Project<>nil then Project.BeginUpdate(true);
  inherited SetIsPartOfProject(AValue);
  UpdateList(uilPartOfProject,IsPartOfProject);
  if IsPartOfProject then UpdateUsageCount(uuIsPartOfProject,0);
  UpdateSourceDirectoryReference;
  if Project<>nil then Project.EndUpdate;
end;

{-------------------------------------------------------------------------------
  procedure TUnitInfo.SetLoaded(const AValue: Boolean);

  Loaded is a flag, that is set, when a unit has finished loading into the
  editor. It is saved to the project info file and a loaded unit will be
  reloaded, when the project is opened.
-------------------------------------------------------------------------------}
procedure TUnitInfo.SetLoaded(const AValue: Boolean);
begin
  if fLoaded=AValue then exit;
  fLoaded:=AValue;
  if fLoaded then begin
    IncreaseAutoRevertLock;
    UpdateUsageCount(uuIsLoaded,0);
  end else begin
    DecreaseAutoRevertLock;
  end;
end;

procedure TUnitInfo.SetModified(const AValue: boolean);
begin
  if fModified=AValue then exit;
  fModified:=AValue;
  if not fModified then begin
    if Source<>nil then
      fSourceChangeStep:=Source.ChangeStep;
  end;
end;

procedure TUnitInfo.SetProject(const AValue: TProject);
var
  ListType: TUnitInfoList;
begin
  if FProject=AValue then exit;
  if FProject<>nil then begin
    for ListType:=Low(TUnitInfoList) to High(TUnitInfoList) do
      Project.RemoveFromList(Self,ListType);
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    if EditorIndex>=0 then Project.AddToList(Self,uilWithEditorIndex);
    if Component<>nil then Project.AddToList(Self,uilWithComponent);
    if Loaded then Project.AddToList(Self,uilLoaded);
    if IsAutoRevertLocked then Project.AddToList(Self,uilAutoRevertLocked);
    if IsPartOfProject then Project.AddToList(Self,uilPartOfProject);
  end;
  UpdateSourceDirectoryReference;
end;

procedure TUnitInfo.SetRunFileIfActive(const AValue: boolean);
begin
  if FRunFileIfActive=AValue then exit;
  FRunFileIfActive:=AValue;
  SessionModified:=true;
end;

procedure TUnitInfo.SetSessionModified(const AValue: boolean);
begin
  if FSessionModified=AValue then exit;
  FSessionModified:=AValue;
end;


{------------------------------------------------------------------------------
                              TProject Class
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  TProject Constructor
 ------------------------------------------------------------------------------}
constructor TProject.Create(ProjectDescription: TProjectDescriptor);
begin
  inherited Create(ProjectDescription);

  fActiveEditorIndexAtStart := -1;
  FSkipCheckLCLInterfaces:=false;
  FAutoCreateForms := true;
  FBookmarks := TProjectBookmarkList.Create;
  CompilerOptions := TProjectCompilerOptions.Create(Self);
  CompilerOptions.ParsedOpts.InvalidateParseOnChange:=true;
  FDefineTemplates:=TProjectDefineTemplates.Create(Self);
  FFlags:=DefaultProjectFlags;
  FJumpHistory:=TProjectJumpHistory.Create;
  FJumpHistory.OnCheckPosition:=@JumpHistoryCheckPosition;
  FJumpHistory.OnLoadSaveFilename:=@OnLoadSaveFilename;
  fMainUnitID := -1;
  fProjectInfoFile := '';
  ProjectSessionFile:='';
  FSourceDirectories:=TFileReferenceList.Create;
  FSourceDirectories.OnChanged:=@SourceDirectoriesChanged;
  FUseAppBundle := True;

  UpdateProjectDirectory;
  FPublishOptions:=TPublishProjectOptions.Create(Self);
  FRunParameterOptions:=TRunParamsOptions.Create;
  FTargetFileExt := GetExecutableExt;
  Title := '';
  FUnitList := TFPList.Create;  // list of TUnitInfo
  
  FResources := TProjectResources.Create;
  FResources.OnModified := @EmbeddedObjectModified;
end;

{------------------------------------------------------------------------------
  TProject Destructor
 ------------------------------------------------------------------------------}
destructor TProject.Destroy;
begin
  FDefineTemplates.Active := False;
  FDestroying := True;
  Clear;
  FreeThenNil(FResources);
  FreeThenNil(FBookmarks);
  FreeThenNil(FUnitList);
  FreeThenNil(FJumpHistory);
  FreeThenNil(FSourceDirectories);
  FreeThenNil(FPublishOptions);
  FreeThenNil(FRunParameterOptions);
  FreeThenNil(FCompilerOptions);
  FreeThenNil(FDefineTemplates);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TProject WriteProject
 ------------------------------------------------------------------------------}
function TProject.WriteProject(ProjectWriteFlags: TProjectWriteFlags;
  const OverrideProjectInfoFile: string): TModalResult;

  procedure SaveFlags(XMLConfig: TXMLConfig; const Path: string);
  var f: TProjectFlag;
  begin
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      xmlconfig.SetDeleteValue(Path+'General/Flags/'
            +ProjectFlagNames[f]+'/Value', f in Flags,f in DefaultProjectFlags);
    end;
  end;
  
  procedure UpdateUsageCounts(const ConfigFilename: string);
  var
    UnitUsageCount: extended;
    DiffTime: TDateTime;
    i: Integer;
  begin
    UnitUsageCount:=0;
    if CompareFileNames(ConfigFilename,fLastReadLPIFilename)=0 then begin
      DiffTime:=Now-fLastReadLPIFileDate;
      if DiffTime>0 then begin
        UnitUsageCount:= DiffTime*24; // one step every hour
      end;
      fLastReadLPIFileDate:=Now;
    end;
    for i:=0 to UnitCount-1 do begin
      if Units[i].IsPartOfProject then
        Units[i].UpdateUsageCount(uuIsPartOfProject,UnitUsageCount)
      else if Units[i].Loaded then
        Units[i].UpdateUsageCount(uuIsLoaded,UnitUsageCount)
      else
        Units[i].UpdateUsageCount(uuNotUsed,UnitUsageCount);
    end;
  end;
  
  function UnitMustBeSaved(i: integer; SaveData, SaveSession: boolean): boolean;
  begin
    Result:=false;
    if not Units[i].IsPartOfProject then begin
      if not SaveSession then exit;
      if (pfSaveOnlyProjectUnits in Flags) then exit;
      if (pwfSaveOnlyProjectUnits in ProjectWriteFlags) then exit;
      if (not Units[i].Loaded) then begin
        if (not (pfSaveClosedUnits in Flags)) then exit;
        if (pwfDontSaveClosedUnits in ProjectWriteFlags) then exit;
        if Units[i].fUsageCount<=0 then exit;
      end;
    end;
    Result:=true;
  end;
  
  procedure SaveUnits(XMLConfig: TXMLConfig; const Path: string;
    SaveData, SaveSession: boolean);
  var i, SaveUnitCount: integer;
  begin
    SaveUnitCount:=0;
    for i:=0 to UnitCount-1 do begin
      if UnitMustBeSaved(i,SaveData,SaveSession) then begin
        Units[i].SaveToXMLConfig(
          xmlconfig,Path+'Units/Unit'+IntToStr(SaveUnitCount)+'/',
          SaveData,SaveSession,fCurStorePathDelim);
        inc(SaveUnitCount);
      end;
    end;
    xmlconfig.SetDeleteValue(Path+'Units/Count',SaveUnitCount,0);
  end;

  procedure SaveSessionInfo(aConfig: TXMLConfig; const Path: string);
  begin
    aConfig.SetDeleteValue(Path+'General/ActiveEditorIndexAtStart/Value',
                           ActiveEditorIndexAtStart,-1);
    aConfig.SetDeleteValue('SkipCheckLCLInterfaces/Value',
                           FSkipCheckLCLInterfaces,false);

    if (not (pfSaveOnlyProjectUnits in Flags))
    and (not (pwfSkipJumpPoints in ProjectWriteFlags)) then begin
      FJumpHistory.DeleteInvalidPositions;
      FJumpHistory.SaveToXMLConfig(aConfig,Path);
    end;

    // save custom session data
    SaveStringToStringTree(aConfig,CustomSessionData,Path+'CustomSessionData/');
  end;

var
  CfgFilename: String;
  Path: String;
  xmlconfig: TXMLConfig;
  SaveSessionInfoInLPI: Boolean;
  CurSessionFilename: String;
  CurFlags: TProjectWriteFlags;
  SessionSaveResult: TModalResult;
begin
  Result := mrCancel;
  fCurStorePathDelim:=StorePathDelim;

  if OverrideProjectInfoFile<>'' then
    CfgFilename := OverrideProjectInfoFile
  else
    CfgFilename := ProjectInfoFile;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(CfgFilename);
    if Result=mrAbort then exit;
  end;
  CfgFilename:=SetDirSeparators(CfgFilename);
  
  UpdateUsageCounts(CfgFilename);

  CurSessionFilename := '';
  if (not (pwfDoNotSaveSessionInfo in ProjectWriteFlags))
  and (SessionStorage in [pssInProjectDir,pssInIDEConfig]) then begin
    // save session in separate file .lps

    if OverrideProjectInfoFile<>'' then
      CurSessionFilename := ChangeFileExt(OverrideProjectInfoFile,'.lps')
    else
      CurSessionFilename := ProjectSessionFile;
  end;

  // first save the .lpi file
  SaveSessionInfoInLPI:=(CurSessionFilename='')
                        or (CompareFilenames(CurSessionFilename,CfgFilename)=0);
  if (pwfDoNotSaveSessionInfo in ProjectWriteFlags) then
    SaveSessionInfoInLPI:=false;
  if (SessionStorage=pssNone) then
    SaveSessionInfoInLPI:=false;
  repeat
    try
      xmlconfig := TXMLConfig.CreateClean(CfgFilename);
    except
      on E: Exception do begin
        DebugLn('ERROR: ',E.Message);
        MessageDlg('Write error',
          'Unable to write the project info file'#13
          +'"'+ProjectInfoFile+'".'#13
          +'Error: '+E.Message
          ,mtError,[mbOk],0);
        Result:=mrCancel;
        exit;
      end;
    end;

    try
      Path:='ProjectOptions/';
      xmlconfig.SetDeleteValue(Path+'PathDelim/Value',PathDelimSwitchToDelim[fCurStorePathDelim],'/');
      xmlconfig.SetValue(Path+'Version/Value',ProjectInfoFileVersion);
      SaveFlags(XMLConfig,Path);
      xmlconfig.SetDeleteValue(Path+'General/SessionStorage/Value',
                               ProjectSessionStorageNames[SessionStorage],
                               ProjectSessionStorageNames[pssInProjectInfo]);
      xmlconfig.SetDeleteValue(Path+'General/MainUnit/Value', MainUnitID,-1);
      xmlconfig.SetDeleteValue(Path+'General/AutoCreateForms/Value',
                               AutoCreateForms,true);
      xmlconfig.SetValue(Path+'General/TargetFileExt/Value',TargetFileExt);
      xmlconfig.SetDeleteValue(Path+'General/Title/Value', Title,'');
      xmlconfig.SetDeleteValue(Path+'General/UseAppBundle/Value', UseAppBundle, True);

      // lazdoc
      xmlconfig.SetDeleteValue(Path+'LazDoc/Paths',
         SwitchPathDelims(CreateRelativeSearchPath(LazDocPaths,ProjectDirectory),
                          fCurStorePathDelim),
         '');
      
      // i18n
      xmlconfig.SetDeleteValue(Path+'i18n/EnableI18N/Value', EnableI18N, false);
      xmlconfig.SetDeleteValue(Path+'i18n/OutDir/Value',
         SwitchPathDelims(CreateRelativePath(POOutputDirectory,ProjectDirectory),
                          fCurStorePathDelim) ,
         '');

      // Resources
      Resources.WriteToProjectFile(xmlconfig, Path);

      // save custom data
      SaveStringToStringTree(xmlconfig,CustomData,Path+'CustomData/');

      // Save the compiler options
      CompilerOptions.SaveToXMLConfig(XMLConfig,'CompilerOptions/');
      
      // save the Publish Options
      PublishOptions.SaveToXMLConfig(xmlconfig,Path+'PublishOptions/',fCurStorePathDelim);

      // save the Run Parameter Options
      RunParameterOptions.Save(xmlconfig,Path,fCurStorePathDelim);
      
      // save dependencies
      SavePkgDependencyList(xmlconfig,Path+'RequiredPackages/',
        FFirstRequiredDependency,pdlRequires,fCurStorePathDelim);

      // save build modes
      CompilerOptions.BuildModes.SaveToXMLConfig(xmlconfig,Path+'BuildModes/',
                                  true,SaveSessionInfoInLPI,fCurStorePathDelim);

      // save units
      SaveUnits(XMLConfig,Path,true,SaveSessionInfoInLPI);

      // save session info
      if SaveSessionInfoInLPI then begin
        SaveSessionInfo(XMLConfig,Path);
      end;

      if Assigned(OnSaveProjectInfo) then begin
        CurFlags:=ProjectWriteFlags;
        if not SaveSessionInfoInLPI then
          CurFlags:=CurFlags+[pwfDoNotSaveSessionInfo];
        OnSaveProjectInfo(Self,XMLConfig,CurFlags);
      end;

      InvalidateFileStateCache;
      xmlconfig.Flush;
      Modified:=false;
      if SaveSessionInfoInLPI then
        SessionModified:=false;
      
      Result:=mrOk;
    except
      on E: Exception do begin
        Result:=MessageDlg('Write error','Unable to write to file "'+CfgFilename+'".',
          mtError,[mbRetry,mbAbort],0);
      end;
    end;
    try
      xmlconfig.Free;
    except
    end;
    xmlconfig:=nil;
  until Result<>mrRetry;

  if (not (pwfDoNotSaveSessionInfo in ProjectWriteFlags))
  and (SessionStorage in [pssInProjectDir,pssInIDEConfig])
  and (CurSessionFilename<>'')
  and (CompareFilenames(CurSessionFilename,CfgFilename)<>0) then begin
    // save session in separate file .lps

    //DebugLn('TProject.WriteProject Write Session File="',CurSessionFilename,'"');

    if Assigned(fOnFileBackup) then begin
      Result:=fOnFileBackup(CurSessionFilename);
      if Result=mrAbort then exit;
    end;
    CurSessionFilename:=SetDirSeparators(CurSessionFilename);
    SessionSaveResult:=mrCancel;
    repeat
      try
        xmlconfig := TXMLConfig.CreateClean(CurSessionFilename);
      except
        on E: Exception do begin
          DebugLn('ERROR: ',E.Message);
          MessageDlg('Write error',
            'Unable to write the project session file'#13
            +'"'+ProjectSessionFile+'".'#13
            +'Error: '+E.Message
            ,mtError,[mbOk],0);
          Result:=mrCancel;
          exit;
        end;
      end;

      try
        Path:='ProjectSession/';
        fCurStorePathDelim:=SessionStorePathDelim;
        xmlconfig.SetDeleteValue(Path+'PathDelim/Value',
                                PathDelimSwitchToDelim[fCurStorePathDelim],'/');
        xmlconfig.SetValue(Path+'Version/Value',ProjectInfoFileVersion);

        // save all units
        SaveUnits(XMLConfig,Path,true,true);

        // build modes
        CompilerOptions.BuildModes.SaveToXMLConfig(xmlconfig,Path+'BuildModes/',
                                                 false,true,fCurStorePathDelim);

        // save session
        SaveSessionInfo(XMLConfig,Path);

        if Assigned(OnSaveProjectInfo) then begin
          CurFlags:=ProjectWriteFlags+[pwfDoNotSaveProjectInfo];
          OnSaveProjectInfo(Self,XMLConfig,CurFlags);
        end;

        SessionSaveResult:=mrOk;
      except
        on E: Exception do begin
          SessionSaveResult:=MessageDlg('Write error',
            'Unable to write to file "'+CurSessionFilename+'".',
            mtError,[mbRetry,mbAbort],0);
        end;
      end;
      try
        xmlconfig.Free;
      except
      end;
      xmlconfig:=nil;
    until SessionSaveResult<>mrRetry;
    if (Result=mrOk) and (SessionSaveResult<>mrOk) then
      Result:=SessionSaveResult;
  end;
end;

procedure TProject.UpdateExecutableType;

  function GetMainSourceType: string;
  var
    AnUnitInfo: TUnitInfo;
  begin
    Result:='';
    if MainUnitID<0 then exit;
    AnUnitInfo:=Units[MainUnitID];
    if AnUnitInfo.Source=nil then exit;
    Result:=CodeToolBoss.GetSourceType(AnUnitInfo.Source,false);
  end;

var
  SourceType: String;
begin
  SourceType:=GetMainSourceType;
  if SysUtils.CompareText(SourceType,'Program')=0 then
    ExecutableType:=petProgram
  else if SysUtils.CompareText(SourceType,'Library')=0 then
    ExecutableType:=petLibrary
  else if SysUtils.CompareText(SourceType,'Unit')=0 then
    ExecutableType:=petUnit
  else if SysUtils.CompareText(SourceType,'Package')=0 then
    ExecutableType:=petPackage
  else
    ExecutableType:=petNone;
end;

function TProject.GetDefaultTitle: string;
begin
  Result:=ExtractFilenameOnly(ProjectInfoFile);
end;

function TProject.TitleIsDefault(Fuzzy: boolean): boolean;
begin
  Result:=(Title='') or (Title=GetDefaultTitle)
    or (Fuzzy and (SysUtils.CompareText(Title,GetDefaultTitle)=0));
end;

function TProject.IDAsString: string;
begin
  Result:='Project'; // TODO: see TLazPackage, when this is changed change also TProjectDefineTemplates.UpdateSrcDirIfDef
end;

function TProject.IDAsWord: string;
begin
  Result:='Project'; // TODO: see TLazPackage when this is changed change also TProjectDefineTemplates.UpdateSrcDirIfDef
end;

{------------------------------------------------------------------------------
  TProject ReadProject
 ------------------------------------------------------------------------------}
function TProject.ReadProject(const NewProjectInfoFile: string): TModalResult;
type
  TOldProjectType = (ptApplication, ptProgram, ptCustomProgram);
const
  OldProjectTypeNames : array[TOldProjectType] of string = (
      'Application', 'Program', 'Custom program'
    );
var
  FileVersion: Integer;
  NewMainUnitID: LongInt;

  procedure LoadCompilerOptions(XMLConfig: TXMLConfig; const Path: string);
  var
    CompOptsPath: String;
  begin
    CompOptsPath:='CompilerOptions/';
    if FileVersion<3 then begin
      // due to an old bug, the XML path can be 'CompilerOptions/' or ''
      if XMLConfig.GetValue('SearchPaths/CompilerPath/Value','')<>'' then
        CompOptsPath:=''
      else if XMLConfig.GetValue(
        'CompilerOptions/SearchPaths/CompilerPath/Value','')<>''
      then
        CompOptsPath:='CompilerOptions/';
    end;
    CompilerOptions.LoadFromXMLConfig(xmlconfig,CompOptsPath);
    if FileVersion<2 then
      CompilerOptions.SrcPath:=xmlconfig.GetValue(Path+'General/SrcPath/Value','');
  end;

  function ReadOldProjectType(XMLConfig: TXMLConfig;
    const Path: string): TOldProjectType;

    function OldProjectTypeNameToType(const s: string): TOldProjectType;
    begin
      for Result:=Low(TOldProjectType) to High(TOldProjectType) do
        if (CompareText(OldProjectTypeNames[Result],s)=0) then exit;
      Result:=ptApplication;
    end;

  begin
    if FileVersion<=4 then
      Result := OldProjectTypeNameToType(xmlconfig.GetValue(
                                          Path+'General/ProjectType/Value', ''))
    else
      Result := ptCustomProgram;
  end;

  procedure LoadFlags(XMLConfig: TXMLConfig; const Path: string);
  
    procedure SetFlag(f: TProjectFlag; Value: boolean);
    begin
      if Value then Include(FFlags,f) else Exclude(FFlags,f);
    end;
  
  var
    f: TProjectFlag;
    OldProjectType: TOldProjectType;
    DefFlags: TProjectFlags;
  begin
    OldProjectType:=ReadOldProjectType(XMLConfig,Path);
    DefFlags:=DefaultProjectFlags;
    if FileVersion<7 then
      Exclude(DefFlags,pfLRSFilesInOutputDirectory);
    FFlags:=[];
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      SetFlag(f,xmlconfig.GetValue(
             Path+'General/Flags/'+ProjectFlagNames[f]+'/Value',f in DefFlags));
    end;
    if FileVersion<=3 then begin
      // set new flags
      SetFlag(pfMainUnitIsPascalSource,
                                   OldProjectType in [ptProgram,ptApplication]);
      SetFlag(pfMainUnitHasUsesSectionForAllUnits,
                                   OldProjectType in [ptProgram,ptApplication]);
      SetFlag(pfMainUnitHasCreateFormStatements,
                                             OldProjectType in [ptApplication]);
      SetFlag(pfMainUnitHasTitleStatement,OldProjectType in [ptApplication]);
      SetFlag(pfRunnable,
                   OldProjectType in [ptProgram,ptApplication,ptCustomProgram]);
    end;
  end;
  
  procedure LoadSessionInfo(XMLConfig: TXMLConfig; const Path: string;
    Merge: boolean);
  var
    NewUnitInfo: TUnitInfo;
    NewUnitCount,i: integer;
    SubPath: String;
    NewUnitFilename: String;
    OldUnitInfo: TUnitInfo;
    MergeUnitInfo: Boolean;
  begin
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject D reading units');{$ENDIF}
    NewUnitCount:=xmlconfig.GetValue(Path+'Units/Count',0);
    for i := 0 to NewUnitCount - 1 do begin
      SubPath:=Path+'Units/Unit'+IntToStr(i)+'/';
      NewUnitFilename:=XMLConfig.GetValue(SubPath+'Filename/Value','');
      OnLoadSaveFilename(NewUnitFilename,true);
      // load unit and add it
      OldUnitInfo:=UnitInfoWithFilename(NewUnitFilename);
      if OldUnitInfo<>nil then begin
        // unit already exists
        if Merge then begin
          NewUnitInfo:=OldUnitInfo;
          MergeUnitInfo:=true;
        end else begin
          // Doppelganger -> inconsistency found, ignore this file
          debugln('TProject.ReadProject file exists twice in lpi file: ignoring "'+NewUnitFilename+'"');
          continue;
        end;
      end else begin
        NewUnitInfo:=TUnitInfo.Create(nil);
        AddFile(NewUnitInfo,false);
        MergeUnitInfo:=false;
      end;

      NewUnitInfo.LoadFromXMLConfig(xmlconfig,SubPath,MergeUnitInfo,Merge);
      if i=NewMainUnitID then begin
        MainUnitID:=IndexOf(NewUnitInfo);
        NewMainUnitID:=-1;
      end;
    end;

    // load editor info
    ActiveEditorIndexAtStart := xmlconfig.GetValue(
       Path+'General/ActiveEditorIndexAtStart/Value', -1);
    FSkipCheckLCLInterfaces:=xmlconfig.GetValue(
       Path+'SkipCheckLCLInterfaces/Value',false);
    FJumpHistory.LoadFromXMLConfig(xmlconfig,Path+'');

    // load custom session data
    LoadStringToStringTree(xmlconfig,CustomSessionData,Path+'CustomSessionData/');
  end;
  
  procedure LoadDefaultSession;
  var
    AnUnitInfo: TUnitInfo;
    BestUnitInfo: TUnitInfo;
  begin
    if FirstUnitWithEditorIndex<>nil then exit;
    
    AnUnitInfo:=FirstPartOfProject;
    BestUnitInfo:=nil;
    while AnUnitInfo<>nil do begin
      if (BestUnitInfo=nil)
      or (FilenameIsPascalUnit(AnUnitInfo.Filename)
           and (not FilenameIsPascalUnit(BestUnitInfo.Filename)))
      then begin
        BestUnitInfo:=AnUnitInfo;
      end;
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
    if BestUnitInfo<>nil then begin
      BestUnitInfo.EditorIndex:=0;
      ActiveEditorIndexAtStart:=0;
      BestUnitInfo.Loaded:=true;
    end;
  end;
  
var
  Path: String;
  xmlconfig: TXMLConfig;
begin
  Result := mrCancel;
  BeginUpdate(true);
  try
    Clear;

    ProjectInfoFile:=NewProjectInfoFile;
    try
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject A reading lpi');{$ENDIF}
      xmlconfig := TXMLConfig.Create(ProjectInfoFile);
      fLastReadLPIFilename:=ProjectInfoFile;
      fLastReadLPIFileDate:=Now;
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject B done lpi');{$ENDIF}
    except
      MessageDlg('Unable to read the project info file'#13'"'+ProjectInfoFile+'".'
          ,mtError,[mbOk],0);
      Result:=mrCancel;
      exit;
    end;

    NewMainUnitID:=-1;
    try
      Path:='ProjectOptions/';
      StorePathDelim:=CheckPathDelim(
        XMLConfig.GetValue(Path+'PathDelim/Value', '/'),fPathDelimChanged);
      fCurStorePathDelim:=StorePathDelim;

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject C reading values');{$ENDIF}
      FileVersion:= XMLConfig.GetValue(Path+'Version/Value',0);
      LoadFlags(XMLConfig,Path);
      
      SessionStorage:=StrToProjectSessionStorage(
                        XMLConfig.GetValue(Path+'General/SessionStorage/Value',
                                 ProjectSessionStorageNames[pssInProjectInfo]));
      //DebugLn('TProject.ReadProject SessionStorage=',dbgs(ord(SessionStorage)),' ProjectSessionFile=',ProjectSessionFile);

      // build modes
      CompilerOptions.BuildModes.LoadFromXMLConfig(xmlconfig,Path+'BuildModes/',
                                                   false,fPathDelimChanged);

      NewMainUnitID := xmlconfig.GetValue(Path+'General/MainUnit/Value', -1);
      AutoCreateForms := xmlconfig.GetValue(
         Path+'General/AutoCreateForms/Value', true);
      TargetFileExt := xmlconfig.GetValue(
         Path+'General/TargetFileExt/Value', GetExecutableExt);
      Title := xmlconfig.GetValue(Path+'General/Title/Value', '');
      UseAppBundle := xmlconfig.GetValue(Path+'General/UseAppBundle/Value', True);

      // Lazdoc
      LazDocPaths := SwitchPathDelims(xmlconfig.GetValue(Path+'LazDoc/Paths', ''),
                             fPathDelimChanged);

      // i18n
      if FileVersion<6 then begin
        POOutputDirectory := SwitchPathDelims(
                   xmlconfig.GetValue(Path+'RST/OutDir', ''),fPathDelimChanged);
        EnableI18N := POOutputDirectory <> '';
      end else begin
        EnableI18N := xmlconfig.GetValue(Path+'i18n/EnableI18N/Value', False);
        POOutputDirectory := SwitchPathDelims(
             xmlconfig.GetValue(Path+'i18n/OutDir/Value', ''),fPathDelimChanged);
      end;

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject E reading comp sets');{$ENDIF}
      // Load the compiler options
      LoadCompilerOptions(XMLConfig,Path);

      // Resources
      Resources.ReadFromProjectFile(xmlconfig, Path);

      // load custom data
      LoadStringToStringTree(xmlconfig,CustomData,Path+'CustomData/');
      
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject update ct boss');{$ENDIF}
      CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
                                                               ProjectDirectory;
      CodeToolBoss.DefineTree.ClearCache;
      
      // load the dependencies
      LoadPkgDependencyList(XMLConfig,Path+'RequiredPackages/',
                          FFirstRequiredDependency,pdlRequires,Self,true,false);

      // load the Run Parameter Options
      RunParameterOptions.Load(xmlconfig,Path,fPathDelimChanged);

      // load the Publish Options
      PublishOptions.LoadFromXMLConfig(xmlconfig,
                                       Path+'PublishOptions/',fPathDelimChanged);

      // load session info
      LoadSessionInfo(XMLConfig,Path,false);

      // call hooks to read their info (e.g. DebugBoss)
      if Assigned(OnLoadProjectInfo) then begin
        OnLoadProjectInfo(Self,XMLConfig,false);
      end;
    finally
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject freeing xml');{$ENDIF}
      fPathDelimChanged:=false;
      try
        xmlconfig.Free;
      except
      end;
      xmlconfig:=nil;
    end;

    // load session file (if available)
    if (SessionStorage in [pssInProjectDir,pssInIDEConfig])
    and (CompareFilenames(ProjectInfoFile,ProjectSessionFile)<>0) then begin
      if FileExistsUTF8(ProjectSessionFile) then begin
        //DebugLn('TProject.ReadProject loading Session ProjectSessionFile=',ProjectSessionFile);
        try
          xmlconfig := TXMLConfig.Create(ProjectSessionFile);

          Path:='ProjectSession/';
          SessionStorePathDelim:=CheckPathDelim(
            XMLConfig.GetValue(Path+'PathDelim/Value', '/'),fPathDelimChanged);
          fCurStorePathDelim:=SessionStorePathDelim;

          FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);

          // load user sepcific build modes
          CompilerOptions.BuildModes.LoadFromXMLConfig(xmlconfig,Path+'BuildModes/',
                                                       true,fPathDelimChanged);

          // load session info
          LoadSessionInfo(XMLConfig,Path,true);

          // call hooks to read their info (e.g. DebugBoss)
          if Assigned(OnLoadProjectInfo) then begin
            OnLoadProjectInfo(Self,XMLConfig,true);
          end;
        except
          MessageDlg('Unable to read the project info file'#13'"'+ProjectInfoFile+'".'
              ,mtError,[mbOk],0);
          Result:=mrCancel;
          exit;
        end;

        fPathDelimChanged:=false;
        try
          xmlconfig.Free;
        except
        end;
        fCurStorePathDelim:=StorePathDelim;
        xmlconfig:=nil;
      end else begin
        // there is no .lps file -> create some defaults
        LoadDefaultSession;
      end;
    end;

  finally
    FixEditorIndex;
    EndUpdate;
  end;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject END');{$ENDIF}
  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject AddFile
 ------------------------------------------------------------------------------}
procedure TProject.AddFile(ProjectFile: TLazProjectFile;
  AddToProjectUsesClause: boolean);
var
  ShortUnitName:string;
  NewIndex: integer;
  AnUnit: TUnitInfo;
begin
  AnUnit:=ProjectFile as TUnitInfo;
  //debugln('TProject.AddUnit A ',AnUnit.Filename,' AddToProjectFile=',dbgs(AddToProjectFile));
  BeginUpdate(true);
  NewIndex:=UnitCount;
  FUnitList.Add(AnUnit);
  AnUnit.Project:=Self;
  AnUnit.OnFileBackup:=@OnUnitFileBackup;
  AnUnit.OnLoadSaveFilename:=@OnLoadSaveFilename;
  AnUnit.OnUnitNameChange:=@OnUnitNameChange;
  
  // check if this is the new Main Unit
  // or if this is the first unit, make it automatically the main unit
  if (MainUnitID<0) and (UnitCount=1) then
    MainUnitID:=0
  else if MainUnitID=NewIndex then
    MainUnitInfo.IncreaseAutoRevertLock;

  if AddToProjectUsesClause and (MainUnitID>=0) and (MainUnitID<>NewIndex) then
  begin
    // add unit to uses section
    ShortUnitName:=AnUnit.AUnitName;
    if (ShortUnitName<>'') and (not UnitIsUsed(ShortUnitName)) then begin
      CodeToolBoss.AddUnitToMainUsesSection(MainUnitInfo.Source,
        ShortUnitName,'',true);
    end;
  end;
  EndUpdate;
  UnitModified(AnUnit);
end;

{------------------------------------------------------------------------------
  TProject RemoveUnit
 ------------------------------------------------------------------------------}
procedure TProject.RemoveUnit(Index: integer; RemoveFromUsesSection: boolean);
var
  OldUnitInfo: TUnitInfo;
begin
  if (Index<0) or (Index>=UnitCount) then begin
    raise Exception.Create('ERROR: TProject.RemoveUnit index out of bounds');
  end;
  if (Index=MainUnitID) then begin
    raise Exception.Create('ERROR: TProject.RemoveUnit index = MainUnit');
  end;
  
  BeginUpdate(true);
  OldUnitInfo:=Units[Index];
  UnitModified(OldUnitInfo);

  if (MainUnitID>=0) then begin
    // remove unit from uses section and from createforms in program file
    if (OldUnitInfo.IsPartOfProject) then begin
      if RemoveFromUsesSection then begin
        if (OldUnitInfo.AUnitName<>'') then begin
          CodeToolBoss.RemoveUnitFromAllUsesSections(MainUnitInfo.Source,
            OldUnitInfo.AUnitName);
        end;
        if (OldUnitInfo.ComponentName<>'') then begin
          CodeToolBoss.RemoveCreateFormStatement(MainUnitInfo.Source,
            OldUnitInfo.ComponentName);
        end;
      end;
    end;
  end;

  // delete bookmarks of this unit
  if OldUnitInfo.EditorIndex>=0 then
    Bookmarks.DeleteAllWithEditorIndex(OldUnitInfo.EditorIndex);

  // adjust MainUnit
  if MainUnitID>=Index then dec(fMainUnitID);

  // delete unitinfo instance
  OldUnitInfo.Free;
  FUnitList.Delete(Index);
  EndUpdate;
end;

function TProject.CreateProjectFile(const Filename: string): TLazProjectFile;
var
  NewBuf: TCodeBuffer;
  AnUnitInfo: TUnitInfo;
begin
  NewBuf:=CodeToolBoss.CreateFile(Filename);
  AnUnitInfo:=TUnitInfo.Create(NewBuf);
  AnUnitInfo.SyntaxHighlighter:=FilenameToLazSyntaxHighlighter(NewBuf.Filename);
  Result:=AnUnitInfo;
end;

procedure TProject.RemoveNonExistingFiles(RemoveFromUsesSection: boolean);
var
  i: Integer;
  AnUnitInfo: TUnitInfo;
begin
  i:=UnitCount-1;
  while (i>=0) do begin
    if i<UnitCount then begin
      AnUnitInfo:=Units[i];
      if (not AnUnitInfo.IsVirtual) and (i<>MainUnitID) then begin
        if not FileExistsUTF8(AnUnitInfo.Filename) then
          RemoveUnit(i,RemoveFromUsesSection);
      end;
    end;
    dec(i);
  end;
end;

{------------------------------------------------------------------------------
  TProject Clear
 ------------------------------------------------------------------------------}
procedure TProject.Clear;
var i:integer;
begin
  BeginUpdate(true);

  // break and free removed dependencies
  while FFirstRemovedDependency<>nil do
    DeleteRemovedDependency(FFirstRemovedDependency);
  // break and free required dependencies
  while FFirstRequiredDependency<>nil do
    DeleteRequiredDependency(FFirstRequiredDependency);

  // delete files
  for i:=0 to UnitCount-1 do Units[i].Free;
  FUnitList.Clear;
  
  FRunParameterOptions.Clear;

  fActiveEditorIndexAtStart := -1;
  FSkipCheckLCLInterfaces:=false;
  FAutoOpenDesignerFormsDisabled := false;
  FBookmarks.Clear;
  FCompilerOptions.Clear;
  FDefineTemplates.Clear;
  FJumpHistory.Clear;
  fMainUnitID := -1;
  fProjectInfoFile := '';
  ProjectSessionFile:='';
  FStateFileDate:=0;
  FStateFlags:=[];
  ClearSourceDirectories;
  UpdateProjectDirectory;
  FPublishOptions.Clear;
  FTargetFileExt := GetExecutableExt;
  Title := '';

  Modified := false;
  SessionModified := false;
  EndUpdate;
end;

procedure TProject.BeginUpdate(Change: boolean);
begin
  inc(FUpdateLock);
  FDefineTemplates.BeginUpdate;
  FSourceDirectories.BeginUpdate;
  if FUpdateLock=1 then begin
    fChanged:=Change;
    if Assigned(OnBeginUpdate) then OnBeginUpdate(Self);
  end else
    fChanged:=fChanged or Change;
end;

procedure TProject.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseException('TProject.EndUpdate');
  dec(FUpdateLock);
  FSourceDirectories.EndUpdate;
  FDefineTemplates.EndUpdate;
  if FUpdateLock=0 then begin
    if Assigned(OnEndUpdate) then OnEndUpdate(Self,fChanged);
  end;
end;

procedure TProject.UnitModified(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.IsPartOfProject then
    Modified:=true
  else
    SessionModified:=true;
end;

function TProject.NeedsDefineTemplates: boolean;
begin
  Result:=not Destroying;
end;

procedure TProject.BeginRevertUnit(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo<>nil then
    inc(AnUnitInfo.FRevertLockCount);
  inc(FRevertLockCount);
  if FRevertLockCount=1 then begin
    Include(FStateFlags,lpsfPropertyDependenciesChanged);
    ClearUnitComponentDependencies([ucdtOldProperty,ucdtProperty]);
    LockUnitComponentDependencies;
    UpdateUnitComponentDependencies;
  end;
end;

procedure TProject.EndRevertUnit(AnUnitInfo: TUnitInfo);
begin
  if FRevertLockCount<=0 then
    raise Exception.Create('TProject.EndRevertUnit Project');
  if (AnUnitInfo<>nil) and (AnUnitInfo.FRevertLockCount<=0) then
    raise Exception.Create('TProject.EndRevertUnit Filename='+AnUnitInfo.Filename);
  if AnUnitInfo<>nil then
    dec(AnUnitInfo.FRevertLockCount);
  dec(FRevertLockCount);
  if FRevertLockCount=0 then
    UnlockUnitComponentDependencies;
end;

function TProject.IsReverting(AnUnitInfo: TUnitInfo): boolean;
begin
  if AnUnitInfo=nil then
    Result:=FRevertLockCount>0
  else
    Result:=AnUnitInfo.FRevertLockCount>0;
end;

function TProject.GetUnits(Index:integer):TUnitInfo;
begin
  Result:=TUnitInfo(FUnitList[Index]);
end;

procedure TProject.SetFlags(const AValue: TProjectFlags);
begin
  inherited SetFlags(AValue);
end;

procedure TProject.SetMainUnitID(const AValue: Integer);
begin
  if AValue>=UnitCount then
    RaiseGDBException('');
    
  if MainUnitID=AValue then exit;
  if (MainUnitID>=0) and (MainUnitID<UnitCount) then
    MainUnitInfo.DecreaseAutoRevertLock;
  fMainUnitID:=AValue;
  if (MainUnitID>=0) and (MainUnitID<UnitCount) then
    MainUnitInfo.IncreaseAutoRevertLock;
end;

function TProject.GetFiles(Index: integer): TLazProjectFile;
begin
  Result:=Units[Index];
end;

procedure TProject.SetModified(const AValue: boolean);
begin
  if AValue = Modified then exit;
  inherited SetModified(AValue);
  if not Modified then 
  begin
    PublishOptions.Modified := False;
    CompilerOptions.Modified := False;
    Resources.Modified := False;
    SessionModified := False;
  end;
end;

procedure TProject.SetSessionModified(const AValue: boolean);
begin
  if AValue=SessionModified then exit;
  inherited SetSessionModified(AValue);
end;

procedure TProject.SetExecutableType(const AValue: TProjectExecutableType);
begin
  inherited SetExecutableType(AValue);
  case ExecutableType of
  petLibrary: CompilerOptions.ExecutableType:=cetLibrary;
  else        CompilerOptions.ExecutableType:=cetProgram;
  end;
end;

function TProject.UnitCount:integer;
begin
  Result:=FUnitList.Count;
end;

function TProject.GetFileCount: integer;
begin
  Result:=UnitCount;
end;

function TProject.NewUniqueUnitName(const AnUnitName: string):string;

  function ExpandedUnitname(const AnUnitName:string):string;
  begin
    Result:=uppercase(ExtractFileNameOnly(AnUnitName));
  end;

  function UnitNameExists(const AnUnitName:string):boolean;
  var i:integer;
    ExpName:string;
  begin
    Result:=true;
    ExpName:=ExpandedUnitName(AnUnitName);
    if ExtractFileNameOnly(fProjectInfoFile)=ExpName then exit;
    for i:=0 to UnitCount-1 do
      if (Units[i].IsPartOfProject) 
      and (ExpandedUnitName(Units[i].FileName)=ExpName) then
        exit;
    Result:=false;
  end;

var
  u:integer;
  Prefix: string;
begin
  Prefix:=AnUnitName;
  while (Prefix<>'') and (Prefix[length(Prefix)] in ['0'..'9']) do
    Prefix:=copy(Prefix,1,length(Prefix)-1);
  if (Prefix='') or (not IsValidIdent(Prefix)) then
    Prefix:='Unit';
  u:=0;
  repeat
    inc(u);
    Result:=Prefix+IntToStr(u);
  until (not UnitNameExists(Result));
end;

function TProject.NewUniqueComponentName(const AComponentPrefix: string
  ): string;

  function FormComponentExists(const AComponentName: string): boolean;
  var i: integer;
  begin
    Result:=true;
    if GetClass(AComponentName)<>nil then exit;
    for i:=0 to UnitCount-1 do begin
      if (Units[i].Component<>nil) then begin
        if CompareText(Units[i].Component.Name,AComponentName)=0 then exit;
        if CompareText(Units[i].Component.ClassName,'T'+AComponentName)=0
        then exit;
      end else if (Units[i].ComponentName<>'')
      and ((Units[i].IsPartOfProject) or (Units[i].Loaded)) then begin
        if AnsiCompareText(Units[i].ComponentName,AComponentName)=0 then exit;
      end;
    end;
    Result:=false;
  end;

var
  u: integer;
  Prefix: string;
begin
  Prefix:=AComponentPrefix;
  while (Prefix<>'') and (Prefix[length(Prefix)] in ['0'..'9']) do
    Prefix:=copy(Prefix,1,length(Prefix)-1);
  if (Prefix='') or (not IsValidIdent(Prefix)) then
    Prefix:='Resource';
  u:=0;
  repeat
    inc(u);
    Result:=Prefix+IntToStr(u);
  until (not FormComponentExists(Result));
end;

function TProject.NewUniqueFilename(const Filename: string): string;
var
  FileNameOnly: String;
  FileExt: String;
  i: Integer;
begin
  FileNameOnly:=ExtractFilenameOnly(Filename);
  while (FileNameOnly<>'')
  and (FileNameOnly[length(FileNameOnly)] in ['0'..'9']) do
    FileNameOnly:=copy(FileNameOnly,1,length(FileNameOnly)-1);
  FileExt:=ExtractFileExt(Filename);
  i:=0;
  repeat
    inc(i);
    Result:=FileNameOnly+IntToStr(i)+FileExt;
  until ProjectUnitWithShortFilename(Result)=nil;
end;

function TProject.AddCreateFormToProjectFile(
  const AClassName, AName: string):boolean;
begin
  if (pfMainUnitHasCreateFormStatements in Project1.Flags) then begin
    Result:=CodeToolBoss.AddCreateFormStatement(MainUnitInfo.Source,
      AClassName,AName);
    if Result then begin
      Modified:=true;
      MainUnitInfo.Modified:=true;
    end;
  end else begin
    Result:=false;
  end;
end;

function TProject.RemoveCreateFormFromProjectFile(
  const AClassName,AName:string):boolean;
begin
  Result:=CodeToolBoss.RemoveCreateFormStatement(MainUnitInfo.Source,
              AName);
  if Result then begin
    Modified:=true;
    MainUnitInfo.Modified:=true;
  end;
end;

function TProject.FormIsCreatedInProjectFile(
  const AClassname,AName:string):boolean;
var p: integer;
begin
  Result:=(CodeToolBoss.FindCreateFormStatement(MainUnitInfo.Source,
                                                1,AClassName,AName,p)=0);
  if p=0 then ;
end;

function TProject.IndexOfUnitWithName(const AnUnitName:string; 
  OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if ((OnlyProjectUnits and Units[Result].IsPartOfProject)
    or (not OnlyProjectUnits))
    and (IgnoreUnit<>Units[Result]) then begin
      if (AnsiCompareText(Units[Result].AUnitName,AnUnitName)=0) then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithComponent(AComponent: TComponent;
  OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject) 
    or (not OnlyProjectUnits)
    and (IgnoreUnit<>Units[Result]) then begin
      if Units[Result].Component=AComponent then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithComponentName(const AComponentName: string;
  OnlyProjectUnits: boolean; IgnoreUnit: TUnitInfo): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if ((OnlyProjectUnits and Units[Result].IsPartOfProject)
    or (not OnlyProjectUnits))
    and (IgnoreUnit<>Units[Result]) then begin
      if (AnsiCompareText(Units[Result].ComponentName,AComponentName)=0)
      or ((Units[Result].Component<>nil)
        and (AnsiCompareText(Units[Result].Component.Name,AComponentName)=0))
      then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.UnitWithEditorIndex(Index:integer):TUnitInfo;
begin
  Result:=fFirst[uilWithEditorIndex];
  while (Result<>nil) and (Result.EditorIndex<>Index) do begin
    Result:=Result.fNext[uilWithEditorIndex];
  end;
end;

function TProject.UnitIsUsed(const ShortUnitName:string):boolean;
var NamePos, InPos: integer;
begin
  Result:=CodeToolBoss.FindUnitInAllUsesSections(MainUnitInfo.Source,
              ShortUnitName,NamePos,InPos);
  if (NamePos<1) or (InPos<1) then ;
end;

function TProject.GetResourceFile(AnUnitInfo: TUnitInfo;
  Index:integer): TCodeBuffer;
var i, LinkIndex: integer;
begin
  LinkIndex:=-1;
  i:=0;
  Result:=nil;
  while (i<Index) do begin
    inc(i);
    Result:=CodeToolBoss.FindNextResourceFile(AnUnitInfo.Source,LinkIndex);
  end;
end;

function TProject.SearchFile(
  const Filename,SearchPaths,InitialDir:string):string;
var StartPos,EndPos:integer;
  CurPath: string;
  OldDir: string;
begin
  OldDir:=GetCurrentDirUTF8;
  SetCurrentDirUTF8(ExtractFilePath(InitialDir));
  try
    StartPos:=1;
    while StartPos<=length(SearchPaths) do begin
      EndPos:=Startpos;
      while (EndPos<=length(SearchPaths)) and (SearchPaths[EndPos]<>';') do 
        inc(EndPos);
      CurPath:=copy(SearchPaths,Startpos,EndPos-StartPos);
      if CurPath<>'' then begin
        if CurPath[length(CurPath)]<>PathDelim then
          CurPath:=CurPath+PathDelim;
        Result:=CurPath+Filename;
        if FileExistsUTF8(Result) then exit;
      end;
      StartPos:=EndPos+1;
    end;
  finally
    SetCurrentDirUTF8(OldDir);
  end;
  Result:='';
end;

procedure TProject.ShortenFilename(var AFilename: string);
begin
  OnLoadSaveFilename(AFilename,false);
end;

procedure TProject.LongenFilename(var AFilename: string);
begin
  OnLoadSaveFilename(AFilename,true);
end;

function TProject.GetMainResourceFilename(AnUnitInfo: TUnitInfo):string;
var CodeBuf: TCodeBuffer;
begin
  CodeBuf:=GetResourceFile(AnUnitInfo,1);
  if CodeBuf=nil then begin
    if AnUnitInfo.Filename='' then exit;
    Result:=ChangeFileExt(AnUnitInfo.Filename,ResourceFileExt);
    exit;
  end else
    Result:=CodeBuf.Filename;
end;

function TProject.IsVirtual: boolean;
begin
  Result:=((MainUnitID>=0) and MainUnitInfo.IsVirtual)
          or (ProjectInfoFile='') or (not FilenameIsAbsolute(ProjectInfoFile));
end;

function TProject.IndexOf(AUnitInfo: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) and (Units[Result]<>AUnitInfo) do dec(Result);
end;

procedure TProject.CloseEditorIndex(EditorIndex:integer);
var i:integer;
  AnUnitInfo, NextUnitInfo: TUnitInfo;
begin
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    NextUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
    if AnUnitInfo.EditorIndex=EditorIndex then
      AnUnitInfo.EditorIndex:=-1
    else if AnUnitInfo.EditorIndex>EditorIndex then
      AnUnitInfo.EditorIndex:=AnUnitInfo.EditorIndex-1;
    AnUnitInfo:=NextUnitInfo;
  end;
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    if (Bookmarks[i].EditorIndex=EditorIndex) then
      Bookmarks.Delete(i)
    else
      Bookmarks[i].EditorIndex:=Bookmarks[i].EditorIndex-1;
    dec(i);
  end;
  SessionModified:=true;
end;

procedure TProject.InsertEditorIndex(EditorIndex:integer);

  function MoveIndex(OldIndex: integer): integer;
  begin
    Result:=OldIndex;
    if OldIndex>=EditorIndex then
      inc(Result);
  end;

var i:integer;
  AnUnitInfo: TUnitInfo;
begin
  // move all editor index of units:
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
  end;
  // move bookmarks
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    Bookmarks[i].EditorIndex:=MoveIndex(Bookmarks[i].EditorIndex);
    dec(i);
  end;
  SessionModified:=true;
end;

procedure TProject.MoveEditorIndex(OldEditorIndex, NewEditorIndex: integer);

  function MoveIndex(OldIndex: integer): integer;
  begin
    Result:=OldIndex;
    if OldIndex=OldEditorIndex then
      // this is the moving index
      Result:=NewEditorIndex
    else if OldIndex>OldEditorIndex then begin
      // right of OldPageIndex ...
      if OldIndex<=NewEditorIndex then
        // .. and left of NewEditorIndex
        // -> move left
        Dec(Result);
    end else begin
      // left of OldPageIndex ...
      if OldIndex>=NewEditorIndex then
        // .. and right of NewEditorIndex
        // -> move right
        Inc(Result);
    end;
  end;

var
  i:integer;
  AnUnitInfo: TUnitInfo;
begin
  if OldEditorIndex=NewEditorIndex then exit;
  // move all editor index of units:
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
  end;
  // move bookmarks
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    Bookmarks[i].EditorIndex:=MoveIndex(Bookmarks[i].EditorIndex);
    dec(i);
  end;
  SessionModified:=true;
end;

procedure TProject.AddToOrRemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.EditorIndex<0 then begin
    RemoveFromList(AnUnitInfo,uilWithEditorIndex);
  end else begin
    AddToList(AnUnitInfo,uilWithEditorIndex);
  end;
end;

procedure TProject.AddToOrRemoveFromComponentList(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.Component=nil then begin
    RemoveFromList(AnUnitInfo,uilWithComponent);
  end else begin
    AddToList(AnUnitInfo,uilWithComponent);
  end;
end;

procedure TProject.AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.Loaded then begin
    RemoveFromList(AnUnitInfo,uilLoaded);
  end else begin
    AddToList(AnUnitInfo,uilLoaded);
  end;
end;

procedure TProject.AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.IsAutoRevertLocked then begin
    RemoveFromList(AnUnitInfo,uilAutoRevertLocked);
  end else begin
    AddToList(AnUnitInfo,uilAutoRevertLocked);
  end;
end;

procedure TProject.AddToOrRemoveFromPartOfProjectList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.IsPartOfProject then begin
    RemoveFromList(AnUnitInfo,uilPartOfProject);
  end else begin
    AddToList(AnUnitInfo,uilPartOfProject);
  end;
end;

function TProject.GetTargetFilename: string;
begin
  Result:=FCompilerOptions.TargetFilename;
end;

procedure TProject.SetTargetFilename(const NewTargetFilename: string);
begin
  FCompilerOptions.TargetFilename:=NewTargetFilename;
end;

procedure TProject.SetEnableI18N(const AValue: boolean);
begin
  if FEnableI18N=AValue then exit;
  FEnableI18N:=AValue;
  Modified:=true;
end;

procedure TProject.SetPOOutputDirectory(const AValue: string);
begin
  if FPOOutputDirectory=AValue then exit;
  FPOOutputDirectory:=AValue;
  Modified:=true;
end;

function TProject.GetMainFilename: String;
begin
  if MainUnitID>=0 then Result:=MainUnitInfo.Filename
  else Result:='';
end;

function TProject.GetFirstPartOfProject: TUnitInfo;
begin
  Result:=FFirst[uilPartOfProject];
end;

function TProject.GetFirstLoadedUnit: TUnitInfo;
begin
  Result:=fFirst[uilLoaded];
end;

procedure TProject.EmbeddedObjectModified(Sender: TObject);
begin
  if Resources.Modified then
    Modified := True;
end;

procedure TProject.FixEditorIndex;
var
  List: TFPList;
  AnUnitInfo: TUnitInfo;
  i: Integer;
  NewActiveEditorIndexAtStart: LongInt;
begin
  List:=TFPList.Create;
  try
    AnUnitInfo:=FirstUnitWithEditorIndex;
    while AnUnitInfo<>nil do
    begin
      List.Add(AnUnitInfo);
      AnUnitInfo:=AnUnitInfo.NextUnitWithEditorIndex;
    end;
    List.Sort(TListSortCompare(@CompareUnitInfoWithEditorIndex));
    NewActiveEditorIndexAtStart:=-1;
    for i:=0 to List.Count-1 do
    begin
      AnUnitInfo:=TUnitInfo(List[i]);
      if (NewActiveEditorIndexAtStart<0)
      and (ActiveEditorIndexAtStart=AnUnitInfo.EditorIndex) then
        NewActiveEditorIndexAtStart:=i;
      AnUnitInfo.EditorIndex:=i;
    end;
    ActiveEditorIndexAtStart:=NewActiveEditorIndexAtStart;
  finally
    List.Free;
  end;
end;

function TProject.GetFirstAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fFirst[uilAutoRevertLocked];
end;

function TProject.GetFirstUnitWithComponent: TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
end;

function TProject.GetFirstUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fFirst[uilWithEditorIndex];
end;

function TProject.GetMainUnitInfo: TUnitInfo;
begin
  if (MainUnitID>=0) and (MainUnitID<UnitCount) then
    Result:=Units[MainUnitID]
  else
    Result:=nil;
end;

function TProject.GetProjectInfoFile:string;
begin
  Result:=fProjectInfoFile;
end;

procedure TProject.SetProjectInfoFile(const NewFilename:string);
var
  NewProjectInfoFile: String;
  OldProjectInfoFile: String;
  DefaultTitle: String;
begin
  NewProjectInfoFile:=TrimFilename(NewFilename);
  if NewProjectInfoFile='' then exit;
  DoDirSeparators(NewProjectInfoFile);
  if fProjectInfoFile=NewProjectInfoFile then exit;
  BeginUpdate(true);
  OldProjectInfoFile:=fProjectInfoFile;
  fProjectInfoFile:=NewProjectInfoFile;
  DefaultTitle:=ExtractFileNameOnly(OldProjectInfoFile);
  if (CompareText(Title,DefaultTitle)=0)
  or (OldProjectInfoFile='') or (Title='') then begin
    Title:=DefaultTitle;
  end;
  UpdateProjectDirectory;
  UpdateSessionFilename;
  if Assigned(OnChangeProjectInfoFile) then
    OnChangeProjectInfoFile(Self);
  FDefineTemplates.SourceDirectoriesChanged;
  Modified:=true;
  EndUpdate;
  //DebugLn('TProject.SetProjectInfoFile FDefineTemplates.FUpdateLock=',dbgs(FDefineTemplates.FUpdateLock));
end;

procedure TProject.SetSessionStorage(const AValue: TProjectSessionStorage);
begin
  if SessionStorage=AValue then exit;
  inherited SetSessionStorage(AValue);
  UpdateSessionFilename;
end;

function TProject.OnUnitFileBackup(const Filename: string): TModalResult;
begin
  if Assigned(fOnFileBackup) then
    Result:=fOnFileBackup(Filename)
  else
    Result:=mrOk;
end;

procedure TProject.OnLoadSaveFilename(var AFilename: string; Load:boolean);
{ This function is used after reading a filename from the config
  and before writing a filename to a config.
  The config can be the lpi or the session.
}
var
  ProjectPath: string;
  FileWasAbsolute: Boolean;
  
  function FileCanBeMadeRelative: boolean;
  begin
    Result:=false;
    if not FileWasAbsolute then exit;
    {$IFdef MSWindows}
    // check that the file is on the same drive / filesystem
    if CompareText(ExtractFileDrive(AFilename),ExtractFileDrive(ProjectPath))<>0
    then exit;
    {$ENDIF}
    Result:=true;
  end;
  
begin
  if AFileName='' then exit;
  //debugln('TProject.OnLoadSaveFilename A "',AFilename,'"');
  if (not fPathDelimChanged) or (not Load) then begin
    FileWasAbsolute:=FilenameIsAbsolute(AFileName);
  end else begin
    {$IFDEF MSWindows}
    // PathDelim changed from '/' to '\'
    FileWasAbsolute:=FilenameIsUnixAbsolute(AFileName);
    {$ELSE}
    // PathDelim changed from '\' to '/'
    FileWasAbsolute:=FilenameIsWinAbsolute(AFileName);
    {$ENDIF}
    DoDirSeparators(AFilename);
  end;
  AFilename:=TrimFilename(AFilename);
  
  ProjectPath:=ProjectDirectory;
  if ProjectPath<>'' then begin
    if Load then begin
      // make filename absolute
      if not FileWasAbsolute then
        AFilename:=TrimFilename(ProjectPath+AFilename);
    end else begin
      // try making filename relative to project file
      if FileCanBeMadeRelative then begin
        AFilename:=CreateRelativePath(AFilename,ProjectPath);
      end;
    end;
  end;

  if (not Load) then
    AFilename:=SwitchPathDelims(AFileName,fCurStorePathDelim);
  //debugln('TProject.OnLoadSaveFilename END "',AFilename,'" FileWasAbsolute=',dbgs(FileWasAbsolute));
end;

function TProject.RemoveProjectPathFromFilename(
  const AFilename: string): string;
var ProjectPath:string;
begin
  ProjectPath:=ProjectDirectory;
  if ProjectPath='' then ProjectPath:=GetCurrentDirUTF8;
  Result:=AFilename;
  DoDirSeparators(Result);
  // try making filename relative to project file
  if FilenameIsAbsolute(Result)
  and (CompareFileNames(copy(Result,1,length(ProjectPath)),ProjectPath)=0)
  then
    Result:=copy(Result,length(ProjectPath)+1,
         length(Result)-length(ProjectPath));
end;

function TProject.FileIsInProjectDir(const AFilename: string): boolean;
var ProjectDir, FilePath: string;
begin
  if FilenameIsAbsolute(AFilename) then begin
    if (not IsVirtual) then begin
      ProjectDir:=ProjectDirectory;
      FilePath:=LeftStr(AFilename,length(ProjectDir));
      Result:=(CompareFileNames(ProjectDir,FilePath)=0);
    end else
      Result:=false;
  end else
    Result:=true;
end;

procedure TProject.GetVirtualDefines(DefTree: TDefineTree;
  DirDef: TDirectoryDefines);
  
  procedure ExtendPath(const AVariable, APath: string);
  var
    TempValue: string;
  begin
    if APath<>'' then begin
      DefTree.ReadValue(DirDef,APath+';','',TempValue);
      DirDef.Values.Prepend(AVariable,TempValue);
    end;
  end;
  
begin
  if (not IsVirtual) then exit;
  ExtendPath(UnitPathMacroName,CompilerOptions.OtherUnitFiles);
  ExtendPath(IncludePathMacroName,CompilerOptions.IncludePath);
  ExtendPath(SrcPathMacroName,CompilerOptions.SrcPath);
end;

procedure TProject.GetUnitsChangedOnDisk(var AnUnitList: TFPList);
var
  AnUnitInfo: TUnitInfo;
begin
  AnUnitList:=nil;
  AnUnitInfo:=fFirst[uilAutoRevertLocked];
  while (AnUnitInfo<>nil) do begin
    if (AnUnitInfo.Source<>nil)
    and AnUnitInfo.ChangedOnDisk(false) then begin
      if AnUnitList=nil then
        AnUnitList:=TFPList.Create;
      AnUnitList.Add(AnUnitInfo);
    end;
    AnUnitInfo:=AnUnitInfo.fNext[uilAutoRevertLocked];
  end;
end;

procedure TProject.SetBookmark(AnUnitInfo: TUnitInfo; X, Y, ID: integer);
begin
  if AnUnitInfo.EditorIndex>=0 then
    Bookmarks.Add(X,Y,AnUnitInfo.EditorIndex,ID);
  AnUnitInfo.Bookmarks.Add(X,Y,ID);
end;

procedure TProject.MergeBookmarks(AnUnitInfo: TUnitInfo);
// merge the bookmarks of the unit with the bookmarks in the source editor
var
  i: Integer;
  UnitMark: TFileBookmark;
  ProjectMark: TProjectBookmark;
begin
  if AnUnitInfo.EditorIndex<0 then exit;
  for i:=0 to AnUnitInfo.Bookmarks.Count-1 do begin
    UnitMark:=AnUnitInfo.Bookmarks[i];
    ProjectMark:=Bookmarks.BookmarkWithIndex(UnitMark.ID);
    // merge the bookmark into the currently existing bookmarks, if the ID is
    // free
    //writeln('TProject.MergeBookmarks ',AnUnitInfo.Filename,' Y=',UnitMark.Y);
    if (ProjectMark=nil) then
      Bookmarks.Add(UnitMark.X,UnitMark.Y,AnUnitInfo.EditorIndex,UnitMark.ID);
  end;
end;

function TProject.FindDependencyByName(const PackageName: string
  ): TPkgDependency;
begin
  Result:=FindDependencyByNameInList(FFirstRequiredDependency,pdlRequires,
                                     PackageName);
end;

function TProject.RequiredDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRequiredDependency,pdlRequires,Index);
end;

function TProject.RemovedDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRemovedDependency,pdlRequires,Index);
end;

procedure TProject.AddRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.AddToList(FFirstRequiredDependency,pdlRequires);
  Dependency.Owner:=Self;
  Dependency.HoldPackage:=true;
  FDefineTemplates.CustomDefinesChanged;
  {$IFDEF VerboseAddProjPkg}
  DebugLn(['TProject.AddRequiredDependency ']);
  {$ENDIF}
  IncreaseCompilerParseStamp;
  Modified:=true;
  EndUpdate;
end;

procedure TProject.RemoveRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.RequiredPackage:=nil;
  Dependency.AddToList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=true;
  FDefineTemplates.CustomDefinesChanged;
  IncreaseCompilerParseStamp;
  Modified:=true;
  EndUpdate;
end;

procedure TProject.DeleteRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.Free;
  FDefineTemplates.CustomDefinesChanged;
  IncreaseCompilerParseStamp;
  EndUpdate;
end;

procedure TProject.DeleteRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Free;
  EndUpdate;
end;

procedure TProject.RemoveRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=false;
  EndUpdate;
end;

procedure TProject.ReaddRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  RemoveRemovedDependency(Dependency);
  AddRequiredDependency(Dependency);
  EndUpdate;
end;

procedure TProject.MoveRequiredDependencyUp(Dependency: TPkgDependency);
begin
  if Dependency.PrevRequiresDependency=nil then exit;
  BeginUpdate(true);
  Dependency.MoveUpInList(FFirstRequiredDependency,pdlRequires);
  FDefineTemplates.CustomDefinesChanged;
  IncreaseCompilerParseStamp;
  EndUpdate;
end;

procedure TProject.MoveRequiredDependencyDown(Dependency: TPkgDependency);
begin
  if Dependency.NextRequiresDependency=nil then exit;
  BeginUpdate(true);
  Dependency.MoveDownInList(FFirstRequiredDependency,pdlRequires);
  FDefineTemplates.CustomDefinesChanged;
  IncreaseCompilerParseStamp;
  EndUpdate;
end;

function TProject.Requires(APackage: TLazPackage; SearchRecursively: boolean
  ): boolean;
begin
  if SearchRecursively then
    Result:=PackageGraph.FindDependencyRecursively(FFirstRequiredDependency,
                                                   APackage)<>nil
  else
    Result:=FindCompatibleDependencyInList(FFirstRequiredDependency,pdlRequires,
                                           APackage)<>nil;
end;

procedure TProject.GetAllRequiredPackages(var List: TFPList);
begin
  if Assigned(OnGetAllRequiredPackages) then
    OnGetAllRequiredPackages(FirstRequiredDependency,List);
end;

procedure TProject.AddPackageDependency(const PackageName: string);
var
  PkgDependency: TPkgDependency;
begin
  if FindDependencyByNameInList(FirstRequiredDependency,pdlRequires,PackageName)
  <>nil then exit;
  PkgDependency:=TPkgDependency.Create;
  PkgDependency.PackageName:=PackageName;
  AddRequiredDependency(PkgDependency);
end;

procedure TProject.LockUnitComponentDependencies;
begin
  inc(FLockUnitComponentDependencies);
  if FLockUnitComponentDependencies=1 then begin
    // update once
    Include(FStateFlags,lpsfPropertyDependenciesChanged);
    Include(FStateFlags,lpsfDesignerChanged);
  end;
end;

procedure TProject.UnlockUnitComponentDependencies;
begin
  if FLockUnitComponentDependencies=0 then
    raise Exception.Create('');
  dec(FLockUnitComponentDependencies);
end;

procedure TProject.UpdateUnitComponentDependencies;

  procedure Search(AnUnitInfo: TUnitInfo; AComponent: TComponent);
  // search the published properties of AComponent for references to other units
  var
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    PropInfo: PPropInfo;
    CurCount: Word;
    ReferenceComponent: TComponent;
    OwnerComponent: TComponent;
    ReferenceUnit: TUnitInfo;
    Dependency: TUnitComponentDependency;
  begin
    if AComponent<>AnUnitInfo.Component then begin
      ReferenceUnit:=UnitWithComponentClass(TComponentClass(AComponent.ClassType));
      {$ifdef VerboseFormEditor}
      DebugLn(['Search UnitComponent=',DbgSName(AnUnitInfo.Component),' AComponent=',DbgSName(AComponent),' ReferenceUnit=',ReferenceUnit<>nil]);
      {$endif}
      if (ReferenceUnit<>nil) then begin
        // component class references another unit
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TProject.UpdateUnitComponentDependencies inline component found: ',DbgSName(AComponent),' ',AnUnitInfo.Filename,' -> ',ReferenceUnit.Filename]);
        {$ENDIF}
        AnUnitInfo.AddRequiresComponentDependency(
                             ReferenceUnit,[ucdtInlineClass]);
      end;
    end;
  
    // read all properties and remove doubles
    TypeInfo:=PTypeInfo(AComponent.ClassInfo);
    repeat
      // read all property infos of current class
      TypeData:=GetTypeData(TypeInfo);
      // skip unitname
      PropInfo:=PPropInfo(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
      // read property count
      CurCount:=PWord(PropInfo)^;
      inc(PtrUInt(PropInfo),SizeOf(Word));
      
      // read properties
      while CurCount>0 do begin
        // point PropInfo to next propinfo record.
        // Located at Name[Length(Name)+1] !
        if (PropInfo^.PropType^.Kind=tkClass) then begin
          // property of kind TObject
          ReferenceComponent:=TComponent(GetObjectProp(AComponent,PropInfo));
          //debugln('TProject.UpdateUnitComponentDependencies Property ',dbgsName(AComponent),' Name=',PropInfo^.Name,' Type=',PropInfo^.PropType^.Name,' Value=',dbgsName(ReferenceComponent),' TypeInfo=',TypeInfo^.Name);
          if ReferenceComponent is TComponent then begin
            // reference is a TComponent
            OwnerComponent:=ReferenceComponent;
            while OwnerComponent.Owner<>nil do
              OwnerComponent:=OwnerComponent.Owner;
            if OwnerComponent<>AnUnitInfo.Component then begin
              // property references a component that is not owned
              // by the current unit
              ReferenceUnit:=UnitWithComponent(OwnerComponent);
              if ReferenceUnit<>nil then begin
                // property references another unit
                {$IFDEF VerboseIDEMultiForm}
                DebugLn(['TProject.UpdateUnitComponentDependencies multi form reference found: ',AnUnitInfo.Filename,' -> ',ReferenceUnit.Filename]);
                {$ENDIF}
                AnUnitInfo.AddRequiresComponentDependency(
                                     ReferenceUnit,[ucdtProperty]);
                if FRevertLockCount>0 then begin
                  Dependency:=AnUnitInfo.AddRequiresComponentDependency(
                                       ReferenceUnit,[ucdtOldProperty]);
                  Dependency.SetUsedByPropPath(
                    Dependency.CreatePropPath(AComponent,PropInfo^.Name),
                    Dependency.CreatePropPath(ReferenceComponent));
                end;
              end;
            end;
          end;
        end;
        PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
        dec(CurCount);
      end;
      TypeInfo:=TypeData^.ParentInfo;
    until TypeInfo=nil;
  end;
  
  procedure DFSRequiredDesigner(AnUnitInfo, IgnoreUnitInfo: TUnitInfo);
  var
    Dependency: TUnitComponentDependency;
    UsingUnitInfo: TUnitInfo;
  begin
    if (AnUnitInfo=nil) or (AnUnitInfo.Component=nil)
    or (uifMarked in AnUnitInfo.FFlags) then
      exit;
    Include(AnUnitInfo.FFlags,uifMarked);
    Dependency:=AnUnitInfo.FirstRequiredComponent;
    while Dependency<>nil do begin
      UsingUnitInfo:=Dependency.RequiresUnit;
      if (UsingUnitInfo<>IgnoreUnitInfo)
      and (not (uifComponentIndirectlyUsedByDesigner in UsingUnitInfo.FFlags))
      then begin
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TProject.UpdateUnitComponentDependencies.DFSRequiredDesigner designer of ',AnUnitInfo.Filename,' uses ',UsingUnitInfo.Filename]);
        {$ENDIF}
        Include(UsingUnitInfo.FFlags,uifComponentIndirectlyUsedByDesigner);
        DFSRequiredDesigner(UsingUnitInfo,IgnoreUnitInfo);
      end;
      Dependency:=Dependency.NextRequiresDependency;
    end;
  end;

var
  AnUnitInfo: TUnitInfo;
  i: Integer;
begin
  if (FLockUnitComponentDependencies=0)
  or (lpsfPropertyDependenciesChanged in FStateFlags) then begin
    Exclude(FStateFlags,lpsfPropertyDependenciesChanged);
    // clear dependencies
    ClearUnitComponentDependencies([ucdtProperty,ucdtInlineClass]);
    {$IFDEF VerboseIDEMultiForm}
    DebugLn(['TProject.UpdateUnitComponentDependencies checking properties ...']);
    {$ENDIF}
    // find property dependencies
    AnUnitInfo:=FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      Search(AnUnitInfo,AnUnitInfo.Component);
      for i:=AnUnitInfo.Component.ComponentCount-1 downto 0 do
        Search(AnUnitInfo,AnUnitInfo.Component.Components[i]);
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
    //WriteDebugReportUnitComponentDependencies('P ');
  end;
  
  if (FLockUnitComponentDependencies=0)
  or (lpsfDesignerChanged in FStateFlags) then begin
    Exclude(FStateFlags,lpsfDesignerChanged);
    {$IFDEF VerboseIDEMultiForm}
    DebugLn(['TProject.UpdateUnitComponentDependencies checking designers ...']);
    {$ENDIF}
    // find designer dependencies
    AnUnitInfo:=FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      AnUnitInfo.FFlags:=AnUnitInfo.FFlags-
        [uifMarked,uifComponentIndirectlyUsedByDesigner,uifComponentUsedByDesigner];
      if FindRootDesigner(AnUnitInfo.Component)<>nil then begin
        {$IFDEF VerboseIDEMultiForm}
        DebugLn(['TProject.UpdateUnitComponentDependencies used by designer: ',AnUnitInfo.Filename]);
        {$ENDIF}
        Include(AnUnitInfo.FFlags,uifComponentUsedByDesigner);
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
    // mark all units that are used indirectly by a designer
    AnUnitInfo:=FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      if (uifComponentUsedByDesigner in AnUnitInfo.FFlags) then
      begin
        // mark all that use indirectly this designer
        Exclude(AnUnitInfo.FFlags,uifMarked);
        DFSRequiredDesigner(AnUnitInfo,AnUnitInfo);
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
    {$IFDEF VerboseTFrame}
    WriteDebugReportUnitComponentDependencies('UUCD ');
    {$ENDIF}
  end;
end;

procedure TProject.InvalidateUnitComponentDesignerDependencies;
begin
  Include(FStateFlags,lpsfDesignerChanged);
end;

procedure TProject.ClearUnitComponentDependencies(
  ClearTypes: TUnitCompDependencyTypes);
var
  i: Integer;
begin
  for i:=UnitCount-1 downto 0 do
    Units[i].ClearUnitComponentDependencies(ClearTypes);
end;

procedure TProject.FindUnitsUsingSubComponent(SubComponent: TComponent;
  List: TFPList; IgnoreOwner: boolean);

  procedure Search(AnUnitInfo: TUnitInfo; AComponent: TComponent);
  // search the published properties of AComponent for references to other units
  var
    TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    PropInfo: PPropInfo;
    CurCount: Word;
    ReferenceComponent: TComponent;
  begin
    if csDestroying in AComponent.ComponentState then exit;

    // read all properties and remove doubles
    TypeInfo:=PTypeInfo(AComponent.ClassInfo);
    repeat
      // read all property infos of current class
      TypeData:=GetTypeData(TypeInfo);
      // skip unitname
      PropInfo:=PPropInfo(PByte(@TypeData^.UnitName)+Length(TypeData^.UnitName)+1);
      // read property count
      CurCount:=PWord(PropInfo)^;
      inc(PtrUInt(PropInfo),SizeOf(Word));

      // read properties
      while CurCount>0 do begin
        // point PropInfo to next propinfo record.
        // Located at Name[Length(Name)+1] !
        if PropInfo^.PropType^.Kind=tkClass then begin
          // property of kind TObject
          ReferenceComponent:=TComponent(GetObjectProp(AComponent,PropInfo));
          //debugln('TProject.FindUnitsUsingSubComponent Property ',dbgsName(AComponent),' Name=',PropInfo^.Name,' Type=',PropInfo^.PropType^.Name,' Value=',dbgsName(ReferenceComponent),' TypeInfo=',TypeInfo^.Name);
          if ReferenceComponent=SubComponent then begin
            if List.IndexOf(AnUnitInfo)<0 then
              List.Add(AnUnitInfo);
          end;
        end;
        PropInfo:=PPropInfo(pointer(@PropInfo^.Name)+PByte(@PropInfo^.Name)^+1);
        dec(CurCount);
      end;
      TypeInfo:=TypeData^.ParentInfo;
    until TypeInfo=nil;
  end;

var
  AnUnitInfo: TUnitInfo;
  i: Integer;
  OwnerComponent: TComponent;
begin
  if SubComponent=nil then exit;
  if IgnoreOwner then begin
    OwnerComponent:=SubComponent;
    while OwnerComponent<>nil do
      OwnerComponent:=OwnerComponent.Owner;
  end else
    OwnerComponent:=nil;
  AnUnitInfo:=FirstUnitWithComponent;
  while AnUnitInfo<>nil do begin
    if csDestroying in AnUnitInfo.Component.ComponentState then continue;
    if AnUnitInfo.Component<>OwnerComponent then begin
      Search(AnUnitInfo,AnUnitInfo.Component);
      for i:=AnUnitInfo.Component.ComponentCount-1 downto 0 do
        Search(AnUnitInfo,AnUnitInfo.Component.Components[i]);
    end;
    AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
  end;
end;

procedure TProject.WriteDebugReportUnitComponentDependencies(Prefix: string);
var
  i: Integer;
  AnUnitInfo: TUnitInfo;
begin
  for i:=0 to UnitCount-1 do begin
    AnUnitInfo:=Units[i];
    if (AnUnitInfo.FirstUsedByComponent<>nil)
    or (AnUnitInfo.FirstRequiredComponent<>nil) then
      AnUnitInfo.WriteDebugReportUnitComponentDependencies(Prefix);
  end;
end;

procedure TProject.AddSrcPath(const SrcPathAddition: string);
begin
  CompilerOptions.SrcPath:=MergeSearchPaths(CompilerOptions.SrcPath,
                                            SetDirSeparators(SrcPathAddition));
end;

function TProject.GetSourceDirs(WithProjectDir, WithoutOutputDir: boolean
  ): string;
begin
  Result:=SourceDirectories.CreateSearchPathFromAllFiles;
  if WithProjectDir then
    Result:=MergeSearchPaths(Result,ProjectDirectory);
  if WithoutOutputDir then
    Result:=RemoveSearchPaths(Result,GetOutputDirectory);
end;

function TProject.GetOutputDirectory: string;
begin
  if IsVirtual then
    Result:=GetTestDirectory
  else
    Result:=CompilerOptions.ParsedOpts.GetParsedValue(pcosOutputDir);
end;

function TProject.GetCompilerFilename: string;
begin
  Result:=CompilerOptions.ParsedOpts.GetParsedValue(pcosCompilerPath);
end;

function TProject.GetStateFilename: string;
begin
  Result:=GetOutputDirectory
          +ChangeFileExt(GetCompileSourceFilename,'.compiled');
end;

function TProject.GetTestDirectory: string;
begin
  if Assigned(OnGetTestDirectory) then
    OnGetTestDirectory(Self,Result)
  else
    Result:=GetCurrentDirUTF8;
end;

function TProject.GetCompileSourceFilename: string;
begin
  if MainUnitID<0 then
    Result:=''
  else
    Result:=ExtractFilename(MainUnitInfo.Filename);
end;

procedure TProject.AutoAddOutputDirToIncPath;
var
  IncPath: String;
begin
  if pfLRSFilesInOutputDirectory in Flags then begin
    // the .lrs files are auto created in the output directory
    // => make sure the project output directory is in the include path
    IncPath:=CompilerOptions.IncludePath;
    if SearchDirectoryInSearchPath(IncPath,'$(ProjOutDir)')<1 then
      CompilerOptions.IncludePath:=MergeSearchPaths(IncPath,';$(ProjOutDir)');
  end;
end;

function TProject.LoadStateFile(IgnoreErrors: boolean): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  CurStateFileAge: Integer;
begin
  StateFile:=GetStateFilename;
  if not FileExistsUTF8(StateFile) then begin
    DebugLn('TProject.DoLoadStateFile Statefile not found: ',StateFile);
    StateFlags:=StateFlags-[lpsfStateFileLoaded];
    Result:=mrOk;
    exit;
  end;

  // read the state file
  CurStateFileAge:=FileAgeUTF8(StateFile);
  if (not (lpsfStateFileLoaded in StateFlags))
  or (StateFileDate<>CurStateFileAge) then
  begin
    StateFlags:=StateFlags-[lpsfStateFileLoaded];
    try
      XMLConfig:=TXMLConfig.Create(StateFile);
      try
        LastCompilerFilename:=XMLConfig.GetValue('Compiler/Value','');
        LastCompilerFileDate:=XMLConfig.GetValue('Compiler/Date',0);
        LastCompilerParams:=XMLConfig.GetValue('Params/Value','');
      finally
        XMLConfig.Free;
      end;
      StateFileDate:=CurStateFileAge;
    except
      on E: Exception do begin
        if IgnoreErrors then begin
          Result:=mrOk;
        end else begin
          Result:=MessageDlg(lisPkgMangErrorReadingFile,
            Format(lisProjMangUnableToReadStateFileOfProjectError, [StateFile,
              IDAsString, #13, E.Message]),
            mtError,[mbAbort],0);
        end;
        exit;
      end;
    end;
    StateFlags:=StateFlags+[lpsfStateFileLoaded];
  end;

  Result:=mrOk;
end;

function TProject.SaveStateFile(const CompilerFilename, CompilerParams: string
  ): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  CompilerFileDate: Integer;
begin
  StateFile:=GetStateFilename;
  try
    CompilerFileDate:=FileAgeUTF8(CompilerFilename);
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
    LastCompilerFilename:=CompilerFilename;
    LastCompilerFileDate:=CompilerFileDate;
    LastCompilerParams:=CompilerParams;
    StateFileDate:=FileAgeUTF8(StateFile);
    StateFlags:=StateFlags+[lpsfStateFileLoaded];
  except
    on E: Exception do begin
      Result:=MessageDlg(lisPkgMangErrorWritingFile,
        Format(lisProjMangUnableToWriteStateFileForProjectError, [IDAsString,
          #13, E.Message]),
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;
  Result:=mrOk;
end;

procedure TProject.UpdateAllCustomHighlighter;
var
  i: Integer;
  AnUnitInfo: TUnitInfo;
begin
  for i:=0 to UnitCount-1 do begin
    AnUnitInfo:=Units[i];
    AnUnitInfo.CustomHighlighter:=AnUnitInfo.SyntaxHighlighter
         <>FilenameToLazSyntaxHighlighter(AnUnitInfo.Filename);
  end;
end;

procedure TProject.UpdateAllSyntaxHighlighter;
var
  AnUnitInfo: TUnitInfo;
  i: Integer;
begin
  for i:=0 to UnitCount-1 do begin
    AnUnitInfo:=Units[i];
    if not AnUnitInfo.CustomHighlighter then
      AnUnitInfo.SyntaxHighlighter:=
        FilenameToLazSyntaxHighlighter(AnUnitInfo.Filename);
  end;
end;

function TProject.GetPOOutDirectory: string;
begin
  Result:=POOutputDirectory;
  IDEMacros.SubstituteMacros(Result);
  Result:=TrimFilename(Result);
  LongenFilename(Result);
end;

procedure TProject.OnUnitNameChange(AnUnitInfo: TUnitInfo;
  const OldUnitName, NewUnitName: string; CheckIfAllowed: boolean;
  var Allowed: boolean);
var
  i:integer;
begin
  if AnUnitInfo.IsPartOfProject then
  begin
    if CheckIfAllowed then begin
      // check if no other project unit has this name
      for i:=0 to UnitCount-1 do begin
        if (Units[i].IsPartOfProject)
        and (Units[i]<>AnUnitInfo) and (Units[i].AUnitName<>'')
        and (CompareText(Units[i].AUnitName,NewUnitName)=0) then begin
          Allowed:=false;
          exit;
        end;
      end;
    end;
    if (OldUnitName<>'') then
    begin
      if (pfMainUnitHasUsesSectionForAllUnits in Flags) then
      begin
        // rename unit in program uses section
        CodeToolBoss.RenameUsedUnit(MainUnitInfo.Source, OldUnitName,
          NewUnitName, '');
      end;
      if MainUnitInfo = AnUnitInfo then
      begin
        // we are renaming a project => update resource directives
        Resources.RenameDirectives(OldUnitName, NewUnitName);
      end;
    end;
  end;
end;

procedure TProject.SetAutoOpenDesignerFormsDisabled(const AValue: boolean);
begin
  if FAutoOpenDesignerFormsDisabled=AValue then exit;
  FAutoOpenDesignerFormsDisabled:=AValue;
end;

procedure TProject.SetCompilerOptions(const AValue: TProjectCompilerOptions);
begin
  if FCompilerOptions=AValue then exit;
  FCompilerOptions:=AValue;
  inherited SetLazCompilerOptions(AValue);
end;

procedure TProject.SetMainProject(const AValue: boolean);
begin
  if MainProject=AValue then exit;
  FMainProject:=AValue;
  if MainProject then
    SourceDirectories.AddFilename(VirtualDirectory)
  else
    SourceDirectories.RemoveFilename(VirtualDirectory);
end;

procedure TProject.SetSkipCheckLCLInterfaces(const AValue: boolean);
begin
  if FSkipCheckLCLInterfaces=AValue then exit;
  FSkipCheckLCLInterfaces:=AValue;
  SessionModified:=true;
end;

function TProject.JumpHistoryCheckPosition(
  APosition: TProjectJumpHistoryPosition): boolean;
var i: integer;
begin
  i:=IndexOfFilename(APosition.Filename);
  Result:=(i>=0) and (Units[i].EditorIndex>=0);
end;

function TProject.SomethingModified(CheckData, CheckSession: boolean): boolean;
var
  i: integer;
begin
  Result := True;
  if CheckData then
  begin
    if Modified then
    begin
      {$IFDEF VerboseProjectModified}
      DebugLn('TProject.SomethingModified Modified');
      {$ENDIF}
      Exit;
    end;
    if CompilerOptions.Modified then
    begin
      {$IFDEF VerboseProjectModified}
      DebugLn(['TProject.SomethingModified CompilerOptions']);
      {$ENDIF}
      Exit;
    end;
    for i := 0 to UnitCount - 1 do
      if (Units[i].IsPartOfProject) and Units[i].Modified then
      begin
        {$IFDEF VerboseProjectModified}
        DebugLn('TProject.SomethingModified PartOfProject ',Units[i].Filename);
        {$ENDIF}
        Exit;
      end;
  end;
  if CheckSession then
  begin
    if SessionModified then
    begin
      {$IFDEF VerboseProjectModified}
      DebugLn('TProject.SomethingModified SessionModified');
      {$ENDIF}
      Exit;
    end;
    for i := 0 to UnitCount - 1 do
    begin
      if Units[i].SessionModified then
      begin
        {$IFDEF VerboseProjectModified}
        DebugLn('TProject.SomethingModified Session ',Units[i].Filename);
        {$ENDIF}
        exit;
      end;
      if (not Units[i].IsPartOfProject) and Units[i].Modified then
      begin
        {$IFDEF VerboseProjectModified}
        DebugLn('TProject.SomethingModified Not PartOfProject ',Units[i].Filename);
        {$ENDIF}
        exit;
      end;
    end;
  end;
  Result := False;
end;

procedure TProject.MainSourceFilenameChanged;
begin

end;

function TProject.UnitWithComponent(AComponent: TComponent): TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
  while (Result<>nil) and (Result.Component<>AComponent) do
    Result:=Result.fNext[uilWithComponent];
end;

function TProject.UnitWithComponentClass(AClass: TComponentClass): TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
  while (Result<>nil) and (Result.Component.ClassType<>AClass) do
    Result:=Result.fNext[uilWithComponent];
end;

function TProject.UnitWithComponentClassName(const AClassName: string
  ): TUnitInfo;
begin
  Result := fFirst[uilWithComponent];
  while (Result<>nil)
  and (SysUtils.CompareText(Result.Component.ClassName, AClassName) <> 0) do
    Result := Result.fNext[uilWithComponent];
end;

function TProject.UnitWithComponentName(AComponentName: String): TUnitInfo;
begin
  Result := fFirst[uilPartOfProject];
  while (Result<>nil)
  and (SysUtils.CompareText(Result.ComponentName, AComponentName) <> 0) do
    Result := Result.fNext[uilPartOfProject];
end;

function TProject.UnitComponentInheritingFrom(AClass: TComponentClass;
  Ignore: TUnitInfo): TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
  while (Result<>nil) do begin
    if (Result<>Ignore) and Result.Component.InheritsFrom(AClass) then exit;
    Result:=Result.fNext[uilWithComponent];
  end;
end;

function TProject.UnitUsingComponentUnit(ComponentUnit: TUnitInfo;
  Types: TUnitCompDependencyTypes): TUnitInfo;
var
  Dependency: TUnitComponentDependency;
begin
  Result:=nil;
  Dependency:=ComponentUnit.FindUsedByComponentDependency(Types);
  if Dependency=nil then exit;
  Result:=Dependency.UsedByUnit;
end;

function TProject.UnitComponentIsUsed(ComponentUnit: TUnitInfo;
  CheckHasDesigner: boolean): boolean;
begin
  if ComponentUnit.Component=nil then exit(false);
  if CheckHasDesigner
  and (uifComponentUsedByDesigner in ComponentUnit.Flags) then
    exit(true);
  if (uifComponentIndirectlyUsedByDesigner in ComponentUnit.Flags) then
    exit(true);
  if ComponentUnit.FindUsedByComponentDependency([ucdtAncestor])<>nil then
    exit(true);
  if ComponentUnit.FindUsedByComponentDependency([ucdtInlineClass])<>nil then
    exit(true);
  Result:=false;
end;

function TProject.UnitInfoWithFilename(const AFilename: string): TUnitInfo;
var
  i: Integer;
begin
  i:=IndexOfFilename(AFilename);
  if i>=0 then
    Result:=Units[i]
  else
    Result:=nil;
end;

function TProject.UnitInfoWithFilename(const AFilename: string;
  SearchFlags: TProjectFileSearchFlags): TUnitInfo;

  function MakeFilenameComparable(const TheFilename: string): string;
  begin
    Result:=TheFilename;
    if (pfsfResolveFileLinks in SearchFlags)
    and FilenameIsAbsolute(Result) then
      Result:=ReadAllLinks(Result,false);
  end;

  function FindFileInList(ListType: TUnitInfoList): TUnitInfo;
  var
    BaseFilename: String;
    CurBaseFilename: String;
  begin
    BaseFilename:=MakeFilenameComparable(AFilename);
    Result:=fFirst[ListType];
    while Result<>nil do begin
      CurBaseFilename:=MakeFilenameComparable(Result.Filename);
      if CompareFilenames(BaseFilename,CurBaseFilename)=0 then exit;
      Result:=Result.fNext[ListType];
    end;
  end;

var
  i: Integer;
begin
  if (SearchFlags-[pfsfResolveFileLinks]=[pfsfOnlyEditorFiles]) then
    // search only in list of Files with EditorIndex
    // There is a list, so we can search much faster
    Result:=FindFileInList(uilWithEditorIndex)
  else if (SearchFlags-[pfsfResolveFileLinks]=[pfsfOnlyProjectFiles]) then
    // search only in list of project files
    // There is a list, so we can search much faster
    Result:=FindFileInList(uilPartOfProject)
  else begin
    // slow search
    i:=IndexOfFilename(AFilename,SearchFlags);
    if i>=0 then
      Result:=Units[i]
    else
      Result:=nil;
  end;
end;

function TProject.UnitWithUnitname(const AnUnitname: string): TUnitInfo;
var
  i: Integer;
begin
  i:=IndexOfUnitWithName(AnUnitName,true,nil);
  if i>=0 then
    Result:=Units[i]
  else
    Result:=nil;
end;

function TProject.SearchFile(const ShortFilename: string;
  SearchFlags: TSearchIDEFileFlags): TUnitInfo;
var
  SearchedFilename: String;

  function FilenameFits(AFilename: string): boolean;
  begin
    if siffIgnoreExtension in SearchFlags then
      AFileName:=ExtractFilenameOnly(AFileName);
    if FilenameIsAbsolute(AFileName) then
      AFileName:=ExtractFilename(AFileName);
    if siffCaseSensitive in SearchFlags then
      Result:=SearchedFilename=AFilename
    else
      Result:=AnsiCompareText(SearchedFilename,AFilename)=0;
  end;
  
begin
  SearchedFilename:=ShortFilename;
  if siffIgnoreExtension in SearchFlags then
    SearchedFilename:=ExtractFilenameOnly(SearchedFilename);

  // search in files which are part of the project
  Result:=FirstPartOfProject;
  while Result<>nil do begin
    if FilenameFits(Result.Filename) then exit;
    Result:=Result.NextPartOfProject;
  end;
  // search in files opened in editor
  if not (siffDoNotCheckOpenFiles in SearchFlags) then begin
    Result:=FirstUnitWithEditorIndex;
    while Result<>nil do begin
      if FilenameFits(Result.Filename) then exit;
      Result:=Result.NextUnitWithEditorIndex;
    end;
  end;
end;

function TProject.FindFile(const AFilename: string;
  SearchFlags: TProjectFileSearchFlags): TLazProjectFile;
begin
  Result:=UnitInfoWithFilename(AFilename, SearchFlags);
end;

function TProject.IndexOfFilename(const AFilename: string): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if CompareFilenames(AFilename,Units[Result].Filename)=0 then exit;
    dec(Result);
  end;
end;

function TProject.IndexOfFilename(const AFilename: string;
  SearchFlags: TProjectFileSearchFlags): integer;

  function MakeFilenameComparable(const TheFilename: string): string;
  begin
    Result:=TheFilename;
    if (pfsfResolveFileLinks in SearchFlags)
    and (FilenameIsAbsolute(Result)) then
      Result:=ReadAllLinks(Result,false);
  end;

var
  BaseFilename: String;
  CurBaseFilename: String;
begin
  BaseFilename:=MakeFilenameComparable(AFilename);
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (pfsfOnlyEditorFiles in SearchFlags)
    and (Units[Result].EditorIndex<0) then begin
      dec(Result);
      continue;
    end;
    if (pfsfOnlyVirtualFiles in SearchFlags)
    and (not Units[Result].IsVirtual) then begin
      dec(Result);
      continue;
    end;
    if (pfsfOnlyProjectFiles in SearchFlags)
    and (not Units[Result].IsPartOfProject) then begin
      dec(Result);
      continue;
    end;
    CurBaseFilename:=MakeFilenameComparable(Units[Result].Filename);
    if CompareFilenames(BaseFilename,CurBaseFilename)=0 then exit;
    dec(Result);
  end;
end;

function TProject.ProjectUnitWithFilename(const AFilename: string): TUnitInfo;
begin
  Result:=fFirst[uilPartOfProject];
  while Result<>nil do begin
    if CompareFileNames(AFilename,Result.Filename)=0 then exit;
    Result:=Result.fNext[uilPartOfProject];
  end;
end;

function TProject.ProjectUnitWithShortFilename(const ShortFilename: string
  ): TUnitInfo;
begin
  Result:=fFirst[uilPartOfProject];
  while Result<>nil do begin
    if CompareFileNames(ShortFilename,ExtractFilename(Result.Filename))=0 then
      exit;
    Result:=Result.fNext[uilPartOfProject];
  end;
end;

function TProject.ProjectUnitWithUnitname(const AnUnitName: string): TUnitInfo;
begin
  Result:=fFirst[uilPartOfProject];
  while Result<>nil do begin
    if CompareText(AnUnitName,Result.AUnitName)=0 then exit;
    Result:=Result.fNext[uilPartOfProject];
  end;
end;

procedure TProject.UpdateProjectDirectory;
begin
  fProjectDirectory:=ExtractFilePath(fProjectInfoFile);
  CompilerOptions.BaseDirectory:=fProjectDirectory;
  if fProjectDirectory<>fProjectDirectoryReferenced then begin
    if fProjectDirectoryReferenced<>'' then
      FSourceDirectories.RemoveFilename(fProjectDirectoryReferenced);
    if fProjectDirectory<>'' then
      FSourceDirectories.AddFilename(fProjectDirectory);
    fProjectDirectoryReferenced:=fProjectDirectory;
  end;
end;

procedure TProject.UpdateSessionFilename;
begin
  case SessionStorage of
  pssInProjectInfo: ProjectSessionFile:=ProjectInfoFile;
  pssInProjectDir: ProjectSessionFile:=ChangeFileExt(ProjectInfoFile,'.lps');
  pssInIDEConfig: ProjectSessionFile:=
                               AppendPathDelim(GetProjectSessionsConfigPath)
                               +ExtractFileNameOnly(ProjectInfoFile)+'.lps';
  pssNone: ProjectSessionFile:='';
  end;
end;

procedure TProject.UpdateSourceDirectories;
var
  Cnt: Integer;
  i: Integer;
  AnUnitInfo: TUnitInfo;
begin
  Cnt:=FUnitList.Count;
  for i:=0 to Cnt-1 do begin
    AnUnitInfo:=Units[i];
    AnUnitInfo.FSourceDirectoryReferenced:=false;
  end;
  ClearSourceDirectories;
  for i:=0 to Cnt-1 do begin
    AnUnitInfo:=Units[i];
    AnUnitInfo.AutoReferenceSourceDir:=true;
    AnUnitInfo.UpdateSourceDirectoryReference;
  end;
  //DebugLn('TProject.UpdateSourceDirectories B ',UnitCount,' "',fSourceDirectories.CreateSearchPathFromAllFiles,'"');
end;

procedure TProject.ClearSourceDirectories;
begin
  FSourceDirectories.Clear;
  fProjectDirectoryReferenced:='';
  if MainProject then
    FSourceDirectories.AddFilename(VirtualDirectory);
  if (fProjectDirectory<>'') then begin
    FSourceDirectories.AddFilename(fProjectDirectory);
    fProjectDirectoryReferenced:=fProjectDirectory;
  end;
end;

procedure TProject.SourceDirectoriesChanged(Sender: TObject);
begin
  FDefineTemplates.SourceDirectoriesChanged;
end;

function TProject.GetMainFile: TLazProjectFile;
begin
  Result:=MainUnitInfo;
end;

function TProject.GetMainFileID: Integer;
begin
  Result:=MainUnitID;
end;

procedure TProject.SetMainFileID(const AValue: Integer);
begin
  MainUnitID:=AValue;
end;

procedure TProject.AddToList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirst[ListType]<>AnUnitInfo)
  and (AnUnitInfo.fNext[ListType]=nil)
  and (AnUnitInfo.fPrev[ListType]=nil) then begin
    AnUnitInfo.fNext[ListType]:=fFirst[ListType];
    AnUnitInfo.fPrev[ListType]:=nil;
    fFirst[ListType]:=AnUnitInfo;
    if AnUnitInfo.fNext[ListType]<>nil then
      AnUnitInfo.fNext[ListType].fPrev[ListType]:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromList(AnUnitInfo: TUnitInfo;
  ListType: TUnitInfoList);
begin
  // remove from list if AnUnitInfo is in list
  if fFirst[ListType]=AnUnitInfo then
    fFirst[ListType]:=AnUnitInfo.fNext[ListType];
  if AnUnitInfo.fNext[ListType]<>nil then
    AnUnitInfo.fNext[ListType].fPrev[ListType]:=
      AnUnitInfo.fPrev[ListType];
  if AnUnitInfo.fPrev[ListType]<>nil then
    AnUnitInfo.fPrev[ListType].fNext[ListType]:=
      AnUnitInfo.fNext[ListType];
  AnUnitInfo.fNext[ListType]:=nil;
  AnUnitInfo.fPrev[ListType]:=nil;
end;

{ TProjectCompilationToolOptions }

procedure TProjectCompilationToolOptions.Clear;
begin
  inherited Clear;
  CompileReasons := crAll;
end;

procedure TProjectCompilationToolOptions.CreateDiff(
  CompOpts: TCompilationToolOptions; Tool: TCompilerDiffTool);
begin
  if (CompOpts is TProjectCompilationToolOptions) then begin
    AddCompileReasonsDiff(Tool,'CompileReasons',CompileReasons,
                       TProjectCompilationToolOptions(CompOpts).CompileReasons);
  end else begin
    Tool.Differ:=true;
  end;
  inherited CreateDiff(CompOpts, Tool);
end;

procedure TProjectCompilationToolOptions.Assign(Src: TCompilationToolOptions);
begin
  inherited Assign(Src);
  if Src is TProjectCompilationToolOptions
  then begin
    CompileReasons := TProjectCompilationToolOptions(Src).CompileReasons;
  end
  else begin
    CompileReasons := crAll;
  end;
end;

procedure TProjectCompilationToolOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; DoSwitchPathDelims: boolean);
begin
  inherited LoadFromXMLConfig(XMLConfig, Path, DoSwitchPathDelims);
  CompileReasons := LoadXMLCompileReasons(XMLConfig, Path+'CompileReasons/',
                                          DefaultCompileReasons);
end;

procedure TProjectCompilationToolOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch);
begin
  inherited SaveToXMLConfig(XMLConfig, Path, UsePathDelim);
  SaveXMLCompileReasons(XMLConfig, Path+'CompileReasons/', CompileReasons,
                        DefaultCompileReasons);
end;

{ TProjectCompilerOptions }

procedure TProjectCompilerOptions.LoadTheCompilerOptions(const APath: string);
begin
  inherited LoadTheCompilerOptions(APath);
  
  // old compatebility
  if XMLConfigFile.GetValue(APath+'SkipCompiler/Value',false)
  then FCompileReasons := []
  else FCompileReasons :=
                   LoadXMLCompileReasons(XMLConfigFile,APath+'CompileReasons/',
                                         crAll);
end;

procedure TProjectCompilerOptions.SaveTheCompilerOptions(const APath: string);
begin
  inherited SaveTheCompilerOptions(APath);
  
  SaveXMLCompileReasons(XMLConfigFile, APath+'CompileReasons/', FCompileReasons,
                        crAll);
end;

procedure TProjectCompilerOptions.SetTargetCPU(const AValue: string);
begin
  inherited SetTargetCPU(AValue);
  FGlobals.TargetCPU:=TargetCPU;
end;

procedure TProjectCompilerOptions.SetTargetOS(const AValue: string);
begin
  inherited SetTargetOS(AValue);
  FGlobals.TargetOS:=TargetOS;
end;

procedure TProjectCompilerOptions.SetCustomOptions(const AValue: string);
begin
  if CustomOptions=AValue then exit;
  InvalidateOptions;
  inherited SetCustomOptions(AValue);
  if Project<>nil then
    Project.DefineTemplates.CustomDefinesChanged;
end;

procedure TProjectCompilerOptions.SetIncludePaths(const AValue: string);
begin
  if IncludePath=AValue then exit;
  InvalidateOptions;
  inherited SetIncludePaths(AValue);
end;

procedure TProjectCompilerOptions.SetLibraryPaths(const AValue: string);
begin
  if Libraries=AValue then exit;
  InvalidateOptions;
  inherited SetLibraryPaths(AValue);
end;

procedure TProjectCompilerOptions.SetLinkerOptions(const AValue: string);
begin
  if LinkerOptions=AValue then exit;
  InvalidateOptions;
  inherited SetLinkerOptions(AValue);
end;

procedure TProjectCompilerOptions.SetObjectPath(const AValue: string);
begin
  if ObjectPath=AValue then exit;
  InvalidateOptions;
  inherited SetObjectPath(AValue);
end;

procedure TProjectCompilerOptions.SetSrcPath(const AValue: string);
begin
  if SrcPath=AValue then exit;
  InvalidateOptions;
  inherited SetSrcPath(AValue);
end;

procedure TProjectCompilerOptions.SetUnitPaths(const AValue: string);
begin
  if OtherUnitFiles=AValue then exit;
  InvalidateOptions;
  inherited SetUnitPaths(AValue);
end;

procedure TProjectCompilerOptions.SetUnitOutputDir(const AValue: string);
begin
  if UnitOutputDirectory=AValue then exit;
  InvalidateOptions;
  inherited SetUnitOutputDir(AValue);
  if Project<>nil then
    Project.DefineTemplates.OutputDirectoryChanged;
end;

procedure TProjectCompilerOptions.Assign(Source: TPersistent);
var
  ProjCompOptions: TProjectCompilerOptions;
begin
  inherited Assign(Source);
  if Source is TProjectCompilerOptions then begin
    ProjCompOptions:=TProjectCompilerOptions(Source);
    FCompileReasons := ProjCompOptions.FCompileReasons;
    FBuildModes.Assign(ProjCompOptions.BuildModes);
  end else begin
    FCompileReasons := [crCompile, crBuild, crRun];
    // keep BuildModes
  end;
  UpdateGlobals;
end;

function TProjectCompilerOptions.IsEqual(CompOpts: TBaseCompilerOptions
  ): boolean;
begin
  Result:=false;
  if not inherited IsEqual(CompOpts) then exit;
  if CompOpts is TProjectCompilerOptions then begin
    if not TProjectCompilerOptions(CompOpts).BuildModes.IsEqual(BuildModes) then
      exit;
  end;
  Result:=true;
end;

procedure TProjectCompilerOptions.CreateDiff(CompOpts: TBaseCompilerOptions;
  Tool: TCompilerDiffTool);
begin
  if (CompOpts is TProjectCompilerOptions) then begin
    AddCompileReasonsDiff(Tool,'CompileReasons',FCompileReasons,
                          TProjectCompilerOptions(CompOpts).FCompileReasons);
  end else begin
    Tool.Differ:=true;
  end;
  inherited CreateDiff(CompOpts, Tool);
end;

procedure TProjectCompilerOptions.InvalidateOptions;
begin
  if (Project=nil) then exit;
  // TODO: propagate change to all dependant projects
end;

function TProjectCompilerOptions.GetEffectiveLCLWidgetType: string;
begin
  if OwnerProject.Requires(PackageGraph.LCLPackage,true) then
    Result:=inherited GetEffectiveLCLWidgetType
  else
    Result:=LCLPlatformDirNames[lpNoGUI];
end;

procedure TProjectCompilerOptions.UpdateGlobals;
begin
  FGlobals.TargetCPU:=TargetCPU;
  FGlobals.TargetOS:=TargetOS;
end;

constructor TProjectCompilerOptions.Create(const AOwner: TObject);
begin
  FGlobals := TGlobalCompilerOptions.Create;
  FBuildModes:=TBuildModeGraph.Create;
  FCompileReasons := [crCompile, crBuild, crRun];
  inherited Create(AOwner, TProjectCompilationToolOptions);
  with TProjectCompilationToolOptions(ExecuteBefore) do begin
    DefaultCompileReasons:=crAll;
    CompileReasons:=DefaultCompileReasons;
  end;
  with TProjectCompilationToolOptions(ExecuteAfter) do begin
    DefaultCompileReasons:=crAll;
    CompileReasons:=DefaultCompileReasons;
  end;
  UpdateGlobals;
  if AOwner <> nil
  then FOwnerProject := AOwner as TProject;
end;

destructor TProjectCompilerOptions.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FGlobals);
  FreeAndNil(FBuildModes);
end;

procedure TProjectCompilerOptions.Clear;
begin
  inherited Clear;
  FBuildModes.ClearModes;
end;

function TProjectCompilerOptions.GetOwnerName: string;
begin
  Result:=OwnerProject.Title;
  if Result='' then Result:=ExtractFilename(OwnerProject.ProjectInfoFile);
end;

function TProjectCompilerOptions.GetDefaultMainSourceFileName: string;
var
  MainUnitInfo: TUnitInfo;
begin
  MainUnitInfo:=FOwnerProject.MainUNitInfo;
  if (MainUnitInfo<>nil) then
    Result:=ExtractFileName(MainUnitInfo.Filename);
  if Result='' then
    Result:=inherited GetDefaultMainSourceFileName;
end;

procedure TProjectCompilerOptions.GetInheritedCompilerOptions(
  var OptionsList: TFPList);
var
  PkgList: TFPList;
begin
  PkgList:=nil;
  OwnerProject.GetAllRequiredPackages(PkgList);
  OptionsList:=GetUsageOptionsList(PkgList);
  PkgList.Free;
end;

{ TProjectDefineTemplates }

procedure TProjectDefineTemplates.SetActive(const AValue: boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if not FActive then Clear else AllChanged;
end;

procedure TProjectDefineTemplates.UpdateMain;
begin
  //DebugLn('TProjectDefineTemplates.UpdateMain ',Project.IDAsString,' Active=',dbgs(Active));
  // update the package block define template (the container for all other
  // define templates of the project)
  if (FMain=nil) and (not Project.Destroying) then begin
    // create the main project template
    FMain:=CreateProjectTemplateWithID(Project.IDAsWord);
    FMain.SetDefineOwner(Owner,false);
    FMain.SetFlags([dtfAutoGenerated],[],false);
  end else
    FMain.Name:=Project.IDAsWord;
  // ClearCache is here unnessary, because it is only a block
end;

procedure TProjectDefineTemplates.UpdateSrcDirIfDef;
var
  NewValue: String;
  Changed: Boolean;
  UnitPathDefTempl: TDefineTemplate;
  IncPathDefTempl: TDefineTemplate;
  SrcPathDefTempl: TDefineTemplate;
begin
  // The options are enclosed by an
  // IFDEF #ProjectSrcMark<PckId> template.
  // Each source directory defines this variable, so that the settings can be
  // activated for each source directory by a simple DEFINE.
  if (FMain=nil) then UpdateMain;
  if FSrcDirectories=nil then begin
    FSrcDirectories:=TDefineTemplate.Create('Source Directories',
      'Source Directories','','',
      da_Block);
    FMain.AddChild(FSrcDirectories);
  end;
  if FSrcDirIfDef=nil then begin
    FSrcDirIfDef:=TDefineTemplate.Create('Source Directory Additions',
      'Additional defines for project source directories',
      '#ProjectSrcMark'+Project.IDAsWord,'',
      da_IfDef);
    FMain.AddChild(FSrcDirIfDef);
    
    // create unit path template for this directory
    UnitPathDefTempl:=TDefineTemplate.Create('UnitPath', lisPkgDefsUnitPath,
      '#UnitPath','$(#UnitPath);$ProjectUnitPath('+Project.IDAsString+')',
      da_Define);
    FSrcDirIfDef.AddChild(UnitPathDefTempl);

    // create include path template for this directory
    IncPathDefTempl:=TDefineTemplate.Create('IncPath','Include Path',
      '#IncPath','$(#IncPath);$ProjectIncPath('+Project.IDAsString+')',
      da_Define);
    FSrcDirIfDef.AddChild(IncPathDefTempl);

    // create src path template for this directory
    SrcPathDefTempl:=TDefineTemplate.Create('SrcPath','Src Path',
      '#SrcPath','$(#SrcPath);$ProjectSrcPath('+Project.IDAsString+')',
      da_Define);
    FSrcDirIfDef.AddChild(SrcPathDefTempl);

    Changed:=true;
  end else begin
    NewValue:='#ProjectSrcMark'+Project.IDAsWord;
    if FSrcDirIfDef.Value<>NewValue then begin
      FSrcDirIfDef.Value:='#ProjectSrcMark'+Project.IDAsWord;
      Changed:=true;
    end;
  end;
  if Changed then
    CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjectDefineTemplates.UpdateDefinesForOutputDirectory;
begin
  //DebugLn('TProjectDefineTemplates.UpdateDefinesForOutputDirectory ',Project.IDAsString);
  if (not Project.NeedsDefineTemplates) or (not Active) then exit;
  if FMain=nil then UpdateMain;

  if FOutputDir=nil then begin
    //DebugLn('TProjectDefineTemplates.UpdateDefinesForOutputDirectory ',Project.IDAsString,' creating FOutputDir');
    FOutputDir:=TDefineTemplate.Create(ProjectOutputDirDefTemplName,
      'Output directoy of project', '', Project.GetOutputDirectory, da_Directory
        );
    FOutputDir.SetDefineOwner(Project,false);
    FOutputDir.SetFlags([dtfAutoGenerated],[],false);
    FMain.AddChild(FOutputDir);
  end else begin
    FOutputDir.Value:=Project.GetOutputDirectory;
  end;

  if (FOutPutSrcPath=nil)
  or (fLastOutputDirSrcPathIDAsString<>Project.IDAsString) then begin
    fLastOutputDirSrcPathIDAsString:=Project.IDAsString;
    FOutputSrcPath:=TDefineTemplate.Create('CompiledSrcPath',
      lisPkgDefsCompiledSrcPathAddition, CompiledSrcPathMacroName,
      '$ProjectSrcPath('+fLastOutputDirSrcPathIDAsString+');'
        +'$('+CompiledSrcPathMacroName+')',
      da_Define);
    FOutputSrcPath.SetDefineOwner(Project,false);
    FOutputSrcPath.SetFlags([dtfAutoGenerated],[],false);
    CodeToolBoss.DefineTree.ReplaceChild(FOutputDir,FOutputSrcPath,
      FOutputSrcPath.Name);
  end;
end;

procedure TProjectDefineTemplates.UpdateSourceDirectories;
var
  NewSourceDirs: TStringList;
  i: Integer;
  SrcDirDefTempl: TDefineTemplate;
  IDHasChanged: Boolean;
  SrcDirMarkDefTempl: TDefineTemplate;
  CurUnitPath: String;
begin
  //DebugLn('TProjectDefineTemplates.UpdateDefinesForSourceDirectories ',Project.IDAsString,' Active=',dbgs(Active),' TimeStamp=',dbgs(fLastSourceDirStamp),' Project.TimeStamp=',dbgs(Project.SourceDirectories.TimeStamp));
  if (not Project.NeedsDefineTemplates) or (not Active) then exit;

  // quick check if something has changed
  IDHasChanged:=fLastSourceDirsIDAsString<>Project.IDAsString;
  CurUnitPath:=Project.CompilerOptions.ParsedOpts.GetParsedValue(pcosUnitPath);
  CurUnitPath:=CreateAbsoluteSearchPath(CurUnitPath,
                                        Project.CompilerOptions.BaseDirectory);

  //DebugLn('TProjectDefineTemplates.UpdateDefinesForSourceDirectories A');
  if (fLastSourceDirectories<>nil)
  and (fLastSourceDirStamp=Project.SourceDirectories.TimeStamp)
  and (not IDHasChanged)
  and (CurUnitPath=fLastUnitPath) then
    exit;
  fLastSourceDirStamp:=Project.SourceDirectories.TimeStamp;
  fLastSourceDirsIDAsString:=Project.IDAsString;
  fLastUnitPath:=CurUnitPath;

  NewSourceDirs:=Project.SourceDirectories.CreateFileList;
  //DebugLn('TProjectDefineTemplates.UpdateDefinesForSourceDirectories B "',NewSourceDirs.Text,'"');
  try
    MergeSearchPaths(NewSourceDirs,CurUnitPath);
    
    // real check if something has changed
    if (fLastSourceDirectories<>nil)
    and (NewSourceDirs.Count=fLastSourceDirectories.Count)
    and (not IDHasChanged) then begin
      i:=NewSourceDirs.Count-1;
      while (i>=0)
      and (CompareFilenames(NewSourceDirs[i],fLastSourceDirectories[i])=0) do
        dec(i);
      if i<0 then exit;
    end;

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
    fLastSourceDirectories.Assign(NewSourceDirs);
    if (FSrcDirIfDef=nil) and (fLastSourceDirectories.Count>0) then
      UpdateSrcDirIfDef;
    for i:=0 to fLastSourceDirectories.Count-1 do begin
      // create directory template
      SrcDirDefTempl:=TDefineTemplate.Create('Source Directory '+IntToStr(i+1),
        fLastSourceDirectories[i],'',fLastSourceDirectories[i],da_Directory);
      fLastSourceDirectories.Objects[i]:=SrcDirDefTempl;
      // add project source directory marker
      SrcDirMarkDefTempl:=TDefineTemplate.Create('ProjectSrcDirMark',
        lisProjProjectSourceDirectoryMark, '#ProjectSrcMark'+Project.IDAsWord,
          '',
        da_Define);
      SrcDirDefTempl.AddChild(SrcDirMarkDefTempl);

      SrcDirDefTempl.SetDefineOwner(Project,false);
      SrcDirDefTempl.SetFlags([dtfAutoGenerated],[],false);
      // add directory
      FSrcDirectories.AddChild(SrcDirDefTempl);
    end;
    CodeToolBoss.DefineTree.ClearCache;

  finally
    NewSourceDirs.Free;
  end;
end;

procedure TProjectDefineTemplates.UpdateDefinesForCustomDefines;
var
  OptionsDefTempl: TDefineTemplate;
  NewCustomOptions: String;
  Changed: Boolean;
begin
  if (not Project.NeedsDefineTemplates) or (not Active) then exit;

  // check if something has changed
  NewCustomOptions:=Project.CompilerOptions.GetOptionsForCTDefines;
  if (FLastCustomOptions=NewCustomOptions) then exit;
  Changed:=false;

  FLastCustomOptions:=NewCustomOptions;
  OptionsDefTempl:=CodeToolBoss.DefinePool.CreateFPCCommandLineDefines(
                        'Custom Options',FLastCustomOptions,false,Project);
  if OptionsDefTempl=nil then begin
    // no custom options -> delete old template
    if FSrcDirIfDef<>nil then begin
      if FSrcDirIfDef.DeleteChild('Custom Options') then
        Changed:=true;
    end;
  end else begin
    UpdateSrcDirIfDef;
    FSrcDirIfDef.ReplaceChild(OptionsDefTempl);
    Changed:=true;
  end;
  if Changed then
    CodeToolBoss.DefineTree.ClearCache;
end;

constructor TProjectDefineTemplates.Create(OwnerProject: TProject);
begin
  inherited Create;
  FOwnerProject:=OwnerProject;
end;

destructor TProjectDefineTemplates.Destroy;
begin
  Clear;
  fLastSourceDirectories.Free;
  inherited Destroy;
end;

procedure TProjectDefineTemplates.Clear;
begin
  if FMain<>nil then begin
    if (CodeToolBoss<>nil) then
      CodeToolBoss.DefineTree.RemoveDefineTemplate(FMain);
    FMain:=nil;
    FSrcDirIfDef:=nil;
    FSrcDirectories:=nil;
    FOutPutSrcPath:=nil;
    FOutputDir:=nil;
    FFlags:=FFlags+[ptfFlagsChanged];
    fLastOutputDirSrcPathIDAsString:='';
    FLastCustomOptions:='';
    if fLastSourceDirectories<>nil then
      fLastSourceDirectories.Clear;
    fLastSourceDirsIDAsString:='';
    fLastUnitPath:='';
  end;
end;

procedure TProjectDefineTemplates.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjectDefineTemplates.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TProjectDefineTemplates.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if ptfFlagsChanged in FFlags then CustomDefinesChanged;
    if ptfSourceDirsChanged in FFlags then SourceDirectoriesChanged;
    if ptfOutputDirChanged in FFlags then OutputDirectoryChanged;
    if ptfCustomDefinesChanged in FFlags then CustomDefinesChanged;
  end;
end;

procedure TProjectDefineTemplates.AllChanged;
begin
  CustomDefinesChanged;
  SourceDirectoriesChanged;
  UpdateGlobalValues;
  UpdateSrcDirIfDef;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjectDefineTemplates.ProjectIDChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfIDChanged);
    exit;
  end;
  Exclude(FFlags,ptfIDChanged);
  UpdateMain;
  UpdateDefinesForOutputDirectory;
  UpdateSourceDirectories;
  UpdateDefinesForCustomDefines;
end;

procedure TProjectDefineTemplates.SourceDirectoriesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfSourceDirsChanged);
    exit;
  end;
  Exclude(FFlags,ptfSourceDirsChanged);
  UpdateSourceDirectories;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjectDefineTemplates.OutputDirectoryChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfOutputDirChanged);
    exit;
  end;
  Exclude(FFlags,ptfOutputDirChanged);
  UpdateDefinesForOutputDirectory;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjectDefineTemplates.CustomDefinesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfCustomDefinesChanged);
    exit;
  end;
  Exclude(FFlags,ptfCustomDefinesChanged);
  UpdateDefinesForCustomDefines; // maybe custom defines changed
end;

procedure TProjectDefineTemplates.UpdateGlobalValues;
var
  NewProjectDir: String;
  Changed: Boolean;
begin
  Changed:=false;
  // the LCLWidgetType, TargetCPU and TargetOS is set by the TBuildManager
  if Owner.IsVirtual then
    NewProjectDir:=VirtualDirectory
  else
    NewProjectDir:=Owner.ProjectDirectory;
  if CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',NewProjectDir)
  then
    Changed:=true;
  if Changed then
    IncreaseCompilerParseStamp;
end;

{ TProjectProgramDescriptor }

constructor TProjectProgramDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameProgram;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement];
end;

function TProjectProgramDescriptor.GetLocalizedName: string;
begin
  Result:=lisProgram;
end;

function TProjectProgramDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(lisProgramAFreepascalProgramTheProgramFileIsAutomatic, [#13]);
end;

function TProjectProgramDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  le: String;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  le:=LineEnding;
  NewSource:='program Project1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+le
    +'  cthreads,'+le
    +'  {$ENDIF}{$ENDIF}'+le
    +'  Classes'+le
    +'  { you can add units after this };'+le
    +le
    +'begin'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);
end;

function TProjectProgramDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TProjectApplicationDescriptor }

constructor TProjectApplicationDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameApplication;
end;

function TProjectApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=dlgPOApplication;
end;

function TProjectApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(lisApplicationAGraphicalLclFreepascalProgramTheProgra, [#13]);
end;

function TProjectApplicationDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  
  // create program source
  le:=LineEnding;
  NewSource:='program Project1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  {$IFDEF UNIX}{$IFDEF UseCThreads}'+le
    +'  cthreads,'+le
    +'  {$ENDIF}{$ENDIF}'+le
    +'  Interfaces, // this includes the LCL widgetset'+le
    +'  Forms'+le
    +'  { you can add units after this };'+le
    +le
    +'begin'+le
    +'  Application.Initialize;'+le
    +'  Application.Run;'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);
  
  // add lcl pp/pas dirs to source search path
  AProject.AddPackageDependency('LCL');
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
end;

function TProjectApplicationDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorForm,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

{ TProjectManualProgramDescriptor }

constructor TProjectManualProgramDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameCustomProgram;
  Flags:=Flags-[pfMainUnitHasUsesSectionForAllUnits,
                pfMainUnitHasCreateFormStatements,
                pfMainUnitHasTitleStatement];
  FAddMainSource:=true;
end;

function TProjectManualProgramDescriptor.GetLocalizedName: string;
begin
  Result:=lisCustomProgram;
end;

function TProjectManualProgramDescriptor.GetLocalizedDescription: string;
begin
  Result:=Format(lisCustomProgramAFreepascalProgram, [#13]);
end;

function TProjectManualProgramDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  le: string;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);
  
  if AddMainSource then begin
    MainFile:=AProject.CreateProjectFile('project1.pas');
    MainFile.IsPartOfProject:=true;
    AProject.AddFile(MainFile,false);
    AProject.MainFileID:=0;

    // create program source
    le:=LineEnding;
    NewSource:='program Project1;'+le
      +le
      +'{$mode objfpc}{$H+}'+le
      +le
      +'uses'+le
      +'  Classes, SysUtils'+le
      +'  { you can add units after this };'+le
      +le
      +'begin'+le
      +'end.'+le
      +le;
    AProject.MainFile.SetSourceText(NewSource);
  end;
end;

function TProjectManualProgramDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  if AProject.MainFile<>nil then
    Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,
                                        [ofProjectLoading,ofRegularFile]);
end;

{ TProjectEmptyProgramDescriptor }

constructor TProjectEmptyProgramDescriptor.Create;
begin
  inherited Create;
  FAddMainSource:=false;
end;

{ TProjectLibraryDescriptor }

constructor TProjectLibraryDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameLibrary;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement];
end;

function TProjectLibraryDescriptor.GetLocalizedName: string;
begin
  Result:=lisPckOptsLibrary;
end;

function TProjectLibraryDescriptor.GetLocalizedDescription: string;
begin
  Result:= Format(lisLibraryAFreepascalLibraryDllUnderWindowsSoUnderLin, [#13]);
end;

function TProjectLibraryDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  le: String;
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.LazCompilerOptions.ExecutableType:=cetLibrary;

  // create program source
  le:=LineEnding;
  NewSource:='library Project1;'+le
    +le
    +'{$mode objfpc}{$H+}'+le
    +le
    +'uses'+le
    +'  Classes'+le
    +'  { you can add units after this };'+le
    +le
    +'begin'+le
    +'end.'+le
    +le;
  AProject.MainFile.SetSourceText(NewSource);
end;

function TProjectLibraryDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TUnitComponentDependency }

procedure TUnitComponentDependency.SetRequiresUnit(const AValue: TUnitInfo);
begin
  if FRequiresUnit=AValue then exit;
  if (AValue<>nil) and (FUsedByUnit=AValue) then
    raise Exception.Create('TUnitComponentDependency.SetRequiresUnit inconsistency');
  if FRequiresUnit<>nil then
    RemoveFromList(FRequiresUnit.FFirstUsedByComponent,ucdlUsedBy);
  FRequiresUnit:=AValue;
  if FRequiresUnit<>nil then
    AddToList(FRequiresUnit.FFirstUsedByComponent,ucdlUsedBy);
end;

procedure TUnitComponentDependency.SetTypes(
  const AValue: TUnitCompDependencyTypes);
begin
  if AValue=FTypes then exit;
  FTypes:=AValue;
  if (not (ucdtOldProperty in FTypes)) and (FCompProps<>nil) then
    ClearComponentProperties;
end;

function TUnitComponentDependency.GetCompPropCount: integer;
begin
  if FCompProps=nil then
    Result:=0
  else
    Result:=FCompProps.Count;
end;

function TUnitComponentDependency.GetCompProps(Index: integer
  ): TUCDComponentProperty;
begin
  Result:=TUCDComponentProperty(FCompProps[Index]);
end;

procedure TUnitComponentDependency.SetUsedByUnit(const AValue: TUnitInfo);
begin
  if FUsedByUnit=AValue then exit;
  if (AValue<>nil) and (FRequiresUnit=AValue) then
    raise Exception.Create('TUnitComponentDependency.SetUsedByUnit inconsistency');
  if FUsedByUnit<>nil then
    RemoveFromList(FUsedByUnit.FFirstRequiredComponent,ucdlRequires);
  FUsedByUnit:=AValue;
  if FUsedByUnit<>nil then
    AddToList(FUsedByUnit.FFirstRequiredComponent,ucdlRequires);
end;

constructor TUnitComponentDependency.Create;
begin

end;

destructor TUnitComponentDependency.Destroy;
begin
  RequiresUnit:=nil;
  UsedByUnit:=nil;
  ClearComponentProperties;
  inherited Destroy;
end;

procedure TUnitComponentDependency.ClearComponentProperties;
var
  i: Integer;
begin
  if FCompProps=nil then exit;
  for i:=0 to FCompProps.Count-1 do TObject(FCompProps[i]).Free;
  FreeAndNil(FCompProps);
end;

function TUnitComponentDependency.NextUsedByDependency
  : TUnitComponentDependency;
begin
  Result:=NextDependency[ucdlUsedBy];
end;

function TUnitComponentDependency.PrevUsedByDependency
  : TUnitComponentDependency;
begin
  Result:=PrevDependency[ucdlUsedBy];
end;

function TUnitComponentDependency.NextRequiresDependency
  : TUnitComponentDependency;
begin
  Result:=NextDependency[ucdlRequires];
end;

function TUnitComponentDependency.PrevRequiresDependency
  : TUnitComponentDependency;
begin
  Result:=PrevDependency[ucdlRequires];
end;

procedure TUnitComponentDependency.AddToList(
  var FirstDependency: TUnitComponentDependency;
  ListType: TUnitCompDependencyList);
begin
  NextDependency[ListType]:=FirstDependency;
  FirstDependency:=Self;
  PrevDependency[ListType]:=nil;
  if NextDependency[ListType]<>nil then
    NextDependency[ListType].PrevDependency[ListType]:=Self;
end;

procedure TUnitComponentDependency.RemoveFromList(
  var FirstDependency: TUnitComponentDependency;
  ListType: TUnitCompDependencyList);
begin
  if FirstDependency=Self then FirstDependency:=NextDependency[ListType];
  if NextDependency[ListType]<>nil then
    NextDependency[ListType].PrevDependency[ListType]:=PrevDependency[ListType];
  if PrevDependency[ListType]<>nil then
    PrevDependency[ListType].NextDependency[ListType]:=NextDependency[ListType];
  NextDependency[ListType]:=nil;
  PrevDependency[ListType]:=nil;
end;

function TUnitComponentDependency.FindUsedByPropPath(
  const UsedByPropPath: string): TUCDComponentProperty;
var
  i: Integer;
begin
  if FCompProps=nil then exit(nil);
  for i:=FCompProps.Count-1 downto 0 do begin
    Result:=CompProps[i];
    if SysUtils.CompareText(Result.UsedByPropPath,UsedByPropPath)=0 then exit;
  end;
  Result:=nil;
end;

function TUnitComponentDependency.SetUsedByPropPath(const UsedByPropPath,
  RequiresPropPath: string): TUCDComponentProperty;
begin
  DebugLn(['TUnitComponentDependency.SetUsedByPropPath ',UsedByPropPath,'=',RequiresPropPath]);
  if (not (ucdtOldProperty in FTypes)) then
    raise Exception.Create('TUnitComponentDependency.SetUsedByPropPath inconsistency');
  Result:=FindUsedByPropPath(UsedByPropPath);
  if Result=nil then begin
    if FCompProps=nil then
      FCompProps:=TFPList.Create;
    Result:=TUCDComponentProperty.Create(UsedByPropPath,RequiresPropPath);
    FCompProps.Add(Result);
  end else begin
    Result.UsedByPropPath:=UsedByPropPath;// update case
    Result.RequiresPropPath:=RequiresPropPath;
  end;
end;

function TUnitComponentDependency.CreatePropPath(AComponent: TComponent;
  const PropName: string): string;
begin
  Result:=PropName;
  while AComponent<>nil do begin
    if Result<>'' then
      Result:='.'+Result;
    Result:=AComponent.Name+Result;
    AComponent:=AComponent.Owner;
  end;
end;

{ TProjectConsoleApplicationDescriptor }

constructor TProjectConsoleApplicationDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameConsoleApplication;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement];
end;

function TProjectConsoleApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=lisConsoleApplication;
end;

function TProjectConsoleApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=GetLocalizedName+#13
    +lisFreepascalProgramUsingTCustomApplicationToEasilyCh;
end;

function TProjectConsoleApplicationDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  NewSource: TStringList;
  MainFile: TLazProjectFile;
  C, T : String;
  CC,CD,CU,CS, CO : Boolean;
  
begin
  Result:=inherited InitProject(AProject);
  If Result<>mrOk then
    Exit;
  With TCustomApplicationOptionsForm.Create(Application) do
    try
      Result:=ShowModal;
      If Result<>mrOk then
        Exit;
      C:=Trim(AppClassName);
      T:=StringReplace(Title,'''','''''',[rfReplaceAll]);
      CC:=CodeConstructor;
      CD:=CodeDestructor;
      CU:=CodeUsage;
      CS:=CodeStopOnError;
      CO:=CodeCheckOptions;
    finally
      Free;
    end;
  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  NewSource:=TStringList.Create;
  NewSource.Add('program Project1;');
  NewSource.Add('');
  NewSource.Add('{$mode objfpc}{$H+}');
  NewSource.Add('');
  NewSource.Add('uses');
  NewSource.Add('  {$IFDEF UNIX}{$IFDEF UseCThreads}');
  NewSource.Add('  cthreads,');
  NewSource.Add('  {$ENDIF}{$ENDIF}');
  NewSource.Add('  Classes, SysUtils, CustApp');
  NewSource.Add('  { you can add units after this };');
  NewSource.Add('');
  NewSource.Add('type');
  NewSource.Add('');
  NewSource.Add('  { '+C+' }');
  NewSource.Add('');
  NewSource.Add('  '+C+' = class(TCustomApplication)');
  NewSource.Add('  protected');
  NewSource.Add('    procedure DoRun; override;');
  NewSource.Add('  public');
  If CC or CS then
    NewSource.Add('    constructor Create(TheOwner: TComponent); override;');
  if CD then
    NewSource.Add('    destructor Destroy; override;');
  if CU then
    NewSource.Add('    procedure WriteHelp; virtual;');
  NewSource.Add('  end;');
  NewSource.Add('');
  NewSource.Add('{ '+C+' }');
  NewSource.Add('');
  NewSource.Add('procedure '+C+'.DoRun;');
  NewSource.Add('var');
  NewSource.Add('  ErrorMsg: String;');
  NewSource.Add('begin');
  if CO then
    begin
    NewSource.Add('  // quick check parameters');
    NewSource.Add('  ErrorMsg:=CheckOptions(''h'',''help'');');
    NewSource.Add('  if ErrorMsg<>'''' then begin');
    NewSource.Add('    ShowException(Exception.Create(ErrorMsg));');
    NewSource.Add('    Terminate;');
    NewSource.Add('    Exit;');
    NewSource.Add('  end;');
    NewSource.Add('');
    end;
  If CU then
    begin
    NewSource.Add('  // parse parameters');
    NewSource.Add('  if HasOption(''h'',''help'') then begin');
    NewSource.Add('    WriteHelp;');
    NewSource.Add('    Terminate;');
    NewSource.Add('    Exit;');
    NewSource.Add('  end;');
    end;
  NewSource.Add('');
  NewSource.Add('  { add your program here }');
  NewSource.Add('');
  NewSource.Add('  // stop program loop');
  NewSource.Add('  Terminate;');
  NewSource.Add('end;');
  NewSource.Add('');
  If CC or CS then
    begin
    NewSource.Add('constructor '+C+'.Create(TheOwner: TComponent);');
    NewSource.Add('begin');
    NewSource.Add('  inherited Create(TheOwner);');
    If CS then
    NewSource.Add('  StopOnException:=True;');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  If CD then
    begin
    NewSource.Add('destructor '+C+'.Destroy;');
    NewSource.Add('begin');
    NewSource.Add('  inherited Destroy;');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  If CU then
    begin
    NewSource.Add('procedure '+C+'.WriteHelp;');
    NewSource.Add('begin');
    NewSource.Add('  { add your help code here }');
    NewSource.Add('  writeln(''Usage: '',ExeName,'' -h'');');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  NewSource.Add('var');
  NewSource.Add('  Application: '+C+';');
  NewSource.Add('begin');
  NewSource.Add('  Application:='+C+'.Create(nil);');
  If (T<>'') then
    NewSource.Add('  Application.Title:='''+T+''';');
  NewSource.Add('  Application.Run;');
  NewSource.Add('  Application.Free;');
  NewSource.Add('end.');
  NewSource.Add('');
  AProject.MainFile.SetSourceText(NewSource.Text);
  NewSource.Free;
end;

function TProjectConsoleApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TUCDComponentProperty }

constructor TUCDComponentProperty.Create(const SrcPath, DestPath: string);
begin
  UsedByPropPath:=SrcPath;
  RequiresPropPath:=DestPath;
end;

end.

