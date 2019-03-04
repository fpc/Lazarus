{
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
    This unit is a support unit for the code tools. It manages compilation
    information, which is not stored in the source, like Makefile information
    and compiler command line options. This information is needed to
    successfully find the right units, include files, predefined variables,
    etc..
    
    The information is stored in a TDefineTree, which contains nodes of type
    TDefineTemplate. Each TDefineTemplate is a tree of defines, undefines,
    definerecurses, ifdefs, ifndefs, elses, elseifs, directories ... .
    
    Simply give a TDefineTree a directory and it will return all predefined
    variables for that directory. These values can be used to parse a unit in
    the directory.
    
    TDefineTree can be saved to and loaded from a XML file.
    
    The TDefinePool contains a list of TDefineTemplate trees, and can generate
    some default templates for Lazarus and FPC sources.
}
unit DefineTemplates;

{$mode objfpc}{$H+}

{ $Define VerboseDefineCache}
{ $Define VerboseFPCSrcScan}

{ $Define ShowTriedFiles}
{ $Define ShowTriedUnits}

interface

uses
  // RTL + FCL
  Classes, SysUtils, contnrs, process, Laz_AVL_Tree,
  // CodeTools
  CodeToolsStrConsts, ExprEval, DirectoryCacher, BasicCodeTools,
  CodeToolsStructs, KeywordFuncLists, LinkScanner, FileProcs,
  // LazUtils
  LazStringUtils, LazFileUtils, LazFileCache,
  LazUTF8, LazUTF8Classes, UTF8Process, LazDbgLog, AvgLvlTree, Laz2_XMLCfg;

const
  ExternalMacroStart = ExprEval.ExternalMacroStart;

  // Standard Template Names (do not translate them)
  StdDefTemplFPC            = 'Free Pascal Compiler';
  StdDefTemplFPCSrc         = 'Free Pascal sources';
  StdDefTemplLazarusSources = 'Lazarus sources';
  StdDefTemplLazarusSrcDir  = 'Lazarus source directory';
  StdDefTemplLazarusBuildOpts = 'Lazarus build options';
  StdDefTemplLCLProject     = 'LCL project';

  // Standard macros
  DefinePathMacroName      = ExternalMacroStart+'DefinePath'; // the current directory
  UnitPathMacroName        = ExternalMacroStart+'UnitPath'; // unit search path separated by semicolon (same as given to FPC)
  IncludePathMacroName     = ExternalMacroStart+'IncPath'; // include file search path separated by semicolon (same as given to FPC)
  SrcPathMacroName         = ExternalMacroStart+'SrcPath'; // unit source search path separated by semicolon (not given to FPC)
  PPUSrcPathMacroName      = ExternalMacroStart+'PPUSrcPath';
  DCUSrcPathMacroName      = ExternalMacroStart+'DCUSrcPath';
  CompiledSrcPathMacroName = ExternalMacroStart+'CompiledSrcPath';
  UnitLinksMacroName       = ExternalMacroStart+'UnitLinks';
  UnitSetMacroName         = ExternalMacroStart+'UnitSet';
  FPCUnitPathMacroName     = ExternalMacroStart+'FPCUnitPath';
  TargetOSMacroName        = ExternalMacroStart+'TargetOS';
  TargetCPUMacroName       = ExternalMacroStart+'TargetCPU';
  NamespacesMacroName      = ExternalMacroStart+'Namespaces';

  DefinePathMacro          = '$('+DefinePathMacroName+')'; // the path of the define template
  UnitPathMacro            = '$('+UnitPathMacroName+')';
  IncludePathMacro         = '$('+IncludePathMacroName+')';
  SrcPathMacro             = '$('+SrcPathMacroName+')';
  PPUSrcPathMacro          = '$('+PPUSrcPathMacroName+')';
  DCUSrcPathMacro          = '$('+DCUSrcPathMacroName+')';
  CompiledSrcPathMacro     = '$('+CompiledSrcPathMacroName+')';
  UnitLinksMacro           = '$('+UnitLinksMacroName+')';
  UnitSetMacro             = '$('+UnitSetMacroName+')';
  FPCUnitPathMacro         = '$('+FPCUnitPathMacroName+')';
  TargetOSMacro            = '$('+TargetOSMacroName+')';
  TargetCPUMacro           = '$('+TargetCPUMacroName+')';
  NamespacesMacro          = '$('+NamespacesMacroName+')';

  MacOSMinSDKVersionMacro = 'MAC_OS_X_VERSION_MIN_REQUIRED';

  // virtual directories
  VirtualDirectory='VIRTUALDIRECTORY';
  VirtualTempDir='TEMPORARYDIRECTORY';
  
  // FPC operating systems and processor types
  FPCOperatingSystemNames: array[1..35] of shortstring =(
     'linux',
     'win32','win64','wince',
     'darwin','macos',
     'freebsd','netbsd','openbsd',
     'aix',
     'amiga',
     'android',
     'aros',
     'atari',
     'beos',
     'embedded',
     'emx',
     'gba',
     'go32v2',
     'haiku',
     'iphonesim',
     'java',
     'msdos',
     'morphos',
     'nds',
     'netware',
     'netwlibc',
     'os2',
     'palmos',
     'qnx',
     'solaris',
     'symbian',
     'watcom',
     'wdosx',
     'wii'
    );
  FPCOperatingSystemCaptions: array[1..35] of shortstring =(
     'AIX',
     'Amiga',
     'Android',
     'AROS',
     'Atari',
     'Beos',
     'Darwin',
     'Embedded',
     'emx',
     'FreeBSD',
     'gba',
     'go32v2',
     'Haiku',
     'iphonesim',
     'Java',
     'Linux',
     'MacOS',
     'MorphOS',
     'MSDOS',
     'nds',
     'NetBSD',
     'Netware',
     'NetwLibC',
     'OpenBSD',
     'OS2',
     'PalmOS',
     'qnx',
     'Solaris',
     'Symbian',
     'Watcom',
     'wdosx',
     'Win32',
     'Win64',
     'WinCE',
     'Wii'
    );

  FPCOperatingSystemAlternativeNames: array[1..2] of shortstring =(
      'unix', 'win' // see GetDefaultSrcOSForTargetOS
    );
  FPCOperatingSystemAlternative2Names: array[1..2] of shortstring =(
      'bsd', 'linux' // see GetDefaultSrcOS2ForTargetOS
    );
  FPCProcessorNames: array[1..13] of shortstring =(
      'aarch64',
      'arm',
      'avr',
      'i386',
      'i8086',
      'jvm',
      'm68k',
      'mips',
      'mipsel',
      'powerpc',
      'powerpc64',
      'sparc',
      'x86_64'
    );
  FPCSyntaxModes: array[1..6] of shortstring = (
    'FPC', 'ObjFPC', 'Delphi', 'TP', 'MacPas', 'ISO'
    );

  Pas2jsPlatformNames: array[1..2] of shortstring = (
    'Browser',
    'NodeJS'
    );
  Pas2jsProcessorNames: array[1..2] of shortstring = (
    'ECMAScript5',
    'ECMAScript6'
    );

  Lazarus_CPU_OS_Widget_Combinations: array[1..91] of shortstring = (
    'i386-linux-gtk',
    'i386-linux-gtk2',
    'i386-linux-qt',
    'i386-linux-qt5',
    'i386-linux-fpgui',
    'i386-linux-nogui',
    'i386-freebsd-gtk',
    'i386-freebsd-gtk2',
    'i386-freebsd-qt',
    'i386-freebsd-qt5',
    'i386-freebsd-nogui',
    'i386-openbsd-gtk',
    'i386-openbsd-gtk2',
    'i386-openbsd-qt',
    'i386-openbsd-qt5',
    'i386-openbsd-nogui',
    'i386-netbsd-gtk',
    'i386-netbsd-gtk2',
    'i386-netbsd-qt',
    'i386-netbsd-qt5',
    'i386-netbsd-nogui',
    'i386-win32-win32',
    'i386-win32-gtk2',
    'i386-win32-qt',
    'i386-win32-qt5',
    'i386-win32-fpgui',
    'i386-win32-nogui',
    'i386-wince-wince',
    'i386-wince-fpgui',
    'i386-wince-nogui',
    'i386-darwin-gtk',
    'i386-darwin-gtk2',
    'i386-darwin-carbon',
    'i386-darwin-qt',
    'i386-darwin-qt5',
    'i386-darwin-fpgui',
    'i386-darwin-nogui',
    'i386-haiku-qt',
    'i386-haiku-qt5',
    'i386-haiku-nogui',
    'i386-aros-mui',
    'i386-aros-nogui',
    'powerpc-darwin-gtk',
    'powerpc-darwin-gtk2',
    'powerpc-darwin-carbon',
    'powerpc-linux-gtk',
    'powerpc-linux-gtk2',
    'powerpc-linux-nogui',
    'powerpc-morphos-mui',
    'powerpc-morphos-nogui',
    'powerpc64-darwin-gtk',
    'powerpc64-darwin-gtk2',
    'powerpc64-darwin-cocoa',
    'powerpc64-darwin-nogui',
    'powerpc64-linux-gtk',
    'powerpc64-linux-gtk2',
    'powerpc64-linux-nogui',
    'powerpc64-aix-gtk',
    'powerpc64-aix-gtk2',
    'powerpc64-aix-nogui',
    'sparc-linux-gtk',
    'sparc-linux-gtk2',
    'sparc-linux-nogui',
    'arm-wince-wince',
    'arm-wince-fpgui',
    'arm-wince-nogui',
    'arm-linux-gtk',
    'arm-linux-gtk2',
    'arm-linux-qt',
    'arm-linux-qt5',
    'arm-linux-android',
    'arm-linux-nogui',
    'arm-darwin-carbon',
    'arm-darwin-nogui',
    'x86_64-freebsd-gtk',
    'x86_64-freebsd-gtk2',
    'x86_64-freebsd-qt',
    'x86_64-freebsd-qt5',
    'x86_64-freebsd-fpgui',
    'x86_64-freebsd-nogui',
    'x86_64-linux-gtk',
    'x86_64-linux-gtk2',
    'x86_64-linux-qt',
    'x86_64-linux-qt5',
    'x86_64-linux-fpgui',
    'x86_64-linux-nogui',
    'x86_64-win64-win32',
    'x86_64-win64-fpgui',
    'x86_64-win64-nogui',
    'm68k-amiga-mui',
    'm68k-amiga-nogui'
    );

type
  //---------------------------------------------------------------------------
  // TDefineTemplate stores a define action, the variablename and the value
  TDefineAction = (
    da_None,
    da_Block,
    da_Define,
    da_DefineRecurse,
    da_Undefine,
    da_UndefineRecurse,
    da_UndefineAll,
    da_If,
    da_IfDef,
    da_IfNDef,
    da_ElseIf,
    da_Else,
    da_Directory
  );

const
  DefineActionBlocks = [da_Block, da_Directory, da_If, da_IfDef, da_IfNDef,
                        da_ElseIf, da_Else];
  DefineActionDefines = [da_Define,da_DefineRecurse,da_Undefine,
                         da_UndefineRecurse,da_UndefineAll];
  DefineActionNames: array[TDefineAction] of string = (
      'None', 'Block', 'Define', 'DefineRecurse', 'Undefine', 'UndefineRecurse',
      'UndefineAll', 'If', 'IfDef', 'IfNDef', 'ElseIf', 'Else', 'Directory'
    );
var
  DefineActionImages: array[TDefineAction] of integer;
  AutogeneratedImage: Integer;

type
  TDefineTree = class;
  TDefineTemplateFlag = (
    dtfAutoGenerated
    );
  TDefineTemplateFlags = set of TDefineTemplateFlag;
  
  TDefineTemplate = class
  private
    FChildCount: integer;
    FFirstChild: TDefineTemplate;
    FLastChild: TDefineTemplate;
    FMarked: boolean;
    FMergeNameBehind: string;
    FMergeNameInFront: string;
    FNext: TDefineTemplate;
    FParent: TDefineTemplate;
    FPrior: TDefineTemplate;
  public
    Name: string;
    Description: string;
    Variable: string;
    Value: string;
    Action: TDefineAction;
    Flags: TDefineTemplateFlags;
    Owner: TObject;
    class procedure MergeTemplates(ParentDefTempl: TDefineTemplate;
                  var FirstSibling, LastSibling:TDefineTemplate;
                  SourceTemplate: TDefineTemplate; WithSiblings: boolean;
                  const NewNamePrefix: string);
    class procedure MergeXMLConfig(ParentDefTempl: TDefineTemplate;
                  var FirstSibling, LastSibling:TDefineTemplate;
                  XMLConfig: TXMLConfig; const Path, NewNamePrefix: string);
    constructor Create(const AName, ADescription, AVariable, AValue: string;
                       AnAction: TDefineAction);
    constructor Create;
    destructor Destroy; override;
    procedure ConsistencyCheck;
    procedure CalcMemSize(Stats: TCTMemStats);
    function  CreateCopy(OnlyMarked: boolean = false;
                         WithSiblings: boolean = true;
                         WithChilds: boolean = true): TDefineTemplate;
    function  CreateMergeCopy: TDefineTemplate;
    function  FindByName(const AName: string;
                     WithSubChilds, WithNextSiblings: boolean): TDefineTemplate;
    function  FindChildByName(const AName: string): TDefineTemplate;
    function  FindRoot: TDefineTemplate;
    function  FindUniqueName(const Prefix: string): string;
    function  GetFirstSibling: TDefineTemplate;
    function  HasDefines(OnlyMarked, WithSiblings: boolean): boolean;
    function  IsAutoGenerated: boolean;
    function  IsEqual(ADefineTemplate: TDefineTemplate;
                      CheckSubNodes, CheckNextSiblings: boolean): boolean;
    function  Level: integer;
    function  LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                ClearOldSiblings, WithMergeInfo: boolean): boolean;
    function  SelfOrParentContainsFlag(AFlag: TDefineTemplateFlag): boolean;
    procedure AddChild(ADefineTemplate: TDefineTemplate);
    procedure ReplaceChild(ADefineTemplate: TDefineTemplate);
    function DeleteChild(const AName: string): boolean;
    procedure Assign(ADefineTemplate: TDefineTemplate; WithSubNodes,
                     WithNextSiblings, ClearOldSiblings: boolean); virtual;
    procedure AssignValues(ADefineTemplate: TDefineTemplate);
    procedure Clear(WithSiblings: boolean);
    procedure CreateMergeInfo(WithSiblings, OnlyMarked: boolean);
    procedure InheritMarks(WithSiblings, WithChilds, Down, Up: boolean);
    procedure InsertBehind(APrior: TDefineTemplate);
    procedure InsertInFront(ANext: TDefineTemplate);
    procedure MoveToLast(Child: TDefineTemplate);
    procedure LoadValuesFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                      WithMergeInfo: boolean);
    procedure MarkFlags(const MustFlags, NotFlags: TDefineTemplateFlags;
                        WithSiblings, WithChilds: boolean);
    procedure MarkNodes(WithSiblings, WithChilds: boolean);
    procedure MarkOwnedBy(TheOwner: TObject;
                          const MustFlags, NotFlags: TDefineTemplateFlags;
                          WithSiblings, WithChilds: boolean);
    procedure RemoveFlags(TheFlags: TDefineTemplateFlags);
    procedure RemoveLeaves(TheOwner: TObject; const MustFlags,
                           NotFlags: TDefineTemplateFlags;
                           WithSiblings: boolean;
                           var FirstDefTemplate: TDefineTemplate);
    procedure RemoveMarked(WithSiblings: boolean;
                           var FirstDefTemplate: TDefineTemplate);
    procedure RemoveOwner(TheOwner: TObject; WithSiblings: boolean);
    procedure ReverseMarks(WithSiblings, WithChilds: boolean);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                              WithSiblings, OnlyMarked,
                              WithMergeInfo, UpdateMergeInfo: boolean);
    procedure SaveValuesToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
                                    WithMergeInfo: boolean);
    procedure SetDefineOwner(NewOwner: TObject; WithSiblings: boolean);
    procedure SetFlags(AddFlags, SubFlags: TDefineTemplateFlags;
                       WithSiblings: boolean);
    procedure Unbind;
    procedure UnmarkNodes(WithSiblings, WithChilds: boolean);
    procedure WriteDebugReport(OnlyMarked: boolean);
    function GetNext: TDefineTemplate;
    function GetNextSkipChildren: TDefineTemplate;
  public
    property ChildCount: integer read FChildCount;
    property FirstChild: TDefineTemplate read FFirstChild;
    property LastChild: TDefineTemplate read FLastChild;
    property Marked: boolean read FMarked write FMarked;
    property Next: TDefineTemplate read FNext;
    property Parent: TDefineTemplate read FParent;
    property Prior: TDefineTemplate read FPrior;
    property MergeNameInFront: string read FMergeNameInFront write FMergeNameInFront;
    property MergeNameBehind: string read FMergeNameBehind write FMergeNameBehind;
  end;

  //---------------------------------------------------------------------------
  //

  { TDirectoryDefines }

  TDirectoryDefines = class
  public
    Path: string;
    Values: TExpressionEvaluator;
    constructor Create;
    destructor Destroy; override;
    procedure CalcMemSize(Stats: TCTMemStats);
  end;
  
  TOnGetVirtualDirectoryDefines = procedure(Sender: TDefineTree;
    Defines: TDirectoryDefines) of object;

  //---------------------------------------------------------------------------
  // TDefineTree caches the define values for directories
  TOnReadValue = procedure(Sender: TObject; const VariableName: string;
                          var Value: string; var Handled: boolean) of object;

  TOnGetVirtualDirectoryAlias = procedure(Sender: TObject;
    var RealDir: string) of object;
    
  TReadFunctionData = record
    Param: string;
    Result: string;
  end;
  PReadFunctionData = ^TReadFunctionData;
  
  TDefTreeCalculate = procedure(Tree: TDefineTree; Node: TDefineTemplate;
    ValueParsed: boolean; const ParsedValue: string;
    ExpressionCalculated: boolean; const ExpressionResult: string;
    Execute: boolean) of object;

  TDefineTree = class
  private
    FDirectoryCachePool: TCTDirectoryCachePool;
    FFirstDefineTemplate: TDefineTemplate;
    FCache: TAVLTree; // tree of TDirectoryDefines
    FDefineStrings: TStringTree;
    FChangeStep: integer;
    FErrorDescription: string;
    FErrorTemplate: TDefineTemplate;
    FMacroFunctions: TKeyWordFunctionList;
    FMacroVariables: TKeyWordFunctionList;
    FOnCalculate: TDefTreeCalculate;
    FOnGetVirtualDirectoryAlias: TOnGetVirtualDirectoryAlias;
    FOnGetVirtualDirectoryDefines: TOnGetVirtualDirectoryDefines;
    FOnPrepareTree: TNotifyEvent;
    FOnReadValue: TOnReadValue;
    FVirtualDirCache: TDirectoryDefines;
    function Calculate(DirDef: TDirectoryDefines): boolean;
    procedure IncreaseChangeStep;
    procedure SetDirectoryCachePool(const AValue: TCTDirectoryCachePool);
    procedure RemoveDoubles(Defines: TDirectoryDefines);
  protected
    function FindDirectoryInCache(const Path: string): TDirectoryDefines;
    function GetDirDefinesForDirectory(const Path: string;
                                    WithVirtualDir: boolean): TDirectoryDefines;
    function GetDirDefinesForVirtualDirectory: TDirectoryDefines;
    function MacroFuncExtractFileExt(Data: Pointer): boolean;
    function MacroFuncExtractFilePath(Data: Pointer): boolean;
    function MacroFuncExtractFileName(Data: Pointer): boolean;
    function MacroFuncExtractFileNameOnly(Data: Pointer): boolean;
    procedure DoClearCache;
    procedure DoPrepareTree;
  public
    property RootTemplate: TDefineTemplate
                           read FFirstDefineTemplate write FFirstDefineTemplate;
    property ChangeStep: integer read FChangeStep;
    property ErrorTemplate: TDefineTemplate read FErrorTemplate;
    property ErrorDescription: string read FErrorDescription;
    property OnGetVirtualDirectoryAlias: TOnGetVirtualDirectoryAlias
             read FOnGetVirtualDirectoryAlias write FOnGetVirtualDirectoryAlias;
    property OnGetVirtualDirectoryDefines: TOnGetVirtualDirectoryDefines
         read FOnGetVirtualDirectoryDefines write FOnGetVirtualDirectoryDefines;
    property OnReadValue: TOnReadValue read FOnReadValue write FOnReadValue;
    property OnPrepareTree: TNotifyEvent read FOnPrepareTree write FOnPrepareTree;
    property OnCalculate: TDefTreeCalculate read FOnCalculate write FOnCalculate;
    property MacroFunctions: TKeyWordFunctionList read FMacroFunctions;
    property MacroVariables: TKeyWordFunctionList read FMacroVariables;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConsistencyCheck;
    procedure CalcMemSize(Stats: TCTMemStats);
    function  ExtractNonAutoCreated: TDefineTemplate;
    function  ExtractTemplatesOwnedBy(TheOwner: TObject; const MustFlags,
                               NotFlags: TDefineTemplateFlags): TDefineTemplate;
    function  FindDefineTemplateByName(const AName: string;
                                       OnlyRoots: boolean): TDefineTemplate;
    function  GetCompiledSrcPathForDirectory(const Directory: string): string;
    function  GetDCUSrcPathForDirectory(const Directory: string): string;
    function  GetDefinesForDirectory(const Path: string;
                                 WithVirtualDir: boolean): TExpressionEvaluator;
    function  GetDefinesForVirtualDirectory: TExpressionEvaluator;
    function  GetIncludePathForDirectory(const Directory: string): string;
    function  GetLastRootTemplate: TDefineTemplate;
    function  GetPPUSrcPathForDirectory(const Directory: string): string;
    function  GetSrcPathForDirectory(const Directory: string): string;
    function  GetUnitPathForDirectory(const Directory: string): string;
    function  IsEqual(SrcDefineTree: TDefineTree): boolean;
    procedure Add(ADefineTemplate: TDefineTemplate);
    procedure AddChild(ParentTemplate, NewDefineTemplate: TDefineTemplate);
    procedure AddFirst(ADefineTemplate: TDefineTemplate);
    procedure MoveToLast(ADefineTemplate: TDefineTemplate);
    procedure Assign(SrcDefineTree: TDefineTree);
    procedure AssignNonAutoCreated(SrcDefineTree: TDefineTree);
    procedure Clear;
    procedure ClearCache;
    procedure MarkNonAutoCreated;
    procedure MarkTemplatesOwnedBy(TheOwner: TObject;
                               const MustFlags, NotFlags: TDefineTemplateFlags);
    procedure MergeDefineTemplates(SourceTemplate: TDefineTemplate;
                                   const NewNamePrefix: string);
    procedure MergeTemplates(SourceTemplate: TDefineTemplate;
                             const NewNamePrefix: string);
    procedure ReadValue(const DirDef: TDirectoryDefines;
                   const PreValue, CurDefinePath: string; out NewValue: string);
    procedure RemoveDefineTemplate(ADefTempl: TDefineTemplate);
    procedure RemoveMarked;
    procedure RemoveRootDefineTemplateByName(const AName: string);
    procedure RemoveTemplatesOwnedBy(TheOwner: TObject;
                               const MustFlags, NotFlags: TDefineTemplateFlags);
    procedure ReplaceChild(ParentTemplate, NewDefineTemplate: TDefineTemplate;
                           const ChildName: string);
    procedure ReplaceRootSameName(ADefineTemplate: TDefineTemplate);
    procedure ReplaceRootSameName(const Name: string;
                                  ADefineTemplate: TDefineTemplate);
    procedure ReplaceRootSameNameAddFirst(ADefineTemplate: TDefineTemplate);
    procedure WriteDebugReport;
    property DirectoryCachePool: TCTDirectoryCachePool read FDirectoryCachePool write SetDirectoryCachePool;
  end;

  //---------------------------------------------------------------------------

  { TDefinePool }

  TDefinePoolProgress = procedure(Sender: TObject;
    Index, MaxIndex: integer; // MaxIndex=-1 if unknown
    const Msg: string;
    var Abort: boolean) of object;

  TDefinePool = class
  private
    FEnglishErrorMsgFilename: string;
    FItems: TFPList; // list of TDefineTemplate;
    FOnProgress: TDefinePoolProgress;
    function GetItems(Index: integer): TDefineTemplate;
    procedure SetEnglishErrorMsgFilename(const AValue: string);
    function CheckAbort(ProgressID, MaxIndex: integer; const Msg: string
                        ): boolean;
  public
    property Items[Index: integer]: TDefineTemplate read GetItems; default;
    function Count: integer;
    procedure Add(ADefineTemplate: TDefineTemplate);
    procedure Insert(Index: integer; ADefineTemplate: TDefineTemplate);
    procedure Delete(Index: integer);
    procedure Move(SrcIndex, DestIndex: integer);
    property EnglishErrorMsgFilename: string
        read FEnglishErrorMsgFilename write SetEnglishErrorMsgFilename;
    // FPC templates
    function CreateFPCTemplate(const CompilerPath, CompilerOptions,
                               TestPascalFile: string;
                               out UnitSearchPath, TargetOS,
                               aTargetCPU: string;
                               Owner: TObject): TDefineTemplate;
    function GetFPCVerFromFPCTemplate(Template: TDefineTemplate;
                        out FPCVersion, FPCRelease, FPCPatch: integer): boolean;
    function CreateFPCCommandLineDefines(const Name, CmdLine: string;
                                 RecursiveDefines: boolean;
                                 Owner: TObject;
                                 AlwaysCreate: boolean = false;
                                 AddPaths: boolean = false
                                 ): TDefineTemplate;
    // Lazarus templates
    function CreateLazarusSrcTemplate(
                          const LazarusSrcDir, WidgetType, ExtraOptions: string;
                          Owner: TObject): TDefineTemplate;
    function CreateLCLProjectTemplate(const LazarusSrcDir, WidgetType,
                          ProjectDir: string; Owner: TObject): TDefineTemplate;
    // Delphi templates
    function CreateDelphiSrcPath(DelphiVersion: integer;
                                 const PathPrefix: string): string;
    function CreateDelphiCompilerDefinesTemplate(DelphiVersion: integer;
                                               Owner: TObject): TDefineTemplate;
    function CreateDelphiDirectoryTemplate(const DelphiDirectory: string;
                       DelphiVersion: integer; Owner: TObject): TDefineTemplate;
    function CreateDelphiProjectTemplate(const ProjectDir,
                                 DelphiDirectory: string; DelphiVersion: integer;
                                 Owner: TObject): TDefineTemplate;
    // Kylix templates
    function CreateKylixCompilerDefinesTemplate(KylixVersion: integer;
                                               Owner: TObject): TDefineTemplate;
    function CreateKylixSrcPath({%H-}KylixVersion: integer;
                                const PathPrefix: string): string;
    function CreateKylixDirectoryTemplate(const KylixDirectory: string;
                        KylixVersion: integer; Owner: TObject): TDefineTemplate;
    function CreateKylixProjectTemplate(const ProjectDir,
                                 KylixDirectory: string; KylixVersion: integer;
                                 Owner: TObject): TDefineTemplate;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property OnProgress: TDefinePoolProgress read FOnProgress write FOnProgress;
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    procedure CalcMemSize(Stats: TCTMemStats);
  end;

  { TFPCSourceRule }

  TFPCSourceRule = class
  public
    Filename: string;
    Score: integer;
    Targets: string; // comma separated list of OS, CPU, e.g. win32,unix,i386 or * for all
    function FitsTargets(const FilterTargets: string): boolean;
    function FitsFilename(const aFilename: string): boolean;
    function IsEqual(Rule: TFPCSourceRule): boolean;
    procedure Assign(Rule: TFPCSourceRule);
  end;

  { TFPCSourceRules }

  TFPCSourceRules = class
  private
    FChangeStamp: integer;
    FItems: TFPList;// list of TFPCSourceRule
    FScore: integer;
    FTargets: string;
    function GetItems(Index: integer): TFPCSourceRule;
    procedure SetTargets(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IsEqual(Rules: TFPCSourceRules): boolean;
    procedure Assign(Rules: TFPCSourceRules);
    function Clone: TFPCSourceRules;
    property Items[Index: integer]: TFPCSourceRule read GetItems; default;
    function Count: integer;
    function Add(const Filename: string): TFPCSourceRule;
    function GetDefaultTargets(TargetOS, TargetCPU: string): string;
    procedure GetRulesForTargets(Targets: string;
                                 var RulesSortedForFilenameStart: TAVLTree);
    function GetScore(Filename: string;
                      RulesSortedForFilenameStart: TAVLTree): integer;
    property Score: integer read FScore write FScore; // used for Add
    property Targets: string read FTargets write SetTargets; // used for Add, e.g. win32,unix,bsd or * for all
    property ChangeStamp: integer read FChangeStamp;
    procedure IncreaseChangeStamp;
  end;

var
  DefaultFPCSourceRules: TFPCSourceRules;
  
const
  DefineTemplateFlagNames: array[TDefineTemplateFlag] of shortstring = (
      'AutoGenerated'
    );

type
  TFPCInfoType = (
    fpciCompilerDate,      // -iD        Return compiler date
    fpciShortVersion,      // -iV        Return short compiler version
    fpciFullVersion,       // -iW        Return full compiler version
    fpciCompilerOS,        // -iSO       Return compiler OS
    fpciCompilerProcessor, // -iSP       Return compiler host processor
    fpciTargetOS,          // -iTO       Return target OS
    fpciTargetProcessor    // -iTP       Return target processor
    );
  TFPCInfoTypes = set of TFPCInfoType;
  TFPCInfoStrings = array[TFPCInfoType] of string;
const
  fpciAll = [low(TFPCInfoType)..high(TFPCInfoType)];

type

  { TPCConfigFileState
    Stores if a config file exists and its modification date }

  TPCConfigFileState = class
  public
    Filename: string;
    FileExists: boolean;
    FileDate: longint;
    constructor Create(const aFilename: string;
                       aFileExists: boolean; aFileDate: longint);
    function Equals(Other: TPCConfigFileState; CheckDate: boolean): boolean; reintroduce;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  TFPCConfigFileState = TPCConfigFileState deprecated 'use TPCConfigFileState'; // Laz 1.9

  { TPCConfigFileStateList
    list of TPCConfigFileState }

  TPCConfigFileStateList = class
  private
    fItems: TFPList;
    function GetItems(Index: integer): TPCConfigFileState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(List: TPCConfigFileStateList);
    function Equals(List: TPCConfigFileStateList; CheckDates: boolean): boolean; reintroduce;
    function Add(aFilename: string; aFileExists: boolean;
                 aFileDate: longint): TPCConfigFileState;
    function Count: integer;
    property Items[Index: integer]: TPCConfigFileState read GetItems; default;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  TFPCConfigFileStateList = TPCConfigFileStateList deprecated 'use TPCConfigFileStateList'; // Laz 1.9

  { TPCFPMFileState
    Stores information about a fppkg .fpm file }

  TPCFPMFileState = class
  public
    Name: string;
    FPMFilename: string;
    FileDate: longint;
    SourcePath: string;
    UnitToSrc: TStringToStringTree; // case insensitive unit name to source file
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(List: TPCFPMFileState);
    function Equals(List: TPCFPMFileState; CheckDates: boolean): boolean; reintroduce;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  TPCTargetConfigCaches = class;

  { TPCTargetConfigCache
    Storing all information (macros, search paths) of one compiler
    with one specific TargetOS and TargetCPU. }

  TPCTargetConfigCache = class(TComponent)
  private
    FChangeStamp: integer;
  public
    // key
    TargetOS: string; // will be passed lowercase
    TargetCPU: string; // will be passed lowercase
    Compiler: string; // full file name
    CompilerOptions: string; // e.g. -V<version> -Xp<path>
    // values
    Kind: TPascalCompiler;
    CompilerDate: longint;
    RealCompiler: string; // when Compiler is fpc.exe, this is the real compiler (e.g. ppc386.exe)
    RealCompilerDate: longint;
    RealTargetOS: string;
    RealTargetCPU: string;
    RealTargetCPUCompiler: string; // the ppc<target>.exe in PATH for TargetCPU
    FullVersion: string; // Version.Release.Patch
    ConfigFiles: TPCConfigFileStateList;
    UnitPaths: TStrings;
    IncludePaths: TStrings;
    UnitScopes: TStrings;
    Defines: TStringToStringTree; // macro to value
    Undefines: TStringToStringTree; // macro
    Units: TStringToStringTree; // unit name to file name
    Includes: TStringToStringTree; // inc name to file name
    UnitToFPM: TStringToPointerTree; // unitname to TPCFPMFileState
    FPMNameToFPM: TStringToPointerTree; // fpm name to TPCFPMFileState
    ErrorMsg: string;
    ErrorTranslatedMsg: string;
    Caches: TPCTargetConfigCaches;
    HasPPUs: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; // values, not keys
    function Equals(Item: TPCTargetConfigCache;
                    CompareKey: boolean = true): boolean; reintroduce;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    function NeedsUpdate: boolean;
    function GetFPCInfoCmdLineOptions(ExtraOptions: string): string;
    function Update(TestFilename: string; ExtraOptions: string = '';
                    const OnProgress: TDefinePoolProgress = nil): boolean;
    function FindDefaultTargetCPUCompiler(aTargetCPU: string; ResolveLinks: boolean): string;
    function GetUnitPaths: string;
    function GetFPCVerNumbers(out FPCVersion, FPCRelease, FPCPatch: integer): boolean;
    function GetFPCVer: string; // e.g. 2.7.1
    function GetFPC_FULLVERSION: integer; // e.g. 20701
    function IndexOfUsedCfgFile: integer;
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
  end;

  TFPCTargetConfigCache = TPCTargetConfigCache deprecated 'use TPCTargetConfigCache'; // Laz 1.9

  { TPCTargetConfigCaches
    List of TPCTargetConfigCache }

  TPCTargetConfigCaches = class(TComponent)
  private
    FChangeStamp: integer;
    FExtraOptions: string;
    fItems: TAVLTree; // tree of TPCTargetConfigCache
    FTestFilename: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Caches: TPCTargetConfigCaches): boolean; reintroduce;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
    function Find(CompilerFilename, CompilerOptions, TargetOS, TargetCPU: string;
                  CreateIfNotExists: boolean): TPCTargetConfigCache;
    procedure GetDefaultCompilerTarget(const CompilerFilename,CompilerOptions: string;
                  out TargetOS, TargetCPU: string);
    function GetListing: string;
    property TestFilename: string read FTestFilename write FTestFilename; // an empty file to test the compiler, will be auto created
    property ExtraOptions: string read FExtraOptions write FExtraOptions; // additional compiler options not used as key, e.g. -Fr<language file>
  end;

  TFPCTargetConfigCaches = TPCTargetConfigCaches deprecated 'use TPCTargetConfigCaches'; // Laz 1.9

  TFPCSourceCaches = class;

  { TFPCSourceCache
    All source files of one FPC source directory }

  TFPCSourceCache = class(TComponent)
  private
    FChangeStamp: integer;
  public
    Directory: string;
    Valid: boolean;
    Files: TStringList;
    Caches: TFPCSourceCaches;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function Equals(Cache: TFPCSourceCache): boolean; reintroduce;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    procedure Update(const OnProgress: TDefinePoolProgress = nil);
    procedure Update(var NewFiles: TStringList); // NewFiles is used for Files! do not free NewFiles
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
  end;

  { TFPCSourceCaches }

  TFPCSourceCaches = class(TComponent)
  private
    FChangeStamp: integer;
    fItems: TAVLTree; // tree of TFPCSourceCacheItem
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function Equals(Caches: TFPCSourceCaches): boolean; reintroduce;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
    function Find(Directory: string;
                  CreateIfNotExists: boolean): TFPCSourceCache;
  end;

  TCompilerDefinesCache = class;

  TFPCUnitToSrcCacheFlag = (
    fuscfSrcRulesNeedUpdate,
    fuscfUnitTreeNeedsUpdate
    );
  TFPCUnitToSrcCacheFlags = set of TFPCUnitToSrcCacheFlag;

  { TFPCUnitSetCache
    Unit name to FPC source file.
    Specific to one compiler, compileroptions, targetos, targetcpu and FPC source directory. }

  TFPCUnitSetCache = class(TComponent)
  private
    FCaches: TCompilerDefinesCache;
    FChangeStamp: integer;
    FCompilerFilename: string;
    FCompilerOptions: string;
    FFPCSourceDirectory: string;
    FTargetCPU: string;
    FTargetOS: string;
    FConfigCache: TPCTargetConfigCache;
    fSourceCache: TFPCSourceCache;
    fSourceRules: TFPCSourceRules;
    fRulesStampOfConfig: integer; // fSourceCache.ChangeStamp while creation of fFPCSourceRules
    fUnitToSourceTree: TStringToStringTree; // unit name to file name (maybe relative)
    fUnitStampOfFPC: integer;   // FConfigCache.ChangeStamp at creation of fUnitToSourceTree
    fUnitStampOfFiles: integer; // fSourceCache.ChangeStamp at creation of fUnitToSourceTree
    fUnitStampOfRules: integer; // fSourceRules.ChangeStamp at creation of fUnitToSourceTree
    fSrcDuplicates: TStringToStringTree; // unit to list of files (semicolon separated)
    fFlags: TFPCUnitToSrcCacheFlags;
    procedure SetCompilerFilename(const AValue: string);
    procedure SetCompilerOptions(const AValue: string);
    procedure SetFPCSourceDirectory(const AValue: string);
    procedure SetTargetCPU(const AValue: string);
    procedure SetTargetOS(const AValue: string);
    procedure ClearConfigCache;
    procedure ClearSourceCache;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Init;
    property Caches: TCompilerDefinesCache read FCaches;
    property CompilerFilename: string read FCompilerFilename write SetCompilerFilename;
    property CompilerOptions: string read FCompilerOptions write SetCompilerOptions;
    property TargetOS: string read FTargetOS write SetTargetOS; // case insensitive, will be passed lowercase
    property TargetCPU: string read FTargetCPU write SetTargetCPU; // case insensitive, will be passed lowercase
    property FPCSourceDirectory: string read FFPCSourceDirectory write SetFPCSourceDirectory;
    function GetConfigCache(AutoUpdate: boolean): TPCTargetConfigCache;
    function GetSourceCache(AutoUpdate: boolean): TFPCSourceCache;
    function GetSourceRules(AutoUpdate: boolean): TFPCSourceRules;
    function GetUnitToSourceTree(AutoUpdate: boolean): TStringToStringTree; // unit name to file name (maybe relative)
    function GetSourceDuplicates(AutoUpdate: boolean): TStringToStringTree; // unit to semicolon separated list of files
    function GetUnitSrcFile(const AnUnitName: string;
                            SrcSearchRequiresPPU: boolean = true;
                            SkipPPUCheckIfTargetIsSourceOnly: boolean = true): string;
    function GetCompiledUnitFile(const AUnitName: string): string;
    property ChangeStamp: integer read FChangeStamp;
    class function GetInvalidChangeStamp: integer;
    procedure IncreaseChangeStamp;
    function GetUnitSetID: string;
    function GetFirstFPCCfg: string;
    function GetUnitScopes: string;
    function GetCompilerKind: TPascalCompiler;
  end;

  { TCompilerDefinesCache }

  TCompilerDefinesCache = class(TComponent)
  private
    FConfigCaches: TPCTargetConfigCaches;
    FConfigCachesSaveStamp: integer;
    FSourceCaches: TFPCSourceCaches;
    FSourceCachesSaveStamp: integer;
    fUnitToSrcCaches: TFPList; // list of TFPCUnitSetCache
    function GetExtraOptions: string;
    function GetTestFilename: string;
    procedure SetConfigCaches(const AValue: TPCTargetConfigCaches);
    procedure SetExtraOptions(AValue: string);
    procedure SetSourceCaches(const AValue: TFPCSourceCaches);
    procedure ClearUnitToSrcCaches;
    procedure SetTestFilename(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromFile(Filename: string);
    procedure SaveToFile(Filename: string);
    function NeedsSave: boolean;
    property SourceCaches: TFPCSourceCaches read FSourceCaches write SetSourceCaches;
    property ConfigCaches: TPCTargetConfigCaches read FConfigCaches write SetConfigCaches;
    property TestFilename: string read GetTestFilename write SetTestFilename; // an empty file to test the compiler, will be auto created
    property ExtraOptions: string read GetExtraOptions write SetExtraOptions; // additional compiler options not used as key, e.g. -Fr<language file>
    function GetFPCVersion(const CompilerFilename, TargetOS, TargetCPU: string;
                           UseCompiledVersionAsDefault: boolean): string; deprecated 'use GetPCVersion'; // 2.0.1
    function GetPCVersion(const CompilerFilename, TargetOS, TargetCPU: string;
                          UseCompiledVersionAsDefault: boolean;
                          out Kind: TPascalCompiler): string;
    function FindUnitSet(const CompilerFilename, TargetOS, TargetCPU,
                         Options, FPCSrcDir: string;
                         CreateIfNotExists: boolean): TFPCUnitSetCache;
    function FindUnitSetWithID(const UnitSetID: string; out Changed: boolean;
                               CreateIfNotExists: boolean): TFPCUnitSetCache;
    function GetUnitSetID(CompilerFilename, TargetOS, TargetCPU, Options,
                          FPCSrcDir: string; ChangeStamp: integer): string;
    procedure ParseUnitSetID(const ID: string; out CompilerFilename,
                             TargetOS, TargetCPU, Options, FPCSrcDir: string;
                             out ChangeStamp: integer);
  end;

  TFPCDefinesCache = TCompilerDefinesCache deprecated 'use TCompilerDefinesCache'; // 1.9

function DefineActionNameToAction(const s: string): TDefineAction;
function DefineTemplateFlagsToString(Flags: TDefineTemplateFlags): string;
function GetDefaultSrcOSForTargetOS(const TargetOS: string): string;
function GetDefaultSrcOS2ForTargetOS(const TargetOS: string): string;
function GetDefaultSrcCPUForTargetCPU(const TargetCPU: string): string;
procedure SplitLazarusCPUOSWidgetCombo(const Combination: string;
  out CPU, OS, WidgetSet: string);
function GetCompiledFPCVersion: integer;
function GetCompiledTargetOS: string;
function GetCompiledTargetCPU: string;
function GetDefaultCompilerFilename(const TargetCPU: string = ''; Cross: boolean = false): string;
procedure GetTargetProcessors(const TargetCPU: string; aList: TStrings);
function GetFPCTargetOS(TargetOS: string): string; // normalize
function GetFPCTargetCPU(TargetCPU: string): string; // normalize

function IsCTExecutable(AFilename: string; out ErrorMsg: string): boolean; // not thread-safe

function GuessPascalCompilerFromExeName(Filename: string): TPascalCompiler; // thread-safe
function IsCompilerExecutable(AFilename: string; out ErrorMsg: string;
  out Kind: TPascalCompiler; Run: boolean): boolean; // not thread-safe
function IsFPCExecutable(AFilename: string; out ErrorMsg: string; Run: boolean): boolean; deprecated; // 2.1, not thread-safe
function IsPas2JSExecutable(AFilename: string; out ErrorMsg: string; Run: boolean): boolean; deprecated; // 2.1, not thread-safe

// functions to quickly setup some defines
function CreateDefinesInDirectories(const SourcePaths, FlagName: string
                                    ): TDefineTemplate;

function GatherFiles(Directory, ExcludeDirMask, IncludeFileMask: string;
         MaxLevel: integer; const OnProgress: TDefinePoolProgress): TStringList; // thread safe
function GatherFilesInFPCSources(Directory: string;
                            const OnProgress: TDefinePoolProgress): TStringList; // thread safe
function MakeRelativeFileList(Files: TStrings; out BaseDir: string): TStringList;
function Compress1FileList(Files: TStrings): TStringList; // thread-safe
function Decompress1FileList(Files: TStrings): TStringList; // thread-safe
function RunTool(const Filename: string; Params: TStrings;
                 WorkingDirectory: string = ''; Quiet: boolean = false): TStringList; // thread-safe
function RunTool(const Filename, Params: string;
                 WorkingDirectory: string = ''; Quiet: boolean = false): TStringList; // thread-safe

type
  // fpc parameter effecting search for compiler
  TFPCFrontEndParam = (
    fpcpT, // -T<targetos>
    fpcpP, // -P<targetprocessor>
    fpcpV, // -V<postfix>
    fpcpXp // -Xp<directory>
    );
  TFPCFrontEndParams = set of TFPCFrontEndParam;
const
  AllFPCFrontEndParams = [low(TFPCFrontEndParam)..high(TFPCFrontEndParam)];

function ParseFPCInfo(FPCInfo: string; InfoTypes: TFPCInfoTypes;
                      out Infos: TFPCInfoStrings): boolean;
function RunFPCInfo(const CompilerFilename: string;
                   InfoTypes: TFPCInfoTypes; const Options: string =''): string;
function FPCVersionToNumber(const FPCVersionString: string): integer; // 2.7.1 -> 20701
function SplitFPCVersion(const FPCVersionString: string;
                        out FPCVersion, FPCRelease, FPCPatch: integer): boolean; // 2.7.1 -> 2,7,1
function ParseFPCVerbose(List: TStrings; // fpc -va output
                         const WorkDir: string;
                         out ConfigFiles: TStrings; // prefix '-' for file not found, '+' for found and read
                         out RealCompilerFilename: string; // what compiler is used by fpc
                         out UnitPaths: TStrings; // unit search paths
                         out IncludePaths: TStrings; // include search paths
                         out UnitScopes: TStrings; // unit scopes/namespaces
                         out Defines, Undefines: TStringToStringTree): boolean;
function RunFPCVerbose(const CompilerFilename, TestFilename: string;
                       out ConfigFiles: TStrings;
                       out RealCompilerFilename: string;
                       out UnitPaths: TStrings;
                       out IncludePaths: TStrings;
                       out UnitScopes: TStrings; // unit scopes/namespaces
                       out Defines, Undefines: TStringToStringTree;
                       const Options: string = ''): boolean;
procedure GatherUnitsInSearchPaths(SearchUnitPaths, SearchIncludePaths: TStrings;
                    const OnProgress: TDefinePoolProgress;
                    out Units: TStringToStringTree;
                    out Includes: TStringToStringTree); // unit names to full file name
procedure GatherUnitsInFPMSources(Units: TStringToStringTree; // unit names to full file name
                    out UnitToFPM: TStringToPointerTree;
                    out FPMNameToFPM: TStringToPointerTree; // TPCFPMFileState
                    const OnProgress: TDefinePoolProgress = nil
                    );
function GatherUnitSourcesInDirectory(Directory: string;
                    MaxLevel: integer = 1): TStringToStringTree; // unit names to full file name
procedure AdjustFPCSrcRulesForPPUPaths(Units: TStringToStringTree;
                                       Rules: TFPCSourceRules); // not for pas2js
function GatherUnitsInFPCSources(Files: TStringList;
                   TargetOS: string = ''; TargetCPU: string = '';
                   Duplicates: TStringToStringTree = nil; // unit to semicolon separated list of files
                   Rules: TFPCSourceRules = nil;
                   const DebugUnitName: string = ''): TStringToStringTree; // not for pas2js
function CreateFPCTemplate(Config: TPCTargetConfigCache;
                           Owner: TObject): TDefineTemplate; overload;
function CreateFPCTemplate(Config: TFPCUnitSetCache;
                           Owner: TObject): TDefineTemplate; overload;
function CreateFPCSourceTemplate(Config: TFPCUnitSetCache;
                                 Owner: TObject): TDefineTemplate; overload; // not for pas2js
function CreateFPCSourceTemplate(FPCSrcDir: string;
                                 Owner: TObject): TDefineTemplate; overload; // not for pas2js
procedure CheckPPUSources(PPUFiles,  // unitname to filename
                          UnitToSource, // unitname to file name
                          UnitToDuplicates: TStringToStringTree; // unitname to semicolon separated list of files
                          var Duplicates, Missing: TStringToStringTree); // not for pas2js
procedure LoadFPCCacheFromFile(Filename: string;
            var Configs: TPCTargetConfigCaches; var Sources: TFPCSourceCaches);
procedure SaveFPCCacheToFile(Filename: string;
                    Configs: TPCTargetConfigCaches; Sources: TFPCSourceCaches);

// FPC
const
  FPCParamEnabled = 'true';

type
  TFPCParamKind = (
    fpkUnknown,
    fpkBoolean, // Values: true = FPCParamEnabled otherwise false
    fpkValue,
    fpkMultiValue, // e.g. -k
    fpkDefine, // -d and -u options
    fpkConfig, // @ parameter
    fpkNonOption  // e.g. source file
    );
  TFPCParamFlag = (
    fpfUnset, // use default, e.g. turns an fpkDefine into an Undefine
    fpfSetTwice,
    fpfValueChanged);
  TFPCParamFlags = set of TFPCParamFlag;

  { TFPCParamValue }

  TFPCParamValue = class
  public
    Name: string;
    Value: string;
    Kind: TFPCParamKind;
    Flags: TFPCParamFlags;
    constructor Create(const aName, aValue: string; aKind: TFPCParamKind; aFlags: TFPCParamFlags = []);
  end;
procedure ParseFPCParameters(const CmdLineParams: string;
  Params: TObjectList { list of TFPCParamValue }; ReadBackslash: boolean = false);
procedure ParseFPCParameters(CmdLineParams: TStrings;
  ParsedParams: TObjectList { list of TFPCParamValue });
procedure ParseFPCParameter(const CmdLineParam: string;
  ParsedParams: TObjectList { list of TFPCParamValue });
function IndexOfFPCParamValue(ParsedParams: TObjectList { list of TFPCParamValue };
  const Name: string): integer;
function GetFPCParamValue(ParsedParams: TObjectList { list of TFPCParamValue };
  const Name: string): TFPCParamValue;
function dbgs(k: TFPCParamKind): string; overload;
function dbgs(f: TFPCParamFlag): string; overload;
function dbgs(const Flags: TFPCParamFlags): string; overload;
function ExtractFPCFrontEndParameters(const CmdLineParams: string;
  const Kinds: TFPCFrontEndParams = AllFPCFrontEndParams): string;

procedure ReadMakefileFPC(const Filename: string; List: TStrings);
procedure ParseMakefileFPC(const Filename, SrcOS: string;
                           out Dirs, SubDirs: string);

function CompareFPCSourceRulesViaFilename(Rule1, Rule2: Pointer): integer;
function CompareFPCTargetConfigCacheItems(CacheItem1, CacheItem2: Pointer): integer;
function CompareFPCSourceCacheItems(CacheItem1, CacheItem2: Pointer): integer;
function CompareDirectoryWithFPCSourceCacheItem(AString, CacheItem: Pointer): integer;


implementation


type
  TUnitNameLink = class
  public
    Unit_Name: string;
    Filename: string;
    ConflictFilename: string;
    MacroCount: integer;
    UsedMacroCount: integer;
    Score: integer;
  end;

function CompareUnitNameLinks(Link1, Link2: Pointer): integer;
var
  UnitLink1: TUnitNameLink absolute Link1;
  UnitLink2: TUnitNameLink absolute Link2;
begin
  Result:=CompareNames(UnitLink1.Unit_Name,UnitLink2.Unit_Name);
end;

function CompareUnitNameWithUnitNameLink(Name, Link: Pointer): integer;
var
  UnitLink: TUnitNameLink absolute Link;
begin
  Result:=CompareNames(AnsiString(Name),UnitLink.Unit_Name);
end;

// some useful functions

function GatherFiles(Directory, ExcludeDirMask, IncludeFileMask: string;
  MaxLevel: integer; const OnProgress: TDefinePoolProgress): TStringList;
{  ExcludeDirMask: check FilenameIsMatching vs the short file name of a directory
   IncludeFileMask: check FilenameIsMatching vs the short file name of a file
}
var
  Files: TAVLTree; // tree of ansistring
  FileCount: integer;
  Abort: boolean;

  procedure Add(Filename: string);
  var
    s: String;
  begin
    if Filename='' then exit;
    // increase refcount
    s:=Filename;
    // add
    Files.Add(PChar(s));
    // keep refcount
    Pointer(s):=nil;
  end;

  procedure Search(CurDir: string; Level: integer);
  var
    FileInfo: TSearchRec;
    ShortFilename: String;
    Filename: String;
  begin
    if Level>MaxLevel then exit;
    //DebugLn(['Search CurDir=',CurDir]);
    if FindFirstUTF8(Directory+CurDir+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        inc(FileCount);
        if (FileCount mod 100=0) and Assigned(OnProgress) then begin
          OnProgress(nil,0,-1,'Scanned files: '+IntToStr(FileCount),Abort);
          if Abort then break;
        end;
        ShortFilename:=FileInfo.Name;
        if (ShortFilename='') or (ShortFilename='.') or (ShortFilename='..') then
          continue;
        //debugln(['Search ShortFilename=',ShortFilename,' IsDir=',(FileInfo.Attr and faDirectory)>0]);
        Filename:=CurDir+ShortFilename;
        if (FileInfo.Attr and faDirectory)>0 then begin
          // directory
          if (ExcludeDirMask='')
          or (not FilenameIsMatching(ExcludeDirMask,ShortFilename,true))
          then begin
            Search(Filename+PathDelim,Level+1);
            if Abort then break;
          end else begin
            //DebugLn(['Search DIR MISMATCH ',Filename]);
          end;
        end else begin
          // file
          if (IncludeFileMask='')
          or FilenameIsMatching(IncludeFileMask,ShortFilename,true) then begin
            //DebugLn(['Search ADD ',Filename]);
            Add(Filename);
          end else begin
            //DebugLn(['Search MISMATCH ',Filename]);
          end;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;

var
  Node: TAVLTreeNode;
  s: String;
  NodeMgr: TAVLTreeNodeMemManager;
begin
  Result:=nil;
  Files:=TAVLTree.Create(@CompareAnsiStringFilenames);
  NodeMgr:=TAVLTreeNodeMemManager.Create;
  Files.SetNodeManager(NodeMgr);
  Abort:=false;
  try
    FileCount:=0;
    Directory:=TrimAndExpandDirectory(Directory);
    if Directory='' then exit;
    Search('',0);
  finally
    if not Abort then
      Result:=TStringList.Create;
    Node:=Files.FindLowest;
    while Node<>nil do begin
      Pointer(s):=Node.Data;
      if Result<>nil then Result.Add(s);
      s:='';
      Node:=Files.FindSuccessor(Node);
    end;
    FreeAndNil(Files);
    FreeAndNil(NodeMgr);
  end;
end;

function GatherFilesInFPCSources(Directory: string;
  const OnProgress: TDefinePoolProgress): TStringList;
begin
  Result:=GatherFiles(Directory,'{.*,CVS}',
                      '{*.pas,*.pp,*.p,*.inc,Makefile.fpc}',8,OnProgress);
end;

function MakeRelativeFileList(Files: TStrings; out BaseDir: string
  ): TStringList;
var
  BaseDirLen: Integer;
  i: Integer;
  Filename: string;
begin
  BaseDir:='';
  Result:=TStringList.Create;
  if (Files=nil) or (Files.Count=0) then exit;
  Result.Assign(Files);
  // delete empty lines
  for i:=Result.Count-1 downto 0 do
    if Result[i]='' then Result.Delete(i);
  if Result.Count=0 then exit;
  // find shortest common BaseDir
  BaseDir:=ChompPathDelim(ExtractFilepath(Result[0]));
  BaseDirLen:=length(BaseDir);
  for i:=1 to Result.Count-1 do begin
    Filename:=Result[i];
    while (BaseDirLen>0) do begin
      if (BaseDirLen<=length(Filename))
      and ((BaseDirLen=length(Filename)) or (Filename[BaseDirLen+1]=PathDelim))
      and (CompareFilenames(BaseDir,copy(Filename,1,BaseDirLen))=0) then
        break;
      BaseDir:=ChompPathDelim(ExtractFilePath(copy(BaseDir,1,BaseDirLen-1)));
      BaseDirLen:=length(BaseDir);
      if (BaseDir='') or (BaseDir[length(BaseDir)]=PathDelim) then break;
    end;
  end;
  // create relative paths
  if (BaseDir<>'') then
    for i:=0 to Result.Count-1 do begin
      Filename:=Result[i];
      delete(Filename,1,BaseDirLen);
      if (Filename<>'') and (Filename[1]=PathDelim) then
        System.Delete(Filename,1,1);
      Result[i]:=Filename;
    end;
end;

function Compress1FileList(Files: TStrings): TStringList;
var
  i: Integer;
  Filename: string;
  LastFilename: String;
  p: Integer;
begin
  Result:=TStringList.Create;
  LastFilename:='';
  for i:=0 to Files.Count-1 do begin
    Filename:=TrimFilename(Files[i]);
    p:=1;
    while (p<=length(Filename)) and (p<=length(LastFilename))
    and (Filename[p]=LastFilename[p]) do
      inc(p);
    Result.Add(IntToStr(p-1)+':'+copy(Filename,p,length(Filename)));
    LastFilename:=Filename;
  end;
end;

function Decompress1FileList(Files: TStrings): TStringList;
var
  LastFilename: String;
  i: Integer;
  Filename: string;
  p: Integer;
  Same: Integer;
begin
  Result:=TStringList.Create;
  LastFilename:='';
  try
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      p:=1;
      Same:=0;
      while (p<=length(Filename)) and (Filename[p] in ['0'..'9']) do begin
        Same:=Same*10+ord(Filename[p])-ord('0');
        inc(p);
      end;
      inc(p);
      Filename:=copy(LastFilename,1,Same)+copy(Filename,p,length(Filename));
      Result.Add(Filename);
      LastFilename:=Filename;
    end;
  except
  end;
end;

function RunTool(const Filename: string; Params: TStrings;
  WorkingDirectory: string; Quiet: boolean): TStringList;
var
  buf: string;
  TheProcess: TProcessUTF8;
  OutputLine: String;
  OutLen: Integer;
  LineStart, i: Integer;
begin
  Result:=nil;
  if not FileIsExecutable(Filename) then
    exit(nil);
  if (WorkingDirectory<>'') and not DirPathExists(WorkingDirectory) then
    exit(nil);
  Result:=TStringList.Create;
  try
    buf:='';
    if (MainThreadID=GetCurrentThreadId) and not Quiet then begin
      DbgOut(['Hint: (lazarus) [RunTool] "',Filename,'"']);
      for i:=0 to Params.Count-1 do
        dbgout(' "',Params[i],'"');
      Debugln;
    end;
    TheProcess := TProcessUTF8.Create(nil);
    try
      TheProcess.Executable := Filename;
      TheProcess.Parameters:=Params;
      TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
      TheProcess.ShowWindow := swoHide;
      TheProcess.CurrentDirectory:=WorkingDirectory;
      TheProcess.Execute;
      OutputLine:='';
      SetLength(buf,4096);
      repeat
        if (TheProcess.Output<>nil) then begin
          OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
        end else
          OutLen:=0;
        //debugln(['RunTool OutLen=',OutLen,' Buf="',copy(Buf,1,OutLen),'"']);
        LineStart:=1;
        i:=1;
        while i<=OutLen do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            Result.Add(OutputLine);
            OutputLine:='';
            if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=OutputLine+copy(Buf,LineStart,OutLen-LineStart+1);
      until OutLen=0;
      //debugln(['RunTool Last=',OutputLine]);
      if OutputLine<>'' then
        Result.Add(OutputLine);
      TheProcess.WaitOnExit;
    finally
      TheProcess.Free;
    end;
  except
    FreeAndNil(Result);
  end;
end;

function RunTool(const Filename, Params: string; WorkingDirectory: string;
  Quiet: boolean): TStringList;
var
  ParamList: TStringList;
begin
  ParamList:=TStringList.Create;
  try
    SplitCmdLineParams(Params,ParamList);
    Result:=RunTool(Filename,ParamList,WorkingDirectory,Quiet);
  finally
    ParamList.Free;
  end;
end;

function ParseFPCInfo(FPCInfo: string; InfoTypes: TFPCInfoTypes;
  out Infos: TFPCInfoStrings): boolean;
var
  i: TFPCInfoType;
  p: PChar;
  StartPos: PChar;
begin
  Result:=false;
  if FPCInfo='' then exit(InfoTypes=[]);
  if copy(FPCInfo,1,6)='Error:' then exit(false);

  p:=PChar(FPCInfo);
  for i:=low(TFPCInfoType) to high(TFPCInfoType) do begin
    if not (i in InfoTypes) then continue;
    StartPos:=p;
    while not (p^ in [' ',#0]) do inc(p);
    if (p=StartPos) then exit(false);
    Infos[i]:=copy(FPCInfo,StartPos-PChar(FPCInfo)+1,p-StartPos);
    // skip space
    if p^<>#0 then
      inc(p);
  end;
  Result:=true;
end;

function RunFPCInfo(const CompilerFilename: string;
  InfoTypes: TFPCInfoTypes; const Options: string): string;
var
  Param: String;
  List: TStringList;
  Params: TStringListUTF8;
begin
  Result:='';
  Param:='';
  if fpciCompilerDate in InfoTypes then Param:=Param+'D';
  if fpciShortVersion in InfoTypes then Param:=Param+'V';
  if fpciFullVersion in InfoTypes then Param:=Param+'W';
  if fpciCompilerOS in InfoTypes then Param:=Param+'SO';
  if fpciCompilerProcessor in InfoTypes then Param:=Param+'SP';
  if fpciTargetOS in InfoTypes then Param:=Param+'TO';
  if fpciTargetProcessor in InfoTypes then Param:=Param+'TP';
  if Param='' then exit;
  Param:='-i'+Param;
  List:=nil;
  Params:=TStringListUTF8.Create;
  try
    Params.Add(Param);
    SplitCmdLineParams(Options,Params);
    List:=RunTool(CompilerFilename,Params);
    if (List=nil) or (List.Count<1) then exit;
    Result:=List[0];
    if copy(Result,1,6)='Error:' then Result:='';
  finally
    Params.Free;
    List.free;
  end;
end;

function FPCVersionToNumber(const FPCVersionString: string): integer;
var
  FPCVersion, FPCRelease, FPCPatch: integer;
begin
  if SplitFPCVersion(FPCVersionString,FPCVersion,FPCRelease,FPCPatch) then
    Result:=FPCVersion*10000+FPCRelease*100+FPCPatch
  else
    Result:=0;
end;

function SplitFPCVersion(const FPCVersionString: string; out FPCVersion,
  FPCRelease, FPCPatch: integer): boolean;
// for example 2.5.1
var
  p: PChar;

  function ReadWord(out v: integer): boolean;
  var
    Empty: Boolean;
  begin
    v:=0;
    Empty:=true;
    while (p^ in ['0'..'9']) do begin
      if v>10000 then exit(false);
      v:=v*10+ord(p^)-ord('0');
      inc(p);
      Empty:=false;
    end;
    Result:=not Empty;
  end;

begin
  Result:=false;
  FPCVersion:=0;
  FPCRelease:=0;
  FPCPatch:=0;
  if FPCVersionString='' then exit;
  p:=PChar(FPCVersionString);
  if not ReadWord(FPCVersion) then exit;
  if (p^<>'.') then exit;
  inc(p);
  if not ReadWord(FPCRelease) then exit;
  if (p^<>'.') then exit;
  inc(p);
  if not ReadWord(FPCPatch) then exit;
  Result:=true;
end;

function ParseFPCVerbose(List: TStrings; const WorkDir: string; out
  ConfigFiles: TStrings; out RealCompilerFilename: string; out
  UnitPaths: TStrings; out IncludePaths: TStrings; out UnitScopes: TStrings; out
  Defines, Undefines: TStringToStringTree): boolean;

  function DeQuote(const s: string): string;
  begin
    if (length(s)>1) and (s[1]='"') and (s[length(s)]='"') then
      Result:=AnsiDequotedStr(s,'"')
    else
      Result:=s;
  end;

  procedure UndefineSymbol(const MacroName: string);
  begin
    {$IFDEF VerboseFPCSrcScan}
    DebugLn(['UndefineSymbol ',MacroName]);
    {$ENDIF}
    Defines.Remove(MacroName);
    Undefines[MacroName]:='';
  end;

  procedure DefineSymbol(const MacroName, Value: string);
  begin
    {$IFDEF VerboseFPCSrcScan}
    if Value='' then
      DebugLn(['DefineSymbol ',MacroName])
    else
      DebugLn(['DefineSymbol ',MacroName,':=',Value]);
    {$ENDIF}
    Undefines.Remove(MacroName);
    Defines[MacroName]:=Value;
  end;

  function ExpFile(const aFilename: string): string;
  begin
    Result:=DeQuote(aFilename);
    if FilenameIsAbsolute(Result) then exit;
    Result:=AppendPathDelim(WorkDir)+Result;
  end;

  procedure ProcessOutputLine(Line: string);
  var
    UpLine: string;

    function IsUpLine(p: integer; const s: string): boolean;
    begin
      Result:=StrLComp(@UpLine[p], PChar(s), length(s)) = 0;
    end;

  var
    SymbolName, SymbolValue, NewPath: string;
    i, len, CurPos: integer;
    Filename: String;
    p: SizeInt;
  begin
    //DebugLn(['ProcessOutputLine ',Line]);
    Line:=SysToUtf8(Line);
    len := length(Line);
    if len <= 6 then Exit; // shortest match

    CurPos := 1;
    // skip timestamp e.g. [0.306]
    if Line[CurPos] = '[' then begin
      repeat
        inc(CurPos);
        if CurPos > len then Exit;
      until line[CurPos] = ']';
      Inc(CurPos, 2); // skip space too
      if len - CurPos < 6 then Exit; // shortest match
    end;

    UpLine:=UpperCaseStr(Line);
    case UpLine[CurPos] of
    'I':
      if IsUpLine(CurPos,'INFO: ') then
        inc(CurPos,6);
    'E':
      if IsUpLine(CurPos,'ERROR: ') then begin
        inc(CurPos,7);
        if RealCompilerFilename='' then begin
          p:=Pos(' returned an error exitcode',Line);
          if p>0 then
            RealCompilerFilename:=copy(Line,CurPos,p-CurPos);
        end;
        exit;
      end;
    end;

    case UpLine[CurPos] of
    'C':
      if IsUpLine(CurPos,'CONFIGFILE SEARCH: ') then
      begin
        // skip keywords
        Inc(CurPos, 19);
        Filename:=ExpFile(GetForcedPathDelims(copy(Line,CurPos,length(Line))));
        ConfigFiles.Add('-'+Filename);
      end else if IsUpLine(CurPos,'COMPILER: ') then begin
        // skip keywords
        Inc(CurPos, 10);
        RealCompilerFilename:=ExpFile(copy(Line,CurPos,length(Line)));
      end;
    'M':
      if IsUpLine(CurPos,'MACRO ') then begin
        // skip keyword macro
        Inc(CurPos, 6);

        if IsUpLine(CurPos,'DEFINED: ') then begin
          Inc(CurPos, 9);
          SymbolName:=copy(Line, CurPos, len);
          DefineSymbol(SymbolName,'');
          Exit;
        end;

        if IsUpLine(CurPos,'UNDEFINED: ') then begin
          Inc(CurPos, 11);
          SymbolName:=copy(Line,CurPos,len);
          UndefineSymbol(SymbolName);
          Exit;
        end;

        // MACRO something...
        i := CurPos;
        while (i <= len) and (Line[i]<>' ') do inc(i);
        SymbolName:=copy(Line,CurPos,i-CurPos);
        CurPos := i + 1; // skip space

        if IsUpLine(CurPos,'SET TO ') then begin
          // MACRO name SET TO "value"
          Inc(CurPos, 7);
          SymbolValue:=DeQuote(copy(Line, CurPos, len));
          DefineSymbol(SymbolName, SymbolValue);
        end;
      end;
    'R':
      if IsUpLine(CurPos,'READING OPTIONS FROM FILE ') then
      begin
        // skip keywords
        Inc(CurPos, 26);
        Filename:=ExpFile(GetForcedPathDelims(copy(Line,CurPos,length(Line))));
        if (ConfigFiles.Count>0)
        and (ConfigFiles[ConfigFiles.Count-1]='-'+Filename) then
          ConfigFiles.Delete(ConfigFiles.Count-1);
        {$IFDEF VerboseFPCSrcScan}
        DebugLn('Used options file: "',Filename,'"');
        {$ENDIF}
        ConfigFiles.Add('+'+Filename);
      end;
    'U':
      if IsUpLine(CurPos,'USING UNIT PATH: ') then begin
        Inc(CurPos, 17);
        NewPath:=ExpFile(GetForcedPathDelims(DeQuote(copy(Line,CurPos,len))));
        NewPath:=ChompPathDelim(TrimFilename(NewPath));
        {$IFDEF VerboseFPCSrcScan}
        DebugLn('Using unit path: "',NewPath,'"');
        {$ENDIF}
        UnitPaths.Add(NewPath);
      end else if IsUpLine(CurPos,'USING INCLUDE PATH: ') then begin
        Inc(CurPos, 20);
        NewPath:=ExpFile(GetForcedPathDelims(DeQuote(copy(Line,CurPos,len))));
        NewPath:=ChompPathDelim(TrimFilename(NewPath));
        {$IFDEF VerboseFPCSrcScan}
        DebugLn('Using include path: "',NewPath,'"');
        {$ENDIF}
        IncludePaths.Add(NewPath);
      end else if IsUpLine(CurPos,'USING UNIT SCOPE: ') then begin
        Inc(CurPos, 18);
        NewPath:=Trim(DeQuote(copy(Line,CurPos,len)));
        {$IFDEF VerboseFPCSrcScan}
        DebugLn('Using unit scope: "',NewPath,'"');
        {$ENDIF}
        UnitScopes.Add(NewPath);
      end;
    end;
  end;

var
  i: Integer;
begin
  Result:=false;
  ConfigFiles:=TStringList.Create;
  RealCompilerFilename:='';
  UnitPaths:=TStringList.Create;
  IncludePaths:=TStringList.Create;
  UnitScopes:=TStringList.Create;
  Defines:=TStringToStringTree.Create(false);
  Undefines:=TStringToStringTree.Create(false);
  try
    for i:=0 to List.Count-1 do
      ProcessOutputLine(List[i]);
    Result:=true;
  finally
    if not Result then begin
      FreeAndNil(ConfigFiles);
      FreeAndNil(UnitPaths);
      FreeAndNil(IncludePaths);
      FreeAndNil(UnitScopes);
      FreeAndNil(Undefines);
      FreeAndNil(Defines);
    end;
  end;
end;

function RunFPCVerbose(const CompilerFilename, TestFilename: string; out
  ConfigFiles: TStrings; out RealCompilerFilename: string; out
  UnitPaths: TStrings; out IncludePaths: TStrings; out UnitScopes: TStrings;
  out Defines, Undefines: TStringToStringTree; const Options: string): boolean;
var
  Params: TStringListUTF8;
  Filename: String;
  WorkDir: String;
  List: TStringList;
  fs: TFileStreamUTF8;
begin
  Result:=false;
  ConfigFiles:=nil;
  RealCompilerFilename:='';
  UnitPaths:=nil;
  IncludePaths:=nil;
  UnitScopes:=nil;
  Defines:=nil;
  Undefines:=nil;

  Params:=TStringListUTF8.Create;
  List:=nil;
  try
    Params.Add('-va');

    if TestFilename<>'' then begin
      // create empty file
      try
        fs:=TFileStreamUTF8.Create(TestFilename,fmCreate);
        fs.Free;
      except
        debugln(['Warning: [RunFPCVerbose] unable to create test file "'+TestFilename+'"']);
        exit;
      end;
      Filename:=ExtractFileName(TestFilename);
      WorkDir:=ExtractFilePath(TestFilename);
      Params.Add(Filename);
    end else
      WorkDir:='';

    SplitCmdLineParams(Options,Params);

    //DebugLn(['RunFPCVerbose ',CompilerFilename,' ',Params,' ',WorkDir]);
    List:=RunTool(CompilerFilename,Params,WorkDir);
    if (List=nil) or (List.Count=0) then begin
      debugln(['Warning: RunFPCVerbose failed: ',CompilerFilename,' ',Params]);
      exit;
    end;
    Result:=ParseFPCVerbose(List,WorkDir,ConfigFiles,RealCompilerFilename,
                            UnitPaths,IncludePaths,UnitScopes,Defines,Undefines);
  finally
    Params.Free;
    List.Free;
    if TestFilename<>'' then
      DeleteFileUTF8(TestFilename);
  end;
end;

procedure GatherUnitsInSearchPaths(SearchUnitPaths, SearchIncludePaths: TStrings;
  const OnProgress: TDefinePoolProgress; out Units: TStringToStringTree;
  out Includes: TStringToStringTree);
{ returns a stringtree,
  where name is unitname and value is the full file name

  SearchUnitsPaths are searched from last to start
  first found wins
  pas, pp, p replaces ppu
}
var
  i: Integer;
  Directory: String;
  FileCount: Integer;
  Abort: boolean;
  FileInfo: TSearchRec;
  ShortFilename: String;
  Filename: String;
  Ext: String;
  File_Name: String;
begin
  // units sources
  Units:=TStringToStringTree.Create(false);
  FileCount:=0;
  Abort:=false;
  if Assigned(SearchUnitPaths) then
    for i:=SearchUnitPaths.Count-1 downto 0 do begin
      Directory:=TrimAndExpandDirectory(SearchUnitPaths[i]);
      if (Directory='') then continue;
      if FindFirstUTF8(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
        repeat
          inc(FileCount);
          if (FileCount mod 100=0) and Assigned(OnProgress) then begin
            OnProgress(nil, 0, -1, Format(ctsScannedFiles, [IntToStr(FileCount)]
              ), Abort);
            if Abort then break;
          end;
          ShortFilename:=FileInfo.Name;
          if (ShortFilename='') or (ShortFilename='.') or (ShortFilename='..') then
            continue;
          //debugln(['GatherUnitsInSearchPaths ShortFilename=',ShortFilename,' IsDir=',(FileInfo.Attr and faDirectory)>0]);
          Filename:=Directory+ShortFilename;
          Ext:=LowerCase(ExtractFileExt(ShortFilename));
          if (Ext='.pas') or (Ext='.pp') or (Ext='.p') or (Ext='.ppu') then begin
            File_Name:=ExtractFileNameOnly(Filename);
            if (not Units.Contains(File_Name))
            or ((Ext<>'.ppu') and (CompareFileExt(Units[File_Name],'ppu',false)=0))
            then
              Units[File_Name]:=Filename;
          end;
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
    end;

  // inc files
  Includes:=TStringToStringTree.Create(false);
  if Assigned(SearchIncludePaths) then
    for i:=SearchIncludePaths.Count-1 downto 0 do begin
      Directory:=TrimAndExpandDirectory(SearchIncludePaths[i]);
      if (Directory='') then continue;
      if FindFirstUTF8(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
        repeat
          inc(FileCount);
          if (FileCount mod 100=0) and Assigned(OnProgress) then begin
            OnProgress(nil, 0, -1, Format(ctsScannedFiles, [IntToStr(FileCount)]
              ), Abort);
            if Abort then break;
          end;
          ShortFilename:=FileInfo.Name;
          if (ShortFilename='') or (ShortFilename='.') or (ShortFilename='..') then
            continue;
          //debugln(['GatherUnitsInSearchPaths ShortFilename=',ShortFilename,' IsDir=',(FileInfo.Attr and faDirectory)>0]);
          Filename:=Directory+ShortFilename;
          Ext:=LowerCase(ExtractFileExt(ShortFilename));
          if (Ext='.inc') then begin
            File_Name:=ExtractFileName(Filename);
            if (not Includes.Contains(File_Name))
            then
              Includes[File_Name]:=Filename;
          end;
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
    end;
end;

procedure GatherUnitsInFPMSources(Units: TStringToStringTree; out
  UnitToFPM: TStringToPointerTree; out FPMNameToFPM: TStringToPointerTree;
  const OnProgress: TDefinePoolProgress);
{ check for each UnitPath of the form
    lib/fpc/<FPCVer>/units/<FPCTarget>/<name>/
  if there is lib/fpc/<FPCVer>/fpmkinst/><FPCTarget>/<name>.fpm
  and search line SourcePath=<directory>
  then search source files in this directory including subdirectories
}
  function SearchPriorPathDelim(var p: integer; const Filename: string): boolean; inline;
  begin
    repeat
      dec(p);
      if p<1 then exit(false)
    until Filename[p]=PathDelim;
    Result:=true;
  end;

var
  Abort: boolean;
  AVLNode: TAVLTreeNode;
  S2SItem: PStringToStringItem;
  CurUnitName, Filename, PkgName, FPMFilename, FPMSourcePath, Line: String;
  p, EndPos, FPCTargetEndPos, i, FileCount: Integer;
  sl: TStringListUTF8;
  FPM: TPCFPMFileState;
begin
  // try to resolve .ppu files via fpmkinst .fpm files
  UnitToFPM:=TStringToPointerTree.Create(false);
  FPMNameToFPM:=TStringToPointerTree.Create(false);
  FPMNameToFPM.FreeValues:=true;
  if Units=nil then exit;
  FileCount:=0;
  Abort:=false;
  AVLNode:=Units.Tree.FindLowest;
  while AVLNode<>nil do begin
    S2SItem:=PStringToStringItem(AVLNode.Data);
    CurUnitName:=S2SItem^.Name;
    Filename:=S2SItem^.Value; // trimmed and expanded filename
    //if Pos('lazmkunit',Filename)>0 then
      //debugln(['GatherUnitsInFPMSources ===== ',Filename]);
    AVLNode:=Units.Tree.FindSuccessor(AVLNode);
    if CompareFileExt(Filename,'ppu',false)<>0 then continue;
    // check if filename has the form
    //                  /something/units/<FPCTarget>/<pkgname>/<unitname>.ppu
    // and if there is  /something/fpmkinst/<FPCTarget>/<pkgname>.fpm
    p:=length(Filename);
    if not SearchPriorPathDelim(p,Filename) then exit;
    // <pkgname>
    EndPos:=p;
    if not SearchPriorPathDelim(p,Filename) then exit;
    PkgName:=copy(Filename,p+1,EndPos-p-1);
    if PkgName='' then continue;
    FPCTargetEndPos:=p;
    if not SearchPriorPathDelim(p,Filename) then exit;
    // <fpctarget>
    EndPos:=p;
    if not SearchPriorPathDelim(p,Filename) then exit;
    // 'units'
    if (EndPos-p<>6) or (CompareIdentifiers(@Filename[p+1],'units')<>0) then
      continue;
    FPMFilename:=copy(Filename,1,p)+'fpmkinst'
                +copy(Filename,EndPos,FPCTargetEndPos-EndPos+1)+PkgName+'.fpm';

    FPM:=TPCFPMFileState(FPMNameToFPM[PkgName]);
    if FPM=nil then begin
      inc(FileCount);
      if (FileCount mod 100=0) and Assigned(OnProgress) then begin
        OnProgress(nil, 0, -1, Format(ctsScannedFiles, [IntToStr(FileCount)]
          ), Abort);
        if Abort then break;
      end;
      FPMSourcePath:='';
      if FileExistsCached(FPMFilename) then begin
        //debugln(['GatherUnitsInFPMSources Found .fpm: ',FPMFilename]);
        sl:=TStringListUTF8.Create;
        try
          try
            sl.LoadFromFile(FPMFilename);
            for i:=0 to sl.Count-1 do begin
              Line:=sl[i];
              if LeftStr(Line,length('SourcePath='))='SourcePath=' then
              begin
                FPMSourcePath:=TrimAndExpandDirectory(copy(Line,length('SourcePath=')+1,length(Line)));
                break;
              end;
            end;
          except
            on E: Exception do
              debugln(['Warning: (lazarus) [GatherUnitsInFPMSources] ',E.Message]);
          end;
        finally
          sl.Free;
        end;
        FPM:=TPCFPMFileState.Create;
        FPM.Name:=PkgName;
        FPM.FPMFilename:=FPMFilename;
        FPM.SourcePath:=FPMSourcePath;
        FPMNameToFPM[PkgName]:=FPM;
        UnitToFPM[CurUnitName]:=FPM;

        if FPMSourcePath<>'' then begin
          //debugln(['GatherUnitsInFPMSources ',FPMFilename,' ',FPMSourcePath]);
          FPM.UnitToSrc:=GatherUnitSourcesInDirectory(FPMSourcePath,3);
        end;
      end;
    end;
  end;
end;

function GatherUnitSourcesInDirectory(Directory: string; MaxLevel: integer
  ): TStringToStringTree;

  procedure Traverse(Dir: string; Tree: TStringToStringTree; Lvl: integer);
  var
    Info: TSearchRec;
    Filename: String;
    AnUnitName: String;
  begin
    if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,Info)=0 then begin
      repeat
        Filename:=Info.Name;
        if (Filename='') or (Filename='.') or (Filename='..') then continue;
        if faDirectory and Info.Attr>0 then begin
          if Lvl<MaxLevel then
            Traverse(Dir+Filename+PathDelim,Tree,Lvl+1);
        end else if FilenameIsPascalUnit(Filename) then begin
          AnUnitName:=ExtractFileNameOnly(Filename);
          if not Tree.Contains(AnUnitName) then
            Tree[AnUnitName]:=Dir+Filename;
        end;
      until FindNextUTF8(Info)<>0;
    end;
    FindCloseUTF8(Info);
  end;

begin
  Result:=TStringToStringTree.Create(false);
  if MaxLevel<1 then exit;
  Directory:=AppendPathDelim(Directory);
  Traverse(Directory,Result,1);
end;

procedure AdjustFPCSrcRulesForPPUPaths(Units: TStringToStringTree;
  Rules: TFPCSourceRules);
var
  Filename: string;
  Rule: TFPCSourceRule;
begin
  if Units.CaseSensitive then
    raise Exception.Create('AdjustFPCSrcRulesForPPUPaths Units is case sensitive');
  // check unit httpd
  Filename:=Units['httpd'];
  if Filename<>'' then begin
    Filename:=ChompPathDelim(ExtractFilePath(Filename));
    Rule:=Rules.Add('packages/'+ExtractFileName(Filename));
    Rule.Score:=10;
    Rule.Targets:='*';
    //DebugLn(['AdjustFPCSrcRulesForPPUPaths ',Rule.Filename,' ',Filename]);
  end;
end;

function GatherUnitsInFPCSources(Files: TStringList; TargetOS: string;
  TargetCPU: string; Duplicates: TStringToStringTree;
  Rules: TFPCSourceRules; const DebugUnitName: string): TStringToStringTree;
{ returns tree unit name to file name (maybe relative)
}

  function CountMatches(Targets, aTxt: PChar): integer;
  // check how many of the comma separated words in Targets are in words of aTxt
  var
    TxtStartPos: PChar;
    TargetPos: PChar;
    TxtPos: PChar;
  begin
    Result:=0;
    if (aTxt=nil) or (Targets=nil) then exit;
    TxtStartPos:=aTxt;
    while true do begin
      while (not (IsIdentChar[TxtStartPos^])) do begin
        if TxtStartPos^=#0 then exit;
        inc(TxtStartPos);
      end;
      //DebugLn(['CountMatches TxtStartPos=',TxtStartPos]);
      TargetPos:=Targets;
      repeat
        while (TargetPos^=',') do inc(TargetPos);
        if TargetPos^=#0 then break;
        //DebugLn(['CountMatches TargetPos=',TargetPos]);
        TxtPos:=TxtStartPos;
        while (TxtPos^=TargetPos^) and (not (TargetPos^ in [#0,','])) do begin
          inc(TargetPos);
          inc(TxtPos);
        end;
        //DebugLn(['CountMatches Test TargetPos=',TargetPos,' TxtPos=',TxtPos]);
        if (TargetPos^ in [#0,',']) and (not IsIdentChar[TxtPos^]) then begin
          // the target fits
          //DebugLn(['CountMatches FITS']);
          inc(Result);
        end;
        // try next target
        while not (TargetPos^ in [#0,',']) do inc(TargetPos);
      until TargetPos^=#0;
      // next txt word
      while IsIdentChar[TxtStartPos^] do inc(TxtStartPos);
    end;
  end;

var
  i: Integer;
  Filename: string;
  Links: TAVLTree;
  Unit_Name: String;
  LastDirectory: String;
  LastDirScore: Integer;
  Directory: String;
  DirScore: LongInt;
  Node: TAVLTreeNode;
  Link: TUnitNameLink;
  TargetRules: TAVLTree;
  Score: LongInt;
  Targets: string;
begin
  Result:=nil;
  if (Files=nil) or (Files.Count=0) then exit;

  if (Duplicates<>nil) and Duplicates.CaseSensitive then
    raise Exception.Create('GatherUnitsInFPCSources: Duplicates case sensitive');

  // get default targets
  if Rules=nil then Rules:=DefaultFPCSourceRules;
  Targets:=Rules.GetDefaultTargets(TargetOS,TargetCPU);

  TargetRules:=nil;
  Links:=TAVLTree.Create(@CompareUnitNameLinks);
  try
    // get Score rules for duplicate units
    Rules.GetRulesForTargets(Targets,TargetRules);
    //DebugLn(['GatherUnitsInFPCSources ',Rules.GetScore('packages/h',TargetRules)]);
    //exit;

    if (TargetRules<>nil) and (TargetRules.Count=0) then
      FreeAndNil(TargetRules);
    LastDirectory:='';
    LastDirScore:=0;
    for i:=0 to Files.Count-1 do begin
      Filename:=Files[i];
      if (CompareFileExt(Filename,'PAS',false)=0)
      or (CompareFileExt(Filename,'PP',false)=0)
      or (CompareFileExt(Filename,'P',false)=0)
      then begin
        if CompareFilenameOnly(PChar(Filename),length(Filename),'fpmake',6,true)=0
        then
          continue; // skip the fpmake.pp files
        // Filename is a pascal unit source
        Directory:=ExtractFilePath(Filename);
        if LastDirectory=Directory then begin
          // same directory => reuse directory Score
          DirScore:=LastDirScore;
        end else begin
          // a new directory => recompute directory score
          // default heuristic: add one point for every target in directory
          DirScore:=CountMatches(PChar(Targets),PChar(Directory));
        end;
        Score:=DirScore;
        // apply target rules
        if TargetRules<>nil then
          inc(Score,Rules.GetScore(Filename,TargetRules));
        // add or update unitlink
        Unit_Name:=ExtractFileNameOnly(Filename);
        Node:=Links.FindKey(Pointer(Unit_Name),@CompareUnitNameWithUnitNameLink);
        if Node<>nil then begin
          // duplicate unit
          Link:=TUnitNameLink(Node.Data);
          if Link.Score<Score then begin
            // found a better unit
            if (DebugUnitName<>'') and (SysUtils.CompareText(Unit_Name,DebugUnitName)=0)
            then
              debugln(['GatherUnitsInFPCSources UnitName=',Unit_Name,' File=',Filename,' Score=',Score,' => better than ',Link.Score]);
            Link.Unit_Name:=Unit_Name;
            Link.Filename:=Filename;
            Link.ConflictFilename:='';
            Link.Score:=Score;
          end else if Link.Score=Score then begin
            // unit with same Score => maybe a conflict
            // be deterministic and choose the highest
            if (DebugUnitName<>'') and (SysUtils.CompareText(Unit_Name,DebugUnitName)=0)
            then
              debugln(['GatherUnitsInFPCSources UnitName=',Unit_Name,' File=',Filename,' Score=',Score,' => duplicate']);
            if CompareStr(Filename,Link.Filename)>0 then begin
              if Link.ConflictFilename<>'' then
                Link.ConflictFilename:=Link.ConflictFilename+';'+Link.Filename
              else
                Link.ConflictFilename:=Link.Filename;
              Link.Filename:=Filename;
            end else begin
              Link.ConflictFilename:=Link.ConflictFilename+';'+Filename;
            end;
          end;
        end else begin
          // new unit source found => add to list
          if (DebugUnitName<>'') and (SysUtils.CompareText(Unit_Name,DebugUnitName)=0)
          then
            debugln(['GatherUnitsInFPCSources UnitName=',Unit_Name,' File=',Filename,' Score=',Score]);
          Link:=TUnitNameLink.Create;
          Link.Unit_Name:=Unit_Name;
          Link.Filename:=Filename;
          Link.Score:=Score;
          Links.Add(Link);
        end;
        LastDirectory:=Directory;
        LastDirScore:=DirScore;
      end;
    end;
    Result:=TStringToStringTree.Create(false);
    Node:=Links.FindLowest;
    while Node<>Nil do begin
      Link:=TUnitNameLink(Node.Data);
      Result[Link.Unit_Name]:=Link.Filename;
      if (Link.ConflictFilename<>'') and (Link.Score>0) then begin
        if (DebugUnitName<>'') and (SysUtils.CompareText(Link.Unit_Name,DebugUnitName)=0)
        then
          DebugLn(['GatherUnitsInFPCSources Ambiguous: ',Link.Score,' ',Link.Filename,' ',Link.ConflictFilename]);
        if Duplicates<>nil then
          Duplicates[Link.Unit_Name]:=Link.Filename+';'+Link.ConflictFilename;
      end;
      Node:=Links.FindSuccessor(Node);
    end;
  finally
    TargetRules.Free;
    Links.FreeAndClear;
    Links.Free;
  end;
end;

function CreateFPCTemplate(Config: TPCTargetConfigCache; Owner: TObject): TDefineTemplate;
var
  Node: TAVLTreeNode;
  StrItem: PStringToStringItem;
  NewDefTempl: TDefineTemplate;
  TargetOS: String;
  SrcOS: String;
  SrcOS2: String;
  TargetCPU: String;
begin
  Result:=TDefineTemplate.Create(StdDefTemplFPC,
    ctsFreePascalCompilerInitialMacros,'','',da_Block);

  // define #TargetOS
  TargetOS:=Config.RealTargetOS;
  if TargetOS='' then
    TargetOS:=Config.TargetOS;
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  NewDefTempl:=TDefineTemplate.Create('Define TargetOS',
    ctsDefaultFPCTargetOperatingSystem,
    ExternalMacroStart+'TargetOS',TargetOS,da_DefineRecurse);
  Result.AddChild(NewDefTempl);
  // define #SrcOS
  SrcOS:=GetDefaultSrcOSForTargetOS(TargetOS);
  if SrcOS='' then SrcOS:=TargetOS;
  NewDefTempl:=TDefineTemplate.Create('Define SrcOS',
    ctsDefaultFPCSourceOperatingSystem,
    ExternalMacroStart+'SrcOS',SrcOS,da_DefineRecurse);
  Result.AddChild(NewDefTempl);
  // define #SrcOS2
  SrcOS2:=GetDefaultSrcOS2ForTargetOS(TargetOS);
  if SrcOS2='' then SrcOS2:=TargetOS;
  NewDefTempl:=TDefineTemplate.Create('Define SrcOS2',
    ctsDefaultFPCSource2OperatingSystem,
    ExternalMacroStart+'SrcOS2',SrcOS2,da_DefineRecurse);
  Result.AddChild(NewDefTempl);
  // define #TargetCPU
  TargetCPU:=Config.RealTargetCPU;
  if TargetCPU='' then
    TargetCPU:=Config.TargetCPU;
  if TargetCPU='' then
    TargetCPU:=GetCompiledTargetCPU;
  NewDefTempl:=TDefineTemplate.Create('Define TargetCPU',
    ctsDefaultFPCTargetProcessor,
    TargetCPUMacroName,TargetCPU,
    da_DefineRecurse);
  Result.AddChild(NewDefTempl);

  if Config.Defines<>nil then begin
    Node:=Config.Defines.Tree.FindLowest;
    while Node<>nil do begin
      StrItem:=PStringToStringItem(Node.Data);
      NewDefTempl:=TDefineTemplate.Create('Define '+StrItem^.Name,
           'Macro',StrItem^.Name,StrItem^.Value,da_DefineRecurse);
      Result.AddChild(NewDefTempl);
      Node:=Config.Defines.Tree.FindSuccessor(Node);
    end;
  end;

  if Config.Undefines<>nil then begin
    Node:=Config.Undefines.Tree.FindLowest;
    while Node<>nil do begin
      StrItem:=PStringToStringItem(Node.Data);
      NewDefTempl:=TDefineTemplate.Create('Undefine '+StrItem^.Name,
           'Macro',StrItem^.Name,'',da_UndefineRecurse);
      Result.AddChild(NewDefTempl);
      Node:=Config.Undefines.Tree.FindSuccessor(Node);
    end;
  end;

  Result.SetFlags([dtfAutoGenerated],[],false);
  Result.SetDefineOwner(Owner,true);
end;

function CreateFPCTemplate(Config: TFPCUnitSetCache; Owner: TObject): TDefineTemplate; overload;
begin
  Result:=CreateFPCTemplate(Config.GetConfigCache(false),Owner);
  Result.AddChild(TDefineTemplate.Create('UnitSet','UnitSet identifier',
                  UnitSetMacroName,Config.GetUnitSetID,da_DefineRecurse));
end;

function CreateFPCSourceTemplate(FPCSrcDir: string; Owner: TObject): TDefineTemplate;
const
  RTLPkgDirs: array[1..4] of string = ('rtl-console','rtl-extra','rtl-objpas','rtl-unicode');
var
  Dir, SrcOS, SrcOS2, aTargetCPU,
  IncPathMacro: string;
  DS: char; // dir separator

  function d(const Filenames: string): string;
  begin
    Result:=GetForcedPathDelims(Filenames);
  end;

  procedure AddSrcOSDefines(ParentDefTempl: TDefineTemplate);
  var
    IfTargetOSIsNotSrcOS: TDefineTemplate;
    RTLSrcOSDir: TDefineTemplate;
    IfTargetOSIsNotSrcOS2: TDefineTemplate;
    RTLSrcOS2Dir: TDefineTemplate;
  begin
    // if TargetOS<>SrcOS
    IfTargetOSIsNotSrcOS:=TDefineTemplate.Create(
      'IF TargetOS<>SrcOS',
      ctsIfTargetOSIsNotSrcOS,'',''''+TargetOSMacro+'''<>'''+SrcOS+'''',da_If);
    // rtl/$(#SrcOS)
    RTLSrcOSDir:=TDefineTemplate.Create('SrcOS',SrcOS,'',
      SrcOS,da_Directory);
    IfTargetOSIsNotSrcOS.AddChild(RTLSrcOSDir);
    // add include path inc
    RTLSrcOSDir.AddChild(TDefineTemplate.Create('Include Path',
      'include path',
      IncludePathMacroName,IncPathMacro+';inc',
      da_Define));
    // add include path $(TargetOS)
    RTLSrcOSDir.AddChild(TDefineTemplate.Create('Include Path',
      'include path to TargetCPU directories',
      IncludePathMacroName,IncPathMacro+';'+aTargetCPU,
      da_Define));
    ParentDefTempl.AddChild(IfTargetOSIsNotSrcOS);

    // if TargetOS<>SrcOS2
    IfTargetOSIsNotSrcOS2:=TDefineTemplate.Create(
      'IF TargetOS is not SrcOS2',
      ctsIfTargetOSIsNotSrcOS,'',''''+TargetOSMacro+'''<>'''+SrcOS2+'''',da_If);
    // rtl/$(#SrcOS2)
    RTLSrcOS2Dir:=TDefineTemplate.Create('SrcOS2',SrcOS2,'',
      SrcOS2,da_Directory);
    IfTargetOSIsNotSrcOS2.AddChild(RTLSrcOS2Dir);
    RTLSrcOS2Dir.AddChild(TDefineTemplate.Create('Include Path',
      'include path to TargetCPU directories',
      IncludePathMacroName,IncPathMacro+';'+aTargetCPU,
      da_DefineRecurse));
    ParentDefTempl.AddChild(IfTargetOSIsNotSrcOS2);
  end;

var
  DefTempl, MainDir, FCLDir, RTLDir, RTLOSDir, PackagesDir, CompilerDir,
  UtilsDir, DebugSvrDir: TDefineTemplate;
  s: string;
  FCLDBDir: TDefineTemplate;
  FCLDBInterbaseDir: TDefineTemplate;
  InstallerDir: TDefineTemplate;
  IFTempl: TDefineTemplate;
  FCLBaseDir: TDefineTemplate;
  FCLBaseSrcDir: TDefineTemplate;
  PackagesFCLAsyncDir: TDefineTemplate;
  PackagesExtraDir: TDefineTemplate;
  PackagesFCLExtraDir: TDefineTemplate;
  PackagesSubDir: TDefineTemplate;
  PkgExtraGraphDir: TDefineTemplate;
  PkgExtraAMunitsDir: TDefineTemplate;
  FCLSubSrcDir: TDefineTemplate;
  FCLSubDir, SubPkgDir: TDefineTemplate;
  Ok: Boolean;
begin
  {$IFDEF VerboseFPCSrcScan}
  DebugLn('CreateFPCSrcTemplate FPCSrcDir="',FPCSrcDir,'"');
  {$ENDIF}
  Result:=nil;
  Ok:=false;
  try
    if (FPCSrcDir='') or (not DirPathExists(FPCSrcDir)) then begin
      DebugLn(['Warning: [CreateFPCSrcTemplate] FPCSrcDir does not exist: FPCSrcDir="',FPCSrcDir,'"']);
      exit;
    end;
    DS:=PathDelim;
    Dir:=AppendPathDelim(FPCSrcDir);
    SrcOS:='$('+ExternalMacroStart+'SrcOS)';
    SrcOS2:='$('+ExternalMacroStart+'SrcOS2)';
    aTargetCPU:=TargetCPUMacro;
    IncPathMacro:=IncludePathMacro;

    Result:=TDefineTemplate.Create(StdDefTemplFPCSrc,
       Format(ctsFreePascalSourcesPlusDesc,['RTL, FCL, Packages, Compiler']),
       '','',da_Block);

    // The Free Pascal sources build a world of their own
    // => reset all search paths
    MainDir:=TDefineTemplate.Create('Free Pascal Source Directory',
      ctsFreePascalSourceDir,'',FPCSrcDir,da_Directory);
    Result.AddChild(MainDir);
    DefTempl:=TDefineTemplate.Create('Reset SrcPath',
      ctsSrcPathInitialization,ExternalMacroStart+'SrcPath','',da_DefineRecurse);
    MainDir.AddChild(DefTempl);
    DefTempl:=TDefineTemplate.Create('Reset UnitPath',
      ctsUnitPathInitialization,UnitPathMacroName,'',da_DefineRecurse);
    MainDir.AddChild(DefTempl);
    DefTempl:=TDefineTemplate.Create('Reset IncPath',
      ctsUnitPathInitialization,IncludePathMacroName,'',da_DefineRecurse);
    MainDir.AddChild(DefTempl);

    // rtl
    RTLDir:=TDefineTemplate.Create('RTL',ctsRuntimeLibrary,'','rtl',da_Directory);
    MainDir.AddChild(RTLDir);

    // rtl include paths
    s:=IncPathMacro
      +';'+Dir+'rtl'+DS+'objpas'+DS
      +';'+Dir+'rtl'+DS+'objpas'+DS+'sysutils'
      +';'+Dir+'rtl'+DS+'objpas'+DS+'classes'
      +';'+Dir+'rtl'+DS+'inc'+DS
      +';'+Dir+'rtl'+DS+'inc'+DS+'graph'+DS
      +';'+Dir+'rtl'+DS+SrcOS+DS
      +';'+Dir+'rtl'+DS+TargetOSMacro+DS
      +';'+Dir+'rtl'+DS+SrcOS2+DS
      +';'+Dir+'rtl'+DS+SrcOS2+DS+aTargetCPU
      +';'+Dir+'rtl'+DS+aTargetCPU+DS
      +';'+Dir+'rtl'+DS+TargetOSMacro+DS+aTargetCPU+DS;
    RTLDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,
      ['objpas, inc,'+aTargetCPU+','+SrcOS]),
      IncludePathMacroName,s,da_DefineRecurse));

    // if solaris or darwin or beos then define FPC_USE_LIBC
    IFTempl:=TDefineTemplate.Create('IF darwin or solaris or beos',
      'If Darwin or Solaris or Beos', '', 'defined(darwin) or defined(solaris) or defined(beos)', da_If);
      // then define FPC_USE_LIBC
      IFTempl.AddChild(TDefineTemplate.Create('define FPC_USE_LIBC',
        'define FPC_USE_LIBC','FPC_USE_LIBC','',da_DefineRecurse));
    RTLDir.AddChild(IFTempl);

    // rtl: IF SrcOS=win then add include path rtl/TargetOS/wininc;rtl/win/wininc;rtl/win
    IFTempl:=TDefineTemplate.Create('If SrcOS=win','If SrcOS=win',
      '',''''+SrcOS+'''=''win''',da_If);
    IFTempl.AddChild(TDefineTemplate.Create('Include Path',
        Format(ctsIncludeDirectoriesPlusDirs,['wininc']),
        IncludePathMacroName,
        IncPathMacro
        +';'+Dir+'rtl'+DS+TargetOSMacro+DS+'wininc'
        +';'+Dir+'rtl'+DS+'win'+DS+'wininc'
        +';'+Dir+'rtl'+DS+'win',
        da_DefineRecurse));
    RTLDir.AddChild(IFTempl);

    // rtl: IF TargetOS=iphonesim then add include path rtl/unix, rtl/bsd, rtl/darwin
    IFTempl:=TDefineTemplate.Create('If TargetOS=iphonesim','If TargetOS=iphonesim',
      '',''''+TargetOSMacro+'''=''iphonesim''',da_If);
    IFTempl.AddChild(TDefineTemplate.Create('Include Path',
        Format(ctsIncludeDirectoriesPlusDirs,['unix,bsd,darwin']),
        IncludePathMacroName,
        IncPathMacro
        +';'+Dir+'rtl'+DS+'unix'
        +';'+Dir+'rtl'+DS+'bsd'
        +';'+Dir+'rtl'+DS+'darwin',
        da_DefineRecurse));
    RTLDir.AddChild(IFTempl);

    // add processor and SrcOS alias defines for the RTL
    AddSrcOSDefines(RTLDir);

    // rtl/$(#TargetOS)
    RTLOSDir:=TDefineTemplate.Create('TargetOS','Target OS','',
                                     TargetOSMacro,da_Directory);
    s:=IncPathMacro
      +';'+Dir+'rtl'+DS+TargetOSMacro+DS+SrcOS+'inc' // e.g. rtl/win32/inc/
      +';'+Dir+'rtl'+DS+TargetOSMacro+DS+aTargetCPU+DS
      ;
    RTLOSDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,[aTargetCPU]),
      IncludePathMacroName,
      s,da_DefineRecurse));
    s:=SrcPathMacro
      +';'+Dir+'rtl'+DS+'objpas'+DS;
    RTLOSDir.AddChild(TDefineTemplate.Create('Src Path',
      Format(ctsAddsDirToSourcePath,[aTargetCPU]),
      ExternalMacroStart+'SrcPath',s,da_DefineRecurse));
    RTLDir.AddChild(RTLOSDir);

    // fcl
    FCLDir:=TDefineTemplate.Create('FCL',ctsFreePascalComponentLibrary,'','fcl',
        da_Directory);
    MainDir.AddChild(FCLDir);
    FCLDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['inc,'+SrcOS]),
      IncludePathMacroName,
      d(   DefinePathMacro+'/inc/'
      +';'+DefinePathMacro+'/classes/'
      +';'+DefinePathMacro+'/'+TargetOSMacro+DS // TargetOS before SrcOS !
      +';'+DefinePathMacro+'/'+SrcOS+DS
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // fcl/db
    FCLDBDir:=TDefineTemplate.Create('DB','DB','','db',da_Directory);
    FCLDir.AddChild(FCLDBDir);
    FCLDBInterbaseDir:=TDefineTemplate.Create('interbase','interbase','',
      'interbase',da_Directory);
    FCLDBDir.AddChild(FCLDBInterbaseDir);
    FCLDBInterbaseDir.AddChild(TDefineTemplate.Create('SrcPath',
      'SrcPath addition',
      ExternalMacroStart+'SrcPath',
      d(Dir+'/packages/base/ibase;'+SrcPathMacro)
      ,da_Define));

    // packages
    PackagesDir:=TDefineTemplate.Create('Packages',ctsPackageDirectories,'',
       'packages',da_Directory);
    MainDir.AddChild(PackagesDir);

    // packages/rtl-*
    for s in RTLPkgDirs do begin
      SubPkgDir:=TDefineTemplate.Create(s,s,'',s,da_Directory);
      PackagesDir.AddChild(SubPkgDir);
      SubPkgDir.AddChild(TDefineTemplate.Create('Include Path',
        Format(ctsIncludeDirectoriesPlusDirs,['inc']),
        IncludePathMacroName,
        d(DefinePathMacro+'/inc'),da_DefineRecurse));
    end;

    // packages/rtl-console
    PackagesSubDir:=TDefineTemplate.Create('rtl-console','rtl-console','','rtl-console',da_Directory);
    PackagesDir.AddChild(PackagesSubDir);
    PackagesSubDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/rtl-console/src/inc']),
      IncludePathMacroName,
      d(DefinePathMacro+'/src/inc;'
       +IncPathMacro)
      ,da_DefineRecurse));

    // packages/rtl-extra
    PackagesSubDir:=TDefineTemplate.Create('rtl-extra','rtl-extra','','rtl-extra',da_Directory);
    PackagesDir.AddChild(PackagesSubDir);
    PackagesSubDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/rtl-extra/src/OS']),
      IncludePathMacroName,
      d(DefinePathMacro+'/src/inc;'
       +DefinePathMacro+'/src/'+TargetOSMacro+';'
       +DefinePathMacro+'/src/'+SrcOS+';'
       +DefinePathMacro+'/src/'+SrcOS2+';'
       +IncPathMacro)
      ,da_DefineRecurse));

    // packages/rtl-objpas
    PackagesSubDir:=TDefineTemplate.Create('rtl-objpas','rtl-objpas','','rtl-objpas',da_Directory);
    PackagesDir.AddChild(PackagesSubDir);
    PackagesSubDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/rtl-objpas/src/inc']),
      IncludePathMacroName,
      d(DefinePathMacro+'/src/inc;'
       +IncPathMacro)
      ,da_DefineRecurse));

    // packages/fcl-base
    FCLBaseDir:=TDefineTemplate.Create('FCL-base',
        ctsFreePascalComponentLibrary,'','fcl-base',
        da_Directory);
    PackagesDir.AddChild(FCLBaseDir);
    // packages/fcl-base/src
    FCLBaseSrcDir:=TDefineTemplate.Create('src',
        'src','','src',
        da_Directory);
    FCLBaseDir.AddChild(FCLBaseSrcDir);
    FCLBaseSrcDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['inc,'+SrcOS]),
      IncludePathMacroName,
      d(   DefinePathMacro+'/inc/'
      +';'+DefinePathMacro+'/'+TargetOSMacro+DS // TargetOS before SrcOS !
      +';'+DefinePathMacro+'/'+SrcOS+DS
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // packages/fcl-process
    FCLSubDir:=TDefineTemplate.Create('FCL-process',
        'fcl-process','','fcl-process',
        da_Directory);
    PackagesDir.AddChild(FCLSubDir);
    // packages/fcl-process/src
    FCLSubSrcDir:=TDefineTemplate.Create('src',
        'src','','src',
        da_Directory);
    FCLSubDir.AddChild(FCLSubSrcDir);
    FCLSubSrcDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['inc,'+SrcOS]),
      IncludePathMacroName,
      d(   DefinePathMacro+'/'+TargetOSMacro+DS // TargetOS before SrcOS !
      +';'+DefinePathMacro+'/'+SrcOS+DS
      +';'+IncPathMacro)
      ,da_DefineRecurse));
    // packages/fcl-process/src
    //   IF SrcOS=win then add include path winall
    IFTempl:=TDefineTemplate.Create('If SrcOS=win','If SrcOS=win',
      '',''''+SrcOS+'''=''win''',da_If);
    IFTempl.AddChild(TDefineTemplate.Create('Include Path',
        Format(ctsIncludeDirectoriesPlusDirs,['winall']),
        IncludePathMacroName,
        IncPathMacro
        +';winall',
        da_DefineRecurse));
    FCLSubDir.AddChild(IFTempl);

    // packages/fcl-async
    PackagesFCLAsyncDir:=TDefineTemplate.Create('fcl-async','fcl-async','','fcl-async',da_Directory);
    PackagesDir.AddChild(PackagesFCLAsyncDir);

    // packages/fcl-async/src
    PackagesFCLAsyncDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/fcl-async/src']),
      IncludePathMacroName,
      d(   DefinePathMacro+'/src/'
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // packages/fcl-extra
    PackagesFCLExtraDir:=TDefineTemplate.Create('fcl-extra','fcl-extra','','fcl-extra',da_Directory);
    PackagesDir.AddChild(PackagesFCLExtraDir);

    // packages/fcl-extra/src
    PackagesFCLExtraDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/fcl-extra/src']),
      IncludePathMacroName,
      d(   DefinePathMacro+'/src/'+SrcOS
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // packages/extra
    PackagesExtraDir:=TDefineTemplate.Create('extra','extra','','extra',da_Directory);
    PackagesDir.AddChild(PackagesExtraDir);

    // packages/extra/graph
    PkgExtraGraphDir:=TDefineTemplate.Create('graph','graph','','graph',
                                             da_Directory);
    PackagesExtraDir.AddChild(PkgExtraGraphDir);
    PkgExtraGraphDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['inc']),
      IncludePathMacroName,
      d(   DefinePathMacro+'/inc/'
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // packages/extra/amunits
    PkgExtraAMunitsDir:=TDefineTemplate.Create('amunits','amunits','','amunits',
                                             da_Directory);
    PackagesExtraDir.AddChild(PkgExtraAMunitsDir);
    PkgExtraAMunitsDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['inc']),
      IncludePathMacroName,
      d(   DefinePathMacro+'/inc/'
      +';'+IncPathMacro)
      ,da_DefineRecurse));

    // packages/graph
    PackagesSubDir:=TDefineTemplate.Create('graph','graph','','graph',da_Directory);
    PackagesDir.AddChild(PackagesSubDir);
    PackagesSubDir.AddChild(TDefineTemplate.Create('Include Path',
      Format(ctsIncludeDirectoriesPlusDirs,['packages/graph/src/inc']),
      IncludePathMacroName,
      d(DefinePathMacro+'/src/inc;'
       +IncPathMacro)
      ,da_DefineRecurse));

    // utils
    UtilsDir:=TDefineTemplate.Create('Utils',ctsUtilsDirectories,'',
       'utils',da_Directory);
    MainDir.AddChild(UtilsDir);

    // utils/debugsvr
    DebugSvrDir:=TDefineTemplate.Create('DebugSvr','Debug Server','',
       'debugsvr',da_Directory);
    UtilsDir.AddChild(DebugSvrDir);
    DebugSvrDir.AddChild(TDefineTemplate.Create('Interface Path',
      Format(ctsAddsDirToSourcePath,['..']),ExternalMacroStart+'SrcPath',
      '..;'+ExternalMacroStart+'SrcPath',da_DefineRecurse));

    // installer
    InstallerDir:=TDefineTemplate.Create('Installer',ctsInstallerDirectories,'',
       'installer',da_Directory);
    InstallerDir.AddChild(TDefineTemplate.Create('SrcPath','SrcPath addition',
      ExternalMacroStart+'SrcPath',
      SrcPathMacro+';'+Dir+'ide;'+Dir+'fv',da_Define));
    MainDir.AddChild(InstallerDir);

    // compiler
    CompilerDir:=TDefineTemplate.Create('Compiler',ctsCompiler,'','compiler',
       da_Directory);
    CompilerDir.AddChild(TDefineTemplate.Create('SrcPath','SrcPath addition',
      ExternalMacroStart+'SrcPath',
      SrcPathMacro+';'+Dir+aTargetCPU,da_Define));
    CompilerDir.AddChild(TDefineTemplate.Create('IncPath','IncPath addition',
      IncludePathMacroName,
      IncPathMacro+';'+Dir+'compiler',da_DefineRecurse));
    MainDir.AddChild(CompilerDir);

    // compiler/utils
    UtilsDir:=TDefineTemplate.Create('utils',ctsUtilsDirectories,'',
       'utils',da_Directory);
    UtilsDir.AddChild(TDefineTemplate.Create('SrcPath','SrcPath addition',
      ExternalMacroStart+'SrcPath',
      SrcPathMacro+';..',da_Define));
    CompilerDir.AddChild(UtilsDir);

    Result.SetDefineOwner(Owner,true);
    Result.SetFlags([dtfAutoGenerated],[],false);

    Ok:=true;
  finally
    if not ok then
      FreeAndNil(Result);
  end;
end;

function CreateFPCSourceTemplate(Config: TFPCUnitSetCache; Owner: TObject
  ): TDefineTemplate;
begin
  Result:=CreateFPCSourceTemplate(Config.FPCSourceDirectory,Owner);
end;

procedure CheckPPUSources(PPUFiles, UnitToSource,
  UnitToDuplicates: TStringToStringTree;
  var Duplicates, Missing: TStringToStringTree);
var
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  Unit_Name: String;
  Filename: String;
  SrcFilename: string;
  DuplicateFilenames: string;
begin
  if PPUFiles.CaseSensitive then
    raise Exception.Create('CheckPPUSources PPUFiles is case sensitive');
  if UnitToSource.CaseSensitive then
    raise Exception.Create('CheckPPUSources UnitToSource is case sensitive');
  if UnitToDuplicates.CaseSensitive then
    raise Exception.Create('CheckPPUSources UnitToDuplicates is case sensitive');
  if (Duplicates<>nil) and Duplicates.CaseSensitive then
    raise Exception.Create('CheckPPUSources Duplicates is case sensitive');
  if (Missing<>nil) and Missing.CaseSensitive then
    raise Exception.Create('CheckPPUSources Missing is case sensitive');
  Node:=PPUFiles.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Unit_Name:=Item^.Name;
    Filename:=Item^.Value;
    if CompareFileExt(Filename,'.ppu',false)=0 then begin
      SrcFilename:=UnitToSource[Unit_Name];
      if SrcFilename<>'' then begin
        DuplicateFilenames:=UnitToDuplicates[Unit_Name];
        if (DuplicateFilenames<>'') and (Duplicates<>nil) then
          Duplicates[Unit_Name]:=DuplicateFilenames;
      end else begin
        if Missing<>nil then
          Missing[Unit_Name]:=Filename;
      end;
    end;
    Node:=PPUFiles.Tree.FindSuccessor(Node);
  end;
end;

procedure LoadFPCCacheFromFile(Filename: string;
  var Configs: TPCTargetConfigCaches; var Sources: TFPCSourceCaches);
var
  XMLConfig: TXMLConfig;
begin
  if Configs=nil then Configs:=TPCTargetConfigCaches.Create(nil);
  if Sources=nil then Sources:=TFPCSourceCaches.Create(nil);
  if not FileExistsUTF8(Filename) then exit;
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    Configs.LoadFromXMLConfig(XMLConfig,'FPCConfigs/');
    Sources.LoadFromXMLConfig(XMLConfig,'FPCSourceDirectories/');
  finally
    XMLConfig.Free;
  end;
end;

procedure SaveFPCCacheToFile(Filename: string; Configs: TPCTargetConfigCaches;
  Sources: TFPCSourceCaches);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    Configs.SaveToXMLConfig(XMLConfig,'FPCConfigs/');
    Sources.SaveToXMLConfig(XMLConfig,'FPCSourceDirectories/');
  finally
    XMLConfig.Free;
  end;
end;

{ TPCFPMFileState }

constructor TPCFPMFileState.Create;
begin
  UnitToSrc:=TStringToStringTree.Create(false);
end;

destructor TPCFPMFileState.Destroy;
begin
  FreeAndNil(UnitToSrc);
  inherited Destroy;
end;

procedure TPCFPMFileState.Clear;
begin
  UnitToSrc.Clear;
  FileDate:=-1;
end;

procedure TPCFPMFileState.Assign(List: TPCFPMFileState);
begin
  // do not assign Name
  FPMFilename:=List.FPMFilename;
  FileDate:=List.FileDate;
  SourcePath:=List.SourcePath;
  UnitToSrc.Assign(List.UnitToSrc);
end;

function TPCFPMFileState.Equals(List: TPCFPMFileState; CheckDates: boolean
  ): boolean;
begin
  Result:=false;
  if Name<>List.Name then exit;
  if FPMFilename<>List.FPMFilename then exit;
  if CheckDates and (FileDate<>List.FileDate) then exit;
  if SourcePath<>List.SourcePath then exit;
  if not UnitToSrc.Equals(List.UnitToSrc) then exit;
  Result:=true;
end;

procedure TPCFPMFileState.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt, i: Integer;
  SubPath, CurName, CurFilename: String;
begin
  // do not read Name
  FPMFilename:=XMLConfig.GetValue(Path+'FPMFile','');
  FileDate:=XMLConfig.GetValue(Path+'FileDate',0);
  SourcePath:=TrimAndExpandDirectory(XMLConfig.GetValue(Path+'SourcePath',''));
  UnitToSrc.Clear;
  Cnt:=XMLConfig.GetValue(Path+'Units/Count',0);
  for i:=1 to Cnt do begin
    SubPath:=Path+'Units/Item'+IntToStr(i)+'/';
    CurName:=XMLConfig.GetValue(SubPath+'Name','');
    CurFilename:=XMLConfig.GetValue(SubPath+'File','');
    UnitToSrc[CurName]:=SourcePath+CurFilename;
  end;
end;

procedure TPCFPMFileState.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Node: TAVLTreeNode;
  S2PItem: PStringToStringItem;
  i: Integer;
  SubPath: String;
begin
  XMLConfig.SetDeleteValue(Path+'Name',Name,'');
  XMLConfig.SetDeleteValue(Path+'File',FPMFilename,'');
  XMLConfig.SetDeleteValue(Path+'FileDate',FileDate,0);
  XMLConfig.SetDeleteValue(Path+'SourcePath',SourcePath,'');
  XMLConfig.SetDeleteValue(Path+'Units/Count',UnitToSrc.Count,0);
  i:=0;
  Node:=UnitToSrc.Tree.FindLowest;
  while Node<>nil do begin
    S2PItem:=PStringToStringItem(Node.Data);
    inc(i);
    SubPath:=Path+'Units/Item'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(SubPath+'Name',S2PItem^.Name,'');
    XMLConfig.SetDeleteValue(SubPath+'File',CreateRelativePath(S2PItem^.Value,SourcePath),'');
    Node:=Node.Successor;
  end;
end;

{ TFPCParamValue }

constructor TFPCParamValue.Create(const aName, aValue: string;
  aKind: TFPCParamKind; aFlags: TFPCParamFlags);
begin
  Name:=aName;
  Value:=aValue;
  Kind:=aKind;
  Flags:=aFlags;
end;

procedure ParseFPCParameters(const CmdLineParams: string;
  Params: TObjectList; ReadBackslash: boolean);
var
  ParamList: TStringList;
begin
  ParamList:=TStringList.Create;
  try
    SplitCmdLineParams(CmdLineParams,ParamList,ReadBackslash);
    ParseFPCParameters(ParamList,Params);
  finally
    ParamList.Free;
  end;
end;

procedure ParseFPCParameters(CmdLineParams: TStrings;
  ParsedParams: TObjectList);
var
  i: Integer;
begin
  if (CmdLineParams=nil) or (CmdLineParams.Count=0) or (ParsedParams=nil) then exit;
  for i:=0 to CmdLineParams.Count-1 do
    ParseFPCParameter(CmdLineParams[i],ParsedParams);
end;

procedure ParseFPCParameter(const CmdLineParam: string;
  ParsedParams: TObjectList);
{ $DEFINE VerboseParseFPCParameter}
const
  AlphaNum = ['a'..'z','A'..'Z','0'..'9'];

  procedure Add(aName, aValue: string; aKind: TFPCParamKind; aFlags: TFPCParamFlags = []);
  var
    i: Integer;
    Param: TFPCParamValue;
  begin
    {$IFDEF VerboseParseFPCParameter}
    debugln(['ParseFPCParameter.Add Name="',aName,'" Value="',aValue,'" Kind=',dbgs(aKind),' Flags=',dbgs(aFlags)]);
    {$ENDIF}
    if (aName='O1') or (aName='O2') or (aName='O3') or (aName='O4') then begin
      aValue:=aName[2];
      aName:='O';
      aKind:=fpkValue;
    end;

    if (aKind in [fpkBoolean,fpkValue,fpkDefine]) then
      // check for duplicates
      for i:=0 to ParsedParams.Count-1 do begin
        Param:=TFPCParamValue(ParsedParams[i]);
        if aKind=fpkDefine then begin
          if CompareText(Param.Name,aName)<>0 then continue;
        end else begin
          if (Param.Name<>aName) then continue;
        end;
        if (aKind=fpkDefine) <> (Param.Kind=fpkDefine) then continue;
        // was already set
        Include(Param.Flags,fpfSetTwice);
        if (aValue<>Param.Value) or ((fpfUnset in aFlags)<>(fpfUnset in Param.Flags))
        or (aKind<>Param.Kind) then
          Include(Param.Flags,fpfValueChanged);
        Param.Kind:=aKind;
        Param.Value:=aValue;
        if fpfUnset in aFlags then
          Include(Param.Flags,fpfUnset)
        else
          Exclude(Param.Flags,fpfUnset);
        exit;
      end;
    ParsedParams.Add(TFPCParamValue.Create(aName, aValue, aKind, aFlags));

    // alias
    if aName='S2' then
      Add('M','objfpc',fpkValue,aFlags)
    else if aName='Sd' then
      Add('M','delphi',fpkValue,aFlags)
    else if aName='So' then
      Add('M','tp',fpkValue,aFlags)
    else if aName='?' then
      Add('h',aValue,aKind,aFlags);
  end;

  procedure AddBooleanFlag(var p: PChar; Len: integer; Prefix: string = '');
  var
    aName: string;
    PrefixLen: Integer;
  begin
    PrefixLen:=length(Prefix);
    SetLength(aName,PrefixLen+Len);
    if PrefixLen>0 then
      Move(Prefix[1],aName[1],PrefixLen);
    if Len>0 then
      Move(p^,aName[PrefixLen+1],Len);
    {$IFDEF VerboseParseFPCParameter}
    debugln(['ParseFPCParameter.AddBooleanFlag p="',p,'" Len=',Len,' Prefix="',Prefix,'" Name="'+aName+'"']);
    {$ENDIF}
    inc(p,Len);
    if p^='-' then begin
      Add(aName,'',fpkBoolean,[fpfUnset]);
      inc(p);
    end else begin
      Add(aName,FPCParamEnabled,fpkBoolean);
      if p^='+' then
        inc(p);
    end;
  end;

  procedure ReadSequence(p: PChar;
    const Specials: string = '');
  // e.g. -Ci-n+o   p points to the 'C'
  // Specials is a space separated list of params:
  //  SO  : a two letter option 'SO'
  //  h:  : a one letter option 'h' followed by a value
  //  ma& : a two letter option 'ma' followed by a multi value
  //  T*  : a boolean option starting with T, e.g. Tcld
  //  P=  : a one letter option 'P' followed by a name=value pair
  var
    Option, c: Char;
    Opt, Opt2, p2: PChar;
    aName: string;
  begin
    if not (p[1] in AlphaNum) then begin
      AddBooleanFlag(p,1,'');
      exit;
    end;
    Option:=p^;
    inc(p);
    repeat
      c:=p^;
      if not (c in AlphaNum) then
        break; // invalid option
      if (p[1]<>#0) and (Specials<>'') then begin
        Opt:=PChar(Specials);
        while Opt^<>#0 do begin
          while Opt^=' ' do inc(Opt);
          p2:=p;
          Opt2:=Opt;
          while (Opt2^ in AlphaNum) and (p2^=Opt2^) do begin
            inc(p2);
            inc(Opt2);
          end;
          case Opt2^ of
          ' ',#0: // boolean option
            begin
              AddBooleanFlag(p,Opt2-Opt,Option);
              break;
            end;
          ':': // option followed by value
            begin
              Add(Option+copy(Specials,Opt-PChar(Specials)+1,Opt2-Opt),p2,fpkValue);
              exit;
            end;
          '&': // option followed by multi value
            begin
              Add(Option+copy(Specials,Opt-PChar(Specials)+1,Opt2-Opt),p2,fpkMultiValue);
              exit;
            end;
          '*': // boolean option with arbitrary name
            begin
              while p2^ in AlphaNum do inc(p2);
              AddBooleanFlag(p,p2-p,Option);
              break;
            end;
          '=': // name=value
            begin
              if not (p2^ in AlphaNum) then exit;  // invalid option
              while p2^ in AlphaNum do inc(p2);
              if (p2^<>'=') then exit; // invalid option
              SetLength(aName,p2-p);
              Move(p^,aName[1],p2-p);
              inc(p2);
              Add(Option+aName,p2,fpkValue);
              exit;
            end
          else
            // mismatch -> try next special option
            Opt:=Opt2;
            while not (Opt^ in [#0,' ']) do inc(Opt);
          end;
        end;
        if Opt^<>#0 then continue;
      end;
      // default: single char flag
      AddBooleanFlag(p,1,Option);
    until false;
  end;

  procedure DisableAllFlags(const Prefix: string);
  var
    i: Integer;
    Param: TFPCParamValue;
  begin
    for i:=0 to ParsedParams.Count-1 do begin
      Param:=TFPCParamValue(ParsedParams[i]);
      if not (Param.Kind in [fpkBoolean,fpkValue,fpkMultiValue]) then continue;
      if LeftStr(Param.Name,length(Prefix))<>Prefix then continue;
      Include(Param.Flags,fpfSetTwice);
      if not (fpfUnset in Param.Flags) then
        Include(Param.Flags,fpfValueChanged);
      Param.Value:='';
      Include(Param.Flags,fpfUnset);
    end;
  end;

var
  p, p2: PChar;
begin
  {$IFDEF VerboseParseFPCParameter}
  debugln(['ParseFPCParameter "',CmdLineParam,'"']);
  {$ENDIF}
  if CmdLineParam='' then exit;
  p:=PChar(CmdLineParam);
  case p^ of
  '-': // option
    begin
      inc(p);
      case p^ of
      'a': ReadSequence(p);
      'C': ReadSequence(p,'a: c: f: F: h: p: P= s: T*');
      'd': Add(copy(CmdLineParam,3,255),'',fpkDefine);
      'D':
        begin
          inc(p);
          case p^ of
          'd','v': Add('D'+p^,PChar(@p[1]),fpkValue);
          else
            AddBooleanFlag(p,1,'D');
          end;
        end;
      'e': Add('e',PChar(@p[1]),fpkValue);
      'F':
        case p[1] of
        'a','f','i','l','N','o','u': Add('F'+p[1],PChar(@p[2]),fpkMultiValue);
        'c','C','D','e','E','L','m','M','r','R','U','W','w': Add('F'+p[1],PChar(@p[2]),fpkValue);
        else AddBooleanFlag(p,2);
        end;
      'g':
        if p[1] in [#0,'+'] then begin
          Add('g',FPCParamEnabled,fpkBoolean,[]);
        end else if p[1]='-' then begin
          DisableAllFlags('g');
          Add('g','',fpkBoolean,[fpfUnset]);
        end else begin
          inc(p);
          repeat
            case p^ of
            'o':
              begin
              Add('g'+p,FPCParamEnabled,fpkBoolean,[]);
              exit;
              end;
            'w':
              case p[1] of
              '2'..'9':
                begin
                  Add('gw',p[1],fpkValue);
                  inc(p,2);
                end;
              else
                Add('gw','2',fpkValue);
                inc(p);
              end;
            'a'..'n','p'..'v','A'..'Z','0'..'9':
              AddBooleanFlag(p,1,'g');
            else
              break;
            end;
          until false;
        end;
      'i': ReadSequence(p,'SO SP TO TP');
      'I': Add(p^,PChar(@p[1]),fpkMultiValue);
      'k': Add(p^,PChar(@p[1]),fpkMultiValue);
      'M': Add(p^,PChar(@p[1]),fpkValue);
      'N':
        case p[1] of
        'S': Add('NS',PChar(@p[2]),fpkMultiValue); // -NS namespaces
        end;
      'o': Add(p^,PChar(@p[1]),fpkValue);
      'O':
        case p[1] of
        '-': DisableAllFlags('O');
        else
          ReadSequence(p,'a= o* p: W: w:');
        end;
      'P': ; // ToDo
      'R': Add(p^,PChar(@p[1]),fpkValue);
      'S': ReadSequence(p,'e: I:');
      's': ReadSequence(p);
      'T': Add(p^,PChar(@p[1]),fpkValue);
      'u': Add(copy(CmdLineParam,3,255),'',fpkDefine,[fpfUnset]);
      'U': ReadSequence(p);
      'v': ReadSequence(p,'m&');
      'V': Add(p^,PChar(@p[1]),fpkValue);
      'W': ReadSequence(p,'B: M: P:');
      'X': ReadSequence(p,'LA LO LD M: P: r: R:');
      else
        p2:=p;
        while p2^ in AlphaNum do inc(p2);
        AddBooleanFlag(p,p2-p);
      end;
    end;
  '@': // config
    Add('',PChar(@p[1]),fpkConfig);
  else
    // filename
    Add('',p,fpkNonOption);
  end;
end;

function IndexOfFPCParamValue(ParsedParams: TObjectList; const Name: string
  ): integer;
begin
  if ParsedParams=nil then exit(-1);
  for Result:=0 to ParsedParams.Count-1 do
    if TFPCParamValue(ParsedParams[Result]).Name=Name then exit;
  Result:=-1;
end;

function GetFPCParamValue(ParsedParams: TObjectList; const Name: string
  ): TFPCParamValue;
var
  i: Integer;
begin
  i:=IndexOfFPCParamValue(ParsedParams,Name);
  if i<0 then
    Result:=nil
  else
    Result:=TFPCParamValue(ParsedParams[i]);
end;

function dbgs(k: TFPCParamKind): string;
begin
  str(k,Result);
end;

function dbgs(f: TFPCParamFlag): string;
begin
  str(f,Result);
end;

function dbgs(const Flags: TFPCParamFlags): string;
var
  f: TFPCParamFlag;
begin
  Result:='';
  for f in TFPCParamFlag do
    if f in Flags then begin
      if Result<>'' then Result+=',';
      Result+=dbgs(f);
    end;
  Result:='['+Result+']';
end;

function ExtractFPCFrontEndParameters(const CmdLineParams: string;
  const Kinds: TFPCFrontEndParams): string;
// extract the parameters for the FPC frontend tool fpc.exe
// The result is normalized:
//   - only the last value
//   - order is: -T -P -V -Xp

  procedure Add(const Name, Value: string);
  begin
    if Value='' then exit;
    if Result<>'' then Result+=' ';
    Result+='-'+Name+StrToCmdLineParam(Value);
  end;

var
  Position: Integer;
  Param, ParamT, ParamP, ParamV, ParamXp: String;
  StartPos: integer;
  p: PChar;
begin
  Result:='';
  ParamT:='';
  ParamP:='';
  ParamV:='';
  ParamXp:='';
  Position:=1;
  while ReadNextFPCParameter(CmdLineParams,Position,StartPos) do begin
    Param:=ExtractFPCParameter(CmdLineParams,StartPos);
    if Param='' then continue;
    p:=PChar(Param);
    if p^<>'-' then continue;
    case p[1] of
    'T': if fpcpT in Kinds then ParamT:=copy(Param,3,255);
    'P': if fpcpP in Kinds then ParamP:=copy(Param,3,255);
    'V': if fpcpV in Kinds then ParamV:=copy(Param,3,length(Param));
    'X':
      case p[2] of
      'p': if fpcpXp in Kinds then ParamXp:=copy(Param,4,length(Param));
      end;
    end;
  end;
  // add parameters
  Add('Xp',ParamXp);
  Add('T',ParamT);
  Add('P',ParamP);
  Add('V',ParamV);
end;

procedure ReadMakefileFPC(const Filename: string; List: TStrings);
var
  MakefileFPC: TStringListUTF8;
  i: Integer;
  Line: string;
  p: LongInt;
  NameValue: String;
begin
  MakefileFPC:=TStringListUTF8.Create;
  MakefileFPC.LoadFromFile(Filename);
  i:=0;
  while i<MakefileFPC.Count do begin
    Line:=MakefileFPC[i];
    if Line='' then begin
    end else if (Line[1]='[') then begin
      // start of section
      p:=System.Pos(']',Line);
      if p<1 then p:=length(Line);
      List.Add(Line);
    end else if (Line[1] in ['a'..'z','A'..'Z','0'..'9','_']) then begin
      // start of name=value pair
      NameValue:=Line;
      repeat
        p:=length(NameValue);
        while (p>=1) and (NameValue[p] in [' ',#9]) do dec(p);
        //List.Add(' NameValue="'+NameValue+'" p='+IntToStr(p)+' "'+NameValue[p]+'"');
        if (p>=1) and (NameValue[p]='\')
        and ((p=1) or (NameValue[p-1]<>'\')) then begin
          // append next line
          NameValue:=copy(NameValue,1,p-1);
          inc(i);
          if i>=MakefileFPC.Count then break;
          NameValue:=NameValue+MakefileFPC[i];
        end else break;
      until false;
      List.Add(NameValue);
    end;
    inc(i);
  end;
  MakefileFPC.Free;
end;

procedure ParseMakefileFPC(const Filename, SrcOS: string;
  out Dirs, SubDirs: string);

  function MakeSearchPath(const s: string): string;
  var
    SrcPos: Integer;
    DestPos: Integer;
  begin
    // check how much space is needed
    SrcPos:=1;
    DestPos:=0;
    while (SrcPos<=length(s)) do begin
      if s[SrcPos] in [#0..#31] then begin
        // space is a delimiter
        inc(SrcPos);
        // skip multiple spaces
        while (SrcPos<=length(s)) and (s[SrcPos] in [#0..#31]) do inc(SrcPos);
        if (DestPos>0) and (SrcPos<=length(s)) then begin
          inc(DestPos);// add semicolon
        end;
      end else begin
        inc(DestPos);
        inc(SrcPos);
      end;
    end;

    // allocate space
    SetLength(Result,DestPos);

    // create semicolon delimited search path
    SrcPos:=1;
    DestPos:=0;
    while (SrcPos<=length(s)) do begin
      if s[SrcPos] in [#0..#32] then begin
        // space is a delimiter
        inc(SrcPos);
        // skip multiple spaces
        while (SrcPos<=length(s)) and (s[SrcPos] in [#0..#32]) do inc(SrcPos);
        if (DestPos>0) and (SrcPos<=length(s)) then begin
          inc(DestPos);// add semicolon
          Result[DestPos]:=';';
        end;
      end else begin
        inc(DestPos);
        Result[DestPos]:=s[SrcPos];
        inc(SrcPos);
      end;
    end;
  end;

var
  Params: TStringList;
  i: Integer;
  Line: string;
  p: LongInt;
  Name: String;
  SubDirsName: String;
begin
  SubDirs:='';
  Dirs:='';
  Params:=TStringList.Create;
  try
    ReadMakefileFPC(Filename,Params);

    SubDirsName:='';
    if SrcOS<>'' then
      SubDirsName:='dirs_'+SrcOS;

    for i:=0 to Params.Count-1 do begin
      Line:=Params[i];
      if Line='' then continue;
      if (Line[1] in ['a'..'z','A'..'Z','0'..'9','_']) then begin
        p:=System.Pos('=',Line);
        if p<1 then continue;
        Name:=copy(Line,1,p-1);
        if Name=SubDirsName then begin
          SubDirs:=MakeSearchPath(copy(Line,p+1,length(Line)));
        end else if Name='dirs' then begin
          Dirs:=MakeSearchPath(copy(Line,p+1,length(Line)));
        end;
      end;
    end;
  except
    on e: Exception do begin
      debugln('Error: [ParseMakefileFPC] Filename=',Filename,': ',E.Message);
    end;
  end;
  Params.Free;
end;

function CompareFPCSourceRulesViaFilename(Rule1, Rule2: Pointer): integer;
var
  SrcRule1: TFPCSourceRule absolute Rule1;
  SrcRule2: TFPCSourceRule absolute Rule2;
begin
  Result:=CompareFilenames(SrcRule1.Filename,SrcRule2.Filename);
end;

function CompareFPCTargetConfigCacheItems(CacheItem1, CacheItem2: Pointer): integer;
var
  Item1: TPCTargetConfigCache absolute CacheItem1;
  Item2: TPCTargetConfigCache absolute CacheItem2;
begin
  Result:=CompareStr(Item1.TargetOS,Item2.TargetOS);
  if Result<>0 then exit;
  Result:=CompareStr(Item1.TargetCPU,Item2.TargetCPU);
  if Result<>0 then exit;
  Result:=CompareFilenames(Item1.Compiler,Item2.Compiler);
  if Result<>0 then exit;
  Result:=CompareFilenames(Item1.CompilerOptions,Item2.CompilerOptions);
end;

function CompareFPCSourceCacheItems(CacheItem1, CacheItem2: Pointer): integer;
var
  Src1: TFPCSourceCache absolute CacheItem1;
  Src2: TFPCSourceCache absolute CacheItem2;
begin
  Result:=CompareFilenames(Src1.Directory,Src2.Directory);
end;

function CompareDirectoryWithFPCSourceCacheItem(AString, CacheItem: Pointer
  ): integer;
var
  Src: TFPCSourceCache absolute CacheItem;
begin
  Result:=CompareFilenames(AnsiString(AString),Src.Directory);
end;

function DefineActionNameToAction(const s: string): TDefineAction;
begin
  for Result:=Low(TDefineAction) to High(TDefineAction) do
    if CompareText(s,DefineActionNames[Result])=0 then exit;
  Result:=da_None;
end;

function DefineTemplateFlagsToString(Flags: TDefineTemplateFlags): string;
var f: TDefineTemplateFlag;
begin
  Result:='';
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if f in Flags then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+DefineTemplateFlagNames[f];
    end;
  end;
end;

function CompareUnitLinkNodes(NodeData1, NodeData2: pointer): integer;
var Link1, Link2: TUnitNameLink;
begin
  Link1:=TUnitNameLink(NodeData1);
  Link2:=TUnitNameLink(NodeData2);
  Result:=CompareText(Link1.Unit_Name,Link2.Unit_Name);
end;

function CompareUnitNameWithUnitLinkNode(AUnitName: Pointer;
  NodeData: pointer): integer;
begin
  Result:=CompareText(String(AUnitName),TUnitNameLink(NodeData).Unit_Name);
end;

function CompareDirectoryDefines(NodeData1, NodeData2: pointer): integer;
var DirDef1, DirDef2: TDirectoryDefines;
begin
  DirDef1:=TDirectoryDefines(NodeData1);
  DirDef2:=TDirectoryDefines(NodeData2);
  Result:=CompareFilenames(DirDef1.Path,DirDef2.Path);
end;

function GetDefaultSrcOSForTargetOS(const TargetOS: string): string;
begin
  Result:='';
  if (CompareText(TargetOS,'linux')=0)
  or (CompareText(TargetOS,'freebsd')=0)
  or (CompareText(TargetOS,'netbsd')=0)
  or (CompareText(TargetOS,'openbsd')=0)
  or (CompareText(TargetOS,'darwin')=0)
  or (CompareText(TargetOS,'solaris')=0)
  or (CompareText(TargetOS,'haiku')=0)
  or (CompareText(TargetOS,'android')=0)
  then
    Result:='unix'
  else
  if (CompareText(TargetOS,'win32')=0)
  or (CompareText(TargetOS,'win64')=0)
  or (CompareText(TargetOS,'wince')=0)
  then
    Result:='win';
end;

function GetDefaultSrcOS2ForTargetOS(const TargetOS: string): string;
begin
  Result:='';
  if (CompareText(TargetOS,'freebsd')=0)
  or (CompareText(TargetOS,'netbsd')=0)
  or (CompareText(TargetOS,'openbsd')=0)
  or (CompareText(TargetOS,'darwin')=0)
  then
    Result:='bsd'
  else if (CompareText(TargetOS,'android')=0) then
    Result:='linux';
end;

function GetDefaultSrcCPUForTargetCPU(const TargetCPU: string): string;
begin
  Result:='';
  if (CompareText(TargetCPU,'i386')=0)
  or (CompareText(TargetCPU,'x86_64')=0)
  then
    Result:='x86';
end;

procedure SplitLazarusCPUOSWidgetCombo(const Combination: string;
  out CPU, OS, WidgetSet: string);
var
  StartPos, EndPos: integer;
begin
  StartPos:=1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  CPU:=copy(Combination,StartPos,EndPos-StartPos);
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  OS:=copy(Combination,StartPos,EndPos-StartPos);
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Combination)) and (Combination[EndPos]<>'-') do
    inc(EndPos);
  WidgetSet:=copy(Combination,StartPos,EndPos-StartPos);
end;

function GetCompiledFPCVersion: integer;
begin
  Result:=FPCVersionToNumber({$I %FPCVERSION%});
end;

function GetCompiledTargetOS: string;
begin
  Result:=lowerCase({$I %FPCTARGETOS%});
end;

function GetCompiledTargetCPU: string;
begin
  Result:=lowerCase({$I %FPCTARGETCPU%});
end;

function GetDefaultCompilerFilename(const TargetCPU: string;
  Cross: boolean): string;
begin
  if Cross then
    {$ifdef darwin}
    Result:='ppc' // the mach-o format supports "fat" binaries whereby
                  // a single executable contains machine code for several architectures
    {$else}
    Result:='ppcross'
    {$endif}
  else
    Result:='ppc';
  if TargetCPU='' then
    Result:='fpc'
  else if SysUtils.CompareText(TargetCPU,'i386')=0 then
    Result:=Result+'386'
  else if SysUtils.CompareText(TargetCPU,'m68k')=0 then
    Result:=Result+'86k'
  else if SysUtils.CompareText(TargetCPU,'alpha')=0 then
    Result:=Result+'apx'
  else if SysUtils.CompareText(TargetCPU,'powerpc')=0 then
    Result:=Result+'ppc'
  else if SysUtils.CompareText(TargetCPU,'powerpc64')=0 then
    Result:=Result+'ppc64'
  else if SysUtils.CompareText(TargetCPU,'arm')=0 then
    Result:=Result+'arm'
  else if SysUtils.CompareText(TargetCPU,'avr')=0 then
    Result:=Result+'avr'
  else if SysUtils.CompareText(TargetCPU,'sparc')=0 then
    Result:=Result+'sparc'
  else if SysUtils.CompareText(TargetCPU,'x86_64')=0 then
    Result:=Result+'x64'
  else if SysUtils.CompareText(TargetCPU,'ia64')=0 then
    Result:=Result+'ia64'
  else if SysUtils.CompareText(TargetCPU,'aarch64')=0 then
    Result:=Result+'aarch64'
  else
    Result:='fpc';
  Result:=Result+ExeExt;
end;

procedure GetTargetProcessors(const TargetCPU: string; aList: TStrings);

  procedure Arm;
  begin
    aList.Add('ARMV3');
    aList.Add('ARMV4');
    aList.Add('ARMV4T');
    aList.Add('ARMV5');
    aList.Add('ARMV5T');
    aList.Add('ARMV5TE');
    aList.Add('ARMV5TEJ');
    aList.Add('ARMV6');
    aList.Add('ARMV6K');
    aList.Add('ARMV6T2');
    aList.Add('ARMV6Z');
    aList.Add('ARMV6M');
    aList.Add('ARMV7');
    aList.Add('ARMV7A');
    aList.Add('ARMV7R');
    aList.Add('ARMV7M');
    aList.Add('ARMV7EM');
    aList.Add('CORTEXM3');
  end;

  procedure Intel_i386;
  begin
    aList.Add('80386');
    aList.Add('Pentium');
    aList.Add('Pentium2');
    aList.Add('Pentium3');
    aList.Add('Pentium4');
    aList.Add('PentiumM');
  end;

  procedure Intel_x86_64;
  begin
    aList.Add('ATHLON64');
  end;

  procedure PowerPC;
  begin
    aList.Add('604');
    aList.Add('750');
    aList.Add('7400');
    aList.Add('970');
  end;

  procedure PowerPC64;
  begin
    //aList.Add('power4');
    aList.Add('970');
    //aList.Add('power5');
    //aList.Add('power5+');
    //aList.Add('power6');
    //aList.Add('power6x');
    //aList.Add('power7');
    //aList.Add('power8');
  end;

  procedure Sparc;
  begin
    aList.Add('SPARC V7');
    aList.Add('SPARC V8');
    aList.Add('SPARC V9');
  end;

  procedure Mips;
  begin
    aList.Add('mips1');
    aList.Add('mips2');
    aList.Add('mips3');
    aList.Add('mips4');
    aList.Add('mips5');
    aList.Add('mips32');
    aList.Add('mips32r2');
  end;
  
  procedure AVR;
  begin
    aList.Add('AVR1');
    aList.Add('AVR2');
    aList.Add('AVR25');
    aList.Add('AVR3');
    aList.Add('AVR31');
    aList.Add('AVR35');
    aList.Add('AVR4');
    aList.Add('AVR5');
    aList.Add('AVR51');
    aList.Add('AVR6');
  end;
  
  procedure M68k;
  begin
    aList.Add('68000');
    aList.Add('68020');
    aList.Add('68040');
    aList.Add('68060');
    aList.Add('ISAA');
    aList.Add('ISAA+');
    aList.Add('ISAB');
    aList.Add('ISAC');
    aList.Add('CFV4');
  end;

begin
  case TargetCPU of
    'arm'    : Arm;
    'avr'    : AVR;
    'i386'   : Intel_i386;
    'm68k'   : M68k;
    'powerpc'  : PowerPC;
    'powerpc64': PowerPC64;
    'sparc'  : Sparc;
    'x86_64' : Intel_x86_64;
    'mipsel','mips' : Mips;
    'jvm'    : ;
    'aarch64'  : ;
  end;
end;

function GetFPCTargetOS(TargetOS: string): string;
begin
  Result:=lowercase(TargetOS);
end;

function GetFPCTargetCPU(TargetCPU: string): string;
begin
  Result:=LowerCase(TargetCPU);
end;

function IsCTExecutable(AFilename: string; out ErrorMsg: string): boolean;
begin
  Result:=false;
  AFilename:=ResolveDots(aFilename);
  //debugln(['IsFPCExecutable ',AFilename]);
  //debugln(['IsFPCompiler START ',aFilename]);
  if aFilename='' then begin
    ErrorMsg:='missing file name';
    exit;
  end;
  if not FilenameIsAbsolute(AFilename) then begin
    ErrorMsg:='file missing path';
    exit;
  end;
  if not FileExistsCached(AFilename) then begin
    ErrorMsg:='file not found';
    exit;
  end;
  if DirPathExistsCached(AFilename) then begin
    ErrorMsg:='file is a directory';
    exit;
  end;
  if not FileIsExecutableCached(AFilename) then begin
    ErrorMsg:='file is not executable';
    exit;
  end;
  ErrorMsg:='';
  Result:=true;
end;

function GuessPascalCompilerFromExeName(Filename: string): TPascalCompiler;
var
  ShortFilename: String;
begin
  ShortFilename:=LowerCase(ExtractFileNameOnly(Filename));

  // *pas2js*
  if Pos('pas2js',ShortFilename)>0 then
    exit(pcPas2js);

  // dcc*.exe
  if (LeftStr(ShortFilename,3)='dcc')
  and ((ExeExt='') or (CompareFileExt(Filename,ExeExt)=0))
  then
    exit(pcDelphi);

  Result:=pcFPC;
end;

function IsCompilerExecutable(AFilename: string; out ErrorMsg: string; out
  Kind: TPascalCompiler; Run: boolean): boolean;
var
  ShortFilename, Line: String;
  Params: TStringListUTF8;
  Lines: TStringList;
  i: Integer;
begin
  Result:=IsCTExecutable(AFilename,ErrorMsg);
  if not Result then exit;
  Kind:=pcFPC;

  // allow scripts like fpc.sh and fpc.bat
  ShortFilename:=ExtractFileNameOnly(AFilename);
  //debugln(['IsFPCompiler Short=',ShortFilename]);

  // check ppc*.exe
  if CompareText(LeftStr(ShortFilename,3),'ppc')=0 then
    exit(true);

  // check pas2js*
  if CompareText(LeftStr(ShortFilename,6),'pas2js')=0 then begin
    Kind:=pcPas2js;
    exit(true);
  end;

  // dcc*.exe
  if (CompareFilenames(LeftStr(ShortFilename,3),'dcc')=0)
      and ((ExeExt='') or (CompareFileExt(ShortFilename,ExeExt)=0))
  then begin
    Kind:=pcDelphi;
    exit(true);
  end;

  if Run then begin
    // run it and check for magics
    debugln(['Note: (lazarus) [IsCompilerExecutable] run "',AFilename,'"']);
    Params:=TStringListUTF8.Create;
    Lines:=nil;
    try
      Params.Add('-va');
      Lines:=RunTool(AFilename,Params);
      if Lines<>nil then begin
        for i:=0 to Lines.Count-1 do
        begin
          Line:=Lines[i];
          if Pos('fpc.cfg',Line)>0 then
          begin
            Kind:=pcFPC;
            exit(true);
          end;
          if Pos('pas2js.cfg',Line)>0 then
          begin
            Kind:=pcPas2js;
            exit(true);
          end;
        end;
        ErrorMsg:='Compiler -va does neither search for fpc.cfg nor pas2js.cfg. This is neither fpc nor pas2js.';
        exit;
      end;
    finally
      Params.Free;
      Lines.Free;
    end;
  end;

  // check fpc<something>
  // Note: fpc.exe is just a wrapper, it can call pas2js
  if CompareFilenames(ShortFilename,'fpc')=0 then
    exit(true);

  ErrorMsg:='fpc executable should start with fpc or ppc';
  Result:=false;
end;

function IsFPCExecutable(AFilename: string; out ErrorMsg: string; Run: boolean
  ): boolean;
var
  ShortFilename: String;
  Kind: TPascalCompiler;
begin
  if Run then begin
    Result:=IsCompilerExecutable(AFilename,ErrorMsg,Kind,true) and (Kind=pcFPC);
    exit;
  end;

  Result:=IsCTExecutable(AFilename,ErrorMsg);
  if not Result then exit;

  // allow scripts like fpc*.sh and fpc*.bat
  ShortFilename:=LowerCase(ExtractFileNameOnly(AFilename));
  //debugln(['IsFPCompiler Short=',ShortFilename]);
  if (LeftStr(ShortFilename,3)='fpc') then
    exit(true);

  // allow ppcxxx.exe
  if (LeftStr(ShortFilename,3)='ppc')
  and ((ExeExt='') or (CompareFileExt(AFilename,ExeExt)=0))
  then
    exit(true);

  ErrorMsg:='fpc executable should start with fpc or ppc';
end;

function IsPas2JSExecutable(AFilename: string; out ErrorMsg: string;
  Run: boolean): boolean;
var
  ShortFilename: String;
  Kind: TPascalCompiler;
begin
  if Run then begin
    Result:=IsCompilerExecutable(AFilename,ErrorMsg,Kind,true) and (Kind=pcPas2js);
    exit;
  end;

  Result:=IsCTExecutable(AFilename,ErrorMsg);
  if not Result then exit;

  // allow scripts like *pas2js*
  ShortFilename:=LowerCase(ExtractFileNameOnly(AFilename));
  if Pos('pas2js',ShortFilename)>0 then
    exit(true);

  ErrorMsg:='pas2js executable should start with pas2js';
end;

function CreateDefinesInDirectories(const SourcePaths, FlagName: string
  ): TDefineTemplate;
var
  StartPos: Integer;
  EndPos: LongInt;
  CurDirectory: String;
  DirsTempl: TDefineTemplate;
  DirTempl: TDefineTemplate;
  SetFlagTempl: TDefineTemplate;
begin
  // create a block template for the directories
  DirsTempl:=TDefineTemplate.Create(FlagName,
    'Block of directories to set '+FlagName,
    '','',da_Block);

  // create a define flag for every directory
  StartPos:=1;
  while StartPos<=length(SourcePaths) do begin
    EndPos:=StartPos;
    while (EndPos<=length(SourcePaths)) and (SourcePaths[EndPos]<>';') do
      inc(EndPos);
    if EndPos>StartPos then begin
      CurDirectory:=copy(SourcePaths,StartPos,EndPos-StartPos);
      DirTempl:=TDefineTemplate.Create('FlagDirectory','FlagDirectory',
        '',CurDirectory,da_Directory);
      SetFlagTempl:=TDefineTemplate.Create(FlagName,FlagName,
        FlagName,'1',da_Define);
      DirTempl.AddChild(SetFlagTempl);
      DirsTempl.AddChild(DirTempl);
    end;
    StartPos:=EndPos+1;
  end;
  
  Result:=DirsTempl;
end;

procedure InitDefaultFPCSourceRules;
begin
  DefaultFPCSourceRules:=TFPCSourceRules.Create;
  with DefaultFPCSourceRules do begin
    // put into an include file for easy edit via an editor
    {$I fpcsrcrules.inc}
  end;
end;

{ TDefineTemplate }

procedure TDefineTemplate.MarkFlags(
  const MustFlags, NotFlags: TDefineTemplateFlags;
  WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=ANode.FMarked
                   or (((ANode.Flags*MustFlags)=MustFlags)
                   and (ANode.Flags*NotFlags=[]));
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkFlags(MustFlags,NotFlags,true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.MarkOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags;
  WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=ANode.FMarked
                   or ((ANode.Owner=TheOwner)
                       and ((ANode.Flags*MustFlags)=MustFlags)
                       and (ANode.Flags*NotFlags=[]));
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkOwnedBy(TheOwner,MustFlags,NotFlags,true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.MarkNodes(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=true;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.ReverseMarks(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=not ANode.FMarked;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.MarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.InheritMarks(WithSiblings, WithChilds, Down,
  Up: boolean);
var
  ANode: TDefineTemplate;
  ChildNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    if WithChilds then begin
      ChildNode:=ANode.FirstChild;
      while ChildNode<>nil do begin
        if Down and ANode.FMarked then
          ChildNode.FMarked:=true;
        ChildNode.InheritMarks(false,true,Down,Up);
        if Up and ChildNode.FMarked then
          ANode.FMarked:=true;
        ChildNode:=ChildNode.Next;
      end;
    end;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.UnmarkNodes(WithSiblings, WithChilds: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.FMarked:=false;
    if (ANode.FirstChild<>nil) and WithChilds then
      ANode.FirstChild.UnmarkNodes(true,true);
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.RemoveMarked(WithSiblings: boolean;
  var FirstDefTemplate: TDefineTemplate);
var ANode, NextNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    NextNode:=ANode.Next;
    if ANode.FirstChild<>nil then begin
      ANode.FirstChild.RemoveMarked(true,FirstDefTemplate);
    end;
    if ANode.FMarked and (ANode.FirstChild=nil) then begin
      if ANode=FirstDefTemplate then FirstDefTemplate:=ANode.Next;
      ANode.Unbind;
      ANode.Free;
    end;
    if not WithSiblings then break;
    ANode:=NextNode;
  end;
end;

procedure TDefineTemplate.RemoveOwner(TheOwner: TObject; WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.RemoveOwner(TheOwner,true);
    if ANode.Owner=TheOwner then ANode.Owner:=nil;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.RemoveLeaves(TheOwner: TObject; const MustFlags,
  NotFlags: TDefineTemplateFlags; WithSiblings: boolean;
  var FirstDefTemplate: TDefineTemplate);
var ANode, NextNode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    NextNode:=ANode.Next;
    if ANode.FirstChild<>nil then
      ANode.FirstChild.RemoveLeaves(TheOwner,MustFlags,NotFlags,true,
                                    FirstDefTemplate);
    if ANode.FirstChild=nil then begin
      // this is a leaf
      if ((ANode.Owner=TheOwner)
      and ((ANode.Flags*MustFlags)=MustFlags)
      and (ANode.Flags*NotFlags=[]))
      then begin
        if ANode=FirstDefTemplate then
          FirstDefTemplate:=ANode.Next;
        ANode.Unbind;
        ANode.Free;
      end;
    end;
    if not WithSiblings then break;
    ANode:=NextNode;
  end;
end;

procedure TDefineTemplate.AddChild(ADefineTemplate: TDefineTemplate);
// add as last child
begin
  if ADefineTemplate=nil then exit;
  if ADefineTemplate.Parent<>nil then
    raise Exception.Create('TDefineTemplate.AddChild');
  if LastChild=nil then begin
    while ADefineTemplate<>nil do begin
      ADefineTemplate.fParent:=Self;
      if ADefineTemplate.Prior=nil then FFirstChild:=ADefineTemplate;
      if ADefineTemplate.Next=nil then FLastChild:=ADefineTemplate;
      inc(FChildCount);
      ADefineTemplate:=ADefineTemplate.Next;
    end;
  end else begin
    ADefineTemplate.InsertBehind(LastChild);
  end;
end;

procedure TDefineTemplate.ReplaceChild(ADefineTemplate: TDefineTemplate);
var
  OldTempl: TDefineTemplate;
begin
  OldTempl:=FindChildByName(ADefineTemplate.Name);
  if OldTempl<>nil then begin
    ADefineTemplate.InsertInFront(OldTempl);
    OldTempl.UnBind;
    OldTempl.Free;
  end else
    AddChild(ADefineTemplate);
end;

function TDefineTemplate.DeleteChild(const AName: string): boolean;
var
  OldTempl: TDefineTemplate;
begin
  OldTempl:=FindChildByName(AName);
  if OldTempl<>nil then begin
    Result:=true;
    OldTempl.Unbind;
    OldTempl.Free;
  end else
    Result:=false;
end;

procedure TDefineTemplate.InsertBehind(APrior: TDefineTemplate);
// insert this and all next siblings behind APrior
var ANode, LastSibling, NewParent: TDefineTemplate;
begin
  if APrior=nil then exit;
  NewParent:=APrior.Parent;
  if Parent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      if ANode=APrior then
        raise Exception.Create('internal error: '
          +'TDefineTemplate.InsertBehind: APrior=ANode');
      dec(Parent.FChildCount);
      ANode.FParent:=nil;
      ANode:=ANode.Next;
    end;
  end;
  LastSibling:=Self;
  while LastSibling.Next<>nil do LastSibling:=LastSibling.Next;
  FParent:=NewParent;
  if Parent<>nil then begin
    ANode:=Self;
    while (ANode<>nil) do begin
      ANode.FParent:=Parent;
      inc(Parent.FChildCount);
      ANode:=ANode.Next;
    end;
    if Parent.LastChild=APrior then Parent.FLastChild:=LastSibling;
  end;
  FPrior:=APrior;
  LastSibling.FNext:=APrior.Next;
  APrior.FNext:=Self;
  if LastSibling.Next<>nil then LastSibling.Next.FPrior:=LastSibling;
end;

procedure TDefineTemplate.InsertInFront(ANext: TDefineTemplate);
// insert this and all next siblings in front of ANext
var ANode, LastSibling: TDefineTemplate;
begin
  if ANext=nil then exit;
  if FParent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      if ANode=ANext then
        raise Exception.Create('internal error: '
          +'TDefineTemplate.InsertInFront: ANext=ANode');
      dec(FParent.FChildCount);
      ANode.FParent:=nil;
      ANode:=ANode.Next;
    end;
  end;
  LastSibling:=Self;
  while LastSibling.Next<>nil do LastSibling:=LastSibling.Next;
  FParent:=ANext.Parent;
  if Parent<>nil then begin
    ANode:=Self;
    while ANode<>nil do begin
      ANode.FParent:=Parent;
      inc(Parent.FChildCount);
      ANode:=ANode.Next;
    end;
    if Parent.FirstChild=ANext then Parent.FFirstChild:=Self;
  end;
  FPrior:=ANext.Prior;
  if Prior<>nil then Prior.FNext:=Self;
  LastSibling.FNext:=ANext;
  ANext.FPrior:=LastSibling;
end;

procedure TDefineTemplate.MoveToLast(Child: TDefineTemplate);
var
  Node: TDefineTemplate;
begin
  if Child.Next=nil then exit;
  Node:=Child.Next;
  while Node.Next<>nil do Node:=Node.Next;
  Child.Unbind;
  Child.InsertBehind(Node);
end;

procedure TDefineTemplate.Assign(ADefineTemplate: TDefineTemplate;
  WithSubNodes, WithNextSiblings, ClearOldSiblings: boolean);
var ChildTemplate, CopyTemplate, NextTemplate: TDefineTemplate;
begin
  Clear(ClearOldSiblings);
  if ADefineTemplate=nil then exit;
  AssignValues(ADefineTemplate);
  if WithSubNodes then begin
    ChildTemplate:=ADefineTemplate.FirstChild;
    if ChildTemplate<>nil then begin
      CopyTemplate:=TDefineTemplate.Create;
      AddChild(CopyTemplate);
      CopyTemplate.Assign(ChildTemplate,true,true,false);
    end;
  end;
  if WithNextSiblings then begin
    NextTemplate:=ADefineTemplate.Next;
    if NextTemplate<>nil then begin
      CopyTemplate:=TDefineTemplate.Create;
      CopyTemplate.InsertBehind(Self);
      CopyTemplate.Assign(NextTemplate,WithSubNodes,true,false);
    end;
  end;
end;

procedure TDefineTemplate.AssignValues(ADefineTemplate: TDefineTemplate);
begin
  Name:=ADefineTemplate.Name;
  Description:=ADefineTemplate.Description;
  Variable:=ADefineTemplate.Variable;
  Value:=ADefineTemplate.Value;
  Action:=ADefineTemplate.Action;
  Flags:=ADefineTemplate.Flags;
  MergeNameInFront:=ADefineTemplate.MergeNameInFront;
  MergeNameBehind:=ADefineTemplate.MergeNameBehind;
  Owner:=ADefineTemplate.Owner;
end;

procedure TDefineTemplate.Unbind;
begin
  if FPrior<>nil then FPrior.FNext:=FNext;
  if FNext<>nil then FNext.FPrior:=FPrior;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNext;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPrior;
    dec(FParent.FChildCount);
  end;
  FNext:=nil;
  FPrior:=nil;
  FParent:=nil;
end;

procedure TDefineTemplate.Clear(WithSiblings: boolean);
begin
  while FFirstChild<>nil do FFirstChild.Free;
  if WithSiblings then
    while FNext<>nil do FNext.Free;
  Name:='';
  Description:='';
  Value:='';
  Variable:='';
  Flags:=[];
end;

constructor TDefineTemplate.Create;
begin
  inherited Create;
end;

constructor TDefineTemplate.Create(const AName, ADescription, AVariable,
  AValue: string; AnAction: TDefineAction);
begin
  inherited Create;
  Name:=AName;
  Description:=ADescription;
  Variable:=AVariable;
  Value:=AValue;
  Action:=AnAction;
end;

function TDefineTemplate.CreateCopy(OnlyMarked: boolean;
  WithSiblings: boolean; WithChilds: boolean): TDefineTemplate;
var LastNewNode, NewNode, ANode: TDefineTemplate;
begin
  Result:=nil;
  LastNewNode:=nil;
  ANode:=Self;
  while ANode<>nil do begin
    if (not OnlyMarked) or (ANode.FMarked) then begin
      // copy node
      NewNode:=TDefineTemplate.Create;
      NewNode.Assign(ANode,false,false,false);
      if LastNewNode<>nil then
        NewNode.InsertBehind(LastNewNode)
      else
        Result:=NewNode;
      LastNewNode:=NewNode;
      // copy children
      if WithChilds and (ANode.FirstChild<>nil) then begin
        NewNode:=ANode.FirstChild.CreateCopy(OnlyMarked,true,true);
        if NewNode<>nil then
          LastNewNode.AddChild(NewNode);
      end;
    end;
    if not WithSiblings then break;
    ANode:=ANode.Next;
  end;
end;

function TDefineTemplate.CreateMergeCopy: TDefineTemplate;
begin
  CreateMergeInfo(false,false);
  Result:=TDefineTemplate.Create;
  Result.Assign(Self,true,false,false);
end;

function TDefineTemplate.FindRoot: TDefineTemplate;
begin
  Result:=Self;
  repeat
    if Result.Parent<>nil then
      Result:=Result.Parent
    else if Result.Prior<>nil then
      Result:=Result.Prior
    else
      break;
  until false;
end;

destructor TDefineTemplate.Destroy;
begin
  Clear(false);
  Unbind;
  inherited Destroy;
end;

function TDefineTemplate.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; ClearOldSiblings, WithMergeInfo: boolean): boolean;
var IndexedPath: string;
  i, LvlCount: integer;
  DefTempl, LastDefTempl: TDefineTemplate;
  NewChild: TDefineTemplate;
begin
  Clear(ClearOldSiblings);
  LvlCount:=XMLConfig.GetValue(Path+'Count/Value',0);
  DefTempl:=nil;
  for i:=1 to LvlCount do begin
    if i=1 then begin
      DefTempl:=Self;
      LastDefTempl:=Prior;
    end else begin
      LastDefTempl:=DefTempl;
      DefTempl:=TDefineTemplate.Create;
      DefTempl.InsertBehind(LastDefTempl);
    end;
    IndexedPath:=Path+'Node'+IntToStr(i)+'/';
    DefTempl.LoadValuesFromXMLConfig(XMLConfig,IndexedPath,WithMergeInfo);
    // load children
    if XMLConfig.GetValue(IndexedPath+'Count/Value',0)>0 then begin
      NewChild:=TDefineTemplate.Create;
      DefTempl.AddChild(NewChild);
      if not NewChild.LoadFromXMLConfig(XMLConfig,IndexedPath,
                                        false,WithMergeInfo) then
      begin
        Result:=false;  exit;
      end;
    end;
  end;
  Result:=true;
end;

procedure TDefineTemplate.LoadValuesFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; WithMergeInfo: boolean);
var f: TDefineTemplateFlag;
begin
  Name:=XMLConfig.GetValue(Path+'Name/Value','no name');
  Description:=XMLConfig.GetValue(Path+'Description/Value','');
  Value:=XMLConfig.GetValue(Path+'Value/Value','');
  Variable:=XMLConfig.GetValue(Path+'Variable/Value','');
  Action:=DefineActionNameToAction(
                         XMLConfig.GetValue(Path+'Action/Value',''));
  Flags:=[];
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if (f<>dtfAutoGenerated)
    and (XMLConfig.GetValue(Path+'Flags/'+DefineTemplateFlagNames[f],false))
    then
      Include(Flags,f);
  end;
  if WithMergeInfo then begin
    MergeNameInFront:=XMLConfig.GetValue(Path+'MergeNameInFront/Value','');
    MergeNameBehind:=XMLConfig.GetValue(Path+'MergeNameInFront/Value','');
  end else begin
    MergeNameInFront:='';
    MergeNameBehind:='';
  end;
end;

procedure TDefineTemplate.SaveValuesToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string; WithMergeInfo: boolean);
var
  f: TDefineTemplateFlag;
begin
  XMLConfig.SetDeleteValue(Path+'Name/Value',Name,'');
  XMLConfig.SetDeleteValue(Path+'Description/Value',Description,'');
  XMLConfig.SetDeleteValue(Path+'Value/Value',Value,'');
  XMLConfig.SetDeleteValue(Path+'Variable/Value',Variable,'');
  XMLConfig.SetDeleteValue(Path+'Action/Value',
                           DefineActionNames[Action],
                           DefineActionNames[da_None]);
  for f:=Low(TDefineTemplateFlag) to High(TDefineTemplateFlag) do begin
    if (f<>dtfAutoGenerated) then
      XMLConfig.SetDeleteValue(
         Path+'Flags/'+DefineTemplateFlagNames[f]
         ,f in Flags,false);
  end;
  if WithMergeInfo then begin
    XMLConfig.SetDeleteValue(Path+'MergeNameInFront/Value',
                             MergeNameInFront,'');
    XMLConfig.SetDeleteValue(Path+'MergeNameBehind/Value',
                             MergeNameBehind,'');
  end else begin
    XMLConfig.SetDeleteValue(Path+'MergeNameInFront/Value','','');
    XMLConfig.SetDeleteValue(Path+'MergeNameBehind/Value','','');
  end;
end;

procedure TDefineTemplate.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string;
  WithSiblings, OnlyMarked, WithMergeInfo, UpdateMergeInfo: boolean);
var IndexedPath: string;
  Index, LvlCount: integer;
  DefTempl: TDefineTemplate;
begin
  if UpdateMergeInfo then CreateMergeInfo(WithSiblings,OnlyMarked);
  DefTempl:=Self;
  LvlCount:=0;
  while DefTempl<>nil do begin
    inc(LvlCount);
    DefTempl:=DefTempl.Next;
  end;
  DefTempl:=Self;
  Index:=0;
  repeat
    if (DefTempl.FMarked) or (not OnlyMarked) then begin
      // save node
      inc(Index);
      IndexedPath:=Path+'Node'+IntToStr(Index)+'/';
      DefTempl.SaveValuesToXMLConfig(XMLConfig,IndexedPath,WithMergeInfo);
      // save children
      if DefTempl.FFirstChild<>nil then
        DefTempl.FirstChild.SaveToXMLConfig(XMLConfig,IndexedPath,
                                   true,OnlyMarked,
                                   WithMergeInfo,false)
      else
        XMLConfig.SetDeleteValue(IndexedPath+'Count/Value',0,0);
    end;
    if not WithSiblings then break;
    DefTempl:=DefTempl.Next;
  until DefTempl=nil;
  XMLConfig.SetDeleteValue(Path+'Count/Value',Index,0);
end;

procedure TDefineTemplate.CreateMergeInfo(WithSiblings, OnlyMarked: boolean);
var
  DefTempl: TDefineTemplate;
begin
  DefTempl:=Self;
  repeat
    if (DefTempl.FMarked) or (not OnlyMarked) then begin
      if DefTempl.Prior<>nil then
        DefTempl.MergeNameInFront:=DefTempl.Prior.Name
      else
        DefTempl.MergeNameInFront:='';
      if DefTempl.Next<>nil then
        DefTempl.MergeNameBehind:=DefTempl.Next.Name
      else
        DefTempl.MergeNameBehind:='';
      // update children
      if DefTempl.FFirstChild<>nil then
        DefTempl.FirstChild.CreateMergeInfo(true,OnlyMarked);
    end;
    if not WithSiblings then break;
    DefTempl:=DefTempl.Next;
  until DefTempl=nil;
end;

class procedure TDefineTemplate.MergeXMLConfig(ParentDefTempl: TDefineTemplate;
  var FirstSibling, LastSibling: TDefineTemplate;
  XMLConfig: TXMLConfig; const Path, NewNamePrefix: string);
var
  SrcNode: TDefineTemplate;
begin
  SrcNode:=TDefineTemplate.Create;
  SrcNode.LoadFromXMLConfig(XMLConfig,Path,false,true);
  MergeTemplates(ParentDefTempl,FirstSibling,LastSibling,SrcNode,true,
                 NewNamePrefix);
  SrcNode.Clear(true);
  SrcNode.Free;
end;

class procedure TDefineTemplate.MergeTemplates(ParentDefTempl: TDefineTemplate;
  var FirstSibling, LastSibling: TDefineTemplate;
  SourceTemplate: TDefineTemplate; WithSiblings: boolean;
  const NewNamePrefix: string);
// merge SourceTemplate. This will keep SourceTemplate untouched
var
  NewNode, PosNode: TDefineTemplate;
  Inserted: boolean;
  SrcNode: TDefineTemplate;
begin
  SrcNode:=SourceTemplate;
  while SrcNode<>nil do begin
    // merge all source nodes
    NewNode:=SrcNode.CreateCopy(false,false,false);
    Inserted:=false;
    if NewNode.Name<>'' then begin
      // node has a name -> test if already exists
      PosNode:=FirstSibling;
      while (PosNode<>nil)
      and (CompareText(PosNode.Name,NewNode.Name)<>0) do
        PosNode:=PosNode.Next;
      if PosNode<>nil then begin
        // node with same name already exists -> check if it is a copy
        if NewNode.IsEqual(PosNode,false,false) then begin
          // node already exists
          NewNode.Free;
          NewNode:=PosNode;
        end else begin
          // node has same name, but different values
          // -> rename node
          NewNode.Name:=NewNode.FindUniqueName(NewNamePrefix+NewNode.Name);
          // insert behind PosNode
          NewNode.InsertBehind(PosNode);
        end;
        Inserted:=true;
      end;
    end;
    if not Inserted then begin
      // node name is unique or empty -> insert node
      if NewNode.MergeNameInFront<>'' then begin
        // last time, node was inserted behind MergeNameInFront
        // -> search MergeNameInFront
        PosNode:=LastSibling;
        while (PosNode<>nil)
        and (CompareText(PosNode.Name,NewNode.MergeNameInFront)<>0) do
          PosNode:=PosNode.Prior;
        if PosNode<>nil then begin
          // MergeNameInFront found -> insert behind
          NewNode.InsertBehind(PosNode);
          Inserted:=true;
        end;
      end;
      if not Inserted then begin
        if NewNode.MergeNameBehind<>'' then begin
          // last time, node was inserted in front of MergeNameBehind
          // -> search MergeNameBehind
          PosNode:=FirstSibling;
          while (PosNode<>nil)
          and (CompareText(PosNode.Name,NewNode.MergeNameBehind)<>0) do
            PosNode:=PosNode.Next;
          if PosNode<>nil then begin
            // MergeNameBehind found -> insert in front
            NewNode.InsertInFront(PosNode);
            Inserted:=true;
          end;
        end;
      end;
      if not Inserted then begin
        // no merge position found -> add as last
        if LastSibling<>nil then begin
          NewNode.InsertBehind(LastSibling);
        end else if ParentDefTempl<>nil then begin
          ParentDefTempl.AddChild(NewNode);
        end;
      end;
    end;
    // NewNode is now inserted -> update FirstSibling and LastSibling
    if FirstSibling=nil then begin
      FirstSibling:=NewNode;
      LastSibling:=NewNode;
    end;
    while FirstSibling.Prior<>nil do
      FirstSibling:=FirstSibling.Prior;
    while LastSibling.Next<>nil do
      LastSibling:=LastSibling.Next;
    // merge children
    MergeTemplates(NewNode,NewNode.FFirstChild,NewNode.FLastChild,
                   SrcNode.FirstChild,true,NewNamePrefix);
    if not WithSiblings then break;
    SrcNode:=SrcNode.Next;
  end;
end;

procedure TDefineTemplate.ConsistencyCheck;
var RealChildCount: integer;
  DefTempl: TDefineTemplate;
begin
  RealChildCount:=0;
  DefTempl:=FFirstChild;
  if DefTempl<>nil then begin
    if DefTempl.Prior<>nil then begin
      // not first child
      RaiseCatchableException('');
    end;
    while DefTempl<>nil do begin
      if DefTempl.Parent<>Self then begin
        DebugLn('  C: DefTempl.Parent<>Self: ',Name,',',DefTempl.Name);
        RaiseCatchableException('');
      end;
      if (DefTempl.Next<>nil) and (DefTempl.Next.Prior<>DefTempl) then
        RaiseCatchableException('');
      if (DefTempl.Prior<>nil) and (DefTempl.Prior.Next<>DefTempl) then
        RaiseCatchableException('');
      DefTempl.ConsistencyCheck;
      DefTempl:=DefTempl.Next;
      inc(RealChildCount);
    end;
  end;
  if (Parent<>nil) then begin
    if (Prior=nil) and (Parent.FirstChild<>Self) then
      RaiseCatchableException('');
    if (Next=nil) and (Parent.LastChild<>Self) then
      RaiseCatchableException('');
  end;
  if RealChildCount<>FChildCount then
    RaiseCatchableException('');
end;

procedure TDefineTemplate.CalcMemSize(Stats: TCTMemStats);
var
  Child: TDefineTemplate;
begin
  Stats.Add('TDefineTemplate Instance Count',1);
  Stats.Add('TDefineTemplate',
    PtrUInt(InstanceSize)
    +MemSizeString(FMergeNameBehind)
    +MemSizeString(FMergeNameInFront)
    +MemSizeString(Name)
    +MemSizeString(Description)
    +MemSizeString(Variable)
    +MemSizeString(Value)
    +MemSizeString(Value)
    );
  Child:=FFirstChild;
  while Child<>nil do begin
    Child.CalcMemSize(Stats);
    Child:=Child.Next;
  end;
end;

procedure TDefineTemplate.SetDefineOwner(NewOwner: TObject;
  WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.Owner:=NewOwner;
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.SetDefineOwner(NewOwner,true);
    if not WithSiblings then exit;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.SetFlags(AddFlags, SubFlags: TDefineTemplateFlags;
  WithSiblings: boolean);
var
  ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    ANode.Flags:=ANode.Flags+AddFlags-SubFlags;
    if ANode.FFirstChild<>nil then
      ANode.FFirstChild.SetFlags(AddFlags,SubFlags,true);
    if not WithSiblings then exit;
    ANode:=ANode.Next;
  end;
end;

procedure TDefineTemplate.WriteDebugReport(OnlyMarked: boolean);

  procedure WriteNode(ANode: TDefineTemplate; const Prefix: string);
  var ActionStr: string;
  begin
    if ANode=nil then exit;
    if (not OnlyMarked) or (ANode.Marked) then begin
      ActionStr:=DefineActionNames[ANode.Action];
      DebugLn(Prefix+'Self='+DbgS(ANode),
        ' Name="'+ANode.Name,'"',
        ' Next='+DbgS(ANode.Next),
        ' Prior='+DbgS(ANode.Prior),
        ' Action='+ActionStr,
        ' Flags=['+DefineTemplateFlagsToString(ANode.Flags),']',
        ' Marked='+dbgs(ANode.Marked)
        );
      DebugLn(Prefix+'   + Description="',ANode.Description,'"');
      DebugLn(Prefix+'   + Variable="',ANode.Variable,'"');
      DebugLn(Prefix+'   + Value="',ANode.Value,'"');
    end;
    WriteNode(ANode.FirstChild,Prefix+'  ');
    WriteNode(ANode.Next,Prefix);
  end;

begin
  WriteNode(Self,'  ');
end;

function TDefineTemplate.GetNext: TDefineTemplate;
begin
  if FirstChild<>nil then
    exit(FirstChild);
  Result:=GetNextSkipChildren;
end;

function TDefineTemplate.GetNextSkipChildren: TDefineTemplate;
begin
  Result:=Self;
  while (Result<>nil) do begin
    if Result.Next<>nil then begin
      Result:=Result.Next;
      exit;
    end;
    Result:=Result.Parent;
  end;
end;

function TDefineTemplate.HasDefines(OnlyMarked, WithSiblings: boolean): boolean;
var
  CurTempl: TDefineTemplate;
begin
  Result:=true;
  CurTempl:=Self;
  while CurTempl<>nil do begin
    if ((not OnlyMarked) or (CurTempl.FMarked))
    and (CurTempl.Action in DefineActionDefines) then exit;
    // go to next
    if CurTempl.FFirstChild<>nil then
      CurTempl:=CurTempl.FFirstChild
    else if (CurTempl.FNext<>nil)
    and (WithSiblings or (CurTempl.Parent<>Parent)) then
      CurTempl:=CurTempl.FNext
    else begin
      // search uncle
      repeat
        CurTempl:=CurTempl.Parent;
        if (CurTempl=Parent)
        or ((CurTempl.Parent=Parent) and not WithSiblings) then begin
          Result:=false;
          exit;
        end;
      until (CurTempl.FNext<>nil);
      CurTempl:=CurTempl.FNext;
    end;
  end;
  Result:=false;
end;

function TDefineTemplate.IsEqual(ADefineTemplate: TDefineTemplate;
  CheckSubNodes, CheckNextSiblings: boolean): boolean;
var SrcNode, DestNode: TDefineTemplate;
begin
  Result:=(ADefineTemplate<>nil)
      and (Name=ADefineTemplate.Name)
      and (Description=ADefineTemplate.Description)
      and (Variable=ADefineTemplate.Variable)
      and (Value=ADefineTemplate.Value)
      and (Action=ADefineTemplate.Action)
      and (Flags=ADefineTemplate.Flags)
      and (Owner=ADefineTemplate.Owner);
  if not Result then begin
    exit;
  end;
  if CheckSubNodes then begin
    if (ChildCount<>ADefineTemplate.ChildCount) then begin
      Result:=false;
      exit;
    end;
    SrcNode:=FirstChild;
    DestNode:=ADefineTemplate.FirstChild;
    if SrcNode<>nil then begin
      Result:=SrcNode.IsEqual(DestNode,CheckSubNodes,true);
      if not Result then exit;
    end;
  end;
  if CheckNextSiblings then begin
    SrcNode:=Next;
    DestNode:=ADefineTemplate.Next;
    while (SrcNode<>nil) and (DestNode<>nil) do begin
      Result:=SrcNode.IsEqual(DestNode,CheckSubNodes,false);
      if not Result then exit;
      SrcNode:=SrcNode.Next;
      DestNode:=DestNode.Next;
    end;
    Result:=(SrcNode=nil) and (DestNode=nil);
    if not Result then begin
      //DebugLn('TDefineTemplate.IsEqual DIFF 3 ',Name,' ',
      //  ADefineTemplate.Name,' ',dbgs(ChildCount),' ',dbgs(ADefineTemplate.ChildCount));
    end;
  end;
end;

function TDefineTemplate.IsAutoGenerated: boolean;
begin
  Result:=SelfOrParentContainsFlag(dtfAutoGenerated);
end;

procedure TDefineTemplate.RemoveFlags(TheFlags: TDefineTemplateFlags);
var ANode: TDefineTemplate;
begin
  ANode:=Self;
  while ANode<>nil do begin
    Flags:=Flags-TheFlags;
    if FirstChild<>nil then FirstChild.RemoveFlags(TheFlags);
    ANode:=ANode.Next;
  end;
end;

function TDefineTemplate.Level: integer;
var ANode: TDefineTemplate;
begin
  Result:=-1;
  ANode:=Self;
  while ANode<>nil do begin
    inc(Result);
    ANode:=ANode.Parent;
  end;
end;

function TDefineTemplate.GetFirstSibling: TDefineTemplate;
begin
  Result:=Self;
  while Result.Prior<>nil do Result:=Result.Prior;
end;

function TDefineTemplate.SelfOrParentContainsFlag(
  AFlag: TDefineTemplateFlag): boolean;
var Node: TDefineTemplate;
begin
  Node:=Self;
  while (Node<>nil) do begin
    if AFlag in Node.Flags then begin
      Result:=true;
      exit;
    end;
    Node:=Node.Parent;
  end;
  Result:=false;
end;

function TDefineTemplate.FindChildByName(const AName: string): TDefineTemplate;
begin
  if FirstChild<>nil then begin
    Result:=FirstChild.FindByName(AName,false,true)
  end else
    Result:=nil;
end;

function TDefineTemplate.FindByName(const AName: string; WithSubChilds,
  WithNextSiblings: boolean): TDefineTemplate;
var ANode: TDefineTemplate;
begin
  if CompareText(AName,Name)=0 then begin
    Result:=Self;
  end else begin
    if WithSubChilds and (FirstChild<>nil) then
      Result:=FirstChild.FindByName(AName,true,true)
    else
      Result:=nil;
    if (Result=nil) and WithNextSiblings then begin
      ANode:=Next;
      while (ANode<>nil) do begin
        Result:=ANode.FindByName(AName,WithSubChilds,false);
        if Result<>nil then break;
        ANode:=ANode.Next;
      end;
    end;
  end;
end;

function TDefineTemplate.FindUniqueName(const Prefix: string): string;
var Root: TDefineTemplate;
  i: integer;
begin
  Root:=FindRoot;
  i:=0;
  repeat
    inc(i);
    Result:=Prefix+IntToStr(i);
  until Root.FindByName(Result,true,true)=nil;
end;


{ TDirectoryDefines }

constructor TDirectoryDefines.Create;
begin
  inherited Create;
  Values:=TExpressionEvaluator.Create;
  Path:='';
end;

destructor TDirectoryDefines.Destroy;
begin
  Values.Free;
  inherited Destroy;
end;

procedure TDirectoryDefines.CalcMemSize(Stats: TCTMemStats);
begin
  Stats.Add('TDirectoryDefines',PtrUInt(InstanceSize)
    +MemSizeString(Path));
  if Values<>nil then
    Stats.Add('TDirectoryDefines.Values',Values.CalcMemSize(false,nil));
end;


{ TDefineTree }

procedure TDefineTree.Clear;
begin
  if FFirstDefineTemplate<>nil then begin
    FFirstDefineTemplate.Clear(true);
    FFirstDefineTemplate.Free;
    FFirstDefineTemplate:=nil;
  end;
  ClearCache;
end;

function TDefineTree.IsEqual(SrcDefineTree: TDefineTree): boolean;
begin
  Result:=false;
  if SrcDefineTree=nil then exit;
  if (FFirstDefineTemplate=nil) xor (SrcDefineTree.FFirstDefineTemplate=nil)
  then exit;
  if (FFirstDefineTemplate<>nil)
  and (not FFirstDefineTemplate.IsEqual(
                                  SrcDefineTree.FFirstDefineTemplate,true,true))
  then exit;
  Result:=true;
end;

procedure TDefineTree.Assign(SrcDefineTree: TDefineTree);
begin
  if IsEqual(SrcDefineTree) then exit;
  Clear;
  if SrcDefineTree.FFirstDefineTemplate<>nil then begin
    FFirstDefineTemplate:=TDefineTemplate.Create;
    FFirstDefineTemplate.Assign(SrcDefineTree.FFirstDefineTemplate,
                                true,true,true);
  end;
end;

procedure TDefineTree.AssignNonAutoCreated(SrcDefineTree: TDefineTree);
var
  SrcNonAutoCreated: TDefineTemplate;
begin
  MarkNonAutoCreated;
  RemoveMarked;
  SrcNonAutoCreated:=SrcDefineTree.ExtractNonAutoCreated;
  if SrcNonAutoCreated=nil then exit;
  //DebugLn('TDefineTree.AssignNonAutoCreated A Front=',SrcNonAutoCreated.MergeNameInFront,' Behind=',SrcNonAutoCreated.MergeNameBehind);
  MergeTemplates(SrcNonAutoCreated,'');
  SrcNonAutoCreated.Clear(true);
  SrcNonAutoCreated.Free;
  FFirstDefineTemplate.CreateMergeInfo(true,false);
  //DebugLn('TDefineTree.AssignNonAutoCreated B Front=',FFirstDefineTemplate.MergeNameInFront,' Behind=',FFirstDefineTemplate.MergeNameBehind);
end;

procedure TDefineTree.ClearCache;
begin
  if (FCache.Count=0) and (FVirtualDirCache=nil) then exit;
  DoClearCache;
end;

constructor TDefineTree.Create;
begin
  inherited Create;
  IncreaseChangeStep;
  FFirstDefineTemplate:=nil;
  FCache:=TAVLTree.Create(@CompareDirectoryDefines);
  FDefineStrings:=TStringTree.Create;

  FMacroFunctions:=TKeyWordFunctionList.Create('TDefineTree.Create.MacroFunctions');
  FMacroFunctions.AddExtended('Ext',nil,@MacroFuncExtractFileExt);
  FMacroFunctions.AddExtended('PATH',nil,@MacroFuncExtractFilePath);
  FMacroFunctions.AddExtended('NAME',nil,@MacroFuncExtractFileName);
  FMacroFunctions.AddExtended('NAMEONLY',nil,@MacroFuncExtractFileNameOnly);
  
  FMacroVariables:=TKeyWordFunctionList.Create('TDefineTree.Create.MacroVariables');
end;

destructor TDefineTree.Destroy;
begin
  Clear;
  FMacroVariables.Free;
  FMacroFunctions.Free;
  FCache.Free;
  FreeAndNil(FDefineStrings);
  inherited Destroy;
end;

function TDefineTree.GetLastRootTemplate: TDefineTemplate;
begin
  Result:=FFirstDefineTemplate;
  if Result=nil then exit;
  while Result.Next<>nil do Result:=Result.Next;
end;

function TDefineTree.FindDirectoryInCache(
  const Path: string): TDirectoryDefines;
var cmp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=FCache.Root;
  while (ANode<>nil) do begin
    cmp:=CompareFilenames(Path,TDirectoryDefines(ANode.Data).Path);
    if cmp<0 then
      ANode:=ANode.Left
    else if cmp>0 then
      ANode:=ANode.Right
    else
      break;
  end;
  if ANode<>nil then
    Result:=TDirectoryDefines(ANode.Data)
  else
    Result:=nil;
end;

function TDefineTree.GetDirDefinesForDirectory(const Path: string;
  WithVirtualDir: boolean): TDirectoryDefines;
var
  ExpPath: String;
begin
  //DebugLn('[TDefineTree.GetDirDefinesForDirectory] "',Path,'"');
  if (Path<>'') or (not WithVirtualDir) then begin
    DoPrepareTree;
    ExpPath:=AppendPathDelim(TrimFilename(Path));
    Result:=FindDirectoryInCache(ExpPath);
    if Result=nil then begin
      Result:=TDirectoryDefines.Create;
      Result.Path:=ExpPath;
      //DebugLn('[TDefineTree.GetDirDefinesForDirectory] B ',ExpPath,' ');
      if Calculate(Result) then begin
        //DebugLn('[TDefineTree.GetDirDefinesForDirectory] C success');
        RemoveDoubles(Result);
        FCache.Add(Result);
      end else begin
        Result.Free;
        Result:=nil;
      end;
    end;
  end else begin
    Result:=GetDirDefinesForVirtualDirectory;
  end;
end;

function TDefineTree.GetDirDefinesForVirtualDirectory: TDirectoryDefines;
begin
  DoPrepareTree;
  if FVirtualDirCache=nil then begin
    //DebugLn('################ TDefineTree.GetDirDefinesForVirtualDirectory');
    FVirtualDirCache:=TDirectoryDefines.Create;
    FVirtualDirCache.Path:=VirtualDirectory;
    if Calculate(FVirtualDirCache) then begin
      //DebugLn('TDefineTree.GetDirDefinesForVirtualDirectory ');
      RemoveDoubles(FVirtualDirCache);
    end else begin
      FVirtualDirCache.Free;
      FVirtualDirCache:=nil;
    end;
  end;
  Result:=FVirtualDirCache;
end;

function TDefineTree.MacroFuncExtractFileExt(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileExt(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFilePath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFilePath(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFileName(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileName(FuncData^.Param);
  Result:=true;
end;

function TDefineTree.MacroFuncExtractFileNameOnly(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
begin
  FuncData:=PReadFunctionData(Data);
  FuncData^.Result:=ExtractFileNameOnly(FuncData^.Param);
  Result:=true;
end;

procedure TDefineTree.DoClearCache;
begin
  {$IFDEF VerboseDefineCache}
  DebugLn('TDefineTree.DoClearCache A +++++++++');
  {$ENDIF}
  if FCache<>nil then FCache.FreeAndClear;
  if FVirtualDirCache<>nil then begin
    FVirtualDirCache.Free;
    FVirtualDirCache:=nil;
  end;
  IncreaseChangeStep;
  FDefineStrings.Clear;
end;

procedure TDefineTree.DoPrepareTree;
begin
  if Assigned(OnPrepareTree) then OnPrepareTree(Self);
end;

procedure TDefineTree.RemoveMarked;
begin
  if FFirstDefineTemplate=nil then exit;
  FFirstDefineTemplate.RemoveMarked(true,FFirstDefineTemplate);
  ClearCache;
end;

procedure TDefineTree.MarkNonAutoCreated;
begin
  if FFirstDefineTemplate=nil then exit;
  with FFirstDefineTemplate do begin
    // clear marks
    UnmarkNodes(true,true);
    // mark each non autocreated node
    MarkFlags([],[dtfAutoGenerated],true,true);
    // mark every parent with a marked child
    InheritMarks(true,true,false,true);
  end;
end;

function TDefineTree.GetUnitPathForDirectory(const Directory: string): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[UnitPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetIncludePathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[IncludePathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetSrcPathForDirectory(const Directory: string): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[SrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetPPUSrcPathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[PPUSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetDCUSrcPathForDirectory(const Directory: string
  ): string;
var Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[DCUSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetCompiledSrcPathForDirectory(const Directory: string
  ): string;
var
  Evaluator: TExpressionEvaluator;
begin
  Evaluator:=GetDefinesForDirectory(Directory,true);
  if Evaluator<>nil then begin
    Result:=Evaluator.Variables[CompiledSrcPathMacroName];
  end else begin
    Result:='';
  end;
end;

function TDefineTree.GetDefinesForDirectory(
  const Path: string; WithVirtualDir: boolean): TExpressionEvaluator;
var
  DirDef: TDirectoryDefines;
begin
  DirDef:=GetDirDefinesForDirectory(Path,WithVirtualDir);
  if DirDef<>nil then
    Result:=DirDef.Values
  else
    Result:=nil;
end;

function TDefineTree.GetDefinesForVirtualDirectory: TExpressionEvaluator;
var
  DirDef: TDirectoryDefines;
begin
  DirDef:=GetDirDefinesForVirtualDirectory;
  if DirDef<>nil then
    Result:=DirDef.Values
  else
    Result:=nil;
end;

procedure TDefineTree.ReadValue(const DirDef: TDirectoryDefines;
  const PreValue, CurDefinePath: string; out NewValue: string);
var
  Buffer: PChar;
  BufferPos: integer;
  BufferSize: integer;
  ValuePos: integer;

  function SearchBracketClose(const s: string; Position:integer): integer;
  var BracketClose:char;
    sLen: Integer;
  begin
    if s[Position]='(' then
      BracketClose:=')'
    else
      BracketClose:='{';
    inc(Position);
    sLen:=length(s);
    while (Position<=sLen) and (s[Position]<>BracketClose) do begin
      if s[Position]=SpecialChar then
        inc(Position)
      else if (s[Position] in ['(','{']) then
        Position:=SearchBracketClose(s,Position);
      inc(Position);
    end;
    Result:=Position;
  end;

  function ExecuteMacroFunction(const FuncName, Params: string): string;
  var
    FuncData: TReadFunctionData;
  begin
    FuncData.Param:=Params;
    FuncData.Result:='';
    FMacroFunctions.DoDataFunction(PChar(Pointer(FuncName)),length(FuncName),
                                   @FuncData);
    Result:=FuncData.Result;
  end;
  
  function ExecuteMacroVariable(var MacroVariable: string): boolean;
  var
    FuncData: TReadFunctionData;
  begin
    FuncData.Param:=MacroVariable;
    FuncData.Result:='';
    Result:=FMacroVariables.DoDataFunction(
                 PChar(Pointer(MacroVariable)),length(MacroVariable),@FuncData);
    if Result then
      MacroVariable:=FuncData.Result;
  end;

  procedure GrowBuffer(MinSize: integer);
  var
    NewSize: Integer;
  begin
    if MinSize<=BufferSize then exit;
    NewSize:=MinSize*2+100;
    ReAllocMem(Buffer,NewSize);
    BufferSize:=NewSize;
  end;

  procedure CopyStringToBuffer(const Src: string);
  begin
    if Src='' then exit;
    Move(Src[1],Buffer[BufferPos],length(Src));
    inc(BufferPos,length(Src));
  end;

  procedure CopyFromValueToBuffer(Len: integer);
  begin
    if Len=0 then exit;
    Move(NewValue[ValuePos],Buffer[BufferPos],Len);
    inc(BufferPos,Len);
    inc(ValuePos,Len);
  end;

  function Substitute(const CurValue: string; ValueLen: integer;
    MacroStart: integer; var MacroEnd: integer): boolean;
  var
    MacroFuncNameEnd: Integer;
    MacroFuncNameLen: Integer;
    MacroStr: String;
    MacroFuncName: String;
    NewMacroLen: Integer;
    MacroParam: string;
    OldMacroLen: Integer;
    Handled: Boolean;
    MacroVarName: String;
  begin
    Result:=false;
    MacroFuncNameEnd:=MacroEnd;
    MacroFuncNameLen:=MacroFuncNameEnd-MacroStart-1;
    MacroEnd:=SearchBracketClose(CurValue,MacroFuncNameEnd)+1;
    if MacroEnd>ValueLen+1 then exit;
    OldMacroLen:=MacroEnd-MacroStart;
    // Macro found
    if MacroFuncNameLen>0 then begin
      MacroFuncName:=copy(CurValue,MacroStart+1,MacroFuncNameLen);
      // Macro function -> substitute macro parameter first
      ReadValue(DirDef,copy(CurValue,MacroFuncNameEnd+1
          ,MacroEnd-MacroFuncNameEnd-2),CurDefinePath,MacroParam);
      // execute the macro function
      //debugln('Substitute MacroFuncName="',MacroFuncName,'" MacroParam="',MacroParam,'"');
      MacroStr:=ExecuteMacroFunction(MacroFuncName,MacroParam);
    end else begin
      // Macro variable
      MacroVarName:=copy(CurValue,MacroStart+2,MacroEnd-MacroStart-3);
      MacroStr:=MacroVarName;
      //DebugLn('**** MacroVarName=',MacroVarName,' ',DirDef.Values.Variables[MacroVarName]);
      //DebugLn('DirDef.Values=',DirDef.Values.AsString);
      if MacroVarName=DefinePathMacroName then begin
        MacroStr:=CurDefinePath;
      end else if DirDef.Values.IsDefined(MacroVarName) then begin
        MacroStr:=DirDef.Values.Variables[MacroVarName];
      end else begin
        Handled:=false;
        if Assigned(FOnReadValue) then begin
          MacroParam:=MacroVarName;
          MacroStr:='';
          FOnReadValue(Self,MacroParam,MacroStr,Handled);
        end;
        if not Handled then begin
          MacroStr:=MacroVarName;
          Handled:=ExecuteMacroVariable(MacroStr);
        end;
        if not Handled then begin
          MacroStr:='';
        end;
      end;
    end;
    NewMacroLen:=length(MacroStr);
    GrowBuffer(BufferPos+NewMacroLen-OldMacroLen+ValueLen-ValuePos+1);
    // copy text between this macro and last macro
    CopyFromValueToBuffer(MacroStart-ValuePos);
    // copy macro value to buffer
    CopyStringToBuffer(MacroStr);
    ValuePos:=MacroEnd;
    Result:=true;
  end;

  procedure SetNewValue;
  var
    RestLen: Integer;
  begin
    if Buffer=nil then exit;
    // write rest to buffer
    RestLen:=length(NewValue)-ValuePos+1;
    if RestLen>0 then begin
      GrowBuffer(BufferPos+RestLen);
      Move(NewValue[ValuePos],Buffer[BufferPos],RestLen);
      inc(BufferPos,RestLen);
    end;
    // copy the buffer into NewValue
    //DebugLn('    [ReadValue] Old="',copy(NewValue,1,100),'"');
    SetLength(NewValue,BufferPos);
    if BufferPos>0 then
      Move(Buffer^,NewValue[1],BufferPos);
    //DebugLn('    [ReadValue] New="',copy(NewValue,1,100),'"');
    // clean up
    FreeMem(Buffer);
    Buffer:=nil;
  end;

var MacroStart,MacroEnd: integer;
  ValueLen: Integer;
begin
  //  DebugLn('    [ReadValue] A   "',copy(PreValue,1,100),'"');
  NewValue:=PreValue;
  if NewValue='' then exit;
  MacroStart:=1;
  ValueLen:=length(NewValue);
  Buffer:=nil;
  BufferSize:=0;
  BufferPos:=0; // position in buffer
  ValuePos:=1;  // same position in value
  while MacroStart<=ValueLen do begin
    // search for macro
    while (MacroStart<=ValueLen) and (NewValue[MacroStart]<>'$') do begin
      if (NewValue[MacroStart]=SpecialChar) then inc(MacroStart);
      inc(MacroStart);
    end;
    if MacroStart>ValueLen then break;
    // read macro function name
    MacroEnd:=MacroStart+1;
    while (MacroEnd<=ValueLen)
    and (NewValue[MacroEnd] in ['0'..'9','A'..'Z','a'..'z','_']) do
      inc(MacroEnd);
    // read macro name / parameters
    if (MacroEnd<ValueLen) and (NewValue[MacroEnd] in ['(','{']) then
    begin
      if not Substitute(NewValue,ValueLen,MacroStart,MacroEnd) then break;
    end;
    MacroStart:=MacroEnd;
  end;
  if Buffer<>nil then SetNewValue;
end;

procedure TDefineTree.MarkTemplatesOwnedBy(TheOwner: TObject; const MustFlags,
  NotFlags: TDefineTemplateFlags);
begin
  if FFirstDefineTemplate=nil then exit;
  with FFirstDefineTemplate do begin
    // unmark all nodes
    UnmarkNodes(true,true);
    // mark each node in filter
    MarkOwnedBy(TheOwner,MustFlags,NotFlags,true,true);
    // mark every parent, that has a marked child
    InheritMarks(true,true,false,true);
  end;
end;

procedure TDefineTree.RemoveTemplatesOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags);
begin
  if FFirstDefineTemplate=nil then exit;
  FFirstDefineTemplate.RemoveLeaves(TheOwner,MustFlags,NotFlags,true,
                                    FFirstDefineTemplate);
  FFirstDefineTemplate.RemoveOwner(TheOwner,true);
  ClearCache;
end;

function TDefineTree.ExtractTemplatesOwnedBy(TheOwner: TObject;
  const MustFlags, NotFlags: TDefineTemplateFlags): TDefineTemplate;
begin
  Result:=nil;
  if FFirstDefineTemplate=nil then exit;
  MarkTemplatesOwnedBy(TheOwner,MustFlags,NotFlags);
  with FFirstDefineTemplate do begin
    // store some information, so that merging the nodes will result in old order
    CreateMergeInfo(true,false);
    // extract marked nodes
    Result:=CreateCopy(true,true,true);
  end;
end;

function TDefineTree.ExtractNonAutoCreated: TDefineTemplate;
begin
  Result:=nil;
  if FFirstDefineTemplate=nil then exit;
  MarkNonAutoCreated;
  with FFirstDefineTemplate do begin
    // store some information, so that merging the nodes will result in old order
    CreateMergeInfo(true,false);
    // extract marked nodes
    Result:=CreateCopy(true,true,true);
  end;
end;

procedure TDefineTree.MergeTemplates(SourceTemplate: TDefineTemplate;
  const NewNamePrefix: string);
var
  LastDefTempl: TDefineTemplate;
begin
  LastDefTempl:=GetLastRootTemplate;
  TDefineTemplate.MergeTemplates(nil,FFirstDefineTemplate,LastDefTempl,
                  SourceTemplate,true,NewNamePrefix);
  ClearCache;
end;

function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
// calculates the values for a single directory
// returns false on error
var
  ExpandedDirectory, EvalResult, TempValue: string;

  procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  
    procedure CalculateIfChildren;
    begin
      // execute children
      CalculateTemplate(DefTempl.FirstChild,CurPath);
      // jump to end of else templates
      while (DefTempl.Next<>nil)
      and (DefTempl.Next.Action in [da_Else,da_ElseIf])
      do begin
        if Assigned(OnCalculate) then
          OnCalculate(Self,DefTempl,false,'',false,'',false);
        DefTempl:=DefTempl.Next;
      end;
    end;

  // procedure CalculateTemplate(DefTempl: TDefineTemplate; const CurPath: string);
  var SubPath, TempValue: string;
    VarName: string;
  begin
    while DefTempl<>nil do begin
      //DebugLn('  [CalculateTemplate] CurPath="',CurPath,'" DefTempl.Name="',DefTempl.Name,'"');
      case DefTempl.Action of
      da_Block:
        // calculate children
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;

      da_Define:
        // Define for a single Directory (not SubDirs)
        begin
          if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
            ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,TempValue,false,'',true);
            ReadValue(DirDef,DefTempl.Variable,CurPath,VarName);
            DirDef.Values.Variables[VarName]:=TempValue;
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',false);
          end;
        end;

      da_DefineRecurse:
        // Define for current and sub directories
        begin
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,true,TempValue,false,'',true);
          ReadValue(DirDef,DefTempl.Variable,CurPath,VarName);
          DirDef.Values.Variables[VarName]:=TempValue;
        end;

      da_Undefine:
        // Undefine for a single Directory (not SubDirs)
        if FilenameIsMatching(CurPath,ExpandedDirectory,true) then begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          ReadValue(DirDef,DefTempl.Variable,CurPath,VarName);
          DirDef.Values.Undefine(VarName);
        end else begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',false);
        end;

      da_UndefineRecurse:
        // Undefine for current and sub directories
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          ReadValue(DirDef,DefTempl.Variable,CurPath,VarName);
          DirDef.Values.Undefine(VarName);
        end;

      da_UndefineAll:
        // Undefine every value for current and sub directories
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          DirDef.Values.Clear;
        end;

      da_If, da_ElseIf:
        begin
          // test expression in value
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          EvalResult:=DirDef.Values.Eval(TempValue,true);
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,true,TempValue,true,EvalResult,EvalResult='1');
          //debugln('da_If,da_ElseIf: DefTempl.Value="',DbgStr(DefTempl.Value),'" CurPath="',CurPath,'" TempValue="',TempValue,'" EvalResult=',EvalResult);
          if DirDef.Values.ErrorPosition>=0 then begin
            FErrorDescription:=Format(ctsSyntaxErrorInExpr,[TempValue]);
            FErrorTemplate:=DefTempl;
            //debugln(['CalculateTemplate "',FErrorDescription,'"']);
          end else if EvalResult='1' then
            CalculateIfChildren;
        end;
      da_IfDef,da_IfNDef:
        // test if variable is defined
        begin
          //DebugLn('da_IfDef A Name=',DefTempl.Name,
          //  ' Variable=',DefTempl.Variable,
          //  ' Is=',dbgs(DirDef.Values.IsDefined(DefTempl.Variable)),
          //  ' CurPath="',CurPath,'"',
          //  ' Values.Count=',dbgs(DirDef.Values.Count));
          ReadValue(DirDef,DefTempl.Variable,CurPath,VarName);
          if DirDef.Values.IsDefined(VarName)=(DefTempl.Action=da_IfDef) then begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',true);
            CalculateIfChildren;
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,false,'',false,'',false);
          end;
        end;

      da_Else:
        // execute children
        begin
          if Assigned(OnCalculate) then
            OnCalculate(Self,DefTempl,false,'',false,'',true);
          CalculateTemplate(DefTempl.FirstChild,CurPath);
        end;

      da_Directory:
        begin
          // template for a sub directory
          ReadValue(DirDef,DefTempl.Value,CurPath,TempValue);
          // Note: CurPath can be ''
          SubPath:=AppendPathDelim(CurPath)+TempValue;
          // test if ExpandedDirectory is part of SubPath
          if (SubPath<>'') and FilenameIsMatching(SubPath,ExpandedDirectory,false)
          then begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,SubPath,false,'',true);
            CalculateTemplate(DefTempl.FirstChild,SubPath);
          end else begin
            if Assigned(OnCalculate) then
              OnCalculate(Self,DefTempl,true,SubPath,false,'',false);
          end;
        end;
      end;
      if ErrorTemplate<>nil then exit;
      if DefTempl<>nil then
        DefTempl:=DefTempl.Next;
    end;
  end;

// function TDefineTree.Calculate(DirDef: TDirectoryDefines): boolean;
begin
  {$IFDEF VerboseDefineCache}
  DebugLn('[TDefineTree.Calculate] ++++++ "',DirDef.Path,'"');
  {$ENDIF}
  Result:=true;
  FErrorTemplate:=nil;
  ExpandedDirectory:=DirDef.Path;
  if (ExpandedDirectory=VirtualDirectory)
  and Assigned(OnGetVirtualDirectoryAlias) then
    OnGetVirtualDirectoryAlias(Self,ExpandedDirectory);
  if (ExpandedDirectory<>VirtualDirectory) then begin
    ReadValue(DirDef,ExpandedDirectory,'',TempValue);
    ExpandedDirectory:=TempValue;
  end;
  DirDef.Values.Clear;
  // compute the result of all matching DefineTemplates
  CalculateTemplate(FFirstDefineTemplate,'');
  if (ExpandedDirectory=VirtualDirectory)
  and (Assigned(OnGetVirtualDirectoryDefines)) then
    OnGetVirtualDirectoryDefines(Self,DirDef);
  Result:=(ErrorTemplate=nil);
end;

procedure TDefineTree.IncreaseChangeStep;
begin
  CTIncreaseChangeStamp(FChangeStep);
  if DirectoryCachePool<>nil then DirectoryCachePool.IncreaseConfigTimeStamp;
end;

procedure TDefineTree.SetDirectoryCachePool(const AValue: TCTDirectoryCachePool);
begin
  if FDirectoryCachePool=AValue then exit;
  FDirectoryCachePool:=AValue;
end;

procedure TDefineTree.RemoveDoubles(Defines: TDirectoryDefines);
// use only one copy of each ansistring
begin
  if Defines=nil then exit;
  Defines.Values.RemoveDoubles(@FDefineStrings.ReplaceString);
end;

procedure TDefineTree.Add(ADefineTemplate: TDefineTemplate);
// add as last
var LastDefTempl: TDefineTemplate;
begin
  if ADefineTemplate=nil then exit;
  if RootTemplate=nil then
    RootTemplate:=ADefineTemplate
  else begin
    // add as last
    LastDefTempl:=RootTemplate;
    while LastDefTempl.Next<>nil do
      LastDefTempl:=LastDefTempl.Next;
    ADefineTemplate.InsertBehind(LastDefTempl);
  end;
  ClearCache;
end;

procedure TDefineTree.AddFirst(ADefineTemplate: TDefineTemplate);
// add as first
begin
  if ADefineTemplate=nil then exit;
  if RootTemplate=nil then
    RootTemplate:=ADefineTemplate
  else begin
    RootTemplate.InsertBehind(ADefineTemplate);
    RootTemplate:=ADefineTemplate;
  end;
  ClearCache;
end;

procedure TDefineTree.MoveToLast(ADefineTemplate: TDefineTemplate);
begin
  if (ADefineTemplate.Next=nil) and (ADefineTemplate.Parent=nil) then exit;
  ADefineTemplate.Unbind;
  if FFirstDefineTemplate=ADefineTemplate then FFirstDefineTemplate:=nil;
  Add(ADefineTemplate);
end;

function TDefineTree.FindDefineTemplateByName(
  const AName: string; OnlyRoots: boolean): TDefineTemplate;
begin
  Result:=RootTemplate;
  if RootTemplate<>nil then
    Result:=RootTemplate.FindByName(AName,not OnlyRoots,true)
  else
    Result:=nil;
end;

procedure TDefineTree.ReplaceRootSameName(const Name: string;
  ADefineTemplate: TDefineTemplate);
// if there is a DefineTemplate with the same name then replace it
// else add as last
var OldDefineTemplate: TDefineTemplate;
begin
  if (Name='') then exit;
  OldDefineTemplate:=FindDefineTemplateByName(Name,true);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(ADefineTemplate,true,false) then begin
      ClearCache;
    end;
    if ADefineTemplate<>nil then
      ADefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else
    Add(ADefineTemplate);
end;

procedure TDefineTree.RemoveRootDefineTemplateByName(const AName: string);
var ADefTempl: TDefineTemplate;
begin
  ADefTempl:=FindDefineTemplateByName(AName,true);
  if ADefTempl<>nil then RemoveDefineTemplate(ADefTempl);
end;

procedure TDefineTree.RemoveDefineTemplate(ADefTempl: TDefineTemplate);
var
  HadDefines: Boolean;
begin
  if ADefTempl=FFirstDefineTemplate then
    FFirstDefineTemplate:=FFirstDefineTemplate.Next;
  HadDefines:=ADefTempl.HasDefines(false,false);
  ADefTempl.Unbind;
  ADefTempl.Free;
  if HadDefines then ClearCache;
end;

procedure TDefineTree.ReplaceChild(ParentTemplate,
  NewDefineTemplate: TDefineTemplate; const ChildName: string);
// if there is a DefineTemplate with the same name then replace it
// else add as last
var OldDefineTemplate: TDefineTemplate;
begin
  if (ChildName='') or (ParentTemplate=nil) then exit;
  OldDefineTemplate:=ParentTemplate.FindChildByName(ChildName);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(NewDefineTemplate,true,false) then begin
      ClearCache;
    end;
    if NewDefineTemplate<>nil then
      NewDefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else begin
    ClearCache;
    ParentTemplate.AddChild(NewDefineTemplate);
  end;
end;

procedure TDefineTree.AddChild(ParentTemplate,
  NewDefineTemplate: TDefineTemplate);
begin
  ClearCache;
  ParentTemplate.AddChild(NewDefineTemplate);
end;

procedure TDefineTree.ReplaceRootSameName(ADefineTemplate: TDefineTemplate);
begin
  if (ADefineTemplate=nil) then exit;
  ReplaceRootSameName(ADefineTemplate.Name,ADefineTemplate);
end;

procedure TDefineTree.ReplaceRootSameNameAddFirst(
  ADefineTemplate: TDefineTemplate);
var OldDefineTemplate: TDefineTemplate;
begin
  if ADefineTemplate=nil then exit;
  OldDefineTemplate:=FindDefineTemplateByName(ADefineTemplate.Name,true);
  if OldDefineTemplate<>nil then begin
    if not OldDefineTemplate.IsEqual(ADefineTemplate,true,false) then begin
      ClearCache;
    end;
    ADefineTemplate.InsertBehind(OldDefineTemplate);
    if OldDefineTemplate=FFirstDefineTemplate then
      FFirstDefineTemplate:=FFirstDefineTemplate.Next;
    OldDefineTemplate.Unbind;
    OldDefineTemplate.Free;
  end else
    AddFirst(ADefineTemplate);
end;

procedure TDefineTree.MergeDefineTemplates(SourceTemplate: TDefineTemplate;
  const NewNamePrefix: string);
var
  LastDefTempl: TDefineTemplate;
begin
  if SourceTemplate=nil then exit;
  // import new defines
  LastDefTempl:=GetLastRootTemplate;
  TDefineTemplate.MergeTemplates(nil,FFirstDefineTemplate,LastDefTempl,
                                 SourceTemplate,true,NewNamePrefix);
  ClearCache;
end;

procedure TDefineTree.ConsistencyCheck;
begin
  if FFirstDefineTemplate<>nil then
    FFirstDefineTemplate.ConsistencyCheck;
  FCache.ConsistencyCheck;
end;

procedure TDefineTree.CalcMemSize(Stats: TCTMemStats);
var
  Node: TAVLTreeNode;
begin
  Stats.Add('TDefineTree',PtrUInt(InstanceSize)
    +MemSizeString(FErrorDescription)
    );
  if FMacroFunctions<>nil then
    Stats.Add('TDefineTree.FMacroFunctions',FMacroFunctions.CalcMemSize);
  if FMacroVariables<>nil then
    Stats.Add('TDefineTree.FMacroVariables',FMacroVariables.CalcMemSize);
  if FFirstDefineTemplate<>nil then
    FFirstDefineTemplate.CalcMemSize(Stats);
  if FVirtualDirCache<>nil then
    FVirtualDirCache.CalcMemSize(Stats);
  if FDefineStrings<>nil then
    Stats.Add('TDefineTree.FDefineStrings',FDefineStrings.CalcMemSize);
  if FCache<>nil then begin
    Stats.Add('TDefineTree.FCache.Count',FCache.Count);
    Node:=FCache.FindLowest;
    while Node<>nil do begin
      TDirectoryDefines(Node.Data).CalcMemSize(Stats);
      Node:=FCache.FindSuccessor(Node);
    end;
  end;
end;

procedure TDefineTree.WriteDebugReport;
begin
  DebugLn('TDefineTree.WriteDebugReport');
  if FFirstDefineTemplate<>nil then
    FFirstDefineTemplate.WriteDebugReport(false)
  else
    DebugLn('  No templates defined');
  DebugLn(FCache.ReportAsString);
  DebugLn('');
  ConsistencyCheck;
end;

    
{ TDefinePool }

constructor TDefinePool.Create;
begin
  inherited Create;
  FItems:=TFPList.Create;
end;

destructor TDefinePool.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TDefinePool.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do begin
    Items[i].Clear(true);
    Items[i].Free;
  end;
  FItems.Clear;
end;

function TDefinePool.GetItems(Index: integer): TDefineTemplate;
begin
  Result:=TDefineTemplate(FItems[Index]);
end;

procedure TDefinePool.SetEnglishErrorMsgFilename(const AValue: string);
begin
  if FEnglishErrorMsgFilename=AValue then exit;
  FEnglishErrorMsgFilename:=AValue;
end;

function TDefinePool.CheckAbort(ProgressID, MaxIndex: integer;
  const Msg: string): boolean;
begin
  Result:=false;
  if Assigned(OnProgress) then
    OnProgress(Self,ProgressID,MaxIndex,Msg,Result);
end;

procedure TDefinePool.Add(ADefineTemplate: TDefineTemplate);
begin
  if ADefineTemplate<>nil then
    FItems.Add(ADefineTemplate);
end;

procedure TDefinePool.Insert(Index: integer; ADefineTemplate: TDefineTemplate);
begin
  FItems.Insert(Index,ADefineTemplate);
end;

procedure TDefinePool.Delete(Index: integer);
begin
  Items[Index].Clear(true);
  Items[Index].Free;
  FItems.Delete(Index);
end;

procedure TDefinePool.Move(SrcIndex, DestIndex: integer);
begin
  FItems.Move(SrcIndex,DestIndex);
end;

function TDefinePool.Count: integer;
begin
  Result:=FItems.Count;
end;

function TDefinePool.CreateFPCTemplate(
  const CompilerPath, CompilerOptions, TestPascalFile: string;
  out UnitSearchPath, TargetOS, aTargetCPU: string;
  Owner: TObject): TDefineTemplate;
// create symbol definitions for the freepascal compiler
// To get reliable values the compiler itself is asked for
var
  LastDefTempl: TDefineTemplate;

  procedure AddTemplate(NewDefTempl: TDefineTemplate);
  begin
    if NewDefTempl=nil then exit;
    if LastDefTempl<>nil then
      NewDefTempl.InsertBehind(LastDefTempl);
    LastDefTempl:=NewDefTempl;
  end;
  
  function FindSymbol(const SymbolName: string): TDefineTemplate;
  begin
    Result:=LastDefTempl;
    while (Result<>nil)
    and (Comparetext(Result.Variable,SymbolName)<>0) do
      Result:=Result.Prior;
  end;

  procedure DefineSymbol(const SymbolName, SymbolValue: string;
    const Description: string = '');
  var NewDefTempl: TDefineTemplate;
    Desc: String;
  begin
    NewDefTempl:=FindSymbol(SymbolName);
    if NewDefTempl=nil then begin
      if Description<>'' then
        Desc:=Description
      else
        Desc:=ctsDefaultFPCSymbol;
      NewDefTempl:=TDefineTemplate.Create('Define '+SymbolName,
           Desc,SymbolName,SymbolValue,da_DefineRecurse);
      AddTemplate(NewDefTempl);
    end else begin
      NewDefTempl.Value:=SymbolValue;
    end;
  end;

  procedure UndefineSymbol(const SymbolName: string);
  var
    ADefTempl: TDefineTemplate;
  begin
    ADefTempl:=FindSymbol(SymbolName);
    if ADefTempl=nil then exit;
    if LastDefTempl=ADefTempl then LastDefTempl:=ADefTempl.Prior;
    ADefTempl.Unbind;
    ADefTempl.Free;
  end;

  procedure ProcessOutputLine(var Line: string);
  var
    SymbolName, SymbolValue, UpLine, NewPath: string;
    i, len, curpos: integer;
  begin
    len := length(Line);
    if len <= 6 then Exit; // shortest match
    
    CurPos := 1;
    // strip timestamp e.g. [0.306]
    if Line[CurPos] = '[' then begin
      repeat
        inc(CurPos);
        if CurPos > len then Exit;
      until line[CurPos] = ']';
      Inc(CurPos, 2); // skip space too
      if len - CurPos < 6 then Exit; // shortest match
    end;

    UpLine:=UpperCaseStr(Line);
    //DebugLn(['ProcessOutputLine ',Line]);
    
    case UpLine[CurPos] of
      'M':
        if StrLComp(@UpLine[CurPos], 'MACRO ', 6) = 0 then begin
          // no macro
          Inc(CurPos, 6);

          if (StrLComp(@UpLine[CurPos], 'DEFINED: ', 9) = 0) then begin
            Inc(CurPos, 9);
            SymbolName:=copy(UpLine, CurPos, len);
            DefineSymbol(SymbolName,'');
            Exit;
          end;

          if (StrLComp(@UpLine[CurPos], 'UNDEFINED: ', 11) = 0) then begin
            Inc(CurPos, 11);
            SymbolName:=copy(UpLine,CurPos,len);
            UndefineSymbol(SymbolName);
            Exit;
          end;

          // MACRO something...
          i := CurPos;
          while (i <= len) and (Line[i]<>' ') do inc(i);
          SymbolName:=copy(UpLine,CurPos,i-CurPos);
          CurPos := i + 1; // skip space

          if StrLComp(@UpLine[CurPos], 'SET TO ', 7) = 0 then begin
            Inc(CurPos, 7);
            SymbolValue:=copy(Line, CurPos, len);
            DefineSymbol(SymbolName, SymbolValue);
          end;
        end;
      'U':
        if (StrLComp(@UpLine[CurPos], 'USING UNIT PATH: ', 17) = 0) then begin
          Inc(CurPos, 17);
          NewPath:=copy(Line,CurPos,len);
          if not FilenameIsAbsolute(NewPath) then
            NewPath:=ExpandFileNameUTF8(NewPath);
          {$IFDEF VerboseFPCSrcScan}
          DebugLn('Using unit path: "',NewPath,'"');
          {$ENDIF}
          UnitSearchPath:=UnitSearchPath+NewPath+';';
        end;
    end;
  end;
  
var
  i, OutLen, LineStart: integer;
  TheProcess: TProcessUTF8;
  OutputLine, Buf: String;
  NewDefTempl: TDefineTemplate;
  SrcOS: string;
  SrcOS2: String;
  Step: String;
  Params: TStringListUTF8;
begin
  Result:=nil;
  //DebugLn('TDefinePool.CreateFPCTemplate PPC386Path="',CompilerPath,'" FPCOptions="',CompilerOptions,'"');
  if TestPascalFile='' then begin
    DebugLn(['Warning: [TDefinePool.CreateFPCTemplate] TestPascalFile empty']);
  end;
  UnitSearchPath:='';
  TargetOS:='';
  SrcOS:='';
  aTargetCPU:='';
  if (CompilerPath='') or (not FileIsExecutable(CompilerPath)) then exit;
  LastDefTempl:=nil;
  // find all initial compiler macros and all unit paths
  // -> ask compiler with the -va switch
  SetLength(Buf,1024);
  Step:='Init';
  try
    Params:=TStringListUTF8.Create;
    TheProcess := TProcessUTF8.Create(nil);
    try
      TheProcess.Executable:=CompilerPath;
      Params.Add('-va');
      if (Pos('pas2js',lowercase(ExtractFileName(CompilerPath)))<1)
          and FileExistsCached(EnglishErrorMsgFilename) then
          Params.Add('-Fr'+EnglishErrorMsgFilename);
      if CompilerOptions<>'' then
        SplitCmdLineParams(CompilerOptions,Params,true);
      Params.Add(TestPascalFile);
      //DebugLn('TDefinePool.CreateFPCTemplate Params="',MergeCmdLineParams(Params),'"');
      TheProcess.Parameters:=Params;
      TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
      TheProcess.ShowWindow := swoHide;
      Step:='Running '+MergeCmdLineParams(Params);
      TheProcess.Execute;
      OutputLine:='';
      repeat
        if (TheProcess.Output<>nil) then begin
          OutLen:=TheProcess.Output.Read(Buf[1],length(Buf));
        end else
          OutLen:=0;
        LineStart:=1;
        i:=1;
        while i<=OutLen do begin
          if Buf[i] in [#10,#13] then begin
            OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
            ProcessOutputLine(OutputLine);
            OutputLine:='';
            if (i<OutLen) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
            then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=copy(Buf,LineStart,OutLen-LineStart+1);
      until OutLen=0;
      TheProcess.WaitOnExit;
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -va: OutputLine="',OutputLine,'"');
      TheProcess.Free;
      Params.Free;
    end;
    DefineSymbol(FPCUnitPathMacroName,UnitSearchPath,'FPC default unit search path');

    //DebugLn('TDefinePool.CreateFPCTemplate First done UnitSearchPath="',UnitSearchPath,'"');

    // ask for target operating system -> ask compiler with switch -iTO
    Params:=TStringListUTF8.Create;
    TheProcess := TProcessUTF8.Create(nil);
    try
      TheProcess.Executable:=CompilerPath;
      if CompilerOptions<>'' then
        SplitCmdLineParams(CompilerOptions,Params,true);
      Params.Add('-iTO');
      TheProcess.Parameters:=Params;
      TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
      TheProcess.ShowWindow := swoHide;
      Step:='Running '+MergeCmdLineParams(Params);
      TheProcess.Execute;
      if (TheProcess.Output<>nil) then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          // define #TargetOS
          TargetOS:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetOS',
            ctsDefaultFPCTargetOperatingSystem,
            ExternalMacroStart+'TargetOS',TargetOS,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          // define #SrcOS
          SrcOS:=GetDefaultSrcOSForTargetOS(TargetOS);
          if SrcOS='' then SrcOS:=TargetOS;
          NewDefTempl:=TDefineTemplate.Create('Define SrcOS',
            ctsDefaultFPCSourceOperatingSystem,
            ExternalMacroStart+'SrcOS',SrcOS,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          // define #SrcOS2
          SrcOS2:=GetDefaultSrcOS2ForTargetOS(TargetOS);
          if SrcOS2='' then SrcOS2:=TargetOS;
          NewDefTempl:=TDefineTemplate.Create('Define SrcOS2',
            ctsDefaultFPCSource2OperatingSystem,
            ExternalMacroStart+'SrcOS2',SrcOS2,da_DefineRecurse);
          AddTemplate(NewDefTempl);
          break;
        end;
        inc(i);
      end;
      TheProcess.WaitOnExit;
      //DebugLn('TDefinePool.CreateFPCTemplate target OS done');
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -iTO: OutputLine="',OutputLine,'"');
      TheProcess.Free;
      Params.Free;
    end;
    
    // ask for target processor -> ask compiler with switch -iTP
    Params:=TStringListUTF8.Create;
    TheProcess := TProcessUTF8.Create(nil);
    try
      TheProcess.Executable:=CompilerPath;
      if CompilerOptions<>'' then
        SplitCmdLineParams(CompilerOptions,Params,true);
      Params.Add('-iTP');
      TheProcess.Parameters:=Params;
      TheProcess.Options:= [poUsePipes, poStdErrToOutPut];
      TheProcess.ShowWindow := swoHide;
      Step:='Running '+MergeCmdLineParams(Params);
      TheProcess.Execute;
      if TheProcess.Output<>nil then
        OutLen:=TheProcess.Output.Read(Buf[1],length(Buf))
      else
        OutLen:=0;
      i:=1;
      while i<=OutLen do begin
        if Buf[i] in [#10,#13] then begin
          aTargetCPU:=copy(Buf,1,i-1);
          NewDefTempl:=TDefineTemplate.Create('Define TargetCPU',
            ctsDefaultFPCTargetProcessor,
            TargetCPUMacroName,aTargetCPU,
            da_DefineRecurse);
          AddTemplate(NewDefTempl);
          break;
        end;
        inc(i);
      end;
      TheProcess.WaitOnExit;
      //DebugLn('TDefinePool.CreateFPCTemplate target CPU done');
    finally
      //DebugLn('TDefinePool.CreateFPCTemplate Run with -iTP: OutputLine="',OutputLine,'"');
      TheProcess.Free;
    end;

    // add
    if (LastDefTempl<>nil) then begin
      Result:=TDefineTemplate.Create(StdDefTemplFPC,
        ctsFreePascalCompilerInitialMacros,'','',da_Block);
      Result.AddChild(LastDefTempl.GetFirstSibling);
      Result.SetFlags([dtfAutoGenerated],[],false);
      //DebugLn('TDefinePool.CreateFPCTemplate FPC defines done');
    end;
  except
    on E: Exception do begin
      DebugLn('Error: TDefinePool.CreateFPCTemplate (',Step,'): ',E.Message);
    end;
  end;
  if Result<>nil then
    Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.GetFPCVerFromFPCTemplate(Template: TDefineTemplate; out
  FPCVersion, FPCRelease, FPCPatch: integer): boolean;
var
  p: Integer;

  function ReadInt(const VarName: string; out AnInteger: integer): boolean;
  var
    StartPos: Integer;
  begin
    StartPos:=p;
    AnInteger:=0;
    while (p<=length(VarName)) and (VarName[p] in ['0'..'9']) do begin
      AnInteger:=AnInteger*10+(ord(VarName[p])-ord('0'));
      if AnInteger>=100 then begin
        Result:=false;
        exit;
      end;
      inc(p);
    end;
    Result:=StartPos<p;
  end;

  function ReadVersion(const VarName: string;
    out NewVersion, NewRelease, NewPatch: integer): integer;
  begin
    Result:=0;
    if (length(VarName)>3) and (VarName[1] in ['V','v'])
    and (VarName[2] in ['E','e']) and (VarName[3] in ['R','r'])
    and (VarName[4] in ['0'..'9']) then begin
      p:=4;
      if not ReadInt(VarName,NewVersion) then exit;
      inc(Result);
      if (p>=length(VarName)) or (VarName[p]<>'_') then exit;
      inc(p);
      if not ReadInt(VarName,NewRelease) then exit;
      inc(Result);
      if (p>=length(VarName)) or (VarName[p]<>'_') then exit;
      inc(p);
      if not ReadInt(VarName,NewPatch) then exit;
      inc(Result);
    end;
  end;

var
  Def: TDefineTemplate;
  VarName: String;
  BestCount: integer;
  NewCount: LongInt;
  NewVersion: integer;
  NewRelease: integer;
  NewPatch: integer;
begin
  Result:=false;
  FPCVersion:=0;
  FPCRelease:=0;
  FPCPatch:=0;
  BestCount:=0;
  Def:=Template;
  while Def<>nil do begin
    if Def.Action in [da_Define,da_DefineRecurse] then begin
      VarName:=Def.Variable;
      NewCount:=ReadVersion(VarName,NewVersion,NewRelease,NewPatch);
      if NewCount>BestCount then begin
        BestCount:=NewCount;
        FPCVersion:=NewVersion;
        if NewCount>1 then FPCRelease:=NewRelease;
        if NewCount>2 then FPCPatch:=NewPatch;
        if NewCount=3 then exit;
      end;
    end;
    Def:=Def.Next;
  end;
end;

function TDefinePool.CreateDelphiSrcPath(DelphiVersion: integer;
  const PathPrefix: string): string;
begin
  case DelphiVersion of
  1..5:
    Result:=PathPrefix+'Source/Rtl/Win;'
      +PathPrefix+'Source/Rtl/Sys;'
      +PathPrefix+'Source/Rtl/Corba;'
      +PathPrefix+'Source/Vcl;';
  else
    // 6 and above
    Result:=PathPrefix+'Source/Rtl/Win;'
      +PathPrefix+'Source/Rtl/Sys;'
      +PathPrefix+'Source/Rtl/Common;'
      +PathPrefix+'Source/Rtl/Corba40;'
      +PathPrefix+'Source/Vcl;';
  end;
end;

function TDefinePool.CreateLazarusSrcTemplate(
  const LazarusSrcDir, WidgetType, ExtraOptions: string;
  Owner: TObject): TDefineTemplate;

  function D(const Filename: string): string;
  begin
    Result:=GetForcedPathDelims(Filename);
  end;
    
var
  MainDir, DirTempl, SubDirTempl, IfTemplate,
  SubTempl: TDefineTemplate;
  TargetOS, SrcOS, SrcPath: string;
  i: Integer;
  CurCPU, CurOS, CurWidgetSet: string;
  ToolsInstallDirTempl: TDefineTemplate;
  AllWidgetSets: String;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') then exit;
  TargetOS:='$('+ExternalMacroStart+'TargetOS)';
  SrcOS:='$('+ExternalMacroStart+'SrcOS)';
  SrcPath:='$('+ExternalMacroStart+'SrcPath)';

  AllWidgetSets:='';
  for i:=Low(Lazarus_CPU_OS_Widget_Combinations)
      to High(Lazarus_CPU_OS_Widget_Combinations) do
  begin
    SplitLazarusCPUOSWidgetCombo(Lazarus_CPU_OS_Widget_Combinations[i],
                                 CurCPU,CurOS,CurWidgetSet);
    if not HasDelimitedItem(AllWidgetSets,';',CurWidgetSet) then begin
      if AllWidgetSets<>'' then
        AllWidgetSets:=AllWidgetSets+';';
      AllWidgetSets:=AllWidgetSets+CurWidgetSet;
    end;
  end;

  // <LazarusSrcDir>
  MainDir:=TDefineTemplate.Create(
    StdDefTemplLazarusSrcDir, ctsDefsForLazarusSources,'',LazarusSrcDir,
    da_Directory);
  // clear src path
  MainDir.AddChild(TDefineTemplate.Create('Clear SrcPath','Clear SrcPath',
    ExternalMacroStart+'SrcPath','',da_DefineRecurse));
  // if SrcOS<>win
  IfTemplate:=TDefineTemplate.Create('IF '''+SrcOS+'''<>''win''',
    ctsIfTargetOSIsNotWin32,'',''''+SrcOS+'''<>''win''',da_If);
    // then define #SrcPath := #SrcPath;lcl/nonwin32
    IfTemplate.AddChild(TDefineTemplate.Create('win32api for non win',
      Format(ctsAddsDirToSourcePath,[d(LazarusSrcDir+'/lcl/nonwin32')]),
      ExternalMacroStart+'SrcPath',
      d(LazarusSrcDir+'/lcl/nonwin32;')+SrcPath,da_DefineRecurse));
  MainDir.AddChild(IfTemplate);
  // define 'LCL'
  MainDir.AddChild(TDefineTemplate.Create('define LCL',
    ctsDefineLCL,'LCL',WidgetType,da_DefineRecurse));
  // define LCLwidgetset, e.g. LCLcarbon, LCLgtk, LCLgtk2
  MainDir.AddChild(TDefineTemplate.Create('Define LCLwidgettype',
    ctsDefineLCLWidgetset,
    'LCL$(#LCLWidgetType)','',da_DefineRecurse));

  // <LazarusSrcDir>/ide
  DirTempl:=TDefineTemplate.Create('ide',ctsIDEDirectory,
    '','ide',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('IDE path addition',
    Format(ctsAddsDirToSourcePath,['designer, debugger, synedit, ...']),
    ExternalMacroStart+'SrcPath',
      d(LazarusSrcDir+'/ide;'
       +LazarusSrcDir+'/ide/frames;'
       +LazarusSrcDir+'/designer;'
       +LazarusSrcDir+'/debugger;'
       +LazarusSrcDir+'/debugger/frames;'
       +LazarusSrcDir+'/converter;'
       +LazarusSrcDir+'/packager;'
       +LazarusSrcDir+'/packager/registration;'
       +LazarusSrcDir+'/packager/frames;'
       +LazarusSrcDir+'/components/ideintf;'
       +LazarusSrcDir+'/components/lazutils;'
       +LazarusSrcDir+'/components/lazcontrols;'
       +LazarusSrcDir+'/components/synedit;'
       +LazarusSrcDir+'/components/codetools;'
       +LazarusSrcDir+'/components/lazdebuggergdbmi;'
       +LazarusSrcDir+'/components/debuggerintf;'
       +LazarusSrcDir+'/lcl;'
       +LazarusSrcDir+'/lcl/interfaces;'
       +LazarusSrcDir+'/lcl/interfaces/'+WidgetType+';'
       +LazarusSrcDir+'/components/custom;'
       +SrcPath)
    ,da_DefineRecurse));
  // include path addition
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsSetsIncPathTo,['include, include/TargetOS, include/SrcOS']),
    IncludePathMacroName,
    d(LazarusSrcDir+'/ide/include;'
      +LazarusSrcDir+'/ide/include/'+TargetOS+';'
      +LazarusSrcDir+'/ide/include/'+SrcOS),
    da_DefineRecurse));
  MainDir.AddChild(DirTempl);

  // <LazarusSrcDir>/designer
  DirTempl:=TDefineTemplate.Create('Designer',ctsDesignerDirectory,
    '','designer',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('components path addition',
    Format(ctsAddsDirToSourcePath,['synedit']),
    SrcPathMacroName,
      d('../components/lazutils'
       +';../components/codetools'
       +';../lcl'
       +';../lcl/interfaces'
       +';../lcl/interfaces/'+WidgetType
       +';../components/ideintf'
       +';../components/synedit'
       +';../components/codetools'
       +';../components/lazcontrols'
       +';../components/custom')
       +';'+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('main path addition',
    Format(ctsAddsDirToSourcePath,[ctsLazarusMainDirectory]),
    SrcPathMacroName,
    d('../ide;../packager;')+SrcPath
    ,da_Define));
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsIncludeDirectoriesPlusDirs,['include']),
    IncludePathMacroName,
    d('../ide/include;../ide/include/'+TargetOS),
    da_Define));
  // <LazarusSrcDir>/designer/units
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/images


  // <LazarusSrcDir>/debugger
  DirTempl:=TDefineTemplate.Create('Debugger',ctsDebuggerDirectory,
    '','debugger',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    ExternalMacroStart+'SrcPath',
      d(LazarusSrcDir+'/debugger;'
       +LazarusSrcDir+'/debugger/frames;'
       +LazarusSrcDir+'/ide;'
       +LazarusSrcDir+'/components/ideintf;'
       +LazarusSrcDir+'/components/lazutils;'
       +LazarusSrcDir+'/components/codetools;'
       +LazarusSrcDir+'/components/lazdebuggergdbmi;'
       +LazarusSrcDir+'/components/debuggerintf;'
       +LazarusSrcDir+'/lcl;'
       +LazarusSrcDir+'/lcl/interfaces;'
       +LazarusSrcDir+'/lcl/interfaces/'+WidgetType+';')
       +SrcPath
    ,da_DefineRecurse));
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/converter
  DirTempl:=TDefineTemplate.Create('Converter',ctsDebuggerDirectory,
    '','converter',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl, components']),
    ExternalMacroStart+'SrcPath',
      d('../ide'
       +';../components/ideintf'
       +';../components/lazutils'
       +';../components/codetools'
       +';../components/synedit'
       +';../components/lazcontrols'
       +';../packager'
       +';../debugger'
       +';../designer'
       +';../lcl'
       +';../lcl/interfaces'
       +';../lcl/interfaces/'+WidgetType)
       +';'+SrcPath
    ,da_Define));
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/packager
  DirTempl:=TDefineTemplate.Create('Packager',ctsDesignerDirectory,
    '','packager',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('src path addition',
    Format(ctsAddsDirToSourcePath,['lcl synedit codetools lazcontrols ideintf']),
    SrcPathMacroName,
      d(LazarusSrcDir+'/lcl'
      +';'+LazarusSrcDir+'/lcl/interfaces'
      +';'+LazarusSrcDir+'/lcl/interfaces/'+WidgetType
      +';'+LazarusSrcDir+'/ide'
      +';'+LazarusSrcDir+'/components/ideintf'
      +';'+LazarusSrcDir+'/components/synedit'
      +';'+LazarusSrcDir+'/components/lazcontrols'
      +';'+LazarusSrcDir+'/components/lazutils'
      +';'+LazarusSrcDir+'/components/codetools'
      +';'+LazarusSrcDir+'/packager/frames'
      +';'+LazarusSrcDir+'/packager/registration'
      +';'+SrcPath)
    ,da_DefineRecurse));
  DirTempl.AddChild(TDefineTemplate.Create('includepath addition',
    Format(ctsIncludeDirectoriesPlusDirs,['include']),
    IncludePathMacroName,
    d('../ide/include;../ide/include/'+TargetOS),
    da_Define));
  // <LazarusSrcDir>/packager/frames
  SubDirTempl:=TDefineTemplate.Create('Frames',
    'Frames','','frames',da_Directory);
  DirTempl.AddChild(SubDirTempl);
  SubDirTempl.AddChild(TDefineTemplate.Create('src path addition',
    Format(ctsAddsDirToSourcePath,['ide']),
    SrcPathMacroName,
    d(LazarusSrcDir+'/ide;'+LazarusSrcDir+'/packager;'+SrcPath)
    ,da_Define));
  // <LazarusSrcDir>/packager/registration
  SubDirTempl:=TDefineTemplate.Create('Registration',
    ctsPackagerRegistrationDirectory,'','registration',da_Directory);
  DirTempl.AddChild(SubDirTempl);
  // <LazarusSrcDir>/packager/units
  SubDirTempl:=TDefineTemplate.Create('Packager Units',
    ctsPackagerUnitsDirectory,'','units',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('CompiledSrcPath',
    ctsCompiledSrcPath,CompiledSrcPathMacroName,
    LazarusSrcDir+d('/packager/registration'),
    da_DefineRecurse));
  DirTempl.AddChild(SubDirTempl);
  MainDir.AddChild(DirTempl);


  // <LazarusSrcDir>/examples
  DirTempl:=TDefineTemplate.Create('Examples',
    Format(ctsNamedDirectory,['Examples']),
    '','examples',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
      d('../lcl'
      +';../lcl/interfaces/'+WidgetType+';'+SrcPath)
    ,da_Define));
  MainDir.AddChild(DirTempl);

  // <LazarusSrcDir>/components
  DirTempl:=TDefineTemplate.Create('Components',ctsComponentsDirectory,
    '','components',da_Directory);

  // <LazarusSrcDir>/components/custom
  SubDirTempl:=TDefineTemplate.Create('Custom Components',
    ctsCustomComponentsDirectory,
    '','custom',da_Directory);
  SubDirTempl.AddChild(TDefineTemplate.Create('lazarus standard components',
    Format(ctsAddsDirToSourcePath,['synedit']),
    ExternalMacroStart+'SrcPath',
    d('../synedit;')
    +SrcPath
    ,da_DefineRecurse));
  DirTempl.AddChild(SubDirTempl);
  MainDir.AddChild(DirTempl);

  // <LazarusSrcDir>/tools
  DirTempl:=TDefineTemplate.Create('Tools',
    ctsToolsDirectory,'','tools',da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
    d('../lcl;../lcl/interfaces/'+WidgetType
    +';../components/codetools')
    +';'+SrcPath
    ,da_Define));
    // <LazarusSrcDir>/tools/install
    ToolsInstallDirTempl:=TDefineTemplate.Create('Install',
      ctsInstallDirectory,'','install',da_Directory);
    DirTempl.AddChild(ToolsInstallDirTempl);
    ToolsInstallDirTempl.AddChild(TDefineTemplate.Create('LCL path addition',
      Format(ctsAddsDirToSourcePath,['lcl']),
      ExternalMacroStart+'SrcPath',
      d('../../lcl;../../lcl/interfaces/'+WidgetType
      +';../../components/codetools')
      +';'+SrcPath
      ,da_Define));
  MainDir.AddChild(DirTempl);

  // extra options
  SubTempl:=CreateFPCCommandLineDefines(StdDefTemplLazarusBuildOpts,
                                        ExtraOptions,true,Owner);
  MainDir.AddChild(SubTempl);

  // put it all into a block
  if MainDir<>nil then begin
    Result:=TDefineTemplate.Create(StdDefTemplLazarusSources,
       ctsLazarusSources,'','',da_Block);
    Result.AddChild(MainDir);
  end;
  
  Result.SetDefineOwner(Owner,true);
  Result.SetFlags([dtfAutoGenerated],[],false);
end;

function TDefinePool.CreateLCLProjectTemplate(
  const LazarusSrcDir, WidgetType, ProjectDir: string;
  Owner: TObject): TDefineTemplate;
var DirTempl: TDefineTemplate;
begin
  Result:=nil;
  if (LazarusSrcDir='') or (WidgetType='') or (ProjectDir='') then exit;
  DirTempl:=TDefineTemplate.Create('ProjectDir',ctsAnLCLProject,
    '',ProjectDir,da_Directory);
  DirTempl.AddChild(TDefineTemplate.Create('LCL',
    Format(ctsAddsDirToSourcePath,['lcl']),
    ExternalMacroStart+'SrcPath',
    LazarusSrcDir+PathDelim+'lcl;'
     +LazarusSrcDir+PathDelim+'lcl'+PathDelim+'interfaces'
     +PathDelim+WidgetType
     +';$('+ExternalMacroStart+'SrcPath)'
    ,da_DefineRecurse));
  Result:=TDefineTemplate.Create(StdDefTemplLCLProject,
       'LCL Project','','',da_Block);
  Result.AddChild(DirTempl);
  Result.SetDefineOwner(Owner,true);
  Result.SetFlags([dtfAutoGenerated],[],false);
end;

function TDefinePool.CreateDelphiCompilerDefinesTemplate(
  DelphiVersion: integer; Owner: TObject): TDefineTemplate;
var
  DefTempl: TDefineTemplate;
  VerMacro: String;
begin
  DefTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)
      +' Compiler Defines',
      Format(ctsOtherCompilerDefines,['Delphi'+IntToStr(DelphiVersion)]),
      '','',da_Block);
  DefTempl.AddChild(TDefineTemplate.Create('Reset',
      ctsResetAllDefines,
      '','',da_UndefineAll));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro DELPHI',
      Format(ctsDefineMacroName,['DELPHI']),
      'DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro FPC_DELPHI',
      Format(ctsDefineMacroName,['FPC_DELPHI']),
      'FPC_DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro MSWINDOWS',
      Format(ctsDefineMacroName,['MSWINDOWS']),
      'MSWINDOWS','',da_DefineRecurse));

  // version
  case DelphiVersion of
  3: VerMacro:='VER_110';
  4: VerMacro:='VER_125';
  5: VerMacro:='VER_130';
  6: VerMacro:='VER_140';
  else
    // else define Delphi 7
    VerMacro:='VER_150';
  end;
  DefTempl.AddChild(TDefineTemplate.Create('Define macro '+VerMacro,
      Format(ctsDefineMacroName,[VerMacro]),
      VerMacro,'',da_DefineRecurse));

  DefTempl.AddChild(TDefineTemplate.Create(
     Format(ctsDefineMacroName,[ExternalMacroStart+'Compiler']),
     'Define '+ExternalMacroStart+'Compiler variable',
     ExternalMacroStart+'Compiler','DELPHI',da_DefineRecurse));

  Result:=DefTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateDelphiDirectoryTemplate(
  const DelphiDirectory: string; DelphiVersion: integer;
  Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)
     +' Directory',
     Format(ctsNamedDirectory,['Delphi'+IntToStr(DelphiVersion)]),
     '',DelphiDirectory,da_Directory);
  MainDirTempl.AddChild(CreateDelphiCompilerDefinesTemplate(DelphiVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsSetsSrcPathTo,['RTL, VCL']),
      ExternalMacroStart+'SrcPath',
      GetForcedPathDelims(
          CreateDelphiSrcPath(DelphiVersion,DefinePathMacro+'/')+'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateDelphiProjectTemplate(
  const ProjectDir, DelphiDirectory: string;
  DelphiVersion: integer; Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Delphi'+IntToStr(DelphiVersion)+' Project',
     Format(ctsNamedProject,['Delphi'+IntToStr(DelphiVersion)]),
     '',ProjectDir,da_Directory);
  MainDirTempl.AddChild(
    CreateDelphiCompilerDefinesTemplate(DelphiVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create(
     'Define '+ExternalMacroStart+'DelphiDir',
     Format(ctsDefineMacroName,[ExternalMacroStart+'DelphiDir']),
     ExternalMacroStart+'DelphiDir',DelphiDirectory,da_DefineRecurse));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsAddsDirToSourcePath,['Delphi RTL+VCL']),
      ExternalMacroStart+'SrcPath',
      GetForcedPathDelims(CreateDelphiSrcPath(DelphiVersion,'$(#DelphiDir)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixCompilerDefinesTemplate(KylixVersion: integer;
  Owner: TObject): TDefineTemplate;
var
  DefTempl: TDefineTemplate;
begin
  DefTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)
      +' Compiler Defines',
      Format(ctsOtherCompilerDefines,['Kylix'+IntToStr(KylixVersion)]),
      '','',da_Block);
  DefTempl.AddChild(TDefineTemplate.Create('Reset',
      ctsResetAllDefines,
      '','',da_UndefineAll));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro KYLIX',
      Format(ctsDefineMacroName,['KYLIX']),
      'KYLIX','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro FPC_DELPHI',
      Format(ctsDefineMacroName,['FPC_DELPHI']),
      'FPC_DELPHI','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro LINUX',
      Format(ctsDefineMacroName,['LINUX']),
      'LINUX','',da_DefineRecurse));
  DefTempl.AddChild(TDefineTemplate.Create('Define macro CPU386',
      Format(ctsDefineMacroName,['CPU386']),
      'CPU386','',da_DefineRecurse));

  // version
  case KylixVersion of
  1:
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_125',
        Format(ctsDefineMacroName,['VER_125']),
        'VER_125','',da_DefineRecurse));
  2:
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_130',
        Format(ctsDefineMacroName,['VER_130']),
        'VER_130','',da_DefineRecurse));
  else
    // else define Kylix 3
    DefTempl.AddChild(TDefineTemplate.Create('Define macro VER_140',
        Format(ctsDefineMacroName,['VER_140']),
        'VER_140','',da_DefineRecurse));
  end;

  DefTempl.AddChild(TDefineTemplate.Create(
     Format(ctsDefineMacroName,[ExternalMacroStart+'Compiler']),
     'Define '+ExternalMacroStart+'Compiler variable',
     ExternalMacroStart+'Compiler','DELPHI',da_DefineRecurse));

  Result:=DefTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixSrcPath(KylixVersion: integer;
  const PathPrefix: string): string;
begin
  Result:=PathPrefix+'source/rtl/linux;'
    +PathPrefix+'source/rtl/sys;'
    +PathPrefix+'source/rtl/common;'
    +PathPrefix+'source/rtl/corba40;'
    +PathPrefix+'source/rtle;'
    +PathPrefix+'source/rtl/clx';
end;

function TDefinePool.CreateKylixDirectoryTemplate(const KylixDirectory: string;
  KylixVersion: integer; Owner: TObject): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)
     +' Directory',
     Format(ctsNamedDirectory,['Kylix'+IntToStr(KylixVersion)]),
     '',KylixDirectory,da_Directory);
  MainDirTempl.AddChild(CreateKylixCompilerDefinesTemplate(KylixVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsSetsSrcPathTo,['RTL, CLX']),
      ExternalMacroStart+'SrcPath',
      GetForcedPathDelims(CreateKylixSrcPath(KylixVersion,DefinePathMacro+'/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateKylixProjectTemplate(const ProjectDir,
  KylixDirectory: string; KylixVersion: integer; Owner: TObject
  ): TDefineTemplate;
var MainDirTempl: TDefineTemplate;
begin
  MainDirTempl:=TDefineTemplate.Create('Kylix'+IntToStr(KylixVersion)+' Project',
     Format(ctsNamedProject,['Kylix'+IntToStr(KylixVersion)]),
     '',ProjectDir,da_Directory);
  MainDirTempl.AddChild(
    CreateDelphiCompilerDefinesTemplate(KylixVersion,Owner));
  MainDirTempl.AddChild(TDefineTemplate.Create(
     'Define '+ExternalMacroStart+'KylixDir',
     Format(ctsDefineMacroName,[ExternalMacroStart+'KylixDir']),
     ExternalMacroStart+'KylixDir',KylixDirectory,da_DefineRecurse));
  MainDirTempl.AddChild(TDefineTemplate.Create('SrcPath',
      Format(ctsAddsDirToSourcePath,['Kylix RTL+VCL']),
      ExternalMacroStart+'SrcPath',
      GetForcedPathDelims(CreateKylixSrcPath(KylixVersion,'$(#KylixDir)/')
                       +'$(#SrcPath)'),
      da_DefineRecurse));

  Result:=MainDirTempl;
  Result.SetDefineOwner(Owner,true);
end;

function TDefinePool.CreateFPCCommandLineDefines(const Name, CmdLine: string;
  RecursiveDefines: boolean; Owner: TObject; AlwaysCreate: boolean;
  AddPaths: boolean): TDefineTemplate;

  procedure CreateMainTemplate;
  begin
    if Result=nil then
      Result:=TDefineTemplate.Create(Name,ctsCommandLineParameters,'','',
                                     da_Block);
  end;
  
  procedure AddDefine(const AName, ADescription, AVariable, AValue: string;
    AnAction: TDefineAction);
  var
    NewTempl: TDefineTemplate;
  begin
    if AName='' then exit;
    NewTempl:=TDefineTemplate.Create(AName, ADescription, AVariable, AValue,
                                     AnAction);
    CreateMainTemplate;
    Result.AddChild(NewTempl);
  end;
  
  procedure AddDefine(const AName, ADescription, AVariable, AValue: string);
  var
    NewAction: TDefineAction;
  begin
    //debugln(['AddDefine Name="',AName,'" Var="',AVariable,'" Value="',AValue,'"']);
    if RecursiveDefines then
      NewAction:=da_DefineRecurse
    else
      NewAction:=da_Define;
    AddDefine(AName,ADescription,AVariable,AValue,NewAction);
  end;

  procedure AddDefine(const AParam: string);
  var
    Identifier: String;
    AValue: String;
  begin
    Identifier:=GetIdentifier(PChar(AParam));
    if Identifier='' then exit;
    AValue:='';
    if length(Identifier)<length(AParam) then begin
      if copy(AParam,length(Identifier)+1,2)=':=' then
        AValue:=copy(AParam,length(Identifier)+3,length(AParam));
    end;
    AddDefine('Define '+Identifier,ctsDefine+AParam,Identifier,AValue);
  end;

  procedure AddUndefine(const AParam: string);
  var
    NewAction: TDefineAction;
    Identifier: String;
  begin
    if RecursiveDefines then
      NewAction:=da_UndefineRecurse
    else
      NewAction:=da_Undefine;
    Identifier:=GetIdentifier(PChar(AParam));
    AddDefine('Undefine '+Identifier,ctsUndefine+AParam,Identifier,'',NewAction);
  end;

  procedure AddDefineUndefine(const AName: string; Define: boolean);
  begin
    if Define then
      AddDefine(AName)
    else
      AddUndefine(AName);
  end;

  function FindControllerUnit(const AControllerName: string): string;
  type
    TControllerType = record
      controllertypestr,
      controllerunitstr: string[20];
    end;
  const
    ControllerTypes: array[0..532] of TControllerType =
     ((controllertypestr:'';                  controllerunitstr:''),
      (controllertypestr:'LPC810M021FN8';     controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC811M001JDH16';   controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC812M101JD20';    controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC812M101JDH16';   controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC812M101JDH20';   controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPCXPRESSO812MAX';  controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC812M101JTB16';   controllerunitstr:'LPC8xx'),
      (controllertypestr:'LPC822M101JDH20';   controllerunitstr:'LPC82x'),
      (controllertypestr:'LPC822M101JHI33';   controllerunitstr:'LPC82x'),
      (controllertypestr:'LPC824M201JDH20';   controllerunitstr:'LPC82x'),
      (controllertypestr:'LPC824M201JHI33';   controllerunitstr:'LPC82x'),
      (controllertypestr:'LPCXPRESSO824MAX';  controllerunitstr:'LPC82x'),

      (controllertypestr:'LPC1110FD20';       controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FDH20_002';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_101';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_102';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_103';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_201';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1111FHN33_203';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FD20_102';   controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FDH20_102';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FDH28_102';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_101';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_102';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_103';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_201';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN24_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHN33_203';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHI33_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1112FHI33_203';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_201';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_203';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_301';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_302';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FHN33_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FBD48_301';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FBD48_302';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1113FBD48_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FDH28_102';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FN28_102';   controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_201';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_202';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_203';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_301';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_302';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHN33_333';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHI33_302';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FHI33_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FBD48_301';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FBD48_302';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FBD48_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FBD48_323';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1114FBD48_333';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1115FBD48_303';  controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC11C12FBD48_301'; controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC11C14FBD48_301'; controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC11C22FBD48_301'; controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC11C24FBD48_301'; controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC11D14FBD100_302';controllerunitstr:'LPC11XX'),
      (controllertypestr:'LPC1224FBD48_101';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1224FBD48_121';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1224FBD64_101';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1224FBD64_121';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1225FBD48_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1225FBD48_321';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1225FBD64_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1225FBD64_321';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1226FBD48_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1226FBD64_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1227FBD48_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1227FBD64_301';  controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC12D27FBD100_301';controllerunitstr:'LPC122X'),
      (controllertypestr:'LPC1311FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1311FHN33_01';   controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1313FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1313FHN33_01';   controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1313FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1313FBD48_01';   controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1315FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1315FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1316FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1316FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1317FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1317FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1317FBD64';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1342FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1342FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1343FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1343FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1345FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1345FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1346FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1346FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1347FHN33';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1347FBD48';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC1347FBD64';      controllerunitstr:'LPC13XX'),
      (controllertypestr:'LPC2114';           controllerunitstr:'LPC21x4'),
      (controllertypestr:'LPC2124';           controllerunitstr:'LPC21x4'),
      (controllertypestr:'LPC2194';           controllerunitstr:'LPC21x4'),
      (controllertypestr:'LPC1754';           controllerunitstr:'LPC1754'),
      (controllertypestr:'LPC1756';           controllerunitstr:'LPC1756'),
      (controllertypestr:'LPC1758';           controllerunitstr:'LPC1758'),
      (controllertypestr:'LPC1764';           controllerunitstr:'LPC1764'),
      (controllertypestr:'LPC1766';           controllerunitstr:'LPC1766'),
      (controllertypestr:'LPC1768';           controllerunitstr:'LPC1768'),
      (controllertypestr:'AT91SAM7S256';      controllerunitstr:'AT91SAM7x256'),
      (controllertypestr:'AT91SAM7SE256';     controllerunitstr:'AT91SAM7x256'),
      (controllertypestr:'AT91SAM7X256';      controllerunitstr:'AT91SAM7x256'),
      (controllertypestr:'AT91SAM7XC256';     controllerunitstr:'AT91SAM7x256'),
      (controllertypestr:'STM32F030C6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F030C8';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F030F4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F030K6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F030R8';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050C4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050C6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050F4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050F6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050G4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050G6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050K4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F050K6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051C4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051C6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051C8';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051K4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051K6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051K8';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051R4';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051R6';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F051R8';       controllerunitstr:'STM32F0XX'),
      (controllertypestr:'STM32F100X4';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F100X6';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F100X8';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F100XB';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F100XC';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F100XD';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F100XE';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F101X4';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F101X6';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F101X8';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F101XB';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F101XC';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F101XD';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F101XE';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F101XF';       controllerunitstr:'STM32F10X_XL'),
      (controllertypestr:'STM32F101XG';       controllerunitstr:'STM32F10X_XL'),
      (controllertypestr:'STM32F102X4';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F102X6';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F102X8';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F102XB';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F103X4';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F103X6';       controllerunitstr:'STM32F10X_LD'),
      (controllertypestr:'STM32F103X8';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F103XB';       controllerunitstr:'STM32F10X_MD'),
      (controllertypestr:'STM32F103XC';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F103XD';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F103XE';       controllerunitstr:'STM32F10X_HD'),
      (controllertypestr:'STM32F103XF';       controllerunitstr:'STM32F10X_XL'),
      (controllertypestr:'STM32F103XG';       controllerunitstr:'STM32F10X_XL'),
      (controllertypestr:'STM32F107X8';       controllerunitstr:'STM32F10X_CONN'),
      (controllertypestr:'STM32F107XB';       controllerunitstr:'STM32F10X_CONN'),
      (controllertypestr:'STM32F107XC';       controllerunitstr:'STM32F10X_CONN'),
      (controllertypestr:'STM32F105R8';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F105RB';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F105RC';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F105V8';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F105VB';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F105VC';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F107RB';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F107RC';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F107VB';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F107VC';       controllerunitstr:'STM32F10X_CL'),
      (controllertypestr:'STM32F401RB';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401VB';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401CC';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401RC';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401VC';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'DISCOVERYF401VC';   controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401CD';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401RD';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401VD';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401CE';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401RE';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'NUCLEOF401RE';      controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F401VE';       controllerunitstr:'STM32F401XX'),
      (controllertypestr:'STM32F407VG';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'DISCOVERYF407VG';   controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F407IG';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F407ZG';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F407VE';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F407ZE';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F407IE';       controllerunitstr:'STM32F407XX'),
      (controllertypestr:'STM32F411CC';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F411RC';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F411VC';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F411CE';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F411RE';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'NUCLEOF411RE';      controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F411VE';       controllerunitstr:'STM32F411XE'),
      (controllertypestr:'DISCOVERYF411VE';   controllerunitstr:'STM32F411XE'),
      (controllertypestr:'STM32F429VG';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429ZG';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429IG';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429VI';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429ZI';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'DISCOVERYF429ZI';   controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429II';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429VE';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429ZE';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429IE';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429BG';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429BI';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429BE';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429NG';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429NI';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F429NE';       controllerunitstr:'STM32F429XX'),
      (controllertypestr:'STM32F446MC';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446RC';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446VC';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446ZC';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446ME';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446RE';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'NUCLEOF446RE';      controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446VE';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F446ZE';       controllerunitstr:'STM32F446XX'),
      (controllertypestr:'STM32F745XE';       controllerunitstr:'STM32F745'),
      (controllertypestr:'STM32F745XG';       controllerunitstr:'STM32F745'),
      (controllertypestr:'STM32F746XE';       controllerunitstr:'STM32F746'),
      (controllertypestr:'STM32F746XG';       controllerunitstr:'STM32F746'),
      (controllertypestr:'STM32F756XE';       controllerunitstr:'STM32F756'),
      (controllertypestr:'STM32F756XG';       controllerunitstr:'STM32F756'),
      (controllertypestr:'LM3S1110';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1133';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1138';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1150';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1162';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1165';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1166';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2110';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2139';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6100';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6110';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1601';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1608';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1620';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1635';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1636';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1637';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1651';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2601';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2608';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2620';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2637';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2651';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6610';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6611';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6618';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6633';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6637';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8630';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1911';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1918';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1937';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1958';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1960';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1968';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S1969';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2911';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2918';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2919';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2939';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2948';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2950';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S2965';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6911';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6918';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6938';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6950';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6952';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S6965';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8930';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8933';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8938';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8962';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8970';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S8971';          controllerunitstr:'LM3FURY'),
      (controllertypestr:'LM3S5951';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S5956';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S1B21';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S2B93';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S5B91';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S9B81';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S9B90';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S9B92';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S9B95';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S9B96';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM3S5D51';          controllerunitstr:'LM3TEMPEST'),
      (controllertypestr:'LM4F120H5';         controllerunitstr:'LM4F120'),
      (controllertypestr:'SC32442B';          controllerunitstr:'SC32442b'),
      (controllertypestr:'XMC4500X1024';      controllerunitstr:'XMC4500'),
      (controllertypestr:'XMC4500X768';       controllerunitstr:'XMC4500'),
      (controllertypestr:'XMC4502X768';       controllerunitstr:'XMC4502'),
      (controllertypestr:'XMC4504X512';       controllerunitstr:'XMC4504'),
      (controllertypestr:'ALLWINNER_A20';     controllerunitstr:'ALLWINNER_A20'),
      (controllertypestr:'MK20DX128VFM5';     controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX128VFT5';     controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX128VLF5';     controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX128VLH5';     controllerunitstr:'MK20D5'),
      (controllertypestr:'TEENSY30'     ;     controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX128VMP5';     controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX32VFM5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX32VFT5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX32VLF5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX32VLH5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX32VMP5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX64VFM5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX64VFT5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX64VLF5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX64VLH5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX64VMP5';      controllerunitstr:'MK20D5'),
      (controllertypestr:'MK20DX128VLH7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX128VLK7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX128VLL7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX128VMC7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX256VLH7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX256VLK7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX256VLL7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX256VMC7';     controllerunitstr:'MK20D7'),
      (controllertypestr:'TEENSY31';          controllerunitstr:'MK20D7'),
      (controllertypestr:'TEENSY32';          controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX64VLH7';      controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX64VLK7';      controllerunitstr:'MK20D7'),
      (controllertypestr:'MK20DX64VMC7';      controllerunitstr:'MK20D7'),
      (controllertypestr:'MK22FN512CAP12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK22FN512CBP12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK22FN512VDC12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK22FN512VLH12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK22FN512VLL12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK22FN512VMP12';    controllerunitstr:'MK22F51212'),
      (controllertypestr:'FREEDOM_K22F';      controllerunitstr:'MK22F51212'),
      (controllertypestr:'MK64FN1M0VDC12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FN1M0VLL12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'FREEDOM_K64F';      controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FN1M0VLQ12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FN1M0VMD12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FX512VDC12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FX512VLL12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FX512VLQ12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'MK64FX512VMD12';    controllerunitstr:'MK64F12'),
      (controllertypestr:'ATSAM3X8E';         controllerunitstr:'SAM3X8E'),
      (controllertypestr:'ARDUINO_DUE';       controllerunitstr:'SAM3X8E'),
      (controllertypestr:'FLIP_N_CLICK';      controllerunitstr:'SAM3X8E'),
      (controllertypestr:'THUMB2_BARE';       controllerunitstr:'THUMB2_BARE'),
      (controllertypestr:'PIC32MX110F016B';   controllerunitstr:'PIC32MX1xxFxxxB'),
      (controllertypestr:'PIC32MX110F016C';   controllerunitstr:'PIC32MX1xxFxxxC'),
      (controllertypestr:'PIC32MX110F016D';   controllerunitstr:'PIC32MX1xxFxxxD'),
      (controllertypestr:'PIC32MX120F032B';   controllerunitstr:'PIC32MX1xxFxxxB'),
      (controllertypestr:'PIC32MX120F032C';   controllerunitstr:'PIC32MX1xxFxxxC'),
      (controllertypestr:'PIC32MX120F032D';   controllerunitstr:'PIC32MX1xxFxxxD'),
      (controllertypestr:'PIC32MX130F064B';   controllerunitstr:'PIC32MX1xxFxxxB'),
      (controllertypestr:'PIC32MX130F064C';   controllerunitstr:'PIC32MX1xxFxxxC'),
      (controllertypestr:'PIC32MX130F064D';   controllerunitstr:'PIC32MX1xxFxxxD'),
      (controllertypestr:'PIC32MX150F128B';   controllerunitstr:'PIC32MX1xxFxxxB'),
      (controllertypestr:'PIC32MX150F128C';   controllerunitstr:'PIC32MX1xxFxxxC'),
      (controllertypestr:'PIC32MX150F128D';   controllerunitstr:'PIC32MX1xxFxxxD'),
      (controllertypestr:'PIC32MX210F016B';   controllerunitstr:'PIC32MX2xxFxxxB'),
      (controllertypestr:'PIC32MX210F016C';   controllerunitstr:'PIC32MX2xxFxxxC'),
      (controllertypestr:'PIC32MX210F016D';   controllerunitstr:'PIC32MX2xxFxxxD'),
      (controllertypestr:'PIC32MX220F032B';   controllerunitstr:'PIC32MX2xxFxxxB'),
      (controllertypestr:'PIC32MX220F032C';   controllerunitstr:'PIC32MX2xxFxxxC'),
      (controllertypestr:'PIC32MX220F032D';   controllerunitstr:'PIC32MX2xxFxxxD'),
      (controllertypestr:'PIC32MX230F064B';   controllerunitstr:'PIC32MX2xxFxxxB'),
      (controllertypestr:'PIC32MX230F064C';   controllerunitstr:'PIC32MX2xxFxxxC'),
      (controllertypestr:'PIC32MX230F064D';   controllerunitstr:'PIC32MX2xxFxxxD'),
      (controllertypestr:'PIC32MX250F128B';   controllerunitstr:'PIC32MX2xxFxxxB'),
      (controllertypestr:'PIC32MX250F128C';   controllerunitstr:'PIC32MX2xxFxxxC'),
      (controllertypestr:'PIC32MX250F128D';   controllerunitstr:'PIC32MX2xxFxxxD'),
      (controllertypestr:'PIC32MX775F256H';   controllerunitstr:'PIC32MX7x5FxxxH'),
      (controllertypestr:'PIC32MX775F256L';   controllerunitstr:'PIC32MX7x5FxxxL'),
      (controllertypestr:'PIC32MX775F512H';   controllerunitstr:'PIC32MX7x5FxxxH'),
      (controllertypestr:'PIC32MX775F512L';   controllerunitstr:'PIC32MX7x5FxxxL'),
      (controllertypestr:'PIC32MX795F512H';   controllerunitstr:'PIC32MX7x5FxxxH'),
      (controllertypestr:'PIC32MX795F512L';   controllerunitstr:'PIC32MX7x5FxxxL'),
      (controllertypestr:'ATMEGA645';         controllerunitstr:'ATMEGA645'),
      (controllertypestr:'ATMEGA165A';        controllerunitstr:'ATMEGA165A'),
      (controllertypestr:'ATTINY44A';         controllerunitstr:'ATTINY44A'),
      (controllertypestr:'ATMEGA649A';        controllerunitstr:'ATMEGA649A'),
      (controllertypestr:'ATMEGA32U4';        controllerunitstr:'ATMEGA32U4'),
      (controllertypestr:'ATTINY26';          controllerunitstr:'ATTINY26'),
      (controllertypestr:'AT90USB1287';       controllerunitstr:'AT90USB1287'),
      (controllertypestr:'AT90PWM161';        controllerunitstr:'AT90PWM161'),
      (controllertypestr:'ATTINY48';          controllerunitstr:'ATTINY48'),
      (controllertypestr:'ATMEGA168P';        controllerunitstr:'ATMEGA168P'),
      (controllertypestr:'ATTINY10';          controllerunitstr:'ATTINY10'),
      (controllertypestr:'ATTINY84A';         controllerunitstr:'ATTINY84A'),
      (controllertypestr:'AT90USB82';         controllerunitstr:'AT90USB82'),
      (controllertypestr:'ATTINY2313';        controllerunitstr:'ATTINY2313'),
      (controllertypestr:'ATTINY461';         controllerunitstr:'ATTINY461'),
      (controllertypestr:'ATMEGA3250PA';      controllerunitstr:'ATMEGA3250PA'),
      (controllertypestr:'ATMEGA3290A';       controllerunitstr:'ATMEGA3290A'),
      (controllertypestr:'ATMEGA165P';        controllerunitstr:'ATMEGA165P'),
      (controllertypestr:'ATTINY43U';         controllerunitstr:'ATTINY43U'),
      (controllertypestr:'AT90USB162';        controllerunitstr:'AT90USB162'),
      (controllertypestr:'ATMEGA16U4';        controllerunitstr:'ATMEGA16U4'),
      (controllertypestr:'ATTINY24A';         controllerunitstr:'ATTINY24A'),
      (controllertypestr:'ATMEGA88P';         controllerunitstr:'ATMEGA88P'),
      (controllertypestr:'ATTINY88';          controllerunitstr:'ATTINY88'),
      (controllertypestr:'ATMEGA6490P';       controllerunitstr:'ATMEGA6490P'),
      (controllertypestr:'ATTINY40';          controllerunitstr:'ATTINY40'),
      (controllertypestr:'ATMEGA324P';        controllerunitstr:'ATMEGA324P'),
      (controllertypestr:'ATTINY167';         controllerunitstr:'ATTINY167'),
      (controllertypestr:'ATMEGA328';         controllerunitstr:'ATMEGA328'),
      (controllertypestr:'ATTINY861';         controllerunitstr:'ATTINY861'),
      (controllertypestr:'ATTINY85';          controllerunitstr:'ATTINY85'),
      (controllertypestr:'ATMEGA64M1';        controllerunitstr:'ATMEGA64M1'),
      (controllertypestr:'ATMEGA645P';        controllerunitstr:'ATMEGA645P'),
      (controllertypestr:'ATMEGA8U2';         controllerunitstr:'ATMEGA8U2'),
      (controllertypestr:'ATMEGA329A';        controllerunitstr:'ATMEGA329A'),
      (controllertypestr:'ATMEGA8A';          controllerunitstr:'ATMEGA8A'),
      (controllertypestr:'ATMEGA324PA';       controllerunitstr:'ATMEGA324PA'),
      (controllertypestr:'ATMEGA32HVB';       controllerunitstr:'ATMEGA32HVB'),
      (controllertypestr:'AT90PWM316';        controllerunitstr:'AT90PWM316'),
      (controllertypestr:'AT90PWM3B';         controllerunitstr:'AT90PWM3B'),
      (controllertypestr:'AT90USB646';        controllerunitstr:'AT90USB646'),
      (controllertypestr:'ATTINY20';          controllerunitstr:'ATTINY20'),
      (controllertypestr:'ATMEGA16';          controllerunitstr:'ATMEGA16'),
      (controllertypestr:'ATMEGA48A';         controllerunitstr:'ATMEGA48A'),
      (controllertypestr:'ATTINY24';          controllerunitstr:'ATTINY24'),
      (controllertypestr:'ATMEGA644';         controllerunitstr:'ATMEGA644'),
      (controllertypestr:'ATMEGA1284';        controllerunitstr:'ATMEGA1284'),
      (controllertypestr:'ATA6285';           controllerunitstr:'ATA6285'),
      (controllertypestr:'AT90CAN64';         controllerunitstr:'AT90CAN64'),
      (controllertypestr:'ATMEGA48';          controllerunitstr:'ATMEGA48'),
      (controllertypestr:'AT90CAN32';         controllerunitstr:'AT90CAN32'),
      (controllertypestr:'ATTINY9';           controllerunitstr:'ATTINY9'),
      (controllertypestr:'ATTINY87';          controllerunitstr:'ATTINY87'),
      (controllertypestr:'ATMEGA1281';        controllerunitstr:'ATMEGA1281'),
      (controllertypestr:'AT90PWM216';        controllerunitstr:'AT90PWM216'),
      (controllertypestr:'ATMEGA3250A';       controllerunitstr:'ATMEGA3250A'),
      (controllertypestr:'ATMEGA88A';         controllerunitstr:'ATMEGA88A'),
      (controllertypestr:'ATMEGA128RFA1';     controllerunitstr:'ATMEGA128RFA1'),
      (controllertypestr:'ATMEGA3290PA';      controllerunitstr:'ATMEGA3290PA'),
      (controllertypestr:'AT90PWM81';         controllerunitstr:'AT90PWM81'),
      (controllertypestr:'ATMEGA325P';        controllerunitstr:'ATMEGA325P'),
      (controllertypestr:'ATTINY84';          controllerunitstr:'ATTINY84'),
      (controllertypestr:'ATMEGA328P';        controllerunitstr:'ATMEGA328P'),
      (controllertypestr:'ATTINY13A';         controllerunitstr:'ATTINY13A'),
      (controllertypestr:'ATMEGA8';           controllerunitstr:'ATMEGA8'),
      (controllertypestr:'ATMEGA1284P';       controllerunitstr:'ATMEGA1284P'),
      (controllertypestr:'ATMEGA16U2';        controllerunitstr:'ATMEGA16U2'),
      (controllertypestr:'ATTINY45';          controllerunitstr:'ATTINY45'),
      (controllertypestr:'ATMEGA3250';        controllerunitstr:'ATMEGA3250'),
      (controllertypestr:'ATMEGA329';         controllerunitstr:'ATMEGA329'),
      (controllertypestr:'ATMEGA32A';         controllerunitstr:'ATMEGA32A'),
      (controllertypestr:'ATTINY5';           controllerunitstr:'ATTINY5'),
      (controllertypestr:'AT90CAN128';        controllerunitstr:'AT90CAN128'),
      (controllertypestr:'ATMEGA6490';        controllerunitstr:'ATMEGA6490'),
      (controllertypestr:'ATMEGA8515';        controllerunitstr:'ATMEGA8515'),
      (controllertypestr:'ATMEGA88PA';        controllerunitstr:'ATMEGA88PA'),
      (controllertypestr:'ATMEGA168A';        controllerunitstr:'ATMEGA168A'),
      (controllertypestr:'ATMEGA128';         controllerunitstr:'ATMEGA128'),
      (controllertypestr:'AT90USB1286';       controllerunitstr:'AT90USB1286'),
      (controllertypestr:'ATMEGA164PA';       controllerunitstr:'ATMEGA164PA'),
      (controllertypestr:'ATTINY828';         controllerunitstr:'ATTINY828'),
      (controllertypestr:'ATMEGA88';          controllerunitstr:'ATMEGA88'),
      (controllertypestr:'ATMEGA645A';        controllerunitstr:'ATMEGA645A'),
      (controllertypestr:'ATMEGA3290P';       controllerunitstr:'ATMEGA3290P'),
      (controllertypestr:'ATMEGA644P';        controllerunitstr:'ATMEGA644P'),
      (controllertypestr:'ATMEGA164A';        controllerunitstr:'ATMEGA164A'),
      (controllertypestr:'ATTINY4313';        controllerunitstr:'ATTINY4313'),
      (controllertypestr:'ATMEGA162';         controllerunitstr:'ATMEGA162'),
      (controllertypestr:'ATMEGA32C1';        controllerunitstr:'ATMEGA32C1'),
      (controllertypestr:'ATMEGA128A';        controllerunitstr:'ATMEGA128A'),
      (controllertypestr:'ATMEGA324A';        controllerunitstr:'ATMEGA324A'),
      (controllertypestr:'ATTINY13';          controllerunitstr:'ATTINY13'),
      (controllertypestr:'ATMEGA2561';        controllerunitstr:'ATMEGA2561'),
      (controllertypestr:'ATMEGA169A';        controllerunitstr:'ATMEGA169A'),
      (controllertypestr:'ATTINY261';         controllerunitstr:'ATTINY261'),
      (controllertypestr:'ATMEGA644A';        controllerunitstr:'ATMEGA644A'),
      (controllertypestr:'ATMEGA3290';        controllerunitstr:'ATMEGA3290'),
      (controllertypestr:'ATMEGA64A';         controllerunitstr:'ATMEGA64A'),
      (controllertypestr:'ATMEGA169P';        controllerunitstr:'ATMEGA169P'),
      (controllertypestr:'ATMEGA2560';        controllerunitstr:'ATMEGA2560'),
      (controllertypestr:'ATMEGA32';          controllerunitstr:'ATMEGA32'),
      (controllertypestr:'ATTINY861A';        controllerunitstr:'ATTINY861A'),
      (controllertypestr:'ATTINY28';          controllerunitstr:'ATTINY28'),
      (controllertypestr:'ATMEGA48P';         controllerunitstr:'ATMEGA48P'),
      (controllertypestr:'ATMEGA8535';        controllerunitstr:'ATMEGA8535'),
      (controllertypestr:'ATMEGA168PA';       controllerunitstr:'ATMEGA168PA'),
      (controllertypestr:'ATMEGA16M1';        controllerunitstr:'ATMEGA16M1'),
      (controllertypestr:'ATMEGA16HVB';       controllerunitstr:'ATMEGA16HVB'),
      (controllertypestr:'ATMEGA164P';        controllerunitstr:'ATMEGA164P'),
      (controllertypestr:'ATMEGA325A';        controllerunitstr:'ATMEGA325A'),
      (controllertypestr:'ATMEGA640';         controllerunitstr:'ATMEGA640'),
      (controllertypestr:'ATMEGA6450';        controllerunitstr:'ATMEGA6450'),
      (controllertypestr:'ATMEGA329P';        controllerunitstr:'ATMEGA329P'),
      (controllertypestr:'ATA6286';           controllerunitstr:'ATA6286'),
      (controllertypestr:'AT90USB647';        controllerunitstr:'AT90USB647'),
      (controllertypestr:'ATMEGA168';         controllerunitstr:'ATMEGA168'),
      (controllertypestr:'ATMEGA6490A';       controllerunitstr:'ATMEGA6490A'),
      (controllertypestr:'ATMEGA32M1';        controllerunitstr:'ATMEGA32M1'),
      (controllertypestr:'ATMEGA64C1';        controllerunitstr:'ATMEGA64C1'),
      (controllertypestr:'ATMEGA32U2';        controllerunitstr:'ATMEGA32U2'),
      (controllertypestr:'ATTINY4';           controllerunitstr:'ATTINY4'),
      (controllertypestr:'ATMEGA644PA';       controllerunitstr:'ATMEGA644PA'),
      (controllertypestr:'AT90PWM1';          controllerunitstr:'AT90PWM1'),
      (controllertypestr:'ATTINY44';          controllerunitstr:'ATTINY44'),
      (controllertypestr:'ATMEGA325PA';       controllerunitstr:'ATMEGA325PA'),
      (controllertypestr:'ATMEGA6450A';       controllerunitstr:'ATMEGA6450A'),
      (controllertypestr:'ATTINY2313A';       controllerunitstr:'ATTINY2313A'),
      (controllertypestr:'ATMEGA329PA';       controllerunitstr:'ATMEGA329PA'),
      (controllertypestr:'ATTINY461A';        controllerunitstr:'ATTINY461A'),
      (controllertypestr:'ATMEGA6450P';       controllerunitstr:'ATMEGA6450P'),
      (controllertypestr:'ATMEGA64';          controllerunitstr:'ATMEGA64'),
      (controllertypestr:'ATMEGA165PA';       controllerunitstr:'ATMEGA165PA'),
      (controllertypestr:'ATMEGA16A';         controllerunitstr:'ATMEGA16A'),
      (controllertypestr:'ATMEGA649';         controllerunitstr:'ATMEGA649'),
      (controllertypestr:'ATMEGA1280';        controllerunitstr:'ATMEGA1280'),
      (controllertypestr:'AT90PWM2B';         controllerunitstr:'AT90PWM2B'),
      (controllertypestr:'ATMEGA649P';        controllerunitstr:'ATMEGA649P'),
      (controllertypestr:'ATMEGA3250P';       controllerunitstr:'ATMEGA3250P'),
      (controllertypestr:'ATMEGA48PA';        controllerunitstr:'ATMEGA48PA'),
      (controllertypestr:'ATTINY1634';        controllerunitstr:'ATTINY1634'),
      (controllertypestr:'ATMEGA325';         controllerunitstr:'ATMEGA325'),
      (controllertypestr:'ATMEGA169PA';       controllerunitstr:'ATMEGA169PA'),
      (controllertypestr:'ATTINY261A';        controllerunitstr:'ATTINY261A'),
      (controllertypestr:'ATTINY25';          controllerunitstr:'ATTINY25'));

  var
    i: integer;
    str: String;
  begin
    str:=UpperCase(AControllerName);
    for i := low(ControllerTypes) to high(ControllerTypes) do
      if ControllerTypes[i].controllertypestr=str then
        exit(ControllerTypes[i].controllerunitstr);
    result:='';
  end;

var
  s: string;
  CompilerMode: String;
  m: Integer;
  UnitPath: String;
  IncPath: String;
  Params: TStrings;
  i: Integer;
  Param, Namespaces: String;
  p: PChar;
  MacMinVer: single;
begin
  Result:=nil;
  if AlwaysCreate then
    CreateMainTemplate;
  CompilerMode:='';
  UnitPath:='';
  IncPath:='';
  Namespaces:='';
  Params:=TStringListUTF8.Create;
  try
    SplitCmdLineParams(CmdLine,Params);
    for i:=0 to Params.Count-1 do begin
      Param:=Params[i];
      if Param='' then continue;
      p:=PChar(Param);
      if p^<>'-' then continue;
      // a parameter
      case p[1] of
      'F':
        case p[2] of
        'i':
          IncPath+=';'+copy(Param,4,length(Param));
        'u':
          UnitPath+=';'+copy(Param,4,length(Param));
        'N':
          Namespaces+=';'+copy(Param,4,length(Param));
        end;

      'd':
        begin
          // define
          AddDefine(copy(Param,3,255));
        end;

      'u':
        begin
          // undefine
          AddUndefine(copy(Param,3,255));
        end;

      'S':
        begin
          // syntax
          inc(p,2);
          repeat
            case p^ of
            '2': CompilerMode:='ObjFPC';
            'd': CompilerMode:='Delphi';
            'o': CompilerMode:='TP';
            'p': CompilerMode:='GPC';
            else break;
            end;
            inc(p);
          until false;
        end;

      'M':
        begin
          // syntax
          CompilerMode:=copy(Param,3,255);
        end;

      'N':
        case p[2] of
        'S': Namespaces+=';'+copy(Param,4,length(Param))
        end;

      'W':
        case p[2] of
        'p':
          begin
            s:=FindControllerUnit(copy(Param,4,255));

            // controller unit
            if s<>'' then
              AddDefine('Define '+MacroControllerUnit,
                ctsDefine+MacroControllerUnit,MacroControllerUnit,
                s);
          end;
        'M':
          begin
            val(copy(Param,4,255),MacMinVer,m);
            if m=0 then
              AddDefine(MacOSMinSDKVersionMacro,MacOSMinSDKVersionMacro,
                MacOSMinSDKVersionMacro,IntToStr(Round(MacMinVer*100)));
          end;
        end;

      end;
    end;
  finally
    Params.Free;
  end;
  if CompilerMode<>'' then begin
    for m:=low(FPCSyntaxModes) to high(FPCSyntaxModes) do
      AddDefineUndefine('FPC_'+FPCSyntaxModes[m],SysUtils.CompareText(CompilerMode,FPCSyntaxModes[m])=0);
  end;
  if AddPaths then begin
    if UnitPath<>'' then
      AddDefine('UnitPath','UnitPath addition',UnitPathMacroName,UnitPathMacro+';'+UnitPath);
    if IncPath<>'' then
      AddDefine('IncPath','IncPath addition',IncludePathMacroName,IncludePathMacro+';'+IncPath);
  end;
  if Namespaces<>'' then
    AddDefine('Namespaces','Namespaces addition',NamespacesMacroName,NamespacesMacro+';'+Namespaces);

  Result.SetDefineOwner(Owner,true);
end;

procedure TDefinePool.ConsistencyCheck;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].ConsistencyCheck;
end;

procedure TDefinePool.WriteDebugReport;
var i: integer;
begin
  DebugLn('TDefinePool.WriteDebugReport');
  for i:=0 to Count-1 do
    Items[i].WriteDebugReport(false);
  ConsistencyCheck;
end;

procedure TDefinePool.CalcMemSize(Stats: TCTMemStats);
var
  i: Integer;
begin
  Stats.Add('TDefinePool',PtrUInt(InstanceSize)
    +MemSizeString(FEnglishErrorMsgFilename));
  if FItems<>nil then begin
    Stats.Add('TDefinePool.Count',Count);
    for i:=0 to Count-1 do
      Items[i].CalcMemSize(Stats);
  end;
end;


{ TFPCSourceRules }

function TFPCSourceRules.GetItems(Index: integer): TFPCSourceRule;
begin
  Result:=TFPCSourceRule(FItems[Index]);
end;

procedure TFPCSourceRules.SetTargets(const AValue: string);
begin
  if FTargets=AValue then exit;
  FTargets:=LowerCase(AValue);
end;

constructor TFPCSourceRules.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TFPCSourceRules.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TFPCSourceRules.Clear;
var
  i: Integer;
begin
  if FItems.Count=0 then exit;
  for i:=0 to FItems.Count-1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  IncreaseChangeStamp;
end;

function TFPCSourceRules.IsEqual(Rules: TFPCSourceRules): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Count<>Rules.Count then exit;
  for i:=0 to Count-1 do
    if not Items[i].IsEqual(Rules[i]) then exit;
  Result:=true;
end;

procedure TFPCSourceRules.Assign(Rules: TFPCSourceRules);
var
  i: Integer;
  SrcRule: TFPCSourceRule;
  Rule: TFPCSourceRule;
begin
  if IsEqual(Rules) then exit;
  Clear;
  for i:=0 to Rules.Count-1 do begin
    SrcRule:=Rules[i];
    Rule:=Add(SrcRule.Filename);
    Rule.Assign(SrcRule);
    //debugln(['TFPCSourceRules.Assign ',i,' ',Rule.Targets,' ',Rule.Filename]);
  end;
  IncreaseChangeStamp;
end;

function TFPCSourceRules.Clone: TFPCSourceRules;
begin
  Result:=TFPCSourceRules.Create;
  Result.Assign(Self);
end;

function TFPCSourceRules.Count: integer;
begin
  Result:=FItems.Count;
end;

function TFPCSourceRules.Add(const Filename: string): TFPCSourceRule;
begin
  Result:=TFPCSourceRule.Create;
  Result.Score:=Score;
  Result.Targets:=Targets;
  //DebugLn(['TFPCSourceRules.Add Targets="',Result.Targets,'" Priority=',Result.Score]);
  Result.Filename:=lowercase(GetForcedPathDelims(Filename));
  FItems.Add(Result);
  IncreaseChangeStamp;
end;

function TFPCSourceRules.GetDefaultTargets(TargetOS, TargetCPU: string): string;
var
  SrcOS: String;
  SrcOS2: String;
  SrcCPU: String;
begin
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  if TargetCPU='' then
    TargetCPU:=GetCompiledTargetCPU;
  Result:=TargetOS+','+TargetCPU;
  SrcOS:=GetDefaultSrcOSForTargetOS(TargetOS);
  SrcOS2:=GetDefaultSrcOS2ForTargetOS(TargetOS);
  SrcCPU:=GetDefaultSrcCPUForTargetCPU(TargetCPU);
  if SrcOS<>'' then Result:=Result+','+SrcOS;
  if SrcOS2<>'' then Result:=Result+','+SrcOS2;
  if SrcCPU<>'' then Result:=Result+','+SrcCPU;
end;

procedure TFPCSourceRules.GetRulesForTargets(Targets: string;
  var RulesSortedForFilenameStart: TAVLTree);
var
  i: Integer;
begin
  if RulesSortedForFilenameStart=nil then
    RulesSortedForFilenameStart:=
                             TAVLTree.Create(@CompareFPCSourceRulesViaFilename);
  for i:=0 to Count-1 do
    if Items[i].FitsTargets(Targets) then
      RulesSortedForFilenameStart.Add(Items[i]);
end;

function TFPCSourceRules.GetScore(Filename: string;
  RulesSortedForFilenameStart: TAVLTree): integer;
var
  Node: TAVLTreeNode;
  Rule: TFPCSourceRule;
  cmp: LongInt;
  Cnt: Integer;
begin
  Result:=0;
  if Filename='' then exit;
  Filename:=LowerCase(Filename);
  {Node:=RulesSortedForFilenameStart.FindLowest;
  while Node<>nil do begin
    Rule:=TFPCSourceRule(Node.Data);
    DebugLn(['TFPCSourceRules.GetScore Rule: ',Rule.Score,' ',Rule.Filename]);
    Node:=RulesSortedForFilenameStart.FindSuccessor(Node);
  end;}
  // find first rule for Filename
  Node:=RulesSortedForFilenameStart.Root;
  while true do begin
    Rule:=TFPCSourceRule(Node.Data);
    cmp:=CompareFilenames(Filename,Rule.Filename);
    //DebugLn(['TFPCSourceRules.GetScore Rule.Filename=',Rule.Filename,' Filename=',Filename,' cmp=',cmp]);
    if cmp=0 then
      break;
    if cmp<0 then begin
      if Node.Left<>nil then
        Node:=Node.Left
      else
        break;
    end else begin
      if Node.Right<>nil then
        Node:=Node.Right
      else
        break;
    end;
  end;
  { The rules are sorted for the file name. Shorter file names comes before
    longer ones.
       packages/httpd20/examples
       packages/httpd22
       packages/httpd22/examples
    A filename packages/httpd22/examples matches
           packages/httpd22
       and packages/httpd22/examples
    If a file name has no exact match the binary search for packages/httpd22/e
    can either point to
           packages/httpd22
        or packages/httpd22/examples
  }

  // run through all fitting rules (the Filename is >= Rule.Filename)
  Cnt:=0;
  while Node<>nil do begin
    inc(Cnt);
    Rule:=TFPCSourceRule(Node.Data);
    if Rule.FitsFilename(Filename) then
      inc(Result,Rule.Score)
    else if Cnt>1 then
      break;
    Node:=RulesSortedForFilenameStart.FindPrecessor(Node);
  end;
end;

procedure TFPCSourceRules.IncreaseChangeStamp;
begin
  if FChangeStamp<High(FChangeStamp) then
    inc(FChangeStamp)
  else
    FChangeStamp:=Low(FChangeStamp);
end;

{ TFPCSourceRule }

function TFPCSourceRule.FitsTargets(const FilterTargets: string): boolean;
var
  FilterStartPos: PChar;
  TargetPos: PChar;
  FilterPos: PChar;
begin
  //DebugLn(['TFPCSourceRule.FitsTargets FilterTargets="',FilterTargets,'" Targets="',Targets,'"']);
  if Targets='*' then exit(true);
  if (Targets='') or (FilterTargets='') then exit(false);
  FilterStartPos:=PChar(FilterTargets);
  while true do begin
    while (FilterStartPos^=',') do inc(FilterStartPos);
    if FilterStartPos^=#0 then exit(false);
    TargetPos:=PChar(Targets);
    repeat
      while (TargetPos^=',') do inc(TargetPos);
      if TargetPos^=#0 then break;
      FilterPos:=FilterStartPos;
      while (FilterPos^=TargetPos^) and (not (FilterPos^ in [#0,','])) do begin
        inc(TargetPos);
        inc(FilterPos);
      end;
      if (TargetPos^ in [#0,',']) then begin
        // the target fits
        exit(true);
      end;
      // try next target
      while not (TargetPos^ in [#0,',']) do inc(TargetPos);
    until TargetPos^=#0;
    // next target filter
    while not (FilterStartPos^ in [#0,',']) do inc(FilterStartPos);
  end;
  Result:=false;
end;

function TFPCSourceRule.FitsFilename(const aFilename: string): boolean;
begin
  Result:=(length(Filename)<=length(aFilename))
         and CompareMem(Pointer(Filename),Pointer(aFilename),length(Filename));
end;

function TFPCSourceRule.IsEqual(Rule: TFPCSourceRule): boolean;
begin
  Result:=false;
  if (Filename<>Rule.Filename)
  or (Score<>Rule.Score)
  or (Targets<>Rule.Targets) then
    exit;
  Result:=true;
end;

procedure TFPCSourceRule.Assign(Rule: TFPCSourceRule);
begin
  Filename:=Rule.Filename;
  Score:=Rule.Score;
  Targets:=Rule.Targets;
end;

{ TPCTargetConfigCache }

constructor TPCTargetConfigCache.Create(AOwner: TComponent);
begin
  CTIncreaseChangeStamp(FChangeStamp); // set to not 0
  inherited Create(AOwner);
  ConfigFiles:=TPCConfigFileStateList.Create;
  if Owner is TPCTargetConfigCaches then
    Caches:=TPCTargetConfigCaches(Owner);
end;

destructor TPCTargetConfigCache.Destroy;
begin
  Clear;
  FreeAndNil(ConfigFiles);
  inherited Destroy;
end;

procedure TPCTargetConfigCache.Clear;
begin
  // keep keys
  Kind:=pcFPC;
  CompilerDate:=0;
  RealCompiler:='';
  RealCompilerDate:=0;
  RealTargetCPU:='';
  RealTargetOS:='';
  RealTargetCPUCompiler:='';
  FullVersion:='';
  HasPPUs:=false;
  ConfigFiles.Clear;
  ErrorMsg:='';
  ErrorTranslatedMsg:='';
  FreeAndNil(Defines);
  FreeAndNil(Undefines);
  FreeAndNil(UnitPaths);
  FreeAndNil(IncludePaths);
  FreeAndNil(UnitScopes);
  FreeAndNil(Units);
  FreeAndNil(Includes);
  FreeAndNil(UnitToFPM);
  FreeAndNil(FPMNameToFPM);
end;

function TPCTargetConfigCache.Equals(Item: TPCTargetConfigCache;
  CompareKey: boolean): boolean;

  function CompareStrings(List1, List2: TStrings): boolean;
  var
    List1Empty: Boolean;
    List2Empty: Boolean;
  begin
    Result:=false;
    List1Empty:=(List1=nil) or (List1.Count=0);
    List2Empty:=(List2=nil) or (List2.Count=0);
    if (List1Empty<>List2Empty) then exit;
    if (not List1Empty) and (not List1.Equals(List2)) then exit;
    Result:=true;
  end;

  function CompareStringTrees(Tree1, Tree2: TStringToStringTree): boolean;
  var
    Tree1Empty: Boolean;
    Tree2Empty: Boolean;
  begin
    Result:=false;
    Tree1Empty:=(Tree1=nil) or (Tree1.Tree.Count=0);
    Tree2Empty:=(Tree2=nil) or (Tree2.Tree.Count=0);
    if (Tree1Empty<>Tree2Empty) then exit;
    if (not Tree1Empty) and (not Tree1.Equals(Tree2)) then exit;
    Result:=true;
  end;

var
  Node1, Node2: TAVLTreeNode;
  S2PItem1, S2PItem2: PStringToPointerTreeItem;
begin
  Result:=false;
  if CompareKey then begin
    if (TargetOS<>Item.TargetOS)
      or (TargetCPU<>Item.TargetCPU)
      or (Compiler<>Item.Compiler)
      or (CompilerOptions<>Item.CompilerOptions)
    then
      exit;
  end;
  if (Kind<>Item.Kind)
    or (CompilerDate<>Item.CompilerDate)
    or (RealCompiler<>Item.RealCompiler)
    or (RealCompilerDate<>Item.RealCompilerDate)
    or (RealTargetOS<>Item.RealTargetOS)
    or (RealTargetCPU<>Item.RealTargetCPU)
    or (RealTargetCPUCompiler<>Item.RealTargetCPUCompiler)
    or (FullVersion<>Item.FullVersion)
    or (HasPPUs<>Item.HasPPUs)
    or (not ConfigFiles.Equals(Item.ConfigFiles,true))
  then
    exit;
  if not CompareStringTrees(Defines,Item.Defines) then exit;
  if not CompareStringTrees(Undefines,Item.Undefines) then exit;
  if not CompareStrings(UnitPaths,Item.UnitPaths) then exit;
  if not CompareStrings(IncludePaths,Item.IncludePaths) then exit;
  if not CompareStrings(UnitScopes,Item.UnitScopes) then exit;
  if not CompareStringTrees(Units,Item.Units) then exit;
  if not CompareStringTrees(Includes,Item.Includes) then exit;

  if UnitToFPM<>nil then begin
    if Item.UnitToFPM=nil then exit;
    if UnitToFPM.Count<>Item.UnitToFPM.Count then exit;
    Node1:=UnitToFPM.Tree.FindLowest;
    Node2:=Item.UnitToFPM.Tree.FindLowest;
    while Node1<>nil do begin
      S2PItem1:=PStringToPointerTreeItem(Node1.Data);
      S2PItem2:=PStringToPointerTreeItem(Node2.Data);
      if S2PItem1^.Name<>S2PItem2^.Name then
        exit;
      if TPCFPMFileState(S2PItem1^.Value).Name<>TPCFPMFileState(S2PItem2^.Value).Name then
        exit;
      Node1:=Node1.Successor;
      Node2:=Node2.Successor;
    end;
  end;

  if FPMNameToFPM<>nil then begin
    if Item.FPMNameToFPM=nil then exit;
    if FPMNameToFPM.Count<>Item.FPMNameToFPM.Count then exit;
    Node1:=FPMNameToFPM.Tree.FindLowest;
    Node2:=Item.FPMNameToFPM.Tree.FindLowest;
    while Node1<>nil do begin
      S2PItem1:=PStringToPointerTreeItem(Node1.Data);
      S2PItem2:=PStringToPointerTreeItem(Node2.Data);
      if S2PItem1^.Name<>S2PItem2^.Name then
        exit;
      if not TPCFPMFileState(S2PItem1^.Value).Equals(TPCFPMFileState(S2PItem2^.Value),true) then
        exit;
      Node1:=Node1.Successor;
      Node2:=Node2.Successor;
    end;
  end;

  Result:=true;
end;

procedure TPCTargetConfigCache.Assign(Source: TPersistent);
var
  Item: TPCTargetConfigCache;
  Node: TAVLTreeNode;
  FPM: TPCFPMFileState;
  S2PItem: PStringToPointerTreeItem;

  procedure AssignStringTree(var Dest: TStringToStringTree; const Src: TStringToStringTree);
  begin
    if Src<>nil then begin
      if Dest=nil then Dest:=TStringToStringTree.Create(false);
      Dest.Assign(Src);
    end else begin
      FreeAndNil(Dest);
    end;
  end;

  procedure AssignStringList(var Dest: TStrings; const Src: TStrings);
  begin
    if Src<>nil then begin
      if Dest=nil then Dest:=TStringList.Create;
      Dest.Assign(Src);
    end else begin
      FreeAndNil(Dest);
    end;
  end;

begin
  if Source is TPCTargetConfigCache then begin
    Item:=TPCTargetConfigCache(Source);
    // keys
    TargetOS:=Item.TargetOS;
    TargetCPU:=Item.TargetCPU;
    Compiler:=Item.Compiler;
    CompilerOptions:=Item.CompilerOptions;
    // values
    Kind:=Item.Kind;
    CompilerDate:=Item.CompilerDate;
    RealCompiler:=Item.RealCompiler;
    RealCompilerDate:=Item.RealCompilerDate;
    RealTargetOS:=Item.RealTargetOS;
    RealTargetCPU:=Item.RealTargetCPU;
    RealTargetCPUCompiler:=Item.RealTargetCPUCompiler;
    FullVersion:=Item.FullVersion;
    HasPPUs:=Item.HasPPUs;
    ConfigFiles.Assign(Item.ConfigFiles);

    AssignStringTree(Defines,Item.Defines);
    AssignStringTree(Undefines,Item.Undefines);
    AssignStringList(UnitPaths,Item.UnitPaths);
    AssignStringList(IncludePaths,Item.IncludePaths);
    AssignStringList(UnitScopes,Item.UnitScopes);
    AssignStringTree(Units,Item.Units);
    AssignStringTree(Includes,Item.Includes);

    FreeAndNil(UnitToFPM);
    FreeAndNil(FPMNameToFPM);
    if (Item.FPMNameToFPM<>nil) and (Item.UnitToFPM=nil) then begin
      FPMNameToFPM:=TStringToPointerTree.Create(false);
      FPMNameToFPM.FreeValues:=true;
      UnitToFPM:=TStringToPointerTree.Create(false);
      // clone TPCFPMFileState objects
      Node:=Item.FPMNameToFPM.Tree.FindLowest;
      while Node<>nil do begin
        S2PItem:=PStringToPointerTreeItem(Node.Data);
        FPM:=TPCFPMFileState.Create;
        FPM.Name:=S2PItem^.Name;
        FPM.Assign(TPCFPMFileState(S2PItem^.Value));
        FPMNameToFPM[FPM.Name]:=FPM;
        Node:=Node.Successor;
      end;
      // clone UnitToFPM
      Node:=Item.UnitToFPM.Tree.FindLowest;
      while Node<>nil do begin
        S2PItem:=PStringToPointerTreeItem(Node.Data);
        FPM:=TPCFPMFileState(FPMNameToFPM[TPCFPMFileState(S2PItem^.Value).Name]);
        UnitToFPM[S2PItem^.Name]:=FPM;
        Node:=Node.Successor;
      end;
    end;

    ErrorMsg:=Item.ErrorMsg;
    ErrorTranslatedMsg:=Item.ErrorTranslatedMsg;
  end else
    inherited Assign(Source);
end;

procedure TPCTargetConfigCache.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);

  procedure LoadPathsFor(out ADest: TStrings; const ASubPath: string);
  var
    i: Integer;
    List: TStringList;
    BaseDir, s: String;
  begin
    // Paths: format: semicolon separated compressed list
    List:=TStringList.Create;
    try
      s:=XMLConfig.GetValue(Path+ASubPath+'Value','');
      List.Delimiter:=';';
      List.StrictDelimiter:=true;
      List.DelimitedText:=s;
      ADest:=Decompress1FileList(List);
      BaseDir:=TrimFilename(AppendPathDelim(XMLConfig.GetValue(Path+ASubPath+'BaseDir','')));
      if BaseDir<>'' then
        for i:=0 to ADest.Count-1 do
          ADest[i]:=ChompPathDelim(TrimFilename(BaseDir+ADest[i]))
      else
        for i:=ADest.Count-1 downto 0 do
          if ADest[i]='' then
            ADest.Delete(i)
          else
            ADest[i]:=ChompPathDelim(TrimFilename(ADest[i]));
      // do not sort, order is important (e.g. for httpd.ppu)
    finally
      List.Free;
    end;
  end;

  procedure LoadSemicolonList(out UnitScopes: TStrings; const ASubPath: string);
  var
    s, Scope: String;
    p: Integer;
  begin
    UnitScopes:=TStringList.Create;
    s:=XMLConfig.GetValue(Path+ASubPath,'');
    p:=1;
    while p<=length(s) do begin
      Scope:=GetNextDelimitedItem(s,';',p);
      if Scope<>'' then
        UnitScopes.Add(Scope);
    end;
  end;

  procedure LoadFilesFor(var ADest: TStringToStringTree; const ASubPath: string);
  var
    i: Integer;
    List: TStringList;
    File_Name, CurPath, s, Filename: String;
    FileList: TStringList;
  begin
    // files: format: ASubPath+Values semicolon separated list of compressed filename
    if ADest=nil then
      ADest:=TStringToStringTree.Create(false);
    List:=TStringList.Create;
    FileList:=nil;
    try
      CurPath:=Path+ASubPath+'Value';
      s:=XMLConfig.GetValue(CurPath,'');
      List.Delimiter:=';';
      List.StrictDelimiter:=true;
      List.DelimitedText:=s;
      FileList:=Decompress1FileList(List);
      for i:=0 to FileList.Count-1 do begin
        Filename:=TrimFilename(FileList[i]);
        File_Name:=ExtractFileNameOnly(Filename);
        if (File_Name='') or not IsDottedIdentifier(File_Name) then begin
          DebugLn(['Warning: [TPCTargetConfigCache.LoadFromXMLConfig] invalid filename "',File_Name,'" in "',XMLConfig.Filename,'" at "',CurPath,'"']);
          continue;
        end;
        ADest[File_Name]:=Filename;
      end;
    finally
      List.Free;
      FileList.Free;
    end;
  end;

var
  Cnt: integer;
  SubPath: String;
  DefineName, DefineValue: String;
  s, CurUnitName, CurFPMName: String;
  i: Integer;
  p: Integer;
  StartPos: Integer;
  FPM: TPCFPMFileState;
begin
  Clear;

  Kind:=StrToPascalCompiler(XMLConfig.GetValue(Path+'Kind',PascalCompilerNames[pcFPC]));
  TargetOS:=XMLConfig.GetValue(Path+'TargetOS','');
  TargetCPU:=XMLConfig.GetValue(Path+'TargetCPU','');
  Compiler:=XMLConfig.GetValue(Path+'Compiler/File','');
  CompilerOptions:=XMLConfig.GetValue(Path+'Compiler/Options','');
  CompilerDate:=XMLConfig.GetValue(Path+'Compiler/Date',0);
  RealCompiler:=XMLConfig.GetValue(Path+'RealCompiler/File','');
  RealCompilerDate:=XMLConfig.GetValue(Path+'RealCompiler/Date',0);
  RealTargetOS:=XMLConfig.GetValue(Path+'RealCompiler/OS','');
  RealTargetCPU:=XMLConfig.GetValue(Path+'RealCompiler/CPU','');
  RealTargetCPUCompiler:=XMLConfig.GetValue(Path+'RealCompiler/InPath','');
  FullVersion:=XMLConfig.GetValue(Path+'RealCompiler/FullVersion','');
  HasPPUs:=XMLConfig.GetValue(Path+'HasPPUs',true);
  ConfigFiles.LoadFromXMLConfig(XMLConfig,Path+'Configs/');

  // defines: format: Define<Number>/Name,Value
  Cnt:=XMLConfig.GetValue(Path+'Defines/Count',0);
  for i:=1 to Cnt do begin
    SubPath:=Path+'Defines/Macro'+IntToStr(i)+'/';
    DefineName:=UpperCaseStr(XMLConfig.GetValue(SubPath+'Name',''));
    if not IsValidIdent(DefineName) then begin
      DebugLn(['Warning: [TPCTargetConfigCache.LoadFromXMLConfig] invalid define name ',DefineName]);
      continue;
    end;
    DefineValue:=XMLConfig.GetValue(SubPath+'Value','');
    if Defines=nil then
      Defines:=TStringToStringTree.Create(false);
    Defines[DefineName]:=DefineValue;
  end;

  // undefines: format: Undefines/Value and comma separated list of names
  s:=XMLConfig.GetValue(Path+'Undefines/Values','');
  if s<>'' then begin
    p:=1;
    while (p<=length(s)) do begin
      StartPos:=1;
      while (p<=length(s)) and (s[p]<>';') do inc(p);
      DefineName:=copy(s,StartPos,p-StartPos);
      if IsValidIdent(DefineName) then begin
        if Undefines=nil then
          Undefines:=TStringToStringTree.Create(false);
        Undefines[DefineName]:='';
      end;
      inc(p);
    end;
  end;

  // Paths
  LoadPathsFor(UnitPaths,'UnitPaths/');
  LoadPathsFor(IncludePaths,'IncludePaths/');

  // Unit scopes
  LoadSemicolonList(UnitScopes, 'UnitScopes');

  // Files
  LoadFilesFor(Units,'Units/');
  LoadFilesFor(Includes,'Includes/');

  // read FPMNameToFPM before UnitToFPM!
  Cnt:=XMLConfig.GetValue(Path+'FPMs/Count',0);
  if Cnt>0 then begin
    FPMNameToFPM:=TStringToPointerTree.Create(false);
    FPMNameToFPM.FreeValues:=true;
    UnitToFPM:=TStringToPointerTree.Create(false);
    for i:=1 to Cnt do begin
      SubPath:=Path+'FPMs/Item'+IntToStr(i)+'/';
      FPM:=TPCFPMFileState.Create;
      FPM.Name:=XMLConfig.GetValue(SubPath+'Name','');
      if FPM.Name='' then
        FPM.Free
      else
        FPM.LoadFromXMLConfig(XMLConfig,SubPath);
      FPMNameToFPM[FPM.Name]:=FPM;
    end;
  end;

  // UnitToFPM
  Cnt:=XMLConfig.GetValue(Path+'UnitToFPM/Count',0);
  if UnitToFPM<>nil then begin
    for i:=1 to Cnt do begin
      SubPath:=Path+'UnitToFPM/Item'+IntToStr(i)+'/';
      CurUnitName:=XMLConfig.GetValue(SubPath+'Unit','');
      if CurUnitName='' then continue;
      CurFPMName:=XMLConfig.GetValue(SubPath+'FPM','');
      if CurFPMName='' then continue;
      FPM:=TPCFPMFileState(FPMNameToFPM[CurFPMName]);
      if FPM=nil then exit;
      UnitToFPM[CurUnitName]:=FPM;
    end;
  end;
end;

procedure TPCTargetConfigCache.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);

  procedure SavePathsFor(const ASource: TStrings; const ASubPath: string);
  var
    List: TStringList;
    RelativeUnitPaths: TStringList;
    BaseDir, s: string;
  begin
    // Paths: write as semicolon separated compressed list
    s:='';
    BaseDir:='';
    if ASource<>nil then begin
      List:=nil;
      RelativeUnitPaths:=nil;
      try
        RelativeUnitPaths:=MakeRelativeFileList(ASource,BaseDir);
        List:=Compress1FileList(RelativeUnitPaths);
        // do not sort, order is important (e.g. for httpd.ppu)
        List.Delimiter:=';';
        List.StrictDelimiter:=true;
        s:=List.DelimitedText;
      finally
        RelativeUnitPaths.Free;
        List.Free;
      end;
    end;
    XMLConfig.SetDeleteValue(Path+ASubPath+'BaseDir',BaseDir,'');
    XMLConfig.SetDeleteValue(Path+ASubPath+'Value',s,'');
  end;

  procedure SaveSemicolonList(List: TStrings; const ASubPath: string);
  var
    i: Integer;
    s: String;
  begin
    s:='';
    if List<>nil then
      for i:=0 to List.Count-1 do
        s:=s+';'+List[i];
    delete(s,1,1);
    XMLConfig.SetDeleteValue(Path+ASubPath,s,'');
  end;

  procedure SaveFilesFor(const ASource: TStringToStringTree; const ASubPath: string);
  var
    List: TStringList;
    FileList: TStringList;
    Filename, s: String;
    Node: TAVLTreeNode;
    Item: PStringToStringItem;
  begin
    // Files: ASubPath+Values semicolon separated list of compressed filenames
    // Files contains thousands of file names. This needs compression.
    s:='';
    List:=nil;
    FileList:=TStringList.Create;
    try
      if ASource<>nil then begin
        // Create a string list of filenames
        Node:=ASource.Tree.FindLowest;
        while Node<>nil do begin
          Item:=PStringToStringItem(Node.Data);
          Filename:=Item^.Value;
          FileList.Add(Filename);
          Node:=ASource.Tree.FindSuccessor(Node);
        end;
        // Sort the strings.
        FileList.CaseSensitive:=true;
        FileList.Sort;
        // Compress the file names
        List:=Compress1FileList(FileList);
        // and write the semicolon separated list
        List.Delimiter:=';';
        List.StrictDelimiter:=true;
        s:=List.DelimitedText;
      end;
    finally
      List.Free;
      FileList.Free;
    end;
    XMLConfig.SetDeleteValue(Path+ASubPath+'Value',s,'');
  end;

var
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  Cnt, i: Integer;
  SubPath: String;
  s: String;
  S2PItem: PStringToPointerTreeItem;
  FPM: TPCFPMFileState;
begin
  XMLConfig.SetDeleteValue(Path+'Kind',PascalCompilerNames[Kind],PascalCompilerNames[pcFPC]);
  XMLConfig.SetDeleteValue(Path+'TargetOS',TargetOS,'');
  XMLConfig.SetDeleteValue(Path+'TargetCPU',TargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'Compiler/File',Compiler,'');
  XMLConfig.SetDeleteValue(Path+'Compiler/Options',CompilerOptions,'');
  XMLConfig.SetDeleteValue(Path+'Compiler/Date',CompilerDate,0);
  XMLConfig.SetDeleteValue(Path+'RealCompiler/File',RealCompiler,'');
  XMLConfig.SetDeleteValue(Path+'RealCompiler/Date',RealCompilerDate,0);
  XMLConfig.SetDeleteValue(Path+'RealCompiler/OS',RealTargetOS,'');
  XMLConfig.SetDeleteValue(Path+'RealCompiler/CPU',RealTargetCPU,'');
  XMLConfig.SetDeleteValue(Path+'RealCompiler/InPath',RealTargetCPUCompiler,'');
  XMLConfig.SetDeleteValue(Path+'RealCompiler/FullVersion',FullVersion,'');
  XMLConfig.SetDeleteValue(Path+'HasPPUs',HasPPUs,true);
  ConfigFiles.SaveToXMLConfig(XMLConfig,Path+'Configs/');

  // Defines: write as Define<Number>/Name,Value
  Cnt:=0;
  if Defines<>nil then begin
    Node:=Defines.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      if IsValidIdent(Item^.Name) then begin
        inc(Cnt);
        SubPath:=Path+'Defines/Macro'+IntToStr(Cnt)+'/';
        XMLConfig.SetDeleteValue(SubPath+'Name',Item^.Name,'');
        XMLConfig.SetDeleteValue(SubPath+'Value',Item^.Value,'');
      end;
      Node:=Defines.Tree.FindSuccessor(Node);
    end;
  end;
  XMLConfig.SetDeleteValue(Path+'Defines/Count',Cnt,0);

  // Undefines: write as Undefines/Value and comma separated list of names
  Cnt:=0;
  s:='';
  if Undefines<>nil then begin
    Node:=Undefines.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      inc(Cnt);
      if s<>'' then s:=s+',';
      s:=s+Item^.Name;
      Node:=Undefines.Tree.FindSuccessor(Node);
    end;
  end;
  XMLConfig.SetDeleteValue(Path+'Undefines/Values',s,'');

  // Paths
  SavePathsFor(UnitPaths, 'UnitPaths/');
  SavePathsFor(IncludePaths, 'IncludePaths/');

  // Unit scopes
  SaveSemicolonList(UnitScopes, 'UnitScopes');

  // Files
  SaveFilesFor(Units, 'Units/');
  SaveFilesFor(Includes, 'Includes/');

  // UnitToFPM
  if UnitToFPM<>nil then begin
    // write as UnitToFPM/Item<i>/Unit,FPM
    i:=0;
    Node:=UnitToFPM.Tree.FindLowest;
    while Node<>nil do begin
      inc(i);
      SubPath:=Path+'UnitToFPM/Item'+IntToStr(i)+'/';
      S2PItem:=PStringToPointerTreeItem(Node.Data);
      XMLConfig.SetValue(SubPath+'Unit',S2PItem^.Name);
      FPM:=TPCFPMFileState(S2PItem^.Value);
      XMLConfig.SetValue(SubPath+'FPM',FPM.Name);
      Node:=Node.Successor;
    end;
    XMLConfig.SetDeleteValue(Path+'UnitToFPM/Count',i,0);
  end;

  // FPMNameToFPM
  if FPMNameToFPM<>nil then begin
    // write as FPMs/Item<i>/
    i:=0;
    Node:=FPMNameToFPM.Tree.FindLowest;
    while Node<>nil do begin
      inc(i);
      SubPath:=Path+'FPMs/Item'+IntToStr(i)+'/';
      S2PItem:=PStringToPointerTreeItem(Node.Data);
      FPM:=TPCFPMFileState(S2PItem^.Value);
      FPM.SaveToXMLConfig(XMLConfig,SubPath);
      Node:=Node.Successor;
    end;
    XMLConfig.SetDeleteValue(Path+'FPMs/Count',i,0);
  end;
end;

procedure TPCTargetConfigCache.LoadFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'FPCConfig/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TPCTargetConfigCache.SaveToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'FPCConfig/');
  finally
    XMLConfig.Free;
  end;
end;

function TPCTargetConfigCache.NeedsUpdate: boolean;
var
  i: Integer;
  Cfg: TPCConfigFileState;
  AFilename: String;
begin
  Result:=true;

  if (not FileExistsCached(Compiler)) then begin
    if CTConsoleVerbosity>0 then
      debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" compiler file missing "',Compiler,'"']);
    exit;
  end;
  if (FileAgeCached(Compiler)<>CompilerDate) then begin
    if CTConsoleVerbosity>0 then
      debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" compiler file changed "',Compiler,'" FileAge=',FileAgeCached(Compiler),' StoredAge=',CompilerDate]);
    exit;
  end;
  if (RealCompiler<>'') and (CompareFilenames(RealCompiler,Compiler)<>0)
  then begin
    if (not FileExistsCached(RealCompiler))
    or (FileAgeCached(RealCompiler)<>RealCompilerDate) then begin
      if CTConsoleVerbosity>0 then
        debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" real compiler file changed "',RealCompiler,'"']);
      exit;
    end;
  end;
  // fpc searches via PATH for the real compiler, resolves any symlink
  // and that is the RealCompiler
  AFilename:=FindDefaultTargetCPUCompiler(TargetCPU,true);
  if RealTargetCPUCompiler<>AFilename then begin
    if CTConsoleVerbosity>0 then
      debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" real compiler in PATH changed from "',RealTargetCPUCompiler,'" to "',AFilename,'"']);
    exit;
  end;
  for i:=0 to ConfigFiles.Count-1 do begin
    Cfg:=ConfigFiles[i];
    if (Cfg.Filename='') or (not FilenameIsAbsolute(Cfg.Filename)) then continue;
    if FileExistsCached(Cfg.Filename)<>Cfg.FileExists then begin
      if CTConsoleVerbosity>0 then
        debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" config fileexists changed "',Cfg.Filename,'"']);
      exit;
    end;
    if Cfg.FileExists and (FileAgeCached(Cfg.Filename)<>Cfg.FileDate) then begin
      if CTConsoleVerbosity>0 then
        debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',CompilerOptions,'" config file changed "',Cfg.Filename,'"']);
      exit;
    end;
  end;
  Result:=false;
end;

function TPCTargetConfigCache.GetFPCInfoCmdLineOptions(ExtraOptions: string
  ): string;
begin
  Result:=CompilerOptions;
  if TargetCPU<>'' then
    Result:=Result+' -P'+LowerCase(TargetCPU);
  if TargetOS<>'' then
    Result:=Result+' -T'+LowerCase(TargetOS);
  if ExtraOptions<>'' then
    Result:=Result+' '+ExtraOptions;
  Result:=Trim(Result);
end;

procedure TPCTargetConfigCache.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
  if Caches<>nil then
    Caches.IncreaseChangeStamp;
end;

function TPCTargetConfigCache.Update(TestFilename: string;
  ExtraOptions: string; const OnProgress: TDefinePoolProgress): boolean;

  procedure PreparePaths(APaths: TStrings);
  var
    i: Integer;
  begin
    if APaths<>nil then
      for i:=0 to APaths.Count-1 do
        APaths[i]:=ChompPathDelim(TrimFilename(APaths[i]));
  end;

var
  i: Integer;
  OldOptions: TPCTargetConfigCache;
  CfgFiles: TStrings;
  Filename: string;
  CfgFileExists: Boolean;
  CfgFileDate: Integer;
  Info: String;
  Infos: TFPCInfoStrings;
  InfoTypes: TFPCInfoTypes;
  BaseDir: String;
  FullFilename, KindErrorMsg: String;
begin
  OldOptions:=TPCTargetConfigCache.Create(nil);
  CfgFiles:=nil;
  try
    // remember old state to find out if something changed
    OldOptions.Assign(Self);
    Clear;

    if CTConsoleVerbosity>0 then
      debugln(['Hint: [TPCTargetConfigCache.NeedsUpdate] ',Compiler,' TargetOS=',TargetOS,' TargetCPU=',TargetCPU,' CompilerOptions=',CompilerOptions,' ExtraOptions=',ExtraOptions,' PATH=',GetEnvironmentVariableUTF8('PATH')]);
    CompilerDate:=-1;
    if FileExistsCached(Compiler) then begin
      CompilerDate:=FileAgeCached(Compiler);
      ExtraOptions:=GetFPCInfoCmdLineOptions(ExtraOptions);// contains TargetOS and TargetCPU
      BaseDir:='';

      // get version, OS and CPU
      InfoTypes:=[fpciTargetOS,fpciTargetProcessor,fpciFullVersion];
      Info:=RunFPCInfo(Compiler,InfoTypes,ExtraOptions);
      if ParseFPCInfo(Info,InfoTypes,Infos) then begin
        // fpc or pas2js
        RealTargetOS:=Infos[fpciTargetOS];
        RealTargetCPU:=Infos[fpciTargetProcessor];
        FullVersion:=Infos[fpciFullVersion];
        if FullVersion='' then
          debugln(['Warning: [TPCTargetConfigCache.Update] cannot determine compiler version: Compiler="'+Compiler+'" Options="'+ExtraOptions+'"']);
      end else begin
        RealTargetOS:=TargetOS;
        if RealTargetOS='' then
          RealTargetOS:=GetCompiledTargetOS;
        RealTargetCPU:=TargetCPU;
        if RealTargetCPU='' then
          RealTargetCPU:=GetCompiledTargetCPU;
      end;

      if FullVersion<>'' then begin
        // run fpc/pas2js and parse output
        RunFPCVerbose(Compiler,TestFilename,CfgFiles,RealCompiler,UnitPaths,
                      IncludePaths,UnitScopes,Defines,Undefines,ExtraOptions);
        //debugln(['TPCTargetConfigCache.Update UnitPaths="',UnitPaths.Text,'"']);
        //debugln(['TPCTargetConfigCache.Update UnitScopes="',UnitScopes.Text,'"']);
        //debugln(['TPCTargetConfigCache.Update IncludePaths="',IncludePaths.Text,'"']);
      end;

      if Defines<>nil then begin
        if Defines.Contains('PAS2JS') and Defines.Contains('PAS2JS_FULLVERSION') then
          Kind:=pcPas2js
        else if Defines.Contains('FPC') and Defines.Contains('FPC_FULLVERSION') then
          Kind:=pcFPC
        else begin
          IsCompilerExecutable(Compiler,KindErrorMsg,Kind,false);
          if KindErrorMsg<>'' then
            debugln(['Warning: [TPCTargetConfigCache.Update] cannot determine type of compiler: Compiler="'+Compiler+'" Options="'+ExtraOptions+'"']);
        end;
      end;
      if Kind=pcFPC then
        RealTargetCPUCompiler:=FindDefaultTargetCPUCompiler(TargetCPU,true);
      PreparePaths(UnitPaths);
      PreparePaths(IncludePaths);
      // store the real compiler file and date
      if (RealCompiler<>'') and FileExistsCached(RealCompiler) then begin
        RealCompilerDate:=FileAgeCached(RealCompiler);
      end else if Kind=pcFPC then begin
        if CTConsoleVerbosity>=-1 then
          debugln(['Warning: [TPCTargetConfigCache.Update] cannot find real compiler for this platform: Compiler="'+Compiler+'" Options="'+ExtraOptions+'" RealCompiler="',RealCompiler,'"']);
      end;
      // store the list of tried and read cfg files
      if CfgFiles<>nil then
        for i:=0 to CfgFiles.Count-1 do begin
          Filename:=CfgFiles[i];
          if Filename='' then continue;
          CfgFileExists:=Filename[1]='+';
          Filename:=copy(Filename,2,length(Filename));
          FullFilename:=ExpandFileNameUTF8(TrimFileName(Filename),BaseDir);
          if CfgFileExists<>FileExistsCached(FullFilename) then begin
            debugln(['Warning: [TPCTargetConfigCache.Update] '+ExtractFileName(Compiler)+' found cfg a file, the IDE did not: "',Filename,'"']);
            CfgFileExists:=not CfgFileExists;
          end;
          CfgFileDate:=0;
          if CfgFileExists then
            CfgFileDate:=FileAgeCached(Filename);
          ConfigFiles.Add(Filename,CfgFileExists,CfgFileDate);
        end;
      // gather all units and include files in search paths
      GatherUnitsInSearchPaths(UnitPaths,IncludePaths,OnProgress,Units,Includes);
      GatherUnitsInFPMSources(Units,UnitToFPM,FPMNameToFPM,OnProgress);
      //if Kind=pcPas2js then begin
      //  debugln(['TPCTargetConfigCache.Update Units:']);
      //  for e in Units do
      //    debugln(['  ',E^.Name,' ',E^.Value]);
      //end;
      if (UnitPaths<>nil) and (UnitPaths.Count=0) then begin
        if CTConsoleVerbosity>=-1 then
          debugln(['Warning: [TPCTargetConfigCache.Update] no unit paths: ',Compiler,' ',ExtraOptions]);
      end;
      // check if the system ppu exists
      HasPPUs:=(Kind=pcFPC) and (Units<>nil)
          and (CompareFileExt(Units['system'],'ppu',false)=0);
      // check compiler version define
      if (CTConsoleVerbosity>=-1) and (Defines<>nil) then begin
        case Kind of
          pcFPC:
            if not Defines.Contains('FPC_FULLVERSION') then
              debugln(['Warning: [TPCTargetConfigCache.Update] invalid fpc: Compiler="'+Compiler+'" Options="'+ExtraOptions+'" RealCompiler="',RealCompiler,'" missing FPC_FULLVERSION']);
          pcDelphi: ;
          pcPas2js:
            if not Defines.Contains('PAS2JS_FULLVERSION') then
              debugln(['Warning: [TPCTargetConfigCache.Update] invalid pas2js: Compiler="'+Compiler+'" Options="'+ExtraOptions+'" missing PAS2JS_FULLVERSION']);
        end;
      end;
    end;
    // check for changes
    if not Equals(OldOptions) then begin
      IncreaseChangeStamp;
      if CTConsoleVerbosity>=0 then
        debugln(['Hint: [TPCTargetConfigCache.Update] has changed']);
    end;
    Result:=true;
  finally
    CfgFiles.Free;
    OldOptions.Free;
  end;
end;

function TPCTargetConfigCache.FindDefaultTargetCPUCompiler(aTargetCPU: string;
  ResolveLinks: boolean): string;

  function Search(const ShortFileName: string): string;
  var
    ExtPath: String;
  begin
    // fpc.exe first searches in -Xp<path>
    ExtPath:=GetLastFPCParameter(CompilerOptions,'-Xp');
    if (ExtPath<>'') and (ExtPath<>'.') then begin
      if not FilenameIsAbsolute(ExtPath) then
        // If -Xp is relative then it is relative to the working directory
        ExtPath:=TrimFilename(AppendPathDelim(GetCurrentDirUTF8)+ExtPath);
      Result:=AppendPathDelim(ExtPath)+ShortFileName;
      if FileExistsCached(Result) then
        exit;
    end;

    // then fpc.exe searches in its own directory
    if Compiler<>'' then begin
      Result:=ExtractFilePath(Compiler);
      if FilenameIsAbsolute(Result) then begin
        Result+=ShortFileName;
        if FileExistsCached(Result) then
          exit;
      end;
    end;

    // finally fpc.exe searches in PATH
    Result:=SearchFileInPath(ShortFileName,GetCurrentDirUTF8,
      GetEnvironmentVariableUTF8('PATH'),PathSeparator,ctsfcDefault);
    if (Result<>'') or (Compiler='') then exit;
  end;

var
  CompiledTargetCPU: String;
  Cross: Boolean;
  Postfix: String;
begin
  Result:='';
  if Kind<>pcFPC then exit;

  CompiledTargetCPU:=GetCompiledTargetCPU;
  if aTargetCPU='' then
    aTargetCPU:=CompiledTargetCPU;
  Cross:=not SameText(aTargetCPU,CompiledTargetCPU);

  // The -V<postfix> parameter searches for ppcx64-postfix instead of ppcx64
  Postfix:=GetLastFPCParameter(CompilerOptions,'-V');
  if Postfix<>'' then
    Postfix:='-'+Postfix;

  Result:=Search(GetDefaultCompilerFilename(aTargetCPU,Cross)+Postfix);
  if (Result='') and Cross then begin
    Result:=Search(GetDefaultCompilerFilename(aTargetCPU,false)+Postfix);
    if Result='' then exit;
  end;
  if ResolveLinks then begin
    Result:=GetPhysicalFilenameCached(Result,false);
  end;
end;

function TPCTargetConfigCache.GetUnitPaths: string;
begin
  if UnitPaths=nil then exit('');
  UnitPaths.Delimiter:=';';
  UnitPaths.StrictDelimiter:=true;
  Result:=UnitPaths.DelimitedText;
end;

function TPCTargetConfigCache.GetFPCVerNumbers(out FPCVersion, FPCRelease,
  FPCPatch: integer): boolean;
var
  v: string;
begin
  // get default FPC version
  v:={$I %FPCVERSION%};
  Result:=SplitFPCVersion(v,FPCVersion,FPCRelease,FPCPatch);
  if Defines<>nil then begin
    // use defines
    FPCVersion:=StrToIntDef(Defines['FPC_VERSION'],FPCVersion);
    FPCRelease:=StrToIntDef(Defines['FPC_RELEASE'],FPCRelease);
    FPCPatch:=StrToIntDef(Defines['FPC_PATCH'],FPCPatch);
  end;
end;

function TPCTargetConfigCache.GetFPCVer: string;
var
  FPCVersion: integer;
  FPCRelease: integer;
  FPCPatch: integer;
begin
  if GetFPCVerNumbers(FPCVersion,FPCRelease,FPCPatch) then
    Result:=IntToStr(FPCVersion)+'.'+IntToStr(FPCRelease)+'.'+IntToStr(FPCPatch)
  else
    Result:='';
end;

function TPCTargetConfigCache.GetFPC_FULLVERSION: integer;
begin
  if Defines<>nil then
    Result:=StrToIntDef(Defines['FPC_FULLVERSION'],0)
  else
    Result:=0;
  if Result=0 then
    Result:=GetCompiledFPCVersion;
end;

function TPCTargetConfigCache.IndexOfUsedCfgFile: integer;
begin
  if ConfigFiles=nil then exit(-1);
  Result:=0;
  while (Result<ConfigFiles.Count) and (not ConfigFiles[Result].FileExists) do
    inc(Result);
  if Result=ConfigFiles.Count then Result:=-1;
end;

{ TPCTargetConfigCaches }

constructor TPCTargetConfigCaches.Create(AOwner: TComponent);
begin
  CTIncreaseChangeStamp(FChangeStamp); // set to not 0
  inherited Create(AOwner);
  fItems:=TAVLTree.Create(@CompareFPCTargetConfigCacheItems);
end;

destructor TPCTargetConfigCaches.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TPCTargetConfigCaches.Clear;
begin
  if fItems.Count=0 then exit;
  fItems.FreeAndClear;
  IncreaseChangeStamp;
end;

function TPCTargetConfigCaches.Equals(Caches: TPCTargetConfigCaches): boolean;
var
  Node1, Node2: TAVLTreeNode;
  Item1: TPCTargetConfigCache;
  Item2: TPCTargetConfigCache;
begin
  Result:=false;
  if Caches.fItems.Count<>fItems.Count then exit;
  Node1:=fItems.FindLowest;
  Node2:=Caches.fItems.FindLowest;
  while Node1<>nil do begin
    Item1:=TPCTargetConfigCache(Node1.Data);
    Item2:=TPCTargetConfigCache(Node2.Data);
    if not Item1.Equals(Item2) then exit;
    Node1:=fItems.FindSuccessor(Node1);
    Node2:=Caches.fItems.FindSuccessor(Node2);
  end;
  Result:=true;
end;

procedure TPCTargetConfigCaches.Assign(Source: TPersistent);
var
  Caches: TPCTargetConfigCaches;
  Node: TAVLTreeNode;
  SrcItem: TPCTargetConfigCache;
  NewItem: TPCTargetConfigCache;
begin
  if Source is TPCTargetConfigCaches then begin
    Caches:=TPCTargetConfigCaches(Source);
    if Equals(Caches) then exit; // no change, keep ChangeStamp
    Clear;
    Node:=Caches.fItems.FindLowest;
    while Node<>nil do begin
      SrcItem:=TPCTargetConfigCache(Node.Data);
      NewItem:=TPCTargetConfigCache.Create(Self);
      NewItem.Assign(SrcItem);
      fItems.Add(NewItem);
      Node:=Caches.fItems.FindSuccessor(Node);
    end;
    IncreaseChangeStamp;
  end else
    inherited Assign(Source);
end;

procedure TPCTargetConfigCaches.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt: integer;
  i: Integer;
  Item: TPCTargetConfigCache;
begin
  Clear;
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=1 to Cnt do begin
    Item:=TPCTargetConfigCache.Create(Self);
    Item.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    if (Item.Compiler<>'') then
      fItems.Add(Item)
    else
      Item.Free;
  end;
  IncreaseChangeStamp;
end;

procedure TPCTargetConfigCaches.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Node: TAVLTreeNode;
  Item: TPCTargetConfigCache;
  i: Integer;
begin
  Node:=fItems.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=TPCTargetConfigCache(Node.Data);
    inc(i);
    Item.SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    Node:=fItems.FindSuccessor(Node);
  end;
  XMLConfig.SetDeleteValue(Path+'Count',i,0);
end;

procedure TPCTargetConfigCaches.LoadFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'FPCConfigs/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TPCTargetConfigCaches.SaveToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'FPCConfigs/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TPCTargetConfigCaches.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

function TPCTargetConfigCaches.Find(CompilerFilename, CompilerOptions,
  TargetOS, TargetCPU: string; CreateIfNotExists: boolean
  ): TPCTargetConfigCache;
var
  Node: TAVLTreeNode;
  Cmp: TPCTargetConfigCache;
begin
  Cmp:=TPCTargetConfigCache.Create(Self);
  try
    Cmp.Compiler:=CompilerFilename;
    Cmp.CompilerOptions:=CompilerOptions;
    Cmp.TargetOS:=TargetOS;
    Cmp.TargetCPU:=TargetCPU;
    Node:=fItems.Find(cmp);
    if Node<>nil then begin
      Result:=TPCTargetConfigCache(Node.Data);
    end else if CreateIfNotExists then begin
      Result:=cmp;
      cmp:=nil;
      fItems.Add(Result);
    end else begin
      Result:=nil;
    end;
  finally
    Cmp.Free;
  end;
end;

procedure TPCTargetConfigCaches.GetDefaultCompilerTarget(
  const CompilerFilename, CompilerOptions: string; out TargetOS,
  TargetCPU: string);
var
  Cfg: TPCTargetConfigCache;
begin
  Cfg:=Find(CompilerFilename,CompilerOptions,'','',true);
  if Cfg=nil then begin
    TargetOS:='';
    TargetCPU:='';
  end else begin
    if Cfg.NeedsUpdate then
      Cfg.Update(TestFilename);
    TargetOS:=Cfg.RealTargetOS;
    TargetCPU:=Cfg.RealTargetCPU;
  end;
end;

function TPCTargetConfigCaches.GetListing: string;
var
  Node: TAVLTreeNode;
  CfgCache: TPCTargetConfigCache;
  i: Integer;
begin
  Result:='TPCTargetConfigCaches.GetListing Count='+dbgs(fItems.Count)+LineEnding;
  i:=0;
  Node:=fItems.FindLowest;
  while Node<>nil do begin
    inc(i);
    CfgCache:=TPCTargetConfigCache(Node.Data);
    Result+='  '+dbgs(i)+':'
           +' TargetOS="'+CfgCache.TargetOS+'"'
           +' TargetCPU="'+CfgCache.TargetCPU+'"'
           +' Compiler="'+CfgCache.Compiler+'"'
           +' CompilerOptions="'+CfgCache.CompilerOptions+'"'
           +LineEnding;
    Node:=fItems.FindSuccessor(Node);
  end;
end;

{ TPCConfigFileStateList }

function TPCConfigFileStateList.GetItems(Index: integer): TPCConfigFileState;
begin
  Result:=TPCConfigFileState(fItems[Index]);
end;

constructor TPCConfigFileStateList.Create;
begin
  fItems:=TFPList.Create;
end;

destructor TPCConfigFileStateList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TPCConfigFileStateList.Clear;
var
  i: Integer;
begin
  for i:=0 to fItems.Count-1 do
    TObject(fItems[i]).Free;
  fItems.Clear;
end;

procedure TPCConfigFileStateList.Assign(List: TPCConfigFileStateList);
var
  i: Integer;
  Item: TPCConfigFileState;
begin
  Clear;
  for i:=0 to List.Count-1 do begin
    Item:=List[i];
    Add(Item.Filename,Item.FileExists,Item.FileDate);
  end;
end;

function TPCConfigFileStateList.Equals(List: TPCConfigFileStateList;
  CheckDates: boolean): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Count<>List.Count then exit;
  for i:=0 to Count-1 do
    if not Items[i].Equals(List[i],CheckDates) then exit;
  Result:=true;
end;

function TPCConfigFileStateList.Add(aFilename: string; aFileExists: boolean;
  aFileDate: longint): TPCConfigFileState;
begin
  Result:=TPCConfigFileState.Create(aFilename,aFileExists,aFileDate);
  fItems.Add(Result);
end;

function TPCConfigFileStateList.Count: integer;
begin
  Result:=fItems.Count;
end;

procedure TPCConfigFileStateList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt: integer;
  Item: TPCConfigFileState;
  i: Integer;
begin
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=1 to Cnt do begin
    Item:=TPCConfigFileState.Create('',false,0);
    Item.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    fItems.Add(Item);
  end;
end;

procedure TPCConfigFileStateList.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  for i:=1 to Count do
    Items[i-1].SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
  XMLConfig.SetDeleteValue(Path+'Count',Count,0);
end;

{ TPCConfigFileState }

constructor TPCConfigFileState.Create(const aFilename: string;
  aFileExists: boolean; aFileDate: longint);
begin
  Filename:=aFilename;
  FileExists:=aFileExists;
  FileDate:=aFileDate;
end;

function TPCConfigFileState.Equals(Other: TPCConfigFileState;
  CheckDate: boolean): boolean;
begin
  Result:=false;
  if (Filename<>Other.Filename) or (FileExists<>Other.FileExists) then exit;
  if CheckDate and FileExists and (FileDate<>Other.FileDate) then exit;
  Result:=true;
end;

procedure TPCConfigFileState.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Filename:=XMLConfig.GetValue(Path+'Filename','');
  FileExists:=XMLConfig.GetValue(Path+'Exists',false);
  FileDate:=XMLConfig.GetValue(Path+'Date',0);
end;

procedure TPCConfigFileState.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Filename',Filename,'');
  XMLConfig.SetDeleteValue(Path+'Exists',FileExists,false);
  XMLConfig.SetDeleteValue(Path+'Date',FileDate,0);
end;

{ TFPCSourceCacheItem }

constructor TFPCSourceCache.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Files:=TStringList.Create;
  Valid:=false;
  CTIncreaseChangeStamp(FChangeStamp); // set to not 0
  if Owner is TFPCSourceCaches then
    Caches:=TFPCSourceCaches(Owner);
end;

destructor TFPCSourceCache.Destroy;
begin
  FreeAndNil(Files);
  inherited Destroy;
end;

procedure TFPCSourceCache.Clear;
begin
  FreeAndNil(Files);
  Valid:=false;
end;

procedure TFPCSourceCache.Assign(Source: TPersistent);
var
  Cache: TFPCSourceCache;
begin
  if Source is TFPCSourceCache then begin
    Cache:=TFPCSourceCache(Source);
    Directory:=Cache.Directory;
    Files.Assign(Cache.Files);
    Valid:=Cache.Valid;
  end else
    inherited Assign(Source);
end;

function TFPCSourceCache.Equals(Cache: TFPCSourceCache): boolean;
begin
  Result:=false;
  if Valid<>Cache.Valid then exit;
  if Directory<>Cache.Directory then exit;
  if not Files.Equals(Cache.Files) then exit;
  Result:=true;
end;

procedure TFPCSourceCache.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  List: TStringList;
begin
  Clear;
  List:=nil;
  try
    Valid:=XMLConfig.GetValue(Path+'Valid',true);
    Directory:=XMLConfig.GetValue(Path+'Directory','');
    List:=TStringList.Create;
    List.StrictDelimiter:=true;
    List.Delimiter:=';';
    List.DelimitedText:=XMLConfig.GetValue(Path+'Files','');
    FreeAndNil(Files);
    Files:=Decompress1FileList(List);
  finally
    if Files=nil then Files:=TStringList.Create;
    List.Free;
  end;
end;

procedure TFPCSourceCache.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  List: TStringList;
  s: String;
begin
  List:=nil;
  try
    XMLConfig.SetDeleteValue(Path+'Valid',Valid,true);
    XMLConfig.SetDeleteValue(Path+'Directory',Directory,'');
    if Files<>nil then begin
      List:=Compress1FileList(Files);
      List.StrictDelimiter:=true;
      List.Delimiter:=';';
      s:=List.DelimitedText;
    end else
      s:='';
    XMLConfig.SetDeleteValue(Path+'Files',s,'');
  finally
    List.Free;
  end;
end;

procedure TFPCSourceCache.LoadFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'FPCSourceDirectory/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TFPCSourceCache.SaveToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'FPCSourceDirectory/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TFPCSourceCache.Update(const OnProgress: TDefinePoolProgress);
var
  NewFiles: TStringList;
begin
  Valid:=false;
  if Directory<>'' then
    NewFiles:=GatherFilesInFPCSources(Directory,OnProgress)
  else
    NewFiles:=TStringList.Create;
  Update(NewFiles);
end;

procedure TFPCSourceCache.Update(var NewFiles: TStringList);
var
  OldFiles: TStringList;
  OldValid: Boolean;
begin
  OldFiles:=Files;
  OldValid:=Valid;
  try
    Files:=NewFiles;
    NewFiles:=nil;
    Valid:=true;
    if (Valid<>OldValid)
    or ((Files=nil)<>(OldFiles=nil))
    or ((Files<>nil) and (Files.Text<>OldFiles.Text)) then begin
      IncreaseChangeStamp;
      if CTConsoleVerbosity>0 then
        debugln(['Hint: [TFPCSourceCache.Update] ',Directory,' has changed.']);
    end;
  finally
    OldFiles.Free;
  end;
end;

procedure TFPCSourceCache.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
  if Caches<>nil then
    Caches.IncreaseChangeStamp;
end;

{ TFPCSourceCache }

constructor TFPCSourceCaches.Create(AOwner: TComponent);
begin
  CTIncreaseChangeStamp(FChangeStamp); // set to not 0
  inherited Create(AOwner);
  fItems:=TAVLTree.Create(@CompareFPCSourceCacheItems);
end;

destructor TFPCSourceCaches.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TFPCSourceCaches.Clear;
begin
  if fItems.Count=0 then exit;
  fItems.FreeAndClear;
  IncreaseChangeStamp;
end;

procedure TFPCSourceCaches.Assign(Source: TPersistent);
var
  Caches: TFPCSourceCaches;
  SrcItem: TFPCSourceCache;
  NewItem: TFPCSourceCache;
  Node: TAVLTreeNode;
begin
  if Source is TFPCSourceCaches then begin
    Caches:=TFPCSourceCaches(Source);
    if Equals(Caches) then exit; // keep ChangeStamp if equal
    Clear;
    Node:=Caches.fItems.FindLowest;
    while Node<>nil do begin
      SrcItem:=TFPCSourceCache(Node.Data);
      NewItem:=TFPCSourceCache.Create(Self);
      NewItem.Assign(SrcItem);
      fItems.Add(NewItem);
      Node:=Caches.fItems.FindSuccessor(Node);
    end;
    IncreaseChangeStamp;
  end else
    inherited Assign(Source);
end;

function TFPCSourceCaches.Equals(Caches: TFPCSourceCaches): boolean;
var
  Node1, Node2: TAVLTreeNode;
  Item1: TFPCSourceCache;
  Item2: TFPCSourceCache;
begin
  Result:=false;
  if Caches.fItems.Count<>fItems.Count then exit;
  Node1:=fItems.FindLowest;
  Node2:=Caches.fItems.FindLowest;
  while Node1<>nil do begin
    Item1:=TFPCSourceCache(Node1.Data);
    Item2:=TFPCSourceCache(Node2.Data);
    if not Item1.Equals(Item2) then exit;
    Node1:=fItems.FindSuccessor(Node1);
    Node2:=Caches.fItems.FindSuccessor(Node2);
  end;
  Result:=true;
end;

procedure TFPCSourceCaches.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Cnt: integer;
  i: Integer;
  Item: TFPCSourceCache;
begin
  Clear;
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=1 to Cnt do begin
    Item:=TFPCSourceCache.Create(Self);
    Item.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    if (Item.Directory='') or (fItems.Find(Item)<>nil) then
      Item.Free
    else
      fItems.Add(Item);
  end;
end;

procedure TFPCSourceCaches.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  Node: TAVLTreeNode;
  Item: TFPCSourceCache;
  Cnt: Integer;
begin
  Cnt:=0;
  Node:=fItems.FindLowest;
  while Node<>nil do begin
    Item:=TFPCSourceCache(Node.Data);
    if Item.Directory<>'' then begin
      inc(Cnt);
      Item.SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(Cnt)+'/');
    end;
    Node:=fItems.FindSuccessor(Node);
  end;
  XMLConfig.SetDeleteValue(Path+'Count',Cnt,0);
end;

procedure TFPCSourceCaches.LoadFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'FPCSourceDirectories/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TFPCSourceCaches.SaveToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'FPCSourceDirectories/');
  finally
    XMLConfig.Free;
  end;
end;

procedure TFPCSourceCaches.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

function TFPCSourceCaches.Find(Directory: string;
  CreateIfNotExists: boolean): TFPCSourceCache;
var
  Node: TAVLTreeNode;
begin
  Directory:=ChompPathDelim(TrimFilename(Directory));
  Node:=fItems.FindKey(PChar(Directory),@CompareDirectoryWithFPCSourceCacheItem);
  if Node<>nil then begin
    Result:=TFPCSourceCache(Node.Data);
  end else if CreateIfNotExists then begin
    Result:=TFPCSourceCache.Create(Self);
    Result.Directory:=Directory;
    fItems.Add(Result);
  end else begin
    Result:=nil;
  end;
end;

{ TCompilerDefinesCache }

procedure TCompilerDefinesCache.SetConfigCaches(const AValue: TPCTargetConfigCaches);
begin
  if FConfigCaches=AValue then exit;
  FConfigCaches:=AValue;
  FConfigCachesSaveStamp:=Low(FConfigCachesSaveStamp);
end;

function TCompilerDefinesCache.GetExtraOptions: string;
begin
  Result:=ConfigCaches.ExtraOptions;
end;

function TCompilerDefinesCache.GetTestFilename: string;
begin
  Result:=ConfigCaches.TestFilename;
end;

procedure TCompilerDefinesCache.SetExtraOptions(AValue: string);
begin
  ConfigCaches.ExtraOptions:=AValue;
end;

procedure TCompilerDefinesCache.SetSourceCaches(const AValue: TFPCSourceCaches);
begin
  if FSourceCaches=AValue then exit;
  FSourceCaches:=AValue;
  FSourceCachesSaveStamp:=low(FSourceCachesSaveStamp);
end;

procedure TCompilerDefinesCache.ClearUnitToSrcCaches;
var
  i: Integer;
begin
  for i:=0 to fUnitToSrcCaches.Count-1 do
    TObject(fUnitToSrcCaches[i]).Free;
  fUnitToSrcCaches.Clear;
end;

procedure TCompilerDefinesCache.SetTestFilename(AValue: string);
begin
  ConfigCaches.TestFilename:=AValue;
end;

constructor TCompilerDefinesCache.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ConfigCaches:=TPCTargetConfigCaches.Create(nil);
  SourceCaches:=TFPCSourceCaches.Create(nil);
  fUnitToSrcCaches:=TFPList.Create;
end;

destructor TCompilerDefinesCache.Destroy;
begin
  ClearUnitToSrcCaches;
  FreeAndNil(FConfigCaches);
  FreeAndNil(FSourceCaches);
  FreeAndNil(fUnitToSrcCaches);
  inherited Destroy;
end;

procedure TCompilerDefinesCache.Clear;
begin
  ClearUnitToSrcCaches;
  if ConfigCaches<>nil then ConfigCaches.Clear;
  if SourceCaches<>nil then SourceCaches.Clear;
end;

procedure TCompilerDefinesCache.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  if ConfigCaches<>nil then begin
    ConfigCaches.LoadFromXMLConfig(XMLConfig,Path+'FPCConfigs/');
    FConfigCachesSaveStamp:=ConfigCaches.ChangeStamp;
  end;
  if SourceCaches<>nil then begin
    SourceCaches.LoadFromXMLConfig(XMLConfig,Path+'FPCSources/');
    FSourceCachesSaveStamp:=SourceCaches.ChangeStamp;
  end;
end;

procedure TCompilerDefinesCache.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  //debugln(['TCompilerDefinesCache.SaveToXMLConfig ']);
  if ConfigCaches<>nil then begin
    ConfigCaches.SaveToXMLConfig(XMLConfig,Path+'FPCConfigs/');
    FConfigCachesSaveStamp:=ConfigCaches.ChangeStamp;
  end;
  if SourceCaches<>nil then begin
    SourceCaches.SaveToXMLConfig(XMLConfig,Path+'FPCSources/');
    FSourceCachesSaveStamp:=SourceCaches.ChangeStamp;
  end;
end;

procedure TCompilerDefinesCache.LoadFromFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.Create(Filename);
  try
    LoadFromXMLConfig(XMLConfig,'');
  finally
    XMLConfig.Free;
  end;
end;

procedure TCompilerDefinesCache.SaveToFile(Filename: string);
var
  XMLConfig: TXMLConfig;
begin
  XMLConfig:=TXMLConfig.CreateClean(Filename);
  try
    SaveToXMLConfig(XMLConfig,'');
  finally
    XMLConfig.Free;
  end;
end;

function TCompilerDefinesCache.NeedsSave: boolean;
begin
  Result:=true;
  if (ConfigCaches<>nil) and (ConfigCaches.ChangeStamp<>FConfigCachesSaveStamp)
  then exit;
  if (SourceCaches<>nil) and (SourceCaches.ChangeStamp<>FSourceCachesSaveStamp)
  then exit;
  Result:=false;
end;

function TCompilerDefinesCache.GetFPCVersion(const CompilerFilename, TargetOS,
  TargetCPU: string; UseCompiledVersionAsDefault: boolean): string;
var
  Kind: TPascalCompiler;
begin
  Result:=GetPCVersion(CompilerFilename,TargetOS,TargetCPU,UseCompiledVersionAsDefault,Kind);
  if Kind=pcFPC then ;
end;

function TCompilerDefinesCache.GetPCVersion(const CompilerFilename, TargetOS,
  TargetCPU: string; UseCompiledVersionAsDefault: boolean; out
  Kind: TPascalCompiler): string;
var
  CfgCache: TPCTargetConfigCache;
  ErrorMsg: string;
begin
  Kind:=pcFPC;
  if UseCompiledVersionAsDefault then
    Result:={$I %FPCVersion%}
  else
    Result:='';
  if not IsCTExecutable(CompilerFilename,ErrorMsg) then
    exit;
  CfgCache:=ConfigCaches.Find(CompilerFilename,ExtraOptions,TargetOS,TargetCPU,true);
  if CfgCache.NeedsUpdate
  and not CfgCache.Update(TestFilename,ExtraOptions) then
    exit;
  Kind:=CfgCache.Kind;
  if CfgCache.FullVersion='' then exit;
  Result:=CfgCache.FullVersion;
end;

function TCompilerDefinesCache.FindUnitSet(const CompilerFilename, TargetOS,
  TargetCPU, Options, FPCSrcDir: string; CreateIfNotExists: boolean
  ): TFPCUnitSetCache;
var
  i: Integer;
begin
  for i:=0 to fUnitToSrcCaches.Count-1 do begin
    Result:=TFPCUnitSetCache(fUnitToSrcCaches[i]);
    if (CompareFilenames(Result.CompilerFilename,CompilerFilename)=0)
    and (SysUtils.CompareText(Result.TargetOS,TargetOS)=0)
    and (SysUtils.CompareText(Result.TargetCPU,TargetCPU)=0)
    and (CompareFilenames(Result.FPCSourceDirectory,FPCSrcDir)=0)
    and (Result.CompilerOptions=Options)
    then
      exit;
  end;
  if CreateIfNotExists then begin
    Result:=TFPCUnitSetCache.Create(Self);
    Result.CompilerFilename:=CompilerFilename;
    Result.CompilerOptions:=Options;
    Result.TargetOS:=TargetOS;
    Result.TargetCPU:=TargetCPU;
    Result.FPCSourceDirectory:=FPCSrcDir;
    fUnitToSrcCaches.Add(Result);
  end else
    Result:=nil;
end;

function TCompilerDefinesCache.FindUnitSetWithID(const UnitSetID: string; out
  Changed: boolean; CreateIfNotExists: boolean): TFPCUnitSetCache;
var
  CompilerFilename, TargetOS, TargetCPU, Options, FPCSrcDir: string;
  ChangeStamp: integer;
begin
  ParseUnitSetID(UnitSetID,CompilerFilename, TargetOS, TargetCPU,
                 Options, FPCSrcDir, ChangeStamp);
  //debugln(['TCompilerDefinesCache.FindUnitToSrcCache UnitSetID="',dbgstr(UnitSetID),'" CompilerFilename="',CompilerFilename,'" TargetOS="',TargetOS,'" TargetCPU="',TargetCPU,'" Options="',Options,'" FPCSrcDir="',FPCSrcDir,'" ChangeStamp=',ChangeStamp,' exists=',FindUnitToSrcCache(CompilerFilename, TargetOS, TargetCPU,Options, FPCSrcDir,false)<>nil]);
  Result:=FindUnitSet(CompilerFilename, TargetOS, TargetCPU,
                             Options, FPCSrcDir, false);
  if Result<>nil then begin
    Changed:=ChangeStamp<>Result.ChangeStamp;
  end else if CreateIfNotExists then begin
    Changed:=true;
    Result:=FindUnitSet(CompilerFilename, TargetOS, TargetCPU,
                               Options, FPCSrcDir, true);
  end else
    Changed:=false;
end;

function TCompilerDefinesCache.GetUnitSetID(CompilerFilename, TargetOS, TargetCPU,
  Options, FPCSrcDir: string; ChangeStamp: integer): string;
begin
  Result:='CompilerFilename='+CompilerFilename+LineEnding
         +'TargetOS='+TargetOS+LineEnding
         +'TargetCPU='+TargetCPU+LineEnding
         +'Options='+Options+LineEnding
         +'FPCSrcDir='+FPCSrcDir+LineEnding
         +'Stamp='+IntToStr(ChangeStamp);
end;

procedure TCompilerDefinesCache.ParseUnitSetID(const ID: string;
  out CompilerFilename, TargetOS, TargetCPU, Options, FPCSrcDir: string;
  out ChangeStamp: integer);
var
  NameStartPos: PChar;

  function NameFits(p: PChar): boolean;
  var
    p1: PChar;
  begin
    p1:=NameStartPos;
    while (FPUpChars[p1^]=FPUpChars[p^]) and (p^<>#0) do begin
      inc(p1);
      inc(p);
    end;
    Result:=p1^='=';
  end;

var
  ValueStartPos: PChar;
  ValueEndPos: PChar;
  Value: String;
begin
  CompilerFilename:='';
  TargetCPU:='';
  TargetOS:='';
  Options:='';
  FPCSrcDir:='';
  ChangeStamp:=0;
  if ID='' then exit;
  // read the lines with name=value
  NameStartPos:=PChar(ID);
  while NameStartPos^<>#0 do begin
    while (NameStartPos^ in [#10,#13]) do inc(NameStartPos);
    ValueStartPos:=NameStartPos;
    while not (ValueStartPos^ in ['=',#10,#13,#0]) do inc(ValueStartPos);
    if ValueStartPos^<>'=' then exit;
    inc(ValueStartPos);
    ValueEndPos:=ValueStartPos;
    while not (ValueEndPos^ in [#10,#13,#0]) do inc(ValueEndPos);
    Value:=copy(ID,ValueStartPos-PChar(ID)+1,ValueEndPos-ValueStartPos);
    //debugln(['TCompilerDefinesCache.ParseUnitSetID Name=',copy(ID,NameStartPos-PChar(ID)+1,ValueStartPos-NameStartPos-1),' Value="',Value,'"']);
    case NameStartPos^ of
    'c','C':
      if NameFits('CompilerFilename') then
        CompilerFilename:=Value;
    'f','F':
      if NameFits('FPCSrcDir') then
        FPCSrcDir:=Value;
    'o','O':
      if NameFits('Options') then
        Options:=Value;
    's','S':
      if NameFits('Stamp') then
        ChangeStamp:=StrToIntDef(Value,0);
    't','T':
      if NameFits('TargetOS') then
        TargetOS:=Value
      else if NameFits('TargetCPU') then
        TargetCPU:=Value;
    end;
    NameStartPos:=ValueEndPos;
  end;
end;

{ TFPCUnitSetCache }

procedure TFPCUnitSetCache.SetCompilerFilename(const AValue: string);
var
  NewFilename: String;
begin
  NewFilename:=ResolveDots(AValue);
  if FCompilerFilename=NewFilename then exit;
  FCompilerFilename:=NewFilename;
  ClearConfigCache;
end;

procedure TFPCUnitSetCache.SetCompilerOptions(const AValue: string);
begin
  if FCompilerOptions=AValue then exit;
  FCompilerOptions:=AValue;
  ClearConfigCache;
end;

procedure TFPCUnitSetCache.SetFPCSourceDirectory(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimAndExpandDirectory(AValue);
  if FFPCSourceDirectory=NewValue then exit;
  FFPCSourceDirectory:=NewValue;
  ClearSourceCache;
end;

procedure TFPCUnitSetCache.SetTargetCPU(const AValue: string);
begin
  if FTargetCPU=AValue then exit;
  FTargetCPU:=AValue;
  ClearConfigCache;
end;

procedure TFPCUnitSetCache.SetTargetOS(const AValue: string);
begin
  if FTargetOS=AValue then exit;
  FTargetOS:=AValue;
  ClearConfigCache;
end;

procedure TFPCUnitSetCache.ClearConfigCache;
begin
  FConfigCache:=nil;
  fFlags:=fFlags+[fuscfUnitTreeNeedsUpdate,fuscfSrcRulesNeedUpdate];
end;

procedure TFPCUnitSetCache.ClearSourceCache;
begin
  fSourceCache:=nil;
  Include(fFlags,fuscfUnitTreeNeedsUpdate);
end;

procedure TFPCUnitSetCache.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if FConfigCache=AComponent then
      ClearConfigCache;
    if fSourceCache=AComponent then
      ClearSourceCache;
  end;
end;

constructor TFPCUnitSetCache.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CTIncreaseChangeStamp(FChangeStamp); // set ot not 0
  FCaches:=TheOwner as TCompilerDefinesCache;
  fUnitToSourceTree:=TStringToStringTree.Create(false);
  fSrcDuplicates:=TStringToStringTree.Create(false);
  fSourceRules:=TFPCSourceRules.Create;
  fFlags:=[fuscfUnitTreeNeedsUpdate,fuscfSrcRulesNeedUpdate];
  fUnitStampOfFPC:=CTInvalidChangeStamp;
  fUnitStampOfFiles:=CTInvalidChangeStamp;
  fUnitStampOfRules:=CTInvalidChangeStamp;
end;

destructor TFPCUnitSetCache.Destroy;
begin
  FreeAndNil(fSourceRules);
  FreeAndNil(fUnitToSourceTree);
  FreeAndNil(fSrcDuplicates);
  inherited Destroy;
end;

procedure TFPCUnitSetCache.Clear;
begin

end;

procedure TFPCUnitSetCache.Init;
begin
  GetUnitToSourceTree(True);
end;

function TFPCUnitSetCache.GetConfigCache(AutoUpdate: boolean
  ): TPCTargetConfigCache;
begin
  if CompilerFilename='' then
    raise Exception.Create('TFPCUnitToSrcCache.GetConfigCache missing CompilerFilename');
  if Caches.TestFilename='' then
    raise Exception.Create('TFPCUnitToSrcCache.GetConfigCache missing TestFilename');
  if FConfigCache=nil then begin
    FConfigCache:=Caches.ConfigCaches.Find(CompilerFilename,CompilerOptions,
                                           TargetOS,TargetCPU,true);
    FConfigCache.FreeNotification(Self);
  end;
  //debugln(['TFPCUnitSetCache.GetConfigCache CompilerOptions="',CompilerOptions,'" FConfigCache.CompilerOptions="',FConfigCache.CompilerOptions,'"']);
  if AutoUpdate and FConfigCache.NeedsUpdate then
    FConfigCache.Update(Caches.TestFilename,Caches.ExtraOptions);
  Result:=FConfigCache;
end;

function TFPCUnitSetCache.GetSourceCache(AutoUpdate: boolean
  ): TFPCSourceCache;
begin
  if fSourceCache=nil then begin
    fSourceCache:=Caches.SourceCaches.Find(FPCSourceDirectory,true);
    fSourceCache.FreeNotification(Self);
  end;
  if AutoUpdate and (not fSourceCache.Valid) then
    fSourceCache.Update(nil);
  Result:=fSourceCache;
end;

function TFPCUnitSetCache.GetSourceRules(AutoUpdate: boolean
  ): TFPCSourceRules;
var
  Cfg: TPCTargetConfigCache;
  NewRules: TFPCSourceRules;
begin
  Cfg:=GetConfigCache(AutoUpdate);
  if (fuscfSrcRulesNeedUpdate in fFlags)
  or (fRulesStampOfConfig<>Cfg.ChangeStamp) then begin
    Exclude(fFlags,fuscfSrcRulesNeedUpdate);
    if Cfg.Kind=pcFPC then begin
      NewRules:=DefaultFPCSourceRules.Clone;
      try
        if Cfg.Units<>nil then
          AdjustFPCSrcRulesForPPUPaths(Cfg.Units,NewRules);
        fSourceRules.Assign(NewRules); // increases ChangeStamp if something changed
      finally
        NewRules.Free;
      end;
    end else begin
      fSourceRules.Clear;
    end;
    fRulesStampOfConfig:=Cfg.ChangeStamp;
  end;
  Result:=fSourceRules;
end;

function TFPCUnitSetCache.GetUnitToSourceTree(AutoUpdate: boolean
  ): TStringToStringTree;
var
  Src: TFPCSourceCache;
  SrcRules: TFPCSourceRules;
  NewUnitToSourceTree: TStringToStringTree;
  NewSrcDuplicates: TStringToStringTree;
  ConfigCache: TPCTargetConfigCache;
begin
  Src:=GetSourceCache(AutoUpdate);
  SrcRules:=GetSourceRules(AutoUpdate);
  ConfigCache:=GetConfigCache(false); // Note: update already done by GetSourceRules(AutoUpdate)

  if ConfigCache.Kind=pcFPC then begin
    if (fuscfUnitTreeNeedsUpdate in fFlags)
    or (fUnitStampOfFPC<>ConfigCache.ChangeStamp)
    or (fUnitStampOfFiles<>Src.ChangeStamp)
    or (fUnitStampOfRules<>SrcRules.ChangeStamp)
    then begin
      Exclude(fFlags,fuscfUnitTreeNeedsUpdate);
      NewSrcDuplicates:=nil;
      NewUnitToSourceTree:=nil;
      try
        NewSrcDuplicates:=TStringToStringTree.Create(false);
        NewUnitToSourceTree:=GatherUnitsInFPCSources(Src.Files,
                       ConfigCache.RealTargetOS,ConfigCache.RealTargetCPU,
                       NewSrcDuplicates,SrcRules);
        if NewUnitToSourceTree=nil then
          NewUnitToSourceTree:=TStringToStringTree.Create(false);
        // ToDo: add/replace sources in PPU search paths
        if not fUnitToSourceTree.Equals(NewUnitToSourceTree) then begin
          fUnitToSourceTree.Assign(NewUnitToSourceTree);
          IncreaseChangeStamp;
        end;
        if not fSrcDuplicates.Equals(NewSrcDuplicates) then begin
          fSrcDuplicates.Assign(NewSrcDuplicates);
          IncreaseChangeStamp;
        end;
        fUnitStampOfFPC:=ConfigCache.ChangeStamp;
        fUnitStampOfFiles:=Src.ChangeStamp;
        fUnitStampOfRules:=SrcRules.ChangeStamp;
      finally
        NewUnitToSourceTree.Free;
        NewSrcDuplicates.Free;
      end;
    end;
  end else begin
    fUnitToSourceTree.Clear;
    fSrcDuplicates.Clear;
    Exclude(fFlags,fuscfUnitTreeNeedsUpdate);
    fUnitStampOfFPC:=ConfigCache.ChangeStamp;
    fUnitStampOfFiles:=Src.ChangeStamp;
    fUnitStampOfRules:=SrcRules.ChangeStamp;
  end;
  Result:=fUnitToSourceTree;
end;

function TFPCUnitSetCache.GetSourceDuplicates(AutoUpdate: boolean
  ): TStringToStringTree;
begin
  GetUnitToSourceTree(AutoUpdate);
  Result:=fSrcDuplicates;
end;

function TFPCUnitSetCache.GetUnitSrcFile(const AnUnitName: string;
  SrcSearchRequiresPPU: boolean; SkipPPUCheckIfTargetIsSourceOnly: boolean): string;
{ Searches the unit in the FPC search path and sources.
  SrcSearchRequiresPPU: only search the sources if there is a ppu in the search path
}
var
  Tree: TStringToStringTree;
  ConfigCache: TPCTargetConfigCache;
  UnitInFPCPath: String;
begin
  Result:='';
  {$IFDEF ShowTriedUnits}
  debugln(['TFPCUnitSetCache.GetUnitSrcFile Unit="',AnUnitName,'" SrcSearchRequiresPPU=',SrcSearchRequiresPPU,' SkipPPUCheckIfTargetIsSourceOnly=',SkipPPUCheckIfTargetIsSourceOnly]);
  {$ENDIF}
  Tree:=GetUnitToSourceTree(false);
  ConfigCache:=GetConfigCache(false);
  if (ConfigCache.Units<>nil) then begin
    UnitInFPCPath:=ConfigCache.Units[AnUnitName];
    //if Pos('lazmkunit',AnUnitName)>0 then debugln(['TFPCUnitSetCache.GetUnitSrcFile UnitInFPCPath=',UnitInFPCPath]);
    if (CompareFileExt(UnitInFPCPath,'ppu',false)=0) then begin
      // there is a ppu
    end else if UnitInFPCPath<>'' then begin
      // there is a pp or pas in the FPC search path
      {$IFDEF ShowTriedUnits}
      debugln(['TFPCUnitSetCache.GetUnitSrcFile Unit="',AnUnitName,'" source in FPC search path: "',Result,'"']);
      {$ENDIF}
      Result:=UnitInFPCPath;
      exit;
    end else begin
      // unit has no ppu in the FPC ppu search path
      if SrcSearchRequiresPPU then begin
        if ConfigCache.HasPPUs then begin
          // but there are other ppu files
          {$IFDEF ShowTriedUnits}
          debugln(['TFPCUnitSetCache.GetUnitSrcFile Unit="',AnUnitName,'" unit has no ppu file in FPC path, but there are other ppu']);
          {$ENDIF}
          exit;
        end else begin
          // no ppu exists at all
          // => the fpc is not installed properly for this target
          {$IFDEF ShowTriedUnits}
          debugln(['TFPCUnitSetCache.GetUnitSrcFile Unit="',AnUnitName,'" there are no ppu files for this target']);
          {$ENDIF}
          if (not SkipPPUCheckIfTargetIsSourceOnly) then
            exit;
          // => search directly in the sources
          // this allows cross editing even if FPC is not installed for this target
        end;
      end;
    end;
  end;
  // search the sources
  if Tree<>nil then begin
    Result:=Tree[AnUnitName];
    if (Result<>'') and (not FilenameIsAbsolute(Result)) then
      Result:=FPCSourceDirectory+Result;
    {$IFDEF ShowTriedUnits}
    debugln(['TFPCUnitSetCache.GetUnitSrcFile Unit="',AnUnitName,'" Result=',Result]);
    {$ENDIF}
  end;
end;

function TFPCUnitSetCache.GetCompiledUnitFile(const AUnitName: string): string;
var
  ConfigCache: TPCTargetConfigCache;
begin
  Result:='';
  ConfigCache:=GetConfigCache(false);
  if ConfigCache.Units=nil then exit;
  Result:=ConfigCache.Units[AUnitName];
  if Result='' then exit;
  if CompareFileExt(Result,'.ppu',false)<>0 then
    Result:='';
end;

class function TFPCUnitSetCache.GetInvalidChangeStamp: integer;
begin
  Result:=CTInvalidChangeStamp;
end;

procedure TFPCUnitSetCache.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

function TFPCUnitSetCache.GetUnitSetID: string;
begin
  Result:=Caches.GetUnitSetID(CompilerFilename,TargetOS,TargetCPU,
                              CompilerOptions,FPCSourceDirectory,ChangeStamp);
end;

function TFPCUnitSetCache.GetFirstFPCCfg: string;
var
  Cfg: TPCTargetConfigCache;
  i: Integer;
  Files: TPCConfigFileStateList;
begin
  Result:='';
  Cfg:=GetConfigCache(false);
  if Cfg=nil then exit;
  Files:=Cfg.ConfigFiles;
  if Files=nil then exit;
  for i:=0 to Files.Count-1 do begin
    if Files[i].FileExists then begin
      Result:=Files[i].Filename;
      exit;
    end;
  end;
end;

function TFPCUnitSetCache.GetUnitScopes: string;
var
  Cfg: TPCTargetConfigCache;
  Scopes: TStrings;
  Scope: String;
  i: Integer;
begin
  Result:='';
  Cfg:=GetConfigCache(false);
  if Cfg=nil then exit;
  Scopes:=Cfg.UnitScopes;
  if Scopes=nil then exit;
  for i:=0 to Scopes.Count-1 do begin
    Scope:=Scopes[i];
    if Scope='' then continue;
    Result:=Result+';'+Scope;
  end;
  Delete(Result,1,1);
end;

function TFPCUnitSetCache.GetCompilerKind: TPascalCompiler;
var
  Cfg: TPCTargetConfigCache;
begin
  Cfg:=GetConfigCache(false);
  if Cfg=nil then exit(pcFPC);
  Result:=Cfg.Kind;
end;

initialization
  InitDefaultFPCSourceRules;

finalization
  FreeAndNil(DefaultFPCSourceRules);

end.


