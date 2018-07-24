{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

}
unit H2PasConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LResources, Forms, Controls, Dialogs, XMLPropStorage,
  // LazUtils
  LazConfigStorage, FileUtil, LazFileUtils, LazFileCache, LazUTF8Classes,
  // CodeTools
  CodeAtom, CodeTree, KeywordFuncLists, NonPascalCodeTools, BasicCodeTools,
  FileProcs, CodeCache, SourceChanger, CodeToolManager,
  // IDEIntf
  TextTools, IDEExternToolIntf, IDEDialogs, LazIDEIntf, SrcEditorIntf,
  IDEMsgIntf, IDETextConverter,
  // H2Pas
  H2PasStrConsts;
  
type

  { TRemoveCPlusPlusExternCTool  (for C header files)
    Remove C++ 'extern "C"' lines }

  TRemoveCPlusPlusExternCTool = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TRemoveEmptyCMacrosTool   (for C header files)
    Remove empty C macros}

  TRemoveEmptyCMacrosTool = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;
  
  
  { TReplaceEdgedBracketPairWithStar  (for C header files)
    Replace [] with * }

  TReplaceEdgedBracketPairWithStar = class(TCustomTextReplaceTool)
  public
    class function ClassDescription: string; override;
    constructor Create(TheOwner: TComponent); override;
  end;


  { TReplaceMacro0PointerWithNULL  (for C header files)
    Replace macro values 0 pointer like (char *)0 with NULL }

  TReplaceMacro0PointerWithNULL = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TConvertFunctionTypesToPointers  (for C header files)
    Replace function types with pointer to function type }

  TConvertFunctionTypesToPointers = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TConvertEnumsToTypeDef  (for C header files)
    Give anoymous enums a name }

  TConvertEnumsToTypeDef = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TCommentComplexCMacros (for C header files)
    Comment macros that are too complex for h2pas }

  TCommentComplexCMacros = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TCommentComplexCFunctions (for C header files)
    Comment functions that are too complex for h2pas }

  TCommentComplexCFunctions = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TAddMissingMacroBrackets (for C header files)
    Add missing brackets around macro values }

  TAddMissingMacroBrackets = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TReplaceUnitFilenameWithUnitName -
    Replace "unit filename;" with "unit name;" }

  TReplaceUnitFilenameWithUnitName = class(TCustomTextReplaceTool)
  public
    class function ClassDescription: string; override;
    constructor Create(TheOwner: TComponent); override;
  end;


  { TRemoveIncludeDirectives - Remove all $i filename }

  TRemoveIncludeDirectives = class(TCustomTextReplaceTool)
  public
    class function ClassDescription: string; override;
    constructor Create(TheOwner: TComponent); override;
  end;


  { TRemoveDoubleSemicolons -
    Remove double semicolons }

  TRemoveDoubleSemicolons = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TRemoveSystemTypes -
    Remove type redefinitions like PLongint }

  TRemoveSystemTypes = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TRemoveRedefinedPointerTypes - Remove redefined pointer types }

  TRemoveRedefinedPointerTypes = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TRemoveEmptyTypeVarConstSections - Remove empty type/var/const sections }

  TRemoveEmptyTypeVarConstSections = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TReplaceImplicitTypes -
    Search implicit types in parameters and add types for them
    For example:
        procedure ProcName(a: array[0..2] of char);
      is replaced with
        procedure ProcName(a: Tarray_0to2_of_char);
      and a new type is added
        Tarray_0to2_of_char = array[0..2] of char;
       }

  TReplaceImplicitTypes = class(TCustomTextConverterTool)
  private
    Src: String;
    ImplicitTypes: TAVLTree;// tree of TImplicitType
    ExplicitTypes: TAVLTree;// tree of TImplicitType
    TypeStart: LongInt;
    TypeEnd: integer; // 0 means invalid
    ConstSectionStart: LongInt;
    ConstSectionEnd: LongInt; // 0 means invalid
    function FindNextImplicitType(var Position: integer;
                                  out aTypeStart, aTypeEnd: integer): boolean;
    function SearchImplicitParameterTypes(
                                        var ModalResult: TModalResult): boolean;
    function PosToStr(Position: integer): string;
    procedure AdjustMinPositions(const Identifier: string);
    function ReadWord(var Position: integer): boolean;
    function ReadUntilAtom(var Position: integer;
                 const StopAtom: string; SkipBrackets: boolean = true): boolean;
    function ReadRecord(var Position: integer): boolean;
    function ReadClass(var Position: integer): boolean;
    function ReadTypeDefinition(var Position: integer): boolean;
    function ReadConstSection(var Position: integer): boolean;
    function FindExplicitTypesAndConstants(
                                        var ModalResult: TModalResult): boolean;
    function InsertNewTypes(var ModalResult: TModalResult): boolean;
    function FindInsertPosition(MinPos: integer): integer;
    function UseNewTypes(var ModalResult: TModalResult): boolean;
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
    function CodeToIdentifier(const Code: string): string;
  end;
  

  { TFixArrayOfParameterType - Replace "array of )" with "array of const)" }

  TFixArrayOfParameterType = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;
  
  
  { TRemoveRedefinitionsInUnit
    Removes redefinitions of types, variables, constants and resourcestrings }

  TRemoveRedefinitionsInUnit = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;
  

  { TAddMissingPointerTypes
    Add missing pointer types like PPPChar }

  TAddMissingPointerTypes = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;


  { TFixAliasDefinitionsInUnit - fix section type of alias definitions

    Checks all alias definitions of the form
    const LeftSide = RightSide;
    looks up RightSide in the unit and if RightSide is a type or var, changes
    the section accordingly }
    
  TFixAliasDefinitionsInUnit = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;
  
  
  { TFixH2PasMissingIFDEFsInUnit - add missing IFDEFs for function bodies }

  TFixH2PasMissingIFDEFsInUnit = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;
  

  { TReduceCompilerDirectivesInUnit - removes unneeded directives }

  TReduceCompilerDirectivesInUnit = class(TCustomTextConverterTool)
  private
    FDefines: TStrings;
    FUndefines: TStrings;
    procedure SetDefines(const AValue: TStrings);
    procedure SetUndefines(const AValue: TStrings);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  published
    property Undefines: TStrings read FUndefines write SetUndefines;
    property Defines: TStrings read FDefines write SetDefines;
  end;


  { TReplaceConstFunctionsInUnit - replace simple assignment functions with constants }

  TReplaceConstFunctionsInUnit = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;

  { TReplaceTypeCastFunctionsInUnit - replace simple type cast functions with types }

  TReplaceTypeCastFunctionsInUnit = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;

  { TFixForwardDefinitions - reorder definitions }

  TFixForwardDefinitions = class(TCustomTextConverterTool)
  public
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  end;

  { TAddToUsesSection - add units to uses section }

  TAddToUsesSection = class(TCustomTextConverterTool)
  private
    FUseUnits: TStrings;
    procedure SetUseUnits(const AValue: TStrings);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  published
    property UseUnits: TStrings read FUseUnits write SetUseUnits;
  end;

type
  { TPretH2PasTools - Combines the common tools. }

  TPreH2PasToolsOption = (
    phRemoveCPlusPlusExternCTool, // Remove C++ 'extern "C"' lines
    phRemoveEmptyCMacrosTool, // Remove empty C macros
    phReplaceEdgedBracketPairWithStar, // Replace [] with *
    phReplaceMacro0PointerWithNULL, // Replace macro values 0 pointer like (char *)0
    phConvertFunctionTypesToPointers, // Convert function types to pointers
    phConvertEnumsToTypeDef, // Convert anonymous enums to ypedef enums
    phCommentComplexCMacros, // Comment macros too complex for hpas
    phCommentComplexCFunctions, // Comment functions too complex for hpas
    phAddMissingMacroBrackets // Add missing macro brackets
    );
  TPreH2PasToolsOptions = set of TPreH2PasToolsOption;
const
  DefaultPreH2PasToolsOptions =
                        [Low(TPreH2PasToolsOption)..High(TPreH2PasToolsOption)];

type
  { TPreH2PasTools }

  TPreH2PasTools = class(TCustomTextConverterTool)
  private
    FOptions: TPreH2PasToolsOptions;
  public
    constructor Create(TheOwner: TComponent); override;
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  published
    property Options: TPreH2PasToolsOptions read FOptions write FOptions default DefaultPreH2PasToolsOptions;
  end;

type
  { TPostH2PasTools - Combines the common tools. }
  TPostH2PasToolsOption = (
    phReplaceUnitFilenameWithUnitName, // Replace "unit filename;" with "unit name;"
    phRemoveIncludeDirectives, // remove include directives
    phRemoveDoubleSemicolons, // Remove double semicolons
    phRemoveSystemTypes, // Remove type redefinitons like PLongint
    phFixH2PasMissingIFDEFsInUnit, // add missing IFDEFs for function bodies
    phReduceCompilerDirectivesInUnit, // removes unneeded directives
    phRemoveRedefinedPointerTypes, // Remove redefined pointer types
    phRemoveEmptyTypeVarConstSections, // Remove empty type/var/const sections
    phReplaceImplicitTypes, // Search implicit types in parameters and add types for them
    phFixArrayOfParameterType, // Replace "array of )" with "array of const)"
    phAddMissingPointerTypes, // add missing pointer types
    phRemoveRedefinitionsInUnit, // Removes redefinitions of types, variables, constants and resourcestrings
    phFixAliasDefinitionsInUnit, // fix section type of alias definitions
    phReplaceConstFunctionsInUnit, // replace simple assignment functions with constants
    phReplaceTypeCastFunctionsInUnit, // replace simple type cast functions with types
    phFixForwardDefinitions, // fix forward definitions by reordering
    phAddUnitsToUsesSection // add units to uses section
    );
  TPostH2PasToolsOptions = set of TPostH2PasToolsOption;
const
  DefaultPostH2PasToolsOptions =
                        [Low(TPostH2PasToolsOption)..High(TPostH2PasToolsOption)];
type
  TPostH2PasTools = class(TCustomTextConverterTool)
  private
    FDefines: TStrings;
    FOptions: TPostH2PasToolsOptions;
    FUndefines: TStrings;
    FUseUnits: TStrings;
    procedure SetDefines(const AValue: TStrings);
    procedure SetUndefines(const AValue: TStrings);
    procedure SetUseUnits(const AValue: TStrings);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function ClassDescription: string; override;
    function Execute(aText: TIDETextConverter): TModalResult; override;
  published
    property Undefines: TStrings read FUndefines write SetUndefines;
    property Defines: TStrings read FDefines write SetDefines;
    property UseUnits: TStrings read FUseUnits write SetUseUnits;
    property Options: TPostH2PasToolsOptions read FOptions write FOptions default DefaultPostH2PasToolsOptions;
  end;

  TH2PasFile = class;

  { TH2PasFileCInclude }

  TH2PasFileCInclude = class
  private
    FFilename: string;
    FH2PasFile: TH2PasFile;
    FOwner: TH2PasFile;
    FSrcFilename: string;
    FSrcPos: TPoint;
    procedure SetFilename(const AValue: string);
    procedure SetH2PasFile(const AValue: TH2PasFile);
    procedure SetSrcFilename(const AValue: string);
    procedure SetSrcPos(const AValue: TPoint);
  public
    constructor Create(TheOwner: TH2PasFile);
    destructor Destroy; override;
    property Owner: TH2PasFile read FOwner;
    property SrcFilename: string read FSrcFilename write SetSrcFilename;
    property SrcPos: TPoint read FSrcPos write SetSrcPos;
    property Filename: string read FFilename write SetFilename;
    property H2PasFile: TH2PasFile read FH2PasFile write SetH2PasFile;
  end;

  TH2PasProject = class;
  TH2PasConverter = class;

  { TH2PasFile }

  TH2PasFile = class(TPersistent)
  private
    FCIncludes: TFPList; // list of TH2PasFileCInclude
    FCIncludesValid: boolean;
    FCIncludesFileAge: TDateTime;
    FCIncludedBy: TFPList; // list of TH2PasFileCInclude
    FEnabled: boolean;
    FFilename: string;
    FMerge: boolean;
    FMergedBy: TH2PasFile;
    FModified: boolean;
    FProject: TH2PasProject;
    function GetCIncludeCount: integer;
    function GetCIncludedBy(Index: integer): TH2PasFileCInclude;
    function GetCIncludedByCount: integer;
    function GetCIncludes(Index: integer): TH2PasFileCInclude;
    procedure SetEnabled(const AValue: boolean);
    procedure SetFilename(const AValue: string);
    procedure SetMerge(const AValue: boolean);
    procedure SetModified(const AValue: boolean);
    procedure SetProject(const AValue: TH2PasProject);
    procedure SearchCIncFilenames;
    procedure InternalAddCIncludedBy(CIncludedBy: TH2PasFileCInclude);
    procedure InternalRemoveCIncludedBy(CIncludedBy: TH2PasFileCInclude);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearIncludedByReferences;
    procedure ClearCIncludes;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AFile: TH2PasFile): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
    function GetOutputFilename: string;
    function GetOutputDirectory: string;
    function GetOutputExtension: string;
    function GetH2PasParameters(const InputFilename: string = ''): string;
    function ReadCIncludes(ForceUpdate: boolean): TModalResult;
    function CIncludesValid: boolean;
    function FindCIncludedByWithOwner(ByOwner: TH2PasFile): TH2PasFileCInclude;
  public
    property Project: TH2PasProject read FProject write SetProject;
    property Filename: string read FFilename write SetFilename;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Modified: boolean read FModified write SetModified;
    property CIncludeCount: integer read GetCIncludeCount;
    property CIncludes[Index: integer]: TH2PasFileCInclude read GetCIncludes;
    property CIncludedByCount: integer read GetCIncludedByCount;
    property CIncludedBy[Index: integer]: TH2PasFileCInclude read GetCIncludedBy;
    property Merge: boolean read FMerge write SetMerge;
    property MergedBy: TH2PasFile read FMergedBy;// automatically chosen by the project
  end;

  { TH2PasProject }

  TH2PasProject = class(TPersistent)
  private
    FBaseDir: string;
    FCHeaderFiles: TFPList;// list of TH2PasFile
    FCompactOutputmode: boolean;
    FConstantsInsteadOfEnums: boolean;
    FConverter: TH2PasConverter;
    FCreateIncludeFile: boolean;
    FFilename: string;
    FIsVirtual: boolean;
    FLibname: string;
    FModified: boolean;
    FOutputDirectory: string;
    FOutputExt: string;
    FPackAllRecords: boolean;
    FPalmOSSYSTrap: boolean;
    FPforPointers: boolean;
    FPostH2PasTools: TComponent;
    FPreH2PasTools: TComponent;
    FStripComments: boolean;
    FStripCommentsAndInfo: boolean;
    FTforTypedefs: boolean;
    FTforTypedefsRemoveUnderscore: boolean;
    FUseExternal: boolean;
    FUseExternalLibname: boolean;
    FUseProcVarsForImport: boolean;
    FVarParams: boolean;
    FWin32Header: boolean;
    FUseCTypes : boolean;
    function GetCHeaderFileCount: integer;
    function GetCHeaderFiles(Index: integer): TH2PasFile;
    procedure InternalAddCHeaderFile(AFile: TH2PasFile);
    procedure InternalRemoveCHeaderFile(AFile: TH2PasFile);
    procedure SetCompactOutputmode(const AValue: boolean);
    procedure SetConstantsInsteadOfEnums(const AValue: boolean);
    procedure SetCreateIncludeFile(const AValue: boolean);
    procedure SetFilename(const AValue: string);
    procedure SetLibname(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure FilenameChanged;
    procedure SetOutputDirectory(const AValue: string);
    procedure SetOutputExt(const AValue: string);
    procedure SetPackAllRecords(const AValue: boolean);
    procedure SetPalmOSSYSTrap(const AValue: boolean);
    procedure SetPforPointers(const AValue: boolean);
    procedure SetStripComments(const AValue: boolean);
    procedure SetStripCommentsAndInfo(const AValue: boolean);
    procedure SetTforTypedefs(const AValue: boolean);
    procedure SetTforTypedefsRemoveUnderscore(const AValue: boolean);
    procedure SetUseExternal(const AValue: boolean);
    procedure SetUseExternalLibname(const AValue: boolean);
    procedure SetUseProcVarsForImport(const AValue: boolean);
    procedure SetVarParams(const AValue: boolean);
    procedure SetWin32Header(const AValue: boolean);
    procedure SetUseCTypes(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(AddDefaults: boolean);
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AProject: TH2PasProject): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    procedure AddFiles(List: TStrings);
    procedure DeleteFiles(List: TStrings);
    function CHeaderFileWithFilename(const AFilename: string): TH2PasFile;
    function CHeaderFileIndexWithFilename(const AFilename: string): integer;
    procedure CHeaderFileMove(OldIndex, NewIndex: integer);
    function ShortenFilename(const AFilename: string): string;
    function LongenFilename(const AFilename: string): string;
    function NormalizeFilename(const AFilename: string): string;
    function HasEnabledFiles: boolean;
    procedure AddDefaultPreH2PasTools;
    procedure AddDefaultPostH2PasTools;
    function SearchIncludedCHeaderFile(aFile: TH2PasFile;
                                       const SrcFilename: string): string;
    function ReadAllCIncludes(ForceUpdate: boolean): TModalResult;
  public
    property CHeaderFileCount: integer read GetCHeaderFileCount;
    property CHeaderFiles[Index: integer]: TH2PasFile read GetCHeaderFiles;
    property Modified: boolean read FModified write SetModified;
    property Filename: string read FFilename write SetFilename;
    property BaseDir: string read FBaseDir;
    property IsVirtual: boolean read FIsVirtual;
    property Converter: TH2PasConverter read FConverter;
    property PreH2PasTools: TComponent read FPreH2PasTools;
    property PostH2PasTools: TComponent read FPostH2PasTools;

    // h2pas options
    property ConstantsInsteadOfEnums: boolean read FConstantsInsteadOfEnums write SetConstantsInsteadOfEnums;
    property CompactOutputmode: boolean read FCompactOutputmode write SetCompactOutputmode;
    property CreateIncludeFile: boolean read FCreateIncludeFile write SetCreateIncludeFile;
    property Libname: string read FLibname write SetLibname;
    property OutputExt: string read FOutputExt write SetOutputExt;
    property PalmOSSYSTrap: boolean read FPalmOSSYSTrap write SetPalmOSSYSTrap;
    property PforPointers: boolean read FPforPointers write SetPforPointers;
    property PackAllRecords: boolean read FPackAllRecords write SetPackAllRecords;
    property StripComments: boolean read FStripComments write SetStripComments;
    property StripCommentsAndInfo: boolean read FStripCommentsAndInfo write SetStripCommentsAndInfo;
    property TforTypedefs: boolean read FTforTypedefs write SetTforTypedefs;
    property TforTypedefsRemoveUnderscore: boolean read FTforTypedefsRemoveUnderscore write SetTforTypedefsRemoveUnderscore;
    property UseExternal: boolean read FUseExternal write SetUseExternal;
    property UseExternalLibname: boolean read FUseExternalLibname write SetUseExternalLibname;
    property UseProcVarsForImport: boolean read FUseProcVarsForImport write SetUseProcVarsForImport;
    property VarParams: boolean read FVarParams write SetVarParams;
    property Win32Header: boolean read FWin32Header write SetWin32Header;
    property UseCTypes: boolean read FUseCTypes write SetUseCTypes;
    property OutputDirectory: string read FOutputDirectory write SetOutputDirectory;
  end;

const
  SubToolH2Pas = 'h2pas';
type
  { TH2PasParser }

  TH2PasParser = class(TExtToolParser)
  public
    class function DefaultSubTool: string; override;
    procedure ReadLine(Line: string; OutputIndex: integer; var {%H-}Handled: boolean
      ); override; // (worker thread)
  end;

  { TH2PasTool }

  TH2PasTool = class(TIDEExternalToolOptions)
  private
    FH2PasFile: TH2PasFile;
    FTargetFilename: string;
  public
    property H2PasFile: TH2PasFile read FH2PasFile write FH2PasFile;
    property TargetFilename: string read FTargetFilename write FTargetFilename;
  end;
  
  { TH2PasConverter }

  TH2PasConverter = class(TPersistent)
  private
    FAutoOpenLastProject: boolean;
    FExecuting: boolean;
    Fh2pasFilename: string;
    FLastUsedFilename: string;
    FModified: boolean;
    FProject: TH2PasProject;
    FProjectHistory: TStrings;
    FWindowBounds: TRect;
    function GetCurrentProjectFilename: string;
    procedure SetAutoOpenLastProject(const AValue: boolean);
    procedure SetCurrentProjectFilename(const AValue: string);
    procedure SetProject(const AValue: TH2PasProject);
    procedure SetProjectHistory(const AValue: TStrings);
    procedure SetWindowBounds(const AValue: TRect);
    procedure Seth2pasFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AConverter: TH2PasConverter): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    procedure LoadProject(const Filename: string);
    procedure SaveProject(const Filename: string);
    function Execute: TModalResult;
    function ConvertFile(AFile: TH2PasFile): TModalResult;
    function CheckMergeDependencies: TModalResult;
    function MergeIncludeFiles(AFile: TH2PasFile;
                               TextConverter: TIDETextConverter): TModalResult;
    function GetH2PasFilename: string;
    function FileIsRelated(const aFilename: string): Boolean;
  public
    property Project: TH2PasProject read FProject write SetProject;
    property ProjectHistory: TStrings read FProjectHistory write SetProjectHistory;
    property CurrentProjectFilename: string read GetCurrentProjectFilename
                                            write SetCurrentProjectFilename;
    property WindowBounds: TRect read FWindowBounds write SetWindowBounds;
    property AutoOpenLastProject: boolean read FAutoOpenLastProject
                                          write SetAutoOpenLastProject;
    property h2pasFilename: string read Fh2pasFilename write Seth2pasFilename;
    property Modified: boolean read FModified write FModified;
    property Executing: boolean read FExecuting;
    property LastUsedFilename: string read FLastUsedFilename;
  end;
  
const
  PreDefinedH2PasTypes: array[1..10] of string = (
    'Char',
    'Byte',
    'SmallInt',
    'Word',
    'Longint',
    'DWord',
    'Int64',
    'QWord',
    'Single',
    'Double'
    );

implementation

{ TH2PasParser }

class function TH2PasParser.DefaultSubTool: string;
begin
  Result:=SubToolH2Pas;
end;

procedure TH2PasParser.ReadLine(Line: string; OutputIndex: integer;
  var Handled: boolean);

  function ReadString(var p: PChar; Expected: PChar): boolean;
  begin
    while Expected^<>#0 do begin
      if p^<>Expected^ then exit(false);
      inc(p);
      inc(Expected);
    end;
    Result:=true;
  end;

var
  p: PChar;
  LineNumber: Integer;
  Msg: String;
  MsgLine: TMessageLine;
begin
  p:=PChar(Line);
  // read 'at line '
  if not ReadString(p,'at line ') then exit;
  // read line number
  if not (p^ in ['0'..'9']) then exit;
  LineNumber:=0;
  while (LineNumber<999999) and (p^ in ['0'..'9']) do begin
    LineNumber:=LineNumber*10+ord(p^)-ord('0');
    inc(p);
  end;
  // read ' error : '
  if not ReadString(p,' error : ') then exit;
  Msg:=p;
  MsgLine:=CreateMsgLine(OutputIndex);
  MsgLine.SubTool:=SubToolH2Pas;
  MsgLine.Urgency:=mluError;
  MsgLine.Msg:=Msg;
end;

{ TH2PasFile }

procedure TH2PasFile.SetFilename(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FFilename=NewValue then exit;
  FFilename:=NewValue;
  FCIncludesValid:=false;
  Modified:=true;
end;

procedure TH2PasFile.SetMerge(const AValue: boolean);
begin
  if FMerge=AValue then exit;
  FMerge:=AValue;
  Modified:=true;
end;

procedure TH2PasFile.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Modified:=true;
end;

function TH2PasFile.GetCIncludeCount: integer;
begin
  if (FCIncludes=nil) or (not FCIncludesValid) then
    Result:=0
  else
    Result:=FCIncludes.Count;
end;

function TH2PasFile.GetCIncludedBy(Index: integer): TH2PasFileCInclude;
begin
  Result:=TH2PasFileCInclude(FCIncludedBy[Index]);
end;

function TH2PasFile.GetCIncludedByCount: integer;
begin
  if (FCIncludedBy=nil) then
    Result:=0
  else
    Result:=FCIncludedBy.Count;
end;

function TH2PasFile.GetCIncludes(Index: integer): TH2PasFileCInclude;
begin
  Result:=TH2PasFileCInclude(FCIncludes[Index]);
end;

procedure TH2PasFile.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if FModified and (Project<>nil) then
    Project.Modified:=true;
end;

procedure TH2PasFile.SetProject(const AValue: TH2PasProject);
begin
  if FProject=AValue then exit;
  FCIncludesValid:=false;
  if FProject<>nil then begin
    FProject.InternalRemoveCHeaderFile(Self);
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    FProject.InternalAddCHeaderFile(Self);
  end;
  Modified:=true;
end;

procedure TH2PasFile.SearchCIncFilenames;
var
  i: Integer;
  IncFile: TH2PasFileCInclude;
begin
  if FCIncludes=nil then exit;
  if Project=nil then exit;
  for i:=0 to FCIncludes.Count-1 do begin
    IncFile:=CIncludes[i];
    IncFile.Filename:=
                    Project.SearchIncludedCHeaderFile(Self,IncFile.SrcFilename);
    IncFile.H2PasFile:=Project.CHeaderFileWithFilename(IncFile.Filename);
  end;
end;

procedure TH2PasFile.InternalAddCIncludedBy(CIncludedBy: TH2PasFileCInclude);
begin
  if FCIncludedBy=nil then
    FCIncludedBy:=TFPList.Create;
  FCIncludedBy.Add(CIncludedBy);
  //DebugLn(['TH2PasFile.InternalAddCIncludedBy ',Filename,' included by ',CIncludedBy.Filename]);
end;

procedure TH2PasFile.InternalRemoveCIncludedBy(CIncludedBy: TH2PasFileCInclude);
begin
  if FCIncludedBy=nil then exit;
  FCIncludedBy.Remove(CIncludedBy);
end;

constructor TH2PasFile.Create;
begin
  Clear;
end;

destructor TH2PasFile.Destroy;
begin
  if FProject<>nil then begin
    Project:=nil;
  end;
  Clear;
  ClearIncludedByReferences;
  FCIncludedBy.Free;
  inherited Destroy;
end;

procedure TH2PasFile.Clear;
begin
  FEnabled:=true;
  FFilename:='';
  FModified:=false;
  FMerge:=false;
  FMergedBy:=nil;
  ClearCIncludes;
end;

procedure TH2PasFile.ClearIncludedByReferences;
var
  i: Integer;
  IncFile: TH2PasFileCInclude;
begin
  if FCIncludedBy=nil then exit;
  for i:=FCIncludedBy.Count-1 downto 0 do begin
    IncFile:=TH2PasFileCInclude(FCIncludedBy[i]);
    if IncFile=nil then continue;
    IncFile.FH2PasFile:=nil;
  end;
  FCIncludedBy.Clear;
end;

procedure TH2PasFile.ClearCIncludes;
var
  i: Integer;
  IncFile: TH2PasFileCInclude;
begin
  FCIncludesValid:=false;
  if FCIncludes<>nil then begin
    for i:=0 to FCIncludes.Count-1 do begin
      IncFile:=TH2PasFileCInclude(FCIncludes[i]);
      IncFile.Free;
    end;
    FreeAndNil(FCIncludes);
  end;
end;

procedure TH2PasFile.Assign(Source: TPersistent);
var
  Src: TH2PasFile;
begin
  if Source is TH2PasFile then begin
    Src:=TH2PasFile(Source);
    if not IsEqual(Src) then begin
      FEnabled:=Src.FEnabled;
      FFilename:=Src.FFilename;
      FCIncludesValid:=false;
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasFile.IsEqual(AFile: TH2PasFile): boolean;
begin
  Result:=(CompareFilenames(Filename,AFile.Filename)=0)
          and (Enabled=AFile.Enabled)
          and (Merge=AFile.Merge);
end;

procedure TH2PasFile.Load(Config: TConfigStorage);
begin
  FEnabled:=Config.GetValue('Enabled/Value',true);
  FMerge:=Config.GetValue('Merge/Value',true);
  FFilename:=Config.GetValue('Filename/Value','');
  if Project<>nil then
    FFilename:=Project.NormalizeFilename(FFilename);
  FCIncludesValid:=false;
  FModified:=false;
end;

procedure TH2PasFile.Save(Config: TConfigStorage);
var
  AFilename: String;
begin
  Config.SetDeleteValue('Enabled/Value',Enabled,true);
  Config.SetDeleteValue('Merge/Value',Merge,true);
  AFilename:=FFilename;
  if Project<>nil then
    AFilename:=Project.ShortenFilename(AFilename);
  Config.SetDeleteValue('Filename/Value',AFilename,'');
  FModified:=false;
end;

function TH2PasFile.GetOutputFilename: string;
begin
  Result:=GetOutputDirectory+ExtractFileNameOnly(Filename)+GetOutputExtension;
end;

function TH2PasFile.GetOutputDirectory: string;
begin
  Result:=Project.OutputDirectory;
  if Result='' then
    Result:=Project.BaseDir;
end;

function TH2PasFile.GetOutputExtension: string;
begin
  Result:=Project.OutputExt;
end;

function TH2PasFile.GetH2PasParameters(const InputFilename: string): string;

  procedure Add(const AnOption: string);
  begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+AnOption;
  end;

begin
  Result:='';
  if Project.ConstantsInsteadOfEnums then Add('-e');
  if Project.CompactOutputmode then Add('-c');
  if Project.CreateIncludeFile then Add('-i');
  if Project.PalmOSSYSTrap then Add('-x');
  if Project.PforPointers then Add('-p');
  if Project.PackAllRecords then Add('-pr');
  if Project.StripComments then Add('-s');
  if Project.StripCommentsAndInfo then Add('-S');
  if Project.TforTypedefs then Add('-t');
  if Project.TforTypedefsRemoveUnderscore then Add('-T');
  if Project.UseExternal then Add('-d');
  if Project.UseExternalLibname then Add('-D');
  if Project.UseProcVarsForImport then Add('-P');
  if Project.VarParams then Add('-v');
  if Project.Win32Header then Add('-w');
  if Project.UseCTypes then Add('-C');
  if Project.Libname<>'' then Add('-l '+Project.Libname);
  Add('-o '+GetOutputFilename);
  if InputFilename<>'' then
    Add(InputFilename)
  else
    Add(Filename);
end;

function TH2PasFile.ReadCIncludes(ForceUpdate: boolean): TModalResult;
var
  sl: TStringList;
  i: Integer;
  SrcFilename: String;
  Item: TH2PasFileCInclude;
begin
  if (not ForceUpdate) and CIncludesValid then exit(mrOk);
  Result:=mrCancel;
  if not FileExistsCached(Filename) then exit;
  ClearCIncludes;
  FCIncludesFileAge:=FileAgeUTF8(Filename);
  FCIncludesValid:=true;
  //DebugLn(['TH2PasFile.ReadCIncludes Filename="',Filename,'"']);
  try
    sl:=TStringListUTF8.Create;
    try
      sl.LoadFromFile(Filename);
      for i:=0 to sl.Count-1 do begin
        if not REMatches(sl[i],'^#include "(.+)"') then continue;
        SrcFilename:=Trim(REVar(1));
        if SrcFilename='' then continue;
        // add new include
        if FCIncludes=nil then FCIncludes:=TFPList.Create;
        Item:=TH2PasFileCInclude.Create(Self);
        Item.SrcFilename:=SrcFilename;
        Item.SrcPos:=Point(1,i);
        //DebugLn(['TH2PasFile.ReadCIncludes Self=',Filename,' include=',SrcFilename,' ',dbgs(Item.SrcPos)]);
        FCIncludes.Add(Item);
      end;
    finally
      sl.Free;
    end;
    SearchCIncFilenames;
    Result:=mrOk;
  except
    on e: Exception do begin
      DebugLn(['TH2PasFile.ReadCIncludes File="',Filename,'" Msg=',E.Message]);
    end;
  end;
end;

function TH2PasFile.CIncludesValid: boolean;
begin
  Result:=false;
  if not FCIncludesValid then exit;
  FCIncludesValid:=false;
  if Project=nil then exit;
  if (not FileExistsCached(Filename)) then exit;
  if FileAgeUTF8(Filename)>FCIncludesFileAge then exit;
  FCIncludesValid:=true;
  Result:=true;
end;

function TH2PasFile.FindCIncludedByWithOwner(ByOwner: TH2PasFile
  ): TH2PasFileCInclude;
var
  i: Integer;
begin
  if FCIncludedBy<>nil then begin
    for i:=0 to CIncludedByCount-1 do begin
      Result:=CIncludedBy[i];
      if Result.Owner=ByOwner then exit;
    end;
  end;
  Result:=nil;
end;

{ TH2PasProject }

function TH2PasProject.GetCHeaderFileCount: integer;
begin
  Result:=FCHeaderFiles.Count;
end;

function TH2PasProject.GetCHeaderFiles(Index: integer): TH2PasFile;
begin
  Result:=TH2PasFile(FCHeaderFiles[Index]);
end;

procedure TH2PasProject.InternalAddCHeaderFile(AFile: TH2PasFile);
begin
  FCHeaderFiles.Add(AFile);
end;

procedure TH2PasProject.InternalRemoveCHeaderFile(AFile: TH2PasFile);
begin
  FCHeaderFiles.Remove(AFile);
end;

procedure TH2PasProject.SetCompactOutputmode(const AValue: boolean);
begin
  if FCompactOutputmode=AValue then exit;
  FCompactOutputmode:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetConstantsInsteadOfEnums(const AValue: boolean);
begin
  if FConstantsInsteadOfEnums=AValue then exit;
  FConstantsInsteadOfEnums:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetCreateIncludeFile(const AValue: boolean);
begin
  if FCreateIncludeFile=AValue then exit;
  FCreateIncludeFile:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetFilename(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FFilename=NewValue then exit;
  FFilename:=NewValue;
  FilenameChanged;
  Modified:=true;
end;

procedure TH2PasProject.SetLibname(const AValue: string);
begin
  if FLibname=AValue then exit;
  FLibname:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

procedure TH2PasProject.FilenameChanged;
begin
  FIsVirtual:=(FFilename='') or (not FilenameIsAbsolute(FFilename));
  FBaseDir:=ExtractFilePath(FFilename);
end;

procedure TH2PasProject.SetOutputDirectory(const AValue: string);
begin
  if FOutputDirectory=AValue then exit;
  FOutputDirectory:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetOutputExt(const AValue: string);
begin
  if FOutputExt=AValue then exit;
  FOutputExt:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetPackAllRecords(const AValue: boolean);
begin
  if FPackAllRecords=AValue then exit;
  FPackAllRecords:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetPalmOSSYSTrap(const AValue: boolean);
begin
  if FPalmOSSYSTrap=AValue then exit;
  FPalmOSSYSTrap:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetPforPointers(const AValue: boolean);
begin
  if FPforPointers=AValue then exit;
  FPforPointers:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetStripComments(const AValue: boolean);
begin
  if FStripComments=AValue then exit;
  FStripComments:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetStripCommentsAndInfo(const AValue: boolean);
begin
  if FStripCommentsAndInfo=AValue then exit;
  FStripCommentsAndInfo:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetTforTypedefs(const AValue: boolean);
begin
  if FTforTypedefs=AValue then exit;
  FTforTypedefs:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetTforTypedefsRemoveUnderscore(const AValue: boolean);
begin
  if FTforTypedefsRemoveUnderscore=AValue then exit;
  FTforTypedefsRemoveUnderscore:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseExternal(const AValue: boolean);
begin
  if FUseExternal=AValue then exit;
  FUseExternal:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseExternalLibname(const AValue: boolean);
begin
  if FUseExternalLibname=AValue then exit;
  FUseExternalLibname:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseProcVarsForImport(const AValue: boolean);
begin
  if FUseProcVarsForImport=AValue then exit;
  FUseProcVarsForImport:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetVarParams(const AValue: boolean);
begin
  if FVarParams=AValue then exit;
  FVarParams:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetWin32Header(const AValue: boolean);
begin
  if FWin32Header=AValue then exit;
  FWin32Header:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseCTypes(const AValue: boolean);
begin
  if FUseCTypes=AValue then exit;
  FUseCTypes:=AValue;
  Modified:=true;
end;

constructor TH2PasProject.Create;
begin
  FCHeaderFiles:=TFPList.Create;
  Clear(true);
end;

destructor TH2PasProject.Destroy;
begin
  Clear(false);
  if (Converter<>nil) and (Converter.Project=Self) then
    Converter.Project:=nil;
  FreeAndNil(FCHeaderFiles);
  FreeAndNil(FPreH2PasTools);
  FreeAndNil(FPostH2PasTools);
  inherited Destroy;
end;

procedure TH2PasProject.Clear(AddDefaults: boolean);
begin
  // FFilename is kept
  FConstantsInsteadOfEnums:=true;
  FCompactOutputmode:=false;
  FCreateIncludeFile:=false;
  FLibname:='';
  FOutputExt:='.pas';
  FPackAllRecords:=false;
  FPalmOSSYSTrap:=false;
  FPforPointers:=true;
  FStripComments:=false;
  FStripCommentsAndInfo:=false;
  FTforTypedefs:=false;
  FTforTypedefsRemoveUnderscore:=false;
  FUseExternal:=false;
  FUseExternalLibname:=true;
  FUseProcVarsForImport:=false;
  FVarParams:=false;
  FWin32Header:=true;
  FUseCTypes:=false;
  FOutputDirectory:='';
  while CHeaderFileCount>0 do
    CHeaderFiles[CHeaderFileCount-1].Free;
  FPreH2PasTools.Free;
  FPreH2PasTools:=TComponent.Create(nil);
  FPostH2PasTools.Free;
  FPostH2PasTools:=TComponent.Create(nil);
  if AddDefaults then 
  begin
    AddDefaultPreH2PasTools;
    AddDefaultPostH2PasTools;
  end;
  FModified:=false;
end;

procedure TH2PasProject.Assign(Source: TPersistent);

  procedure CopyTools(SrcList: TComponent; var DestList: TComponent);
  var
    SrcComponent: TComponent;
    NewComponent: TObject;
    i: Integer;
  begin
    DestList.Free;
    DestList:=TComponent.Create(nil);
    for i:=0 to SrcList.ComponentCount-1 do begin
      SrcComponent:=SrcList.Components[i];
      if SrcComponent is TCustomTextConverterTool then begin
        NewComponent:=
               TComponentClass(SrcComponent.ClassType).Create(DestList);
        TCustomTextConverterTool(NewComponent).Assign(SrcComponent);
      end;
    end;
  end;

var
  Src: TH2PasProject;
  i: Integer;
  NewCHeaderFile: TH2PasFile;
begin
  if Source is TH2PasProject then begin
    Src:=TH2PasProject(Source);
    if not IsEqual(Src) then begin
      // FFilename is kept
      FConstantsInsteadOfEnums:=Src.FConstantsInsteadOfEnums;
      FCompactOutputmode:=Src.FCompactOutputmode;
      FCreateIncludeFile:=Src.FCreateIncludeFile;
      FLibname:=Src.FLibname;
      FOutputExt:=Src.FOutputExt;
      FPackAllRecords:=Src.FPackAllRecords;
      FPalmOSSYSTrap:=Src.FPalmOSSYSTrap;
      FPforPointers:=Src.FPforPointers;
      FStripComments:=Src.FStripComments;
      FStripCommentsAndInfo:=Src.FStripCommentsAndInfo;
      FTforTypedefs:=Src.FTforTypedefs;
      FTforTypedefsRemoveUnderscore:=Src.FTforTypedefsRemoveUnderscore;
      FUseExternal:=Src.FUseExternal;
      FUseExternalLibname:=Src.FUseExternalLibname;
      FUseProcVarsForImport:=Src.FUseProcVarsForImport;
      FVarParams:=Src.FVarParams;
      FWin32Header:=Src.FWin32Header;
      FUseCTypes:=Src.FUseCTypes;
      FOutputDirectory:=Src.FOutputDirectory;
      Clear(false);
      for i:=0 to Src.CHeaderFileCount-1 do begin
        NewCHeaderFile:=TH2PasFile.Create;
        NewCHeaderFile.Project:=Self;
        NewCHeaderFile.Assign(Src.CHeaderFiles[i]);
      end;
      CopyTools(Src.FPreH2PasTools,FPreH2PasTools);
      CopyTools(Src.FPostH2PasTools,FPostH2PasTools);
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasProject.IsEqual(AProject: TH2PasProject): boolean;
var
  i: Integer;
begin
  Result:=(AProject.CHeaderFileCount=CHeaderFileCount)
      and (FConstantsInsteadOfEnums=AProject.FConstantsInsteadOfEnums)
      and (FCompactOutputmode=AProject.FCompactOutputmode)
      and (FCreateIncludeFile=AProject.FCreateIncludeFile)
      and (FLibname=AProject.FLibname)
      and (FOutputExt=AProject.FOutputExt)
      and (FPackAllRecords=AProject.FPackAllRecords)
      and (FPalmOSSYSTrap=AProject.FPalmOSSYSTrap)
      and (FPforPointers=AProject.FPforPointers)
      and (FStripComments=AProject.FStripComments)
      and (FStripCommentsAndInfo=AProject.FStripCommentsAndInfo)
      and (FTforTypedefs=AProject.FTforTypedefs)
      and (FTforTypedefsRemoveUnderscore=AProject.FTforTypedefsRemoveUnderscore)
      and (FUseExternal=AProject.FUseExternal)
      and (FUseExternalLibname=AProject.FUseExternalLibname)
      and (FUseProcVarsForImport=AProject.FUseProcVarsForImport)
      and (FVarParams=AProject.FVarParams)
      and (FWin32Header=AProject.FWin32Header)
      and (FUseCTypes=AProject.FUseCTypes)
      and (FOutputDirectory=AProject.FOutputDirectory);
  if not Result then exit;
  for i:=0 to CHeaderFileCount-1 do
    if not CHeaderFiles[i].IsEqual(AProject.CHeaderFiles[i]) then
      exit(false);
  if (not CompareComponents(FPreH2PasTools,AProject.FPreH2PasTools))
  or (not CompareComponents(FPostH2PasTools,AProject.FPostH2PasTools)) then
    exit(false);
end;

procedure TH2PasProject.Load(Config: TConfigStorage);
  procedure LoadTools(const SubPath: string; List: TComponent);
  var
    NewComponent: TComponent;
    NewCount: LongInt;
    i: Integer;
  begin
    // load PreH2PasTools
    Config.AppendBasePath(SubPath);
    try
      NewCount:=Config.GetValue('Count',0);
      for i:=0 to NewCount-1 do begin
        Config.AppendBasePath('Tool'+IntToStr(i+1));
        try
          NewComponent:=nil;
          LoadComponentFromConfig(Config,'Value',NewComponent,
                                  @TextConverterToolClasses.FindClass,List);
        finally
          Config.UndoAppendBasePath;
        end;
      end;
    finally
      Config.UndoAppendBasePath;
    end;
  end;

var
  NewCount: LongInt;
  i: Integer;
  NewCHeaderFile: TH2PasFile;
begin
  Clear(false);
  
  // FFilename is not saved
  FConstantsInsteadOfEnums:=Config.GetValue('ConstantsInsteadOfEnums/Value',true);
  FCompactOutputmode:=Config.GetValue('CompactOutputmode/Value',false);
  FCreateIncludeFile:=Config.GetValue('CreateIncludeFile/Value',false);
  FLibname:=Config.GetValue('Libname/Value','');
  FOutputExt:=Config.GetValue('OutputExt/Value','.pas');
  FPackAllRecords:=Config.GetValue('PackAllRecords/Value',false);
  FPalmOSSYSTrap:=Config.GetValue('PalmOSSYSTrap/Value',false);
  FPforPointers:=Config.GetValue('PforPointers/Value',true);
  FStripComments:=Config.GetValue('StripComments/Value',false);
  FStripCommentsAndInfo:=Config.GetValue('StripCommentsAndInfo/Value',false);
  FTforTypedefs:=Config.GetValue('TforTypedefs/Value',false);
  FTforTypedefsRemoveUnderscore:=Config.GetValue('TforTypedefsRemoveUnderscore/Value',false);
  FUseExternal:=Config.GetValue('UseExternal/Value',false);
  FUseExternalLibname:=Config.GetValue('UseExternalLibname/Value',true);
  FUseProcVarsForImport:=Config.GetValue('UseProcVarsForImport/Value',false);
  FVarParams:=Config.GetValue('VarParams/Value',false);
  FWin32Header:=Config.GetValue('Win32Header/Value',true);
  FUseCTypes:=Config.GetValue('UseCTypes/Value',false);
  FOutputDirectory:=NormalizeFilename(Config.GetValue('OutputDirectory/Value',''));

  // load CHeaderFiles
  Config.AppendBasePath('CHeaderFiles');
  try
    NewCount:=Config.GetValue('Count',0);
    for i:=0 to NewCount-1 do begin
      Config.AppendBasePath('File'+IntToStr(i+1));
      try
        NewCHeaderFile:=TH2PasFile.Create;
        NewCHeaderFile.Project:=Self;
        NewCHeaderFile.Load(Config);
      finally
        Config.UndoAppendBasePath;
      end;
    end;
  finally
    Config.UndoAppendBasePath;
  end;

  LoadTools('PreH2PasTools',FPreH2PasTools);
  LoadTools('PostH2PasTools',FPostH2PasTools);

  FModified:=false;
end;

procedure TH2PasProject.Save(Config: TConfigStorage);

  procedure SaveTools(const SubPath: string; List: TComponent);
  var
    i: Integer;
  begin
    Config.AppendBasePath(SubPath);
    try
      Config.SetDeleteValue('Count',List.ComponentCount,0);
      for i:=0 to List.ComponentCount-1 do begin
        Config.AppendBasePath('Tool'+IntToStr(i+1));
        try
          SaveComponentToConfig(Config,'Value',List.Components[i]);
        finally
          Config.UndoAppendBasePath;
        end;
      end;
    finally
      Config.UndoAppendBasePath;
    end;
  end;

var
  i: Integer;
begin
  // FFilename is kept
  Config.SetDeleteValue('ConstantsInsteadOfEnums/Value',FConstantsInsteadOfEnums,true);
  Config.SetDeleteValue('CompactOutputmode/Value',FCompactOutputmode,false);
  Config.SetDeleteValue('CreateIncludeFile/Value',FCreateIncludeFile,false);
  Config.SetDeleteValue('Libname/Value',FLibname,'');
  Config.SetDeleteValue('OutputExt/Value',FOutputExt,'.pas');
  Config.SetDeleteValue('PackAllRecords/Value',FPackAllRecords,false);
  Config.SetDeleteValue('PalmOSSYSTrap/Value',FPalmOSSYSTrap,false);
  Config.SetDeleteValue('PforPointers/Value',FPforPointers,true);
  Config.SetDeleteValue('StripComments/Value',FStripComments,false);
  Config.SetDeleteValue('StripCommentsAndInfo/Value',FStripCommentsAndInfo,false);
  Config.SetDeleteValue('TforTypedefs/Value',FTforTypedefs,false);
  Config.SetDeleteValue('TforTypedefsRemoveUnderscore/Value',FTforTypedefsRemoveUnderscore,false);
  Config.SetDeleteValue('UseExternal/Value',FUseExternal,false);
  Config.SetDeleteValue('UseExternalLibname/Value',FUseExternalLibname,true);
  Config.SetDeleteValue('UseProcVarsForImport/Value',FUseProcVarsForImport,false);
  Config.SetDeleteValue('VarParams/Value',FVarParams,false);
  Config.SetDeleteValue('Win32Header/Value',FWin32Header,true);
  Config.SetDeleteValue('UseCTypes/Value',FUseCTypes,false);
  Config.SetDeleteValue('OutputDirectory/Value',ShortenFilename(FOutputDirectory),'');

  // save CHeaderFiles
  Config.AppendBasePath('CHeaderFiles');
  try
    Config.SetDeleteValue('Count',CHeaderFileCount,0);
    for i:=0 to CHeaderFileCount-1 do begin
      Config.AppendBasePath('File'+IntToStr(i+1));
      try
        CHeaderFiles[i].Save(Config);
      finally
        Config.UndoAppendBasePath;
      end;
    end;
  finally
    Config.UndoAppendBasePath;
  end;
  
  SaveTools('PreH2PasTools',FPreH2PasTools);
  SaveTools('PostH2PasTools',FPostH2PasTools);
  FModified:=false;
end;

procedure TH2PasProject.LoadFromFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,true);
  try
    Load(Config);
  finally
    Config.Free;
  end;
end;

procedure TH2PasProject.SaveToFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,false);
  try
    Save(Config);
    DebugLn(['TH2PasProject.SaveToFile ',AFilename]);
    Config.WriteToDisk;
  finally
    Config.Free;
  end;
end;

procedure TH2PasProject.AddFiles(List: TStrings);
var
  i: Integer;
  NewFilename: string;
  NewFile: TH2PasFile;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do begin
    NewFilename:=CleanAndExpandFilename(List[i]);
    if (NewFilename='') or (not FileExistsUTF8(NewFilename)) then exit;
    if CHeaderFileWithFilename(NewFilename)<>nil then exit;
    NewFile:=TH2PasFile.Create;
    NewFile.Project:=Self;
    NewFile.Filename:=NewFilename;
  end;
end;

procedure TH2PasProject.DeleteFiles(List: TStrings);
var
  i: Integer;
  NewFilename: String;
  CurFile: TH2PasFile;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do begin
    NewFilename:=CleanAndExpandFilename(List[i]);
    if (NewFilename='') then exit;
    CurFile:=CHeaderFileWithFilename(NewFilename);
    if CurFile<>nil then begin
      CurFile.Free;
    end;
  end;
end;

function TH2PasProject.CHeaderFileWithFilename(const AFilename: string
  ): TH2PasFile;
var
  i: LongInt;
begin
  i:=CHeaderFileIndexWithFilename(AFilename);
  if i>=0 then
    Result:=CHeaderFiles[i]
  else
    Result:=nil;
end;

function TH2PasProject.CHeaderFileIndexWithFilename(const AFilename: string
  ): integer;
begin
  Result:=CHeaderFileCount-1;
  while (Result>=0)
  and (CompareFilenames(AFilename,CHeaderFiles[Result].Filename)<>0) do
    dec(Result);
end;

procedure TH2PasProject.CHeaderFileMove(OldIndex, NewIndex: integer);
begin
  FCHeaderFiles.Move(OldIndex,NewIndex);
end;

function TH2PasProject.ShortenFilename(const AFilename: string): string;
begin
  if IsVirtual then
    Result:=AFilename
  else
    Result:=CreateRelativePath(AFilename,fBaseDir);
end;

function TH2PasProject.LongenFilename(const AFilename: string): string;
begin
  if IsVirtual then
    Result:=AFilename
  else if not FilenameIsAbsolute(AFilename) then
    Result:=TrimFilename(BaseDir+AFilename);
end;

function TH2PasProject.NormalizeFilename(const AFilename: string): string;
begin
  Result:=LongenFilename(GetForcedPathDelims(AFilename));
end;

function TH2PasProject.HasEnabledFiles: boolean;
var
  i: Integer;
begin
  for i:=0 to CHeaderFileCount-1 do
    if CHeaderFiles[i].Enabled and (not CHeaderFiles[i].Merge) then exit(true);
  Result:=false;
end;

procedure TH2PasProject.AddDefaultPreH2PasTools;
begin
  AddNewTextConverterTool(FPreH2PasTools,TPreH2PasTools);
end;

procedure TH2PasProject.AddDefaultPostH2PasTools;
begin
  AddNewTextConverterTool(FPostH2PasTools,TPostH2PasTools);
end;

function TH2PasProject.SearchIncludedCHeaderFile(aFile: TH2PasFile;
  const SrcFilename: string): string;
var
  AFilename: String;
  i: Integer;
  CurFile: TH2PasFile;
begin
  AFilename:=GetForcedPathDelims(SrcFilename);
  if System.Pos(PathDelim,AFilename)>0 then begin
    // with sub path -> only search relative to AFile
    Result:=TrimFilename(ExtractFilePath(aFile.Filename)+AFilename);
    if FileExistsCached(Result) then exit;
  end else begin
    // search relative to AFile
    Result:=TrimFilename(ExtractFilePath(aFile.Filename)+AFilename);
    if FileExistsCached(Result) then exit;
    // search relative to all other .h files
    for i:=0 to CHeaderFileCount-1 do begin
      CurFile:=CHeaderFiles[i];
      Result:=TrimFilename(ExtractFilePath(CurFile.Filename)+AFilename);
      if FileExistsCached(Result) then exit;
    end;
  end;
  Result:='';
end;

function TH2PasProject.ReadAllCIncludes(ForceUpdate: boolean): TModalResult;
var
  i: Integer;
  CurFile: TH2PasFile;
  DefaultMergeFile: TH2PasFile;
begin
  // read includes
  DefaultMergeFile:=nil;
  for i:=0 to CHeaderFileCount-1 do begin
    CurFile:=CHeaderFiles[i];
    CurFile.FMergedBy:=nil;
    Result:=CurFile.ReadCIncludes(ForceUpdate);
    if Result=mrAbort then exit;
    if (not CurFile.Merge) then
      DefaultMergeFile:=CurFile;
  end;
  
  // create merge connections
  for i:=0 to CHeaderFileCount-1 do begin
    CurFile:=CHeaderFiles[i];
    if CurFile.Merge and (CurFile.CIncludedByCount=0) then begin
      // this file should be merged, but is not included by any other file
      // append it to the first unit
      CurFile.FMergedBy:=DefaultMergeFile;
    end;
  end;
  
  Result:=mrOk;
end;

{ TH2PasConverter }

function TH2PasConverter.GetCurrentProjectFilename: string;
begin
  if FProjectHistory.Count>0 then
    Result:=FProjectHistory[FProjectHistory.Count-1]
  else
    Result:='';
end;

procedure TH2PasConverter.SetAutoOpenLastProject(const AValue: boolean);
begin
  if FAutoOpenLastProject=AValue then exit;
  FAutoOpenLastProject:=AValue;
  Modified:=true;
end;

procedure TH2PasConverter.SetCurrentProjectFilename(const AValue: string);
const
  ProjectHistoryMax=30;
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if NewValue='' then exit;
  if CompareFilenames(GetCurrentProjectFilename,NewValue)=0 then exit;
  FProjectHistory.Add(NewValue);
  while FProjectHistory.Count>ProjectHistoryMax do
    FProjectHistory.Delete(0);
  Modified:=true;
end;

procedure TH2PasConverter.SetProject(const AValue: TH2PasProject);
begin
  if FProject=AValue then exit;
  if FProject<>nil then begin
    FProject.fConverter:=nil;
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    FProject.fConverter:=Self;
    if FProject.Filename<>'' then
      CurrentProjectFilename:=FProject.Filename;
  end;
end;

procedure TH2PasConverter.SetProjectHistory(const AValue: TStrings);
begin
  if FProjectHistory=AValue then exit;
  FProjectHistory.Assign(AValue);
end;

procedure TH2PasConverter.SetWindowBounds(const AValue: TRect);
begin
  if CompareRect(@FWindowBounds,@AValue) then exit;
  FWindowBounds:=AValue;
  Modified:=true;
end;

procedure TH2PasConverter.Seth2pasFilename(const AValue: string);
begin
  if Fh2pasFilename=AValue then exit;
  Fh2pasFilename:=AValue;
  Modified:=true;
end;

constructor TH2PasConverter.Create;
begin
  FProjectHistory:=TStringList.Create;
  Clear;
end;

destructor TH2PasConverter.Destroy;
begin
  FreeAndNil(FProject);
  Clear;
  FreeAndNil(FProjectHistory);
  inherited Destroy;
end;

procedure TH2PasConverter.Clear;
begin
  FAutoOpenLastProject:=true;
  if FProject<>nil then FreeAndNil(FProject);
  FProjectHistory.Clear;
  FWindowBounds:=Rect(0,0,0,0);
  Fh2pasFilename:='h2pas';
  FModified:=false;
end;

procedure TH2PasConverter.Assign(Source: TPersistent);
var
  Src: TH2PasConverter;
begin
  if Source is TH2PasConverter then begin
    Src:=TH2PasConverter(Source);
    if not IsEqual(Src) then begin
      Clear;
      // Note: project is kept unchanged
      FProjectHistory.Assign(Src.FProjectHistory);
      FWindowBounds:=Src.FWindowBounds;
      Fh2pasFilename:=Src.Fh2pasFilename;
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasConverter.IsEqual(AConverter: TH2PasConverter): boolean;
begin
  if (FAutoOpenLastProject<>AConverter.FAutoOpenLastProject)
  or (not CompareRect(@FWindowBounds,@AConverter.FWindowBounds))
  or (Fh2pasFilename<>AConverter.h2pasFilename)
  or (not FProjectHistory.Equals(AConverter.FProjectHistory))
  then
    exit(false);
  Result:=true;
end;

procedure TH2PasConverter.Load(Config: TConfigStorage);
var
  i: Integer;
begin
  FAutoOpenLastProject:=Config.GetValue('AutoOpenLastProject/Value',true);
  Fh2pasFilename:=Config.GetValue('h2pas/Filename','h2pas');
  Config.GetValue('WindowBounds/',FWindowBounds,Rect(0,0,0,0));
  Config.GetValue('ProjectHistory/',FProjectHistory);
  for i:=FProjectHistory.Count-1 downto 0 do
    if FProjectHistory[i]='' then FProjectHistory.Delete(i);
  
  // Note: project is saved in its own file
end;

procedure TH2PasConverter.Save(Config: TConfigStorage);
begin
  Config.SetDeleteValue('AutoOpenLastProject/Value',FAutoOpenLastProject,true);
  Config.SetDeleteValue('h2pas/Filename',Fh2pasFilename,'h2pas');
  Config.SetDeleteValue('WindowBounds/',FWindowBounds,Rect(0,0,0,0));
  Config.SetValue('ProjectHistory/',FProjectHistory);
end;

procedure TH2PasConverter.LoadFromFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,true);
  try
    Load(Config);
  finally
    Config.Free;
  end;
end;

procedure TH2PasConverter.SaveToFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,false);
  try
    Save(Config);
    Config.WriteToDisk;
  finally
    Config.Free;
  end;
end;

procedure TH2PasConverter.LoadProject(const Filename: string);
begin
  DebugLn(['TH2PasConverter.LoadProject ',Filename]);
  if FProject=nil then
    FProject:=TH2PasProject.Create;
  FProject.Filename:=Filename;
  FProject.LoadFromFile(Filename);
  CurrentProjectFilename:=Filename;
end;

procedure TH2PasConverter.SaveProject(const Filename: string);
begin
  DebugLn(['TH2PasConverter.SaveProject ',Filename]);
  FProject.Filename:=Filename;
  FProject.SaveToFile(Filename);
  CurrentProjectFilename:=Filename;
end;

function TH2PasConverter.Execute: TModalResult;
var
  i: Integer;
  AFile: TH2PasFile;
  CurResult: TModalResult;
begin
  if FExecuting then begin
    DebugLn(['TH2PasConverter.Execute FAILED: Already executing']);
    exit(mrCancel);
  end;

  Result:=mrOK;
  FExecuting:=true;
  try
    FLastUsedFilename:='';

    CurResult:=CheckMergeDependencies;
    if CurResult=mrAbort then begin
      DebugLn(['TH2PasConverter.Execute aborted because merging not possible']);
      exit(mrAbort);
    end;

    // convert every c header file
    for i:=0 to Project.CHeaderFileCount-1 do begin
      AFile:=Project.CHeaderFiles[i];
      if not AFile.Enabled then continue;
      if AFile.Merge then continue;
      CurResult:=ConvertFile(AFile);
      if CurResult=mrAbort then begin
        DebugLn(['TH2PasConverter.Execute aborted on file ',AFile.Filename]);
        exit(mrAbort);
      end;
      if CurResult<>mrOK then Result:=mrCancel;
    end;
  finally
    FExecuting:=false;
  end;
end;

function TH2PasConverter.ConvertFile(AFile: TH2PasFile): TModalResult;
var
  TextConverter: TIDETextConverter;

  procedure CloseOrRevertEditorFile(const Filename: string);
  begin
    if FileExistsUTF8(Filename) then
      LazarusIDE.DoRevertEditorFile(Filename)
    else
      LazarusIDE.DoCloseEditorFile(Filename,[cfQuiet]);
  end;
  
  function ExecuteTools(List: TComponent; const DefaultFilename: string
    ): TModalResult;
  var
    ErrorComponent: TComponent;
    ErrorTool: TCustomTextConverterTool;
    ErrMsg: String;
    Line: Integer;
    Col: Integer;
    Filename: String;
    BaseDir: String;
  begin
    Result:=TextConverter.Execute(List,ErrorComponent);
    if Result=mrOk then exit;
    Line:=0;
    Col:=0;
    Filename:='';
    if ErrorComponent is TCustomTextConverterTool then begin
      ErrorTool:=TCustomTextConverterTool(ErrorComponent);
      Line:=ErrorTool.ErrorLine;
      Col:=ErrorTool.ErrorColumn;
      Filename:=ErrorTool.ErrorFilename;
    end;
    if Filename='' then
      Filename:=DefaultFilename;
    // create error message
    BaseDir:=ExtractFilePath(Project.BaseDir);
    ErrMsg:=CreateRelativePath(Filename,BaseDir);

    if Line>0 then begin
      ErrMsg:=ErrMsg+'('+IntToStr(Line)+',';
      if Col>0 then
        ErrMsg:=ErrMsg+IntToStr(Col)
      else
        ErrMsg:=ErrMsg+'1';
      ErrMsg:=ErrMsg+')';
    end;
    ErrMsg:=ErrMsg+' Error: '+ErrorTool.ErrorMsg+' ('+ErrorTool.Caption+')';
    DebugLn(['TH2PasConverter.ConvertFile Failed: ',ErrMsg]);
    IDEMessagesWindow.AddCustomMessage(mluError,ErrorTool.ErrorMsg,Filename,Line,Col,ErrorTool.Caption);
    LazarusIDE.DoJumpToCompilerMessage(true);
    Result:=mrAbort;
  end;

var
  OutputFilename: String;
  TempCHeaderFilename: String;
  InputFilename: String;
  Tool: TH2PasTool;
begin
  Result:=mrCancel;
  FLastUsedFilename:='';
  
  // check if file exists
  InputFilename:=AFile.Filename;
  if not FileExistsCached(InputFilename) then begin
    Result := IDEMessageDialog(h2pFileNotFound,
      Format(h2pCHeaderFileNotFound, [InputFilename]),
      mtError,[mbCancel,mbAbort],'');
    exit;
  end;

  OutputFilename:=AFile.GetOutputFilename;
  TempCHeaderFilename:=ChangeFileExt(OutputFilename,'.tmp.h');
  TextConverter:=TIDETextConverter.Create(nil);
  try
    if not CopyFile(InputFilename,TempCHeaderFilename) then begin
      Result := IDEMessageDialog(h2pCopyingFileFailed,
        Format(h2pUnableToCopyFileTo, [InputFilename, #13, TempCHeaderFilename]),
        mtError,[mbCancel,mbAbort],'');
      exit;
    end;
    
    TextConverter.Filename:=TempCHeaderFilename;
    FLastUsedFilename:=TextConverter.Filename;
    DebugLn(['TH2PasConverter.ConvertFile TempCHeaderFilename="',TempCHeaderFilename,'" CurrentType=',ord(TextConverter.CurrentType),' FileSize=',FileSize(TempCHeaderFilename)]);
    
    // merge files
    TextConverter.LoadFromFile(InputFilename);
    Result:=MergeIncludeFiles(AFile,TextConverter);
    if Result<>mrOk then begin
      DebugLn(['TH2PasConverter.ConvertFile Failed merging include files in ',TempCHeaderFilename]);
      exit;
    end;

    // run converters for .h file to make it compatible for h2pas
    Result:=ExecuteTools(Project.PreH2PasTools,TempCHeaderFilename);
    if Result<>mrOk then begin
      DebugLn(['TH2PasConverter.ConvertFile Failed running Project.PreH2PasTools on ',TempCHeaderFilename]);
      exit;
    end;

    //DebugLn(['TH2PasConverter.ConvertFile CCC1 ',TextConverter.Source]);
    // run h2pas
    Tool:=TH2PasTool.Create;
    try
      Tool.Title:='h2pas';
      Tool.H2PasFile:=AFile;
      Tool.TargetFilename:=TextConverter.Filename;
      Tool.Executable:=GetH2PasFilename;
      Tool.CmdLineParams:=AFile.GetH2PasParameters(Tool.TargetFilename);
      Tool.WorkingDirectory:=Project.BaseDir;
      DebugLn(['TH2PasConverter.ConvertFile Tool.Executable="',Tool.Executable,'" Tool.CmdLineParams="',Tool.CmdLineParams,'"']);
      Tool.Scanners.Add(SubToolH2Pas);
      if not RunExternalTool(Tool) then
        exit(mrAbort);
      if IDEMessagesWindow.SelectFirstUrgentMessage(mluError,false) then
        exit(mrAbort);
    finally
      Tool.Free;
    end;

    // run beautification tools for new pascal code
    TextConverter.InitWithFilename(OutputFilename);
    //DebugLn(['TH2PasConverter.ConvertFile Output: ',copy(TextConverter.Source,1,300)]);
    //DebugLn(['TH2PasConverter.ConvertFile CCC2 ',TextConverter.Source]);
    Result:=ExecuteTools(Project.PostH2PasTools,OutputFilename);
    if Result<>mrOk then begin
      DebugLn(['TH2PasConverter.ConvertFile Failed running Project.PostH2PasTools on ',OutputFilename]);
      exit;
    end;
    TextConverter.Filename:=OutputFilename;// save
    
    // clean up
    if FileExistsUTF8(TempCHeaderFilename) then
      DeleteFileUTF8(TempCHeaderFilename);
  finally
    TextConverter.Free;
    if (LazarusIDE<>nil) then begin
      // reload changed files, so that IDE does not report changed files
      CloseOrRevertEditorFile(TempCHeaderFilename);
      CloseOrRevertEditorFile(OutputFilename);
    end;
  end;

  Result:=mrOk;
end;

function TH2PasConverter.CheckMergeDependencies: TModalResult;
var
  CheckedFiles: TFPList;

  procedure AddIncludedByFiles(IncludedByFiles: TFPList; CurFile: TH2PasFile);
  var
    i: Integer;
    IncludedBy: TH2PasFile;
  begin
    if CheckedFiles.IndexOf(CurFile)>=0 then exit;
    CheckedFiles.Add(CurFile);
    for i:=0 to CurFile.CIncludedByCount-1 do begin
      IncludedBy:=CurFile.CIncludedBy[i].Owner;
      if IncludedBy.Merge then
        AddIncludedByFiles(IncludedByFiles,IncludedBy)
      else
        if IncludedByFiles.IndexOf(IncludedBy)<0 then
          IncludedByFiles.Add(IncludedBy);
    end;
  end;

var
  i: Integer;
  CurFile: TH2PasFile;
  j: Integer;
  IncludedByFiles: TFPList;
  Warning: String;
begin
  // update graph
  Result:=Project.ReadAllCIncludes(true);
  if Result=mrAbort then begin
    DebugLn(['TH2PasConverter.CheckMergeDependencies aborted reading all include dependencies']);
    exit;
  end;

  Warning:='';
  for i:=0 to Project.CHeaderFileCount-1 do begin
    CurFile:=Project.CHeaderFiles[i];
    if CurFile.Merge then begin
      // this file should be merged
      // -> check if it is included only once
      IncludedByFiles:=TFPList.Create;
      CheckedFiles:=TFPList.Create;
      AddIncludedByFiles(IncludedByFiles,CurFile);
      if IncludedByFiles.Count>1 then begin
        // this merged file is included by more than one unit
        Warning := Format(h2pWarningTheFileWillBeMergedIntoMultipleFiles, [Warning, Project.ShortenFilename(CurFile.Filename), #13, #13]);
        for j:=0 to IncludedByFiles.Count-1 do begin
          if j>0 then
            Warning:=Warning+', ';
          Warning:=Warning
              +Project.ShortenFilename(TH2PasFile(IncludedByFiles[j]).Filename);
        end;
        Warning:=Warning+#13;
      end;
      CheckedFiles.Free;
      IncludedByFiles.Free;
    end;
  end;
  
  if Warning<>'' then begin
    Result := MessageDlg(h2pWarning,
      Format(h2pAmbiguousMerges, [#13, Warning]), mtWarning, [mbIgnore, mbAbort], 0);
    if Result<>mrIgnore then exit(mrCancel);
  end;

  Result:=mrOk;
end;

function TH2PasConverter.MergeIncludeFiles(AFile: TH2PasFile;
  TextConverter: TIDETextConverter): TModalResult;
  
  procedure GetIncludeMergeFiles(MergedFiles: TFPList; CurFile: TH2PasFile);
  var
    i: Integer;
    CInclude: TH2PasFileCInclude;
    IncFile: TH2PasFile;
  begin
    //DebugLn(['GetMergeFiles CurFile=',CurFile.Filename,' CurFile.CIncludeCount=',CurFile.CIncludeCount]);
    // merge include files
    for i:=0 to CurFile.CIncludeCount-1 do begin
      CInclude:=CurFile.CIncludes[i];
      IncFile:=CInclude.H2PasFile;
      if IncFile=nil then continue;
      //DebugLn(['GetMergeFiles AFile=',AFile.Filename,' CInclude=',CInclude.Filename,' IncFile.Merge=',IncFile.Merge,' ']);
      if not IncFile.Merge then continue;
      if not IncFile.Enabled then continue;
      if IncFile=AFile then continue;
      if MergedFiles.IndexOf(IncFile)<0 then begin
        MergedFiles.Add(IncFile);
        GetIncludeMergeFiles(MergedFiles,IncFile);
      end;
    end;
  end;
  
  procedure GetProjectMergeFiles(MergedFiles: TFPList; CurFile: TH2PasFile);
  var
    IncFile: TH2PasFile;
    i: Integer;
  begin
    // merge non include files
    if Project<>nil then begin
      for i:=0 to Project.CHeaderFileCount-1 do begin
        IncFile:=Project.CHeaderFiles[i];
        if not IncFile.Enabled then continue;
        if IncFile=CurFile then continue;
        if IncFile.MergedBy=CurFile then begin
          if MergedFiles.IndexOf(IncFile)<0 then begin
            MergedFiles.Add(IncFile);
            GetIncludeMergeFiles(MergedFiles,IncFile);
          end;
        end;
      end;
    end;
  end;
  
var
  MergedFiles: TFPList;// list of TH2PasFile
  i: Integer;
  IncludeFile: TH2PasFile;
  fs: TFileStream;
  s: string;
begin
  Result:=mrCancel;
  MergedFiles:=TFPList.Create;
  try
    GetIncludeMergeFiles(MergedFiles,AFile);
    GetProjectMergeFiles(MergedFiles,AFile);
    for i:=0 to MergedFiles.Count-1 do begin
      IncludeFile:=TH2PasFile(MergedFiles[i]);
      DebugLn(['TH2PasConverter.MergeIncludeFiles merging file '
         ,'"'+IncludeFile.Filename+'"'+' into "'+TextConverter.Filename+'"']);
      try
        fs:=TFileStreamUTF8.Create(IncludeFile.Filename,fmOpenRead);
        try
          SetLength(s,fs.Size);
          if s<>'' then begin
            fs.Read(s[1],length(s));
            TextConverter.Source:=TextConverter.Source+LineEnding+s;
          end;
        finally
          fs.Free;
        end;
      except
        on E: Exception do begin
          MessageDlg(h2pError, Format(h2pUnableToMergeFileInto, [IncludeFile.Filename, TextConverter.Filename]), mtError, [mbCancel], 0);
          exit;
        end;
      end;
    end;
    Result:=mrOk;
  finally
    MergedFiles.Free;
  end;
end;

function TH2PasConverter.GetH2PasFilename: string;
begin
  Result:=FindDefaultExecutablePath(h2pasFilename);
end;

function TH2PasConverter.FileIsRelated(const aFilename: string): Boolean;
begin
  Result:=(CompareFilenames(AFilename,LastUsedFilename)=0)
      or ((Project<>nil) and (Project.CHeaderFileWithFilename(aFilename)<>nil));
end;

{ TRemoveCPlusPlusExternCTool }

class function TRemoveCPlusPlusExternCTool.ClassDescription: string;
begin
  Result := h2pRemoveCExternCLines;
end;

function TRemoveCPlusPlusExternCTool.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  i: Integer;
  Lines: TStrings;
  Line: string;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  i:=0;
  while i<=Lines.Count-1 do begin
    Line:=Trim(Lines[i]);
    if Line='extern "C" {' then begin
      Lines[i]:='';
    end
    else if (i>0) and (Line='}')
    and ((Lines[i-1]='#if defined(__cplusplus)')
      or (Lines[i-1]='#ifdef __cplusplus'))
    then begin
      Lines[i]:='';
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

{ TRemoveEmptyCMacrosTool }

class function TRemoveEmptyCMacrosTool.ClassDescription: string;
begin
  Result := h2pRemoveEmptyCMacros;
end;

function TRemoveEmptyCMacrosTool.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  EmptyMacros: TAVLTree;// tree of PChar

  procedure AddEmptyMacro(const MacroName: string);
  var
    TempStr: String;
    Identifier: PChar;
  begin
    //DebugLn(['AddEmptyMacro MacroName="',MacroName,'"']);
    if EmptyMacros=nil then
      EmptyMacros:=TAVLTree.Create(TListSortCompare(@CompareIdentifiers));
    Identifier:=@MacroName[1];
    if EmptyMacros.Find(Identifier)<>nil then exit;
    TempStr:=MacroName; // increase refcount
    if TempStr<>'' then
      Pointer(TempStr):=nil;
    EmptyMacros.Add(Identifier);
  end;
  
  procedure DeleteEmptyMacro(const MacroName: string);
  var
    OldMacroName: String;
    Identifier: PChar;
    Node: TAVLTreeNode;
  begin
    //DebugLn(['DeleteEmptyMacro MacroName="',MacroName,'"']);
    if EmptyMacros=nil then exit;
    Identifier:=@MacroName[1];
    Node:=EmptyMacros.Find(Identifier);
    if Node=nil then exit;
    OldMacroName:='';
    Pointer(OldMacroName):=Node.Data;
    if OldMacroName<>'' then OldMacroName:=''; // decrease refcount
    EmptyMacros.Delete(Node);
  end;

  procedure FreeMacros;
  var
    CurMacroName: String;
    Node: TAVLTreeNode;
  begin
    if EmptyMacros=nil then exit;
    CurMacroName:='';
    Node:=EmptyMacros.FindLowest;
    while Node<>nil do begin
      Pointer(CurMacroName):=Node.Data;
      if CurMacroName<>'' then CurMacroName:=''; // decrease refcount
      Node:=EmptyMacros.FindSuccessor(Node);
    end;
    EmptyMacros.Free;
  end;
  
  procedure RemoveEmptyMacrosFromString(var s: string);
  var
    IdentEnd: Integer;
    IdentStart: LongInt;
    Identifier: PChar;
    IdentLen: LongInt;
  begin
    if EmptyMacros=nil then exit;
    IdentEnd:=1;
    repeat
      IdentStart:=FindNextIdentifier(s,IdentEnd,length(s));
      if IdentStart>length(s) then exit;
      Identifier:=@s[IdentStart];
      IdentLen:=GetIdentLen(Identifier);
      if EmptyMacros.Find(Identifier)<>nil then begin
        // empty macro found -> remove
        System.Delete(s,IdentStart,IdentLen);
        IdentEnd:=IdentStart;
      end else begin
        IdentEnd:=IdentStart+IdentLen;
      end;
    until false;
  end;
  
var
  MacroStart, MacroLen: integer;
  Lines: TStrings;
  i: Integer;
  Line: string;
  MacroName: String;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  EmptyMacros:=nil;
  try
    i:=0;
    while i<=Lines.Count-1 do begin
      Line:=Lines[i];
      if REMatches(Line,'^#define\s+([a-zA-Z0-9_]+)\b(.*)$') then begin
        REVarPos(1,MacroStart,MacroLen);
        MacroName:=copy(Line,MacroStart,MacroLen);
        if Trim(copy(Line,MacroStart+MacroLen,length(Line)))='' then
          AddEmptyMacro(MacroName)
        else
          DeleteEmptyMacro(MacroName);
      end;
      if (Line<>'') and (Line[1]<>'#') then
        RemoveEmptyMacrosFromString(Line);
      Lines[i]:=Line;
      inc(i);
    end;
  finally
    FreeMacros;
  end;
  Result:=mrOk;
end;

{ TReplaceMacro0PointerWithNULL }

class function TReplaceMacro0PointerWithNULL.ClassDescription: string;
begin
  Result := h2pReplaceMacroValues0PointerLikeChar0WithNULL;
end;

function TReplaceMacro0PointerWithNULL.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Lines: TStrings;
  i: Integer;
  Line: string;
  MacroStart, MacroLen: integer;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  i:=0;
  while i<=Lines.Count-1 do begin
    Line:=Lines[i];
    // example: #define MPI_ARGV_NULL (char **)0
    if REMatches(Line,'^#define\s+([a-zA-Z0-9_]+)\s+(\(.*\*\)0)\s*($|//|/\*)')
    then begin
      REVarPos(2,MacroStart,MacroLen);
      Line:=copy(Line,1,MacroStart-1)+'NULL'
        +copy(Line,MacroStart+MacroLen,length(Line));
      Lines[i]:=Line;
    end
    else // example: #define MPI_NULL_COPY_FN   ((MPI_Copy_function *)0)
    if REMatches(Line,'^#define\s+([a-zA-Z0-9_]+)\s+(\(\(.*\*\)0\))\s*($|//|/\*)')
    then begin
      REVarPos(2,MacroStart,MacroLen);
      Line:=copy(Line,1,MacroStart-1)+'NULL'
        +copy(Line,MacroStart+MacroLen,length(Line));
      Lines[i]:=Line;
    end
    else // example: *)0)
    if REMatches(Line,'\*\)(0)\)')
    then begin
      REVarPos(1,MacroStart,MacroLen);
      Line:=copy(Line,1,MacroStart-1)+'NULL'
        +copy(Line,MacroStart+MacroLen,length(Line));
      Lines[i]:=Line;
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

{ TReplaceEdgedBracketPairWithStar }

class function TReplaceEdgedBracketPairWithStar.ClassDescription: string;
begin
  Result := h2pReplaceWith;
end;

constructor TReplaceEdgedBracketPairWithStar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SearchFor:='[]';
  ReplaceWith:='*';
end;

{ TReplaceUnitFilenameWithUnitName }

class function TReplaceUnitFilenameWithUnitName.ClassDescription: string;
begin
  Result := h2pReplaceUnitFilenameWithUnitName;
end;

constructor TReplaceUnitFilenameWithUnitName.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SearchFor:='^(unit\s).*(/|\\)([a-z_0-9]+;)';
  ReplaceWith:='$1$3';
  Options:=Options+[trtRegExpr];
end;

{ TRemoveSystemTypes }

class function TRemoveSystemTypes.ClassDescription: string;
begin
  Result := h2pRemoveTypeRedefinitionsLikePLongint;
end;

function TRemoveSystemTypes.Execute(aText: TIDETextConverter): TModalResult;
var
  Source: String;
  Flags: TSrcEditSearchOptions;
  Prompt: Boolean;
  SearchFor: string;
  i: Integer;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Source:=aText.Source;
  
  Flags:=[sesoReplace,sesoReplaceAll,sesoRegExpr];
  Prompt:=false;
  SearchFor:='';
  for i:=Low(PreDefinedH2PasTypes) to High(PreDefinedH2PasTypes) do begin
    if SearchFor<>'' then
      SearchFor:=SearchFor+'|';
    SearchFor:=SearchFor
               +'P'+PreDefinedH2PasTypes[i]+'\s*=\s*\^'+PreDefinedH2PasTypes[i];
  end;
  SearchFor:='^\s*('+SearchFor+');\s*$';
  Result:=IDESearchInText('',Source,SearchFor,'',Flags,Prompt,nil);
  if Result<>mrOk then begin
    ErrorMsg := Format(h2pDeletionOfFailed, [SearchFor]);
    exit;
  end;
  
  // replace NULL with nil
  Flags:=[sesoReplace,sesoReplaceAll,sesoRegExpr,sesoMatchCase];
  Result:=IDESearchInText('',Source,'\bNULL\b','nil',Flags,Prompt,nil);
  if Result<>mrOk then begin
    ErrorMsg := h2pReplacingOfNULLWithNilFailed;
    exit;
  end;

  aText.Source:=Source;
end;

{ TRemoveRedefinedPointerTypes }

class function TRemoveRedefinedPointerTypes.ClassDescription: string;
begin
  Result := h2pRemoveRedefinedPointerTypes;
end;

function TRemoveRedefinedPointerTypes.Execute(aText: TIDETextConverter
  ): TModalResult;
{ search for
    Pname  = ^name;
  if PName has a redefinition, delete the second one
}
var
  Lines: TStrings;
  i: Integer;
  Line: string;
  PointerName: String;
  TypeName: String;
  j: Integer;
  Pattern: String;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  i:=0;
  while i<=Lines.Count-1 do begin
    Line:=Lines[i];
    if REMatches(Line,'^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*=\s*\^\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*;\s*($|//|/\*)') then begin
      PointerName:=REVar(1);
      TypeName:=REVar(2);
      Pattern:='^\s*'+PointerName+'\s*=\s*\^\s*'+TypeName+'\s*;';
      j:=Lines.Count-1;
      while (j>i) do begin
        if REMatches(Lines[j],Pattern) then
          Lines.Delete(j);
        dec(j);
      end;
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

{ TRemoveEmptyTypeVarConstSections }

class function TRemoveEmptyTypeVarConstSections.ClassDescription: string;
begin
  Result := h2pRemoveEmptyTypeVarConstSections;
end;

function TRemoveEmptyTypeVarConstSections.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Src: String;
  p: Integer;
  AtomStart: Integer;
  CurAtom, NextAtom: PChar;
  KeyWordStart: LongInt;
  KeyWordEnd: LongInt;
  DeleteSection: Boolean;
  Modified: Boolean;
begin
  Result:=mrCancel;
  Src:=aText.Source;
  p:=1;
  AtomStart:=p;
  repeat
    ReadRawNextPascalAtom(Src,p,AtomStart);
    if p>length(Src) then break;
    CurAtom:=@Src[AtomStart];
    if (CompareIdentifiers(CurAtom,'type')=0)
    or (CompareIdentifiers(CurAtom,'var')=0)
    or (CompareIdentifiers(CurAtom,'const')=0)
    or (CompareIdentifiers(CurAtom,'threadvar')=0)
    or (CompareIdentifiers(CurAtom,'resourcestring')=0)
    then begin
      // start of a section found
      // read next atoms to check if they are identifier plus definition operator
      //   'name =' or 'name:' or 'name,'
      KeyWordStart:=AtomStart;
      KeyWordEnd:=p;
      ReadRawNextPascalAtom(Src,p,AtomStart);
      if p<length(Src) then begin
        NextAtom:=@Src[AtomStart];
        DeleteSection:=true;
        if GetIdentLen(NextAtom)>0 then begin
          ReadRawNextPascalAtom(Src,p,AtomStart);
          if (p<=length(Src)) and (p-AtomStart=1)
          and (Src[AtomStart] in ['=',':',',']) then
            DeleteSection:=false;
        end;
        if DeleteSection then begin
          // this section is empty -> delete it
          Src:=copy(Src,1,KeyWordStart-1)+copy(Src,KeyWordEnd,length(Src));
          Modified:=true;
          // adjust position
          p:=KeyWordStart;
        end;
      end;
    end;
  until false;
  if Modified then
    aText.Source:=Src;

  Result:=mrOk;
end;

type
  TImplicitType = class
  public
    Name: string;
    Code: string;
    MinPosition: integer;
    MaxPosition: integer;
    MinPositionNeedsTypeSection: boolean;
  end;

function CompareImplicitTypeNames(Type1, Type2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(TImplicitType(Type1).Name),
                             PChar(TImplicitType(Type2).Name));
end;

function CompareImplicitTypeStringAndName(Identifier,
  ImplicitType: Pointer): integer;
begin
  Result:=CompareIdentifiers(PChar(Identifier),
                             PChar(TImplicitType(ImplicitType).Name));
end;

function CompareImplicitTypeMinPositions(Type1, Type2: Pointer): integer;
begin
  Result:=TImplicitType(Type1).MinPosition-TImplicitType(Type2).MinPosition;
end;

{ TReplaceImplicitParameterTypes }

class function TReplaceImplicitTypes.ClassDescription: string;
begin
  Result := Format(h2pReplaceImplicitTypesForExampleProcedureProcNameAAr, [#13, #13, #13, #13, #13, #13]);
end;

function TReplaceImplicitTypes.FindNextImplicitType(var Position: integer;
  out aTypeStart, aTypeEnd: integer): boolean;
var
  AtomStart: LongInt;

  function ReadTilTypeEnd: boolean;
  var
    CurAtom: String;
  begin
    repeat
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if CurAtom='' then exit(false);
      if (length(CurAtom)=1) and (CurAtom[1] in ['(','[']) then begin
        // skip brackets
        if not ReadTilPascalBracketClose(Src,Position) then exit(false);
      end else if (length(CurAtom)=1) and (CurAtom[1] in [';',')',']'])
      then begin
        // type end found
        aTypeEnd:=AtomStart;
        Result:=true;
        exit;
      end;
    until false;
  end;
  
var
  CurAtom: string;
begin
  Result:=false;
  aTypeStart:=0;
  aTypeEnd:=0;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    if CurAtom='' then break;
    //DebugLn(['TReplaceImplicitTypes.FindNextImplicitType atom ',CurAtom]);
    if CurAtom=':' then begin
      // var, const, out declaration
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if CurAtom='' then break;
      aTypeStart:=AtomStart;
      if CompareIdentifiers(PChar(CurAtom),'array')=0 then begin
        // :array
        CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
        if CurAtom='' then break;
        if CurAtom='[' then begin
          // :array[
          if not ReadTilPascalBracketClose(Src,Position) then break;
          // :array[..]
          Result:=ReadTilTypeEnd;
          exit;
        end;
      end
      else if CompareIdentifiers(PChar(CurAtom),'function')=0 then begin
        // :function
        // for example: function hci_for_each_dev(func:function (dd:longint):longint):longint;
        Result:=ReadTilTypeEnd;
        exit;
      end
      else if CompareIdentifiers(PChar(CurAtom),'procedure')=0 then begin
        // :procedure
        // for example: procedure hci_for_each_dev(func:function (dd:longint):longint);
        Result:=ReadTilTypeEnd;
        exit;
      end;
    end;
  until CurAtom='';
end;

function TReplaceImplicitTypes.SearchImplicitParameterTypes(
  var ModalResult: TModalResult): boolean;
var
  Position: Integer;
  StartPos, EndPos: integer;
  TypeCode: String;
  TypeName: String;
  NewType: TImplicitType;
begin
  Result:=false;
  ModalResult:=mrCancel;
  Position:=1;
  while FindNextImplicitType(Position,StartPos,EndPos) do begin
    TypeCode:=copy(Src,StartPos,EndPos-StartPos);
    //DebugLn(['SearchImplicitParameterTypes ',StartPos,' TypeCode="',TypeCode,'"']);
    TypeName:=CodeToIdentifier(TypeCode);
    if TypeName='' then continue;
    if (ImplicitTypes<>nil)
    and (ImplicitTypes.FindKey(Pointer(TypeName),
                       @CompareImplicitTypeStringAndName)<>nil)
    then begin
      // type exists already
      continue;
    end;
    // add new type
    //DebugLn(['SearchImplicitParameterTypes Adding new type ',StartPos,' TypeName=',TypeName,' TypeCode="',TypeCode,'"']);
    NewType:=TImplicitType.Create;
    NewType.Name:=TypeName;
    NewType.Code:=TypeCode;
    NewType.MaxPosition:=StartPos;
    if ImplicitTypes=nil then
      ImplicitTypes:=TAVLTree.Create(@CompareImplicitTypeNames);
    ImplicitTypes.Add(NewType);
  end;
  ModalResult:=mrOk;
  Result:=true;
end;

function TReplaceImplicitTypes.PosToStr(Position: integer): string;
var
  Line, Col: integer;
begin
  SrcPosToLineCol(Src,Position,Line,Col);
  Result:='(y='+IntToStr(Line)+',x='+IntToStr(Col)+')';
end;

procedure TReplaceImplicitTypes.AdjustMinPositions(const Identifier: string);
var
  Node: TAVLTreeNode;
  Item: TImplicitType;
  Position: Integer;
  AtomStart: LongInt;
  CurAtom: String;
  MinPos: LongInt;
begin
  if TypeEnd>0 then
    MinPos:=TypeEnd
  else if ConstSectionEnd>0 then
    MinPos:=ConstSectionEnd
  else
    exit;
  //DebugLn(['AdjustMinPositions Identifier=',Identifier]);

  // search Identifier in all implicit type definitions
  Node:=ImplicitTypes.FindLowest;
  while Node<>nil do begin
    Item:=TImplicitType(Node.Data);
    if Item.MaxPosition>=TypeEnd then begin
      // search Identifier in Item.Code
      Position:=1;
      AtomStart:=Position;
      repeat
        CurAtom:=ReadNextPascalAtom(Item.Code,Position,AtomStart);
        if CurAtom='' then break;
        //DebugLn(['AdjustMinPositions ',Item.Name,' ',CurAtom]);
        if CompareIdentifiers(PChar(Identifier),PChar(CurAtom))=0 then begin
          // this implicit type depends on an explicit type defined
          // prior in this source file
          {DebugLn(['AdjustMinPositions "',Item.Name,'=',Item.Code,'"',
            ' depends on ',Identifier,
            ' defined at ',PosToStr(MinPos),
            ' as "',copy(Src,MinPos,30),'"']);}
          if Item.MinPosition<MinPos then begin
            Item.MinPosition:=MinPos;
            Item.MinPositionNeedsTypeSection:=TypeEnd<1;
          end;
          break;
        end;
      until false;
    end;
    Node:=ImplicitTypes.FindSuccessor(Node);
  end;
end;

function TReplaceImplicitTypes.ReadWord(var Position: integer): boolean;
var
  AtomStart: LongInt;
  CurAtom: String;
begin
  AtomStart:=Position;
  CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
  if (CurAtom<>'') and IsIdentStartChar[CurAtom[1]] then
    Result:=true
  else begin
    DebugLn(['ReadWord word not found at ',PosToStr(AtomStart)]);
    Result:=false;
  end;
end;

function TReplaceImplicitTypes.ReadUntilAtom(var Position: integer;
  const StopAtom: string; SkipBrackets: boolean = true): boolean;
var
  AtomStart: LongInt;
  CurAtom: String;
  StartPos: LongInt;
begin
  Result:=false;
  StartPos:=Position;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    if CurAtom='' then begin
      DebugLn(['ReadUntilAtom atom not found: "',StopAtom,'" (starting at ',PosToStr(StartPos),')']);
      exit;
    end;
    if SkipBrackets then begin
      if CurAtom='(' then begin
        // skip round bracket open
        if not ReadUntilAtom(Position,')') then exit;
      end else if CurAtom='[' then begin
        // skip edged bracket open
        if not ReadUntilAtom(Position,']') then exit;
      end;
    end;
  until CurAtom=StopAtom;
  Result:=true;
end;

function TReplaceImplicitTypes.ReadRecord(var Position: integer): boolean;
var
  AtomStart: LongInt;
  CurAtom: String;
begin
  Result:=false;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    if CurAtom='' then begin
      DebugLn(['ReadRecord record end not found']);
      exit;
    end else if CurAtom='(' then begin
      // skip round bracket open
      if not ReadUntilAtom(Position,')') then exit;
    end else if CurAtom='[' then begin
      // skip edged bracket open
      if not ReadUntilAtom(Position,']') then exit;
    end else if CompareIdentifiers(PChar(CurAtom),'CASE')=0 then begin
      // read identifier
      if not ReadWord(Position) then exit;
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      //DebugLn(['ReadRecord CASE colon or "of" CurAtom="',CurAtom,'"']);
      if CurAtom=':' then begin
        // read case type
        if not ReadWord(Position) then begin
          DebugLn(['ReadRecord missing case type at ',PosToStr(Position)]);
          exit;
        end;
        // read 'of'
        CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
        if CurAtom='' then begin
          DebugLn(['ReadRecord missing "of" at ',PosToStr(Position)]);
          exit;
        end;
      end;
      if CompareIdentifiers(PChar(CurAtom),'OF')<>0 then begin
        DebugLn(['ReadRecord record case "of" not found at ',PosToStr(AtomStart)]);
        exit;
      end;
    end else if CurAtom=':' then begin
      // skip type
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if CurAtom='(' then begin
        // skip case brackets
        if not ReadUntilAtom(Position,')') then exit;
      end else begin
        // read normal type
        Position:=AtomStart;
        if not ReadTypeDefinition(Position) then exit;
      end;
    end;
  until CompareIdentifiers(PChar(CurAtom),'END')=0;
  Result:=true;
end;

function TReplaceImplicitTypes.ReadClass(var Position: integer): boolean;
var
  AtomStart: LongInt;
  CurAtom: String;
begin
  //DebugLn(['ReadClass at ',PosToStr(Position)]);
  Result:=false;
  AtomStart:=Position;
  CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
  //DebugLn(['ReadClass first atom "',CurAtom,'"']);
  if CurAtom=';' then begin
    // this is a forward class definition
    //DebugLn(['ReadClass forward defined class found']);
    Result:=true;
    exit;
  end;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    //DebugLn(['ReadClass CurAtom="',CurAtom,'"']);
    if CurAtom='' then begin
      DebugLn(['ReadClass class end not found']);
      exit;
    end else if CurAtom='(' then begin
      // skip round bracket open
      if not ReadUntilAtom(Position,')') then exit;
    end else if CurAtom='[' then begin
      // skip edged bracket open
      if not ReadUntilAtom(Position,']') then exit;
    end else if CurAtom=':' then begin
      // skip type
      if not ReadTypeDefinition(Position) then exit;
    end;
  until CompareIdentifiers(PChar(CurAtom),'END')=0;
  Result:=true;
end;

function TReplaceImplicitTypes.ReadTypeDefinition(
  var Position: integer): boolean;
// Position must be after the colon
var
  AtomStart: LongInt;
  CurAtom: String;
  Enum: String;
begin
  //DebugLn(['ReadTypeDefinition reading type definition at ',PosToStr(Position)]);
  Result:=false;
  AtomStart:=Position;
  CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
  if CurAtom='(' then begin
    // enumeration constants
    //DebugLn(['ReadTypeDefinition enumeration found at ',PosToStr(AtomStart)]);
    repeat
      Enum:=ReadNextPascalAtom(Src,Position,AtomStart);
      if (Enum='') then exit;// missing bracket close
      if Enum=')' then exit(true);// type end found
      if (not IsIdentStartChar[Enum[1]]) then exit;// enum missing
      //DebugLn(['ReadTypeDefinition enum ',Enum,' found at ',PosToStr(AtomStart)]);
      AdjustMinPositions(Enum);
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if CurAtom=')' then exit(true);// type end found
      if CurAtom<>',' then exit;// comma missing
    until false;
  end;
  repeat
    //DebugLn(['ReadTypeDefinition CurAtom="',CurAtom,'"']);
    if CurAtom='' then begin
      DebugLn(['ReadTypeDefinition type end not found']);
      exit;
    end;
    if IsIdentStartChar[CurAtom[1]] then begin
      if CompareIdentifiers(PChar(CurAtom),'RECORD')=0 then begin
        // skip record
        Result:=ReadRecord(Position);
        exit;
      end;
      if (CompareIdentifiers(PChar(CurAtom),'CLASS')=0)
      or (CompareIdentifiers(PChar(CurAtom),'OBJECT')=0)
      or (CompareIdentifiers(PChar(CurAtom),'INTERFACE')=0)
      or (CompareIdentifiers(PChar(CurAtom),'DISPINTERFACE')=0)
      then begin
        // skip record
        Result:=ReadClass(Position);
        exit;
      end;
    end else if CurAtom='(' then begin
      // skip round bracket open
      if not ReadUntilAtom(Position,')') then exit;
    end else if CurAtom='[' then begin
      // skip edged bracket open
      if not ReadUntilAtom(Position,']') then exit;
    end else if (length(CurAtom)=1) and (CurAtom[1] in [';',')',']']) then
      break;
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
  until false;
  Result:=true;
end;

function TReplaceImplicitTypes.ReadConstSection(var Position: integer): boolean;
// Position must be after the 'const' keyword
var
  AtomStart: LongInt;
  CurAtom: String;
  ConstStart: LongInt;
begin
  Result:=false;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    if CurAtom='' then begin
      DebugLn(['ReadConstSection end not found']);
      exit;
    end;
    if IsIdentStartChar[CurAtom[1]] then begin
      // const identifier(s) or end of const section
      //DebugLn(['ReadConstSection Const name ',CurAtom,' at ',PosToStr(AtomStart)]);
      ConstStart:=AtomStart;
      // for example: a,b,c: integer = 1; d=1, e:integer=0;
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if (length(CurAtom)<>1) or (not (CurAtom[1] in [',','=',':'])) then
      begin
        // end of const section
        Position:=ConstStart;
        Result:=true;
        exit;
      end;
      Position:=ConstStart;
      repeat
        CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
        // read identifier
        if (CurAtom<>'') and IsIdentStartChar[CurAtom[1]] then begin
          // identifier
          AdjustMinPositions(CurAtom);
        end else begin
          DebugLn(['ReadConstSection end of section missing']);
          exit;
        end;
        CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
        if (CurAtom='=') or (CurAtom=':') then begin
          // skip type and expression
          if not ReadUntilAtom(Position,';') then exit;
          break;
        end else if CurAtom=',' then begin
          // next const name
        end else begin
          DebugLn(['ReadConstSection end of section missing']);
          exit;
        end;
      until false;
    end else begin
      // end of const section
      break;
    end;
  until false;
  Result:=true;
end;

function TReplaceImplicitTypes.FindExplicitTypesAndConstants(
  var ModalResult: TModalResult): boolean;
{ every implicit type can contain references to explicit types and constants
  For example: array[0..3] of bogus
  If 'bogus' is defined in this source, then the new type must be defined
  after 'bogus'.
  => Search all explicit types
}
var
  Position: Integer;
  AtomStart: LongInt;
  CurAtom: String;
  Identifier: String;
  TypeDefStart: LongInt;
  ErrLine: integer;
  ErrCol: integer;
begin
  Result:=false;
  ModalResult:=mrCancel;

  Position:=1;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
    //DebugLn(['FindExplicitTypes CurAtom="',CurAtom,'"']);
    if CurAtom='' then break;
    if CompareIdentifiers(PChar(CurAtom),'type')=0 then begin
      // type section found
      //DebugLn(['FindExplicitTypes type section found at ',PosToStr(AtomStart)]);
      repeat
        Identifier:=ReadNextPascalAtom(Src,Position,AtomStart);
        if (Identifier<>'') and (IsIdentStartChar[Identifier[1]]) then begin
          // word found (can be an identifier or start of next section)
          TypeStart:=AtomStart;
          TypeEnd:=0;
          CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
          if CurAtom<>'=' then begin
            //DebugLn(['FindExplicitTypes type section ended at ',PosToStr(AtomStart)]);
            break;
          end;
          // Identifier is a type => find end of type definition
          //DebugLn(['FindExplicitTypes type definition found: ',Identifier,' at ',PosToStr(TypeStart)]);
          TypeDefStart:=Position;
          Result:=ReadTypeDefinition(Position);
          if not Result then begin
            SrcPosToLineCol(Src,TypeStart,ErrLine,ErrCol);
            ErrorColumn:=ErrCol;
            ErrorLine:=ErrLine;
            ErrorMsg := Format(h2pFindExplicitTypesFAILEDReadingTypeDefinition, [Identifier]);
            DebugLn(['FindExplicitTypes FAILED reading type definition ',Identifier,' at ',PosToStr(TypeStart)]);
            exit;
          end;
          TypeEnd:=Position;
          // add the semicolon, if not already done
          CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
          if CurAtom=';' then
            TypeEnd:=Position;
          // adjust implicit identifiers
          AdjustMinPositions(Identifier);
          // reread the type for the enums
          Position:=TypeDefStart;
          //DebugLn(['FindExplicitTypes Rereading type definition ',Identifier,' at ',PosToStr(TypeStart)]);
          Result:=ReadTypeDefinition(Position);
          if not Result then begin
            SrcPosToLineCol(Src,TypeStart,ErrLine,ErrCol);
            ErrorColumn:=ErrCol;
            ErrorLine:=ErrLine;
            ErrorMsg := Format(h2pFindExplicitTypesFAILEDRereadingTypeDefinition, [Identifier]);
            DebugLn(['FindExplicitTypes FAILED Rereading type definition ',Identifier,' at ',PosToStr(TypeStart)]);
            exit;
          end;
          // skip semicolon
          Position:=TypeEnd;
          TypeEnd:=0;
        end;
      until false;
    end
    else if CompareIdentifiers(PChar(CurAtom),'const')=0 then begin
      ConstSectionStart:=Position;
      ConstSectionEnd:=0;
      // find end of const section
      //DebugLn(['TReplaceImplicitTypes.FindExplicitTypesAndConstants finding end of const section ...']);
      Result:=ReadConstSection(Position);
      if not Result then begin
        SrcPosToLineCol(Src,ConstSectionStart,ErrLine,ErrCol);
        ErrorColumn:=ErrCol;
        ErrorLine:=ErrLine;
        ErrorMsg := h2pFindExplicitTypesFAILEDReadingConstSection;
        DebugLn(['FindExplicitTypes FAILED reading const section at ',PosToStr(ConstSectionStart)]);
        exit;
      end;
      ConstSectionEnd:=Position;
      // reread the section for the identifiers
      Position:=ConstSectionStart;
      //DebugLn(['TReplaceImplicitTypes.FindExplicitTypesAndConstants collecting const identifiers ...']);
      Result:=ReadConstSection(Position);
      if not Result then begin
        SrcPosToLineCol(Src,ConstSectionStart,ErrLine,ErrCol);
        ErrorColumn:=ErrCol;
        ErrorLine:=ErrLine;
        ErrorMsg := h2pFindExplicitTypesFAILEDReadingConstSection;
        DebugLn(['FindExplicitTypes FAILED reading const section at ',PosToStr(ConstSectionStart)]);
        exit;
      end;
      ConstSectionEnd:=0;
    end;
  until false;

  ModalResult:=mrOk;
  Result:=true;
end;

function TReplaceImplicitTypes.InsertNewTypes(var ModalResult: TModalResult
  ): boolean;

  function CreateCode(Item: TImplicitType): string;
  begin
    Result:='  '+Item.Name+' = '+Item.Code+';';
  end;

var
  Node: TAVLTreeNode;
  Item: TImplicitType;
  InsertPos: integer;
  NextItem: TImplicitType;
  NextInsertPos: integer;
  NewCode: String;
begin
  Result:=false;
  ModalResult:=mrCancel;
  if (ImplicitTypes<>nil) then begin
    // re-sort the ImplicitTypes for MinPosition
    ImplicitTypes.OnCompare:=@CompareImplicitTypeMinPositions;
    try
      // Insert every type
      Node:=ImplicitTypes.FindHighest;
      while Node<>nil do begin
        Item:=TImplicitType(Node.Data);
        NewCode:=CreateCode(Item);
        if Item.MinPositionNeedsTypeSection or (Item.MinPosition=0) then
          NewCode:='type'+LineEnding+NewCode;
        InsertPos:=FindInsertPosition(Item.MinPosition);
        // add all items at the same position
        repeat
          Node:=ImplicitTypes.FindPrecessor(Node);
          if (Node=nil) then break;
          NextItem:=TImplicitType(Node.Data);
          NextInsertPos:=FindLineEndOrCodeAfterPosition(Src,NextItem.MinPosition,
                                                        length(Src)+1,false);
          if InsertPos>NextInsertPos then
            break;
          NewCode:=NewCode+LineEnding+CreateCode(NextItem);
        until false;

        // insert line ends
        if (InsertPos>1) and (InsertPos<length(Src))
        and (not (Src[InsertPos-1] in [#10,#13])) then
          NewCode:=LineEnding+NewCode;
        if (InsertPos<=length(Src)) and (not (Src[InsertPos] in [#10,#13])) then
          NewCode:=NewCode+LineEnding;

        // insert code
        DebugLn(['TReplaceImplicitTypes.InsertNewTypes Insert at ',PosToStr(InsertPos),' NewCode="',NewCode,'"']);
        Src:=copy(Src,1,InsertPos-1)+NewCode+copy(Src,InsertPos,length(Src));
      end;
    finally
      // re-sort the ImplicitTypes for Names
      ImplicitTypes.OnCompare:=@CompareImplicitTypeNames;
    end;
  end;
  ModalResult:=mrOk;
  Result:=true;
end;

function TReplaceImplicitTypes.FindInsertPosition(MinPos: integer): integer;
var
  Position: Integer;
  AtomStart: LongInt;
  CurAtom: String;
begin
  if MinPos>0 then begin
    Result:=FindLineEndOrCodeAfterPosition(Src,MinPos,length(Src)+1,false);
  end else begin
    // find insert position for a first type section
    Result:=1;
    Position:=1;
    AtomStart:=Position;
    repeat
      CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
      if CurAtom='' then break;
      if (CompareIdentifiers(PChar(CurAtom),'UNIT')=0)
      or (CompareIdentifiers(PChar(CurAtom),'PROGRAM')=0)
      or (CompareIdentifiers(PChar(CurAtom),'LIBRARY')=0)
      or (CompareIdentifiers(PChar(CurAtom),'PACKAGE')=0)
      or (CompareIdentifiers(PChar(CurAtom),'USES')=0)
      then begin
        ReadUntilAtom(Position,';');
        Result:=Position;
      end
      else if (CompareIdentifiers(PChar(CurAtom),'INTERFACE')=0)
      or (CompareIdentifiers(PChar(CurAtom),'IMPLEMENTATION')=0)
      then begin
        Result:=Position;
        // skip uses section
        CurAtom:=ReadNextPascalAtom(Src,Position,AtomStart);
        if (CurAtom<>'')
        and (CompareIdentifiers(PChar(CurAtom),'USES')=0) then begin
          ReadUntilAtom(Position,';');
          Result:=Position;
        end;
        break;
      end else
        break;
    until false;
  end;
end;

function TReplaceImplicitTypes.UseNewTypes(var ModalResult: TModalResult
  ): boolean;
var
  Position: Integer;
  StartPos: Integer;
  EndPos: Integer;
  TypeCode: String;
  TypeName: String;
  Node: TAVLTreeNode;
  Item: TImplicitType;
begin
  Result:=false;
  ModalResult:=mrCancel;
  if (ImplicitTypes<>nil) then begin
    Position:=1;
    StartPos:=1;
    EndPos:=1;
    while FindNextImplicitType(Position,StartPos,EndPos) do begin
      TypeCode:=copy(Src,StartPos,EndPos-StartPos);
      //DebugLn(['UseNewTypes ',StartPos,' TypeCode="',TypeCode,'"']);
      TypeName:=CodeToIdentifier(TypeCode);
      if TypeName='' then continue;
      Node:=ImplicitTypes.FindKey(Pointer(TypeName),
                         @CompareImplicitTypeStringAndName);
      if Node<>nil then begin
        // replace
        Item:=TImplicitType(Node.Data);
        Src:=copy(Src,1,StartPos-1)+Item.Name+copy(Src,EndPos,length(Src));
        Position:=StartPos+length(Item.Name);
      end;
    end;
  end;
  ModalResult:=mrOk;
  Result:=true;
end;

function TReplaceImplicitTypes.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Src:=aText.Source;
  if Src='' then exit(mrOk);
  
  ImplicitTypes:=nil;
  ExplicitTypes:=nil;
  TypeEnd:=0;
  ConstSectionEnd:=0;
  try
    if not SearchImplicitParameterTypes(Result) then exit;
    if (ImplicitTypes<>nil) then begin
      if not FindExplicitTypesAndConstants(Result) then exit;
      if not InsertNewTypes(Result) then exit;
      if not UseNewTypes(Result) then exit;
      aText.Source:=Src;
    end;
  finally
    if ImplicitTypes<>nil then begin
      ImplicitTypes.FreeAndClear;
      ImplicitTypes.Free;
    end;
    if ExplicitTypes<>nil then begin
      ExplicitTypes.FreeAndClear;
      ExplicitTypes.Free;
    end;
  end;
  Result:=mrOk;
end;

function TReplaceImplicitTypes.CodeToIdentifier(const Code: string): string;
// for example:
//   array[0..3] of integer  -> TArray0to3OfInteger
var
  Position: Integer;
  AtomStart: LongInt;
  CurAtom: String;
  i: Integer;
begin
  Result:='T';
  Position:=1;
  AtomStart:=Position;
  repeat
    CurAtom:=ReadNextPascalAtom(Code,Position,AtomStart);
    if CurAtom='' then exit;
    if CurAtom='..' then
      // range
      Result:=Result+'to'
    else if IsIdentStartChar[CurAtom[1]] then
      // word
      Result:=Result+upCase(CurAtom[1])+copy(CurAtom,2,length(CurAtom))
    else begin
      // otherwise: add word and number characters
      for i:=1 to length(CurAtom) do begin
        case CurAtom[i] of
        '0'..'9','_','a'..'z','A'..'Z': Result:=Result+CurAtom[i];
        '.': Result:=Result+'.';
        end;
      end;
    end;
    if length(Result)>200 then begin
      Result:=copy(Result,1,200);
      exit;
    end;
  until false;
end;

{ TFixArrayOfParameterType }

class function TFixArrayOfParameterType.ClassDescription: string;
begin
  Result := Format(h2pFixOpenArraysReplaceArrayOfWithArrayOfConst, [#13]);
end;

function TFixArrayOfParameterType.Execute(aText: TIDETextConverter
  ): TModalResult;
{ search for
    array of )
  and replace it with
    array of const)
}
var
  Lines: TStrings;
  i: Integer;
  Line: string;
  MatchPos: integer;
  MatchLen: integer;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  i:=0;
  while i<=Lines.Count-1 do begin
    Line:=Lines[i];
    if REMatches(Line,'array of *\)','I') then begin
      REVarPos(0,MatchPos,MatchLen);
      Lines[i]:=copy(Line,1,MatchPos-1)+'array of const)'
                +copy(Line,MatchPos+MatchLen,length(Line));
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

{ TH2PasFileCInclude }

procedure TH2PasFileCInclude.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TH2PasFileCInclude.SetH2PasFile(const AValue: TH2PasFile);
begin
  if FH2PasFile=AValue then exit;
  if (FH2PasFile<>nil) then
    FH2PasFile.InternalRemoveCIncludedBy(Self);
  FH2PasFile:=AValue;
  if (FH2PasFile<>nil) then
    FH2PasFile.InternalAddCIncludedBy(Self);
end;

procedure TH2PasFileCInclude.SetSrcFilename(const AValue: string);
begin
  if FSrcFilename=AValue then exit;
  FSrcFilename:=AValue;
  FFilename:='';
end;

procedure TH2PasFileCInclude.SetSrcPos(const AValue: TPoint);
begin
  FSrcPos:=AValue;
end;

constructor TH2PasFileCInclude.Create(TheOwner: TH2PasFile);
begin
  FOwner:=TheOwner;
end;

destructor TH2PasFileCInclude.Destroy;
begin
  H2PasFile:=nil;
  inherited Destroy;
end;

{ TRemoveRedefinitionsInUnit }

class function TRemoveRedefinitionsInUnit.ClassDescription: string;
begin
  Result := h2pRemoveRedefinitionsInPascalUnit;
end;

function TRemoveRedefinitionsInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Result:=mrCancel;
  //DebugLn(['TRemoveRedefinitionsInUnit.Execute START ',aText.Source]);
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TRemoveRedefinitionsInUnit.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.RemoveAllRedefinitions(TCodeBuffer(aText.CodeBuffer)) then begin
    AssignCodeToolBossError;
    DebugLn(['TRemoveRedefinitionsInUnit.Execute RemoveAllRedefinitions failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  //DebugLn(['TRemoveRedefinitionsInUnit.Execute END ',aText.Source]);
  Result:=mrOk;
end;

{ TFixAliasDefinitionsInUnit }

class function TFixAliasDefinitionsInUnit.ClassDescription: string;
begin
  Result := Format(h2pFixesSectionTypeOfAliasDefinitionsInPascalUnitChec, [#13, #13, #13]);
end;

function TFixAliasDefinitionsInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TFixAliasDefinitionsInUnit.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.FixAllAliasDefinitions(TCodeBuffer(aText.CodeBuffer)) then begin
    AssignCodeToolBossError;
    DebugLn(['TFixAliasDefinitionsInUnit.Execute FixAllAliasDefinitions failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  Result:=mrOk;
end;

{ TFixH2PasMissingIFDEFsInUnit }

class function TFixH2PasMissingIFDEFsInUnit.ClassDescription: string;
begin
  Result := h2pAddMissingH2pasIFDEFsForFunctionBodies;
end;

function TFixH2PasMissingIFDEFsInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Code: TCodeBuffer;
  Changed: Boolean;
begin
  Result:=mrCancel;
  Changed:=false;
  Code:=TCodeBuffer(aText.CodeBuffer);
  if not CodeToolBoss.FixMissingH2PasDirectives(Code,Changed) then begin
    AssignCodeToolBossError;
    DebugLn(['TFixH2PasMissingIFDEFsInUnit.Execute failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  Result:=mrOk;
end;

{ TReduceCompilerDirectivesInUnit }

procedure TReduceCompilerDirectivesInUnit.SetDefines(const AValue: TStrings);
begin
  if FDefines=AValue then exit;
  FDefines.Assign(AValue);
end;

procedure TReduceCompilerDirectivesInUnit.SetUndefines(const AValue: TStrings);
begin
  if FUndefines=AValue then exit;
  FUndefines.Assign(AValue);
end;

constructor TReduceCompilerDirectivesInUnit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUndefines:=TStringList.Create;
  FDefines:=TStringList.Create;
end;

destructor TReduceCompilerDirectivesInUnit.Destroy;
begin
  FreeAndNil(FUndefines);
  FreeAndNil(FDefines);
  inherited Destroy;
end;

class function TReduceCompilerDirectivesInUnit.ClassDescription: string;
begin
  Result := Format(h2pReduceCompilerDirectivesInPascalFileShortensExpres, [#13, #13]);
end;

function TReduceCompilerDirectivesInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Changed: Boolean;
  Code: TCodeBuffer;
begin
  Result:=mrCancel;
  Changed:=false;
  Code:=TCodeBuffer(aText.CodeBuffer);
  if not CodeToolBoss.ReduceCompilerDirectives(Code,Undefines,Defines,Changed)
  then begin
    AssignCodeToolBossError;
    DebugLn(['TReduceCompilerDirectivesInUnit.Execute failed ',ErrorMsg]);
    exit;
  end;
  Result:=mrOk;
end;

{ TReplaceConstFunctionsInUnit }

class function TReplaceConstFunctionsInUnit.ClassDescription: string;
begin
  Result := h2pReplaceSimpleFunctionsWithConstants;
end;

function TReplaceConstFunctionsInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Result:=mrCancel;
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TReplaceConstFunctionsInUnit.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.ReplaceAllConstFunctions(TCodeBuffer(aText.CodeBuffer)) then begin
    AssignCodeToolBossError;
    DebugLn(['TReplaceConstFunctionsInUnit.Execute ReplaceAllConstFunctions failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  Result:=mrOk;
end;

{ TReplaceTypeCastFunctionsInUnit }

class function TReplaceTypeCastFunctionsInUnit.ClassDescription: string;
begin
  Result := h2pReplaceSimpleFunctionsWithTypeCasts;
end;

function TReplaceTypeCastFunctionsInUnit.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Result:=mrCancel;
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TReplaceTypeCastFunctionsInUnit.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.ReplaceAllTypeCastFunctions(TCodeBuffer(aText.CodeBuffer)) then begin
    AssignCodeToolBossError;
    DebugLn(['TReplaceTypeCastFunctionsInUnit.Execute ReplaceAllTypeCastFunctions failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  Result:=mrOk;
end;

{ TPreH2PasTools }

constructor TPreH2PasTools.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOptions:=DefaultPreH2PasToolsOptions;
end;

class function TPreH2PasTools.ClassDescription: string;
begin
  Result := Format(h2pPreH2PasASetOfCommonToolsToRunBeforeH2pasPhRemoveC, [#13, #13, #13, #13, #13, #13, #13, #13, #13])
    ;
end;

function TPreH2PasTools.Execute(aText: TIDETextConverter): TModalResult;

  function Run(Option: TPreH2PasToolsOption;
    ToolClass: TCustomTextConverterToolClass;
    out aResult: TModalResult): boolean;
  var
    Tool: TCustomTextConverterTool;
  begin
    Result:=true;
    aResult:=mrOk;
    if not (Option in Options) then exit;
    DebugLn(['TPreH2PasTools.Execute.Run ',ToolClass.ClassName]);
    Tool:=ToolClass.Create(nil);
    try
      Tool.ClearError;
      aResult:=Tool.Execute(aText);
      if aResult<>mrOk then begin
        AssignError(Tool);
        DebugLn(['TPreH2PasTools.Execute.Run failed: ',ToolClass.ClassName]);
        exit(false);
      end;
    finally
      Tool.Free;
    end;
  end;

begin
  if not Run(phRemoveCPlusPlusExternCTool,
             TRemoveCPlusPlusExternCTool,Result) then exit;
  if not Run(phRemoveEmptyCMacrosTool,
             TRemoveEmptyCMacrosTool,Result) then exit;
  if not Run(phReplaceEdgedBracketPairWithStar,
             TReplaceEdgedBracketPairWithStar,Result) then exit;
  if not Run(phReplaceMacro0PointerWithNULL,
             TReplaceMacro0PointerWithNULL,Result) then exit;
  if not Run(phConvertFunctionTypesToPointers,
             TConvertFunctionTypesToPointers,Result) then exit;
  if not Run(phConvertEnumsToTypeDef,
             TConvertEnumsToTypeDef,Result) then exit;
  if not Run(phCommentComplexCMacros,
             TCommentComplexCMacros,Result) then exit;
  if not Run(phCommentComplexCFunctions,
             TCommentComplexCFunctions,Result) then exit;
  if not Run(phAddMissingMacroBrackets,
             TAddMissingMacroBrackets,Result) then exit;
  Result:=mrOk;
end;

{ TPostH2PasTools }

procedure TPostH2PasTools.SetDefines(const AValue: TStrings);
begin
  if FDefines=AValue then exit;
  FDefines.Assign(AValue);
end;

procedure TPostH2PasTools.SetUndefines(const AValue: TStrings);
begin
  if FUndefines=AValue then exit;
  FUndefines.Assign(AValue);
end;

procedure TPostH2PasTools.SetUseUnits(const AValue: TStrings);
begin
  if FUseUnits=AValue then exit;
  FUseUnits.Assign(FUseUnits);
end;

constructor TPostH2PasTools.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefines:=TStringList.Create;
  FUndefines:=TStringList.Create;
  FUseUnits:=TStringList.Create;
  FOptions:=DefaultPostH2PasToolsOptions;
end;

destructor TPostH2PasTools.Destroy;
begin
  FreeAndNil(FDefines);
  FreeAndNil(FUndefines);
  FreeAndNil(FUseUnits);
  inherited Destroy;
end;

class function TPostH2PasTools.ClassDescription: string;
begin
  Result := Format(h2pPostH2PasASetOfCommonToolsToRunAfterH2pasPhReplace, [#13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13, #13])
    ;
end;

function TPostH2PasTools.Execute(aText: TIDETextConverter): TModalResult;

  function Run(Option: TPostH2PasToolsOption;
    ToolClass: TCustomTextConverterToolClass;
    var aResult: TModalResult): boolean;
  var
    Tool: TCustomTextConverterTool;
  begin
    Result:=true;
    aResult:=mrOk;
    if not (Option in Options) then exit;
    DebugLn(['TPostH2PasTools.Execute.Run ',ToolClass.ClassName]);
    Tool:=ToolClass.Create(nil);
    try
      Tool.ClearError;
      aResult:=Tool.Execute(aText);
      if aResult<>mrOk then begin
        AssignError(Tool);
        DebugLn(['TPostH2PasTools.Execute.Run failed: ',ToolClass.ClassName]);
        exit(false);
      end;
    finally
      Tool.Free;
    end;
  end;
  
  function ReduceCompilerDirectives(var Changed: boolean;
    var aResult: TModalResult): boolean;
  var
    Code: TCodeBuffer;
  begin
    aResult:=mrOk;
    if not (phReduceCompilerDirectivesInUnit in Options) then exit;
    DebugLn(['TPostH2PasTools.Execute.ReduceCompilerDirectives ']);
    Code:=TCodeBuffer(aText.CodeBuffer);
    if not CodeToolBoss.ReduceCompilerDirectives(Code,Undefines,Defines,Changed)
    then begin
      DebugLn(['TPostH2PasTools.Execute.ReduceCompilerDirectives failed']);
      AssignCodeToolBossError;
      aResult:=mrCancel;
      exit(false);
    end;
    aResult:=mrOk;
    Result:=true;
  end;

  function AddToUsesSection(var Changed: boolean;
    var aResult: TModalResult): boolean;
  var
    i: Integer;
    UnitName: string;
  begin
    aResult:=mrOk;
    if not (phAddUnitsToUsesSection in Options) then exit;
    DebugLn(['TPostH2PasTools.Execute.AddToUsesSection ']);
    for i:=0 to FUseUnits.Count-1 do begin
      UnitName:=FUseUnits[i];
      if (UnitName='') then continue;
      if not IsValidIdent(UnitName) then
        raise Exception.Create(Format(h2pTPostH2PasToolsExecuteAddToUsesSectionInvalidUnitn, [UnitName]));
      Changed:=true;
      if not CodeToolBoss.AddUnitToMainUsesSection(TCodeBuffer(aText.CodeBuffer),UnitName,'')
      then begin
        AssignCodeToolBossError;
        DebugLn(['TPostH2PasTools.Execute.AddToUsesSection failed ',CodeToolBoss.ErrorMessage]);
        aResult:=mrCancel;
        exit(false);
      end;
    end;
    aResult:=mrOk;
    Result:=true;
  end;

  function ConvertSimpleFunctions(var Changed: boolean;
    var aResult: TModalResult): boolean;
  var
    Code: TCodeBuffer;
    OldChangeStep: LongInt;
  begin
    aResult:=mrOk;
    OldChangeStep:=CodeToolBoss.ChangeStep;
    if (phReplaceConstFunctionsInUnit in Options) then begin
      DebugLn(['TPostH2PasTools.Execute ReplaceAllConstFunctions ']);
      Code:=TCodeBuffer(aText.CodeBuffer);
      if not CodeToolBoss.ReplaceAllConstFunctions(Code) then begin
        DebugLn(['ReplaceAllConstFunctions failed']);
        AssignCodeToolBossError;
        aResult:=mrCancel;
        exit(false);
      end;
    end;
    if (phReplaceTypeCastFunctionsInUnit in Options) then begin
      Code:=TCodeBuffer(aText.CodeBuffer);
      DebugLn(['TPostH2PasTools.Execute ReplaceAllTypeCastFunctions ']);
      if not CodeToolBoss.ReplaceAllTypeCastFunctions(Code) then begin
        DebugLn(['ReplaceAllTypeCastFunctions failed']);
        AssignCodeToolBossError;
        aResult:=mrCancel;
        exit(false);
      end;
    end;
    if OldChangeStep<>CodeToolBoss.ChangeStep then
      Changed:=true;
    aResult:=mrOk;
    Result:=true;
  end;

  function FixAliasDefinitions(var Changed: boolean;
    var aResult: TModalResult): boolean;
  var
    Code: TCodeBuffer;
    OldChangeStep: LongInt;
  begin
    aResult:=mrOk;
    OldChangeStep:=CodeToolBoss.ChangeStep;
    if (phFixAliasDefinitionsInUnit in Options) then begin
      DebugLn(['TPostH2PasTools.Execute FixAllAliasDefinitions ']);
      Code:=TCodeBuffer(aText.CodeBuffer);
      if not CodeToolBoss.FixAllAliasDefinitions(Code) then begin
        DebugLn(['FixAliasDefinitions failed']);
        AssignCodeToolBossError;
        aResult:=mrCancel;
        exit(false);
      end;
    end;
    if OldChangeStep<>CodeToolBoss.ChangeStep then
      Changed:=true;
    aResult:=mrOk;
    Result:=true;
  end;

var
  Changed: boolean;
begin
  Result:=mrOk;
  Changed:=false;
  // basic h2pas fixes (unit name, system types, missing IFDEFs)
  if not Run(phReplaceUnitFilenameWithUnitName,
             TReplaceUnitFilenameWithUnitName,Result) then exit;
  if not Run(phRemoveIncludeDirectives,
             TRemoveIncludeDirectives,Result) then exit;
  if not Run(phRemoveDoubleSemicolons,
             TRemoveDoubleSemicolons,Result) then exit;
  if not Run(phRemoveSystemTypes,
             TRemoveSystemTypes,Result) then exit;
  if not Run(phFixH2PasMissingIFDEFsInUnit,
             TFixH2PasMissingIFDEFsInUnit,Result) then exit;
  // reduce compiler directives so that other tools can work with less double data
  if not ReduceCompilerDirectives(Changed,Result) then exit;
  // remove h2pas redefinitions to get unambiguous types
  if not Run(phRemoveRedefinedPointerTypes,
             TRemoveRedefinedPointerTypes,Result) then exit;
  if not Run(phRemoveEmptyTypeVarConstSections,
             TRemoveEmptyTypeVarConstSections,Result) then exit;
  // add / replace implicit types, not converted by h2pas
  if not Run(phReplaceImplicitTypes,
             TReplaceImplicitTypes,Result) then exit;
  if not Run(phFixArrayOfParameterType,
             TFixArrayOfParameterType,Result) then exit;
  if not Run(phAddMissingPointerTypes,
             TAddMissingPointerTypes,Result) then exit;
  // remove redefinitions, to get unambiguous types
  if not Run(phRemoveRedefinitionsInUnit,
             TRemoveRedefinitionsInUnit,Result) then exit;

  // optimization
  repeat
    Changed:=false;
    if not ReduceCompilerDirectives(Changed,Result) then exit;
    if not FixAliasDefinitions(Changed,Result) then exit;
    if not ConvertSimpleFunctions(Changed,Result) then exit;
  until Changed=false;
  
  // fix forward definitions
  if not Run(phFixForwardDefinitions,
             TFixForwardDefinitions,Result) then exit;
  // add units to uses section
  if not AddToUsesSection(Changed,Result) then exit;
end;

{ TRemoveIncludeDirectives }

class function TRemoveIncludeDirectives.ClassDescription: string;
begin
  Result := h2pRemoveAllIncludeDirectives;
end;

constructor TRemoveIncludeDirectives.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SearchFor:='\{\$(include|i)\b.*\}';
  ReplaceWith:='';
  Options:=Options+[trtRegExpr];
end;

{ TConvertFunctionTypesToPointers }

class function TConvertFunctionTypesToPointers.ClassDescription: string;
begin
  Result := h2pConvertFunctionTypesToPointers;
end;

function TConvertFunctionTypesToPointers.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Src: String;
  SrcLen: Integer;
  FuncTypes: TAVLTree; // tree of TImplicitType

  procedure CheckTypeDef(var p: integer);
  // Check if it is:  typedef identifier ( funcname ) (
  var
    StartPos: LongInt;
    EndPos: LongInt;
    NewType: TImplicitType;
  begin
    // typedef found
    inc(p,length('typedef'));
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    // skip identifier
    if not IsIdentStartChar[Src[p]] then exit;
    while (p<SrcLen) and IsIdentChar[Src[p]] do inc(p);
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    // skip (
    if Src[p]<>'(' then exit;
    inc(p);
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    if p>=SrcLen then exit;
    // read name of function type
    StartPos:=p;
    if not IsIdentStartChar[Src[p]] then exit;
    while (p<SrcLen) and IsIdentChar[Src[p]] do inc(p);
    EndPos:=p;
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    if p>=SrcLen then exit;
    // skip )
    if Src[p]<>')' then exit;
    inc(p);
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    if p>=SrcLen then exit;
    // skip (
    if Src[p]<>'(' then exit;
    // function type found
    NewType:=TImplicitType.Create;
    NewType.Name:=copy(Src,StartPos,EndPos-StartPos);
    writeln('TConvertFunctionTypesToPointers.Execute.CheckType function type found  Name=',NewType.Name);
    if FuncTypes=nil then
      FuncTypes:=TAVLTree.Create(@CompareImplicitTypeNames);
    FuncTypes.Add(NewType);
    // add * in front of name
    System.Insert('*',Src,StartPos);
    SrcLen:=length(Src);
  end;
  
  procedure CheckIdentifier(var p: integer);
  var
    IdentPos: LongInt;
    IdentEnd: LongInt;
  begin
    IdentPos:=p;
    // skip identifier
    while (p<=SrcLen) and IsIdentChar[Src[p]] do inc(p);
    if FuncTypes.FindKey(@Src[IdentPos],@CompareImplicitTypeStringAndName)=nil
    then
      exit;
    // this identifier is a function type
    IdentEnd:=p;
    // skip space
    while (p<SrcLen) and IsSpaceChar[Src[p]] do inc(p);
    if p>=SrcLen then exit;
    // remove * behind identifier
    if Src[p]<>'*' then exit;
    writeln('TConvertFunctionTypesToPointers.Execute.CheckIdentifier removing * behind reference to ',GetIdentifier(@Src[IdentPos]));
    System.Delete(Src,IdentEnd,p-IdentEnd+1);
    SrcLen:=length(Src);
    p:=IdentEnd;
  end;

var
  p: Integer;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  FuncTypes:=nil;
  try
    Src:=aText.Source;
    SrcLen:=length(Src);
    // Search all  typedef identifier ( funcname ) (
    // and insert a * in front of the funcname
    p:=1;
    while (p<SrcLen) do begin
      if (Src[p]='t') and ((p=1) or (not IsIdentChar[Src[p-1]]))
      and (CompareIdentifiers('typedef',@Src[p])=0) then begin
        CheckTypeDef(p);
      end else
        inc(p);
    end;
    if FuncTypes<>nil then begin
      // remove the * behind all references
      p:=1;
      while (p<SrcLen) do begin
        if (IsIdentStartChar[Src[p]]) and ((p=1) or (not IsIdentChar[Src[p-1]]))
        then begin
          CheckIdentifier(p);
        end else
          inc(p);
      end;
    end;
  finally
    if FuncTypes<>nil then begin
      FuncTypes.FreeAndClear;
      FuncTypes.Free;
      aText.Source:=Src;
    end;
  end;
  
  Result:=mrOk;
end;

{ TFixForwardDefinitions }

class function TFixForwardDefinitions.ClassDescription: string;
begin
  Result := h2pFixForwardDefinitionsByReordering;
end;

function TFixForwardDefinitions.Execute(aText: TIDETextConverter
  ): TModalResult;
begin
  Result:=mrCancel;
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TFixForwardDefinitions.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.FixForwardDefinitions(TCodeBuffer(aText.CodeBuffer)) then begin
    AssignCodeToolBossError;
    DebugLn(['TFixForwardDefinitions.Execute failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  Result:=mrOk;
end;

{ TRemoveDoubleSemicolons }

class function TRemoveDoubleSemicolons.ClassDescription: string;
begin
  Result := h2pRemoveDoubleSemicolons;
end;

function TRemoveDoubleSemicolons.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Position: Integer;
  Source, NewSrc: String;
  AtomStart: integer;
  LastAtomWasSemicolon: Boolean;
  SemicolonPositions: array of integer;
  SemicolonCount: Integer;
  i: Integer;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Source:=aText.Source;
  //DebugLn(['TRemoveDoubleSemicolons.Execute START ',Source]);

  // find all double semicolons
  Position:=1;
  LastAtomWasSemicolon:=false;
  Setlength(SemicolonPositions,0);
  SemicolonCount:=0;
  repeat
    ReadRawNextPascalAtom(Source,Position,AtomStart,true);
    if AtomStart>length(Source) then break;
    if Source[AtomStart]=';' then begin
      if LastAtomWasSemicolon then begin
        if length(SemicolonPositions)<=SemicolonCount then
          SetLength(SemicolonPositions,length(SemicolonPositions)*2+2);
        SemicolonPositions[SemicolonCount]:=AtomStart;
        inc(SemicolonCount);
      end;
      LastAtomWasSemicolon:=true;
    end else begin
      LastAtomWasSemicolon:=false;
    end;
  until false;

  // build new source without semicolons
  if SemicolonCount>0 then begin
    SetLength(NewSrc,length(Source)-SemicolonCount);
    AtomStart:=1;
    i:=0;
    while i<SemicolonCount do begin
      Position:=SemicolonPositions[i];
      if Position>AtomStart then
        System.Move(Source[AtomStart],NewSrc[AtomStart-i],Position-AtomStart);
      AtomStart:=Position+1;
      inc(i);
    end;
    Position:=length(Source)+1;
    if Position>AtomStart then
      System.Move(Source[AtomStart],NewSrc[AtomStart-i],Position-AtomStart);
    aText.Source:=NewSrc;
  end;

  // clean up
  Setlength(SemicolonPositions,0);
  Result:=mrOk;
end;

{ TAddMissingPointerTypes }

class function TAddMissingPointerTypes.ClassDescription: string;
begin
  Result := h2pAddMissingPointerTypesLikePPPChar;
end;

function TAddMissingPointerTypes.Execute(aText: TIDETextConverter
  ): TModalResult;
{ h2pas converts implicit pointer types like 'Identifier ***' to PPPIdentifier,
  but it only adds PIdentifier = ^Identifier.
  This tool adds the missing
    PPIdentifier = ^PIdentifier;
    PPPIdentifier = ^PPIdentifier;
}
var
  Tool: TCodeTool;
  Definitions: TAVLTree;// tree of TCodeTreeNodeExtension
  NeededPointerTypes: TAVLTree; // tree of TImplicitType
  DefaultTypeSectionPos: integer;
  
  function IdentifierIsDefined(Identifier: PChar): boolean;
  var
    i: Integer;
  begin
    if WordIsKeyWord.DoItCaseInsensitive(Identifier) then exit(true);
    if WordIsPredefinedFPCIdentifier.DoItCaseInsensitive(Identifier) then exit(true);
    if (Definitions<>nil)
    and (Definitions.FindKey(Identifier,@CompareIdentifierWithCodeTreeNodeExt)<>nil)
    then exit(true);
    for i:=Low(PreDefinedH2PasTypes) to High(PreDefinedH2PasTypes) do begin
      if CompareIdentifierPtrs(Identifier,Pointer(PreDefinedH2PasTypes[i]))=0 then
        exit(true);
      // check for predefined pointer types
      if (Identifier^ in ['p','P'])
      and (IsIdentChar[Identifier[1]])
      and (CompareIdentifierPtrs(@Identifier[1],Pointer(PreDefinedH2PasTypes[i]))=0)
      then
        exit(true);
    end;
    //DebugLn(['IdentifierIsDefined not found: ',GetIdentifier(Identifier)]);
    Result:=false;
  end;
  
  procedure AddNeededPointerType(Position, Count: integer);
  var
    Item: TImplicitType;
    Identifier: PChar;
    AVLNode: TAVLTreeNode;
  begin
    if NeededPointerTypes=nil then
      NeededPointerTypes:=TAVLTree.Create(@CompareImplicitTypeNames);
    Identifier:=@Tool.Src[Position+Count];
    AVLNode:=NeededPointerTypes.FindKey(Identifier,
                                        @CompareImplicitTypeStringAndName);
    DebugLn(['AddNeededPointerType Identifier ',GetIdentifier(Identifier),' Position=',Position,' Count=',Count]);
    DebugLn(['AddNeededPointerType Position ',copy(Tool.Src,Position,100)]);
    if AVLNode<>nil then begin
      Item:=TImplicitType(AVLNode.Data);
      if Item.MaxPosition<Count then
        Item.MaxPosition:=Count;
    end else begin
      Item:=TImplicitType.Create;
      Item.Name:=GetIdentifier(Identifier);
      Item.MinPosition:=Position;
      Item.MaxPosition:=Count;
      NeededPointerTypes.Add(Item);
    end;
  end;
  
  procedure CheckIdentifier(Position: integer);
  var
    Identifier: PChar;
    Level: Integer;
  begin
    Identifier:=@Tool.Src[Position];
    Level:=0;
    while (Identifier[Level] in ['p','P']) do begin
      // this identifier starts with a P, so it can be a pointer type
      if IdentifierIsDefined(@Tool.Src[Position+Level]) then break;
      inc(Level);
    end;
    //DebugLn(['CheckIdentifier ',GetIdentifier(Identifier),' Level=',Level]);
    if Level=0 then begin
      // the identifier is defined
      exit;
    end;
    if (not (Identifier[Level] in ['p','P']))
    and (IsIdentChar[Identifier[Level]])
    and not (IdentifierIsDefined(@Identifier[Level])) then begin
      // the base type is not defined
      // => this is not a pointer type
      exit;
    end;
    AddNeededPointerType(Position,Level);
  end;
  
  function AddNeededPointerTypesToSource(Item: TImplicitType): boolean;
  var
    AVLNode: TAVLTreeNode;
    NodeExt: TCodeTreeNodeExtension;
    Node: TCodeTreeNode;
    i: Integer;
    NewTxt: String;
    InsertPos: LongInt;
    Indent: LongInt;
    Identifier: String;
  begin
    Result:=false;
    
    CodeToolBoss.SourceChangeCache.MainScanner:=Tool.Scanner;
    
    // find definition
    InsertPos:=0;
    if (Definitions<>nil) then begin
      AVLNode:=Definitions.FindKey(Pointer(Item.Name),
                                   @CompareIdentifierWithCodeTreeNodeExt);
      if AVLNode<>nil then begin
        NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
        Node:=NodeExt.Node;
        InsertPos:=Tool.FindLineEndOrCodeAfterPosition(Node.EndPos);
        Indent:=GetLineIndent(Tool.Src,Node.StartPos);
      end;
    end;
    if (InsertPos<1) then begin
      if DefaultTypeSectionPos<1 then begin
        // start a type section at the beginning
        Node:=Tool.FindMainUsesNode(false);
        if Node<>nil then begin
          if Node.NextBrother<>nil then
            Node:=Node.NextBrother;
        end else begin
          Node:=Tool.FindInterfaceNode;
          if Node<>nil then begin
            if Node.FirstChild<>nil then
              Node:=Node.FirstChild;
          end;
        end;
        if Node<>nil then begin
          if Node.Desc=ctnUsesSection then begin
            // insert behind node
            DefaultTypeSectionPos:=
                    Tool.FindLineEndOrCodeAfterPosition(Node.EndPos);
          end else if Node.Desc=ctnInterface then begin
            // insert at end of node
            DefaultTypeSectionPos:=Node.EndPos;
          end else begin
            // insert in front of node
            DefaultTypeSectionPos:=
                    Tool.FindLineEndOrCodeInFrontOfPosition(Node.StartPos,true);
          end;
        end else begin
          DefaultTypeSectionPos:=1;
        end;
        DebugLn(['AddNeededPointerTypesToSource start type section']);
        if not CodeToolBoss.SourceChangeCache.Replace(gtEmptyLine,gtNewLine,
          DefaultTypeSectionPos,DefaultTypeSectionPos,'type') then exit;
      end;
      InsertPos:=DefaultTypeSectionPos;
      Indent:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.Indent;
    end;
    
    // add pointer types
    Identifier:=Item.Name;
    NewTxt:='';
    for i:=Item.MaxPosition downto 1 do begin
      if NewTxt<>'' then
        NewTxt:=NewTxt+CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.LineEnd;
      NewTxt:=NewTxt+GetIndentStr(Indent)+'P'+Identifier+'=^'+Identifier+';';
      Identifier:='P'+Identifier;
    end;
    DebugLn(['AddNeededPointerTypesToSource Add pointer types: "',NewTxt,'"']);
    Result:=CodeToolBoss.SourceChangeCache.Replace(gtNewLine,gtNewLine,
      InsertPos,InsertPos,NewTxt);
  end;
  
  function CheckTypes: boolean;
  var
    Node: TCodeTreeNode;
  begin
    Node:=Tool.Tree.Root;
    while Node<>nil do begin
      if (Node.Desc in [ctnIdentifier,ctnOpenArrayType,
        ctnRangedArrayType,ctnTypeType,ctnPointerType,ctnConstant])
      and (Node.FirstChild=nil)
      then begin
        Tool.MoveCursorToCleanPos(Node.StartPos);
        while Tool.CurPos.StartPos<Node.EndPos do begin
          Tool.ReadNextAtom;
          if Tool.CurPos.StartPos>=Node.EndPos then break;
          if (Tool.CurPos.Flag=cafWord) then
            CheckIdentifier(Tool.CurPos.StartPos);
        end;
        Node:=Node.NextSkipChilds;
      end else
        Node:=Node.Next;
    end;
    Result:=true;
  end;

  function AddNeededPointerTypesToSource: boolean;
  var
    AVLNode: TAVLTreeNode;
    Item: TImplicitType;
  begin
    Result:=true;
    if NeededPointerTypes<>nil then begin
      AVLNode:=NeededPointerTypes.FindLowest;
      while AVLNode<>nil do begin
        Item:=TImplicitType(AVLNode.Data);
        if not AddNeededPointerTypesToSource(Item) then exit;
        AVLNode:=NeededPointerTypes.FindSuccessor(AVLNode);
      end;
      Result:=CodeToolBoss.SourceChangeCache.Apply;
    end;
  end;
  
begin
  Result:=mrCancel;
  if aText=nil then exit;
  DebugLn(['TAddMissingPointerTypes.Execute START ',aText.Source]);
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TAddMissingPointerTypes.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  if not CodeToolBoss.Explore(TCodeBuffer(aText.CodeBuffer),Tool,true,false)
  then begin
    AssignCodeToolBossError;
    DebugLn(['TAddMissingPointerTypes.Execute Explore failed ',CodeToolBoss.ErrorMessage]);
    exit;
  end;
  DebugLn(['TAddMissingPointerTypes.Execute ']);
  Definitions:=nil;
  NeededPointerTypes:=nil;
  DefaultTypeSectionPos:=0;
  try
    // collect definitions
    if not Tool.GatherUnitDefinitions(Definitions,true,false) then begin
      AssignCodeToolBossError;
      DebugLn(['TAddMissingPointerTypes.Execute GatherUnitDefinitions failed ',CodeToolBoss.ErrorMessage]);
      exit;
    end;
    // check all used identifiers
    if not CheckTypes then exit;
    // add all needed pointer types
    if not AddNeededPointerTypesToSource then exit;
  finally
    if Definitions<>nil then begin
      DisposeAVLTree(Definitions);
      Definitions:=nil;
    end;
    if NeededPointerTypes<>nil then begin
      NeededPointerTypes.FreeAndClear;
      NeededPointerTypes.Free;
    end;
  end;
  DebugLn(['TAddMissingPointerTypes.Execute END ',aText.Source]);
  Result:=mrOk;
end;

{ TConvertEnumsToTypeDef }

class function TConvertEnumsToTypeDef.ClassDescription: string;
begin
  Result := h2pGiveAnonymousCEnumsATypedefName;
end;

function TConvertEnumsToTypeDef.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Src: String;
  SrcLen: Integer;
  
  function CreateEnumName(StartPos, EndPos: integer): string;
  var
    AtomStart: LongInt;
  begin
    Result:='';
    AtomStart:=StartPos;
    while StartPos<=EndPos do begin
      ReadNextCAtom(Src,StartPos,AtomStart);
      if AtomStart>SrcLen then exit;
      if IsIdentStartChar[Src[AtomStart]] then begin
        Result:=Result+copy(Src,AtomStart,StartPos-AtomStart);
        if length(Result)>60 then exit;
      end;
    end;
  end;

var
  p: Integer;
  AtomStart: Integer;
  LastAtomStart: LongInt;
  Changed: Boolean;

  procedure AdjustAfterReplace(var APosition: integer;
    FromPos, ToPos, NewLength: integer);
  begin
    if APosition<FromPos then
      exit
    else if APosition<ToPos then
      APosition:=FromPos
    else
      inc(APosition,NewLength-(FromPos-ToPos));
  end;
  
  procedure Replace(FromPos, ToPos: integer; const NewSrc: string);
  begin
    DebugLn(['TConvertEnumsToTypeDef.Execute.Replace ',FromPos,'-',ToPos,' NewSrc="',NewSrc,'"']);
    Src:=copy(Src,1,FromPos-1)+NewSrc+copy(Src,ToPos,length(Src));
    AdjustAfterReplace(p,FromPos,ToPos,length(NewSrc));
    AdjustAfterReplace(AtomStart,FromPos,ToPos,length(NewSrc));
    AdjustAfterReplace(LastAtomStart,FromPos,ToPos,length(NewSrc));
    Changed:=true;
  end;
  
var
  EnumStart: LongInt;
  EnumEnd: LongInt;
  EnumName: String;
  BracketStart: LongInt;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Changed:=false;
  Src:=aText.Source;
  SrcLen:=length(Src);
  p:=1;
  AtomStart:=1;
  LastAtomStart:=-1;
  repeat
    ReadNextCAtom(Src,p,AtomStart);
    if p>SrcLen then break;
    //DebugLn(['TConvertEnumsToTypeDef.Execute ',AtomStart,' "',dbgstr(copy(Src,AtomStart,p-AtomStart)),'"']);
    case Src[AtomStart] of
    'a'..'z','A'..'Z','_':
      begin
        // identifier
        if (CompareCIdentifiers(@Src[AtomStart],'enum')=0)
        and ((LastAtomStart<1)
             or (CompareCIdentifiers(@Src[AtomStart],'typedef')<>0)) then
        begin
          // enum without typedef
          DebugLn(['TConvertEnumsToTypeDef.Execute enum without typedef found']);
          EnumStart:=AtomStart;
          // read curly bracket open
          ReadNextCAtom(Src,p,AtomStart);
          if (AtomStart>SrcLen) or (Src[AtomStart]<>'{') then break;
          BracketStart:=AtomStart;
          // read til curly bracket close
          if not ReadTilCBracketClose(Src,AtomStart) then break;
          p:=AtomStart;
          // read semicolon
          ReadNextCAtom(Src,p,AtomStart);
          if (AtomStart>SrcLen) or (Src[AtomStart]<>';') then break;
          EnumEnd:=AtomStart;
          DebugLn(['TConvertEnumsToTypeDef.Execute Enum block: ',copy(Src,EnumStart,EnumEnd-EnumStart)]);
          // read enums to create a unique name
          EnumName:=CreateEnumName(BracketStart,EnumEnd);
          if EnumName='' then begin
            // empty enum => remove
            Replace(EnumStart,EnumEnd,'');
          end else begin
            // insert 'typedef' and name
            // IMPORTANT: insert in reverse order
            Replace(EnumEnd,EnumEnd,EnumName);
            Replace(EnumStart,EnumStart,'typedef ');
          end;
        end;
      end;
    end;
    LastAtomStart:=AtomStart;
  until false;
  
  if Changed then
    aText.Source:=Src;
  Result:=mrOk;
end;

{ TCommentComplexCMacros }

class function TCommentComplexCMacros.ClassDescription: string;
begin
  Result := h2pCommentMacrosThatAreTooComplexForH2pas;
end;

function TCommentComplexCMacros.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Src: String;
  SrcLen: Integer;

  function DefineIsTooComplex(StartPos, EndPos: integer): boolean;
  // h2pas has problems with
  // - backslash + newline
  // - whole functions { }
  var
    p: LongInt;
    AtomStart: integer;
  begin
    p:=StartPos;
    repeat
      ReadRawNextCAtom(Src,p,AtomStart);
      if (AtomStart>=EndPos) or (AtomStart>length(Src)) then break;
      if Src[AtomStart]='{' then begin
        // this macro is a whole function => too complex
        exit(true);
      end;
      if (Src[AtomStart] in [#10,#13]) then begin
        // this macro uses multiple lines => too complex
        exit(true);
      end;
    until false;
    Result:=false;
  end;

var
  Changed: Boolean;
  p: Integer;
  AtomStart: Integer;

  procedure AdjustAfterReplace(var APosition: integer;
    FromPos, ToPos, NewLength: integer);
  begin
    if APosition<FromPos then
      exit
    else if APosition<ToPos then
      APosition:=FromPos
    else
      inc(APosition,NewLength-(FromPos-ToPos));
  end;

  procedure Replace(FromPos, ToPos: integer; const NewSrc: string);
  begin
    //DebugLn(['TCommentComplexCMacros.Execute.Replace ',FromPos,'-',ToPos,' NewSrc="',NewSrc,'"']);
    Src:=copy(Src,1,FromPos-1)+NewSrc+copy(Src,ToPos,length(Src));
    AdjustAfterReplace(p,FromPos,ToPos,length(NewSrc));
    AdjustAfterReplace(AtomStart,FromPos,ToPos,length(NewSrc));
    Changed:=true;
  end;
  
  procedure Comment(StartPos, EndPos: integer);
  begin
    // replace sub comments
    while (StartPos<EndPos-1) do begin
      if (Src[StartPos]='/') and (Src[StartPos+1]='*') then begin
        // sub comment found -> disable
        // IMPORTANT: replacement must be the same size to keep the positions
        Replace(StartPos,StartPos+1,'(');
      end;
      if (Src[StartPos]='*') and (Src[StartPos+1]='/') then begin
        // sub comment found -> disable
        // IMPORTANT: replacement must be the same size to keep the positions
        Replace(StartPos+1,StartPos+2,')');
      end;
      inc(StartPos);
    end;

    // IMPORTANT: insert in reverse order
    Replace(EndPos,EndPos,'*/');
    Replace(StartPos,StartPos,'/*');
  end;

var
  DefineStart: LongInt;
  DefineEnd: LongInt;
  ValueStart: LongInt;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Changed:=false;
  Src:=aText.Source;
  SrcLen:=length(Src);
  p:=1;
  AtomStart:=1;
  repeat
    ReadRawNextCAtom(Src,p,AtomStart);
    if p>SrcLen then break;
    if (Src[AtomStart]='#') and (AtomStart<SrcLen) then begin
      // pragma found
      if CompareCIdentifiers(@Src[AtomStart+1],'define')=0 then begin
        // #define found
        DefineStart:=AtomStart;
        inc(p,length('define'));
        ValueStart:=p;
        ReadTilCLineEnd(Src,p);
        DefineEnd:=p;
        if DefineIsTooComplex(ValueStart,DefineEnd) then begin
          DebugLn(['TCommentComplexCMacros.Execute commenting macro "',dbgstr(copy(Src,DefineStart,DefineEnd-DefineStart)),'"']);
          Comment(DefineStart,DefineEnd);
        end;
      end;
    end;
  until false;

  if Changed then
    aText.Source:=Src;
  Result:=mrOk;
end;

{ TCommentComplexCFunctions }

class function TCommentComplexCFunctions.ClassDescription: string;
begin
  Result := h2pCommentFunctionsThatAreTooComplexForH2pas;
end;

function TCommentComplexCFunctions.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Src: String;
  SrcLen: Integer;

  function DefineIsTooComplex(StartPos, EndPos: integer): boolean;
  // h2pas has problems with
  // - backslash + newline
  // - whole functions { }
  begin
    while (StartPos<EndPos) do begin
      if Src[StartPos]='{' then begin
        // this macro is a whole function => too complex
        exit(true);
      end;
      if (Src[StartPos] in [#10,#13]) then begin
        // this macro uses multiple lines => too complex
        exit(true);
      end;
      inc(StartPos);
    end;
    Result:=false;
  end;

var
  Changed: Boolean;
  p: Integer;
  AtomStart: Integer;
  DefinitionStart: Integer;

  procedure AdjustAfterReplace(var APosition: integer;
    FromPos, ToPos, NewLength: integer);
  begin
    if APosition<FromPos then
      exit
    else if APosition<ToPos then
      APosition:=FromPos
    else
      inc(APosition,NewLength-(FromPos-ToPos));
  end;

  procedure Replace(FromPos, ToPos: integer; const NewSrc: string);
  begin
    Src:=copy(Src,1,FromPos-1)+NewSrc+copy(Src,ToPos,length(Src));
    AdjustAfterReplace(p,FromPos,ToPos,length(NewSrc));
    AdjustAfterReplace(AtomStart,FromPos,ToPos,length(NewSrc));
    AdjustAfterReplace(DefinitionStart,FromPos,ToPos,length(NewSrc));
    Changed:=true;
  end;

  procedure Comment(StartPos, EndPos: integer);
  begin
    // replace sub comments
    while (StartPos<EndPos-1) do begin
      if (Src[StartPos]='/') and (Src[StartPos+1]='*') then begin
        // sub comment found -> disable
        // IMPORTANT: replacement must be the same size to keep the positions
        Replace(StartPos,StartPos+1,'(');
      end;
      if (Src[StartPos]='*') and (Src[StartPos+1]='/') then begin
        // sub comment found -> disable
        // IMPORTANT: replacement must be the same size to keep the positions
        Replace(StartPos+1,StartPos+2,')');
      end;
      inc(StartPos);
    end;

    // IMPORTANT: insert in reverse order
    Replace(EndPos,EndPos,'*/');
    Replace(StartPos,StartPos,'/*');
  end;

  function ReadFunction: boolean;
  var
    FuncEnd: LongInt;
  begin
    Result:=false;
    //DebugLn(['ReadFunction START "',copy(Src,AtomStart,p-AtomStart),'"']);
    // a C function works like this:
    // [modifiers, macros] type name(param list){ statements }
    // 'type' can be an identifier or identifier* or something with brackets

    // read name
    if not IsIdentStartChar[Src[AtomStart]] then exit;
    ReadNextCAtom(Src,p,AtomStart);
    if p>SrcLen then exit;
    // read round bracket open
    if Src[AtomStart]<>'(' then exit;
    p:=AtomStart;
    if not ReadTilCBracketClose(Src,p) then exit;
    // read curly bracket open
    ReadNextCAtom(Src,p,AtomStart);
    if p>SrcLen then exit;
    if Src[AtomStart]<>'{' then exit;
    p:=AtomStart;
    if not ReadTilCBracketClose(Src,p) then exit;
    // function found
    FuncEnd:=p;
    Result:=true;
    DebugLn(['TCommentComplexCFunctions.Execute.ReadFunction Function="',copy(Src,DefinitionStart,FuncEnd-DefinitionStart),'"']);
    Comment(DefinitionStart,FuncEnd);
  end;

var
  OldP: LongInt;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Changed:=false;
  Src:=aText.Source;
  SrcLen:=length(Src);
  p:=1;
  AtomStart:=1;
  DefinitionStart:=0;
  repeat
    // read next definition
    ReadNextCAtom(Src,p,AtomStart);
    if p>SrcLen then break;
    if Src[AtomStart]=';' then begin
      // definition end found
      DefinitionStart:=0;
      continue;
    end else if Src[AtomStart]='{' then begin
      // block found = definition end found
      DefinitionStart:=0;
      p:=AtomStart;
      if not ReadTilCBracketClose(Src,p) then break;
      continue;
    end else begin
      // in definition
      if DefinitionStart<1 then
        DefinitionStart:=AtomStart;
      if Src[AtomStart] in ['(','['] then begin
        // skip bracket
        p:=AtomStart;
        if not ReadTilCBracketClose(Src,p) then break;
      end else if IsIdentStartChar[Src[AtomStart]] then begin
        // identifier found => check if function
        OldP:=p;
        if ReadFunction then begin
          DefinitionStart:=0;
        end else begin
          p:=OldP;
        end;
      end;
    end;
  until false;

  if Changed then
    aText.Source:=Src;
  Result:=mrOk;
end;

{ TAddToUsesSection }

procedure TAddToUsesSection.SetUseUnits(const AValue: TStrings);
begin
  if FUseUnits=AValue then exit;
  FUseUnits.Assign(AValue);
end;

constructor TAddToUsesSection.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FUseUnits:=TStringList.Create;
end;

destructor TAddToUsesSection.Destroy;
begin
  FreeAndNil(FUseUnits);
  inherited Destroy;
end;

class function TAddToUsesSection.ClassDescription: string;
begin
  Result := h2pAddUnitsToUsesSection;
end;

function TAddToUsesSection.Execute(aText: TIDETextConverter): TModalResult;
var
  AUnitName: string;
  i: Integer;
begin
  Result:=mrCancel;
  if (not FilenameIsPascalUnit(aText.Filename)) then begin
    DebugLn(['TAddToUsesSection.Execute file is not pascal: ',aText.Filename]);
    exit(mrOk);// ignore
  end;
  for i:=0 to FUseUnits.Count-1 do begin
    AUnitName:=FUseUnits[i];
    if (AUnitName='') then continue;
    if not IsValidIdent(AUnitName) then
      raise Exception.Create(Format(h2pTAddToUsesSectionExecuteInvalidUnitname, [AUnitName]));
    if not CodeToolBoss.AddUnitToMainUsesSection(TCodeBuffer(aText.CodeBuffer),AUnitName,'')
    then begin
      AssignCodeToolBossError;
      DebugLn(['TAddToUsesSection.Execute failed ',CodeToolBoss.ErrorMessage]);
      exit;
    end;
  end;
  Result:=mrOk;
end;

{ TAddMissingMacroBrackets }

class function TAddMissingMacroBrackets.ClassDescription: string;
begin
  Result := h2pAddMissingBracketsAroundMacroValues;
end;

function TAddMissingMacroBrackets.Execute(aText: TIDETextConverter
  ): TModalResult;
var
  Macro: String;
  Lines: TStrings;
  i: Integer;
  Line: string;
  Value: String;
begin
  Result:=mrCancel;
  if aText=nil then exit;
  Lines:=aText.Strings;
  i:=0;
  while i<=Lines.Count-1 do begin
    Line:=Lines[i];
    // example: #define READ_CURRENT_IAC_LAP_RP_SIZE 2+3*MAX_IAC_LAP
    if REMatches(Line,'^(#define\s+[a-zA-Z0-9_]+\s+)(.+)')
    then begin
      Macro:=REVar(1);
      Value:=REVar(2);
      if (Value<>'') and (Value[1]<>'(')
      and (REMatches(Value,'[^a-zA-Z0-9_()]')) then begin
        // macro needs values
        Line:=Macro+'('+Value+')';
        Lines[i]:=Line;
      end;
    end;
    inc(i);
  end;
  Result:=mrOk;
end;

end.
