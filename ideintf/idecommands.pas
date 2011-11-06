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

 Abstract:
   Interface unit for IDE commands.
   IDE commands are functions like open file, save, build, ... .
   
   Every command can have up to two shortcuts. For example:
     ecCopy: two shortcuts: Ctrl+C and Ctrl+Insert
     ecDeleteChar: one shortcut: Delete
     ecInsertDateTime: no shortcut
   
   Commands are sorted into categories. For example:
     ecCopy is in the category 'Selection'.
     This is only to help the user find commands.
     
   Scopes:
     A command can work globally or only in some IDE windows.
     For example: When the user presses a key in the source editor, the IDE
       first searches in all commands with the Scope IDECmdScopeSrcEdit.
       Then it will search in all commands without scope.
}
unit IDECommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, LCLType, Menus, PropEdits, TextTools;
  
const
  { editor commands constants. see syneditkeycmds.pp for more

   These values can change from version to version, so DO NOT save them to file!

   To add one static key do the following:
     1. Add a constant with a unique value in the list below.
     2. Add it to GetDefaultKeyForCommand to define the default keys+shiftstates
     3. Add it to EditorCommandToDescriptionString to define the description
     4. Add it to TKeyCommandRelationList.CreateDefaultMapping to define the
        category.
  }
  ecNone                    = 0;
  
  ecFirstLazarus            = 1001;

  // search
  ecFind                    = ecFirstLazarus + 1;
  ecFindAgain               = ecFirstLazarus + 2;
  ecFindNext                = ecFindAgain;
  ecFindPrevious            = ecFirstLazarus + 3;
  ecReplace                 = ecFirstLazarus + 4;
  ecIncrementalFind         = ecFirstLazarus + 5;
  ecFindProcedureDefinition = ecFirstLazarus + 6;
  ecFindProcedureMethod     = ecFirstLazarus + 7;
  ecGotoLineNumber          = ecFirstLazarus + 8;
  ecFindNextWordOccurrence  = ecFirstLazarus + 9;
  ecFindPrevWordOccurrence  = ecFirstLazarus + 10;
  ecFindInFiles             = ecFirstLazarus + 11;
  ecJumpBack                = ecFirstLazarus + 12;
  ecJumpForward             = ecFirstLazarus + 13;
  ecAddJumpPoint            = ecFirstLazarus + 14;
  ecViewJumpHistory         = ecFirstLazarus + 15;
  ecJumpToNextError         = ecFirstLazarus + 16;
  ecJumpToPrevError         = ecFirstLazarus + 17;
  ecProcedureList           = ecFirstLazarus + 18;

  // search code
  ecFindDeclaration         = ecFirstLazarus + 20;
  ecFindBlockOtherEnd       = ecFirstLazarus + 21;
  ecFindBlockStart          = ecFirstLazarus + 22;
  ecOpenFileAtCursor        = ecFirstLazarus + 23;
  ecGotoIncludeDirective    = ecFirstLazarus + 24;

  // edit selection
  ecSelectionUpperCase      = ecFirstLazarus + 50;
  ecSelectionLowerCase      = ecFirstLazarus + 51;
  ecSelectionSwapCase       = ecFirstLazarus + 52;
  ecSelectionTabs2Spaces    = ecFirstLazarus + 53;
  ecSelectionEnclose        = ecFirstLazarus + 54;
  ecSelectionComment        = ecFirstLazarus + 55;
  ecSelectionUncomment      = ecFirstLazarus + 56;
  ecSelectionSort           = ecFirstLazarus + 57;
  ecSelectionBreakLines     = ecFirstLazarus + 58;
  ecSelectToBrace           = ecFirstLazarus + 59;
  ecSelectCodeBlock         = ecFirstLazarus + 60;
  ecSelectWord              = ecFirstLazarus + 61;
  ecSelectLine              = ecFirstLazarus + 62;
  ecSelectParagraph         = ecFirstLazarus + 63;
  ecSelectionEncloseIFDEF   = ecFirstLazarus + 64;
  ecToggleComment           = ecFirstLazarus + 65;

  // insert text
  ecInsertCharacter         = ecFirstLazarus + 80;
  ecInsertGPLNotice         = ecFirstLazarus + 81;
  ecInsertLGPLNotice        = ecFirstLazarus + 82;
  ecInsertUserName          = ecFirstLazarus + 83;
  ecInsertDateTime          = ecFirstLazarus + 84;
  ecInsertChangeLogEntry    = ecFirstLazarus + 85;
  ecInsertCVSAuthor         = ecFirstLazarus + 86;
  ecInsertCVSDate           = ecFirstLazarus + 87;
  ecInsertCVSHeader         = ecFirstLazarus + 88;
  ecInsertCVSID             = ecFirstLazarus + 89;
  ecInsertCVSLog            = ecFirstLazarus + 90;
  ecInsertCVSName           = ecFirstLazarus + 91;
  ecInsertCVSRevision       = ecFirstLazarus + 92;
  ecInsertCVSSource         = ecFirstLazarus + 93;
  ecInsertModifiedLGPLNotice= ecFirstLazarus + 94;
  ecInsertGUID              = ecFirstLazarus + 95;
  ecInsertFilename          = ecFirstLazarus + 96;

  // source tools
  ecWordCompletion          = ecFirstLazarus + 100;
  ecCompleteCode            = ecFirstLazarus + 101;
  ecIdentCompletion         = ecFirstLazarus + 102;
  ecSyntaxCheck             = ecFirstLazarus + 103;
  ecGuessUnclosedBlock      = ecFirstLazarus + 104;
  ecGuessMisplacedIFDEF     = ecFirstLazarus + 105;
  ecConvertDFM2LFM          = ecFirstLazarus + 106;
  ecCheckLFM                = ecFirstLazarus + 107;
  ecConvertDelphiUnit       = ecFirstLazarus + 108;
  ecConvertDelphiProject    = ecFirstLazarus + 109;
  ecConvertDelphiPackage    = ecFirstLazarus + 110;
  ecConvertEncoding         = ecFirstLazarus + 111;
  ecMakeResourceString      = ecFirstLazarus + 112;
  ecDiff                    = ecFirstLazarus + 113;
  ecExtractProc             = ecFirstLazarus + 114;
  ecFindIdentifierRefs      = ecFirstLazarus + 115;
  ecRenameIdentifier        = ecFirstLazarus + 116;
  ecInvertAssignment        = ecFirstLazarus + 117;
  ecShowCodeContext         = ecFirstLazarus + 118;
  ecShowAbstractMethods     = ecFirstLazarus + 119;
  ecRemoveEmptyMethods      = ecFirstLazarus + 120;
  ecRemoveUnusedUnits       = ecFirstLazarus + 121;
  ecUseUnit                 = ecFirstLazarus + 122;
  ecFindOverloads           = ecFirstLazarus + 123;

  // file menu
  ecNew                     = ecFirstLazarus + 201;
  ecNewUnit                 = ecFirstLazarus + 202;
  ecNewForm                 = ecFirstLazarus + 203;
  ecOpen                    = ecFirstLazarus + 205;
  ecRevert                  = ecFirstLazarus + 206;
  ecSave                    = ecFirstLazarus + 207;
  ecSaveAs                  = ecFirstLazarus + 208;
  ecSaveAll                 = ecFirstLazarus + 209;
  ecClose                   = ecFirstLazarus + 210;
  ecCloseAll                = ecFirstLazarus + 211;
  ecCleanDirectory          = ecFirstLazarus + 212;
  ecRestart                 = ecFirstLazarus + 213;
  ecQuit                    = ecFirstLazarus + 214;

  // IDE navigation
  ecToggleFormUnit          = ecFirstLazarus + 301;
  ecToggleObjectInsp        = ecFirstLazarus + 302;
  ecToggleSourceEditor      = ecFirstLazarus + 303;
  ecToggleCodeExpl          = ecFirstLazarus + 304;
  ecToggleFPDocEditor       = ecFirstLazarus + 305;
  ecToggleMessages          = ecFirstLazarus + 306;
  ecToggleWatches           = ecFirstLazarus + 307;
  ecToggleBreakPoints       = ecFirstLazarus + 308;
  ecToggleDebuggerOut       = ecFirstLazarus + 309;
  ecViewUnitDependencies    = ecFirstLazarus + 312;
  ecViewUnitInfo            = ecFirstLazarus + 313;
  ecToggleLocals            = ecFirstLazarus + 314;
  ecToggleCallStack         = ecFirstLazarus + 315;
  ecToggleSearchResults     = ecFirstLazarus + 316;
  ecViewAnchorEditor        = ecFirstLazarus + 317;
  ecViewTabOrder            = ecFirstLazarus + 318;
  ecToggleCodeBrowser       = ecFirstLazarus + 319;
  ecToggleCompPalette       = ecFirstLazarus + 320;
  ecToggleIDESpeedBtns      = ecFirstLazarus + 321;
  ecViewComponents          = ecFirstLazarus + 322;
  ecToggleRestrictionBrowser= ecFirstLazarus + 323;
  ecViewTodoList            = ecFirstLazarus + 324;
  ecToggleRegisters         = ecFirstLazarus + 325;
  ecToggleAssembler         = ecFirstLazarus + 326;
  ecToggleDebugEvents       = ecFirstLazarus + 327;
  ecViewPseudoTerminal      = ecFirstLazarus + 328;
  ecViewThreads             = ecFirstLazarus + 329;
  ecViewHistory             = ecFirstLazarus + 460;

  // sourcenotebook commands
  ecNextEditor              = ecFirstLazarus + 330;
  ecPrevEditor              = ecFirstLazarus + 331;
  ecMoveEditorLeft          = ecFirstLazarus + 332;
  ecMoveEditorRight         = ecFirstLazarus + 333;
  ecToggleBreakPoint        = ecFirstLazarus + 334;
  ecRemoveBreakPoint        = ecFirstLazarus + 335;
  ecMoveEditorLeftmost      = ecFirstLazarus + 336;
  ecMoveEditorRightmost     = ecFirstLazarus + 337;

  ecNextSharedEditor        = ecFirstLazarus + 338;
  ecPrevSharedEditor        = ecFirstLazarus + 339;

  ecNextWindow              = ecFirstLazarus + 340;
  ecPrevWindow              = ecFirstLazarus + 341;
  ecMoveEditorNextWindow    = ecFirstLazarus + 342;
  ecMoveEditorPrevWindow    = ecFirstLazarus + 343;
  ecMoveEditorNewWindow     = ecFirstLazarus + 344;
  ecCopyEditorNextWindow    = ecFirstLazarus + 345;
  ecCopyEditorPrevWindow    = ecFirstLazarus + 346;
  ecCopyEditorNewWindow     = ecFirstLazarus + 347;

  ecGotoEditor1             = ecFirstLazarus + 350;
  ecGotoEditor2             = ecGotoEditor1 + 1;
  ecGotoEditor3             = ecGotoEditor2 + 1;
  ecGotoEditor4             = ecGotoEditor3 + 1;
  ecGotoEditor5             = ecGotoEditor4 + 1;
  ecGotoEditor6             = ecGotoEditor5 + 1;
  ecGotoEditor7             = ecGotoEditor6 + 1;
  ecGotoEditor8             = ecGotoEditor7 + 1;
  ecGotoEditor9             = ecGotoEditor8 + 1;
  ecGotoEditor0             = ecGotoEditor9 + 1;

  ecLockEditor              = ecFirstLazarus + 370;

  // marker
  ecSetFreeBookmark         = ecFirstLazarus + 381;
  ecPrevBookmark            = ecFirstLazarus + 382;
  ecNextBookmark            = ecFirstLazarus + 383;

  // run menu
  ecCompile                 = ecFirstLazarus + 400;
  ecBuild                   = ecFirstLazarus + 401;
  ecQuickCompile            = ecFirstLazarus + 402;
  ecCleanUpCompiled         = ecFirstLazarus + 403;
  ecAbortBuild              = ecFirstLazarus + 404;
  ecRun                     = ecFirstLazarus + 410;
  ecPause                   = ecFirstLazarus + 411;
  ecStepInto                = ecFirstLazarus + 412;
  ecStepOver                = ecFirstLazarus + 413;
  ecRunToCursor             = ecFirstLazarus + 414;
  ecStopProgram             = ecFirstLazarus + 415;
  ecResetDebugger           = ecFirstLazarus + 416;
  ecRunParameters           = ecFirstLazarus + 417;
  ecBuildFile               = ecFirstLazarus + 431;
  ecRunFile                 = ecFirstLazarus + 432;
  ecConfigBuildFile         = ecFirstLazarus + 433;
  ecInspect                 = ecFirstLazarus + 440;
  ecEvaluate                = ecFirstLazarus + 441;
  ecAddWatch                = ecFirstLazarus + 442;
  ecShowExecutionPoint      = ecFirstLazarus + 443;
  ecStepOut                 = ecFirstLazarus + 444;
  ecStepIntoInstr           = ecFirstLazarus + 445;
  ecStepOverInstr           = ecFirstLazarus + 446;
  ecStepIntoContext         = ecFirstLazarus + 447;
  ecStepOverContext         = ecFirstLazarus + 448;
  ecAddBpSource             = ecFirstLazarus + 449;
  ecAddBpAddress            = ecFirstLazarus + 450;
  ecAddBpDataWatch          = ecFirstLazarus + 451;

  // 460++ : used for ecViewHistory (debugger)

  // project menu
  ecNewProject              = ecFirstLazarus + 500;
  ecNewProjectFromFile      = ecFirstLazarus + 501;
  ecOpenProject             = ecFirstLazarus + 502;
  ecCloseProject            = ecFirstLazarus + 503;
  ecSaveProject             = ecFirstLazarus + 504;
  ecSaveProjectAs           = ecFirstLazarus + 505;
  ecPublishProject          = ecFirstLazarus + 506;
  ecProjectInspector        = ecFirstLazarus + 507;
  ecAddCurUnitToProj        = ecFirstLazarus + 508;
  ecRemoveFromProj          = ecFirstLazarus + 509;
  ecViewProjectUnits        = ecFirstLazarus + 510;
  ecViewProjectForms        = ecFirstLazarus + 511;
  ecViewProjectSource       = ecFirstLazarus + 512;
  ecProjectOptions          = ecFirstLazarus + 513;

  // package menu
  ecOpenPackage             = ecFirstLazarus + 600;
  ecOpenPackageFile         = ecFirstLazarus + 601;
  ecOpenPackageOfCurUnit    = ecFirstLazarus + 602;
  ecAddCurFileToPkg         = ecFirstLazarus + 603;
//  ecAddCurUnitToPkg         = ecAddCurFileToPkg deprecated;
  ecPackageGraph            = ecFirstLazarus + 604;
  ecEditInstallPkgs         = ecFirstLazarus + 605;
  ecConfigCustomComps       = ecFirstLazarus + 606;
  ecNewPackage              = ecFirstLazarus + 607;

  // custom tools menu
  ecExtToolFirst            = ecFirstLazarus + 700;
  ecExtToolLast             = ecFirstLazarus + 799;

  // tools menu
  ecExtToolSettings         = ecFirstLazarus + 820;
  ecEnvironmentOptions      = ecFirstLazarus + 821;
  ecEditCodeTemplates       = ecFirstLazarus + 822;
  ecCodeToolsDefinesEd      = ecFirstLazarus + 823;
  ecRescanFPCSrcDir         = ecFirstLazarus + 824;
  ecManageExamples          = ecFirstLazarus + 825;
  ecConfigBuildLazarus      = ecFirstLazarus + 830;
  ecBuildLazarus            = ecFirstLazarus + 831;
  ecBuildAdvancedLazarus    = ecFirstLazarus + 832;

  // help menu
  ecAboutLazarus            = ecFirstLazarus + 900;
  ecOnlineHelp              = ecFirstLazarus + 901;
  ecContextHelp             = ecFirstLazarus + 903;
  ecEditContextHelp         = ecFirstLazarus + 904;
  ecReportingBug            = ecFirstLazarus + 905;
  ecFocusHint               = ecFirstLazarus + 906;

  // designer
  ecDesignerCopy            = ecFirstLazarus + 1000;
  ecDesignerCut             = ecFirstLazarus + 1001;
  ecDesignerPaste           = ecFirstLazarus + 1002;
  ecDesignerSelectParent    = ecFirstLazarus + 1003;
  ecDesignerMoveToFront     = ecFirstLazarus + 1004;
  ecDesignerMoveToBack      = ecFirstLazarus + 1005;
  ecDesignerForwardOne      = ecFirstLazarus + 1006;
  ecDesignerBackOne         = ecFirstLazarus + 1007;

  // custom commands
  ecLazarusLast             = ecFirstLazarus + 2000;



type
  TIDECommand = class;
  TIDECommandCategory = class;

  TNotifyProcedure = procedure(Sender: TObject);

  { TIDECommandScope
    A TIDECommandScope defines a set of IDE windows that will share the same
    IDE commands. An IDE command can be valid in several scopes at the same
    time. }
    
  { TIDECommandScope }

  TIDECommandScope = class(TPersistent)
  private
    FName: string;
    FIDEWindowClasses: TFPList;// list of TCustomFormClass
    FCategories: TFPList;
    function GetCategories(Index: integer): TIDECommandCategory;
    function GetIDEWindowClasses(Index: integer): TCustomFormClass;
  public
    constructor Create(const TheName: string);
    destructor Destroy; override;
    procedure AddWindowClass(AWindowClass: TCustomFormClass);
    procedure RemoveWindowClass(AWindowClass: TCustomFormClass);
    function IDEWindowClassCount: integer;
    function CategoryCount: integer;
    function HasIDEWindowClass(AWindowClass: TCustomFormClass): boolean;
    function Intersects(AScope: TIDECommandScope): boolean;
    procedure WriteDebugReport;
  public
    property Name: string read FName;
    property IDEWindowClasses[Index: integer]: TCustomFormClass read GetIDEWindowClasses;
    property Categories[Index: integer]: TIDECommandCategory read GetCategories;
  end;

  { TIDECommandScopes }

  TIDECommandScopes = class(TPersistent)
  private
    FItems: TFPList;
    function GetItems(Index: integer): TIDECommandScope;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(NewItem: TIDECommandScope);
    function IndexOf(AnItem: TIDECommandScope): Integer;
    function IndexByName(const AName: string): Integer;
    function FindByName(const AName: string): TIDECommandScope;
    function CreateUniqueName(const AName: string): string;
    function Count: integer;
  public
    property Items[Index: integer]: TIDECommandScope read GetItems;
  end;
  

  { TIDEShortCut }

  TIDEShortCut = record
    Key1: word;
    Shift1: TShiftState;
    Key2: word;
    Shift2: TShiftState;
  end;
  PIDEShortCut = ^TIDEShortCut;

  { TIDECommandCategory
    TIDECommandCategory is used to divide the commands in handy packets }
    
  TIDECommandCategory = class(TList)
  private
    procedure SetDescription(const AValue: string);
  protected
    FDescription: string;
    FName: string;
    FParent: TIDECommandCategory;
    FScope: TIDECommandScope;
    procedure SetScope(const AValue: TIDECommandScope);
  public
    destructor Destroy; override;
    function ScopeIntersects(AScope: TIDECommandScope): boolean;
    procedure WriteScopeDebugReport;
  public
    property Name: string read FName;
    property Description: string read FDescription write SetDescription;
    property Parent: TIDECommandCategory read FParent;
    procedure Delete(Index: Integer); virtual;
    property Scope: TIDECommandScope read FScope write SetScope;
  end;
  
  
  { TIDECommand }
  { class for storing the keys of a single command
    (shortcut-command relationship) }
  TIDECommand = class
  private
    FCategory: TIDECommandCategory;
    FCommand: word;
    FLocalizedName: string;
    FName: String;
    FOnChange: TNotifyEvent;
    FOnExecute: TNotifyEvent;
    FOnExecuteProc: TNotifyProcedure;
    FShortcutA: TIDEShortCut;
    FShortcutB: TIDEShortCut;
  protected
    function GetLocalizedName: string; virtual;
    procedure SetLocalizedName(const AValue: string); virtual;
    procedure SetCategory(const AValue: TIDECommandCategory); virtual;
    procedure SetShortcutA(const AValue: TIDEShortCut); virtual;
    procedure SetShortcutB(const AValue: TIDEShortCut); virtual;
    procedure Change;
  public
    function AsShortCut: TShortCut; virtual;
    constructor Create(TheCategory: TIDECommandCategory;
              const TheName, TheLocalizedName: String; TheCommand: word;
              const TheShortcutA, TheShortcutB: TIDEShortCut;
              const ExecuteMethod: TNotifyEvent; const ExecuteProc: TNotifyProcedure);
    constructor Create(TheCategory: TIDECommandCategory;
              const TheName, TheLocalizedName: String; TheCommand: word);
    constructor Create(ACommand: TIDECommand; ACategory: TIDECommandCategory);
    destructor Destroy; override;
    procedure Assign(ACommand: TIDECommand);
    function IsEqual(ACommand: TIDECommand): boolean;
  public
    DefaultShortcutA: TIDEShortCut;
    DefaultShortcutB: TIDEShortCut;
    procedure ClearShortcutA;
    procedure ClearShortcutB;
    function GetCategoryAndName: string;
    function Execute(Sender: TObject): boolean;
  public
    property Name: String read FName;
    property Command: word read FCommand;// see the ecXXX constants above
    property LocalizedName: string read GetLocalizedName write SetLocalizedName;
    property Category: TIDECommandCategory read FCategory write SetCategory;
    property ShortcutA: TIDEShortCut read FShortcutA write SetShortcutA;
    property ShortcutB: TIDEShortCut read FShortcutB write SetShortcutB;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnExecuteProc: TNotifyProcedure read FOnExecuteProc write FOnExecuteProc;
  end;


  { TIDECommands }

  TIDECommands = class
  protected
    function GetCategory(Index: integer): TIDECommandCategory; virtual; abstract;
  public
    function FindIDECommand(ACommand: word): TIDECommand; virtual; abstract;
    function CreateCategory(Parent: TIDECommandCategory;
                            const Name, Description: string;
                            Scope: TIDECommandScope = nil): TIDECommandCategory; virtual; abstract;
    function CreateCommand(Category: TIDECommandCategory;
                           const Name, Description: string;
                           const TheShortcutA, TheShortcutB: TIDEShortCut;
                           const OnExecuteMethod: TNotifyEvent = nil;
                           const OnExecuteProc: TNotifyProcedure = nil
                           ): TIDECommand; virtual; abstract;
    function FindCategoryByName(const CategoryName: string): TIDECommandCategory; virtual; abstract;
    function FindCommandByName(const CommandName: string): TIDECommand; virtual; abstract;
    function CategoryCount: integer; virtual; abstract;
  public
    property Categories[Index: integer]: TIDECommandCategory read GetCategory;
  end;

const
  CleanIDEShortCut: TIDEShortCut =
    (Key1: VK_UNKNOWN; Shift1: []; Key2: VK_UNKNOWN; Shift2: []);

function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word = VK_UNKNOWN; Shift2: TShiftState = []): TIDEShortCut;


type
  TExecuteIDEShortCut =
    procedure(Sender: TObject; var Key: word; Shift: TShiftState;
              IDEWindowClass: TCustomFormClass) of object;
  TExecuteIDECommand = function(Sender: TObject; Command: word): boolean of object;

var
  OnExecuteIDEShortCut: TExecuteIDEShortCut;
  OnExecuteIDECommand: TExecuteIDECommand;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
  IDEWindowClass: TCustomFormClass);
procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState);
function ExecuteIDECommand(Sender: TObject; Command: word): boolean;

var
  // will be set by the IDE
  IDECommandList: TIDECommands;
  IDECommandScopes: TIDECommandScopes = nil;
var
  IDECmdScopeSrcEdit: TIDECommandScope;
  IDECmdScopeSrcEditOnly: TIDECommandScope;
  IDECmdScopeSrcEditOnlyTmplEdit: TIDECommandScope;
  IDECmdScopeSrcEditOnlyTmplEditOff: TIDECommandScope;
  IDECmdScopeSrcEditOnlySyncroEditSel: TIDECommandScope;
  IDECmdScopeSrcEditOnlySyncroEdit: TIDECommandScope;
  IDECmdScopeSrcEditOnlySyncroEditOff: TIDECommandScope;
  IDECmdScopeDesignerOnly: TIDECommandScope;
  IDECmdScopeObjectInspectorOnly: TIDECommandScope;

const
  CommandCategoryToolMenuName = 'ToolMenu';
  CommandCategoryCustomName = 'Custom';
  CommandCategoryTextEditingName = 'text editing commands';
  CommandCategoryViewName = 'ViewMenu';
  CommandCategoryCodeTools = 'CodeTools';

// register a new IDE command category (i.e. set of commands)
function RegisterIDECommandCategory(Parent: TIDECommandCategory;
                          const Name, Description: string): TIDECommandCategory;

// register a new IDE command (i.e. a shortcut, IDE function)
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const OnExecuteMethod: TNotifyEvent = nil;
  const OnExecuteProc: TNotifyProcedure = nil): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string; Key1: word; Shift1: TShiftState;
  const OnExecuteMethod: TNotifyEvent = nil;
  const OnExecuteProc: TNotifyProcedure = nil): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string; const ShortCut1: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent = nil;
  const OnExecuteProc: TNotifyProcedure = nil): TIDECommand;
function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const ShortCut1, ShortCut2: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent = nil;
  const OnExecuteProc: TNotifyProcedure = nil): TIDECommand;

// register a new IDE command scope (i.e. a set of windows)
function RegisterIDECommandScope(const Name: string): TIDECommandScope;

procedure CreateStandardIDECommandScopes;


function CompareIDEShortCuts(Data1, Data2: Pointer): integer;
function CompareIDEShortCutKey1s(Data1, Data2: Pointer): integer;

function IdentToIDECommand(const Ident: string; var Cmd: longint): boolean;
function IDECommandToIdent(Cmd: longint; var Ident: string): boolean;

implementation

function IDEShortCut(Key1: word; Shift1: TShiftState;
  Key2: word; Shift2: TShiftState): TIDEShortCut;
begin
  Result.Key1:=Key1;
  Result.Shift1:=Shift1;
  Result.Key2:=Key2;
  Result.Shift2:=Shift2;
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word; Shift: TShiftState;
  IDEWindowClass: TCustomFormClass);
begin
  if (OnExecuteIDECommand<>nil) and (Key<>VK_UNKNOWN) then
    OnExecuteIDEShortCut(Sender,Key,Shift,IDEWindowClass);
end;

procedure ExecuteIDEShortCut(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  OnExecuteIDEShortCut(Sender,Key,Shift,nil);
end;

function ExecuteIDECommand(Sender: TObject; Command: word): boolean;
begin
  if (OnExecuteIDECommand<>nil) and (Command<>0) then
    Result:=OnExecuteIDECommand(Sender,Command)
  else
    Result:=false;
end;

procedure CreateStandardIDECommandScopes;
begin
  IDECommandScopes:=TIDECommandScopes.Create;
  IDECmdScopeSrcEdit:=RegisterIDECommandScope('SourceEditor');
  IDECmdScopeSrcEditOnly:=RegisterIDECommandScope('SourceEditorOnly');
  IDECmdScopeSrcEditOnlyTmplEdit:=RegisterIDECommandScope('SourceEditorOnlyTemplateEdit');
  IDECmdScopeSrcEditOnlyTmplEditOff:=RegisterIDECommandScope('SourceEditorOnlyTemplateEditOff');
  IDECmdScopeSrcEditOnlySyncroEditSel:=RegisterIDECommandScope('SourceEditorOnlySyncroEditSel');
  IDECmdScopeSrcEditOnlySyncroEdit:=RegisterIDECommandScope('SourceEditorOnlySyncroEdit');
  IDECmdScopeSrcEditOnlySyncroEditOff:=RegisterIDECommandScope('SourceEditorOnlySyncroEdit');
  IDECmdScopeDesignerOnly:=RegisterIDECommandScope('DesignerOnly');
  IDECmdScopeObjectInspectorOnly:=RegisterIDECommandScope('ObjectInspectorOnly');
end;

type
// in fpc 2.3.1 TShiftState is declared with {$packset 1}
{$IF sizeof(TShiftState)=2}
  TShiftStateInt = word;
{$ELSE}
  TShiftStateInt = integer;
{$ENDIF}

function CompareIDEShortCuts(Data1, Data2: Pointer): integer;
var
  ShortCut1: PIDEShortCut absolute Data1;
  ShortCut2: PIDEShortCut absolute Data2;
begin
  if ShortCut1^.Key1>ShortCut2^.Key1 then
    Result:=1
  else if ShortCut1^.Key1<ShortCut2^.Key1 then
    Result:=-1
  else if TShiftStateInt(ShortCut1^.Shift1)>TShiftStateInt(ShortCut2^.Shift1) then
    Result:=1
  else if TShiftStateInt(ShortCut1^.Shift1)<TShiftStateInt(ShortCut2^.Shift1) then
    Result:=-1
  else if ShortCut1^.Key2>ShortCut2^.Key2 then
    Result:=1
  else if ShortCut1^.Key2<ShortCut2^.Key2 then
    Result:=-1
  else if TShiftStateInt(ShortCut1^.Shift2)>TShiftStateInt(ShortCut2^.Shift2) then
    Result:=1
  else if TShiftStateInt(ShortCut1^.Shift2)<TShiftStateInt(ShortCut2^.Shift2) then
    Result:=-1
  else
    Result:=0;
end;

function CompareIDEShortCutKey1s(Data1, Data2: Pointer): integer;
var
  ShortCut1: PIDEShortCut;
  ShortCut2: PIDEShortCut;
begin
  ShortCut1:=PIDEShortCut(Data1);
  ShortCut2:=PIDEShortCut(Data2);
  if ShortCut1^.Key1>ShortCut2^.Key1 then
    Result:=1
  else if ShortCut1^.Key1<ShortCut2^.Key1 then
    Result:=-1
  else if TShiftStateInt(ShortCut1^.Shift1)>TShiftStateInt(ShortCut2^.Shift1) then
    Result:=1
  else if TShiftStateInt(ShortCut1^.Shift1)<TShiftStateInt(ShortCut2^.Shift1) then
    Result:=-1
  else
    Result:=0;
end;

function RegisterIDECommandCategory(Parent: TIDECommandCategory;
  const Name, Description: string): TIDECommandCategory;
begin
  Result:=IDECommandList.CreateCategory(Parent,Name,Description);
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const OnExecuteMethod: TNotifyEvent = nil;
  const OnExecuteProc: TNotifyProcedure = nil): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,IDEShortCut(VK_UNKNOWN,[]),
                             OnExecuteMethod,OnExecuteProc);
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  Key1: word; Shift1: TShiftState;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,IDEShortCut(Key1,Shift1),
                             OnExecuteMethod,OnExecuteProc);
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;  const ShortCut1: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TIDECommand;
begin
  Result:=RegisterIDECommand(Category,Name,Description,
                             ShortCut1,IDEShortCut(VK_UNKNOWN,[]),
                             OnExecuteMethod,OnExecuteProc);
end;

function RegisterIDECommand(Category: TIDECommandCategory;
  const Name, Description: string;
  const ShortCut1, ShortCut2: TIDEShortCut;
  const OnExecuteMethod: TNotifyEvent;
  const OnExecuteProc: TNotifyProcedure): TIDECommand;
begin
  Result:=IDECommandList.CreateCommand(Category,Name,Description,
                                       ShortCut1,ShortCut2,OnExecuteMethod,
                                       OnExecuteProc);
end;

function RegisterIDECommandScope(const Name: string): TIDECommandScope;
begin
  Result:=TIDECommandScope.Create(Name);
  IDECommandScopes.Add(Result);
end;

{ TIDECommand }

procedure TIDECommand.SetShortcutA(const AValue: TIDEShortCut);
begin
  if CompareIDEShortCuts(@FShortcutA,@AValue)=0 then exit;
  FShortcutA:=AValue;
  //DebugLn('TIDECommand.SetShortcutA ',dbgs(Assigned(OnChange)),' ',Name);
  Change;
end;

procedure TIDECommand.SetShortcutB(const AValue: TIDEShortCut);
begin
  if CompareIDEShortCuts(@FShortcutB,@AValue)=0 then exit;
  FShortcutB:=AValue;
  //DebugLn('TIDECommand.SetShortcutB ',dbgs(Assigned(OnChange)),' ',Name);
  Change;
end;

procedure TIDECommand.Change;
begin
  if Assigned(OnChange) then OnChange(Self);
end;

function TIDECommand.GetLocalizedName: string;
begin
  if FLocalizedName<>'' then
    Result:=FLocalizedName
  else
    Result:=Name;
end;

procedure TIDECommand.SetLocalizedName(const AValue: string);
begin
  if FLocalizedName=AValue then exit;
  FLocalizedName:=AValue;
  //DebugLn('TIDECommand.SetLocalizedName ',dbgs(Assigned(OnChange)),' ',Name);
  Change;
end;

procedure TIDECommand.SetCategory(const AValue: TIDECommandCategory);
begin
  if FCategory=AValue then exit;
  // unbind
  if Category<>nil then
    Category.Remove(Self);
  // bind
  fCategory:=AValue;
  if Category<>nil then
    Category.Add(Self);
  //DebugLn('TIDECommand.SetCategory ',dbgs(Assigned(OnChange)),' ',Name);
  Change;
end;

function TIDECommand.AsShortCut: TShortCut;
var
  CurKey: TIDEShortCut;
begin
  if (ShortcutA.Key1<>VK_UNKNOWN) and (ShortcutA.Key2=VK_UNKNOWN) then
    CurKey:=ShortcutA
  else if (ShortcutB.Key1<>VK_UNKNOWN) and (ShortcutB.Key2=VK_UNKNOWN) then
    CurKey:=ShortcutB
  else
    CurKey:=CleanIDEShortCut;
  Result:=CurKey.Key1;
  if ssCtrl in CurKey.Shift1 then
    Result:=Result+scCtrl;
  if ssShift in CurKey.Shift1 then
    Result:=Result+scShift;
  if ssAlt in CurKey.Shift1 then
    Result:=Result+scAlt;
end;

constructor TIDECommand.Create(TheCategory: TIDECommandCategory;
  const TheName, TheLocalizedName: String; TheCommand: word;
  const TheShortcutA, TheShortcutB: TIDEShortCut;
  const ExecuteMethod: TNotifyEvent;
  const ExecuteProc: TNotifyProcedure);
begin
  fCommand:=TheCommand;
  fName:=TheName;
  FLocalizedName:=TheLocalizedName;
  fShortcutA:=TheShortcutA;
  fShortcutB:=TheShortcutB;
  DefaultShortcutA:=ShortcutA;
  DefaultShortcutB:=ShortcutB;
  Category:=TheCategory;
  FOnExecute:=ExecuteMethod;
  FOnExecuteProc:=ExecuteProc;
  //DebugLn('TIDECommand.Create Name=',Name,' ',ShortCutToText(AsShortCut),' ',dbgs(Pointer(Self)));
end;

constructor TIDECommand.Create(TheCategory: TIDECommandCategory;
  const TheName, TheLocalizedName: String; TheCommand: word);
begin
  Create(TheCategory, TheName, TheLocalizedName, TheCommand,
         CleanIDEShortCut, CleanIDEShortCut, Nil, Nil);
end;

constructor TIDECommand.Create(ACommand: TIDECommand; ACategory: TIDECommandCategory);
begin
  fCommand:=ACommand.Command;
  fName:=ACommand.Name;
  FLocalizedName:=ACommand.LocalizedName;
  fShortcutA:=ACommand.ShortcutA;
  fShortcutB:=ACommand.ShortcutB;
  DefaultShortcutA:=ACommand.ShortcutA;
  DefaultShortcutB:=ACommand.ShortcutB;
  Category:=ACategory;
end;

destructor TIDECommand.Destroy;
begin
  inherited Destroy;
end;

procedure TIDECommand.Assign(ACommand: TIDECommand);
begin
  if IsEqual(ACommand) then exit;
  //DebugLn('TIDECommand.Assign ',dbgs(Assigned(OnChange)),' ',Name,' ');
  FShortcutA:=ACommand.FShortcutA;
  FShortcutB:=ACommand.FShortcutB;
  Change;
end;

function TIDECommand.IsEqual(ACommand: TIDECommand): boolean;
begin
  Result:=(CompareIDEShortCuts(@FShortcutA,@ACommand.FShortcutA)=0)
          and (CompareIDEShortCuts(@FShortcutB,@ACommand.FShortcutB)=0);
end;

procedure TIDECommand.ClearShortcutA;
begin
  ShortcutA:=CleanIDEShortCut;
end;

procedure TIDECommand.ClearShortcutB;
begin
  ShortcutB:=CleanIDEShortCut;
end;

function TIDECommand.GetCategoryAndName: string;
begin
  Result:='"'+GetLocalizedName+'"';
  if Category<>nil then
    Result:=Result+' in "'+Category.Description+'"';
end;

function TIDECommand.Execute(Sender: TObject): boolean;
begin
  Result:=false;
  if Assigned(OnExecute) then begin
    Result:=true;
    OnExecute(Sender);
  end;
  if Assigned(OnExecuteProc) then begin
    Result:=true;
    OnExecuteProc(Sender);
  end;
end;

{ TIDECommandScopes }

function TIDECommandScopes.GetItems(Index: integer): TIDECommandScope;
begin
  Result:=TIDECommandScope(FItems[Index]);
end;

constructor TIDECommandScopes.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TIDECommandScopes.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TIDECommandScopes.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TIDECommandScopes.Add(NewItem: TIDECommandScope);
begin
  NewItem.fName:=CreateUniqueName(NewItem.Name);
  FItems.Add(NewItem);
end;

function TIDECommandScopes.IndexOf(AnItem: TIDECommandScope): Integer;
begin
  Result:=FItems.IndexOf(AnItem);
end;

function TIDECommandScopes.IndexByName(const AName: string): Integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(AName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TIDECommandScopes.FindByName(const AName: string): TIDECommandScope;
var
  i: LongInt;
begin
  i:=IndexByName(AName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDECommandScopes.CreateUniqueName(const AName: string): string;
begin
  Result:=AName;
  if IndexByName(Result)<0 then exit;
  Result:=CreateFirstIdentifier(Result);
  while IndexByName(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

function TIDECommandScopes.Count: integer;
begin
  Result:=FItems.Count;
end;

{ TIDECommandCategory }

procedure TIDECommandCategory.SetDescription(const AValue: string);
begin
  if FDescription=AValue then exit;
  FDescription:=AValue;
end;

procedure TIDECommandCategory.SetScope(const AValue: TIDECommandScope);
begin
  if FScope=AValue then exit;
  if FScope<>nil then
    FScope.FCategories.Remove(Self);
  FScope:=AValue;
  if FScope<>nil then
    FScope.FCategories.Add(Self);
end;

destructor TIDECommandCategory.Destroy;
begin
  Scope:=nil;
  inherited Destroy;
end;

function TIDECommandCategory.ScopeIntersects(AScope: TIDECommandScope
  ): boolean;
begin
  if (Scope=nil) or (AScope=nil) then
    Result:=true
  else
    Result:=Scope.Intersects(AScope);
end;

procedure TIDECommandCategory.WriteScopeDebugReport;
begin
  debugln('TIDECommandCategory.WriteScopeDebugReport ',Name,'=',Description);
  if Scope<>nil then
    Scope.WriteDebugReport
  else
    debugln('  Scope=nil');
end;

procedure TIDECommandCategory.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

{ TIDECommandScope }

function TIDECommandScope.GetCategories(Index: integer): TIDECommandCategory;
begin
  Result:=TIDECommandCategory(FCategories[Index]);
end;

function TIDECommandScope.GetIDEWindowClasses(Index: integer): TCustomFormClass;
begin
  Result:=TCustomFormClass(FIDEWindowClasses[Index]);
end;

constructor TIDECommandScope.Create(const TheName: string);
begin
  FName:=TheName;
  FIDEWindowClasses:=TFPList.Create;
  FCategories:=TFPList.Create;
end;

destructor TIDECommandScope.Destroy;
var
  i: Integer;
begin
  for i:=FCategories.Count-1 downto 0 do
    Categories[i].Scope:=nil;
  FreeAndNil(FIDEWindowClasses);
  FreeAndNil(FCategories);
  inherited Destroy;
end;

procedure TIDECommandScope.AddWindowClass(AWindowClass: TCustomFormClass);
begin
  if FIDEWindowClasses.IndexOf(AWindowClass)>=0 then
    RaiseGDBException('TIDECommandScope.AddWindowClass');
  FIDEWindowClasses.Add(AWindowClass);
end;

procedure TIDECommandScope.RemoveWindowClass(AWindowClass: TCustomFormClass);
begin
  FIDEWindowClasses.Remove(AWindowClass);
end;

function TIDECommandScope.IDEWindowClassCount: integer;
begin
  Result:=FIDEWindowClasses.Count;
end;

function TIDECommandScope.CategoryCount: integer;
begin
  Result:=FCategories.Count;
end;

function TIDECommandScope.HasIDEWindowClass(AWindowClass: TCustomFormClass
  ): boolean;
var
  i: Integer;
begin
  if AWindowClass<>nil then begin
    for i:=0 to FIDEWindowClasses.Count-1 do begin
      if (FIDEWindowClasses[i]=nil)
      or AWindowClass.InheritsFrom(TCustomFormClass(FIDEWindowClasses[i])) then
        exit(true);
    end;
  end else begin
    if FIDEWindowClasses.IndexOf(nil)>=0 then
      exit(true);
  end;
  Result:=false;
end;

function TIDECommandScope.Intersects(AScope: TIDECommandScope): boolean;
var
  i: Integer;
  CurClass: TCustomFormClass;
begin
  if AScope=nil then
    Result:=true
  else begin
    for i:=0 to FIDEWindowClasses.Count-1 do begin
      CurClass:=TCustomFormClass(FIDEWindowClasses[i]);
      if (CurClass=nil)
      or (AScope.FIDEWindowClasses.IndexOf(FIDEWindowClasses[i])>=0) then
        exit(true);
    end;
    Result:=false;
  end;
end;

procedure TIDECommandScope.WriteDebugReport;
var
  i: Integer;
begin
  debugln('TIDECommandScope.WriteDebugReport ',Name);
  for i:=0 to FIDEWindowClasses.Count-1 do begin
    if FIDEWindowClasses[i]=nil then
      debugln('  ',dbgs(i),'/',dbgs(FIDEWindowClasses.Count),' nil')
    else
      debugln('  ',dbgs(i),'/',dbgs(FIDEWindowClasses.Count),' ',TClass(FIDEWindowClasses[i]).ClassName);
  end;
end;

function IdentToIDECommand(const Ident: string; var Cmd: longint): boolean;
var
  IDECommand: TIDECommand;
begin
  if IDECommandList=nil then exit(false);
  IDECommand := IDECommandList.FindCommandByName(Ident);
  if IDECommand=nil then exit(false);
  Result:=true;
  Cmd := IDECommand.Command;
end;

function IDECommandToIdent(Cmd: longint; var Ident: string): boolean;
var
  IDECommand: TIDECommand;
begin
  if IDECommandList=nil then exit(false);
  IDECommand := IDECommandList.FindIDECommand(Cmd);
  if IDECommand=nil then exit(false);
  Result:=true;
  Ident:=IDECommand.Name;
end;

end.

