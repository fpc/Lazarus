{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Classes, SysUtils,
  // LCL
  LCLProc, LCLType, LCLIntf, Forms, Menus,
  // LazUtils
  LazLoggerBase, LazTracer,
  // IdeIntf
  IDEImagesIntf;
  
const
  { editor commands constants. see syneditkeycmds.pp for more

   These values can change from version to version, so DO NOT save them to file!

   To add one static key do the following:
     1. Add a constant with a unique value in the list below.
     2. Update IDEEditorCommandStrs to define a name (used for configs)
     3. Add it to GetDefaultKeyForCommand to define the default keys+shiftstates
     4. Add it to EditorCommandToDescriptionString to define the description
     5. Add it to TKeyCommandRelationList.DefineCommandCategories to define the category.
  }
  ecNone                    = 0;
  
  ecFirstLazarus            = 1001;  // syneditkeycmds.ecUserFirst = 1001;

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
  ecJumpToSection           = ecFirstLazarus + 25;
  ecJumpToInterface         = ecFirstLazarus + 26;
  ecJumpToInterfaceUses     = ecFirstLazarus + 27;
  ecJumpToImplementation    = ecFirstLazarus + 28;
  ecJumpToImplementationUses= ecFirstLazarus + 29;
  ecJumpToInitialization    = ecFirstLazarus + 30;
  ecJumpToProcedureHeader   = ecFirstLazarus + 31;
  ecJumpToProcedureBegin    = ecFirstLazarus + 32;

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
  ecInsertCharacter         = ecFirstLazarus + 70;
  ecInsertGUID              = ecFirstLazarus + 71;
  ecInsertFilename          = ecFirstLazarus + 72;
  ecInsertUserName          = ecFirstLazarus + 73;
  ecInsertDateTime          = ecFirstLazarus + 74;
  ecInsertChangeLogEntry    = ecFirstLazarus + 75;
  ecInsertCVSAuthor         = ecFirstLazarus + 76;
  ecInsertCVSDate           = ecFirstLazarus + 77;
  ecInsertCVSHeader         = ecFirstLazarus + 78;
  ecInsertCVSID             = ecFirstLazarus + 79;
  ecInsertCVSLog            = ecFirstLazarus + 80;
  ecInsertCVSName           = ecFirstLazarus + 81;
  ecInsertCVSRevision       = ecFirstLazarus + 82;
  ecInsertCVSSource         = ecFirstLazarus + 83;
  ecInsertGPLNotice         = ecFirstLazarus + 84;
  ecInsertGPLNoticeTranslated = ecFirstLazarus + 85;
  ecInsertLGPLNotice        = ecFirstLazarus + 86;
  ecInsertLGPLNoticeTranslated = ecFirstLazarus + 87;
  ecInsertModifiedLGPLNotice= ecFirstLazarus + 88;
  ecInsertModifiedLGPLNoticeTranslated = ecFirstLazarus + 89;
  ecInsertMITNotice         = ecFirstLazarus + 90;
  ecInsertMITNoticeTranslated = ecFirstLazarus + 91;

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
  ecFindUsedUnitRefs        = ecFirstLazarus + 124;
  ecCompleteCodeInteractive = ecFirstLazarus + 125;

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
  ecOpenUnit                = ecFirstLazarus + 215;
  ecOpenRecent              = ecFirstLazarus + 216;
  ecCloseOtherTabs          = ecFirstLazarus + 217;
  ecCloseRightTabs          = ecFirstLazarus + 218;

  // edit menu
  ecMultiPaste              = ecFirstLazarus + 230;

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
  ecViewMacroList           = ecFirstLazarus + 461;

  // sourcenotebook commands
  ecNextEditor              = ecFirstLazarus + 330;
  ecPrevEditor              = ecFirstLazarus + 331;
  ecMoveEditorLeft          = ecFirstLazarus + 332;
  ecMoveEditorRight         = ecFirstLazarus + 333;
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
  ecPrevEditorInHistory     = ecFirstLazarus + 348;
  ecNextEditorInHistory     = ecFirstLazarus + 349;

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
  ecClearBookmarkForFile    = ecFirstLazarus + 384;
  ecClearAllBookmark        = ecFirstLazarus + 385;

  ecGotoBookmarks           = ecFirstLazarus + 386;
  ecToggleBookmarks         = ecFirstLazarus + 387;

  // Macro
  ecSynMacroRecord          = ecFirstLazarus + 390;
  ecSynMacroPlay            = ecFirstLazarus + 391;

  // run menu
  ecCompile                 = ecFirstLazarus + 400;
  ecBuild                   = ecFirstLazarus + 401;
  ecQuickCompile            = ecFirstLazarus + 402;
  ecCleanUpAndBuild         = ecFirstLazarus + 403;
  ecBuildManyModes          = ecFirstLazarus + 404;
  ecAbortBuild              = ecFirstLazarus + 405;
  ecRunWithoutDebugging     = ecFirstLazarus + 409;
  ecRun                     = ecFirstLazarus + 410;
  ecPause                   = ecFirstLazarus + 411;
  ecStepInto                = ecFirstLazarus + 412;
  ecStepOver                = ecFirstLazarus + 413;
  ecStepToCursor             = ecFirstLazarus + 414;
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
  ecAttach                  = ecFirstLazarus + 452;
  ecDetach                  = ecFirstLazarus + 453;

  // 460++ : used for ecViewHistory (debugger) / ecViewMacroList

  ecToggleBreakPoint        = ecFirstLazarus + 470;
  ecRemoveBreakPoint        = ecFirstLazarus + 471;
  ecToggleBreakPointEnabled = ecFirstLazarus + 472;


  // project menu
  ecNewProject              = ecFirstLazarus + 500;
  ecNewProjectFromFile      = ecFirstLazarus + 501;
  ecOpenProject             = ecFirstLazarus + 502;
  ecOpenRecentProject       = ecFirstLazarus + 503;
  ecCloseProject            = ecFirstLazarus + 504;
  ecSaveProject             = ecFirstLazarus + 505;
  ecSaveProjectAs           = ecFirstLazarus + 506;
  ecPublishProject          = ecFirstLazarus + 507;
  ecProjectInspector        = ecFirstLazarus + 508;
  ecAddCurUnitToProj        = ecFirstLazarus + 509;
  ecRemoveFromProj          = ecFirstLazarus + 510;
  ecViewProjectUnits        = ecFirstLazarus + 511;
  ecViewProjectForms        = ecFirstLazarus + 512;
  ecViewProjectSource       = ecFirstLazarus + 513;
  ecProjectOptions          = ecFirstLazarus + 514;
  ecProjectChangeBuildMode  = ecFirstLazarus + 515;
  ecProjectResaveFormsWithI18n = ecFirstLazarus + 516;

  // package menu
  ecOpenPackage             = ecFirstLazarus + 600;
  ecOpenPackageFile         = ecFirstLazarus + 601;
  ecOpenPackageOfCurUnit    = ecFirstLazarus + 602;
  ecOpenRecentPackage       = ecFirstLazarus + 603;
  ecAddCurFileToPkg         = ecFirstLazarus + 604;
  ecNewPkgComponent         = ecFirstLazarus + 605;
  ecPackageGraph            = ecFirstLazarus + 606;
  ecPackageLinks            = ecFirstLazarus + 607;
  ecEditInstallPkgs         = ecFirstLazarus + 608;
  ecConfigCustomComps       = ecFirstLazarus + 609;
  ecNewPackage              = ecFirstLazarus + 610;

  // custom tools menu
  ecExtToolFirst            = ecFirstLazarus + 700;
  ecExtToolLast             = ecFirstLazarus + 799;

  // tools menu
  ecEnvironmentOptions      = ecFirstLazarus + 820;
  ecRescanFPCSrcDir         = ecFirstLazarus + 821;
  ecEditCodeTemplates       = ecFirstLazarus + 822;
  ecCodeToolsDefinesEd      = ecFirstLazarus + 823;

  ecExtToolSettings         = ecFirstLazarus + 824;
  ecManageDesktops          = ecFirstLazarus + 825;
  ecManageExamples          = ecFirstLazarus + 826;
  ecConfigBuildLazarus      = ecFirstLazarus + 830;
  ecBuildLazarus            = ecFirstLazarus + 831;
  ecBuildAdvancedLazarus    = ecFirstLazarus + 832;

  // window menu
  ecManageSourceEditors     = ecFirstLazarus + 840;

  // help menu
  ecAboutLazarus            = ecFirstLazarus + 900;
  ecOnlineHelp              = ecFirstLazarus + 901;
  ecContextHelp             = ecFirstLazarus + 903;
  ecEditContextHelp         = ecFirstLazarus + 904;
  ecReportingBug            = ecFirstLazarus + 905;
  ecFocusHint               = ecFirstLazarus + 906;
  ecSmartHint               = ecFirstLazarus + 907;

  // designer
  ecDesignerCopy            = ecFirstLazarus + 1000;
  ecDesignerCut             = ecFirstLazarus + 1001;
  ecDesignerPaste           = ecFirstLazarus + 1002;
  ecDesignerSelectParent    = ecFirstLazarus + 1003;
  ecDesignerMoveToFront     = ecFirstLazarus + 1004;
  ecDesignerMoveToBack      = ecFirstLazarus + 1005;
  ecDesignerForwardOne      = ecFirstLazarus + 1006;
  ecDesignerBackOne         = ecFirstLazarus + 1007;
  ecDesignerToggleNonVisComps= ecFirstLazarus + 1008;


  (* SynEdit Plugins
     Define fixed values for the IDE. Must be mapped to plugincommands,
     when assigned to KeyMap.
     Offsets are defined in KeyMapping
     See: TKeyCommandRelationList.AssignTo
  *)
  ecFirstPlugin = ecFirstLazarus +  5000; // 6001
  ecLastPlugin = ecFirstLazarus +  6000; // 6001
  // custom commands
  ecLazarusLast             = ecLastPlugin;

  // TSynPluginTemplateEdit - In cell
  ecIdePTmplEdNextCell           = ecFirstPlugin +  0;
  ecIdePTmplEdNextCellSel        = ecFirstPlugin +  1;
  ecIdePTmplEdNextCellRotate     = ecFirstPlugin +  2;
  ecIdePTmplEdNextCellSelRotate  = ecFirstPlugin +  3;
  ecIdePTmplEdPrevCell           = ecFirstPlugin +  4;
  ecIdePTmplEdPrevCellSel        = ecFirstPlugin +  5;
  ecIdePTmplEdCellHome           = ecFirstPlugin +  6;
  ecIdePTmplEdCellEnd            = ecFirstPlugin +  7;
  ecIdePTmplEdCellSelect         = ecFirstPlugin +  8;
  ecIdePTmplEdFinish             = ecFirstPlugin +  9;
  ecIdePTmplEdEscape             = ecFirstPlugin + 10;
  ecIdePTmplEdNextFirstCell           = ecFirstPlugin + 11;
  ecIdePTmplEdNextFirstCellSel        = ecFirstPlugin + 12;
  ecIdePTmplEdNextFirstCellRotate     = ecFirstPlugin + 13;
  ecIdePTmplEdNextFirstCellSelRotate  = ecFirstPlugin + 14;
  ecIdePTmplEdPrevFirstCell           = ecFirstPlugin + 15;
  ecIdePTmplEdPrevFirstCellSel        = ecFirstPlugin + 16;

  // TSynPluginTemplateEdit - Out off Cell
  ecIdePTmplEdOutNextCell           = ecFirstPlugin +  20;
  ecIdePTmplEdOutNextCellSel        = ecFirstPlugin +  21;
  ecIdePTmplEdOutNextCellRotate     = ecFirstPlugin +  22;
  ecIdePTmplEdOutNextCellSelRotate  = ecFirstPlugin +  23;
  ecIdePTmplEdOutPrevCell           = ecFirstPlugin +  24;
  ecIdePTmplEdOutPrevCellSel        = ecFirstPlugin +  25;
  ecIdePTmplEdOutCellHome           = ecFirstPlugin +  26;
  ecIdePTmplEdOutCellEnd            = ecFirstPlugin +  27;
  ecIdePTmplEdOutCellSelect         = ecFirstPlugin +  28;
  ecIdePTmplEdOutFinish             = ecFirstPlugin +  29;
  ecIdePTmplEdOutEscape             = ecFirstPlugin +  30;
  ecIdePTmplEdOutNextFirstCell           = ecFirstPlugin + 31;
  ecIdePTmplEdOutNextFirstCellSel        = ecFirstPlugin + 32;
  ecIdePTmplEdOutNextFirstCellRotate     = ecFirstPlugin + 33;
  ecIdePTmplEdOutNextFirstCellSelRotate  = ecFirstPlugin + 34;
  ecIdePTmplEdOutPrevFirstCell           = ecFirstPlugin + 35;
  ecIdePTmplEdOutPrevFirstCellSel        = ecFirstPlugin + 36;

  // TSynPluginSyncroEdit - in celll
  ecIdePSyncroEdNextCell           = ecFirstPlugin +  50;
  ecIdePSyncroEdNextCellSel        = ecFirstPlugin +  51;
  ecIdePSyncroEdPrevCell           = ecFirstPlugin +  52;
  ecIdePSyncroEdPrevCellSel        = ecFirstPlugin +  53;
  ecIdePSyncroEdCellHome           = ecFirstPlugin +  54;
  ecIdePSyncroEdCellEnd            = ecFirstPlugin +  55;
  ecIdePSyncroEdCellSelect         = ecFirstPlugin +  56;
  ecIdePSyncroEdEscape             = ecFirstPlugin +  57;
  ecIdePSyncroEdNextFirstCell      = ecFirstPlugin +  58;
  ecIdePSyncroEdNextFirstCellSel   = ecFirstPlugin +  59;
  ecIdePSyncroEdPrevFirstCell      = ecFirstPlugin +  60;
  ecIdePSyncroEdPrevFirstCellSel   = ecFirstPlugin +  61;

  // TSynPluginSyncroEdit - Out off cell
  ecIdePSyncroEdOutNextCell           = ecFirstPlugin +  70;
  ecIdePSyncroEdOutNextCellSel        = ecFirstPlugin +  71;
  ecIdePSyncroEdOutPrevCell           = ecFirstPlugin +  72;
  ecIdePSyncroEdOutPrevCellSel        = ecFirstPlugin +  73;
  ecIdePSyncroEdOutCellHome           = ecFirstPlugin +  74;
  ecIdePSyncroEdOutCellEnd            = ecFirstPlugin +  75;
  ecIdePSyncroEdOutCellSelect         = ecFirstPlugin +  76;
  ecIdePSyncroEdOutEscape             = ecFirstPlugin +  77;
  ecIdePSyncroEdOutNextFirstCell      = ecFirstPlugin +  78;
  ecIdePSyncroEdOutNextFirstCellSel   = ecFirstPlugin +  79;
  ecIdePSyncroEdOutPrevFirstCell      = ecFirstPlugin +  80;
  ecIdePSyncroEdOutPrevFirstCellSel   = ecFirstPlugin +  81;

  // TSynPluginSyncroEdit - selecting
  ecIdePSyncroEdSelStart              = ecFirstPlugin +  90;


type
  TIDECommand = class;
  TIDECommandCategory = class;
  TIDESpecialCommand = class;
  TIDESpecialCommands = class;

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

  { TIDECommandCategory - list of TKeyCommandRelation/TIDECommand
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
    procedure DoOnUpdate;
  public
    property Name: string read FName;
    property Description: string read FDescription write SetDescription;
    property Parent: TIDECommandCategory read FParent;
    procedure Delete(Index: Integer); virtual;
    property Scope: TIDECommandScope read FScope write SetScope;
  end;
  
  
  { TIDECommand }
  { class for storing the keys of a single command (shortcut-command relationship) }
  TIDECommand = class
  private
    FCategory: TIDECommandCategory;
    FCommand: word;
    FLocalizedName: string;
    FName: String;
    FOnExecute: TNotifyEvent;
    FOnExecuteProc: TNotifyProcedure;
    FShortcutA: TIDEShortCut;
    FShortcutB: TIDEShortCut;
    FOnUpdateMethod: TNotifyEvent;
    FOnUpdateProc: TNotifyProcedure;
    FUsers: TIDESpecialCommands;

    function GetUser(Index: Integer): TIDESpecialCommand;
    function GetUserCount: Integer;
    procedure SetOnExecute(const aOnExecute: TNotifyEvent);
    procedure SetOnExecuteProc(const aOnExecuteProc: TNotifyProcedure);
    procedure SetEnabled(const AEnabled: Boolean);
    procedure SetCaption(const ACaption: string);
    procedure SetHint(const AHint: string);
  protected
    function GetLocalizedName: string; virtual;
    procedure SetLocalizedName(const AValue: string); virtual;
    procedure SetCategory(const AValue: TIDECommandCategory); virtual;
    procedure SetShortcutA(const AValue: TIDEShortCut); virtual;
    procedure SetShortcutB(const AValue: TIDEShortCut); virtual;
    procedure Change;
    procedure Init; virtual;
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
    procedure UserAdded(const aUser: TIDESpecialCommand);
    procedure UserRemoved(const aUser: TIDESpecialCommand);
    procedure DoOnUpdate; overload;
    procedure DoOnUpdate(Sender: TObject); overload;
  public
    property Enabled: Boolean write SetEnabled; // set Enabled of all "Users" TIDESpecialCommand, use Users to read
    property Caption: string write SetCaption; // set Caption of all "Users" TIDESpecialCommand, use Users to read
    property Hint: string write SetHint; // set Hint of all "Users" TIDESpecialCommand, use Users to read
    // don't add Visible property here - it is not generic. Tool buttons should never be hidden programmatically
  public
    property Name: String read FName;
    property Command: word read FCommand;// see the ecXXX constants above
    property LocalizedName: string read GetLocalizedName write SetLocalizedName;
    property Category: TIDECommandCategory read FCategory write SetCategory;
    property ShortcutA: TIDEShortCut read FShortcutA write SetShortcutA;
    property ShortcutB: TIDEShortCut read FShortcutB write SetShortcutB;
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
    property OnExecuteProc: TNotifyProcedure read FOnExecuteProc write SetOnExecuteProc;
    property OnUpdate: TNotifyEvent read FOnUpdateMethod write FOnUpdateMethod;
    property OnUpdateProc: TNotifyProcedure read FOnUpdateProc write FOnUpdateProc;

    property Users[Index: Integer]: TIDESpecialCommand read GetUser;
    property UserCount: Integer read GetUserCount;
  end;


  { TIDECommands }

  TIDECommands = class
  private
    FCustomUpdateEvents: TMethodList;
    FDontExecuteUpdateEventsUntil: QWord;

    procedure ApplicationOnIdle({%H-}Sender: TObject; var {%H-}Done: Boolean);
  protected
    function GetCategory(Index: integer): TIDECommandCategory; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

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
    function FindCommandsByShortCut(const ShortCutMask: TIDEShortCut;
            IDEWindowClass: TCustomFormClass = nil): TFPList; virtual; abstract; // list of TIDECommand
    function RemoveShortCut(ShortCutMask: TIDEShortCut;
            IDEWindowClass: TCustomFormClass = nil): Integer; virtual; abstract;
    function CategoryCount: integer; virtual; abstract;
  public
    procedure StartUpdateEvents;
    procedure StopUpdateEvents;

    procedure ExecuteUpdateEvents;
    procedure CancelPostponeUpdateEvents;
    procedure PostponeUpdateEvents;

    procedure AddCustomUpdateEvent(const aEvent: TNotifyEvent);
    procedure RemoveCustomUpdateEvent(const aEvent: TNotifyEvent);
  public
    property Categories[Index: integer]: TIDECommandCategory read GetCategory;
  end;

  // MenuItem and ButtonCommand inherit from SpecialCommand.

  TGetHintCaptionEvent = procedure(Sender: TObject; var ACaption, AHint: string) of object;

  TIDESpecialCommand = class(TPersistent)
  private
    FCommand: TIDECommand;
    FName: string;
    FCaption: string;
    FEnabled: Boolean;
    FChecked: Boolean;
    FHint: string;
    FImageIndex: Integer;
    FOnClickMethod: TNotifyEvent;
    FOnClickProc: TNotifyProcedure;
    FOnRequestCaption: TGetHintCaptionEvent;
    FSyncProperties: Boolean;
    FBlockSync: Integer;
  protected
    function SyncAvailable: Boolean; virtual;
    function GetCaption: string; virtual;
    procedure SetCommand(const AValue: TIDECommand); virtual;
    procedure SetName(const aName: string); virtual;
    procedure SetCaption(aCaption: string); virtual;
    procedure SetEnabled(const aEnabled: Boolean); virtual;
    procedure SetChecked(const aChecked: Boolean); virtual;
    procedure SetHint(const aHint: string); virtual;
    procedure SetImageIndex(const aImageIndex: Integer); virtual;
    procedure SetOnClickMethod(const aOnClick: TNotifyEvent); virtual;
    procedure SetOnClickProc(const aOnClickProc: TNotifyProcedure); virtual;
    procedure SetOnRequestCaption(
      const aOnRequestCaptionHint: TGetHintCaptionEvent); virtual;
    procedure SetResourceName(const aResourceName: string); virtual;
    procedure ShortCutsUpdated(const {%H-}aShortCut, {%H-}aShortCutKey2: TShortCut); virtual;
  public
    constructor Create(const aName: string); virtual;
    destructor Destroy; override;
  public
    procedure DoOnClick; overload;
    procedure DoOnClick(Sender: TObject); virtual; overload;
    function DoOnRequestCaption(Sender: TObject): Boolean; virtual;

    procedure BlockSync;
    procedure UnblockSync;
  public
    function GetCaptionWithShortCut: String; virtual;
    function GetHintOrCaptionWithShortCut: String; virtual;
    function GetShortcut: String; virtual;

    property Command: TIDECommand read FCommand write SetCommand;
    property SyncProperties: Boolean read FSyncProperties write FSyncProperties;
    property Name: string read FName write SetName;
    property Caption: string read GetCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Checked: Boolean read FChecked write SetChecked;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ResourceName: string write SetResourceName;
    // don't add Visible property here - it is not generic. Tool buttons should never be hidden programmatically

    property OnClick: TNotifyEvent read FOnClickMethod write SetOnClickMethod;
    property OnClickProc: TNotifyProcedure read FOnClickProc write SetOnClickProc;
    property OnRequestCaptionHint: TGetHintCaptionEvent read FOnRequestCaption write SetOnRequestCaption;
  end;

  TIDESpecialCommandEnumerator = class
  private
    FList: TIDESpecialCommands;
    FPosition: Integer;
  public
    constructor Create(AButtons: TIDESpecialCommands);
    function GetCurrent: TIDESpecialCommand;
    function MoveNext: Boolean;
    property Current: TIDESpecialCommand read GetCurrent;
  end;

  TIDESpecialCommands = class
  private
    FList: TFPList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TIDESpecialCommand;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetEnumerator: TIDESpecialCommandEnumerator;
    procedure Add(const aUser: TIDESpecialCommand);
    procedure Remove(const aUser: TIDESpecialCommand);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TIDESpecialCommand read GetItems; default;
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
function IsValidIDECommandKey(Key: word): boolean;

var
  // will be set by the IDE
  IDECommandList: TIDECommands;
  IDECommandScopes: TIDECommandScopes = nil;
var
  IDECmdScopeSrcEdit: TIDECommandScope;
  IDECmdScopeSrcEditOnly: TIDECommandScope;
  IDECmdScopeSrcEditOnlyMultiCaret: TIDECommandScope;
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
function IDECommandToIdent(Cmd: longint): string;
procedure GetIDEEditorCommandValues(Proc: TGetStrProc);

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

function IsValidIDECommandKey(Key: word): boolean;
begin
  case Key of
  VK_UNDEFINED,VK_UNKNOWN,
  VK_CONTROL,VK_LCONTROL,VK_RCONTROL,
  VK_SHIFT,VK_LSHIFT,VK_RSHIFT,
  VK_LBUTTON,VK_MBUTTON,VK_RBUTTON,
  VK_LWIN,VK_RWIN:
    exit(false);
  end;
  Result:=true;
end;

procedure CreateStandardIDECommandScopes;
begin
  IDECommandScopes:=TIDECommandScopes.Create;
  IDECmdScopeSrcEdit:=RegisterIDECommandScope('SourceEditor');
  IDECmdScopeSrcEditOnly:=RegisterIDECommandScope('SourceEditorOnly');
  IDECmdScopeSrcEditOnlyMultiCaret:=RegisterIDECommandScope('IDECmdScopeSrcEditOnlyMultiCaret');
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

function TIDECommandScope.HasIDEWindowClass(AWindowClass: TCustomFormClass): boolean;
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

procedure TIDECommandCategory.DoOnUpdate;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TIDECommand(Items[i]).DoOnUpdate;
end;

function TIDECommandCategory.ScopeIntersects(AScope: TIDECommandScope): boolean;
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

procedure TIDECommand.UserAdded(const aUser: TIDESpecialCommand);
begin
  FUsers.Add(aUser);
end;

procedure TIDECommand.UserRemoved(const aUser: TIDESpecialCommand);
begin
  FUsers.Remove(aUser);
end;

procedure TIDECommand.Change;
var
  xUser: TIDESpecialCommand;
begin
  for xUser in FUsers do
  begin
    xUser.ShortCutsUpdated(KeyToShortCut(ShortcutA.Key1,ShortcutA.Shift1),
                           KeyToShortCut(ShortcutA.Key2,ShortcutA.Shift2));
  end;
end;

procedure TIDECommand.Init;
begin
  //
end;

function TIDECommand.GetLocalizedName: string;
begin
  if FLocalizedName<>'' then
    Result:=FLocalizedName
  else
    Result:=Name;
end;

function TIDECommand.GetUser(Index: Integer): TIDESpecialCommand;
begin
  Result := FUsers[Index];
end;

function TIDECommand.GetUserCount: Integer;
begin
  Result := FUsers.Count;
end;

procedure TIDECommand.SetLocalizedName(const AValue: string);
begin
  if FLocalizedName=AValue then exit;
  FLocalizedName:=AValue;
  //DebugLn('TIDECommand.SetLocalizedName ',dbgs(Assigned(OnChange)),' ',Name);
  Change;
end;

procedure TIDECommand.SetOnExecute(const aOnExecute: TNotifyEvent);
var
  xUser: TIDESpecialCommand;
begin
  if CompareMethods(TMethod(FOnExecute), TMethod(aOnExecute)) then Exit;
  FOnExecute := aOnExecute;
  for xUser in FUsers do
    if xUser.SyncProperties then
    begin
      xUser.BlockSync;
      try
        xUser.OnClick := aOnExecute;
      finally
        xUser.UnblockSync;
      end;
    end;
end;

procedure TIDECommand.SetOnExecuteProc(const aOnExecuteProc: TNotifyProcedure);
var
  xUser: TIDESpecialCommand;
begin
  if FOnExecuteProc = aOnExecuteProc then Exit;
  FOnExecuteProc := aOnExecuteProc;
  for xUser in FUsers do
    if xUser.SyncProperties then
    begin
      xUser.BlockSync;
      try
        xUser.OnClickProc := aOnExecuteProc;
      finally
        xUser.UnblockSync;
      end;
    end;
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

procedure TIDECommand.SetEnabled(const AEnabled: Boolean);
var
  xUser: TIDESpecialCommand;
begin
  for xUser in FUsers do
    if xUser.SyncProperties then
    begin
      xUser.BlockSync;
      try
        xUser.Enabled := AEnabled;
      finally
        xUser.UnblockSync;
      end;
    end;
end;

procedure TIDECommand.SetHint(const AHint: string);
var
  xUser: TIDESpecialCommand;
begin
  for xUser in FUsers do
    if xUser.SyncProperties then
    begin
      xUser.BlockSync;
      try
        xUser.Hint := AHint;
      finally
        xUser.UnblockSync;
      end;
    end;
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
  FUsers:=TIDESpecialCommands.Create;
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
  Init;
end;

constructor TIDECommand.Create(TheCategory: TIDECommandCategory;
  const TheName, TheLocalizedName: String; TheCommand: word);
begin
  Create(TheCategory, TheName, TheLocalizedName, TheCommand,
         CleanIDEShortCut, CleanIDEShortCut, Nil, Nil);
end;

constructor TIDECommand.Create(ACommand: TIDECommand; ACategory: TIDECommandCategory);
begin
  Create(ACategory, ACommand.Name, ACommand.LocalizedName, ACommand.Command,
    ACommand.ShortcutA, ACommand.ShortcutB, ACommand.OnExecute, ACommand.OnExecuteProc);
end;

destructor TIDECommand.Destroy;
begin
  if Category <> nil then
    Category := nil;
  FUsers.Free;
  inherited Destroy;
end;

procedure TIDECommand.DoOnUpdate(Sender: TObject);
begin
  if Assigned(FOnUpdateProc) then
    FOnUpdateProc(Sender);
  if Assigned(FOnUpdateMethod) then
    FOnUpdateMethod(Sender);
end;

procedure TIDECommand.DoOnUpdate;
begin
  DoOnUpdate(Self);
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

procedure TIDECommand.SetCaption(const ACaption: string);
var
  xUser: TIDESpecialCommand;
begin
  for xUser in FUsers do
    if xUser.SyncProperties then
    begin
      xUser.BlockSync;
      try
        xUser.Caption := ACaption;
      finally
        xUser.UnblockSync;
      end;
    end;
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
    Result:='"'+Category.Description+'" -> '+Result;
end;

function TIDECommand.Execute(Sender: TObject): boolean;
begin
  Result:=false;
  if Assigned(OnExecute) then begin
    Result:=true;
    OnExecute(Sender);
  end else
  if Assigned(OnExecuteProc) then begin
    Result:=true;
    OnExecuteProc(Sender);
  end;
end;

{ TIDECommands }

procedure TIDECommands.AddCustomUpdateEvent(const aEvent: TNotifyEvent);
begin
  FCustomUpdateEvents.Add(TMethod(aEvent));
end;

procedure TIDECommands.ApplicationOnIdle(Sender: TObject; var Done: Boolean);
begin
  if (FDontExecuteUpdateEventsUntil > 0) and (GetTickCount64 < FDontExecuteUpdateEventsUntil) then
    Exit;

  ExecuteUpdateEvents;
  FDontExecuteUpdateEventsUntil := 0;
end;

constructor TIDECommands.Create;
begin
  inherited Create;

  FCustomUpdateEvents := TMethodList.Create;
end;

destructor TIDECommands.Destroy;
begin
  FCustomUpdateEvents.Free;
  inherited Destroy;
end;

procedure TIDECommands.ExecuteUpdateEvents;
var
  i: Integer;
begin
  if not Application.Active or
     (ActivePopupMenu <> nil) or//no popup menus
     (Application.ModalLevel > 0) or//no modal windows
     not IsWindowEnabled(Application.MainForm.Handle)//main IDE must be enabled
  then
    Exit;

  FCustomUpdateEvents.CallNotifyEvents(Self);
  for i := 0 to CategoryCount-1 do
    Categories[i].DoOnUpdate;
end;

procedure TIDECommands.CancelPostponeUpdateEvents;
begin
  FDontExecuteUpdateEventsUntil := 0;
end;

procedure TIDECommands.PostponeUpdateEvents;
begin
  FDontExecuteUpdateEventsUntil := GetTickCount64 + 500;
end;

procedure TIDECommands.RemoveCustomUpdateEvent(const aEvent: TNotifyEvent);
begin
  FCustomUpdateEvents.Remove(TMethod(aEvent));
end;

procedure TIDECommands.StartUpdateEvents;
begin
  Application.AddOnIdleHandler(@ApplicationOnIdle, False);
end;

procedure TIDECommands.StopUpdateEvents;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
end;

{ TIDESpecialCommand }

constructor TIDESpecialCommand.Create(const aName: string);
begin
  inherited Create;

  FSyncProperties:=true;
  FName := aName;
  FEnabled:=true;
  FImageIndex:=-1;
end;

procedure TIDESpecialCommand.BlockSync;
begin
  Inc(FBlockSync);
end;

destructor TIDESpecialCommand.Destroy;
begin
  if Assigned(FCommand) then
    FCommand.UserRemoved(Self);
  inherited Destroy;
end;

procedure TIDESpecialCommand.DoOnClick(Sender: TObject);
begin
  if Assigned(FOnClickProc) then
    FOnClickProc(Sender)
  else
  if Assigned(FOnClickMethod) then
    FOnClickMethod(Sender);
end;

procedure TIDESpecialCommand.DoOnClick;
begin
  DoOnClick(Self);
end;

function TIDESpecialCommand.DoOnRequestCaption(Sender: TObject): Boolean;
var
  xCaption, xHint: string;
begin
  Result := Assigned(FOnRequestCaption);
  if Result then
  begin
    xCaption := Caption;
    xHint := Hint;
    FOnRequestCaption(Sender, xCaption, xHint);
    Caption := xCaption;
    Hint := xHint;
  end;
end;

function TIDESpecialCommand.GetCaption: string;
begin
  if FCaption<>'' then
    Result:=FCaption
  else if (FCommand<>nil) and (FCommand.LocalizedName<>'') then
    Result:=FCommand.LocalizedName
  else
    Result:=FName;
end;

function TIDESpecialCommand.GetCaptionWithShortCut: String;
begin
  Result := Caption;
  DeleteAmpersands(Result);
  Result := Result + GetShortcut;
end;

function TIDESpecialCommand.GetHintOrCaptionWithShortCut: String;
begin
  if Hint <> '' then
    Result := Hint
  else
    Result := Caption;
  DeleteAmpersands(Result);
  Result := Result + GetShortcut;
end;

function TIDESpecialCommand.GetShortcut: String;
begin
  Result := '';
  if Assigned(FCommand) then
    Result := ShortCutToText(FCommand.AsShortCut);
  if Result <> '' then
    Result := ' (' + Result + ')';
end;

procedure TIDESpecialCommand.SetCaption(aCaption: string);
var
  xUser: TIDESpecialCommand;
begin
  if FCaption=aCaption then Exit;
  FCaption := aCaption;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.Caption:=aCaption;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetChecked(const aChecked: Boolean);
var
  xUser: TIDESpecialCommand;
begin
  if FChecked=aChecked then Exit;
  FChecked := aChecked;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.Checked:=aChecked;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetCommand(const AValue: TIDECommand);
begin
  if FCommand = AValue then
    Exit;
  FCommand := AValue;
  if FCommand <> nil then
  begin
    if (FCommand.OnExecute=nil) and (OnClick<>nil) then
      FCommand.OnExecute := OnClick
    else
    if (OnClick=nil) and (FCommand.OnExecute<>nil) then
      OnClick := FCommand.OnExecute;

    if (FCommand.OnExecuteProc=nil) and (OnClickProc<>nil) then
      FCommand.OnExecuteProc := OnClickProc
    else
    if (OnClickProc=nil) and (FCommand.OnExecuteProc<>nil) then
      OnClickProc := FCommand.OnExecuteProc;

    FCommand.UserAdded(Self);
    FCommand.Change;
  end;
end;

procedure TIDESpecialCommand.SetEnabled(const aEnabled: Boolean);
var
  xUser: TIDESpecialCommand;
begin
  if FEnabled=aEnabled then Exit;
  FEnabled := aEnabled;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.Enabled:=aEnabled;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetHint(const aHint: string);
var
  xUser: TIDESpecialCommand;
begin
  if FHint=aHint then Exit;
  FHint := aHint;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.Hint:=aHint;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetImageIndex(const aImageIndex: Integer);
var
  xUser: TIDESpecialCommand;
begin
  if FImageIndex=aImageIndex then Exit;
  FImageIndex := aImageIndex;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.ImageIndex:=aImageIndex;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetName(const aName: string);
begin
  FName := aName;
end;

procedure TIDESpecialCommand.SetOnClickMethod(const aOnClick: TNotifyEvent);
begin
  if CompareMethods(TMethod(FOnClickMethod), TMethod(aOnClick)) then Exit;
  FOnClickMethod := aOnClick;
  if (FCommand<>nil) and SyncAvailable then
    FCommand.OnExecute:=aOnClick;
end;

procedure TIDESpecialCommand.SetOnClickProc(const aOnClickProc: TNotifyProcedure);
begin
  if FOnClickProc = aOnClickProc then Exit;
  FOnClickProc := aOnClickProc;
  if (FCommand<> nil) and SyncAvailable then
    FCommand.OnExecuteProc:=aOnClickProc;
end;

procedure TIDESpecialCommand.SetOnRequestCaption(
  const aOnRequestCaptionHint: TGetHintCaptionEvent);
var
  xUser: TIDESpecialCommand;
begin
  if FOnRequestCaption = aOnRequestCaptionHint then Exit;
  FOnRequestCaption := aOnRequestCaptionHint;
  if (FCommand<> nil) and SyncAvailable then
    for xUser in FCommand.FUsers do
      if (xUser <> Self) and xUser.SyncProperties then
      begin
        xUser.BlockSync;
        try
          xUser.OnRequestCaptionHint:=aOnRequestCaptionHint;
        finally
          xUser.UnblockSync;
        end;
      end;
end;

procedure TIDESpecialCommand.SetResourceName(const aResourceName: string);
begin
  if aResourceName <> '' then
    ImageIndex := IDEImages.LoadImage(aResourceName)
  else
    ImageIndex := -1;
end;

procedure TIDESpecialCommand.ShortCutsUpdated(const aShortCut,
  aShortCutKey2: TShortCut);
begin
  //nothing here, override in descendants
end;

function TIDESpecialCommand.SyncAvailable: Boolean;
begin
  Result := FSyncProperties and (FBlockSync=0);
end;

procedure TIDESpecialCommand.UnblockSync;
begin
  Dec(FBlockSync);
end;

{ TIDESpecialCommandEnumerator }

constructor TIDESpecialCommandEnumerator.Create(AButtons: TIDESpecialCommands);
begin
  inherited Create;
  FList := AButtons;
  FPosition := -1;
end;

function TIDESpecialCommandEnumerator.GetCurrent: TIDESpecialCommand;
begin
  Result := TIDESpecialCommand(FList[FPosition]);
end;

function TIDESpecialCommandEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TIDESpecialCommands }

procedure TIDESpecialCommands.Add(const aUser: TIDESpecialCommand);
begin
  FList.Add(aUser);
end;

constructor TIDESpecialCommands.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TIDESpecialCommands.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Items[I].FCommand := nil;
  FList.Free;
  inherited Destroy;
end;

function TIDESpecialCommands.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIDESpecialCommands.GetEnumerator: TIDESpecialCommandEnumerator;
begin
  Result := TIDESpecialCommandEnumerator.Create(Self);
end;

function TIDESpecialCommands.GetItems(Index: Integer): TIDESpecialCommand;
begin
  Result := TIDESpecialCommand(FList[Index]);
end;

procedure TIDESpecialCommands.Remove(const aUser: TIDESpecialCommand);
begin
  FList.Remove(aUser);
end;

const
  IDEEditorCommandStrs: array[0..322] of TIdentMapEntry = (
  // search
    (Value: ecFind;                                   Name: 'ecFind'),
    (Value: ecFindAgain;                              Name: 'ecFindAgain'),
    (Value: ecFindNext;                               Name: 'ecFindNext'),
    (Value: ecFindPrevious;                           Name: 'ecFindPrevious'),
    (Value: ecReplace;                                Name: 'ecReplace'),
    (Value: ecIncrementalFind;                        Name: 'ecIncrementalFind'),
    (Value: ecFindProcedureDefinition;                Name: 'ecFindProcedureDefinition'),
    (Value: ecFindProcedureMethod;                    Name: 'ecFindProcedureMethod'),
    (Value: ecGotoLineNumber;                         Name: 'ecGotoLineNumber'),
    (Value: ecFindNextWordOccurrence;                 Name: 'ecFindNextWordOccurrence'),
    (Value: ecFindPrevWordOccurrence;                 Name: 'ecFindPrevWordOccurrence'),
    (Value: ecFindInFiles;                            Name: 'ecFindInFiles'),
    (Value: ecJumpBack;                               Name: 'ecJumpBack'),
    (Value: ecJumpForward;                            Name: 'ecJumpForward'),
    (Value: ecAddJumpPoint;                           Name: 'ecAddJumpPoint'),
    (Value: ecViewJumpHistory;                        Name: 'ecViewJumpHistory'),
    (Value: ecJumpToNextError;                        Name: 'ecJumpToNextError'),
    (Value: ecJumpToPrevError;                        Name: 'ecJumpToPrevError'),
    (Value: ecProcedureList;                          Name: 'ecProcedureList'),

  // search code
    (Value: ecFindDeclaration;                        Name: 'ecFindDeclaration'),
    (Value: ecFindBlockOtherEnd;                      Name: 'ecFindBlockOtherEnd'),
    (Value: ecFindBlockStart;                         Name: 'ecFindBlockStart'),
    (Value: ecOpenFileAtCursor;                       Name: 'ecOpenFileAtCursor'),
    (Value: ecGotoIncludeDirective;                   Name: 'ecGotoIncludeDirective'),
    (Value: ecJumpToInterface;                        Name: 'ecJumpToInterface'),
    (Value: ecJumpToInterfaceUses;                    Name: 'ecJumpToInterfaceUses'),
    (Value: ecJumpToImplementation;                   Name: 'ecJumpToImplementation'),
    (Value: ecJumpToImplementationUses;               Name: 'ecJumpToImplementationUses'),
    (Value: ecJumpToInitialization;                   Name: 'ecJumpToInitialization'),

  // edit selection
    (Value: ecSelectionUpperCase;                     Name: 'ecSelectionUpperCase'),
    (Value: ecSelectionLowerCase;                     Name: 'ecSelectionLowerCase'),
    (Value: ecSelectionSwapCase;                      Name: 'ecSelectionSwapCase'),
    (Value: ecSelectionTabs2Spaces;                   Name: 'ecSelectionTabs2Spaces'),
    (Value: ecSelectionEnclose;                       Name: 'ecSelectionEnclose'),
    (Value: ecSelectionComment;                       Name: 'ecSelectionComment'),
    (Value: ecSelectionUncomment;                     Name: 'ecSelectionUncomment'),
    (Value: ecSelectionSort;                          Name: 'ecSelectionSort'),
    (Value: ecSelectionBreakLines;                    Name: 'ecSelectionBreakLines'),
    (Value: ecSelectToBrace;                          Name: 'ecSelectToBrace'),
    (Value: ecSelectCodeBlock;                        Name: 'ecSelectCodeBlock'),
    (Value: ecSelectWord;                             Name: 'ecSelectWord'),
    (Value: ecSelectLine;                             Name: 'ecSelectLine'),
    (Value: ecSelectParagraph;                        Name: 'ecSelectParagraph'),
    (Value: ecSelectionEncloseIFDEF;                  Name: 'ecSelectionEncloseIFDEF'),
    (Value: ecToggleComment;                          Name: 'ecToggleComment'),

  // insert text
    (Value: ecInsertCharacter;                        Name: 'ecInsertCharacter'),
    (Value: ecInsertGUID;                             Name: 'ecInsertGUID'),
    (Value: ecInsertFilename;                         Name: 'ecInsertFilename'),
    (Value: ecInsertUserName;                         Name: 'ecInsertUserName'),
    (Value: ecInsertDateTime;                         Name: 'ecInsertDateTime'),
    (Value: ecInsertChangeLogEntry;                   Name: 'ecInsertChangeLogEntry'),
    (Value: ecInsertCVSAuthor;                        Name: 'ecInsertCVSAuthor'),
    (Value: ecInsertCVSDate;                          Name: 'ecInsertCVSDate'),
    (Value: ecInsertCVSHeader;                        Name: 'ecInsertCVSHeader'),
    (Value: ecInsertCVSID;                            Name: 'ecInsertCVSID'),
    (Value: ecInsertCVSLog;                           Name: 'ecInsertCVSLog'),
    (Value: ecInsertCVSName;                          Name: 'ecInsertCVSName'),
    (Value: ecInsertCVSRevision;                      Name: 'ecInsertCVSRevision'),
    (Value: ecInsertCVSSource;                        Name: 'ecInsertCVSSource'),
    (Value: ecInsertGPLNotice;                        Name: 'ecInsertGPLNotice'),
    (Value: ecInsertGPLNoticeTranslated;              Name: 'ecInsertGPLNoticeTranslated'),
    (Value: ecInsertLGPLNotice;                       Name: 'ecInsertLGPLNotice'),
    (Value: ecInsertLGPLNoticeTranslated;             Name: 'ecInsertLGPLNoticeTranslated'),
    (Value: ecInsertModifiedLGPLNotice;               Name: 'ecInsertModifiedLGPLNotice'),
    (Value: ecInsertModifiedLGPLNoticeTranslated;     Name: 'ecInsertModifiedLGPLNoticeTranslated'),
    (Value: ecInsertMITNotice;                        Name: 'ecInsertMITNotice'),
    (Value: ecInsertMITNoticeTranslated;              Name: 'ecInsertMITNoticeTranslated'),

  // source tools
    (Value: ecWordCompletion;                         Name: 'ecWordCompletion'),
    (Value: ecCompleteCode;                           Name: 'ecCompleteCode'),
    (Value: ecIdentCompletion;                        Name: 'ecIdentCompletion'),
    (Value: ecSyntaxCheck;                            Name: 'ecSyntaxCheck'),
    (Value: ecGuessUnclosedBlock;                     Name: 'ecGuessUnclosedBlock'),
    (Value: ecGuessMisplacedIFDEF;                    Name: 'ecGuessMisplacedIFDEF'),
    (Value: ecConvertDFM2LFM;                         Name: 'ecConvertDFM2LFM'),
    (Value: ecCheckLFM;                               Name: 'ecCheckLFM'),
    (Value: ecConvertDelphiUnit;                      Name: 'ecConvertDelphiUnit'),
    (Value: ecConvertDelphiProject;                   Name: 'ecConvertDelphiProject'),
    (Value: ecConvertDelphiPackage;                   Name: 'ecConvertDelphiPackage'),
    (Value: ecConvertEncoding;                        Name: 'ecConvertEncoding'),
    (Value: ecMakeResourceString;                     Name: 'ecMakeResourceString'),
    (Value: ecDiff;                                   Name: 'ecDiff'),
    (Value: ecExtractProc;                            Name: 'ecExtractProc'),
    (Value: ecFindIdentifierRefs;                     Name: 'ecFindIdentifierRefs'),
    (Value: ecRenameIdentifier;                       Name: 'ecRenameIdentifier'),
    (Value: ecInvertAssignment;                       Name: 'ecInvertAssignment'),
    (Value: ecShowCodeContext;                        Name: 'ecShowCodeContext'),
    (Value: ecShowAbstractMethods;                    Name: 'ecShowAbstractMethods'),
    (Value: ecRemoveEmptyMethods;                     Name: 'ecRemoveEmptyMethods'),
    (Value: ecRemoveUnusedUnits;                      Name: 'ecRemoveUnusedUnits'),
    (Value: ecUseUnit;                                Name: 'ecUseUnit'),
    (Value: ecFindOverloads;                          Name: 'ecFindOverloads'),
    (Value: ecFindUsedUnitRefs;                       Name: 'ecFindUsedUnitRefs'),
    (Value: ecCompleteCodeInteractive;                Name: 'ecCompleteCodeInteractive'),

  // file menu
    (Value: ecNew;                                    Name: 'ecNew'),
    (Value: ecNewUnit;                                Name: 'ecNewUnit'),
    (Value: ecNewForm;                                Name: 'ecNewForm'),
    (Value: ecOpen;                                   Name: 'ecOpen'),
    (Value: ecRevert;                                 Name: 'ecRevert'),
    (Value: ecSave;                                   Name: 'ecSave'),
    (Value: ecSaveAs;                                 Name: 'ecSaveAs'),
    (Value: ecSaveAll;                                Name: 'ecSaveAll'),
    (Value: ecClose;                                  Name: 'ecClose'),
    (Value: ecCloseAll;                               Name: 'ecCloseAll'),
    (Value: ecCleanDirectory;                         Name: 'ecCleanDirectory'),
    (Value: ecRestart;                                Name: 'ecRestart'),
    (Value: ecQuit;                                   Name: 'ecQuit'),
    (Value: ecCloseOtherTabs;                         Name: 'ecCloseOtherTabs'),
    (Value: ecCloseRightTabs;                         Name: 'ecCloseRightTabs'),

  // edit menu
    (Value: ecMultiPaste;                             Name: 'ecMultiPaste'),

  // IDE navigation
    (Value: ecToggleFormUnit;                         Name: 'ecToggleFormUnit'),
    (Value: ecToggleObjectInsp;                       Name: 'ecToggleObjectInsp'),
    (Value: ecToggleSourceEditor;                     Name: 'ecToggleSourceEditor'),
    (Value: ecToggleCodeExpl;                         Name: 'ecToggleCodeExpl'),
    (Value: ecToggleFPDocEditor;                      Name: 'ecToggleFPDocEditor'),
    (Value: ecToggleMessages;                         Name: 'ecToggleMessages'),
    (Value: ecToggleWatches;                          Name: 'ecToggleWatches'),
    (Value: ecToggleBreakPoints;                      Name: 'ecToggleBreakPoints'),
    (Value: ecToggleDebuggerOut;                      Name: 'ecToggleDebuggerOut'),
    (Value: ecViewUnitDependencies;                   Name: 'ecViewUnitDependencies'),
    (Value: ecViewUnitInfo;                           Name: 'ecViewUnitInfo'),
    (Value: ecToggleLocals;                           Name: 'ecToggleLocals'),
    (Value: ecToggleCallStack;                        Name: 'ecToggleCallStack'),
    (Value: ecToggleSearchResults;                    Name: 'ecToggleSearchResults'),
    (Value: ecViewAnchorEditor;                       Name: 'ecViewAnchorEditor'),
    (Value: ecViewTabOrder;                           Name: 'ecViewTabOrder'),
    (Value: ecToggleCodeBrowser;                      Name: 'ecToggleCodeBrowser'),
    (Value: ecToggleCompPalette;                      Name: 'ecToggleCompPalette'),
    (Value: ecToggleIDESpeedBtns;                     Name: 'ecToggleIDESpeedBtns'),
    (Value: ecViewComponents;                         Name: 'ecViewComponents'),
    (Value: ecToggleRestrictionBrowser;               Name: 'ecToggleRestrictionBrowser'),
    (Value: ecViewTodoList;                           Name: 'ecViewTodoList'),
    (Value: ecToggleRegisters;                        Name: 'ecToggleRegisters'),
    (Value: ecToggleAssembler;                        Name: 'ecToggleAssembler'),
    (Value: ecToggleDebugEvents;                      Name: 'ecToggleDebugEvents'),
    (Value: ecViewPseudoTerminal;                     Name: 'ecViewPseudoTerminal'),
    (Value: ecViewThreads;                            Name: 'ecViewThreads'),
    (Value: ecViewHistory;                            Name: 'ecViewHistory'),
    (Value: ecViewMacroList;                          Name: 'ecViewMacroList'),

  // sourcenotebook commands
    (Value: ecNextEditor;                             Name: 'ecNextEditor'),
    (Value: ecPrevEditor;                             Name: 'ecPrevEditor'),
    (Value: ecMoveEditorLeft;                         Name: 'ecMoveEditorLeft'),
    (Value: ecMoveEditorRight;                        Name: 'ecMoveEditorRight'),
    (Value: ecToggleBreakPoint;                       Name: 'ecToggleBreakPoint'),
    (Value: ecToggleBreakPointEnabled;                Name: 'ecToggleBreakPointEnabled'),

    (Value: ecRemoveBreakPoint;                       Name: 'ecRemoveBreakPoint'),
    (Value: ecMoveEditorLeftmost;                     Name: 'ecMoveEditorLeftmost'),
    (Value: ecMoveEditorRightmost;                    Name: 'ecMoveEditorRightmost'),

    (Value: ecNextSharedEditor;                       Name: 'ecNextSharedEditor'),
    (Value: ecPrevSharedEditor;                       Name: 'ecPrevSharedEditor'),

    (Value: ecNextWindow;                             Name: 'ecNextWindow'),
    (Value: ecPrevWindow;                             Name: 'ecPrevWindow'),
    (Value: ecMoveEditorNextWindow;                   Name: 'ecMoveEditorNextWindow'),
    (Value: ecMoveEditorPrevWindow;                   Name: 'ecMoveEditorPrevWindow'),
    (Value: ecMoveEditorNewWindow;                    Name: 'ecMoveEditorNewWindow'),
    (Value: ecCopyEditorNextWindow;                   Name: 'ecCopyEditorNextWindow'),
    (Value: ecCopyEditorPrevWindow;                   Name: 'ecCopyEditorPrevWindow'),
    (Value: ecCopyEditorNewWindow;                    Name: 'ecCopyEditorNewWindow'),
    (Value: ecPrevEditorInHistory;                    Name: 'ecPrevEditorInHistory'),
    (Value: ecNextEditorInHistory;                    Name: 'ecNextEditorInHistory'),

    (Value: ecGotoEditor1;                            Name: 'ecGotoEditor1'),
    (Value: ecGotoEditor2;                            Name: 'ecGotoEditor2'),
    (Value: ecGotoEditor3;                            Name: 'ecGotoEditor3'),
    (Value: ecGotoEditor4;                            Name: 'ecGotoEditor4'),
    (Value: ecGotoEditor5;                            Name: 'ecGotoEditor5'),
    (Value: ecGotoEditor6;                            Name: 'ecGotoEditor6'),
    (Value: ecGotoEditor7;                            Name: 'ecGotoEditor7'),
    (Value: ecGotoEditor8;                            Name: 'ecGotoEditor8'),
    (Value: ecGotoEditor9;                            Name: 'ecGotoEditor9'),
    (Value: ecGotoEditor0;                            Name: 'ecGotoEditor0'),

    (Value: ecLockEditor;                             Name: 'ecLockEditor'),

  // marker
    (Value: ecSetFreeBookmark;                        Name: 'ecSetFreeBookmark'),
    (Value: ecPrevBookmark;                           Name: 'ecPrevBookmark'),
    (Value: ecNextBookmark;                           Name: 'ecNextBookmark'),
    (Value: ecClearBookmarkForFile;                   Name: 'ecClearBookmarkForFile'),
    (Value: ecClearAllBookmark;                       Name: 'ecClearAllBookmark'),
    (Value: ecGotoBookmarks;                          Name: 'ecGotoBookmarks'),
    (Value: ecToggleBookmarks;                        Name: 'ecToggleBookmarks'),

  // Macro
    (Value: ecSynMacroRecord;                         Name: 'ecSynMacroRecord'),
    (Value: ecSynMacroPlay;                           Name: 'ecSynMacroPlay'),

  // run menu
    (Value: ecCompile;                                Name: 'ecCompile'),
    (Value: ecBuild;                                  Name: 'ecBuild'),
    (Value: ecQuickCompile;                           Name: 'ecQuickCompile'),
    (Value: ecCleanUpAndBuild;                        Name: 'ecCleanUpAndBuild'),
    (Value: ecAbortBuild;                             Name: 'ecAbortBuild'),
    (Value: ecRunWithoutDebugging;                    Name: 'ecRunWithoutDebugging'),
    (Value: ecRun;                                    Name: 'ecRun'),
    (Value: ecPause;                                  Name: 'ecPause'),
    (Value: ecStepInto;                               Name: 'ecStepInto'),
    (Value: ecStepOver;                               Name: 'ecStepOver'),
    (Value: ecStepToCursor;                            Name: 'ecStepToCursor'),
    (Value: ecStopProgram;                            Name: 'ecStopProgram'),
    (Value: ecResetDebugger;                          Name: 'ecResetDebugger'),
    (Value: ecRunParameters;                          Name: 'ecRunParameters'),
    (Value: ecBuildFile;                              Name: 'ecBuildFile'),
    (Value: ecRunFile;                                Name: 'ecRunFile'),
    (Value: ecConfigBuildFile;                        Name: 'ecConfigBuildFile'),
    (Value: ecInspect;                                Name: 'ecInspect'),
    (Value: ecEvaluate;                               Name: 'ecEvaluate'),
    (Value: ecAddWatch;                               Name: 'ecAddWatch'),
    (Value: ecShowExecutionPoint;                     Name: 'ecShowExecutionPoint'),
    (Value: ecStepOut;                                Name: 'ecStepOut'),
    (Value: ecStepIntoInstr;                          Name: 'ecStepIntoInstr'),
    (Value: ecStepOverInstr;                          Name: 'ecStepOverInstr'),
    (Value: ecStepIntoContext;                        Name: 'ecStepIntoContext'),
    (Value: ecStepOverContext;                        Name: 'ecStepOverContext'),
    (Value: ecAddBpSource;                            Name: 'ecAddBpSource'),
    (Value: ecAddBpAddress;                           Name: 'ecAddBpAddress'),
    (Value: ecAddBpDataWatch;                         Name: 'ecAddBpDataWatch'),
    (Value: ecAttach;                                 Name: 'ecAttach'),
    (Value: ecDetach;                                 Name: 'ecDetach'),

  // 460++ : used for ecViewHistory (debugger) / ecViewMacroList

  // project menu
    (Value: ecNewProject;                             Name: 'ecNewProject'),
    (Value: ecNewProjectFromFile;                     Name: 'ecNewProjectFromFile'),
    (Value: ecOpenProject;                            Name: 'ecOpenProject'),
    (Value: ecCloseProject;                           Name: 'ecCloseProject'),
    (Value: ecSaveProject;                            Name: 'ecSaveProject'),
    (Value: ecSaveProjectAs;                          Name: 'ecSaveProjectAs'),
    (Value: ecPublishProject;                         Name: 'ecPublishProject'),
    (Value: ecProjectInspector;                       Name: 'ecProjectInspector'),
    (Value: ecAddCurUnitToProj;                       Name: 'ecAddCurUnitToProj'),
    (Value: ecRemoveFromProj;                         Name: 'ecRemoveFromProj'),
    (Value: ecViewProjectUnits;                       Name: 'ecViewProjectUnits'),
    (Value: ecViewProjectForms;                       Name: 'ecViewProjectForms'),
    (Value: ecViewProjectSource;                      Name: 'ecViewProjectSource'),
    (Value: ecProjectOptions;                         Name: 'ecProjectOptions'),
    (Value: ecProjectChangeBuildMode;                 Name: 'ecProjectChangeBuildMode'),
    (Value: ecProjectResaveFormsWithI18n;             Name: 'ecProjectResaveFormsWithI18n'),

  // package menu
    (Value: ecOpenPackage;                            Name: 'ecOpenPackage'),
    (Value: ecOpenPackageFile;                        Name: 'ecOpenPackageFile'),
    (Value: ecOpenPackageOfCurUnit;                   Name: 'ecOpenPackageOfCurUnit'),
    (Value: ecAddCurFileToPkg;                        Name: 'ecAddCurFileToPkg'),
    (Value: ecNewPkgComponent;                        Name: 'ecNewPkgComponent'),
    //(Value: ecAddCurUnitToPkg;                        Name: 'ecAddCurUnitToPkg'),
    (Value: ecPackageGraph;                           Name: 'ecPackageGraph'),
    (Value: ecEditInstallPkgs;                        Name: 'ecEditInstallPkgs'),
    (Value: ecConfigCustomComps;                      Name: 'ecConfigCustomComps'),
    (Value: ecNewPackage;                             Name: 'ecNewPackage'),

  // custom tools menu
    (Value: ecExtToolFirst;                           Name: 'ecExtToolFirst'),
    (Value: ecExtToolLast;                            Name: 'ecExtToolLast'),

  // tools menu
    (Value: ecEnvironmentOptions;                     Name: 'ecEnvironmentOptions'),
    (Value: ecManageDesktops;                         Name: 'ecManageDesktops'),
    (Value: ecRescanFPCSrcDir;                        Name: 'ecRescanFPCSrcDir'),
    (Value: ecEditCodeTemplates;                      Name: 'ecEditCodeTemplates'),
    (Value: ecCodeToolsDefinesEd;                     Name: 'ecCodeToolsDefinesEd'),

    (Value: ecExtToolSettings;                        Name: 'ecExtToolSettings'),
    (Value: ecManageExamples;                         Name: 'ecManageExamples'),
    (Value: ecConfigBuildLazarus;                     Name: 'ecConfigBuildLazarus'),
    (Value: ecBuildLazarus;                           Name: 'ecBuildLazarus'),
    (Value: ecBuildAdvancedLazarus;                   Name: 'ecBuildAdvancedLazarus'),

  // window menu
    (Value: ecManageSourceEditors;                          Name: 'ecWindowManager'),

  // help menu
    (Value: ecAboutLazarus;                           Name: 'ecAboutLazarus'),
    (Value: ecOnlineHelp;                             Name: 'ecOnlineHelp'),
    (Value: ecContextHelp;                            Name: 'ecContextHelp'),
    (Value: ecEditContextHelp;                        Name: 'ecEditContextHelp'),
    (Value: ecReportingBug;                           Name: 'ecReportingBug'),
    (Value: ecFocusHint;                              Name: 'ecFocusHint'),
    (Value: ecSmartHint;                              Name: 'ecSmartHint'),

  // designer
    (Value: ecDesignerCopy;                           Name: 'ecDesignerCopy'),
    (Value: ecDesignerCut;                            Name: 'ecDesignerCut'),
    (Value: ecDesignerPaste;                          Name: 'ecDesignerPaste'),
    (Value: ecDesignerSelectParent;                   Name: 'ecDesignerSelectParent'),
    (Value: ecDesignerMoveToFront;                    Name: 'ecDesignerMoveToFront'),
    (Value: ecDesignerMoveToBack;                     Name: 'ecDesignerMoveToBack'),
    (Value: ecDesignerForwardOne;                     Name: 'ecDesignerForwardOne'),
    (Value: ecDesignerBackOne;                        Name: 'ecDesignerBackOne'),
    (Value: ecDesignerToggleNonVisComps;              Name: 'ecDesignerToggleNonVisComps'),

  // TSynPluginTemplateEdit - In cell
    (Value: ecIdePTmplEdNextCell;                     Name: 'ecIdePTmplEdNextCell'),
    (Value: ecIdePTmplEdNextCellSel;                  Name: 'ecIdePTmplEdNextCellSel'),
    (Value: ecIdePTmplEdNextCellRotate;               Name: 'ecIdePTmplEdNextCellRotate'),
    (Value: ecIdePTmplEdNextCellSelRotate;            Name: 'ecIdePTmplEdNextCellSelRotate'),
    (Value: ecIdePTmplEdPrevCell;                     Name: 'ecIdePTmplEdPrevCell'),
    (Value: ecIdePTmplEdPrevCellSel;                  Name: 'ecIdePTmplEdPrevCellSel'),
    (Value: ecIdePTmplEdCellHome;                     Name: 'ecIdePTmplEdCellHome'),
    (Value: ecIdePTmplEdCellEnd;                      Name: 'ecIdePTmplEdCellEnd'),
    (Value: ecIdePTmplEdCellSelect;                   Name: 'ecIdePTmplEdCellSelect'),
    (Value: ecIdePTmplEdFinish;                       Name: 'ecIdePTmplEdFinish'),
    (Value: ecIdePTmplEdEscape;                       Name: 'ecIdePTmplEdEscape'),
    (Value: ecIdePTmplEdNextFirstCell;                Name: 'ecIdePTmplEdNextFirstCell'),
    (Value: ecIdePTmplEdNextFirstCellSel;             Name: 'ecIdePTmplEdNextFirstCellSel'),
    (Value: ecIdePTmplEdNextFirstCellRotate;          Name: 'ecIdePTmplEdNextFirstCellRotate'),
    (Value: ecIdePTmplEdNextFirstCellSelRotate;       Name: 'ecIdePTmplEdNextFirstCellSelRotate'),
    (Value: ecIdePTmplEdPrevFirstCell;                Name: 'ecIdePTmplEdPrevFirstCell'),
    (Value: ecIdePTmplEdPrevFirstCellSel;             Name: 'ecIdePTmplEdPrevFirstCellSel'),

  // TSynPluginTemplateEdit - Out off Cell
    (Value: ecIdePTmplEdOutNextCell;                  Name: 'ecIdePTmplEdOutNextCell'),
    (Value: ecIdePTmplEdOutNextCellSel;               Name: 'ecIdePTmplEdOutNextCellSel'),
    (Value: ecIdePTmplEdOutNextCellRotate;            Name: 'ecIdePTmplEdOutNextCellRotate'),
    (Value: ecIdePTmplEdOutNextCellSelRotate;         Name: 'ecIdePTmplEdOutNextCellSelRotate'),
    (Value: ecIdePTmplEdOutPrevCell;                  Name: 'ecIdePTmplEdOutPrevCell'),
    (Value: ecIdePTmplEdOutPrevCellSel;               Name: 'ecIdePTmplEdOutPrevCellSel'),
    (Value: ecIdePTmplEdOutCellHome;                  Name: 'ecIdePTmplEdOutCellHome'),
    (Value: ecIdePTmplEdOutCellEnd;                   Name: 'ecIdePTmplEdOutCellEnd'),
    (Value: ecIdePTmplEdOutCellSelect;                Name: 'ecIdePTmplEdOutCellSelect'),
    (Value: ecIdePTmplEdOutFinish;                    Name: 'ecIdePTmplEdOutFinish'),
    (Value: ecIdePTmplEdOutEscape;                    Name: 'ecIdePTmplEdOutEscape'),
    (Value: ecIdePTmplEdOutNextFirstCell;             Name: 'ecIdePTmplEdOutNextFirstCell'),
    (Value: ecIdePTmplEdOutNextFirstCellSel;          Name: 'ecIdePTmplEdOutNextFirstCellSel'),
    (Value: ecIdePTmplEdOutNextFirstCellRotate;       Name: 'ecIdePTmplEdOutNextFirstCellRotate'),
    (Value: ecIdePTmplEdOutNextFirstCellSelRotate;    Name: 'ecIdePTmplEdOutNextFirstCellSelRotate'),
    (Value: ecIdePTmplEdOutPrevFirstCell;             Name: 'ecIdePTmplEdOutPrevFirstCell'),
    (Value: ecIdePTmplEdOutPrevFirstCellSel;          Name: 'ecIdePTmplEdOutPrevFirstCellSel'),

  // TSynPluginSyncroEdit - in celll
    (Value: ecIdePSyncroEdNextCell;                   Name: 'ecIdePSyncroEdNextCell'),
    (Value: ecIdePSyncroEdNextCellSel;                Name: 'ecIdePSyncroEdNextCellSel'),
    (Value: ecIdePSyncroEdPrevCell;                   Name: 'ecIdePSyncroEdPrevCell'),
    (Value: ecIdePSyncroEdPrevCellSel;                Name: 'ecIdePSyncroEdPrevCellSel'),
    (Value: ecIdePSyncroEdCellHome;                   Name: 'ecIdePSyncroEdCellHome'),
    (Value: ecIdePSyncroEdCellEnd;                    Name: 'ecIdePSyncroEdCellEnd'),
    (Value: ecIdePSyncroEdCellSelect;                 Name: 'ecIdePSyncroEdCellSelect'),
    (Value: ecIdePSyncroEdEscape;                     Name: 'ecIdePSyncroEdEscape'),
    (Value: ecIdePSyncroEdNextFirstCell;              Name: 'ecIdePSyncroEdNextFirstCell'),
    (Value: ecIdePSyncroEdNextFirstCellSel;           Name: 'ecIdePSyncroEdNextFirstCellSel'),
    (Value: ecIdePSyncroEdPrevFirstCell;              Name: 'ecIdePSyncroEdPrevFirstCell'),
    (Value: ecIdePSyncroEdPrevFirstCellSel;           Name: 'ecIdePSyncroEdPrevFirstCellSel'),

  // TSynPluginSyncroEdit - Out off cell
    (Value: ecIdePSyncroEdOutNextCell;                Name: 'ecIdePSyncroEdOutNextCell'),
    (Value: ecIdePSyncroEdOutNextCellSel;             Name: 'ecIdePSyncroEdOutNextCellSel'),
    (Value: ecIdePSyncroEdOutPrevCell;                Name: 'ecIdePSyncroEdOutPrevCell'),
    (Value: ecIdePSyncroEdOutPrevCellSel;             Name: 'ecIdePSyncroEdOutPrevCellSel'),
    (Value: ecIdePSyncroEdOutCellHome;                Name: 'ecIdePSyncroEdOutCellHome'),
    (Value: ecIdePSyncroEdOutCellEnd;                 Name: 'ecIdePSyncroEdOutCellEnd'),
    (Value: ecIdePSyncroEdOutCellSelect;              Name: 'ecIdePSyncroEdOutCellSelect'),
    (Value: ecIdePSyncroEdOutEscape;                  Name: 'ecIdePSyncroEdOutEscape'),
    (Value: ecIdePSyncroEdOutNextFirstCell;           Name: 'ecIdePSyncroEdOutNextFirstCell'),
    (Value: ecIdePSyncroEdOutNextFirstCellSel;        Name: 'ecIdePSyncroEdOutNextFirstCellSel'),
    (Value: ecIdePSyncroEdOutPrevFirstCell;           Name: 'ecIdePSyncroEdOutPrevFirstCell'),
    (Value: ecIdePSyncroEdOutPrevFirstCellSel;        Name: 'ecIdePSyncroEdOutPrevFirstCellSel'),

  // TSynPluginSyncroEdit - selecting
    (Value: ecIdePSyncroEdSelStart; Name: 'ecIdePSyncroEdSelStart')
  );

function IdentToIDECommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, IDEEditorCommandStrs);
end;

function IDECommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := IntToIdent(Cmd, Ident, IDEEditorCommandStrs);
end;

function IDECommandToIdent(Cmd: longint): string;
begin
  Result := '';
  if not IDECommandToIdent(Cmd, Result) then
    raise Exception.CreateFmt('IDECommandToIdent: command %d not found', [Cmd]);
end;

procedure GetIDEEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(IDEEditorCommandStrs) to High(IDEEditorCommandStrs) do
    Proc(IDEEditorCommandStrs[I].Name);
end;

end.

