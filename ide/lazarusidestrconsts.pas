{  $Id$  }
{
 /***************************************************************************
                          lazarusidestrconsts.pas
                          -----------------------
              This unit contains all resource strings of the IDE


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
{
  Note: All resource strings should be prefixed with 'lis'

}
unit LazarusIDEStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
resourcestring
  // version
  lisLazarusVersionString = '0.8.5 alpha';

  // command line help
  lisCmdLineHlpHeader = 'lazarus [options] <project-filename>' + LineEnding
              + LineEnding
              + 'IDE Options:' + LineEnding
              + LineEnding
              + '--help or -?             this help message' + LineEnding
              + LineEnding;

  lisCmdLinePrimaryConfigPathDesc =
     '--primary-config-path <path>' + LineEnding
    +'                         primary config directory, where Lazarus' + LineEnding
    +'                         stores its config files. Default is ' + LineEnding
    +'                         %s' + LineEnding
    + LineEnding;
    
  lisCmdLineSecondaryConfigPathDesc =
     '--secondary-config-path <path>' + LineEnding
    +'                         secondary config directory, where Lazarus' + LineEnding
    +'                         searches for config template files.' + LineEnding
    +'                         Default is %s' + LineEnding
    + LineEnding;
    
  lisCmdLineLCLInterfaceSpecificOptions =
    'LCL Interface specific options:';

  // component palette
  lisSelectionTool = 'Selection tool';
  
  // macros
  lisCursorColumnInCurrentEditor = 'Cursor column in current editor';
  lisCursorRowInCUrrentEditor = 'Cursor row in current editor';
  lisCompilerFilename = 'Compiler filename';
  lisWordAtCursorInCurrentEditor = 'Word at cursor in current editor';
  lisExpandedFilenameOfCurrentEditor = 'Expanded filename of current editor file';
  lisFreePascalSourceDirectory = 'Freepascal source directory';
  lisLazarusDirectory = 'Lazarus directory';
  lisLCLWidgetType = 'LCL Widget Type';
  lisCommandLineParamsOfProgram = 'Command line parameters of program';
  lisPromptForValue = 'Prompt for value';
  lisProjectFilename = 'Project filename';
  lisProjectDirectory = 'Project directory';
  lisSaveCurrentEditorFile = 'save current editor file';
  lisSaveAllModified = 'save all modified files';
  lisTargetFilenameOfProject = 'Target filename of project';
  lisTargetFilenamePlusParams = 'Target filename + params';
  lisTestDirectory = 'Test directory';
  lisLaunchingCmdLine = 'Launching target command line';
  lisPublishProjDir = 'Publish project directory';

  // main bar menu
  lisMenuFile = '&File';
  lisMenuEdit = '&Edit';
  lisMenuSearch = '&Search';
  lisMenuView = '&View';
  lisMenuProject = '&Project';
  lisMenuRun = '&Run';
  lisMenuComponents = '&Components';
  lisMenuTools = '&Tools';
  lisMenuEnvironent = 'E&nvironment';
  lisMenuWindows = '&Windows';
  lisMenuHelp = '&Help';
  
  lisMenuNewUnit = 'New Unit';
  lisMenuNewForm = 'New Form';
  lisMenuNewOther = 'New ...';
  lisMenuOpen = 'Open';
  lisMenuRevert = 'Revert';
  lisMenuOpenRecent = 'Open Recent';
  lisMenuSave = 'Save';
  lisMenuSaveAs = 'Save As';
  lisMenuSaveAll = 'Save All';
  lisMenuClose = 'Close';
  lisMenuCloseAll = 'Close all editor files';
  lisMenuQuit = 'Quit';
  
  lisMenuUndo = 'Undo';
  lisMenuRedo = 'Redo';
  lisMenuCut = 'Cut';
  lisMenuCopy = 'Copy';
  lisMenuPaste = 'Paste';
  lisMenuIndentSelection = 'Indent selection';
  lisMenuUnindentSelection = 'Unindent selection';
  lisMenuUpperCaseSelection = 'Uppercase selection';
  lisMenuLowerCaseSelection = 'Lowercase selection';
  lisMenuTabsToSpacesSelection = 'Tabs to spaces in selection';
  lisMenuCommentSelection = 'Comment selection';
  lisMenuUncommentSelection = 'Uncomment selection';
  lisMenuSortSelection = 'Sort selection';
  lisMenuSelect = 'Select';
  lisMenuSelectAll = 'Select all';
  lisMenuSelectToBrace = 'Select to brace';
  lisMenuSelectCodeBlock = 'Select code block';
  lisMenuSelectLine = 'Select line';
  lisMenuSelectParagraph = 'Select paragraph';
  lisMenuInsertText = 'Insert text';
  lisMenuInsertCVSKeyword = 'CVS keyword';
  lisMenuInsertGeneral = 'General';
  lisMenuCompleteCode = 'Complete Code';

  lisMenuInsertGPLNotice = 'GPL notice';
  lisMenuInsertLGPLNotice = 'LGPL notice';
  lisMenuInsertUserName = 'Current username';
  lisMenuInsertDateTime = 'Current date and time';
  lisMenuInsertChangeLogEntry = 'ChangeLog entry';

  lisMenuFind = 'Find';
  lisMenuFindNext = 'Find &Next';
  lisMenuFindPrevious = 'Find &Previous';
  lisMenuFindInFiles = 'Find &in files';
  lisMenuReplace = 'Replace';
  lisMenuIncrementalFind = 'Incremental Find';
  lisMenuGotoLine = 'Goto line';
  lisMenuJumpBack = 'Jump back';
  lisMenuJumpForward = 'Jump forward';
  lisMenuAddJumpPointToHistory = 'Add jump point to history';
  lisMenuViewJumpHistory = 'View Jump-History';
  lisMenuFindBlockOtherEndOfCodeBlock = 'Find other end of code block';
  lisMenuFindCodeBlockStart = 'Find code block start';
  lisMenuFindDeclarationAtCursor = 'Find Declaration at cursor';
  lisMenuOpenFilenameAtCursor = 'Open filename at cursor';
  lisMenuGotoIncludeDirective = 'Goto include directive';
  
  lisMenuViewObjectInspector = 'Object Inspector';
  lisMenuViewProjectExplorer = 'Project Explorer';
  lisMenuViewCodeExplorer = 'Code Explorer';
  lisMenuViewUnits = 'Units...';
  lisMenuViewForms = 'Forms...';
  lisMenuViewUnitDependencies = 'View Unit Dependencies';
  lisMenuViewToggleFormUnit = 'Toggle form/unit view';
  lisMenuViewMessages = 'Messages';
  lisMenuDebugWindows = 'Debug windows';
  lisMenuViewWatches = 'Watches';
  lisMenuViewBreakPoints = 'BreakPoints';
  lisMenuViewLocalVariables = 'Local Variables';
  lisMenuViewCallStack = 'Call Stack';
  lisMenuViewDebugOutput = 'Debug output';
  
  lisMenuNewProject = 'New Project';
  lisMenuNewProjectFromFile = 'New Project from file';
  lisMenuOpenProject = 'Open Project';
  lisMenuOpenRecentProject = 'Open Recent Project';
  lisMenuSaveProject = 'Save Project';
  lisMenuSaveProjectAs = 'Save Project As...';
  lisMenuPublishProject = 'Publish Project';
  lisMenuAddUnitToProject = 'Add active unit to Project';
  lisMenuRemoveUnitFromProject = 'Remove from Project';
  lisMenuViewSource = 'View Source';
  lisMenuViewProjectTodos = 'View ToDo List';
  lisMenuProjectOptions = 'Project Options...';
  
  lisMenuBuild = 'Build';
  lisMenuBuildAll = 'Build all';
  lisMenuProjectRun = 'Run';
  lisMenuPause = 'Pause';
  lisMenuStepInto = 'Step into';
  lisMenuStepOver = 'Step over';
  lisMenuRunToCursor = 'Run to cursor';
  lisMenuStop = 'Stop';
  lisMenuCompilerOptions = 'Compiler Options...';
  lisMenuRunParameters = 'Run Parameters ...';
  
  lisMenuConfigCustomComps = 'Configure custom components';
  lisMenuEditPackage = 'Edit package';
  lisMenuOpenRecentPkg = 'Open recent package';
  lisMenuOpenPackageFile = 'Open package file';
  lisMenuPackageGraph = 'Package Graph';

  lisMenuSettings = 'Configure custom tools ...';
  lisMenuQuickSyntaxCheck = 'Quick syntax check';
  lisMenuGuessUnclosedBlock = 'Guess unclosed block';
  lisMenuGuessMisplacedIFDEF = 'Guess misplaced IFDEF/ENDIF';
  lisMenuMakeResourceString = 'Make Resource String';
  lisMenuDiff = 'Diff';
  lisMenuConvertDFMtoLFM = 'Convert DFM file to LFM';
  lisMenuBuildLazarus = 'Build Lazarus';
  lisMenuConfigureBuildLazarus = 'Configure "Build Lazarus"';
  
  lisMenuGeneralOptions = 'Environment options';
  lisMenuEditorOptions = 'Editor options';
  lisMenuCodeToolsOptions = 'CodeTools options';
  lisMenuCodeToolsDefinesEditor = 'CodeTools defines editor';
  
  lisMenuAboutLazarus = 'About Lazarus';

  // resource files
  lisResourceFileComment =
    'This is an automatically generated lazarus resource file';

  // file dialogs
  lisOpenFile = 'Open file';
  lisOpenProjectFile = 'Open Project File';
  lisOpenPackageFile = 'Open Package File';
  lisSaveSpace = 'Save ';
  lisSelectDFMFiles = 'Select Delphi form files (*.dfm)';
  lisChooseDirectory = 'Choose directory';

  // dialogs
  lisSaveChangesToProject = 'Save changes to project %s?';
  lisProjectChanged = 'Project changed';

  lisFPCSourceDirectoryError = 'FPC Source Directory error';
  lisPLzCheckTheFPCSourceDirectory = 'Please check the freepascal source directory';
  lisCompilerError = 'Compiler error';
  lisPlzCheckTheCompilerName = 'Please check the compiler name';
  lisAboutLazarus = 'About Lazarus';
  lisVersion = 'Version';
  lisClose = 'Close';
  lisAboutLazarusMsg =
       'License: GPL/LGPL'
      +'%s'
      +'Lazarus are the class libraries for Free Pascal that '
      +'emulate Delphi. Free Pascal is a (L)GPL''ed compiler that '
      +'runs on Linux, Win32, OS/2, 68K and more. Free Pascal '
      +'is designed to be able to understand and compile Delphi '
      +'syntax, which is of course OOP.'
      +'%s'
      +'Lazarus is the missing part of the puzzle that will allow '
      +'you to develop Delphi like programs in all of the above '
      +'platforms. The IDE will eventually become a RAD tool like '
      +'Delphi.'
      +'%s'
      +'As Lazarus is growing we need more developers.';
  lisUnitNameAlreadyExistsCap = 'Unitname already in project';
  lisUnitNameAlreadyExistsText = 'The unit "%s" already exists.' + LineEnding
       +'Ignore will force the renaming,' + LineEnding
       +'Cancel will cancel the saving of this source and' + LineEnding
       +'Abort will abort the whole saving.';
  lisInvalidPascalIdentifierCap = 'Invalid Pascal Identifier';
  lisInvalidPascalIdentifierText =
    'The name "%s" is not a valid pascal identifier.';
  lisCopyError = 'Copy Error';

  // hints
  lisHintNewUnit = 'New Unit';
  lisHintOpen = 'Open';
  lisHintSave = 'Save';
  lisHintSaveAll = 'Save all';
  lisHintNewForm = 'New Form';
  lisHintToggleFormUnit = 'Toggle Form/Unit';
  lisHintViewUnits = 'View Units';
  lisHintViewForms = 'View Forms';
  lisHintRun = 'Run';
  lisHintPause = 'Pause';
  lisHintStepInto = 'Step Into';
  lisHintStepOver = 'Step Over';
  
  lisGPLNotice =
    'Copyright (C) <year> <name of author>'
   +'%s'
   +'This program is free software; you can redistribute it and/or modify '
   +'it under the terms of the GNU General Public License as published by '
   +'the Free Software Foundation; either version 2 of the License, or '
   +'(at your option) any later version. '
   +'%s'
   +'This program is distributed in the hope that it will be useful, '
   +'but WITHOUT ANY WARRANTY; without even the implied warranty of '
   +'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the '
   +'GNU Library General Public License for more details. '
   +'%s'
   +'You should have received a copy of the GNU General Public License '
   +'along with this program; if not, write to the Free Software '
   +'Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.';
   
  lisLGPLNotice =
    'Copyright (C) <year> <name of author>'
   +'%s'
   +'This library is free software; you can redistribute it and/or modify '
   +'it under the terms of the GNU Library General Public License as published '
   +'by the Free Software Foundation; either version 2 of the License, or '
   +'(at your option) any later version. '
   +'%s'
   +'This program is distributed in the hope that it will be useful, '
   +'but WITHOUT ANY WARRANTY; without even the implied warranty of '
   +'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the '
   +'GNU Library General Public License for more details. '
   +'%s'
   +'You should have received a copy of the GNU Library General Public License '
   +'along with this library; if not, write to the Free Software '
   +'Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.';


  //IDE components
  ideStandard = 'Standard';
  ideAdditional = 'Additional';
  ideMisc = 'Misc';
  ideSystem = 'System';
  ideDialogs = 'Dialogs';
  //I''ve skipped samples as sample components are usually placed there.
  //Unfortunately, not in lazarus now... It might be a bad idea to have two
  //palletes, for example 'ðÒÉÍÅÒÙ' and 'Samples'
  ideDataAccess = 'Data Access';
  ideInterbase = 'Interbase Data Access';

  //Environment dialog
  dlgBakDirectory='(no subdirectoy)';
  
  //dlgEnvOpts = 'Environment Options';  = lisMenuGeneralOptions
  dlgDesktop = 'Desktop';
  dlgFrmEditor = 'Form Editor';
  dlgObjInsp = 'Object Inspector';
  dlgEnvFiles = 'Files';
  dlgEnvBckup = 'Backup';
  dlgNaming = 'Naming';
  dlgCancel = 'Cancel';
  dlgEnvLanguage = 'Language';
  dlgAutoSave = 'Auto save';
  dlgEdFiles = 'Editor files';
  dlgEnvProject = 'Project';
  dlgIntvInSec = 'Interval in secs';
  dlgDesktopFiles = 'Desktop files';
  dlgSaveDFile = 'Save desktop settings to file';
  dlgLoadDFile = 'Load desktop settings from file';
  dlgMinimizeAllOnMinimizeMain = 'Minimize all on minimize main';
  dlgPalHints = 'Hints for component palette';
  dlgSpBHints = 'Hints for main speed buttons (open, save, ...)';
  dlgWinPos = 'Window Positions';
  dlgMainMenu = 'Main Menu';
  dlgSrcEdit = 'Source Editor';
  dlgMsgs = 'Messages';
  dlgProjFiles = 'Project files';
  dlgEnvType = 'Type';
  dlgEnvNone = 'None';
  dlgSmbFront = 'Symbol in front (.~pp)';
  dlgSmbBehind = 'Symbol behind (.pp~)';
  dlgSmbCounter = 'Counter (.pp;1)';
  dlgCustomExt = 'User defined extension (.pp.xxx)';
  dlgBckUpSubDir = 'Same name (in subdirectory)';
  dlgEdCustomExt = 'User defined extension';
  dlgMaxCntr = 'Maximum counter';
  dlgEdBSubDir = 'Sub directory';
  dlgEnvOtherFiles = 'Other files';
  dlgMaxRecentFiles = 'Max recent files';
  dlgMaxRecentProjs = 'Max recent project files';
  dlgQOpenLastPrj = 'Open last project at start';
  dlgLazarusDir = 'Lazarus directory (default for all projects)';
  dlgFpcPath = 'Compiler path (ppc386)';
  dlgFpcSrcPath = 'FPC source directory';
  dlgDebugType = 'Debugger type and path';
  dlgTestPrjDir = 'Directory for building test projects';
  dlgQShowGrid = 'Show grid';
  dlgGridColor = 'Grid color';
  dlgQSnapToGrid = 'Snap to grid';
  dlgGridX = 'Grid size X';
  dlgGridY = 'Grid size Y';
  dlgGuideLines = 'Show Guide Lines';
  dlgSnapGuideLines = 'Snap to Guide Lines';
  dlgLeftTopClr = 'color for left, top';
  dlgRightBottomClr = 'color for right, bottom';
  dlgShowCaps = 'Show component captions';
  dlgShowEdrHints = 'Show editor hints';
  dlgAutoForm = 'Auto create form when opening unit';
  dlgGrabberColor = 'Grabber color';
  dlgMarkerColor = 'Marker color';
  dlgEnvGrid = 'Grid';
  dlgEnvLGuideLines = 'Guide lines';
  dlgEnvMisc = 'Miscellaneous';
  dlgRuberbandSelectionColor = 'Selection';
  dlgRuberbandCreationColor = 'Creation';
  dlgRubberbandSelectsGrandChilds = 'Select grand childs';
  dlgRubberBandGroup='Rubber band';
  dlgPasExt = 'Default pascal extension';
  dlgPasAutoLower = '%sSave As%s always saves pascal files lowercase';
  dlgPasAskLower = '%sSave As%s asks to save pascal files lowercase';
  dlgAmbigFileAct = 'Ambigious file action:';
  dlgEnvAsk = 'Ask';
  dlgAutoDel = 'Auto delete file';
  dlgAutoRen = 'Auto rename file';
  dlgAmbigWarn = 'Warn on compile';
  dlgIgnoreVerb = 'Ignore';
  dlgBackColor = 'Background color';
  dlgOIMiscellaneous = 'Miscellaneous';
  dlgOIItemHeight = 'Item height';
  dlgEnvColors = 'Colors';
  dlgEnvBackupHelpNote =
    'Notes: Project files are all files in the project directory';
  lisEnvOptDlgInvalidCompilerFilename = 'Invalid compiler filename';
  lisEnvOptDlgInvalidCompilerFilenameMsg =
    'The compiler file "%s" is not an executable.';
  lisEnvOptDlgInvalidDebuggerFilename = 'Invalid debugger filename';
  lisEnvOptDlgInvalidDebuggerFilenameMsg =
    'The debugger file "%s" is not an executable.';
  lisEnvOptDlgDirectoryNotFound = 'Directory not found';
  lisEnvOptDlgLazarusDirNotFoundMsg = 'Lazarus directory "%s" not found.';
  lisEnvOptDlgInvalidLazarusDir = 'The lazarus directory "%s" does not look correct.'
    +' Normally it contains directories like lcl, debugger, designer, components, ... .';
  lisEnvOptDlgFPCSrcDirNotFoundMsg = 'FPC source directory "%s" not found.';
  lisEnvOptDlgInvalidFPCSrcDir = 'The FPC source directory "%s" does not look correct.'
    +' Normally it contains directories like rtl, fcl, packages, compiler, ... .';
  lisEnvOptDlgTestDirNotFoundMsg = 'Test directory "%s" not found.';

  // editor options
  //dlgEdOptsCap = 'Editor Options'; = lismenueditoroptions
  dlgEdDisplay = 'Display';
  dlgKeyMapping = 'Key Mappings';
  dlgEdColor = 'Color';
  dlgKeyMappingErrors = 'Key mapping errors';
  dlgEdBack = 'Back';
  dlgReport = 'Report';
  dlgEdNoErr = 'No errors in key mapping found.';
  dlgDelTemplate = 'Delete template ';
  dlgChsCodeTempl = 'Choose code template file (*.dci)';
  dlgAllFiles = 'All files';
  dlgAltSetClMode = 'Alt Sets Column Mode';
  dlgAutoIdent = 'Auto Indent';
  dlgBracHighlight = 'Bracket Highlight';
  dlgDragDropEd = 'Drag Drop Editing';
  dlgDropFiles = 'Drop Files';
  dlgHalfPageScroll = 'Half Page Scroll';
  dlgKeepCaretX = 'Keep Caret X';
  dlgPersistentCaret = 'Persistent Caret';
  dlgScrollByOneLess = 'Scroll By One Less';
  dlgScrollPastEndFile = 'Scroll Past End of File';
  dlgScrollPastEndLine = 'Scroll Past End of Line';
  dlgCloseButtonsNotebook = 'Show Close Buttons in notebook';
  dlgShowScrollHint = 'Show Scroll Hint';
  dlgMouseLinks = 'Mouse links';
  dlgSmartTabs = 'Smart Tabs';
  dlgTabsToSpaces = 'Tabs To Spaces';
  dlgTrimTrailingSpaces = 'Trim Trailing Spaces';
  dlgUndoAfterSave = 'Undo after save';
  dlgDoubleClickLine = 'Double click line';
  dlgFindTextatCursor = 'Find text at cursor';
  dlgUseSyntaxHighlight = 'Use syntax highlight';
  dlgBlockIndent = 'Block indent:';
  dlgUndoLimit = 'Undo limit:';
  dlgTabWidths = 'Tab widths:';
  dlgMarginGutter = 'Margin and gutter';//What is gutter?
  dlgVisibleRightMargin = 'Visible right margin';
  dlgVisibleGutter = 'Visible gutter';//I know only about fish guts... :( :)
  dlgShowLineNumbers = 'Show line numbers';
  dlgRightMargin = 'Right margin';
  dlgRightMarginColor = 'Right margin color';
  dlgGutterWidth = 'Gutter width';// as I am food technology bachelor
  dlgGutterColor = 'Gutter color';// and fish technology engineer :) - VVI
  dlgEditorFont = 'Editor font';
  dlgDefaultEditorFont='Default editor font';
  dlgEditorFontHeight = 'Editor font height';
  dlgExtraLineSpacing = 'Extra line spacing';
  dlgKeyMappingScheme = 'Key Mapping Scheme';
  dlgCheckConsistency = 'Check consistency';
  dlgEdHintCommand = 'Hint: click on the command you want to edit';
  dlgLang = 'Language:';
  dlgClrScheme = 'Color Scheme:';
  dlgFileExts = 'File extensions:';
  dlgEdElement = 'Element';
  dlgSetElementDefault = 'Set element to default';
  dlgSetAllElementDefault = 'Set all elements to default';
  dlgForecolor = 'Foreground color';
  dlgEdUseDefColor = 'Use default color';
  dlgTextAttributes = 'Text attributes';
  dlgEdBold = 'Bold';
  dlgEdItal = 'Italic';
  dlgEdUnder = 'Underline';
  dlgEdIdComlet = 'Identifier completion';
  dlgEdCodeParams = 'Code parameters';
  dlgTooltipEval = 'Tooltip expression evaluation';
  dlgTooltipTools = 'Tooltip symbol Tools';
  dlgEdDelay = 'Delay';
  dlgTimeSecondUnit = 'sec';
  dlgEdCodeTempl = 'Code templates';
  dlgTplFName = 'Template file name';
  dlgEdAdd = 'Add...';
  dlgEdEdit = 'Edit...';
  dlgEdDelete = 'Delete';
  dlgIndentCodeTo = 'Indent code to';
  dlgCodeToolsTab = 'Code Tools';
  
  //CodeTools dialogue
  dlgCodeToolsOpts = 'CodeTools Options';
  dlgCodeCreation = 'Code Creation';
  dlgWordsPolicies = 'Words';
  dlgLineSplitting = 'Line Splitting';
  dlgSpaceNotCosmos{:)} = 'Space';
  dlgAdditionalSrcPath = 'Additional Source search path for all projects (.pp;.pas)';
  dlgJumpingETC = 'Jumping (e.g. Method Jumping)';
  dlgAdjustTopLine = 'Adjust top line due to comment in front';
  dlgCenterCursorLine = 'Center Cursor Line';
  dlgCursorBeyondEOL = 'Cursor beyond EOL';
  dlgClassInsertPolicy = 'Class part insert policy';
  dlgAlphabetically = 'Alphabetically';
  dlgCDTLast = 'Last';
  dlgMixMethodsAndProperties = 'Mix methods and properties';
  dlgForwardProcsInsertPolicy = 'Procedure insert policy';
  dlgLast = 'Last (i.e. at end of source)';
  dlgInFrontOfMethods = 'In front of methods';
  dlgBehindMethods = 'Behind methods';
  dlgForwardProcsKeepOrder = 'Keep order of procedures';
  dlgMethodInsPolicy = 'Method insert policy';
  dlgCDTClassOrder = 'Class order';
  dlgKeywordPolicy = 'Keyword policy';
  dlgCDTLower = 'lowercase';
  dlgCDTUPPERCASE = 'UPPERCASE';
  dlg1UP2low = 'Lowercase, first letter up';
  dlgIdentifierPolicy = 'Identifier policy';
  dlgPropertyCompletion = 'Property completion';
  dlgCompleteProperties = 'Complete properties';
  dlgCDTReadPrefix = 'Read prefix';
  dlgCDTWritePrefix = 'Write prefix';
  dlgCDTStoredPostfix = 'Stored postfix';
  dlgCDTVariablePrefix = 'Variable prefix';
  dlgSetPropertyVariable = 'Set property Variable';
  dlgMaxLineLength = 'Max line length:';
  dlgNotSplitLineFront = 'Do not split line In front of:';
  dlgNotSplitLineAfter = 'Do not split line after:';
  dlgCDTPreview = 'Preview (Max line length = 1)';
  dlgInsSpaceFront = 'Insert space in front of';
  dlgInsSpaceAfter = 'Insert space after';
  dlgWRDPreview = 'Preview';

  locwndSrcEditor = 'Lazarus Source Editor';
  
  // compiler options
  dlgCompilerOptions = 'Compiler Options';
  dlgSearchPaths = 'Search Paths';
  dlgCOParsing = 'Parsing';
  dlgCodeGeneration = 'Code Generation';
  dlgCOLinking = 'Linking';
  dlgCOMessages = 'Messages';
  dlgCOOther = 'Other';
  dlgShowCompilerOptions = 'Show compiler options';
  dlgCOOpts = 'Options: ';
  dlgCOStyle = 'Style:';
  dlgCOAsIs = 'As-Is';
  dlgSymantecChecking = 'Symantec Checking:';
  dlgDelphi2Ext = 'Delphi 2 Extensions';
  dlgCOCOps = 'C Style Operators (*=, +=, /= and -=)';
  dlgAssertCode = 'Include Assertion Code';
  dlgLabelGoto = 'Allow LABEL and GOTO';
  dlgCppInline = 'C++ Styled INLINE';
  dlgCMacro = 'C Style Macros (global)';
  dlgBP7Cptb = 'TP/BP 7.0 Compatible';
  dlgInitDoneOnly = 'Constructor name must be ''' + 'init' + ''' (destructor must be ''' + 'done' + ''')';
  dlgStaticKeyword = 'Static Keyword in Objects';
  dlgDeplhiComp = 'Delphi Compatible';
  dlgCOAnsiStr = 'Use Ansi Strings';
  dlgGPCComp = 'GPC (GNU Pascal Compiler) Compatible';
  dlgCOUnitStyle = 'Unit Style:';
  dlgStatic = 'Static';
  dlgDynamic = 'Dynamic';
  dlgCOSmart = 'Smart';
  dlgCOChecks = 'Checks:';
  dlgCORange = 'Range';
  dlgCOOverflow = 'Overflow';
  dlgCOStack = 'Stack';
  dlgHeapSize = 'Heap Size';
  dlgCOGenerate = 'Generate:';
  dlgCOFast = 'Faster Code';
  dlgCOSmaller = 'Smaller Code';
  dlgTargetProc = 'Target Processor:';
  dlgOptimiz = 'Optimizations:';
  dlgCOKeepVarsReg = 'Keep certain variables in registers';
  dlgUncertOpt = 'Uncertain Optimizations';
  dlgLevel1Opt = 'Level 1 (Quick Optimizations)';
  dlgLevel2Opt = 'Level 2 (Level 1 + Slower Optimizations)';
  dlgLevel3Opt = 'Level 3 (Level 2 + Uncertain)';
  dlgTargetOS = 'Target OS';
  dlgCODebugging = 'Debugging:';
  dlgCOGDB = 'Generate Debugging Info For GDB (Slows Compiling)';
  dlgCODBX = 'Generate Debugging Info For DBX (Slows Compiling)';
  dlgLNumsBct = 'Display Line Numbers in Run-time Error Backtraces';
  dlgCOHeaptrc = 'Use Heaptrc Unit';
  dlgGPROF = 'Generate code for gprof';
  dlgCOStrip = 'Strip Symbols From Executable';
  dlgLinkLibraries = 'Link Libraries:';
  dlgLinkDinLibs = 'Link With Dynamic Libraries';
  dlgLinkStatLibs = 'Link With Static Libraries';
  dlgLinkSmart = 'Link Smart';
  dlgPassOptsLinker = 'Pass Options To The Linker (Delimiter is space)';
  dlgVerbosity = 'Verbosity:';
  dlgCOShowErr = 'Show Errors';
  dlgShowWarnings = 'Show Warnings';
  dlgShowNotes = 'Show Notes';
  dlgShowHint = 'Show Hints';
  dlgShowGeneralInfo = 'Show General Info';
  dlgShowProcsError = 'Show all procs on error';
  dlgShowEverything ='Show Everything';
  dlgShowDebugInfo = 'Show Debug Info';
  dlgShowUsedFiles = 'Show Used Files';
  dlgShowTriedFiles = 'Show Tried Files';
  dlgShowDefinedMacros = 'Show Defined Macros';
  dlgShowCompiledProcedures = 'Show Compiled Procedures';
  dlgShowConditionals = 'Show Conditionals';
  dlgShowNothing = 'Show Nothing (only errors)';
  dlgWriteFPCLogo = 'Write an FPC Logo';
  dlgHintsUnused = 'Show Hints for unused project units';
  dlgConfigFiles = 'Config Files:';
  dlgUseFpcCfg = 'Use Compiler Config File (fpc.cfg)';
  dlgUseAdditionalConfig = 'Use Additional Compiler Config File';
  dlgStopAfterNrErr = 'Stop after number of errors:';
  dlgOtherUnitFiles = 'Other Unit Files (Delimiter is semicolon):';
  dlgCOIncFiles = 'Include Files:';
  dlgCOSources = 'Other Sources:  (.pp/.pas files)';
  dlgCOLibraries = 'Libraries:';
  dlgToFPCPath = 'Path To Compiler:';
  dlgUnitOutp = 'Unit output directory:';
  dlgLCLWidgetType = 'LCL Widget Type';
  dlgButApply = 'Apply';
  dlgCOShowOptions = 'Show Options';
  dlgMainViewForms = 'View project forms';
  dlgMainViewUnits = 'View project units';
  dlgMulti = 'Multi';
  
  // project options dialog
  dlgProjectOptions = 'Project Options';
  dlgPOApplication = 'Application';
  dlgPOFroms = 'Forms';
  dlgPOInfo = 'Info';
  dlgApplicationSettings = 'Application Settings';
  dlgPOTitle = 'Title:';
  dlgPOOutputSettings = 'Output Settings';
  dlgPOTargetFileName = 'Target file name:';
  dlgAutoCreateForms = 'Auto-create forms:';
  dlgAvailableForms = 'Available forms:';
  dlgAutoCreateNewForms = 'When creating new forms, add them to auto-created forms';
  dlgSaveEditorInfo = 'Save editor info for closed files';
  dlgSaveEditorInfoProject = 'Save editor info only for project files';
  dlgRunParameters = 'Run parameters';
  dlgRunOLocal = 'Local';
  dlgRunOEnvironment = 'Environment';
  dlgHostApplication = 'Host application';
  dlgCommandLineParams = 'Command line parameters (without application name)';
  dlgUseLaunchingApp = 'Use launching application';
  dlgROWorkingDirectory = 'Working directory';
  dlgRunODisplay = 'Display (not for win32, e.g. 198.112.45.11:0, x.org:1, hydra:0.1)';
  dlgRunOUsedisplay = 'Use display';
  dlgRunOSystemVariables = 'System variables';
  dlgRunOVariable = 'Variable';
  dlgRunOValue = 'Value';
  dlgRunOUserOverrides = 'User overrides';
  dlgIncludeSystemVariables = 'Include system variables';
  dlgDirectoryDoesNotExist = 'Directory does not exist';
  dlgTheDirectory = 'The directory "';
  dlgDoesNotExist = '" does not exist.';
  dlgTextToFing = '&Text to Find';
  dlgReplaceWith = '&Replace With';
  dlgFROpts = 'Options';
  dlgCaseSensitive = 'Case Sensitive';
  dlgWholeWordsOnly = 'Whole Words Only';
  dlgRegularExpressions = 'Regular Expressions';
  dlgMultiLine = 'Multi Line';
  dlgPromptOnReplace = 'Prompt On Replace';
  dlgSROrigin = 'Origin';
  dlgFromCursor = 'From Cursor';
  dlgEntireScope = 'Entire Scope';
  dlgScope = 'Scope';
  dlgGlobal = 'Global';
  dlgSelectedText = 'Selected Text';
  dlgDirection = 'Direction';
  dlgUpWord = 'Up';
  dlgDownWord = 'Down';
  dlgReplaceAll = 'Replace All';
  
  //IDEOptionDefs
  dlgGetPosition = 'Get position';
  dlgLeftPos     = 'Left:';
  dlgWidthPos    = 'Width:';
  dlgTopPos      = 'Top:';
  DlgHeightPos   = 'Height:';
  rsiwpUseWindowManagerSetting = 'Use windowmanager setting';
  rsiwpDefault                 = 'Default';
  rsiwpRestoreWindowGeometry   = 'Restore window geometry';
  rsiwpDocked                  = 'Docked';
  rsiwpCustomPosition          = 'Custom position';
  rsiwpRestoreWindowSize       = 'Restore window size';

  // Code Explorer
  lisCodeExplorer = 'Code Explorer';

  // Unit editor
  uemFindDeclaration = '&Find Declaration';
  uemOpenFileAtCursor = '&Open file at cursor';
  uemClosePage = '&Close Page';
  uemGotoBookmark = '&Goto Bookmark';
  uemBookmarkN = 'Bookmark';
  uemSetBookmark = '&Set Bookmark';
  uemReadOnly = 'Read Only';
  uemUnitInfo = 'Unit Info';
  uemDebugWord = 'Debug';
  uemToggleBreakpoint = '&Toggle Breakpoint';
  uemAddWatchAtCursor = '&Add Watch At Cursor';
  uemRunToCursor='&Run to Cursor';
  uemMoveEditorLeft='Move Editor Left';
  uemMoveEditorRight='Move Editor Right';
  uemEditorproperties='Editor properties';
  ueNotImplCap='Not implemented yet';
  ueNotImplText='If You can help us to implement this feature, mail to'#13
   +'lazarus@miraclec.com';
  ueNotImplCapAgain='I told You: Not implemented yet';
  ueFileROCap= 'File is readonly';
  ueFileROText1='The file "';
  ueFileROText2='" is not writable.';
  ueModified='Modified';
  uepReadonly= 'Readonly';
  uepIns='INS';
  uepOvr='OVR';

  // Form designer
  fdInvalidMutliselectionCap='Invalid mutliselection';
  fdInvalidMutliselectionText='Multiselected components must be of a single form.';
  fdmAlignWord='Align';
  fdmMirrorHorizontal='Mirror horizontal';
  fdmMirrorVertical='Mirror vertical';
  fdmScaleWord='Scale';
  fdmSizeWord='Size';
  fdmBringTofront='Bring to front';
  fdmSendtoback='Send to back';
  fdmDeleteSelection='Delete selection';
  fdmSnapToGridOption='Option: Snap to grid';
  fdmSnapToGuideLinesOption='Option: Snap to guide lines';
  fdmShowOptions='Show Options for form editing';

  //-----------------------
  // keyMapping
  //
  srkmEditKeys ='Edit Keys';
  srkmCommand  = 'Command ';
  srkmConflic  = 'Conflict ';
  srkmConflicW = ' conflicts with ';
  srkmCommand1 = '    command1 "';
  srkmCommand2 = '    command2 "';
  srkmEditForCmd='Edit keys for command';
  srkmKey      = 'Key';
  srkmGrabKey  = 'Grab Key';
  srkmPressKey = 'Please press a key ...';
  srkmAlternKey= 'Alternative Key';
  srkmAlreadyConnected = ' The key "%s" is already connected to "%s".';

  //Commands
  srkmecWordLeft              = 'Move cursor word left';
  srkmecWordRight             = 'Move cursor word right';
  srkmecLineStart             = 'Move cursor to line start';
  srkmecLineEnd               = 'Move cursor to line end';
  srkmecPageUp                = 'Move cursor up one page';
  srkmecPageDown              = 'Move cursor down one page';
  srkmecPageLeft              = 'Move cursor left one page';
  srkmecPageRight             = 'Move cursor right one page';
  srkmecPageTop               = 'Move cursor to top of page';
  srkmecPageBottom            = 'Move cursor to bottom of page';
  srkmecEditorTop             = 'Move cursor to absolute beginning';
  srkmecEditorBottom          = 'Move cursor to absolute end';
  srkmecGotoXY                = 'Goto XY';
  srkmecSelLeft               = 'SelLeft';
  srkmecSelRight              = 'SelRight';
  srkmecSelUp                 = 'Select Up';
  srkmecSelDown               = 'Select Down';
  srkmecSelWordLeft           = 'Select Word Left';
  srkmecSelWordRight          = 'Select Word Right';
  srkmecSelLineStart          = 'Select Line Start';
  srkmecSelLineEnd            = 'Select Line End';
  srkmecSelPageUp             = 'Select Page Up';
  srkmecSelPageDown           = 'Select Page Down';
  srkmecSelPageLeft           = 'Select Page Left';
  srkmecSelPageRight          = 'Select Page Right';
  srkmecSelPageTop            = 'Select Page Top';
  srkmecSelPageBottom         = 'Select Page Bottom';
  srkmecSelEditorTop          = 'Select to absolute beginning';
  srkmecSelEditorBottom       = 'Select to absolute end';
  srkmecSelGotoXY             = 'Select Goto XY';
  srkmecSelectAll             = 'Select All';
  srkmecDeleteLastChar        = 'Delete Last Char';
  srkmecDeletechar            = 'Delete char at cursor';
  srkmecDeleteWord            = 'Delete to end of word';
  srkmecDeleteLastWord        = 'Delete to start of word';
  srkmecDeleteBOL             = 'Delete to beginning of line';
  srkmecDeleteEOL             = 'Delete to end of line';
  srkmecDeleteLine            = 'Delete current line';
  srkmecClearAll              = 'Delete whole text';
  srkmecLineBreak             = 'Break line and move cursor';
  srkmecInsertLine            = 'Break line, leave cursor';
  srkmecChar                  = 'Char';
  srkmecImeStr                = 'Ime Str';
  srkmecCut                   = 'Cut selection to clipboard';
  srkmecCopy                  = 'Copy selection to clipboard';
  srkmecPaste                 = 'Paste clipboard to current position';
  srkmecScrollUp              = 'Scroll up one line';
  srkmecScrollDown            = 'Scroll down one line';
  srkmecScrollLeft            = 'Scroll left one char';
  srkmecScrollRight           = 'Scroll right one char';
  srkmecInsertMode            = 'Insert Mode';
  srkmecOverwriteMode         = 'Overwrite Mode';
  srkmecToggleMode            = 'Toggle Mode';
  srkmecBlockIndent           = 'Indent block';
  srkmecBlockUnindent         = 'Unindent block';
  srkmecShiftTab              = 'Shift Tab';
  srkmecMatchBracket          = 'Go to matching bracket';
  srkmecNormalSelect          = 'Normal selection mode';
  srkmecColumnSelect          = 'Column selection mode';
  srkmecLineSelect            = 'Line selection mode';
  srkmecAutoCompletion        = 'Code template completion';
  srkmecUserFirst             = 'User First';
  srkmecGotoMarker            = 'Go to Marker %d';
  srkmecSetMarker             = 'Set Marker %d';
  srkmecPeriod                = 'period';
  // sourcenotebook
  srkmecJumpToEditor          = 'Focus to source editor';
  srkmecNextEditor            = 'Go to next editor';
  srkmecPrevEditor            = 'Go to prior editor';
  srkmecMoveEditorLeft        = 'Move editor left';
  srkmecMoveEditorRight       = 'Move editor right';
  srkmecGotoEditor            = 'Go to editor %d';
  // file menu
  srkmecNew                   = 'New';
  srkmecNewUnit               = 'New unit';
  srkmecNewForm               = 'New form';
  srkmecSaveAs                = 'Save as';
  srkmecSaveAll               = 'Save all';
  srkmecCloseAll              = 'Close all';

  // edit menu
  srkmecSelectionTabs2Spaces  = 'Convert tabs to spaces in selection';
  srkmecInsertGPLNotice       = 'Insert GPL notice';
  srkmecInsertLGPLNotice      = 'Insert LGPL notice';
  srkmecInsertUserName        = 'Insert current username';
  srkmecInsertDateTime        = 'Insert current date and time';
  srkmecInsertChangeLogEntry  = 'Insert ChangeLog entry';
  srkmecInsertCVSAuthor       = 'Insert CVS keyword Author';
  srkmecInsertCVSDate         = 'Insert CVS keyword Date';
  srkmecInsertCVSHeader       = 'Insert CVS keyword Header';
  srkmecInsertCVSID           = 'Insert CVS keyword ID';
  srkmecInsertCVSLog          = 'Insert CVS keyword Log';
  srkmecInsertCVSName         = 'Insert CVS keyword Name';
  srkmecInsertCVSRevision     = 'Insert CVS keyword Revision';
  srkmecInsertCVSSource       = 'Insert CVS keyword Source';
  // search menu
  srkmecFind                      = 'Find text';
  srkmecFindNext                  = 'Find next';
  srkmecFindPrevious              = 'Find previous';
  srkmecFindInFiles               = 'Find in files';
  srkmecReplace                   = 'Replace text';
  srkmecFindProcedureDefinition   = 'Find procedure definiton';
  srkmecFindProcedureMethod       = 'Find procedure method';
  srkmecGotoLineNumber            = 'Go to line number';
  srkmecAddJumpPoint              = 'Add jump point';
  srkmecOpenFileAtCursor          = 'Open file at cursor';
  srkmecGotoIncludeDirective      = 'Go to to include directive of current include file';
  // view menu
  srkmecToggleFormUnit            = 'Switch between form and unit';
  srkmecToggleObjectInsp          = 'View Object Inspector';
  srkmecToggleProjectExpl         = 'View project explorer';
  srkmecTogglecodeExpl            = 'View code explorer';
  srkmecToggleMessages            = 'View messages';
  srkmecToggleWatches             = 'View watches';
  srkmecToggleBreakPoints         = 'View breakpoints';
  srkmecToggleDebuggerOut         = 'View debugger output';
  srkmecToggleLocals              = 'View local variables';
  srkmecTogglecallStack           = 'View call stack';
  srkmecViewUnits                 = 'View units';
  srkmecViewForms                 = 'View forms';
  srkmecViewUnitDependencies      = 'View unit dependencies';
  // codetools
  srkmecWordCompletion            = 'Word completion';
  srkmecCompletecode              = 'Complete code';
  srkmecSyntaxCheck               = 'Syntax check';
  srkmecGuessMisplacedIFDEF       = 'Guess misplaced $IFDEF';
  srkmecFindDeclaration           = 'Find declaration';
  srkmecFindBlockOtherEnd         = 'Find block other end';
  srkmecFindBlockStart            = 'Find block start';
  // project uuse menu resource

  // run menu
  srkmecBuild                     = 'build program/project';
  srkmecBuildAll                  = 'build all files of program/project';
  srkmecRun                       = 'run program';
  srkmecPause                     = 'pause program';
  srkmecStopProgram               = 'stop program';
  srkmecRunParameters             = 'run parameters';
  srkmecCompilerOptions           = 'compiler options';
  // tools menu
  srkmecExtToolSettings           = 'External tools settings';
  srkmecBuildLazarus              = 'Build lazarus';
  srkmecExtTool                   = 'External tool %d';
  // environment menu
  srkmecEnvironmentOptions        = 'General environment options';
  srkmecCodeToolsOptions          = 'Codetools options';
  srkmecCodeToolsDefinesEd        = 'Codetools defines editor';
  srkmecMakeResourceString        = 'Make resource string';
  srkmecDiff                      = 'Diff';
  // help menu
  srkmecunknown                   = 'unknown editor command';
   
  //Key strings
  srVK_UNKNOWN    = 'Unknown';
  srVK_LBUTTON    = 'Mouse Button Left';
  srVK_RBUTTON    = 'Mouse Button Right';
  //srVK_CANCEL     = 'Cancel'; = dlgCancel
  srVK_MBUTTON    = 'Mouse Button Middle';
  srVK_BACK       = 'Backspace';
  srVK_TAB        = 'Tab';
  srVK_CLEAR      = 'Clear';
  srVK_RETURN     = 'Return';
  srVK_SHIFT      = 'Shift';
  srVK_CONTROL    = 'Control';
  srVK_MENU       = 'Menu';
  srVK_PAUSE      = 'Pause key';
  srVK_CAPITAL    = 'Capital';
  srVK_KANA       = 'Kana';
  srVK_JUNJA      = 'Junja';
  srVK_FINAL      = 'Final';
  srVK_HANJA      = 'Hanja';
  srVK_ESCAPE     = 'Escape';
  srVK_CONVERT    = 'Convert';
  srVK_NONCONVERT = 'Nonconvert';
  srVK_ACCEPT     = 'Accept';
  srVK_MODECHANGE = 'Mode Change';
  srVK_SPACE      = 'Space key';
  srVK_PRIOR      = 'Prior';
  srVK_NEXT       = 'Next';
  srVK_END        = 'End';
  srVK_HOME       = 'Home';
  srVK_LEFT       = 'Left';
  srVK_UP         = 'Up';
  srVK_RIGHT      = 'Right';
  //srVK_DOWN       = 'Down'; = dlgdownword
  //srVK_SELECT     = 'Select'; = lismenuselect
  srVK_PRINT      = 'Print';
  srVK_EXECUTE    = 'Execute';
  srVK_SNAPSHOT   = 'Snapshot';
  srVK_INSERT     = 'Insert';
  //srVK_DELETE     = 'Delete'; dlgeddelete
  srVK_HELP       = 'Help';
  srVK_LWIN       = 'left windows key';
  srVK_RWIN       = 'right windows key';
  srVK_APPS       = 'application key';
  srVK_NUMPAD     = 'Numpad %d';
  srVK_NUMLOCK    = 'Numlock';
  srVK_SCROLL     = 'Scroll';
  srVK_IRREGULAR  = 'Irregular ';
   
  //Category
  srkmCatCursorMoving   = 'Cursor moving commands';
  srkmCatSelection      = 'Text selection commands';
  srkmCatEditing        = 'Text editing commands';
  srkmCatCmdCmd         = 'Command commands';
  srkmCatSearchReplace  = 'Text search and replace commands';
  srkmCatMarker         = 'Text marker commands';
  srkmCatCodeTools      = 'CodeTools commands';
  srkmCatSrcNoteBook    = 'Source Notebook commands';
  srkmCatFileMenu       = 'File menu commands';
  srkmCatViewMenu       = 'View menu commands';
  srkmCatProjectMenu    = 'Project menu commands';
  srkmCatRunMenu        = 'Run menu commands';
  srkmCatComponentsMenu = 'Components menu commands';
  srkmCatToolMenu       = 'Tools menu commands';
  srkmCatEnvMenu        = 'Environment menu commands';
  srkmCarHelpMenu       = 'Help menu commands';

  //Languages
  rsLanguageAutomatic   = 'Automatic (or english)';
  rsLanguageEnglish     = 'English';
  rsLanguageDeutsch     = 'Deutsch';
  rsLanguageSpanish     = 'Español';
  rsLanguageFrench      = 'French';
  rsLanguageRussian     = 'òÕÓÓËÉÊ';

  //Units dependencies
  dlgUnitDepCaption     = 'Unit dependencies';
  dlgUnitDepBrowse      = 'Browse...';
  dlgUnitDepRefresh     = 'Refresh';
   
  // Build lazarus dialog
  lisBuildJITForm = 'Build JITForm';
  lisCleanLazarusSource = 'Clean Lazarus Source';
  lisBuildLCL = 'Build LCL';
  lisBuildComponent = 'Build Component';
  lisBuildSynEdit = 'Build SynEdit';
  lisBuildIDE = 'Build IDE';
  lisBuildExamples = 'Build Examples';
  lisConfigureBuildLazarus = 'Configure %sBuild Lazarus%s';
  lisLazBuildCleanAll = 'Clean all';
  lisLazBuildSetToBuildAll = 'Set to %sBuild All%s';
  lisLazBuildBuildComponentsSynEditCodeTools = 'Build Components (SynEdit, '
    +'CodeTools)';
  lisLazBuildBuildSynEdit = 'Build SynEdit';
  lisLazBuildBuildCodeTools = 'Build CodeTools';
  lisLazBuildBuildIDE = 'Build IDE';
  lisLazBuildBuildExamples = 'Build Examples';
  lisLazBuildOptions = 'Options:';
  lisLazBuildTargetOS = 'Target OS:';
  lisLazBuildLCLInterface = 'LCL interface';
  lisLazBuildBuildJITForm = 'Build JITForm';
  lisLazBuildOk = 'Ok';
  lisLazBuildCancel = 'Cancel';
  lisLazBuildNone = 'None';
  lisLazBuildBuild = 'Build';
  lisLazBuildCleanBuild = 'Clean+Build';
   
  // compiler
  lisCompilerErrorInvalidCompiler = 'Error: invalid compiler: %s';
  lisCompilerHintYouCanSetTheCompilerPath = 'Hint: you can set the compiler '
    +'path in Environment->Environment options->Files->Compiler Path';
  lisCompilerNOTELoadingOldCodetoolsOptionsFile = 'NOTE: loading old '
    +'codetools options file: ';
  lisCompilerNOTECodetoolsConfigFileNotFoundUsingDefaults = 'NOTE: codetools '
    +'config file not found - using defaults';
     
  // codetools options dialog
  lisCodeToolsOptsOk          = 'Ok';
  lisCodeToolsOptsNone        = 'None';
  lisCodeToolsOptsKeyword     = 'Keyword';
  lisCodeToolsOptsIdentifier  = 'Identifier';
  lisCodeToolsOptsColon       = 'Colon';
  lisCodeToolsOptsSemicolon   = 'Semicolon';
  lisCodeToolsOptsComma       = 'Comma';
  lisCodeToolsOptsPoint       = 'Point';
  lisCodeToolsOptsAt          = 'At';
  lisCodeToolsOptsNumber      = 'Number';
  lisCodeToolsOptsStringConst = 'String constant';
  lisCodeToolsOptsNewLine     = 'Newline';
  lisCodeToolsOptsSpace       = 'Space';
  lisCodeToolsOptsSymbol      = 'Symbol';
  
  // codetools defines
  lisCodeToolsDefsCodeToolsDefinesPreview = 'CodeTools Defines Preview';
  lisCodeToolsDefsWriteError = 'Write error';
  lisCodeToolsDefsErrorWhileWriting = 'Error while writing %s%s%s%s%s';
  lisCodeToolsDefsErrorWhileWritingProjectInfoFile = 'Error while writing '
    +'project info file %s%s%s%s%s';
  lisCodeToolsDefsReadError = 'Read error';
  lisCodeToolsDefsErrorReading = 'Error reading %s%s%s%s%s';
  lisCodeToolsDefsErrorReadingProjectInfoFile = 'Error reading project info '
    +'file %s%s%s%s%s';
  lisCodeToolsDefsNodeIsReadonly = 'Node is readonly';
  lisCodeToolsDefsAutoGeneratedNodesCanNotBeEdited = 'Auto generated nodes '
    +'can not be edited.';
  lisCodeToolsDefsInvalidPreviousNode = 'Invalid previous node';
  lisCodeToolsDefsPreviousNodeCanNotContainChildNodes = 'Previous node can '
    +'not contain child nodes.';
  lisCodeToolsDefsCreateFPCMacrosAndPathsForAFPCProjectDirectory = 'Create '
    +'FPC Macros and paths for a fpc project directory';
  lisCodeToolsDefsProjectDirectory = 'Project directory';
  lisCodeToolsDefsTheFreePascalProjectDirectory = 'The Free Pascal project '
    +'directory.';
  lisCodeToolsDefscompilerPath = 'compiler path';
  lisCodeToolsDefsThePathToTheFreePascalCompilerForThisProject = 'The path to '
    +'the free pascal compiler for this project. Only required if you set the '
    +'FPC CVS source below. Used to autocreate macros.';
  lisCodeToolsDefsFPCCVSSourceDirectory = 'FPC CVS source directory';
  lisCodeToolsDefsTheFreePascalCVSSourceDirectory = 'The Free Pascal CVS '
    +'source directory. Not required. This will improve find declarationand '
    +'debugging.';
  lisCodeToolsDefsCreateDefinesForFreePascalCompiler = 'Create Defines for '
    +'Free Pascal Compiler';
  lisCodeToolsDefsThePathToTheFreePascalCompilerForExample = 'The '
    +'path to the free pascal compiler.%s For example %s/usr/bin/ppc386 -n%s '
    +'or %s/usr/local/bin/fpc @/etc/11fpc.cfg%s.';
  lisCodeToolsDefsCreateDefinesForFreePascalCVSSources = 'Create Defines for '
    +'Free Pascal CVS Sources';
  lisCodeToolsDefsTheFreePascalCVSSourceDir = 'The Free Pascal CVS source '
    +'directory.';
  lisCodeToolsDefsCreateDefinesForLazarusDir = 'Create Defines for Lazarus '
    +'Directory';
  lisCodeToolsDefsLazarusDirectory = 'Lazarus Directory';
  lisCodeToolsDefsTheLazarusMainDirectory = 'The Lazarus main directory.';
  lisCodeToolsDefsCreateDefinesForDirectory = 'Create Defines for %s Directory';
  lisCodeToolsDefsdirectory = '%s directory';
  lisCodeToolsDefsDelphiMainDirectoryDesc = 'The %s main directory,%swhere '
    +'Borland has installed all %s sources.%sFor example: C:/Programme/'
    +'Borland/Delphi%s';
  lisCodeToolsDefsCreateDefinesForProject = 'Create Defines for %s Project';
  lisCodeToolsDefsprojectDirectory2 = '%s project directory';
  lisCodeToolsDefsTheProjectDirectory = 'The %s project directory,%swhich '
    +'contains the .dpr, dpk file.';
  lisCodeToolsDefsDelphiMainDirectoryForProject = 'The %s main directory,%'
    +'swhere Borland has installed all %s sources,%swhich are used by this %s '
    +'project.%sFor example: C:/Programme/Borland/Delphi%s';
  lisCodeToolsDefsExit = 'Exit';
  lisCodeToolsDefsSaveAndExit = 'Save and Exit';
  lisCodeToolsDefsExitWithoutSave = 'Exit without Save';
  lisCodeToolsDefsEdit = 'Edit';
  lisCodeToolsDefsMoveNodeUp = 'Move node up';
  lisCodeToolsDefsMoveNodeDown = 'Move node down';
  lisCodeToolsDefsMoveNodeOneLevelUp = 'Move node one level up';
  lisCodeToolsDefsMoveNodeOneLevelDown = 'Move node one level down';
  lisCodeToolsDefsInsertNodeBelow = 'Insert node below';
  lisCodeToolsDefsInsertNodeAsChild = 'Insert node as child';
  lisCodeToolsDefsDeleteNode = 'Delete node';
  lisCodeToolsDefsConvertNode = 'Convert node';
  lisCodeToolsDefsDefine = 'Define';
  lisCodeToolsDefsDefineRecurse = 'Define Recurse';
  lisCodeToolsDefsUndefine = 'Undefine';
  lisCodeToolsDefsUndefineRecurse = 'Undefine Recurse';
  lisCodeToolsDefsUndefineAll = 'Undefine All';
  lisCodeToolsDefsBlock = 'Block';
  lisCodeToolsDefsInsertBehindDirectory = 'Directory';
  lisCodeToolsDefsIf = 'If';
  lisCodeToolsDefsIfDef = 'IfDef';
  lisCodeToolsDefsIfNDef = 'IfNDef';
  lisCodeToolsDefsElseIf = 'ElseIf';
  lisCodeToolsDefsElse = 'Else';
  lisCodeToolsDefsInsertTemplate = 'Insert Template';
  lisCodeToolsDefsInsertFreePascalProjectTe = 'Insert Free Pascal Project '
    +'Template';
  lisCodeToolsDefsInsertFreePascalCompilerT = 'Insert Free Pascal Compiler '
    +'Template';
  lisCodeToolsDefsInsertFreePascalCVSSource = 'Insert Free Pascal CVS Source '
    +'Template';
  lisCodeToolsDefsInsertLazarusDirectoryTem = 'Insert Lazarus Directory '
    +'Template';
  lisCodeToolsDefsInsertDelphi5CompilerTemp = 'Insert Delphi 5 Compiler '
    +'Template';
  lisCodeToolsDefsInsertDelphi5DirectoryTem = 'Insert Delphi 5 Directory '
    +'Template';
  lisCodeToolsDefsInsertDelphi5ProjectTempl =
    'Insert Delphi 5 Project Template';
  lisCodeToolsDefsInsertDelphi6CompilerTemp = 'Insert Delphi 6 Compiler '
    +'Template';
  lisCodeToolsDefsInsertDelphi6DirectoryTem = 'Insert Delphi 6 Directory '
    +'Template';
  lisCodeToolsDefsInsertDelphi6ProjectTempl =
    'Insert Delphi 6 Project Template';
  lisCodeToolsDefsSelectedNode = 'Selected Node:';
  lisCodeToolsDefsNodeAndItsChildrenAreOnly = 'Node and its children are only '
    +'valid for this project';
  lisCodeToolsDefsName = 'Name:';
  lisCodeToolsDefsDescription = 'Description:';
  lisCodeToolsDefsVariable = 'Variable:';
  lisCodeToolsDefsValueAsText = 'Value as Text';
  lisCodeToolsDefsValueAsFilePaths = 'Value as File Paths';
  lisCodeToolsDefsAction = 'Action: %s';
  lisCodeToolsDefsautoGenerated = '%s, auto generated';
  lisCodeToolsDefsprojectSpecific = '%s, project specific';
  lisCodeToolsDefsnoneSelected = 'none selected';
  lisCodeToolsDefsInvalidParent = 'Invalid parent';
  lisCodeToolsDefsAutoCreatedNodesReadOnly = 'Auto created nodes can not be '
    +'edited,%snor can they have non auto created child nodes.';
  lisCodeToolsDefsInvalidParentNode = 'Invalid parent node';
  lisCodeToolsDefsParentNodeCanNotContainCh = 'Parent node can not contain '
    +'child nodes.';
  lisCodeToolsDefsNewNode = 'NewNode';
  lisCodeToolsDefsCodeToolsDefinesEditor = 'CodeTools Defines Editor';
  
  // code template dialog
  lisCodeTemplAddCodeTemplate = 'Add code template';
  lisCodeTemplAdd = 'Add';
  lisCodeTemplEditCodeTemplate = 'Edit code template';
  lisCodeTemplChange = 'Change';
  lisCodeTemplToken = 'Token:';
  lisCodeTemplComment = 'Comment:';
  lisCodeTemplATokenAlreadyExists = ' A token %s%s%s already exists! ';
  lisCodeTemplError = 'Error';
  
  // make resource string dialog
  lisMakeResourceString = 'Make ResourceString';
  lisMakeResStrInvalidResourcestringSect = 'Invalid Resourcestring section';
  lisMakeResStrPleaseChooseAResourstring = 'Please choose a resourstring '
    +'section from the list.';
  lisMakeResStrResourcestringAlreadyExis = 'Resourcestring already exists';
  lisMakeResStrChooseAnotherName = 'The resourcestring %s%s%s already exists.%'
    +'sPlease choose another name.%sUse Ignore to add it anyway.';
  lisMakeResStrStringConstantInSource = 'String Constant in source';
  lisMakeResStrConversionOptions = 'Conversion Options';
  lisMakeResStrIdentifierPrefix = 'Identifier Prefix:';
  lisMakeResStrIdentifierLength = 'Identifier Length:';
  lisMakeResStrCustomIdentifier = 'Custom Identifier';
  lisMakeResStrResourcestringSection = 'Resourcestring Section:';
  lisMakeResStrStringsWithSameValue = 'Strings with same value:';
  lisMakeResStrAppendToSection = 'Append to section';
  lisMakeResStrInsertAlphabetically = 'Insert alphabetically';
  lisMakeResStrSourcePreview = 'Source preview';
  
  // diff dialog
  lisDiffDlgText1 = 'Text1';
  lisDiffDlgOnlySelection = 'Only selection';
  lisDiffDlgText2 = 'Text2';
  lisDiffDlgCaseInsensitive = 'Case Insensitive';
  lisDiffDlgIgnoreIfEmptyLinesWereAdd = 'Ignore if empty lines were added or '
    +'removed';
  lisDiffDlgIgnoreSpacesAtStartOfLine = 'Ignore spaces at start of line';
  lisDiffDlgIgnoreSpacesAtEndOfLine = 'Ignore spaces at end of line';
  lisDiffDlgIgnoreIfLineEndCharsDiffe = 'Ignore difference in line ends (e.'
    +'g. #10 = #13#10)';
  lisDiffDlgIgnoreIfSpaceCharsWereAdd = 'Ignore amount of space chars';
  lisDiffDlgIgnoreSpaces = 'Ignore spaces (newline chars not included)';
  lisDiffDlgOpenDiffInEditor = 'Open Diff in editor';

  //todolist
  dlgTodoListCaption='ToDo List';
  dlgTodolistRefresh='Refresh todo items';
  dlgTodoListGotoLine='Goto selected source line';
  dlgTodoListPrintList='Print todo items';
  dlgToDoListOptions='ToDo options...';

implementation
end.

