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
unit LazarusIDEStrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
resourcestring
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
  lisLaunchingCmdLine = 'Launching target command line';
  
  // main bar menu
  lisMenuFile = '&File';
  lisMenuEdit = '&Edit';
  lisMenuSearch = '&Search';
  lisMenuView = '&View';
  lisMenuProject = '&Project';
  lisMenuRun = '&Run';
  lisMenuTools = '&Tools';
  lisMenuEnvironent = 'E&nvironment';
  lisMenuHelp = '&Help';
  
  lisMenuNewUnit = 'New Unit';
  lisMenuNewForm = 'New Form';
  lisMenuOpen = 'Open';
  lisMenuRevert = 'Revert';
  lisMenuOpenRecent = 'Open Recent';
  lisMenuSave = 'Save';
  lisMenuSaveAs = 'Save As';
  lisMenuSaveAll = 'Save All';
  lisMenuClose = 'Close';
  lisMenuCloseAll = 'Close All';
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
  lisMenuInsertUserName = 'Current username';
  lisMenuInsertDateTime = 'Current date and time';
  lisMenuInsertChangeLogEntry = 'ChangeLog entry';

  lisMenuFind = 'Find';
  lisMenuFindNext = 'Find &Next';
  lisMenuFindPrevious = 'Find &Previous';
  lisMenuFindInFiles = 'Find &in files';
  lisMenuReplace = 'Replace';
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
  lisMenuAddUnitToProject = 'Add active unit to Project';
  lisMenuRemoveUnitFromProject = 'Remove from Project';
  lisMenuViewSource = 'View Source';
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
  
  lisMenuSettings = 'Configure custom tools ...';
  lisMenuQuickSyntaxCheck = 'Quick syntax check';
  lisMenuGuessUnclosedBlock = 'Guess unclosed block';
  lisMenuGuessMisplacedIFDEF = 'Guess misplaced IFDEF/ENDIF';
  lisMenuConvertDFMtoLFM = 'Convert DFM file to LFM';
  lisMenuBuildLazarus = 'Build Lazarus';
  lisMenuConfigureBuildLazarus = 'Configure "Build Lazarus"';
  
  lisMenuGeneralOptions = 'General options';
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
  lisSaveSpace = 'Save ';
  lisSelectDFMFiles = 'Select Delphi form files (*.dfm)';

  // dialogs
  lisSaveChangesToProject = 'Save changes to project %s?';
  lisProjectChanged = 'Project changed';

  lisFPCSourceDirectoryError = 'FPC Source Directory error';
  lisPLzCheckTheFPCSourceDirectory = 'Please check the freepascal source directory';
  lisCompilerError = 'Compiler error';
  lisPlzCheckTheCmpilerName = 'Please check the compiler name';
  lisAboutLazarus = 'About Lazarus';
  lisAboutLazarusMsg =
     'License: GPL/LGPL'
    + LineEnding
    +'Lazarus are the class libraries for Free Pascal that emulate Delphi.' + LineEnding
    +'Free Pascal is a (L)GPL''ed compiler that runs on Linux,' + LineEnding
    +'Win32, OS/2, 68K and more. Free Pascal is designed to be able to' + LineEnding
    +'understand and compile Delphi syntax, which is of course OOP.' + LineEnding
    +'Lazarus is the missing part of the puzzle that will allow you to' + LineEnding
    +'develop Delphi like programs in all of the above platforms.' + LineEnding
    +'The IDE will eventually become a RAD tool like Delphi.' + LineEnding
    + LineEnding
    +'As Lazarus is growing we need more developers.' + LineEnding
    +'For example: Write a nicer about dialog with a logo.';
  lisUnitNameAlreadyExistsCap = 'Unitname already in project';
  lisUnitNameAlreadyExistsText = 'The unit "%s" already exists.' + LineEnding
       +'Ignore will force the renaming,' + LineEnding
       +'Cancel will cancel the saving of this source and' + LineEnding
       +'Abort will abort the whole saving.';
  lisInvalidPascalIdentifierCap = 'Invalid Pascal Identifier';
  lisInvalidPascalIdentifierText =
    'The name "%s" is not a valid pascal identifier.';

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
    'This program is free software; you can redistribute it and/or modify '
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
   +'Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. ';
//IDE components   
   ideStandard = 'Standard';
   ideAdditional = 'Additional';
   ideMisc = 'Misc';
   ideSystem = 'System';
   ideDialogs = 'Dialogs';
//I''ve skipped samples as sample components are usually placed there.
//Unfortunately, not in lazarus now... It might be a bad idea to have two
//palletes, for example 'Примеры' and 'Samples'
   ideDataAccess = 'Data Access';
   ideInterbase = 'Interbase Data Access';
//Environment dialogue
dlgEnvOpts = 'Environment Options';
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
dlgAutoForm = 'Auto create forms';
dlgEnvGrid = 'Grid';
dlgEnvLGuideLines = 'Guide lines';
dlgEnvMisc = 'Miscellaneous';
dlgPasExt = 'Default pascal extension';
dlgPasLower = 'Save pascal files lowercase';
dlgAmbigFileAct = 'Ambigious file action:';
//TODO Make it
dlgEnvAsk = 'Ask';
dlgAutoDel = 'Auto delete file';
dlgAutoRen = 'Auto rename file';
dlgAmbigWarn = 'Warn on compile';
dlgIgnoreVerb = 'Ignore';
//It''s OK then
dlgBackColor = 'Background color';
dlgEnvColors = 'Colors';
dlgEnvNotes = 'Notes: ';
dlgEdOptsCap = 'Editor Options';
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
dlgCloseButtonsNotebook = 'Close buttons in notebook';
dlgShowScrollHint = 'Show Scroll Hint';
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
dlgEdIdComlet = 'Identfier completion';
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

implementation

end.

