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
  
ResourceString
  // command line help
  lisCmdLineHlpHeader = 'lazarus [options] <project-filename>'#13#10
              +#13#10
              +'IDE Options:'#13#10
              +#13#10
              +'--help or -?             this help message'#13#10
              +#13#10;
  lisCmdLinePrimaryConfigPathDesc =
     '--primary-config-path <path>'#13#10
    +'                         primary config directory, where Lazarus'#13#10
    +'                         stores its config files. Default is '#13#10
    +'                         %s'#13#10
    +#13#10;
  lisCmdLineSecondaryConfigPathDesc =
     '--secondary-config-path <path>'#13#10
    +'                         secondary config directory, where Lazarus'#13#10
    +'                         searches for config template files.'#13#10
    +'                         Default is %s'#13#10
    +#13#10;
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
    +#13
    +'Lazarus are the class libraries for Free Pascal that emulate Delphi.'#13
    +'Free Pascal is a (L)GPL''ed compiler that runs on Linux,'#13
    +'Win32, OS/2, 68K and more. Free Pascal is designed to be able to'#13
    +'understand and compile Delphi syntax, which is of course OOP.'#13
    +'Lazarus is the missing part of the puzzle that will allow you to'#13
    +'develop Delphi like programs in all of the above platforms.'#13
    +'The IDE will eventually become a RAD tool like Delphi.'#13
    +#13
    +'As Lazarus is growing we need more developers.'#13
    +'For example: Write a nicer about dialog with a logo.';
  lisUnitNameAlreadyExistsCap = 'Unitname already in project';
  lisUnitNameAlreadyExistsText = 'The unit "%s" already exists.'#13
       +'Ignore will force the renaming,'#13
       +'Cancel will cancel the saving of this source and'#13
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

  
implementation

end.

