{  $Id$  }
{
 /***************************************************************************
                          mainbar.pp  -  Toolbar
                          ----------------------
  TMainIDEBar is main window of the IDE, containing the menu and the component
  palette.

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
unit MainBar;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, StdCtrls, Buttons, Menus, ComCtrls, Controls, ExtCtrls, Dialogs,
  Forms;

type
  { TMainIDEBar }

  TMainIDEBar = class(TForm)
  
    // the speedbuttons panel for frequently used IDE functions
    pnlSpeedButtons : TPanel;
    ViewUnitsSpeedBtn   : TSpeedButton;
    ViewFormsSpeedBtn   : TSpeedButton;
    NewUnitSpeedBtn     : TSpeedButton;
    OpenFileSpeedBtn    : TSpeedButton;
    OpenFileArrowSpeedBtn: TSpeedButton;
    SaveSpeedBtn        : TSpeedButton;
    SaveAllSpeedBtn     : TSpeedButton;
    ToggleFormSpeedBtn  : TSpeedButton;
    NewFormSpeedBtn     : TSpeedButton;
    RunSpeedButton      : TSpeedButton;
    PauseSpeedButton    : TSpeedButton;
    StepIntoSpeedButton : TSpeedButton;
    StepOverSpeedButton : TSpeedButton;
    OpenFilePopUpMenu   : TPopupMenu;

    // MainMenu
    mnuMain: TMainMenu;

    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuSearch: TMenuItem;
    mnuView: TMenuItem;
    mnuProject: TMenuItem;
    mnuRun: TMenuItem;
    mnuComponents: TMenuItem;
    mnuTools: TMenuItem;
    mnuEnvironment: TMenuItem;
    mnuWindows: TMenuItem;
    mnuHelp: TMenuItem;

    // file menu
    itmFileNewUnit: TMenuItem;
    itmFileNewForm: TMenuItem;
    itmFileNewOther: TMenuItem;
    itmFileOpen: TMenuItem;
    itmFileRevert: TMenuItem;
    itmFileRecentOpen: TMenuItem;
    itmFileSave: TMenuItem;
    itmFileSaveAs: TMenuItem;
    itmFileSaveAll: TMenuItem;
    itmFileClose: TMenuItem;
    itmFileCloseAll: TMenuItem;
    itmFileCleanDirectory: TMenuItem;
    {$IFDEF UseStartLazarus}
    itmFileRestart: TMenuItem;
    {$ENDIF}
    itmFileQuit: TMenuItem;

    // edit menu
    itmEditUndo: TMenuItem;
    itmEditRedo: TMenuItem;
    itmEditCut: TMenuItem;
    itmEditCopy: TMenuItem;
    itmEditPaste: TMenuItem;
    itmEditIndentBlock: TMenuItem;
    itmEditUnindentBlock: TMenuItem;
    itmEditEncloseBlock: TMenuItem;
    itmEditUpperCaseBlock: TMenuItem;
    itmEditLowerCaseBlock: TMenuItem;
    itmEditTabsToSpacesBlock: TMenuItem;
    itmEditCommentBlock: TMenuItem;
    itmEditUncommentBlock: TMenuItem;
    itmEditConditionalBlock: TMenuItem;
    itmEditSortBlock: TMenuItem;
    itmEditSelectionBreakLines: TMenuItem;
    itmEditSelect: TMenuItem;
    itmEditSelectAll: TMenuItem;
    itmEditSelectToBrace: TMenuItem;
    itmEditSelectCodeBlock: TMenuItem;
    itmEditSelectLine: TMenuItem;
    itmEditSelectParagraph: TMenuItem;
    itmEditInsertCharacter: TMenuItem;
    itmEditInsertText: TMenuItem;
    itmEditCompleteCode: TMenuItem;
    itmEditExtractProc: TMenuItem;

    itmEditInsertCVSKeyWord: TMenuItem;
    itmEditInsertGeneral: TMenuItem;

    itmEditInsertCVSAuthor: TMenuItem;
    itmEditInsertCVSDate: TMenuItem;
    itmEditInsertCVSHeader: TMenuItem;
    itmEditInsertCVSID: TMenuItem;
    itmEditInsertCVSLog: TMenuItem;
    itmEditInsertCVSName: TMenuItem;
    itmEditInsertCVSRevision: TMenuItem;
    itmEditInsertCVSSource: TMenuItem;

    itmEditInsertGPLNotice: TMenuItem;
    itmEditInsertLGPLNotice: TMenuItem;
    itmEditInsertUsername: TMenuItem;
    itmEditInsertDateTime: TMenuItem;
    itmEditInsertChangeLogEntry: TMenuItem;
    
    // search menu
    itmSearchFind: TMenuItem;
    itmSearchFindNext: TMenuItem;
    itmSearchFindPrevious: TMenuItem;
    itmSearchFindInFiles: TMenuItem;
    itmSearchFindIdentifierRefs: TMenuItem;
    itmSearchReplace: TMenuItem;
    itmSearchRenameIdentifier: TMenuItem;
    itmIncrementalFind: TMenuItem;
    itmGotoLine: TMenuItem;
    itmJumpBack: TMenuItem;
    itmJumpForward: TMenuItem;
    itmAddJumpPoint: TMenuItem;
    itmJumpHistory: TMenuItem;
    itmJumpToNextError: TMenuItem;
    itmJumpToPrevError: TMenuItem;
    itmFindBlockOtherEnd: TMenuItem;
    itmFindBlockStart: TMenuItem;
    itmFindDeclaration: TMenuItem;
    itmOpenFileAtCursor: TMenuItem;
    itmGotoIncludeDirective: TMenuItem;

    // view menu
    itmViewInspector: TMenuItem;
    itmViewSourceEditor: TMenuItem;
    itmViewCodeExplorer : TMenuItem;
    itmViewUnits : TMenuItem;
    itmViewForms : TMenuItem;
    itmViewUnitDependencies : TMenuItem;
    itmViewUnitInfo: TMenuItem;
    itmViewMessage : TMenuItem;
    itmViewSearchResults : TMenuItem;
    itmViewToggleFormUnit: TMenuItem;
    itmViewDebugWindows: TMenuItem;
    itmViewWatches: TMenuItem;
    itmViewBreakpoints: TMenuItem;
    itmViewLocals: TMenuItem;
    itmViewCallStack: TMenuItem;
    itmViewDebugOutput: TMenuItem;

    // project menu
    itmProjectNew: TMenuItem;
    itmProjectNewFromFile: TMenuItem;
    itmProjectOpen: TMenuItem;
    itmProjectRecentOpen: TMenuItem;
    itmProjectSave: TMenuItem;
    itmProjectSaveAs: TMenuItem;
    itmProjectPublish: TMenuItem;
    itmProjectInspector: TMenuItem;
    itmProjectAddTo: TMenuItem;
    itmProjectRemoveFrom: TMenuItem;
    itmProjectViewSource: TMenuItem;
    itmProjectViewToDos: TMenuItem;
    itmProjectOptions: TMenuItem;
    itmProjectCompilerOptions: TMenuItem;

    // run menu
    itmRunMenuBuild: TMenuItem;
    itmRunMenuBuildAll: TMenuItem;
    itmRunMenuAbortBuild: TMenuItem;
    itmRunMenuRun: TMenuItem;
    itmRunMenuPause: TMenuItem;
    itmRunMenuStepInto: TMenuItem;
    itmRunMenuStepOver: TMenuItem;
    itmRunMenuRunToCursor: TMenuItem;
    itmRunMenuStop: TMenuItem;
    itmRunMenuRunParameters: TMenuItem;
    itmRunMenuResetDebugger: TMenuItem;
    itmRunMenuBuildFile: TMenuItem;
    itmRunMenuRunFile: TMenuItem;
    itmRunMenuConfigBuildFile: TMenuItem;

    // components menu
    itmPkgOpenPackage: TMenuItem;
    itmPkgOpenPackageFile: TMenuItem;
    itmPkgOpenPackageOfCurUnit: TMenuItem;
    itmPkgOpenRecent: TMenuItem;
    itmPkgAddCurUnitToPkg: TMenuItem;
    itmPkgPkgGraph: TMenuItem;
    {$IFDEF CustomIDEComps}
    itmCompsConfigCustomComps: TMenuItem;
    {$ENDIF}

    // tools menu
    itmToolConfigure: TMenuItem;
    itmToolSyntaxCheck: TMenuItem;
    itmToolGuessUnclosedBlock: TMenuItem;
    itmToolGuessMisplacedIFDEF: TMenuItem;
    itmToolCheckLFM: TMenuItem;
    itmToolConvertDelphiUnit: TMenuItem;
    itmToolConvertDelphiProject: TMenuItem;
    itmToolConvertDFMtoLFM: TMenuItem;
    itmToolMakeResourceString: TMenuItem;
    itmToolDiff: TMenuItem;
    itmToolBuildLazarus: TMenuItem;
    itmToolConfigureBuildLazarus: TMenuItem;

    // environment menu
    itmEnvGeneralOptions: TMenuItem;
    itmEnvEditorOptions: TMenuItem;
    itmEnvDebuggerOptions: TMenuItem;
    itmEnvCodeToolsOptions: TMenuItem;
    itmEnvCodeToolsDefinesEditor: TMenuItem;
    itmEnvRescanFPCSrcDir: TMenuItem;

    // help menu
    itmHelpAboutLazarus: TMenuItem;
    itmHelpOnlineHelp: TMenuItem;
    itmHelpConfigureHelp: TMenuItem;

    // component palette
    ComponentNotebook : TNotebook;
    GlobalMouseSpeedButton: TSpeedButton;
  end;

var
  MainIDEBar: TMainIDEBar;

implementation

end.

