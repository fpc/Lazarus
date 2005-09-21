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
  Classes, StdCtrls, Forms, Controls, Buttons, Menus, ComCtrls, ExtCtrls,
  Dialogs, MenuIntf;

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

    {$IFDEF UseMenuIntf}
    // MainMenu
    mnuMainMenu: TMainMenu;
    mnuMain: TIDEMenuSection;

    // file menu
    mnuFile: TIDEMenuSection;
      itmFileNew: TIDEMenuSection;
        itmFileNewUnit: TIDEMenuCommand;
        itmFileNewForm: TIDEMenuCommand;
        itmFileNewOther: TIDEMenuCommand;
      itmFileOpenSave: TIDEMenuSection;
        itmFileOpen: TIDEMenuCommand;
        itmFileRevert: TIDEMenuCommand;
        itmFileRecentOpen: TIDEMenuSection;
        itmFileSave: TIDEMenuCommand;
        itmFileSaveAs: TIDEMenuCommand;
        itmFileSaveAll: TIDEMenuCommand;
        itmFileClose: TIDEMenuCommand;
        itmFileCloseAll: TIDEMenuCommand;
      itmFileDirectories: TIDEMenuSection;
        itmFileCleanDirectory: TIDEMenuCommand;
      itmFileIDEStart: TIDEMenuSection;
        itmFileRestart: TIDEMenuCommand;
        itmFileQuit: TIDEMenuCommand;

    // edit menu
    mnuEdit: TIDEMenuSection;
      itmEditReUndo: TIDEMenuSection;
        itmEditUndo: TIDEMenuCommand;
        itmEditRedo: TIDEMenuCommand;
      itmEditClipboard: TIDEMenuSection;
        itmEditCut: TIDEMenuCommand;
        itmEditCopy: TIDEMenuCommand;
        itmEditPaste: TIDEMenuCommand;
      itmEditBlockIndentation: TIDEMenuSection;
        itmEditIndentBlock: TIDEMenuCommand;
        itmEditUnindentBlock: TIDEMenuCommand;
        itmEditEncloseBlock: TIDEMenuCommand;
        itmEditCommentBlock: TIDEMenuCommand;
        itmEditUncommentBlock: TIDEMenuCommand;
        itmEditConditionalBlock: TIDEMenuCommand;
        itmEditSortBlock: TIDEMenuCommand;
      itmEditBlockCharConversion: TIDEMenuSection;
        itmEditUpperCaseBlock: TIDEMenuCommand;
        itmEditLowerCaseBlock: TIDEMenuCommand;
        itmEditTabsToSpacesBlock: TIDEMenuCommand;
      itmEditSelections: TIDEMenuSection;
        itmEditSelectionBreakLines: TIDEMenuCommand;
        itmEditSelect: TIDEMenuSection;
          itmEditSelectAll: TIDEMenuCommand;
          itmEditSelectToBrace: TIDEMenuCommand;
          itmEditSelectCodeBlock: TIDEMenuCommand;
          itmEditSelectLine: TIDEMenuCommand;
          itmEditSelectParagraph: TIDEMenuCommand;
      itmEditInsertions: TIDEMenuSection;
        itmEditInsertCharacter: TIDEMenuCommand;
        itmEditInsertText: TIDEMenuSection;
          itmEditInsertCVSKeyWord: TIDEMenuCommand;
            itmEditInsertCVSAuthor: TIDEMenuCommand;
            itmEditInsertCVSDate: TIDEMenuCommand;
            itmEditInsertCVSHeader: TIDEMenuCommand;
            itmEditInsertCVSID: TIDEMenuCommand;
            itmEditInsertCVSLog: TIDEMenuCommand;
            itmEditInsertCVSName: TIDEMenuCommand;
            itmEditInsertCVSRevision: TIDEMenuCommand;
            itmEditInsertCVSSource: TIDEMenuCommand;
          itmEditInsertGeneral: TIDEMenuCommand;
            itmEditInsertGPLNotice: TIDEMenuCommand;
            itmEditInsertLGPLNotice: TIDEMenuCommand;
            itmEditInsertUsername: TIDEMenuCommand;
            itmEditInsertDateTime: TIDEMenuCommand;
            itmEditInsertChangeLogEntry: TIDEMenuCommand;
      itmEditCodeTools: TIDEMenuSection;
        itmEditCompleteCode: TIDEMenuCommand;
        itmEditExtractProc: TIDEMenuCommand;

    // search menu
    mnuSearch: TIDEMenuSection;
      itmSearchFindReplace: TIDEMenuSection;
        itmSearchFind: TIDEMenuCommand;
        itmSearchFindNext: TIDEMenuCommand;
        itmSearchFindPrevious: TIDEMenuCommand;
        itmSearchFindInFiles: TIDEMenuCommand;
        itmSearchFindIdentifierRefs: TIDEMenuCommand;
        itmSearchReplace: TIDEMenuCommand;
        itmIncrementalFind: TIDEMenuCommand;
        itmGotoLine: TIDEMenuCommand;
      itmJumpings: TIDEMenuSection;
        itmJumpBack: TIDEMenuCommand;
        itmJumpForward: TIDEMenuCommand;
        itmAddJumpPoint: TIDEMenuCommand;
        itmJumpHistory: TIDEMenuCommand;
        itmJumpToNextError: TIDEMenuCommand;
        itmJumpToPrevError: TIDEMenuCommand;
      itmBookmarks: TIDEMenuSection;
        itmSetFreeBookmark: TIDEMenuCommand;
        itmJumpToNextBookmark: TIDEMenuCommand;
        itmJumpToPrevBookmark: TIDEMenuCommand;
      itmCodeToolSearches: TIDEMenuSection;
        itmFindDeclaration: TIDEMenuCommand;
        itmSearchRenameIdentifier: TIDEMenuCommand;
        itmFindBlockOtherEnd: TIDEMenuCommand;
        itmFindBlockStart: TIDEMenuCommand;
        itmOpenFileAtCursor: TIDEMenuCommand;
        itmGotoIncludeDirective: TIDEMenuCommand;

    // view menu
    mnuView: TIDEMenuSection;
    itmViewMainWindows: TIDEMenuSection;
      itmViewInspector: TIDEMenuCommand;
      itmViewSourceEditor: TIDEMenuCommand;
      itmViewCodeExplorer: TIDEMenuCommand;
      itmViewLazDoc: TIDEMenuCommand;
    itmViewUnitWindows: TIDEMenuSection;
      itmViewUnits: TIDEMenuCommand;
      itmViewForms: TIDEMenuCommand;
      itmViewUnitDependencies: TIDEMenuCommand;
      itmViewUnitInfo: TIDEMenuCommand;
      itmViewToggleFormUnit: TIDEMenuCommand;
    itmViewSecondaryWindows: TIDEMenuSection;
      itmViewAnchorEditor: TIDEMenuCommand;
      itmViewMessage: TIDEMenuCommand;
      itmViewSearchResults: TIDEMenuCommand;
      itmViewDebugWindows: TIDEMenuSection;
        itmViewWatches: TIDEMenuCommand;
        itmViewBreakpoints: TIDEMenuCommand;
        itmViewLocals: TIDEMenuCommand;
        itmViewCallStack: TIDEMenuCommand;
        itmViewDebugOutput: TIDEMenuCommand;

    // project menu
    mnuProject: TIDEMenuSection;
      itmProjectNews: TIDEMenuSection;
        itmProjectNew: TIDEMenuCommand;
        itmProjectNewFromFile: TIDEMenuCommand;
      itmProjectOpens: TIDEMenuSection;
        itmProjectOpen: TIDEMenuCommand;
        itmProjectRecentOpen: TIDEMenuCommand;
      itmProjectSaves: TIDEMenuSection;
        itmProjectSave: TIDEMenuCommand;
        itmProjectSaveAs: TIDEMenuCommand;
        itmProjectPublish: TIDEMenuCommand;
      itmProjectWindows: TIDEMenuSection;
        itmProjectInspector: TIDEMenuCommand;
        itmProjectOptions: TIDEMenuCommand;
        itmProjectCompilerOptions: TIDEMenuCommand;
        itmProjectViewToDos: TIDEMenuCommand;
      itmProjectAddRemoves: TIDEMenuSection;
        itmProjectAddTo: TIDEMenuCommand;
        itmProjectRemoveFrom: TIDEMenuCommand;
        itmProjectViewSource: TIDEMenuCommand;
      {$IFDEF TRANSLATESTRING}
      itmProjectPoFiles:TIDEMenuCommand;
        itmProjectCreatePoFiles:TIDEMenuCommand;
        itmProjectCollectPoFiles:TIDEMenuCommand;
      {$ENDIF}

    // run menu
    mnuRun: TIDEMenuSection;
      itmRunBuilding: TIDEMenuSection;
        itmRunMenuBuild: TIDEMenuCommand;
        itmRunMenuBuildAll: TIDEMenuCommand;
        itmRunMenuAbortBuild: TIDEMenuCommand;
      itmRunnning: TIDEMenuSection;
        itmRunMenuRun: TIDEMenuCommand;
        itmRunMenuPause: TIDEMenuCommand;
        itmRunMenuStepInto: TIDEMenuCommand;
        itmRunMenuStepOver: TIDEMenuCommand;
        itmRunMenuRunToCursor: TIDEMenuCommand;
        itmRunMenuStop: TIDEMenuCommand;
        itmRunMenuRunParameters: TIDEMenuCommand;
        itmRunMenuResetDebugger: TIDEMenuCommand;
      itmRunBuildignFile: TIDEMenuSection;
        itmRunMenuBuildFile: TIDEMenuCommand;
        itmRunMenuRunFile: TIDEMenuCommand;
        itmRunMenuConfigBuildFile: TIDEMenuCommand;
      itmRunDebugging: TIDEMenuSection;
        itmRunMenuInspect: TIDEMenuCommand;
        itmRunMenuEvaluate: TIDEMenuCommand;
        itmRunMenuAddWatch: TIDEMenuCommand;
        itmRunMenuAddBreakpoint: TIDEMenuCommand;
        itmRunMenuAddBpSource: TIDEMenuCommand;

    // components menu
    mnuComponents: TIDEMenuSection;
      itmPkgOpening: TIDEMenuCommand;
        itmPkgOpenPackage: TIDEMenuCommand;
        itmPkgOpenPackageFile: TIDEMenuCommand;
        itmPkgOpenPackageOfCurUnit: TIDEMenuCommand;
        itmPkgOpenRecent: TIDEMenuCommand;
      itmPkgUnits: TIDEMenuCommand;
        itmPkgAddCurUnitToPkg: TIDEMenuCommand;
      itmPkgGraphs: TIDEMenuCommand;
        itmPkgPkgGraph: TIDEMenuCommand;
        itmPkgEditInstallPkgs: TIDEMenuCommand;
        {$IFDEF CustomIDEComps}
        itmCompsConfigCustomComps: TIDEMenuCommand;
        {$ENDIF}

    // tools menu
    mnuTools: TIDEMenuSection;
      itmCustomTools: TIDEMenuSection;
        itmToolConfigure: TIDEMenuCommand;
      itmCodeToolChecks: TIDEMenuSection;
        itmToolSyntaxCheck: TIDEMenuCommand;
        itmToolGuessUnclosedBlock: TIDEMenuCommand;
        itmToolGuessMisplacedIFDEF: TIDEMenuCommand;
      itmDelphiConversion: TIDEMenuSection;
        itmToolCheckLFM: TIDEMenuCommand;
        itmToolConvertDelphiUnit: TIDEMenuCommand;
        itmToolConvertDelphiProject: TIDEMenuCommand;
        itmToolConvertDFMtoLFM: TIDEMenuCommand;
      itmSecondaryTools: TIDEMenuSection;
        itmToolMakeResourceString: TIDEMenuCommand;
        itmToolDiff: TIDEMenuCommand;
      itmBuildingLazarus: TIDEMenuSection;
        itmToolBuildLazarus: TIDEMenuCommand;
        itmToolConfigureBuildLazarus: TIDEMenuCommand;

    // environment menu
    mnuEnvironment: TIDEMenuSection;
      itmOptionsDialogs: TIDEMenuSection;
        itmEnvGeneralOptions: TIDEMenuCommand;
        itmEnvEditorOptions: TIDEMenuCommand;
        itmEnvCodeTemplates: TIDEMenuCommand;
        itmEnvDebuggerOptions: TIDEMenuCommand;
        itmEnvCodeToolsOptions: TIDEMenuCommand;
        itmEnvCodeToolsDefinesEditor: TIDEMenuCommand;
      itmIDECache: TIDEMenuSection;
        itmEnvRescanFPCSrcDir: TIDEMenuCommand;

    // windows menu
    mnuWindows: TIDEMenuSection;

    // help menu
    mnuHelp: TIDEMenuSection;
      itmOnlineHelps: TIDEMenuSection;
        itmHelpOnlineHelp: TIDEMenuCommand;
        itmHelpConfigureHelp: TIDEMenuCommand;
      itmInfoHelps: TIDEMenuSection;
        itmHelpAboutLazarus: TIDEMenuCommand;
    {$ELSE}
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
    itmFileRestart: TMenuItem;
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
    itmSetFreeBookmark: TMenuItem;
    itmJumpToNextBookmark: TMenuItem;
    itmJumpToPrevBookmark: TMenuItem;
    itmFindBlockOtherEnd: TMenuItem;
    itmFindBlockStart: TMenuItem;
    itmFindDeclaration: TMenuItem;
    itmOpenFileAtCursor: TMenuItem;
    itmGotoIncludeDirective: TMenuItem;

    // view menu
    itmViewInspector: TMenuItem;
    itmViewSourceEditor: TMenuItem;
    itmViewCodeExplorer : TMenuItem;
    itmViewLazDoc: TMenuItem;   //DBlaszijk 5-sep-05
    itmViewUnits : TMenuItem;
    itmViewForms : TMenuItem;
    itmViewUnitDependencies : TMenuItem;
    itmViewUnitInfo: TMenuItem;
    itmViewAnchorEditor : TMenuItem;
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
    {$IFDEF TRANSLATESTRING}
    itmProjectCreatePoFiles:TMenuItem;
    itmProjectCollectPoFiles:TMenuItem;
    {$ENDIF}

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
    itmRunMenuInspect: TMenuItem;
    itmRunMenuEvaluate: TMenuItem;
    itmRunMenuAddWatch: TMenuItem;
    itmRunMenuAddBreakpoint: TMenuItem;
    itmRunMenuAddBpSource: TMenuItem;

    // components menu
    itmPkgOpenPackage: TMenuItem;
    itmPkgOpenPackageFile: TMenuItem;
    itmPkgOpenPackageOfCurUnit: TMenuItem;
    itmPkgOpenRecent: TMenuItem;
    itmPkgAddCurUnitToPkg: TMenuItem;
    itmPkgPkgGraph: TMenuItem;
    itmPkgEditInstallPkgs: TMenuItem;
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
    itmEnvCodeTemplates: TMenuItem;
    itmEnvDebuggerOptions: TMenuItem;
    itmEnvCodeToolsOptions: TMenuItem;
    itmEnvCodeToolsDefinesEditor: TMenuItem;
    itmEnvRescanFPCSrcDir: TMenuItem;

    // help menu
    itmHelpAboutLazarus: TMenuItem;
    itmHelpOnlineHelp: TMenuItem;
    itmHelpConfigureHelp: TMenuItem;
    {$ENDIF}

    // component palette
    ComponentNotebook : TNotebook;
    GlobalMouseSpeedButton: TSpeedButton;
  private
    FOldWindowState: TWindowState;
  public
    procedure HideIDE;
    procedure UnhideIDE;
  end;

var
  MainIDEBar: TMainIDEBar;

implementation

{ TMainIDEBar }

procedure TMainIDEBar.HideIDE;
begin
  if WindowState=wsMinimized then exit;
  FOldWindowState:=WindowState;
  WindowState:=wsMinimized;
end;

procedure TMainIDEBar.UnhideIDE;
begin
  WindowState:=FOldWindowState;
end;

end.

