{  $Id$  }
{
 /***************************************************************************
                          mainbar.pp  -  Toolbar
                          ----------------------
                   TMainBar is the application toolbar window
                   and the base class for TMainIDE.


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
  Classes, LazarusIDEStrConsts, LCLType, LclLinux, Compiler, StdCtrls, Forms,
  Buttons, Menus, ComCtrls, Spin, Project, SysUtils, FileCtrl, Controls,
  Graphics, ExtCtrls, Dialogs, LazConf, CompReg, CodeToolManager,
  ObjectInspector, PropEdits, SynEditKeyCmds, OutputFilter,
  MsgView, EnvironmentOpts, EditorOptions, IDEComp, FormEditor,
  KeyMapping, IDEProcs, UnitEditor, Debugger, IDEOptionDefs, CodeToolsDefines;

const
  Version_String = '0.8.5 alpha';

type
  // The IDE is at anytime in a specific state:
  TIDEToolStatus = (
    itNone,      // The default mode. All editing allowed.
    itBuilder,   // compiling the project.
                 //    Loading/Saving/Debugging is not allowed.
    itDebugger,  // debugging the project.
                 //    Loading/Saving/Compiling is not allowed.
    itCodeTools, // the CodeToolBoss is working and has called the progress
                 //    event. This mode can be deactivated at any time.
    itCustom     // this state is not used yet.
    );

  // new file flags
  TNewFlag = (nfIsPartOfProject // force IsPartOfProject,
                                //   default is to use a heuristic
              );
  TNewFlags = set of TNewFlag;

  // save file flags
  TSaveFlag = (sfSaveAs,
               sfSaveToTestDir,
               sfProjectSaving,
               sfCheckAmbigiousFiles
               );
  TSaveFlags = set of TSaveFlag;
  
  // open file flags
  TOpenFlag = (ofProjectLoading,// this open is part of opening a whole project
               ofOnlyIfExists,  // do not auto create non existing files
               ofRevert,        // reload file if already open
               ofQuiet,         // less messages
               ofAddToRecent,   // add file to recent files
               ofRegularFile,   // open as regular file (e.g. not a whole project)
               ofVirtualFile    // open the virtual file
               );
  TOpenFlags = set of TOpenFlag;
  
  // revert file flags
  TRevertFlag = (rfQuiet);
  TRevertFlags = set of TRevertFlag;
  
  // close file flags
  TCloseFlag = (cfSaveFirst, // check if modified and save
                cfProjectClosing);
  TCloseFlags = set of TCloseFlag;
  
  // load buffer flags
  TLoadBufferFlag = (lbfUpdateFromDisk, lbfRevert, lbfCheckIfText);
  TLoadBufferFlags = set of TLoadBufferFlag;

  // codetools flags
  TCodeToolsFlag = (
    ctfSwitchToFormSource, // bring source notebook to front and show source of
                           //   current designed form
    ctfActivateAbortMode   // activate the CodeToolBoss.Abortable mode
    );
  TCodeToolsFlags = set of TCodeToolsFlag;


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
    mnuTools: TMenuItem;
    mnuEnvironment: TMenuItem;
    mnuHelp: TMenuItem;

    itmFileNewUnit : TMenuItem;
    itmFileNewForm : TMenuItem;
    itmFileOpen: TMenuItem;
    itmFileRevert: TMenuItem;
    itmFileRecentOpen: TMenuItem;
    itmFileSave: TMenuItem;
    itmFileSaveAs: TMenuItem;
    itmFileSaveAll: TMenuItem;
    itmFileClose: TMenuItem;
    itmFileCloseAll: TMenuItem;
    itmFileQuit: TMenuItem;

    itmEditUndo: TMenuItem;
    itmEditRedo: TMenuItem;
    itmEditCut: TMenuItem;
    itmEditCopy: TMenuItem;
    itmEditPaste: TMenuItem;
    itmEditIndentBlock: TMenuItem;
    itmEditUnindentBlock: TMenuItem;
    itmEditUpperCaseBlock: TMenuItem;
    itmEditLowerCaseBlock: TMenuItem;
    itmEditTabsToSpacesBlock: TMenuItem;
    itmEditCommentBlock: TMenuItem;
    itmEditUncommentBlock: TMenuItem;
    itmEditSelect: TMenuItem;
    itmEditSelectAll: TMenuItem;
    itmEditSelectToBrace: TMenuItem;
    itmEditSelectCodeBlock: TMenuItem;
    itmEditSelectLine: TMenuItem;
    itmEditSelectParagraph: TMenuItem;
    itmEditInsertText: TMenuItem;
    itmEditCompleteCode: TMenuItem;

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
    itmEditInsertUsername: TMenuItem;
    itmEditInsertDateTime: TMenuItem;
    itmEditInsertChangeLogEntry: TMenuItem;
    
    itmSearchFind: TMenuItem;
    itmSearchFindNext: TMenuItem;
    itmSearchFindPrevious: TMenuItem;
    itmSearchFindInFiles: TMenuItem;
    itmSearchReplace: TMenuItem;
    itmGotoLine: TMenuItem;
    itmJumpBack: TMenuItem;
    itmJumpForward: TMenuItem;
    itmAddJumpPoint: TMenuItem;
    itmJumpHistory: TMenuItem;
    itmFindBlockOtherEnd: TMenuItem;
    itmFindBlockStart: TMenuItem;
    itmFindDeclaration: TMenuItem;
    itmOpenFileAtCursor: TMenuItem;
    itmGotoIncludeDirective: TMenuItem;

    itmViewInspector: TMenuItem;
    itmViewProject: TMenuItem;
    itmViewUnits : TMenuItem;
    itmViewCodeExplorer : TMenuItem;
    itmViewForms : TMenuItem;
    itmViewUnitDependencies : TMenuItem;
    itmViewMessage : TMenuItem;
    itmViewDebugWindows: TMenuItem;
    itmViewWatches: TMenuItem;
    itmViewBreakpoints: TMenuItem;
    itmViewLocals: TMenuItem;
    itmViewCallStack: TMenuItem;
    itmViewDebugOutput: TMenuItem;
    itmViewToggleFormUnit: TMenuItem;

    itmProjectNew: TMenuItem;
    itmProjectNewFromFile: TMenuItem;
    itmProjectOpen: TMenuItem;
    itmProjectRecentOpen: TMenuItem;
    itmProjectSave: TMenuItem;
    itmProjectSaveAs: TMenuItem;
    itmProjectPublish: TMenuItem;
    itmProjectAddTo: TMenuItem;
    itmProjectRemoveFrom: TMenuItem;
    itmProjectViewSource: TMenuItem;
    itmProjectOptions: TMenuItem;

    itmProjectBuild: TMenuItem;
    itmProjectBuildAll: TMenuItem;
    itmProjectRun: TMenuItem;
    itmProjectPause: TMenuItem;
    itmProjectStepInto: TMenuItem;
    itmProjectStepOver: TMenuItem;
    itmProjectRunToCursor: TMenuItem;
    itmProjectStop: TMenuItem;
    itmProjectCompilerSettings: TMenuItem;
    itmProjectRunParameters: TMenuItem;

    itmToolConfigure: TMenuItem;
    itmToolSyntaxCheck: TMenuItem;
    itmToolGuessUnclosedBlock: TMenuItem;
    itmToolGuessMisplacedIFDEF: TMenuItem;
    itmToolConvertDFMtoLFM: TMenuItem;
    itmToolBuildLazarus: TMenuItem;
    itmToolConfigureBuildLazarus: TMenuItem;

    itmEnvGeneralOptions: TMenuItem;
    itmEnvEditorOptions: TMenuItem;
    itmEnvCodeToolsOptions: TMenuItem;
    itmEnvCodeToolsDefinesEditor: TMenuItem;

    itmHelpAboutLazarus: TMenuItem;

    // component palette
    ComponentNotebook : TNotebook;
    GlobalMouseSpeedButton: TSpeedButton;

    // hints. Note/ToDo: hints should be controlled by the lcl, this is a workaround
    HintTimer1 : TIdleTimer;
    HintWindow1 : THintWindow;
  protected
    TheCompiler: TCompiler;
    TheOutputFilter: TOutputFilter;
    
    function CreateMenuSeparator : TMenuItem;
    procedure SetupFileMenu; virtual;
    procedure SetupEditMenu; virtual;
    procedure SetupSearchMenu; virtual;
    procedure SetupViewMenu; virtual;
    procedure SetupProjectMenu; virtual;
    procedure SetupRunMenu; virtual;
    procedure SetupToolsMenu; virtual;
    procedure SetupEnvironmentMenu; virtual;
    procedure SetupHelpMenu; virtual;
    
    procedure LoadMenuShortCuts; virtual;
  public
    ToolStatus: TIDEToolStatus;
    function FindUnitFile(const AFilename: string): string; virtual; abstract;
    procedure GetCurrentUnit(var ActiveSourceEditor:TSourceEditor;
      var ActiveUnitInfo:TUnitInfo); virtual; abstract;
      
    function GetTestBuildDir: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;

    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoInitProjectRun: TModalResult; virtual; abstract;
    
    function DoCheckFilesOnDisk: TModalResult; virtual; abstract;
    function DoCheckAmbigiousSources(const AFilename: string;
      Compiling: boolean): TModalResult;
  end;

var
  MainIDE : TMainIDEBar;

  ObjectInspector1 : TObjectInspector;
  PropertyEditorHook1 : TPropertyEditorHook;
  SourceNotebook : TSourceNotebook;
  Project1: TProject;

const
  OpenFlagNames: array[TOpenFlag] of string = (
     'ofProjectLoading',
     'ofOnlyIfExists',
     'ofRevert',
     'ofQuiet',
     'ofAddToRecent',
     'ofRegularFile',
     'ofVirtualFile'
    );
    
  SaveFlagNames: array[TSaveFlag] of string = (
     'sfSaveAs',
     'sfSaveToTestDir',
     'sfProjectSaving',
     'sfCheckAmbigiousFiles'
    );

function OpenFlagsToString(Flags: TOpenFlags): string;
function SaveFlagsToString(Flags: TSaveFlags): string;

implementation


function OpenFlagsToString(Flags: TOpenFlags): string;
var
  Flag: TOpenFlag;
begin
  Result:='';
  for Flag:=Low(TOpenFlag) to High(TOpenFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+OpenFlagNames[Flag];
    end;
  end;
  Result:='['+Result+']';
end;

function SaveFlagsToString(Flags: TSaveFlags): string;
var
  Flag: TSaveFlag;
begin
  Result:='';
  for Flag:=Low(TSaveFlag) to High(TSaveFlag) do begin
    if Flag in Flags then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+SaveFlagNames[Flag];
    end;
  end;
  Result:='['+Result+']';
end;

function LoadPixmap(const ResourceName:string): TPixmap;
begin
  Result:=TPixmap.Create;
  Result.LoadFromLazarusResource(ResourceName);
end;


{ TMainIDEBar }

function TMainIDEBar.CreateMenuSeparator : TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := '-';
end;

procedure TMainIDEBar.SetupFileMenu;
begin
  itmFileNewUnit := TMenuItem.Create(Self);
  itmFileNewUnit.Name:='itmFileNewUnit';
  itmFileNewUnit.Caption := lisMenuNewUnit;
  itmFileNewUnit.Graphic:=LoadPixmap('menu_new');
  mnuFile.Add(itmFileNewUnit);

  itmFileNewForm := TMenuItem.Create(Self);
  itmFileNewForm.Name:='itmFileNewForm';
  itmFileNewForm.Caption := lisMenuNewForm;
  itmFileNewForm.Graphic:=LoadPixmap('menu_new');
  mnuFile.Add(itmFileNewForm);

  mnuFile.Add(CreateMenuSeparator);

  itmFileOpen := TMenuItem.Create(Self);
  itmFileOpen.Name:='itmFileOpen';
  itmFileOpen.Caption := lisMenuOpen;
  itmFileOpen.Graphic:=LoadPixmap('menu_open');
  mnuFile.Add(itmFileOpen);

  itmFileRevert := TMenuItem.Create(Self);
  itmFileRevert.Name:='itmFileRevert';
  itmFileRevert.Caption := lisMenuRevert;
  itmFileRevert.Graphic:=LoadPixmap('menu_undo');
  mnuFile.Add(itmFileRevert);

  itmFileRecentOpen := TMenuItem.Create(Self);
  itmFileRecentOpen.Name:='itmFileRecentOpen';
  itmFileRecentOpen.Caption := lisMenuOpenRecent;
  mnuFile.Add(itmFileRecentOpen);

  itmFileSave := TMenuItem.Create(Self);
  itmFileSave.Name:='itmFileSave';
  itmFileSave.Caption := lisMenuSave;
  itmFileSave.Graphic:=LoadPixmap('menu_save');
  mnuFile.Add(itmFileSave);

  itmFileSaveAs := TMenuItem.Create(Self);
  itmFileSaveAs.Name:='itmFileSaveAs';
  itmFileSaveAs.Caption := lisMenuSaveAs;
  itmFileSaveAs.Graphic:=LoadPixmap('menu_save');
  mnuFile.Add(itmFileSaveAs);

  itmFileSaveAll := TMenuItem.Create(Self);
  itmFileSaveAll.Name:='itmFileSaveAll';
  itmFileSaveAll.Caption := lisMenuSaveAll;
  itmFileSaveAll.Graphic:=LoadPixmap('menu_save');
  mnuFile.Add(itmFileSaveAll);

  itmFileClose := TMenuItem.Create(Self);
  itmFileClose.Name:='itmFileClose';
  itmFileClose.Caption := lisMenuClose;
  itmFileClose.Enabled := False;
  mnuFile.Add(itmFileClose);

  itmFileCloseAll := TMenuItem.Create(Self);
  itmFileCloseAll.Name:='itmFileCloseAll';
  itmFileCloseAll.Caption := lisMenuCloseAll;
  itmFileCloseAll.Enabled := False;
  mnuFile.Add(itmFileCloseAll);

  mnuFile.Add(CreateMenuSeparator);

  itmFileQuit := TMenuItem.Create(Self);
  itmFileQuit.Name:='itmFileQuit';
  itmFileQuit.Caption := lisMenuQuit;
  mnuFile.Add(itmFileQuit);
end;

procedure TMainIDEBar.SetupEditMenu;
begin
  itmEditUndo := TMenuItem.Create(Self);
  itmEditUndo.Name:='itmEditUndo';
  itmEditUndo.Caption := lisMenuUndo;
  itmEditUndo.Graphic:=LoadPixmap('menu_undo');
  mnuEdit.Add(itmEditUndo);

  itmEditRedo := TMenuItem.Create(Self);
  itmEditRedo.Name:='itmEditRedo';
  itmEditRedo.Caption := lisMenuRedo;
  itmEditRedo.Graphic:=LoadPixmap('menu_redo');
  mnuEdit.Add(itmEditRedo);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditCut  := TMenuItem.Create(Self);
  itmEditCut.Name:='itmEditCut';
  itmEditCut.Caption := lisMenuCut;
  itmEditCut.Graphic:=LoadPixmap('menu_cut');
  mnuEdit.Add(itmEditCut);

  itmEditCopy := TMenuItem.Create(Self);
  itmEditCopy.Name:='itmEditCopy';
  itmEditCopy.Caption := lisMenuCopy;
  itmEditCopy.Graphic:=LoadPixmap('menu_copy');
  mnuEdit.Add(itmEditCopy);

  itmEditPaste := TMenuItem.Create(Self);
  itmEditPaste.Name:='itmEditPaste';
  itmEditPaste.Caption := lisMenuPaste;
  itmEditPaste.Graphic:=LoadPixmap('menu_paste');
  mnuEdit.Add(itmEditPaste);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditIndentBlock := TMenuItem.Create(Self);
  itmEditIndentBlock.Name:='itmEditIndentBlock';
  itmEditIndentBlock.Caption := lisMenuIndentSelection;
  itmEditIndentBlock.Graphic:=LoadPixmap('menu_indent');
  mnuEdit.Add(itmEditIndentBlock);

  itmEditUnindentBlock := TMenuItem.Create(Self);
  itmEditUnindentBlock.Name:='itmEditUnindentBlock';
  itmEditUnindentBlock.Caption := lisMenuUnindentSelection;
  itmEditUnindentBlock.Graphic:=LoadPixmap('menu_unindent');
  mnuEdit.Add(itmEditUnindentBlock);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditUpperCaseBlock := TMenuItem.Create(Self);
  itmEditUpperCaseBlock.Name:='itmEditUpperCaseBlock';
  itmEditUpperCaseBlock.Caption := lisMenuUpperCaseSelection;
  mnuEdit.Add(itmEditUpperCaseBlock);

  itmEditLowerCaseBlock := TMenuItem.Create(Self);
  itmEditLowerCaseBlock.Name:='itmEditLowerCaseBlock';
  itmEditLowerCaseBlock.Caption := lisMenuLowerCaseSelection;
  mnuEdit.Add(itmEditLowerCaseBlock);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditTabsToSpacesBlock := TMenuItem.Create(Self);
  itmEditTabsToSpacesBlock.Name:='itmEditTabsToSpacesBlock';
  itmEditTabsToSpacesBlock.Caption := lisMenuTabsToSpacesSelection;
  mnuEdit.Add(itmEditTabsToSpacesBlock);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditCommentBlock := TMenuItem.Create(Self);
  itmEditCommentBlock.Name:='itmEditCommentBlock';
  itmEditCommentBlock.Caption := lisMenuCommentSelection;
  mnuEdit.Add(itmEditCommentBlock);

  itmEditUncommentBlock := TMenuItem.Create(Self);
  itmEditUncommentBlock.Name:='itmEditUncommentBlock';
  itmEditUncommentBlock.Caption := lisMenuUncommentSelection;
  mnuEdit.Add(itmEditUncommentBlock);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditSelect := TMenuItem.Create(Self);
  itmEditSelect.Name:='itmEditSelect';
  itmEditSelect.Caption := lisMenuSelect;
  mnuEdit.Add(itmEditSelect);

  begin
    // select sub menu items
    itmEditSelectAll := TMenuItem.Create(Self);
    itmEditSelectAll.Name:='itmEditSelectAll';
    itmEditSelectAll.Caption := lisMenuSelectAll;
    itmEditSelect.Add(itmEditSelectAll);

    itmEditSelectToBrace := TMenuItem.Create(Self);
    itmEditSelectToBrace.Name:='itmEditSelectToBrace';
    itmEditSelectToBrace.Caption := lisMenuSelectToBrace;
    itmEditSelect.Add(itmEditSelectToBrace);

    itmEditSelectCodeBlock := TMenuItem.Create(Self);
    itmEditSelectCodeBlock.Name:='itmEditSelectCodeBlock';
    itmEditSelectCodeBlock.Caption := lisMenuSelectCodeBlock;
    itmEditSelectCodeBlock.Enabled:=false;
    itmEditSelect.Add(itmEditSelectCodeBlock);

    itmEditSelectLine := TMenuItem.Create(Self);
    itmEditSelectLine.Name:='itmEditSelectLine';
    itmEditSelectLine.Caption := lisMenuSelectLine;
    itmEditSelect.Add(itmEditSelectLine);

    itmEditSelectParagraph := TMenuItem.Create(Self);
    itmEditSelectParagraph.Name:='itmEditSelectParagraph';
    itmEditSelectParagraph.Caption := lisMenuSelectParagraph;
    itmEditSelect.Add(itmEditSelectParagraph);
  end;

  itmEditInsertText := TMenuItem.Create(Self);
  itmEditInsertText.Name:='itmEditInsertText';
  itmEditInsertText.Caption := lisMenuInsertText;
  mnuEdit.Add(itmEditInsertText);

  begin
    // insert text sub menu items
    itmEditInsertCVSKeyWord := TMenuItem.Create(Self);
    itmEditInsertCVSKeyWord.Name:='itmEditInsertCVSKeyWord';
    itmEditInsertCVSKeyWord.Caption := lisMenuInsertCVSKeyword;
    itmEditInsertText.Add(itmEditInsertCVSKeyWord);

    begin
      // insert CVS keyword sub menu items
      itmEditInsertCVSAuthor := TMenuItem.Create(Self);
      itmEditInsertCVSAuthor.Name:='itmEditInsertCVSAuthor';
      itmEditInsertCVSAuthor.Caption := 'Author';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSAuthor);
      
      itmEditInsertCVSDate := TMenuItem.Create(Self);
      itmEditInsertCVSDate.Name:='itmEditInsertCVSDate';
      itmEditInsertCVSDate.Caption := 'Date';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSDate);

      itmEditInsertCVSHeader := TMenuItem.Create(Self);
      itmEditInsertCVSHeader.Name:='itmEditInsertCVSHeader';
      itmEditInsertCVSHeader.Caption := 'Header';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSHeader);

      itmEditInsertCVSID := TMenuItem.Create(Self);
      itmEditInsertCVSID.Name:='itmEditInsertCVSID';
      itmEditInsertCVSID.Caption := 'ID';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSID);

      itmEditInsertCVSLog := TMenuItem.Create(Self);
      itmEditInsertCVSLog.Name:='itmEditInsertCVSLog';
      itmEditInsertCVSLog.Caption := 'Log';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSLog);

      itmEditInsertCVSName := TMenuItem.Create(Self);
      itmEditInsertCVSName.Name:='itmEditInsertCVSName';
      itmEditInsertCVSName.Caption := 'Name';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSName);

      itmEditInsertCVSRevision := TMenuItem.Create(Self);
      itmEditInsertCVSRevision.Name:='itmEditInsertCVSRevision';
      itmEditInsertCVSRevision.Caption := 'Revision';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSRevision);

      itmEditInsertCVSSource := TMenuItem.Create(Self);
      itmEditInsertCVSSource.Name:='itmEditInsertCVSSource';
      itmEditInsertCVSSource.Caption := 'Source';
      itmEditInsertCVSKeyWord.Add(itmEditInsertCVSSource);
    end;
    
    itmEditInsertGeneral := TMenuItem.Create(Self);
    itmEditInsertGeneral.Name:='itmEditInsertGeneral';
    itmEditInsertGeneral.Caption := lisMenuInsertGeneral;
    itmEditInsertText.Add(itmEditInsertGeneral);
    
    begin
      // insert general text sub menu items
      itmEditInsertGPLNotice := TMenuItem.Create(Self);
      itmEditInsertGPLNotice.Name:='itmEditInsertGPLNotice';
      itmEditInsertGPLNotice.Caption := lisMenuInsertGPLNotice;
      itmEditInsertGeneral.Add(itmEditInsertGPLNotice);

      itmEditInsertUsername := TMenuItem.Create(Self);
      itmEditInsertUsername.Name:='itmEditInsertUsername';
      itmEditInsertUsername.Caption := lisMenuInsertUsername;
      itmEditInsertGeneral.Add(itmEditInsertUsername);

      itmEditInsertDateTime := TMenuItem.Create(Self);
      itmEditInsertDateTime.Name:='itmEditInsertDateTime';
      itmEditInsertDateTime.Caption := lisMenuInsertDateTime;
      itmEditInsertGeneral.Add(itmEditInsertDateTime);

      itmEditInsertChangeLogEntry := TMenuItem.Create(Self);
      itmEditInsertChangeLogEntry.Name:='itmEditInsertChangeLogEntry';
      itmEditInsertChangeLogEntry.Caption := lisMenuInsertChangeLogEntry;
      itmEditInsertGeneral.Add(itmEditInsertChangeLogEntry);
    end;
  end;

  mnuEdit.Add(CreateMenuSeparator);

  itmEditCompleteCode := TMenuItem.Create(Self);
  itmEditCompleteCode.Name:='itmEditCompleteCode';
  itmEditCompleteCode.Caption := lisMenuCompleteCode;
  mnuEdit.Add(itmEditCompleteCode);
end;

procedure TMainIDEBar.SetupSearchMenu;
begin
  itmSearchFind := TMenuItem.Create(Self);
  itmSearchFind.Name:='itmSearchFind';
  itmSearchFind.Caption := lisMenuFind;
  mnuSearch.add(itmSearchFind);

  itmSearchFindNext := TMenuItem.Create(Self);
  itmSearchFindNext.Name:='itmSearchFindNext';
  itmSearchFindNext.Caption := lisMenuFindNext;
  mnuSearch.add(itmSearchFindNext);

  itmSearchFindPrevious := TMenuItem.Create(Self);
  itmSearchFindPrevious.Name:='itmSearchFindPrevious';
  itmSearchFindPrevious.Caption := lisMenuFindPrevious;
  mnuSearch.add(itmSearchFindPrevious);

  itmSearchFindInFiles := TMenuItem.Create(Self);
  itmSearchFindInFiles.Name:='itmSearchFindInFiles';
  itmSearchFindInFiles.Caption := lisMenuFindInFiles;
  mnuSearch.add(itmSearchFindInFiles);

  itmSearchReplace := TMenuItem.Create(Self);
  itmSearchReplace.Name:='itmSearchReplace';
  itmSearchReplace.Caption := lisMenuReplace;
  mnuSearch.add(itmSearchReplace);

  mnuSearch.Add(CreateMenuSeparator);

  itmGotoLine := TMenuItem.Create(Self);
  itmGotoLine.Name:='itmGotoLine';
  itmGotoLine.Caption := lisMenuGotoLine;
  mnuSearch.add(itmGotoLine);

  mnuSearch.Add(CreateMenuSeparator);

  itmJumpBack := TMenuItem.Create(Self);
  itmJumpBack.Name:='itmJumpBack';
  itmJumpBack.Caption := lisMenuJumpBack;
  mnuSearch.add(itmJumpBack);

  itmJumpForward := TMenuItem.Create(Self);
  itmJumpForward.Name:='itmJumpForward';
  itmJumpForward.Caption := lisMenuJumpForward;
  mnuSearch.add(itmJumpForward);

  itmAddJumpPoint := TMenuItem.Create(Self);
  itmAddJumpPoint.Name:='itmAddJumpPoint';
  itmAddJumpPoint.Caption := lisMenuAddJumpPointToHistory;
  mnuSearch.add(itmAddJumpPoint);

  itmJumpHistory := TMenuItem.Create(Self);
  itmJumpHistory.Name:='itmJumpHistory';
  itmJumpHistory.Caption := lisMenuViewJumpHistory;
  mnuSearch.add(itmJumpHistory);

  mnuSearch.Add(CreateMenuSeparator);

  itmFindBlockOtherEnd := TMenuItem.Create(Self);
  itmFindBlockOtherEnd.Name:='itmFindBlockOtherEnd';
  itmFindBlockOtherEnd.Caption := lisMenuFindBlockOtherEndOfCodeBlock;
  mnuSearch.add(itmFindBlockOtherEnd);

  itmFindBlockStart := TMenuItem.Create(Self);
  itmFindBlockStart.Name:='itmFindBlockStart';
  itmFindBlockStart.Caption := lisMenuFindCodeBlockStart;
  mnuSearch.add(itmFindBlockStart);

  mnuSearch.Add(CreateMenuSeparator);

  itmFindDeclaration := TMenuItem.Create(Self);
  itmFindDeclaration.Name:='itmFindDeclaration';
  itmFindDeclaration.Caption := lisMenuFindDeclarationAtCursor;
  mnuSearch.add(itmFindDeclaration);

  itmOpenFileAtCursor := TMenuItem.Create(Self);
  itmOpenFileAtCursor.Name:='itmOpenFileAtCursor';
  itmOpenFileAtCursor.Caption := lisMenuOpenFilenameAtCursor;
  mnuSearch.add(itmOpenFileAtCursor);

  itmGotoIncludeDirective := TMenuItem.Create(Self);
  itmGotoIncludeDirective.Name:='itmGotoIncludeDirective';
  itmGotoIncludeDirective.Caption := lisMenuGotoIncludeDirective;
  mnuSearch.add(itmGotoIncludeDirective);
end;

procedure TMainIDEBar.SetupViewMenu;
begin
  itmViewInspector := TMenuItem.Create(Self);
  itmViewInspector.Name:='itmViewInspector';
  itmViewInspector.Caption := lisMenuViewObjectInspector;
  mnuView.Add(itmViewInspector);

  itmViewProject  := TMenuItem.Create(Self);
  itmViewProject.Name:='itmViewProject';
  itmViewProject.Caption := lisMenuViewProjectExplorer;
  mnuView.Add(itmViewProject);

  mnuView.Add(CreateMenuSeparator);

  itmViewCodeExplorer := TMenuItem.Create(Self);
  itmViewCodeExplorer.Name:='itmViewCodeExplorer';
  itmViewCodeExplorer.Caption := lisMenuViewCodeExplorer;
  mnuView.Add(itmViewCodeExplorer);

  mnuView.Add(CreateMenuSeparator);

  itmViewUnits := TMenuItem.Create(Self);
  itmViewUnits.Name:='itmViewUnits';
  itmViewUnits.Caption := lisMenuViewUnits;
  mnuView.Add(itmViewUnits);

  itmViewForms := TMenuItem.Create(Self);
  itmViewForms.Name:='itmViewForms';
  itmViewForms.Caption := lisMenuViewForms;
  mnuView.Add(itmViewForms);

  itmViewUnitDependencies := TMenuItem.Create(Self);
  itmViewUnitDependencies.Name:='itmViewUnitDependencies';
  itmViewUnitDependencies.Caption := lisMenuViewUnitDependencies;
  mnuView.Add(itmViewUnitDependencies);

  mnuView.Add(CreateMenuSeparator);

  itmViewToggleFormUnit := TMenuItem.Create(Self);
  itmViewToggleFormUnit.Name:='itmViewToggleFormUnit';
  itmViewToggleFormUnit.Caption := lisMenuViewToggleFormUnit;
  mnuView.Add(itmViewToggleFormUnit);

  mnuView.Add(CreateMenuSeparator);

  itmViewMessage := TMenuItem.Create(Self);
  itmViewMessage.Name:='itmViewMessage';
  itmViewMessage.Caption := lisMenuViewMessages;
  mnuView.Add(itmViewMessage);

  itmViewDebugWindows := TMenuItem.Create(Self);
  itmViewDebugWindows.Name := 'itmViewDebugWindows';
  itmViewDebugWindows.Caption := lisMenuDebugWindows;
  mnuView.Add(itmViewDebugWindows);

  itmViewWatches := TMenuItem.Create(Self);
  itmViewWatches.Name:='itmViewWatches';
  itmViewWatches.Caption := lisMenuViewWatches;
  itmViewDebugWindows.Add(itmViewWatches);

  itmViewBreakPoints := TMenuItem.Create(Self);
  itmViewBreakPoints.Name:='itmViewBreakPoints';
  itmViewBreakPoints.Caption := lisMenuViewBreakPoints;
  itmViewDebugWindows.Add(itmViewBreakPoints);

  itmViewLocals := TMenuItem.Create(Self);
  itmViewLocals.Name:='itmViewLocals';
  itmViewLocals.Caption := lisMenuViewLocalVariables;
  itmViewDebugWindows.Add(itmViewLocals);

  itmViewCallStack := TMenuItem.Create(Self);
  itmViewCallStack.Name:='itmViewCallStack';
  itmViewCallStack.Caption := lisMenuViewCallStack;
  itmViewDebugWindows.Add(itmViewCallStack);

  itmViewDebugOutput := TMenuItem.Create(Self);
  itmViewDebugOutput.Name:='itmViewDebugOutput';
  itmViewDebugOutput.Caption := lisMenuViewDebugOutput;
  itmViewDebugWindows.Add(itmViewDebugOutput);
end;

procedure TMainIDEBar.SetupProjectMenu;
begin
  itmProjectNew := TMenuItem.Create(Self);
  itmProjectNew.Name:='itmProjectNew';
  itmProjectNew.Caption := lisMenuNewProject;
  mnuProject.Add(itmProjectNew);

  itmProjectNewFromFile := TMenuItem.Create(Self);
  itmProjectNewFromFile.Name:='itmProjectNewFromFile';
  itmProjectNewFromFile.Caption := lisMenuNewProjectFromFile;
  mnuProject.Add(itmProjectNewFromFile);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectOpen := TMenuItem.Create(Self);
  itmProjectOpen.Name:='itmProjectOpen';
  itmProjectOpen.Caption := lisMenuOpenProject;
  mnuProject.Add(itmProjectOpen);

  itmProjectRecentOpen := TMenuItem.Create(Self);
  itmProjectRecentOpen.Name:='itmProjectRecentOpen';
  itmProjectRecentOpen.Caption := lisMenuOpenRecentProject;
  mnuProject.Add(itmProjectRecentOpen);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectSave := TMenuItem.Create(Self);
  itmProjectSave.Name:='itmProjectSave';
  itmProjectSave.Caption := lisMenuSaveProject;
  mnuProject.Add(itmProjectSave);

  itmProjectSaveAs := TMenuItem.Create(Self);
  itmProjectSaveAs.Name:='itmProjectSaveAs';
  itmProjectSaveAs.Caption := lisMenuSaveProjectAs;
  mnuProject.Add(itmProjectSaveAs);

  itmProjectPublish := TMenuItem.Create(Self);
  itmProjectPublish.Name:='itmProjectPublish';
  itmProjectPublish.Caption := lisMenuPublishProject;
  mnuProject.Add(itmProjectPublish);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectAddTo := TMenuItem.Create(Self);
  itmProjectAddTo.Name:='itmProjectAddTo';
  itmProjectAddTo.Caption := lisMenuAddUnitToProject;
  mnuProject.Add(itmProjectAddTo);

  itmProjectRemoveFrom := TMenuItem.Create(Self);
  itmProjectRemoveFrom.Name:='itmProjectRemoveFrom';
  itmProjectRemoveFrom.Caption := lisMenuRemoveUnitFromProject;
  mnuProject.Add(itmProjectRemoveFrom);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectViewSource := TMenuItem.Create(Self);
  itmProjectViewSource.Name:='itmProjectViewSource';
  itmProjectViewSource.Caption := lisMenuViewSource;
  mnuProject.Add(itmProjectViewSource);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectOptions := TMenuItem.Create(Self);
  itmProjectOptions.Name:='itmProjectOptions';
  itmProjectOptions.Caption := lisMenuProjectOptions;
  mnuProject.Add(itmProjectOptions);
end;

procedure TMainIDEBar.SetupRunMenu;
begin
  itmProjectBuild := TMenuItem.Create(Self);
  itmProjectBuild.Name:='itmProjectBuild';
  itmProjectBuild.Caption := lisMenuBuild;
  mnuRun.Add(itmProjectBuild);

  itmProjectBuildAll := TMenuItem.Create(Self);
  itmProjectBuildAll.Name:='itmProjectBuildAll';
  itmProjectBuildAll.Caption := lisMenuBuildAll;
  mnuRun.Add(itmProjectBuildAll);

  mnuRun.Add(CreateMenuSeparator);

  itmProjectRun := TMenuItem.Create(Self);
  itmProjectRun.Name:='itmProjectRun';
  itmProjectRun.Caption := lisMenuProjectRun;
  mnuRun.Add(itmProjectRun);

  itmProjectPause := TMenuItem.Create(Self);
  itmProjectPause.Name:='itmProjectPause';
  itmProjectPause.Caption := lisMenuPause;
  itmProjectPause.Enabled := false;
  mnuRun.Add(itmProjectPause);

  itmProjectStepInto := TMenuItem.Create(Self);
  itmProjectStepInto.Name:='itmProjectStepInto';
  itmProjectStepInto.Caption := lisMenuStepInto;
  mnuRun.Add(itmProjectStepInto);

  itmProjectStepOver := TMenuItem.Create(Self);
  itmProjectStepOver.Name:='itmProjectStepOver';
  itmProjectStepOver.Caption := lisMenuStepOver;
  mnuRun.Add(itmProjectStepOver);

  itmProjectRunToCursor := TMenuItem.Create(Self);
  itmProjectRunToCursor.Name:='itmProjectRunToCursor';
  itmProjectRunToCursor.Caption := lisMenuRunToCursor;
  mnuRun.Add(itmProjectRunToCursor);

  itmProjectStop := TMenuItem.Create(Self);
  itmProjectStop.Name:='itmProjectStop';
  itmProjectStop.Caption := lisMenuStop;
  mnuRun.Add(itmProjectStop);

  mnuRun.Add(CreateMenuSeparator);

  itmProjectCompilerSettings := TMenuItem.Create(Self);
  itmProjectCompilerSettings.Name:='itmProjectCompilerSettings';
  itmProjectCompilerSettings.Caption := lisMenuCompilerOptions;
  mnuRun.Add(itmProjectCompilerSettings);

  itmProjectRunParameters := TMenuItem.Create(Self);
  itmProjectRunParameters.Name:='itmProjectRunParameters';
  itmProjectRunParameters.Caption := lisMenuRunParameters;
  mnuRun.Add(itmProjectRunParameters);
end;

procedure TMainIDEBar.SetupToolsMenu;
begin
  itmToolConfigure := TMenuItem.Create(Self);
  itmToolConfigure.Name:='itmToolConfigure';
  itmToolConfigure.Caption := lisMenuSettings;
  mnuTools.Add(itmToolConfigure);

  mnuTools.Add(CreateMenuSeparator);

  itmToolSyntaxCheck := TMenuItem.Create(Self);
  itmToolSyntaxCheck.Name:='itmToolSyntaxCheck';
  itmToolSyntaxCheck.Caption := lisMenuQuickSyntaxCheck;
  mnuTools.Add(itmToolSyntaxCheck);

  itmToolGuessUnclosedBlock := TMenuItem.Create(Self);
  itmToolGuessUnclosedBlock.Name:='itmToolGuessUnclosedBlock';
  itmToolGuessUnclosedBlock.Caption := lisMenuGuessUnclosedBlock;
  mnuTools.Add(itmToolGuessUnclosedBlock);

  itmToolGuessMisplacedIFDEF := TMenuItem.Create(Self);
  itmToolGuessMisplacedIFDEF.Name:='itmToolGuessMisplacedIFDEF';
  itmToolGuessMisplacedIFDEF.Caption := lisMenuGuessMisplacedIFDEF;
  mnuTools.Add(itmToolGuessMisplacedIFDEF);

  mnuTools.Add(CreateMenuSeparator);

  itmToolConvertDFMtoLFM := TMenuItem.Create(Self);
  itmToolConvertDFMtoLFM.Name:='itmToolConvertDFMtoLFM';
  itmToolConvertDFMtoLFM.Caption := lisMenuConvertDFMtoLFM;
  mnuTools.Add(itmToolConvertDFMtoLFM);

  mnuTools.Add(CreateMenuSeparator);

  itmToolBuildLazarus := TMenuItem.Create(Self);
  itmToolBuildLazarus.Name:='itmToolBuildLazarus';
  itmToolBuildLazarus.Caption := lisMenuBuildLazarus;
  mnuTools.Add(itmToolBuildLazarus);

  itmToolConfigureBuildLazarus := TMenuItem.Create(Self);
  itmToolConfigureBuildLazarus.Name:='itmToolConfigureBuildLazarus';
  itmToolConfigureBuildLazarus.Caption := lisMenuConfigureBuildLazarus;
  mnuTools.Add(itmToolConfigureBuildLazarus);
end;

procedure TMainIDEBar.SetupEnvironmentMenu;
begin
  itmEnvGeneralOptions := TMenuItem.Create(Self);
  itmEnvGeneralOptions.Name:='itmEnvGeneralOptions';
  itmEnvGeneralOptions.Caption := lisMenuGeneralOptions;
  mnuEnvironment.Add(itmEnvGeneralOptions);

  itmEnvEditorOptions := TMenuItem.Create(Self);
  itmEnvEditorOptions.Name:='itmEnvEditorOptions';
  itmEnvEditorOptions.Caption := lisMenuEditorOptions;
  mnuEnvironment.Add(itmEnvEditorOptions);

  itmEnvCodeToolsOptions := TMenuItem.Create(Self);
  itmEnvCodeToolsOptions.Name:='itmEnvCodeToolsOptions';
  itmEnvCodeToolsOptions.Caption := lisMenuCodeToolsOptions;
  mnuEnvironment.Add(itmEnvCodeToolsOptions);

  itmEnvCodeToolsDefinesEditor := TMenuItem.Create(Self);
  itmEnvCodeToolsDefinesEditor.Name:='itmEnvCodeToolsDefinesEditor';
  itmEnvCodeToolsDefinesEditor.Caption := lisMenuCodeToolsDefinesEditor;
  mnuEnvironment.Add(itmEnvCodeToolsDefinesEditor);
end;

procedure TMainIDEBar.SetupHelpMenu;
begin
  itmHelpAboutLazarus := TMenuItem.Create(Self);
  itmHelpAboutLazarus.Name:='itmHelpAboutLazarus';
  itmHelpAboutLazarus.Caption := lisMenuAboutLazarus;
  mnuHelp.Add(itmHelpAboutLazarus);
end;

procedure TMainIDEBar.LoadMenuShortCuts;
begin
  with EditorOpts.KeyMap do begin
    itmFileNewUnit.ShortCut:=CommandToShortCut(ecNewUnit);
    itmFileNewForm.ShortCut:=CommandToShortCut(ecNewForm);
    itmFileOpen.ShortCut:=CommandToShortCut(ecOpen);
    itmFileRevert.ShortCut:=CommandToShortCut(ecRevert);
    //itmFileRecentOpen.ShortCut:=CommandToShortCut(ec);
    itmFileSave.ShortCut:=CommandToShortCut(ecSave);
    itmFileSaveAs.ShortCut:=CommandToShortCut(ecSaveAs);
    itmFileSaveAll.ShortCut:=CommandToShortCut(ecSaveAll);
    itmFileClose.ShortCut:=CommandToShortCut(ecClose);
    itmFileCloseAll.ShortCut:=CommandToShortCut(ecCloseAll);
    itmFileQuit.ShortCut:=CommandToShortCut(ecQuit);

    itmEditUndo.ShortCut:=CommandToShortCut(ecUndo);
    itmEditRedo.ShortCut:=CommandToShortCut(ecRedo);
    itmEditCut.ShortCut:=CommandToShortCut(ecCut);
    itmEditCopy.ShortCut:=CommandToShortCut(ecCopy);
    itmEditPaste.ShortCut:=CommandToShortCut(ecPaste);
    itmEditIndentBlock.ShortCut:=CommandToShortCut(ecBlockIndent);
    itmEditUnindentBlock.ShortCut:=CommandToShortCut(ecBlockUnindent);
    itmEditUpperCaseBlock.ShortCut:=CommandToShortCut(ecSelectionUpperCase);
    itmEditLowerCaseBlock.ShortCut:=CommandToShortCut(ecSelectionLowerCase);
    itmEditTabsToSpacesBlock.ShortCut:=CommandToShortCut(ecSelectionTabs2Spaces);
    itmEditCommentBlock.ShortCut:=CommandToShortCut(ecSelectionComment);
    itmEditUncommentBlock.ShortCut:=CommandToShortCut(ecSelectionUncomment);
    itmEditSelectAll.ShortCut:=CommandToShortCut(ecSelectAll);
    itmEditSelectToBrace.ShortCut:=CommandToShortCut(ecSelectToBrace);
    itmEditSelectCodeBlock.ShortCut:=CommandToShortCut(ecSelectCodeBlock);
    itmEditSelectLine.ShortCut:=CommandToShortCut(ecSelectLine);
    itmEditSelectParagraph.ShortCut:=CommandToShortCut(ecSelectParagraph);
    itmEditCompleteCode.ShortCut:=CommandToShortCut(ecCompleteCode);

    itmEditInsertCVSAuthor.ShortCut:=CommandToShortCut(ecInsertCVSAuthor);
    itmEditInsertCVSDate.ShortCut:=CommandToShortCut(ecInsertCVSDate);
    itmEditInsertCVSHeader.ShortCut:=CommandToShortCut(ecInsertCVSHeader);
    itmEditInsertCVSID.ShortCut:=CommandToShortCut(ecInsertCVSID);
    itmEditInsertCVSLog.ShortCut:=CommandToShortCut(ecInsertCVSLog);
    itmEditInsertCVSName.ShortCut:=CommandToShortCut(ecInsertCVSName);
    itmEditInsertCVSRevision.ShortCut:=CommandToShortCut(ecInsertCVSRevision);
    itmEditInsertCVSSource.ShortCut:=CommandToShortCut(ecInsertCVSSource);

    itmEditInsertGPLNotice.ShortCut:=CommandToShortCut(ecInsertGPLNotice);
    itmEditInsertUsername.ShortCut:=CommandToShortCut(ecInsertUserName);
    itmEditInsertDateTime.ShortCut:=CommandToShortCut(ecInsertDateTime);
    itmEditInsertChangeLogEntry.ShortCut:=CommandToShortCut(ecInsertChangeLogEntry);

    itmSearchFind.ShortCut:=CommandToShortCut(ecFind);
    itmSearchFindNext.ShortCut:=CommandToShortCut(ecFindNext);
    itmSearchFindPrevious.ShortCut:=CommandToShortCut(ecFindPrevious);
    itmSearchFindInFiles.ShortCut:=CommandToShortCut(ecFindInFiles);
    itmSearchReplace.ShortCut:=CommandToShortCut(ecReplace);
    itmGotoLine.ShortCut:=CommandToShortCut(ecGotoLineNumber);
    itmJumpBack.ShortCut:=CommandToShortCut(ecJumpBack);
    itmJumpForward.ShortCut:=CommandToShortCut(ecJumpForward);
    itmAddJumpPoint.ShortCut:=CommandToShortCut(ecAddJumpPoint);
    itmJumpHistory.ShortCut:=CommandToShortCut(ecViewJumpHistory);
    itmFindBlockOtherEnd.ShortCut:=CommandToShortCut(ecFindBlockOtherEnd);
    itmFindBlockStart.ShortCut:=CommandToShortCut(ecFindBlockStart);
    itmFindDeclaration.ShortCut:=CommandToShortCut(ecFindDeclaration);
    itmOpenFileAtCursor.ShortCut:=CommandToShortCut(ecOpenFileAtCursor);
    itmGotoIncludeDirective.ShortCut:=CommandToShortCut(ecGotoIncludeDirective);

    itmViewInspector.ShortCut:=CommandToShortCut(ecToggleObjectInsp);
    itmViewProject.ShortCut:=CommandToShortCut(ecToggleProjectExpl);
    itmViewUnits.ShortCut:=CommandToShortCut(ecViewUnits);
    itmViewCodeExplorer.ShortCut:=CommandToShortCut(ecToggleCodeExpl);
    itmViewUnitDependencies.ShortCut:=CommandToShortCut(ecViewUnitDependencies);
    itmViewForms.ShortCut:=CommandToShortCut(ecViewForms);
    itmViewToggleFormUnit.ShortCut:=CommandToShortCut(ecToggleFormUnit);
    itmViewMessage.ShortCut:=CommandToShortCut(ecToggleMessages);

    itmProjectNew.ShortCut:=CommandToShortCut(ecNewProject);
    itmProjectNewFromFile.ShortCut:=CommandToShortCut(ecNewProjectFromFile);
    itmProjectOpen.ShortCut:=CommandToShortCut(ecOpenProject);
    //itmProjectRecentOpen.ShortCut:=CommandToShortCut(ec);
    itmProjectSave.ShortCut:=CommandToShortCut(ecSaveProject);
    itmProjectSaveAs.ShortCut:=CommandToShortCut(ecSaveProjectAs);
    itmProjectPublish.ShortCut:=CommandToShortCut(ecPublishProject);
    itmProjectAddTo.ShortCut:=CommandToShortCut(ecAddCurUnitToProj);
    itmProjectRemoveFrom.ShortCut:=CommandToShortCut(ecRemoveFromProj);
    itmProjectViewSource.ShortCut:=CommandToShortCut(ecViewProjectSource);
    itmProjectOptions.ShortCut:=CommandToShortCut(ecProjectOptions);

    itmProjectBuild.ShortCut:=CommandToShortCut(ecBuild);
    itmProjectBuildAll.ShortCut:=CommandToShortCut(ecBuildAll);
    itmProjectRun.ShortCut:=CommandToShortCut(ecRun);
    itmProjectPause.ShortCut:=CommandToShortCut(ecPause);
    itmProjectStepInto.ShortCut:=CommandToShortCut(ecStepInto);
    itmProjectStepOver.ShortCut:=CommandToShortCut(ecStepOver);
    itmProjectRunToCursor.ShortCut:=CommandToShortCut(ecRunToCursor);
    itmProjectStop.ShortCut:=CommandToShortCut(ecStopProgram);
    itmProjectCompilerSettings.ShortCut:=CommandToShortCut(ecCompilerOptions);
    itmProjectRunParameters.ShortCut:=CommandToShortCut(ecRunParameters);

    itmToolConfigure.ShortCut:=CommandToShortCut(ecExtToolSettings);
    itmToolSyntaxCheck.ShortCut:=CommandToShortCut(ecSyntaxCheck);
    itmToolGuessUnclosedBlock.ShortCut:=CommandToShortCut(ecGuessUnclosedBlock);
    itmToolGuessMisplacedIFDEF.ShortCut:=CommandToShortCut(ecGuessMisplacedIFDEF);
    itmToolConvertDFMtoLFM.ShortCut:=CommandToShortCut(ecConvertDFM2LFM);
    itmToolBuildLazarus.ShortCut:=CommandToShortCut(ecBuildLazarus);
    itmToolConfigureBuildLazarus.ShortCut:=CommandToShortCut(ecConfigBuildLazarus);

    itmEnvGeneralOptions.ShortCut:=CommandToShortCut(ecEnvironmentOptions);
    itmEnvEditorOptions.ShortCut:=CommandToShortCut(ecEditorOptions);
    itmEnvCodeToolsOptions.ShortCut:=CommandToShortCut(ecCodeToolsOptions);
    itmEnvCodeToolsDefinesEditor.ShortCut:=CommandToShortCut(ecCodeToolsDefinesEd);

    itmHelpAboutLazarus.ShortCut:=CommandToShortCut(ecAboutLazarus);
  end;
end;

{-------------------------------------------------------------------------------
  function TMainIDEBar.DoCheckAmbigiousSources(const AFilename: string
    ): TModalResult;

  Checks if file exists with same name and similar extension. The compiler
  prefers for example .pp to .pas files. So, if we save a .pas file delete .pp
  file, so that compiling does what is expected.
-------------------------------------------------------------------------------}
function TMainIDEBar.DoCheckAmbigiousSources(const AFilename: string;
  Compiling: boolean): TModalResult;

  function DeleteAmbigiousFile(const AmbigiousFilename: string): TModalResult;
  begin
    if not DeleteFile(AmbigiousFilename) then begin
      Result:=MessageDlg('Error deleting file',
       'Unable to delete ambigious file "'+AmbigiousFilename+'"',
       mtError,[mbOk,mbAbort],0);
    end else
      Result:=mrOk;
  end;

  function RenameAmbigiousFile(const AmbigiousFilename: string): TModalResult;
  var
    NewFilename: string;
  begin
    NewFilename:=AmbigiousFilename+'.ambigious';
    if not RenameFile(AmbigiousFilename,NewFilename) then
    begin
      Result:=MessageDlg('Error renaming file',
       'Unable to rename ambigious file "'+AmbigiousFilename+'"'#13
       +'to "'+NewFilename+'"',
       mtError,[mbOk,mbAbort],0);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbigiousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      TheOutputFilter.ReadLine('Warning: ambigious file found: "'+AmbigiousFilename+'"'
        +'. Source file is: "'+AFilename+'"',true);
    end;
  end;

  function CheckFile(const AmbigiousFilename: string): TModalResult;
  begin
    if not FileExists(AmbigiousFilename) then exit;
    if Compiling then begin
      Result:=AddCompileWarning(AmbigiousFilename);
      exit;
    end;
    case EnvironmentOptions.AmbigiousFileAction of
    afaAsk:
      begin
        Result:=MessageDlg('Ambigious file found',
          'There is a file with the same name and a similar extension ond disk'#13
          +'File: '+AFilename+#13
          +'Ambigious File: '+AmbigiousFilename+#13
          +#13
          +'Delete ambigious file?',
          mtWarning,[mbYes,mbIgnore,mbAbort],0);
        case Result of
        mrYes:    Result:=DeleteAmbigiousFile(AmbigiousFilename);
        mrIgnore: Result:=mrOk;
        end;
      end;

    afaAutoDelete:
      Result:=DeleteAmbigiousFile(AmbigiousFilename);

    afaAutoRename:
      Result:=RenameAmbigiousFile(AmbigiousFilename);

    afaWarnOnCompile:
      Result:=AddCompileWarning(AmbigiousFilename);

    else
      Result:=mrOk;
    end;
  end;

var
  Ext, LowExt: string;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbigiousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbigiousFileAction=afaWarnOnCompile)
  and not Compiling then exit;

  if FilenameIsPascalUnit(AFilename) then begin
    Ext:=ExtractFileExt(AFilename);
    LowExt:=lowercase(Ext);
    if LowExt='.pp' then
      Result:=CheckFile(ChangeFileExt(AFilename,'.pas'))
    else if LowExt='.pas' then
      Result:=CheckFile(ChangeFileExt(AFilename,'.pp'));
  end;
end;


end.

