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
  Classes, LCLType, LCLIntf, StdCtrls, Buttons, Menus, ComCtrls, SysUtils,
  Controls, Graphics, ExtCtrls, Dialogs, FileCtrl, Forms, CodeToolManager,
  CodeCache, AVL_Tree, SynEditKeyCmds, LazConf, LazarusIDEStrConsts,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
  {$IFDEF DisablePkgs}
  CompReg, IDEComp,
  {$ELSE}
  ComponentReg,
  {$ENDIF}
  Splash, TransferMacros, ObjectInspector, PropEdits,
  OutputFilter, IDEDefs, MsgView, EnvironmentOpts, EditorOptions,
  FormEditor, CompilerOptions, KeyMapping, IDEProcs, UnitEditor, Debugger,
  IDEOptionDefs, CodeToolsDefines, DialogProcs;

type
  // The IDE is at anytime in a specific state:
  TIDEToolStatus = (
    itNone,      // The default mode. All editing allowed.
    itExiting,   // the ide is shutting down
    itBuilder,   // compiling (the project, a package, an external tool)
                 //    Loading/Saving/Debugging is not allowed.
    itDebugger,  // debugging the project.
                 //    Loading/Saving/Compiling is not allowed.
    itCodeTools, // the CodeToolBoss is working and has called the progress
                 //    event.
    itCodeToolAborting,// the CodeToolBoss is working and is about to abort
    itCustom     // this state is not used yet.
    );

  // window in front
  TDisplayState = (
    dsSource,     // focussing sourcenotebook
    dsInspector,  // focussing object inspector after Source
    dsForm,       // focussing designer form
    dsInspector2  // focussing object inspector after form
    );

  // new file flags
  TNewFlag = (
    nfIsPartOfProject, // force IsPartOfProject,
                       //   default is to use a heuristic
    nfIsNotPartOfProject,// forbid IsPartOfProject
    nfOpenInEditor,    // open in editor
    nfSave,            // save file instantly
    nfAddToRecent,     // add file to recent files
    nfQuiet,           // less messages
    nfConvertMacros,   // replace macros in filename
    nfBeautifySrc,     // beautify custom source
    nfCreateDefaultSrc // create initial source based on the type
    );
  TNewFlags = set of TNewFlag;

  // save file flags
  TSaveFlag = (
    sfSaveAs,
    sfSaveToTestDir,
    sfProjectSaving,
    sfCheckAmbigiousFiles
    );
  TSaveFlags = set of TSaveFlag;
  
  // open file flags
  TOpenFlag = (
    ofProjectLoading,// this open is part of opening a whole project
    ofOnlyIfExists,  // do not auto create non existing files
    ofRevert,        // reload file if already open
    ofQuiet,         // less messages
    ofAddToRecent,   // add file to recent files
    ofRegularFile,   // open as regular file (e.g. do not open projects)
    ofVirtualFile,   // open the virtual file
    ofConvertMacros, // replace macros in filename
    ofUseCache,      // do not update file from disk
    ofMultiOpen      // set during loading multiple files
    );
  TOpenFlags = set of TOpenFlag;
  
  // revert file flags
  TRevertFlag = (
    rfQuiet
    );
  TRevertFlags = set of TRevertFlag;
  
  // close file flags
  TCloseFlag = (
    cfSaveFirst, // check if modified and save
    cfProjectClosing
    );
  TCloseFlags = set of TCloseFlag;
  
  // codetools flags
  TCodeToolsFlag = (
    ctfSwitchToFormSource, // bring source notebook to front and show source of
                           //   current designed form
    ctfActivateAbortMode   // activate the CodeToolBoss.Abortable mode
    );
  TCodeToolsFlags = set of TCodeToolsFlag;
  
  // find source flags
  TFindSourceFlag = (
    fsfSearchForProject,
    fsfUseIncludePaths,
    fsfUseDebugPath
    );
  TFindSourceFlags = set of TFindSourceFlag;
  
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
    itmSearchReplace: TMenuItem;
    itmIncrementalFind: TMenuItem;
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

    // view menu
    itmViewInspector: TMenuItem;
    itmViewSourceEditor: TMenuItem;
    itmViewUnits : TMenuItem;
    itmViewCodeExplorer : TMenuItem;
    itmViewForms : TMenuItem;
    itmViewUnitDependencies : TMenuItem;
    itmViewMessage : TMenuItem;
    itmViewSearchResults : TMenuItem;
    itmViewDebugWindows: TMenuItem;
    itmViewWatches: TMenuItem;
    itmViewBreakpoints: TMenuItem;
    itmViewLocals: TMenuItem;
    itmViewCallStack: TMenuItem;
    itmViewDebugOutput: TMenuItem;
    itmViewToggleFormUnit: TMenuItem;

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

    // run menu
    itmRunMenuBuild: TMenuItem;
    itmRunMenuBuildAll: TMenuItem;
    itmRunMenuAbortBuild: TMenuItem;
    itmRunMenuCompilerSettings: TMenuItem;
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
    itmPkgOpenRecent: TMenuItem;
    itmPkgAddCurUnitToPkg: TMenuItem;
    itmPkgPkgGraph: TMenuItem;
    itmCompsConfigCustomComps: TMenuItem;

    // tools menu
    itmToolConfigure: TMenuItem;
    itmToolSyntaxCheck: TMenuItem;
    itmToolGuessUnclosedBlock: TMenuItem;
    itmToolGuessMisplacedIFDEF: TMenuItem;
    itmToolCheckLFM: TMenuItem;
    itmToolConvertDelphiUnit: TMenuItem;
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

    // component palette
    ComponentNotebook : TNotebook;
    GlobalMouseSpeedButton: TSpeedButton;

    // hints. Note/ToDo: hints should be controlled by the lcl, this is a workaround
    HintTimer1 : TIdleTimer;
    HintWindow1 : THintWindow;
    
    procedure mnuWindowsItemClick(Sender: TObject);
  private
    FToolStatus: TIDEToolStatus;
  protected
    CurrentParsedCompilerOption: TParsedCompilerOptions;
    TheCompiler: TCompiler;
    TheOutputFilter: TOutputFilter;
    
    function CreateMenuSeparator : TMenuItem;
    procedure SetupFileMenu; virtual;
    procedure SetupEditMenu; virtual;
    procedure SetupSearchMenu; virtual;
    procedure SetupViewMenu; virtual;
    procedure SetupProjectMenu; virtual;
    procedure SetupRunMenu; virtual;
    procedure SetupComponentsMenu; virtual;
    procedure SetupToolsMenu; virtual;
    procedure SetupEnvironmentMenu; virtual;
    procedure SetupWindowsMenu; virtual;
    procedure SetupHelpMenu; virtual;
    
    procedure LoadMenuShortCuts; virtual;
    procedure SetToolStatus(const AValue: TIDEToolStatus); virtual;
  public
    MacroList: TTransferMacroList;
    HiddenWindowsOnRun: TList; // list of forms, that were automatically hidden
                               // and will be shown when debugged program stops

    property ToolStatus: TIDEToolStatus read FToolStatus write SetToolStatus;
    procedure UpdateCaption; virtual; abstract;
    procedure HideIDE; virtual; abstract;
    procedure UnhideIDE; virtual; abstract;

    procedure CreateOftenUsedForms; virtual; abstract;

    function FindUnitFile(const AFilename: string): string; virtual; abstract;
    function FindSourceFile(const AFilename, BaseDirectory: string;
                            Flags: TFindSourceFlags): string; virtual; abstract;
    procedure GetCurrentUnit(var ActiveSourceEditor:TSourceEditor;
      var ActiveUnitInfo:TUnitInfo); virtual; abstract;
      
    function GetTestBuildDir: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;
    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
                        NeededFlags: TIDEFileStateFlags;
                        var ResultFlags: TIDEFileStateFlags); virtual; abstract;

    function DoNewEditorFile(NewUnitType: TNewUnitType;
        NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags): TModalResult; virtual; abstract;
    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoInitProjectRun: TModalResult; virtual; abstract;
    function DoOpenMacroFile(Sender: TObject;
        const AFilename: string): TModalResult; virtual;

    function DoShowProjectInspector: TModalResult; virtual; abstract;
    function DoImExportCompilerOptions(Sender: TObject): TModalResult; virtual; abstract;

    function PrepareForCompile: TModalResult; virtual; abstract;
    function DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; virtual; abstract;
    function DoExecuteCompilationTool(Tool: TCompilationTool;
                                      const WorkingDir, ToolTitle: string
                                      ): TModalResult; virtual; abstract;
    function DoSaveForBuild: TModalResult; virtual; abstract;
    function DoCheckFilesOnDisk: TModalResult; virtual; abstract;
    function DoPublishModul(Options: TPublishModuleOptions;
                            const SrcDirectory, DestDirectory: string
                            ): TModalResult; virtual; abstract;
    function DoCheckAmbigiousSources(const AFilename: string;
                                     Compiling: boolean): TModalResult; virtual;
    function DoCheckCreatingFile(const AFilename: string;
                                 CheckReadable: boolean): TModalResult; virtual;
    function DoSaveStringToFile(const Filename, Src,
                                FileDescription: string): TModalResult; virtual; abstract;
    function DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
                                    const AFilename: string;
                                    IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoBackupFile(const Filename:string;
      IsPartOfProject:boolean): TModalResult; virtual; abstract;
    function DoDeleteAmbigiousFiles(const Filename:string
                                    ): TModalResult; virtual;
    function DoCheckUnitPathForAmbigiousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; virtual;

    procedure UpdateWindowsMenu; virtual;
    procedure SaveEnvironment; virtual; abstract;
    procedure SetRecentSubMenu(ParentMenuItem: TMenuItem; FileList: TStringList;
       OnClickEvent: TNotifyEvent); virtual;
    function DoJumpToSourcePos(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               AddJumpPoint: boolean): TModalResult; virtual; abstract;
    function DoJumpToCodePos(
      ActiveSrcEdit: TSourceEditor; ActiveUnitInfo: TUnitInfo;
      NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
      AddJumpPoint: boolean): TModalResult; virtual; abstract;
    procedure DoJumpToCodeToolBossError; virtual; abstract;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); virtual; abstract;
  end;

var
  MainIDE : TMainIDEBar;

  ObjectInspector1 : TObjectInspector;
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
     'ofVirtualFile',
     'ofConvertMacros',
     'ofUseCache',
     'ofMultiOpen'
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


type
  TUnitFile = record
    UnitName: string;
    Filename: string;
  end;
  PUnitFile = ^TUnitFile;

function CompareUnitFiles(UnitFile1, UnitFile2: PUnitFile): integer;
begin
  Result:=AnsiCompareText(UnitFile1^.UnitName,UnitFile2^.UnitName);
end;

function CompareUnitNameAndUnitFile(UnitName: PChar;
  UnitFile: PUnitFile): integer;
begin
  Result:=CompareStringPointerI(UnitName,PChar(UnitFile^.UnitName));
end;

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

procedure TMainIDEBar.mnuWindowsItemClick(Sender: TObject);
var
  i: Integer;
begin
  i:=Screen.CustomFormCount-1;
  while (i>=0) do begin
    if Screen.CustomForms[i].Caption=TMenuItem(Sender).Caption then begin
      Screen.CustomForms[i].BringToFront;
      break;
    end;
    dec(i);
  end;
end;

procedure TMainIDEBar.SetToolStatus(const AValue: TIDEToolStatus);
begin
  if FToolStatus=AValue then exit;
  FToolStatus:=AValue;
  UpdateCaption;
end;

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
  itmFileNewUnit.Bitmap:=LoadPixmap('menu_new');
  mnuFile.Add(itmFileNewUnit);

  itmFileNewForm := TMenuItem.Create(Self);
  itmFileNewForm.Name:='itmFileNewForm';
  itmFileNewForm.Caption := lisMenuNewForm;
  itmFileNewForm.Bitmap:=LoadPixmap('menu_new');
  mnuFile.Add(itmFileNewForm);

  itmFileNewOther := TMenuItem.Create(Self);
  itmFileNewOther.Name:='itmFileNewOther';
  itmFileNewOther.Caption := lisMenuNewOther;
  itmFileNewOther.Bitmap:=LoadPixmap('menu_new');
  mnuFile.Add(itmFileNewOther);

  mnuFile.Add(CreateMenuSeparator);

  itmFileOpen := TMenuItem.Create(Self);
  itmFileOpen.Name:='itmFileOpen';
  itmFileOpen.Caption := lisMenuOpen;
  itmFileOpen.Bitmap:=LoadPixmap('menu_open');
  mnuFile.Add(itmFileOpen);

  itmFileRevert := TMenuItem.Create(Self);
  itmFileRevert.Name:='itmFileRevert';
  itmFileRevert.Caption := lisMenuRevert;
  itmFileRevert.Bitmap:=LoadPixmap('menu_undo');
  mnuFile.Add(itmFileRevert);

  itmFileRecentOpen := TMenuItem.Create(Self);
  itmFileRecentOpen.Name:='itmFileRecentOpen';
  itmFileRecentOpen.Caption := lisMenuOpenRecent;
  mnuFile.Add(itmFileRecentOpen);

  itmFileSave := TMenuItem.Create(Self);
  itmFileSave.Name:='itmFileSave';
  itmFileSave.Caption := lisMenuSave;
  itmFileSave.Bitmap:=LoadPixmap('menu_save');
  mnuFile.Add(itmFileSave);

  itmFileSaveAs := TMenuItem.Create(Self);
  itmFileSaveAs.Name:='itmFileSaveAs';
  itmFileSaveAs.Caption := lisMenuSaveAs;
  itmFileSaveAs.Bitmap:=LoadPixmap('menu_save');
  mnuFile.Add(itmFileSaveAs);

  itmFileSaveAll := TMenuItem.Create(Self);
  itmFileSaveAll.Name:='itmFileSaveAll';
  itmFileSaveAll.Caption := lisMenuSaveAll;
  itmFileSaveAll.Bitmap:=LoadPixmap('menu_save');
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

  itmFileCleanDirectory := TMenuItem.Create(Self);
  itmFileCleanDirectory.Name:='itmFileCleanDirectory';
  itmFileCleanDirectory.Caption := lisMenuCleanDirectory;
  mnuFile.Add(itmFileCleanDirectory);

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
  itmEditUndo.Bitmap:=LoadPixmap('menu_undo');
  mnuEdit.Add(itmEditUndo);

  itmEditRedo := TMenuItem.Create(Self);
  itmEditRedo.Name:='itmEditRedo';
  itmEditRedo.Caption := lisMenuRedo;
  itmEditRedo.Bitmap:=LoadPixmap('menu_redo');
  mnuEdit.Add(itmEditRedo);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditCut  := TMenuItem.Create(Self);
  itmEditCut.Name:='itmEditCut';
  itmEditCut.Caption := lisMenuCut;
  itmEditCut.Bitmap:=LoadPixmap('menu_cut');
  mnuEdit.Add(itmEditCut);

  itmEditCopy := TMenuItem.Create(Self);
  itmEditCopy.Name:='itmEditCopy';
  itmEditCopy.Caption := lisMenuCopy;
  itmEditCopy.Bitmap:=LoadPixmap('menu_copy');
  mnuEdit.Add(itmEditCopy);

  itmEditPaste := TMenuItem.Create(Self);
  itmEditPaste.Name:='itmEditPaste';
  itmEditPaste.Caption := lisMenuPaste;
  itmEditPaste.Bitmap:=LoadPixmap('menu_paste');
  mnuEdit.Add(itmEditPaste);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditIndentBlock := TMenuItem.Create(Self);
  itmEditIndentBlock.Name:='itmEditIndentBlock';
  itmEditIndentBlock.Caption := lisMenuIndentSelection;
  itmEditIndentBlock.Bitmap:=LoadPixmap('menu_indent');
  mnuEdit.Add(itmEditIndentBlock);

  itmEditUnindentBlock := TMenuItem.Create(Self);
  itmEditUnindentBlock.Name:='itmEditUnindentBlock';
  itmEditUnindentBlock.Caption := lisMenuUnindentSelection;
  itmEditUnindentBlock.Bitmap:=LoadPixmap('menu_unindent');
  mnuEdit.Add(itmEditUnindentBlock);

  itmEditEncloseBlock := TMenuItem.Create(Self);
  itmEditEncloseBlock.Name:='itmEditEncloseBlock';
  itmEditEncloseBlock.Caption := lisMenuEncloseSelection;
  mnuEdit.Add(itmEditEncloseBlock);

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

  itmEditSelectionBreakLines := TMenuItem.Create(Self);
  itmEditSelectionBreakLines.Name:='itmEditSelectionBreakLines';
  itmEditSelectionBreakLines.Caption := lisMenuBeakLinesInSelection;
  mnuEdit.Add(itmEditSelectionBreakLines);

  mnuEdit.Add(CreateMenuSeparator);

  itmEditCommentBlock := TMenuItem.Create(Self);
  itmEditCommentBlock.Name:='itmEditCommentBlock';
  itmEditCommentBlock.Caption := lisMenuCommentSelection;
  mnuEdit.Add(itmEditCommentBlock);

  itmEditUncommentBlock := TMenuItem.Create(Self);
  itmEditUncommentBlock.Name:='itmEditUncommentBlock';
  itmEditUncommentBlock.Caption := lisMenuUncommentSelection;
  mnuEdit.Add(itmEditUncommentBlock);

  itmEditSortBlock := TMenuItem.Create(Self);
  itmEditSortBlock.Name:='itmEditSortBlock';
  itmEditSortBlock.Caption := lisMenuSortSelection;
  mnuEdit.Add(itmEditSortBlock);

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

  itmEditInsertCharacter := TMenuItem.Create(Self);
  itmEditInsertCharacter.Name:='itmEditInsertCharacter';
  itmEditInsertCharacter.Caption := lisMenuInsertCharacter;
  mnuEdit.Add(itmEditInsertCharacter);

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

      itmEditInsertLGPLNotice := TMenuItem.Create(Self);
      itmEditInsertLGPLNotice.Name:='itmEditInsertLGPLNotice';
      itmEditInsertLGPLNotice.Caption := lisMenuInsertLGPLNotice;
      itmEditInsertGeneral.Add(itmEditInsertLGPLNotice);

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

  itmEditExtractProc := TMenuItem.Create(Self);
  itmEditExtractProc.Name:='itmEditExtractProc';
  itmEditExtractProc.Caption := lisMenuExtractProc;
  mnuEdit.Add(itmEditExtractProc);
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

  itmIncrementalFind := TMenuItem.Create(Self);
  itmIncrementalFind.Name:='itmIncrementalFind';
  itmIncrementalFind.Caption := lisMenuIncrementalFind;
  mnuSearch.add(itmIncrementalFind);

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

  itmViewSourceEditor := TMenuItem.Create(Self);
  itmViewSourceEditor.Name:='itmViewSourceEditor';
  itmViewSourceEditor.Caption := lisMenuViewSourceEditor;
  mnuView.Add(itmViewSourceEditor);

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

  itmViewSearchResults := TMenuItem.Create(Self);
  itmViewSearchResults.Name:='itmViewSearchResults';
  itmViewSearchResults.Caption := lisMenuViewSearchResults;
  mnuView.Add(itmViewSearchResults);

  itmViewDebugWindows := TMenuItem.Create(Self);
  itmViewDebugWindows.Name := 'itmViewDebugWindows';
  itmViewDebugWindows.Caption := lisMenuDebugWindows;
  itmViewDebugWindows.Bitmap:=LoadPixmap('menu_debugger');
  mnuView.Add(itmViewDebugWindows);

  itmViewWatches := TMenuItem.Create(Self);
  itmViewWatches.Name:='itmViewWatches';
  itmViewWatches.Caption := lisMenuViewWatches;
  itmViewWatches.Bitmap:=LoadPixmap('menu_watches');
  itmViewDebugWindows.Add(itmViewWatches);

  itmViewBreakPoints := TMenuItem.Create(Self);
  itmViewBreakPoints.Name:='itmViewBreakPoints';
  itmViewBreakPoints.Caption := lisMenuViewBreakPoints;
  itmViewBreakPoints.Bitmap:=LoadPixmap('menu_breakpoints');
  itmViewDebugWindows.Add(itmViewBreakPoints);

  itmViewLocals := TMenuItem.Create(Self);
  itmViewLocals.Name:='itmViewLocals';
  itmViewLocals.Caption := lisMenuViewLocalVariables;
  itmViewDebugWindows.Add(itmViewLocals);

  itmViewCallStack := TMenuItem.Create(Self);
  itmViewCallStack.Name:='itmViewCallStack';
  itmViewCallStack.Caption := lisMenuViewCallStack;
  itmViewCallStack.Bitmap:=LoadPixmap('menu_callstack');
  itmViewDebugWindows.Add(itmViewCallStack);

  itmViewDebugOutput := TMenuItem.Create(Self);
  itmViewDebugOutput.Name:='itmViewDebugOutput';
  itmViewDebugOutput.Caption := lisMenuViewDebugOutput;
  itmViewDebugOutput.Bitmap:=LoadPixmap('menu_debugoutput');
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
  itmProjectOpen.Bitmap:=LoadPixmap('menu_openproject');
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

  itmProjectInspector := TMenuItem.Create(Self);
  itmProjectInspector.Name:='itmProjectInspector';
  itmProjectInspector.Caption := lisMenuProjectInspector;
  itmProjectInspector.Bitmap:=LoadPixmap('menu_projectinspector');
  {$IFNDEF DisablePkgs}
  mnuProject.Add(itmProjectInspector);
  {$ENDIF}

  itmProjectAddTo := TMenuItem.Create(Self);
  itmProjectAddTo.Name:='itmProjectAddTo';
  itmProjectAddTo.Caption := lisMenuAddToProject;
  mnuProject.Add(itmProjectAddTo);

  itmProjectRemoveFrom := TMenuItem.Create(Self);
  itmProjectRemoveFrom.Name:='itmProjectRemoveFrom';
  itmProjectRemoveFrom.Caption := lisMenuRemoveFromProject;
  mnuProject.Add(itmProjectRemoveFrom);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectViewSource := TMenuItem.Create(Self);
  itmProjectViewSource.Name:='itmProjectViewSource';
  itmProjectViewSource.Caption := lisMenuViewSource;
  mnuProject.Add(itmProjectViewSource);

  itmProjectViewToDos := TMenuItem.Create(Self);
  itmProjectViewToDos.Name:='itmProjectViewToDos';
  itmProjectViewToDos.Caption := lisMenuViewProjectTodos;
  mnuProject.Add(itmProjectViewToDos);

  mnuProject.Add(CreateMenuSeparator);

  itmProjectOptions := TMenuItem.Create(Self);
  itmProjectOptions.Name:='itmProjectOptions';
  itmProjectOptions.Caption := lisMenuProjectOptions;
  itmProjectOptions.Bitmap:=LoadPixmap('menu_projectoptions');
  mnuProject.Add(itmProjectOptions);
end;

procedure TMainIDEBar.SetupRunMenu;
begin
  itmRunMenuBuild := TMenuItem.Create(Self);
  itmRunMenuBuild.Name:='itmRunMenuBuild';
  itmRunMenuBuild.Caption := lisMenuBuild;
  itmRunMenuBuild.Bitmap:=LoadPixmap('menu_build');
  mnuRun.Add(itmRunMenuBuild);

  itmRunMenuBuildAll := TMenuItem.Create(Self);
  itmRunMenuBuildAll.Name:='itmRunMenuBuildAll';
  itmRunMenuBuildAll.Caption := lisMenuBuildAll;
  itmRunMenuBuildAll.Bitmap:=LoadPixmap('menu_buildall');
  mnuRun.Add(itmRunMenuBuildAll);

  itmRunMenuAbortBuild := TMenuItem.Create(Self);
  itmRunMenuAbortBuild.Name:='itmRunMenuAbortBuild';
  itmRunMenuAbortBuild.Caption := lisMenuAbortBuild;
  mnuRun.Add(itmRunMenuAbortBuild);

  itmRunMenuCompilerSettings := TMenuItem.Create(Self);
  itmRunMenuCompilerSettings.Name:='itmRunMenuCompilerSettings';
  itmRunMenuCompilerSettings.Caption := lisMenuCompilerOptions;
  mnuRun.Add(itmRunMenuCompilerSettings);

  mnuRun.Add(CreateMenuSeparator);

  itmRunMenuRun := TMenuItem.Create(Self);
  itmRunMenuRun.Name:='itmRunMenuRun';
  itmRunMenuRun.Caption := lisMenuProjectRun;
  itmRunMenuRun.Bitmap:=LoadPixmap('menu_run');
  mnuRun.Add(itmRunMenuRun);

  itmRunMenuPause := TMenuItem.Create(Self);
  itmRunMenuPause.Name:='itmRunMenuPause';
  itmRunMenuPause.Caption := lisMenuPause;
  itmRunMenuPause.Enabled := false;
  itmRunMenuPause.Bitmap:=LoadPixmap('menu_pause');
  mnuRun.Add(itmRunMenuPause);

  itmRunMenuStepInto := TMenuItem.Create(Self);
  itmRunMenuStepInto.Name:='itmRunMenuStepInto';
  itmRunMenuStepInto.Caption := lisMenuStepInto;
  itmRunMenuStepInto.Bitmap:=LoadPixmap('menu_stepinto');
  mnuRun.Add(itmRunMenuStepInto);

  itmRunMenuStepOver := TMenuItem.Create(Self);
  itmRunMenuStepOver.Name:='itmRunMenuStepOver';
  itmRunMenuStepOver.Caption := lisMenuStepOver;
  itmRunMenuStepOver.Bitmap:=LoadPixmap('menu_stepover');
  mnuRun.Add(itmRunMenuStepOver);

  itmRunMenuRunToCursor := TMenuItem.Create(Self);
  itmRunMenuRunToCursor.Name:='itmRunMenuRunToCursor';
  itmRunMenuRunToCursor.Caption := lisMenuRunToCursor;
  mnuRun.Add(itmRunMenuRunToCursor);

  itmRunMenuStop := TMenuItem.Create(Self);
  itmRunMenuStop.Name:='itmRunMenuStop';
  itmRunMenuStop.Caption := lisMenuStop;
  mnuRun.Add(itmRunMenuStop);

  mnuRun.Add(CreateMenuSeparator);

  itmRunMenuRunParameters := TMenuItem.Create(Self);
  itmRunMenuRunParameters.Name:='itmRunMenuRunParameters';
  itmRunMenuRunParameters.Caption := lisMenuRunParameters;
  mnuRun.Add(itmRunMenuRunParameters);

  itmRunMenuResetDebugger := TMenuItem.Create(Self);
  itmRunMenuResetDebugger.Name:='itmRunMenuResetDebugger';
  itmRunMenuResetDebugger.Caption := lisMenuResetDebugger;
  mnuRun.Add(itmRunMenuResetDebugger);

  mnuRun.Add(CreateMenuSeparator);

  itmRunMenuBuildFile := TMenuItem.Create(Self);
  itmRunMenuBuildFile.Name:='itmRunMenuBuildFile';
  itmRunMenuBuildFile.Caption := lisMenuBuildFile;
  mnuRun.Add(itmRunMenuBuildFile);

  itmRunMenuRunFile := TMenuItem.Create(Self);
  itmRunMenuRunFile.Name:='itmRunMenuRunFile';
  itmRunMenuRunFile.Caption := lisMenuRunFile;
  mnuRun.Add(itmRunMenuRunFile);

  itmRunMenuConfigBuildFile := TMenuItem.Create(Self);
  itmRunMenuConfigBuildFile.Name:='itmRunMenuConfigBuildFile';
  itmRunMenuConfigBuildFile.Caption := lisMenuConfigBuildFile;
  mnuRun.Add(itmRunMenuConfigBuildFile);
end;

procedure TMainIDEBar.SetupComponentsMenu;
begin
  itmPkgOpenPackage := TMenuItem.Create(Self);
  itmPkgOpenPackage.Name:='itmPkgOpenPackage';
  itmPkgOpenPackage.Caption := lisMenuOpenPackage;
  itmPkgOpenPackage.Bitmap:=LoadPixmap('pkg_package');
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(itmPkgOpenPackage);
  {$ENDIF}

  itmPkgOpenPackageFile := TMenuItem.Create(Self);
  itmPkgOpenPackageFile.Name:='itmPkgOpenPackageFile';
  itmPkgOpenPackageFile.Caption := lisMenuOpenPackageFile;
  itmPkgOpenPackageFile.Bitmap:=LoadPixmap('pkg_package');
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(itmPkgOpenPackageFile);
  {$ENDIF}

  itmPkgOpenRecent := TMenuItem.Create(Self);
  itmPkgOpenRecent.Name:='itmPkgOpenRecent';
  itmPkgOpenRecent.Caption := lisMenuOpenRecentPkg;
  itmPkgOpenRecent.Bitmap:=LoadPixmap('pkg_package');
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(itmPkgOpenRecent);
  {$ENDIF}

  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}

  itmPkgAddCurUnitToPkg := TMenuItem.Create(Self);
  itmPkgAddCurUnitToPkg.Name:='itmPkgAddCurUnitToPkg';
  itmPkgAddCurUnitToPkg.Caption := lisMenuAddCurUnitToPkg;
  itmPkgAddCurUnitToPkg.Bitmap:=LoadPixmap('pkg_addunittopackage');
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(itmPkgAddCurUnitToPkg);
  {$ENDIF}

  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}

  itmPkgPkgGraph := TMenuItem.Create(Self);
  itmPkgPkgGraph.Name:='itmPkgPkgGraph';
  itmPkgPkgGraph.Caption := lisMenuPackageGraph;
  itmPkgPkgGraph.Bitmap:=LoadPixmap('pkg_packagegraph');
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(itmPkgPkgGraph);
  {$ENDIF}

  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}

  itmCompsConfigCustomComps := TMenuItem.Create(Self);
  itmCompsConfigCustomComps.Name:='itmCompsConfigCustomComps';
  itmCompsConfigCustomComps.Caption := lisMenuConfigCustomComps;
  mnuComponents.Add(itmCompsConfigCustomComps);
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

  itmToolMakeResourceString := TMenuItem.Create(Self);
  itmToolMakeResourceString.Name:='itmToolMakeResourceString';
  itmToolMakeResourceString.Caption := lisMenuMakeResourceString;
  mnuTools.Add(itmToolMakeResourceString);

  itmToolDiff := TMenuItem.Create(Self);
  itmToolDiff.Name:='itmToolDiff';
  itmToolDiff.Caption := lisMenuDiff;
  mnuTools.Add(itmToolDiff);

  mnuTools.Add(CreateMenuSeparator);

  itmToolCheckLFM := TMenuItem.Create(Self);
  itmToolCheckLFM.Name:='itmToolCheckLFM';
  itmToolCheckLFM.Caption := lisMenuCheckLFM;
  mnuTools.Add(itmToolCheckLFM);

  itmToolConvertDelphiUnit := TMenuItem.Create(Self);
  itmToolConvertDelphiUnit.Name:='itmToolConvertDelphiUnit';
  itmToolConvertDelphiUnit.Caption := lisMenuConvertDelphiUnit;
  mnuTools.Add(itmToolConvertDelphiUnit);

  itmToolConvertDFMtoLFM := TMenuItem.Create(Self);
  itmToolConvertDFMtoLFM.Name:='itmToolConvertDFMtoLFM';
  itmToolConvertDFMtoLFM.Caption := lisMenuConvertDFMtoLFM;
  mnuTools.Add(itmToolConvertDFMtoLFM);

  mnuTools.Add(CreateMenuSeparator);

  itmToolBuildLazarus := TMenuItem.Create(Self);
  itmToolBuildLazarus.Name:='itmToolBuildLazarus';
  itmToolBuildLazarus.Caption := lisMenuBuildLazarus;
  itmToolBuildLazarus.Bitmap:=LoadPixmap('menu_buildlazarus');
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
  itmEnvGeneralOptions.Bitmap:=LoadPixmap('menu_environmentoptions');
  mnuEnvironment.Add(itmEnvGeneralOptions);

  itmEnvEditorOptions := TMenuItem.Create(Self);
  itmEnvEditorOptions.Name:='itmEnvEditorOptions';
  itmEnvEditorOptions.Caption := lisMenuEditorOptions;
  itmEnvEditorOptions.Bitmap:=LoadPixmap('menu_editoroptions');
  mnuEnvironment.Add(itmEnvEditorOptions);

  itmEnvDebuggerOptions := TMenuItem.Create(Self);
  itmEnvDebuggerOptions.Name:='itmEnvDebuggerOptions';
  itmEnvDebuggerOptions.Caption := lisMenDebuggerOptions;
//  itmEnvDebuggerOptions.Bitmap:=LoadPixmap('menu_editoroptions');
  mnuEnvironment.Add(itmEnvDebuggerOptions);

  itmEnvCodeToolsOptions := TMenuItem.Create(Self);
  itmEnvCodeToolsOptions.Name:='itmEnvCodeToolsOptions';
  itmEnvCodeToolsOptions.Caption := lisMenuCodeToolsOptions;
  itmEnvCodeToolsOptions.Bitmap:=LoadPixmap('menu_codetoolsoptions');
  mnuEnvironment.Add(itmEnvCodeToolsOptions);

  itmEnvCodeToolsDefinesEditor := TMenuItem.Create(Self);
  itmEnvCodeToolsDefinesEditor.Name:='itmEnvCodeToolsDefinesEditor';
  itmEnvCodeToolsDefinesEditor.Caption := lisMenuCodeToolsDefinesEditor;
  itmEnvCodeToolsDefinesEditor.Bitmap:=LoadPixmap('menu_codetoolsdefineseditor');
  mnuEnvironment.Add(itmEnvCodeToolsDefinesEditor);

  mnuEnvironment.Add(CreateMenuSeparator);

  itmEnvRescanFPCSrcDir := TMenuItem.Create(Self);
  itmEnvRescanFPCSrcDir.Name:='itmEnvRescanFPCSrcDir';
  itmEnvRescanFPCSrcDir.Caption := lisMenuRescanFPCSourceDirectory;
  mnuEnvironment.Add(itmEnvRescanFPCSrcDir);
end;

procedure TMainIDEBar.SetupWindowsMenu;
begin

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
    // file menu
    itmFileNewUnit.ShortCut:=CommandToShortCut(ecNewUnit);
    itmFileNewForm.ShortCut:=CommandToShortCut(ecNewForm);
    itmFileNewOther.ShortCut:=CommandToShortCut(ecNew);
    itmFileOpen.ShortCut:=CommandToShortCut(ecOpen);
    itmFileRevert.ShortCut:=CommandToShortCut(ecRevert);
    itmFileSave.ShortCut:=CommandToShortCut(ecSave);
    itmFileSaveAs.ShortCut:=CommandToShortCut(ecSaveAs);
    itmFileSaveAll.ShortCut:=CommandToShortCut(ecSaveAll);
    itmFileClose.ShortCut:=CommandToShortCut(ecClose);
    itmFileCloseAll.ShortCut:=CommandToShortCut(ecCloseAll);
    itmFileCleanDirectory.ShortCut:=CommandToShortCut(ecCleanDirectory);
    itmFileQuit.ShortCut:=CommandToShortCut(ecQuit);

    // edit menu
    itmEditUndo.ShortCut:=CommandToShortCut(ecUndo);
    itmEditRedo.ShortCut:=CommandToShortCut(ecRedo);
    itmEditCut.ShortCut:=CommandToShortCut(ecCut);
    itmEditCopy.ShortCut:=CommandToShortCut(ecCopy);
    itmEditPaste.ShortCut:=CommandToShortCut(ecPaste);
    itmEditIndentBlock.ShortCut:=CommandToShortCut(ecBlockIndent);
    itmEditUnindentBlock.ShortCut:=CommandToShortCut(ecBlockUnindent);
    itmEditEncloseBlock.ShortCut:=CommandToShortCut(ecSelectionEnclose);
    itmEditUpperCaseBlock.ShortCut:=CommandToShortCut(ecSelectionUpperCase);
    itmEditLowerCaseBlock.ShortCut:=CommandToShortCut(ecSelectionLowerCase);
    itmEditTabsToSpacesBlock.ShortCut:=CommandToShortCut(ecSelectionTabs2Spaces);
    itmEditCommentBlock.ShortCut:=CommandToShortCut(ecSelectionComment);
    itmEditUncommentBlock.ShortCut:=CommandToShortCut(ecSelectionUncomment);
    itmEditSortBlock.ShortCut:=CommandToShortCut(ecSelectionSort);
    itmEditSelectionBreakLines.ShortCut:=CommandToShortCut(ecSelectionBreakLines);
    itmEditSelectAll.ShortCut:=CommandToShortCut(ecSelectAll);
    itmEditSelectToBrace.ShortCut:=CommandToShortCut(ecSelectToBrace);
    itmEditSelectCodeBlock.ShortCut:=CommandToShortCut(ecSelectCodeBlock);
    itmEditSelectLine.ShortCut:=CommandToShortCut(ecSelectLine);
    itmEditSelectParagraph.ShortCut:=CommandToShortCut(ecSelectParagraph);
    itmEditCompleteCode.ShortCut:=CommandToShortCut(ecCompleteCode);
    itmEditExtractProc.ShortCut:=CommandToShortCut(ecExtractProc);

    itmEditInsertCVSAuthor.ShortCut:=CommandToShortCut(ecInsertCVSAuthor);
    itmEditInsertCVSDate.ShortCut:=CommandToShortCut(ecInsertCVSDate);
    itmEditInsertCVSHeader.ShortCut:=CommandToShortCut(ecInsertCVSHeader);
    itmEditInsertCVSID.ShortCut:=CommandToShortCut(ecInsertCVSID);
    itmEditInsertCVSLog.ShortCut:=CommandToShortCut(ecInsertCVSLog);
    itmEditInsertCVSName.ShortCut:=CommandToShortCut(ecInsertCVSName);
    itmEditInsertCVSRevision.ShortCut:=CommandToShortCut(ecInsertCVSRevision);
    itmEditInsertCVSSource.ShortCut:=CommandToShortCut(ecInsertCVSSource);

    itmEditInsertGPLNotice.ShortCut:=CommandToShortCut(ecInsertGPLNotice);
    itmEditInsertLGPLNotice.ShortCut:=CommandToShortCut(ecInsertLGPLNotice);
    itmEditInsertUsername.ShortCut:=CommandToShortCut(ecInsertUserName);
    itmEditInsertDateTime.ShortCut:=CommandToShortCut(ecInsertDateTime);
    itmEditInsertChangeLogEntry.ShortCut:=CommandToShortCut(ecInsertChangeLogEntry);

    // search menu
    itmSearchFind.ShortCut:=CommandToShortCut(ecFind);
    itmSearchFindNext.ShortCut:=CommandToShortCut(ecFindNext);
    itmSearchFindPrevious.ShortCut:=CommandToShortCut(ecFindPrevious);
    itmSearchFindInFiles.ShortCut:=CommandToShortCut(ecFindInFiles);
    itmSearchReplace.ShortCut:=CommandToShortCut(ecReplace);
    itmIncrementalFind.ShortCut:=CommandToShortCut(ecIncrementalFind);
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

    // view menu
    itmViewInspector.ShortCut:=CommandToShortCut(ecToggleObjectInsp);
    itmViewSourceEditor.ShortCut:=CommandToShortCut(ecToggleSourceEditor);
    itmViewUnits.ShortCut:=CommandToShortCut(ecViewUnits);
    itmViewCodeExplorer.ShortCut:=CommandToShortCut(ecToggleCodeExpl);
    itmViewUnitDependencies.ShortCut:=CommandToShortCut(ecViewUnitDependencies);
    itmViewForms.ShortCut:=CommandToShortCut(ecViewForms);
    itmViewToggleFormUnit.ShortCut:=CommandToShortCut(ecToggleFormUnit);
    itmViewMessage.ShortCut:=CommandToShortCut(ecToggleMessages);
    itmViewSearchResults.ShortCut:=CommandToShortCut(ecToggleSearchResults);

    // project menu
    itmProjectNew.ShortCut:=CommandToShortCut(ecNewProject);
    itmProjectNewFromFile.ShortCut:=CommandToShortCut(ecNewProjectFromFile);
    itmProjectOpen.ShortCut:=CommandToShortCut(ecOpenProject);
    itmProjectSave.ShortCut:=CommandToShortCut(ecSaveProject);
    itmProjectSaveAs.ShortCut:=CommandToShortCut(ecSaveProjectAs);
    itmProjectPublish.ShortCut:=CommandToShortCut(ecPublishProject);
    itmProjectInspector.ShortCut:=CommandToShortCut(ecProjectInspector);
    itmProjectAddTo.ShortCut:=CommandToShortCut(ecAddCurUnitToProj);
    itmProjectRemoveFrom.ShortCut:=CommandToShortCut(ecRemoveFromProj);
    itmProjectViewSource.ShortCut:=CommandToShortCut(ecViewProjectSource);
    itmProjectOptions.ShortCut:=CommandToShortCut(ecProjectOptions);

    // run menu
    itmRunMenuBuild.ShortCut:=CommandToShortCut(ecBuild);
    itmRunMenuBuildAll.ShortCut:=CommandToShortCut(ecBuildAll);
    itmRunMenuAbortBuild.ShortCut:=CommandToShortCut(ecAbortBuild);
    itmRunMenuRun.ShortCut:=CommandToShortCut(ecRun);
    itmRunMenuPause.ShortCut:=CommandToShortCut(ecPause);
    itmRunMenuStepInto.ShortCut:=CommandToShortCut(ecStepInto);
    itmRunMenuStepOver.ShortCut:=CommandToShortCut(ecStepOver);
    itmRunMenuRunToCursor.ShortCut:=CommandToShortCut(ecRunToCursor);
    itmRunMenuStop.ShortCut:=CommandToShortCut(ecStopProgram);
    itmRunMenuResetDebugger.ShortCut:=CommandToShortCut(ecResetDebugger);
    itmRunMenuCompilerSettings.ShortCut:=CommandToShortCut(ecCompilerOptions);
    itmRunMenuRunParameters.ShortCut:=CommandToShortCut(ecRunParameters);
    itmRunMenuBuildFile.ShortCut:=CommandToShortCut(ecBuildFile);
    itmRunMenuRunFile.ShortCut:=CommandToShortCut(ecRunFile);
    itmRunMenuConfigBuildFile.ShortCut:=CommandToShortCut(ecConfigBuildFile);

    // components menu
    itmPkgOpenPackage.ShortCut:=CommandToShortCut(ecOpenPackage);
    itmPkgOpenPackageFile.ShortCut:=CommandToShortCut(ecOpenPackageFile);
    itmPkgAddCurUnitToPkg.ShortCut:=CommandToShortCut(ecAddCurUnitToPkg);
    itmPkgPkgGraph.ShortCut:=CommandToShortCut(ecPackageGraph);
    itmCompsConfigCustomComps.ShortCut:=CommandToShortCut(ecConfigCustomComps);

    // tools menu
    itmToolConfigure.ShortCut:=CommandToShortCut(ecExtToolSettings);
    itmToolSyntaxCheck.ShortCut:=CommandToShortCut(ecSyntaxCheck);
    itmToolGuessUnclosedBlock.ShortCut:=CommandToShortCut(ecGuessUnclosedBlock);
    itmToolGuessMisplacedIFDEF.ShortCut:=CommandToShortCut(ecGuessMisplacedIFDEF);
    itmToolMakeResourceString.ShortCut:=CommandToShortCut(ecMakeResourceString);
    itmToolDiff.ShortCut:=CommandToShortCut(ecDiff);
    itmToolConvertDFMtoLFM.ShortCut:=CommandToShortCut(ecConvertDFM2LFM);
    itmToolCheckLFM.ShortCut:=CommandToShortCut(ecCheckLFM);
    itmToolConvertDelphiUnit.ShortCut:=CommandToShortCut(ecConvertDelphiUnit);
    itmToolBuildLazarus.ShortCut:=CommandToShortCut(ecBuildLazarus);
    itmToolConfigureBuildLazarus.ShortCut:=CommandToShortCut(ecConfigBuildLazarus);

    // environment menu
    itmEnvGeneralOptions.ShortCut:=CommandToShortCut(ecEnvironmentOptions);
    itmEnvEditorOptions.ShortCut:=CommandToShortCut(ecEditorOptions);
    itmEnvCodeToolsOptions.ShortCut:=CommandToShortCut(ecCodeToolsOptions);
    itmEnvCodeToolsDefinesEditor.ShortCut:=CommandToShortCut(ecCodeToolsDefinesEd);
    itmEnvRescanFPCSrcDir.ShortCut:=CommandToShortCut(ecRescanFPCSrcDir);

    // help menu
    itmHelpAboutLazarus.ShortCut:=CommandToShortCut(ecAboutLazarus);
  end;
end;

function TMainIDEBar.DoOpenMacroFile(Sender: TObject; const AFilename: string
  ): TModalResult;
begin
  Result:=DoOpenEditorFile(AFilename,-1,
                  [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros]);
end;

{-------------------------------------------------------------------------------
  function TMainIDEBar.DoCheckCreatingFile(const AFilename: string;
    CheckReadable: boolean): TModalResult;
-------------------------------------------------------------------------------}
function TMainIDEBar.DoCheckCreatingFile(const AFilename: string;
  CheckReadable: boolean): TModalResult;
var
  fs: TFileStream;
  c: char;
begin
  // create if not yet done
  if not FileExists(AFilename) then begin
    try
      fs:=TFileStream.Create(AFilename,fmCreate);
      fs.Free;
    except
      Result:=MessageDlg(lisUnableToCreateFile,
        Format(lisUnableToCreateFilename, ['"', AFilename, '"']), mtError, [
          mbCancel, mbAbort], 0);
      exit;
    end;
  end;
  // check writable
  try
    if CheckReadable then
      fs:=TFileStream.Create(AFilename,fmOpenWrite)
    else
      fs:=TFileStream.Create(AFilename,fmOpenReadWrite);
    try
      fs.Position:=fs.Size;
      fs.Write(' ',1);
    finally
      fs.Free;
    end;
  except
    Result:=MessageDlg(lisUnableToWriteFile,
      Format(lisUnableToWriteFilename, ['"', AFilename, '"']), mtError, [
        mbCancel, mbAbort], 0);
    exit;
  end;
  // check readable
  try
    fs:=TFileStream.Create(AFilename,fmOpenReadWrite);
    try
      fs.Position:=fs.Size-1;
      fs.Read(c,1);
    finally
      fs.Free;
    end;
  except
    Result:=MessageDlg(lisUnableToReadFile,
      Format(lisUnableToReadFilename, ['"', AFilename, '"']), mtError, [
        mbCancel, mbAbort], 0);
    exit;
  end;
  Result:=mrOk;
end;

function TMainIDEBar.DoDeleteAmbigiousFiles(const Filename: string
  ): TModalResult;
var
  ADirectory: String;
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurFilename: String;
  IsPascalUnit: Boolean;
  UnitName: String;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbigiousFileAction=afaIgnore then exit;
  if EnvironmentOptions.AmbigiousFileAction
    in [afaAsk,afaAutoDelete,afaAutoRename]
  then begin
    ADirectory:=AppendPathDelim(ExtractFilePath(Filename));
    if SysUtils.FindFirst(ADirectory+FindMask,faAnyFile,FileInfo)=0 then begin
      ShortFilename:=ExtractFileName(Filename);
      IsPascalUnit:=FilenameIsPascalUnit(ShortFilename);
      UnitName:=ExtractFilenameOnly(ShortFilename);
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..')
        or ((FileInfo.Attr and faDirectory)<>0) then continue;
        if (ShortFilename=FileInfo.Name) then continue;
        if (AnsiCompareText(ShortFilename,FileInfo.Name)<>0)
        and ((not IsPascalUnit) or (not FilenameIsPascalUnit(FileInfo.Name))
           or (AnsiCompareText(UnitName,ExtractFilenameOnly(FileInfo.Name))<>0))
        then
          continue;

        CurFilename:=ADirectory+FileInfo.Name;
        if EnvironmentOptions.AmbigiousFileAction=afaAsk then begin
          if MessageDlg(lisDeleteAmbigiousFile,
            Format(lisAmbigiousFileFoundThisFileCanBeMistakenWithDelete, ['"',
              CurFilename, '"', #13, '"', ShortFilename, '"', #13, #13]),
            mtConfirmation,[mbYes,mbNo],0)=mrNo
          then continue;
        end;
        if EnvironmentOptions.AmbigiousFileAction in [afaAutoDelete,afaAsk]
        then begin
          if not DeleteFile(CurFilename) then begin
            MessageDlg(lisDeleteFileFailed,
              Format(lisPkgMangUnableToDeleteFile, ['"', CurFilename, '"']),
              mtError,[mbOk],0);
          end;
        end else if EnvironmentOptions.AmbigiousFileAction=afaAutoRename then
        begin
          Result:=DoBackupFile(CurFilename,false);
          if Result=mrABort then exit;
          Result:=mrOk;
        end;
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    FindClose(FileInfo);
  end;
end;

{-------------------------------------------------------------------------------
  function TMainIDEBar.DoCheckUnitPathForAmbigiousPascalFiles(
    const BaseDir, TheUnitPath, CompiledExt, ContextDescription: string
    ): TModalResult;

  Collect all pascal files and all compiled units in the unit path and check
  for ambigious files. For example: doubles.
-------------------------------------------------------------------------------}
function TMainIDEBar.DoCheckUnitPathForAmbigiousPascalFiles(
  const BaseDir, TheUnitPath, CompiledExt, ContextDescription: string): TModalResult;
  
  procedure FreeUnitTree(var Tree: TAVLTree);
  var
    ANode: TAVLTreeNode;
    AnUnitFile: PUnitFile;
  begin
    if Tree<>nil then begin
      ANode:=Tree.FindLowest;
      while ANode<>nil do begin
        AnUnitFile:=PUnitFile(ANode.Data);
        Dispose(AnUnitFile);
        ANode:=Tree.FindSuccessor(ANode);
      end;
      Tree.Free;
      Tree:=nil;
    end;
  end;
  
var
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  FileInfo: TSearchRec;
  SourceUnitTree, CompiledUnitTree: TAVLTree;
  ANode: TAVLTreeNode;
  CurUnitName: String;
  CurFilename: String;
  AnUnitFile: PUnitFile;
  CurUnitTree: TAVLTree;
  FileInfoNeedClose: Boolean;
  UnitPath: String;
begin
  Result:=mrOk;
  UnitPath:=TrimSearchPath(TheUnitPath,BaseDir);

  //writeln('TMainIDEBar.DoCheckUnitPathForAmbigiousPascalFiles A UnitPath="',UnitPath,'" Ext=',CompiledExt,' Context=',ContextDescription);

  SourceUnitTree:=TAVLTree.Create(@CompareUnitFiles);
  CompiledUnitTree:=TAVLTree.Create(@CompareUnitFiles);
  FileInfoNeedClose:=false;
  try
    // collect all units (.pas, .pp, compiled units)
    EndPos:=1;
    while EndPos<=length(UnitPath) do begin
      StartPos:=EndPos;
      while (StartPos<=length(UnitPath)) and (UnitPath[StartPos]=';') do
        inc(StartPos);
      EndPos:=StartPos;
      while (EndPos<=length(UnitPath)) and (UnitPath[EndPos]<>';') do
        inc(EndPos);
      if EndPos>StartPos then begin
        CurDir:=AppendPathDelim(TrimFilename(copy(
                                             UnitPath,StartPos,EndPos-StartPos)));
        FileInfoNeedClose:=true;
        if SysUtils.FindFirst(CurDir+FindMask,faAnyFile,FileInfo)=0 then begin
          repeat
            if (FileInfo.Name='.') or (FileInfo.Name='..')
            or ((FileInfo.Attr and faDirectory)<>0) then continue;
            if FilenameIsPascalUnit(FileInfo.Name) then
              CurUnitTree:=SourceUnitTree
            else if (CompareFileExt(FileInfo.Name,CompiledExt,false)=0) then
              CurUnitTree:=CompiledUnitTree
            else
              continue;
            CurUnitName:=ExtractFilenameOnly(FileInfo.Name);
            CurFilename:=CurDir+FileInfo.Name;
            // check if unit already found
            ANode:=CurUnitTree.FindKey(PChar(CurUnitName),
                                       @CompareUnitNameAndUnitFile);
            if ANode<>nil then begin
              // pascal unit exists twice
              Result:=MessageDlg('Ambigious unit found',
                'The unit '+CurUnitName+' exists twice in the unit path of the '
                +ContextDescription+':'#13
                +#13
                +'1. "'+PUnitFile(ANode.Data)^.Filename+'"'#13
                +'2. "'+CurFilename+'"'#13
                +#13
                +'Hint: Check if two packages contain a unit with the same name.',
                mtWarning,[mbAbort,mbIgnore],0);
              if Result<>mrIgnore then exit;
            end;
            // add unit to tree
            New(AnUnitFile);
            AnUnitFile^.UnitName:=CurUnitName;
            AnUnitFile^.Filename:=CurFilename;
            CurUnitTree.Add(AnUnitFile);
          until SysUtils.FindNext(FileInfo)<>0;
        end;
        FindClose(FileInfo);
        FileInfoNeedClose:=false;
      end;
    end;
  finally
    // clean up
    if FileInfoNeedClose then FindClose(FileInfo);
    FreeUnitTree(SourceUnitTree);
    FreeUnitTree(CompiledUnitTree);
  end;
  Result:=mrOk;
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
      Result:=MessageDlg(lisErrorDeletingFile,
       Format(lisUnableToDeleteAmbigiousFile, ['"', AmbigiousFilename, '"']),
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
      Result:=MessageDlg(lisErrorRenamingFile,
       Format(lisUnableToRenameAmbigiousFileTo, ['"', AmbigiousFilename, '"',
         #13, '"', NewFilename, '"']),
       mtError,[mbOk,mbAbort],0);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbigiousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      TheOutputFilter.ReadLine(Format(lisWarningAmbigiousFileFoundSourceFileIs,
        ['"', AmbigiousFilename, '"', '"', AFilename, '"']), true);
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
        Result:=MessageDlg(lisAmbigiousFileFound,
          Format(lisThereIsAFileWithTheSameNameAndASimilarExtension, [#13,
            AFilename, #13, AmbigiousFilename, #13, #13]),
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

procedure TMainIDEBar.UpdateWindowsMenu;
var
  WindowsList: TList;
  i: Integer;
  CurMenuItem: TMenuItem;
  AForm: TForm;
begin
  WindowsList:=TList.Create;
  // add typical IDE windows at the start of the list
  if (SourceNotebook<>nil) and (SourceNotebook.Visible) then
    WindowsList.Add(SourceNotebook);
  if (ObjectInspector1<>nil) and (ObjectInspector1.Visible) then
    WindowsList.Add(ObjectInspector1);
  // add special IDE windows
  for i:=0 to Screen.FormCount-1 do begin
    AForm:=Screen.Forms[i];
    if (AForm<>Self) and (AForm<>SplashForm)
    and (AForm.Designer=nil) and (AForm.Visible)
    and (WindowsList.IndexOf(AForm)<0) then
      WindowsList.Add(AForm);
  end;
  // add designer forms and datamodule forms
  for i:=0 to Screen.FormCount-1 do begin
    AForm:=Screen.Forms[i];
    if (AForm.Designer<>nil) and (WindowsList.IndexOf(AForm)<0) then
      WindowsList.Add(AForm);
  end;
  // create menuitems
  for i:=0 to WindowsList.Count-1 do begin
    if mnuWindows.Count>i then
      CurMenuItem:=mnuWindows.Items[i]
    else begin
      CurMenuItem:=TMenuItem.Create(Self);
      mnuWindows.Add(CurMenuItem);
      CurMenuItem.OnClick:=@mnuWindowsItemClick;
    end;
    CurMenuItem.Caption:=TCustomForm(WindowsList[i]).Caption;
  end;
  // remove unused menuitems
  while mnuWindows.Count>WindowsList.Count do
    mnuWindows.Items[mnuWindows.Count-1].Free;
  // clean up
  WindowsList.Free;
end;

procedure TMainIDEBar.SetRecentSubMenu(ParentMenuItem: TMenuItem;
  FileList: TStringList; OnClickEvent: TNotifyEvent);
var i: integer;
  AMenuItem: TMenuItem;
begin
  // create enough menuitems
  while ParentMenuItem.Count<FileList.Count do begin
    AMenuItem:=TMenuItem.Create(Self);
    AMenuItem.Name:=
      ParentMenuItem.Name+'Recent'+IntToStr(ParentMenuItem.Count);
    ParentMenuItem.Add(AMenuItem);
  end;
  // delete unused menuitems
  while ParentMenuItem.Count>FileList.Count do
    ParentMenuItem.Items[ParentMenuItem.Count-1].Free;
  // set captions and event
  for i:=0 to FileList.Count-1 do begin
    AMenuItem:=ParentMenuItem.Items[i];
    AMenuItem.Caption := FileList[i];
    AMenuItem.OnClick := OnClickEvent;
  end;
end;


end.

