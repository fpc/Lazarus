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
    procedure CreateMenuItem(MenuItemParent,MenuItem:TMenuItem;MenuItemName,MenuItemCaption:String;bmpName:String='';mnuEnabled:Boolean=true);
    procedure CreateMenuItemPkg(MenuItemParent,MenuItem:TMenuItem;MenuItemName,MenuItemCaption:String;bmpName:String='');
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

procedure TMainIDEBar.CreateMenuItem(MenuItemParent,MenuItem:TMenuItem;MenuItemName,MenuItemCaption:String;bmpName:String='';mnuEnabled:Boolean=true);
begin
  MenuItem:=TMenuItem.Create(Self);
  MenuItem.Name:=MenuItemName;
  MenuItem.Caption := MenuItemCaption;
  if not mnuEnabled then
   MenuItem.enabled:=mnuEnabled;
  if bmpName<>'' then
   MenuItem.Bitmap.LoadFromLazarusResource(bmpName);
  MenuItemParent.Add(MenuItem);
end;
procedure TMainIDEBar.CreateMenuItemPkg(MenuItemParent,MenuItem:TMenuItem;MenuItemName,MenuItemCaption:String;bmpName:String='');
begin
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name:=MenuItemName;
  MenuItem.Caption := MenuItemCaption;
  if bmpName<>'' then
   MenuItem.Bitmap.LoadFromLazarusResource(bmpName);
  {$IFNDEF DisablePkgs}
  MenuItemParent.Add(MenuItem);
  {$ENDIF}  
end;

procedure TMainIDEBar.SetupFileMenu;
begin

  CreateMenuItem(mnuFile,itmFileNewUnit,'itmFileNewUnit',lisMenuNewUnit,'menu_new');
  CreateMenuItem(mnuFile,itmFileNewForm,'itmFileNewForm',lisMenuNewForm,'menu_new');
  CreateMenuItem(mnuFile,itmFileNewOther,'itmFileNewOther',lisMenuNewOther,'menu_new');
    
  mnuFile.Add(CreateMenuSeparator);
 
  CreateMenuItem(mnuFile,itmFileOpen,'itmFileOpen',lisMenuOpen,'menu_open');
  CreateMenuItem(mnuFile,itmFileRevert,'itmFileRevert',lisMenuRevert,'menu_undo');
  CreateMenuItem(mnuFile,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent,'');
  CreateMenuItem(mnuFile,itmFileSave,'itmFileSave',lisMenuSave,'menu_save');
  CreateMenuItem(mnuFile,itmFileSaveAs,'itmFileSaveAs',lisMenuSaveAs,'menu_save');
  CreateMenuItem(mnuFile,itmFileSaveAll,'itmFileSaveAll',lisMenuSaveAll,'menu_save');
  CreateMenuItem(mnuFile,itmFileClose,'itmFileClose',lisMenuClose,'',false);
  CreateMenuItem(mnuFile,itmFileCloseAll,'itmFileCloseAll',lisMenuCloseAll,'',false);

  mnuFile.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuFile,itmFileCleanDirectory,'itmFileCleanDirectory',lisMenuCleanDirectory);
  
  mnuFile.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuFile,itmFileQuit,'itmFileQuit',lisMenuQuit);

end;

procedure TMainIDEBar.SetupEditMenu;
begin
  CreateMenuItem(mnuEdit,itmEditUndo,'itmEditUndo',lisMenuUndo,'menu_undo');
  CreateMenuItem(mnuEdit,itmEditRedo,'itmEditRedo',lisMenuRedo,'menu_redo');
  
  mnuEdit.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuEdit,itmEditCut,'itmEditCut',lisMenuCut,'menu_cut');  
  CreateMenuItem(mnuEdit,itmEditCopy,'itmEditCopy',lisMenuCopy,'menu_copy');  
  CreateMenuItem(mnuEdit,itmEditPaste,'itmEditPaste',lisMenuPaste,'menu_paste');  
  
  mnuEdit.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuEdit,itmEditIndentBlock,'itmEditIndentBlock',lisMenuIndentSelection,'menu_indent');  
  CreateMenuItem(mnuEdit,itmEditUnindentBlock,'itmEditUnindentBlock',lisMenuUnindentSelection,'menu_unindent');  
  CreateMenuItem(mnuEdit,itmEditEncloseBlock,'itmEditEncloseBlock',lisMenuEncloseSelection);  
  
  mnuEdit.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuEdit,itmEditUpperCaseBlock,'itmEditUpperCaseBlock',lisMenuUpperCaseSelection);  
  CreateMenuItem(mnuEdit,itmEditLowerCaseBlock,'itmEditLowerCaseBlock',lisMenuLowerCaseSelection);  

  mnuEdit.Add(CreateMenuSeparator);


  CreateMenuItem(mnuEdit,itmEditTabsToSpacesBlock,'itmEditTabsToSpacesBlock',lisMenuTabsToSpacesSelection);  
  CreateMenuItem(mnuEdit,itmEditSelectionBreakLines,'itmEditSelectionBreakLines',lisMenuBeakLinesInSelection);  

  mnuEdit.Add(CreateMenuSeparator);

  CreateMenuItem(mnuEdit,itmEditCommentBlock,'itmEditCommentBlock',lisMenuCommentSelection);  
  CreateMenuItem(mnuEdit,itmEditUncommentBlock,'itmEditUncommentBlock',lisMenuUncommentSelection);  
  CreateMenuItem(mnuEdit,itmEditSortBlock,'itmEditSortBlock',lisMenuSortSelection);  

  mnuEdit.Add(CreateMenuSeparator);

  CreateMenuItem(mnuEdit,itmEditSelect,'itmEditSelect',lisMenuSelect);  
  begin
     // select sub menu items
    CreateMenuItem(itmEditSelect,itmEditSelectAll,'itmEditSelectAll',lisMenuSelectAll);  
    CreateMenuItem(itmEditSelect,itmEditSelectToBrace,'itmEditSelectToBrace',lisMenuSelectToBrace);  
    CreateMenuItem(itmEditSelect,itmEditSelectCodeBlock,'itmEditSelectCodeBlock',lisMenuSelectCodeBlock);  
    CreateMenuItem(itmEditSelect,itmEditSelectLine,'itmEditSelectLine',lisMenuSelectLine);  
    CreateMenuItem(itmEditSelect,itmEditSelectParagraph,'itmEditSelectParagraph',lisMenuSelectParagraph);  
  end;
  
  CreateMenuItem(mnuEdit,itmEditInsertCharacter,'itmEditInsertCharacter',lisMenuInsertCharacter);  
  CreateMenuItem(mnuEdit,itmEditInsertText,'itmEditInsertText',lisMenuInsertText);  
   begin
    // insert text sub menu items
    CreateMenuItem(itmEditInsertText,itmEditInsertCVSKeyWord,'itmEditInsertCVSKeyWord',lisMenuInsertCVSKeyword);  
    begin
      // insert CVS keyword sub menu items
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSAuthor,'itmEditInsertCVSAuthor','Author');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSDate,'itmEditInsertCVSDate','Date');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSHeader,'itmEditInsertCVSHeader','Header');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSID,'itmEditInsertCVSID','ID');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSLog,'itmEditInsertCVSLog','Log');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSName,'itmEditInsertCVSName','Name');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSRevision,'itmEditInsertCVSRevision','Revision');  
      CreateMenuItem(itmEditInsertCVSKeyWord,itmEditInsertCVSSource,'itmEditInsertCVSSource','Source');  
    end;
    
    CreateMenuItem(itmEditInsertText,itmEditInsertGeneral,'itmEditInsertGeneral',lisMenuInsertGeneral);  
    begin
      // insert general text sub menu items
      CreateMenuItem(itmEditInsertGeneral,itmEditInsertGPLNotice,'itmEditInsertGPLNotice',lisMenuInsertGPLNotice);  
      CreateMenuItem(itmEditInsertGeneral,itmEditInsertLGPLNotice,'itmEditInsertLGPLNotice',lisMenuInsertLGPLNotice);  
      CreateMenuItem(itmEditInsertGeneral,itmEditInsertUsername,'itmEditInsertUsername',lisMenuInsertUsername);  
      CreateMenuItem(itmEditInsertGeneral,itmEditInsertDateTime,'itmEditInsertDateTime',lisMenuInsertDateTime);  
      CreateMenuItem(itmEditInsertGeneral,itmEditInsertChangeLogEntry,'itmEditInsertChangeLogEntry',lisMenuInsertChangeLogEntry);  
    end;
  end;
  
  mnuEdit.Add(CreateMenuSeparator);
  
 CreateMenuItem(mnuEdit,itmEditCompleteCode,'itmEditCompleteCode',lisMenuCompleteCode);  
 CreateMenuItem(mnuEdit,itmEditExtractProc,'itmEditExtractProc',lisMenuExtractProc);  

end;

procedure TMainIDEBar.SetupSearchMenu;
begin
  CreateMenuItem(mnuSearch,itmSearchFind,'itmSearchFind',lisMenuFind); 
  CreateMenuItem(mnuSearch,itmSearchFindNext,'itmSearchFindNext',lisMenuFindNext); 
  CreateMenuItem(mnuSearch,itmSearchFindPrevious,'itmSearchFindPrevious',lisMenuFindPrevious); 
  CreateMenuItem(mnuSearch,itmSearchFindInFiles,'itmSearchFindInFiles',lisMenuFindInFiles); 
  CreateMenuItem(mnuSearch,itmSearchReplace,'itmSearchReplace',lisMenuReplace); 
  CreateMenuItem(mnuSearch,itmIncrementalFind,'itmIncrementalFind',lisMenuIncrementalFind); 

  mnuSearch.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuSearch,itmGotoLine,'itmGotoLine',lisMenuGotoLine); 

  mnuSearch.Add(CreateMenuSeparator);

  CreateMenuItem(mnuSearch,itmJumpBack,'itmJumpBack',lisMenuJumpBack); 
  CreateMenuItem(mnuSearch,itmJumpForward,'itmJumpForward',lisMenuJumpForward); 
  CreateMenuItem(mnuSearch,itmAddJumpPoint,'itmAddJumpPoint',lisMenuAddJumpPointToHistory); 
  CreateMenuItem(mnuSearch,itmJumpHistory,'itmJumpHistory',lisMenuViewJumpHistory); 

  mnuSearch.Add(CreateMenuSeparator);

  CreateMenuItem(mnuSearch,itmFindBlockOtherEnd,'itmFindBlockOtherEnd',lisMenuFindBlockOtherEndOfCodeBlock); 
  CreateMenuItem(mnuSearch,itmFindBlockStart,'itmFindBlockStart',lisMenuFindCodeBlockStart); 

  mnuSearch.Add(CreateMenuSeparator);

  CreateMenuItem(mnuSearch,itmFindDeclaration,'itmFindDeclaration',lisMenuFindDeclarationAtCursor); 
  CreateMenuItem(mnuSearch,itmOpenFileAtCursor,'itmOpenFileAtCursor',lisMenuOpenFilenameAtCursor); 
  CreateMenuItem(mnuSearch,itmGotoIncludeDirective,'itmGotoIncludeDirective',lisMenuGotoIncludeDirective); 
end;

procedure TMainIDEBar.SetupViewMenu;
begin
  CreateMenuItem(mnuView,itmViewInspector,'itmViewInspector',lisMenuViewObjectInspector); 
  CreateMenuItem(mnuView,itmViewSourceEditor,'itmViewSourceEditor',lisMenuViewSourceEditor); 
  CreateMenuItem(mnuView,itmViewCodeExplorer,'itmViewCodeExplorer',lisMenuViewCodeExplorer); 
  mnuView.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuView,itmViewUnits,'itmViewUnits',lisMenuViewUnits); 
  CreateMenuItem(mnuView,itmViewForms,'itmViewForms',lisMenuViewForms); 
  CreateMenuItem(mnuView,itmViewUnitDependencies,'itmViewUnitDependencies',lisMenuViewUnitDependencies); 
  mnuView.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuView,itmViewToggleFormUnit,'itmViewToggleFormUnit',lisMenuViewToggleFormUnit); 
  mnuView.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuView,itmViewMessage,'itmViewMessage',lisMenuViewMessages); 
  CreateMenuItem(mnuView,itmViewSearchResults,'itmViewSearchResults',lisMenuViewSearchResults); 
  CreateMenuItem(mnuView,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'menu_debugger'); 
  CreateMenuItem(mnuView,itmViewWatches,'itmViewWatches',lisMenuViewWatches,'menu_watches'); 
  CreateMenuItem(mnuView,itmViewBreakPoints,'itmViewBreakPoints',lisMenuViewBreakPoints,'menu_breakpoints'); 
  CreateMenuItem(mnuView,itmViewLocals,'itmViewLocals',lisMenuViewLocalVariables,''); 
  CreateMenuItem(mnuView,itmViewCallStack,'itmViewCallStack',lisMenuViewCallStack,'menu_callstack'); 
  CreateMenuItem(mnuView,itmViewDebugOutput,'itmViewDebugOutput',lisMenuViewDebugOutput,'menu_debugoutput'); 
end;

procedure TMainIDEBar.SetupProjectMenu;
begin
  CreateMenuItem(mnuProject,itmProjectNew,'itmProjectNew',lisMenuNewProject); 
  CreateMenuItem(mnuProject,itmProjectNewFromFile,'itmProjectNewFromFile',lisMenuNewProjectFromFile); 
  mnuProject.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuProject,itmProjectOpen,'itmProjectOpen',lisMenuOpenProject,'menu_openproject'); 
  CreateMenuItem(mnuProject,itmProjectRecentOpen,'itmProjectRecentOpen',lisMenuOpenRecentProject); 
  mnuProject.Add(CreateMenuSeparator);
  

  CreateMenuItem(mnuProject,itmProjectSave,'itmProjectSave',lisMenuSaveProject); 
  CreateMenuItem(mnuProject,itmProjectSaveAs,'itmProjectSaveAs',lisMenuSaveProjectAs); 
  CreateMenuItem(mnuProject,itmProjectPublish,'itmProjectPublish',lisMenuPublishProject); 
  mnuProject.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuProject,itmProjectInspector,'itmProjectInspector',lisMenuProjectInspector,'menu_projectinspector'); 
  CreateMenuItem(mnuProject,itmProjectOptions,'itmProjectOptions',lisMenuProjectOptions,'menu_projectoptions'); 
  CreateMenuItem(mnuProject,itmProjectCompilerOptions,'itmProjectCompilerOptions',lisMenuCompilerOptions); 
  mnuProject.Add(CreateMenuSeparator);

  CreateMenuItem(mnuProject,itmProjectAddTo,'itmProjectAddTo',lisMenuAddToProject); 
  CreateMenuItem(mnuProject,itmProjectRemoveFrom,'itmProjectRemoveFrom',lisMenuRemoveFromProject); 
  mnuProject.Add(CreateMenuSeparator);
 
  CreateMenuItem(mnuProject,itmProjectViewSource,'itmProjectViewSource',lisMenuViewSource); 
  CreateMenuItem(mnuProject,itmProjectViewToDos,'itmProjectViewToDos',lisMenuViewProjectTodos); 
end;

procedure TMainIDEBar.SetupRunMenu;
begin
  CreateMenuItem(mnuRun,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build'); 
  CreateMenuItem(mnuRun,itmRunMenuBuildAll,'itmRunMenuBuildAll',lisMenuBuildAll,'menu_buildall'); 
  CreateMenuItem(mnuRun,itmRunMenuAbortBuild,'itmRunMenuAbortBuild',lisMenuAbortBuild); 
  mnuRun.Add(CreateMenuSeparator);

  CreateMenuItem(mnuRun,itmRunMenuRun,'itmRunMenuRun',lisMenuProjectRun,'menu_run'); 
  CreateMenuItem(mnuRun,itmRunMenuPause,'itmRunMenuPause',lisMenuPause,'menu_pause'); 
  CreateMenuItem(mnuRun,itmRunMenuStepInto,'itmRunMenuStepInto',lisMenuStepInto,'menu_stepinto'); 
  CreateMenuItem(mnuRun,itmRunMenuStepOver,'itmRunMenuStepOver',lisMenuStepOver,'menu_stepover'); 
  CreateMenuItem(mnuRun,itmRunMenuRunToCursor,'itmRunMenuRunToCursor',lisMenuRunToCursor); 
  CreateMenuItem(mnuRun,itmRunMenuStop,'itmRunMenuStop',lisMenuStop,''); 
  mnuRun.Add(CreateMenuSeparator);

  CreateMenuItem(mnuRun,itmRunMenuRunParameters,'itmRunMenuRunParameters',lisMenuRunParameters); 
  CreateMenuItem(mnuRun,itmRunMenuResetDebugger,'itmRunMenuResetDebugger',lisMenuResetDebugger); 
  mnuRun.Add(CreateMenuSeparator);

  CreateMenuItem(mnuRun,itmRunMenuBuildFile,'itmRunMenuBuildFile',lisMenuBuildFile); 
  CreateMenuItem(mnuRun,itmRunMenuRunFile,'itmRunMenuRunFile',lisMenuRunFile); 
  CreateMenuItem(mnuRun,itmRunMenuConfigBuildFile,'itmRunMenuConfigBuildFile',lisMenuConfigBuildFile); 
end;

procedure TMainIDEBar.SetupComponentsMenu;
begin
  CreateMenuItemPkg(mnuComponents,itmPkgOpenPackage,'itmPkgOpenPackage',lisMenuOpenPackage,'pkg_package'); 
  CreateMenuItemPkg(mnuComponents,itmPkgOpenPackageFile,'itmPkgOpenPackageFile',lisMenuOpenPackageFile,'pkg_package'); 
  CreateMenuItemPkg(mnuComponents,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg,'pkg_package'); 
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}
  
  CreateMenuItemPkg(mnuComponents,itmPkgAddCurUnitToPkg,'itmPkgAddCurUnitToPkg',lisMenuAddCurUnitToPkg,'pkg_addunittopackage'); 
  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}

  CreateMenuItemPkg(mnuComponents,itmPkgPkgGraph,'itmPkgPkgGraph',lisMenuPackageGraph,'pkg_packagegraph'); 

  {$IFNDEF DisablePkgs}
  mnuComponents.Add(CreateMenuSeparator);
  {$ENDIF}
  
  CreateMenuItem(mnuComponents,itmCompsConfigCustomComps,'itmCompsConfigCustomComps',lisMenuConfigCustomComps); 
   
end;

procedure TMainIDEBar.SetupToolsMenu;
begin
  CreateMenuItem(mnuTools,itmToolConfigure,'itmToolConfigure',lisMenuSettings); 
  mnuTools.Add(CreateMenuSeparator);

  CreateMenuItem(mnuTools,itmToolSyntaxCheck,'itmToolSyntaxCheck',lisMenuQuickSyntaxCheck); 
  CreateMenuItem(mnuTools,itmToolGuessUnclosedBlock,'itmToolGuessUnclosedBlock',lisMenuGuessUnclosedBlock); 
  CreateMenuItem(mnuTools,itmToolGuessMisplacedIFDEF,'itmToolGuessMisplacedIFDEF',lisMenuGuessMisplacedIFDEF); 
  CreateMenuItem(mnuTools,itmToolMakeResourceString,'itmToolMakeResourceString',lisMenuMakeResourceString); 
  CreateMenuItem(mnuTools,itmToolDiff,'itmToolDiff',lisMenuDiff); 
  mnuTools.Add(CreateMenuSeparator);

  CreateMenuItem(mnuTools,itmToolCheckLFM,'itmToolCheckLFM',lisMenuCheckLFM); 
  CreateMenuItem(mnuTools,itmToolConvertDelphiUnit,'itmToolConvertDelphiUnit',lisMenuConvertDelphiUnit); 
  CreateMenuItem(mnuTools,itmToolConvertDFMtoLFM,'itmToolConvertDFMtoLFM',lisMenuConvertDFMtoLFM); 
  mnuTools.Add(CreateMenuSeparator);
  
  CreateMenuItem(mnuTools,itmToolBuildLazarus,'itmToolBuildLazarus',lisMenuBuildLazarus,'menu_buildlazarus'); 
  CreateMenuItem(mnuTools,itmToolConfigureBuildLazarus,'itmToolConfigureBuildLazarus',lisMenuConfigureBuildLazarus); 
end;

procedure TMainIDEBar.SetupEnvironmentMenu;
begin
  CreateMenuItem(mnuEnvironment,itmEnvGeneralOptions,'itmEnvGeneralOptions',lisMenuGeneralOptions,'menu_environmentoptions'); 
  CreateMenuItem(mnuEnvironment,itmEnvEditorOptions,'itmEnvEditorOptions',lisMenuEditorOptions,'menu_editoroptions'); 
  CreateMenuItem(mnuEnvironment,itmEnvDebuggerOptions,'itmEnvDebuggerOptions',lisMenDebuggerOptions,''); 
  CreateMenuItem(mnuEnvironment,itmEnvCodeToolsOptions,'itmEnvCodeToolsOptions',lisMenuCodeToolsOptions,'menu_codetoolsoptions'); 
  CreateMenuItem(mnuEnvironment,itmEnvCodeToolsDefinesEditor,'itmEnvCodeToolsDefinesEditor',lisMenuCodeToolsDefinesEditor,'menu_codetoolsdefineseditor'); 
  mnuEnvironment.Add(CreateMenuSeparator);
  CreateMenuItem(mnuEnvironment,itmEnvRescanFPCSrcDir,'itmEnvRescanFPCSrcDir',lisMenuRescanFPCSourceDirectory); 
end;

procedure TMainIDEBar.SetupWindowsMenu;
begin

end;

procedure TMainIDEBar.SetupHelpMenu;
begin
  CreateMenuItem(mnuHelp,itmHelpAboutLazarus,'itmHelpAboutLazarus',lisMenuAboutLazarus); 
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
    itmProjectOptions.ShortCut:=CommandToShortCut(ecProjectOptions);
    itmProjectCompilerOptions.ShortCut:=CommandToShortCut(ecCompilerOptions);
    itmProjectAddTo.ShortCut:=CommandToShortCut(ecAddCurUnitToProj);
    itmProjectRemoveFrom.ShortCut:=CommandToShortCut(ecRemoveFromProj);
    itmProjectViewSource.ShortCut:=CommandToShortCut(ecViewProjectSource);

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

