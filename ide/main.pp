{  $Id$  }
{
 /***************************************************************************
                            main.pp  -  Toolbar
                            -------------------
                   TMainIDE is the application toolbar window.


                 Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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
unit Main;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

{ $DEFINE UseNewMenuEditor}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  // fpc packages
  Classes, SysUtils, Process, TypInfo,
  // codetools
  CodeToolsStructs, CodeToolManager, CodeCache, DefineTemplates,
  // lcl
  LCLType, LclLinux, LMessages, LResources, StdCtrls, Forms, Buttons, Menus,
  FileCtrl, Controls, Graphics, GraphType, ExtCtrls, Dialogs,
  // synedit
  SynEditKeyCmds,
  // compile
  Compiler, CompilerOptions,
  // projects
  Project, ProjectDefs, NewProjectDlg, ProjectOpts, PublishProjectDlg,
  ProjectInspector,
  // designer
  {$IFDEF EnablePkgs}
  ComponentPalette, ComponentReg,
  {$ELSE}
  CompReg, IDEComp,
  {$ENDIF}
  AbstractFormEditor, Designer, FormEditor, CustomFormEditor,
  ObjectInspector, PropEdits, ControlSelection, ColumnDlg,
  {$IFDEF UseNewMenuEditor}
  MenuEditorForm,
  {$ELSE}
  MenuPropEdit,
  {$ENDIF}
  // debugger
  RunParamsOpts, BaseDebugManager, DebugManager,
  // packager
  PkgManager, BasePkgManager,
  // source editing
  UnitEditor, EditDefineTree, CodeToolsOptions, IDEOptionDefs, CodeToolsDefines,
  DiffDialog, DiskDiffsDialog, UnitInfoDlg, EditorOptions, ViewUnit_dlg,
  // rest of the ide
  IDEDefs, LazarusIDEStrConsts, LazConf, MsgView, EnvironmentOpts,
  TransferMacros, KeyMapping, IDEProcs, ExtToolDialog, ExtToolEditDlg,
  MacroPromptDlg, OutputFilter, BuildLazDialog, MiscOptions,
  InputHistory, UnitDependencies, ClipBoardHistory, ProcessList,
  InitialSetupDlgs, NewDialog, MakeResStrDlg, ToDoList, AboutFrm,
  FindReplaceDialog, FindInFilesDlg,
  // main ide
  MainBar;

type
  TMainIDE = class(TMainIDEBar)
    // event handlers
    
    //procedure FormShow(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose: boolean);
    //procedure FormPaint(Sender : TObject);
    procedure OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
    procedure OnApplicationIdle(Sender: TObject);

    // file menu
    procedure mnuNewUnitClicked(Sender : TObject);
    procedure mnuNewFormClicked(Sender : TObject);
    procedure mnuNewOtherClicked(Sender : TObject);
    procedure mnuOpenClicked(Sender : TObject);
    procedure mnuOpenRecentClicked(Sender : TObject);
    procedure mnuRevertClicked(Sender : TObject);
    procedure mnuSaveClicked(Sender : TObject);
    procedure mnuSaveAsClicked(Sender : TObject);
    procedure mnuSaveAllClicked(Sender : TObject);
    procedure mnuCloseClicked(Sender : TObject);
    procedure mnuCloseAllClicked(Sender : TObject);
    procedure mnuQuitClicked(Sender : TObject);

    // edit menu
    procedure mnuEditUndoClicked(Sender: TObject);
    procedure mnuEditRedoClicked(Sender: TObject);
    procedure mnuEditCutClicked(Sender: TObject);
    procedure mnuEditCopyClicked(Sender: TObject);
    procedure mnuEditPasteClicked(Sender: TObject);
    procedure mnuEditIndentBlockClicked(Sender: TObject);
    procedure mnuEditUnindentBlockClicked(Sender: TObject);
    procedure mnuEditUpperCaseBlockClicked(Sender: TObject);
    procedure mnuEditLowerCaseBlockClicked(Sender: TObject);
    procedure mnuEditTabsToSpacesBlockClicked(Sender: TObject);
    procedure mnuEditCommentBlockClicked(Sender: TObject);
    procedure mnuEditUncommentBlockClicked(Sender: TObject);
    procedure mnuEditSortBlockClicked(Sender: TObject);
    procedure mnuEditSelectAllClick(Sender: TObject);
    procedure mnuEditSelectCodeBlockClick(Sender: TObject);
    procedure mnuEditSelectToBraceClick(Sender: TObject);
    procedure mnuEditSelectLineClick(Sender: TObject);
    procedure mnuEditSelectParagraphClick(Sender: TObject);
    procedure mnuEditCompleteCodeClicked(Sender: TObject);

    // edit->insert text->CVS keyword
    procedure mnuEditInsertCVSAuthorClick(Sender: TObject);
    procedure mnuEditInsertCVSDateClick(Sender: TObject);
    procedure mnuEditInsertCVSHeaderClick(Sender: TObject);
    procedure mnuEditInsertCVSIDClick(Sender: TObject);
    procedure mnuEditInsertCVSLogClick(Sender: TObject);
    procedure mnuEditInsertCVSNameClick(Sender: TObject);
    procedure mnuEditInsertCVSRevisionClick(Sender: TObject);
    procedure mnuEditInsertCVSSourceClick(Sender: TObject);

    // edit->insert text->general
    procedure mnuEditInsertGPLNoticeClick(Sender: TObject);
    procedure mnuEditInsertLGPLNoticeClick(Sender: TObject);
    procedure mnuEditInsertUsernameClick(Sender: TObject);
    procedure mnuEditInsertDateTimeClick(Sender: TObject);
    procedure mnuEditInsertChangeLogEntryClick(Sender: TObject);

    // search menu
    procedure mnuSearchFindBlockOtherEnd(Sender: TObject);
    procedure mnuSearchFindBlockStart(Sender: TObject);
    procedure mnuSearchFindDeclaration(Sender: TObject);
    procedure mnuFindDeclarationClicked(Sender : TObject);
    procedure mnuOpenFileAtCursorClicked(Sender : TObject);
    procedure mnuGotoIncludeDirectiveClicked(Sender : TObject);

    // view menu
    procedure mnuViewInspectorClicked(Sender : TObject);
    procedure mnuViewUnitsClicked(Sender : TObject);
    procedure mnuViewFormsClicked(Sender : TObject);
    procedure mnuViewUnitDependenciesClicked(Sender : TObject);
    procedure mnuViewCodeExplorerClick(Sender : TObject);
    procedure mnuViewMessagesClick(Sender : TObject);
    procedure mnuToggleFormUnitClicked(Sender : TObject);

    // project menu
    procedure mnuNewProjectClicked(Sender : TObject);
    procedure mnuNewProjectFromFileClicked(Sender : TObject);
    procedure mnuOpenProjectClicked(Sender : TObject);
    procedure mnuSaveProjectClicked(Sender : TObject);
    procedure mnuSaveProjectAsClicked(Sender : TObject);
    procedure mnuPublishProjectClicked(Sender : TObject);
    procedure mnuProjectInspectorClicked(Sender : TObject);
    procedure mnuAddToProjectClicked(Sender : TObject);
    procedure mnuRemoveFromProjectClicked(Sender : TObject);
    procedure mnuViewProjectSourceClicked(Sender : TObject);
    procedure mnuViewProjectTodosClicked(Sender : TObject);
    procedure mnuProjectOptionsClicked(Sender : TObject);
    
    // run menu
    procedure mnuBuildProjectClicked(Sender : TObject);
    procedure mnuBuildAllProjectClicked(Sender : TObject);
    procedure mnuRunProjectClicked(Sender : TObject);
    procedure mnuPauseProjectClicked(Sender : TObject);
    procedure mnuStepIntoProjectClicked(Sender : TObject);
    procedure mnuStepOverProjectClicked(Sender : TObject);
    procedure mnuRunToCursorProjectClicked(Sender : TObject);
    procedure mnuStopProjectClicked(Sender : TObject);
    procedure mnuRunParametersClicked(Sender : TObject);
    procedure mnuProjectCompilerSettingsClicked(Sender : TObject);
    
    // components menu

    // tools menu
    procedure mnuToolConfigureClicked(Sender : TObject);
    procedure mnuToolSyntaxCheckClicked(Sender : TObject);
    procedure mnuToolGuessUnclosedBlockClicked(Sender : TObject);
    procedure mnuToolGuessMisplacedIFDEFClicked(Sender : TObject);
    procedure mnuToolMakeResourceStringClicked(Sender : TObject);
    procedure mnuToolDiffClicked(Sender : TObject);
    procedure mnuToolConvertDFMtoLFMClicked(Sender : TObject);
    procedure mnuToolBuildLazarusClicked(Sender : TObject);
    procedure mnuToolConfigBuildLazClicked(Sender : TObject);
    procedure mnuCustomExtToolClick(Sender : TObject);

    // environment menu
    procedure mnuEnvGeneralOptionsClicked(Sender : TObject);
    procedure mnuEnvEditorOptionsClicked(Sender : TObject);
    procedure mnuEnvCodeToolsOptionsClicked(Sender : TObject);
    procedure mnuEnvCodeToolsDefinesEditorClicked(Sender : TObject);

    // windows menu

    // help menu
    procedure mnuHelpAboutLazarusClicked(Sender : TObject);

    procedure OpenFileDownArrowClicked(Sender : TObject);
    procedure mnuOpenFilePopupClick(Sender : TObject);
    {$IFNDEF EnablePkgs}
    procedure ControlClick(Sender : TObject);
    {$ENDIF}

  published
    // Global IDE events
    procedure OnProcessIDECommand(Sender: TObject; Command: word;
      var Handled: boolean);

    // Environment options dialog events
    procedure OnLoadEnvironmentSettings(Sender: TObject;
       TheEnvironmentOptions: TEnvironmentOptions);
    procedure OnSaveEnvironmentSettings(Sender: TObject;
       TheEnvironmentOptions: TEnvironmentOptions);
    procedure DoShowEnvGeneralOptions(StartPage: TEnvOptsDialogPage);

    // SourceNotebook events
    procedure OnSrcNoteBookActivated(Sender : TObject);
    procedure OnSrcNoteBookAddJumpPoint(ACaretXY: TPoint; ATopLine: integer; 
      APageIndex: integer; DeleteForwardHistory: boolean);
    procedure OnSrcNoteBookCtrlMouseUp(Sender : TObject;
      Button : TMouseButton; Shift: TShiftstate; X, Y: Integer);
    procedure OnSrcNotebookDeleteLastJumPoint(Sender: TObject);
    procedure OnSrcNotebookEditorVisibleChanged(Sender : TObject);
    procedure OnSrcNotebookEditorChanged(Sender : TObject);
    procedure OnSrcNotebookFileNew(Sender : TObject);
    procedure OnSrcNotebookFileOpen(Sender : TObject);
    procedure OnSrcNotebookFileOpenAtCursor(Sender : TObject);
    procedure OnSrcNotebookFileSave(Sender : TObject);
    procedure OnSrcNotebookFileSaveAs(Sender : TObject);
    procedure OnSrcNotebookFileClose(Sender : TObject);
    procedure OnSrcNotebookFindDeclaration(Sender : TObject);
    procedure OnSrcNotebookInitIdentCompletion(Sender: TObject;
      var Handled, Abort: boolean);
    procedure OnSrcNotebookJumpToHistoryPoint(var NewCaretXY: TPoint;
      var NewTopLine, NewPageIndex: integer; Action: TJumpHistoryAction);
    procedure OnSrcNotebookMovingPage(Sender: TObject;
      OldPageIndex, NewPageIndex: integer);
    procedure OnSrcNotebookReadOnlyChanged(Sender : TObject);
    procedure OnSrcNotebookSaveAll(Sender : TObject);
    procedure OnSrcNotebookShowHintForSource(SrcEdit: TSourceEditor;
          ClientPos: TPoint; CaretPos: TPoint);
    procedure OnSrcNoteBookShowUnitInfo(Sender: TObject);
    procedure OnSrcNotebookToggleFormUnit(Sender : TObject);
    procedure OnSrcNotebookToggleObjectInsp(Sender: TObject);
    procedure OnSrcNotebookViewJumpHistory(Sender : TObject);

    // ObjectInspector + PropertyEditorHook events
    procedure OIOnSelectComponent(AComponent:TComponent);
    procedure OIOnShowOptions(AComponent:TComponent);
    procedure OnPropHookGetMethods(TypeData:PTypeData; Proc:TGetStringProc);
    function OnPropHookMethodExists(const AMethodName:ShortString;
      TypeData: PTypeData;
      var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    function OnPropHookCreateMethod(const AMethodName:ShortString;
      ATypeInfo:PTypeInfo): TMethod;
    procedure OnPropHookShowMethod(const AMethodName:ShortString);
    procedure OnPropHookRenameMethod(const CurName, NewName:ShortString);
    procedure OnPropHookComponentRenamed(AComponent: TComponent);
    procedure OnPropHookComponentAdded(AComponent: TComponent; Select: boolean);
    procedure OnPropHookDeleteComponent(AComponent: TComponent);

    // designer events
    procedure OnDesignerGetSelectedComponentClass(Sender: TObject;
      var RegisteredComponent: TRegisteredComponent);
    procedure OnDesignerUnselectComponentClass(Sender: TObject);
    procedure OnDesignerSetDesigning(Sender: TObject; Component: TComponent;
      Value: boolean);
    procedure OnDesignerShowOptions(Sender: TObject);
    procedure OnDesignerPropertiesChanged(Sender: TObject);
    procedure OnDesignerComponentAdded(Sender: TObject; AComponent: TComponent;
      AComponentClass: TRegisteredComponent);
    procedure OnDesignerComponentDeleted(Sender: TObject;
      AComponent: TComponent);
    procedure OnDesignerRemoveComponent(Sender: TObject; AComponent: TComponent);
    procedure OnDesignerModified(Sender: TObject);
    Procedure OnDesignerActivated(Sender : TObject);
    procedure OnDesignerRenameComponent(ADesigner: TDesigner;
      AComponent: TComponent; const NewName: string);

    procedure OnControlSelectionChanged(Sender: TObject);
    procedure OnControlSelectionPropsChanged(Sender: TObject);
    procedure OnControlSelectionFormChanged(Sender: TObject; OldForm,
          NewForm: TCustomForm);
          
    // project inspector
    procedure ProjInspectorOpen(Sender: TObject);
    function ProjInspectorAddUnitToProject(Sender: TObject;
      AnUnitInfo: TUnitInfo): TModalresult;
    function ProjInspectorRemoveFile(Sender: TObject;
      AnUnitInfo: TUnitInfo): TModalresult;

    // unit dependencies events
    procedure UnitDependenciesViewAccessingSources(Sender: TObject);
    function UnitDependenciesViewGetProjectMainFilename(
      Sender: TObject): string;
    procedure UnitDependenciesViewOpenFile(Sender: TObject;
      const Filename: string);

    // view project ToDo list events
    procedure ViewProjectTodosOpenFile(Sender: TObject;
      const Filename: string);

    // CodeToolBoss events
    procedure OnBeforeCodeToolBossApplyChanges(Manager: TCodeToolManager;
                                    var Abort: boolean);
    procedure OnAfterCodeToolBossApplyChanges(Manager: TCodeToolManager);
    function OnCodeToolBossSearchUsedUnit(const SrcFilename: string;
          const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
    function OnCodeToolBossCheckAbort: boolean;
    procedure CodeToolBossGetVirtualDirectoryAlias(Sender: TObject;
          var RealDir: string);
    procedure CodeToolBossGetVirtualDirectoryDefines(DefTree: TDefineTree;
          DirDef: TDirectoryDefines);
    function MacroFunctionProject(Data: Pointer): boolean;
    procedure OnCompilerGraphStampIncreased;

    // MessagesView events
    procedure MessagesViewSelectionChanged(sender : TObject);
    procedure MessageViewDblClick(Sender : TObject);

    // External Tools events
    procedure OnExtToolNeedsOutputFilter(var OutputFilter: TOutputFilter;
                                         var Abort: boolean);
    procedure OnExtToolFreeOutputFilter(OutputFilter: TOutputFilter;
                                        ErrorOccurred: boolean);
  private
    FDisplayState : TDisplayState;
    FLastFormActivated : TCustomForm;// used to find the last form so you can
                                     // display the correct tab
    {$IFNDEF EnablePkgs}
    FSelectedComponent : TRegisteredComponent;
    {$ENDIF}
    FOpenEditorsOnCodeToolChange: boolean;

    FRunProcess: TProcess; // temp solution, will be replaced by dummydebugger

    procedure SetDefaultsForForm(aForm : TCustomForm);
    procedure InvalidateAllDesignerForms;
  protected
    procedure ToolButtonClick(Sender : TObject);
    procedure OnApplyWindowLayout(ALayout: TIDEWindowLayout);
    procedure AddRecentFileToEnvironment(const AFilename: string);
    
    // methods for start
    procedure LoadGlobalOptions;
    procedure SetupMainMenu;
    procedure SetRecentFilesMenu;
    procedure SetRecentProjectFilesMenu;
    procedure SetupFileMenu; override;
    procedure SetupEditMenu; override;
    procedure SetupSearchMenu; override;
    procedure SetupViewMenu; override;
    procedure SetupProjectMenu; override;
    procedure SetupRunMenu; override;
    procedure SetupComponentsMenu; override;
    procedure SetupToolsMenu; override;
    procedure SetupEnvironmentMenu; override;
    procedure SetupWindowsMenu; override;
    procedure SetupHelpMenu; override;
    procedure LoadMenuShortCuts; override;
    procedure ConnectMainBarEvents;
    procedure SetupSpeedButtons;
    procedure SetupComponentNoteBook;
    {$IFNDEF EnablePkgs}
    procedure SetupComponentTabs;
    {$ENDIF}
    procedure SetupHints;
    procedure SetupOutputFilter;
    procedure SetupObjectInspector;
    procedure SetupCompilerInterface;
    procedure SetupFormEditor;
    procedure SetupSourceNotebook;
    procedure SetupTransferMacros;
    procedure SetupControlSelection;
    procedure SetupStartProject;
    
    // methods for 'new unit'
    function CreateNewCodeBuffer(NewUnitType:TNewUnitType;
        NewFilename: string; var NewCodeBuffer: TCodeBuffer;
        var NewUnitName: string): TModalResult;
    function CreateNewForm(NewUnitInfo: TUnitInfo): TModalResult;
    procedure ShowDesignForm(AForm: TCustomForm);
    
    // methods for 'save unit'
    function DoLoadResourceFile(AnUnitInfo: TUnitInfo;
        var LFMCode, ResourceCode: TCodeBuffer;
        IgnoreSourceErrors: boolean): TModalResult;
    function DoShowSaveFileAsDialog(AnUnitInfo: TUnitInfo;
        var ResourceCode: TCodeBuffer): TModalResult;
    function DoSaveFileResources(AnUnitInfo: TUnitInfo;
        ResourceCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;

    // methods for 'open unit' and 'open main unit'
    function DoOpenNotExistingFile(const AFileName:string;
        Flags: TOpenFlags): TModalResult;
    function DoOpenUnknownFile(const AFileName:string; Flags: TOpenFlags;
        var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
    procedure DoRestoreBookMarks(AnUnitInfo: TUnitInfo; ASrcEdit:TSourceEditor);
    function DoOpenFileInSourceNotebook(AnUnitInfo: TUnitInfo;
        PageIndex: integer; Flags: TOpenFlags): TModalResult;
    function DoLoadLFM(AnUnitInfo: TUnitInfo; Flags: TOpenFlags): TModalResult;
    
    // methods for 'close unit'
    function CloseDesignerForm(AnUnitInfo: TUnitInfo): TModalResult;

    // methods for 'save project'
    procedure GetMainUnit(var MainUnitInfo: TUnitInfo;
        var MainUnitSrcEdit: TSourceEditor; UpdateModified: boolean);
    procedure SaveSrcEditorProjectSpecificSettings(AnUnitInfo: TUnitInfo);
    procedure SaveSourceEditorProjectSpecificSettings;
    function DoShowSaveProjectAsDialog: TModalResult;
    
    // methods for open project, create project from source
    function DoCompleteLoadingProjectInfo: TModalResult;
    
    // methods for publish project
    procedure OnCopyFile(const Filename: string; var Copy: boolean;
      Data: TObject);
    procedure OnCopyError(const ErrorData: TCopyErrorData;
      var Handled: boolean; Data: TObject);

  public
    CustomExtToolMenuSeparator: TMenuItem;
    class procedure ParseCmdLineOptions;
    
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateOftenUsedForms; override;

    // files/units
    function DoNewEditorFile(NewUnitType: TNewUnitType;
        NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags): TModalResult; override;
    function DoNewOther: TModalResult;
    function DoSaveEditorFile(PageIndex:integer;
        Flags: TSaveFlags): TModalResult;
    function DoCloseEditorFile(PageIndex:integer;
        Flags: TCloseFlags):TModalResult;
    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; override;
    function DoOpenFileAtCursor(Sender: TObject): TModalResult;
    function DoSaveAll(Flags: TSaveFlags): TModalResult;
    function DoOpenMainUnit(ProjectLoading: boolean): TModalResult;
    function DoRevertMainUnit: TModalResult;
    function DoViewUnitsAndForms(OnlyForms: boolean): TModalResult;
    procedure DoViewUnitDependencies;

    // project(s)
    function DoNewProject(NewProjectType:TProjectType):TModalResult;
    function DoSaveProject(Flags: TSaveFlags):TModalResult;
    function DoCloseProject:TModalResult;
    function DoOpenProjectFile(AFileName:string; Flags: TOpenFlags):TModalResult;
    function DoPublishProject(Flags: TSaveFlags;
      ShowDialog: boolean):TModalResult;
    function DoShowProjectInspector: TModalResult; override;
    function DoAddActiveUnitToProject: TModalResult;
    function DoRemoveFromProjectDialog: TModalResult;
    procedure DoWarnAmbigiousFiles;
    function DoSaveForBuild: TModalResult; override;
    function DoBuildProject(BuildAll: boolean): TModalResult;
    function DoInitProjectRun: TModalResult; override;
    function DoRunProject: TModalResult;
    function SomethingOfProjectIsModified: boolean;
    function DoCreateProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
    function DoSaveProjectToTestDirectory: TModalResult;
    function DoShowToDoList: TModalResult;
    
    // edit menu
    procedure DoEditMenuCommand(EditorCommand: integer);
    
    // tools
    function DoConvertDFMtoLFM: TModalResult;
    procedure UpdateCustomToolsInMenu;

    // external tools
    function DoRunExternalTool(Index: integer): TModalResult;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; override;

    // useful information methods
    procedure GetCurrentUnit(var ActiveSourceEditor: TSourceEditor;
      var ActiveUnitInfo: TUnitInfo); override;
    procedure GetUnitWithPageIndex(PageIndex: integer;
      var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
    procedure GetDesignerUnit(ADesigner: TDesigner;
      var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
    procedure GetUnitWithForm(AForm: TCustomForm;
      var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
    function GetSourceEditorForUnitInfo(AnUnitInfo: TUnitInfo): TSourceEditor;
    procedure UpdateDefaultPascalFileExtensions;
    function CreateSrcEditPageName(const AnUnitName, AFilename: string;
      IgnorePageIndex: integer): string;

    // useful file methods
    function FindUnitFile(const AFilename: string): string; override;
    function DoSaveStreamToFile(AStream:TStream; const Filename:string;
                                IsPartOfProject:boolean): TModalResult;
    function DoSaveStringToFile(const Filename, Src,
                               FileDescription: string): TModalResult; override;
    function DoLoadMemoryStreamFromFile(MemStream: TMemoryStream;
                                        const AFilename:string): TModalResult;
    function DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
                                    const AFilename: string;
                                    IsPartOfProject:boolean): TModalResult; override;
    function DoLoadCodeBuffer(var ACodeBuffer: TCodeBuffer;
                              const AFilename: string;
                              Flags: TLoadBufferFlags): TModalResult; override;
    function DoBackupFile(const Filename:string;
                          IsPartOfProject:boolean): TModalResult; override;
    function DoDeleteAmbigiousFiles(const Filename:string): TModalResult; override;
    function DoCheckFilesOnDisk: TModalResult; override;

    // useful frontend methods
    procedure DoSwitchToFormSrc(var ActiveSourceEditor:TSourceEditor;
      var ActiveUnitInfo:TUnitInfo);
    procedure DoSwitchToFormSrc(ADesigner: TDesigner;
      var ActiveSourceEditor:TSourceEditor; var ActiveUnitInfo:TUnitInfo);
    function GetFormOfSource(AnUnitInfo: TUnitInfo;
      LoadForm: boolean): TCustomForm;
    procedure UpdateCaption;
    function DoConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
    
    // methods for codetools
    procedure InitCodeToolBoss;
    procedure UpdateEnglishErrorMsgFilename;
    procedure ActivateCodeToolAbortableMode;
    function BeginCodeTool(var ActiveSrcEdit: TSourceEditor;
      var ActiveUnitInfo: TUnitInfo; Flags: TCodeToolsFlags): boolean;
    function BeginCodeTool(ADesigner: TDesigner; var ActiveSrcEdit: TSourceEditor;
      var ActiveUnitInfo: TUnitInfo; Flags: TCodeToolsFlags): boolean;
    function DoJumpToCodePos(ActiveSrcEdit: TSourceEditor;
      ActiveUnitInfo: TUnitInfo;
      NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
      AddJumpPoint: boolean): TModalResult;
    procedure DoJumpToCodeToolBossError; override;
    procedure UpdateSourceNames;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); override;
    procedure ApplyCodeToolChanges;
    procedure DoJumpToProcedureSection;
    procedure DoFindDeclarationAtCursor;
    procedure DoFindDeclarationAtCaret(CaretXY: TPoint);
    function DoInitIdentCompletion: boolean;
    procedure DoCompleteCodeAtCursor;
    function DoCheckSyntax: TModalResult;
    procedure DoGoToPascalBlockOtherEnd;
    procedure DoGoToPascalBlockStart;
    procedure DoJumpToGuessedUnclosedBlock(FindNext: boolean);
    procedure DoJumpToGuessedMisplacedIFDEF(FindNext: boolean);
    procedure DoGotoIncludeDirective;
    procedure SaveIncludeLinks;
    function DoMakeResourceString: TModalResult;
    function DoDiff: TModalResult;

    // methods for debugging, compiling and external tools
    function DoJumpToCompilerMessage(Index:integer;
      FocusEditor: boolean): boolean;
    procedure DoShowMessagesView;
    procedure DoArrangeSourceEditorAndMessageView(PutOnTop: boolean);
    function GetTestBuildDir: string; override;
    function GetProjectTargetFilename: string;
    function GetTestProjectFilename: string;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; override;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string;
    function IsTestUnitFilename(const AFilename: string): boolean; override;
    function GetRunCommandLine: string; override;
    function GetProjPublishDir: string;
    procedure OnMacroSubstitution(TheMacro: TTransferMacro; var s:string;
                                  var Handled, Abort: boolean);
    function OnSubstituteCompilerOption(Options: TParsedCompilerOptions;
                                        const UnparsedValue: string): string;
    function OnMacroPromptFunction(const s:string; var Abort: boolean):string;
    procedure OnCmdLineCreate(var CmdLine: string; var Abort:boolean);
    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
      NeededFlags: TIDEFileStateFlags; var ResultFlags: TIDEFileStateFlags); override;

    // form editor and designer
    {$IFNDEF EnablePkgs}
    property SelectedComponent : TRegisteredComponent
      read FSelectedComponent write FSelectedComponent;
    {$ENDIF}
    procedure DoBringToFrontFormOrUnit;
    procedure DoBringToFrontFormOrInspector;
    procedure DoShowDesignerFormOfCurrentSrc;
    procedure DoShowSourceOfActiveDesignerForm;
    procedure SetDesigning(AComponent: TComponent; Value : Boolean);

    // editor and environment options
    procedure SaveEnvironment; override;
    procedure LoadDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
    procedure SaveDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
  end;


const
  CodeToolsIncludeLinkFile = 'includelinks.xml';


implementation

uses
  Math;

//==============================================================================
{
  This function creates a LFM file from any form.
  To create the resource file use the program lazres or the
  LFMtoLFCfile function.
}
function CreateLFM(AForm:TCustomForm):integer;
// 0 = ok
// -1 = error while streaming AForm to binary stream
// -2 = error while streaming binary stream to text file
var BinStream,TxtMemStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  TxtFileStream:TFileStream;
begin
  Result:=0;
  BinStream:=TMemoryStream.Create;
  try
    try
      Driver:=TBinaryObjectWriter.Create(BinStream,4096);
      try
        Writer:=TWriter.Create(Driver);
        try
          Writer.WriteDescendent(AForm,nil);
        finally
          Writer.Free;
        end;
      finally
        Driver.Free;
      end;
    except
      Result:=-1;
      exit;
    end;
    try
      // transform binary to text and save LFM file
      TxtMemStream:=TMemoryStream.Create;
      TxtFileStream:=TFileStream.Create(lowercase(AForm.ClassName)+'.lfm'
                           ,fmCreate);
      try
        BinStream.Position:=0;
        ObjectBinaryToText(BinStream,TxtMemStream);
        TxtMemStream.Position:=0;
        TxtFileStream.CopyFrom(TxtMemStream,TxtMemStream.Size);
      finally
        TxtMemStream.Free;
        TxtFileStream.Free;
      end;
    except
      Result:=-2;
      exit;
    end;
  finally
    BinStream.Free;
  end;
end;

function LoadPixmapRes(const ResourceName:string; PixMap:TPixMap):boolean;
var
  ms:TMemoryStream;
  res:TLResource;
begin
  Result:=false;
  res:=LazarusResources.Find(ResourceName);
  if (res<>nil) and (res.Value<>'') and (res.ValueType='XPM') then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(res.Value[1],length(res.Value));
      ms.Position:=0;
      Pixmap.LoadFromStream(ms);
      Result:=true;
    finally
      ms.Free;
    end;
  end;
end;

function LoadSpeedBtnPixMap(const ResourceName:string):TPixmap;
begin
  Result:=TPixmap.Create;
  Result.TransparentColor:=clBtnFace;
  if not LoadPixmapRes(ResourceName,Result) then
    LoadPixmapRes('default',Result);
end;


//==============================================================================


{ TMainIDE }

{-------------------------------------------------------------------------------
  procedure TMainIDE.ParseCmdLineOptions;

  Parses the command line for the IDE.
-------------------------------------------------------------------------------}
procedure TMainIDE.ParseCmdLineOptions;
const
  PrimaryConfPathOpt='--primary-config-path=';
  SecondaryConfPathOpt='--secondary-config-path=';
var i: integer;
begin
  if (ParamCount>0)
  and ((AnsiCompareText(ParamStr(1),'--help')=0)
    or (AnsiCompareText(ParamStr(1),'-help')=0)
    or (AnsiCompareText(ParamStr(1),'-?')=0)) then
  begin
    TranslateResourceStrings(ProgramDirectory,'');
    writeln(lisCmdLineHlpHeader);
    writeln(Format(lisCmdLinePrimaryConfigPathDesc,[GetPrimaryConfigPath]));
    writeln(Format(lisCmdLineSecondaryConfigPathDesc,[GetSecondaryConfigPath]));
    writeln('');
    writeln(lisCmdLineLCLInterfaceSpecificOptions);
    writeln('');
    writeln(GetCmdLineParamDescForInterface);
    Application.Terminate;
    Halt;
  end;
  for i:=1 to ParamCount do begin
    if AnsiCompareText(LeftStr(ParamStr(i),length(PrimaryConfPathOpt)),
      PrimaryConfPathOpt)=0 then
    begin
      SetPrimaryConfigPath(copy(ParamStr(i),length(PrimaryConfPathOpt)+1,
               length(ParamStr(i))));
    end;
    if AnsiCompareText(LeftStr(ParamStr(i),length(SecondaryConfPathOpt)),
      SecondaryConfPathOpt)=0 then
    begin
      SetSecondaryConfigPath(copy(ParamStr(i),length(SecondaryConfPathOpt)+1,
               length(ParamStr(i))));
    end;
  end;
end;

procedure TMainIDE.LoadGlobalOptions;
// load environment, miscellaneous, editor and codetools options
var
  InteractiveSetup: boolean;
begin
  InteractiveSetup:=true;

  EnvironmentOptions:=TEnvironmentOptions.Create;
  with EnvironmentOptions do begin
    SetLazarusDefaultFilename;
    Load(false);
    TranslateResourceStrings(EnvironmentOptions.LazarusDirectory,
      LazarusLanguageIDs[EnvironmentOptions.Language]);

    SetupCompilerFilename(InteractiveSetup);
    SetupFPCSourceDirectory(InteractiveSetup);
    SetupLazarusDirectory(InteractiveSetup);
    
    ExternalTools.OnNeedsOutputFilter:=@OnExtToolNeedsOutputFilter;
    ExternalTools.OnFreeOutputFilter:=@OnExtToolFreeOutputFilter;
    OnApplyWindowLayout:=@Self.OnApplyWindowLayout;
  end;
  UpdateDefaultPascalFileExtensions;
  
  EditorOpts:=TEditorOptions.Create;
  EditorOpts.Load;

  EnvironmentOptions.ExternalTools.LoadShortCuts(EditorOpts.KeyMap);

  MiscellaneousOptions:=TMiscellaneousOptions.Create;
  MiscellaneousOptions.Load;

  CodeToolsOpts:=TCodeToolsOptions.Create;
  with CodeToolsOpts do begin
    SetLazarusDefaultFilename;
    Load;
  end;
  
  InputHistories:=TInputHistories.Create;
  with InputHistories do begin
    SetLazarusDefaultFilename;
    Load;
  end;
end;

constructor TMainIDE.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // load options
  CreatePrimaryConfigPath;
  LoadGlobalOptions;
  
  // set the IDE mode to none (= editing mode)
  ToolStatus:=itNone;

  // setup the code tools
  InitCodeToolBoss;

  // build and position the MainIDE form
  Name := NonModalIDEWindowNames[nmiwMainIDEName];
  EnvironmentOptions.IDEWindowLayoutList.Apply(TForm(Self),Name);

  {$IFNDEF EnablePkgs}
  InitIDEComponents;
  {$ENDIF}
  if LazarusResources.Find(ClassName)=nil then begin
    SetupMainMenu;
    SetupSpeedButtons;
    SetupComponentNoteBook;
    ConnectMainBarEvents;
  end;

  // initialize the other IDE managers
  DebugBoss:=TDebugManager.Create(Self);
  DebugBoss.ConnectMainBarEvents;
  PkgBoss:=TPkgManager.Create(Self);
  PkgBoss.ConnectMainBarEvents;
  // setup the IDE components
  LoadMenuShortCuts;
  SetupOutputFilter;
  SetupCompilerInterface;
  SetupObjectInspector;
  SetupFormEditor;
  SetupSourceNotebook;
  SetupTransferMacros;
  SetupControlSelection;
  {$IFNDEF EnablePkgs}
  SetupComponentTabs;
  {$ENDIF}

  // Main IDE bar created and setup completed -> Show it
  Show;
  
  // load installed packages
  PkgBoss.LoadInstalledPackages;

  UpdateWindowsMenu;

  // Now load a project
  SetupStartProject;
  
  // set OnIdle handlers
  Application.AddOnUserInputHandler(@OnApplicationUserInput);
  Application.AddOnIdleHandler(@OnApplicationIdle);
  SetupHints;
end;

destructor TMainIDE.Destroy;
begin
  writeln('[TMainIDE.Destroy] A');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy A ');{$ENDIF}
  FreeThenNil(ProjInspector);
  
  if DebugBoss<>nil then DebugBoss.EndDebugging;

  Application.RemoveOnUserInputHandler(@OnApplicationUserInput);

  FreeThenNil(Project1);
  if TheControlSelection<>nil then begin
    TheControlSelection.OnChange:=nil;
    TheControlSelection.OnSelectionFormChanged:=nil;
    FreeThenNil(TheControlSelection);
  end;
  FreeThenNil(FormEditor1);
  FreeThenNil(PkgBoss);
  FreeThenNil(PropertyEditorHook1);
  FreeThenNil(TheCompiler);
  FreeThenNil(TheOutputFilter);
  FreeThenNil(MacroList);
  FreeThenNil(CodeToolsOpts);
  FreeThenNil(MiscellaneousOptions);
  FreeThenNil(EditorOpts);
  FreeThenNil(EnvironmentOptions);
  FreeThenNil(InputHistories);

  writeln('[TMainIDE.Destroy] B  -> inherited Destroy...');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy B ');{$ENDIF}
  inherited Destroy;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy C ');{$ENDIF}
  writeln('[TMainIDE.Destroy] END');
end;

procedure TMainIDE.CreateOftenUsedForms;
begin
  Application.CreateForm(TMessagesView, MessagesView);
  Application.CreateForm(TLazFindReplaceDialog, FindReplaceDlg);
  Application.CreateForm(TLazFindInFilesDialog, FindInFilesDialog);
end;

procedure TMainIDE.OIOnSelectComponent(AComponent:TComponent);
begin
  TheControlSelection.AssignComponent(AComponent);
  if AComponent.Owner is TControl then
    TControl(AComponent.Owner).Invalidate;
end;

procedure TMainIDE.OIOnShowOptions(AComponent: TComponent);
begin
  DoShowEnvGeneralOptions(eodpObjectInspector);
end;

procedure TMainIDE.OnPropHookGetMethods(TypeData:PTypeData;
  Proc:TGetStringProc);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookGetMethods] ************');
  {$ENDIF}
  if not CodeToolBoss.GetCompatiblePublishedMethods(ActiveUnitInfo.Source,
    ActiveUnitInfo.Form.ClassName,TypeData,Proc) then
  begin
    DoJumpToCodeToolBossError;
  end;
end;

Procedure TMainIDE.ToolButtonClick(Sender : TObject);
Begin
  Assert(False, 'Trace:TOOL BUTTON CLICK!');

end;

{------------------------------------------------------------------------------}
procedure TMainIDE.FormClose(Sender : TObject; var Action: TCloseAction);
begin
  SaveEnvironment;
  SaveIncludeLinks;
  InputHistories.Save;
  PkgBoss.SaveSettings;
  if TheControlSelection<>nil then TheControlSelection.Clear;
  if SourceNoteBook<>nil then SourceNoteBook.ClearUnUsedEditorComponents(true);
end;

procedure TMainIDE.FormCloseQuery(Sender : TObject; var CanClose: boolean);
var
  MsgResult: integer;
begin
  // check packages
  if (PkgBoss.DoSaveAllPackages([psfAskBeforeSaving])<>mrOk)
  or (PkgBoss.DoCloseAllPackageEditors<>mrOk) then begin
    CanClose:=false;
    exit;
  end;

  // check project
  if SomethingOfProjectIsModified then begin
    MsgResult:=MessageDlg(lisProjectChanged, Format(lisSaveChangesToProject,
      [Project1.Title]), mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case MsgResult of

    mrYes:
      begin
        CanClose := DoSaveProject([]) <> mrAbort;
        if not CanClose then exit;
      end;

    mrCancel:
      begin
        CanClose:= false;
        Exit;
      end;
    end;  
  end;
  
  CanClose:=(DoCloseProject <> mrAbort);
end;

{------------------------------------------------------------------------------}
type 
  TMoveFlags = set of (mfTop, mfLeft);

procedure TMainIDE.SetupSpeedButtons;
  
  function CreateButton(const AName, APixName: String; ANumGlyphs: Integer;
    var ALeft, ATop: Integer; const AMoveFlags: TMoveFlags;
    const AOnClick: TNotifyEvent; const AHint : String): TSpeedButton;
  begin
    Result := TSpeedButton.Create(Self);
    with Result do
    begin
      Name := AName;
      Parent := pnlSpeedButtons;
      Enabled := True;
      Top := ATop;
      Left := ALeft;
      OnClick := AOnClick;
      Glyph := LoadSpeedBtnPixMap(APixName);
      NumGlyphs := ANumGlyphs;
      Flat := True;
      //Transparent:=True;
      if mfTop in AMoveFlags then Inc(ATop, Height);
      if mfLeft in AMoveFlags then Inc(ALeft, Width);
      Hint := AHint;
    end;
  end;

var
  ButtonTop, ButtonLeft, n: Integer;
begin
  pnlSpeedButtons := TPanel.Create(Self);
  with pnlSpeedButtons do begin
    Name := 'pnlSpeedButtons';
    Parent:= Self;
    Top := 0;
    Left:= 0;
    Caption:= '';
    BevelWidth:=1;
    BevelOuter:=bvRaised;
  end;


  ButtonTop := 1;
  ButtonLeft := 1;
  NewUnitSpeedBtn       := CreateButton('NewUnitSpeedBtn'      , 'btn_newunit'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewUnitClicked, lisHintNewUnit);

  OpenFileSpeedBtn      := CreateButton('OpenFileSpeedBtn'     , 'btn_openfile'  , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuOpenClicked, lisHintOpen);

  // store left
  n := ButtonLeft;
  OpenFileArrowSpeedBtn := CreateButton('OpenFileArrowSpeedBtn', 'btn_downarrow' , 1, ButtonLeft, ButtonTop, [mfLeft], @OpenFileDownArrowClicked, '');
  OpenFileArrowSpeedBtn.Width := 12;
  ButtonLeft := n+12+1;
  
  SaveSpeedBtn          := CreateButton('SaveSpeedBtn'         , 'btn_save'      , 2, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveClicked, lisHintSave);
  SaveAllSpeedBtn       := CreateButton('SaveAllSpeedBtn'      , 'btn_saveall'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveAllClicked, lisHintSaveAll);
  NewFormSpeedBtn       := CreateButton('NewFormSpeedBtn'      , 'btn_newform'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewFormClicked, lisHintNewForm);
  ToggleFormSpeedBtn    := CreateButton('ToggleFormSpeedBtn'   , 'btn_toggleform', 2, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuToggleFormUnitCLicked, lisHintToggleFormUnit);

  // new row
  ButtonLeft := 1;
  ViewUnitsSpeedBtn     := CreateButton('ViewUnitsSpeedBtn'    , 'btn_viewunits' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewUnitsClicked, lisHintViewUnits);
  ViewFormsSpeedBtn     := CreateButton('ViewFormsSpeedBtn'    , 'btn_viewforms' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewFormsClicked, lisHintViewForms);
  inc(ButtonLeft,13);
  RunSpeedButton        := CreateButton('RunSpeedButton'       , 'btn_run'       , 2, ButtonLeft, ButtonTop, [mfLeft], @mnuRunProjectClicked, lisHintRun);
  PauseSpeedButton      := CreateButton('PauseSpeedButton'     , 'btn_pause'       , 2, ButtonLeft, ButtonTop, [mfLeft], @mnuPauseProjectClicked, lisHintPause);
  PauseSpeedButton.Enabled:=false;
  StepIntoSpeedButton   := CreateButton('StepIntoSpeedButton'  , 'btn_stepinto'       , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuStepIntoProjectClicked, lisHintStepInto);
  StepOverSpeedButton   := CreateButton('StepOverpeedButton'   , 'btn_stepover'       , 1, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuStepOverProjectClicked, lisHintStepOver);
  
  pnlSpeedButtons.Width := ButtonLeft+3;
  pnlSpeedButtons.Height := ButtonTop+3;
  

  // create the popupmenu for the OpenFileArrowSpeedBtn
  OpenFilePopUpMenu := TPopupMenu.Create(self);
  OpenFilePopupMenu.Name:='OpenFilePopupMenu';
  OpenFilePopupMenu.AutoPopup := False;
  {
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := 'No files have been opened';
    MenuItem.OnClick := nil;
    OpenFilePopupMenu.Items.Add(MenuItem);
  }
end;

procedure TMainIDE.SetupComponentNoteBook;
begin
  // Component Notebook
  ComponentNotebook := TNotebook.Create(Self);
  with ComponentNotebook do begin
    Parent := Self;
    Name := 'ComponentNotebook';
    Left := pnlSpeedButtons.Left + pnlSpeedButtons.Width;
    Top := 0;
    Width := Self.ClientWidth - Left;
    Height := 60; //Self.ClientHeight - ComponentNotebook.Top;
  end;
end;

{$IFNDEF EnablePkgs}
procedure TMainIDE.SetupComponentTabs;
var
  PageCount, I, X: integer;
  RegComp     : TRegisteredComponent;
  RegCompPage : TRegisteredComponentPage;
  IDEComponent: TIDEComponent;
  SelectionPointerPixmap: TPixmap;
begin
  PageCount := 0;
  for I := 0 to RegCompList.PageCount-1 do begin
    // Component Notebook Pages
    RegCompPage := RegCompList.Pages[i];
    if RegCompPage.Name <> '' then
    Begin
      if (PageCount = 0) and (ComponentNotebook.PageCount>0) then
        ComponentNotebook.Pages.Strings[PageCount] := RegCompPage.Name
      else
        ComponentNotebook.Pages.Add(RegCompPage.Name);
      GlobalMouseSpeedButton := TSpeedButton.Create(Self);
      SelectionPointerPixmap:=LoadSpeedBtnPixMap('tmouse');
      with GlobalMouseSpeedButton do
      begin
        Parent := ComponentNotebook.Page[PageCount];
        Width := ComponentPaletteBtnWidth;
        Height := ComponentPaletteBtnHeight;
        OnClick := @ControlClick;
        Glyph := SelectionPointerPixmap;
        Flat := True;
        GroupIndex:= 1;
        Down := True;
        Name := 'GlobalMouseSpeedButton'+IntToStr(PageCount);
        Hint := lisSelectionTool;
      end;
      for x := 0 to RegCompPage.Count-1 do //for every component on the page....
      begin
        RegComp := RegCompPage.Items[x];
        IDEComponent := TIDEComponent.Create;
        IDEComponent.RegisteredComponent := RegComp;
        IDEComponent._SpeedButton(Self,ComponentNotebook.Page[PageCount]);
        IDEComponent.SpeedButton.OnClick := @ControlClick;
        IDEComponent.SpeedButton.Hint := RegComp.ComponentClass.ClassName;
        IDEComponent.SpeedButton.Name := IDEComponent.SpeedButton.Hint;
        IDEComponent.SpeedButton.GroupIndex := 1;
        IDECompList.Add(IDEComponent);
      end;
      inc(PageCount);
    end;
  end;
  ComponentNotebook.PageIndex := 0;   // Set it to the first page
  ComponentNotebook.OnPageChanged := @ControlClick;
  ComponentNotebook.Show;
end;
{$ENDIF}

procedure TMainIDE.SetupHints;
var
  CurShowHint: boolean;
  AControl: TControl;
  i, j: integer;
begin
  if EnvironmentOptions=nil then exit;
  // update all hints in the component palette
  CurShowHint:=EnvironmentOptions.ShowHintsForComponentPalette;
  for i:=0 to ComponentNotebook.PageCount-1 do begin
    for j:=0 to ComponentNotebook.Page[i].ControlCount-1 do begin
      AControl:=ComponentNotebook.Page[i].Controls[j];
      AControl.ShowHint:=CurShowHint;
    end;
  end;
  // update all hints in main ide toolbars
  CurShowHint:=EnvironmentOptions.ShowHintsForMainSpeedButtons;
  for i:=0 to pnlSpeedButtons.ControlCount-1 do begin
    AControl:=pnlSpeedButtons.Controls[i];
    AControl.ShowHint:=CurShowHint;
  end;
end;

procedure TMainIDE.SetupOutputFilter;
begin
  TheOutputFilter:=TOutputFilter.Create;
  TheOutputFilter.OnGetIncludePath:=@CodeToolBoss.GetIncludePathForDirectory;
end;

procedure TMainIDE.SetupObjectInspector;

begin
  ObjectInspector1 := TObjectInspector.Create(Self);
  ObjectInspector1.OnSelectComponentInOI:=@OIOnSelectComponent;
  ObjectInspector1.OnShowOptions:=@OIOnShowOptions;
  
  PropertyEditorHook1:=TPropertyEditorHook.Create;
  PropertyEditorHook1.OnGetMethods:=@OnPropHookGetMethods;
  PropertyEditorHook1.OnMethodExists:=@OnPropHookMethodExists;
  PropertyEditorHook1.OnCreateMethod:=@OnPropHookCreateMethod;
  PropertyEditorHook1.OnShowMethod:=@OnPropHookShowMethod;
  PropertyEditorHook1.OnRenameMethod:=@OnPropHookRenameMethod;
  PropertyEditorHook1.OnComponentRenamed:=@OnPropHookComponentRenamed;
  PropertyEditorHook1.OnComponentAdded:=@OnPropHookComponentAdded;
  ObjectInspector1.PropertyEditorHook:=PropertyEditorHook1;
  EnvironmentOptions.IDEWindowLayoutList.Apply(TForm(ObjectInspector1),
                                               DefaultObjectInspectorName);
  with EnvironmentOptions do begin
    ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;
end;

procedure TMainIDE.SetupCompilerInterface;
begin
  TheCompiler := TCompiler.Create;
  with TheCompiler do begin
    OnCommandLineCreate:=@OnCmdLineCreate;
    OutputFilter:=TheOutputFilter;
  end;
end;

procedure TMainIDE.SetupFormEditor;
begin
  FormEditor1 := TFormEditor.Create;
  FormEditor1.Obj_Inspector := ObjectInspector1;
end;

procedure TMainIDE.SetupSourceNotebook;
begin
  SourceNotebook := TSourceNotebook.Create(Self);
  SourceNotebook.OnActivate := @OnSrcNoteBookActivated;
  SourceNotebook.OnAddJumpPoint := @OnSrcNoteBookAddJumpPoint;
  SourceNotebook.OnCloseClicked := @OnSrcNotebookFileClose;
  SourceNotebook.OnCtrlMouseUp := @OnSrcNoteBookCtrlMouseUp;
  SourceNotebook.OnDeleteLastJumpPoint := @OnSrcNotebookDeleteLastJumPoint;
  SourceNotebook.OnEditorVisibleChanged := @OnSrcNotebookEditorVisibleChanged;
  SourceNotebook.OnEditorChanged := @OnSrcNotebookEditorChanged;
  SourceNotebook.OnEditorPropertiesClicked := @mnuEnvEditorOptionsClicked;
  SourceNotebook.OnFindDeclarationClicked := @OnSrcNotebookFindDeclaration;
  SourceNotebook.OnInitIdentCompletion :=@OnSrcNotebookInitIdentCompletion;
  SourceNotebook.OnJumpToHistoryPoint := @OnSrcNotebookJumpToHistoryPoint;
  SourceNotebook.OnMovingPage := @OnSrcNotebookMovingPage;
  SourceNotebook.OnNewClicked := @OnSrcNotebookFileNew;
  SourceNotebook.OnOpenClicked := @OnSrcNotebookFileOpen;
  SourceNotebook.OnOpenFileAtCursorClicked := @OnSrcNotebookFileOpenAtCursor;
  SourceNotebook.OnProcessUserCommand := @OnProcessIDECommand;
  SourceNotebook.OnReadOnlyChanged := @OnSrcNotebookReadOnlyChanged;
  SourceNotebook.OnSaveClicked := @OnSrcNotebookFileSave;
  SourceNotebook.OnSaveAsClicked := @OnSrcNotebookFileSaveAs;
  SourceNotebook.OnSaveAllClicked := @OnSrcNotebookSaveAll;
  SourceNotebook.OnShowHintForSource :=@OnSrcNotebookShowHintForSource;
  SourceNotebook.OnShowUnitInfo := @OnSrcNoteBookShowUnitInfo;
  SourceNotebook.OnToggleFormUnitClicked := @OnSrcNotebookToggleFormUnit;
  SourceNotebook.OnToggleObjectInspClicked:= @OnSrcNotebookToggleObjectInsp;
  SourceNotebook.OnViewJumpHistory := @OnSrcNotebookViewJumpHistory;
  DebugBoss.ConnectSourceNotebookEvents;

  // connect search menu to sourcenotebook
  itmSearchFind.OnClick := @SourceNotebook.FindClicked;
  itmSearchFindNext.OnClick := @SourceNotebook.FindNextClicked;
  itmSearchFindPrevious.OnClick := @SourceNotebook.FindPreviousClicked;
  itmSearchFindInFiles.OnClick := @SourceNotebook.FindInFilesClicked;
  itmSearchReplace.OnClick := @SourceNotebook.ReplaceClicked;
  itmIncrementalFind.OnClick := @SourceNotebook.IncrementalFindClicked;
  itmGotoLine.OnClick := @SourceNotebook.GotoLineClicked;
  itmJumpBack.OnClick := @SourceNotebook.JumpBackClicked;
  itmJumpForward.OnClick := @SourceNotebook.JumpForwardClicked;
  itmAddJumpPoint.OnClick := @SourceNotebook.AddJumpPointClicked;
  itmJumpHistory.OnClick := @SourceNotebook.ViewJumpHistoryClicked;
  itmFindBlockStart.OnClick:=@mnuSearchFindBlockStart;
  itmFindBlockOtherEnd.OnClick:=@mnuSearchFindBlockOtherEnd;
  itmFindDeclaration.OnClick:=@mnuSearchFindDeclaration;
  itmOpenFileAtCursor.OnClick:=@mnuOpenFileAtCursorClicked;
end;

procedure TMainIDE.SetupTransferMacros;
begin
  MacroList:=TTransferMacroList.Create;
  MacroList.Add(TTransferMacro.Create('Col','',
                    lisCursorColumnInCurrentEditor,nil,[]));
  MacroList.Add(TTransferMacro.Create('Row','',
                    lisCursorRowInCUrrentEditor,nil,[]));
  MacroList.Add(TTransferMacro.Create('CompPath','',
                    lisCompilerFilename,nil,[]));
  MacroList.Add(TTransferMacro.Create('CurToken','',
                    lisWordAtCursorInCurrentEditor,nil,[]));
  MacroList.Add(TTransferMacro.Create('EdFile','',
                    lisExpandedFilenameOfCurrentEditor,nil,[]));
  MacroList.Add(TTransferMacro.Create('FPCSrcDir','',
                    lisFreePascalSourceDirectory,nil,[]));
  MacroList.Add(TTransferMacro.Create('LazarusDir','',
                    lisLazarusDirectory,nil,[]));
  MacroList.Add(TTransferMacro.Create('LCLWidgetType','',
                    lisLCLWidgetType,nil,[]));
  MacroList.Add(TTransferMacro.Create('Params','',
                    lisCommandLineParamsOfProgram,nil,[]));
  MacroList.Add(TTransferMacro.Create('Prompt','',
                    lisPromptForValue,@OnMacroPromptFunction,[tmfInteractive]));
  MacroList.Add(TTransferMacro.Create('ProjFile','',
                    lisProjectFilename,nil,[]));
  MacroList.Add(TTransferMacro.Create('ProjPath','',
                    lisProjectDirectory,nil,[]));
  MacroList.Add(TTransferMacro.Create('Save','',
                    lisSaveCurrentEditorFile,nil,[tmfInteractive]));
  MacroList.Add(TTransferMacro.Create('SaveAll','',
                    lisSaveAllModified,nil,[tmfInteractive]));
  MacroList.Add(TTransferMacro.Create('TargetFile','',
                    lisTargetFilenameOfProject,nil,[]));
  MacroList.Add(TTransferMacro.Create('TargetCmdLine','',
                    lisTargetFilenamePlusParams,nil,[]));
  MacroList.Add(TTransferMacro.Create('TestDir','',
                    lisTestDirectory,nil,[]));
  MacroList.Add(TTransferMacro.Create('RunCmdLine','',
                    lisLaunchingCmdLine,nil,[]));
  MacroList.Add(TTransferMacro.Create('ProjPublishDir','',
                    lisPublishProjDir,nil,[]));
  MacroList.Add(TTransferMacro.Create('ProjUnitPath','',
                    lisProjectUnitPath,nil,[]));
  MacroList.Add(TTransferMacro.Create('ProjIncPath','',
                    lisProjectIncPath,nil,[]));
  MacroList.Add(TTransferMacro.Create('ProjSrcPath','',
                    lisProjectSrcPath,nil,[]));
  MacroList.Add(TTransferMacro.Create('ConfDir','',
                    lisProjectSrcPath,nil,[]));

  MacroList.OnSubstitution:=@OnMacroSubstitution;
  CompilerOptions.OnParseString:=@OnSubstituteCompilerOption;
end;

procedure TMainIDE.SetupControlSelection;
begin
  TheControlSelection:=TControlSelection.Create;
  TheControlSelection.OnChange:=@OnControlSelectionChanged;
  TheControlSelection.OnPropertiesChanged:=@OnControlSelectionPropsChanged;
  TheControlSelection.OnSelectionFormChanged:=@OnControlSelectionFormChanged;
end;

procedure TMainIDE.SetupStartProject;
begin
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.Create A ***********');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject A');{$ENDIF}
  // load command line project or last project or create a new project
  if (ParamCount>0) and (ParamStr(ParamCount)[1]<>'-')
  and (ExtractFileExt(ParamStr(ParamCount))='.lpi')
  and (DoOpenProjectFile(ParamStr(ParamCount),[])=mrOk) then
    // command line project loaded
  else if (EnvironmentOptions.OpenLastprojectAtStart)
  and (FileExists(EnvironmentOptions.LastSavedProjectFile))
  and (DoOpenProjectFile(EnvironmentOptions.LastSavedProjectFile,[])=mrOk) then
  begin
    // last project loaded
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.Create last project loaded successfully');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject B');{$ENDIF}
  end else
    // create new project
    DoNewProject(ptApplication);

  UpdateWindowsMenu;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.Create B');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject C');{$ENDIF}
end;


{------------------------------------------------------------------------------}
procedure TMainIDE.SetupMainMenu;
begin
  mnuMain := TMainMenu.Create(Self);
  mnuMain.Name:='mnuMainMenu';
  Menu := mnuMain;

  mnuFile := TMenuItem.Create(Self);
  mnuFile.Name:='mnuFile';
  mnuFile.Caption := lisMenuFile;
  mnuMain.Items.Add(mnuFile);

  mnuEdit := TMenuItem.Create(Self);
  mnuEdit.Name:='mnuEdit';
  mnuEdit.Caption := lisMenuEdit;
  mnuMain.Items.Add(mnuEdit);

  mnuSearch := TMenuItem.Create(Self);
  mnuSearch.Name:='mnuSearch';
  mnuSearch.Caption := lisMenuSearch;
  mnuMain.Items.Add(mnuSearch);

  mnuView := TMenuItem.Create(Self);
  mnuView.Name:='mnuView';
  mnuView.Caption := lisMenuView;
  mnuMain.Items.Add(mnuView);

  mnuProject := TMenuItem.Create(Self);
  mnuProject.Name:='mnuProject';
  mnuProject.Caption := lisMenuProject;
  mnuMain.Items.Add(mnuProject);

  mnuRun := TMenuItem.Create(Self);
  mnuRun.Name:='mnuRun';
  mnuRun.Caption := lisMenuRun;
  mnuMain.Items.Add(mnuRun);

  mnuComponents := TMenuItem.Create(Self);
  mnuComponents.Name:='mnuComponents';
  mnuComponents.Caption := lisMenuComponents;
  mnuMain.Items.Add(mnuComponents);

  mnuTools := TMenuItem.Create(Self);
  mnuTools.Name:='mnuTools';
  mnuTools.Caption := lisMenuTools;
  mnuMain.Items.Add(mnuTools);

  mnuEnvironment := TMenuItem.Create(Self);
  mnuEnvironment.Name:='mnuEnvironment';
  mnuEnvironment.Caption := lisMenuEnvironent;
  mnuMain.Items.Add(mnuEnvironment);

  mnuWindows := TMenuItem.Create(Self);
  mnuWindows.Name:='mnuWindows';
  mnuWindows.Caption := lisMenuWindows;
  mnuMain.Items.Add(mnuWindows);

  mnuHelp := TMenuItem.Create(Self);
  mnuHelp.Name:='mnuHelp';
  mnuHelp.Caption := lisMenuHelp;
  mnuMain.Items.Add(mnuHelp);

  SetupFileMenu;
  SetupEditMenu;
  SetupSearchMenu;
  SetupViewMenu;
  SetupProjectMenu;
  SetupRunMenu;
  SetupComponentsMenu;
  SetupToolsMenu;
  SetupEnvironmentMenu;
  SetupWindowsMenu;
  SetupHelpMenu;
end;

procedure TMainIDE.SetRecentFilesMenu;
begin
  SetRecentSubMenu(itmFileRecentOpen,EnvironmentOptions.RecentOpenFiles,
                    @mnuOpenRecentClicked);
end;

procedure TMainIDE.SetRecentProjectFilesMenu;
begin
  SetRecentSubMenu(itmProjectRecentOpen,EnvironmentOptions.RecentProjectFiles,
                   @mnuOpenProjectClicked);
end;

procedure TMainIDE.SetupFileMenu;
begin
  inherited;
  itmFileNewUnit.OnClick := @mnuNewUnitClicked;
  itmFileNewForm.OnClick := @mnuNewFormClicked;
  itmFileNewOther.OnClick := @mnuNewOtherClicked;
  itmFileOpen.OnClick := @mnuOpenClicked;
  itmFileRevert.OnClick := @mnuRevertClicked;
  SetRecentFilesMenu;
  itmFileSave.OnClick := @mnuSaveClicked;
  itmFileSaveAs.OnClick := @mnuSaveAsClicked;
  itmFileSaveAll.OnClick := @mnuSaveAllClicked;
  itmFileClose.Enabled := False;
  itmFileClose.OnClick := @mnuCloseClicked;
  itmFileCloseAll.Enabled := False;
  itmFileCloseAll.OnClick := @mnuCloseAllClicked;
  itmFileQuit.OnClick := @mnuQuitClicked;
end;

procedure TMainIDE.SetupEditMenu;
begin
  inherited;
  itmEditUndo.OnClick:=@mnuEditUndoClicked;
  itmEditRedo.OnClick:=@mnuEditRedoClicked;
  itmEditCut.OnClick:=@mnuEditCutClicked;
  itmEditCopy.OnClick:=@mnuEditCopyClicked;
  itmEditPaste.OnClick:=@mnuEditPasteClicked;
  itmEditIndentBlock.OnClick:=@mnuEditIndentBlockClicked;
  itmEditUnindentBlock.OnClick:=@mnuEditUnindentBlockClicked;
  itmEditUpperCaseBlock.OnClick:=@mnuEditUpperCaseBlockClicked;
  itmEditLowerCaseBlock.OnClick:=@mnuEditLowerCaseBlockClicked;
  itmEditTabsToSpacesBlock.OnClick:=@mnuEditTabsToSpacesBlockClicked;
  itmEditCommentBlock.OnClick:=@mnuEditCommentBlockClicked;
  itmEditUncommentBlock.OnClick:=@mnuEditUncommentBlockClicked;
  itmEditSortBlock.OnClick:=@mnuEditSortBlockClicked;
  itmEditSelectAll.OnClick:=@mnuEditSelectAllClick;
  itmEditSelectToBrace.OnClick:=@mnuEditSelectToBraceClick;
  itmEditSelectCodeBlock.OnClick:=@mnuEditSelectCodeBlockClick;
  itmEditSelectLine.OnClick:=@mnuEditSelectLineClick;
  itmEditSelectParagraph.OnClick:=@mnuEditSelectParagraphClick;
  itmEditCompleteCode.OnClick:=@mnuEditCompleteCodeClicked;

  // insert text->CVS keyword
  itmEditInsertCVSAuthor.OnClick:=@mnuEditInsertCVSAuthorClick;
  itmEditInsertCVSDate.OnClick:=@mnuEditInsertCVSDateClick;
  itmEditInsertCVSHeader.OnClick:=@mnuEditInsertCVSHeaderClick;
  itmEditInsertCVSID.OnClick:=@mnuEditInsertCVSIDClick;
  itmEditInsertCVSLog.OnClick:=@mnuEditInsertCVSLogClick;
  itmEditInsertCVSName.OnClick:=@mnuEditInsertCVSNameClick;
  itmEditInsertCVSRevision.OnClick:=@mnuEditInsertCVSRevisionClick;
  itmEditInsertCVSSource.OnClick:=@mnuEditInsertCVSSourceClick;

  // insert text->general
  itmEditInsertGPLNotice.OnClick:=@mnuEditInsertGPLNoticeClick;
  itmEditInsertLGPLNotice.OnClick:=@mnuEditInsertLGPLNoticeClick;
  itmEditInsertUsername.OnClick:=@mnuEditInsertUsernameClick;
  itmEditInsertDateTime.OnClick:=@mnuEditInsertDateTimeClick;
  itmEditInsertChangeLogEntry.OnClick:=@mnuEditInsertChangeLogEntryClick;
end;

procedure TMainIDE.SetupSearchMenu;
begin
  inherited;
  itmGotoIncludeDirective.OnClick:=@mnuGotoIncludeDirectiveClicked;
end;

procedure TMainIDE.SetupViewMenu;
begin
  inherited;
  itmViewInspector.OnClick := @mnuViewInspectorClicked;
  itmViewCodeExplorer.OnClick := @mnuViewCodeExplorerClick;
  itmViewUnits.OnClick := @mnuViewUnitsClicked;
  itmViewForms.OnClick := @mnuViewFormsClicked;
  itmViewUnitDependencies.OnClick := @mnuViewUnitDependenciesClicked;
  itmViewToggleFormUnit.OnClick := @mnuToggleFormUnitClicked;
  itmViewMessage.OnClick := @mnuViewMessagesClick;
end;

procedure TMainIDE.SetupProjectMenu;
begin
  inherited;
  itmProjectNew.OnClick := @mnuNewProjectClicked;
  itmProjectNewFromFile.OnClick := @mnuNewProjectFromFileClicked;
  itmProjectOpen.OnClick := @mnuOpenProjectClicked;
  SetRecentProjectFilesMenu;
  itmProjectSave.OnClick := @mnuSaveProjectClicked;
  itmProjectSaveAs.OnClick := @mnuSaveProjectAsClicked;
  itmProjectPublish.OnClick := @mnuPublishProjectClicked;
  itmProjectInspector.OnClick := @mnuProjectInspectorClicked;
  itmProjectAddTo.OnClick := @mnuAddToProjectClicked;
  itmProjectRemoveFrom.OnClick := @mnuRemoveFromProjectClicked;
  itmProjectViewSource.OnClick := @mnuViewProjectSourceClicked;
  itmProjectViewToDos.OnClick := @mnuViewProjectTodosClicked;
  itmProjectOptions.OnClick := @mnuProjectOptionsClicked;
end;

procedure TMainIDE.SetupRunMenu;
begin
  inherited;
  itmProjectBuild.OnClick := @mnuBuildProjectClicked;
  itmProjectBuildAll.OnClick := @mnuBuildAllProjectClicked;
  itmProjectRun.OnClick := @mnuRunProjectClicked;
  itmProjectPause.Enabled := false;
  itmProjectPause.OnClick := @mnuPauseProjectClicked;
  itmProjectStepInto.OnClick := @mnuStepIntoProjectClicked;
  itmProjectStepOver.OnClick := @mnuStepOverProjectClicked;
  itmProjectRunToCursor.OnClick := @mnuRunToCursorProjectClicked;
  itmProjectStop.OnClick := @mnuStopProjectClicked;
  itmProjectCompilerSettings.OnClick := @mnuProjectCompilerSettingsClicked;
  itmProjectRunParameters.OnClick := @mnuRunParametersClicked;
end;

procedure TMainIDE.SetupComponentsMenu;
begin
  inherited SetupComponentsMenu;
end;

procedure TMainIDE.SetupToolsMenu;
begin
  inherited;
  itmToolConfigure.OnClick := @mnuToolConfigureClicked;
  itmToolSyntaxCheck.OnClick := @mnuToolSyntaxCheckClicked;
  itmToolGuessUnclosedBlock.OnClick := @mnuToolGuessUnclosedBlockClicked;
  itmToolGuessMisplacedIFDEF.OnClick := @mnuToolGuessMisplacedIFDEFClicked;
  itmToolMakeResourceString.OnClick := @mnuToolMakeResourceStringClicked;
  itmToolDiff.OnClick := @mnuToolDiffClicked;
  itmToolConvertDFMtoLFM.OnClick := @mnuToolConvertDFMtoLFMClicked;
  itmToolBuildLazarus.OnClick := @mnuToolBuildLazarusClicked;
  itmToolConfigureBuildLazarus.OnClick := @mnuToolConfigBuildLazClicked;
  CustomExtToolMenuSeparator:=nil;
  UpdateCustomToolsInMenu;
end;

procedure TMainIDE.SetupEnvironmentMenu;
begin
  inherited;
  itmEnvGeneralOptions.OnClick := @mnuEnvGeneralOptionsClicked;
  itmEnvEditorOptions.OnClick := @mnuEnvEditorOptionsClicked;
  itmEnvCodeToolsOptions.OnClick := @mnuEnvCodeToolsOptionsClicked;
  itmEnvCodeToolsDefinesEditor.OnClick := @mnuEnvCodeToolsDefinesEditorClicked;
end;

procedure TMainIDE.SetupWindowsMenu;
begin
  inherited SetupWindowsMenu;
end;

procedure TMainIDE.SetupHelpMenu;
begin
  inherited;
  itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
end;

procedure TMainIDE.LoadMenuShortCuts;
begin
  inherited LoadMenuShortCuts;
  DebugBoss.SetupMainBarShortCuts;
end;

procedure TMainIDE.ConnectMainBarEvents;
begin
  //OnShow := @FormShow;
  OnClose := @FormClose;
  OnCloseQuery := @FormCloseQuery;
end;

{------------------------------------------------------------------------------}

procedure TMainIDE.mnuToggleFormUnitClicked(Sender : TObject);
begin
  DoBringToFrontFormOrUnit;
end;

Procedure TMainIDE.SetDesigning(AComponent: TComponent; Value : Boolean);
Begin
  AComponent.SetDesigning(Value);
  if Value then CNSendMessage(LM_SETDESIGNING, AComponent, nil);
end;

{------------------------------------------------------------------------------}
{$IFNDEF EnablePkgs}
procedure TMainIDE.ControlClick(Sender : TObject);
var
  IDECOmp : TIDEComponent;
  Speedbutton : TSpeedbutton;
  i : integer;
begin
  if Sender is TSpeedButton then
  begin
    SpeedButton := TSpeedButton(Sender);
    // find the IDEComponent that belongs to this speedbutton
    IDEComp := IDECompList.FindCompBySpeedButton(SpeedButton);
    if IDEComp <> nil then begin
      SelectedComponent := IDEComp.RegisteredComponent;
    end else begin
      SelectedComponent := nil;
    end;
  end
  else
  begin
    // select the selection tool
    SelectedComponent := nil;
    for i:= 0 to ComponentNotebook.PageCount - 1 do begin
      TSpeedButton(ComponentNotebook.Page[i].Controls[0]).Down:= true;
    end;
  end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
procedure TMainIDE.mnuFindDeclarationClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoFindDeclarationAtCursor;
end;

procedure TMainIDE.mnuNewUnitClicked(Sender : TObject);
begin
  DoNewEditorFile(nuUnit,'','',[nfOpenInEditor,nfCreateDefaultSrc]);
end;

procedure TMainIDE.mnuNewFormClicked(Sender : TObject);
begin
  DoNewEditorFile(nuForm,'','',[nfOpenInEditor,nfCreateDefaultSrc]);
end;

procedure TMainIDE.mnuNewOtherClicked(Sender: TObject);
begin
  DoNewOther;
end;

procedure TMainIDE.mnuOpenClicked(Sender : TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentFilesMenu;
    SaveEnvironment;
  end;

var
  OpenDialog: TOpenDialog;
  AFilename: string;
  I: Integer;
  OpenFlags: TOpenFlags;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      OpenFlags:=[ofAddToRecent];
      if OpenDialog.Files.Count>1 then
        Include(OpenFlags,ofRegularFile);
      For I := 0 to OpenDialog.Files.Count-1 do
        Begin
          AFilename:=CleanAndExpandFilename(OpenDialog.Files.Strings[i]);
          if DoOpenEditorFile(AFilename,-1,OpenFlags)=mrAbort then begin
            break;
          end;
        end;
      UpdateEnvironment;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TMainIDE.mnuOpenRecentClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentFilesMenu;
    SaveEnvironment;
  end;

var
  AFilename: string;
begin
  AFileName:=ExpandFilename(TMenuItem(Sender).Caption);
  if DoOpenEditorFile(AFilename,-1,[ofAddToRecent])=mrOk then begin
    UpdateEnvironment;
  end else begin
    // open failed
    if not FileExists(AFilename) then begin
      // file does not exist -> delete it from recent file list
      EnvironmentOptions.RemoveFromRecentOpenFiles(AFilename);
      UpdateEnvironment;
    end;
  end;
end;

procedure TMainIDE.mnuRevertClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoOpenEditorFile('',SourceNoteBook.NoteBook.PageIndex,[ofRevert]);
end;

procedure TMainIDE.mnuOpenFileAtCursorClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoOpenFileAtCursor(Sender);  
end;

procedure TMainIDE.mnuGotoIncludeDirectiveClicked(Sender : TObject);
begin
  DoGotoIncludeDirective;
end;

procedure TMainIDE.mnuSaveClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoSaveEditorFile(SourceNoteBook.NoteBook.PageIndex,[sfCheckAmbigiousFiles]);
end;

procedure TMainIDE.mnuSaveAsClicked(Sender : TObject);
begin
  if SourceNoteBook.NoteBook=nil then exit;
  DoSaveEditorFile(SourceNoteBook.NoteBook.PageIndex,
                   [sfSaveAs,sfCheckAmbigiousFiles]);
end;

procedure TMainIDE.mnuSaveAllClicked(Sender : TObject);
begin
  DoSaveAll([sfCheckAmbigiousFiles]);
end;

procedure TMainIDE.mnuCloseClicked(Sender : TObject);
var PageIndex: integer;
begin
  if SourceNoteBook.NoteBook=nil then exit;
  if Sender is TPage then begin
    PageIndex:=SourceNoteBook.NoteBook.PageList.IndexOf(Sender);
    if PageIndex<0 then
      PageIndex:=SourceNoteBook.NoteBook.PageIndex;
  end else begin
    PageIndex:=SourceNoteBook.NoteBook.PageIndex;
  end;
  DoCloseEditorFile(PageIndex,[cfSaveFirst]);
end;

procedure TMainIDE.mnuCloseAllClicked(Sender : TObject);
begin
  DoSaveAll([]);
  while (SourceNoteBook.NoteBook<>nil)
  and (DoCloseEditorFile(SourceNoteBook.NoteBook.PageIndex,
       [cfSaveFirst])=mrOk) do ;
end;

Procedure TMainIDE.OnSrcNotebookFileNew(Sender : TObject);
begin
  mnuNewFormClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileClose(Sender : TObject);
begin
  mnuCloseClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileOpen(Sender : TObject);
begin
  mnuOpenClicked(Sender);
end;

Procedure TMainIDE.OnSrcNoteBookFileOpenAtCursor(Sender : TObject);
begin
  mnuOpenFileAtCursorClicked(Sender);  
end;

Procedure TMainIDE.OnSrcNotebookFileSave(Sender : TObject);
begin
  mnuSaveClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileSaveAs(Sender : TObject);
begin
  mnuSaveAsClicked(Sender);
end;

Procedure TMainIDE.OnSrcNoteBookFindDeclaration(Sender : TObject);
begin
  mnuFindDeclarationClicked(Sender);
end;

procedure TMainIDE.OnSrcNotebookInitIdentCompletion(Sender: TObject;
  var Handled, Abort: boolean);
begin
  Handled:=true;
  Abort:=not DoInitIdentCompletion;
end;

Procedure TMainIDE.OnSrcNotebookSaveAll(Sender : TObject);
begin
  mnuSaveAllClicked(Sender);
end;

procedure TMainIDE.OnSrcNotebookToggleFormUnit(Sender : TObject);
begin
  mnuToggleFormUnitClicked(Sender);
end;

procedure TMainIDE.OnSrcNotebookToggleObjectInsp(Sender: TObject);
begin
  mnuViewInspectorClicked(Sender);
end;

procedure TMainIDE.OnProcessIDECommand(Sender: TObject;
  Command: word;  var Handled: boolean);
var
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  Handled:=true;
  
  case Command of
   ecSave:
     if Sender is TDesigner then begin
       GetDesignerUnit(TDesigner(Sender),ASrcEdit,AnUnitInfo);
       if (AnUnitInfo<>nil) and (AnUnitInfo.EditorIndex>=0) then
         DoSaveEditorFile(AnUnitInfo.EditorIndex,[sfCheckAmbigiousFiles]);
     end else if Sender is TSourceNotebook then
       mnuSaveClicked(Self);

   ecOpen:
     mnuOpenClicked(Self);

   ecSaveAll:
     DoSaveAll([sfCheckAmbigiousFiles]);

   ecBuild,
   ecBuildAll:    DoBuildProject(Command=ecBuildAll);
    
   ecRun:         DoRunProject;
   ecPause:       DebugBoss.DoPauseProject;
   ecStepInto:    DebugBoss.DoStepIntoProject;
   ecStepOver:    DebugBoss.DoStepOverProject;
   ecRunToCursor: DebugBoss.DoRunToCursor;
   ecStopProgram: DebugBoss.DoStopProject;
    
   ecFindProcedureDefinition,
   ecFindProcedureMethod:
     DoJumpToProcedureSection;
      
   ecFindDeclaration:
     DoFindDeclarationAtCursor;
     
   ecFindBlockOtherEnd:
     DoGoToPascalBlockOtherEnd;
     
   ecFindBlockStart:
     DoGoToPascalBlockStart;
     
   ecGotoIncludeDirective:
     DoGotoIncludeDirective;
    
   ecCompleteCode:
     DoCompleteCodeAtCursor;
     
   ecConfigCustomComps:
     PkgBoss.ShowConfigureCustomComponents;
      
   ecExtToolFirst..ecExtToolLast:
     DoRunExternalTool(Command-ecExtToolFirst);
    
   ecSyntaxCheck:
     DoCheckSyntax;
     
   ecGuessUnclosedBlock:
     DoJumpToGuessedUnclosedBlock(true);
    
   ecGuessMisplacedIFDEF:
     DoJumpToGuessedMisplacedIFDEF(true);

   ecMakeResourceString:
     DoMakeResourceString;

   ecDiff:
     DoDiff;

   ecConvertDFM2LFM:
     DoConvertDFMtoLFM;

   ecBuildLazarus:
     DoBuildLazarus([]);
     
   ecConfigBuildLazarus:
     mnuToolConfigBuildLazClicked(Self);
     
   ecToggleFormUnit:
     mnuToggleFormUnitClicked(Self);
     
   ecToggleObjectInsp:
     mnuViewInspectorClicked(Self);  
     
   ecAboutLazarus:
     mnuHelpAboutLazarusClicked(Self);

  else
    Handled:=false;
  end;
end;

procedure TMainIDE.OnSrcNoteBookCtrlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftstate; X, Y: Integer);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if ActiveSrcEdit=nil then exit;
  DoFindDeclarationAtCaret(
    ActiveSrcEdit.EditorComponent.PixelsToRowColumn(Point(X,Y)));
end;

procedure TMainIDE.OnSrcNoteBookShowUnitInfo(Sender: TObject);
var ActiveSrcEdit:TSourceEditor;
  ActiveUnitInfo:TUnitInfo;
  ShortUnitName, AFilename, FileDir: string;
  ClearIncludedByFile: boolean;
begin
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  ShortUnitName:=ActiveSrcEdit.PageName;
  AFilename:=ActiveUnitInfo.Filename;
  FileDir:=ExtractFilePath(AFilename);
  ShowUnitInfoDlg(ShortUnitName,
    LazSyntaxHighlighterNames[ActiveUnitInfo.SyntaxHighlighter],
    ActiveUnitInfo.IsPartOfProject, length(ActiveSrcEdit.Source.Text),
    ActiveSrcEdit.Source.Count,
    Project1.RemoveProjectPathFromFilename(AFilename),
    Project1.RemoveProjectPathFromFilename(ActiveUnitInfo.Source.LastIncludedByFile),
    ClearIncludedByFile,
    TrimSearchPath(CodeToolBoss.GetUnitPathForDirectory(FileDir),FileDir),
    TrimSearchPath(CodeToolBoss.GetIncludePathForDirectory(FileDir),FileDir),
    TrimSearchPath(CodeToolBoss.GetSrcPathForDirectory(FileDir),FileDir)
    );
  if ClearIncludedByFile then
    ActiveUnitInfo.Source.LastIncludedByFile:='';
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.OpenFileDownArrowClicked(Sender : TObject);
var
  CurIndex: integer;
  
  procedure AddFile(const Filename: string);
  var
    AMenuItem: TMenuItem;
  begin
    if OpenFilePopupMenu.Items.Count>CurIndex then
      AMenuItem:=OpenFilePopupMenu.Items[CurIndex]
    else begin
      AMenuItem:=TMenuItem.Create(Self);
      AMenuItem.Name:=OpenFilePopupMenu.Name+'Recent'+IntToStr(CurIndex);
      AMenuItem.OnClick:=@mnuOpenFilePopupClick;
      OpenFilePopupMenu.Items.Add(AMenuItem);
    end;
    AMenuItem.Caption:=Filename;
    inc(CurIndex);
  end;

  procedure AddFiles(List: TStringList; MaxCount: integer);
  var i: integer;
  begin
    i:=0;
    while (i<List.Count) and (i<MaxCount) do begin
      AddFile(List[i]);
      inc(i);
    end;
  end;

Begin
  // fill the PopupMenu:
  CurIndex:=0;
  // first add 8 recent projects
  AddFiles(EnvironmentOptions.RecentProjectFiles,8);
  // add a separator
  AddFile('-');
  // add 12 recent files
  AddFiles(EnvironmentOptions.RecentOpenFiles,12);
  // remove unused menuitems
  while OpenFilePopupMenu.Items.Count>CurIndex do
    OpenFilePopupMenu.Items[OpenFilePopupMenu.Items.Count-1].Free;
  // display the PopupMenu
  if OpenFilePopupMenu.Items.Count > 0 then
    OpenFilePopupMenu.Popup(0,0);
end;

procedure TMainIDE.mnuOpenFilePopupClick(Sender: TObject);
var
  TheMenuItem: TMenuItem;
  Index, SeparatorIndex: integer;
  AFilename: string;
begin
  TheMenuItem:=TMenuItem(Sender);
  if TheMenuItem.Caption='-' then exit;
  Index:=TheMenuItem.MenuIndex;
  SeparatorIndex:=0;
  while SeparatorIndex<OpenFilePopupMenu.Items.Count do begin
    if OpenFilePopupMenu.Items[SeparatorIndex].Caption='-' then break;
    inc(SeparatorIndex);
  end;
  if Index=SeparatorIndex then exit;
  if Index<SeparatorIndex then begin
    // open recent project
    AFilename:=EnvironmentOptions.RecentProjectFiles[Index];
    DoOpenProjectFile(AFileName,[ofAddToRecent]);
  end else begin
    // open recent file
    dec(Index, SeparatorIndex+1);
    if DoOpenEditorFile(EnvironmentOptions.RecentOpenFiles[Index],-1,
      [ofAddToRecent])=mrOk then
    begin
      SetRecentFilesMenu;
      SaveEnvironment;
    end;
  end;
end;

Procedure TMainIDE.SetDefaultsforForm(aForm : TCustomForm);
Begin
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.SetDefaultsforForm] A');
  {$ENDIF}
  aForm.Designer := TDesigner.Create(aForm, TheControlSelection);
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.SetDefaultsforForm] B');
  {$ENDIF}
  with TDesigner(aForm.Designer) do begin
    TheFormEditor := FormEditor1;
    OnActivated:=@OnDesignerActivated;
    OnComponentAdded:=@OnDesignerComponentAdded;
    OnComponentDeleted:=@OnDesignerComponentDeleted;
    {$IFDEF EnablePkgs}
    OnGetNonVisualCompIconCanvas:=
      @TComponentPalette(IDEComponentPalette).OnGetNonVisualCompIconCanvas;
    {$ELSE}
    OnGetNonVisualCompIconCanvas:=@IDECompList.OnGetNonVisualCompIconCanvas;
    {$ENDIF}
    OnGetSelectedComponentClass:=@OnDesignerGetSelectedComponentClass;
    OnModified:=@OnDesignerModified;
    OnProcessCommand:=@OnProcessIDECommand;
    OnPropertiesChanged:=@OnDesignerPropertiesChanged;
    OnRemoveComponent:=@OnDesignerRemoveComponent;
    OnRenameComponent:=@OnDesignerRenameComponent;
    OnSetDesigning:=@OnDesignerSetDesigning;
    OnShowOptions:=@OnDesignerShowOptions;
    OnUnselectComponentClass:=@OnDesignerUnselectComponentClass;
    ShowEditorHints:=EnvironmentOptions.ShowEditorHints;
    ShowComponentCaptionHints:=EnvironmentOptions.ShowComponentCaptions;
  end;
  SetDesigning(AForm,True);
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.InvalidateAllDesignerForms
  Params: none
  Result: none
  
  Calls 'Invalidate' in all designer forms.
-------------------------------------------------------------------------------}
procedure TMainIDE.InvalidateAllDesignerForms;
var
  AnUnitInfo: TUnitInfo;
begin
  AnUnitInfo:=Project1.FirstUnitWithForm;
  while AnUnitInfo<>nil do begin
    if AnUnitInfo.Form<>nil then begin
      if AnUnitInfo.Form is TControl then
        TControl(AnUnitInfo.Form).Invalidate;
    end;
    AnUnitInfo:=AnUnitInfo.NextUnitWithForm;
  end;
end;


{------------------------------------------------------------------------------}

procedure TMainIDE.mnuQuitClicked(Sender : TObject);
var CanClose: boolean;
begin
  CanClose:=true;
  OnCloseQuery(Sender, CanClose);
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.mnuQuitClicked 1');
  {$ENDIF}
  if CanClose then Close;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.mnuQuitClicked 2');
  {$ENDIF}
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.mnuViewInspectorClicked(Sender : TObject);
begin
  DoBringToFrontFormOrInspector;
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuViewUnitsClicked(Sender : TObject);
begin
  DoViewUnitsAndForms(false);
end;

Procedure TMainIDE.mnuViewFormsClicked(Sender : TObject);
Begin
  DoViewUnitsAndForms(true);
end;

Procedure TMainIDE.mnuViewUnitDependenciesClicked(Sender : TObject);
begin
  DoViewUnitDependencies;
end;

Procedure TMainIDE.mnuViewCodeExplorerClick(Sender : TObject);
begin
  SourceNotebook.Show;
end;

Procedure TMainIDE.mnuViewMessagesClick(Sender : TObject);
Begin
  MessagesView.Show;
End;


{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuNewProjectClicked(Sender : TObject);
var
  NewProjectType: TProjectType;
Begin
  if ChooseNewProject(NewProjectType)=mrCancel then exit;
  DoNewProject(NewProjectType);
end;

procedure TMainIDE.mnuNewProjectFromFileClicked(Sender: TObject);
var
  OpenDialog:TOpenDialog;
  AFilename: string;
  PreReadBuf: TCodeBuffer;
Begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:='Choose program source (*.pp,*.pas,*.lpr)';
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    if OpenDialog.Execute then begin
      AFilename:=ExpandFilename(OpenDialog.Filename);
      if not FilenameIsPascalSource(AFilename) then begin
        MessageDlg('Invalid file extension',
          'Program source must have a pascal extension like .pas, .pp or .lpr',
          mtError,[mbOk],0);
        exit;
      end;
      if mrOk<>DoLoadCodeBuffer(PreReadBuf,AFileName,
                          [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert])
      then
        exit;
      if DoCreateProjectForProgram(PreReadBuf)=mrOk then begin
      
        exit;
      end;
    end;
  finally
    InputHistories.StoreFileDialogSettings(OpenDialog);
    OpenDialog.Free;
  end;
end;

Procedure TMainIDE.mnuOpenProjectClicked(Sender : TObject);
var
  OpenDialog:TOpenDialog;
  AFileName: string;
begin
  if Sender=itmProjectOpen then begin
    OpenDialog:=TOpenDialog.Create(Application);
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Title:=lisOpenProjectFile+' (*.lpi)';
      OpenDialog.Filter := 'Lazarus Project Info (*.lpi)|*.lpi|All Files|*.*';
      if OpenDialog.Execute then begin
        AFilename:=ExpandFilename(OpenDialog.Filename);
        DoOpenProjectFile(AFilename,[ofAddToRecent]);
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    finally
      OpenDialog.Free;
    end;
  end else if Sender is TMenuItem then begin
    AFileName:=ExpandFilename(TMenuItem(Sender).Caption);
    if DoOpenProjectFile(AFilename,[ofAddToRecent])=mrOk then begin
      AddRecentFileToEnvironment(AFilename);
    end else begin
      // open failed
      if not FileExists(AFilename) then begin
        EnvironmentOptions.RemoveFromRecentProjectFiles(AFilename);
        AddRecentFileToEnvironment(AFilename);
      end;
    end;
  end;
end;

Procedure TMainIDE.mnuSaveProjectClicked(Sender : TObject);
Begin
  DoSaveProject([]);
end;

procedure TMainIDE.mnuSaveProjectAsClicked(Sender : TObject);
begin
  DoSaveProject([sfSaveAs]);
end;

procedure TMainIDE.mnuPublishProjectClicked(Sender: TObject);
begin
  DoPublishProject([],true);
end;

procedure TMainIDE.mnuProjectInspectorClicked(Sender: TObject);
begin
  DoShowProjectInspector;
end;

procedure TMainIDE.mnuAddToProjectClicked(Sender : TObject);
begin
  DoAddActiveUnitToProject;
end;

procedure TMainIDE.mnuRemoveFromProjectClicked(Sender : TObject);
begin
  DoRemoveFromProjectDialog;
end;

procedure TMainIDE.mnuViewProjectSourceClicked(Sender : TObject);
begin
  DoOpenMainUnit(false);
end;

procedure TMainIDE.mnuViewProjectTodosClicked(Sender: TObject);
begin
  DoShowToDoList;
end;

procedure TMainIDE.mnuProjectOptionsClicked(Sender : TObject);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  BeginCodeTool(ActiveSrcEdit, ActiveUnitInfo, []);
  if ShowProjectOptionsDialog(Project1)=mrOk then begin
    
  end;
end;

Procedure TMainIDE.mnuBuildProjectClicked(Sender : TObject);
Begin
  DoBuildProject(false);
end;

Procedure TMainIDE.mnuBuildAllProjectClicked(Sender : TObject);
Begin
  DoBuildProject(true);
end;

Procedure TMainIDE.mnuRunProjectClicked(Sender : TObject);
begin
  DoRunProject;
end;

Procedure TMainIDE.mnuPauseProjectClicked(Sender : TObject);
begin
  DebugBoss.DoPauseProject;
end;

Procedure TMainIDE.mnuStepIntoProjectClicked(Sender : TObject);
begin
  DebugBoss.DoStepIntoProject;
end;

Procedure TMainIDE.mnuStepOverProjectClicked(Sender : TObject);
begin
  DebugBoss.DoStepOverProject;
end;

Procedure TMainIDE.mnuRunToCursorProjectClicked(Sender : TObject);
begin
  DebugBoss.DoRunToCursor;
end;

Procedure TMainIDE.mnuStopProjectClicked(Sender : TObject);
begin
  DebugBoss.DoStopProject;
end;

procedure TMainIDE.mnuProjectCompilerSettingsClicked(Sender : TObject);
var frmCompilerOptions:TfrmCompilerOptions;
  NewCaption: String;
begin
  frmCompilerOptions:=TfrmCompilerOptions.Create(Application);
  try
    NewCaption:=Project1.Title;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(Project1.ProjectInfoFile);
    frmCompilerOptions.Caption:='Compiler Options for Project: '+NewCaption;
    frmCompilerOptions.CompilerOpts:=Project1.CompilerOptions;
    frmCompilerOptions.GetCompilerOptions;
    if frmCompilerOptions.ShowModal=mrOk then begin
      Project1.DefineTemplates.AllChanged;
    end;
  finally
    frmCompilerOptions.Free;
  end;
end;

procedure TMainIDE.mnuRunParametersClicked(Sender : TObject);
begin
  ShowRunParamsOptsDlg(Project1.RunParameterOptions);
end;

//------------------------------------------------------------------------------

procedure TMainIDE.mnuToolConfigureClicked(Sender : TObject);
begin
  if ShowExtToolDialog(EnvironmentOptions.ExternalTools,MacroList)=mrOk then
  begin
    // save to enviroment options
    SaveDesktopSettings(EnvironmentOptions);
    EnvironmentOptions.Save(false);
    // save shortcuts to editor options
    EnvironmentOptions.ExternalTools.SaveShortCuts(EditorOpts.KeyMap);
    EditorOpts.Save;
    SourceNotebook.ReloadEditorOptions;
    UpdateCustomToolsInMenu;
  end;
end;

procedure TMainIDE.mnuToolSyntaxCheckClicked(Sender : TObject);
begin
  DoCheckSyntax;
end;

procedure TMainIDE.mnuToolGuessUnclosedBlockClicked(Sender : TObject);
begin
  DoJumpToGuessedUnclosedBlock(true);
end;

procedure TMainIDE.mnuToolGuessMisplacedIFDEFClicked(Sender : TObject);
begin
  DoJumpToGuessedMisplacedIFDEF(true);
end;

procedure TMainIDE.mnuToolMakeResourceStringClicked(Sender : TObject);
begin
  DoMakeResourceString;
end;

procedure TMainIDE.mnuToolDiffClicked(Sender : TObject);
begin
  DoDiff;
end;

procedure TMainIDE.mnuToolConvertDFMtoLFMClicked(Sender : TObject);
begin
  DoConvertDFMtoLFM;
end;

procedure TMainIDE.mnuToolBuildLazarusClicked(Sender : TObject);
begin
  DoBuildLazarus([]);
end;

procedure TMainIDE.mnuToolConfigBuildLazClicked(Sender : TObject);
var
  CmdLineDefines: TDefineTemplate;
  LazSrcTemplate: TDefineTemplate;
  LazSrcDirTemplate: TDefineTemplate;
begin
  if ShowConfigureBuildLazarusDlg(MiscellaneousOptions.BuildLazOpts)=mrOk then
  begin
    MiscellaneousOptions.Save;
    LazSrcTemplate:=CodeToolBoss.DefineTree.FindDefineTemplateByName(
                                                StdDefTemplLazarusSources,true);
    if LazSrcTemplate<>nil then begin
      LazSrcDirTemplate:=LazSrcTemplate.FindChildByName(
                                                      StdDefTemplLazarusSrcDir);
      if LazSrcDirTemplate<>nil then begin
        CmdLineDefines:=CodeToolBoss.DefinePool.CreateFPCCommandLineDefines(
                                StdDefTemplLazarusBuildOpts,
                                MiscellaneousOptions.BuildLazOpts.ExtraOptions,
                                true,CodeToolsOpts);
        CodeToolBoss.DefineTree.ReplaceChild(LazSrcDirTemplate,CmdLineDefines,
                                             StdDefTemplLazarusBuildOpts);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.mnuCustomExtToolClick(Sender: TObject);
  
  Handler for clicking on a menuitem for a custom external tool.
-------------------------------------------------------------------------------}
procedure TMainIDE.mnuCustomExtToolClick(Sender: TObject);
var
  Index: integer;
begin
  if CustomExtToolMenuSeparator=nil then exit;
  Index:=TMenuItem(Sender).MenuIndex-CustomExtToolMenuSeparator.MenuIndex-1;
  if (Index<0) or (Index>=EnvironmentOptions.ExternalTools.Count) then exit;
  DoRunExternalTool(Index);
end;

procedure TMainIDE.mnuEnvGeneralOptionsClicked(Sender: TObject);
begin
  DoShowEnvGeneralOptions(eodpFiles);
end;

//------------------------------------------------------------------------------

procedure TMainIDE.SaveDesktopSettings(
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  with TheEnvironmentOptions do begin
    IDEWindowLayoutList.StoreWindowPositions;
    ObjectInspectorOptions.Assign(ObjectInspector1);
  end;
end;

procedure TMainIDE.LoadDesktopSettings(
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  with TheEnvironmentOptions do begin
    ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;
end;

procedure TMainIDE.UpdateDefaultPascalFileExtensions;
var nut: TNewUnitType;
  npt: TProjectType;
  DefPasExt: string;
begin
  // change default pascal file extensions
  DefPasExt:=PascalExtension[EnvironmentOptions.PascalFileExtension];
  for nut:=Low(TNewUnitType) to High(TNewUnitType) do
    if (UnitTypeDefaultExt[nut]='.pas') or (UnitTypeDefaultExt[nut]='.pp')
    then UnitTypeDefaultExt[nut]:=DefPasExt;
  for npt:=Low(TProjectType) to High(TProjectType) do
    if (ProjectDefaultExt[npt]='.pas') or (ProjectDefaultExt[npt]='.pp')
    then ProjectDefaultExt[npt]:=DefPasExt;
end;

function TMainIDE.CreateSrcEditPageName(const AnUnitName, AFilename: string;
  IgnorePageIndex: integer): string;
begin
  Result:=AnUnitName;
  if Result='' then
    Result:=AFilename;
  if FilenameIsPascalUnit(Result) then
    Result:=ExtractFileNameOnly(Result)
  else
    Result:=ExtractFileName(Result);
  Result:=SourceNoteBook.FindUniquePageName(Result,IgnorePageIndex);
end;

function TMainIDE.DoConvertDFMFileToLFMFile(const DFMFilename: string
  ): TModalResult;
var DFMStream, LFMStream: TMemoryStream;
  LFMFilename: string;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    try
      DFMStream.LoadFromFile(DFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg('Read error','Unable to read file "'+DFMFilename+'"'#13
          +'Error: '+E.Message,
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=MessageDlg('Format error',
          'Unable to convert file "'+DFMFilename+'"'#13
          +'Error: '+E.Message,
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
    LFMFilename:=ChangeFileExt(DFMFilename,'.lfm');
    try
      LFMStream.SaveToFile(LFMFilename);
    except
      on E: Exception do begin
        Result:=MessageDlg('Write error',
          'Unable to write file "'+LFMFilename+'"'#13
          +'Error: '+E.Message,
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

procedure TMainIDE.OnLoadEnvironmentSettings(Sender: TObject; 
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  LoadDesktopSettings(TheEnvironmentOptions);
end;

procedure TMainIDE.OnSaveEnvironmentSettings(Sender: TObject; 
  TheEnvironmentOptions: TEnvironmentOptions);
begin
  SaveDesktopSettings(TheEnvironmentOptions);
end;

procedure TMainIDE.DoShowEnvGeneralOptions(StartPage: TEnvOptsDialogPage);
var EnvironmentOptionsDialog: TEnvironmentOptionsDialog;
  MacroValueChanged, FPCSrcDirChanged, FPCCompilerChanged: boolean;
  OldCompilerFilename, CompilerUnitSearchPath, CompilerUnitLinks: string;
  CompilerTemplate, FPCSrcTemplate: TDefineTemplate;
  
  procedure ChangeMacroValue(const MacroName, NewValue: string);
  begin
    with CodeToolBoss.GlobalValues do begin
      if Variables[ExternalMacroStart+MacroName]=NewValue then exit;
      FPCSrcDirChanged:=FPCSrcDirChanged or (Macroname='FPCSrcDir');
      Variables[ExternalMacroStart+MacroName]:=NewValue;
    end;
    MacroValueChanged:=true;
  end;
  
  procedure RescanCompilerDefines;
  begin
    // rescan compiler defines
    // ask the compiler for its settings
    CompilerTemplate:=CodeToolBoss.DefinePool.CreateFPCTemplate(
                      EnvironmentOptions.CompilerFilename,
                      CreateCompilerTestPascalFilename,CompilerUnitSearchPath,
                      CodeToolsOpts);
    if CompilerTemplate<>nil then begin
      CodeToolBoss.DefineTree.ReplaceRootSameNameAddFirst(CompilerTemplate);
      // create compiler macros to simulate the Makefiles of the FPC sources
      FPCSrcTemplate:=CodeToolBoss.DefinePool.CreateFPCSrcTemplate(
        CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir'],
        CompilerUnitSearchPath, false, CompilerUnitLinks,CodeToolsOpts);
      if FPCSrcTemplate<>nil then begin
        CodeToolBoss.DefineTree.RemoveRootDefineTemplateByName(
                                                       FPCSrcTemplate.Name);
        FPCSrcTemplate.InsertBehind(CompilerTemplate);
      end else begin
        MessageDlg(lisFPCSourceDirectoryError,
          lisPLzCheckTheFPCSourceDirectory,
          mtError,[mbOk],0);
      end;
      // save unitlinks
      InputHistories.SetLastFPCUnitLinks(
             EnvironmentOptions.CompilerFilename,
             CompilerUnitSearchPath,CompilerUnitLinks);
      InputHistories.Save;
    end else begin
      MessageDlg(lisCompilerError,lisPlzCheckTheCompilerName,
        mtError,[mbOk],0);
    end;
  end;
  
  procedure UpdateDesigners;
  var
    AForm: TCustomForm;
    AnUnitInfo: TUnitInfo;
    ADesigner: TDesigner;
  begin
    AnUnitInfo:=Project1.FirstUnitWithForm;
    while AnUnitInfo<>nil do begin
      if (AnUnitInfo.Form<>nil) and (AnUnitInfo.Form is TCustomForm) then begin
        AForm:=TCustomForm(AnUnitInfo.Form);
        ADesigner:=TDesigner(AForm.Designer);
        if ADesigner<>nil then begin
          ADesigner.ShowEditorHints:=EnvironmentOptions.ShowEditorHints;
          ADesigner.ShowComponentCaptionHints:=
            EnvironmentOptions.ShowComponentCaptions;
        end;
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithForm;
    end;
    InvalidateAllDesignerForms;
  end;
  
  procedure UpdateObjectInspector;
  begin
    EnvironmentOptions.ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;
  
Begin
  EnvironmentOptionsDialog:=TEnvironmentOptionsDialog.Create(Application);
  try
    EnvironmentOptionsDialog.CategoryPage:=StartPage;
    // update EnvironmentOptions (save current window positions)
    SaveDesktopSettings(EnvironmentOptions);
    with EnvironmentOptionsDialog do begin
      OnLoadEnvironmentSettings:=@Self.OnLoadEnvironmentSettings;
      OnSaveEnvironmentSettings:=@Self.OnSaveEnvironmentSettings;
      // load settings from EnvironmentOptions to EnvironmentOptionsDialog
      ReadSettings(EnvironmentOptions);
    end;
    if EnvironmentOptionsDialog.ShowModal=mrOk then begin
      // invalidate cached substituted macros
      IncreaseCompilerParseStamp;
      
      // load settings from EnvironmentOptionsDialog to EnvironmentOptions
      OldCompilerFilename:=EnvironmentOptions.CompilerFilename;
      EnvironmentOptionsDialog.WriteSettings(EnvironmentOptions);
      UpdateDefaultPascalFileExtensions;
      
      // set global variables
      UpdateEnglishErrorMsgFilename;
      MacroValueChanged:=false;
      FPCSrcDirChanged:=false;
      FPCCompilerChanged:=
        OldCompilerFilename<>EnvironmentOptions.CompilerFilename;
      ChangeMacroValue('LazarusDir',EnvironmentOptions.LazarusDirectory);
      ChangeMacroValue('FPCSrcDir',EnvironmentOptions.FPCSourceDirectory);
      
      if MacroValueChanged then CodeToolBoss.DefineTree.ClearCache;
      if FPCCompilerChanged or FPCSrcDirChanged then begin
        RescanCompilerDefines;
      end;
        
      // save to disk
      EnvironmentOptions.Save(false);
      
      // update environment
      UpdateDesigners;
      UpdateObjectInspector;
      SetupHints;
    end;
  finally
    EnvironmentOptionsDialog.Free;
  end;
End;

procedure TMainIDE.mnuEnvEditorOptionsClicked(Sender : TObject);
var EditorOptionsForm: TEditorOptionsForm;
Begin
  EditorOptionsForm:=TEditorOptionsForm.Create(Application);
  try
    if EditorOptionsForm.ShowModal=mrOk then begin
      SourceNotebook.ReloadEditorOptions;
      LoadMenuShortCuts;
    end;
  finally
    EditorOptionsForm.Free;
  end;
End;

procedure TMainIDE.mnuEnvCodeToolsOptionsClicked(Sender : TObject);
begin
  ShowCodeToolsOptions(CodeToolsOpts,@SourceNoteBook.GetSynEditPreviewSettings);
end;

procedure TMainIDE.mnuEnvCodeToolsDefinesEditorClicked(Sender : TObject);
begin
  ShowCodeToolsDefinesEditor(CodeToolBoss,CodeToolsOpts,MacroList);
end;

procedure TMainIDE.SaveEnvironment;
begin
  SaveDesktopSettings(EnvironmentOptions);
  EnvironmentOptions.Save(false);
end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------

procedure TMainIDE.mnuHelpAboutLazarusClicked(Sender : TObject);
begin
  ShowAboutForm;
end;

//------------------------------------------------------------------------------

Procedure TMainIDE.MessageViewDblClick(Sender : TObject);
Begin

end;

//==============================================================================

function TMainIDE.CreateNewCodeBuffer(NewUnitType:TNewUnitType;
  NewFilename: string;
  var NewCodeBuffer: TCodeBuffer; var NewUnitName: string): TModalResult;
begin
  if NewFilename='' then begin
    NewUnitName:=Project1.NewUniqueUnitName(NewUnitType);
    NewCodeBuffer:=CodeToolBoss.CreateFile(
                                   NewUnitName+UnitTypeDefaultExt[NewUnitType]);
  end else begin
    NewUnitName:=ExtractFileNameOnly(NewFilename);
    if FilenameIsPascalUnit(NewFilename) then begin
      if EnvironmentOptions.PascalFileAutoLowerCase
      or EnvironmentOptions.PascalFileAskLowerCase then
        NewFilename:=ExtractFilePath(NewFilename)
                     +lowercase(ExtractFileName(NewFilename));
    end;
    NewCodeBuffer:=CodeToolBoss.CreateFile(NewFilename);
  end;
  if NewCodeBuffer<>nil then
    Result:=mrOk
  else
    Result:=mrCancel;
end;

function TMainIDE.CreateNewForm(NewUnitInfo: TUnitInfo): TModalResult;
var
  NewForm: TCustomForm;
  ResourceCode: TCodeBuffer;
  CInterface : TComponentInterface;
begin
  // create a buffer for the new resource file and for the LFM file
  ResourceCode:=
    CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,ResourceFileExt));
  ResourceCode.Source:='{ '+lisResourceFileComment+' }';
  CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,'.lfm'));

  // clear formeditor
  if not Assigned(FormEditor1) then
    FormEditor1 := TFormEditor.Create;
  FormEditor1.ClearSelected;

  // create jitform
  CInterface := TComponentInterface(
    FormEditor1.CreateComponent(nil,TForm,
      ObjectInspector1.Left+ObjectInspector1.Width+60,Top+Height+80,400,300));
  FormEditor1.SetFormNameAndClass(CInterface,
    NewUnitInfo.FormName,'T'+NewUnitInfo.FormName);
  NewForm:=TForm(CInterface.Component);
  NewUnitInfo.Form:=NewForm;
  SetDefaultsForForm(NewForm);

  NewUnitInfo.FormName:=NewForm.Name;
  NewUnitInfo.FormResourceName:=NewUnitInfo.FormName;
  if NewUnitInfo.IsPartOfProject and Project1.AutoCreateForms then
    Project1.AddCreateFormToProjectFile(NewForm.ClassName,NewForm.Name);
    
  Result:=mrOk;
end;

procedure TMainIDE.ShowDesignForm(AForm: TCustomForm);
begin
  // show form
  AForm.Show;
  FDisplayState:= dsForm;

  // select the new form (object inspector, formeditor, control selection)
  PropertyEditorHook1.LookupRoot := AForm;
  TDesigner(AForm.Designer).SelectOnlyThisComponent(AForm);
  AForm.ShowOnTop;
end;

function TMainIDE.DoLoadResourceFile(AnUnitInfo: TUnitInfo;
  var LFMCode, ResourceCode: TCodeBuffer;
  IgnoreSourceErrors: boolean): TModalResult;
var LinkIndex: integer;
  LFMFilename, MsgTxt: string;
begin
  LFMCode:=nil;
  ResourceCode:=nil;
  if AnUnitInfo.HasResources then begin
    // first try to find the resource file via the unit source
    LinkIndex:=-1;
    ResourceCode:=CodeToolBoss.FindNextResourceFile(
      AnUnitInfo.Source,LinkIndex);
    // if unit source has errors, then try the last resource file
    if (ResourceCode=nil) then begin
      if not IgnoreSourceErrors then
        DoJumpToCodeToolBossError;
      if (AnUnitInfo.ResourceFileName<>'')
      then begin
        Result:=DoLoadCodeBuffer(ResourceCode,AnUnitInfo.ResourceFileName,
                                       [lbfCheckIfText]);
        if Result=mrAbort then exit;
      end;
    end;
    // if no resource file found then tell the user
    if (ResourceCode=nil) and (not IgnoreSourceErrors)
    then begin
      MsgTxt:='Unable to load old resource file.'#13
              +'The resource file is the first include file in the'#13
              +'initialization section.'#13
              +'For example {$I '+AnUnitInfo.UnitName+'.lrs}.'#13
              +'Probably a syntax error.';
      Result:=MessageDlg('Resource load error',MsgTxt,mtWarning,
                         [mbIgnore,mbAbort],0);
      if Result=mrAbort then exit;
    end;
    // load lfm file
    if (not AnUnitInfo.IsVirtual) and (AnUnitInfo.Form<>nil) then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
      if (FileExists(LFMFilename)) then begin
        Result:=DoLoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText]);
        if not (Result in [mrOk,mrIgnore]) then exit;
      end;
    end;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoShowSaveFileAsDialog(AnUnitInfo: TUnitInfo;
  var ResourceCode: TCodeBuffer): TModalResult;
var
  SaveDialog: TSaveDialog;
  SaveAsFilename, SaveAsFileExt, NewFilename, NewUnitName, NewFilePath,
  NewResFilename, NewResFilePath, OldFilePath, NewLFMFilename,
  AlternativeUnitName: string;
  ACaption, AText: string;
  SrcEdit: TSourceEditor;
  NewSource: TCodeBuffer;
  NewHighlighter: TLazSyntaxHighlighter;
  FileWithoutPath: String;
  AmbigiousFiles: TStringList;
  i: Integer;
  AmbigiousFilename: String;
  PkgDefaultDirectory: String;
  OldFilename: String;
begin
  SrcEdit:=GetSourceEditorForUnitInfo(AnUnitInfo);
  OldFilePath:=ExtractFilePath(AnUnitInfo.Filename);
  OldFilename:=AnUnitInfo.Filename;
  
  // try to keep the old filename and extension
  SaveAsFileExt:=ExtractFileExt(AnUnitInfo.FileName);
  if SaveAsFileExt='' then begin
    if SrcEdit.SyntaxHighlighterType in [lshFreePascal, lshDelphi]
    then
      SaveAsFileExt:=PascalExtension[EnvironmentOptions.PascalFileExtension]
    else
      SaveAsFileExt:=EditorOpts.HighlighterList.GetDefaultFilextension(
                         SrcEdit.SyntaxHighlighterType);
  end;
  AnUnitInfo.ReadUnitNameFromSource;
  SaveAsFilename:=AnUnitInfo.UnitName;
  if SaveAsFilename='' then
    SaveAsFilename:=ExtractFileNameOnly(AnUnitInfo.Filename);
  if SaveAsFilename='' then
    SaveAsFilename:='noname';
    
  // let user choose a filename
  SaveDialog:=TSaveDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=lisSaveSpace+SaveAsFilename+' (*'+SaveAsFileExt+')';
    SaveDialog.FileName:=SaveAsFilename+SaveAsFileExt;
    // if this is a project file, start in project directory
    if AnUnitInfo.IsPartOfProject and (not Project1.IsVirtual)
    and (not FileIsInPath(SaveDialog.InitialDir,Project1.ProjectDirectory)) then
      SaveDialog.InitialDir:=Project1.ProjectDirectory;
    // if this is a package file, then start in package directory
    if (not AnUnitInfo.IsVirtual) then begin
      PkgDefaultDirectory:=
        PkgBoss.GetDefaultSaveDirectoryForFile(AnUnitInfo.Filename);
      if (PkgDefaultDirectory<>'')
      and (not FileIsInPath(SaveDialog.InitialDir,PkgDefaultDirectory)) then
        SaveDialog.InitialDir:=PkgDefaultDirectory;
    end;
    // show save dialog
    if (not SaveDialog.Execute) or (ExtractFileName(SaveDialog.Filename)='')
    then begin
      // user cancels
      Result:=mrCancel;
      exit;
    end;
    NewFilename:=ExpandFilename(SaveDialog.Filename);
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;

  // check file path
  NewFilePath:=ExtractFilePath(NewFilename);
  if not DirectoryExists(NewFilePath) then begin
    ACaption:='Directory not found';
    AText:='The destination directory'#13+
      '"'+NewFilePath+'" does not exist.';
    MessageDlg(ACaption, AText, mtConfirmation,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;

  // check unitname
  if FilenameIsPascalUnit(NewFilename) then begin
    NewUnitName:=ExtractFileNameOnly(NewFilename);
    if NewUnitName='' then begin
      Result:=mrCancel;
      exit;
    end;
    if not IsValidIdent(NewUnitName) then begin
      AlternativeUnitName:=NameToValidIdentifier(NewUnitName);
      Result:=MessageDlg(lisInvalidPascalIdentifierCap,
        Format(lisInvalidPascalIdentifierText,[NewUnitName,AlternativeUnitName]),
        mtWarning,[mbIgnore,mbCancel],0);
      if Result=mrCancel then exit;
      NewUnitName:=AlternativeUnitName;
      Result:=mrCancel;
    end;
    if Project1.IndexOfUnitWithName(NewUnitName,true,AnUnitInfo)>=0 then
    begin
      Result:=MessageDlg(lisUnitNameAlreadyExistsCap,
         Format(lisUnitNameAlreadyExistsText,[NewUnitName]),
          mtConfirmation,[mbIgnore,mbCancel,mbAbort],0);
      if Result=mrIgnore then
        Result:=mrCancel
      else
        exit;
    end;
  end else begin
    NewUnitName:='';
  end;

  // check file extension
  if ExtractFileExt(NewFilename)='' then begin
    NewFilename:=NewFilename+SaveAsFileExt;
  end;
  
  // check filename
  if FilenameIsPascalUnit(NewFilename) then begin
    FileWithoutPath:=ExtractFileName(NewFilename);
    // check if file should be auto renamed
    if EnvironmentOptions.PascalFileAskLowerCase then begin
      if lowercase(FileWithoutPath)<>FileWithoutPath
      then begin
        Result:=MessageDlg('Rename file?',
           'This looks like a pascal file.'#13
          +'fpc 1.0.x expects pascal files lowercase.'#13
          +'Rename it to lowercase?',
          mtWarning,[mbYes,mbNo],0);
        if Result=mrYes then
          NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
        Result:=mrOk;
      end;
    end else begin
      if EnvironmentOptions.PascalFileAutoLowerCase then
        NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
    end;
  end;
  if (AnUnitInfo.IsVirtual
      or (CompareFilenames(NewFilename,AnUnitInfo.Filename)<>0))
  and FileExists(NewFilename) then begin
    ACaption:='Overwrite file?';
    AText:='A file "'+NewFilename+'" already exists.'#13'Replace it?';
    Result:=MessageDlg(ACaption, AText, mtConfirmation,[mbok,mbCancel],0);
    if Result=mrCancel then exit;
  end;
  
  // check ambigious files
  NewFilePath:=ExtractFilePath(NewFilename);
  AmbigiousFiles:=
    FindFilesCaseInsensitive(NewFilePath,ExtractFilename(NewFilename),true);
  if AmbigiousFiles<>nil then begin
    Result:=MessageDlg('Ambigious files found',
      'There are other files in the directory with the same name,'#13
      +'which only differ in case:'#13
      +AmbigiousFiles.Text+#13
      +'Delete them?',
      mtWarning,[mbYes,mbNo,mbAbort],0);
    if Result=mrAbort then exit;
    if Result=mrYes then begin
      NewFilePath:=AppendPathDelim(ExtractFilePath(NewFilename));
      for i:=0 to AmbigiousFiles.Count-1 do begin
        AmbigiousFilename:=NewFilePath+AmbigiousFiles[i];
        if (FileExists(AmbigiousFilename))
        and (not DeleteFile(AmbigiousFilename))
        and (MessageDlg('Delete failed','Deleting of file "'+AmbigiousFilename+'"'
             +' failed.',mtError,[mbIgnore,mbCancel],0)=mrCancel) then
        begin
          Result:=mrCancel;
          exit;
        end;
      end;
    end;
  end;
  
  // check new resource file
  if AnUnitInfo.FormName='' then begin
    // unit has no form
    // -> remove lfm file, so that it will not be auto loaded on next open
    NewLFMFilename:=ChangeFileExt(NewFilename,'.lfm');
    if (FileExists(NewLFMFilename))
    and (not DeleteFile(NewLFMFilename))
    and (MessageDlg('Delete failed','Deleting of file "'+NewLFMFilename+'"'
         +' failed.',mtError,[mbIgnore,mbCancel],0)=mrCancel) then
    begin
      Result:=mrCancel;
      exit;
    end;
  end;
  
  // save source in the new position
  if not CodeToolBoss.SaveBufferAs(AnUnitInfo.Source,NewFilename,NewSource)
  then begin
    Result:=mrCancel;
    exit;
  end;
  // get final filename
  NewFilename:=NewSource.Filename;
  NewFilePath:=ExtractFilePath(NewFilename);
  EnvironmentOptions.AddToRecentOpenFiles(NewFilename);
  SetRecentFilesMenu;

  // rename Resource file
  if (ResourceCode<>nil) then begin
    // the resource include line in the code will be changed later after
    // changing the unitname
    NewResFilePath:=ExtractFilePath(ResourceCode.Filename);
    if FilenameIsAbsolute(OldFilePath)
    and (OldFilePath=copy(NewResFilePath,1,length(OldFilePath))) then
    begin
      // resource code was in the same or in a sub directory of source
      // -> try to keep this relationship
      NewResFilePath:=NewFilePath
                       +copy(ResourceCode.Filename,length(OldFilePath)+1,
                         length(ResourceCode.Filename));
      if not DirectoryExists(NewResFilePath) then
        NewResFilePath:=NewFilePath;
    end else begin
      // resource code was not in the same or in a sub dircetoy of source
      // copy resource into the same directory as the source
      NewResFilePath:=NewFilePath;
    end;
    NewResFilename:=NewResFilePath
                    +ExtractFileNameOnly(NewFilename)+ResourceFileExt;
    CodeToolBoss.SaveBufferAs(ResourceCode,NewResFilename,ResourceCode);
    if ResourceCode<>nil then
      AnUnitInfo.ResourceFileName:=ResourceCode.Filename;

    {$IFDEF IDE_DEBUG}
    writeln('TMainIDE.ShowSaveFileAsDialog D ',ResourceCode<>nil);
    writeln('   NewResFilePath="',NewResFilePath,'" NewResFilename="',NewresFilename,'"');
    if ResourceCode<>nil then writeln('*** ResourceFileName ',ResourceCode.Filename);
    {$ENDIF}
  end else begin
    NewResFilename:='';
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.ShowSaveFileAsDialog C ',ResourceCode<>nil);
  {$ENDIF}

  // set new codebuffer in unitinfo and sourceeditor
  AnUnitInfo.Source:=NewSource;
  AnUnitInfo.Modified:=false;
  SrcEdit.CodeBuffer:=NewSource; // the code is not changed,
                                 // therefore the marks are kept
                                       
  // change unitname in project and in source
  AnUnitInfo.UnitName:=NewUnitName;
  if ResourceCode<>nil then begin
    // change resource filename in the source include directive
    CodeToolBoss.RenameMainInclude(AnUnitInfo.Source,
      ExtractRelativePath(NewFilePath,NewResFilename),false);
  end;

  // change unitname on SourceNotebook
  UpdateSourceNames;

  // change syntax highlighter
  if not AnUnitInfo.CustomHighlighter then begin
    NewHighlighter:=
      ExtensionToLazSyntaxHighlighter(ExtractFileExt(NewFilename));
    if NewHighlighter<>AnUnitInfo.SyntaxHighlighter then begin
      AnUnitInfo.SyntaxHighlighter:=NewHighlighter;
      SrcEdit.SyntaxHighlighterType:=AnUnitInfo.SyntaxHighlighter;
    end;
  end;

  // change packages containing files
  Result:=PkgBoss.OnRenameFile(OldFilename,AnUnitInfo.Filename);

  Result:=mrOk;
end;

function TMainIDE.DoSaveFileResources(AnUnitInfo: TUnitInfo;
  ResourceCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;
var
  FormSavingOk: boolean;
  MemStream,BinCompStream,TxtCompStream:TMemoryStream;
  Driver: TAbstractObjectWriter;
  Writer:TWriter;
  ACaption, AText: string;
  CompResourceCode, LFMFilename, TestFilename, ResTestFilename: string;
begin
  Result:=mrCancel;
  
  // save lrs - lazarus resource file and lfm - lazarus form text file
  // Note: When there is a bug in the source, no resource code can be found,
  //       but the LFM file should always be saved
  if (AnUnitInfo.Form<>nil) then begin
    // stream component to resource code and to lfm file
    FormSavingOk:=true;

    // stream component to binary stream
    BinCompStream:=TMemoryStream.Create;
    try
      Result:=mrOk;
      repeat
        try
          BinCompStream.Position:=0;
          Driver:=TBinaryObjectWriter.Create(BinCompStream,4096);
          try
            Writer:=TWriter.Create(Driver);
            try
              Writer.WriteDescendent(AnUnitInfo.Form,nil);
            finally
              Writer.Free;
            end;
          finally
            Driver.Free;
          end;
        except
          ACaption:='Streaming error';
          AText:='Unable to stream '
              +AnUnitInfo.FormName+':T'+AnUnitInfo.FormName+'.';
          Result:=MessageDlg(ACaption, AText, mtError,
                     [mbAbort, mbRetry, mbIgnore], 0);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
          FormSavingOk:=false;
        end;
      until Result<>mrRetry;

      // create lazarus form resource code
      if FormSavingOk then begin
        if ResourceCode=nil then begin
          if (sfSaveToTestDir in Flags) then
            ResTestFilename:=ChangeFileExt(GetTestUnitFilename(AnUnitInfo),
                                           ResourceFileExt)
          else
            ResTestFilename:=ChangeFileExt(AnUnitInfo.Filename,
                                           ResourceFileExt);
          ResourceCode:=CodeToolBoss.CreateFile(ResTestFilename);
          FormSavingOk:=(ResourceCode<>nil);
        end;
        if FormSavingOk then begin
          // there is no bug in the source, so the resource code should be
          // changed too
          MemStream:=TMemoryStream.Create;
          try
            BinCompStream.Position:=0;
            BinaryToLazarusResourceCode(BinCompStream,MemStream
              ,'T'+AnUnitInfo.FormName,'FORMDATA');
            MemStream.Position:=0;
            SetLength(CompResourceCode,MemStream.Size);
            MemStream.Read(CompResourceCode[1],length(CompResourceCode));
          finally
            MemStream.Free;
          end;
        end;
        if FormSavingOk then begin
          {$IFDEF IDE_DEBUG}
          writeln('TMainIDE.SaveFileResources E ',CompResourceCode);
          {$ENDIF}
          // replace lazarus form resource code
          if not (sfSaveToTestDir in Flags) then begin
            if (AnUnitInfo.FormName<>AnUnitInfo.FormResourceName)
            and (AnUnitInfo.FormResourceName<>'') then begin
              CodeToolBoss.RemoveLazarusResource(ResourceCode,
                                               'T'+AnUnitInfo.FormResourceName);
            end;
            if (not CodeToolBoss.AddLazarusResourceHeaderComment(ResourceCode,
               lisResourceFileComment)) then
            begin
              ACaption:='Resource save error';
              AText:='Unable to add resource header comment'
                +' to resource file '#13
                +'"'+ResourceCode.FileName+'".'#13
                +'Probably a syntax error.';
              Result:=MessageDlg(ACaption,AText,mtError,[mbIgnore,mbAbort],0);
              if Result=mrAbort then exit;
            end;
            if (not CodeToolBoss.AddLazarusResource(ResourceCode,
               'T'+AnUnitInfo.FormName,CompResourceCode)) then
            begin
              ACaption:='Resource save error';
              AText:='Unable to add resource '
                +'T'+AnUnitInfo.FormName+':FORMDATA to resource file '#13
                +'"'+ResourceCode.FileName+'".'#13
                +'Probably a syntax error.';
              Result:=MessageDlg(ACaption, AText, mtError, [mbIgnore, mbAbort],0);
              if Result=mrAbort then exit;
            end else begin
              AnUnitInfo.ResourceFileName:=ResourceCode.Filename;
              AnUnitInfo.FormResourceName:=AnUnitInfo.FormName;
            end;
          end else begin
            ResourceCode.Source:=CompResourceCode;
          end;
        end;
        if (not (sfSaveToTestDir in Flags)) then begin
          // save lfm file
          LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
          if LFMCode=nil then begin
            LFMCode:=CodeToolBoss.CreateFile(LFMFilename);
            if LFMCode=nil then begin
              MessageDlg('Unable to create file',
                'Unable to create file "'+LFMFilename+'"',
                mtWarning,[mbIgnore,mbCancel],0);
            end;
          end;
          if LFMCode<>nil then begin
            {$IFDEF IDE_DEBUG}
            writeln('TMainIDE.SaveFileResources E2 LFM=',LFMCode.Filename);
            {$ENDIF}
            Result:=mrOk;
            repeat
              try
                // transform binary to text
                TxtCompStream:=TMemoryStream.Create;
                try
                  BinCompStream.Position:=0;
                  ObjectBinaryToText(BinCompStream,TxtCompStream);
                  TxtCompStream.Position:=0;
                  LFMCode.LoadFromStream(TxtCompStream);
                  Result:=DoSaveCodeBufferToFile(LFMCode,LFMCode.Filename,
                                   AnUnitInfo.IsPartOfProject);
                  if not Result=mrOk then exit;
                  Result:=mrCancel;
                finally
                  TxtCompStream.Free;
                end;
              except
                ACaption:='Streaming error';
                AText:='Unable to transform binary component stream of '
                   +AnUnitInfo.FormName+':T'+AnUnitInfo.FormName
                   +' into text.';
                Result:=MessageDlg(ACaption, AText, mtError,
                                   [mbAbort, mbRetry, mbIgnore], 0);
                if Result=mrAbort then exit;
                if Result=mrIgnore then Result:=mrOk;
              end;
            until Result<>mrRetry;
          end;
        end;
      end;
    finally
      BinCompStream.Free;
    end;
  end;
  {$IFDEF IDE_DEBUG}
  if ResourceCode<>nil then
    writeln('TMainIDE.SaveFileResources F ',ResourceCode.Modified);
  {$ENDIF}
  if ResourceCode<>nil then begin
    if not (sfSaveToTestDir in Flags) then begin
      if (ResourceCode.Modified) then begin
        Result:=DoSaveCodeBufferToFile(ResourceCode,ResourceCode.Filename,
            AnUnitInfo.IsPartOfProject);
        if not Result=mrOk then exit;
      end;
    end else begin
      TestFilename:=GetTestUnitFilename(AnUnitInfo);
      Result:=DoSaveCodeBufferToFile(ResourceCode,
                 ChangeFileExt(TestFilename,
                               ExtractFileExt(ResourceCode.Filename)),
                 false);
      if not Result=mrOk then exit;
    end;
  end;
  Result:=mrOk;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.SaveFileResources G ',LFMCode<>nil);
  {$ENDIF}
end;

function TMainIDE.DoOpenNotExistingFile(const AFileName: string;
  Flags: TOpenFlags): TModalResult;
begin
  if ofProjectLoading in Flags then begin
    // this is a file, that was loaded last time, but was removed from disk
    Result:=MessageDlg('File not found',
      'The file "'+AFilename+'"'#13
      +'was not found.'#13
      +'Ignore will go on loading the project,'#13
      +'Abort  will stop the loading.',
      mtError, [mbIgnore, mbAbort], 0);
    exit;
  end;

  // Default to cancel
  Result:=mrCancel;
  if ofQuiet in Flags then Exit;

  if ofOnlyIfExists in Flags 
  then begin
    MessageDlg('File not found','File "'+AFilename+'" not found.'#13,
               mtInformation,[mbCancel],0);
    // cancel loading file
    Exit;       
  end;

  if MessageDlg('File not found',
    'File "'+AFilename+'" not found.'#13
    +'Do you want to create it?'#13
    ,mtInformation,[mbYes,mbNo],0)=mrYes then
  begin
    // create new file
    if FilenameIsPascalSource(AFilename) then
      Result:=DoNewEditorFile(nuUnit,AFilename,'',
                              [nfOpenInEditor,nfCreateDefaultSrc])
    else
      Result:=DoNewEditorFile(nuEmpty,AFilename,'',
                              [nfOpenInEditor,nfCreateDefaultSrc]);
  end;
end;

function TMainIDE.DoOpenUnknownFile(const AFileName: string; Flags: TOpenFlags;
  var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
var
  Ext, NewProgramName, LPIFilename, ACaption, AText: string;
  PreReadBuf: TCodeBuffer;
begin
  Handled:=false;
  Ext:=lowercase(ExtractFileExt(AFilename));

  if ([ofProjectLoading,ofRegularFile]*Flags<>[]) and (ToolStatus=itNone)
  and (Ext='.lpi') then begin
    // this is a project info file -> load whole project
    Result:=DoOpenProjectFile(AFilename,[ofAddToRecent]);
    Handled:=true;
    exit;
  end;

  // load the source
  Result:=DoLoadCodeBuffer(PreReadBuf,AFileName,
                           [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert]);
  if Result<>mrOk then exit;
  NewUnitInfo:=nil;

  // check if unit is a program
  if ([ofProjectLoading,ofRegularFile]*Flags=[])
  and FilenameIsPascalSource(AFilename)
  and (CodeToolBoss.GetSourceType(PreReadBuf,false)='PROGRAM') then begin
    NewProgramName:=CodeToolBoss.GetSourceName(PreReadBuf,false);
    if NewProgramName<>'' then begin
      // source is a program
      // either this is a lazarus project
      // or it is not yet a lazarus project ;)
      LPIFilename:=ChangeFileExt(AFilename,'.lpi');
      if FileExists(LPIFilename) then begin
        AText:='The file "'+AFilename+'"'#13
            +'seems to be the program file of an existing lazarus Project1.'#13
            +'Open project?'#13
            +'Cancel will load the file as normal source.';
        ACaption:='Project info file detected';
        if MessageDlg(ACaption, AText, mtconfirmation,
             [mbok, mbcancel], 0)=mrOk then
        begin
          Result:=DoOpenProjectFile(LPIFilename,[]);
          Handled:=true;
          exit;
        end;
      end else begin
        AText:='The file "'+AFilename+'"'#13
            +'seems to be a program. Close current project'
            +' and create a new lazarus project for this program?'#13
            +'Cancel will load the file as normal source.';
        ACaption:='Program detected';
        if MessageDlg(ACaption, AText, mtConfirmation,
            [mbOk, mbCancel], 0)=mrOk then
        begin
          Result:=DoCreateProjectForProgram(PreReadBuf);
          Handled:=true;
          exit;
        end;
      end;
    end;
  end;
  NewUnitInfo:=TUnitInfo.Create(PreReadBuf);
  if FilenameIsPascalSource(NewUnitInfo.Filename) then
    NewUnitInfo.ReadUnitNameFromSource;
  Project1.AddUnit(NewUnitInfo,false);
  Result:=mrOk;
end;

procedure TMainIDE.DoRestoreBookMarks(AnUnitInfo: TUnitInfo;
  ASrcEdit: TSourceEditor);
var
  BookmarkID, i: integer;
begin
  Project1.MergeBookmarks(AnUnitInfo);
  for BookmarkID:=0 to 9 do begin
    i:=Project1.Bookmarks.IndexOfID(BookmarkID);
    if i<0 then continue;
    if (Project1.Bookmarks[i].EditorIndex=AnUnitInfo.EditorIndex) then begin
      ASrcEdit.EditorComponent.SetBookmark(BookmarkID,
         Project1.Bookmarks[i].CursorPos.X,Project1.Bookmarks[i].CursorPos.Y);
    end;
  end;
end;

function TMainIDE.DoLoadLFM(AnUnitInfo: TUnitInfo;
  Flags: TOpenFlags): TModalResult;
var
  LFMFilename, ACaption, AText: string;
  LFMBuf: TCodeBuffer;
  FormLoadingOk: boolean;
  TxtLFMStream, BinLFMStream:TMemoryStream;
  CInterface: TComponentInterface;
  TempForm: TCustomForm;
begin
  CloseDesignerForm(AnUnitInfo);

  LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
  LFMBuf:=nil;
  if FileExists(LFMFilename) then begin
    Result:=DoLoadCodeBuffer(LFMBuf,LFMFilename,[lbfUpdateFromDisk]);
    if Result<>mrOk then exit;
  end;
  FormLoadingOk:=(LFMBuf<>nil);

  if FormLoadingOk then begin
    // there is a lazarus form text file -> load it
    BinLFMStream:=TMemoryStream.Create;
    try
      TxtLFMStream:=TMemoryStream.Create;
      try
        LFMBuf.SaveToStream(TxtLFMStream);
        TxtLFMStream.Position:=0;
        // convert text to binary format
        try
          ObjectTextToBinary(TxtLFMStream,BinLFMStream);
          BinLFMStream.Position:=0;
          Result:=mrOk;
        except
          on E: Exception do begin
            ACaption:='Format error';
            AText:='Unable to convert text form data of file '#13
              +'"'+LFMBuf.Filename+'"'#13
              +'into binary stream. ('+E.Message+')';
            Result:=MessageDlg(ACaption, AText, mtError, [mbOk, mbCancel], 0);
            if Result=mrCancel then Result:=mrAbort;
            if Result<>mrOk then exit;
            FormLoadingOk:=false;
          end;
        end;
      finally
        TxtLFMStream.Free;
      end;
      if FormLoadingOk then begin
        if not Assigned(FormEditor1) then
          FormEditor1 := TFormEditor.Create;
        if not (ofProjectLoading in Flags) then FormEditor1.ClearSelected;

        // create jitform
        CInterface := TComponentInterface(
          FormEditor1.CreateFormFromStream(BinLFMStream));
        if CInterface=nil then begin
          ACaption:='Form load error';
          AText:='Unable to build form from file '#13
                      +'"'+LFMBuf.Filename+'".';
          Result:=MessageDlg(ACaption, AText, mterror, [mbok, mbcancel], 0);
          if Result=mrCancel then Result:=mrAbort;
          if Result<>mrOk then exit;
          TempForm:=nil;
          AnUnitInfo.Form:=TempForm;
        end else begin
          TempForm:=TForm(CInterface.Component);
          AnUnitInfo.Form:=TempForm;
          SetDefaultsForForm(TempForm);
          AnUnitInfo.FormName:=TempForm.Name;
          AnUnitInfo.FormResourceName:=AnUnitInfo.FormName;
          // show form
          TDesigner(TempForm.Designer).SourceEditor:=
            SourceNoteBook.GetActiveSE;

          if not (ofProjectLoading in Flags) then begin
            TempForm.Hide; Show;
            FDisplayState:= dsForm;
          end;

          // select the new form (object inspector, formeditor, control selection)
          if not (ofProjectLoading in Flags) then begin
            PropertyEditorHook1.LookupRoot := TempForm;
            TDesigner(TempForm.Designer).SelectOnlyThisComponent(TempForm);
          end;
          FLastFormActivated:=TempForm;
        end;
      end;
      {$IFDEF IDE_DEBUG}
      writeln('[TMainIDE.DoLoadLFM] LFM end');
      {$ENDIF}
    finally
      BinLFMStream.Free;
    end;
  end;
  Result:=mrOk;
end;

{-------------------------------------------------------------------------------
  function TMainIDE.CloseDesignerForm
  
  Params: AnUnitInfo: TUnitInfo
  Result: TModalResult;
  
  Free the designer form of a unit.
-------------------------------------------------------------------------------}
function TMainIDE.CloseDesignerForm(AnUnitInfo: TUnitInfo): TModalResult;
var
  AForm: TCustomForm;
  i: integer;
  OldDesigner: TDesigner;
begin
  AForm:=TCustomForm(AnUnitInfo.Form);
  if AForm<>nil then begin
    if FLastFormActivated=AForm then
      FLastFormActivated:=nil;
    // unselect controls
    for i:=AForm.ComponentCount-1 downto 0 do
      TheControlSelection.Remove(
        AForm.Components[i]);
    TheControlSelection.Remove(AForm);
    // free designer and design form
    OldDesigner:=TDesigner(AForm.Designer);
    OldDesigner.DeleteFormAndFree;
    AnUnitInfo.Form:=nil;
  end;
  Result:=mrOk;
end;

procedure TMainIDE.GetMainUnit(var MainUnitInfo: TUnitInfo;
  var MainUnitSrcEdit: TSourceEditor; UpdateModified: boolean);
begin
  MainUnitSrcEdit:=nil;
  if Project1.MainUnitID>=0 then begin
    MainUnitInfo:=Project1.MainUnitInfo;
    if MainUnitInfo.Loaded then begin
      MainUnitSrcEdit:=SourceNoteBook.FindSourceEditorWithPageIndex(
        MainUnitInfo.EditorIndex);
      if UpdateModified and MainUnitSrcEdit.Modified then begin
        MainUnitSrcEdit.UpdateCodeBuffer;
        MainUnitInfo.Modified:=true;
      end;
    end;
  end else
    MainUnitInfo:=nil;
end;

procedure TMainIDE.SaveSrcEditorProjectSpecificSettings(AnUnitInfo: TUnitInfo);
var
  BookmarkID, BookmarkX, BookmarkY: integer;
  ASrcEdit: TSourceEditor;
begin
  Project1.Bookmarks.DeleteAllWithEditorIndex(AnUnitInfo.EditorIndex);
  ASrcEdit:=
    SourceNoteBook.FindSourceEditorWithPageIndex(AnUnitInfo.EditorIndex);
  if ASrcEdit=nil then exit;
  AnUnitInfo.TopLine:=ASrcEdit.EditorComponent.TopLine;
  AnUnitInfo.CursorPos:=ASrcEdit.EditorComponent.CaretXY;
  // bookmarks
  AnUnitInfo.Bookmarks.Clear;
  for BookmarkID:=0 to 9 do begin
    if (ASrcEdit.EditorComponent.GetBookMark(BookmarkID,BookmarkX,BookmarkY))
    then begin
      Project1.SetBookmark(AnUnitInfo,BookmarkX,BookmarkY,BookmarkID);
    end;
  end;
end;

procedure TMainIDE.SaveSourceEditorProjectSpecificSettings;
var
  AnUnitInfo: TUnitInfo;
begin
  Project1.Bookmarks.Clear;
  AnUnitInfo:=Project1.FirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    if (not AnUnitInfo.Loaded) then continue;
    SaveSrcEditorProjectSpecificSettings(AnUnitInfo);
    AnUnitInfo:=AnUnitInfo.NextUnitWithEditorIndex;
  end;
end;

function TMainIDE.DoShowSaveProjectAsDialog: TModalResult;
var
  MainUnitSrcEdit: TSourceEditor;
  MainUnitInfo: TUnitInfo;
  SaveDialog: TSaveDialog;
  NewFilename, NewProgramFilename, NewProgramName, AText, ACaption,
  Ext: string;
  NewBuf: TCodeBuffer;
  OldProjectPath: string;
begin
  OldProjectPath:=Project1.ProjectDirectory;
  
  SaveDialog:=TSaveDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:='Save Project '+Project1.Title+' (*.lpi)';
    
    // build a nice project info filename suggestion
    NewFilename:='';
    if (Project1.MainUnitID>=0) then
      NewFileName:=Project1.MainUnitInfo.UnitName;
    if NewFilename='' then
      NewFilename:=ExtractFileName(Project1.ProjectInfoFile);
    if NewFilename='' then
      NewFilename:=ExtractFileName(Project1.MainFilename);
    if NewFilename='' then
      NewFilename:=Trim(Project1.Title);
    if NewFilename='' then
      NewFilename:='project1';
    Ext:=lowercase(ExtractFileExt(NewFilename));
    if (Ext='') or FilenameIsPascalSource(NewFilename) then
      NewFilename:=ChangeFileExt(NewFilename,'.lpi');
    SaveDialog.FileName:=NewFilename;
    
    NewProgramName:='';     // the pascal program identifier
    NewProgramFilename:=''; // the program source filename
    repeat
      Result:=mrCancel;
      
      if not SaveDialog.Execute then begin
        // user cancels
        Result:=mrCancel;
        exit;
      end;
      NewFilename:=ExpandFilename(SaveDialog.Filename);
      NewProgramName:=ExtractFileNameOnly(NewFilename);

      // check programname
      if (NewProgramName='') or (not IsValidIdent(NewProgramName)) then begin
        Result:=MessageDlg('Invalid project filename',
          '"'+SaveDialog.Filename+'" is an invalid project name.'#13
          +'Please choose another (e.g. project1.lpi)',
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;
      
      // append default extension
      Ext:=ExtractFileExt(NewFilename);
      if Ext='' then begin
        NewFilename:=NewFilename+'.lpi';
        Ext:='.lpi';
      end;
      
      // check pascal identifier
      if FilenameIsPascalSource(NewFilename) then begin
        if not IsValidIdent(NewProgramName) then begin
          Result:=MessageDlg('Invalid Pascal Identifier',
            'The name "'+NewProgramName+'" is not a valid pascal identifier.'
            ,mtWarning,[mbIgnore,mbCancel],0);
          if Result=mrCancel then exit;
          Result:=mrCancel;
        end;
      end;
      
      // apply naming conventions
      NewProgramName:=ExtractFileNameOnly(NewFilename);
      if EnvironmentOptions.PascalFileAutoLowerCase then
        NewFileName:=ExtractFilePath(NewFilename)
                    +lowercase(ExtractFileName(NewFilename));

      if Project1.MainUnitID>=0 then begin
        // check mainunit filename
        Ext:=ExtractFileExt(Project1.MainUnitInfo.Filename);
        if Ext='' then Ext:=ProjectDefaultExt[Project1.ProjectType];
        NewProgramFilename:=ChangeFileExt(NewFilename,Ext);
        if CompareFilenames(NewFilename,NewProgramFilename)=0 then begin
          ACaption:='Choose a different name';
          AText:='The project info file "'+NewFilename+'"'#13
             +'is equal to the project main source file!';
          Result:=MessageDlg(ACaption, AText, mtError, [mbAbort,mbRetry],0);
          if Result=mrAbort then exit;
          continue; // try again
        end;
        // check programname
        if FilenameIsPascalUnit(NewProgramFilename)
        and (Project1.IndexOfUnitWithName(NewProgramName,true,
                                       Project1.MainUnitInfo)>=0) then
        begin
          ACaption:='Unit identifier exists';
          AText:='There is a unit with the name "'+NewProgramName+'"'
              +' in the project.'#13
              +'Plz choose a different name';
          Result:=MessageDlg(ACaption,AText,mtError,[mbRetry,mbAbort],0);
          if Result=mrAbort then exit;
          continue; // try again
        end;
        Result:=mrOk;
      end else begin
        NewProgramFilename:='';
        Result:=mrOk;
      end;
    until Result<>mrRetry;
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;

  // check if info file or source file already exists
  if FileExists(NewFilename) then begin
    ACaption:='Overwrite file?';
    AText:='A file "'+NewFilename+'" already exists.'#13'Replace it?';
    Result:=MessageDlg(ACaption, AText, mtConfirmation, [mbOk, mbCancel], 0);
    if Result=mrCancel then exit;
  end else if Project1.ProjectType in [ptProgram, ptApplication] then begin
    if FileExists(NewProgramFilename) then begin
      ACaption:='Overwrite file?';
      AText:='A file "'+NewProgramFilename+'" already exists.'#13
                      +'Replace it?';
      Result:=MessageDlg(ACaption, AText, mtConfirmation,[mbOk,mbCancel],0);
      if Result=mrCancel then exit;
    end;
  end;
  
  // set new project filename
  Project1.ProjectInfoFile:=NewFilename;
  EnvironmentOptions.AddToRecentProjectFiles(NewFilename);
  SetRecentProjectFilesMenu;

  // set new project directory
  if OldProjectPath<>Project1.ProjectDirectory then begin
    CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
      Project1.ProjectDirectory;
    CodeToolBoss.DefineTree.ClearCache;
  end;
  
  // change main source
  if (Project1.MainUnitID>=0) then begin
    GetMainUnit(MainUnitInfo,MainUnitSrcEdit,true);
    
    // switch MainUnitInfo.Source to new code
    NewBuf:=CodeToolBoss.CreateFile(NewProgramFilename);
    if NewBuf=nil then begin
      Result:=MessageDlg('Error creating file','Unable to create file'#13
           +'"'+NewProgramFilename+'"',mtError,[mbCancel],0);
      exit;
    end;
    
    // copy the source to the new buffer
    NewBuf.Source:=MainUnitInfo.Source.Source;
    
    // assign the new buffer to the MainUnit
    MainUnitInfo.Source:=NewBuf;
    if MainUnitSrcEdit<>nil then
      MainUnitSrcEdit.CodeBuffer:=NewBuf;

    // change program name
    MainUnitInfo.UnitName:=NewProgramName;
    MainUnitInfo.Modified:=true;

    // TODO: rename resource include directive

    // update source notebook page names
    UpdateSourceNames;
  end;
  
  // invalidate cached substituted macros
  IncreaseCompilerParseStamp;
  
  Result:=mrOk;
end;

function TMainIDE.DoCompleteLoadingProjectInfo: TModalResult;
begin
  UpdateCaption;
  EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
  EnvironmentOptions.Save(false);
  // load required packages
  PkgBoss.OpenProjectDependencies(Project1);

  Project1.DefineTemplates.AllChanged;
  Result:=mrOk;
end;

procedure TMainIDE.OnCopyFile(const Filename: string; var Copy: boolean;
  Data: TObject);
begin
  if Data=nil then exit;
  if Data is TPublishProjectOptions then begin
    Copy:=TPublishProjectOptions(Data).FileCanBePublished(Filename);
    //writeln('TMainIDE.OnCopyFile "',Filename,'" ',Copy);
  end;
end;

procedure TMainIDE.OnCopyError(const ErrorData: TCopyErrorData;
  var Handled: boolean; Data: TObject);
begin
  case ErrorData.Error of
    ceSrcDirDoesNotExists:
      MessageDlg('Copy error',
        'Source directory "'+ErrorData.Param1+'" does not exists.',
        mtError,[mbCancel],0);
    ceCreatingDirectory:
      MessageDlg('Copy error',
        'Unable to create directory "'+ErrorData.Param1+'".',
        mtError,[mbCancel],0);
    ceCopyFileError:
      MessageDlg('Copy error',
        'Unable to copy file "'+ErrorData.Param1+'"'#13
        +'to "'+ErrorData.Param1+'"',
        mtError,[mbCancel],0);
  end;
end;

function TMainIDE.DoOpenFileInSourceNotebook(AnUnitInfo: TUnitInfo;
  PageIndex: integer; Flags: TOpenFlags): TModalResult;
var NewSrcEdit: TSourceEditor;
  AFilename: string;
  NewSrcEditorCreated: boolean;
begin
  AFilename:=AnUnitInfo.Filename;

  // get syntax highlighter type
  if not AnUnitInfo.CustomHighlighter then
    AnUnitInfo.SyntaxHighlighter:=
      ExtensionToLazSyntaxHighlighter(ExtractFileExt(AFilename));
      
  NewSrcEditorCreated:=false;
  if (not (ofRevert in Flags)) or (PageIndex<0) then begin
    // create a new source editor

    // update marks and cursor positions in Project1, so that merging the old
    // settings during restoration will work
    SaveSourceEditorProjectSpecificSettings;
    SourceNotebook.NewFile(CreateSrcEditPageName(AnUnitInfo.UnitName,
      AFilename,-1),AnUnitInfo.Source,false);
    NewSrcEdit:=SourceNotebook.GetActiveSE;
    NewSrcEditorCreated:=true;
    itmFileClose.Enabled:=True;
    itmFileCloseAll.Enabled:=True;
  end else begin
    // revert code in existing source editor
    NewSrcEdit:=SourceNotebook.FindSourceEditorWithPageIndex(PageIndex);
    NewSrcEdit.CodeBuffer:=AnUnitInfo.Source;
    NewSrcEdit.Modified:=false;
    AnUnitInfo.Modified:=false;
  end;

  // update editor indices in project
  if (not (ofProjectLoading in Flags)) and NewSrcEditorCreated then
    Project1.InsertEditorIndex(SourceNotebook.NoteBook.PageIndex);
  AnUnitInfo.EditorIndex:=SourceNotebook.FindPageWithEditor(NewSrcEdit);

  // restore source editor settings
  DoRestoreBookMarks(AnUnitInfo,NewSrcEdit);
  NewSrcEdit.SyntaxHighlighterType:=AnUnitInfo.SyntaxHighlighter;
  NewSrcEdit.EditorComponent.CaretXY:=AnUnitInfo.CursorPos;
  NewSrcEdit.EditorComponent.TopLine:=AnUnitInfo.TopLine;
  NewSrcEdit.EditorComponent.LeftChar:=1;
  NewSrcEdit.ReadOnly:=AnUnitInfo.ReadOnly;

  // mark unit as loaded
  AnUnitInfo.Loaded:=true;
  
  // update statusbar and focus editor
  if (not (ofProjectLoading in Flags)) then
    SourceNotebook.FocusEditor;
  SourceNoteBook.UpdateStatusBar;
    
  Result:=mrOk;
end;
  
function TMainIDE.DoNewEditorFile(NewUnitType:TNewUnitType;
  NewFilename: string; const NewSource: string;
  NewFlags: TNewFlags): TModalResult;

  function BeautifySrc(const s: string): string;
  begin
    Result:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                  BeautifyStatement(s,0);
  end;

var NewUnitInfo:TUnitInfo;
  NewSrcEdit: TSourceEditor;
  NewUnitName: string;
  NewBuffer: TCodeBuffer;
  OldUnitIndex: Integer;
begin
  writeln('TMainIDE.DoNewEditorFile A NewFilename=',NewFilename);
  SaveSourceEditorChangesToCodeCache(-1);

  // convert macros in filename
  if nfConvertMacros in NewFlags then begin
    if not MacroList.SubstituteStr(NewFilename) then begin
      Result:=mrCancel;
      exit;
    end;
  end;
  
  // create new codebuffer and apply naming conventions
  Result:=CreateNewCodeBuffer(NewUnitType,NewFilename,NewBuffer,NewUnitName);
  if Result<>mrOk then exit;

  NewFilename:=NewBuffer.Filename;
  OldUnitIndex:=Project1.IndexOfFilename(NewFilename);
  if OldUnitIndex>=0 then begin
    // the file is not really new
    NewUnitInfo:=Project1.Units[OldUnitIndex];
    // close form
    CloseDesignerForm(NewUnitInfo);
    // assign source
    NewUnitInfo.Source:=NewBuffer;
  end else
    NewUnitInfo:=TUnitInfo.Create(NewBuffer);

  // create source code
  if nfCreateDefaultSrc in NewFlags then begin
    if NewUnitType in [nuForm] then begin
      NewUnitInfo.FormName:=Project1.NewUniqueFormName(NewUnitType);
      NewUnitInfo.FormResourceName:='';
      CodeToolBoss.CreateFile(ChangeFileExt(NewFilename,ResourceFileExt));
    end;
    NewUnitInfo.CreateStartCode(NewUnitType,NewUnitName);
  end else begin
    if nfBeautifySrc in NewFlags then
      NewBuffer.Source:=BeautifySrc(NewSource)
    else
      NewBuffer.Source:=NewSource;
    NewUnitInfo.Modified:=true;
  end;
  
  // add to project
  with NewUnitInfo do begin
    Loaded:=true;
    IsPartOfProject:=(nfIsPartOfProject in NewFlags)
                     or (Project1.FileIsInProjectDir(NewFilename)
                         and (not (nfIsNotPartOfProject in NewFlags)));
  end;
  if OldUnitIndex<0 then begin
    Project1.AddUnit(NewUnitInfo,(NewUnitType in [nuForm, nuUnit])
                                 and NewUnitInfo.IsPartOfProject);
  end;
                              
  // syntax highlighter type
  if NewUnitType in [nuForm, nuUnit] then begin
    NewUnitInfo.SyntaxHighlighter:=lshFreePascal;
  end else begin
    NewUnitInfo.SyntaxHighlighter:=
      ExtensionToLazSyntaxHighlighter(ExtractFileExt(NewFilename))
  end;

  if nfOpenInEditor in NewFlags then begin
    // open a new sourceeditor
    SourceNotebook.NewFile(CreateSrcEditPageName(NewUnitInfo.UnitName,
                                                 NewUnitInfo.Filename,-1),
                           NewUnitInfo.Source,true);
    itmFileClose.Enabled:=True;
    itmFileCloseAll.Enabled:=True;
    NewSrcEdit:=SourceNotebook.GetActiveSE;
    NewSrcEdit.SyntaxHighlighterType:=NewUnitInfo.SyntaxHighlighter;
    Project1.InsertEditorIndex(SourceNotebook.NoteBook.PageIndex);
    NewUnitInfo.EditorIndex:=SourceNotebook.NoteBook.PageIndex;

    // create form
    if NewUnitType in [nuForm] then begin
      Result:=CreateNewForm(NewUnitInfo);
      if Result<>mrOk then exit;
    end;

    // show form and select form
    if NewUnitType in [nuForm] then begin
      // show form
      TDesigner(TCustomForm(NewUnitInfo.Form).Designer).SourceEditor :=
        SourceNoteBook.GetActiveSE;
      ShowDesignForm(TCustomForm(NewUnitInfo.Form));
    end else begin
      FDisplayState:= dsSource;
    end;

    if nfSave in NewFlags then begin
      NewUnitInfo.Modified:=true;
      Result:=DoSaveEditorFile(NewUnitInfo.EditorIndex,[sfCheckAmbigiousFiles]);
      if Result<>mrOk then exit;
    end;
  end else begin
    // do not open in editor
    
    if nfSave in NewFlags then begin
      NewBuffer.Save;
    end;
  end;
  
  Result:=mrOk;
  writeln('TMainIDE.DoNewEditorFile end ',NewUnitInfo.Filename);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoNewUnit end');{$ENDIF}
end;

function TMainIDE.DoNewOther: TModalResult;
var
  NewIDEItem: TNewIDEItem;
begin
  Result:=ShowNewIDEItemDialog(NewIDEItem);
  try
    if Result<>mrOk then exit;
    case NewIDEItem.TheType of
    // files
    niiText: Result:=DoNewEditorFile(nuText,'','',
                                     [nfOpenInEditor,nfCreateDefaultSrc]);
    niiUnit: Result:=DoNewEditorFile(nuUnit,'','',
                                     [nfOpenInEditor,nfCreateDefaultSrc]);
    niiForm: Result:=DoNewEditorFile(nuForm,'','',
                                     [nfOpenInEditor,nfCreateDefaultSrc]);
    // projects
    niiApplication: DoNewProject(ptApplication);
    niiFPCProject: DoNewProject(ptProgram);
    niiCustomProject: DoNewProject(ptCustomProgram);
    // packages
    niiPackage: PkgBoss.DoNewPackage;
    else
      MessageDlg('Not implemented yet',
                 'Sorry, this type is not yet implemented',
        mtInformation,[mbOk],0);
    end;
  finally
    NewIDEItem.Free;
  end;
end;

function TMainIDE.DoSaveEditorFile(PageIndex:integer;
  Flags: TSaveFlags):TModalResult;
var ActiveSrcEdit:TSourceEditor;
  ActiveUnitInfo:TUnitInfo;
  TestFilename, DestFilename: string;
  ResourceCode, LFMCode: TCodeBuffer;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoSaveEditorFile A PageIndex=',PageIndex,' Flags=',SaveFlagsToString(Flags));
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoSaveEditorFile A');{$ENDIF}
  Result:=mrCancel;
  if not (ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  GetUnitWithPageIndex(PageIndex,ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo=nil then exit;

  // check if file is writable on disk
  if (not ActiveUnitInfo.IsVirtual)
  and FileExists(ActiveUnitInfo.Filename) then
    ActiveUnitInfo.FileReadOnly:=not FileIsWritable(ActiveUnitInfo.Filename)
  else
    ActiveUnitInfo.FileReadOnly:=false;
  
  // if this file is part of the project and the project is virtual then save
  // project first
  if (not (sfProjectSaving in Flags)) and Project1.IsVirtual
  and ActiveUnitInfo.IsPartOfProject then
  begin
    Result:=DoSaveProject(Flags*[sfSaveToTestDir]);
    exit;
  end;

  // update codetools cache and collect Modified flags
  if not (sfProjectSaving in Flags) then
    SaveSourceEditorChangesToCodeCache(-1);

  // if this is a new unit then a simple Save becomes a SaveAs
  if (not (sfSaveToTestDir in Flags)) and (ActiveUnitInfo.IsVirtual) then
    Include(Flags,sfSaveAs);

  // update source notebook page names
  if (not (sfProjectSaving in Flags)) then
    UpdateSourceNames;

  // if file is readonly then a simple Save is skipped
  if (ActiveUnitInfo.ReadOnly) and ([sfSaveToTestDir,sfSaveAs]*Flags=[]) then
  begin
    Result:=mrOk;
    exit;
  end;

  // if nothing modified then a simple Save can be skipped
  if ([sfSaveToTestDir,sfSaveAs]*Flags=[])
  and (not ActiveUnitInfo.NeedsSaveToDisk) then begin
    Result:=mrOk;
    exit;
  end;
  
  // load resource file
  Result:=DoLoadResourceFile(ActiveUnitInfo,LFMCode,ResourceCode,
                             not (sfSaveAs in Flags));
  if Result in [mrIgnore, mrOk] then
    Result:=mrCancel
  else
    exit;

  if [sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs] then begin
    // let user choose a filename
    Result:=DoShowSaveFileAsDialog(ActiveUnitInfo,ResourceCode);
    if Result in [mrIgnore, mrOk] then
      Result:=mrCancel
    else
      exit;
    LFMCode:=nil;
  end;

  // save source
  if not (sfSaveToTestDir in Flags) then begin
    if ActiveUnitInfo.Modified then begin
      // save source to file
      Result:=ActiveUnitInfo.WriteUnitSource;
      if Result=mrAbort then exit;
      DestFilename:=ActiveUnitInfo.Filename;
    end;
  end else begin
    // save source to test directory
    TestFilename:=GetTestUnitFilename(ActiveUnitInfo);
    if TestFilename<>'' then begin
      Result:=ActiveUnitInfo.WriteUnitSourceToFile(TestFilename);
      if Result<>mrOk then exit;
      DestFilename:=TestFilename;
    end else
      exit;
  end;

  if sfCheckAmbigiousFiles in Flags then
    DoCheckAmbigiousSources(DestFilename,false);

  {$IFDEF IDE_DEBUG}
  writeln('*** HasResources=',ActiveUnitInfo.HasResources);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoSaveEditorFile B');{$ENDIF}
  // save resource file and lfm file
  if (ResourceCode<>nil) or (ActiveUnitInfo.Form<>nil) then begin
    Result:=DoSaveFileResources(ActiveUnitInfo,ResourceCode,LFMCode,Flags);
    if Result in [mrIgnore, mrOk] then
      Result:=mrCancel
    else
      exit;
  end;
  
  // unset all modified flags
  if not (sfSaveToTestDir in Flags) then begin
    ActiveUnitInfo.Modified:=false;
    ActiveSrcEdit.Modified:=false;
  end;
  SourceNoteBook.UpdateStatusBar;

  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoSaveEditorFile END');
  {$ENDIF}
  Result:=mrOk;
end;

function TMainIDE.DoCloseEditorFile(PageIndex:integer;
  Flags: TCloseFlags):TModalResult;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  ACaption,AText: string;
  i:integer;
begin
  writeln('TMainIDE.DoCloseEditorFile A PageIndex=',PageIndex);
  Result:=mrCancel;
  GetUnitWithPageIndex(PageIndex,ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo=nil then exit;
  if ActiveUnitInfo.Form=FLastFormActivated then
    FLastFormActivated:=nil;

  // save some meta data of the source
  SaveSrcEditorProjectSpecificSettings(ActiveUnitInfo);

  // if SaveFirst then save the source
  if (cfSaveFirst in Flags) and (not ActiveUnitInfo.ReadOnly)
  and ((ActiveSrcEdit.Modified) or (ActiveUnitInfo.Modified)) then begin
    if ActiveUnitInfo.Filename<>'' then
      AText:='File "'+ActiveUnitInfo.Filename+'" has changed. Save?'
    else if ActiveUnitInfo.UnitName<>'' then
      AText:='Unit "'+ActiveUnitInfo.Unitname+'" has changed. Save?'
    else
      AText:='Source of page "'+ActiveSrcEdit.PageName+'" has changed. Save?';
    ACaption:='Source modified';
    Result:=Messagedlg(ACaption, AText,
                       mtConfirmation, [mbYes, mbNo, mbAbort], 0);
    if Result=mrYes then begin
      Result:=DoSaveEditorFile(PageIndex,[sfCheckAmbigiousFiles]);
    end;
    if Result=mrAbort then exit;
    Result:=mrOk;
  end;
  
  // close form
  CloseDesignerForm(ActiveUnitInfo);

  // close source editor
  SourceNoteBook.CloseFile(PageIndex);
  itmFileClose.Enabled:=SourceNoteBook.NoteBook<>nil;
  itmFileCloseAll.Enabled:=itmFileClose.Enabled;

  // close file in project
  Project1.CloseEditorIndex(ActiveUnitInfo.EditorIndex);
  ActiveUnitInfo.Loaded:=false;
  i:=Project1.IndexOf(ActiveUnitInfo);
  if (i<>Project1.MainUnitID) and (ActiveUnitInfo.IsVirtual) then begin
    Project1.RemoveUnit(i);
  end;
  
  writeln('TMainIDE.DoCloseEditorFile end');
  Result:=mrOk;
end;

function TMainIDE.DoOpenEditorFile(AFileName:string;
  PageIndex: integer; Flags: TOpenFlags):TModalResult;
var
  UnitIndex: integer;
  ReOpen, Handled:boolean;
  NewUnitInfo:TUnitInfo;
  NewBuf: TCodeBuffer;
  OtherUnitIndex: Integer;
  FilenameNoPath: String;
  LoadBufferFlags: TLoadBufferFlags;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('');
  writeln('*** TMainIDE.DoOpenEditorFile START "',AFilename,'" ',OpenFlagsToString(Flags));
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenEditorFile START');{$ENDIF}
  Result:=mrCancel;

  // replace macros
  if ofConvertMacros in Flags then begin
    if not MacroList.SubstituteStr(AFilename) then exit;
    AFilename:=ExpandFilename(AFilename);
  end;

  // normalize filename
  AFilename:=TrimFilename(AFilename);
  FilenameNoPath:=ExtractFilename(AFilename);
  
  // check to not open directories
  if (not (ofRevert in Flags))
  and ((FilenameNoPath='') or (FilenameNoPath='.') or (FilenameNoPath='..'))
  then
    exit;

  if ([ofAddToRecent,ofRevert,ofVirtualFile]*Flags=[ofAddToRecent])
  and (AFilename<>'') and FilenameIsAbsolute(AFilename) then
    EnvironmentOptions.AddToRecentOpenFiles(AFilename);

  // check if this is a hidden unit:
  // if this is the main unit, it is already
  // loaded and needs only to be shown in the sourceeditor/formeditor
  if (not (ofRevert in Flags))
  and (CompareFilenames(Project1.MainFilename,AFilename,true)=0)
  then begin
    Result:=DoOpenMainUnit(ofProjectLoading in Flags);
    exit;
  end;
  
  // check for special files
  if ([ofRegularFile,ofRevert,ofProjectLoading]*Flags=[])
  and FilenameIsAbsolute(AFilename) and FileExists(AFilename) then begin
    // check for project information files (.lpi)
    if (CompareFileExt(AFilename,'.lpi',false)=0) then begin
      if MessageDlg('Open Project?',
        'Open the project '+AFilename+'?'#13
        +'Answer No to load it as xml file.',
        mtConfirmation,[mbYes,mbNo],0)=mrYes
      then begin
        Result:=DoOpenProjectFile(AFilename,[ofAddToRecent]);
        exit;
      end;
    end;
    {$IFDEF EnablePkgs}
    if (CompareFileExt(AFilename,'.lpk',false)=0) then begin
      if MessageDlg('Open Package?',
        'Open the package '+AFilename+'?'#13
        +'Answer No to load it as xml file.',
        mtConfirmation,[mbYes,mbNo],0)=mrYes
      then begin
        Result:=PkgBoss.DoOpenPackageFile(AFilename,[pofAddToRecent]);
        exit;
      end;
    end;
    {$ENDIF}
  end;
  
  // check if the project knows this file
  if (not (ofRevert in Flags)) then begin
    UnitIndex:=Project1.IndexOfFilename(AFilename);
    ReOpen:=(UnitIndex>=0);
    // check if there is already a symlinked file open in the editor
    OtherUnitIndex:=Project1.IndexOfFilename(AFilename,
                                  [pfsfOnlyEditorFiles,pfsfResolveFileLinks]);
    if (OtherUnitIndex>=0) and (OtherUnitIndex<>UnitIndex) then begin
      // There is another file open in the editor symlinked to the same file
      // ToDo
    end;
    if ReOpen then begin
      NewUnitInfo:=Project1.Units[UnitIndex];
      if (not (ofProjectLoading in Flags)) and NewUnitInfo.Loaded then begin
        // file already open -> change source notebook page
        SourceNoteBook.NoteBook.PageIndex:=NewUnitInfo.EditorIndex;
        Result:=mrOk;
        exit;
      end;
    end;
  end else begin
    // revert
    NewUnitInfo:=Project1.UnitWithEditorIndex(PageIndex);
    UnitIndex:=Project1.IndexOf(NewUnitInfo);
    AFilename:=NewUnitInfo.Filename;
    if NewUnitInfo.IsVirtual then begin
      if (not (ofQuiet in Flags)) then begin
        MessageDlg('Revert failed','File "'+AFilename+'" is virtual.',
          mtInformation,[mbCancel],0);
      end;
      Result:=mrCancel;
      exit;
    end;
    ReOpen:=true;
  end;
  
  // check if file exists
  if FilenameIsAbsolute(AFilename) and (not FileExists(AFilename)) then begin
    // file does not exists
    if (ofRevert in Flags) then begin
      // revert failed, due to missing file
      if not (ofQuiet in Flags) then begin
        MessageDlg('Revert failed','File "'+AFilename+'" not found.',
          mtError,[mbCancel],0);
      end;
      Result:=mrCancel;
      exit;
    end else begin
      Result:=DoOpenNotExistingFile(AFilename,Flags);
      exit;
    end;
  end;
  
  // load the source
  if ReOpen then begin
    // project knows this file => all the meta data is known
    // -> just load the source
    NewUnitInfo:=Project1.Units[UnitIndex];
    LoadBufferFlags:=[lbfCheckIfText];
    if FilenameIsAbsolute(AFilename) then begin
      if (not (ofUseCache in Flags)) then
        Include(LoadBufferFlags,lbfUpdateFromDisk);
      if ofRevert in Flags then
        Include(LoadBufferFlags,lbfRevert);
    end;
    Result:=DoLoadCodeBuffer(NewBuf,AFileName,LoadBufferFlags);
    if Result<>mrOk then exit;
    NewUnitInfo.Source:=NewBuf;
    NewUnitInfo.Modified:=NewUnitInfo.Source.FileOnDiskNeedsUpdate;
    if FilenameIsPascalUnit(NewUnitInfo.Filename) then
      NewUnitInfo.ReadUnitNameFromSource;
  end else begin
    // open unknown file
    Handled:=false;
    Result:=DoOpenUnknownFile(AFilename,Flags,NewUnitInfo,Handled);
    if Result<>mrOk then exit;
    if Handled then exit;
  end;

  // check readonly
  NewUnitInfo.FileReadOnly:=FileExists(NewUnitInfo.Filename)
                            and (not FileIsWritable(NewUnitInfo.Filename));
  
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.DoOpenEditorFile] B');
  {$ENDIF}
  // open file in source notebook
  Result:=DoOpenFileInSourceNoteBook(NewUnitInfo,PageIndex,Flags);
  if Result<>mrOk then exit;

  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.DoOpenEditorFile] C');
  {$ENDIF}

  // read form data
  if FilenameIsPascalUnit(AFilename) then begin
    // this could be a unit with a form
    if EnvironmentOptions.AutoCreateFormsOnOpen
    or (NewUnitInfo.Form<>nil) then begin
      // -> try to (re)load the lfm file
      Result:=DoLoadLFM(NewUnitInfo,Flags);
      if Result<>mrOk then exit;
    end;
  end else if NewUnitInfo.Form<>nil then begin
    // this is no pascal source and there is a designer form
    // This can be the case, when the file is renamed and reverted
    // -> close form
    CloseDesignerForm(NewUnitInfo);
  end;

  Result:=mrOk;
  writeln('TMainIDE.DoOpenEditorFile END "',AFilename,'"');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenEditorFile END');{$ENDIF}
end;

function TMainIDE.DoOpenMainUnit(ProjectLoading: boolean): TModalResult;
var MainUnitInfo: TUnitInfo;
  OpenFlags: TOpenFlags;
begin
  writeln('[TMainIDE.DoOpenMainUnit] A');
  Result:=mrCancel;
  if Project1.MainUnitID<0 then exit;
  MainUnitInfo:=Project1.MainUnitInfo;
  
  // check if main unit is already open in source editor
  if MainUnitInfo.Loaded and (not ProjectLoading) then begin
    // already loaded -> switch to source editor
    SourceNotebook.NoteBook.PageIndex:=MainUnitInfo.EditorIndex;
    Result:=mrOk;
    exit;
  end;

  // open file in source notebook
  OpenFlags:=[];
  if ProjectLoading then Include(OpenFlags,ofProjectLoading);
  Result:=DoOpenFileInSourceNoteBook(MainUnitInfo,-1,OpenFlags);
  if Result<>mrOk then exit;

  // build a nice pagename for the sourcenotebook
  Result:=mrOk;
  {$IFDEF IDE_VERBOSE}
  writeln('[TMainIDE.DoOpenMainUnit] END');
  {$ENDIF}
end;

function TMainIDE.DoRevertMainUnit: TModalResult;
begin
  Result:=mrOk;
  if Project1.MainUnitID<0 then exit;
  if Project1.MainUnitInfo.EditorIndex>=0 then
    // main unit is loaded, so we can just revert
    Result:=DoOpenEditorFile('',Project1.MainUnitInfo.EditorIndex,[ofRevert])
  else begin
    // main unit is only loaded in background
    // -> just reload the source and update the source name
    Result:=Project1.MainUnitInfo.ReadUnitSource(true,true);
  end;
end;

function TMainIDE.DoViewUnitsAndForms(OnlyForms: boolean): TModalResult;
var UnitList: TList;
  i: integer;
  MainUnitName, DlgCaption: string;
  MainUnitInfo, AnUnitInfo: TUnitInfo;
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  AForm: TForm;
Begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  UnitList:=TList.Create;
  try
    for i:=0 to Project1.UnitCount-1 do begin
      if not Project1.Units[i].IsPartOfProject then continue;
      if OnlyForms then begin
        // add all form names of project
        if Project1.Units[i].FormName<>'' then begin
          UnitList.Add(TViewUnitsEntry.Create(
            Project1.Units[i].FormName,i,Project1.Units[i]=ActiveUnitInfo));
        end;
      end else begin
        // add all unit names of project
        if (Project1.Units[i].UnitName<>'') then begin
          UnitList.Add(TViewUnitsEntry.Create(
            Project1.Units[i].UnitName,i,Project1.Units[i]=ActiveUnitInfo));
        end else if Project1.MainUnitID=i then begin
          MainUnitInfo:=Project1.MainUnitInfo;
          if Project1.ProjectType in [ptProgram,ptApplication,ptCustomProgram]
          then begin
            MainUnitName:=CreateSrcEditPageName(MainUnitInfo.UnitName,
              MainUnitInfo.Filename,MainUnitInfo.EditorIndex);
            if MainUnitName<>'' then begin
              UnitList.Add(TViewUnitsEntry.Create(
                MainUnitName,i,MainUnitInfo=ActiveUnitInfo));
            end;
          end;
        end;
      end;
    end;
    if OnlyForms then
      DlgCaption:=dlgMainViewForms 
    else
      DlgCaption:=dlgMainViewUnits ;
    if ShowViewUnitsDlg(UnitList,true,DlgCaption)=mrOk then begin
      AnUnitInfo:=nil;
      for i:=0 to UnitList.Count-1 do begin
        if TViewUnitsEntry(UnitList[i]).Selected then begin
          AnUnitInfo:=Project1.Units[TViewUnitsEntry(UnitList[i]).ID];
          if AnUnitInfo.EditorIndex>=0 then begin
            SourceNoteBook.NoteBook.PageIndex:=AnUnitInfo.EditorIndex;
          end else begin
            if Project1.MainUnitInfo=AnUnitInfo then
              Result:=DoOpenMainUnit(false)
            else
              Result:=DoOpenEditorFile(AnUnitInfo.Filename,-1,[ofOnlyIfExists]);
            if Result=mrAbort then exit;
          end;
        end;
      end;
      if (AnUnitInfo<>nil) then begin
        AForm:=SourceNotebook;
        if OnlyForms and (AnUnitInfo.Form<>nil) then begin
          AForm:=TForm(AnUnitInfo.Form);
        end;
        AForm.ShowOnTop;
      end;
    end;
  finally
    UnitList.Free;
  end;
  Result:=mrOk;
end;

procedure TMainIDE.DoViewUnitDependencies;
var
  WasVisible: boolean;
  ALayout: TIDEWindowLayout;
begin
  if UnitDependenciesView=nil then begin
    UnitDependenciesView:=TUnitDependenciesView.Create(Self);
    UnitDependenciesView.OnAccessingSources:=
      @UnitDependenciesViewAccessingSources;
    UnitDependenciesView.OnGetProjectMainFilename:=
      @UnitDependenciesViewGetProjectMainFilename;
    UnitDependenciesView.OnOpenFile:=@UnitDependenciesViewOpenFile;
    WasVisible:=false;
  end else
    WasVisible:=UnitDependenciesView.Visible;

  if not UnitDependenciesView.RootValid then begin
    if Project1.MainUnitID>=0 then begin
      UnitDependenciesView.RootFilename:=Project1.MainUnitInfo.Filename;
      UnitDependenciesView.RootShortFilename:=
        ExtractFilename(Project1.MainUnitInfo.Filename);
    end;
  end;

  UnitDependenciesView.Show;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByEnum(nmiwUnitDependenciesName);
  ALayout.Apply;
  if not WasVisible then
    UnitDependenciesView.ShowOnTop;
end;

function TMainIDE.DoSaveStringToFile(const Filename, Src,
  FileDescription: string): TModalResult;
var
  fs: TFileStream;
begin
  try
    ClearFile(Filename,true);
    fs:=TFileStream.Create(Filename,fmCreate);
    try
      if Src<>'' then
        fs.Write(Src[1],length(Src));
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      Result:=MessageDlg('Error writing file',
        'Unable to write '+FileDescription+#13
        +'"'+Filename+'".',
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoOpenFileAtCursor(Sender: TObject):TModalResult;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  FName,SPath: String;

  function FindFile(var FName: String; SPath: String): Boolean;
  //  Searches for FName in SPath
  //  If FName is not found, we'll check extensions pp and pas too
  //  Returns true if found. FName contains the full file+path in that case
  var TempFile,TempPath,CurPath,FinalFile, Ext: String;
      i,p,c: Integer;
  begin
    if SPath='' then SPath:='.';
    Result:=true;
    TempPath:=SPath;
    while TempPath<>'' do begin
      p:=pos(';',TempPath);
      if p=0 then p:=length(TempPath)+1;
      CurPath:=copy(TempPath,1,p-1);
      Delete(TempPath,1,p);
      if CurPath='' then continue;
      CurPath:=AppendPathDelim(CurPath);
      if not FilenameIsAbsolute(CurPath) then begin
        if ActiveUnitInfo.IsVirtual then
          CurPath:=AppendPathDelim(Project1.ProjectDirectory)+CurPath
        else
          CurPath:=AppendPathDelim(ExtractFilePath(ActiveUnitInfo.Filename))
                   +CurPath;
      end;
      for c:=0 to 2 do begin
        case c of
          0: TempFile:=FName;
          1: TempFile:=LowerCase(FName);
          2: TempFile:=UpperCase(FName);
        end;
        if ExtractFileExt(TempFile)='' then begin
          for i:=0 to 2 do begin
            case i of
              1: Ext:='.pp';
              2: Ext:='.pas';
              else Ext:='';
            end;
            FinalFile:=ExpandFileName(CurPath+TempFile+Ext);
            if FileExists(FinalFile) then begin
              FName:=FinalFile;
              exit;
            end;
          end;
        end else begin
          FinalFile:=ExpandFileName(CurPath+TempFile);
          if FileExists(FinalFile) then begin
            FName:=FinalFile;
            exit;
          end;
        end;
      end;
    end;
    Result:=false;
  end;
  
  function CheckIfIncludeDirectiveInFront(const Line: string;
    X: integer): boolean;
  var
    DirectiveEnd, DirectiveStart: integer;
    Directive: string;
  begin
    Result:=false;
    DirectiveEnd:=X;
    while (DirectiveEnd>1) and (Line[DirectiveEnd-1] in [' ',#9]) do
      dec(DirectiveEnd);
    DirectiveStart:=DirectiveEnd-1;
    while (DirectiveStart>0) and (Line[DirectiveStart]<>'$') do
      dec(DirectiveStart);
    Directive:=uppercase(copy(Line,DirectiveStart,DirectiveEnd-DirectiveStart));
    if (Directive='$INCLUDE') or (Directive='$I') then begin
      if ((DirectiveStart>1) and (Line[DirectiveStart-1]='{'))
      or ((DirectiveStart>2)
        and (Line[DirectiveStart-2]='(') and (Line[DirectiveStart-1]='*'))
      then begin
        Result:=true;
      end;
    end;
  end;

  function GetFilenameAtRowCol(XY: TPoint;
    var IsIncludeDirective: boolean): string;
  var
    Line: string;
    Len, Stop: integer;
    StopChars: set of char;
  begin
    Result := '';
    IsIncludeDirective:=false;
    if (XY.Y >= 1) and (XY.Y <= ActiveSrcEdit.EditorComponent.Lines.Count) then 
    begin
      Line := ActiveSrcEdit.EditorComponent.Lines.Strings[XY.Y - 1];
      Len := Length(Line);
      if (XY.X >= 1) and (XY.X <= Len + 1) then begin
        StopChars := [',',';',':','[',']','{','}','(',')',' ','''','"','`'
                     ,'#','%','=','>'];
        Stop := XY.X;
        while (Stop <= Len) and (not (Line[Stop] in StopChars)) do
          Inc(Stop);
        while (XY.X > 1) and (not (Line[XY.X - 1] in StopChars)) do
          Dec(XY.X);
        if Stop > XY.X then begin
          Result := Copy(Line, XY.X, Stop - XY.X);
          IsIncludeDirective:=CheckIfIncludeDirectiveInFront(Line,XY.X);
        end;
      end;
    end;
  end;

var IsIncludeDirective: boolean;
begin
  Result:=mrCancel;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  
  // parse filename at cursor
  FName:=GetFilenameAtRowCol(ActiveSrcEdit.EditorComponent.CaretXY,
                             IsIncludeDirective);
  if FName='' then exit;
  
  // get searchpath for directory of current file
  if ActiveUnitInfo.IsVirtual then
    SPath:='.'
  else begin
    if IsIncludeDirective then
      SPath:='.;'+CodeToolBoss.DefineTree.GetIncludePathForDirectory(
                            ExtractFilePath(ActiveUnitInfo.Filename))
    else
      SPath:='.;'+CodeToolBoss.DefineTree.GetUnitPathForDirectory(
                            ExtractFilePath(ActiveUnitInfo.Filename))
             +';'+CodeToolBoss.DefineTree.GetSrcPathForDirectory(
                            ExtractFilePath(ActiveUnitInfo.Filename));
  end;
  
  // search file in path (search especially for pascal files)
  if FindFile(FName,SPath) then begin
    result:=mrOk;
    InputHistories.FileDialogSettings.InitialDir:=ExtractFilePath(FName);
    if DoOpenEditorFile(FName,-1,[])=mrOk then begin
      EnvironmentOptions.AddToRecentOpenFiles(FName);
      SetRecentFilesMenu;
      SaveEnvironment;
    end;
  end;
end;

function TMainIDE.DoNewProject(NewProjectType:TProjectType):TModalResult;
var i:integer;
Begin
writeln('TMainIDE.DoNewProject A');
  Result:=mrCancel;

  // invalidate cached substituted macros
  IncreaseCompilerParseStamp;

  // close current project first
  If Project1<>nil then begin
    if SomethingOfProjectIsModified then begin
        if MessageDlg(lisProjectChanged, Format(lisSaveChangesToProject,
         [Project1.Title]),
          mtconfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes then begin
        if DoSaveProject([])=mrAbort then begin
          Result:=mrAbort;
          exit;
        end;
      end;
    end;
    if DoCloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  // create a virtual project (i.e. unsaved and without real project directory)
  
  // switch codetools to virtual project directory
  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
    VirtualDirectory;

  // create new project (TProject will automatically create the mainunit)
  Project1:=TProject.Create(NewProjectType);
  Project1.BeginUpdate(true);
  try
    Project1.OnFileBackup:=@DoBackupFile;
    Project1.Title := 'project1';
    Project1.CompilerOptions.CompilerPath:='$(CompPath)';
    UpdateCaption;
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;

    // add and load default required packages
    PkgBoss.AddDefaultDependencies(Project1);

    // set the project type specific things
    case NewProjectType of

    ptApplication:
      // create a first form unit
      DoNewEditorFile(nuForm,'','',
                        [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);

    ptProgram,ptCustomProgram:
      // show program unit
      DoOpenMainUnit(false);

    end;

    // rebuild codetools defines
    // (i.e. remove old project specific things and create new)
    IncreaseCompilerParseStamp;
    Project1.DefineTemplates.AllChanged;
  finally
    Project1.EndUpdate;
  end;

  // set all modified to false
  for i:=0 to Project1.UnitCount-1 do
    Project1.Units[i].Modified:=false;
  Project1.Modified:=false;

writeln('TMainIDE.DoNewProject end ',CodeToolBoss.ConsistencyCheck);
  Result:=mrOk;
end;

function TMainIDE.DoSaveProject(Flags: TSaveFlags):TModalResult;
var
  MainUnitSrcEdit: TSourceEditor;
  MainUnitInfo: TUnitInfo;
  i: integer;
  DestFilename: string;
  SkipSavingMainSource: Boolean;
begin
  Result:=mrCancel;
  if not (ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  SaveSourceEditorChangesToCodeCache(-1);
  SkipSavingMainSource:=false;

  writeln('TMainIDE.DoSaveProject A SaveAs=',sfSaveAs in Flags,' SaveToTestDir=',sfSaveToTestDir in Flags);

  // check that all new units are saved first to get valid filenames
  // (this can alter the mainunit: e.g. used unit names)
  for i:=0 to Project1.UnitCount-1 do begin
    if (Project1.Units[i].Loaded) and (Project1.Units[i].IsVirtual)
    and (Project1.MainUnitID<>i) then begin
      Result:=DoSaveEditorFile(Project1.Units[i].EditorIndex,
           [sfSaveAs,sfProjectSaving]
           +[sfSaveToTestDir,sfCheckAmbigiousFiles]*Flags);
      if (Result=mrAbort) or (Result=mrCancel) then exit;
    end;
  end;

  if SourceNotebook.Notebook=nil then
    Project1.ActiveEditorIndexAtStart:=-1
  else
    Project1.ActiveEditorIndexAtStart:=SourceNotebook.Notebook.PageIndex;

  // update source notebook page names
  UpdateSourceNames;

  // find mainunit
  GetMainUnit(MainUnitInfo,MainUnitSrcEdit,true);

  // save project specific settings of the source editor
  SaveSourceEditorProjectSpecificSettings;

  if Project1.IsVirtual then Include(Flags,sfSaveAs);
  if ([sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs]) then begin
    // let user choose a filename
    Result:=DoShowSaveProjectAsDialog;
    if Result<>mrOk then exit;
  end;
  
  // save project info file
  if not (sfSaveToTestDir in Flags) then begin
    Result:=Project1.WriteProject([],'');
    if Result=mrAbort then exit;
    EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
    EnvironmentOptions.Save(false);
    SaveIncludeLinks;
    UpdateCaption;
    if Result=mrAbort then exit;
  end;

  // save main source
  if MainUnitInfo<>nil then begin
    if MainUnitInfo.Loaded then begin
      // loaded in source editor
      Result:=DoSaveEditorFile(MainUnitInfo.EditorIndex,
               [sfProjectSaving]+[sfSaveToTestDir,sfCheckAmbigiousFiles]*Flags);
      if Result=mrAbort then exit;
    end else begin
      // not loaded in source editor (hidden)
      if not (sfSaveToTestDir in Flags) then begin
        DestFilename:=MainUnitInfo.Filename;
        if not MainUnitInfo.NeedsSaveToDisk then
          SkipSavingMainSource:=true;
      end else
        DestFilename:=GetTestUnitFilename(MainUnitInfo);
      if not SkipSavingMainSource then begin
        Result:=DoSaveCodeBufferToFile(MainUnitInfo.Source, DestFilename,
                                       not (sfSaveToTestDir in Flags));
        if Result=mrAbort then exit;
      end;
    end;
    // clear modified flags
    if not (sfSaveToTestDir in Flags) then begin
      if (Result=mrOk) then begin
        if MainUnitInfo<>nil then MainUnitInfo.Modified:=false;
        if MainUnitSrcEdit<>nil then MainUnitSrcEdit.Modified:=false;
      end;
    end;
  end;

  // save all editor files
  if (SourceNoteBook.Notebook<>nil) and (not (sfSaveToTestDir in Flags)) then
  begin
    for i:=0 to SourceNoteBook.Notebook.PageCount-1 do begin
      if (Project1.MainUnitID<0)
      or (Project1.MainUnitInfo.EditorIndex<>i) then begin
        Result:=DoSaveEditorFile(i,[sfProjectSaving]
                                +[sfSaveToTestDir,sfCheckAmbigiousFiles]*Flags);
        if Result=mrAbort then exit;
      end;
    end;
  end;

writeln('TMainIDE.DoSaveProject End');
end;

function TMainIDE.DoCloseProject:TModalResult;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoCloseProject A');
  {$ENDIF}
  // close all loaded files
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject A');{$ENDIF}
  while SourceNotebook.NoteBook<>nil do begin
    Result:=DoCloseEditorFile(SourceNotebook.Notebook.PageCount-1,
                              [cfProjectClosing]);
    if Result=mrAbort then exit;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject B');{$ENDIF}
  IncreaseCompilerParseStamp;
  // close Project
  if ProjInspector<>nil then ProjInspector.LazProject:=nil;
  FreeThenNil(Project1);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject C');{$ENDIF}
  Result:=mrOk;
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoCloseProject end ',CodeToolBoss.ConsistencyCheck);
  {$ENDIF}
end;

function TMainIDE.DoOpenProjectFile(AFileName:string;
  Flags: TOpenFlags):TModalResult;
var Ext,AText,ACaption: string;
  LowestEditorIndex,LowestUnitIndex,LastEditorIndex,i: integer;
  NewBuf: TCodeBuffer;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoOpenProjectFile A "'+AFileName+'"');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile A');{$ENDIF}
  Result:=mrCancel;
  if ExtractFileNameOnly(AFileName)='' then exit;
  
  AFilename:=ExpandFileName(TrimFilename(AFilename));
  Ext:=lowercase(ExtractFileExt(AFilename));
  
  // check if file exists
  if not FileExists(AFilename) then begin
    ACaption:='File not found';
    AText:='File "'+AFilename+'" not found.';
    Result:=MessageDlg(ACaption, AText, mtError, [mbAbort], 0);
    exit;
  end;

  // if there is a project info file, load that instead
  if (Ext<>'.lpi') and (FileExists(ChangeFileExt(AFileName,'.lpi'))) then begin
    // load instead of lazarus program file the project info file
    AFileName:=ChangeFileExt(AFileName,'.lpi');
    Ext:='.lpi';
  end;
  
  if (not FileIsText(AFilename)) then begin
    ACaption:='File not text';
    AText:='File "'+AFilename+'"'#13
          +'does not look like a text file.'#13
          +'Open it anyway?';
    Result:=MessageDlg(ACaption, AText, mtConfirmation, [mbYes, mbAbort], 0);
    if Result=mrAbort then exit;
  end;
  
  if ofAddToRecent in Flags then begin
    EnvironmentOptions.AddToRecentProjectFiles(AFileName);
    SetRecentProjectFilesMenu;
    SaveEnvironment;
  end;
  
  // close the old project
  if SomethingOfProjectIsModified then begin
    if MessageDlg(lisProjectChanged,
      Format(lisSaveChangesToProject, [Project1.Title]),
      mtconfirmation,[mbYes, mbNo, mbCancel],0) = mrYes then
    begin
      if DoSaveProject([])=mrAbort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
  end;
  Result:=DoCloseProject;
  if Result=mrAbort then exit;
  
  // create a new project
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoOpenProjectFile B');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile B');{$ENDIF}
  Project1:=TProject.Create(ptProgram);
  Project1.BeginUpdate(true);
  try
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
    Project1.OnFileBackup:=@DoBackupFile;

    // read project info file
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile B3');{$ENDIF}
    Project1.ReadProject(AFilename);
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile B4');{$ENDIF}
    Result:=DoCompleteLoadingProjectInfo;
  finally
    Project1.EndUpdate;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile B5');{$ENDIF}
  if Result<>mrOk then exit;

  if Project1.MainUnitID>=0 then begin
    // read MainUnit Source
    Result:=DoLoadCodeBuffer(NewBuf,Project1.MainFilename,
                             [lbfUpdateFromDisk,lbfRevert,lbfCheckIfText]);
    if Result=mrIgnore then Result:=mrAbort;
    if Result=mrAbort then exit;
    Project1.MainUnitInfo.Source:=NewBuf;
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.DoOpenProjectFile C');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile C');{$ENDIF}
  IncreaseCompilerParseStamp;

  // restore files
  LastEditorIndex:=-1;
  repeat
    // find the unit which was loaded last time and has the lowest editor index
    // of all not opened units
    LowestUnitIndex:=-1;
    LowestEditorIndex:=-1;
    for i:=0 to Project1.UnitCount-1 do begin
      if (Project1.Units[i].Loaded) then begin
        if (Project1.Units[i].EditorIndex>LastEditorIndex)
        and ((Project1.Units[i].EditorIndex<LowestEditorIndex)
             or (LowestEditorIndex<0)) then
        begin
          LowestEditorIndex:=Project1.Units[i].EditorIndex;
          LowestUnitIndex:=i;
        end;
      end;
    end;
    if LowestEditorIndex>=0 then begin
      // reopen file
      Result:=DoOpenEditorFile(Project1.Units[LowestUnitIndex].Filename,-1,
                    [ofProjectLoading,ofOnlyIfExists]);
      if Result=mrAbort then begin
        // mark all files, that are left to open as unloaded:
        for i:=0 to Project1.UnitCount-1 do begin
          if Project1.Units[i].Loaded
          and (Project1.Units[i].EditorIndex>LastEditorIndex) then begin
            Project1.Units[i].Loaded:=false;
            Project1.Units[i].EditorIndex:=-1;
            Project1.ActiveEditorIndexAtStart:=-1;
          end;
        end;
        exit;
      end;
      if Result=mrOk then begin
        // open successful
        if Project1.ActiveEditorIndexAtStart=LowestEditorIndex then
          Project1.ActiveEditorIndexAtStart:=SourceNoteBook.NoteBook.PageIndex;
        LastEditorIndex:=LowestEditorIndex;
      end else begin
        // open failed -> ignore this unit
        Project1.Units[LowestUnitIndex].EditorIndex:=-1;
        Project1.Units[LowestUnitIndex].Loaded:=false;
        if Project1.ActiveEditorIndexAtStart=LowestEditorIndex then
          Project1.ActiveEditorIndexAtStart:=-1;
      end;
    end;
  until LowestEditorIndex<0;
  Result:=mrCancel;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.DoOpenProjectFile D');
  {$ENDIF}

  // set active editor source editor
  if (SourceNoteBook.NoteBook<>nil) and (Project1.ActiveEditorIndexAtStart>=0)
  and (Project1.ActiveEditorIndexAtStart<SourceNoteBook.NoteBook.PageCount)
  then
    SourceNoteBook.Notebook.PageIndex:=Project1.ActiveEditorIndexAtStart;
    
  // select a form (object inspector, formeditor, control selection)
  if FLastFormActivated<>nil then begin
    PropertyEditorHook1.LookupRoot := FLastFormActivated;
    TDesigner(FLastFormActivated.Designer).SelectOnlyThisComponent(
                                                            FLastFormActivated);
  end;

  // set all modified to false
  for i:=0 to Project1.UnitCount-1 do begin
    Project1.Units[i].Modified:=false;
  end;
  Project1.Modified:=false;
  
  IncreaseCompilerParseStamp;
  Result:=mrOk;
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoOpenProjectFile end  CodeToolBoss.ConsistencyCheck=',CodeToolBoss.ConsistencyCheck);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile end');{$ENDIF}
end;

function TMainIDE.DoPublishProject(Flags: TSaveFlags;
  ShowDialog: boolean): TModalResult;
var
  SrcDir, DestDir: string;
  NewProjectFilename: string;
  Tool: TExternalToolOptions;
  CommandAfter, CmdAfterExe, CmdAfterParams: string;
  
  procedure ShowErrorForCommandAfter;
  begin
    MessageDlg('Invalid command',
      'The command after "'+CmdAfterExe+'" is not executable.',
      mtError,[mbCancel],0);
  end;
  
begin
  // show the publish project dialog
  if ShowDialog then begin
    Result:=ShowPublishProjectDialog(Project1.PublishOptions);
    if Result<>mrOk then exit;
    IncreaseCompilerParseStamp;
  end;

  // save project
  Result:=DoSaveProject(Flags);
  if Result<>mrOk then exit;

  // check command after
  CommandAfter:=Project1.PublishOptions.CommandAfter;
  if not MacroList.SubstituteStr(CommandAfter) then begin
    Result:=mrCancel;
    exit;
  end;
  SplitCmdLine(CommandAfter,CmdAfterExe,CmdAfterParams);
  if (CmdAfterExe<>'') and not FileIsExecutable(CmdAfterExe) then begin
    ShowErrorForCommandAfter;
    Result:=mrCancel;
    exit;
  end;

  // clear destination directory
  DestDir:=GetProjPublishDir;
  if (DestDir='') then begin
    MessageDlg('Invalid destination directory',
      'Destination directory "'+DestDir+'" is invalid.'#13
      +'Please choose a complete path.',
      mtError,[mbOk],0);
    Result:=mrCancel;
    exit;
  end;
  if DirectoryExists(DestDir) and (not DeleteDirectory(DestDir,true)) then
  begin
    MessageDlg('Unable to clean up destination directory',
      'Unable to clean up "'+DestDir+'".'#13
      +'Please check permissions.',
      mtError,[mbOk],0);
    Result:=mrCancel;
    exit;
  end;
  
  // copy the project directory
  SrcDir:=AppendPathDelim(Project1.ProjectDirectory);
  if not CopyDirectoryWithMethods(SrcDir,DestDir,
    @OnCopyFile,@OnCopyError,Project1.PublishOptions) then
  begin
    Result:=mrCancel;
    exit;
  end;

  // write a filtered .lpi file
  NewProjectFilename:=DestDir+ExtractFilename(Project1.ProjectInfoFile);
  DeleteFile(NewProjectFilename);
  Result:=Project1.WriteProject(Project1.PublishOptions.WriteFlags,
                                NewProjectFilename);
  if Result<>mrOk then exit;
  
  // execute 'CommandAfter'
  if (CmdAfterExe<>'') then begin
    if FileIsExecutable(CmdAfterExe) then begin
      Tool:=TExternalToolOptions.Create;
      Tool.Filename:=CmdAfterExe;
      Tool.Title:='Command after publishing project';
      Tool.WorkingDirectory:=DestDir;
      Tool.CmdLineParams:=CmdAfterParams;
      Result:=EnvironmentOptions.ExternalTools.Run(Tool,MacroList);
      if Result<>mrOk then exit;
    end else begin
      ShowErrorForCommandAfter;
      Result:=mrCancel;
      exit;
    end;
  end;
end;

function TMainIDE.DoShowProjectInspector: TModalResult;
begin
  if ProjInspector=nil then begin
    ProjInspector:=TProjectInspectorForm.Create(Self);
    ProjInspector.OnOpen:=@ProjInspectorOpen;
    ProjInspector.OnShowOptions:=@mnuProjectOptionsClicked;
    ProjInspector.OnAddUnitToProject:=@ProjInspectorAddUnitToProject;
    ProjInspector.OnRemoveFile:=@ProjInspectorRemoveFile;

    ProjInspector.LazProject:=Project1;
  end;
  ProjInspector.ShowOnTop;
  Result:=mrOk;
end;

function TMainIDE.DoCreateProjectForProgram(
  ProgramBuf: TCodeBuffer): TModalResult;
var NewProjectType:TProjectType;
  MainUnitInfo: TUnitInfo;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('[TMainIDE.DoCreateProjectForProgram] A ',ProgramBuf.Filename);
  {$ENDIF}
  Result:=mrCancel;

  if SomethingOfProjectIsModified then begin
    if MessageDlg(lisProjectChanged, Format(lisSaveChangesToProject,
      [Project1.Title]),
      mtconfirmation, [mbYes, mbNo, mbCancel], 0)=mrYes then
    begin
      if DoSaveProject([])=mrAbort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
  end;

  // let user choose the program type
  if ChooseNewProject(NewProjectType)=mrCancel then exit;

  // close old project
  If Project1<>nil then begin
    if DoCloseProject=mrAbort then begin
      Result:=mrAbort;
      exit;
    end;
  end;

  // switch codetools to new project directory
  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
    ExpandFilename(ExtractFilePath(ProgramBuf.Filename));

  // create a new project
  Project1:=TProject.Create(NewProjectType);
  Project1.BeginUpdate(true);
  try
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
    Project1.OnFileBackup:=@DoBackupFile;
    MainUnitInfo:=Project1.MainUnitInfo;
    MainUnitInfo.Source:=ProgramBuf;
    Project1.ProjectInfoFile:=ChangeFileExt(ProgramBuf.Filename,'.lpi');
    Project1.CompilerOptions.CompilerPath:='$(CompPath)';
    UpdateCaption;
    IncreaseCompilerParseStamp;

    // add and load default required packages
    PkgBoss.AddDefaultDependencies(Project1);

    // rebuild project specific codetools defines
    // ToDo
  finally
    Project1.EndUpdate;
  end;
  if Result<>mrOk then exit;

  // show program unit
  Result:=DoOpenMainUnit(false);
  if Result=mrAbort then exit;

  {$IFDEF IDE_VERBOSE}
  writeln('[TMainIDE.DoCreateProjectForProgram] END');
  {$ENDIF}
  Result:=mrOk;
end;

function TMainIDE.DoAddActiveUnitToProject: TModalResult;
var
  ActiveSourceEditor:TSourceEditor; 
  ActiveUnitInfo:TUnitInfo;
  s, ShortUnitName: string;
begin
  Result:=mrCancel;
  BeginCodeTool(ActiveSourceEditor,ActiveUnitInfo,[]);
  if ActiveUnitInfo<>nil then begin
    if ActiveUnitInfo.IsPartOfProject=false then begin
      if not ActiveUnitInfo.IsVirtual then
        s:='"'+ActiveUnitInfo.Filename+'"'
      else
        s:='"'+ActiveSourceEditor.PageName+'"';
      if (Project1.ProjectType in [ptProgram, ptApplication])
      and (ActiveUnitInfo.UnitName<>'')
      and (Project1.IndexOfUnitWithName(ActiveUnitInfo.UnitName,
          true,ActiveUnitInfo)>=0) then
      begin
        MessageDlg('Unable to add '+s+' to project, because there is already a '
           +'unit with the same name in the Project1.',mtInformation,[mbOk],0);
      end else begin
        if MessageDlg('Add '+s+' to project?',mtConfirmation,[mbOk,mbCancel],0)
          =mrOk then
        begin
          ActiveUnitInfo.IsPartOfProject:=true;
          if (ActiveUnitInfo.UnitName<>'')
          and (Project1.ProjectType in [ptProgram, ptApplication]) then begin
            ActiveUnitInfo.ReadUnitNameFromSource;
            ShortUnitName:=ActiveUnitInfo.UnitName;
            if (ShortUnitName<>'') then begin
              if CodeToolBoss.AddUnitToMainUsesSection(
                 Project1.MainUnitInfo.Source,ShortUnitName,'')
              then
                Project1.MainUnitInfo.Modified:=true;
            end;
          end;
          Project1.Modified:=true;
        end;
      end;
    end else begin
      if not ActiveUnitInfo.IsVirtual then
        s:='The file "'+ActiveUnitInfo.Filename+'"'
      else
        s:='The file "'+ActiveSourceEditor.PageName+'"';
      s:=s+' is already part of the Project1.';
      MessageDlg(s,mtInformation,[mbOk],0);
    end;
  end else begin
    Result:=mrOk;
  end;
end;

function TMainIDE.DoRemoveFromProjectDialog: TModalResult;
var UnitList: TList;
  i:integer;
  AName: string;
  AnUnitInfo: TUnitInfo;
Begin
  UnitList:=TList.Create;
  try
    for i:=0 to Project1.UnitCount-1 do begin
      AnUnitInfo:=Project1.Units[i];
      if (AnUnitInfo.IsPartOfProject) and (i<>Project1.MainUnitID) then begin
        AName:=Project1.RemoveProjectPathFromFilename(AnUnitInfo.FileName);
        UnitList.Add(TViewUnitsEntry.Create(AName,i,false));
      end;
    end;
    if ShowViewUnitsDlg(UnitList,true,'Remove from project')=mrOk then begin
      for i:=0 to UnitList.Count-1 do begin
        if TViewUnitsEntry(UnitList[i]).Selected then begin
          AnUnitInfo:=Project1.Units[TViewUnitsEntry(UnitList[i]).ID];
          AnUnitInfo.IsPartOfProject:=false;
          if (Project1.MainUnitID>=0)
          and (Project1.ProjectType in [ptProgram, ptApplication]) then begin
            if (AnUnitInfo.UnitName<>'') then begin
              if CodeToolBoss.RemoveUnitFromAllUsesSections(
                Project1.MainUnitInfo.Source,AnUnitInfo.UnitName)
              then
                Project1.MainUnitInfo.Modified:=true;
            end;
            if (AnUnitInfo.FormName<>'') then begin
              Project1.RemoveCreateFormFromProjectFile(
                  'T'+AnUnitInfo.FormName,AnUnitInfo.FormName);
            end;
          end;
        end;
      end;
    end;
  finally
    UnitList.Free;
  end;
  Result:=mrOk;
end;

procedure TMainIDE.DoWarnAmbigiousFiles;
var
  AnUnitInfo: TUnitInfo;
  i: integer;
  DestFilename: string;
begin
  for i:=0 to Project1.UnitCount-1 do begin
    AnUnitInfo:=Project1.Units[i];
    if (AnUnitInfo.IsPartOfProject) and (not AnUnitInfo.IsVirtual) then begin
      DestFilename:=GetTargetUnitFilename(AnUnitInfo);
      DoCheckAmbigiousSources(DestFilename,true);
    end;
  end;
end;

function TMainIDE.DoSaveForBuild: TModalResult;
begin
  Result:=mrCancel;
  if ToolStatus<>itNone then begin
    Result:=mrAbort;
    exit;
  end;
  if Project1=nil then Begin
    MessageDlg('Create a project first!',mterror,[mbok],0);
    Exit;
  end;

  // check for a main file to compile
  if Project1.MainFilename='' then exit;

  // save all files
  if not Project1.IsVirtual then
    Result:=DoSaveAll([sfCheckAmbigiousFiles])
  else
    Result:=DoSaveProjectToTestDirectory;
  if Result<>mrOk then exit;
    
  Result:=PkgBoss.DoSaveAllPackages([]);
end;

function TMainIDE.DoSaveProjectToTestDirectory: TModalResult;
begin
  Result:=mrCancel;
  if (EnvironmentOptions.TestBuildDirectory='')
  or (not DirectoryExists(EnvironmentOptions.TestBuildDirectory)) then begin
    if (EnvironmentOptions.TestBuildDirectory<>'') then begin
      MessageDlg('The Test Directory could not be found:'#13
             +'"'+EnvironmentOptions.TestBuildDirectory+'"'#13
             +'(see environment options)',mtError,[mbCancel],0);
      Result:=mrCancel;
      exit;
    end;
    Result:=MessageDlg('Build new project',
       'The project must be saved before building'#13
      +'If you set the Test Directory in the environment options,'#13
      +'you can create new projects and build them at once.'#13
      +'Save project?',mtInformation,[mbYes,mbNo],0);
    if Result<>mrYes then exit;
    Result:=DoSaveAll([sfCheckAmbigiousFiles]);
    exit;
  end;
  Result:=DoSaveProject([sfSaveToTestDir,sfCheckAmbigiousFiles]);
end;

function TMainIDE.DoShowToDoList: TModalResult;
begin
  if not Assigned(frmToDo) then begin
    frmToDo:=TfrmToDo.Create(Self);
    frmToDo.OnOpenFile:=@ViewProjectTodosOpenFile;
  end;

  frmToDo.FileName:=Project1.MainUnitInfo.Filename;

  frmToDo.ShowOnTop;
  Result:=mrOk;
end;

function TMainIDE.DoBuildProject(BuildAll: boolean): TModalResult;
var
  DefaultFilename: string;
begin
  Result:=DoSaveForBuild;
  if Result<>mrOk then exit;

  // compile required packages
  {$IFDEF EnablePkgs}
  Result:=PkgBoss.DoCompileProjectDependencies(Project1,[pcfDoNotSaveEditorFiles]);
  if Result<>mrOk then exit;
  {$ENDIF}

  // get main source filename
  if not Project1.IsVirtual then
    DefaultFilename:=''
  else
    DefaultFilename:=GetTestUnitFilename(Project1.MainUnitInfo);

  // clear old error lines
  SourceNotebook.ClearErrorLines;

  try
    // change tool status
    ToolStatus:=itBuilder;
    
    // show messages
    MessagesView.Clear;
    DoArrangeSourceEditorAndMessageView(false);
    TheOutputFilter.OnOutputString:=@MessagesView.Add;
    TheOutputFilter.OnReadLine:=@MessagesView.ShowProgress;

    // warn ambigious files
    DoWarnAmbigiousFiles;

    // compile
    Result:=TheCompiler.Compile(Project1,BuildAll,DefaultFilename);
    if Result=mrOk then begin
      MessagesView.Add(
        'Project "'+Project1.Title+'" successfully built. :)');
    end else begin
      DoJumpToCompilerMessage(-1,true);
    end;
    DoCheckFilesOnDisk;
  finally
    ToolStatus:=itNone;
  end;
end;

function TMainIDE.DoInitProjectRun: TModalResult;
var
  ProgramFilename, WorkingDir: String;
begin
  if ToolStatus <> itNone
  then begin
    // already running so no initialization needed
    Result := mrOk;
    Exit;
  end; 

  Result := mrCancel;

  // Check if we can run this project
  if not (Project1.ProjectType in [ptProgram, ptApplication, ptCustomProgram])
  or (Project1.MainUnitID < 0)
  or (ToolStatus <> itNone)
  then Exit;

  // Build project first
  if DoBuildProject(false) <> mrOk 
  then Exit;
  
  // Check project build
  ProgramFilename := GetProjectTargetFilename;
  if not FileExists(ProgramFilename)
  then begin
    MessageDlg('File not found',
      Format('No program file "%s" found.', [ProgramFilename]), mtError,
      [mbCancel], 0);
    Exit;
  end;

  // Setup debugger
  case EnvironmentOptions.DebuggerType of
    dtGnuDebugger: begin
      if (DebugBoss.DoInitDebugger <> mrOk)
      then Exit;
      // ToDo: set working directory
    end;
  else 
    // Temp solution, in future it will be run by dummy debugger
    try
      CheckIfFileIsExecutable(ProgramFilename);
      FRunProcess := TProcess.Create(nil);
      FRunProcess.CommandLine := GetRunCommandLine;
      WorkingDir:=Project1.RunParameterOptions.WorkingDirectory;
      if WorkingDir='' then
        WorkingDir:=ExtractFilePath(GetProjectTargetFilename);
      if not MacroList.SubstituteStr(WorkingDir) then begin
        Result:=mrCancel;
        exit;
      end;
      FRunProcess.CurrentDirectory:=ExpandFilename(WorkingDir);
      Project1.RunParameterOptions.AssignEnvironmentTo(FRunProcess.Environment);

      FRunProcess.Options:= [poNoConsole];
      FRunProcess.ShowWindow := swoNone;
    except
      on e: Exception do 
        MessageDlg(Format('Error initializing program'#13 + 
                          '"%s"'#13 + 
                          'Error: %s', [ProgramFilename, e.Message]), mterror, [mbok], 0);
    end;
  end;   
  
  Result := mrOK;
  ToolStatus := itDebugger;
end;

function TMainIDE.DoRunProject: TModalResult;
begin
  Writeln('[TMainIDE.DoRunProject] A');
  
  if (DoInitProjectRun <> mrOK)
  or (ToolStatus <> itDebugger)
  then begin
    Result := mrAbort;
    Exit;
  end;

  Result := mrCancel;

  if EnvironmentOptions.DebuggerType <> dtNone then begin
    DebugBoss.RunDebugger;
    Result := mrOK;
  end else begin
    if FRunProcess = nil then Exit;
    try
      Writeln('  EXECUTING "',FRunProcess.CommandLine,'"');
      Writeln('    WorkingDir "',FRunProcess.CurrentDirectory,'"');
      // just run the program and don't care (no watch, no debugging)
      // just check from time to time, if it has terminated and clean up
      GetDefaultProcessList.Add(FRunProcess);
      FRunProcess.Execute;
      ToolStatus:=itNone;
      Result := mrOk;
    except
      on e: Exception do 
        MessageDlg(Format('Error initializing program'#13 + 
                          '"%s"'#13 + 
                          'Error: %s',
                          [FRunProcess.CommandLine, e.Message]), mtError,
                          [mbOk], 0);
    end;
  end;   
  Writeln('[TMainIDE.DoRunProject] END');
end;

function TMainIDE.SomethingOfProjectIsModified: boolean;
begin
  Result:=(Project1<>nil)
      and (Project1.SomethingModified or SourceNotebook.SomethingModified);
end;

function TMainIDE.DoSaveAll(Flags: TSaveFlags): TModalResult;
begin
  writeln('TMainIDE.DoSaveAll');
  Result:=DoSaveProject(Flags);
  SaveEnvironment;
  SaveIncludeLinks;
  InputHistories.Save;
  // ToDo: save package, cvs settings, ...
end;

//-----------------------------------------------------------------------------

function TMainIDE.DoRunExternalTool(Index: integer): TModalResult;
begin
  SourceNotebook.ClearErrorLines;
  Result:=EnvironmentOptions.ExternalTools.Run(Index,MacroList);
  DoCheckFilesOnDisk;
end;

function TMainIDE.DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult;
var
  PkgOptions: string;
  IDEBuildFlags: TBuildLazarusFlags;
begin
  try
    // first compile all lazarus components (LCL, SynEdit, CodeTools, ...)
    SourceNotebook.ClearErrorLines;
    Result:=BuildLazarus(MiscellaneousOptions.BuildLazOpts,
                         EnvironmentOptions.ExternalTools,MacroList,
                         '',Flags+[blfWithoutIDE]);
    if Result<>mrOk then exit;
    
    // then compile the IDE
    if ([blfWithStaticPackages,blfOnlyIDE]*Flags=[])
    and (MiscellaneousOptions.BuildLazOpts.ItemIDE.MakeMode=mmNone) then exit;

    // prepare static auto install packages
    PkgOptions:='';
    {$IFDEF EnablePkgs}
    if (blfWithStaticPackages in Flags)
    or MiscellaneousOptions.BuildLazOpts.WithStaticPackages then begin
      // compile auto install static packages
      Result:=PkgBoss.DoCompileAutoInstallPackages([]);
      if Result<>mrOk then exit;

      // create uses addition for IDE
      Result:=PkgBoss.DoSaveAutoInstallConfig;
      if Result<>mrOk then exit;

      // create inherited compiler options
      PkgOptions:=PkgBoss.DoGetIDEInstallPackageOptions;
    end;
    {$ENDIF}

    // save extra options
    IDEBuildFlags:=Flags+[blfOnlyIDE];
    Result:=SaveIDEMakeOptions(MiscellaneousOptions.BuildLazOpts,
                     MacroList,PkgOptions,IDEBuildFlags);
    if Result<>mrOk then exit;

    // make ide
    SourceNotebook.ClearErrorLines;
    Result:=BuildLazarus(MiscellaneousOptions.BuildLazOpts,
                         EnvironmentOptions.ExternalTools,MacroList,
                         PkgOptions,IDEBuildFlags+[blfUseMakeIDECfg]);
    if Result<>mrOk then exit;
  finally
    DoCheckFilesOnDisk;
  end;
end;

function TMainIDE.DoConvertDFMtoLFM: TModalResult;
var
  OpenDialog: TOpenDialog;
  i: integer;
  AFilename: string;
begin
  Result:=mrOk;
  OpenDialog:=TOpenDialog.Create(Self);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisSelectDFMFiles;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      For I := 0 to OpenDialog.Files.Count-1 do begin
        AFilename:=ExpandFilename(OpenDialog.Files.Strings[i]);
        if DoConvertDFMFileToLFMFile(AFilename)=mrAbort then begin
          Result:=mrAbort;
          break;
        end else
          Result:=mrOk;
      end;
      SaveEnvironment;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
  DoCheckFilesOnDisk;
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.UpdateCustomToolsInMenu;
  
  Creates a TMenuItem for each custom external tool.
-------------------------------------------------------------------------------}
procedure TMainIDE.UpdateCustomToolsInMenu;
var
  ToolCount: integer;
  
  procedure CreateToolMenuItems;
  var
    CurMenuItem: TMenuItem;
    LastIndex, FirstIndex, ExistingCount: integer;
  begin
    // add separator
    if (ToolCount>0) and (CustomExtToolMenuSeparator=nil) then begin
      CustomExtToolMenuSeparator:=CreateMenuSeparator;
      mnuTools.Add(CustomExtToolMenuSeparator);
    end;
    // add enough menuitems
    if CustomExtToolMenuSeparator=nil then exit;
    FirstIndex:=CustomExtToolMenuSeparator.MenuIndex+1;
    LastIndex:=FirstIndex;
    while (LastIndex<mnuTools.Count) and (mnuTools[LastIndex].Caption<>'-') do
      inc(LastIndex);
    ExistingCount:=LastIndex-FirstIndex;
    while ExistingCount<ToolCount do begin
      CurMenuItem := TMenuItem.Create(Self);
      CurMenuItem.Name:='itmToolCustomExt'+IntToStr(ExistingCount);
      CurMenuItem.Caption:=CurMenuItem.Name;
      mnuTools.Insert(LastIndex,CurMenuItem);
      inc(LastIndex);
      inc(ExistingCount);
    end;
    // delete unneeded menuitems
    while ExistingCount>ToolCount do begin
      mnuTools[LastIndex].Free;
      dec(LastIndex);
      dec(ExistingCount);
    end;
  end;

  procedure SetToolMenuItems;
  var
    CurMenuItem: TMenuItem;
    i, Index: integer;
    ExtTool: TExternalToolOptions;
  begin
    if CustomExtToolMenuSeparator=nil then exit;
    i:=CustomExtToolMenuSeparator.MenuIndex+1;
    Index:=0;
    while (i<mnuTools.Count) do begin
      CurMenuItem:=mnuTools[i];
      if CurMenuItem.Caption='-' then break;
      ExtTool:=EnvironmentOptions.ExternalTools[Index];
      CurMenuItem.Caption:=ExtTool.Title;
      CurMenuItem.ShortCut:=ShortCut(ExtTool.Key,ExtTool.Shift);
      CurMenuItem.OnClick:=@mnuCustomExtToolClick;
      inc(i);
      inc(Index);
    end;
  end;

begin
  ToolCount:=EnvironmentOptions.ExternalTools.Count;
  CreateToolMenuItems;
  SetToolMenuItems;
end;

function TMainIDE.DoCheckSyntax: TModalResult;
var
  ActiveUnitInfo:TUnitInfo;
  ActiveSrcEdit:TSourceEditor;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  ErrorMsg: string;
begin
  Result:=mrOk;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveUnitInfo=nil) or (ActiveUnitInfo.Source=nil)
  or (ActiveSrcEdit=nil) then exit;
  SaveSourceEditorChangesToCodeCache(-1);
  CodeToolBoss.VisibleEditorLines:=ActiveSrcEdit.EditorComponent.LinesInWindow;
  if not CodeToolBoss.CheckSyntax(ActiveUnitInfo.Source,NewCode,NewX,NewY,
    NewTopLine,ErrorMsg) then
  begin
    DoJumpToCodeToolBossError;
  end;
end;

//-----------------------------------------------------------------------------

procedure TMainIDE.GetCurrentUnit(var ActiveSourceEditor:TSourceEditor;
  var ActiveUnitInfo:TUnitInfo);
begin
  if SourceNoteBook.NoteBook=nil then begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end else begin
    GetUnitWithPageIndex(SourceNotebook.NoteBook.PageIndex,ActiveSourceEditor,
       ActiveUnitInfo);
  end;
end;

procedure TMainIDE.GetUnitWithPageIndex(PageIndex:integer;
  var ActiveSourceEditor:TSourceEditor; var ActiveUnitInfo:TUnitInfo);
begin
  if SourceNoteBook.NoteBook=nil then begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end else begin
    ActiveSourceEditor:=SourceNoteBook.FindSourceEditorWithPageIndex(PageIndex);
    if ActiveSourceEditor=nil then
      ActiveUnitInfo:=nil
    else 
      ActiveUnitInfo:=Project1.UnitWithEditorIndex(PageIndex);
  end;
end;

procedure TMainIDE.GetDesignerUnit(ADesigner: TDesigner;
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
begin
  if ADesigner<>nil then begin
    GetUnitWithForm(ADesigner.Form,ActiveSourceEditor,ActiveUnitInfo);
  end else begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end;
end;

procedure TMainIDE.GetUnitWithForm(AForm: TCustomForm;
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
var
  i: integer;
begin
  if AForm<>nil then begin
    i:=Project1.IndexOfUnitWithForm(AForm,false,nil);
    if i>=0 then begin
      ActiveUnitInfo:=Project1.Units[i];
      ActiveSourceEditor:=SourceNoteBook.FindSourceEditorWithPageIndex(
        ActiveUnitInfo.EditorIndex);
      exit;
    end;
  end;
  ActiveSourceEditor:=nil;
  ActiveUnitInfo:=nil;
end;

function TMainIDE.GetSourceEditorForUnitInfo(AnUnitInfo: TUnitInfo
  ): TSourceEditor;
begin
  Result:=SourceNoteBook.FindSourceEditorWithPageIndex(AnUnitInfo.EditorIndex);
end;

function TMainIDE.DoSaveStreamToFile(AStream:TStream; 
  const Filename:string; IsPartOfProject:boolean):TModalResult;
// save to file with backup and user interaction
var AText,ACaption:string;
  NewBuf: TCodeBuffer;
begin
  Result:=DoBackupFile(Filename,IsPartOfProject);
  if Result<>mrOk then exit;
  repeat
    NewBuf:=CodeToolBoss.CreateFile(FileName);
    if (NewBuf<>nil) or (not NewBuf.SaveToFile(Filename)) then begin
      ACaption:='Write error';
      AText:='Unable to save file "'+Filename+'"';
      Result:=MessageDlg(ACaption,AText,mterror, [mbabort, mbretry, mbignore],0);
      if Result=mrIgnore then Result:=mrOk;
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoLoadMemoryStreamFromFile(MemStream: TMemoryStream; 
  const AFilename:string): TModalResult;
var FileStream: TFileStream;
  ACaption,AText:string;
begin
  repeat
    try
      FileStream:=TFileStream.Create(AFilename,fmOpenRead);
      try
        FileStream.Position:=0;
        MemStream.CopyFrom(FileStream,FileStream.Size);
        MemStream.Position:=0;
      finally
        FileStream.Free;
      end;
      Result:=mrOk;
    except
      ACaption:='Read Error';
      AText:='Unable to read file "'+AFilename+'"!';
      result := Application.MessageBox(PChar(aText),pChar(aCaption),mb_IconError+mb_AbortRetryIgnore);
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoSaveCodeBufferToFile(ABuffer: TCodeBuffer;
  const AFilename: string; IsPartOfProject:boolean): TModalResult;
var
  ACaption,AText:string;
begin
  Result:=DoBackupFile(AFilename,IsPartOfProject);
  if Result<>mrOk then exit;
  repeat
    if ABuffer.SaveToFile(AFilename) then begin
      Result:=mrOk;
    end else begin
      ACaption:='Write Error';
      AText:='Unable to write to file "'+AFilename+'"!';
      Result:=MessageDlg(ACaption,AText,mtError,[mbAbort, mbRetry, mbIgnore],0);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoLoadCodeBuffer(var ACodeBuffer: TCodeBuffer; 
  const AFilename: string; Flags: TLoadBufferFlags): TModalResult;
var
  ACaption,AText:string;
begin
  repeat
    {$IFDEF IDE_DEBUG}
    writeln('[TMainIDE.DoLoadCodeBuffer] A ',AFilename);
    {$ENDIF}
    if (lbfCheckIfText in Flags)
    and FileExists(AFilename) and (not FileIsText(AFilename))
    then begin
      if lbfQuiet in Flags then begin
        Result:=mrCancel;
      end else begin
        ACaption:='File not text';
        AText:='File "'+AFilename+'"'#13
              +'does not look like a text file.'#13
              +'Open it anyway?';
        Result:=MessageDlg(ACaption, AText, mtConfirmation,
                           [mbOk, mbIgnore, mbAbort], 0);
      end;
      if Result<>mrOk then break;
    end;
    ACodeBuffer:=CodeToolBoss.LoadFile(AFilename,lbfUpdateFromDisk in Flags,
                                       lbfRevert in Flags);
    if ACodeBuffer<>nil then begin
      Result:=mrOk;
      {$IFDEF IDE_DEBUG}
      writeln('[TMainIDE.DoLoadCodeBuffer] ',ACodeBuffer.SourceLength,' ',ACodeBuffer.Filename);
      {$ENDIF}
    end else begin
      if lbfQuiet in Flags then
        Result:=mrCancel
      else begin
        ACaption:='Read Error';
        AText:='Unable to read file "'+AFilename+'"!';
        Result:=MessageDlg(ACaption,AText,mtError,[mbAbort,mbRetry,mbIgnore],0);
      end;
      if Result=mrAbort then break;
    end;
  until Result<>mrRetry;
  if (ACodeBuffer=nil) and (lbfCreateClearOnError in Flags) then begin
    ACodeBuffer:=CodeToolBoss.CreateFile(AFilename);
  end;
end;

{-------------------------------------------------------------------------------
  TMainIDE DoBackupFile

  Params:  const Filename:string;
           IsPartOfProject:boolean
  Returns: TModalResult

  Rename existing file to backup file.
-------------------------------------------------------------------------------}
function TMainIDE.DoBackupFile(const Filename:string; 
  IsPartOfProject:boolean): TModalResult;
var BackupFilename, CounterFilename: string;
  AText,ACaption:string;
  BackupInfo: TBackupInfo;
  FilePath, FileNameOnly, FileExt, SubDir: string;
  i: integer;
begin
  Result:=mrOk;
  if not (FileExists(Filename)) then exit;
  if IsPartOfProject then
    BackupInfo:=EnvironmentOptions.BackupInfoProjectFiles
  else
    BackupInfo:=EnvironmentOptions.BackupInfoOtherFiles;
  if (BackupInfo.BackupType=bakNone)
  or ((BackupInfo.BackupType=bakSameName) and (BackupInfo.SubDirectory='')) then
    exit;
  FilePath:=ExtractFilePath(Filename);
  FileExt:=ExtractFileExt(Filename);
  FileNameOnly:=ExtractFilenameOnly(Filename);
  if BackupInfo.SubDirectory<>'' then begin
    SubDir:=FilePath+BackupInfo.SubDirectory;
    repeat
      if not DirectoryExists(SubDir) then begin
        if not CreateDir(SubDir) then begin
          Result:=MessageDlg('Unable to create backup directory "'+SubDir+'".'
                ,mtWarning,[mbAbort,mbRetry,mbIgnore],0);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end;
  if BackupInfo.BackupType in
     [bakSymbolInFront,bakSymbolBehind,bakUserDefinedAddExt,bakSameName] then
  begin
    case BackupInfo.BackupType of
      bakSymbolInFront:
        BackupFilename:=FileNameOnly+'.~'+copy(FileExt,2,length(FileExt)-1);
      bakSymbolBehind:
        BackupFilename:=FileNameOnly+FileExt+'~';
      bakUserDefinedAddExt:
        BackupFilename:=FileNameOnly+FileExt+'.'+BackupInfo.AdditionalExtension;
      bakSameName:
        BackupFilename:=FileNameOnly+FileExt;
    end;
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+PathDelim+BackupFilename
    else
      BackupFilename:=FilePath+BackupFilename;
    // remove old backup file
    repeat
      if FileExists(BackupFilename) then begin
        if not DeleteFile(BackupFilename) then begin
          ACaption:='Delete file failed';
          AText:='Unable to remove old backup file "'+BackupFilename+'"!';
          Result:=MessageDlg(ACaption,AText,mtError,[mbAbort,mbRetry,mbIgnore],
                             0);
          if Result=mrAbort then exit;
          if Result=mrIgnore then Result:=mrOk;
        end;
      end;
    until Result<>mrRetry;
  end else begin
    // backup with counter
    if BackupInfo.SubDirectory<>'' then
      BackupFilename:=SubDir+PathDelim+FileNameOnly+FileExt+';'
    else
      BackupFilename:=Filename+';';
    if BackupInfo.MaxCounter<=0 then begin
      // search first non existing backup filename
      i:=1;
      while FileExists(BackupFilename+IntToStr(i)) do inc(i);
      BackupFilename:=BackupFilename+IntToStr(i);
    end else begin
      // rename all backup files (increase number)
      i:=1;
      while FileExists(BackupFilename+IntToStr(i))
      and (i<=BackupInfo.MaxCounter) do inc(i);
      if i>BackupInfo.MaxCounter then begin
        dec(i);
        CounterFilename:=BackupFilename+IntToStr(BackupInfo.MaxCounter);
        // remove old backup file
        repeat
          if FileExists(CounterFilename) then begin
            if not DeleteFile(CounterFilename) then begin
              ACaption:='Delete file failed';
              AText:='Unable to remove old backup file "'+CounterFilename+'"!';
              Result:=MessageDlg(ACaption,AText,mtError,
                                 [mbAbort,mbRetry,mbIgnore],0);
              if Result=mrAbort then exit;
              if Result=mrIgnore then Result:=mrOk;
            end;
          end;
        until Result<>mrRetry;
      end;
      // rename all old backup files
      dec(i);
      while i>=1 do begin
        repeat
          if not RenameFile(BackupFilename+IntToStr(i),
             BackupFilename+IntToStr(i+1)) then
          begin
            ACaption:='Rename file failed';
            AText:='Unable to rename file "'+BackupFilename+IntToStr(i)
                  +'" to "'+BackupFilename+IntToStr(i+1)+'"!';
            Result:=MessageDlg(ACaption,AText,mtError,
                               [mbAbort,mbRetry,mbIgnore],0);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
          end;
        until Result<>mrRetry;
        dec(i);
      end;
      BackupFilename:=BackupFilename+'1';
    end;
  end;
  // backup file
  repeat
    if not BackupFile(Filename,BackupFilename) then begin
      ACaption:='Backup file failed';
      AText:='Unable to backup file "'+Filename+'" to "'+BackupFilename+'"!';
      Result:=MessageDlg(ACaption,AText,mterror,[mbabort,mbretry,mbignore],0);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoDeleteAmbigiousFiles(const Filename: string): TModalResult;
var
  ADirectory: String;
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurFilename: String;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbigiousFileAction=afaIgnore then exit;
  if EnvironmentOptions.AmbigiousFileAction
    in [afaAsk,afaAutoDelete,afaAutoRename]
  then begin
    ADirectory:=AppendPathDelim(ExtractFilePath(Filename));
    if SysUtils.FindFirst(ADirectory+FindMask,faAnyFile,FileInfo)=0 then begin
      ShortFilename:=ExtractFileName(Filename);
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..')
        or ((FileInfo.Attr and faDirectory)<>0) then continue;
        if (ShortFilename<>FileInfo.Name)
        and (AnsiCompareText(ShortFilename,FileInfo.Name)=0)
        then begin
          CurFilename:=ADirectory+FileInfo.Name;
          if EnvironmentOptions.AmbigiousFileAction=afaAsk then begin
            if MessageDlg('Delete ambigious file?',
              'Delete ambigious file "'+CurFilename+'"?',
              mtConfirmation,[mbYes,mbNo],0)=mrNo
            then continue;
          end;
          if EnvironmentOptions.AmbigiousFileAction in [afaAutoDelete,afaAsk]
          then begin
            if not DeleteFile(CurFilename) then begin
              MessageDlg('Delete file failed',
                'Unable to delete file "'+CurFilename+'".',
                mtError,[mbOk],0);
            end;
          end else if EnvironmentOptions.AmbigiousFileAction=afaAutoRename then
          begin
            Result:=DoBackupFile(CurFilename,false);
            if Result=mrABort then exit;
            Result:=mrOk;
          end;
        end;
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    FindClose(FileInfo);
  end;
end;

function TMainIDE.DoCheckFilesOnDisk: TModalResult;
var
  AnUnitList: TList; // list of TUnitInfo
  i: integer;
  CurUnit: TUnitInfo;
begin
  Result:=mrOk;
  if Project1=nil then exit;
  Project1.GetUnitsChangedOnDisk(AnUnitList);
  if AnUnitList=nil then exit;
  Result:=ShowDiskDiffsDialog(AnUnitList);
  if Result in [mrYesToAll] then
    Result:=mrOk;
  for i:=0 to AnUnitList.Count-1 do begin
    CurUnit:=TUnitInfo(AnUnitList[i]);
    if Result=mrOk then begin
      if CurUnit.EditorIndex>=0 then begin
        Result:=DoOpenEditorFile('',CurUnit.EditorIndex,[ofRevert]);
      end else if CurUnit.IsMainUnit then begin
        Result:=DoRevertMainUnit;
      end else
        Result:=mrIgnore;
      if Result=mrAbort then exit;
    end else begin
      CurUnit.IgnoreCurrentFileDateOnDisk;
    end;
    Result:=mrOk;
  end;
  AnUnitList.Free;
end;

procedure TMainIDE.UpdateCaption;
var NewCaption:string;
begin
  NewCaption := 'Lazarus Editor v'+lisLazarusVersionString;
  if Project1<>nil then begin
    if Project1.Title<>'' then
      NewCaption:=NewCaption +' - '+Project1.Title
    else if Project1.ProjectInfoFile<>'' then
      NewCaption:=NewCaption+' - '+ExtractFileName(Project1.ProjectInfoFile)
    else
      NewCaption:=NewCaption+' - (new project)'
  end;
  Caption:=NewCaption;
end;

procedure TMainIDE.DoBringToFrontFormOrUnit;
begin
  if FDisplayState = dsSource then begin
    DoShowDesignerFormOfCurrentSrc;
  end else begin
    DoShowSourceOfActiveDesignerForm;
  end;
end;

procedure TMainIDE.DoBringToFrontFormOrInspector;
begin
  case FDisplayState of
  
    dsInspector:
      DoShowDesignerFormOfCurrentSrc;

    dsInspector2:
      DoShowSourceOfActiveDesignerForm;

    else begin
      if ObjectInspector1=nil then exit;
      ObjectInspector1.ShowOnTop;
      FDisplayState:= Succ(FDisplayState);
    end;
  end;
end;

procedure TMainIDE.DoShowDesignerFormOfCurrentSrc;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  AForm: TCustomForm;
begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  if (ActiveUnitInfo = nil) then exit;
  // load the form, if not already done
  AForm:=GetFormOfSource(ActiveUnitInfo,true);
  if AForm=nil then exit;
  FDisplayState:= dsForm;
  FLastFormActivated:=AForm;
  AForm.ShowOnTop;
  if TheControlSelection.SelectionForm<>AForm then begin
    // select the new form (object inspector, formeditor, control selection)
    PropertyEditorHook1.LookupRoot := AForm;
    TDesigner(AForm.Designer).SelectOnlyThisComponent(AForm);
  end;
end;

procedure TMainIDE.DoShowSourceOfActiveDesignerForm;
var
  ActiveUnitInfo: TUnitInfo;
begin
  if SourceNoteBook.NoteBook = nil then exit;
  if FLastFormActivated <> nil then begin
    ActiveUnitInfo:= Project1.UnitWithForm(FLastFormActivated);
    if (ActiveUnitInfo <> nil) and (ActiveUnitInfo.EditorIndex >= 0) then
    begin
      SourceNotebook.Notebook.PageIndex:= ActiveUnitInfo.EditorIndex;
    end;
  end;
  SourceNoteBook.ShowOnTop;
  FDisplayState:= dsSource;
end;

procedure TMainIDE.OnMacroSubstitution(TheMacro: TTransferMacro; var s:string;
  var Handled, Abort: boolean);
var MacroName:string;
begin
  if TheMacro=nil then begin
    writeln('WARNING: Macro not defined: "'+s+'".');
    s:='';
    //MessageDlg('Unknown Macro','Macro not defined: "'+s+'".',mtError,[mbAbort],0);
    Handled:=true;
    exit;
  end;
  MacroName:=lowercase(TheMacro.Name);
  if MacroName='save' then begin
    Handled:=true;
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      Abort:=(DoSaveEditorFile(SourceNoteBook.NoteBook.PageIndex,
              [sfCheckAmbigiousFiles])<>mrOk);
    s:='';
  end else if MacroName='saveall' then begin
    Handled:=true;
    Abort:=(DoSaveAll([sfCheckAmbigiousFiles])<>mrOk);
    s:='';
  end else if MacroName='edfile' then begin
    Handled:=true;
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      s:=Project1.UnitWithEditorIndex(SourceNoteBook.NoteBook.PageIndex).Filename
    else
      s:='';
  end else if MacroName='col' then begin
    Handled:=true;
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      s:=IntToStr(SourceNoteBook.GetActiveSE.EditorComponent.CaretX);
  end else if MacroName='row' then begin
    Handled:=true;
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      s:=IntToStr(SourceNoteBook.GetActiveSE.EditorComponent.CaretY);
  end else if MacroName='projfile' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.MainFilename
    else
      s:='';
  end else if MacroName='projpath' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.ProjectDirectory
    else
      s:='';
  end else if MacroName='projunitpath' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.CompilerOptions.GetUnitPath(false)
    else
      s:='';
  end else if MacroName='projincpath' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.CompilerOptions.GetIncludePath(false)
    else
      s:='';
  end else if MacroName='projsrcpath' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.CompilerOptions.GetSrcPath(false)
    else
      s:='';
  end else if MacroName='projpublishdir' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.PublishOptions.DestinationDirectory
    else
      s:='';
  end else if MacroName='curtoken' then begin
    Handled:=true;
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      with SourceNoteBook.GetActiveSE.EditorComponent do
        s:=GetWordAtRowCol(CaretXY);
  end else if MacroName='lazarusdir' then begin
    Handled:=true;
    s:=EnvironmentOptions.LazarusDirectory;
  end else if MacroName='lclwidgettype' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.CompilerOptions.LCLWidgetType
    else
      s:='';
    if s='' then s:='gtk';
  end else if MacroName='fpcsrcdir' then begin
    Handled:=true;
    s:=EnvironmentOptions.FPCSourceDirectory;
  end else if MacroName='comppath' then begin
    Handled:=true;
    s:=EnvironmentOptions.CompilerFilename;
  end else if MacroName='params' then begin
    Handled:=true;
    if Project1<>nil then
      s:=Project1.RunParameterOptions.CmdLineParams
    else
      s:='';
  end else if MacroName='targetfile' then begin
    Handled:=true;
    if Project1<>nil then
      s:=GetProjectTargetFilename
    else
      s:='';
  end else if MacroName='targetcmdline' then begin
    Handled:=true;
    if Project1<>nil then begin
      s:=Project1.RunParameterOptions.CmdLineParams;
      if s='' then
        s:=GetProjectTargetFilename
      else
        s:=GetProjectTargetFilename+' '+s;
    end else
      s:='';
  end else if MacroName='testdir' then begin
    Handled:=true;
    if Project1<>nil then
      s:=GetTestBuildDir
    else
      s:='';
  end else if MacroName='runcmdline' then begin
    Handled:=true;
    if Project1<>nil then
      s:=GetRunCommandLine
    else
      s:='';
  end else if MacroName='projpublishdir' then begin
    Handled:=true;
    if Project1<>nil then
      s:=GetProjPublishDir
    else
      s:='';
  end else if MacroName='confdir' then begin
    Handled:=true;
    s:=GetPrimaryConfigPath;
  end;
end;

function TMainIDE.OnSubstituteCompilerOption(Options: TParsedCompilerOptions;
  const UnparsedValue: string): string;
begin
  CurrentParsedCompilerOption:=Options;
  Result:=UnparsedValue;
  MacroList.SubstituteStr(Result);
end;

function TMainIDE.OnMacroPromptFunction(const s:string;
  var Abort: boolean):string;
begin
  Result:=s;
  Abort:=(ShowMacroPromptDialog(Result)<>mrOk);
end;

procedure TMainIDE.OnCmdLineCreate(var CmdLine: string; var Abort:boolean);
// replace all transfer macros in command line
begin
  Abort:=not MacroList.SubstituteStr(CmdLine);
end;

procedure TMainIDE.GetIDEFileState(Sender: TObject; const AFilename: string;
  NeededFlags: TIDEFileStateFlags; var ResultFlags: TIDEFileStateFlags);
var
  AnUnitInfo: TUnitInfo;
begin
  ResultFlags:=[];
  AnUnitInfo:=Project1.UnitInfoWithFilename(AFilename);
  if AnUnitInfo<>nil then begin
    // readonly
    if (ifsReadOnly in NeededFlags) and AnUnitInfo.ReadOnly then
      Include(ResultFlags,ifsReadOnly);
    // part of project
    if (ifsPartOfProject in NeededFlags) and AnUnitInfo.IsPartOfProject then
      Include(ResultFlags,ifsPartOfProject);
    // open in editor
    if (ifsOpenInEditor in NeededFlags) and (AnUnitInfo.EditorIndex>=0) then
      Include(ResultFlags,ifsOpenInEditor);
  end else if FileExists(AFilename) then begin
    // readonly
    if (ifsReadOnly in NeededFlags) and (not FileIsWritable(AFilename)) then
      Include(ResultFlags,ifsReadOnly);
  end;
end;

function TMainIDE.DoJumpToCompilerMessage(Index:integer;
  FocusEditor: boolean): boolean;
var MaxMessages: integer;
  Filename, Ext, SearchedFilename: string;
  CaretXY: TPoint;
  TopLine: integer;
  MsgType: TErrorType;
  SrcEdit: TSourceEditor;
  OpenFlags: TOpenFlags;
begin
  Result:=false;
  MaxMessages:=MessagesView.MessageView.Items.Count;
  if Index>=MaxMessages then exit;
  if (Index<0) then begin
    // search relevant message (first error, first fatal)
    Index:=0;
    while (Index<MaxMessages) do begin
      if (TheOutputFilter.GetSourcePosition(
        MessagesView.MessageView.Items[Index],
        Filename,CaretXY,MsgType)) then
      begin
        if MsgType in [etError,etFatal,etPanic] then break;
      end;
      inc(Index);
    end;
    if Index>=MaxMessages then exit;
    MessagesView.SelectedMessageIndex:=Index;
  end;
  if TheOutputFilter.GetSourcePosition(MessagesView.MessageView.Items[Index],
        Filename,CaretXY,MsgType) then begin

    OpenFlags:=[ofOnlyIfExists,ofRegularFile];
    if not IsTestUnitFilename(Filename) then
      SearchedFilename := FindUnitFile(Filename)
    else begin
      SearchedFilename := ExtractFileName(Filename);
      Include(OpenFlags,ofVirtualFile);
    end;
    
    if SearchedFilename<>'' then begin
      // open the file in the source editor
      Ext:=lowercase(ExtractFileExt(SearchedFilename));
      if (not FilenameIsFormText(SearchedFilename)) and (Ext<>'.lpi') then begin
        Result:=(DoOpenEditorFile(SearchedFilename,-1,OpenFlags)=mrOk);
        if Result then begin
          // set caret position
          SourceNotebook.AddJumpPointClicked(Self);
          SrcEdit:=SourceNoteBook.GetActiveSE;
          if CaretXY.Y>SrcEdit.EditorComponent.Lines.Count then
            CaretXY.Y:=SrcEdit.EditorComponent.Lines.Count;
          TopLine:=CaretXY.Y-(SrcEdit.EditorComponent.LinesInWindow div 2);
          if TopLine<1 then TopLine:=1;
          if FocusEditor then begin
            //SourceNotebook.BringToFront;
            MessagesView.ShowOnTop;
            SourceNoteBook.ShowOnTop;
            SourceNotebook.FocusEditor;
          end;
          SrcEdit.EditorComponent.CaretXY:=CaretXY;
          SrcEdit.EditorComponent.TopLine:=TopLine;
          with SrcEdit.EditorComponent do begin
            BlockBegin:=CaretXY;
            BlockEnd:=CaretXY;
            LeftChar:=Max(CaretXY.X-CharsInWindow,1);
          end;
          SrcEdit.ErrorLine:=CaretXY.Y;
        end;
      end;
    end else begin
      if FilenameIsAbsolute(Filename) then begin
        MessageDlg('Unable to find file "'+Filename+'".',
           mtInformation,[mbOk],0)
      end else begin
        MessageDlg('Unable to find file "'+Filename+'".'#13
           +'Check search path in'#13
           +'Run->Compiler Options...->Search Paths->Other Unit Files',
           mtInformation,[mbOk],0);
      end;
    end;
  end;
end;

procedure TMainIDE.DoShowMessagesView;
var
  WasVisible: boolean;
  ALayout: TIDEWindowLayout;
begin
  WasVisible:=MessagesView.Visible;
  MessagesView.Visible:=true;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByEnum(nmiwMessagesViewName);
  ALayout.Apply;
  if not WasVisible then
    // the sourcenotebook is more interesting than the messages
    SourceNotebook.ShowOnTop;

  //set the event here for the selectionchanged event
  if not assigned(MessagesView.OnSelectionChanged) then
    MessagesView.OnSelectionChanged := @MessagesViewSelectionChanged;
end;

procedure TMainIDE.DoArrangeSourceEditorAndMessageView(PutOnTop: boolean);
begin
  DoShowMessagesView;

  if (iwpDefault=EnvironmentOptions.IDEWindowLayoutList.ItemByEnum(
                                        nmiwSourceNoteBookName).WindowPlacement)
  and ((SourceNotebook.Top+SourceNotebook.Height) > MessagesView.Top) then
    SourceNotebook.Height := Max(50,Min(SourceNotebook.Height,
       MessagesView.Top-SourceNotebook.Top));
  if PutOnTop then begin
    MessagesView.ShowOnTop;
    SourceNotebook.ShowOnTop;
  end;
end;

function TMainIDE.GetTestBuildDir: string;
begin
  Result:=EnvironmentOptions.TestBuildDirectory;
  if (Result='') then exit;
  Result:=AppendPathDelim(Result);
end;

function TMainIDE.GetProjectTargetFilename: string;
begin
  Result:='';
  if Project1=nil then exit;
  Result:=Project1.RunParameterOptions.HostApplicationFilename;
  if Result='' then begin
    if Project1.IsVirtual then
      Result:=GetTestProjectFilename
    else begin
      if Project1.MainUnitID>=0 then begin
        Result:=
          Project1.CompilerOptions.CreateTargetFilename(Project1.MainFilename)
      end;
    end;
  end;
end;

function TMainIDE.GetTestProjectFilename: string;
begin
  Result:='';
  if (Project1.MainUnitID<0) then exit;
  Result:=GetTestUnitFilename(Project1.MainUnitInfo);
  if Result='' then exit;
  Result:=Project1.CompilerOptions.CreateTargetFilename(Result);
end;

function TMainIDE.GetTestUnitFilename(AnUnitInfo: TUnitInfo): string;
var TestDir: string;
begin
  Result:='';
  if AnUnitInfo=nil then exit;
  TestDir:=GetTestBuildDir;
  if TestDir='' then exit;
  Result:=ExtractFilename(AnUnitInfo.Filename);
  if Result='' then exit;
  Result:=TestDir+Result;
end;

function TMainIDE.GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string;
begin
  if Project1.IsVirtual then
    Result:=GetTestUnitFilename(AnUnitInfo)
  else
    Result:=AnUnitInfo.Filename;
end;

function TMainIDE.IsTestUnitFilename(const AFilename: string): boolean;
var
  TestDir: string;
begin
  Result:=false;
  if Project1.IsVirtual then begin
    TestDir:=GetTestBuildDir;
    Result:=CompareFileNames(TestDir,ExtractFilePath(AFilename))=0;
  end;
end;

function TMainIDE.GetRunCommandLine: string;
begin     
  if Project1.RunParameterOptions.UseLaunchingApplication then
    Result := Project1.RunParameterOptions.LaunchingApplicationPathPlusParams
  else
    Result := '';
  
  if Result='' 
  then begin
    Result:=Project1.RunParameterOptions.CmdLineParams;
    if MacroList.SubstituteStr(Result) then begin
      if Result='' then
        Result:=GetProjectTargetFilename
      else
        Result:=GetProjectTargetFilename+' '+Result;
    end else
      Result:='';
  end else begin
    if not MacroList.SubstituteStr(Result) then Result:='';
  end;
end;

function TMainIDE.GetProjPublishDir: string;
begin
  Result:=Project1.PublishOptions.DestinationDirectory;
  if MacroList.SubstituteStr(Result) then begin
    if FilenameIsAbsolute(Result) then begin
      Result:=AppendPathDelim(TrimFilename(Result));
    end else begin
      Result:='';
    end;
  end else begin
    Result:='';
  end;
end;

function TMainIDE.FindUnitFile(const AFilename: string): string;
var 
  SearchPath, ProjectDir: string;
begin
  ProjectDir:=Project1.ProjectDirectory;
  SearchPath:=CodeToolBoss.DefineTree.GetUnitPathForDirectory(ProjectDir)
            +';'+CodeToolBoss.DefineTree.GetSrcPathForDirectory(ProjectDir);
  Result:=SearchFileInPath(AFilename,ProjectDir,SearchPath,';',[]);
end;

//------------------------------------------------------------------------------

procedure TMainIDE.OnDesignerGetSelectedComponentClass(Sender: TObject; 
  var RegisteredComponent: TRegisteredComponent);
begin
  {$IFDEF EnablePkgs}
  RegisteredComponent:=TComponentPalette(IDEComponentPalette).Selected;
  {$ELSE}
  RegisteredComponent:=SelectedComponent;
  {$ENDIF}
end;

procedure TMainIDE.OnDesignerUnselectComponentClass(Sender: TObject);
begin
  {$IFDEF EnablePkgs}
  TComponentPalette(IDEComponentPalette).Selected:=nil;
  {$ELSE}
  ControlClick(ComponentNoteBook);
  {$ENDIF}
end;

procedure TMainIDE.OnDesignerSetDesigning(Sender: TObject; 
  Component: TComponent;  Value: boolean);
begin
  SetDesigning(Component,Value);
end;

procedure TMainIDE.OnDesignerShowOptions(Sender: TObject);
begin
  DoShowEnvGeneralOptions(eodpFormEditor);
end;

procedure TMainIDE.OnDesignerPropertiesChanged(Sender: TObject);
begin
  ObjectInspector1.RefreshPropertyValues;
end;

procedure TMainIDE.OnDesignerComponentAdded(Sender: TObject;
  AComponent: TComponent; AComponentClass: TRegisteredComponent);
var
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
  FormClassName: string;
begin
  if not (Sender is TDesigner) then begin
    writeln('TMainIDE.OnDesignerComponentAdded ERROR: Sender.ClassName=',
            Sender.ClassName);
    exit;
  end;
  BeginCodeTool(TDesigner(Sender),ActiveSrcEdit,ActiveUnitInfo,
                [ctfSwitchToFormSource]);

  // add needed package to required packages
  {$IFDEF EnablePkgs}
  PkgBoss.AddProjectRegCompDependency(Project1,AComponentClass);
  {$ENDIF}
  // add needed unit to source
  CodeToolBoss.AddUnitToMainUsesSection(ActiveUnitInfo.Source,
      AComponentClass.{$IFDEF EnablePkgs}GetUnitName{$ELSE}UnitName{$ENDIF},'');
  ActiveUnitInfo.Modified:=true;
  // add component definition to form source
  FormClassName:=TDesigner(Sender).Form.ClassName;
  if not CodeToolBoss.PublishedVariableExists(ActiveUnitInfo.Source,
    FormClassName,AComponent.Name) then begin
    // ! AddPublishedVariable does not rebuild the CodeTree, so we need
    // PublishedVariableExists before !
    CodeToolBoss.AddPublishedVariable(ActiveUnitInfo.Source,FormClassName,
      AComponent.Name, AComponent.ClassName);
  end;

  ObjectInspector1.FillComponentComboBox;
end;

procedure TMainIDE.OnDesignerComponentDeleted(Sender: TObject;
  AComponent: TComponent);
var
  CurDesigner: TDesigner;
begin
  CurDesigner:=TDesigner(Sender);
  if dfDestroyingForm in CurDesigner.Flags then exit;
  
  ObjectInspector1.FillComponentComboBox;
end;

procedure TMainIDE.OnDesignerRemoveComponent(Sender: TObject;
  AComponent: TComponent);
var i: integer;
  ActiveForm: TCustomForm;
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
  FormClassName: string;
  CurDesigner: TDesigner;
begin
  CurDesigner:=TDesigner(Sender);
  if dfDestroyingForm in CurDesigner.Flags then exit;
  
  BeginCodeTool(CurDesigner,ActiveSrcEdit,ActiveUnitInfo,
                [ctfSwitchToFormSource]);
  ActiveForm:=CurDesigner.Form;
  if ActiveForm=nil then begin
    RaiseException('[TMainIDE.OnDesignerAddComponent] Error: TDesigner without a form');
  end;
  // find source for form
  i:=Project1.IndexOfUnitWithForm(ActiveForm,false,nil);
  if i<0 then begin
    RaiseException('[TMainIDE.OnDesignerAddComponent] Error: form without source');
  end;
  ActiveUnitInfo:=Project1.Units[i];
  // remove component definition to form source
  FormClassName:=ActiveForm.ClassName;
  CodeToolBoss.RemovePublishedVariable(ActiveUnitInfo.Source,FormClassName,
    AComponent.Name);
end;

procedure TMainIDE.OnDesignerModified(Sender: TObject);
var i: integer;
  SrcEdit: TSourceEditor;
begin
  i:=Project1.IndexOfUnitWithForm(TDesigner(Sender).Form,false,nil);
  if i>=0 then begin
    Project1.Units[i].Modified:=true;
    if Project1.Units[i].Loaded then
      SrcEdit:=SourceNotebook.FindSourceEditorWithPageIndex(
        Project1.Units[i].EditorIndex);
    if SrcEdit<>nil then begin
      SrcEdit.Modified:=true;
      SourceNotebook.UpdateStatusBar;
    end;
  end;
end;

procedure TMainIDE.OnControlSelectionChanged(Sender: TObject);
var
  NewSelectedComponents : TComponentSelectionList;
  i: integer;
begin
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.OnControlSelectionChanged]');
  {$ENDIF}
  if (TheControlSelection=nil) or (FormEditor1=nil) then exit;
  NewSelectedComponents:=TComponentSelectionList.Create;
  for i:=0 to TheControlSelection.Count-1 do
    NewSelectedComponents.Add(TheControlSelection[i].Component);
  FormEditor1.SelectedComponents:=NewSelectedComponents;
  NewSelectedComponents.Free;
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.OnControlSelectionChanged] END');
  {$ENDIF}
end;

procedure TMainIDE.OnControlSelectionPropsChanged(Sender: TObject);
begin
  if (TheControlSelection=nil) or (FormEditor1=nil) then exit;
  ObjectInspector1.RefreshPropertyValues;
end;

procedure TMainIDE.OnControlSelectionFormChanged(Sender: TObject; OldForm,
  NewForm: TCustomForm);
begin
  if (TheControlSelection=nil) or (FormEditor1=nil) then exit;
  if OldForm<>nil then
    OldForm.Invalidate;
  if NewForm<>nil then
    NewForm.Invalidate;
end;


// -----------------------------------------------------------------------------

procedure TMainIDE.UnitDependenciesViewAccessingSources(Sender: TObject);
begin
  SaveSourceEditorChangesToCodeCache(-1);
end;

function TMainIDE.UnitDependenciesViewGetProjectMainFilename(Sender: TObject
  ): string;
begin
  if Project1.MainUnitID>=0 then
    Result:=Project1.MainUnitInfo.Filename;
end;

procedure TMainIDE.UnitDependenciesViewOpenFile(Sender: TObject;
  const Filename: string);
begin
  DoOpenEditorFile(Filename,-1,[]);
end;

procedure TMainIDE.ViewProjectTodosOpenFile(Sender: TObject;
  const Filename: string);
begin
  DoOpenEditorFile(Filename,-1,[]);
end;

// -----------------------------------------------------------------------------

procedure TMainIDE.InitCodeToolBoss;
// initialize the CodeToolBoss, which is the frontend for the codetools.
//  - sets a basic set of compiler macros

  procedure AddTemplate(ADefTempl: TDefineTemplate; AddToPool: boolean; 
    const ErrorMsg: string);
  begin
    if ADefTempl=nil then begin
      writeln('');
      writeln(ErrorMsg);
    end else begin;
      if AddToPool then
        CodeToolBoss.DefinePool.Add(ADefTempl.CreateCopy(false,true,true));
      CodeToolBoss.DefineTree.Add(ADefTempl);
    end;
  end;

var CompilerUnitSearchPath, CompilerUnitLinks: string;
  ADefTempl: TDefineTemplate;
  c: integer;
  AFilename: string;
  UnitLinksChanged: boolean;
begin
  FOpenEditorsOnCodeToolChange:=false;
  
  CodeToolBoss.SourceCache.ExpirationTimeInDays:=365;
  CodeToolBoss.DefineTree.OnGetVirtualDirectoryAlias:=
    @CodeToolBossGetVirtualDirectoryAlias;
  CodeToolBoss.DefineTree.OnGetVirtualDirectoryDefines:=
    @CodeToolBossGetVirtualDirectoryDefines;

  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECT',nil,@MacroFunctionProject);

  CodeToolsOpts.AssignTo(CodeToolBoss);
  if (not FileExists(EnvironmentOptions.CompilerFilename)) then begin
    writeln('');
    writeln('NOTE: Compiler Filename not set! (see Environment Options)');
  end;

  if (EnvironmentOptions.LazarusDirectory='') then begin
    writeln('');
    writeln(
      'NOTE: Lazarus Source Directory not set!  (see Environment Options)');
  end;
  if (EnvironmentOptions.FPCSourceDirectory='') then begin
    writeln('');
    writeln('NOTE: FPC Source Directory not set! (see Environment Options)');
  end;
  
  // set global variables
  with CodeToolBoss.GlobalValues do begin
    Variables[ExternalMacroStart+'LazarusDir']:=
      EnvironmentOptions.LazarusDirectory;
    Variables[ExternalMacroStart+'FPCSrcDir']:=
      EnvironmentOptions.FPCSourceDirectory;
    Variables[ExternalMacroStart+'LCLWidgetType']:='gtk';
    Variables[ExternalMacroStart+'ProjPath']:=VirtualDirectory;
  end;
  
  // build DefinePool and Define Tree
  UpdateEnglishErrorMsgFilename;
  with CodeToolBoss.DefinePool do begin
    // start the compiler and ask for his settings
    ADefTempl:=CreateFPCTemplate(EnvironmentOptions.CompilerFilename,
                       CreateCompilerTestPascalFilename,CompilerUnitSearchPath,
                       CodeToolsOpts);
    AddTemplate(ADefTempl,false,
      'NOTE: Could not create Define Template for Free Pascal Compiler');
      
    // create compiler macros to simulate the Makefiles of the FPC sources
    InputHistories.LastFPCPath:=EnvironmentOptions.CompilerFilename;
    CompilerUnitLinks:=InputHistories.LastFPCUnitLinks;
    UnitLinksChanged:=InputHistories.LastFPCUnitLinksNeedsUpdate(
                                                        CompilerUnitSearchPath);
    ADefTempl:=CreateFPCSrcTemplate(
            CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir'],
            CompilerUnitSearchPath, not UnitLinksChanged,
            CompilerUnitLinks,CodeToolsOpts);
    // save unitlinks
    if UnitLinksChanged
    or (InputHistories.LastFPCUnitLinks<>InputHistories.LastFPCUnitLinks)
    then begin
      InputHistories.SetLastFPCUnitLinks(EnvironmentOptions.CompilerFilename,
                                      CompilerUnitSearchPath,CompilerUnitLinks);
      InputHistories.Save;
    end;
    AddTemplate(ADefTempl,false,
      'NOTE: Could not create Define Template for Free Pascal Sources');
        
    // create compiler macros for the lazarus sources
    ADefTempl:=CreateLazarusSrcTemplate(
      '$('+ExternalMacroStart+'LazarusDir)',
      '$('+ExternalMacroStart+'LCLWidgetType)',
      MiscellaneousOptions.BuildLazOpts.ExtraOptions,CodeToolsOpts);
    AddTemplate(ADefTempl,true,
      'NOTE: Could not create Define Template for Lazarus Sources');
  end;

  // load include file relationships
  AFilename:=AppendPathDelim(GetPrimaryConfigPath)+CodeToolsIncludeLinkFile;
  if FileExists(AFilename) then
    CodeToolBoss.SourceCache.LoadIncludeLinksFromFile(AFilename);

  
  with CodeToolBoss do begin
    WriteExceptions:=true;
    CatchExceptions:=true;
    OnBeforeApplyChanges:=@OnBeforeCodeToolBossApplyChanges;
    OnAfterApplyChanges:=@OnAfterCodeToolBossApplyChanges;
    OnSearchUsedUnit:=@OnCodeToolBossSearchUsedUnit;
  end;
  
  CodeToolsOpts.AssignGlobalDefineTemplatesToTree(CodeToolBoss.DefineTree);
  
  CompilerGraphStampIncreased:=@OnCompilerGraphStampIncreased;

  // codetools consistency check
  c:=CodeToolBoss.ConsistencyCheck;
  if c<>0 then begin
    RaiseException('CodeToolBoss.ConsistencyCheck='+IntToStr(c));
  end;
end;

procedure TMainIDE.UpdateEnglishErrorMsgFilename;
begin
  if EnvironmentOptions.LazarusDirectory<>'' then
    CodeToolBoss.DefinePool.EnglishErrorMsgFilename:=
      AppendPathDelim(EnvironmentOptions.LazarusDirectory)+
      'components'+PathDelim+'codetools'+PathDelim+'fpc.errore.msg';
end;

procedure TMainIDE.ActivateCodeToolAbortableMode;
begin
  if ToolStatus=itNone then
    RaiseException('TMainIDE.ActivateCodeToolAbortableMode Error 1');
  ToolStatus:=itCodeTools;
  CodeToolBoss.OnCheckAbort:=@OnCodeToolBossCheckAbort;
  CodeToolBoss.Abortable:=true;
end;

procedure TMainIDE.OnBeforeCodeToolBossApplyChanges(Manager: TCodeToolManager;
  var Abort: boolean);
// the CodeToolBoss built a list of Sources that will be modified
// 1. open all of them in the source notebook
// 2. lock the editors to reduce repaints and undo steps
var i: integer;
begin
  if FOpenEditorsOnCodeToolChange then begin
    // open all sources in editor
    for i:=0 to Manager.SourceChangeCache.BuffersToModifyCount-1 do begin
      if DoOpenEditorFile(Manager.SourceChangeCache.BuffersToModify[i].Filename,
        -1,[ofOnlyIfExists])<>mrOk then
      begin
        Abort:=true;
        exit;
      end;
    end;
  end;
  // lock all editors
  SourceNoteBook.LockAllEditorsInSourceChangeCache;
end;

procedure TMainIDE.OnAfterCodeToolBossApplyChanges(Manager: TCodeToolManager);
begin
  SourceNoteBook.UnlockAllEditorsInSourceChangeCache;
end;

function TMainIDE.OnCodeToolBossSearchUsedUnit(const SrcFilename: string;
  const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
var
  AnUnitInfo: TUnitInfo;
begin
  Result:=nil;
  // check if SrcFilename is project file
  AnUnitInfo:=Project1.ProjectUnitWithFilename(SrcFilename);
  if AnUnitInfo=nil then exit;
  // SrcFilename is a project file
  // -> search virtual project files
  AnUnitInfo:=Project1.ProjectUnitWithUnitname(TheUnitName);
  if AnUnitInfo=nil then exit;
  // virtual unit found
  Result:=AnUnitInfo.Source;
end;

function TMainIDE.OnCodeToolBossCheckAbort: boolean;
begin
  Result:=true;
  if ToolStatus<>itCodeTools then exit;
  Application.ProcessMessages;
  Result:=ToolStatus<>itCodeTools;
end;

procedure TMainIDE.CodeToolBossGetVirtualDirectoryAlias(Sender: TObject;
  var RealDir: string);
begin
  if (Project1<>nil) and (Project1.ProjectDirectory<>'') then
    RealDir:=Project1.ProjectDirectory;
end;

procedure TMainIDE.CodeToolBossGetVirtualDirectoryDefines(DefTree: TDefineTree;
  DirDef: TDirectoryDefines);
begin
  if (Project1<>nil) and Project1.IsVirtual then
    Project1.GetVirtualDefines(DefTree,DirDef);
end;

function TMainIDE.MacroFunctionProject(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  Param: String;
begin
  Result:=true;
  if Project1=nil then exit;
  FuncData:=PReadFunctionData(Data);
  Param:=FuncData^.Param;
  if AnsiCompareText(Param,'SrcPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetSrcPath(false)
  else if AnsiCompareText(Param,'IncPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetIncludePath(false)
  else if AnsiCompareText(Param,'UnitPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetUnitPath(false)
  else
    FuncData^.Result:='';
end;

procedure TMainIDE.OnCompilerGraphStampIncreased;
begin
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TMainIDE.SaveSourceEditorChangesToCodeCache(PageIndex: integer);
// save all open sources to code tools cache
var i: integer;

  procedure SaveChanges(APageIndex: integer);
  var
    SrcEdit: TSourceEditor;
    AnUnitInfo: TUnitInfo;
  begin
    GetUnitWithPageIndex(APageIndex,SrcEdit,AnUnitInfo);
    if (SrcEdit<>nil) and (AnUnitInfo<>nil) and (SrcEdit.Modified) then begin
      SrcEdit.UpdateCodeBuffer;
      AnUnitInfo.Modified:=true;
    end;
  end;
  
begin
  if PageIndex<0 then begin
    if (SourceNotebook.NoteBook<>nil) then begin
      for i:=0 to SourceNotebook.NoteBook.PageCount-1 do
        SaveChanges(i);
    end;
  end else begin
    SaveChanges(PageIndex);
  end;
end;

function TMainIDE.BeginCodeTool(var ActiveSrcEdit: TSourceEditor;
  var ActiveUnitInfo: TUnitInfo; Flags: TCodeToolsFlags): boolean;
begin
  Result:=BeginCodeTool(nil,ActiveSrcEdit,ActiveUnitInfo,Flags);
end;

function TMainIDE.BeginCodeTool(ADesigner: TDesigner;
  var ActiveSrcEdit: TSourceEditor; var ActiveUnitInfo: TUnitInfo;
  Flags: TCodeToolsFlags): boolean;
begin
  Result:=false;
  if (SourceNoteBook.NoteBook=nil)
  or (ToolStatus in [itCodeTools,itCodeToolAborting]) then exit;
  if ctfSwitchToFormSource in Flags then
    DoSwitchToFormSrc(ADesigner,ActiveSrcEdit,ActiveUnitInfo)
  else if Designer<>nil then
    GetDesignerUnit(ADesigner,ActiveSrcEdit,ActiveUnitInfo)
  else
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  SaveSourceEditorChangesToCodeCache(-1);
  CodeToolBoss.VisibleEditorLines:=ActiveSrcEdit.EditorComponent.LinesInWindow;
  
  if ctfActivateAbortMode in Flags then
    ActivateCodeToolAbortableMode;

  Result:=true;
end;

function TMainIDE.DoJumpToCodePos(ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
  AddJumpPoint: boolean): TModalResult;
var NewSrcEdit: TSourceEditor;
  NewUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
  if AddJumpPoint then begin
    if (NewSource<>ActiveUnitInfo.Source)
    or (ActiveSrcEdit.EditorComponent.CaretX<>NewX)
    or (ActiveSrcEdit.EditorComponent.CaretY<>NewY) then
      SourceNotebook.AddJumpPointClicked(Self);
  end;
  if NewSource<>ActiveUnitInfo.Source then begin
    // jump to other file -> open it
    Result:=DoOpenEditorFile(NewSource.Filename,-1,[ofOnlyIfExists]);
    if Result<>mrOk then begin
      UpdateSourceNames;
      exit;
    end;
    GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,NewSrcEdit,
      NewUnitInfo);
  end else begin
    NewSrcEdit:=ActiveSrcEdit;
  end;
  //writeln('[TMainIDE.DoJumpToCodePos] ',NewX,',',NewY,',',NewTopLine);
  with NewSrcEdit.EditorComponent do begin
    CaretXY:=Point(NewX,NewY);
    BlockBegin:=CaretXY;
    BlockEnd:=CaretXY;
    TopLine:=NewTopLine;
    LeftChar:=Max(NewX-CharsInWindow,1);
  end;
  SourceNoteBook.ShowOnTop;
  SourceNotebook.FocusEditor;
  UpdateSourceNames;
  Result:=mrOk;
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.UpdateSourceNames
  Params: none

  Check every unit in sourceeditor if the source name has changed and updates
  the notebook page names.
-------------------------------------------------------------------------------}
procedure TMainIDE.UpdateSourceNames;
var
  PageIndex: integer;
  AnUnitInfo: TUnitInfo;
  SourceName, PageName: string;
begin
  if SourceNotebook.NoteBook=nil then exit;
  for PageIndex:=0 to SourceNotebook.NoteBook.PageCount-1 do begin
    AnUnitInfo:=Project1.UnitWithEditorIndex(PageIndex);
    if AnUnitInfo=nil then continue;
    if FilenameIsPascalUnit(AnUnitInfo.Filename) then begin
      SourceName:=CodeToolBoss.GetCachedSourceName(AnUnitInfo.Source);
      if SourceName<>'' then
        AnUnitInfo.UnitName:=SourceName;
    end else
      SourceName:='';
    PageName:=CreateSrcEditPageName(SourceName,AnUnitInfo.Filename,PageIndex);
    SourceNotebook.FindSourceEditorWithPageIndex(PageIndex).PageName:=PageName;
  end;
end;

procedure TMainIDE.ApplyCodeToolChanges;
begin
  // all changes were handled automatically by events
  // just clear the logs
  CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
end;

procedure TMainIDE.DoJumpToProcedureSection;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  RevertableJump: boolean;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoJumpToProcedureSection] ************');
  {$ENDIF}
  if CodeToolBoss.JumpToMethod(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine,RevertableJump) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo, 
      NewSource, NewX, NewY, NewTopLine, not RevertableJump);
  end else
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.DoJumpToCodeToolBossError;
var
  ActiveSrcEdit:TSourceEditor;
  ErrorCaret: TPoint;
begin
  if CodeToolBoss.ErrorMessage='' then begin
    UpdateSourceNames;
    exit;
  end;
  // syntax error -> show error and jump
  // show error in message view
  DoArrangeSourceEditorAndMessageView(false);
  MessagesView.ClearTillLastSeparator;
  MessagesView.AddSeparator;
  if CodeToolBoss.ErrorCode<>nil then begin
    MessagesView.Add(Project1.RemoveProjectPathFromFilename(
       CodeToolBoss.ErrorCode.Filename)
      +'('+IntToStr(CodeToolBoss.ErrorLine)
      +','+IntToStr(CodeToolBoss.ErrorColumn)
      +') Error: '+CodeToolBoss.ErrorMessage);
  end else
    MessagesView.Add(CodeToolBoss.ErrorMessage);
  MessagesView.SelectedMessageIndex:=MessagesView.MsgCount-1;

  // jump to error in source editor
  if CodeToolBoss.ErrorCode<>nil then begin
    SourceNotebook.AddJumpPointClicked(Self);
    ErrorCaret:=Point(CodeToolBoss.ErrorColumn,CodeToolBoss.ErrorLine);
    if DoOpenEditorFile(CodeToolBoss.ErrorCode.Filename,-1,
      [ofOnlyIfExists,ofUseCache])=mrOk
    then begin
      ActiveSrcEdit:=SourceNoteBook.GetActiveSE;
      MessagesView.ShowOnTop;
      SourceNoteBook.ShowOnTop;
      with ActiveSrcEdit.EditorComponent do begin
        CaretXY:=ErrorCaret;
        BlockBegin:=CaretXY;
        BlockEnd:=CaretXY;
        if CodeToolBoss.ErrorTopLine>0 then
          TopLine:=CodeToolBoss.ErrorTopLine;
      end;
      SourceNotebook.FocusEditor;
      SourceNotebook.ClearErrorLines;
      ActiveSrcEdit.ErrorLine:=ErrorCaret.Y;
    end;
  end;
  UpdateSourceNames;
end;

procedure TMainIDE.DoFindDeclarationAtCursor;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if ActiveSrcEdit=nil then exit;
  DoFindDeclarationAtCaret(ActiveSrcEdit.EditorComponent.CaretXY);
end;

procedure TMainIDE.DoFindDeclarationAtCaret(CaretXY: TPoint);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoFindDeclarationAtCaret] ************');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoFindDeclarationAtCaret A');{$ENDIF}
  if CodeToolBoss.FindDeclaration(ActiveUnitInfo.Source,
    CaretXY.X,CaretXY.Y,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, true);
  end else begin
    DoJumpToCodeToolBossError;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoFindDeclarationAtCaret B');{$ENDIF}
end;

function TMainIDE.DoInitIdentCompletion: boolean;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  CaretXY: TPoint;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoInitIdentCompletion] ************');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoInitIdentCompletion A');{$ENDIF}
  CaretXY:=ActiveSrcEdit.EditorComponent.CaretXY;
  Result:=CodeToolBoss.GatherIdentifiers(ActiveUnitInfo.Source,
                                         CaretXY.X,CaretXY.Y);
  if not Result then begin
    DoJumpToCodeToolBossError;
    exit;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoInitIdentCompletion B');{$ENDIF}
end;

procedure TMainIDE.DoGoToPascalBlockOtherEnd;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoGoToPascalBlockOtherEnd] ************');
  {$ENDIF}
  if CodeToolBoss.FindBlockCounterPart(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo, 
      NewSource, NewX, NewY, NewTopLine, false);
  end else 
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.DoGoToPascalBlockStart;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoGoToPascalBlockStart] ************');
  {$ENDIF}
  if CodeToolBoss.FindBlockStart(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, false);
  end else
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.DoJumpToGuessedUnclosedBlock(FindNext: boolean);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  StartX, StartY, NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoJumpToGuessedUnclosedBlock] ************');
  {$ENDIF}
  if FindNext then begin
    StartX:=ActiveSrcEdit.EditorComponent.CaretX;
    StartY:=ActiveSrcEdit.EditorComponent.CaretY;
  end else begin
    StartX:=1;
    StartY:=1;
  end;
  if CodeToolBoss.GuessUnclosedBlock(ActiveUnitInfo.Source,
    StartX,StartY,NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo, 
      NewSource, NewX, NewY, NewTopLine, true);
  end else 
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.DoJumpToGuessedMisplacedIFDEF(FindNext: boolean);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  StartX, StartY, NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoJumpToGuessedMisplacedIFDEF] ************');
  {$ENDIF}
  if FindNext then begin
    StartX:=ActiveSrcEdit.EditorComponent.CaretX;
    StartY:=ActiveSrcEdit.EditorComponent.CaretY;
  end else begin
    StartX:=1;
    StartY:=1;
  end;
  if CodeToolBoss.GuessMisplacedIfdefEndif(ActiveUnitInfo.Source,
    StartX,StartY,NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, true);
  end else
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.DoGotoIncludeDirective;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoGotoIncludeDirective] ************');
  {$ENDIF}
  if CodeToolBoss.FindEnclosingIncludeDirective(ActiveUnitInfo.Source,
    ActiveSrcEdit.EditorComponent.CaretX,
    ActiveSrcEdit.EditorComponent.CaretY,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, false);
  end else
    DoJumpToCodeToolBossError;
end;

procedure TMainIDE.SaveIncludeLinks;
var AFilename: string;
begin
  // save include file relationships
  AFilename:=AppendPathDelim(GetPrimaryConfigPath)+CodeToolsIncludeLinkFile;
  CodeToolBoss.SourceCache.SaveIncludeLinksToFile(AFilename,true);
end;

function TMainIDE.DoMakeResourceString: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  StartPos, EndPos: TPoint;
  StartCode, EndCode: TCodeBuffer;
  NewIdentifier, NewIdentValue: string;
  NewSourceLines: string;
  InsertPolicy: TResourcestringInsertPolicy;
  SectionCode: TCodeBuffer;
  SectionCaretXY: TPoint;
  DummyResult: Boolean;
  SelectedStartPos: TPoint;
  SelectedEndPos: TPoint;
  CursorCode: TCodeBuffer;
  CursorXY: TPoint;
begin
  FOpenEditorsOnCodeToolChange:=true;
  try
    Result:=mrCancel;
    if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.DoMakeResourceString] ************');
    {$ENDIF}
    // calculate start and end of expression in source
    CursorCode:=ActiveUnitInfo.Source;
    CursorXY:=ActiveSrcEdit.EditorComponent.CaretXY;
    if CodeToolBoss.GetStringConstBounds(
      CursorCode,CursorXY.X,CursorXY.Y,
      StartCode,StartPos.X,StartPos.Y,
      EndCode,EndPos.X,EndPos.Y,
      true) then
    begin
      // the codetools have calculated the maximum bounds
      if (StartCode=EndCode) and (CompareCaret(StartPos,EndPos)=0) then begin
        MessageDlg('No String Constant Found',
        +'Hint: The Make Resourcestring Function expects a string constant.'#13
        +'Please select the expression and try again.',
        mtError,[mbCancel],0);
        exit;
      end;
      // the user can shorten this range by selecting text
      if (Trim(ActiveSrcEdit.EditorComponent.SelText)='') then begin
        // the user has not selected text
        // -> check if the string constant is in single file
        // (replacing code that contains an $include directive is ambigious)
        if (StartCode<>ActiveUnitInfo.Source)
        or (EndCode<>ActiveUnitInfo.Source)
        then begin
          MessageDlg('No String Constant Found','Invalid expression.'#13
          +'Hint: The Make Resourcestring Function expects a string constant'
          +' in a single file. Please select the expression and try again.',
          mtError,[mbCancel],0);
          exit;
        end;
      end else begin
        // the user has selected text
        // -> check if the selection is only part of the maximum bounds
        SelectedStartPos:=ActiveSrcEdit.EditorComponent.BlockBegin;
        SelectedEndPos:=ActiveSrcEdit.EditorComponent.BlockEnd;
        if (CompareCaret(SelectedStartPos,StartPos)>0)
        or (CompareCaret(SelectedEndPos,EndPos)<0)
        then begin
          MessageDlg('Selection exceeds string constant',
          'Hint: The Make Resourcestring Function expects a string constant.'#13
          +'Please select only a string expression and try again.',
          mtError,[mbCancel],0);
          exit;
        end;
        StartPos:=SelectedStartPos;
        EndPos:=SelectedEndPos;
      end;
    end else begin
      DoJumpToCodeToolBossError;
      exit;
    end;

    // gather all reachable resourcestring sections
    if not CodeToolBoss.GatherResourceStringSections(
      CursorCode,CursorXY.X,CursorXY.Y,nil)
    then begin
      DoJumpToCodeToolBossError;
      exit;
    end;
    if CodeToolBoss.Positions.Count=0 then begin
      MessageDlg('No ResourceString Section found',
        'Unable to find a ResourceString section in this '
        +'or any of the used units.',
        mtError,[mbCancel],0);
      exit;
    end;

    // show make resourcestring dialog
    Result:=ShowMakeResStrDialog(StartPos,EndPos,StartCode,
                                 CodeToolBoss.Positions,
                                 NewIdentifier,NewIdentValue,NewSourceLines,
                                 SectionCode,SectionCaretXY,InsertPolicy);
    if (Result<>mrOk) then exit;

    // replace source
    ActiveSrcEdit.ReplaceLines(StartPos.Y,EndPos.Y,NewSourceLines);

    // add new resourcestring to resourcestring section
    if (InsertPolicy<>rsipNone) then
      DummyResult:=CodeToolBoss.AddResourcestring(
                       CursorCode,CursorXY.X,CursorXY.Y,
                       SectionCode,SectionCaretXY.X,SectionCaretXY.Y,
                       NewIdentifier,''''+NewIdentValue+'''',InsertPolicy)
    else
      DummyResult:=true;
    ApplyCodeToolChanges;
    if not DummyResult then begin
      DoJumpToCodeToolBossError;
      exit;
    end;
    
    // switch back to source
    ActiveSrcEdit.Activate;

    Result:=mrOk;
  finally
    FOpenEditorsOnCodeToolChange:=false;
  end;
end;

function TMainIDE.DoDiff: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  OpenDiffInEditor: boolean;
  DiffText: string;
  Files: TDiffFiles;
  NewDiffFilename: String;
begin
  Result:=mrCancel;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if ActiveSrcEdit=nil then exit;
  
  Files:=SourceNoteBook.GetDiffFiles;
  Result:=ShowDiffDialog(Files,ActiveSrcEdit.PageIndex,
                         @SourceNotebook.GetSourceText,
                         OpenDiffInEditor,DiffText);
  Files.Free;
  if OpenDiffInEditor then begin
    NewDiffFilename:=CreateSrcEditPageName('','diff.txt',-1);
    Result:=DoNewEditorFile(nuText,NewDiffFilename,DiffText,[nfOpenInEditor]);
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
    if ActiveSrcEdit=nil then exit;
  end;
end;

procedure TMainIDE.DoCompleteCodeAtCursor;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  FOpenEditorsOnCodeToolChange:=true;
  try
    if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.DoCompleteCodeAtCursor] ************');
    {$ENDIF}
    if CodeToolBoss.CompleteCode(ActiveUnitInfo.Source,
      ActiveSrcEdit.EditorComponent.CaretX,
      ActiveSrcEdit.EditorComponent.CaretY,
      ActiveSrcEdit.EditorComponent.TopLine,
      NewSource,NewX,NewY,NewTopLine) then
    begin
      ApplyCodeToolChanges;
      DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
        NewSource, NewX, NewY, NewTopLine, true);
    end else begin
      // error: probably a syntax error or just not in a procedure head/body
      // or not in a class
      // -> there are enough events to handle everything, so it can be ignored here
      ApplyCodeToolChanges;
      DoJumpToCodeToolBossError;
    end;
  finally  
    FOpenEditorsOnCodeToolChange:=false;
  end;
end;

//-----------------------------------------------------------------------------

procedure TMainIDE.MessagesViewSelectionChanged(sender : TObject);
begin
  DoJumpToCompilerMessage(TMessagesView(sender).SelectedMessageIndex,True);
end;

Procedure TMainIDE.OnSrcNotebookEditorVisibleChanged(Sender : TObject);
var
  ActiveUnitInfo : TUnitInfo;
begin
  if SourceNotebook.Notebook = nil then Exit;
  
  ActiveUnitInfo :=
    Project1.UnitWithEditorIndex(SourceNotebook.Notebook.PageIndex);
  if ActiveUnitInfo = nil then Exit;

  SaveSpeedBtn.Enabled := SourceNotebook.GetActiveSe.Modified;
  ToggleFormSpeedBtn.Enabled := Assigned(ActiveUnitInfo.Form)
                                or (ActiveUnitInfo.FormName<>'');
end;

//this is fired when the editor is focused, changed, ?.  Anything that causes the status change
Procedure TMainIDE.OnSrcNotebookEditorChanged(Sender : TObject);
begin
  if SourceNotebook.Notebook = nil then Exit;
  SaveSpeedBtn.Enabled := SourceNotebook.GetActiveSE.Modified;
end;

procedure TMainIDE.OnSrcNotebookShowHintForSource(SrcEdit: TSourceEditor;
  ClientPos: TPoint; CaretPos: TPoint);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  Identifier, SmartHintStr: string;
  DebugEval: string;
begin
  if (SrcEdit=nil) then exit;
  if (ToolStatus<>itNone) and (ToolStatus<>itDebugger) then exit;
  // check if there is an identifier
  Identifier:=SrcEdit.GetWordFromCaret(CaretPos);
  if (Identifier='') or (not IsValidIdent(Identifier)) then exit;
  SourceNotebook.SetActiveSE(SrcEdit);

  if not BeginCodeTool(ActiveSrcEdit, ActiveUnitInfo,
    [{ctfActivateAbortMode}]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnSrcNotebookShowHintForSource] ************ ',ActiveUnitInfo.Source.Filename,' X=',CaretPos.X,' Y=',CaretPos.Y);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.OnSrcNotebookShowHintForSource A');{$ENDIF}
  SmartHintStr:=CodeToolBoss.FindSmartHint(ActiveUnitInfo.Source,
    CaretPos.X,CaretPos.Y);
  CodeToolBoss.Abortable:=false;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.OnSrcNotebookShowHintForSource B');{$ENDIF}

  if (ToolStatus=itDebugger)
  and DebugBoss.Evaluate(Identifier,DebugEval) then begin
    if (DebugEval<>'') then
      SmartHintStr:=SmartHintStr+' = '+DebugEval;
  end;

  if SmartHintStr<>'' then
    SrcEdit.ActivateHint(ClientPos,SmartHintStr);
end;

procedure TMainIDE.OnSrcNoteBookActivated(Sender : TObject);
begin
  FDisplayState:= dsSource;
  DoCheckFilesOnDisk;
end;

Procedure TMainIDE.OnDesignerActivated(Sender : TObject);
begin
  FDisplayState:= dsForm;
  FLastFormActivated := TDesigner(Sender).Form;
end;

procedure TMainIDE.OnDesignerRenameComponent(ADesigner: TDesigner;
  AComponent: TComponent; const NewName: string);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  i: integer;
  NewClassName: string;
  BossResult: boolean;
  
  procedure ApplyBossResult(const Msg: string);
  begin
    ApplyCodeToolChanges;
    if not BossResult then begin
      DoJumpToCodeToolBossError;
      raise Exception.Create(Msg);
    end;
  end;
  
begin
  if (not IsValidIdent(NewName)) or (NewName='') then
    raise Exception.Create('Component name "'+Newname+'" is not a valid identifier');
  BeginCodeTool(ADesigner,ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource]);
  ActiveUnitInfo:=Project1.UnitWithForm(ADesigner.Form);
  if CodeToolBoss.IsKeyWord(ActiveUnitInfo.Source,NewName) then
    raise Exception.Create('Component name "'+Newname+'" is keyword');
  if AComponent.Owner<>nil then begin
    // rename published variable in form source
    BossResult:=CodeToolBoss.RenamePublishedVariable(ActiveUnitInfo.Source,
      ADesigner.Form.ClassName,
      AComponent.Name,NewName,AComponent.ClassName);
    ApplyBossResult('Unable to rename variable in source.'#13
                    +'See messages.');
  end else if AComponent=ADesigner.Form then begin
    // rename form
    // ToDo:
    //   rename form in source
    //   rename formclass
    //   replace createform statement

    // check if formname already exists
    i:=Project1.IndexOfUnitWithFormName(NewName,true,ActiveUnitInfo);
    if i>=0 then
      raise Exception.Create(
                         'There is already a form with the name "'+Newname+'"');
    NewClassName:='T'+NewName;

    // rename form in source
    BossResult:=CodeToolBoss.RenameForm(ActiveUnitInfo.Source,
      AComponent.Name,AComponent.ClassName,
      NewName,NewClassName);
    ApplyBossResult('Unable to rename form in source.'#13
                    +'See messages.');
    ActiveUnitInfo.FormName:=NewName;

    // rename form class
    FormEditor1.JITFormList.RenameFormClass(TForm(AComponent),NewClassName);

    // change createform statement
    if ActiveUnitInfo.IsPartOfProject and (Project1.MainUnitID>=0)
    then begin
      BossResult:=CodeToolBoss.ChangeCreateFormStatement(
        Project1.MainUnitInfo.Source,
        AComponent.ClassName,AComponent.Name,
        NewClassName,NewName,true);
      ApplyCodeToolChanges;
      if not BossResult then begin
        DoJumpToCodeToolBossError;
        // don't raise an exception
      end;
    end;
    
  end else begin
    if (aComponent is TMenuItem) or (aComponent is TMenu)
    then
      writeln ('**SH: Warn: TMainIDE.OnDesignerRenameComponent MenuItem / TMenu with Owner = nil'+self.Name)
    else
      RaiseException('TMainIDE.OnDesignerRenameComponent internal error:'+AComponent.Name);
  end;
end;

Procedure TMainIDE.OnSrcNoteBookAddJumpPoint(ACaretXY: TPoint;
  ATopLine: integer; APageIndex: integer; DeleteForwardHistory: boolean);
{off $DEFINE VerboseJumpHistory}
var
  ActiveUnitInfo: TUnitInfo;
  NewJumpPoint: TProjectJumpHistoryPosition;
begin
  {$IFDEF VerboseJumpHistory}
  writeln('');
  writeln('[TMainIDE.OnSrcNoteBookAddJumpPoint] A Line=',ACaretXY.Y,' Col=',ACaretXY.X,' DeleteForwardHistory=',DeleteForwardHistory,' Count=',Project1.JumpHistory.Count,',HistoryIndex=',Project1.JumpHistory.HistoryIndex);
  {$ENDIF}
  ActiveUnitInfo:=Project1.UnitWithEditorIndex(APageIndex);
  if (ActiveUnitInfo=nil) then exit;
  NewJumpPoint:=TProjectJumpHistoryPosition.Create(ActiveUnitInfo.Filename,
    ACaretXY,ATopLine);
  {$IFDEF VerboseJumpHistory}
  //Project1.JumpHistory.WriteDebugReport;
  {$ENDIF}
  Project1.JumpHistory.InsertSmart(Project1.JumpHistory.HistoryIndex+1,
                                   NewJumpPoint);
  {$IFDEF VerboseJumpHistory}
  writeln('[TMainIDE.OnSrcNoteBookAddJumpPoint] B INSERTED');
  Project1.JumpHistory.WriteDebugReport;
  {$ENDIF}
  if DeleteForwardHistory then Project1.JumpHistory.DeleteForwardHistory;
  {$IFDEF VerboseJumpHistory}
  writeln('[TMainIDE.OnSrcNoteBookAddJumpPoint] END Line=',ACaretXY.Y,',DeleteForwardHistory=',DeleteForwardHistory,' Count=',Project1.JumpHistory.Count,',HistoryIndex=',Project1.JumpHistory.HistoryIndex);
  Project1.JumpHistory.WriteDebugReport;
  {$ENDIF}
end;

Procedure TMainIDE.OnSrcNotebookDeleteLastJumPoint(Sender: TObject);
begin
  Project1.JumpHistory.DeleteLast;
end;

Procedure TMainIDE.OnSrcNotebookJumpToHistoryPoint(var NewCaretXY: TPoint;
  var NewTopLine, NewPageIndex: integer;  Action: TJumpHistoryAction);
{ How the HistoryIndex works:

  When the user jumps around each time an item is added to the history list
  and the HistoryIndex points to the last added item (i.e. Count-1).
  
  Jumping back:
    The sourceditor will be repositioned to the item with the HistoryIndex.
    Then the historyindex is moved to the previous item.
    If HistoryIndex is the last item in the history, then this is the first
    back jump and the current sourceeditor position is smart added to the
    history list. Smart means that if the added Item is similar to the last
    item then the last item will be replaced else a new item is added.

  Jumping forward:

}
var DestIndex, UnitIndex: integer;
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
  DestJumpPoint: TProjectJumpHistoryPosition;
  CursorPoint, NewJumpPoint: TProjectJumpHistoryPosition;
begin
  NewPageIndex:=-1;
  NewCaretXY.Y:=-1;
  
  {$IFDEF VerboseJumpHistory}
  writeln('');
  writeln('[TMainIDE.OnSrcNotebookJumpToHistoryPoint] A Back=',Action=jhaBack);
  Project1.JumpHistory.WriteDebugReport;
  {$ENDIF}

  // update jump history (e.g. delete jumps to closed editors)
  Project1.JumpHistory.DeleteInvalidPositions;

  // get destination jump point
  DestIndex:=Project1.JumpHistory.HistoryIndex;
  if Action=jhaForward then
    inc(DestIndex);
  if (DestIndex<0) or (DestIndex>=Project1.JumpHistory.Count) then exit;

  CursorPoint:=nil;
  if (SourceNoteBook<>nil) then begin
    // get current cursor position
    GetCurrentUnit(ASrcEdit,AnUnitInfo);
    if (ASrcEdit<>nil) and (AnUnitInfo<>nil) then begin
      CursorPoint:=TProjectJumpHistoryPosition.Create(AnUnitInfo.Filename,
        ASrcEdit.EditorComponent.CaretXY,ASrcEdit.EditorComponent.TopLine);
      {$IFDEF VerboseJumpHistory}
      writeln('  Current Position: ',CursorPoint.Filename,
              ' ',CursorPoint.CaretXY.X,',',CursorPoint.CaretXY.Y);
      {$ENDIF}
    end;
  end;

  if (Action=jhaBack) and (Project1.JumpHistory.Count=DestIndex+1)
  and (CursorPoint<>nil) then begin
    // this is the first back jump
    // -> insert current source position into history
    {$IFDEF VerboseJumpHistory}
    writeln('  First back jump -> add current cursor position');
    {$ENDIF}
    NewJumpPoint:=TProjectJumpHistoryPosition.Create(CursorPoint);
    Project1.JumpHistory.InsertSmart(Project1.JumpHistory.HistoryIndex+1,
                                     NewJumpPoint);
  end;
  
  // find the next jump point that is not where the cursor is
  DestIndex:=Project1.JumpHistory.HistoryIndex;
  if Action=jhaForward then
    inc(DestIndex);
  while (DestIndex>=0) and (DestIndex<Project1.JumpHistory.Count) do begin
    DestJumpPoint:=Project1.JumpHistory[DestIndex];
    UnitIndex:=Project1.IndexOfFilename(DestJumpPoint.Filename);
    {$IFDEF VerboseJumpHistory}
    writeln(' DestIndex=',DestIndex,' UnitIndex=',UnitIndex);
    {$ENDIF}
    if (UnitIndex>=0) and (Project1.Units[UnitIndex].EditorIndex>=0)
    and ((CursorPoint=nil) or not DestJumpPoint.IsSimilar(CursorPoint)) then
    begin
      if Action=jhaBack then
        dec(DestIndex);
      Project1.JumpHistory.HistoryIndex:=DestIndex;
      NewCaretXY:=DestJumpPoint.CaretXY;
      NewTopLine:=DestJumpPoint.TopLine;
      NewPageIndex:=Project1.Units[UnitIndex].EditorIndex;
      {$IFDEF VerboseJumpHistory}
      writeln('[TMainIDE.OnSrcNotebookJumpToHistoryPoint] Result Line=',NewCaretXY.Y,' Col=',NewCaretXY.X);
      {$ENDIF}
      break;
    end;
    if Action=jhaBack then
      dec(DestIndex)
    else
      inc(DestIndex);
  end;
  
  CursorPoint.Free;

  {$IFDEF VerboseJumpHistory}
  writeln('[TMainIDE.OnSrcNotebookJumpToHistoryPoint] END Count=',Project1.JumpHistory.Count,',HistoryIndex=',Project1.JumpHistory.HistoryIndex);
  Project1.JumpHistory.WriteDebugReport;
  writeln('');
  {$ENDIF}
end;

procedure TMainIDE.OnSrcNotebookMovingPage(Sender: TObject; OldPageIndex,
  NewPageIndex: integer);
begin
  Project1.MoveEditorIndex(OldPageIndex,NewPageIndex);
end;

procedure TMainIDE.OnSrcNotebookReadOnlyChanged(Sender: TObject);
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  ActiveUnitInfo.UserReadOnly:=ActiveSourceEditor.ReadOnly;
end;

Procedure TMainIDE.OnSrcNotebookViewJumpHistory(Sender : TObject);
begin
  // ToDo
  MessageDlg('Not implemented yet','Sorry, not implemented yet',mtInformation,
     [mbOk],0);
end;

procedure TMainIDE.OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
begin
  if ToolStatus=itCodeTools then begin
    // abort codetools
    ToolStatus:=itCodeToolAborting;
  end;
end;

procedure TMainIDE.OnApplicationIdle(Sender: TObject);
begin
  UpdateWindowsMenu;
  GetDefaultProcessList.FreeStoppedProcesses;
  EnvironmentOptions.ExternalTools.FreeStoppedProcesses;
end;

function TMainIDE.ProjInspectorAddUnitToProject(Sender: TObject;
  AnUnitInfo: TUnitInfo): TModalresult;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  ShortUnitName: String;
  Dummy: Boolean;
begin
  Result:=mrOk;
  BeginCodeTool(ActiveSourceEditor,ActiveUnitInfo,[]);
  AnUnitInfo.IsPartOfProject:=true;
  if FilenameIsPascalUnit(AnUnitInfo.Filename)
  and (Project1.ProjectType in [ptProgram, ptApplication]) then begin
    AnUnitInfo.ReadUnitNameFromSource;
    ShortUnitName:=AnUnitInfo.UnitName;
    if (ShortUnitName<>'') then begin
      Dummy:=CodeToolBoss.AddUnitToMainUsesSection(
         Project1.MainUnitInfo.Source,ShortUnitName,'');
      ApplyCodeToolChanges;
      if Dummy then begin
        Project1.MainUnitInfo.Modified:=true;
      end else begin
        DoJumpToCodeToolBossError;
        Result:=mrCancel;
      end;
    end;
  end;
  Project1.Modified:=true;
end;

function TMainIDE.ProjInspectorRemoveFile(Sender: TObject; AnUnitInfo: TUnitInfo
  ): TModalresult;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  ShortUnitName: String;
  Dummy: Boolean;
begin
  Result:=mrOk;
  AnUnitInfo.IsPartOfProject:=false;
  if (Project1.MainUnitID>=0)
  and (Project1.ProjectType in [ptProgram, ptApplication]) then begin
    BeginCodeTool(ActiveSourceEditor,ActiveUnitInfo,[]);
    ShortUnitName:=AnUnitInfo.UnitName;
    if (ShortUnitName<>'') then begin
      Dummy:=CodeToolBoss.RemoveUnitFromAllUsesSections(
                                    Project1.MainUnitInfo.Source,ShortUnitName);
      if Dummy then
        Project1.MainUnitInfo.Modified:=true
      else begin
        ApplyCodeToolChanges;
        DoJumpToCodeToolBossError;
        Result:=mrCancel;
        exit;
      end;
    end;
    if (AnUnitInfo.FormName<>'') then begin
      Dummy:=Project1.RemoveCreateFormFromProjectFile(
          'T'+AnUnitInfo.FormName,AnUnitInfo.FormName);
      if not Dummy then begin
        ApplyCodeToolChanges;
        DoJumpToCodeToolBossError;
        Result:=mrCancel;
        exit;
      end;
    end;
    ApplyCodeToolChanges;
  end;
  Project1.Modified:=true;
end;

procedure TMainIDE.ProjInspectorOpen(Sender: TObject);
var
  CurUnitInfo: TUnitInfo;
begin
  CurUnitInfo:=ProjInspector.GetSelectedFile;
  if CurUnitInfo<>nil then begin
    DoOpenEditorFile(CurUnitInfo.Filename,-1,[ofRegularFile]);
    exit;
  end;
  if PkgBoss.OnProjectInspectorOpen(Sender) then exit;
end;

procedure TMainIDE.OnExtToolNeedsOutputFilter(var OutputFilter: TOutputFilter;
  var Abort: boolean);
begin
  OutputFilter:=TheOutputFilter;
  OutputFilter.Project:=Project1;
  if ToolStatus<>itNone then begin
    Abort:=true;
    exit;
  end;
  SourceNotebook.ClearErrorLines;

  ToolStatus:=itBuilder;
  MessagesView.Clear;
  DoArrangeSourceEditorAndMessageView(false);

  TheOutputFilter.OnOutputString:=@MessagesView.Add;
  TheOutputFilter.OnReadLine:=@MessagesView.ShowProgress;
end;

procedure TMainIDE.OnExtToolFreeOutputFilter(OutputFilter: TOutputFilter;
  ErrorOccurred: boolean);
begin
  if ToolStatus=itBuilder then
    ToolStatus:=itNone;
  if ErrorOccurred then
    DoJumpToCompilerMessage(-1,true);
end;

procedure TMainIDE.DoSwitchToFormSrc(var ActiveSourceEditor: TSourceEditor;
  var ActiveUnitInfo: TUnitInfo);
begin
  DoSwitchToFormSrc(nil,ActiveSourceEditor,ActiveUnitInfo);
end;

procedure TMainIDE.DoSwitchToFormSrc(ADesigner: TDesigner;
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
var i: integer;
begin
  if (ADesigner<>nil) then
    i:=Project1.IndexOfUnitWithForm(ADesigner.Form,false,nil)
  else if PropertyEditorHook1.LookupRoot<>nil then
    i:=Project1.IndexOfUnitWithForm(PropertyEditorHook1.LookupRoot,false,nil)
  else
    i:=-1;
  if (i>=0) then begin
    i:=Project1.Units[i].EditorIndex;
    if (i>=0) then begin
      SourceNoteBook.NoteBook.PageIndex:=i;
      GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
      exit;
    end;
  end;
  ActiveSourceEditor:=nil;
  ActiveUnitInfo:=nil;
end;

function TMainIDE.GetFormOfSource(AnUnitInfo: TUnitInfo; LoadForm: boolean
  ): TCustomForm;
begin
  Result:=TCustomForm(AnUnitInfo.Form);
  if (Result=nil) and LoadForm and (not AnUnitInfo.IsVirtual)
  and FilenameIsPascalSource(AnUnitInfo.Filename) then begin
    DoLoadLFM(AnUnitInfo,[]);
    Result:=TCustomForm(AnUnitInfo.Form);
  end;
end;

function TMainIDE.OnPropHookMethodExists(const AMethodName: ShortString;
  TypeData: PTypeData;
  var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean): boolean;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookMethodExists] ************ ',AMethodName);
  {$ENDIF}
  Result:=CodeToolBoss.PublishedMethodExists(ActiveUnitInfo.Source,
                            ActiveUnitInfo.Form.ClassName,AMethodName,TypeData,
                            MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  if CodeToolBoss.ErrorMessage<>'' then begin
    DoJumpToCodeToolBossError;
    raise Exception.Create('Unable to find method.'
                          +' Plz fix the error shown in the message window.');
  end;
end;

function TMainIDE.OnPropHookCreateMethod(const AMethodName: ShortString;
  ATypeInfo: PTypeInfo): TMethod;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  r: boolean;
begin
  Result.Code:=nil;
  Result.Data:=nil;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookCreateMethod] ************ ',AMethodName);
  {$ENDIF}
  FOpenEditorsOnCodeToolChange:=true;
  try
    // create published method
    r:=CodeToolBoss.CreatePublishedMethod(ActiveUnitInfo.Source,
                ActiveUnitInfo.Form.ClassName,AMethodName,ATypeInfo);
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.OnPropHookCreateMethod] ************2 ',r,' ',AMethodName);
    {$ENDIF}
    ApplyCodeToolChanges;
    if r then begin
      Result:=FormEditor1.JITFormList.CreateNewMethod(TForm(ActiveUnitInfo.Form)
                                                      ,AMethodName);
    end else begin
      DoJumpToCodeToolBossError;
      raise Exception.Create('Unable to create new method.'
                            +' Plz fix the error shown in the message window.');
    end;
  finally
    FOpenEditorsOnCodeToolChange:=false;
  end;
end;

procedure TMainIDE.OnPropHookShowMethod(const AMethodName: ShortString);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookShowMethod] ************ "',AMethodName,'" ',ActiveUnitInfo.Filename);
  {$ENDIF}

  if CodeToolBoss.JumpToPublishedMethodBody(ActiveUnitInfo.Source,
    ActiveUnitInfo.Form.ClassName,AMethodName,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, true);
  end else begin
    DoJumpToCodeToolBossError;
    raise Exception.Create('Unable to show method.'
                           +' Plz fix the error shown in the message window.');
  end;
end;

procedure TMainIDE.OnPropHookRenameMethod(const CurName, NewName: ShortString);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  r: boolean;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookRenameMethod] ************');
  {$ENDIF}
  FOpenEditorsOnCodeToolChange:=true;
  try
    // create published method
    r:=CodeToolBoss.RenamePublishedMethod(ActiveUnitInfo.Source,
                ActiveUnitInfo.Form.ClassName,CurName,NewName);
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.OnPropHookRenameMethod] ************2 ',r);
    {$ENDIF}
    ApplyCodeToolChanges;
    if r then begin
      FormEditor1.JITFormList.RenameMethod(TForm(ActiveUnitInfo.Form),
                                           CurName,NewName);
    end else begin
      DoJumpToCodeToolBossError;
      raise Exception.Create('Unable to rename method.'
                            +' Plz fix the error shown in the message window.');
    end;
  finally
    FOpenEditorsOnCodeToolChange:=false;
  end;
end;

procedure TMainIDE.OnPropHookComponentRenamed(AComponent: TComponent);
begin
  ObjectInspector1.FillComponentComboBox;
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.OnPropHookComponentAdded(AComponent: TComponent;
    Select: boolean);

  This handler is called whenever a new component was added to a designed form
  and should be added to form source
-------------------------------------------------------------------------------}
procedure TMainIDE.OnPropHookComponentAdded(AComponent: TComponent;
  Select: boolean);
var
  ComponentClass: TRegisteredComponent;
  ADesigner: TIDesigner;
begin
writeln('TMainIDE.OnPropHookComponentAdded A ',AComponent.Name,':',AComponent.ClassName);
  {$IFDEF EnablePkgs}
  ComponentClass:=IDEComponentPalette.FindComponent(AComponent.ClassName);
  {$ELSE}
  ComponentClass:=FindRegsiteredComponentClass(AComponent.ClassName);
  {$ENDIF}
  if ComponentClass=nil then begin
    writeln('TMainIDE.OnPropHookComponentAdded ',AComponent.ClassName,
            ' not registered');
    exit;
  end;
  // create unique name
  AComponent.Name:=FormEditor1.CreateUniqueComponentName(AComponent);
  //writeln('TMainIDE.OnPropHookComponentAdded B ',AComponent.Name,':',AComponent.ClassName);
  // create component interface
  if FormEditor1.FindComponent(AComponent)=nil then
    FormEditor1.CreateComponentInterface(AComponent);
  // set component into design mode
  SetDesigning(AComponent,true);
  //writeln('TMainIDE.OnPropHookComponentAdded C ',AComponent.Name,':',AComponent.ClassName);
  // add to source
  ADesigner:=FindRootDesigner(AComponent);
  OnDesignerComponentAdded(ADesigner,AComponent,ComponentClass);
  //writeln('TMainIDE.OnPropHookComponentAdded D ',AComponent.Name,':',AComponent.ClassName,' ',Select);
  // select component
  if Select then begin
    TheControlSelection.AssignComponent(AComponent);
  end;
writeln('TMainIDE.OnPropHookComponentAdded END ',AComponent.Name,':',AComponent.ClassName,' ',Select);
end;

procedure TMainIDE.OnPropHookDeleteComponent(AComponent: TComponent);
var
  ADesigner: TDesigner;
begin
  writeln('TMainIDE.OnPropHookDeleteComponent A ',AComponent.Name,':',AComponent.ClassName);
  ADesigner:=TDesigner(FindRootDesigner(AComponent));
  if ADesigner=nil then exit;
  ADesigner.RemoveComponentAndChilds(AComponent);
end;

procedure TMainIDE.mnuEditCopyClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecCopy);
end;

procedure TMainIDE.mnuEditCutClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecCut);
end;

procedure TMainIDE.mnuEditPasteClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecPaste);
end;

procedure TMainIDE.mnuEditRedoClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecRedo);
end;

procedure TMainIDE.mnuEditUndoClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecUndo);
end;

procedure TMainIDE.mnuEditIndentBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecBlockIndent);
end;

procedure TMainIDE.mnuEditUnindentBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecBlockUnindent);
end;

procedure TMainIDE.mnuEditUpperCaseBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionUpperCase);
end;

procedure TMainIDE.mnuEditLowerCaseBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionLowerCase);
end;

procedure TMainIDE.mnuEditTabsToSpacesBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionTabs2Spaces);
end;

procedure TMainIDE.mnuEditCommentBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionComment);
end;

procedure TMainIDE.mnuEditUncommentBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionUncomment);
end;

procedure TMainIDE.mnuEditSortBlockClicked(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectionSort);
end;

procedure TMainIDE.mnuEditSelectAllClick(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectAll);
end;

procedure TMainIDE.mnuEditSelectCodeBlockClick(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectCodeBlock);
end;

procedure TMainIDE.mnuEditSelectToBraceClick(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectToBrace);
end;

procedure TMainIDE.mnuEditSelectLineClick(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectLine);
end;

procedure TMainIDE.mnuEditSelectParagraphClick(Sender: TObject);
begin
  DoEditMenuCommand(ecSelectParagraph);
end;

procedure TMainIDE.mnuEditInsertGPLNoticeClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertGPLNotice);
end;

procedure TMainIDE.mnuEditInsertLGPLNoticeClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertLGPLNotice);
end;

procedure TMainIDE.mnuEditInsertUsernameClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertUserName);
end;

procedure TMainIDE.mnuEditInsertDateTimeClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertDateTime);
end;

procedure TMainIDE.mnuEditInsertChangeLogEntryClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertChangeLogEntry);
end;

procedure TMainIDE.mnuEditCompleteCodeClicked(Sender: TObject);
begin
  DoCompleteCodeAtCursor;
end;

procedure TMainIDE.mnuEditInsertCVSAuthorClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSAuthor);
end;

procedure TMainIDE.mnuEditInsertCVSDateClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSDate);
end;

procedure TMainIDE.mnuEditInsertCVSHeaderClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSHeader);
end;

procedure TMainIDE.mnuEditInsertCVSIDClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSID);
end;

procedure TMainIDE.mnuEditInsertCVSLogClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSLog);
end;

procedure TMainIDE.mnuEditInsertCVSNameClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSName);
end;

procedure TMainIDE.mnuEditInsertCVSRevisionClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSRevision);
end;

procedure TMainIDE.mnuEditInsertCVSSourceClick(Sender: TObject);
begin
  DoEditMenuCommand(ecInsertCVSSource);
end;

procedure TMainIDE.DoEditMenuCommand(EditorCommand: integer);
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  if FDisplayState = dsSource then begin
    // send command to source editor
    if ActiveSourceEditor=nil then exit;
    ActiveSourceEditor.DoEditorExecuteCommand(EditorCommand);
  end else begin
    // send command to form editor
    if ActiveUnitInfo=nil then exit;

    // ToDo: send command to form editor/designer
        
  end;
end;

procedure TMainIDE.OnApplyWindowLayout(ALayout: TIDEWindowLayout);
var
  l: TNonModalIDEWindow;
begin
  if (ALayout=nil) or (ALayout.Form=nil) then exit;
//writeln('AAA TMainIDE.OnApplyWindowLayout ',ALayout.Form.Name,' ',ALayout.Form.Classname,' ',IDEWindowPlacementNames[ALayout.WindowPlacement],' ',ALayout.CustomCoordinatesAreValid,' ',ALayout.Left,' ',ALayout.Top,' ',ALayout.Width,' ',ALayout.Height);
  if (ALayout.WindowPlacement in [iwpCustomPosition,iwpRestoreWindowGeometry])
  and (ALayout.CustomCoordinatesAreValid) then begin
    // explicit position
    ALayout.Form.SetBounds(
      ALayout.Left,ALayout.Top,ALayout.Width,ALayout.Height)
  end else
  if (not (ALayout.WindowPlacement in [iwpDocked,iwpUseWindowManagerSetting]))
  then begin
    // default position
    l:=NonModalIDEFormIDToEnum(ALayout.FormID);
    case l of
    nmiwMainIDEName:
      ALayout.Form.SetBounds(0,0,Screen.Width-10,95);
    nmiwSourceNoteBookName:
      ALayout.Form.SetBounds(250,Top+Height+30,Max(50,Screen.Width-300),
        Max(50,Screen.Height-200-Top-Height));
    nmiwUnitDependenciesName:
      ALayout.Form.SetBounds(200,200,400,300);
    nmiwCodeExplorerName:
      ALayout.Form.SetBounds(Screen.Width-200,130,170,Max(50,Screen.Height-230));
    nmiwClipbrdHistoryName:
      ALayout.Form.SetBounds(250,Screen.Height-400,400,300);
    nmiwPkgGraphExplorer:
      ALayout.Form.SetBounds(250,150,500,350);
    nmiwProjectInspector:
      ALayout.Form.SetBounds(210,150,400,300);
    nmiwMessagesViewName:
      ALayout.Form.SetBounds(260,SourceNotebook.Top+SourceNotebook.Height+30,
        Max(50,Screen.Width-300),80);
    else
      if ALayout.FormID=DefaultObjectInspectorName then begin
        ALayout.Form.SetBounds(
          Left,Top+Height+30,230,Max(Screen.Height-Top-Height-120,50));
      end;
    end;
  end;
end;

procedure TMainIDE.AddRecentFileToEnvironment(const AFilename: string);
begin
  EnvironmentOptions.AddToRecentProjectFiles(AFilename);
  SetRecentProjectFilesMenu;
  SaveEnvironment;
end;

procedure TMainIDE.mnuSearchFindBlockOtherEnd(Sender: TObject);
begin
  DoGoToPascalBlockOtherEnd;
end;

procedure TMainIDE.mnuSearchFindBlockStart(Sender: TObject);
begin
  DoGoToPascalBlockStart;
end;

procedure TMainIDE.mnuSearchFindDeclaration(Sender: TObject);
begin
  DoFindDeclarationAtCursor;
end;


//-----------------------------------------------------------------------------

initialization
  { $I mainide.lrs}
  {$I images/laz_images.lrs}
  {$I images/mainicon.lrs}


end.


{ =============================================================================
  $Log$
  Revision 1.554  2003/05/05 13:40:26  mattias
  ide extra options are now saved, so creating a packaged ide is now possible on commandline

  Revision 1.553  2003/05/04 21:48:52  mattias
  added parsing make errors

  Revision 1.552  2003/05/03 08:13:27  mattias
  fixed clean all

  Revision 1.551  2003/05/02 22:22:14  mattias
  localization, added context policy to make resource string dialog

  Revision 1.550  2003/05/02 10:28:59  mattias
  improved file checking

  Revision 1.549  2003/05/01 20:48:47  mattias
  fixed building lazarus without IDE

  Revision 1.548  2003/05/01 11:44:03  mattias
  fixed changing menuitem separator and normal

  Revision 1.547  2003/05/01 10:40:06  mattias
  open special files for lpi files

  Revision 1.546  2003/04/29 19:00:41  mattias
  added package gtkopengl

  Revision 1.545  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.544  2003/04/29 09:31:09  mattias
  changed macro name ProjectDir to ProjPath

  Revision 1.543  2003/04/28 20:46:54  mattias
  implemented replacing installed packages

  Revision 1.542  2003/04/28 08:16:13  mattias
  implemented environment path browse buttons

  Revision 1.541  2003/04/27 20:31:53  mattias
  implemented open package for component palette

  Revision 1.540  2003/04/27 12:53:57  mattias
  implemented saving auto install packages

  Revision 1.539  2003/04/27 09:29:56  mattias
  implemented installing static packages

  Revision 1.538  2003/04/26 07:34:54  mattias
  implemented custom package initialization

  Revision 1.537  2003/04/25 14:40:49  mattias
  implemented add file to a package dialog

  Revision 1.536  2003/04/24 16:44:28  mattias
  implemented define templates for projects with packages

  Revision 1.535  2003/04/22 18:53:12  mattias
  implemented compiling project dependencies and auto add dependency

  Revision 1.534  2003/04/22 13:27:09  mattias
  implemented installing components in component palette

  Revision 1.533  2003/04/22 07:56:17  mattias
  implemented dynamic component palette

  Revision 1.532  2003/04/21 18:00:52  mattias
  fixed opening main unit

  Revision 1.531  2003/04/21 16:21:28  mattias
  implemented default package for custom IDE components

  Revision 1.530  2003/04/20 23:10:03  mattias
  implemented inherited project compiler options

  Revision 1.529  2003/04/20 20:32:40  mattias
  implemented removing, re-adding, updating project dependencies

  Revision 1.528  2003/04/20 07:36:28  mattias
  fixed loading form name

  Revision 1.527  2003/04/19 17:54:37  mattias
  improved new menueditor from Martin Patik

  Revision 1.526  2003/04/19 16:55:38  mattias
  started project inspector

  Revision 1.525  2003/04/17 11:40:40  mattias
  implemented compilation of simple packages

  Revision 1.524  2003/04/16 22:11:35  mattias
  fixed codetools Makefile, fixed default prop not found error

  Revision 1.523  2003/04/15 17:58:28  mattias
  implemented inherited Compiler Options View

  Revision 1.522  2003/04/15 08:54:26  mattias
  fixed TMemo.WordWrap

  Revision 1.521  2003/04/13 22:39:19  mattias
  implemented package links, automatic package loading

  Revision 1.520  2003/04/13 13:45:04  mattias
  implemented broken dependencies dialog

  Revision 1.519  2003/04/11 21:21:34  mattias
  implemented closing unneeded package

  Revision 1.518  2003/04/10 19:42:16  mattias
  implemented package graph showing open packages

  Revision 1.517  2003/04/10 16:38:37  mattias
  fixed crash in new menueditor

  Revision 1.516  2003/04/10 10:17:27  mattias
  added Abort to save on modified

  Revision 1.515  2003/04/09 23:36:33  mattias
  implemented removing and readding package items

  Revision 1.514  2003/04/08 22:02:15  mattias
  implemented open package file

  Revision 1.513  2003/04/08 20:14:26  mattias
  implemented open package

  Revision 1.512  2003/04/08 16:56:55  mattias
  implemented saving package

  Revision 1.511  2003/04/07 23:49:03  mattias
  implemented adding units to packages

  Revision 1.510  2003/04/07 12:12:12  mattias
  implemented auto completion for add new component to package

  Revision 1.509  2003/04/06 22:39:47  mattias
  implemented searching packages

  Revision 1.508  2003/04/04 22:14:32  mattias
  implemented LCL and FCL packages, started package registration

  Revision 1.507  2003/04/03 20:05:43  mattias
  added menueditor from Martin Patik, not yet working

  Revision 1.506  2003/04/03 15:02:23  mattias
  fixed loading .files

  Revision 1.505  2003/04/02 17:06:27  mattias
  improved deb creation

  Revision 1.504  2003/04/01 23:35:28  mattias
  added menu component editor from Olivier

  Revision 1.503  2003/04/01 17:29:36  mattias
  added packagemanager and added config dialog for custom components from Tony

  Revision 1.502  2003/04/01 16:25:45  mattias
  started packageeditor and packagelinks

  Revision 1.501  2003/03/29 21:41:19  mattias
  fixed path delimiters for environment directories

  Revision 1.500  2003/03/29 17:20:04  mattias
  added TMemoScrollBar

  Revision 1.499  2003/03/27 08:50:47  mattias
  find declaration: implemented file type

  Revision 1.498  2003/03/26 10:56:30  mattias
  added ToDo List from Olivier

  Revision 1.497  2003/03/26 00:21:24  mattias
  implemented build lazarus extra options -d

  Revision 1.496  2003/03/25 17:13:23  mattias
  reduced output

  Revision 1.495  2003/03/25 17:11:16  mattias
  set Project.AutoCreateForms default to true

  Revision 1.494  2003/03/25 10:45:40  mattias
  reduced focus handling and improved focus setting

  Revision 1.493  2003/03/17 13:00:35  mattias
  improved but not fixed transient windows

  Revision 1.492  2003/03/16 11:07:41  mattias
  fixed insert text GPL and LGPL

  Revision 1.491  2003/03/16 08:44:17  mattias
  fixed run and wait script

  Revision 1.490  2003/03/15 15:50:10  mattias
  fixed DoJumpToError

  Revision 1.489  2003/03/15 13:26:07  mattias
  fixes for fpc 1.1

  Revision 1.488  2003/03/14 23:27:46  mattias
  launching in terminal now keeps terminal open after execution

  Revision 1.487  2003/03/14 22:51:24  mattias
  improved case renaming of pascal files

  Revision 1.486  2003/03/14 21:38:36  mattias
  implemented diff dialog

  Revision 1.485  2003/03/14 14:57:03  mattias
  make resourcestring dialog now checks for doubles (idents and values)

  Revision 1.484  2003/03/13 10:11:41  mattias
  fixed TControl.Show in design mode

  Revision 1.483  2003/03/11 22:56:41  mattias
  added visiblechanging

  Revision 1.482  2003/03/11 11:05:50  mattias
  fixed toggle form or src button

  Revision 1.481  2003/03/11 11:00:08  mattias
  implemented auto loading forms on open

  Revision 1.480  2003/03/11 09:57:51  mattias
  implemented ProjectOpt: AutoCreateNewForms, added designer Show Options

  Revision 1.479  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.478  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.477  2003/03/08 21:51:56  mattias
  make resource string dialog nearly complete

  Revision 1.476  2003/03/07 13:32:40  mattias
  fixed checking readonly for non existing files

  Revision 1.475  2003/03/07 11:41:21  mattias
  fixed readonly check and added script to quick create lazarus snapshot

  Revision 1.474  2003/03/06 22:41:33  mattias
  open file now asks on .lpi files

  Revision 1.473  2003/03/02 09:04:02  mattias
  added make resourcestring dialog, not finished

  Revision 1.472  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.471  2003/02/28 15:38:00  mattias
  bookmarks are now saved also for closed files and merged when possible

  Revision 1.470  2003/02/28 10:14:28  mattias
  started package system (packager)

  Revision 1.469  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.468  2003/02/24 11:51:44  mattias
  combobox height can now be set, added OI item height option

  Revision 1.467  2003/02/23 09:03:15  mattias
  fixed save as with no filename

  Revision 1.466  2003/02/21 22:43:54  mattias
  skipped messages are now shown to see progress

  Revision 1.465  2003/02/21 10:42:57  mattias
  fixed updating OI when moving components

  Revision 1.464  2003/02/20 11:03:20  mattias
  save as of project files now starts in project dierctory

  Revision 1.463  2003/02/19 23:17:45  mattias
  added warnings when fpc source dir invalid

  Revision 1.462  2003/02/19 22:27:33  mattias
  added some messages when compiler filename is invalid

  Revision 1.461  2003/02/07 19:13:57  mattias
  fixed searching lazarus in current dir

  Revision 1.460  2003/02/07 17:49:21  mattias
  added ReadAllLinks

  Revision 1.459  2003/02/06 20:46:51  mattias
  default fpc src dirs and clean ups

  Revision 1.458  2003/02/05 13:46:57  mattias
  fixed TCustomEdit.SelStart when nothing selected

  Revision 1.457  2003/02/03 22:28:08  mattias
  small bugfixes and fixed non checked menu items activate

  Revision 1.456  2003/01/28 17:04:33  mattias
  renamed one Rect

  Revision 1.455  2003/01/19 14:44:27  mattias
  started make resource string

  Revision 1.454  2003/01/15 09:08:08  mattias
  fixed search paths for virtual projects

  Revision 1.453  2003/01/14 17:07:22  mattias
  improved source error message

  Revision 1.452  2003/01/14 17:02:49  mattias
  updated translations and raised message window on error

  Revision 1.451  2003/01/10 17:22:38  mattias
  fixed virtual directory

  Revision 1.450  2003/01/06 10:51:40  mattias
  freeing stopped external tools

  Revision 1.449  2003/01/04 11:58:32  mattias
  added Windows menu to IDE

  Revision 1.448  2003/01/03 12:27:48  mattias
  added sort selection

  Revision 1.447  2003/01/02 04:33:55  mattias
  implemented incremental find and unit usage counts

  Revision 1.446  2002/12/29 22:43:46  mattias
  activated identifier completion

  Revision 1.445  2002/12/29 18:13:38  mattias
  identifier completion: basically working, still hidden

  Revision 1.444  2002/12/27 10:34:54  mattias
  message view scrolls to message

  Revision 1.443  2002/12/26 11:00:14  mattias
  added included by to unitinfo and a few win32 functions

  Revision 1.442  2002/12/25 13:30:36  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.441  2002/12/23 13:41:48  mattias
  remove from project now extracts project path

  Revision 1.440  2002/12/23 13:20:45  mattias
  fixed backuping symlinks

  Revision 1.439  2002/12/20 11:08:47  mattias
  method resolution clause, class ancestor find declaration, 1.1. makros

  Revision 1.438  2002/12/17 19:49:33  mattias
  finished publish project

  Revision 1.437  2002/12/16 23:27:01  mattias
  fixed command line internationalization

  Revision 1.436  2002/12/02 10:44:55  mattias
  fixed SourceChangeCache when nothing has changed

  Revision 1.435  2002/11/29 17:51:04  mattias
  star mark for modified editors and OI combo multiselection text

  Revision 1.434  2002/11/18 11:39:48  mattias
  improved icons, started codeexplorer

  Revision 1.433  2002/11/18 11:01:58  mattias
  improved icons, started codeexplorer

  Revision 1.432  2002/11/16 14:38:47  mattias
  fixed TControl.Show and Visible of designer forms

  Revision 1.431  2002/11/13 13:40:12  lazarus
  MG: codetools can now work with localized fpc output

  Revision 1.430  2002/11/12 14:55:33  lazarus
  Show Object Inspector when not visible if toggling OI/form/code view.

  Revision 1.429  2002/11/10 22:02:35  lazarus
  MG: added smart hints in debug mode

  Revision 1.428  2002/11/10 21:49:23  lazarus
  MG: added smart hints in edit mode

  Revision 1.427  2002/11/09 18:13:30  lazarus
  MG: fixed gdkwindow checks

  Revision 1.426  2002/11/05 20:09:36  lazarus
  MG: reduced output

  Revision 1.425  2002/11/05 20:03:38  lazarus
  MG: implemented hints

  Revision 1.424  2002/11/04 20:57:24  lazarus
  Make object inspector toggling work.

  Revision 1.423  2002/11/04 19:49:34  lazarus
  MG: added persistent hints for main ide bar

  Revision 1.422  2002/11/02 10:59:28  lazarus
  MG: fixed designer hints

  Revision 1.421  2002/10/31 17:31:08  lazarus
  MG: fixed return polygon point

  Revision 1.420  2002/10/30 22:28:47  lazarus
  MG: fixed used virtual files and IsPartOfProject Bug

  Revision 1.419  2002/10/30 18:24:42  lazarus
  MG: fixed unitname update bug

  Revision 1.418  2002/10/30 17:29:42  lazarus
  MG: multi open file now skips checking for special files

  Revision 1.417  2002/10/23 14:12:01  lazarus
  MG: implemented indirect deletion of designed components

  Revision 1.416  2002/10/22 17:53:35  lazarus
  MG: fixed updating OI when adding/emoving component and selecting in OI

  Revision 1.415  2002/10/21 22:12:46  lazarus
  MG: fixed frmactivate

  Revision 1.414  2002/10/14 11:43:05  lazarus
  MG: fixed double jumping in history

  Revision 1.413  2002/10/14 07:59:56  lazarus
  MG: fixed clicking on message view on same selection

  Revision 1.412  2002/10/14 07:16:34  lazarus
  MG: fixed showing form

  Revision 1.411  2002/10/13 21:23:42  lazarus
  MG: code completion: added local variable completion

  Revision 1.410  2002/10/13 10:19:18  lazarus
  MG: publish project: save project after dialog

  Revision 1.409  2002/10/13 09:35:34  lazarus
  MG: added publish project

  Revision 1.408  2002/10/09 20:08:39  lazarus
  Cleanups

  Revision 1.407  2002/10/09 12:27:44  lazarus
  MG: fixed maxdist of guidelines

  Revision 1.406  2002/10/06 17:55:43  lazarus
  MG: JITForms now sets csDesigning before creation

  Revision 1.405  2002/10/06 16:46:44  lazarus
  MG: added selection changes checks

  Revision 1.404  2002/10/05 14:17:50  lazarus
  MG: jump point is now added if method jumping not revertable

  Revision 1.403  2002/10/05 09:31:09  lazarus
  MG: added consistency checks and fixed selection drawing on wrong forms

  Revision 1.402  2002/10/04 21:31:54  lazarus
  MG: added some component rename checks

  Revision 1.401  2002/10/03 14:48:14  lazarus
  MG: fixed jump history

  Revision 1.400  2002/10/03 07:19:34  lazarus
  MG: deactivated publish project

  Revision 1.399  2002/10/02 20:30:26  lazarus
  MG: fixed jumping history

  Revision 1.398  2002/10/02 16:16:36  lazarus
  MG: accelerated unitdependencies

  Revision 1.397  2002/10/02 07:56:19  lazarus
  MG: fixed frozen selection in designer on sizing

  Revision 1.396  2002/10/02 00:17:03  lazarus
  MWE:
    + Honoured the ofQuiet flag in DoOpenNotExistingFile, so custom messages
      can be shown
    + Added a dialog to make custom locate of a debug file possible

  Revision 1.395  2002/09/30 14:01:04  lazarus
  MG: undid the TBinaryObjectWriter Buffersize

  Revision 1.394  2002/09/30 09:26:40  lazarus
  MG: added DoSaveAll before CloseAll

  Revision 1.393  2002/09/20 14:41:34  lazarus
  MG: cleanup

  Revision 1.392  2002/09/20 11:40:05  lazarus
  MG: added Move Page Left/Right for sourcenotebook

  Revision 1.391  2002/09/20 08:36:42  lazarus
  MG: workaround for TBinaryObjectWriter till we announce the new compiler

  Revision 1.390  2002/09/20 07:26:34  lazarus
  MG: applied localization from Vasily

  Revision 1.389  2002/09/19 14:54:53  lazarus
  MG: history jumps now works without double jumps

  Revision 1.388  2002/09/17 22:19:32  lazarus
  MG: fixed creating project from file

  Revision 1.387  2002/09/17 21:33:14  lazarus
  MG: accelerated designer mouse move and added Delete Selection to designer popupmenu

  Revision 1.386  2002/09/16 17:19:05  lazarus
  MG: reduced output

  Revision 1.384  2002/09/16 16:06:19  lazarus
  MG: replaced halt with raiseexception

  Revision 1.383  2002/09/16 06:44:06  lazarus
  MG: added localization from Vasily

  Revision 1.382  2002/09/14 17:27:44  lazarus
  MG: double click on unit dependencies now opens the file

  Revision 1.381  2002/09/14 17:04:23  lazarus
  MG: unnit dependencies basically working

  Revision 1.380  2002/09/14 16:00:27  lazarus
  MG: added Refresh to Unit Dependencies

  Revision 1.379  2002/09/14 07:05:12  lazarus
  MG: added uni dependencies

  Revision 1.378  2002/09/13 16:58:23  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.377  2002/09/13 07:01:17  lazarus
  MG: fixed memcheck

  Revision 1.376  2002/09/11 13:19:54  lazarus
  MG: added  CVS keywords

  Revision 1.375  2002/09/11 12:18:18  lazarus
  MG: added  insert ChangeLog Entry

  Revision 1.374  2002/09/11 12:05:45  lazarus
  MG: added  insert Username and Datetime

  Revision 1.373  2002/09/11 11:31:22  lazarus
  MG: added  insert GPL notice

  Revision 1.372  2002/09/11 08:32:54  lazarus
  MG: added selection menu items

  Revision 1.371  2002/09/10 18:17:30  lazarus
  MG: added  new project from file

  Revision 1.370  2002/09/10 17:45:02  lazarus
  MG: fixed creating project with program source

  Revision 1.369  2002/09/10 14:48:21  lazarus
  MG: added SaveFlagNames

  Revision 1.368  2002/09/10 07:43:19  lazarus
  MG: fixed auto revert

  Revision 1.367  2002/09/09 12:36:34  lazarus
  MG: added keymapping to designer

  Revision 1.366  2002/09/08 12:23:40  lazarus
  MG: TComponentPropertyEditor now shows child properties

  Revision 1.365  2002/09/08 10:01:54  lazarus
  MG: fixed streaming visible=false

  Revision 1.364  2002/09/06 19:11:46  lazarus
  MG: fixed scrollbars of TTreeView

  Revision 1.363  2002/09/05 19:03:34  lazarus
  MG: improved handling of ambigious source files

  Revision 1.362  2002/09/05 15:24:26  lazarus
  MG: added auto deleting of ambigious source files

  Revision 1.361  2002/09/05 12:11:39  lazarus
  MG: TNotebook is now streamable

  Revision 1.360  2002/09/04 09:32:15  lazarus
  MG: improved streaming error handling

  Revision 1.359  2002/09/03 20:01:59  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.358  2002/09/02 19:10:25  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.357  2002/08/31 11:37:07  lazarus
  MG: fixed destroying combobox

  Revision 1.356  2002/08/29 00:07:00  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.355  2002/08/28 10:44:43  lazarus
  MG: implemented run param environment variables

  Revision 1.354  2002/08/27 09:22:44  lazarus
  MG: not existing files will now be removed from recent lists

  Revision 1.353  2002/08/27 08:55:27  lazarus
  MG: fixed editing empty resources

  Revision 1.352  2002/08/27 08:21:28  lazarus
  MG: fixed replacing form resources

  Revision 1.351  2002/08/24 15:49:55  lazarus
  MG: loading forms now creates all TComponentInterfaces, fixed removing components

  Revision 1.350  2002/08/24 13:41:27  lazarus
  MG: fixed TSpeedButton.SetDown and Invalidate

  Revision 1.349  2002/08/23 19:00:12  lazarus
  MG: implemented Ctrl+Mouse links in source editor

  Revision 1.348  2002/08/23 11:33:03  lazarus
  MG: imroved error handling for renamed forms

  Revision 1.347  2002/08/23 11:24:41  lazarus
  MG: implemented form renaming

  Revision 1.346  2002/08/23 07:05:14  lazarus
  MG: started form renaming

  Revision 1.345  2002/08/21 07:16:57  lazarus
  MG: reduced mem leak of clipping stuff, still not fixed

  Revision 1.344  2002/08/19 18:24:25  lazarus
  MG: fixed mouse coords while component dragging

  Revision 1.343  2002/08/17 11:38:01  lazarus
  MG: fixed keygrabbing key translation

  Revision 1.342  2002/08/16 20:13:07  lazarus
  MG: custom external tools are now shown in the menu

  Revision 1.341  2002/08/16 19:00:54  lazarus
  MG: added Un-/Comment Selection

  Revision 1.340  2002/08/16 17:47:37  lazarus
  MG: added some IDE menuicons, fixed submenu indicator bug

  Revision 1.339  2002/08/09 19:48:12  lazarus
  MG: Open File at cursor now checks for include directive

  Revision 1.338  2002/08/08 10:33:48  lazarus
  MG: main bar speedbar open arrow now shows recent projects and files

  Revision 1.337  2002/08/08 09:38:34  lazarus
  MG: recent file menus are now updated instantly

  Revision 1.336  2002/08/07 09:55:26  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.335  2002/08/05 08:56:54  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.334  2002/08/03 14:30:37  lazarus
  MG: added file access monitoring and diff view

  Revision 1.333  2002/08/01 14:15:02  lazarus
  MG: started file access monitoring for loaded files

  Revision 1.332  2002/08/01 14:10:28  lazarus
  MG: started file access monitoring for loaded files

  Revision 1.331  2002/08/01 08:06:25  lazarus
  MG: reduced output

  Revision 1.330  2002/08/01 08:03:01  lazarus
  MG: accelerated searches in project

  Revision 1.329  2002/07/31 15:21:50  lazarus
  MG: added tool: Convert DFM file to LFM

  Revision 1.328  2002/07/31 09:00:03  lazarus
  MG: fixed undefined editor topline on codetool error

  Revision 1.327  2002/07/29 13:26:54  lazarus
  MG: source notebook pagenames are now updated more often

  Revision 1.326  2002/07/22 18:25:10  lazarus
  MG: reduced output

  Revision 1.325  2002/07/20 13:54:13  lazarus
  MG: fixed show designed form on new project

  Revision 1.324  2002/07/06 06:37:04  lazarus
  MG: added Revert

  Revision 1.323  2002/07/05 12:54:27  lazarus
  MG: syntax highlighter is now set on open non existing file

  Revision 1.322  2002/07/05 10:32:44  lazarus
  MG: added  handling of invalid programnamnes on save project

  Revision 1.321  2002/07/05 09:36:45  lazarus
  MG: fixed unreleased gdiobjects on printing cmd line help

  Revision 1.320  2002/07/04 14:48:22  lazarus
  MG: added internationalization, started with german

  Revision 1.319  2002/07/04 10:44:50  lazarus
  MG: added comment for MainIDE:=Self;

  Revision 1.318  2002/07/04 10:22:46  lazarus
  MG: fixed double parsing of command line

  Revision 1.317  2002/06/28 06:34:54  lazarus
  MG: added Tabs To Spaces in Selection

  Revision 1.316  2002/06/26 15:11:07  lazarus
  MG: added new tool: Guess misplaced $IFDEF/$ENDIF

  Revision 1.315  2002/06/21 17:54:22  lazarus
  MG: in design mode the mouse cursor is now also set for hidden gdkwindows

  Revision 1.314  2002/06/19 19:46:05  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.313  2002/06/14 14:57:05  lazarus
  MG: fixed open file at cursor search path

  Revision 1.312  2002/06/13 18:37:47  lazarus
  MG: added upper/lowercase selection

  Revision 1.311  2002/06/12 14:27:15  lazarus
  MG: reduced IDE output

  Revision 1.310  2002/06/12 14:14:50  lazarus
  MG: fixed working directory of running programms

  Revision 1.309  2002/06/11 15:40:27  lazarus
  MG: implemented GridSizeX, GridSizeY and DisplayGrid

  Revision 1.308  2002/06/09 07:08:41  lazarus
  MG: fixed window jumping

  Revision 1.307  2002/06/08 17:15:59  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.306  2002/06/04 15:17:17  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.305  2002/06/01 08:41:27  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.304  2002/05/30 22:08:09  lazarus
  MG: TMenuItems now all owned by mainbar

  Revision 1.303  2002/05/30 21:40:08  lazarus
  * component-palette: TMainMenu replaces TMenu
  + TMenuItem registered as unvisible component
  + TMainMenu registered as visible component
  stoppok

  Revision 1.302  2002/05/30 14:18:46  lazarus
  MG: filedialogs now saves size and history

  Revision 1.301  2002/05/27 17:58:40  lazarus
  MG: added command line help

  Revision 1.300  2002/05/27 14:38:32  lazarus
  MG; fixed find declaration of overloaded procs and expression input types

  Revision 1.299  2002/05/24 07:18:14  lazarus
  MG: save is now possible during debugging

  Revision 1.298  2002/05/16 15:51:48  lazarus
  MG: closing editor now switches to left editor

  Revision 1.297  2002/05/16 14:35:19  lazarus
  MG: fixed codetools error jumping

  Revision 1.296  2002/05/16 14:01:45  lazarus
  MG: backuping files now save file attributes/permissions

  Revision 1.295  2002/05/16 13:00:55  lazarus
  MG: fixed changing syntax highlighter on save as

  Revision 1.294  2002/05/13 06:12:54  lazarus
  MG: fixed saving unitlinks after changing fpc soure path

  Revision 1.293  2002/05/10 06:57:42  lazarus
  MG: updated licenses

  Revision 1.292  2002/05/08 14:45:55  lazarus

     New About Dialog Window added; Splash screen modified to stay visible
     longer.  MAH

  Revision 1.291  2002/05/06 12:21:35  lazarus
  MG: nicer default window positions

  Revision 1.290  2002/04/30 15:01:05  lazarus
  MWE:
    + Added view callstack menu item

  Revision 1.289  2002/04/28 14:10:27  lazarus
  MG: fixes for saving resource files

  Revision 1.288  2002/04/28 06:42:47  lazarus
  MG: fixed saving OI settings

  Revision 1.287  2002/04/27 18:56:47  lazarus
  MG: started component renaming

  Revision 1.286  2002/04/26 13:50:14  lazarus
  MG: IDE and codetools work now with trimmed filenames

  Revision 1.285  2002/04/26 12:53:29  lazarus
  MG: fixed debug line coloring

  Revision 1.284  2002/04/26 12:27:26  lazarus
  MG: added Delphi6 Templates

  Revision 1.283  2002/04/22 07:16:34  lazarus
  MG: fixed missing include directives

  Revision 1.282  2002/04/21 14:30:36  lazarus
  MG: fixed inputhistory load/save

  Revision 1.281  2002/04/21 13:24:05  lazarus
  MG: small updates and fixes

  Revision 1.280  2002/04/21 06:53:52  lazarus
  MG: fixed save lrs to test dir

  Revision 1.279  2002/04/16 08:55:04  lazarus
  MG: added path editor for compiler options

  Revision 1.278  2002/04/15 10:56:05  lazarus
  MG: fixes for open lpi files and improved jump points

  Revision 1.277  2002/04/12 16:36:07  lazarus
  MG: FPC unitlinks are now saved

  Revision 1.276  2002/04/12 10:21:53  lazarus
  MG: added Event Assignment completion

  Revision 1.275  2002/04/11 08:08:47  lazarus
  MG: small fixes, cleanups and started event assignment completion

  Revision 1.274  2002/04/06 11:20:41  lazarus
  MG: added fpc define templates

  Revision 1.273  2002/04/05 18:17:59  lazarus
  MG: fixed removing virtual units

  Revision 1.272  2002/04/05 16:34:13  lazarus
  MG: fixed autocreate form editing in project opts

  Revision 1.271  2002/04/04 17:21:17  lazarus
  MG: fixed outputfilter for linker errors

  Revision 1.270  2002/04/03 18:20:49  lazarus
  MG: fixed mem leaks

  Revision 1.269  2002/04/03 10:34:05  lazarus
  MG: fixed crash on open project

  Revision 1.268  2002/04/02 17:18:24  lazarus
  MG: fixed save project as, renaming source name

  Revision 1.267  2002/03/31 23:20:35  lazarus
  MG: fixed initial size of TPage

  Revision 1.266  2002/03/29 23:22:20  lazarus
  MG: started internationalization of IDE

  Revision 1.265  2002/03/29 14:32:46  lazarus
  MG: further internationalization

  Revision 1.264  2002/03/28 16:39:04  lazarus
  MG: added find replace history

  Revision 1.263  2002/03/28 11:49:48  lazarus
  MG: added search function: Goto Include Directive

  Revision 1.262  2002/03/28 09:34:06  lazarus
  MG: show objectinspector only if needed

  Revision 1.261  2002/03/28 00:11:04  lazarus
  MG: removed unused

  Revision 1.260  2002/03/27 11:35:56  lazarus
  MG: removed ide_debugger.inc

  Revision 1.259  2002/03/27 10:39:42  lazarus
  MG: splitted main.pp: debugger management in TDebugManager

  Revision 1.258  2002/03/27 09:25:31  lazarus
  MG: renamed main Project to Project1

  Revision 1.257  2002/03/27 09:18:11  lazarus
  MG: splitted main.pp: TMainIDE has now an ancestor TMainIDEBar

  Revision 1.256  2002/03/25 16:48:25  lazarus
  MG: clean ups for main.pp, many minor fixes

  Revision 1.255  2002/03/25 07:29:21  lazarus
  MG: added TOpen/SaveFlags and splittet some methods

  Revision 1.254  2002/03/23 19:05:50  lazarus
  MG: pascal lowercase for open new unit

  Revision 1.253  2002/03/23 16:40:29  lazarus
  MWE:
    + Added loval variables menu item
    * Honoured the UseLaunchingApplication checkbox

  Revision 1.252  2002/03/23 15:54:28  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.251  2002/03/22 17:36:09  lazarus
  MG: added include link history

  Revision 1.250  2002/03/22 13:11:29  lazarus
  Added Application.ProcessMessages in MainMouseMoved to prevent the IDE hanging.

  Shane

  Revision 1.249  2002/03/22 12:36:44  lazarus
  MG: many fixes, to make it short: events

  Revision 1.248  2002/03/21 23:15:39  lazarus
  MG: fixes for save-project-as and pagenames

  Revision 1.247  2002/03/21 22:44:06  lazarus
  MG: fixes for save-as and form streaming exceptions

  Revision 1.246  2002/03/18 12:29:53  lazarus
  MG: added complete properties checkbox and ecSaveAll

  Revision 1.245  2002/03/14 14:39:39  lazarus
  MG: implemented run parameters: wd, launching app, sys vars

  Revision 1.244  2002/03/12 23:55:34  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

  Revision 1.243  2002/03/09 02:03:57  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.242  2002/03/08 11:37:40  lazarus
  MG: outputfilter can now find include files

  Revision 1.241  2002/03/07 14:14:23  lazarus
  MG: fixed find declaration new nodecache flags, find next

  Revision 1.240  2002/03/05 14:52:15  lazarus
  MG: updates for codetools defines

  Revision 1.239  2002/03/05 08:14:58  lazarus
  MG: updates for codetools defines editor

  Revision 1.238  2002/03/02 11:08:36  lazarus
  MG: fixed method search diff proc, fixed synedit insert in empty line, small fixes, started define editor

  Revision 1.237  2002/03/01 15:51:06  lazarus
  MG: added selection keys and nil operand

  Revision 1.236  2002/02/28 12:09:07  lazarus
  MG: fixes, code creation policies, keymap categories, menu shortcuts

  Revision 1.235  2002/02/25 23:04:23  lazarus
  MG: added close all menuitem

  Revision 1.234  2002/02/25 22:56:55  lazarus
  MG: fixed resetting error line before compiling

  Revision 1.233  2002/02/25 19:17:27  lazarus
  MG: fixed restore window positions

  Revision 1.232  2002/02/25 16:48:10  lazarus
  MG: new IDE window layout system

  Revision 1.231  2002/02/24 20:51:22  lazarus
  Improved TSpeedButton (Glyph, Spacing, Margin, drawing)
  Added PageCount to TNotebook
  Optimized component selection buttons a bit.

  Revision 1.230  2002/02/22 17:39:40  lazarus
  MG: improved LinkScanner error messages

  Revision 1.229  2002/02/22 14:25:00  lazarus
  MG: fixed completion box for templates and identifiers

  Revision 1.228  2002/02/22 14:05:55  lazarus
  MG: edit menu reconnected to source editor

  Revision 1.227  2002/02/20 23:33:23  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.226  2002/02/20 16:01:43  lazarus
  MG: fixed editor opts general flags

  Revision 1.225  2002/02/18 22:46:10  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.224  2002/02/17 19:51:10  lazarus
  MG: fixed running project

  Revision 1.223  2002/02/17 19:34:44  lazarus
  MG: fixed view units/forms

  Revision 1.222  2002/02/11 15:12:00  lazarus
  MG: started OI events

  Revision 1.221  2002/02/10 20:44:00  lazarus
  MG: fixed a node cache range bug

  Revision 1.220  2002/02/09 22:24:50  lazarus
  MG: get compatible published methods now works

  Revision 1.219  2002/02/09 21:09:19  lazarus
  MG: fixed sourcenotebook closing and form-unit switching

  Revision 1.218  2002/02/09 20:32:08  lazarus
  MG: many fixes on my way to events

  Revision 1.217  2002/02/08 21:08:00  lazarus
  MG: saving of virtual project files will now save the whole project

  Revision 1.214  2002/02/07 18:18:59  lazarus
  MG: fixed deactivating hints

  Revision 1.213  2002/02/07 13:48:47  lazarus
  MG: fixed mem leak FBreakPts

  Revision 1.212  2002/02/06 22:23:13  lazarus
  MG: codetools now know the compiler options

  Revision 1.211  2002/02/06 09:37:40  lazarus
  MG: outputfilter now recognizes, if compiler in sub directory

  Revision 1.210  2002/02/06 08:58:27  lazarus
  MG: fixed compiler warnings and asking to create non existing files

  Revision 1.209  2002/02/05 23:16:47  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.208  2002/02/03 00:23:54  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.207  2002/01/27 19:08:43  lazarus
  MWE: Removed ^M

  Revision 1.206  2002/01/24 14:12:52  lazarus
  MG: added build lazarus feature and config dialog

  Revision 1.205  2002/01/23 22:12:54  lazarus
  MG: external tool output parsing for fpc and make messages

  Revision 1.204  2002/01/23 20:07:20  lazarus
  MG: added outputfilter

  Revision 1.203  2002/01/21 14:17:44  lazarus
  MG: added find-block-start and renamed find-block-other-end

  Revision 1.202  2002/01/17 11:00:00  lazarus
  MG: increased IDE version to 0.8.2 alpha

  Revision 1.201  2002/01/15 20:21:37  lazarus
  MG: jump history for find declaration

  Revision 1.200  2002/01/13 12:46:17  lazarus
  MG: fixed linker options, compiler options dialog

  Revision 1.199  2002/01/11 20:41:52  lazarus
  MG: added  guess unclosed block

  Revision 1.197  2002/01/02 13:32:52  lazarus
  MG: fixed clean abort of project loading

  Revision 1.196  2001/12/31 22:45:41  lazarus
  Took out some test code.
  Shane

  Revision 1.195  2001/12/31 22:42:59  lazarus
  Added a TViewColumn editor to be used in the object inspector as TViewColumn's property editor.
  Shane

  Revision 1.194  2001/12/28 11:01:20  lazarus
  MG: fixed save as with lfm and lrs files

  Revision 1.193  2001/12/20 19:11:22  lazarus
  Changed the delay for the hints from 100 miliseconds to 500.  I'm hoping this reduces the crashing for some people until I determine the problem.
  Shane

  Revision 1.192  2001/12/19 22:09:13  lazarus
  MG: added GUID and alias parsing, added DoJumpToCodeToolBossError

  Revision 1.191  2001/12/19 20:28:50  lazarus
  Enabled Alignment of columns in a TListView.
  Shane

  Revision 1.190  2001/12/18 21:09:58  lazarus
  MOre additions for breakpoints dialog
  Added a TSynEditPlugin in SourceEditor to get notified of lines inserted and deleted from the source.
  Shane

  Revision 1.189  2001/12/18 21:00:59  lazarus
  MG: compiler, fpc source and lazarus src can now be changed without restart

  Revision 1.188  2001/12/17 19:41:05  lazarus
  MG: added binary file recognition and readonly recognition

  Revision 1.186  2001/12/17 11:16:08  lazarus
  MG: fixed open file key in source editor

  Revision 1.185  2001/12/16 22:24:54  lazarus
  MG: changes for new compiler 20011216

  Revision 1.183  2001/12/16 11:20:26  lazarus
  MG: find declaration for uses sections

  Revision 1.182  2001/12/15 22:58:09  lazarus
  MG: fixed code completion in virtual files

  Revision 1.181  2001/12/15 10:57:48  lazarus
  MG: added hint checkboxes to environment options

  Revision 1.180  2001/12/14 18:38:55  lazarus
  Changed code for TListView
  Added a generic Breakpoints dialog
  Shane

  Revision 1.179  2001/12/13 23:09:57  lazarus
  MG: enhanced code caching, fixed CursorToCleanPos and beautify statement

  Revision 1.178  2001/12/12 16:49:14  lazarus
  Added code to disable save button when the active unit is not "modified".
  Shane

  Revision 1.177  2001/12/12 15:12:31  lazarus
  MG: added file path to files in TOpenDialog

  Revision 1.176  2001/12/12 14:25:03  lazarus
  Changes to allow multiple files being opened in main.pp
  Shane

  Revision 1.175  2001/12/11 16:51:36  lazarus
  Modified the Watches dialog
  Shane

  Revision 1.174  2001/12/11 15:43:35  lazarus
  MG: TCodeBuffer.LoadFromFile now checks file date

  Revision 1.172  2001/12/11 11:14:10  lazarus
  MG: fixed save project, saving units twice

  Revision 1.171  2001/12/11 09:34:32  lazarus
  MG: fixed open file at cursor

  Revision 1.170  2001/12/10 23:03:18  lazarus
  MG: enhanced open file at cursor to read more than one word

  Revision 1.169  2001/12/10 22:39:36  lazarus
  MG: added perl highlighter

  Revision 1.168  2001/12/10 16:22:40  lazarus
  MG: started open file at cursor

  Revision 1.167  2001/12/10 14:32:57  lazarus
  MOdified the Watches dialog and added the lfm and lrs files for it and the insert watch dialog.

  Shane

  Revision 1.166  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.165  2001/12/07 20:12:13  lazarus
  Added a watch dialog.
  Shane

  Revision 1.164  2001/12/05 18:19:10  lazarus
  MG: added calendar to allunits and removed unused vars

  Revision 1.163  2001/12/04 14:28:04  lazarus
  Added hints to the main ide.
  Shane

  Revision 1.162  2001/12/02 13:05:33  lazarus
  MG: reduced output

  Revision 1.161  2001/12/02 11:03:35  lazarus
  MG: added default pascal file extension option

  Revision 1.160  2001/12/01 22:17:26  lazarus
  MG: added jump-history

  Revision 1.159  2001/11/27 15:06:11  lazarus
  MG: added multi language syntax hilighting

  Revision 1.158  2001/11/22 14:28:30  lazarus
  MG: cropped all component icons

  Revision 1.157  2001/11/22 10:37:23  lazarus
  MG: moved lazres, added images/README

  Revision 1.156  2001/11/21 19:32:31  lazarus
  TComboBox can now be moved in FormEditor
  Shane

  Revision 1.155  2001/11/21 13:09:50  lazarus
  MG: moved executable check to ideprocs.pp

  Revision 1.153  2001/11/20 19:39:45  lazarus
  MG: DoRunProject writes the programfilename

  Revision 1.152  2001/11/20 18:30:30  lazarus
  Pressing DEL when form is the only thing selected in designer no longer crashes Lazarus.
  Shane

  Revision 1.151  2001/11/20 15:09:21  lazarus
  MG: open project now only opens lpi files

  Revision 1.150  2001/11/19 22:01:25  lazarus
  MG: run button and menu run  now builds+runs

  Revision 1.148  2001/11/19 15:23:17  lazarus
  MG: added quick syntax check via codetools

  Revision 1.147  2001/11/19 12:15:03  lazarus
  MG: added dirty about lazarus dlg

  Revision 1.146  2001/11/17 10:16:23  lazarus
  MG: clear define cache on changing env paths

  Revision 1.145  2001/11/15 13:49:49  lazarus
  MG: fixed open non existing file and unitname in save project as

  Revision 1.144  2001/11/14 19:10:00  lazarus
  MG: fixes for parser and linkscanner and small cleanups

  Revision 1.143  2001/11/14 17:46:54  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.141  2001/11/12 16:56:04  lazarus
  MG: CLIPBOARD

  Revision 1.140  2001/11/09 18:39:11  lazarus
  MG: turned back to stable ground (use old process.pp)

  Revision 1.139  2001/11/09 18:15:20  lazarus
  MG: added external tools

  Revision 1.138  2001/11/07 16:14:11  lazarus
  MG: fixes for the new compiler

  Revision 1.137  2001/11/06 15:47:31  lazarus
  MG: added build all

  Revision 1.136  2001/11/06 12:20:30  lazarus
  MG: added Run Parameter Options - not enabled yet

  Revision 1.135  2001/11/05 18:18:13  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.134  2001/11/05 00:12:50  lazarus
  MWE: First steps of a debugger.

  Revision 1.133  2001/11/03 08:37:34  lazarus
  MG: fixed errorline showing, resource adding and published var editing and added make cleanall

  Revision 1.132  2001/11/01 21:30:32  lazarus
  Changes to Messagebox.
  Added line to CodeTools to prevent duplicate USES entries.

  Revision 1.131  2001/11/01 18:48:48  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.130  2001/10/31 18:09:51  lazarus
  MG: fixed DirectoryExists

  Revision 1.129  2001/10/31 16:29:20  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.128  2001/10/26 20:36:48  lazarus
  Added an OnSelectionChanged event in Main.pp fired by MSgView dialog.  This fires when the ListBox gets clicked on.
  This allows the editor to highlight different lines when you click on different error messages.
  Shane

  Revision 1.126  2001/10/23 09:13:51  lazarus
  MG: fixed TestProject

  Revision 1.125  2001/10/18 13:34:03  lazarus
  MG: keys for debugging

  Revision 1.123  2001/10/17 13:43:15  lazarus
  MG: added find previous to source editor

  Revision 1.122  2001/10/16 14:19:10  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.121  2001/10/15 17:41:30  lazarus
  MG: fixed splashform showing

  Revision 1.115  2001/10/09 09:46:49  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.113  2001/07/31 18:57:48  lazarus
  MG: fixed source ediotr statusbar filename

  Revision 1.111  2001/07/29 20:33:23  lazarus
  MG: bugfixed event propeditor, DoJumpToMethod with searchpath

  Revision 1.110  2001/07/10 10:44:15  lazarus
  MG: save unit only if modified

  Revision 1.105  2001/07/01 15:55:43  lazarus
  MG: JumpToCompilerMessage now centered in source editor

  Revision 1.104  2001/06/27 21:43:23  lazarus
  MG: added project bookmark support

  Revision 1.103  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.102  2001/06/06 12:30:40  lazarus
  MG: bugfixes

  Revision 1.100  2001/06/05 10:27:50  lazarus
  MG: saving recent file lists

  Revision 1.98  2001/05/29 08:16:26  lazarus
  MG: bugfixes + starting programs

  Revision 1.97  2001/05/28 10:00:54  lazarus
  MG: removed unused code. fixed editor name bug.

  Revision 1.96  2001/05/27 11:52:00  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.92  2001/04/21 14:50:21  lazarus
  MG: bugfix for mainunits ext <> .lpr

  Revision 1.91  2001/04/13 17:56:16  lazarus
  MWE:
  * Moved menubar outside clientarea
  * Played a bit with the IDE layout
  * Moved the creation of the toolbarspeedbuttons to a separate function

  Revision 1.90  2001/04/04 13:58:50  lazarus
  Added some changes to compreg.pp

  Revision 1.89  2001/04/04 13:55:34  lazarus
  MG: finished TComponentPropertyEditor, added OnModified to oi, cfe and designer

  Revision 1.88  2001/04/04 12:20:34  lazarus
  MG: added  add to/remove from project, small bugfixes

  Revision 1.86  2001/03/31 13:35:22  lazarus
  MG: added non-visual-component code to IDE and LCL

  Revision 1.85  2001/03/29 13:11:33  lazarus
  MG: fixed loading program file bug

  Revision 1.84  2001/03/29 12:38:59  lazarus
  MG: new environment opts, ptApplication bugfixes

  Revision 1.83  2001/03/28 14:08:45  lazarus
  MG: added backup code and fixed removing controls

  Revision 1.82  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.81  2001/03/26 14:52:30  lazarus
  MG: TSourceLog + compiling bugfixes

  Revision 1.75  2001/03/19 14:00:46  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.74  2001/03/12 18:57:31  lazarus
  MG: new designer and controlselection code

  Revision 1.68  2001/03/03 11:06:15  lazarus
  added project support, codetools

  Revision 1.62  2001/02/22 17:04:57  lazarus
  added environment options + killed ide unit circles

  Revision 1.61  2001/02/21 22:55:24  lazarus
  small bugfixes + added TOIOptions

  Revision 1.60  2001/02/20 16:53:24  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.59  2001/02/16 19:13:29  lazarus
  Added some functions
  Shane

  Revision 1.58  2001/02/08 06:09:25  lazarus
  Partially implemented Save Project As menu selection.               CAW

  Revision 1.57  2001/02/06 13:38:57  lazarus
  Fixes from Mattias for EditorOPtions
  Fixes to COmpiler that should allow people to compile if their path is set up.
  Changes to code completion.
  Shane

  Revision 1.56  2001/02/04 04:18:11  lazarus
  Code cleanup and JITForms bug fix.
  Shane

  Revision 1.55  2001/02/02 14:23:37  lazarus
  Start of code completion code.
  Shane

  Revision 1.54  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.52  2001/01/31 13:03:33  lazarus
  Commitng source with new editor.
  Shane

  Revision 1.51  2001/01/31 06:25:35  lazarus
  Removed global unit.
  Removed and commented all references to TUnitInfo.

  Revision 1.50  2001/01/29 05:46:30  lazarus
  Moved Project Options and Compiler Options menus to the Project menu.
  Added Project property to TMainIDE class to allow the project to be
  accessed from other units.                                            CAW

  Revision 1.49  2001/01/18 13:27:30  lazarus
  Minor changees
  Shane

  Revision 1.48  2001/01/16 23:30:45  lazarus
  trying to determine what's crashing LAzarus on load.
  Shane

  Revision 1.45  2001/01/15 20:55:44  lazarus
  Changes for loading filesa
  Shane

  Revision 1.44  2001/01/15 18:25:51  lazarus
  Fixed a stupid error I caused by using a variable as an index in main.pp and this variable sometimes caused an exception because the index was out of range.
  Shane

  Revision 1.43  2001/01/14 03:56:57  lazarus
  Shane

  Revision 1.42  2001/01/13 06:11:06  lazarus
  Minor fixes
  Shane

  Revision 1.41  2001/01/13 03:09:37  lazarus
  Minor changes
  Shane

  Revision 1.40  2001/01/12 18:46:49  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.39  2001/01/12 18:10:53  lazarus
  Changes for keyevents in the editor.
  Shane

  Revision 1.38  2001/01/09 21:06:06  lazarus
  Started taking KeyDown messages in TDesigner
  Shane

  Revision 1.37  2001/01/09 18:23:20  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.36  2001/01/08 23:48:33  lazarus
  MWE:
    ~ Changed makefiles
    ~ Removed testform from lararus and changed it into program
    * some formatting

  Revision 1.35  2001/01/06 06:28:47  lazarus
  Made Designer control the control movement and such.  I am now using ISDesignMsg to move the controls.
  Shane

  Revision 1.32  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.30  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.29  2000/12/29 20:43:17  lazarus
  I added the run button with an Enable and disable icon

  Revision 1.25  2000/12/29 13:35:50  lazarus
  Mattias submitted new lresources.pp and lazres.pp files.
  Shane

  Revision 1.23  2000/12/21 20:28:33  lazarus
  Project - RUN will run the program IF the program is the active unit in the Editor.
  Shane

  Revision 1.22  2000/12/20 20:04:30  lazarus
  Made PRoject Build compile the active unit.  This way we can actually play with it by compiling units.

  Revision 1.19  2000/12/19 18:43:12  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.18  2000/12/15 18:25:16  lazarus
  Changes from Mattias and I.
  Shane

  Revision 1.16  2000/12/01 20:23:34  lazarus
  renamed Object_Inspector and Prop_edits by removing the underline.
  Shane

  Revision 1.5  2000/08/10 13:22:51  lazarus
  Additions for the FIND dialog
  Shane

  Revision 1.4  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.2  2000/08/07 19:15:05  lazarus
  Added the Search menu to the IDE.
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

}

