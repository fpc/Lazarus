{  $Id$  }
{
 /***************************************************************************
                    main.pp  -  the "integrated" in IDE
                    -----------------------------------
  TMainIDE is the main controlling and instance of the IDE, which connects the
  various parts of the IDE.

  main.pp      - TMainIDE = class(TMainIDEBase)
                   The highest manager/boss of the IDE. Only lazarus.pp uses
                   this unit.
  mainbase.pas - TMainIDEBase = class(TMainIDEInterface)
                   The ancestor class used by (and only by) the other
                   bosses/managers like debugmanager, pkgmanager.
  mainintf.pas - TMainIDEInterface = class(TLazIDEInterface)
                   The interface class of the top level functions of the IDE.
                   TMainIDEInterface is used by functions/units, that uses
                   several different parts of the IDE (designer, source editor,
                   codetools), so they can't be added to a specific boss and
                   which are yet too small to become a boss of their own.
  lazideintf.pas - TLazIDEInterface = class(TComponent)
                   For designtime packages, this is the interface class of the
                   top level functions of the IDE.


                 Initial Revision : Sun Mar 28 23:15:32 CST 1999


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

// TODO: Test on all platforms
{$IFNDEF DisableAsyncProcess}
  {$IFDEF Linux}
    {$IFDEF CPUI386}
      {off $DEFINE UseAsyncProcess}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  // fpc packages
  Classes, SysUtils, Process, AsyncProcess, TypInfo,
  // lcl
  LCLProc, LCLMemManager, LCLType, LCLIntf, LMessages, LResources, StdCtrls,
  Forms, Buttons, Menus, FileUtil, Controls, GraphType, Graphics, ExtCtrls,
  Dialogs, InterfaceBase,
  // codetools
  AVL_Tree, Laz_XMLCfg,
  CodeToolsStructs, CodeToolManager, CodeCache, DefineTemplates,
  // synedit
  SynEditKeyCmds,
  // IDE interface
  AllIDEIntf, BaseIDEIntf, ObjectInspector, PropEdits, MacroIntf, IDECommands,
  SrcEditorIntf, NewItemIntf, IDEExternToolIntf, IDEMsgIntf, PackageIntf,
  ProjectIntf, MenuIntf, LazIDEIntf, IDEDialogs,
  
  {$ifdef EnableNewDialogs}
  // some dialog units really need to get splitted up so structures and dialog
  // are separated. To avoid conflicts the new units are declared first
  ProjectOpts_new, CompilerOptionsDlg_new, EditorOptions_new, EnvironmentOpts_new,
  {$endif}
  // protocol
  IDEProtocol,
  // compile
  Compiler, CompilerOptions, CompilerOptionsDlg, CheckCompilerOpts,
  W32VersionInfo, ImExportCompilerOpts,
  // projects
  Project, ProjectDefs, NewProjectDlg, ProjectOpts,
  PublishProjectDlg, ProjectInspector,
  // help manager
  IDEContextHelpEdit, HelpManager,
  // designer
  JITForm, JITForms, ComponentPalette, ComponentList, ComponentReg,
  ObjInspExt, Designer, FormEditor, CustomFormEditor,
  ControlSelection, AnchorEditor,
  {$DEFINE UseNewMenuEditor}
  {$IFDEF UseNewMenuEditor}
  MenuEditorForm,
  {$ELSE}
  MenuPropEdit,
  {$ENDIF}
  //LRT stuff
  LrtPoTools,
  // debugger
  RunParamsOpts, BaseDebugManager, DebugManager,
  // packager
  PackageSystem, PkgManager, BasePkgManager,
  // source editing
  UnitEditor, CodeToolsOptions, IDEOptionDefs, CheckLFMDlg,
  CodeToolsDefines, DiffDialog, DiskDiffsDialog, UnitInfoDlg, EditorOptions,
  SourceEditProcs, MsgQuickFixes, ViewUnit_dlg,
  // converter
  DelphiUnit2Laz, DelphiProject2Laz, LazXMLForms,
  // rest of the ide
  Splash, IDEDefs, LazarusIDEStrConsts, LazConf, MsgView, SearchResultView,
  CodeTemplatesDlg, CodeBrowser,
  PublishModule, EnvironmentOpts, TransferMacros, KeyMapping, IDETranslations,
  IDEProcs, ExtToolDialog, ExtToolEditDlg, OutputFilter,
  BuildLazDialog, MiscOptions, InputHistory, UnitDependencies, ClipBoardHistory,
  ProcessList, InitialSetupDlgs, NewDialog, MakeResStrDlg, ToDoList,
  DialogProcs, FindReplaceDialog, FindInFilesDlg, CodeExplorer, BuildFileDlg,
  ProcedureList, ExtractProcDlg, FindRenameIdentifier,
  CleanDirDlg, CodeContextForm, AboutFrm, BuildManager,
  // main ide
  MainBar, MainIntf, MainBase;

type

  { TMainIDE }

  TMainIDE = class(TMainIDEBase)
    // event handlers

    procedure MainIDEFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MainIDEFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure OnApplicationActivate(Sender: TObject);
    procedure OnApplicationKeyDown(Sender: TObject;
                                   var Key: Word; Shift: TShiftState);
    procedure OnScreenRemoveForm(Sender: TObject; AForm: TCustomForm);
    procedure OnRemoteControlTimer(Sender: TObject);

    // file menu
    procedure mnuFileClicked(Sender: TObject);
    procedure mnuNewUnitClicked(Sender: TObject);
    procedure mnuNewFormClicked(Sender: TObject);
    procedure mnuNewOtherClicked(Sender: TObject);
    procedure mnuOpenClicked(Sender: TObject);
    procedure mnuOpenRecentClicked(Sender: TObject);
    procedure mnuRevertClicked(Sender: TObject);
    procedure mnuSaveClicked(Sender: TObject);
    procedure mnuSaveAsClicked(Sender: TObject);
    procedure mnuSaveAllClicked(Sender: TObject);
    procedure mnuCloseClicked(Sender: TObject);
    procedure mnuCloseAllClicked(Sender: TObject);
    procedure mnuCleanDirectoryClicked(Sender: TObject);
    procedure mnuRestartClicked(Sender: TObject);
    procedure mnuQuitClicked(Sender: TObject);

    // edit menu
    procedure mnuEditClicked(Sender: TObject);
    procedure mnuEditUndoClicked(Sender: TObject);
    procedure mnuEditRedoClicked(Sender: TObject);
    procedure mnuEditCutClicked(Sender: TObject);
    procedure mnuEditCopyClicked(Sender: TObject);
    procedure mnuEditPasteClicked(Sender: TObject);
    procedure mnuEditIndentBlockClicked(Sender: TObject);
    procedure mnuEditUnindentBlockClicked(Sender: TObject);
    procedure mnuEditEncloseBlockClicked(Sender: TObject);
    procedure mnuEditUpperCaseBlockClicked(Sender: TObject);
    procedure mnuEditLowerCaseBlockClicked(Sender: TObject);
    procedure mnuEditTabsToSpacesBlockClicked(Sender: TObject);
    procedure mnuEditCommentBlockClicked(Sender: TObject);
    procedure mnuEditUncommentBlockClicked(Sender: TObject);
    procedure mnuEditConditionalBlockClicked(Sender: TObject);
    procedure mnuEditSortBlockClicked(Sender: TObject);
    procedure mnuEditSelectionBreakLinesClicked(Sender: TObject);
    procedure mnuEditSelectAllClick(Sender: TObject);
    procedure mnuEditSelectCodeBlockClick(Sender: TObject);
    procedure mnuEditSelectToBraceClick(Sender: TObject);
    procedure mnuEditSelectLineClick(Sender: TObject);
    procedure mnuEditSelectParagraphClick(Sender: TObject);
    procedure mnuEditCompleteCodeClicked(Sender: TObject);
    procedure mnuEditExtractProcClicked(Sender: TObject);
    procedure mnuEditInsertCharacterClicked(Sender: TObject);

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
    procedure mnuEditInsertModifiedLGPLNoticeClick(Sender: TObject);
    procedure mnuEditInsertUsernameClick(Sender: TObject);
    procedure mnuEditInsertDateTimeClick(Sender: TObject);
    procedure mnuEditInsertChangeLogEntryClick(Sender: TObject);
    procedure mnuInsertTodo(Sender: TObject);

    // search menu
    procedure mnuSearchFindInFiles(Sender: TObject);
    procedure mnuSearchFindIdentifierRefsClicked(Sender: TObject);
    procedure mnuSearchRenameIdentifierClicked(Sender: TObject);
    procedure mnuSearchFindBlockOtherEnd(Sender: TObject);
    procedure mnuSearchFindBlockStart(Sender: TObject);
    procedure mnuSearchFindDeclaration(Sender: TObject);
    procedure mnuFindDeclarationClicked(Sender: TObject);
    procedure mnuOpenFileAtCursorClicked(Sender: TObject);
    procedure mnuGotoIncludeDirectiveClicked(Sender: TObject);
    procedure mnuSearchProcedureList(Sender: TObject);

    // view menu
    procedure mnuViewInspectorClicked(Sender: TObject);
    procedure mnuViewSourceEditorClicked(Sender: TObject);
    procedure mnuViewUnitsClicked(Sender: TObject);
    procedure mnuViewFormsClicked(Sender: TObject);
    procedure mnuViewUnitDependenciesClicked(Sender: TObject);
    procedure mnuViewUnitInfoClicked(Sender: TObject);
    procedure mnuViewLazDocClicked(Sender: TObject);
    procedure mnuViewCodeExplorerClick(Sender: TObject);
    procedure mnuViewCodeBrowserClick(Sender: TObject);
    procedure mnuViewComponentsClick(Sender: TObject);
    procedure mnuViewMessagesClick(Sender: TObject);
    procedure mnuViewSearchResultsClick(Sender: TObject);
    procedure mnuToggleFormUnitClicked(Sender: TObject);
    procedure mnuViewAnchorEditorClicked(Sender: TObject);
    procedure mnuViewComponentPaletteClicked(Sender: TObject);
    procedure mnuViewIDESpeedButtonsClicked(Sender: TObject);

    // project menu
    procedure mnuNewProjectClicked(Sender: TObject);
    procedure mnuNewProjectFromFileClicked(Sender: TObject);
    procedure mnuOpenProjectClicked(Sender: TObject);
    procedure mnuCloseProjectClicked(Sender: TObject);
    procedure mnuSaveProjectClicked(Sender: TObject);
    procedure mnuSaveProjectAsClicked(Sender: TObject);
    procedure mnuPublishProjectClicked(Sender: TObject);
    procedure mnuProjectInspectorClicked(Sender: TObject);
    procedure mnuAddToProjectClicked(Sender: TObject);
    procedure mnuRemoveFromProjectClicked(Sender: TObject);
    procedure mnuViewProjectSourceClicked(Sender: TObject);
    procedure mnuViewProjectTodosClicked(Sender: TObject);
    procedure mnuProjectOptionsClicked(Sender: TObject);
    {$ifdef EnableNewDialogs}
    procedure mnuProjectOptionsClicked_new(Sender: TObject);
    {$endif}
    procedure mnuProjectCompilerSettingsClicked(Sender: TObject);
    {$ifdef EnableNewDialogs}
    procedure mnuProjectCompilerSettingsClicked_new(Sender: TObject);
    {$endif}


    // run menu
    procedure mnuBuildProjectClicked(Sender: TObject);
    procedure mnuBuildAllProjectClicked(Sender: TObject);
    procedure mnuQuickCompileProjectClicked(Sender: TObject);
    procedure mnuAbortBuildProjectClicked(Sender: TObject);
    procedure mnuRunProjectClicked(Sender: TObject);
    procedure mnuPauseProjectClicked(Sender: TObject);
    procedure mnuStepIntoProjectClicked(Sender: TObject);
    procedure mnuStepOverProjectClicked(Sender: TObject);
    procedure mnuRunToCursorProjectClicked(Sender: TObject);
    procedure mnuStopProjectClicked(Sender: TObject);
    procedure mnuRunParametersClicked(Sender: TObject);
    procedure mnuBuildFileClicked(Sender: TObject);
    procedure mnuRunFileClicked(Sender: TObject);
    procedure mnuConfigBuildFileClicked(Sender: TObject);

    // components menu
    // see pkgmanager.pas

    // tools menu
    procedure mnuToolConfigureClicked(Sender: TObject);
    procedure mnuToolSyntaxCheckClicked(Sender: TObject);
    procedure mnuToolGuessUnclosedBlockClicked(Sender: TObject);
    procedure mnuToolGuessMisplacedIFDEFClicked(Sender: TObject);
    procedure mnuToolMakeResourceStringClicked(Sender: TObject);
    procedure mnuToolDiffClicked(Sender: TObject);
    procedure mnuToolConvertDFMtoLFMClicked(Sender: TObject);
    procedure mnuToolCheckLFMClicked(Sender: TObject);
    procedure mnuToolConvertDelphiUnitClicked(Sender: TObject);
    procedure mnuToolConvertDelphiProjectClicked(Sender: TObject);
    procedure mnuToolConvertDelphiPackageClicked(Sender: TObject);
    procedure mnuToolBuildLazarusClicked(Sender: TObject);
    procedure mnuToolConfigBuildLazClicked(Sender: TObject);
    procedure mnuCustomExtToolClick(Sender: TObject);

    // environment menu
    procedure mnuEnvGeneralOptionsClicked(Sender: TObject);
    {$ifdef EnableNewDialogs}
    procedure mnuEnvGeneralOptionsClicked_new(Sender: TObject);
    {$endif}
    procedure mnuEnvEditorOptionsClicked(Sender: TObject);
    {$ifdef EnableNewDialogs}
    procedure mnuEnvEditorOptionsClicked_new(Sender: TObject);
    {$endif}
    procedure mnuEnvCodeTemplatesClicked(Sender: TObject);
    procedure mnuEnvCodeToolsOptionsClicked(Sender: TObject);
    procedure mnuEnvCodeToolsDefinesEditorClicked(Sender: TObject);
    procedure mnuEnvRescanFPCSrcDirClicked(Sender: TObject);

    // windows menu

    // help menu
    // see helpmanager.pas

    procedure OpenFileDownArrowClicked(Sender: TObject);
    procedure mnuOpenFilePopupClick(Sender: TObject);
  public
    // Global IDE events
    procedure OnProcessIDECommand(Sender: TObject; Command: word;
                                  var Handled: boolean);
    procedure OnExecuteIDEShortCut(Sender: TObject;
                       var Key: word; Shift: TShiftState;
                       IDEWindowClass: TCustomFormClass);
    function OnExecuteIDECommand(Sender: TObject; Command: word): boolean;
    function OnSelectDirectory(const Title, InitialDir: string): string;
    procedure OnInitIDEFileDialog(AFileDialog: TFileDialog);
    procedure OnStoreIDEFileDialog(AFileDialog: TFileDialog);
    function OnIDEMessageDialog(const aCaption, aMsg: string;
                                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                                const HelpKeyword: string): Integer;
    function OnIDEQuestionDialog(const aCaption, aMsg: string;
                                 DlgType: TMsgDlgType; Buttons: array of const;
                                 const HelpKeyword: string): Integer;

    // Environment options dialog events
    procedure OnLoadEnvironmentSettings(Sender: TObject;
      TheEnvironmentOptions: TEnvironmentOptions);
    procedure OnSaveEnvironmentSettings(Sender: TObject;
      TheEnvironmentOptions: TEnvironmentOptions);
    procedure DoShowEnvGeneralOptions(StartPage: TEnvOptsDialogPage);

    // SourceNotebook events
    procedure OnSrcNoteBookActivated(Sender: TObject);
    procedure OnSrcNoteBookAddJumpPoint(ACaretXY: TPoint; ATopLine: integer;
      APageIndex: integer; DeleteForwardHistory: boolean);
    procedure OnSrcNoteBookCtrlMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftstate; X, Y: Integer);
    procedure OnSrcNotebookDeleteLastJumPoint(Sender: TObject);
    procedure OnSrcNotebookEditorVisibleChanged(Sender: TObject);
    procedure OnSrcNotebookEditorChanged(Sender: TObject);
    procedure OnSrcNotebookCurCodeBufferChanged(Sender: TObject);
    procedure OnSrcNotebookFileNew(Sender: TObject);
    procedure OnSrcNotebookFileOpen(Sender: TObject);
    procedure OnSrcNotebookFileOpenAtCursor(Sender: TObject);
    procedure OnSrcNotebookFileSave(Sender: TObject);
    procedure OnSrcNotebookFileSaveAs(Sender: TObject);
    procedure OnSrcNotebookFileClose(Sender: TObject; InvertedClose: boolean);
    procedure OnSrcNotebookFindDeclaration(Sender: TObject);
    procedure OnSrcNotebookInitIdentCompletion(Sender: TObject;
      JumpToError: boolean; out Handled, Abort: boolean);
    procedure OnSrcNotebookShowCodeContext(JumpToError: boolean;
                                           out Abort: boolean);
    procedure OnSrcNotebookJumpToHistoryPoint(var NewCaretXY: TPoint;
      var NewTopLine, NewPageIndex: integer; JumpAction: TJumpHistoryAction);
    procedure OnSrcNotebookMovingPage(Sender: TObject;
      OldPageIndex, NewPageIndex: integer);
    procedure OnSrcNotebookReadOnlyChanged(Sender: TObject);
    procedure OnSrcNotebookSaveAll(Sender: TObject);
    procedure OnSrcNotebookShowHintForSource(SrcEdit: TSourceEditor;
                                           ClientPos: TPoint; CaretPos: TPoint);
    procedure OnSrcNoteBookShowUnitInfo(Sender: TObject);
    procedure OnSrcNotebookToggleFormUnit(Sender: TObject);
    procedure OnSrcNotebookToggleObjectInsp(Sender: TObject);
    procedure OnSrcNotebookViewJumpHistory(Sender: TObject);
    procedure OnSrcNotebookShowSearchResultsView(Sender: TObject);
    procedure OnSrcNoteBookPopupMenu(const AddMenuItemProc: TAddMenuItemProc);

    // ObjectInspector + PropertyEditorHook events
    procedure OIOnSelectPersistents(Sender: TObject);
    procedure OIOnShowOptions(Sender: TObject);
    procedure OIOnDestroy(Sender: TObject);
    procedure OIRemainingKeyDown(Sender: TObject; var Key: Word;
       Shift: TShiftState);
    procedure OIOnAddToFavourites(Sender: TObject);
    procedure OIOnRemoveFromFavourites(Sender: TObject);
    procedure OIOnFindDeclarationOfProperty(Sender: TObject);
    function OnPropHookGetMethodName(const Method: TMethod;
                                     CheckOwner: TObject): ShortString;
    procedure OnPropHookGetMethods(TypeData: PTypeData; Proc:TGetStringProc);
    function OnPropHookMethodExists(const AMethodName: ShortString;
                                    TypeData: PTypeData;
                                    var MethodIsCompatible, MethodIsPublished,
                                    IdentIsMethod: boolean): boolean;
    function OnPropHookCreateMethod(const AMethodName:ShortString;
                                    ATypeInfo:PTypeInfo;
                                    APersistent: TPersistent;
                                    const APropertyPath: string): TMethod;
    procedure OnPropHookShowMethod(const AMethodName:ShortString);
    procedure OnPropHookRenameMethod(const CurName, NewName:ShortString);
    function OnPropHookBeforeAddPersistent(Sender: TObject;
                                           APersistentClass: TPersistentClass;
                                           AParent: TPersistent): boolean;
    procedure OnPropHookComponentRenamed(AComponent: TComponent);
    procedure OnPropHookPersistentAdded(APersistent: TPersistent;
                                        Select: boolean);
    procedure OnPropHookPersistentDeleting(APersistent: TPersistent);
    procedure OnPropHookDeletePersistent(var APersistent: TPersistent);
    procedure OnPropHookAddDependency(const AClass: TClass;
                                      const AnUnitName: shortstring);
                                      
    // designer events
    procedure OnDesignerGetSelectedComponentClass(Sender: TObject;
                                 var RegisteredComponent: TRegisteredComponent);
    procedure OnDesignerUnselectComponentClass(Sender: TObject);
    procedure OnDesignerSetDesigning(Sender: TObject; Component: TComponent;
                                     Value: boolean);
    procedure OnDesignerShowOptions(Sender: TObject);
    procedure OnDesignerPasteComponent(Sender: TObject; LookupRoot: TComponent;
                            TxtCompStream: TStream; ParentControl: TWinControl;
                            var NewComponent: TComponent);
    procedure OnDesignerPropertiesChanged(Sender: TObject);
    procedure OnDesignerPersistentDeleted(Sender: TObject;
                                          APersistent: TPersistent);
    procedure OnDesignerModified(Sender: TObject);
    procedure OnDesignerActivated(Sender: TObject);
    procedure OnDesignerCloseQuery(Sender: TObject);
    procedure OnDesignerRenameComponent(ADesigner: TDesigner;
                                 AComponent: TComponent; const NewName: string);
    procedure OnDesignerViewLFM(Sender: TObject);
    procedure OnDesignerSaveAsXML(Sender: TObject);

    // control selection
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

    // compiler options dialog events
    procedure OnCompilerOptionsDialogTest(Sender: TObject);
    procedure OnCompilerOptionsImExport(Sender: TObject);

    // unit dependencies events
    procedure UnitDependenciesViewAccessingSources(Sender: TObject);
    function UnitDependenciesViewGetProjectMainFilename(
        Sender: TObject): string;
    procedure UnitDependenciesViewOpenFile(Sender: TObject;
        const Filename: string);

    // code explorer events
    procedure OnCodeExplorerGetCodeTree(Sender: TObject;
                                        var ACodeTool: TCodeTool);
    procedure OnCodeExplorerGetDirectivesTree(Sender: TObject;
                                          var ADirectivesTool: TDirectivesTool);
    procedure OnCodeExplorerJumpToCode(Sender: TObject; const Filename: string;
                                       const Caret: TPoint; TopLine: integer);

    // view project ToDo list events
    procedure ViewProjectTodosOpenFile(Sender: TObject;
      const Filename: string; const LineNumber: integer);

    // CodeToolBoss events
    procedure OnCodeToolNeedsExternalChanges(Manager: TCodeToolManager;
                                             var Abort: boolean);
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
    procedure OnCodeToolBossFindDefineProperty(Sender: TObject;
               const PersistentClassName, AncestorClassName, Identifier: string;
               var IsDefined: boolean);
    procedure CodeToolBossPrepareTree(Sender: TObject);
    function CTMacroFunctionProject(Data: Pointer): boolean;
    procedure OnCompilerGraphStampIncreased;

    // MessagesView events
    procedure MessagesViewSelectionChanged(sender: TObject);

    // SearchResultsView events
    procedure SearchResultsViewSelectionChanged(sender: TObject);

    // External Tools events
    procedure OnExtToolNeedsOutputFilter(var OutputFilter: TOutputFilter;
                                         var Abort: boolean);
    procedure OnExtToolFreeOutputFilter(OutputFilter: TOutputFilter;
                                        ErrorOccurred: boolean);
  private
    FDisplayState: TDisplayState;
    FLastFormActivated: TCustomForm;// used to find the last form so you can
                                    // display the correct tab
    FCheckingFilesOnDisk: boolean;
    FCheckFilesOnDiskNeeded: boolean;
    FRemoteControlTimer: TTimer;
    FRemoteControlFileValid: boolean;

    FRebuildingCompilerGraphCodeToolsDefinesNeeded: boolean;
    
    FRenamingComponents: TFPList; // list of TComponents currently renaming
    procedure RenameInheritedMethods(AnUnitInfo: TUnitInfo; List: TStrings);
  protected
    procedure SetToolStatus(const AValue: TIDEToolStatus); override;
    function DoResetToolStatus(Interactive: boolean): boolean; override;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;

    procedure OnApplyWindowLayout(ALayout: TIDEWindowLayout);
    procedure AddRecentProjectFileToEnvironment(const AFilename: string);

    // methods for start
    procedure StartProtocol;
    procedure LoadGlobalOptions;
    procedure SetupMainMenu; override;
    procedure SetupStandardIDEMenuItems;
    procedure SetupStandardProjectTypes;
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
    procedure SetupDialogs;
    procedure SetupComponentNoteBook;
    procedure SetupHints;
    procedure SetupOutputFilter;
    procedure SetupObjectInspector;
    procedure SetupFormEditor;
    procedure SetupSourceNotebook;
    procedure SetupTransferMacros;
    procedure SetupCodeMacros;
    procedure SetupControlSelection;
    procedure SetupIDECommands;
    procedure SetupIDEMsgQuickFixItems;
    procedure SetupStartProject;
    procedure SetupRemoteControl;
    procedure ReOpenIDEWindows;
    procedure CloseIDEWindows;
    procedure FreeIDEWindows;
    function CloseQueryIDEWindows: boolean;

    procedure ReloadMenuShortCuts;

    // methods for 'new unit'
    function CreateNewCodeBuffer(Descriptor: TProjectFileDescriptor;
        NewOwner: TObject; NewFilename: string; var NewCodeBuffer: TCodeBuffer;
        var NewUnitName: string): TModalResult;
    function CreateNewForm(NewUnitInfo: TUnitInfo;
        AncestorType: TPersistentClass; ResourceCode: TCodeBuffer): TModalResult;

    // methods for 'save unit'
    function DoShowSaveFileAsDialog(AnUnitInfo: TUnitInfo;
        var ResourceCode: TCodeBuffer): TModalResult;
    function DoSaveUnitComponent(AnUnitInfo: TUnitInfo;
        ResourceCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;
    function DoSaveUnitComponentToBinStream(AnUnitInfo: TUnitInfo;
        var BinCompStream: TExtMemoryStream): TModalResult;
    function DoRemoveDanglingEvents(AnUnitInfo: TUnitInfo;
        OkOnCodeErrors: boolean): TModalResult;
    function DoRenameUnit(AnUnitInfo: TUnitInfo; NewFilename, NewUnitName: string;
        var ResourceCode: TCodeBuffer): TModalresult;

    // methods for 'open unit' and 'open main unit'
    function DoOpenNotExistingFile(const AFileName:string;
        Flags: TOpenFlags): TModalResult;
    function DoOpenUnknownFile(const AFileName:string; Flags: TOpenFlags;
        var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
    procedure DoRestoreBookMarks(AnUnitInfo: TUnitInfo; ASrcEdit:TSourceEditor);
    function DoOpenFileInSourceEditor(AnUnitInfo: TUnitInfo;
        PageIndex: integer; Flags: TOpenFlags): TModalResult;
    function DoLoadResourceFile(AnUnitInfo: TUnitInfo;
        var LFMCode, ResourceCode: TCodeBuffer;
        IgnoreSourceErrors: boolean): TModalResult;
    function DoLoadLFM(AnUnitInfo: TUnitInfo; OpenFlags: TOpenFlags;
                       CloseFlags: TCloseFlags): TModalResult;
    function DoLoadLFM(AnUnitInfo: TUnitInfo; LFMBuf: TCodeBuffer;
                       OpenFlags: TOpenFlags;
                       CloseFlags: TCloseFlags): TModalResult;
    function DoLoadComponentDependencyHidden(AnUnitInfo: TUnitInfo;
                           const AComponentClassName: string; Flags: TOpenFlags;
                           var AComponentClass: TComponentClass;
                           var ComponentUnitInfo: TUnitInfo): TModalResult;

    // methods for 'close unit'
    function CloseUnitComponent(AnUnitInfo: TUnitInfo; Flags: TCloseFlags
                                ): TModalResult;
    function CloseDependingUnitComponents(AnUnitInfo: TUnitInfo;
                                          Flags: TCloseFlags): TModalResult;
    function UnitComponentIsUsed(AnUnitInfo: TUnitInfo;
                                 CheckHasDesigner: boolean): boolean;

    // methods for creating a project
    function CreateProjectObject(ProjectDesc,
                             FallbackProjectDesc: TProjectDescriptor): TProject; override;
    procedure OnLoadProjectInfoFromXMLConfig(TheProject: TProject;
                                             XMLConfig: TXMLConfig;
                                             Merge: boolean);
    procedure OnSaveProjectInfoToXMLConfig(TheProject: TProject;
                         XMLConfig: TXMLConfig; WriteFlags: TProjectWriteFlags);
    procedure OnProjectGetTestDirectory(TheProject: TProject;
                                        out TestDir: string);
    procedure OnProjectChangeInfoFile(TheProject: TProject);

    // methods for 'save project'
    procedure GetMainUnit(var MainUnitInfo: TUnitInfo;
        var MainUnitSrcEdit: TSourceEditor; UpdateModified: boolean);
    procedure SaveSrcEditorProjectSpecificSettings(AnUnitInfo: TUnitInfo);
    procedure SaveSourceEditorProjectSpecificSettings;
    function DoShowSaveProjectAsDialog: TModalResult;
    function DoUpdateLRSFromLFM(const LRSFilename: string): TModalResult;

    // methods for open project, create project from source
    function DoCompleteLoadingProjectInfo: TModalResult;

    // methods for publish project
    procedure OnCopyFile(const Filename: string; var Copy: boolean;
        Data: TObject);
    procedure OnCopyError(const ErrorData: TCopyErrorData;
        var Handled: boolean; Data: TObject);
  public
    class procedure ParseCmdLineOptions;

    constructor Create(TheOwner: TComponent); override;
    procedure StartIDE; override;
    destructor Destroy; override;
    procedure CreateOftenUsedForms; override;
    procedure CreateSearchResultWindow;
    procedure UpdateDefaultPascalFileExtensions;

    // files/units
    function DoNewFile(NewFileDescriptor: TProjectFileDescriptor;
        var NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags; NewOwner: TObject): TModalResult; override;
    function DoNewOther: TModalResult;
    function DoSaveEditorFile(PageIndex:integer;
        Flags: TSaveFlags): TModalResult;
    function DoCloseEditorFile(PageIndex:integer;
        Flags: TCloseFlags):TModalResult; override;
    function DoCloseEditorFile(const Filename: string;
        Flags: TCloseFlags): TModalResult; override;
    function DoOpenEditorFile(AFileName: string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; override;
    function DoOpenFileAtCursor(Sender: TObject): TModalResult;
    function DoOpenFileAndJumpToIdentifier(const AFilename, AnIdentifier: string;
        PageIndex: integer; Flags: TOpenFlags): TModalResult; override;
    function DoOpenFileAndJumpToPos(const AFilename: string;
        const CursorPosition: TPoint; TopLine: integer;
        PageIndex: integer; Flags: TOpenFlags): TModalResult; override;
    function DoRevertEditorFile(const Filename: string): TModalResult; override;
    function DoSaveAll(Flags: TSaveFlags): TModalResult;
    procedure DoRestart;
    procedure DoExecuteRemoteControl;
    function DoOpenMainUnit(Flags: TOpenFlags): TModalResult;
    function DoRevertMainUnit: TModalResult;
    function DoViewUnitsAndForms(OnlyForms: boolean): TModalResult;
    procedure DoViewUnitDependencies;
    procedure DoViewUnitInfo;
    procedure DoShowCodeExplorer;
    procedure DoShowCodeBrowser;
    procedure DoShowComponentList;
    procedure DoShowLazDoc;
    function CreateNewUniqueFilename(const Prefix, Ext: string;
       NewOwner: TObject; Flags: TSearchIDEFileFlags; TryWithoutNumber: boolean
       ): string; override;

    // project(s)
    function DoNewProject(ProjectDesc: TProjectDescriptor): TModalResult; override;
    function DoSaveProject(Flags: TSaveFlags): TModalResult; override;
    function DoCloseProject: TModalResult; override;
    function DoOpenProjectFile(AFileName: string;
                               Flags: TOpenFlags): TModalResult; override;
    function DoPublishProject(Flags: TSaveFlags;
                              ShowDialog: boolean): TModalResult; override;
    function DoImExportCompilerOptions(Sender: TObject): TModalResult; override;
    function DoShowProjectInspector: TModalResult; override;
    function DoAddActiveUnitToProject: TModalResult;
    function DoRemoveFromProjectDialog: TModalResult;
    function DoWarnAmbiguousFiles: TModalResult;
    procedure DoUpdateProjectResourceInfo;
    function DoUpdateProjectAutomaticFiles: TModalResult;
    function DoSaveForBuild: TModalResult; override;
    function DoCheckIfProjectNeedsCompilation(AProject: TProject;
                                         const CompilerFilename, CompilerParams,
                                         SrcFilename: string): TModalResult;
    function DoBuildProject(const AReason: TCompileReason;
                            Flags: TProjectBuildFlags): TModalResult; override;
    function UpdateProjectPOFile(AProject: TProject): TModalResult;
    function DoAbortBuild: TModalResult;
    procedure DoQuickCompile;
    function DoInitProjectRun: TModalResult; override;
    function DoRunProject: TModalResult;
    function SomethingOfProjectIsModified: boolean;
    function DoCreateProjectForProgram(ProgramBuf: TCodeBuffer): TModalResult;
    function DoSaveProjectIfChanged: TModalResult;
    function DoSaveProjectToTestDirectory(Flags: TSaveFlags): TModalResult;
    function DoShowToDoList: TModalResult;
    function DoTestCompilerSettings(
                            TheCompilerOptions: TCompilerOptions): TModalResult;
    function QuitIDE: boolean;
    
    // edit menu
    procedure DoCommand(EditorCommand: integer); override;
    procedure DoSourceEditorCommand(EditorCommand: integer);

    // Delphi conversion
    function DoConvertDFMtoLFM: TModalResult;
    function DoCheckLFMInEditor: TModalResult;
    function DoConvertDelphiUnit(const DelphiFilename: string): TModalResult;
    function DoConvertDelphiProject(const DelphiFilename: string): TModalResult;
    function DoConvertDelphiPackage(const DelphiFilename: string): TModalResult;
    procedure UpdateCustomToolsInMenu;

    // external tools
    function PrepareForCompile: TModalResult; override;
    function OnRunExternalTool(Tool: TIDEExternalToolOptions): TModalResult;
    function DoRunExternalTool(Index: integer): TModalResult;
    function DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags): TModalResult; override;
    function DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult; override;
    function DoBuildFile: TModalResult;
    function DoRunFile: TModalResult;
    function DoConfigBuildFile: TModalResult;
    function GetIDEDirectives(AnUnitInfo: TUnitInfo;
                              DirectiveList: TStrings): TModalResult;

    // useful information methods
    procedure GetCurrentUnit(var ActiveSourceEditor: TSourceEditor;
                             var ActiveUnitInfo: TUnitInfo); override;
    procedure GetUnitWithPageIndex(PageIndex: integer;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); override;
    procedure GetDesignerUnit(ADesigner: TDesigner;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); override;
    function GetDesignerWithProjectFile(AFile: TLazProjectFile;
                             LoadForm: boolean): TIDesigner; override;
    function GetDesignerFormOfSource(AnUnitInfo: TUnitInfo;
                                     LoadForm: boolean): TCustomForm;
    function GetProjectFileWithRootComponent(AComponent: TComponent): TLazProjectFile; override;
    function GetProjectFileWithDesigner(ADesigner: TIDesigner): TLazProjectFile; override;
    procedure GetObjectInspectorUnit(
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); override;
    procedure GetUnitWithForm(AForm: TCustomForm;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); override;
    procedure GetUnitWithPersistent(APersistent: TPersistent;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); override;
    function GetSourceEditorForUnitInfo(AnUnitInfo: TUnitInfo): TSourceEditor; override;
    function CreateSrcEditPageName(const AnUnitName, AFilename: string;
      IgnorePageIndex: integer): string;
    function GetAncestorUnit(AnUnitInfo: TUnitInfo): TUnitInfo;
    function GetAncestorLookupRoot(AnUnitInfo: TUnitInfo): TComponent;
    procedure UpdateSaveMenuItemsAndButtons(UpdateSaveAll: boolean);

    // useful file methods
    function FindUnitFile(const AFilename: string): string; override;
    function FindSourceFile(const AFilename, BaseDirectory: string;
                            Flags: TFindSourceFlags): string; override;
    function FileExistsInIDE(const Filename: string;
                             SearchFlags: TProjectFileSearchFlags): boolean;
    function LoadIDECodeBuffer(var ACodeBuffer: TCodeBuffer;
                               const AFilename: string;
                               Flags: TLoadBufferFlags): TModalResult;
    function DoLoadMemoryStreamFromFile(MemStream: TMemoryStream;
                                        const AFilename:string): TModalResult;
    function DoRenameUnitLowerCase(AnUnitInfo: TUnitInfo;
                                   AskUser: boolean): TModalresult;
    function DoCheckFilesOnDisk(Instantaneous: boolean = false): TModalResult; override;
    function DoPublishModule(Options: TPublishModuleOptions;
                             const SrcDirectory, DestDirectory: string
                             ): TModalResult; override;

    // useful frontend methods
    procedure DoSwitchToFormSrc(var ActiveSourceEditor:TSourceEditor;
      var ActiveUnitInfo:TUnitInfo);
    procedure DoSwitchToFormSrc(ADesigner: TDesigner;
      var ActiveSourceEditor:TSourceEditor; var ActiveUnitInfo:TUnitInfo);
    procedure UpdateCaption; override;
    procedure HideIDE; override;
    procedure HideUnmodifiedDesigners;
    procedure UnhideIDE; override;

    // methods for codetools
    procedure InitCodeToolBoss;
    procedure UpdateEnglishErrorMsgFilename;
    procedure ActivateCodeToolAbortableMode;
    function BeginCodeTools: boolean; override;
    function BeginCodeTool(var ActiveSrcEdit: TSourceEditor;
                           var ActiveUnitInfo: TUnitInfo;
                           Flags: TCodeToolsFlags): boolean;
    function BeginCodeTool(ADesigner: TDesigner;
                           var ActiveSrcEdit: TSourceEditor;
                           var ActiveUnitInfo: TUnitInfo;
                           Flags: TCodeToolsFlags): boolean;
    function DoJumpToSourcePosition(const Filename: string;
                               NewX, NewY, NewTopLine: integer;
                               AddJumpPoint: boolean): TModalResult; override;
    function DoJumpToCodePos(
                        ActiveSrcEdit: TSourceEditor;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean): TModalResult; override;
    procedure DoJumpToCodeToolBossError; override;
    procedure UpdateSourceNames;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); override;
    procedure ApplyCodeToolChanges;
    procedure DoJumpToProcedureSection;
    procedure DoFindDeclarationAtCursor;
    procedure DoFindDeclarationAtCaret(const LogCaretXY: TPoint);
    function DoFindRenameIdentifier(Rename: boolean): TModalResult;
    function DoInitIdentCompletion(JumpToError: boolean): boolean;
    function DoShowCodeContext(JumpToError: boolean): boolean;
    procedure DoCompleteCodeAtCursor;
    procedure DoExtractProcFromSelection;
    function DoCheckSyntax: TModalResult;
    procedure DoGoToPascalBlockOtherEnd;
    procedure DoGoToPascalBlockStart;
    procedure DoJumpToGuessedUnclosedBlock(FindNext: boolean);
    procedure DoJumpToGuessedMisplacedIFDEF(FindNext: boolean);

    procedure DoGotoIncludeDirective;
    procedure SaveIncludeLinks;

    // tools
    function DoMakeResourceString: TModalResult;
    function DoDiff: TModalResult;
    function DoFindInFiles: TModalResult;

    // message view
    function DoJumpToCompilerMessage(Index:integer;
                                     FocusEditor: boolean): boolean; override;
    procedure DoJumpToNextError(DirectionDown: boolean); override;
    procedure DoShowMessagesView; override;
    procedure DoArrangeSourceEditorAndMessageView(PutOnTop: boolean);

    // methods for debugging, compiling and external tools
    function GetTestBuildDirectory: string; override;
    procedure OnMacroSubstitution(TheMacro: TTransferMacro;
                               const MacroName: string; var s: string;
                               const Data: PtrInt; var Handled, Abort: boolean);
    procedure GetIDEFileState(Sender: TObject; const AFilename: string;
      NeededFlags: TIDEFileStateFlags; var ResultFlags: TIDEFileStateFlags); override;

    // search results
    function DoJumpToSearchResult(FocusEditor: boolean): boolean;
    procedure DoShowSearchResultsView;

    // form editor and designer
    procedure DoBringToFrontFormOrUnit;
    procedure DoBringToFrontFormOrInspector(ForceInspector: boolean);
    procedure DoShowDesignerFormOfCurrentSrc;
    procedure DoShowSourceOfActiveDesignerForm;
    procedure SetDesigning(AComponent: TComponent; Value: Boolean);
    procedure CreateDesignerForComponent(AComponent: TComponent);
    procedure InvalidateAllDesignerForms;
    procedure UpdateIDEComponentPalette;
    procedure ShowDesignerForm(AForm: TCustomForm);
    procedure DoViewAnchorEditor;
    procedure DoToggleViewComponentPalette;
    procedure DoToggleViewIDESpeedButtons;

    // editor and environment options
    procedure SaveEnvironment; override;
    procedure LoadDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
    procedure SaveDesktopSettings(TheEnvironmentOptions: TEnvironmentOptions);
  end;


const
  CodeToolsIncludeLinkFile = 'includelinks.xml';

var
  ShowSplashScreen: boolean = false;

implementation

uses
  Math;

const
  LRSStreamChunkSize = 4096; // allocating mem in 4k chunks helps many mem managers

var
  SkipAutoLoadingLastProject: boolean = false;
  StartedByStartLazarus: boolean = false;
  EnableRemoteControl: boolean = false;

//==============================================================================


{ TMainIDE }

{-------------------------------------------------------------------------------
  procedure TMainIDE.ParseCmdLineOptions;

  Parses the command line for the IDE.
-------------------------------------------------------------------------------}
class procedure TMainIDE.ParseCmdLineOptions;

  function ParamIsOption(ParamIndex: integer;
    const Option: string): boolean;
  begin
    Result:=CompareText(ParamStr(ParamIndex),Option)=0;
  end;

  function ParamIsOptionPlusValue(ParamIndex: integer;
    const Option: string; var AValue: string): boolean;
  var
    p: String;
  begin
    p:=ParamStr(ParamIndex);
    Result:=CompareText(LeftStr(p,length(Option)),Option)=0;
    if Result then
      AValue:=copy(p,length(Option)+1,length(p))
    else
      AValue:='';
  end;

const
  space = '                      ';
var
  i: integer;
  AValue: string;
begin
  StartedByStartLazarus:=false;
  SkipAutoLoadingLastProject:=false;
  EnableRemoteControl:=false;
  if (ParamCount>0)
  and ((CompareText(ParamStr(1),'--help')=0)
    or (CompareText(ParamStr(1),'-help')=0)
    or (CompareText(ParamStr(1),'-?')=0)
    or (CompareText(ParamStr(1),'-h')=0)) then
  begin
    TranslateResourceStrings(ProgramDirectory,'');
    writeln(lislazarusOptionsProjectFilename);
    writeln('');
    writeln(lisIDEOptions);
    writeln('');
    writeln('--help or -?             ', listhisHelpMessage);
    writeln('');
    writeln(PrimaryConfPathOptLong,' <path>');
    writeln('or ',PrimaryConfPathOptShort,' <path>');
    writeln(BreakString(space+lisprimaryConfigDirectoryWhereLazarusStoresItsConfig,
                        75, 22), LazConf.GetPrimaryConfigPath);
    writeln('');
    writeln(SecondaryConfPathOptLong,' <path>');
    writeln('or ',SecondaryConfPathOptShort,' <path>');
    writeln(BreakString(space+lissecondaryConfigDirectoryWhereLazarusSearchesFor,
                        75, 22), LazConf.GetSecondaryConfigPath);
    writeln('');
    writeln(DebugLogOpt,' <file>');
    writeln(BreakString(space+lisFileWhereDebugOutputIsWritten, 75, 22));
    writeln('');
    writeln(NoSplashScreenOptLong);
    writeln('or ',NoSplashScreenOptShort);
    writeln(BreakString(space+lisDoNotShowSplashScreen,75, 22));
    writeln('');
    writeln(SkipLastProjectOpt);
    writeln(BreakString(space+lisSkipLoadingLastProject, 75, 22));
    writeln('');
    writeln(LanguageOpt);
    writeln(BreakString(space+lisOverrideLanguage,75, 22));
    writeln('');
    writeln('');
    writeln('');
    writeln(lisCmdLineLCLInterfaceSpecificOptions);
    writeln('');
    writeln(GetCmdLineParamDescForInterface);
    Application.Terminate;
    exit;
  end;
  for i:=1 to ParamCount do begin
    if ParamIsOptionPlusValue(i,PrimaryConfPathOptLong,AValue) then begin
      SetPrimaryConfigPath(AValue);
    end;
    if ParamIsOptionPlusValue(i,PrimaryConfPathOptShort,AValue) then begin
      SetPrimaryConfigPath(AValue);
    end;
    if ParamIsOptionPlusValue(i,SecondaryConfPathOptLong,AValue) then begin
      SetSecondaryConfigPath(AValue);
    end;
    if ParamIsOptionPlusValue(i,SecondaryConfPathOptShort,AValue) then begin
      SetSecondaryConfigPath(AValue);
    end;
    if ParamIsOption(i,NoSplashScreenOptLong)
    or ParamIsOption(i,NoSplashScreenOptShort) then begin
      ShowSplashScreen:=false;
    end;
    if ParamIsOption(i,SkipLastProjectOpt) then
      SkipAutoLoadingLastProject:=true;
    if ParamIsOption(i,StartedByStartLazarusOpt) then
      StartedByStartLazarus:=true;
    if ParamIsOption(i,EnableRemoteControlOpt) then
      EnableRemoteControl:=true;
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
    if Application.HasOption('language') then begin
      debugln('TMainIDE.LoadGlobalOptions overriding language with command line: ',
        Application.GetOptionValue('language'));
      EnvironmentOptions.LanguageID:=Application.GetOptionValue('language');
    end;
    TranslateResourceStrings(EnvironmentOptions.LazarusDirectory,
                             EnvironmentOptions.LanguageID);

    SetupCompilerFilename(InteractiveSetup);
    SetupFPCSourceDirectory(InteractiveSetup);
    SetupLazarusDirectory(InteractiveSetup);

    ExternalTools.OnNeedsOutputFilter:=@OnExtToolNeedsOutputFilter;
    ExternalTools.OnFreeOutputFilter:=@OnExtToolFreeOutputFilter;
    OnApplyWindowLayout:=@Self.OnApplyWindowLayout;
  end;
  UpdateDefaultPascalFileExtensions;

  EditorOpts:=TEditorOptions.Create;
  SetupIDECommands;
  SetupIDEMsgQuickFixItems;
  EditorOpts.Load;

  EnvironmentOptions.ExternalTools.LoadShortCuts(EditorOpts.KeyMap);

  MiscellaneousOptions:=TMiscellaneousOptions.Create;
  MiscellaneousOptions.Load;

  CodeToolsOpts:=TCodeToolsOptions.Create;
  with CodeToolsOpts do begin
    SetLazarusDefaultFilename;
    Load;
  end;

  MainBuildBoss.SetupInputHistories;

  CreateDir(GetProjectSessionsConfigPath);
end;

constructor TMainIDE.Create(TheOwner: TComponent);
begin
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create START');{$ENDIF}
  inherited Create(TheOwner);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create INHERITED');{$ENDIF}

  SetupDialogs;
  RunExternalTool:=@OnRunExternalTool;
  {$IFDEF UseAsyncProcess}
  TOutputFilterProcess:=TAsyncProcess;
  {$ELSE}
  TOutputFilterProcess:=TProcess;
  {$ENDIF}

  MainBuildBoss:=TBuildManager.Create;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create BUILD MANAGER');{$ENDIF}

  // load options
  CreatePrimaryConfigPath;
  StartProtocol;
  LoadGlobalOptions;

  // set the IDE mode to none (= editing mode)
  ToolStatus:=itNone;

  // setup macros
  SetupTransferMacros;
  SetupCodeMacros;

  // setup the code tools
  InitCodeToolBoss;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create CODETOOLS');{$ENDIF}

  // build and position the MainIDE form
  Application.CreateForm(TMainIDEBar,MainIDEBar);
  MainIDEBar.OnDestroy:=@OnMainBarDestroy;
  {$IFNDEF IDEDocking}
  MainIDEBar.Constraints.MaxHeight:=110;
  {$ENDIF}
  MainIDEBar.Name := NonModalIDEWindowNames[nmiwMainIDEName];
  EnvironmentOptions.IDEWindowLayoutList.Apply(MainIDEBar,MainIDEBar.Name);
  HiddenWindowsOnRun:=TList.Create;

  // menu
  SetupStandardIDEMenuItems;
  SetupMainMenu;
  SetupSpeedButtons;
  SetupComponentNoteBook;
  ConnectMainBarEvents;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create MENU');{$ENDIF}

  // create main IDE register items
  NewIDEItems:=TNewLazIDEItemCategories.Create;
  SetupStandardProjectTypes;

  // initialize the other IDE managers
  DebugBoss:=TDebugManager.Create(nil);
  DebugBoss.ConnectMainBarEvents;
  PkgBoss:=TPkgManager.Create(nil);
  PkgBoss.ConnectMainBarEvents;
  HelpBoss:=THelpManager.Create(nil);
  HelpBoss.ConnectMainBarEvents;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create MANAGERS');{$ENDIF}
  // setup the IDE components
  LoadMenuShortCuts;
  SetupOutputFilter;
  MainBuildBoss.SetupCompilerInterface;
  SetupObjectInspector;
  SetupFormEditor;
  SetupSourceNotebook;
  SetupControlSelection;
  SetupTextConverters;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create IDE COMPONENTS');{$ENDIF}

  // Main IDE bar created and setup completed -> Show it
  MainIDEBar.Show;

  // load installed packages
  PkgBoss.LoadInstalledPackages;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Create INSTALLED COMPONENTS');{$ENDIF}

  // load package configs
  HelpBoss.LoadHelpOptions;

  UpdateWindowsMenu;
end;

procedure TMainIDE.StartIDE;
begin
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.StartIDE START');{$ENDIF}
  // set Application handlers
  Application.AddOnUserInputHandler(@OnApplicationUserInput);
  Application.AddOnIdleHandler(@OnApplicationIdle);
  Application.AddOnActivateHandler(@OnApplicationActivate);
  Application.AddOnKeyDownHandler(@OnApplicationKeyDown);
  Screen.AddHandlerRemoveForm(@OnScreenRemoveForm);
  SetupHints;
  
  // Now load a project
  SetupStartProject;

  // reopen extra windows
  ReOpenIDEWindows;
  DoShowMessagesView;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.StartIDE END');{$ENDIF}
end;

destructor TMainIDE.Destroy;
begin
  ToolStatus:=itExiting;

  DebugLn('[TMainIDE.Destroy] A ');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy A ');{$ENDIF}

  if DebugBoss<>nil then DebugBoss.EndDebugging;

  // free control selection
  if TheControlSelection<>nil then begin
    TheControlSelection.OnChange:=nil;
    TheControlSelection.OnSelectionFormChanged:=nil;
    FreeThenNil(TheControlSelection);
  end;

  FreeAndNil(ComponentListForm);
  FreeThenNil(ProjInspector);
  FreeThenNil(CodeExplorerView);
  FreeThenNil(CodeBrowserView);
  FreeAndNil(LazFindReplaceDialog);
  FreeAndNil(MessagesView);
  FreeThenNil(AnchorDesigner);
  FreeThenNil(ObjectInspector1);
  FreeThenNil(SourceNotebook);

  // disconnect handlers
  Application.RemoveAllHandlersOfObject(Self);
  Screen.RemoveAllHandlersOfObject(Self);
  IDECommands.OnExecuteIDECommand:=nil;

  // free project, if it is still there
  FreeThenNil(Project1);

  // free IDE parts
  FreeFormEditor;
  FreeTextConverters;
  FreeStandardIDEQuickFixItems;
  FreeThenNil(GlobalDesignHook);
  FreeThenNil(PkgBoss);
  FreeThenNil(HelpBoss);
  FreeThenNil(DebugBoss);
  FreeThenNil(TheCompiler);
  FreeThenNil(HiddenWindowsOnRun);
  FreeThenNil(TheOutputFilter);
  FreeThenNil(GlobalMacroList);
  FreeThenNil(IDEMacros);
  FreeThenNil(IDECodeMacros);
  FreeThenNil(LazProjectFileDescriptors);
  FreeThenNil(LazProjectDescriptors);
  FreeThenNil(NewIDEItems);
  FreeThenNil(IDEMenuRoots);
  // IDE options objects
  FreeThenNil(CodeToolsOpts);
  FreeThenNil(MiscellaneousOptions);
  FreeThenNil(EditorOpts);
  FreeThenNil(EnvironmentOptions);
  FreeThenNil(IDECommandScopes);

  DebugLn('[TMainIDE.Destroy] B  -> inherited Destroy... ',ClassName);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy B ');{$ENDIF}
  FreeThenNil(MainBuildBoss);
  inherited Destroy;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.Destroy C ');{$ENDIF}

  FreeThenNil(IDEProtocolOpts);
  DebugLn('[TMainIDE.Destroy] END');
end;

procedure TMainIDE.CreateOftenUsedForms;
begin
  MessagesView:=TMessagesView.Create(nil);
  LazFindReplaceDialog:=TLazFindReplaceDialog.Create(nil);
end;

procedure TMainIDE.CreateSearchResultWindow;
begin
  if SearchResultsView<>nil then exit;
  Application.CreateForm(TSearchResultsView, SearchResultsView);
  with SearchResultsView do begin
    OnSelectionChanged:= @SearchResultsViewSelectionChanged;
  end;
end;

procedure TMainIDE.OIOnSelectPersistents(Sender: TObject);
begin
  TheControlSelection.AssignSelection(ObjectInspector1.Selection);
  GlobalDesignHook.SetSelection(ObjectInspector1.Selection);
end;

procedure TMainIDE.OIOnShowOptions(Sender: TObject);
begin
  DoShowEnvGeneralOptions(eodpObjectInspector);
end;

procedure TMainIDE.OIOnDestroy(Sender: TObject);
begin
  if ObjectInspector1=Sender then
    ObjectInspector1:=nil;
end;

procedure TMainIDE.OIRemainingKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OnExecuteIDEShortCut(Sender,Key,Shift,nil);
end;

procedure TMainIDE.OIOnAddToFavourites(Sender: TObject);
begin
  ShowAddRemoveFavouriteDialog(ObjectInspector1,true);
end;

procedure TMainIDE.OIOnRemoveFromFavourites(Sender: TObject);
begin
  ShowAddRemoveFavouriteDialog(ObjectInspector1,false);
end;

procedure TMainIDE.OIOnFindDeclarationOfProperty(Sender: TObject);
var
  AnInspector: TObjectInspectorDlg;
  Code: TCodeBuffer;
  Caret: TPoint;
  NewTopLine: integer;
begin
  if not BeginCodeTools then exit;
  if Sender=nil then Sender:=ObjectInspector1;
  if Sender is TObjectInspectorDlg then begin
    AnInspector:=TObjectInspectorDlg(Sender);
    if FindDeclarationOfOIProperty(AnInspector,nil,Code,Caret,NewTopLine) then
      DoOpenFileAndJumpToPos(Code.Filename,Caret,NewTopLine,-1,[]);
  end;
end;

function TMainIDE.OnPropHookGetMethodName(const Method: TMethod;
  CheckOwner: TObject): ShortString;
begin
  if Method.Code<>nil then begin
    if Method.Data<>nil then begin
      if (CheckOwner<>nil) and (TObject(Method.Data)<>CheckOwner) then
        Result:=''
      else begin
        Result:=TObject(Method.Data).MethodName(Method.Code);
        if Result='' then
          Result:='<Unpublished>';
      end;
    end else
      Result:='<No LookupRoot>';
  end else if IsJITMethod(Method) then begin
    Result:=TJITMethod(Method.Data).TheMethodName;
  end else
    Result:='';
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
    ActiveUnitInfo.Component.ClassName,TypeData,Proc) then
  begin
    DoJumpToCodeToolBossError;
  end;
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.MainIDEFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveEnvironment;
  CloseIDEWindows;
  SaveIncludeLinks;
  InputHistories.Save;
  PkgBoss.SaveSettings;
  if TheControlSelection<>nil then TheControlSelection.Clear;
  if SourceNoteBook<>nil then SourceNoteBook.ClearUnUsedEditorComponents(true);
  FreeIDEWindows;
end;

procedure TMainIDE.MainIDEFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  MsgResult: integer;
begin
  CanClose:=false;
  FCheckingFilesOnDisk:=true;
  try
    // stop debugging/compiling/...
    if (ToolStatus=itExiting) and (not DoResetToolStatus(true)) then exit;

    // check foreign windows
    if not CloseQueryIDEWindows then exit;

    // check packages
    if (PkgBoss.DoSaveAllPackages([psfAskBeforeSaving])<>mrOk)
    or (PkgBoss.DoCloseAllPackageEditors<>mrOk) then exit;

    // check project
    if SomethingOfProjectIsModified then begin
      MsgResult:=QuestionDlg(lisProjectChanged,
        Format(lisSaveChangesToProject, [Project1.Title]), mtConfirmation,
        [mrYes, lisMenuSave, mrNo, lisDiscardChanges,
         mrAbort, lisDoNotCloseTheIDE],
        0);
      case MsgResult of

      mrYes:
        begin
          CanClose := DoSaveProject([]) <> mrAbort;
          if not CanClose then exit;
        end;

      mrCancel, mrAbort:
        begin
          Exit;
        end;
      end;
    end;

    CanClose:=(DoCloseProject <> mrAbort);
  finally
    FCheckingFilesOnDisk:=false;
    if not CanClose then
      DoCheckFilesOnDisk(false);
  end;
end;

{------------------------------------------------------------------------------}
type
  TMoveFlags = set of (mfTop, mfLeft);

procedure TMainIDE.SetupSpeedButtons;

  function CreateButton(const AName, APixName: String; ANumGlyphs: Integer;
    var ALeft, ATop: Integer; const AMoveFlags: TMoveFlags;
    const AOnClick: TNotifyEvent; const AHint: String): TSpeedButton;
  var
    B: TBitmap;
  begin
    Result := TSpeedButton.Create(OwningComponent);
    with Result do
    begin
      Name := AName;
      Parent := MainIDEBar.pnlSpeedButtons;
      Enabled := True;
      Top := ATop;
      Left := ALeft;
      OnClick := AOnClick;
      B := LoadBitmapFromLazarusResource(APixName);
      Glyph := B;
      B.Free;
      NumGlyphs := ANumGlyphs;
      Flat := True;
      //Transparent:=true;
      if mfTop in AMoveFlags then Inc(ATop, Height);
      if mfLeft in AMoveFlags then Inc(ALeft, Width);
      Hint := AHint;
    end;
  end;

var
  ButtonTop, ButtonLeft, n: Integer;
begin
  MainIDEBar.pnlSpeedButtons := TPanel.Create(OwningComponent);
  with MainIDEBar.pnlSpeedButtons do begin
    Name := 'pnlSpeedButtons';
    Parent:= MainIDEBar;
    Align := alLeft;
    Caption:= '';
    BevelWidth:=1;
    BevelOuter:=bvRaised;
    Visible:=EnvironmentOptions.IDESpeedButtonsVisible;
  end;


  ButtonTop := 2;
  ButtonLeft := 2;
  MainIDEBar.NewUnitSpeedBtn       := CreateButton('NewUnitSpeedBtn'      , 'menu_new_unit'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewUnitClicked, lisMenuNewUnit);

  MainIDEBar.OpenFileSpeedBtn      := CreateButton('OpenFileSpeedBtn'     , 'menu_open'  , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuOpenClicked, lisHintOpen);

  // store left
  n := ButtonLeft;
  MainIDEBar.OpenFileArrowSpeedBtn := CreateButton('OpenFileArrowSpeedBtn', 'btn_downarrow' , 1, ButtonLeft, ButtonTop, [mfLeft], @OpenFileDownArrowClicked, '');
  MainIDEBar.OpenFileArrowSpeedBtn.Width := 12;
  ButtonLeft := n+12+1;

  MainIDEBar.SaveSpeedBtn          := CreateButton('SaveSpeedBtn'         , 'menu_save'   , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveClicked, lisHintSave);
  MainIDEBar.SaveAllSpeedBtn       := CreateButton('SaveAllSpeedBtn'      , 'menu_save_all', 1, ButtonLeft, ButtonTop, [mfLeft], @mnuSaveAllClicked, lisHintSaveAll);
  MainIDEBar.NewFormSpeedBtn       := CreateButton('NewFormSpeedBtn'      , 'menu_new_form'    , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuNewFormClicked, lisMenuNewForm);
  MainIDEBar.ToggleFormSpeedBtn    := CreateButton('ToggleFormSpeedBtn'   , 'menu_view_toggle_form_unit' , 1, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuToggleFormUnitCLicked, lisHintToggleFormUnit);

  // new row
  ButtonLeft := 2;
  MainIDEBar.ViewUnitsSpeedBtn     := CreateButton('ViewUnitsSpeedBtn'    , 'menu_view_units' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewUnitsClicked, lisHintViewUnits);
  MainIDEBar.ViewFormsSpeedBtn     := CreateButton('ViewFormsSpeedBtn'    , 'menu_view_forms' , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuViewFormsClicked, lisHintViewForms);
  inc(ButtonLeft,13);
  MainIDEBar.RunSpeedButton        := CreateButton('RunSpeedButton'       , 'menu_run'       , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuRunProjectClicked, lisHintRun);
  MainIDEBar.PauseSpeedButton      := CreateButton('PauseSpeedButton'     , 'menu_pause'       , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuPauseProjectClicked, lisHintPause);
  MainIDEBar.PauseSpeedButton.Enabled:=false;
  MainIDEBar.StepIntoSpeedButton   := CreateButton('StepIntoSpeedButton'  , 'menu_stepinto'       , 1, ButtonLeft, ButtonTop, [mfLeft], @mnuStepIntoProjectClicked, lisHintStepInto);
  MainIDEBar.StepOverSpeedButton   := CreateButton('StepOverpeedButton'   , 'menu_stepover'       , 1, ButtonLeft, ButtonTop, [mfLeft, mfTop], @mnuStepOverProjectClicked, lisHintStepOver);

  MainIDEBar.pnlSpeedButtons.Width := ButtonLeft+3;

  // create the popupmenu for the OpenFileArrowSpeedBtn
  MainIDEBar.OpenFilePopUpMenu := TPopupMenu.Create(OwningComponent);
  MainIDEBar.OpenFilePopupMenu.Name:='OpenFilePopupMenu';
  MainIDEBar.OpenFilePopupMenu.AutoPopup := False;
end;

procedure TMainIDE.SetupDialogs;
begin
  LazIDESelectDirectory:=@OnSelectDirectory;
  InitIDEFileDialog:=@OnInitIDEFileDialog;
  StoreIDEFileDialog:=@OnStoreIDEFileDialog;
  IDEMessageDialog:=@OnIDEMessageDialog;
  IDEQuestionDialog:=@OnIDEQuestionDialog;
end;

procedure TMainIDE.SetupComponentNoteBook;
begin
  // Component Notebook
  MainIDEBar.ComponentNotebook := TNotebook.Create(OwningComponent);
  with MainIDEBar.ComponentNotebook do begin
    Parent := MainIDEBar;
    Name := 'ComponentNotebook';
    Align := alClient;
    Visible:=EnvironmentOptions.ComponentPaletteVisible;
  end;
end;

procedure TMainIDE.SetupHints;
var
  CurShowHint: boolean;
  AControl: TControl;
  i, j: integer;
begin
  if EnvironmentOptions=nil then exit;
  // update all hints in the component palette
  CurShowHint:=EnvironmentOptions.ShowHintsForComponentPalette;
  for i:=0 to MainIDEBar.ComponentNotebook.PageCount-1 do begin
    for j:=0 to MainIDEBar.ComponentNotebook.Page[i].ControlCount-1 do begin
      AControl:=MainIDEBar.ComponentNotebook.Page[i].Controls[j];
      AControl.ShowHint:=CurShowHint;
    end;
  end;
  // update all hints in main ide toolbars
  CurShowHint:=EnvironmentOptions.ShowHintsForMainSpeedButtons;
  for i:=0 to MainIDEBar.pnlSpeedButtons.ControlCount-1 do begin
    AControl:=MainIDEBar.pnlSpeedButtons.Controls[i];
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
  ObjectInspector1 := TObjectInspectorDlg.Create(OwningComponent);
  ObjectInspector1.BorderStyle:=bsSizeable;
  ObjectInspector1.Favourites:=LoadOIFavouriteProperties;
  ObjectInspector1.FindDeclarationPopupmenuItem.Visible:=true;
  ObjectInspector1.OnAddToFavourites:=@OIOnAddToFavourites;
  ObjectInspector1.OnFindDeclarationOfProperty:=@OIOnFindDeclarationOfProperty;
  ObjectInspector1.OnRemainingKeyDown:=@OIRemainingKeyDown;
  ObjectInspector1.OnRemoveFromFavourites:=@OIOnRemoveFromFavourites;
  ObjectInspector1.OnSelectPersistentsInOI:=@OIOnSelectPersistents;
  ObjectInspector1.OnShowOptions:=@OIOnShowOptions;
  ObjectInspector1.OnDestroy:=@OIOnDestroy;
  ObjectInspector1.ShowFavouritePage:=true;
  IDECmdScopeObjectInspectorOnly.AddWindowClass(TObjectInspectorDlg);

  GlobalDesignHook:=TPropertyEditorHook.Create;
  GlobalDesignHook.GetPrivateDirectory:=AppendPathDelim(GetPrimaryConfigPath);
  GlobalDesignHook.AddHandlerGetMethodName(@OnPropHookGetMethodName);
  GlobalDesignHook.AddHandlerGetMethods(@OnPropHookGetMethods);
  GlobalDesignHook.AddHandlerMethodExists(@OnPropHookMethodExists);
  GlobalDesignHook.AddHandlerCreateMethod(@OnPropHookCreateMethod);
  GlobalDesignHook.AddHandlerShowMethod(@OnPropHookShowMethod);
  GlobalDesignHook.AddHandlerRenameMethod(@OnPropHookRenameMethod);
  GlobalDesignHook.AddHandlerBeforeAddPersistent(@OnPropHookBeforeAddPersistent);
  GlobalDesignHook.AddHandlerComponentRenamed(@OnPropHookComponentRenamed);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPropHookPersistentAdded);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPropHookPersistentDeleting);
  GlobalDesignHook.AddHandlerDeletePersistent(@OnPropHookDeletePersistent);

  ObjectInspector1.PropertyEditorHook:=GlobalDesignHook;
  EnvironmentOptions.IDEWindowLayoutList.Apply(ObjectInspector1,
                                               DefaultObjectInspectorName);
  with EnvironmentOptions do begin
    ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;

  ShowAnchorDesigner:=@mnuViewAnchorEditorClicked;
end;

procedure TMainIDE.SetupFormEditor;
begin
  CreateFormEditor;
  FormEditor1.Obj_Inspector := ObjectInspector1;
end;

procedure TMainIDE.SetupSourceNotebook;
begin
  SourceNotebook := TSourceNotebook.Create(OwningComponent);
  SourceNotebook.OnActivate := @OnSrcNoteBookActivated;
  SourceNotebook.OnAddJumpPoint := @OnSrcNoteBookAddJumpPoint;
  SourceNotebook.OnCloseClicked := @OnSrcNotebookFileClose;
  SourceNotebook.OnCtrlMouseUp := @OnSrcNoteBookCtrlMouseUp;
  SourceNotebook.OnCurrentCodeBufferChanged:=@OnSrcNotebookCurCodeBufferChanged;
  SourceNotebook.OnDeleteLastJumpPoint := @OnSrcNotebookDeleteLastJumPoint;
  SourceNotebook.OnEditorVisibleChanged := @OnSrcNotebookEditorVisibleChanged;
  SourceNotebook.OnEditorChanged := @OnSrcNotebookEditorChanged;
  SourceNotebook.OnEditorPropertiesClicked := @mnuEnvEditorOptionsClicked;
  SourceNotebook.OnFindDeclarationClicked := @OnSrcNotebookFindDeclaration;
  SourceNotebook.OnInitIdentCompletion :=@OnSrcNotebookInitIdentCompletion;
  SourceNotebook.OnInsertTodoClicked := @mnuInsertTodo;
  SourceNotebook.OnShowCodeContext :=@OnSrcNotebookShowCodeContext;
  SourceNotebook.OnJumpToHistoryPoint := @OnSrcNotebookJumpToHistoryPoint;
  SourceNotebook.OnMovingPage := @OnSrcNotebookMovingPage;
  SourceNotebook.OnOpenFileAtCursorClicked := @OnSrcNotebookFileOpenAtCursor;
  SourceNotebook.OnProcessUserCommand := @OnProcessIDECommand;
  SourceNotebook.OnReadOnlyChanged := @OnSrcNotebookReadOnlyChanged;
  SourceNotebook.OnShowHintForSource :=@OnSrcNotebookShowHintForSource;
  SourceNotebook.OnShowUnitInfo := @OnSrcNoteBookShowUnitInfo;
  SourceNotebook.OnToggleFormUnitClicked := @OnSrcNotebookToggleFormUnit;
  SourceNotebook.OnToggleObjectInspClicked:= @OnSrcNotebookToggleObjectInsp;
  SourceNotebook.OnViewJumpHistory := @OnSrcNotebookViewJumpHistory;
  SourceNotebook.OnShowSearchResultsView := @OnSrcNotebookShowSearchResultsView;
  SourceNotebook.OnPopupMenu := @OnSrcNoteBookPopupMenu;
  DebugBoss.ConnectSourceNotebookEvents;

  // connect search menu to sourcenotebook
  MainIDEBar.itmSearchFind.OnClick := @SourceNotebook.FindClicked;
  MainIDEBar.itmSearchFindNext.OnClick := @SourceNotebook.FindNextClicked;
  MainIDEBar.itmSearchFindPrevious.OnClick := @SourceNotebook.FindPreviousClicked;
  MainIDEBar.itmSearchFindInFiles.OnClick := @mnuSearchFindInFiles;
  MainIDEBar.itmSearchReplace.OnClick := @SourceNotebook.ReplaceClicked;
  MainIDEBar.itmIncrementalFind.OnClick := @SourceNotebook.IncrementalFindClicked;
  MainIDEBar.itmGotoLine.OnClick := @SourceNotebook.GotoLineClicked;
  MainIDEBar.itmJumpBack.OnClick := @SourceNotebook.JumpBackClicked;
  MainIDEBar.itmJumpForward.OnClick := @SourceNotebook.JumpForwardClicked;
  MainIDEBar.itmAddJumpPoint.OnClick := @SourceNotebook.AddJumpPointClicked;
  MainIDEBar.itmJumpHistory.OnClick := @SourceNotebook.ViewJumpHistoryClicked;
  MainIDEBar.itmJumpToNextBookmark.OnClick := @SourceNotebook.BookMarkNextClicked;
  MainIDEBar.itmJumpToPrevBookmark.OnClick := @SourceNotebook.BookMarkPrevClicked;
  MainIDEBar.itmFindBlockStart.OnClick:=@mnuSearchFindBlockStart;
  MainIDEBar.itmFindBlockOtherEnd.OnClick:=@mnuSearchFindBlockOtherEnd;
  MainIDEBar.itmFindDeclaration.OnClick:=@mnuSearchFindDeclaration;
  MainIDEBar.itmOpenFileAtCursor.OnClick:=@mnuOpenFileAtCursorClicked;
  
  SourceNotebook.InitMacros(GlobalMacroList);
end;

procedure TMainIDE.SetupTransferMacros;
begin
  MainBuildBoss.SetupTransferMacros;
  GlobalMacroList.OnSubstitution:=@OnMacroSubstitution;

  // source editor
  GlobalMacroList.Add(TTransferMacro.Create('Save','',
                      lisSaveCurrentEditorFile,nil,[tmfInteractive]));
  GlobalMacroList.Add(TTransferMacro.Create('SaveAll','',
                      lisSaveAllModified,nil,[tmfInteractive]));
end;

procedure TMainIDE.SetupCodeMacros;
begin
  CreateStandardCodeMacros;
end;

procedure TMainIDE.SetupControlSelection;
begin
  TheControlSelection:=TControlSelection.Create;
  TheControlSelection.OnChange:=@OnControlSelectionChanged;
  TheControlSelection.OnPropertiesChanged:=@OnControlSelectionPropsChanged;
  TheControlSelection.OnSelectionFormChanged:=@OnControlSelectionFormChanged;
end;

procedure TMainIDE.SetupIDECommands;
begin
  IDECommandList:=EditorOpts.KeyMap;
  IDECommands.OnExecuteIDECommand:=@OnExecuteIDECommand;
  IDECommands.OnExecuteIDEShortCut:=@OnExecuteIDEShortCut;
  CreateStandardIDECommandScopes;
  IDECmdScopeSrcEdit.AddWindowClass(TSourceEditorWindowInterface);
  IDECmdScopeSrcEdit.AddWindowClass(nil);
  IDECmdScopeSrcEditOnly.AddWindowClass(TSourceEditorWindowInterface);

  EditorOpts.KeyMap.CreateDefaultMapping;
end;

procedure TMainIDE.SetupIDEMsgQuickFixItems;
begin
  InitStandardIDEQuickFixItems;
end;

procedure TMainIDE.SetupStartProject;

  function ExtractCmdLineFilenames: TStrings;
  var
    i: LongInt;
    Filename: String;
  begin
    Result:=nil;
    i:=ParamCount;
    while (i>0) do begin
      Filename:=ParamStr(i);
      if (Filename='') or (Filename[1]='-') then break;
      if Result=nil then Result:=TStringList.Create;
      Result.Insert(0,Filename);
      dec(i);
    end;
  end;

  function AskIfLoadLastFailingProject: boolean;
  begin
    Result:=QuestionDlg(lisOpenProject2,
      Format(lisAnErrorOccuredAtLastStartupWhileLoadingLoadThisPro, [
        EnvironmentOptions.LastSavedProjectFile, #13, #13]), mtWarning,
        [mrYes, lisOpenProjectAgain, mrNo, lisStartWithANewProject], 0)=
          mrYes;
  end;

var
  ProjectLoaded: Boolean;
  AProjectFilename: String;
  CmdLineFiles: TStrings;
  i: Integer;
  OpenFlags: TOpenFlags;
  AFilename: String;
begin
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.SetupStartProject A ***********');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject A');{$ENDIF}
  // load command line project or last project or create a new project
  CmdLineFiles:=ExtractCmdLineFilenames;
  try
    ProjectLoaded:=false;

    // try command line project
    if (CmdLineFiles<>nil) and (CmdLineFiles.Count>0) then begin
      AProjectFilename:=CmdLineFiles[0];
      if (CompareFileExt(AProjectFilename,'.lpr',false)=0) then
        AProjectFilename:=ChangeFileExt(AProjectFilename,'.lpi');
      // only try to load .lpi files, other files are loaded later
      if (CompareFileExt(AProjectFilename,'.lpi',false)=0) then begin
        AProjectFilename:=CleanAndExpandFilename(AProjectFilename);
        if FileExists(AProjectFilename) then begin
          CmdLineFiles.Delete(0);
          ProjectLoaded:=(DoOpenProjectFile(AProjectFilename,[])=mrOk);
        end;
      end;
    end;

    // try loading last project if lazarus didn't fail last time
    if (not ProjectLoaded)
    and (not SkipAutoLoadingLastProject)
    and (EnvironmentOptions.OpenLastProjectAtStart)
    and (FileExists(EnvironmentOptions.LastSavedProjectFile)) then begin
      if (not IDEProtocolOpts.LastProjectLoadingCrashed)
      or AskIfLoadLastFailingProject then begin
        // protocol that the IDE is trying to load the last project and did not
        // yet succeed
        IDEProtocolOpts.LastProjectLoadingCrashed := True;
        IDEProtocolOpts.Save;
        // try loading the project
        ProjectLoaded:=
          (DoOpenProjectFile(EnvironmentOptions.LastSavedProjectFile,[])=mrOk);
        // protocol that the IDE was able to open the project without crashing
        IDEProtocolOpts.LastProjectLoadingCrashed := false;
        IDEProtocolOpts.Save;
      end;
    end;
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject B');{$ENDIF}

    if not ProjectLoaded then
      // create new project
      DoNewProject(ProjectDescriptorApplication);

    UpdateWindowsMenu;

    // load the cmd line files
    if CmdLineFiles<>nil then begin
      for i:=0 to CmdLineFiles.Count-1 do
        Begin
          AFilename:=CleanAndExpandFilename(CmdLineFiles.Strings[i]);
          if CompareFileExt(AFilename,'.lpk',false)=0 then begin
            if PkgBoss.DoOpenPackageFile(AFilename,[pofAddToRecent])=mrAbort
            then
              break;
          end else begin
            OpenFlags:=[ofAddToRecent,ofRegularFile];
            if i<CmdLineFiles.Count then
              Include(OpenFlags,ofMultiOpen);
            if DoOpenEditorFile(AFilename,-1,OpenFlags)=mrAbort then begin
              break;
            end;
          end;
        end;
    end;

    {$IFDEF IDE_DEBUG}
    writeln('TMainIDE.Create B');
    {$ENDIF}
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.SetupStartProject C');{$ENDIF}
  finally
    CmdLineFiles.Free;
  end;
end;

procedure TMainIDE.SetupRemoteControl;
var
  Filename: String;
begin
  // delete old remote control file
  Filename:=GetRemoteControlFilename;
  if FileExists(Filename) then
    DeleteFile(Filename);
  // start timer
  FRemoteControlTimer:=TTimer.Create(Self);
  FRemoteControlTimer.Interval:=500;
  FRemoteControlTimer.OnTimer:=@OnRemoteControlTimer;
  FRemoteControlTimer.Enabled:=true;
end;

procedure TMainIDE.ReOpenIDEWindows;
var
  i: Integer;
  ALayout: TIDEWindowLayout;
  FormEnum: TNonModalIDEWindow;
begin
  for i:=0 to EnvironmentOptions.IDEWindowLayoutList.Count-1 do begin
    ALayout:=EnvironmentOptions.IDEWindowLayoutList[i];
    if not ALayout.Visible then continue;
    FormEnum:=NonModalIDEFormIDToEnum(ALayout.FormID);
    if FormEnum in NonModalIDEWindowManualOpen then continue;
    case FormEnum of
    nmiwUnitDependenciesName:
      DoViewUnitDependencies;
    nmiwProjectInspector:
      DoShowProjectInspector;
    nmiwCodeBrowser:
      DoShowCodeBrowser;
    nmiwCodeExplorerName:
      DoShowCodeExplorer;
    nmiwLazDocName:
      DoShowLazDoc;
    nmiwAnchorEditor:
      DoViewAnchorEditor;
    nmiwMessagesViewName:
      DoShowMessagesView;
    nmiwBreakPoints:
      ;//itmViewBreakPoints.OnClick(Self);
    nmiwWatches:
      ;//itmViewWatches.OnClick(Self);
    nmiwLocals:
      ;//itmViewLocals.OnClick(Self);
    nmiwCallStack:
      ;//itmViewCallStack.OnClick(Self);
    end;
  end;
end;

procedure TMainIDE.CloseIDEWindows;
var
  i: Integer;
  AForm: TCustomForm;
begin
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=Screen.CustomForms[i];
    if AForm<>MainIDEBar then
      AForm.Close;
    i:=Math.Min(i,Screen.CustomFormCount)-1;
  end;
end;

procedure TMainIDE.FreeIDEWindows;
var
  i: Integer;
  AForm: TCustomForm;
begin
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=Screen.CustomForms[i];
    if (AForm<>MainIDEBar)
    and ((AForm.Owner=MainIDEBar) or (AForm.Owner=Self)) then begin
      DebugLn(['TMainIDE.FreeIDEWindows ',dbgsName(AForm)]);
      AForm.Free;
    end;
    i:=Math.Min(i,Screen.CustomFormCount)-1;
  end;
end;

function TMainIDE.CloseQueryIDEWindows: boolean;
var
  i: Integer;
  AForm: TCustomForm;
begin
  for i:=0 to Screen.CustomFormCount-1 do begin
    AForm:=Screen.CustomForms[i];
    if AForm<>MainIDEBar then begin
      if not AForm.CloseQuery then exit(false);
    end;
  end;
  Result:=true;
end;

procedure TMainIDE.ReloadMenuShortCuts;
begin
  //LoadMenuShortCuts;
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.SetupMainMenu;
begin
  inherited SetupMainMenu;
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
  mnuMain.MenuItem:=MainIDEBar.mnuMainMenu.Items;
end;

procedure TMainIDE.SetupStandardIDEMenuItems;
begin
  IDEMenuRoots:=TIDEMenuRoots.Create;
  RegisterStandardSourceEditorMenuItems;
  RegisterStandardMessagesViewMenuItems;
  RegisterStandardCodeExplorerMenuItems;
  RegisterStandardCodeTemplatesMenuItems;
  RegisterStandardDesignerMenuItems;
end;

procedure TMainIDE.SetupStandardProjectTypes;
begin
  NewIDEItems.Add(TNewLazIDEItemCategoryFile.Create(FileDescGroupName));
  NewIDEItems.Add(TNewLazIDEItemCategoryProject.Create(ProjDescGroupName));

  // file descriptors
  LazProjectFileDescriptors:=TLazProjectFileDescriptors.Create;
  LazProjectFileDescriptors.DefaultPascalFileExt:=
                        PascalExtension[EnvironmentOptions.PascalFileExtension];
  RegisterProjectFileDescriptor(TFileDescPascalUnit.Create);
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithForm.Create);
  RegisterProjectFileDescriptor(TFileDescPascalUnitWithDataModule.Create);
  RegisterProjectFileDescriptor(TFileDescSimplePascalProgram.Create);
  RegisterProjectFileDescriptor(TFileDescText.Create);

  // project descriptors
  LazProjectDescriptors:=TLazProjectDescriptors.Create;
  RegisterProjectDescriptor(TProjectApplicationDescriptor.Create);
  RegisterProjectDescriptor(TProjectProgramDescriptor.Create);
  RegisterProjectDescriptor(TProjectConsoleApplicationDescriptor.Create);
  RegisterProjectDescriptor(TProjectLibraryDescriptor.Create);
  RegisterProjectDescriptor(TProjectManualProgramDescriptor.Create);
end;

procedure TMainIDE.SetRecentFilesMenu;
begin
  SetRecentSubMenu(itmFileRecentOpen,
                   EnvironmentOptions.RecentOpenFiles,
                   @mnuOpenRecentClicked);
end;

procedure TMainIDE.SetRecentProjectFilesMenu;
begin
  SetRecentSubMenu(itmProjectRecentOpen,
                   EnvironmentOptions.RecentProjectFiles,
                   @mnuOpenProjectClicked);
end;

procedure TMainIDE.SetupFileMenu;
begin
  inherited SetupFileMenu;
  with MainIDEBar do begin
    mnuFile.OnClick:=@mnuFileClicked;
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
    itmFileCleanDirectory.OnClick := @mnuCleanDirectoryClicked;
    itmFileRestart.OnClick := @mnuRestartClicked;
    itmFileQuit.OnClick := @mnuQuitClicked;
  end;
end;

procedure TMainIDE.SetupEditMenu;
begin
  inherited SetupEditMenu;
  with MainIDEBar do begin
    mnuEdit.OnClick:=@mnuEditClicked;
    itmEditUndo.OnClick:=@mnuEditUndoClicked;
    itmEditRedo.OnClick:=@mnuEditRedoClicked;
    itmEditCut.OnClick:=@mnuEditCutClicked;
    itmEditCopy.OnClick:=@mnuEditCopyClicked;
    itmEditPaste.OnClick:=@mnuEditPasteClicked;
    itmEditIndentBlock.OnClick:=@mnuEditIndentBlockClicked;
    itmEditUnindentBlock.OnClick:=@mnuEditUnindentBlockClicked;
    itmEditEncloseBlock.OnClick:=@mnuEditEncloseBlockClicked;
    itmEditUpperCaseBlock.OnClick:=@mnuEditUpperCaseBlockClicked;
    itmEditLowerCaseBlock.OnClick:=@mnuEditLowerCaseBlockClicked;
    itmEditTabsToSpacesBlock.OnClick:=@mnuEditTabsToSpacesBlockClicked;
    itmEditCommentBlock.OnClick:=@mnuEditCommentBlockClicked;
    itmEditUncommentBlock.OnClick:=@mnuEditUncommentBlockClicked;
    itmEditConditionalBlock.OnClick:=@mnuEditConditionalBlockClicked;
    itmEditSortBlock.OnClick:=@mnuEditSortBlockClicked;
    itmEditSelectionBreakLines.OnClick:=@mnuEditSelectionBreakLinesClicked;
    itmEditSelectAll.OnClick:=@mnuEditSelectAllClick;
    itmEditSelectToBrace.OnClick:=@mnuEditSelectToBraceClick;
    itmEditSelectCodeBlock.OnClick:=@mnuEditSelectCodeBlockClick;
    itmEditSelectLine.OnClick:=@mnuEditSelectLineClick;
    itmEditSelectParagraph.OnClick:=@mnuEditSelectParagraphClick;
    itmEditCompleteCode.OnClick:=@mnuEditCompleteCodeClicked;
    itmEditExtractProc.OnClick:=@mnuEditExtractProcClicked;
    itmEditInsertCharacter.OnClick:=@mnuEditInsertCharacterClicked;

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
    itmEditInsertModifiedLGPLNotice.OnClick:=@mnuEditInsertModifiedLGPLNoticeClick;
    itmEditInsertUsername.OnClick:=@mnuEditInsertUsernameClick;
    itmEditInsertDateTime.OnClick:=@mnuEditInsertDateTimeClick;
    itmEditInsertChangeLogEntry.OnClick:=@mnuEditInsertChangeLogEntryClick;
  end;
end;

procedure TMainIDE.SetupSearchMenu;
begin
  inherited SetupSearchMenu;
  with MainIDEBar do begin
    itmSearchFindIdentifierRefs.OnClick:=@mnuSearchFindIdentifierRefsClicked;
    itmSearchRenameIdentifier.OnClick:=@mnuSearchRenameIdentifierClicked;
    itmGotoIncludeDirective.OnClick:=@mnuGotoIncludeDirectiveClicked;
    itmSearchProcedureList.OnClick := @mnuSearchProcedureList;
  end;
end;

procedure TMainIDE.SetupViewMenu;
begin
  inherited SetupViewMenu;
  with MainIDEBar do begin
    itmViewInspector.OnClick := @mnuViewInspectorClicked;
    itmViewSourceEditor.OnClick := @mnuViewSourceEditorClicked;
    itmViewCodeExplorer.OnClick := @mnuViewCodeExplorerClick;
    itmViewCodeBrowser.OnClick := @mnuViewCodeBrowserClick;
    itmViewComponents.OnClick := @mnuViewComponentsClick;
    itmViewLazDoc.OnClick := @mnuViewLazDocClicked;  //DBlaszijk 5-sep-05
    itmViewUnits.OnClick := @mnuViewUnitsClicked;
    itmViewForms.OnClick := @mnuViewFormsClicked;
    itmViewUnitDependencies.OnClick := @mnuViewUnitDependenciesClicked;
    itmViewUnitInfo.OnClick := @mnuViewUnitInfoClicked;
    itmViewToggleFormUnit.OnClick := @mnuToggleFormUnitClicked;
    itmViewMessage.OnClick := @mnuViewMessagesClick;
    itmViewSearchResults.OnClick := @mnuViewSearchResultsClick;
    itmViewAnchorEditor.OnClick := @mnuViewAnchorEditorClicked;
    itmViewComponentPalette.OnClick := @mnuViewComponentPaletteClicked;
    itmViewIDESpeedButtons.OnClick := @mnuViewIDESpeedButtonsClicked;
  end;
end;

procedure TMainIDE.SetupProjectMenu;
begin
  inherited SetupProjectMenu;
  with MainIDEBar do begin
    itmProjectNew.OnClick := @mnuNewProjectClicked;
    itmProjectNewFromFile.OnClick := @mnuNewProjectFromFileClicked;
    itmProjectOpen.OnClick := @mnuOpenProjectClicked;
    SetRecentProjectFilesMenu;
    itmProjectClose.OnClick := @mnuCloseProjectClicked;
    itmProjectSave.OnClick := @mnuSaveProjectClicked;
    itmProjectSaveAs.OnClick := @mnuSaveProjectAsClicked;
    itmProjectPublish.OnClick := @mnuPublishProjectClicked;
    itmProjectInspector.OnClick := @mnuProjectInspectorClicked;
    itmProjectOptions.OnClick := @mnuProjectOptionsClicked;
    {$ifdef EnableNewDialogs}
    itmProjectOptions_new.OnClick := @mnuProjectOptionsClicked_new;
    {$endif}
    itmProjectCompilerOptions.OnClick := @mnuProjectCompilerSettingsClicked;
    {$ifdef EnableNewDialogs}
    itmProjectCompilerOptions_new.OnClick := @mnuEnvGeneralOptionsClicked_new;
    {$endif}
    itmProjectAddTo.OnClick := @mnuAddToProjectClicked;
    itmProjectRemoveFrom.OnClick := @mnuRemoveFromProjectClicked;
    itmProjectViewSource.OnClick := @mnuViewProjectSourceClicked;
    itmProjectViewToDos.OnClick := @mnuViewProjectTodosClicked;
  end;
end;

procedure TMainIDE.SetupRunMenu;
begin
  inherited SetupRunMenu;
  with MainIDEBar do begin
    itmRunMenuBuild.OnClick := @mnuBuildProjectClicked;
    itmRunMenuBuildAll.OnClick := @mnuBuildAllProjectClicked;
    itmRunMenuQuickCompile.OnClick := @mnuQuickCompileProjectClicked;
    itmRunMenuAbortBuild.OnClick := @mnuAbortBuildProjectClicked;
    itmRunMenuRun.OnClick := @mnuRunProjectClicked;
    itmRunMenuPause.Enabled := false;
    itmRunMenuPause.OnClick := @mnuPauseProjectClicked;
    itmRunMenuStepInto.OnClick := @mnuStepIntoProjectClicked;
    itmRunMenuStepOver.OnClick := @mnuStepOverProjectClicked;
    itmRunMenuRunToCursor.OnClick := @mnuRunToCursorProjectClicked;
    itmRunMenuStop.OnClick := @mnuStopProjectClicked;
    itmRunMenuRunParameters.OnClick := @mnuRunParametersClicked;
    itmRunMenuBuildFile.OnClick := @mnuBuildFileClicked;
    itmRunMenuRunFile.OnClick := @mnuRunFileClicked;
    itmRunMenuConfigBuildFile.OnClick := @mnuConfigBuildFileClicked;
  end;
end;

procedure TMainIDE.SetupComponentsMenu;
begin
  inherited SetupComponentsMenu;
end;

procedure TMainIDE.SetupToolsMenu;
begin
  inherited SetupToolsMenu;
  with MainIDEBar do begin
    itmToolConfigure.OnClick := @mnuToolConfigureClicked;
    itmToolSyntaxCheck.OnClick := @mnuToolSyntaxCheckClicked;
    itmToolGuessUnclosedBlock.OnClick := @mnuToolGuessUnclosedBlockClicked;
    itmToolGuessMisplacedIFDEF.OnClick := @mnuToolGuessMisplacedIFDEFClicked;
    itmToolMakeResourceString.OnClick := @mnuToolMakeResourceStringClicked;
    itmToolDiff.OnClick := @mnuToolDiffClicked;
    itmToolConvertDFMtoLFM.OnClick := @mnuToolConvertDFMtoLFMClicked;
    itmToolConvertDelphiUnit.OnClick := @mnuToolConvertDelphiUnitClicked;
    itmToolConvertDelphiProject.OnClick := @mnuToolConvertDelphiProjectClicked;
    itmToolConvertDelphiPackage.OnClick := @mnuToolConvertDelphiPackageClicked;
    itmToolBuildLazarus.OnClick := @mnuToolBuildLazarusClicked;
    itmToolConfigureBuildLazarus.OnClick := @mnuToolConfigBuildLazClicked;
  end;
  UpdateCustomToolsInMenu;
end;

procedure TMainIDE.SetupEnvironmentMenu;
begin
  inherited SetupEnvironmentMenu;
  with MainIDEBar do begin
    itmEnvGeneralOptions.OnClick := @mnuEnvGeneralOptionsClicked;
    {$ifdef EnableNewDialogs}
    itmEnvGeneralOptions_new.OnClick := @mnuEnvGeneralOptionsClicked_new;
    {$endif}
    itmEnvEditorOptions.OnClick := @mnuEnvEditorOptionsClicked;
    {$ifdef EnableNewDialogs}
    itmEnvEditorOptions_new.OnClick := @mnuEnvEditorOptionsClicked_new;
    {$endif}
    itmEnvCodeTemplates.OnClick := @mnuEnvCodeTemplatesClicked;
    itmEnvCodeToolsOptions.OnClick := @mnuEnvCodeToolsOptionsClicked;
    itmEnvCodeToolsDefinesEditor.OnClick := @mnuEnvCodeToolsDefinesEditorClicked;
    itmEnvRescanFPCSrcDir.OnClick := @mnuEnvRescanFPCSrcDirClicked;
  end;
end;

procedure TMainIDE.SetupWindowsMenu;
begin
  inherited SetupWindowsMenu;
end;

procedure TMainIDE.SetupHelpMenu;
begin
  inherited SetupHelpMenu;
end;

procedure TMainIDE.LoadMenuShortCuts;
begin
  inherited LoadMenuShortCuts;
  DebugBoss.SetupMainBarShortCuts;
end;

procedure TMainIDE.ConnectMainBarEvents;
begin
  MainIDEBar.OnClose := @MainIDEFormClose;
  MainIDEBar.OnCloseQuery := @MainIDEFormCloseQuery;
end;

{------------------------------------------------------------------------------}

procedure TMainIDE.mnuToggleFormUnitClicked(Sender: TObject);
begin
  DoBringToFrontFormOrUnit;
end;

procedure TMainIDE.mnuViewAnchorEditorClicked(Sender: TObject);
begin
  DoViewAnchorEditor;
end;

procedure TMainIDE.mnuViewComponentPaletteClicked(Sender: TObject);
begin
  DoToggleViewComponentPalette;
end;

procedure TMainIDE.mnuViewIDESpeedButtonsClicked(Sender: TObject);
begin
  DoToggleViewIDESpeedButtons;
end;

Procedure TMainIDE.SetDesigning(AComponent: TComponent; Value: Boolean);
Begin
  SetComponentDesignMode(AComponent,Value);
  if Value then WidgetSet.SetDesigning(AComponent);
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.mnuFindDeclarationClicked(Sender: TObject);
begin
  if SourceNoteBook.Notebook=nil then exit;
  DoFindDeclarationAtCursor;
end;

procedure TMainIDE.mnuNewUnitClicked(Sender: TObject);
begin
  DoNewEditorFile(FileDescriptorUnit,'','',[nfOpenInEditor,nfCreateDefaultSrc]);
end;

procedure TMainIDE.mnuNewFormClicked(Sender: TObject);
begin
  DoNewEditorFile(FileDescriptorForm,'','',[nfOpenInEditor,nfCreateDefaultSrc]);
end;

procedure TMainIDE.mnuNewOtherClicked(Sender: TObject);
begin
  DoNewOther;
end;

procedure TMainIDE.mnuOpenClicked(Sender: TObject);

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
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    OpenDialog.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+lisLazarusForm+' (*.lfm)|*.lfm'
                 +'|'+lisLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+lisLazarusProjectSource+' (*.lpr)|*.lpr';
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      OpenFlags:=[ofAddToRecent];
      //debugln('TMainIDE.mnuOpenClicked OpenDialog.Files.Count=',dbgs(OpenDialog.Files.Count));
      if OpenDialog.Files.Count>1 then
        Include(OpenFlags,ofRegularFile);
      For I := 0 to OpenDialog.Files.Count-1 do
        Begin
          AFilename:=CleanAndExpandFilename(OpenDialog.Files.Strings[i]);
          if i<OpenDialog.Files.Count-1 then
            Include(OpenFlags,ofMultiOpen)
          else
            Exclude(OpenFlags,ofMultiOpen);
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
  AFileName:=ExpandFilename((Sender as TIDEMenuItem).Caption);
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

procedure TMainIDE.mnuRevertClicked(Sender: TObject);
begin
  if (SourceNoteBook.Notebook=nil)
  or (SourceNoteBook.Notebook.PageIndex<0) then exit;
  DoOpenEditorFile('',SourceNoteBook.Notebook.PageIndex,[ofRevert]);
end;

procedure TMainIDE.mnuOpenFileAtCursorClicked(Sender: TObject);
begin
  if SourceNoteBook.Notebook=nil then exit;
  DoOpenFileAtCursor(Sender);
end;

procedure TMainIDE.mnuGotoIncludeDirectiveClicked(Sender: TObject);
begin
  DoGotoIncludeDirective;
end;

procedure TMainIDE.mnuSearchProcedureList(Sender: TObject);
begin
  ProcedureList.ExecuteProcedureList(Sender);
end;

procedure TMainIDE.mnuSaveClicked(Sender: TObject);
begin
  if SourceNoteBook.Notebook=nil then exit;
  DoSaveEditorFile(SourceNoteBook.Notebook.PageIndex,[sfCheckAmbiguousFiles]);
end;

procedure TMainIDE.mnuSaveAsClicked(Sender: TObject);
begin
  if SourceNoteBook.Notebook=nil then exit;
  DoSaveEditorFile(SourceNoteBook.Notebook.PageIndex,
                   [sfSaveAs,sfCheckAmbiguousFiles]);
end;

procedure TMainIDE.mnuSaveAllClicked(Sender: TObject);
begin
  DoSaveAll([sfCheckAmbiguousFiles]);
end;

procedure TMainIDE.mnuCloseClicked(Sender: TObject);
var PageIndex: integer;
begin
  if SourceNoteBook.Notebook=nil then exit;
  if Sender is TPage then begin
    PageIndex:=SourceNoteBook.Notebook.Pages.IndexOfObject(Sender);
    if PageIndex<0 then
      PageIndex:=SourceNoteBook.Notebook.PageIndex;
  end else begin
    PageIndex:=SourceNoteBook.Notebook.PageIndex;
  end;
  DoCloseEditorFile(PageIndex,[cfSaveFirst]);
end;

procedure TMainIDE.mnuCloseAllClicked(Sender: TObject);
begin
  DoSaveAll([]);
  while (SourceNoteBook.Notebook<>nil)
  and (DoCloseEditorFile(SourceNoteBook.Notebook.PageIndex,
       [cfSaveFirst])=mrOk) do ;
end;

procedure TMainIDE.mnuCleanDirectoryClicked(Sender: TObject);
begin
  ShowCleanDirectoryDialog(Project1.ProjectDirectory,GlobalMacroList);
end;

Procedure TMainIDE.OnSrcNotebookFileNew(Sender: TObject);
begin
  mnuNewFormClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileClose(Sender: TObject;
  InvertedClose: boolean);
var
  PageIndex: LongInt;
  i: Integer;
begin
  if InvertedClose then begin
    // close all source editors except the clicked
    if SourceNoteBook.Notebook=nil then exit;
    if Sender is TPage then begin
      PageIndex:=SourceNoteBook.Notebook.Pages.IndexOfObject(Sender);
      if PageIndex<0 then
        PageIndex:=SourceNoteBook.Notebook.PageIndex;
    end else begin
      PageIndex:=SourceNoteBook.Notebook.PageIndex;
    end;
    repeat
      i:=SourceNoteBook.Notebook.PageCount-1;
      if i=PageIndex then dec(i);
      if i<0 then break;
      if DoCloseEditorFile(i,[cfSaveFirst])<>mrOk then exit;
      if i<PageIndex then PageIndex:=i;
    until false;
  end else
    // close only the clicked source editor
    mnuCloseClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileOpen(Sender: TObject);
begin
  mnuOpenClicked(Sender);
end;

Procedure TMainIDE.OnSrcNoteBookFileOpenAtCursor(Sender: TObject);
begin
  mnuOpenFileAtCursorClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileSave(Sender: TObject);
begin
  mnuSaveClicked(Sender);
end;

Procedure TMainIDE.OnSrcNotebookFileSaveAs(Sender: TObject);
begin
  mnuSaveAsClicked(Sender);
end;

Procedure TMainIDE.OnSrcNoteBookFindDeclaration(Sender: TObject);
begin
  mnuFindDeclarationClicked(Sender);
end;

procedure TMainIDE.OnSrcNotebookInitIdentCompletion(Sender: TObject;
  JumpToError: boolean; out Handled, Abort: boolean);
begin
  Handled:=true;
  Abort:=not DoInitIdentCompletion(JumpToError);
end;

procedure TMainIDE.OnSrcNotebookShowCodeContext(
  JumpToError: boolean; out Abort: boolean);
begin
  Abort:=not DoShowCodeContext(JumpToError);
end;

Procedure TMainIDE.OnSrcNotebookSaveAll(Sender: TObject);
begin
  mnuSaveAllClicked(Sender);
end;

procedure TMainIDE.OnSrcNotebookToggleFormUnit(Sender: TObject);
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
  IDECmd: TIDECommand;
begin
  //debugln('TMainIDE.OnProcessIDECommand ',dbgs(Command));

  Handled:=true;

  case Command of

  ecContextHelp:
    if Sender=MessagesView then
      HelpBoss.ShowHelpForMessage(-1)
    else if Sender is TObjectInspectorDlg then
      HelpBoss.ShowHelpForObjectInspector(Sender);

  ecSave:
    if (Sender is TDesigner) then begin
      GetDesignerUnit(TDesigner(Sender),ASrcEdit,AnUnitInfo);
      if (AnUnitInfo<>nil) and (AnUnitInfo.EditorIndex>=0) then
        DoSaveEditorFile(AnUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
    end else if (Sender is TObjectInspectorDlg) then begin
      GetObjectInspectorUnit(ASrcEdit,AnUnitInfo);
      if (AnUnitInfo<>nil) and (AnUnitInfo.EditorIndex>=0) then
        DoSaveEditorFile(AnUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
    end else if Sender is TSourceNotebook then
      mnuSaveClicked(Self);

  ecOpen:
    mnuOpenClicked(Self);

  ecSaveAll:
    DoSaveAll([sfCheckAmbiguousFiles]);

  ecQuit:
    mnuQuitClicked(Self);

  ecBuild:
    begin
      GetCurrentUnit(ASrcEdit,AnUnitInfo);
      if (AnUnitInfo<>nil)
      and AnUnitInfo.BuildFileIfActive then
        DoBuildFile
      else
        DoBuildProject(crCompile,[]);
    end;

  ecBuildAll:    DoBuildProject(crBuild,[pbfCleanCompile,
                                         pbfCompileDependenciesClean]);
  ecQuickCompile:DoQuickCompile;
  ecAbortBuild:  DoAbortBuild;

  ecRun:
    begin
      GetCurrentUnit(ASrcEdit,AnUnitInfo);
      if (AnUnitInfo<>nil)
      and AnUnitInfo.RunFileIfActive then
        DoRunFile
      else
        DoRunProject;
    end;

  ecBuildFile:
    DoBuildFile;

  ecRunFile:
    DoRunFile;

  ecJumpToPrevError:
    DoJumpToNextError(true);

  ecJumpToNextError:
    DoJumpToNextError(false);

  ecFindInFiles:
    DoFindInFiles;

  ecFindProcedureDefinition,
  ecFindProcedureMethod:
    DoJumpToProcedureSection;

  ecFindDeclaration:
    DoFindDeclarationAtCursor;

  ecFindIdentifierRefs:
    DoFindRenameIdentifier(false);

  ecRenameIdentifier:
    DoFindRenameIdentifier(true);

  ecFindBlockOtherEnd:
    DoGoToPascalBlockOtherEnd;

  ecFindBlockStart:
    DoGoToPascalBlockStart;

  ecGotoIncludeDirective:
    DoGotoIncludeDirective;

  ecCompleteCode:
    DoCompleteCodeAtCursor;

  ecExtractProc:
    DoExtractProcFromSelection;

  ecToggleMessages:
    // user used shortcut/menu item to show the window, so focusing is ok.
    MessagesView.EnsureVisible;

  ecToggleCodeExpl:
    DoShowCodeExplorer;

  ecToggleCodeBrowser:
    DoShowCodeBrowser;

  ecViewComponents:
    DoShowComponentList;
    
  ecToggleLazDoc:
    DoShowLazDoc;

  ecViewUnits:
    DoViewUnitsAndForms(false);

  ecViewForms:
    DoViewUnitsAndForms(true);

  ecProjectInspector:
    DoShowProjectInspector;

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

  ecToggleSearchResults:
    mnuViewSearchResultsClick(Self);

  ecAboutLazarus:
    MainIDEBar.itmHelpAboutLazarus.OnClick(Self);

  ecAddBreakPoint:
    SourceNotebook.ToggleBreakpointClicked(Self);

  ecRemoveBreakPoint:
    SourceNotebook.DeleteBreakpointClicked(Self);
    
  ecProcedureList:
    mnuSearchProcedureList(self);

  else
    Handled:=false;
    // let the bosses handle it
    DebugBoss.ProcessCommand(Command,Handled);
    if Handled then exit;
    PkgBoss.ProcessCommand(Command,Handled);
    // custom commands
    IDECmd:=IDECommandList.FindIDECommand(Command);
    //DebugLn('TMainIDE.OnProcessIDECommand Command=',dbgs(Command),' ',dbgs(IDECmd));
    if (IDECmd<>nil) then begin
      Handled:=IDECmd.Execute(Self);
    end;
  end;

  //DebugLn('TMainIDE.OnProcessIDECommand Handled=',dbgs(Handled),' Command=',dbgs(Command));
end;

function TMainIDE.OnExecuteIDECommand(Sender: TObject; Command: word): boolean;
begin
  Result:=false;
  OnProcessIDECommand(Sender,Command,Result);
end;

function TMainIDE.OnSelectDirectory(const Title, InitialDir: string
  ): string;
var
  Dialog: TSelectDirectoryDialog;
  DummyResult: Boolean;
begin
  Result:='';
  Dialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(Dialog);
    Dialog.Title:=Title;
    Dialog.Options:=Dialog.Options+[ofFileMustExist];
    if InitialDir<>'' then
      Dialog.InitialDir:=InitialDir;
    DummyResult:=Dialog.Execute;
    InputHistories.StoreFileDialogSettings(Dialog);
    if DummyResult and DirPathExists(Dialog.Filename) then begin
      Result:=Dialog.Filename;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TMainIDE.OnInitIDEFileDialog(AFileDialog: TFileDialog);
begin
  InputHistories.ApplyFileDialogSettings(AFileDialog);
end;

procedure TMainIDE.OnStoreIDEFileDialog(AFileDialog: TFileDialog);
begin
  InputHistories.StoreFileDialogSettings(AFileDialog);
end;

function TMainIDE.OnIDEMessageDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string
  ): Integer;
begin
  Result:=MessageDlg(aCaption,aMsg,DlgType,Buttons,HelpKeyword);
end;

function TMainIDE.OnIDEQuestionDialog(const aCaption, aMsg: string;
  DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string
  ): Integer;
begin
  Result:=QuestionDlg(aCaption,aMsg,DlgType,Buttons,HelpKeyword);
end;

procedure TMainIDE.OnExecuteIDEShortCut(Sender: TObject; var Key: word;
  Shift: TShiftState;
  IDEWindowClass: TCustomFormClass);
var
  Command: Word;
  Handled: Boolean;
begin
  if Key=VK_UNKNOWN then exit;
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,IDEWindowClass);
  if (Command = ecNone) then exit;
  Handled := false;
  OnProcessIDECommand(Sender, Command, Handled);
  if Handled then
    Key := VK_UNKNOWN;
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
    ActiveSrcEdit.EditorComponent.PixelsToLogicalPos(Point(X,Y)));
end;

procedure TMainIDE.OnSrcNoteBookShowUnitInfo(Sender: TObject);
begin
  DoViewUnitInfo;
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.OpenFileDownArrowClicked(Sender: TObject);
var
  CurIndex: integer;
  PopupPos: TPoint;
  OpenMenuItem: TPopupMenu;

  procedure AddFile(const Filename: string);
  var
    AMenuItem: TMenuItem;
  begin
    if MainIDEBar.OpenFilePopupMenu.Items.Count>CurIndex then
      AMenuItem:=MainIDEBar.OpenFilePopupMenu.Items[CurIndex]
    else begin
      AMenuItem:=TMenuItem.Create(OwningComponent);
      AMenuItem.Name:=MainIDEBar.OpenFilePopupMenu.Name+'Recent'+IntToStr(CurIndex);
      AMenuItem.OnClick:=@mnuOpenFilePopupClick;
      MainIDEBar.OpenFilePopupMenu.Items.Add(AMenuItem);
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
  OpenMenuItem:=MainIDEBar.OpenFilePopupMenu;
  // remove unused menuitems
  while OpenMenuItem.Items.Count>CurIndex do
    OpenMenuItem.Items[OpenMenuItem.Items.Count-1].Free;
  // calculate screen position to show menu
  PopupPos := MainIDEBar.OpenFileSpeedBtn.ClientToScreen(
                                  Point(0, MainIDEBar.OpenFileSpeedBtn.Height));
  // display the PopupMenu
  if OpenMenuItem.Items.Count > 0 then
    OpenMenuItem.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TMainIDE.mnuOpenFilePopupClick(Sender: TObject);
var
  TheMenuItem: TMenuItem;
  Index, SeparatorIndex: integer;
  AFilename: string;
begin
  TheMenuItem:=(Sender as TMenuItem);
  if TheMenuItem.Caption='-' then exit;
  Index:=TheMenuItem.MenuIndex;
  SeparatorIndex:=0;
  while SeparatorIndex<MainIDEBar.OpenFilePopupMenu.Items.Count do begin
    if MainIDEBar.OpenFilePopupMenu.Items[SeparatorIndex].Caption='-' then
      break;
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

Procedure TMainIDE.CreateDesignerForComponent(AComponent: TComponent);
var
  DesignerForm: TCustomForm;
Begin
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.CreateDesignerForComponent] A ',AComponent.Name,':',AComponent.ClassName);
  {$ENDIF}
  // create designer form
  if (AComponent is TCustomForm) then
    DesignerForm:=TCustomForm(AComponent)
  else
    DesignerForm:=FormEditor1.CreateNonControlForm(AComponent);
  // create designer
  DesignerForm.Designer := TDesigner.Create(DesignerForm, TheControlSelection);
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.CreateDesignerForComponent] B');
  {$ENDIF}
  with TDesigner(DesignerForm.Designer) do begin
    TheFormEditor := FormEditor1;
    OnActivated:=@OnDesignerActivated;
    OnCloseQuery:=@OnDesignerCloseQuery;
    OnPersistentDeleted:=@OnDesignerPersistentDeleted;
    OnGetNonVisualCompIcon:=
                 @TComponentPalette(IDEComponentPalette).OnGetNonVisualCompIcon;
    OnGetSelectedComponentClass:=@OnDesignerGetSelectedComponentClass;
    OnModified:=@OnDesignerModified;
    OnPasteComponent:=@OnDesignerPasteComponent;
    OnProcessCommand:=@OnProcessIDECommand;
    OnPropertiesChanged:=@OnDesignerPropertiesChanged;
    OnRenameComponent:=@OnDesignerRenameComponent;
    OnSetDesigning:=@OnDesignerSetDesigning;
    OnShowOptions:=@OnDesignerShowOptions;
    OnUnselectComponentClass:=@OnDesignerUnselectComponentClass;
    OnViewLFM:=@OnDesignerViewLFM;
    OnSaveAsXML:=@OnDesignerSaveAsXML;
    ShowEditorHints:=EnvironmentOptions.ShowEditorHints;
    ShowComponentCaptionHints:=EnvironmentOptions.ShowComponentCaptions;
  end;
  // set component and designer form into design mode (csDesigning)
  SetDesigning(AComponent,True);
  if AComponent<>DesignerForm then
    SetDesigning(DesignerForm,True);
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
  CurDesignerForm: TCustomForm;
begin
  AnUnitInfo:=Project1.FirstUnitWithComponent;
  while AnUnitInfo<>nil do begin
    if AnUnitInfo.Component<>nil then begin
      CurDesignerForm:=FormEditor1.GetDesignerForm(AnUnitInfo.Component);
      if CurDesignerForm<>nil then
        CurDesignerForm.Invalidate;
    end;
    AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
  end;
end;

procedure TMainIDE.UpdateIDEComponentPalette;
var
  ShowControlsInComponentalette: Boolean;
begin
  ShowControlsInComponentalette:=(FLastFormActivated=nil)
    or (TDesigner(FLastFormActivated.Designer).LookupRoot is TControl);
  IDEComponentPalette.ShowHideControls(ShowControlsInComponentalette);
end;

procedure TMainIDE.ShowDesignerForm(AForm: TCustomForm);
begin
  // do not call 'AForm.Show', because it will set Visible to true
  AForm.BringToFront;
  LCLIntf.ShowWindow(AForm.Handle,SW_SHOWNORMAL);
end;

procedure TMainIDE.DoViewAnchorEditor;
begin
  if AnchorDesigner=nil then
    AnchorDesigner:=TAnchorDesigner.Create(OwningComponent);
  AnchorDesigner.EnsureVisible(true);
end;

procedure TMainIDE.DoToggleViewComponentPalette;
var
  ComponentPalleteVisible: boolean;
begin
  ComponentPalleteVisible:=not MainIDEBar.ComponentNotebook.Visible;
  MainIDEBar.itmViewComponentPalette.Checked:=ComponentPalleteVisible;
  MainIDEBar.ComponentNotebook.Visible:=ComponentPalleteVisible;
  EnvironmentOptions.ComponentPaletteVisible:=ComponentPalleteVisible;
end;

procedure TMainIDE.DoToggleViewIDESpeedButtons;
var
  SpeedButtonsVisible: boolean;
begin
  SpeedButtonsVisible:=not MainIDEBar.pnlSpeedButtons.Visible;
  MainIDEBar.itmViewIDESpeedButtons.Checked:=SpeedButtonsVisible;
  MainIDEBar.pnlSpeedButtons.Visible:=SpeedButtonsVisible;
  EnvironmentOptions.IDESpeedButtonsVisible:=MainIDEBar.pnlSpeedButtons.Visible;
end;

procedure TMainIDE.SetToolStatus(const AValue: TIDEToolStatus);
begin
  inherited SetToolStatus(AValue);
  if DebugBoss<>nil then
    DebugBoss.UpdateButtonsAndMenuItems;
end;

function TMainIDE.DoResetToolStatus(Interactive: boolean): boolean;
begin
  Result:=false;
  case ToolStatus of

  itDebugger:
    begin
      if Interactive
      and (QuestionDlg(lisStopDebugging,
          lisStopTheDebugging, mtConfirmation,
          [mrYes, lisMenuStop, mrCancel, lisContinue], 0)<>mrYes)
      then exit;
      DebugBoss.DoStopProject;
    end;

  end;
  Result:=ToolStatus=itNone;
end;

procedure TMainIDE.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;


{------------------------------------------------------------------------------}

procedure TMainIDE.mnuRestartClicked(Sender: TObject);
begin
  DoRestart;
end;

procedure TMainIDE.mnuQuitClicked(Sender: TObject);
begin
  QuitIDE;
end;

procedure TMainIDE.mnuEditClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
  Editable: Boolean;
  SelAvail: Boolean;
  SelEditable: Boolean;
begin
  GetCurrentUnit(ASrcEdit,AnUnitInfo);
  Editable:=(ASrcEdit<>nil) and (not ASrcEdit.ReadOnly);
  SelAvail:=(ASrcEdit<>nil) and (ASrcEdit.SelectionAvailable);
  SelEditable:=Editable and SelAvail;
  with MainIDEBar do begin
    itmEditUndo.Enabled:=Editable;
    itmEditRedo.Enabled:=Editable;
  //itmEditClipboard: TIDEMenuSection;
    itmEditCut.Enabled:=SelEditable;
    itmEditCopy.Enabled:=SelAvail;
    itmEditPaste.Enabled:=Editable;
  //itmEditBlockIndentation: TIDEMenuSection;
    itmEditIndentBlock.Enabled:=SelEditable;
    itmEditUnindentBlock.Enabled:=SelEditable;
    itmEditEncloseBlock.Enabled:=SelEditable;
    itmEditCommentBlock.Enabled:=SelEditable;
    itmEditUncommentBlock.Enabled:=SelEditable;
    itmEditConditionalBlock.Enabled:=SelEditable;
    itmEditSortBlock.Enabled:=SelEditable;
  //itmEditBlockCharConversion: TIDEMenuSection;
    itmEditUpperCaseBlock.Enabled:=SelEditable;
    itmEditLowerCaseBlock.Enabled:=SelEditable;
    itmEditTabsToSpacesBlock.Enabled:=SelEditable;
    itmEditSelectionBreakLines.Enabled:=SelEditable;
  //itmEditSelect: TIDEMenuSection;
    //itmEditSelectAll: TIDEMenuCommand;
    //itmEditSelectToBrace: TIDEMenuCommand;
    //itmEditSelectCodeBlock: TIDEMenuCommand;
    //itmEditSelectLine: TIDEMenuCommand;
    //itmEditSelectParagraph: TIDEMenuCommand;
  //itmEditInsertions: TIDEMenuSection;
    itmEditInsertCharacter.Enabled:=Editable;
    //itmEditInsertText: TIDEMenuSection;
      //itmEditInsertCVSKeyWord: TIDEMenuSection;
        itmEditInsertCVSAuthor.Enabled:=Editable;
        itmEditInsertCVSDate.Enabled:=Editable;
        itmEditInsertCVSHeader.Enabled:=Editable;
        itmEditInsertCVSID.Enabled:=Editable;
        itmEditInsertCVSLog.Enabled:=Editable;
        itmEditInsertCVSName.Enabled:=Editable;
        itmEditInsertCVSRevision.Enabled:=Editable;
        itmEditInsertCVSSource.Enabled:=Editable;
      //itmEditInsertGeneral: TIDEMenuSection;
        itmEditInsertGPLNotice.Enabled:=Editable;
        itmEditInsertLGPLNotice.Enabled:=Editable;
        itmEditInsertModifiedLGPLNotice.Enabled:=Editable;
        itmEditInsertUsername.Enabled:=Editable;
        itmEditInsertDateTime.Enabled:=Editable;
        itmEditInsertChangeLogEntry.Enabled:=Editable;
  //itmEditMenuCodeTools: TIDEMenuSection;
    itmEditCompleteCode.Enabled:=Editable;
    itmEditExtractProc.Enabled:=SelEditable;
  end;
end;

{------------------------------------------------------------------------------}
procedure TMainIDE.mnuViewInspectorClicked(Sender: TObject);
begin
  DoBringToFrontFormOrInspector(true);
end;

procedure TMainIDE.mnuViewSourceEditorClicked(Sender: TObject);
begin
  SourceNotebook.ShowOnTop;
end;

{------------------------------------------------------------------------------}

Procedure TMainIDE.mnuViewUnitsClicked(Sender: TObject);
begin
  DoViewUnitsAndForms(false);
end;

Procedure TMainIDE.mnuViewFormsClicked(Sender: TObject);
Begin
  DoViewUnitsAndForms(true);
end;

Procedure TMainIDE.mnuViewUnitDependenciesClicked(Sender: TObject);
begin
  DoViewUnitDependencies;
end;

procedure TMainIDE.mnuViewUnitInfoClicked(Sender: TObject);
begin
  DoViewUnitInfo;
end;

Procedure TMainIDE.mnuViewCodeExplorerClick(Sender: TObject);
begin
  DoShowCodeExplorer;
end;

Procedure TMainIDE.mnuViewCodeBrowserClick(Sender: TObject);
begin
  DoShowCodeBrowser;
end;

Procedure TMainIDE.mnuViewComponentsClick(Sender: TObject);
begin
  DoShowComponentList;
end;

Procedure TMainIDE.mnuViewMessagesClick(Sender: TObject);
begin
  // it was already visible, but user does not see it, try to move in view
  MessagesView.EnsureVisible;
end;

Procedure TMainIDE.mnuViewSearchResultsClick(Sender: TObject);
Begin
  CreateSearchResultWindow;
  SearchResultsView.ShowOnTop;
End;

Procedure TMainIDE.mnuNewProjectClicked(Sender: TObject);
var
  NewProjectDesc: TProjectDescriptor;
Begin
  NewProjectDesc:=nil;
  if ChooseNewProject(NewProjectDesc)<>mrOk then exit;
  //debugln('TMainIDE.mnuNewProjectClicked ',dbgsName(NewProjectDesc));
  DoNewProject(NewProjectDesc);
end;

procedure TMainIDE.mnuNewProjectFromFileClicked(Sender: TObject);
var
  OpenDialog:TOpenDialog;
  AFilename: string;
  PreReadBuf: TCodeBuffer;
Begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseProgramSourcePpPasLpr;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist,ofFileMustExist];
    if OpenDialog.Execute then begin
      AFilename:=ExpandFilename(OpenDialog.Filename);
      if not FilenameIsPascalSource(AFilename) then begin
        MessageDlg(lisPkgMangInvalidFileExtension,
          lisProgramSourceMustHaveAPascalExtensionLikePasPpOrLp,
          mtError,[mbOk],0);
        exit;
      end;
      if mrOk<>LoadCodeBuffer(PreReadBuf,AFileName,
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

Procedure TMainIDE.mnuOpenProjectClicked(Sender: TObject);
var
  OpenDialog:TOpenDialog;
  AFileName: string;
begin
  if (Sender is TIDEMenuItem)
  and (TIDEMenuItem(Sender).Section=itmProjectRecentOpen) then begin
    AFileName:=ExpandFilename(TIDEMenuItem(Sender).Caption);
    if DoOpenProjectFile(AFilename,[ofAddToRecent])=mrOk then begin
      AddRecentProjectFileToEnvironment(AFilename);
    end else begin
      // open failed
      if not FileExists(AFilename) then begin
        EnvironmentOptions.RemoveFromRecentProjectFiles(AFilename);
      end else
        AddRecentProjectFileToEnvironment(AFilename);
    end;
  end
  else begin
    OpenDialog:=TOpenDialog.Create(nil);
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Title:=lisOpenProjectFile+' (*.lpi)';
      OpenDialog.Filter := lisLazarusProjectInfoFile+' (*.lpi)|*.lpi|'
                          +lisAllFiles+'|'+GetAllFilesMask;
      if OpenDialog.Execute then begin
        AFilename:=ExpandFilename(OpenDialog.Filename);
        DoOpenProjectFile(AFilename,[ofAddToRecent]);
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    finally
      OpenDialog.Free;
    end;
  end;
end;

procedure TMainIDE.mnuCloseProjectClicked(Sender: TObject);
var
  DlgResult: TModalResult;
begin
  // stop debugging/compiling/...
  if not DoResetToolStatus(true) then exit;

  // check foreign windows
  if not CloseQueryIDEWindows then exit;

  // check project
  if SomethingOfProjectIsModified then begin
    DlgResult:=QuestionDlg(lisProjectChanged,
      Format(lisSaveChangesToProject, [Project1.Title]), mtConfirmation,
      [mrYes, lisMenuSave, mrNo, lisDiscardChanges,
       mrAbort, lisDoNotCloseTheProject],
      0);
    case DlgResult of
    mrYes:
      if not (DoSaveProject([]) in [mrOk,mrIgnore]) then exit;
    mrCancel, mrAbort:
      Exit;
    end;
  end;

  // close
  DoCloseProject;
  
  // ask what to do next
  while Project1=nil do begin
    DlgResult:=QuestionDlg(lisProjectClosed,
      Format(lisTheProjectIsClosedThereAreNowThreePossibilitiesHin, [#13]),
      mtInformation,
      [mrNo, lisQuitLazarus, mrYes, lisCreateNewProject, mrOk, lisOpenProject2
        ], 0);
    case DlgResult of
    mrNo:
      if QuitIDE then exit;
    mrYes:
      mnuNewProjectClicked(Sender);
    mrOk:
      mnuOpenProjectClicked(Sender);
    end;
  end;
end;

Procedure TMainIDE.mnuSaveProjectClicked(Sender: TObject);
Begin
  DoSaveProject([]);
end;

procedure TMainIDE.mnuSaveProjectAsClicked(Sender: TObject);
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

procedure TMainIDE.mnuAddToProjectClicked(Sender: TObject);
begin
  DoAddActiveUnitToProject;
end;

procedure TMainIDE.mnuRemoveFromProjectClicked(Sender: TObject);
begin
  DoRemoveFromProjectDialog;
end;

procedure TMainIDE.mnuViewProjectSourceClicked(Sender: TObject);
begin
  DoOpenMainUnit([]);
end;

procedure TMainIDE.mnuViewProjectTodosClicked(Sender: TObject);
begin
  DoShowToDoList;
end;

procedure TMainIDE.mnuProjectOptionsClicked(Sender: TObject);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  BeginCodeTool(ActiveSrcEdit, ActiveUnitInfo, []);
  if ShowProjectOptionsDialog(Project1)=mrOk then begin

  end;
end;

{$ifdef EnableNewDialogs}
procedure TMainIDE.mnuProjectOptionsClicked_new(Sender: TObject);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  BeginCodeTool(ActiveSrcEdit, ActiveUnitInfo, []);
  if projectopts_new.ShowProjectOptionsDialog(Project1)=mrOk then begin

  end;
end;
{$endif}

function TMainIDE.UpdateProjectPOFile(AProject: TProject): TModalResult;
var
  Files: TStringList;
  POFilename: String;
  AnUnitInfo: TUnitInfo;
  CurFilename: String;
  POFileAge: LongInt;
  POFileAgeValid: Boolean;
  POOutDir: String;
  LRTFilename: String;
  RSTFilename: String;
begin
  Result:=mrCancel;
  if (not AProject.EnableI18N) or AProject.IsVirtual then exit(mrOk);

  POFilename := MainBuildBoss.GetProjectTargetFilename;
  if POFilename='' then begin
    DebugLn(['TMainIDE.UpdateProjectPOFile unable to get project target filename']);
    exit;
  end;
  POFilename:=ChangeFileExt(POFilename, '.po');

  if AProject.POOutputDirectory <> '' then begin
    POOutDir:=AProject.GetPOOutDirectory;
    if POOutDir<>'' then
      POFilename:=TrimFilename(AppendPathDelim(POOutDir)+ExtractFileName(POFilename));
  end;
  
  POFileAgeValid:=false;
  if FileExistsCached(POFilename) then begin
    POFileAge:=FileAge(POFilename);
    POFileAgeValid:=true;
  end;
  
  //DebugLn(['TMainIDE.UpdateProjectPOFile Updating POFilename="',POFilename,'"']);

  Files := TStringList.Create;
  try
    AnUnitInfo:=AProject.FirstPartOfProject;
    while AnUnitInfo<>nil do begin
      CurFilename:=AnUnitInfo.Filename;
      if (not AnUnitInfo.IsVirtual) and FilenameIsPascalSource(CurFilename) then
      begin
        // check .lst file
        LRTFilename:=ChangeFileExt(CurFilename,'.lrt');
        if FileExistsCached(LRTFilename)
        and ((not POFileAgeValid) or (FileAge(LRTFilename)>POFileAge)) then
          Files.Add(LRTFilename);
        // check .rst file
        RSTFilename:=ChangeFileExt(CurFilename,'.rst');
        if FileExistsCached(RSTFilename)
        and ((not POFileAgeValid) or (FileAge(RSTFilename)>POFileAge)) then
          Files.Add(RSTFilename);
      end;
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
    Result:=AddFiles2Po(Files, POFilename);
  finally
    Files.Destroy;
  end;
end;

Procedure TMainIDE.mnuBuildProjectClicked(Sender: TObject);
Begin
  DoBuildProject(crCompile,[]);
end;

Procedure TMainIDE.mnuBuildAllProjectClicked(Sender: TObject);
Begin
  DoBuildProject(crBuild,[pbfCleanCompile,pbfCompileDependenciesClean]);
end;

procedure TMainIDE.mnuQuickCompileProjectClicked(Sender: TObject);
begin
  DoQuickCompile;
end;

Procedure TMainIDE.mnuAbortBuildProjectClicked(Sender: TObject);
Begin
  DoAbortBuild;
end;

Procedure TMainIDE.mnuRunProjectClicked(Sender: TObject);
begin
  DoRunProject;
end;

Procedure TMainIDE.mnuPauseProjectClicked(Sender: TObject);
begin
  DebugBoss.DoPauseProject;
end;

Procedure TMainIDE.mnuStepIntoProjectClicked(Sender: TObject);
begin
  DebugBoss.DoStepIntoProject;
end;

Procedure TMainIDE.mnuStepOverProjectClicked(Sender: TObject);
begin
  DebugBoss.DoStepOverProject;
end;

Procedure TMainIDE.mnuRunToCursorProjectClicked(Sender: TObject);
begin
  DebugBoss.DoRunToCursor;
end;

Procedure TMainIDE.mnuStopProjectClicked(Sender: TObject);
begin
  DebugBoss.DoStopProject;
end;

procedure TMainIDE.mnuProjectCompilerSettingsClicked(Sender: TObject);
var
  frmCompilerOptions: TfrmCompilerOptions;
  NewCaption: String;
begin
  frmCompilerOptions:=TfrmCompilerOptions.Create(nil);
  try
    NewCaption:=Project1.Title;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(Project1.ProjectInfoFile);
    frmCompilerOptions.Caption:=Format(lisCompilerOptionsForProject, [NewCaption
      ]);
    frmCompilerOptions.CompilerOpts:=Project1.CompilerOptions;
    frmCompilerOptions.GetCompilerOptions;
    frmCompilerOptions.OnTest:=@OnCompilerOptionsDialogTest;
    frmCompilerOptions.OnImExportCompilerOptions:=@OnCompilerOptionsImExport;
    if frmCompilerOptions.ShowModal=mrOk then begin
      MainBuildBoss.RescanCompilerDefines(true);
      Project1.DefineTemplates.AllChanged;
      IncreaseCompilerGraphStamp;
    end;
  finally
    frmCompilerOptions.Free;
  end;
end;

{$ifdef EnableNewDialogs}
procedure TMainIDE.mnuProjectCompilerSettingsClicked_new(Sender: TObject);
var
  frmCompilerOptions: TfrmCompilerOptionsNew;
  NewCaption: String;
begin
  frmCompilerOptions:=TfrmCompilerOptionsNew.Create(nil);
  try
    NewCaption:=Project1.Title;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(Project1.ProjectInfoFile);
    frmCompilerOptions.Caption:=Format(lisCompilerOptionsForProject, [NewCaption
      ]);
    frmCompilerOptions.CompilerOpts:=Project1.CompilerOptions;
    frmCompilerOptions.GetCompilerOptions;
    frmCompilerOptions.OnTest:=@OnCompilerOptionsDialogTest;
    frmCompilerOptions.OnImExportCompilerOptions:=@OnCompilerOptionsImExport;
    if frmCompilerOptions.ShowModal=mrOk then begin
      MainBuildBoss.RescanCompilerDefines(true);
      Project1.DefineTemplates.AllChanged;
      IncreaseCompilerGraphStamp;
    end;
  finally
    frmCompilerOptions.Free;
  end;
end;
{$endif}

procedure TMainIDE.mnuBuildFileClicked(Sender: TObject);
begin
  DoBuildFile;
end;

procedure TMainIDE.mnuRunFileClicked(Sender: TObject);
begin
  DoRunFile;
end;

procedure TMainIDE.mnuConfigBuildFileClicked(Sender: TObject);
begin
  DoConfigBuildFile;
end;

procedure TMainIDE.mnuRunParametersClicked(Sender: TObject);
begin
  if ShowRunParamsOptsDlg(Project1.RunParameterOptions)=mrOK then
    Project1.Modified:=true;
end;

//------------------------------------------------------------------------------

procedure TMainIDE.mnuToolConfigureClicked(Sender: TObject);
begin
  if ShowExtToolDialog(EnvironmentOptions.ExternalTools,GlobalMacroList)=mrOk then
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

procedure TMainIDE.mnuToolSyntaxCheckClicked(Sender: TObject);
begin
  DoCheckSyntax;
end;

procedure TMainIDE.mnuToolGuessUnclosedBlockClicked(Sender: TObject);
begin
  DoJumpToGuessedUnclosedBlock(true);
end;

procedure TMainIDE.mnuToolGuessMisplacedIFDEFClicked(Sender: TObject);
begin
  DoJumpToGuessedMisplacedIFDEF(true);
end;

procedure TMainIDE.mnuToolMakeResourceStringClicked(Sender: TObject);
begin
  DoMakeResourceString;
end;

procedure TMainIDE.mnuToolDiffClicked(Sender: TObject);
begin
  DoDiff;
end;

procedure TMainIDE.mnuViewLazDocClicked(Sender: TObject);
begin
  DoShowLazDoc;
end;

procedure TMainIDE.mnuToolConvertDFMtoLFMClicked(Sender: TObject);
begin
  DoConvertDFMtoLFM;
end;

procedure TMainIDE.mnuToolCheckLFMClicked(Sender: TObject);
begin
  DoCheckLFMInEditor;
end;

procedure TMainIDE.mnuToolConvertDelphiUnitClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentFilesMenu;
    SaveEnvironment;
  end;

var
  OpenDialog: TOpenDialog;
  AFilename: string;
  i: Integer;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiUnit;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    if InputHistories.LastConvertDelphiUnit<>'' then begin
      OpenDialog.InitialDir:=
                       ExtractFilePath(InputHistories.LastConvertDelphiUnit);
      OpenDialog.Filename:=
                       ExtractFileName(InputHistories.LastConvertDelphiUnit);
    end;
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      for i := 0 to OpenDialog.Files.Count-1 do begin
        AFilename:=CleanAndExpandFilename(OpenDialog.Files.Strings[i]);
        if FileExists(AFilename)
        and (DoConvertDelphiUnit(AFilename)=mrAbort) then
          break;
      end;
      UpdateEnvironment;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TMainIDE.mnuToolConvertDelphiProjectClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentFilesMenu;
    SaveEnvironment;
  end;

var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiProject;
    OpenDialog.Filter:=lisDelphiProject+' (*.dpr)|*.dpr|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
    if InputHistories.LastConvertDelphiProject<>'' then begin
      OpenDialog.InitialDir:=
                       ExtractFilePath(InputHistories.LastConvertDelphiProject);
      OpenDialog.Filename:=
                       ExtractFileName(InputHistories.LastConvertDelphiProject);
    end;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExists(AFilename) then
        DoConvertDelphiProject(AFilename);
      UpdateEnvironment;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TMainIDE.mnuToolConvertDelphiPackageClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentFilesMenu;
    SaveEnvironment;
  end;

var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisChooseDelphiPackage;
    OpenDialog.Filter:=lisDelphiProject+' (*.dpk)|*.dpk|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
    if InputHistories.LastConvertDelphiPackage<>'' then begin
      OpenDialog.InitialDir:=
                       ExtractFilePath(InputHistories.LastConvertDelphiPackage);
      OpenDialog.Filename:=
                       ExtractFileName(InputHistories.LastConvertDelphiPackage);
    end;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      //debugln('TMainIDE.mnuToolConvertDelphiProjectClicked A ',AFilename);
      if FileExists(AFilename) then
        DoConvertDelphiPackage(AFilename);
      UpdateEnvironment;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TMainIDE.mnuToolBuildLazarusClicked(Sender: TObject);
begin
  if MiscellaneousOptions.BuildLazOpts.ConfirmBuild then
    if MessageDlg(lisConfirmLazarusRebuild, mtConfirmation, mbYesNo, 0)=mrNo then
      exit;
  DoBuildLazarus([]);
end;

procedure TMainIDE.mnuToolConfigBuildLazClicked(Sender: TObject);
var
  CmdLineDefines: TDefineTemplate;
  LazSrcTemplate: TDefineTemplate;
  LazSrcDirTemplate: TDefineTemplate;
  DlgResult: TModalResult;
begin
  DlgResult:=ShowConfigureBuildLazarusDlg(MiscellaneousOptions.BuildLazOpts);
  if DlgResult in [mrOk,mrYes] then begin
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
  if DlgResult=mrYes then
    DoBuildLazarus([]);
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.mnuCustomExtToolClick(Sender: TObject);

  Handler for clicking on a menuitem for a custom external tool.
-------------------------------------------------------------------------------}
procedure TMainIDE.mnuCustomExtToolClick(Sender: TObject);
var
  Index: integer;
begin
  if not (Sender is TIDEMenuItem) then exit;
  Index:=itmCustomTools.IndexOf(TIDEMenuItem(Sender))-1;
  if (Index<0) or (Index>=EnvironmentOptions.ExternalTools.Count) then exit;
  DoRunExternalTool(Index);
end;

procedure TMainIDE.mnuEnvGeneralOptionsClicked(Sender: TObject);
begin
  DoShowEnvGeneralOptions(eodpFiles);
end;

{$ifdef EnableNewDialogs}
procedure TMainIDE.mnuEnvGeneralOptionsClicked_new(Sender: TObject);
  // I really saw no way to add something temporary to show the temp new dialog
  // so I copied the whole routine. This shouldn't be long here anyway (MWE)

var
  EnvironmentOptionsDialog: TEnvironmentOptionsDialogNew;
  MacroValueChanged, FPCSrcDirChanged, FPCCompilerChanged: boolean;
  OldCompilerFilename: string;
  OldLanguage: String;

  procedure ChangeMacroValue(const MacroName, NewValue: string);
  begin
    with CodeToolBoss.GlobalValues do begin
      if Variables[ExternalMacroStart+MacroName]=NewValue then exit;
      FPCSrcDirChanged:=FPCSrcDirChanged or (Macroname='FPCSrcDir');
      Variables[ExternalMacroStart+MacroName]:=NewValue;
    end;
    MacroValueChanged:=true;
  end;

  procedure UpdateDesigners;
  var
    AForm: TCustomForm;
    AnUnitInfo: TUnitInfo;
    ADesigner: TDesigner;
  begin
    AnUnitInfo:=Project1.FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      if (AnUnitInfo.Component<>nil)
      then begin
        AForm:=FormEditor1.GetDesignerForm(AnUnitInfo.Component);
        ADesigner:=TDesigner(AForm.Designer);
        if ADesigner<>nil then begin
          ADesigner.ShowEditorHints:=EnvironmentOptions.ShowEditorHints;
          ADesigner.ShowComponentCaptionHints:=
            EnvironmentOptions.ShowComponentCaptions;
        end;
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
    InvalidateAllDesignerForms;
  end;

  procedure UpdateObjectInspector;
  begin
    EnvironmentOptions.ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;

begin
  EnvironmentOptionsDialog:=TEnvironmentOptionsDialogNew.Create(nil);
  try
    EnvironmentOptionsDialog.CategoryPage:=eodpFiles;
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
      OldLanguage:=EnvironmentOptions.LanguageID;
      EnvironmentOptionsDialog.WriteSettings(EnvironmentOptions);

      UpdateDefaultPascalFileExtensions;

      //DebugLn(['TMainIDE.DoShowEnvGeneralOptions OldLanguage=',OldLanguage,' EnvironmentOptions.LanguageID=',EnvironmentOptions.LanguageID]);
      if OldLanguage<>EnvironmentOptions.LanguageID then begin
        TranslateResourceStrings(EnvironmentOptions.LazarusDirectory,
                                 EnvironmentOptions.LanguageID);
        PkgBoss.TranslateResourceStrings;
      end;

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
        MainBuildBoss.RescanCompilerDefines(false);
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
end;
{$endif}

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
var
  DefPasExt: string;
begin
  // change default pascal file extensions
  DefPasExt:=PascalExtension[EnvironmentOptions.PascalFileExtension];
  if LazProjectFileDescriptors<>nil then
    LazProjectFileDescriptors.DefaultPascalFileExt:=DefPasExt;
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
var
  EnvironmentOptionsDialog: TEnvironmentOptionsDialog;
  MacroValueChanged, FPCSrcDirChanged, FPCCompilerChanged: boolean;
  OldCompilerFilename: string;
  OldLanguage: String;

  procedure ChangeMacroValue(const MacroName, NewValue: string);
  begin
    with CodeToolBoss.GlobalValues do begin
      if Variables[ExternalMacroStart+MacroName]=NewValue then exit;
      FPCSrcDirChanged:=FPCSrcDirChanged or (Macroname='FPCSrcDir');
      Variables[ExternalMacroStart+MacroName]:=NewValue;
    end;
    MacroValueChanged:=true;
  end;

  procedure UpdateDesigners;
  var
    AForm: TCustomForm;
    AnUnitInfo: TUnitInfo;
    ADesigner: TDesigner;
  begin
    AnUnitInfo:=Project1.FirstUnitWithComponent;
    while AnUnitInfo<>nil do begin
      if (AnUnitInfo.Component<>nil)
      then begin
        AForm:=FormEditor1.GetDesignerForm(AnUnitInfo.Component);
        ADesigner:=TDesigner(AForm.Designer);
        if ADesigner<>nil then begin
          ADesigner.ShowEditorHints:=EnvironmentOptions.ShowEditorHints;
          ADesigner.ShowComponentCaptionHints:=
            EnvironmentOptions.ShowComponentCaptions;
        end;
      end;
      AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    end;
    InvalidateAllDesignerForms;
  end;

  procedure UpdateObjectInspector;
  begin
    EnvironmentOptions.ObjectInspectorOptions.AssignTo(ObjectInspector1);
  end;

Begin
  EnvironmentOptionsDialog:=TEnvironmentOptionsDialog.Create(nil);
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
      OldLanguage:=EnvironmentOptions.LanguageID;
      EnvironmentOptionsDialog.WriteSettings(EnvironmentOptions);
      
      UpdateDefaultPascalFileExtensions;

      //DebugLn(['TMainIDE.DoShowEnvGeneralOptions OldLanguage=',OldLanguage,' EnvironmentOptions.LanguageID=',EnvironmentOptions.LanguageID]);
      if OldLanguage<>EnvironmentOptions.LanguageID then begin
        TranslateResourceStrings(EnvironmentOptions.LazarusDirectory,
                                 EnvironmentOptions.LanguageID);
        PkgBoss.TranslateResourceStrings;
      end;

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
        MainBuildBoss.RescanCompilerDefines(false);
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

procedure TMainIDE.mnuEnvEditorOptionsClicked(Sender: TObject);
var EditorOptionsForm: TEditorOptionsForm;
Begin
  EditorOptionsForm:=TEditorOptionsForm.Create(nil);
  try
    Project1.UpdateCustomHighlighter;
    if EditorOptionsForm.ShowModal=mrOk then begin
      Project1.UpdateSyntaxHighlighter;
      SourceNotebook.ReloadEditorOptions;
      ReloadMenuShortCuts;
    end;
  finally
    EditorOptionsForm.Free;
  end;
End;

{$ifdef EnableNewDialogs}
procedure TMainIDE.mnuEnvEditorOptionsClicked_new(Sender: TObject);
var
  EditorOptionsForm: TEditorOptionsFormNew;
begin
  EditorOptionsForm:=TEditorOptionsFormNew.Create(nil);
  try
    Project1.UpdateCustomHighlighter;
    if EditorOptionsForm.ShowModal=mrOk
    then begin
      Project1.UpdateSyntaxHighlighter;
      SourceNotebook.ReloadEditorOptions;
      ReloadMenuShortCuts;
    end;
  finally
    EditorOptionsForm.Free;
  end;
end;
{$endif}

procedure TMainIDE.mnuEnvCodeTemplatesClicked(Sender: TObject);
begin
  if ShowCodeTemplateDialog=mrOk then
    SourceNotebook.ReloadEditorOptions;
end;

procedure TMainIDE.mnuEnvCodeToolsOptionsClicked(Sender: TObject);
begin
  ShowCodeToolsOptions(CodeToolsOpts,@SourceNoteBook.GetSynEditPreviewSettings);
end;

procedure TMainIDE.mnuEnvCodeToolsDefinesEditorClicked(Sender: TObject);
begin
  ShowCodeToolsDefinesEditor(CodeToolBoss,CodeToolsOpts,GlobalMacroList);
end;

procedure TMainIDE.mnuEnvRescanFPCSrcDirClicked(Sender: TObject);
begin
  MainBuildBoss.RescanCompilerDefines(false);
end;

procedure TMainIDE.SaveEnvironment;
begin
  SaveDesktopSettings(EnvironmentOptions);
  EnvironmentOptions.Save(false);
  //debugln('TMainIDE.SaveEnvironment A ',dbgsName(ObjectInspector1.Favourites));
  if (ObjectInspector1<>nil) and (ObjectInspector1.Favourites<>nil) then
    SaveOIFavouriteProperties(ObjectInspector1.Favourites);
end;

//==============================================================================

function TMainIDE.CreateNewCodeBuffer(Descriptor: TProjectFileDescriptor;
  NewOwner: TObject; NewFilename: string;
  var NewCodeBuffer: TCodeBuffer; var NewUnitName: string): TModalResult;
var
  NewShortFilename: String;
  NewFileExt: String;
  SearchFlags: TSearchIDEFileFlags;
begin
  //debugln('TMainIDE.CreateNewCodeBuffer START NewFilename=',NewFilename,' ',Descriptor.DefaultFilename,' ',Descriptor.ClassName);
  NewUnitName:='';
  if NewFilename='' then begin
    // create a new unique filename
    SearchFlags:=[siffCheckAllProjects];
    if Descriptor.IsPascalUnit then begin
      if NewUnitName='' then
        NewUnitName:=Descriptor.DefaultSourceName;
      NewShortFilename:=lowercase(NewUnitName);
      NewFileExt:=Descriptor.DefaultFileExt;
      SearchFlags:=SearchFlags+[siffIgnoreExtension];
    end else begin
      NewFilename:=ExtractFilename(Descriptor.DefaultFilename);
      NewShortFilename:=ExtractFilenameOnly(NewFilename);
      NewFileExt:=ExtractFileExt(NewFilename);
      SearchFlags:=[];
    end;
    NewFilename:=CreateNewUniqueFilename(NewShortFilename,NewFileExt,NewOwner,
                                         SearchFlags,true);
    if NewFilename='' then
      RaiseException('');
    NewShortFilename:=ExtractFilenameOnly(NewFilename);
    // use as unitname the NewShortFilename, but with the case of the
    // original unitname. e.g. 'unit12.pas' becomes 'Unit12.pas'
    if Descriptor.IsPascalUnit then begin
      NewUnitName:=ChompEndNumber(NewUnitName);
      NewUnitName:=NewUnitName+copy(NewShortFilename,length(NewUnitName)+1,
                                    length(NewShortFilename));
    end;
  end;
  //debugln('TMainIDE.CreateNewCodeBuffer NewFilename=',NewFilename,' NewUnitName=',NewUnitName);

  if FilenameIsPascalUnit(NewFilename) then begin
    if NewUnitName='' then
      NewUnitName:=ExtractFileNameOnly(NewFilename);
    if EnvironmentOptions.CharcaseFileAction in [ccfaAsk, ccfaAutoRename] then
      NewFilename:=ExtractFilePath(NewFilename)
                   +lowercase(ExtractFileName(NewFilename));
  end;

  NewCodeBuffer:=CodeToolBoss.CreateFile(NewFilename);
  if NewCodeBuffer<>nil then
    Result:=mrOk
  else
    Result:=mrCancel;
end;

function TMainIDE.CreateNewForm(NewUnitInfo: TUnitInfo;
  AncestorType: TPersistentClass; ResourceCode: TCodeBuffer): TModalResult;
var
  CInterface: TComponentInterface;
  NewComponent: TComponent;
  new_x, new_y: integer;
begin
  if not AncestorType.InheritsFrom(TComponent) then
    RaiseException('TMainIDE.CreateNewForm invalid AncestorType');

  //debugln('TMainIDE.CreateNewForm START ',NewUnitInfo.Filename,' ',AncestorType.ClassName,' ',dbgs(ResourceCode<>nil));
  // create a buffer for the new resource file and for the LFM file
  if ResourceCode=nil then begin
    ResourceCode:=
      CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,
                              ResourceFileExt));
  end;
  //debugln('TMainIDE.CreateNewForm B ',ResourceCode.Filename);
  ResourceCode.Source:='{ '+lisResourceFileComment+' }';
  CodeToolBoss.CreateFile(ChangeFileExt(NewUnitInfo.Filename,'.lfm'));

  // clear formeditor
  FormEditor1.ClearSelection;

  // Figure out where we want to put the new form
  // if there is more place left of the OI put it left, otherwise right
  new_x:=ObjectInspector1.Left+ObjectInspector1.Width; //+60;
  new_y:=MainIDEBar.Top+MainIDEBar.Height; //+80;
  if screen.width-new_x>=ObjectInspector1.left then inc(new_x, 60) else new_x:=16;
  if screen.height-new_y>=MainIDEBar.top then inc(new_y, 80) else new_y:=24;

  // create jit component
  CInterface := TComponentInterface(
    FormEditor1.CreateComponent(nil,TComponentClass(AncestorType),
      NewUnitInfo.CreateUnitName, new_x, new_y, 400,300));
  FormEditor1.SetComponentNameAndClass(CInterface,
    NewUnitInfo.ComponentName,'T'+NewUnitInfo.ComponentName);
  NewComponent:=CInterface.Component;
  if NewComponent is TControl then
    TControl(NewComponent).Visible:=false;
  NewUnitInfo.Component:=NewComponent;
  CreateDesignerForComponent(NewComponent);

  NewUnitInfo.ComponentName:=NewComponent.Name;
  NewUnitInfo.ComponentResourceName:=NewUnitInfo.ComponentName;
  if NewUnitInfo.IsPartOfProject and Project1.AutoCreateForms
  and (pfMainUnitHasCreateFormStatements in Project1.Flags) then begin
    Project1.AddCreateFormToProjectFile(NewComponent.ClassName,
                                        NewComponent.Name);
  end;

  Result:=mrOk;
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
    //writeln('TMainIDE.DoLoadResourceFile A "',AnUnitInfo.Filename,'" "',AnUnitInfo.ResourceFileName,'"');
    // first try to find the resource file (.lrs) via the unit source
    LinkIndex:=-1;
    ResourceCode:=CodeToolBoss.FindNextResourceFile(
                                                   AnUnitInfo.Source,LinkIndex);
    // if unit source has errors, then show the error and try the last resource
    // file (.lrs)
    if (ResourceCode=nil) and (CodeToolBoss.ErrorMessage<>'') then begin
      if not IgnoreSourceErrors then
        DoJumpToCodeToolBossError;
      if (AnUnitInfo.ResourceFileName<>'') then begin
        Result:=LoadCodeBuffer(ResourceCode,AnUnitInfo.ResourceFileName,
                               [lbfCheckIfText]);
        if Result=mrAbort then exit;
      end;
    end;
    // if no resource file found (i.e. normally the .lrs file)
    // then tell the user
    if (ResourceCode=nil) and (not IgnoreSourceErrors) then begin
      MsgTxt:=Format(lisUnableToLoadOldResourceFileTheResourceFileIs, [#13,
        #13, #13, AnUnitInfo.UnitName, #13]);
      Result:=QuestionDlg(lisResourceLoadError, MsgTxt, mtWarning,
                         [mrIgnore, lisIgnoreMissingFile, mrAbort], 0);
      if Result=mrAbort then exit;
    end;

    // then load the lfm file (without parsing)
    if (not AnUnitInfo.IsVirtual) and (AnUnitInfo.Component<>nil) then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
      if (FileExists(LFMFilename)) then begin
        Result:=LoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText]);
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
  AlternativeUnitName: string;
  ACaption, AText: string;
  SrcEdit: TSourceEditor;
  FileWithoutPath: String;
  PkgDefaultDirectory: String;
  OldUnitName: String;
begin
  SrcEdit:=GetSourceEditorForUnitInfo(AnUnitInfo);
  //debugln('TMainIDE.DoShowSaveFileAsDialog ',AnUnitInfo.Filename);

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
  OldUnitName:=AnUnitInfo.ParseUnitNameFromSource(false);
  //debugln('TMainIDE.DoShowSaveFileAsDialog sourceunitname=',OldUnitName);
  SaveAsFilename:=OldUnitName;
  if SaveAsFilename='' then
    SaveAsFilename:=ExtractFileNameOnly(AnUnitInfo.Filename);
  if SaveAsFilename='' then
    SaveAsFilename:=lisnoname;

  // let user choose a filename
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=lisSaveSpace+SaveAsFilename+' (*'+SaveAsFileExt+')';
    SaveDialog.FileName:=SaveAsFilename+SaveAsFileExt;
    // if this is a project file, start in project directory
    if AnUnitInfo.IsPartOfProject and (not Project1.IsVirtual)
    and (not FileIsInPath(SaveDialog.InitialDir,Project1.ProjectDirectory)) then
    begin
      SaveDialog.InitialDir:=Project1.ProjectDirectory;
    end;
    // if this is a package file, then start in package directory
    PkgDefaultDirectory:=
      PkgBoss.GetDefaultSaveDirectoryForFile(AnUnitInfo.Filename);
    if (PkgDefaultDirectory<>'')
    and (not FileIsInPath(SaveDialog.InitialDir,PkgDefaultDirectory)) then
      SaveDialog.InitialDir:=PkgDefaultDirectory;
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

  // check file extension
  if ExtractFileExt(NewFilename)='' then begin
    NewFilename:=NewFilename+SaveAsFileExt;
  end;

  // check file path
  NewFilePath:=ExtractFilePath(NewFilename);
  if not DirPathExists(NewFilePath) then begin
    ACaption:=lisEnvOptDlgDirectoryNotFound;
    AText:=Format(lisTheDestinationDirectoryDoesNotExist, [#13, '"',
      NewFilePath, '"']);
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
      Result:=QuestionDlg(lisUnitNameAlreadyExistsCap,
         Format(lisTheUnitAlreadyExistsIgnoreWillForceTheRenaming, ['"',
           NewUnitName, '"', #13, #13, #13]),
          mtConfirmation, [mrIgnore, lisForceRenaming,
                          mrCancel, lisCancelRenaming,
                          mrAbort, lisAbortAll], 0);
      if Result=mrIgnore then
        Result:=mrCancel
      else
        exit;
    end;
  end else begin
    NewUnitName:='';
  end;

  // check filename
  if FilenameIsPascalUnit(NewFilename) then begin
    FileWithoutPath:=ExtractFileName(NewFilename);
    // check if file should be auto renamed

    if EnvironmentOptions.CharcaseFileAction = ccfaAsk then begin
      if lowercase(FileWithoutPath)<>FileWithoutPath
      then begin
        Result:=QuestionDlg(lisRenameFile,
             Format(lisThisLooksLikeAPascalFileItIsRecommendedToUseLowerC, [
               #13, #13]),
          mtWarning, [mrYes, lisRenameToLowercase, mrNo, lisKeepName], 0);
        if Result=mrYes then
          NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
        Result:=mrOk;
      end;
    end else begin
      if EnvironmentOptions.CharcaseFileAction = ccfaAutoRename then
        NewFileName:=ExtractFilePath(NewFilename)+lowercase(FileWithoutPath);
    end;
  end;

  // check overwrite existing file
  if (AnUnitInfo.IsVirtual
      or (CompareFilenames(NewFilename,AnUnitInfo.Filename)<>0))
  and FileExists(NewFilename) then begin
    ACaption:=lisOverwriteFile;
    AText:=Format(lisAFileAlreadyExistsReplaceIt, ['"', NewFilename, '"', #13]);
    Result:=QuestionDlg(ACaption, AText, mtConfirmation,
      [mrYes, lisOverwriteFileOnDisk, mbCancel], 0);
    if Result=mrCancel then exit;
  end;

  Result:=DoRenameUnit(AnUnitInfo,NewFilename,NewUnitName,ResourceCode);
end;

{ TLRTGrubber }
type
  TLRTGrubber = class(TObject)
  private
    FGrubbed: TStrings;
    FWriter: TWriter;
  public
    constructor Create(TheWriter: TWriter);
    destructor Destroy; override;
    procedure Grub(Sender: TObject; const Instance: TPersistent;
                   PropInfo: PPropInfo; var Content: string);
    property Grubbed: TStrings read FGrubbed;
    property Writer: TWriter read FWriter write FWriter;
  end;

constructor TLRTGrubber.Create(TheWriter: TWriter);
begin
  inherited Create;
  FGrubbed:=TStringList.Create;
  FWriter:=TheWriter;
  FWriter.OnWriteStringProperty:=@Grub;
end;

destructor TLRTGrubber.Destroy;
begin
  FGrubbed.Free;
  inherited Destroy;
end;

procedure TLRTGrubber.Grub(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo; var Content: string);
var
  LRSWriter: TLRSObjectWriter;
  Path: String;
begin
  if not Assigned(Instance) then exit;
  if not Assigned(PropInfo) then exit;
  if CompareText(PropInfo^.PropType^.Name,'TTRANSLATESTRING')<>0 then exit;
  Path:='';
  if Writer.Driver is TLRSObjectWriter then begin
    LRSWriter:=TLRSObjectWriter(Writer.Driver);
    Path:=LRSWriter.GetStackPath(Writer.Root);
  end else begin
    Path:=Instance.ClassName+'.'+PropInfo^.Name;
  end;

  FGrubbed.Add(Uppercase(Path)+'='+Content);
  //DebugLn(['TLRTGrubber.Grub "',FGrubbed[FGrubbed.Count-1],'"']);
end;

function TMainIDE.DoSaveUnitComponent(AnUnitInfo: TUnitInfo;
  ResourceCode, LFMCode: TCodeBuffer; Flags: TSaveFlags): TModalResult;
var
  ComponentSavingOk: boolean;
  MemStream, BinCompStream, TxtCompStream: TExtMemoryStream;
  DestroyDriver: Boolean;
  Writer: TWriter;
  ACaption, AText: string;
  CompResourceCode, LFMFilename, TestFilename, ResTestFilename: string;
  UnitSaveFilename: String;
  ADesigner: TDesigner;
  AncestorUnit: TUnitInfo;
  AncestorInstance: TComponent;
  Grubber: TLRTGrubber;
  LRTFilename: String;
begin
  Result:=mrCancel;

  // save lrs - lazarus resource file and lfm - lazarus form text file
  // Note: When there is a bug in the source, the include directive of the
  //       resource code can not be found, therefore the LFM file should always
  //       be saved first.
  //       And therefore each TUnitInfo stores the resource filename (.lrs).

  // the lfm file is saved before the lrs file, because the IDE only needs the
  // lfm file to recreate the lrs file.
  // by VVI - now a LRT file is saved in addition to LFM and LRS
  // LRT file format (in present) are lines
  // <ClassName>.<PropertyName>=<PropertyValue>

  if (AnUnitInfo.Component<>nil) then begin
    // stream component to resource code and to lfm file
    ComponentSavingOk:=true;

    // clean up component
    Result:=DoRemoveDanglingEvents(AnUnitInfo,true);
    if Result<>mrOk then exit;

    // save designer form properties to the component
    FormEditor1.SaveHiddenDesignerFormProperties(AnUnitInfo.Component);

    // stream component to binary stream
    BinCompStream:=TExtMemoryStream.Create;
    if AnUnitInfo.ComponentLastBinStreamSize>0 then
      BinCompStream.Capacity:=
                       AnUnitInfo.ComponentLastBinStreamSize+LRSStreamChunkSize;
    Writer:=nil;
    DestroyDriver:=false;
    Grubber:=nil;
    try
      Result:=mrOk;
      repeat
        try
          BinCompStream.Position:=0;
          Writer:=CreateLRSWriter(BinCompStream,DestroyDriver);
          //used to save lrt files
          if AnUnitInfo.Project.EnableI18N then begin
            Grubber:=TLRTGrubber.Create(Writer);
          end;
          {$IFDEF EnableFakeMethods}
          Writer.OnWriteMethodProperty:=@FormEditor1.WriteMethodPropertyEvent;
          {$ENDIF}
          AncestorUnit:=GetAncestorUnit(AnUnitInfo);
          if AncestorUnit<>nil then
            AncestorInstance:=AncestorUnit.Component
          else
            AncestorInstance:=nil;
          //DebugLn(['TMainIDE.DoSaveUnitComponent AncestorInstance=',dbgsName(AncestorInstance)]);
          Writer.WriteDescendent(AnUnitInfo.Component,AncestorInstance);
          if DestroyDriver then Writer.Driver.Free;
          FreeAndNil(Writer);
          AnUnitInfo.ComponentLastBinStreamSize:=BinCompStream.Size;
        except
          on E: Exception do begin
            DumpExceptionBackTrace;
            ACaption:=lisStreamingError;
            AText:=Format(lisUnableToStreamT, [AnUnitInfo.ComponentName,
                          AnUnitInfo.ComponentName])+#13
                          +E.Message;
            Result:=MessageDlg(ACaption, AText, mtError,
                       [mbAbort, mbRetry, mbIgnore], 0);
            if Result=mrAbort then exit;
            if Result=mrIgnore then Result:=mrOk;
            ComponentSavingOk:=false;
          end;
        end;
      until Result<>mrRetry;

      // create lazarus form resource code
      if ComponentSavingOk then begin
        if ResourceCode=nil then begin
          if (sfSaveToTestDir in Flags) then
            UnitSaveFilename:=MainBuildBoss.GetTestUnitFilename(AnUnitInfo)
          else
            UnitSaveFilename:=AnUnitInfo.Filename;
          ResTestFilename:=ChangeFileExt(UnitSaveFilename,ResourceFileExt);
          ResourceCode:=CodeToolBoss.CreateFile(ResTestFilename);
          ComponentSavingOk:=(ResourceCode<>nil);
        end;
        if ComponentSavingOk then begin
          // there is no bug in the source, so the resource code should be
          // changed too
          MemStream:=TExtMemoryStream.Create;
          if AnUnitInfo.ComponentLastLRSStreamSize>0 then
            MemStream.Capacity:=AnUnitInfo.ComponentLastLRSStreamSize+LRSStreamChunkSize;
          try
            BinCompStream.Position:=0;
            BinaryToLazarusResourceCode(BinCompStream,MemStream
              ,'T'+AnUnitInfo.ComponentName,'FORMDATA');
            AnUnitInfo.ComponentLastLRSStreamSize:=MemStream.Size;
            MemStream.Position:=0;
            SetLength(CompResourceCode,MemStream.Size);
            MemStream.Read(CompResourceCode[1],length(CompResourceCode));
          finally
            MemStream.Free;
          end;
        end;
        if ComponentSavingOk then begin
          {$IFDEF IDE_DEBUG}
          writeln('TMainIDE.SaveFileResources E ',CompResourceCode);
          {$ENDIF}
          // replace lazarus form resource code in include file (.lrs)
          if not (sfSaveToTestDir in Flags) then begin
            // if resource name has changed, delete old resource
            if (AnUnitInfo.ComponentName<>AnUnitInfo.ComponentResourceName)
            and (AnUnitInfo.ComponentResourceName<>'') then begin
              CodeToolBoss.RemoveLazarusResource(ResourceCode,
                                          'T'+AnUnitInfo.ComponentResourceName);
            end;
            // add comment to resource file (if not already exists)
            if (not CodeToolBoss.AddLazarusResourceHeaderComment(ResourceCode,
               lisResourceFileComment)) then
            begin
              ACaption:=lisResourceSaveError;
              AText:=Format(lisUnableToAddResourceHeaderCommentToResourceFile, [
                #13, '"', ResourceCode.FileName, '"', #13]);
              Result:=MessageDlg(ACaption,AText,mtError,[mbIgnore,mbAbort],0);
              if Result<>mrIgnore then exit;
            end;
            // add resource to resource file
            if (not CodeToolBoss.AddLazarusResource(ResourceCode,
               'T'+AnUnitInfo.ComponentName,CompResourceCode)) then
            begin
              ACaption:=lisResourceSaveError;
              AText:=Format(
                lisUnableToAddResourceTFORMDATAToResourceFileProbably, [
                AnUnitInfo.ComponentName,
                #13, '"', ResourceCode.FileName, '"', #13]
                );
              Result:=MessageDlg(ACaption, AText, mtError, [mbIgnore, mbAbort],0);
              if Result<>mrIgnore then exit;
            end else begin
              AnUnitInfo.ResourceFileName:=ResourceCode.Filename;
              AnUnitInfo.ComponentResourceName:=AnUnitInfo.ComponentName;
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
              Result:=QuestionDlg(lisUnableToCreateFile,
                Format(lisUnableToCreateFile2, ['"', LFMFilename, '"']),
                mtWarning, [mrIgnore, lisContinueWithoutLoadingForm,
                           mrCancel, lisCancelLoadingUnit,
                           mrAbort, lisAbortAllLoading], 0);
              if Result<>mrIgnore then exit;
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
                TxtCompStream:=TExtMemoryStream.Create;
                if AnUnitInfo.ComponentLastLFMStreamSize>0 then
                  TxtCompStream.Capacity:=AnUnitInfo.ComponentLastLFMStreamSize
                                          +LRSStreamChunkSize;
                try
                  BinCompStream.Position:=0;
                  LRSObjectBinaryToText(BinCompStream,TxtCompStream);
                  AnUnitInfo.ComponentLastLFMStreamSize:=TxtCompStream.Size;
                  // stream text to file
                  TxtCompStream.Position:=0;
                  LFMCode.LoadFromStream(TxtCompStream);
                  Result:=SaveCodeBufferToFile(LFMCode,LFMCode.Filename);
                  if not Result=mrOk then exit;
                  Result:=mrCancel;
                finally
                  TxtCompStream.Free;
                end;
              except
                on E: Exception do begin
                  // added to get more feedback on issue 7009
                  Debugln('TMainIDE.SaveFileResources E3: ', E.Message);
                  DumpExceptionBackTrace;
                  ACaption:=lisStreamingError;
                  AText:=Format(
                    lisUnableToTransformBinaryComponentStreamOfTIntoText, [
                    AnUnitInfo.ComponentName, AnUnitInfo.ComponentName])
                    +#13+E.Message;
                  Result:=MessageDlg(ACaption, AText, mtError,
                                     [mbAbort, mbRetry, mbIgnore], 0);
                  if Result=mrAbort then exit;
                  if Result=mrIgnore then Result:=mrOk;
                end;
              end;
            until Result<>mrRetry;
          end;
        end;
      end;
      // Now the most important file (.lfm) is saved.
      // Now save the secondary files

      // save the .lrt file containing the list of all translatable strings of
      // the component
      if ComponentSavingOk
      and (Grubber<>nil) and (Grubber.Grubbed.Count>0)
      and not (sfSaveToTestDir in Flags) then begin
        LRTFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lrt');
        DebugLn(['TMainIDE.DoSaveUnitComponent save lrt: ',LRTFilename]);
        Result:=SaveStringToFile(LRTFilename,Grubber.Grubbed.Text,
                                 [mbIgnore,mbAbort],AnUnitInfo.Filename);
        if (Result<>mrOk) and (Result<>mrIgnore) then exit;
      end;

    finally
      try
        BinCompStream.Free;
        if DestroyDriver and (Writer<>nil) then Writer.Driver.Free;
        Writer.Free;
        Grubber.Free;
      except
        on E: Exception do begin
          debugln('TMainIDE.SaveFileResources Error cleaning up: ',E.Message);
        end;
      end;
    end;
  end;
  {$IFDEF IDE_DEBUG}
  if ResourceCode<>nil then
    writeln('TMainIDE.SaveFileResources F ',ResourceCode.Modified);
  {$ENDIF}
  // save binary stream (.lrs)
  if ResourceCode<>nil then begin
    if not (sfSaveToTestDir in Flags) then begin
      if (ResourceCode.Modified) then begin
        Result:=SaveCodeBufferToFile(ResourceCode,ResourceCode.Filename);
        if not Result=mrOk then exit;
      end;
    end else begin
      TestFilename:=MainBuildBoss.GetTestUnitFilename(AnUnitInfo);
      Result:=SaveCodeBufferToFile(ResourceCode,
                 ChangeFileExt(TestFilename,
                               ExtractFileExt(ResourceCode.Filename)));
      if not Result=mrOk then exit;
    end;
  end;
  // mark designer unmodified
  ADesigner:=FindRootDesigner(AnUnitInfo.Component) as TDesigner;
  if ADesigner<>nil then
    ADesigner.DefaultFormBoundsValid:=false;

  Result:=mrOk;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.SaveFileResources G ',LFMCode<>nil);
  {$ENDIF}
end;

function TMainIDE.DoSaveUnitComponentToBinStream(AnUnitInfo: TUnitInfo;
  var BinCompStream: TExtMemoryStream): TModalResult;
var
  Writer: TWriter;
  DestroyDriver: Boolean;
begin
  // save designer form properties to the component
  FormEditor1.SaveHiddenDesignerFormProperties(AnUnitInfo.Component);

  // stream component to binary stream
  if BinCompStream=nil then
    BinCompStream:=TExtMemoryStream.Create;
  if AnUnitInfo.ComponentLastBinStreamSize>0 then
    BinCompStream.Capacity:=Max(BinCompStream.Capacity,BinCompStream.Position+
                      AnUnitInfo.ComponentLastBinStreamSize+LRSStreamChunkSize);
  Writer:=nil;
  DestroyDriver:=false;
  try
    Result:=mrOk;
    try
      BinCompStream.Position:=0;
      Writer:=CreateLRSWriter(BinCompStream,DestroyDriver);
      Writer.WriteDescendent(AnUnitInfo.Component,nil);
      if DestroyDriver then Writer.Driver.Free;
      FreeAndNil(Writer);
      AnUnitInfo.ComponentLastBinStreamSize:=BinCompStream.Size;
    except
      on E: Exception do begin
        DumpExceptionBackTrace;
        Result:=MessageDlg(lisStreamingError,
            Format(lisUnableToStreamT, [AnUnitInfo.ComponentName,
                              AnUnitInfo.ComponentName])+#13
                              +E.Message,
            mtError,[mbAbort, mbRetry, mbIgnore], 0);
        if Result=mrAbort then exit;
        if Result=mrIgnore then Result:=mrOk;
      end;
    end;
  finally
    try
      if DestroyDriver and (Writer<>nil) then Writer.Driver.Free;
      Writer.Free;
    except
      on E: Exception do begin
        debugln('TMainIDE.DoSaveFileResourceToBinStream Error cleaning up: ',E.Message);
      end;
    end;
  end;
end;

function TMainIDE.DoRemoveDanglingEvents(AnUnitInfo: TUnitInfo;
  OkOnCodeErrors: boolean): TModalResult;
var
  ComponentModified: boolean;
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  Result:=mrOk;
  if (AnUnitInfo.Component=nil) then exit;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  // unselect methods in ObjectInspector1
  if (ObjectInspector1.PropertyEditorHook.LookupRoot=AnUnitInfo.Component) then
  begin
    ObjectInspector1.EventGrid.ItemIndex:=-1;
    ObjectInspector1.FavouriteGrid.ItemIndex:=-1;
  end;
  //debugln('TMainIDE.DoRemoveDanglingEvents ',AnUnitInfo.Filename,' ',dbgsName(AnUnitInfo.Component));
  // remove dangling methods
  Result:=RemoveDanglingEvents(AnUnitInfo.Component,AnUnitInfo.Source,true,
                               ComponentModified);
  // update ObjectInspector1
  if ComponentModified
  and (ObjectInspector1.PropertyEditorHook.LookupRoot=AnUnitInfo.Component) then
  begin
    ObjectInspector1.EventGrid.RefreshPropertyValues;
    ObjectInspector1.FavouriteGrid.RefreshPropertyValues;
  end;
end;

function TMainIDE.DoRenameUnit(AnUnitInfo: TUnitInfo;
  NewFilename, NewUnitName: string;
  var ResourceCode: TCodeBuffer): TModalresult;
var
  NewLFMFilename: String;
  OldSourceCode: String;
  NewSource: TCodeBuffer;
  NewFilePath: String;
  NewResFilePath: String;
  OldFilePath: String;
  OldResFilePath: String;
  SrcEdit: TSourceEditor;
  OldFilename: String;
  NewResFilename: String;
  NewHighlighter: TLazSyntaxHighlighter;
  AmbiguousFiles: TStringList;
  AmbiguousText: string;
  i: Integer;
  AmbiguousFilename: String;
  OldUnitPath: String;
begin
  OldFilename:=AnUnitInfo.Filename;
  OldFilePath:=ExtractFilePath(OldFilename);
  SrcEdit:=GetSourceEditorForUnitInfo(AnUnitInfo);
  if NewUnitName='' then
    NewUnitName:=AnUnitInfo.UnitName;
  //debugln('TMainIDE.DoRenameUnit ',AnUnitInfo.Filename,' NewUnitName=',NewUnitName,' OldUnitName=',AnUnitInfo.UnitName);

  // check new resource file
  if AnUnitInfo.ComponentName='' then begin
    // unit has no component
    // -> remove lfm file, so that it will not be auto loaded on next open
    NewLFMFilename:=ChangeFileExt(NewFilename,'.lfm');
    if (FileExists(NewLFMFilename))
    and (not DeleteFile(NewLFMFilename))
    and (MessageDlg(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed, [
      '"', NewLFMFilename, '"']), mtError, [mbIgnore, mbCancel], 0)=mrCancel)
      then
    begin
      Result:=mrCancel;
      exit;
    end;
  end;

  // check new resource file
  if AnUnitInfo.ComponentName='' then begin
    // unit has no component
    // -> remove lfm file, so that it will not be auto loaded on next open
    NewLFMFilename:=ChangeFileExt(NewFilename,'.lfm');
    if (FileExists(NewLFMFilename))
    and (not DeleteFile(NewLFMFilename))
    and (MessageDlg(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed, [
      '"', NewLFMFilename, '"']), mtError, [mbIgnore, mbCancel], 0)=mrCancel)
      then
    begin
      Result:=mrCancel;
      exit;
    end;
  end;

  // create new source with the new filename
  OldSourceCode:=AnUnitInfo.Source.Source;
  NewSource:=CodeToolBoss.CreateFile(NewFilename);
  NewSource.Source:=OldSourceCode;
  if NewSource=nil then begin
    Result:=MessageDlg(lisUnableToCreateFile,
      Format(lisCanNotCreateFile, ['"', NewFilename, '"']),
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  // get final filename
  NewFilename:=NewSource.Filename;
  NewFilePath:=ExtractFilePath(NewFilename);
  EnvironmentOptions.AddToRecentOpenFiles(NewFilename);
  SetRecentFilesMenu;

  // add new path to unit path
  if AnUnitInfo.IsPartOfProject
  and (not Project1.IsVirtual)
  and (FilenameIsPascalUnit(NewFilename))
  and (CompareFilenames(NewFilePath,Project1.ProjectDirectory)<>0) then begin
    OldUnitPath:=Project1.CompilerOptions.GetUnitPath(false);

    if SearchDirectoryInSearchPath(OldUnitPath,NewFilePath,1)<1 then begin
      //DebugLn('TMainIDE.DoRenameUnit NewFilePath="',NewFilePath,'" OldUnitPath="',OldUnitPath,'"');
      if MessageDlg(lisExtendUnitPath,
        Format(lisTheDirectoryIsNotYetInTheUnitPathAddIt, ['"', NewFilePath,
          '"', #13]),
        mtConfirmation,[mbYes,mbNo],0)=mrYes then
      begin
        Project1.CompilerOptions.OtherUnitFiles:=
                        Project1.CompilerOptions.OtherUnitFiles+';'+NewFilePath;
      end;
    end;
  end;

  // rename Resource file
  if (ResourceCode<>nil) then begin
    // the resource include line in the code will be changed later after
    // changing the unitname
    OldResFilePath:=ExtractFilePath(ResourceCode.Filename);
    NewResFilePath:=OldResFilePath;
    if FilenameIsAbsolute(OldFilePath)
    and FileIsInPath(OldResFilePath,OldFilePath) then begin
      // resource code was in the same or in a sub directory of source
      // -> try to keep this relationship
      NewResFilePath:=NewFilePath
                       +copy(ResourceCode.Filename,length(OldFilePath)+1,
                         length(ResourceCode.Filename));
      if not DirPathExists(NewResFilePath) then
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
    if (AnUnitInfo.Component<>nil) then
      FormEditor1.RenameJITComponentUnitname(AnUnitInfo.Component,NewUnitName);

    {$IFDEF IDE_DEBUG}
    writeln('TMainIDE.DoRenameUnit C ',ResourceCode<>nil);
    writeln('   NewResFilePath="',NewResFilePath,'" NewResFilename="',NewResFilename,'"');
    if ResourceCode<>nil then writeln('*** ResourceFileName ',ResourceCode.Filename);
    if AnUnitInfo.Component<>nil then writeln('*** AnUnitInfo.Component ',dbgsName(AnUnitInfo.Component),' ClassUnitname=',GetClassUnitName(AnUnitInfo.Component.ClassType));
    {$ENDIF}
  end else begin
    NewResFilename:='';
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.DoRenameUnit D ',ResourceCode<>nil);
  {$ENDIF}

  // set new codebuffer in unitinfo and sourceeditor
  AnUnitInfo.Source:=NewSource;
  AnUnitInfo.ClearModifieds;
  if SrcEdit<>nil then
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
  if SrcEdit<>nil then
    UpdateSourceNames;

  // change syntax highlighter
  if not AnUnitInfo.CustomHighlighter then begin
    NewHighlighter:=
      ExtensionToLazSyntaxHighlighter(ExtractFileExt(NewFilename));
    if NewHighlighter<>AnUnitInfo.SyntaxHighlighter then begin
      AnUnitInfo.SyntaxHighlighter:=NewHighlighter;
      if SrcEdit<>nil then
        SrcEdit.SyntaxHighlighterType:=AnUnitInfo.SyntaxHighlighter;
    end;
  end;

  // save file
  Result:=SaveCodeBufferToFile(NewSource,NewSource.Filename);
  if Result<>mrOk then exit;

  // change packages containing the file
  Result:=PkgBoss.OnRenameFile(OldFilename,AnUnitInfo.Filename,
                               AnUnitInfo.IsPartOfProject);
  if Result=mrAbort then exit;

  // delete ambiguous files
  NewFilePath:=ExtractFilePath(NewFilename);
  AmbiguousFiles:=
    FindFilesCaseInsensitive(NewFilePath,ExtractFilename(NewFilename),true);
  if AmbiguousFiles<>nil then begin
    try
      if (AmbiguousFiles.Count=1)
      and (CompareFilenames(OldFilePath,NewFilePath)=0)
      and (CompareFilenames(AmbiguousFiles[0],ExtractFilename(OldFilename))=0)
      then
        AmbiguousText:=Format(lisDeleteOldFile, ['"', ExtractFilename(
          OldFilename), '"'])
      else
        AmbiguousText:=
          Format(lisThereAreOtherFilesInTheDirectoryWithTheSameName,
                 [#13, #13, AmbiguousFiles.Text, #13]);
      Result:=MessageDlg(lisAmbiguousFilesFound, AmbiguousText,
        mtWarning,[mbYes,mbNo,mbAbort],0);
      if Result=mrAbort then exit;
      if Result=mrYes then begin
        NewFilePath:=AppendPathDelim(ExtractFilePath(NewFilename));
        for i:=0 to AmbiguousFiles.Count-1 do begin
          AmbiguousFilename:=NewFilePath+AmbiguousFiles[i];
          if (FileExists(AmbiguousFilename))
          and (not DeleteFile(AmbiguousFilename))
          and (MessageDlg(lisPkgMangDeleteFailed, Format(lisDeletingOfFileFailed,
            ['"', AmbiguousFilename, '"']), mtError, [mbIgnore, mbCancel], 0)=
            mrCancel) then
          begin
            Result:=mrCancel;
            exit;
          end;
        end;
      end;
    finally
      AmbiguousFiles.Free;
    end;
  end;

  // remove old path from unit path
  if AnUnitInfo.IsPartOfProject
  and (FilenameIsPascalUnit(OldFilename))
  and (OldFilePath<>'') then begin
    //DebugLn('TMainIDE.DoRenameUnit OldFilePath="',OldFilePath,'" SourceDirs="',Project1.SourceDirectories.CreateSearchPathFromAllFiles,'"');
    if (SearchDirectoryInSearchPath(
       Project1.SourceDirectories.CreateSearchPathFromAllFiles,OldFilePath,1)<1)
    then begin
      //DebugLn('TMainIDE.DoRenameUnit OldFilePath="',OldFilePath,'" UnitPath="',Project1.CompilerOptions.GetUnitPath(false),'"');
      if (SearchDirectoryInSearchPath(
                   Project1.CompilerOptions.GetUnitPath(false),OldFilePath,1)<1)
      then begin
        if MessageDlg(lisCleanUpUnitPath,
            Format(lisTheDirectoryIsNoLongerNeededInTheUnitPathRemoveIt, ['"',
              OldFilePath, '"', #13]),
            mtConfirmation,[mbYes,mbNo],0)=mrYes then
        begin
          Project1.CompilerOptions.OtherUnitFiles:=
                      RemoveSearchPaths(Project1.CompilerOptions.OtherUnitFiles,
                                        OldUnitPath);
        end;
      end;
    end;
  end;

  Result:=mrOk;
end;

function TMainIDE.DoOpenNotExistingFile(const AFileName: string;
  Flags: TOpenFlags): TModalResult;
var
  NewFlags: TNewFlags;
begin
  if ofProjectLoading in Flags then begin
    // this is a file, that was loaded last time, but was removed from disk
    Result:=QuestionDlg(lisFileNotFound,
      Format(lisTheFileWasNotFoundIgnoreWillGoOnLoadingTheProject, ['"',
        AFilename, '"', #13, #13, #13]),
      mtError, [mrIgnore, lisSkipFileAndContinueLoading,
                mrAbort, lisAbortLoadingProject],
      0);
    exit;
  end;

  // Default to cancel
  Result:=mrCancel;
  if ofQuiet in Flags then Exit;

  if ofOnlyIfExists in Flags
  then begin
    MessageDlg(lisFileNotFound, Format(lisFileNotFound2, ['"', AFilename, '"',
      #13]),
               mtInformation,[mbCancel],0);
    // cancel loading file
    Exit;
  end;

  if MessageDlg(lisFileNotFound,
    Format(lisFileNotFoundDoYouWantToCreateIt, ['"', AFilename, '"', #13, #13])
    ,mtInformation,[mbYes,mbNo],0)=mrYes then
  begin
    // create new file
    NewFlags:=[nfOpenInEditor,nfCreateDefaultSrc];
    if ofAddToProject in Flags then
      Include(NewFlags,nfIsPartOfProject);
    if FilenameIsPascalSource(AFilename) then
      Result:=DoNewEditorFile(FileDescriptorUnit,AFilename,'',NewFlags)
    else
      Result:=DoNewEditorFile(FileDescriptorText,AFilename,'',NewFlags);
  end;
end;

function TMainIDE.DoOpenUnknownFile(const AFileName: string; Flags: TOpenFlags;
  var NewUnitInfo: TUnitInfo; var Handled: boolean): TModalResult;
var
  Ext, NewProgramName, LPIFilename, ACaption, AText: string;
  PreReadBuf: TCodeBuffer;
  LoadFlags: TLoadBufferFlags;
begin
  Handled:=false;
  Ext:=lowercase(ExtractFileExt(AFilename));

  if ([ofProjectLoading,ofRegularFile]*Flags=[]) and (ToolStatus=itNone)
  and (Ext='.lpi') then begin
    // this is a project info file -> load whole project
    Result:=DoOpenProjectFile(AFilename,[ofAddToRecent]);
    Handled:=true;
    exit;
  end;

  // load the source
  LoadFlags := [lbfCheckIfText,lbfUpdateFromDisk,lbfRevert];
  if ofQuiet in Flags then Include(LoadFlags, lbfQuiet);
  Result:=LoadCodeBuffer(PreReadBuf,AFileName,LoadFlags);
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
        if QuestionDlg(lisProjectInfoFileDetected,
          Format(lisTheFileSeemsToBeTheProgramFileOfAnExistingLazarusP, [
            AFilename]), mtConfirmation,
            [mrOk, lisOpenProject2, mrCancel, lisOpenTheFileAsNormalSource], 0)
          =mrOk then
        begin
          Result:=DoOpenProjectFile(LPIFilename,[]);
          Handled:=true;
          exit;
        end;
      end else begin
        AText:=Format(lisTheFileSeemsToBeAProgramCloseCurrentProject, ['"',
          AFilename, '"', #13, #13]);
        ACaption:=lisProgramDetected;
        if MessageDlg(ACaption, AText, mtConfirmation,
            [mbYes, mbNo], 0)=mrYes then
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
    NewUnitInfo.ReadUnitNameFromSource(true);
  Project1.AddFile(NewUnitInfo,false);
  if (ofAddToProject in Flags) and (not NewUnitInfo.IsPartOfProject) then
  begin
    NewUnitInfo.IsPartOfProject:=true;
    Project1.Modified:=true;
  end;

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
      //writeln('TMainIDE.DoRestoreBookMarks ',BookmarkID,' ',
      //   Project1.Bookmarks[i].CursorPos.X,' ',Project1.Bookmarks[i].CursorPos.Y);
      ASrcEdit.EditorComponent.SetBookmark(BookmarkID,
         Project1.Bookmarks[i].CursorPos.X,Project1.Bookmarks[i].CursorPos.Y);
    end;
  end;
end;

function TMainIDE.DoLoadLFM(AnUnitInfo: TUnitInfo;
  OpenFlags: TOpenFlags; CloseFlags: TCloseFlags): TModalResult;
// if there is a .lfm file, open the resource
var
  LFMFilename: string;
  LFMBuf: TCodeBuffer;
begin
  Result:=CloseUnitComponent(AnUnitInfo,CloseFlags);
  if Result<>mrOk then begin
    DebugLn(['TMainIDE.DoLoadLFM failed due to CloseUnitComponent for file ',AnUnitInfo.Filename]);
    exit;
  end;

  // Note: think about virtual and normal .lfm files.
  LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
  LFMBuf:=nil;
  if not FileExistsInIDE(LFMFilename,[pfsfOnlyEditorFiles]) then begin
    // there is no LFM file -> ok
    debugln('TMainIDE.DoLoadLFM there is no LFM file for "',AnUnitInfo.Filename,'"');
    Result:=mrOk;
    exit;
  end;

  // there is a lazarus form text file -> load it
  Result:=LoadIDECodeBuffer(LFMBuf,LFMFilename,[lbfUpdateFromDisk]);
  if Result<>mrOk then exit;

  Result:=DoLoadLFM(AnUnitInfo,LFMBuf,OpenFlags,
                    CloseFlags-[cfSaveFirst,cfSaveDependencies]);
end;

function TMainIDE.DoLoadLFM(AnUnitInfo: TUnitInfo; LFMBuf: TCodeBuffer;
  OpenFlags: TOpenFlags; CloseFlags: TCloseFlags
  ): TModalResult;
const
  BufSize = 4096; // allocating mem in 4k chunks helps many mem managers
var
  TxtLFMStream, BinStream, AncestorBinStream: TExtMemoryStream;
  NewComponent: TComponent;
  AncestorType: TComponentClass;
  DesignerForm: TCustomForm;
  NewClassName: String;
  LFMType: String;
  AncestorClassName: String;
  ACaption, AText: String;
  NewUnitName: String;
  AncestorUnitInfo: TUnitInfo;
begin
  debugln('TMainIDE.DoLoadLFM A ',AnUnitInfo.Filename,' IsPartOfProject=',dbgs(AnUnitInfo.IsPartOfProject),' ');

  // close old designer form
  Result:=CloseUnitComponent(AnUnitInfo,CloseFlags);
  if Result<>mrOk then exit;

  // check installed packages
  if (AnUnitInfo.Component=nil) and AnUnitInfo.IsPartOfProject
  and (not (ofProjectLoading in OpenFlags)) then begin
    // opening a form of the project -> check installed packages
    Result:=PkgBoss.CheckProjectHasInstalledPackages(Project1);
    if not (Result in [mrOk,mrIgnore]) then exit;
  end;

  //debugln('TMainIDE.DoLoadLFM LFM file loaded, parsing "',LFMBuf.Filename,'" ...');

  if not AnUnitInfo.HasResources then begin
    // someone created a .lfm file -> Update HasResources
    AnUnitInfo.HasResources:=true;
  end;

  //debugln('TMainIDE.DoLoadLFM LFM="',LFMBuf.Source,'"');

  if AnUnitInfo.Component=nil then begin
    // load/create new instance
  
    // find the classname of the LFM, and check for inherited form
    ReadLFMHeader(LFMBuf.Source,NewClassName,LFMType);
    if (NewClassName='') or (LFMType='') then begin
      Result:=MessageDlg(lisLFMFileCorrupt,
        Format(lisUnableToFindAValidClassnameIn, ['"', LFMBuf.Filename, '"']),
        mtError,[mbIgnore,mbCancel,mbAbort],0);
      exit;
    end;

    BinStream:=nil;
    AncestorBinStream:=nil;
    try
      // find the ancestor type in the source
      AncestorClassName:='';
      AncestorType:=nil;
      AncestorUnitInfo:=nil;
      if not CodeToolBoss.FindFormAncestor(AnUnitInfo.Source,NewClassName,
                                           AncestorClassName,true)
      then begin
        DebugLn('TMainIDE.DoLoadLFM Filename="',AnUnitInfo.Filename,'" NewClassName=',NewClassName,'. Unable to find ancestor class: ',CodeToolBoss.ErrorMessage);
      end;
      if AncestorClassName<>'' then begin
        if CompareText(AncestorClassName,'TForm')=0 then begin
          AncestorType:=TForm;
        end else if CompareText(AncestorClassName,'TDataModule')=0 then begin
          // use our TDataModule
          // (some fpc versions have non designable TDataModule)
          AncestorType:=TDataModule;
        end else if CompareText(AncestorClassName,'TCustomForm')=0 then begin
          MessageDlg(lisCodeTemplError, Format(
            lisTheResourceClassDescendsFromProbablyThisIsATypoFor, ['"',
            NewClassName, '"', '"', AncestorClassName, '"']),
            mtError,[mbCancel],0);
          Result:=mrCancel;
          exit;
        end else if CompareText(AncestorClassName,'TComponent')=0 then begin
          MessageDlg(lisCodeTemplError, Format(
            lisUnableToOpenDesignerTheClassDoesNotDescendFromADes, [#13,
            NewClassName]),
            mtError,[mbCancel],0);
          Result:=mrCancel;
          exit;
        end;
      end else begin
        AncestorType:=TForm;
      end;

      // try loading the ancestor first (unit, lfm and component instance)
      if (AncestorType=nil) then begin
        Result:=DoLoadComponentDependencyHidden(AnUnitInfo,AncestorClassName,
                                       OpenFlags,AncestorType,AncestorUnitInfo);
        if Result=mrAbort then exit;
        case  Result of
        mrAbort: exit;
        mrOk:
          if AncestorUnitInfo<>nil then begin
            Result:=DoSaveUnitComponentToBinStream(AncestorUnitInfo,
                                                   AncestorBinStream);
            if Result<>mrOk then exit;
            AncestorBinStream.Position:=0;
          end;
        mrIgnore:
          begin
            // use TForm as default
            AncestorType:=TForm;
            AncestorUnitInfo:=nil;
          end;
        else
          // cancel
          Result:=mrCancel;
          exit;
        end;
      end;

      // use TForm as default ancestor
      if AncestorType=nil then
        AncestorType:=TForm;
      //DebugLn('TMainIDE.DoLoadLFM Filename="',AnUnitInfo.Filename,'" AncestorClassName=',AncestorClassName,' AncestorType=',AncestorType.ClassName);

      BinStream:=TExtMemoryStream.Create;
      TxtLFMStream:=TExtMemoryStream.Create;
      try
        LFMBuf.SaveToStream(TxtLFMStream);
        AnUnitInfo.ComponentLastLFMStreamSize:=TxtLFMStream.Size;
        TxtLFMStream.Position:=0;

        // convert text to binary format
        try
          if AnUnitInfo.ComponentLastBinStreamSize>0 then
            BinStream.Capacity:=AnUnitInfo.ComponentLastBinStreamSize+BufSize;
          LRSObjectTextToBinary(TxtLFMStream,BinStream);
          AnUnitInfo.ComponentLastBinStreamSize:=BinStream.Size;
          BinStream.Position:=0;
          Result:=mrOk;
        except
          on E: Exception do begin
            DumpExceptionBackTrace;
            ACaption:=lisFormatError;
            AText:=Format(lisUnableToConvertTextFormDataOfFileIntoBinaryStream,
              [#13, '"', LFMBuf.Filename, '"', #13, E.Message]);
            Result:=MessageDlg(ACaption, AText, mtError, [mbOk, mbCancel], 0);
            if Result=mrCancel then Result:=mrAbort;
            exit;
          end;
        end;
      finally
        TxtLFMStream.Free;
      end;
      if ([ofProjectLoading,ofLoadHiddenResource]*OpenFlags=[]) then
        FormEditor1.ClearSelection;

      // create JIT component
      NewUnitName:=AnUnitInfo.UnitName;
      if NewUnitName='' then
        NewUnitName:=ExtractFileNameOnly(AnUnitInfo.Filename);
      NewComponent:=FormEditor1.CreateRawComponentFromStream(BinStream,
                   AncestorType,AncestorBinStream,copy(NewUnitName,1,255),true);
      AnUnitInfo.Component:=NewComponent;
      if (AncestorUnitInfo<>nil) then
        AnUnitInfo.AddRequiresComponentDependency(AncestorUnitInfo);
      if NewComponent=nil then begin
        // error streaming component -> examine lfm file
        DebugLn('ERROR: streaming failed lfm="',LFMBuf.Filename,'"');
        // open lfm file in editor
        Result:=DoOpenEditorFile(LFMBuf.Filename,AnUnitInfo.EditorIndex+1,
          OpenFlags+[ofOnlyIfExists,ofQuiet,ofRegularFile]);
        if Result<>mrOk then exit;
        Result:=DoCheckLFMInEditor;
        if Result=mrOk then Result:=mrCancel;
        exit;
      end;
    finally
      BinStream.Free;
      AncestorBinStream.Free;
    end;
  end else begin
    // keep old instance, just add a designer
    DebugLn(['TMainIDE.DoLoadLFM Creating designer for hidden component of ',AnUnitInfo.Filename]);
  end;

  NewComponent:=AnUnitInfo.Component;
  // create the designer
  if ([ofProjectLoading,ofLoadHiddenResource]*OpenFlags=[]) then
    FormEditor1.ClearSelection;
  FormEditor1.CreateComponentInterface(NewComponent,true);
  DebugLn('SUCCESS: streaming lfm="',LFMBuf.Filename,'"');
  AnUnitInfo.ComponentName:=NewComponent.Name;
  AnUnitInfo.ComponentResourceName:=AnUnitInfo.ComponentName;
  DesignerForm:=nil;
  if not (ofLoadHiddenResource in OpenFlags) then begin
    CreateDesignerForComponent(NewComponent);
    DesignerForm:=FormEditor1.GetDesignerForm(NewComponent);
  end;

  // select the new form (object inspector, formeditor, control selection)
  if ([ofProjectLoading,ofLoadHiddenResource]*OpenFlags=[]) then begin
    FDisplayState:=dsForm;
    GlobalDesignHook.LookupRoot:=NewComponent;
    TheControlSelection.AssignPersistent(NewComponent);
  end;

  // show new form
  if DesignerForm<>nil then begin
    DesignerForm.ControlStyle:=DesignerForm.ControlStyle-[csNoDesignVisible];
    if NewComponent is TControl then
      TControl(NewComponent).ControlStyle:=
                        TControl(NewComponent).ControlStyle-[csNoDesignVisible];
    LCLIntf.ShowWindow(DesignerForm.Handle,SW_SHOWNORMAL);
    FLastFormActivated:=DesignerForm;
  end;

  {$IFDEF IDE_DEBUG}
  debugln('[TMainIDE.DoLoadLFM] LFM end');
  {$ENDIF}
  Result:=mrOk;
end;

function TMainIDE.DoLoadComponentDependencyHidden(AnUnitInfo: TUnitInfo;
  const AComponentClassName: string; Flags: TOpenFlags;
  var AComponentClass: TComponentClass; var ComponentUnitInfo: TUnitInfo
  ): TModalResult;
var
  CTErrorMsg: string;
  CTErrorCode: TCodeBuffer;
  CTErrorLine: LongInt;
  CTErrorCol: LongInt;

  function FindClassInUnit(UnitCode: TCodeBuffer;
    out TheModalResult: TModalResult;
    var LFMCode: TCodeBuffer;
    var ClassFound: boolean): boolean;
  var
    AncestorClassName: String;
    UsedFilename: String;
    UsingFilename: String;
    LFMFilename: String;
    AComponentName: String;
  begin
    Result:=false;
    TheModalResult:=mrCancel;
    LFMCode:=nil;
    ClassFound:=false;
    
    AncestorClassName:='';
    if not CodeToolBoss.FindFormAncestor(UnitCode,AComponentClassName,
      AncestorClassName,true) then
    begin
      if CodeToolBoss.ErrorMessage<>'' then begin
        CTErrorMsg:=CodeToolBoss.ErrorMessage;
        CTErrorCode:=CodeToolBoss.ErrorCode;
        CTErrorLine:=CodeToolBoss.ErrorLine;
        CTErrorCol:=CodeToolBoss.ErrorColumn;
      end;
      exit;
    end;

    // this unit contains the class
    ClassFound:=true;
    LFMFilename:=ChangeFileExt(UnitCode.Filename,'.lfm');
    if FileExists(LFMFilename) then begin
      UsingFilename:=AnUnitInfo.Filename;
      Project1.ShortenFilename(UsingFilename);
      UsedFilename:=UnitCode.Filename;
      Project1.ShortenFilename(UsedFilename);
      TheModalResult:=QuestionDlg(lisCodeTemplError,
        Format(lisClassConflictsWithLfmFileTheUnitUsesTheTheUnitWhic, [#13,
          UsingFilename, #13, UsedFilename, #13, AComponentClassName, #13, #13,
          #13, AComponentClassName]),
        mtError,
          [mrCancel, lisCancelLoadingThisComponent,
           mrAbort, lisAbortWholeLoading,
           mrIgnore, lisIgnoreUseTFormAsAncestor], 0);
      exit;
    end;
    // there is no .lfm file

    // create a dummy lfm file
    LFMCode:=CodeToolBoss.CreateFile(LFMFilename);
    if LFMCode=nil then begin
      debugln('TMainIDE.DoLoadComponentDependencyHidden Failed creating dummy lfm ',LFMFilename);
      exit;
    end;
    AComponentName:=AComponentClassName;
    if AComponentName[1] in ['T','t'] then
      AComponentName:=copy(AComponentName,2,length(AComponentName));
    LFMCode.Source:=
      'inherited '+AComponentName+': '+AComponentClassName+LineEnding
      +'end';
      
    Result:=true;
    TheModalResult:=mrOk;
  end;

  function TryUnit(const UnitFilename: string; out TheModalResult: TModalResult;
    TryWithoutLFM: boolean): boolean;
  // returns true if the unit contains the component class and sets
  // TheModalResult to the result of the loading
  var
    LFMFilename: String;
    LFMCode: TCodeBuffer;
    LFMClassName: string;
    LFMType: string;
    CurUnitInfo: TUnitInfo;
    UnitCode: TCodeBuffer;
  begin
    Result:=false;
    TheModalResult:=mrCancel;
    
    CurUnitInfo:=Project1.UnitInfoWithFilename(UnitFilename);
    if (CurUnitInfo<>nil) and (CurUnitInfo.Component<>nil) then
    begin
      // unit with loaded component found -> check if it is the right one
      //DebugLn(['TMainIDE.DoLoadComponentDependencyHidden unit with a component found CurUnitInfo=',CurUnitInfo.Filename,' ',dbgsName(CurUnitInfo.Component)]);
      if CompareText(CurUnitInfo.Component.ClassName,AComponentClassName)=0
      then begin
        // component found (it was already loaded)
        ComponentUnitInfo:=CurUnitInfo;
        AComponentClass:=TComponentClass(ComponentUnitInfo.Component.ClassType);
        Result:=true;
        TheModalResult:=mrOk;
      end else begin
        // this unit does not have this component
      end;
      exit;
    end;
    
    if not TryWithoutLFM then begin
      LFMFilename:=ChangeFileExt(UnitFilename,'.lfm');
      if FileExists(LFMFilename) then begin
        // load the lfm file
        TheModalResult:=LoadCodeBuffer(LFMCode,LFMFilename,[lbfCheckIfText]);
        if TheModalResult<>mrOk then begin
          debugln('TMainIDE.DoLoadComponentDependencyHidden Failed loading ',LFMFilename);
          exit;
        end;
        // read the LFM classname
        ReadLFMHeader(LFMCode.Source,LFMClassName,LFMType);
        if LFMType='' then ;
        if CompareText(LFMClassName,AComponentClassName)<>0 then exit;
        
        // .lfm found
        Result:=true;
      end else if not TryWithoutLFM then begin
        // unit has no .lfm
        exit;
      end;
    end;

    //debugln('TMainIDE.DoLoadComponentDependencyHidden ',AnUnitInfo.Filename,' Loading ancestor unit ',UnitFilename);
    // load unit source
    TheModalResult:=LoadCodeBuffer(UnitCode,UnitFilename,[lbfCheckIfText]);
    if TheModalResult<>mrOk then begin
      debugln('TMainIDE.DoLoadComponentDependencyHidden Failed loading ',UnitFilename);
      exit;
    end;
    
    if TryWithoutLFM then begin
      if not FindClassInUnit(UnitCode,TheModalResult,LFMCode,Result) then exit;
    end;
    
    // create unit info
    if CurUnitInfo=nil then begin
      CurUnitInfo:=TUnitInfo.Create(UnitCode);
      CurUnitInfo.ReadUnitNameFromSource(true);
      Project1.AddFile(CurUnitInfo,false);
    end;
    
    // load resource hidden
    TheModalResult:=DoLoadLFM(CurUnitInfo,LFMCode,
                              Flags+[ofLoadHiddenResource],[]);
    if (TheModalResult=mrOk) then begin
      ComponentUnitInfo:=CurUnitInfo;
      AComponentClass:=TComponentClass(ComponentUnitInfo.Component.ClassType);
      debugln('TMainIDE.DoLoadComponentDependencyHidden Wanted=',AComponentClassName,' Class=',AComponentClass.ClassName);
      TheModalResult:=mrOk;
    end else begin
      debugln('TMainIDE.DoLoadComponentDependencyHidden Failed to load component ',AComponentClassName);
      if TheModalResult<>mrAbort then
        TheModalResult:=mrCancel;
    end;
  end;
  
  function TryRegisteredClasses(out TheModalResult: TModalResult): boolean;
  begin
    Result:=false;
    AComponentClass:=
                   FormEditor1.FindDesignerBaseClassByName(AComponentClassName);
    if AComponentClass<>nil then begin
      DebugLn(['TMainIDE.DoLoadComponentDependencyHidden.TryRegisteredClasses found: ',AComponentClass.ClassName]);
      TheModalResult:=mrOk;
      Result:=true;
    end;
  end;

var
  UsedUnitFilenames: TStrings;
  i: Integer;
begin
  Result:=mrCancel;
  CTErrorMsg:='';
  CTErrorCode:=nil;
  CTErrorLine:=0;
  CTErrorCol:=0;

  // check for circles
  if AnUnitInfo.LoadingComponent then begin
    Result:=QuestionDlg(lisCodeTemplError, Format(
      lisUnableToLoadTheComponentClassBecauseItDependsOnIts, ['"',
      AComponentClassName, '"']),
      mtError, [mrCancel, lisCancelLoadingThisComponent,
               mrAbort, lisAbortWholeLoading], 0);
    exit;
  end;

  AnUnitInfo.LoadingComponent:=true;
  try
    // search component lfm
    debugln('TMainIDE.DoLoadComponentDependencyHidden ',AnUnitInfo.Filename,' AComponentName=',AComponentClassName,' AComponentClass=',dbgsName(AComponentClass));

    // first search the resource of ComponentUnitInfo
    if ComponentUnitInfo<>nil then begin
      if TryUnit(ComponentUnitInfo.Filename,Result,false) then exit;
    end;

    // then try registered classes
    if TryRegisteredClasses(Result) then exit;

    // finally search in used units
    UsedUnitFilenames:=nil;
    try
      if not CodeToolBoss.FindUsedUnitFiles(AnUnitInfo.Source,UsedUnitFilenames)
      then begin
        DoJumpToCodeToolBossError;
        Result:=mrCancel;
        exit;
      end;

      if (UsedUnitFilenames<>nil) then begin
        // search for every used unit the .lfm file
        for i:=UsedUnitFilenames.Count-1 downto 0 do begin
          if TryUnit(UsedUnitFilenames[i],Result,false) then exit;
        end;
        // search in every used unit the class
        for i:=UsedUnitFilenames.Count-1 downto 0 do begin
          if TryUnit(UsedUnitFilenames[i],Result,true) then exit;
        end;
        if CTErrorMsg<>'' then begin
          // class not found and there was a parser error
          // maybe that's the reason, why the class was not found
          // show the user
          if ([ofProjectLoading,ofQuiet]*Flags=[]) then begin
            CodeToolBoss.SetError(CTErrorCode,CTErrorLine,CTErrorCol,CTErrorMsg);
            DoJumpToCodeToolBossError;
            Result:=mrAbort;
            exit;
          end;
        end;
      end;
    finally
      UsedUnitFilenames.Free;
    end;

    Result:=QuestionDlg(lisCodeTemplError, Format(
      lisUnableToFindTheUnitOfComponentClass, ['"', AComponentClassName, '"']),
      mtError, [mrCancel, lisCancelLoadingThisComponent,
               mrAbort, lisAbortWholeLoading,
               mrIgnore, lisIgnoreUseTFormAsAncestor], 0);
  finally
    AnUnitInfo.LoadingComponent:=false;
  end;
end;

{-------------------------------------------------------------------------------
  function TMainIDE.CloseUnitComponent

  Params: AnUnitInfo: TUnitInfo
  Result: TModalResult;

  Free the designer form of a unit.
-------------------------------------------------------------------------------}
function TMainIDE.CloseUnitComponent(AnUnitInfo: TUnitInfo; Flags: TCloseFlags
  ): TModalResult;

  procedure FreeUnusedComponents;
  var
    CompUnitInfo: TUnitInfo;
  begin
    CompUnitInfo:=Project1.FirstUnitWithComponent;
    while CompUnitInfo<>nil do begin
      //DebugLn(['FreeUnusedComponents ',CompUnitInfo.Filename,' ',dbgsName(CompUnitInfo.Component),' UnitComponentIsUsed=',UnitComponentIsUsed(CompUnitInfo,true)]);
      if not UnitComponentIsUsed(CompUnitInfo,true) then begin
        CloseUnitComponent(CompUnitInfo,Flags);
        exit;
      end;
      CompUnitInfo:=CompUnitInfo.NextUnitWithComponent;
    end;
  end;

var
  AForm: TCustomForm;
  OldDesigner: TDesigner;
  LookupRoot: TComponent;
begin
  LookupRoot:=AnUnitInfo.Component;
  if LookupRoot=nil then exit(mrOk);
  //DebugLn(['TMainIDE.CloseUnitComponent ',AnUnitInfo.Filename,' ',dbgsName(LookupRoot)]);

  // save
  if (cfSaveFirst in Flags) and (AnUnitInfo.EditorIndex>=0) then begin
    Result:=DoSaveEditorFile(AnUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
    if Result<>mrOk then exit;
  end;

  // close dependencies
  if cfCloseDependencies in Flags then begin
    DumpStack;
    Result:=CloseDependingUnitComponents(AnUnitInfo,Flags);
    if Result<>mrOk then exit;
  end;

  AForm:=FormEditor1.GetDesignerForm(LookupRoot);
  OldDesigner:=nil;
  if AForm<>nil then
    OldDesigner:=TDesigner(AForm.Designer);
  if FLastFormActivated=AForm then
    FLastFormActivated:=nil;
  if (OldDesigner=nil) then begin
    // hidden component
    //DebugLn(['TMainIDE.CloseUnitComponent freeing hidden component without designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
    if UnitComponentIsUsed(AnUnitInfo,false) then begin
      // hidden component is still used => keep it
    end else begin
      // hidden component is not used => free it
      FormEditor1.DeleteComponent(LookupRoot,true);
      AnUnitInfo.Component:=nil;
      FreeUnusedComponents;
    end;
  end else begin
    // component with designer
    if UnitComponentIsUsed(AnUnitInfo,false) then begin
      // free designer, keep component hidden
      //DebugLn(['TMainIDE.CloseUnitComponent hiding component and freeing designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
      OldDesigner.FreeDesigner(false);
    end else begin
      // free designer and design form
      //DebugLn(['TMainIDE.CloseUnitComponent freeing component and designer: ',AnUnitInfo.Filename,' ',DbgSName(AnUnitInfo.Component)]);
      OldDesigner.FreeDesigner(true);
      AnUnitInfo.Component:=nil;
      FreeUnusedComponents;
    end;
  end;

  Result:=mrOk;
end;

function TMainIDE.CloseDependingUnitComponents(AnUnitInfo: TUnitInfo;
  Flags: TCloseFlags): TModalResult;
var
  DependingUnitInfo: TUnitInfo;
  UserAsked: Boolean;
  DependenciesFlags: TCloseFlags;
begin
  Result:=mrCancel;
  UserAsked:=false;
  repeat
    DependingUnitInfo:=Project1.UnitUsingComponentUnit(AnUnitInfo);
    if DependingUnitInfo=nil then exit(mrOk);
    if (not UserAsked) and (not (cfQuiet in Flags)) then begin
      Result:=IDEQuestionDialog('Close component?',
        'Close component '+dbgsName(DependingUnitInfo.Component)+'?',
        mtConfirmation,[mrYes,mrAbort]);
      if Result<>mrYes then exit;
      UserAsked:=true;
    end;
    // close recursively
    DependenciesFlags:=Flags+[cfCloseDependencies];
    if cfSaveDependencies in Flags then
      Include(DependenciesFlags,cfSaveFirst);
    Result:=CloseUnitComponent(DependingUnitInfo,DependenciesFlags);
    if Result<>mrOk then exit;
  until false;
end;

function TMainIDE.UnitComponentIsUsed(AnUnitInfo: TUnitInfo;
  CheckHasDesigner: boolean): boolean;
var
  LookupRoot: TComponent;
  AForm: TCustomForm;
begin
  Result:=false;
  LookupRoot:=AnUnitInfo.Component;
  if LookupRoot=nil then exit;
  // check if a designer is open
  if CheckHasDesigner then begin
    AForm:=FormEditor1.GetDesignerForm(LookupRoot);
    if (AForm<>nil) and (AForm.Designer<>nil) then exit(true);
  end;
  // check if another component uses this component
  if Project1.UnitUsingComponentUnit(AnUnitInfo)<>nil then
    exit(true);
end;

function TMainIDE.GetAncestorUnit(AnUnitInfo: TUnitInfo): TUnitInfo;
begin
  if (AnUnitInfo=nil) or (AnUnitInfo.Component=nil) then
    Result:=nil
  else
    Result:=AnUnitInfo.FindAncestorUnit;
end;

function TMainIDE.GetAncestorLookupRoot(AnUnitInfo: TUnitInfo): TComponent;
var
  AncestorUnit: TUnitInfo;
begin
  AncestorUnit:=GetAncestorUnit(AnUnitInfo);
  if AncestorUnit<>nil then
    Result:=AncestorUnit.Component
  else
    Result:=nil;
end;

procedure TMainIDE.UpdateSaveMenuItemsAndButtons(UpdateSaveAll: boolean);
var
  SrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(SrcEdit,AnUnitInfo);
  // menu items
  if UpdateSaveAll then
    MainIDEBar.itmProjectSave.Enabled :=
     SomethingOfProjectIsModified
     or ((Project1<>nil) and Project1.IsVirtual);
  MainIDEBar.itmFileSave.Enabled :=
    ((SrcEdit<>nil) and SrcEdit.Modified)
    or ((AnUnitInfo<>nil) and (AnUnitInfo.IsVirtual));
  if UpdateSaveAll then
    MainIDEBar.itmFileSaveAll.Enabled := MainIDEBar.itmProjectSave.Enabled;
  // toolbar buttons
  MainIDEBar.SaveSpeedBtn.Enabled := MainIDEBar.itmFileSave.Enabled;
  if UpdateSaveAll then
    MainIDEBar.SaveAllSpeedBtn.Enabled := MainIDEBar.itmFileSaveAll.Enabled;
end;

function TMainIDE.CreateProjectObject(ProjectDesc,
  FallbackProjectDesc: TProjectDescriptor): TProject;
begin
  Result:=TProject.Create(ProjectDesc);
  // custom initialization
  Result.BeginUpdate(true);
  if ProjectDesc.InitProject(Result)<>mrOk then begin
    Result.EndUpdate;
    Result.Free;
    Result:=nil;
    if FallbackProjectDesc=nil then exit;
    Result:=TProject.Create(FallbackProjectDesc);
    FallbackProjectDesc.InitProject(Result);
  end
  else
    Result.EndUpdate;

  Result.MainProject:=true;
  Result.OnFileBackup:=@MainBuildBoss.BackupFile;
  Result.OnLoadProjectInfo:=@OnLoadProjectInfoFromXMLConfig;
  Result.OnSaveProjectInfo:=@OnSaveProjectInfoToXMLConfig;
  Result.OnGetTestDirectory:=@OnProjectGetTestDirectory;
  Result.OnChangeProjectInfoFile:=@OnProjectChangeInfoFile;
end;

procedure TMainIDE.OnLoadProjectInfoFromXMLConfig(TheProject: TProject;
  XMLConfig: TXMLConfig; Merge: boolean);
begin
  if TheProject=Project1 then
    DebugBoss.LoadProjectSpecificInfo(XMLConfig,Merge);
end;

procedure TMainIDE.OnSaveProjectInfoToXMLConfig(TheProject: TProject;
  XMLConfig: TXMLConfig; WriteFlags: TProjectWriteFlags);
begin
  if (TheProject=Project1) and (not (pwfSkipDebuggerSettings in WriteFlags))
  then
    DebugBoss.SaveProjectSpecificInfo(XMLConfig,WriteFlags);
end;

procedure TMainIDE.OnProjectGetTestDirectory(TheProject: TProject;
  out TestDir: string);
begin
  TestDir:=GetTestBuildDirectory;
end;

procedure TMainIDE.OnProjectChangeInfoFile(TheProject: TProject);
begin
  if TheProject<>Project1 then exit;
  if TheProject.IsVirtual then
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',VirtualDirectory)
  else
    CodeToolBoss.SetGlobalValue(ExternalMacroStart+'ProjPath',
                                Project1.ProjectDirectory)
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
      if (MainUnitSrcEdit<>nil) and UpdateModified and MainUnitSrcEdit.Modified
      then begin
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

  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=Format(lisSaveProjectLpi, [Project1.Title]);

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
      if not FilenameIsAbsolute(NewFilename) then
        RaiseException('TMainIDE.DoShowSaveProjectAsDialog: buggy ExpandFileName');
      NewProgramName:=ExtractFileNameOnly(NewFilename);

      // check programname
      if (NewProgramName='') or (not IsValidIdent(NewProgramName)) then begin
        Result:=MessageDlg(lisInvalidProjectFilename,
          Format(lisisAnInvalidProjectNamePleaseChooseAnotherEGProject, ['"',
            SaveDialog.Filename, '"', #13]),
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
          Result:=MessageDlg(lisInvalidPascalIdentifierCap,
            Format(lisTheNameIsNotAValidPascalIdentifier, ['"', NewProgramName,
              '"'])
            ,mtWarning,[mbIgnore,mbCancel],0);
          if Result=mrCancel then exit;
          Result:=mrCancel;
        end;
      end;

      // apply naming conventions
      NewProgramName:=ExtractFileNameOnly(NewFilename);

      if EnvironmentOptions.CharcaseFileAction = ccfaAutoRename then
        NewFileName:=ExtractFilePath(NewFilename)
                    +lowercase(ExtractFileName(NewFilename));

      if Project1.MainUnitID>=0 then begin
        // check mainunit filename
        Ext:=ExtractFileExt(Project1.MainUnitInfo.Filename);
        if Ext='' then Ext:='.pas';
        NewProgramFilename:=ChangeFileExt(NewFilename,Ext);
        if CompareFilenames(NewFilename,NewProgramFilename)=0 then begin
          ACaption:=lisChooseADifferentName;
          AText:=Format(lisTheProjectInfoFileIsEqualToTheProjectMainSource, [
            '"', NewFilename, '"', #13]);
          Result:=MessageDlg(ACaption, AText, mtError, [mbAbort,mbRetry],0);
          if Result=mrAbort then exit;
          continue; // try again
        end;
        // check programname
        if FilenameIsPascalUnit(NewProgramFilename)
        and (Project1.IndexOfUnitWithName(NewProgramName,true,
                                       Project1.MainUnitInfo)>=0) then
        begin
          ACaption:=lisUnitIdentifierExists;
          AText:=Format(lisThereIsAUnitWithTheNameInTheProjectPleaseChoose, ['"',
            NewProgramName, '"', #13]);
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
    ACaption:=lisOverwriteFile;
    AText:=Format(lisAFileAlreadyExistsReplaceIt, ['"', NewFilename, '"', #13]);
    Result:=MessageDlg(ACaption, AText, mtConfirmation, [mbOk, mbCancel], 0);
    if Result=mrCancel then exit;
  end
  else begin
    if FileExists(NewProgramFilename) then begin
      ACaption:=lisOverwriteFile;
      AText:=Format(lisAFileAlreadyExistsReplaceIt, ['"', NewProgramFilename,
        '"', #13]);
      Result:=MessageDlg(ACaption, AText, mtConfirmation,[mbOk,mbCancel],0);
      if Result=mrCancel then exit;
    end;
  end;

  // set new project filename
  Project1.ProjectInfoFile:=NewFilename;
  EnvironmentOptions.AddToRecentProjectFiles(NewFilename);
  SetRecentProjectFilesMenu;

  // change main source
  if (Project1.MainUnitID>=0) then begin
    GetMainUnit(MainUnitInfo,MainUnitSrcEdit,true);

    // switch MainUnitInfo.Source to new code
    NewBuf:=CodeToolBoss.CreateFile(NewProgramFilename);
    if NewBuf=nil then begin
      Result:=MessageDlg(lisErrorCreatingFile, Format(lisUnableToCreateFile3, [
        #13, '"', NewProgramFilename, '"']), mtError, [mbCancel], 0);
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

  // update paths
  Project1.CompilerOptions.OtherUnitFiles:=
    RebaseSearchPath(Project1.CompilerOptions.OtherUnitFiles,OldProjectPath,
                     Project1.ProjectDirectory,true);
  Project1.CompilerOptions.IncludePath:=
    RebaseSearchPath(Project1.CompilerOptions.IncludePath,OldProjectPath,
                     Project1.ProjectDirectory,true);
  Project1.CompilerOptions.Libraries:=
    RebaseSearchPath(Project1.CompilerOptions.Libraries,OldProjectPath,
                     Project1.ProjectDirectory,true);
  Project1.CompilerOptions.ObjectPath:=
    RebaseSearchPath(Project1.CompilerOptions.ObjectPath,OldProjectPath,
                     Project1.ProjectDirectory,true);
  Project1.CompilerOptions.SrcPath:=
    RebaseSearchPath(Project1.CompilerOptions.SrcPath,OldProjectPath,
                     Project1.ProjectDirectory,true);
  Project1.CompilerOptions.DebugPath:=
    RebaseSearchPath(Project1.CompilerOptions.DebugPath,OldProjectPath,
                     Project1.ProjectDirectory,true);

  // invalidate cached substituted macros
  IncreaseCompilerParseStamp;

  Result:=mrOk;
end;

function TMainIDE.DoUpdateLRSFromLFM(const LRSFilename: string): TModalResult;
var
  LFMFilename: String;
begin
  Result:=mrOk;
  // check if there is a .lrs file
  if LRSFilename='' then exit;
  if not FilenameIsAbsolute(LRSFilename) then exit;
  LFMFilename:=ChangeFileExt(LRSFilename,'.lfm');
  if LRSFilename=LFMFilename then exit;
  // check if there is a .lfm file
  if not FileExists(LFMFilename) then exit;
  // check if .lrs file is newer than .lfm file
  if FileExists(LRSFilename) and (FileAge(LFMFilename)<=FileAge(LRSFilename))
  then exit;
  debugln('TMainIDE.DoUpdateLRSFromLFM ',LRSFilename,' ',dbgs(FileAge(LFMFilename)),' ',dbgs(FileAge(LRSFilename)));
  // the .lrs file does not exist, or is older than the .lfm file
  // -> update .lrs file
  Result:=ConvertLFMToLRSFileInteractive(LFMFilename,LRSFilename);
end;

function TMainIDE.DoCompleteLoadingProjectInfo: TModalResult;
begin
  UpdateCaption;
  EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
  EnvironmentOptions.Save(false);
  MainBuildBoss.RescanCompilerDefines(true);

  // load required packages
  PkgBoss.OpenProjectDependencies(Project1,true);

  Project1.DefineTemplates.AllChanged;
  //DebugLn('TMainIDE.DoCompleteLoadingProjectInfo ',Project1.IDAsString);
  Project1.DefineTemplates.Active:=true;
  Result:=mrOk;
end;

procedure TMainIDE.OnCopyFile(const Filename: string; var Copy: boolean;
  Data: TObject);
begin
  if Data=nil then exit;
  if Data is TPublishModuleOptions then begin
    Copy:=TPublishModuleOptions(Data).FileCanBePublished(Filename);
    //writeln('TMainIDE.OnCopyFile "',Filename,'" ',Copy);
  end;
end;

procedure TMainIDE.OnCopyError(const ErrorData: TCopyErrorData;
  var Handled: boolean; Data: TObject);
begin
  case ErrorData.Error of
    ceSrcDirDoesNotExists:
      MessageDlg(lisCopyError2,
        Format(lisSourceDirectoryDoesNotExist, ['"', ErrorData.Param1, '"']),
        mtError,[mbCancel],0);
    ceCreatingDirectory:
      MessageDlg(lisCopyError2,
        Format(lisUnableToCreateDirectory, ['"', ErrorData.Param1, '"']),
        mtError,[mbCancel],0);
    ceCopyFileError:
      MessageDlg(lisCopyError2,
        Format(lisUnableToCopyFileTo, ['"', ErrorData.Param1, '"', #13, '"',
          ErrorData.Param1, '"']),
        mtError,[mbCancel],0);
  end;
end;

function TMainIDE.DoOpenFileInSourceEditor(AnUnitInfo: TUnitInfo;
  PageIndex: integer; Flags: TOpenFlags): TModalResult;
var NewSrcEdit: TSourceEditor;
  AFilename: string;
  NewSrcEditorCreated: boolean;
  NewCaretXY: TPoint;
  NewTopLine: LongInt;
  NewLeftChar: LongInt;
  NewErrorLine: LongInt;
  NewExecutionLine: LongInt;
begin
  AFilename:=AnUnitInfo.Filename;

  // get syntax highlighter type
  if not AnUnitInfo.CustomHighlighter then
    AnUnitInfo.SyntaxHighlighter:=
      ExtensionToLazSyntaxHighlighter(ExtractFileExt(AFilename));

  NewSrcEditorCreated:=false;
  //DebugLn(['TMainIDE.DoOpenFileInSourceEditor Revert=',ofRevert in Flags,' ',AnUnitInfo.Filename,' PageIndex=',PageIndex]);
  if (not (ofRevert in Flags)) or (PageIndex<0) then begin
    // create a new source editor

    // update marks and cursor positions in Project1, so that merging the old
    // settings during restoration will work
    SaveSourceEditorProjectSpecificSettings;
    SourceNotebook.NewFile(CreateSrcEditPageName(AnUnitInfo.UnitName,
      AFilename,-1),AnUnitInfo.Source,false);
    NewSrcEdit:=SourceNotebook.GetActiveSE;
    NewSrcEdit.EditorComponent.BeginUpdate;
    NewSrcEditorCreated:=true;
    MainIDEBar.itmFileClose.Enabled:=True;
    MainIDEBar.itmFileCloseAll.Enabled:=True;
    NewCaretXY:=AnUnitInfo.CursorPos;
    NewTopLine:=AnUnitInfo.TopLine;
    NewLeftChar:=1;
    NewErrorLine:=-1;
    NewExecutionLine:=-1;
  end else begin
    // revert code in existing source editor
    NewSrcEdit:=SourceNotebook.FindSourceEditorWithPageIndex(PageIndex);
    NewCaretXY:=NewSrcEdit.EditorComponent.CaretXY;
    NewTopLine:=NewSrcEdit.EditorComponent.TopLine;
    NewLeftChar:=NewSrcEdit.EditorComponent.LeftChar;
    NewErrorLine:=NewSrcEdit.ErrorLine;
    NewExecutionLine:=NewSrcEdit.ExecutionLine;
    NewSrcEdit.EditorComponent.BeginUpdate;
    if NewSrcEdit.CodeBuffer=AnUnitInfo.Source then begin
      AnUnitInfo.Source.AssignTo(NewSrcEdit.EditorComponent.Lines,true);
    end else
      NewSrcEdit.CodeBuffer:=AnUnitInfo.Source;
    AnUnitInfo.ClearModifieds;
    //DebugLn(['TMainIDE.DoOpenFileInSourceEditor NewCaretXY=',dbgs(NewCaretXY),' NewTopLine=',NewTopLine]);
  end;

  // update editor indices in project
  if (not (ofProjectLoading in Flags)) and NewSrcEditorCreated then
    Project1.InsertEditorIndex(SourceNotebook.Notebook.PageIndex);
  AnUnitInfo.EditorIndex:=SourceNotebook.FindPageWithEditor(NewSrcEdit);

  // restore source editor settings
  DoRestoreBookMarks(AnUnitInfo,NewSrcEdit);
  DebugBoss.DoRestoreDebuggerMarks(AnUnitInfo);
  NewSrcEdit.SyntaxHighlighterType:=AnUnitInfo.SyntaxHighlighter;
  NewSrcEdit.EditorComponent.CaretXY:=NewCaretXY;
  NewSrcEdit.EditorComponent.TopLine:=NewTopLine;
  NewSrcEdit.EditorComponent.LeftChar:=NewLeftChar;
  NewSrcEdit.ErrorLine:=NewErrorLine;
  NewSrcEdit.ExecutionLine:=NewExecutionLine;
  NewSrcEdit.ReadOnly:=AnUnitInfo.ReadOnly;
  NewSrcEdit.Modified:=false;

  // mark unit as loaded
  NewSrcEdit.EditorComponent.EndUpdate;
  AnUnitInfo.Loaded:=true;

  // update statusbar and focus editor
  if (not (ofProjectLoading in Flags)) then
    SourceNotebook.FocusEditor;
  SourceNoteBook.UpdateStatusBar;

  Result:=mrOk;
end;

function TMainIDE.DoNewFile(NewFileDescriptor: TProjectFileDescriptor;
  var NewFilename: string; const NewSource: string;
  NewFlags: TNewFlags; NewOwner: TObject): TModalResult;

  function BeautifySrc(const s: string): string;
  begin
    Result:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                  BeautifyStatement(s,0);
  end;

var
  NewUnitInfo:TUnitInfo;
  NewSrcEdit: TSourceEditor;
  NewUnitName: string;
  NewBuffer: TCodeBuffer;
  OldUnitIndex: Integer;
  AncestorType: TPersistentClass;
  LFMFilename: String;
  SearchFlags: TProjectFileSearchFlags;
  LFMSourceText: String;
  LFMCode: TCodeBuffer;
  AProject: TProject;
begin
  debugln('TMainIDE.DoNewEditorFile A NewFilename=',NewFilename);
  SaveSourceEditorChangesToCodeCache(-1);

  // convert macros in filename
  if nfConvertMacros in NewFlags then begin
    if not GlobalMacroList.SubstituteStr(NewFilename) then begin
      Result:=mrCancel;
      exit;
    end;
  end;

  if NewOwner is TProject then
    AProject:=TProject(NewOwner)
  else
    AProject:=Project1;

  // create new codebuffer and apply naming conventions
  Result:=CreateNewCodeBuffer(NewFileDescriptor,NewOwner,NewFilename,NewBuffer,
                              NewUnitName);
  if Result<>mrOk then exit;

  NewFilename:=NewBuffer.Filename;
  OldUnitIndex:=AProject.IndexOfFilename(NewFilename);
  if OldUnitIndex>=0 then begin
    // the file is not really new
    NewUnitInfo:=AProject.Units[OldUnitIndex];
    // close form
    Result:=CloseUnitComponent(NewUnitInfo,
                               [cfCloseDependencies,cfSaveDependencies]);
    if Result<>mrOk then exit;
    // assign source
    NewUnitInfo.Source:=NewBuffer;
  end else
    NewUnitInfo:=TUnitInfo.Create(NewBuffer);
  NewUnitInfo.ImproveUnitNameCache(NewUnitName);

  // create source code
  //debugln('TMainIDE.DoNewEditorFile A nfCreateDefaultSrc=',nfCreateDefaultSrc in NewFlags,' ResourceClass=',dbgs(NewFileDescriptor.ResourceClass));
  if nfCreateDefaultSrc in NewFlags then begin
    if (NewFileDescriptor.ResourceClass<>nil) then begin
      NewUnitInfo.ComponentName:=
        AProject.NewUniqueComponentName(NewFileDescriptor.DefaultResourceName);
      NewUnitInfo.ComponentResourceName:='';
    end;
    NewUnitInfo.CreateStartCode(NewFileDescriptor,NewUnitName);
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
                     or (NewOwner is TProject)
                     or (AProject.FileIsInProjectDir(NewFilename)
                         and (not (nfIsNotPartOfProject in NewFlags)));
  end;
  if OldUnitIndex<0 then begin
    Project1.AddFile(NewUnitInfo,
                     NewFileDescriptor.AddToProject
                     and NewUnitInfo.IsPartOfProject);
  end;

  // syntax highlighter type
  NewUnitInfo.SyntaxHighlighter:=
    ExtensionToLazSyntaxHighlighter(ExtractFileExt(NewFilename));

  if nfOpenInEditor in NewFlags then begin
    // open a new sourceeditor
    SourceNotebook.NewFile(CreateSrcEditPageName(NewUnitInfo.UnitName,
                                                 NewUnitInfo.Filename,-1),
                           NewUnitInfo.Source,true);
    MainIDEBar.itmFileClose.Enabled:=True;
    MainIDEBar.itmFileCloseAll.Enabled:=True;
    NewSrcEdit:=SourceNotebook.GetActiveSE;
    NewSrcEdit.SyntaxHighlighterType:=NewUnitInfo.SyntaxHighlighter;
    Project1.InsertEditorIndex(SourceNotebook.Notebook.PageIndex);
    NewUnitInfo.EditorIndex:=SourceNotebook.Notebook.PageIndex;

    // create component
    AncestorType:=NewFileDescriptor.ResourceClass;
    if AncestorType<>nil then begin
      LFMSourceText:=NewFileDescriptor.GetResourceSource;
      if LFMSourceText<>'' then begin
        // the NewFileDescriptor provides a custom .lfm source
        // -> put it into a new .lfm buffer and load it
        LFMFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lfm');
        LFMCode:=CodeToolBoss.CreateFile(LFMFilename);
        LFMCode.Source:=LFMSourceText;
        //debugln('TMainIDE.DoNewEditorFile A ',LFMFilename);
        Result:=DoLoadLFM(NewUnitInfo,LFMCode,[],[]);
      end else begin
        // create a default form/datamodule
        Result:=CreateNewForm(NewUnitInfo,AncestorType,nil);
      end;
      if Result<>mrOk then exit;
    end;

    // show form and select form
    if NewUnitInfo.Component<>nil then begin
      // show form
      DoShowDesignerFormOfCurrentSrc;
    end else begin
      FDisplayState:= dsSource;
    end;

    if nfSave in NewFlags then begin
      NewUnitInfo.Modified:=true;
      Result:=DoSaveEditorFile(NewUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
      if Result<>mrOk then exit;
    end;
  end else begin
    // do not open in editor
    if nfSave in NewFlags then begin
      NewBuffer.Save;
    end;
  end;

  // Update HasResources property (if the .lfm file was created separately)
  if (not NewUnitInfo.HasResources)
  and FilenameIsPascalUnit(NewUnitInfo.Filename) then begin
    //debugln('TMainIDE.DoNewEditorFile no HasResources ',NewUnitInfo.Filename);
    LFMFilename:=ChangeFileExt(NewUnitInfo.Filename,'.lfm');
    SearchFlags:=[];
    if NewUnitInfo.IsPartOfProject then
      Include(SearchFlags,pfsfOnlyProjectFiles);
    if NewUnitInfo.IsVirtual then
      Include(SearchFlags,pfsfOnlyVirtualFiles);
    if (AProject.UnitInfoWithFilename(LFMFilename,SearchFlags)<>nil) then begin
      //debugln('TMainIDE.DoNewEditorFile no HasResources ',NewUnitInfo.Filename,' ResourceFile exists');
      NewUnitInfo.ResourceFileName:=ChangeFileExt(NewUnitInfo.Filename,'.lrs');
      NewUnitInfo.HasResources:=true;
    end;
  end;

  Result:=mrOk;
  DebugLn('TMainIDE.DoNewEditorFile end ',NewUnitInfo.Filename);
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoNewUnit end');{$ENDIF}
end;

function TMainIDE.DoNewOther: TModalResult;
var
  NewIDEItem: TNewIDEItemTemplate;
begin
  Result:=ShowNewIDEItemDialog(NewIDEItem);
  if Result<>mrOk then exit;
  if NewIDEItem is TNewItemProjectFile then begin
    // file
    Result:=DoNewEditorFile(TNewItemProjectFile(NewIDEItem).Descriptor,
                                   '','',[nfOpenInEditor,nfCreateDefaultSrc]);
  end else if NewIDEItem is TNewItemProject then begin
    // project
    //debugln('TMainIDE.DoNewOther ',dbgsName(TNewItemProject(NewIDEItem).Descriptor));
    Result:=DoNewProject(TNewItemProject(NewIDEItem).Descriptor);
  end else if NewIDEItem is TNewItemPackage then begin
    // packages
    PkgBoss.DoNewPackage;
  end else begin
    MessageDlg(ueNotImplCap,
               lisSorryThisTypeIsNotYetImplemented,
      mtInformation,[mbOk],0);
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
  //writeln('TMainIDE.DoSaveEditorFile A ',ActiveUnitInfo.Filename,' ',ActiveUnitInfo.NeedsSaveToDisk);
  if ([sfSaveToTestDir,sfSaveAs]*Flags=[])
  and (not ActiveUnitInfo.NeedsSaveToDisk) then begin
    Result:=mrOk;
    exit;
  end;

  // load old resource file
  Result:=DoLoadResourceFile(ActiveUnitInfo,LFMCode,ResourceCode,
                             not (sfSaveAs in Flags));
  if Result in [mrIgnore,mrOk] then
    Result:=mrCancel
  else
    exit;

  if [sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs] then begin
    // let user choose a filename
    Result:=DoShowSaveFileAsDialog(ActiveUnitInfo,ResourceCode);
    if Result in [mrIgnore,mrOk] then
      Result:=mrCancel
    else
      exit;
    LFMCode:=nil;
  end;

  // save source
  if (sfSaveToTestDir in Flags) or ActiveUnitInfo.IsVirtual then begin
    // save source to test directory
    TestFilename:=MainBuildBoss.GetTestUnitFilename(ActiveUnitInfo);
    if TestFilename<>'' then begin
      Result:=ActiveUnitInfo.WriteUnitSourceToFile(TestFilename);
      if Result<>mrOk then exit;
      DestFilename:=TestFilename;
    end else
      exit;
  end else begin
    if ActiveUnitInfo.Modified or ActiveUnitInfo.NeedsSaveToDisk then begin
      // save source to file
      Result:=ActiveUnitInfo.WriteUnitSource;
      if Result=mrAbort then exit;
      DestFilename:=ActiveUnitInfo.Filename;
    end;
  end;

  if sfCheckAmbiguousFiles in Flags then
    MainBuildBoss.CheckAmbiguousSources(DestFilename,false);

  {$IFDEF IDE_DEBUG}
  writeln('*** HasResources=',ActiveUnitInfo.HasResources);
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoSaveEditorFile B');{$ENDIF}
  // save resource file and lfm file
  if (ResourceCode<>nil) or (ActiveUnitInfo.Component<>nil) then begin
    Result:=DoSaveUnitComponent(ActiveUnitInfo,ResourceCode,LFMCode,Flags);
    if Result in [mrIgnore, mrOk] then
      Result:=mrCancel
    else
      exit;
  end;

  // unset all modified flags
  if not (sfSaveToTestDir in Flags) then begin
    ActiveUnitInfo.ClearModifieds;
    ActiveSrcEdit.Modified:=false;
    UpdateSaveMenuItemsAndButtons(not (sfProjectSaving in Flags));
  end;
  SourceNoteBook.UpdateStatusBar;

  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoSaveEditorFile END');
  {$ENDIF}
  Result:=mrOk;
end;

function TMainIDE.DoCloseEditorFile(PageIndex:integer;
  Flags: TCloseFlags): TModalResult;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  ACaption, AText: string;
  i: integer;
begin
  debugln('TMainIDE.DoCloseEditorFile A PageIndex=',IntToStr(PageIndex));
  Result:=mrCancel;
  GetUnitWithPageIndex(PageIndex,ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo=nil then exit;
  if (ActiveUnitInfo.Component<>nil)
  and (FLastFormActivated<>nil)
  and (TDesigner(FLastFormActivated.Designer).LookupRoot=ActiveUnitInfo.Component)
  then
    FLastFormActivated:=nil;

  // save some meta data of the source
  SaveSrcEditorProjectSpecificSettings(ActiveUnitInfo);

  // if SaveFirst then save the source
  if (cfSaveFirst in Flags) and (not ActiveUnitInfo.ReadOnly)
  and ((ActiveSrcEdit.Modified) or (ActiveUnitInfo.Modified)) then begin
    if not (cfQuiet in Flags) then begin
      // ask user
      if ActiveUnitInfo.Filename<>'' then
        AText:=Format(lisFileHasChangedSave, ['"', ActiveUnitInfo.Filename, '"'])
      else if ActiveUnitInfo.UnitName<>'' then
        AText:=Format(lisUnitHasChangedSave, ['"', ActiveUnitInfo.Unitname, '"'])
      else
        AText:=Format(lisSourceOfPageHasChangedSave, ['"',
          ActiveSrcEdit.PageName, '"']);
      ACaption:=lisSourceModified;
      Result:=Messagedlg(ACaption, AText,
                         mtConfirmation, [mbYes, mbNo, mbAbort], 0);
    end else
      Result:=mrYes;
    if Result=mrYes then begin
      Result:=DoSaveEditorFile(PageIndex,[sfCheckAmbiguousFiles]);
    end;
    if Result=mrAbort then exit;
    Result:=mrOk;
  end;

  // close form soft (keep it if used by another component)
  CloseUnitComponent(ActiveUnitInfo,[]);

  // close source editor
  SourceNoteBook.CloseFile(PageIndex);
  MainIDEBar.itmFileClose.Enabled:=SourceNoteBook.Notebook<>nil;
  MainIDEBar.itmFileCloseAll.Enabled:=MainIDEBar.itmFileClose.Enabled;

  // close file in project
  Project1.CloseEditorIndex(ActiveUnitInfo.EditorIndex);
  ActiveUnitInfo.Loaded:=false;
  if ActiveUnitInfo<>Project1.MainUnitInfo then
    ActiveUnitInfo.Source:=nil;
  i:=Project1.IndexOf(ActiveUnitInfo);
  if (i<>Project1.MainUnitID) and (ActiveUnitInfo.IsVirtual) then begin
    Project1.RemoveUnit(i);
  end;

  DebugLn('TMainIDE.DoCloseEditorFile end');
  Result:=mrOk;
end;

function TMainIDE.DoCloseEditorFile(const Filename: string; Flags: TCloseFlags
  ): TModalResult;
var
  UnitIndex: Integer;
  AnUnitInfo: TUnitInfo;
begin
  Result:=mrOk;
  if Filename='' then exit;
  UnitIndex:=Project1.IndexOfFilename(TrimFilename(Filename),
                                    [pfsfOnlyEditorFiles,pfsfResolveFileLinks]);
  if UnitIndex<0 then exit;
  AnUnitInfo:=Project1.Units[UnitIndex];
  if AnUnitInfo.EditorIndex>=0 then
    Result:=DoCloseEditorFile(AnUnitInfo.EditorIndex,Flags)
  else
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
  DiskFilename: String;

  function OpenResource: TModalResult;
  var
    CloseFlags: TCloseFlags;
  begin
    // read form data
    if FilenameIsPascalUnit(AFilename) then begin
      // this could be a unit with a form
      //debugln('TMainIDE.DoOpenEditorFile ',AFilename,' ',OpenFlagsToString(Flags));
      if (not (ofDoNotLoadResource in Flags))
      and ( (ofDoLoadResource in Flags)
         or ((not Project1.AutoOpenDesignerFormsDisabled)
             and (EnvironmentOptions.AutoCreateFormsOnOpen
                  or (NewUnitInfo.Component<>nil))))
      then begin
        // -> try to (re)load the lfm file
        //debugln('TMainIDE.DoOpenEditorFile Loading LFM for ',NewUnitInfo.Filename);
        CloseFlags:=[cfSaveDependencies];
        if ofRevert in Flags then
          Include(CloseFlags,cfCloseDependencies);
        Result:=DoLoadLFM(NewUnitInfo,Flags,CloseFlags);
        if Result<>mrOk then exit;
      end else begin
        Result:=mrOk;
      end;
    end else if NewUnitInfo.Component<>nil then begin
      // this is no pascal source and there is a designer form
      // This can be the case, when the file is renamed and/or reverted
      // -> close form
      Result:=CloseUnitComponent(NewUnitInfo,
                                 [cfCloseDependencies,cfSaveDependencies]);
    end else
      Result:=mrOk;
  end;


begin
  {$IFDEF IDE_VERBOSE}
  DebugLn('');
  DebugLn('*** TMainIDE.DoOpenEditorFile START "',AFilename,'" ',OpenFlagsToString(Flags));
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenEditorFile START');{$ENDIF}
  Result:=mrCancel;

  // replace macros
  if ofConvertMacros in Flags then begin
    if not GlobalMacroList.SubstituteStr(AFilename) then exit;
    AFilename:=ExpandFilename(AFilename);
  end;

  // revert: use source editor filename
  if (ofRevert in Flags) and (PageIndex>=0) then
    AFilename:=SourceNotebook.FindSourceEditorWithPageIndex(PageIndex).FileName;

  // normalize filename
  AFilename:=TrimFilename(AFilename);
  DiskFilename:=FindDiskFilename(AFilename);
  if DiskFilename<>AFilename then begin
    debugln('WARNING: TMainIDE.DoOpenEditorFile Opening "',DiskFilename,'" instead "',AFilename,'"');
    AFilename:=DiskFilename;
  end;
  
  FilenameNoPath:=ExtractFilename(AFilename);

  // check to not open directories
  if ((FilenameNoPath='') or (FilenameNoPath='.') or (FilenameNoPath='..')) then
  begin
    DebugLn(['TMainIDE.DoOpenEditorFile ignoring special file: ',AFilename]);
    exit;
  end;

  if ([ofAddToRecent,ofRevert,ofVirtualFile]*Flags=[ofAddToRecent])
  and (AFilename<>'') and FilenameIsAbsolute(AFilename) then
    EnvironmentOptions.AddToRecentOpenFiles(AFilename);

  // check if this is a hidden unit:
  // if this is the main unit, it is already
  // loaded and needs only to be shown in the sourceeditor/formeditor
  if (not (ofRevert in Flags))
  and (CompareFilenames(Project1.MainFilename,AFilename,
       not (ofVirtualFile in Flags))=0)
  then begin
    Result:=DoOpenMainUnit(Flags);
    exit;
  end;

  // check for special files
  if ([ofRegularFile,ofRevert,ofProjectLoading]*Flags=[])
  and FilenameIsAbsolute(AFilename) and FileExists(AFilename) then begin
    // check if file is a lazarus project (.lpi)
    if (CompareFileExt(AFilename,'.lpi',false)=0) then begin
      if QuestionDlg(lisOpenProject, Format(lisOpenTheProject, [AFilename]),
        mtConfirmation, [mrYes, lisOpenProject2, mrNo, lisOpenAsXmlFile], 0)=
          mrYes
      then begin
        Result:=DoOpenProjectFile(AFilename,[ofAddToRecent]);
        exit;
      end;
      include(Flags, ofRegularFile);
    end;
    // check if file is a lazarus package (.lpk)
    if (CompareFileExt(AFilename,'.lpk',false)=0) then begin
      if QuestionDlg(lisOpenPackage,
        Format(lisOpenThePackage, [AFilename]), mtConfirmation,
        [mrYes, lisCompPalOpenPackage, mrNo, lisOpenAsXmlFile], 0)=mrYes
      then begin
        Result:=PkgBoss.DoOpenPackageFile(AFilename,[pofAddToRecent]);
        exit;
      end;
    end;
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
      if (ofAddToProject in Flags) and (not NewUnitInfo.IsPartOfProject) then
      begin
        NewUnitInfo.IsPartOfProject:=true;
        Project1.Modified:=true;
      end;
      if (not (ofProjectLoading in Flags)) and (NewUnitInfo.EditorIndex>=0) then
      begin
        //DebugLn('TMainIDE.DoOpenEditorFile file already open ',NewUnitInfo.Filename);
        // file already open -> change source notebook page
        SourceNoteBook.Notebook.PageIndex:=NewUnitInfo.EditorIndex;
        if ofDoLoadResource in Flags then
          Result:=OpenResource
        else
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
        MessageDlg(lisRevertFailed, Format(lisFileIsVirtual, ['"', AFilename,
          '"']),
          mtInformation,[mbCancel],0);
      end;
      Result:=mrCancel;
      exit;
    end;
    ReOpen:=true;
    if (ofAddToProject in Flags) and (not NewUnitInfo.IsPartOfProject) then
    begin
      NewUnitInfo.IsPartOfProject:=true;
      Project1.Modified:=true;
    end;
  end;

  // check if file exists
  if FilenameIsAbsolute(AFilename) and (not FileExists(AFilename)) then begin
    // file does not exist
    if (ofRevert in Flags) then begin
      // revert failed, due to missing file
      if not (ofQuiet in Flags) then begin
        MessageDlg(lisRevertFailed, Format(lisPkgMangFileNotFound, ['"',
          AFilename, '"']),
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
    Result:=LoadCodeBuffer(NewBuf,AFileName,LoadBufferFlags);
    if Result<>mrOk then begin
      DebugLn(['TMainIDE.DoOpenEditorFile failed LoadCodeBuffer: ',AFilename]);
      exit;
    end;
    NewUnitInfo.Source:=NewBuf;
    NewUnitInfo.Modified:=NewUnitInfo.Source.FileOnDiskNeedsUpdate;
    if FilenameIsPascalUnit(NewUnitInfo.Filename) then
      NewUnitInfo.ReadUnitNameFromSource(false);
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
  Result:=DoOpenFileInSourceEditor(NewUnitInfo,PageIndex,Flags);
  if Result<>mrOk then begin
    DebugLn(['TMainIDE.DoOpenEditorFile failed DoOpenFileInSourceEditor: ',AFilename]);
    exit;
  end;

  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.DoOpenEditorFile] C');
  {$ENDIF}

  // open resource component (designer, form, datamodule, ...)
  Result:=OpenResource;
  if Result<>mrOk then begin
    DebugLn(['TMainIDE.DoOpenEditorFile failed OpenResource: ',AFilename]);
    exit;
  end;

  Result:=mrOk;
  //writeln('TMainIDE.DoOpenEditorFile END "',AFilename,'"');
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenEditorFile END');{$ENDIF}
end;

function TMainIDE.DoOpenMainUnit(Flags: TOpenFlags): TModalResult;
var MainUnitInfo: TUnitInfo;
begin
  {$IFDEF IDE_VERBOSE}
  debugln(['[TMainIDE.DoOpenMainUnit] A ProjectLoading=',ofProjectLoading in Flags,' MainUnitID=',Project1.MainUnitID]);
  {$ENDIF}
  Result:=mrCancel;
  if Project1.MainUnitID<0 then exit;
  MainUnitInfo:=Project1.MainUnitInfo;

  // check if main unit is already open in source editor
  if (MainUnitInfo.EditorIndex>=0) and (not (ofProjectLoading in Flags)) then
  begin
    // already loaded -> switch to source editor
    SourceNotebook.Notebook.PageIndex:=MainUnitInfo.EditorIndex;
    Result:=mrOk;
    exit;
  end;

  // open file in source notebook
  Result:=DoOpenFileInSourceEditor(MainUnitInfo,-1,Flags);
  if Result<>mrOk then exit;

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
var
  UnitList: TStringList;
  i: integer;
  MainUnitName, DlgCaption: string;
  MainUnitInfo, AnUnitInfo: TUnitInfo;
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  AForm: TCustomForm;
Begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  UnitList := TStringList.Create;
  UnitList.Sorted := True;
  try
    for i:=0 to Project1.UnitCount-1 do begin
      if not Project1.Units[i].IsPartOfProject then continue;
      //debugln('TMainIDE.DoViewUnitsAndForms OnlyForms=',dbgs(OnlyForms),' CompName=',Project1.Units[i].ComponentName,' UnitName=',Project1.Units[i].UnitName);
      if OnlyForms then
      begin
        // add all form names of project
        if Project1.Units[i].ComponentName<>'' then
        begin
          UnitList.AddObject(Project1.Units[i].UnitName, TViewUnitsEntry.Create(
            Project1.Units[i].ComponentName, i, Project1.Units[i]=ActiveUnitInfo));
        end;
      end else
      begin
        // add all unit names of project
        if (Project1.Units[i].UnitName <> '') then
        begin
          UnitList.AddObject(Project1.Units[i].UnitName, TViewUnitsEntry.Create(
            Project1.Units[i].UnitName, i, Project1.Units[i]=ActiveUnitInfo));
        end
        else if Project1.MainUnitID = i then
        begin
          MainUnitInfo := Project1.MainUnitInfo;
          if pfMainUnitIsPascalSource in Project1.Flags then
          begin
            MainUnitName := CreateSrcEditPageName('',
              MainUnitInfo.Filename,MainUnitInfo.EditorIndex);
            if MainUnitName <> '' then
            begin
              UnitList.AddObject(MainUnitName, TViewUnitsEntry.Create(
                MainUnitName,i,MainUnitInfo=ActiveUnitInfo));
            end;
          end;
        end;
      end;
    end;
    if OnlyForms then
      DlgCaption := dlgMainViewForms
    else
      DlgCaption := dlgMainViewUnits ;
    if ShowViewUnitsDlg(UnitList,true,DlgCaption) = mrOk then
    begin
      { This is where we check what the user selected. }
      AnUnitInfo:=nil;
      for i := 0 to UnitList.Count-1 do
      begin
        if TViewUnitsEntry(UnitList.Objects[i]).Selected then begin
          AnUnitInfo := Project1.Units[TViewUnitsEntry(UnitList.Objects[i]).ID];
          if AnUnitInfo.EditorIndex >= 0 then begin
            SourceNoteBook.Notebook.PageIndex := AnUnitInfo.EditorIndex;
          end else begin
            if Project1.MainUnitInfo = AnUnitInfo then
              Result:=DoOpenMainUnit([])
            else
              Result:=DoOpenEditorFile(AnUnitInfo.Filename,-1,[ofOnlyIfExists]);
            if Result=mrAbort then exit;
          end;
          if OnlyForms and (AnUnitInfo.ComponentName<>'') then begin
            AForm:=GetDesignerFormOfSource(AnUnitInfo,true);
            if AForm<>nil then
              ShowDesignerForm(AForm);
          end;
        end;
      end;  { for }
      if (AnUnitInfo<>nil) and (not OnlyForms) then
      begin
        SourceNotebook.ShowOnTop;
      end;
    end;  { if ShowViewUnitDlg... }
  finally
    for i:=0 to UnitList.Count-1 do
      TViewUnitsEntry(UnitList.Objects[i]).Free;
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
    UnitDependenciesView:=TUnitDependenciesView.Create(OwningComponent);
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
      UnitDependenciesView.BeginUpdate;
      UnitDependenciesView.RootFilename:=Project1.MainUnitInfo.Filename;
      UnitDependenciesView.RootShortFilename:=
        ExtractFilename(Project1.MainUnitInfo.Filename);
      UnitDependenciesView.EndUpdate;
    end;
  end;

  UnitDependenciesView.Show;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByEnum(nmiwUnitDependenciesName);
  ALayout.Apply;
  if not WasVisible then
    UnitDependenciesView.ShowOnTop;
end;

procedure TMainIDE.DoViewUnitInfo;
var ActiveSrcEdit:TSourceEditor;
  ActiveUnitInfo:TUnitInfo;
  ShortUnitName, AFilename, FileDir: string;
  ClearIncludedByFile: boolean;
  DlgResult: TModalResult;
begin
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then exit;
  ShortUnitName:=ActiveSrcEdit.PageName;
  AFilename:=ActiveUnitInfo.Filename;
  FileDir:=ExtractFilePath(AFilename);
  DlgResult:=ShowUnitInfoDlg(ShortUnitName,
    LazSyntaxHighlighterNames[ActiveUnitInfo.SyntaxHighlighter],
    ActiveUnitInfo.IsPartOfProject, length(ActiveSrcEdit.Source.Text),
    ActiveSrcEdit.Source.Count,
    AFilename,
    ActiveUnitInfo.Source.LastIncludedByFile,
    ClearIncludedByFile,
    TrimSearchPath(CodeToolBoss.GetUnitPathForDirectory(FileDir),FileDir),
    TrimSearchPath(CodeToolBoss.GetIncludePathForDirectory(FileDir),FileDir),
    TrimSearchPath(CodeToolBoss.GetCompleteSrcPathForDirectory(FileDir),FileDir)
    );
  if ClearIncludedByFile then
    ActiveUnitInfo.Source.LastIncludedByFile:='';
  if (DlgResult=mrYes) and (ActiveUnitInfo.Source.LastIncludedByFile<>'') then
    DoGotoIncludeDirective;
end;

procedure TMainIDE.DoShowCodeExplorer;
begin
  if CodeExplorerView=nil then begin
    CodeExplorerView:=TCodeExplorerView.Create(OwningComponent);
    CodeExplorerView.OnGetCodeTree:=@OnCodeExplorerGetCodeTree;
    CodeExplorerView.OnGetDirectivesTree:=@OnCodeExplorerGetDirectivesTree;
    CodeExplorerView.OnJumpToCode:=@OnCodeExplorerJumpToCode;
  end;

  EnvironmentOptions.IDEWindowLayoutList.ItemByEnum(nmiwCodeExplorerName).Apply;
  CodeExplorerView.ShowOnTop;
  CodeExplorerView.Refresh(true);
end;

procedure TMainIDE.DoShowCodeBrowser;
begin
  if CodeBrowserView=nil then begin
    CodeBrowserView:=TCodeBrowserView.Create(OwningComponent);
  end;

  CodeBrowserView.ShowOnTop;
end;

procedure TMainIDE.DoShowComponentList;
begin
  if not Assigned(ComponentListForm)
  then ComponentListForm := TComponentListForm.Create(Self);
  ComponentListForm.Show;
end;

procedure TMainIDE.DoShowLazDoc;
begin
  SourceNotebook.ShowLazDoc;
end;

function TMainIDE.CreateNewUniqueFilename(const Prefix, Ext: string;
  NewOwner: TObject; Flags: TSearchIDEFileFlags; TryWithoutNumber: boolean): string;

  function FileIsUnique(const ShortFilename: string): boolean;
  begin
    Result:=false;

    // search in NewOwner
    if NewOwner<>nil then begin
      if (NewOwner is TProject) then begin
        if TProject(NewOwner).SearchFile(ShortFilename,Flags)<>nil then exit;
      end;
    end;

    // search in all packages
    if PkgBoss.SearchFile(ShortFilename,Flags,NewOwner)<>nil then exit;

    // search in current project
    if (NewOwner<>Project1)
    and (Project1.SearchFile(ShortFilename,Flags)<>nil) then exit;

    // search file in all loaded projects
    if (siffCheckAllProjects in Flags) then begin
    end;

    Result:=true;
  end;

var
  i: Integer;
  WorkingPrefix: String;
begin
  if TryWithoutNumber then begin
    Result:=Prefix+Ext;
    if FileIsUnique(Result) then exit;
  end;
  // remove number at end of Prefix
  WorkingPrefix:=ChompEndNumber(Prefix);
  i:=0;
  repeat
    inc(i);
    Result:=WorkingPrefix+IntToStr(i)+Ext;
  until FileIsUnique(Result);
end;

function TMainIDE.LoadIDECodeBuffer(var ACodeBuffer: TCodeBuffer;
  const AFilename: string; Flags: TLoadBufferFlags): TModalResult;
begin
  if Project1.UnitInfoWithFilename(AFilename,[pfsfOnlyEditorFiles])<>nil then
    Exclude(Flags,lbfUpdateFromDisk);
  Result:=LoadCodeBuffer(ACodeBuffer,AFilename,Flags);
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
      p,c: Integer;
      PasExt: TPascalExtType;
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
        // FPC searches first lowercase, then keeping case, then uppercase
        case c of
          0: TempFile:=LowerCase(FName);
          1: TempFile:=FName;
          2: TempFile:=UpperCase(FName);
        end;
        if ExtractFileExt(TempFile)='' then begin
          for PasExt:=Low(TPascalExtType) to High(TPascalExtType) do begin
            Ext:=PascalExtension[PasExt];
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
  IsIncludeDirective:=false;
  FName:=GetFilenameAtRowCol(ActiveSrcEdit.EditorComponent.LogicalCaretXY,
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
    Result:=mrOk;
    InputHistories.SetFileDialogSettingsInitialDir(ExtractFilePath(FName));
    if DoOpenEditorFile(FName,-1,[ofAddToRecent])=mrOk then begin
      // success
    end;
  end;
end;

function TMainIDE.DoOpenFileAndJumpToIdentifier(const AFilename,
  AnIdentifier: string; PageIndex: integer; Flags: TOpenFlags): TModalResult;
var
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  Result:=DoOpenEditorFile(AFilename, PageIndex, Flags);
  if Result<>mrOk then exit;
  Result:=mrCancel;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  if CodeToolBoss.FindDeclarationInInterface(ActiveUnitInfo.Source,
    AnIdentifier,NewSource, NewX, NewY, NewTopLine)
  then begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
                    NewSource, NewX, NewY, NewTopLine, true);
    Result:=mrOk;
  end else
    DoJumpToCodeToolBossError;
end;

function TMainIDE.DoOpenFileAndJumpToPos(const AFilename: string;
  const CursorPosition: TPoint; TopLine: integer; PageIndex: integer;
  Flags: TOpenFlags): TModalResult;
var
  ActiveUnitInfo, OldActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit, OldActiveSrcEdit: TSourceEditor;
begin
  GetCurrentUnit(OldActiveSrcEdit,OldActiveUnitInfo);
  Result:=DoOpenEditorFile(AFilename, PageIndex, Flags);
  if Result<>mrOk then exit;
  GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if ActiveUnitInfo<>nil then begin
    DoJumpToCodePos(OldActiveSrcEdit, OldActiveUnitInfo,
                    ActiveUnitInfo.Source,
                    CursorPosition.X, CursorPosition.Y, TopLine, true);
    Result:=mrOk;
  end else begin
    Result:=mrCancel;
  end;
end;

function TMainIDE.DoRevertEditorFile(const Filename: string): TModalResult;
var
  AnUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
  if (Project1<>nil) then begin
    AnUnitInfo:=Project1.UnitInfoWithFilename(Filename,[]);
    if (AnUnitInfo<>nil) and (AnUnitInfo.EditorIndex>=0) then
      Result:=DoOpenEditorFile(AnUnitInfo.Filename,AnUnitInfo.EditorIndex,
                               [ofRevert]);
  end;
end;

function TMainIDE.DoNewProject(ProjectDesc: TProjectDescriptor):TModalResult;
var i:integer;
Begin
  DebugLn('TMainIDE.DoNewProject A');

  // init the descriptor (it can now ask the user for options)
  Result:=ProjectDesc.InitDescriptor;
  if Result<>mrOk then exit;

  // invalidate cached substituted macros
  IncreaseCompilerParseStamp;

  // close current project first
  If Project1<>nil then begin
    if SomethingOfProjectIsModified then begin
      Result:=MessageDlg(lisProjectChanged, Format(lisSaveChangesToProject,
       [Project1.Title]),
        mtconfirmation, [mbYes, mbNo, mbAbort], 0);
      if Result=mrYes then begin
        Result:=DoSaveProject([]);
        if Result=mrAbort then exit;
      end else if Result in [mrCancel,mrAbort] then
        exit;
    end;
    Result:=DoCloseProject;
    if Result=mrAbort then exit;
  end;

  // create a virtual project (i.e. unsaved and without real project directory)

  // switch codetools to virtual project directory
  CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
    VirtualDirectory;

  // create new project (TProject will automatically create the mainunit)

  Project1:=CreateProjectObject(ProjectDesc,ProjectDescriptorProgram);
  Project1.BeginUpdate(true);
  try
    Project1.CompilerOptions.CompilerPath:='$(CompPath)';
    UpdateCaption;
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;

    // add and load default required packages
    PkgBoss.AddDefaultDependencies(Project1);

    if ProjectDesc.CreateStartFiles(Project1)<>mrOk then begin
      debugln('TMainIDE.DoNewProject ProjectDesc.CreateStartFiles failed');
    end;

    // rebuild codetools defines
    MainBuildBoss.RescanCompilerDefines(true);
    // (i.e. remove old project specific things and create new)
    IncreaseCompilerParseStamp;
    Project1.DefineTemplates.AllChanged;
    Project1.DefineTemplates.Active:=true;
  finally
    Project1.EndUpdate;
  end;

  // set all modified to false
  for i:=0 to Project1.UnitCount-1 do
    Project1.Units[i].ClearModifieds;
  Project1.Modified:=false;

  //DebugLn('TMainIDE.DoNewProject end ');
  Result:=mrOk;
end;

function TMainIDE.DoSaveProject(Flags: TSaveFlags):TModalResult;
var
  MainUnitSrcEdit: TSourceEditor;
  MainUnitInfo: TUnitInfo;
  i: integer;
  DestFilename: string;
  SkipSavingMainSource: Boolean;
  AnUnitInfo: TUnitInfo;
  SaveFileFlags: TSaveFlags;
begin
  Result:=mrCancel;
  if not (ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  SaveSourceEditorChangesToCodeCache(-1);
  SkipSavingMainSource:=false;


  {$IFDEF IDE_DEBUG}
  DebugLn('TMainIDE.DoSaveProject A SaveAs=',dbgs(sfSaveAs in Flags),' SaveToTestDir=',dbgs(sfSaveToTestDir in Flags),' ProjectInfoFile=',Project1.ProjectInfoFile);
  {$ENDIF}

  if DoCheckFilesOnDisk(true) in [mrCancel,mrAbort] then exit;

  if (not Project1.IsVirtual) or (not (sfDoNotSaveVirtualFiles in Flags)) then
  begin
    // check that all new units are saved first to get valid filenames
    // (this can alter the mainunit: e.g. used unit names)
    for i:=0 to Project1.UnitCount-1 do begin
      AnUnitInfo:=Project1.Units[i];
      if (AnUnitInfo.Loaded) and (AnUnitInfo.IsVirtual)
      and (Project1.MainUnitID<>i) then begin
        SaveFileFlags:=[sfSaveAs,sfProjectSaving]
                       +[sfCheckAmbiguousFiles]*Flags;
        if sfSaveToTestDir in Flags then begin
          if AnUnitInfo.IsPartOfProject or AnUnitInfo.IsVirtual then
            Include(SaveFileFlags,sfSaveToTestDir);
        end;
        Result:=DoSaveEditorFile(AnUnitInfo.EditorIndex,SaveFileFlags);
        if (Result=mrAbort) or (Result=mrCancel) then exit;
      end;
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

  if Project1.IsVirtual
  and (not (sfDoNotSaveVirtualFiles in Flags)) then
    Include(Flags,sfSaveAs);
  if ([sfSaveAs,sfSaveToTestDir]*Flags=[sfSaveAs]) then begin
    // let user choose a filename
    Result:=DoShowSaveProjectAsDialog;
    if Result<>mrOk then exit;
  end;

  // update HasResources information
  DoUpdateProjectResourceInfo;

  // save project info file
  if (not (sfSaveToTestDir in Flags))
  and (not Project1.IsVirtual) then begin
    Result:=Project1.WriteProject([],'');
    if Result=mrAbort then exit;
    EnvironmentOptions.LastSavedProjectFile:=Project1.ProjectInfoFile;
    IDEProtocolOpts.LastProjectLoadingCrashed := False;
    AddRecentProjectFileToEnvironment(Project1.ProjectInfoFile);
    SaveIncludeLinks;
    UpdateCaption;
    if Result=mrAbort then exit;
  end;

  // save main source
  if (MainUnitInfo<>nil) and (not (sfDoNotSaveVirtualFiles in flags)) then begin
    if MainUnitInfo.Loaded then begin
      // loaded in source editor
      Result:=DoSaveEditorFile(MainUnitInfo.EditorIndex,
               [sfProjectSaving]+[sfSaveToTestDir,sfCheckAmbiguousFiles]*Flags);
      if Result=mrAbort then exit;
    end else begin
      // not loaded in source editor (hidden)
      if not (sfSaveToTestDir in Flags) then begin
        DestFilename:=MainUnitInfo.Filename;
        if not MainUnitInfo.NeedsSaveToDisk then
          SkipSavingMainSource:=true;
      end else
        DestFilename:=MainBuildBoss.GetTestUnitFilename(MainUnitInfo);
      if (not SkipSavingMainSource) and (MainUnitInfo.Source<>nil) then begin
        Result:=SaveCodeBufferToFile(MainUnitInfo.Source, DestFilename);
        if Result=mrAbort then exit;
      end;
    end;
    // clear modified flags
    if not (sfSaveToTestDir in Flags) then begin
      if (Result=mrOk) then begin
        if MainUnitInfo<>nil then MainUnitInfo.ClearModifieds;
        if MainUnitSrcEdit<>nil then MainUnitSrcEdit.Modified:=false;
      end;
    end;
  end;

  // save all editor files
  if (SourceNoteBook.Notebook<>nil) then begin
    for i:=0 to SourceNoteBook.Notebook.PageCount-1 do begin
      if (Project1.MainUnitID<0)
      or (Project1.MainUnitInfo.EditorIndex<>i) then begin
        SaveFileFlags:=[sfProjectSaving]
                       +Flags*[sfCheckAmbiguousFiles];
        AnUnitInfo:=Project1.UnitWithEditorIndex(i);
        if AnUnitInfo.IsVirtual then begin
          if (sfSaveToTestDir in Flags) then
            Include(SaveFileFlags,sfSaveToTestDir)
          else
            continue;
        end;
        Result:=DoSaveEditorFile(i,SaveFileFlags);
        if Result=mrAbort then exit;
      end;
    end;
  end;

  // update all lrs files
  DoUpdateProjectAutomaticFiles;
  
  // everything went well => clear all modified flags
  Project1.ClearModifieds(true);

  // update menu and buttons state
  UpdateSaveMenuItemsAndButtons(true);

  DebugLn('TMainIDE.DoSaveProject End');
  Result:=mrOk;
end;

function TMainIDE.DoCloseProject: TModalResult;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoCloseProject A');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject A');{$ENDIF}
  Result:=DebugBoss.DoStopProject;
  if Result<>mrOk then begin
    debugln('TMainIDE.DoCloseProject DebugBoss.DoStopProject failed');
    exit;
  end;
  // close all loaded files
  while SourceNotebook.Notebook<>nil do begin
    Result:=DoCloseEditorFile(SourceNotebook.Notebook.PageCount-1,
                              [cfProjectClosing]);
    if Result=mrAbort then exit;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject B');{$ENDIF}
  IncreaseCompilerParseStamp;
  // close Project
  if ProjInspector<>nil then ProjInspector.LazProject:=nil;
  FreeThenNil(Project1);
  if IDEMessagesWindow<>nil then IDEMessagesWindow.Clear;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoCloseProject C');{$ENDIF}
  Result:=mrOk;
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoCloseProject end ',CodeToolBoss.ConsistencyCheck);
  {$ENDIF}
end;

function TMainIDE.DoOpenProjectFile(AFileName: string;
  Flags: TOpenFlags): TModalResult;
var
  Ext,AText,ACaption: string;
  LowestEditorIndex,LowestUnitIndex,LastEditorIndex,i: integer;
  NewBuf: TCodeBuffer;
  LastDesigner: TDesigner;
  AnUnitInfo: TUnitInfo;
  FileReadable: Boolean;
begin
  // close the old project
  if SomethingOfProjectIsModified then begin
    case MessageDlg(lisProjectChanged, Format(lisSaveChangesToProject, [Project1.Title]),
      mtconfirmation,[mbYes, mbNo, mbCancel],0) of
      mrYes: if DoSaveProject([])=mrAbort then begin
          Result:=mrAbort;
          exit;
        end;
      mrNo:;//nothing;
      mrCancel:exit;
    end;
  end;
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoOpenProjectFile A "'+AFileName+'"');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile A');{$ENDIF}
  Result:=mrCancel;
  if ExtractFileNameOnly(AFileName)='' then exit;
  //debugln('TMainIDE.DoOpenProjectFile A1 "'+AFileName+'"');
  AFilename:=ExpandFileName(TrimFilename(AFilename));
  //debugln('TMainIDE.DoOpenProjectFile A2 "'+AFileName+'"');
  if not FilenameIsAbsolute(AFilename) then
    RaiseException('TMainIDE.DoOpenProjectFile: buggy ExpandFileName');
  Ext:=lowercase(ExtractFileExt(AFilename));

  // check if file exists
  if not FileExists(AFilename) then begin
    ACaption:=lisFileNotFound;
    AText:=Format(lisPkgMangFileNotFound, ['"', AFilename, '"']);
    Result:=MessageDlg(ACaption, AText, mtError, [mbAbort], 0);
    exit;
  end;

  // if there is a project info file, load that instead
  if (Ext<>'.lpi') and (FileExists(ChangeFileExt(AFileName,'.lpi'))) then begin
    // load instead of program file the project info file
    AFileName:=ChangeFileExt(AFileName,'.lpi');
    Ext:='.lpi';
  end;

  if (not FileIsText(AFilename,FileReadable)) and FileReadable then begin
    ACaption:=lisFileNotText;
    AText:=Format(lisFileDoesNotLookLikeATextFileOpenItAnyway, ['"', AFilename,
      '"', #13, #13]);
    Result:=MessageDlg(ACaption, AText, mtConfirmation, [mbYes, mbAbort], 0);
    if Result=mrAbort then exit;
  end;
  if not FileReadable then begin
    Result:=QuestionDlg('Unable to read file',
      'Unable to read file "'+AFilename+'".',
      mtError,[mrCancel,'Skip file',mrAbort,'Abort all loading'],0);
    exit;
  end;

  if ofAddToRecent in Flags then
    AddRecentProjectFileToEnvironment(AFileName);

  Result:=DoCloseProject;
  if Result=mrAbort then exit;

  // create a new project
  {$IFDEF IDE_VERBOSE}
  writeln('TMainIDE.DoOpenProjectFile B');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile B');{$ENDIF}
  Project1:=CreateProjectObject(ProjectDescriptorProgram,
                                ProjectDescriptorProgram);
  LastEditorIndex:=-1;
  try
    Project1.BeginUpdate(true);
    try
      if ProjInspector<>nil then ProjInspector.LazProject:=Project1;

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
      Result:=LoadCodeBuffer(NewBuf,Project1.MainFilename,
                             [lbfUpdateFromDisk,lbfRevert,lbfCheckIfText]);
      if (Result<>mrOk) then exit;
      Project1.MainUnitInfo.Source:=NewBuf;
    end;
    {$IFDEF IDE_DEBUG}
    writeln('TMainIDE.DoOpenProjectFile C');
    {$ENDIF}
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile C');{$ENDIF}
    IncreaseCompilerParseStamp;

    // restore files
    repeat
      // find the unit which was loaded last time and has the lowest editor index
      // of all not opened units
      LowestUnitIndex:=-1;
      LowestEditorIndex:=-1;
      for i:=0 to Project1.UnitCount-1 do begin
        AnUnitInfo:=Project1.Units[i];
        if (AnUnitInfo.Loaded)
        and (SourceNotebook.FindSourceEditorWithFilename(AnUnitInfo.Filename)=nil)
        then begin
          if (AnUnitInfo.EditorIndex>LastEditorIndex)
          and ((AnUnitInfo.EditorIndex<LowestEditorIndex)
               or (LowestEditorIndex<0)) then
          begin
            LowestEditorIndex:=AnUnitInfo.EditorIndex;
            LowestUnitIndex:=i;
          end;
        end;
      end;
      if LowestEditorIndex<0 then break;

      // reopen file
      AnUnitInfo:=Project1.Units[LowestUnitIndex];
      if (not AnUnitInfo.IsPartOfProject)
      and (not FileExistsCached(AnUnitInfo.Filename)) then begin
        // this file does not exist, but is not important => silently ignore
      end
      else begin
        // reopen file
        Result:=DoOpenEditorFile(AnUnitInfo.Filename,-1,
                      [ofProjectLoading,ofMultiOpen,ofOnlyIfExists]);
        if Result=mrAbort then begin
          exit;
        end;
      end;
      if ((AnUnitInfo.Filename<>'')
      and (SourceNotebook.FindSourceEditorWithFilename(AnUnitInfo.Filename)<>nil))
      then begin
        // open source was successful (at least the source)
        if Project1.ActiveEditorIndexAtStart=LowestEditorIndex then
          Project1.ActiveEditorIndexAtStart:=SourceNoteBook.Notebook.PageIndex;
        LastEditorIndex:=LowestEditorIndex;
      end else begin
        // failed to open entirely -> mark as unloaded, so that next time
        // it will not be tried again
        AnUnitInfo.EditorIndex:=-1;
        AnUnitInfo.Loaded:=false;
        if Project1.ActiveEditorIndexAtStart=LowestEditorIndex then
          Project1.ActiveEditorIndexAtStart:=-1;
      end;
    until LowestEditorIndex<0;
    Result:=mrCancel;
    {$IFDEF IDE_DEBUG}
    writeln('TMainIDE.DoOpenProjectFile D');
    {$ENDIF}

    // set active editor source editor
    if (SourceNoteBook.Notebook<>nil) and (Project1.ActiveEditorIndexAtStart>=0)
    and (Project1.ActiveEditorIndexAtStart<SourceNoteBook.Notebook.PageCount)
    then
      SourceNoteBook.Notebook.PageIndex:=Project1.ActiveEditorIndexAtStart;

    // select a form (object inspector, formeditor, control selection)
    if FLastFormActivated<>nil then begin
      LastDesigner:=TDesigner(FLastFormActivated.Designer);
      LastDesigner.SelectOnlyThisComponent(LastDesigner.LookupRoot);
    end;

    // set all modified to false
    Project1.ClearModifieds(true);

    IncreaseCompilerParseStamp;
    IDEProtocolOpts.LastProjectLoadingCrashed := False;
    Result:=mrOk;
  finally
    if (Result<>mrOk) and (Project1<>nil) then begin
      // mark all files, that are left to open as unloaded:
      for i:=0 to Project1.UnitCount-1 do begin
        AnUnitInfo:=Project1.Units[i];
        if AnUnitInfo.Loaded
        and (AnUnitInfo.EditorIndex>LastEditorIndex) then begin
          AnUnitInfo.Loaded:=false;
          AnUnitInfo.EditorIndex:=-1;
          Project1.ActiveEditorIndexAtStart:=-1;
        end;
      end;
    end;
  end;
  {$IFDEF IDE_VERBOSE}
  debugln('TMainIDE.DoOpenProjectFile end  CodeToolBoss.ConsistencyCheck=',IntToStr(CodeToolBoss.ConsistencyCheck));
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoOpenProjectFile end');{$ENDIF}
end;

function TMainIDE.DoPublishProject(Flags: TSaveFlags;
  ShowDialog: boolean): TModalResult;
begin
  // show the publish project dialog
  if ShowDialog then begin
    Result:=ShowPublishProjectDialog(Project1.PublishOptions);
    Project1.Modified:=Project1.PublishOptions.Modified;
    if Result<>mrOk then exit;
    IncreaseCompilerParseStamp;
  end;

  //debugln('TMainIDE.DoPublishProject A');
  // save project
  Result:=DoSaveProject(Flags);
  if Result<>mrOk then exit;

  // publish project
  //debugln('TMainIDE.DoPublishProject B');
  Result:=DoPublishModule(Project1.PublishOptions,Project1.ProjectDirectory,
                          MainBuildBoss.GetProjectPublishDir);
end;

function TMainIDE.DoImExportCompilerOptions(Sender: TObject): TModalResult;
var
  CompOptsDialog: TfrmCompilerOptions;
  ImExportResult: TImExportCompOptsResult;
  Filename: string;
begin
  Result:=mrOk;
  if not (Sender is TfrmCompilerOptions) then
    RaiseException('TMainIDE.OnCompilerOptionsImExport');
  CompOptsDialog:=TfrmCompilerOptions(Sender);
  ImExportResult:=ShowImExportCompilerOptionsDialog(
                                          CompOptsDialog.CompilerOpts,Filename);
  if (ImExportResult=iecorCancel) or (Filename='') then exit;
  if ImExportResult=iecorImport then
    Result:=DoImportCompilerOptions(CompOptsDialog,CompOptsDialog.CompilerOpts,
                                    Filename)
  else if ImExportResult=iecorExport then
    Result:=DoExportCompilerOptions(CompOptsDialog,CompOptsDialog.CompilerOpts,
                                    Filename);
end;

function TMainIDE.DoShowProjectInspector: TModalResult;
begin
  if ProjInspector=nil then begin
    ProjInspector:=TProjectInspectorForm.Create(OwningComponent);
    ProjInspector.OnOpen:=@ProjInspectorOpen;
    ProjInspector.OnShowOptions:=@mnuProjectOptionsClicked;
    ProjInspector.OnAddUnitToProject:=@ProjInspectorAddUnitToProject;
    ProjInspector.OnAddDependency:=@PkgBoss.OnProjectInspectorAddDependency;
    ProjInspector.OnRemoveFile:=@ProjInspectorRemoveFile;
    ProjInspector.OnRemoveDependency:=
                                    @PkgBoss.OnProjectInspectorRemoveDependency;
    ProjInspector.OnReAddDependency:=
                                    @PkgBoss.OnProjectInspectorReAddDependency;

    ProjInspector.LazProject:=Project1;
  end;
  ProjInspector.ShowOnTop;
  Result:=mrOk;
end;

function TMainIDE.DoCreateProjectForProgram(
  ProgramBuf: TCodeBuffer): TModalResult;
var
  NewProjectDesc: TProjectDescriptor;
  MainUnitInfo: TUnitInfo;
begin
  {$IFDEF IDE_VERBOSE}
  writeln('[TMainIDE.DoCreateProjectForProgram] A ',ProgramBuf.Filename);
  {$ENDIF}
  Result:=DoSaveProjectIfChanged;
  if Result=mrAbort then exit;

  // let user choose the program type
  NewProjectDesc:=nil;
  if ChooseNewProject(NewProjectDesc)<>mrOk then exit;

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
  Project1:=CreateProjectObject(NewProjectDesc,ProjectDescriptorProgram);
  Project1.BeginUpdate(true);
  try
    if ProjInspector<>nil then ProjInspector.LazProject:=Project1;
    MainUnitInfo:=Project1.MainUnitInfo;
    MainUnitInfo.Source:=ProgramBuf;
    Project1.ProjectInfoFile:=ChangeFileExt(ProgramBuf.Filename,'.lpi');
    UpdateCaption;
    IncreaseCompilerParseStamp;

    // add and load default required packages
    PkgBoss.AddDefaultDependencies(Project1);
    
    Result:=DoCompleteLoadingProjectInfo;
    if Result<>mrOk then exit;
  finally
    Project1.EndUpdate;
  end;

  // show program unit
  Result:=DoOpenEditorFile(ProgramBuf.Filename,-1,
                           [ofAddToRecent,ofRegularFile]);
  if Result=mrAbort then exit;

  {$IFDEF IDE_VERBOSE}
  writeln('[TMainIDE.DoCreateProjectForProgram] END');
  {$ENDIF}
  Result:=mrOk;
end;

function TMainIDE.DoSaveProjectIfChanged: TModalResult;
begin
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
  Result:=mrOk;
end;

function TMainIDE.DoAddActiveUnitToProject: TModalResult;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  s, ShortUnitName: string;
begin
  Result:=mrCancel;
  if BeginCodeTool(ActiveSourceEditor,ActiveUnitInfo,[])
    and (ActiveUnitInfo<>nil) then begin
    if ActiveUnitInfo.IsPartOfProject=false then begin
      if not ActiveUnitInfo.IsVirtual then
        s:='"'+ActiveUnitInfo.Filename+'"'
      else
        s:='"'+ActiveSourceEditor.PageName+'"';
      if (ActiveUnitInfo.UnitName<>'')
      and (Project1.IndexOfUnitWithName(ActiveUnitInfo.UnitName,
          true,ActiveUnitInfo)>=0) then
      begin
        MessageDlg(Format(
          lisUnableToAddToProjectBecauseThereIsAlreadyAUnitWith, [s]),
          mtInformation, [mbOk], 0);
      end else begin
        if MessageDlg(Format(lisAddToProject, [s]), mtConfirmation, [mbYes,
          mbCancel], 0) in [mrOk,mrYes]
        then begin
          Result:=DoRenameUnitLowerCase(ActiveUnitInfo,true);
          if Result=mrIgnore then Result:=mrOk;
          if Result<>mrOk then begin
            debugln('TMainIDE.DoAddActiveUnitToProject A DoRenameUnitLowerCase failed ',ActiveUnitInfo.Filename);
            exit;
          end;
          ActiveUnitInfo.IsPartOfProject:=true;
          if (FilenameIsPascalUnit(ActiveUnitInfo.Filename))
          and (pfMainUnitHasUsesSectionForAllUnits in Project1.Flags)
          then begin
            ActiveUnitInfo.ReadUnitNameFromSource(false);
            ShortUnitName:=ActiveUnitInfo.CreateUnitName;
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
        s:=Format(lisTheFile, ['"', ActiveUnitInfo.Filename, '"'])
      else
        s:=Format(lisTheFile, ['"', ActiveSourceEditor.PageName, '"']);
      s:=Format(lisisAlreadyPartOfTheProject, [s]);
      MessageDlg(s,mtInformation,[mbOk],0);
    end;
  end else begin
    Result:=mrOk;
  end;
end;

function TMainIDE.DoRemoveFromProjectDialog: TModalResult;
var
  UnitList: TStringList;
  i:integer;
  AName: string;
  AnUnitInfo: TUnitInfo;
Begin
  UnitList := TStringList.Create;
  UnitList.Sorted := True;

  try
    for i := 0 to Project1.UnitCount-1 do
    begin
      AnUnitInfo:=Project1.Units[i];
      if (AnUnitInfo.IsPartOfProject) and (i<>Project1.MainUnitID) then
      begin
        AName := Project1.RemoveProjectPathFromFilename(AnUnitInfo.FileName);
        UnitList.AddObject(AName, TViewUnitsEntry.Create(AName,i,false));
      end;
    end;
    if ShowViewUnitsDlg(UnitList, true, lisRemoveFromProject) = mrOk then
    begin
      { This is where we check what the user selected. }
      for i:=0 to UnitList.Count-1 do
      begin
        if TViewUnitsEntry(UnitList.Objects[i]).Selected then
        begin
          AnUnitInfo:=Project1.Units[TViewUnitsEntry(UnitList.Objects[i]).ID];
          AnUnitInfo.IsPartOfProject := false;
          if (Project1.MainUnitID >= 0) and
             (pfMainUnitHasUsesSectionForAllUnits in Project1.Flags) then
          begin
            if (AnUnitInfo.UnitName <> '') then
            begin
              if CodeToolBoss.RemoveUnitFromAllUsesSections(
                Project1.MainUnitInfo.Source, AnUnitInfo.UnitName)
              then
                Project1.MainUnitInfo.Modified := true;
            end;
            if (AnUnitInfo.ComponentName <> '') then
            begin
              Project1.RemoveCreateFormFromProjectFile(
                  'T' + AnUnitInfo.ComponentName, AnUnitInfo.ComponentName);
            end;
          end;
        end;
      end;  { for }
    end;  { if ShowViewUnitsDlg.. }
  finally
    for i := 0 to UnitList.Count-1 do
      TViewUnitsEntry(UnitList.Objects[i]).Free;
    UnitList.Free;
  end;
  Result := mrOk;
end;

function TMainIDE.DoWarnAmbiguousFiles: TModalResult;
var
  AnUnitInfo: TUnitInfo;
  i: integer;
  DestFilename: string;
begin
  for i:=0 to Project1.UnitCount-1 do begin
    AnUnitInfo:=Project1.Units[i];
    if (AnUnitInfo.IsPartOfProject) and (not AnUnitInfo.IsVirtual) then begin
      DestFilename:=MainBuildBoss.GetTargetUnitFilename(AnUnitInfo);
      Result:=MainBuildBoss.CheckAmbiguousSources(DestFilename,true);
      if Result<>mrOk then exit;
    end;
  end;
  Result:=mrOk;
end;

procedure TMainIDE.DoUpdateProjectResourceInfo;
var
  AnUnitInfo: TUnitInfo;
  LFMFilename: String;
begin
  AnUnitInfo:=Project1.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    if (not AnUnitInfo.HasResources)
    and (not AnUnitInfo.IsVirtual) and FilenameIsPascalUnit(AnUnitInfo.Filename)
    then begin
      LFMFilename:=ChangeFileExt(AnUnitInfo.Filename,'.lfm');
      if FileExists(LFMFilename) then begin
        AnUnitInfo.HasResources:=true;
        AnUnitInfo.ResourceFileName:=ChangeFileExt(LFMFilename,'.lrs');
      end else begin
        AnUnitInfo.HasResources:=false;
      end;
    end;
    if AnUnitInfo.HasResources and (not AnUnitInfo.IsVirtual) then begin
      if (AnUnitInfo.ResourceFileName='')
      or (not FilenameIsAbsolute(AnUnitInfo.ResourceFileName)) then begin
        AnUnitInfo.ResourceFileName:=ChangeFileExt(AnUnitInfo.Filename,'.lrs');
      end;
    end;
    AnUnitInfo:=AnUnitInfo.NextPartOfProject;
  end;
end;

function TMainIDE.DoUpdateProjectAutomaticFiles: TModalResult;
var
  AnUnitInfo: TUnitInfo;
begin
  AnUnitInfo:=Project1.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    if AnUnitInfo.HasResources then begin
      Result:=DoUpdateLRSFromLFM(AnUnitInfo.ResourceFileName);
      if Result=mrIgnore then Result:=mrOk;
      if Result<>mrOk then exit;
    end;
    AnUnitInfo:=AnUnitInfo.NextPartOfProject;
  end;
end;

function TMainIDE.DoSaveForBuild: TModalResult;
begin
  Result:=mrCancel;
  if not (ToolStatus in [itNone,itDebugger]) then begin
    {$IFDEF VerboseSaveForBuild}
    DebugLn('TMainIDE.DoSaveForBuild ToolStatus disallows it');
    {$ENDIF}
    Result:=mrAbort;
    exit;
  end;
  if Project1=nil then Begin
    MessageDlg(lisCreateAProjectFirst, mterror, [mbok], 0);
    Exit;
  end;

  // save all files
  {$IFDEF VerboseSaveForBuild}
  DebugLn('TMainIDE.DoSaveForBuild Project1.IsVirtual=',dbgs(Project1.IsVirtual));
  {$ENDIF}
  if not Project1.IsVirtual then
    Result:=DoSaveAll([sfCheckAmbiguousFiles])
  else
    Result:=DoSaveProjectToTestDirectory([sfSaveNonProjectFiles]);
  if Result<>mrOk then begin
    {$IFDEF VerboseSaveForBuild}
    DebugLn('TMainIDE.DoSaveForBuild project saving failed');
    {$ENDIF}
    exit;
  end;

  Result:=PkgBoss.DoSaveAllPackages([]);
end;

function TMainIDE.DoCheckIfProjectNeedsCompilation(AProject: TProject;
  const CompilerFilename, CompilerParams, SrcFilename: string): TModalResult;
var
  StateFilename: String;
  StateFileAge: LongInt;
  AnUnitInfo: TUnitInfo;
begin
  // check state file
  StateFilename:=AProject.GetStateFilename;
  Result:=AProject.LoadStateFile(false);
  if Result<>mrOk then exit;
  if not (lpsfStateFileLoaded in AProject.StateFlags) then begin
    DebugLn('TMainIDE.CheckIfPackageNeedsCompilation  No state file for ',AProject.IDAsString);
    Result:=mrYes;
    exit;
  end;

  StateFileAge:=FileAge(StateFilename);

  // check main source file
  if FileExists(SrcFilename) and (StateFileAge<FileAge(SrcFilename)) then
  begin
    DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  SrcFile outdated ',AProject.IDAsString);
    Result:=mrYes;
    exit;
  end;

  // check all required packages
  Result:=PackageGraph.CheckIfDependenciesNeedCompilation(
                                 AProject.FirstRequiredDependency,StateFileAge);
  if Result<>mrNo then exit;

  Result:=mrYes;

  // check compiler and params
  if CompilerFilename<>AProject.LastCompilerFilename then begin
    DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Compiler filename changed for ',AProject.IDAsString);
    DebugLn('  Old="',AProject.LastCompilerFilename,'"');
    DebugLn('  Now="',CompilerFilename,'"');
    exit;
  end;
  if not FileExists(CompilerFilename) then begin
    DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Compiler filename not found for ',AProject.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit;
  end;
  if FileAge(CompilerFilename)<>AProject.LastCompilerFileDate then begin
    DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Compiler file changed for ',AProject.IDAsString);
    DebugLn('  File="',CompilerFilename,'"');
    exit;
  end;
  if CompilerParams<>AProject.LastCompilerParams then begin
    DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Compiler params changed for ',AProject.IDAsString);
    DebugLn('  Old="',AProject.LastCompilerParams,'"');
    DebugLn('  Now="',CompilerParams,'"');
    exit;
  end;

  // check project files
  AnUnitInfo:=AProject.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    if FileExists(AnUnitInfo.Filename)
    and (StateFileAge<FileAge(AnUnitInfo.Filename)) then begin
      DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Src has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      exit;
    end;
    AnUnitInfo:=AnUnitInfo.NextPartOfProject;
  end;

  // check all open editor files (maybe the user forgot to add them to the project)
  AnUnitInfo:=AProject.FirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    if (not AnUnitInfo.IsPartOfProject)
    and FileExists(AnUnitInfo.Filename)
    and (StateFileAge<FileAge(AnUnitInfo.Filename)) then begin
      DebugLn('TMainIDE.CheckIfProjectNeedsCompilation  Src has changed ',AProject.IDAsString,' ',AnUnitInfo.Filename);
      exit;
    end;
    AnUnitInfo:=AnUnitInfo.NextUnitWithEditorIndex;
  end;

  Result:=mrNo;
end;

function TMainIDE.DoSaveProjectToTestDirectory(Flags: TSaveFlags): TModalResult;
begin
  Result:=mrCancel;
  if (EnvironmentOptions.TestBuildDirectory='')
  or (not DirPathExists(EnvironmentOptions.TestBuildDirectory)) then begin
    if (EnvironmentOptions.TestBuildDirectory<>'') then begin
      MessageDlg(Format(lisTheTestDirectoryCouldNotBeFoundSeeEnvironmentOpt, [
        #13, '"', EnvironmentOptions.TestBuildDirectory, '"', #13]), mtError, [
        mbCancel], 0);
      Result:=mrCancel;
      exit;
    end;
    Result:=MessageDlg(lisBuildNewProject,
       Format(lisTheProjectMustBeSavedBeforeBuildingIfYouSetTheTest, [#13, #13,
         #13]), mtInformation, [mbYes, mbNo], 0);
    if Result<>mrYes then exit;
    Result:=DoSaveAll([sfCheckAmbiguousFiles]);
    exit;
  end;
  Result:=DoSaveProject([sfSaveToTestDir,sfCheckAmbiguousFiles]+Flags);
end;

function TMainIDE.DoShowToDoList: TModalResult;
begin
  if not Assigned(frmToDo) then begin
    frmToDo:=TfrmToDo.Create(OwningComponent);
    frmToDo.OnOpenFile:=@ViewProjectTodosOpenFile;
  end;

  frmToDo.FileName:=Project1.MainUnitInfo.Filename;

  frmToDo.ShowOnTop;
  Result:=mrOk;
end;

function TMainIDE.DoTestCompilerSettings(
  TheCompilerOptions: TCompilerOptions): TModalResult;
begin
  Result:=mrCancel;
  if (Project1=nil) or (ToolStatus<>itNone) then exit;

  // change tool status
  CheckCompilerOptsDlg:=TCheckCompilerOptsDlg.Create(nil);
  try
    CheckCompilerOptsDlg.Options:=TheCompilerOptions;
    CheckCompilerOptsDlg.MacroList:=GlobalMacroList;
    Result:=CheckCompilerOptsDlg.ShowModal;
  finally
    FreeThenNil(CheckCompilerOptsDlg);
  end;
end;

function TMainIDE.QuitIDE: boolean;
begin
  Result:=true;
  MainIDEBar.OnCloseQuery(Self, Result);
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.QuitIDE 1');
  {$ENDIF}
  if Result then MainIDEBar.Close;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.QuitIDE 2');
  {$ENDIF}
end;

function TMainIDE.DoBuildProject(const AReason: TCompileReason;
  Flags: TProjectBuildFlags): TModalResult;
var
  SrcFilename: string;
  ToolBefore: TProjectCompilationToolOptions;
  ToolAfter: TProjectCompilationToolOptions;
  PkgFlags: TPkgCompileFlags;
  CompilerFilename: String;
  WorkingDir: String;
  CompilerParams: String;
  Count: integer;
  VersionInfo: TProjectVersionInfo;
begin
  if Project1.MainUnitInfo=nil then begin
    // this project has not source to compile
    Result:=mrCancel;
    exit;
  end;

  Result:=PrepareForCompile;
  if Result<>mrOk then exit;

  // show messages
  MessagesView.BeginBlock;

  try
    Result:=DoSaveForBuild;
    if Result<>mrOk then exit;

    // handle versioninfo
    VersionInfo := Project1.VersionInfo;
    Result := VersionInfo.CreateRCFile(Project1.MainFilename,
      MainBuildBoss.GetTargetOS(true));

    for Count := 1 to VersionInfo.VersionInfoMessages.Count do
      MessagesView.AddMsg(Format(VersionInfo.VersionInfoMessages[Count - 1],
                                  ['"', Project1.ShortDescription, '"']), '' ,-1);
    if Result <> mrOk then exit;

    // handle manifest
    Result := Project1.XPManifest.CreateRCFile(Project1.MainFilename,
      MainBuildBoss.GetTargetOS(true));
    for Count := 1 to Project1.XPManifest.Messages.Count do
      MessagesView.AddMsg(Format(Project1.XPManifest.Messages[Count - 1],
                                  ['"', Project1.ShortDescription, '"']), '' ,-1);
    if Result <> mrOk then exit;

    // compile required packages
    if not (pbfDoNotCompileDependencies in Flags) then begin
      PkgFlags:=[pcfDoNotSaveEditorFiles];
      if pbfCompileDependenciesClean in Flags then
        Include(PkgFlags,pcfCompileDependenciesClean);
      Result:=PkgBoss.DoCompileProjectDependencies(Project1,PkgFlags);
      if Result<>mrOk then exit;
    end;

    // clear old error lines
    SourceNotebook.ClearErrorLines;

    DoArrangeSourceEditorAndMessageView(false);

    // get main source filename
    if not Project1.IsVirtual then begin
      WorkingDir:=Project1.ProjectDirectory;
      SrcFilename:=CreateRelativePath(Project1.MainUnitInfo.Filename,WorkingDir);
    end else begin
      WorkingDir:=GetTestBuildDirectory;
      SrcFilename:=MainBuildBoss.GetTestUnitFilename(Project1.MainUnitInfo);
    end;
    CompilerFilename:=Project1.GetCompilerFilename;
    //DebugLn(['TMainIDE.DoBuildProject CompilerFilename="',CompilerFilename,'" CompilerPath="',Project1.CompilerOptions.CompilerPath,'"']);
    
    CompilerParams :=
      Project1.CompilerOptions.MakeOptionsString(SrcFilename,nil,[]) + ' ' +
      PrepareCmdLineOption(SrcFilename);
    //DebugLn('TMainIDE.DoBuildProject WorkingDir="',WorkingDir,'" SrcFilename="',SrcFilename,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

    // warn for ambiguous files
    Result:=DoWarnAmbiguousFiles;
    if Result<>mrOk then exit;

    // check if build is needed (only if we will call the compiler)
    if  (AReason in Project1.CompilerOptions.CompileReasons)
    and (pbfOnlyIfNeeded in Flags)
    and (not (pfAlwaysBuild in Project1.Flags))
    then begin
      Result:=DoCheckIfProjectNeedsCompilation(Project1,
                                               CompilerFilename,CompilerParams,
                                               SrcFilename);
      if Result=mrNo then begin
        Result:=mrOk;
        exit;
      end;
      if Result<>mrYes then exit;
    end;

    // execute compilation tool 'Before'
    if not (pbfSkipTools in Flags) then begin
      ToolBefore:=TProjectCompilationToolOptions(
                                        Project1.CompilerOptions.ExecuteBefore);
      if (AReason in ToolBefore.CompileReasons) then begin
        Result:=Project1.CompilerOptions.ExecuteBefore.Execute(
                           Project1.ProjectDirectory,lisExecutingCommandBefore);
        if Result<>mrOk then exit;
      end;
    end;

    if (AReason in Project1.CompilerOptions.CompileReasons)
    and (not (pbfDoNotCompileProject in Flags)) then begin
      try
        // change tool status
        ToolStatus:=itBuilder;

        ConnectOutputFilter;

        // compile
        Result:=TheCompiler.Compile(Project1,
                                WorkingDir,CompilerFilename,CompilerParams,
                                pbfCleanCompile in Flags,pbfSkipLinking in Flags,
                                pbfSkipAssembler in Flags);
        if Result<>mrOk then begin
          DoJumpToCompilerMessage(-1,true);
          exit;
        end;
        // compilation succeded -> write state file
        Result:=Project1.SaveStateFile(CompilerFilename,CompilerParams);
        if Result<>mrOk then exit;

        // upate project .po file
        Result:=UpdateProjectPOFile(Project1);
        if Result<>mrOk then exit;

      finally
        ToolStatus:=itNone;
      end;
    end;

    // execute compilation tool 'After'
    if not (pbfSkipTools in Flags) then begin
      ToolAfter:=TProjectCompilationToolOptions(
                                         Project1.CompilerOptions.ExecuteAfter);
      // no need to check for mrOk, we are exit if it wasn't
      if (AReason in ToolAfter.CompileReasons) then begin
        Result:=Project1.CompilerOptions.ExecuteAfter.Execute(
                            Project1.ProjectDirectory,lisExecutingCommandAfter);
        if Result<>mrOk then exit;
      end;
    end;

    // add success message
    MessagesView.AddMsg(Format(lisProjectSuccessfullyBuilt, ['"',
                                        Project1.ShortDescription, '"']),'',-1);

  finally
    // check sources
    DoCheckFilesOnDisk;

    MessagesView.EndBlock;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoAbortBuild: TModalResult;
begin
  Result:=mrOk;
  if ToolStatus<>itBuilder then exit;
  TheOutputFilter.StopExecute:=true;
end;

procedure TMainIDE.DoQuickCompile;
begin
  DoBuildProject(crCompile,[pbfSkipLinking,pbfSkipTools,pbfSkipAssembler]);
end;

function TMainIDE.DoInitProjectRun: TModalResult;
var
  ProgramFilename: string;
begin
  if ToolStatus <> itNone
  then begin
    // already running so no initialization needed
    Result := mrOk;
    Exit;
  end;

  Result := mrCancel;

  // Check if we can run this project
  debugln('TMainIDE.DoInitProjectRun A ',dbgs(pfRunnable in Project1.Flags),' ',dbgs(Project1.MainUnitID));
  if (not (pfRunnable in Project1.Flags))
  or (Project1.MainUnitID < 0)
  then Exit;

  debugln('TMainIDE.DoInitProjectRun B');
  // Build project first
  if DoBuildProject(crRun,[pbfOnlyIfNeeded]) <> mrOk
  then Exit;

  // Check project build
  ProgramFilename := MainBuildBoss.GetProjectTargetFilename;
  if not FileExists(ProgramFilename)
  then begin
    MessageDlg(lisFileNotFound,
      Format(lisNoProgramFileSFound, ['"', ProgramFilename, '"']),
      mtError,[mbCancel], 0);
    Exit;
  end;

  // Setup debugger
  if not DebugBoss.InitDebugger then Exit;
  
  Result := mrOK;
  ToolStatus := itDebugger;
end;

function TMainIDE.DoRunProject: TModalResult;
begin
  DebugLn('[TMainIDE.DoRunProject] A');

  if (DoInitProjectRun <> mrOK)
  or (ToolStatus <> itDebugger)
  then begin
    Result := mrAbort;
    Exit;
  end;
  debugln('[TMainIDE.DoRunProject] B ',EnvironmentOptions.DebuggerClass);

  Result := mrCancel;

  Result := DebugBoss.RunDebugger;
//  if Result<>mrOk then exit;

  DebugLn('[TMainIDE.DoRunProject] END');
end;

function TMainIDE.SomethingOfProjectIsModified: boolean;
begin
  Result:=(Project1<>nil)
      and (Project1.SomethingModified(true,true)
           or SourceNotebook.SomethingModified);
end;

function TMainIDE.DoSaveAll(Flags: TSaveFlags): TModalResult;
var
  CurResult: TModalResult;
begin
  DebugLn('TMainIDE.DoSaveAll');
  Result:=mrOk;
  CurResult:=DoCallModalFunctionHandler(lihtOnSavingAll);
  if CurResult=mrAbort then exit(mrAbort);
  if CurResult<>mrOk then Result:=mrCancel;
  CurResult:=DoSaveProject(Flags);
  SaveEnvironment;
  SaveIncludeLinks;
  InputHistories.Save;
  if CurResult=mrAbort then exit(mrAbort);
  if CurResult<>mrOk then Result:=mrCancel;
  CurResult:=DoCallModalFunctionHandler(lihtOnSavedAll);
  if CurResult=mrAbort then exit(mrAbort);
  if CurResult<>mrOk then Result:=mrCancel;
  UpdateSaveMenuItemsAndButtons(true);
end;

procedure TMainIDE.DoRestart;

  procedure StartStarter;
  var
    StartLazProcess: TProcess;
    ExeName: string;
  begin
    StartLazProcess := TProcess.Create(nil);
    try
      // TODO: use the target directory, where the new startlazarus is
      StartLazProcess.CurrentDirectory := ExtractFileDir(ParamStr(0));
      ExeName := AppendPathDelim(StartLazProcess.CurrentDirectory) +
        'startlazarus' + GetExecutableExt;
      if not FileExists(ExeName) then begin
        IDEMessageDialog('Error',Format(lisCannotFindLazarusStarter,
                            [LineEnding, ExeName]),mtError,[mbCancel]);
        exit;
      end;
      StartLazProcess.CommandLine := format('%s --lazarus-pid=%d',
        [ExeName, GetProcessID]);
      StartLazProcess.Execute;
    finally
      StartLazProcess.Free;
    end;
  end;

var CanClose: boolean;
begin
  DebugLn(['TMainIDE.DoRestart ']);
  CanClose:=true;
  MainIDEBar.OnCloseQuery(Self, CanClose);
  if not CanClose then exit;
  MainIDEBar.Close;
  if Application.Terminated then begin
    if StartedByStartLazarus then
      ExitCode := ExitCodeRestartLazarus
    else
      StartStarter;
  end;
end;

procedure TMainIDE.DoExecuteRemoteControl;

  procedure OpenFiles(Files: TStrings);
  var
    AProjectFilename: string;
    ProjectLoaded: Boolean;
    AFilename: String;
    i: Integer;
    OpenFlags: TOpenFlags;
  begin
    if (Files=nil) or (Files.Count=0) then exit;
    ProjectLoaded:=Project1<>nil;
    DebugLn(['TMainIDE.DoExecuteRemoteControl.OpenFiles ProjectLoaded=',ProjectLoaded]);
    
    // open project
    if (Files<>nil) and (Files.Count>0) then begin
      AProjectFilename:=Files[0];
      if (CompareFileExt(AProjectFilename,'.lpr',false)=0) then
        AProjectFilename:=ChangeFileExt(AProjectFilename,'.lpi');
      if (CompareFileExt(AProjectFilename,'.lpi',false)=0) then begin
        AProjectFilename:=CleanAndExpandFilename(AProjectFilename);
        if FileExists(AProjectFilename) then begin
          DebugLn(['TMainIDE.DoExecuteRemoteControl.OpenFiles AProjectFilename="',AProjectFilename,'"']);
          Files.Delete(0);
          ProjectLoaded:=(DoOpenProjectFile(AProjectFilename,[])=mrOk);
        end;
      end;
    end;

    if not ProjectLoaded then begin
      // create new project
      DoNewProject(ProjectDescriptorApplication);
    end;

    // load the files
    if Files<>nil then begin
      for i:=0 to Files.Count-1 do begin
        AFilename:=CleanAndExpandFilename(Files.Strings[i]);
        DebugLn(['TMainIDE.DoExecuteRemoteControl.OpenFiles AFilename="',AFilename,'"']);
        if CompareFileExt(AFilename,'.lpk',false)=0 then begin
          if PkgBoss.DoOpenPackageFile(AFilename,[pofAddToRecent])=mrAbort
          then
            break;
        end else begin
          OpenFlags:=[ofAddToRecent,ofRegularFile];
          if i<Files.Count then
            Include(OpenFlags,ofMultiOpen);
          if DoOpenEditorFile(AFilename,-1,OpenFlags)=mrAbort then begin
            break;
          end;
        end;
      end;
    end;
  end;

var
  Filename: String;
  List: TStringList;
  Files: TStrings;
  i: Integer;
begin
  Filename:=GetRemoteControlFilename;
  if FileExists(Filename) then begin
    // the control file exists
    if FRemoteControlFileValid then begin
      List:=TStringList.Create;
      Files:=nil;
      try
        // load and delete the file
        try
          List.LoadFromFile(Filename);
        except
          DebugLn(['TMainIDE.DoExecuteRemoteControl reading file failed: ',Filename]);
        end;
        DeleteFile(Filename);
        FRemoteControlFileValid:=not FileExists(Filename);
        // execute
        Files:=TStringList.Create;
        for i:=0 to List.Count-1 do begin
          if CompareText(copy(List[i],1,5),'open ')=0 then
            Files.Add(copy(List[i],6,length(List[i])));
        end;
        if Files.Count>0 then begin
          OpenFiles(Files);
        end;
      finally
        List.Free;
        Files.Free;
      end;
    end else begin
      // the last time there was an error (e.g. read/delete failed)
      // do not waste time again
    end;
  end else begin
    // the control file does not exist
    // => remember the good state
    FRemoteControlFileValid:=true;
  end;
end;

//-----------------------------------------------------------------------------

function TMainIDE.DoRunExternalTool(Index: integer): TModalResult;
begin
  SourceNotebook.ClearErrorLines;
  Result:=EnvironmentOptions.ExternalTools.Run(Index,GlobalMacroList);
  DoCheckFilesOnDisk;
end;

function TMainIDE.DoSaveBuildIDEConfigs(Flags: TBuildLazarusFlags
  ): TModalResult;
var
  PkgOptions: string;
  InheritedOptionStrings: TInheritedCompOptsStrings;
  FPCVersion, FPCRelease, FPCPatch: integer;
  IDEBuildFlags: TBuildLazarusFlags;
begin
  // create uses section addition for lazarus.pp
  Result:=PkgBoss.DoSaveAutoInstallConfig;
  if Result<>mrOk then exit;

  // prepare static auto install packages
  PkgOptions:='';
  if (blfWithStaticPackages in Flags)
  or MiscellaneousOptions.BuildLazOpts.WithStaticPackages then begin
    // create inherited compiler options
    PkgOptions:=PkgBoss.DoGetIDEInstallPackageOptions(InheritedOptionStrings);

    // check ambiguous units
    CodeToolBoss.GetFPCVersionForDirectory(
                               EnvironmentOptions.LazarusDirectory,
                               FPCVersion,FPCRelease,FPCPatch);
    if (FPCVersion=0) or (FPCRelease=0) or (FPCPatch=0) then ;
  end;

  // save extra options
  IDEBuildFlags:=Flags+[blfOnlyIDE];
  Result:=SaveIDEMakeOptions(MiscellaneousOptions.BuildLazOpts,
                             GlobalMacroList,PkgOptions,IDEBuildFlags);
  if Result<>mrOk then exit;
end;

function TMainIDE.DoBuildLazarus(Flags: TBuildLazarusFlags): TModalResult;
var
  PkgOptions: string;
  IDEBuildFlags: TBuildLazarusFlags;
  InheritedOptionStrings: TInheritedCompOptsStrings;
  CompiledUnitExt: String;
  FPCVersion, FPCRelease, FPCPatch: integer;
begin
  if ToolStatus<>itNone then begin
    MessageDlg(lisNotNow,
      lisYouCanNotBuildLazarusWhileDebuggingOrCompiling,
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  
  Result:=DoSaveAll([sfDoNotSaveVirtualFiles]);
  if Result<>mrOk then exit;

  MessagesView.BeginBlock;
  try
    MainBuildBoss.SetBuildTargetIDE;

    // first compile all lazarus components (LCL, SynEdit, CodeTools, ...)
    SourceNotebook.ClearErrorLines;
    Result:=BuildLazarus(MiscellaneousOptions.BuildLazOpts,
                         EnvironmentOptions.ExternalTools,GlobalMacroList,
                         '',EnvironmentOptions.CompilerFilename,
                         EnvironmentOptions.MakeFilename,
                         Flags+[blfWithoutLinkingIDE]);
    if Result<>mrOk then begin
      DebugLn('TMainIDE.DoBuildLazarus: Build Lazarus without linking failed.');
      exit;
    end;

    // then compile the 'installed' packages
    if ([blfWithStaticPackages,blfOnlyIDE]*Flags=[])
    and (MiscellaneousOptions.BuildLazOpts.ItemIDE.MakeMode=mmNone) then exit;

    // prepare static auto install packages
    PkgOptions:='';
    if (blfWithStaticPackages in Flags)
    or MiscellaneousOptions.BuildLazOpts.WithStaticPackages then begin
      // compile auto install static packages
      Result:=PkgBoss.DoCompileAutoInstallPackages([]);
      if Result<>mrOk then begin
        DebugLn('TMainIDE.DoBuildLazarus: Compile AutoInstall Packages failed.');
        exit;
      end;

      // create uses section addition for lazarus.pp
      Result:=PkgBoss.DoSaveAutoInstallConfig;
      if Result<>mrOk then begin
        DebugLn('TMainIDE.DoBuildLazarus: Save AutoInstall Config failed.');
        exit;
      end;

      // create inherited compiler options
      PkgOptions:=PkgBoss.DoGetIDEInstallPackageOptions(InheritedOptionStrings);

      // check ambiguous units
      CodeToolBoss.GetFPCVersionForDirectory(
                                 EnvironmentOptions.LazarusDirectory,
                                 FPCVersion,FPCRelease,FPCPatch);
      if FPCPatch=0 then ;
      CompiledUnitExt:=MiscellaneousOptions.BuildLazOpts.CompiledUnitExt(
                         FPCVersion,FPCRelease);
      Result:=MainBuildBoss.CheckUnitPathForAmbiguousPascalFiles(
                       EnvironmentOptions.LazarusDirectory,
                       InheritedOptionStrings[icoUnitPath],
                       CompiledUnitExt,'IDE');
      if Result<>mrOk then begin
        DebugLn('TMainIDE.DoBuildLazarus: Check UnitPath for ambiguous pascal files failed.');
        exit;
      end;
    end;

    // save extra options
    IDEBuildFlags:=Flags+[blfOnlyIDE];
    Result:=SaveIDEMakeOptions(MiscellaneousOptions.BuildLazOpts,
                               GlobalMacroList,PkgOptions,IDEBuildFlags);
    if Result<>mrOk then begin
      DebugLn('TMainIDE.DoBuildLazarus: Save IDEMake options failed.');
      exit;
    end;

    // make ide
    SourceNotebook.ClearErrorLines;
    Result:=BuildLazarus(MiscellaneousOptions.BuildLazOpts,
                         EnvironmentOptions.ExternalTools,GlobalMacroList,
                         PkgOptions,EnvironmentOptions.CompilerFilename,
                         EnvironmentOptions.MakeFilename,
                         IDEBuildFlags+[blfUseMakeIDECfg,blfDontClean,
                                        blfWithoutCompilingIDE]
                         );
    if Result<>mrOk then exit;

  finally
    MainBuildBoss.SetBuildTarget('','','');

    DoCheckFilesOnDisk;
    MessagesView.EndBlock;
  end;
  if (Result=mrOK) and MiscellaneousOptions.BuildLazOpts.RestartAfterBuild then
    mnuRestartClicked(nil);
end;

function TMainIDE.DoBuildFile: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  DirectiveList: TStringList;
  BuildWorkingDir: String;
  BuildCommand: String;
  BuildScan: TIDEDirBuildScanFlags;
  ProgramFilename: string;
  Params: string;
  ExtTool: TExternalToolOptions;
  Filename: String;
begin
  Result:=mrCancel;
  if ToolStatus<>itNone then exit;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  Result:=DoSaveEditorFile(ActiveUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
  if Result<>mrOk then exit;
  DirectiveList:=TStringList.Create;
  try
    Result:=GetIDEDirectives(ActiveUnitInfo,DirectiveList);
    if Result<>mrOk then exit;

    // get values form directive list
    // build
    BuildWorkingDir:=GetIDEStringDirective(DirectiveList,
                                         IDEDirectiveNames[idedBuildWorkingDir],
                                         '');
    if BuildWorkingDir='' then
      BuildWorkingDir:=ExtractFilePath(ActiveUnitInfo.Filename);
    if not GlobalMacroList.SubstituteStr(BuildWorkingDir) then begin
      Result:=mrCancel;
      exit;
    end;
    BuildCommand:=GetIDEStringDirective(DirectiveList,
                                      IDEDirectiveNames[idedBuildCommand],
                                      IDEDirDefaultBuildCommand);
    if (not GlobalMacroList.SubstituteStr(BuildCommand))
    or (BuildCommand='') then begin
      Result:=mrCancel;
      exit;
    end;
    BuildScan:=GetIDEDirBuildScanFromString(GetIDEStringDirective(DirectiveList,
                                   IDEDirectiveNames[idedBuildScan],''));

    SourceNotebook.ClearErrorLines;

    SplitCmdLine(BuildCommand,ProgramFilename,Params);
    if not FilenameIsAbsolute(ProgramFilename) then begin
      Filename:=FindProgram(ProgramFilename,BuildWorkingDir,true);
      if Filename<>'' then ProgramFilename:=Filename;
    end;
    if ProgramFilename='' then begin
      Result:=mrCancel;
      exit;
    end;

    ExtTool:=TExternalToolOptions.Create;
    try
      ExtTool.Filename:=ProgramFilename;
      ExtTool.ScanOutputForFPCMessages:=idedbsfFPC in BuildScan;
      ExtTool.ScanOutputForMakeMessages:=idedbsfMake in BuildScan;
      ExtTool.ScanOutput:=true;
      ExtTool.Title:='Build File '+ActiveUnitInfo.Filename;
      ExtTool.WorkingDirectory:=BuildWorkingDir;
      ExtTool.CmdLineParams:=Params;

      // run
      Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,GlobalMacroList);
    finally
      // clean up
      ExtTool.Free;
    end;
  finally
    DirectiveList.Free;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoRunFile: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  RunFlags: TIDEDirRunFlags;
  AlwaysBuildBeforeRun: boolean;
  RunWorkingDir: String;
  RunCommand: String;
  ProgramFilename: string;
  Params: string;
  ExtTool: TExternalToolOptions;
  Filename: String;
  DirectiveList: TStringList;
begin
  Result:=mrCancel;
  if ToolStatus<>itNone then exit;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  if not FilenameIsAbsolute(ActiveUnitInfo.Filename) then begin
    Result:=DoSaveEditorFile(ActiveUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
    if Result<>mrOk then exit;
  end;
  DirectiveList:=TStringList.Create;
  try
    Result:=GetIDEDirectives(ActiveUnitInfo,DirectiveList);
    if Result<>mrOk then exit;

    RunFlags:=GetIDEDirRunFlagFromString(
                 GetIDEStringDirective(DirectiveList,
                                       IDEDirectiveNames[idedRunFlags],''));
    AlwaysBuildBeforeRun:=idedrfBuildBeforeRun in RunFlags;
    if AlwaysBuildBeforeRun then begin
      Result:=DoBuildFile;
      if Result<>mrOk then exit;
    end;
    RunWorkingDir:=GetIDEStringDirective(DirectiveList,
                                       IDEDirectiveNames[idedRunWorkingDir],'');
    if RunWorkingDir='' then
      RunWorkingDir:=ExtractFilePath(ActiveUnitInfo.Filename);
    if not GlobalMacroList.SubstituteStr(RunWorkingDir) then begin
      Result:=mrCancel;
      exit;
    end;
    RunCommand:=GetIDEStringDirective(DirectiveList,
                                    IDEDirectiveNames[idedRunCommand],
                                    IDEDirDefaultRunCommand);
    if (not GlobalMacroList.SubstituteStr(RunCommand))
    or (RunCommand='') then begin
      Result:=mrCancel;
      exit;
    end;

    SourceNotebook.ClearErrorLines;

    SplitCmdLine(RunCommand,ProgramFilename,Params);
    if not FilenameIsAbsolute(ProgramFilename) then begin
      Filename:=FindProgram(ProgramFilename,RunWorkingDir,true);
      if Filename<>'' then ProgramFilename:=Filename;
    end;
    if ProgramFilename='' then begin
      Result:=mrCancel;
      exit;
    end;

    ExtTool:=TExternalToolOptions.Create;
    try
      ExtTool.Filename:=ProgramFilename;
      ExtTool.Title:='Run File '+ActiveUnitInfo.Filename;
      ExtTool.WorkingDirectory:=RunWorkingDir;
      ExtTool.CmdLineParams:=Params;

      // run
      Result:=EnvironmentOptions.ExternalTools.Run(ExtTool,GlobalMacroList);
    finally
      // clean up
      ExtTool.Free;
    end;
  finally
    DirectiveList.Free;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoConfigBuildFile: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  DirectiveList: TStringList;
  CodeResult: Boolean;
  BuildFileDialog: TBuildFileDialog;
begin
  Result:=mrCancel;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  if not FilenameIsAbsolute(ActiveUnitInfo.Filename) then begin
    Result:=DoSaveEditorFile(ActiveUnitInfo.EditorIndex,[sfCheckAmbiguousFiles]);
    if Result<>mrOk then exit;
  end;
  DirectiveList:=TStringList.Create;
  try
    Result:=GetIDEDirectives(ActiveUnitInfo,DirectiveList);
    if Result<>mrOk then exit;

    BuildFileDialog:=TBuildFileDialog.Create(nil);
    try
      BuildFileDialog.DirectiveList:=DirectiveList;
      BuildFileDialog.BuildFileIfActive:=ActiveUnitInfo.BuildFileIfActive;
      BuildFileDialog.RunFileIfActive:=ActiveUnitInfo.RunFileIfActive;
      BuildFileDialog.MacroList:=GlobalMacroList;
      BuildFileDialog.Filename:=
        CreateRelativePath(ActiveUnitInfo.Filename,Project1.ProjectDirectory);
      if BuildFileDialog.ShowModal<>mrOk then begin
        Result:=mrCancel;
        exit;
      end;
      ActiveUnitInfo.BuildFileIfActive:=BuildFileDialog.BuildFileIfActive;
      ActiveUnitInfo.RunFileIfActive:=BuildFileDialog.RunFileIfActive;
    finally
      BuildFileDialog.Free;
    end;

    // save IDE directives
    if FilenameIsPascalSource(ActiveUnitInfo.Filename) then begin
      // parse source for IDE directives (i.e. % comments)
      CodeResult:=CodeToolBoss.SetIDEDirectives(ActiveUnitInfo.Source,
                                                DirectiveList);
      ApplyCodeToolChanges;
      if not CodeResult then begin
        DoJumpToCodeToolBossError;
        exit;
      end;

    end else begin
      // ToDo: load .lfi file
      exit;
    end;

  finally
    DirectiveList.Free;
  end;

  Result:=mrOk;
end;

function TMainIDE.GetIDEDirectives(AnUnitInfo: TUnitInfo;
  DirectiveList: TStrings): TModalResult;
var
  CodeResult: Boolean;
begin
  Result:=mrCancel;
  if FilenameIsPascalSource(AnUnitInfo.Filename) then begin
    // parse source for IDE directives (i.e. % comments)
    CodeResult:=CodeToolBoss.GetIDEDirectives(AnUnitInfo.Source,DirectiveList);
    if not CodeResult then begin
      DoJumpToCodeToolBossError;
      exit;
    end;

  end else begin
    // ToDo: load .lfi file
    MessageDlg('Not implemented',
      'Sorry, IDE directives are only implemented for pascal sources',
      mtInformation,[mbCancel],0);
    exit;
  end;
  Result:=mrOk;
end;

function TMainIDE.DoConvertDFMtoLFM: TModalResult;
var
  OpenDialog: TOpenDialog;
  i: integer;
  AFilename: string;
begin
  Result:=mrOk;
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisSelectDFMFiles;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    OpenDialog.Filter := rsFormDataFileDfm
                         + '|' + dlgAllFiles + '|'+GetAllFilesMask;
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      For I := 0 to OpenDialog.Files.Count-1 do begin
        AFilename:=ExpandFilename(OpenDialog.Files.Strings[i]);
        if ConvertDFMFileToLFMFile(AFilename)=mrAbort then begin
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

function TMainIDE.DoCheckLFMInEditor: TModalResult;
var
  LFMSrcEdit: TSourceEditor;
  LFMUnitInfo: TUnitInfo;
  UnitFilename: String;
  PascalBuf: TCodeBuffer;
  i: integer;
begin
  // check, if a .lfm file is opened in the source editor
  GetCurrentUnit(LFMSrcEdit,LFMUnitInfo);
  if (LFMUnitInfo=nil)
  or (CompareFileExt(LFMUnitInfo.Filename,'.lfm',false)<>0) then begin
    MessageDlg('No LFM file',
      'This function needs an open .lfm file in the source editor.',
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  // try to find the pascal unit
  for i:=Low(PascalFileExt) to High(PascalFileExt) do begin
    UnitFilename:=ChangeFileExt(LFMUnitInfo.Filename,PascalFileExt[i]);
    if FileExists(UnitFilename) then
      break
    else
      UnitFilename:='';
  end;
  if UnitFilename='' then begin
    MessageDlg('No pascal file',
      'Unable to find pascal unit (.pas,.pp) for .lfm file'#13
      +'"'+LFMUnitInfo.Filename+'"',
      mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;

  if ToolStatus<>itNone then begin
    DebugLn(['TMainIDE.DoCheckLFMInEditor ToolStatus<>itNone']);
    Result:=mrCancel;
    exit;
  end;
  // load the pascal unit
  SaveSourceEditorChangesToCodeCache(-1);
  Result:=LoadCodeBuffer(PascalBuf,UnitFilename,[]);
  if Result<>mrOk then exit;

  // open messages window
  SourceNotebook.ClearErrorLines;
  if MessagesView<>nil then
    MessagesView.Clear;
  DoArrangeSourceEditorAndMessageView(false);

  // parse the LFM file and the pascal unit
  if CheckLFMBuffer(PascalBuf,LFMUnitInfo.Source,@MessagesView.AddMsg,
                        true,true)<>mrOk
  then begin
    DoJumpToCompilerMessage(-1,true);
  end;

  Result:=mrOk;
end;

function TMainIDE.DoConvertDelphiUnit(const DelphiFilename: string
  ): TModalResult;
var
  OldChange: Boolean;
begin
  InputHistories.LastConvertDelphiUnit:=DelphiFilename;
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    Result:=DelphiProject2Laz.ConvertDelphiToLazarusUnit(DelphiFilename,[]);
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

function TMainIDE.DoConvertDelphiProject(const DelphiFilename: string
  ): TModalResult;
var
  OldChange: Boolean;
begin
  InputHistories.LastConvertDelphiProject:=DelphiFilename;
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    Result:=DelphiProject2Laz.ConvertDelphiToLazarusProject(DelphiFilename);
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

function TMainIDE.DoConvertDelphiPackage(const DelphiFilename: string
  ): TModalResult;
var
  OldChange: Boolean;
begin
  InputHistories.LastConvertDelphiPackage:=DelphiFilename;
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    Result:=DelphiProject2Laz.ConvertDelphiToLazarusPackage(DelphiFilename);
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
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
    Section: TIDEMenuSection;
  begin
    Section:=itmCustomTools;
    // add enough menuitems
    while Section.Count-1<ToolCount do
      RegisterIDEMenuCommand(Section.GetPath,
                          'itmToolCustomExt'+IntToStr(Section.Count),'');
    // delete unneeded menuitems
    while Section.Count-1>ToolCount do
      Section[Section.Count-1].Free;
  end;

  procedure SetToolMenuItems;
  var
    CurMenuItem: TIDEMenuItem;
    i, Index: integer;
    ExtTool: TExternalToolOptions;
  begin
    i:=1;
    Index:=0;
    while (i<itmCustomTools.Count) do begin
      CurMenuItem:=itmCustomTools[i];
      ExtTool:=EnvironmentOptions.ExternalTools[Index];
      CurMenuItem.Caption:=ExtTool.Title;
      if CurMenuItem is TIDEMenuCommand then
        TIDEMenuCommand(CurMenuItem).Command:=
          EditorOpts.KeyMap.FindIDECommand(ecExtToolFirst+Index);
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

function TMainIDE.PrepareForCompile: TModalResult;
begin
  Result:=mrOk;
  if ToolStatus=itDebugger then begin
    Result:=MessageDlg(lisStopDebugging2,
      lisStopCurrentDebuggingAndRebuildProject,
      mtConfirmation,[mbYes,mbNo,mbAbort],0);
    if Result=mrNo then Result:=mrCancel;
    if Result<>mrYes then exit;

    Result:=DebugBoss.DoStopProject;
    if Result<>mrOk then exit;
  end;
end;

function TMainIDE.OnRunExternalTool(Tool: TIDEExternalToolOptions): TModalResult;
begin
  SourceNotebook.ClearErrorLines;
  Result:=EnvironmentOptions.ExternalTools.Run(Tool,GlobalMacroList);
  DoCheckFilesOnDisk;
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
  if (ErrorMsg='') or (NewTopLine=0) or (NewX=0) or (NewY=0) or (NewCode=nil) then ;
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

function TMainIDE.GetDesignerWithProjectFile(AFile: TLazProjectFile;
  LoadForm: boolean): TIDesigner;
var
  AnUnitInfo: TUnitInfo;
  AForm: TCustomForm;
begin
  AnUnitInfo:=AFile as TUnitInfo;
  AForm:=GetDesignerFormOfSource(AnUnitInfo,LoadForm);
  if AForm<>nil then
    Result:=AForm.Designer;
end;

procedure TMainIDE.GetObjectInspectorUnit(
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
begin
  ActiveSourceEditor:=nil;
  ActiveUnitInfo:=nil;
  if (ObjectInspector1=nil) or (ObjectInspector1.PropertyEditorHook=nil)
  or (ObjectInspector1.PropertyEditorHook.LookupRoot=nil)
  then exit;
  GetUnitWithPersistent(ObjectInspector1.PropertyEditorHook.LookupRoot,
    ActiveSourceEditor,ActiveUnitInfo);
end;

procedure TMainIDE.GetUnitWithForm(AForm: TCustomForm;
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
var
  AComponent: TComponent;
begin
  if AForm<>nil then begin
    if (AForm.Designer=nil) then
      RaiseException('TMainIDE.GetUnitWithForm AForm.Designer');
    AComponent:=TDesigner(AForm.Designer).LookupRoot;
    if AComponent=nil then
      RaiseException('TMainIDE.GetUnitWithForm AComponent=nil');
    GetUnitWithPersistent(AComponent,ActiveSourceEditor,ActiveUnitInfo);
  end else begin
    ActiveSourceEditor:=nil;
    ActiveUnitInfo:=nil;
  end;
end;

procedure TMainIDE.GetUnitWithPersistent(APersistent: TPersistent;
  var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo);
begin
  if APersistent<>nil then begin
    ActiveUnitInfo:=Project1.FirstUnitWithComponent;
    while ActiveUnitInfo<>nil do begin
      if ActiveUnitInfo.Component=APersistent then begin
        ActiveSourceEditor:=SourceNoteBook.FindSourceEditorWithPageIndex(
                                                    ActiveUnitInfo.EditorIndex);
        exit;
      end;
      ActiveUnitInfo:=ActiveUnitInfo.NextUnitWithComponent;
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
      ACaption:=lisReadError;
      AText:=Format(lisUnableToReadFile2, ['"', AFilename, '"']);
      result := Application.MessageBox(PChar(aText),pChar(aCaption),mb_IconError+mb_AbortRetryIgnore);
      if Result=mrAbort then exit;
    end;
  until Result<>mrRetry;
end;

function TMainIDE.DoRenameUnitLowerCase(AnUnitInfo: TUnitInfo;
  AskUser: boolean): TModalresult;
var
  OldFilename: String;
  OldShortFilename: String;
  NewFilename: String;
  NewShortFilename: String;
  ResourceCode: TCodeBuffer;
  NewUnitName: String;
begin
  Result:=mrOk;
  OldFilename:=AnUnitInfo.Filename;
  // check if file is unit
  if not FilenameIsPascalUnit(OldFilename) then exit;
  // check if file is already lowercase (or it does not matter in current OS)
  OldShortFilename:=ExtractFilename(OldFilename);
  NewShortFilename:=lowercase(OldShortFilename);
  if CompareFilenames(OldShortFilename,NewShortFilename)=0 then exit;
  // create new filename
  NewFilename:=ExtractFilePath(OldFilename)+NewShortFilename;

  // rename unit
  if AskUser then begin
    Result:=MessageDlg(lisFileNotLowercase,
      Format(lisTheUnitIsNotLowercaseTheFreePascalCompiler10XNeeds, ['"',
        OldFilename, '"', #13, #13, #13]),
      mtConfirmation,[mbYes,mbNo,mbAbort],0);
    if Result=mrNo then Result:=mrIgnore;
    if Result<>mrYes then exit;
  end;
  NewUnitName:=AnUnitInfo.UnitName;
  if NewUnitName='' then begin
    AnUnitInfo.ReadUnitNameFromSource(false);
    NewUnitName:=AnUnitInfo.CreateUnitName;
  end;
  ResourceCode:=nil;
  Result:=DoRenameUnit(AnUnitInfo,NewFilename,NewUnitName,ResourceCode);
end;

function TMainIDE.DoCheckFilesOnDisk(Instantaneous: boolean): TModalResult;
var
  AnUnitList: TFPList; // list of TUnitInfo
  i: integer;
  CurUnit: TUnitInfo;
begin
  Result:=mrOk;
  if FCheckingFilesOnDisk then exit;
  if Project1=nil then exit;
  if Screen.GetCurrentModalForm<>nil then exit;

  if not Instantaneous then begin
    FCheckFilesOnDiskNeeded:=true;
    exit;
  end;
  FCheckFilesOnDiskNeeded:=false;

  //debugln('TMainIDE.DoCheckFilesOnDisk');
  FCheckingFilesOnDisk:=true;
  try
    InvalidateFileStateCache;
    Project1.GetUnitsChangedOnDisk(AnUnitList);
    if AnUnitList=nil then exit;
    Result:=ShowDiskDiffsDialog(AnUnitList);
    if Result in [mrYesToAll] then
      Result:=mrOk;
    for i:=0 to AnUnitList.Count-1 do begin
      CurUnit:=TUnitInfo(AnUnitList[i]);
      //DebugLn(['TMainIDE.DoCheckFilesOnDisk revert ',CurUnit.Filename,' EditorIndex=',CurUnit.EditorIndex]);
      if Result=mrOk then begin
        if CurUnit.EditorIndex>=0 then begin
          Result:=DoOpenEditorFile(CurUnit.Filename,CurUnit.EditorIndex,[ofRevert]);
          //DebugLn(['TMainIDE.DoCheckFilesOnDisk DoOpenEditorFile=',Result]);
        end else if CurUnit.IsMainUnit then begin
          Result:=DoRevertMainUnit;
          //DebugLn(['TMainIDE.DoCheckFilesOnDisk DoRevertMainUnit=',Result]);
        end else
          Result:=mrIgnore;
        if Result=mrAbort then exit;
      end else begin
        //DebugLn(['TMainIDE.DoCheckFilesOnDisk IgnoreCurrentFileDateOnDisk']);
        CurUnit.IgnoreCurrentFileDateOnDisk;
      end;
    end;
    Result:=mrOk;
    AnUnitList.Free;
  finally
    FCheckingFilesOnDisk:=false;
  end;
end;

function TMainIDE.DoPublishModule(Options: TPublishModuleOptions;
  const SrcDirectory, DestDirectory: string): TModalResult;
var
  SrcDir, DestDir: string;
  NewProjectFilename: string;
  Tool: TExternalToolOptions;
  CommandAfter, CmdAfterExe, CmdAfterParams: string;
  CurProject: TProject;
  TempCmd: String;

  procedure ShowErrorForCommandAfter;
  begin
    MessageDlg(lisInvalidCommand,
      Format(lisTheCommandAfterIsNotExecutable, ['"', CmdAfterExe, '"']),
      mtError,[mbCancel],0);
  end;

begin
  //DebugLn('TMainIDE.DoPublishModule A');
  Result:=mrCancel;

  // do not delete project files
  DestDir:=TrimFilename(AppendPathDelim(DestDirectory));
  SrcDir:=TrimFilename(AppendPathDelim(SrcDirectory));
  if (DestDir='') then begin
    MessageDlg('Invalid publishing Directory',
      'Destination directory for publishing is empty.',mtError,
      [mbCancel],0);
    Result:=mrCancel;
    exit;
  end;
  //DebugLn('TMainIDE.DoPublishModule A SrcDir="',SrcDir,'" DestDir="',DestDir,'"');
  if CompareFilenames(CleanAndExpandDirectory(SrcDir),
                      CleanAndExpandDirectory(DestDir))=0
  then begin
    MessageDlg('Invalid publishing Directory',
      'Source directory "'+SrcDir+'"'#13
      +'and destination directory "'+DestDir+'"'#13
      +'are the same.'#13
      +#13
      +'Maybe you misunderstand this feature.'#13
      +'It will clean/recreate the destination directory'#13
      +'and copies the package/project into it.',mtError,[mbCancel],0);
    Result:=mrCancel;
    exit;
  end;

  // check command after
  CommandAfter:=Options.CommandAfter;
  if not GlobalMacroList.SubstituteStr(CommandAfter) then begin
    Result:=mrCancel;
    exit;
  end;
  SplitCmdLine(CommandAfter,CmdAfterExe,CmdAfterParams);
  if (CmdAfterExe<>'') then begin
    //DebugLn('TMainIDE.DoPublishModule A CmdAfterExe="',CmdAfterExe,'"');
    // first look in the project directory
    TempCmd:=CmdAfterExe;
    if not FilenameIsAbsolute(TempCmd) then
      TempCmd:=TrimFilename(AppendPathDelim(Project1.ProjectDirectory)+TempCmd);
    if FileExists(TempCmd) then begin
      CmdAfterExe:=TempCmd;
    end else begin
      TempCmd:=FindDefaultExecutablePath(CmdAfterExe);
      if TempCmd<>'' then
        CmdAfterExe:=TempCmd;
    end;
    if not FileIsExecutableCached(CmdAfterExe) then begin
      MessageDlg(lisCommandAfterInvalid,
        Format(lisTheCommandAfterPublishingIsInvalid, [#13, '"', CmdAfterExe,
          '"']), mtError, [mbCancel], 0);
      Result:=mrCancel;
      exit;
    end;
  end;

  // clear destination directory
  if DirPathExists(DestDir) then begin
    if (not DeleteDirectory(ChompPathDelim(DestDir),true)) then begin
      MessageDlg(lisUnableToCleanUpDestinationDirectory,
        Format(lisUnableToCleanUpPleaseCheckPermissions, ['"', DestDir, '"', #13]
          ),
        mtError,[mbOk],0);
      Result:=mrCancel;
      exit;
    end;
  end;

  // copy the directory
  if not CopyDirectoryWithMethods(SrcDir,DestDir,
    @OnCopyFile,@OnCopyError,Options) then
  begin
    debugln('TMainIDE.DoPublishModule CopyDirectoryWithMethods failed');
    Result:=mrCancel;
    exit;
  end;

  // write a filtered .lpi file
  if Options is TPublishProjectOptions then begin
    CurProject:=TProject(TPublishProjectOptions(Options).Owner);
    NewProjectFilename:=DestDir+ExtractFilename(CurProject.ProjectInfoFile);
    DeleteFile(NewProjectFilename);
    Result:=CurProject.WriteProject(CurProject.PublishOptions.WriteFlags
                                   +[pwfSkipDebuggerSettings,pwfSkipJumpPoints],
                                   NewProjectFilename);
    if Result<>mrOk then begin
      debugln('TMainIDE.DoPublishModule CurProject.WriteProject failed');
      exit;
    end;
  end;

  // execute 'CommandAfter'
  if (CmdAfterExe<>'') then begin
    if FileIsExecutableCached(CmdAfterExe) then begin
      Tool:=TExternalToolOptions.Create;
      Tool.Filename:=CmdAfterExe;
      Tool.Title:=lisCommandAfterPublishingModule;
      Tool.WorkingDirectory:=DestDir;
      Tool.CmdLineParams:=CmdAfterParams;
      Result:=EnvironmentOptions.ExternalTools.Run(Tool,GlobalMacroList);
      if Result<>mrOk then exit;
    end else begin
      ShowErrorForCommandAfter;
      Result:=mrCancel;
      exit;
    end;
  end;
end;

procedure TMainIDE.UpdateCaption;
var NewCaption: string;
begin
  if MainIDEBar=nil then exit;
  NewCaption := Format(lisLazarusEditorV, [GetLazarusVersionString]);
  if MainBarSubTitle<>'' then begin
    NewCaption:=NewCaption+' - '+MainBarSubTitle;
  end else begin
    if Project1<>nil then begin
      if Project1.Title<>'' then
        NewCaption:=NewCaption +' - '+Project1.Title
      else if Project1.ProjectInfoFile<>'' then
        NewCaption:=NewCaption+' - '+ExtractFileName(Project1.ProjectInfoFile)
      else
        NewCaption:=Format(lisnewProject, [NewCaption])
    end;
  end;
  case ToolStatus of
  itBuilder:  NewCaption:=Format(liscompiling, [NewCaption]);
  itDebugger: NewCaption:=Format(lisdebugging, [NewCaption]);
  end;
  MainIDEBar.Caption:=NewCaption;
end;

procedure TMainIDE.HideIDE;
var
  i: Integer;
  AForm: TCustomForm;
begin
  // hide hints
  Application.HideHint;
  SourceNotebook.HideHint;

  // hide designer forms
  HideUnmodifiedDesigners;

  // collect all windows except the main bar
  for i:=0 to Screen.CustomFormCount-1 do begin
    AForm:=Screen.CustomForms[i];
    if (AForm<>MainIDEBar)                    // ignore the main bar
    and (AForm.Designer=nil)                  // ignore designer forms
    and (AForm.Visible)                       // ignore hidden forms
    and (not (fsModal in AForm.FormState))    // ignore modal forms
    and (HiddenWindowsOnRun.IndexOf(AForm)<0) // ignore already collected forms
    then
      HiddenWindowsOnRun.Add(AForm);
  end;

  // hide all collected windows
  for i:=0 to HiddenWindowsOnRun.Count-1 do begin
    AForm:=TCustomForm(HiddenWindowsOnRun[i]);
    if not (csDesigning in ComponentState) then
      AForm.Hide;
  end;

  // minimize IDE
  MainIDEBar.HideIDE;
end;

procedure TMainIDE.HideUnmodifiedDesigners;
var
  AnUnitInfo: TUnitInfo;
  NextUnitInfo: TUnitInfo;
begin
  AnUnitInfo:=Project1.FirstUnitWithComponent;
  while AnUnitInfo<>nil do begin
    NextUnitInfo:=AnUnitInfo.NextUnitWithComponent;
    if not AnUnitInfo.NeedsSaveToDisk then
      CloseUnitComponent(AnUnitInfo,[]);
    AnUnitInfo:=NextUnitInfo;
  end;
end;

procedure TMainIDE.UnhideIDE;
var
  AForm: TCustomForm;
begin
  // unminimize IDE
  MainIDEBar.UnhideIDE;

  // show other windows
  while HiddenWindowsOnRun.Count>0 do begin
    AForm:=TCustomForm(HiddenWindowsOnRun[0]);
    if (csDesigning in ComponentState) then
      ShowDesignerForm(AForm)
    else
      AForm.Show;
    HiddenWindowsOnRun.Delete(0);
  end;
end;

procedure TMainIDE.DoBringToFrontFormOrUnit;
begin
  if FDisplayState = dsSource then begin
    DoShowDesignerFormOfCurrentSrc;
  end else begin
    DoShowSourceOfActiveDesignerForm;
  end;
end;

procedure TMainIDE.DoBringToFrontFormOrInspector(ForceInspector: boolean);

  procedure ShowInspector;
  begin
    if ObjectInspector1=nil then exit;
    ObjectInspector1.ShowOnTop;
    FDisplayState:= Succ(FDisplayState);
  end;

begin
  if ForceInspector then begin
    ShowInspector;
    exit;
  end;
  case FDisplayState of

    dsInspector:
      DoShowDesignerFormOfCurrentSrc;

    dsInspector2:
      DoShowSourceOfActiveDesignerForm;

    else
      ShowInspector;
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
  AForm:=GetDesignerFormOfSource(ActiveUnitInfo,true);
  if AForm=nil then exit;
  FDisplayState:= dsForm;
  FLastFormActivated:=AForm;
  ShowDesignerForm(AForm);
  if TheControlSelection.SelectionForm<>AForm then begin
    // select the new form (object inspector, formeditor, control selection)
    TheControlSelection.AssignPersistent(ActiveUnitInfo.Component);
  end;
end;

procedure TMainIDE.DoShowSourceOfActiveDesignerForm;
var
  ActiveUnitInfo: TUnitInfo;
begin
  if SourceNoteBook.NoteBook = nil then exit;
  if FLastFormActivated <> nil then begin
    ActiveUnitInfo:= Project1.UnitWithComponent(
                             TDesigner(FLastFormActivated.Designer).LookupRoot);
    if (ActiveUnitInfo <> nil) and (ActiveUnitInfo.EditorIndex >= 0) then
    begin
      SourceNotebook.Notebook.PageIndex:= ActiveUnitInfo.EditorIndex;
    end;
  end;
  SourceNoteBook.ShowOnTop;
  FDisplayState:= dsSource;
end;

procedure TMainIDE.OnMacroSubstitution(TheMacro: TTransferMacro;
  const MacroName: string; var s:string;
  const Data: PtrInt; var Handled, Abort: boolean);
var MacroLName:string;
begin
  if TheMacro=nil then begin
    DebugLn('WARNING: Macro not defined: "'+MacroName+'".');
    s:='';
    //MessageDlg('Unknown Macro','Macro not defined: "'+s+'".',mtError,[mbAbort],0);
    //DumpStack;
    Handled:=true;
    exit;
  end;
  MacroLName:=lowercase(MacroName);
  Handled:=true;
  if MacroLName='save' then begin
    if (SourceNoteBook<>nil) and (SourceNoteBook.NoteBook<>nil) then
      Abort:=(DoSaveEditorFile(SourceNoteBook.NoteBook.PageIndex,
              [sfCheckAmbiguousFiles])<>mrOk);
    s:='';
  end else if MacroLName='saveall' then begin
    Abort:=(DoSaveAll([sfCheckAmbiguousFiles])<>mrOk);
    s:='';
  end else
    Handled:=false;
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
  Filename, SearchedFilename: string;
  LogCaretXY: TPoint;
  TopLine: integer;
  MsgType: TErrorType;
  SrcEdit: TSourceEditor;
  OpenFlags: TOpenFlags;
  CurMsg, CurDir: string;
  NewFilename: String;
begin
  Result:=false;
  
  MaxMessages:=MessagesView.VisibleItemCount;
  if Index>=MaxMessages then exit;
  if (Index<0) then begin
    // search relevant message (first error, first fatal)
    Index:=0;
    while (Index<MaxMessages) do begin
      CurMsg:=MessagesView.VisibleItems[Index].Msg;
      if (TheOutputFilter.GetSourcePosition(
        CurMsg,Filename,LogCaretXY,MsgType)) then
      begin
        if MsgType in [etError,etFatal,etPanic] then break;
      end;
      inc(Index);
    end;
    if Index>=MaxMessages then exit;
  end;
  MessagesView.SelectedMessageIndex:=Index;

  // first try the plugins
  if MessagesView.ExecuteMsgLinePlugin(imqfoJump) then exit;

  // default: jump to source position
  MessagesView.GetVisibleMessageAt(Index,CurMsg,CurDir);
  if TheOutputFilter.GetSourcePosition(CurMsg,Filename,LogCaretXY,MsgType)
  then begin
    if (not FilenameIsAbsolute(Filename)) and (CurDir<>'') then begin
      // the directory was just hidden, re-append it
      NewFilename:=AppendPathDelim(CurDir)+Filename;
      if FileExists(NewFilename) then
        Filename:=NewFilename;
    end;

    OpenFlags:=[ofOnlyIfExists,ofRegularFile];
    if MainBuildBoss.IsTestUnitFilename(Filename) then begin
      SearchedFilename := ExtractFileName(Filename);
      Include(OpenFlags,ofVirtualFile);
    end else begin
      SearchedFilename := FindUnitFile(Filename);
      if not FilenameIsAbsolute(SearchedFilename) then
        Include(OpenFlags,ofVirtualFile);
    end;

    if SearchedFilename<>'' then begin
      // open the file in the source editor
      Result:=(DoOpenEditorFile(SearchedFilename,-1,OpenFlags)=mrOk);
      if Result then begin
        // set caret position
        SourceNotebook.AddJumpPointClicked(Self);
        SrcEdit:=SourceNoteBook.GetActiveSE;
        if LogCaretXY.Y>SrcEdit.EditorComponent.Lines.Count then
          LogCaretXY.Y:=SrcEdit.EditorComponent.Lines.Count;
        TopLine:=LogCaretXY.Y-(SrcEdit.EditorComponent.LinesInWindow div 2);
        if TopLine<1 then TopLine:=1;
        if FocusEditor then begin
          //SourceNotebook.BringToFront;
          MessagesView.ShowOnTop;
          SourceNoteBook.ShowOnTop;
          SourceNotebook.FocusEditor;
        end;
        SrcEdit.EditorComponent.LogicalCaretXY:=LogCaretXY;
        SrcEdit.EditorComponent.TopLine:=TopLine;
        with SrcEdit.EditorComponent do begin
          BlockBegin:=LogCaretXY;
          BlockEnd:=LogCaretXY;
          LeftChar:=Max(LogCaretXY.X-CharsInWindow,1);
        end;
        SrcEdit.ErrorLine:=LogCaretXY.Y;
      end;
    end else begin
      if FilenameIsAbsolute(Filename) then begin
        MessageDlg(Format(lisUnableToFindFile, ['"', Filename, '"']),
           mtInformation,[mbOk],0)
      end else begin
        MessageDlg(Format(
          lisUnableToFindFileCheckSearchPathInProjectCompilerOption, ['"',
          Filename, '"', #13, #13]),
           mtInformation,[mbOk],0);
      end;
    end;
  end;
end;

procedure TMainIDE.DoJumpToNextError(DirectionDown: boolean);
var
  Index: integer;
  MaxMessages: integer;
  CurMsg: String;
  Filename: string;
  LogCaretXY: TPoint;
  MsgType: TErrorType;
  OldIndex: integer;
  RoundCount: Integer;
begin
  // search relevant message (next error, fatal or panic)
  MaxMessages:=MessagesView.VisibleItemCount;
  OldIndex:=MessagesView.SelectedMessageIndex;
  Index:=OldIndex;
  RoundCount:=0;
  while (Index>=0) and (Index<MaxMessages) do begin
    // goto to next message
    if DirectionDown then begin
      inc(Index);
      if Index>=MaxMessages then begin
        inc(RoundCount);
        Index:=0;
      end;
    end else begin
      dec(Index);
      if Index<0 then begin
        inc(RoundCount);
        Index:=MaxMessages-1;
      end;
    end;
    if(Index=OldIndex) or (RoundCount>1) then exit;

    // check if it is an error
    CurMsg:=MessagesView.VisibleItems[Index].Msg;
    if (TheOutputFilter.GetSourcePosition(
      CurMsg,Filename,LogCaretXY,MsgType)) then
    begin
      if MsgType in [etError,etFatal,etPanic] then break;
    end;
  end;
  MessagesView.SelectedMessageIndex:=Index;
  DoJumpToCompilerMessage(Index,true);
end;

function TMainIDE.DoJumpToSearchResult(FocusEditor: boolean): boolean;
var
  AFileName: string;
  SearchedFilename: string;
  LogCaretXY: TPoint;
  TopLine: integer;
  OpenFlags: TOpenFlags;
  SrcEdit: TSourceEditor;
begin
  Result:=false;
  CreateSearchResultWindow;
  if pos('(',SearchResultsView.GetSelectedText) > 0 then
  begin
    AFileName:= SearchResultsView.GetSourceFileName;
    if AFilename='' then exit;
    LogCaretXY:= SearchResultsView.GetSourcePositon;
    OpenFlags:=[ofOnlyIfExists,ofRegularFile];
    if MainBuildBoss.IsTestUnitFilename(AFilename) then begin
      SearchedFilename := ExtractFileName(AFilename);
      Include(OpenFlags,ofVirtualFile);
    end else begin
      SearchedFilename := FindUnitFile(AFilename);
    end;
    if SearchedFilename<>'' then begin
      // open the file in the source editor
      Result:=(DoOpenEditorFile(SearchedFilename,-1,OpenFlags)=mrOk);
      if Result then begin
        // set caret position
        SourceNotebook.AddJumpPointClicked(Self);
        SrcEdit:=SourceNoteBook.GetActiveSE;
        if LogCaretXY.Y>SrcEdit.EditorComponent.Lines.Count then
          LogCaretXY.Y:=SrcEdit.EditorComponent.Lines.Count;
        TopLine:=LogCaretXY.Y-(SrcEdit.EditorComponent.LinesInWindow div 2);
        if TopLine<1 then TopLine:=1;
        if FocusEditor then begin
          //SourceNotebook.BringToFront;
          SearchResultsView.ShowOnTop;
          SourceNoteBook.ShowOnTop;
          SourceNotebook.FocusEditor;
        end;
        SrcEdit.EditorComponent.LogicalCaretXY:=LogCaretXY;
        SrcEdit.EditorComponent.TopLine:=TopLine;
        with SrcEdit.EditorComponent do begin
          BlockBegin:=LogCaretXY;
          BlockEnd:=LogCaretXY;
          LeftChar:= Math.Max(LogCaretXY.X-CharsInWindow,1);
        end;
        SrcEdit.ErrorLine:=LogCaretXY.Y;
      end;
    end else if AFilename<>'' then begin
      if FilenameIsAbsolute(AFilename) then begin
        MessageDlg(Format(lisUnableToFindFile, ['"', AFilename, '"']),
           mtInformation,[mbOk],0)
      end else begin
        MessageDlg(Format(
          lisUnableToFindFileCheckSearchPathInProjectCompilerOption, ['"',
          AFilename, '"', #13, #13]),
           mtInformation,[mbOk],0);
      end;
    end;
  end;//if
end;


procedure TMainIDE.DoShowMessagesView;
var
  WasVisible: boolean;
  ALayout: TIDEWindowLayout;
begin
  //debugln('TMainIDE.DoShowMessagesView');
  WasVisible:=MessagesView.Visible;
  MessagesView.Visible:=true;
  if not WasVisible then begin
    // don't move the messagesview, if it was already visible.
    ALayout:=EnvironmentOptions.IDEWindowLayoutList.
                                               ItemByEnum(nmiwMessagesViewName);
    ALayout.Apply;

    // the sourcenotebook is more interesting than the messages
    // TODO: don't do this when messages content intersect the editor content
    SourceNotebook.ShowOnTop;
  end;

  //set the event here for the selectionchanged event
  if not assigned(MessagesView.OnSelectionChanged) then
    MessagesView.OnSelectionChanged := @MessagesViewSelectionChanged;
end;

procedure TMainIDE.DoShowSearchResultsView;
var
  WasVisible: boolean;
  ALayout: TIDEWindowLayout;
begin
  CreateSearchResultWindow;
  WasVisible := SearchResultsView.Visible;
  SearchResultsView.Visible:=true;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByEnum(nmiwSearchResultsViewName);
  ALayout.Apply;
  if not WasVisible then
    // the sourcenotebook is more interesting than the messages
    SourceNotebook.ShowOnTop;

  //set the event here for the selectionchanged event
  if not assigned(SearchresultsView.OnSelectionChanged) then
    SearchresultsView.OnSelectionChanged := @SearchresultsViewSelectionChanged;
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

function TMainIDE.GetTestBuildDirectory: string;
begin
  Result:=MainBuildBoss.GetTestBuildDirectory;
end;

function TMainIDE.FindUnitFile(const AFilename: string): string;
var
  SearchPath, ProjectDir: string;
  AnUnitInfo: TUnitInfo;
begin
  if FilenameIsAbsolute(AFilename) then begin
    Result:=AFilename;
    exit;
  end;
  Result:='';
  // search in virtual (unsaved) files
  AnUnitInfo:=Project1.UnitInfoWithFilename(AFilename,
                                   [pfsfOnlyProjectFiles,pfsfOnlyVirtualFiles]);
  if AnUnitInfo<>nil then begin
    Result:=AnUnitInfo.Filename;
    exit;
  end;
  // search in search path
  if not Project1.IsVirtual then begin
    // ToDo: use the CodeTools way to find the pascal source
    ProjectDir:=Project1.ProjectDirectory;
    SearchPath:=CodeToolBoss.GetCompleteSrcPathForDirectory(ProjectDir);
    Result:=SearchFileInPath(AFilename,ProjectDir,SearchPath,';',[]);
    if Result<>'' then exit;
  end;
end;

{------------------------------------------------------------------------------
  function TMainIDE.FindSourceFile(const AFilename, BaseDirectory: string;
    Flags: TFindSourceFlags): string;

  AFilename can be an absolute or relative filename, of a source file or a
  compiled unit (.ppu, .ppw).
  Find the source filename (pascal source or include file) and returns
  the absolute path.

  First it searches in the current projects src path, then its unit path, then
  its include path. Then all used package source directories are searched.
  Finally the fpc sources are searched.
------------------------------------------------------------------------------}
function TMainIDE.FindSourceFile(const AFilename, BaseDirectory: string;
  Flags: TFindSourceFlags): string;
var
  CompiledSrcExt: String;
  BaseDir: String;
  AlreadySearchedPaths: string;
  StartUnitPath: String;

  procedure MarkPathAsSearched(const AddSearchPath: string);
  begin
    AlreadySearchedPaths:=MergeSearchPaths(AlreadySearchedPaths,AddSearchPath);
  end;

  function SearchIndirectIncludeFile: string;
  var
    UnitPath: String;
    CurDir: String;
    AlreadySearchedUnitDirs: String;
    CompiledUnitPath: String;
    AllSrcPaths: String;
    CurSrcPath: String;
    CurIncPath: String;
    PathPos: Integer;
    AllIncPaths: String;
    SearchPath: String;
    SearchFile: String;
  begin
    if CompiledSrcExt='' then exit;
    // get unit path for compiled units
    UnitPath:=BaseDir+';'+StartUnitPath;
    UnitPath:=TrimSearchPath(UnitPath,BaseDir);

    // Extract all directories with compiled units
    CompiledUnitPath:='';
    AlreadySearchedUnitDirs:='';
    PathPos:=1;
    while PathPos<=length(UnitPath) do begin
      CurDir:=GetNextDirectoryInSearchPath(UnitPath,PathPos);
      // check if directory is already tested
      if SearchDirectoryInSearchPath(AlreadySearchedUnitDirs,CurDir,1)>0 then
        continue;
      AlreadySearchedUnitDirs:=MergeSearchPaths(AlreadySearchedUnitDirs,CurDir);
      // check if directory contains a compiled unit
      if FindFirstFileWithExt(CurDir,CompiledSrcExt)<>'' then
        CompiledUnitPath:=CompiledUnitPath+';'+CurDir;
    end;
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.SearchIndirectIncludeFile CompiledUnitPath="',CompiledUnitPath,'"');
    {$ENDIF}

    // collect all src paths for the compiled units
    AllSrcPaths:=CompiledUnitPath;
    PathPos:=1;
    while PathPos<=length(CompiledUnitPath) do begin
      CurDir:=GetNextDirectoryInSearchPath(CompiledUnitPath,PathPos);
      CurSrcPath:=CodeToolBoss.GetCompiledSrcPathForDirectory(CurDir);
      CurSrcPath:=TrimSearchPath(CurSrcPath,CurDir);
      AllSrcPaths:=MergeSearchPaths(AllSrcPaths,CurSrcPath);
    end;
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.SearchIndirectIncludeFile AllSrcPaths="',AllSrcPaths,'"');
    {$ENDIF}

    // add fpc src directories
    // ToDo

    // collect all include paths
    AllIncPaths:=AllSrcPaths;
    PathPos:=1;
    while PathPos<=length(AllSrcPaths) do begin
      CurDir:=GetNextDirectoryInSearchPath(AllSrcPaths,PathPos);
      CurIncPath:=CodeToolBoss.GetIncludePathForDirectory(CurDir);
      CurIncPath:=TrimSearchPath(CurIncPath,CurDir);
      AllIncPaths:=MergeSearchPaths(AllIncPaths,CurIncPath);
    end;
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.SearchIndirectIncludeFile AllIncPaths="',AllIncPaths,'"');
    {$ENDIF}

    SearchFile:=AFilename;
    SearchPath:=AllIncPaths;
    Result:=SearchFileInPath(SearchFile,BaseDir,SearchPath,';',[]);
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.SearchIndirectIncludeFile Result="',Result,'"');
    {$ENDIF}
    MarkPathAsSearched(SearchPath);
  end;

  function SearchInPath(const TheSearchPath, SearchFile: string;
    var Filename: string): boolean;
  var
    SearchPath: String;
  begin
    Filename:='';
    SearchPath:=RemoveSearchPaths(TheSearchPath,AlreadySearchedPaths);
    if SearchPath<>'' then begin
      Filename:=SearchFileInPath(SearchFile,BaseDir,SearchPath,';',[]);
      {$IFDEF VerboseFindSourceFile}
      writeln('TMainIDE.FindSourceFile trying "',SearchPath,'" Result=',Result);
      {$ENDIF}
      MarkPathAsSearched(SearchPath);
    end;
    Result:=Filename<>'';
  end;

var
  SearchPath: String;
  SearchFile: String;
begin
  {$IFDEF VerboseFindSourceFile}
  writeln('TMainIDE.FindSourceFile Filename="',AFilename,'" BaseDirectory="',BaseDirectory,'"');
  {$ENDIF}
  if FilenameIsAbsolute(AFilename) then begin
    if FileExists(AFilename) then
      Result:=AFilename
    else
      Result:='';
    exit;
  end;

  AlreadySearchedPaths:='';
  BaseDir:=AppendPathDelim(TrimFilename(BaseDirectory));

  // search file in base directory
  Result:=TrimFilename(BaseDir+AFilename);
  {$IFDEF VerboseFindSourceFile}
  writeln('TMainIDE.FindSourceFile trying Base "',Result,'"');
  {$ENDIF}
  if FileExists(Result) then exit;
  MarkPathAsSearched(BaseDir);

  // search file in debug path
  if fsfUseDebugPath in Flags then begin
    SearchPath:=MergeSearchPaths(Project1.CompilerOptions.DebugPath,
                                 EnvironmentOptions.DebuggerSearchPath);
    SearchPath:=TrimSearchPath(SearchPath,Project1.ProjectDirectory);
    if SearchInPath(SearchPath,AFilename,Result) then exit;
  end;

  CompiledSrcExt:=CodeToolBoss.GetCompiledSrcExtForDirectory(BaseDir);
  StartUnitPath:=CodeToolBoss.GetCompleteSrcPathForDirectory(BaseDir);
  StartUnitPath:=TrimSearchPath(StartUnitPath,BaseDir);

  // if file is a pascal unit, search via unit and src paths
  if FilenameIsPascalUnit(AFilename) then begin
    // first search file in unit path
    if SearchInPath(StartUnitPath,AFilename,Result) then exit;

    // search unit in fpc source directory
    Result:=CodeToolBoss.FindUnitInUnitLinks(BaseDir,
                                             ExtractFilenameOnly(AFilename));
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.FindSourceFile trying unit links Result=',Result);
    {$ENDIF}
    if Result<>'' then exit;
  end;

  if fsfUseIncludePaths in Flags then begin
    // search in include path
    if (fsfSearchForProject in Flags) then
      SearchPath:=Project1.CompilerOptions.GetIncludePath(false)
    else
      SearchPath:=CodeToolBoss.GetIncludePathForDirectory(BaseDir);
    SearchPath:=TrimSearchPath(SearchPath,BaseDir);
    if SearchInPath(StartUnitPath,AFilename,Result) then exit;

    // search include file in source directories of all required packages
    SearchFile:=AFilename;
    Result:=PkgBoss.FindIncludeFileInProjectDependencies(Project1,SearchFile);
    {$IFDEF VerboseFindSourceFile}
    writeln('TMainIDE.FindSourceFile trying packages "',SearchPath,'" Result=',Result);
    {$ENDIF}
    if Result<>'' then exit;

    Result:=SearchIndirectIncludeFile;
    if Result<>'' then exit;
  end;

  Result:='';
end;

function TMainIDE.FileExistsInIDE(const Filename: string;
  SearchFlags: TProjectFileSearchFlags): boolean;
begin
  Result:=FileExists(Filename)
          or (Project1.UnitInfoWithFilename(Filename,SearchFlags)<>nil);
end;

//------------------------------------------------------------------------------

procedure TMainIDE.OnDesignerGetSelectedComponentClass(Sender: TObject;
  var RegisteredComponent: TRegisteredComponent);
begin
  RegisteredComponent:=TComponentPalette(IDEComponentPalette).Selected;
end;

procedure TMainIDE.OnDesignerUnselectComponentClass(Sender: TObject);
begin
  TComponentPalette(IDEComponentPalette).Selected:=nil;
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

procedure TMainIDE.OnDesignerPasteComponent(Sender: TObject;
  LookupRoot: TComponent; TxtCompStream: TStream; ParentControl: TWinControl;
  var NewComponent: TComponent);
var
  NewClassName: String;
  ARegComp: TRegisteredComponent;
  BinCompStream: TMemoryStream;
  CInterface: TComponentInterface;
begin
  DebugLn('TMainIDE.OnDesignerPasteComponent A');
  NewComponent:=nil;

  // check the class of the new component
  NewClassName:=FindLFMClassName(TxtCompStream);

  // check if component class is registered
  ARegComp:=IDEComponentPalette.FindComponent(NewClassName);
  if ARegComp=nil then begin
    MessageDlg(lisClassNotFound,
      Format(lisClassIsNotARegisteredComponentClassUnableToPaste, ['"',
        NewClassName, '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;

  // check if there is a valid parent
  if (ParentControl=nil) and ARegComp.IsTControl then begin
    MessageDlg(lisControlNeedsParent,
      Format(lisTheClassIsATControlAndCanNotBePastedOntoANonContro, ['"',
        NewClassName, '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;

  // convert text to binary format
  BinCompStream:=TMemoryStream.Create;
  try
    try
      LRSObjectTextToBinary(TxtCompStream,BinCompStream);
    except
      on E: Exception do begin
        MessageDlg(lisConversionError,
          Format(lisUnableToConvertComponentTextIntoBinaryFormat, [#13,
            E.Message]),
          mtError,[mbCancel],0);
        exit;
      end;
    end;

    BinCompStream.Position:=0;

    // create the component
    CInterface := TComponentInterface(
                     FormEditor1.CreateChildComponentFromStream(BinCompStream,
                     ARegComp.ComponentClass,LookupRoot,ParentControl));
    if CInterface=nil then begin
      DebugLn('TMainIDE.OnDesignerPasteComponent FAILED');
      exit;
    end;
    NewComponent:=CInterface.Component;

  finally
    BinCompStream.Free;
  end;
end;

procedure TMainIDE.OnDesignerPropertiesChanged(Sender: TObject);
begin
  ObjectInspector1.RefreshPropertyValues;
end;

procedure TMainIDE.OnDesignerPersistentDeleted(Sender: TObject;
  APersistent: TPersistent);
var
  CurDesigner: TDesigner;
begin
  CurDesigner:=TDesigner(Sender);
  if dfDestroyingForm in CurDesigner.Flags then exit;
  ObjectInspector1.FillPersistentComboBox;
end;

procedure TMainIDE.OnPropHookPersistentDeleting(APersistent: TPersistent);
var
  ActiveForm: TCustomForm;
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
  OwnerClassName: string;
  CurDesigner: TDesigner;
begin
  if not (APersistent is TComponent) then exit;

  CurDesigner:=TDesigner(FindRootDesigner(TComponent(APersistent)));
  if CurDesigner=nil then exit;

  if dfDestroyingForm in CurDesigner.Flags then exit;

  if not BeginCodeTool(CurDesigner,ActiveSrcEdit,ActiveUnitInfo,
                [ctfSwitchToFormSource]) then exit;
  ActiveForm:=CurDesigner.Form;
  if ActiveForm=nil then
    RaiseException('[TMainIDE.OnPropHookPersistentDeleting] Error: TDesigner without a form');
  // find source for form
  ActiveUnitInfo:=Project1.UnitWithComponent(CurDesigner.LookupRoot);
  if ActiveUnitInfo=nil then begin
    RaiseException('[TMainIDE.OnPropHookPersistentDeleting] Error: form without source');
  end;
  if APersistent is TComponent then begin
    // remember cursor position
    SourceNotebook.AddJumpPointClicked(Self);

    // remove component definition from owner source
    OwnerClassName:=CurDesigner.LookupRoot.ClassName;
    CodeToolBoss.RemovePublishedVariable(ActiveUnitInfo.Source,OwnerClassName,
                                         TComponent(APersistent).Name,false);
  end;
end;

procedure TMainIDE.OnDesignerModified(Sender: TObject);
var
  SrcEdit: TSourceEditor;
  CurDesigner: TDesigner;
  AnUnitInfo: TUnitInfo;
begin
  CurDesigner:=TDesigner(Sender);
  if dfDestroyingForm in CurDesigner.Flags then exit;
  AnUnitInfo:=Project1.UnitWithComponent(CurDesigner.LookupRoot);
  if AnUnitInfo<>nil then begin
    AnUnitInfo.Modified:=true;
    if AnUnitInfo.Loaded then
      SrcEdit:=SourceNotebook.FindSourceEditorWithPageIndex(
                                                        AnUnitInfo.EditorIndex);
    if SrcEdit<>nil then begin
      SrcEdit.Modified:=true;
      SourceNotebook.UpdateStatusBar;
      {$IFDEF VerboseDesignerModified}
      DumpStack;
      {$ENDIF}
    end;
  end;
end;

procedure TMainIDE.OnControlSelectionChanged(Sender: TObject);
var
  NewSelection: TPersistentSelectionList;
  i: integer;
begin
  {$IFDEF IDE_DEBUG}
  writeln('[TMainIDE.OnControlSelectionChanged]');
  {$ENDIF}
  if (TheControlSelection=nil) or (FormEditor1=nil) then exit;

  NewSelection:=TPersistentSelectionList.Create;
  for i:=0 to TheControlSelection.Count-1 do
    NewSelection.Add(TheControlSelection[i].Persistent);
  FormEditor1.Selection:=NewSelection;
  NewSelection.Free;
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
  UpdateIDEComponentPalette;
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

procedure TMainIDE.OnCodeExplorerGetCodeTree(Sender: TObject;
  var ACodeTool: TCodeTool);
var
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
begin
  ACodeTool:=nil;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  CodeToolBoss.Explore(ActiveUnitInfo.Source,ACodeTool,false);
end;

procedure TMainIDE.OnCodeExplorerGetDirectivesTree(Sender: TObject;
  var ADirectivesTool: TDirectivesTool);
var
  ActiveUnitInfo: TUnitInfo;
  ActiveSrcEdit: TSourceEditor;
begin
  ADirectivesTool:=nil;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  CodeToolBoss.ExploreDirectives(ActiveUnitInfo.Source,ADirectivesTool);
end;

procedure TMainIDE.OnCodeExplorerJumpToCode(Sender: TObject;
  const Filename: string; const Caret: TPoint; TopLine: integer);
begin
  DoJumpToSourcePosition(Filename,Caret.X,Caret.Y,TopLine,true);
end;

procedure TMainIDE.ViewProjectTodosOpenFile(Sender: TObject;
  const Filename: string; const LineNumber: integer);
begin
  DoJumpToSourcePosition(Filename,1,LineNumber,-1,true);
end;

procedure TMainIDE.OnCodeToolNeedsExternalChanges(Manager: TCodeToolManager;
  var Abort: boolean);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  Abort:=not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]);
end;

// -----------------------------------------------------------------------------

procedure TMainIDE.InitCodeToolBoss;
// initialize the CodeToolBoss, which is the frontend for the codetools.
//  - sets a basic set of compiler macros

  procedure AddTemplate(ADefTempl: TDefineTemplate; AddToPool: boolean;
    const ErrorMsg: string);
  begin
    if ADefTempl=nil then begin
      DebugLn('');
      DebugLn(ErrorMsg);
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
  TargetOS, TargetProcessor: string;
begin
  OpenEditorsOnCodeToolChange:=false;

  CodeToolBoss.SourceCache.ExpirationTimeInDays:=365;
  CodeToolBoss.DefineTree.OnGetVirtualDirectoryAlias:=
    @CodeToolBossGetVirtualDirectoryAlias;
  CodeToolBoss.DefineTree.OnGetVirtualDirectoryDefines:=
    @CodeToolBossGetVirtualDirectoryDefines;
  CodeToolBoss.DefineTree.OnPrepareTree:=@CodeToolBossPrepareTree;

  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PROJECT',nil,@CTMacroFunctionProject);

  CodeToolsOpts.AssignTo(CodeToolBoss);
  if (not FileExists(EnvironmentOptions.CompilerFilename)) then begin
    DebugLn('');
    DebugLn('NOTE: Compiler Filename not set! (see Environment Options)');
  end;

  if (EnvironmentOptions.LazarusDirectory='')
  or not DirPathExists(EnvironmentOptions.LazarusDirectory) then begin
    DebugLn('');
    DebugLn(
      'NOTE: Lazarus Source Directory not set!  (see Environment Options)');
  end;
  if (EnvironmentOptions.FPCSourceDirectory='')
  or not DirPathExists(EnvironmentOptions.FPCSourceDirectory) then begin
    DebugLn('');
    DebugLn('NOTE: FPC Source Directory not set! (see Environment Options)');
  end;

  // set global variables
  with CodeToolBoss.GlobalValues do begin
    Variables[ExternalMacroStart+'LazarusDir']:=
      EnvironmentOptions.LazarusDirectory;
    Variables[ExternalMacroStart+'FPCSrcDir']:=
      EnvironmentOptions.FPCSourceDirectory;
    Variables[ExternalMacroStart+'ProjPath']:=VirtualDirectory;
    Variables[ExternalMacroStart+'LCLWidgetType']:=
      LCLPlatformDirNames[GetDefaultLCLWidgetType];
  end;

  // build DefinePool and Define Tree
  UpdateEnglishErrorMsgFilename;
  with CodeToolBoss.DefinePool do begin
    // start the compiler and ask for his settings
    TargetOS:='';
    TargetProcessor:='';
    MainBuildBoss.CurDefinesCompilerFilename:=EnvironmentOptions.CompilerFilename;
    MainBuildBoss.CurDefinesCompilerOptions:='';
    MainBuildBoss.GetFPCCompilerParamsForEnvironmentTest(
                                       MainBuildBoss.CurDefinesCompilerOptions);
    //DebugLn('TMainIDE.InitCodeToolBoss CurDefinesCompilerOptions="',CurDefinesCompilerOptions,'"');
    ADefTempl:=CreateFPCTemplate(MainBuildBoss.CurDefinesCompilerFilename,
                       MainBuildBoss.CurDefinesCompilerOptions,
                       CreateCompilerTestPascalFilename,CompilerUnitSearchPath,
                       TargetOS,TargetProcessor,CodeToolsOpts);
    AddTemplate(ADefTempl,false,
      'NOTE: Could not create Define Template for Free Pascal Compiler');

    // create compiler macros to simulate the Makefiles of the FPC sources
    InputHistories.FPCConfigCache.CompilerPath:=
                                            EnvironmentOptions.CompilerFilename;
    CompilerUnitLinks:=InputHistories.FPCConfigCache.GetUnitLinks('');
    UnitLinksChanged:=InputHistories.LastFPCUnitLinksNeedsUpdate('',
                  CompilerUnitSearchPath,EnvironmentOptions.FPCSourceDirectory);
    ADefTempl:=CreateFPCSrcTemplate(
            CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'FPCSrcDir'],
            CompilerUnitSearchPath,
            CodeToolBoss.GetCompiledSrcExtForDirectory(''),
            TargetOS,TargetProcessor,
            not UnitLinksChanged,CompilerUnitLinks,
            CodeToolsOpts);

    // save unitlinks
    if UnitLinksChanged
    or (CompilerUnitLinks<>InputHistories.FPCConfigCache.GetUnitLinks(''))
    then begin
      InputHistories.SetLastFPCUnitLinks(EnvironmentOptions.CompilerFilename,
                                         '', // default options ''
                                         CompilerUnitSearchPath,
                                         EnvironmentOptions.FPCSourceDirectory,
                                         CompilerUnitLinks);
      InputHistories.Save;
    end;
    AddTemplate(ADefTempl,false,
      lisNOTECouldNotCreateDefineTemplateForFreePascal);

    // create compiler macros for the lazarus sources
    ADefTempl:=CreateLazarusSrcTemplate(
      '$('+ExternalMacroStart+'LazarusDir)',
      '$('+ExternalMacroStart+'LCLWidgetType)',
      MiscellaneousOptions.BuildLazOpts.ExtraOptions,CodeToolsOpts);
    AddTemplate(ADefTempl,true,
      lisNOTECouldNotCreateDefineTemplateForLazarusSources);
  end;

  // load include file relationships
  AFilename:=AppendPathDelim(GetPrimaryConfigPath)+CodeToolsIncludeLinkFile;
  if FileExists(AFilename) then
    CodeToolBoss.SourceCache.LoadIncludeLinksFromFile(AFilename);


  with CodeToolBoss do begin
    WriteExceptions:=true;
    CatchExceptions:=true;
    OnGatherExternalChanges:=@OnCodeToolNeedsExternalChanges;
    OnBeforeApplyChanges:=@OnBeforeCodeToolBossApplyChanges;
    OnAfterApplyChanges:=@OnAfterCodeToolBossApplyChanges;
    OnSearchUsedUnit:=@OnCodeToolBossSearchUsedUnit;
    OnFindDefineProperty:=@OnCodeToolBossFindDefineProperty;
    OnGetMethodName:=@OnPropHookGetMethodName;
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

function TMainIDE.BeginCodeTools: boolean;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  Result:=BeginCodeTool(nil,ActiveSrcEdit,ActiveUnitInfo,
                        [ctfSourceEditorNotNeeded]);
end;

procedure TMainIDE.OnBeforeCodeToolBossApplyChanges(Manager: TCodeToolManager;
  var Abort: boolean);
// the CodeToolBoss built a list of Sources that will be modified
// 1. open all of them in the source notebook
// 2. lock the editors to reduce repaints and undo steps
var
  i: integer;
  Flags: TOpenFlags;
  CodeBuf: TCodeBuffer;
begin
  if OpenEditorsOnCodeToolChange then begin
    // open all sources in editor
    for i:=0 to Manager.SourceChangeCache.BuffersToModifyCount-1 do begin
      CodeBuf:=Manager.SourceChangeCache.BuffersToModify[i];
      //DebugLn(['TMainIDE.OnBeforeCodeToolBossApplyChanges i=',i,' ',CodeBUf.Filename]);
      Flags:=[ofOnlyIfExists,ofDoNotLoadResource];
      if CodeBuf.IsVirtual then
        Include(Flags,ofVirtualFile);
      if DoOpenEditorFile(Manager.SourceChangeCache.BuffersToModify[i].Filename,
        -1,Flags)<>mrOk then
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
var
  i: Integer;
  SrcBuf: TCodeBuffer;
  AnUnitInfo: TUnitInfo;
begin
  for i:=0 to CodeToolBoss.SourceChangeCache.BuffersToModifyCount-1 do begin
    SrcBuf:=CodeToolBoss.SourceChangeCache.BuffersToModify[i];
    AnUnitInfo:=Project1.UnitInfoWithFilename(SrcBuf.Filename);
    if AnUnitInfo<>nil then
      AnUnitInfo.Modified:=true;
  end;
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

procedure TMainIDE.OnCodeToolBossFindDefineProperty(Sender: TObject;
  const PersistentClassName, AncestorClassName, Identifier: string;
  var IsDefined: boolean);
begin
  FormEditor1.FindDefineProperty(PersistentClassName,AncestorClassName,
                                 Identifier,IsDefined);
end;

procedure TMainIDE.CodeToolBossPrepareTree(Sender: TObject);
begin
  if FRebuildingCompilerGraphCodeToolsDefinesNeeded then begin
    FRebuildingCompilerGraphCodeToolsDefinesNeeded:=false;
    CodeToolBoss.DefineTree.ClearCache;
    if Project1<>nil then
      Project1.DefineTemplates.AllChanged;
    PkgBoss.RebuildDefineTemplates;
    //DebugLn('TMainIDE.CodeToolBossPrepareTree CompilerGraphStamp=',dbgs(CompilerGraphStamp));
  end;
end;

function TMainIDE.CTMacroFunctionProject(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  Param: String;
begin
  Result:=true;
  if Project1=nil then exit;
  FuncData:=PReadFunctionData(Data);
  Param:=FuncData^.Param;
  //debugln('TMainIDE.MacroFunctionProject A Param="',Param,'"');
  if CompareText(Param,'SrcPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetSrcPath(false)
  else if CompareText(Param,'IncPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetIncludePath(false)
  else if CompareText(Param,'UnitPath')=0 then
    FuncData^.Result:=Project1.CompilerOptions.GetUnitPath(false)
  else begin
    FuncData^.Result:='<unknown parameter for CodeTools Macro project:"'+Param+'">';
    debugln('TMainIDE.MacroFunctionProject WARNING: ',FuncData^.Result);
  end;
end;

procedure TMainIDE.OnCompilerGraphStampIncreased;
begin
  FRebuildingCompilerGraphCodeToolsDefinesNeeded:=true;
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
  // check global stati
  if (ToolStatus in [itCodeTools,itCodeToolAborting]) then begin
    debugln('TMainIDE.BeginCodeTool impossible ',dbgs(ord(ToolStatus)));
    exit;
  end;
  if (not (ctfSourceEditorNotNeeded in Flags)) and (SourceNoteBook.NoteBook=nil)
  then begin
    DebugLn('TMainIDE.BeginCodeTool no editor');
    exit;
  end;

  // check source editor
  if ctfSwitchToFormSource in Flags then
    DoSwitchToFormSrc(ADesigner,ActiveSrcEdit,ActiveUnitInfo)
  else if ADesigner<>nil then
    GetDesignerUnit(ADesigner,ActiveSrcEdit,ActiveUnitInfo)
  else
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if (not (ctfSourceEditorNotNeeded in Flags))
  and ((ActiveSrcEdit=nil) or (ActiveUnitInfo=nil)) then exit;

  // init codetools
  SaveSourceEditorChangesToCodeCache(-1);
  if ActiveSrcEdit<>nil then begin
    CodeToolBoss.VisibleEditorLines:=ActiveSrcEdit.EditorComponent.LinesInWindow;
    CodeToolBoss.TabWidth:=ActiveSrcEdit.EditorComponent.TabWidth;
    CodeToolBoss.IndentSize:=ActiveSrcEdit.EditorComponent.BlockIndent;
  end else begin
    CodeToolBoss.VisibleEditorLines:=25;
    CodeToolBoss.TabWidth:=EditorOpts.TabWidth;
    CodeToolBoss.IndentSize:=EditorOpts.BlockIndent;
  end;

  if ctfActivateAbortMode in Flags then
    ActivateCodeToolAbortableMode;

  Result:=true;
end;

function TMainIDE.DoJumpToSourcePosition(const Filename: string; NewX, NewY,
  NewTopLine: integer; AddJumpPoint: boolean): TModalResult;
var
  CodeBuffer: TCodeBuffer;
begin
  Result:=mrCancel;
  CodeBuffer:=CodeToolBoss.LoadFile(CleanAndExpandFilename(Filename),true,false);
  if CodeBuffer=nil then exit;
  Result:=DoJumpToCodePos(nil,nil,CodeBuffer,NewX,NewY,NewTopLine,AddJumpPoint);
end;

function TMainIDE.DoJumpToCodePos(
  ActiveSrcEdit: TSourceEditor; ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
  AddJumpPoint: boolean): TModalResult;
var
  NewSrcEdit: TSourceEditor;
  NewUnitInfo: TUnitInfo;
begin
  Result:=mrCancel;
  if NewSource=nil then begin
    DebugLn(['TMainIDE.DoJumpToCodePos ERROR: missing NewSource']);
    DumpStack;
    exit;
  end;
  
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) then
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
  if AddJumpPoint then begin
    if (NewSource<>ActiveUnitInfo.Source)
    or (ActiveSrcEdit.EditorComponent.CaretX<>NewX)
    or (ActiveSrcEdit.EditorComponent.CaretY<>NewY) then
      SourceNotebook.AddJumpPointClicked(Self);
  end;
  if NewSource<>ActiveUnitInfo.Source then begin
    // jump to other file -> open it
    Result:=DoOpenEditorFile(NewSource.Filename,-1,[ofOnlyIfExists,ofRegularFile]);
    if Result<>mrOk then begin
      UpdateSourceNames;
      exit;
    end;
    GetUnitWithPageIndex(SourceNoteBook.NoteBook.PageIndex,NewSrcEdit,
      NewUnitInfo);
  end else begin
    NewSrcEdit:=ActiveSrcEdit;
  end;
  if NewX<1 then NewX:=1;
  if NewY<1 then NewY:=1;
  if NewTopLine<1 then
    NewTopLine:=Max(1,NewY-(NewSrcEdit.EditorComponent.LinesInWindow div 2));
  //debugln(['[TMainIDE.DoJumpToCodePos] ',NewX,',',NewY,',',NewTopLine]);
  with NewSrcEdit.EditorComponent do begin
    MoveLogicalCaretIgnoreEOL(Point(NewX,NewY));
    BlockBegin:=LogicalCaretXY;
    BlockEnd:=BlockBegin;
    TopLine:=NewTopLine;
    //DebugLn('TMainIDE.DoJumpToCodePos NewY=',dbgs(NewY),' ',dbgs(TopLine),' ',dbgs(NewTopLine));
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
        AnUnitInfo.ReadUnitNameFromSource(true);
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
  OpenFlags: TOpenFlags;
  ErrorFilename: string;
  ErrorTopLine: integer;
begin
  if CodeToolBoss.ErrorMessage='' then begin
    UpdateSourceNames;
    debugln('TMainIDE.DoJumpToCodeToolBossError No errormessage');
    exit;
  end;
  // syntax error -> show error and jump
  // show error in message view
  DoArrangeSourceEditorAndMessageView(false);
  MessagesView.ClearTillLastSeparator;
  MessagesView.AddSeparator;
  if CodeToolBoss.ErrorCode<>nil then begin
    MessagesView.AddMsg(Project1.RemoveProjectPathFromFilename(
       CodeToolBoss.ErrorCode.Filename)
      +'('+IntToStr(CodeToolBoss.ErrorLine)
      +','+IntToStr(CodeToolBoss.ErrorColumn)
      +') Error: '+CodeToolBoss.ErrorMessage,
      Project1.ProjectDirectory,-1);
  end else
    MessagesView.AddMsg(CodeToolBoss.ErrorMessage,Project1.ProjectDirectory,-1);
  MessagesView.SelectedMessageIndex:=MessagesView.MsgCount-1;

  // jump to error in source editor
  if CodeToolBoss.ErrorCode<>nil then begin
    ErrorCaret:=Point(CodeToolBoss.ErrorColumn,CodeToolBoss.ErrorLine);
    ErrorFilename:=CodeToolBoss.ErrorCode.Filename;
    ErrorTopLine:=CodeToolBoss.ErrorTopLine;
    SourceNotebook.AddJumpPointClicked(Self);
    OpenFlags:=[ofOnlyIfExists,ofUseCache];
    if CodeToolBoss.ErrorCode.IsVirtual then
      Include(OpenFlags,ofVirtualFile);
    if DoOpenEditorFile(ErrorFilename,-1,OpenFlags)=mrOk
    then begin
      ActiveSrcEdit:=SourceNoteBook.GetActiveSE;
      MessagesView.ShowOnTop;
      SourceNoteBook.ShowOnTop;
      with ActiveSrcEdit.EditorComponent do begin
        LogicalCaretXY:=ErrorCaret;
        BlockBegin:=ErrorCaret;
        BlockEnd:=ErrorCaret;
        if ErrorTopLine>0 then
          TopLine:=ErrorTopLine;
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
  DoFindDeclarationAtCaret(ActiveSrcEdit.EditorComponent.LogicalCaretXY);
end;

procedure TMainIDE.DoFindDeclarationAtCaret(const LogCaretXY: TPoint);
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
    LogCaretXY.X,LogCaretXY.Y,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, true);
  end else begin
    DoJumpToCodeToolBossError;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoFindDeclarationAtCaret B');{$ENDIF}
end;

{-------------------------------------------------------------------------------
  function TMainIDE.DoFindRenameIdentifier(Rename: boolean): TModalResult;

-------------------------------------------------------------------------------}
function TMainIDE.DoFindRenameIdentifier(Rename: boolean): TModalResult;
var
  Options: TFindRenameIdentifierOptions;

  // TODO: replace Files: TStringsList with a AVL tree

  function AddExtraFiles(Files: TStrings): TModalResult;
  var
    i: Integer;
    CurFileMask: string;
    FileInfo: TSearchRec;
    CurDirectory: String;
    CurFilename: String;
  begin
    Result:=mrCancel;
    if (Options.ExtraFiles=nil) then begin
      for i:=0 to Options.ExtraFiles.Count-1 do begin
        CurFileMask:=Options.ExtraFiles[i];
        if not GlobalMacroList.SubstituteStr(CurFileMask) then exit;
        if SysUtils.FindFirst(CurFileMask,faAnyFile,FileInfo)=0
        then begin
          CurDirectory:=AppendPathDelim(ExtractFilePath(CurFileMask));
          if not FilenameIsAbsolute(CurDirectory) then begin
            CurDirectory:=AppendPathDelim(Project1.ProjectDirectory)
                          +CurDirectory;
          end;
          repeat
            // check if special file
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
            then
              continue;
            CurFilename:=CurDirectory+FileInfo.Name;
            if FileIsText(CurFilename) then
              Files.Add(CurFilename);
          until SysUtils.FindNext(FileInfo)<>0;
        end;
        SysUtils.FindClose(FileInfo);
      end;
    end;
    Result:=mrOk;
  end;

var
  TargetSrcEdit, DeclarationSrcEdit: TSourceEditor;
  TargetUnitInfo, DeclarationUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  LogCaretXY, DeclarationCaretXY: TPoint;
  OwnerList: TFPList;
  ExtraFiles: TStrings;
  Files: TStringList;
  Identifier: string;
  TreeOfPCodeXYPosition: TAVLTree;
begin
  Result:=mrCancel;
  if not BeginCodeTool(TargetSrcEdit,TargetUnitInfo,[]) then exit;

  // find the main declaration
  LogCaretXY:=TargetSrcEdit.EditorComponent.LogicalCaretXY;
  if not CodeToolBoss.FindMainDeclaration(TargetUnitInfo.Source,
    LogCaretXY.X,LogCaretXY.Y,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodeToolBossError;
    exit;
  end;
  DoJumpToCodePos(TargetSrcEdit, TargetUnitInfo,
    NewSource, NewX, NewY, NewTopLine, true);
  CodeToolBoss.GetIdentifierAt(NewSource,NewX,NewY,Identifier);

  GetCurrentUnit(DeclarationSrcEdit,DeclarationUnitInfo);
  DeclarationCaretXY:=DeclarationSrcEdit.EditorComponent.LogicalCaretXY;
  debugln('TMainIDE.DoFindRenameIdentifier A DeclarationCaretXY=x=',dbgs(DeclarationCaretXY.X),' y=',dbgs(DeclarationCaretXY.Y));

  // let user choose the search scope
  Result:=ShowFindRenameIdentifierDialog(DeclarationUnitInfo.Source.Filename,
    DeclarationCaretXY,Rename,Rename,nil);
  if Result<>mrOk then begin
    debugln('TMainIDE.DoFindRenameIdentifier failed: let user choose the search scope');
    exit;
  end;

  Files:=nil;
  OwnerList:=nil;
  TreeOfPCodeXYPosition:=nil;
  try
    // create the file list
    Files:=TStringList.Create;
    Files.Add(TargetUnitInfo.Filename);
    if CompareFilenames(DeclarationUnitInfo.Filename,TargetUnitInfo.Filename)<>0
    then
      Files.Add(DeclarationUnitInfo.Filename);

    Options:=MiscellaneousOptions.FindRenameIdentifierOptions;

    // add packages, projects
    case Options.Scope of
    frProject:
      begin
        OwnerList:=TFPList.Create;
        OwnerList.Add(Project1);
      end;
    frOwnerProjectPackage,frAllOpenProjectsAndPackages:
      begin
        OwnerList:=PkgBoss.GetOwnersOfUnit(TargetUnitInfo.Filename);
        if (OwnerList<>nil)
        and (Options.Scope=frAllOpenProjectsAndPackages) then begin
          PkgBoss.ExtendOwnerListWithUsedByOwners(OwnerList);
          ReverseList(OwnerList);
        end;
      end;
    end;

    // get source files of packages and projects
    if OwnerList<>nil then begin
      ExtraFiles:=PkgBoss.GetSourceFilesOfOwners(OwnerList);
      try
        if ExtraFiles<>nil then
          Files.AddStrings(ExtraFiles);
      finally
        ExtraFiles.Free;
      end;
    end;

    // add user defined extra files
    Result:=AddExtraFiles(Files);
    if Result<>mrOk then begin
      debugln('TMainIDE.DoFindRenameIdentifier unable to add user defined extra files');
      exit;
    end;

    // gather identifiers
    Result:=GatherIdentifierReferences(Files,DeclarationUnitInfo.Source,
      DeclarationCaretXY,Options.SearchInComments,TreeOfPCodeXYPosition);
    if CodeToolBoss.ErrorMessage<>'' then
      DoJumpToCodeToolBossError;
    if Result<>mrOk then begin
      debugln('TMainIDE.DoFindRenameIdentifier unable to gather identifiers');
      exit;
    end;

    // show result
    if (not Options.Rename) or (not Rename) then begin
      CreateSearchResultWindow;
      Result:=ShowIdentifierReferences(DeclarationUnitInfo.Source,
        DeclarationCaretXY,TreeOfPCodeXYPosition);
      if Result<>mrOk then exit;
    end;

    // rename identifier
    if Options.Rename and Rename then begin
      if not CodeToolBoss.RenameIdentifier(TreeOfPCodeXYPosition,
        Identifier,Options.RenameTo)
      then begin
        DoJumpToCodeToolBossError;
        debugln('TMainIDE.DoFindRenameIdentifier unable to rename identifier');
        Result:=mrCancel;
        exit;
      end;
    end;

  finally
    Files.Free;
    OwnerList.Free;
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
  end;
end;

{-------------------------------------------------------------------------------
  function TMainIDE.DoInitIdentCompletion(JumpToError: boolean): boolean;
-------------------------------------------------------------------------------}
function TMainIDE.DoInitIdentCompletion(JumpToError: boolean): boolean;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  LogCaretXY: TPoint;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoInitIdentCompletion] ************');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoInitIdentCompletion A');{$ENDIF}
  LogCaretXY:=ActiveSrcEdit.EditorComponent.LogicalCaretXY;
  Result:=CodeToolBoss.GatherIdentifiers(ActiveUnitInfo.Source,
                                         LogCaretXY.X,LogCaretXY.Y);
  if not Result then begin
    if JumpToError then
      DoJumpToCodeToolBossError;
    exit;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoInitIdentCompletion B');{$ENDIF}
end;

function TMainIDE.DoShowCodeContext(JumpToError: boolean): boolean;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoShowCodeContext] ************');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoShowCodeContext A');{$ENDIF}
  Result:=ShowCodeContext(ActiveUnitInfo.Source);
  if not Result then begin
    if JumpToError then
      DoJumpToCodeToolBossError;
    exit;
  end;
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.DoShowCodeContext B');{$ENDIF}
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
  end else begin
    if CodeToolBoss.ErrorMessage='' then begin
      MessageDlg(lisSuccess, lisAllBlocksLooksOk, mtInformation, [mbOk], 0);
    end else
      DoJumpToCodeToolBossError;
  end;
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
  OldChange: Boolean;
begin
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    Result:=mrCancel;
    if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
    {$IFDEF IDE_DEBUG}
    debugln('');
    debugln('[TMainIDE.DoMakeResourceString] ************');
    {$ENDIF}
    // calculate start and end of expression in source
    CursorCode:=ActiveUnitInfo.Source;
    if ActiveSrcEdit.EditorComponent.SelAvail then
      CursorXY:=ActiveSrcEdit.EditorComponent.BlockBegin
    else
      CursorXY:=ActiveSrcEdit.EditorComponent.LogicalCaretXY;
    if not CodeToolBoss.GetStringConstBounds(
      CursorCode,CursorXY.X,CursorXY.Y,
      StartCode,StartPos.X,StartPos.Y,
      EndCode,EndPos.X,EndPos.Y,
      true) then
    begin
      DoJumpToCodeToolBossError;
      exit;
    end;

    // the codetools have calculated the maximum bounds
    if (StartCode=EndCode) and (CompareCaret(StartPos,EndPos)=0) then begin
      MessageDlg(lisNoStringConstantFound,
      Format(lisHintTheMakeResourcestringFunctionExpectsAStringCon, [#13]),
      mtError,[mbCancel],0);
      exit;
    end;
    // the user can shorten this range by selecting text
    if (ActiveSrcEdit.EditorComponent.SelText='') then begin
      // the user has not selected text
      // -> check if the string constant is in single file
      // (replacing code that contains an $include directive is ambiguous)
      //debugln('TMainIDE.DoMakeResourceString user has not selected text');
      if (StartCode<>ActiveUnitInfo.Source)
      or (EndCode<>ActiveUnitInfo.Source)
      then begin
        MessageDlg(lisNoStringConstantFound, Format(
          lisInvalidExpressionHintTheMakeResourcestringFunction, [#13]),
        mtError,[mbCancel],0);
        exit;
      end;
    end else begin
      // the user has selected text
      // -> check if the selection is only part of the maximum bounds
      SelectedStartPos:=ActiveSrcEdit.EditorComponent.BlockBegin;
      SelectedEndPos:=ActiveSrcEdit.EditorComponent.BlockEnd;
      CodeToolBoss.ImproveStringConstantStart(
                      ActiveSrcEdit.EditorComponent.Lines[SelectedStartPos.Y-1],
                      SelectedStartPos.X);
      CodeToolBoss.ImproveStringConstantEnd(
                        ActiveSrcEdit.EditorComponent.Lines[SelectedEndPos.Y-1],
                        SelectedEndPos.X);
      //debugln('TMainIDE.DoMakeResourceString user has selected text: Selected=',dbgs(SelectedStartPos),'-',dbgs(SelectedEndPos),' Maximum=',dbgs(StartPos),'-',dbgs(EndPos));
      if (CompareCaret(SelectedStartPos,StartPos)>0)
      or (CompareCaret(SelectedEndPos,EndPos)<0)
      then begin
        MessageDlg(lisSelectionExceedsStringConstant,
        Format(lisHintTheMakeResourcestringFunctionExpectsAStringCon2, [#13]),
        mtError,[mbCancel],0);
        exit;
      end;
      StartPos:=SelectedStartPos;
      EndPos:=SelectedEndPos;
    end;

    // gather all reachable resourcestring sections
    //debugln('TMainIDE.DoMakeResourceString gather all reachable resourcestring sections ...');
    if not CodeToolBoss.GatherResourceStringSections(
      CursorCode,CursorXY.X,CursorXY.Y,nil)
    then begin
      DoJumpToCodeToolBossError;
      exit;
    end;
    if CodeToolBoss.Positions.Count=0 then begin
      MessageDlg(lisNoResourceStringSectionFound,
        lisUnableToFindAResourceStringSectionInThisOrAnyOfThe,
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
    OpenEditorsOnCodeToolChange:=OldChange;
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
    Result:=DoNewEditorFile(FileDescriptorText,NewDiffFilename,DiffText,
                            [nfOpenInEditor]);
    GetCurrentUnit(ActiveSrcEdit,ActiveUnitInfo);
    if ActiveSrcEdit=nil then exit;
  end;
end;

function TMainIDE.DoFindInFiles: TModalResult;
begin
  Result:=mrOk;
  DoArrangeSourceEditorAndMessageView(true);
  SourceNotebook.FindInFilesPerDialog(Project1);
end;

procedure TMainIDE.DoCompleteCodeAtCursor;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  OldChange: Boolean;
begin
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
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
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

procedure TMainIDE.DoExtractProcFromSelection;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  BlockBegin: TPoint;
  BlockEnd: TPoint;
  NewSource: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
  CTResult: boolean;
  OldChange: Boolean;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[]) then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.DoExtractProcFromSelection] ************');
  {$ENDIF}
  BlockBegin:=ActiveSrcEdit.EditorComponent.BlockBegin;
  BlockEnd:=ActiveSrcEdit.EditorComponent.BlockEnd;

  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    CTResult:=ShowExtractProcDialog(ActiveUnitInfo.Source,BlockBegin,BlockEnd,
      NewSource,NewX,NewY,NewTopLine)=mrOk;
    ApplyCodeToolChanges;
    if CodeToolBoss.ErrorMessage<>'' then begin
      DoJumpToCodeToolBossError;
    end else if CTResult then begin
      DoJumpToCodePos(ActiveSrcEdit,ActiveUnitInfo,
        NewSource,NewX,NewY,NewTopLine,true);
    end;
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

//-----------------------------------------------------------------------------

procedure TMainIDE.MessagesViewSelectionChanged(sender: TObject);
begin
  DoJumpToCompilerMessage(TMessagesView(Sender).SelectedMessageIndex,True);
end;

procedure TMainIDE.SearchResultsViewSelectionChanged(sender: TObject);
begin
  DoJumpToSearchResult(True);
end;

Procedure TMainIDE.OnSrcNotebookEditorVisibleChanged(Sender: TObject);
var
  ActiveUnitInfo: TUnitInfo;
begin
  if SourceNotebook.Notebook = nil then Exit;

  ActiveUnitInfo :=
    Project1.UnitWithEditorIndex(SourceNotebook.Notebook.PageIndex);
  if ActiveUnitInfo = nil then Exit;

  UpdateSaveMenuItemsAndButtons(false);
  MainIDEBar.ToggleFormSpeedBtn.Enabled := Assigned(ActiveUnitInfo.Component)
                                          or (ActiveUnitInfo.ComponentName<>'');
end;

//this is fired when the editor is focused, changed, ?.  Anything that causes the status change
Procedure TMainIDE.OnSrcNotebookEditorChanged(Sender: TObject);
begin
  if SourceNotebook.Notebook = nil then Exit;
  UpdateSaveMenuItemsAndButtons(false);
end;

procedure TMainIDE.OnSrcNotebookCurCodeBufferChanged(Sender: TObject);
begin
  if SourceNotebook.Notebook = nil then Exit;
  if CodeExplorerView<>nil then CodeExplorerView.CurrentCodeBufferChanged;
end;

procedure TMainIDE.OnSrcNotebookShowHintForSource(SrcEdit: TSourceEditor;
  ClientPos: TPoint; CaretPos: TPoint);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  Identifier, SmartHintStr: string;
  Expression, DebugEval: string;
begin
  if (SrcEdit=nil) then exit;

  // check if there is an identifier
  case ToolStatus of
    itNone: begin
      Identifier := SrcEdit.GetWordFromCaret(CaretPos);
      if (Identifier='') or (not IsValidIdent(Identifier)) then exit;
    end;
    itDebugger: begin
//      Identifier := SrcEdit.GetWordFromCaretEx(CaretPos,
//        ['A'..'Z', 'a'..'z', '0'..'9', '(', '[', '.', ''''],
//        ['A'..'Z', 'a'..'z', '0'..'9', ')', ']', '^', '''']);
      Identifier := SrcEdit.GetWordFromCaret(CaretPos);
      if Identifier = '' then Exit;
    end;
  else
    Exit;
  end;
  SourceNotebook.SetActiveSE(SrcEdit);

  if not BeginCodeTool(ActiveSrcEdit, ActiveUnitInfo,
    [{ctfActivateAbortMode}]) then exit;

  case ToolStatus of
    itNone: begin
      {$IFDEF IDE_DEBUG}
      writeln('');
      writeln('[TMainIDE.OnSrcNotebookShowHintForSource] ************ ',ActiveUnitInfo.Source.Filename,' X=',CaretPos.X,' Y=',CaretPos.Y);
      {$ENDIF}
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.OnSrcNotebookShowHintForSource A');{$ENDIF}
      SmartHintStr:=CodeToolBoss.FindSmartHint(ActiveUnitInfo.Source,
        CaretPos.X,CaretPos.Y);
      CodeToolBoss.Abortable:=false;
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TMainIDE.OnSrcNotebookShowHintForSource B');{$ENDIF}
    end;
    itDebugger: begin
      if SrcEdit.SelectionAvailable
      and SrcEdit.CaretInSelection(CaretPos)
      then Expression := SrcEdit.GetText(True)
      else Expression := Identifier;
      if not DebugBoss.Evaluate(Expression, DebugEval)
      or (DebugEval = '')
      then DebugEval := '???';
      SmartHintStr := Expression + ' = ' + DebugEval;
    end;
  else
    Exit;
  end;

  if SmartHintStr<>'' then
    SrcEdit.ActivateHint(ClientPos,SmartHintStr);
end;

procedure TMainIDE.OnSrcNoteBookActivated(Sender: TObject);
begin
  FDisplayState:= dsSource;
end;

Procedure TMainIDE.OnDesignerActivated(Sender: TObject);
begin
  FDisplayState:= dsForm;
  FLastFormActivated := (Sender as TDesigner).Form;
  UpdateIDEComponentPalette;
end;

procedure TMainIDE.OnDesignerCloseQuery(Sender: TObject);
var
  ADesigner: TDesigner;
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  ADesigner:=TDesigner(Sender);
  GetDesignerUnit(ADesigner,ASrcEdit,AnUnitInfo);
  if AnUnitInfo.NeedsSaveToDisk
  then begin
    case MessageDlg(lisSaveChanges,
                    Format(lisSaveFileBeforeClosingForm, ['"',
                      AnUnitInfo.Filename, '"', #13, '"',
                      ADesigner.LookupRoot.Name, '"']),
                   mtConfirmation,[mbYes,mbNo,mbCancel],0) of
      mrYes: begin
        if DoSaveEditorFile(AnUnitInfo.EditorIndex,[sfCheckAmbiguousFiles])<>mrOk
        then Exit;
      end;
      mrNo:;
    else
      Exit;
    end;
  end;
  CloseUnitComponent(AnUnitInfo,[]);
end;

procedure TMainIDE.OnDesignerRenameComponent(ADesigner: TDesigner;
  AComponent: TComponent; const NewName: string);
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  NewClassName: string;
  BossResult: boolean;
  AncestorRoot: TComponent;
  s: String;
  OldName: String;
  OldClassName: String;
  OldOpenEditorsOnCodeToolChange: Boolean;

  procedure ApplyBossResult(const ErrorMsg: string);
  var
    CodeToolBossErrMsg: String;
  begin
    ApplyCodeToolChanges;
    if not BossResult then begin
      CodeToolBossErrMsg:=CodeToolBoss.ErrorMessage;
      DoJumpToCodeToolBossError;
      raise Exception.Create(ErrorMsg+#13#13+lisError+CodeToolBossErrMsg
                             +#13#13+lisSeeMessages);
    end;
  end;

  procedure CheckInterfaceName(const AName: string);
  var
    i: LongInt;
  begin
    if CompareText(ActiveUnitInfo.UnitName,AName)=0 then
      raise Exception.Create(Format(
        lisTheUnitItselfHasAlreadyTheNamePascalIdentifiersMus, ['"', AName, '"']
        ));
    if ActiveUnitInfo.IsPartOfProject then begin
      // check if component name already exists in project
      i:=Project1.IndexOfUnitWithComponentName(AName,true,ActiveUnitInfo);
      if i>=0 then
        raise Exception.Create(
                           Format(lisThereIsAlreadyAFormWithTheName, ['"',
                             AName, '"']));
      // check if pascal identifier already exists in the units
      i:=Project1.IndexOfUnitWithName(AName,true,nil);
      if i>=0 then
        raise Exception.Create(Format(
          lisThereIsAlreadyAUnitWithTheNamePascalIdentifiersMus, ['"', AName,
          '"']));
    end;

  end;
  
  procedure RenameInheritedComponents(RenamedUnit: TUnitInfo;
    Simulate: boolean);
  var
    UsedByDependency: TUnitComponentDependency;
    DependingUnit: TUnitInfo;
    InheritedComponent: TComponent;
    DependingDesigner: TCustomForm;
  begin
    UsedByDependency:=ActiveUnitInfo.FirstUsedByComponent;
    while UsedByDependency<>nil do begin
      DependingUnit:=UsedByDependency.UsedByUnit;
      if (DependingUnit.Component<>nil)
      and (DependingUnit.Component.ClassParent=RenamedUnit.Component.ClassType)
      then begin
        // the root component inherits from the DependingUnit root component
        if DependingUnit.Component.ClassParent=AComponent.ClassType then begin
          if OldClassName<>AComponent.ClassName then begin
            // replace references to classname, ignoring errors
            CodeToolBoss.ReplaceWord(DependingUnit.Source,
                                     OldClassName,AComponent.ClassName,false);
          end;
        end;
        
        // rename inherited component
        InheritedComponent:=
                         DependingUnit.Component.FindComponent(AComponent.Name);
        if InheritedComponent<>nil then begin
          // inherited component found
          if FRenamingComponents=nil then
            FRenamingComponents:=TFPList.Create;
          FRenamingComponents.Add(InheritedComponent);
          try
            DebugLn(['RenameInheritedComponents ',dbgsName(InheritedComponent),' Owner=',dbgsName(InheritedComponent.Owner)]);
            if Simulate then begin
              // only check if rename is possible
              if (InheritedComponent.Owner<>nil)
              and (InheritedComponent.Owner.FindComponent(NewName)<>nil) then
              begin
                raise EComponentError.Createfmt(
                  lisDuplicateNameAComponentNamedAlreadyExistsInTheInhe, ['"',
                  NewName, '"', dbgsName(InheritedComponent.Owner)]);
              end;
            end else begin
              // rename component and references in code
              InheritedComponent.Name:=NewName;
              DependingDesigner:=GetDesignerFormOfSource(DependingUnit,false);
              if DependingDesigner<>nil then
                DependingUnit.Modified:=true;
              // replace references, ignoring errors
              CodeToolBoss.ReplaceWord(DependingUnit.Source,OldName,NewName,
                                       false);
            end;
          finally
            if FRenamingComponents<>nil then begin
              FRenamingComponents.Remove(InheritedComponent);
              if FRenamingComponents.Count=0 then
                FreeThenNil(FRenamingComponents);
            end;
          end;
        end;
        // rename recursively
        RenameInheritedComponents(DependingUnit,Simulate);
      end;
      UsedByDependency:=UsedByDependency.NextUsedByDependency;
    end;
  end;
  
  procedure RenameMethods;
  var
    PropList: PPropList;
    PropCount: LongInt;
    i: Integer;
    PropInfo: PPropInfo;
    DefaultName: Shortstring;
    CurMethod: TMethod;
    Root: TComponent;
    CurMethodName: Shortstring;
    RootClassName: ShortString;
    NewMethodName: String;
    CTResult: Boolean;
    RenamedMethods: TStringList;
  begin
    PropCount:=GetPropList(PTypeInfo(AComponent.ClassInfo),PropList);
    if PropCount=0 then exit;
    RenamedMethods:=nil;
    try
      Root:=ActiveUnitInfo.Component;
      RootClassName:=Root.ClassName;
      if Root=AComponent then RootClassName:=OldClassName;
      for i:=0 to PropCount-1 do begin
        PropInfo:=PropList^[i];
        if PropInfo^.PropType^.Kind<>tkMethod then continue;
        CurMethod:=GetMethodProp(AComponent,PropInfo);
        if (CurMethod.Data=nil) and (CurMethod.Code=nil) then continue;
        CurMethodName:=GlobalDesignHook.GetMethodName(CurMethod,Root);
        if CurMethodName='' then continue;
        DefaultName:=TMethodPropertyEditor.GetDefaultMethodName(
                          Root,AComponent,RootClassName,OldName,PropInfo^.Name);
        if (DefaultName<>CurMethodName) then continue;
        // this method has the default name (component name + method type name)
        NewMethodName:=TMethodPropertyEditor.GetDefaultMethodName(
                       Root,AComponent,Root.ClassName,NewName,PropInfo^.Name);
        if (CurMethodName=NewMethodName) then continue;
        // auto rename it
        DebugLn(['RenameMethods OldMethodName="',DefaultName,'" NewMethodName="',NewMethodName,'"']);

        // rename/create published method in source
        CTResult:=CodeToolBoss.RenamePublishedMethod(ActiveUnitInfo.Source,
              ActiveUnitInfo.Component.ClassName,CurMethodName,NewMethodName);
        if CTResult then begin
          // renamed in source, now rename in JIT class
          FormEditor1.RenameJITMethod(ActiveUnitInfo.Component,
                                      CurMethodName,NewMethodName);
          // add to the list of renamed methods
          if RenamedMethods=nil then
            RenamedMethods:=TStringList.Create;
          RenamedMethods.Add(CurMethodName);
          RenamedMethods.Add(NewMethodName);
        end else begin
          // unable to rename method in source
          // this is just a nice to have feature -> ignore the error
          DebugLn(['TMainIDE.OnDesignerRenameComponent.RenameMethods failed OldMethodName="',CurMethodName,'" NewMethodName="',NewMethodName,'" Error=',CodeToolBoss.ErrorMessage]);
        end;
      end;
      ApplyCodeToolChanges;
    finally
      FreeMem(PropList);
      if RenamedMethods<>nil then begin
        RenameInheritedMethods(ActiveUnitInfo,RenamedMethods);
        RenamedMethods.Free;
      end;
    end;
  end;

begin
  DebugLn('TMainIDE.OnDesignerRenameComponent Old=',AComponent.Name,':',AComponent.ClassName,' New=',NewName,' Owner=',dbgsName(AComponent.Owner));
  if (not IsValidIdent(NewName)) or (NewName='') then
    raise Exception.Create(Format(lisComponentNameIsNotAValidIdentifier, ['"',
      Newname, '"']));
  if AComponent.Name='' then begin
    // this component was never added to the source. It is a new component.
    exit;
  end;

  if (FRenamingComponents<>nil)
  and (FRenamingComponents.IndexOf(AComponent)>=0) then begin
    // already validated
    exit;
  end;

  BeginCodeTool(ADesigner,ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource]);
  ActiveUnitInfo:=Project1.UnitWithComponent(ADesigner.LookupRoot);
  if CodeToolBoss.IsKeyWord(ActiveUnitInfo.Source,NewName) then
    raise Exception.Create(Format(lisComponentNameIsKeyword, ['"', Newname, '"']
      ));

  OldOpenEditorsOnCodeToolChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try

    // check ancestor component
    AncestorRoot:=FormEditor1.GetAncestorLookupRoot(AComponent);
    if AncestorRoot<>nil then begin
      s:='The component '+dbgsName(AComponent)
         +' is inherited from '+dbgsName(AncestorRoot)+'.'#13
         +'To rename an inherited component open the ancestor and rename it there.';
      raise EComponentError.Create(s);
    end;


    OldName:=AComponent.Name;
    OldClassName:=AComponent.ClassName;

    // check inherited components
    RenameInheritedComponents(ActiveUnitInfo,true);

    if AComponent=ADesigner.LookupRoot then begin
      // rename owner component (e.g. the form)

      CheckInterfaceName(NewName);
      NewClassName:='T'+NewName;
      CheckInterfaceName(NewClassName);

      // rename form component in source
      BossResult:=CodeToolBoss.RenameForm(ActiveUnitInfo.Source,
        AComponent.Name,AComponent.ClassName,
        NewName,NewClassName);
      ApplyBossResult(Format(lisUnableToRenameFormInSource, [#13]));
      ActiveUnitInfo.ComponentName:=NewName;

      // rename form component class
      FormEditor1.RenameJITComponent(AComponent,NewClassName);

      // change createform statement
      if ActiveUnitInfo.IsPartOfProject and (Project1.MainUnitID>=0)
      then begin
        BossResult:=CodeToolBoss.ChangeCreateFormStatement(
          Project1.MainUnitInfo.Source,
          AComponent.ClassName,AComponent.Name,
          NewClassName,NewName,true);
        Project1.MainUnitInfo.Modified:=true;
        ApplyBossResult(lisUnableToUpdateCreateFormStatementInProjectSource);
      end;
    end else if ADesigner.LookupRoot<>nil then begin
      // rename published variable in form source
      BossResult:=CodeToolBoss.RenamePublishedVariable(ActiveUnitInfo.Source,
        ADesigner.LookupRoot.ClassName,
        AComponent.Name,NewName,AComponent.ClassName,true);
      ApplyBossResult(Format(lisUnableToRenameVariableInSource, [#13])
        );
    end else begin
      RaiseException('TMainIDE.OnDesignerRenameComponent internal error:'+AComponent.Name+':'+AComponent.ClassName);
    end;

    // rename inherited components
    RenameInheritedComponents(ActiveUnitInfo,false);

    // rename methods
    RenameMethods;
  finally
    OpenEditorsOnCodeToolChange:=OldOpenEditorsOnCodeToolChange;
  end;
end;

procedure TMainIDE.OnDesignerViewLFM(Sender: TObject);
var
  ADesigner: TDesigner;
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  ADesigner:=TDesigner(Sender);
  GetDesignerUnit(ADesigner,ASrcEdit,AnUnitInfo);
  debugln('TMainIDE.OnDesignerViewLFM ',AnUnitInfo.Filename);
  OnDesignerCloseQuery(Sender);
  DoOpenEditorFile(ChangeFileExt(AnUnitInfo.Filename,'.lfm'),
                   AnUnitInfo.EditorIndex+1,[]);
end;

procedure TMainIDE.OnDesignerSaveAsXML(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  SaveAsFilename: String;
  SaveAsFileExt: String;
  PkgDefaultDirectory: String;
  Filename: String;
  XMLConfig: TXMLConfig;
  ADesigner: TDesigner;
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  ADesigner:=TDesigner(Sender);
  GetDesignerUnit(ADesigner,ASrcEdit,AnUnitInfo);
  debugln('TMainIDE.OnDesignerViewLFM ',AnUnitInfo.Filename);

  SaveAsFileExt:='.xml';
  SaveAsFilename:=ChangeFileExt(AnUnitInfo.Filename,SaveAsFileExt);
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:=lisSaveSpace+SaveAsFilename+' (*'+SaveAsFileExt+')';
    SaveDialog.FileName:=SaveAsFilename;
    // if this is a project file, start in project directory
    if AnUnitInfo.IsPartOfProject and (not Project1.IsVirtual)
    and (not FileIsInPath(SaveDialog.InitialDir,Project1.ProjectDirectory)) then
    begin
      SaveDialog.InitialDir:=Project1.ProjectDirectory;
    end;
    // if this is a package file, then start in package directory
    PkgDefaultDirectory:=
                    PkgBoss.GetDefaultSaveDirectoryForFile(AnUnitInfo.Filename);
    if (PkgDefaultDirectory<>'')
    and (not FileIsInPath(SaveDialog.InitialDir,PkgDefaultDirectory)) then
      SaveDialog.InitialDir:=PkgDefaultDirectory;
    // show save dialog
    if (not SaveDialog.Execute) or (ExtractFileName(SaveDialog.Filename)='')
    then begin
      // user cancels
      exit;
    end;
    Filename:=ExpandFilename(SaveDialog.Filename);
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;

  try
    XMLConfig:=TXMLConfig.Create(Filename);
    try
      WriteComponentToXMLConfig(XMLConfig,'Component',ADesigner.LookupRoot);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error',E.Message,mtError,[mbCancel],0);
    end;
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
  var NewTopLine, NewPageIndex: integer;  JumpAction: TJumpHistoryAction);
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
  writeln('[TMainIDE.OnSrcNotebookJumpToHistoryPoint] A Back=',JumpAction=jhaBack);
  Project1.JumpHistory.WriteDebugReport;
  {$ENDIF}

  // update jump history (e.g. delete jumps to closed editors)
  Project1.JumpHistory.DeleteInvalidPositions;

  // get destination jump point
  DestIndex:=Project1.JumpHistory.HistoryIndex;
  if JumpAction=jhaForward then
    inc(DestIndex);
  if (DestIndex<0) or (DestIndex>=Project1.JumpHistory.Count) then exit;

  CursorPoint:=nil;
  if (SourceNoteBook<>nil) then begin
    // get current cursor position
    GetCurrentUnit(ASrcEdit,AnUnitInfo);
    if (ASrcEdit<>nil) and (AnUnitInfo<>nil) then begin
      CursorPoint:=TProjectJumpHistoryPosition.Create(AnUnitInfo.Filename,
        ASrcEdit.EditorComponent.LogicalCaretXY,
        ASrcEdit.EditorComponent.TopLine);
      {$IFDEF VerboseJumpHistory}
      writeln('  Current Position: ',CursorPoint.Filename,
              ' ',CursorPoint.CaretXY.X,',',CursorPoint.CaretXY.Y);
      {$ENDIF}
    end;
  end;

  if (JumpAction=jhaBack) and (Project1.JumpHistory.Count=DestIndex+1)
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
  if JumpAction=jhaForward then
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
      if JumpAction=jhaBack then
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
    if JumpAction=jhaBack then
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

Procedure TMainIDE.OnSrcNotebookViewJumpHistory(Sender: TObject);
begin
  // ToDo
  MessageDlg(ueNotImplCap, lisSorryNotImplementedYet, mtInformation,
     [mbOk],0);
end;

procedure TMainIDE.OnSrcNotebookShowSearchResultsView(Sender: TObject);
begin
  CreateSearchResultWindow;
end;

procedure TMainIDE.OnSrcNoteBookPopupMenu(
  const AddMenuItemProc: TAddMenuItemProc);
begin
  PkgBoss.OnSourceEditorPopupMenu(AddMenuItemProc);
end;

procedure TMainIDE.OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
begin
  if ToolStatus=itCodeTools then begin
    // abort codetools
    ToolStatus:=itCodeToolAborting;
  end;
end;

procedure TMainIDE.OnApplicationIdle(Sender: TObject; var Done: Boolean);
var
  SrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
  AnIDesigner: TIDesigner;
begin
  UpdateWindowsMenu;
  GetDefaultProcessList.FreeStoppedProcesses;
  EnvironmentOptions.ExternalTools.FreeStoppedProcesses;
  if (SplashForm<>nil) then FreeThenNil(SplashForm);
  FormEditor1.CheckDesignerPositions;
  FormEditor1.PaintAllDesignerItems;
  GetCurrentUnit(SrcEdit,AnUnitInfo);
  UpdateSaveMenuItemsAndButtons(true);
  if Screen.ActiveForm<>nil then begin
    AnIDesigner:=Screen.ActiveForm.Designer;
    if AnIDesigner is TDesigner then begin
      MainIDEBar.ToggleFormSpeedBtn.Enabled:=true;
    end else begin
      MainIDEBar.ToggleFormSpeedBtn.Enabled:=(AnUnitInfo<>nil)
                                             and AnUnitInfo.HasResources;
    end;
  end;

  if FCheckFilesOnDiskNeeded then
    DoCheckFilesOnDisk(true);
    
  if (FRemoteControlTimer=nil) and EnableRemoteControl then
    SetupRemoteControl;
end;

procedure TMainIDE.OnApplicationActivate(Sender: TObject);
begin
  DoCheckFilesOnDisk;
end;

procedure TMainIDE.OnApplicationKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Command: Word;
begin
  //DebugLn('TMainIDE.OnApplicationKeyDown ',dbgs(Key),' ',dbgs(Shift));
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,nil);
  if Command=ecEditContextHelp then begin
    Key:=VK_UNKNOWN;
    ShowContextHelpEditor(Sender);
  end else if Command=ecContextHelp then begin
    Key:=VK_UNKNOWN;
    ShowContextHelpForIDE(Sender);
  end;
end;

procedure TMainIDE.OnScreenRemoveForm(Sender: TObject; AForm: TCustomForm);
begin
  HiddenWindowsOnRun.Remove(AForm);
  EnvironmentOptions.IDEWindowLayoutList.CloseForm(AForm);
end;

procedure TMainIDE.OnRemoteControlTimer(Sender: TObject);
begin
  FRemoteControlTimer.Enabled:=false;
  DoExecuteRemoteControl;
  FRemoteControlTimer.Enabled:=true;
end;

procedure TMainIDE.mnuFileClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ASrcEdit,AnUnitInfo);
  with MainIDEBar do begin
    itmFileClose.Enabled := ASrcEdit<>nil;
    itmFileCloseAll.Enabled := ASrcEdit<>nil;
  end;
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
  and (pfMainUnitHasUsesSectionForAllUnits in Project1.Flags)
  then begin
    AnUnitInfo.ReadUnitNameFromSource(false);
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
  and (pfMainUnitHasUsesSectionForAllUnits in Project1.Flags)
  then begin
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
    if (AnUnitInfo.ComponentName<>'') then begin
      Dummy:=Project1.RemoveCreateFormFromProjectFile(
          'T'+AnUnitInfo.ComponentName,AnUnitInfo.ComponentName);
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

procedure TMainIDE.OnCompilerOptionsDialogTest(Sender: TObject);
begin
  DoTestCompilerSettings(Sender as TCompilerOptions);
end;

procedure TMainIDE.OnCompilerOptionsImExport(Sender: TObject);
begin
  DoImExportCompilerOptions(Sender);
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
  if ToolStatus<>itNone then begin
    Abort:=true;
    exit;
  end;
  SourceNotebook.ClearErrorLines;

  ToolStatus:=itBuilder;
  MessagesView.Clear;
  DoArrangeSourceEditorAndMessageView(false);
  ConnectOutputFilter;
end;

procedure TMainIDE.OnExtToolFreeOutputFilter(OutputFilter: TOutputFilter;
  ErrorOccurred: boolean);
begin
  if ToolStatus=itBuilder then
    ToolStatus:=itNone;
  if ErrorOccurred then
    DoJumpToCompilerMessage(-1,true);
end;

procedure TMainIDE.RenameInheritedMethods(AnUnitInfo: TUnitInfo; List: TStrings
  );
var
  UsedByDependency: TUnitComponentDependency;
  DependingUnit: TUnitInfo;
  OldName: string;
  NewName: string;
  i: Integer;
begin
  if List=nil then exit;
  UsedByDependency:=AnUnitInfo.FirstUsedByComponent;
  while UsedByDependency<>nil do begin
    DependingUnit:=UsedByDependency.UsedByUnit;
    if (DependingUnit.Component<>nil)
    and (DependingUnit.Component.ClassParent=AnUnitInfo.Component.ClassType)
    then begin
      // the root component inherits from the DependingUnit root component
      i:=0;
      while i<List.Count-1 do begin
        OldName:=List[i];
        NewName:=List[i+1];
        // replace references, ignoring errors
        if CodeToolBoss.ReplaceWord(DependingUnit.Source,OldName,NewName,false)
        then begin
          // renamed in source, now rename in JIT class
          FormEditor1.RenameJITMethod(DependingUnit.Component,
                                      OldName,NewName);
        end;
        inc(i,2);
      end;
      ApplyCodeToolChanges;
      // rename recursively
      RenameInheritedMethods(DependingUnit,List);
    end;
    UsedByDependency:=UsedByDependency.NextUsedByDependency;
  end;
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
  ActiveSourceEditor:=nil;
  ActiveUnitInfo:=nil;
  if (ADesigner<>nil) then
    ActiveUnitInfo:=Project1.UnitWithComponent(ADesigner.LookupRoot)
  else if (GlobalDesignHook.LookupRoot<>nil)
  and (GlobalDesignHook.LookupRoot is TComponent) then
    ActiveUnitInfo:=
      Project1.UnitWithComponent(TComponent(GlobalDesignHook.LookupRoot))
  else
    ActiveUnitInfo:=nil;
  if (ActiveUnitInfo<>nil) then begin
    i:=ActiveUnitInfo.EditorIndex;
    if (i>=0) then begin
      SourceNoteBook.NoteBook.PageIndex:=i;
      GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
      exit;
    end;
  end;
end;

function TMainIDE.GetDesignerFormOfSource(AnUnitInfo: TUnitInfo; LoadForm: boolean
  ): TCustomForm;
begin
  Result:=nil;
  if AnUnitInfo.Component<>nil then
    Result:=FormEditor1.GetDesignerForm(AnUnitInfo.Component);
  if ((Result=nil) or (Result.Designer=nil)) and LoadForm
  and FilenameIsPascalSource(AnUnitInfo.Filename) then begin
    //DebugLn(['TMainIDE.GetFormOfSource ',AnUnitInfo.Filename,' ',dbgsName(AnUnitInfo.Component)]);
    DoLoadLFM(AnUnitInfo,[],[]);
  end;
  if (Result=nil) and (AnUnitInfo.Component<>nil) then
    Result:=FormEditor1.GetDesignerForm(AnUnitInfo.Component);
  if (Result<>nil) and (Result.Designer=nil) then
    Result:=nil;
end;

function TMainIDE.GetProjectFileWithRootComponent(AComponent: TComponent
  ): TLazProjectFile;
var
  AnUnitInfo: TUnitInfo;
begin
  if AComponent=nil then exit(nil);
  AnUnitInfo:=Project1.FirstUnitWithComponent;
  while AnUnitInfo<>nil do begin
    if AnUnitInfo.Component=AComponent then begin
      Result:=AnUnitInfo;
      exit;
    end;
    AnUnitInfo:=AnUnitInfo.NextUnitWithComponent;
  end;
  Result:=nil;
end;

function TMainIDE.GetProjectFileWithDesigner(ADesigner: TIDesigner
  ): TLazProjectFile;
var
  TheDesigner: TDesigner;
  AComponent: TComponent;
begin
  TheDesigner:=ADesigner as TDesigner;
  AComponent:=TheDesigner.LookupRoot;
  if AComponent=nil then
    RaiseException('TMainIDE.GetProjectFileWithDesigner Designer.LookupRoot=nil');
  Result:=GetProjectFileWithRootComponent(AComponent);
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
                        ActiveUnitInfo.Component.ClassName,AMethodName,TypeData,
                        MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  if CodeToolBoss.ErrorMessage<>'' then begin
    DoJumpToCodeToolBossError;
    raise Exception.Create(lisUnableToFindMethodPleaseFixTheErrorShownInTheMessage
      );
  end;
end;

function TMainIDE.OnPropHookCreateMethod(const AMethodName: ShortString;
  ATypeInfo: PTypeInfo;
  APersistent: TPersistent; const APropertyPath: string): TMethod;
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  r: boolean;
  OldChange: Boolean;
begin
  Result.Code:=nil;
  Result.Data:=nil;
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookCreateMethod] ************ ',AMethodName);
  DebugLn(['[TMainIDE.OnPropHookCreateMethod] Persistent=',dbgsName(APersistent),' Unit=',GetClassUnitName(APersistent.ClassType),' Path=',APropertyPath]);
  {$ENDIF}
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    // create published method
    r:=CodeToolBoss.CreatePublishedMethod(ActiveUnitInfo.Source,
        ActiveUnitInfo.Component.ClassName,AMethodName,
        ATypeInfo,false,GetClassUnitName(APersistent.ClassType),APropertyPath);
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.OnPropHookCreateMethod] ************2 ',r,' ',AMethodName);
    {$ENDIF}
    ApplyCodeToolChanges;
    if r then begin
      Result:=FormEditor1.CreateNewJITMethod(ActiveUnitInfo.Component,
                                             AMethodName);
    end else begin
      DebugLn(['TMainIDE.OnPropHookCreateMethod failed adding method to source']);
      DoJumpToCodeToolBossError;
      raise Exception.Create(lisUnableToCreateNewMethodPleaseFixTheErrorShownIn);
    end;
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

procedure TMainIDE.OnPropHookShowMethod(const AMethodName: ShortString);
var
  ActiveSrcEdit: TSourceEditor;
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
    ActiveUnitInfo.Component.ClassName,AMethodName,
    NewSource,NewX,NewY,NewTopLine) then
  begin
    DoJumpToCodePos(ActiveSrcEdit, ActiveUnitInfo,
      NewSource, NewX, NewY, NewTopLine, true);
  end else begin
    DebugLn(['TMainIDE.OnPropHookShowMethod failed finding the method in code']);
    DoJumpToCodeToolBossError;
    raise Exception.Create(lisUnableToShowMethodPleaseFixTheErrorShownInTheMessage
      );
  end;
end;

procedure TMainIDE.OnPropHookRenameMethod(const CurName, NewName: ShortString);
var ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  BossResult: boolean;
  ErrorMsg: String;
  OldChange: Boolean;
  RenamedMethods: TStringList;
begin
  if not BeginCodeTool(ActiveSrcEdit,ActiveUnitInfo,[ctfSwitchToFormSource])
  then exit;
  {$IFDEF IDE_DEBUG}
  writeln('');
  writeln('[TMainIDE.OnPropHookRenameMethod] ************');
  {$ENDIF}
  OldChange:=OpenEditorsOnCodeToolChange;
  OpenEditorsOnCodeToolChange:=true;
  try
    // rename/create published method
    BossResult:=CodeToolBoss.RenamePublishedMethod(ActiveUnitInfo.Source,
                            ActiveUnitInfo.Component.ClassName,CurName,NewName);
    {$IFDEF IDE_DEBUG}
    writeln('');
    writeln('[TMainIDE.OnPropHookRenameMethod] ************2 ');
    {$ENDIF}
    ApplyCodeToolChanges;
    if BossResult then begin
      FormEditor1.RenameJITMethod(ActiveUnitInfo.Component,CurName,NewName);
      RenamedMethods:=TStringList.Create;
      try
        RenamedMethods.Add(CurName);
        RenamedMethods.Add(NewName);
        RenameInheritedMethods(ActiveUnitInfo,RenamedMethods);
      finally
        RenamedMethods.Free;
      end;
    end else begin
      ErrorMsg:=CodeToolBoss.ErrorMessage;
      DoJumpToCodeToolBossError;
      raise Exception.Create(
        lisUnableToRenameMethodPleaseFixTheErrorShownInTheMessag
        +#13#13+lisError+ErrorMsg);
    end;
  finally
    OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

function TMainIDE.OnPropHookBeforeAddPersistent(Sender: TObject;
  APersistentClass: TPersistentClass; AParent: TPersistent): boolean;
begin
  Result:=false;
  if (not (AParent is TControl))
  and (APersistentClass.InheritsFrom(TControl)) then begin
    MessageDlg(lisCodeToolsDefsInvalidParent,
      Format(lisACanNotHoldTControlsYouCanOnlyPutNonVisualComponen, [
        AParent.ClassName, #13]),
      mtError,[mbCancel],0);
    UpdateIDEComponentPalette;
    exit;
  end;
  Result:=true;
end;

procedure TMainIDE.OnPropHookComponentRenamed(AComponent: TComponent);
begin
  if (AComponent.Owner=nil) then
    FormEditor1.UpdateDesignerFormName(AComponent);
  ObjectInspector1.FillPersistentComboBox;
end;

{-------------------------------------------------------------------------------
  procedure TMainIDE.OnPropHookPersistentAdded(APersistent: TPersistent;
    Select: boolean);

  This handler is called whenever a new component was added to a designed form
  and should be added to form source
-------------------------------------------------------------------------------}
procedure TMainIDE.OnPropHookPersistentAdded(APersistent: TPersistent;
  Select: boolean);
var
  RegComp: TRegisteredComponent;
  ADesigner: TDesigner;
  AComponent: TComponent;
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  Ancestor: TComponent;
  ComponentClassNames: TStringList;
begin
  DebugLn('TMainIDE.OnPropHookPersistentAdded A ',dbgsName(APersistent));
  ADesigner:=nil;
  if APersistent is TComponent then
    AComponent:=TComponent(APersistent)
  else
    AComponent:=nil;
  RegComp:=IDEComponentPalette.FindComponent(APersistent.ClassName);
  if (RegComp=nil) and (AComponent<>nil) then begin
    DebugLn('TMainIDE.OnPropHookPersistentAdded ',APersistent.ClassName,
            ' not registered');
    exit;
  end;
  if AComponent<>nil then begin
    // create unique name
    if AComponent.Name='' then
      AComponent.Name:=FormEditor1.CreateUniqueComponentName(AComponent);
    //writeln('TMainIDE.OnPropHookPersistentAdded B ',AComponent.Name,':',AComponent.ClassName);
    // create component interface
    if FormEditor1.FindComponent(AComponent)=nil then
      FormEditor1.CreateComponentInterface(AComponent,false);
    // set component into design mode
    SetDesigning(AComponent,true);
    //writeln('TMainIDE.OnPropHookPersistentAdded C ',AComponent.Name,':',AComponent.ClassName);
    // add to source
    ADesigner:=FindRootDesigner(AComponent) as TDesigner;
  end;

  if RegComp<>nil then begin
    if not BeginCodeTool(ADesigner,ActiveSrcEdit,ActiveUnitInfo,
      [ctfSwitchToFormSource])
    then exit;

    // remember cursor position
    SourceNotebook.AddJumpPointClicked(Self);

    // add needed package to required packages
    ComponentClassNames:=TStringList.Create;
    try
      ComponentClassNames.Add(APersistent.ClassName);
      //DebugLn(['TMainIDE.OnPropHookPersistentAdded ComponentClassNames=',ComponentClassNames.Text]);
      PkgBoss.AddUnitDependenciesForComponentClasses(ActiveUnitInfo.Filename,
        ComponentClassNames,true);
    finally
      ComponentClassNames.Free;
    end;

    // add component definitions to form source
    Ancestor:=GetAncestorLookupRoot(ActiveUnitInfo);
    CodeToolBoss.CompleteComponent(ActiveUnitInfo.Source,ADesigner.LookupRoot,
                                   Ancestor);
  end;

  ObjectInspector1.FillPersistentComboBox;

  //writeln('TMainIDE.OnPropHookPersistentAdded D ',AComponent.Name,':',AComponent.ClassName,' ',Select);
  // select component
  if Select then begin
    TheControlSelection.AssignPersistent(APersistent);
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TMainIDE.OnPropHookPersistentAdded END ',dbgsName(APersistent),' Select=',Select);
  {$ENDIF}
end;

procedure TMainIDE.OnPropHookDeletePersistent(var APersistent: TPersistent);
var
  ADesigner: TDesigner;
  AComponent: TComponent;
begin
  if APersistent=nil then exit;
  DebugLn('TMainIDE.OnPropHookDeletePersistent A ',dbgsName(APersistent));
  if APersistent is TComponent then begin
    AComponent:=TComponent(APersistent);
    ADesigner:=TDesigner(FindRootDesigner(AComponent));
    if ADesigner=nil then exit;
    ADesigner.RemovePersistentAndChilds(AComponent);
  end else begin
    APersistent.Free;
  end;
  APersistent:=nil;
end;

procedure TMainIDE.OnPropHookAddDependency(const AClass: TClass;
  const AnUnitName: shortstring);
// add a package dependency to the package/project of the currently active
// designed component.
var
  RequiredUnitName: String;
  AnUnitInfo: TUnitInfo;
begin
  // check input
  if AClass<>nil then begin
    RequiredUnitName:=GetClassUnitName(AClass);
    if (AnUnitName<>'')
    and (SysUtils.CompareText(AnUnitName,RequiredUnitName)<>0) then
      raise Exception.Create(
        'TMainIDE.OnPropHookAddDependency unitname and class do not fit:'
        +'unitname='+AnUnitName
        +' class='+dbgs(AClass)+' class.unitname='+RequiredUnitName);
  end else begin
    RequiredUnitName:=AnUnitName;
  end;
  if RequiredUnitName='' then
    raise Exception.Create('TMainIDE.OnPropHookAddDependency no unitname');

  // find current designer and unit
  if not (GlobalDesignHook.LookupRoot is TComponent) then exit;
  AnUnitInfo:=Project1.UnitWithComponent(TComponent(GlobalDesignHook.LookupRoot));
  if AnUnitInfo=nil then begin
    DebugLn(['TMainIDE.OnPropHookAddDependency LookupRoot not found']);
    exit;
  end;

  PkgBoss.AddDependencyToUnitOwners(AnUnitInfo.Filename,RequiredUnitName);
end;

procedure TMainIDE.mnuEditCopyClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecCopy);
end;

procedure TMainIDE.mnuEditCutClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecCut);
end;

procedure TMainIDE.mnuEditPasteClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecPaste);
end;

procedure TMainIDE.mnuEditRedoClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecRedo);
end;

procedure TMainIDE.mnuEditUndoClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecUndo);
end;

procedure TMainIDE.mnuEditIndentBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecBlockIndent);
end;

procedure TMainIDE.mnuEditUnindentBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecBlockUnindent);
end;

procedure TMainIDE.mnuEditEncloseBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionEnclose);
end;

procedure TMainIDE.mnuEditUpperCaseBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionUpperCase);
end;

procedure TMainIDE.mnuEditLowerCaseBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionLowerCase);
end;

procedure TMainIDE.mnuEditTabsToSpacesBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionTabs2Spaces);
end;

procedure TMainIDE.mnuEditCommentBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionComment);
end;

procedure TMainIDE.mnuEditUncommentBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionUncomment);
end;

procedure TMainIDE.mnuEditConditionalBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionConditional);
end;

procedure TMainIDE.mnuEditSortBlockClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionSort);
end;

procedure TMainIDE.mnuEditSelectionBreakLinesClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectionBreakLines);
end;

procedure TMainIDE.mnuEditSelectAllClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectAll);
end;

procedure TMainIDE.mnuEditSelectCodeBlockClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectCodeBlock);
end;

procedure TMainIDE.mnuEditSelectToBraceClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectToBrace);
end;

procedure TMainIDE.mnuEditSelectLineClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectLine);
end;

procedure TMainIDE.mnuEditSelectParagraphClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecSelectParagraph);
end;

procedure TMainIDE.mnuEditInsertGPLNoticeClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertGPLNotice);
end;

procedure TMainIDE.mnuEditInsertLGPLNoticeClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertLGPLNotice);
end;

procedure TMainIDE.mnuEditInsertModifiedLGPLNoticeClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertModifiedLGPLNotice);
end;

procedure TMainIDE.mnuEditInsertUsernameClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertUserName);
end;

procedure TMainIDE.mnuEditInsertDateTimeClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertDateTime);
end;

procedure TMainIDE.mnuEditInsertChangeLogEntryClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertChangeLogEntry);
end;

procedure TMainIDE.mnuInsertTodo(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertTodo);
end;

procedure TMainIDE.mnuSearchFindInFiles(Sender: TObject);
begin
  DoFindInFiles;
end;

procedure TMainIDE.mnuSearchFindIdentifierRefsClicked(Sender: TObject);
begin
  DoFindRenameIdentifier(false);
end;

procedure TMainIDE.mnuSearchRenameIdentifierClicked(Sender: TObject);
begin
  DoFindRenameIdentifier(true);
end;

procedure TMainIDE.mnuEditCompleteCodeClicked(Sender: TObject);
begin
  DoCompleteCodeAtCursor;
end;

procedure TMainIDE.mnuEditExtractProcClicked(Sender: TObject);
begin
  DoExtractProcFromSelection;
end;

procedure TMainIDE.mnuEditInsertCharacterClicked(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCharacter);
end;

procedure TMainIDE.mnuEditInsertCVSAuthorClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSAuthor);
end;

procedure TMainIDE.mnuEditInsertCVSDateClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSDate);
end;

procedure TMainIDE.mnuEditInsertCVSHeaderClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSHeader);
end;

procedure TMainIDE.mnuEditInsertCVSIDClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSID);
end;

procedure TMainIDE.mnuEditInsertCVSLogClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSLog);
end;

procedure TMainIDE.mnuEditInsertCVSNameClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSName);
end;

procedure TMainIDE.mnuEditInsertCVSRevisionClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSRevision);
end;

procedure TMainIDE.mnuEditInsertCVSSourceClick(Sender: TObject);
begin
  DoSourceEditorCommand(ecInsertCVSSource);
end;

procedure TMainIDE.DoCommand(EditorCommand: integer);
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  if FDisplayState = dsSource then begin
    // send command to source editor
    if (ActiveSourceEditor=nil) then exit;
    ActiveSourceEditor.DoEditorExecuteCommand(EditorCommand);
  end else begin
    // send command to form editor
    if ActiveUnitInfo=nil then exit;

    // ToDo: send command to form editor/designer

  end;
end;

procedure TMainIDE.DoSourceEditorCommand(EditorCommand: integer);
var
  CurFocusControl: TWinControl;
begin
  // check that the currently focus is on the MainIDEBar or on the SourceEditor
  CurFocusControl:=FindOwnerControl(GetFocus);
  if (CurFocusControl<>nil) then begin
    CurFocusControl:=GetParentForm(CurFocusControl);
    if (CurFocusControl<>MainIDEBar) and (CurFocusControl<>SourceNotebook) then
    begin
      // continue processing shortcut, not handled yet
      MainIDEBar.mnuMainMenu.ShortcutHandled := false;
      exit;
    end;
  end;
  DoCommand(EditorCommand);
end;

procedure TMainIDE.OnApplyWindowLayout(ALayout: TIDEWindowLayout);
var
  l: TNonModalIDEWindow;
  BarBottom: Integer;
  DockingAllowed: Boolean;
  NewHeight: Integer;
begin
  if (ALayout=nil) or (ALayout.Form=nil) then exit;
  // debugln('TMainIDE.OnApplyWindowLayout ',ALayout.Form.Name,' ',ALayout.Form.Classname,' ',IDEWindowPlacementNames[ALayout.WindowPlacement],' ',ALayout.CustomCoordinatesAreValid,' ',ALayout.Left,' ',ALayout.Top,' ',ALayout.Width,' ',ALayout.Height);
  DockingAllowed:={$IFDEF IDEDocking}true{$ELSE}false{$ENDIF};
  if DockingAllowed then begin
    ALayout.Form.Constraints.MaxHeight:=0;
  end;

  l:=NonModalIDEFormIDToEnum(ALayout.FormID);
  if DockingAllowed then begin
    if l in [nmiwSourceNoteBookName] then
      ALayout.WindowPlacement:=iwpDocked;
  end;

  case ALayout.WindowPlacement of
  iwpCustomPosition,iwpRestoreWindowGeometry:
    begin
      case ALayout.WindowState of
      iwsMinimized: ALayout.Form.WindowState:=wsMinimized;
      iwsMaximized: ALayout.Form.WindowState:=wsMaximized;
      end;

      if (ALayout.CustomCoordinatesAreValid) then begin
        // explicit position
        ALayout.Form.SetRestoredBounds(
          ALayout.Left,ALayout.Top,ALayout.Width,ALayout.Height);
        exit;
      end;

      if ALayout.WindowState in [iwsMinimized, iwsMaximized] then
        exit;
    end;

  iwpUseWindowManagerSetting:
    begin
      exit;
    end;
  end;
  // no layout found => use default
  BarBottom:=MainIDEBar.Top+MainIDEBar.Height;
  // default window positions
  case l of
  nmiwMainIDEName:
    begin
      NewHeight:=95;
      if (MainIDEBar.ComponentNotebook<>nil)
      and (MainIDEBar.ComponentNotebook.ActivePageComponent<>nil) then begin
        dec(NewHeight,MainIDEBar.ComponentNotebook.ActivePageComponent.ClientHeight-25);
      end;
      ALayout.Form.SetBounds(0,0,Screen.Width-10,NewHeight);
      if DockingAllowed then begin
        ALayout.Form.Align:=alTop;
      end;
    end;
  nmiwSourceNoteBookName:
    begin
      ALayout.Form.SetBounds(250,BarBottom+30,Max(50,Screen.Width-300),
        Max(50,Screen.Height-200-BarBottom));
      if DockingAllowed then begin
        debugln('TMainIDE.OnApplyWindowLayout ',dbgsName(ALayout.Form));
        ALayout.Form.ManualDock(MainIDEBar,nil,alBottom,false);
      end;
    end;
  nmiwUnitDependenciesName:
    ALayout.Form.SetBounds(200,200,400,300);
  nmiwCodeExplorerName:
    begin
      ALayout.Form.SetBounds(Screen.Width-200,130,170,Max(50,Screen.Height-230));
    end;
  nmiwCodeBrowser:
    begin
      ALayout.Form.SetBounds(200,100,650,500);
    end;
  nmiwClipbrdHistoryName:
    ALayout.Form.SetBounds(250,Screen.Height-400,400,300);
  nmiwPkgGraphExplorer:
    ALayout.Form.SetBounds(250,150,500,350);
  nmiwProjectInspector:
    ALayout.Form.SetBounds(200,150,400,300);
  nmiwMessagesViewName:
    begin
      ALayout.Form.SetBounds(250,SourceNotebook.Top+SourceNotebook.Height+30,
        Max(50,Screen.Width-300),80);
    end;
  else
    if ALayout.FormID=DefaultObjectInspectorName then begin
      ALayout.Form.SetBounds(
        MainIDEBar.Left,BarBottom+30,230,Max(Screen.Height-BarBottom-120,50));
    end;
  end;
end;

procedure TMainIDE.AddRecentProjectFileToEnvironment(const AFilename: string);
begin
  EnvironmentOptions.AddToRecentProjectFiles(AFilename);
  SetRecentProjectFilesMenu;
  SaveEnvironment;
end;

procedure TMainIDE.StartProtocol;
begin
  IDEProtocolOpts:=TIDEProtocol.Create;
  IDEProtocolOpts.Load;
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
  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('main.pp: initialization');{$ENDIF}
  {$I ../images/laz_images.lrs}
  {$I ../images/mainicon.lrs}
  ShowSplashScreen:=true;

end.

