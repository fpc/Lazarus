{
 /***************************************************************************
                    mainbase.pas  -  the "integrated" in IDE
                    ----------------------------------------
  TMainIDEBase is the ancestor of TMainIDE. The various top level parts of the
  IDE (called bosses/managers) access the TMainIDE via TMainIDEBase.


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
unit MainBase;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, LCLType, LCLProc, LCLIntf, StdCtrls, Buttons, Menus, ComCtrls,
  SysUtils, Controls, Graphics, ExtCtrls, Dialogs, FileUtil, Forms,
  CodeToolManager, CodeCache, AVL_Tree, SynEditKeyCmds,
  // IDEIntf
  LazConf, LazarusIDEStrConsts, SrcEditorIntf, LazIDEIntf, MenuIntf,
  IDECommands, IDEMsgIntf, IDEWindowIntf,
  // IDE
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
  ComponentReg, OutputFilter,
  TransferMacros, ObjectInspector, PropEdits, IDEDefs, MsgView,
  EnvironmentOpts, EditorOptions, CompilerOptions, KeyMapping, IDEProcs,
  Debugger, IDEOptionDefs, CodeToolsDefines, Splash, Designer,
  SourceEditor, BuildManager, FindInFilesDlg,
  MainBar, MainIntf, PseudoTerminalDlg;

type
  TResetToolFlag = (
    rfInteractive,
    rfCloseOnDone
  );
  TResetToolFlags = set of TResetToolFlag;

  { TMainIDEBase }

  TMainIDEBase = class(TMainIDEInterface)
  private
    FToolStatus: TIDEToolStatus;
  protected
    FNeedUpdateHighlighters: boolean;
    FLastWindowMenuUpdate: TDateTime;

    function CreateMenuSeparator : TMenuItem;
    procedure CreateMenuItem(Section: TIDEMenuSection;
                             var MenuCommand: TIDEMenuCommand;
                             const MenuItemName, MenuItemCaption: String;
                             const bmpName: String = '';
                             mnuEnabled: Boolean = true;
                             mnuChecked: Boolean = false;
                             mnuVisible: Boolean = true);
    procedure CreateMenuSeparatorSection(ParentSection: TIDEMenuSection;
                             var Section: TIDEMenuSection; const AName: String);
    procedure CreateMenuSubSection(ParentSection: TIDEMenuSection;
                             var Section: TIDEMenuSection;
                             const AName, ACaption: String;
                             const bmpName: String = '');
    procedure CreateMainMenuItem(var Section: TIDEMenuSection;
                                 const MenuItemName, MenuItemCaption: String);
    procedure SetupMainMenu; virtual;
    procedure SetupFileMenu; virtual;
    procedure SetupEditMenu; virtual;
    procedure SetupSearchMenu; virtual;
    procedure SetupViewMenu; virtual;
    procedure SetupSourceMenu; virtual;
    procedure SetupProjectMenu; virtual;
    procedure SetupRunMenu; virtual;
    procedure SetupPackageMenu; virtual;
    procedure SetupToolsMenu; virtual;
    procedure SetupWindowsMenu; virtual;
    procedure SetupHelpMenu; virtual;

    procedure LoadMenuShortCuts; virtual;
    function GetToolStatus: TIDEToolStatus; override;
    procedure SetToolStatus(const AValue: TIDEToolStatus); virtual;

    procedure mnuWindowItemClick(Sender: TObject); virtual;
    procedure mnuWindowSourceItemClick(Sender: TObject); virtual;
    procedure OnMainBarDestroy(Sender: TObject); virtual;

    procedure ConnectOutputFilter;

  public
    property ToolStatus: TIDEToolStatus read FToolStatus write SetToolStatus;
    function DoResetToolStatus(AFlags: TResetToolFlags): boolean; virtual; abstract;

    constructor Create(TheOwner: TComponent); override;
    procedure StartIDE; virtual; abstract;
    destructor Destroy; override;
    procedure CreateOftenUsedForms; virtual; abstract;
    function GetMainBar: TComponent; override;

    procedure GetUnitInfoForDesigner(ADesigner: TIDesigner;
                              out ActiveSourceEditor: TSourceEditorInterface;
                              out ActiveUnitInfo: TUnitInfo); override;

    procedure GetCurrentUnitInfo(out ActiveSourceEditor: TSourceEditorInterface;
                              out ActiveUnitInfo: TUnitInfo); override;
    procedure GetCurrentUnit(out ActiveSourceEditor: TSourceEditor;
                             out ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithPageIndex(PageIndex, WindowIndex: integer;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
          deprecated; // deprecated in 0.9.29 March 2010
    procedure GetDesignerUnit(ADesigner: TDesigner;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetObjectInspectorUnit(
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithForm(AForm: TCustomForm;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithPersistent(APersistent: TPersistent;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;

    function DoOpenMacroFile(Sender: TObject; const AFilename: string
                             ): TModalResult; override;

    procedure UpdateWindowMenu(Immediately: boolean = false); override;
    procedure SetRecentSubMenu(Section: TIDEMenuSection; FileList: TStringList;
                               OnClickEvent: TNotifyEvent); override;
    procedure UpdateHighlighters(Immediately: boolean = false); override;

    //function DoJumpToCodePos(
    //                    ActiveSrcEdit: TSourceEditor;
    //                    ActiveUnitInfo: TUnitInfo;
    //                    NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
    //                    AddJumpPoint: boolean;
    //                    FocusEditor: Boolean = True;
    //                    MarkLine: Boolean = False): TModalResult; virtual;

    procedure FindInFilesPerDialog(AProject: TProject); override;
    procedure FindInFiles(AProject: TProject; const FindText: string); override;
  end;

function  GetMainIde: TMainIDEBase;
procedure SetMainIde(AValue: TMainIDEBase);

property MainIDE: TMainIDEBase read GetMainIde write SetMainIde;

  { Normally the IDE builds itself with packages named in config files.
    When the IDE should keep the packages installed in the current executable
    set KeepInstalledPackages to true. }
var KeepInstalledPackages: boolean = false;

implementation

uses
  IDEImagesIntf;

function GetMainIde: TMainIDEBase;
begin
  Result := TMainIDEBase(MainIDEIntf)
end;

procedure SetMainIde(AValue: TMainIDEBase);
begin
  MainIDEIntf := AValue;
end;

//{$IFDEF LCLCarbon}
//var
//  mnuApple: TIDEMenuSection = nil;
//{$ENDIF}

{ TMainIDEBase }

procedure TMainIDEBase.mnuWindowItemClick(Sender: TObject);
var
  i: Integer;
begin
  i:=Screen.CustomFormCount-1;
  while (i>=0) do begin
    if Screen.CustomForms[i].Caption=(Sender as TIDEMenuCommand).Caption then
    begin
      IDEWindowCreators.ShowForm(Screen.CustomForms[i],true);
      break;
    end;
    dec(i);
  end;
end;

procedure TMainIDEBase.mnuWindowSourceItemClick(Sender: TObject);
var
  i: LongInt;
begin
  if SourceEditorManager = nil then exit;
  i:=(sender as TIDEMenuCommand).tag;
  if (i<0) or (i>=SourceEditorManager.SourceEditorCount) then exit;
  SourceEditorManager.ActiveEditor := SourceEditorManager.SourceEditors[i];
  SourceEditorManager.ShowActiveWindowOnTop(True);
end;

procedure TMainIDEBase.OnMainBarDestroy(Sender: TObject);
begin
  //writeln('TMainIDEBase.OnMainBarDestroy');
end;

procedure TMainIDEBase.ConnectOutputFilter;
begin
  TheOutputFilter.OnAddFilteredLine:=@MessagesView.AddMsg;
  TheOutputFilter.OnReadLine:=@MessagesView.AddProgress;
  TheOutputFilter.OnEndReading:=@MessagesView.CollectLineParts;
  TheOutputFilter.OnBeginUpdate:=@MessagesView.BeginUpdateNotification;
  TheOutputFilter.OnEndUpdate:=@MessagesView.EndUpdateNotification;
end;

procedure TMainIDEBase.SetToolStatus(const AValue: TIDEToolStatus);
begin
  if FToolStatus=AValue then exit;
  FToolStatus:=AValue;
  UpdateCaption;
end;

constructor TMainIDEBase.Create(TheOwner: TComponent);
begin
  MainIDE:=Self;
  // Do not own everything in one big component hierachy. Otherwise the
  // notifications slow down everything
  fOwningComponent:=TComponent.Create(nil);
  inherited Create(TheOwner);
end;

destructor TMainIDEBase.Destroy;
begin
  FreeThenNil(fOwningComponent);
  inherited Destroy;
  MainIDE:=nil;
end;

procedure TMainIDEBase.GetUnitInfoForDesigner(ADesigner: TIDesigner;
  out ActiveSourceEditor: TSourceEditorInterface; out ActiveUnitInfo: TUnitInfo
  );
var
  SrcEdit: TSourceEditor;
begin
  ActiveSourceEditor:=nil;
  ActiveUnitInfo:=nil;
  if ADesigner is TDesigner then begin
    GetDesignerUnit(TDesigner(ADesigner),SrcEdit,ActiveUnitInfo);
    ActiveSourceEditor:=SrcEdit;
  end;
end;

procedure TMainIDEBase.GetCurrentUnitInfo(
  out ActiveSourceEditor: TSourceEditorInterface; out ActiveUnitInfo: TUnitInfo
  );
var
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  GetCurrentUnit(ASrcEdit, AnUnitInfo);
  ActiveSourceEditor:=ASrcEdit;
  ActiveUnitInfo:=AnUnitInfo;
end;

function TMainIDEBase.GetMainBar: TComponent;
begin
  Result:=MainIDEBar;
end;

function TMainIDEBase.CreateMenuSeparator : TMenuItem;
begin
  Result := TMenuItem.Create(MainIDEBar);
  Result.Caption := '-';
end;

procedure TMainIDEBase.CreateMenuItem(Section: TIDEMenuSection;
  var MenuCommand: TIDEMenuCommand; const MenuItemName, MenuItemCaption: String;
  const bmpName: String; mnuEnabled: Boolean; mnuChecked: Boolean;
  mnuVisible: Boolean);
begin
  MenuCommand:=RegisterIDEMenuCommand(Section,MenuItemName,MenuItemCaption);
  MenuCommand.Enabled:=mnuEnabled;
  MenuCommand.Checked:=mnuChecked;
  MenuCommand.Visible:=mnuVisible;
  if bmpName<>'' then
    MenuCommand.ImageIndex := IDEImages.LoadImage(16, bmpName);
end;

procedure TMainIDEBase.CreateMenuSeparatorSection(
  ParentSection: TIDEMenuSection; var Section: TIDEMenuSection;
  const AName: String);
begin
  Section:=RegisterIDEMenuSection(ParentSection,AName);
  Section.ChildsAsSubMenu := false;
end;

procedure TMainIDEBase.CreateMenuSubSection(ParentSection: TIDEMenuSection;
  var Section: TIDEMenuSection; const AName, ACaption: String;
  const bmpName: String = '');
begin
  Section:=RegisterIDESubMenu(ParentSection,AName,ACaption);
  if bmpName<>'' then
    Section.ImageIndex := IDEImages.LoadImage(16, bmpName);
end;

procedure TMainIDEBase.CreateMainMenuItem(var Section: TIDEMenuSection;
  const MenuItemName, MenuItemCaption: String);
begin
  Section:=RegisterIDESubMenu(mnuMain,MenuItemName,MenuItemCaption);
end;

procedure TMainIDEBase.SetupMainMenu;
begin
  MainIDEBar.mnuMainMenu := TMainMenu.Create(MainIDEBar);
  MainIDEBar.mnuMainMenu.Images := IDEImages.Images_16;
  with MainIDEBar do begin
    mnuMain:=RegisterIDEMenuRoot('IDEMainMenu',nil);
    {$ifdef LCLCarbon}
    // Under Apple there is a special policy: every application should create
    // a special Apple menu and put Quit, About there.
    // See issue: http://bugs.freepascal.org/view.php?id=12294
    // See http://lists.apple.com/archives/carbon-development/2002/Apr/msg01183.html, for details
    //CreateMainMenuItem(mnuApple,'AppleApplication','');
    {$endif}
    CreateMainMenuItem(mnuFile,'File',lisMenuFile);
    CreateMainMenuItem(mnuEdit,'Edit',lisMenuEdit);
    CreateMainMenuItem(mnuSearch,'Search',lisMenuSearch);
    CreateMainMenuItem(mnuView,'View',lisMenuView);
    CreateMainMenuItem(mnuSource,'Source',lisMenuSource);
    CreateMainMenuItem(mnuProject,'Project',lisMenuProject);
    CreateMainMenuItem(mnuRun,'Run',lisMenuRun);
    CreateMainMenuItem(mnuPackage,'Package',lisMenuPackage);
    mnuComponent:=mnuPackage;
    CreateMainMenuItem(mnuTools,'Tools',lisMenuTools);
    CreateMainMenuItem(mnuWindow,'Window',lisMenuWindow);
    CreateMainMenuItem(mnuHelp,'Help',lisMenuHelp);
  end;
end;

procedure TMainIDEBase.SetupFileMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuFile,itmFileNew,'itmFileNew');
    ParentMI:=itmFileNew;
    CreateMenuItem(ParentMI,itmFileNewUnit,'itmFileNewUnit',lisMenuNewUnit,'item_unit');
    CreateMenuItem(ParentMI,itmFileNewForm,'itmFileNewForm',lisMenuNewForm,'item_form');
    CreateMenuItem(ParentMI,itmFileNewOther,'itmFileNewOther',lisMenuNewOther,'menu_new');

    CreateMenuSeparatorSection(mnuFile,itmFileOpenSave,'itmFileOpenSave');
    ParentMI:=itmFileOpenSave;
    CreateMenuItem(ParentMI, itmFileOpen, 'itmFileOpen', lisMenuOpen, 'laz_open');
    CreateMenuItem(ParentMI,itmFileRevert,'itmFileRevert',lisMenuRevert, 'menu_file_revert');
    CreateMenuSubSection(ParentMI,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent);
    CreateMenuItem(ParentMI,itmFileSave,'itmFileSave',lisMenuSave,'laz_save');
    CreateMenuItem(ParentMI,itmFileSaveAs,'itmFileSaveAs',lisMenuSaveAs,'menu_saveas');
    CreateMenuItem(ParentMI,itmFileSaveAll,'itmFileSaveAll',lisMenuSaveAll,'menu_save_all');
    CreateMenuItem(ParentMI,itmFileClose,'itmFileClose',lisMenuClose,'menu_close',false);
    CreateMenuItem(ParentMI,itmFileCloseAll,'itmFileCloseAll',lisMenuCloseAll,'menu_close_all',false);

    CreateMenuSeparatorSection(mnuFile,itmFileDirectories,'itmFileDirectories');
    ParentMI:=itmFileDirectories;
    CreateMenuItem(ParentMI,itmFileCleanDirectory,'itmFileCleanDirectory',lisMenuCleanDirectory, 'menu_clean');

    CreateMenuSeparatorSection(mnuFile,itmFileIDEStart,'itmFileIDEStart');
    ParentMI:=itmFileIDEStart;
    CreateMenuItem(ParentMI,itmFileRestart,'itmFileRestart',lisMenuRestart, 'laz_refresh');
    CreateMenuItem(ParentMI,itmFileQuit,'itmFileQuit',lisMenuQuit, 'menu_exit');
  end;
end;

procedure TMainIDEBase.SetupEditMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuEdit,itmEditReUndo,'itmEditReUndo');
    ParentMI:=itmEditReUndo;
    CreateMenuItem(ParentMI,itmEditUndo,'itmEditUndo',lisMenuUndo,'menu_undo');
    CreateMenuItem(ParentMI,itmEditRedo,'itmEditRedo',lisMenuRedo,'menu_redo');

    CreateMenuSeparatorSection(mnuEdit,itmEditClipboard,'itmEditClipboard');
    ParentMI:=itmEditClipboard;
    CreateMenuItem(ParentMI,itmEditCut,'itmEditCut',lisMenuCut,'laz_cut');
    CreateMenuItem(ParentMI,itmEditCopy,'itmEditCopy',lisMenuCopy,'laz_copy');
    CreateMenuItem(ParentMI,itmEditPaste,'itmEditPaste',lisMenuPaste,'laz_paste');

    // "Select" menu items
    CreateMenuSeparatorSection(mnuEdit,itmEditSelect,'itmEditSelect');
    ParentMI:=itmEditSelect;
    CreateMenuItem(ParentMI,itmEditSelectAll,'itmEditSelectAll',lisMenuSelectAll, 'menu_select_all');
    CreateMenuItem(ParentMI,itmEditSelectToBrace,'itmEditSelectToBrace',lisMenuSelectToBrace);
    CreateMenuItem(ParentMI,itmEditSelectCodeBlock,'itmEditSelectCodeBlock',lisMenuSelectCodeBlock);
    CreateMenuItem(ParentMI,itmEditSelectWord,'itmEditSelectWord',lisMenuSelectWord);
    CreateMenuItem(ParentMI,itmEditSelectLine,'itmEditSelectLine',lisMenuSelectLine);
    CreateMenuItem(ParentMI,itmEditSelectParagraph,'itmEditSelectParagraph',lisMenuSelectParagraph);

    // "Char Conversion" menu items
    CreateMenuSeparatorSection(mnuEdit,itmEditBlockActions,'itmEditBlockActions');
    ParentMI:=itmEditBlockActions;
    CreateMenuItem(ParentMI,itmEditIndentBlock,'itmEditIndentBlock',lisMenuIndentSelection,'menu_indent');
    CreateMenuItem(ParentMI,itmEditUnindentBlock,'itmEditUnindentBlock',lisMenuUnindentSelection,'menu_unindent');
    CreateMenuItem(ParentMI,itmEditUpperCaseBlock,'itmEditUpperCaseBlock',lisMenuUpperCaseSelection, 'menu_edit_uppercase');
    CreateMenuItem(ParentMI,itmEditLowerCaseBlock,'itmEditLowerCaseBlock',lisMenuLowerCaseSelection, 'menu_edit_lowercase');
    CreateMenuItem(ParentMI,itmEditSwapCaseBlock,'itmEditSwapCaseBlock',lisMenuSwapCaseSelection, 'menu_edit_uppercase');
    CreateMenuItem(ParentMI,itmEditSortBlock,'itmEditSortBlock',lisMenuSortSelection, 'menu_edit_sort');
    CreateMenuItem(ParentMI,itmEditTabsToSpacesBlock,'itmEditTabsToSpacesBlock',lisMenuTabsToSpacesSelection);
    CreateMenuItem(ParentMI,itmEditSelectionBreakLines,'itmEditSelectionBreakLines',lisMenuBeakLinesInSelection);

    // *** insert text ***:
    CreateMenuSeparatorSection(mnuEdit,itmEditInsertions,'itmEditInsertions');
    ParentMI:=itmEditInsertions;
    CreateMenuItem(ParentMI,itmEditInsertCharacter,'itmEditInsertCharacter',lisMenuInsertCharacter);
  end;
end;

procedure TMainIDEBase.SetupSearchMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuSearch,itmSearchFindReplace,'itmSearchFindReplace');
    ParentMI:=itmSearchFindReplace;

    CreateMenuItem(ParentMI,itmSearchFind, 'itmSearchFind', lisMenuFind2, 'menu_search_find');
    CreateMenuItem(ParentMI,itmSearchFindNext,'itmSearchFindNext',lisMenuFindNext, 'menu_search_find_next');
    CreateMenuItem(ParentMI,itmSearchFindPrevious,'itmSearchFindPrevious',lisMenuFindPrevious, 'menu_search_find_previous');
    CreateMenuItem(ParentMI,itmSearchFindInFiles,'itmSearchFindInFiles',lisMenuFindInFiles, 'menu_search_files');
    CreateMenuItem(ParentMI, itmSearchReplace, 'itmSearchReplace',
      lisMenuReplace2, 'menu_search_replace');
    CreateMenuItem(ParentMI,itmIncrementalFind,'itmIncrementalFind',lisMenuIncrementalFind, 'menu_search_incremental');

    CreateMenuSeparatorSection(mnuSearch,itmJumpings,'itmJumpings');
    ParentMI:=itmJumpings;

    CreateMenuItem(ParentMI,itmGotoLine,'itmGotoLine',lisMenuGotoLine, 'menu_goto_line');
    CreateMenuItem(ParentMI,itmJumpBack,'itmJumpBack',lisMenuJumpBack, 'menu_search_jumpback');
    CreateMenuItem(ParentMI,itmJumpForward,'itmJumpForward',lisMenuJumpForward, 'menu_search_jumpforward');
    CreateMenuItem(ParentMI,itmAddJumpPoint,'itmAddJumpPoint',lisMenuAddJumpPointToHistory);
    CreateMenuItem(ParentMI,itmJumpToNextError,'itmJumpToNextError',lisMenuJumpToNextError);
    CreateMenuItem(ParentMI,itmJumpToPrevError,'itmJumpToPrevError',lisMenuJumpToPrevError);

    CreateMenuSeparatorSection(mnuSearch,itmBookmarks,'itmBookmarks');
    ParentMI:=itmBookmarks;

    CreateMenuItem(ParentMI,itmSetFreeBookmark,'itmSetFreeBookmark',lisMenuSetFreeBookmark);
    CreateMenuItem(ParentMI,itmJumpToNextBookmark,'itmJumpToNextBookmark',lisMenuJumpToNextBookmark, 'menu_search_next_bookmark');
    CreateMenuItem(ParentMI,itmJumpToPrevBookmark,'itmJumpToPrevBookmark',lisMenuJumpToPrevBookmark, 'menu_search_previous_bookmark');

    CreateMenuSeparatorSection(mnuSearch,itmCodeToolSearches,'itmCodeToolSearches');
    ParentMI:=itmCodeToolSearches;

    CreateMenuItem(ParentMI,itmFindBlockOtherEnd,'itmFindBlockOtherEnd',lisMenuFindBlockOtherEndOfCodeBlock);
    CreateMenuItem(ParentMI,itmFindBlockStart,'itmFindBlockStart',lisMenuFindCodeBlockStart);
    CreateMenuItem(ParentMI,itmFindDeclaration,'itmFindDeclaration',lisMenuFindDeclarationAtCursor);
    CreateMenuItem(ParentMI,itmOpenFileAtCursor,'itmOpenFileAtCursor',lisMenuOpenFilenameAtCursor,'menu_search_openfile_atcursor');
    CreateMenuItem(ParentMI,itmGotoIncludeDirective,'itmGotoIncludeDirective',lisMenuGotoIncludeDirective);
    CreateMenuItem(ParentMI,itmSearchFindIdentifierRefs,'itmSearchFindIdentifierRefs',lisMenuFindIdentifierRefs);
    CreateMenuItem(ParentMI,itmSearchProcedureList,'itmSearchProcedureList',lisMenuProcedureList);
  end;
end;

procedure TMainIDEBase.SetupViewMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuView,itmViewMainWindows,'itmViewMainWindows');
    ParentMI:=itmViewMainWindows;
    CreateMenuItem(ParentMI,itmViewInspector,'itmViewInspector',lisMenuViewObjectInspector, 'menu_view_inspector');
    CreateMenuItem(ParentMI,itmViewSourceEditor,'itmViewSourceEditor',lisMenuViewSourceEditor, 'menu_view_source_editor');
    CreateMenuItem(ParentMI,itmViewMessage,'itmViewMessage',lisMenuViewMessages);
    CreateMenuItem(ParentMI,itmViewCodeExplorer,'itmViewCodeExplorer',lisMenuViewCodeExplorer, 'menu_view_code_explorer');
    CreateMenuItem(ParentMI,itmViewFPDocEditor,'itmViewFPDocEditor',lisFPDocEditor);
    CreateMenuItem(ParentMI,itmViewCodeBrowser,'itmViewCodeBrowser',lisMenuViewCodeBrowser, 'menu_view_code_browser');
    CreateMenuItem(ParentMI,itmViewRestrictionBrowser,'itmViewRestrictionBrowser',lisMenuViewRestrictionBrowser, 'menu_view_rectriction_browser');
    CreateMenuItem(ParentMI,itmViewComponents,'itmViewComponents',lisMenuViewComponents);
    CreateMenuItem(ParentMI,itmJumpHistory,'itmJumpHistory',lisMenuViewJumpHistory);

    CreateMenuSeparatorSection(mnuView,itmViewUnitWindows,'itmViewUnitWindows');
    ParentMI:=itmViewUnitWindows;
    CreateMenuItem(ParentMI,itmViewUnitDependencies,'itmViewUnitDependencies',lisMenuViewUnitDependencies);
    CreateMenuItem(ParentMI,itmViewToggleFormUnit,'itmViewToggleFormUnit',lisMenuViewToggleFormUnit, 'menu_view_toggle_form_unit');

    CreateMenuSeparatorSection(mnuView,itmViewDesignerWindows,'itmViewDesignerWindows');
    ParentMI:=itmViewDesignerWindows;
    CreateMenuItem(ParentMI,itmViewAnchorEditor,'itmViewAnchorEditor',lisMenuViewAnchorEditor,'menu_view_anchor_editor');
    CreateMenuItem(ParentMI,itmViewTabOrder,'itmViewTabOrder',lisMenuViewTabOrder,'tab_order');

    CreateMenuSeparatorSection(mnuView,itmViewSecondaryWindows,'itmViewSecondaryWindows');
    ParentMI:=itmViewSecondaryWindows;
    CreateMenuItem(ParentMI,itmViewSearchResults,'itmViewSearchResults',lisMenuViewSearchResults);
    CreateMenuItem(ParentMI,itmViewComponentPalette,'itmViewComponentPalette',lisMenuViewComponentPalette, '', true, EnvironmentOptions.ComponentPaletteVisible);
    CreateMenuItem(ParentMI,itmViewIDESpeedButtons,'itmViewIDESpeedButtons',lisMenuViewIDESpeedButtons, '', true, EnvironmentOptions.IDESpeedButtonsVisible);
    CreateMenuSubSection(ParentMI,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'debugger');
    begin
      CreateMenuItem(itmViewDebugWindows,itmViewWatches,'itmViewWatches',lisMenuViewWatches,'debugger_watches');
      CreateMenuItem(itmViewDebugWindows,itmViewBreakPoints,'itmViewBreakPoints',lisMenuViewBreakPoints,'debugger_breakpoints');
      CreateMenuItem(itmViewDebugWindows,itmViewLocals,'itmViewLocals',lisMenuViewLocalVariables);
      if HasConsoleSupport then
        CreateMenuItem(itmViewDebugWindows,itmViewPseudoTerminal,'itmViewPseudoTerminal',lisMenuViewPseudoTerminal)
      else
        itmViewPseudoTerminal := nil;
      CreateMenuItem(itmViewDebugWindows,itmViewRegisters,'itmViewRegisters',lisMenuViewRegisters);
      CreateMenuItem(itmViewDebugWindows,itmViewCallStack,'itmViewCallStack',lisMenuViewCallStack,'debugger_call_stack');
      CreateMenuItem(itmViewDebugWindows,itmViewThreads,'itmViewThreads',lisMenuViewThreads);
      CreateMenuItem(itmViewDebugWindows,itmViewAssembler,'itmViewAssembler',lisMenuViewAssembler);
      CreateMenuItem(itmViewDebugWindows,itmViewDebugEvents,'itmViewDebugEvents',lisMenuViewDebugEvents,'debugger_event_log');
      CreateMenuItem(itmViewDebugWindows,itmViewDebugOutput,'itmViewDebugOutput',lisMenuViewDebugOutput,'debugger_output');
      CreateMenuItem(itmViewDebugWindows,itmViewDbgHistory,'itmViewDbgHistory',lisMenuViewHistory);
    end;
    CreateMenuSubSection(ParentMI, itmViewIDEInternalsWindows, 'itmViewIDEInternalsWindows', lisMenuIDEInternals);
    begin
      CreateMenuItem(itmViewIDEInternalsWindows, itmViewPackageLinks, 'itmViewPackageLinks', lisMenuPackageLinks);
      CreateMenuItem(itmViewIDEInternalsWindows, itmViewFPCInfo, 'itmViewFPCInfo', lisMenuAboutFPC);
      CreateMenuItem(itmViewIDEInternalsWindows, itmViewIDEInfo, 'itmViewIDEInfo', lisAboutIDE);
    end;
  end;
end;

procedure TMainIDEBase.SetupSourceMenu;
var
  ParentMI, SubParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuSource,itmSourceBlockActions,'itmSourceBlockActions');
    ParentMI:=itmSourceBlockActions;
    CreateMenuItem(ParentMI,itmSourceCommentBlock,'itmSourceCommentBlock',lisMenuCommentSelection, 'menu_comment');
    CreateMenuItem(ParentMI,itmSourceUncommentBlock,'itmSourceUncommentBlock',lisMenuUncommentSelection, 'menu_uncomment');
    CreateMenuItem(ParentMI,itmSourceToggleComment,'itmSourceToggleComment',lisMenuToggleComment, 'menu_comment');
    CreateMenuItem(ParentMI,itmSourceEncloseBlock,'itmSourceEncloseBlock',lisMenuEncloseSelection);
    CreateMenuItem(ParentMI,itmSourceEncloseInIFDEF,'itmSourceEncloseInIFDEF',lisMenuEncloseInIFDEF);
    CreateMenuItem(ParentMI,itmSourceCompleteCode,'itmSourceCompleteCode',lisMenuCompleteCode);
    CreateMenuItem(ParentMI,itmSourceUseUnit,'itmSourceUseUnit',lisMenuUseUnit);
    // Refactor
    CreateMenuSeparatorSection(mnuSource,itmSourceRefactor,'itmSourceRefactor');
    CreateMenuSubSection(ParentMI,itmSourceRefactor,'itmSourceRefactor',uemRefactor);
    SubParentMI:=itmSourceRefactor;
      CreateMenuSeparatorSection(SubParentMI,itmRefactorCodeTools,'itmRefactorCodeTools');
      ParentMI:=itmRefactorCodeTools;
      CreateMenuItem(ParentMI,itmRefactorRenameIdentifier,'itmRefactorRenameIdentifier',lisMenuRenameIdentifier);
      CreateMenuItem(ParentMI,itmRefactorExtractProc,'itmRefactorExtractProc',lisMenuExtractProc);
      CreateMenuItem(ParentMI,itmRefactorInvertAssignment,'itmInvertAssignment',uemInvertAssignment);

      CreateMenuSeparatorSection(SubParentMI,itmRefactorAdvanced,'itmRefactorAdvanced');
      ParentMI:=itmRefactorAdvanced;
      CreateMenuItem(ParentMI,itmRefactorShowAbstractMethods,'itmShowAbstractMethods',srkmecAbstractMethods);
      CreateMenuItem(ParentMI,itmRefactorShowEmptyMethods,'itmShowEmptyMethods',srkmecEmptyMethods);
      CreateMenuItem(ParentMI,itmRefactorShowUnusedUnits,'itmShowUnusedUnits',srkmecUnusedUnits);
      {$IFDEF EnableFindOverloads}
      CreateMenuItem(ParentMI,itmRefactorFindOverloads,'itmFindOverloads',srkmecFindOverloadsCapt);
      {$ENDIF}

      CreateMenuSeparatorSection(SubParentMI,itmRefactorTools,'itmRefactorTools');
      ParentMI:=itmRefactorTools;
      CreateMenuItem(ParentMI,itmRefactorMakeResourceString,'itmRefactorMakeResourceString',
                     lisMenuMakeResourceString,'menu_tool_make_resourcestring');
    // CodeToolChecks
    CreateMenuSeparatorSection(mnuSource,itmSourceCodeToolChecks,'itmSourceCodeToolChecks');
    ParentMI:=itmSourceCodeToolChecks;
    CreateMenuItem(ParentMI,itmSourceSyntaxCheck,'itmSourceSyntaxCheck',lisMenuQuickSyntaxCheck, 'menu_tool_syntax_check');
    CreateMenuItem(ParentMI,itmSourceGuessUnclosedBlock,'itmSourceGuessUnclosedBlock',lisMenuGuessUnclosedBlock);
    CreateMenuItem(ParentMI,itmSourceGuessMisplacedIFDEF,'itmSourceGuessMisplacedIFDEF',lisMenuGuessMisplacedIFDEF);

    CreateMenuSeparatorSection(mnuSource,itmSourceInsertions,'itmSourceInsertions');
    ParentMI:=itmSourceInsertions;
    // *** insert text ***:
    CreateMenuSubSection(ParentMI,itmSourceInsertCVSKeyWord,'itmSourceInsertCVSKeyWord',lisMenuInsertCVSKeyword);
    SubParentMI:=itmSourceInsertCVSKeyWord;
      // insert CVS keyword sub menu items
      CreateMenuItem(SubParentMI,itmSourceInsertCVSAuthor,'itmSourceInsertCVSAuthor','Author');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSDate,'itmSourceInsertCVSDate','Date');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSHeader,'itmSourceInsertCVSHeader','Header');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSID,'itmSourceInsertCVSID','ID');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSLog,'itmSourceInsertCVSLog','Log');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSName,'itmSourceInsertCVSName','Name');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSRevision,'itmSourceInsertCVSRevision','Revision');
      CreateMenuItem(SubParentMI,itmSourceInsertCVSSource,'itmSourceInsertCVSSource','Source');

    CreateMenuSubSection(ParentMI,itmSourceInsertGeneral,'itmSourceInsertGeneral',lisMenuInsertGeneral);
    SubParentMI:=itmSourceInsertGeneral;
      // insert general text sub menu items
      CreateMenuItem(SubParentMI,itmSourceInsertGPLNotice,'itmSourceInsertGPLNotice',lisMenuInsertGPLNotice);
      CreateMenuItem(SubParentMI,itmSourceInsertLGPLNotice,'itmSourceInsertLGPLNotice',lisMenuInsertLGPLNotice);
      CreateMenuItem(SubParentMI,itmSourceInsertModifiedLGPLNotice,'itmSourceInsertModifiedLGPLNotice',lisMenuInsertModifiedLGPLNotice);
      CreateMenuItem(SubParentMI,itmSourceInsertUsername,'itmSourceInsertUsername',lisMenuInsertUsername);
      CreateMenuItem(SubParentMI,itmSourceInsertDateTime,'itmSourceInsertDateTime',lisMenuInsertDateTime);
      CreateMenuItem(SubParentMI,itmSourceInsertChangeLogEntry,'itmSourceInsertChangeLogEntry',lisMenuInsertChangeLogEntry);
      CreateMenuItem(SubParentMI,itmSourceInsertGUID,'itmSourceInsertGUID',srkmecInsertGUID);

    CreateMenuItem(itmSourceInsertions,itmSourceInsertFilename,'itmSourceInsertFilename',lisMenuInsertFilename);

    CreateMenuSeparatorSection(mnuSource,itmSourceTools,'itmSourceTools');
    ParentMI:=itmSourceTools;
    CreateMenuItem(ParentMI,itmSourceUnitInfo,'itmViewUnitInfo',lisMenuViewUnitInfo, 'menu_view_unit_info');
  end;
end;

procedure TMainIDEBase.SetupProjectMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuProject,itmProjectNewSection,'itmProjectNewSection');
    ParentMI:=itmProjectNewSection;
    CreateMenuItem(ParentMI,itmProjectNew,'itmProjectNew',lisMenuNewProject, 'item_project');
    CreateMenuItem(ParentMI,itmProjectNewFromFile,'itmProjectNewFromFile',lisMenuNewProjectFromFile, 'menu_project_from_file');

    CreateMenuSeparatorSection(mnuProject,itmProjectOpenSection,'itmProjectOpenSection');
    ParentMI:=itmProjectOpenSection;
    CreateMenuItem(ParentMI,itmProjectOpen,'itmProjectOpen',lisMenuOpenProject,'menu_project_open');
    CreateMenuSubSection(ParentMI,itmProjectRecentOpen,'itmProjectRecentOpen',lisMenuOpenRecentProject);
    CreateMenuItem(ParentMI,itmProjectClose,'itmProjectClose',lisMenuCloseProject, 'menu_project_close');

    CreateMenuSeparatorSection(mnuProject,itmProjectSaveSection,'itmProjectSaveSection');
    ParentMI:=itmProjectSaveSection;
    CreateMenuItem(ParentMI,itmProjectSave,'itmProjectSave',lisMenuSaveProject, 'menu_project_save');
    CreateMenuItem(ParentMI,itmProjectSaveAs,'itmProjectSaveAs',lisMenuSaveProjectAs, 'menu_project_saveas');
    CreateMenuItem(ParentMI,itmProjectPublish,'itmProjectPublish',lisMenuPublishProject);

    CreateMenuSeparatorSection(mnuProject,itmProjectWindowSection,'itmProjectWindowSection');
    ParentMI:=itmProjectWindowSection;
    CreateMenuItem(ParentMI,itmProjectInspector,'itmProjectInspector',lisMenuProjectInspector,'menu_project_inspector');
    CreateMenuItem(ParentMI,itmProjectOptions,'itmProjectOptions',lisMenuProjectOptions,'menu_project_options');

    CreateMenuSeparatorSection(mnuProject,itmProjectAddRemoveSection,'itmProjectAddRemoveSection');
    ParentMI:=itmProjectAddRemoveSection;
    CreateMenuItem(ParentMI,itmProjectAddTo,'itmProjectAddTo',lisMenuAddToProject, 'menu_project_add');
    CreateMenuItem(ParentMI,itmProjectRemoveFrom,'itmProjectRemoveFrom',lisMenuRemoveFromProject, 'menu_project_remove');
    CreateMenuItem(ParentMI,itmProjectViewUnits,'itmProjectViewUnits',lisMenuViewUnits, 'menu_view_units');
    CreateMenuItem(ParentMI,itmProjectViewForms,'itmProjectViewForms',lisMenuViewForms, 'menu_view_forms');
    CreateMenuItem(ParentMI,itmProjectViewSource,'itmProjectViewSource',lisMenuViewProjectSource, 'menu_project_viewsource');
  end;
end;

procedure TMainIDEBase.SetupRunMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuRun,itmRunBuilding,'itmRunBuilding');
    ParentMI:=itmRunBuilding;
    CreateMenuItem(ParentMI,itmRunMenuCompile,'itmRunMenuCompile',lisMenuCompile,'menu_build');
    CreateMenuItem(ParentMI,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build_all');
    CreateMenuItem(ParentMI,itmRunMenuQuickCompile,'itmRunMenuQuickCompile',lisMenuQuickCompile,'menu_quick_compile');
    CreateMenuItem(ParentMI,itmRunMenuCleanUpCompiled,'itmRunMenuCleanUpCompiled',lisMenuCleanUpCompiled,'menu_build');
    CreateMenuItem(ParentMI,itmRunMenuAbortBuild,'itmRunMenuAbortBuild',lisMenuAbortBuild,'menu_abort_build');

    CreateMenuSeparatorSection(mnuRun,itmRunnning,'itmRunnning');
    ParentMI:=itmRunnning;
    CreateMenuItem(ParentMI,itmRunMenuRun,'itmRunMenuRun',lisMenuProjectRun,'menu_run');
    CreateMenuItem(ParentMI,itmRunMenuPause,'itmRunMenuPause',lisMenuPause,'menu_pause', False);
    CreateMenuItem(ParentMI,itmRunMenuShowExecutionPoint,'itmRunMenuShowExecutionPoint',
                   lisMenuShowExecutionPoint,'debugger_show_execution_point', False);
    CreateMenuItem(ParentMI,itmRunMenuStepInto,'itmRunMenuStepInto',lisMenuStepInto,'menu_stepinto');
    CreateMenuItem(ParentMI,itmRunMenuStepOver,'itmRunMenuStepOver',lisMenuStepOver,'menu_stepover');
    CreateMenuItem(ParentMI,itmRunMenuStepOut,'itmRunMenuStepOut',lisMenuStepOut,'menu_stepout');
    CreateMenuItem(ParentMI,itmRunMenuRunToCursor,'itmRunMenuRunToCursor',lisMenuRunToCursor,'menu_run_cursor');
    CreateMenuItem(ParentMI,itmRunMenuStop,'itmRunMenuStop',lisMenuStop,'menu_stop', False);
    CreateMenuItem(ParentMI,itmRunMenuRunParameters,'itmRunMenuRunParameters',lisMenuRunParameters, 'menu_run_parameters');
    CreateMenuItem(ParentMI,itmRunMenuResetDebugger,'itmRunMenuResetDebugger',lisMenuResetDebugger, 'menu_reset_debugger');

    CreateMenuSeparatorSection(mnuRun,itmRunBuildingFile,'itmRunBuildingFile');
    ParentMI:=itmRunBuildingFile;
    CreateMenuItem(ParentMI,itmRunMenuBuildFile,'itmRunMenuBuildFile',lisMenuBuildFile, 'menu_build_file');
    CreateMenuItem(ParentMI,itmRunMenuRunFile,'itmRunMenuRunFile',lisMenuRunFile,'menu_run_file');
    CreateMenuItem(ParentMI,itmRunMenuConfigBuildFile,'itmRunMenuConfigBuildFile',lisMenuConfigBuildFile, 'menu_build_run_file');

    CreateMenuSeparatorSection(mnuRun,itmRunDebugging,'itmRunDebugging');
    ParentMI:=itmRunDebugging;
    CreateMenuItem(ParentMI,itmRunMenuInspect,'itmRunMenuInspect',lisMenuInspect, 'debugger_inspect', False);
    CreateMenuItem(ParentMI,itmRunMenuEvaluate,'itmRunMenuEvaluate',lisMenuEvaluate, 'debugger_modify', False);
    CreateMenuItem(ParentMI,itmRunMenuAddWatch,'itmRunMenuAddWatch',lisMenuAddWatch, '', False);
    CreateMenuSubSection(ParentMI,itmRunMenuAddBreakpoint,'itmRunMenuAddBreakpoint',lisMenuAddBreakpoint, '');
    CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBPSource,'itmRunMenuAdddBPSource',lisSourceBreakpoint, '', False);
    CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBPAddress,'itmRunMenuAddBPAddress',lisAddressBreakpoint, '', False);
    CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBpWatchPoint,'itmRunMenuAddBpWatchPoint',lisWatchPointBreakpoint, '', False);
  end;
end;

procedure TMainIDEBase.SetupPackageMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuComponent,itmPkgOpening,'itmPkgOpening');
    ParentMI:=itmPkgOpening;
    CreateMenuItem(ParentMI,itmPkgNewPackage,'itmPkgNewPackage',lisMenuNewPackage);
    CreateMenuItem(ParentMI,itmPkgOpenPackage,'itmPkgOpenPackage',lisMenuOpenPackage,'pkg_installed');
    CreateMenuItem(ParentMI,itmPkgOpenPackageFile,'itmPkgOpenPackageFile',lisMenuOpenPackageFile,'pkg_open');
    CreateMenuItem(ParentMI,itmPkgOpenPackageOfCurUnit,'itmPkgOpenPackageOfCurUnit',lisMenuOpenPackageOfCurUnit);
    CreateMenuSubSection(ParentMI,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg);

    CreateMenuSeparatorSection(mnuComponent,itmPkgUnits,'itmPkgUnits');
    ParentMI:=itmPkgUnits;
    CreateMenuItem(ParentMI,itmPkgAddCurFileToPkg,'itmPkgAddCurFileToPkg',lisMenuAddCurFileToPkg,'pkg_add');

    CreateMenuSeparatorSection(mnuComponent,itmPkgGraphSection,'itmPkgGraphSection');
    ParentMI:=itmPkgGraphSection;
    CreateMenuItem(ParentMI,itmPkgPkgGraph,'itmPkgPkgGraph',lisMenuPackageGraph,'pkg_graph');
    CreateMenuItem(ParentMI,itmPkgEditInstallPkgs,'itmPkgEditInstallPkgs',lisMenuEditInstallPkgs,'pkg_properties');

    {$IFDEF CustomIDEComps}
    CreateMenuItem(ParentMI,itmCompsConfigCustomComps,'itmCompsConfigCustomComps',lisMenuConfigCustomComps);
    {$ENDIF}
  end;
end;

procedure TMainIDEBase.SetupToolsMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuTools,itmOptionsDialogs,'itmOptionsDialogs');
    ParentMI:=itmOptionsDialogs;
    CreateMenuItem(ParentMI,itmEnvGeneralOptions,'itmEnvGeneralOptions',
                   lisMenuGeneralOptions,'menu_environment_options');
    CreateMenuItem(ParentMI,itmToolRescanFPCSrcDir,'itmToolRescanFPCSrcDir',
                   lisMenuRescanFPCSourceDirectory);
    CreateMenuItem(ParentMI,itmEnvCodeTemplates,'itmEnvCodeTemplates',lisMenuEditCodeTemplates,'');
    CreateMenuItem(ParentMI,itmEnvCodeToolsDefinesEditor,'itmEnvCodeToolsDefinesEditor',
                   lisMenuCodeToolsDefinesEditor,'menu_codetoolsdefineseditor');

    CreateMenuSeparatorSection(mnuTools,itmCustomTools,'itmCustomTools');
    ParentMI:=itmCustomTools;
    CreateMenuItem(ParentMI,itmToolConfigure,'itmToolConfigure',lisMenuConfigExternalTools);

    CreateMenuSeparatorSection(mnuTools,itmSecondaryTools,'itmSecondaryTools');
    ParentMI:=itmSecondaryTools;
    CreateMenuItem(ParentMI,itmToolManageExamples,'itmToolManageExamples',lisMenuExampleProjects, 'camera');
    CreateMenuItem(ParentMI,itmToolDiff,'itmToolDiff',lisMenuDiff, 'menu_tool_diff');

    CreateMenuSeparatorSection(mnuTools,itmDelphiConversion,'itmDelphiConversion');
    ParentMI:=itmDelphiConversion;
    CreateMenuItem(ParentMI,itmToolCheckLFM,'itmToolCheckLFM',lisMenuCheckLFM, 'menu_tool_check_lfm');
    CreateMenuItem(ParentMI,itmToolConvertDelphiUnit,'itmToolConvertDelphiUnit',lisMenuConvertDelphiUnit,'menu_tool_dfm_to_lfm');
    CreateMenuItem(ParentMI,itmToolConvertDelphiProject,'itmToolConvertDelphiProject',lisMenuConvertDelphiProject,'menu_tool_dfm_to_lfm');
    CreateMenuItem(ParentMI,itmToolConvertDelphiPackage,'itmToolConvertDelphiPackage',lisMenuConvertDelphiPackage,'menu_tool_dfm_to_lfm');
    CreateMenuItem(ParentMI,itmToolConvertDFMtoLFM,'itmToolConvertDFMtoLFM',lisMenuConvertDFMtoLFM,'menu_tool_dfm_to_lfm');
    CreateMenuItem(ParentMI,itmToolConvertEncoding,'itmToolConvertEncoding',lisMenuConvertEncoding);

    CreateMenuSeparatorSection(mnuTools,itmBuildingLazarus,'itmBuildingLazarus');
    ParentMI:=itmBuildingLazarus;
    CreateMenuItem(ParentMI,itmToolBuildLazarus,'itmToolBuildLazarus',lisMenuBuildLazarus,'menu_build_lazarus');
    CreateMenuItem(ParentMI,itmToolConfigureBuildLazarus,'itmToolConfigureBuildLazarus',
                   lisMenuConfigureBuildLazarus, 'menu_configure_build_lazarus');
  end;
end;

procedure TMainIDEBase.SetupWindowsMenu;
begin

end;

procedure TMainIDEBase.SetupHelpMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuHelp,itmOnlineHelps,'itmOnlineHelps');
    ParentMI:=itmOnlineHelps;

    CreateMenuItem(ParentMI,itmHelpOnlineHelp,'itmHelpOnlineHelp',
                   lisMenuOnlineHelp, 'menu_help');
    CreateMenuItem(ParentMI,itmHelpReportingBug,'itmHelpReportingBug',
                   lisMenuReportingBug, 'menu_reportingbug');

    // old behavior restored, until Tiger issue is fixed.
    // http://bugs.freepascal.org/view.php?id=14411
    (*
   {$ifdef LCLCarbon}
    // under Carbon: add About item to the Apple menu
    CreateMenuItem(mnuApple, itmHelpAboutLazarus,'itmHelpAboutLazarus',
                   lisAboutLazarus, 'menu_information');
    
    CreateMenuSeparatorSection(mnuHelp,itmInfoHelps,'itmInfoHelps');
    {$else}*)
    // otherwise: add About item to the Help menu
    CreateMenuSeparatorSection(mnuHelp,itmInfoHelps,'itmInfoHelps');
    ParentMI:=itmInfoHelps;
    CreateMenuItem(ParentMI,itmHelpAboutLazarus,'itmHelpAboutLazarus',
                 lisAboutLazarus, 'menu_information');
    //{$endif}

    CreateMenuSeparatorSection(mnuHelp,itmHelpTools,'itmHelpTools');
    ParentMI:=itmHelpTools;
  end;
end;

procedure TMainIDEBase.LoadMenuShortCuts;

  function GetCommand(ACommand: word): TIDECommand;
  begin
    Result:=IDECommandList.FindIDECommand(ACommand);
  end;

begin
  with MainIDEBar do begin
    // file menu
    itmFileNewUnit.Command:=GetCommand(ecNewUnit);
    itmFileNewForm.Command:=GetCommand(ecNewForm);
    itmFileNewOther.Command:=GetCommand(ecNew);
    itmFileOpen.Command:=GetCommand(ecOpen);
    itmFileRevert.Command:=GetCommand(ecRevert);
    itmFileSave.Command:=GetCommand(ecSave);
    itmFileSaveAs.Command:=GetCommand(ecSaveAs);
    itmFileSaveAll.Command:=GetCommand(ecSaveAll);
    itmFileClose.Command:=GetCommand(ecClose);
    itmFileCloseAll.Command:=GetCommand(ecCloseAll);
    itmFileCleanDirectory.Command:=GetCommand(ecCleanDirectory);
    itmFileQuit.Command:=GetCommand(ecQuit);
    itmFileQuit.Command:=GetCommand(ecQuit);

    // edit menu
    itmEditUndo.Command:=GetCommand(ecUndo);
    itmEditRedo.Command:=GetCommand(ecRedo);
    itmEditCut.Command:=GetCommand(ecCut);
    itmEditCopy.Command:=GetCommand(ecCopy);
    itmEditPaste.Command:=GetCommand(ecPaste);

    itmEditSelectAll.Command:=GetCommand(ecSelectAll);
    itmEditSelectToBrace.Command:=GetCommand(ecSelectToBrace);
    itmEditSelectCodeBlock.Command:=GetCommand(ecSelectCodeBlock);
    itmEditSelectWord.Command:=GetCommand(ecSelectWord);
    itmEditSelectLine.Command:=GetCommand(ecSelectLine);
    itmEditSelectParagraph.Command:=GetCommand(ecSelectParagraph);

    itmEditIndentBlock.Command:=GetCommand(ecBlockIndent);
    itmEditUnindentBlock.Command:=GetCommand(ecBlockUnindent);
    itmEditUpperCaseBlock.Command:=GetCommand(ecSelectionUpperCase);
    itmEditLowerCaseBlock.Command:=GetCommand(ecSelectionLowerCase);
    itmEditSwapCaseBlock.Command:=GetCommand(ecSelectionSwapCase);
    itmEditSortBlock.Command:=GetCommand(ecSelectionSort);
    itmEditTabsToSpacesBlock.Command:=GetCommand(ecSelectionTabs2Spaces);
    itmEditSelectionBreakLines.Command:=GetCommand(ecSelectionBreakLines);

    itmEditInsertCharacter.Command:=GetCommand(ecInsertCharacter);

    // search menu
    itmSearchFind.Command:=GetCommand(ecFind);
    itmSearchFindNext.Command:=GetCommand(ecFindNext);
    itmSearchFindPrevious.Command:=GetCommand(ecFindPrevious);
    itmSearchFindInFiles.Command:=GetCommand(ecFindInFiles);
    itmSearchFindIdentifierRefs.Command:=GetCommand(ecFindIdentifierRefs);
    itmSearchReplace.Command:=GetCommand(ecReplace);
    itmIncrementalFind.Command:=GetCommand(ecIncrementalFind);
    itmGotoLine.Command:=GetCommand(ecGotoLineNumber);
    itmJumpBack.Command:=GetCommand(ecJumpBack);
    itmJumpForward.Command:=GetCommand(ecJumpForward);
    itmAddJumpPoint.Command:=GetCommand(ecAddJumpPoint);
    itmJumpToNextError.Command:=GetCommand(ecJumpToNextError);
    itmJumpToPrevError.Command:=GetCommand(ecJumpToPrevError);
    itmSetFreeBookmark.Command:=GetCommand(ecSetFreeBookmark);
    itmJumpToNextBookmark.Command:=GetCommand(ecNextBookmark);
    itmJumpToPrevBookmark.Command:=GetCommand(ecPrevBookmark);
    itmFindBlockOtherEnd.Command:=GetCommand(ecFindBlockOtherEnd);
    itmFindBlockStart.Command:=GetCommand(ecFindBlockStart);
    itmFindDeclaration.Command:=GetCommand(ecFindDeclaration);
    itmOpenFileAtCursor.Command:=GetCommand(ecOpenFileAtCursor);
    itmGotoIncludeDirective.Command:=GetCommand(ecGotoIncludeDirective);
    itmSearchProcedureList.Command:=GetCommand(ecProcedureList);

    // view menu
    itmViewInspector.Command:=GetCommand(ecToggleObjectInsp);
    itmViewSourceEditor.Command:=GetCommand(ecToggleSourceEditor);
    itmViewCodeExplorer.Command:=GetCommand(ecToggleCodeExpl);
    itmViewFPDocEditor.Command:=GetCommand(ecToggleFPDocEditor);
    itmViewCodeBrowser.Command:=GetCommand(ecToggleCodeBrowser);
    itmViewRestrictionBrowser.Command:=GetCommand(ecToggleRestrictionBrowser);
    itmViewComponents.Command:=GetCommand(ecViewComponents);
    itmJumpHistory.Command:=GetCommand(ecViewJumpHistory);
    itmViewUnitDependencies.Command:=GetCommand(ecViewUnitDependencies);
    itmViewToggleFormUnit.Command:=GetCommand(ecToggleFormUnit);
    itmViewMessage.Command:=GetCommand(ecToggleMessages);
    itmViewSearchResults.Command:=GetCommand(ecToggleSearchResults);
    itmViewAnchorEditor.Command:=GetCommand(ecViewAnchorEditor);
    itmViewTabOrder.Command:=GetCommand(ecViewTabOrder);
    itmViewComponentPalette.Command:=GetCommand(ecToggleCompPalette);
    itmViewIDESpeedButtons.Command:=GetCommand(ecToggleIDESpeedBtns);
    //itmViewPackageLinks.Command:=GetCommand(ec?);

    // source menu
    itmSourceCommentBlock.Command:=GetCommand(ecSelectionComment);
    itmSourceUncommentBlock.Command:=GetCommand(ecSelectionUncomment);
    itmSourceToggleComment.Command:=GetCommand(ecToggleComment);
    itmSourceEncloseBlock.Command:=GetCommand(ecSelectionEnclose);
    itmSourceEncloseInIFDEF.Command:=GetCommand(ecSelectionEncloseIFDEF);
    itmSourceCompleteCode.Command:=GetCommand(ecCompleteCode);
    itmSourceUseUnit.Command:=GetCommand(ecUseUnit);

    itmSourceSyntaxCheck.Command:=GetCommand(ecSyntaxCheck);
    itmSourceGuessUnclosedBlock.Command:=GetCommand(ecGuessUnclosedBlock);
    itmSourceGuessMisplacedIFDEF.Command:=GetCommand(ecGuessMisplacedIFDEF);

    itmSourceInsertCVSAuthor.Command:=GetCommand(ecInsertCVSAuthor);
    itmSourceInsertCVSDate.Command:=GetCommand(ecInsertCVSDate);
    itmSourceInsertCVSHeader.Command:=GetCommand(ecInsertCVSHeader);
    itmSourceInsertCVSID.Command:=GetCommand(ecInsertCVSID);
    itmSourceInsertCVSLog.Command:=GetCommand(ecInsertCVSLog);
    itmSourceInsertCVSName.Command:=GetCommand(ecInsertCVSName);
    itmSourceInsertCVSRevision.Command:=GetCommand(ecInsertCVSRevision);
    itmSourceInsertCVSSource.Command:=GetCommand(ecInsertCVSSource);

    itmSourceInsertGPLNotice.Command:=GetCommand(ecInsertGPLNotice);
    itmSourceInsertLGPLNotice.Command:=GetCommand(ecInsertLGPLNotice);
    itmSourceInsertModifiedLGPLNotice.Command:=GetCommand(ecInsertModifiedLGPLNotice);
    itmSourceInsertUsername.Command:=GetCommand(ecInsertUserName);
    itmSourceInsertDateTime.Command:=GetCommand(ecInsertDateTime);
    itmSourceInsertChangeLogEntry.Command:=GetCommand(ecInsertChangeLogEntry);
    itmSourceInsertGUID.Command:=GetCommand(ecInsertGUID);
    itmSourceInsertFilename.Command:=GetCommand(ecInsertFilename);

    itmSourceUnitInfo.Command:=GetCommand(ecViewUnitInfo);

    // refactor menu
    itmRefactorRenameIdentifier.Command:=GetCommand(ecRenameIdentifier);
    itmRefactorExtractProc.Command:=GetCommand(ecExtractProc);
    itmRefactorInvertAssignment.Command:=GetCommand(ecInvertAssignment);

    itmRefactorShowAbstractMethods.Command:=GetCommand(ecShowAbstractMethods);
    itmRefactorShowEmptyMethods.Command:=GetCommand(ecRemoveEmptyMethods);
    itmRefactorShowUnusedUnits.Command:=GetCommand(ecRemoveUnusedUnits);
    {$IFDEF EnableFindOverloads}
    itmRefactorFindOverloads.Command:=GetCommand(ecFindOverloads);
    {$ENDIF}
    itmRefactorMakeResourceString.Command:=GetCommand(ecMakeResourceString);

    // project menu
    itmProjectNew.Command:=GetCommand(ecNewProject);
    itmProjectNewFromFile.Command:=GetCommand(ecNewProjectFromFile);
    itmProjectOpen.Command:=GetCommand(ecOpenProject);
    itmProjectClose.Command:=GetCommand(ecCloseProject);
    itmProjectSave.Command:=GetCommand(ecSaveProject);
    itmProjectSaveAs.Command:=GetCommand(ecSaveProjectAs);
    itmProjectPublish.Command:=GetCommand(ecPublishProject);
    itmProjectInspector.Command:=GetCommand(ecProjectInspector);
    itmProjectOptions.Command:=GetCommand(ecProjectOptions);
    itmProjectAddTo.Command:=GetCommand(ecAddCurUnitToProj);
    itmProjectRemoveFrom.Command:=GetCommand(ecRemoveFromProj);
    itmProjectViewUnits.Command:=GetCommand(ecViewProjectUnits);
    itmProjectViewForms.Command:=GetCommand(ecViewProjectForms);
    itmProjectViewSource.Command:=GetCommand(ecViewProjectSource);

    // run menu
    itmRunMenuCompile.Command:=GetCommand(ecCompile);
    itmRunMenuBuild.Command:=GetCommand(ecBuild);
    itmRunMenuQuickCompile.Command:=GetCommand(ecQuickCompile);
    itmRunMenuAbortBuild.Command:=GetCommand(ecAbortBuild);
    itmRunMenuRun.Command:=GetCommand(ecRun);
    itmRunMenuPause.Command:=GetCommand(ecPause);
    itmRunMenuStepInto.Command:=GetCommand(ecStepInto);
    itmRunMenuStepOver.Command:=GetCommand(ecStepOver);
    itmRunMenuStepOut.Command:=GetCommand(ecStepOut);
    itmRunMenuRunToCursor.Command:=GetCommand(ecRunToCursor);
    itmRunMenuStop.Command:=GetCommand(ecStopProgram);
    itmRunMenuResetDebugger.Command:=GetCommand(ecResetDebugger);
    itmRunMenuRunParameters.Command:=GetCommand(ecRunParameters);
    itmRunMenuBuildFile.Command:=GetCommand(ecBuildFile);
    itmRunMenuRunFile.Command:=GetCommand(ecRunFile);
    itmRunMenuConfigBuildFile.Command:=GetCommand(ecConfigBuildFile);

    // components menu
    itmPkgNewPackage.Command:=GetCommand(ecNewPackage);
    itmPkgOpenPackage.Command:=GetCommand(ecOpenPackage);
    itmPkgOpenPackageFile.Command:=GetCommand(ecOpenPackageFile);
    itmPkgOpenPackageOfCurUnit.Command:=GetCommand(ecOpenPackageOfCurUnit);
    itmPkgAddCurFileToPkg.Command:=GetCommand(ecAddCurFileToPkg);
    itmPkgPkgGraph.Command:=GetCommand(ecPackageGraph);
    itmPkgEditInstallPkgs.Command:=GetCommand(ecEditInstallPkgs);
    {$IFDEF CustomIDEComps}
    itmCompsConfigCustomComps.Command:=GetCommand(ecConfigCustomComps);
    {$ENDIF}

    // tools menu
    itmEnvGeneralOptions.Command:=GetCommand(ecEnvironmentOptions);
    itmToolRescanFPCSrcDir.Command:=GetCommand(ecRescanFPCSrcDir);
    itmEnvCodeTemplates.Command:=GetCommand(ecEditCodeTemplates);
    itmEnvCodeToolsDefinesEditor.Command:=GetCommand(ecCodeToolsDefinesEd);

    itmToolConfigure.Command:=GetCommand(ecExtToolSettings);
    itmToolDiff.Command:=GetCommand(ecDiff);

    itmToolConvertDFMtoLFM.Command:=GetCommand(ecConvertDFM2LFM);
    itmToolCheckLFM.Command:=GetCommand(ecCheckLFM);
    itmToolConvertDelphiUnit.Command:=GetCommand(ecConvertDelphiUnit);
    itmToolConvertDelphiProject.Command:=GetCommand(ecConvertDelphiProject);
    itmToolConvertDelphiPackage.Command:=GetCommand(ecConvertDelphiPackage);
    itmToolConvertEncoding.Command:=GetCommand(ecConvertEncoding);
    itmToolManageExamples.Command:=GetCommand(ecManageExamples);
    itmToolBuildLazarus.Command:=GetCommand(ecBuildLazarus);
    itmToolConfigureBuildLazarus.Command:=GetCommand(ecConfigBuildLazarus);

    // help menu
    itmHelpAboutLazarus.Command:=GetCommand(ecAboutLazarus);
    itmHelpOnlineHelp.Command:=GetCommand(ecOnlineHelp);
    itmHelpReportingBug.Command:=GetCommand(ecReportingBug);
  end;
end;

function TMainIDEBase.GetToolStatus: TIDEToolStatus;
begin
  Result:=FToolStatus;
end;

function TMainIDEBase.DoOpenMacroFile(Sender: TObject; const AFilename: string
  ): TModalResult;
begin
  Result:=DoOpenEditorFile(AFilename,-1,-1,
                  [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros]);
end;

procedure TMainIDEBase.UpdateWindowMenu(Immediately: boolean = false);
const
  UpdatePause = 5/864000; // half a second

  function GetMenuItem(Index: Integer): TIDEMenuItem; inline;
  begin
    if mnuWindow.Count > Index then
      Result := mnuWindow.Items[Index]
    else
      Result := RegisterIDEMenuCommand(mnuWindow.GetPath,
                                          'Window'+IntToStr(Index),'');
  end;

var
  WindowsList: TFPList;
  i, ItemCount: Integer;
  CurMenuItem: TIDEMenuItem;
  AForm: TForm;
  t: TDateTime;
begin
  t:=Now;
  if (not Immediately) then begin
    if (FLastWindowMenuUpdate<>0) and (t-FLastWindowMenuUpdate<UpdatePause) then
      exit;
  end;
  FLastWindowMenuUpdate:=t;

  WindowsList:=TFPList.Create;
  // add typical IDE windows at the start of the list
  for i := 0 to SourceEditorManager.SourceWindowCount - 1 do
    WindowsList.Add(SourceEditorManager.SourceWindows[i]);
  if (ObjectInspector1<>nil) and (ObjectInspector1.Visible) then
    WindowsList.Add(ObjectInspector1);
  // add special IDE windows
  for i:=0 to Screen.FormCount-1 do begin
    AForm:=Screen.Forms[i];
    //debugln(['TMainIDEBase.UpdateWindowMenu ',DbgSName(AForm),' Vis=',AForm.IsVisible,' Des=',DbgSName(AForm.Designer)]);
    if (not AForm.IsVisible) or (AForm=MainIDEBar) or (AForm=SplashForm)
    or (AForm.Designer<>nil) or (WindowsList.IndexOf(AForm)>=0) then
      continue;
    if IDEDockMaster<>nil then
    begin
      if not IDEDockMaster.AddableInWindowMenu(AForm) then continue;
    end else begin
      if AForm.Parent<>nil then continue;
    end;
    WindowsList.Add(AForm);
  end;
  // add designer forms and datamodule forms
  for i:=0 to Screen.FormCount-1 do begin
    AForm:=Screen.Forms[i];
    if (AForm.Designer<>nil) and (WindowsList.IndexOf(AForm)<0) then
      WindowsList.Add(AForm);
  end;

  // create menuitems
  ItemCount := WindowsList.Count;
  for i:=0 to WindowsList.Count-1 do
  begin
    CurMenuItem := GetMenuItem(i);
    CurMenuItem.Caption:=TCustomForm(WindowsList[i]).Caption;
    CurMenuItem.MenuItem.Checked := Screen.ActiveCustomForm = TCustomForm(WindowsList[i]);
    CurMenuItem.OnClick:=@mnuWindowItemClick;
  end;

  //create source page menuitems

  if (SourceEditorManager.SourceWindowCount > 0)
     and not (nbcPageListPopup in SourceEditorManager.SourceWindows[0].GetCapabilities) then
  begin
    CurMenuItem := GetMenuItem(ItemCount);
    CurMenuItem.OnClick:=nil;
    CurMenuItem.Caption:='-';
    inc(ItemCount);

    for i := 0 to SourceEditorManager.SourceEditorCount - 1 do
    begin
      CurMenuItem := GetMenuItem(ItemCount);
      CurMenuItem.Caption := SourceEditorManager.SourceEditors[i].PageName;
      CurMenuItem.MenuItem.Checked := SourceEditorManager.ActiveEditor = SourceEditorManager.SourceEditors[i] ;
      CurMenuItem.OnClick := @mnuWindowSourceItemClick;
      CurMenuItem.Tag := i;
      inc(ItemCount);
    end;
  end;
  // remove unused menuitems
  while mnuWindow.Count > ItemCount do
    mnuWindow.Items[mnuWindow.Count-1].Free;

    // clean up
  WindowsList.Free;
end;

procedure TMainIDEBase.SetRecentSubMenu(Section: TIDEMenuSection;
  FileList: TStringList; OnClickEvent: TNotifyEvent);
var
  i: integer;
  AMenuItem: TIDEMenuItem;
begin
  // create enough menuitems
  while Section.Count<FileList.Count do begin
    AMenuItem:=RegisterIDEMenuCommand(Section.GetPath,
                              Section.Name+'Recent'+IntToStr(Section.Count),'');
  end;
  // delete unused menuitems
  while Section.Count>FileList.Count do
    Section.Items[Section.Count-1].Free;
  Section.Enabled:=(Section.Count>0);
  // set captions and event
  for i:=0 to FileList.Count-1 do begin
    AMenuItem:=Section.Items[i];
    AMenuItem.Caption := FileList[i];
    AMenuItem.OnClick := OnClickEvent;
  end;
end;

procedure TMainIDEBase.UpdateHighlighters(Immediately: boolean = false);
var
  ASrcEdit: TSourceEditor;
  h: TLazSyntaxHighlighter;
  i: Integer;
  AnEditorInfo: TUnitEditorInfo;
begin
  if Immediately then begin
    FNeedUpdateHighlighters:=false;
    Project1.UpdateAllSyntaxHighlighter;
    for h := Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
      if Highlighters[h]<>nil then begin
        Highlighters[h].BeginUpdate;
        EditorOpts.GetHighlighterSettings(Highlighters[h]);
        Highlighters[h].EndUpdate;
      end;
    for i := 0 to SourceEditorManager.SourceEditorCount - 1 do begin
      ASrcEdit := SourceEditorManager.SourceEditors[i];
      AnEditorInfo:=Project1.EditorInfoWithEditorComponent(ASrcEdit);
      if AnEditorInfo <> nil then
        ASrcEdit.SyntaxHighlighterType := AnEditorInfo.SyntaxHighlighter;
    end;
  end else begin
    FNeedUpdateHighlighters:=true;
  end;
end;

//function TMainIDEBase.DoJumpToCodePos(ActiveSrcEdit: TSourceEditor; ActiveUnitInfo: TUnitInfo;
//  NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer; AddJumpPoint: boolean;
//  FocusEditor: Boolean; MarkLine: Boolean): TModalResult;
//var
//  Flags: TJumpToCodePosFlags;
//begin
//  Flags := [];
//  if FocusEditor then Include(Flags, jfFocusEditor);
//  if AddJumpPoint then Include(Flags, jfAddJumpPoint);
//  if MarkLine then Include(Flags, jfMarkLine);
//  DoJumpToCodePosition(ActiveSrcEdit, ActiveUnitInfo, NewSource, NewX, NewY, NewTopLine,
//    Flags)
//end;

procedure TMainIDEBase.FindInFilesPerDialog(AProject: TProject);
begin
  FindInFilesDialog.FindInFilesPerDialog(AProject);
end;

procedure TMainIDEBase.FindInFiles(AProject: TProject; const FindText: string);
begin
  FindInFilesDialog.FindInFiles(AProject, FindText);
end;

end.


