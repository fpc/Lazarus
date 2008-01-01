{  $Id$  }
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
  Classes, LCLType, LCLIntf, StdCtrls, Buttons, Menus, ComCtrls, SysUtils,
  Controls, Graphics, ExtCtrls, Dialogs, FileUtil, Forms, CodeToolManager,
  CodeCache, AVL_Tree, SynEditKeyCmds,
  // IDE
  LazConf, LazarusIDEStrConsts, SrcEditorIntf, LazIDEIntf, MenuIntf,
  IDECommands, IDEMsgIntf,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
  ComponentReg, OutputFilter,
  TransferMacros, ObjectInspector, PropEdits, IDEDefs, MsgView,
  EnvironmentOpts, EditorOptions, CompilerOptions, KeyMapping, IDEProcs,
  Debugger, IDEOptionDefs, CodeToolsDefines, Splash, Designer,
  UnitEditor, BuildManager,
  MainBar, MainIntf;

type
  { TMainIDEBase }

  TMainIDEBase = class(TMainIDEInterface)
  private
    FToolStatus: TIDEToolStatus;
  protected
    OwningComponent: TComponent;

    function GetMainBar: TComponent; override;

    function CreateMenuSeparator : TMenuItem;
    procedure CreateMenuItem(Section: TIDEMenuSection;
                             var MenuItem: TIDEMenuCommand;
                             const MenuItemName, MenuItemCaption: String;
                             const bmpName: String = '';
                             mnuEnabled: Boolean = true;
                             mnuChecked: Boolean = false);
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
    procedure SetupProjectMenu; virtual;
    procedure SetupRunMenu; virtual;
    procedure SetupComponentsMenu; virtual;
    procedure SetupToolsMenu; virtual;
    procedure SetupEnvironmentMenu; virtual;
    procedure SetupWindowsMenu; virtual;
    procedure SetupHelpMenu; virtual;

    procedure LoadMenuShortCuts; virtual;
    function GetToolStatus: TIDEToolStatus; override;
    procedure SetToolStatus(const AValue: TIDEToolStatus); virtual;

    procedure mnuWindowsItemClick(Sender: TObject); virtual;
    procedure OnMainBarDestroy(Sender: TObject); virtual;
    
    procedure ConnectOutputFilter;
  public
    property ToolStatus: TIDEToolStatus read FToolStatus write SetToolStatus;
    function DoResetToolStatus(Interactive: boolean): boolean; virtual; abstract;

    constructor Create(TheOwner: TComponent); override;
    procedure StartIDE; virtual; abstract;
    destructor Destroy; override;
    procedure CreateOftenUsedForms; virtual; abstract;

    procedure GetUnitInfoForDesigner(ADesigner: TIDesigner;
                              var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); override;

    procedure GetCurrentUnitInfo(var ActiveSourceEditor: TSourceEditorInterface;
                              var ActiveUnitInfo: TUnitInfo); override;
    procedure GetCurrentUnit(var ActiveSourceEditor: TSourceEditor;
                             var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithPageIndex(PageIndex: integer;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetDesignerUnit(ADesigner: TDesigner;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetObjectInspectorUnit(
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithForm(AForm: TCustomForm;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    procedure GetUnitWithPersistent(APersistent: TPersistent;
          var ActiveSourceEditor: TSourceEditor; var ActiveUnitInfo: TUnitInfo); virtual; abstract;
    function GetSourceEditorForUnitInfo(AnUnitInfo: TUnitInfo): TSourceEditor; virtual; abstract;

    function DoOpenMacroFile(Sender: TObject; const AFilename: string
                             ): TModalResult; override;

    procedure UpdateWindowsMenu; override;
    procedure SetRecentSubMenu(Section: TIDEMenuSection; FileList: TStringList;
                               OnClickEvent: TNotifyEvent); override;

    function DoJumpToCodePosition(
                        ActiveSrcEdit: TSourceEditorInterface;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean): TModalResult; override;
    function DoJumpToCodePos(
                        ActiveSrcEdit: TSourceEditor;
                        ActiveUnitInfo: TUnitInfo;
                        NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer;
                        AddJumpPoint: boolean): TModalResult; virtual; abstract;
                        
    procedure FindInFilesPerDialog(AProject: TProject); override;
    procedure FindInFiles(AProject: TProject; const FindText: string); override;
  end;

var
  MainIDE: TMainIDEBase = nil;

implementation

uses
  IDEImagesIntf;

{ TMainIDEBase }

procedure TMainIDEBase.mnuWindowsItemClick(Sender: TObject);
var
  i: Integer;
begin
  i:=Screen.CustomFormCount-1;
  while (i>=0) do begin
    if Screen.CustomForms[i].Caption=(Sender as TIDEMenuCommand).Caption then
    begin
      Screen.CustomForms[i].BringToFront;
      break;
    end;
    dec(i);
  end;
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
  OwningComponent:=TComponent.Create(nil);
  inherited Create(TheOwner);
end;

destructor TMainIDEBase.Destroy;
begin
  FreeThenNil(OwningComponent);
  inherited Destroy;
  MainIDE:=nil;
end;

procedure TMainIDEBase.GetUnitInfoForDesigner(ADesigner: TIDesigner;
  var ActiveSourceEditor: TSourceEditorInterface; var ActiveUnitInfo: TUnitInfo
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
  var ActiveSourceEditor: TSourceEditorInterface; var ActiveUnitInfo: TUnitInfo
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
  var MenuItem: TIDEMenuCommand; const MenuItemName, MenuItemCaption: String;
  const bmpName: String; mnuEnabled: Boolean; mnuChecked: Boolean);
begin
  MenuItem:=RegisterIDEMenuCommand(Section,MenuItemName,MenuItemCaption);
  MenuItem.Enabled:=mnuEnabled;
  MenuItem.Checked:=mnuChecked;
  if bmpName<>'' then
    MenuItem.ImageIndex := IDEImages.LoadImage(16, bmpName);
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
    CreateMainMenuItem(mnuFile,'File',lisMenuFile);
    CreateMainMenuItem(mnuEdit,'Edit',lisMenuEdit);
    CreateMainMenuItem(mnuSearch,'Search',lisMenuSearch);
    CreateMainMenuItem(mnuView,'View',lisMenuView);
    CreateMainMenuItem(mnuProject,'Project',lisMenuProject);
    CreateMainMenuItem(mnuRun,'Run',lisMenuRun);
    CreateMainMenuItem(mnuComponents,'Components',lisMenuComponents);
    CreateMainMenuItem(mnuTools,'Tools',lisMenuTools);
    CreateMainMenuItem(mnuEnvironment,'Environment',lisMenuEnvironent);
    CreateMainMenuItem(mnuWindows,'Windows',lisMenuWindows);
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

    CreateMenuItem(ParentMI, itmFileOpen, 'itmFileOpen', lisMenuOpen, 'open');
    CreateMenuItem(ParentMI,itmFileRevert,'itmFileRevert',lisMenuRevert, 'menu_file_revert');
    CreateMenuSubSection(ParentMI,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent);
    CreateMenuItem(ParentMI,itmFileSave,'itmFileSave',lisMenuSave,'menu_save');
    CreateMenuItem(ParentMI,itmFileSaveAs,'itmFileSaveAs',lisMenuSaveAs,'menu_saveas');
    CreateMenuItem(ParentMI,itmFileSaveAll,'itmFileSaveAll',lisMenuSaveAll,'menu_save_all');
    CreateMenuItem(ParentMI,itmFileClose,'itmFileClose',lisMenuClose,'menu_close',false);
    CreateMenuItem(ParentMI,itmFileCloseAll,'itmFileCloseAll',lisMenuCloseAll,'menu_close_all',false);

    CreateMenuSeparatorSection(mnuFile,itmFileDirectories,'itmFileDirectories');
    ParentMI:=itmFileDirectories;

    CreateMenuItem(ParentMI,itmFileCleanDirectory,'itmFileCleanDirectory',lisMenuCleanDirectory, 'menu_clean');

    CreateMenuSeparatorSection(mnuFile,itmFileIDEStart,'itmFileIDEStart');
    ParentMI:=itmFileIDEStart;

    CreateMenuItem(ParentMI,itmFileRestart,'itmFileRestart',lisMenuRestart, 'refresh');
    CreateMenuItem(ParentMI,itmFileQuit,'itmFileQuit',lisMenuQuit, 'menu_exit');
  end;
end;

procedure TMainIDEBase.SetupEditMenu;
var
  ParentMI: TIDEMenuSection;
  SubParentMI: TIDEMenuSection;
  SubSubParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuEdit,itmEditReUndo,'itmEditReUndo');
    ParentMI:=itmEditReUndo;
    CreateMenuItem(ParentMI,itmEditUndo,'itmEditUndo',lisMenuUndo,'menu_undo');
    CreateMenuItem(ParentMI,itmEditRedo,'itmEditRedo',lisMenuRedo,'menu_redo');

    CreateMenuSeparatorSection(mnuEdit,itmEditClipboard,'itmEditClipboard');
    ParentMI:=itmEditClipboard;

    CreateMenuItem(ParentMI,itmEditCut,'itmEditCut',lisMenuCut,'cut');
    CreateMenuItem(ParentMI,itmEditCopy,'itmEditCopy',lisMenuCopy,'copy');
    CreateMenuItem(ParentMI,itmEditPaste,'itmEditPaste',lisMenuPaste,'paste');

    CreateMenuSeparatorSection(mnuEdit,itmEditBlockIndentation,'itmEditBlockIndentation');
    ParentMI:=itmEditBlockIndentation;

    CreateMenuItem(ParentMI,itmEditIndentBlock,'itmEditIndentBlock',lisMenuIndentSelection,'menu_indent');
    CreateMenuItem(ParentMI,itmEditUnindentBlock,'itmEditUnindentBlock',lisMenuUnindentSelection,'menu_unindent');
    CreateMenuItem(ParentMI,itmEditEncloseBlock,'itmEditEncloseBlock',lisMenuEncloseSelection);
    CreateMenuItem(ParentMI,itmEditCommentBlock,'itmEditCommentBlock',lisMenuCommentSelection, 'menu_comment');
    CreateMenuItem(ParentMI,itmEditUncommentBlock,'itmEditUncommentBlock',lisMenuUncommentSelection, 'menu_uncomment');
    CreateMenuItem(ParentMI,itmEditConditionalBlock,'itmEditConditionalBlock',lisMenuConditionalSelection);
    CreateMenuItem(ParentMI,itmEditSortBlock,'itmEditSortBlock',lisMenuSortSelection, 'menu_edit_sort');

    CreateMenuSeparatorSection(mnuEdit,itmEditBlockCharConversion,'itmEditBlockCharConversion');
    ParentMI:=itmEditBlockCharConversion;

    CreateMenuItem(ParentMI,itmEditUpperCaseBlock,'itmEditUpperCaseBlock',lisMenuUpperCaseSelection, 'menu_edit_uppercase');
    CreateMenuItem(ParentMI,itmEditLowerCaseBlock,'itmEditLowerCaseBlock',lisMenuLowerCaseSelection, 'menu_edit_lowercase');
    CreateMenuItem(ParentMI,itmEditTabsToSpacesBlock,'itmEditTabsToSpacesBlock',lisMenuTabsToSpacesSelection);
    CreateMenuItem(ParentMI,itmEditSelectionBreakLines,'itmEditSelectionBreakLines',lisMenuBeakLinesInSelection);

    CreateMenuSubSection(mnuEdit,itmEditSelect,'itmEditSelect',lisMenuSelect);
    begin
      // select sub menu items
      SubParentMI:=itmEditSelect;
      CreateMenuItem(SubParentMI,itmEditSelectAll,'itmEditSelectAll',lisMenuSelectAll, 'menu_select_all');
      CreateMenuItem(SubParentMI,itmEditSelectToBrace,'itmEditSelectToBrace',lisMenuSelectToBrace);
      CreateMenuItem(SubParentMI,itmEditSelectCodeBlock,'itmEditSelectCodeBlock',lisMenuSelectCodeBlock);
      CreateMenuItem(SubParentMI,itmEditSelectLine,'itmEditSelectLine',lisMenuSelectLine);
      CreateMenuItem(SubParentMI,itmEditSelectParagraph,'itmEditSelectParagraph',lisMenuSelectParagraph);
    end;

    CreateMenuSeparatorSection(mnuEdit,itmEditInsertions,'itmEditInsertions');
    ParentMI:=itmEditInsertions;

    CreateMenuItem(ParentMI,itmEditInsertCharacter,'itmEditInsertCharacter',lisMenuInsertCharacter);
    CreateMenuSubSection(ParentMI,itmEditInsertText,'itmEditInsertText',lisMenuInsertText);
     begin
      // insert text sub menu items
      SubParentMI:=itmEditInsertText;
      CreateMenuSubSection(SubParentMI,itmEditInsertCVSKeyWord,'itmEditInsertCVSKeyWord',lisMenuInsertCVSKeyword);
      begin
        // insert CVS keyword sub menu items
        SubSubParentMI:=itmEditInsertCVSKeyWord;
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSAuthor,'itmEditInsertCVSAuthor','Author');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSDate,'itmEditInsertCVSDate','Date');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSHeader,'itmEditInsertCVSHeader','Header');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSID,'itmEditInsertCVSID','ID');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSLog,'itmEditInsertCVSLog','Log');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSName,'itmEditInsertCVSName','Name');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSRevision,'itmEditInsertCVSRevision','Revision');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSSource,'itmEditInsertCVSSource','Source');
      end;

      CreateMenuSubSection(SubParentMI,itmEditInsertGeneral,'itmEditInsertGeneral',lisMenuInsertGeneral);
      begin
        // insert general text sub menu items
        SubSubParentMI:=itmEditInsertGeneral;
        CreateMenuItem(SubSubParentMI,itmEditInsertGPLNotice,'itmEditInsertGPLNotice',lisMenuInsertGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertLGPLNotice,'itmEditInsertLGPLNotice',lisMenuInsertLGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertModifiedLGPLNotice,'itmEditInsertModifiedLGPLNotice',lisMenuInsertModifiedLGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertUsername,'itmEditInsertUsername',lisMenuInsertUsername);
        CreateMenuItem(SubSubParentMI,itmEditInsertDateTime,'itmEditInsertDateTime',lisMenuInsertDateTime);
        CreateMenuItem(SubSubParentMI,itmEditInsertChangeLogEntry,'itmEditInsertChangeLogEntry',lisMenuInsertChangeLogEntry);
      end;
    end;

    CreateMenuSeparatorSection(mnuEdit,itmEditMenuCodeTools,'itmEditMenuCodeTools');
    ParentMI:=itmEditMenuCodeTools;

    CreateMenuItem(ParentMI,itmEditCompleteCode,'itmEditCompleteCode',lisMenuCompleteCode);
    CreateMenuItem(ParentMI,itmEditExtractProc,'itmEditExtractProc',lisMenuExtractProc);
  end;
end;

procedure TMainIDEBase.SetupSearchMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuSearch,itmSearchFindReplace,'itmSearchFindReplace');
    ParentMI:=itmSearchFindReplace;

    CreateMenuItem(ParentMI, itmSearchFind, 'itmSearchFind', lisMenuFind2, 'menu_search_find');
    CreateMenuItem(ParentMI,itmSearchFindNext,'itmSearchFindNext',lisMenuFindNext, 'menu_search_find_next');
    CreateMenuItem(ParentMI,itmSearchFindPrevious,'itmSearchFindPrevious',lisMenuFindPrevious, 'menu_search_find_previous');
    CreateMenuItem(ParentMI,itmSearchFindInFiles,'itmSearchFindInFiles',lisMenuFindInFiles, 'menu_search_files');
    CreateMenuItem(ParentMI, itmSearchReplace, 'itmSearchReplace',
      lisMenuReplace2, 'menu_search_replace');
    CreateMenuItem(ParentMI,itmIncrementalFind,'itmIncrementalFind',lisMenuIncrementalFind);
    CreateMenuItem(ParentMI,itmGotoLine,'itmGotoLine',lisMenuGotoLine, 'menu_goto_line');

    CreateMenuSeparatorSection(mnuSearch,itmJumpings,'itmJumpings');
    ParentMI:=itmJumpings;

    CreateMenuItem(ParentMI,itmJumpBack,'itmJumpBack',lisMenuJumpBack);
    CreateMenuItem(ParentMI,itmJumpForward,'itmJumpForward',lisMenuJumpForward);
    CreateMenuItem(ParentMI,itmAddJumpPoint,'itmAddJumpPoint',lisMenuAddJumpPointToHistory);
    CreateMenuItem(ParentMI,itmJumpHistory,'itmJumpHistory',lisMenuViewJumpHistory);
    CreateMenuItem(ParentMI,itmJumpToNextError,'itmJumpToNextError',lisMenuJumpToNextError);
    CreateMenuItem(ParentMI,itmJumpToPrevError,'itmJumpToPrevError',lisMenuJumpToPrevError);

    CreateMenuSeparatorSection(mnuSearch,itmBookmarks,'itmBookmarks');
    ParentMI:=itmBookmarks;

    CreateMenuItem(ParentMI,itmSetFreeBookmark,'itmSetFreeBookmark',lisMenuSetFreeBookmark);
    CreateMenuItem(ParentMI,itmJumpToNextBookmark,'itmJumpToNextBookmark',lisMenuJumpToNextBookmark);
    CreateMenuItem(ParentMI,itmJumpToPrevBookmark,'itmJumpToPrevBookmark',lisMenuJumpToPrevBookmark);

    CreateMenuSeparatorSection(mnuSearch,itmCodeToolSearches,'itmCodeToolSearches');
    ParentMI:=itmCodeToolSearches;

    CreateMenuItem(ParentMI,itmFindBlockOtherEnd,'itmFindBlockOtherEnd',lisMenuFindBlockOtherEndOfCodeBlock);
    CreateMenuItem(ParentMI,itmFindBlockStart,'itmFindBlockStart',lisMenuFindCodeBlockStart);
    CreateMenuItem(ParentMI,itmFindDeclaration,'itmFindDeclaration',lisMenuFindDeclarationAtCursor);
    CreateMenuItem(ParentMI,itmOpenFileAtCursor,'itmOpenFileAtCursor',lisMenuOpenFilenameAtCursor,'menu_search_openfile_atcursor');
    CreateMenuItem(ParentMI,itmGotoIncludeDirective,'itmGotoIncludeDirective',lisMenuGotoIncludeDirective);
    CreateMenuItem(ParentMI,itmSearchFindIdentifierRefs,'itmSearchFindIdentifierRefs',lisMenuFindIdentifierRefs);
    CreateMenuItem(ParentMI,itmSearchRenameIdentifier,'itmSearchRenameIdentifier',lisMenuRenameIdentifier);
    CreateMenuItem(ParentMI,itmSearchProcedureList,'itmSearchProcedureList',srkmecProcedureList);
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
    CreateMenuItem(ParentMI,itmViewCodeExplorer,'itmViewCodeExplorer',lisMenuViewCodeExplorer, 'menu_view_code_explorer');
    CreateMenuItem(ParentMI,itmViewLazDoc,'itmViewLazDoc',lisMenuLazDoc);   //DBlaszijk 5-sep-05
    CreateMenuItem(ParentMI,itmViewCodeBrowser,'itmViewCodeBrowser',lisMenuViewCodeBrowser);
    CreateMenuItem(ParentMI,itmViewComponents,'itmViewComponents',lisMenuViewComponents);

    CreateMenuSeparatorSection(mnuView,itmViewUnitWindows,'itmViewUnitWindows');
    ParentMI:=itmViewUnitWindows;

    CreateMenuItem(ParentMI,itmViewUnits,'itmViewUnits',lisMenuViewUnits, 'menu_view_units');
    CreateMenuItem(ParentMI,itmViewForms,'itmViewForms',lisMenuViewForms, 'menu_view_forms');
    CreateMenuItem(ParentMI,itmViewUnitDependencies,'itmViewUnitDependencies',lisMenuViewUnitDependencies);
    CreateMenuItem(ParentMI,itmViewUnitInfo,'itmViewUnitInfo',lisMenuViewUnitInfo, 'menu_view_unit_info');
    CreateMenuItem(ParentMI,itmViewToggleFormUnit,'itmViewToggleFormUnit',lisMenuViewToggleFormUnit, 'menu_view_toggle_form_unit');

    CreateMenuSeparatorSection(mnuView,itmViewSecondaryWindows,'itmViewSecondaryWindows');
    ParentMI:=itmViewSecondaryWindows;

    CreateMenuItem(ParentMI,itmViewMessage,'itmViewMessage',lisMenuViewMessages);
    CreateMenuItem(ParentMI,itmViewSearchResults,'itmViewSearchResults',lisMenuViewSearchResults);
    CreateMenuItem(ParentMI,itmViewAnchorEditor,'itmViewAnchorEditor',lisMenuViewAnchorEditor,'menu_view_anchor_editor');
    CreateMenuItem(ParentMI,itmViewComponentPalette,'itmViewComponentPalette',lisMenuViewComponentPalette, '', true, EnvironmentOptions.ComponentPaletteVisible);
    CreateMenuItem(ParentMI,itmViewIDESpeedButtons,'itmViewIDESpeedButtons',lisMenuViewIDESpeedButtons, '', true, EnvironmentOptions.IDESpeedButtonsVisible);
    CreateMenuSubSection(ParentMI,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'debugger');
    begin
      CreateMenuItem(itmViewDebugWindows,itmViewWatches,'itmViewWatches',lisMenuViewWatches,'debugger_watches');
      CreateMenuItem(itmViewDebugWindows,itmViewBreakPoints,'itmViewBreakPoints',lisMenuViewBreakPoints,'debugger_breakpoints');
      CreateMenuItem(itmViewDebugWindows,itmViewLocals,'itmViewLocals',lisMenuViewLocalVariables,'');
      CreateMenuItem(itmViewDebugWindows,itmViewCallStack,'itmViewCallStack',lisMenuViewCallStack,'debugger_call_stack');
      CreateMenuItem(itmViewDebugWindows,itmViewDebugOutput,'itmViewDebugOutput',lisMenuViewDebugOutput,'debugger_output');
    end;
    CreateMenuSubSection(ParentMI, itmViewIDEInternalsWindows, 'itmViewIDEInternalsWindows', lisMenuIDEInternals, '');
    begin
      CreateMenuItem(itmViewIDEInternalsWindows, itmViewPackageLinks, 'itmViewPackageLinks', lisMenuPackageLinks, '');
    end;
  end;
end;

procedure TMainIDEBase.SetupProjectMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuProject,itmProjectNewSection,'itmProjectNewSection');
    ParentMI:=itmProjectNewSection;

    CreateMenuItem(ParentMI,itmProjectNew,'itmProjectNew',lisMenuNewProject, 'menu_project_new');
    CreateMenuItem(ParentMI,itmProjectNewFromFile,'itmProjectNewFromFile',lisMenuNewProjectFromFile);

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
    {$ifdef EnableNewDialogs}
    CreateMenuItem(ParentMI,itmProjectOptions_new,'itmProjectOptions_new',lisMenuProjectOptions+' (new)','menu_project_options');
    {$endif}
    CreateMenuItem(ParentMI,itmProjectCompilerOptions,'itmProjectCompilerOptions',lisMenuCompilerOptions,'menu_compiler_options');
    {$ifdef EnableNewDialogs}
    CreateMenuItem(ParentMI,itmProjectCompilerOptions_new,'itmProjectCompilerOptions_new',lisMenuCompilerOptions+' (new)','menu_compiler_options');
    {$endif}
    CreateMenuItem(ParentMI,itmProjectViewToDos,'itmProjectViewToDos',lisMenuViewProjectTodos, 'menu_project_todo');

    CreateMenuSeparatorSection(mnuProject,itmProjectAddRemoveSection,'itmProjectAddRemoveSection');
    ParentMI:=itmProjectAddRemoveSection;

    CreateMenuItem(ParentMI,itmProjectAddTo,'itmProjectAddTo',lisMenuAddToProject, 'menu_project_add');
    CreateMenuItem(ParentMI,itmProjectRemoveFrom,'itmProjectRemoveFrom',lisMenuRemoveFromProject, 'menu_project_remove');
    CreateMenuItem(ParentMI,itmProjectViewSource,'itmProjectViewSource',lisMenuViewSource, 'menu_project_viewsource');

  end;
end;

procedure TMainIDEBase.SetupRunMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuRun,itmRunBuilding,'itmRunBuilding');
    ParentMI:=itmRunBuilding;

    CreateMenuItem(ParentMI,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build');
    CreateMenuItem(ParentMI,itmRunMenuBuildAll,'itmRunMenuBuildAll',lisMenuBuildAll,'menu_build_all');
    CreateMenuItem(ParentMI,itmRunMenuQuickCompile,'itmRunMenuQuickCompile',lisMenuQuickCompile,'menu_quick_compile');
    CreateMenuItem(ParentMI,itmRunMenuAbortBuild,'itmRunMenuAbortBuild',lisMenuAbortBuild,'menu_abort_build');

    CreateMenuSeparatorSection(mnuRun,itmRunnning,'itmRunnning');
    ParentMI:=itmRunnning;

    CreateMenuItem(ParentMI,itmRunMenuRun,'itmRunMenuRun',lisMenuProjectRun,'menu_run');
    CreateMenuItem(ParentMI,itmRunMenuPause,'itmRunMenuPause',lisMenuPause,'menu_pause');
    CreateMenuItem(ParentMI,itmRunMenuStepInto,'itmRunMenuStepInto',lisMenuStepInto,'menu_stepinto');
    CreateMenuItem(ParentMI,itmRunMenuStepOver,'itmRunMenuStepOver',lisMenuStepOver,'menu_stepover');
    CreateMenuItem(ParentMI,itmRunMenuRunToCursor,'itmRunMenuRunToCursor',lisMenuRunToCursor,'menu_run_cursor');
    CreateMenuItem(ParentMI,itmRunMenuStop,'itmRunMenuStop',lisMenuStop,'menu_stop');
    CreateMenuItem(ParentMI,itmRunMenuRunParameters,'itmRunMenuRunParameters',lisMenuRunParameters, 'menu_run_parameters');
    CreateMenuItem(ParentMI,itmRunMenuResetDebugger,'itmRunMenuResetDebugger',lisMenuResetDebugger, 'menu_reset_debugger');

    CreateMenuSeparatorSection(mnuRun,itmRunBuildingFile,'itmRunBuildingFile');
    ParentMI:=itmRunBuildingFile;

    CreateMenuItem(ParentMI,itmRunMenuBuildFile,'itmRunMenuBuildFile',lisMenuBuildFile, 'menu_build_file');
    CreateMenuItem(ParentMI,itmRunMenuRunFile,'itmRunMenuRunFile',lisMenuRunFile,'menu_run_file');
    CreateMenuItem(ParentMI,itmRunMenuConfigBuildFile,'itmRunMenuConfigBuildFile',lisMenuConfigBuildFile);

    CreateMenuSeparatorSection(mnuRun,itmRunDebugging,'itmRunDebugging');
    ParentMI:=itmRunDebugging;

    CreateMenuItem(ParentMI,itmRunMenuInspect,'itmRunMenuInspect',lisMenuInspect, '', False);
    CreateMenuItem(ParentMI,itmRunMenuEvaluate,'itmRunMenuEvaluate',lisMenuEvaluate, 'debugger_modify', False);
    CreateMenuItem(ParentMI,itmRunMenuAddWatch,'itmRunMenuAddWatch',lisMenuAddWatch, '', False);
    CreateMenuSubSection(ParentMI,itmRunMenuAddBreakpoint,'itmRunMenuAddBreakpoint',lisMenuAddBreakpoint, '');
    CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBPSource,'itmRunMenuAdddBPSource',lisMenuAddBPSource, '', False);
  end;
end;

procedure TMainIDEBase.SetupComponentsMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuComponents,itmPkgOpening,'itmPkgOpening');
    ParentMI:=itmPkgOpening;

    CreateMenuItem(ParentMI,itmPkgOpenPackage,'itmPkgOpenPackage',lisMenuOpenPackage,'pkg_installed');
    CreateMenuItem(ParentMI,itmPkgOpenPackageFile,'itmPkgOpenPackageFile',lisMenuOpenPackageFile,'pkg_open');
    CreateMenuItem(ParentMI,itmPkgOpenPackageOfCurUnit,'itmPkgOpenPackageOfCurUnit',lisMenuOpenPackageOfCurUnit);
    CreateMenuSubSection(ParentMI,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg);

    CreateMenuSeparatorSection(mnuComponents,itmPkgUnits,'itmPkgUnits');
    ParentMI:=itmPkgUnits;

    CreateMenuItem(ParentMI,itmPkgAddCurUnitToPkg,'itmPkgAddCurUnitToPkg',lisMenuAddCurUnitToPkg,'pkg_add');

    CreateMenuSeparatorSection(mnuComponents,itmPkgGraphSection,'itmPkgGraphSection');
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
    CreateMenuSeparatorSection(mnuTools,itmCustomTools,'itmCustomTools');
    ParentMI:=itmCustomTools;

    CreateMenuItem(ParentMI,itmToolConfigure,'itmToolConfigure',lisMenuSettings);

    CreateMenuSeparatorSection(mnuTools,itmCodeToolChecks,'itmCodeToolChecks');
    ParentMI:=itmCodeToolChecks;

    CreateMenuItem(ParentMI,itmToolSyntaxCheck,'itmToolSyntaxCheck',lisMenuQuickSyntaxCheck, 'menu_tool_syntax_check');
    CreateMenuItem(ParentMI,itmToolGuessUnclosedBlock,'itmToolGuessUnclosedBlock',lisMenuGuessUnclosedBlock);
    CreateMenuItem(ParentMI,itmToolGuessMisplacedIFDEF,'itmToolGuessMisplacedIFDEF',lisMenuGuessMisplacedIFDEF);

    CreateMenuSeparatorSection(mnuTools,itmSecondaryTools,'itmSecondaryTools');
    ParentMI:=itmSecondaryTools;

    CreateMenuItem(ParentMI,itmToolMakeResourceString,'itmToolMakeResourceString',lisMenuMakeResourceString, 'menu_tool_make_resourcestring');
    CreateMenuItem(ParentMI,itmToolDiff,'itmToolDiff',lisMenuDiff, 'menu_tool_diff');

    CreateMenuSeparatorSection(mnuTools,itmDelphiConversion,'itmDelphiConversion');
    ParentMI:=itmDelphiConversion;

    CreateMenuItem(ParentMI,itmToolCheckLFM,'itmToolCheckLFM',lisMenuCheckLFM, 'menu_tool_check_lfm');
    CreateMenuItem(ParentMI,itmToolConvertDelphiUnit,'itmToolConvertDelphiUnit',lisMenuConvertDelphiUnit);
    CreateMenuItem(ParentMI,itmToolConvertDelphiProject,'itmToolConvertDelphiProject',lisMenuConvertDelphiProject);
    CreateMenuItem(ParentMI,itmToolConvertDelphiPackage,'itmToolConvertDelphiPackage',lisMenuConvertDelphiPackage);
    CreateMenuItem(ParentMI,itmToolConvertDFMtoLFM,'itmToolConvertDFMtoLFM',lisMenuConvertDFMtoLFM,'menu_tool_dfm_to_lfm');

    CreateMenuSeparatorSection(mnuTools,itmBuildingLazarus,'itmBuildingLazarus');
    ParentMI:=itmBuildingLazarus;

    CreateMenuItem(ParentMI,itmToolBuildLazarus,'itmToolBuildLazarus',lisMenuBuildLazarus,'menu_build_lazarus');
    CreateMenuItem(ParentMI,itmToolConfigureBuildLazarus,'itmToolConfigureBuildLazarus',lisMenuConfigureBuildLazarus, 'menu_configure_build_lazarus');
  end;
end;

procedure TMainIDEBase.SetupEnvironmentMenu;
var
  ParentMI: TIDEMenuSection;
begin
  with MainIDEBar do begin
    CreateMenuSeparatorSection(mnuEnvironment,itmOptionsDialogs,'itmOptionsDialogs');
    ParentMI:=itmOptionsDialogs;

    CreateMenuItem(ParentMI,itmEnvGeneralOptions,'itmEnvGeneralOptions',
                   lisMenuGeneralOptions,'menu_environment_options');
    {$ifdef EnableNewDialogs}
    CreateMenuItem(ParentMI,itmEnvGeneralOptions_new,'itmEnvGeneralOptions_new',
                   lisMenuGeneralOptions+' (new)','menu_environment_options');
    {$endif}
    CreateMenuItem(ParentMI,itmEnvEditorOptions,'itmEnvEditorOptions',
                   lisMenuEditorOptions,'menu_editor_options');
    {$ifdef EnableNewDialogs}
    CreateMenuItem(ParentMI,itmEnvEditorOptions_new,'itmEnvEditorOptions_new',
                   lisMenuEditorOptions+' (new)','menu_editor_options');
    {$endif}
    CreateMenuItem(ParentMI,itmEnvCodeTemplates,'itmEnvCodeTemplates',
                   lisMenuEditCodeTemplates,'');
    CreateMenuItem(ParentMI,itmEnvDebuggerOptions,'itmEnvDebuggerOptions',
                   lisMenDebuggerOptions,'debugger_options');
    CreateMenuItem(ParentMI,itmEnvCodeToolsOptions,'itmEnvCodeToolsOptions',
                   lisMenuCodeToolsOptions,'menu_codetoolsoptions');
    CreateMenuItem(ParentMI,itmEnvCodeToolsDefinesEditor,
                   'itmEnvCodeToolsDefinesEditor',lisMenuCodeToolsDefinesEditor,
                   'menu_codetoolsdefineseditor');

    CreateMenuSeparatorSection(mnuEnvironment,itmIDECacheSection,'itmIDECacheSection');
    ParentMI:=itmIDECacheSection;

    CreateMenuItem(ParentMI,itmEnvRescanFPCSrcDir,'itmEnvRescanFPCSrcDir',
                   lisMenuRescanFPCSourceDirectory);
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
    CreateMenuItem(ParentMI,itmHelpConfigureHelp,'itmHelpConfigureHelp',
                   lisMenuConfigureHelp, 'menu_configure_help');

    CreateMenuSeparatorSection(mnuHelp,itmInfoHelps,'itmInfoHelps');
    ParentMI:=itmInfoHelps;

    CreateMenuItem(ParentMI,itmHelpAboutLazarus,'itmHelpAboutLazarus',
                   lisAboutLazarus, 'menu_information');

    CreateMenuSeparatorSection(mnuHelp,itmHelpTools,'itmHelpTools');
    ParentMI:=itmHelpTools;

    CreateMenuItem(ParentMI,itmHelpCreateLazDoc,'itmHelpCreateLazDoc',
                   lisMenuCreateLazDocFiles);
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
    itmEditIndentBlock.Command:=GetCommand(ecBlockIndent);
    itmEditUnindentBlock.Command:=GetCommand(ecBlockUnindent);
    itmEditEncloseBlock.Command:=GetCommand(ecSelectionEnclose);
    itmEditUpperCaseBlock.Command:=GetCommand(ecSelectionUpperCase);
    itmEditLowerCaseBlock.Command:=GetCommand(ecSelectionLowerCase);
    itmEditTabsToSpacesBlock.Command:=GetCommand(ecSelectionTabs2Spaces);
    itmEditCommentBlock.Command:=GetCommand(ecSelectionComment);
    itmEditUncommentBlock.Command:=GetCommand(ecSelectionUncomment);
    itmEditConditionalBlock.Command:=GetCommand(ecSelectionConditional);
    itmEditSortBlock.Command:=GetCommand(ecSelectionSort);
    itmEditSelectionBreakLines.Command:=GetCommand(ecSelectionBreakLines);
    itmEditSelectAll.Command:=GetCommand(ecSelectAll);
    itmEditSelectToBrace.Command:=GetCommand(ecSelectToBrace);
    itmEditSelectCodeBlock.Command:=GetCommand(ecSelectCodeBlock);
    itmEditSelectLine.Command:=GetCommand(ecSelectLine);
    itmEditSelectParagraph.Command:=GetCommand(ecSelectParagraph);
    itmEditCompleteCode.Command:=GetCommand(ecCompleteCode);
    itmEditExtractProc.Command:=GetCommand(ecExtractProc);

    itmEditInsertCVSAuthor.Command:=GetCommand(ecInsertCVSAuthor);
    itmEditInsertCVSDate.Command:=GetCommand(ecInsertCVSDate);
    itmEditInsertCVSHeader.Command:=GetCommand(ecInsertCVSHeader);
    itmEditInsertCVSID.Command:=GetCommand(ecInsertCVSID);
    itmEditInsertCVSLog.Command:=GetCommand(ecInsertCVSLog);
    itmEditInsertCVSName.Command:=GetCommand(ecInsertCVSName);
    itmEditInsertCVSRevision.Command:=GetCommand(ecInsertCVSRevision);
    itmEditInsertCVSSource.Command:=GetCommand(ecInsertCVSSource);

    itmEditInsertGPLNotice.Command:=GetCommand(ecInsertGPLNotice);
    itmEditInsertLGPLNotice.Command:=GetCommand(ecInsertLGPLNotice);
    itmEditInsertModifiedLGPLNotice.Command:=GetCommand(ecInsertModifiedLGPLNotice);
    itmEditInsertUsername.Command:=GetCommand(ecInsertUserName);
    itmEditInsertDateTime.Command:=GetCommand(ecInsertDateTime);
    itmEditInsertChangeLogEntry.Command:=GetCommand(ecInsertChangeLogEntry);

    // search menu
    itmSearchFind.Command:=GetCommand(ecFind);
    itmSearchFindNext.Command:=GetCommand(ecFindNext);
    itmSearchFindPrevious.Command:=GetCommand(ecFindPrevious);
    itmSearchFindInFiles.Command:=GetCommand(ecFindInFiles);
    itmSearchFindIdentifierRefs.Command:=GetCommand(ecFindIdentifierRefs);
    itmSearchReplace.Command:=GetCommand(ecReplace);
    itmSearchRenameIdentifier.Command:=GetCommand(ecRenameIdentifier);
    itmIncrementalFind.Command:=GetCommand(ecIncrementalFind);
    itmGotoLine.Command:=GetCommand(ecGotoLineNumber);
    itmJumpBack.Command:=GetCommand(ecJumpBack);
    itmJumpForward.Command:=GetCommand(ecJumpForward);
    itmAddJumpPoint.Command:=GetCommand(ecAddJumpPoint);
    itmJumpHistory.Command:=GetCommand(ecViewJumpHistory);
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
    itmViewUnits.Command:=GetCommand(ecViewUnits);
    itmViewCodeExplorer.Command:=GetCommand(ecToggleCodeExpl);
    itmViewLazDoc.Command:=GetCommand(ecToggleLazDoc);
    itmViewCodeBrowser.Command:=GetCommand(ecToggleCodeBrowser);
    itmViewComponents.Command:=GetCommand(ecViewComponents);
    itmViewUnitDependencies.Command:=GetCommand(ecViewUnitDependencies);
    itmViewUnitInfo.Command:=GetCommand(ecViewUnitInfo);
    itmViewForms.Command:=GetCommand(ecViewForms);
    itmViewToggleFormUnit.Command:=GetCommand(ecToggleFormUnit);
    itmViewMessage.Command:=GetCommand(ecToggleMessages);
    itmViewSearchResults.Command:=GetCommand(ecToggleSearchResults);
    itmViewAnchorEditor.Command:=GetCommand(ecViewAnchorEditor);
    itmViewComponentPalette.Command:=GetCommand(ecToggleCompPalette);
    itmViewIDESpeedButtons.Command:=GetCommand(ecToggleIDESpeedBtns);
    //itmViewPackageLinks.Command:=GetCommand(ec?);

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
    itmProjectCompilerOptions.Command:=GetCommand(ecCompilerOptions);
    itmProjectAddTo.Command:=GetCommand(ecAddCurUnitToProj);
    itmProjectRemoveFrom.Command:=GetCommand(ecRemoveFromProj);
    itmProjectViewSource.Command:=GetCommand(ecViewProjectSource);

    // run menu
    itmRunMenuBuild.Command:=GetCommand(ecBuild);
    itmRunMenuBuildAll.Command:=GetCommand(ecBuildAll);
    itmRunMenuQuickCompile.Command:=GetCommand(ecQuickCompile);
    itmRunMenuAbortBuild.Command:=GetCommand(ecAbortBuild);
    itmRunMenuRun.Command:=GetCommand(ecRun);
    itmRunMenuPause.Command:=GetCommand(ecPause);
    itmRunMenuStepInto.Command:=GetCommand(ecStepInto);
    itmRunMenuStepOver.Command:=GetCommand(ecStepOver);
    itmRunMenuRunToCursor.Command:=GetCommand(ecRunToCursor);
    itmRunMenuStop.Command:=GetCommand(ecStopProgram);
    itmRunMenuResetDebugger.Command:=GetCommand(ecResetDebugger);
    itmRunMenuRunParameters.Command:=GetCommand(ecRunParameters);
    itmRunMenuBuildFile.Command:=GetCommand(ecBuildFile);
    itmRunMenuRunFile.Command:=GetCommand(ecRunFile);
    itmRunMenuConfigBuildFile.Command:=GetCommand(ecConfigBuildFile);

    // components menu
    itmPkgOpenPackage.Command:=GetCommand(ecOpenPackage);
    itmPkgOpenPackageFile.Command:=GetCommand(ecOpenPackageFile);
    itmPkgOpenPackageOfCurUnit.Command:=GetCommand(ecOpenPackageOfCurUnit);
    itmPkgAddCurUnitToPkg.Command:=GetCommand(ecAddCurUnitToPkg);
    itmPkgPkgGraph.Command:=GetCommand(ecPackageGraph);
    itmPkgEditInstallPkgs.Command:=GetCommand(ecEditInstallPkgs);
    {$IFDEF CustomIDEComps}
    itmCompsConfigCustomComps.Command:=GetCommand(ecConfigCustomComps);
    {$ENDIF}

    // tools menu
    itmToolConfigure.Command:=GetCommand(ecExtToolSettings);
    itmToolSyntaxCheck.Command:=GetCommand(ecSyntaxCheck);
    itmToolGuessUnclosedBlock.Command:=GetCommand(ecGuessUnclosedBlock);
    itmToolGuessMisplacedIFDEF.Command:=GetCommand(ecGuessMisplacedIFDEF);
    itmToolMakeResourceString.Command:=GetCommand(ecMakeResourceString);
    itmToolDiff.Command:=GetCommand(ecDiff);
    itmToolConvertDFMtoLFM.Command:=GetCommand(ecConvertDFM2LFM);
    itmToolCheckLFM.Command:=GetCommand(ecCheckLFM);
    itmToolConvertDelphiUnit.Command:=GetCommand(ecConvertDelphiUnit);
    itmToolConvertDelphiProject.Command:=GetCommand(ecConvertDelphiProject);
    itmToolConvertDelphiPackage.Command:=GetCommand(ecConvertDelphiPackage);
    itmToolBuildLazarus.Command:=GetCommand(ecBuildLazarus);
    itmToolConfigureBuildLazarus.Command:=GetCommand(ecConfigBuildLazarus);

    // environment menu
    itmEnvGeneralOptions.Command:=GetCommand(ecEnvironmentOptions);
    itmEnvEditorOptions.Command:=GetCommand(ecEditorOptions);
    itmEnvCodeTemplates.Command:=GetCommand(ecEditCodeTemplates);
    itmEnvCodeToolsOptions.Command:=GetCommand(ecCodeToolsOptions);
    itmEnvCodeToolsDefinesEditor.Command:=GetCommand(ecCodeToolsDefinesEd);
    itmEnvRescanFPCSrcDir.Command:=GetCommand(ecRescanFPCSrcDir);

    // help menu
    itmHelpAboutLazarus.Command:=GetCommand(ecAboutLazarus);
    itmHelpOnlineHelp.Command:=GetCommand(ecOnlineHelp);
    itmHelpReportingBug.Command:=GetCommand(ecReportingBug);
    itmHelpConfigureHelp.Command:=GetCommand(ecConfigureHelp);
  end;
end;

function TMainIDEBase.GetToolStatus: TIDEToolStatus;
begin
  Result:=FToolStatus;
end;

function TMainIDEBase.DoOpenMacroFile(Sender: TObject; const AFilename: string
  ): TModalResult;
begin
  Result:=DoOpenEditorFile(AFilename,-1,
                  [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros]);
end;

procedure TMainIDEBase.UpdateWindowsMenu;
var
  WindowsList: TFPList;
  i: Integer;
  CurMenuItem: TIDEMenuItem;
  AForm: TForm;
begin
  WindowsList:=TFPList.Create;
  // add typical IDE windows at the start of the list
  if (SourceNotebook<>nil) and (SourceNotebook.Visible) then
    WindowsList.Add(SourceNotebook);
  if (ObjectInspector1<>nil) and (ObjectInspector1.Visible) then
    WindowsList.Add(ObjectInspector1);
  // add special IDE windows
  for i:=0 to Screen.FormCount-1 do begin
    AForm:=Screen.Forms[i];
    if (AForm.Parent=nil) and (AForm<>MainIDEBar) and (AForm<>SplashForm)
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
      CurMenuItem:=RegisterIDEMenuCommand(mnuWindows.GetPath,
                                          'Window'+IntToStr(i),'');
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

function TMainIDEBase.DoJumpToCodePosition(
  ActiveSrcEdit: TSourceEditorInterface; ActiveUnitInfo: TUnitInfo;
  NewSource: TCodeBuffer; NewX, NewY, NewTopLine: integer; AddJumpPoint: boolean
  ): TModalResult;
var
  SrcEdit: TSourceEditor;
begin
  if ActiveSrcEdit=nil then
    SrcEdit:=nil
  else
    SrcEdit:=ActiveSrcEdit as TSourceEditor;
  Result:=DoJumpToCodePos(SrcEdit as TSourceEditor, ActiveUnitInfo,
                          NewSource, NewX, NewY, NewTopLine, AddJumpPoint);
end;

procedure TMainIDEBase.FindInFilesPerDialog(AProject: TProject);
begin
  SourceNotebook.FindInFilesPerDialog(AProject);
end;

procedure TMainIDEBase.FindInFiles(AProject: TProject; const FindText: string);
begin
  SourceNotebook.FindInFiles(AProject, FindText);
end;

end.


