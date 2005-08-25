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
  LazConf, LazarusIDEStrConsts, SrcEditorIntf,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler, LazIDEIntf,
  ComponentReg,
  TransferMacros, ObjectInspector, PropEdits, OutputFilter, IDEDefs, MsgView,
  EnvironmentOpts, EditorOptions, CompilerOptions, KeyMapping, IDEProcs,
  Debugger, IDEOptionDefs, CodeToolsDefines, Splash, Designer,
  UnitEditor, MainBar, MainIntf;

type
  { TMainIDEBase }

  TMainIDEBase = class(TMainIDEInterface)
  private
    FToolStatus: TIDEToolStatus;
  protected
    CurrentParsedCompilerOption: TParsedCompilerOptions;
    TheCompiler: TCompiler;
    TheOutputFilter: TOutputFilter;
    OwningComponent: TComponent;

    function CreateMenuSeparator : TMenuItem;
    procedure CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
                             const MenuItemName, MenuItemCaption: String);
    procedure CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
                             const MenuItemName, MenuItemCaption: String;
                             const bmpName: String);
    procedure CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
                             const MenuItemName, MenuItemCaption: String;
                             const bmpName: String; mnuEnabled: Boolean);
    procedure CreateMainMenuItem(MainMenu: TMainMenu; var MenuItem: TMenuItem;
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
  public
    property ToolStatus: TIDEToolStatus read FToolStatus write SetToolStatus;

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

    function DoCheckAmbiguousSources(const AFilename: string;
                                     Compiling: boolean): TModalResult; override;
    function DoCheckCreatingFile(const AFilename: string;
                                 CheckReadable: boolean): TModalResult; override;
    function DoDeleteAmbiguousFiles(const Filename:string
                                    ): TModalResult; override;
    function DoCheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; override;
    function DoOpenMacroFile(Sender: TObject; const AFilename: string
                             ): TModalResult; override;

    procedure UpdateWindowsMenu; override;
    procedure SetRecentSubMenu(ParentMenuItem: TMenuItem; FileList: TStringList;
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
  MainIDE: TMainIDEBase;

  ObjectInspector1 : TObjectInspector;
  SourceNotebook : TSourceNotebook;

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

{ TMainIDEBase }

procedure TMainIDEBase.mnuWindowsItemClick(Sender: TObject);
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

procedure TMainIDEBase.OnMainBarDestroy(Sender: TObject);
begin
  //writeln('TMainIDEBase.OnMainBarDestroy');
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

function TMainIDEBase.CreateMenuSeparator : TMenuItem;
begin
  Result := TMenuItem.Create(MainIDEBar);
  Result.Caption := '-';
end;

procedure TMainIDEBase.CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
  const MenuItemName, MenuItemCaption: String);
begin
  CreateMenuItem(MenuItemParent,MenuItem,MenuItemName,MenuItemCaption,'');
end;

procedure TMainIDEBase.CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
  const MenuItemName, MenuItemCaption: String; const bmpName: String);
begin
  CreateMenuItem(MenuItemParent,MenuItem,MenuItemName,MenuItemCaption,bmpName,
                 true);
end;

procedure TMainIDEBase.CreateMenuItem(MenuItemParent, MenuItem: TMenuItem;
  const MenuItemName, MenuItemCaption: String; const bmpName: String;
  mnuEnabled: Boolean);
begin
  MenuItem:=TMenuItem.Create(MainIDEBar);
  MenuItem.Name:=MenuItemName;
  MenuItem.Caption := MenuItemCaption;
  if not mnuEnabled then
    MenuItem.enabled:=mnuEnabled;
  if bmpName<>'' then
    MenuItem.Bitmap.LoadFromLazarusResource(bmpName);
  MenuItemParent.Add(MenuItem);
end;

procedure TMainIDEBase.CreateMainMenuItem(MainMenu: TMainMenu;
  var MenuItem: TMenuItem; const MenuItemName, MenuItemCaption: String);
begin
  MenuItem:=TMenuItem.Create(MainIDEBar);
  MenuItem.Name:=MenuItemName;
  MenuItem.Caption := MenuItemCaption;
  MainMenu.items.Add(MenuItem);
end;

procedure TMainIDEBase.SetupMainMenu;
begin
  MainIDEBar.mnuMain := TMainMenu.Create(MainIDEBar);
  with MainIDEBar do begin
    mnuMain.Name:='mnuMainMenu';
    Menu := mnuMain;
    CreateMainMenuItem(mnuMain,mnuFile,'mnuFile',lisMenuFile);
    CreateMainMenuItem(mnuMain,mnuEdit,'mnuEdit',lisMenuEdit);
    CreateMainMenuItem(mnuMain,mnuSearch,'mnuSearch',lisMenuSearch);
    CreateMainMenuItem(mnuMain,mnuView,'mnuView',lisMenuView);
    CreateMainMenuItem(mnuMain,mnuProject,'mnuProject',lisMenuProject);
    CreateMainMenuItem(mnuMain,mnuRun,'mnuRun',lisMenuRun);
    CreateMainMenuItem(mnuMain,mnuComponents,'mnuComponents',lisMenuComponents);
    CreateMainMenuItem(mnuMain,mnuTools,'mnuTools',lisMenuTools);
    CreateMainMenuItem(mnuMain,mnuEnvironment,'mnuEnvironment',lisMenuEnvironent);
    CreateMainMenuItem(mnuMain,mnuWindows,'mnuWindows',lisMenuWindows);
    CreateMainMenuItem(mnuMain,mnuHelp,'mnuHelp',lisMenuHelp);
  end;
end;

procedure TMainIDEBase.SetupFileMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuFile;
  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmFileNewUnit,'itmFileNewUnit',lisMenuNewUnit,'menu_new');
    CreateMenuItem(ParentMI,itmFileNewForm,'itmFileNewForm',lisMenuNewForm,'menu_new');
    CreateMenuItem(ParentMI,itmFileNewOther,'itmFileNewOther',lisMenuNewOther,'menu_new');

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFileOpen,'itmFileOpen',lisMenuOpen,'menu_open');
    CreateMenuItem(ParentMI,itmFileRevert,'itmFileRevert',lisMenuRevert,'menu_undo');
    CreateMenuItem(ParentMI,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent,'');
    CreateMenuItem(ParentMI,itmFileSave,'itmFileSave',lisMenuSave,'menu_save');
    CreateMenuItem(ParentMI,itmFileSaveAs,'itmFileSaveAs',lisMenuSaveAs,'menu_save');
    CreateMenuItem(ParentMI,itmFileSaveAll,'itmFileSaveAll',lisMenuSaveAll,'menu_save');
    CreateMenuItem(ParentMI,itmFileClose,'itmFileClose',lisMenuClose,'menu_close',false);
    CreateMenuItem(ParentMI,itmFileCloseAll,'itmFileCloseAll',lisMenuCloseAll,'',false);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFileCleanDirectory,'itmFileCleanDirectory',lisMenuCleanDirectory);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFileRestart,'itmFileRestart',lisMenuRestart);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFileQuit,'itmFileQuit',lisMenuQuit);
  end;
end;

procedure TMainIDEBase.SetupEditMenu;
var
  ParentMI: TMenuItem;
  SubParentMI: TMenuItem;
  SubSubParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuEdit;
  
  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmEditUndo,'itmEditUndo',lisMenuUndo,'menu_undo');
    CreateMenuItem(ParentMI,itmEditRedo,'itmEditRedo',lisMenuRedo,'menu_redo');

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditCut,'itmEditCut',lisMenuCut,'menu_cut');
    CreateMenuItem(ParentMI,itmEditCopy,'itmEditCopy',lisMenuCopy,'menu_copy');
    CreateMenuItem(ParentMI,itmEditPaste,'itmEditPaste',lisMenuPaste,'menu_paste');

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditIndentBlock,'itmEditIndentBlock',lisMenuIndentSelection,'menu_indent');
    CreateMenuItem(ParentMI,itmEditUnindentBlock,'itmEditUnindentBlock',lisMenuUnindentSelection,'menu_unindent');
    CreateMenuItem(ParentMI,itmEditEncloseBlock,'itmEditEncloseBlock',lisMenuEncloseSelection);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditUpperCaseBlock,'itmEditUpperCaseBlock',lisMenuUpperCaseSelection);
    CreateMenuItem(ParentMI,itmEditLowerCaseBlock,'itmEditLowerCaseBlock',lisMenuLowerCaseSelection);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditTabsToSpacesBlock,'itmEditTabsToSpacesBlock',lisMenuTabsToSpacesSelection);
    CreateMenuItem(ParentMI,itmEditSelectionBreakLines,'itmEditSelectionBreakLines',lisMenuBeakLinesInSelection);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditCommentBlock,'itmEditCommentBlock',lisMenuCommentSelection);
    CreateMenuItem(ParentMI,itmEditUncommentBlock,'itmEditUncommentBlock',lisMenuUncommentSelection);
    CreateMenuItem(ParentMI,itmEditConditionalBlock,'itmEditConditionalBlock',lisMenuConditionalSelection);
    CreateMenuItem(ParentMI,itmEditSortBlock,'itmEditSortBlock',lisMenuSortSelection);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditSelect,'itmEditSelect',lisMenuSelect);
    begin
      // select sub menu items
      SubParentMI:=MainIDEBar.itmEditSelect;
      CreateMenuItem(SubParentMI,itmEditSelectAll,'itmEditSelectAll',lisMenuSelectAll);
      CreateMenuItem(SubParentMI,itmEditSelectToBrace,'itmEditSelectToBrace',lisMenuSelectToBrace);
      CreateMenuItem(SubParentMI,itmEditSelectCodeBlock,'itmEditSelectCodeBlock',lisMenuSelectCodeBlock);
      CreateMenuItem(SubParentMI,itmEditSelectLine,'itmEditSelectLine',lisMenuSelectLine);
      CreateMenuItem(SubParentMI,itmEditSelectParagraph,'itmEditSelectParagraph',lisMenuSelectParagraph);
    end;

    CreateMenuItem(ParentMI,itmEditInsertCharacter,'itmEditInsertCharacter',lisMenuInsertCharacter);
    CreateMenuItem(ParentMI,itmEditInsertText,'itmEditInsertText',lisMenuInsertText);
     begin
      // insert text sub menu items
      SubParentMI:=MainIDEBar.itmEditInsertText;
      CreateMenuItem(SubParentMI,itmEditInsertCVSKeyWord,'itmEditInsertCVSKeyWord',lisMenuInsertCVSKeyword);
      begin
        // insert CVS keyword sub menu items
        SubSubParentMI:=MainIDEBar.itmEditInsertCVSKeyWord;
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSAuthor,'itmEditInsertCVSAuthor','Author');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSDate,'itmEditInsertCVSDate','Date');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSHeader,'itmEditInsertCVSHeader','Header');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSID,'itmEditInsertCVSID','ID');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSLog,'itmEditInsertCVSLog','Log');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSName,'itmEditInsertCVSName','Name');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSRevision,'itmEditInsertCVSRevision','Revision');
        CreateMenuItem(SubSubParentMI,itmEditInsertCVSSource,'itmEditInsertCVSSource','Source');
      end;

      CreateMenuItem(SubParentMI,itmEditInsertGeneral,'itmEditInsertGeneral',lisMenuInsertGeneral);
      begin
        // insert general text sub menu items
        SubSubParentMI:=MainIDEBar.itmEditInsertGeneral;
        CreateMenuItem(SubSubParentMI,itmEditInsertGPLNotice,'itmEditInsertGPLNotice',lisMenuInsertGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertLGPLNotice,'itmEditInsertLGPLNotice',lisMenuInsertLGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertUsername,'itmEditInsertUsername',lisMenuInsertUsername);
        CreateMenuItem(SubSubParentMI,itmEditInsertDateTime,'itmEditInsertDateTime',lisMenuInsertDateTime);
        CreateMenuItem(SubSubParentMI,itmEditInsertChangeLogEntry,'itmEditInsertChangeLogEntry',lisMenuInsertChangeLogEntry);
      end;
    end;

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmEditCompleteCode,'itmEditCompleteCode',lisMenuCompleteCode);
    CreateMenuItem(ParentMI,itmEditExtractProc,'itmEditExtractProc',lisMenuExtractProc);
  end;
end;

procedure TMainIDEBase.SetupSearchMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuSearch;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmSearchFind,'itmSearchFind',lisMenuFind);
    CreateMenuItem(ParentMI,itmSearchFindNext,'itmSearchFindNext',lisMenuFindNext);
    CreateMenuItem(ParentMI,itmSearchFindPrevious,'itmSearchFindPrevious',lisMenuFindPrevious);
    CreateMenuItem(ParentMI,itmSearchFindInFiles,'itmSearchFindInFiles',lisMenuFindInFiles);
    CreateMenuItem(ParentMI,itmSearchReplace,'itmSearchReplace',lisMenuReplace);
    CreateMenuItem(ParentMI,itmIncrementalFind,'itmIncrementalFind',lisMenuIncrementalFind);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmGotoLine,'itmGotoLine',lisMenuGotoLine);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmJumpBack,'itmJumpBack',lisMenuJumpBack);
    CreateMenuItem(ParentMI,itmJumpForward,'itmJumpForward',lisMenuJumpForward);
    CreateMenuItem(ParentMI,itmAddJumpPoint,'itmAddJumpPoint',lisMenuAddJumpPointToHistory);
    CreateMenuItem(ParentMI,itmJumpHistory,'itmJumpHistory',lisMenuViewJumpHistory);
    CreateMenuItem(ParentMI,itmJumpToNextError,'itmJumpToNextError',lisMenuJumpToNextError);
    CreateMenuItem(ParentMI,itmJumpToPrevError,'itmJumpToPrevError',lisMenuJumpToPrevError);
    CreateMenuItem(ParentMI,itmJumpToNextBookmark,'itmJumpToNextBookmark',lisMenuJumpToNextBookmark);
    CreateMenuItem(ParentMI,itmJumpToPrevBookmark,'itmJumpToPrevBookmark',lisMenuJumpToPrevBookmark);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFindBlockOtherEnd,'itmFindBlockOtherEnd',lisMenuFindBlockOtherEndOfCodeBlock);
    CreateMenuItem(ParentMI,itmFindBlockStart,'itmFindBlockStart',lisMenuFindCodeBlockStart);

    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmFindDeclaration,'itmFindDeclaration',lisMenuFindDeclarationAtCursor);
    CreateMenuItem(ParentMI,itmOpenFileAtCursor,'itmOpenFileAtCursor',lisMenuOpenFilenameAtCursor);
    CreateMenuItem(ParentMI,itmGotoIncludeDirective,'itmGotoIncludeDirective',lisMenuGotoIncludeDirective);
    CreateMenuItem(ParentMI,itmSearchFindIdentifierRefs,'itmSearchFindIdentifierRefs',lisMenuFindIdentifierRefs);
    CreateMenuItem(ParentMI,itmSearchRenameIdentifier,'itmSearchRenameIdentifier',lisMenuRenameIdentifier);
  end;
end;

procedure TMainIDEBase.SetupViewMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuView;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmViewInspector,'itmViewInspector',lisMenuViewObjectInspector);
    CreateMenuItem(ParentMI,itmViewSourceEditor,'itmViewSourceEditor',lisMenuViewSourceEditor);
    CreateMenuItem(ParentMI,itmViewCodeExplorer,'itmViewCodeExplorer',lisMenuViewCodeExplorer);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmViewUnits,'itmViewUnits',lisMenuViewUnits);
    CreateMenuItem(ParentMI,itmViewForms,'itmViewForms',lisMenuViewForms);
    CreateMenuItem(ParentMI,itmViewUnitDependencies,'itmViewUnitDependencies',lisMenuViewUnitDependencies);
    CreateMenuItem(ParentMI,itmViewUnitInfo,'itmViewUnitInfo',lisMenuViewUnitInfo);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmViewToggleFormUnit,'itmViewToggleFormUnit',lisMenuViewToggleFormUnit);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmViewMessage,'itmViewMessage',lisMenuViewMessages);
    CreateMenuItem(ParentMI,itmViewSearchResults,'itmViewSearchResults',lisMenuViewSearchResults);
    CreateMenuItem(ParentMI,itmViewAnchorEditor,'itmViewAnchorEditor',lisMenuViewAnchorEditor);
    CreateMenuItem(ParentMI,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'menu_debugger');
    begin
      CreateMenuItem(itmViewDebugWindows,itmViewWatches,'itmViewWatches',lisMenuViewWatches,'menu_watches');
      CreateMenuItem(itmViewDebugWindows,itmViewBreakPoints,'itmViewBreakPoints',lisMenuViewBreakPoints,'menu_breakpoints');
      CreateMenuItem(itmViewDebugWindows,itmViewLocals,'itmViewLocals',lisMenuViewLocalVariables,'');
      CreateMenuItem(itmViewDebugWindows,itmViewCallStack,'itmViewCallStack',lisMenuViewCallStack,'menu_callstack');
      CreateMenuItem(itmViewDebugWindows,itmViewDebugOutput,'itmViewDebugOutput',lisMenuViewDebugOutput,'menu_debugoutput');
    end;
  end;
end;

procedure TMainIDEBase.SetupProjectMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuProject;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmProjectNew,'itmProjectNew',lisMenuNewProject);
    CreateMenuItem(ParentMI,itmProjectNewFromFile,'itmProjectNewFromFile',lisMenuNewProjectFromFile);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmProjectOpen,'itmProjectOpen',lisMenuOpenProject,'menu_openproject');
    CreateMenuItem(ParentMI,itmProjectRecentOpen,'itmProjectRecentOpen',lisMenuOpenRecentProject);
    ParentMI.Add(CreateMenuSeparator);


    CreateMenuItem(ParentMI,itmProjectSave,'itmProjectSave',lisMenuSaveProject);
    CreateMenuItem(ParentMI,itmProjectSaveAs,'itmProjectSaveAs',lisMenuSaveProjectAs);
    CreateMenuItem(ParentMI,itmProjectPublish,'itmProjectPublish',lisMenuPublishProject);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmProjectInspector,'itmProjectInspector',lisMenuProjectInspector,'menu_projectinspector');
    CreateMenuItem(ParentMI,itmProjectOptions,'itmProjectOptions',lisMenuProjectOptions,'menu_projectoptions');
    CreateMenuItem(ParentMI,itmProjectCompilerOptions,'itmProjectCompilerOptions',lisMenuCompilerOptions);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmProjectAddTo,'itmProjectAddTo',lisMenuAddToProject);
    CreateMenuItem(ParentMI,itmProjectRemoveFrom,'itmProjectRemoveFrom',lisMenuRemoveFromProject);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmProjectViewSource,'itmProjectViewSource',lisMenuViewSource);
    CreateMenuItem(ParentMI,itmProjectViewToDos,'itmProjectViewToDos',lisMenuViewProjectTodos);
    
    {$IFDEF TRANSLATESTRING}
    CreateMenuItem(ParentMI, itmProjectCreatePoFiles,'itmProjectCreatePoFiles', lisMenuCreatePoFile);
    CreateMenuItem(ParentMI, itmProjectCollectPoFiles, 'itmProjectCollectPoFiles', lisMenuCollectPoFil);
    {$ENDIF}
  end;
end;

procedure TMainIDEBase.SetupRunMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuRun;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build');
    CreateMenuItem(ParentMI,itmRunMenuBuildAll,'itmRunMenuBuildAll',lisMenuBuildAll,'menu_buildall');
    CreateMenuItem(ParentMI,itmRunMenuAbortBuild,'itmRunMenuAbortBuild',lisMenuAbortBuild);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmRunMenuRun,'itmRunMenuRun',lisMenuProjectRun,'menu_run');
    CreateMenuItem(ParentMI,itmRunMenuPause,'itmRunMenuPause',lisMenuPause,'menu_pause');
    CreateMenuItem(ParentMI,itmRunMenuStepInto,'itmRunMenuStepInto',lisMenuStepInto,'menu_stepinto');
    CreateMenuItem(ParentMI,itmRunMenuStepOver,'itmRunMenuStepOver',lisMenuStepOver,'menu_stepover');
    CreateMenuItem(ParentMI,itmRunMenuRunToCursor,'itmRunMenuRunToCursor',lisMenuRunToCursor);
    CreateMenuItem(ParentMI,itmRunMenuStop,'itmRunMenuStop',lisMenuStop,'');
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmRunMenuRunParameters,'itmRunMenuRunParameters',lisMenuRunParameters);
    CreateMenuItem(ParentMI,itmRunMenuResetDebugger,'itmRunMenuResetDebugger',lisMenuResetDebugger);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmRunMenuBuildFile,'itmRunMenuBuildFile',lisMenuBuildFile);
    CreateMenuItem(ParentMI,itmRunMenuRunFile,'itmRunMenuRunFile',lisMenuRunFile);
    CreateMenuItem(ParentMI,itmRunMenuConfigBuildFile,'itmRunMenuConfigBuildFile',lisMenuConfigBuildFile);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmRunMenuInspect,'itmRunMenuInspect',lisMenuInspect, '', False);
    CreateMenuItem(ParentMI,itmRunMenuEvaluate,'itmRunMenuEvaluate',lisMenuEvaluate, '', False);
    CreateMenuItem(ParentMI,itmRunMenuAddWatch,'itmRunMenuAddWatch',lisMenuAddWatch, '', False);
    CreateMenuItem(ParentMI,itmRunMenuAddBreakpoint,'itmRunMenuAddBreakpoint',lisMenuAddBreakpoint, '');
    CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBPSource,'itmRunMenuAdddBPSource',lisMenuAddBPSource, '', False);
  end;
end;

procedure TMainIDEBase.SetupComponentsMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuComponents;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmPkgOpenPackage,'itmPkgOpenPackage',lisMenuOpenPackage,'pkg_package');
    CreateMenuItem(ParentMI,itmPkgOpenPackageFile,'itmPkgOpenPackageFile',lisMenuOpenPackageFile,'pkg_package');
    CreateMenuItem(ParentMI,itmPkgOpenPackageOfCurUnit,'itmPkgOpenPackageOfCurUnit',lisMenuOpenPackageOfCurUnit,'pkg_package');
    CreateMenuItem(ParentMI,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg,'pkg_package');
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmPkgAddCurUnitToPkg,'itmPkgAddCurUnitToPkg',lisMenuAddCurUnitToPkg,'pkg_addunittopackage');
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmPkgPkgGraph,'itmPkgPkgGraph',lisMenuPackageGraph,'pkg_packagegraph');
    CreateMenuItem(ParentMI,itmPkgEditInstallPkgs,'itmPkgEditInstallPkgs',lisMenuEditInstallPkgs,'pkg_package_install');

    {$IFDEF CustomIDEComps}
    ParentMI.Add(CreateMenuSeparator);
    CreateMenuItem(ParentMI,itmCompsConfigCustomComps,'itmCompsConfigCustomComps',lisMenuConfigCustomComps);
    {$ENDIF}
  end;
end;

procedure TMainIDEBase.SetupToolsMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuTools;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmToolConfigure,'itmToolConfigure',lisMenuSettings);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmToolSyntaxCheck,'itmToolSyntaxCheck',lisMenuQuickSyntaxCheck);
    CreateMenuItem(ParentMI,itmToolGuessUnclosedBlock,'itmToolGuessUnclosedBlock',lisMenuGuessUnclosedBlock);
    CreateMenuItem(ParentMI,itmToolGuessMisplacedIFDEF,'itmToolGuessMisplacedIFDEF',lisMenuGuessMisplacedIFDEF);
    CreateMenuItem(ParentMI,itmToolMakeResourceString,'itmToolMakeResourceString',lisMenuMakeResourceString);
    CreateMenuItem(ParentMI,itmToolDiff,'itmToolDiff',lisMenuDiff);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmToolCheckLFM,'itmToolCheckLFM',lisMenuCheckLFM);
    CreateMenuItem(ParentMI,itmToolConvertDelphiUnit,'itmToolConvertDelphiUnit',lisMenuConvertDelphiUnit);
    CreateMenuItem(ParentMI,itmToolConvertDelphiProject,'itmToolConvertDelphiProject',lisMenuConvertDelphiProject);
    CreateMenuItem(ParentMI,itmToolConvertDFMtoLFM,'itmToolConvertDFMtoLFM',lisMenuConvertDFMtoLFM);
    ParentMI.Add(CreateMenuSeparator);

    CreateMenuItem(ParentMI,itmToolBuildLazarus,'itmToolBuildLazarus',lisMenuBuildLazarus,'menu_buildlazarus');
    CreateMenuItem(ParentMI,itmToolConfigureBuildLazarus,'itmToolConfigureBuildLazarus',lisMenuConfigureBuildLazarus);
  end;
end;

procedure TMainIDEBase.SetupEnvironmentMenu;
var
  ParentMI: TMenuItem;
begin
  ParentMI:=MainIDEBar.mnuEnvironment;

  with MainIDEBar do begin
    CreateMenuItem(ParentMI,itmEnvGeneralOptions,'itmEnvGeneralOptions',
                   lisMenuGeneralOptions,'menu_environmentoptions');
    CreateMenuItem(ParentMI,itmEnvEditorOptions,'itmEnvEditorOptions',
                   lisMenuEditorOptions,'menu_editoroptions');
    CreateMenuItem(ParentMI,itmEnvDebuggerOptions,'itmEnvDebuggerOptions',
                   lisMenDebuggerOptions,'');
    CreateMenuItem(ParentMI,itmEnvCodeToolsOptions,'itmEnvCodeToolsOptions',
                   lisMenuCodeToolsOptions,'menu_codetoolsoptions');
    CreateMenuItem(ParentMI,itmEnvCodeToolsDefinesEditor,
                   'itmEnvCodeToolsDefinesEditor',lisMenuCodeToolsDefinesEditor,
                   'menu_codetoolsdefineseditor');
    ParentMI.Add(CreateMenuSeparator);
    CreateMenuItem(ParentMI,itmEnvRescanFPCSrcDir,'itmEnvRescanFPCSrcDir',
                   lisMenuRescanFPCSourceDirectory);
  end;
end;

procedure TMainIDEBase.SetupWindowsMenu;
begin

end;

procedure TMainIDEBase.SetupHelpMenu;
begin
  with MainIDEBar do begin
    CreateMenuItem(mnuHelp,itmHelpOnlineHelp,'itmHelpOnlineHelp',
                   lisMenuOnlineHelp);
    CreateMenuItem(mnuHelp,itmHelpConfigureHelp,'itmHelpConfigureHelp',
                   lisMenuConfigureHelp);
    mnuHelp.Add(CreateMenuSeparator);
    CreateMenuItem(mnuHelp,itmHelpAboutLazarus,'itmHelpAboutLazarus',
                   lisMenuAboutLazarus);
  end;
end;

procedure TMainIDEBase.LoadMenuShortCuts;
begin
  with MainIDEBar, EditorOpts.KeyMap do begin
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
    itmEditConditionalBlock.ShortCut:=CommandToShortCut(ecSelectionConditional);
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
    itmSearchFindIdentifierRefs.ShortCut:=CommandToShortCut(ecFindIdentifierRefs);
    itmSearchReplace.ShortCut:=CommandToShortCut(ecReplace);
    itmSearchRenameIdentifier.ShortCut:=CommandToShortCut(ecRenameIdentifier);
    itmIncrementalFind.ShortCut:=CommandToShortCut(ecIncrementalFind);
    itmGotoLine.ShortCut:=CommandToShortCut(ecGotoLineNumber);
    itmJumpBack.ShortCut:=CommandToShortCut(ecJumpBack);
    itmJumpForward.ShortCut:=CommandToShortCut(ecJumpForward);
    itmAddJumpPoint.ShortCut:=CommandToShortCut(ecAddJumpPoint);
    itmJumpHistory.ShortCut:=CommandToShortCut(ecViewJumpHistory);
    itmJumpToNextError.ShortCut:=CommandToShortCut(ecJumpToNextError);
    itmJumpToPrevError.ShortCut:=CommandToShortCut(ecJumpToPrevError);
    itmJumpToNextBookmark.ShortCut:=CommandToShortCut(ecNextBookmark);
    itmJumpToPrevBookmark.ShortCut:=CommandToShortCut(ecPrevBookmark);
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
    itmViewUnitInfo.ShortCut:=CommandToShortCut(ecViewUnitInfo);
    itmViewForms.ShortCut:=CommandToShortCut(ecViewForms);
    itmViewToggleFormUnit.ShortCut:=CommandToShortCut(ecToggleFormUnit);
    itmViewMessage.ShortCut:=CommandToShortCut(ecToggleMessages);
    itmViewSearchResults.ShortCut:=CommandToShortCut(ecToggleSearchResults);
    itmViewAnchorEditor.ShortCut:=CommandToShortCut(ecViewAnchorEditor);

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
    itmPkgOpenPackageOfCurUnit.ShortCut:=CommandToShortCut(ecOpenPackageOfCurUnit);
    itmPkgAddCurUnitToPkg.ShortCut:=CommandToShortCut(ecAddCurUnitToPkg);
    itmPkgPkgGraph.ShortCut:=CommandToShortCut(ecPackageGraph);
    itmPkgEditInstallPkgs.ShortCut:=CommandToShortCut(ecEditInstallPkgs);
    {$IFDEF CustomIDEComps}
    itmCompsConfigCustomComps.ShortCut:=CommandToShortCut(ecConfigCustomComps);
    {$ENDIF}

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
    itmToolConvertDelphiProject.ShortCut:=CommandToShortCut(ecConvertDelphiProject);
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
    itmHelpOnlineHelp.ShortCut:=CommandToShortCut(ecOnlineHelp);
    itmHelpConfigureHelp.ShortCut:=CommandToShortCut(ecConfigureHelp);
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

{-------------------------------------------------------------------------------
  function TMainIDEBase.DoCheckCreatingFile(const AFilename: string;
    CheckReadable: boolean): TModalResult;
-------------------------------------------------------------------------------}
function TMainIDEBase.DoCheckCreatingFile(const AFilename: string;
  CheckReadable: boolean): TModalResult;
var
  fs: TFileStream;
  c: char;
begin
  // create if not yet done
  if not FileExists(AFilename) then begin
    try
      InvalidateFileStateCache;
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
    if CheckReadable then begin
      InvalidateFileStateCache;
      fs:=TFileStream.Create(AFilename,fmOpenWrite)
    end else
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
    InvalidateFileStateCache;
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

function TMainIDEBase.DoDeleteAmbiguousFiles(const Filename: string
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
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if EnvironmentOptions.AmbiguousFileAction
    in [afaAsk,afaAutoDelete,afaAutoRename]
  then begin
    ADirectory:=AppendPathDelim(ExtractFilePath(Filename));
    if SysUtils.FindFirst(ADirectory+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
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
        if EnvironmentOptions.AmbiguousFileAction=afaAsk then begin
          if MessageDlg(lisDeleteAmbiguousFile,
            Format(lisAmbiguousFileFoundThisFileCanBeMistakenWithDelete, ['"',
              CurFilename, '"', #13, '"', ShortFilename, '"', #13, #13]),
            mtConfirmation,[mbYes,mbNo],0)=mrNo
          then continue;
        end;
        if EnvironmentOptions.AmbiguousFileAction in [afaAutoDelete,afaAsk]
        then begin
          if not DeleteFile(CurFilename) then begin
            MessageDlg(lisDeleteFileFailed,
              Format(lisPkgMangUnableToDeleteFile, ['"', CurFilename, '"']),
              mtError,[mbOk],0);
          end;
        end else if EnvironmentOptions.AmbiguousFileAction=afaAutoRename then
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
  function TMainIDEBase.DoCheckUnitPathForAmbiguousPascalFiles(
    const BaseDir, TheUnitPath, CompiledExt, ContextDescription: string
    ): TModalResult;

  Collect all pascal files and all compiled units in the unit path and check
  for ambiguous files. For example: doubles.
-------------------------------------------------------------------------------}
function TMainIDEBase.DoCheckUnitPathForAmbiguousPascalFiles(
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

  //writeln('TMainIDEBase.DoCheckUnitPathForAmbiguousPascalFiles A UnitPath="',UnitPath,'" Ext=',CompiledExt,' Context=',ContextDescription);

  SourceUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
  CompiledUnitTree:=TAVLTree.Create(TListSortCompare(@CompareUnitFiles));
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
        if SysUtils.FindFirst(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
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
                                 TListSortCompare(@CompareUnitNameAndUnitFile));
            if ANode<>nil then begin
              // pascal unit exists twice
              Result:=MessageDlg('Ambiguous unit found',
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
  function TMainIDEBase.DoCheckAmbiguousSources(const AFilename: string
    ): TModalResult;

  Checks if file exists with same name and similar extension. The compiler
  prefers for example .pp to .pas files. So, if we save a .pas file delete .pp
  file, so that compiling does what is expected.
-------------------------------------------------------------------------------}
function TMainIDEBase.DoCheckAmbiguousSources(const AFilename: string;
  Compiling: boolean): TModalResult;

  function DeleteAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  begin
    if not DeleteFile(AmbiguousFilename) then begin
      Result:=MessageDlg(lisErrorDeletingFile,
       Format(lisUnableToDeleteAmbiguousFile, ['"', AmbiguousFilename, '"']),
       mtError,[mbOk,mbAbort],0);
    end else
      Result:=mrOk;
  end;

  function RenameAmbiguousFile(const AmbiguousFilename: string): TModalResult;
  var
    NewFilename: string;
  begin
    NewFilename:=AmbiguousFilename+'.ambiguous';
    if not RenameFile(AmbiguousFilename,NewFilename) then
    begin
      Result:=MessageDlg(lisErrorRenamingFile,
       Format(lisUnableToRenameAmbiguousFileTo, ['"', AmbiguousFilename, '"',
         #13, '"', NewFilename, '"']),
       mtError,[mbOk,mbAbort],0);
    end else
      Result:=mrOk;
  end;

  function AddCompileWarning(const AmbiguousFilename: string): TModalResult;
  begin
    Result:=mrOk;
    if Compiling then begin
      TheOutputFilter.ReadLine(Format(lisWarningAmbiguousFileFoundSourceFileIs,
        ['"', AmbiguousFilename, '"', '"', AFilename, '"']), true);
    end;
  end;

  function CheckFile(const AmbiguousFilename: string): TModalResult;
  begin
    if not FileExists(AmbiguousFilename) then exit;
    if Compiling then begin
      Result:=AddCompileWarning(AmbiguousFilename);
      exit;
    end;
    case EnvironmentOptions.AmbiguousFileAction of
    afaAsk:
      begin
        Result:=MessageDlg(lisAmbiguousFileFound,
          Format(lisThereIsAFileWithTheSameNameAndASimilarExtension, [#13,
            AFilename, #13, AmbiguousFilename, #13, #13]),
          mtWarning,[mbYes,mbIgnore,mbAbort],0);
        case Result of
        mrYes:    Result:=DeleteAmbiguousFile(AmbiguousFilename);
        mrIgnore: Result:=mrOk;
        end;
      end;

    afaAutoDelete:
      Result:=DeleteAmbiguousFile(AmbiguousFilename);

    afaAutoRename:
      Result:=RenameAmbiguousFile(AmbiguousFilename);

    afaWarnOnCompile:
      Result:=AddCompileWarning(AmbiguousFilename);

    else
      Result:=mrOk;
    end;
  end;

var
  Ext, LowExt: string;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbiguousFileAction=afaWarnOnCompile)
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

procedure TMainIDEBase.UpdateWindowsMenu;
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
    if (AForm<>MainIDEBar) and (AForm<>SplashForm)
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
    if MainIDEBar.mnuWindows.Count>i then
      CurMenuItem:=MainIDEBar.mnuWindows.Items[i]
    else begin
      CurMenuItem:=TMenuItem.Create(MainIDEBar);
      MainIDEBar.mnuWindows.Add(CurMenuItem);
      CurMenuItem.OnClick:=@mnuWindowsItemClick;
    end;
    CurMenuItem.Caption:=TCustomForm(WindowsList[i]).Caption;
  end;
  // remove unused menuitems
  while MainIDEBar.mnuWindows.Count>WindowsList.Count do
    MainIDEBar.mnuWindows.Items[MainIDEBar.mnuWindows.Count-1].Free;
  // clean up
  WindowsList.Free;
end;

procedure TMainIDEBase.SetRecentSubMenu(ParentMenuItem: TMenuItem;
  FileList: TStringList; OnClickEvent: TNotifyEvent);
var i: integer;
  AMenuItem: TMenuItem;
begin
  // create enough menuitems
  while ParentMenuItem.Count<FileList.Count do begin
    AMenuItem:=TMenuItem.Create(MainIDEBar);
    AMenuItem.Name:=
      ParentMenuItem.Name+'Recent'+IntToStr(ParentMenuItem.Count);
    ParentMenuItem.Add(AMenuItem);
  end;
  // delete unused menuitems
  while ParentMenuItem.Count>FileList.Count do
    ParentMenuItem.Items[ParentMenuItem.Count-1].Free;
  ParentMenuItem.Enabled:=(ParentMenuItem.Count>0);
  // set captions and event
  for i:=0 to FileList.Count-1 do begin
    AMenuItem:=ParentMenuItem.Items[i];
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

