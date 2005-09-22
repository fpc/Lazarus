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
  IDECommands,
  ProjectDefs, Project, PublishModule, BuildLazDialog, Compiler,
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
    {$IFDEF UseMenuIntf}
    procedure CreateMenuItem(Section: TIDEMenuSection;
                             var MenuItem: TIDEMenuCommand;
                             const MenuItemName, MenuItemCaption: String;
                             const bmpName: String = '';
                             mnuEnabled: Boolean = true);
    procedure CreateMenuSeparatorSection(ParentSection: TIDEMenuSection;
                             var Section: TIDEMenuSection; const AName: String);
    procedure CreateMenuSubSection(ParentSection: TIDEMenuSection;
                             var Section: TIDEMenuSection;
                             const AName, ACaption: String;
                             const bmpName: String = '');
    procedure CreateMainMenuItem(var Section: TIDEMenuSection;
                                 const MenuItemName, MenuItemCaption: String);
    {$ELSE}
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
    {$ENDIF}
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
    procedure SetRecentSubMenu({$IFDEF UseMenuIntf}Section: TIDEMenuSection;
                               {$ELSE}Section: TMenuItem;{$ENDIF}
                               FileList: TStringList;
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

{$IFDEF UseMenuIntf}
procedure TMainIDEBase.CreateMenuItem(Section: TIDEMenuSection;
  var MenuItem: TIDEMenuCommand; const MenuItemName, MenuItemCaption: String;
  const bmpName: String; mnuEnabled: Boolean);
begin
  MenuItem:=RegisterIDEMenuCommand(Section.GetPath,MenuItemName,MenuItemCaption);
  MenuItem.Enabled:=mnuEnabled;
  if bmpName<>'' then
    MenuItem.Bitmap.LoadFromLazarusResource(bmpName);
end;

procedure TMainIDEBase.CreateMenuSeparatorSection(
  ParentSection: TIDEMenuSection; var Section: TIDEMenuSection;
  const AName: String);
begin
  Section:=RegisterIDEMenuSection(ParentSection.GetPath,AName);
  Section.ChildsAsSubMenu := false;
end;

procedure TMainIDEBase.CreateMenuSubSection(ParentSection: TIDEMenuSection;
  var Section: TIDEMenuSection; const AName, ACaption: String;
  const bmpName: String = '');
begin
  Section:=RegisterIDESubMenu(ParentSection.GetPath,AName,ACaption);
  if bmpName<>'' then
    Section.Bitmap.LoadFromLazarusResource(bmpName);
end;

procedure TMainIDEBase.CreateMainMenuItem(var Section: TIDEMenuSection;
  const MenuItemName, MenuItemCaption: String);
begin
  Section:=RegisterIDESubMenu(MainIDEBar.mnuMain.GetPath,MenuItemName,
                              MenuItemCaption);
end;
{$ELSE}
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
{$ENDIF}

procedure TMainIDEBase.SetupMainMenu;
begin
  {$IFDEF UseMenuIntf}
  MainIDEBar.mnuMainMenu := TMainMenu.Create(MainIDEBar);
  with MainIDEBar do begin
    mnuMain:=RegisterIDEMenuRoot('IDEMainMenu',mnuMainMenu.Items);
    CreateMainMenuItem(mnuFile,'mnuFile',lisMenuFile);
    CreateMainMenuItem(mnuEdit,'mnuEdit',lisMenuEdit);
    CreateMainMenuItem(mnuSearch,'mnuSearch',lisMenuSearch);
    CreateMainMenuItem(mnuView,'mnuView',lisMenuView);
    CreateMainMenuItem(mnuProject,'mnuProject',lisMenuProject);
    CreateMainMenuItem(mnuRun,'mnuRun',lisMenuRun);
    CreateMainMenuItem(mnuComponents,'mnuComponents',lisMenuComponents);
    CreateMainMenuItem(mnuTools,'mnuTools',lisMenuTools);
    CreateMainMenuItem(mnuEnvironment,'mnuEnvironment',lisMenuEnvironent);
    CreateMainMenuItem(mnuWindows,'mnuWindows',lisMenuWindows);
    CreateMainMenuItem(mnuHelp,'mnuHelp',lisMenuHelp);
  end;
  {$ELSE}
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
  {$ENDIF}
end;

procedure TMainIDEBase.SetupFileMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuFile,itmFileNew,'itmFileNew');
    ParentMI:=itmFileNew;
    {$ELSE}
    ParentMI:=mnuFile;
    {$ENDIF}
    
    CreateMenuItem(ParentMI,itmFileNewUnit,'itmFileNewUnit',lisMenuNewUnit,'menu_new');
    CreateMenuItem(ParentMI,itmFileNewForm,'itmFileNewForm',lisMenuNewForm,'menu_new');
    CreateMenuItem(ParentMI,itmFileNewOther,'itmFileNewOther',lisMenuNewOther,'menu_new');

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuFile,itmFileOpenSave,'itmFileOpenSave');
    ParentMI:=itmFileOpenSave;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmFileOpen,'itmFileOpen',lisMenuOpen,'menu_open');
    CreateMenuItem(ParentMI,itmFileRevert,'itmFileRevert',lisMenuRevert,'menu_undo');
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent);
    {$ELSE}
    CreateMenuItem(ParentMI,itmFileRecentOpen,'itmFileRecentOpen',lisMenuOpenRecent);
    {$ENDIF}
    CreateMenuItem(ParentMI,itmFileSave,'itmFileSave',lisMenuSave,'menu_save');
    CreateMenuItem(ParentMI,itmFileSaveAs,'itmFileSaveAs',lisMenuSaveAs,'menu_save');
    CreateMenuItem(ParentMI,itmFileSaveAll,'itmFileSaveAll',lisMenuSaveAll,'menu_save');
    CreateMenuItem(ParentMI,itmFileClose,'itmFileClose',lisMenuClose,'menu_close',false);
    CreateMenuItem(ParentMI,itmFileCloseAll,'itmFileCloseAll',lisMenuCloseAll,'',false);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuFile,itmFileDirectories,'itmFileDirectories');
    ParentMI:=itmFileDirectories;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmFileCleanDirectory,'itmFileCleanDirectory',lisMenuCleanDirectory);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuFile,itmFileIDEStart,'itmFileIDEStart');
    ParentMI:=itmFileIDEStart;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmFileRestart,'itmFileRestart',lisMenuRestart);
    CreateMenuItem(ParentMI,itmFileQuit,'itmFileQuit',lisMenuQuit);
  end;
end;

procedure TMainIDEBase.SetupEditMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
  SubParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
  SubSubParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditReUndo,'itmEditReUndo');
    ParentMI:=itmEditReUndo;
    {$ELSE}
    ParentMI:=mnuEdit;
    {$ENDIF}
    CreateMenuItem(ParentMI,itmEditUndo,'itmEditUndo',lisMenuUndo,'menu_undo');
    CreateMenuItem(ParentMI,itmEditRedo,'itmEditRedo',lisMenuRedo,'menu_redo');

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditClipboard,'itmEditClipboard');
    ParentMI:=itmEditClipboard;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEditCut,'itmEditCut',lisMenuCut,'menu_cut');
    CreateMenuItem(ParentMI,itmEditCopy,'itmEditCopy',lisMenuCopy,'menu_copy');
    CreateMenuItem(ParentMI,itmEditPaste,'itmEditPaste',lisMenuPaste,'menu_paste');

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditBlockIndentation,'itmEditBlockIndentation');
    ParentMI:=itmEditBlockIndentation;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEditIndentBlock,'itmEditIndentBlock',lisMenuIndentSelection,'menu_indent');
    CreateMenuItem(ParentMI,itmEditUnindentBlock,'itmEditUnindentBlock',lisMenuUnindentSelection,'menu_unindent');
    CreateMenuItem(ParentMI,itmEditEncloseBlock,'itmEditEncloseBlock',lisMenuEncloseSelection);
    CreateMenuItem(ParentMI,itmEditCommentBlock,'itmEditCommentBlock',lisMenuCommentSelection);
    CreateMenuItem(ParentMI,itmEditUncommentBlock,'itmEditUncommentBlock',lisMenuUncommentSelection);
    CreateMenuItem(ParentMI,itmEditConditionalBlock,'itmEditConditionalBlock',lisMenuConditionalSelection);
    CreateMenuItem(ParentMI,itmEditSortBlock,'itmEditSortBlock',lisMenuSortSelection);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditBlockCharConversion,'itmEditBlockCharConversion');
    ParentMI:=itmEditBlockCharConversion;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEditUpperCaseBlock,'itmEditUpperCaseBlock',lisMenuUpperCaseSelection);
    CreateMenuItem(ParentMI,itmEditLowerCaseBlock,'itmEditLowerCaseBlock',lisMenuLowerCaseSelection);
    CreateMenuItem(ParentMI,itmEditTabsToSpacesBlock,'itmEditTabsToSpacesBlock',lisMenuTabsToSpacesSelection);
    CreateMenuItem(ParentMI,itmEditSelectionBreakLines,'itmEditSelectionBreakLines',lisMenuBeakLinesInSelection);

    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(mnuEdit,itmEditSelect,'itmEditSelect',lisMenuSelect);
    {$ELSE}
    CreateMenuItem(ParentMI,itmEditSelect,'itmEditSelect',lisMenuSelect);
    {$ENDIF}
    begin
      // select sub menu items
      SubParentMI:=itmEditSelect;
      CreateMenuItem(SubParentMI,itmEditSelectAll,'itmEditSelectAll',lisMenuSelectAll);
      CreateMenuItem(SubParentMI,itmEditSelectToBrace,'itmEditSelectToBrace',lisMenuSelectToBrace);
      CreateMenuItem(SubParentMI,itmEditSelectCodeBlock,'itmEditSelectCodeBlock',lisMenuSelectCodeBlock);
      CreateMenuItem(SubParentMI,itmEditSelectLine,'itmEditSelectLine',lisMenuSelectLine);
      CreateMenuItem(SubParentMI,itmEditSelectParagraph,'itmEditSelectParagraph',lisMenuSelectParagraph);
    end;

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditInsertions,'itmEditInsertions');
    ParentMI:=itmEditInsertions;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}
    
    CreateMenuItem(ParentMI,itmEditInsertCharacter,'itmEditInsertCharacter',lisMenuInsertCharacter);
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmEditInsertText,'itmEditInsertText',lisMenuInsertText);
    {$ELSE}
    CreateMenuItem(ParentMI,itmEditInsertText,'itmEditInsertText',lisMenuInsertText);
    {$ENDIF}
     begin
      // insert text sub menu items
      SubParentMI:=itmEditInsertText;
      {$IFDEF UseMenuIntf}
      CreateMenuSubSection(SubParentMI,itmEditInsertCVSKeyWord,'itmEditInsertCVSKeyWord',lisMenuInsertCVSKeyword);
      {$ELSE}
      CreateMenuItem(SubParentMI,itmEditInsertCVSKeyWord,'itmEditInsertCVSKeyWord',lisMenuInsertCVSKeyword);
      {$ENDIF}
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

      {$IFDEF UseMenuIntf}
      CreateMenuSubSection(SubParentMI,itmEditInsertGeneral,'itmEditInsertGeneral',lisMenuInsertGeneral);
      {$ELSE}
      CreateMenuItem(SubParentMI,itmEditInsertGeneral,'itmEditInsertGeneral',lisMenuInsertGeneral);
      {$ENDIF}
      begin
        // insert general text sub menu items
        SubSubParentMI:=itmEditInsertGeneral;
        CreateMenuItem(SubSubParentMI,itmEditInsertGPLNotice,'itmEditInsertGPLNotice',lisMenuInsertGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertLGPLNotice,'itmEditInsertLGPLNotice',lisMenuInsertLGPLNotice);
        CreateMenuItem(SubSubParentMI,itmEditInsertUsername,'itmEditInsertUsername',lisMenuInsertUsername);
        CreateMenuItem(SubSubParentMI,itmEditInsertDateTime,'itmEditInsertDateTime',lisMenuInsertDateTime);
        CreateMenuItem(SubSubParentMI,itmEditInsertChangeLogEntry,'itmEditInsertChangeLogEntry',lisMenuInsertChangeLogEntry);
      end;
    end;

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEdit,itmEditMenuCodeTools,'itmEditMenuCodeTools');
    ParentMI:=itmEditMenuCodeTools;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEditCompleteCode,'itmEditCompleteCode',lisMenuCompleteCode);
    CreateMenuItem(ParentMI,itmEditExtractProc,'itmEditExtractProc',lisMenuExtractProc);
  end;
end;

procedure TMainIDEBase.SetupSearchMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuSearch,itmSearchFindReplace,'itmSearchFindReplace');
    ParentMI:=itmSearchFindReplace;
    {$ELSE}
    ParentMI:=mnuSearch;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmSearchFind,'itmSearchFind',lisMenuFind);
    CreateMenuItem(ParentMI,itmSearchFindNext,'itmSearchFindNext',lisMenuFindNext);
    CreateMenuItem(ParentMI,itmSearchFindPrevious,'itmSearchFindPrevious',lisMenuFindPrevious);
    CreateMenuItem(ParentMI,itmSearchFindInFiles,'itmSearchFindInFiles',lisMenuFindInFiles);
    CreateMenuItem(ParentMI,itmSearchReplace,'itmSearchReplace',lisMenuReplace);
    CreateMenuItem(ParentMI,itmIncrementalFind,'itmIncrementalFind',lisMenuIncrementalFind);
    CreateMenuItem(ParentMI,itmGotoLine,'itmGotoLine',lisMenuGotoLine);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuSearch,itmJumpings,'itmJumpings');
    ParentMI:=itmJumpings;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmJumpBack,'itmJumpBack',lisMenuJumpBack);
    CreateMenuItem(ParentMI,itmJumpForward,'itmJumpForward',lisMenuJumpForward);
    CreateMenuItem(ParentMI,itmAddJumpPoint,'itmAddJumpPoint',lisMenuAddJumpPointToHistory);
    CreateMenuItem(ParentMI,itmJumpHistory,'itmJumpHistory',lisMenuViewJumpHistory);
    CreateMenuItem(ParentMI,itmJumpToNextError,'itmJumpToNextError',lisMenuJumpToNextError);
    CreateMenuItem(ParentMI,itmJumpToPrevError,'itmJumpToPrevError',lisMenuJumpToPrevError);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuSearch,itmBookmarks,'itmBookmarks');
    ParentMI:=itmBookmarks;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmSetFreeBookmark,'itmSetFreeBookmark',lisMenuSetFreeBookmark);
    CreateMenuItem(ParentMI,itmJumpToNextBookmark,'itmJumpToNextBookmark',lisMenuJumpToNextBookmark);
    CreateMenuItem(ParentMI,itmJumpToPrevBookmark,'itmJumpToPrevBookmark',lisMenuJumpToPrevBookmark);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuSearch,itmCodeToolSearches,'itmCodeToolSearches');
    ParentMI:=itmCodeToolSearches;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmFindBlockOtherEnd,'itmFindBlockOtherEnd',lisMenuFindBlockOtherEndOfCodeBlock);
    CreateMenuItem(ParentMI,itmFindBlockStart,'itmFindBlockStart',lisMenuFindCodeBlockStart);
    CreateMenuItem(ParentMI,itmFindDeclaration,'itmFindDeclaration',lisMenuFindDeclarationAtCursor);
    CreateMenuItem(ParentMI,itmOpenFileAtCursor,'itmOpenFileAtCursor',lisMenuOpenFilenameAtCursor);
    CreateMenuItem(ParentMI,itmGotoIncludeDirective,'itmGotoIncludeDirective',lisMenuGotoIncludeDirective);
    CreateMenuItem(ParentMI,itmSearchFindIdentifierRefs,'itmSearchFindIdentifierRefs',lisMenuFindIdentifierRefs);
    CreateMenuItem(ParentMI,itmSearchRenameIdentifier,'itmSearchRenameIdentifier',lisMenuRenameIdentifier);
  end;
end;

procedure TMainIDEBase.SetupViewMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuView,itmViewMainWindows,'itmViewMainWindows');
    ParentMI:=itmViewMainWindows;
    {$ELSE}
    ParentMI:=mnuView;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmViewInspector,'itmViewInspector',lisMenuViewObjectInspector);
    CreateMenuItem(ParentMI,itmViewSourceEditor,'itmViewSourceEditor',lisMenuViewSourceEditor);
    CreateMenuItem(ParentMI,itmViewCodeExplorer,'itmViewCodeExplorer',lisMenuViewCodeExplorer);
    CreateMenuItem(ParentMI,itmViewLazDoc,'itmViewLazDoc',lisMenuLazDoc);   //DBlaszijk 5-sep-05

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuView,itmViewUnitWindows,'itmViewUnitWindows');
    ParentMI:=itmViewUnitWindows;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmViewUnits,'itmViewUnits',lisMenuViewUnits);
    CreateMenuItem(ParentMI,itmViewForms,'itmViewForms',lisMenuViewForms);
    CreateMenuItem(ParentMI,itmViewUnitDependencies,'itmViewUnitDependencies',lisMenuViewUnitDependencies);
    CreateMenuItem(ParentMI,itmViewUnitInfo,'itmViewUnitInfo',lisMenuViewUnitInfo);
    CreateMenuItem(ParentMI,itmViewToggleFormUnit,'itmViewToggleFormUnit',lisMenuViewToggleFormUnit);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuView,itmViewSecondaryWindows,'itmViewSecondaryWindows');
    ParentMI:=itmViewSecondaryWindows;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmViewMessage,'itmViewMessage',lisMenuViewMessages);
    CreateMenuItem(ParentMI,itmViewSearchResults,'itmViewSearchResults',lisMenuViewSearchResults);
    CreateMenuItem(ParentMI,itmViewAnchorEditor,'itmViewAnchorEditor',lisMenuViewAnchorEditor);
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'menu_debugger');
    {$ELSE}
    CreateMenuItem(ParentMI,itmViewDebugWindows,'itmViewDebugWindows',lisMenuDebugWindows,'menu_debugger');
    {$ENDIF}
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
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectNewSection,'itmProjectNewSection');
    ParentMI:=itmProjectNewSection;
    {$ELSE}
    ParentMI:=mnuProject;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmProjectNew,'itmProjectNew',lisMenuNewProject);
    CreateMenuItem(ParentMI,itmProjectNewFromFile,'itmProjectNewFromFile',lisMenuNewProjectFromFile);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectOpenSection,'itmProjectOpenSection');
    ParentMI:=itmProjectOpenSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmProjectOpen,'itmProjectOpen',lisMenuOpenProject,'menu_openproject');
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmProjectRecentOpen,'itmProjectRecentOpen',lisMenuOpenRecentProject);
    {$ELSE}
    CreateMenuItem(ParentMI,itmProjectRecentOpen,'itmProjectRecentOpen',lisMenuOpenRecentProject);
    {$ENDIF}

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectSaveSection,'itmProjectSaveSection');
    ParentMI:=itmProjectSaveSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmProjectSave,'itmProjectSave',lisMenuSaveProject);
    CreateMenuItem(ParentMI,itmProjectSaveAs,'itmProjectSaveAs',lisMenuSaveProjectAs);
    CreateMenuItem(ParentMI,itmProjectPublish,'itmProjectPublish',lisMenuPublishProject);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectWindowSection,'itmProjectWindowSection');
    ParentMI:=itmProjectWindowSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmProjectInspector,'itmProjectInspector',lisMenuProjectInspector,'menu_projectinspector');
    CreateMenuItem(ParentMI,itmProjectOptions,'itmProjectOptions',lisMenuProjectOptions,'menu_projectoptions');
    CreateMenuItem(ParentMI,itmProjectCompilerOptions,'itmProjectCompilerOptions',lisMenuCompilerOptions);
    CreateMenuItem(ParentMI,itmProjectViewToDos,'itmProjectViewToDos',lisMenuViewProjectTodos);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectAddRemoveSection,'itmProjectAddRemoveSection');
    ParentMI:=itmProjectAddRemoveSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmProjectAddTo,'itmProjectAddTo',lisMenuAddToProject);
    CreateMenuItem(ParentMI,itmProjectRemoveFrom,'itmProjectRemoveFrom',lisMenuRemoveFromProject);
    CreateMenuItem(ParentMI,itmProjectViewSource,'itmProjectViewSource',lisMenuViewSource);

    {$IFDEF TRANSLATESTRING}
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuProject,itmProjectPoFileSection,'itmProjectPoFileSection');
    ParentMI:=itmProjectPoFileSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}
    CreateMenuItem(ParentMI, itmProjectCreatePoFiles,'itmProjectCreatePoFiles', lisMenuCreatePoFile);
    CreateMenuItem(ParentMI, itmProjectCollectPoFiles, 'itmProjectCollectPoFiles', lisMenuCollectPoFil);
    {$ENDIF}
  end;
end;

procedure TMainIDEBase.SetupRunMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuRun,itmRunBuilding,'itmRunBuilding');
    ParentMI:=itmRunBuilding;
    {$ELSE}
    ParentMI:=mnuRun;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmRunMenuBuild,'itmRunMenuBuild',lisMenuBuild,'menu_build');
    CreateMenuItem(ParentMI,itmRunMenuBuildAll,'itmRunMenuBuildAll',lisMenuBuildAll,'menu_buildall');
    CreateMenuItem(ParentMI,itmRunMenuAbortBuild,'itmRunMenuAbortBuild',lisMenuAbortBuild);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuRun,itmRunnning,'itmRunnning');
    ParentMI:=itmRunnning;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmRunMenuRun,'itmRunMenuRun',lisMenuProjectRun,'menu_run');
    CreateMenuItem(ParentMI,itmRunMenuPause,'itmRunMenuPause',lisMenuPause,'menu_pause');
    CreateMenuItem(ParentMI,itmRunMenuStepInto,'itmRunMenuStepInto',lisMenuStepInto,'menu_stepinto');
    CreateMenuItem(ParentMI,itmRunMenuStepOver,'itmRunMenuStepOver',lisMenuStepOver,'menu_stepover');
    CreateMenuItem(ParentMI,itmRunMenuRunToCursor,'itmRunMenuRunToCursor',lisMenuRunToCursor);
    CreateMenuItem(ParentMI,itmRunMenuStop,'itmRunMenuStop',lisMenuStop,'');
    CreateMenuItem(ParentMI,itmRunMenuRunParameters,'itmRunMenuRunParameters',lisMenuRunParameters);
    CreateMenuItem(ParentMI,itmRunMenuResetDebugger,'itmRunMenuResetDebugger',lisMenuResetDebugger);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuRun,itmRunBuildingFile,'itmRunBuildingFile');
    ParentMI:=itmRunBuildingFile;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmRunMenuBuildFile,'itmRunMenuBuildFile',lisMenuBuildFile);
    CreateMenuItem(ParentMI,itmRunMenuRunFile,'itmRunMenuRunFile',lisMenuRunFile);
    CreateMenuItem(ParentMI,itmRunMenuConfigBuildFile,'itmRunMenuConfigBuildFile',lisMenuConfigBuildFile);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuRun,itmRunDebugging,'itmRunDebugging');
    ParentMI:=itmRunDebugging;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmRunMenuInspect,'itmRunMenuInspect',lisMenuInspect, '', False);
    CreateMenuItem(ParentMI,itmRunMenuEvaluate,'itmRunMenuEvaluate',lisMenuEvaluate, '', False);
    CreateMenuItem(ParentMI,itmRunMenuAddWatch,'itmRunMenuAddWatch',lisMenuAddWatch, '', False);
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmRunMenuAddBreakpoint,'itmRunMenuAddBreakpoint',lisMenuAddBreakpoint, '');
    {$ELSE}
    CreateMenuItem(ParentMI,itmRunMenuAddBreakpoint,'itmRunMenuAddBreakpoint',lisMenuAddBreakpoint, '');
    {$ENDIF}
      CreateMenuItem(itmRunMenuAddBreakpoint,itmRunMenuAddBPSource,'itmRunMenuAdddBPSource',lisMenuAddBPSource, '', False);
  end;
end;

procedure TMainIDEBase.SetupComponentsMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuComponents,itmPkgOpening,'itmPkgOpening');
    ParentMI:=itmPkgOpening;
    {$ELSE}
    ParentMI:=mnuComponents;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmPkgOpenPackage,'itmPkgOpenPackage',lisMenuOpenPackage,'pkg_package');
    CreateMenuItem(ParentMI,itmPkgOpenPackageFile,'itmPkgOpenPackageFile',lisMenuOpenPackageFile,'pkg_package');
    CreateMenuItem(ParentMI,itmPkgOpenPackageOfCurUnit,'itmPkgOpenPackageOfCurUnit',lisMenuOpenPackageOfCurUnit,'pkg_package');
    {$IFDEF UseMenuIntf}
    CreateMenuSubSection(ParentMI,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg,'pkg_package');
    {$ELSE}
    CreateMenuItem(ParentMI,itmPkgOpenRecent,'itmPkgOpenRecent',lisMenuOpenRecentPkg,'pkg_package');
    {$ENDIF}

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuComponents,itmPkgUnits,'itmPkgUnits');
    ParentMI:=itmPkgUnits;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmPkgAddCurUnitToPkg,'itmPkgAddCurUnitToPkg',lisMenuAddCurUnitToPkg,'pkg_addunittopackage');

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuComponents,itmPkgGraphSection,'itmPkgGraphSection');
    ParentMI:=itmPkgGraphSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmPkgPkgGraph,'itmPkgPkgGraph',lisMenuPackageGraph,'pkg_packagegraph');
    CreateMenuItem(ParentMI,itmPkgEditInstallPkgs,'itmPkgEditInstallPkgs',lisMenuEditInstallPkgs,'pkg_package_install');

    {$IFDEF CustomIDEComps}
    {$IFDEF UseMenuIntf}
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}
    CreateMenuItem(ParentMI,itmCompsConfigCustomComps,'itmCompsConfigCustomComps',lisMenuConfigCustomComps);
    {$ENDIF}
  end;
end;

procedure TMainIDEBase.SetupToolsMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuTools,itmCustomTools,'itmCustomTools');
    ParentMI:=itmCustomTools;
    {$ELSE}
    ParentMI:=mnuTools;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmToolConfigure,'itmToolConfigure',lisMenuSettings);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuTools,itmCodeToolChecks,'itmCodeToolChecks');
    ParentMI:=itmCodeToolChecks;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmToolSyntaxCheck,'itmToolSyntaxCheck',lisMenuQuickSyntaxCheck);
    CreateMenuItem(ParentMI,itmToolGuessUnclosedBlock,'itmToolGuessUnclosedBlock',lisMenuGuessUnclosedBlock);
    CreateMenuItem(ParentMI,itmToolGuessMisplacedIFDEF,'itmToolGuessMisplacedIFDEF',lisMenuGuessMisplacedIFDEF);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuTools,itmSecondaryTools,'itmSecondaryTools');
    ParentMI:=itmSecondaryTools;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmToolMakeResourceString,'itmToolMakeResourceString',lisMenuMakeResourceString);
    CreateMenuItem(ParentMI,itmToolDiff,'itmToolDiff',lisMenuDiff);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuTools,itmDelphiConversion,'itmDelphiConversion');
    ParentMI:=itmDelphiConversion;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmToolCheckLFM,'itmToolCheckLFM',lisMenuCheckLFM);
    CreateMenuItem(ParentMI,itmToolConvertDelphiUnit,'itmToolConvertDelphiUnit',lisMenuConvertDelphiUnit);
    CreateMenuItem(ParentMI,itmToolConvertDelphiProject,'itmToolConvertDelphiProject',lisMenuConvertDelphiProject);
    CreateMenuItem(ParentMI,itmToolConvertDFMtoLFM,'itmToolConvertDFMtoLFM',lisMenuConvertDFMtoLFM);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuTools,itmBuildingLazarus,'itmBuildingLazarus');
    ParentMI:=itmBuildingLazarus;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmToolBuildLazarus,'itmToolBuildLazarus',lisMenuBuildLazarus,'menu_buildlazarus');
    CreateMenuItem(ParentMI,itmToolConfigureBuildLazarus,'itmToolConfigureBuildLazarus',lisMenuConfigureBuildLazarus);
  end;
end;

procedure TMainIDEBase.SetupEnvironmentMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEnvironment,itmOptionsDialogs,'itmOptionsDialogs');
    ParentMI:=itmOptionsDialogs;
    {$ELSE}
    ParentMI:=mnuEnvironment;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEnvGeneralOptions,'itmEnvGeneralOptions',
                   lisMenuGeneralOptions,'menu_environmentoptions');
    CreateMenuItem(ParentMI,itmEnvEditorOptions,'itmEnvEditorOptions',
                   lisMenuEditorOptions,'menu_editoroptions');
    CreateMenuItem(ParentMI,itmEnvCodeTemplates,'itmEnvCodeTemplates',
                   lisMenuEditCodeTemplates,'');
    CreateMenuItem(ParentMI,itmEnvDebuggerOptions,'itmEnvDebuggerOptions',
                   lisMenDebuggerOptions,'');
    CreateMenuItem(ParentMI,itmEnvCodeToolsOptions,'itmEnvCodeToolsOptions',
                   lisMenuCodeToolsOptions,'menu_codetoolsoptions');
    CreateMenuItem(ParentMI,itmEnvCodeToolsDefinesEditor,
                   'itmEnvCodeToolsDefinesEditor',lisMenuCodeToolsDefinesEditor,
                   'menu_codetoolsdefineseditor');

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuEnvironment,itmIDECacheSection,'itmIDECacheSection');
    ParentMI:=itmIDECacheSection;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmEnvRescanFPCSrcDir,'itmEnvRescanFPCSrcDir',
                   lisMenuRescanFPCSourceDirectory);
  end;
end;

procedure TMainIDEBase.SetupWindowsMenu;
begin

end;

procedure TMainIDEBase.SetupHelpMenu;
var
  ParentMI: {$IFDEF UseMenuIntf}TIDEMenuSection{$ELSE}TMenuItem{$ENDIF};
begin
  with MainIDEBar do begin
    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuHelp,itmOnlineHelps,'itmOnlineHelps');
    ParentMI:=itmOnlineHelps;
    {$ELSE}
    ParentMI:=mnuHelp;
    {$ENDIF}

    CreateMenuItem(ParentMI,itmHelpOnlineHelp,'itmHelpOnlineHelp',
                   lisMenuOnlineHelp);
    CreateMenuItem(ParentMI,itmHelpConfigureHelp,'itmHelpConfigureHelp',
                   lisMenuConfigureHelp);

    {$IFDEF UseMenuIntf}
    CreateMenuSeparatorSection(mnuHelp,itmInfoHelps,'itmInfoHelps');
    ParentMI:=itmInfoHelps;
    {$ELSE}
    ParentMI.Add(CreateMenuSeparator);
    {$ENDIF}

    CreateMenuItem(ParentMI,itmHelpAboutLazarus,'itmHelpAboutLazarus',
                   lisMenuAboutLazarus);
  end;
end;

procedure TMainIDEBase.LoadMenuShortCuts;

  {$IFDEF UseMenuIntf}
  function GetCommand(ACommand: word): TIDECommand;
  begin
    Result:=IDECommandList.FindIDECommand(ACommand);
  end;
  {$ENDIF}

begin
  {$IFDEF UseMenuIntf}
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

    // view menu
    itmViewInspector.Command:=GetCommand(ecToggleObjectInsp);
    itmViewSourceEditor.Command:=GetCommand(ecToggleSourceEditor);
    itmViewUnits.Command:=GetCommand(ecViewUnits);
    itmViewCodeExplorer.Command:=GetCommand(ecToggleCodeExpl);
    //itmViewLazDoc.Command:=GetCommand(ecLazDoc);   //DBlaszijk 5-sep-05
    itmViewUnitDependencies.Command:=GetCommand(ecViewUnitDependencies);
    itmViewUnitInfo.Command:=GetCommand(ecViewUnitInfo);
    itmViewForms.Command:=GetCommand(ecViewForms);
    itmViewToggleFormUnit.Command:=GetCommand(ecToggleFormUnit);
    itmViewMessage.Command:=GetCommand(ecToggleMessages);
    itmViewSearchResults.Command:=GetCommand(ecToggleSearchResults);
    itmViewAnchorEditor.Command:=GetCommand(ecViewAnchorEditor);

    // project menu
    itmProjectNew.Command:=GetCommand(ecNewProject);
    itmProjectNewFromFile.Command:=GetCommand(ecNewProjectFromFile);
    itmProjectOpen.Command:=GetCommand(ecOpenProject);
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
    itmHelpConfigureHelp.Command:=GetCommand(ecConfigureHelp);
  end;
  {$ELSE}
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
    itmSetFreeBookmark.ShortCut:=CommandToShortCut(ecSetFreeBookmark);
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
    //itmViewLazDoc.ShortCut:=CommandToShortCut(ecLazDoc);   //DBlaszijk 5-sep-05
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
    itmEnvCodeTemplates.ShortCut:=CommandToShortCut(ecEditCodeTemplates);
    itmEnvCodeToolsOptions.ShortCut:=CommandToShortCut(ecCodeToolsOptions);
    itmEnvCodeToolsDefinesEditor.ShortCut:=CommandToShortCut(ecCodeToolsDefinesEd);
    itmEnvRescanFPCSrcDir.ShortCut:=CommandToShortCut(ecRescanFPCSrcDir);

    // help menu
    itmHelpAboutLazarus.ShortCut:=CommandToShortCut(ecAboutLazarus);
    itmHelpOnlineHelp.ShortCut:=CommandToShortCut(ecOnlineHelp);
    itmHelpConfigureHelp.ShortCut:=CommandToShortCut(ecConfigureHelp);
  end;
  {$ENDIF}
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
        or (FileInfo.Name='')
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
            if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
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
  i: integer;
begin
  Result:=mrOk;
  if EnvironmentOptions.AmbiguousFileAction=afaIgnore then exit;
  if (EnvironmentOptions.AmbiguousFileAction=afaWarnOnCompile)
  and not Compiling then exit;

  if FilenameIsPascalUnit(AFilename) then begin
    Ext:=ExtractFileExt(AFilename);
    LowExt:=lowercase(Ext);
    for i:=Low(PascalFileExt) to High(PascalFileExt) do begin
      if LowExt<>PascalFileExt[i] then begin
        Result:=CheckFile(ChangeFileExt(AFilename,PascalFileExt[i]));
        if Result<>mrOk then exit;
      end;
    end;
  end;
end;

procedure TMainIDEBase.UpdateWindowsMenu;
var
  WindowsList: TList;
  i: Integer;
  CurMenuItem: {$IFDEF UseMenuIntf}TIDEMenuItem{$ELSE}TMenuItem{$ENDIF};
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
      {$IFDEF UseMenuIntf}
      CurMenuItem:=RegisterIDEMenuCommand(MainIDEBar.mnuWindows.GetPath,
                                          'Window'+IntToStr(i),'');
      {$ELSE}
      CurMenuItem:=TMenuItem.Create(MainIDEBar);
      MainIDEBar.mnuWindows.Add(CurMenuItem);
      {$ENDIF}
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

procedure TMainIDEBase.SetRecentSubMenu(
  {$IFDEF UseMenuIntf}Section: TIDEMenuSection;
  {$ELSE}Section: TMenuItem;{$ENDIF}
  FileList: TStringList; OnClickEvent: TNotifyEvent);
var
  i: integer;
  AMenuItem: {$IFDEF UseMenuIntf}TIDEMenuItem{$ELSE}TMenuItem{$ENDIF};
begin
  // create enough menuitems
  while Section.Count<FileList.Count do begin
    {$IFDEF UseMenuIntf}
    AMenuItem:=RegisterIDEMenuCommand(Section.GetPath,
                              Section.Name+'Recent'+IntToStr(Section.Count),'');
    {$ELSE}
    AMenuItem:=TMenuItem.Create(MainIDEBar);
    AMenuItem.Name:=
      Section.Name+'Recent'+IntToStr(Section.Count);
    Section.Add(AMenuItem);
    {$ENDIF}
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

