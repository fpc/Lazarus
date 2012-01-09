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
  Classes, SysUtils, LCLProc, StdCtrls, Forms, Controls, Buttons, Menus,
  ComCtrls, ExtCtrls, Dialogs, LMessages,
  // IDEIntf
  ProjectIntf, NewItemIntf, MenuIntf, LazIDEIntf,
  EnvironmentOpts, LazarusIDEStrConsts;

type
  { TMainIDEBar }

  TMainIDEBar = class(TForm)
  
    // the speedbuttons panel for frequently used IDE functions
    pnlSpeedButtons      : TPanel;
    tbStandard           : TToolBar;
      NewUnitSpeedBtn      : TToolButton;
      NewFormSpeedBtn      : TToolButton;
      tbDivider1           : TToolButton;
      OpenFileSpeedBtn     : TToolButton;
      OpenFilePopUpMenu    : TPopupMenu;
      SaveSpeedBtn         : TToolButton;
      SaveAllSpeedBtn      : TToolButton;
      tbDivider2           : TToolButton;
      ToggleFormSpeedBtn   : TToolButton;
    tbViewDebug            : TToolBar;
      ViewUnitsSpeedBtn    : TToolButton;
      ViewFormsSpeedBtn    : TToolButton;
      tbDivider3           : TToolButton;
      BuildModeSpeedButton : TToolButton;
      RunSpeedButton       : TToolButton;
      SetBuildModePopupMenu: TPopupMenu;
      PauseSpeedButton     : TToolButton;
      StopSpeedButton      : TToolButton;
      StepIntoSpeedButton  : TToolButton;
      StepOverSpeedButton  : TToolButton;
      StepOutSpeedButton   : TToolButton;

    NewUnitFormPopupMenu : TPopupMenu;
      NewUFSetDefaultMenuItem: TMenuItem;

    // MainMenu
    mnuMainMenu: TMainMenu;

    //mnuMain: TIDEMenuSection;

    // file menu
    //mnuFile: TIDEMenuSection;
      //itmFileNew: TIDEMenuSection;
        itmFileNewUnit: TIDEMenuCommand;
        itmFileNewForm: TIDEMenuCommand;
        itmFileNewOther: TIDEMenuCommand;
      //itmFileOpenSave: TIDEMenuSection;
        itmFileOpen: TIDEMenuCommand;
        itmFileRevert: TIDEMenuCommand;
        //itmFileRecentOpen: TIDEMenuSection;
        itmFileSave: TIDEMenuCommand;
        itmFileSaveAs: TIDEMenuCommand;
        itmFileSaveAll: TIDEMenuCommand;
        itmFileClose: TIDEMenuCommand;
        itmFileCloseAll: TIDEMenuCommand;
      //itmFileDirectories: TIDEMenuSection;
        itmFileCleanDirectory: TIDEMenuCommand;
      //itmFileIDEStart: TIDEMenuSection;
        itmFileRestart: TIDEMenuCommand;
        itmFileQuit: TIDEMenuCommand;

    // edit menu
    //mnuEdit: TIDEMenuSection;
      //itmEditReUndo: TIDEMenuSection;
        itmEditUndo: TIDEMenuCommand;
        itmEditRedo: TIDEMenuCommand;
      //itmEditClipboard: TIDEMenuSection;
        itmEditCut: TIDEMenuCommand;
        itmEditCopy: TIDEMenuCommand;
        itmEditPaste: TIDEMenuCommand;
      //itmEditSelect: TIDEMenuSection;
        itmEditSelectAll: TIDEMenuCommand;
        itmEditSelectToBrace: TIDEMenuCommand;
        itmEditSelectCodeBlock: TIDEMenuCommand;
        itmEditSelectWord: TIDEMenuCommand;
        itmEditSelectLine: TIDEMenuCommand;
        itmEditSelectParagraph: TIDEMenuCommand;
      //itmEditBlockActions: TIDEMenuSection;
        itmEditIndentBlock: TIDEMenuCommand;
        itmEditUnindentBlock: TIDEMenuCommand;
        itmEditUpperCaseBlock: TIDEMenuCommand;
        itmEditLowerCaseBlock: TIDEMenuCommand;
        itmEditSwapCaseBlock: TIDEMenuCommand;
        itmEditSortBlock: TIDEMenuCommand;
        itmEditTabsToSpacesBlock: TIDEMenuCommand;
        itmEditSelectionBreakLines: TIDEMenuCommand;
      //itmEditInsertions: TIDEMenuSection;
        itmEditInsertCharacter: TIDEMenuCommand;

    // search menu
    //mnuSearch: TIDEMenuSection;
      //itmSearchFindReplace: TIDEMenuSection;
        itmSearchFind: TIDEMenuCommand;
        itmSearchFindNext: TIDEMenuCommand;
        itmSearchFindPrevious: TIDEMenuCommand;
        itmSearchFindInFiles: TIDEMenuCommand;
        itmSearchReplace: TIDEMenuCommand;
        itmIncrementalFind: TIDEMenuCommand;
      //itmJumpings: TIDEMenuSection;
        itmGotoLine: TIDEMenuCommand;
        itmJumpBack: TIDEMenuCommand;
        itmJumpForward: TIDEMenuCommand;
        itmAddJumpPoint: TIDEMenuCommand;
        itmJumpToNextError: TIDEMenuCommand;
        itmJumpToPrevError: TIDEMenuCommand;
      //itmBookmarks: TIDEMenuSection;
        itmSetFreeBookmark: TIDEMenuCommand;
        itmJumpToNextBookmark: TIDEMenuCommand;
        itmJumpToPrevBookmark: TIDEMenuCommand;
      //itmCodeToolSearches: TIDEMenuSection;
        itmFindDeclaration: TIDEMenuCommand;
        itmFindBlockOtherEnd: TIDEMenuCommand;
        itmFindBlockStart: TIDEMenuCommand;
        itmOpenFileAtCursor: TIDEMenuCommand;
        itmGotoIncludeDirective: TIDEMenuCommand;
        itmSearchFindIdentifierRefs: TIDEMenuCommand;
        itmSearchProcedureList: TIDEMenuCommand;

    // view menu
    //mnuView: TIDEMenuSection;
      //itmViewMainWindows: TIDEMenuSection;
        itmViewInspector: TIDEMenuCommand;
        itmViewSourceEditor: TIDEMenuCommand;
        itmViewCodeExplorer: TIDEMenuCommand;
        itmViewFPDocEditor: TIDEMenuCommand;
        itmViewCodeBrowser: TIDEMenuCommand;
        itmViewRestrictionBrowser: TIDEMenuCommand;
        itmViewComponents: TIDEMenuCommand;
        itmJumpHistory: TIDEMenuCommand;
      //itmViewUnitWindows: TIDEMenuSection;
        itmViewUnitDependencies: TIDEMenuCommand;
        itmViewToggleFormUnit: TIDEMenuCommand;
      //itmViewSecondaryWindows: TIDEMenuSection;
        itmViewAnchorEditor: TIDEMenuCommand;
        itmViewTabOrder: TIDEMenuCommand;
        itmViewComponentPalette: TIDEMenuCommand;
        itmViewIDESpeedButtons: TIDEMenuCommand;
        itmViewMessage: TIDEMenuCommand;
        itmViewSearchResults: TIDEMenuCommand;
        //itmViewDebugWindows: TIDEMenuSection;
          itmViewWatches: TIDEMenuCommand;
          itmViewBreakpoints: TIDEMenuCommand;
          itmViewLocals: TIDEMenuCommand;
          itmViewRegisters: TIDEMenuCommand;
          itmViewCallStack: TIDEMenuCommand;
          itmViewThreads: TIDEMenuCommand;
          itmViewAssembler: TIDEMenuCommand;
          itmViewDebugOutput: TIDEMenuCommand;
          itmViewDebugEvents: TIDEMenuCommand;
          itmViewPseudoTerminal: TIDEMenuCommand;
          itmViewDbgHistory: TIDEMenuCommand;
        //itmViewIDEInternalsWindows: TIDEMenuSection;
          itmViewPackageLinks: TIDEMenuCommand;
          itmViewFPCInfo: TIDEMenuCommand;
          itmViewIDEInfo: TIDEMenuCommand;
          itmViewIDEModifiedInfo: TIDEMenuCommand;

    // source menu
    //mnuSource: TIDEMenuSection;
      //itmSourceBlockActions: TIDEMenuSection;
        itmSourceCommentBlock: TIDEMenuCommand;
        itmSourceUncommentBlock: TIDEMenuCommand;
        itmSourceToggleComment: TIDEMenuCommand;
        itmSourceEncloseBlock: TIDEMenuCommand;
        itmSourceEncloseInIFDEF: TIDEMenuCommand;
        itmSourceCompleteCode: TIDEMenuCommand;
        itmSourceUseUnit: TIDEMenuCommand;
      //itmSourceCodeToolChecks: TIDEMenuSection;
        itmSourceSyntaxCheck: TIDEMenuCommand;
        itmSourceGuessUnclosedBlock: TIDEMenuCommand;
        itmSourceGuessMisplacedIFDEF: TIDEMenuCommand;
      //itmSourceInsertCVSKeyWord: TIDEMenuSection;
        itmSourceInsertCVSAuthor: TIDEMenuCommand;
        itmSourceInsertCVSDate: TIDEMenuCommand;
        itmSourceInsertCVSHeader: TIDEMenuCommand;
        itmSourceInsertCVSID: TIDEMenuCommand;
        itmSourceInsertCVSLog: TIDEMenuCommand;
        itmSourceInsertCVSName: TIDEMenuCommand;
        itmSourceInsertCVSRevision: TIDEMenuCommand;
        itmSourceInsertCVSSource: TIDEMenuCommand;
      //itmSourceInsertGeneral: TIDEMenuSection;
        itmSourceInsertGPLNotice: TIDEMenuCommand;
        itmSourceInsertLGPLNotice: TIDEMenuCommand;
        itmSourceInsertModifiedLGPLNotice: TIDEMenuCommand;
        itmSourceInsertUsername: TIDEMenuCommand;
        itmSourceInsertDateTime: TIDEMenuCommand;
        itmSourceInsertChangeLogEntry: TIDEMenuCommand;
        itmSourceInsertGUID: TIDEMenuCommand;
        itmSourceInsertTodo: TIDEMenuCommand;
      itmSourceInsertFilename: TIDEMenuCommand;
    // itmSourceTools
      itmSourceUnitInfo: TIDEMenuCommand;

    // refactor menu
    //mnuRefactor: TIDEMenuSection;
      //itmRefactorCodeTools: TIDEMenuSection;
        itmRefactorRenameIdentifier: TIDEMenuCommand;
        itmRefactorExtractProc: TIDEMenuCommand;
        itmRefactorInvertAssignment: TIDEMenuCommand;
      //itmRefactorAdvanced: TIDEMenuSection;
        itmRefactorShowAbstractMethods: TIDEMenuCommand;
        itmRefactorShowEmptyMethods: TIDEMenuCommand;
        itmRefactorShowUnusedUnits: TIDEMenuCommand;
        itmRefactorFindOverloads: TIDEMenuCommand;
      //itmRefactorTools: TIDEMenuSection;
        itmRefactorMakeResourceString: TIDEMenuCommand;

    // project menu
    //mnuProject: TIDEMenuSection;
      //itmProjectNewSection: TIDEMenuSection;
        itmProjectNew: TIDEMenuCommand;
        itmProjectNewFromFile: TIDEMenuCommand;
      //itmProjectOpenSection: TIDEMenuSection;
        itmProjectOpen: TIDEMenuCommand;
        //itmProjectRecentOpen: TIDEMenuSection;
        itmProjectClose: TIDEMenuCommand;
      //itmProjectSaveSection: TIDEMenuSection;
        itmProjectSave: TIDEMenuCommand;
        itmProjectSaveAs: TIDEMenuCommand;
        itmProjectPublish: TIDEMenuCommand;
      //itmProjectWindowSection: TIDEMenuSection;
        itmProjectInspector: TIDEMenuCommand;
        itmProjectOptions: TIDEMenuCommand;
        //itmProjectCompilerOptions: TIDEMenuCommand;
      //itmProjectAddRemoveSection: TIDEMenuSection;
        itmProjectAddTo: TIDEMenuCommand;
        itmProjectRemoveFrom: TIDEMenuCommand;
        itmProjectViewUnits: TIDEMenuCommand;
        itmProjectViewForms: TIDEMenuCommand;
        itmProjectViewSource: TIDEMenuCommand;

    // run menu
    //mnuRun: TIDEMenuSection;
      //itmRunBuilding: TIDEMenuSection;
        itmRunMenuCompile: TIDEMenuCommand;
        itmRunMenuBuild: TIDEMenuCommand;
        itmRunMenuQuickCompile: TIDEMenuCommand;
        itmRunMenuCleanUpCompiled: TIDEMenuCommand;
        itmRunMenuAbortBuild: TIDEMenuCommand;
      //itmRunnning: TIDEMenuSection;
        itmRunMenuRun: TIDEMenuCommand;
        itmRunMenuPause: TIDEMenuCommand;
        itmRunMenuShowExecutionPoint: TIDEMenuCommand;
        itmRunMenuStepInto: TIDEMenuCommand;
        itmRunMenuStepOver: TIDEMenuCommand;
        itmRunMenuStepOut: TIDEMenuCommand;
        itmRunMenuRunToCursor: TIDEMenuCommand;
        itmRunMenuStop: TIDEMenuCommand;
        itmRunMenuRunParameters: TIDEMenuCommand;
        itmRunMenuResetDebugger: TIDEMenuCommand;
      //itmRunBuildingFile: TIDEMenuSection;
        itmRunMenuBuildFile: TIDEMenuCommand;
        itmRunMenuRunFile: TIDEMenuCommand;
        itmRunMenuConfigBuildFile: TIDEMenuCommand;
      //itmRunDebugging: TIDEMenuSection;
        itmRunMenuInspect: TIDEMenuCommand;
        itmRunMenuEvaluate: TIDEMenuCommand;
        itmRunMenuAddWatch: TIDEMenuCommand;
        //itmRunMenuAddBreakpoint: TIDEMenuSection;
          itmRunMenuAddBpSource: TIDEMenuCommand;
          itmRunMenuAddBpAddress: TIDEMenuCommand;
          itmRunMenuAddBpWatchPoint: TIDEMenuCommand;

    // packages menu
    //mnuComponents: TIDEMenuSection;
      //itmPkgOpening: TIDEMenuSection;
        itmPkgNewPackage: TIDEMenuCommand;
        itmPkgOpenPackage: TIDEMenuCommand;
        itmPkgOpenPackageFile: TIDEMenuCommand;
        itmPkgOpenPackageOfCurUnit: TIDEMenuCommand;
        //itmPkgOpenRecent: TIDEMenuSection;
      //itmPkgUnits: TIDEMenuSection;
        itmPkgAddCurFileToPkg: TIDEMenuCommand;
      //itmPkgGraphSection: TIDEMenuSection;
        itmPkgPkgGraph: TIDEMenuCommand;
        itmPkgEditInstallPkgs: TIDEMenuCommand;
        {$IFDEF CustomIDEComps}
        itmCompsConfigCustomComps: TIDEMenuCommand;
        {$ENDIF}

    // tools menu
    //mnuTools: TIDEMenuSection;
      //itmOptionsDialogs: TIDEMenuSection;
        itmEnvGeneralOptions: TIDEMenuCommand;
        itmToolRescanFPCSrcDir: TIDEMenuCommand;
        itmEnvCodeTemplates: TIDEMenuCommand;
        itmEnvCodeToolsDefinesEditor: TIDEMenuCommand;
      //itmCustomTools: TIDEMenuSection;
        itmToolConfigure: TIDEMenuCommand;
      //itmSecondaryTools: TIDEMenuSection;
        itmToolDiff: TIDEMenuCommand;
      //itmDelphiConversion: TIDEMenuSection;
        itmToolCheckLFM: TIDEMenuCommand;
        itmToolConvertDelphiUnit: TIDEMenuCommand;
        itmToolConvertDelphiProject: TIDEMenuCommand;
        itmToolConvertDelphiPackage: TIDEMenuCommand;
        itmToolConvertDFMtoLFM: TIDEMenuCommand;
        itmToolConvertEncoding: TIDEMenuCommand;
      //itmBuildingLazarus: TIDEMenuSection;
        itmToolManageExamples: TIDEMenuCommand;
        itmToolBuildLazarus: TIDEMenuCommand;
        itmToolConfigureBuildLazarus: TIDEMenuCommand;

    // windows menu
    //mnuWindow: TIDEMenuSection;

    // help menu
    //mnuHelp: TIDEMenuSection;
      //itmOnlineHelps: TIDEMenuSection;
        itmHelpOnlineHelp: TIDEMenuCommand;
        itmHelpReportingBug: TIDEMenuCommand;
        //itmHelpConfigureHelp: TIDEMenuCommand;
      //itmInfoHelps: TIDEMenuSection;
        itmHelpAboutLazarus: TIDEMenuCommand;
      //itmHelpTools: TIDEMenuSection;

    // component palette
    ComponentPageControl: TPageControl;
    GlobalMouseSpeedButton: TSpeedButton;
  private
    FOldWindowState: TWindowState;
    FOnActive: TNotifyEvent;
    procedure NewUFDefaultClick(Sender: TObject);
    procedure NewUnitFormPopupMenuPopup(Sender: TObject);
  protected
    procedure DoActive;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure HideIDE;
    procedure UnhideIDE;
    procedure CreatePopupMenus(TheOwner: TComponent);
    property OnActive: TNotifyEvent read FOnActive write FOnActive;
  end;

var
  MainIDEBar: TMainIDEBar;

implementation

{ TMainIDEBar }

procedure TMainIDEBar.NewUFDefaultClick(Sender: TObject);
var
  Category: TNewIDEItemCategory;
  i: Integer;
  Item: TMenuItem;
  Template: TNewIDEItemTemplate;
begin
  Item:=Sender as TMenuItem;
  Category:=NewIDEItems.FindCategoryByPath(FileDescGroupName,true);
  i:=Item.MenuIndex;
  if (i<0) or (i>=Category.Count) then exit;
  Template:=Category[i];
  if NewUnitFormPopupMenu.Tag=1 then
    EnvironmentOptions.NewUnitTemplate:=Template.Name
  else
    EnvironmentOptions.NewFormTemplate:=Template.Name;
  //DebugLn(['TMainIDEBar.NewUFDefaultClick ',Template.Name]);
  EnvironmentOptions.Save(False);
end;

procedure TMainIDEBar.NewUnitFormPopupMenuPopup(Sender: TObject);
var
  TemplateName: String;
  Category: TNewIDEItemCategory;
  i: Integer;
  CurTemplate: TNewIDEItemTemplate;
  Index: Integer;
  Item: TMenuItem;
begin
  Category:=NewIDEItems.FindCategoryByPath(FileDescGroupName,true);
  // find default template name
  if NewUnitFormPopupMenu.PopupComponent=NewUnitSpeedBtn then begin
    TemplateName:=EnvironmentOptions.NewUnitTemplate;
    if (TemplateName='') or (Category.FindTemplateByName(TemplateName)=nil) then
      TemplateName:=FileDescNamePascalUnit;
    NewUnitFormPopupMenu.Tag:=1;
  end else begin
    TemplateName:=EnvironmentOptions.NewFormTemplate;
    if (TemplateName='') or (Category.FindTemplateByName(TemplateName)=nil) then
      TemplateName:=FileDescNameLCLForm;
    NewUnitFormPopupMenu.Tag:=2;
  end;
  // create menu items
  Index:=0;
  for i:=0 to Category.Count-1 do begin
    CurTemplate:=Category[i];
    if not CurTemplate.VisibleInNewDialog then continue;
    if Index<NewUFSetDefaultMenuItem.Count then
      Item:=NewUFSetDefaultMenuItem[Index]
    else begin
      Item:=TMenuItem.Create(NewUFSetDefaultMenuItem);
      Item.Name:='NewUFSetDefaultMenuItem'+IntToStr(Index);
      Item.OnClick:=@NewUFDefaultClick;
      NewUFSetDefaultMenuItem.Add(Item);
    end;
    Item.Caption:=CurTemplate.LocalizedName;
    Item.ShowAlwaysCheckable:=true;
    Item.Checked:=SysUtils.CompareText(TemplateName,CurTemplate.Name)=0;
    inc(Index);
  end;
  // remove unneeded items
  while NewUFSetDefaultMenuItem.Count>Index do
    NewUFSetDefaultMenuItem.Items[NewUFSetDefaultMenuItem.Count-1].Free;
end;

procedure TMainIDEBar.DoActive;
begin
  if Assigned(FOnActive) then
    FOnActive(Self);
end;

procedure TMainIDEBar.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);
  if (Message.Msg=LM_ACTIVATE) and (Message.Result=0) then
    DoActive;
end;

constructor TMainIDEBar.Create(TheOwner: TComponent);
begin
  // This form has no resource => must be constructed using CreateNew
  inherited CreateNew(TheOwner, 1);
end;

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

procedure TMainIDEBar.CreatePopupMenus(TheOwner: TComponent);
begin
  // create the popupmenu for the MainIDEBar.OpenFileArrowSpeedBtn
  OpenFilePopUpMenu := TPopupMenu.Create(TheOwner);
  OpenFilePopupMenu.Name:='OpenFilePopupMenu';

  SetBuildModePopupMenu:=TPopupMenu.Create(TheOwner);
  SetBuildModePopupMenu.Name:='SetBuildModePopupMenu';

  NewUnitFormPopupMenu:=TPopupMenu.Create(TheOwner);
  NewUnitFormPopupMenu.Name:='NewUnitFormPopupMenu';
  NewUnitFormPopupMenu.OnPopup:=@NewUnitFormPopupMenuPopup;
  NewUnitSpeedBtn.PopupMenu := NewUnitFormPopupMenu;
  NewFormSpeedBtn.PopupMenu := NewUnitFormPopupMenu;
  NewUFSetDefaultMenuItem:=TMenuItem.Create(TheOwner);
  NewUFSetDefaultMenuItem.Name:='NewUFSetDefaultMenuItem';
  NewUFSetDefaultMenuItem.Caption:=lisSetDefault;
  NewUnitFormPopupMenu.Items.Add(NewUFSetDefaultMenuItem);
end;

end.

