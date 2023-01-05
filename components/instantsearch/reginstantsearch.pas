unit reginstantsearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, idemcindexer, LazIDEIntf, System.UITypes, ProjectIntf;

Type

  { TInstantSearchHandler }

  TInstantSearchHandler = class(TObject)
    // event handler to rescan FPC directory
    Procedure RescanFPCDir(sender : TObject);
    // Called when user opens project
    function HandleProjectOpen(Sender: TObject; AProject: TLazProject): TModalResult;
    // Called when user closes project
    function HandleProjectClose(Sender: TObject; AProject: TLazProject): TModalResult;
    // Called from project menu
    Procedure MarkProjectIndexable(Sender: TObject);
    // Called from project menu
    Procedure IndexActiveProject(Sender: TObject);
    // Called when project inspector popup menu is shown
    Procedure OnPrjInspPopup(Sender : TObject);
    // Called when project inspector popup menu is shown
    Procedure OnMainmenuPopup(Sender : TObject);
    // Called when source trees definitions change;
    Procedure DoSourceTreesChanged(Sender : TObject);
  end;

procedure Register;

implementation

uses
  IDEOptionsIntf, LCLType, IDECommands, IDEWindowIntf, MenuIntf, IDEOptEditorIntf,
  frmInstantSearch, instantsearchstrings, frainstantsearchoptions, IDEExternToolIntf,
  ideinstantsearch;

var
  IDEInstantSearchOptionsFrameID: integer = 2000;
  InstantSearchHandler : TInstantSearchHandler;
  PopupMenuCmdIndex : TIDEMenuCommand;
  PopupMenuCmdMarkIndexable : TIDEMenuCommand;
  MainMenuCmdIndex : TIDEMenuCommand;
  MainMenuCmdMarkIndexable : TIDEMenuCommand;


procedure Register;

var
  CmdCatInstantSearch: TIDECommandCategory;
  ViewInstantSearchFormCommand: TIDECommand;
  CmdIndexProject,
  CmdMarkIndexable : TIDECommand;
  MenuItemCaption: String;

begin
  TInstantSearchForm.OptionsFrameClass:=TIDEInstantSearchOptionsFrame;

  IDEInstantSearchManager.CreateDefaultTrees;
  IDEInstantSearchManager.Load;

  InstantSearchHandler:=TInstantSearchHandler.Create;
  LazarusIDE.AddHandlerFPCSrcDirScanned(@InstantSearchHandler.RescanFPCDir,True);
  LazarusIDE.AddHandlerOnProjectOpened(@InstantSearchHandler.HandleProjectOpen,True);
  LazarusIDE.AddHandlerOnProjectClose(@InstantSearchHandler.HandleProjectClose);
  IDEInstantSearchManager.OnSourceTreesChanged:=@InstantSearchHandler.DoSourceTreesChanged;

  //  LazarusIDE.AddHandlerPr
  // register shortcut and menu item
  MenuItemCaption:=lrsInstantSearchMenu;
  // search shortcut category
  CmdCatInstantSearch:=RegisterIDECommandCategory(Nil,'InstantSearch',lrsInstantSearch);


  // register shortcut
  ViewInstantSearchFormCommand:=RegisterIDECommand(CmdCatInstantSearch,
    'ViewInstantSearchForm',
    MenuItemCaption,
    IDEShortCut(VK_F, [ssCtrl,ssAlt]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowInstantSearchForm);
  // register menu item in View menu
   RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewInstantSearchForm',
    MenuItemCaption, nil, nil, ViewInstantSearchFormCommand);

  // register dockable Window
  InstantSearchFormCreator:=IDEWindowCreators.Add(
    'InstantSearchForm',
    @CreateInstantSearchForm, nil,
    '100', '100', '300', '300'  // default place at left=100, top=100, right=300, bottom=300
      // you can also define percentage values of screen or relative positions, see wiki
    );

    // add IDE options frame
  IDEInstantSearchOptionsFrameID:=RegisterIDEOptionsEditor(GroupEnvironment,TIDEInstantSearchOptionsFrame,
                                                IDEInstantSearchOptionsFrameID)^.Index;

  // Commands
  CmdMarkIndexable:=RegisterIDECommand(CmdCatInstantSearch ,'InstantSearchMarkIndexable',
    lrsMarkProjectIndexable,
    IDEShortCut(VK_M, [ssCtrl,ssAlt]), // <- set here your default shortcut
    CleanIDEShortCut,
    @InstantSearchHandler.MarkProjectIndexable);

  CmdIndexProject:=RegisterIDECommand(CmdCatInstantSearch ,'InstantSearchIndex',
    lrsIndexProject,
    IDEShortCut(VK_I, [ssCtrl,ssAlt]), // <- set here your default shortcut
    CleanIDEShortCut,
    @InstantSearchhandler.IndexActiveProject);

  // Project inspector menu items
  PopupMenuCmdMarkIndexable:=RegisterIDEMenuCommand(ProjectInspectorItemsMenuRoot,'InstantSearchMarkIndexable'+'ProjectInspector',
    lrsMarkProjectIndexable,
    Nil,Nil,CmdMarkIndexable);
  PopupMenuCmdIndex:=RegisterIDEMenuCommand(ProjectInspectorItemsMenuRoot,'InstantSearchIndex'+'ProjectInspector',
    lrsIndexProject,
    Nil,Nil,CmdIndexProject);
  // Register handler to update visibility
  ProjectInspectorItemsMenuRoot.AddHandlerOnShow(@InstantSearchHandler.OnPrjInspPopup);

  // Main project menu items
  MainMenuCmdMarkIndexable:=RegisterIDEMenuCommand(mnuProject,'MarkInstantSearchIndexable'+'MainMenu',
    lrsMarkProjectIndexable,
    Nil,Nil,CmdMarkIndexable);

  MainMenuCmdIndex:=RegisterIDEMenuCommand(mnuProject,'InstantSearchIndex'+'MainMenu',
    lrsIndexProject,
    Nil,Nil,CmdIndexProject);
  // Register handler to update visibility
  mnuProject.AddHandlerOnShow(@InstantSearchHandler.OnMainMenuPopup);

end;

{ TInstantSearchHandler }

procedure TInstantSearchHandler.RescanFPCDir(sender: TObject);
begin
  IDEInstantSearchManager.RescanFPCDir;
end;

function TInstantSearchHandler.HandleProjectOpen(Sender: TObject;
  AProject: TLazProject): TModalResult;

Var
  aTreeID : String;

begin
  Result:=mrOK;
  aTreeID:=aProject.CustomSessionData[SInstantSearchID];
  if (aTreeID='') then
    begin
    if IDEInstantSearchManager.IndexProjectStrategy=ipsAll then
      aTreeID:=IDEInstantSearchManager.AssignProjectTreeID(aProject)
    else
      begin
      if IDEInstantSearchManager.IndexProjectStrategy=ipsTimed then
        IDEInstantSearchManager.StartMarkProjectTimer(aProject);
      end;
    end;
  IDEInstantSearchManager.ProjectTreeName:=aTreeID;
  IDEInstantSearchManager.SearchProject:=(aTreeID<>'');
  if Assigned(InstantSearchForm) then
    InstantSearchForm.ActiveProjectChanged;
end;

function TInstantSearchHandler.HandleProjectClose(Sender: TObject;
  AProject: TLazProject): TModalResult;
begin
  Result:=mrOK;
  IDEInstantSearchManager.ClearMarkProjectTimer;
end;

procedure TInstantSearchHandler.MarkProjectIndexable(Sender: TObject);
begin
  IDEInstantSearchManager.MarkProjectIndexed(LazarusIDE.ActiveProject);
end;

procedure TInstantSearchHandler.IndexActiveProject(Sender: TObject);
begin
  if assigned(LazarusIDE.ActiveProject) then
    IDEInstantSearchManager.IndexProjectFiles(LazarusIDE.ActiveProject);
end;

procedure TInstantSearchHandler.OnPrjInspPopup(Sender: TObject);

var
  hasID : Boolean;

begin
  HasID:=LazarusIDE.ActiveProject.CustomSessionData[SInstantSearchID] <>'';
  PopupMenuCmdMarkIndexable.Visible:=Not HasID;
  PopupMenuCmdIndex.Visible:=HasID;
end;

procedure TInstantSearchHandler.OnMainmenuPopup(Sender: TObject);
var
  hasID : Boolean;

begin
  HasID:=LazarusIDE.ActiveProject.CustomSessionData[SInstantSearchID] <>'';
  MainMenuCmdMarkIndexable.Visible:=Not HasID;
  MainMenuCmdIndex.Visible:=HasID;
end;

procedure TInstantSearchHandler.DoSourceTreesChanged(Sender: TObject);
begin
  if Assigned(InstantSearchForm) then
    InstantSearchForm.TreesChanged;
end;

finalization
  FreeAndNil(InstantSearchHandler);

end.

