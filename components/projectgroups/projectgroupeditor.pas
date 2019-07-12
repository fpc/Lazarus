{
  Todo:
    - activate project when project is opened
    - deactivate project when project is closed
    - show active build mode
    - auto load last group on IDE start when option enabled
}
unit ProjectGroupEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, LCLProc, Clipbrd, ImgList,
  // LazUtils
  LazFileUtils, LazLoggerBase, LazFileCache,
  // IdeIntf
  LazIDEIntf, PackageIntf, ProjectIntf, ProjectGroupIntf, MenuIntf, IDEWindowIntf,
  IDEDialogs, IDECommands, IDEImagesIntf,
  // ProjectGroups
  ProjectGroupStrConst, ProjectGroup, PrjGrpOptionsFrm, PrjGrpInfoFrm;

type
  TNodeType = (
    ntUnknown,
    ntProjectGroup,
    ntTarget,
    ntMissingTarget,
    ntBuildModes,
    ntBuildMode,
    ntFiles,
    ntFile,
    ntDependencies,
    ntDependency
    );

  TNodeData = class(TObject)
    NodeType: TNodeType;
    Target, ParentTarget: TPGCompileTarget;
    Value: string; // ntFile = Filename, ntDependency = PkgName, ntBuildMode = BuildMode name
  end;

  { TProjectGroupEditorForm }

  TProjectGroupEditorForm = class(TForm)
    AProjectGroupNew: TAction;
    AProjectGroupAddCurrent: TAction;
    ATargetInfo: TAction;
    AProjectGroupOptions: TAction;
    AProjectGroupRedo: TAction;
    AProjectGroupUndo: TAction;
    AProjectGroupReload: TAction;
    ATargetCompileFromHere: TAction;
    ATargetCopyFilename: TAction;
    AProjectGroupAddExisting: TAction;
    ATargetCompile: TAction;
    ATargetCompileClean: TAction;
    AProjectGroupAddNew: TAction;
    ATargetActivate: TAction;
    ATargetOpen: TAction;
    AProjectGroupSaveAs: TAction;
    ATargetUninstall: TAction;
    ATargetInstall: TAction;
    ATargetRun: TAction;
    ATargetProperties: TAction;
    ATargetLater: TAction;
    ATargetEarlier: TAction;
    AProjectGroupDelete: TAction;
    AProjectGroupSave: TAction;
    ActionListMain: TActionList;
    PMINew: TMenuItem;
    PMIAddExisting: TMenuItem;
    PMIAddCurrent: TMenuItem;
    PMIInfo: TMenuItem;
    PMIOptions: TMenuItem;
    PMIRedo: TMenuItem;
    PMIUndo: TMenuItem;
    PMICompileFromHere: TMenuItem;
    PMIRunMenuItem: TMenuItem;
    PMICopyFilenameMenuItem: TMenuItem;
    PMIOpen: TMenuItem;
    PMISaveAs: TMenuItem;
    PMIProperties: TMenuItem;
    PMILater: TMenuItem;
    PMIEarlier: TMenuItem;
    PMIDelete: TMenuItem;
    PMICompileClean: TMenuItem;
    PMICompile: TMenuItem;
    OpenDialogTarget: TOpenDialog;
    PopupMenuAdd: TPopupMenu;
    PopupMenuMore: TPopupMenu;
    PopupMenuTree: TPopupMenu;
    SBPG: TStatusBar;
    TBProjectGroup: TToolBar;
    TBSave: TToolButton;
    TBAdd: TToolButton;
    TBNewTarget: TToolButton;
    TBDelete: TToolButton;
    TBCompile: TToolButton;
    TBCompileClean: TToolButton;
    ToolButton1: TToolButton;
    TBTargetUp: TToolButton;
    TBTargetLater: TToolButton;
    TBMore: TToolButton;
    TBActivate: TToolButton;
    TBReload: TToolButton;
    TVPG: TTreeView;
    procedure AProjectGroupAddCurrentExecute(Sender: TObject);
    procedure AProjectGroupAddCurrentUpdate(Sender: TObject);
    procedure AProjectGroupAddExistingExecute(Sender: TObject);
    procedure AProjectGroupAddExistingUpdate(Sender: TObject);
    procedure AProjectGroupDeleteExecute(Sender: TObject);
    procedure AProjectGroupDeleteUpdate(Sender: TObject);
    procedure AProjectGroupNewExecute(Sender: TObject);
    procedure AProjectGroupOptionsExecute(Sender: TObject);
    procedure AProjectGroupRedoExecute(Sender: TObject);
    procedure AProjectGroupRedoUpdate(Sender: TObject);
    procedure AProjectGroupReloadExecute(Sender: TObject);
    procedure AProjectGroupSaveAsExecute(Sender: TObject);
    procedure AProjectGroupSaveAsUpdate(Sender: TObject);
    procedure AProjectGroupSaveExecute(Sender: TObject);
    procedure AProjectGroupSaveUpdate(Sender: TObject);
    procedure AProjectGroupUndoExecute(Sender: TObject);
    procedure AProjectGroupUndoUpdate(Sender: TObject);
    procedure ATargetActivateExecute(Sender: TObject);
    procedure ATargetActivateUpdate(Sender: TObject);
    procedure ATargetCompileCleanExecute(Sender: TObject);
    procedure ATargetCompileCleanUpdate(Sender: TObject);
    procedure ATargetCompileExecute(Sender: TObject);
    procedure ATargetCompileFromHereExecute(Sender: TObject);
    procedure ATargetCompileFromHereUpdate(Sender: TObject);
    procedure ATargetCompileUpdate(Sender: TObject);
    procedure ATargetCopyFilenameExecute(Sender: TObject);
    procedure ATargetCopyFilenameUpdate(Sender: TObject);
    procedure ATargetEarlierExecute(Sender: TObject);
    procedure ATargetEarlierUpdate(Sender: TObject);
    procedure ATargetInfoExecute(Sender: TObject);
    procedure ATargetInfoUpdate(Sender: TObject);
    procedure ATargetInstallExecute(Sender: TObject);
    procedure ATargetInstallUpdate(Sender: TObject);
    procedure ATargetLaterExecute(Sender: TObject);
    procedure ATargetLaterUpdate(Sender: TObject);
    procedure ATargetOpenExecute(Sender: TObject);
    procedure ATargetOpenUpdate(Sender: TObject);
    procedure ATargetPropertiesExecute(Sender: TObject);
    procedure ATargetPropertiesUpdate(Sender: TObject);
    procedure ATargetRunExecute(Sender: TObject);
    procedure ATargetRunUpdate(Sender: TObject);
    procedure ATargetUninstallExecute(Sender: TObject);
    procedure ATargetUninstallUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PopupMenuMorePopup(Sender: TObject);
    procedure TVPGAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure TVPGDblClick(Sender: TObject);
    procedure TVPGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVPGSelectionChanged(Sender: TObject);
  private
    // Nodelist image indexes
    NIProjectGroup             : integer;// = 0;
    NITargetProject            : integer;// = 3;
    NITargetPackage            : integer;// = 4;
    NITargetProjectGroup       : integer;// = 5;
    NIBuildModes               : integer;// = 12;
    NIBuildMode                : integer;// = 12;
    NIFiles                    : integer;// = 16;
    NIFile                     : integer;// = 17;
    NIDependencies             : integer;// = 1;
    NIDependency               : integer;// = 1;

    // Node state image index
    NSIActive                  : Integer;// = 20; // State index for active.
    NSIMissing                 : Integer; // State index for missing

    // overlay index
    NSIChecked                 : Integer;// = 22;
    NSIUnchecked               : Integer;// = 23;

    procedure LoadImages;
  private
    FBuildCommandRedirected: boolean;
    FProjectGroup: TProjectGroup;
    FProjectGroupTVNode: TTreeNode;
    FActiveTarget: TPGCompileTarget;
    FLastShowTargetPaths: boolean;
    FOldBuildExecute, FOldBuildUpdate: TNotifyEvent;
    FOldCompileExecute, FOldCompileUpdate: TNotifyEvent;
    // Project group callbacks
    procedure IDEProjectGroupManagerEditorOptionsChanged(Sender: TObject);
    procedure InitTVNode(Node: TTreeNode; Const ACaption: String;
      ANodeData: TNodeData);
    procedure OnApplicationActivate(Sender: TObject);
    procedure OnBuildExecute(Sender: TObject);
    procedure OnBuildUpdate(Sender: TObject);
    procedure OnCompileExecute(Sender: TObject);
    procedure OnCompileUpdate(Sender: TObject);
    procedure OnIDEClose(Sender: TObject);
    procedure OnProjectGroupDestroy(Sender: TObject);
    procedure OnProjectGroupFileNameChanged(Sender: TObject);
    procedure OnTargetInserted(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetDeleted(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetActiveChanged(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetExchanged(Sender: TObject; Target1, Target2: TPGCompileTarget);
    function AllowPerform(ATargetAction: TPGTargetAction; AAction: TAction= Nil): Boolean;
    procedure ClearEventCallBacks(AProjectGroup: TProjectGroup);
    procedure SetBuildCommandRedirected(const AValue: boolean);
    procedure SetEventCallBacks(AProjectGroup: TProjectGroup);
    // Some helpers
    procedure SetProjectGroup(AValue: TProjectGroup);
    function ShowDependencies(AParent: TTreeNode; T: TPGCompileTarget): TTreeNode;
    procedure ShowFileName;
    procedure Perform(ATargetAction: TPGTargetAction);
    function GetActiveTarget: TPGCompileTarget;
    // Treeview Node management
    function FindTVNodeOfTarget(ATarget: TPGCompileTarget): TTreeNode;
    function FindBuildModeNodeRecursively(TVNode: TTreeNode; aMode: string): TTreeNode;
    function FindTVNodeOfBuildMode(aMode: TPGBuildMode): TTreeNode;
    procedure FreeNodeData;
    class function TargetFromNode(N: TTreeNode): TPGCompileTarget;
    function DisplayFileName(aTarget: TPGCompileTarget): string;
    function DisplayFileName(Node: TTreeNode): string;
    function DisplayFileName(NodeData: TNodeData): string;
    function CreateSectionNode(AParent: TTreeNode; Const ACaption: String; ANodeType: TNodeType): TTreeNode;
    function CreateTargetNode(AParent: TTreeNode; ANodeType: TNodeType; aTarget: TPGCompileTarget): TTreeNode;
    function CreateSubNode(AParent: TTreeNode; ANodeType: TNodeType; aParentTarget: TPGCompileTarget; aValue: string): TTreeNode;
    procedure ClearNodeData(TVNode: TTreeNode);
    procedure ClearChildNodes(TVNode: TTreeNode);
    procedure FillPackageNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillTargetNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectGroupNode(TVNode: TTreeNode; AProjectGroup: TProjectGroup);
    function GetNodeImageIndex(ANodeType: TNodeType; ANodeData: TPGCompileTarget ): Integer;
    procedure AddTarget(const aFileName: string);
    function SelectedNodeData: TNodeData;
    function SelectedTarget: TPGCompileTarget;
    function GetTVNodeFilename(TVNode: TTreeNode): string;
    function GetBuildMode(TVNode: TTreeNode): TPGBuildMode;
    function GetNearestTargget(TVNode: TTreeNode): TPGCompileTarget;
    function SelectedNodeType: TPGCompileTarget;
    procedure UpdateIDEMenuCommandFromAction(Sender: TObject; Item: TIDEMenuCommand);
    procedure UpdateStatusBarTargetCount;
  protected
    procedure Localize;
    procedure ShowProjectGroup;
    procedure UpdateShowing; override;
  public
    property ProjectGroup: TProjectGroup Read FProjectGroup Write SetProjectGroup;
    property ActiveTarget: TPGCompileTarget Read GetActiveTarget;
    procedure UpdateNodeTexts;
    property BuildCommandRedirected: boolean read FBuildCommandRedirected write SetBuildCommandRedirected;
  end;

var
  ProjectGroupEditorForm: TProjectGroupEditorForm;
  ProjectGroupEditorCreator: TIDEWindowCreator; // set by RegProjectGroup.Register

const
  ProjectGroupEditorName = 'ProjectGroupEditor';
procedure ShowProjectGroupEditor(Sender: TObject; AProjectGroup: TProjectGroup);
procedure CreateProjectGroupEditor(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);
procedure SetProjectGroupEditorCallBack;

function dbgs(NodeType: TNodeType): string; overload;

implementation

{$R *.lfm}

const
  // Status bar Panel indexes
  piTargetCount  = 0;
  piActiveTarget = 1;

procedure ShowProjectGroupEditor(Sender: TObject; AProjectGroup: TProjectGroup);
begin
  IDEWindowCreators.ShowForm(ProjectGroupEditorCreator.FormName,true);
  if AProjectGroup<>nil then
    ProjectGroupEditorForm.ProjectGroup:=AProjectGroup;
end;

procedure CreateProjectGroupEditor(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName,ProjectGroupEditorName)<>0 then begin
    DebugLn(['ERROR: CreateProjectGroupEditor: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm,TProjectGroupEditorForm,DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
end;

procedure SetProjectGroupEditorCallBack;
begin
  ProjectGroupEditorCreator:=IDEWindowCreators.Add(ProjectGroupEditorName,
    @CreateProjectGroupEditor,nil,
    '30%','30%','+30%','+40%','ProjectGroupEditor',alNone);
  OnShowProjectGroupEditor:=@ShowProjectGroupEditor;
end;

function dbgs(NodeType: TNodeType): string;
begin
  str(NodeType,Result);
end;

{ TProjectGroupEditorForm }

procedure TProjectGroupEditorForm.ClearEventCallBacks(AProjectGroup: TProjectGroup);
Var
  PG: TIDEProjectGroup;
begin
  //debugln(['TProjectGroupEditorForm.ClearEventCallBacks ']);
  PG:=AProjectGroup as TIDEProjectGroup;
  PG.RemoveAllHandlersOfObject(Self);
  PG.OnFileNameChange:=Nil;
  PG.OnTargetInserted:=Nil;
  PG.OnTargetDeleted:=Nil;
  PG.OnTargetActiveChanged:=Nil;
  PG.OnTargetsExchanged:=Nil;
end;

procedure TProjectGroupEditorForm.ClearNodeData(TVNode: TTreeNode);
begin
  if TVNode.Data<>nil then
  begin
    TObject(TVNode.Data).Free;
    TVNode.Data:=nil;
  end;
end;

procedure TProjectGroupEditorForm.SetBuildCommandRedirected(
  const AValue: boolean);
var
  CompileCmd, BuildCmd: TIDECommand;
begin
  if FBuildCommandRedirected=AValue then Exit;
  FBuildCommandRedirected:=AValue;
  if IDECommandList=nil then exit;
  BuildCmd:=IDECommandList.FindIDECommand(ecBuild);
  CompileCmd:=IDECommandList.FindIDECommand(ecCompile);
  if FBuildCommandRedirected then begin
    // ecBuild
    FOldBuildExecute:=BuildCmd.OnExecute;
    FOldBuildUpdate:=BuildCmd.OnUpdate;
    BuildCmd.OnExecute:=@OnBuildExecute;
    BuildCmd.OnUpdate:=@OnBuildUpdate;
    // ecCompile
    FOldCompileExecute:=CompileCmd.OnExecute;
    FOldCompileUpdate:=CompileCmd.OnUpdate;
    CompileCmd.OnExecute:=@OnCompileExecute;
    CompileCmd.OnUpdate:=@OnCompileUpdate;
  end else begin
    // build
    BuildCmd.OnExecute:=FOldBuildExecute;
    BuildCmd.OnUpdate:=FOldBuildUpdate;
    // compile
    CompileCmd.OnExecute:=FOldCompileExecute;
    CompileCmd.OnUpdate:=FOldCompileUpdate;
  end;
end;

procedure TProjectGroupEditorForm.SetEventCallBacks(AProjectGroup: TProjectGroup);
Var
  PG: TIDEProjectGroup;
begin
  PG:=AProjectGroup as TIDEProjectGroup;
  PG.AddHandlerOnDestroy(@OnProjectGroupDestroy);
  PG.OnFileNameChange:=@OnProjectGroupFileNameChanged;
  PG.OnTargetInserted:=@OnTargetInserted;
  PG.OnTargetDeleted:=@OnTargetDeleted;
  PG.OnTargetActiveChanged:=@OnTargetActiveChanged;
  PG.OnTargetsExchanged:=@OnTargetExchanged;
end;

procedure TProjectGroupEditorForm.SetProjectGroup(AValue: TProjectGroup);
begin
  //debugln(['TProjectGroupEditorForm.SetProjectGroup START ',FProjectGroup=AValue,' new=',DbgSName(AValue)]);
  if FProjectGroup=AValue then Exit;
  if ProjectGroup<>nil then
  begin
    ClearEventCallBacks(ProjectGroup);
  end;
  FProjectGroup:=AValue;
  if ProjectGroup<>nil then begin
    SetEventCallBacks(ProjectGroup);
  end;
  FActiveTarget:=Nil;
  ShowProjectGroup;
end;

procedure TProjectGroupEditorForm.Localize;

  procedure ConfigAction(A: TAction; AImageName: string; Const ACaption,AHint: String; Mnu: TIDEMenuCommand);
  begin
    A.Caption:=ACaption;
    A.Hint:=AHint;
    if AImageName<>'' then
      A.ImageIndex:=IDEImages.GetImageIndex(AImageName)
    else
      A.ImageIndex:=-1;
    If Assigned(mnu) then
      Mnu.OnClick:=A.OnExecute;
  end;

begin
  ConfigAction(AProjectGroupSave,'laz_save',lisProjectGroupSaveCaption,lisProjectGroupSaveHint,Nil);
  ConfigAction(AProjectGroupSaveAs,'menu_saveas',lisProjectGroupSaveAsCaption,lisProjectGroupSaveAsHint,Nil);
  ConfigAction(AProjectGroupNew,'laz_wand',lisProjectGroupNewCaption,lisProjectGroupNewHint,Nil);
  ConfigAction(AProjectGroupAddExisting,'menu_project_open',lisProjectGroupAddExistingCaption,lisProjectGroupAddExistingHint,Nil);
  ConfigAction(AProjectGroupAddCurrent,'menu_project_add',lisProjectGroupAddCurrentProjectCaption,lisProjectGroupAddCurrentProjectHint,Nil);
  ConfigAction(AProjectGroupDelete,'laz_delete',lisProjectGroupDeleteCaption,lisProjectGroupDeleteHint,Nil);
  ConfigAction(AProjectGroupAddNew,'menu_project_new',lisProjectGroupAddNewCaption,lisProjectGroupAddNewHint,Nil);
  ConfigAction(ATargetEarlier,'arrow_up',lisTargetEarlierCaption,lisTargetEarlierHint,Nil);
  ConfigAction(ATargetLater,'arrow_down',lisTargetLaterCaption,lisTargetLaterHint,Nil);
  ConfigAction(ATargetCompile,'menu_build',lisTargetCompileCaption,lisTargetCompileHint,Nil);
  ConfigAction(ATargetCompileClean,'menu_build_clean',lisTargetCompileCleanCaption,lisTargetCompileCleanHint,Nil);
  ConfigAction(ATargetProperties,'menu_project_options',lisTargetPropertiesCaption,lisTargetPropertiesHint,Nil);
  ConfigAction(ATargetRun,'menu_run',lisTargetRunCaption,lisTargetRunHint,Nil);
  ConfigAction(ATargetInstall,'pkg_install',lisTargetInstallCaption,lisTargetInstallHint,Nil);
  ConfigAction(ATargetUninstall,'pkg_package_uninstall',lisTargetUninstallCaption,lisTargetUninstallHint,Nil);
  ConfigAction(ATargetInfo,'menu_information',lisTargetInfoCaption,'',Nil);
  ConfigAction(ATargetActivate,'',lisTargetActivateCaption,lisTargetActivateHint,Nil);
  ConfigAction(ATargetOpen,'',lisTargetOpenCaption,lisTargetOpenHint,Nil);
  ConfigAction(ATargetCopyFilename,'',lisTargetCopyFilename,'',Nil);
  ConfigAction(ATargetCompileFromHere,'',lisTargetCompileFromHere,'',Nil);
  ConfigAction(AProjectGroupReload,'laz_refresh',lisProjectGroupReload,'',Nil);
  ConfigAction(AProjectGroupUndo, 'menu_undo', lisUndo, '', nil);
  ConfigAction(AProjectGroupRedo, 'menu_redo', lisRedo, '', nil);
  ConfigAction(AProjectGroupOptions, 'menu_environment_options', lisOptions, '', nil);
  TBMore.Caption:=lisMore;
  TBAdd.Caption := lisProjectGroupAddCaption;
  TBAdd.ImageIndex := IDEImages.GetImageIndex('laz_add');
  TBAdd.Hint := lisProjectGroupAddHint;
  ActionListMain.Images := IDEImages.Images_16;
  PopupMenuMore.Images := ActionListMain.Images;
  PopupMenuAdd.Images := ActionListMain.Images;
  PopupMenuTree.Images := ActionListMain.Images;
  TBProjectGroup.Images := ActionListMain.Images;
  TVPG.Images := ActionListMain.Images;
  TVPG.StateImages := ActionListMain.Images;
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FProjectGroup<>nil)
    and (FProjectGroup.Modified or (FProjectGroup.FileName=''));
  TBAdd.Enabled:=(FProjectGroup<>nil);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdSaveProjectGroup);
end;

procedure TProjectGroupEditorForm.AProjectGroupUndoExecute(Sender: TObject);
begin
  IDEProjectGroupManager.Undo;
end;

procedure TProjectGroupEditorForm.AProjectGroupUndoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=IDEProjectGroupManager.CanUndo;
end;

procedure TProjectGroupEditorForm.ATargetEarlierExecute(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) or (T.Target.Parent=nil) then
    exit;
  PG:=T.Target.Parent.ProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  if I>0 then
    PG.ExchangeTargets(I,I-1);
end;

procedure TProjectGroupEditorForm.ATargetEarlierUpdate(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  I:=0;
  T:=SelectedNodeData;
  if (T<>nil) and (T.Target<>nil) and (T.Target.Parent<>nil) then
  begin
    PG:=T.Target.Parent.ProjectGroup;
    if PG<>nil then begin
      I:=PG.IndexOfTarget(T.Target);
    end;
  end;
  (Sender as TAction).Enabled:=I>0;
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetEarlier);
end;

procedure TProjectGroupEditorForm.ATargetInfoExecute(Sender: TObject);
begin
  ShowPrgGrpInfo(SelectedTarget as TIDECompileTarget);
end;

procedure TProjectGroupEditorForm.ATargetInfoUpdate(Sender: TObject);
begin

end;

procedure TProjectGroupEditorForm.ATargetLaterExecute(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) or (T.Target.Parent=nil) then
    exit;
  PG:=T.Target.Parent.ProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  if I<0 then exit;
  if (I+1<PG.TargetCount) then
    PG.ExchangeTargets(I,I+1);
end;

procedure TProjectGroupEditorForm.ATargetLaterUpdate(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  I:=-1;
  PG:=nil;
  if (T<>nil) and (T.Target<>nil) and (T.Target.Parent<>nil) then
  begin
    PG:=T.Target.Parent.ProjectGroup;
    if PG<>nil then
      I:=PG.IndexOfTarget(T.Target);
  end;
  (Sender as TAction).Enabled:=(PG<>nil) and (I+1<PG.TargetCount);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetLater);
end;

procedure TProjectGroupEditorForm.ATargetUninstallExecute(Sender: TObject);
begin
  Perform(taInstall);
end;

procedure TProjectGroupEditorForm.ATargetUninstallUpdate(Sender: TObject);
begin
  AllowPerform(taUninstall,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetUninstall);
end;

procedure TProjectGroupEditorForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=IDEProjectGroupManager.CheckSaved;
end;

procedure TProjectGroupEditorForm.UpdateIDEMenuCommandFromAction(
  Sender: TObject; Item: TIDEMenuCommand);
begin
  Item.Enabled:=(Sender as TAction).Enabled;
  Item.Visible:=(Sender as TAction).Visible;
end;

procedure TProjectGroupEditorForm.UpdateStatusBarTargetCount;
var
  Cnt: Integer;
begin
  if FProjectGroup<>nil then
    Cnt:=FProjectGroup.TargetCount
  else
    Cnt:=0;
  SBPG.Panels[piTargetCount].Text:=Format(lisTargetCount, [Cnt]);
end;

procedure TProjectGroupEditorForm.FormCreate(Sender: TObject);

  procedure SetItem(Item: TIDEMenuCommand; AnOnClick: TNotifyEvent;
                    aShow: boolean = true; AEnable: boolean = true);
  begin
    //debugln(['SetItem ',Item.Caption,' Visible=',aShow,' Enable=',AEnable]);
    Item.OnClick:=AnOnClick;
    Item.Visible:=aShow;
    Item.Enabled:=AEnable;
  end;

begin
  if ProjectGroupEditorForm=nil then
    ProjectGroupEditorForm:=Self;
  IDEProjectGroupManager.Editor:=Self;
  IDEProjectGroupManager.OnEditorOptionsChanged:=@IDEProjectGroupManagerEditorOptionsChanged;

  PGEditMenuSectionMisc.MenuItem:=PopupMenuMore.Items;
  SetItem(MnuCmdTargetAdd,@AProjectGroupAddExistingExecute);
  SetItem(MnuCmdTargetRemove,@AProjectGroupDeleteExecute);
  SetItem(MnuCmdTargetCompile,@ATargetCompileExecute);
  SetItem(MnuCmdTargetCompileClean,@ATargetCompileCleanExecute);
  SetItem(MnuCmdTargetCompileFromHere,@ATargetCompileFromHereExecute);
  SetItem(MnuCmdTargetInstall,@ATargetInstallExecute);
  SetItem(MnuCmdTargetUninstall,@ATargetUnInstallExecute);
  SetItem(MnuCmdTargetLater,@ATargetLaterExecute);
  SetItem(MnuCmdTargetEarlier,@ATargetEarlierExecute);
  SetItem(MnuCmdTargetCopyFilename,@ATargetCopyFilenameExecute);
  SetItem(MnuCmdProjGrpUndo,@AProjectGroupUndoExecute);
  SetItem(MnuCmdProjGrpRedo,@AProjectGroupRedoExecute);
  SetItem(MnuCmdProjGrpOptions,@AProjectGroupOptionsExecute);

  LazarusIDE.AddHandlerOnIDEClose(@OnIDEClose);
  Application.AddOnActivateHandler(@OnApplicationActivate);

  if IDEProjectGroupManager.Options.BuildCommandToCompileTarget then
    BuildCommandRedirected:=true;

  LoadImages;
end;

procedure TProjectGroupEditorForm.FormDestroy(Sender: TObject);
begin
  //debugln(['TProjectGroupEditorForm.FormDestroy START ',ProjectGroup<>nil]);
  BuildCommandRedirected:=false;
  ProjectGroup:=nil;
  if ProjectGroupEditorForm=Self then
    ProjectGroupEditorForm:=nil;
  ProjectGroupManager.Editor:=Self;
  if (PGEditMenuSectionMisc<>nil)
  and (PGEditMenuSectionMisc.MenuItem=PopupMenuMore.Items) then
    PGEditMenuSectionMisc.MenuItem:=nil;
  //debugln(['TProjectGroupEditorForm.FormDestroy END ',ProjectGroup<>nil]);
end;

procedure TProjectGroupEditorForm.FormShow(Sender: TObject);
begin

end;

procedure TProjectGroupEditorForm.PopupMenuMorePopup(Sender: TObject);
var
  ND: TNodeData;
  AllowedActions: TPGTargetActions;
begin
  ND:=SelectedNodeData;
  if (ND<>nil) and (ND.Target<>nil) then begin
    AllowedActions:=PGTargetActions[ND.Target.TargetType];
  end else begin
    AllowedActions:=[taOpen,taSettings];
  end;
  PMIOpen.Visible:=taOpen in AllowedActions;
  PMIProperties.Visible:=taSettings in AllowedActions;
  PMICompile.Visible:=taCompile in AllowedActions;
  PMICompileClean.Visible:=taCompileClean in AllowedActions;
  PMIRunMenuItem.Visible:=taRun in AllowedActions;
end;

procedure TProjectGroupEditorForm.TVPGAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);

  procedure PaintOverlayImage(const AImageIndex: Integer);
  var
    r: TRect;
    y: LongInt;
    ImagesRes: TScaledImageListResolution;
  begin
    ImagesRes :=Sender.Images.ResolutionForControl[Sender.ImagesWidth, Sender];
    r:=Node.DisplayRect(true);
    r.Left:=Node.DisplayIconLeft+1;
    y:=(r.Top+r.Bottom-ImagesRes.Height) div 2;
    ImagesRes.Draw(Sender.Canvas,r.Left,y,AImageIndex);
  end;
var
  ND: TNodeData;
  r: TRect;
  y: LongInt;
begin
  if Stage=cdPostPaint then begin
    ND:=TNodeData(Node.Data);
    if (ND.Target<>nil) and ND.Target.Missing then begin
      // Missing target file: draw red line strike through text an
      PaintOverlayImage(NSIMissing);

      r:=Node.DisplayRect(true);
      TVPG.Canvas.Pen.Color:=clRed;
      y:=(r.Top+r.Bottom) div 2;
      TVPG.Canvas.Line(r.Left,y,r.Right,y);
    end;
  end;
end;

procedure TProjectGroupEditorForm.TVPGDblClick(Sender: TObject);
Var
  ND: TNodeData;
  aFilename, PkgName: String;
  PG: TProjectGroup;
begin
  ND:=SelectedNodeData;
  //debugln(['TProjectGroupEditorForm.TVPGDblClick ',DbgSName(Sender),' ',TVPG.Selected.Text,' ',ND<>nil]);
  if ND=nil then exit;
  case ND.NodeType of
  ntProjectGroup,
  ntTarget:
    begin
      PG:=ND.Target.GetOwnerProjectGroup;
      if PG=nil then exit;
      case ND.Target.TargetType of
      ttProject,
      ttPackage,
      ttPascalFile:
        PG.Perform(ND.Target,taOpen)
      end;
    end;
  ntMissingTarget:
    ;
  ntFile:
    begin
      // open file in source editor
      aFilename:=ND.Value;
      //debugln(['TProjectGroupEditorForm.TVPGDblClick File=',aFilename]);
      if aFilename='' then exit;
      LazarusIDE.DoOpenEditorFile(aFilename,-1,-1,[ofAddToRecent,
        ofRegularFile,ofDoNotLoadResource,ofOnlyIfExists]);
    end;
  ntDependency:
    begin
      // open package editor
      PkgName:=ND.Value;
      if PackageEditingInterface.DoOpenPackageWithName(PkgName,[pofAddToRecent],false)<>mrOk
      then begin
        IDEMessageDialog(lisPackageNotFound, Format(lisPackageNotFound2, [PkgName]), mtError, [mbOk]);
        exit;
      end;
    end;
  end;
end;

procedure TProjectGroupEditorForm.TVPGMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TVNode: TTreeNode;
  ND: TNodeData;
  aMode: TPGBuildMode;
begin
  TVNode:=TVPG.GetNodeAt(X,Y);
  if TVNode=nil then exit;
  ND:=TNodeData(TVNode.Data);
  if ND=nil then exit;
  if mbLeft=Button then begin
    if (ND.NodeType=ntBuildMode) and ([ssShift,ssCtrl]*Shift=[]) then
    begin
      if (TVNode.DisplayStateIconLeft<=X) and (X<TVNode.DisplayIconLeft) then
      begin
        if TVNode.StateIndex=NSIChecked then
          TVNode.StateIndex:=NSIUnchecked
        else
          TVNode.StateIndex:=NSIChecked;
        aMode:=GetBuildMode(TVNode);
        if aMode<>nil then
          aMode.Compile:=TVNode.StateIndex=NSIChecked;
      end;
    end;
  end;
end;

procedure TProjectGroupEditorForm.TVPGSelectionChanged(Sender: TObject);
var
  TVNode: TTreeNode;
  ND: TNodeData;
  s: String;
begin
  TVNode:=TVPG.Selected;
  s:='';
  if (TVNode<>nil) and (TVNode.Data<>nil) then begin
    ND:=TNodeData(TVNode.Data);
    if ND.Target<>nil then begin
      s:=ND.Target.Filename;
    end else begin
      case ND.NodeType of
      ntBuildMode: s := Format(lisBuildMode2, [ND.Value]);
      ntFile: s:=ND.Value;
      end;
    end;
  end;

  SBPG.Panels[piActiveTarget].Text:=s;
end;

procedure TProjectGroupEditorForm.OnTargetInserted(Sender: TObject;
  Target: TPGCompileTarget);
Var
  N: TTreeNode;
begin
  (Target as TIDECompileTarget).LoadTarget(true);
  if Sender<>ProjectGroup then exit; // ToDo: sub groups
  TVPG.BeginUpdate;
  try
    N:=CreateTargetNode(FProjectGroupTVNode,ntTarget,Target);
    N.Index:=Target.GetIndex;
    FillTargetNode(N,Target);
    TVPG.Selected:=N;
  finally
    TVPG.EndUpdate;
  end;
  UpdateStatusBarTargetCount;
end;

procedure TProjectGroupEditorForm.OnTargetDeleted(Sender: TObject;
  Target: TPGCompileTarget);
Var
  N: TTreeNode;
begin
  if Sender<>ProjectGroup then exit; // ToDo: sub groups
  N:=FindTVNodeOfTarget(Target);
  TVPG.BeginUpdate;
  try
    ClearChildNodes(N);
    ClearNodeData(N);
    TVPG.Items.Delete(N);
    TVPG.Selected:=FProjectGroupTVNode;
  finally
    TVPG.EndUpdate;
  end;
  UpdateStatusBarTargetCount;
end;

procedure TProjectGroupEditorForm.OnTargetActiveChanged(Sender: TObject;
  Target: TPGCompileTarget);
Var
  OldActiveTVNode,NewActiveTVNode: TTreeNode;
begin
  OldActiveTVNode:=FindTVNodeOfTarget(FActiveTarget);
  NewActiveTVNode:=FindTVNodeOfTarget(Target);
  if (OldActiveTVNode<>NewActiveTVNode) then
  begin
    if Assigned(OldActiveTVNode) then
      OldActiveTVNode.StateIndex:=-1;
    if Assigned(NewActiveTVNode) then
      NewActiveTVNode.StateIndex:=NSIActive;
    FActiveTarget:=Target;
  end;
  //N:=DisplayFileName(Target);
  //SBPG.Panels[piActiveTarget].Text:=Format(lisActiveTarget,[N]);
end;

procedure TProjectGroupEditorForm.OnTargetExchanged(Sender: TObject; Target1,
  Target2: TPGCompileTarget);
var
  N1, N2: TTreeNode;
  OldIndex: Integer;
begin
  N1:=FindTVNodeOfTarget(Target1);
  N2:=FindTVNodeOfTarget(Target2);
  If (N1=Nil) or (N2=Nil) then
    exit;
  OldIndex:=N1.Index;
  N1.Index:=N2.Index;
  N2.Index:=OldIndex;
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveExecute(Sender: TObject);
Var
  P: String;
begin
  if FProjectGroup=nil then exit;
  P:=FProjectGroup.FileName;
  ProjectGroupManager.SaveProjectGroup;
  if P<>FProjectGroup.FileName then
    ShowProjectGroup;
end;

procedure TProjectGroupEditorForm.AProjectGroupAddExistingExecute(Sender: TObject);
var
  i: Integer;
begin
  if FProjectGroup=nil then exit;
  InitIDEFileDialog(OpenDialogTarget);
  OpenDialogTarget.Filter :=
           lisLazarusSupportedInProjectGroups + '|*.lpi;*.lpk;*.lpg;*.pas;*.pp;*.p'
   + '|' + lisLazarusProjectsLpi + '|*.lpi'
   + '|' + lisLazarusPackagesLpk + '|*.lpk'
   + '|' + lisLazarusProjectGroupsLpg + '|*.lpg'
   + '|' + lisPascalFilePasPpP + '|*.pas;*.pp;*.p';
  If OpenDialogTarget.Execute then
    for i:=0 to OpenDialogTarget.Files.Count-1 do
      AddTarget(OpenDialogTarget.Files[i]);
  StoreIDEFileDialog(OpenDialogTarget);
end;

procedure TProjectGroupEditorForm.AProjectGroupAddExistingUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled:=FProjectGroup<>nil;
end;

procedure TProjectGroupEditorForm.ATargetActivateUpdate(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  T:=SelectedTarget;
  (Sender as TAction).Enabled:=Assigned(T) and Not T.Active;
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetActivate);
end;

procedure TProjectGroupEditorForm.ATargetActivateExecute(Sender: TObject);
Var
  ND: TNodeData;
begin
  ND:=SelectedNodeData;
  if (ND=nil) or (ND.Target=nil) then
    exit;
  ND.Target.Activate;
end;

procedure TProjectGroupEditorForm.AProjectGroupReloadExecute(Sender: TObject);
var
  PG: TIDEProjectGroup;
begin
  if ProjectGroup=nil then exit;
  if FileExistsCached(ProjectGroup.FileName) then
  begin
    PG:=TIDEProjectGroup(ProjectGroup);
    if PG.Modified then begin
      // ToDo: revert
      IDEMessageDialog(lisNeedSave, lisPleaseSaveYourChangesBeforeReloadingTheProjectGrou,
        mtError,[mbOK]);
      PG.UpdateMissing;
      exit;
    end;
    ProjectGroup:=nil;
    try
      PG.LoadFromFile([pgloLoadRecursively]);
    finally
      ProjectGroup:=PG;
    end;
  end else
    PG.UpdateMissing;
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveAsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FProjectGroup<>nil);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdSaveProjectGroupAs);
end;

procedure TProjectGroupEditorForm.ATargetCompileCleanExecute(Sender: TObject);
begin
  Perform(taCompileClean);
end;

procedure TProjectGroupEditorForm.ATargetCompileCleanUpdate(Sender: TObject);
begin
  AllowPerform(taCompileClean,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetCompileClean);
end;

function TProjectGroupEditorForm.AllowPerform(ATargetAction: TPGTargetAction; AAction: TAction = Nil): Boolean;
Var
  ND: TNodeData;
  aTarget: TPGCompileTarget;
begin
  Result:=false;
  ND:=SelectedNodeData;
  if ND<>nil then begin
    if ND.Target<>nil then begin
      Result:=(not ND.Target.Missing) and (ATargetAction in ND.Target.AllowedActions);
    end else begin
      aTarget:=GetNearestTargget(TVPG.Selected);
      case ND.NodeType of
      ntBuildMode:
        Result:=(not aTarget.Missing)
          and (ATargetAction in [taCompile,taCompileClean,taCompileFromHere,taRun]);
      end;
    end;
  end;
  If Assigned(AAction) then
    AAction.Enabled:=Result;
end;

procedure TProjectGroupEditorForm.AProjectGroupAddCurrentExecute(
  Sender: TObject);
begin
  if LazarusIDE.ActiveProject.ProjectInfoFile<>'' then
    AddTarget(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

procedure TProjectGroupEditorForm.AProjectGroupAddCurrentUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := (FProjectGroup<>nil) and (LazarusIDE.ActiveProject<>nil)
    and (LazarusIDE.ActiveProject.ProjectInfoFile<>'');
end;

procedure TProjectGroupEditorForm.AddTarget(const aFileName: string);
var
  aTarget: TIDECompileTarget;
  aMode: TPGBuildMode;
  TVNode: TTreeNode;
begin
  if FProjectGroup.IndexOfTarget(aFileName)>=0 then
    Exit;
  aTarget:=FProjectGroup.AddTarget(aFileName) as TIDECompileTarget;
  aTarget.LoadTarget(true);
  if aTarget.BuildModeCount>1 then
  begin
    aMode:=aTarget.BuildModes[0];
    aMode.Compile:=true;
    // ToDo: implement changed notification
    TVNode:=FindTVNodeOfBuildMode(aMode);
    TVNode.StateIndex:=NSIChecked;
  end;
end;

procedure TProjectGroupEditorForm.Perform(ATargetAction: TPGTargetAction);
Var
  ND: TNodeData;
  aTarget: TPGCompileTarget;
begin
  ND:=SelectedNodeData;
  if (ND=nil) then exit;
  aTarget:=ND.Target;
  if aTarget<>nil then
    aTarget.GetOwnerProjectGroup.Perform(aTarget,ATargetAction)
  else begin
    aTarget:=GetNearestTargget(TVPG.Selected);
    case ND.NodeType of
    ntBuildMode:
      aTarget.PerformBuildModeAction(ATargetAction,ND.Value);
    end;
  end;
end;

procedure TProjectGroupEditorForm.ATargetCompileExecute(Sender: TObject);
begin
  Perform(taCompile);
end;

procedure TProjectGroupEditorForm.ATargetCompileFromHereExecute(Sender: TObject
  );
begin
  Perform(taCompileFromHere);
end;

procedure TProjectGroupEditorForm.ATargetCompileFromHereUpdate(Sender: TObject);
begin
  AllowPerform(taCompileFromHere,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetCompile);
end;

procedure TProjectGroupEditorForm.ATargetCompileUpdate(Sender: TObject);
begin
  AllowPerform(taCompile,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetCompile);
end;

procedure TProjectGroupEditorForm.AProjectGroupDeleteExecute(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  T:=SelectedTarget;
  if (T=nil) or (T.Parent=nil) then exit;
  T.Parent.ProjectGroup.RemoveTarget(T);
end;

procedure TProjectGroupEditorForm.AProjectGroupDeleteUpdate(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  T:=SelectedTarget;
  (Sender as TAction).Enabled:=(T<>nil) and (T<>ProjectGroup.SelfTarget);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetRemove);
end;

procedure TProjectGroupEditorForm.AProjectGroupNewExecute(Sender: TObject);
begin
  IDEProjectGroupManager.DoNewClick(Sender);
end;

procedure TProjectGroupEditorForm.AProjectGroupOptionsExecute(Sender: TObject);
begin
  LazarusIDE.DoOpenIDEOptions(TProjGrpOptionsFrame);
end;

procedure TProjectGroupEditorForm.AProjectGroupRedoExecute(Sender: TObject);
begin
  // ToDo
  debugln(['TProjectGroupEditorForm.AProjectGroupRedoExecute Todo']);
end;

procedure TProjectGroupEditorForm.AProjectGroupRedoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=IDEProjectGroupManager.CanRedo;
end;

procedure TProjectGroupEditorForm.ATargetCopyFilenameExecute(Sender: TObject);
var
  ND: TNodeData;
  aFilename: String;
begin
  ND:=SelectedNodeData;
  if ND=nil then exit;
  if ND.Target<>nil then
    aFilename:=ND.Target.Filename
  else if ND.NodeType=ntFile then
    aFilename:=ND.Value
  else
    exit;
  Clipboard.AsText:=aFilename;
end;

procedure TProjectGroupEditorForm.ATargetCopyFilenameUpdate(Sender: TObject);
var
  ND: TNodeData;
begin
  ND:=SelectedNodeData;
  (Sender as TAction).Enabled:=(ND<>nil)
    and ((ND.Target<>nil) or (ND.NodeType in [ntFile]));
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetCopyFilename);
end;

procedure TProjectGroupEditorForm.ATargetInstallExecute(Sender: TObject);
begin
  Perform(taInstall);
end;

procedure TProjectGroupEditorForm.ATargetInstallUpdate(Sender: TObject);
begin
  AllowPerform(taInstall,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetInstall);
end;

procedure TProjectGroupEditorForm.ATargetOpenExecute(Sender: TObject);
begin
  Perform(taOpen);
end;

procedure TProjectGroupEditorForm.ATargetOpenUpdate(Sender: TObject);
begin
  AllowPerform(taOpen,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetOpen);
end;

procedure TProjectGroupEditorForm.ATargetPropertiesExecute(Sender: TObject);
begin
  Perform(taSettings);
end;

procedure TProjectGroupEditorForm.ATargetPropertiesUpdate(Sender: TObject);
begin
  AllowPerform(taSettings,Sender as Taction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetProperties);
end;

procedure TProjectGroupEditorForm.ATargetRunExecute(Sender: TObject);
begin
  Perform(taRun);
end;

procedure TProjectGroupEditorForm.ATargetRunUpdate(Sender: TObject);
begin
  AllowPerform(taRun,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetRun);
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveAsExecute(Sender: TObject);
begin
  IDEProjectGroupManager.DoSaveAsClick(Sender);
end;

procedure TProjectGroupEditorForm.FreeNodeData;
Var
  N: TTreeNode;
  I: Integer;
begin
  FActiveTarget:=nil;
  FProjectGroupTVNode:=Nil;
  For I:=0 to TVPG.Items.Count-1 do
  begin
    N:=TVPG.Items[I];
    TNodeData(N.Data).Free; // Would be nice to have a FreeAndNilData method in TTreeNode.
    N.Data:=Nil;
  end;
end;

function TProjectGroupEditorForm.GetNodeImageIndex(ANodeType: TNodeType;
  ANodeData: TPGCompileTarget): Integer;
begin
  case ANodeType of
    ntProjectGroup: Result:=NIProjectGroup;
    ntTarget, ntMissingTarget :
        Case ANodeData.TargetType of
          ttProject: Result:=NITargetProject;
          ttPackage: Result:=NITargetPackage;
          ttProjectGroup: Result:=NITargetProjectGroup;
          ttPascalFile: Result:=NIFile;
        end;
    ntBuildModes: Result:=NIBuildModes;
    ntBuildMode: Result:=NIBuildMode;
    ntFiles: Result:=NIFiles;
    ntFile: Result:=NIFile;
    //ntRemovedFiles: Result:=NIRemovedFiles;
    //ntRemovedFile: Result:=NIRemovedFile;
    ntDependencies: Result:=NIDependencies;
    ntDependency: Result:=NIDependency;
    //ntRemovedDependencies: Result:=NIRemovedDependencies;
    //ntRemovedDependency: Result:=NIRemovedDependency;
  else
    Result:=-1;
  end;
end;

function TProjectGroupEditorForm.SelectedNodeData: TNodeData;
Var
  N: TTreeNode;
begin
  N:=TVPG.Selected;
  If Assigned(N) then
    Result:=TNodeData(N.Data)
  else
    Result:=Nil;
end;

function TProjectGroupEditorForm.SelectedTarget: TPGCompileTarget;
Var
  N: TNodeData;
begin
  N:=SelectedNodeData;
  if Assigned(N) then
    Result:=N.Target
  else
    Result:=Nil;
end;

function TProjectGroupEditorForm.GetTVNodeFilename(TVNode: TTreeNode): string;
var
  ND: TNodeData;
begin
  Result:='';
  if (TVNode=nil) then exit;
  ND:=TNodeData(TVNode.Data);
  if (ND.Target<>nil) then
    exit(ND.Target.Filename);
  case ND.NodeType of
  ntFile: Result:=ND.Value;
  end;
end;

function TProjectGroupEditorForm.GetBuildMode(TVNode: TTreeNode): TPGBuildMode;
var
  ND: TNodeData;
begin
  Result:=nil;
  if TVNode=nil then exit;
  ND:=TNodeData(TVNode.Data);
  if (ND=nil) or (ND.NodeType<>ntBuildMode) then exit;
  while TVNode<>nil do begin
    if (TVNode.Data<>nil) and (TNodeData(TVNode.Data).Target<>nil) then
    begin
      Result:=TNodeData(TVNode.Data).Target.FindBuildMode(ND.Value);
      exit;
    end;
    TVNode:=TVNode.Parent;
  end;
end;

function TProjectGroupEditorForm.GetNearestTargget(TVNode: TTreeNode
  ): TPGCompileTarget;
begin
  Result:=nil;
  while (TVNode<>nil) do begin
    if (TVNode.Data<>nil) then begin
      Result:=TNodeData(TVNode.Data).Target;
      if Result<>nil then exit;
    end;
    TVNode:=TVNode.Parent;
  end;
  Result:=nil;
end;

function TProjectGroupEditorForm.SelectedNodeType: TPGCompileTarget;
Var
  N: TNodeData;
begin
  N:=SelectedNodeData;
  if Assigned(N) then
    Result:=N.Target
  else
    Result:=Nil;
end;

procedure TProjectGroupEditorForm.InitTVNode(Node: TTreeNode;
  const ACaption: String; ANodeData: TNodeData);
begin
  Node.Data:=ANodeData;
  If (ACaption<>'') then
    Node.Text:=ACaption;
  Node.ImageIndex:=GetNodeImageIndex(ANodeData.NodeType,ANodeData.Target);
  Node.SelectedIndex:=Node.ImageIndex;
  if Assigned(ANodeData.Target) and ANodeData.Target.Active then
    Node.StateIndex:=NSIActive
  else
    Node.StateIndex:=-1;
end;

procedure TProjectGroupEditorForm.LoadImages;
begin
  NIProjectGroup := IDEImages.GetImageIndex('projectgroup');
  NITargetProject := IDEImages.GetImageIndex('item_project');
  NITargetPackage := IDEImages.GetImageIndex('item_package');
  NITargetProjectGroup := NIProjectGroup;
  NIBuildModes := IDEImages.GetImageIndex('menu_build_all');
  NIBuildMode := IDEImages.GetImageIndex('menu_build');
  NIFiles := IDEImages.GetImageIndex('pkg_files');
  NIFile := IDEImages.GetImageIndex('item_unit');
  NIDependencies := IDEImages.GetImageIndex('pkg_required');
  NIDependency := IDEImages.GetImageIndex('pkg_required');

  // Node state image index
  NSIActive := IDEImages.GetImageIndex('pg_active');
  NSIMissing := IDEImages.GetImageIndex('laz_cancel');

  // overlay index
  NSIChecked := IDEImages.GetImageIndex('laz_tick');
  NSIUnchecked := IDEImages.GetImageIndex('laz_cancel');
end;

procedure TProjectGroupEditorForm.IDEProjectGroupManagerEditorOptionsChanged(
  Sender: TObject);
var
  Opts: TIDEProjectGroupOptions;
begin
  Invalidate;
  Opts:=IDEProjectGroupManager.Options;
  if Opts.BuildCommandToCompileTarget then
    BuildCommandRedirected:=true;
  if FLastShowTargetPaths<>Opts.ShowTargetPaths then
    UpdateNodeTexts;
end;

procedure TProjectGroupEditorForm.OnApplicationActivate(Sender: TObject);
begin
  if ProjectGroup<>nil then
    ProjectGroup.UpdateMissing;
end;

procedure TProjectGroupEditorForm.OnBuildExecute(Sender: TObject);
var
  ND: TNodeData;
begin
  if BuildCommandRedirected then begin
    ND:=SelectedNodeData;
    if (ND<>nil) and (ND.Target<>nil) then begin
      Perform(taCompileClean);
      exit;
    end;
  end;
  if Assigned(FOldBuildExecute) then
    FOldBuildExecute(Sender);
end;

procedure TProjectGroupEditorForm.OnBuildUpdate(Sender: TObject);
begin
  if Assigned(FOldBuildUpdate) then
    FOldBuildUpdate(Sender);
end;

procedure TProjectGroupEditorForm.OnCompileExecute(Sender: TObject);
var
  ND: TNodeData;
begin
  if BuildCommandRedirected then begin
    ND:=SelectedNodeData;
    if (ND<>nil) and (ND.Target<>nil) then begin
      Perform(taCompile);
      exit;
    end;
  end;
  // execute IDE's compile action
  if Assigned(FOldCompileExecute) then
    FOldCompileExecute(Sender);
end;

procedure TProjectGroupEditorForm.OnCompileUpdate(Sender: TObject);
begin
  //debugln(['TProjectGroupEditorForm.OnCompileUpdate ',DbgSName(Sender)]);
  if Assigned(FOldCompileUpdate) then
    FOldCompileUpdate(Sender);
end;

procedure TProjectGroupEditorForm.OnIDEClose(Sender: TObject);
var
  Opts: TIDEProjectGroupOptions;
begin
  if IsVisible then
  begin
    Opts:=IDEProjectGroupManager.Options;
    if Opts.OpenLastGroupOnStart then
    begin
      if (ProjectGroup<>nil) and FilenameIsAbsolute(ProjectGroup.FileName) then
        Opts.LastGroupFile:=ProjectGroup.FileName
      else
        Opts.LastGroupFile:='';
      if Opts.Modified then
        Opts.SaveSafe;
    end;
  end;
end;

procedure TProjectGroupEditorForm.OnProjectGroupDestroy(Sender: TObject);
begin
  if Sender=FProjectGroup then begin
    ProjectGroup:=nil;
  end;
end;

procedure TProjectGroupEditorForm.OnProjectGroupFileNameChanged(Sender: TObject);
var
  TVNode: TTreeNode;
  NodeData: TNodeData;
begin
  if Sender<>ProjectGroup then exit; // ToDo: sub groups
  ShowFileName;
  // update all nodes with file names
  TVPG.BeginUpdate;
  TVNode:=TVPG.Items.GetFirstNode;
  while TVNode<>nil do begin
    NodeData:=TNodeData(TVNode.Data);
    if NodeData is TNodeData then begin
      if NodeData.NodeType in [ntTarget] then begin
        TVNode.Text:=DisplayFileName(NodeData);
      end;
    end;
    TVNode:=TVNode.GetNext;
  end;
  TVPG.EndUpdate;
end;

function TProjectGroupEditorForm.CreateSectionNode(AParent: TTreeNode;
  const ACaption: String; ANodeType: TNodeType): TTreeNode;
Var
  ND: TNodeData;
begin
  ND:=TNodeData.Create;
  ND.NodeType:=ANodeType;
  Result:=TVPG.Items.AddChild(AParent,ACaption);
  InitTVNode(Result,'',ND);
end;

function TProjectGroupEditorForm.CreateTargetNode(AParent: TTreeNode;
  ANodeType: TNodeType; aTarget: TPGCompileTarget): TTreeNode;
var
  ND: TNodeData;
begin
  ND:=TNodeData.Create;
  ND.NodeType:=ANodeType;
  ND.Target:=aTarget;
  if aTarget<>nil then
    ND.ParentTarget:=aTarget.Parent;
  Result:=TVPG.Items.AddChild(AParent,DisplayFileName(ND));
  InitTVNode(Result,'',ND);
end;

function TProjectGroupEditorForm.CreateSubNode(AParent: TTreeNode;
  ANodeType: TNodeType; aParentTarget: TPGCompileTarget; aValue: string
  ): TTreeNode;
var
  ND: TNodeData;
  aCaption: String;
begin
  ND:=TNodeData.Create;
  ND.NodeType:=ANodeType;
  ND.ParentTarget:=aParentTarget;
  ND.Value:=aValue;
  aCaption:=aValue;
  if ANodeType=ntFile then
    aCaption:=CreateRelativePath(aCaption,ExtractFilePath(aParentTarget.Filename));
  Result:=TVPG.Items.AddChild(AParent,aCaption);
  InitTVNode(Result,'',ND);
end;

procedure TProjectGroupEditorForm.ClearChildNodes(TVNode: TTreeNode);

  procedure FreeChildrenNodeData(aTVNode: TTreeNode);
  var
    i: Integer;
    ChildNode: TTreeNode;
  begin
    if aTVNode=nil then exit;
    for i:=0 to aTVNode.Count-1 do
    begin
      ChildNode:=aTVNode[i];
      ClearNodeData(ChildNode);
      FreeChildrenNodeData(ChildNode);
    end;
  end;

begin
  FreeChildrenNodeData(TVNode);
  TVNode.DeleteChildren;
end;

function TProjectGroupEditorForm.DisplayFileName(aTarget: TPGCompileTarget
  ): string;
var
  BaseDir: String;
begin
  Result:='';
  if aTarget=nil then exit('?');
  if IDEProjectGroupManager.Options.ShowTargetPaths then
  begin
    if aTarget.Parent<>nil then
      BaseDir:=ExtractFilePath(aTarget.Parent.Filename)
    else
      BaseDir:='';
    Result:=aTarget.Filename;
    if Result='' then
      Result:='?'
    else
      Result:=CreateRelativePath(Result,BaseDir);
  end else begin
    //debugln(['TProjectGroupEditorForm.DisplayFileName ',aTarget.Filename,' ',aTarget.TargetType=ttPascalFile]);
    if aTarget.TargetType in [ttPascalFile] then
      Result:=ExtractFileName(aTarget.Filename)
    else
      Result:=ExtractFileNameOnly(aTarget.Filename);
  end;
end;

function TProjectGroupEditorForm.DisplayFileName(Node: TTreeNode): string;
begin
  Result:='';
  if (Node=nil) or (Node.Data=nil) then exit;
  Result:=DisplayFileName(TNodeData(Node.Data));
end;

function TProjectGroupEditorForm.DisplayFileName(NodeData: TNodeData): string;
begin
  if NodeData=nil then exit('');
  Result:=DisplayFileName(NodeData.Target);
end;

procedure TProjectGroupEditorForm.ShowFileName;
Var
  N: String;
begin
  if FProjectGroup=nil then
    N:=''
  else
    N:=FProjectGroup.FileName;
  if (N='') then
    Caption:=lisNewProjectGroup
  else
    Caption:=Format(LisProjectGroup,[DisplayFileName(FProjectGroup.SelfTarget)]);
  if Assigned(FProjectGroupTVNode) then
    FProjectGroupTVNode.Text:=DisplayFileName(FProjectGroupTVNode);
end;

function TProjectGroupEditorForm.FindTVNodeOfTarget(ATarget: TPGCompileTarget): TTreeNode;
Var
  I: Integer;
begin
  Result:=Nil;
  if ATarget=nil then exit;
  I:=0;
  While (Result=Nil) and (I<TVPG.Items.Count) do
  begin
    Result:=TVPG.Items[I];
    If Not (Assigned(Result.Data) and (TNodeData(Result.Data).Target=ATarget)) then
      Result:=Nil;
    Inc(I);
  end;
end;

function TProjectGroupEditorForm.FindBuildModeNodeRecursively(
  TVNode: TTreeNode; aMode: string): TTreeNode;
var
  ND: TNodeData;
begin
  Result:=nil;
  if TVNode=nil then exit;
  if (TVNode.Data=nil) then exit;
  ND:=TNodeData(TVNode.Data);
  if (ND.NodeType=ntBuildMode) and (CompareText(ND.Value,aMode)=0) then
    exit(TVNode);
  TVNode:=TVNode.GetFirstChild;
  while TVNode<>nil do begin
    Result:=FindBuildModeNodeRecursively(TVNode,aMode);
    if Result<>nil then exit;
    TVNode:=TVNode.GetNextSibling;
  end;
end;

function TProjectGroupEditorForm.FindTVNodeOfBuildMode(aMode: TPGBuildMode
  ): TTreeNode;
begin
  Result:=nil;
  if aMode=nil then exit;
  // find project node
  Result:=FindTVNodeOfTarget(aMode.Target);
  if Result=nil then exit;
  // find build mdoe node
  Result:=FindBuildModeNodeRecursively(Result,aMode.Identifier);
end;

procedure TProjectGroupEditorForm.ShowProjectGroup;
Var
  N: TTreeNode;
begin
  FLastShowTargetPaths:=IDEProjectGroupManager.Options.ShowTargetPaths;
  TVPG.BeginUpdate;
  try
    FreeNodeData;
    ShowFileName; // Needs FProjectGroupTVNode
    TVPG.Items.Clear;
    if FProjectGroup<>nil then begin
      FProjectGroupTVNode:=CreateTargetNode(Nil,
        ntProjectGroup,ProjectGroup.SelfTarget);
      FillProjectGroupNode(FProjectGroupTVNode,FProjectGroup);
      N:=FindTVNodeOfTarget(FActiveTarget);
      if (N=Nil) then
      begin
        FActiveTarget:=ProjectGroup.SelfTarget;
        TVPG.Selected:=FProjectGroupTVNode;
      end else
        TVPG.Selected:=N;
    end else begin
      FProjectGroupTVNode:=nil;
    end;
    UpdateStatusBarTargetCount;
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible then
    Localize;
end;

procedure TProjectGroupEditorForm.UpdateNodeTexts;
var
  TVNode: TTreeNode;
begin
  FLastShowTargetPaths:=IDEProjectGroupManager.Options.ShowTargetPaths;
  TVPG.BeginUpdate;
  try
    for TVNode in TVPG.Items do begin
      TVNode.Text:=DisplayFileName(TVNode);
    end;
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.FillProjectGroupNode(TVNode: TTreeNode;
  AProjectGroup: TProjectGroup);
Const
  TNT: Array[Boolean] of TNodeType = (ntTarget,ntMissingTarget);
Var
  T: TPGCompileTarget;
  TN: TTreeNode;
  I: Integer;
begin
  TVPG.BeginUpdate;
  try
    ClearChildNodes(TVNode);
    // 2 Passes: one to show all nodes, one to fill them with target-specific data.
    // Display all nodes
    For I:=0 to AProjectGroup.TargetCount-1 do
    begin
      T:=AProjectGroup.Targets[i];
      CreateTargetNode(TVNode,TNT[T.Missing],T);
    end;
    // Fill all nodes.
    For I:=0 to TVNode.Count-1 do
    begin
      TN:=TVNode.Items[i];
      FillTargetNode(TN,TargetFromNode(TN));
    end;
    TVNode.Expand(False);
  finally
    TVPG.EndUpdate;
  end;
end;

function TProjectGroupEditorForm.ShowDependencies(AParent: TTreeNode;
  T: TPGCompileTarget): TTreeNode;
Var
  i: Integer;
  Pkg: TIDEPackage;
  PkgName: String;
begin
  Result:=CreateSectionNode(AParent,lisNodeDependencies,ntDependencies);
  For i:=0 to T.RequiredPackageCount-1 do
  begin
    PkgName:=T.RequiredPackages[i].PackageName;
    Pkg:=PackageEditingInterface.FindPackageWithName(PkgName);
    if Pkg<>nil then
      PkgName:=Pkg.Name;
    CreateSubNode(Result,ntDependency,T,PkgName);
  end;
end;

procedure TProjectGroupEditorForm.FillProjectNode(TVNode: TTreeNode;
  T: TPGCompileTarget);
Var
  FilesNode: TTreeNode;
  i: Integer;
  BuildModeNode, SubTVNode: TTreeNode;
  aMode: TPGBuildMode;
begin
  TVPG.BeginUpdate;
  try
    ClearChildNodes(TVNode);

    // buildmodes
    if T.BuildModeCount>1 then
    begin
      BuildModeNode:=CreateSectionNode(TVNode,lisNodeBuildModes,ntBuildModes);
      for i:=0 to T.BuildModeCount-1 do
      begin
        aMode:=T.BuildModes[i];
        SubTVNode:=CreateSubNode(BuildModeNode,ntBuildMode,T,aMode.Identifier);
        if aMode.Compile then
          SubTVNode.StateIndex:=NSIChecked
        else
          SubTVNode.StateIndex:=NSIUnchecked;
      end;
    end;
    // files
    FilesNode:=CreateSectionNode(TVNode,lisNodeFiles,ntFiles);
    for i:=0 to T.FileCount-1 do
      CreateSubNode(FilesNode,ntFile,T,T.Files[i]);
    // dependencies
    ShowDependencies(TVNode,T);
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.FillPackageNode(TVNode: TTreeNode;
  T: TPGCompileTarget);
Var
  FilesNode: TTreeNode;
  i: Integer;
begin
  TVPG.BeginUpdate;
  try
    ClearChildNodes(TVNode);
    FilesNode:=CreateSectionNode(TVNode,lisNodeFiles,ntFiles);
    for i:=0 to T.FileCount-1 do
      CreateSubNode(FilesNode,ntFile,T,T.Files[i]);
    ShowDependencies(TVNode,T);
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.FillTargetNode(TVNode: TTreeNode;
  T: TPGCompileTarget);
begin
  TVPG.BeginUpdate;
  try
    ClearChildNodes(TVNode);
    If T=Nil then
      T:=TargetFromNode(TVNode);
    if T=Nil then
      exit;
    case T.TargetType of
      ttProject: FillProjectNode(TVNode,T);
      ttPackage: FillPackageNode(TVNode,T);
      ttProjectGroup: FillProjectgroupNode(TVNode,T.ProjectGroup);
      ttPascalFile: ;
    end;
  finally
    TVPG.EndUpdate;
  end;
end;

function TProjectGroupEditorForm.GetActiveTarget: TPGCompileTarget;
begin
  Result:=FActiveTarget;
end;

class function TProjectGroupEditorForm.TargetFromNode(N: TTreeNode
  ): TPGCompileTarget;
begin
  if (N<>Nil) and (N.Data<>Nil) then
    Result:=TNodeData(N.Data).Target
  else
    Result:=Nil;
end;


end.

