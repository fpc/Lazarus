{
  Todo:
    - show * when modified
    - close windows on IDE close
    - activate project when project is opened
    - deactivate project when project is closed
    - make dockable
}
unit ProjectGroupEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ActnList, LCLProc, LazIDEIntf, PackageIntf, ProjectIntf, ProjectGroupIntf,
  MenuIntf, IDEDialogs, IDEWindowIntf, LazFileUtils, LazLogger,
  ProjectGroupStrConst, ProjectGroup;

type
  TNodeType = (ntUnknown,
               ntProjectGroup,
               ntTargets,
               ntRemovedTargets,
               ntTarget,
               ntRemovedTarget,
               ntFiles,
               ntFile,
               //ntRemovedFiles,
               //ntRemovedFile,
               ntDependencies,
               ntDependency
               //ntRemovedDependencies,
               //ntRemovedDependency
               );

  TNodeData = class(TObject)
    NodeType: TNodeType;
    Target, ParentTarget: TPGCompileTarget;
    Value: string; // ntFile = Filename, ntDependency = PkgName
  end;
  TTargetNodes = Array[Boolean] of TTreeNode;

  { TProjectGroupEditorForm }

  TProjectGroupEditorForm = class(TForm)
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
    ImageListMain: TImageList;
    PMIOPen: TMenuItem;
    PMISaveAs: TMenuItem;
    PMIProperties: TMenuItem;
    PMILater: TMenuItem;
    PMIEarlier: TMenuItem;
    PMIDelete: TMenuItem;
    PMICompileClean: TMenuItem;
    PMICompile: TMenuItem;
    OpenDialogTarget: TOpenDialog;
    PopupMenuMore: TPopupMenu;
    PopupMenuTree: TPopupMenu;
    SaveDialogPG: TSaveDialog;
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
    TVPG: TTreeView;
    procedure ATargetActivateExecute(Sender: TObject);
    procedure ATargetActivateUpdate(Sender: TObject);
    procedure AProjectGroupAddExistingExecute(Sender: TObject);
    procedure ATargetCompileCleanExecute(Sender: TObject);
    procedure ATargetCompileCleanUpdate(Sender: TObject);
    procedure ATargetCompileExecute(Sender: TObject);
    procedure ATargetCompileUpdate(Sender: TObject);
    procedure AProjectGroupDeleteExecute(Sender: TObject);
    procedure AProjectGroupDeleteUpdate(Sender: TObject);
    procedure ATargetInstallExecute(Sender: TObject);
    procedure ATargetInstallUpdate(Sender: TObject);
    procedure ATargetOpenExecute(Sender: TObject);
    procedure ATargetOpenUpdate(Sender: TObject);
    procedure ATargetPropertiesExecute(Sender: TObject);
    procedure ATargetPropertiesUpdate(Sender: TObject);
    procedure ATargetRunExecute(Sender: TObject);
    procedure ATargetRunUpdate(Sender: TObject);
    procedure AProjectGroupSaveAsExecute(Sender: TObject);
    procedure AProjectGroupSaveExecute(Sender: TObject);
    procedure AProjectGroupSaveUpdate(Sender: TObject);
    procedure ATargetEarlierExecute(Sender: TObject);
    procedure ATargetEarlierUpdate(Sender: TObject);
    procedure ATargetLaterExecute(Sender: TObject);
    procedure ATargetLaterUpdate(Sender: TObject);
    procedure ATargetUninstallExecute(Sender: TObject);
    procedure ATargetUninstallUpdate(Sender: TObject);
    procedure OnProjectGroupFileNameChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TVPGDblClick(Sender: TObject);
  private
    FProjectGroup: TProjectGroup;
    FProjectGroupTVNode: TTreeNode;
    FActiveTarget: TPGCompileTarget;
    FTargetNodes: TTargetNodes;
    // Project group callbacks
    procedure InitTVNode(Node: TTreeNode; Const ACaption: String;
      ANodeData: TNodeData);
    procedure OnProjectGroupDestroy(Sender: TObject);
    procedure OnTargetAdded(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetDeleted(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetActivated(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetExchanged(Sender: TObject; Target1, Target2: TPGCompileTarget);
    function AllowPerform(ATargetAction: TPGTargetAction; AAction: TAction= Nil): Boolean;
    procedure ClearEventCallBacks(AProjectGroup: TProjectGroup);
    procedure SetEventCallBacks(AProjectGroup: TProjectGroup);
    // Some helpers
    procedure SetProjectGroup(AValue: TProjectGroup);
    procedure ShowDependencies(AParent: TTreeNode; T: TPGCompileTarget; Out PD: TTargetNodes);
    procedure ShowFileName;
    procedure Perform(ATargetAction: TPGTargetAction);
    function GetActiveTarget: TPGCompileTarget;
    // Treeview Node management
    function FindNodeFromTarget(ATarget: TPGCompileTarget): TTreeNode;
    procedure FreeNodeData;
    class function TargetFromNode(N: TTreeNode): TPGCompileTarget;
    function DisplayFileName(aTarget: TPGCompileTarget): string;
    function DisplayFileName(Node: TTreeNode): string;
    function DisplayFileName(NodeData: TNodeData): string;
    function CreateSectionNode(AParent: TTreeNode; Const ACaption: String; ANodeType: TNodeType): TTreeNode;
    function CreateTargetNode(AParent: TTreeNode; ANodeType: TNodeType; aTarget: TPGCompileTarget): TTreeNode;
    function CreateSubNode(AParent: TTreeNode; ANodeType: TNodeType; aParentTarget: TPGCompileTarget; aValue: string): TTreeNode;
    procedure FillPackageNode(AParent: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectNode(AParent: TTreeNode; T: TPGCompileTarget);
    procedure FillTargetNode(AParent: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectGroupNode(AParent: TTreeNode; AProjectGroup: TProjectGroup; Out TargetNodes: TTargetNodes);
    function GetNodeIndex(ANodeType: TNodeType; ANodeData: TPGCompileTarget ): Integer;
    function SelectedNodeData: TNodeData;
    function SelectedTarget: TPGCompileTarget;
    function GetTVNodeFilename(TVNode: TTreeNode): string;
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

var
  // Nodelist image indexes
  NIProjectGroup             : integer = 0;
  NITargets                  : integer = 1;
  NIRemovedTargerts          : integer = 2;
  NITargetProject            : integer = 3;
  NITargetPackage            : integer = 4;
  NITargetProjectGroup       : integer = 5;
  NIRemovedTargetProject     : integer = 3;
  NIRemovedTargetPackage     : integer = 4;
  NIRemovedTargetProjectGroup: integer = 5;
  NIFiles                    : integer = 16;
  NIFile                     : integer = 17;
  //NIRemovedFiles             : integer = 18;
  //NIRemovedFile              : integer = 17;
  NIDependencies             : integer = 1;
  NIDependency               : integer = 1;
  //NIRemovedDependencies      : integer = 2;
  //NIRemovedDependency        : integer = 2;

  // Node state image index
  NSIActive                  : Integer = 20; // State index for active.

  // Action image indexes
  iiProjectGroupSave         : Integer = -1;
  iiProjectGroupSaveAs       : Integer = -1;
  iiProjectGroupAddExisting  : Integer = -1;
  iiProjectGroupDelete       : Integer = -1;
  iiProjectGroupAddNew       : Integer = -1;
  iiTargetEarlier            : Integer = -1;
  iiTargetLater              : Integer = -1;
  iiTargetCompile            : Integer = -1;
  iiTargetCompileClean       : Integer = -1;
  iiTargetProperties         : Integer = -1;
  iiTargetRun                : Integer = -1;
  iiTargetInstall            : Integer = -1;
  iiTargetUninstall          : Integer = -1;
  iiTargetActivate           : Integer = -1;
  iiTargetOpen               : Integer = -1;

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
  if AProjectGroup is TIDEProjectGroup then
    PG:=TIDEProjectGroup(AProjectGroup)
  else
    exit;
  PG.RemoveAllHandlersOfObject(Self);
  PG.OnFileNameChange:=Nil;
  PG.OnTargetAdded:=Nil;
  PG.OnTargetDeleted:=Nil;
  PG.OnTargetActivated:=Nil;
  PG.OnTargetsExchanged:=Nil;
end;

procedure TProjectGroupEditorForm.SetEventCallBacks(AProjectGroup: TProjectGroup);
Var
  PG: TIDEProjectGroup;
begin
  if AProjectGroup is TIDEProjectGroup then
    PG:=TIDEProjectGroup(AProjectGroup)
  else
    exit;
  PG.AddHandlerOnDestroy(@OnProjectGroupDestroy);
  PG.OnFileNameChange:=@OnProjectGroupFileNameChanged;
  PG.OnTargetAdded:=@OnTargetAdded;
  PG.OnTargetDeleted:=@OnTargetDeleted;
  PG.OnTargetActivated:=@OnTargetActivated;
  PG.OnTargetsExchanged:=@OnTargetExchanged;
end;

procedure TProjectGroupEditorForm.SetProjectGroup(AValue: TProjectGroup);
begin
  debugln(['TProjectGroupEditorForm.SetProjectGroup START ',FProjectGroup=AValue]);
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

  procedure ConfigAction(A: TAction; AImageIndex: Integer; Const ACaption,AHint: String; Mnu: TIDEMenuCommand);
  begin
    A.Caption:=ACaption;
    A.Hint:=AHint;
    if AImageIndex<>-1 then
      A.ImageIndex:=AImageIndex;
    If Assigned(mnu) then
      Mnu.OnClick:=A.OnExecute; // ToDo Enabled and visible don't play well with actions...
  end;

begin
  ConfigAction(AProjectGroupSave,iiProjectGroupSave,lisProjectGroupSaveCaption,lisProjectGroupSaveHint,Nil);
  ConfigAction(AProjectGroupSaveAs,iiProjectGroupSaveAs,lisProjectGroupSaveAsCaption,lisProjectGroupSaveAsHint,Nil);
  ConfigAction(AProjectGroupAddExisting,iiProjectGroupAddExisting,lisProjectGroupAddExistingCaption,lisProjectGroupAddExistingHint,Nil);
  ConfigAction(AProjectGroupDelete,iiProjectGroupDelete,lisProjectGroupDeleteCaption,lisProjectGroupDeleteHint,Nil);
  ConfigAction(AProjectGroupAddNew,iiProjectGroupAddNew,lisProjectGroupAddNewCaption,lisProjectGroupAddNewHint,Nil);
  ConfigAction(ATargetEarlier,iiTargetEarlier,lisTargetEarlierCaption,lisTargetEarlierHint,Nil);
  ConfigAction(ATargetLater,iiTargetLater,lisTargetLaterCaption,lisTargetLaterHint,Nil);
  ConfigAction(ATargetCompile,iiTargetCompile,lisTargetCompileCaption,lisTargetCompileHint,Nil);
  ConfigAction(ATargetCompileClean,iiTargetCompileClean,lisTargetCompileCleanCaption,lisTargetCompileCleanHint,Nil);
  ConfigAction(ATargetProperties,iiTargetProperties,lisTargetPropertiesCaption,lisTargetPropertiesHint,Nil);
  ConfigAction(ATargetRun,iiTargetRun,lisTargetRunCaption,lisTargetRunHint,Nil);
  ConfigAction(ATargetInstall,iiTargetInstall,lisTargetInstallCaption,lisTargetInstallHint,Nil);
  ConfigAction(ATargetUninstall,iiTargetUninstall,lisTargetUninstallCaption,lisTargetUninstallHint,Nil);
  ConfigAction(ATargetActivate,iiTargetActivate,lisTargetActivateCaption,lisTargetActivateHint,Nil);
  ConfigAction(ATargetOpen,iiTargetOpen,lisTargetOpenCaption,lisTargetOpenHint,Nil);
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FProjectGroup<>nil)
    and (FProjectGroup.Modified or (FProjectGroup.FileName=''));
  UpdateIDEMenuCommandFromAction(Sender,cmdSaveProjectGroup);
end;

procedure TProjectGroupEditorForm.ATargetEarlierExecute(Sender: TObject);
Var
  T: TNodeData;
  I,J: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) then
    exit;
  PG:=T.Target.ParentProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  J:=I-1;
  if J>=0 then
    PG.ExchangeTargets(I,J);
end;

procedure TProjectGroupEditorForm.ATargetEarlierUpdate(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  I:=-1;
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) then
    exit;
  PG:=T.Target.ParentProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  (Sender as TAction).Enabled:=I>0;
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetEarlier);
end;

procedure TProjectGroupEditorForm.ATargetLaterExecute(Sender: TObject);
Var
  T: TNodeData;
  I,J: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) then
    exit;
  PG:=T.Target.ParentProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  J:=I+1;
  if (J<PG.TargetCount) then
    PG.ExchangeTargets(I,J);
end;

procedure TProjectGroupEditorForm.ATargetLaterUpdate(Sender: TObject);
Var
  T: TNodeData;
  I: Integer;
  PG: TProjectGroup;
begin
  T:=SelectedNodeData;
  if (T=nil) or (T.Target=nil) then
    exit;
  PG:=T.Target.ParentProjectGroup;
  if PG=nil then exit;
  I:=PG.IndexOfTarget(T.Target);
  (Sender as TAction).Enabled:=I+1<PG.TargetCount;
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetLater);
end;

procedure TProjectGroupEditorForm.ATargetUninstallExecute(Sender: TObject);
begin
  Perform(taInstall);
end;

procedure TProjectGroupEditorForm.ATargetUninstallUpdate(Sender: TObject);
begin
  AllowPerform(taUninstall,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetUninstall);
end;

procedure TProjectGroupEditorForm.OnProjectGroupFileNameChanged(Sender: TObject);
var
  TVNode: TTreeNode;
  NodeData: TNodeData;
begin
  ShowFileName;
  // ToDo: update nodes
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
  PGEditMenuSectionMisc.MenuItem:=PopupMenuMore.Items;
  SetItem(cmdTargetAdd,@AProjectGroupAddExistingExecute);
  SetItem(cmdTargetRemove,@AProjectGroupDeleteExecute);
  SetItem(cmdTargetCompile,@ATargetCompileExecute);
  SetItem(cmdTargetCompileClean,@ATargetCompileCleanExecute);
  SetItem(cmdTargetInstall,@ATargetInstallExecute);
  SetItem(cmdTargetUnInstall,@ATargetUnInstallExecute);
  SetItem(cmdTargetLater,@ATargetLaterExecute);
  SetItem(cmdTargetEarlier,@ATargetEarlierExecute);
end;

procedure TProjectGroupEditorForm.FormDestroy(Sender: TObject);
begin
  if ProjectGroupEditorForm=Self then
    ProjectGroupEditorForm:=nil;
end;

procedure TProjectGroupEditorForm.TVPGDblClick(Sender: TObject);
Var
  ND: TNodeData;
  aFilename: String;
begin
  ND:=SelectedNodeData;
  debugln(['TProjectGroupEditorForm.TVPGDblClick ',DbgSName(Sender),' ',TVPG.Selected.Text,' ',ND<>nil]);
  if ND=nil then exit;
  case ND.NodeType of
  ntUnknown: ;
  ntProjectGroup: ;
  ntTargets: ;
  ntRemovedTargets: ;
  ntTarget: ;
  ntRemovedTarget: ;
  ntFiles: ;
  ntFile:
    begin
      aFilename:=GetTVNodeFilename(TVPG.Selected);
      debugln(['TProjectGroupEditorForm.TVPGDblClick File=',aFilename]);
      if aFilename='' then exit;
      LazarusIDE.DoOpenEditorFile(aFilename,-1,-1,[ofAddToRecent]);
    end;
  ntDependencies: ;
  ntDependency: ;
  end;
  {if (ND<>nil) and (ND.NodeType<>ntTarget) then
  begin
    ND.ProjectGroup.ActivateTarget(ND.Target);
    if (ND.ProjectGroup<>FProjectGroup) then // No callback, fake it.
      OnTargetActivated(ND.ProjectGroup,ND.Target);
  end;}
end;

procedure TProjectGroupEditorForm.OnTargetAdded(Sender: TObject;
  Target: TPGCompileTarget);
Var
  N: TTreeNode;
begin
  // ToDo: use of FTargetNodes is wrong if PG<>FProjectGroup
  N:=CreateTargetNode(FTargetNodes[False],ntTarget,Target);
  FillTargetNode(N,Target);
  TVPG.Selected:=N;
  UpdateStatusBarTargetCount;
end;

procedure TProjectGroupEditorForm.OnTargetDeleted(Sender: TObject;
  Target: TPGCompileTarget);
Var
  N: TTreeNode;
begin
  N:=FindNodeFromTarget(Target);
  TVPG.Items.Delete(N);
  // ToDo: The use of FTargetNodes is not correct when PG<>FProjectGroup
  CreateTargetNode(FTargetNodes[True],ntRemovedTarget,Target);
  TVPG.Selected:=FProjectGroupTVNode;
  UpdateStatusBarTargetCount;
end;

procedure TProjectGroupEditorForm.OnTargetActivated(Sender: TObject;
  Target: TPGCompileTarget);
Var
  OldActiveTVNode,NewActiveTVNode: TTreeNode;
  N: String;
begin
  OldActiveTVNode:=FindNodeFromTarget(FActiveTarget);
  NewActiveTVNode:=FindNodeFromTarget(Target);
  if (OldActiveTVNode<>NewActiveTVNode) then
  begin
    if Assigned(OldActiveTVNode) then
      OldActiveTVNode.StateIndex:=-1;
    if Assigned(NewActiveTVNode) then
      NewActiveTVNode.StateIndex:=NSIActive;
    FActiveTarget:=Target;
  end;
  N:=DisplayFileName(Target);
  SBPG.Panels[piActiveTarget].Text:=Format(lisActiveTarget,[N]);
end;

procedure TProjectGroupEditorForm.OnTargetExchanged(Sender: TObject; Target1,
  Target2: TPGCompileTarget);
Var
  S,N1,N2: TTreeNode;
  ND1,ND2: TNodeData;
  NT1,NT2: TCaption;
begin
  N1:=FindNodeFromTarget( Target1);
  N2:=FindNodeFromTarget(Target2);
  If (N1=Nil) or (N2=Nil) then
    exit;
  ND1:=TNodeData(N1.Data);
  if TVPG.Selected=N1 then
    S:=N2
  else if TVPG.Selected=N2 then
    S:=N1
  else
    S:=Nil;
  NT1:=N1.Text;
  ND2:=TNodeData(N2.Data);
  NT2:=N2.Text;
  InitTVNode(N1,NT2,ND2);
  InitTVNode(N2,NT1,ND1);
  if (S<>Nil) then
    TVPG.Selected:=S;
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveExecute(Sender: TObject);
Var
  P: String;
begin
  if FProjectGroup=nil then exit;
  P:=FProjectGroup.FileName;
  ProjectGroupManager.SaveProjectGroup;
  if CompareFilenames(ExtractFilePath(P),ExtractFilePath(FProjectGroup.FileName))<>0 then
    ShowProjectGroup;
end;

procedure TProjectGroupEditorForm.AProjectGroupAddExistingExecute(Sender: TObject);
var
  aTarget: TIDECompileTarget;
begin
  if FProjectGroup=nil then exit;
  InitIDEFileDialog(OpenDialogTarget);
  With OpenDialogTarget do
  begin
    // TODO: Needs to be fetched from central set of strings
    Filter:='Lazarus projects|*.lpi|Lazarus packages|*.lpk|Lazarus project groups|*.lpg';
    If Execute then
    begin
      aTarget:=FProjectGroup.AddTarget(FileName) as TIDECompileTarget;
      aTarget.LoadTarget(true);
    end;
  end;
  StoreIDEFileDialog(OpenDialogTarget);
end;

procedure TProjectGroupEditorForm.ATargetActivateUpdate(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  T:=SelectedTarget;
  (Sender as TAction).Enabled:=Assigned(T) and Not T.Active;
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetActivate);
end;

procedure TProjectGroupEditorForm.ATargetActivateExecute(Sender: TObject);
Var
  ND: TNodeData;
begin
  ND:=SelectedNodeData;
  if (ND=nil) or (ND.Target=nil) then
    exit;
  ND.Target.ParentProjectGroup.ActivateTarget(ND.Target);
end;

procedure TProjectGroupEditorForm.ATargetCompileCleanExecute(Sender: TObject);
begin
  Perform(taCompileClean);
end;

procedure TProjectGroupEditorForm.ATargetCompileCleanUpdate(Sender: TObject);
begin
  AllowPerform(taCompileClean,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetCompileClean);
end;

function TProjectGroupEditorForm.AllowPerform(ATargetAction: TPGTargetAction; AAction: TAction = Nil): Boolean;
Var
  T: TPGCompileTarget;
begin
  T:=SelectedTarget;
  Result:=Assigned(T) and (ATargetAction in T.AllowedActions);
  If Assigned(AAction) then
    AAction.Enabled:=Result;
end;

procedure TProjectGroupEditorForm.Perform(ATargetAction: TPGTargetAction);
Var
  ND: TNodeData;
begin
  ND:=SelectedNodeData;
  if (ND=nil) or (ND.Target=nil) then exit;
  ND.Target.ParentProjectGroup.Perform(ND.Target,ATargetAction);
end;

procedure TProjectGroupEditorForm.ATargetCompileExecute(Sender: TObject);
begin
  Perform(taCompile);
end;

procedure TProjectGroupEditorForm.ATargetCompileUpdate(Sender: TObject);
begin
  AllowPerform(taCompile,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetCompile);
end;

procedure TProjectGroupEditorForm.AProjectGroupDeleteExecute(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  if FProjectGroup=nil then exit;
  T:=SelectedTarget;
  FProjectGroup.RemoveTarget(T);
end;

procedure TProjectGroupEditorForm.AProjectGroupDeleteUpdate(Sender: TObject);
Var
  T: TPGCompileTarget;
begin
  if FProjectGroup=nil then exit;
  T:=SelectedTarget;
  (Sender as TAction).Enabled:=(T<>Nil) and (T<>ProjectGroup.CompileTarget) and Not T.Removed;
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetRemove);
end;

procedure TProjectGroupEditorForm.ATargetInstallExecute(Sender: TObject);
begin
  Perform(taInstall);
end;

procedure TProjectGroupEditorForm.ATargetInstallUpdate(Sender: TObject);
begin
  AllowPerform(taInstall,Sender as Taction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetInstall);
end;

procedure TProjectGroupEditorForm.ATargetOpenExecute(Sender: TObject);
begin
  Perform(taOpen);
end;

procedure TProjectGroupEditorForm.ATargetOpenUpdate(Sender: TObject);
begin
  AllowPerform(taOpen,Sender as TAction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetOpen);
end;

procedure TProjectGroupEditorForm.ATargetPropertiesExecute(Sender: TObject);
begin
  Perform(taSettings);
end;

procedure TProjectGroupEditorForm.ATargetPropertiesUpdate(Sender: TObject);
begin
  AllowPerform(taSettings,Sender as Taction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetProperties);
end;

procedure TProjectGroupEditorForm.ATargetRunExecute(Sender: TObject);
begin
  Perform(taRun);
end;

procedure TProjectGroupEditorForm.ATargetRunUpdate(Sender: TObject);
begin
  AllowPerform(taRun,Sender as Taction);
  UpdateIDEMenuCommandFromAction(Sender,cmdTargetRun);
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
  FProjectGroupTVNode:=Nil;
  FTargetNodes[False]:=Nil;
  FTargetNodes[True]:=Nil;
  For I:=0 to TVPG.Items.Count-1 do
    begin
    N:=TVPG.Items[I];
    TNodeData(N.Data).Free; // Would be nide to have a FreeAndNilData method in TTreeNode.
    N.Data:=Nil;
    end;
end;

function TProjectGroupEditorForm.GetNodeIndex(ANodeType: TNodeType; ANodeData: TPGCompileTarget): Integer;
begin
  Case ANodeType of
    ntProjectGroup: Result:=NIProjectGroup;
    ntTargets: Result:=NITargets;
    ntRemovedTargets: Result:=NIRemovedTargerts;
    ntTarget :
        Case ANodeData.TargetType of
          ttProject: Result:=NITargetProject;
          ttPackage: Result:=NITargetPackage;
          ttProjectGroup: Result:=NITargetProjectGroup;
        end;
    ntRemovedTarget:
        Case ANodeData.TargetType of
          ttProject: Result:=NIRemovedTargetProject;
          ttPackage: Result:=NIRemovedTargetPackage;
          ttProjectGroup: Result:=NIRemovedTargetProjectGroup;
        end;
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
  Node.ImageIndex:=GetNodeIndex(ANodeData.NodeType,ANodeData.Target);
  Node.SelectedIndex:=Node.ImageIndex;
  if Assigned(ANodeData.Target) and ANodeData.Target.Active then
    Node.StateIndex:=NSIActive
  else
    Node.StateIndex:=-1;
end;

procedure TProjectGroupEditorForm.OnProjectGroupDestroy(Sender: TObject);
begin
  if Sender=FProjectGroup then begin
    ProjectGroup:=nil;
  end;
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

function TProjectGroupEditorForm.DisplayFileName(aTarget: TPGCompileTarget
  ): string;
var
  BaseDir: String;
begin
  Result:='';
  if aTarget=nil then exit('?');
  if aTarget.Parent<>nil then
    BaseDir:=ExtractFilePath(aTarget.Parent.Filename)
  else
    BaseDir:='';
  Result:=aTarget.Filename;
  if Result='' then
    Result:='?'
  else
    Result:=CreateRelativePath(Result,BaseDir);
end;

function TProjectGroupEditorForm.DisplayFileName(Node: TTreeNode): string;
begin
  Result:='';
  if (Node=nil) or (Node.Data=nil) then exit;
  Result:=DisplayFileName(TNodeData(Node.Data));
end;

function TProjectGroupEditorForm.DisplayFileName(NodeData: TNodeData): string;
var
  BaseDir: String;
begin
  Result:='';
  if NodeData.ParentTarget<>nil then
    BaseDir:=ExtractFilePath(NodeData.ParentTarget.Filename)
  else
    BaseDir:='';
  if NodeData.Target<>nil then
    Result:=NodeData.Target.Filename;
  debugln(['TProjectGroupEditorForm.DisplayFileName ',dbgs(NodeData.NodeType),' BaseDir=',BaseDir,' File=',Result]);
  if Result='' then
    Result:='?'
  else
    Result:=CreateRelativePath(Result,BaseDir);
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
    Caption:=Format(LisProjectGroup,[DisplayFileName(FProjectGroup.CompileTarget)]);
  if Assigned(FProjectGroupTVNode) then
    FProjectGroupTVNode.Text:=DisplayFileName(FProjectGroupTVNode);
end;

function TProjectGroupEditorForm.FindNodeFromTarget(ATarget: TPGCompileTarget): TTreeNode;
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

procedure TProjectGroupEditorForm.ShowProjectGroup;
Var
  N: TTreeNode;
begin
  TVPG.BeginUpdate;
  try
    ShowFileName; // Needs FProjectGroupTVNode
    FreeNodeData;
    TVPG.Items.Clear;
    FTargetNodes[False]:=Nil;
    FTargetNodes[True]:=Nil;
    if FProjectGroup<>nil then begin
      FProjectGroupTVNode:=CreateTargetNode(Nil,
        ntProjectGroup,ProjectGroup.CompileTarget);
      FillProjectGroupNode(FProjectGroupTVNode,FProjectGroup,FTargetNodes);
      N:=FindNodeFromTarget(FActiveTarget);
      if (N=Nil) then
      begin
        FActiveTarget:=ProjectGroup.CompileTarget;
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

procedure TProjectGroupEditorForm.FillProjectGroupNode(AParent: TTreeNode;
  AProjectGroup: TProjectGroup; out TargetNodes: TTargetNodes);
Const
  TNT: Array[Boolean] of TNodeType = (ntTarget,ntRemovedTarget);
Var
  T: TPGCompileTarget;
  TTN,TN: TTreeNode;
  I: Integer;
begin
  TVPG.BeginUpdate;
  try
    TTN:=CreateSectionNode(AParent,lisNodeTargets,ntTargets);
    TargetNodes[False]:=TTN;
    TargetNodes[True]:=CreateSectionNode(AParent,lisNodeRemovedTargets,ntTargets);
    // 2 Passes: one to show all nodes, one to fill them with target-specific data.
    // Display all nodes
    For I:=0 to AProjectGroup.TargetCount-1 do
    begin
      T:=AProjectGroup.Targets[i];
      TN:=CreateTargetNode(TargetNodes[T.Removed],TNT[T.Removed],T);
    end;
    // Fill all nodes.
    For I:=0 to TTN.Count-1 do
    begin
      TN:=TTN.Items[i];
      FillTargetNode(TN,TargetFromNode(TN));
    end;
    AParent.Expand(False);
    TargetNodes[False].Expand(False);
    TargetNodes[True].Expand(False);
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.ShowDependencies(AParent: TTreeNode;
  T: TPGCompileTarget; out PD: TTargetNodes);
Var
  i: Integer;
  Pkg: TIDEPackage;
  PkgName: String;
begin
  PD[False]:=CreateSectionNode(AParent,lisNodeDependencies,ntDependencies);
  PD[True]:=nil; //CreateNode(AParent,lisNodeRemovedDependencies,ntRemovedDependencies,Nil,AProjectGroup);
  For i:=0 to T.RequiredPackageCount-1 do
  begin
    PkgName:=T.RequiredPackages[i].PackageName;
    Pkg:=PackageEditingInterface.FindPackageWithName(PkgName);
    if Pkg<>nil then
      PkgName:=Pkg.Name;
    CreateSubNode(PD[False],ntDependency,T,Pkg.Name);
  end;
end;

procedure TProjectGroupEditorForm.FillProjectNode(AParent: TTreeNode;
  T: TPGCompileTarget);
Var
  PF,PD: TTargetNodes;
  i: Integer;
begin
  TVPG.BeginUpdate;
  try
    PF[False]:=CreateSectionNode(AParent,lisNodeFiles,ntFiles);
    PF[True]:=nil; //CreateNode(AParent,lisNodeRemovedFiles,ntFiles,Nil,AProjectGroup);
    // TODO Ideally, we can show removed files
    for i:=0 to T.FileCount-1 do
      CreateSubNode(PF[False],ntFile,T,T.Files[i]);
    ShowDependencies(AParent,T,PD);
    // TODO: Build mode info Not available ?
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.FillPackageNode(AParent: TTreeNode;
  T: TPGCompileTarget);
Var
  PF,PD: TTargetNodes;
  i: Integer;
begin
  TVPG.BeginUpdate;
  try
    PF[False]:=CreateSectionNode(AParent,lisNodeFiles,ntFiles);
    PF[True]:=nil; //CreateNode(AParent,lisNodeRemovedFiles,ntFiles,Nil,AProjectGroup);
    // ToDo Ideally, we can show removed files
    for i:=0 to T.FileCount-1 do
      CreateSubNode(PF[False],ntFile,T,T.Files[i]);
    ShowDependencies(AParent,T,PD);
  finally
    TVPG.EndUpdate;
  end;
end;

procedure TProjectGroupEditorForm.FillTargetNode(AParent: TTreeNode;
  T: TPGCompileTarget);
Var
  PN: TTargetNodes;
begin
  TVPG.BeginUpdate;
  try
    If T=Nil then
      T:=TargetFromNode(AParent);
    if T=Nil then
      exit;
    Case T.TargetType of
      ttProject: FillProjectNode(AParent,T);
      ttPackage: FillPackageNode(AParent,T);
      ttProjectGroup: FillProjectgroupNode(AParent,T.ParentProjectGroup,PN);
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

