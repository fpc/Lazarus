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
  ActnList, LCLProc, Clipbrd,
  // LazUtils
  LazFileUtils, LazLoggerBase, LazFileCache,
  // IdeIntf
  LazIDEIntf, PackageIntf, ProjectIntf, ProjectGroupIntf, MenuIntf, IDEWindowIntf,
  IDEDialogs,
  // ProjectGroups
  ProjectGroupStrConst, ProjectGroup;

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
    ImageListMain: TImageList;
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
    TBReload: TToolButton;
    TVPG: TTreeView;
    procedure AProjectGroupAddExistingExecute(Sender: TObject);
    procedure AProjectGroupDeleteExecute(Sender: TObject);
    procedure AProjectGroupDeleteUpdate(Sender: TObject);
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
    procedure PopupMenuMorePopup(Sender: TObject);
    procedure TVPGDblClick(Sender: TObject);
    procedure TVPGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVPGSelectionChanged(Sender: TObject);
  private
    FProjectGroup: TProjectGroup;
    FProjectGroupTVNode: TTreeNode;
    FActiveTarget: TPGCompileTarget;
    // Project group callbacks
    procedure InitTVNode(Node: TTreeNode; Const ACaption: String;
      ANodeData: TNodeData);
    procedure OnProjectGroupDestroy(Sender: TObject);
    procedure OnProjectGroupFileNameChanged(Sender: TObject);
    procedure OnTargetInserted(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetDeleted(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetActiveChanged(Sender: TObject; Target: TPGCompileTarget);
    procedure OnTargetExchanged(Sender: TObject; Target1, Target2: TPGCompileTarget);
    function AllowPerform(ATargetAction: TPGTargetAction; AAction: TAction= Nil): Boolean;
    procedure ClearEventCallBacks(AProjectGroup: TProjectGroup);
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
    procedure ClearChildNodes(TVNode: TTreeNode);
    procedure FillPackageNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillTargetNode(TVNode: TTreeNode; T: TPGCompileTarget);
    procedure FillProjectGroupNode(TVNode: TTreeNode; AProjectGroup: TProjectGroup);
    function GetNodeImageIndex(ANodeType: TNodeType; ANodeData: TPGCompileTarget ): Integer;
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
  NITargetProject            : integer = 3;
  NITargetPackage            : integer = 4;
  NITargetProjectGroup       : integer = 5;
  NIMissingTargetProject     : integer = 3;
  NIMissingTargetPackage     : integer = 4;
  NIMissingTargetProjectGroup: integer = 5;
  NIBuildModes               : integer = 12;
  NIBuildMode                : integer = 12;
  NIFiles                    : integer = 16;
  NIFile                     : integer = 17;
  NIDependencies             : integer = 1;
  NIDependency               : integer = 1;

  // Node state image index
  NSIActive                  : Integer = 20; // State index for active.

  // overlay index
  NSIChecked                 : Integer = 22;
  NSIUnchecked               : Integer = 23;

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
  //debugln(['TProjectGroupEditorForm.ClearEventCallBacks ']);
  PG:=AProjectGroup as TIDEProjectGroup;
  PG.RemoveAllHandlersOfObject(Self);
  PG.OnFileNameChange:=Nil;
  PG.OnTargetInserted:=Nil;
  PG.OnTargetDeleted:=Nil;
  PG.OnTargetActiveChanged:=Nil;
  PG.OnTargetsExchanged:=Nil;
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

  procedure ConfigAction(A: TAction; AImageIndex: Integer; Const ACaption,AHint: String; Mnu: TIDEMenuCommand);
  begin
    A.Caption:=ACaption;
    A.Hint:=AHint;
    if AImageIndex<>-1 then
      A.ImageIndex:=AImageIndex;
    If Assigned(mnu) then
      Mnu.OnClick:=A.OnExecute;
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
  ConfigAction(ATargetCopyFilename,0,lisTargetCopyFilename,'',Nil);
  ConfigAction(ATargetCompileFromHere,0,lisTargetCompileFromHere,'',Nil);
  ConfigAction(AProjectGroupReload,0,lisProjectGroupReload,'',Nil);
  TBMore.Caption:=lisMore;
end;

procedure TProjectGroupEditorForm.AProjectGroupSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FProjectGroup<>nil)
    and (FProjectGroup.Modified or (FProjectGroup.FileName=''));
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
end;

procedure TProjectGroupEditorForm.FormDestroy(Sender: TObject);
begin
  debugln(['TProjectGroupEditorForm.FormDestroy START ',ProjectGroup<>nil]);
  ProjectGroup:=nil;
  if ProjectGroupEditorForm=Self then
    ProjectGroupEditorForm:=nil;
  if (PGEditMenuSectionMisc<>nil)
  and (PGEditMenuSectionMisc.MenuItem=PopupMenuMore.Items) then
    PGEditMenuSectionMisc.MenuItem:=nil;
  debugln(['TProjectGroupEditorForm.FormDestroy END ',ProjectGroup<>nil]);
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
      if (TVNode.DisplayStateIconLeft<X) and (X<TVNode.DisplayIconLeft) then
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
  aTarget: TIDECompileTarget;
  aMode: TPGBuildMode;
  TVNode: TTreeNode;
  i: Integer;
begin
  if FProjectGroup=nil then exit;
  aTarget:=TIDECompileTarget(SelectedTarget);
  InitIDEFileDialog(OpenDialogTarget);
  OpenDialogTarget.Filter := lisLazarusProjectsLpi + '|*.lpi'
   + '|' + lisLazarusPackagesLpk + '|*.lpk'
   + '|' + lisLazarusProjectGroupsLpg + '|*.lpg'
   + '|' + lisPascalFilePasPpP + '|*.pas;*.pp;*.p';
  If OpenDialogTarget.Execute then
    for i:=0 to OpenDialogTarget.Files.Count-1 do
    begin
      aTarget:=FProjectGroup.AddTarget(OpenDialogTarget.Files[i]) as TIDECompileTarget;
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
  StoreIDEFileDialog(OpenDialogTarget);
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
      exit;
    end;
    ProjectGroup:=nil;
    try
      PG.LoadFromFile([pgloLoadRecursively]);
    finally
      ProjectGroup:=PG;
    end;
  end;
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
  (Sender as TAction).Enabled:=(T<>nil) and (T<>ProjectGroup.CompileTarget);
  UpdateIDEMenuCommandFromAction(Sender,MnuCmdTargetRemove);
end;

procedure TProjectGroupEditorForm.AProjectGroupRedoExecute(Sender: TObject);
begin
  // ToDo
  writeln('TProjectGroupEditorForm.AProjectGroupRedoExecute Todo');
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
    ntTarget :
        Case ANodeData.TargetType of
          ttProject: Result:=NITargetProject;
          ttPackage: Result:=NITargetPackage;
          ttProjectGroup: Result:=NITargetProjectGroup;
          ttPascalFile: Result:=NIFile;
        end;
    ntMissingTarget:
        Case ANodeData.TargetType of
          ttProject: Result:=NIMissingTargetProject;
          ttPackage: Result:=NIMissingTargetPackage;
          ttProjectGroup: Result:=NIMissingTargetProjectGroup;
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
      if ChildNode.Data<>nil then
      begin
        TObject(ChildNode.Data).Free;
        ChildNode.Data:=nil;
      end;
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
var
  BaseDir: String;
begin
  if (NodeData.Target<>nil)
  and (not IDEProjectGroupManager.Options.ShowTargetPaths) then
  begin
    if NodeData.Target.TargetType in [ttPascalFile] then
      Result:=ExtractFileName(NodeData.Target.Filename)
    else
      Result:=ExtractFileNameOnly(NodeData.Target.Filename);
  end else begin
    Result:='';
    if NodeData.ParentTarget<>nil then
      BaseDir:=ExtractFilePath(NodeData.ParentTarget.Filename)
    else
      BaseDir:='';
    if NodeData.Target<>nil then
      Result:=NodeData.Target.Filename;
    if Result='' then
      Result:='?'
    else
      Result:=CreateRelativePath(Result,BaseDir);
  end;
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
  TVPG.BeginUpdate;
  try
    FreeNodeData;
    ShowFileName; // Needs FProjectGroupTVNode
    TVPG.Items.Clear;
    if FProjectGroup<>nil then begin
      FProjectGroupTVNode:=CreateTargetNode(Nil,
        ntProjectGroup,ProjectGroup.CompileTarget);
      FillProjectGroupNode(FProjectGroupTVNode,FProjectGroup);
      N:=FindTVNodeOfTarget(FActiveTarget);
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

