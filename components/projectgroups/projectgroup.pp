{
  ToDo:
    - menu item  View -> Project Groups
    - update files when project/package/file changes in IDE
    - update dependencies when changed in IDE
    - update when files changed on disk
    - show active build mode, active project
    - upate menu items enabled state
    - "find" as in the Messages window
    - find in files
    - options: show file names with relative paths
    - options: show icons, show text, show icons+text
    - "New" button to create a package/project/file and add to project groups
    - clean function, like the Run / Clean up and build dialog
    - drag and drop within the editor
      - order targets
      - move targets between sub groups
      - move file to another project
    - save session in project group, allowing to quickly switch the active project
    - load sub projects in IDE to use code navigation for files not in the active project
    - find references in files

    - menu item: open project in new IDE instance
    - add menu items for project for all project inspector functions.
    - add menu items for package for all package editor functions.
    - add root node for packages: when opening a package editor add node instead
    - multiple project groups in editor
}
unit ProjectGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, LazConfigStorage, Laz2_XMLCfg,
  LazTracer, LazUtilities, AvgLvlTree, LazStringUtils,
  // LCL
  Controls, Forms, Dialogs,
  // CodeTools
  FileProcs, CodeToolManager, CodeCache,
  // IdeIntf
  PackageIntf, ProjectIntf, MenuIntf, LazIDEIntf, IDEDialogs, CompOptsIntf,
  BaseIDEIntf, IDECommands, IDEExternToolIntf, MacroIntf, IDEMsgIntf,
  ToolBarIntf, MacroDefIntf, PackageDependencyIntf, PackageLinkIntf,
  // ProjectGroups
  ProjectGroupIntf, ProjectGroupStrConst;

const
  PGOptionsFileName = 'projectgroupsoptions.xml';
  PGFileVersion = 1;

type
  { TIDECompileTarget }

  TIDECompileTarget = class(TPGCompileTarget)
  private
    FBuildModes: TObjectList;
    FFiles: TStringList;
    FRequiredPackages: TObjectList; // list of TPGDependency
    function CheckIDEIsReadyForBuild: boolean;
    function CompileUsingLazBuild(const AAction: TPGTargetAction; aBuildMode: string = ''): TPGActionResult;
  protected
    function GetBuildModeCount: integer; override;
    function GetBuildModes(Index: integer): TPGBuildMode; override;
    function GetFileCount: integer; override;
    function GetFiles(Index: integer): string; override;
    function GetRequiredPackageCount: integer; override;
    function GetRequiredPackages(Index: integer): TPGDependency; override;
    procedure LoadPackage;
    procedure LoadProject;
    procedure LoadProject_GroupSettings(XMLConfig: TXMLConfig; aPath: string);
    procedure SaveProject_GroupSettings(XMLConfig: TXMLConfig; aPath: string);
    procedure LoadProjectGroup(Recursively: boolean);
    function ProjectAction(AAction: TPGTargetAction; StartBuildMode: string = ''): TPGActionResult;
    function PackageAction(AAction: TPGTargetAction): TPGActionResult;
    function ProjectGroupAction(AAction: TPGTargetAction): TPGActionResult;
    function PascalFileAction(AAction: TPGTargetAction): TPGActionResult;
    function ExternalToolAction(AAction: TPGTargetAction): TPGActionResult;
    function PerformAction(AAction: TPGTargetAction): TPGActionResult; override;
    function PerformNextTarget(AAction: TPGTargetAction): TPGActionResult;
    procedure ActiveChanged(Sender: TPGCompileTarget); override;
    procedure SetParent(NewParent: TPGCompileTarget); virtual;
  public
    destructor Destroy; override;
    procedure LoadTarget(Recursively: boolean); virtual;
    procedure LoadGroupSettings(XMLConfig: TXMLConfig; aPath: string);
    procedure SaveGroupSettings(XMLConfig: TXMLConfig; aPath: string);
    procedure UnLoadTarget; virtual;
    procedure Modified; override;
    function PerformBuildModeAction(AAction: TPGTargetAction;
      aModeIdentifier: string): TPGActionResult; override;
  end;

  // Since a project group iself is also a target, we need a target to represent
  // the root projectgroup.

  { TRootProjectGroupTarget }

  TRootProjectGroupTarget = class(TIDECompileTarget)
  protected
    procedure SetTargetType(AValue: TPGTargetType); override;
  public
    constructor Create(aOwner: TProjectGroup);
  end;

  TTargetEvent = procedure(Sender: TObject; Target: TPGCompileTarget) of object;
  TTargetExchangeEvent = procedure(Sender: TObject; Target1,Target2: TPGCompileTarget) of object;

  { TIDEProjectGroup }

  TIDEProjectGroup = class(TProjectGroup)
  private
    FActiveTarget: TPGCompileTarget;
    FOnFileNameChange: TNotifyEvent;
    FOnTargetActiveChanged: TTargetEvent;
    FOnTargetInserted: TTargetEvent;
    FOnTargetDeleted: TTargetEvent;
    FOnTargetsExchanged: TTargetExchangeEvent;
    FTargets: TFPObjectList;
  protected
    procedure SetFileName(AValue: String); override;
    function GetTarget(Index: Integer): TPGCompileTarget; override;
    function GetTargetCount: Integer; override;
    function GetActiveTarget: TPGCompileTarget; override;
    procedure SetActiveTarget(AValue: TPGCompileTarget); override;
    procedure DoTargetDeleted(Sender: TObject; Target: TPGCompileTarget);
    procedure DoTargetInserted(Sender: TObject; Target: TPGCompileTarget);
  public
    constructor Create(aCompileTarget: TIDECompileTarget);
    destructor Destroy; override;
    procedure Clear;
    procedure CheckInvalidCycle(const aFilename: string);
    function IndexOfTarget(const Target: TPGCompileTarget): Integer; override;
    function AddTarget(Const AFileName: String): TPGCompileTarget; override;
    function InsertTarget(const Target: TPGCompileTarget; Index: Integer
      ): Integer; override;
    procedure RemoveTarget(Index: Integer); override;
    procedure ExchangeTargets(OldIndex, NewIndex: Integer); override;
    procedure ActiveTargetChanged(T: TPGCompileTarget);
    function UpdateMissing: boolean; override;
    function LoadFromFile(Options: TProjectGroupLoadOptions): Boolean;
    function SaveToFile: Boolean;
    property OnFileNameChange: TNotifyEvent Read FOnFileNameChange Write FOnFileNameChange;
    property OnTargetInserted: TTargetEvent Read FOnTargetInserted Write FOnTargetInserted;
    property OnTargetDeleted: TTargetEvent Read FOnTargetDeleted Write FOnTargetDeleted;
    property OnTargetActiveChanged: TTargetEvent Read FOnTargetActiveChanged Write FOnTargetActiveChanged;
    property OnTargetsExchanged: TTargetExchangeEvent Read FOnTargetsExchanged Write FOnTargetsExchanged;
  end;

  { TIDEProjectGroupOptions }

  TIDEProjectGroupOptions = class
  private
    FBuildCommandToCompileTarget: Boolean;
    FChangeStamp: integer;
    FLastGroupFile: string;
    FLastSavedChangeStamp: integer;
    FOpenLastGroupOnStart: Boolean;
    FRecentProjectGroups: TStringList;
    FShowTargetPaths: boolean;
    function GetModified: boolean;
    procedure SetBuildCommandToCompileTarget(const AValue: Boolean);
    procedure SetLastGroupFile(const AValue: string);
    procedure SetModified(AValue: boolean);
    procedure SetOpenLastGroupOnStart(const AValue: Boolean);
    procedure SetShowTargetPaths(AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(aFilename: string);
    procedure LoadFromFile(aFilename: string);
    // changestamp
    procedure IncreaseChangeStamp;
    property ChangeStamp: integer read FChangeStamp;
    property Modified: boolean read GetModified write SetModified;
    // recent project groups
    property RecentProjectGroups: TStringList read FRecentProjectGroups;
    procedure AddToRecentProjectGroups(aFilename: string);
    // misc
    property LastGroupFile: string read FLastGroupFile write SetLastGroupFile;
    property OpenLastGroupOnStart: Boolean read FOpenLastGroupOnStart write SetOpenLastGroupOnStart;
    property ShowTargetPaths: boolean read FShowTargetPaths write SetShowTargetPaths;
    property BuildCommandToCompileTarget: Boolean read FBuildCommandToCompileTarget write SetBuildCommandToCompileTarget;
  end;

  { TPGUndoItem }

  TPGUndoItem = class
  end;

  { TPGUndoDelete }

  TPGUndoDelete = class(TPGUndoItem)
  public
    Group: TIDEProjectGroup;
    Target: TIDECompileTarget; // owned by this undo item
    InFrontFile, BehindFile: string;
    destructor Destroy; override;
  end;

  { TIDEProjectGroupManager }

  TIDEProjectGroupManager = Class(TProjectGroupManager)
  private
    FIdleConnected: boolean;
    FOnEditorOptionsChanged: TNotifyEvent;
    FUndoList: TObjectList; // list of TPGUndoItem
    FRedoList: TObjectList; // list of TPGUndoItem
    FOptions: TIDEProjectGroupOptions;
    procedure AddToRecentGroups(aFilename: string);
    function GetNewFileName: Boolean;
    function GetPGSrcPaths(const s: string; const {%H-}Data: PtrInt;
      var Abort: boolean): string;
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure SetIdleConnected(const AValue: boolean);
    procedure AddSrcPathOfFile(SrcPaths: TFilenameToStringTree; Filename: string);
    procedure AddProjectSrcPaths(Target: TIDECompileTarget; SrcPaths, LPKFiles: TFilenameToStringTree);
    procedure AddPackageSrcPaths(Target: TIDECompileTarget; SrcPaths, LPKFiles: TFilenameToStringTree);
    procedure AddPackageNameSrcPaths(PkgName, PreferredFile, DefaultFile: string; SrcPaths, LPKFiles: TFilenameToStringTree);
    procedure AddLPKSrcPaths(LPKFilename: string; SrcPaths, LPKFiles: TFilenameToStringTree);
    procedure AddGroupSrcPaths(Group: TProjectGroup; SrcPaths, LPKFiles: TFilenameToStringTree);
  protected
    FIDEStarted: boolean;
    FProjectGroup: TIDEProjectGroup;
  protected
    function GetCurrentProjectGroup: TProjectGroup; override;
    function ShowProjectGroupEditor: Boolean;
    procedure TargetDeleting(Group: TIDEProjectGroup; Index: integer);
    function GroupExists(Group: TIDEProjectGroup): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateRecentProjectGroupMenu;
    function CheckSaved: Boolean;
    // Events for main menu
    procedure DoNewClick(Sender: TObject);
    procedure DoOpenClick(Sender: TObject);
    procedure DoOpenRecentClick(Sender: TObject);
    procedure DoSaveClick(Sender: TObject);
    procedure DoSaveAsClick(Sender: TObject);
    // Public interface
    function CanUndo: boolean; override;
    function CanRedo: boolean; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); override;
    procedure SaveProjectGroup; override;
    function GetSrcPaths: string; override;
  public
    property Options: TIDEProjectGroupOptions read FOptions;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property OnEditorOptionsChanged: TNotifyEvent read FOnEditorOptionsChanged write FOnEditorOptionsChanged;
  end;

  TEditProjectGroupHandler = procedure(Sender: TObject; AProjectGroup: TProjectGroup);
  // Method variant.
  TEditProjectGroupEvent = procedure(Sender: TObject; AProjectGroup: TProjectGroup) of object;

var
  OnShowProjectGroupEditor: TEditProjectGroupHandler; // Takes precedence
  OnShowProjectGroupEditorEvent: TEditProjectGroupEvent; // method variant

  IDEProjectGroupManager: TIDEProjectGroupManager;

  ViewProjGrpShortCutX: TIDEShortCut;
  ProjectGroupEditorMenuRoot: TIDEMenuSection = nil;
    PGEditMenuSectionFiles, // e.g. sort files, clean up files
    PGEditMenuSectionAddRemove, // e.g. add unit, add dependency
    PGEditMenuSectionCompile, // e.g. build clean, create Makefile
    PGEditMenuSectionUse, // Target up/down
    PGEditMenuSectionMisc: TIDEMenuSection; // e.g. options
  PGOpenRecentSubMenu: TIDEMenuSection;

const
  ProjectGroupCmdCategoryName = 'ProjectGroups';
var
  PGCmdCategory: TIDECommandCategory;

  // IDE main bar menu items
  ViewProjectGroupsCommand: TIDECommand;
  ViewProjectGroupsButtonCommand: TIDEButtonCommand;
  CmdOpenProjectGroup: TIDECommand;
  MnuCmdOpenProjectGroup: TIDEMenuCommand;
  CmdSaveProjectGroup: TIDECommand;
  MnuCmdSaveProjectGroup: TIDEMenuCommand;
  CmdSaveProjectGroupAs: TIDECommand;
  MnuCmdSaveProjectGroupAs: TIDEMenuCommand;
  CmdNewProjectGroup: TIDECommand;
  MnuCmdNewProjectGroup: TIDEMenuCommand;

  // editor menu items
  MnuCmdTargetAdd,
  MnuCmdTargetRemove,
  MnuCmdTargetEarlier,
  MnuCmdTargetActivate,
  MnuCmdTargetLater,
  MnuCmdTargetCompile,
  MnuCmdTargetCompileClean,
  MnuCmdTargetCompileFromHere,
  MnuCmdTargetInstall,
  MnuCmdTargetOpen,
  MnuCmdTargetRun,
  MnuCmdTargetProperties,
  MnuCmdTargetUninstall,
  MnuCmdTargetCopyFilename: TIDEMenuCommand;
  MnuCmdProjGrpUndo: TIDEMenuCommand;
  MnuCmdProjGrpRedo: TIDEMenuCommand;
  MnuCmdProjGrpOptions: TIDEMenuCommand;

function LoadXML(aFilename: string; Quiet: boolean): TXMLConfig;
function CreateXML(aFilename: string; Quiet: boolean): TXMLConfig;
function GetLazBuildFilename: string;

implementation

function LoadXML(aFilename: string; Quiet: boolean): TXMLConfig;
var
  Code: TCodeBuffer;
begin
  Result:=nil;
  aFilename:=TrimFilename(aFilename);
  if (aFilename='') or (not FilenameIsAbsolute(aFilename)) then begin
    debugln(['Error: (lazarus) [TIDECompileTarget.LoadXML] invalid filename "',aFilename,'"']);
    if not Quiet then
      IDEMessageDialog(lisInvalidFile, Format(lisInvalidXmlFileName, [aFilename]), mtError, [mbOk]);
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(aFilename,true,false);
  if Code=nil then begin
    debugln(['Error: (lazarus) [TIDECompileTarget.LoadXML] unable to load file "',aFilename,'"']);
    if not Quiet then
      IDEMessageDialog(lisReadError, Format(lisUnableToLoadFile, [aFilename]), mtError, [mbOk]);
    exit;
  end;
  try
    Result:=TXMLConfig.CreateWithSource(aFilename,Code.Source);
  except
    on E: Exception do begin
      debugln(['Error: (lazarus) [TIDECompileTarget.LoadXML] xml syntax error in "',aFilename,'": '+E.Message]);
      if not Quiet then
        IDEMessageDialog(lisReadError, Format(lisXMLSyntaxErrorInFile, [aFilename, E.Message]), mtError, [mbOk]);
    end;
  end;
end;

function CreateXML(aFilename: string; Quiet: boolean): TXMLConfig;
begin
  Result:=nil;
  aFilename:=TrimFilename(aFilename);
  if (aFilename='') or (not FilenameIsAbsolute(aFilename)) then begin
    debugln(['Error: (lazarus) [TIDECompileTarget.CreateXML] invalid filename "',aFilename,'"']);
    exit;
  end;
  try
    Result:=TXMLConfig.CreateClean(aFilename);
  except
    on E: Exception do begin
      debugln(['Error: (lazarus) [TIDECompileTarget.CreateXML] unable to create file "',aFilename,'": '+E.Message]);
      if not Quiet then
        IDEMessageDialog(lisWriteError, Format(lisUnableToCreateFile, [aFilename, E.Message]), mtError, [mbOk]);
    end;
  end;
end;

function GetLazBuildFilename: string;
begin
  // first check the lazbuild executable in the lazarus directory
  Result:='$(LazarusDir)'+PathDelim+'$MakeExe(lazbuild)';
  IDEMacros.SubstituteMacros(Result);
  if FileExistsCached(Result) then
    exit;
  // then search in PATH
  Result:=FindDefaultExecutablePath('lazbuild'+ExeExt);
end;

{ TPGUndoDelete }

destructor TPGUndoDelete.Destroy;
begin
  FreeAndNil(Target);
  inherited Destroy;
end;

{ TIDEProjectGroupOptions }

function TIDEProjectGroupOptions.GetModified: boolean;
begin
  Result:=FLastSavedChangeStamp<>FChangeStamp
end;

procedure TIDEProjectGroupOptions.SetBuildCommandToCompileTarget(
  const AValue: Boolean);
begin
  if FBuildCommandToCompileTarget=AValue then Exit;
  FBuildCommandToCompileTarget:=AValue;
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroupOptions.SetLastGroupFile(const AValue: string);
begin
  if FLastGroupFile=AValue then Exit;
  FLastGroupFile:=AValue;
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroupOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FLastSavedChangeStamp:=FChangeStamp;
end;

procedure TIDEProjectGroupOptions.SetOpenLastGroupOnStart(const AValue: Boolean
  );
begin
  if FOpenLastGroupOnStart=AValue then Exit;
  FOpenLastGroupOnStart:=AValue;
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroupOptions.SetShowTargetPaths(AValue: boolean);
begin
  if FShowTargetPaths=AValue then Exit;
  FShowTargetPaths:=AValue;
  IncreaseChangeStamp;
end;

constructor TIDEProjectGroupOptions.Create;
begin
  FRecentProjectGroups:=TStringList.Create;
  FOpenLastGroupOnStart:=true;
end;

destructor TIDEProjectGroupOptions.Destroy;
begin
  FreeAndNil(FRecentProjectGroups);
  inherited Destroy;
end;

procedure TIDEProjectGroupOptions.SaveSafe;
begin
  try
    SaveToFile(PGOptionsFileName);
    Modified:=false;
  except
    on E: Exception do
      debugln(['Error: (lazarus) [TIDEProjectGroupOptions.SaveSafe] ',E.Message]);
  end;
end;

procedure TIDEProjectGroupOptions.LoadSafe;
begin
  try
    LoadFromFile(PGOptionsFileName);
  except
    on E: Exception do
      debugln(['Error: (lazarus) [TIDEProjectGroupOptions.LoadSafe] ',E.Message]);
  end;
  Modified:=false;
end;

procedure TIDEProjectGroupOptions.SaveToFile(aFilename: string);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(aFilename,false);
  try
    Cfg.SetValue('RecentProjectGroups/',FRecentProjectGroups);
    Cfg.SetDeleteValue('OpenLastGroupOnStart/Value',OpenLastGroupOnStart,true);
    Cfg.SetDeleteValue('LastGroupFile/Value',LastGroupFile,'');
    Cfg.SetDeleteValue('ShowTargetPaths/Value',ShowTargetPaths,false);
    Cfg.SetDeleteValue('BuildCommandToCompileTarget/Value',BuildCommandToCompileTarget,false);
  finally
    Cfg.Free;
  end;
end;

procedure TIDEProjectGroupOptions.LoadFromFile(aFilename: string);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(aFilename,true);
  try
    Cfg.GetValue('RecentProjectGroups/',FRecentProjectGroups);
    OpenLastGroupOnStart:=Cfg.GetValue('OpenLastGroupOnStart/Value',true);
    LastGroupFile:=Cfg.GetValue('LastGroupFile/Value','');
    ShowTargetPaths:=Cfg.GetValue('ShowTargetPaths/Value',false);
    BuildCommandToCompileTarget:=Cfg.GetValue('BuildCommandToCompileTarget/Value',false);
  finally
    Cfg.Free;
  end;
end;

procedure TIDEProjectGroupOptions.AddToRecentProjectGroups(aFilename: string);
var
  i: Integer;
begin
  FRecentProjectGroups.Insert(0,aFilename);
  for i:=FRecentProjectGroups.Count-1 downto 1 do
    if CompareFilenames(FRecentProjectGroups[i],aFilename)=0 then
      FRecentProjectGroups.Delete(i);
  while FRecentProjectGroups.Count>30 do
    FRecentProjectGroups.Delete(FRecentProjectGroups.Count-1);
end;

procedure TIDEProjectGroupOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(FChangeStamp);
end;

{ TIDEProjectGroupManager }

function TIDEProjectGroupManager.CheckSaved: Boolean;
begin
  if (FProjectGroup=nil) or (not FProjectGroup.Modified) then exit(true);
  case IDEQuestionDialog(lisProjectGroupModified,
                         Format(lisProjectGroupModifiedConfirm,[FProjectGroup.FileName]),
                         mtWarning,
                         [mrYes,lisSavePG,
                          mrNo,lisDiscard,
                          mrAbort,lisAbort],'') of
  mrYes :
    begin
      SaveProjectGroup;
      Result:=true;
    end;
  mrNo :
    begin
      FProjectGroup.Modified:=False;
      Result:=True;
    end
  else
    Result:=False;
  end;
end;

function TIDEProjectGroupManager.GetCurrentProjectGroup: TProjectGroup;
begin
  Result:=FProjectGroup;
end;

function TIDEProjectGroupManager.ShowProjectGroupEditor: Boolean;
begin
  Result:=Assigned(FProjectGroup);
  if Result then
  begin
    if Assigned(OnShowProjectGroupEditor) then
      OnShowProjectGroupEditor(FProjectGroup,FProjectGroup)
    else if Assigned(OnShowProjectGroupEditorEvent) then
      OnShowProjectGroupEditorEvent(FProjectGroup,FProjectGroup)
    else
      Result:=False;
  end;
end;

procedure TIDEProjectGroupManager.TargetDeleting(Group: TIDEProjectGroup;
  Index: integer);
var
  UndoDelete: TPGUndoDelete;
  Target: TIDECompileTarget;
begin
  UndoDelete:=TPGUndoDelete.Create;
  FUndoList.Add(UndoDelete);
  Target:=Group.Targets[Index] as TIDECompileTarget;
  UndoDelete.Target:=Target;
  UndoDelete.Group:=Group;
  if Index>0 then
    UndoDelete.InFrontFile:=Group.Targets[Index-1].Filename;
  if Index+1<Group.TargetCount then
    UndoDelete.BehindFile:=Group.Targets[Index+1].Filename;
end;

function TIDEProjectGroupManager.GroupExists(Group: TIDEProjectGroup): boolean;

  function HasGroupRecursive(CurGroup: TProjectGroup): boolean;
  var
    i: Integer;
    Target: TPGCompileTarget;
  begin
    if CurGroup=Group then exit(true);
    for i:=0 to CurGroup.TargetCount-1 do
    begin
      Target:=CurGroup.Targets[i];
      if Target.ProjectGroup=nil then continue;
      if HasGroupRecursive(Target.ProjectGroup) then exit(true);
    end;
    Result:=false;
  end;

begin
  Result:=HasGroupRecursive(GetCurrentProjectGroup);
end;

constructor TIDEProjectGroupManager.Create;
begin
  FOptions:=TIDEProjectGroupOptions.Create;
  FUndoList:=TObjectList.Create(true);
  FRedoList:=TObjectList.Create(true);
  IdleConnected:=true;

  IDEMacros.Add(TTransferMacro.Create('PGSrcPaths','','Project groups source paths',@GetPGSrcPaths,[]));
end;

destructor TIDEProjectGroupManager.Destroy;
begin
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);
  FreeAndNil(FProjectGroup);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TIDEProjectGroupManager.UpdateRecentProjectGroupMenu;
var
  i: Integer;
  Item: TIDEMenuItem;
  aFilename: String;
begin
  i:=0;
  while i<Options.RecentProjectGroups.Count do begin
    aFilename:=Options.RecentProjectGroups[i];
    if i<PGOpenRecentSubMenu.Count then begin
      Item:=PGOpenRecentSubMenu[i];
      Item.Caption:=aFilename;
    end
    else begin
      Item:=RegisterIDEMenuCommand(PGOpenRecentSubMenu,'OpenRecentProjectGroup'+IntToStr(i),aFilename,@DoOpenRecentClick);
    end;
    inc(i);
  end;
  while i<PGOpenRecentSubMenu.Count do
    PGOpenRecentSubMenu[i].Free;
end;

procedure TIDEProjectGroupManager.DoNewClick(Sender: TObject);
var
  AProject: TLazProject;
  aTarget: TIDECompileTarget;
begin
  if Not CheckSaved then
    Exit;
  FreeAndNil(FProjectGroup);
  FProjectGroup:=TIDEProjectGroup.Create(nil);
  MnuCmdSaveProjectGroupAs.Enabled:=true;

  // add current project
  AProject:=LazarusIDE.ActiveProject;
  if (AProject<>nil) and FilenameIsAbsolute(AProject.ProjectInfoFile)
  and FileExistsCached(AProject.ProjectInfoFile) then begin
    aTarget:=FProjectGroup.AddTarget(AProject.ProjectInfoFile) as TIDECompileTarget;
    aTarget.LoadTarget(true);
  end;

  ShowProjectGroupEditor;
end;

procedure TIDEProjectGroupManager.DoOpenClick(Sender: TObject);
var
  F: TOpenDialog;
begin
  if Not CheckSaved then
    Exit;
  F:=TOpenDialog.Create(Nil);
  With F do
    try
      InitIDEFileDialog(F);
      F.Options:=[ofFileMustExist,ofEnableSizing];
      F.Filter:=lisLazarusProjectGroupsLpg+'|*.lpg|'+lisAllFiles+'|'+AllFilesMask;
      if F.Execute then
        LoadProjectGroup(FileName,[pgloLoadRecursively]);
      StoreIDEFileDialog(F);
    finally
      F.Free;
    end;
end;

procedure TIDEProjectGroupManager.DoOpenRecentClick(Sender: TObject);
var
  Item: TIDEMenuCommand;
  aFilename: String;
begin
  Item:=Sender as TIDEMenuCommand;
  aFilename:=Item.Caption;
  //debugln(['TIDEProjectGroupManager.DoOpenRecentClick ',aFilename]);
  LoadProjectGroup(aFilename,[pgloLoadRecursively]);
end;

procedure TIDEProjectGroupManager.DoSaveClick(Sender: TObject);
begin
  SaveProjectGroup;
end;

function TIDEProjectGroupManager.GetNewFileName: Boolean;
var
  Dlg: TSaveDialog;
begin
  Result:=False;
  Dlg:=IDESaveDialogClass.Create(nil);
  try
    Dlg.FileName:=FProjectGroup.FileName;
    InitIDEFileDialog(Dlg);
    Dlg.Options:=[ofOverwritePrompt,ofPathMustExist,ofEnableSizing];
    Dlg.Filter:=lisLazarusProjectGroupsLpg+'|*.lpg|'+lisAllFiles+'|'+AllFilesMask;
    Dlg.DefaultExt:='.lpg';
    Result:=Dlg.Execute;
    if Result then begin
      FProjectGroup.FileName:=TrimAndExpandFilename(Dlg.FileName);
    end;
  finally
    StoreIDEFileDialog(Dlg);
    Dlg.Free;
  end;
end;

function TIDEProjectGroupManager.GetPGSrcPaths(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Abort:=false;
  if (s<>'') and (ConsoleVerbosity>=0) then
    debugln(['Hint: (lazarus) [TIDEProjectGroupManager.GetPGSrcPaths] ignoring macro PGSrcPaths parameter "',s,'"']);
  Result:=GetSrcPaths;
end;

procedure TIDEProjectGroupManager.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if FIDEStarted then
  begin
    IdleConnected:=false;
    exit;
  end;

  if Screen.GetCurrentModalForm<>nil then
    exit;
  FIDEStarted:=true;
  if (CurrentProjectGroup=nil)
      and Options.OpenLastGroupOnStart
      and (Options.LastGroupFile<>'')
      and FileExistsCached(Options.LastGroupFile) then
  begin
    LoadProjectGroup(Options.LastGroupFile,[pgloLoadRecursively,pgloSkipInvalid]);
  end;
  IdleConnected:=false;
end;

procedure TIDEProjectGroupManager.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TIDEProjectGroupManager.AddSrcPathOfFile(
  SrcPaths: TFilenameToStringTree; Filename: string);
var
  SrcPath: String;
begin
  //debugln(['TIDEProjectGroupManager.AddSrcPathOfFile ',Filename]);
  SrcPath:=ChompPathDelim(ExtractFilePath(ResolveDots(Filename)));
  SrcPaths[SrcPath]:='1';
end;

procedure TIDEProjectGroupManager.AddProjectSrcPaths(Target: TIDECompileTarget;
  SrcPaths, LPKFiles: TFilenameToStringTree);
var
  aProject: TLazProject;
  p, i: Integer;
  Paths, Path: String;
begin
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil)
      and (CompareFilenames(aProject.ProjectInfoFile,Target.Filename)=0) then
  begin
    // active project, can be virtual
    //debugln(['TIDEProjectGroupManager.AddProjectSrcPaths Active project']);
    AddSrcPathOfFile(SrcPaths,aProject.ProjectInfoFile);
    Paths:=aProject.LazCompilerOptions.GetSrcPath(false);
    //debugln(['TIDEProjectGroupManager.AddProjectSrcPaths Active project Paths="',Paths,'"']);
    p:=1;
    repeat
      Path:=GetNextDelimitedItem(Paths,';',p);
      if p>length(Paths) then break;
      SrcPaths[Path]:='1';
    until false;
  end else begin
    // lpi on disk -> use files in Target
    //debugln(['TIDEProjectGroupManager.AddProjectSrcPaths Inactive project']);
    AddSrcPathOfFile(SrcPaths,Target.Filename);
    for i:=0 to Target.FileCount-1 do
      AddSrcPathOfFile(SrcPaths,Target.Files[i]);
  end;
  // add SrcPaths of required packages
  for i:=0 to Target.RequiredPackageCount-1 do
    AddPackageNameSrcPaths(Target.RequiredPackages[i].PackageName,'','',SrcPaths,LPKFiles);
end;

procedure TIDEProjectGroupManager.AddPackageSrcPaths(Target: TIDECompileTarget;
  SrcPaths, LPKFiles: TFilenameToStringTree);
begin
  AddLPKSrcPaths(Target.Filename,SrcPaths,LPKFiles);
end;

procedure TIDEProjectGroupManager.AddPackageNameSrcPaths(PkgName,
  PreferredFile, DefaultFile: string; SrcPaths, LPKFiles: TFilenameToStringTree
  );
var
  LPKFilename: String;
  Link: TPackageLink;
begin
  if not IsValidPkgName(PkgName) then exit;
  if FilenameIsAbsolute(PreferredFile) and FileExistsCached(PreferredFile) then
    LPKFilename:=PreferredFile
  else if FilenameIsAbsolute(DefaultFile) and FileExistsCached(DefaultFile) then
    LPKFilename:=DefaultFile
  else begin
    Link:=PkgLinks.FindLinkWithPkgName(PkgName);
    if Link=nil then begin
      debugln(['Warning: (lazarus) [TIDEProjectGroupManager.AddPackageNameSrcPaths] package "',PkgName,'" not found']);
      exit;
    end;
    LPKFilename:=Link.GetEffectiveFilename;
    if not FilenameIsAbsolute(LPKFilename) then
      exit;
  end;
  AddLPKSrcPaths(LPKFilename,SrcPaths,LPKFiles);
end;

procedure TIDEProjectGroupManager.AddLPKSrcPaths(LPKFilename: string; SrcPaths,
  LPKFiles: TFilenameToStringTree);
var
  xml: TXMLConfig;
  Path, SubPath, CurFilename, PkgName, PreferredFilename,
    DefaultFilename, Paths, BaseDir: String;
  Cnt, i, p: Integer;
  Pkg: TIDEPackage;
begin
  if LPKFiles.Contains(LPKFilename) then exit;
  //debugln(['TIDEProjectGroupManager.AddLPKSrcPaths ',LPKFilename]);
  for i:=0 to PackageEditingInterface.GetPackageCount-1 do
  begin
    Pkg:=PackageEditingInterface.GetPackages(i);
    if CompareFilenames(Pkg.Filename,LPKFilename)=0 then
    begin
      // loaded package, can be virtual
      //debugln(['TIDEProjectGroupManager.AddPackageSrcPaths LOADED Pkg.Filename=',Pkg.Filename]);
      AddSrcPathOfFile(SrcPaths,Pkg.Filename);
      Paths:=Pkg.LazCompilerOptions.GetSrcPath(false);
      //debugln(['TIDEProjectGroupManager.AddPackageSrcPaths LOADED Paths=',Paths]);
      p:=1;
      repeat
        Path:=GetNextDelimitedItem(Paths,';',p);
        if p>length(Paths) then break;
        SrcPaths[Path]:='1';
      until false;
      exit;
    end;
  end;
  // not loaded lpk -> parse xml
  // Note: do not open package, as this might clash with active packages
  xml:=LoadXML(LPKFilename,true);
  try
    if xml=nil then exit;
    AddSrcPathOfFile(SrcPaths,LPKFilename);
    BaseDir:=ExtractFilePath(LPKFilename);
    // list of files
    Path:='Files/';
    Cnt:=xml.GetValue(Path+'Count',0);
    for i:=1 to Cnt do begin
      SubPath:=Path+'Item'+IntToStr(i)+'/';
      CurFilename:=xml.GetValue(SubPath+'Filename/Value','');
      if CurFilename='' then continue;
      AddSrcPathOfFile(SrcPaths,CurFilename);
    end;

    // load list of RequiredPackages from lpk
    Path:='Package/RequiredPkgs/';
    Cnt:=xml.GetValue(Path+'Count',0);
    for i:=1 to Cnt do begin
      SubPath:=Path+'Item'+IntToStr(i)+'/';
      PkgName:=xml.GetValue(SubPath+'PackageName/Value','');
      if not IsValidPkgName(PkgName) then continue;
      PreferredFilename:=xml.GetValue(SubPath+'DefaultFilename/Prefer','');
      if (PreferredFilename<>'') and not FilenameIsAbsolute(PreferredFilename) then
        PreferredFilename:=ResolveDots(BaseDir+PreferredFilename);
      DefaultFilename:=xml.GetValue(SubPath+'DefaultFilename/Value','');
      if (DefaultFilename<>'') and not FilenameIsAbsolute(DefaultFilename) then
        DefaultFilename:=ResolveDots(BaseDir+DefaultFilename);
      AddPackageNameSrcPaths(PkgName,PreferredFilename,DefaultFilename,SrcPaths,LPKFiles);
    end;
  finally
    xml.Free;
  end;
end;

procedure TIDEProjectGroupManager.AddGroupSrcPaths(Group: TProjectGroup;
  SrcPaths, LPKFiles: TFilenameToStringTree);
var
  i: Integer;
  Target: TIDECompileTarget;
begin
  if Group=nil then exit;
  //debugln(['TIDEProjectGroupManager.AddGroupSrcPaths ',Group.FileName,' Group.TargetCount=',Group.TargetCount]);
  for i:=0 to Group.TargetCount-1 do
  begin
    Target:=TIDECompileTarget(Group.Targets[i]);
    case Target.TargetType of
      ttProject: AddProjectSrcPaths(Target,SrcPaths,LPKFiles);
      ttPackage: AddPackageSrcPaths(Target,SrcPaths,LPKFiles);
      ttProjectGroup: AddGroupSrcPaths(Target.ProjectGroup,SrcPaths,LPKFiles);
      ttPascalFile: AddSrcPathOfFile(SrcPaths,Target.Filename);
    end;
  end;
end;

procedure TIDEProjectGroupManager.AddToRecentGroups(aFilename: string);
begin
  Options.AddToRecentProjectGroups(AFileName);
  Options.SaveSafe;
  UpdateRecentProjectGroupMenu;
end;

procedure TIDEProjectGroupManager.DoSaveAsClick(Sender: TObject);
begin
  if FProjectGroup=nil then exit;
  if GetNewFileName then
    SaveProjectGroup;
end;

function TIDEProjectGroupManager.CanUndo: boolean;
begin
  Result:=FUndoList.Count>0;
end;

function TIDEProjectGroupManager.CanRedo: boolean;
begin
  Result:=FRedoList.Count>0;
end;

procedure TIDEProjectGroupManager.Undo;
var
  Item: TPGUndoItem;
  UndoDelete: TPGUndoDelete;
  Target: TIDECompileTarget;
  Group: TIDEProjectGroup;
  i: Integer;
begin
  if not CanUndo then exit;
  Item:=TPGUndoItem(FUndoList[FUndoList.Count-1]);
  FUndoList.OwnsObjects:=false;
  FUndoList.Delete(FUndoList.Count-1);
  FUndoList.OwnsObjects:=true;
  try
    if Item is TPGUndoDelete then
    begin
      UndoDelete:=TPGUndoDelete(Item);
      Target:=UndoDelete.Target;
      UndoDelete.Target:=nil;
      Group:=UndoDelete.Group;
      if GroupExists(Group) then
      begin
        if Group.IndexOfTarget(Target.Filename)>=0 then
          exit;
        i:=Group.IndexOfTarget(UndoDelete.InFrontFile);
        if i>=0 then
          inc(i)
        else begin
          i:=Group.IndexOfTarget(UndoDelete.BehindFile);
          if i<0 then
            i:=Group.TargetCount;
        end;
        Group.InsertTarget(Target,i);
      end;
    end;
  finally
    Item.Free;
  end;
end;

procedure TIDEProjectGroupManager.Redo;
begin

end;

procedure TIDEProjectGroupManager.LoadProjectGroup(AFileName: string;
  AOptions: TProjectGroupLoadOptions);
begin
  AFileName:=TrimAndExpandFilename(AFileName);
  if Not CheckSaved then
    Exit;
  FreeAndNil(FProjectGroup);

  AddToRecentGroups(AFileName);
  FProjectGroup:=TIDEProjectGroup.Create(nil);
  FProjectGroup.FileName:=AFileName;
  FProjectGroup.LoadFromFile(AOptions);
  if not (pgloSkipDialog in AOptions) then
    ShowProjectGroupEditor;
  MnuCmdSaveProjectGroupAs.Enabled:=true;
end;

procedure TIDEProjectGroupManager.SaveProjectGroup;
begin
  if not Assigned(FProjectGroup) then exit;
  if (FProjectGroup.FileName<>'') or GetNewFileName then begin
    FProjectGroup.SaveToFile;
    AddToRecentGroups(FProjectGroup.FileName);
  end;
end;

function TIDEProjectGroupManager.GetSrcPaths: string;
var
  SrcPaths, LPKFiles: TFilenameToStringTree;
  s: PStringToStringItem;
begin
  Result:='';
  if not Assigned(FProjectGroup) then exit;
  LPKFiles:=TFilenameToStringTree.Create(false);
  SrcPaths:=TFilenameToStringTree.Create(false);
  try
    AddGroupSrcPaths(FProjectGroup,SrcPaths,LPKFiles);
    for s in SrcPaths do begin
      if s^.Name='' then continue;
      if Result<>'' then
        Result:=Result+';';
      Result:=Result+s^.Name;
    end;
  finally
    SrcPaths.Free;
    LPKFiles.Free;
  end;
end;

{ TRootProjectGroupTarget }

procedure TRootProjectGroupTarget.SetTargetType(AValue: TPGTargetType);
begin
  if (AValue<>ttProjectGroup) then
    Raise Exception.Create(lisErronlyProjectGroupAllowed);
  inherited SetTargetType(AValue);
end;

constructor TRootProjectGroupTarget.Create(aOwner: TProjectGroup);
begin
  inherited Create(nil);
  TargetType:=ttProjectGroup;
  FProjectGroup:=aOwner;
  Filename:=ProjectGroup.FileName;
end;

{ TIDEProjectGroup }

procedure TIDEProjectGroup.SetFileName(AValue: String);
begin
  if FileName=AValue then Exit;
  inherited SetFileName(AValue);
  if Assigned(FOnFileNameChange) then
    FOnFileNameChange(Self);
end;

function TIDEProjectGroup.GetTarget(Index: Integer): TPGCompileTarget;
begin
  Result:=TPGCompileTarget(FTargets[Index]);
end;

function TIDEProjectGroup.GetTargetCount: Integer;
begin
  Result:=FTargets.Count;
end;

function TIDEProjectGroup.GetActiveTarget: TPGCompileTarget;
begin
  Result:=FActiveTarget;
end;

procedure TIDEProjectGroup.SetActiveTarget(AValue: TPGCompileTarget);
begin
  if AValue=FActiveTarget then exit;
  if FActiveTarget<>nil then
    FActiveTarget.DeActivate;
  if AValue<>nil then
    AValue.Activate;
end;

procedure TIDEProjectGroup.DoTargetDeleted(Sender: TObject;
  Target: TPGCompileTarget);
begin
  if Assigned(OnTargetDeleted) then
    OnTargetDeleted(Sender,Target);
  if Parent<>nil then
    TIDEProjectGroup(Parent).DoTargetDeleted(Sender,Target);
end;

procedure TIDEProjectGroup.DoTargetInserted(Sender: TObject;
  Target: TPGCompileTarget);
begin
  if Assigned(OnTargetInserted) then
    OnTargetInserted(Sender,Target);
  if Parent<>nil then
    TIDEProjectGroup(Parent).DoTargetInserted(Sender,Target);
end;

constructor TIDEProjectGroup.Create(aCompileTarget: TIDECompileTarget);
begin
  inherited Create;
  if aCompileTarget=nil then begin
    FSelfTarget:=TRootProjectGroupTarget.Create(Self);
  end else begin
    FSelfTarget:=aCompileTarget;
    if FSelfTarget.Parent<>nil then
      FParent:=FSelfTarget.Parent.ProjectGroup;
  end;
  FTargets:=TFPObjectList.Create(True);
end;

destructor TIDEProjectGroup.Destroy;
begin
  FreeAndNil(FTargets);
  FreeAndNil(FSelfTarget);
  inherited Destroy;
end;

procedure TIDEProjectGroup.Clear;
begin
  FTargets.Clear;
end;

procedure TIDEProjectGroup.CheckInvalidCycle(const aFilename: string);
var
  Group: TProjectGroup;
begin
  Group:=Self;
  while Group<>nil do begin
    if CompareFilenames(AFileName,Group.FileName)=0 then
      raise Exception.Create(lisInvalidCycleAProjectGroupCannotHaveItselfAsTarget);
    Group:=Group.Parent;
  end;
end;

function TIDEProjectGroup.IndexOfTarget(const Target: TPGCompileTarget): Integer;
begin
  Result:=FTargets.IndexOf(Target);
end;

function TIDEProjectGroup.AddTarget(const AFileName: String): TPGCompileTarget;
begin
  Result:=Nil;
  if not FilenameIsAbsolute(AFileName) then
    RaiseGDBException('TIDEProjectGroup.AddTarget [20190629165305] '+AFileName);
  CheckInvalidCycle(AFileName);
  Result:=TIDECompileTarget.Create(SelfTarget);
  Result.FileName:=AFileName;
  FTargets.Add(Result);
  IncreaseChangeStamp;
  DoTargetInserted(Self,Result);
end;

function TIDEProjectGroup.InsertTarget(const Target: TPGCompileTarget;
  Index: Integer): Integer;
begin
  if Target=nil then
    RaiseGDBException('TIDEProjectGroup.InsertTarget [20190629165001]');
  if Target.Parent<>nil then
    RaiseGDBException(Target.Filename);
  CheckInvalidCycle(Target.FileName);
  if Index<0 then
    RaiseGDBException('TIDEProjectGroup.InsertTarget [20190629165007]');
  if Index>TargetCount then
    RaiseGDBException('TIDEProjectGroup.InsertTarget [20190629165009]');
  FTargets.Insert(Index,Target);
  TIDECompileTarget(Target).SetParent(SelfTarget);
  IncreaseChangeStamp;
  DoTargetInserted(Self,Target);
  Result:=FTargets.IndexOf(Target);
end;

procedure TIDEProjectGroup.RemoveTarget(Index: Integer);
var
  Target: TPGCompileTarget;
begin
  Target:=Targets[Index];
  IDEProjectGroupManager.TargetDeleting(Self,Index);
  Target.DeActivate;
  FTargets.OwnsObjects:=false;
  FTargets.Delete(Index);
  FTargets.OwnsObjects:=true;
  TIDECompileTarget(Target).SetParent(nil);
  Modified:=true;
  DoTargetDeleted(Self,Target);
end;

procedure TIDEProjectGroup.ExchangeTargets(OldIndex, NewIndex: Integer);
var
  Root: TIDEProjectGroup;
begin
  if OldIndex=NewIndex then exit;
  FTargets.Exchange(OldIndex,NewIndex);
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetsExchanged) then
    Root.OnTargetsExchanged(Self,GetTarget(OldIndex),GetTarget(NewIndex));
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroup.ActiveTargetChanged(T: TPGCompileTarget);
var
  Root: TIDEProjectGroup;
begin
  if T.Active then begin
    FActiveTarget:=T;
  end else begin
    if FActiveTarget=T then
      FActiveTarget:=nil;
  end;
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetActiveChanged) then
    Root.OnTargetActiveChanged(Self,T);
end;

function TIDEProjectGroup.UpdateMissing: boolean;
var
  i: Integer;
  Target: TPGCompileTarget;
  Missing: Boolean;
begin
  Result:=false;
  for i:=0 to TargetCount-1 do
  begin
    Target:=Targets[i];
    Missing:=not FileExistsCached(Target.Filename);
    if Target.Missing<>Missing then begin
      Target.Missing:=Missing;
      Result:=true;
    end;
    // todo sub groups
  end;
  if Result then
    if ProjectGroupManager.Editor<>nil then
      ProjectGroupManager.Editor.Invalidate;
end;

function TIDEProjectGroup.LoadFromFile(Options: TProjectGroupLoadOptions
  ): Boolean;
Var
  ARoot: String;
  TargetFileName: String;
  BaseDir, APath: String;
  XMLConfig: TXMLConfig;
  i,ACount: Integer;
  Target: TIDECompileTarget;
  aGroup: TProjectGroup;
begin
  Result:=false;
  if not FilenameIsAbsolute(FileName) then exit;
  if not FileExistsCached(Filename) then exit;

  Clear;

  aGroup:=Parent;
  while aGroup<>nil do begin
    if CompareFilenames(aGroup.FileName,Filename)=0 then
      exit; // circular
    aGroup:=aGroup.Parent;
  end;

  BaseDir:=AppendPathDelim(ExtractFilePath(FileName));
  try
    XMLConfig := LoadXML(Filename,pgloSkipDialog in Options);
    try
      ARoot:='ProjectGroup';
      ACount:=XMLConfig.GetValue(ARoot+'/Targets/Count',0);
      for i:=0 to ACount-1 do
      begin
        Target:=Nil;
        APath:=Format(ARoot+'/Targets/Target%d/',[i]);
        TargetFileName:=XMLConfig.GetValue(APath+'FileName','');
        TargetFileName:=TrimFilename(GetForcedPathDelims(TargetFileName));
        if not FilenameIsAbsolute(TargetFileName) then
          TargetFileName:=TrimFilename(BaseDir+TargetFileName);
        if (TargetFileName<>'') and FileExistsCached(TargetFileName) then begin
          Target:=TIDECompileTarget(AddTarget(TargetFileName));
          if pgloLoadRecursively in Options then
            Target.LoadTarget(true);
        end
        else if (pgloRemoveInvalid in Options) then
        begin
          // remove = do not load it
        end
        else if (pgloSkipInvalid in options) then
        begin
          Target:=TIDECompileTarget(AddTarget(TargetFileName));
          Target.Missing:=True;
        end
        else if (pgloErrorInvalid in options) then
          exit
        else
          case IDEQuestionDialog(lisErrTargetDoesNotExist,
              Format(lisErrNoSuchFile,[TargetFileName]),mtWarning,
              [mrYes,lisRemoveTarget,
               mrNo,lisAbortLoadingProjectGroup,
               mrYesToAll,lisSkipAllTargets],'') of
           mrYes: ;
           mrNo:
             exit;
           mrYesToAll:
             begin
               Target:=TIDECompileTarget(AddTarget(TargetFileName));
               Target.Missing:=True;
               Include(Options,pgloSkipInvalid);
             end;
          else
            exit;
          end;
        if Target<>nil then
          Target.LoadGroupSettings(XMLConfig,APath);
      end;
    finally
      Modified:=false;
      XMLConfig.Free;
    end;
    Result:=true;
  except
    on E: Exception do begin
      IDEMessageDialog(lisReadError, Format(lisErrorReadingProjectGroupFile, [Filename, #13, E.Message]),
        mtError,[mbOk]);
    end;
  end;
end;

function TIDEProjectGroup.SaveToFile: Boolean;
Var
  TargetPath: String;
  RelativeFileName: String;
  ARoot, APath: String;
  XMLConfig: TXMLConfig;
  i,ACount: Integer;
  aTarget: TIDECompileTarget;
  SubPG: TIDEProjectGroup;
begin
  Result:=True;
  // save .lpg
  try
    XMLConfig := CreateXML(FileName,false);
    try
      TargetPath:=ExtractFilePath(FileName);
      ARoot:='ProjectGroup';
      XMLConfig.SetValue(ARoot+'/FileVersion',PGFileVersion);
      ACount:=0;
      For i:=0 to TargetCount-1 do
      begin
        aTarget:=TIDECompileTarget(GetTarget(i));
        APath:=Format(ARoot+'/Targets/Target%d/',[ACount]);
        RelativeFileName:=ExtractRelativepath(TargetPath,aTarget.FileName);
        StringReplace(RelativeFileName,'\','/',[rfReplaceAll]); // normalize, so that files look the same x-platform, for less svn changes
        XMLConfig.SetDeleteValue(APath+'FileName',RelativeFileName,'');
        aTarget.SaveGroupSettings(XMLConfig,APath);
        Inc(ACount);
      end;
      XMLConfig.SetDeleteValue(ARoot+'/Targets/Count',ACount,0);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisWriteError, Format(lisUnableToWriteProjectGroupFile, [Filename, #13, E.Message]),
        mtError,[mbOk]);
      Result:=false;
    end;
  end;
  if not Result then exit;
  // save nested .plg
  For i:=0 to TargetCount-1 do
  begin
    aTarget:=TIDECompileTarget(GetTarget(i));
    if aTarget.TargetType=ttProjectGroup then
    begin
      SubPG:=TIDEProjectGroup(aTarget.ProjectGroup);
      if not SubPG.SaveToFile then
        exit(false);
    end;
  end;

  Modified:=False;
  Result:=true;
end;

{ TIDECompileTarget }

procedure TIDECompileTarget.LoadTarget(Recursively: boolean);
begin
  case TargetType of
    ttProject: LoadProject;
    ttPackage: LoadPackage;
    ttProjectGroup: LoadProjectGroup(Recursively);
  end;
end;

procedure TIDECompileTarget.LoadGroupSettings(XMLConfig: TXMLConfig;
  aPath: string);
begin
  case TargetType of
    ttProject: LoadProject_GroupSettings(XMLConfig,aPath);
  end;
  if not Missing then
    if XMLConfig.GetValue(APath+'Active',False) then
      Activate;
end;

procedure TIDECompileTarget.SaveGroupSettings(XMLConfig: TXMLConfig;
  aPath: string);
begin
  case TargetType of
    ttProject: SaveProject_GroupSettings(XMLConfig,aPath);
  end;
  XMLConfig.SetDeleteValue(APath+'Active',Active and not Missing,False);
end;

procedure TIDECompileTarget.UnLoadTarget;
begin
  if (FProjectGroup<>nil) and not (Self is TRootProjectGroupTarget) then
    FreeAndNil(FProjectGroup);
  if FBuildModes<>nil then
    FreeAndNil(FBuildModes);
  if FFiles<>nil then
    FreeAndNil(FFiles);
  if FRequiredPackages<>nil then
    FreeAndNil(FRequiredPackages);
end;

destructor TIDECompileTarget.Destroy;
begin
  UnLoadTarget;
  inherited Destroy;
end;

procedure TIDECompileTarget.Modified;
var
  PG: TProjectGroup;
begin
  PG:=GetOwnerProjectGroup;
  PG.Modified:=true;
end;

function TIDECompileTarget.PerformBuildModeAction(AAction: TPGTargetAction;
  aModeIdentifier: string): TPGActionResult;
begin
  if TargetType<>ttProject then exit(arNotAllowed);
  Result:=ProjectAction(AAction,aModeIdentifier);
end;

function TIDECompileTarget.CompileUsingLazBuild(const AAction: TPGTargetAction;
  aBuildMode: string): TPGActionResult;
var
  FPCParser: TFPCParser;
  Params: TStringList;
  WorkingDir: String;
  LazBuildFilename: String;
  CompileHint: String;
  ToolTitle, ToolKind: String;
  Tool: TAbstractExternalTool;
begin
  Result:=arFailed;
  case TargetType of
  ttProject:
    begin
      ToolTitle := Format(lisCompileProject, [ExtractFileNameOnly(Filename)]);
      if aBuildMode<>'' then
        ToolTitle += Format(lisBuildMode, [aBuildMode]);
      ToolKind := lisOtherProject;
    end;
  ttPackage:
    begin
      ToolTitle := Format(lisCompilePackage, [ExtractFileNameOnly(Filename)]);
      ToolKind := lisPackage;
    end;
  else exit;
  end;

  CompileHint := Format(lisProjectGroup2, [Parent.Filename + LineEnding]);

  LazBuildFilename:=GetLazBuildFilename;
  if LazBuildFilename='' then begin
    IDEMessageDialog(lisLazbuildNotFound, Format(lisTheLazbuildWasNotFound, [ExeExt])
      , mtError, [mbOk]);
    exit(arFailed);
  end;

  WorkingDir:=ExtractFilePath(Filename);
  Params:=TStringList.Create;
  if AAction=taCompileClean then
    Params.Add('-B');
  if aBuildMode<>'' then
    Params.Add('--build-mode='+aBuildMode);
  Params.Add(Filename);

  Tool:=ExternalToolList.Add(ToolTitle);
  Tool.Reference(Self, ClassName);
  try
    Tool.Data:=TIDEExternalToolData.Create(ToolKind, ExtractFileNameOnly(
      Filename), Filename);
    Tool.FreeData:=true;
    Tool.Hint:=CompileHint;
    Tool.Process.Executable:=LazBuildFilename;
    Tool.Process.Parameters:=Params;
    Tool.Process.CurrentDirectory:=WorkingDir;
    FPCParser:=TFPCParser(Tool.AddParsers(SubToolFPC));
    FPCParser.HideHintsSenderNotUsed:=true; //not AProject.CompilerOptions.ShowHintsForSenderNotUsed;
    FPCParser.HideHintsUnitNotUsedInMainSource:=true; //not AProject.CompilerOptions.ShowHintsForUnusedUnitsInMainSrc;
    //if (not AProject.CompilerOptions.ShowHintsForUnusedUnitsInMainSrc)
    //and (AProject.MainFilename<>'') then
    //  FPCParser.FilesToIgnoreUnitNotUsed.Add(AProject.MainFilename);
    Tool.AddParsers(SubToolMake);
    Tool.Execute;
    Tool.WaitForExit;
    if Tool.ErrorMessage='' then
      Result:=arOK;
  finally
    Tool.Release(Self);
    Params.Free;
  end;
end;

function TIDECompileTarget.CheckIDEIsReadyForBuild: boolean;
begin
  // check toolstatus
  if LazarusIDE.ToolStatus<>itNone then begin
    IDEMessageDialog(lisBePatient, lisThereIsStillAnotherBuildInProgress,
      mtInformation, [mbOk]);
    exit(false);
  end;
  Result:=true;
end;

function TIDECompileTarget.GetBuildModeCount: integer;
begin
  if FBuildModes=nil then
    Result:=0
  else
    Result:=FBuildModes.Count;
end;

function TIDECompileTarget.GetBuildModes(Index: integer): TPGBuildMode;
begin
  Result:=TPGBuildMode(FBuildModes[Index]);
end;

function TIDECompileTarget.GetFileCount: integer;
begin
  if FFiles=nil then
    Result:=0
  else
    Result:=FFiles.Count;
end;

function TIDECompileTarget.GetFiles(Index: integer): string;
begin
  Result:=FFiles[Index];
end;

function TIDECompileTarget.GetRequiredPackageCount: integer;
begin
  if FRequiredPackages<>nil then
    Result:=FRequiredPackages.Count
  else
    Result:=0;
end;

function TIDECompileTarget.GetRequiredPackages(Index: integer): TPGDependency;
begin
  Result:=TPGDependency(FRequiredPackages[Index]);
end;

procedure TIDECompileTarget.LoadPackage;
var
  MR: TModalResult;
  I: Integer;
  Pkg, RequiredPkg: TIDEPackage;
  PkgName: String;
  PkgList: TFPList;
begin
  if FFiles<>nil then exit; // already loaded

  debugln(['TIDECompileTarget.LoadPackage ',Filename]);
  FFiles:=TStringList.Create;
  FRequiredPackages:=TObjectList.Create(True);

  PkgName:=ExtractFileUnitname(Filename,true);
  if PkgName='' then begin
    debugln(['Warning: (lazarus) [TIDECompileTarget.LoadPackage] invalid package filename "',Filename,'"']);
    exit;
  end;

  Pkg:=PackageEditingInterface.FindPackageWithName(PkgName);
  if Pkg=nil then begin
    MR:=PackageEditingInterface.DoOpenPackageFile(Filename,
        [pofDoNotOpenEditor],False);
    if MR<>mrOk then begin
      debugln(['Warning: (lazarus) [TIDECompileTarget.LoadPackage] DoOpenPackageFile failed on file "',Filename,'"']);
      exit;
    end;
    Pkg:=PackageEditingInterface.FindPackageWithName(PkgName);
    if Pkg=nil then begin
      debugln(['Warning: (lazarus) [TIDECompileTarget.LoadPackage] DoOpenPackageFile failed pkgname="',PkgName,'" on file "',Filename,'"']);
      exit;
    end;
  end;
  if CompareFilenames(Pkg.Filename,Filename)<>0 then begin
    debugln(['Warning: (lazarus) [TIDECompileTarget.LoadPackage] there is already a package with that name: wanted="',Filename,'" loaded="',Pkg.Filename,'"']);
    exit;
  end;

  // load list of file
  for i:=0 to Pkg.FileCount-1 do
    FFiles.Add(Pkg.Files[i].Filename);

  // load list of required package
  PkgList:=nil;
  try
    PackageEditingInterface.GetRequiredPackages(Pkg,PkgList,[pirNotRecursive]);
    if PkgList<>nil then
      for i:=0 to PkgList.Count-1 do begin
        RequiredPkg:=TIDEPackage(PkgList[i]);
        PkgName:=ExtractFileUnitname(RequiredPkg.Filename,true);
        FRequiredPackages.Add(TPGDependency.Create(Self,PkgName));
      end;
  finally
    PkgList.Free;
  end;
end;

procedure TIDECompileTarget.LoadProject;
var
  AProject: TLazProject;
  i, Cnt, ALPIFileVersion: Integer;
  ProjFile: TLazProjectFile;
  PkgList: TFPList;
  Pkg: TIDEPackage;
  PkgName, Path, SubPath, CurFilename, BaseDir, BuildMode: String;
  xml: TXMLConfig;
  LazBuildMode: TLazProjectBuildMode;
  LegacyList: Boolean;
begin
  if FFiles<>nil then exit; // already loaded

  //debugln(['TIDECompileTarget.LoadProject ',Filename]);
  FBuildModes:=TObjectList.Create(True);
  FFiles:=TStringList.Create;
  FRequiredPackages:=TObjectList.Create(True);

  AProject:=LazarusIDE.ActiveProject;
  if (AProject<>nil) and (CompareFilenames(AProject.ProjectInfoFile,Filename)=0)
  then begin
    // load from active project
    for i:=0 to AProject.FileCount-1 do begin
      ProjFile:=AProject.Files[i];
      if not ProjFile.IsPartOfProject then continue;
      FFiles.Add(ProjFile.Filename);
    end;

    // load dependencies from active project
    PkgList:=nil;
    try
      PackageEditingInterface.GetRequiredPackages(AProject,PkgList,[pirNotRecursive,pirCompileOrder]);
      if PkgList<>nil then begin
        for i:=0 to PkgList.Count-1 do begin
          Pkg:=TIDEPackage(PkgList[i]);
          PkgName:=ExtractFileUnitname(Pkg.Filename,true);
          FRequiredPackages.Add(TPGDependency.Create(Self,PkgName));
        end;
      end;
    finally
      PkgList.Free;
    end;

    // load buildmodes
    for i:=0 to AProject.LazBuildModes.Count-1 do begin
      LazBuildMode:=AProject.LazBuildModes.BuildModes[i];
      FBuildModes.Add(TPGBuildMode.Create(Self,LazBuildMode.Identifier,false));
    end;
  end else begin
    // load from .lpi file

    xml:=LoadXML(Filename,true);
    try
      if xml<>nil then begin
        // load list of files from lpi
        BaseDir:=ExtractFilePath(Filename);
        Path:='ProjectOptions/';
        ALPIFileVersion := xml.GetValue(Path+'Version/Value',0);

        Path:='ProjectOptions/Units/';
        LegacyList:=(ALPIFileVersion<=11) or xml.IsLegacyList(Path);
        Cnt:=xml.GetListItemCount(Path, 'Unit', LegacyList);
        for i := 0 to Cnt - 1 do begin
          SubPath:=Path+xml.GetListItemXPath('Unit', i, LegacyList)+'/';
          if not xml.GetValue(SubPath+'IsPartOfProject/Value',False) then
            continue;
          CurFilename:=xml.GetValue(SubPath+'Filename/Value','');
          if CurFilename='' then continue;
          if not FilenameIsAbsolute(CurFilename) then
            CurFilename:=TrimFilename(BaseDir+CurFilename);
          FFiles.Add(CurFilename);
        end;

        // load list of RequiredPackages from lpi
        Path:='ProjectOptions/RequiredPackages/';
        Cnt:=xml.GetValue(Path+'Count',0);
        for i:=1 to Cnt do begin
          SubPath:=Path+'Item'+IntToStr(i)+'/';
          PkgName:=xml.GetValue(SubPath+'PackageName/Value','');
          if PkgName='' then continue;
          FRequiredPackages.Add(TPGDependency.Create(Self,PkgName));
        end;

        // load build modes
        Path:='ProjectOptions/BuildModes/';
        LegacyList:=(ALPIFileVersion<=11) or xml.IsLegacyList(Path);
        Cnt:=xml.GetListItemCount(Path, 'Item', LegacyList);
        for i:=0 to Cnt-1 do begin
          SubPath:=Path+xml.GetListItemXPath('Item', i, LegacyList, True)+'/';
          BuildMode:=xml.GetValue(SubPath+'Name','');
          // load/store compile in lpg
          if BuildMode<>'' then
            FBuildModes.Add(TPGBuildMode.Create(Self,BuildMode,false));
        end;
      end;
    finally
      xml.Free;
    end;
  end;
end;

procedure TIDECompileTarget.LoadProject_GroupSettings(XMLConfig: TXMLConfig;
  aPath: string);
var
  Cnt, i: Integer;
  SubPath, aName: String;
  aMode: TPGBuildMode;
begin
  Cnt:=XMLConfig.GetValue(aPath+'BuildModes/Count',0);
  for i:=1 to Cnt do begin
    SubPath:=aPath+'Mode'+IntToStr(i)+'/';
    aName:=XMLConfig.GetValue(SubPath+'Name','');
    aMode:=FindBuildMode(aName);
    if aMode=nil then continue;
    aMode.Compile:=XMLConfig.GetValue(SubPath+'Compile',false);
  end;
end;

procedure TIDECompileTarget.SaveProject_GroupSettings(XMLConfig: TXMLConfig;
  aPath: string);
var
  i: Integer;
  SubPath: String;
  aMode: TPGBuildMode;
begin
  XMLConfig.SetDeleteValue(aPath+'BuildModes/Count',BuildModeCount,0);
  for i:=1 to BuildModeCount do begin
    SubPath:=aPath+'Mode'+IntToStr(i)+'/';
    aMode:=BuildModes[i-1];
    XMLConfig.SetDeleteValue(SubPath+'Name',aMode.Identifier,'');
    XMLConfig.SetDeleteValue(SubPath+'Compile',aMode.Compile,false);
  end;
end;

procedure TIDECompileTarget.LoadProjectGroup(Recursively: boolean);
var
  PG: TIDEProjectGroup;
  Flags: TProjectGroupLoadOptions;
begin
  if ProjectGroup<>nil then exit;

  debugln(['TIDECompileTarget.LoadProjectGroup ',Filename]);
  PG:=TIDEProjectGroup.Create(Self);
  FProjectGroup:=PG;
  PG.FileName:=Self.FileName;
  Flags:=[];
  if Recursively then
    Include(Flags,pgloLoadRecursively);
  PG.LoadFromFile(Flags);
end;

function TIDECompileTarget.ProjectAction(AAction: TPGTargetAction;
  StartBuildMode: string): TPGActionResult;
var
  R: TCompileReason;
  i: Integer;
  aMode: TPGBuildMode;
  aProject: TLazProject;
begin
  Result:=arFailed;

  debugln(['TIDECompileTarget.ProjectAction ',Filename]);
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil)
  and (CompareFilenames(aProject.ProjectInfoFile,Filename)=0)
  then begin
    // project loaded => use IDE functions

    if StartBuildMode<>'' then begin
      // switch to build mode
      if CompareText(StartBuildMode,aProject.ActiveBuildModeID)<>0 then
      begin
        if not CheckIDEIsReadyForBuild then exit;
        aProject.ActiveBuildModeID:=StartBuildMode;
      end;
    end;

    case AAction of
     taSettings :
       begin
         if not ExecuteIDECommand(Self,ecProjectOptions) then
           Result:=arOK;
       end;
     taCompile,
     taCompileClean,
     taCompileFromHere:
       begin
         if not CheckIDEIsReadyForBuild then exit;
         // save project
         if LazarusIDE.DoSaveProject([])<>mrOk then exit;

         R:= crCompile;
         if (AAction=taCompileClean) then
           R:= crBuild;
         if BuildModeCount>1 then begin
           i:=0;
           if StartBuildMode<>'' then begin
             i:=aProject.LazBuildModes.IndexOf(StartBuildMode);
             if i<0 then exit;
           end;
           while i<BuildModeCount do begin
             aMode:=BuildModes[i];
             inc(i);
             debugln(['TIDECompileTarget.ProjectAction ',(aMode.Identifier<>StartBuildMode),' ',aMode.Identifier,' StartBuildMode=',StartBuildMode,' ',AAction=taCompileFromHere]);
             if (aMode.Identifier<>StartBuildMode) and (not aMode.Compile) then continue;
             // switch build mode
             aProject.ActiveBuildModeID:=aMode.Identifier;
             if aProject.ActiveBuildModeID<>aMode.Identifier
             then begin
               IDEMessageDialog(lisBuildModeNotFound, Format(lisBuildModeNotFound2, [aMode.Identifier]), mtError, [mbOk]);
               exit;
             end;
             // compile project in active buildmode
             if LazarusIDE.DoBuildProject(R,[])<>mrOk then
               exit;
             if (StartBuildMode<>'') and (AAction<>taCompileFromHere) then
               exit(arOK);
             StartBuildMode:='';
           end;
         end else begin
           // compile default buildmode
           if LazarusIDE.DoBuildProject(R,[])<>mrOk then
             exit;
         end;
         Result:=arOK;
         if AAction=taCompileFromHere then
           Result:=PerformNextTarget(taCompileFromHere);
       end;
     taRun :
       begin
         if LazarusIDE.DoRunProject<>mrOk then exit;
         Result:=arOk;
       end;
    end;
  end else begin
    // project not loaded => use lazbuild
    case AAction of
    taOpen,taSettings:
      begin
        // open project
        if LazarusIDE.DoOpenProjectFile(Filename,[ofAddToRecent])<>mrOk then
          exit;
        if AAction=taSettings then
          if not ExecuteIDECommand(Self,ecProjectOptions) then
            exit;
        Result:=arOK;
      end;
    taCompile,
    taCompileClean,
    taCompileFromHere:
      begin
        if not CheckIDEIsReadyForBuild then exit;

        // save files
        if LazarusIDE.DoSaveProject([])<>mrOk then exit;

        LazarusIDE.ToolStatus:=itBuilder;
        try
          if BuildModeCount>1 then begin
            IDEMessagesWindow.Clear;
            i:=0;
            if StartBuildMode<>'' then begin
              while (i<BuildModeCount) and (CompareText(BuildModes[i].Identifier,StartBuildMode)<>0)
              do inc(i);
            end;
            while i<BuildModeCount do begin
              aMode:=BuildModes[i];
              inc(i);
              if (aMode.Identifier<>StartBuildMode) and (not aMode.Compile) then continue;
              // run lazbuild as external tool
              Result:=CompileUsingLazBuild(AAction,aMode.Identifier);
              if Result<>arOK then exit;

              if (StartBuildMode<>'') and (AAction<>taCompileFromHere) then
                exit(arOK);
              StartBuildMode:='';
            end;
          end else begin
            IDEMessagesWindow.Clear;
            // run lazbuild as external tool
            Result:=CompileUsingLazBuild(AAction);
            if Result<>arOK then exit;
          end;
        finally
          LazarusIDE.ToolStatus:=itNone;
        end;
        Result:=arOK;
        if AAction=taCompileFromHere then
          Result:=PerformNextTarget(taCompileFromHere);
      end;
    taRun:
      begin
        // open project, then run
        if LazarusIDE.DoOpenProjectFile(Filename,[ofAddToRecent])<>mrOk then
          exit;
        if LazarusIDE.DoRunProject<>mrOk then
          exit;
        Result:=arOk;
      end;
    end;
  end;
end;

function TIDECompileTarget.PackageAction(AAction: TPGTargetAction): TPGActionResult;
begin
  Result:=arFailed;

  case AAction of
  taOpen,
  taSettings:
    begin
      if PackageEditingInterface.DoOpenPackageFile(FileName,[],False)<>mrOk then
        exit(arFailed);
      Result:=arOK;
    end;
  taCompile,
  taCompileClean,
  taCompileFromHere:
    begin
      if not CheckIDEIsReadyForBuild then exit;
      // compile independent of active project => use lazbuild
      Result:=CompileUsingLazBuild(AAction);
      if Result<>arOK then exit;
      if AAction=taCompileFromHere then
        Result:=PerformNextTarget(taCompileFromHere);
    end;
  taInstall: ;  // ToDo install
  taUninstall: ; // ToDo uninstall
  end;
end;

function TIDECompileTarget.ProjectGroupAction(AAction: TPGTargetAction
  ): TPGActionResult;
var
  i: Integer;
  aTarget: TIDECompileTarget;
begin
  Result:=arFailed;

  case AAction of
  taOpen:
    ProjectGroupManager.LoadProjectGroup(FileName,[]);
  taSettings: ;
  taCompile,
  taCompileClean:
    begin
      for i:=0 to ProjectGroup.TargetCount-1 do begin
        aTarget:=TIDECompileTarget(ProjectGroup.Targets[i]);
        if AAction in aTarget.AllowedActions then
          if aTarget.PerformAction(AAction)<>arOk then
            exit;
      end;
      Result:=arOk;
    end;
  taCompileFromHere:
    begin
      if ProjectGroupAction(taCompile)<>arOK then
        exit;
      Result:=arOK;
      aTarget:=TIDECompileTarget(GetNext(true));
      if aTarget=nil then exit;
      Result:=aTarget.PerformAction(taCompileFromHere);
    end;
  end;
end;

function TIDECompileTarget.PascalFileAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=arFailed;
  case AAction of
  taOpen,
  taSettings:
    begin
      if LazarusIDE.DoOpenEditorFile(Filename,-1,-1,[ofAddToRecent,ofRegularFile])<>mrOK then
        exit;
      if AAction=taSettings then
        if LazarusIDE.DoConfigureBuildFile<>mrOk then exit;
      Result:=arOK;
    end;
  taCompile,
  taCompileClean,
  taCompileFromHere:
    begin
      if not CheckIDEIsReadyForBuild then exit;
      if LazarusIDE.DoBuildFile(false,Filename)<>mrOK then
        exit;
      Result:=arOK;
      if AAction=taCompileFromHere then
        Result:=PerformNextTarget(taCompileFromHere);
    end;
  taRun:
    begin
      if not CheckIDEIsReadyForBuild then exit;
      if LazarusIDE.DoRunFile(Filename)<>mrOK then
        exit;
      Result:=arOK;
    end;
  end;
end;

function TIDECompileTarget.ExternalToolAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=arFailed;
  debugln(['TIDECompileTarget.ExternalToolAction ToDo']);
  // ToDo
  case AAction of
  taSettings: ;
  taCompile,
  taCompileClean,
  taCompileFromHere:
    begin
      if AAction=taCompileFromHere then
        Result:=PerformNextTarget(taCompileFromHere);
    end;
  taRun: ;
  end;
end;

function TIDECompileTarget.PerformAction(AAction: TPGTargetAction): TPGActionResult;
begin
  case TargetType of
    ttProject: Result:=ProjectAction(AAction);
    ttPackage: Result:=PackageAction(AAction);
    ttProjectGroup: Result:=ProjectGroupAction(AAction);
    ttPascalFile: Result:=PascalFileAction(AAction);
    ttExternalTool: Result:=ExternalToolAction(AAction);
  end;
end;

function TIDECompileTarget.PerformNextTarget(AAction: TPGTargetAction
  ): TPGActionResult;
var
  aTarget: TIDECompileTarget;
begin
  aTarget:=TIDECompileTarget(GetNext(false));
  while (aTarget<>nil) do
  begin
    if AAction in aTarget.AllowedActions then
    begin
      Result:=aTarget.PerformAction(AAction);
      exit;
    end;
    aTarget:=TIDECompileTarget(aTarget.GetNext(false));
  end;
  Result:=arOK;
end;

procedure TIDECompileTarget.ActiveChanged(Sender: TPGCompileTarget);
begin
  (GetRootProjectGroup as TIDEProjectGroup).ActiveTargetChanged(Sender);
end;

procedure TIDECompileTarget.SetParent(NewParent: TPGCompileTarget);
begin
  FParent:=NewParent;
end;

end.

