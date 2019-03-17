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
  LazFileUtils, FileUtil, LazFileCache, LazConfigStorage, Laz2_XMLCfg, LazTracer,
  // LCL
  Controls, Forms, Dialogs,
  // CodeTools
  FileProcs, CodeToolManager, CodeCache,
  // IdeIntf
  PackageIntf, ProjectIntf, MenuIntf, LazIDEIntf, IDEDialogs, CompOptsIntf,
  BaseIDEIntf, IDECommands, IDEExternToolIntf, MacroIntf, IDEMsgIntf,
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
    FOnTargetAdded: TTargetEvent;
    FOnTargetDeleted: TTargetEvent;
    FOnTargetReadded: TTargetEvent;
    FOnTargetsExchanged: TTargetExchangeEvent;
    FTargets: TFPObjectList;
    FRemovedTargets: TFPObjectList;
  protected
    procedure SetFileName(AValue: String); override;
    function GetTarget(Index: Integer): TPGCompileTarget; override;
    function GetTargetCount: Integer; override;
    function GetRemovedTargetCount: Integer; override;
    function GetRemovedTarget(Index: Integer): TPGCompileTarget; override;
    function GetActiveTarget: TPGCompileTarget; override;
    procedure SetActiveTarget(AValue: TPGCompileTarget); override;
  public
    constructor Create(aCompileTarget: TIDECompileTarget);
    destructor Destroy; override;
    procedure Clear;
    function IndexOfTarget(const Target: TPGCompileTarget): Integer; override;
    function IndexOfRemovedTarget(const Target: TPGCompileTarget): Integer; override;
    function AddTarget(Const AFileName: String): TPGCompileTarget; override;
    procedure ReAddTarget(Target: TPGCompileTarget); override;
    procedure RemoveTarget(Index: Integer); override;
    procedure ExchangeTargets(ASource, ATarget: Integer); override;
    procedure ActiveTargetChanged(T: TPGCompileTarget);
    function LoadFromFile(Options: TProjectGroupLoadOptions): Boolean;
    function SaveToFile: Boolean;
    property OnFileNameChange: TNotifyEvent Read FOnFileNameChange Write FOnFileNameChange;
    property OnTargetAdded: TTargetEvent Read FOnTargetAdded Write FOnTargetAdded;
    property OnTargetDeleted: TTargetEvent Read FOnTargetDeleted Write FOnTargetDeleted;
    property OnTargetReadded: TTargetEvent Read FOnTargetReadded Write FOnTargetReadded;
    property OnTargetActiveChanged: TTargetEvent Read FOnTargetActiveChanged Write FOnTargetActiveChanged;
    property OnTargetsExchanged: TTargetExchangeEvent Read FOnTargetsExchanged Write FOnTargetsExchanged;
  end;

  { TIDEProjectGroupOptions }

  TIDEProjectGroupOptions = class
  private
    FChangeStamp: integer;
    FLastSavedChangeStamp: integer;
    FRecentProjectGroups: TStringList;
    FShowTargetPaths: boolean;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
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
    property ShowTargetPaths: boolean read FShowTargetPaths write SetShowTargetPaths;
  end;

  { TIDEProjectGroupManager }

  TIDEProjectGroupManager = Class(TProjectGroupManager)
  private
    FOptions: TIDEProjectGroupOptions;
    procedure AddToRecentGroups(aFilename: string);
    function GetNewFileName: Boolean;
  protected
    FProjectGroup: TIDEProjectGroup;
  protected
    function GetCurrentProjectGroup: TProjectGroup; override;
    function ShowProjectGroupEditor: Boolean;
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
    procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); override;
    procedure SaveProjectGroup; override;
  public
    property Options: TIDEProjectGroupOptions read FOptions;
  end;

  TEditProjectGroupHandler = procedure(Sender: TObject; AProjectGroup: TProjectGroup);
  // Method variant.
  TEditProjectGroupEvent = procedure(Sender: TObject; AProjectGroup: TProjectGroup) of object;

var
  OnShowProjectGroupEditor: TEditProjectGroupHandler; // Takes precedence
  OnShowProjectGroupEditorEvent: TEditProjectGroupEvent; // method variant

  IDEProjectGroupManager: TIDEProjectGroupManager;

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

{ TIDEProjectGroupOptions }

function TIDEProjectGroupOptions.GetModified: boolean;
begin
  Result:=FLastSavedChangeStamp<>FChangeStamp
end;

procedure TIDEProjectGroupOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FLastSavedChangeStamp:=FChangeStamp;
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
    Cfg.SetDeleteValue('ShowTargetPaths/',ShowTargetPaths,false);
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
    ShowTargetPaths:=Cfg.GetValue('ShowTargetPaths/',false);
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

constructor TIDEProjectGroupManager.Create;
begin
  FOptions:=TIDEProjectGroupOptions.Create;
end;

destructor TIDEProjectGroupManager.Destroy;
begin
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
  F: TSaveDialog;
begin
  Result:=False;
  F:=TSaveDialog.Create(Nil);
  With F do
    try
      FileName:=FProjectGroup.FileName;
      InitIDEFileDialog(F);
      F.Options:=[ofOverwritePrompt,ofPathMustExist,ofEnableSizing];
      F.Filter:=lisLazarusProjectGroupsLpg+'|*.lpg|'+lisAllFiles+'|'+AllFilesMask;
      F.DefaultExt:='.lpg';
      Result:=F.Execute;
      if Result then begin
        FProjectGroup.FileName:=TrimAndExpandFilename(FileName);
      end;
      StoreIDEFileDialog(F);
    finally
      F.Free;
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

function TIDEProjectGroup.GetRemovedTargetCount: Integer;
begin
  Result:=FRemovedTargets.Count;
end;

function TIDEProjectGroup.GetRemovedTarget(Index: Integer): TPGCompileTarget;
begin
  Result:=TPGCompileTarget(FRemovedTargets[Index]);
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

constructor TIDEProjectGroup.Create(aCompileTarget: TIDECompileTarget);
begin
  inherited Create;
  if aCompileTarget=nil then begin
    FCompileTarget:=TRootProjectGroupTarget.Create(Self);
  end else begin
    FCompileTarget:=aCompileTarget;
    if FCompileTarget.Parent<>nil then
      FParent:=FCompileTarget.Parent.ProjectGroup;
  end;
  FTargets:=TFPObjectList.Create(True);
  FRemovedTargets:=TFPObjectList.Create(True);
end;

destructor TIDEProjectGroup.Destroy;
begin
  FreeAndNil(FTargets);
  FreeAndNil(FRemovedTargets);
  FreeAndNil(FCompileTarget);
  inherited Destroy;
end;

procedure TIDEProjectGroup.Clear;
begin
  FTargets.Clear;
  FRemovedTargets.Clear;
end;

function TIDEProjectGroup.IndexOfTarget(const Target: TPGCompileTarget): Integer;
begin
  Result:=FTargets.IndexOf(Target);
end;

function TIDEProjectGroup.IndexOfRemovedTarget(const Target: TPGCompileTarget
  ): Integer;
begin
  Result:=FRemovedTargets.IndexOf(Target);
end;

function TIDEProjectGroup.AddTarget(const AFileName: String): TPGCompileTarget;
var
  Root: TIDEProjectGroup;
begin
  Result:=Nil;
  if not FilenameIsAbsolute(AFileName) then
    RaiseGDBException(AFileName);
  if CompareFilenames(AFileName,FileName)=0 then
    raise Exception.Create(lisInvalidCycleAProjectGroupCannotHaveItselfAsTarget);
  Result:=TIDECompileTarget.Create(CompileTarget);
  Result.FileName:=AFileName;
  FTargets.Add(Result);
  IncreaseChangeStamp;
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetAdded) then
    Root.OnTargetAdded(Self,Result);
end;

procedure TIDEProjectGroup.ReAddTarget(Target: TPGCompileTarget);
var
  Root: TIDEProjectGroup;
begin
  if (Target=nil) or (not Target.Removed) then
    raise Exception.Create('');
  FRemovedTargets.OwnsObjects:=false;
  FRemovedTargets.Remove(Target);
  FRemovedTargets.OwnsObjects:=true;
  FTargets.Add(Target);
  Target.Removed:=false;
  Modified:=true;
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetReadded) then
    Root.OnTargetReadded(Self,Target);
end;

procedure TIDEProjectGroup.RemoveTarget(Index: Integer);
var
  Target: TPGCompileTarget;
  Root: TIDEProjectGroup;
begin
  Target:=Targets[Index];
  Target.DeActivate;
  FTargets.OwnsObjects:=false;
  FTargets.Delete(Index);
  FTargets.OwnsObjects:=true;
  FRemovedTargets.Add(Target);
  Target.Removed:=true;
  Modified:=true;
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetDeleted) then
    Root.OnTargetDeleted(Self,Target);
end;

procedure TIDEProjectGroup.ExchangeTargets(ASource, ATarget: Integer);
var
  Root: TIDEProjectGroup;
begin
  if ASource=ATarget then exit;
  FTargets.Exchange(ASource,ATarget);
  Root:=TIDEProjectGroup(GetRootGroup);
  if Assigned(Root.OnTargetsExchanged) then
    Root.OnTargetsExchanged(Self,GetTarget(ASource),GetTarget(ATarget));
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
          Target:=TIDECompileTarget(AddTarget(TargetFileName));
          Target.Removed:=True;
        end
        else if (pgloSkipInvalid in options) then
          // Do nothing
        else if (pgloErrorInvalid in options) then
          exit
        else
          case IDEQuestionDialog(lisErrTargetDoesNotExist,
              Format(lisErrNoSuchFile,[TargetFileName]),mtWarning,
              [mrYes,lisRemoveTarget,
               mrNo,lisAbortLoadingProjectGroup,
               mrYesToAll,lisSkipAllTargets],'') of
           mrYes :
             begin
               Target:=TIDECompileTarget(AddTarget(TargetFileName));
               Target.Removed:=True;
             end;
           mrNo:
             exit;
           mrYesToAll:
             begin
               Target:=TIDECompileTarget(AddTarget(TargetFileName));
               Target.Removed:=True;
               Include(Options,pgloRemoveInvalid);
             end;
          else
            exit;
          end;
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
        if aTarget.Removed then continue;
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
    if aTarget.Removed then continue;
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
  if not Removed then
    if XMLConfig.GetValue(APath+'Active',False) then
      Activate;
end;

procedure TIDECompileTarget.SaveGroupSettings(XMLConfig: TXMLConfig;
  aPath: string);
begin
  case TargetType of
    ttProject: SaveProject_GroupSettings(XMLConfig,aPath);
  end;
  XMLConfig.SetDeleteValue(APath+'Active',Active and not Removed,False);
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
  i, Cnt: Integer;
  ProjFile: TLazProjectFile;
  PkgList: TFPList;
  Pkg: TIDEPackage;
  PkgName, Path, SubPath, CurFilename, BaseDir, BuildMode: String;
  xml: TXMLConfig;
  LazBuildMode: TLazProjectBuildMode;
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
        Path:='ProjectOptions/Units/';
        Cnt:=xml.GetValue(Path+'Count',0);
        for i:=0 to Cnt-1 do begin
          SubPath:=Path+'Unit'+IntToStr(i)+'/';
          if xml.GetValue(SubPath+'IsPartOfProject/Value','')<>'True' then
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
        Cnt:=xml.GetValue(Path+'Count',0);
        for i:=1 to Cnt do begin
          SubPath:=Path+'Item'+IntToStr(i)+'/';
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

end.

