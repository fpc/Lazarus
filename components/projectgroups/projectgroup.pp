{
  ToDo:
    - save/restore Active target
    - Build file
    - build modes of project as nodes with checkboxes
    - run external tool
}
unit ProjectGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Laz2_XMLCfg, Controls, Forms, Dialogs, LCLProc,
  LazFileUtils, LazFileCache, LazConfigStorage, FileUtil, PackageIntf,
  ProjectIntf, MenuIntf, LazIDEIntf, IDEDialogs, CompOptsIntf, BaseIDEIntf,
  IDECommands, IDEExternToolIntf, MacroIntf, IDEMsgIntf, ProjectGroupIntf,
  ProjectGroupStrConst, FileProcs, CodeToolManager, CodeCache;

const
  PGOptionsFileName = 'projectgroupsoptions.xml';

type
  { TIDECompileTarget }

  TIDECompileTarget = class(TPGCompileTarget)
  private
    FFiles: TStringList;
    FRequiredPackages: TObjectList; // list of TPGDependency
  protected
    function GetFileCount: integer; override;
    function GetFiles(Index: integer): string; override;
    function GetRequiredPackageCount: integer; override;
    function GetRequiredPackages(Index: integer): TPGDependency; override;
    procedure LoadPackage;
    procedure LoadProject;
    procedure LoadProjectGroup(Recursively: boolean);
    function ProjectAction(AAction: TPGTargetAction): TPGActionResult;
    function PackageAction(AAction: TPGTargetAction): TPGActionResult;
    function ProjectGroupAction(AAction: TPGTargetAction): TPGActionResult;
    function PascalFileAction(AAction: TPGTargetAction): TPGActionResult;
    function ExternalToolAction(AAction: TPGTargetAction): TPGActionResult;
    function PerformAction(AAction: TPGTargetAction): TPGActionResult; override;
    procedure ActiveChanged(Sender: TPGCompileTarget); override;
  public
    procedure LoadTarget(Recursively: boolean); virtual;
    procedure UnLoadTarget; virtual;
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
  TTargetExchangeEvent = procedure(Sender: TObject; Target1,Target2: TPGCompileTarget) of object; // ToDo: use index

  { TIDEProjectGroup }

  TIDEProjectGroup = class(TProjectGroup)
  private
    FActiveTarget: TPGCompileTarget;
    FOnFileNameChange: TNotifyEvent;
    FOnTargetActiveChanged: TTargetEvent;
    FOnTargetAdded: TTargetEvent;
    FOnTargetDeleted: TTargetEvent;
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
    function IndexOfTarget(const Target: TPGCompileTarget): Integer; override;
    function IndexOfRemovedTarget(const Target: TPGCompileTarget): Integer; override;
    function AddTarget(Const AFileName: String): TPGCompileTarget; override;
    procedure RemoveTarget(Index: Integer); override;
    procedure ExchangeTargets(ASource, ATarget: Integer); override; // ToDo: replace with MoveTarget
    procedure ActiveTargetChanged(T: TPGCompileTarget);
    function LoadFromFile(Options: TProjectGroupLoadOptions): Boolean;
    function SaveToFile: Boolean;
    property OnFileNameChange: TNotifyEvent Read FOnFileNameChange Write FOnFileNameChange;
    property OnTargetAdded: TTargetEvent Read FOnTargetAdded Write FOnTargetAdded;
    property OnTargetDeleted: TTargetEvent Read FOnTargetDeleted Write FOnTargetDeleted;
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
    function GetNewFileName: Boolean;
  protected
    FProjectGroup: TIDEProjectGroup;
  protected
    function CheckSaved: Boolean;
    function GetCurrentProjectGroup: TProjectGroup; override;
    function ShowProjectGroupEditor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateRecentProjectGroupMenu;
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
  // Project group editor(s). Should probably move to MenuIntf
  ProjectGroupMenuRoot: TIDEMenuSection = nil;
    PGEditMenuSectionFiles, // e.g. sort files, clean up files
    PGEditMenuSectionAddRemove, // e.g. add unit, add dependency
    PGEditMenuSectionCompile, // e.g. build clean, create Makefile
    PGEditMenuSectionUse, // Target up/down
    PGEditMenuSectionMisc: TIDEMenuSection; // e.g. options

var
  cmdOpenProjectGroup,
  cmdSaveProjectGroup,
  cmdCreateProjectGroup,
  cmdSaveProjectGroupAs,

  cmdTargetAdd,
  cmdTargetRemove,
  cmdTargetEarlier,
  cmdTargetActivate,
  cmdTargetLater,
  cmdTargetCompile,
  cmdTargetCompileClean,
  cmdTargetInstall,
  cmdTargetOpen,
  cmdTargetRun,
  cmdTargetProperties,
  cmdTargetUninstall,
  cmdTargetCopyFilename: TIDEMenuCommand;

  OpenRecentProjectGroupSubMenu: TIDEMenuSection;

function LoadXML(aFilename: string; Quiet: boolean): TXMLConfig;
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
    exit;
  end;
  Code:=CodeToolBoss.LoadFile(aFilename,true,false);
  if Code=nil then begin
    debugln(['Error: (lazarus) [TIDECompileTarget.LoadXML] unable to load file "',aFilename,'"']);
    if not Quiet then
      IDEMessageDialog('Read error','Unable to load file "'+aFilename+'"',mtError,[mbOk]);
    exit;
  end;
  try
    Result:=TXMLConfig.CreateWithSource(aFilename,Code.Source);
  except
    on E: Exception do begin
      debugln(['Error: (lazarus) [TIDECompileTarget.LoadXML] xml syntax error in "',aFilename,'": '+E.Message]);
      if not Quiet then
        IDEMessageDialog('Read error','XML syntax error in file "'+aFilename+'": '+E.Message,mtError,[mbOk]);
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
  Result:=Not Assigned(FProjectGroup);
  if Not Result then
    begin
    Result:=Not FProjectGroup.Modified;
    If Not Result then
      // For some reason, only 2 buttons are shown ???
      Case IDEQuestionDialog(lisProjectGroupModified,
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
    if i<OpenRecentProjectGroupSubMenu.Count then begin
      Item:=OpenRecentProjectGroupSubMenu[i];
      Item.Caption:=aFilename;
    end
    else begin
      Item:=RegisterIDEMenuCommand(OpenRecentProjectGroupSubMenu,'OpenRecentProjectGroup'+IntToStr(i),aFilename,@DoOpenRecentClick);
    end;
    inc(i);
  end;
  while i<OpenRecentProjectGroupSubMenu.Count do
    OpenRecentProjectGroupSubMenu[i].Free;
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
      F.Filter:='Lazarus project group|*.lpg|All files|'+AllFilesMask;
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
  debugln(['TIDEProjectGroupManager.DoOpenRecentClick ',aFilename]);
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
      F.Filter:=lisLazarusProjectGroup+'|*.lpg|'+lisAllFiles+'|'+AllFilesMask;
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

procedure TIDEProjectGroupManager.DoSaveAsClick(Sender: TObject);
begin
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

  Options.AddToRecentProjectGroups(AFileName);
  Options.SaveSafe;
  UpdateRecentProjectGroupMenu;

  FProjectGroup:=TIDEProjectGroup.Create(nil);
  FProjectGroup.FileName:=AFileName;
  FProjectGroup.LoadFromFile(AOptions);
  If not (pgloSkipDialog in AOptions) then
    ShowProjectGroupEditor;
end;

procedure TIDEProjectGroupManager.SaveProjectGroup;
begin
  If Assigned(FProjectGroup) then
  begin
    If (FProjectGroup.FileName<>'') or GetNewFileName then
      FProjectGroup.SaveToFile;
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
  end;
  FTargets:=TFPObjectList.Create(True);
  FRemovedTargets:=TFPObjectList.Create(True);
end;

destructor TIDEProjectGroup.Destroy;
begin
  FreeAndNil(FTargets);
  FreeAndNil(FRemovedTargets);
  if FParent=nil then
    FreeAndNil(FCompileTarget);
  inherited Destroy;
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
begin
  Result:=Nil;
  if not FilenameIsAbsolute(AFileName) then
    RaiseGDBException(AFileName);
  if not FileExistsCached(AFileName) then exit;
  Result:=TIDECompileTarget.Create(CompileTarget);
  Result.FileName:=AFileName;
  FTargets.Add(Result);
  IncreaseChangeStamp;
  If Assigned(FOnTargetAdded) then
    FOnTargetAdded(Self,Result);
end;

procedure TIDEProjectGroup.RemoveTarget(Index: Integer);
var
  Target: TPGCompileTarget;
begin
  Target:=Targets[Index];
  Target.DeActivate;
  FTargets.OwnsObjects:=false;
  FTargets.Delete(Index);
  FTargets.OwnsObjects:=true;
  FRemovedTargets.Add(Target);
  Modified:=true;
  Target.Removed:=true;
  if Assigned(FOnTargetDeleted) then
    FOnTargetDeleted(Self,Target);
end;

procedure TIDEProjectGroup.ExchangeTargets(ASource, ATarget: Integer);
begin
  if ASource=ATarget then exit;
  if Assigned(FOnTargetsExchanged) then
    FOnTargetsExchanged(Self,GetTarget(ASource),GetTarget(ATarget));
  FTargets.Exchange(ASource,ATarget);
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroup.ActiveTargetChanged(T: TPGCompileTarget);
begin
  if T.Active then begin
    FActiveTarget:=T;
  end else begin
    if FActiveTarget=T then
      FActiveTarget:=nil;
  end;
  if Assigned(FOnTargetActiveChanged) then
    FOnTargetActiveChanged(Self,T);
end;

function TIDEProjectGroup.LoadFromFile(Options: TProjectGroupLoadOptions
  ): Boolean;
Var
  ARoot: String;
  TargetFileName: String;
  BaseDir: String;
  XMLConfig: TXMLConfig;
  I,ACount: Integer;
  Target: TPGCompileTarget;
  aGroup: TProjectGroup;
begin
  Result:=false;
  if not FilenameIsAbsolute(FileName) then exit;
  if not FileExistsCached(Filename) then exit;

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
      I:=0;
      While (I<ACount) do
      begin
        Target:=Nil;
        TargetFileName:=XMLConfig.GetValue(Format(ARoot+'/Targets/Target%d/FileName',[i]),'');
        TargetFileName:=TrimFilename(SetDirSeparators(TargetFileName));
        if not FilenameIsAbsolute(TargetFileName) then
          TargetFileName:=TrimFilename(BaseDir+TargetFileName);
        If (TargetFileName<>'') and FileExistsCached(TargetFileName) then begin
          Target:=AddTarget(TargetFileName);
          if pgloLoadRecursively in Options then
            (Target as TIDECompileTarget).LoadTarget(true);
        end
        else if (pgloRemoveInvalid in Options) then
        begin
          Target:=AddTarget(TargetFileName);
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
               Target:=AddTarget(TargetFileName);
               Target.Removed:=True;
             end;
           mrNo:
             exit;
           mrYesToAll:
             begin
               Target:=AddTarget(TargetFileName);
               Target.Removed:=True;
             end;
          else
            exit;
          end;
        if Assigned(Target) and Not Target.Removed then
          if XMLConfig.GetValue(Format(ARoot+'/Targets/Target%d/Active',[i]),False) then
            Target.Activate;
        Inc(I);
      end;
    finally
      Modified:=false;
      XMLConfig.Free;
    end;
    Result:=true;
  except
    on E: Exception do begin
      IDEMessageDialog('Read Error','Error reading project group file "'+Filename+'"'#13+E.Message,
        mtError,[mbOk]);
    end;
  end;
end;

function TIDEProjectGroup.SaveToFile: Boolean;
Var
  TargetPath: String;
  RelativeFileName: String;
  ARoot: String;
  XMLConfig: TXMLConfig;
  I,ACount: Integer;
  CompTarget: TPGCompileTarget;
begin
  TargetPath:=ExtractFilePath(FileName);
  Result:=True;
  try
    XMLConfig := TXMLConfig.Create(FileName);
    try
      ARoot:='ProjectGroup';
      ACount:=0;
      For I:=0 to TargetCount-1 do
        if not GetTarget(I).Removed then
          Inc(ACount);
      XMLConfig.Clear;
      XMLConfig.SetValue(ARoot+'/Targets/Count',ACount);
      I:=0;
      ACount:=0;
      For I:=0 to TargetCount-1 do
        begin
        CompTarget:=GetTarget(I);
        If not CompTarget.Removed then
          begin
          RelativeFileName:=ExtractRelativepath(TargetPath,CompTarget.FileName);
          XMLConfig.SetValue(Format(ARoot+'/Targets/Target%d/FileName',[ACount]),RelativeFileName);
          Inc(ACount);
          end;
        end;
      XMLConfig.Flush;
      Modified:=False;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog('Write Error','Unable to write project group file "'+Filename+'"'#13+E.Message,
        mtError,[mbOk]);
      Result:=false;
    end;
  end;
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

procedure TIDECompileTarget.UnLoadTarget;
begin
  if FProjectGroup<>nil then
    FreeAndNil(FProjectGroup);
  if FFiles<>nil then
    FreeAndNil(FFiles);
  if FRequiredPackages<>nil then
    FreeAndNil(FRequiredPackages);
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
  Pkg: TIDEPackage;
  PkgName: String;
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
  // ToDo
end;

procedure TIDECompileTarget.LoadProject;
var
  AProject: TLazProject;
  i, Cnt: Integer;
  ProjFile: TLazProjectFile;
  PkgList: TFPList;
  Pkg: TIDEPackage;
  PkgName, Path, SubPath, CurFilename, BaseDir: String;
  xml: TXMLConfig;
begin
  if FFiles<>nil then exit; // already loaded

  debugln(['TIDECompileTarget.LoadProject ',Filename]);
  FFiles:=TStringList.Create;
  FRequiredPackages:=TObjectList.Create(True);

  AProject:=LazarusIDE.ActiveProject;
  if (AProject<>nil) and (CompareFilenames(AProject.ProjectInfoFile,Filename)=0)
  then begin
    // load from active project
    FFiles:=TStringList.Create;
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
          FRequiredPackages.Add(TPGDependency.Create(PkgName));
        end;
      end;
    finally
      PkgList.Free;
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
          FRequiredPackages.Add(TPGDependency.Create(PkgName));
        end;
      end;
    finally
      xml.Free;
    end;
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

function TIDECompileTarget.ProjectAction(AAction: TPGTargetAction): TPGActionResult;
var
  F: TProjectBuildFlags;
  Tool: TAbstractExternalTool;
  aTitle, aCompileHint, LazBuildFilename, WorkingDir: String;
  Params: TStringList;
  FPCParser: TFPCParser;
begin
  Result:=arFailed;

  // ToDo: if project loaded
  if (LazarusIDE.ActiveProject<>nil)
  and (CompareFilenames(LazarusIDE.ActiveProject.ProjectInfoFile,Filename)=0)
  then begin
    // project loaded => use IDE functions
    case AAction of
       taSettings :
         begin
           if ExecuteIDECommand(Self,ecProjectOptions) then
             Result:=arOK;
         end;
       taCompileClean,
       taCompile :
         begin
           F:=[];
           if (AAction=taCompileClean) then
             Include(F,pbfCleanCompile);
           if LazarusIDE.DoBuildProject(crCompile,F)=mrOk then
             exit(arOK);
         end;
       taRun :
         begin
           if LazarusIDE.DoRunProject=mrOk then
             Result:=arOk;
         end;
    end;
  end else begin
    // project not loaded => use lazbuild
    case AAction of
    taOpen:
      begin
        // open project
        if LazarusIDE.DoOpenProjectFile(Filename,[ofAddToRecent])=mrOk then
          Result:=arOk;
      end;
    taSettings:
      begin
        // open project, then show options
        if LazarusIDE.DoOpenProjectFile(Filename,[ofAddToRecent])<>mrOk then
          exit(arFailed);
        if ExecuteIDECommand(Self,ecProjectOptions) then
          Result:=arOK;
      end;
    taCompile,
    taCompileClean:
      begin
        // run lazbuild as external tool
        IDEMessagesWindow.Clear;

        aTitle:='Compile Project '+ExtractFileNameOnly(Filename);
        aCompileHint:='Project Group: '+Parent.Filename+LineEnding;

        LazBuildFilename:=GetLazBuildFilename;
        if LazBuildFilename='' then begin
          IDEMessageDialog('lazbuild not found','The lazbuild'+ExeExt+' was not found.'
            ,mtError,[mbOk]);
          exit(arFailed);
        end;

        WorkingDir:=ExtractFilePath(Filename);
        Params:=TStringList.Create;
        if AAction=taCompileClean then
          Params.Add('-B');
        Params.Add(Filename);

        Tool:=ExternalToolList.Add(aTitle);
        Tool.Reference(Self,ClassName);
        try
          Tool.Data:=TIDEExternalToolData.Create('Other Project','',Filename);
          Tool.FreeData:=true;
          Tool.Hint:=aCompileHint;
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
    taRun:
      begin
        // open project, then run
        if LazarusIDE.DoOpenProjectFile(Filename,[ofAddToRecent])<>mrOk then
          exit(arFailed);
        if LazarusIDE.DoRunProject=mrOk then
          Result:=arOk;
      end;
    end;
  end;
end;

function TIDECompileTarget.PackageAction(AAction: TPGTargetAction): TPGActionResult;
begin
  Result:=arFailed;

  if (AAction in [taOpen,taSettings]) then
    if PackageEditingInterface.DoOpenPackageFile(FileName,[pofDoNotOpenEditor],False)<>mrOk then
      exit;
  case AAction of
     taSettings :
       ; // TODO: Need IDE integration
     taCompile :
       ; // TODO: Need IDE integration
     taCompileClean :
       ; // TODO: Need IDE integration
     taInstall :
       ; // TODO: Need IDE integration
     taUninstall :
       ; // TODO: Need IDE integration
  end;
end;

function TIDECompileTarget.ProjectGroupAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=arFailed;

  case AAction of
  taOpen: ProjectGroupManager.LoadProjectGroup(FileName,[]);
  taSettings: ;
  taCompile: ;
  taCompileClean: ;
  taRun: ;
  taInstall: ;
  taUninstall: ;
  end;
end;

function TIDECompileTarget.PascalFileAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=arFailed;
  debugln(['TIDECompileTarget.PascalFileAction ToDo']);
  case AAction of
  taOpen: ;
  taSettings: ;
  taCompile: ;
  taCompileClean: ;
  taRun: ;
  taInstall: ;
  taUninstall: ;
  end;
end;

function TIDECompileTarget.ExternalToolAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=arFailed;
  debugln(['TIDECompileTarget.ExternalToolAction ToDo']);
  case AAction of
  taOpen: ;
  taSettings: ;
  taCompile: ;
  taCompileClean: ;
  taRun: ;
  taInstall: ;
  taUninstall: ;
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

procedure TIDECompileTarget.ActiveChanged(Sender: TPGCompileTarget);
begin
  (GetRootProjectGroup as TIDEProjectGroup).ActiveTargetChanged(Sender);
end;

end.

