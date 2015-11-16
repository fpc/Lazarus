{
  ToDo:
    - Build file
    - build modes of project as nodes with checkboxes
    - run external tool
}
unit ProjectGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  Laz2_XMLCfg,
  Controls, Forms, Dialogs, LazFileUtils, LazFileCache,
  PackageIntf, ProjectIntf, MenuIntf,
  LazIDEIntf, IDEDialogs, CompOptsIntf, ProjectGroupIntf,
  ProjectGroupStrConst;


type

  { TIDECompileTarget }

  TIDECompileTarget = class(TCompileTarget)
  private
    FTarget: TPersistent;
  protected
    procedure LoadPackage;
    procedure LoadProject;
    procedure LoadProjectGroup;
    function ProjectAction(AAction: TPGTargetAction): TPGActionResult;
    function PackageAction(AAction: TPGTargetAction): TPGActionResult;
    function ProjectGroupAction(AAction: TPGTargetAction): TPGActionResult;
    function GetIDEPackage: TIDEPackage; override;
    function GetLazProject: TLazProject; override;
    function GetProjectGroup: TProjectGroup; override;
    function PerformAction(AAction: TPGTargetAction): TPGActionResult; override;
  public
    procedure LoadTarget;
    procedure UnLoadTarget;
  end;

  // Since a project group iself is also a target, we need a target to represent
  // the root projectgroup.

  { TProjectGroupTarget }

  TProjectGroupTarget = Class(TIDECompileTarget)
  protected
    procedure SetTargetType(AValue: TPGTargetType); override;
  public
    constructor Create(AProjectGroup: TProjectGroup);
  end;

  TTargetEvent = procedure(Sender: TObject; Target: TCompileTarget) of object;
  TTargetExchangeEvent = procedure(Sender: TObject; Target1,Target2: TCompileTarget) of object; // ToDo: use index

  { TIDEProjectGroup }

  TIDEProjectGroup = Class(TProjectGroup)
  private
    FOnFileNameChange: TNotifyEvent;
    FOnTargetActivated: TTargetEvent;
    FOnTargetAdded: TTargetEvent;
    FOnTargetDeleted: TTargetEvent;
    FOnTargetsExchanged: TTargetExchangeEvent;
    FTargets: TFPObjectList;
    FRemovedTargets: TFPObjectList;
    FModified: Boolean;
  protected
    procedure SetFileName(AValue: String); override;
    function GetModified: Boolean; override;
    function GetTarget(Index: Integer): TCompileTarget; override;
    function GetTargetCount: Integer; override;
    function GetRemovedTargetCount: Integer; override;
    function GetRemovedTarget(Index: Integer): TCompileTarget; override;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfTarget(const Target: TCompileTarget): Integer; override;
    function IndexOfRemovedTarget(const Target: TCompileTarget): Integer; override;
    function AddTarget(Const AFileName: String): TCompileTarget; override;
    procedure RemoveTarget(Index: Integer); override;
    procedure ExchangeTargets(ASource, ATarget: Integer); override; // ToDo: replace with MoveTarget
    procedure ActivateTarget(T: TCompileTarget); override;
    function LoadFromFile(Options: TProjectGroupLoadOptions): Boolean;
    function SaveToFile: Boolean;
    property OnFileNameChange: TNotifyEvent Read FOnFileNameChange Write FOnFileNameChange;
    property OnTargetAdded: TTargetEvent Read FOnTargetAdded Write FOnTargetAdded;
    property OnTargetDeleted: TTargetEvent Read FOnTargetDeleted Write FOnTargetDeleted;
    property OnTargetActivated: TTargetEvent Read FOnTargetActivated Write FOnTargetActivated;
    property OnTargetsExchanged: TTargetExchangeEvent Read FOnTargetsExchanged Write FOnTargetsExchanged;
  end;

  { TIDEProjectGroupManager }

  TIDEProjectGroupManager = Class(TProjectGroupManager)
  private
    function GetNewFileName: Boolean;
  protected
    FProjectGroup: TIDEProjectGroup;
  protected
    function CheckSaved: Boolean;
    function GetCurrentProjectGroup: TProjectGroup; override;
    function ShowProjectGroupEditor: Boolean;
  public
    // Events for main menu
    procedure DoNewClick(Sender: TObject); virtual;
    procedure DoOpenClick(Sender: TObject); virtual;
    procedure DoSaveClick(Sender: TObject); virtual;
    procedure DoSaveAsClick(Sender: TObject); virtual;
    // Public interface
    procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); override;
    procedure SaveProjectGroup; override;
  end;

  TEditProjectGroupOption = (epgoReusewindow);
  TEditProjectGroupOptions = Set of TEditProjectGroupOption;

  TEditProjectGroupHandler = procedure(AProjectGroup: TProjectGroup;
                                       Options: TEditProjectGroupOptions);
  // Method variant.
  TEditProjectGroupEvent = procedure(AProjectGroup: TProjectGroup;
                                   Options: TEditProjectGroupOptions) of object;

var
  OnEditProjectGroup: TEditProjectGroupHandler; // Takes precedence
  OnEditProjectGroupEvent: TEditProjectGroupEvent;

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
  cmdTargetUninstall: TIDEMenuCommand;



implementation

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
           FProjectGroup.FModified:=False;
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
    if Assigned(OnEditProjectGroup) then
      OnEditProjectGroup(FProjectGroup,[])
    else if Assigned(OnEditProjectGroupEvent) then
      OnEditProjectGroupEvent(FProjectGroup,[])
    else
      Result:=False;
  end;
end;

procedure TIDEProjectGroupManager.DoNewClick(Sender: TObject);
begin
  if Not CheckSaved then
    Exit;
  FreeAndNil(FProjectGroup);
  FProjectGroup:=TIDEProjectGroup.Create;
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
        LoadProjectGroup(FileName,[]);
      StoreIDEFileDialog(F);
    finally
      F.Free;
    end;
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
      if Result then
        FProjectGroup.FileName:=TrimAndExpandFilename(FileName);
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
  FProjectGroup:=TIDEProjectGroup.Create;
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

{ TProjectGroupTarget }

procedure TProjectGroupTarget.SetTargetType(AValue: TPGTargetType);
begin
  if (AValue<>ttProjectGroup) then
    Raise Exception.Create(lisErronlyProjectGroupAllowed);
  inherited SetTargetType(AValue);
end;

Constructor TProjectGroupTarget.Create(AProjectGroup: TProjectGroup);
begin
  FTarget:=AProjectGroup;
  TargetType:=ttProjectGroup;
end;

{ TIDEProjectGroup }

procedure TIDEProjectGroup.SetFileName(AValue: String);
begin
  if FileName=AValue then Exit;
  inherited SetFileName(AValue);
  if Assigned(FOnFileNameChange) then
    FOnFileNameChange(Self);
end;

function TIDEProjectGroup.GetModified: Boolean;
begin
  Result:=FModified;
end;

function TIDEProjectGroup.GetTarget(Index: Integer): TCompileTarget;
begin
  Result:=TCompileTarget(FTargets[Index]);
end;

function TIDEProjectGroup.GetTargetCount: Integer;
begin
  Result:=FTargets.Count;
end;

function TIDEProjectGroup.GetRemovedTargetCount: Integer;
begin
  Result:=FRemovedTargets.Count;
end;

function TIDEProjectGroup.GetRemovedTarget(Index: Integer): TCompileTarget;
begin
  Result:=TCompileTarget(FRemovedTargets[Index]);
end;

constructor TIDEProjectGroup.Create;
begin
  inherited Create;
  FTargets:=TFPObjectList.Create(True);
  FRemovedTargets:=TFPObjectList.Create(True);
end;

destructor TIDEProjectGroup.Destroy;
begin
  FreeAndNil(FTargets);
  FreeAndNil(FRemovedTargets);
  inherited Destroy;
end;

function TIDEProjectGroup.IndexOfTarget(const Target: TCompileTarget): Integer;
begin
  Result:=FTargets.IndexOf(Target);
end;

function TIDEProjectGroup.IndexOfRemovedTarget(const Target: TCompileTarget
  ): Integer;
begin
  Result:=FRemovedTargets.IndexOf(Target);
end;

function TIDEProjectGroup.AddTarget(const AFileName: String): TCompileTarget;
begin
  Result:=Nil;
  if FileExistsCached(AFileName) then
  begin
    Result:=TIDECompileTarget.Create;
    Result.FileName:=AFileName;
    FTargets.Add(Result);
    FModified:=True;
    If Assigned(FOnTargetAdded) then
      FOnTargetAdded(Self,Result);
  end;
end;

procedure TIDEProjectGroup.RemoveTarget(Index: Integer);
var
  Target: TCompileTarget;
begin
  Target:=Targets[Index];
  FTargets.Delete(Index);
  FRemovedTargets.Add(Target);
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
  FModified:=True;
end;

procedure TIDEProjectGroup.ActivateTarget(T: TCompileTarget);
begin
  if T.Active then exit;
  inherited ActivateTarget(T);
  If Assigned(FOnTargetActivated) then
    FOnTargetActivated(Self,T);
end;

function TIDEProjectGroup.LoadFromFile(Options: TProjectGroupLoadOptions
  ): Boolean;
Var
  ARoot: String;
  TargetFileName: String;
  TargetPath: String;
  XMLConfig: TXMLConfig;
  I,ACount: Integer;
  Target: TCompileTarget;
begin
  TargetPath:=ExpandFileNameUTF8(ExtractFilePath(FileName));
  Result:=True;
  try
    XMLConfig := TXMLConfig.Create(FileName);
    try
      ARoot:='ProjectGroup';
      ACount:=XMLConfig.GetValue(ARoot+'/Targets/Count',0);
      I:=0;
      While Result and (I<ACount) do
        begin
        Target:=Nil;
        TargetFileName:=XMLConfig.GetValue(Format(ARoot+'/Targets/Target%d/FileName',[i]),'');
        TargetFileName:=TrimFilename(SetDirSeparators(TargetFileName));
        if not FilenameIsAbsolute(TargetFileName) then
          TargetFileName:=TargetPath+TargetFileName;
        If (TargetFileName<>'') and FileExistsCached(TargetFileName) then
          Target:=AddTarget(TargetFileName)
        else if (pgloRemoveInvalid in Options) then
          begin
          Target:=AddTarget(TargetFileName);
          Target.Removed:=True;
          end
        else if (pgloSkipInvalid in options) then
          // Do nothing
        else if (pgloErrorInvalid in options) then
          Result:=False
        else
          Case IDEQuestionDialog(lisErrTargetDoesNotExist,
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
               Result:=False;
             mrYesToAll:
               begin
               Target:=AddTarget(TargetFileName);
               Target.Removed:=True;
               end;
          else
            Result:=False;
          end;
        if Assigned(Target) and Not Target.Removed then
          if XMLConfig.GetValue(Format(ARoot+'/Targets/Target%d/Active',[i]),False) then
            ActivateTarget(Target);
        Inc(I);
        end;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog('Read Error','Error reading project group file "'+Filename+'"'#13+E.Message,
        mtError,[mbOk]);
      Result:=false;
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
  CompTarget: TCompileTarget;
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
      FModified:=False;
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

procedure TIDECompileTarget.LoadTarget;
begin
  case TargetType of
    ttProject: LoadProject;
    ttPackage: LoadPackage;
    ttProjectGroup: LoadProjectGroup;
  end;
end;

procedure TIDECompileTarget.UnLoadTarget;
begin
  if (FTarget<>Nil) and (FTarget is TProjectGroup) then
    FreeAndNil(FTarget);
  FTarget:=Nil;
end;

procedure TIDECompileTarget.LoadPackage;
var
  MR: TModalResult;
  I: Integer;
  Pkg: TIDEPackage;
begin
  FTarget:=Nil;
  MR:=PackageEditingInterface.DoOpenPackageFile(Filename,
      [pofRevert, pofConvertMacros, pofDoNotOpenEditor],
      False);
  if (MR=mrOK) then
  begin
    I:=0;
    while (FTarget=Nil) and (I<PackageEditingInterface.GetPackageCount) do
    begin
      Pkg:=PackageEditingInterface.GetPackages(I);
      if CompareFilenames(Pkg.Filename,Self.Filename)=0 then
        FTarget:=Pkg; // ToDo: free notification
      Inc(I);
    end;
  end;
end;

procedure TIDECompileTarget.LoadProject;
const
  Flags = [];
{  Flags = [ofOnlyIfExists, ofProjectLoading, ofQuiet, ofVirtualFile,
           ofUseCache, ofMultiOpen, ofDoNotLoadResource,
           ofLoadHiddenResource, ofInternalFile];}
var
  MR: TModalResult;
begin
  UnloadTarget;
  MR:=LazarusIDE.DoOpenProjectFile(FileName,Flags);
  if (MR=mrOK) then
    FTarget:=LazarusIDE.ActiveProject; // ToDo: free notification
end;

procedure TIDECompileTarget.LoadProjectGroup;
var
  PG: TIDEProjectGroup;
begin
  PG:=TIDEProjectGroup.Create;
  PG.FileName:=Self.FileName;
  PG.LoadFromFile([]);
end;

function TIDECompileTarget.ProjectAction(AAction: TPGTargetAction): TPGActionResult;
var
  F: TProjectBuildFlags;
begin
  Result:=arOK;
  if (LazarusIDE.ActiveProject.ProjectInfoFile<>LazProject.ProjectInfoFile) then
    if  LazarusIDE.DoOpenProjectFile(FileName,[ofOnlyIfExists,ofQuiet,ofUseCache])<>mrOK then
      exit;
  // If action was open, we're now all set
  case AAction of
     taSettings :
       ; // TODO: Need IDE integration
     taCompileClean,
     taCompile :
       begin
       F:=[];
       if (AAction=taCompileClean) then
         Include(F,pbfCleanCompile);
       LazarusIDE.DoBuildProject(crCompile,F);
       end;
     taRun :
       ; // TODO: Need IDE integration
  end;
end;

function TIDECompileTarget.PackageAction(AAction: TPGTargetAction): TPGActionResult;
Var
  L: TObjectList;
begin
  Result:=arOK;
  if (AAction in [taOpen,taSettings]) then
    PackageEditingInterface.DoOpenPackageFile(FileName,[],False);
  case AAction of
     taSettings :
       ; // TODO: Need IDE integration
     taCompile :
       ; // TODO: Need IDE integration
     taCompileClean :
       ; // TODO: Need IDE integration
     taInstall :
       begin
       L:=TObjectList.Create(False);
       try
         L.Add(LazPackage);
         PackageEditingInterface.InstallPackages(L,[]);
       finally
         L.Free;
       end;
       end;
     taUninstall :
       ; // TODO: Need IDE integration
  end;
end;

function TIDECompileTarget.ProjectGroupAction(AAction: TPGTargetAction
  ): TPGActionResult;
begin
  if AAction=taOpen then
    ProjectGroupManager.LoadProjectGroup(FileName,[])
  else
    Result:=GetProjectGroup.PerformFrom(0,AAction);
end;

function TIDECompileTarget.GetIDEPackage: TIDEPackage;
begin
  If FTarget=Nil then
    LoadTarget;
  Result:=TIDEPackage(FTarget);
end;

function TIDECompileTarget.GetLazProject: TLazProject;
begin
  If FTarget=Nil then
    LoadTarget;
  Result:=TLazProject(FTarget);
end;

function TIDECompileTarget.GetProjectGroup: TProjectGroup;
begin
  If FTarget=Nil then
    LoadTarget;
  Result:=TProjectGroup(FTarget);
end;

function TIDECompileTarget.PerformAction(AAction: TPGTargetAction): TPGActionResult;
begin
  if FTarget=Nil then
    LoadTarget;
  case TargetType of
     ttProject: Result:=ProjectAction(AAction);
     ttPackage: Result:=PackageAction(AAction);
     ttProjectGroup: Result:=ProjectGroupAction(AAction);
  end;
end;

end.

