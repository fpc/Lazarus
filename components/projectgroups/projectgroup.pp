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
  Controls, Forms, Dialogs, LCLProc, LazFileUtils, LazFileCache,
  PackageIntf, ProjectIntf, MenuIntf,
  LazIDEIntf, IDEDialogs, CompOptsIntf, ProjectGroupIntf,
  ProjectGroupStrConst, FileProcs;


type
  { TIDECompileTarget }

  TIDECompileTarget = class(TPGCompileTarget)
  private
    FTarget: TPersistent;
    FFiles: TStringList;
    FRequiredPackages: TObjectList; // list of TPGDependency
  protected
    function GetFileCount: integer; override;
    function GetFiles(Index: integer): string; override;
    function GetRequiredPackageCount: integer; override;
    function GetRequiredPackages(Index: integer): TPGDependency; override;
    procedure LoadPackage;
    procedure LoadProject;
    procedure LoadProjectGroup;
    function ProjectAction(AAction: TPGTargetAction): TPGActionResult;
    function PackageAction(AAction: TPGTargetAction): TPGActionResult;
    function ProjectGroupAction(AAction: TPGTargetAction): TPGActionResult;
    function GetProjectGroup: TProjectGroup; override;
    function PerformAction(AAction: TPGTargetAction): TPGActionResult; override;
  public
    procedure LoadTarget;
    procedure UnLoadTarget;
  end;

  // Since a project group iself is also a target, we need a target to represent
  // the root projectgroup.

  { TProjectGroupTarget }

  TProjectGroupTarget = class(TIDECompileTarget)
  protected
    procedure SetTargetType(AValue: TPGTargetType); override;
  public
    constructor Create(AProjectGroup: TProjectGroup);
  end;

  TTargetEvent = procedure(Sender: TObject; Target: TPGCompileTarget) of object;
  TTargetExchangeEvent = procedure(Sender: TObject; Target1,Target2: TPGCompileTarget) of object; // ToDo: use index

  { TIDEProjectGroup }

  TIDEProjectGroup = class(TProjectGroup)
  private
    FOnFileNameChange: TNotifyEvent;
    FOnTargetActivated: TTargetEvent;
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
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfTarget(const Target: TPGCompileTarget): Integer; override;
    function IndexOfRemovedTarget(const Target: TPGCompileTarget): Integer; override;
    function AddTarget(Const AFileName: String): TPGCompileTarget; override;
    procedure RemoveTarget(Index: Integer); override;
    procedure ExchangeTargets(ASource, ATarget: Integer); override; // ToDo: replace with MoveTarget
    procedure ActivateTarget(T: TPGCompileTarget); override;
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

procedure TIDEProjectGroupManager.DoNewClick(Sender: TObject);
var
  AProject: TLazProject;
begin
  if Not CheckSaved then
    Exit;
  FreeAndNil(FProjectGroup);
  FProjectGroup:=TIDEProjectGroup.Create;

  // add current project
  AProject:=LazarusIDE.ActiveProject;
  if (AProject<>nil) and FilenameIsAbsolute(AProject.ProjectInfoFile)
  and FileExistsCached(AProject.ProjectInfoFile) then begin
    FProjectGroup.AddTarget(AProject.ProjectInfoFile);
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
  IncreaseChangeStamp;
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
  Result:=TIDECompileTarget.Create;
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
  IncreaseChangeStamp;
end;

procedure TIDEProjectGroup.ActivateTarget(T: TPGCompileTarget);
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
  BaseDir: String;
  XMLConfig: TXMLConfig;
  I,ACount: Integer;
  Target: TPGCompileTarget;
begin
  BaseDir:=AppendPathDelim(ExpandFileNameUTF8(ExtractFilePath(FileName)));
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
          TargetFileName:=BaseDir+TargetFileName;
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
  if FTarget<>nil then
    FreeAndNil(FTarget);
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
  FTarget:=Nil;

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
  FFiles:=TStringList.Create;
  for i:=0 to Pkg.FileCount-1 do
    FFiles.Add(Pkg.Files[i].Filename);

  // load list of required package
  FRequiredPackages:=TObjectList.Create(True);
  // ToDo
end;

procedure TIDECompileTarget.LoadProject;
var
  AProject: TLazProject;
  i: Integer;
  ProjFile: TLazProjectFile;
  PkgList: TFPList;
  Pkg: TIDEPackage;
  PkgName: String;
begin
  UnloadTarget;

  debugln(['TIDECompileTarget.LoadProject ',Filename]);
  AProject:=LazarusIDE.ActiveProject;
  if (AProject<>nil) and (CompareFilenames(AProject.ProjectInfoFile,Filename)=0)
  then begin
    // load from active project
    FFiles:=TStringList.Create;
    for i:=0 to AProject.FileCount-1 do begin
      ProjFile:=AProject.Files[i];
      if ProjFile.IsPartOfProject then
        FFiles.Add(ProjFile.Filename);
    end;

    // load dependencies from active project
    PkgList:=nil;
    try
      PackageEditingInterface.GetRequiredPackages(AProject,PkgList,[pirCompileOrder]);
      if PkgList<>nil then begin
        FRequiredPackages:=TObjectList.Create(True);
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

    LazarusIDE.ActiveProject;
    // ToDo
  end;
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
  Result:=arFailed;

  // ToDo: if project loaded
  if (LazarusIDE.ActiveProject<>nil)
  and (CompareFilenames(LazarusIDE.ActiveProject.ProjectInfoFile,Filename)=0)
  then begin
    // project loaded => use IDE functions
    // ToDo
    case AAction of
       taSettings :
         ; // TODO: Need IDE integration
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
         ; // TODO: Need IDE integration
    end;
  end else begin
    // project not loaded => use lazbuild
    // ToDo
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
  if AAction=taOpen then
    ProjectGroupManager.LoadProjectGroup(FileName,[])
  else
    Result:=GetProjectGroup.PerformFrom(0,AAction);
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

