unit ProjectGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  Laz2_XMLCfg,
  Controls, Forms, Dialogs, LazFileUtils, LazFileCache,
  ProjectGroupIntf, PackageIntf, ProjectIntf, MenuIntf,
  LazIDEIntf, IDEDialogs, CompOptsIntf;


Type

  { TIDECompileTarget }

  TIDECompileTarget = class(TCompileTarget)
  private
    FTarget : TPersistent;
  protected
    Procedure LoadPackage;
    Procedure LoadProject;
    Procedure LoadProjectGroup;
    Function ProjectAction(AAction : TTargetAction) : TActionResult;
    Function PackageAction(AAction : TTargetAction) : TActionResult;
    Function ProjectGroupAction(AAction : TTargetAction) : TActionResult;
    function GetIDEPackage: TIDEPackage; override;
    function GetLazProject: TLazProject; override;
    function GetProjectGroup: TProjectGroup; override;
    function PerformAction(AAction: TTargetAction): TActionResult; override;
  public
    Procedure LoadTarget;
    Procedure UnLoadTarget;
  end;

  // Since a project group iself is also a target, we need a target to represent
  // the root projectgroup.

  { TProjectGroupTarget }

  TProjectGroupTarget = Class(TIDECompileTarget)
  protected
    procedure SetTargetType(AValue: TTargetType); override;
  Public
    Constructor Create(AProjectGroup : TProjectGroup);
  end;

  TTargetEvent = Procedure(Sender : TObject; Target : TCompileTarget) of object;
  TTargetExchangeEvent = Procedure(Sender : TObject; Target1,Target2 : TCompileTarget) of object;

  { TIDEProjectGroup }

  TIDEProjectGroup = Class(TProjectGroup)
  Private
    FOnFileNameChange: TNotifyEvent;
    FOnTargetActivated: TTargetEvent;
    FOnTargetAdded: TTargetEvent;
    FOnTargetDeleted: TTargetEvent;
    FOnTargetsExchanged: TTargetExchangeEvent;
    FTargets : TFPObjectList;
    FModified : Boolean;
  protected
    procedure SetFileName(AValue: String); override;
    function GetModified: Boolean; override;
    function GetTarget(AIndex: Integer): TCompileTarget; override;
    function GetTargetCount: Integer; override;
  Public
    Constructor Create;
    Function AddTarget(Const AFileName: String): TCompileTarget; override;
    Procedure RemoveTarget(T : TCompileTarget) ; override;
    Procedure ExchangeTargets(ASource, ATarget: Integer); override;
    Procedure ActivateTarget(T : TCompileTarget); override;
    Function LoadFromFile(Options : TProjectGroupLoadOptions ) : Boolean;
    Function SaveToFile : Boolean;
    Property OnFileNameChange : TNotifyEvent Read FOnFileNameChange Write FOnFileNameChange;
    Property OnTargetAdded : TTargetEvent Read FOnTargetAdded Write FOnTargetAdded;
    Property OnTargetDeleted : TTargetEvent Read FOnTargetDeleted Write FOnTargetDeleted;
    Property OnTargetActivated : TTargetEvent Read FOnTargetActivated Write FOnTargetActivated;
    Property OnTargetsExchanged : TTargetExchangeEvent Read FOnTargetsExchanged Write FOnTargetsExchanged;
  end;

  { TIDEProjectGroupManager }

  TIDEProjectGroupManager = Class(TProjectGroupManager)
  private
    Function GetNewFileName: Boolean;
  Protected
    FProjectGroup : TIDEProjectGroup;
  protected
    Function CheckSaved : Boolean;
    function GetCurrentProjectGroup: TProjectGroup; override;
    Function ShowProjectGroupEditor : Boolean;
  public
    // Events for main menu
    Procedure DoNewClick(Sender : TObject); virtual;
    Procedure DoOpenClick(Sender : TObject); virtual;
    Procedure DoSaveClick(Sender : TObject); virtual;
    Procedure DoSaveAsClick(Sender : TObject); virtual;
    // Public interface
    Procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); override;
    Procedure SaveProjectGroup; override;
  end;

  TEditProjectGroupOption = (epgoReusewindow);
  TEditProjectGroupOptions = Set of TEditProjectGroupOption;

  TEditProjectGroupHandler = Procedure(AProjectGroup : TProjectGroup; Options : TEditProjectGroupOptions);
  // Method variant.
  TEditProjectGroupEvent = Procedure(AProjectGroup : TProjectGroup; Options : TEditProjectGroupOptions) of object;

Var
  OnEditProjectGroup : TEditProjectGroupHandler; // Takes precedence
  OnEditProjectGroupEvent : TEditProjectGroupEvent;

  IDEProjectGroupManager : TIDEProjectGroupManager;
  // Project group editor(s). Should probably move to MenuIntf
  ProjectGroupMenuRoot: TIDEMenuSection = nil;
    PGEditMenuSectionFiles, // e.g. sort files, clean up files
    PGEditMenuSectionAddRemove, // e.g. add unit, add dependency
    PGEditMenuSectionCompile, // e.g. build clean, create Makefile
    PGEditMenuSectionUse, // Target up/down
    PGEditMenuSectionMisc: TIDEMenuSection; // e.g. options

Var
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

Resourcestring
  lisErrTargetDoesNotExist      = 'Target does not exist. Remove ?';
  lisErrNoSuchFile              = 'Could not find target file'+sLineBreak+
                                  '"%s"'+sLineBreak+
                                  'What do you want to do ?';
  lisRemoveTarget                = 'Remove target';
  lisAbortLoadingProjectGroup    = 'Abort loading project group';
  lisSkipAllTargets              = 'Remove all invalid targets';
  lisErrOnlyProjectGroupAllowed  = 'Only target type "projectgroup" is allowed for root project group';
  lisProjectGroupModified        = 'Project group modified';
  lisProjectGroupModifiedConfirm = 'Project group "%s" is modified.'+sLineBreak+
                                   'what do you want to do?';

  lisSavePG  = 'Save project group';
  lisDiscard = 'Discard changes';
  lisAbort   = 'Abort';


{ TIDEProjectGroupManager }

Function TIDEProjectGroupManager.CheckSaved: Boolean;
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

Function TIDEProjectGroupManager.ShowProjectGroupEditor : Boolean;

begin
  Result:=Assigned(FProjectGroup);
  if Result then
    begin
    if Assigned(OnEditProjectGroup) then
      OnEditProjectGroup(FProjectGroup,[])
    else if Assigned(OnEditProjectGroupEvent) then
      OnEditProjectGroupEvent(FProjectGroup,[])
    Else
      Result:=False;
    end;
end;

Procedure TIDEProjectGroupManager.DoNewClick(Sender: TObject);
begin
  if Not CheckSaved then
    Exit;
  FreeAndNil(FProjectGroup);
  FProjectGroup:=TIDEProjectGroup.Create;
  ShowProjectGroupEditor;
end;

Procedure TIDEProjectGroupManager.DoOpenClick(Sender: TObject);

Var
  F : TOpenDialog;
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

Procedure TIDEProjectGroupManager.DoSaveClick(Sender: TObject);
begin
  SaveProjectGroup;
end;

Function TIDEProjectGroupManager.GetNewFileName : Boolean;
Var
  F : TSaveDialog;
begin
  Result:=False;
  F:=TSaveDialog.Create(Nil);
  With F do
   try
    FileName:=FProjectGroup.FileName;
    InitIDEFileDialog(F);
    F.Options:=[ofOverwritePrompt,ofPathMustExist,ofEnableSizing];
    F.Filter:='Lazarus project group|*.lpg|All files|'+AllFilesMask;
    F.DefaultExt:='.lpg';
    Result:=F.Execute;
    if Result then
      FProjectGroup.FileName:=TrimAndExpandFilename(FileName);
    StoreIDEFileDialog(F);
   finally
     F.Free;
   end;
end;

Procedure TIDEProjectGroupManager.DoSaveAsClick(Sender: TObject);
begin
  if GetNewFileName then
    SaveProjectGroup;
end;

Procedure TIDEProjectGroupManager.LoadProjectGroup(AFileName: string;
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

Procedure TIDEProjectGroupManager.SaveProjectGroup;
begin
  If Assigned(FProjectGroup) then
  begin
    If (FProjectGroup.FileName<>'') or GetNewFileName then
      FProjectGroup.SaveToFile;
    end;
end;

{ TProjectGroupTarget }

procedure TProjectGroupTarget.SetTargetType(AValue: TTargetType);
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

function TIDEProjectGroup.GetTarget(AIndex: Integer): TCompileTarget;
begin
  Result:=TCompileTarget(FTargets[AIndex]);
end;

function TIDEProjectGroup.GetTargetCount: Integer;
begin
  Result:=FTargets.Count;
end;

Constructor TIDEProjectGroup.Create;
begin
  inherited Create;
  FTargets:=TFPObjectList.Create(True);
end;

Function TIDEProjectGroup.AddTarget(Const AFileName: String): TCompileTarget;
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

Procedure TIDEProjectGroup.RemoveTarget(T: TCompileTarget);
begin
  If Assigned(FOnTargetDeleted) then
    FOnTargetDeleted(Self,T);
  inherited RemoveTarget(T);
end;

Procedure TIDEProjectGroup.ExchangeTargets(ASource, ATarget: Integer);
begin
  if ASource=ATarget then exit;
  if Assigned(FOnTargetsExchanged) then
    FOnTargetsExchanged(Self,GetTarget(ASource),GetTarget(ATarget));
  FTargets.Exchange(ASource,ATarget);
  FModified:=True;
end;

Procedure TIDEProjectGroup.ActivateTarget(T: TCompileTarget);
begin
  if T.Active then exit;
  inherited ActivateTarget(T);
  If Assigned(FOnTargetActivated) then
    FOnTargetActivated(Self,T);
end;

Function TIDEProjectGroup.LoadFromFile(Options: TProjectGroupLoadOptions
  ): Boolean;
Var
  ARoot : String;
  TargetFileName : String;
  TargetPath : String;
  XMLConfig : TXMLConfig;
  I,ACount : Integer;
  Target : TCompileTarget;
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

Function TIDEProjectGroup.SaveToFile: Boolean;
Var
  TargetPath : String;
  RelativeFileName : String;
  ARoot : String;
  XMLConfig : TXMLConfig;
  I,ACount : Integer;
  CompTarget : TCompileTarget;
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

Procedure TIDECompileTarget.LoadTarget;
begin
  Case TargetType of
    ttProject : LoadProject;
    ttPackage : LoadPackage;
    ttProjectGroup : LoadProjectGroup;
  end;
end;

Procedure TIDECompileTarget.UnLoadTarget;
begin
  if (FTarget<>Nil) and (FTarget is TProjectGroup) then
    FreeAndNil(FTarget);
  FTarget:=Nil;
end;

Procedure TIDECompileTarget.LoadPackage;

Var
  MR : TModalResult;
  I : Integer;
  Pkg : TIDEPackage;

begin
  FTarget:=Nil;
  MR:=PackageEditingInterface.DoOpenPackageFile(Filename,
      [pofRevert, pofConvertMacros, pofDoNotOpenEditor],
      False);
  If (MR=mrOK) then
    begin
    I:=0;
    While (FTarget=Nil) and (I<PackageEditingInterface.GetPackageCount) do
      begin
      Pkg:=PackageEditingInterface.GetPackages(I);
      If CompareFilenames(Pkg.Filename,Self.Filename)=0 then
        FTarget:=Pkg; // ToDo: free notification
      Inc(I);
      end;
    end;
end;

Procedure TIDECompileTarget.LoadProject;

Const
  Flags = [];
{  Flags = [ofOnlyIfExists, ofProjectLoading, ofQuiet, ofVirtualFile,
           ofUseCache, ofMultiOpen, ofDoNotLoadResource,
           ofLoadHiddenResource, ofInternalFile];}

Var
  MR : TModalResult;

begin
  UnloadTarget;
  MR:=LazarusIDE.DoOpenProjectFile(FileName,Flags);
  if (MR=mrOK) then
    FTarget:=LazarusIDE.ActiveProject; // ToDo: free notification
end;

Procedure TIDECompileTarget.LoadProjectGroup;

Var
  PG : TIDEProjectGroup;

begin
  PG:=TIDEProjectGroup.Create;
  PG.FileName:=Self.FileName;
  PG.LoadFromFile([]);
end;

Function TIDECompileTarget.ProjectAction(AAction: TTargetAction): TActionResult;
Var
  F : TProjectBuildFlags;
begin
  Result:=arOK;
  If (LazarusIDE.ActiveProject.ProjectInfoFile<>LazProject.ProjectInfoFile) then
    if  LazarusIDE.DoOpenProjectFile(FileName,[ofOnlyIfExists,ofQuiet,ofUseCache])<>mrOK then
      exit;
  // If action was open, we're now all set
  Case AAction of
     taSettings :
       ; // TODO : Need IDE integration
     taCompileClean,
     taCompile :
       begin
       F:=[];
       if (AAction=taCompileClean) then
         Include(F,pbfCleanCompile);
       LazarusIDE.DoBuildProject(crCompile,F);
       end;
     taRun :
       ; // TODO : Need IDE integration
  end;
end;

Function TIDECompileTarget.PackageAction(AAction: TTargetAction): TActionResult;
Var
  L : TObjectList;
begin
  Result:=arOK;
  if (AAction in [taOpen,taSettings]) then
    PackageEditingInterface.DoOpenPackageFile(FileName,[],False);
  Case AAction of
     taSettings :
       ; // TODO : Need IDE integration
     taCompile :
       ; // TODO : Need IDE integration
     taCompileClean :
       ; // TODO : Need IDE integration
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
       ; // TODO : Need IDE integration
  end;
end;

Function TIDECompileTarget.ProjectGroupAction(AAction: TTargetAction
  ): TActionResult;
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

function TIDECompileTarget.PerformAction(AAction: TTargetAction): TActionResult;
begin
  if FTarget=Nil then
    LoadTarget;
  Case TargetType of
     ttProject : Result:=ProjectAction(AAction);
     ttPackage : Result:=PackageAction(AAction);
     ttProjectGroup : Result:=ProjectGroupAction(AAction);
  end;
end;

end.

