{ If you want to extend the package only access this unit.
}
unit ProjectGroupIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, PackageIntf, ProjectIntf, LazFileUtils,
  LazFileCache, LazMethodList, LazLoggerBase;

Type
  TPGTargetType = (
    ttUnknown,
    ttProject,
    ttPackage,
    ttProjectGroup, // nested group
    ttPascalFile,  // build/run file, parameters stored IDE directives
    ttExternalTool
    );
  TPGTargetTypes = set of TPGTargetType;

  TPGTargetAction = (
    taOpen,
    taSettings,
    taCompile,
    taCompileClean,
    taCompileFromHere,
    taRun,
    taInstall,
    taUninstall);
  TPGTargetActions = set of TPGTargetAction;

  TPGActionResult = (arNotAllowed,arOK,arFailed);
  TPGActionResults = set of TPGActionResult;

  TProjectGroup = class;
  TPGCompileTarget = class;

  { TPGBuildMode }

  TPGBuildMode = class
  private
    FCompile: boolean;
    FIdentifier: string;
    FTarget: TPGCompileTarget;
    procedure SetCompile(AValue: boolean);
  public
    constructor Create(aTarget: TPGCompileTarget; const anIdentifier: string; aCompile: boolean);
    property Target: TPGCompileTarget read FTarget;
    property Identifier: string read FIdentifier;
    property Compile: boolean read FCompile write SetCompile;
  end;

  { TPGDependency }

  TPGDependency = class
  private
    FPackageName: string;
    FTarget: TPGCompileTarget;
  public
    constructor Create(aTarget: TPGCompileTarget; const aPkgName: string);
    property Target: TPGCompileTarget read FTarget;
    property PackageName: string read FPackageName;
  end;

  { TPGCompileTarget - a node in the tree, see TPGTargetType }

  TPGCompileTarget = class
  private
    FActive: Boolean;
    FFilename: string;
    FTargetType: TPGTargetType;
    FMissing: boolean;
  protected
    FParent: TPGCompileTarget;
    FProjectGroup: TProjectGroup;
    function GetAllowedActions: TPGTargetActions; virtual; // By default, return all allowed actions for target type.
    function GetBuildModeCount: integer; virtual; abstract;
    function GetBuildModes(Index: integer): TPGBuildMode; virtual; abstract;
    function GetFileCount: integer; virtual; abstract;
    function GetFiles(Index: integer): string; virtual; abstract;
    function GetRequiredPackageCount: integer; virtual; abstract;
    function GetRequiredPackages(Index: integer): TPGDependency; virtual; abstract;
    function Perform(AAction: TPGTargetAction): TPGActionResult;
    function PerformAction(AAction: TPGTargetAction): TPGActionResult; virtual; abstract;
    procedure SetFilename(const AValue: string); virtual;
    procedure SetMissing(const AValue: boolean); virtual;
    procedure SetTargetType(AValue: TPGTargetType); virtual;
    procedure DoDeactivateChildren;
    procedure ActiveChanged(Sender: TPGCompileTarget); virtual; abstract;
    procedure DoActivate(DeactivateChildren: boolean);
    procedure DoDeActivate(DeactivateParents: boolean);
  public
    constructor Create(aParent: TPGCompileTarget);
    procedure Activate;
    procedure DeActivate;
    function GetOwnerProjectGroup: TProjectGroup;
    function GetRootProjectGroup: TProjectGroup;
    function GetNext(SkipChildren: boolean): TPGCompileTarget;
    function IndexOfBuildMode(aName: string): integer;
    function FindBuildMode(aName: string): TPGBuildMode;
    function PerformBuildModeAction(AAction: TPGTargetAction; aModeIdentifier: string): TPGActionResult; virtual; abstract;
    procedure Modified; virtual; abstract;
    property Parent: TPGCompileTarget read FParent;
    property Filename: string read FFilename write SetFilename; // Absolute, not relative.
    property Missing: boolean read FMissing write SetMissing;
    property TargetType: TPGTargetType read FTargetType write SetTargetType;
    property Active: Boolean Read FActive;
    function GetIndex: Integer;
    // Currently allowed actions.
    property AllowedActions: TPGTargetActions Read GetAllowedActions;
    //
    property ProjectGroup: TProjectGroup read FProjectGroup; // only set if TargetType=ttProjectGroup
    property BuildModes[Index: integer]: TPGBuildMode read GetBuildModes;
    property BuildModeCount: integer read GetBuildModeCount;
    property Files[Index: integer]: string read GetFiles;
    property FileCount: integer read GetFileCount;
    property RequiredPackages[Index: integer]: TPGDependency read GetRequiredPackages;
    property RequiredPackageCount: integer read GetRequiredPackageCount;
  end;

  TProjectGroupHandler = (
    pghDestroy
    );

  { TProjectGroup }

  TProjectGroup = class(TPersistent)
  private
    FHandlers: array[TProjectGroupHandler] of TMethodList;
    FChangeStamp: int64;
    FFileName: String;
    FLastSavedChangeStamp: int64;
    procedure SetModified(AValue: Boolean);
  protected
    FCompileTarget: TPGCompileTarget;
    FParent: TProjectGroup;
    procedure SetFileName(AValue: String); virtual;
    function GetModified: Boolean; virtual;
    function GetTargetCount: Integer; virtual; abstract;
    function GetTarget(Index: Integer): TPGCompileTarget; virtual; abstract;
    procedure DoCallNotifyHandler(HandlerType: TProjectGroupHandler;
                                  Sender: TObject); overload;
    procedure AddHandler(HandlerType: TProjectGroupHandler;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TProjectGroupHandler;
                            const AMethod: TMethod);
    function GetActiveTarget: TPGCompileTarget; virtual; abstract;
    procedure SetActiveTarget(AValue: TPGCompileTarget); virtual; abstract;
  public
    destructor Destroy; override;
    function GetRootGroup: TProjectGroup;
    property FileName: String Read FFileName Write SetFileName; // absolute
    property CompileTarget: TPGCompileTarget read FCompileTarget; // this group as target
    property Parent: TProjectGroup read FParent;
    // actions
    function Perform(Index: Integer; AAction: TPGTargetAction): TPGActionResult;
    function Perform(Const AFileName: String; AAction: TPGTargetAction): TPGActionResult;
    function Perform(Target: TPGCompileTarget; AAction: TPGTargetAction): TPGActionResult; virtual;
    function ActionAllowsFrom(Index: Integer; AAction: TPGTargetAction): Boolean; virtual;
    function PerformFrom(AIndex: Integer; AAction: TPGTargetAction): TPGActionResult; virtual;
    // targets
    function IndexOfTarget(Const Target: TPGCompileTarget): Integer; overload; virtual; abstract;
    function IndexOfTarget(Const AFilename: String): Integer; overload; virtual;
    function AddTarget(Const AFileName: String): TPGCompileTarget; virtual; abstract;
    function InsertTarget(Const Target: TPGCompileTarget; Index: Integer): Integer; virtual; abstract;
    procedure ExchangeTargets(ASource, ATarget: Integer); virtual; abstract;
    procedure RemoveTarget(Index: Integer); virtual; abstract;
    procedure RemoveTarget(Const AFileName: String);
    procedure RemoveTarget(Target: TPGCompileTarget);
    property Targets[Index: Integer]: TPGCompileTarget Read GetTarget;
    property TargetCount: Integer Read GetTargetCount;
    property ActiveTarget: TPGCompileTarget Read GetActiveTarget Write SetActiveTarget;
  public
    // modified
    procedure IncreaseChangeStamp;
    property Modified: Boolean Read GetModified write SetModified;
    property ChangeStamp: int64 read FChangeStamp;
  public
    // handlers
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnDestroy(const OnDestroy: TNotifyEvent; AsLast: boolean = false);
    procedure RemoveHandlerOnDestroy(const OnDestroy: TNotifyEvent);
  end;

  TProjectGroupLoadOption  = (
    pgloRemoveInvalid, // Remove non-existing targets from group automatically while loading
    pgloSkipInvalid, // Mark non-existing as Missing.
    pgloErrorInvalid, // Stop with error on non-existing.
    pgloSkipDialog, // do not show Project Group editor.
    pgloLoadRecursively // load all sub nodes
  );
  TProjectGroupLoadOptions = set of TProjectGroupLoadOption;

  { TProjectGroupManager }

  TProjectGroupManager = Class(TPersistent)
  protected
    function GetCurrentProjectGroup: TProjectGroup; virtual; abstract;
  public
    procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); virtual; abstract;
    procedure SaveProjectGroup; virtual; abstract;
    function CanUndo: boolean; virtual; abstract;
    function CanRedo: boolean; virtual; abstract;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual; abstract;
    property CurrentProjectGroup: TProjectGroup Read GetCurrentProjectGroup; // Always top-level.
  end;

var
  ProjectGroupManager: TProjectGroupManager = nil;

const
  PGTargetActions: array[TPGTargetType] of TPGTargetActions = (
    [], // ttUnknown
    [taOpen,taSettings,taCompile,taCompileClean,taCompileFromHere,taRun], // ttProject
    [taOpen,taSettings,taCompile,taCompileClean,taCompileFromHere,taInstall,taUninstall], // ttPackage
    [taOpen,taCompile,taCompileClean,taCompileFromHere], // ttProjectGroup
    [taOpen,taSettings,taCompile,taCompileFromHere,taRun], // ttPascalFile
    [taOpen,taCompile,taCompileClean,taCompileFromHere,taRun] // ttExternalTool
  );

function TargetTypeFromExtenstion(AExt: String): TPGTargetType;
function TargetSupportsAction(ATarget: TPGTargetType; AAction: TPGTargetAction): Boolean;
function ActionAllowsMulti(AAction: TPGTargetAction): Boolean;

implementation

function TargetTypeFromExtenstion (AExt: String): TPGTargetType;
begin
  while (AExt<>'') and (AExt[1]='.') do
    Delete(AExt,1,1);
  case LowerCase(AExt) of
    'lpi',
    'lpr': Result:=ttProject;
    'lpk': Result:=ttPackage;
    'lpg': Result:=ttProjectGroup;
    'pas',
    'pp',
    'p'  : Result:=ttPascalFile;
  else
    Result:=ttUnknown;
  end;
end;

function TargetSupportsAction(ATarget: TPGTargetType; AAction: TPGTargetAction
  ): Boolean;
begin
  Result:=AAction in PGTargetActions[ATarget];
end;

function ActionAllowsMulti(AAction: TPGTargetAction): Boolean;
begin
  Result:=AAction in [taCompile,taCompileClean];
end;

{ TPGBuildMode }

procedure TPGBuildMode.SetCompile(AValue: boolean);
begin
  if FCompile=AValue then Exit;
  FCompile:=AValue;
  Target.Modified;
end;

constructor TPGBuildMode.Create(aTarget: TPGCompileTarget; const anIdentifier: string;
  aCompile: boolean);
begin
  FTarget:=aTarget;
  FIdentifier:=anIdentifier;
  FCompile:=aCompile;
end;

{ TPGDependency }

constructor TPGDependency.Create(aTarget: TPGCompileTarget;
  const aPkgName: string);
begin
  FTarget:=aTarget;
  FPackageName:=aPkgName;
end;

{ TProjectGroup }

procedure TProjectGroup.SetModified(AValue: Boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FLastSavedChangeStamp:=FChangeStamp;
end;

procedure TProjectGroup.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  IncreaseChangeStamp;
  if CompileTarget<>nil then
    CompileTarget.Filename:=Filename;
end;

function TProjectGroup.GetModified: Boolean;
begin
  Result:=FLastSavedChangeStamp<>FChangeStamp;
end;

procedure TProjectGroup.DoCallNotifyHandler(HandlerType: TProjectGroupHandler;
  Sender: TObject);
begin
  FHandlers[HandlerType].CallNotifyEvents(Sender);
end;

procedure TProjectGroup.AddHandler(HandlerType: TProjectGroupHandler;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod,AsLast);
end;

procedure TProjectGroup.RemoveHandler(HandlerType: TProjectGroupHandler;
  const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

destructor TProjectGroup.Destroy;
var
  HandlerType: TProjectGroupHandler;
begin
  DoCallNotifyHandler(pghDestroy,Self);
  for HandlerType:=Low(FHandlers) to High(FHandlers) do
    FreeAndNil(FHandlers[HandlerType]);
  inherited Destroy;
end;

function TProjectGroup.GetRootGroup: TProjectGroup;
begin
  Result:=Self;
  while Result.Parent<>nil do
    Result:=Result.Parent;
end;

function TProjectGroup.Perform(Index: Integer; AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=Perform(GetTarget(Index),AAction);
end;

function TProjectGroup.Perform(const AFileName: String; AAction: TPGTargetAction
  ): TPGActionResult;
begin
  Result:=Perform(IndexOfTarget(AFileName),AAction);
end;

function TProjectGroup.Perform(Target: TPGCompileTarget; AAction: TPGTargetAction): TPGActionResult;
begin
  Result:=Target.Perform(AAction);
end;

function TProjectGroup.ActionAllowsFrom(Index: Integer; AAction: TPGTargetAction
  ): Boolean;
Var
  C: Integer;
  T: TPGCompileTarget;
begin
  Result:=ActionAllowsMulti(AAction);
  C:=TargetCount;
  while Result and (Index<C)  do
  begin
    T:=GetTarget(Index);
    if not T.Missing then
      Result:=Result and (AAction in T.AllowedActions);
    Inc(Index);
  end;
end;

function TProjectGroup.PerformFrom(AIndex: Integer; AAction: TPGTargetAction
  ): TPGActionResult;
Var
  I: Integer;
begin
  Result:=arOK;
  I:=AIndex;
  while (Result=arOK) and (I<TargetCount) do
  begin
    Result:=Perform(I,AAction);
    Inc(I);
  end;
end;

function TProjectGroup.IndexOfTarget(const AFilename: String): Integer;
begin
  Result:=TargetCount-1;
  while (Result>=0) and (CompareFilenames(AFileName,GetTarget(Result).Filename)<>0) do
    Dec(Result);
end;

procedure TProjectGroup.RemoveTarget(const AFileName: String);
begin
  RemoveTarget(IndexOfTarget(AFileName))
end;

procedure TProjectGroup.RemoveTarget(Target: TPGCompileTarget);
begin
  RemoveTarget(IndexOfTarget(Target))
end;

procedure TProjectGroup.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp64(FChangeStamp);
  if Parent<>nil then
    Parent.IncreaseChangeStamp;
end;

procedure TProjectGroup.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TProjectGroupHandler;
begin
  for HandlerType in TProjectGroupHandler do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TProjectGroup.AddHandlerOnDestroy(const OnDestroy: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(pghDestroy,TMethod(OnDestroy),AsLast);
end;

procedure TProjectGroup.RemoveHandlerOnDestroy(const OnDestroy: TNotifyEvent);
begin
  RemoveHandler(pghDestroy,TMethod(OnDestroy));
end;

{ TPGCompileTarget }

function TPGCompileTarget.GetIndex: Integer;
var
  Group: TProjectGroup;
begin
  if Parent=nil then exit(0);
  Group:=Parent.ProjectGroup;
  if Group=nil then exit(0);
  Result:=Group.IndexOfTarget(Self);
end;

function TPGCompileTarget.GetAllowedActions: TPGTargetActions;
begin
  Result:=PGTargetActions[TargetType];
end;

procedure TPGCompileTarget.SetTargetType(AValue: TPGTargetType);
begin
  if FTargetType=AValue then Exit;
  FTargetType:=AValue;
end;

procedure TPGCompileTarget.DoDeactivateChildren;
var
  i: Integer;
begin
  if ProjectGroup=nil then exit;
  for i:=0 to ProjectGroup.TargetCount-1 do
    ProjectGroup.Targets[i].DoDeActivate(false);
end;

procedure TPGCompileTarget.DoActivate(DeactivateChildren: boolean);
var
  OldActive: TPGCompileTarget;
  PG: TProjectGroup;
begin
  if DeactivateChildren then
    DoDeactivateChildren;
  if Active then exit;
  if Parent<>nil then
  begin
    PG:=Parent.ProjectGroup;
    if PG<>nil then
    begin
      OldActive:=PG.ActiveTarget;
      if OldActive<>nil then
        OldActive.DoDeActivate(false);
    end;
    Parent.DoActivate(false);
    PG.IncreaseChangeStamp;
  end;
  FActive:=True;
end;

procedure TPGCompileTarget.DoDeActivate(DeactivateParents: boolean);
begin
  if not Active then exit;
  if ProjectGroup<>nil then
  begin
    ProjectGroup.IncreaseChangeStamp;
    DoDeactivateChildren;
  end;
  FActive:=False;
  if DeactivateParents and (Parent<>nil) then
    Parent.DoDeActivate(true);
end;

constructor TPGCompileTarget.Create(aParent: TPGCompileTarget);
begin
  FParent:=aParent;
end;

procedure TPGCompileTarget.SetFilename(const AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  TargetType:=TargetTypeFromExtenstion(ExtractFileExt(AValue));
  if ProjectGroup<>nil then
    ProjectGroup.FileName:=Filename;
end;

procedure TPGCompileTarget.SetMissing(const AValue: boolean);
begin
  if Missing=AValue then exit;
  FMissing:=AValue;
end;

procedure TPGCompileTarget.Activate;
begin
  if Active then exit;
  DoActivate(true);
  ActiveChanged(Self);
end;

procedure TPGCompileTarget.DeActivate;
begin
  if not Active then exit;
  DoDeActivate(true);
  ActiveChanged(Self);
end;

function TPGCompileTarget.GetOwnerProjectGroup: TProjectGroup;
var
  aTarget: TPGCompileTarget;
begin
  aTarget:=Self;
  while (aTarget<>nil) do begin
    Result:=aTarget.ProjectGroup;
    if Result<>nil then exit;
    aTarget:=aTarget.Parent;
  end;
  Result:=nil;
end;

function TPGCompileTarget.GetRootProjectGroup: TProjectGroup;
var
  aTarget: TPGCompileTarget;
begin
  aTarget:=Self;
  while (aTarget.Parent<>nil) do aTarget:=aTarget.Parent;
  Result:=aTarget.ProjectGroup;
end;

function TPGCompileTarget.GetNext(SkipChildren: boolean): TPGCompileTarget;
var
  aTarget: TPGCompileTarget;
  PG: TProjectGroup;
  i: Integer;
begin
  // check first child
  if (not SkipChildren) and (ProjectGroup<>nil) and (ProjectGroup.TargetCount>0)
  then begin
    Result:=ProjectGroup.Targets[0];
    exit(Result);
  end;
  // check next sibling
  aTarget:=Self;
  while aTarget.Parent<>nil do begin
    PG:=aTarget.Parent.ProjectGroup;
    if PG<>nil then begin
      i:=PG.IndexOfTarget(aTarget);
      if (i>=0) and (i+1<PG.TargetCount) then begin
        Result:=PG.Targets[i+1];
        exit(Result);
      end;
    end;
    aTarget:=aTarget.Parent;
  end;
  Result:=nil;
end;

function TPGCompileTarget.IndexOfBuildMode(aName: string): integer;
begin
  Result:=BuildModeCount-1;
  while (Result>=0) and (CompareText(aName,BuildModes[Result].Identifier)<>0) do
    dec(Result);
end;

function TPGCompileTarget.FindBuildMode(aName: string): TPGBuildMode;
var
  i: Integer;
begin
  i:=IndexOfBuildMode(aName);
  if i>=0 then
    Result:=BuildModes[i]
  else
    Result:=nil;
end;

function TPGCompileTarget.Perform(AAction: TPGTargetAction): TPGActionResult;
begin
  if Not (AAction in AllowedActions) then
    Result:=arNotAllowed
  else
    Result:=PerformAction(AAction);
end;

end.

