{ If you want to extend the package only access this unit.
}
unit ProjectGroupIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, PackageIntf, ProjectIntf, LazFileUtils,
  LazFileCache;

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
    taRun,
    taInstall,
    taUninstall);
  TPGTargetActions = set of TPGTargetAction;

  TPGActionResult = (arNotAllowed,arOK,arFailed);
  TPGActionResults = set of TPGActionResult;

  TProjectGroup = class;

  { TPGDependency }

  TPGDependency = class
  public
    PackageName: string;
    constructor Create(const aPkgName: string);
  end;

  { TPGCompileTarget - a node in the tree, see TPGTargetType }

  TPGCompileTarget = class
  private
    FActive: Boolean;
    FFilename: string;
    FTargetType: TPGTargetType;
    FRemoved: boolean;
  protected
    function GetAllowedActions: TPGTargetActions; virtual; // By default, return all allowed actions for target type.
    function GetFileCount: integer; virtual; abstract;
    function GetFiles(Index: integer): string; virtual; abstract;
    function GetProjectGroup: TProjectGroup; virtual; abstract;
    function GetRequiredPackageCount: integer; virtual; abstract;
    function GetRequiredPackages(Index: integer): TPGDependency; virtual; abstract;
    function Perform (AAction: TPGTargetAction): TPGActionResult;
    function PerformAction (AAction: TPGTargetAction): TPGActionResult; virtual; abstract;
    procedure Activate; virtual;
    procedure DeActivate; virtual;
    procedure SetFilename(const AValue: string); virtual;
    procedure SetRemoved(const AValue: boolean); virtual;
    procedure SetTargetType(AValue: TPGTargetType); virtual;
  public
    property Filename: string read FFilename write SetFilename; // Absolute, not relative. (ToDo: store them relative)
    property Removed: boolean read FRemoved write SetRemoved;
    property TargetType: TPGTargetType read FTargetType write SetTargetType;
    property Active: Boolean Read FActive;
    // Currently allowed actions.
    property AllowedActions: TPGTargetActions Read GetAllowedActions;
    //
    property ProjectGroup: TProjectGroup Read GetProjectGroup;
    property Files[Index: integer]: string read GetFiles;
    property FileCount: integer read GetFileCount;
    property RequiredPackages[Index: integer]: TPGDependency read GetRequiredPackages;
    property RequiredPackageCount: integer read GetRequiredPackageCount;
  end;

  { TProjectGroup }

  TProjectGroup = class(TPersistent)
  private
    FChangeStamp: int64;
    FFileName: String;
    FLastSavedChangeStamp: int64;
    function GetActiveTarget: TPGCompileTarget;
    procedure SetActiveTarget(AValue: TPGCompileTarget);
    procedure SetModified(AValue: Boolean);
  protected
    procedure SetFileName(AValue: String); virtual;
    function GetModified: Boolean; virtual;
    function GetTargetCount: Integer; virtual; abstract;
    function GetTarget(Index: Integer): TPGCompileTarget; virtual; abstract;
    function GetRemovedTargetCount: Integer; virtual; abstract;
    function GetRemovedTarget(Index: Integer): TPGCompileTarget; virtual; abstract;
  public
    function Perform(Index: Integer; AAction: TPGTargetAction): TPGActionResult;
    function Perform(Const AFileName: String; AAction: TPGTargetAction): TPGActionResult;
    function Perform(Target: TPGCompileTarget; AAction: TPGTargetAction): TPGActionResult; virtual;
    function ActionAllowsFrom(Index: Integer; AAction: TPGTargetAction): Boolean; virtual;
    function PerformFrom(AIndex: Integer; AAction: TPGTargetAction): TPGActionResult; virtual;
    function IndexOfTarget(Const Target: TPGCompileTarget): Integer; virtual; abstract;
    function IndexOfTarget(Const AFilename: String): Integer; virtual;
    function IndexOfRemovedTarget(Const Target: TPGCompileTarget): Integer; virtual; abstract;
    function IndexOfRemovedTarget(Const AFilename: String): Integer; virtual;
    function AddTarget(Const AFileName: String): TPGCompileTarget; virtual; abstract;
    procedure ExchangeTargets(ASource, ATarget: Integer); virtual; abstract;
    procedure RemoveTarget(Index: Integer); virtual; abstract;
    procedure RemoveTarget(Const AFileName: String);
    procedure RemoveTarget(Target: TPGCompileTarget);
    procedure ActivateTarget(Index: Integer);
    procedure ActivateTarget(Const AFileName: String);
    procedure ActivateTarget(Target: TPGCompileTarget); virtual;
    procedure IncreaseChangeStamp;
    property FileName: String Read FFileName Write SetFileName; // absolute
    property Targets[Index: Integer]: TPGCompileTarget Read GetTarget;
    property TargetCount: Integer Read GetTargetCount;
    property RemovedTargets[Index: Integer]: TPGCompileTarget Read GetRemovedTarget;
    property RemovedTargetCount: Integer Read GetRemovedTargetCount;
    property ActiveTarget: TPGCompileTarget Read GetActiveTarget Write SetActiveTarget;
    property Modified: Boolean Read GetModified write SetModified;
    property ChangeStamp: int64 read FChangeStamp;
  end;

  TProjectGroupLoadOption  = (
    pgloRemoveInvalid, // Mark non-existing targets from group as removed.
    pgloSkipInvalid, // Ignore non-existing, add as-is.
    pgloErrorInvalid, // Stop with error on non-existing.
    pgloSkipDialog // do not show Project Group editor.
  );
  TProjectGroupLoadOptions = set of TProjectGroupLoadOption;

  { TProjectGroupManager }

  TProjectGroupManager = Class(TPersistent)
  protected
    function GetCurrentProjectGroup: TProjectGroup; virtual; abstract;
  public
    procedure LoadProjectGroup(AFileName: string; AOptions: TProjectGroupLoadOptions); virtual; abstract;
    procedure SaveProjectGroup; virtual; abstract;
    property CurrentProjectGroup: TProjectGroup Read GetCurrentProjectGroup; // Always top-level.
  end;

var
  ProjectGroupManager: TProjectGroupManager = nil;

const
  PGTargetActions: array[TPGTargetType] of TPGTargetActions = (
    [], // ttUnknown
    [taOpen,taSettings,taCompile,taCompileClean,taRun], // ttProject
    [taOpen,taSettings,taCompile,taCompileClean,taInstall,taUninstall], // ttPackage
    [taOpen,taCompile,taCompileClean], // ttProjectGroup
    [taOpen,taCompile,taRun], // ttPascalFile
    [taOpen,taRun] // ttExternalTool
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

{ TPGDependency }

constructor TPGDependency.Create(const aPkgName: string);
begin
  PackageName:=aPkgName;
end;


{ TProjectGroup }

function TProjectGroup.GetActiveTarget: TPGCompileTarget;
Var
  I: Integer;
begin
  I:=0;
  for i:=0 to TargetCount-1 do
  begin
    Result:=GetTarget(I);
    if Result.Active then exit;
  end;
  Result:=Nil;
end;

procedure TProjectGroup.SetActiveTarget(AValue: TPGCompileTarget);
begin
  ActivateTarget(AValue);
  IncreaseChangeStamp;
end;

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
end;

function TProjectGroup.GetModified: Boolean;
begin
  Result:=FLastSavedChangeStamp<>FChangeStamp;
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
    if not T.Removed then
      Result:=AAction in T.AllowedActions;;
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
    if Not GetTarget(i).Removed then
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

function TProjectGroup.IndexOfRemovedTarget(const AFilename: String): Integer;
begin
  Result:=RemovedTargetCount-1;
  while (Result>=0) and (CompareFilenames(AFileName,GetRemovedTarget(Result).Filename)<>0) do
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

procedure TProjectGroup.ActivateTarget(Index: Integer);
begin
  ActivateTarget(GetTarget(Index));
end;

procedure TProjectGroup.ActivateTarget(const AFileName: String);
begin
  ActivateTarget(IndexOfTarget(AFileName));
end;

procedure TProjectGroup.ActivateTarget(Target: TPGCompileTarget);
var
  I: Integer;
  TD: TPGCompileTarget;
begin
  if Target.Active then exit;
  for I:=0 to TargetCount-1 do
  begin
    TD:=GetTarget(I);
    if TD.Active then
      TD.Deactivate;
  end;
  Target.Activate;
end;

procedure TProjectGroup.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp64(FChangeStamp);
end;

{ TPGCompileTarget }

function TPGCompileTarget.GetAllowedActions: TPGTargetActions;
begin
  Result:=PGTargetActions[TargetType];
end;

procedure TPGCompileTarget.SetTargetType(AValue: TPGTargetType);
begin
  if FTargetType=AValue then Exit;
  FTargetType:=AValue;
end;

procedure TPGCompileTarget.SetFilename(const AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  TargetType:=TargetTypeFromExtenstion(ExtractFileExt(AValue));
end;

procedure TPGCompileTarget.SetRemoved(const AValue: boolean);
begin
  if Removed=AValue then exit;
  FRemoved:=AValue;
  If FRemoved then
    Deactivate;
end;

procedure TPGCompileTarget.Activate;
begin
  FActive:=True;
end;

procedure TPGCompileTarget.DeActivate;
begin
  FActive:=False;
end;

function TPGCompileTarget.Perform(AAction: TPGTargetAction): TPGActionResult;
begin
  if Not (AAction in AllowedActions) then
    Result:=arNotAllowed
  else
    Result:=PerformAction(AAction);
end;

end.

