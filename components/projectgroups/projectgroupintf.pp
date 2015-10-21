{ If you want to extend the package only access this unit.
}
unit ProjectGroupIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, PackageIntf, ProjectIntf, LazFileUtils;

Type
  TTargetType = (ttUnknown, ttProject, ttPackage, ttProjectGroup);
  TTargetTypes = set of TTargetType;
  TTargetAction = (taOpen,taSettings,taCompile,taCompileClean,taRun,taInstall,taUninstall);
  TTargetActions = set of TTargetAction;
  TActionResult = (arNotAllowed,arOK,arFailed);
  TActionResults = set of TActionResult;

  TProjectGroup = class;

  { TCompileTarget - can be a project, package or project group}

  TCompileTarget = class
  private
    FActive: Boolean;
    FFilename: string;
    FTargetType: TTargetType;
    FRemoved: boolean;
  protected
    Function PerformAction (AAction : TTargetAction) : TActionResult; virtual; abstract;
    Function Perform (AAction : TTargetAction) : TActionResult;
    // By default, return all allowed actions for target type.
    function GetAllowedActions: TTargetActions; virtual;
    function GetLazProject: TLazProject; virtual; abstract;
    function GetProjectGroup: TProjectGroup; virtual; abstract;
    procedure SetTargetType(AValue: TTargetType); virtual;
    procedure SetFilename(const AValue: string); virtual;
    function GetIDEPackage: TIDEPackage; virtual; abstract;
    procedure SetRemoved(const AValue: boolean); virtual;
    Procedure Activate; virtual;
    Procedure DeActivate; virtual;
  public
    // Fully qualified FileName. Not relative. (ToDo: store them relative)
    property Filename: string read FFilename write SetFilename;
    property Removed: boolean read FRemoved write SetRemoved;
    property TargetType: TTargetType read FTargetType write SetTargetType;
    property Active : Boolean Read FActive;
    // Currently allowed actions.
    property AllowedActions : TTargetActions Read GetAllowedActions;
    //
    property LazPackage: TIDEPackage read GetIDEPackage;
    property LazProject : TLazProject Read GetLazProject;
    property ProjectGroup : TProjectGroup Read GetProjectGroup;
  end;

  { TProjectGroup }

  TProjectGroup = Class(TPersistent)
  private
    FFileName: String;
    function GetActiveTarget: TCompileTarget;
    procedure SetActiveTarget(AValue: TCompileTarget);
  Protected
    procedure SetFileName(AValue: String); virtual;
    function GetTargetCount: Integer; virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    function GetTarget(AIndex : Integer): TCompileTarget; virtual; abstract;
  Public
    Function Perform(AIndex : Integer;AAction : TTargetAction) : TActionResult;
    Function Perform(Const AFileName : String;AAction : TTargetAction) : TActionResult;
    Function Perform(ATarget : TCompileTarget; AAction : TTargetAction) : TActionResult; virtual;
    Function ActionAllowsFrom(AIndex : Integer;AAction : TTargetAction) : Boolean; virtual;
    Function PerformFrom (AIndex : Integer;AAction : TTargetAction) : TActionResult; virtual;
    Function IndexOfTarget(Const T : TCompileTarget) : Integer; virtual;
    Function IndexOfTarget(Const AFilename : String) : Integer; virtual;
    Function AddTarget(Const AFileName : String) : TCompileTarget; virtual; abstract;
    Procedure ExchangeTargets(ASource,ATarget : Integer);  virtual; abstract;
    Procedure RemoveTarget(AIndex : Integer);
    Procedure RemoveTarget(Const AFileName : String) ;
    Procedure RemoveTarget(T : TCompileTarget) ; virtual;
    Procedure ActivateTarget(AIndex : Integer);
    Procedure ActivateTarget(Const AFileName : String);
    Procedure ActivateTarget(T : TCompileTarget); virtual;
    Property FileName : String Read FFileName Write SetFileName;
    Property Targets[AIndex : Integer] : TCompileTarget Read GetTarget;
    Property TargetCount : Integer Read GetTargetCount;
    Property ActiveTarget : TCompileTarget Read GetActiveTarget Write SetActiveTarget;
    Property Modified : Boolean Read GetModified;
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
  Public
    Procedure LoadProjectGroup(AFileName : string; AOptions : TProjectGroupLoadOptions); virtual; abstract;
    Procedure SaveProjectGroup; virtual; abstract;
    Property CurrentProjectGroup : TProjectGroup Read GetCurrentProjectGroup; // Always top-level.
  end;

Var
  ProjectGroupManager : TProjectGroupManager = nil;

Function TargetTypeFromExtenstion (AExt : String) : TTargetType;
Function TargetActions(ATarget : TTargetType) : TTargetActions;
Function TargetSupportsAction(ATarget : TTargetType; AAction : TTargetAction) : Boolean;
Function ActionAllowsMulti(AAction : TTargetAction) : Boolean;

implementation

Function TargetTypeFromExtenstion (AExt : String) : TTargetType;
begin
  While (AExt<>'') and (AExt[1]='.') do
    Delete(AExt,1,1);
  Case LowerCase(AExt) of
    'lpi',
    'lpr' : Result:=ttProject;
    'lpk' : Result:=ttPackage;
    'lpg' : Result:=ttProjectGroup;
  else
    Result:=ttUnknown;
  end;
end;

function TargetActions(ATarget: TTargetType): TTargetActions;
begin
  begin
  Case ATarget of
    ttUnknown      : Result:=[];
    ttProject      : Result:=[taOpen,taSettings,taCompile,taCompileClean,taRun];
    ttPackage      : Result:=[taOpen,taSettings,taCompile,taCompileClean,taInstall,taUninstall];
    ttProjectGroup : Result:=[taOpen,taCompile,taCompileClean];
  end;
end;

end;

function TargetSupportsAction(ATarget: TTargetType; AAction: TTargetAction
  ): Boolean;
begin
  Result:=AAction in TargetActions(ATarget);
end;

Function ActionAllowsMulti(AAction: TTargetAction): Boolean;
begin
  Result:=AAction in [taCompile,taCompileClean];
end;


{ TProjectGroup }

function TProjectGroup.GetActiveTarget: TCompileTarget;
Var
  I : Integer;
begin
  I:=0;
  for i:=0 to TargetCount-1 do
  begin
    Result:=GetTarget(I);
    if Result.Active then exit;
  end;
  Result:=Nil;
end;

procedure TProjectGroup.SetActiveTarget(AValue: TCompileTarget);
begin
  ActivateTarget(AValue);
end;

procedure TProjectGroup.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

Function TProjectGroup.Perform(AIndex: Integer; AAction: TTargetAction
  ): TActionResult;
begin
  Result:=Perform(GetTarget(AIndex),AAction);
end;

Function TProjectGroup.Perform(Const AFileName: String; AAction: TTargetAction
  ): TActionResult;
begin
  Result:=Perform(IndexOfTarget(AFileName),AAction);
end;

Function TProjectGroup.Perform(ATarget: TCompileTarget; AAction : TTargetAction): TActionResult;
begin
  Result:=ATarget.Perform(AAction);
end;

Function TProjectGroup.ActionAllowsFrom(AIndex: Integer; AAction: TTargetAction
  ): Boolean;
Var
  C : Integer;
  T : TCompileTarget;
begin
  Result:=ActionAllowsMulti(AAction);
  C:=TargetCount;
  While Result and (AIndex<C)  do
    begin
    T:=GetTarget(AIndex);
    if not T.Removed then
      Result:=AAction in T.AllowedActions;;
    Inc(AIndex);
    end;
end;

Function TProjectGroup.PerformFrom(AIndex: Integer; AAction: TTargetAction
  ): TActionResult;
Var
  I : Integer;
begin
  Result:=arOK;
  I:=AIndex;
  While (Result=arOK) and (I<TargetCount) do
    if Not GetTarget(i).Removed then
      begin
      Result:=Perform(I,AAction);
      Inc(I);
      end;
end;

Function TProjectGroup.IndexOfTarget(Const T: TCompileTarget): Integer;
begin
  Result:=TargetCount-1;
  While (Result>=0) and (T<>GetTarget(Result)) do
    Dec(Result);
end;

Function TProjectGroup.IndexOfTarget(Const AFilename: String): Integer;
begin
  Result:=TargetCount-1;
  While (Result>=0) and (CompareFilenames(AFileName,GetTarget(Result).Filename)<>0) do
    Dec(Result);
end;

Procedure TProjectGroup.RemoveTarget(AIndex: Integer);
begin
  RemoveTarget(Targets[AIndex])
end;

Procedure TProjectGroup.RemoveTarget(Const AFileName: String);
begin
  RemoveTarget(IndexOfTarget(AFileName))
end;

Procedure TProjectGroup.RemoveTarget(T: TCompileTarget);
begin
  T.Removed:=True;
end;

Procedure TProjectGroup.ActivateTarget(AIndex: Integer);
begin
  ActivateTarget(GetTarget(AIndex));
end;

Procedure TProjectGroup.ActivateTarget(Const AFileName: String);
begin
  ActivateTarget(IndexOfTarget(AFileName));
end;

Procedure TProjectGroup.ActivateTarget(T: TCompileTarget);
Var
  I : Integer;
  TD : TCompileTarget;
begin
  if T.Active then exit;
  For I:=0 to TargetCount-1 do
    begin
    TD:=GetTarget(I);
    If TD.Active then
      TD.Deactivate;
    end;
  T.Activate;
end;

{ TCompileTarget }

function TCompileTarget.GetAllowedActions: TTargetActions;
begin
  Result:=TargetActions(TargetType)
end;

procedure TCompileTarget.SetTargetType(AValue: TTargetType);
begin
  if FTargetType=AValue then Exit;
  FTargetType:=AValue;
end;

procedure TCompileTarget.SetFilename(const AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  TargetType:=TargetTypeFromExtenstion(ExtractFileExt(AValue));
end;

procedure TCompileTarget.SetRemoved(const AValue: boolean);
begin
  if Removed=AValue then exit;
  FRemoved:=AValue;
  If FRemoved then
    Deactivate;
end;

procedure TCompileTarget.Activate;
begin
  FActive:=True;
end;

procedure TCompileTarget.DeActivate;
begin
  FActive:=False;
end;

Function TCompileTarget.Perform(AAction: TTargetAction) : TActionResult;
begin
  if Not (AAction in AllowedActions) then
    Result:=arNotAllowed
  else
    Result:=PerformAction(AAction);
end;

end.

