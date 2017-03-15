unit PackageDependencyIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPkgVersion }

  TPkgVersionValid = (
    pvtNone,
    pvtMajor,
    pvtMinor,
    pvtRelease,
    pvtBuild
    );

  TPkgVersion = class
  public
    Major: integer;
    Minor: integer;
    Release: integer;
    Build: integer;
    Valid: TPkgVersionValid;
    OnChange: TNotifyEvent;
    procedure Clear;
    function Compare(Version2: TPkgVersion): integer;
    function CompareMask(ExactVersion: TPkgVersion): integer;
    procedure Assign(Source: TPkgVersion);
    function AsString: string;
    function AsWord: string;
    function ReadString(const s: string): boolean;
    procedure SetValues(NewMajor, NewMinor, NewRelease, NewBuild: integer;
                        NewValid: TPkgVersionValid = pvtBuild);
    function VersionBound(v: integer): integer;
  end;

  { PkgDependency flags }

  TPkgDependencyFlag = (
    pdfMinVersion, // >= MinVersion
    pdfMaxVersion // <= MaxVersion
    );
  TPkgDependencyFlags = set of TPkgDependencyFlag;

  { TPkgDependencyBase }

  TPkgDependencyBase = class
  private
  protected
    FFlags: TPkgDependencyFlags;
    FMaxVersion: TPkgVersion;
    FMinVersion: TPkgVersion;
    FPackageName: string;
    FRemoved: boolean;
    //FRequiredPackage: TIDEPackage;
    procedure SetPackageName(const AValue: string); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    // API for iterating dependencies.
    function NextUsedByDependency: TPkgDependencyBase; virtual; abstract;
    function PrevUsedByDependency: TPkgDependencyBase; virtual; abstract;
    function NextRequiresDependency: TPkgDependencyBase; virtual; abstract;
    function PrevRequiresDependency: TPkgDependencyBase; virtual; abstract;
    // API for adding / removing dependencies.
    procedure AddRequiresDep(var FirstDependency: TPkgDependencyBase); virtual; abstract;
    procedure AddUsedByDep(var FirstDependency: TPkgDependencyBase); virtual; abstract;
    procedure RemoveRequiresDep(var FirstDependency: TPkgDependencyBase); virtual; abstract;
    procedure RemoveUsedByDep(var FirstDependency: TPkgDependencyBase); virtual; abstract;
  public
    property Flags: TPkgDependencyFlags read FFlags write FFlags;
    property MinVersion: TPkgVersion read FMinVersion write FMinVersion;
    property MaxVersion: TPkgVersion read FMaxVersion write FMaxVersion;
    property PackageName: string read FPackageName write SetPackageName;
    property Removed: boolean read FRemoved write FRemoved;
  end;

implementation

{ TPkgVersion }

procedure TPkgVersion.Clear;
begin
  SetValues(0,0,0,0,pvtBuild);
end;

function TPkgVersion.Compare(Version2: TPkgVersion): integer;
begin
  Result:=Major-Version2.Major;
  if Result<>0 then exit;
  Result:=Minor-Version2.Minor;
  if Result<>0 then exit;
  Result:=Release-Version2.Release;
  if Result<>0 then exit;
  Result:=Build-Version2.Build;
end;

function TPkgVersion.CompareMask(ExactVersion: TPkgVersion): integer;
begin
  if Valid=pvtNone then exit(0);
  Result:=Major-ExactVersion.Major;
  if Result<>0 then exit;
  if Valid=pvtMajor then exit;
  Result:=Minor-ExactVersion.Minor;
  if Result<>0 then exit;
  if Valid=pvtMinor then exit;
  Result:=Release-ExactVersion.Release;
  if Result<>0 then exit;
  if Valid=pvtRelease then exit;
  Result:=Build-ExactVersion.Build;
end;

procedure TPkgVersion.Assign(Source: TPkgVersion);
begin
  SetValues(Source.Major,Source.Minor,Source.Release,Source.Build,Source.Valid);
end;

function TPkgVersion.AsString: string;
begin
  Result:=IntToStr(Major)+'.'+IntToStr(Minor);
  if (Build<>0) then
    Result:=Result+'.'+IntToStr(Release)+'.'+IntToStr(Build)
  else if (Release<>0) then
    Result:=Result+'.'+IntToStr(Release)
end;

function TPkgVersion.AsWord: string;
begin
  Result:=IntToStr(Major)+'_'+IntToStr(Minor);
  if (Build<>0) then
    Result:=Result+'_'+IntToStr(Release)+'_'+IntToStr(Build)
  else if (Release<>0) then
    Result:=Result+'_'+IntToStr(Release)
end;

function TPkgVersion.ReadString(const s: string): boolean;
var
  ints: array[1..4] of integer;
  i: integer;
  CurPos: Integer;
  StartPos: Integer;
  NewValid: TPkgVersionValid;
begin
  Result:=false;
  CurPos:=1;
  NewValid:=pvtNone;
  for i:=1 to 4 do begin
    ints[i]:=0;
    if CurPos<length(s) then begin
      if i>Low(ints) then begin
        // read point
        if s[CurPos]<>'.' then exit;
        inc(CurPos);
      end;
      // read int
      StartPos:=CurPos;
      while (CurPos<=length(s)) and (i<=9999)
      and (s[CurPos] in ['0'..'9']) do begin
        ints[i]:=ints[i]*10+ord(s[CurPos])-ord('0');
        inc(CurPos);
      end;
      if (StartPos=CurPos) then exit;
      NewValid:=succ(NewValid);
    end;
  end;
  if CurPos<=length(s) then exit;
  SetValues(ints[1],ints[2],ints[3],ints[4],NewValid);

  Result:=true;
end;

procedure TPkgVersion.SetValues(NewMajor, NewMinor, NewRelease,
  NewBuild: integer; NewValid: TPkgVersionValid);
begin
  NewMajor:=VersionBound(NewMajor);
  NewMinor:=VersionBound(NewMinor);
  NewRelease:=VersionBound(NewRelease);
  NewBuild:=VersionBound(NewBuild);
  if (NewMajor=Major) and (NewMinor=Minor) and (NewRelease=Release)
  and (NewBuild=Build) and (NewValid=Valid) then exit;
  Major:=NewMajor;
  Minor:=NewMinor;
  Release:=NewRelease;
  Build:=NewBuild;
  Valid:=NewValid;
  if Assigned(OnChange) then OnChange(Self);
end;

function TPkgVersion.VersionBound(v: integer): integer;
begin
  if v>9999 then
    Result:=9999
  else if v<0 then
    Result:=0
  else
    Result:=v;
end;

{ TLazPackageID }

constructor TPkgDependencyBase.Create;
begin
  inherited Create;
  MinVersion:=TPkgVersion.Create;
  MaxVersion:=TPkgVersion.Create;
  Clear;
end;

destructor TPkgDependencyBase.Destroy;
begin
  PackageName:='';
  FreeAndNil(fMinVersion);
  FreeAndNil(fMaxVersion);
  inherited Destroy;
end;

procedure TPkgDependencyBase.Clear;
begin
  PackageName:='';
  FRemoved:=false;
  FFlags:=[];
  FMaxVersion.Clear;
  FMinVersion.Clear;
end;

end.

