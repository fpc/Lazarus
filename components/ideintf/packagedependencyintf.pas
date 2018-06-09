unit PackageDependencyIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils;

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
  private
    FMajor: integer;
    FMinor: integer;
    FRelease: integer;
    FBuild: integer;
    FValid: TPkgVersionValid;
    FOnChange: TNotifyEvent;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
  public
    procedure Clear;
    function Compare(Version2: TPkgVersion): integer;
    function CompareMask(ExactVersion: TPkgVersion): integer;
    procedure Assign(Source: TPkgVersion);
    //function AsString: string;
    function AsWord: string;
    function GetIsNullVersion: Boolean;
    function ReadString(const s: string): boolean;
    procedure SetValues(NewMajor, NewMinor, NewRelease, NewBuild: integer;
                        NewValid: TPkgVersionValid = pvtBuild);
    function VersionBound(v: integer): integer;
  public
    property AsString: String read GetAsString write SetAsString;
    property Major: Integer read FMajor write FMajor;
    property Minor: Integer read FMinor write FMinor;
    property Release: Integer read FRelease write FRelease;
    property Build: Integer read FBuild write FBuild;
    property IsNullVersion: Boolean read GetIsNullVersion;
    property Valid: TPkgVersionValid read FValid write FValid;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    function IsMakingSense: boolean;
    function IsCompatible(const Version: TPkgVersion): boolean; overload;
    function IsCompatible(const PkgName: string; const Version: TPkgVersion): boolean; overload;
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

function LazIsValidIdent(const Ident: string; AllowDots: Boolean = False;
                         StrictDots: Boolean = False): Boolean;
function IsValidUnitName(AUnitName: String): Boolean; inline;
function IsValidPkgName(APkgName: String): Boolean; inline;
function PackageFileNameIsValid(const AFilename: string): boolean;


implementation

function LazIsValidIdent(const Ident: string; AllowDots: Boolean = False;
                         StrictDots: Boolean = False): Boolean;
// This is a copy of IsValidIdent from FPC 3.1.
// ToDo: Switch to using IsValidIdent from FPC 3.2 when it is the minimum requirement.
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  for I := 1 to Len do
  begin
    if First then
    begin
      Result := Ident[I] in Alpha;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        Result := I < Len;
        First := True;
      end;
    end
    else
      Result := Ident[I] in AlphaNum;
    if not Result then
      Break;
  end;
end;

function IsValidUnitName(AUnitName: String): Boolean;
begin
  Result := LazIsValidIdent(AUnitName, True, True);
end;

function IsValidPkgName(APkgName: String): Boolean;
begin
  Result := LazIsValidIdent(APkgName, True, True);
end;

function PackageFileNameIsValid(const AFilename: string): boolean;
var
  PkgName: String;
begin
  Result:=false;
  if CompareFileExt(AFilename,'.lpk',false)<>0 then exit;
  PkgName:=ExtractFileNameOnly(AFilename);
  if (PkgName='') or (not IsValidPkgName(PkgName)) then exit;
  Result:=true;
end;

{ TPkgVersion }

procedure TPkgVersion.Clear;
begin
  SetValues(0,0,0,0,pvtBuild);
end;

function TPkgVersion.Compare(Version2: TPkgVersion): integer;
begin
  Result:=FMajor-Version2.FMajor;
  if Result<>0 then exit;
  Result:=FMinor-Version2.FMinor;
  if Result<>0 then exit;
  Result:=FRelease-Version2.FRelease;
  if Result<>0 then exit;
  Result:=FBuild-Version2.FBuild;
end;

function TPkgVersion.CompareMask(ExactVersion: TPkgVersion): integer;
begin
  if FValid=pvtNone then exit(0);
  Result:=FMajor-ExactVersion.FMajor;
  if Result<>0 then exit;
  if FValid=pvtMajor then exit;
  Result:=FMinor-ExactVersion.FMinor;
  if Result<>0 then exit;
  if FValid=pvtMinor then exit;
  Result:=FRelease-ExactVersion.FRelease;
  if Result<>0 then exit;
  if FValid=pvtRelease then exit;
  Result:=FBuild-ExactVersion.FBuild;
end;

procedure TPkgVersion.Assign(Source: TPkgVersion);
begin
  SetValues(Source.FMajor,Source.FMinor,Source.FRelease,Source.FBuild,Source.FValid);
end;

function TPkgVersion.GetAsString: String;
begin
  Result:=IntToStr(FMajor)+'.'+IntToStr(FMinor);
  if (FBuild<>0) then
    Result:=Result+'.'+IntToStr(FRelease)+'.'+IntToStr(FBuild)
  else if (FRelease<>0) then
    Result:=Result+'.'+IntToStr(FRelease)
end;

procedure TPkgVersion.SetAsString(const AValue: String);
var
  Version: String;
  P, I, V: Integer;
begin
  Clear;
  if AValue = '' then Exit;
  I := 0;
  Version := Trim(AValue) + '.';
  repeat
     Inc(I);
     P := Pos('.', Version);
     if P <> 0 then
     begin
       V := StrToIntDef(Copy(Version, 1, P-1), 0);
       case I of
         1: FMajor   := V;
         2: FMinor   := V;
         3: FRelease := V;
         4: FBuild   := V;
       end;
       Delete(Version, 1, P);
     end;
  until (Version = '') or (P = 0) or (I > 4);
end;

function TPkgVersion.AsWord: string;
begin
  Result:=IntToStr(FMajor)+'_'+IntToStr(FMinor);
  if (FBuild<>0) then
    Result:=Result+'_'+IntToStr(FRelease)+'_'+IntToStr(FBuild)
  else if (FRelease<>0) then
    Result:=Result+'_'+IntToStr(FRelease)
end;

function TPkgVersion.GetIsNullVersion: Boolean;
begin
  Result := (FMajor = 0) and (FMinor = 0) and (FRelease = 0) and (FBuild = 0);
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
  if (NewMajor=FMajor) and (NewMinor=FMinor) and (NewRelease=FRelease)
  and (NewBuild=FBuild) and (NewValid=FValid) then exit;
  FMajor:=NewMajor;
  FMinor:=NewMinor;
  FRelease:=NewRelease;
  FBuild:=NewBuild;
  FValid:=NewValid;
  if Assigned(FOnChange) then FOnChange(Self);
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

function TPkgDependencyBase.IsMakingSense: boolean;
begin
  Result:=IsValidPkgName(PackageName);
  if Result
  and (pdfMinVersion in FFlags) and (pdfMaxVersion in FFlags)
  and (MinVersion.Compare(MaxVersion)>0) then
    Result:=false;
end;

function TPkgDependencyBase.IsCompatible(const Version: TPkgVersion): boolean;
begin
  if ((pdfMinVersion in FFlags) and (MinVersion.Compare(Version)>0))
  or ((pdfMaxVersion in FFlags) and (MaxVersion.Compare(Version)<0)) then
    Result:=false
  else
    Result:=true;
end;

function TPkgDependencyBase.IsCompatible(const PkgName: string; const Version: TPkgVersion): boolean;
begin
  Result:=(CompareText(PkgName,PackageName)=0) and IsCompatible(Version);
end;

end.

