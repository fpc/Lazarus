{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Implementation of the serializable package class. Information about the
   repository packages are stored in a json file. After the JSON is downloaded
   it gets serialized to a package list.}

unit opkman_serializablepackages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Variants, fpjson, jsonparser, md5, contnrs,
  PackageIntf, Laz2_XMLCfg;


type
  TPackageType = (
    ptRunAndDesignTime,
    ptDesignTime,
    ptRunTime,
    ptRunTimeOnly);

const
  PackageTypeIdents: array[TPackageType] of string = (
    'RunAndDesignTime',
    'DesignTime',
    'RunTime',
    'RunTimeOnly');

type
  TPackageState = (
    psRepository,
    psDownloaded,
    psExtracted,
    psInstalled,
    psError);
  TPackageStates = set of TPackageState;

  TChangeType = (ctAdd, ctRemove);
  { TPackageVersion }

   TPackageVersion = class(TPersistent)
   private
     FMajor: Integer;
     FMinor: Integer;
     FRelease: Integer;
     FBuild: Integer;
     function GetAsString: String;
     procedure SetAsString(const AValue: String);
     function GetIsNullVersion: Boolean;
   public
     procedure SetDefaults;
     procedure Assign(ASource: TPersistent); override;
     function CompareVersion(AVersion: TPackageVersion): Integer;
     function SameVersion(AVersion: TPackageVersion): Boolean;
     property AsString: String read GetAsString write SetAsString;
   published
      property Major: Integer read FMajor write FMajor;
      property Minor: Integer read FMinor write FMinor;
      property Release: Integer read FRelease write FRelease;
      property Build: Integer read FBuild write FBuild;
      property IsNullVersion: Boolean read GetIsNullVersion;
   end;

   { TPackageDependency }

   TPackageDependency = class(TCollectionItem)
   private
     FMaxVersion: TPackageVersion;
     FMinVersion: TPackageVersion;
     FPackageFileName: String;
     procedure SetMinVersion(const AValue: TPackageVersion);
     procedure SetMaxVersion(const AValue: TPackageVersion);
   public
     procedure Assign(ASource: TPersistent); override;
     constructor Create(ACollection: TCollection); override;
     destructor Destroy; override;
   published
     property PackageFileName: String read FPackageFileName write FPackageFileName;
     property MinVersion: TPackageVersion read FMinVersion write SetMinVersion;
     property MaxVersion: TPackageVersion read FMaxVersion write SetMaxVersion;
   end;

   { TPackageDependencies }

   TPackageDependencies = class(TCollection)
   private
     function GetDependency(AIndex: Integer): TPackageDependency;
     procedure SetDependency(AIndex: Integer; const AValue: TPackageDependency);
   public
     function GetDependenciesAsString(const AIsDisplayString: Boolean): String;
     procedure SetDependenciesAsString(const AValue: String);
     property Dependencies[AIndex: Integer]: TPackageDependency read GetDependency write SetDependency; default;
   end;

  { TPackageFile }
  TPackageFile = class(TCollectionItem)
  private
    FName: String;
    FDescription: String;
    FAuthor: String;
    FLicense: String;
    FPackageState: TPackageState;
    FPackageStates: TPackageStates;
    FPackageType: TPackageType;
    FLazCompatibility: String;
    FFPCCompatibility: String;
    FSupportedWidgetSet: String;
    FPackageRelativePath: String;
    FPackageAbsolutePath: String;
    FInstalledFileName: String;
    FInstalledFileVersion: String;
    FUpdateVersion: String;
    FForceNotify: Boolean;
    FInternalVersion: Integer;
    FInternalVersionOld: Integer;
    FHasUpdate: Boolean;
    FVersion: TPackageVersion;
    FVersionAsString: String;
    FDependencies: TPackageDependencies;
    FDependenciesAsString: String;
    FChecked: Boolean;
    function GetVersionAsString: String;
    function GetDependenciesAsString: String;
    procedure SetVersionAsString(const AValue: String);
    procedure SetDependenciesAsString(const AValue: String);
    function GetInstallable: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    property Version: TPackageVersion read FVersion write FVersion;
    property Dependencies: TPackageDependencies read FDependencies write FDependencies;
    property PackageStates: TPackageStates read FPackageStates write FPackageStates;
    property PackageState: TPackageState read FPackageState write FPackageState;
    property InstalledFileName: String read FInstalledFileName write FInstalledFileName;
    property InstalledFileVersion: String read FInstalledFileVersion write FInstalledFileVersion;
    property UpdateVersion: String read FUpdateVersion write FUpdateVersion;
    property PackageAbsolutePath: String read FPackageAbsolutePath write FPackageAbsolutePath;
    property Checked: Boolean read FChecked write FChecked;
    property IsInstallable: Boolean read GetInstallable;
    property ForceNotify: Boolean read FForceNotify write FForceNotify;
    property InternalVersion: Integer read FInternalVersion write FInternalVersion;
    property InternalVersionOld: Integer read FInternalVersionOld write FInternalVersionOld;
    property HasUpdate: Boolean read FHasUpdate write FHasUpdate;
  published
    property Name: String read FName write FName;
    property Author: String read FAuthor write FAuthor;
    property Description: String read FDescription write FDescription;
    property PackageRelativePath: string read FPackageRelativePath write FPackageRelativePath;
    property VersionAsString: String read GetVersionAsString write SetVersionAsString;
    property LazCompatibility: String read FLazCompatibility write FLazCompatibility;
    property FPCCompatibility: String read FFPCCompatibility write FFPCCompatibility;
    property SupportedWidgetSet: String read FSupportedWidgetSet write FSupportedWidgetSet;
    property PackageType: TPackageType read FPackageType write FPackageType;
    property License: String read FLicense write FLicense;
    property DependenciesAsString: String read GetDependenciesAsString write SetDependenciesAsString;
  end;

  {TPackage}

  TPackage = class(TCollectionItem)
  private
    FName: String;
    FDisplayName: String;
    FCategory: String;
    FRepositoryFileName: String;
    FRepositoryFileSize: Int64;
    FRepositoryFileHash: String;
    FChecked: Boolean;
    FRepositoryDate: TDate;
    FPackageState: TPackageState;
    FPackageStates: TPackageStates;
    FPackageBaseDir: String;
    FHomePageURL: String;
    FDownloadURL: String;
    FDownloadZipURL: String;
    FHasUpdate: Boolean;
    FDisableInOPM: Boolean;
    FSVNURL: String;
    FUpdateSize: Int64;
    FIsDirZipped: Boolean;
    FPackageFiles: TCollection;
    function GetDownloadable: Boolean;
    function GetExtractable: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure ChangePackageStates(const AChangeType: TChangeType; APackageState: TPackageState);
    function FindPackageFile(const APackageFileName: String): TPackageFile;
  public
    property PackageStates: TPackageStates read FPackageStates;
    property PackageState: TPackageState read FPackageState;
    property IsDownloadable: Boolean read GetDownloadable;
    property IsExtractable: Boolean read GetExtractable;
    property UpdateSize: Int64 read FUpdateSize write FUpdateSize;
    property IsDirZipped: Boolean read FIsDirZipped write FIsDirZipped;
    property DownloadZipURL: String read FDownloadZipURL write FDownloadZipURL;
    property HasUpdate: Boolean read FHasUpdate write FHasUpdate;
    property DisableInOPM: Boolean read FDisableInOPM write FDisableInOPM;
  published
    property Name: String read FName write FName;
    property DisplayName: String read FDisplayName write FDisplayName;
    property Category: String read FCategory write FCategory;
    property Checked: Boolean read FChecked write FChecked;
    property RepositoryFileName: String read FRepositoryFileName write FRepositoryFileName;
    property RepositoryFileSize: int64 read FRepositoryFileSize write FRepositoryFileSize;
    property RepositoryFileHash: String read FRepositoryFileHash write FRepositoryFileHash;
    property RepositoryDate: TDate read FRepositoryDate write FRepositoryDate;
    property PackageBaseDir: String read FPackageBaseDir write FPackageBaseDir;
    property PackageFiles: TCollection read FPackageFiles write FPackageFiles;
    property HomePageURL: String read FHomePageURL write FHomePageURL;
    property DownloadURL: String read FDownloadURL write FDownloadURL;
    property SVNURL: String read FSVNURL write FSVNURL;
  end;

  { TSerializablePackages }

  TFindPackageBy = (fpbPackageName, fpbRepositoryFilename);
  TSerializablePackages = class
  private
    FPackages: TCollection;
    FLastError: String;
    FOnProcessJSON: TNotifyEvent;
    function GetCount: Integer;
    function GetDownloadCount: Integer;
    function GetExtractCount: Integer;
    function GetInstallCount: Integer;
    function GetItem(const AIndex: Integer): TPackage;
    procedure SetItem(const AIndex: Integer; const APackage: TPackage);
    procedure DoGetPackageDependencies(const APackageFileName: String; ASL: TStringList; ALevel: Integer);
    function JSONToPackageData(const APackageData: TJSONData; var APackage: TPackage): Boolean;
    function JSONToPackageFiles(const APackageData: TJSONData; var APackage: TPackage): Boolean;
    function PackageDataToJSON(APackage: TPackage; var APackageData: TJSONObject): Boolean;
    function PackageFilesToJSON(APackage: TPackage; var APackageFiles: TJSONArray): Boolean;
    function IsPackageDownloaded(const APackage: TPackage): Boolean;
    function IsPackageExtracted(const APackage: TPackage): Boolean;
    function IsPackageInstalled(const APackageFile: TPackageFile; const APackageBaseDir: String): Boolean;
    function IsAtLeastOnePackageFileInstalled(const APackage: TPackage): Boolean;
    function GetRuntimePackageVersion(const APath: String): String;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear;
    function AddPackage(const AName: String): TPackage;
    procedure DeletePackage(const AIndex: Integer);
    function FindPackage(const AValue: String; const AFindPackageBy: TFindPackageBy): TPackage;
    function FindPackageIndex(const AValue: String; const AFindPackageBy: TFindPackageBy): Integer;
    function FindPackageFile(const APackageFileName: String): TPackageFile;
    function JSONToPackages(JSON: TJSONStringType): Boolean;
    function PackagesToJSON(var JSON: TJSONStringType): Boolean;
    procedure GetPackageDependencies(const APackageFileName: String; List: TObjectList; Recurse, OnlyUnresolved: Boolean);
    procedure GetPackageStates;
    procedure RemoveErrorState;
    procedure MarkRuntimePackages;
    function Cleanup: Integer;
    function IsDependencyOk(PackageDependency: TPackageDependency; DependencyPackage: TPackageFile): Boolean;
    function IsInstalledVersionOk(PackageDependency: TPackageDependency; InstalledVersion: String): Boolean;
    function GetPackageInstallState(const APackage: TPackage): Integer; overload;
    procedure DeleteDownloadedZipFiles;
  public
    property Count: Integer read GetCount;
    property DownloadCount: Integer read GetDownloadCount;
    property ExtractCount: Integer read GetExtractCount;
    property InstallCount: Integer read GetInstallCount;
    property Items[Index: Integer]: TPackage read GetItem write SetItem;
    property LastError: String read FlastError;
    property OnProcessJSON: TNotifyEvent read FOnProcessJSON write FOnProcessJSON;
  end;

var
  SerializablePackages: TSerializablePackages = nil;


implementation
uses opkman_common, opkman_const, opkman_options;

{ TPackageVersion }

function TPackageVersion.GetAsString: String;
begin
  Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Release) + '.' + IntToStr(Build);
end;

procedure TPackageVersion.SetAsString(const AValue: String);
var
  Version: String;
  P, I: Integer;
begin
  SetDefaults;
  if AValue = '' then
    Exit;
  I := 0;
  Version := Trim(AValue) + '.';
  repeat
     Inc(I);
     P := Pos('.', Version);
     if P <> 0 then
     begin
       case I of
         1: FMajor := StrToIntDef(Copy(Version, 1, P - 1), 0);
         2: FMinor := StrToIntDef(Copy(Version, 1, P - 1), 0);
         3: FRelease := StrToIntDef(Copy(Version, 1, P - 1), 0);
         4: FBuild := StrToIntDef(Copy(Version, 1, P - 1), 0);
       end;
       Delete(Version, 1, P);
     end;
  until (Version = '') or (P = 0) or (I > 4);
end;

function TPackageVersion.GetIsNullVersion: Boolean;
begin
  Result := (FMajor = 0) and (FMinor = 0) and (FRelease = 0) and (FBuild = 0);
end;

procedure TPackageVersion.SetDefaults;
begin
  FMajor := 0;
  FMinor := 0;
  FRelease := 0;
  FBuild := 0;
end;

procedure TPackageVersion.Assign(ASource: TPersistent);
var
  Source: TPackageVersion;
begin
  SetDefaults;
  if ASource is TPackageVersion then
  begin
    Source := ASource as TPackageVersion;
    Major := Source.Major;
    Minor := Source.Minor;
    Release := Source.Release;
    Build := Source.Build;
  end
  else
    inherited Assign(Source);
end;

function TPackageVersion.CompareVersion(AVersion: TPackageVersion): Integer;
begin
  Result := Major - AVersion.Major;
  if (Result = 0) then
  begin
    Result := Minor - AVersion.Minor;
    if (Result = 0) then
    begin
      Result := Release - AVersion.Release;
      if (Result = 0) then
        Result := Build - AVersion.Build;
    end;
  end;
end;

function TPackageVersion.SameVersion(AVersion: TPackageVersion): Boolean;
begin
  Result := CompareVersion(AVersion) = 0;
end;

{ TPackageDependency }

procedure TPackageDependency.SetMinVersion(const AValue: TPackageVersion);
begin
  FMinVersion.Assign(AValue);
end;

procedure TPackageDependency.SetMaxVersion(const AValue: TPackageVersion);
begin
  FMaxVersion.Assign(AValue);
end;

procedure TPackageDependency.Assign(ASource: TPersistent);
var
  Source: TPackageDependency;
begin
  if ASource is TPackageDependency then
  begin
    Source := ASource as TPackageDependency;
    FPackageFileName := Source.PackageFileName;
    if Assigned(Source.MinVersion) then
      FMinVersion.Assign(Source.MinVersion);
    if Assigned(Source.MaxVersion) then
      FMaxVersion.Assign(Source.MaxVersion);
  end
  else
    inherited Assign(Source);
end;

constructor TPackageDependency.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMinVersion := TPackageVersion.Create;
  FMaxVersion := TPackageVersion.Create;
end;

destructor TPackageDependency.Destroy;
begin
  if Assigned(FMinVersion) then
    FMinVersion.Free;
  if Assigned(fMaxVersion) then
    FMaxVersion.Free;
  inherited Destroy;
end;

{ TPackageDependencies }

function TPackageDependencies.GetDependency(AIndex: Integer): TPackageDependency;
begin
  Result := TPackageDependency(Items[AIndex]);
end;

procedure TPackageDependencies.SetDependency(AIndex: Integer;
  const AValue: TPackageDependency);
begin
  Items[AIndex] := AValue;
end;

function TPackageDependencies.GetDependenciesAsString(const AIsDisplayString: Boolean): String;
var
  I: Integer;
  MinVer, MaxVer: String;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    MinVer := '';
    MaxVer := '';
    if not Dependencies[I].FMinVersion.IsNullVersion then
    begin
      if AIsDisplayString then
        MinVer := '(>=' + IntToStr(Dependencies[I].FMinVersion.Major) + '.' + IntToStr(Dependencies[I].FMinVersion.Minor) + ')'
      else
        MinVer := '(' + Dependencies[I].FMinVersion.AsString + ')';
    end;
    if not Dependencies[I].FMaxVersion.IsNullVersion then
    begin
      if AIsDisplayString then
        MaxVer := '(<=' + IntToStr(Dependencies[I].FMaxVersion.Major) + '.' + IntToStr(Dependencies[I].FMaxVersion.Minor) + ')'
      else
        MaxVer := '(' + Dependencies[I].FMaxVersion.AsString + ')'
    end;
    if Result = '' then
      Result := Dependencies[I].PackageFileName + MinVer + MaxVer
    else
      Result := Result + ', ' + Dependencies[I].PackageFileName + MinVer + MaxVer;
  end;
end;

procedure TPackageDependencies.SetDependenciesAsString(const AValue: String);
var
  PackageDependency: TPackageDependency;
  SL: TStringList;
  P1, P2: Integer;
  Str: String;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.DelimitedText := AValue;
    for I := 0 to SL.Count - 1  do
    begin
      Str := Trim(SL.Strings[I]);
      PackageDependency := TPackageDependency(Self.Add);
      if not Assigned(PackageDependency.FMinVersion) then
        PackageDependency.FMinVersion := TPackageVersion.Create;
      if not Assigned(PackageDependency.FMaxVersion) then
        PackageDependency.FMaxVersion := TPackageVersion.Create;
      P1 := Pos('(', Str);
      P2 := Pos(')', Str);
      if (P1 <> 0) and (P2 <> 0) then
      begin
        PackageDependency.PackageFileName := Trim(Copy(Str, 1, P1 - 1));
        PackageDependency.FMinVersion.AsString := Trim(Copy(Str, P1 + 1, P2 - P1 - 1));
        System.Delete(Str, 1, P2);
        if Length(Trim(Str)) > 0 then
          PackageDependency.FMaxVersion.AsString := Trim(Copy(Str, 2, Length(Str) - 2));
      end
      else
        PackageDependency.PackageFileName := Trim(Str);
    end;
  finally
    SL.Free;
  end;
end;

{ TPackageFile }

function TPackageFile.GetVersionAsString: String;
begin
  Result := FVersion.AsString;
end;

procedure TPackageFile.SetVersionAsString(const AValue: String);
begin
  if not Assigned(FVersion) then
  begin
    if not Assigned(FVersion) then
      FVersion := TPackageVersion.Create;
    FVersion.AsString := AValue;
  end;
  FVersionAsString := AValue;
end;

function TPackageFile.GetDependenciesAsString: String;
begin
  Result := FDependencies.GetDependenciesAsString(False);
end;

procedure TPackageFile.SetDependenciesAsString(const AValue: String);
begin
  if not Assigned(FDependencies) then
  begin
    FDependencies := TPackageDependencies.Create(TPackageDependency);
    FDependencies.SetDependenciesAsString(AValue);
  end;
  FDependenciesAsString := AValue;
end;

function TPackageFile.GetInstallable: Boolean;
begin
  case PackageAction of
     paDownloadTo:
       Result := False;
     paInstall, paUpdate:
       Result := (Checked) and
                 (psExtracted in PackageStates) and
                 (not (psError in PackageStates));
   end;
end;

constructor TPackageFile.Create;
begin
  FVersion := TPackageVersion.Create;
  FVersion.SetDefaults;
  PackageStates := [];
  FDependencies := TPackageDependencies.Create(TPackageDependency);
end;

destructor TPackageFile.Destroy;
begin
  if Assigned(FVersion) then
    FreeAndNil(FVersion);
  if Assigned(FDependencies) then
    FreeAndNil(FDependencies);
  inherited Destroy;
end;

{ TPackage }
function TPackage.GetDownloadable: Boolean;
begin
  case PackageAction of
    paDownloadTo, paUpdate:
      Result := (Checked) and (not (psError in PackageStates));
    paInstall:
      Result := (Checked) and
                (psRepository in PackageStates) and
                (not (psError in PackageStates)) and
                ((Options.ForceDownloadAndExtract) or ((not (psDownloaded in PackageStates)) and (not (psExtracted in PackageStates))));
  end;
end;

function TPackage.GetExtractable: Boolean;
begin
  case PackageAction of
     paDownloadTo, paUpdate:
       Result := (Checked) and (not (psError in PackageStates));
     paInstall:
       Result := (Checked) and
                 (psDownloaded in PackageStates) and
                 (not (psError in PackageStates)) and
                 ((Options.ForceDownloadAndExtract) or ((not (psExtracted in PackageStates)) and (not (psInstalled in PackageStates))));
   end;
end;

constructor TPackage.Create;
begin
  FPackageFiles := TCollection.Create(TPackageFile);
end;

destructor TPackage.Destroy;
var
  I: Integer;
begin
  FPackageFiles.Clear;
  for I := FPackageFiles.Count - 1 downto 0  do
    FPackageFiles.Items[I].Free;
  FPackageFiles.Free;
  inherited Destroy;
end;

procedure TPackage.ChangePackageStates(const AChangeType: TChangeType;
  APackageState: TPackageState);
var
  I: Integer;
  PackageFile: TPackageFile;
begin
  if APackageState = psInstalled then
    Exit;
  //propagate states to package files
  case AChangeType of
    ctAdd:
      begin
        FPackageStates := FPackageStates + [APackageState];
        for I := 0 to PackageFiles.Count - 1 do
        begin
          PackageFile := TPackageFile(PackageFiles.Items[I]);
          PackageFile.PackageStates := PackageFile.PackageStates + [APackageState];
          PackageFile.PackageState := APackageState;
        end;
      end;
    ctRemove:
      begin
        FPackageStates := FPackageStates - [APackageState];
        for I := 0 to PackageFiles.Count - 1 do
        begin
          PackageFile := TPackageFile(PackageFiles.Items[I]);
          PackageFile.PackageStates := PackageFile.PackageStates - [APackageState];
        end;
      end;
  end;
end;

function TPackage.FindPackageFile(const APackageFileName: String): TPackageFile;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FPackageFiles.Count - 1 do
  begin
    if UpperCase(TPackageFile(FPackageFiles.Items[I]).Name) = UpperCase(APackageFileName) then
    begin
      Result := TPackageFile(FPackageFiles.Items[I]);
      Break;
    end;
  end;
end;

{ TSerializablePackages }

constructor TSerializablePackages.Create;
begin
  FPackages := TCollection.Create(TPackage);
end;

destructor TSerializablePackages.Destroy;
begin
  Clear;
  FPackages.Free;
  inherited Destroy;
end;

procedure TSerializablePackages.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
  FPackages.Clear;
end;

function TSerializablePackages.GetCount: Integer;
begin
  Result := FPackages.Count;
end;

function TSerializablePackages.GetDownloadCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsDownloadable then
      Inc(Result);
end;

function TSerializablePackages.GetExtractCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].IsExtractable then
      Inc(Result);
end;

function TSerializablePackages.GetInstallCount: Integer;
var
  I, J: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].FPackageFiles.Count - 1 do
      if TPackageFile(Items[I].FPackageFiles.Items[J]).IsInstallable then
        Inc(Result);
end;

function TSerializablePackages.GetItem(const AIndex: Integer): TPackage;
begin
  Result := nil;
  if AIndex > FPackages.Count - 1 then
    Exit;
  Result := TPackage(FPackages.Items[AIndex]);
end;

procedure TSerializablePackages.SetItem(const AIndex: Integer;
  const APackage: TPackage);
begin
  if AIndex > FPackages.Count - 1 then
    Exit;
  FPackages.Items[AIndex] := TPackage(APackage);
end;

procedure TSerializablePackages.DoGetPackageDependencies(
  const APackageFileName: String; ASL: TStringList; ALevel: Integer);
var
  PackageFile: TPackageFile;
  D2, D1: TPackageDependency;
  I, J: Integer;
begin
  if (ALevel > 10) then
    Exit;
  PackageFile := FindPackageFile(APackageFileName);
  if PackageFile = nil then
    Exit;
  for I := 0 to PackageFile.Dependencies.Count - 1 do
  begin
    D1 := PackageFile.Dependencies[I];
    J := ASL.IndexOf(APackageFileName);
    If J = -1 then
    begin
      D2 := TPackageDependency.Create(nil);
      D2.Assign(D1);
      ASL.AddObject(D2.PackageFileName, D2);
    end
    else
    begin
      D2 := ASL.Objects[J] as TPackageDependency;
      if D1.MinVersion.CompareVersion(D2.MinVersion) > 0 then
        D2.MinVersion.Assign(D1.MinVersion);
    end;
    if (ALevel >= 0) and (J = -1) Then
      DoGetPackageDependencies(D2.PackageFileName, ASL, ALevel + 1);
  end;
end;

function TSerializablePackages.AddPackage(const AName: String): TPackage;
var
  Package: TPackage;
begin
  Result := nil;
  Package := FindPackage(AName, fpbPackageName);
  if Package <> nil then
  begin
    FLastError := rsMainFrm_PackageNameAlreadyExists;
    Exit;
  end;
  Result := TPackage(FPackages.Add);
  Result.FPackageFiles := TCollection.Create(TPackageFile);
  Result.Name := AName;
end;

procedure TSerializablePackages.DeletePackage(const AIndex: Integer);
begin
  if AIndex > FPackages.Count - 1 then
    Exit;
  FPackages.Delete(AIndex);
end;

function TSerializablePackages.FindPackage(const AValue: String;
  const AFindPackageBy: TFindPackageBy): TPackage;
var
  I: Integer;
  NeedToBreak: Boolean;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    case AFindPackageBy of
      fpbPackageName: NeedToBreak := UpperCase(Items[I].Name) = UpperCase(AValue);
      fpbRepositoryFilename: NeedToBreak := UpperCase(Items[I].RepositoryFileName) = UpperCase(AValue)
    end;
    if NeedToBreak then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TSerializablePackages.FindPackageIndex(const AValue: String;
  const AFindPackageBy: TFindPackageBy): Integer;
var
  I: Integer;
  NeedToBreak: Boolean;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    case AFindPackageBy of
      fpbPackageName: NeedToBreak := Items[I].Name = AValue;
      fpbRepositoryFilename: NeedToBreak := Items[I].RepositoryFileName = AValue
    end;
    if NeedToBreak then
    begin
      Result := I;
      Break;
    end;
  end;
end;


function TSerializablePackages.FindPackageFile(const APackageFileName: String): TPackageFile;
var
  I, J: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    for J := 0 to Items[I].FPackageFiles.Count - 1 do
    begin
      if UpperCase(TPackageFile(Items[I].FPackageFiles.Items[J]).Name) = UpperCase(APackageFileName) then
      begin
        Result := TPackageFile(Items[I].FPackageFiles.Items[J]);
        Break;
      end;
    end;
  end;
end;

function TSerializablePackages.JSONToPackageData(const APackageData: TJSONData;
  var APackage: TPackage): Boolean;
var
  PackageData: TJSONObject;
begin
  Result := True;
  try
    PackageData := TJSONObject(APackageData);
    APackage := TPackage(FPackages.Add);
    //need to change
    APackage.Name := PackageData.Get('Name');
    APackage.DisplayName := PackageData.Get('DisplayName');
    APackage.Category := PackageData.Get('Category');
    APackage.RepositoryFileName := PackageData.Get('RepositoryFileName');
    APackage.RepositoryFileSize := PackageData.Get('RepositoryFileSize');
    APackage.RepositoryFileHash := PackageData.Get('RepositoryFileHash');
    APackage.RepositoryDate := VarToDateTime(PackageData.Get('RepositoryDate'));
    APackage.PackageBaseDir := PackageData.Get('PackageBaseDir');
    if APackage.PackageBaseDir <> '' then
      APackage.PackageBaseDir := StringReplace(APackage.PackageBaseDir, '\/', PathDelim, [rfReplaceAll]);;
    APackage.HomePageURL := PackageData.Get('HomePageURL');
    APackage.DownloadURL := PackageData.Get('DownloadURL');
    APackage.SVNURL := PackageData.Get('SVNURL');
  except
    on E: Exception do
    begin
      Result := False;
      FlastError := '"' + APackage.Name + '": ' + E.Message;
    end;
  end;
end;

function TSerializablePackages.JSONToPackageFiles(const APackageData: TJSONData;
  var APackage: TPackage): Boolean;
var
  PackageFiles: TJSONArray;
  PackageFilesObject: TJSONObject;
  PackageFile: TPackageFile;
  I: Integer;
begin
  Result := True;
  try
    PackageFiles := TJSONArray(APackageData);
    APackage.PackageFiles := TCollection.Create(TPackageFile);
    for I := 0 to PackageFiles.Count - 1 do
    begin
      if PackageFiles.Items[I].JSONType = jtObject then
      begin
       PackageFilesObject := TJSONObject(PackageFiles.Items[I]);
       PackageFile := TPackageFile(APackage.PackageFiles.Add);
       //need to change
       PackageFile.Name := PackageFilesObject.Get('Name');
       PackageFile.Description := PackageFilesObject.Get('Description');
       PackageFile.Author := PackageFilesObject.Get('Author');
       PackageFile.License := PackageFilesObject.Get('License');
       PackageFile.PackageRelativePath := PackageFilesObject.Get('RelativeFilePath');
       if PackageFile.PackageRelativePath <> '' then
         PackageFile.PackageRelativePath := StringReplace(PackageFile.PackageRelativePath, '\/', PathDelim, [rfReplaceAll]);
       PackageFile.VersionAsString := PackageFilesObject.Get('VersionAsString');
       PackageFile.LazCompatibility := PackageFilesObject.Get('LazCompatibility');
       PackageFile.FPCCompatibility := PackageFilesObject.Get('FPCCompatibility');
       PackageFile.SupportedWidgetSet := PackageFilesObject.Get('SupportedWidgetSet');
       PackageFile.PackageType := TPackageType(PackageFilesObject.Get('PackageType'));
       PackageFile.DependenciesAsString := PackageFilesObject.Get('DependenciesAsString');
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      FlastError := '"' + PackageFile.Name + '": ' + E.Message;
    end;
  end;
end;

function TSerializablePackages.JSONToPackages(JSON: TJSONStringType): Boolean;
var
  Data: TJSONData;
  Parser: TJSONParser;
  I: Integer;
  Package: TPackage;
begin
  Clear;
  Result := True;
  Parser := TJSONParser.Create(JSON);
  try
    Data := Parser.Parse;
    try
      Package := nil;
      try
        if Data.JSONType = jtObject then
        begin
          for I := 0 to Data.Count - 1 do
          begin
            if Assigned(FOnProcessJSON) then
              FOnProcessJSON(Self);
            if Data.Items[I].JSONType = jtObject then
            begin
              if not JSONToPackageData(Data.Items[I], Package) then
                Result := False;
            end
            else if Data.Items[I].JSONType = jtArray then
            begin
              if not JSONToPackageFiles(Data.Items[I], Package) then
                Result := False;
            end;
          end;
          if Result then
            GetPackageStates;
        end;
      except
        Result := False;
      end;
    finally
      Data.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TSerializablePackages.PackageFilesToJSON(APackage: TPackage;
 var APackageFiles: TJSONArray): Boolean;
var
  PackageFile: TPackageFile;
  PackageFileObject: TJSONObject;
  I: Integer;
begin
  Result := True;
  try
    APackageFiles := TJSONArray.Create;
    for I := 0 to APackage.FPackageFiles.Count - 1 do
    begin
      PackageFile := TPackageFile(APackage.FPackageFiles.Items[I]);
      PackageFileObject := TJSONObject.Create;
      //need to change
      PackageFileObject.Add('Name', PackageFile.Name);
      PackageFileObject.Add('Description', PackageFile.Description);
      PackageFileObject.Add('Author', PackageFile.Author);
      PackageFileObject.Add('License', PackageFile.License);
      PackageFileObject.Add('RelativeFilePath', PackageFile.PackageRelativePath);
      PackageFileObject.Add('VersionAsString', PackageFile.VersionAsString);
      PackageFileObject.Add('LazCompatibility', PackageFile.LazCompatibility);
      PackageFileObject.Add('FPCCompatibility', PackageFile.FPCCompatibility);
      PackageFileObject.Add('SupportedWidgetSet', PackageFile.SupportedWidgetSet);
      PackageFileObject.Add('PackageType', Ord(PackageFile.PackageType));
      PackageFileObject.Add('DependenciesAsString', PackageFile.DependenciesAsString);
      APackageFiles.Add(PackageFileObject);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      FlastError := '"' + PackageFile.Name + '": ' + E.Message;
    end;
  end;
end;

function TSerializablePackages.IsPackageDownloaded(const APackage: TPackage): Boolean;
var
  FileName: String;
begin
  FileName := Options.LocalRepositoryArchive + APackage.RepositoryFileName;
  Result := (FileExists(FileName)) and
            (MD5Print(MD5File(FileName)) = APackage.RepositoryFileHash) and
            (FileUtil.FileSize(FileName) = APackage.RepositoryFileSize);
end;

function TSerializablePackages.IsPackageExtracted(const APackage: TPackage): Boolean;
var
  I: Integer;
  PackageFile: TPackageFile;
begin
  Result := True;
  for I := 0 to APackage.FPackageFiles.Count - 1 do
  begin
    PackageFile := TPackageFile(APackage.FPackageFiles.Items[I]);
    PackageFile.FPackageAbsolutePath := Options.LocalRepositoryPackages + APackage.PackageBaseDir + PackageFile.FPackageRelativePath + PackageFile.Name;
    if not FileExists(PackageFile.FPackageAbsolutePath) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TSerializablePackages.GetRuntimePackageVersion(const APath: String): String;

  function VersionBound(const AVersion: Integer): Integer;
  begin
    if AVersion > 9999 then
      Result := 9999
    else if AVersion < 0 then
      Result := 0
    else
      Result := AVersion;
  end;

  function GetVersion(const AXMLConfig: TXMLConfig; const APath: String): String;
  var
    Major, Minor, Release, Build: Integer;
  begin
    Major := VersionBound(AXMLConfig.GetValue(APath + '/Major', 0));
    Minor := VersionBound(AXMLConfig.GetValue(APath + '/Minor', 0));
    Release := VersionBound(AXMLConfig.GetValue(APath + '/Release', 0));
    Build := VersionBound(AXMLConfig.GetValue(APath + '/Build', 0));
    Result := IntToStr(Major) + '.' + IntToStr(Minor) + '.' + IntToStr(Release) + '.' + IntToStr(Build);
  end;

var
  XMLConfig: TXMLConfig;
begin
  Result := '-';
  XMLConfig := TXMLConfig.Create(APath);
  try
    Result := GetVersion(XMLConfig, 'Package/Version');
  finally
    XMLConfig.Free;
  end;
end;


function TSerializablePackages.IsPackageInstalled(const APackageFile: TPackageFile;
  const APackageBaseDir: String): Boolean;

  function CheckIDEPackages: Boolean;
  var
    PackageCnt: Integer;
    I: Integer;
    Package: TIDEPackage;
  begin
    Result := False;
    PackageCnt := PackageEditingInterface.GetPackageCount;
    for I := 0 to PackageCnt - 1 do
    begin
      Package := PackageEditingInterface.GetPackages(I);
      if ExtractFileName(Package.FileName) = APackageFile.Name then
      begin
        APackageFile.InstalledFileName := Package.Filename;
        APackageFile.InstalledFileVersion := IntToStr(Package.Version.Major) + '.' +
                                         IntToStr(Package.Version.Minor) + '.' +
                                         IntToStr(Package.Version.Release) + '.' +
                                         IntToStr(Package.Version.Build);
        Result := True;
        Break;
      end;
    end;
  end;

var
  FileName: String;
begin
  Result := False;
  case APackageFile.PackageType of
    ptRunTime, ptRunTimeOnly:
      begin
        FileName := StringReplace(APackageFile.Name, '.lpk', '.opkman', [rfIgnoreCase]);
        Result := (psExtracted in APackageFile.PackageStates) and
                  FileExists(Options.LocalRepositoryPackages + APackageBaseDir + APackageFile.PackageRelativePath + FileName);
        if Result then
        begin
          APackageFile.InstalledFileName := Options.LocalRepositoryPackages + APackageBaseDir + APackageFile.FPackageRelativePath + APackageFile.Name;
          APackageFile.InstalledFileVersion := GetRuntimePackageVersion(APackageFile.InstalledFileName);
          Result := True;
        end
        else
          Result := CheckIDEPackages
      end;
    ptDesignTime, ptRunAndDesignTime:
      begin
        Result := CheckIDEPackages
      end;
  end;
end;

function TSerializablePackages.GetPackageInstallState(const APackage: TPackage): Integer;
var
  I: Integer;
  PackageFile: TPackageFile;
  InstCnt: Integer;
begin
  InstCnt := 0;
  for I := 0 to APackage.PackageFiles.Count - 1 do
  begin
    PackageFile := TPackageFile(APackage.PackageFiles.Items[I]);
    if IsPackageInstalled(PackageFile, APackage.PackageBaseDir) then
      Inc(InstCnt);
  end;
  case InstCnt of
    0: Result := 0;
    1..High(Integer):
        if InstCnt < APackage.PackageFiles.Count then
          Result := 2
        else
          Result := 1;
  end;
end;

function TSerializablePackages.PackageDataToJSON(APackage: TPackage;
 var APackageData: TJSONObject): Boolean;
begin
  //need to change
  Result := True;
  try
    APackageData := TJSONObject.Create;
    APackageData.Add('Name', TPackage(APackage).Name);
    APackageData.Add('DisplayName', APackage.DisplayName);
    APackageData.Add('Category', TPackage(APackage).Category);
    APackageData.Add('RepositoryFileName', TPackage(APackage).RepositoryFileName);
    APackageData.Add('RepositoryFileSize', TPackage(APackage).RepositoryFileSize);
    APackageData.Add('RepositoryFileHash', TPackage(APackage).RepositoryFileHash);
    APackageData.Add('RepositoryDate', TPackage(APackage).RepositoryDate);
    APackageData.Add('PackageBaseDir', TPackage(APackage).PackageBaseDir);
    APackageData.Add('HomePageURL', TPackage(APackage).HomePageURL);
    APackageData.Add('DownloadURL', TPackage(APackage).DownloadURL);
    APackageData.Add('SVNURL', TPackage(APackage).SVNURL);
  except
    on E: Exception do
    begin
      Result := False;
      FlastError := '"' + TPackage(APackage).Name + '": ' + E.Message;
    end;
  end;
end;

function TSerializablePackages.PackagesToJSON(var JSON: TJSONStringType): Boolean;
var
  PackageObject: TJSONObject;
  PackageData: TJSONObject;
  PackageFiles: TJSONArray;
  I: Integer;
  Package: TPackage;
begin
  Result := True;
  PackageObject := TJSONObject.Create;
  try
    PackageFiles := nil;
    PackageData := nil;
    try
      for I := 0 to FPackages.Count - 1 do
      begin
        Package := TPackage(FPackages.Items[I]);
        if not PackageFilesToJSON(Package, PackageFiles) then
          Result := False;
        if not PackageDataToJSON(Package, PackageData) then
          Result := False;
        PackageObject.Add('PackageData' + IntToStr(I), PackageData);
        PackageObject.Add('PackageFiles' + IntToStr(I), PackageFiles);
      end;
      if Result then
        JSON := PackageObject.FormatJSON(DefaultFormat, DefaultIndentSize);
    except
      Result := False;
    end;
  finally
    PackageObject.Free;
  end;
end;

procedure TSerializablePackages.GetPackageDependencies(const APackageFileName: String;
 List: TObjectList; Recurse, OnlyUnresolved: Boolean);
var
  SL: TStringList;
  I, J: Integer;
  PackageFileName: String;
  Installed: Boolean;
  Package: TIDEPackage;
  PackageFile: TPackageFile;
begin
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    DoGetPackageDependencies(APackageFileName, SL, Ord(Recurse) - 1);
    if OnlyUnresolved then
    begin
      for I := SL.Count - 1 downto 0 do
      begin
        PackageFileName := TPackageDependency(SL.Objects[I]).PackageFileName + '.lpk';
        Installed := False;
        for J := 0 to PackageEditingInterface.GetPackageCount - 1 do
        begin
          Package := PackageEditingInterface.GetPackages(J);
          if UpperCase(ExtractFileName(Package.Filename)) = UpperCase(PackageFileName) then
          begin
            PackageFile := FindPackageFile(PackageFileName);
            if PackageFile <> nil then
              Installed := IsInstalledVersionOk(TPackageDependency(SL.Objects[I]), PackageFile.InstalledFileVersion)
            else
              Installed := True;
            Break;
          end;
        end;
        if Installed then
          SL.Objects[I].Free
        else
          List.Add(SL.Objects[I])
      end;
    end
    else
      for I := 0 to SL.Count - 1 do
        List.Add(SL.Objects[I]);
  finally
    SL.Free;
  end;
end;

procedure TSerializablePackages.GetPackageStates;
var
  I, J: Integer;
  PackageFile: TPackageFile;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].FPackageState := psRepository;
    Items[I].ChangePackageStates(ctAdd, psRepository);

    if IsPackageDownloaded(Items[I]) then
      Items[I].ChangePackageStates(ctAdd, psDownloaded)
    else
      Items[I].ChangePackageStates(ctRemove, psDownloaded);


    if IsPackageExtracted(Items[I]) then
      Items[I].ChangePackageStates(ctAdd, psExtracted)
    else
      Items[I].ChangePackageStates(ctRemove, psExtracted);

    for J := 0 to Items[I].FPackageFiles.Count - 1 do
    begin
      PackageFile := TPackageFile(Items[I].FPackageFiles.Items[J]);
      if IsPackageInstalled(PackageFile, Items[I].PackageBaseDir) then
      begin
        PackageFile.PackageStates := PackageFile.PackageStates + [psInstalled];
        PackageFile.PackageState := psInstalled;
      end
      else
        PackageFile.PackageStates := PackageFile.PackageStates - [psInstalled];
    end;
  end;
end;

procedure TSerializablePackages.RemoveErrorState;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if psError in Items[I].PackageStates then
      Items[I].ChangePackageStates(ctRemove, psError);
    for J := 0 to Items[I].FPackageFiles.Count - 1 do
     if psError in TPackageFile(Items[I].FPackageFiles.Items[J]).PackageStates then
       TPackageFile(Items[I].FPackageFiles.Items[J]).PackageStates := TPackageFile(Items[I].FPackageFiles.Items[J]).PackageStates - [psError];
  end;
end;

procedure TSerializablePackages.MarkRuntimePackages;
var
  I, J: Integer;
  FileName: String;
  PackageFile: TPackageFile;
begin
  for I := 0 to Count - 1 do
  begin
    for J := 0 to Items[I].FPackageFiles.Count - 1 do
    begin
      PackageFile := TPackageFile(Items[I].FPackageFiles.Items[J]);
      if (PackageFile.Checked) and
         (psInstalled in PackageFile.PackageStates) and
           (not (psError in PackageFile.PackageStates)) and
             (PackageFile.PackageType in [ptRunTime, ptRunTimeOnly]) then
      begin
        FileName := StringReplace(PackageFile.Name, '.lpk', '.opkman', [rfIgnoreCase]);
        FileCreate(Options.LocalRepositoryPackages + Items[I].PackageBaseDir + PackageFile.PackageRelativePath + FileName);
      end;
    end;
  end;
end;

function TSerializablePackages.IsAtLeastOnePackageFileInstalled(
  const APackage: TPackage): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to APackage.PackageFiles.Count - 1 do
  begin
    if IsPackageInstalled(TPackageFile(APackage.FPackageFiles.Items[I]), APackage.PackageBaseDir) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TSerializablePackages.Cleanup: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if not IsAtLeastOnePackageFileInstalled(Items[I]) then
    begin
      if IsPackageDownloaded(Items[I]) then
      begin
        if DeleteFile(Options.LocalRepositoryArchive + Items[I].RepositoryFileName) then
          Inc(Result);
      end;
      if IsPackageExtracted(Items[I]) then
        if DirectoryExists(Options.LocalRepositoryPackages + Items[I].PackageBaseDir) then
          DeleteDirectory(Options.LocalRepositoryPackages + Items[I].PackageBaseDir, False);
    end;
  end;
end;

procedure TSerializablePackages.DeleteDownloadedZipFiles;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    case PackageAction of
      paInstall:
        begin
          if IsPackageDownloaded(Items[I]) then
            DeleteFile(Options.LocalRepositoryArchive + Items[I].RepositoryFileName)
        end;
      paUpdate:
        begin
          if FileExists(Options.LocalRepositoryUpdate + Items[I].RepositoryFileName) then
            DeleteFile(Options.LocalRepositoryUpdate + Items[I].RepositoryFileName)
        end;
    end;
  end;
end;


function TSerializablePackages.IsDependencyOk(PackageDependency: TPackageDependency;
  DependencyPackage: TPackageFile): Boolean;
var
  MinVerOk: Boolean;
  MaxVerOk: Boolean;
begin
  if PackageDependency.MinVersion.IsNullVersion then
    MinVerOk := True
  else
    MinVerOk := PackageDependency.MinVersion.CompareVersion(DependencyPackage.Version) <= 0;

  if PackageDependency.MaxVersion.IsNullVersion then
    MaxVerOk := True
  else
    MaxVerOk := PackageDependency.MaxVersion.CompareVersion(DependencyPackage.Version) >= 0;

  Result := (MinVerOk) and (MaxVerOk)
end;

function TSerializablePackages.IsInstalledVersionOk(PackageDependency: TPackageDependency;
  InstalledVersion: String): Boolean;
var
  MinVerOk: Boolean;
  MaxVerOk: Boolean;
begin
  if PackageDependency.MinVersion.IsNullVersion then
    MinVerOk := True
  else
    MinVerOk := PackageDependency.MinVersion.AsString <= InstalledVersion;

  if PackageDependency.MaxVersion.IsNullVersion then
    MaxVerOk := True
  else
    MaxVerOk := PackageDependency.MaxVersion.AsString >= InstalledVersion;

  Result := (MinVerOk) and (MaxVerOk)
end;

end.

