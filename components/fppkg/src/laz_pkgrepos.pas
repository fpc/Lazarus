unit laz_pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ComCtrls,
  fprepos;

type
  TLazPackageData = record
    Name: string;
    InstalledVersion: string;
    AvialableVersion: string;
    Description: string;
    Keywords: string;
    Category: string;
    State: string;
    Support: string;
    Author: string;
    License: string;
    HomepageURL: string;
    DownloadURL: string;
    FileName: string;
    Email: string;
    OS: string;
    CPU: string;
  end;

  TPackageSortType = (stNone);

  { TLazPackages }

  TLazPackages = class(TObject)
    FPkgData: array of TLazPackageData;
  private
    FCount: integer;
    FSort: TPackageSortType;
    FSortType: TPackageSortType;
    function GetPkgData(index: integer): TLazPackageData;
    procedure SetSort(const AValue: TPackageSortType);
    procedure SetSortType(const AValue: TPackageSortType);

  public
    constructor Create;
    destructor Destroy; override;

    property PkgData[index: integer]: TLazPackageData read GetPkgData;
    property Count: integer read FCount;
    procedure Add(Pkg: TLazPackageData);
    procedure Clear;

    property SortType: TPackageSortType read FSortType write SetSortType;
    procedure Sort;
    function FindPackage(const AName: string): TLazPackageData;
  end;

procedure Laz_ListPackages;

var
  Laz_Packages: TLazPackages;

implementation

uses
  pkgglobals,
  pkgrepos;

function Laz_PackageInstalledVersionStr(const AName:String;const ShowUsed: boolean = false;const Local: boolean = false):string;
var
  P: TFPPackage;
begin
  P := InstalledRepository.FindPackage(AName);
  if P <> nil then
    Result := P.Version.AsString
  else
    Result := '-';
end;

function GetPackage(const AName: string): TFPPackage;
begin
  Result := AvailableRepository.FindPackage(AName);

  if not Assigned(Result) then
    Result := InstalledRepository.FindPackage(AName);
end;

procedure Laz_ListPackages;
var
  i: integer;
  SL: TStringList;
  PackageName: string;
  pkg: TLazPackageData;
  P: TFPPackage;
begin
  SL := TStringList.Create;
  SL.Sorted := True;
  SL.Duplicates := dupIgnore;

  for i := 0 to AvailableRepository.PackageCount - 1 do
    SL.Add(AvailableRepository.Packages[i].Name);

  for i := 0 to InstalledRepository.PackageCount - 1 do
    SL.Add(InstalledRepository.Packages[i].Name);

  Laz_Packages.Clear;

  for i := 0 to SL.Count - 1 do
  begin
    PackageName := SL[i];
    if (PackageName <> CmdLinePackageName) and (PackageName <>
      CurrentDirPackageName) then
    begin
      pkg.Name := PackageName;

      pkg.State := PackageInstalledStateStr(PackageName);
      pkg.InstalledVersion := PackageInstalledVersionStr(PackageName);
      pkg.AvialableVersion := PackageAvailableVersionStr(PackageName);

      P := GetPackage(PackageName);
      if Assigned(P) then
      begin
        pkg.Description := P.Description;
        pkg.Author := P.Author;
        pkg.HomepageURL := P.HomepageURL;
        pkg.DownloadURL := P.DownloadURL;
        pkg.FileName := P.FileName;
        pkg.Email := P.Email;
        pkg.OS := OSesToString(P.OSes);
        pkg.CPU := CPUSToString(P.CPUs);
      end;

      Laz_Packages.Add(pkg);
    end;
  end;

  FreeAndNil(SL);
end;

{ TLazPackages }

function TLazPackages.GetPkgData(index: integer): TLazPackageData;
begin
  Result := FPkgData[index];
end;

procedure TLazPackages.SetSort(const AValue: TPackageSortType);
begin
  if FSort = AValue then
    exit;
  FSort := AValue;
end;

procedure TLazPackages.SetSortType(const AValue: TPackageSortType);
begin
  if FSortType = AValue then
    exit;
  FSortType := AValue;
end;

constructor TLazPackages.Create;
begin
  Clear;

  SortType := stNone;
end;

destructor TLazPackages.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLazPackages.Add(Pkg: TLazPackageData);
begin
  Inc(FCount);

  SetLength(FPkgData, FCount);
  FPkgData[FCount - 1] := Pkg;
end;

procedure TLazPackages.Clear;
begin
  FCount := 0;
  SetLength(FPkgData, FCount);
end;

procedure TLazPackages.Sort;
begin
  case SortType of
    //no sorting
    stNone:
  end;
end;

function TLazPackages.FindPackage(const AName: string): TLazPackageData;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if FPkgData[i].Name = AName then
    begin
      Result := FPkgData[i];
      exit;
    end;
end;

initialization
  Laz_Packages := TLazPackages.Create;

finalization
  FreeAndNil(Laz_Packages);

end.

