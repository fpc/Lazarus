unit laz_pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  LMessages,
  fgl,
  pkgFppkg,
  fprepos;

const
  WM_LogMessageWaiting = LM_USER + 1;
  WM_WorkerThreadDone = LM_USER + 2;

type
  TFppkgConfigOptions = record
    ConfigFile: string;
  end;

  { TLazFPPackage }

  TLazFPPackageList = specialize TFPGObjectList<TFPPackage>;
  TLazPackageInstallState = (lpiDownloadable, lpiAvailabe, lpiInstalled);

  { TLazPackage }

  TLazPackage = Class(TComponent)
  private
    FName: string;
    FPackageManager: TpkgFPpkg;
    FPPackageList: TLazFPPackageList;
    function GetCategory: string;
    function GetDefaultFPPackage: TFPPackage;
    function GetDescription: string;
    function GetKeywords: string;
    function GetState: TLazPackageInstallState;
    function GetSupport: string;
    function GetVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddFPPackage(AFPPackage: TFPPackage);

    function GetInfo(PackageManager: TpkgFPpkg): string;

    property Name: string read FName;
    property State: TLazPackageInstallState read GetState;
    property PackageManager: TpkgFPpkg read FPackageManager write FPackageManager;
    property Version: string read GetVersion;
    property Description: string read GetDescription;
    property Category: string read GetCategory;
    property Keywords: string read GetKeywords;
    property Support: string read GetSupport;
  end;

  TLazPackageList = specialize TFPGObjectList<TLazPackage>;

  { TLazPackages }

  TLazPackages = class(TComponent)
  private
    FPackageManager: TpkgFPpkg;
    FLazPackageList: TLazPackageList;
    function GetCount: integer;
    function GetLazPackage(index: integer): TLazPackage;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PkgData[index: integer]: TLazPackage read GetLazPackage;
    property Count: integer read GetCount;
    procedure AddFPPackage(AFPPackage: TFPPackage);
    procedure Clear;

    property PackageManager: TpkgFPpkg read FPackageManager write FPackageManager;
  end;

const
  SLazPackageInstallStateString: array[TLazPackageInstallState] of string = (
    'Downloadable',
    'Available',
    'Installed'
  );

implementation

uses
  pkgglobals,
  pkgoptions,
  pkgrepos;

{ TLazPackage }

function TLazPackage.GetDefaultFPPackage: TFPPackage;
var
  Package: TFPPackage;
  i: Integer;
begin
  Result := nil;
  for i := 0 to FPPackageList.Count -1 do
    begin
    Package := FPPackageList.Items[i];
    if Package.Repository.RepositoryType = fprtInstalled then
      Result := Package
    else if not assigned(Result) then
      Result := Package;
    end;
end;

function TLazPackage.GetCategory: string;
begin
  Result := GetDefaultFPPackage.Category;
end;

function TLazPackage.GetDescription: string;
begin
  Result := GetDefaultFPPackage.Description;
end;

function TLazPackage.GetKeywords: string;
begin
  Result := GetDefaultFPPackage.Keywords;
end;

function TLazPackage.GetInfo(PackageManager: TpkgFPpkg): string;
var
  Package: TFPPackage;
begin
  Result := '';
  Package := GetDefaultFPPackage;
  if PackageManager.PackageIsBroken(Package, nil) then
    Result := 'Broken';
end;

function TLazPackage.GetState: TLazPackageInstallState;
var
  i: Integer;
  Package: TFPPackage;
  ArchiveFile: string;
begin
  result := lpiAvailabe;
  for i := 0 to FPPackageList.Count-1 do
    begin
    Package := FPPackageList.Items[i];
    if Package.Repository.RepositoryType = fprtInstalled then
      begin
      Result := lpiInstalled;
      Exit;
      end;
    if Package.PackagesStructure.UnzipBeforeUse then
      begin
        ArchiveFile:=PackageManager.PackageLocalArchive(Package);
        if (ArchiveFile<>'') and not FileExists(ArchiveFile) then
          result := lpiDownloadable;
      end;
    end;
end;

function TLazPackage.GetSupport: string;
begin
  result := GetDefaultFPPackage.Support;
end;

function TLazPackage.GetVersion: string;
begin
  result := GetDefaultFPPackage.Version.AsString;
end;

constructor TLazPackage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPPackageList := TLazFPPackageList.Create(False);
end;

destructor TLazPackage.Destroy;
begin
  FPPackageList.Free;
  inherited Destroy;
end;

procedure TLazPackage.AddFPPackage(AFPPackage: TFPPackage);
begin
  if FPPackageList.Count = 0 then
    begin
    FName := AFPPackage.Name;
    end;
  assert(AFPPackage.Name=FName);
  FPPackageList.Add(AFPPackage);
end;

{ TLazPackages }

function TLazPackages.GetCount: integer;
begin
  Result := FLazPackageList.Count;
end;

function TLazPackages.GetLazPackage(index: integer): TLazPackage;
begin
  result := FLazPackageList.Items[index];
end;

constructor TLazPackages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLazPackageList := TLazPackageList.Create(False);
end;

destructor TLazPackages.Destroy;
begin
  FLazPackageList.Free;
  inherited Destroy;
end;

procedure TLazPackages.AddFPPackage(AFPPackage: TFPPackage);
var
  i: Integer;
  LazPackage: TLazPackage;
begin
  for i := 0 to FLazPackageList.Count -1 do
    begin
    if FLazPackageList.Items[i].Name = AFPPackage.Name then
      begin
      FLazPackageList.Items[i].AddFPPackage(AFPPackage);
      Exit;
      end;
    end;

  LazPackage := TLazPackage.Create(Owner);
  LazPackage.PackageManager := PackageManager;
  LazPackage.AddFPPackage(AFPPackage);
  FLazPackageList.Add(LazPackage);
end;

procedure TLazPackages.Clear;
begin
  FLazPackageList.Clear;
end;

end.

