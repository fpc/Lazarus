unit FppkgHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fprepos,
  pkgFppkg;

type

  { TFppkgHelper }

  TFppkgHelper = class
  private
    FFPpkg: TpkgFPpkg;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TFppkgHelper;
    function HasPackage(const PackageName: string): Boolean;
    procedure ListPackages(AList: TStringList);
  end;

implementation

var
  GFppkgHelper: TFppkgHelper = nil;

{ TFppkgHelper }

constructor TFppkgHelper.Create;
var
  FPpkg: TpkgFPpkg;
begin
  FPpkg := TpkgFPpkg.Create(nil);
  try
    FPpkg.InitializeGlobalOptions('');
    FPpkg.InitializeCompilerOptions;

    FPpkg.CompilerOptions.CheckCompilerValues;
    FPpkg.FpmakeCompilerOptions.CheckCompilerValues;

    FPpkg.LoadLocalAvailableMirrors;

    FPpkg.ScanPackages;

    FFPpkg := FPpkg;
    FPpkg := nil;
  finally
    FPpkg.Free;
  end;
end;

destructor TFppkgHelper.Destroy;
begin
  FFPpkg.Free;
  inherited Destroy;
end;

class function TFppkgHelper.Instance: TFppkgHelper;
begin
  if not Assigned(GFppkgHelper) then
    GFppkgHelper := TFppkgHelper.Create;
  Result := GFppkgHelper;
end;

function TFppkgHelper.HasPackage(const PackageName: string): Boolean;
begin
  Result :=
    Assigned(FFPpkg.FindPackage(PackageName,pkgpkInstalled)) or
    Assigned(FFPpkg.FindPackage(PackageName,pkgpkAvailable)) or
    Assigned(FFPpkg.FindPackage(PackageName,pkgpkBoth));

  if not Result then
    begin
    // rescan and try again
    FFppkg.LoadLocalAvailableMirrors;
    FFppkg.ScanPackages;

    Result :=
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkInstalled)) or
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkAvailable)) or
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkBoth));
    end;
end;

procedure TFppkgHelper.ListPackages(AList: TStringList);
var
  I, J: Integer;
  Repository: TFPRepository;
begin
  for I := 0 to FFPpkg.RepositoryList.Count -1 do
    begin
    Repository := FFPpkg.RepositoryList.Items[I] as TFPRepository;
    for J := 0 to Repository.PackageCount -1 do
      begin
      AList.AddObject(Repository.Packages[J].Name, Repository.Packages[J]);
      end;
    end;
end;

finalization
  GFppkgHelper.Free;
end.
