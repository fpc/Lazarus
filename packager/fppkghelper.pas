unit FppkgHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$IFNDEF VER3_0}
  pkgFppkg,
  {$ENDIF VER3_0}
  fprepos;

type

  { TFppkgHelper }

  TFppkgHelper = class
  private
    {$IFNDEF VER3_0}
    FFPpkg: TpkgFPpkg;
    {$ENDIF}
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
{$IFNDEF VER3_0}
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
{$ELSE}
begin

end;
{$ENDIF VER3_0}

destructor TFppkgHelper.Destroy;
begin
{$IFNDEF VER3_0}
  FFPpkg.Free;
{$ENDIF VER3_0}
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
{$IFNDEF VER3_0}
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
{$ELSE }
  Result := True;
{$ENDIF VER3_0}
end;

procedure TFppkgHelper.ListPackages(AList: TStringList);
{$IFNDEF VER3_0}
var
  I, J: Integer;
  Repository: TFPRepository;
{$ENDIF VER3_0}
begin
{$IFNDEF VER3_0}
  for I := 0 to FFPpkg.RepositoryList.Count -1 do
    begin
    Repository := FFPpkg.RepositoryList.Items[I] as TFPRepository;
    for J := 0 to Repository.PackageCount -1 do
      begin
      AList.AddObject(Repository.Packages[J].Name, Repository.Packages[J]);
      end;
    end;
{$ENDIF VER3_0}
end;

finalization
  GFppkgHelper.Free;
end.
