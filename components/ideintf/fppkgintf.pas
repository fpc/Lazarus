unit FppkgIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  PackageIntf;

type

  { TFppkgInterface }

  TFppkgInterface = class
  protected
    function GetUseFPMakeWhenPossible: Boolean; virtual; abstract;
    function GetInstallFPMakeDependencies: Boolean; virtual; abstract;
  public
    function ConstructFpMakeInterfaceSection(APackage: TIDEPackage): string; virtual; abstract;
    function ConstructFpMakeImplementationSection(APackage: TIDEPackage): string; virtual; abstract;
    function ConstructFpMakeDependenciesFileSection(APackage: TIDEPackage): string; virtual; abstract;
    property InstallFPMakeDependencies: Boolean read GetInstallFPMakeDependencies;
    property UseFPMakeWhenPossible: Boolean read GetUseFPMakeWhenPossible;
  end;

var
  FppkgInterface: TFppkgInterface = nil;

implementation

{ TFppkgInterface }

end.

