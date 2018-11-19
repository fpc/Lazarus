unit Fppkg_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FppkgIntf,
  Fppkg_EnvironmentOptions;

type

  { TFppkgInterfaceEx }

  TFppkgInterfaceEx = class(TFppkgInterface)
  protected
    function GetInstallFPMakeDependencies: Boolean; override;
    function GetUseFPMakeWhenPossible: Boolean; override;
  end;

implementation

{ TFppkgInterfaceEx }

function TFppkgInterfaceEx.GetInstallFPMakeDependencies: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).InstallFPMakeDependencies;
end;

function TFppkgInterfaceEx.GetUseFPMakeWhenPossible: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).UseFPMakeWhenPossible;
end;

initialization
  FppkgInterface := TFppkgInterfaceEx.Create;
finalization
  FreeAndNil(FppkgInterface);
end.

