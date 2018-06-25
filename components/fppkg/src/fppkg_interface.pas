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
  end;

implementation

{ TFppkgInterfaceEx }

function TFppkgInterfaceEx.GetInstallFPMakeDependencies: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).InstallFPMakeDependencies;
end;

initialization
  FppkgInterface := TFppkgInterfaceEx.Create;
finalization
  FreeAndNil(FppkgInterface);
end.

