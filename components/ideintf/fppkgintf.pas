unit FppkgIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TFppkgInterface = class
  protected
    function GetInstallFPMakeDependencies: Boolean; virtual; abstract;
  public
    property InstallFPMakeDependencies: Boolean read GetInstallFPMakeDependencies;
  end;

var
  FppkgInterface: TFppkgInterface = nil;

implementation

end.

