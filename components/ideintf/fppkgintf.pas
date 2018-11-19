unit FppkgIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TFppkgInterface }

  TFppkgInterface = class
  protected
    function GetUseFPMakeWhenPossible: Boolean; virtual; abstract;
    function GetInstallFPMakeDependencies: Boolean; virtual; abstract;
  public
    property InstallFPMakeDependencies: Boolean read GetInstallFPMakeDependencies;
    property UseFPMakeWhenPossible: Boolean read GetUseFPMakeWhenPossible;
  end;

var
  FppkgInterface: TFppkgInterface = nil;

implementation

{ TFppkgInterface }

end.

