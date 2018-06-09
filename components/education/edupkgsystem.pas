{
 *****************************************************************************
  This file is part of the EducationLaz package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Options for Lazarus package system.
}
unit EduPkgSystem;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  LResources, Forms, Controls, StdCtrls,
  // LazUtils
  LazConfigStorage,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // Education
  EduOptions;

type

  { TEduPkgSystemOptions }

  TEduPkgSystemOptions = class(TEduOptionsNode)
  private
    FHideConfigureInstalledPkgs: boolean;
    FHideCreatePackage: boolean;
    FHideOpenPackage: boolean;
    FHidePackageGraph: boolean;
    procedure SetHideConfigureInstalledPkgs(const AValue: boolean);
    procedure SetHideCreatePackage(const AValue: boolean);
    procedure SetHideOpenPackage(const AValue: boolean);
    procedure SetHidePackageGraph(const AValue: boolean);
  public
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    property HideCreatePackage: boolean read FHideCreatePackage write SetHideCreatePackage;
    property HideOpenPackage: boolean read FHideOpenPackage write SetHideOpenPackage;
    property HidePackageGraph: boolean read FHidePackageGraph write SetHidePackageGraph;
    property HideConfigureInstalledPkgs: boolean read FHideConfigureInstalledPkgs write SetHideConfigureInstalledPkgs;
  end;

  { TEduPkgSystemFrame }

  TEduPkgSystemFrame = class(TAbstractIDEOptionsEditor)
    HideCreatePackageCheckBox: TCheckBox;
    HideOpenPackageCheckBox: TCheckBox;
    HidePackageGraphCheckBox: TCheckBox;
    HideConfigureInstalledPkgsCheckBox: TCheckBox;
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  //EducationOptions.Root.Add(TEduPkgSystemOptions.Create);
  //RegisterIDEOptionsEditor(GroupEducation,TEduPkgSystemFrame,EduOptionPackagesID);
end;

{ TEduPkgSystemFrame }

function TEduPkgSystemFrame.GetTitle: String;
begin
  Result:='Packages';
end;

procedure TEduPkgSystemFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  HideCreatePackageCheckBox.Caption:='Hide menu items to create new packages';
  HideOpenPackageCheckBox.Caption:='Hide menu items to open package';
  HidePackageGraphCheckBox.Caption:='Hide menu item package graph';
  HideConfigureInstalledPkgsCheckBox.Caption:='Hide "Configure installed packages"';
end;

procedure TEduPkgSystemFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
  end;
end;

procedure TEduPkgSystemFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
  end;
end;

class function TEduPkgSystemFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

{ TEduPkgSystemOptions }

procedure TEduPkgSystemOptions.SetHideConfigureInstalledPkgs(
  const AValue: boolean);
begin
  if FHideConfigureInstalledPkgs=AValue then exit;
  FHideConfigureInstalledPkgs:=AValue;
  Changed;
end;

procedure TEduPkgSystemOptions.SetHideCreatePackage(const AValue: boolean);
begin
  if FHideCreatePackage=AValue then exit;
  FHideCreatePackage:=AValue;
  Changed;
end;

procedure TEduPkgSystemOptions.SetHideOpenPackage(const AValue: boolean);
begin
  if FHideOpenPackage=AValue then exit;
  FHideOpenPackage:=AValue;
  Changed;
end;

procedure TEduPkgSystemOptions.SetHidePackageGraph(const AValue: boolean);
begin
  if FHidePackageGraph=AValue then exit;
  FHidePackageGraph:=AValue;
  Changed;
end;

function TEduPkgSystemOptions.Load(Config: TConfigStorage): TModalResult;
begin
  FHideConfigureInstalledPkgs:=Config.GetValue('HideConfigureInstalledPackages',false);
  FHideCreatePackage:=Config.GetValue('HideCreatePackage',false);
  FHideOpenPackage:=Config.GetValue('HideOpenPackage',false);
  FHidePackageGraph:=Config.GetValue('HidePackageGraph',false);
  Result:=inherited Load(Config);
end;

function TEduPkgSystemOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('HideConfigureInstalledPackages',FHideConfigureInstalledPkgs,false);
  Config.SetDeleteValue('HideCreatePackage',FHideCreatePackage,false);
  Config.SetDeleteValue('HideOpenPackage',FHideOpenPackage,false);
  Config.SetDeleteValue('HidePackageGraph',FHidePackageGraph,false);
  Result:=inherited Save(Config);
end;

{$R *.lfm}

end.
