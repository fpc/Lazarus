unit Package_Provides_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  StdCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs;

type

  { TPackageProvidesOptionsFrame }

  TPackageProvidesOptionsFrame = class(TAbstractIDEOptionsEditor)
    ProvidesGroupBox: TGroupBox;
    ProvidesMemo: TMemo;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageProvidesOptionsFrame }

function TPackageProvidesOptionsFrame.GetTitle: string;
begin
  Result := lisPckOptsProvides;
end;

procedure TPackageProvidesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ProvidesGroupBox.Caption := lisPckOptsThisPackageProvidesTheSameAsTheFollowingPackages;
end;

procedure TPackageProvidesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  ProvidesMemo.Lines.Assign(LazPackage.Provides);
end;

procedure TPackageProvidesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  LazPackage.Provides := ProvidesMemo.Lines;
end;

class function TPackageProvidesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TPackageIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageProvidesOptionsFrame,
    PackageOptionsProvides);
end.

