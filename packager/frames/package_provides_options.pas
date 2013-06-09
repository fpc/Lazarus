unit Package_Provides_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  IDEOptionsIntf, LazarusIDEStrConsts, PackageDefs;

type

  { TPackageProvidesOptionsFrame }

  TPackageProvidesOptionsFrame = class(TAbstractIDEOptionsEditor)
    ProvidesGroupBox: TGroupBox;
    ProvidesMemo: TMemo;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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
  LazPackage: TLazPackage absolute AOptions;
begin
  ProvidesMemo.Lines.Assign(LazPackage.Provides);
end;

procedure TPackageProvidesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  LazPackage.Provides := ProvidesMemo.Lines;
end;

class function TPackageProvidesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TLazPackage;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageProvidesOptionsFrame,
    PackageOptionsProvides);
end.

