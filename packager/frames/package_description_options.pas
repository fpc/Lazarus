unit package_description_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, Spin, LCLProc,
  // IdeIntf
  PackageDependencyIntf, IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, PackageDefs, PackageSystem, BrokenDependenciesDlg;

type

  { TPackageDescriptionOptionsFrame }

  TPackageDescriptionOptionsFrame = class(TAbstractIDEOptionsEditor)
    AuthorEdit: TEdit;
    AuthorGroupBox: TGroupBox;
    DescriptionGroupBox: TGroupBox;
    DescriptionMemo: TMemo;
    dummyForSizing: TLabel;
    LicenseGroupBox: TGroupBox;
    LicenseMemo: TMemo;
    VersionBuildLabel: TLabel;
    VersionBuildSpinEdit: TSpinEdit;
    VersionGroupBox: TGroupBox;
    VersionMajorLabel: TLabel;
    VersionMajorSpinEdit: TSpinEdit;
    VersionMinorLabel: TLabel;
    VersionMinorSpinEdit: TSpinEdit;
    VersionReleaseLabel: TLabel;
    VersionReleaseSpinEdit: TSpinEdit;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageDescriptionOptionsFrame }

function TPackageDescriptionOptionsFrame.GetTitle: string;
begin
  Result := lisCodeHelpDescrTag;
end;

procedure TPackageDescriptionOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  DescriptionGroupBox.Caption := lisPckOptsDescriptionAbstract;
  AuthorGroupBox.Caption := lisPckOptsAuthor;
  LicenseGroupBox.Caption := lisPckOptsLicense;
  VersionGroupBox.Caption := lisVersion;
  VersionMajorLabel.Caption := lisPckOptsMajor;
  VersionMinorLabel.Caption := lisPckOptsMinor;
  VersionReleaseLabel.Caption := lisPckOptsRelease;
  VersionBuildLabel.Caption := lisBuildNumber;
end;

procedure TPackageDescriptionOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  DescriptionMemo.Text := LazPackage.Description;
  AuthorEdit.Text := LazPackage.Author;
  LicenseMemo.Text := LazPackage.License;
  VersionMajorSpinEdit.Value := LazPackage.Version.Major;
  VersionMinorSpinEdit.Value := LazPackage.Version.Minor;
  VersionReleaseSpinEdit.Value := LazPackage.Version.Release;
  VersionBuildSpinEdit.Value := LazPackage.Version.Build;
end;

procedure TPackageDescriptionOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
  NewVersion: TPkgVersion;
  BrokenDependencies: TFPList;
  RenameDependencies: boolean;
  MsgResult: TModalResult;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  LazPackage.Description := DescriptionMemo.Text;
  LazPackage.Author := AuthorEdit.Text;
  LazPackage.License := LicenseMemo.Text;
  NewVersion := TPkgVersion.Create;
  try
    NewVersion.Major := RoundToInt(VersionMajorSpinEdit.Value);
    NewVersion.Minor := RoundToInt(VersionMinorSpinEdit.Value);
    NewVersion.Release := RoundToInt(VersionReleaseSpinEdit.Value);
    NewVersion.Build := RoundToInt(VersionBuildSpinEdit.Value);

    // check for broken dependencies
    BrokenDependencies := PackageGraph.GetBrokenDependenciesWhenChangingPkgID(LazPackage,
                                                    LazPackage.Name, NewVersion);
    RenameDependencies := False;
    try
      if BrokenDependencies.Count > 0 then
      begin
        MsgResult := ShowBrokenDependencies(BrokenDependencies);
        if MsgResult = mrOK then            // = Yes
          RenameDependencies := True
        else if MsgResult <> mrClose then   // <> Ignore
          exit;
      end;
    finally
      BrokenDependencies.Free;
    end;

    PackageGraph.ChangePackageID(LazPackage, LazPackage.Name, NewVersion, RenameDependencies, True);
  finally
    NewVersion.Free;
  end;
end;

class function TPackageDescriptionOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TPackageIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageDescriptionOptionsFrame,
    PackageOptionsDescription);
end.

