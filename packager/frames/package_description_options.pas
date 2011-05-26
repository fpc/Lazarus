unit package_description_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, Controls, StdCtrls, Spin, LCLProc,
  PackageIntf, IDEOptionsIntf, LazarusIDEStrConsts, PackageDefs, PackageSystem,
  BrokenDependenciesDlg;

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
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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
  LazPackage: TLazPackage absolute AOptions;
begin
  DescriptionMemo.Text := LazPackage.Description;
  AuthorEdit.Text := LazPackage.Author;
  LicenseMemo.Text := LazPackage.License;
  //debugln(['TPackageDescriptionOptionsFrame.ReadSettings ',lazpa]);
  VersionMajorSpinEdit.Value := LazPackage.Version.Major;
  VersionMinorSpinEdit.Value := LazPackage.Version.Minor;
  VersionReleaseSpinEdit.Value := LazPackage.Version.Release;
  VersionBuildSpinEdit.Value := LazPackage.Version.Build;
end;

procedure TPackageDescriptionOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
  NewVersion: TPkgVersion;
  BrokenDependencies: TFPList;
  RenameDependencies: boolean;
  MsgResult: TModalResult;
begin
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
    BrokenDependencies := PackageGraph.GetBrokenDependenciesWhenChangingPkgID(LazPackage, LazPackage.Name, NewVersion);
    RenameDependencies := False;
    try
      if BrokenDependencies.Count > 0 then
      begin
        MsgResult := ShowBrokenDependencies(BrokenDependencies, DefaultBrokenDepButtons);
        if MsgResult = mrYes then
          RenameDependencies := True
        else if MsgResult = mrNo then
          RenameDependencies := False
        else
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
  Result := TLazPackage;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageDescriptionOptionsFrame,
    PackageOptionsDescription);
end.

