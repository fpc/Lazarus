unit package_i18n_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  IDEOptionsIntf, LazarusIDEStrConsts, PackageDefs, IDEDialogs;

type

  { TPackageI18NOptionsFrame }

  TPackageI18NOptionsFrame = class(TAbstractIDEOptionsEditor)
    EnableI18NCheckBox: TCheckBox;
    I18NGroupBox: TGroupBox;
    POOutDirButton: TButton;
    POOutDirEdit: TEdit;
    PoOutDirLabel: TLabel;
    PoForFormsCheckBox: TCheckBox;
    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure POOutDirButtonClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPackageI18NOptionsFrame }

procedure TPackageI18NOptionsFrame.EnableI18NCheckBoxChange(Sender: TObject);
begin
  I18NGroupBox.Enabled := EnableI18NCheckBox.Checked;
end;

procedure TPackageI18NOptionsFrame.POOutDirButtonClick(Sender: TObject);
var
  NewDirectory: string;
begin
  NewDirectory := LazSelectDirectory(lisPOChoosePoFileDirectory,
    FLazPackage.Directory);
  if NewDirectory = '' then
    exit;
  FLazPackage.ShortenFilename(NewDirectory, True);
  POOutDirEdit.Text := NewDirectory;
end;

function TPackageI18NOptionsFrame.GetTitle: string;
begin
  Result := dlgPOI18n;
end;

procedure TPackageI18NOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  EnableI18NCheckBox.Caption := rsEnableI18n;
  I18NGroupBox.Caption := rsI18nOptions;
  PoOutDirlabel.Caption := rsPOOutputDirectory;
  PoForFormsCheckBox.Caption:=lisCreateUpdatePoFileWhenSavingALfmFile;
  PoForFormsCheckBox.Hint:=
    lisYouCanDisableThisForIndividualFormsViaThePackageEd;
end;

procedure TPackageI18NOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  FLazPackage := LazPackage;
  EnableI18NCheckBox.Checked := LazPackage.EnableI18N;
  I18NGroupBox.Enabled := LazPackage.EnableI18N;
  POOutDirEdit.Text := LazPackage.POOutputDirectory;
  PoForFormsCheckBox.Checked:=LazPackage.EnableI18NForLFM;
end;

procedure TPackageI18NOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage absolute AOptions;
begin
  LazPackage.EnableI18N := EnableI18NCheckBox.Checked;
  LazPackage.POOutputDirectory := POOutDirEdit.Text;
  LazPackage.EnableI18NForLFM := PoForFormsCheckBox.Checked;
end;

class function TPackageI18NOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TLazPackage;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageI18NOptionsFrame,
    PackageOptionsI18N);
end.

