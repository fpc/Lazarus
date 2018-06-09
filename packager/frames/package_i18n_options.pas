unit package_i18n_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LazUtils
  FileUtil,
  // LCL
  Forms, StdCtrls, EditBtn,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, PackageDefs;

type

  { TPackageI18NOptionsFrame }

  TPackageI18NOptionsFrame = class(TAbstractIDEOptionsEditor)
    EnableI18NCheckBox: TCheckBox;
    I18NGroupBox: TGroupBox;
    POOutDirEdit: TEditButton;
    PoOutDirLabel: TLabel;
    PoForFormsCheckBox: TCheckBox;
    procedure EnableI18NCheckBoxChange(Sender: TObject);
    procedure POOutDirButtonClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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
  PoForFormsCheckBox.Caption := lisCreateUpdatePoFileWhenSavingALfmFile;
  PoForFormsCheckBox.Hint := lisYouCanDisableThisForIndividualFormsViaThePackageEd;
end;

procedure TPackageI18NOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FLazPackage := (AOptions as TPackageIDEOptions).Package;
  EnableI18NCheckBox.Checked := FLazPackage.EnableI18N;
  I18NGroupBox.Enabled := FLazPackage.EnableI18N;
  POOutDirEdit.Text := FLazPackage.POOutputDirectory;
  PoForFormsCheckBox.Checked:=FLazPackage.EnableI18NForLFM;

  IDEImages.AssignImage(POOutDirEdit.Button, ResBtnSelDir); //DirectoryEdit
end;

procedure TPackageI18NOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TLazPackage;
begin
  LazPackage := (AOptions as TPackageIDEOptions).Package;
  LazPackage.EnableI18N := EnableI18NCheckBox.Checked;
  LazPackage.POOutputDirectory := POOutDirEdit.Text;
  LazPackage.EnableI18NForLFM := PoForFormsCheckBox.Checked;
end;

class function TPackageI18NOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TPackageIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TPackageI18NOptionsFrame,
    PackageOptionsI18N);
end.

