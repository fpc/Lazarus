unit fppkg_packageoptionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  PackageIntf,
  IDEOptEditorIntf,
  IDEOptionsIntf;

type

  { TFppkgPackageOptionsFrm }

  TFppkgPackageOptionsFrm = class(TAbstractIDEOptionsEditor)
    cbBuildMethod: TComboBox;
    gbBuildMethod: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
  protected
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  public

  end;

implementation

var
  FppkgPackageOptionID: integer = 500;


{$R *.lfm}

resourcestring
  lisFppkgPckOptsTitle = 'Build method';
  lisFppkgPckOptsBuildMethod = 'Supported build methods';
  lisFppkgBuildMethodFPMake = 'FPMake';
  lisFppkgBuildMethodLazarus = 'Lazbuild';
  lisFppkgBuildMethodBoth = 'Both';


{ TFppkgPackageOptionsFrm }

function TFppkgPackageOptionsFrm.GetTitle: String;
begin
  Result := lisFppkgPckOptsTitle;
end;

procedure TFppkgPackageOptionsFrm.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbBuildMethod.Caption := lisFppkgPckOptsBuildMethod;
  cbBuildMethod.Items.Clear;
  cbBuildMethod.Items.Add(lisFppkgBuildMethodLazarus);
  cbBuildMethod.Items.Add(lisFppkgBuildMethodFPMake);
  cbBuildMethod.Items.Add(lisFppkgBuildMethodBoth);
end;

procedure TFppkgPackageOptionsFrm.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TIDEPackage;
begin
  LazPackage := (AOptions as TAbstractPackageIDEOptions).Package;
  cbBuildMethod.ItemIndex := Ord(LazPackage.BuildMethod);
end;

procedure TFppkgPackageOptionsFrm.WriteSettings(AOptions: TAbstractIDEOptions);
var
  LazPackage: TIDEPackage;
begin
  LazPackage := (AOptions as TAbstractPackageIDEOptions).Package;
  LazPackage.BuildMethod := TBuildMethod(cbBuildMethod.ItemIndex);
end;

class function TFppkgPackageOptionsFrm.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAbstractPackageIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupPackage, TFppkgPackageOptionsFrm, FppkgPackageOptionID);
end.

