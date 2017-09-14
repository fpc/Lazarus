unit opkman_addrepositorypackagefrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus;

type

  { TAddRepositoryPackageFrm }

  TAddRepositoryPackageFrm = class(TForm)
    bCancel: TButton;
    bOk: TButton;
    ODPack: TOpenDialog;
    pnButtons: TPanel;
    rbCreateNew: TRadioButton;
    rbAddExisting: TRadioButton;
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FJSONFile: String;
    FPackageFile: String;
  public
    property JSONFile: String read FJSONFile;
    property PackageFile: String read FPackageFile;
  end;

var
  AddRepositoryPackageFrm: TAddRepositoryPackageFrm;

implementation

uses opkman_const, opkman_options, opkman_common;
{$R *.lfm}

{ TAddRepositoryPackageFrm }

procedure TAddRepositoryPackageFrm.FormCreate(Sender: TObject);
begin
  Caption := rsAddRepositoryPackageFrm_Caption;
  rbCreateNew.Caption := rsAddRepositoryPackageFrm_rbCreateNew_Caption;
  rbAddExisting.Caption := rsAddRepositoryPackageFrm_rbAddExisting_Caption;
  bOk.Caption := rsAddRepositoryPackageFrm_bOk_Caption;
  bOk.Hint := rsAddRepositoryPackageFrm_bOk_Hint;
  bCancel.Caption := rsAddRepositoryPackageFrm_bCancel_Caption;
  bCancel.Hint := rsAddRepositoryPackageFrm_bCancel_Hint;
end;

procedure TAddRepositoryPackageFrm.bOkClick(Sender: TObject);
begin
  if rbAddExisting.Checked then
  begin
    ODPack.InitialDir := Options.LastPackagedirDst;
    if ODPack.Execute then
    begin
      FJSONFile := ODPack.FileName;
      FPackageFile := ChangeFileExt(FJSONFile, '.zip');
      if not FileExists(FPackageFile) then
      begin
        MessageDlgEx(Format(rsCreateRepositoryFrm_Info5, [ExtractFileName(FPackageFile)]), mtInformation, [mbOk], Self);
        Exit;
      end;
    end
    else
      Exit;
  end;
  ModalResult := mrOk;
end;

end.

