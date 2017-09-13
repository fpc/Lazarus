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
    pnButtons: TPanel;
    rbCreateNew: TRadioButton;
    rbAddExisting: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AddRepositoryPackageFrm: TAddRepositoryPackageFrm;

implementation
uses opkman_const;
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

end.

