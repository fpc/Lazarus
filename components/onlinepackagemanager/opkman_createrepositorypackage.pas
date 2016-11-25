unit opkman_createrepositorypackage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, opkman_createrepositorypackagefr, Forms,
  Controls, Graphics, Dialogs, ExtCtrls;

type

  { TCreateRepositoryPackagesFrm }

  TCreateRepositoryPackagesFrm = class(TForm)
    frCreateRep: TCreateRepositoryPackagefr;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  CreateRepositoryPackagesFrm: TCreateRepositoryPackagesFrm;

implementation

uses opkman_const;
{$R *.lfm}

{ TCreateRepositoryPackagesFrm }

procedure TCreateRepositoryPackagesFrm.FormCreate(Sender: TObject);
begin
  Caption := rsCreateRepositoryPackageFrm_Caption;
  frCreateRep.InitializeFrame;
end;

procedure TCreateRepositoryPackagesFrm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  frCreateRep.FinalizeFrame;
end;

end.

