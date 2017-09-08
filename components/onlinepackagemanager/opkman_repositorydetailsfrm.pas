unit opkman_repositorydetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  { TRepositoryDetailsFrm }

  TRepositoryDetailsFrm = class(TForm)
    bOk: TButton;
    bCancel: TButton;
    cbAddToRepositories: TCheckBox;
    edName: TEdit;
    edAddress: TEdit;
    lbName: TLabel;
    lbAddress: TLabel;
    lbDescription: TLabel;
    mDescription: TMemo;
    pnButtons: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;

implementation

{$R *.lfm}

{ TRepositoryDetailsFrm }

procedure TRepositoryDetailsFrm.FormCreate(Sender: TObject);
begin

end;

end.

