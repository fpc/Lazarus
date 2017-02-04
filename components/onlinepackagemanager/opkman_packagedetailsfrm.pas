unit opkman_packagedetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TPackageDetailsFrm }

  TPackageDetailsFrm = class(TForm)
    bOk: TButton;
    mDetails: TMemo;
    pnButtons: TPanel;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  PackageDetailsFrm: TPackageDetailsFrm;

implementation

{$R *.lfm}

{ TPackageDetailsFrm }

procedure TPackageDetailsFrm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close;
end;

end.

