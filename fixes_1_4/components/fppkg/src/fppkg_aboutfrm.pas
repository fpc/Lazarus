unit fppkg_aboutfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, fppkg_const;

type

  { TFppkgAboutForm }

  TFppkgAboutForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FppkgAboutForm: TFppkgAboutForm;

implementation

{$R *.lfm}

{ TFppkgAboutForm }

procedure TFppkgAboutForm.FormCreate(Sender: TObject);
begin
  Caption := rsAboutForm;
end;

end.

