unit GenericListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, LazarusIDEStrConsts;

type

  { TGenericListEditForm }

  TGenericListEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender:TObject);
  private

  public

  end; 

var
  GenericListEditForm: TGenericListEditForm;

implementation

{$R *.lfm}

{ TGenericListEditForm }

procedure TGenericListEditForm.FormCreate(Sender:TObject);
begin
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
end;

end.

