unit GenericCheckList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, CheckLst, LazarusIDEStrConsts;

type

  { TGenericCheckListForm }

  TGenericCheckListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end; 

var
  GenericCheckListForm: TGenericCheckListForm;

implementation

{$R *.lfm}

{ TGenericCheckListForm }

procedure TGenericCheckListForm.FormCreate(Sender: TObject);
begin
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=dlgCancel;
end;

end.

