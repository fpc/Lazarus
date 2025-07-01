unit GenericListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, ButtonPanel, LCLType;

type
  TGenericListEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{$R *.lfm}

{ TGenericListEditForm }

procedure TGenericListEditForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    if ButtonPanel1.OKButton.IsEnabled then
    begin
      Key := 0;
      ModalResult := mrOK;
    end;
  end;
end;

end.

