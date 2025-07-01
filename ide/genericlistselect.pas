unit GenericListSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, Forms, Dialogs, StdCtrls, ButtonPanel, LCLType;

type
  TGenericListSelectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListBox: TListBox;
    InfoLabel: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
  private
    procedure UpdateButtons;
  end;

implementation

{$R *.lfm}

procedure TGenericListSelectForm.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TGenericListSelectForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TGenericListSelectForm.ListBoxClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TGenericListSelectForm.ListBoxDblClick(Sender: TObject);
begin
  if ListBox.ItemIndex >= 0 then
    ModalResult := mrOK;
end;

procedure TGenericListSelectForm.UpdateButtons;
begin
  ButtonPanel1.OKButton.Enabled := ListBox.ItemIndex >= 0;
end;

end.

