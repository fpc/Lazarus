unit GenericListSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Controls, Forms, StdCtrls, Dialogs, ButtonPanel;

type

  { TGenericListSelectForm }

  TGenericListSelectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListBox: TListBox;
    InfoLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
  private
    procedure UpdateButtons;
  end;

implementation

{$R *.lfm}

{ TGenericListSelectForm }

procedure TGenericListSelectForm.FormShow(Sender: TObject);
begin
  UpdateButtons;
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

