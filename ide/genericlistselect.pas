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
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  GenericListSelectForm: TGenericListSelectForm;

implementation

{$R *.lfm}

{ TGenericListSelectForm }

constructor TGenericListSelectForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  InfoLabel.Caption := '';
end;

destructor TGenericListSelectForm.Destroy;
begin
  inherited Destroy;
end;

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
  if ListBox.ItemIndex > -1 then
    ModalResult:=mrOK;
end;

procedure TGenericListSelectForm.UpdateButtons;
begin
  ButtonPanel1.OKButton.Enabled := ListBox.ItemIndex >= 0;
  //ListBox.Selected[1];
end;

end.

