unit ListViewTestFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type
  
  { TMyForm }

  TMyForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    ListView: TListView;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    FItemIndex: Cardinal;
  public

  end;

var
  MyForm: TMyForm;

implementation

{$R *.lfm}

{ TMyForm }

procedure TMyForm.Button1Click(Sender: TObject);
var
  Item: TListItem;
begin                                        
  Inc(FItemIndex);
  Item := ListView.Items.Add;
  Item.Caption := Format('Item %d', [FItemIndex]);
  Item.SubItems.Add(Format('Sub %d.1', [FItemIndex]));
  Item.SubItems.Add(Format('Sub %d.2', [FItemIndex]));
end;

procedure TMyForm.Button2Click(Sender: TObject);
begin
  ListView.Selected.Free;
end;

procedure TMyForm.Edit1Change(Sender: TObject);
begin
  if ListView.Selected = nil then Exit;
  ListView.Selected.Caption := Edit1.Text;
end;

procedure TMyForm.Edit2Change(Sender: TObject);
begin
  if ListView.Selected = nil then Exit;
  ListView.Selected.SubItems[0] := Edit2.Text; 
end;

end.

