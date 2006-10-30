unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Printers;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // fill in the printer list
  Listbox1.Items.Assign(Printer.Printers);
end;

procedure PrintRawString(const S:String);
var
  Written: Integer;
begin
  Printer.BeginDoc;
  Printer.Write(S[1], Length(S), Written);
  Printer.EndDoc;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Listbox1.ItemIndex<0 then begin
    ShowMessage('Select a printer from the list');
    exit;
  end;
  if Edit1.Text='' then begin
    ShowMessage('There is nothing to print!');
    exit;
  end;
  Printer.PrinterIndex := Listbox1.ItemIndex;
  Printer.Title := Caption;
  Printer.RawMode := True;
  PrintRawString(Edit1.Text);
end;

initialization
  {$I unitmain.lrs}

end.

