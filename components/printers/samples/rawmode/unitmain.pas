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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure PrintString(S:String);
    procedure PrintStream(St:TStream);
    procedure PrintSample;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // fill in the printer list
  Listbox1.Items.Assign(Printer.Printers);
end;

procedure TForm1.PrintString(S: String);
var
  Written: Integer;
begin
  Printer.Write(S[1], Length(S), Written);
end;

const
  MaxBufSize = 256;

procedure TForm1.PrintStream(St: TStream);
var
  Written: Integer;
  Buffer: array[0..MaxBufSize-1] of byte;
begin
  while St.Position<St.Size do begin
    Written := St.Read(Buffer, MaxBufSize);
    Printer.Write(Buffer, Written, Written);
  end;
end;

procedure TForm1.PrintSample;
var
  S: TStringStream;
begin
  // print a plain string
  PrintString('===   FIRST A STRING   ==='+LineEnding);
  PrintString(Memo1.Text);
  PrintString('=== NOW USING A STREAM ==='+LineEnding);
  // print using a stream
  S := TStringStream.Create(Memo1.Text);
  PrintStream(S);
  S.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Listbox1.ItemIndex<0 then begin
    ShowMessage('Select a printer from the list');
    exit;
  end;

  // on a freshly retrieved printer list, either method could
  // be used to select a printer: SetPrinter or PrinterIndex
  //Printer.PrinterIndex := Listbox1.ItemIndex;
  Printer.SetPrinter(ListBox1.Items[Listbox1.ItemIndex]);
  Printer.Title := Caption;
  Printer.RawMode := True;
  Printer.BeginDoc;
  PrintSample;
  Printer.EndDoc;

end;
end.

