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
    btnZPL: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure btnZPLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure PrintString(S:String);
    procedure PrintStream(St:TStream);
    procedure PrintSample;
    procedure PrintZebraSample;
    function SetCurrentPrinter: boolean;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

const
  CRLF = #13#10;

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

procedure TForm1.PrintZebraSample;
var
  s: String;
begin
  // sample code taken from the Online ZPL viewer
  // http://labelary.com/viewer.html
  s :=
    '^XA' + CRLF +
    '^FX Top section with company logo, name and address.' + CRLF +
    '^CF0,60' + CRLF +
    '^FO50,50^GB100,100,100^FS' + CRLF +
    '^FO75,75^FR^GB100,100,100^FS' + CRLF +
    '^FO88,88^GB50,50,50^FS' + CRLF +
    '^FO220,50^FDInternational Shipping, Inc.^FS' + CRLF +
    '^CF0,40' + CRLF +
    '^FO220,100^FD1000 Shipping Lane^FS' + CRLF +
    '^FO220,135^FDShelbyville TN 38102^FS' + CRLF +
    '^FO220,170^FDUnited States (USA)^FS' + CRLF +
    '^FO50,250^GB700,1,3^FS' + CRLF +
    '^FX Second section with recipient address and permit information.' + CRLF +
    '^CFA,30' + CRLF +
    '^FO50,300^FDJohn Doe^FS' + CRLF +
    '^FO50,340^FD100 Main Street^FS' + CRLF +
    '^FO50,380^FDSpringfield TN 39021^FS' + CRLF +
    '^FO50,420^FDUnited States (USA)^FS' + CRLF +
    '^CFA,15' + CRLF +
    '^FO600,300^GB150,150,3^FS' + CRLF +
    '^FO638,340^FDPermit^FS' + CRLF +
    '^FO638,390^FD123456^FS' + CRLF +
    '^FO50,500^GB700,1,3^FS' + CRLF +
    '^FX Third section with barcode.' + CRLF +
    '^BY5,2,270' + CRLF +
    '^FO100,550^BC^FD12345678^FS' + CRLF +
    '^FX Fourth section (the two boxes on the bottom).' + CRLF +
    '^FO50,900^GB700,250,3^FS' + CRLF +
    '^FO400,900^GB1,250,3^FS' + CRLF +
    '^CF0,40' + CRLF +
    '^FO100,960^FDShipping Ctr. X34B-1^FS' + CRLF +
    '^FO100,1010^FDREF1 F00B47^FS' + CRLF +
    '^FO100,1060^FDREF2 BL4H8^FS' + CRLF +
    '^CF0,190' + CRLF +
    '^FO485,965^FDCA^FS' + CRLF +
    '^XZ' + CRLF;
  PrintString(s);
end;

function TForm1.SetCurrentPrinter: boolean;
begin
  result := false;
  if Listbox1.ItemIndex<0 then begin
    ShowMessage('Select a printer from the list');
    exit;
  end;

  // on a freshly retrieved printer list, either method could
  // be used to select a printer: SetPrinter or PrinterIndex
  //Printer.PrinterIndex := Listbox1.ItemIndex;
  Printer.SetPrinter(ListBox1.Items[Listbox1.ItemIndex]);
  result := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not SetCurrentPrinter then
    exit;

  Printer.Title := Caption;
  Printer.RawMode := True;
  Printer.BeginDoc;
  PrintSample;
  Printer.EndDoc;
end;

procedure TForm1.btnZPLClick(Sender: TObject);
begin
  if not SetCurrentPrinter then
    exit;

  Printer.Title := 'Zebra test';
  Printer.RawMode := True;
  Printer.BeginDoc;
  PrintZebraSample;
  Printer.EndDoc;
end;

end.

