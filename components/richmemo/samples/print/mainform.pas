unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  Printers,
  StdCtrls, RichMemo, RichMemoUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    PageSetupDialog1: TPageSetupDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure RichMemo1PrintAction(Sender: TObject;
      APrintAction: TPrintAction; PrintCanvas: TCanvas;
      CurrentPage: Integer; var AbortPrint: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadRTFFile(RichMemo1, OpenDialog1.FileName);
end;


procedure PageSetupToMargins(pg: TPageSetupDialog; var p: TPrintParams);
var
  md : Single; // modifies to tunr into inches
  pt : Single;
begin
  if pg.Units=unMM then md:=0.254
  else md:=0.001;

  p.Margins.Left:=pg.Margins.Left*md*72;
  p.Margins.Top:=pg.Margins.Top*md*72;
  p.Margins.Right:=-pg.Margins.Right*md*72;
  p.Margins.Bottom:=pg.Margins.Bottom*md*72;

  writeln(p.Margins.Left:0:2);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  prm : TPrintParams;
begin
  if not Assigned(Printer) then begin
    ShowMessage('No printer found.');
    Exit;
  end;
  InitPrintParams(prm);
  prm.SelStart:=RichMemo1.SelStart;
  prm.SelLength:=RichMemo1.SelLength;
  prm.JobTitle:='Rich Memo Printing';
  PageSetupToMargins(PageSetupDialog1, prm);

  RichMemo1.Print(prm);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  PageSetupDialog1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichMemo1.OnPrintAction:=@RichMemo1PrintAction;
end;

procedure TForm1.RichMemo1PrintAction(Sender: TObject;
  APrintAction: TPrintAction; PrintCanvas: TCanvas; CurrentPage: Integer;
  var AbortPrint: Boolean);
begin
  if APrintAction=paPageStart then begin
    PrintCanvas.Brush.Color:=clBlue;
    PrintCanvas.Brush.Style:=bsSolid;
    PrintCanvas.Ellipse(100,100,200,200);
  end;
  writeln('action: ', APrintAction);
end;

end.

