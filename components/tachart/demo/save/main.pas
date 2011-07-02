unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, Dialogs, ComCtrls, TAGraph, TASeries, TAFuncSeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1FuncSeries1: TFuncSeries;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    tbSaveAsBMP: TToolButton;
    tbSaveAsPNG: TToolButton;
    tbCopyToClipboard: TToolButton;
    tbSaveAsJPEG: TToolButton;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
    procedure tbCopyToClipboardClick(Sender: TObject);
    procedure tbSaveAsBMPClick(Sender: TObject);
    procedure tbSaveAsJPEGClick(Sender: TObject);
    procedure tbSaveAsPNGClick(Sender: TObject);
  private
    function GetFileName(const AExt: String): String;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := AX * AX / 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RandSeed := 103489;
  for i := 1 to 10 do
    Chart1BarSeries1.AddXY(i, i * i / 2 + Random(6) + 1 + Random);
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
end;

function TForm1.GetFileName(const AExt: String): String;
begin
  with SaveDialog1 do begin
    FileName := '';
    DefaultExt := AExt;
    if not Execute then Abort;
    Result := FileName;
  end;
end;

procedure TForm1.tbCopyToClipboardClick(Sender: TObject);
begin
  Chart1.CopyToClipboardBitmap;
end;

procedure TForm1.tbSaveAsBMPClick(Sender: TObject);
begin
  Chart1.SaveToBitmapFile(GetFileName('bmp'));
end;

procedure TForm1.tbSaveAsJPEGClick(Sender: TObject);
begin
  Chart1.SaveToFile(TJPEGImage, GetFileName('jpg'));
end;

procedure TForm1.tbSaveAsPNGClick(Sender: TObject);
begin
  Chart1.SaveToFile(TPortableNetworkGraphic, GetFileName('png'));
end;

end.

