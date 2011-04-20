// NOTE: This is very experimental code. Do not expect it to work!

unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  FPVectorial, SVGVectorialWriter, TADrawerFPVectorial;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  d: TvVectorialDocument;
begin
  d := TvVectorialDocument.Create;
  d.Width := Chart1.Width;
  d.Height := Chart1.Height;
  with Chart1 do
    Draw(TFPVectorialDrawer.Create(d), Rect(0, 0, Width, Height));
  d.WriteToFile('test.svg', vfSVG);
end;

end.

