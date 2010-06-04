unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbRotated: TCheckBox;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    cbLineType: TComboBox;
    cb3D: TCheckBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cb3DChange(Sender: TObject);
    procedure cbLineTypeChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cb3DChange(Sender: TObject);
begin
  with Chart1LineSeries1 do
    Depth := 15 - Depth;
end;

procedure TForm1.cbLineTypeChange(Sender: TObject);
begin
  Chart1LineSeries1.LineType := TLineType(cbLineType.ItemIndex);
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
begin
  with Chart1LineSeries1 do begin
    AxisIndexY := Ord(cbRotated.Checked);
    AxisIndexX := 1 - AxisIndexY;
  end;
end;

end.

