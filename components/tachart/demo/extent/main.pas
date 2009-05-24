unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    cgUseBounds: TCheckGroup;
    Chart1: TChart;
    clRight: TLine;
    clLeft: TLine;
    clTop: TLine;
    clBottom: TLine;
    Chart1LineSeries: TLineSeries;
    fseBounds: TFloatSpinEdit;
    lblBoundValue: TLabel;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cgUseBoundsItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure fseBoundsChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.cgUseBoundsItemClick(Sender: TObject; Index: integer);
begin
  with Chart1.Extent do begin
    UseXMin := cgUseBounds.Checked[0];
    UseXMax := cgUseBounds.Checked[1];
    UseYMin := cgUseBounds.Checked[2];
    UseYMax := cgUseBounds.Checked[3];
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fseBoundsChange(nil);
end;

procedure TForm1.fseBoundsChange(Sender: TObject);
begin
  clRight.Position := fseBounds.Value;
  clLeft.Position := -fseBounds.Value;
  clTop.Position := fseBounds.Value;
  clBottom.Position := -fseBounds.Value;
  with Chart1.Extent do begin
    XMin := -fseBounds.Value;
    XMax := fseBounds.Value;
    YMin := -fseBounds.Value;
    YMax := fseBounds.Value;
  end;
end;

initialization
  {$I main.lrs}

end.

