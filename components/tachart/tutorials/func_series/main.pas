unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries, TATools, Forms,
  Controls, Graphics, Dialogs, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1ConstantLine1: TConstantLine;
    Chart1ConstantLine2: TConstantLine;
    Chart1FuncSeries1: TFuncSeries;
    Chart1FuncSeries2: TFuncSeries;
    procedure Chart1AfterDrawBackWall(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect);
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateDomainExclusions;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils;

{ TForm1 }

{ In the trunk version of Lazarus, you can use the event Chart.OnExtentChanging
  for updating the domain exclusions - its name is more intuitive... }
procedure TForm1.Chart1AfterDrawBackWall(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect);
begin
  Unused(ASender);
  Unused(ACanvas);
  Unused(ARect);
  UpdateDomainExclusions;
end;

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := tan(AX);
end;

procedure TForm1.Chart1FuncSeries2Calculate(const AX: Double; out AY: Double);
begin
  AY := ln(AX);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Chart1FuncSeries2.DomainExclusions do begin
    AddRange(NegInfinity, 0.0);
  end;
end;

procedure TForm1.UpdateDomainExclusions;
var
  ex: TDoubleRect;
  x: Integer;
begin
  ex := Chart1.CurrentExtent;
  Chart1.DisableRedrawing;
  try
    with Chart1FuncSeries1.DomainExclusions do begin
      Clear;
      for x := Floor(ex.a.x / Pi - 0.5) to Ceil(ex.b.x / Pi + 0.5) do
        AddPoint((x + 0.5) * Pi);
    end;
  finally
    Chart1.EnableRedrawing;
  end;
end;

end.

