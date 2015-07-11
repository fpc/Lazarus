unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  plotpanel;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure CreatePlotter(Sender: TObject);
  private
    { private declarations }
    FPlot:TPlotFunctionPanel;
  public
    { public declarations }
    Procedure PlotResult(Const X : TPlotFloat; Out Y : TPlotFloat);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CreatePlotter(Sender: TObject);
begin
  FPlot:=TPlotFunctionPanel.Create(Self);
  FPlot.parent:=Self;
  FPlot.Align:=alClient;
  Fplot.OnCalcPlot:=@PlotResult;
  FPlot.Active:=True;
end;

procedure TMainForm.PlotResult(const X: TPlotFloat; out Y: TPlotFloat);
begin
  Y:=X*X/100;
end;

end.

