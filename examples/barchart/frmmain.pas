unit frmmain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, chart;

type
  TForm1 = class(TForm)
    BarChart1: TBarChart;
    procedure Form1Show(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Form1Show(Sender: TObject);
begin
  BarChart1.AddBar('1999',200,clGreen);
  BarChart1.AddBar('2000',100,clred);
  BarChart1.AddBar('2001',300,clred);
  BarChart1.AddBar('2003',400,clGreen);
  BarChart1.AddBar('2004',400,clGreen);
end;

initialization
  {$I frmmain.lrs}

end.

