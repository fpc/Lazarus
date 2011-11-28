unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs; 

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clRed;
  Canvas.Rectangle(10, 10, 100, 100);
  Canvas.Brush.Color := clGreen;
  Canvas.Rectangle(100, 100, 200, 200);
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(200, 200, 300, 300);
end;

end.

