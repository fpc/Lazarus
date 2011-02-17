unit ellipseunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type

  { TfrmEllipse }

  TfrmEllipse = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmEllipse: TfrmEllipse;

implementation

{ TfrmEllipse }

procedure TfrmEllipse.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clBlue;

  Canvas.Brush.Style := bsClear;

  Canvas.TextOut(50, 30, 'Ellipse');
  Canvas.TextOut(150, 30, 'Chord');
  Canvas.TextOut(250, 30, 'Pie');

  Canvas.TextOut(50, 130, 'Arc S=0 L=90');
  Canvas.TextOut(150, 130, 'Arc S=-25 L=45');
  Canvas.TextOut(250, 130, 'Arc S=30 L=-45');

  Canvas.Brush.Style := bsSolid;

  Canvas.Ellipse(50, 50, 100, 100);
  Canvas.Chord(150, 50, 200, 100, 0, 90*16);
  Canvas.RadialPie(250, 50, 300, 100, 0, 90*16);

  Canvas.Arc(50, 150, 100, 200, 0, 90*16);
  Canvas.Arc(150, 150, 200, 200, -25*16, 45*16);
  Canvas.Arc(250, 150, 300, 200, 30*16, -45*16);
end;

initialization
  {$I ellipseunit.lrs}

end.

