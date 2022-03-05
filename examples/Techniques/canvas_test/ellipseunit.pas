unit ellipseunit;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics;

type

  { TfrmEllipse }

  TfrmEllipse = class(TForm)
    procedure FormPaint(Sender: TObject);
  private

  public

  end; 

var
  frmEllipse: TfrmEllipse;

implementation

{$R *.lfm}

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

  Canvas.TextOut(50, 215, 'Arc SX=0 SY=0 EX=300 EY=300');
  Canvas.TextOut(150, 230, 'Arc SX=300 SY=300 EX=0 EY=0');
  //Canvas.TextOut(250, 230, 'Arc S=30 L=-45');

  Canvas.Brush.Style := bsSolid;

  Canvas.Ellipse(50, 50, 100, 100);
  Canvas.Chord(150, 50, 200, 100, 0, 90*16);
  Canvas.RadialPie(250, 50, 300, 100, 0, 90*16);

  Canvas.Arc(50, 150, 100, 200, 0, 90*16);
  Canvas.Arc(150, 150, 200, 200, -25*16, 45*16);
  Canvas.Arc(250, 150, 300, 200, 30*16, -45*16);

  Canvas.Arc(50, 250, 100, 300, 0, 0, 300, 300);
  Canvas.Arc(150, 250, 200, 300, 300, 300, 0, 0);
  //Canvas.Arc(250, 150, 300, 200, 30*16, -45*16);
end;

end.

