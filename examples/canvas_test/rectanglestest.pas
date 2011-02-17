unit rectanglestest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type

  { TfrmRectangles }

  TfrmRectangles = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmRectangles: TfrmRectangles;

implementation

{ TfrmRectangles }

procedure TfrmRectangles.FormPaint(Sender: TObject);
var
  MyBitmap: TBitmap;
begin
  MyBitmap := TBitmap.Create;
  try
    { Set a size for the image }
    MyBitmap.Height := Height;// 150
    MyBitmap.Width := Width;// 400
    { After memory has been reserved by setting the size
      of the image, we can start drawing }
    // Background
    MyBitmap.Canvas.Brush.Color := clWhite;
    MyBitmap.Canvas.Pen.Color := clWhite;
    MyBitmap.Canvas.Rectangle(0, 0, Width, Height);
    // Explaning text
    MyBitmap.Canvas.TextOut(100,  5, 'Brush: Red  Pen: Black  Grid: Blue');
    MyBitmap.Canvas.TextOut( 10, 25, 'DrawFocusRect');
    MyBitmap.Canvas.TextOut(125, 25, 'FillRect');
    MyBitmap.Canvas.TextOut(225, 25, 'FrameRect');
    MyBitmap.Canvas.TextOut(325, 25, 'Rectangle');
    // Grid
    MyBitmap.Canvas.Pen.Color := clBlue;
    // Horizontal
    MyBitmap.Canvas.Line( 0,  75, 400,  75);
    MyBitmap.Canvas.Line( 0, 125, 400, 125);
    // Vertical
    MyBitmap.Canvas.Line( 25, 40,  25, 150);
    MyBitmap.Canvas.Line( 75, 40,  75, 150);
    MyBitmap.Canvas.Line(125, 40, 125, 150);
    MyBitmap.Canvas.Line(175, 40, 175, 150);
    MyBitmap.Canvas.Line(225, 40, 225, 150);
    MyBitmap.Canvas.Line(275, 40, 275, 150);
    MyBitmap.Canvas.Line(325, 40, 325, 150);
    MyBitmap.Canvas.Line(375, 40, 375, 150);
    // Different rectangles
    MyBitmap.Canvas.Brush.Color := clRed;
    MyBitmap.Canvas.Pen.Color := clBlack;
    MyBitmap.Canvas.DrawFocusRect(Bounds(25, 75, 50, 50));
    MyBitmap.Canvas.FillRect(Bounds(125, 75, 50, 50));
    MyBitmap.Canvas.FrameRect(Bounds(225, 75, 50, 50));
    MyBitmap.Canvas.Rectangle(Bounds(325, 75, 50, 50));
    { Draw the bitmap to the form }
    Canvas.Draw(0, 0, MyBitmap);
  finally
    MyBitmap.Free;
  end;
end;

initialization
  {$I rectanglestest.lrs}

end.

