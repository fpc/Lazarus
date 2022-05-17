unit brushtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Graphics;

type

  { TfrmBrush }

  TfrmBrush = class(TForm)
    procedure FormPaint(Sender: TObject);
  private

  public

  end; 

var
  frmBrush: TfrmBrush;

implementation

{$R *.lfm}

{ TfrmBrush }

procedure TfrmBrush.FormPaint(Sender: TObject);
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
    MyBitmap.Canvas.TextOut(100,  5, 'Brush: Blue  Pen: Black');

    // Image brushes
    MyBitmap.Canvas.TextOut( 25,  25, 'Canvas-aligned bitmapped Brush:');

    MyBitmap.Canvas.Brush.Bitmap := TBitmap.Create;
    MyBitmap.Canvas.Brush.Bitmap.Height := 20;
    MyBitmap.Canvas.Brush.Bitmap.Width := 20;
    MyBitmap.Canvas.Brush.Bitmap.Canvas.Brush.Color := clWhite;
    MyBitmap.Canvas.Brush.Bitmap.Canvas.FillRect(0, 0, 20, 20);
    MyBitmap.Canvas.Brush.Bitmap.Canvas.Brush.Color := clRed;
    MyBitmap.Canvas.Brush.Bitmap.Canvas.Ellipse(0, 0, 20, 20);
    MyBitmap.Canvas.Rectangle(Bounds( 25, 50, 50, 50));
    MyBitmap.Canvas.Rectangle(Bounds(100, 50, 50, 50));
    MyBitmap.Canvas.Brush.Bitmap.Free;
    MyBitmap.Canvas.Brush.Bitmap := nil;

    // Brush styles

    MyBitmap.Canvas.TextOut( 25, 110, 'Brush styles:');

    MyBitmap.Canvas.TextOut( 25, 130, 'bsSolid');
    MyBitmap.Canvas.TextOut(125, 130, 'bsClear');
    MyBitmap.Canvas.TextOut(225, 130, 'bsHorizontal');
    MyBitmap.Canvas.TextOut(325, 130, 'bsVertical');
    MyBitmap.Canvas.TextOut( 25, 200, 'bsFDiagonal');
    MyBitmap.Canvas.TextOut(125, 200, 'bsBDiagonal');
    MyBitmap.Canvas.TextOut(225, 200, 'bsCross');
    MyBitmap.Canvas.TextOut(325, 200, 'bsDiagCross');

    MyBitmap.Canvas.Brush.Color := clBlue;
    MyBitmap.Canvas.Pen.Color := clBlack;

    MyBitmap.Canvas.Brush.Style := bsSolid;
    MyBitmap.Canvas.Rectangle(Bounds( 25, 145, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsClear;
    MyBitmap.Canvas.Rectangle(Bounds(125, 145, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsHorizontal;
    MyBitmap.Canvas.Rectangle(Bounds(225, 145, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsVertical;
    MyBitmap.Canvas.Rectangle(Bounds(325, 145, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsFDiagonal;
    MyBitmap.Canvas.Rectangle(Bounds( 25, 215, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsBDiagonal;
    MyBitmap.Canvas.Rectangle(Bounds(125, 215, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsCross;
    MyBitmap.Canvas.Rectangle(Bounds(225, 215, 50, 50));
    MyBitmap.Canvas.Brush.Style := bsDiagCross;
    MyBitmap.Canvas.Rectangle(Bounds(325, 215, 50, 50));

    // FillRect
    MyBitmap.Canvas.TextOut( 25, 280, 'FillRect');
    MyBitmap.Canvas.Brush.Color := clGray;
    MyBitmap.Canvas.Brush.Style := bsSolid;
    MyBitmap.Canvas.FillRect(Bounds( 25, 300, 50, 50));

    { Draw the bitmap to the form }
    Canvas.Draw(0, 0, MyBitmap);
  finally
    MyBitmap.Free;
  end;
end;

end.

