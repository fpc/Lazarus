unit fonttest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics;

type

  { TfrmFont }

  TfrmFont = class(TForm)
    procedure FormPaint(Sender: TObject);
  private

  public

  end; 

var
  frmFont: TfrmFont;

implementation

{$R *.lfm}

{ TfrmFont }

procedure TfrmFont.FormPaint(Sender: TObject);
var
  MyBitmap: TBitmap;
  Angle: Integer;
begin
  MyBitmap := TBitmap.Create;
  try
    { Set a size for the image }
    MyBitmap.Height := Height;
    MyBitmap.Width := Width;
    { After memory has been reserved by setting the size
      of the image, we can start drawing }
    // Background
    MyBitmap.Canvas.Brush.Color := clWhite;
    MyBitmap.Canvas.Pen.Color := clWhite;
    MyBitmap.Canvas.Rectangle(0, 0, Width, Height);

    // Various rotated texts
    MyBitmap.Canvas.Font.Size := 14;
    MyBitmap.Canvas.Brush.Style := bsClear;
    MyBitmap.Canvas.Font.Style := [fsUnderline];
    MyBitmap.Canvas.TextOut(50,  25, 'Rotated texts:');
    MyBitmap.Canvas.Font.Style := [];

    Angle := 0;
    while Angle < 360 do
    begin
      MyBitmap.Canvas.Font.Orientation := Angle * 10;
      MyBitmap.Canvas.TextOut(75, 300, '          '
        + 'Font rotated by ' + IntToStr(Angle) + ' degrees');
      if Angle = 100 then Angle := 260
      else Angle := Angle + 20;
    end;

//    MyBitmap.Canvas.Brush.Style := bsSolid;
//    MyBitmap.Canvas.Rectangle(245, 245, 255, 255);

    // Different fonts
    MyBitmap.Canvas.Font.Orientation := 0;
    MyBitmap.Canvas.Font.Style := [fsUnderline];
    MyBitmap.Canvas.TextOut(325,  25, 'Various fonts:');
    MyBitmap.Canvas.Font.Style := [];
    MyBitmap.Canvas.Font.Size := 16;
    MyBitmap.Canvas.Font.Name := 'Arial';
    MyBitmap.Canvas.TextOut(325, 60, 'Arial Font');
    MyBitmap.Canvas.Font.Name := 'Times New Roman';
    MyBitmap.Canvas.TextOut(325, 85, 'Timew New Roman Font');
    MyBitmap.Canvas.Font.Name := 'Courier New';
    MyBitmap.Canvas.TextOut(325, 110, 'Courier New Font');
    MyBitmap.Canvas.Font.Name := 'Osaka';
    MyBitmap.Canvas.TextOut(325, 135, 'ラザロクールだ！ (Osaka Font)');
{    MyBitmap.Canvas.Font.Name := 'Courier New';
    MyBitmap.Canvas.TextOut(325, 150, 'عربي‎');
    MyBitmap.Canvas.Pen.Color := clRed;
    MyBitmap.Canvas.Line(325, 135, 500, 135);
    MyBitmap.Canvas.Line(325, 150, 500, 150);}

    // Font sizes
    MyBitmap.Canvas.Font.Orientation := 0;
    MyBitmap.Canvas.Font.Size := 14;
    MyBitmap.Canvas.Font.Style := [fsUnderline];
    MyBitmap.Canvas.TextOut(325, 200, 'Font sizes:');
    MyBitmap.Canvas.Font.Style := [];
    MyBitmap.Canvas.Font.Name := 'Arial';
    MyBitmap.Canvas.Font.Size := 8;
    MyBitmap.Canvas.TextOut(325, 220, 'Size 8');
    MyBitmap.Canvas.Font.Size := 12;
    MyBitmap.Canvas.TextOut(325, 240, 'Size 12');
    MyBitmap.Canvas.Font.Size := 16;
    MyBitmap.Canvas.TextOut(325, 260, 'Size 16');
    MyBitmap.Canvas.Font.Size := 20;
    MyBitmap.Canvas.TextOut(325, 280, 'Size 20');
    MyBitmap.Canvas.Font.Size := 30;
    MyBitmap.Canvas.TextOut(325, 300, 'Size 30');

    // Font styles
    MyBitmap.Canvas.Font.Orientation := 0;
    MyBitmap.Canvas.Font.Size := 14;
    MyBitmap.Canvas.Font.Style := [fsUnderline];
    MyBitmap.Canvas.TextOut(325, 350, 'Font styles:');
    MyBitmap.Canvas.Font.Style := [fsBold];
    MyBitmap.Canvas.TextOut(325, 375, 'fsBold');
    MyBitmap.Canvas.Font.Style := [fsItalic];
    MyBitmap.Canvas.TextOut(325, 400, 'fsItalic');
    MyBitmap.Canvas.Font.Style := [fsStrikeOut];
    MyBitmap.Canvas.TextOut(325, 425, 'fsStrikeOut');
    MyBitmap.Canvas.Font.Style := [fsUnderline];
    MyBitmap.Canvas.TextOut(325, 450, 'fsUnderline');

    { Draw the bitmap to the form }
    Canvas.Draw(0, 0, MyBitmap);
  finally
    MyBitmap.Free;
  end;
end;

end.

