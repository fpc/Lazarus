unit Unit1;

{$mode objfpc}{$H+}

{Author: Mike Sapsard, 2015.
 License: GPL3.

 LCL demo of redraw event order for images, panels and forms. it should work
 for all widget sets eg Windows, qt, OSX, GTK etc.

 Repaint or Invalidate are not needed for resizing events.}

interface

uses
  {$ifdef Windows} Windows, {$Endif}SysUtils, Forms, Graphics, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBoxImage: TCheckBox;
    CheckBoxPanel: TCheckBox;
    CheckBoxForm: TCheckBox;
    Image1: TImage;
    Memo1: TMemo;
    Pnl1: TPanel;
    Pnl2: TPanel;
    PnlImage: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1Resize(Sender: TObject);
    procedure PnlImageResize(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Bitmap: TBitmap;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.DoubleBuffered := True;      //Stop flickering on some widget sets
  Bitmap := TBitmap.Create;
  with Bitmap do
  begin
    // Use ClientWidth and ClientHeight so that code works across widget sets
    // eg Windows, qt, OSX, GTK etc
    // Height and Width do not allow for widget set and theme borders
    Width := PnlImage.ClientWidth;
    Height := PnlImage.ClientHeight;
    Canvas.brush.color := clBlue;
    Canvas.Rectangle(0, 0, Image1.ClientWidth, Image1.ClientHeight);
    Canvas.Font.Size := 20;
    Canvas.TextOut(15, 170, 'This is the Image');
  end;
  Image1.Picture.Graphic := Bitmap; //Assign the bitmap to Image1
  Memo1.Clear;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if CheckBoxForm.Checked then
  begin
    with Image1.Picture.Bitmap do
    begin
      // Bitmap has a height and width, but these do not change from default,
      // because a copy is used by Image1.Picture.Bitmap
      Image1.Picture.Bitmap.SetSize(Image1.ClientWidth, Image1.ClientHeight);
      // If panel is green, this event was last
      Canvas.Brush.Color := clGreen;
      Canvas.Rectangle(0, 0, Image1.ClientWidth, Image1.ClientHeight);
      Canvas.Font.Color := clYellow;
      Canvas.Font.Size := 15;
      Canvas.TextOut(10, 90, 'Form resized');
    end;
    Memo1.Lines.Add(IntToStr(Memo1.Lines.Count) + '  Form resized');
  end;
  if (Memo1.Lines.Count > 200) then
    Memo1.Lines.Clear;
end;

procedure TForm1.Image1Resize(Sender: TObject);
begin
  if CheckBoxImage.Checked then
  begin
    // Bitmap has a height and width, but these do not change from default,
    // because a copy is used by Image1.Picture.Bitmap
    Image1.Picture.Bitmap.SetSize(Image1.ClientWidth, Image1.ClientHeight);
    with Image1.Picture.Bitmap do
    begin
      // If panel is white, this event was last
      Canvas.Brush.Color := clWhite;
      Canvas.Rectangle(0, 0, Image1.ClientWidth, Image1.ClientHeight);
      Canvas.Font.Color := clGreen;
      Canvas.Font.Size := 15;
      Canvas.TextOut(10, 10, 'Image resized');
    end;
    Memo1.Lines.Add(IntToStr(Memo1.Lines.Count) + '  Image resize');
  end;
  if (Memo1.Lines.Count > 200) then
    Memo1.Lines.Clear;
end;

procedure TForm1.PnlImageResize(Sender: TObject);
begin
  if CheckBoxPanel.Checked then
  begin
    // Bitmap has a height and width, but these do not change from default,
    // because a copy is used by Image1.Picture.Bitmap
    Image1.Picture.Bitmap.SetSize(Image1.ClientWidth, Image1.ClientHeight);
    with Image1.Picture.Bitmap do
    begin
      // If panel is red, this event was last
      Canvas.Brush.Color := clRed;
      Canvas.Rectangle(0, 0, Image1.ClientWidth, Image1.ClientHeight);
      Canvas.Font.Color := clBlack;
      Canvas.Font.Size := 15;
      Canvas.TextOut(10, 50, 'Panel resized');
    end;
    Memo1.Lines.Add(IntToStr(Memo1.Lines.Count) + '  Panel resize');
  end;
  if (Memo1.Lines.Count > 200) then
    Memo1.Lines.Clear;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Bitmap.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
