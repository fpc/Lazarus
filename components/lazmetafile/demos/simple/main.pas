unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, lmf;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FLmfImg: TLmfImage;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  LmfCanvas: TlmfCanvas;
begin
  FLmfImg := TlmfImage.Create;
  FLmfImg.Width := 300;
  FLmfImg.Height := 200;

  LmfCanvas := TlmfCanvas.Create(FLmfImg);
  try
    // Rectangle
    LmfCanvas.Brush.Color := clSkyBlue;
    LmfCanvas.Rectangle(0, 0, 300, 200);

    // Line
    LmfCanvas.Pen.Width := 1;
    LmfCanvas.Line(0, 0, 300, 200);

    // MoveTo + LineTo
    LmfCanvas.Pen.Width := 3;
    LmfCanvas.MoveTo(0, 199);
    LmfCanvas.LineTo(299, 0);

    // FillRect
    LmfCanvas.Brush.Color := clBlue;
    LmfCanvas.FillRect(10, 100, 100, 150);
    LmfCanvas.Brush.Color := clWhite;
    LmfCanvas.FillRect(Rect(15, 110, 50, 130));

    // Ellipse
    LmfCanvas.Brush.Style := bsHorizontal;
    LmfCanvas.Brush.Color := clRed;
    LmfCanvas.Pen.Color := clYellow;
    LmfCanvas.Pen.Style := psSolid;
    LmfCanvas.Pen.Width := 2;
    LmfCanvas.Ellipse(50, 50, 250, 150);

    // Text
    LmfCanvas.Font.Size := 12;
    LmfCanvas.Font.Color := clOlive;
    LmfCanvas.TextOut(20, 20, 'Painted in OnCreate');

  finally
    LmfCanvas.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLmfImg.Free;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if Assigned(FLmfImg) then
  begin
    // Paint metafile image on the canvas of the form
    Canvas.Draw(10, 10, FLmfImg);
  end;
end;

end.

