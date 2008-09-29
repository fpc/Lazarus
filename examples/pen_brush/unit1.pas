unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    PenBox: TPaintBox;
    BrushBox: TPaintBox;
    procedure BrushBoxPaint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PenBoxPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  TypInfo;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BrushBoxPaint(Sender: TObject);
var
  bs: TBrushStyle;
  y: integer;
begin
  for bs := bsSolid to bsDiagCross do
  begin
    y := 15 * (Ord(bs) + 1);
    BrushBox.Canvas.Brush.Color := clBtnFace;
    BrushBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TBrushStyle), Ord(bs)));
    BrushBox.Canvas.Brush.Color := clRed;
    BrushBox.Canvas.Brush.Style := bs;
    BrushBox.Canvas.FillRect(120, y - 5, PenBox.Width - 10, y + 5);
  end;
end;

procedure TForm1.PenBoxPaint(Sender: TObject);
var
  ps: TPenStyle;
  y: integer;
begin
  for ps := psSolid to psDashDotDot do
  begin
    y := 15 * (Ord(ps) + 1);
    PenBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TPenStyle), Ord(ps)));
    PenBox.Canvas.Pen.Style := ps;
    PenBox.Canvas.Line(120, y, PenBox.Width - 10, y);
  end;
end;

initialization
  {$I unit1.lrs}

end.

