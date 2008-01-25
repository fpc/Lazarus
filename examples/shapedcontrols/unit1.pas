unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure ShapeControl(AControl: TWinControl);
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShapeControl(Self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Handle;
  ShapeControl(Button1);
end;

procedure TForm1.ShapeControl(AControl: TWinControl);
var
  ABitmap: TBitmap;
  Points: array of TPoint;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Monochrome := True;
  ABitmap.Width := AControl.Width;
  ABitmap.Height := AControl.Height;
  SetLength(Points, 6);
  Points[0] := Point(0, ABitmap.Height div 2);
  Points[1] := Point(10, 0);
  Points[2] := Point(ABitmap.Width - 10, 0);
  Points[3] := Point(ABitmap.Width, ABitmap.Height div 2);
  Points[4] := Point(ABitmap.Width - 10, ABitmap.Height);
  Points[5] := Point(10, ABitmap.Height);
  with ABitmap.Canvas do
  begin
    Brush.Color := clBlack; // transparent color
    FillRect(0, 0, ABitmap.Width, ABitmap.Height);
    Brush.Color := clWhite; // mask color
    Polygon(Points);
  end;
  AControl.SetShape(ABitmap);
  ABitmap.Free;
end;

initialization
  {$I unit1.lrs}

end.

