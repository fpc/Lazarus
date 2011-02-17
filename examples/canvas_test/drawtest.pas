unit drawtest;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls;

type

  { TfrmDraw }

  TfrmDraw = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDraw: TfrmDraw;

implementation

{ TfrmDraw }

procedure TfrmDraw.FormPaint(Sender: TObject);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.Width := 50;
  Bmp.Height := 50;
  Bmp.Canvas.Brush.Color := clBlue;
  Bmp.Canvas.Rectangle(0, 0, 50, 50);

  Canvas.TextOut(50, 30, 'cmDstInvert');
  Canvas.CopyMode := cmDstInvert;
  Canvas.Draw(50, 50, Bmp);

  Bmp.Free;
end;

procedure TfrmDraw.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I drawtest.lrs}

end.

