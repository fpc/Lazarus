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

{$R *.lfm}

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

  // BrushCopy

  Canvas.TextOut(50, 30, 'CopyBrush simple');
  Canvas.Brush.Color := clYellow;
  Canvas.BrushCopy(Bounds(50, 50, 50, 50), Bmp, Bounds(0, 0, 50, 50), clBlue);

  Canvas.TextOut(200, 30, 'CopyBrush stretch');
  Canvas.Brush.Color := clYellow;
  Canvas.BrushCopy(Bounds(200, 50, 20, 20), Bmp, Bounds(0, 0, 50, 50), clBlue);

  Canvas.TextOut(350, 30, 'CopyBrush SrcRect');
  Canvas.Brush.Color := clYellow;
  Canvas.BrushCopy(Bounds(350, 50, 20, 20), Bmp, Bounds(30, 30, 20, 20), clBlue);

  // CopyMode

  Canvas.TextOut(50, 130, 'cmBlackness');
  Canvas.CopyMode := cmBlackness;
  Canvas.Draw(50, 150, Bmp);

  Canvas.TextOut(150, 130, 'cmDstInvert');
  Canvas.CopyMode := cmDstInvert;
  Canvas.Draw(150, 150, Bmp);

  Canvas.TextOut(250, 130, 'cmMergeCopy');
  Canvas.CopyMode := cmMergeCopy;
  Canvas.Draw(250, 150, Bmp);

  Canvas.TextOut(350, 130, 'cmMergePaint');
  Canvas.CopyMode := cmMergePaint;
  Canvas.Draw(350, 150, Bmp);

  Canvas.TextOut(450, 130, 'cmNotSrcCopy');
  Canvas.CopyMode := cmNotSrcCopy;
  Canvas.Draw(450, 150, Bmp);

  // CopyMode line 2

  Canvas.TextOut(50, 230, 'cmNotSrcErase');
  Canvas.CopyMode := cmNotSrcErase;
  Canvas.Draw(50, 250, Bmp);

  Canvas.TextOut(150, 230, 'cmPatCopy');
  Canvas.CopyMode := cmPatCopy;
  Canvas.Draw(150, 250, Bmp);

  Canvas.TextOut(250, 230, 'cmPatInvert');
  Canvas.CopyMode := cmPatInvert;
  Canvas.Draw(250, 250, Bmp);

  Canvas.TextOut(350, 230, 'cmPatPaint');
  Canvas.CopyMode := cmPatPaint;
  Canvas.Draw(350, 250, Bmp);

  Canvas.TextOut(450, 230, 'cmSrcAnd');
  Canvas.CopyMode := cmSrcAnd;
  Canvas.Draw(450, 250, Bmp);

  // CopyMode line 3

  Canvas.TextOut(50, 330, 'cmSrcCopy');
  Canvas.CopyMode := cmSrcCopy;
  Canvas.Draw(50, 350, Bmp);

  Canvas.TextOut(150, 330, 'cmSrcErase');
  Canvas.CopyMode := cmSrcErase;
  Canvas.Draw(150, 350, Bmp);

  Canvas.TextOut(250, 330, 'cmSrcInvert');
  Canvas.CopyMode := cmSrcInvert;
  Canvas.Draw(250, 350, Bmp);

  Canvas.TextOut(350, 330, 'cmSrcPaint');
  Canvas.CopyMode := cmSrcPaint;
  Canvas.Draw(350, 350, Bmp);

  Canvas.TextOut(450, 330, 'cmWhiteness');
  Canvas.CopyMode := cmWhiteness;
  Canvas.Draw(450, 350, Bmp);

  Bmp.Free;
end;

procedure TfrmDraw.FormCreate(Sender: TObject);
begin

end;

end.

