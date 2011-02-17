unit canvastestunit;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL, LCL
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  // Project units
  rectanglestest, brushtest, pentest, fonttest, shapedwindowtest,
  ellipseunit, screenshotunit, drawtest, imagetest;

type

  { TfrmCanvasTest }

  TfrmCanvasTest = class(TForm)
    btnBrush: TButton;
    btnPen: TButton;
    btnFont: TButton;
    btnClose: TButton;
    btnRectangles: TButton;
    btnShapedWindow: TButton;
    btnEllipse: TButton;
    btnScreenshot: TButton;
    btnDraw: TButton;
    btnImage: TButton;
    procedure btnBrushClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDrawClick(Sender: TObject);
    procedure btnEllipseClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnImageClick(Sender: TObject);
    procedure btnPenClick(Sender: TObject);
    procedure btnRectanglesClick(Sender: TObject);
    procedure btnScreenshotClick(Sender: TObject);
    procedure btnShapedWindowClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmCanvasTest: TfrmCanvasTest;

implementation

{ TfrmCanvasTest }

procedure TfrmCanvasTest.btnBrushClick(Sender: TObject);
begin
  frmBrush.ShowModal;
end;

procedure TfrmCanvasTest.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCanvasTest.btnDrawClick(Sender: TObject);
begin
  frmDraw.ShowModal;
end;

procedure TfrmCanvasTest.btnEllipseClick(Sender: TObject);
begin
  frmEllipse.ShowModal;
end;

procedure TfrmCanvasTest.btnFontClick(Sender: TObject);
begin
  frmFont.ShowModal;
end;

procedure TfrmCanvasTest.btnImageClick(Sender: TObject);
begin
  frmImage.ShowModal;
end;

procedure TfrmCanvasTest.btnPenClick(Sender: TObject);
begin
  frmPen.ShowModal;
end;

procedure TfrmCanvasTest.btnRectanglesClick(Sender: TObject);
begin
  frmRectangles.ShowModal;
end;

procedure TfrmCanvasTest.btnScreenshotClick(Sender: TObject);
begin
  frmScreenshot.ShowModal;
end;

procedure TfrmCanvasTest.btnShapedWindowClick(Sender: TObject);
begin
  frmShapedWindow.ShowModal;
end;

initialization
  {$I canvastestunit.lrs}

end.

