program canvastest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, canvastestunit, LResources, pentest,
  brushtest, fonttest, rectanglestest, shapedwindowtest, ellipseunit,
  screenshotunit, drawtest, imagetest;

//{$IFDEF WINDOWS}{$R canvastest.rc}{$ENDIF}

begin
  {$I canvastest.lrs}
  Application.Initialize;
  Application.CreateForm(TfrmCanvasTest, frmCanvasTest);
  Application.CreateForm(TfrmPen, frmPen);
  Application.CreateForm(TfrmBrush, frmBrush);
  Application.CreateForm(TfrmFont, frmFont);
  Application.CreateForm(TfrmRectangles, frmRectangles);
  Application.CreateForm(TfrmShapedWindow, frmShapedWindow);
  Application.CreateForm(TfrmScreenshot, frmScreenshot);
  Application.CreateForm(TfrmEllipse, frmEllipse);
  Application.CreateForm(TfrmDraw, frmDraw);
  Application.CreateForm(TfrmImage, frmImage);
  Application.Run;
end.

