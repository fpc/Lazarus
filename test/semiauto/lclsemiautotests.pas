unit lclsemiautotests;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpcunit,
  Interfaces, LCLType, LCLIntf, Forms, Graphics,
  testglobals, semiautotest;

type

  { TTestLCLFormCanvas }

  TTestLCLFormCanvas = class(TSemiAutomaticTest)
  private
    APt: TPoint;
    procedure TestOneHandlePaint(Sender: TObject);
  published
    procedure TestOne;
  end;

implementation

{ TTestIdeNew }

procedure TTestLCLFormCanvas.TestOneHandlePaint(Sender: TObject);
begin
  FDialog.Canvas.Pen.Color := clRed;
  FDialog.Canvas.Brush.Color := clBlue;
  FDialog.Canvas.Rectangle(Bounds(APt.X, APt.Y, 100, 100));
end;

procedure TTestLCLFormCanvas.TestOne;
var
  Str: string;
  lDialog: TForm;
begin
  Str := 'Please verify is a rectangle with a red frame and blue contents is drawn'
   ;
  lDialog := GetCanvasDialog(APt);
  lDialog.OnPaint := @TestOneHandlePaint;
  AssertTrue(ShowCanvasDialog('TTestLCLFormCanvas.TestOne', Str));
end;

initialization
  AddToSemiAutoTestSuite(TTestLCLFormCanvas);

end.

