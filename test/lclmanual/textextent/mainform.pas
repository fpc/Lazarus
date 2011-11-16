unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses lclintf, types;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  lRes: Integer;
  lStr: String;
  lMaxCount: Integer;
  lPartialWidths: array[0..23] of Integer;
  lSize: TSize;
  i: Integer;
begin
  // Basic initialization
  Image.Canvas.Font.Size := 12;
  Image.Canvas.Pen.Color := clWhite;
  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.Rectangle(0, 0, Image.Width, Image.Height);

  // TextFitInfo test 1 (calls GetTextExtentExPoint with partial widths = nil)
  lStr := 'ABCDEFGHIJKLMNOPQRSXYZ';
  lRes := Image.Canvas.TextFitInfo(lStr, 80);
  Image.Canvas.TextOut(20, 20, Format('TextFitInfo Test 1. Returned %d should be aprox. 7 to 10', [lRes]));
  Image.Canvas.Brush.Color := clBlue;
  Image.Canvas.Rectangle(20, 35, 100, 50);
  Image.Canvas.TextOut(20, 35, lStr);

  // GetTextExtentExPoint test 1 with partial widths and MaxCount
  lStr := 'ABCDEFGHIJKLMNOPQRSXYZ';
  LCLIntf.GetTextExtentExPoint(Image.Canvas.Handle, PChar(lStr),
    Length(lStr), 50, @lMaxCount, @lPartialWidths[0], lSize);
  lRes := Image.Canvas.TextFitInfo(lStr, 50);
  Image.Canvas.TextOut(20, 60, Format('GetTextExtentExPoint Test 1. Returned MaxCount=%d should be aprox. 4 to 6', [lMaxCount]));
  Image.Canvas.Brush.Color := clBlue;
  for i := 0 to 6 do
  begin
    Image.Canvas.Rectangle(20, 75 + i * 15, 20+lPartialWidths[i], 90 + i * 15);
    Image.Canvas.TextOut(20, 75 + i *15, lStr);
  end;
end;

end.

