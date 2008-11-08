unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ColorBox, LCLIntf;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TBitBtn;
    PenColorBox: TColorBox;
    GeometricCheck: TCheckBox;
    Label6: TLabel;
    WidthCombo: TComboBox;
    CapsCombo: TComboBox;
    JoinCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PenBox: TPaintBox;
    BrushBox: TPaintBox;
    procedure BrushBoxPaint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PenBoxPaint(Sender: TObject);
    procedure WidthComboChange(Sender: TObject);
  private
    FPattern: TBitmap;
    procedure PenChange;
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

procedure TForm1.FormCreate(Sender: TObject);
const
  LineBitsDotted: array[0..7] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA);
begin
  FPattern := TBitmap.Create;
  FPattern.SetHandles(CreateBitmap(8, 8, 1, 1, @LineBitsDotted), 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPattern.Free;
end;

procedure TForm1.BrushBoxPaint(Sender: TObject);
var
  bs: TBrushStyle;
  y: integer;
begin
  BrushBox.Canvas.Brush.Bitmap := nil;
  y := 15;
  for bs := bsSolid to bsDiagCross do
  begin
    BrushBox.Canvas.Brush.Color := clBtnFace;
    BrushBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TBrushStyle), Ord(bs)));
    BrushBox.Canvas.Brush.Color := clRed;
    BrushBox.Canvas.Brush.Style := bs;
    BrushBox.Canvas.FillRect(120, y - 5, PenBox.Width - 10, y + 5);
    inc(y, 15);
  end;

  BrushBox.Canvas.Brush.Color := clBtnFace;
  BrushBox.Canvas.TextOut(10, y - 7, 'Pattern');
  BrushBox.Canvas.Brush.Color := clRed;
  BrushBox.Canvas.Brush.Bitmap := FPattern;
  BrushBox.Canvas.FillRect(120, y - 5, PenBox.Width - 10, y + 5);
end;

procedure TForm1.PenBoxPaint(Sender: TObject);
var
  ps: TPenStyle;
  y: integer;
begin
  y := 15;
  for ps := psSolid to psDashDotDot do
  begin
    PenBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TPenStyle), Ord(ps)));
    PenBox.Canvas.Pen.Style := ps;
    PenBox.Canvas.Line(120, y, PenBox.Width - 10, y);
    inc(y, 15);
  end;
  PenBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TPenStyle), Ord(psClear)));
  PenBox.Canvas.Pen.Style := psClear;
  PenBox.Canvas.Line(120, y, PenBox.Width - 10, y);
  inc(y, 15);

  PenBox.Canvas.TextOut(10, y - 7, GetEnumName(TypeInfo(TPenStyle), Ord(psPattern)));
  PenBox.Canvas.Pen.Style := psPattern;
  PenBox.Canvas.Line(120, y, PenBox.Width - 10, y);
end;

procedure TForm1.PenChange;
var
  Dashes: array[0..3] of DWord = (1, 1, 1, 1);
begin
  PenBox.Canvas.Pen.Color := PenColorBox.Selected;
  PenBox.Canvas.Pen.Width := StrToInt(WidthCombo.Text);
  PenBox.Canvas.Pen.Geometric := GeometricCheck.Checked;
  PenBox.Canvas.Pen.EndCap := TPenEndCap(CapsCombo.ItemIndex);
  PenBox.Canvas.Pen.JoinStyle := TPenJoinStyle(JoinCombo.ItemIndex);
  PenBox.Canvas.Pen.SetPattern(Dashes);
  PenBox.Invalidate;
end;

procedure TForm1.WidthComboChange(Sender: TObject);
begin
  PenChange;
end;

initialization
  {$I unit1.lrs}

end.

