unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ColorBox, LCLIntf, FPCanvas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Button1: TBitBtn;
    cbCosmetic: TCheckBox;
    cbAntialiasing: TCheckBox;
    FigureCombo: TComboBox;
    Label10: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BrushColorBox: TColorBox;
    PenStyleCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PenColorBox: TColorBox;
    Label6: TLabel;
    BrushStyleCombo: TComboBox;
    WidthCombo: TComboBox;
    CapsCombo: TComboBox;
    JoinCombo: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PaintBox: TPaintBox;
    procedure BrushChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbAntialiasingChange(Sender: TObject);
    procedure FigureComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenChange(Sender: TObject);
  private
    FPattern: TBitmap;
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
  Monitor;
end;

procedure TForm1.cbAntialiasingChange(Sender: TObject);
const
  AntialiasingMode: array[TCheckBoxState] of TAntialiasingMode =
  (
    amOff,
    amOn,
    amDontCare
  );
begin
  PaintBox.Canvas.AntialiasingMode := AntialiasingMode[cbAntialiasing.State];
  PaintBox.Invalidate;
end;

procedure TForm1.FigureComboChange(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.BrushChange(Sender: TObject);
begin
  if BrushStyleCombo.ItemIndex <> -1 then
    PaintBox.Canvas.Brush.Style := TBrushStyle(BrushStyleCombo.ItemIndex);

  if PaintBox.Canvas.Brush.Style = bsPattern then
    PaintBox.Canvas.Brush.Bitmap := FPattern
  else
    PaintBox.Canvas.Brush.Bitmap := nil;

  PaintBox.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  LineBitsDotted: array[0..7] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA);
var
  ps: TPenStyle;
  bs: TBrushStyle;
begin
  case PaintBox.Canvas.AntialiasingMode of
    amDontCare: cbAntialiasing.State := cbGrayed;
    amOn: cbAntialiasing.State := cbChecked;
    amOff: cbAntialiasing.State := cbUnchecked;
  end;


  FPattern := TBitmap.Create;
  FPattern.SetHandles(CreateBitmap(8, 8, 1, 1, @LineBitsDotted), 0);

  PenStyleCombo.Items.BeginUpdate;
  for ps := Low(ps) to High(ps) do
    PenStyleCombo.Items.Add(GetEnumName(TypeInfo(TPenStyle), Ord(ps)));
  PenStyleCombo.Items.EndUpdate;
  PenStyleCombo.ItemIndex := 0;

  BrushStyleCombo.Items.BeginUpdate;
  for bs := Low(bs) to High(bs) do
    BrushStyleCombo.Items.Add(GetEnumName(TypeInfo(TBrushStyle), Ord(bs)));
  BrushStyleCombo.Items.EndUpdate;
  BrushStyleCombo.ItemIndex := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPattern.Free;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);

  function RandomPoint(R: TRect): TPoint;
  begin
    Result.x := Random(R.Right - R.Left) + R.Left;
    Result.y := Random(R.Bottom - R.Top) + R.Top;
  end;

  procedure DrawFigure(R: TRect); inline;
  var
    Points: array of TPoint;
  begin
    inflateRect(R, -10, -10);
    case FigureCombo.ItemIndex of
      0: // Line
        PaintBox.Canvas.Line(R.TopLeft, R.BottomRight);
      1: // PolyLine
        begin
          SetLength(Points, 4);
          Points[0] := R.TopLeft;
          Points[1] := RandomPoint(R);
          Points[2] := RandomPoint(R);
          Points[3] := R.BottomRight;
          PaintBox.Canvas.Polyline(Points);
        end;
      2: // Ellipse
        PaintBox.Canvas.Ellipse(R);
      3: // Rectangle
        begin
          PaintBox.Canvas.FillRect(R);
          PaintBox.Canvas.Rectangle(R);
        end;
      4: // Triangle
        begin
          SetLength(Points, 4);
          Points[0] := Point(R.Left, R.Bottom);
          Points[3] := Points[0];
          Points[1] := Point((R.Left + R.Right) div 2, R.Top);
          Points[2] := R.BottomRight;
          PaintBox.Canvas.Polygon(Points);
        end;
    end;
  end;

var
  i, j: integer;
  ColWidth, RowHeight: Integer;
  R: TRect;
begin
  PaintBox.Canvas.Brush.Color := BrushColorBox.Selected;

  ColWidth := PaintBox.Width div 3;
  RowHeight := PaintBox.Height div 2;

  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      R := Rect(i * ColWidth, j * RowHeight, (i + 1) * ColWidth, (j + 1) * RowHeight);
      DrawFigure(R);
    end;
end;

procedure TForm1.PenChange(Sender: TObject);
var
  Dashes: array[0..3] of DWord = (3, 7, 8, 6);
begin
  if PenStyleCombo.ItemIndex <> -1 then
    PaintBox.Canvas.Pen.Style := TPenStyle(PenStyleCombo.ItemIndex);
  PaintBox.Canvas.Pen.Color := PenColorBox.Selected;

  PaintBox.Canvas.Pen.Width := StrToInt(WidthCombo.Text);
  PaintBox.Canvas.Pen.Cosmetic := cbCosmetic.Checked;
  PaintBox.Canvas.Pen.EndCap := TPenEndCap(CapsCombo.ItemIndex);
  PaintBox.Canvas.Pen.JoinStyle := TPenJoinStyle(JoinCombo.ItemIndex);
  PaintBox.Canvas.Pen.SetPattern(Dashes);
  PaintBox.Invalidate;
end;

initialization
  {$I unit1.lrs}

end.

