unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons,
  ColorBox,
  LCLIntf, LCLType, FPCanvas;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BgColorBox: TColorBox;
    cbOpaque: TCheckBox;
    FontColorBox: TColorBox;
    Button1: TBitBtn;
    cbCosmetic: TCheckBox;
    cbAntialiasing: TCheckBox;
    FigureCombo: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    LblBgColor: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BrushColorBox: TColorBox;
    LblBgColor1: TLabel;
    PenStyleCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PenColorBox: TColorBox;
    Label6: TLabel;
    BrushStyleCombo: TComboBox;
    PenStyleInfoBtn: TSpeedButton;
    BrushStyleInfoBtn: TSpeedButton;
    WidthCombo: TComboBox;
    CapsCombo: TComboBox;
    JoinCombo: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PaintBox: TPaintBox;
    procedure BrushChange(Sender: TObject);
    procedure cbAntialiasingChange(Sender: TObject);
    procedure cbOpaqueChange(Sender: TObject);
    procedure FigureComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PenChange(Sender: TObject);
    procedure BrushStyleInfoBtnClick(Sender: TObject);
    procedure PenStyleInfoBtnClick(Sender: TObject);
  private
    FPattern: TCustomBitmap;
    FImage: TCustomBitmap;
  public

  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TypInfo;

const
  BK_MODE: array[boolean] of Integer = (TRANSPARENT, OPAQUE);

{ TForm1 }

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

procedure TForm1.cbOpaqueChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TForm1.FigureComboChange(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.BrushChange(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  //LineBitsDotted: array[0..7] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA);
  LineBitsCheckerboard: array[0..7] of Word = ($3C, $3C, $3C, $3C, $C3, $C3, $C3, $C3);
var
  ps: TPenStyle;
  bs: TBrushStyle;
begin
  case PaintBox.Canvas.AntialiasingMode of
    amDontCare: cbAntialiasing.State := cbGrayed;
    amOn: cbAntialiasing.State := cbChecked;
    amOff: cbAntialiasing.State := cbUnchecked;
  end;

  FImage := TPortableNetworkGraphic.Create;
  FImage.LoadFromFile('image.png');

  FPattern := TBitmap.Create;
  FPattern.SetHandles(CreateBitmap(8, 8, 1, 1, @LineBitsCheckerboard), 0);

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
  FImage.Free;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);

  function RandomPoint(R: TRect): TPoint;
  begin
    Result.x := Random(R.Right - R.Left) + R.Left;
    Result.y := Random(R.Bottom - R.Top) + R.Top;
  end;

  procedure DrawFigure(R: TRect); inline;
  var
    Points: array of TPoint = nil;
    txt: String;
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
      5: // Text
        begin
          txt := 'Text';
          SetLength(Points, 1);
          Points[0].X := (R.Left + R.Right - Paintbox.Canvas.TextWidth(txt)) div 2;
          Points[0].Y := (R.Top + R.Bottom - Paintbox.Canvas.TextHeight(txt)) div 2;
          SetBkMode(Paintbox.Canvas.Handle, BK_MODE[cbOpaque.Checked]);
          Paintbox.Canvas.Font.Color := FontColorBox.Selected;
          Paintbox.Canvas.TextOut(Points[0].X, Points[0].Y, txt);
        end;
    end;
  end;

var
  i, j: integer;
  ColWidth, RowHeight: Integer;
  Dashes: Graphics.TPenPattern = (3, 7, 8, 6);
  R: TRect;
begin
  // Draw background
  Paintbox.Canvas.Pen.Style := psSolid;
  Paintbox.Canvas.GradientFill(Rect(0, 0, Paintbox.Width, Paintbox.Height), clSkyBlue, clWhite, gdVertical);

  if not (Paintbox.Canvas.Brush.Style in [bsPattern, bsImage]) then
  begin
    SetBkMode(Paintbox.Canvas.Handle, BK_MODE[cbOpaque.Checked]);
    SetBkColor(Paintbox.Canvas.Handle, BgColorBox.Selected);
  end;

  // Set pen parameters
  if PenStyleCombo.ItemIndex <> -1 then
    PaintBox.Canvas.Pen.Style := TPenStyle(PenStyleCombo.ItemIndex);
  PaintBox.Canvas.Pen.Color := PenColorBox.Selected;

  PaintBox.Canvas.Pen.Width := StrToInt(WidthCombo.Text);
  PaintBox.Canvas.Pen.Cosmetic := cbCosmetic.Checked;
  PaintBox.Canvas.Pen.EndCap := TPenEndCap(CapsCombo.ItemIndex);
  PaintBox.Canvas.Pen.JoinStyle := TPenJoinStyle(JoinCombo.ItemIndex);
  if PaintBox.Canvas.Pen.Style = psPattern then;
    PaintBox.Canvas.Pen.SetPattern(Dashes);

  // Must be called before setting Brush.Bitmap since that will reset Style to bsSolid
  PaintBox.Canvas.Brush.Color := BrushColorBox.Selected;

  if BrushStyleCombo.ItemIndex <> -1 then
    PaintBox.Canvas.Brush.Style := TBrushStyle(BrushStyleCombo.ItemIndex);

  if PaintBox.Canvas.Brush.Style in [bsPattern, bsImage] then
  begin
    if Paintbox.Canvas.Brush.Style = bsPattern then
      PaintBox.Canvas.Brush.Bitmap := FPattern
    else
      Paintbox.Canvas.Brush.Bitmap := FImage;
    SetBkColor(Paintbox.Canvas.Handle, BgColorBox.Selected);
    Paintbox.Canvas.Font.Color := BrushColorBox.Selected;
  end
  else begin
    PaintBox.Canvas.Brush.Bitmap := nil;
  end;

  SetBkMode(Paintbox.Canvas.Handle, BK_MODE[cbOpaque.Checked]);
  if cbOpaque.Checked then
  begin
    SetBkColor(Paintbox.Canvas.Handle, BgColorBox.Selected);
    Paintbox.Canvas.Font.Color := FontColorBox.Selected;  // Any color is sufficient for transparent Brush background here.
  end;

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
begin
  PaintBox.Invalidate;
end;

procedure TForm1.BrushStyleInfoBtnClick(Sender: TObject);
const
  INFO = 'The background of non-solid brushes can be filled by activating ' +
         'opaque text background. The background color is defined by the ' +
         'text background color. ' +
         LineEnding + LineEnding +
         'On Windows, the foreground and background colors of user-defined '+
         'patterns can be changed if the pattern bitmap is monochrome. The '+
         'foreground color is defined by the Brush.Color, the background color '+
         'by the text background color (SetBkColor).' +
         LineEnding + LineEnding +
         'The user-defined brush styles, bsImage and bsPicture, work in the same '+
         'way. A bitmap must be assigned to the Brush.Bitmap property. '+
         'A standard 24- or 32-bpp bitmap is rendered in color while a monochrome '+
         'bitmap is rendered such that white is replaced by the '+
         'text foreground color (Canvas.Font.Color), and black is replaced by '+
         'the text background color (SetBkColor). Note that this color '+
         'replacement is working only on Windows, and that user-defined brushes ' +
         'cannot be rendered transparently (unless the bitmap has 32 bpp on Linux).';
begin
  MessageDlg(INFO, mtInformation, [mbOK], 0);
end;

procedure TForm1.PenStyleInfoBtnClick(Sender: TObject);
const
  INFO = 'The gaps of dashed/dotted pen styles can be filled by activating '+
         'opaque text background. ' +
         'The gaps are filled by the text background color.' +
         'This does not work for user-defined patterns, though.' +
         'On Windows, this requires a "cosmetic" pen of line width 1.' +
         LineEnding + LineEnding +
         'A user-defined pattern is defined by an integer array assigned to the ' +
         'Pen.Pattern property. The 1st, 3rd, 5th etc array elements define the '+
         'lengths of the strokes, the 2nd, 4th, 6th etc elements the lengths of '+
         'the spacings between the strokes.';
begin
  MessageDlg(INFO, mtInformation, [mbOK], 0);
end;

end.

