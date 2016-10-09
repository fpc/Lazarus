unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, mcGrid;

type
  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    Grid: TMCStringGrid;
    procedure DrawCellTextHandler(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState; AText: String; var Handled: Boolean);
    procedure MergeCellsHandler(Sender: TObject; ACol, ARow: Integer;
      var ALeft, ATop, ARight, ABottom: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create an instance of TStringGridEx at runtime for testing
  Grid := TMCStringGrid.Create(Self);
  Grid.Parent := self;
  Grid.Align := alClient;
  Grid.RowCount := 20;
  Grid.ColCount := 10;
  Grid.Cells[1, 0] := 'Merged';
  Grid.Cells[3, 0] := 'Single';
  Grid.Cells[1, 1] := 'combined';
  Grid.Cells[3, 1] := 'abc';
  Grid.Cells[4, 1] := 'bold';
  Grid.Cells[5, 1] := 'Image';
  Grid.Cells[2, 7] := 'Image';
  Grid.Cells[2, 3] := 'This is a long text' + LineEnding + 'with line break.';
  Grid.Cells[0, 2] := 'Vertical text';
  Grid.Cells[0, 6] := 'Centered';
  Grid.OnDrawCellText := @DrawCellTextHandler;
  Grid.OnMergeCells := @MergeCellsHandler;
  Grid.Options := Grid.Options + [goColSpanning, goEditing, goDrawFocusSelected];
  if Grid.DefaultRowHeight < ImageList1.Height + 4 then
    Grid.DefaultRowHeight := ImageList1.Height + 4
end;

{ This event handler takes care of painting the cell text. Normally it is not
  needed for merged cells, but it implements here vertical text direction,
  line breaks etc. }
procedure TForm1.DrawCellTextHandler(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState; AText: String; var Handled: Boolean);
var
  ts: TTextStyle;
  x, y: Integer;
  bmp: TBitmap;
begin
  Handled := True;
  if (ACol in [2..4]) and (ARow in [3..5]) then
  begin
    // Word-wrapped text
    ts := Grid.Canvas.TextStyle;
    ts.SingleLine := false;
    ts.Wordbreak := true;
    x := ARect.Left + constCellPadding;
    y := ARect.Top + constCellPadding;
    Grid.Canvas.TextRect(ARect, x, y, AText, ts);
  end else
  if (ACol = 0) and (ARow in [2..5]) then
  begin
    // Vertical text
    Grid.Canvas.Font.Orientation := 900;
    x := (ARect.Left + ARect.Right - Grid.Canvas.TextHeight('Tg')) div 2;
    y := ARect.Bottom - constCellPadding;
    Grid.Canvas.TextOut(x, y, AText);
    Grid.Canvas.Font.Orientation := 0;
  end else
  if (ACol = 0) and (ARow = 6) then
  begin
    // Centered text
    ts := Grid.Canvas.TextStyle;
    ts.Alignment := taCenter;
    ts.Layout := tlCenter;
    x := (ARect.Left + ARect.Right) div 2;
    y := (ARect.Top + ARect.Bottom) div 2;
    Grid.Canvas.TextRect(ARect, x, y, AText, ts);
  end else
  if (ACol = 4) and (ARow = 1) then
  begin
    // Bold text
    Grid.Canvas.Font.Style := [fsBold];
    x := ARect.Left + constCellPadding;
    y := ARect.Top + constCellPadding;
    Grid.Canvas.TextOut(x, y, AText);
    Grid.Canvas.Font.Style := [];
  end else
  if (ACol = 5) and (ARow = 1) then
  begin
    // Cell with image
    bmp := TBitmap.Create;
    try
      ImageList1.GetBitmap(0, bmp);
      x := ARect.Left + constCellPadding;
      y := (ARect.Top + ARect.Bottom - bmp.Height) div 2;
      Grid.Canvas.Draw(x, y, bmp);
      inc(x, bmp.Width + constCellpadding);
      y := ARect.Top + constCellPadding;
      Grid.Canvas.TextOut(x, y, AText);
    finally
      bmp.Free;
    end;
  end else
    Handled := false;
end;

{ This is the event handler defining the merged block. In this example we
  assume that the cells defined by columns 2..3 and rows 3..5 are will be merged }
procedure TForm1.MergeCellsHandler(Sender: TObject; ACol, ARow: Integer;
  var ALeft, ATop, ARight, ABottom: Integer);
begin
  // Define a merged block which is a single row heigh
  if (ACol in [1..2]) and (ARow = 1) then begin
    ALeft := 1;
    ARight := 2;
  end else
  // Define a merged block covering several columns and rows (for the word-wrap text)
  if (ACol in [2..3]) and (ARow in [3..5]) then begin
    ALeft := 2;
    ARight := 3;
    Atop := 3;
    ABottom := 5;
  end;
  // Define a merged block in the column headers
  if (ACol in [1..2]) and (ARow = 0) then begin
    ALeft := 1;
    ARight := 2;
  end else
  // Define a merged block in the row headers (for the vertical text)
  if (ACol = 0) and (ARow in [2..5]) then begin
    ATop := 2;
    ABottom := 5;
  end else
  // Merge the next two cells adjacent to cell with text 'Image'
  if (ACol > 1) and (Grid.Cells[ACol-1, ARow] = 'Image') then begin
    ALeft := ACol;
    ARight := ALeft + 1;
  end else
  if (ACol > 2) and (Grid.Cells[ACol-2, ARow] = 'Image') then begin
    ALeft := ACol - 1;
    ARight := ALeft + 1;
  end;
end;

end.

