unit comain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, FileUtil, Forms, Graphics, Grids, ComCtrls,
  SysUtils, Types;

type

  TStringGrid = class(Grids.TStringGrid)
  protected
    procedure ColWidthsChanged; override;
    procedure DoEditorHide; override;
    procedure DoEditorShow; override;
    procedure DrawRow(ARow: Integer); override;
    function OverflowCellRect(ACol, ARow: integer; AState: TGridDrawState;
      out ANumColsNeededAtRight: Integer): TRect; overload;
    function OverflowCellRect(ACol, ARow: Integer): TRect; overload;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGridClick(Sender: TObject);
    procedure StringGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ Helper routines copied from grids.pas }

function HorizontalIntersect(const aRect,bRect: TRect): boolean;
begin
  result := (aRect.Left < bRect.Right) and (aRect.Right > bRect.Left);
end;

function VerticalIntersect(const aRect,bRect: TRect): boolean;
begin
  result := (aRect.Top < bRect.Bottom) and (aRect.Bottom > bRect.Top);
end;


{------------------------------------------------------------------------------}
{                        Extended StringGrid                                   }
{------------------------------------------------------------------------------}

{ Makes sure that overflowing cells are painted correctly if they a column
  width changes. }
procedure TStringGrid.ColWidthsChanged;
begin
  inherited;
  InvalidateGrid;
end;

{ After editing a repaint of the current row is needed because the edited cell
  may overflow differently }
procedure TStringGrid.DoEditorHide;
begin
  inherited;
  InvalidateRow(Row);
end;

{ Make sure that the cell editor has the same size as the OverflowCellRect }
procedure TStringGrid.DoEditorShow;
begin
  inherited;
  with OverflowCellRect(Col, Row) do begin
    Editor.Width := Right - Left - 4;
  end;
  InvalidateRow(Row);
end;

{ Draws an entire row to correctly process overflowing cells.
  Most of the code is copied from TCustomGrid.DrawRow }
procedure TStringGrid.DrawRow(ARow: Integer);
var
  gds: TGridDrawState;
  aCol: Integer;
  ncols: Integer;
  Rs: Boolean;
  R: TRect;
  tmpR: TRect;
  ClipArea: Trect;

  function IsPushCellActive: boolean;
  begin
    with GCache do
      result := (PushedCell.X<>-1) and (PushedCell.Y<>-1);
  end;

  procedure DoDrawCell;
  begin
    with GCache do begin
      if (aCol=HotCell.x) and (aRow=HotCell.y) and not IsPushCellActive() then begin
        Include(gds, gdHot);
        HotCellPainted := True;
      end;
      if ClickCellPushed and (aCol=PushedCell.x) and (aRow=PushedCell.y) then begin
        Include(gds, gdPushed);
      end;
    end;

    Canvas.SaveHandleState;
    try
      InterSectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      DrawCell(aCol, aRow, R, gds);
    finally
      Canvas.RestoreHandleState;
    end;
  end;

begin
  // Upper and Lower bounds for this row
  ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);

  // is this row within the ClipRect?
  ClipArea := Canvas.ClipRect;
  if (R.Top>=R.Bottom) or not VerticalIntersect(R, ClipArea) then begin
    {$IFDEF DbgVisualChange}
    DebugLn('Drawrow: Skipped row: ', IntToStr(aRow));
    {$ENDIF}
    exit;
  end;

  // Draw columns in this row
  with GCache.VisibleGrid do begin
    aCol := FixedCols;
    while (aCol < ColCount) do begin
      gds := GetGridDrawState(ACol, ARow);
      with OverflowCellRect(ACol, ARow, gds, ncols) do begin
        R.Left := Left;
        R.Right := Right;
      end;
      if (R.Left < R.Right) and HorizontalIntersect(R, ClipArea) then
        DoDrawCell;
      inc(aCol, ncols);
    end;

    Rs := (goRowSelect in Options);
    // Draw the focus Rect
    if FocusRectVisible and (ARow = Row) and
       ((Rs and (ARow >= Top) and (ARow <= Bottom)) or IsCellVisible(Col, ARow))
    then begin
      if not EditorMode then begin
        if Rs then
          CalcFocusRect(R, false) // will be adjusted when calling DrawFocusRect
        else
          ColRowToOffset(True, True, Col, R.Left, R.Right);
        // is this column within the ClipRect?
        if HorizontalIntersect(R, ClipArea) then
          DrawFocusRect(Col, Row, R);
      end;
    end;
  end;

  // Draw fixed columns
  For aCol:=0 to FixedCols-1 do begin
    gds := [gdFixed];
    ColRowToOffset(True, True, aCol, R.Left, R.Right);
    // Is this column within the ClipRect?
    if (R.Left<R.Right) and HorizontalIntersect(R, ClipArea) then
      DoDrawCell;
  end;
end;

{ Calculates the rectangle coordinates of the cell block covered by the
  overflowing cell at the specified column and row }
function TStringGrid.OverflowCellRect(ACol, ARow: Integer): TRect;
var
  n: Integer;
begin
  Result := OverflowCellRect(ACol, ARow, [], n);
end;

{ Calculates the pixel coordinates of the cell block covered by the
  overflowing cell at the specified column and row, and retunrs in
  ANumColsNeededAtRight the number of columns covered by the cell at
  ACol/ARow when going to the right }
function TStringGrid.OverflowCellRect(ACol, ARow: integer; AState: TGridDrawState;
  out ANumColsNeededAtRight: Integer): TRect;
var
  lIndex: integer;
  ts: TTextStyle;
  lCol, rCol: Integer;
  len: Integer;
begin
  Result := CellRect(ACol, ARow);
  ANumColsNeededAtRight := 1;
  len := Canvas.TextWidth(Cells[ACol, ARow]) + 2*constCellPadding;
  if len > ColWidths[ACol] then begin
    PrepareCanvas(ACol, ARow, AState);
    ts := Canvas.TextStyle;
    case ts.Alignment of
      taLeftJustify:
        for lIndex := ACol + 1 to ColCount - 1 do
          if (Cells[lIndex, ARow] = EmptyStr) then
          begin
            Result.Right := CellRect(lIndex, ARow).Right;
            ANumColsNeededAtRight := lIndex - ACol + 1;
            if Result.Right - Result.Left > len then Break;
          end else
            Break;
      taRightJustify:
        for lIndex := ACol - 1 downto FixedCols do
          if (Cells[lIndex, ARow] = EmptyStr) then
          begin
            Result.Left := CellRect(lIndex, ARow).Left;
            if Result.Right - Result.Left > len then Break;
          end else
            Break;
      taCenter:
        begin
          lIndex := 1;
          while true do begin
            lCol := ACol - lIndex;
            rCol := ACol + lIndex;
            if (lCol < FixedCols) or (rCol >= ColCount) then
              break;
            if (Cells[rCol, ARow] = EmptyStr) and (Cells[lCol, ARow] = EmptyStr)
            then begin
              Result.Left := CellRect(lCol, ARow).Left;
              Result.Right := CellRect(rCol, ARow).Right;
              ANumColsNeededAtRight := rCol - ACol + 1;
              if Result.Right - Result.Left > len then Break;
            end else
              Break;
            inc(lIndex);
          end;
        end;
    end;
  end;
end;


{------------------------------------------------------------------------------}
{                                  TMainForm                                   }
{------------------------------------------------------------------------------}

{ Sets up basic grid content }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  StringGrid.Cells[1, 2] := 'ABC';
  StringGrid.Cells[1, 1] := 'This is a long string for my string grid';
  StringGrid.Cells[StringGrid.ColCount-1, 1] := '124';
  StringGrid.Cells[StringGrid.ColCount-1, StringGrid.RowCount-1] := 'Another long text, right-aligned now';
  StringGrid.Cells[4, 6] := 'This is a long string which is centered.';
end;

{ Displays information on the clicked cell in the status bar }
procedure TMainForm.StringGridClick(Sender: TObject);
begin
  Statusbar.SimpleText := Format('Column %d, row %d, text: "%s"',
    [StringGrid.Col, StringGrid.Row, StringGrid.Cells[StringGrid.Col, StringGrid.Row]]);
end;

{ Prepares centered text in column 4, and right-aligned text in the last column}
procedure TMainForm.StringGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if (aCol = StringGrid.ColCount-1) then begin
    ts := StringGrid.Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    StringGrid.Canvas.TextStyle := ts;
  end else
  if (aCol = 4) then begin
    ts := StringGrid.Canvas.TextStyle;
    ts.Alignment := taCenter;
    StringGrid.Canvas.TextStyle := ts;
  end;
end;

end.
