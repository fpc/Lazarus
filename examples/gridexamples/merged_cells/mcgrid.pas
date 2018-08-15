unit mcgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids;

type
  TDrawCellTextEvent = procedure (Sender: TObject; ACol, ARow: Integer;
    ARect: TRect; AState: TGridDrawState; AText: String;
    var Handled: Boolean) of object;

  TMergeCellsEvent = procedure (Sender: TObject; ACol, ARow: Integer;
    var ALeft, ATop, ARight, ABottom: Integer) of object;

  { TMCStringGrid: MC = "merged cells" }

  TMCStringGrid = class(TStringGrid)
  private
    FMergeLock: Integer;
    FOnMergeCells: TMergeCellsEvent;
    FOnDrawCellText: TDrawCellTextEvent;
  protected
    procedure CalcCellExtent(ACol, ARow: Integer; var ARect: TRect); override;
    procedure DoEditorShow; override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawCellText(ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState; AText: String); override;
    function GetCells(ACol, ARow: Integer): String; override;
    function GetEditText(ACol, ARow: Integer): String; override;
    function IsMerged(ACol, ARow: Integer): Boolean; overload;
    function IsMerged(ACol, ARow: Integer;
      out ALeft, ATop, ARight, ABottom: Integer): Boolean; overload;
    procedure MoveSelection; override;
    procedure PrepareCanvas(aCol, aRow: Integer; AState: TGridDrawState); override;
    procedure SetEditText(ACol, ARow: LongInt; const Value: String); override;
    function  MoveNextSelectable(Relative: Boolean; DCol, DRow: Integer): Boolean; override;
  published
    property OnDrawCelLText: TDrawCellTextEvent read FOnDrawCellText write FOnDrawCellText;
    property OnMergeCells: TMergeCellsEvent read FOnMergeCells write FOnMergeCells;
  end;


implementation

{ Calculates the size of the merged block }
procedure TMCStringGrid.CalcCellExtent(ACol, ARow: Integer; var ARect: TRect);
var
  L, T, R, B, dummy: Integer;
begin
  if IsMerged(ACol, ARow, L, T, R, B) then begin
    ColRowToOffset(true, true, L, ARect.Left, dummy);
    ColRowToOffset(true, true, R, dummy, ARect.Right);
    ColRowToOffset(false, true, T, ARect.Top, dummy);
    ColRowToOffset(false, true, B, dummy, ARect.Bottom);
  end else
    // Call the inherited procedure to handle non-merged cells
    inherited;
end;

{ Make sure that the cell editor of a merged block is the same size as the
  merged block }
procedure TMCStringGrid.DoEditorShow;
var
  R: TRect;
begin
  inherited;
  if (goColSpanning in Options) and Assigned(Editor) then begin
    R := CellRect(Col, Row);
    CalcCellExtent(Col, Row, R);
    Editor.SetBounds(R.Left, R.Top, R.Right-R.Left-1, R.Bottom-R.Top-1);
  end;
end;

procedure TMCStringGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  L, T, R, B: Integer;
begin
  if IsMerged(aCol, aRow, L, T, R, B) and ((aCol<>L) or (aRow<>T)) then
    // nothing to draw
  else
    inherited DrawCell(aCol, aRow, aRect, aState);
end;

{ Draws the cell text. Allows to hook in an external painting routine which
  will replace the built-in painting routine if it sets "Handled" to true. }
procedure TMCStringGrid.DrawCellText(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState; AText: String);
var
  handled: Boolean;
begin
  handled := false;
  if Assigned(FOnDrawCellText) then
    FOnDrawCellText(Self, ACol, ARow, ARect, AState, AText, handled);
  if not handled then
    inherited;
end;

{ Returns the string to be displayed in the specified cell. In case of a merged
  block only the text assigned to the top-left cell of the block is used. }
function TMCStringGrid.GetCells(ACol, ARow: Integer): String;
var
  L, T, R, B: Integer;
begin
  if (FMergeLock = 0) and IsMerged(ACol, ARow, L, T, R, B) then
    Result := inherited GetCells(L, T)
  else
    Result := inherited GetCells(ACol, ARow);
end;

{ Make sure to use only the topleft cell of a merged block for editing }
function TMCStringGrid.GetEditText(ACol, ARow: Integer): String;
begin
  Result := GetCells(ACol, ARow);
  if Assigned(OnGetEditText) then OnGetEditText(self, ACol, ARow, Result);
end;

{ Check whether the specified cell belongs to a merged block}
function TMCStringGrid.IsMerged(ACol, ARow: Integer): Boolean;
var
  L, T, R, B: Integer;
begin
  Result := IsMerged(ACol, ARow, L, T, R, B);
end;

{ Checks whether the specified cell belongs to a merged block and returns the
  cell coordinate of the block extent }
function TMCStringGrid.IsMerged(ACol,ARow: Integer;
  out ALeft, ATop, ARight, ABottom: Integer): Boolean;
var
  tmp: Integer;
begin
  Result := false;
  if not (goColSpanning in Options) then exit;
  if not Assigned(FOnMergeCells) then exit;
  inc(FMergeLock);

  ALeft := ACol;
  ARight := ACol;
  ATop := ARow;
  ABottom := ARow;
  FOnMergeCells(Self, ACol, ARow, ALeft, ATop, ARight, ABottom);
  if ALeft > ARight then begin
    tmp := ALeft;
    ALeft := ARight;
    ARight := tmp;
  end;
  if ATop > ABottom then begin
    tmp := ATop;
    ATop := ABottom;
    ABottom := tmp;
  end;
  Result := (ALeft <> ARight) or (ATop <> ABottom);
  dec(FMergeLock);
end;

{ Repaints the entire grid after the selection is moved because normally only
  the selected cell would be painted, and this would result in an imcompletely
  painted merged block }
procedure TMCStringGrid.MoveSelection;
begin
  if SelectActive then
    InvalidateGrid;
  inherited;
end;

{ Makes sure that all cells of the merged block are drawn as selected/focused,
  not just the active cell }
procedure TMCStringGrid.PrepareCanvas(aCol, aRow: Integer;
  AState: TGridDrawState);
var
  L, T, R, B: Integer;
begin
  if IsMerged(ACol, ARow, L, T, R, B) and
    (Col >= L) and (Col <= R) and (Row >= T) and (Row <= B) and
    not ((ACol = Col) and (ARow = Row))
  then
    AState := AState + [gdSelected, gdFocused];
  inherited;
end;

{ Writes the edited text back into the grid. Makes sure that, in case of a
  merged block, the edited text is assigned to the top/left cell }
procedure TMCStringGrid.SetEditText(ACol, ARow: LongInt; const Value: String);
var
  L, T, R, B: Integer;
begin
  if IsMerged(ACol, ARow, L,T,R,B) then
    inherited SetEditText(L, T, Value)
  else
    inherited SetEditText(ACol, ARow, Value);
end;

function TMCStringGrid.MoveNextSelectable(Relative: Boolean; DCol, DRow: Integer
  ): Boolean;
var
  L, T, R, B: Integer;
begin
  if Relative and IsMerged(Col, Row, L, T, R, B) then begin
    // we are only interested on relative movement (basically by keyboard)
    if DCol>0 then DCol := R - Col + 1 else
    if DCol<0 then DCol := L - Col - 1 else
    if DRow>0 then DRow := B - Row + 1 else
    if DRow<0 then DRow := T - Row - 1;
  end;
  Result := inherited MoveNextSelectable(Relative, DCol, DRow);
end;

end.

