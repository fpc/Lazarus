unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    grid: TStringGrid;
    procedure gridBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure gridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.gridBeforeSelection(Sender: TObject; aCol, aRow: Integer
  );
begin

  if Grid.Col<>aCol then
  begin
    grid.InvalidateCell(grid.Col, 0);
    grid.InvalidateCell(aCol, 0);
  end;

  if Grid.Row<>aRow then
  begin
    grid.InvalidateCell(0, grid.Row);
    grid.InvalidateCell(0, aRow);
  end;

end;

procedure TForm1.gridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if gdFixed in aState then
  begin
    if (aCol=grid.Col) or (aRow=grid.Row) then
      grid.Canvas.Brush.Color := clInactiveCaption;
  end;
end;

initialization
  {$I main.lrs}

end.

