unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Buttons, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    grid: TStringGrid;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    SaveButton: TToolButton;
    LoadButton: TToolButton;
    procedure gridBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure gridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure gridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    { private declarations }
    function IndexToAlphaIndex(AIndex: Integer): string;
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

procedure TForm1.gridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);

  procedure HorizontalCenter;
  var
    aTextStyle : TTextStyle;
  begin
    aTextStyle := grid.Canvas.TextStyle;
    aTextStyle.Alignment:=taCenter;
    grid.Canvas.TextStyle:=aTextStyle;
  end;

begin

  if gdFixed in aState then
  begin
    if (aCol=0) and (aRow>=Grid.FixedRows) then
    begin
      HorizontalCenter;
      grid.Canvas.TextRect(aRect, aRect.Left, aRect.Top, IntToStr(aRow));
      exit;
    end else
    if (aRow=0) and (aCol>=Grid.FixedCols) then
    begin
      HorizontalCenter;
      grid.Canvas.TextRect(aRect, aRect.Left, aRect.Top,
                                 IndexToAlphaIndex(aCol-Grid.FixedCols));
      exit;
    end;
  end;

  grid.DefaultDrawCell(aCol,aRow,aRect,aState);
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

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    grid.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    grid.SaveToFile(SaveDialog1.FileName);
end;

function TForm1.IndexToAlphaIndex(AIndex: Integer): string;
var
  i: Integer;
begin
  Result := chr((AIndex mod 26) + ord('A'));
  i := (AIndex div 26)-1;
  if i>25 then
    result := '['+IntToStr(AIndex)+']'
  else
  if i>=0 then
    result := chr(i + ord('A')) + Result;
end;

initialization
  {$I main.lrs}

end.

