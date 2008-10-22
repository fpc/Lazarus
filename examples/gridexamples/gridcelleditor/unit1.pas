unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    StringGrid1: TStringGrid;
    procedure ComboBox1EditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
  public
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  y: Integer;
  x: Integer;
begin
  // fill the grid
  for x:=0 to StringGrid1.ColCount-1 do begin
    for y:=0 to StringGrid1.RowCount-1 do begin
      StringGrid1.Cells[x,y]:=IntToStr(x)+','+IntToStr(y);
    end;
  end;
end;

procedure TForm1.ComboBox1EditingDone(Sender: TObject);
begin
  StringGrid1.Cells[StringGrid1.Col,StringGrid1.Row]:=ComboBox1.Text;
end;

procedure TForm1.StringGrid1SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if (aCol=3) and (aRow>0) then begin
    ComboBox1.BoundsRect:=StringGrid1.CellRect(aCol,aRow);
    ComboBox1.Text:=StringGrid1.Cells[StringGrid1.Col,StringGrid1.Row];
    Editor:=ComboBox1;
  end;
end;

initialization
  {$I unit1.lrs}

end.

