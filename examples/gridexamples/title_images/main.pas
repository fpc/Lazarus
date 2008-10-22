unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    RadioGroup1: TRadioGroup;
    StringGrid1: TStringGrid;
    procedure RadioGroup1Click(Sender: TObject);
    procedure StringGrid1HeaderClick(
      Sender: TObject; IsColumn: Boolean;Index: Integer);
  private
    procedure Refresh;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  Buttons;

{ TForm1 }

procedure TForm1.RadioGroup1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to StringGrid1.Columns.Count - 1 do begin
    if RadioGroup1.ItemIndex>1 then
      StringGrid1.RowHeights[0] := 2*StringGrid1.DefaultRowHeight
    else
      StringGrid1.RowHeights[0] := StringGrid1.DefaultRowHeight;
    StringGrid1.Columns[i].Title.ImageLayout :=
      TButtonLayout(RadioGroup1.ItemIndex);
  end;
end;

procedure TForm1.Refresh;
var
  i, j: Integer;
  t: String;
begin
  with StringGrid1 do
    for i := 1 to RowCount - 2 do
      for j := i + 1 to RowCount - 1 do begin
        if
          (Columns[0].Title.ImageIndex = 1) and (Cells[1, i] > Cells[1, j]) or
          (Columns[0].Title.ImageIndex = 2) and (Cells[1, i] < Cells[1, j]) or
          (Columns[1].Title.ImageIndex = 1) and (StrToInt(Cells[2, i]) > StrToInt(Cells[2, j])) or
          (Columns[1].Title.ImageIndex = 2) and (StrToInt(Cells[2, i]) < StrToInt(Cells[2, j]))
        then begin
          t := Cells[1, i]; Cells[1, i] := Cells[1, j]; Cells[1, j] := t;
          t := Cells[2, i]; Cells[2, i] := Cells[2, j]; Cells[2, j] := t;
        end;
      end;
end;

procedure TForm1.StringGrid1HeaderClick(
  Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then exit;
  with StringGrid1.Columns[Index - 1].Title do begin
    if ImageIndex = 2 then
      ImageIndex := 0
    else
      ImageIndex := ImageIndex + 1;
    if ImageIndex > 0 then
      StringGrid1.Columns[2 - Index].Title.ImageIndex := 0;
  end;
  Refresh;
end;

initialization
  {$I main.lrs}

end.

