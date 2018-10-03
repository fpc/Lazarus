unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, ComCtrls, db, DBGrids, memds, Forms, ExtCtrls, StdCtrls,
  TADbSource, TAGraph, TASeries, TACustomSource, Grids, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1PieSeries1: TPieSeries;
    ComboBox1: TComboBox;
    Datasource1: TDatasource;
    DbChartSource1: TDbChartSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    MemDataset1: TMemDataset;
    Panel1: TPanel;
    procedure ComboBox1Change(Sender: TObject);
    procedure DbChartSource1GetItem(ASender: TDbChartSource;
      var AItem: TChartDataItem);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure MemDataset1AfterPost(DataSet: TDataSet);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Graphics;

{ TForm1 }

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case Combobox1.ItemIndex of
    0:  // Get color from data field
      begin
        DbChartSource1.OnGetItem := nil;
        DbChartSource1.FieldColor := 'Color';
      end;
    1: // Get color from OnGetItem event
      begin
        DbChartSource1.FieldColor := '';
        DbChartSource1.OnGetItem := @DbChartSource1GetItem;
      end;
  end;
end;

procedure TForm1.DbChartSource1GetItem(ASender: TDbChartSource;
  var AItem: TChartDataItem);
const
  COLORS: array[1..3] of TColor = (clNavy, clBlue, clSkyBlue);
var
  s: String;
  i: integer;
begin
  DbChartSource1.DefaultGetItem(AItem);
  s := '';
  i := Length(AItem.Text);
  while (i > 0) and (AItem.Text[i] in ['0'..'9']) do begin
    s := AItem.Text[i] + s;
    dec(i);
  end;
  AItem.Color := COLORS[StrToInt(s)];
end;

procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if Column.Field.FieldName = 'Color' then begin
    DBGrid1.Canvas.Brush.Color := Column.Field.AsInteger;
    DBGrid1.Canvas.Rectangle(Rect.Left + 2, Rect.Top+2, Rect.Right-2, Rect.Bottom-2);
  end else
    DBGrid1.Canvas.TextOut(Rect.Left+2, Rect.Top+2, Column.Field.DisplayText);
end;

{ Add dummy data to start with }
procedure TForm1.FormCreate(Sender: TObject);
const
  N = 3;
var
  i: Integer;
  Fx, Fy, Ftxt, Fcolor: TField;
begin
  MemDataset1.Open;

  Fx := MemDataset1.FieldByName('X');
  Fy := MemDataset1.FieldByName('Y');
  Ftxt := MemDataset1.FieldByName('Txt');
  Fcolor := MemDataset1.FieldByName('Color');
  for i:= 1 to N do begin
    MemDataset1.Append;
    //Fx.AsInteger := i;    // Note: in an un-exploded pie series, x is not needed.
    if i=1 then Fx.AsFloat := 0.1 else Fx.AsFloat := 0;
    Fy.AsFloat := Random * (i+1);
    Ftxt.AsString := 'Item ' + IntToStr(i);
    FColor.AsInteger := RgbToColor(Random(255), Random(255), Random(255));
    MemDataset1.Post;
  end;
end;

procedure TForm1.MemDataset1AfterPost(DataSet: TDataSet);
begin
  Chart1.Invalidate;
end;

end.

