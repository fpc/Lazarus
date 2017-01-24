unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,
  TAGraph, TASeries, TAStyles, TASources, TAChartListbox;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnAddBar: TButton;
    BtnAddLevel: TButton;
    BtnDelete: TButton;
    CalculatedChartSource1: TCalculatedChartSource;
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    CbPercent: TCheckBox;
    ChartStyles1: TChartStyles;
    ListBox1: TListBox;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    procedure BtnAddBarClick(Sender: TObject);
    procedure BtnAddLevelClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure CbPercentChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  N = 5;

procedure TForm1.BtnAddBarClick(Sender: TObject);
var
  x, y: Double;
  i, n: Integer;
  style: TChartStyle;
begin
  x := ListChartSource1.Count;
  y := Random * 0.9 + 0.1;   // Random number between 0.1 and 1.0
  n := ListChartSource1.Add(x, y);
  for i:=0 to ListChartSource1.YCount-2 do
    ListChartSource1.Item[n]^.YList[i] := random;

  if ChartStyles1.Styles.Count = 0 then begin
    style := TChartStyle(ChartStyles1.Styles.Add);
    style.Brush.Color := RgbToColor(random(256), random(256), random(256));
    style.Text := 'Y';

    Listbox1.Items.Add(style.Text);
  end;

  BtnDelete.Enabled := true;
end;

procedure TForm1.BtnAddLevelClick(Sender: TObject);
var
  yidx: Integer;
  i: Integer;
  style: TChartStyle;
begin
  // Increment the number of y values and add random data to the new y value
  ListChartSource1.BeginUpdate;
  try
    yidx := ListChartSource1.YCount - 1;
    ListChartSource1.YCount := ListChartSource1.YCount + 1;
    for i := 0 to ListChartSource1.Count-1 do
      ListChartSource1.Item[i]^.YList[yidx] := random;
  finally
    ListChartSource1.EndUpdate;
  end;

  style := TChartStyle(ChartStyles1.Styles.Add);
  style.Brush.Color := RgbToColor(random(256), random(256), random(256));
  style.Text := 'YList[' + IntToStr(yidx) + ']';

  Listbox1.Items.Add(style.Text);
  Listbox1.ItemIndex := Listbox1.Items.Count - 1;

  BtnDelete.Enabled := true;
end;

procedure TForm1.BtnDeleteClick(Sender: TObject);
var
  idx: Integer;  // Index of level to be deleted
  i, j: Integer;
begin
  idx := Listbox1.ItemIndex;

  if (ListChartSource1.YCount = 1) then begin
    ListChartSource1.Clear;
    exit;
  end;

  // Delete level in Listsource
  ListChartSource1.BeginUpdate;
  try
    for i := 0 to ListChartSource1.Count-1 do begin
      if idx = 0 then
        ListChartSource1.Item[i]^.Y := ListChartSource1.Item[i]^.YList[0];
      for j:=idx to ListChartSource1.YCount-3 do
        ListChartSource1.Item[i]^.YList[j] := ListChartSource1.Item[i]^.YList[j+1];
    end;
    ListChartSource1.YCount := ListChartSource1.YCount - 1;
  finally
    ListChartSource1.EndUpdate;
  end;

  // Delete level in ChartStyles
  ChartStyles1.Styles.Delete(idx);

  // Delete item in Listbox
  Listbox1.Items.Delete(idx);
  if idx < Listbox1.Items.Count then
    Listbox1.ItemIndex := idx
  else
    Listbox1.ItemIndex := Listbox1.Items.Count - 1;

  BtnDelete.Enabled := Listbox1.Items.Count > 0;
end;

procedure TForm1.CbPercentChange(Sender: TObject);
begin
  CalculatedChartSource1.Percentage := CbPercent.Checked;
end;

end.

