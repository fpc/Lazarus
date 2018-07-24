unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ValEdit, Grids, TAGraph, TASources, TASeries, TAChartListbox,
  TAFuncSeries, TADrawUtils, TAChartCombos, TACustomSource;

type

  { TMainForm }

  TMainForm = class(TForm)
    CbYErrDifferent: TCheckBox;
    CbYErrShow: TCheckBox;
    Chart1: TChart;
    Chart1BSplineSeries1: TBSplineSeries;
    Chart1CubicSplineSeries1: TCubicSplineSeries;
    Chart1FitSeries1: TFitSeries;
    Chart1LineSeries1: TLineSeries;
    CbRotated: TCheckBox;
    CbErrPenStyle: TChartComboBox;
    ChartListbox1: TChartListbox;
    CbXErrShow: TCheckBox;
    CbXErrDifferent: TCheckBox;
    EdXErrConst: TFloatSpinEdit;
    EdYErrConst: TFloatSpinEdit;
    EdXErrPercent: TFloatSpinEdit;
    EdYErrPercent: TFloatSpinEdit;
    GbXErr: TGroupBox;
    GbYErr: TGroupBox;
    GbErrorBarStyle: TGroupBox;
    Label1: TLabel;
    LblErrLineWidth: TLabel;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    RgChartSource: TRadioGroup;
    RbYErrChartSource: TRadioButton;
    RbXErrConst: TRadioButton;
    RbYErrConst: TRadioButton;
    RbXErrPercent: TRadioButton;
    RbXErrChartSource: TRadioButton;
    RandomChartSource1: TRandomChartSource;
    RbYErrPercent: TRadioButton;
    EdErrPenWidth: TSpinEdit;
    StringGrid1: TStringGrid;
    UserDefinedChartSource1: TUserDefinedChartSource;

    procedure CbErrPenStyleChange(Sender: TObject);
    procedure CbRotatedChange(Sender: TObject);

    procedure CbXErrShowChange(Sender: TObject);
    procedure CbYErrShowChange(Sender: TObject);

    procedure CbXErrDifferentChange(Sender: TObject);
    procedure CbYErrDifferentChange(Sender: TObject);

    procedure Chart1AfterPaint(ASender: TChart);

    procedure EdErrPenWidthChange(Sender: TObject);

    procedure EdXErrConstChange(Sender: TObject);
    procedure EdYErrConstChange(Sender: TObject);

    procedure EdXErrPercentChange(Sender: TObject);
    procedure EdYErrPercentChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);

    procedure RbXErrConstChange(Sender: TObject);
    procedure RbYErrConstChange(Sender: TObject);

    procedure RbXErrPercentChange(Sender: TObject);
    procedure RbYErrPercentChange(Sender: TObject);

    procedure RbXErrChartSourceChange(Sender: TObject);
    procedure RbYErrChartSourceChange(Sender: TObject);

    procedure RgChartSourceClick(Sender: TObject);

    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);

    procedure UserDefinedChartSource1GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);

  private
    procedure PopulateListSource;
    procedure PopulateUserData;
    procedure SetXErrorBarParams;
    procedure SetYErrorBarParams;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math;

type
  TDataRec = record
    x, y, dxp, dxn, dyp, dyn: Double;
  end;

var
  Data: array of TDataRec;

type
  TMySource = class(TCustomChartSource)
  public
    property XErrorBarData;
    property YErrorBarData;
  end;

{ TMainForm }

procedure TMainForm.CbRotatedChange(Sender: TObject);
begin
  if CbRotated.Checked then begin
    Chart1LineSeries1.AxisIndexX := 0;
    Chart1LineSeries1.AxisIndexY := 1;
  end else begin
    Chart1LineSeries1.AxisIndexX := 1;
    Chart1LineSeries1.AxisIndexY := 0;
  end;

  Chart1BSplineSeries1.AxisIndexX := Chart1lineSeries1.AxisIndexX;
  Chart1BSplineSeries1.AxisIndexY := Chart1LineSeries1.AxisIndexY;

  Chart1CubicSplineSeries1.AxisIndexX := Chart1lineSeries1.AxisIndexX;
  Chart1CubicSplineSeries1.AxisIndexY := Chart1LineSeries1.AxisIndexY;

  Chart1FitSeries1.AxisIndexX := Chart1lineSeries1.AxisIndexX;
  Chart1FitSeries1.AxisIndexY := Chart1LineSeries1.AxisIndexY;
end;

procedure TMainForm.CbXErrShowChange(Sender: TObject);
begin
  Chart1LineSeries1.XErrorBars.Visible := CbXErrShow.Checked;
  Chart1BSplineSeries1.XErrorBars.Visible := CbXErrShow.Checked;
  Chart1CubicSplineSeries1.XErrorBars.Visible := CbXErrShow.Checked;
  Chart1FitSeries1.XErrorBars.Visible := CbXErrShow.Checked;
end;

procedure TMainForm.CbXErrDifferentChange(Sender: TObject);
begin
  SetXErrorBarParams;
end;

procedure TMainForm.CbYErrDifferentChange(Sender: TObject);
begin
  SetYErrorBarParams;
end;

procedure TMainForm.CbYErrShowChange(Sender: TObject);
begin
  Chart1LineSeries1.YErrorBars.Visible := CbYErrShow.Checked;
  Chart1BSplineSeries1.YErrorBars.Visible := CbYErrShow.Checked;
  Chart1CubicSplineSeries1.YErrorBars.Visible := CbYErrShow.Checked;
  Chart1FitSeries1.YErrorBars.Visible := CbYErrShow.Checked;
end;

procedure TMainForm.Chart1AfterPaint(ASender: TChart);
const
  EPS = 1E-9;
var
  i: Integer;
  src: TCustomChartSource;
  x, y, dxp, dxn, dyp, dyn: Double;
  fx, fy: Double;
  sp, sn: String;
begin
  src := Chart1LineSeries1.Source;
  StringGrid1.RowCount := src.Count + 1;
  fx := EdXErrPercent.Value * 0.01;
  fy := EdYErrPercent.Value * 0.01;
  for i:=1 to StringGrid1.RowCount-1 do begin
    x := src.Item[i-1]^.X;
    y := src.Item[i-1]^.Y;
    src.GetXErrorBarValues(i-1, dxp, dxn);
    src.GetYErrorBarValues(i-1, dyp, dyn);
    StringGrid1.Cells[0, i] := Format('%.3f', [x]);
    StringGrid1.Cells[1, i] := Format('%.3f', [y]);
    if (dxp = 0) and (dxn = 0) then
      StringGrid1.Cells[2, i] := ''
    else if SameValue(dxp, dxn, EPS) then
      StringGrid1.Cells[2, i] := Format('±%.3f', [dxp])
    else
      StringGrid1.Cells[2, i] := Format('+%.2f/-%.2f', [dxp, dxn]);
    if (dyp = 0) and (dyn = 0) then
      StringGrid1.Cells[3, i] := ''
    else if SameValue(dyp, dyn, EPS) then
      StringGrid1.Cells[3, i] := Format('±%.3f', [dyp])
    else
      StringGrid1.Cells[3, i] := Format('+%.2f/-%.2f', [dyp, dyn]);
  end;
end;

procedure TMainForm.CbErrPenStyleChange(Sender: TObject);
begin
  Chart1LineSeries1.XErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1BSplineSeries1.XErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1CubicSplineSeries1.XErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1FitSeries1.XErrorBars.Pen.Style := CbErrPenStyle.PenStyle;

  Chart1LineSeries1.YErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1BSplineSeries1.YErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1CubicSplineSeries1.YErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
  Chart1FitSeries1.YErrorBars.Pen.Style := CbErrPenStyle.PenStyle;
end;

procedure TMainForm.EdXErrConstChange(Sender: TObject);
begin
  SetXErrorBarParams;
end;

procedure TMainForm.EdErrPenWidthChange(Sender: TObject);
begin
  Chart1LineSeries1.XErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1BSplineSeries1.XErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1CubicSplineSeries1.XErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1FitSeries1.XErrorBars.Pen.Width := EdErrPenWidth.Value;

  Chart1LineSeries1.YErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1BSplineSeries1.YErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1CubicSplineSeries1.YErrorBars.Pen.Width := EdErrPenWidth.Value;
  Chart1FitSeries1.YErrorBars.Pen.Width := EdErrPenWidth.Value;
end;

procedure TMainForm.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := StringGrid1.Canvas.TextStyle;
  ts.Alignment := taCenter;
  StringGrid1.Canvas.TextStyle := ts;
end;

procedure TMainForm.UserDefinedChartSource1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data[AIndex].X;
  AItem.Y := Data[AIndex].Y;
  if ASource.XCount > 1 then
    AItem.XList[0] := Data[AIndex].dxp;
  if ASource.XCount > 2 then
    AItem.XList[1] := Data[AIndex].dxn;
  if ASource.YCount > 1 then
    AItem.YList[0] := Data[AIndex].dyp;
  if ASource.YCount > 2 then
    AItem.YList[1] := Data[AIndex].dyn;
end;

procedure TMainForm.EdXErrPercentChange(Sender: TObject);
begin
  SetXErrorBarParams;
end;

procedure TMainForm.EdYErrConstChange(Sender: TObject);
begin
  SetYErrorBarParams;
end;

procedure TMainForm.EdYErrPercentChange(Sender: TObject);
begin
  SetYErrorBarParams;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PopulateUserData;
end;

procedure TMainForm.PopulateListSource;
const
  N = 10;
  XMIN = 0;
  XMAX = 10;
var
  i: Integer;
  x, y: Double;
  idx: Integer;
  fx, fy: Double;
begin
  RandSeed := 1;
  fx := EdXErrPercent.Value * 0.01 * (XMAX - XMIN);
  fy := EdYErrPercent.Value * 0.01 * (XMAX - XMIN);

  ListChartSource1.Clear;
//  ListChartSource1.XCount := IfThen(CbXErrShow.Checked and RbXErrChartSource.Checked, 2, 1);
//  ListChartSource1.YCount := IfThen(CbYErrShow.Checked and RbYErrChartSource.Checked, 2, 1);
  ListChartSource1.BeginUpdate;
  try
    for i:=0 to N-1 do begin
      x := XMIN + (XMAX - XMIN) * i / (N-1);
      y := sqr(x - 5) / 2.5 + (random*2 - 1) * 0.5;
      idx := ListChartSource1.Add(x, y);
      if ListChartSource1.XCount > 1 then
        ListChartSource1.Item[idx]^.SetX(1, random * fx);
      if ListChartSource1.XCount > 2 then
        ListChartSource1.Item[idx]^.SetX(2, random * fx);
      if ListChartSource1.YCount > 1 then
        ListChartSource1.Item[idx]^.SetY(1, random * fy);
      if ListChartSource1.YCount > 2 then
        ListChartSource1.Item[idx]^.SetY(2, random * fy);
    end;
  finally
    ListChartSource1.EndUpdate;
  end;
end;

procedure TMainForm.PopulateUserData;
const
  N = 10;
  XMIN = 0;
  XMAX = 10;
var
  i: Integer;
  x, y: Double;
  fx, fy: Double;
begin
  RandSeed := 2;
  fx := EdXErrConst.Value;
  fy := EdYErrConst.Value;

  SetLength(Data, N);
  for i := 0 to N - 1 do begin
    x := XMIN + (XMAX - XMIN) * i / (N-1);
    y := 10 - sqr(x - 5) / 2.5 + (random*2 - 1) * 0.5;
    Data[i].x := x;
    Data[i].y := y;
    Data[i].dxp := random*fx;
    Data[i].dxn := 0.5 * Data[i].dxp;
    Data[i].dyp := random * fy;
    Data[i].dyn := 0.5 * Data[i].dyp;
  end;

  UserDefinedChartSource1.PointsNumber := N;
end;

procedure TMainForm.RgChartSourceClick(Sender: TObject);
var
  src: TCustomChartSource;
begin
  case RgChartSource.ItemIndex of
    0: begin
         Chart1LineSeries1.Source := RandomChartSource1;
         Chart1BSplineSeries1.Source := RandomChartSource1;
         Chart1CubicSplineSeries1.Source := RandomChartSource1;
         Chart1FitSeries1.Source := RandomChartSource1;
       end;
    1: begin
         Chart1LineSeries1.Source := ListChartSource1;
         Chart1BSplineSeries1.Source := ListChartSource1;
         Chart1CubicSplineSeries1.Source := ListChartSource1;
         Chart1FitSeries1.Source := ListChartSource1;
       end;
    2: begin
         Chart1LineSeries1.Source := UserDefinedChartSource1;
         Chart1BSplineSeries1.Source := UserDefinedChartSource1;
         Chart1CubicSplineSeries1.Source := UserDefinedChartSource1;
         Chart1FitSeries1.Source := UserDefinedChartSource1;
       end;
  end;

  SetXErrorBarParams;
  SetYErrorBarParams;
end;

procedure TMainForm.RbXErrConstChange(Sender: TObject);
begin
  EdXErrConst.Show;
  EdXErrPercent.Hide;
  SetXErrorBarParams;
end;

procedure TMainForm.RbYErrConstChange(Sender: TObject);
begin
  EdYErrConst.Show;
  EdYErrPercent.Hide;
  SetYErrorBarParams;
end;

procedure TMainForm.RbXErrPercentChange(Sender: TObject);
begin
  EdXErrConst.Hide;
  EdXErrPercent.Show;
  SetXErrorBarParams;
end;

procedure TMainForm.RbYErrPercentChange(Sender: TObject);
begin
  EdYErrConst.Hide;
  EdYErrPercent.Show;
  SetYErrorBarParams;
end;

procedure TMainForm.RbXErrChartSourceChange(Sender: TObject);
begin
  EdXErrConst.Hide;
  EdXErrPercent.Hide;
  SetXErrorBarParams;
end;

procedure TMainForm.RbYErrChartSourceChange(Sender: TObject);
begin
  EdYErrConst.Hide;
  EdYErrPercent.Hide;
  SetYErrorBarParams;
end;

procedure TMainForm.SetXErrorBarParams;
begin
  if RbXErrConst.Checked then
    with RandomChartSource1 do begin
      XErrorBarData.Kind := ebkConst;
      XErrorBarData.ValuePlus := EdXErrConst.Value;
      if CbXErrDifferent.Checked then
        XErrorBarData.ValueMinus := 0.5 * XErrorBarData.ValuePlus
      else
        XErrorBarData.ValueMinus := -1;
      XCount := 1;
    end
  else if RbXErrPercent.Checked then
    with RandomChartSource1 do begin
      XErrorBarData.Kind := ebkPercent;
      XErrorBarData.ValuePlus := EdXErrPercent.Value;
      if CbXErrDifferent.Checked then
        XErrorBarData.ValueMinus := 0.5 * XErrorBarData.ValuePlus
      else
        XErrorBarData.ValueMinus := -1;
      XCount := 1;
    end
  else if RbXErrChartSource.Checked then
    with RandomChartSource1 do begin
      XErrorBarData.Kind := ebkChartSource;
      XErrorBarData.ValuePlus := 20;   // 20% of X extent
      XErrorBarData.IndexPlus := 1;
      if CbXErrDifferent.Checked then begin
        XErrorBarData.IndexMinus := 2;
        XErrorBarData.ValueMinus := XErrorBarData.ValuePlus;
        XCount := 3;
      end else begin
        XErrorBarData.IndexMinus := -1;
        XErrorBarData.ValueMinus := -1;
        XCount := 2;
      end;
    end
  else
    exit;

  ListChartSource1.XErrorBarData.Assign(RandomChartSource1.XErrorBarData);
  ListChartSource1.XCount := RandomChartSource1.XCount;
  PopulateListSource;

  UserDefinedChartSource1.XErrorBarData.Assign(RandomChartSource1.XErrorBarData);
  UserDefinedChartSource1.XCount := RandomChartSource1.XCount;
  UserDefinedChartSource1.Reset;
end;

procedure TMainForm.SetYErrorBarParams;
begin
  if RbYErrConst.Checked then
    with RandomChartSource1 do begin
      YErrorBarData.Kind := ebkConst;
      YErrorBarData.ValuePlus := EdYErrConst.Value;
      if CbYErrDifferent.Checked then
        YErrorBarData.ValueMinus := 0.5 * YErrorBarData.ValuePlus
      else
        YErrorBarData.ValueMinus := -1;
      YCount := 1;
    end
  else if RbYErrPercent.Checked then
    with RandomChartSource1 do begin
      YErrorBarData.Kind := ebkPercent;
      YErrorBarData.ValuePlus := EdYErrPercent.Value;
      if CbYErrDifferent.Checked then
        YErrorBarData.ValueMinus := 0.5 * YErrorBarData.ValuePlus
      else
        YErrorBarData.ValueMinus := -1;
      YCount := 1;
    end
  else if RbYErrChartSource.Checked then
    with RandomChartSource1 do begin
      YErrorBarData.Kind := ebkChartSource;
      YErrorBarData.ValuePlus := 20;   // 20% of X extent
      YErrorBarData.IndexPlus := 1;
      if CbYErrDifferent.Checked then begin
        YErrorBarData.IndexMinus := 2;
        YErrorBarData.ValueMinus := YErrorBarData.ValuePlus;
        YCount := 3;
      end else begin
        YErrorBarData.IndexMinus := -1;
        YErrorBarData.ValueMinus := -1;
        YCount := 2;
      end
    end
  else
    exit;

  ListChartSource1.YErrorBarData.Assign(RandomChartSource1.YErrorBarData);
  ListChartSource1.YCount := RandomChartSource1.YCount;
  PopulateListSource;

  UserDefinedChartSource1.YErrorBarData.Assign(RandomChartSource1.YErrorBarData);
  UserDefinedChartSource1.YCount := RandomChartSource1.YCount;
  UserDefinedChartSource1.Reset;
end;

end.

