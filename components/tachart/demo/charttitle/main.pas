unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, TAGraph, TATextElements, Types, TAChartUtils, TASeries;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Chart1: TChart;
    cbFullWidth: TCheckBox;
    cbShowAxisMarksAndTitle: TCheckBox;
    cbShape: TComboBox;
    cbFrame: TCheckBox;
    clbFrameColor: TColorButton;
    gbTitleMargins: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblTitleText: TLabel;
    lblShape: TLabel;
    mmoTitleText: TMemo;
    rgAlignment: TRadioGroup;
    seLeftMargin: TSpinEdit;
    seRightMargin: TSpinEdit;
    procedure cbFrameChange(Sender: TObject);
    procedure Chart1ChartTitleGetShape(ASender: TChartTextElement;
      const ABoundingBox: TRect; var APolygon: TPointArray);
    procedure cbFullWidthChange(Sender: TObject);
    procedure cbShowAxisMarksAndTitleChange(Sender: TObject);
    procedure cbShapeChange(Sender: TObject);
    procedure clbFrameColorColorChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mmoTitleTextChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seLeftMarginChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.mmoTitleTextChange(Sender: TObject);
begin
  Chart1.Title.Text.Text := mmoTitleText.Lines.Text;
end;

procedure TMainForm.rgAlignmentClick(Sender: TObject);
begin
  case rgAlignment.ItemIndex of
    0: Chart1.Title.Alignment := taLeftJustify;
    1: Chart1.Title.Alignment := taCenter;
    2: Chart1.Title.Alignment := taRightJustify;
  end;
end;

procedure TMainForm.seLeftMarginChange(Sender: TObject);
begin
  Chart1.Title.Margins.Left := seLeftMargin.Value;
end;

procedure TMainForm.seRightMarginChange(Sender: TObject);
begin
  Chart1.Title.Margins.Right := seRightMargin.Value;
end;

procedure TMainForm.cbFullWidthChange(Sender: TObject);
begin
  Chart1.Title.FullWidth := cbFullWidth.Checked;
end;

procedure TMainForm.Chart1ChartTitleGetShape(ASender: TChartTextElement;
  const ABoundingBox: TRect; var APolygon: TPointArray);
var
  delta: Integer;
begin
  delta := ABoundingBox.Height div 2;
  SetLength(APolygon, 4);
  APolygon[0] := Point(ABoundingBox.Left + delta, ABoundingBox.Top);
  APolygon[1] := Point(ABoundingBox.Right, ABoundingBox.Top);
  APolygon[2] := Point(ABoundingBox.Right - delta, ABoundingBox.Bottom);
  APolygon[3] := Point(ABoundingBox.Left, ABoundingBox.Bottom);
end;

procedure TMainForm.cbFrameChange(Sender: TObject);
begin
  Chart1.Title.Frame.Visible := cbFrame.Checked;
end;

procedure TMainForm.cbShowAxisMarksAndTitleChange(Sender: TObject);
begin
  Chart1.BottomAxis.Marks.Visible := cbShowAxisMarksAndTitle.Checked;
  Chart1.BottomAxis.Title.Visible := cbShowAxisMarksAndTitle.Checked;
  Chart1.LeftAxis.Marks.Visible := cbShowAxisMarksAndTitle.Checked;
  Chart1.LeftAxis.Title.Visible := cbShowAxisMarksAndTitle.Checked;
end;

procedure TMainForm.cbShapeChange(Sender: TObject);
begin
  Chart1.Title.Shape := TChartLabelShape(cbShape.ItemIndex);
  if Chart1.Title.Shape = clsUserDefined then
    Chart1.Title.OnGetShape := @Chart1ChartTitleGetShape
  else
    Chart1.Title.OnGetShape := nil;
end;

procedure TMainForm.clbFrameColorColorChanged(Sender: TObject);
begin
  Chart1.Title.Frame.Color := clbFrameColor.ButtonColor;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

end.

