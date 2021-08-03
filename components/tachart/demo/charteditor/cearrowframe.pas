unit ceArrowFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin,
  TATypes;

type

  { TChartArrowFrame }

  TChartArrowFrame = class(TFrame)
    cbArrowVisible: TCheckBox;
    lblArrowBaseLength: TLabel;
    lblArrowLength: TLabel;
    lblArrowWidth: TLabel;
    seArrowBaseLength: TSpinEdit;
    seArrowLength: TSpinEdit;
    seArrowWidth: TSpinEdit;
    procedure cbArrowVisibleChange(Sender: TObject);
    procedure seArrowBaseLengthChange(Sender: TObject);
    procedure seArrowLengthChange(Sender: TObject);
    procedure seArrowWidthChange(Sender: TObject);
  private
    FArrow: TChartArrow;
    procedure DoChange;
  public
    procedure Prepare(Arrow: TChartArrow);
  end;

implementation

{$R *.lfm}

procedure TChartArrowFrame.cbArrowVisibleChange(Sender: TObject);
begin
  FArrow.Visible := cbArrowVisible.Checked;
  DoChange;
end;

procedure TChartArrowFrame.DoChange;
begin
  lblArrowBaseLength.Enabled := cbArrowVisible.Checked;
  seArrowBaseLength.Enabled := cbArrowVisible.Checked;
  lblArrowLength.Enabled := cbArrowVisible.Checked;
  seArrowLength.Enabled := cbArrowVisible.Checked;
  lblArrowWidth.Enabled := cbArrowVisible.Checked;
  seArrowWidth.Enabled := cbArrowVisible.Checked;
end;

procedure TChartArrowFrame.Prepare(Arrow: TChartArrow);
begin
  FArrow := Arrow;
  cbArrowVisible.Checked := Arrow.Visible;
  seArrowBaseLength.Value := Arrow.BaseLength;
  seArrowLength.Value := Arrow.Length;
  seArrowWidth.Value := Arrow.Width;
  DoChange;
end;

procedure TChartArrowFrame.seArrowBaseLengthChange(Sender: TObject);
begin
  FArrow.BaseLength := seArrowBaseLength.Value;
end;

procedure TChartArrowFrame.seArrowLengthChange(Sender: TObject);
begin
  FArrow.Length := seArrowLength.Value;
end;

procedure TChartArrowFrame.seArrowWidthChange(Sender: TObject);
begin
  FArrow.Width := seArrowWidth.Value;
end;


end.

