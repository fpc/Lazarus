unit frmNormDistr;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, StrUtils, Math,
  Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TAGraph, TASources, TASeries, TATransformations;

type
  TNormDistrFrame = class(TFrame)
    catCumulNormDistr: TChartAxisTransformations;
    catCumulNormDistrCumulNormDistrAxisTransform1: TCumulNormDistrAxisTransform;
    catCumulNormDistrLinearAxisTransform1: TLinearAxisTransform;
    cbPercent: TCheckBox;
    cbUseAxisTransform: TCheckBox;
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    edDataCount: TSpinEdit;
    lblDataCount: TLabel;
    pnCumulNormDistr: TPanel;
    rgRandDistr: TRadioGroup;
    procedure cbPercentChange(Sender: TObject);
    procedure cbUseAxisTransformChange(Sender: TObject);
    procedure edDataCountChange(Sender: TObject);
    procedure rgRandDistrClick(Sender: TObject);
  private
    procedure FillCumulNormDistrSource;

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TNormDistrFrame.Create(AOwner: TComponent);
begin
  inherited;
  FillCumulNormDistrSource;
end;

procedure TNormDistrFrame.cbPercentChange(Sender: TObject);
begin
  catCumulNormDistrLinearAxisTransform1.Enabled := cbPercent.Checked;
  FillCumulNormDistrSource;
  Chart.LeftAxis.Title.Caption :=
    'Cumulative probability' + IfThen(cbPercent.Checked, ' (%)', '');
end;

procedure TNormDistrFrame.cbUseAxisTransformChange(Sender: TObject);
begin
  catCumulNormDistrCumulNormDistrAxisTransform1.Enabled := cbUseAxisTransform.Checked;
end;

procedure TNormDistrFrame.edDataCountChange(Sender: TObject);
begin
  FillCumulNormDistrSource;
end;

procedure TNormDistrFrame.FillCumulNormDistrSource;
var
  i: Integer;
  y: Double;
  s: TListChartSource;
begin
  RandSeed := 976896;
  s := ChartLineSeries1.ListSource;
  s.BeginUpdate;
  try
    s.Clear;
    // Add random test data as x values --> random values will
    // get sorted in ascending direction automatically.
    s.Sorted := false;
    for i := 1 to edDataCount.Value do
      case rgRandDistr.ItemIndex of
        0: s.Add(Random, 0);
        1: s.Add(RandG(0.0, 1.0), 0);
      end;
    s.Sorted := true;
    // Calculate cumulative probability from index in sorted list.
    for i := 0 to s.Count - 1 do begin
      y := (i + 1) / (s.Count + 1); // Add 1 since y=0 and y=1 are not valid.
      s.Item[i]^.Y := IfThen(CbPercent.Checked, y * 100, y);
    end;
  finally
    s.EndUpdate;
  end;
end;

procedure TNormDistrFrame.rgRandDistrClick(Sender: TObject);
begin
  FillCumulNormDistrSource;
end;

end.

