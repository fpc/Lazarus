unit ceLegendFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  Forms, Controls, ExtCtrls, StdCtrls, Spin,
  TALegend, TAGraph,
  ceFontFrame, ceSimpleBrushFrame, ceSimplePenFrame;

type

  { TChartLegendFrame }

  TChartLegendFrame = class(TFrame)
    Bevel1: TBevel;
    Background_Border_Spacer: TBevel;
    cbInverted: TCheckBox;
    cbItemFillOrder: TComboBox;
    cbShow: TCheckBox;
    cbUseSideBar: TCheckBox;
    cbHTML: TCheckBox;
    gbAlignment: TGroupBox;
    gbBackground: TGroupBox;
    gbBorder: TGroupBox;
    gbFont: TGroupBox;
    gbItems: TGroupBox;
    gbMargins: TGroupBox;
    lblColumns: TLabel;
    lblItemFillOrder: TLabel;
    lblMarginX: TLabel;
    lblMarginY: TLabel;
    lblSpacing: TLabel;
    lblSymbolWidth: TLabel;
    PanelTop: TPanel;
    ParamsPanel: TPanel;
    rbBottomCenter: TRadioButton;
    rbBottomLeft: TRadioButton;
    rbBottomRight: TRadioButton;
    rbCenterLeft: TRadioButton;
    rbCenterRight: TRadioButton;
    rbTopCenter: TRadioButton;
    rbTopLeft: TRadioButton;
    rbTopRight: TRadioButton;
    seColumns: TSpinEdit;
    seMarginX: TSpinEdit;
    seMarginY: TSpinEdit;
    seSpacing: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    procedure cbHTMLChange(Sender: TObject);
    procedure cbInvertedChange(Sender: TObject);
    procedure cbItemFillOrderChange(Sender: TObject);
    procedure cbShowChange(Sender: TObject);
    procedure cbUseSideBarChange(Sender: TObject);
    procedure gbAlignmentClick(Sender: TObject);
    procedure seColumnsChange(Sender: TObject);
    procedure seMarginXChange(Sender: TObject);
    procedure seMarginYChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
  private
    FLegend: TChartLegend;
    FFontFrame: TChartFontFrame;
    FBackgroundFrame: TSimpleChartBrushFrame;
    FBorderFrame: TSimpleChartPenFrame;
    function GetAlignment: TLegendAlignment;
    procedure SetAlignment(AValue: TLegendAlignment);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    function GetChart: TChart;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(ALegend: TChartLegend);
  end;


implementation

{$R *.lfm}

uses
  Math,
  TATypes, TAChartUtils,
  ceUtils;

constructor TChartLegendFrame.Create(AOwner: TComponent);
begin
  inherited;

  FFontFrame := TChartFontFrame.Create(self);
  FFontFrame.Parent := gbFont;
  FFontFrame.Name := '';
  FFontFrame.Align := alClient;
  FFontFrame.BorderSpacing.Left := 8;
  FFontFrame.BorderSpacing.Right := 8;
  FFontFrame.AutoSize := true;
  gbFont.AutoSize := true;
  gbFont.Caption := 'Font';

  FBackgroundFrame := TSimpleChartBrushFrame.Create(self);
  FBackgroundFrame.Name := '';
  FBackgroundFrame.Align := alClient;
  FBackgroundFrame.BorderSpacing.Left := 8;
  FBackgroundFrame.BorderSpacing.Right := 8;
  FBackgroundFrame.BorderSpacing.Bottom := 8;
  FBackgroundFrame.Parent := gbBackground;
  FBackgroundFrame.AutoSize := true;
  gbBackground.AutoSize := true;
  gbBackground.Caption := 'Background';

  FBorderFrame := TSimpleChartPenFrame.Create(self);
  FBorderFrame.Name := '';
  FBorderFrame.Align := alClient;
  FBorderFrame.BorderSpacing.Left := 8;
  FBorderFrame.BorderSpacing.Right := 8;
  FBorderFrame.BorderSpacing.Bottom := 8;
  FBorderFrame.AutoSize := true;
  FBorderFrame.Parent := gbBorder;
  gbBorder.AutoSize := true;
  gbBorder.Caption := 'Border';

  BoldHeaders(Self);
end;

procedure TChartLegendFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredHeight := PanelTop.Height + PanelTop.BorderSpacing.Bottom +
    Max(
      gbAlignment.Height + gbBackground.Height + gbBackground.BorderSpacing.Top +
        gbMargins.Height + gbMargins.Borderspacing.Top,
      gbFont.Height + gbItems.Height + gbItems.BorderSpacing.Top
    );

  PreferredWidth := gbAlignment.Width + gbFont.Width + gbFont.BorderSpacing.Left;
end;

procedure TChartLegendFrame.cbHTMLChange(Sender: TObject);
begin
  FLegend.TextFormat := TEXT_FORMAT[cbHTML.Checked];
end;

procedure TChartLegendFrame.cbInvertedChange(Sender: TObject);
begin
  FLegend.Inverted := cbInverted.Checked;
end;

procedure TChartLegendFrame.cbItemFillOrderChange(Sender: TObject);
begin
  FLegend.ItemFillOrder := TLegendItemFillOrder(cbItemFillOrder.ItemIndex);
end;

procedure TChartLegendFrame.cbShowChange(Sender: TObject);
begin
  FLegend.Visible := cbShow.Checked;
  cbUseSideBar.Visible := cbShow.Checked;
  cbHTML.Visible := cbShow.Checked;
  gbAlignment.Visible := cbShow.Checked;
  gbFont.Visible := cbShow.Checked;
  gbBackground.Visible := cbShow.Checked;
  gbBorder.Visible := cbShow.Checked;
  gbItems.Visible := cbShow.Checked;
  gbMargins.Visible := cbShow.Checked;
end;

procedure TChartLegendFrame.cbUseSideBarChange(Sender: TObject);
begin
  FLegend.UseSideBar := cbUseSideBar.Checked;
end;

function TChartLegendFrame.GetChart: TChart;
begin
  Result := FLegend.GetOwner as TChart;
end;

function TChartLegendFrame.GetAlignment: TLegendAlignment;
var
  i: Integer;
  rb: TRadioButton;
begin
  for i := 0 to gbAlignment.ControlCount-1 do
    if (gbAlignment.Controls[i] is TRadioButton) then begin
      rb := TRadioButton(gbAlignment.Controls[i]);
      if rb.Checked then begin
        Result := TLegendAlignment(rb.Tag);
        exit;
      end;
    end;
  Result := laTopRight;
end;

procedure TChartLegendFrame.gbAlignmentClick(Sender: TObject);
begin
  FLegend.Alignment := GetAlignment;
end;

procedure TChartLegendFrame.Prepare(ALegend: TChartLegend);
begin
  FLegend := ALegend;

  cbShow.Checked := ALegend.Visible;
  cbHTML.Checked := (ALegend.TextFormat = tfHTML);
  SetAlignment(ALegend.Alignment);

  seMarginX.Value := ALegend.MarginX;
  seMarginY.Value := ALegend.MarginY;

  cbUseSideBar.Checked := ALegend.UseSidebar;
  cbInverted.Checked := ALegend.Inverted;
  seColumns.Value := ALegend.ColumnCount;
  seSymbolWidth.Value := ALegend.SymbolWidth;
  seSpacing.Value := ALegend.Spacing;
  cbItemFillOrder.ItemIndex := ord(ALegend.ItemFillOrder);

  FFontFrame.Prepare(ALegend.Font, false);
  FBackgroundFrame.Prepare(ALegend.BackgroundBrush);
  FBorderFrame.Prepare(ALegend.Frame);
end;

procedure TChartLegendFrame.seColumnsChange(Sender: TObject);
begin
  FLegend.ColumnCount := seColumns.Value;
end;

procedure TChartLegendFrame.seMarginXChange(Sender: TObject);
begin
  FLegend.MarginX := seMarginX.Value;
end;

procedure TChartLegendFrame.seMarginYChange(Sender: TObject);
begin
  FLegend.MarginY := seMarginY.Value;
end;

procedure TChartLegendFrame.seSpacingChange(Sender: TObject);
begin
  FLegend.Spacing := seSpacing.Value;
end;

procedure TChartLegendFrame.seSymbolWidthChange(Sender: TObject);
begin
  FLegend.SymbolWidth := seSymbolWidth.Value;
end;

procedure TChartLegendFrame.SetAlignment(AValue: TLegendAlignment);
var
  i: Integer;
  rb: TRadioButton;
begin
  for i:=0 to gbAlignment.ControlCount-1 do
    if (gbAlignment.Controls[i] is TRadioButton) then begin
      rb := TRadioButton(gbAlignment.Controls[i]);
      if rb.Tag = ord(AValue) then begin
        rb.Checked := true;
        exit;
      end;
    end;
end;

end.

