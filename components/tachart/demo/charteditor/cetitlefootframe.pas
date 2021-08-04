unit ceTitleFootFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin,
  TATextElements, TAGraph,
  ceFontFrame, ceShapeBrushPenMarginsFrame;

type

  { TChartTitleFootFrame }

  TChartTitleFootFrame = class(TFrame)
    Bevel1: TBevel;
    cbShow: TCheckBox;
    cbWordwrap: TCheckBox;
    cbHTML: TCheckBox;
    gbFont: TGroupBox;
    gbShapeBrushPenMargins: TGroupBox;
    gbMargin: TGroupBox;
    lblMargin: TLabel;
    lblText: TLabel;
    MemoPanel: TPanel;
    mmoText: TMemo;
    PanelTop: TPanel;
    ParamsPanel: TPanel;
    rgAlignment: TRadioGroup;
    seMargin: TSpinEdit;
    procedure cbHTMLChange(Sender: TObject);
    procedure cbShowChange(Sender: TObject);
    procedure cbWordwrapClick(Sender: TObject);
    procedure mmoTextChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
    procedure seMarginChange(Sender: TObject);
  private
    FTitle: TChartTitle;
    FFontFrame: TChartFontFrame;
    FShapeBrushPenMarginsFrame: TChartShapeBrushPenMarginsFrame;
    procedure ChangedHandler(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
    procedure ShapeChangedHandler(AShape: TChartLabelShape);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    function GetChart: TChart;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(ATitle: TChartTitle);
  end;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils,
  ceUtils;

constructor TChartTitleFootFrame.Create(AOwner: TComponent);
begin
  inherited;

  FFontFrame := TChartFontFrame.Create(self);
  FFontFrame.Name := '';
  FFontFrame.Align := alClient;
  FFontFrame.BorderSpacing.Around := 8;
  FFontFrame.OnChange := @ChangedHandler;
  FFontFrame.AutoSize := true;
  FFontFrame.Parent := gbFont;
  gbFont.AutoSize := true;
  gbFont.Caption := 'Font';

  FShapeBrushPenMarginsFrame := TChartShapeBrushPenMarginsFrame.Create(self);
  FShapeBrushPenMarginsFrame.Name := '';
  FShapeBrushPenMarginsFrame.Align := alClient;
  FShapeBrushPenMarginsFrame.BorderSpacing.Around := 8;
  FShapeBrushPenMarginsFrame.OnChange := @ChangedHandler;
  FShapeBrushPenMarginsFrame.OnShapeChange := @ShapeChangedHandler;
  FShapeBrushPenMarginsFrame.AutoSize := true;
  FShapeBrushPenMarginsFrame.Parent := gbShapeBrushPenMargins;
  gbShapeBrushPenMargins.AutoSize := true;
  // Caption of this groupbox depends on title/footer. Will be set by Prepare.

  BoldHeaders(Self);

  ParamsPanel.AutoSize := true;
end;

procedure TChartTitleFootFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  w: Integer = 0;
  h: Integer = 0;
begin
  PreferredHeight := PanelTop.Height +
    MemoPanel.Constraints.MinHeight +
    ParamsPanel.Height + ParamsPanel.BorderSpacing.Top + ParamsPanel.BorderSpacing.Bottom;

//  w := 100;
  gbShapeBrushPenMargins.GetPreferredSize(w, h);
  PreferredWidth :=
    Max(w, Max(gbFont.Width, rgAlignment.Width)) * 2 +
    Bevel1.Width;
end;

procedure TChartTitleFootFrame.cbShowChange(Sender: TObject);
begin
  FTitle.Visible := cbShow.Checked;
  lblText.Visible := cbShow.Checked;
  mmoText.Visible := cbShow.Checked;
  rgAlignment.Visible := cbShow.Checked;
  gbShapeBrushPenMargins.Visible := cbShow.Checked;
  gbFont.Visible := cbShow.Checked;
  cbWordwrap.Visible := cbShow.Checked;
  cbHTML.Visible := cbShow.Checked;
  gbMargin.Visible := cbShow.Checked;
end;

procedure TChartTitleFootFrame.cbHTMLChange(Sender: TObject);
begin
  FTitle.TextFormat := TEXT_FORMAT[cbHTML.Checked];
end;

procedure TChartTitleFootFrame.cbWordwrapClick(Sender: TObject);
begin
  FTitle.Wordwrap := cbWordwrap.Checked;
end;

procedure TChartTitleFootFrame.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

function TChartTitleFootFrame.GetAlignment: TAlignment;
const
  ALIGNMENTS: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
begin
  Result := ALIGNMENTS[rgAlignment.ItemIndex];
end;

function TChartTitleFootFrame.GetChart: TChart;
begin
  Result := FTitle.GetOwner as TChart;
end;

procedure TChartTitleFootFrame.mmoTextChange(Sender: TObject);
begin
  FTitle.Text.Assign(mmoText.Lines);
end;

procedure TChartTitleFootFrame.Prepare(ATitle: TChartTitle);
begin
  FTitle := ATitle;

  cbShow.Checked := ATitle.Visible;
  cbWordwrap.Checked := ATitle.Wordwrap;
  cbHTML.Checked := (ATitle.TextFormat = tfHTML);
  seMargin.Value := ATitle.Margin;
  mmoText.Lines.Assign(ATitle.Text);

  SetAlignment(ATitle.Alignment);

  FFontFrame.Prepare(ATitle.Font, false);
  FShapeBrushPenMarginsFrame.Prepare(ATitle.Shape, ATitle.Brush, ATitle.Frame, ATitle.Margins);
  if ATitle = GetChart.Title then
    gbShapeBrushPenMargins.Caption := 'Title background'
  else
    gbShapeBrushPenMargins.Caption := 'Footer background';
end;

procedure TChartTitleFootFrame.rgAlignmentClick(Sender: TObject);
begin
  FTitle.Alignment := GetAlignment;
end;

procedure TChartTitleFootFrame.seMarginChange(Sender: TObject);
begin
  FTitle.Margin := seMargin.Value;
end;

procedure TChartTitleFootFrame.SetAlignment(AValue: TAlignment);
const
  ALIGNMENTS: array[TAlignment] of Integer = (0, 2, 1);
begin
  rgAlignment.ItemIndex := ALIGNMENTS[AValue];
end;

procedure TChartTitleFootFrame.ShapeChangedHandler(AShape: TChartLabelShape);
begin
  FTitle.Shape := AShape;
end;

end.

