unit ceAxisFrame;

{$MODE ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms,
  Controls, ExtCtrls, ComCtrls, StdCtrls, Dialogs, Spin,
  TATextElements, TAChartAxis, TAGraph,
  ceFontFrame, cePenFrame, ceShapeBrushPenMarginsFrame, ceArrowFrame;

type
  TChartAxisEditorPage = (aepTitle, aepLabels, aepGrid, aepLine);

  { TChartAxisFrame }
  TChartAxisFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    cbAutoMax: TCheckBox;
    cbAutoMin: TCheckBox;
    cbAxisLineVisible: TCheckBox;
    cbFrameVisible: TCheckBox;
    cbGridVisible: TCheckBox;
    cbInverted: TCheckBox;
    cbLabelsVisible: TCheckBox;
    cbShow: TCheckBox;
    cbTickColor: TColorButton;
    cbTitleVisible: TCheckBox;
    cbTitleHTML: TCheckBox;
    cbTitleWordwrap: TCheckBox;
    edLabelFormat: TEdit;
    gbArrow: TGroupBox;
    gbAxisLine: TGroupBox;
    gbAxisRange: TGroupBox;
    gbFrame: TGroupBox;
    gbGrid: TGroupBox;
    gbLabelFont: TGroupBox;
    gbLabels: TGroupBox;
    gbShapeFillBorder: TGroupBox;
    gbTicks: TGroupBox;
    gbTitleFont: TGroupBox;
    gbTitleShapeBrushPenMargins: TGroupBox;
    lblAutomatic: TLabel;
    lblLabelDistance: TLabel;
    lblLabelFormat: TLabel;
    lblTickInnerLength: TLabel;
    lblTickLength: TLabel;
    lblTitle: TLabel;
    lblTitleDistance: TLabel;
    mmoTitle: TMemo;
    PageControl: TPageControl;
    PanelTop: TPanel;
    pgGrid: TTabSheet;
    pgLabels: TTabSheet;
    pgLine: TTabSheet;
    pgTitle: TTabSheet;
    rgTitleAlignment: TRadioGroup;
    seLabelDistance: TSpinEdit;
    seMaximum: TFloatSpinEdit;
    seMinimum: TFloatSpinEdit;
    seTickInnerLength: TSpinEdit;
    seTickLength: TSpinEdit;
    seTitleDistance: TSpinEdit;
    Spacer: TBevel;
    TitleMemoPanel: TPanel;
    TitleParamsPanel: TPanel;
    procedure cbAutoMaxChange(Sender: TObject);
    procedure cbAutoMinChange(Sender: TObject);
    procedure cbAxisLineVisibleChange(Sender: TObject);
    procedure cbFrameVisibleChange(Sender: TObject);
    procedure cbGridVisibleChange(Sender: TObject);
    procedure cbInvertedChange(Sender: TObject);
    procedure cbLabelsVisibleChange(Sender: TObject);
    procedure cbShowChange(Sender: TObject);
    procedure cbTickColorColorChanged(Sender: TObject);
    procedure cbTitleHTMLChange(Sender: TObject);
    procedure cbTitleVisibleChange(Sender: TObject);
    procedure cbTitleWordwrapChange(Sender: TObject);
    procedure edLabelFormatEditingDone(Sender: TObject);
    procedure mmoTitleChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure rgTitleAlignmentClick(Sender: TObject);
    procedure seLabelDistanceChange(Sender: TObject);
    procedure seMaximumChange(Sender: TObject);
    procedure seMinimumChange(Sender: TObject);
    procedure seTickInnerLengthChange(Sender: TObject);
    procedure seTickLengthChange(Sender: TObject);
    procedure seTitleDistanceChange(Sender: TObject);
  private
    FAxis: TChartAxis;
    FAxisMin, FAxisMax: Double;
    FTitleFontFrame: TChartFontFrame;
    FTitleShapeBrushPenMarginsFrame: TChartShapeBrushPenMarginsFrame;
    FLabelFontFrame: TChartFontFrame;
    FGridPenFrame: TChartPenFrame;
    FFramePenFrame: TChartPenFrame;
    FAxisLinePenFrame: TChartPenFrame;
    FLabelShapeBrushPenMarginsFrame: TChartShapeBrushPenMarginsFrame;
    FArrowFrame: TChartArrowFrame;

    function GetAlignment(AItemIndex: Integer): TAlignment;
    function GetAlignmentIndex(AValue: TAlignment): Integer;
    function GetPage: TChartAxisEditorPage;
    procedure SetPage(AValue: TChartAxisEditorPage);

    procedure ChangedHandler(Sender: TObject);
    procedure LabelChangedHandler(Sender: TObject);
    procedure LabelFontChangedHandler(Sender: TObject);
    procedure LabelShapeChangedHandler(AShape: TChartLabelShape);
    procedure TitleShapeChangedHandler(AShape: TChartLabelShape);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    function GetChart: TChart;
    function GetRealAxisMax: Double;
    function GetRealAxisMin: Double;
    procedure UpdateControlState;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(Axis: TChartAxis);
    function Validate(out AMsg: String; out AControl: TWinControl): Boolean;
    property Page: TChartAxisEditorPage read GetPage write SetPage;
  end;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils,
  ceUtils;

constructor TChartAxisFrame.Create(AOwner: TComponent);
begin
  inherited;

  FTitleFontFrame := TChartFontFrame.Create(self);
  FTitleFontFrame.Name := '';
  FTitleFontFrame.Align := alClient;
  FTitleFontFrame.BorderSpacing.Around := 8;
  FTitleFontFrame.AutoSize := true;
  FTitleFontFrame.Parent := gbTitleFont;
  gbTitleFont.AutoSize := true;
  gbTitleFont.Caption := 'Font';

  FTitleShapeBrushPenMarginsFrame := TChartShapeBrushPenMarginsFrame.Create(self);
  FTitleShapeBrushPenMarginsFrame.Name := '';
  FTitleShapeBrushPenMarginsFrame.Align := alClient;
  FTitleShapeBrushPenMarginsFrame.BorderSpacing.Around := 8;
  FTitleShapeBrushPenMarginsFrame.OnShapeChange := @TitleShapeChangedHandler;
  FTitleShapeBrushPenMarginsFrame.AutoSize := true;
  FTitleShapeBrushPenMarginsFrame.Parent := gbTitleShapeBrushPenMargins;
  gbTitleShapeBrushPenMargins.AutoSize := true;
  gbTitleShapeBrushPenMargins.Caption := 'Title background';

  FLabelFontFrame := TChartFontFrame.Create(self);
  FLabelFontFrame.Name := '';
  FLabelFontFrame.Align := alClient;
  FLabelFontFrame.BorderSpacing.Around := 8;
  FLabelFontFrame.Parent := gbLabelFont;
  FLabelFontFrame.OnChange := @LabelFontChangedHandler;
  gbLabelFont.AutoSize := true;
  gbLabelFont.Caption := 'Label font';

  FLabelShapeBrushPenMarginsFrame := TChartShapeBrushPenMarginsFrame.Create(self);
  FLabelShapeBrushPenMarginsFrame.Name := '';
  FLabelShapeBrushPenMarginsFrame.Align := alClient;
  FLabelShapeBrushPenMarginsFrame.BorderSpacing.Around := 8;
  FLabelShapeBrushPenMarginsFrame.OnShapeChange := @LabelShapeChangedHandler;
  FLabelShapeBrushPenMarginsFrame.AutoSize := true;
  FLabelShapeBrushPenMarginsFrame.Parent := gbShapeFillBorder;
  gbShapeFillBorder.AutoSize := true;
  gbShapeFillBorder.Caption := 'Label background';

  FGridPenFrame := TChartPenFrame.Create(Self);
  FGridPenFrame.Name := '';
  FGridPenFrame.Align := alTop;
  FGridPenFrame.Top := 1000;
  FGridPenFrame.BorderSpacing.Around := 8;
  FGridPenFrame.Parent := gbGrid;
  FGridPenFrame.OnChange := @ChangedHandler;
  gbGrid.AutoSize := true;
  gbGrid.Caption := 'Grid lines';

  FFramePenFrame := TChartPenFrame.Create(Self);
  FFramePenFrame.Name := '';
  FFramePenFrame.Align := alTop;
  FFramePenFrame.Top := 1000;
  FFramePenFrame.BorderSpacing.Around := 8;
  FFramePenFrame.Parent := gbFrame;
//  FFramePenFrame.OnChange := @ChangedHandler;
  gbFrame.AutoSize := true;
  gbFrame.Caption := 'Frame';

  FAxisLinePenFrame := TChartPenFrame.Create(Self);
  FAxisLinePenFrame.Name := '';
  FAxisLinePenFrame.Align := alTop;
  FAxisLinePenFrame.Top := 1000;
  FAxisLinePenFrame.BorderSpacing.Around := 8;
  FAxisLinePenFrame.Parent := gbAxisLine;
//  FAxisLinePenFrame.OnChange := @ChangedHandler;
  gbAxisLine.AutoSize := true;
  gbAxisLine.Caption := 'Axis line';

  FArrowFrame := TChartArrowFrame.Create(self);
  FArrowFrame.Name := '';
  FArrowFrame.Align := alClient;
  FArrowFrame.BorderSpacing.Around := 8;
  FArrowFrame.AutoSize := true;
  FArrowFrame.Parent := gbArrow;
  gbArrow.AutoSize := true;
  gbArrow.Caption := 'Arrow';

  BoldHeaders(self);

  TitleParamsPanel.AutoSize := true;
end;

procedure TChartAxisFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  w: Integer = 0;
  h: Integer = 0;
begin
  gbTitleShapeBrushPenMargins.GetPreferredsize(w, h);
//  inc(w, FTitleShapeBrushPenMarginsFrame.BorderSpacing.Around);
  PreferredWidth :=       w*2 +
//    Max(w, gbTitleFont.Width) * 2 +
    Bevel1.Width +
    TitleMemoPanel.BorderSpacing.Around * 2;

  PreferredHeight :=
    Max(
      gbTicks.Top + gbTicks.Height + gbTicks.BorderSpacing.Bottom,
      gbShapeFillBorder.Top + gbShapeFillBorder.Height + gbShapeFillBorder.BorderSpacing.Bottom
    ) +
    PageControl.Height - PageControl.ClientHeight +
    PanelTop.Height;
end;

procedure TChartAxisFrame.cbAutoMaxChange(Sender: TObject);
begin
  FAxis.Range.UseMax := not cbAutoMax.Checked;
  seMaximum.Visible := FAxis.Range.UseMax;
end;

procedure TChartAxisFrame.cbAutoMinChange(Sender: TObject);
begin
  FAxis.Range.UseMin := not cbAutoMin.Checked;
  seMinimum.Visible := FAxis.Range.UseMin;
end;

procedure TChartAxisFrame.cbAxisLineVisibleChange(Sender: TObject);
begin
  FAxis.AxisPen.Visible := cbAxisLineVisible.Checked;
  UpdateControlState;
end;

procedure TChartAxisFrame.cbFrameVisibleChange(Sender: TObject);
begin
  GetChart.Frame.Visible := cbFrameVisible.Checked;
  UpdateControlState;
end;

procedure TChartAxisFrame.cbGridVisibleChange(Sender: TObject);
begin
  FAxis.Grid.Visible := cbGridVisible.Checked;
  UpdateControlState;
end;

procedure TChartAxisFrame.cbInvertedChange(Sender: TObject);
begin
  FAxis.Inverted := cbInverted.Checked;
end;

procedure TChartAxisFrame.cbLabelsVisibleChange(Sender: TObject);
begin
  FAxis.Marks{%H-}.Visible := cbLabelsVisible.Checked;
  UpdateControlState;
end;

procedure TChartAxisFrame.cbShowChange(Sender: TObject);
begin
  FAxis.Visible := cbShow.Checked;
  PageControl.Visible := cbShow.checked;
end;

procedure TChartAxisFrame.cbTickColorColorChanged(Sender: TObject);
begin
  FAxis.TickColor := cbTickColor.ButtonColor;
end;

procedure TChartAxisFrame.cbTitleHTMLChange(Sender: TObject);
begin
  FAxis.Title.TextFormat := TEXT_FORMAT[cbTitleHTML.Checked];
end;

procedure TChartAxisFrame.cbTitleVisibleChange(Sender: TObject);
begin
  FAxis.Title.Visible := cbTitleVisible.Checked;
  UpdateControlState;
end;

procedure TChartAxisFrame.cbTitleWordwrapChange(Sender: TObject);
begin
  FAxis.Title.Wordwrap := cbTitleWordwrap.Checked;
end;

procedure TChartAxisFrame.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TChartAxisFrame.edLabelFormatEditingDone(Sender: TObject);
begin
  try
    FAxis.Marks{%H-}.Format := edLabelFormat.Text;
  except
  end;
end;

function TChartAxisFrame.GetAlignment(AItemIndex: Integer): TAlignment;
const
  ALIGNMENTS: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
begin
  Result := ALIGNMENTS[AItemIndex];
end;

function TChartAxisFrame.GetAlignmentIndex(AValue: TAlignment): Integer;
const
  ALIGNMENTS: array[TAlignment] of Integer = (0, 2, 1);
begin
  Result := ALIGNMENTS[AValue];
end;

function TChartAxisFrame.GetChart: TChart;
begin
  Result := FAxis.Collection.Owner as TChart;
end;

function TChartAxisFrame.GetPage: TChartAxisEditorPage;
begin
  Result := TChartAxisEditorPage(PageControl.ActivePageIndex);
end;

function TChartAxisFrame.GetRealAxisMax: Double;
begin
  if cbAutoMax.Checked then
    Result := FAxisMax
  else
    Result := seMaximum.Value;
end;

function TChartAxisFrame.GetRealAxisMin: Double;
begin
  if cbAutoMin.Checked then
    Result := FAxisMin
  else
    Result := seMinimum.Value;
end;

procedure TChartAxisFrame.LabelChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TChartAxisFrame.LabelFontChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TChartAxisFrame.LabelShapeChangedHandler(AShape: TChartLabelShape);
begin
  FAxis.Marks{%H-}.Shape := AShape;
end;

procedure TChartAxisFrame.mmoTitleChange(Sender: TObject);
begin
  FAxis.Title.Caption := mmoTitle.Lines.Text;
end;

procedure TChartAxisFrame.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
var
  msg: String;
  C: TWinControl;
begin
  if not Validate(msg, C) then
  begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    AllowChange := false;
  end;
end;

procedure TChartAxisFrame.Prepare(Axis: TChartAxis);
begin
  FAxis := Axis;

  // Page "Title"
  cbTitleVisible.Checked := Axis.Title.Visible;
  cbTitleWordwrap.Checked := Axis.Title.Wordwrap;
  cbTitleHTML.Checked := (Axis.Title.TextFormat = tfHTML);
  mmoTitle.Lines.Text := Axis.Title.Caption;
  with Axis.Title do begin
    rgTitleAlignment.ItemIndex := GetAlignmentIndex(Alignment);
    seTitleDistance.Value := Distance;
    FTitleFontFrame.Prepare(LabelFont, true);
    FTitleShapeBrushPenMarginsFrame.Prepare(Shape, LabelBrush, Frame, Margins);
  end;

  // Page "Labels"
  GetChart.GetAllSeriesAxisLimits(Axis, FAxisMin, FAxisMax);
  seMaximum.Value := IfThen(Axis.Range.UseMax, Axis.Range.Max, FAxisMax);
  seMinimum.Value := IfThen(Axis.Range.UseMin, Axis.Range.Min, FAxisMin);
  cbAutoMax.Checked := not Axis.Range.UseMax;
  cbAutoMin.Checked := not Axis.Range.UseMin;
  cbInverted.Checked := Axis.Inverted;
  seTickLength.Value := Axis.TickLength;
  seTickInnerLength.Value := Axis.TickInnerLength;
  cbTickColor.ButtonColor := Axis.TickColor;
  with Axis{%H-}.Marks do begin
    seLabelDistance.Value := Distance;
    cbLabelsVisible.Checked := Visible;
    edLabelFormat.Text := Format;
    FLabelFontFrame.Prepare(LabelFont, true);
    FLabelShapeBrushPenMarginsFrame.Prepare(Shape, LabelBrush, Frame, Margins);
  end;

  // Page "Grid"
  cbGridVisible.Checked := Axis.Grid.EffVisible;
  FGridPenFrame.Prepare(Axis.Grid);

  // Page "Line"
  cbFrameVisible.Checked := GetChart.Frame.EffVisible;
  FFramePenFrame.Prepare(GetChart.Frame);
  cbAxisLineVisible.Checked := Axis.AxisPen.EffVisible;
  FAxisLinePenFrame.Prepare(Axis.AxisPen);
  FArrowFrame.Prepare(Axis.Arrow);

  UpdateControlState;
end;

procedure TChartAxisFrame.rgTitleAlignmentClick(Sender: TObject);
begin
  if Assigned(FAxis) then
    FAxis.Title.Alignment := GetAlignment(rgTitleAlignment.ItemIndex);
end;

procedure TChartAxisFrame.seLabelDistanceChange(Sender: TObject);
begin
  if Assigned(FAxis) then
    FAxis.Marks{%H-}.Distance := seLabelDistance.Value;
end;

procedure TChartAxisFrame.seMaximumChange(Sender: TObject);
begin
  if Assigned(FAxis) then
  begin
    FAxis.Range.Max := seMaximum.Value;
    cbAutoMax.Checked := false;
  end;
end;

procedure TChartAxisFrame.seMinimumChange(Sender: TObject);
begin
  FAxis.Range.Min := seMinimum.Value;
  cbAutoMin.Checked := false;
end;

procedure TChartAxisFrame.SetPage(AValue: TChartAxisEditorPage);
begin
  PageControl.ActivePageIndex := ord(AValue);
end;

procedure TChartAxisFrame.seTickLengthChange(Sender: TObject);
begin
  if Assigned(FAxis) then
    FAxis.TickLength := seTickLength.Value;
end;

procedure TChartAxisFrame.seTickInnerLengthChange(Sender: TObject);
begin
  if Assigned(FAxis) then
    FAxis.TickInnerLength := seTickInnerLength.Value;
end;

procedure TChartAxisFrame.seTitleDistanceChange(Sender: TObject);
begin
  if Assigned(FAxis) then
    FAxis.Title.Distance := seTitleDistance.Value;
end;

procedure TChartAxisFrame.TitleShapeChangedHandler(AShape: TChartLabelShape);
begin
  FAxis.Title.Shape := AShape;
end;

procedure TChartAxisFrame.UpdateControlstate;
begin
  // title
  cbTitleWordwrap.Enabled := cbTitleVisible.Checked;
  cbTitleHTML.Enabled := cbTitleVisible.Checked;
  lblTitle.Enabled := cbTitleVisible.Checked;
  mmoTitle.Enabled := cbTitleVisible.Checked;
  TitleParamsPanel.Enabled := cbTitleVisible.Checked;

  // labels
  lblLabelFormat.Enabled := cbLabelsVisible.Checked;
  edlabelFormat.Enabled := cbLabelsVisible.Checked;
  lblLabelDistance.Enabled := cbLabelsVisible.Checked;
  seLabelDistance.Enabled := cbLabelsVisible.Checked;

  // grid
  FGridPenFrame.Enabled := cbGridVisible.Checked;

  // Line
  FAxisLinePenFrame.Enabled := cbAxisLineVisible.Checked;
  FFramePenFrame.Enabled := cbFrameVisible.Checked;
end;

function TChartAxisFrame.Validate(out AMsg: String; out AControl: TWinControl): Boolean;
begin
  Result := false;
  if GetRealAxisMin >= GetRealAxisMax then
  begin
    AMsg := 'The axis minimum must be smaller than the axis maximum.';
    if seMaximum.Visible then
      AControl := seMaximum
    else if seMinimum.Visible then
      AControl := seMinimum
    else
      AControl := cbAutoMax;
    exit;
  end;
  Result := true;
end;


end.

