unit ceAxisDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, Buttons, ComCtrls, Spin,
  TAChartAxis, TAChartAxisUtils, TATextElements, TAChartCombos, TAGraph,
  ceShapeBrushPenMarginsFrame, ceFontFrame, cePenFrame;

type

  { TAxisEditor }

  TAxisEditorPage = (aepTitle, aepLabels, aepGrid, aepLine);

  TAxisEditor = class(TForm)
    GridPenFrame: TPenFrame;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    ButtonPanel: TButtonPanel;
    cbAxisLineVisible: TCheckBox;
    cbGridVisible: TCheckBox;
    cbPenColor: TColorButton;
    cbPenStyle: TChartComboBox;
    cbPenWidth: TChartComboBox;
    cbTickColor: TColorButton;
    cbShow: TCheckBox;
    cbAutoMin: TCheckBox;
    cbAutoMax: TCheckBox;
    cbTitleVisible: TCheckBox;
    cbInverted: TCheckBox;
    cbLabelsVisible: TCheckBox;
    cbFrameVisible: TCheckBox;
    cbArrowVisible: TCheckBox;
    edLabelFormat: TEdit;
    AxisLinePenFrame: TPenFrame;
    gbAxisLine: TGroupBox;
    gbGrid: TGroupBox;
    gbTitleFont: TGroupBox;
    gbLabelFont: TGroupBox;
    gbShapeFillBorder: TGroupBox;
    GroupBox1: TGroupBox;
    gbLabels: TGroupBox;
    gbTicks: TGroupBox;
    gbFrame: TGroupBox;
    gbArrow: TGroupBox;
    lblArrowWidth: TLabel;
    lblArrowLength: TLabel;
    lblArrowBaseLength: TLabel;
    lblLabelDistance: TLabel;
    lblPenStyle: TLabel;
    lblPenWidth: TLabel;
    lblTitleDistance: TLabel;
    lblTickLength: TLabel;
    lblTickInnerLength: TLabel;
    FramePenFrame: TPenFrame;
    seTickLength: TSpinEdit;
    seTickInnerLength: TSpinEdit;
    seTitleDistance: TSpinEdit;
    seLabelDistance: TSpinEdit;
    seArrowBaseLength: TSpinEdit;
    seArrowLength: TSpinEdit;
    seArrowWidth: TSpinEdit;
    pgGrid: TTabSheet;
    TitleFontFrame: TFontFrame;
    LabelFontFrame: TFontFrame;
    gbAxisRange: TGroupBox;
    lblLabelFormat: TLabel;
    PanelTop: TPanel;
    seMinimum: TFloatSpinEdit;
    seMaximum: TFloatSpinEdit;
    lblAutomatic: TLabel;
    lblTitle: TLabel;
    mmoTitle: TMemo;
    LabelShapeBrushPenMarginsFrame: TShapeBrushPenMarginsFrame;
    TitleShapeBrushPenMarginsFrame: TShapeBrushPenMarginsFrame;
    TitleMemoPanel: TPanel;
    PageControl: TPageControl;
    TitleParamsPanel: TPanel;
    rgTitleAlignment: TRadioGroup;
    pgLabels: TTabSheet;
    pgTitle: TTabSheet;
    pgLine: TTabSheet;
    procedure cbArrowVisibleChange(Sender: TObject);
    procedure cbAutoMaxChange(Sender: TObject);
    procedure cbAutoMinChange(Sender: TObject);
    procedure cbFrameVisibleChange(Sender: TObject);
    procedure cbGridVisibleChange(Sender: TObject);
    procedure cbInvertedChange(Sender: TObject);
    procedure cbLabelsVisibleChange(Sender: TObject);
    procedure cbAxisLineVisibleChange(Sender: TObject);
    procedure cbShowChange(Sender: TObject);
    procedure cbTickColorColorChanged(Sender: TObject);
    procedure cbTitleVisibleChange(Sender: TObject);
    procedure edLabelFormatEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure mmoTitleChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rgTitleAlignmentClick(Sender: TObject);
    procedure seArrowBaseLengthChange(Sender: TObject);
    procedure seArrowLengthChange(Sender: TObject);
    procedure seArrowWidthChange(Sender: TObject);
    procedure seLabelDistanceChange(Sender: TObject);
    procedure seTitleDistanceChange(Sender: TObject);
    procedure seMaximumChange(Sender: TObject);
    procedure seMinimumChange(Sender: TObject);
    procedure seTickLengthChange(Sender: TObject);
    procedure seTickInnerLengthChange(Sender: TObject);
  private
    FAxis: TChartAxis;
    FOKClicked: Boolean;
    FSavedAxis: TChartAxis;
    function GetAlignment(AItemIndex: Integer): TAlignment;
    function GetAlignmentIndex(AValue: TAlignment): Integer;
    function GetPage: TAxisEditorPage;
    procedure SetPage(AValue: TAxisEditorPage);
    procedure ChangedHandler(Sender: TObject);
    procedure LabelFontChangedHandler(Sender: TObject);
    procedure LabelShapeChangedHandler(AShape: TChartLabelShape);
    procedure TitleFontChangedHandler(Sender: TObject);
    procedure TitleShapeChangedHandler(AShape: TChartLabelShape);
  protected
    function GetChart: TChart;
  public
    procedure Prepare(Axis: TChartAxis; ACaptionMask: String);
    property Page: TAxisEditorPage read GetPage write SetPage;

  end;

var
  AxisEditor: TAxisEditor;

implementation

{$R *.lfm}

uses
  ceUtils;

procedure TAxisEditor.cbAutoMaxChange(Sender: TObject);
begin
  FAxis.Range.UseMax := not cbAutoMax.Checked;
end;

procedure TAxisEditor.cbArrowVisibleChange(Sender: TObject);
begin
  FAxis.Arrow.Visible := cbArrowVisible.Checked;
end;

procedure TAxisEditor.cbAutoMinChange(Sender: TObject);
begin
  FAxis.Range.UseMin := not cbAutoMin.Checked;
end;

procedure TAxisEditor.cbFrameVisibleChange(Sender: TObject);
begin
  GetChart.Frame.Visible := cbFrameVisible.Checked;
end;

procedure TAxisEditor.cbGridVisibleChange(Sender: TObject);
begin
  FAxis.Grid.Visible := cbGridVisible.Checked;
end;

procedure TAxisEditor.cbInvertedChange(Sender: TObject);
begin
  FAxis.Inverted := not FAxis.Inverted;
end;

procedure TAxisEditor.cbLabelsVisibleChange(Sender: TObject);
begin
  FAxis.Marks.Visible := cbLabelsVisible.Checked;
end;

procedure TAxisEditor.cbAxisLineVisibleChange(Sender: TObject);
begin
  FAxis.AxisPen.Visible := cbAxisLineVisible.Checked;
end;

procedure TAxisEditor.cbShowChange(Sender: TObject);
begin
  FAxis.Visible := cbShow.Checked;
end;

procedure TAxisEditor.cbTickColorColorChanged(Sender: TObject);
begin
  FAxis.TickColor := cbTickColor.ButtonColor;
end;

procedure TAxisEditor.cbTitleVisibleChange(Sender: TObject);
begin
  FAxis.Title.Visible := cbTitleVisible.Checked;
end;

procedure TAxisEditor.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TAxisEditor.edLabelFormatEditingDone(Sender: TObject);
begin
  try
    FAxis.Marks.Format := edLabelFormat.Text;
  except
  end;
end;

procedure TAxisEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not CanClose then exit;
  if not FOKClicked then begin
    FAxis.Assign(FSavedAxis);
    FAxis.Visible := cbShow.Checked;
    GetChart.Invalidate;
  end;
end;

procedure TAxisEditor.FormCreate(Sender: TObject);
begin
  BoldHeaders(self);

  TitleShapeBrushPenMarginsFrame.OnChange := @ChangedHandler;
  TitleShapeBrushPenMarginsFrame.OnShapeChange := @TitleShapeChangedHandler;
  TitleFontFrame.OnChange := @TitleFontChangedHandler;

  LabelShapeBrushPenMarginsFrame.OnChange := @ChangedHandler;
  LabelShapeBrushPenMarginsFrame.OnShapeChange := @LabelShapeChangedHandler;
  LabelFontFrame.OnChange := @LabelFontChangedHandler;

  FramePenFrame.OnChange := @ChangedHandler;
  AxisLinePenFrame.OnChange := @ChangedHandler;

  GridPenFrame.OnChange := @ChangedHandler;
end;

procedure TAxisEditor.FormDestroy(Sender: TObject);
begin
  FSavedAxis.Free;
end;

procedure TAxisEditor.FormShow(Sender: TObject);
begin
  FOKClicked := false;
end;

function TAxisEditor.GetAlignment(AItemIndex: Integer): TAlignment;
const
  ALIGNMENTS: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
begin
  Result := ALIGNMENTS[AItemIndex];
end;

function TAxisEditor.GetAlignmentIndex(AValue: TAlignment): Integer;
const
  ALIGNMENTS: array[TAlignment] of Integer = (0, 2, 1);
begin
  Result := ALIGNMENTS[AValue];
end;

function TAxisEditor.GetChart: TChart;
begin
  Result := FAxis.Collection.Owner as TChart;
end;

function TAxisEditor.GetPage: TAxisEditorPage;
begin
  Result := TAxisEditorPage(PageControl.ActivePageIndex);
end;

procedure TAxisEditor.HelpButtonClick(Sender: TObject);
begin
  ModalResult := mrYesToAll;
end;

procedure TAxisEditor.LabelFontChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TAxisEditor.LabelShapeChangedHandler(AShape: TChartLabelShape);
begin
  FAxis.Marks.Shape := AShape;
end;

procedure TAxisEditor.mmoTitleChange(Sender: TObject);
begin
  FAxis.Title.Caption := mmoTitle.Lines.Text;
end;

procedure TAxisEditor.OKButtonClick(Sender: TObject);
begin
  FOKClicked := true;
end;

procedure TAxisEditor.Prepare(Axis: TChartAxis;
  ACaptionMask: String);
begin
  FAxis := Axis;
  if FSavedAxis = nil then
    FSavedAxis := TChartAxis.Create(FAxis.Collection);
  FSavedAxis.Assign(FAxis);
  FSavedAxis.Visible := false;
  if FAxis.Title.Caption <> '' then
    Caption := Format(ACaptionMask, [FAxis.Title.Caption])
  else
    Caption := Format(ACaptionMask, ['#' + IntToStr(FAxis.Index)]);

  cbShow.Checked := Axis.Visible;

  // Page "Title"
  cbTitleVisible.Checked := Axis.Title.Visible;
  mmoTitle.Lines.Text := Axis.Title.Caption;
  with Axis.Title do begin
    rgTitleAlignment.ItemIndex := GetAlignmentIndex(Alignment);
    seTitleDistance.Value := Distance;
    TitleFontFrame.Prepare(LabelFont, true);
    TitleShapeBrushPenMarginsFrame.Prepare(Shape, LabelBrush, Frame, Margins);
  end;

  // Page "Labels"
  cbAutoMin.Checked := not Axis.Range.UseMin;
  cbAutoMax.Checked := not Axis.Range.UseMax;
  seMinimum.Value := Axis.Range.Min;
  seMaximum.Value := Axis.Range.Max;
  cbInverted.Checked := Axis.Inverted;
  seTickLength.Value := Axis.TickLength;
  seTickInnerLength.Value := Axis.TickInnerLength;
  cbTickColor.ButtonColor := Axis.TickColor;
  with Axis.Marks do begin
    seLabelDistance.Value := Distance;
    cbLabelsVisible.Checked := Visible;
    edLabelFormat.Text := Format;
    LabelFontFrame.Prepare(LabelFont, true);
    LabelShapeBrushPenMarginsFrame.Prepare(Shape, LabelBrush, Frame, Margins);
  end;

  // Page "Grid"
  GridPenFrame.Prepare(FAxis.Grid);

  // Page "Line"
  cbFrameVisible.Checked := GetChart.Frame.EffVisible;
  FramePenFrame.Prepare(GetChart.Frame);
  cbAxisLineVisible.Checked := FAxis.AxisPen.EffVisible;
  AxisLinePenFrame.Prepare(FAxis.AxisPen);
  cbArrowVisible.Checked := FAxis.Arrow.Visible;
  seArrowBaseLength.Value := FAxis.Arrow.BaseLength;
  seArrowLength.Value := FAxis.Arrow.Length;
  seArrowWidth.Value := FAxis.Arrow.Width;
end;

procedure TAxisEditor.rgTitleAlignmentClick(Sender: TObject);
begin
  FAxis.Title.Alignment := GetAlignment(rgTitleAlignment.ItemIndex);
end;

procedure TAxisEditor.seArrowBaseLengthChange(Sender: TObject);
begin
  FAxis.Arrow.BaseLength := seArrowBaseLength.value;
end;

procedure TAxisEditor.seArrowLengthChange(Sender: TObject);
begin
  FAxis.Arrow.Length := seArrowLength.Value;
end;

procedure TAxisEditor.seArrowWidthChange(Sender: TObject);
begin
  FAxis.Arrow.Width := seArrowWidth.Value;
end;

procedure TAxisEditor.seLabelDistanceChange(Sender: TObject);
begin
  FAxis.Marks.Distance := seLabelDistance.Value;
end;

procedure TAxisEditor.seTitleDistanceChange(Sender: TObject);
begin
  FAxis.Title.Distance := seTitleDistance.Value;
end;

procedure TAxisEditor.seMaximumChange(Sender: TObject);
begin
  FAxis.Range.Max := seMaximum.Value;
end;

procedure TAxisEditor.seMinimumChange(Sender: TObject);
begin
  FAxis.Range.Min := seMinimum.Value;
end;

procedure TAxisEditor.seTickLengthChange(Sender: TObject);
begin
  FAxis.TickLength := seTickLength.Value;
end;

procedure TAxisEditor.seTickInnerLengthChange(Sender: TObject);
begin
  FAxis.TickInnerLength := seTickInnerLength.Value;
end;

procedure TAxisEditor.SetPage(AValue: TAxisEditorPage);
begin
  PageControl.ActivePageIndex := ord(AValue);
end;

procedure TAxisEditor.TitleFontChangedHandler(Sender: TObject);
begin
  mmoTitle.Font.Assign(FAxis.Title.LabelFont);
end;

procedure TAxisEditor.TitleShapeChangedHandler(AShape: TChartLabelShape);
begin
  FAxis.Title.Shape := AShape;
end;


end.

