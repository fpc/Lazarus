unit ceLegendDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, Buttons, ColorBox, ComCtrls, Spin, ceFontFrame,
  TAGraph, TALegend;

type

  { TLegendEditor }

  TLegendEditor = class(TForm)
    Bevel1: TBevel;
    ButtonPanel: TButtonPanel;
    cbFilled: TCheckBox;
    cbShowBorder: TCheckBox;
    cbShow: TCheckBox;
    cbFillColor: TColorBox;
    cbBorderColor: TColorBox;
    cbInverted: TCheckBox;
    cbUseSideBar: TCheckBox;
    cbItemFillOrder: TComboBox;
    FontFrame1: TFontFrame;
    gbBackground: TGroupBox;
    gbBorder: TGroupBox;
    gbFont: TGroupBox;
    gbMargins: TGroupBox;
    gbItems: TGroupBox;
    lblMarginX: TLabel;
    lblMarginY: TLabel;
    lblItemFillOrder: TLabel;
    lblColumns: TLabel;
    lblSymbolWidth: TLabel;
    lblSpacing: TLabel;
    PanelTop: TPanel;
    ParamsPanel: TPanel;
    rbTopLeft: TRadioButton;
    rbTopCenter: TRadioButton;
    rbTopRight: TRadioButton;
    rbCenterLeft: TRadioButton;
    rbCenterRight: TRadioButton;
    rbBottomLeft: TRadioButton;
    rbBottomCenter: TRadioButton;
    rbBottomRight: TRadioButton;
    gbAlignment: TGroupBox;
    seColumns: TSpinEdit;
    seMarginX: TSpinEdit;
    seMarginY: TSpinEdit;
    seSymbolWidth: TSpinEdit;
    seSpacing: TSpinEdit;
    procedure cbBorderColorChange(Sender: TObject);
    procedure cbFillColorChange(Sender: TObject);
    procedure cbInvertedChange(Sender: TObject);
    procedure cbItemFillOrderChange(Sender: TObject);
    procedure cbFilledChange(Sender: TObject);
    procedure cbShowBorderChange(Sender: TObject);
    procedure cbShowChange(Sender: TObject);
    procedure cbUseSideBarChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rbAlignmentChange(Sender: TObject);
    procedure seColumnsChange(Sender: TObject);
    procedure seMarginXChange(Sender: TObject);
    procedure seMarginYChange(Sender: TObject);
    procedure seSpacingChange(Sender: TObject);
    procedure seSymbolWidthChange(Sender: TObject);
  private
    FLegend: TChartLegend;
    FSavedLegend: TChartLegend;
    FOKClicked: Boolean;
    procedure ChangedHandler(Sender: TObject);
    function GetAlignment: TLegendAlignment;
    procedure SetAlignment(AValue: TLegendAlignment);
  protected
    function GetChart: TChart;
  public
    procedure Prepare(ALegend: TChartLegend; ACaption: String = '');

  end;

var
  AxisTitleEditor: TLegendEditor;

implementation

{$R *.lfm}

uses
  ceUtils;

type
  TChartLegendAccess = class(TChartLegend);

procedure TLegendEditor.cbBorderColorChange(Sender: TObject);
begin
  FLegend.Frame.Color := cbBorderColor.Selected;
end;

procedure TLegendEditor.cbFillColorChange(Sender: TObject);
begin
  FLegend.BackgroundBrush.Color := cbFillColor.Selected;
end;

procedure TLegendEditor.cbInvertedChange(Sender: TObject);
begin
  FLegend.Inverted := cbInverted.Checked;
end;

procedure TLegendEditor.cbItemFillOrderChange(Sender: TObject);
begin
  FLegend.ItemFillOrder := TLegendItemFillOrder(cbItemFillOrder.ItemIndex);
end;

procedure TLegendEditor.cbFilledChange(Sender: TObject);
begin
  cbFillColor.Visible := cbFilled.Checked;
  if cbFilled.Checked then
    FLegend.BackgroundBrush.Style := bsSolid
  else
    FLegend.BackgroundBrush.Style := bsClear;
end;

procedure TLegendEditor.cbShowBorderChange(Sender: TObject);
begin
  FLegend.Frame.Visible := cbShowBorder.Checked;
  cbBorderColor.Visible := cbShowBorder.Checked;
end;

procedure TLegendEditor.cbShowChange(Sender: TObject);
begin
  FLegend.Visible := cbShow.Checked;
  cbUseSideBar.Visible := cbShow.Checked;
  gbAlignment.Visible := cbShow.Checked;
  gbFont.Visible := cbShow.Checked;
  gbBackground.Visible := cbShow.Checked;
  gbBorder.Visible := cbShow.Checked;
  gbItems.Visible := cbShow.Checked;
  gbMargins.Visible := cbShow.Checked;
end;

procedure TLegendEditor.cbUseSideBarChange(Sender: TObject);
begin
  FLegend.UseSideBar := cbUseSideBar.Checked;
end;

procedure TLegendEditor.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
end;

procedure TLegendEditor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not CanClose then exit;
  if not FOKClicked then begin
    FLegend.Assign(FSavedLegend);
    GetChart.Invalidate;
  end;
end;

procedure TLegendEditor.FormCreate(Sender: TObject);
begin
  BoldHeaders(Self);
  FontFrame1.OnChange := @ChangedHandler;
end;

procedure TLegendEditor.FormDestroy(Sender: TObject);
begin
  FSavedLegend.Free;
end;

procedure TLegendEditor.FormShow(Sender: TObject);
begin
  if cbShow.Checked then begin
    AutoSize := true;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    AutoSize := false;
  end;

  FOKClicked := false;
end;

function TLegendEditor.GetAlignment: TLegendAlignment;
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
end;

function TLegendEditor.GetChart: TChart;
begin
  Result := TChartLegendAccess(FLegend).GetOwner as TChart;
end;

procedure TLegendEditor.OKButtonClick(Sender: TObject);
begin
  FOKClicked := true;
end;

procedure TLegendEditor.rbAlignmentChange(Sender: TObject);
begin
  FLegend.Alignment := GetAlignment;
end;

procedure TLegendEditor.seColumnsChange(Sender: TObject);
begin
  FLegend.ColumnCount := seColumns.Value;
end;

procedure TLegendEditor.seMarginXChange(Sender: TObject);
begin
  FLegend.MarginX := seMarginX.Value;
end;

procedure TLegendEditor.seMarginYChange(Sender: TObject);
begin
  FLegend.MarginY := seMarginY.Value;
end;

procedure TLegendEditor.seSpacingChange(Sender: TObject);
begin
  FLegend.Spacing := seSpacing.Value;
end;

procedure TLegendEditor.seSymbolWidthChange(Sender: TObject);
begin
  FLegend.SymbolWidth := seSymbolWidth.Value;
end;

procedure TLegendEditor.SetAlignment(AValue: TLegendAlignment);
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

procedure TLegendEditor.Prepare(ALegend: TChartLegend;
  ACaption: String = '');
begin
  FLegend := ALegend;
  if FSavedLegend = nil then
    FSavedLegend := TChartLegend.Create(nil);
  FSavedLegend.Assign(ALegend);

  if ACaption <> '' then
    Caption := ACaption;

  cbShow.Checked := ALegend.Visible;
  SetAlignment(ALegend.Alignment);

  cbFilled.Checked := ALegend.BackgroundBrush.Style <> bsClear;
  cbFillColor.Selected := ALegend.BackgroundBrush.Color;

  cbShowBorder.Checked := (ALegend.Frame.Style <> psClear) and ALegend.Frame.Visible;
  cbBorderColor.Selected := ALegend.Frame.Color;

  seMarginX.Value := ALegend.MarginX;
  seMarginY.Value := ALegend.MarginY;

  cbUseSideBar.Checked := ALegend.UseSidebar;
  cbInverted.Checked := ALegend.Inverted;
  seColumns.Value := ALegend.ColumnCount;
  seSymbolWidth.Value := ALegend.SymbolWidth;
  seSpacing.Value := ALegend.Spacing;
  cbItemFillOrder.ItemIndex := ord(ALegend.ItemFillOrder);

  FontFrame1.Prepare(ALegend.Font, false);
end;

end.

