unit ceTitleFootDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, Buttons, ComCtrls, ceShapeBrushPenMarginsFrame,
  ceFontFrame, TAGraph, TATextElements;

type

  { TTitleFootEditor }

  TTitleFootEditor = class(TForm)
    ButtonPanel: TButtonPanel;
    cbShow: TCheckBox;
    FontFrame1: TFontFrame;
    gbShapeBrushPenMargins: TGroupBox;
    gbFont: TGroupBox;
    lblText: TLabel;
    mmoText: TMemo;
    MemoPanel: TPanel;
    PanelTop: TPanel;
    ParamsPanel: TPanel;
    rgAlignment: TRadioGroup;
    ShapeBrushPenMarginsFrame1: TShapeBrushPenMarginsFrame;
    procedure cbShowChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTextChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
  private
    FTitle: TChartTitle;
    FSavedTitle: TChartTitle;
    FOKClicked: boolean;
    procedure ChangedHandler(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
  protected
    function GetChart: TChart;
  public
    procedure Prepare(ATitle: TChartTitle; ACaption: String = '');

  end;

var
  AxisTitleEditor: TTitleFootEditor;

implementation

{$R *.lfm}

uses
  ceUtils;

type
  TChartTitleAccess = class(TChartTitle);

procedure TTitleFootEditor.cbShowChange(Sender: TObject);
begin
  lblText.Visible := cbShow.Checked;
  mmoText.Visible := cbShow.Checked;
  rgAlignment.Visible := cbShow.Checked;
  gbShapeBrushPenMargins.Visible := cbShow.Checked;
  gbFont.Visible := cbShow.Checked;
end;

procedure TTitleFootEditor.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
  mmoText.Font.Assign(FTitle.Font);
end;

procedure TTitleFootEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if not CanClose then exit;
  if not FOKClicked then begin
    FTitle.Assign(FSavedTitle);
    GetChart.Invalidate;
  end;
end;

procedure TTitleFootEditor.FormCreate(Sender: TObject);
begin
  BoldHeaders(Self);
  FontFrame1.OnChange := @ChangedHandler;
end;

procedure TTitleFootEditor.FormDestroy(Sender: TObject);
begin
  FSavedTitle.Free;
end;

procedure TTitleFootEditor.FormShow(Sender: TObject);
begin
  if cbShow.Checked then begin
    AutoSize := true;
    Constraints.MinWidth := Width;
    Constraints.MinHeight := Height;
    AutoSize := false;
  end;
end;

procedure TTitleFootEditor.mmoTextChange(Sender: TObject);
begin
  FTitle.Text.Assign(mmoText.Lines);
end;

function TTitleFootEditor.GetAlignment: TAlignment;
const
  ALIGNMENTS: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
begin
  Result := ALIGNMENTS[rgAlignment.ItemIndex];
end;

function TTitleFootEditor.GetChart: TChart;
begin
  Result := TChartTitleAccess(FTitle).GetOwner as TChart;
end;

procedure TTitleFootEditor.OKButtonClick(Sender: TObject);
begin
  FOKClicked := true;
end;

procedure TTitleFootEditor.Prepare(ATitle: TChartTitle; ACaption: String = '');
begin
  FTitle := ATitle;
  if FSavedTitle = nil then
    FSavedTitle := TChartTitle.Create(TChart(TChartTitleAccess(FTitle).GetOwner));
  FSavedTitle.Assign(FTitle);

  if ACaption <> '' then
    Caption := ACaption;

  cbShow.Checked := ATitle.Visible;
  mmoText.Lines.Assign(ATitle.Text);
  SetAlignment(ATitle.Alignment);

  mmoText.Font.Assign(ATitle.Font);
  FontFrame1.Prepare(ATitle.Font, false);
  ShapeBrushPenMarginsFrame1.Prepare(ATitle.Shape, ATitle.Brush, ATitle.Frame, ATitle.Margins);
end;

procedure TTitleFootEditor.rgAlignmentClick(Sender: TObject);
begin
  FTitle.Alignment := GetAlignment;
end;

procedure TTitleFootEditor.SetAlignment(AValue: TAlignment);
const
  ALIGNMENTS: array[TAlignment] of Integer = (0, 2, 1);
begin
  rgAlignment.ItemIndex := ALIGNMENTS[AValue];
end;

end.

