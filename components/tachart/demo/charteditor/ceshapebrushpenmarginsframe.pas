unit ceShapeBrushPenMarginsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, Dialogs, Spin, ExtCtrls,
  TATextElements;

type

  TShapeChangeEvent = procedure (AShape: TChartLabelShape) of object;

  { TShapeBrushPenMarginsFrame }

  TShapeBrushPenMarginsFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    cbBorderColor: TColorButton;
    cbFillColor: TColorButton;
    cbFilled: TCheckBox;
    cbShowBorder: TCheckBox;
    cmbShape: TComboBox;
    gbBackground: TGroupBox;
    gbBorder: TGroupBox;
    gbMargins: TGroupBox;
    seBottomMargin: TSpinEdit;
    seLeftMargin: TSpinEdit;
    seRightMargin: TSpinEdit;
    seTopMargin: TSpinEdit;
    procedure cbBorderColorColorChanged(Sender: TObject);
    procedure cbFillColorColorChanged(Sender: TObject);
    procedure cbFilledChange(Sender: TObject);
    procedure cbShowBorderChange(Sender: TObject);
    procedure cmbShapeChange(Sender: TObject);
    procedure seBottomMarginChange(Sender: TObject);
    procedure seLeftMarginChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure seTopMarginChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FOnShapeChange: TShapeChangeEvent;
    FBrush: TBrush;
    FPen: TPen;
    FMargins: TChartLabelMargins;
    FShape: TChartLabelShape;
    procedure DoChanged;
    procedure DoShapeChanged(AShape: TChartLabelShape);
    procedure UpdateControls;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(out AShape: TChartLabelShape; ABrush: TBrush;
      APen: TPen; AMargins: TChartLabelMargins);
    procedure Prepare(AShape: TChartLabelShape; ABrush: TBrush; APen: TPen;
      AMargins: TChartLabelMargins);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnShapeChange: TShapeChangeEvent read FOnShapeChange write FOnShapeChange;
  end;

implementation

{$R *.lfm}

uses
  ceUtils;

{ TShapeBrushPenMarginsFrame }

constructor TShapeBrushPenMarginsFrame.Create(AOwner: TComponent);
begin
  inherited;
  cbFillColor.Width := cbFillColor.Height;
  cbBorderColor.Width := cbBorderColor.Height;
  cmbShape.DropdownCount := DEFAULT_DROPDOWN_COUNT;
end;

procedure TShapeBrushPenMarginsFrame.cbBorderColorColorChanged(Sender: TObject);
begin
  FPen.Color := cbBorderColor.ButtonColor;
  if FPen.Style <> psClear then
    DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.cbFillColorColorChanged(Sender: TObject);
var
  bs: TBrushStyle;
begin
  bs := FBrush.Style;
  FBrush.Color := cbFillColor.ButtonColor;
  FBrush.Style := bs;
  if FBrush.Style <> bsClear then
    DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.cbFilledChange(Sender: TObject);
begin
  if cbFilled.Checked then FBrush.Style := bsSolid else FBrush.Style := bsClear;
  UpdateControls;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.cbShowBorderChange(Sender: TObject);
begin
  if cbShowBorder.Checked then FPen.Style := psSolid else FPen.Style := psClear;
  UpdateControls;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.DoChanged;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TShapeBrushPenMarginsFrame.DoShapeChanged(AShape: TChartLabelShape);
begin
  if Assigned(FOnShapeChange) then FOnShapeChange(AShape);
end;

procedure TShapeBrushPenMarginsFrame.cmbShapeChange(Sender: TObject);
begin
  FShape := TChartLabelShape(cmbShape.ItemIndex);
  DoShapeChanged(FShape);
end;

procedure TShapeBrushPenMarginsFrame.seBottomMarginChange(Sender: TObject);
begin
  FMargins.Bottom := seBottomMargin.Value;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.seLeftMarginChange(Sender: TObject);
begin
  FMargins.Left := seLeftMargin.Value;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.seRightMarginChange(Sender: TObject);
begin
  FMargins.Right := seRightMargin.Value;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.seTopMarginChange(Sender: TObject);
begin
  FMargins.Top := seTopMargin.Value;
  DoChanged;
end;

procedure TShapeBrushPenMarginsFrame.GetData(out AShape: TChartLabelShape;
  ABrush: TBrush; APen: TPen; AMargins: TChartLabelMargins);
begin
  AShape := TChartLabelShape(cmbShape.ItemIndex);
  if cbFilled.Checked then ABrush.Style := bsSolid else ABrush.Style := bsClear;
  ABrush.Color := cbFillColor.ButtonColor;
  if cbShowBorder.Checked then APen.Style := psSolid else APen.Style := psClear;
  APen.Color := cbBorderColor.ButtonColor;
  AMargins.Top := seTopMargin.Value;
  AMargins.Left := seLeftMargin.Value;
  AMargins.Right := seRightMargin.Value;
  AMargins.Bottom := seBottomMargin.Value;
end;

procedure TShapeBrushPenMarginsFrame.Prepare(AShape: TChartLabelShape;
  ABrush: TBrush; APen: TPen; AMargins: TChartLabelMargins);
begin
  FShape := AShape;
  FBrush := ABrush;
  FPen := APen;
  FMargins := AMargins;
  cmbShape.ItemIndex := ord(AShape);
  cbFilled.Checked := ABrush.Style <> bsClear;
  cbFillColor.ButtonColor := ABrush.Color;
  cbShowBorder.Checked := APen.Style <> psClear;
  cbBorderColor.ButtonColor := APen.Color;
  seTopMargin.Value := AMargins.Top;
  seLeftMargin.Value := AMargins.Left;
  seRightMargin.Value := AMargins.Right;
  seBottomMargin.Value := AMargins.Bottom;
end;

procedure TShapeBrushPenMarginsFrame.UpdateControls;
begin
  cbBorderColor.Visible := cbShowBorder.Checked;
  cmbShape.Enabled := cbShowBorder.Checked or cbFilled.Checked;
  cbFillColor.Visible := cbFilled.Checked;
end;

end.

