unit ceShapeBrushPenMarginsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, Dialogs, Spin, ExtCtrls,
  TATypes, TATextElements,
  ceSimplePenFrame, ceSimpleBrushFrame;

type

  TChartShapeChangeEvent = procedure (AShape: TChartLabelShape) of object;

  { TChartShapeBrushPenMarginsFrame }

  TChartShapeBrushPenMarginsFrame = class(TFrame)
    cmbShape: TComboBox;
    gbBackground: TGroupBox;
    gbBorder: TGroupBox;
    gbMargins: TGroupBox;
    Panel1: TPanel;
    seBottomMargin: TSpinEdit;
    seLeftMargin: TSpinEdit;
    seRightMargin: TSpinEdit;
    seTopMargin: TSpinEdit;
    procedure cmbShapeChange(Sender: TObject);
    procedure seBottomMarginChange(Sender: TObject);
    procedure seLeftMarginChange(Sender: TObject);
    procedure seRightMarginChange(Sender: TObject);
    procedure seTopMarginChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FOnShapeChange: TChartShapeChangeEvent;
    FBrush: TBrush;
    FPen: TChartPen;
    FMargins: TChartLabelMargins;
    FShape: TChartLabelShape;
    FLockEvents: Integer;
    FBrushFrame: TSimpleChartBrushFrame;
    FPenFrame: TSimpleChartPenFrame;
    procedure ChangeHandler(Sender: TObject);
    procedure DoChange;
    procedure DoShapeChanged(AShape: TChartLabelShape);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(out AShape: TChartLabelShape; ABrush: TBrush;
      APen: TChartPen; AMargins: TChartLabelMargins);
    procedure Prepare(AShape: TChartLabelShape; ABrush: TBrush; APen: TChartPen;
      AMargins: TChartLabelMargins);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnShapeChange: TChartShapeChangeEvent read FOnShapeChange write FOnShapeChange;
  end;

implementation

{$R *.lfm}

uses
  Math,
  ceUtils;

{ TChartShapeBrushPenMarginsFrame }

constructor TChartShapeBrushPenMarginsFrame.Create(AOwner: TComponent);
begin
  inherited;

  cmbShape.DropdownCount := DEFAULT_DROPDOWN_COUNT;

  FBrushFrame := TSimpleChartBrushFrame.Create(self);
  FBrushFrame.Name := '';
  FBrushFrame.BorderSpacing.Around := 8;
  FBrushFrame.Align := alClient;
  FBrushFrame.OnChange := @ChangeHandler;
  FBrushFrame.AutoSize := true;
  FBrushFrame.Parent := gbBackground;
  gbBackground.AutoSize := true;
  gbBackground.Caption := 'Background';

  FPenFrame := TSimpleChartPenFrame.Create(self);
  FPenFrame.Name := '';
  FPenFrame.BorderSpacing.Around := 8;
  FPenFrame.Align := alClient;
  FPenFrame.OnChange := @ChangeHandler;
  FPenFrame.AutoSize := true;
  FPenFrame.Parent := gbBorder;
  gbBorder.AutoSize := true;
  gbBorder.Caption := 'Border';

  Panel1.AutoSize := true;
end;

procedure TChartShapeBrushPenMarginsFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  w: Integer = 0;
  h: Integer = 0;
begin
  PreferredHeight := cmbShape.Height +
    gbBorder.BorderSpacing.Top + gbBorder.Height +
    gbMargins.BorderSpacing.Top + gbMargins.Height;

  gbBorder.GetPreferredSize(w, h);
  PreferredWidth :=
    gbBackground.Width + w + FPenFrame.BorderSpacing.Around*2 +
    Max(gbBackground.BorderSpacing.Right, gbBorder.BorderSpacing.Left);
end;

procedure TChartShapeBrushPenMarginsFrame.ChangeHandler(Sender: TObject);
begin
  if FLockEvents = 0 then
  begin
    if (Sender is TChartPen) then
      FPen := TChartPen(Sender)
    else if (Sender is TBrush) then
      FBrush := TBrush(Sender);
    cmbShape.Enabled := FPen.EffVisible or (FBrush.Style <> bsClear);
    gbMargins.Enabled := cmbShape.Enabled;
    DoChange;
  end;
end;

procedure TChartShapeBrushPenMarginsFrame.cmbShapeChange(Sender: TObject);
begin
  DoShapeChanged(TChartLabelShape(cmbShape.ItemIndex));
end;

procedure TChartShapeBrushPenMarginsFrame.DoChange;
begin
  if (FLockEvents = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TChartShapeBrushPenMarginsFrame.DoShapeChanged(AShape: TChartLabelShape);
begin
  if (FLockEvents = 0) and Assigned(FOnShapeChange) then
    FOnShapeChange(AShape);
end;

procedure TChartShapeBrushPenMarginsFrame.seBottomMarginChange(Sender: TObject);
begin
  FMargins.Bottom := seBottomMargin.Value;
  DoChange;
end;

procedure TChartShapeBrushPenMarginsFrame.seLeftMarginChange(Sender: TObject);
begin
  FMargins.Left := seLeftMargin.Value;
  DoChange;
end;

procedure TChartShapeBrushPenMarginsFrame.seRightMarginChange(Sender: TObject);
begin
  FMargins.Right := seRightMargin.Value;
  DoChange;
end;

procedure TChartShapeBrushPenMarginsFrame.seTopMarginChange(Sender: TObject);
begin
  FMargins.Top := seTopMargin.Value;
  DoChange;
end;

procedure TChartShapeBrushPenMarginsFrame.GetData(out AShape: TChartLabelShape;
  ABrush: TBrush; APen: TChartPen; AMargins: TChartLabelMargins);
begin
  AShape := TChartLabelShape(cmbShape.ItemIndex);
  if HandleAllocated then
  begin
    FBrushFrame.GetData(ABrush);
    FPenFrame.GetData(APen);
    APen.Style := psSolid;
  end;
  AMargins.Top := seTopMargin.Value;
  AMargins.Left := seLeftMargin.Value;
  AMargins.Right := seRightMargin.Value;
  AMargins.Bottom := seBottomMargin.Value;
end;

procedure TChartShapeBrushPenMarginsFrame.Prepare(AShape: TChartLabelShape;
  ABrush: TBrush; APen: TChartPen; AMargins: TChartLabelMargins);
begin
  inc(FLockEvents);

  FShape := AShape;
  FBrush := ABrush;
  FPen := APen;
  FMargins := AMargins;

  cmbShape.ItemIndex := ord(AShape);
  cmbShape.Enabled := APen.EffVisible or (ABrush.Style <> bsClear);
  gbMargins.Enabled := cmbShape.Enabled;
  FBrushFrame.Prepare(ABrush);
  FPenFrame.Prepare(APen);
  seTopMargin.Value := AMargins.Top;
  seLeftMargin.Value := AMargins.Left;
  seRightMargin.Value := AMargins.Right;
  seBottomMargin.Value := AMargins.Bottom;

  dec(FLockEvents);

end;

end.

