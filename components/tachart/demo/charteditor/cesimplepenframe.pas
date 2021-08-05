unit ceSimplePenFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, Dialogs,
  TATypes, TAChartCombos;

type

  { TSimpleChartPenFrame }

  TSimpleChartPenFrame = class(TFrame)
    cbPenColor: TColorButton;
    cbVisible: TCheckBox;
    cbPenWidth: TChartComboBox;
    lblPenWidth: TLabel;
    procedure cbPenColorColorChanged(Sender: TObject);
    procedure cbPenWidthChange(Sender: TObject);
    procedure cbVisibleChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FPen: TChartPen;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    function GetWidthLeft: Integer;
    procedure SetWidthLeft(const AValue: Integer);
  protected
    procedure CalculatePreferredSize(
      var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(APen: TChartPen);
    procedure Prepare(APen: TChartPen);
    property WidthLeft: Integer read GetWidthLeft write SetWidthLeft;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, Math;

{ TSimpleChartPenFrame }

constructor TSimpleChartPenFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  cbPenWidth.DropdownCount := DEFAULT_DROPDOWN_COUNT;
  cbPenColor.Width := cbPenColor.Height;
end;

procedure TSimpleChartPenFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredWidth := Max(
    cbPenColor.Left + cbPenColor.Width,
    cbPenWidth.Left + cbPenWidth.Constraints.MinWidth
  );
  PreferredHeight := cbPenWidth.Top + cbPenWidth.Height;
end;

procedure TSimpleChartPenFrame.cbPenColorColorChanged(Sender: TObject);
begin
  FPen.Color := cbPenColor.ButtonColor;
  DoChange;
end;

procedure TSimpleChartPenFrame.cbPenWidthChange(Sender: TObject);
begin
  FPen.Width := cbPenWidth.PenWidth;
  DoChange;
end;

procedure TSimpleChartPenFrame.cbVisibleChange(Sender: TObject);
begin
  FPen.Visible := cbVisible.Checked;
  if FPen.Visible then FPen.Style := psSolid else FPen.Style := psClear;
  cbPenColor.Enabled := cbVisible.Checked;
  cbPenWidth.Enabled := cbVisible.Checked;
  lblPenWidth.Enabled := cbVisible.Checked;
  DoChange;
end;

procedure TSimpleChartPenFrame.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(FPen);
end;

procedure TSimpleChartPenFrame.FrameResize(Sender: TObject);
begin
  cbPenWidth.SymbolWidth := cbPenWidth.ClientWidth - GetSystemMetrics(SM_CXVSCROLL) - 16;
end;

// Transfers the properties of the GUI elements (or the internal pen) to the pen.
procedure TSimpleChartPenFrame.GetData(APen: TChartPen);
begin
  APen.Assign(FPen);
end;

function TSimpleChartPenFrame.GetWidthLeft: Integer;
begin
  Result := cbPenWidth.Left;
end;

// Applies the pen properties to the GUI elements
procedure TSimpleChartPenFrame.Prepare(APen: TChartPen);
begin
  FPen := APen;
  cbVisible.Checked := APen.EffVisible;
  cbPenWidth.PenWidth := APen.Width;
  if APen.Color = clDefault then
    cbPenColor.ButtonColor := ColorToRGB(clWindowText)
  else
    cbPenColor.ButtonColor := ColorToRGB(APen.Color);
end;

procedure TSimpleChartPenFrame.SetWidthLeft(const AValue: Integer);
var
  d: Integer;
begin
  d := AValue - lblPenWidth.Width;
  if d > 0 then
    cbPenWidth.BorderSpacing.Left := d;
end;

end.

