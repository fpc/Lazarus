unit cePointerFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Spin,
  TATypes, TAChartCombos, TAGraph,
  ceBrushFrame, cePenFrame;

type

  { TPointerFrame }

  TPointerFrame = class(TFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    cbPointerStyle: TChartComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblPointerWidth: TLabel;
    lblPointerHeight: TLabel;
    PointerPenFrame: TPenFrame;
    PointerBrushFrame: TBrushFrame;
    sePointerWidth: TSpinEdit;
    sePointerHeight: TSpinEdit;
    procedure cbPointerStyleChange(Sender: TObject);
    procedure sePointerHeightChange(Sender: TObject);
    procedure sePointerWidthChange(Sender: TObject);
  private
    FPointer: TSeriesPointer;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  protected
    function GetChart: TChart;

  public
    procedure Prepare(APointer: TSeriesPointer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.lfm}

type
  TSeriesPointerAccess = class(TSeriesPointer);

procedure TPointerFrame.cbPointerStyleChange(Sender: TObject);
begin
  FPointer.Style := cbPointerStyle.PointerStyle;
  DoChange;
end;

procedure TPointerFrame.sePointerHeightChange(Sender: TObject);
begin
  FPointer.VertSize := sePointerHeight.Value;
  DoChange;
end;

procedure TPointerFrame.sePointerWidthChange(Sender: TObject);
begin
  FPointer.HorizSize := sePointerWidth.Value;
  DoChange;
end;

procedure TPointerFrame.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(FPointer);
end;

function TPointerFrame.GetChart: TChart;
begin
  Result := TSeriesPointerAccess(FPointer).GetOwner as TChart;
end;

procedure TPointerFrame.Prepare(APointer: TSeriesPointer);
begin
  FPointer := APointer;
  cbPointerStyle.PointerStyle := APointer.Style;
  sePointerWidth.Value := APointer.HorizSize;
  sePointerHeight.Value := APointer.VertSize;
  PointerBrushFrame.Prepare(APointer.Brush);
  PointerPenFrame.Prepare(APointer.Pen);
end;

end.

