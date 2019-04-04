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
    lblPointerSize: TLabel;
    PointerPenFrame: TPenFrame;
    PointerBrushFrame: TBrushFrame;
    sePointerSize: TSpinEdit;
    procedure cbPointerStyleChange(Sender: TObject);
    procedure sePointerSizeChange(Sender: TObject);
  private
    FPointer: TSeriesPointer;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  protected
    function GetChart: TChart;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(APointer: TSeriesPointer);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.lfm}

type
  TSeriesPointerAccess = class(TSeriesPointer);

constructor TPointerFrame.Create(AOwner: TComponent);
begin
  inherited;
  cbPointerStyle.DropdownCount := DEFAULT_DROPDOWN_COUNT;
end;

procedure TPointerFrame.cbPointerStyleChange(Sender: TObject);
begin
  FPointer.Style := cbPointerStyle.PointerStyle;
  DoChange;
end;

procedure TPointerFrame.sePointerSizeChange(Sender: TObject);
begin
  FPointer.HorizSize := sePointerSize.Value;
  FPointer.VertSize := sePointerSize.Value;
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
  sePointerSize.Value := APointer.HorizSize;
  PointerBrushFrame.Prepare(APointer.Brush);
  PointerPenFrame.Prepare(APointer.Pen);
end;

end.

