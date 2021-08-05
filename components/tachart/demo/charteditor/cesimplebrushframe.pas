unit ceSimpleBrushFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls, Dialogs;

type

  { TSimpleChartBrushFrame }

  TSimpleChartBrushFrame = class(TFrame)
    cbFillColor: TColorButton;
    cbFilled: TCheckBox;
    procedure cbFillColorColorChanged(Sender: TObject);
    procedure cbFilledChange(Sender: TObject);
  private
    FBrush: TBrush;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetData(ABrush: TBrush);
    procedure Prepare(ABrush: TBrush);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.lfm}

constructor TSimpleChartBrushFrame.Create(AOwner: TComponent);
begin
  inherited;
  cbFillColor.Width := cbFillColor.Height;
end;

procedure TSimpleChartBrushFrame.cbFillColorColorChanged(Sender: TObject);
var
  bs: TBrushStyle;
begin
  // Be careful: Setting the Brush.Color switches the Style of a transparent brush
  // to solid. --> We store the style to reset it and after changing the color.
  bs := FBrush.Style;
  FBrush.Color := cbFillColor.ButtonColor;
  FBrush.Style := bs;
  DoChange;
end;

procedure TSimpleChartBrushFrame.cbFilledChange(Sender: TObject);
begin
  if cbFilled.Checked then FBrush.Style := bsSolid else FBrush.Style := bsClear;
  cbFillColor.Enabled := cbFilled.Checked;
  DoChange;
end;

procedure TSimpleChartBrushFrame.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(FBrush);
end;

// Transfers the properties of the GUI elements (or the internal brush) to the
// brush provided as parameter.
procedure TSimpleChartBrushFrame.GetData(ABrush: TBrush);
begin
  ABrush.Assign(FBrush);
end;

// Applies the brush properties to the GUI elements
procedure TSimpleChartBrushFrame.Prepare(ABrush: TBrush);
begin
  FBrush := ABrush;
  cbFilled.Checked := ABrush.Style <> bsClear;
  if ABrush.Color = clDefault then
    cbFillColor.ButtonColor := ColorToRGB(clWindow)
  else
    cbFillColor.ButtonColor := ColorToRGB(ABrush.Color);
end;

end.

