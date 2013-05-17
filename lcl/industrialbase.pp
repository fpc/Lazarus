unit IndustrialBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics;

type

  { TIndustrialBase }

  TIndustrialBase = class(TGraphicControl)
  private
    FAntiAliasingMode: TAntialiasingMode;
    procedure SetAntiAliasingMode(AValue: TAntialiasingMode);
  protected
    procedure GraphicChanged;
    procedure Paint; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property AntiAliasingMode: TAntialiasingMode read FAntiAliasingMode
                                    write SetAntiAliasingMode default amDontCare;
  end;

implementation

constructor TIndustrialBase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAntiAliasingMode := amDontCare;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TIndustrialBase.GraphicChanged;
begin
  if Assigned(Parent) and (Visible or (csDesigning in ComponentState))
   then Invalidate;
end;

procedure TIndustrialBase.Paint;
begin
  Canvas.AntialiasingMode := FAntiAliasingMode;
  inherited Paint;
end;

procedure TIndustrialBase.SetAntiAliasingMode(AValue: TAntialiasingMode);
begin
  if FAntiAliasingMode=AValue then Exit;
  FAntiAliasingMode:=AValue;
  GraphicChanged;
end;


end.

