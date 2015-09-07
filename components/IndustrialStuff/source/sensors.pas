{ Copyright (C) 1998-2000, written by Shkolnik Mike
  FIDOnet: 2:463/106.14
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  TStopLightSensor and TAnalogSensor sensor components
  Modified by Jurassic Pork for Lazarus "Industrial" package
}
unit Sensors;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, LResources, Classes, Controls, Graphics, Stdctrls, Extctrls;

type
  TStopLights = (slUNKNOWN, slRED, slYELLOW, slGREEN);

type
  TSensorPanel = class(TPanel)
  private
    FlblShowText: TLabel;     {sensor value}
    FShowText: Boolean;
    FShowLevel: Boolean;      {show the RED and YELLOW levels or not}

    FValue: Double;
    FValueMin: Double;
    FValueMax: Double;
    FValueRed: Double;
    FValueYellow: Double;

    FColorBack: TColor;
    FColorFore: TColor;
    FColorRed: TColor;
    FColorYellow: TColor;

    function GetCaption: TCaption;
    procedure SetCaption(AValue: TCaption);

    procedure SetShowText(AValue: Boolean);
    procedure SetShowLevel(AValue: Boolean);

    procedure SetColorInd(Index: Integer; AValue: TColor);

    procedure SetValue(AValue: Double); virtual;
    procedure SetValueMin(AValue: Double);
    procedure SetValueMax(AValue: Double);
    procedure SetValueRed(AValue: Double);
    procedure SetValueYellow(AValue: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetStatus: TStopLights;
    procedure SetColorState(slStopLight: TStopLights); virtual;
  published
   property Caption read GetCaption write SetCaption;

    property ShowText: Boolean read FShowText write SetShowText;
    property ShowLevel: Boolean read FShowLevel write SetShowLevel;

    property ColorFore: TColor index 0 read FColorFore write SetColorInd default clLime;
    property ColorBack: TColor index 1 read FColorBack write SetColorInd default clBlack;
    property ColorRed: TColor index 2 read FColorRed   write SetColorInd default clRed;
    property ColorYellow: TColor index 3 read FColorYellow write SetColorInd default clYellow;

    property Value: Double read FValue write SetValue;
    property ValueMin: Double read FValueMin write SetValueMin;
    property ValueMax: Double read FValueMax write SetValueMax;
    property ValueRed: Double read FValueRed write SetValueRed;
    property ValueYellow: Double read FValueYellow write SetValueYellow;
  end;

  TAnalogKind = (akAnalog, akHorizontal, akVertical);
  TAnalogSensor = class(TSensorPanel)
  private
    FAnalogKind: TAnalogKind;

    procedure PaintAsNeedle;
    procedure PaintAsHorizontal;
    procedure PaintAsVertical;
    procedure SetAnalogKind(AValue: TAnalogKind);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Font;
    property AnalogKind: TAnalogKind read FAnalogKind write SetAnalogKind;
  end;

  TStopLightSensor = class(TImage)
  private
    FState: TStopLights;
    procedure SetState(AValue: TStopLights);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Center default True;
    property State: TStopLights read FState write SetState;
  end;


implementation

{$R sensors.res}

uses SysUtils;

{ TSensorPanel }

constructor TSensorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Height := 75;
  Width := 170;
  Parent := AOwner as TWinControl;

  FValue := 0;

  FValueMin := 0;
  FValueMax := 100;
  FValueRed := 30;
  FValueYellow := 60;

  FColorFore := {clGreen} clLime;
  FColorBack := clBlack {clWhite};
  FColorRed := clRed;
  FColorYellow := clYellow;

  FlblShowText := TLabel.Create(Self);
  with FlblShowText do
  begin
    Alignment := taCenter;
    AutoSize := False;
    Font := Self.Font;
    Height := 17;
    Left := 5;
    Top := 57;
    Width := 160;
    Parent := Self;
    Align := alBottom;
  end;

  FShowLevel := True;
  Caption := '';
  ShowText := True;
end;

destructor TSensorPanel.Destroy;
begin
//  FlblShowText.Free;

  inherited Destroy;
end;

function TSensorPanel.GetStatus: TStopLights;
begin
  Result := slUNKNOWN;
  if (Value > ValueMin) and (Value < ValueMin) then Result := slGREEN;
  if (Value < ValueYellow) then Result := slYellow;
  if (Value < ValueRed) then Result := slRED;
end;

procedure TSensorPanel.SetColorState(slStopLight: TStopLights);
begin
  FlblShowText.Font := Font;
  case slStopLight of
    slRED: FlblShowText.Font.Color := FColorRed;
    slYELLOW: FlblShowText.Font.Color := FColorYellow;
  else // slUNKNOWN, slGREEN
//    FlblShowText.Font := Font;
  end;
end;

procedure TSensorPanel.SetValue(AValue: Double);
begin
  if (AValue < FValueMin) then
    AValue := FValueMin
  else
    if (AValue > FValueMax) then
      AValue := FValueMax;
  if (FValue <> AValue) then
  begin
    FValue := AValue;
    FlblShowText.Caption := FlblShowText.Hint + FloatToStr(FValue);
    Invalidate;
  end;
end;

function TSensorPanel.GetCaption: TCaption;
begin
  // Modif J.P 05/2013  Caption replace Hint
  Result := FlblShowText.Hint;
end;

procedure TSensorPanel.SetCaption(AValue: TCaption);
begin
  // Modif J.P 05/2013  Caption replace Hint
  FlblShowText.Hint := AValue;
  inherited Caption := '';
   FlblShowText.Caption := FlblShowText.Hint + FloatToStr(FValue);
  Invalidate;
end;

procedure TSensorPanel.SetShowText(AValue: Boolean);
begin
  if (AValue <> FShowText) then
  begin
    FShowText := AValue;
    FlblShowText.Visible := FShowText;
  end;
end;

procedure TSensorPanel.SetShowLevel(AValue: Boolean);
begin
  if (AValue <> FShowLevel) then
  begin
    FShowlevel := AValue;
    Invalidate;
  end;
end;

procedure TSensorPanel.SetColorInd(Index: Integer; AValue: TColor);
begin
  if (AValue <> FColorFore) then
  begin
    case Index of
      0: FColorFore := AValue;
      1: FColorBack := AValue;
      2: FColorRed := AValue;
      3: FColorYellow := AValue;
    end;
    Invalidate;
  end;
end;

procedure TSensorPanel.SetValueMin(AValue: Double);
begin
  if (AValue <> FValueMin) then
  begin
    if (AValue > FValueMin) then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt('OutOfRange', [-MaxInt, Round(FValueMax - 1)]);
    FValueMin := AValue;
    if (FValue < AValue) then FValue := AValue;
    Invalidate;
  end;
end;

procedure TSensorPanel.SetValueMax(AValue: Double);
begin
  if (AValue <> FValueMax) then
  begin
    if (AValue < FValueMin) then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt('SOutOfRange', [Round(FValueMin + 1), MaxInt]);
    FValueMax := AValue;
    if (FValue > AValue) then FValue := AValue;
    Invalidate;
  end;
end;

procedure TSensorPanel.SetValueRed(AValue: Double);
begin
  if (AValue <> FValueRed) then
  begin
    if (AValue < FValueMin) or (AValue > FValueMax) then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt('SOutOfRange', [Round(FValueMin), Round(FValueMax)]);
    FValueRed := AValue;
    Invalidate;
  end;
end;

procedure TSensorPanel.SetValueYellow(AValue: Double);
begin
  if (AValue <> FValueYellow) then
  begin
    if (AValue < FValueRed) or (AValue > FValueMax) then
      if not (csLoading in ComponentState) then
        raise EInvalidOperation.CreateFmt('SOutOfRange', [Round(FValueRed), Round(FValueMax)]);
    FValueYellow := AValue;
    Invalidate;
  end;
end;


{ TAnalogSensor }
constructor TAnalogSensor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Value := 20;
  AnalogKind := akAnalog;
end;

procedure TAnalogSensor.Paint;
begin
  inherited Paint;
  case FAnalogKind of
    akAnalog: PaintAsNeedle;
    akHorizontal: PaintAsHorizontal;
    akVertical: PaintAsVertical;
  end;
end;

function SolveForY(X, Z: Double): Double;
begin
  if Z = 0 then
    Result := 0
  else
    Result := X/Z;
end;

procedure TAnalogSensor.PaintAsNeedle;
var MiddleX: Integer;
    Angle: Double;
    X, Y, W, H: Integer;
begin
  X := 20;
  Y := 23;
  W := ClientWidth - 2*20; //130;
  H := ClientHeight - 2*23; //33;
  if (W < 1) or (H < 1) then Exit;

  with Canvas do
  begin
    Brush.Color := ColorBack;
    Pen.Color := clBlack;
    Pen.Width := 1;

    { draw a pie }
    Pie(X, Y, X + W, Y + 2*H, X + W, Y + H - 1, X, Y + H - 1);
//    Chord(X, Y, X+W, (Y+H)*2, X+W, Y+H-1, X, Y+H-1);

    MiddleX := W div 2;
    { draw pie for current value }
    Brush.Color := ColorFore;
    Pen.Color := clBlack;
    MoveTo(X + MiddleX, Y + H - 1);
    Angle := Pi * SolveForY(FValue - FValueMin, FValueMax - FValueMin);
    Pie(X, Y, X + W, Y + 2*H, Round(X + MiddleX*(1 - Cos(Angle))), Round(Y - 1 + H*(1 - Sin(Angle))), X, Y+H);

    if FShowLevel then
    begin
//      Pen.Width := 1;
      { draw a RED level line }
      Pen.Color := ColorRed;
      MoveTo(X + MiddleX, Y + H - 1);
      Angle := Pi * SolveForY(FValueRed - FValueMin, FValueMax - FValueMin);
      LineTo(Round(X + MiddleX*(1 - Cos(Angle))), Round(Y - 1 + H*(1 - Sin(Angle))));

      { draw a YELLOW level line }
      Pen.Color := ColorYellow;
      MoveTo(X + MiddleX, Y + H - 1);
      Angle := Pi * SolveForY(FValueYellow - FValueMin, FValueMax - FValueMin);
      LineTo(Round(X + MiddleX*(1 - Cos(Angle))), Round(Y - 1 + H*(1 - Sin(Angle))));
    end;
  end;
end;

procedure TAnalogSensor.PaintAsHorizontal;
var MiddleX: Integer;
    X, Y, W, H: Integer;
begin
  X := 20;
  Y := 23;
  W := ClientWidth - 2*20; //130;
  H := ClientHeight - 2*23; //33;
  if (W < 1) or (H < 1) then Exit;

  with Canvas do
  begin
    Brush.Color := ColorBack;
    Pen.Color := clBlack;
    Pen.Width := 1;

    Rectangle(X, Y, X + W, Y + H);

    { draw pie for current value }
    Brush.Color := ColorFore;
    Pen.Color := clBlack;
    MiddleX := Round(W*SolveForY(FValue - FValueMin, FValueMax - FValueMin));
    Rectangle(X, Y, X + MiddleX, Y + H);

    if FShowLevel then
    begin
      { draw a RED level line }
      Pen.Color := ColorRed;
      MiddleX := Round(W*SolveForY(FValueRed - FValueMin, FValueMax - FValueMin));
      MoveTo(X + MiddleX, Y + 1);
      LineTo(X + MiddleX, Y + H - 1);

      { draw a YELLOW level line }
      Pen.Color := ColorYellow;
      MiddleX := Round(W*SolveForY(FValueYellow - FValueMin, FValueMax - FValueMin));
      MoveTo(X + MiddleX, Y + 1);
      LineTo(X + MiddleX, Y + H - 1);
    end;
  end;
end;

procedure TAnalogSensor.PaintAsVertical;
var MiddleY: Integer;
    X, Y, W, H: Integer;
begin
  X := 20;
  Y := 23;
  W := ClientWidth - 2*20; //130;
  H := ClientHeight - 2*23; //33;
  if (W < 1) or (H < 1) then Exit;

  with Canvas do
  begin
    Brush.Color := ColorBack;
    Pen.Color := clBlack;
    Pen.Width := 1;

    Rectangle(X + W - 1, Y + H - 1, X, Y);

    { draw pie for current value }
    Brush.Color := ColorFore;
    Pen.Color := clBlack;
    MiddleY := Round(H*SolveForY(FValue - FValueMin, FValueMax - FValueMin));
    Rectangle(X, Y + H - 1 - MiddleY, X + W - 1, Y + H - 1);

    if FShowLevel then
    begin
      { draw a RED level line }
      Pen.Color := ColorRed;
      MiddleY := Round(H*SolveForY(FValueRed - FValueMin, FValueMax - FValueMin));
      MoveTo(X + 1, Y + H - 1 - MiddleY);
      LineTo(X + W - 1, Y + H - 1 - MiddleY);

      { draw a YELLOW level line }
      Pen.Color := ColorYellow;
      MiddleY := Round(H*SolveForY(FValueYellow - FValueMin, FValueMax - FValueMin));
      MoveTo(X + 1, Y + H - 1 - MiddleY);
      LineTo(X + W - 1, Y + H - 1 - MiddleY);
    end;
  end;
end;

procedure TAnalogSensor.SetAnalogKind(AValue: TAnalogKind);
begin
  if (AValue <> FAnalogKind) then
  begin
    FAnalogKind := AValue;
    Invalidate;
  end;
end;

{ TStopLightSensor }
constructor TStopLightSensor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 23;
  Height := 43;
  Center := True;
  FState := slRED;
  State := slUNKNOWN;
end;

procedure TStopLightSensor.SetState(AValue: TStopLights);
begin
  if (AValue <> FState) then
  begin
    FState := AValue;

    case AValue of
      slUNKNOWN: Picture.LoadFromResourceName(HInstance, 'STOP_UNKNOWN', TPortableNetworkGraphic);
      slRED: Picture.LoadFromResourceName(HInstance, 'STOP_RED', TPortableNetworkGraphic);
      slYELLOW: Picture.LoadFromResourceName(HInstance, 'STOP_YELLOW', TPortableNetworkGraphic);
      slGREEN: Picture.LoadFromResourceName(HInstance, 'STOP_GREEN', TPortableNetworkGraphic);
    end;
  end;
end;

end.
