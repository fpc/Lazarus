{
 /***************************************************************************
                               typingindicator.pp
                             -------------------
               basic control displaying a 'user is typing' indicator
 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit TypingIndicator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls;

const
  DefaultTypingCycle = 2000;
  DefaultTypingInterval = 100;
  DefaultTypingMinMargin = 4;
  DefaultTypingMaxBulge = 2;
  DefaultBalloonColor = $00E6E7ED;
  DefaultDot1Color = $00AFAFAF;
  DefaultDot2Color = $00A0A0A0;
  DefaultDot3Color = $00B0B0B0;

Type

  { TTypingDotIndicatorSettings }

  TTypingDotIndicatorSettings = class(TPersistent)
  Private
    FBalloonColor: TColor;
    FCycle: Integer;
    FDotRadius: Integer;
    FMaxBulge: Integer;
    FMinMargin: Integer;
    FInterval: Integer;
    FDotColors : Array[0..2] of TColor;
    function GetDotColor(AIndex: Integer): TColor;
    procedure SetBalloonColor(AValue: TColor);
    procedure SetCycle(AValue: Integer);
    procedure SetDotColor(AIndex: Integer; AValue: TColor);
    procedure SetDotRadius(AValue: Integer);
    procedure SetInterval(AValue: Integer);
    procedure SetMaxBulge(AValue: Integer);
    procedure SetMinMargin(AValue: Integer);
  Protected
    Procedure Changed; virtual;
  Public
    procedure Assign(Source: TPersistent); override;
    Constructor Create;
    Property CycleAt : Integer Read FCycle Write SetCycle default DefaultTypingCycle;
    Property Interval : Integer Read FInterval Write SetInterval default DefaultTypingInterval;
    Property DotRadius : Integer Read FDotRadius Write SetDotRadius default 0;
    Property MaxBulge : Integer Read FMaxBulge Write SetMaxBulge default DefaultTypingMaxBulge;
    Property MinMargin : Integer Read FMinMargin Write SetMinMargin default DefaultTypingMinMargin;
    Property Dot1Color : TColor Index 0 Read GetDotColor Write SetDotColor Default DefaultDot1Color;
    Property Dot2Color : TColor Index 1 Read GetDotColor Write SetDotColor  Default DefaultDot2Color;
    Property Dot3Color : TColor Index 2 Read GetDotColor Write SetDotColor  Default DefaultDot2Color;
    Property BalloonColor : TColor Read FBalloonColor Write SetBalloonColor default DefaultBalloonColor;
  end;

  { TTypingIndicator }

  TTypingIndicator = Class(TGraphicControl)
  Private
    FActive: Boolean;
    FSettings: TTypingDotIndicatorSettings;
    FTimer: TTimer;
    FCurrent: Integer;
    FColors : Array[0..2] of TColor;
    FCurrentMargin : Integer;
    function CreateDotSettings: TTypingDotIndicatorSettings;
    procedure DrawDot(Idx: Integer; aPos: TPoint; aRadius: Integer; aColor: TColor);
    function GetBalloonColor: TColor;
    function GetCycle: Integer;
    function GetDotColor(AIndex: Integer): TColor;
    function GetMaxBulge: Integer;
    function GetMinMargin: Integer;
    procedure HandleTimer(Sender: TObject);
    function GetInterval: Integer;
    procedure SetActive(const aValue: Boolean);
    procedure SetSettings(AValue: TTypingDotIndicatorSettings);
  protected
    Type

      { TMyTypingDotIndicatorSettings }

      TMyTypingDotIndicatorSettings = Class(TTypingDotIndicatorSettings)
      Private
        FIndicator : TTypingIndicator;
      Protected
        Procedure Changed; override;
        property Indicator : TTypingIndicator Read FIndicator;
      Public
        Constructor Create(aIndicator : TTypingIndicator);
      end;
    Procedure Loaded; override;
    Procedure DotSettingsChanged;
    procedure Bulge; virtual;
    procedure RotateColors; virtual;
    procedure StepAnimation; virtual;
    procedure StepCounter; virtual;
    function Steps: Integer;
    Property CycleAt : Integer Read GetCycle;
    Property Interval : Integer Read GetInterval;
    Property MaxBulge : Integer Read GetMaxBulge;
    Property MinMargin : Integer Read GetMinMargin;
    Property BalloonColor : TColor Read GetBalloonColor;
    Property Dot1Color : TColor Index 0 Read GetDotColor;
    Property Dot2Color : TColor Index 1 Read GetDotColor;
    Property Dot3Color : TColor Index 2 Read GetDotColor;
  Public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  Published
    Property Active : Boolean Read FActive Write SetActive;
    Property DotSettings : TTypingDotIndicatorSettings Read FSettings Write SetSettings;
  end;


implementation

{ TTypingDotIndicatorSettings }

function TTypingDotIndicatorSettings.GetDotColor(AIndex: Integer): TColor;
begin
  Result:=FDotColors[aIndex];
end;

procedure TTypingDotIndicatorSettings.SetBalloonColor(AValue: TColor);
begin
  if aValue=FBalloonColor then exit;
  FBalloonColor:=aValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.SetCycle(AValue: Integer);
begin
  if FCycle=AValue then Exit;
  FCycle:=AValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.SetDotColor(AIndex: Integer; AValue: TColor);
begin
  if FDotColors[aIndex]=aValue then exit;

end;

procedure TTypingDotIndicatorSettings.SetDotRadius(AValue: Integer);
begin
  if FDotRadius=AValue then Exit;
  FDotRadius:=AValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.SetInterval(AValue: Integer);
begin
  if aValue=FInterval then exit;
  FInterval:=aValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.SetMaxBulge(AValue: Integer);
begin
  if FMaxBulge=AValue then Exit;
  FMaxBulge:=AValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.SetMinMargin(AValue: Integer);
begin
  if FMinMargin=AValue then Exit;
  FMinMargin:=AValue;
  Changed;
end;

procedure TTypingDotIndicatorSettings.Changed;
begin
  // Do nothing
end;

procedure TTypingDotIndicatorSettings.Assign(Source: TPersistent);
var
  aSource: TTypingDotIndicatorSettings absolute Source;
begin
  if Source is TTypingDotIndicatorSettings then
    begin
    FMinMargin:=aSource.FMinMargin;
    FMaxBulge:=aSource.FMaxBulge;
    FInterval:=aSource.FInterval;
    FDotColors:=aSource.FDotColors;
    FCycle:=aSource.FCycle;
    FBalloonColor:=aSource.FBalloonColor;
    FDotRadius:=aSource.DotRadius;
    Changed;
    end
  else
    inherited Assign(Source);
end;

constructor TTypingDotIndicatorSettings.Create;

Const
  DefaultColors : Array[0..2] of TColor = (DefaultDot1Color,DefaultDot2Color,DefaultDot3Color);

var
  I : Integer;

begin
  FDotRadius:=0; // Auto calculate
  FCycle:=DefaultTypingCycle;
  FBalloonColor:=DefaultBalloonColor; // $00DDDDDD;
  FMinMargin:=DefaultTypingMinMargin;
  FMaxBulge:=DefaultTypingMaxBulge;
  FInterval:=DefaultTypingInterval;
  For I:=0 to 2 do
    FDotColors[i]:=DefaultColors[i];
end;

{ TTypingIndicator }

function TTypingIndicator.Steps: Integer;

begin
  result:=CycleAt div InterVal
end;

procedure TTypingIndicator.RotateColors;

var
  I,Offset,Third : Integer;

begin
  Third:=Steps Div 9;
  If Third=0 then
    offset:=0
  else
    Offset:=FCurrent div Third;
  for I:=0 to 2 do
    FColors[I]:=FSettings.GetDotColor((I+Offset) mod 3);
end;

procedure TTypingIndicator.Bulge;

begin
  FCurrentMargin:=MinMargin+MaxBulge+Round(MaxBulge*Sin(2*Pi*(FCurrent/Steps)));
end;

function TTypingIndicator.GetInterval: Integer;
begin
  Result:=FTimer.Interval;
end;

procedure TTypingIndicator.SetActive(const aValue: Boolean);
begin
  if FActive=aValue then Exit;
  FActive:=aValue;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled:=Factive
  else
    FTimer.Enabled:=False;
end;

procedure TTypingIndicator.SetSettings(AValue: TTypingDotIndicatorSettings);
begin
  if FSettings=AValue then Exit;
  FSettings.Assign(AValue);
end;

procedure TTypingIndicator.Loaded;
begin
  inherited Loaded;
  if Factive then
    FTimer.Enabled:=True;
end;

procedure TTypingIndicator.DotSettingsChanged;
begin
  FTimer.Interval:=FSettings.Interval;
  FCurrent:=0;
  Invalidate;
end;

procedure TTypingIndicator.StepCounter;

begin
  Inc(FCurrent);
  if FCurrent=Steps then FCurrent:=0;
end;

procedure TTypingIndicator.StepAnimation;

begin
  StepCounter;
  RotateColors;
  Bulge;
  Invalidate;
end;

procedure TTypingIndicator.HandleTimer(Sender: TObject);
begin
  StepAnimation;
end;

function TTypingIndicator.GetDotColor(AIndex: Integer): TColor;
begin
  Result:=FSettings.GetDotColor(aIndex);
end;

function TTypingIndicator.GetCycle: Integer;
begin
  Result:=FSettings.CycleAt;
end;

function TTypingIndicator.GetMaxBulge: Integer;
begin
  Result:=FSettings.MaxBulge;
end;

function TTypingIndicator.GetMinMargin: Integer;
begin
  Result:=FSettings.MinMargin;
end;

constructor TTypingIndicator.Create(aOwner: TComponent);

var i : Integer;

begin
  Inherited;
  FSettings:=CreateDotSettings;
  FTimer:=TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval:=DefaultTypingInterval;
  FTimer.OnTimer:=@HandleTimer;
  For I:=0 to 2 do
    FColors[i]:=FSettings.GetDotColor(i);
end;

destructor TTypingIndicator.Destroy;
begin
  FreeAndNil(FTimer);
  inherited destroy;
end;

function TTypingIndicator.CreateDotSettings: TTypingDotIndicatorSettings;
begin
  Result:=TMyTypingDotIndicatorSettings.Create(Self);
end;

function TTypingIndicator.GetBalloonColor: TColor;
begin
  Result:=FSettings.BalloonColor;
end;

procedure TTypingIndicator.DrawDot(Idx: Integer; aPos: TPoint; aRadius: Integer; aColor: TColor);
begin
  Canvas.Brush.Color:=aColor;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Color:=aColor;
  Canvas.Pen.Style:=psSolid;
  Canvas.Ellipse(aPos.X-aRadius,aPos.Y-aRadius,aPos.X+aRadius,aPos.Y+aRadius);
end;

procedure TTypingIndicator.Paint;


var
  R : TRect;
  Radius : Integer;
  P : TPoint;
  I,Sep : Integer;
  CR : Integer;

begin
  Sep:=4;
  R:=Rect(0,0,Width,Height);
  R.Inflate(-1,-1);
  Canvas.Brush.Color:=Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.FillRect(R);
  Canvas.Brush.Color:=BalloonColor;
  Canvas.Brush.Style:=bsSolid;
  R.Inflate(-FCurrentMargin,-FCurrentMargin);
  Radius:=(Height-(2*FCurrentMargin)); // div 2;
  Canvas.Pen.Color:=BalloonColor;
  Canvas.Pen.Style:=psSolid;
  Canvas.RoundRect(R,Radius,Radius);
  CR:=Radius div 4;
  Canvas.Ellipse(FCurrentMargin,Height-FCurrentMargin-CR,FCurrentMargin+CR,Height-FCurrentMargin);

  Radius:=DotSettings.DotRadius;
  if Radius=0 then
    Radius:=Height Div 6;

  For I:=0 to 2 do
    begin
    P:=R.CenterPoint;
    Inc(P.X,(I-1)*(2*Radius+Sep));
    DrawDot(I,P,Radius,FColors[i]);
    end;
end;

{ TTypingIndicator.TMyTypingDotIndicatorSettings }

procedure TTypingIndicator.TMyTypingDotIndicatorSettings.Changed;
begin
  inherited Changed;
  If Assigned(FInDicator) then
    FIndicator.DotSettingsChanged;
end;

constructor TTypingIndicator.TMyTypingDotIndicatorSettings.Create(aIndicator: TTypingIndicator);
begin
  Inherited Create;
  FIndicator:=aIndicator;
end;

end.

