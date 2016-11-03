{   Component(s):
    TindLed  ---> old cindy name tcyled

    Description:
    A simple led with Group feature
    depending on the state: ON/OFF/DISABLE


    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * No contributors for now ...
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****
    
Modified by Jurassic Pork 2013 for package Industrial of Lazarus}

unit IndLed;

{$mode objfpc}{$H+}

interface

uses Classes, Types, Controls, Graphics, indcyBaseLed, indcyTypes, indcyClasses, indcyGraphics;

type
  TShapeType = (stRectangle, stRoundRect, stEllipse);

  TcyCustomLed = class(TcyBaseLed)
  private
    FLedColorOn: TColor;
    FLedColorOff: TColor;
    FLedColorDisabled: TColor;
    FShapeRoundRectX: Integer;
    FShapeRoundRectY: Integer;
    FShapeLedColorOn: TColor;
    FShapeLedColorOff: TColor;
    FShapeLedColorDisabled: TColor;
    FBevels: TcyBevels;
    FShapeType: TShapeType;
    FShapePenWidth: Word;
    FTransparent: boolean;
    procedure SetShapeLedColorOn(Value: TColor);
    procedure SetShapePenWidth(Value: Word);
    procedure SetShapeType(Value: TShapeType);
    procedure SetShapeRoundRectX(Value: Integer);
    procedure SetShapeRoundRectY(Value: Integer);
    procedure SetBevels(const Value: TcyBevels);
    procedure SetLedColorDisabled(Value: TColor);
    procedure SetLedColorOff(Value: TColor);
    procedure SetLedColorOn(Value: TColor);
    procedure SetTransparent(const Value: boolean);
    procedure SetShapeLedColorDisabled(const Value: TColor);
    procedure SetShapeLedColorOff(const Value: TColor);
  protected
    procedure Paint; override;
    function TransparentColorAtPos(Point: TPoint): boolean; override;
    property Transparent: boolean read FTransparent write SetTransparent default false;
    property LedColorOn: TColor read FLedColorOn write SetLedColorOn;
    property LedColorOff: TColor read FLedColorOff write SetLedColorOff;
    property LedColorDisabled: TColor read FLedColorDisabled write SetLedColorDisabled;
    property ShapeLedColorOn: TColor read FShapeLedColorOn write SetShapeLedColorOn;
    property ShapeLedColorOff: TColor read FShapeLedColorOff write SetShapeLedColorOff;
    property ShapeLedColorDisabled: TColor read FShapeLedColorDisabled write SetShapeLedColorDisabled;
    property ShapePenWidth: Word read FShapePenWidth write SetShapePenWidth default 1;
    property ShapeType: TShapeType read FShapeType write SetShapeType default stRectangle;
    property ShapeRoundRectX: Integer read FShapeRoundRectX write SetShapeRoundRectX default 10;
    property ShapeRoundRectY: Integer read FShapeRoundRectY write SetShapeRoundRectY default 10;
    property Bevels: TcyBevels read FBevels write SetBevels;
    property Height default 25;
    property Width default 25;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TindLed = class(TcyCustomLed)
  private
  protected
  public
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ShowHint;
    // Herited from TcyBaseLed :
    property AllowAllOff;
    property GroupIndex;
    property LedValue;
    property ReadOnly;
    // Herited from TcyCustomLed :
    property Bevels;
    property LedColorOn;
    property LedColorOff;
    property LedColorDisabled;
    property ShapeLedColorOn;
    property ShapeLedColorOff;
    property ShapeLedColorDisabled;
    property ShapePenWidth;
    property ShapeType;
    property ShapeRoundRectX;
    property ShapeRoundRectY;
    property Transparent;
  end;


implementation

constructor TcyCustomLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevels := TcyBevels.Create(self, TcyBevel);

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState
  then
    if Owner <> nil
    then
      if not (csLoading in Owner.ComponentState)  // we have just added the component at design time
      then begin
        with FBevels.Add do    // Frame
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
        end;

        with FBevels.Add do    // Inner 3D frame
          Width := 3;

        with FBevels.Add do    // Contrast Frame
          Style := bcLowered;

        with FBevels.Add do    // Border between Bevels and Shape
        begin
          HighlightColor := clBlack;
          ShadowColor := clBlack;
          Width := 1;
        end;
      end;

  FTransparent := false;
  FShapeType := stRectangle;
  FShapePenWidth:= 1;
  FShapeRoundRectX := 10;
  FShapeRoundRectY := 10;
  FShapeLedColorOn := clGreen;
  FShapeLedColorOff := $00004000;  // Dark green
  FShapeLedColorDisabled := $00003468;  // Dark maroon
  FLedColorOn:= clLime;
  FLedColorOff:= clGreen;
  FLedColorDisabled:= $000059B3;  // Maroon
  Height := 25;
  Width := 25;
end;

destructor TcyCustomLed.Destroy;
begin
  FBevels.Free;
  FBevels := Nil;
  inherited Destroy;
end;

procedure TcyCustomLed.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  // Draw background :
  if not FTransparent
  then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
  end;

  Bevels.DrawBevels(Canvas, Rect, false);

  case ledStatus of
    lsOn:       Canvas.Brush.Color := FLedColorOn;
    lsOff:      Canvas.Brush.Color := FLedColorOff;
    lsDisabled: Canvas.Brush.Color := FLedColorDisabled;
  end;

  if FShapePenWidth > 0
  then begin
    Rect := classes.Rect(Rect.Left + FShapePenWidth div 2,
                         Rect.Top + FShapePenWidth div 2,
                         Rect.Right - (FShapePenWidth-1) div 2,
                         Rect.Bottom - (FShapePenWidth-1) div 2);

    case ledStatus of
      lsOn:       Canvas.Pen.Color := FShapeLedColorOn;
      lsOff:      Canvas.Pen.Color := FShapeLedColorOff;
      lsDisabled: Canvas.Pen.Color := FShapeLedColorDisabled;
    end;

    Canvas.Pen.Width := FShapePenWidth;
  end
  else begin
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Pen.Width := 1;
  end;

  case FShapeType of
    stRectangle: canvas.Rectangle(Rect);
    stRoundRect: canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, ShapeRoundRectX, ShapeRoundRectY);
    stEllipse  : canvas.Ellipse(Rect);
  end;
end;

function TcyCustomLed.TransparentColorAtPos(Point: TPoint): boolean;
begin
  RESULT := false;

  if FTransparent and (Bevels.Count = 0) and (FShapeType = stEllipse)
  then RESULT := not PointInEllipse(Point, ClientRect);
end; 

procedure TcyCustomLed.SetTransparent(const Value: boolean);
begin
  if value <> FTransparent
  then begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeLedColorOn(Value: TColor);
begin
  if value <> FShapeLedColorOn
  then begin
    FShapeLedColorOn := Value;

    if GetLedStatus = lsOn
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeLedColorOff(const Value: TColor);
begin
  if value <> FShapeLedColorOff
  then begin
    FShapeLedColorOff := Value;

    if GetLedStatus = lsOff
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeLedColorDisabled(const Value: TColor);
begin
  if value <> FShapeLedColorDisabled
  then begin
    FShapeLedColorDisabled := Value;

    if GetLedStatus = lsDisabled
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapePenWidth(Value: Word);
begin
  if value <> FShapePenWidth
  then begin
    FShapePenWidth := Value;
    Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeRoundRectX(Value: Integer);
begin
  if Value <> FShapeRoundRectX
  then begin
    FShapeRoundRectX := value;

    if FShapeType = stRoundRect
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeRoundRectY(Value: Integer);
begin
  if Value <> FShapeRoundRectY
  then begin
    FShapeRoundRectY := value;

    if FShapeType = stRoundRect
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetShapeType(Value: TShapeType);
begin
  if value <> FShapeType
  then begin
    FShapeType := Value;
    Invalidate;
  end;
end;

procedure TcyCustomLed.SetLedColorOn(Value: TColor);
begin
  if value <> FLedColorOn
  then begin
    FLedColorOn := Value;

    if GetLedStatus = lsOn
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetLedColorOff(Value: TColor);
begin
  if value <> FLedColorOff
  then begin
    FLedColorOff := Value;

    if GetLedStatus = lsOff
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetLedColorDisabled(Value: TColor);
begin
  if value <> FLedColorDisabled
  then begin
    FLedColorDisabled := Value;

    if GetLedStatus = lsDisabled
    then Invalidate;
  end;
end;

procedure TcyCustomLed.SetBevels(const Value: TcyBevels);
begin
  FBevels := Value;
end;

end.
