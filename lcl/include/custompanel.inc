{%MainUnit ../extctrls.pp}

{******************************************************************************
                                TCustomRadioBox
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{

  Delphi compatibility:

  - TPanel is compatible with Delphi implementation
}

{------------------------------------------------------------------------------
  constructor TCustomPanel.Create (TheOwner : TComponent);
 ------------------------------------------------------------------------------}
constructor TCustomPanel.Create (TheOwner : TComponent);
begin
  inherited Create (TheOwner);
  FCompStyle:= csPanel;
  ControlStyle := ControlStyle + [csAcceptsControls, csCaptureMouse,
    csClickEvents, csSetCaption, csDoubleClicks, csReplicatable,
    csNoFocus]
    - [csOpaque]; // we need the default background
  FBevelOuter := bvRaised;
  FBevelInner := bvNone;
  FBevelWidth := 1;
  FAlignment := taCenter;
  Color:=clBtnface;// clBackground;
  SetInitialBounds(0,0,170,50);
  ParentColor := True;
end;

procedure TCustomPanel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBevelWidth(const Value: TBevelWidth);
begin
  if FBevelWidth <> Value then begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBevelInner(const Value: TPanelBevel);
begin
  if BevelInner <> Value then begin
    FBevelInner := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBevelOuter(const Value: TPanelBevel);
begin
  if BevelOuter <> Value then begin
    FBevelOuter := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.SetBorderWidth(const Value: TBorderWidth);
begin
  if FBorderWidth <> Value then begin
    FBorderWidth := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TCustomPanel.Paint;
var
  ARect: TRect;
  TS : TTextStyle;
begin
  ARect := GetClientRect;
  if BorderStyle = bsSingle then begin
    Canvas.Rectangle(ARect);
    InflateRect(ARect, -1, -1);
  end;

  if BevelOuter <> bvNone then
    Canvas.Frame3d(ARect, BevelWidth, BevelOuter);

  if BevelInner <> bvNone then begin
    if BorderWidth > 0 then InflateRect(ARect, -BorderWidth, -BorderWidth);
    Canvas.Frame3d(ARect, BevelWidth, BevelInner);
  end;

  if Caption <> '' then begin
    TS.Alignment:= Alignment;
    TS.Layout:= tlCenter;
    TS.Opaque:= false;
    TS.Clipping:= false;
    Canvas.TextRect(ARect, 0, 0, Caption, TS);
  end;
end;

function TCustomPanel.ParentColorIsStored: boolean;
begin
  Result:=not ParentColor;
end;

procedure TCustomPanel.AdjustClientRect(var Rect: TRect);
var
  BevelSize: Integer;
begin
  inherited AdjustClientRect(Rect);
  InflateRect(Rect, -BorderWidth, -BorderWidth);
  BevelSize := 0;
  if BevelOuter <> bvNone then Inc(BevelSize, BevelWidth);
  if BevelInner <> bvNone then Inc(BevelSize, BevelWidth);
  InflateRect(Rect, -BevelSize, -BevelSize);
end;

procedure TCustomPanel.SetBorderStyle(const Value: TControlBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

function TCustomPanel.GetText: TCaption;
begin
  Result := FCaption;
end;

procedure TCustomPanel.SetText(const Value: TCaption);
begin
  if Caption <> Value then begin
    FCaption := Value;
//    TextChanged;
    Invalidate;
  end;
end;

// included by extctrls.pp

