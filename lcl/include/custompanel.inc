{%MainUnit ../extctrls.pp}

{******************************************************************************
                                TCustomRadioGroup
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  FFullRepaint := true;
  Color:=clBtnFace;// clBackground;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ParentColor := True;
  UseDockManager := True;
end;

procedure TCustomPanel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomPanel.UpdateBordersAndBevels;
begin
  InvalidateClientRectCache(True);
  DoAdjustClientRectChange;
  Invalidate;
end;

procedure TCustomPanel.SetBevelWidth(const Value: TBevelWidth);
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    UpdateBordersAndBevels;
  end;
end;

procedure TCustomPanel.SetBevelInner(const Value: TPanelBevel);
begin
  if BevelInner <> Value then
  begin
    FBevelInner := Value;
    UpdateBordersAndBevels;
  end;
end;

procedure TCustomPanel.SetBevelOuter(const Value: TPanelBevel);
begin
  if BevelOuter <> Value then
  begin
    FBevelOuter := Value;
    UpdateBordersAndBevels;
  end;
end;

procedure TCustomPanel.SetBorderWidth(const Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    UpdateBordersAndBevels;
  end;
end;

procedure TCustomPanel.Paint;
var
  ARect: TRect;
  TS : TTextStyle;
begin
  ARect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    Canvas.Frame3d(ARect, BevelWidth, BevelOuter);
    InflateRect(ARect, -1, -1);
  end;

  if BevelInner <> bvNone then 
  begin
    if BorderWidth > 0 then InflateRect(ARect, -BorderWidth, -BorderWidth);
    Canvas.Frame3d(ARect, BevelWidth, BevelInner);
    InflateRect(ARect, -1, -1);
  end;
  if Caption <> '' then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment:= Alignment;
    TS.Layout:= tlCenter;
    TS.Opaque:= false;
    TS.Clipping:= false;
    TS.SystemFont:=Canvas.Font.IsDefault;
    if not Enabled then
    begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(ARect, 1, 1);
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(ARect, -1, -1);
    end
    else
      Canvas.Font.Color := Font.Color;

    Canvas.TextRect(ARect,ARect.Left,ARect.Top, Caption, TS);
  end;

  inherited Paint;
end;

procedure TCustomPanel.AdjustClientRect(var Rect: TRect);
var
  BevelSize: Integer;
begin
  inherited AdjustClientRect(Rect);
  BevelSize := BorderWidth;
  if BevelOuter <> bvNone then
    Inc(BevelSize, BevelWidth);
  if BevelInner <> bvNone then
    Inc(BevelSize, BevelWidth);
  InflateRect(Rect, -BevelSize, -BevelSize);
end;

class function TCustomPanel.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=170;
  Result.Y:=50;
end;

procedure TCustomPanel.Loaded;
begin
  inherited Loaded;
  UpdateParentColorChange;
end;

procedure TCustomPanel.UpdateParentColorChange;
begin
  if ParentColor then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

procedure TCustomPanel.CMParentColorChanged(var Message: TLMessage);
begin
  UpdateParentColorChange;
  inherited;
end;

function TCustomPanel.GetDefaultDockCaption: String;
begin
  Result := Caption;
end;

procedure TCustomPanel.RealSetText(const Value: TCaption);
begin
  if Caption <> Value 
  then begin
    inherited RealSetText(Value);
    Invalidate;
  end;
end;

// included by extctrls.pp
