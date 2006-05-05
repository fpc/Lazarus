{%MainUnit ../dialogs.pp}

{******************************************************************************
                                   TColorButton
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

{ TColorButton }

constructor TColorButton.Create(AnOwner: TComponent);
begin
  Inherited Create(AnOwner);
  FBorderWidth:=2;
  SetInitialBounds(1,1,75,25);
end;

destructor TColorButton.Destroy;
Begin
  inherited Destroy;
end;

procedure TColorButton.Paint;
var
  ARect: TRect;
begin
  ARect:=Bounds(0, 0, Width, Height);
  with Canvas do begin
    Frame3d(ARect,FBorderWidth,bvRaised);
    //debugln('TColorButton.Paint A ',dbgs(ARect));
    InflateRect(ARect,-FBorderWidth-2,-FBorderWidth-2);
    //debugln('TColorButton.Paint B ',dbgs(ARect));
    Brush.Color:=clBlack;
    FillRect(ARect);
    InflateRect(ARect,-1,-1);
    Brush.Color:=ButtonColor;
    FillRect(ARect);
  end;
  inherited Paint;
end;

procedure TColorButton.SetButtonColor(Value:TColor);
begin
  if Value=FButtonColor then exit;
  FButtonColor:=Value;
  if Assigned(FOnColorChanged) and (not (csLoading in ComponentState)) then
    FOnColorChanged(Self);
  Invalidate;
end;

procedure TColorButton.ShowColorDialog;
var NewColor: TColor;
begin
  if FColorDialog<>nil then exit;
  if not Enabled then exit;
  NewColor:=ButtonColor;
  FColorDialog:=TColorDialog.Create(Application);
  try
    FColorDialog.Color:=ButtonColor;
    if FColorDialog.Execute then
      NewColor:=FColorDialog.Color;
  finally
    FColorDialog.Free;
    FColorDialog:=nil;
  end;
  ButtonColor:=NewColor;
end;

procedure TColorButton.SetBorderWidth(const AValue: integer);
begin
  if FBorderWidth=AValue then exit;
  FBorderWidth:=AValue;
  Invalidate;
end;

procedure TColorButton.Click;
begin
  inherited Click;
  ShowColorDialog;
end;

// included by buttons.pp
