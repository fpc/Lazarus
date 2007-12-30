{%MainUnit ../forms.pp}

{ THintWindow

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
   use:
   HintWindow := THintWindow.Create(nil);
   Rect := HintWindow.CalcHintRect(0,'This is the hint',nil);
   HintWindow.ActivateHint(Rect,'This is the hint');

}

const HintBorderWidth=2;

constructor THintWindow.Create(AOwner: TComponent);
var
  TheTimer: TCustomTimer;
begin
  inherited Create(AOwner);
  fCompStyle := csHintWindow;
  Parent := nil;
  Color := clInfoBk;
  Canvas.Font := Screen.HintFont;
  BorderStyle := bsNone;
  Caption := 'THintWindow';
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  FHideInterval := 3000;
  TheTimer := TCustomTimer.Create(self);
  FAutoHideTimer := TheTimer;
  TheTimer.Interval := HideInterval;
  TheTimer.Enabled := False;
  TheTimer.OnTimer := @AutoHideHint;
end;

destructor THintWindow.Destroy;
begin
  fAutoHideTimer.Free;
  fAutoHideTimer:=nil;
  inherited Destroy;
end;

Procedure THintWindow.SetHideInterval(Value : Integer);
Begin
  FHideInterval := Value;
  if FAutoHideTimer<>nil then
    TCustomTimer(FAutoHideTimer).Interval := FHideInterval;
end;

Procedure THintWindow.SetAutoHide(Value : Boolean);
Begin
  FAutoHide := Value;
  if not (value) and (FAutoHideTimer<>nil) then
    TCustomTimer(FAutoHideTimer).Enabled := False;
end;


Procedure THintWindow.AutoHideHint(Sender : TObject);
Begin
  if FAutoHideTimer<>nil then
    TCustomTimer(FAutoHideTimer).Enabled := False;
  if Visible then
    Visible := False;//Hide;
End;

procedure THintWindow.Paint;
var
  ARect: TRect;
begin
  ARect := ClientRect;
  Canvas.Brush.Color := Color;
  //DebugLn(['THintWindow.Paint Canvas.Brush.Color=',dbgs(Canvas.Brush.Color)]);
  Canvas.Pen.Width := 1;
  Canvas.FillRect(ARect);
  DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
  InflateRect(ARect,-2*HintBorderWidth,-2*HintBorderWidth);
  DrawText(Canvas.GetUpdatedHandle([csFontValid]),
           PChar(Caption), Length(Caption), ARect,
           DT_NOPREFIX or DT_CENTER or DT_VCENTER or DT_WORDBREAK);
end;

class function THintWindow.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=25;
  Result.Y:=25;
end;

procedure THintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  ActivateHintData(ARect, AHint, nil);
end;

procedure THintWindow.ActivateHintData(ARect: TRect; const AHint: String;
  AData: pointer);
var
  InvalidateNeeded: Boolean;
begin
  if FActivating then exit;
  FActivating := True;
  try
    //debugln('THintWindow.ActivateHint OldHint="',DbgStr(Caption),'" NewHint="',DbgStr(AHint),'"');
    InvalidateNeeded:=Visible and (Caption<>AHint);
    Caption := AHint;
    if ARect.Bottom > Screen.Height then
    begin
      ARect.Top := Screen.Height - (ARect.Bottom - ARect.Top);
      ARect.Bottom := Screen.Height;
    end;
    if ARect.Right > Screen.Width then
    begin
      ARect.Left := Screen.Width - (ARect.Right - ARect.Left);
      ARect.Right := Screen.Width;
    end;
    if ARect.Left < 0 then ARect.Left := 0;
    if ARect.Top < 0 then ARect.Top := 0;
    SetBounds(ARect.Left, ARect.Top,
              ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
    Visible := True;
    TCustomTimer(FAutoHideTimer).Enabled := False;
    TCustomTimer(FAutoHideTimer).Enabled := FAutoHide;
    if InvalidateNeeded then Invalidate;
  finally
    FActivating := False;
  end;
end;

function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: Pointer): TRect;
begin
  if MaxWidth<=0 then
    MaxWidth:=Screen.Width-4*HintBorderWidth;
  Result := Rect(0,0, MaxWidth, Screen.Height-4*HintBorderWidth);
  if AHint='' then exit;
  DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(AHint), Length(AHint),
           Result, DT_CalcRect or DT_NOPREFIX or DT_WORDBREAK);
  inc(Result.Right,4*HintBorderWidth);
  inc(Result.Bottom,4*HintBorderWidth);
  //debugln('THintWindow.CalcHintRect Result=',dbgs(Result));
end;

procedure THintWindow.ReleaseHandle;
begin
  DestroyHandle;
end;

// included by forms.pp
