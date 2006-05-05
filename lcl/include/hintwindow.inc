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
  SetInitialBounds(1,1,25,25);
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
  TS : TTextStyle;
begin
  ARect := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Width := 1;
  //debugln('THintWindow.Paint A ',dbgs(ARect));
  DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
  InflateRect(ARect,-2*HintBorderWidth,-2*HintBorderWidth);
  //debugln('THintWindow.Paint B ',dbgs(ARect));
  with TS do
  begin
    Alignment  := taCenter;
    Layout     := tlCenter;
    SingleLine := false;
    Clipping   := true;
    ExpandTabs := true;
    ShowPrefix := false;
    WordBreak  := true;
    Opaque     := false;
    SystemFont := false;
  end;
  Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
end;

procedure THintWindow.ActivateHint(ARect: TRect; const AHint: String);
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
  Result := Rect(0,0, MaxWidth, ClientHeight);
  if AHint='' then exit;
  DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(AHint), Length(AHint),
           Result, DT_CalcRect or DT_NOPREFIX);
  inc(Result.Right,4*HintBorderWidth);
  inc(Result.Bottom,4*HintBorderWidth);
  //debugln('THintWindow.CalcHintRect Result=',dbgs(Result));
end;

procedure THintWindow.ReleaseHandle;
begin
  DestroyHandle;
end;

// included by forms.pp
