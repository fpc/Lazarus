// included by forms.pp

{ THintWindow

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

   use:
   HintWindow := THintWindow.Create(nil);
   Rect := HintWindow.CalcHintRect(0,'This is the hint',nil);
   HintWindow.ActivateHint(Rect,'This is the hint');
   
}

constructor THintWindow.Create(AOwner: TComponent);
var
  TheTimer: TTimer;
begin
  inherited Create(AOwner);
  fCompStyle := csHintWindow;
  parent := nil;
  color := clInfoBk;
  Canvas.Font := Screen.HintFont;
  Caption := 'THintWindow';
  SetBounds(1,1,25,25);
  FHideInterval := 3000;
  TheTimer := TTimer.Create(self);
  FAutoHideTimer := TheTimer;
  TheTimer.Interval := HideInterval;
  TheTimer.Enabled := False;
  TheTimer.OnTimer := @AutoHideHint;
end;

destructor THintWIndow.Destroy;
begin
  fAutoHideTimer.Free;
  fAutoHideTimer:=nil;
  inherited;
end;

Procedure THintWindow.SetHideInterval(Value : Integer);
Begin
  FHideInterval := Value;
  if FAutoHideTimer<>nil then
    TTimer(FAutoHideTimer).Interval := FHideInterval;
end;

Procedure THintWindow.SetAutoHide(Value : Boolean);
Begin
  FAutoHide := Value;
  if not (value) and (FAutoHideTimer<>nil) then
    TTimer(FAutoHideTimer).Enabled := False;
end;


Procedure THintWindow.AutoHideHint(Sender : TObject);
Begin
  if FAutoHideTimer<>nil then
    TTimer(FAutoHideTimer).Enabled := False;
  if Visible then
    Visible := False;//Hide;
End;

procedure THintWindow.Paint;
var
  Rect: TRect;
  TS : TTextStyle;
  //DefaultDraw: Boolean;
begin
  Rect := ClientRect;
  Dec(Rect.Right, 1);
  Dec(Rect.Bottom, 1);
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle(Rect);
  FillChar(TS, SizeOf(TS),0);
  With TS do
    Clipping   := True;
  InflateRect(Rect, -1, -1);
  Canvas.TextRect(Rect, 1, 1, Caption, TS);
end;

procedure THintWindow.ActivateHint(ARect: TRect; const AHint: String);
begin
  if FActivating then exit;
  FActivating := True;
  try
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
    TTimer(FAutoHideTimer).Enabled := False;
    TTimer(FAutoHideTimer).Enabled := FAutoHide;
  finally
    FActivating := False;
  end;
end;


function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: Pointer): TRect;
begin
  Result := Rect(0,0, MaxWidth, ClientHeight);
  SelectObject(Canvas.Handle, Canvas.Font.Handle);
  DrawText(Canvas.Handle, PChar(AHint), Length(AHint), Result, DT_CalcRect or
    DT_NOPREFIX);
  InflateRect(Result, 2, 2);
  Inc(Result.Right, 3);
end;


