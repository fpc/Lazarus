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
var TheTimer: TTimer;
begin
  inherited Create(AOwner);
  fCompStyle := csHintWindow;
  parent := nil;
  Canvas.Font := Screen.HintFont;
  color := clInfoBk;
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
  inherited;
end;

Procedure THintWindow.SetHideInterval(Value : Integer);
Begin
  FHideInterval := Value;
  TTimer(FAutoHideTimer).Interval := FHideInterval;
end;

Procedure THintWindow.SetAutoHide(Value : Boolean);
Begin
  FAutoHide := Value;
  if not(value) then
     TTimer(FAutoHideTimer).Enabled := False;
end;


Procedure THintWindow.AutoHideHint(Sender : TObject);
Begin
  TTimer(FAutoHideTimer).Enabled := False;
  if Visible then Visible := False;//Hide;
End;


procedure THintWindow.Paint;
var
  Rect: TRect;
  //DefaultDraw: Boolean;
begin
  Rect := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle(Rect);
  Canvas.TextRect(Rect, 3, 3, Caption);
end;

procedure THintWindow.ActivateHint(Rect: TRect; const AHint: String);
begin
  FActivating := True;
  try
    Caption := AHint;
    if Rect.Bottom > Screen.Height then
    begin
      Rect.Top := Screen.Height - (Rect.Bottom - Rect.Top);
      Rect.Bottom := Screen.Height;
    end;
    if Rect.Right > Screen.Width then
    begin
      Rect.Left := Screen.Width - (Rect.Right - Rect.Left);
      Rect.Right := Screen.Width;
    end;
    if Rect.Left < 0 then Rect.Left := 0;
    if Rect.Bottom < 0 then Rect.Bottom := 0;
    SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
    Visible := True;
    TTimer(FAutoHideTimer).Enabled := False;
    TTimer(FAutoHideTimer).Enabled := FAutoHide;
  finally
    FActivating := False;
  end;
end;


function THintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String;
  AData: Pointer): TRect;
var
  //Temp : Integer;
  Num : Integer;
  tempHint : String;
  LongestLine : String;
  Lines    : Integer;
begin
  Result.Left := 0;
  Result.Top := 0;

  TempHint := AHint;
  LongestLine := '';
  num := pos(#10,TempHint);
  Lines := 1;
  if Num > 0 then
     Begin
       //set TempHint to the longest line.
       //set Lines to the number of lines.
       while num > 0 do
          Begin
             inc(Lines);
             if Canvas.TextWidth(copy(TempHint,1,num-1)) > Canvas.TextWidth(LongestLine) then
                LongestLine := Copy(TempHint,1,num-1);
             delete(TempHint,1,num);
             Num := pos(#10,TempHint);
          end;
     end;
  if Canvas.TextWidth(copy(TempHint,1,Length(TempHint))) > Canvas.TextWidth(LongestLine) then
     LongestLine := Copy(TempHint,1,Length(TempHint));
     
  TempHint := LongestLine;
  if ((MaxWidth > 0) and (Canvas.TextWidth(TempHint) > MaxWidth)) then
      Result.Right := Result.Left + MaxWidth
      else
      Result.Right := Result.Left + Canvas.TextWidth(TempHint);
  Result.Bottom := result.Top + (Lines * (Canvas.TextHeight(AHint)));
  Inc(Result.Bottom, 4);
  Dec(Result.Top, 2);
  Inc(Result.Right, 8);
end;


