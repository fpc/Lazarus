{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomLabel
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

  TODO:

    - Enable Tabbing/Focusing to focus FocusControl
    - Enable Escaped '&' Shortcut to focus FocusControl
    - Compare/Match AutoSize to Delphi/Kylix's
    - ?? Check For Full Delphi/Kylix Compatibility

}

procedure TCustomLabel.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
// assumes: (Parent <> nil) and Parent.HandleAllocated
var
  R : TRect;
  DC : hDC;
  Flags: Cardinal;
  OldFont: HGDIOBJ;
  LabelText: string;
begin
  if (Parent=nil) or (not Parent.HandleAllocated) then exit;
  DC := GetDC(Parent.Handle);
  try
    R := Rect(0,0, Width, Height);
    OldFont:=SelectObject(DC, Font.Handle);
    Flags:=DT_CalcRect;
    if WordWrap then
      inc(Flags, DT_WordBreak)
    else
    if not HasMultiLine then
      inc(Flags, DT_SingleLine);

    LabelText:=GetLabelText;
    DrawText(DC, PChar(LabelText), Length(LabelText), R, Flags);
    SelectObject(DC, OldFont);
    // add one to be able to display disabled label
    PreferredWidth := R.Right - R.Left + 1;
    PreferredHeight := R.Bottom - R.Top + 1;
  finally
    ReleaseDC(Parent.Handle, DC);
  end;
end;

procedure TCustomLabel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  AdjustSize;
end;

class function TCustomLabel.GetControlClassDefaultSize: TPoint;
begin
  Result.X := 65;
  Result.Y := 17;
end;

function TCustomLabel.HasMultiLine: boolean;
var
  s: String;
begin
  s := GetLabelText;
  result := (pos(#10, s) > 0) or (pos(#13, s) > 0);
end;

procedure TCustomLabel.DoAutoSize;
var
  NewWidth, NewHeight: integer;
  CurAnchors: TAnchors;
begin
  //debugln('TCustomLabel.DoAutoSize ',DbgSName(Self),' AutoSizing=',dbgs(AutoSizing),' AutoSize=',dbgs(AutoSize),' Parent=',DbgSName(Parent),' csLoading=',dbgs(csLoading in ComponentState),' Parnet.HandleAllocated=',dbgs((Parent<>nil) and (Parent.HandleAllocated)));
  if OptimalFill and (not AutoSize) then
  begin
    AdjustFontForOptimalFill;
    exit;
  end;
  
  if AutoSizeDelayed then
    exit;
  BeginAutoSizing;
  try
    GetPreferredSize(NewWidth, NewHeight);
    //debugln('TCustomLabel.DoAutoSize ',dbgsName(Self),' Nice ',dbgs(Left),',',dbgs(Top),',w=',dbgs(NewWidth),',h=',dbgs(NewHeight),' Caption="',dbgstr(Caption),'"');
    CurAnchors:=[];
    if Align in [alLeft,alRight,alBottom,alTop,alClient] then
      CurAnchors:=AnchorAlign[Align];
    CurAnchors:=Anchors+CurAnchors;
    if CurAnchors*[akLeft,akRight]=[akLeft,akRight] then
      NewWidth:=Width;
    if CurAnchors*[akTop,akBottom]=[akTop,akBottom] then
      NewHeight:=Height;

    //debugln('TCustomLabel.DoAutoSize ',dbgsName(Self),' Anchored ',dbgs(Left),',',dbgs(Top),',w=',dbgs(NewWidth),',h=',dbgs(NewHeight));
    SetBoundsKeepBase(Left, Top, NewWidth, NewHeight);
  finally
    EndAutoSizing;
  end;
end;

procedure TCustomLabel.SetAlignment(Value : TAlignment);
begin
  //debugln('TCustomLabel.SetAlignment Old=',dbgs(ord(Alignment)),' New=',dbgs(ord(Value)),' csLoading=',dbgs(csLoading in ComponentState));
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomLabel.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FFocusControl) and (Operation = opRemove) then
    FFocusControl:= nil;
end;

procedure TCustomLabel.SetFocusControl(Value : TWinControl);
begin
  if Value <> FFocusControl then
  begin
    FFocusControl:= Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TCustomLabel.WMActivate(var Message: TLMActivate);
begin
  if (FFocusControl <> nil) and (FFocusControl.CanFocus) then
    FFocusControl.SetFocus;
end;

Function TCustomLabel.GetAlignment : TAlignment;
begin
  Result := FAlignment;
end;

function TCustomLabel.GetLabelText: string;
begin
  Result := Caption;
end;

Procedure TCustomLabel.SetShowAccelChar(Value : Boolean);
begin
  If FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TCustomLabel.TextChanged;
begin
  Invalidate;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomLabel.Resize;
begin
  inherited Resize;
  if OptimalFill and (not AutoSize) then
    AdjustFontForOptimalFill;
end;

Function TCustomLabel.GetShowAccelChar : Boolean;
begin
  Result := FShowAccelChar;
end;

function TCustomLabel.CanTab: boolean;
begin
  Result:=false;
end;

procedure TCustomLabel.DoMeasureTextPosition(var TextTop: integer;
  var TextLeft: integer);
var
  lTextHeight: integer;
  lTextWidth: integer;
begin
  TextLeft := 0;
  if Layout = tlTop then
  begin
    TextTop := 0;
  end else
  begin
    GetPreferredSize(lTextWidth, lTextHeight);
    case Layout of
      tlCenter: TextTop := (Height - lTextHeight) div 2;
      tlBottom: TextTop := Height - lTextHeight;
    end;
  end;
end;

constructor TCustomLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := [csSetCaption, csClickEvents, csDoubleClicks, csReplicatable];
  setInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  FShowAccelChar := True;
  Color := clNone;
  AutoSize:=true;
end;

function TCustomLabel.GetTransparent: boolean;
begin
  Result := Color = clNone;
end;

procedure TCustomLabel.SetColor(NewColor: TColor);
begin
  inherited;

  // if color = clnone then transparent, so not opaque
  if NewColor = clNone then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetLayout
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout:= Value;
    Invalidate;
  end;
end;

procedure TCustomLabel.SetTransparent(NewTransparent: boolean);
begin
  if Transparent = NewTransparent then
    exit;
  if NewTransparent then
    Color := clNone
  else
  if Color = clNone then
    Color := clBackground;
end;

{------------------------------------------------------------------------------
  Method: TCustomLabel.SetWordWrap
  Params: None
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomLabel.SetWordWrap(Value: Boolean);
begin
  if fWordWrap <> value then begin
    fWordWrap:= value;
    Invalidate;
  end;
end;

function TCustomLabel.DialogChar(var Message: TLMKey): boolean;
begin
  Result := false;
  if not FShowAccelChar then exit;
  if FFocusControl = nil then exit;

  if IsAccel(Message.CharCode, GetLabelText) and FFocusControl.CanFocus then
  begin
    Result := true;
    FFocusControl.SetFocus;
  end else
    Result:=inherited DialogChar(Message);
end;

procedure TCustomLabel.Loaded;
begin
  inherited Loaded;
  AdjustSize;
end;

{-------------------------------------------------------------------------------
  function TCustomLabel.CalcFittingFontHeight(const TheText: string;
    MaxWidth, MaxHeight: Integer;
    var FontSize, NeededWidth, NeededHeight: integer): Boolean;

  Calculates the maximum font size for TheText to fit into MaxWidth and
  MaxHeight.
-------------------------------------------------------------------------------}
function TCustomLabel.CalcFittingFontHeight(const TheText: string;
  MaxWidth, MaxHeight: Integer;
  var FontHeight, NeededWidth, NeededHeight: integer): Boolean;
var
  R : TRect;
  DC : hDC;
  Flags: Cardinal;
  OldFont: HGDIOBJ;
  MinFontHeight: Integer;
  MaxFontHeight: Integer;
  AFont: TFont;
  CurFontHeight: LongInt;
begin
  Result:=false;
  if AutoSizeDelayed or (TheText='') or (MaxWidth<1) or (MaxHeight<1) then exit;
  AFont:=TFont.Create;
  AFont.Assign(Font);
  CurFontHeight:=AFont.Height;
  MinFontHeight:=5;
  MaxFontHeight:=MaxHeight*2;
  if (CurFontHeight<MinFontHeight) or (CurFontHeight>MaxFontHeight) then
    CurFontHeight:=(MinFontHeight+MaxFontHeight) div 2;
  Flags:=DT_CalcRect or DT_NoPrefix;
  if WordWrap then inc(Flags,DT_WordBreak);
  
  // give a clipping rectangle with space, so that the bounds returned by
  // DrawText can be bigger and we know, the tried font is too big.
  R := Rect(0,0, MaxWidth, MaxHeight*2);
  
  DC := GetDC(Parent.Handle);
  try
    while (MinFontHeight<=MaxFontHeight) and (CurFontHeight>=MinFontHeight)
    and (CurFontHeight<=MaxFontHeight) do begin
      AFont.Height:=CurFontHeight; // NOTE: some TFont do not allow any integer
      //debugln('TCustomLabel.CalcFittingFontHeight A ',dbgs(MinFontHeight),'<=',dbgs(AFont.Height),'<=',dbgs(MaxFontHeight));
      OldFont:=SelectObject(DC, AFont.Handle);
      DrawText(DC, PChar(TheText), Length(TheText), R, Flags);
      SelectObject(DC, OldFont);
      NeededWidth := R.Right - R.Left;
      NeededHeight := R.Bottom - R.Top;
      //debugln('TCustomLabel.CalcFittingFontHeight B NeededWidth=',dbgs(NeededWidth),' NeededHeight=',dbgs(NeededHeight),' MaxWidth=',dbgs(MaxWidth),' MaxHeight=',dbgs(MaxHeight));
      if (NeededWidth>0) and (NeededHeight>0)
      and (NeededWidth<=MaxWidth) and (NeededHeight<=MaxHeight) then begin
        // TheText fits into the bounds
        if (not Result) or (FontHeight<AFont.Height) then
          FontHeight:=AFont.Height;
        Result:=true;
        MinFontHeight:=CurFontHeight;
        // -> try bigger (binary search)
        CurFontHeight:=(MaxFontHeight+CurFontHeight+1) div 2; // +1 to round up
        if CurFontHeight=MinFontHeight then break;
      end else begin
        // TheText does not fit into the bounds
        MaxFontHeight:=CurFontHeight-1;
        // -> try smaller (binary search)
        CurFontHeight:=(MinFontHeight+CurFontHeight) div 2;
      end;
    end
  finally
    ReleaseDC(Parent.Handle, DC);
    AFont.Free;
  end;
end;

function TCustomLabel.ColorIsStored: boolean;
begin
  Result:=(Color<>clNone);
  if Result and ParentColor and (Parent<>nil) then
    Result:=false;
end;

{-------------------------------------------------------------------------------
  function TCustomLabel.AdjustFontForOptimalFill: Boolean;
  
  Maximizes Font.Height
  Return true if Font.Height changed.
-------------------------------------------------------------------------------}
function TCustomLabel.AdjustFontForOptimalFill: Boolean;
var
  NeededWidth: Integer;
  NeededHeight: Integer;
  NewFontHeight: Integer;
  OldFontHeight: LongInt;
begin
  Result:=false;
  if not CalcFittingFontHeight(GetLabelText,Width,Height,NeededWidth,NeededHeight,
                               NewFontHeight) then exit;
  if Font.Height=NewFontHeight then exit;
  //debugln('TCustomLabel.AdjustFontForOptimalFill OldFontHeight=',dbgs(Font.Height),' NewFontHeight=',dbgs(NewFontHeight));
  OldFontHeight:=Font.Height;
  Font.Height:=NewFontHeight;
  Result:=OldFontHeight<>Font.Height;
end;

procedure TCustomLabel.Paint;
var
  TR : TTextStyle;
  R : TRect;
  TextLeft, TextTop: integer;
  LabelText: string;
begin
  R := Rect(0,0,Width,Height);
  with Canvas do
  begin
    if Enabled then
      Brush.Color := Self.Color
    else
      Brush.Color := clNone;
    Font := Self.Font;
    if (Color<>clNone) and not Transparent then
    begin
      Brush.Style:=bsSolid;
      FillRect(R);
    end else
      Brush.Style:=bsClear;
{
    If BorderStyle <> sbsNone then begin
      InflateRect(R,-2,-2);
      Pen.Style := psSolid;
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clBtnHighlight;
      MoveTo(0, 0);
      LineTo(Width - 1,0);
      MoveTo(0, 0);
      LineTo(0,Height - 1);
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnHighlight
      else
        Pen.Color := clBtnShadow;
      MoveTo(0,Height - 1);
      LineTo(Width - 1,Height - 1);
      MoveTo(Width - 1, 0);
      LineTo(Width - 1,Height);
    end;
}
    //Brush.Color:=clRed;
    //FillRect(R);
    FillChar(TR,SizeOf(TR),0);
    with TR do
    begin
      Alignment := Self.Alignment;
      WordBreak := wordWrap;
      SingleLine:= not WordWrap and not HasMultiLine;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
      SystemFont:=false;
      RightToLeft:=UseRightToLeftReading;
    end;
    DoMeasureTextPosition(TextTop, TextLeft);
    //debugln('TCustomLabel.Paint ',dbgs(Alignment=tacenter),' ',dbgs(Layout=tlCenter),' ',dbgs(TextLeft),' TextTop=',dbgs(TextTop),' ',dbgs(R));
    LabelText := GetLabelText;
    if not Enabled then
    begin
      Font.Color := clBtnHighlight;
      TextRect(R, TextLeft + 1, TextTop + 1, LabelText, TR);
      Font.color := clBtnShadow;
    end;
    TextRect(R, TextLeft, TextTop, LabelText, TR)
  end;
end;

procedure TCustomLabel.SetOptimalFill(const AValue: Boolean);
begin
  if FOptimalFill=AValue then exit;
  FOptimalFill:=AValue;
  if OptimalFill and AutoSize then
    AutoSize:=false;
  Invalidate;
end;

 // included by stdctrls.pp


