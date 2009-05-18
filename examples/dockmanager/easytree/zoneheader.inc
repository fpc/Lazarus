type
  TDockHeaderImageKind =
  (
    dhiRestore,
    dhiClose
  );

  TDockHeaderImages = array[TDockHeaderImageKind] of TCustomBitmap;

const
  DockHeaderImageNames: array[TDockHeaderImageKind] of String =
  (
{ dhiRestore } 'easy_dock_restore',
{ dhiClose   } 'easy_dock_close'
  );

var
  DockBtnImages: TDockHeaderImages;


procedure CreateDockHeaderImages;
var
  ImageKind: TDockHeaderImageKind;
begin
  for ImageKind := Low(TDockHeaderImageKind) to High(TDockHeaderImageKind) do
    DockBtnImages[ImageKind] := CreateBitmapFromLazarusResource(DockHeaderImageNames[ImageKind]);
end;

procedure DestroyDockHeaderImages;
var
  ImageKind: TDockHeaderImageKind;
begin
  for ImageKind := Low(TDockHeaderImageKind) to High(TDockHeaderImageKind) do
    FreeAndNil(DockBtnImages[ImageKind]);
end;


{ TEasyDockHeader }

const
  dSizer = 4;
  dBorder = 2; //frame and inner bevel
  dDist = 1; //button distance
{$IFDEF newsplitter}
  dHeader = 22 - dSizer; //splitter outside header!
{$ELSE}
  dHeader = 22;
{$ENDIF}

constructor TEasyDockHeader.Create;
{
  procedure dump;
  var
    r, r2: TRect;
  begin
    r := Rect(0, 0, 200, HeaderSize); //LTBR
    r2 := GetRectOfPart(r, doVertical, zpCaption);
    DebugLn('%s (%d,%d)-(%d,%d)', ['caption', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, doVertical, zpCloseButton);
    DebugLn('%s (%d,%d)-(%d,%d)', ['closer ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, doVertical, zpSizer);
    DebugLn('%s (%d,%d)-(%d,%d)', ['sizer  ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
  end;
}
begin
  HeaderSize := dHeader; //some meaningful value?
//debug
  //dump;
end;

class function TEasyDockHeader.GetRectOfPart(AHeaderRect: TRect; AOrientation: TDockOrientation;
  APart: TEasyZonePart; HasSplitter: boolean): TRect;
var
  d, dRight, dWidth: Integer;
begin
  if (APart = zpNowhere) or (APart = zpClient)
  or ((APart = zpSizer) and not HasSplitter) then begin
    Result := Rect(0,0,0,0);
    exit;
  end;

  Result := AHeaderRect;
  //if APart = zpAll then Exit; //include sizer?

  if APart = zpSizer then begin
  //at top/left - visible only if HasSplitter
    if AOrientation = doVertical then
      Result.Bottom := Result.Top + dSizer
    else
      Result.Right := Result.Left + dSizer;
    exit;
  end;
//exclude sizer
if not HasSplitter then begin
  if AOrientation = doVertical then
    inc(Result.Top, dSizer)
  else
    inc(Result.Left, dSizer);
end;

  if APart = zpAll then
    Exit; //exclude sizer

//exclude border, assume 1 pixel border, 1 pixel inner distance
  InflateRect(Result, -dBorder, -dBorder); //border(2), remaining = rectangular button width/height
//get remaining size for buttons
  if AOrientation = doVertical then
    d := Result.Bottom - Result.Top
  else
    d := Result.Right - Result.Left;

  dWidth := 0;
  case APart of
  //zpAll: - see above
  zpCloseButton: dRight := dDist;
{$IFDEF restore}
  zpRestoreButton: dRight := d + 2*dDist;
  zpCaption: dWidth := 2*(d + dDist); //2 * (button + dist)
{$ELSE}
  zpCaption: dWidth := (d + dDist); //1 * (button + dist)
{$ENDIF}
  //zpSizer: - see above
  //zpClient, //here: invalid argument!
  //zpNowhere:  Result := Rect(0,0,0,0);
  end;

  if AOrientation = doVertical then begin
    if dWidth > 0 then begin //caption
      dec(Result.Right, dBorder+dWidth);
    end else begin //buttons
      dec(Result.Right, dBorder+dRight);
      Result.Left := Result.Right - d;
    end;
  end else begin
    if dWidth > 0 then begin //caption
      inc(Result.Top, dBorder+dWidth);
    end else begin //buttons
      inc(Result.Top, dBorder+dRight);
      Result.Bottom := Result.Top + d;
    end;
  end;
end;

function TEasyDockHeader.FindPart(AZone: TEasyZone; MousePos: TPoint; fButtonDown: boolean): TEasyZonePart;
var
  SubRect, r: TRect;
  Control: TControl;
  Part: TEasyZonePart;
  aHandle : HWND;

  function MouseInPart(APart: TEasyZonePart): boolean;
  begin
  //on hit: retain Part and SubRect
    SubRect := GetRectOfPart(r, Control.DockOrientation, APart, AZone.HasSizer);
    Result := PtInRect(SubRect, MousePos);
    if Result then
      Part := APart;
  end;

begin
(* Called from mouse message handler (only!).
  Remember draw state of current zone.
*)
  r := AZone.GetBounds;
  if (AZone.FChildControl = nil) or not PtInRect(r, MousePos) then
    Result := zpNowhere
  else begin
    Control := AZone.FChildControl;
    if Control.DockOrientation = doVertical then
      r.Bottom := Control.Top
    else
      r.Right := Control.Left;
    if not PtInRect(r, MousePos) then
      Part := zpClient //if not in header, must be in control
    else if MouseInPart(zpSizer) or MouseInPart(zpCloseButton)
    {$IFDEF restore}
      or MouseInPart(zpRestoreButton)
    {$ENDIF}
    then
      //all done
    else
      Part := zpCaption;
  end;

  aHandle:=AZone.GetHandle;
//check old state changed
  if (self.MouseZone <> nil)
  and ((MouseZone <> AZone) or (MousePart <> Part) or (MouseDown <> fButtonDown)) then begin
  //reset state?
    if MousePart in HeaderButtons then
      InvalidateRect(aHandle, @PartRect, false); //old button
  end;
//check new state
  if (MouseDown <> fButtonDown) and (MousePart in HeaderButtons) then
    InvalidateRect(aHandle, @SubRect, false); //new button
//set new state
  MouseZone := AZone;
  MousePart := Part;
  MouseDown := fButtonDown;
  PartRect := SubRect;
//done
  Result := Part;
end;

procedure TEasyDockHeader.Draw(AZone: TEasyZone; ACanvas: TCanvas; ACaption: string; const MousePos: TPoint);
(* Problem with colors on other than win32 widgetsets (gtk2...)
*)
{$DEFINE LDock} //mimic LDockTree?
const
  clBack = clHighlight;
  clFont = clHighlightText;

  procedure DrawButton(ARect: TRect; IsMouseDown, IsMouseOver: Boolean; ABitmap: TCustomBitmap); inline;
  const
    // ------------- Pressed, Hot -----------------------
    BtnDetail: array[Boolean, Boolean] of TThemedToolBar =
    (
     (ttbButtonNormal, ttbButtonHot),
     (ttbButtonNormal, ttbButtonPressed)
    );
  var
    Details: TThemedElementDetails;
    dx, dy: integer;
  begin
    Details := ThemeServices.GetElementDetails(BtnDetail[IsMouseDown, IsMouseOver]);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
    ARect := ThemeServices.ContentRect(ACanvas.Handle, Details, ARect);
  //zoom button into rect?
  {$IFDEF LDock}
    dx := (ARect.Right - ARect.Left - ABitmap.Width) div 2;
    dy := (ARect.Bottom - ARect.Top - ABitmap.Height) div 2;
    ACanvas.Draw(ARect.Left + dx, ARect.Top + dy, ABitmap);
  {$ELSE}
    ACanvas.Draw(ARect.Left, ARect.Top, ABitmap);
  {$ENDIF}
  end;

var
  BtnRect: TRect;
  ARect, DrawRect: TRect;
  // LCL doesn't handle orientation in TFont
  OldFont, RotatedFont: HFONT;
  OldMode: Integer;
  ALogFont: TLogFont;
  IsMouseDown: Boolean; //obsolete
  AOrientation: TDockOrientation;
  AControl: TControl;
begin
(* Some colors inavailable on some widgetsets!
  (NewSplitter at opposite side of header! - not in this version)
*)
  //IsMouseDown := self.MouseDown;  // not always correct?
  IsMouseDown := (GetKeyState(VK_LBUTTON) and $80) <> 0;
//debug
  AControl := AZone.FChildControl;
  AOrientation := AControl.DockOrientation;

  ARect := AZone.GetBounds;
  BtnRect := ARect;
  if AZone.FChildControl.DockOrientation = doVertical then begin
    ARect.Bottom := ARect.Top + HeaderSize; //entire header area
    BtnRect.Top := BtnRect.Bottom - dSizer; //splitter
  end else begin
    ARect.Right := ARect.Left + HeaderSize;
    BtnRect.Left := BtnRect.Right - dSizer;
  end;
  DrawRect := ARect;
  ACanvas.Brush.Color := clBtnFace;

  if AZone.HasSizer then begin
{$IFDEF newsplitter}
// splitter no more in header! - BtnRect initialized above
    ACanvas.FillRect(BtnRect);
{$ELSE}
//draw splitter?
  BtnRect := GetRectOfPart(ARect, AOrientation, zpSizer, AZone.HasSizer);
{$IFDEF LDock}
{$ELSE}
  ACanvas.Brush.Color := clBtnFace;
{$ENDIF}
  ACanvas.FillRect(BtnRect);
{$ENDIF}
  end;

//erase?
  DrawRect := GetRectOfPart(ARect, AOrientation, zpAll, AZone.HasSizer);
{$IFDEF LDock}
{$ELSE}
  ACanvas.Brush.Color := clBack; // clActiveCaption;
{$ENDIF}
  ACanvas.FillRect(DrawRect);

//what's this?
  InflateRect(DrawRect, -1, -1); //outer bevel?
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FrameRect(DrawRect);
{$IFDEF LDock}
  InflateRect(DrawRect, -1, -1); //inner bevel?
  //the value is not used any more!
{$ELSE}
{$ENDIF}

// draw caption
{$IFDEF LDock}
{$ELSE}
  ACanvas.Font.Color := clFont; //clCaptionText;
{$ENDIF}
  DrawRect := GetRectOfPart(ARect, AOrientation, zpCaption, AZone.HasSizer);

  OldMode := SetBkMode(ACanvas.Handle, TRANSPARENT);

  if AOrientation = doVertical then begin
    DrawText(ACanvas.Handle, PChar(ACaption), -1, DrawRect, DT_LEFT or DT_SINGLELINE or DT_VCENTER)
  end else begin
    OldFont := 0;
    if GetObject(ACanvas.Font.Reference.Handle, SizeOf(ALogFont), @ALogFont) <> 0 then
    begin
      ALogFont.lfEscapement := 900;
      RotatedFont := CreateFontIndirect(ALogFont);
      if RotatedFont <> 0 then
        OldFont := SelectObject(ACanvas.Handle, RotatedFont);
    end;
    // from msdn: DrawText doesnot support font with orientation and escapement <> 0
    TextOut(ACanvas.Handle, DrawRect.Left, DrawRect.Bottom, PChar(ACaption), Length(ACaption));
    if OldFont <> 0 then
      DeleteObject(SelectObject(ACanvas.Handle, OldFont));
  end;
  SetBkMode(ACanvas.Handle, OldMode);

// buttons - which colors to use?
{$IFDEF LDock}
{$ELSE}
  ACanvas.Brush.Color := clBtnFace;
  //ACanvas.Pen.Color := clButtonText;
{$ENDIF}

// draw close button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpCloseButton, AZone.HasSizer);
  ACanvas.FillRect(BtnRect);
  DrawButton(BtnRect, IsMouseDown, PtInRect(BtnRect, MousePos), DockBtnImages[dhiClose]);

{$IFDEF restore}
// draw restore button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpRestoreButton, AZone.HasSizer);
  ACanvas.FillRect(BtnRect);
  DrawButton(BtnRect, IsMouseDown, PtInRect(BtnRect, MousePos), DockBtnImages[dhiRestore]);
{$ENDIF}
end;

