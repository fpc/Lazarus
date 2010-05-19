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
{$IFDEF freeImages}
  //called from unit finalization only!
  // this code can result in crashes, due to missing handles!
  for ImageKind := Low(TDockHeaderImageKind) to High(TDockHeaderImageKind) do
    FreeAndNil(DockBtnImages[ImageKind]);
{$ELSE}
{$ENDIF}
end;


{ TEasyDockHeader }

type
  TZonePartMap = record
    dTop, dBottom, dLeft, dRight: integer;
  end;

const //zone decoration sizes
  dSizer = 4;
  dBorder = 2; //frame and inner bevel
  dDist = 1; //button distance
  dButton = 14; //include some space for themes
  dHeader = dButton + 2*dBorder; // 22 - dSizer; //splitter outside header!
//minimal (Delphi) header
  GrabberSize = 10; // 12;
  dDDist = 1;
  dDBorder = 1;
  dDHeader = GrabberSize; //?
  dDButton = GrabberSize - 2*dDBorder; //10?

(* Zone part map.
  In portrait mode (header on top), the zone rectangle is adjusted according
  to the given offsets. In landscape mode (header on the left), the offsets
  have to be applied to the rotated coordinates.

  Positive offsets mean self-relative adjustment, towards the opposite edge.
  This operation has highest precedence.
  Negative offsets mean adjustment relative to the adjusted opposite edge.

  zpAll excludes the splitter and client area.
  zpCaption excludes borders and buttons from zpAll.
*)
(*
  The splitter is excluded first, if present.
  [zpSizer] entry is unused.
*)
HeaderPartMap: array[TEasyHeaderStyle, TEasyZonePart] of TZonePartMap = (
//hsMinimal
  (
    {zpNowhere} (),
    {zpClient}  (dTop:dDHeader; dBottom:0),
    {zpAll}     (dTop:0; dBottom:-dDHeader),
    {zpCaption} (dTop:dDBorder; dBottom:-dDButton; dLeft:dDBorder; dRight:2*dDBorder+dDButton),
    {zpSizer}   (dTop:0; dBottom:-dSizer),
  {$IFDEF restore}
    {zpRestoreButton} (),
  {$ENDIF}
    {zpCloseButton} (dTop:dDBorder; dBottom:-dDButton; dLeft:-dDButton; dRight:dDBorder)
  ),
//hsForm
  (
    (),    //zpNowhere - not in any zone
    (dTop:dHeader; dBottom:0),    //zpClient - on client control
    (dTop:0; dBottom:-dHeader),    //zpAll - total header rect
    (dTop:dBorder; dBottom:-dButton; dLeft:dBorder; dRight:dBorder+dButton),    //zpCaption
    (dTop:0; dBottom:-dSizer),    //zpSizer - splitter/sizer
    {$IFDEF restore}
    (dTop:dBorder; dBottom:-dButton; dLeft:-dButton; dRight:dButton+2*dBorder)    //zpRestoreButton,  // header restore button
    {$ENDIF}
    (dTop:dBorder; dBottom:-dButton; dLeft:-dButton; dRight:dBorder)    //zpCloseButton     // header close button
  ),
 //hsNone deserves special handling, the map is ignored
  (
    {zpNowhere} (),
    {zpClient}  (dTop:0; dBottom:0),
    {zpAll}     (dTop:0; dBottom:-dDHeader),
    {zpCaption} (dTop:dDBorder; dBottom:-dDButton; dLeft:dDBorder; dRight:2*dDBorder+dDButton),
    {zpSizer}   (dTop:0; dBottom:-dSizer),
  {$IFDEF restore}
    {zpRestoreButton} (),
  {$ENDIF}
    {zpCloseButton} (dTop:dDBorder; dBottom:-dDButton; dLeft:-dDButton; dRight:dDBorder)
  )
);

constructor TEasyDockHeader.Create;
{
  procedure dump;
  var
    r, r2: TRect;
  const
    hc = true;
    orn = doVertical;
  begin
    r := Rect(0, 0, 200, 200); //LTBR
    r2 := GetRectOfPart(r, orn, zpAll, hc);
    DebugLn('%s (%d,%d)-(%d,%d)', ['header ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, orn, zpCaption, hc);
    DebugLn('%s (%d,%d)-(%d,%d)', ['caption', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, orn, zpCloseButton, hc);
    DebugLn('%s (%d,%d)-(%d,%d)', ['closer ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, orn, zpClient, hc);
    DebugLn('%s (%d,%d)-(%d,%d)', ['client ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, orn, zpSizer, hc);
    DebugLn('%s (%d,%d)-(%d,%d)', ['sizer  ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
  end;
}
begin
  //HeaderSize := dHeader; //some meaningful value?
//debug
  //dump; - not shown in console
end;

function TEasyDockHeader.GetRectOfPart(ARect: TRect; AOrientation: TDockOrientation;
  APart: TEasyZonePart; HasSplitter: boolean; AStyle: TEasyHeaderStyle): TRect;
begin
(* ARect is (must be) TLBR zone rectangle, on input.
  HasSplitter determines whether to exclude the splitter from ARect,
  and also the splitter area itself.

  hsNone (no header) must be handled separately.
*)
  if (APart = zpNowhere)
  or ((APart = zpSizer) and not HasSplitter)
  or ((AStyle = hsNone) and (APart <> zpClient))
  then begin
    Result := Rect(0,0,0,0);
    exit;
  end;

  Result := ARect;
  if AStyle = hsNone then
    exit; //client fills entire area

  with HeaderPartMap[AStyle, APart] do begin
    if AOrientation = doVertical then begin //portrait
    //handle client w/o splitter
      if (APart = zpSizer) then begin
        Result.Bottom := Result.Top + dSizer;
        exit;
      end;
      if HasSplitter then
        inc(Result.Top, dSizer); //exclude splitter(?)
      if dTop > 0 then
        inc(Result.Top, dTop);
      if dBottom > 0 then
        dec(Result.Bottom, dBottom)
      else if dBottom < 0 then
        Result.Bottom := Result.Top - dBottom;
      if dTop < 0 then
        Result.Top := Result.Bottom + dTop;
      if dLeft > 0 then
        inc(Result.Left, dLeft);
      if dRight > 0 then
        dec(Result.Right, dRight)
      else if dRight < 0 then
        Result.Right := Result.Left + dRight;
      if dLeft < 0 then
        Result.Left := Result.Right + dLeft;
    end else begin //landscape
    //handle client w/o splitter
      if (APart = zpSizer) then begin
        Result.Right := Result.Left + dSizer;
        exit;
      end;
      if HasSplitter then
        inc(Result.Left, dSizer);
      if dTop > 0 then
        inc(Result.Left, dTop);
      if dBottom > 0 then
        dec(Result.Right, dBottom)
      else if dBottom < 0 then
        Result.Right := Result.Left - dBottom;
      if dTop < 0 then
        Result.Left := Result.Right + dTop;

      if dLeft > 0 then
        dec(Result.Bottom, dLeft);
      if dRight > 0 then
        inc(Result.Top, dRight)
      else if dRight < 0 then
        Result.Top := Result.Bottom + dRight;
      if dLeft < 0 then
        Result.Bottom := Result.Top - dLeft;
    end;
  end;
end;

function TEasyDockHeader.FindPart(AZone: TEasyZone; MousePos: TPoint; fButtonDown: boolean): TEasyZonePart;
var
  SubRect, r: TRect;
  Control: TControl;
  Part: TEasyZonePart;
  aHandle : HWND;
  aOrient: TDockOrientation;

  function MouseInPart(APart: TEasyZonePart): boolean;
  begin
  //on hit: retain Part and SubRect
    SubRect := GetRectOfPart(r, aOrient, APart, AZone.HasSizer, AZone.Style);
    Result := PtInRect(SubRect, MousePos);
    if Result then
      Part := APart;
  end;

begin
(* Called from mouse message handler (only!).
  Remember draw state of current zone.
*)
//exclude higher level splitters?
  r := AZone.GetBounds;
  if not PtInRect(r, MousePos) or (AZone.Parent = nil) then
  //possibly empty root zone
    Part := zpNowhere
  else if (AZone.FChildControl = nil) then begin
  //newSplitter: also zones without controls can have an splitter
    Control := nil;
    aOrient := AZone.Parent.Orientation;
    if not AZone.HasSizer or not MouseInPart(zpSizer) then
      Part := zpNowhere; //client?
  end else begin
    Control := AZone.FChildControl;
    aOrient := Control.DockOrientation;
    if MouseInPart(zpSizer) or MouseInPart(zpCloseButton)
    {$IFDEF restore}
      or MouseInPart(zpRestoreButton)
    {$ENDIF}
    or MouseInPart(zpClient)
    then
      //all done, result stored in Part
    else
      Part := zpCaption; //include borders
  end;
{ TODO -cdocking : sometimes Part is invalid? }
//DebugLn('IN ', PartNames[Part]);

  aHandle:=AZone.GetHandle;
//check old state changed - buttons also change state on mouse enter/exit
  if (self.MouseZone <> nil) //else Mouse... invalid
  and ((MouseZone <> AZone) or (MousePart in HeaderButtons)) then
      InvalidateRect(aHandle, @PartRect, false); //old button
//check new state
  if (Part in HeaderButtons) then begin
    InvalidateRect(aHandle, @SubRect, false); //refresh button
  end;
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
const
  clBack = clHighlight;
  clFont = clHighlightText;

var
  IsMouseDown: Boolean;

  //procedure DrawButton(ARect: TRect; IsMouseDown, IsMouseOver: Boolean; ABitmap: TCustomBitmap); inline;
  procedure DrawButton(ARect: TRect; ABitmap: TCustomBitmap);
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
    IsMouseOver: boolean;
  begin
    ACanvas.FillRect(ARect);
    IsMouseOver := PtInRect(ARect, MousePos);
    Details := ThemeServices.GetElementDetails(BtnDetail[IsMouseDown, IsMouseOver]);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
    ARect := ThemeServices.ContentRect(ACanvas.Handle, Details, ARect);
  //move button into rect, excluding border
    dx := (ARect.Right - ARect.Left - ABitmap.Width) div 2;
    dy := (ARect.Bottom - ARect.Top - ABitmap.Height) div 2;
    ACanvas.Draw(ARect.Left + dx, ARect.Top + dy, ABitmap);
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    with ACanvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top);
    end;
  end;

var
  BtnRect: TRect;
  ARect, DrawRect: TRect;
  // LCL doesn't handle orientation in TFont
  OldFont, RotatedFont: HFONT;
  OldMode: Integer;
  ALogFont: TLogFont;
  AOrientation: TDockOrientation;
  AControl: TControl;
  AStyle: TEasyHeaderStyle;
  HasSizer: boolean;
begin
(* Some colors inavailable on some widgetsets!
*)
  if AZone.Style = hsNone then
    exit; //no header at all

  IsMouseDown := (GetKeyState(VK_LBUTTON) and $80) <> 0;
//debug
  AControl := AZone.FChildControl;
  AStyle := AZone.Style;
  HasSizer := AZone.HasSizer;
  AOrientation := AControl.DockOrientation;

  ARect := AZone.GetBounds;
  ACanvas.Brush.Color := clBtnFace;

//draw splitter?
  if HasSizer then begin
  // border?
    BtnRect := GetRectOfPart(ARect, AOrientation, zpSizer, HasSizer, AStyle);
    ACanvas.FillRect(BtnRect);
  end;

//erase - which color(s)?
  DrawRect := GetRectOfPart(ARect, AOrientation, zpAll, HasSizer, AStyle);
  //ACanvas.Brush.Color := clBack; // clActiveCaption;
  ACanvas.FillRect(DrawRect);

//border
  InflateRect(DrawRect, -1, -1); //outer bevel?
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FrameRect(DrawRect); //1 pixel border
  ACanvas.Brush.Color := clBtnFace;

// draw caption
  case AStyle of
  hsMinimal:
    with DrawRect do begin
      DrawRect := GetRectOfPart(ARect, AOrientation, zpCaption, HasSizer, AStyle);
      if AOrientation = doVertical then begin
        inc(Top, dDBorder);
        DrawGrabberLine(Left, Top, Right, Top+2);
        inc(Top, 3);
        DrawGrabberLine(Left, Top, Right, Top+2);
      end else begin
        inc(Left, dDBorder);
        DrawGrabberLine(Left, Top, Left+2, Bottom);
        inc(Left, 3);
        DrawGrabberLine(Left, Top, Left+2, Bottom);
      end;
    end;
  hsForm:
    begin
      //ACanvas.Font.Color := clFont; //clCaptionText;
      DrawRect := GetRectOfPart(ARect, AOrientation, zpCaption, HasSizer, AStyle);

      OldMode := SetBkMode(ACanvas.Handle, TRANSPARENT);

    //for some reason the caption is not offset properly?
      if AOrientation = doVertical then begin
        inc(DrawRect.Left, dBorder); //looks better
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
        //TextOut(ACanvas.Handle, DrawRect.Left, DrawRect.Bottom, PChar(ACaption), Length(ACaption));
        TextOut(ACanvas.Handle, DrawRect.Left-2, DrawRect.Bottom-2, PChar(ACaption), Length(ACaption));
        if OldFont <> 0 then
          DeleteObject(SelectObject(ACanvas.Handle, OldFont));
      end;
      SetBkMode(ACanvas.Handle, OldMode);
    end;
  end;

// buttons - which colors to use?
  ACanvas.Brush.Color := clBtnFace;
  //ACanvas.Pen.Color := clButtonText;

// draw close button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpCloseButton, HasSizer, AStyle);
  DrawButton(BtnRect, DockBtnImages[dhiClose]);

{$IFDEF restore}
// draw restore button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpRestoreButton, HasSizer, AStyle);
  DrawButton(BtnRect, DockBtnImages[dhiRestore]);
{$ENDIF}
end;

