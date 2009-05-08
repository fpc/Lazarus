unit EasyDockHelpers;
(* Defines helper classes for TEasyDockSite.
 Parts stolen from LDockTree...

- Zone header class
- Basic zone class, used in painting the header

The zone header class may become a component, derived e.g. from TSplitter.
  It paints all parts of the zone header.

Support for an (experimental) Restore button is conditionally available,
  but it deserves a definition of its purpose.
  Hiding and unhiding controls requires docking manager notifications in the LCL!
*)

{$mode objfpc}{$H+}
{.$DEFINE restore} //use restore button?

interface

uses
  Types, LCLType, Controls, Graphics, ExtCtrls;

type
  TEasyZonePart =
  (
    zpNowhere,        // not in any zone
    zpClient,         // on client control
    zpAll,            // total header rect
    zpCaption,        // header caption
    zpSizer,          // splitter/sizer
  {$IFDEF restore}
    zpRestoreButton,  // header restore button
  {$ENDIF}
    zpCloseButton     // header close button
  );

//minimal zone interface, used by TEasyDockHeader and other helper classes

  { TCustomDockZone }

  TCustomDockZone = class
  protected //deserve direct access in derived classes
    FChildControl: TControl;
    function  GetHeaderSize: integer; virtual;
    function  GetHandle: HWND; virtual; abstract;
    function  GetRectOfPart(APart: TEasyZonePart): TRect;
  public
    function  GetBounds: TRect; virtual;
    function  HasSizer: boolean; virtual;
  end;

  { TEasyDockHeader }

// maybe once it will be control, so now better to move all related to header things to class
  TEasyDockHeader = class
  public
    HeaderSize: integer;
  //state last drawn
    MouseZone: TCustomDockZone;
    MouseDown: boolean;
    MousePart: TEasyZonePart;
    PartRect: TRect;
  public
    constructor Create;
    class function  GetRectOfPart(AHeaderRect: TRect; AOrientation: TDockOrientation; APart: TEasyZonePart; HasSplitter: boolean): TRect;
    function  FindPart(AZone: TCustomDockZone; MousePos: TPoint; fButtonDown: boolean): TEasyZonePart;
    procedure Draw(AZone: TCustomDockZone; ACanvas: TCanvas; ACaption: string; const MousePos: TPoint);
  end;

  TEasySplitter = TCustomSplitter;

const
{$IFDEF restore}
  HeaderButtons = [zpCloseButton, zpRestoreButton];
{$ELSE}
  HeaderButtons = [zpCloseButton];
{$ENDIF}

implementation

uses
  Classes, SysUtils, math, Themes, LResources, LCLIntf, LCLProc;

type
{
  TDockHeaderMouseState = record
    Rect: TRect;
    IsMouseDown: Boolean;
  end;
}
  TDockHeaderImageKind =
  (
    dhiRestore,
    dhiClose
  );

  TDockHeaderImages = array[TDockHeaderImageKind] of TCustomBitmap;

const
  DockHeaderImageNames: array[TDockHeaderImageKind] of String =
  (
{ dhiRestore } 'lcl_dock_restore',
{ dhiClose   } 'lcl_dock_close'
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


type
  TZonePartMap = record
    dTop, dBottom, dLeft, dRight: integer;
  end;

const //zone decoration sizes
  dSizer = 4;
  dBorder = 2; //frame and inner bevel
  dDist = 1; //button distance
  dButton = 14;
  dHeader = dButton + 2*dBorder; // 22 - dSizer; //splitter outside header!

(* Zone part map.
  In portrait mode (header on top), the zone rectangle is adjusted according
  to the given offsets. In landscape mode (header on the left), the offsets
  have to be applied to the rotated coordinates.

  Positive offsets mean self-relative adjustment, towards the opposite edge.
  This operation has highest precedence.
  Negative offsets mean adjustment relative to the opposite edge.

  The map reflects new splitter placement (past client area),
  and no restore button.
*)
HeaderPartMap: array[TEasyZonePart] of TZonePartMap = (
  (),    //zpNowhere,        // not in any zone
  (dTop:dHeader; dBottom:0),    //zpClient,         // on client control
  (dTop:0; dBottom:-dHeader),    //zpAll,            // total header rect
  (dTop:dBorder; dBottom:-dButton; dLeft:dBorder; dRight:dBorder+dButton),    //zpCaption,        // header caption
  (dTop:-dSizer),    //zpSizer,          // splitter/sizer
  {$IFDEF restore}
  (...),    //zpRestoreButton,  // header restore button
  {$ENDIF}
  (dTop:dBorder; dBottom:-dHeader; dLeft:-(dBorder+dButton); dRight:dBorder)    //zpCloseButton     // header close button
);

constructor TEasyDockHeader.Create;

  procedure dump;
  var
    r, r2: TRect;
  begin
    r := Rect(0, 0, 200, HeaderSize); //LTBR
    r2 := GetRectOfPart(r, doVertical, zpCaption, True);
    DebugLn('%s (%d,%d)-(%d,%d)', ['caption', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, doVertical, zpCloseButton, true);
    DebugLn('%s (%d,%d)-(%d,%d)', ['closer ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
    r2 := GetRectOfPart(r, doVertical, zpSizer, true);
    DebugLn('%s (%d,%d)-(%d,%d)', ['sizer  ', r2.Top, r2.Left, r2.Bottom, r2.Right]);
  end;

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
(* AHeaderRect is (must be) TLBR zone rectangle, on input.
*)
  if (APart = zpNowhere) or ((APart = zpSizer) and not HasSplitter) then begin
    Result := Rect(0,0,0,0);
    exit;
  end;

  Result := AHeaderRect;
  with HeaderPartMap[APart] do begin
    if AOrientation = doVertical then begin //portrait
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
    //handle client w/o splitter
      if (APart = zpClient) and HasSplitter then
        dec(Result.Bottom, dSizer);
    end else begin //landscape
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
        Result.Bottom := Result.Top + dLeft;
    //handle client w/o splitter
      if (APart = zpClient) and HasSplitter then
        dec(Result.Right, dSizer);
    end;
  end;
end;

function TEasyDockHeader.FindPart(AZone: TCustomDockZone; MousePos: TPoint; fButtonDown: boolean): TEasyZonePart;
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
  {
    if Control.DockOrientation = doVertical then
      r.Bottom := Control.Top
    else
      r.Right := Control.Left;
    if not PtInRect(r, MousePos) then
      Part := zpClient //if not in header, must be in control
  }
    if MouseInPart(zpSizer) or MouseInPart(zpCloseButton)
    {$IFDEF restore}
      or MouseInPart(zpRestoreButton)
    {$ENDIF}
    or MouseInPart(zpClient)
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

procedure TEasyDockHeader.Draw(AZone: TCustomDockZone; ACanvas: TCanvas; ACaption: string; const MousePos: TPoint);
(* Problem with colors on other than win32 widgetsets (gtk2...)
*)
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
  {$IFDEF old}
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
  // LCL dont handle orientation in TFont
  OldFont, RotatedFont: HFONT;
  OldMode: Integer;
  ALogFont: TLogFont;
  IsMouseDown: Boolean; //obsolete
  AOrientation: TDockOrientation;
  AControl: TControl;
begin
(* Some colors inavailable on some widgetsets!
*)
  IsMouseDown := self.MouseDown;  // (GetKeyState(VK_LBUTTON) and $80) <> 0;
//debug
  AControl := AZone.FChildControl;
  AOrientation := AControl.DockOrientation;

  ARect := AZone.GetBounds;
  BtnRect := ARect;
  if AZone.FChildControl.DockOrientation = doVertical then begin
    ARect.Bottom := ARect.Top + HeaderSize;
    BtnRect.Top := BtnRect.Bottom - dSizer;
  end else begin
    ARect.Right := ARect.Left + HeaderSize;
    BtnRect.Left := BtnRect.Right - dSizer;
  end;
  DrawRect := ARect;

// splitter no more in header! - BtnRect initialized above
  if AZone.HasSizer and not IsMouseDown then begin
    ACanvas.Brush.Color := clBtnFace;
    ACanvas.FillRect(BtnRect);
  end;

//erase?
  DrawRect := GetRectOfPart(ARect, AOrientation, zpAll, AZone.HasSizer);
  ACanvas.Brush.Color := clBack; // clActiveCaption;
  ACanvas.FillRect(DrawRect);

//what's this? (from LDockTree)
  InflateRect(DrawRect, -1, -1); //outer bevel?
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FrameRect(DrawRect);
  //InflateRect(DrawRect, -1, -1); //inner bevel?

// draw caption
  ACanvas.Font.Color := clFont; //clCaptionText;
  DrawRect := GetRectOfPart(ARect, AOrientation, zpCaption, AZone.HasSizer);

  OldMode := SetBkMode(ACanvas.Handle, TRANSPARENT);

  if AOrientation = doVertical then begin
    // from msdn: DrawText doesnot support font with orientation and escapement <> 0
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
    TextOut(ACanvas.Handle, DrawRect.Left, DrawRect.Bottom, PChar(ACaption), Length(ACaption));
    if OldFont <> 0 then
      DeleteObject(SelectObject(ACanvas.Handle, OldFont));
  end;
  SetBkMode(ACanvas.Handle, OldMode);

// buttons - which colors to use?
  ACanvas.Brush.Color := clBtnFace;
  //ACanvas.Pen.Color := clButtonText;

// draw close button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpCloseButton, AZone.HasSizer);
  ACanvas.FillRect(BtnRect);
  DrawButton(BtnRect, IsMouseDown, PtInRect(BtnRect, MousePos), DockBtnImages[dhiClose]);

{$IFDEF restore}
// draw restore button
  BtnRect := GetRectOfPart(ARect, AOrientation, zpRestoreButton, AZone.hasSizer);
  ACanvas.FillRect(BtnRect);
  DrawButton(BtnRect, IsMouseDown, PtInRect(BtnRect, MousePos), DockBtnImages[dhiRestore]);
{$ENDIF}
end;

{ TCustomDockZone }

function TCustomDockZone.GetBounds: TRect;
begin
  Result := FChildControl.BaseBounds; //avoid (0,0,0,0)
end;

function TCustomDockZone.GetHeaderSize: integer;
begin
  Result := dHeader; //???
end;

function TCustomDockZone.GetRectOfPart(APart: TEasyZonePart): TRect;
begin
(* This method could hold the entire implementation.
*)
  if FChildControl = nil then
    Result := Rect(0,0,0,0)
  else
    Result := TEasyDockHeader.GetRectOfPart(GetBounds, FChildControl.DockOrientation, APart, HasSizer);
end;

function TCustomDockZone.HasSizer: boolean;
begin
  Result := True; //always show - simplest solution?
end;


initialization
{$I lcl_dock_images.lrs}
  CreateDockHeaderImages;
finalization
  DestroyDockHeaderImages;
end.

