{ $Id$}
{
 *****************************************************************************
 *                             Win32WSButtons.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Win32WSButtons;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows, Buttons, Graphics, Controls,
////////////////////////////////////////////////////
  WSButtons, WSLCLClasses, Win32WSControls, LCLType;

type

  { TWin32WSButton }

  TWin32WSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ActiveDefaultButtonChanged(const AButton: TCustomButton); override;
    class procedure SetShortCut(const AButton: TCustomButton; const OldKey, NewKey: word); override;
  end;

  { TWin32WSBitBtn }

  TWin32WSBitBtn = class(TWSBitBtn)
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TBitmap); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWin32WSSpeedButton }

  TWin32WSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;

procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; ButtonCaption: PChar);

implementation

uses
  Win32Int, InterfaceBase;

{ TWin32WSButton }

function TWin32WSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    if TCustomButton(AWinControl).Default Then
      Flags := Flags or BS_DEFPUSHBUTTON
    else
      Flags := Flags or BS_PUSHBUTTON;
    pClassName := 'BUTTON';
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

procedure TWin32WSButton.ActiveDefaultButtonChanged(const AButton: TCustomButton);
var
  WindowStyle: dword;
begin
  WindowStyle := Windows.GetWindowLong(AButton.Handle, GWL_STYLE) and not (BS_DEFPUSHBUTTON or BS_PUSHBUTTON);
  If AButton.Active then
    WindowStyle := WindowStyle or BS_DEFPUSHBUTTON
  else
    WindowStyle := WindowStyle or BS_PUSHBUTTON;
  Windows.SendMessage(AButton.Handle, BM_SETSTYLE, WindowStyle, 1);
end;

procedure TWin32WSButton.SetShortCut(const AButton: TCustomButton; const OldKey, NewKey: word);
begin
  // TODO: implement me!
end;

{ TWin32WSBitBtn }

const
  BUTTON_IMAGELIST_ALIGN_LEFT   = 0;
  BUTTON_IMAGELIST_ALIGN_RIGHT  = 1;
  BUTTON_IMAGELIST_ALIGN_TOP    = 2;
  BUTTON_IMAGELIST_ALIGN_BOTTOM = 3;
  BUTTON_IMAGELIST_ALIGN_CENTER = 4;

  BCM_FIRST = $1600;
  BCM_GETIDEALSIZE  = BCM_FIRST + 1;
  BCM_SETIMAGELIST  = BCM_FIRST + 2;
  BCM_GETIMAGELIST  = BCM_FIRST + 3;
  BCM_SETTEXTMARGIN = BCM_FIRST + 4;
  BCM_GETTEXTMARGIN = BCM_FIRST + 5;

  { - you do need to destroy the imagelist yourself. 
    - you'll need 5 images to support all themed xp button states...

    Image 0 = normal
    Image 1 = mouse hover
    Image 2 = button down
    Image 3 = button disabled
    Image 4 = button focus       
  }

  XPBitBtn_ImageIndexToEnabled: array[0..4] of Boolean = 
    (true, true, true, false, true);
  
type
  BUTTON_IMAGELIST = packed record
    himl: Windows.HIMAGELIST;
    margin: Windows.RECT;
    uAlign: UINT;
  end;

{------------------------------------------------------------------------------
  Method: DrawBitBtnImage
  Params:  BitBtn: The TCustomBitBtn to update the image of
           ButtonCaption: new button caption
  Returns: Nothing

  Updates the button image combining the glyph and caption
 ------------------------------------------------------------------------------}
procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; ButtonCaption: PChar);
var
  BitmapHandle: HBITMAP; // Handle of the button glyph
  BitBtnLayout: TButtonLayout; // Layout of button and glyph
  BitBtnHandle: HWND; // Handle to bitbtn window
  BitBtnDC: HDC; // Handle to DC of bitbtn window
  OldFontHandle: HFONT; // Handle of previous font in hdcNewBitmap
  hdcNewBitmap: HDC; // Device context of the new Bitmap
  BitmapInfo: BITMAP; // Buffer for bitmap
  TextSize: Windows.SIZE; // For computing the length of button caption in pixels
  OldBitmap: HBITMAP; // Handle to the old selected bitmap
  NewBitmap: HBITMAP; // Handle of the new bitmap
  XDestBitmap, YDestBitmap: integer; // X,Y coordinate of destination rectangle for bitmap
  XDestText, YDestText: integer; // X,Y coordinates of destination rectangle for caption
  newWidth, newHeight: integer; // dimensions of new combined bitmap
  BitmapRect: Windows.RECT;
  oldImageList: HIMAGELIST;
  ButtonImageList: BUTTON_IMAGELIST;
  I: integer;

  procedure DrawBitmap(Enabled: boolean);
  var
    SrcDC, MaskDC: HBITMAP;
    MaskBmp, OldSrcBmp, OldMaskBmp: HBITMAP;
    BkColor: TColorRef;
    
    OldBitmapHandle: HBITMAP; // Handle of the provious bitmap in hdcNewBitmap
    BitmapFlags: integer; // flags for glyph (enabled or disabled)
    TextFlags: integer; // flags for caption (enabled or disabled)
  begin
    BitmapFlags := DST_BITMAP;
    TextFlags := DST_PREFIXTEXT;

    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);
    if Enabled or TWin32WidgetSet(InterfaceObject).ThemesActive
    then begin
      // themed 
      if not Enabled 
      then begin
        BitmapFlags := BitmapFlags or DSS_DISABLED;
        TextFlags := TextFlags or DSS_DISABLED;
      end;
      
      Windows.FillRect(hdcNewBitmap, BitmapRect, BitBtn.Brush.Handle);
      if BitmapHandle <> 0 
      then DrawState(hdcNewBitmap, 0, nil, BitmapHandle, 0, XDestBitmap, YDestBitmap, 0, 0, BitmapFlags);
    end
    else begin
      // non themed 
      // When disabled only create a black and white image
      // Let windows itself draw the disabled state
      Windows.FillRect(hdcNewBitmap, BitmapRect, Windows.GetStockObject(WHITE_BRUSH));
      if BitmapHandle <> 0 
      then begin
        // Create a source DC
        SrcDC := CreateCompatibleDC(hdcNewBitmap);
        OldSrcBmp := SelectObject(SrcDC, BitmapHandle);
        // Create a mask DC
        MaskBmp := CreateBitmap(BitmapInfo.bmWidth, BitmapInfo.bmHeight, 1, 1, nil);
        MaskDC := CreateCompatibleDC(hdcNewBitmap);
        OldMaskBmp := SelectObject(MaskDC, MaskBmp);
        // Create the black and white image
        BkColor := SetBkColor(SrcDC, BitBtn.Brush.Color);
        BitBlt(MaskDC, 0, 0, BitmapInfo.bmWidth, BitmapInfo.bmHeight, SrcDC, 0, 0, SrcCopy);
        SetBkColor(SrcDC, BkColor);
        // Draw the black and white image
        BitBlt(hdcNewBitmap, XDestBitmap, YDestBitmap, BitmapInfo.bmWidth, BitmapInfo.bmHeight,
               MaskDC, 0, 0, SRCCOPY);  
  
        SelectObject(SrcDC, OldSrcBmp);
        DeleteDC(SrcDC);
  
        SelectObject(MaskDC, OldMaskBmp);
        DeleteDC(MaskDC);
        DeleteObject(MaskBmp);
      end;  
    end;
    
    SetBkMode(hdcNewBitmap, TRANSPARENT);
    DrawState(hdcNewBitmap, 0, nil, LPARAM(ButtonCaption), 0, XDestText, YDestText, 0, 0, TextFlags);
    SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;
  
begin
  // gather info about bitbtn
  BitBtnHandle := BitBtn.Handle;
  if BitBtn.Glyph.Empty then
  begin
    BitmapHandle := 0;
    BitmapInfo.bmWidth := 0;
    BitmapInfo.bmHeight := 0;
  end else begin
    BitmapHandle := BitBtn.Glyph.Handle;
    Windows.GetObject(BitmapHandle, sizeof(BitmapInfo), @BitmapInfo);
  end;
  BitBtnLayout := BitBtn.Layout;
  BitBtnDC := GetDC(BitBtnHandle);
  hdcNewBitmap := CreateCompatibleDC(BitBtnDC);
  OldFontHandle := SelectObject(hdcNewBitmap, BitBtn.Font.Handle);
  GetTextExtentPoint32(hdcNewBitmap, LPSTR(ButtonCaption), Length(ButtonCaption), TextSize);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      newWidth := TextSize.cx + BitmapInfo.bmWidth;
      if BitmapHandle <> 0 then
        inc(newWidth, 2);
      newHeight := TextSize.cy;
      if newHeight < BitmapInfo.bmHeight then
        newHeight := BitmapInfo.bmHeight;
      YDestBitmap := (newHeight - BitmapInfo.bmHeight) shr 1;
      YDestText := (newHeight - TextSize.cy) shr 1;
    end;
    blGlyphTop, blGlyphBottom:
    begin
      newWidth := TextSize.cx;
      if newWidth < BitmapInfo.bmWidth then
        newWidth := BitmapInfo.bmWidth;
      newHeight := TextSize.cy + BitmapInfo.bmHeight;
      if BitmapHandle <> 0 then
        inc(newHeight, 2);
      XDestBitmap := (newWidth - BitmapInfo.bmWidth) shr 1;
      XDestText := (newWidth - TextSize.cx) shr 1;
    end;
  end;
  case BitBtnLayout of
    blGlyphLeft: 
    begin
      XDestBitmap := 0;
      XDestText := newWidth - TextSize.cx;
    end;
    blGlyphRight: 
    begin
      XDestBitmap := newWidth - BitmapInfo.bmWidth;
      XDestText := 0;
    end;
    blGlyphTop: 
    begin
      YDestBitmap := 0;
      YDestText := newHeight - TextSize.cy;
    end;
    blGlyphBottom: begin
      YDestBitmap := newHeight - BitmapInfo.bmHeight;
      YDestText := 0;
    end;
  end;
  // create new
  if (newWidth = 0) and (newHeight = 0) then
    NewBitmap := 0
  else
    NewBitmap := CreateCompatibleBitmap(BitBtnDC, newWidth, newHeight);
  BitmapRect.left := 0;
  BitmapRect.top := 0;
  BitmapRect.right := newWidth;
  BitmapRect.bottom := newHeight;
  // destroy previous bitmap, set new bitmap
  if TWin32WidgetSet(InterfaceObject).ThemesActive then
  begin
    // winxp draws BM_SETIMAGE bitmap with old style button!
    // need to use BCM_SETIMAGELIST
    oldImageList := Windows.SendMessage(BitBtnHandle, BCM_GETIMAGELIST, 0, LPARAM(@ButtonImageList)); 
    if oldImageList <> 0 then
      oldImageList := ButtonImageList.himl;
    if NewBitmap <> 0 then
    begin
      ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 5, 0);
      ButtonImageList.margin.left := 2;
      ButtonImageList.margin.right := 2;
      ButtonImageList.margin.top := 2;
      ButtonImageList.margin.bottom := 2;
      ButtonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER;
      // for some reason, if bitmap added to imagelist, need to redrawn, otherwise it's black!?
      for I := 0 to 4 do
      begin
        DrawBitmap(XPBitBtn_ImageIndexToEnabled[I]);
        ImageList_AddMasked(ButtonImageList.himl, NewBitmap, ColorToRGB(BitBtn.Brush.Color));
      end;
    end else begin
      ButtonImageList.himl := 0;
    end;
    Windows.SendMessage(BitBtnHandle, BCM_SETIMAGELIST, 0, LPARAM(@ButtonImageList));
    if oldImageList <> 0 then
      ImageList_Destroy(oldImageList);
    if NewBitmap <> 0 then
      DeleteObject(NewBitmap);
  end else begin
    OldBitmap := Windows.SendMessage(BitBtnHandle, BM_GETIMAGE, IMAGE_BITMAP, 0);
    if NewBitmap <> 0 then
      DrawBitmap(BitBtn.Enabled);
    Windows.SendMessage(BitBtnHandle, BM_SETIMAGE, IMAGE_BITMAP, NewBitmap);
    if OldBitmap <> 0 then
      DeleteObject(OldBitmap);
  end;
  SelectObject(hdcNewBitmap, OldFontHandle);
  DeleteDC(hdcNewBitmap);
  ReleaseDC(BitBtnHandle, BitBtnDC);
end;

function TWin32WSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'BUTTON';
    if TCustomBitBtn(AWinControl).Default Then
      Flags := Flags or BS_DEFPUSHBUTTON
    else
      Flags := Flags or BS_PUSHBUTTON;
    Flags := Flags or BS_BITMAP;
    WindowTitle := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

procedure TWin32WSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TBitmap);
begin
  DrawBitBtnImage(ABitBtn, PChar(ABitBtn.Caption));
end;

procedure TWin32WSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  DrawBitBtnImage(ABitBtn, PChar(ABitBtn.Caption));
end;

procedure TWin32WSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  DrawBitBtnImage(ABitBtn, PChar(ABitBtn.Caption));
end;

procedure TWin32WSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  DrawBitBtnImage(ABitBtn, PChar(ABitBtn.Caption));
end;

procedure TWin32WSBitBtn.SetText(const AWinControl: TWinControl; const AText: string);
begin
  DrawBitBtnImage(TCustomBitBtn(AWinControl), PChar(AText));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomButton, TWin32WSButton);
  RegisterWSComponent(TCustomBitBtn, TWin32WSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TWin32WSSpeedButton);
////////////////////////////////////////////////////
end.
