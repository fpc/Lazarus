{ $Id: WinCEwsbuttons.pp 8815 2006-02-24 13:31:16Z mattias $}
{
 *****************************************************************************
 *                              WinCEWSButtons.pp                            *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit WinCEWSButtons;

{$mode delphi}{$H+}

interface

uses
  // Libs
  Windows,
  // LCL
  SysUtils, Controls, LCLType, Forms, graphics, InterfaceBase, Buttons, LMessages,WinCEProc, WinCEWSControls,
  // Widgetset
  WSButtons, WSLCLClasses;

type
  { TWinCEWSBitBtn }

  TWinCEWSBitBtn = class(TWSBitBtn)
    private
    protected
    public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; //override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl;
          const ALeft, ATop, AWidth, AHeight: integer); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWinCEWSSpeedButton }

  TWinCEWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;

procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; ButtonCaption: PWideChar);

implementation

uses WinCEInt, WinCEExtra;



{ TWinCEWSBitBtn }
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

  XPBitBtn_ImageIndexToState: array[1..6] of TButtonState = 
    (bsUp, bsExclusive, bsDown, bsDisabled, bsUp, bsUp);
  BitBtnEnabledToButtonState: array[boolean] of TButtonState =
    (bsDisabled, bsUp);

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
procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; ButtonCaption: PWideChar);
var
  BitmapHandle: HBITMAP; // Handle of the button glyph
  BitBtnLayout: TButtonLayout; // Layout of button and glyph
  BitBtnHandle: HWND; // Handle to bitbtn window
  BitBtnDC: HDC; // Handle to DC of bitbtn window
  OldFontHandle: HFONT; // Handle of previous font in hdcNewBitmap
  hdcNewBitmap: HDC; // Device context of the new Bitmap
  SrcDC, MaskDC: HDC;
  BitmapInfo: BITMAP; // Buffer for bitmap
  TextSize: Windows.SIZE; // For computing the length of button caption in pixels
  OldBitmap: HBITMAP; // Handle to the old selected bitmap
  NewBitmap: HBITMAP; // Handle of the new bitmap
  MaskBmp, OldSrcBmp, OldMaskBmp: HBITMAP;
  OldBitmapHandle: HBITMAP; // Handle of the provious bitmap in hdcNewBitmap
  XDestBitmap, YDestBitmap: integer; // X,Y coordinate of destination rectangle for bitmap
  XDestText, YDestText: integer; // X,Y coordinates of destination rectangle for caption
  newWidth, newHeight: integer; // dimensions of new combined bitmap
  srcWidth: integer; // width of glyph to use, bitmap may have multiple glyphs
  BitmapRect: Windows.RECT;
  ButtonImageList: BUTTON_IMAGELIST;
  I: integer;

  procedure DrawBitmap(AState: TButtonState);
  var
    MonoDC: HDC;
    MonoBmp, OldMonoBmp: HBITMAP;
    BkColor: TColorRef;

    TextFlags: integer; // flags for caption (enabled or disabled)
    numGlyphs, glyphLeft, glyphWidth, glyphHeight: integer;
    themesActive, emulateDisabled: boolean;
  begin
    emulateDisabled := false;
    glyphLeft := 0;
    glyphWidth := srcWidth;
    glyphHeight := BitmapInfo.bmHeight;
    TextFlags := DST_PREFIXTEXT;
    numGlyphs := BitBtn.NumGlyphs;
    case AState of
      bsDisabled:
      begin
        if numGlyphs > 1 then
          glyphLeft := glyphWidth
        else
          emulateDisabled := true;
        TextFlags := TextFlags or DSS_DISABLED;
      end;
      bsDown:      if numGlyphs > 2 then glyphLeft := 2*glyphWidth;
      bsExclusive: if numGlyphs > 3 then glyphLeft := 3*glyphWidth;
    end;

    // fill with background color
    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);
    Windows.FillRect(hdcNewBitmap, BitmapRect, BitBtn.Brush.Handle);
    if not emulateDisabled then
    begin
      if MaskBmp <> 0 then 
        BitBlt(hdcNewBitmap, XDestBitmap, YDestBitmap, glyphWidth, 
          glyphHeight, MaskDC, glyphLeft, 0, SRCCOPY);
    end else begin
      // when not themed, windows wants a white background picture for disabled button image
      ///themesActive := TWinCEWidgetSet(WidgetSet).ThemesActive;
      ///if not themesActive then
        FillRect(hdcNewBitmap, BitmapRect, GetStockObject(BLACK_BRUSH));
      if BitmapHandle <> 0 then 
      begin
        // Create a Mono DC
        MonoBmp := CreateBitmap(glyphWidth, glyphHeight, 1, 1, nil);
        MonoDC := CreateCompatibleDC(hdcNewBitmap);
        OldMonoBmp := SelectObject(MonoDC, MonoBmp);
        // Create the black and white image
        BkColor := SetBkColor(MaskDC, ColorToRGB(BitBtn.Brush.Color));
        BitBlt(MonoDC, 0, 0, glyphWidth, glyphHeight, MaskDC, glyphLeft, 0, SRCCOPY);
        SetBkColor(MaskDC, BkColor);
        {if themesActive then
        begin
          // non-themed winapi wants white/other as background/picture-disabled colors
          // themed winapi draws bitmap-as, with transparency defined by bitbtn.brush color
          BkColor := SetBkColor(hdcNewBitmap, ColorToRGB(BitBtn.Brush.Color));
          SetTextColor(hdcNewBitmap, GetSysColor(COLOR_BTNSHADOW));
        end;}
        // Draw the black and white image
        BitBlt(hdcNewBitmap, XDestBitmap, YDestBitmap, glyphWidth, glyphHeight,
               MonoDC, 0, 0, SRCCOPY);  
  
        SelectObject(MonoDC, OldMonoBmp);
        DeleteDC(MonoDC);
        DeleteObject(MonoBmp);
      end;  
    end;

    SetBkMode(hdcNewBitmap, TRANSPARENT);
    SetTextColor(hdcNewBitmap, 0);
    DrawState(hdcNewBitmap, 0, nil, LPARAM(ButtonCaption), 0, XDestText, YDestText, 0, 0, TextFlags);
    SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;
var
tmpWideText : PWideChar;
begin
  // gather info about bitbtn
  BitBtnHandle := BitBtn.Handle;
  if BitBtn.Glyph.Empty then
  begin
    BitmapHandle := 0;
    BitmapInfo.bmWidth := 0;
    BitmapInfo.bmHeight := 0;
    srcWidth := 0;
  end else begin
    BitmapHandle := BitBtn.Glyph.Handle;
    Windows.GetObject(BitmapHandle, sizeof(BitmapInfo), @BitmapInfo);
    srcWidth := BitmapInfo.bmWidth;
    if BitBtn.NumGlyphs > 1 then
      srcWidth := srcWidth div BitBtn.NumGlyphs;
  end;
  BitBtnLayout := BitBtn.Layout;
  BitBtnDC := GetDC(BitBtnHandle);
  hdcNewBitmap := CreateCompatibleDC(BitBtnDC);
  OldFontHandle := SelectObject(hdcNewBitmap, BitBtn.Font.Handle);
  tmpWideText := StringToPWideChar(ButtonCaption);
  GetTextExtentPoint32(hdcNewBitmap, tmpWideText, Length(ButtonCaption), @TextSize);
  FreeMem(tmpWideText);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      if BitBtn.Spacing = -1 then
        newWidth := BitBtn.Width - 10
      else
        newWidth := TextSize.cx + srcWidth + BitBtn.Spacing;
      if BitmapHandle <> 0 then
        inc(newWidth, 2);
      newHeight := TextSize.cy;
      if newHeight < BitmapInfo.bmHeight then
        newHeight := BitmapInfo.bmHeight;
      YDestBitmap := (newHeight - BitmapInfo.bmHeight) div 2;
      YDestText := (newHeight - TextSize.cy) div 2;
      case BitBtnLayout of
        blGlyphLeft: 
        begin
          XDestBitmap := 0;
          XDestText := srcWidth;
          if BitBtn.Spacing = -1 then
            inc(XDestText, (newWidth - srcWidth - TextSize.cx) div 2)
          else
            inc(XDestText, BitBtn.Spacing);
        end;
        blGlyphRight: 
        begin
          XDestBitmap := newWidth - srcWidth;
          XDestText := XDestBitmap - TextSize.cx;
          if BitBtn.Spacing = -1 then
            dec(XDestText, (newWidth - srcWidth - TextSize.cx) div 2)
          else
            dec(XDestText, BitBtn.Spacing);
        end;
      end;
    end;
    blGlyphTop, blGlyphBottom:
    begin
      newWidth := TextSize.cx;
      if newWidth < srcWidth then
        newWidth := srcWidth;
      if BitBtn.Spacing = -1 then
        newHeight := BitBtn.Height - 10
      else
        newHeight := TextSize.cy + BitmapInfo.bmHeight + BitBtn.Spacing;
      if BitmapHandle <> 0 then
        inc(newHeight, 2);
      XDestBitmap := (newWidth - srcWidth) shr 1;
      XDestText := (newWidth - TextSize.cx) shr 1;
      case BitBtnLayout of
        blGlyphTop: 
        begin
          YDestBitmap := 0;
          YDestText := BitmapInfo.bmHeight;
          if BitBtn.Spacing = -1 then
            inc(YDestText, (newHeight - BitmapInfo.bmHeight - TextSize.cy) div 2)
          else
            inc(YDestText, BitBtn.Spacing);
        end;
        blGlyphBottom: 
        begin
          YDestBitmap := newHeight - BitmapInfo.bmHeight;
          YDestText := YDestBitmap - TextSize.cy;
          if BitBtn.Spacing = -1 then
            dec(YDestText, (newHeight - BitmapInfo.bmHeight - TextSize.cy) div 2)
          else
            dec(YDestText, BitBtn.Spacing);
        end;
      end;
    end;
  end;

  // create new
  BitmapRect.left := 0;
  BitmapRect.top := 0;
  BitmapRect.right := newWidth;
  BitmapRect.bottom := newHeight;
  if (newWidth = 0) or (newHeight = 0) then
  begin
    NewBitmap := 0;
    MaskDC := 0;
    MaskBmp := 0;
  end else begin
    NewBitmap := CreateCompatibleBitmap(BitBtnDC, newWidth, newHeight);
    // prepare masked bitmap
    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);
    MaskDC := CreateCompatibleDC(hdcNewBitmap);
    if BitBtn.Glyph.MaskHandleAllocated then
    begin
      // Create a mask DC
      MaskBmp := CreateCompatibleBitmap(hdcNewBitmap, BitmapInfo.bmWidth, BitmapInfo.bmHeight);
      OldMaskBmp := SelectObject(MaskDC, MaskBmp);
      SrcDC := CreateCompatibleDC(hdcNewBitmap);
      OldSrcBmp := SelectObject(SrcDC, BitmapHandle);
      FillRect(MaskDC, BitmapRect, BitBtn.Brush.Handle);
      TWinCEWidgetSet(WidgetSet).MaskBlt(MaskDC, 0, 0, BitmapInfo.bmWidth, BitmapInfo.bmHeight, SrcDC,
        0, 0, BitBtn.Glyph.MaskHandle, 0, 0);
    end else begin
      MaskBmp := BitmapHandle;
      OldMaskBmp := SelectObject(MaskDC, MaskBmp);
    end;
    SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;
    
  // destroy previous bitmap, set new bitmap
  {if TWinCEWidgetSet(WidgetSet).ThemesActive then
  begin
    // winxp draws BM_SETIMAGE bitmap with old style button!
    // need to use BCM_SETIMAGELIST
    if Windows.SendMessage(BitBtnHandle, BCM_GETIMAGELIST, 0, LPARAM(@ButtonImageList)) <> 0 then
      if ButtonImageList.himl <> 0 then
        ImageList_Destroy(ButtonImageList.himl);
    if NewBitmap <> 0 then
    begin
      ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 5, 0);
      ButtonImageList.margin.left := 5;
      ButtonImageList.margin.right := 5;
      ButtonImageList.margin.top := 5;
      ButtonImageList.margin.bottom := 5;
      ButtonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER;
      // for some reason, if bitmap added to imagelist, need to redrawn, otherwise it's black!?
      for I := 1 to 6 do
      begin
        DrawBitmap(XPBitBtn_ImageIndexToState[I]);
        ImageList_AddMasked(ButtonImageList.himl, NewBitmap, ColorToRGB(BitBtn.Brush.Color));
      end;
    end else begin
      ButtonImageList.himl := 0;
    end;

    Windows.SendMessage(BitBtnHandle, BCM_SETIMAGELIST, 0, LPARAM(@ButtonImageList));
    if NewBitmap <> 0 then
      DeleteObject(NewBitmap);
  end else }begin
    OldBitmap := Windows.SendMessage(BitBtnHandle, BM_GETIMAGE, IMAGE_BITMAP, 0);
    if NewBitmap <> 0 then
      DrawBitmap(BitBtnEnabledToButtonState[BitBtn.Enabled]);
    Windows.SendMessage(BitBtnHandle, BM_SETIMAGE, IMAGE_BITMAP, NewBitmap);
    if OldBitmap <> 0 then
      DeleteObject(OldBitmap);
  end;
  SelectObject(hdcNewBitmap, OldFontHandle);
  DeleteDC(hdcNewBitmap);
  ReleaseDC(BitBtnHandle, BitBtnDC);
  if BitBtn.Glyph.MaskHandleAllocated then
  begin
    SelectObject(SrcDC, OldSrcBmp);
    DeleteDC(SrcDC);
    DeleteObject(MaskBmp);
  end;
  if MaskDC <> 0 then
  begin
    SelectObject(MaskDC, OldMaskBmp);
    DeleteDC(MaskDC);
  end;

  BitBtn.Invalidate;
end;


class function TWinCEWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
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


class procedure TWinCEWSBitBtn.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  BitmapInfo: BITMAP; // Buffer for bitmap
  BitBtn: TBitBtn absolute AWinControl;
  Glyph: TBitmap;
  spacing, srcWidth: integer;
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    Glyph := BitBtn.Glyph;
    if not Glyph.Empty then
    begin
      Windows.GetObject(Glyph.Handle, sizeof(BitmapInfo), @BitmapInfo);
      srcWidth := BitmapInfo.bmWidth;
      if BitBtn.NumGlyphs > 1 then
        srcWidth := srcWidth div BitBtn.NumGlyphs;
      if BitBtn.Spacing = -1 then
        spacing := 8
      else
        spacing := BitBtn.Spacing;
      if BitBtn.Layout in [blGlyphLeft, blGlyphRight] then
      begin
        Inc(PreferredWidth, spacing + srcWidth);
        if BitmapInfo.bmHeight > PreferredHeight then
          PreferredHeight := BitmapInfo.bmHeight;
      end else begin
        Inc(PreferredHeight, spacing + BitmapInfo.bmHeight);
        if srcWidth > PreferredWidth then
          PreferredWidth := srcWidth;
      end;
    end;
    Inc(PreferredWidth, 20);
    Inc(PreferredHeight, 12);
  end;
end;

class procedure TWinCEWSBitBtn.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: integer);
var
  pwCaption : PWideChar;
begin
  TWinCEWSWinControl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
  if TCustomBitBtn(AWinControl).Spacing = -1 then
    begin
      pwCaption := StringToPWideChar(AWinControl.Caption);
      DrawBitBtnImage(TCustomBitBtn(AWinControl), pwCaption);
      FreeMem(pwCaption);
    end;
end;

class procedure TWinCEWSBitBtn.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  pwCaption : PWideChar;
begin
  TWinCEWSWinControl.SetFont(AWinControl, AFont);
  pwCaption := StringToPWideChar(AWinControl.Caption);
  DrawBitBtnImage(TCustomBitBtn(AWinControl), pwCaption);
  FreeMem(pwCaption);
end;

class procedure TWinCEWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
var
  pwCaption : PWideChar;
begin
  pwCaption := StringToPWideChar(ABitBtn.Caption);
  DrawBitBtnImage(ABitBtn, pwCaption);
  FreeMem(pwCaption);
end;

class procedure TWinCEWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  pwCaption : PWideChar;
begin
  pwCaption := StringToPWideChar(ABitBtn.Caption);
  DrawBitBtnImage(ABitBtn, pwCaption);
  FreeMem(pwCaption);
end;

class procedure TWinCEWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  pwCaption : PWideChar;
begin
  pwCaption := StringToPWideChar(ABitBtn.Caption);
  DrawBitBtnImage(ABitBtn, pwCaption);
  FreeMem(pwCaption);
end;

class procedure TWinCEWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
var
  pwCaption : PWideChar;
begin
  pwCaption := StringToPWideChar(ABitBtn.Caption);
  DrawBitBtnImage(ABitBtn, pwCaption);
  FreeMem(pwCaption);
end;

class procedure TWinCEWSBitBtn.SetText(const AWinControl: TWinControl; const AText: string);
var
  pwCaption : PWideChar;
begin
  pwCaption := StringToPWideChar(AText);
  DrawBitBtnImage(TCustomBitBtn(AWinControl), pwCaption);
  FreeMem(pwCaption);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomBitBtn, TWinCEWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TWinCEWSSpeedButton);
////////////////////////////////////////////////////
end.
