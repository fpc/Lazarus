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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  CommCtrl, Windows, Classes, Buttons, Graphics, GraphType, Controls,
////////////////////////////////////////////////////
  WSProc, WSControls, WSButtons, WSLCLClasses,
  Win32WSControls, Win32WSImgList, LCLType, LCLProc, Themes;

type

  { TWin32WSBitBtn }

  TWin32WSBitBtn = class(TWSBitBtn)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl;
          const ALeft, ATop, AWidth, AHeight: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWin32WSSpeedButton }

  TWin32WSSpeedButton = class(TWSSpeedButton)
  published
  end;

procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; const ButtonCaption: string);

implementation

uses
  Win32Int, InterfaceBase, Win32Proc;

type
  TBitBtnAceess = class(TCustomBitBtn)
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

    Image 0 = NORMAL
    Image 1 = HOT
    Image 2 = PRESSED
    Image 3 = DISABLED
    Image 4 = DEFAULTED
    Image 5 = STYLUSHOT - for tablet computers
  }

  XPBitBtn_ImageIndexToState: array[1..6] of TButtonState =
    (bsUp, bsHot, bsDown, bsDisabled, bsUp, bsHot);
  BitBtnEnabledToButtonState: array[boolean] of TButtonState =
    (bsDisabled, bsUp);

type
  BUTTON_IMAGELIST = record
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
procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; const ButtonCaption: string);
var
  BitBtnLayout: TButtonLayout; // Layout of button and glyph
  BitBtnHandle: HWND; // Handle to bitbtn window
  BitBtnDC: HDC; // Handle to DC of bitbtn window
  OldFontHandle: HFONT; // Handle of previous font in hdcNewBitmap
  hdcNewBitmap: HDC; // Device context of the new Bitmap
  TextSize: Windows.SIZE; // For computing the length of button caption in pixels
  OldBitmap: HBITMAP; // Handle to the old selected bitmap
  NewBitmap: HBITMAP; // Handle of the new bitmap
  XDestBitmap, YDestBitmap: integer; // X,Y coordinate of destination rectangle for bitmap
  XDestText, YDestText: integer; // X,Y coordinates of destination rectangle for caption
  newWidth, newHeight: integer; // dimensions of new combined bitmap
  srcWidth, srcHeight: integer; // width of glyph to use, bitmap may have multiple glyphs
  BitmapRect: Windows.RECT;
  ButtonImageList: BUTTON_IMAGELIST;
  I: integer;
  {$IFDEF WindowsUnicodeSupport}
  ButtonCaptionA: string;
  ButtonCaptionW: widestring;
  {$ENDIF}

  procedure DrawBitmap(AState: TButtonState; UseThemes: Boolean);
  const
    ButtonsDetail: array[TButtonState] of TThemedButton = (
     { bsUp        } tbPushButtonNormal,
     { bsDisabled  } tbPushButtonDisabled,
     { bsDown      } tbPushButtonPressed,
     { bsExclusive } tbPushButtonNormal,
     { bsHot       } tbPushButtonHot
    );

  var
    TextFlags: integer; // flags for caption (enabled or disabled)
    glyphWidth, glyphHeight: integer;
    OldBitmapHandle: HBITMAP; // Handle of the provious bitmap in hdcNewBitmap
    AIndex: Integer;
    AEffect: TGraphicsDrawEffect;
    Details: TThemedElementDetails;
  begin
    glyphWidth := srcWidth;
    glyphHeight := srcHeight;
  { TODO: if we want to handle properly Windows settings about focus rect and
    keyboard accelerator drawing we need to query control state using this method
    if SendMessage(BitBtnHandle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL <> 0 then
  }
    TextFlags := DST_PREFIXTEXT;

    if not UseThemes and (AState = bsDisabled) then
      TextFlags := TextFlags or DSS_DISABLED;

    // fill with background color
    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);

    // dont use BitBtn.Brush.Reference.Handle - since button is painted with BtnFace color and
    // only glyph will have that bg - this will look very ugly

    Windows.FillRect(hdcNewBitmap, BitmapRect, GetSysColorBrush(COLOR_BTNFACE));
    SetTextColor(hdcNewBitmap, ColorToRGB(BitBtn.Font.Color));
    if AState <> bsDisabled then
    begin
      if (srcWidth <> 0) and (srcHeight <> 0) then
      begin
        TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(AState, AIndex, AEffect);
        TWin32WSCustomImageList.DrawToDC(TBitBtnAceess(BitBtn).FButtonGlyph.Images, AIndex,
          hdcNewBitmap, Rect(XDestBitmap, YDestBitmap, glyphWidth, glyphHeight),
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.BkColor,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.BlendColor, AEffect,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.DrawingStyle,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.ImageType);
      end;
    end else
    begin
      // when not themed, windows wants a white background picture for disabled button image
      if not UseThemes then
        FillRect(hdcNewBitmap, BitmapRect, GetStockObject(WHITE_BRUSH));

      if (srcWidth <> 0) and (srcHeight <> 0) then
      begin
        TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(AState, AIndex, AEffect);
        if UseThemes then
        begin
          // non-themed winapi wants white/other as background/picture-disabled colors
          // themed winapi draws bitmap-as, with transparency defined by bitbtn.brush color
          SetBkColor(hdcNewBitmap, GetSysColor(COLOR_BTNFACE));
          SetTextColor(hdcNewBitmap, GetSysColor(COLOR_BTNSHADOW));
        end
        else
        if AEffect = gdeDisabled then
          AEffect := gde1Bit;

        TWin32WSCustomImageList.DrawToDC(TBitBtnAceess(BitBtn).FButtonGlyph.Images, AIndex,
          hdcNewBitmap, Rect(XDestBitmap, YDestBitmap, glyphWidth, glyphHeight),
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.BkColor,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.BlendColor, AEffect,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.DrawingStyle,
          TBitBtnAceess(BitBtn).FButtonGlyph.Images.ImageType);
      end;
    end;
    SetBkMode(hdcNewBitmap, TRANSPARENT);
    if not UseThemes then
    begin
      {$IFDEF WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        ButtonCaptionW := UTF8ToUTF16(ButtonCaption);
        DrawStateW(hdcNewBitmap, 0, nil, LPARAM(ButtonCaptionW), 0, XDestText, YDestText, 0, 0, TextFlags);
      end
      else begin
        ButtonCaptionA := Utf8ToAnsi(ButtonCaption);
        DrawState(hdcNewBitmap, 0, nil, LPARAM(ButtonCaptionA), 0, XDestText, YDestText, 0, 0, TextFlags);
      end;
      {$ELSE}
      DrawState(hdcNewBitmap, 0, nil, LPARAM(ButtonCaption), 0, XDestText, YDestText, 0, 0, TextFlags);
      {$ENDIF}
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(ButtonsDetail[AState]);
      ThemeServices.DrawText(hdcNewBitmap, Details, ButtonCaption,
        Rect(XDestText, YDestText, XDestText + TextSize.cx, YDestText + TextSize.cy), 0, 0);
    end;
    SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;

begin
  // gather info about bitbtn
  BitBtnHandle := BitBtn.Handle;
  if BitBtn.CanShowGlyph then
  begin
    srcWidth := BitBtn.Glyph.Width;
    srcHeight := BitBtn.Glyph.Height;
    if BitBtn.NumGlyphs > 1 then
      srcWidth := srcWidth div BitBtn.NumGlyphs;
  end else
  begin
    srcWidth := 0;
    srcHeight := 0;
  end;
  BitBtnLayout := BitBtn.Layout;
  BitBtnDC := GetDC(BitBtnHandle);
  hdcNewBitmap := CreateCompatibleDC(BitBtnDC);
  OldFontHandle := SelectObject(hdcNewBitmap, BitBtn.Font.Reference.Handle);
  MeasureText(BitBtn, ButtonCaption, TextSize.cx, TextSize.cy);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      if BitBtn.Spacing = -1 then
        newWidth := BitBtn.Width - 10
      else
        newWidth := TextSize.cx + srcWidth + BitBtn.Spacing;
      newHeight := TextSize.cy;
      if newHeight < srcHeight then
        newHeight := srcHeight;
      YDestBitmap := (newHeight - srcHeight) div 2;
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
        newHeight := TextSize.cy + srcHeight + BitBtn.Spacing;
      XDestBitmap := (newWidth - srcWidth) shr 1;
      XDestText := (newWidth - TextSize.cx) shr 1;
      case BitBtnLayout of
        blGlyphTop:
        begin
          YDestBitmap := 0;
          YDestText := srcHeight;
          if BitBtn.Spacing = -1 then
            inc(YDestText, (newHeight - srcHeight - TextSize.cy) div 2)
          else
            inc(YDestText, BitBtn.Spacing);
        end;
        blGlyphBottom:
        begin
          YDestBitmap := newHeight - srcHeight;
          YDestText := YDestBitmap - TextSize.cy;
          if BitBtn.Spacing = -1 then
            dec(YDestText, (newHeight - srcHeight - TextSize.cy) div 2)
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
    NewBitmap := 0
  else
    NewBitmap := CreateCompatibleBitmap(BitBtnDC, newWidth, newHeight);

  // if new api availble then use it
  if ThemeServices.ThemesAvailable and
     (Windows.SendMessage(BitBtnHandle, BCM_GETIMAGELIST, 0, LPARAM(@ButtonImageList)) <> 0) then
  begin
    // destroy previous bitmap, set new bitmap
    if ButtonImageList.himl <> 0 then
      ImageList_Destroy(ButtonImageList.himl);
    if NewBitmap <> 0 then
    begin
      if ThemeServices.ThemesEnabled then
        ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 5, 0)
      else
        ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 1, 0);
      ButtonImageList.margin.left := 5;
      ButtonImageList.margin.right := 5;
      ButtonImageList.margin.top := 5;
      ButtonImageList.margin.bottom := 5;
      ButtonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER;
      // if themes are not enabled then we need to fill only one state bitmap, else
      // fill all bitmas
      if ThemeServices.ThemesEnabled then
      begin
        for I := 1 to 6 do
        begin
          DrawBitmap(XPBitBtn_ImageIndexToState[I], True);
          ImageList_AddMasked(ButtonImageList.himl, NewBitmap, GetSysColor(COLOR_BTNFACE));
        end;
      end
      else
      begin
        DrawBitmap(BitBtnEnabledToButtonState[IsWindowEnabled(BitBtnHandle) or (csDesigning in BitBtn.ComponentState)], True);
        ImageList_AddMasked(ButtonImageList.himl, NewBitmap, GetSysColor(COLOR_BTNFACE));
      end;
    end
    else
    begin
      ButtonImageList.himl := 0;
    end;
    Windows.SendMessage(BitBtnHandle, BCM_SETIMAGELIST, 0, LPARAM(@ButtonImageList));
    if NewBitmap <> 0 then
      DeleteObject(NewBitmap);
  end else
  begin
    OldBitmap := HBITMAP(Windows.SendMessage(BitBtnHandle, BM_GETIMAGE, IMAGE_BITMAP, 0));
    if NewBitmap <> 0 then
      DrawBitmap(BitBtnEnabledToButtonState[IsWindowEnabled(BitBtnHandle) or (csDesigning in BitBtn.ComponentState)], False);
    Windows.SendMessage(BitBtnHandle, BM_SETIMAGE, IMAGE_BITMAP, LPARAM(NewBitmap));
    if OldBitmap <> 0 then
      DeleteObject(OldBitmap);
  end;
  SelectObject(hdcNewBitmap, OldFontHandle);
  DeleteDC(hdcNewBitmap);
  ReleaseDC(BitBtnHandle, BitBtnDC);
  BitBtn.Invalidate;
end;

function BitBtnWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWindowInfo;
  Control: TWinControl;
begin
  Info := GetWindowInfo(Window);
  if (Info = nil) or (Info^.WinControl = nil) then
  begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    Exit;
  end
  else
    Control := Info^.WinControl;

  case Msg of
    WM_GETFONT:
      begin
        Result := LResult(Control.Font.Reference.Handle);
      end;
    else
      Result := WindowProc(Window, Msg, WParam, LParam);
  end;
end;


class function TWin32WSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
    if TCustomBitBtn(AWinControl).Default Then
      Flags := Flags or BS_DEFPUSHBUTTON
    else
      Flags := Flags or BS_PUSHBUTTON;
    Flags := Flags or BS_BITMAP;
    WindowTitle := '';
    SubClassWndProc := @BitBtnWndProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSBitBtn.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  BitmapInfo: BITMAP; // Buffer for bitmap
  BitBtn: TBitBtn absolute AWinControl;
  Glyph: TBitmap;
  spacing, srcWidth: integer;
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    if BitBtn.CanShowGlyph then
    begin
      Glyph := BitBtn.Glyph;
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
    Inc(PreferredHeight, 4);
    if WithThemeSpace then
    begin
      Inc(PreferredWidth, 6);
      Inc(PreferredHeight, 6);
    end;
  end;
end;

class procedure TWin32WSBitBtn.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: integer);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds') then Exit;
  TWin32WSWinControl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
  if TCustomBitBtn(AWinControl).Spacing = -1 then
    DrawBitBtnImage(TCustomBitBtn(AWinControl), AWinControl.Caption);
end;

class procedure TWin32WSBitBtn.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  TWin32WSWinControl.SetColor(AWinControl);
  DrawBitBtnImage(TCustomBitBtn(AWinControl), AWinControl.Caption);
end;

class procedure TWin32WSBitBtn.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then Exit;
  TWin32WSWinControl.SetFont(AWinControl, AFont);
  DrawBitBtnImage(TCustomBitBtn(AWinControl), AWinControl.Caption);
end;

class procedure TWin32WSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then Exit;
  DrawBitBtnImage(ABitBtn, ABitBtn.Caption);
end;

class procedure TWin32WSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then Exit;
  DrawBitBtnImage(ABitBtn, ABitBtn.Caption);
end;

class procedure TWin32WSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin') then Exit;
  DrawBitBtnImage(ABitBtn, ABitBtn.Caption);
end;

class procedure TWin32WSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing') then Exit;
  DrawBitBtnImage(ABitBtn, ABitBtn.Caption);
end;

class procedure TWin32WSBitBtn.SetText(const AWinControl: TWinControl; const AText: string);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;
  DrawBitBtnImage(TCustomBitBtn(AWinControl), AText);
end;

end.
