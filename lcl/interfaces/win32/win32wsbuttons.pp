{ $Id$}
{
 *****************************************************************************
 *                             Win32WSButtons.pp                             *
 *                             -----------------                             *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Windows, CommCtrl, Classes, Buttons, Graphics, GraphType, Controls,
  LCLType, LCLMessageGlue, LMessages, LazUTF8, Themes, ImgList,
////////////////////////////////////////////////////
  WSProc, WSButtons, Win32WSControls, Win32WSImgList,
  UxTheme, Win32Themes;

type

  { TWin32WSBitBtn }

  TWin32WSBitBtn = class(TWSBitBtn)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl;
          const ALeft, ATop, AWidth, AHeight: integer); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
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
  Win32Int, Win32Proc;

type
  TBitBtnAceess = class(TCustomBitBtn)
  end;

{ TWin32WSBitBtn }

const
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

function Create32BitHBitmap(ADC: HDC; AWidth, AHeight: Integer; out BitsPtr: Pointer): HBitmap;
var
  Info: Windows.TBitmapInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  Info.bmiHeader.biWidth := AWidth;
  Info.bmiHeader.biHeight := -AHeight; // top down
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;
  BitsPtr := nil;
  Result := Windows.CreateDIBSection(ADC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, BitsPtr, 0, 0);
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
  ButtonCaptionW: widestring;
  AIndex: Integer;
  AImageRes: TScaledImageListResolution;
  AEffect: TGraphicsDrawEffect;

  procedure DrawBitmap(AState: TButtonState; UseThemes, AlphaDraw: Boolean);
  const
    DSS_HIDEPREFIX = $0200;
    StateToDetail: array[TButtonState] of TThemedButton =
    (
     { bsUp        } tbPushButtonNormal,
     { bsDisabled  } tbPushButtonDisabled,
     { bsDown      } tbPushButtonPressed,
     { bsExclusive } tbPushButtonPressed,
     { bsHot       } tbPushButtonHot
    );
  var
    TextFlags: DWord; // flags for caption (enabled or disabled)
    glyphWidth, glyphHeight: integer;
    OldBitmapHandle: HBITMAP; // Handle of the provious bitmap in hdcNewBitmap
    OldTextAlign: Integer;
    TmpDC: HDC;
    PaintBuffer: HPAINTBUFFER;
    Options: DTTOpts;
    Details: TThemedElementDetails;
    ShowAccel: Boolean;
    Color: TColor;
    PaintParams: TBP_PaintParams;
  begin
    glyphWidth := srcWidth;
    glyphHeight := srcHeight;

    if WindowsVersion >= wv2000 then
      ShowAccel := (SendMessage(BitBtnHandle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL) = 0
    else
      ShowAccel := True;

    OldBitmapHandle := SelectObject(hdcNewBitmap, NewBitmap);
    if UseThemes and AlphaDraw then
    begin
      FillChar(PaintParams, SizeOf(PaintParams), 0);
      PaintParams.cbSize := SizeOf(PaintParams);
      PaintParams.dwFlags := BPPF_ERASE;
      PaintBuffer := BeginBufferedPaint(hdcNewBitmap, @BitmapRect, BPBF_COMPOSITED, @PaintParams, TmpDC);
    end
    else
    begin
      TmpDC := hdcNewBitmap;
      PaintBuffer := 0;
    end;
    OldFontHandle := SelectObject(TmpDC, BitBtn.Font.Reference.Handle);
    OldTextAlign := GetTextAlign(TmpDC);

    // clear background:
    // for alpha bitmap clear it with $00000000 else make it solid color for
    // further masking
    if PaintBuffer = 0 then
    begin
      Windows.FillRect(TmpDC, BitmapRect, GetSysColorBrush(COLOR_BTNFACE));
      Color := BitBtn.Font.Color;
      if Color = clDefault then
        Color := BitBtn.GetDefaultColor(dctFont);
      SetTextColor(TmpDC, ColorToRGB(Color));
    end;

    if AState <> bsDisabled then
    begin
      if (srcWidth <> 0) and (srcHeight <> 0) then
      begin
        TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(AState, BitBtn.Font.PixelsPerInch, 1,
          AImageRes, AIndex, AEffect);
        TWin32WSCustomImageListResolution.DrawToDC(
          AImageRes.Resolution,
          AIndex, TmpDC, Rect(XDestBitmap, YDestBitmap, glyphWidth, glyphHeight),
          AImageRes.Resolution.ImageList.BkColor,
          AImageRes.Resolution.ImageList.BlendColor, AEffect,
          AImageRes.Resolution.ImageList.DrawingStyle,
          AImageRes.Resolution.ImageList.ImageType);
      end;
    end else
    begin
      // when not themed, windows wants a white background picture for disabled button image
      if not UseThemes then
        FillRect(TmpDC, BitmapRect, GetStockObject(WHITE_BRUSH));

      if (srcWidth <> 0) and (srcHeight <> 0) then
      begin
        TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(AState, BitBtn.Font.PixelsPerInch, 1,
          AImageRes, AIndex, AEffect);
        if UseThemes and not AlphaDraw then
        begin
          // non-themed winapi wants white/other as background/picture-disabled colors
          // themed winapi draws bitmap-as, with transparency defined by bitbtn.brush color
          SetBkColor(TmpDC, GetSysColor(COLOR_BTNFACE));
          SetTextColor(TmpDC, GetSysColor(COLOR_BTNSHADOW));
        end
        else
        if (AEffect = gdeDisabled) and not AlphaDraw then
          AEffect := gde1Bit;

        TWin32WSCustomImageListResolution.DrawToDC(
          AImageRes.Resolution,
          AIndex, TmpDC, Rect(XDestBitmap, YDestBitmap, glyphWidth, glyphHeight),
          AImageRes.Resolution.ImageList.BkColor,
          AImageRes.Resolution.ImageList.BlendColor, AEffect,
          AImageRes.Resolution.ImageList.DrawingStyle,
          AImageRes.Resolution.ImageList.ImageType);
      end;
    end;
    if PaintBuffer = 0 then
    begin
      TextFlags := DST_PREFIXTEXT;

      if (AState = bsDisabled) then
        TextFlags := TextFlags or DSS_DISABLED;

      if not ShowAccel then
        TextFlags := TextFlags or DSS_HIDEPREFIX;

      SetBkMode(TmpDC, TRANSPARENT);
      if BitBtn.UseRightToLeftReading then
        SetTextAlign(TmpDC, OldTextAlign or TA_RTLREADING);
      ButtonCaptionW := UTF8ToUTF16(ButtonCaption);
      DrawStateW(TmpDC, 0, nil, LPARAM(ButtonCaptionW), 0, XDestText, YDestText, 0, 0, TextFlags);
    end
    else
    begin
      Details := ThemeServices.GetElementDetails(StateToDetail[AState]);
      FillChar(Options, SizeOf(Options), 0);
      Options.dwSize := SizeOf(Options);
      Options.dwFlags := DTT_COMPOSITED;
      TextFlags := DT_SINGLELINE;
      if not ShowAccel then
        TextFlags := TextFlags or DT_HIDEPREFIX;
      if AState <> bsDisabled then
      begin
        // change color to requested or it will be black
        Color := BitBtn.Font.Color;
        if Color = clDefault then
          Color := BitBtn.GetDefaultColor(dctFont);
        Options.crText := ThemeServices.ColorToRGB(Color, @Details);
        Options.dwFlags := Options.dwFlags or DTT_TEXTCOLOR;
      end;
      TWin32ThemeServices(ThemeServices).DrawTextEx(TmpDC, Details, ButtonCaption,
        Rect(XDestText, YDestText, XDestText + TextSize.cx, YDestText + TextSize.cy),
        TextFlags, @Options);
    end;
    SetTextAlign(TmpDC, OldTextAlign);
    SelectObject(TmpDC, OldFontHandle);
    if PaintBuffer <> 0 then
      EndBufferedPaint(PaintBuffer, True);
    NewBitmap := SelectObject(hdcNewBitmap, OldBitmapHandle);
  end;

var
  RGBA: PRGBAQuad;
  AlphaDraw: Boolean;
  ASpacing: Integer;
  lMargin: Integer;
begin
  // gather info about bitbtn
  BitBtnHandle := BitBtn.Handle;
  ASpacing := BitBtn.Spacing;
  if BitBtn.Margin = -1 then lMargin := 0 else lMargin := BitBtn.Margin;

  if BitBtn.CanShowGlyph(True) then
  begin
    TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(Low(TButtonState), BitBtn.Font.PixelsPerInch, 1,
      AImageRes, AIndex, AEffect);
    srcWidth := AImageRes.Width;
    srcHeight := AImageRes.Height;
  end else
  begin
    srcWidth := 0;
    srcHeight := 0;
  end;
  {set spacing to LCL's default if bitbtn does not have glyph.issue #23255}
  if (srcWidth = 0) or (srcHeight = 0) then
    ASpacing := 0;
  newWidth := 0;
  newHeight := 0;
  BitBtnLayout := BidiAdjustButtonLayout(BitBtn.UseRightToLeftReading, BitBtn.Layout);
  BitBtnDC := GetDC(BitBtnHandle);
  hdcNewBitmap := CreateCompatibleDC(BitBtnDC);
  MeasureText(BitBtn, ButtonCaption, TextSize.cx, TextSize.cy);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      if ASpacing = -1 then
        newWidth := BitBtn.Width
      else
        newWidth := TextSize.cx + srcWidth + ASpacing + lMargin;
      newHeight := TextSize.cy;
      if newHeight < srcHeight then
        newHeight := srcHeight;
      YDestBitmap := (newHeight - srcHeight) div 2;
      YDestText := (newHeight - TextSize.cy) div 2;
      case BitBtnLayout of
        blGlyphLeft:
        begin
          XDestBitmap := lMargin;
          XDestText := srcWidth;
          if ASpacing = -1 then begin
            if BitBtn.Margin = -1 then begin
              XDestBitmap := (BitBtn.Width - (srcWidth + TextSize.cx)) div 3;
              XDestText := 2*XDestBitmap + srcWidth;
            end else
              inc(XDestText, (newWidth - srcWidth - TextSize.cx + lMargin) div 2);
          end else
            inc(XDestText, ASpacing + lMargin);
        end;
        blGlyphRight:
        begin
          XDestBitmap := newWidth - srcWidth - lMargin;
          XDestText := XDestBitmap - TextSize.cx;
          if ASpacing = -1 then begin
            if BitBtn.Margin = -1 then begin
              XDestText := (BitBtn.Width - (srcWidth + TextSize.cx)) div 3;
              XDestBitmap := 2 * XDestText + TextSize.cx;
            end else
              dec(XDestText, (newWidth - srcWidth - TextSize.cx - lMargin) div 2)
          end else
            dec(XDestText, ASpacing);
        end;
      end;
    end;
    blGlyphTop, blGlyphBottom:
    begin
      newWidth := TextSize.cx;
      if newWidth < srcWidth then
        newWidth := srcWidth;
      if ASpacing = -1 then
        newHeight := BitBtn.Height
      else
        newHeight := TextSize.cy + srcHeight + ASpacing + lMargin;
      XDestBitmap := (newWidth - srcWidth) shr 1;
      XDestText := (newWidth - TextSize.cx) shr 1;
      case BitBtnLayout of
        blGlyphTop:
        begin
          YDestBitmap := lMargin;
          YDestText := srcHeight;
          if ASpacing = -1 then begin
            if BitBtn.Margin = -1 then begin
              YDestBitmap := (BitBtn.Height - (srcHeight + TextSize.cy)) div 3;
              YDestText := 2*YDestBitmap + srcHeight;
            end else
              inc(YDestText, (newHeight - srcHeight - TextSize.cy + lMargin) div 2)
          end else
            inc(YDestText, ASpacing + lMargin);
        end;
        blGlyphBottom:
        begin
          YDestBitmap := newHeight - srcHeight - lMargin;
          YDestText := YDestBitmap - TextSize.cy;
          if ASpacing = -1 then begin
            if BitBtn.Margin = -1 then begin
              YDestText := (BitBtn.Height - (srcHeight + TextSize.cy)) div 3;
              YDestBitmap := 2 * YDestText + TextSize.cy;
            end else
              dec(YDestText, (newHeight - srcHeight - TextSize.cy - lMargin) div 2)
          end else
            dec(YDestText, ASpacing);
        end;
      end;
    end;
  end;

  // create new
  BitmapRect.left := 0;
  BitmapRect.top := 0;
  BitmapRect.right := newWidth;
  BitmapRect.bottom := newHeight;

  AlphaDraw := ThemeServices.ThemesEnabled and (BeginBufferedPaint <> nil);

  if (newWidth = 0) or (newHeight = 0) then
    NewBitmap := 0
  else
  if AlphaDraw then
    NewBitmap := Create32BitHBitmap(BitBtnDC, newWidth, newHeight, RGBA)
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
        if AlphaDraw then
          ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLOR32, 5, 0)
        else
          ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 5, 0)
      else
        ButtonImageList.himl := ImageList_Create(newWidth, newHeight, ILC_COLORDDB or ILC_MASK, 1, 0);
      ButtonImageList.margin.left := 0; //5;
      ButtonImageList.margin.right := 0; //5;
      ButtonImageList.margin.top := 0; //5;
      ButtonImageList.margin.bottom := 0; //5;
      if (BitBtn.Margin = -1) then
        ButtonImageList.uAlign := BUTTON_IMAGELIST_ALIGN_CENTER
      else
        ButtonImageList.uAlign := ord(BitBtnLayout);
      // if themes are enabled then we need to fill all state bitmaps,
      // else fill only current state bitmap
      if ThemeServices.ThemesEnabled then
      begin
        for I := 1 to 6 do
        begin
          DrawBitmap(XPBitBtn_ImageIndexToState[I], True, AlphaDraw);
          if AlphaDraw then
            ImageList_Add(ButtonImageList.himl, NewBitmap, 0)
          else
            ImageList_AddMasked(ButtonImageList.himl, NewBitmap, GetSysColor(COLOR_BTNFACE));
        end;
      end
      else
      begin
        DrawBitmap(BitBtnEnabledToButtonState[IsWindowEnabled(BitBtnHandle) or (csDesigning in BitBtn.ComponentState)], True, False);
        ImageList_AddMasked(ButtonImageList.himl, NewBitmap, GetSysColor(COLOR_BTNFACE));
      end;
      if NewBitmap <> 0 then
        DeleteObject(NewBitmap);
    end
    else
    begin
      ButtonImageList.himl := 0;
    end;
    Windows.SendMessage(BitBtnHandle, BCM_SETIMAGELIST, 0, LPARAM(@ButtonImageList));
  end else
  begin
    OldBitmap := HBITMAP(Windows.SendMessage(BitBtnHandle, BM_GETIMAGE, IMAGE_BITMAP, 0));
    if NewBitmap <> 0 then
      DrawBitmap(BitBtnEnabledToButtonState[IsWindowEnabled(BitBtnHandle) or (csDesigning in BitBtn.ComponentState)], False, False);
    Windows.SendMessage(BitBtnHandle, BM_SETIMAGE, IMAGE_BITMAP, LPARAM(NewBitmap));
    if OldBitmap <> 0 then
      DeleteObject(OldBitmap);
  end;
  DeleteDC(hdcNewBitmap);
 { if NewBitmap <> 0 then
      DeleteObject(NewBitmap);}
  ReleaseDC(BitBtnHandle, BitBtnDC);
  BitBtn.Invalidate;
end;

function BitBtnWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWin32WindowInfo;
  Control: TWinControl;
  ButtonImageList: BUTTON_IMAGELIST;
  ImageList: HIMAGELIST;
  OldBitmap: HBITMAP;
  LMessage: TLMessage;
begin
  Info := GetWin32WindowInfo(Window);
  if (Info = nil) or (Info^.WinControl = nil) then
  begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    Exit;
  end
  else
    Control := Info^.WinControl;

  case Msg of
    WM_DESTROY:
      begin
        if Assigned(ThemeServices) and ThemeServices.ThemesAvailable and
           (Windows.SendMessage(Window, BCM_GETIMAGELIST, 0, Windows.LPARAM(@ButtonImageList)) <> 0) then
        begin
          // delete and destroy button imagelist
          if ButtonImageList.himl <> 0 then
          begin
            ImageList:=ButtonImageList.himl;
            ButtonImageList.himl:=0;
            Windows.SendMessage(Window, BCM_SETIMAGELIST, 0, Windows.LPARAM(@ButtonImageList));
            ImageList_Destroy(ImageList);
          end;
        end else
        begin
          OldBitmap := HBITMAP(Windows.SendMessage(Window, BM_GETIMAGE, IMAGE_BITMAP, 0));
          if OldBitmap <> 0 then
            DeleteObject(OldBitmap);
        end;
        Result := WindowProc(Window, Msg, WParam, LParam);
      end;
    WM_GETFONT:
      begin
        Result := LResult(Control.Font.Reference.Handle);
      end;
    WM_UPDATEUISTATE:
      begin
        Result := WindowProc(Window, Msg, WParam, LParam);
        DrawBitBtnImage(TBitBtn(Control), TBitBtn(Control).Caption);
      end;
    WM_PAINT,
    WM_ERASEBKGND:
      begin
        if not Control.DoubleBuffered then
        begin
          LMessage.msg := Msg;
          LMessage.wParam := WParam;
          LMessage.lParam := LParam;
          LMessage.Result := 0;
          Result := DeliverMessage(Control, LMessage);
        end
        else
          Result := WindowProc(Window, Msg, WParam, LParam);
      end;
    WM_PRINTCLIENT:
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
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
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName[0];
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
  BitBtn: TBitBtn absolute AWinControl;
  spacing, srcWidth, AIndex: integer;
  AImageRes: TScaledImageListResolution;
  AEffect: TGraphicsDrawEffect;
begin
  if MeasureText(AWinControl, AWinControl.Caption, PreferredWidth, PreferredHeight) then
  begin
    if BitBtn.CanShowGlyph(True) then
    begin
      TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(Low(TButtonState), BitBtn.Font.PixelsPerInch, 1,
        AImageRes, AIndex, AEffect);
      srcWidth := AImageRes.Width;
      if BitBtn.Spacing = -1 then
        spacing := 8
      else
        spacing := BitBtn.Spacing;
      if BitBtn.Layout in [blGlyphLeft, blGlyphRight] then
      begin
        Inc(PreferredWidth, spacing + srcWidth);
        if AImageRes.Height > PreferredHeight then
          PreferredHeight := AImageRes.Height;
      end else begin
        Inc(PreferredHeight, spacing + AImageRes.Height);
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

class procedure TWin32WSBitBtn.SetBiDiMode(const AWinControl: TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar: Boolean);
begin
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
//  TWin32WSWinControl.SetText(AWinControl, AText);
  DrawBitBtnImage(TCustomBitBtn(AWinControl), AText);
end;

end.
