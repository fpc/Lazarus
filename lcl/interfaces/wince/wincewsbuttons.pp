{ $Id: WinCEwsbuttons.pp 8815 2006-02-24 13:31:16Z mattias $}
{
 *****************************************************************************
 *                              WinCEWSButtons.pp                            *
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WinCEWSButtons;

{$mode delphi}{$H+}

interface

uses
  // Libs
  Windows,
  // RTL
  SysUtils, Classes,
  // LCL
  Controls, Buttons, Graphics, GraphType, LCLType, LCLProc, LazUTF8,
  // Widgetset
  WSButtons, WSLCLClasses, WinCEWSControls, WinCEWSImgList, WinCEProc;

type
  { TWinCEWSBitBtn }

  TWinCEWSBitBtn = class(TWSBitBtn)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
          var PreferredWidth, PreferredHeight: integer;
          WithThemeSpace: Boolean); override;
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

procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; DrawStruct: PDrawItemStruct);

implementation

uses ImgList, WinCEInt, WinCEExtra;

type
  TBitBtnAceess = class(TCustomBitBtn)
  end;

const
  ButtonStateToFrameState: array[TButtonState] of UInt =
  (
{ bsUp        } DFCS_BUTTONPUSH,
{ bsDisabled  } DFCS_BUTTONPUSH or DFCS_INACTIVE,
{ bsDown      } DFCS_BUTTONPUSH or DFCS_PUSHED,
{ bsExclusive } DFCS_BUTTONPUSH,
{ bsHot       } DFCS_BUTTONPUSH
  );

{ TWinCEWSBitBtn }

{------------------------------------------------------------------------------
  Method: DrawBitBtnImage
  Params:  BitBtn: The TCustomBitBtn to update the image of
           ButtonCaption: new button caption
  Returns: Nothing

  Updates the button image combining the glyph and caption
 ------------------------------------------------------------------------------}
procedure DrawBitBtnImage(BitBtn: TCustomBitBtn; DrawStruct: PDrawItemStruct);
var
  BitBtnLayout: TButtonLayout; // Layout of button and glyph
  OldFontHandle: HFONT; // Handle of previous font in hdcNewBitmap
  TextSize: Windows.SIZE; // For computing the length of button caption in pixels
  XDestBitmap, YDestBitmap: integer; // X,Y coordinate of destination rectangle for bitmap
  XDestText, YDestText: integer; // X,Y coordinates of destination rectangle for caption
  newWidth, newHeight: integer; // dimensions of new combined bitmap
  srcWidth, srcHeight: integer; // width of glyph to use, bitmap may have multiple glyphs
  DrawRect: TRect;
  ButtonCaption: PWideChar;
  ButtonState: TButtonState;
  AImageRes: TScaledImageListResolution;

  procedure DrawBitmap;
  var
    TextFlags: integer; // flags for caption (enabled or disabled)
    w, h: integer;
    AIndex: Integer;
    AEffect: TGraphicsDrawEffect;
  begin
    TextFlags := DST_PREFIXTEXT;
    if ButtonState = bsDisabled then
      TextFlags := TextFlags or DSS_DISABLED;

    // fill with background color

    if (srcWidth <> 0) and (srcHeight <> 0) then
    begin
      TBitBtnAceess(BitBtn).FButtonGlyph.GetImageIndexAndEffect(ButtonState, BitBtn.Font.PixelsPerInch, 1,
        AImageRes, AIndex, AEffect);

      
      w := TBitBtnAceess(BitBtn).FButtonGlyph.Images.Width;
      h := TBitBtnAceess(BitBtn).FButtonGlyph.Images.Height;

      TWinCEWSCustomImageListResolution.DrawToDC(AImageRes.Resolution, AIndex,
        DrawStruct^._hDC, Rect(XDestBitmap, YDestBitmap, w, h),
        TBitBtnAceess(BitBtn).FButtonGlyph.Images.BkColor,
        TBitBtnAceess(BitBtn).FButtonGlyph.Images.BlendColor, AEffect,
        TBitBtnAceess(BitBtn).FButtonGlyph.Images.DrawingStyle,
        TBitBtnAceess(BitBtn).FButtonGlyph.Images.ImageType);
    end;
    SetBkMode(DrawStruct^._hDC, TRANSPARENT);
    if ButtonState = bsDown then
      SetTextColor(DrawStruct^._hDC, $FFFFFF)
    else
      SetTextColor(DrawStruct^._hDC, 0);
    DrawState(DrawStruct^._hDC, 0, nil, LPARAM(ButtonCaption), 0, XDestText, YDestText, 0, 0, TextFlags);
  end;

begin
  DrawRect := DrawStruct^.rcItem;

  if DrawStruct^.itemState and ODS_DISABLED <> 0 then
    ButtonState := bsDisabled
  else
  if DrawStruct^.itemState and (ODS_FOCUS or ODS_SELECTED) = (ODS_FOCUS or ODS_SELECTED) then
    ButtonState := bsDown
  else
  if DrawStruct^.itemState and ODS_FOCUS = ODS_FOCUS then
    ButtonState := bsHot
  else
    ButtonState := bsUp;

  DrawFrameControl(DrawStruct^._hDC, DrawRect, DFC_BUTTON, ButtonStateToFrameState[ButtonState]);

  // DFCS_ADJUSTRECT doesnot work
  InflateRect(DrawRect, -4, -4);
  
  ButtonCaption := PWideChar(UTF8Decode(BitBtn.Caption));

  // gather info about bitbtn
  if BitBtn.CanShowGlyph then
  begin
    srcWidth := TBitBtnAceess(BitBtn).FButtonGlyph.Images.Width;
    srcHeight := TBitBtnAceess(BitBtn).FButtonGlyph.Images.Height;
  end else
  begin
    srcWidth := 0;
    srcHeight := 0;
  end;
  BitBtnLayout := BitBtn.Layout;

  OldFontHandle := SelectObject(DrawStruct^._hDC, BitBtn.Font.Reference.Handle);
  GetTextExtentPoint32W(DrawStruct^._hDC, ButtonCaption, Length(BitBtn.Caption), TextSize);
  // calculate size of new bitmap
  case BitBtnLayout of
    blGlyphLeft, blGlyphRight:
    begin
      YDestBitmap := (DrawRect.Bottom + DrawRect.Top - srcHeight) div 2;
      YDestText := (DrawRect.Bottom + DrawRect.Top - TextSize.cy) div 2;

      newWidth := TextSize.cx + srcWidth;
      
      if BitBtn.Spacing <> -1 then
        newWidth := newWidth + BitBtn.Spacing;
        
      if srcWidth <> 0 then
        inc(newWidth, 2);
        
      case BitBtnLayout of
        blGlyphLeft:
        begin
          XDestBitmap := (DrawRect.Right + DrawRect.Left - newWidth) div 2;
          XDestText := XDestBitmap + srcWidth;
        end;
        blGlyphRight:
        begin
          XDestText := (DrawRect.Right + DrawRect.Left - newWidth) div 2;
          XDestBitmap := XDestText + TextSize.cx;
        end;
      end;
    end;
    blGlyphTop, blGlyphBottom:
    begin
      XDestBitmap := (DrawRect.Right + DrawRect.Left - srcWidth) shr 1;
      XDestText := (DrawRect.Right + DrawRect.Left - TextSize.cx) shr 1;

      newHeight := TextSize.cy + srcHeight;

      if BitBtn.Spacing <> -1 then
        newHeight := newHeight + BitBtn.Spacing;
        
      if srcHeight <> 0 then
        inc(newHeight, 2);
        
      case BitBtnLayout of
        blGlyphTop:
        begin
          YDestBitmap := (DrawRect.Top + DrawRect.Bottom - newHeight) div 2;
          YDestText := YDestBitmap + srcHeight;
        end;
        blGlyphBottom:
        begin
          YDestText := (DrawRect.Top + DrawRect.Bottom - newHeight) div 2;
          YDestBitmap := YDestText + TextSize.cy;
        end;
      end;
    end;
  end;

  DrawBitmap;

  SelectObject(DrawStruct^._hDC, OldFontHandle);
end;


class function TWinCEWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ButtonClsName;
    Flags := Flags or BS_OWNERDRAW; // Draw bitmap on WM_DRAWITEM
    WindowTitle := '';
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

class procedure TWinCEWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
begin
  ABitBtn.invalidate;
end;

class procedure TWinCEWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  ABitBtn.invalidate;
end;

class procedure TWinCEWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  ABitBtn.invalidate;
end;

class procedure TWinCEWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  ABitBtn.invalidate;
end;

class procedure TWinCEWSBitBtn.SetText(const AWinControl: TWinControl; const AText: string);
begin
  AWinControl.invalidate;
end;

end.
