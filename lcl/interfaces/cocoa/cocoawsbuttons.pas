{
 *****************************************************************************
 *                              CocoaWSButtons.pp                            *
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
unit cocoawsbuttons;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // libs
  MacOSAll, CocoaAll, SysUtils, Math,
  // LCL
  Classes, Controls, Buttons, LCLType, LCLProc, Graphics, GraphType, ImgList,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // LCL Cocoa
  CocoaWSCommon, CocoaWSStdCtrls, CocoaGDIObjects, CocoaPrivate, CocoaUtils,
  cocoa_extra, CocoaButtons;

type

  { TCocoaWSBitBtn }

  TCocoaWSBitBtn = class(TWSBitBtn)
  private
    class function  LCLGlyphPosToCocoa(ALayout: TButtonLayout): NSCellImagePosition;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    //
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCocoaWSSpeedButton }

  TCocoaWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

{ TCocoaWSBitBtn }

class function TCocoaWSBitBtn.LCLGlyphPosToCocoa(ALayout: TButtonLayout
  ): NSCellImagePosition;
begin
  case ALayout of
  blGlyphLeft:   Result := NSImageLeft;
  blGlyphRight:  Result := NSImageRight;
  blGlyphTop:    Result := NSImageAbove;
  blGlyphBottom: Result := NSImageBelow;
  else
    Result := NSNoImage;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new bevel button with bitmap in Cocoa interface with the
  specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn: NSButton;
begin
  btn := AllocButton(AWinControl, TLCLButtonCallBack, AParams, NSRegularSquareBezelStyle, NSMomentaryPushInButton);
  Result := TLCLIntfHandle(btn);
end;

class procedure TCocoaWSBitBtn.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  lButton: TCustomBitBtn absolute AWinControl;
  lButtonHandle: TCocoaButton;
  Size: NSSize;
begin
  if not AWinControl.HandleAllocated then Exit;

  lButtonHandle := TCocoaButton(AWinControl.Handle);
  // fittingSize is 10.7+
  if lButtonHandle.respondsToSelector(objcselector('fittingSize')) then
  begin
    Size := lButtonHandle.fittingSize();
    if lButton.Glyph <> nil then
      Size.Height := Max(Size.Height, lButton.Glyph.Height + 6); // This nr is arbitrary
    PreferredWidth := Round(Size.Width);
    PreferredHeight := Round(Size.Height);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetGlyph
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap

  Sets the bitmap of bevel button in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  Img: NSImage;
  AGlyph: TBitmap;
  lButtonHandle: TCocoaButton;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  AImgRes: TScaledImageListResolution;
  ImgSize: NSSize;
  ScaleFactor: Double;
begin
  //WriteLn('[TCocoaWSBitBtn.SetGlyph]');
  Img := nil;
  if ABitBtn.CanShowGlyph(True) then
  begin
    AGlyph := TBitmap.Create;
    ScaleFactor := ABitBtn.GetCanvasScaleFactor;
    AValue.GetImageIndexAndEffect(bsUp, ABitBtn.Font.PixelsPerInch,
      ScaleFactor, AImgRes, AIndex, AEffect);
    AImgRes.GetBitmap(AIndex, AGlyph, AEffect);
    Img := TCocoaBitmap(AGlyph.Handle).image;
    if AImgRes.Resolution.ImageList.Scaled and not SameValue(ScaleFactor, 1) then // resize only if the image list is scaled
    begin
      ImgSize := Img.size;
      ImgSize.height := ImgSize.height / ScaleFactor;
      ImgSize.width := ImgSize.width / ScaleFactor;
      Img.setSize(ImgSize);
    end;
    lButtonHandle := TCocoaButton(ABitBtn.Handle);
    lButtonHandle.setImage(Img);
    lButtonHandle.setImagePosition(LCLGlyphPosToCocoa(ABitBtn.Layout));
    lButtonHandle.setImageScaling(NSImageScaleNone); // do not scale - retina scaling is done above with Img.setSize
    if Assigned(lButtonHandle.Glyph) then
      FreeAndNil(lButtonHandle.Glyph);
    lButtonHandle.Glyph := AGlyph;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout

  Sets the bitmap and caption layout of bevel button in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  ImagePos: NSCellImagePosition;
begin

  if ABitBtn.CanShowGlyph(True) then
    ImagePos := LCLGlyphPosToCocoa(AValue)
  else
    ImagePos := NSNoImage;
  NSButton(ABitBtn.Handle).SetImagePosition(ImagePos);
end;

end.
