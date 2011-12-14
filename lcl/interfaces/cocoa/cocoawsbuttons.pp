{
 *****************************************************************************
 *                              CocoaWSButtons.pp                            *
 *                              --------------                               *
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
unit cocoawsbuttons;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // libs
  MacOSAll, CocoaAll,
  // LCL
  Classes, Controls, Buttons, LCLType, LCLProc, Graphics, GraphType,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // LCL Carbon
  CocoaWSCommon, CocoaWSStdCtrls, CocoaGDIObjects, CocoaUtils;

type

  { TCocoaWSBitBtn }

  TCocoaWSBitBtn = class(TWSBitBtn)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCocoaWSSpeedButton }

  TCocoaWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

{ TCocoaWSBitBtn }

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new bevel button with bitmap in Carbon interface with the
  specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  btn: NSButton;
begin
  btn := AllocButton(AWinControl, TLCLButtonCallBack, AParams, NSRoundedBezelStyle, NSMomentaryPushInButton);
  if Assigned(btn) then
    AddViewToNSObject(btn, NSObject(AParams.WndParent), AParams.X, AParams.Y);
  Result := TLCLIntfHandle(btn);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetGlyph
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap

  Sets the bitmap of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
var
  Img: NSImage;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
begin
  Img := nil;
  if ABitBtn.CanShowGlyph then
  begin
    AGlyph := TBitmap.Create;
    AValue.GetImageIndexAndEffect(bsUp, AIndex, AEffect);
    AValue.Images.GetBitmap(AIndex, AGlyph, AEffect);
    Img := TCocoaBitmap(AGlyph.Handle).image;
    NSButton(ABitBtn.Handle).setImage(Img);
    AGlyph.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout

  Sets the bitmap and caption layout of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  ImagePosition: NSCellImagePosition;
begin

  if (ABitBtn.CanShowGlyph) then
  begin
    case AValue of
      blGlyphLeft  : ImagePosition := NSImageLeft;
      blGlyphRight : ImagePosition := NSImageRight;
      blGlyphTop   : ImagePosition := NSImageBelow;
      blGlyphBottom: ImagePosition := NSImageAbove;
    end;
  end
  else
    ImagePosition := NSNoImage;
  NSButton(ABitBtn.Handle).SetImagePosition(ImagePosition);
end;

end.
