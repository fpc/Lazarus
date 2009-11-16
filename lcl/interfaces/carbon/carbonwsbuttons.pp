{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSButtons.pp                           *
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
unit CarbonWSButtons;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll,
  // LCL
  Classes, Controls, Buttons, LCLType, LCLProc, Graphics,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonButtons, CarbonWSControls, CarbonGDIObjects;

type

  { TCarbonWSBitBtn }

  TCarbonWSBitBtn = class(TWSBitBtn)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCarbonWSSpeedButton }

  TCarbonWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

uses
  CarbonProc, CarbonDbgConsts, CarbonCanvas;

{ TCarbonWSBitBtn }

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new bevel button with bitmap in Carbon interface with the
  specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonBitBtn.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.SetGlyph
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap

  Sets the bitmap of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
var
  Img     : CGImageRef;
  R       : TRect;
begin
  if not CheckHandle(ABitBtn, Self, 'SetGlyph') then Exit;

  Img := nil;
  if ABitBtn.CanShowGlyph and (AValue.Glyph <> nil) and (AValue.Glyph.Width > 0) and (AValue.Glyph.Height > 0) then
  begin
    if TObject(AValue.Glyph.Handle) is TCarbonBitmap then
    begin
      if AValue.NumGlyphs <= 1 then
        Img := TCarbonBitmap(AValue.Glyph.Handle).CreateMaskedImage(TCarbonBitmap(AValue.Glyph.MaskHandle))
      else
      begin
        // TODO: consider button style (down, disabled)
        R := Classes.Rect(0, 0, AValue.Glyph.Width div ABitBtn.NumGlyphs, AValue.Glyph.Height);
        Img := TCarbonBitmap(AValue.Glyph.Handle).CreateMaskedImage(TCarbonBitmap(AValue.Glyph.MaskHandle), R);
      end;
    end;
  end;

  {if ABitBtn.CanShowGlyph then
    TCarbonBitBtn(ABitBtn.Handle).SetGlyph(AValue.Glyph)
  else}
  TCarbonBitBtn(ABitBtn.Handle).SetGlyph(Img);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout

  Sets the bitmap and caption layout of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  Placement: ControlButtonTextPlacement;
  TextAlign: ControlButtonTextAlignment;
begin
  if not CheckHandle(ABitBtn, Self, 'SetLayout') then Exit;

  if (ABitBtn.CanShowGlyph) and (ABitBtn.Glyph <> nil) and (ABitBtn.Glyph.Width > 0) and (ABitBtn.Glyph.Height > 0) then
  begin
    TextAlign := kControlBevelButtonAlignLeft;
    case AValue of
      blGlyphLeft  : Placement := kControlBevelButtonPlaceToRightOfGraphic;
      blGlyphRight : Placement := kControlBevelButtonPlaceToLeftOfGraphic;
      blGlyphTop   : Placement := kControlBevelButtonPlaceBelowGraphic;
      blGlyphBottom: Placement := kControlBevelButtonPlaceAboveGraphic;
    end;
  end
  else // if Glyph is empty, then align center
  begin
    TextAlign := kControlBevelButtonAlignTextCenter;
    Placement := kControlBevelButtonPlaceNormally;
  end;
  TCarbonBitBtn(ABitBtn.Handle).SetLayout(Placement, TextAlign);
end;

end.
