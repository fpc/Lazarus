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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

uses
  // libs
  FPCMacOSAll,
  // LCL
  Controls, Buttons, LCLType, LCLProc, Graphics,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate, CarbonGDIObjects,
  CarbonWSControls;

type

  { TCarbonWSButton }

  TCarbonWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
  end;

  { TCarbonWSBitBtn }

  TCarbonWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TBitmap); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCarbonWSSpeedButton }

  TCarbonWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

{ TCarbonWSButton }

{------------------------------------------------------------------------------
  Method:  TCarbonWSButton.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface
  
  Creates new button control in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  // create the Carbon button widget
  Result := TLCLIntfHandle(TCarbonButton.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSButton.SetDefault
  Params:  AButton  - LCL button control
           ADefault
  Returns: Nothing

  Sets button default indication
 ------------------------------------------------------------------------------}
class procedure TCarbonWSButton.SetDefault(const AButton: TCustomButton;
  ADefault: Boolean);
begin
  if not CheckHandle(AButton, Self, 'SetDefault') then Exit;

  SetControlData(AsControlRef(AButton.Handle), kControlEntireControl,
    kControlPushButtonDefaultTag, SizeOf(Boolean), @ADefault);
end;

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
  Returns: Nothing

  Sets the bitmap of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TBitmap);
var
  ContentInfo: ControlButtonContentInfo;
begin
  if not CheckHandle(ABitBtn, Self, 'SetGlyph') then Exit;
  
  ContentInfo.contentType := kControlContentCGImageRef;
  if AValue = nil then
    ContentInfo.imageRef := nil
  else
    ContentInfo.imageRef := TCarbonBitmap(AValue.Handle).CGImage;
  
  SetBevelButtonContentInfo(AsControlRef(ABitBtn.Handle), @ContentInfo);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout
  Returns: Nothing

  Sets the bitmap nad caption layout of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
var
  Placement: ControlButtonTextPlacement;
begin
  if not CheckHandle(ABitBtn, Self, 'SetLayout') then Exit;
  
  case AValue of
    blGlyphLeft  : Placement := kControlBevelButtonPlaceToRightOfGraphic;
    blGlyphRight : Placement := kControlBevelButtonPlaceToLeftOfGraphic;
    blGlyphTop   : Placement := kControlBevelButtonPlaceBelowGraphic;
    blGlyphBottom: Placement := kControlBevelButtonPlaceAboveGraphic;
  end;
  
  SetBevelButtonTextPlacement(AsControlRef(ABitBtn.Handle),
    Placement);
  TCarbonWidget(ABitBtn.Handle).Invalidate;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomButton, TCarbonWSButton);
  RegisterWSComponent(TCustomBitBtn, TCarbonWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TCarbonWSSpeedButton);
////////////////////////////////////////////////////
end.
