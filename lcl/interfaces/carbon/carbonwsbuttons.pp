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

// debugging defines
{$I carbondebug.inc}

uses
  // libs
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  // LCL
  Classes, Controls, Buttons, LCLType, LCLProc, Graphics,
  // widgetset
  WSButtons, WSLCLClasses, WSProc,
  // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonButtons, CarbonWSControls;

type

  { TCarbonWSBitBtn }

  TCarbonWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TCarbonWSSpeedButton }

  TCarbonWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
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
begin
  if not CheckHandle(ABitBtn, Self, 'SetGlyph') then Exit;
  
  TCarbonBitBtn(ABitBtn.Handle).SetGlyph(AValue.Glyph);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSBitBtn.SetLayout
  Params:  ABitBtn - LCL custom bitmap button
           AValue  - Bitmap and caption layout

  Sets the bitmap and caption layout of bevel button in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  if not CheckHandle(ABitBtn, Self, 'SetLayout') then Exit;

  TCarbonBitBtn(ABitBtn.Handle).SetLayout(AValue);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomBitBtn, TCarbonWSBitBtn);
  RegisterWSComponent(TCustomSpeedButton, TCarbonWSSpeedButton);
////////////////////////////////////////////////////
end.
