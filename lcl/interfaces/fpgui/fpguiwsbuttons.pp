{ $Id: FpGuiwsbuttons.pp 5682 2004-07-15 10:43:39Z mattias $}
{
 *****************************************************************************
 *                              FpGuiWSButtons.pp                               * 
 *                              --------------                               * 
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
unit FpGuiWSButtons;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  Buttons, LCLType, Controls,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TFpGuiWSBitBtn }

  TFpGuiWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TFpGuiWSSpeedButton }

  TFpGuiWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

{ TFpGuiWSBitBtn }

class function TFpGuiWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateButton.Create(AWinControl, AParams));
end;

class procedure TFpGuiWSBitBtn.DestroyHandle(const AWinControl: TWinControl);
begin
  TFPGUIPrivateButton(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;



initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomBitBtn, TFpGuiWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TFpGuiWSSpeedButton);
////////////////////////////////////////////////////
end.
