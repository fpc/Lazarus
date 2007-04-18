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
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Buttons,
////////////////////////////////////////////////////
  WSButtons, WSLCLClasses, LCLType, Controls;

type

  { TFpGuiWSButton }

  TFpGuiWSButton = class(TWSButton)
  private
  protected
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TFpGuiWSBitBtn }

  TFpGuiWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
  end;

  { TFpGuiWSSpeedButton }

  TFpGuiWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses FPGUIWSPrivate;

{ TFpGuiWSButton }

class function TFpGuiWSButton.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := True;
  AText := TFPGUIPrivateButton(AWinControl.Handle).GetText;
end;

class procedure TFpGuiWSButton.SetText(const AWinControl: TWinControl;
  const AText: String);
begin
  TFPGUIPrivateButton(AWinControl.Handle).SetText(AText);
end;

class function TFpGuiWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TFPGUIPrivateButton.Create(AWinControl, AParams));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(Buttons.TCustomButton, TFpGuiWSButton);
//  RegisterWSComponent(TCustomBitBtn, TFpGuiWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TFpGuiWSSpeedButton);
////////////////////////////////////////////////////
end.
