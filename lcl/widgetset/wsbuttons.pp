{ $Id$}
{
 *****************************************************************************
 *                               WSButtons.pp                                * 
 *                               ------------                                * 
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
unit WSButtons;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, Controls, Buttons, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSStdCtrls, WSControls, LCLType, LCLIntf, WSFactory;

type

  { TWSBitBtn }
  
  TWSBitBtnClass = class of TWSBitBtn;
  TWSBitBtn = class(TWSButton)
  published
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); virtual;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); virtual;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); virtual;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); virtual;
  end;

  { TWSSpeedButton }

  TWSSpeedButtonClass = class of TWSSpeedButton;
  TWSSpeedButton = class(TWSGraphicControl)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterCustomBitBtn;
  procedure RegisterCustomSpeedButton;

implementation

uses
  LResources;


// TODO: Can't be virtual abstract ?

{ TWSCustomBitBtn }

class procedure TWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
begin
end;

class procedure TWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
end;

class procedure TWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
end;

class procedure TWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomBitBtn;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomBitBtn;
  RegisterPropertyToSkip(TBitBtn, 'Style', 'VCL compatibility property', '');
//  if not WSRegisterCustomBitBtn then
//    RegisterWSComponent(TCustomBitBtn, TWSBitBtn);
  Done := True;
end;

procedure RegisterCustomSpeedButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSpeedButton;
//  if not WSRegisterCustomSpeedButton then
//    RegisterWSComponent(TCustomSpeedButton, TWSSpeedButton);
  Done := True;
end;

end.
