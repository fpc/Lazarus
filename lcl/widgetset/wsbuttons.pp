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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Buttons, Graphics, 
////////////////////////////////////////////////////
  WSLCLClasses, WSStdCtrls, WSControls;

type
  { TWSButton }

  TWSButton = class(TWSButtonControl)
  end;

  { TWSBitBtn } 
  
  TWSBitBtnClass = class of TWSBitBtn;
  TWSBitBtn = class(TWSButton)
    class procedure SetGlyph(const ABitBtn: TBitBtn; const AValue: TBitmap); virtual;
    class procedure SetLayout(const ABitBtn: TBitBtn; const AValue: TButtonLayout); virtual;
    class procedure SetMargin(const ABitBtn: TBitBtn; const AValue: Integer); virtual;
    class procedure SetSpacing(const ABitBtn: TBitBtn; const AValue: Integer); virtual;
  end;

  { TWSSpeedButton }

  TWSSpeedButton = class(TWSGraphicControl)
  end;


implementation 

uses       
  // TODO: remove when TWSBitBtn is implemented for win32 
  Controls, LMessages;


  { TWSBitBtn }
  
procedure TWSBitBtn.SetGlyph(const ABitBtn: TBitBtn; const AValue: TBitmap); 
begin
  //TODO: remove when implemented for win32
  CNSendMessage(LM_IMAGECHANGED, ABitBtn, nil);
  ABitBtn.Invalidate;
end;

procedure TWSBitBtn.SetLayout(const ABitBtn: TBitBtn; const AValue: TButtonLayout); 
begin
  //TODO: remove when implemented for win32
  CNSendMessage(LM_LAYOUTCHANGED, ABitBtn, nil);
end;

procedure TWSBitBtn.SetMargin(const ABitBtn: TBitBtn; const AValue: Integer); 
begin
  //TODO: remove when implemented for win32
  CNSendMessage(LM_LAYOUTCHANGED, ABitBtn, nil);
end;

procedure TWSBitBtn.SetSpacing(const ABitBtn: TBitBtn; const AValue: Integer); 
begin   
  //TODO: remove when implemented for win32
  CNSendMessage(LM_LAYOUTCHANGED, ABitBtn, nil);
end;


initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TButton, TWSButton);
  RegisterWSComponent(TBitBtn, TWSBitBtn);
//  RegisterWSComponent(TSpeedButton, TWSSpeedButton);
////////////////////////////////////////////////////
end.
