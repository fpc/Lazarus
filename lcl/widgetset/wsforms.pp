{ $Id$}
{
 *****************************************************************************
 *                                WSForms.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WSForms;

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
  Forms,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, Controls, LCLType;

type
  { TWSScrollingWinControl }

  TWSScrollingWinControlClass = class of TWSScrollingWinControl;
  TWSScrollingWinControl = class(TWSWinControl)
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; 
      const DeltaX, DeltaY: integer); virtual;
  end;

  { TWSScrollBox }

  TWSScrollBox = class(TWSScrollingWinControl)
  end;

  { TWSCustomFrame }

  TWSCustomFrame = class(TWSScrollingWinControl)
  end;

  { TWSFrame }

  TWSFrame = class(TWSCustomFrame)
  end;

  { TWSCustomForm }

  TWSCustomForm = class(TWSScrollingWinControl)
    class procedure CloseModal(const ACustomForm: TCustomForm); virtual;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); virtual;
    class procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); virtual;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); virtual;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle: TFormStyle); virtual;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); virtual;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); virtual;
    class procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); virtual;
    class procedure ShowModal(const ACustomForm: TCustomForm); virtual;
  end;
  TWSCustomFormClass = class of TWSCustomForm;

  { TWSForm }

  TWSForm = class(TWSCustomForm)
  end;

  { TWSHintWindow }

  TWSHintWindow = class(TWSCustomForm)
  end;

  { TWSScreen }

  TWSScreen = class(TWSLCLComponent)
  end;

  { TWSApplicationProperties }

  TWSApplicationProperties = class(TWSLCLComponent)
  end;


implementation

{ TWSScrollingWinControl }

class procedure TWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
begin
end;
  
{ TWSCustomForm }

class procedure TWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
end;

class procedure TWSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
end;

class procedure TWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
begin
end;

class procedure TWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  // will be done in interface override
end;

class procedure TWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle: TFormStyle);
begin
end;
    
class procedure TWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
begin
end;

class procedure TWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
end;

class procedure TWSCustomForm.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
end;
   
class procedure TWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWSCustomFrame);
//  RegisterWSComponent(TFrame, TWSFrame);
//  RegisterWSComponent(TCustomForm, TWSCustomForm);
//  RegisterWSComponent(TForm, TWSForm);
//  RegisterWSComponent(THintWindow, TWSHintWindow);
//  RegisterWSComponent(TScreen, TWSScreen);
//  RegisterWSComponent(TApplicationProperties, TWSApplicationProperties);
////////////////////////////////////////////////////
end.
