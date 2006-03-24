{ $Id: wsforms.pp 7361 2005-07-16 00:08:26Z marc $}
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
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSForms;

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
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows,
  SysUtils, Controls, LCLType, Forms,
  InterfaceBase,
////////////////////////////////////////////////////
  WSForms, WSLCLClasses;

type

  { TWinCEWSScrollingWinControl }

  TWinCEWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TWinCEWSScrollBox }

  TWinCEWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomFrame }

  TWinCEWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TWinCEWSFrame }

  TWinCEWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TWinCEWSCustomForm }

  TWinCEWSCustomForm = class(TWSCustomForm)
  private
//    class procedure SetSlots(const QtCustomForm: TQtCustomForm);
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
{    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;}
  end;

  { TWinCEWSForm }

  TWinCEWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWinCEWSHintWindow }

  TWinCEWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TWinCEWSScreen }

  TWinCEWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TWinCEWSApplicationProperties }

  TWinCEWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

uses Winceint;

{------------------------------------------------------------------------------
  Method: TWinCEWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TWinCEWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  hwnd: THandle;
  Str: array[0..255] of WideChar;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomForm.CreateHandle');
  {$endif}

  MultiByteToWideChar(CP_UTF8, 0, PChar(AWinControl.Caption), -1, @Str, 256);

  hwnd := CreateWindow(
    @ClsName,           // Name of the registered class
    @Str,               // Title of the window
    WS_OVERLAPPEDWINDOW,// Style of the window
    CW_USEDEFAULT,      // x-position (at beginning)
    CW_USEDEFAULT,      // y-position (at beginning)
    CW_USEDEFAULT,      // window width
    CW_USEDEFAULT,      // window height
    0,                  // handle to parent or owner window
    0,                  // handle to menu
    System.hInstance,   // handle to application instance
    nil);               // pointer to window-creation data

  if (hwnd = 0) then WriteLn('CreateWindow failed');

  ShowWindow(hwnd, SW_SHOW);
  UpdateWindow(hwnd);

  Result := hwnd;
  {$ifdef VerboseWinCE}
  WriteLn('Window Handle = ' + IntToStr(Result));
  {$endif}
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TWinCEWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TWinCEWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWinCEWSCustomFrame);
//  RegisterWSComponent(TFrame, TWinCEWSFrame);
  RegisterWSComponent(TCustomForm, TWinCEWSCustomForm);
//  RegisterWSComponent(TForm, TWinCEWSForm);
//  RegisterWSComponent(THintWindow, TWinCEWSHintWindow);
//  RegisterWSComponent(TScreen, TWinCEWSScreen);
//  RegisterWSComponent(TApplicationProperties, TWinCEWSApplicationProperties);
////////////////////////////////////////////////////
end.
