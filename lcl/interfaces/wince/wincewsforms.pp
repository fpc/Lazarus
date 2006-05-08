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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  SysUtils, Controls, LCLType, Forms, winceproc,wincewscontrols ,
  InterfaceBase,
////////////////////////////////////////////////////
  WSForms, WSLCLClasses;

type

  { TWinCEWSScrollingWinControl }

  TWinCEWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
      const DeltaX, DeltaY: integer); override;
  end;

  { TWinCEWSScrollBox }

  TWinCEWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
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
    class procedure SetBorderIcons(const AForm: TCustomForm;
          const ABorderIcons: TBorderIcons); override;

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

{ TWin32WSScrollBox }

function TWinCEWSScrollBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @ClsName;
    Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWinCEWSScrollingWinControl }

function ScrollWindowPtr(hWnd:HWND; dx:longint; dy:longint; prcScroll:lpRECT; prcClip:lpRECT;hrgnUpdate:HRGN;
prcUpdate:LPRECT; flags:UINT):longint; external UserDLLCore name 'ScrollWindowEx';

procedure TWinCEWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
var
  lVisible: boolean;
  rgn : HRGN;
  rect : trect;
begin
 lVisible := AWinControl.HandleAllocated and Windows.IsWindowVisible(AWinControl.Handle);
 rgn := 0;//roozbeh : seems to be ok?
// GetClipRgn(AWinControl.Handle,rgn);
//roozbeh:which flags really are rewuired?!
 if lVisible then
    ScrollWindowPtr(AWinControl.Handle, DeltaX, DeltaY, nil, nil,rgn,nil,SW_INVALIDATE or SW_ERASE or SW_SCROLLCHILDREN);
end;

{ TWinCEWSCustomForm }

function CalcBorderIconsFlags(const AForm: TCustomForm): dword;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if biSystemMenu in BorderIcons then
    Result := Result or WS_SYSMENU;
  if GetDesigningBorderStyle(AForm) in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZEBOX;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZEBOX;
  end;
end;

procedure CalcFormWindowFlags(const AForm: TCustomForm; var Flags, FlagsEx: dword);
var
  BorderStyle: TFormBorderStyle;
begin
  BorderStyle := GetDesigningBorderStyle(AForm);
  Flags := BorderStyleToWin32Flags(BorderStyle);
  FlagsEx := BorderStyleToWin32FlagsEx(BorderStyle);
  if (AForm.FormStyle in fsAllStayOnTop) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TWinCEWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
//  hwnd: THandle;
//  Str: array[0..255] of WideChar;
  Params: TCreateWindowExParams;
  LForm : TCustomForm;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomForm.CreateHandle');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
    pClassName := @ClsName;
    FlagsEx := 0;
    Flags := WS_OVERLAPPEDWINDOW;
    lForm := TCustomForm(AWinControl);
    CalcFormWindowFlags(lForm, Flags, FlagsEx);
    SubClassWndProc := nil;
    Parent := 0;
    Left:=CW_USEDEFAULT;
    Top:=CW_USEDEFAULT;
    Height:=CW_USEDEFAULT;
    Width:=CW_USEDEFAULT;
  end;
  
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  {$ifdef VerboseWinCE}
  WriteLn('Window Handle = ' + IntToStr(Result));
  {$endif}
end;

procedure TWinCEWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
 const ABorderIcons: TBorderIcons);
begin
  UpdateWindowStyle(AForm.Handle, CalcBorderIconsFlags(AForm),
    WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  SetIcon(AForm, 0);
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollingWinControl, TWinCEWSScrollingWinControl);
  RegisterWSComponent(TScrollBox, TWinCEWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWinCEWSCustomFrame);
//  RegisterWSComponent(TFrame, TWinCEWSFrame);
  RegisterWSComponent(TCustomForm, TWinCEWSCustomForm);
//  RegisterWSComponent(TForm, TWinCEWSForm);
//  RegisterWSComponent(THintWindow, TWinCEWSHintWindow);
//  RegisterWSComponent(TScreen, TWinCEWSScreen);
//  RegisterWSComponent(TApplicationProperties, TWinCEWSApplicationProperties);
////////////////////////////////////////////////////
end.