{ $Id$}
{
 *****************************************************************************
 *                             CarbonWSForms.pp                              *
 *                               ------------                                *
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
unit CarbonWSForms;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
  // Libs
  FPCMacOSAll, CarbonUtils,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSForms, WSLCLClasses, WSProc,
  // LCL Carbon
  CarbonDef, CarbonProc, CarbonPrivate, CarbonWSControls;

type

  { TCarbonWSScrollingWinControl }

  TCarbonWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TCarbonWSScrollBox }

  TCarbonWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TCarbonWSCustomFrame }

  TCarbonWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TCarbonWSFrame }

  TCarbonWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TCarbonWSCustomForm }
  TCarbonWSCustomFormClass = class of TCarbonWSCustomForm;
  TCarbonWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
  end;

  { TCarbonWSForm }

  TCarbonWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TCarbonWSHintWindow }

  TCarbonWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSScreen }

  TCarbonWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TCarbonWSApplicationProperties }

  TCarbonWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TCarbonWSCustomForm }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Carbon interface

  Creates new window in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonWindow.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.CloseModal
  Params:  ACustomForm - LCL custom form

  Closes modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, 'CloseModal') then Exit;
  
  FPCMacOSAll.SetWindowModality(AsWindowRef(ACustomForm.Handle),
    kWindowModalityNone, nil);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.ShowModal
  Params:  ACustomForm - LCL custom form

  Shows modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, 'ShowModal') then Exit;

  SetWindowModality(AsWindowRef(ACustomForm.Handle),
    kWindowModalityAppModal, nil);
  SelectWindow(AsWindowRef(ACustomForm.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetBorderIcons
  Params:  AForm        - LCL custom form
           ABorderIcons - Border icons

  Sets the border icons of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
var
  AttrsSet, AttrsClear: WindowAttributes;
begin
  if not CheckHandle(AForm, Self, 'SetBorderIcons') then Exit;

  AttrsSet := 0;
  AttrsClear := 0;
  
  if (biMinimize in ABorderIcons) and (biSystemMenu in ABorderIcons) then
    AttrsSet := AttrsSet or kWindowCollapseBoxAttribute
  else
    AttrsClear := AttrsClear or kWindowCollapseBoxAttribute;
    
  if (biMaximize in ABorderIcons) and (biSystemMenu in ABorderIcons) then
    AttrsSet := AttrsSet or kWindowFullZoomAttribute
  else
    AttrsClear := AttrsClear or kWindowFullZoomAttribute;
    
  if biSystemMenu in ABorderIcons then
    AttrsSet := AttrsSet or kWindowCloseBoxAttribute
  else
    AttrsClear := AttrsClear or kWindowCloseBoxAttribute;
    
  ChangeWindowAttributes(AsWindowRef(AForm.Handle), AttrsSet, AttrsClear);
end;

class procedure TCarbonWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;


{ TCarbonWSHintWindow }

{------------------------------------------------------------------------------
  Method:  TCarbonWSHintWindow.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Carbon interface

  Creates new hint window in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonHintWindow.Create(AWinControl, AParams));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TCarbonWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TCarbonWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TCarbonWSCustomFrame);
//  RegisterWSComponent(TFrame, TCarbonWSFrame);
  RegisterWSComponent(TCustomForm, TCarbonWSCustomForm);
//  RegisterWSComponent(TForm, TCarbonWSForm);
  RegisterWSComponent(THintWindow, TCarbonWSHintWindow);
//  RegisterWSComponent(TScreen, TCarbonWSScreen);
//  RegisterWSComponent(TApplicationProperties, TCarbonWSApplicationProperties);
////////////////////////////////////////////////////

end.
