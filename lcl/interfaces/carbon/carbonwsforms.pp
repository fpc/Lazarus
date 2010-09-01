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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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

// defines
{$I carbondefines.inc}

uses
  // Libs
  MacOSAll,
  CarbonUtils,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSForms, WSLCLClasses, WSProc,
  // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonWSControls;

type

  { TCarbonWSScrollingWinControl }

  TCarbonWSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
  end;

  { TCarbonWSScrollBox }

  TCarbonWSScrollBox = class(TWSScrollBox)
  published
  end;

  { TCarbonWSCustomFrame }

  TCarbonWSCustomFrame = class(TWSCustomFrame)
  published
  end;

  { TCarbonWSFrame }

  TCarbonWSFrame = class(TWSFrame)
  published
  end;

  { TCarbonWSCustomForm }
  TCarbonWSCustomFormClass = class of TCarbonWSCustomForm;
  TCarbonWSCustomForm = class(TWSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;

    class procedure ShowHide(const AWinControl: TWinControl); override;
    
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;

    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte); override;
    class procedure SetFormStyle(const ACustomForm: TCustomForm; const ANewFormStyle, AOldFormStyle: TFormStyle); override;

    class procedure SetPopupParent(const ACustomForm: TCustomForm;
      const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;
  end;

  { TCarbonWSForm }

  TCarbonWSForm = class(TWSForm)
  published
  end;

  { TCarbonWSHintWindow }

  TCarbonWSHintWindow = class(TWSHintWindow)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSScreen }

  TCarbonWSScreen = class(TWSScreen)
  published
  end;

  { TCarbonWSApplicationProperties }

  TCarbonWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

uses
  CarbonProc, CarbonDbgConsts;
  

{ TCarbonWSScrollingWinControl }

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollingWinControl.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Carbon interface

  Creates new scrolling window control in Carbon interface with the specified
  parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSScrollingWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonScrollingWinControl.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSScrollingWinControl.ScrollBy
  Params:  AWinControl - LCL scrolling win control
           DX, DY      -

  Scrolls the content of the passed window
 ------------------------------------------------------------------------------}
class procedure TCarbonWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
begin
  if not CheckHandle(AWinControl, Self, 'ScrollBy') then Exit;

  TCarbonWidget(AWinControl.Handle).ScrollBy(DeltaX, DeltaY);
end;

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
var
  frm : TCustomForm;
begin
  if csDesigning in AWinControl.ComponentState then
    Result := TLCLIntfHandle(TCarbonDesignWindow.Create(AWinControl, AParams))
  else
    Result := TLCLIntfHandle(TCarbonWindow.Create(AWinControl, AParams));
  frm:=TCustomForm(AWinControl);
  if Assigned(frm) then
  begin
    if (AParams.WndParent<>0) and ((AParams.Style and WS_CHILD) = 0) then
      SetWindowGroup(TCarbonWindow(Result).Window, GetWindowGroupOfClass(kHelpWindowClass));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.CloseModal
  Params:  ACustomForm - LCL custom form

  Closes modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, 'CloseModal') then Exit;
  
  TCarbonWindow(ACustomForm.Handle).CloseModal;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.ShowModal
  Params:  ACustomForm - LCL custom form

  Shows modal window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, SShowModal) then Exit;

  TCarbonWindow(ACustomForm.Handle).ShowModal;
end;

class procedure TCarbonWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  nCmdShow : Integer;
begin
  if not CheckHandle(AWinControl, Self, 'ShowHide') then Exit;

  if AWinControl.HandleObjectShouldBeVisible then
  begin
    case TCustomForm(AWinControl).WindowState of
      wsMaximized: nCmdShow := SW_SHOWMAXIMIZED;
      wsMinimized: nCmdShow := SW_SHOWMINIMIZED;
    else
      nCmdShow := SW_SHOW;
    end;
    TCarbonWindow(AWinControl.Handle).ShowHide(True);
    TCarbonWindow(AWinControl.Handle).Show(nCmdShow);
  end
  else
    TCarbonWindow(AWinControl.Handle).ShowHide(False);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetBorderIcons
  Params:  AForm        - LCL custom form
           ABorderIcons - Border icons

  Sets the border icons of window  in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  if not CheckHandle(AForm, Self, 'SetBorderIcons') then Exit;

  TCarbonWindow(AForm.Handle).SetBorderIcons(ABorderIcons);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomForm.SetFormBorderStyle
  Params:  AForm            - LCL custom form
           AFormBorderStyle - Form border style

  Sets the form border style of window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if not CheckHandle(AForm, Self, 'SetFormBorderStyle') then Exit;

  TCarbonWindow(AForm.Handle).SetFormBorderStyle(AFormBorderStyle);
end;

class procedure TCarbonWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte);
var
  v : single;
begin
  if not CheckHandle(ACustomForm, Self, 'SetFormBorderStyle') then Exit;
  if not AlphaBlend then v:=1 else v:=Alpha/255;
  SetWindowAlpha( TCarbonWindow(ACustomForm.Handle).Window, v);
end;

class procedure TCarbonWSCustomForm.SetFormStyle(const ACustomForm:TCustomForm;
  const ANewFormStyle,AOldFormStyle:TFormStyle);
var
  newClass  : WindowClass;
begin
  if not CheckHandle(ACustomForm, Self, 'SetFormStyle') then Exit;

  case ANewFormStyle of
    fsStayOnTop, fsSplash: newClass:=kFloatingWindowClass;
    fsSystemStayOnTop: newClass:=kUtilityWindowClass;
  else
    newClass:=kDocumentWindowClass;
  end;
  OSError(
    SetWindowGroup( TCarbonWindow(ACustomForm.Handle).Window, GetWindowGroupOfClass(newClass)),
    Self, 'SetFormStyle', 'SetWindowGroup');
end;

class procedure TCarbonWSCustomForm.SetPopupParent(const ACustomForm:TCustomForm;
  const APopupMode:TPopupMode;const APopupParent:TCustomForm);
begin
  //todo: better "popup-parent" hanlding
  if Assigned(APopupParent) and (APopupParent.Handle<>0) then
  begin
    SetWindowGroup( TCarbonWindow(ACustomForm.Handle).Window, GetWindowGroupOfClass(kHelpWindowClass));
  end
  else
    SetFormStyle(ACustomForm, ACustomForm.FormStyle, ACustomForm.FormStyle);
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

end.
