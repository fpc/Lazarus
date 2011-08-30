{
 *****************************************************************************
 *                            AndroidWSForms.pp                              *
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
unit androidwsforms;

{$mode objfpc}{$H+}

interface

// defines
//{$I Androiddefines.inc}

uses
  // Libs
//  MacOSAll,
//  AndroidUtils,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSForms, WSLCLClasses, WSProc,
  // LCL Android
  AndroidPrivate;

type

  { TAndroidWSScrollingWinControl }

  TAndroidWSScrollingWinControl = class(TWSScrollingWinControl)
  published
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
  end;

  { TAndroidWSScrollBox }

  TAndroidWSScrollBox = class(TWSScrollBox)
  published
  end;

  { TAndroidWSCustomFrame }

  TAndroidWSCustomFrame = class(TWSCustomFrame)
  published
  end;

  { TAndroidWSFrame }

  TAndroidWSFrame = class(TWSFrame)
  published
  end;

  { TAndroidWSCustomForm }
  TAndroidWSCustomFormClass = class of TAndroidWSCustomForm;
  TAndroidWSCustomForm = class(TWSCustomForm)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

//    class procedure CloseModal(const ACustomForm: TCustomForm); override;
//    class procedure ShowModal(const ACustomForm: TCustomForm); override;

//    class procedure ShowHide(const AWinControl: TWinControl); override;
    
//    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
//    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;

//    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte); override;
//    class procedure SetFormStyle(const ACustomForm: TCustomForm; const ANewFormStyle, AOldFormStyle: TFormStyle); override;

//    class procedure SetPopupParent(const ACustomForm: TCustomForm;
//      const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;

    // TWSWinControl
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TAndroidWSForm }

  TAndroidWSForm = class(TWSForm)
  published
  end;

  { TAndroidWSHintWindow }

  TAndroidWSHintWindow = class(TWSHintWindow)
  published
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TAndroidWSScreen }

  TAndroidWSScreen = class(TWSScreen)
  published
  end;

  { TAndroidWSApplicationProperties }

  TAndroidWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

(*{ TAndroidWSScrollingWinControl }

{------------------------------------------------------------------------------
  Method:  TAndroidWSScrollingWinControl.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Android interface

  Creates new scrolling window control in Android interface with the specified
  parameters
 ------------------------------------------------------------------------------}
class function TAndroidWSScrollingWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TAndroidScrollingWinControl.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TAndroidWSScrollingWinControl.ScrollBy
  Params:  AWinControl - LCL scrolling win control
           DX, DY      -

  Scrolls the content of the passed window
 ------------------------------------------------------------------------------}
class procedure TAndroidWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
begin
  if not CheckHandle(AWinControl, Self, 'ScrollBy') then Exit;

  TAndroidWidget(AWinControl.Handle).ScrollBy(DeltaX, DeltaY);
end;                        *)

{ TAndroidWSCustomForm }

{------------------------------------------------------------------------------
  Method:  TAndroidWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Android interface

  Creates new window in Android interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TAndroidWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  frm: TCustomForm absolute AWinControl;
begin
  Result := TLCLIntfHandle(TAndroidWindow.Create(frm, AParams));
{  if Assigned(frm) then
  begin
    if (AParams.WndParent<>0) and ((AParams.Style and WS_CHILD) = 0) then
      SetWindowGroup(TAndroidWindow(Result).Window, GetWindowGroupOfClass(kHelpWindowClass));
  end;}
end;

class function TAndroidWSCustomForm.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  frm: TCustomForm absolute AWinControl;
  lHandle: TAndroidWindow;
begin
  Result := False;
  if frm = nil then Exit;
  lHandle := TAndroidWindow(AWinControl.Handle);
  if lHandle = nil then Exit;

  AText := lHandle.GetText();
  Result := True;
end;

class procedure TAndroidWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  frm: TCustomForm absolute AWinControl;
  lHandle: TAndroidWindow;
begin
  if frm = nil then Exit;
  lHandle := TAndroidWindow(AWinControl.Handle);
  if lHandle = nil then Exit;

  lHandle.SetText(AText);
end;

(*
{------------------------------------------------------------------------------
  Method:  TAndroidWSCustomForm.CloseModal
  Params:  ACustomForm - LCL custom form

  Closes modal window in Android interface
 ------------------------------------------------------------------------------}
class procedure TAndroidWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, 'CloseModal') then Exit;
  
  TAndroidWindow(ACustomForm.Handle).CloseModal;
end;

{------------------------------------------------------------------------------
  Method:  TAndroidWSCustomForm.ShowModal
  Params:  ACustomForm - LCL custom form

  Shows modal window in Android interface
 ------------------------------------------------------------------------------}
class procedure TAndroidWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if not CheckHandle(ACustomForm, Self, SShowModal) then Exit;

  TAndroidWindow(ACustomForm.Handle).ShowModal;
end;

class procedure TAndroidWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  nCmdShow : Integer;
begin
  if not CheckHandle(AWinControl, Self, 'ShowHide') then Exit;

  if AWinControl.HandleObjectShouldBeVisible then
  begin
    case TCustomForm(AWinControl).WindowState of
      wsFullScreen: nCmdShow := SW_SHOWFULLSCREEN;
      wsMaximized: nCmdShow := SW_SHOWMAXIMIZED;
      wsMinimized: nCmdShow := SW_SHOWMINIMIZED;
    else
      nCmdShow := SW_SHOW;
    end;
    TAndroidWindow(AWinControl.Handle).ShowHide(True);
    TAndroidWindow(AWinControl.Handle).Show(nCmdShow);
  end
  else
    TAndroidWindow(AWinControl.Handle).ShowHide(False);
end;

{------------------------------------------------------------------------------
  Method:  TAndroidWSCustomForm.SetBorderIcons
  Params:  AForm        - LCL custom form
           ABorderIcons - Border icons

  Sets the border icons of window  in Android interface
 ------------------------------------------------------------------------------}
class procedure TAndroidWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  if not CheckHandle(AForm, Self, 'SetBorderIcons') then Exit;

  TAndroidWindow(AForm.Handle).SetBorderIcons(ABorderIcons);
end;

{------------------------------------------------------------------------------
  Method:  TAndroidWSCustomForm.SetFormBorderStyle
  Params:  AForm            - LCL custom form
           AFormBorderStyle - Form border style

  Sets the form border style of window in Android interface
 ------------------------------------------------------------------------------}
class procedure TAndroidWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if not CheckHandle(AForm, Self, 'SetFormBorderStyle') then Exit;

  TAndroidWindow(AForm.Handle).SetFormBorderStyle(AFormBorderStyle);
end;

class procedure TAndroidWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte);
var
  v : single;
begin
  if not CheckHandle(ACustomForm, Self, 'SetFormBorderStyle') then Exit;
  if not AlphaBlend then v:=1 else v:=Alpha/255;
  SetWindowAlpha( TAndroidWindow(ACustomForm.Handle).Window, v);
end;

class procedure TAndroidWSCustomForm.SetFormStyle(const ACustomForm:TCustomForm;
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
    SetWindowGroup( TAndroidWindow(ACustomForm.Handle).Window, GetWindowGroupOfClass(newClass)),
    Self, 'SetFormStyle', 'SetWindowGroup');
end;

class procedure TAndroidWSCustomForm.SetPopupParent(const ACustomForm:TCustomForm;
  const APopupMode:TPopupMode;const APopupParent:TCustomForm);
begin
  //todo: better "popup-parent" hanlding
  if Assigned(APopupParent) and (APopupParent.Handle<>0) then
  begin
    SetWindowGroup( TAndroidWindow(ACustomForm.Handle).Window, GetWindowGroupOfClass(kHelpWindowClass));
  end
  else
    SetFormStyle(ACustomForm, ACustomForm.FormStyle, ACustomForm.FormStyle);
end;*)

{ TAndroidWSHintWindow }

{------------------------------------------------------------------------------
  Method:  TAndroidWSHintWindow.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Android interface

  Creates new hint window in Android interface with the specified parameters
 ------------------------------------------------------------------------------}
{class function TAndroidWSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TAndroidHintWindow.Create(AWinControl, AParams));
end;}

end.
