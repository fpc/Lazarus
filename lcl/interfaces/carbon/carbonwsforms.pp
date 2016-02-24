{
 *****************************************************************************
 *                             CarbonWSForms.pp                              *
 *                               ------------                                *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  // LCL
  Controls, Forms, Graphics, LCLType, LCLProc, Classes,
  // Widgetset
  WSForms, WSLCLClasses,
  // LCL Carbon
  CarbonDef, CarbonPrivate;

type

  { TCarbonWSScrollingWinControl }

  TCarbonWSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
    class procedure SetFormStyle(const ACustomForm: TCustomForm; const ANewFormStyle, {%H-}AOldFormStyle: TFormStyle); override;

    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); override;
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
  if IsFormDesign(AWinControl) then
    Result := TLCLIntfHandle(TCarbonDesignWindow.Create(AWinControl, AParams))
  else
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
  ModalForm: TCustomForm;
  ACurrentWindowClass: WindowClass;
  function HaveModalForms: Boolean;
  var
    i: Integer;
    AForm: TCustomForm;
  begin
    Result := False;
    for i := 0 to Screen.CustomFormZOrderCount - 1 do
    begin
      AForm := Screen.CustomFormsZOrdered[i];
      Result := AForm.Visible and AForm.HandleAllocated and (fsModal in AForm.FormState);
      if Result then
      begin
        ModalForm := AForm;
        break;
      end;
    end;
  end;

  function ShowNonModalOverModal(const AShowSheet: Boolean): Boolean;
  var
    AForm: TCustomForm;
  begin
    Result := False;
    AForm := TCustomForm(AWinControl);
    ModalForm := nil;
    if AWinControl.HandleObjectShouldBeVisible and
      not (csDesigning in AForm.ComponentState) and
      not (fsModal in AForm.FormState) and
      (AForm.FormStyle <> fsMDIChild) and
      not (AForm.FormStyle in fsAllStayOnTop) and
      HaveModalForms and
      (AForm.BorderStyle in [bsDialog, bsSingle, bsSizeable]) and
      (AForm.Parent = nil) and
      (AForm.PopupParent = nil) and (AForm.PopupMode = pmNone) then
    begin
      if AShowSheet then
      begin
        TCarbonWindow(ModalForm.Handle).SheetWin := TCarbonWindow(AForm.Handle).Window;
        OSError(ShowSheetWindow(TCarbonWindow(AForm.Handle).Window, TCarbonWindow(ModalForm.Handle).Window),
          Self,'ShowHide','ShowSheetWindow');
      end;

      Result := True;
    end;
  end;
begin
  if not CheckHandle(AWinControl, Self, 'ShowHide') then Exit;

  OSError(GetWindowClass(TCarbonWindow(AWinControl.Handle).Window, ACurrentWindowClass{%H-}),
    Self,'ShowHide','GetWindowClass');

  if AWinControl.HandleObjectShouldBeVisible then
  begin
    case TCustomForm(AWinControl).WindowState of
      wsFullScreen: nCmdShow := SW_SHOWFULLSCREEN;
      wsMaximized: nCmdShow := SW_SHOWMAXIMIZED;
      wsMinimized: nCmdShow := SW_SHOWMINIMIZED;
    else
      nCmdShow := SW_SHOW;
    end;
    if (ACurrentWindowClass <> kSheetWindowClass) and ShowNonModalOverModal(False) then
    begin
      CREATESHEETWINDOW := PtrUInt(AWinControl);
      RecreateWnd(AWinControl);
      exit;
    end;
    if (ACurrentWindowClass = kSheetWindowClass) and
      not ShowNonModalOverModal(False) then
    begin
      RecreateWnd(AWinControl);
      exit;
    end;

    if (ACurrentWindowClass = kSheetWindowClass) then
      ShowNonModalOverModal(True);

    TCarbonWindow(AWinControl.Handle).ShowHide(True);
    TCarbonWindow(AWinControl.Handle).Show(nCmdShow);
  end else
  begin
    if not (csDesigning in AWinControl.ComponentState) then
    begin
      if TCarbonWindow(AWinControl.Handle).SheetWin <> nil then
      begin
        HideSheetWindow(TCarbonWindow(AWinControl.Handle).SheetWin);
        TCarbonWindow(AWinControl.Handle).SheetWin := nil;
      end else
      if (ACurrentWindowClass = kSheetWindowClass) then
        HideSheetWindow(TCarbonWindow(AWinControl.Handle).Window);
    end;
    TCarbonWindow(AWinControl.Handle).ShowHide(False);
  end;
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

class procedure TCarbonWSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
  {ToDo: implement PopupMode/PopupParent for Carbon}
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
