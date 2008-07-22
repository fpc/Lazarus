{ $Id$}
{
 *****************************************************************************
 *                               QtWSForms.pp                                * 
 *                               ------------                                * 
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
unit QtWSForms;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtobjects, qtwidgets, qtproc,
  // LCL
  SysUtils, Classes, Controls, LCLType, Forms,
  // Widgetset
  InterfaceBase, WSForms, WSProc, WSLCLClasses;

type

  { TQtWSScrollingWinControl }

  TQtWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TQtWSScrollBox }

  TQtWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
      const DeltaX, DeltaY: integer); override;
  end;

  { TQtWSCustomFrame }

  TQtWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TQtWSFrame }

  TQtWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TQtWSCustomForm }

  TQtWSCustomForm = class(TWSCustomForm)
  private
    class function GetQtBorderStyle(const AFormBorderStyle: TFormBorderStyle): QtWindowFlags;
    class function GetQtBorderIcons(const ABorderIcons: TBorderIcons): QtWindowFlags;
    class function GetQtFormStyle(const AFormStyle: TFormStyle): QtWindowFlags;
    class procedure UpdateWindowFlags(const AWidget: TQtMainWindow;
      ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons; AFormStyle: TFormStyle);
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
  end;

  { TQtWSForm }

  TQtWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TQtWSHintWindow }

  TQtWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSScreen }

  TQtWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TQtWSApplicationProperties }

  TQtWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

uses QtWSControls;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Qt Form and initializes it according to it's properties
 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtMainWindow: TQtMainWindow;
  Str: WideString;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSCustomForm.CreateHandle] Height: ', IntToStr(AWinControl.Height),
     ' Width: ', IntToStr(AWinControl.Width));
  {$endif}

  // Creates the window

  if csDesigning in AWinControl.ComponentState then
    QtMainWindow := TQtDesignWidget.Create(AWinControl, AParams)
  else
    QtMainWindow := TQtMainWindow.Create(AWinControl, AParams);
  
  // Set´s initial properties

  Str := GetUtf8String(AWinControl.Caption);

  QtMainWindow.SetWindowTitle(@Str);

  if not (csDesigning in TCustomForm(AWinControl).ComponentState) then
  begin
    UpdateWindowFlags(QtMainWindow, TCustomForm(AWinControl).BorderStyle,
      TCustomForm(AWinControl).BorderIcons, TCustomForm(AWinControl).FormStyle);
  end;

  if (TCustomForm(AWinControl).ShowInTaskBar in [stDefault, stNever]) and not
     (TCustomForm(AWinControl).FormStyle in [fsMDIChild]) and 
     {QtTool have not minimize button !}
     {$ifdef linux}
     not (TCustomForm(AWinControl).BorderStyle in [bsSizeToolWin, bsToolWindow]) and
     {$endif}
     (Application <> nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm.HandleAllocated) and
     (Application.MainForm <> AWinControl) then
    QtMainWindow.setShowInTaskBar(False);

  // Sets Various Events
  QtMainWindow.AttachEvents;
  
  if (TCustomForm(AWinControl).FormStyle in [fsMDIChild]) and
     (Application.MainForm.FormStyle = fsMdiForm) and
     not (csDesigning in AWinControl.ComponentState) then
  begin
    QMdiArea_addSubWindow(TQtMainWindow(Application.MainForm.Handle).MDIAreaHandle, QtMainWindow.Widget, QtWindow);
    QWidget_setFocusProxy(QtMainWindow.Widget, QtMainWindow.getContainerWidget);
  end;
    

  // Return the handle
  Result := TLCLIntfHandle(QtMainWindow);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.CloseModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  inherited CloseModal(ACustomForm);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetFormBorderStyle
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  UpdateWindowFlags(TQtMainWindow(AForm.Handle), AFormBorderStyle, AForm.BorderIcons, AForm.FormStyle);
end;

class procedure TQtWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle: TFormStyle);
begin
  UpdateWindowFlags(TQtMainWindow(AForm.Handle), AForm.BorderStyle, AForm.BorderIcons, AFormStyle);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetIcon
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
var
  Icon: TQtIcon;
begin
  Icon := TQtIcon(AIcon);
  if Icon <> nil then
    TQtWidget(AForm.Handle).setWindowIcon(Icon.Handle)
  else
    TQtWidget(AForm.Handle).setWindowIcon(nil);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetShowInTaskbar
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar);
var
  Enable: Boolean;
begin
  if (AForm.Parent<>nil) or not (AForm.HandleAllocated) then exit;

  Enable := AValue <> stNever;
  if (AValue = stDefault) and
     (Application<>nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm <> AForm) then
    Enable := false;
  TQtMainWindow(AForm.Handle).setShowInTaskBar(Enable);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.ShowModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  {
    Setting modal flags is done in TQtWSCustomControl.ShowHide
    Since that flags has effect only when Widget is not visible
    
    We can ofcource hide widget, set flags here and then show it, but we dont
    want window flickering :)
  }
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetBorderIcons
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  UpdateWindowFlags(TQtMainWindow(AForm.Handle), AForm.BorderStyle, ABorderIcons, AForm.FormStyle);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.GetQtWindowBorderStyle
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.GetQtBorderStyle(const AFormBorderStyle: TFormBorderStyle): QtWindowFlags;
begin
  case AFormBorderStyle of
    bsNone:
      Result := QtWindow or QtFramelessWindowHint;
    bsSingle:
      Result := QtWindow or QtMSWindowsFixedSizeDialogHint;
    bsSizeable:
      Result := QtWindow;
    bsDialog:
      Result := QtDialog;
    bsToolWindow:
      Result := QtTool or QtMSWindowsFixedSizeDialogHint;
    bsSizeToolWin:
      // qt on most platforms (except windows) doesn't have sizeToolWin, it's regular qtWindow
      Result := {$ifdef windows}QtTool{$else}QtWindow{$endif}; 
    else
      Result := QtWidget;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetQtBorderIcons
  Params:  None
  Returns: Nothing

  Same comment as SetQtWindowBorderStyle above
 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.GetQtBorderIcons(const ABorderIcons: TBorderIcons): QtWindowFlags;
begin
  Result := 0;
  
  if (biSystemMenu in ABorderIcons) then
    Result := Result or QtWindowSystemMenuHint;

  if (biMinimize in ABorderIcons) then
    Result := Result or QtWindowMinimizeButtonHint;

  if (biMaximize in ABorderIcons) then
    Result := Result or QtWindowMaximizeButtonHint;

  if (biHelp in ABorderIcons) then
    Result := Result or QtWindowContextHelpButtonHint;
end;

class function TQtWSCustomForm.GetQtFormStyle(const AFormStyle: TFormStyle
  ): QtWindowFlags;
begin
  if AFormStyle = fsStayOnTop then
    Result := QtWindowStaysOnTopHint
  else
    Result := 0;
end;

class procedure TQtWSCustomForm.UpdateWindowFlags(const AWidget: TQtMainWindow;
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons; AFormStyle: TFormStyle);
var
  Flags: QtWindowFlags;
  AVisible: Boolean;
begin
  AVisible := AWidget.getVisible;
  Flags := GetQtBorderStyle(ABorderStyle) or GetQtFormStyle(AFormStyle);
  if (Flags and QtFramelessWindowHint) = 0 then
    Flags := Flags or GetQtBorderIcons(ABorderIcons);
  if not (csDesigning in AWidget.LCLObject.ComponentState) then
    AWidget.setWindowFlags(Flags);
  AWidget.setVisible(AVisible);
end;

{ TQtWSHintWindow }

class function TQtWSHintWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtMainWindow: TQtMainWindow;
begin
  QtMainWindow := TQtHintWindow.Create(AWinControl, AParams);
  // Sets Various Events
  QtMainWindow.AttachEvents;
  Result := TLCLIntfHandle(QtMainWindow);
end;

{ TQtWSScrollBox }

class procedure TQtWSScrollBox.ScrollBy(
  const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
var
  Widget: TQtAbstractScrollArea;
begin
  if not WSCheckHandleAllocated(AWinControl, 'ScollBy') then
    Exit;
  Widget := TQtAbstractScrollArea(AWinControl.Handle);
  Widget.viewport.scroll(-DeltaX, -DeltaY);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TQtWSScrollingWinControl);
  RegisterWSComponent(TScrollBox, TQtWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TQtWSCustomFrame);
//  RegisterWSComponent(TFrame, TQtWSFrame);
  RegisterWSComponent(TCustomForm, TQtWSCustomForm);
//  RegisterWSComponent(TForm, TQtWSForm);
  RegisterWSComponent(THintWindow, TQtWSHintWindow);
//  RegisterWSComponent(TScreen, TQtWSScreen);
//  RegisterWSComponent(TApplicationProperties, TQtWSApplicationProperties);
////////////////////////////////////////////////////
end.
