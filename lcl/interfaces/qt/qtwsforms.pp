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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSForms;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtwidgets,
  // LCL
  SysUtils, Controls, LCLType, Forms,
  // Widgetset
  InterfaceBase, WSForms, WSLCLClasses;

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
    class procedure SetSlots(const QtMainWindow: TQtMainWindow);
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
     const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
     const ABorderIcons: TBorderIcons); override;
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

{ TQtWSCustomForm }

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetSlots
  Params:  None
  Returns: Nothing

  Initializes the events
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetSlots(const QtMainWindow: TQtMainWindow);
var
  Method: TMethod;
  Hook : QObject_hookH;
begin
  // Various Events

  Hook := QObject_hook_create(QtMainWindow.Widget); // PaintBox
  
  TEventFilterMethod(Method) := QtMainWindow.EventFilter;
  
  QObject_hook_hook_events(Hook, Method);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Qt Form and initializes it according to it´s properties
 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtMainWindow: TQtMainWindow;
var
  Str: WideString;
begin
  QtMainWindow := TQtMainWindow.Create(AWinControl, AParams);

  Str := WideString(AWinControl.Caption);

  QtMainWindow.SetWindowTitle(@Str);

  SetSlots(QtMainWindow);

  Result := THandle(QtMainWindow);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.DestroyHandle
  Params:  None
  Returns: Nothing

  Destroys a Qt Form and releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtMainWindow(AWinControl.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.GetText
  Params:  AWinControl     - the calling object
           AText           - The Text
  Returns: Nothing

 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtWidget(AWinControl.Handle).WindowTitle(@Str);

  AText := String(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetText
  Params:  AWinControl     - the calling object
           AText           - The Text
  Returns: Nothing

 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetText(const AWinControl: TWinControl; const AText: string);
var
  Str: WideString;
begin
  Str := WideString(AText);

  TQtWidget(AWinControl.Handle).SetWindowTitle(@Str);
end;

class procedure TQtWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  inherited CloseModal(ACustomForm);
end;

class procedure TQtWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  inherited SetFormBorderStyle(AForm, AFormBorderStyle);
end;

class procedure TQtWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
begin
  inherited SetIcon(AForm, AIcon);
end;

class procedure TQtWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  inherited SetShowInTaskbar(AForm, AValue);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.ShowModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  QtDialog: TQtDialog;
begin
  QtDialog := TQtDialog.Create;
  try
    TQtWidget(ACustomForm.Handle).setParent(QtDialog.Widget);

    if QtDialog.exec = Integer(QDialogRejected) then ACustomForm.ModalResult := mrCancel
    else ACustomForm.ModalResult := mrOk;
    
    TQtWidget(ACustomForm.Handle).setParent(nil);
  finally
    QtDialog.Free;
  end;
end;

class procedure TQtWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  inherited SetBorderIcons(AForm, ABorderIcons);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TQtWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TQtWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TQtWSCustomFrame);
//  RegisterWSComponent(TFrame, TQtWSFrame);
  RegisterWSComponent(TCustomForm, TQtWSCustomForm);
//  RegisterWSComponent(TForm, TQtWSForm);
//  RegisterWSComponent(THintWindow, TQtWSHintWindow);
//  RegisterWSComponent(TScreen, TQtWSScreen);
//  RegisterWSComponent(TApplicationProperties, TQtWSApplicationProperties);
////////////////////////////////////////////////////
end.
