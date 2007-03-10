{ $Id$}
{
 *****************************************************************************
 *                              QtWSButtons.pp                               * 
 *                              --------------                               * 
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
unit QtWSButtons;

{$mode delphi}{$H+}

interface

uses
  // Libs
  qt4, qtwidgets,
  // LCL
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages, Graphics,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses;

type

  { TQtWSButton }

  TQtWSButton = class(TWSButton)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses QtWSControls;

{ TQtWSButton }

{------------------------------------------------------------------------------
  Function: TQtWSButton.CreateHandle
  Params:  None
  Returns: Nothing
  
  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtPushButton: TQtPushButton;
  Method: TMethod;
  Hook : QObject_hookH;
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);

  // Various Events

  Hook := QObject_hook_create(QtPushButton.Widget);

  TEventFilterMethod(Method) := QtPushButton.EventFilter;

  QObject_hook_hook_events(Hook, Method);
  
  // OnClick Event

  QAbstractButton_clicked2_Event(Method) := QtPushButton.SlotClicked;

  QAbstractButton_hook_hook_clicked2(QAbstractButton_hook_create(QtPushButton.Widget), Method);

  // Focus

  QWidget_setFocusPolicy(QtPushButton.Widget, QtStrongFocus);

  // Returns the Handle

  Result := THandle(QtPushButton);
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtPushButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWincontrol, 'GetText')
  then Exit;

  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  Str: WideString;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSButton.SetColor
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{ TQtWSBitBtn }

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TQtWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtPushButton: TQtPushButton;
  Method: TMethod;
  Hook : QObject_hookH;
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);

  // Various Events

  Hook := QObject_hook_create(QtPushButton.Widget);

  TEventFilterMethod(Method) := QtPushButton.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // OnClick Event

  QAbstractButton_clicked2_Event(Method) := QtPushButton.SlotClicked;

  QAbstractButton_hook_hook_clicked2(QAbstractButton_hook_create(QtPushButton.Widget), Method);

  // Focus

  QWidget_setFocusPolicy(QtPushButton.Widget, QtStrongFocus);

  // Returns the Handle

  Result := THandle(QtPushButton);
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.DestroyHandle
  Params:  None
  Returns: Nothing

  Releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtPushButton(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSBitBtn.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWincontrol, 'GetText')
  then Exit;

  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  Str: WideString;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;
  Str := UTF8Decode(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSBitBtn.SetColor
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor')
  then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomButton, TQtWSButton);
  RegisterWSComponent(TCustomBitBtn, TQtWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TQtWSSpeedButton);
////////////////////////////////////////////////////
end.
