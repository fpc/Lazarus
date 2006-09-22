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
  qt4, qtprivate,
  // LCL
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages,
  // Widgetset
  WSButtons, WSLCLClasses;

type

  { TQtWSButton }

  TQtWSButton = class(TWSButton)
  private
    class procedure SetSlots(const QtButton: TQtPushButton);
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
//    class procedure ActiveDefaultButtonChanged(const AButton: TCustomButton); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
  end;

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  private
    class procedure SetSlots(const QtButton: TQtPushButton);
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
//    class procedure ActiveDefaultButtonChanged(const AButton: TCustomButton); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
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
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);

  SetSlots(QtPushButton);

  if AWinControl.Visible then QtPushButton.Show;

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
  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := string(Str);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSButton.SetSlots
  Params:  None
  Returns: Nothing
  
  Initializes the events
 ------------------------------------------------------------------------------}
class procedure TQtWSButton.SetSlots(const QtButton: TQtPushButton);
var
  Method: TMethod;
  Hook : QObject_hookH;
begin
  // Various Events

  Hook := QObject_hook_create(QtButton.Widget);

  TEventFilterMethod(Method) := QtButton.EventFilter;

  QObject_hook_hook_events(Hook, Method);
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
  Str := WideString(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
end;

{ TQtWSBitBtn }

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetSlots
  Params:  None
  Returns: Nothing

  Initializes the events
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetSlots(const QtButton: TQtPushButton);
var
  Method: TMethod;
begin
  // Inherited Callbacks
//  TQtWSWinControl.SetSlots(QtButton);

  // OnClick Event

  QAbstractButton_clicked2_Event(Method) := QtButton.SlotClicked;

  QAbstractButton_hook_hook_clicked2(QAbstractButton_hook_create(QtButton.Widget), Method);
end;

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
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);

  SetSlots(QtPushButton);

  if AWinControl.Visible then QtPushButton.Show;

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
class function TQtWSBitBtn.GetText(const AWinControl: TWinControl; var AText: String
  ): Boolean;
var
  Str: WideString;
begin
  TQtAbstractButton(AWinControl.Handle).Text(@Str);

  AText := string(Str);

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
  Str := WideString(AText);

  TQtAbstractButton(AWinControl.Handle).SetText(@Str);
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
