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

{$mode objfpc}{$H+}

interface

uses
  // Libs
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets, qtobjects,
  // LCL
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages, Graphics,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses;

type

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
//    class procedure SetShortcut(const AButton: TCustomButton; const OldShortcut, NewShortcut: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
//    class procedure GetPreferredSize(const AWinControl: TWinControl;
//                        var PreferredWidth, PreferredHeight: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TBitmap); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses QtWSControls;

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
begin
  QtPushButton := TQtPushButton.Create(AWinControl, AParams);
  QtPushButton.AttachEvents;

  // Focus

  QWidget_setFocusPolicy(QtPushButton.Widget, QtStrongFocus);

  // Returns the Handle

  Result := THandle(QtPushButton);
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

  if not WSCheckHandleAllocated(AWincontrol, 'GetText') then Exit;

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
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

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
  if not WSCheckHandleAllocated(AWincontrol, 'SetColor') then Exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color:=ColorToRGB(AWinControl.Color);

  // Fill QColor
  QColor_setRgb(QColorH(@QColor),Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtAbstractButton(AWinControl.Handle).SetColor(@QColor);
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetDefault
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
var
  APushButton: QPushButtonH;
begin
  APushButton := QPushButtonH(TQtAbstractButton(AButton.Handle).Widget);
  QPushButton_setDefault(APushButton, ADefault);
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetGlyph
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TBitmap);
var
  AButton: QAbstractButtonH;
  AIcon: QIconH;
  APixmap: QPixmapH;
begin
  AButton := QAbstractButtonH(TQtAbstractButton(Abitbtn.Handle).Widget);

  APixmap := QPixmap_create();
  QPixmap_fromImage(APixmap, TQtImage(AValue.Handle).Handle);
  try
    if APixmap <> nil then
    begin
      AIcon := QIcon_create(APixmap);
      QAbstractButton_setIcon(AButton, AIcon);
    end;
  finally
    QPixmap_destroy(APixmap);
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomBitBtn, TQtWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TQtWSSpeedButton);
////////////////////////////////////////////////////
end.
