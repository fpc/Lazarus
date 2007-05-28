{ $Id$}
{
 *****************************************************************************
 *                              QtWSControls.pp                              * 
 *                              ---------------                              * 
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
unit QtWSControls;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtwidgets,
  // LCL
  SysUtils, Controls, LCLType, LCLProc, Forms, Graphics,
  // Widgetset
  InterfaceBase, WSControls, WSLCLClasses;

type

  { TQtWSDragImageList }

  TQtWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TQtWSControl }

  TQtWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TQtWSWinControl }

  TQtWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
  public
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;

//    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

{    class procedure AddControl(const AControl: TControl); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;}
  end;

  { TQtWSGraphicControl }

  TQtWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TQtWSCustomControl }

  TQtWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TQtWSImageList }

  TQtWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

{------------------------------------------------------------------------------
  Function: TQtWSWinControl.CanFocus
  Params:  TWinControl
  Returns: Boolean
 ------------------------------------------------------------------------------}
class function TQtWSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget: TQtWidget;
  FocusWidget: QWidgetH;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := TQtWidget(AWinControl.Handle);
    if Assigned(Widget.LCLObject.Parent) then
    FocusWidget := QWidget_focusWidget(TQtWidget(Widget.LCLObject.Parent.Handle).Widget)
    else
    FocusWidget := QWidget_focusWidget(Widget.Widget);
    
    Result := (FocusWidget <> nil)
                and QWidget_isEnabled(FocusWidget)
                and QWidget_isVisible(FocusWidget)
                and (QWidget_focusPolicy(FocusWidget) <> QtNoFocus);
  end else
    Result := False;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtWidget: TQtWidget;
  Method: TMethod;
  Hook : QObject_hookH;
begin
  {$ifdef VerboseQt}
    WriteLn('> TQtWSWinControl.CreateHandle for ',dbgsname(AWinControl));
  {$endif}
  QtWidget := TQtWidget.Create(AWinControl, AParams);

  // Various Events

  Hook := QObject_hook_create(QtWidget.Widget);

  TEventFilterMethod(Method) := QtWidget.EventFilter;

  QObject_hook_hook_events(Hook, Method);
  
  // Finalization

  Result := THandle(QtWidget);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSWinControl.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.Invalidate
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Update;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  QWidget_move(TQtWidget(AWinControl.Handle).Widget, ALeft, ATop);
  QWidget_resize(TQtWidget(AWinControl.Handle).Widget, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetPos
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
  Returns: Nothing

  Sets the position of a widget
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
begin
  QWidget_move(TQtWidget(AWinControl.Handle).Widget, ALeft, ATop);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetSize
  Params:  AWinControl     - the calling object
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the size of a widget
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
begin
  QWidget_resize(TQtWidget(AWinControl.Handle).Widget, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.ShowHide
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Shows or hides a widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSWinControl.ShowHide]');
  {$endif}

  if AWinControl = nil then exit;

  if not AWinControl.HandleAllocated then exit;

  { if the widget is a form, this is a place to set the Tab order }
  if (AWinControl is TForm) and AWinControl.HandleObjectShouldBeVisible then
   TQtMainWindow(AWinControl.Handle).SetTabOrders;

  if AWinControl.HandleObjectShouldBeVisible then
   QWidget_setVisible(TQtWidget(AWinControl.Handle).Widget, True)
  else QWidget_setVisible(TQtWidget(AWinControl.Handle).Widget, False);
  
  {$ifdef VerboseQt}
    Write('Trace:< [TQtWSWinControl.ShowHide] ');

    if AWinControl is TForm then Write('Is TForm, ');

    if AWinControl.HandleObjectShouldBeVisible then WriteLn('Visible: True')
    else WriteLn('Visible: False');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetColor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  Color: TColor;
begin
  if AWinControl = nil then exit;

  if not AWinControl.HandleAllocated then exit;

  if AWinControl.Color = CLR_INVALID then exit;

  // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
  Color := ColorToRGB(AWinControl.Color);
  
  // Fill QColor
  QColor_setRgb(@QColor,Red(Color),Green(Color),Blue(Color));

  // Set color of the widget to QColor
  TQtWidget(AWinControl.Handle).SetColor(@QColor);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetCursor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the cursor of the widget.
 ------------------------------------------------------------------------------}
class procedure TQtWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
begin
  if AWinControl = nil then exit;
  if not AWinControl.HandleAllocated then exit;
  
  TQtWidget(AWinControl.Handle).SetCursor(QCursorH(ACursor));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TQtWSDragImageList);
//  RegisterWSComponent(TControl, TQtWSControl);
  RegisterWSComponent(TWinControl, TQtWSWinControl);
//  RegisterWSComponent(TGraphicControl, TQtWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TQtWSCustomControl);
//  RegisterWSComponent(TImageList, TQtWSImageList);
////////////////////////////////////////////////////
end.
