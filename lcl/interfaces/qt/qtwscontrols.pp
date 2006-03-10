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
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSControls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  qt4, qtprivate,
  // LCL
  Controls, Forms, LCLType,
  // Widgetset
  WSControls, WSLCLClasses;

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
    class procedure SetSlots(const AWidget: QWidgetH);
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  public
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
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
  Function: TQtWSButton.SetSlots
  Params:  None
  Returns: Nothing
  
  Initializes the basic events for all controls with handles
 ------------------------------------------------------------------------------}
procedure TQtWSWinControl.SetSlots(const AWidget: QWidgetH);
begin
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtWidget: TQtWidget;
begin
  QtWidget := TQtWidget.Create(AWinControl, AParams);

  SetSlots(QtWidget.Widget);

  Result := THandle(QtWidget);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
procedure TQtWSWinControl.SetBounds(const AWinControl: TWinControl;
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
procedure TQtWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  if AWinControl = nil then exit;

  if not AWinControl.HandleAllocated then exit;

  if AWinControl.HandleObjectShouldBeVisible then
   QWidget_setVisible(TQtWidget(AWinControl.Handle).Widget, True)
  else QWidget_setVisible(TQtWidget(AWinControl.Handle).Widget, False);
  
  {$ifdef VerboseQt}
    if AWinControl is TForm then WriteLn('Is TForm');

    if AWinControl.Visible then
     WriteLn('True') else WriteLn('False');
  {$endif}
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
