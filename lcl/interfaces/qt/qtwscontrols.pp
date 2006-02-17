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
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  qt4,
  Controls,
////////////////////////////////////////////////////
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
//  QAbstractButton_hook_hook_clicked

{  GtkWidgetSet.SetCallback(LM_SHOWWINDOW, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_DESTROY, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_FOCUS, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_WINDOWPOSCHANGED, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_PAINT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_EXPOSEEVENT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_CHAR, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEMOVE, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEWHEEL, AGTKObject, AComponent);}
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
