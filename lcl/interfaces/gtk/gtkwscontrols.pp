{ $Id$}
{
 *****************************************************************************
 *                             GtkWSControls.pp                              * 
 *                             ----------------                              * 
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
unit GtkWSControls;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF GTK2} Gtk2, Glib2, {$ELSE} Gtk, Glib, {$ENDIF}
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  Classes, LMessages,
  WSControls, WSLCLClasses;

type

  { TGtkWSDragImageList }

  TGtkWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TGtkWSControl }

  TGtkWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TGtkWSWinControl }

  TGtkWSWinControl = class(TWSWinControl)
  private
  protected
  public
    // Internal public
    class procedure SetCallbacks(const AGTKObject: PGTKObject; const AComponent: TComponent);
  public
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetCursor(const AControl: TControl; const ACursor: TCursor); override;
  end;

  { TGtkWSGraphicControl }

  TGtkWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TGtkWSCustomControl }

  TGtkWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TGtkWSImageList }

  TGtkWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

uses
  GtkProc, GtkDef, GtkInt;

{ TGtkWSWinControl }
  
procedure TGtkWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  Allocation: TGTKAllocation;
begin                
  Allocation.X := gint16(ALeft);
  Allocation.Y := gint16(ATop);
  Allocation.Width := guint16(AWidth);
  Allocation.Height := guint16(AHeight);
  gtk_widget_size_allocate(PGtkWidget(AWinControl.Handle), @Allocation);
end;

procedure TGtkWSWinControl.SetCallbacks(const AGTKObject: PGTKObject; const AComponent: TComponent);
//TODO: Remove ALCLObject when the creation splitup is finished
begin
  GtkWidgetSet.SetCallback(LM_SHOWWINDOW, AGTKObject, AComponent);
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
  GtkWidgetSet.SetCallback(LM_MOUSEWHEEL, AGTKObject, AComponent);
end;

procedure TGtkWSWinControl.SetCursor(const AControl: TControl; const ACursor: TCursor);
begin
  GtkProc.SetCursor(AControl as TWinControl, ACursor); 
end;

procedure TGtkWSWinControl.SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); 
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin                
  Widget := PGtkWidget(AWinControl.Handle);
  Allocation.X := gint16(ALeft);
  Allocation.Y := gint16(ATop);
  Allocation.Width := guint16(Widget^.Allocation.Width);
  Allocation.Height := guint16(Widget^.Allocation.Height);
  gtk_widget_size_allocate(Widget, @Allocation);
end;

procedure TGtkWSWinControl.SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); 
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin                
  Widget := PGtkWidget(AWinControl.Handle);
  Allocation.X := Widget^.Allocation.X;
  Allocation.Y := Widget^.Allocation.Y;
  Allocation.Width := guint16(AWidth);
  Allocation.Height := guint16(AHeight);
  gtk_widget_size_allocate(Widget, @Allocation);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TGtkWSDragImageList);
//  RegisterWSComponent(TControl, TGtkWSControl);
  RegisterWSComponent(TWinControl, TGtkWSWinControl);
//  RegisterWSComponent(TGraphicControl, TGtkWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TGtkWSCustomControl);
//  RegisterWSComponent(TImageList, TGtkWSImageList);
////////////////////////////////////////////////////
end.
