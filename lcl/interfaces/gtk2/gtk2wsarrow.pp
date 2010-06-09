{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSArrow.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSArrow;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Arrow,
////////////////////////////////////////////////////
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  Classes,
  // LCL
  Controls, LCLType,
  Arrow,
  // WS
  WSArrow, WSLCLClasses,
  // Gtk2WS
  Gtk2Proc, Gtk2Def, Gtk2WSControls;



type

  { TGtk2WSArrow }

  TGtk2WSArrow = class(TWSArrow)
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType;
      const AShadowType: TShadowType); override;
  end;


implementation

const
  LCLToGTKArrowTypeMap: array[TArrowType] of TGtkArrowType =
  (
{ atUp   } GTK_ARROW_UP,
{ atDown } GTK_ARROW_DOWN,
{ atLeft } GTK_ARROW_LEFT,
{ atRight} GTK_ARROW_RIGHT
  );

  LCLToGTKShadowTypeMap: array[TShadowType] of TGtkShadowType =
  (
{ stNone     } GTK_SHADOW_NONE,
{ stIn       } GTK_SHADOW_IN,
{ stOut      } GTK_SHADOW_OUT,
{ stEtchedIn } GTK_SHADOW_ETCHED_IN,
{ stEtchedOut} GTK_SHADOW_ETCHED_OUT
  );

{ TGtk2WSArrow }

class procedure TGtk2WSArrow.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSArrow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  EventBox, ArrowWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGtkAllocation;
begin
  EventBox := gtk_event_box_new();
  ArrowWidget := gtk_arrow_new(gtk_arrow_left, gtk_shadow_etched_in);
  gtk_container_add(PGtkContainer(EventBox), ArrowWidget);
  gtk_widget_show_all(EventBox);

  Result := TLCLIntfHandle(PtrUInt(EventBox));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(EventBox, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, EventBox);
  SetCallBacks(EventBox, WidgetInfo);
end;

class procedure TGtk2WSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
var
  ArrowWidget: PGtkWidget;
begin
  ArrowWidget := PGtkBin(AArrow.Handle)^.child;

  gtk_arrow_set(PGtkArrow(ArrowWidget),
    LCLToGTKArrowTypeMap[AArrowType],
    LCLToGTKShadowTypeMap[AShadowType]);
end;


end.
