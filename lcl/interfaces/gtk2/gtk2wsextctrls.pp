{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtCtrls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSExtCtrls;

{$I gtk2defines.inc}
{$define UseStatusIcon} // can be used only with fpc r13008, from 2009


{$mode objfpc}{$H+}

interface

uses
  // RTL
  GLib2, Gtk2, Gdk2, Gdk2Pixbuf,
  Classes, SysUtils, Types,
  // LCL
  Gtk2Int, Gtk2Def,
  {$ifdef UseStatusIcon}Gtk2Ext, {$endif}
  LCLProc, ExtCtrls, Controls, Graphics, LCLType,
  // widgetset
  WSExtCtrls, WSLCLClasses,
  Gtk2WSControls, Gtk2Proc, Gtk2Globals;

type

  { TGtk2WSPage }

  TGtk2WSPage = class(TWSPage)
  published
  end;

  { TGtk2WSNotebook }

  TGtk2WSNotebook = class(TWSNotebook)
  published
  end;

  { TGtk2WSShape }

  TGtk2WSShape = class(TWSShape)
  published
  end;

  { TGtk2WSCustomSplitter }

  TGtk2WSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TGtk2WSSplitter }

  TGtk2WSSplitter = class(TWSSplitter)
  published
  end;

  { TGtk2WSPaintBox }

  TGtk2WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtk2WSCustomImage }

  TGtk2WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtk2WSImage }

  TGtk2WSImage = class(TWSImage)
  published
  end;

  { TGtk2WSBevel }

  TGtk2WSBevel = class(TWSBevel)
  published
  end;

  { TGtk2WSCustomRadioGroup }

  TGtk2WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtk2WSRadioGroup }

  TGtk2WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtk2WSCustomCheckGroup }

  TGtk2WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtk2WSCheckGroup }

  TGtk2WSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TGtk2WSBoundLabel }

  {TGtk2WSBoundLabel = class(TWSBoundLabel)
  private
  protected
  public
  end;}

  { TGtk2WSCustomLabeledEdit }

  TGtk2WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtk2WSLabeledEdit }

  TGtk2WSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TGtk2WSCustomPanel }

  TGtk2WSCustomPanel = class(TWSCustomPanel)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDefaultColor(const {%H-}AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtk2WSPanel }

  TGtk2WSPanel = class(TWSPanel)
  published
  end;

  { TGtk2WSCustomTrayIcon }

  TGtk2WSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

{$ifdef HasX}
uses
  {$ifdef HasGdk2X}
    gdk2x,
  {$endif}
  x, xlib;
{$endif}

{ TGtk2WSCustomPanel }

class procedure TGtk2WSCustomPanel.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtk2WSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Frame, WidgetClient: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
  bwidth: gint;
  Style: PGtkRCStyle;
  BorderStyle: TBorderStyle;
begin
  Frame := gtk_frame_new(nil);
  BorderStyle:=TCustomControl(AWinControl).BorderStyle;
  gtk_frame_set_shadow_type(PGtkFrame(Frame),BorderStyleShadowMap[BorderStyle]);

  case BorderStyle of
  bsSingle:
    bwidth:=1;
  else
    bwidth:=0
  end;
  Style := gtk_widget_get_modifier_style(Frame);
  Style^.xthickness := bwidth;
  Style^.ythickness := bwidth;
  gtk_widget_modify_style(Frame, Style);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Frame, dbgsName(AWinControl));
  {$ENDIF}

  WidgetClient := CreateFixedClientWidget(True);

  gtk_container_add(GTK_CONTAINER(Frame), WidgetClient);
  GTK_WIDGET_SET_FLAGS(Frame, GTK_CAN_FOCUS);

  WidgetInfo := CreateWidgetInfo(Frame, AWinControl, AParams);
  WidgetInfo^.ClientWidget := WidgetClient;
  WidgetInfo^.CoreWidget := Frame;
  WidgetInfo^.LCLObject := AWinControl;
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

  g_object_set_data(PGObject(WidgetClient), 'widgetinfo', WidgetInfo);

  gtk_widget_show_all(Frame);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Frame, @Allocation);

  //debugln(['TGtk2WSCustomPanel.CreateHandle Frame^.allocation=',dbgs(Frame^.allocation),' WidgetClient^.allocation=',dbgs(WidgetClient^.allocation)]);

  Set_RC_Name(AWinControl, Frame);

  // issue #23940. Hide panel if we are not visible, but before setting callbacks.
  // so it won't trigger unnecessary events to LCL.
  if not AWinControl.HandleObjectShouldBeVisible and not (csDesigning in AWinControl.ComponentState) then
    gtk_widget_hide(Frame);

  SetCallbacks(Frame, WidgetInfo);

  Result := TLCLIntfHandle({%H-}PtrUInt(Frame));
end;

class function TGtk2WSCustomPanel.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBackground,
 { dctFont } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

class procedure TGtk2WSCustomPanel.SetColor(const AWinControl: TWinControl);
var
  MainWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  MainWidget:=GetFixedWidget({%H-}pGtkWidget(AWinControl.handle));
  if MainWidget<>nil then
  Gtk2WidgetSet.SetWidgetColor(MainWidget,
                              AWinControl.Font.Color, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);

  UpdateWidgetStyleOfControl(AWinControl);
end;


{$include gtk2trayicon.inc}

end.
