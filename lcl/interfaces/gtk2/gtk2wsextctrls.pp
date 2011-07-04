{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtCtrls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSExtCtrls;

{$I gtk2defines.inc}
{$define UseStatusIcon} // can be used only with fpc r13008, from 2009


{$mode objfpc}{$H+}

interface

uses
  // libs
  Math, GLib2, Gtk2, Gdk2, Gdk2Pixbuf, Gtk2Int, Gtk2Def, {$ifdef UseStatusIcon}Gtk2Ext, {$endif}
  // LCL
  LCLProc, ExtCtrls, Classes, Controls, SysUtils, Graphics, LCLType, LMessages,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSProc,
  Gtk2WSControls, Gtk2WSPrivate, Gtk2Proc, Gtk2Globals;

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

uses
{$ifdef HasX}
  x, xlib, xutil,
{$endif}
//  gtk2, gdk2, glib2, gtk2def, gtk2proc,
{$ifdef HasGdk2X}
  gdk2x,
{$endif}
  interfacebase;

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
begin
  Frame := gtk_frame_new(nil);
  gtk_frame_set_shadow_type(PGtkFrame(Frame),
    BorderStyleShadowMap[TCustomControl(AWinControl).BorderStyle]);

  case TCustomPanel(AWinControl).BorderStyle of
    bsSingle:
      bwidth:=1
    else
      bwidth:=0
  end;
  style := gtk_widget_get_modifier_style(Frame);
  style^.xthickness := bwidth;
  style^.ythickness := bwidth;
  gtk_widget_modify_style(Frame, style);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Frame, dbgsName(AWinControl));
  {$ENDIF}

  WidgetClient := CreateFixedClientWidget(True);

  gtk_container_add(GTK_CONTAINER(Frame), WidgetClient);

  WidgetInfo := CreateWidgetInfo(Frame, AWinControl, AParams);
  WidgetInfo^.ClientWidget := WidgetClient;
  WidgetInfo^.CoreWidget := WidgetClient;
  WidgetInfo^.LCLObject := AWinControl;
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  g_object_set_data(PGObject(WidgetClient), 'widgetinfo', WidgetInfo);

  gtk_widget_show_all(Frame);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Frame, @Allocation);

  Set_RC_Name(AWinControl, Frame);
  SetCallbacks(Frame, WidgetInfo);

  Result := TLCLIntfHandle(PtrUInt(Frame));
end;

class procedure TGtk2WSCustomPanel.SetColor(const AWinControl: TWinControl);
var
  MainWidget: PGtkWidget;
  FontColor, BGColor: TColor;
begin
  if not AWinControl.HandleAllocated then exit;
  MainWidget:=GetFixedWidget(pGtkWidget(AWinControl.handle));
  if MainWidget<>nil then
  Gtk2WidgetSet.SetWidgetColor(MainWidget,
                              AWinControl.Font.Color, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);

  UpdateWidgetStyleOfControl(AWinControl);
end;


{$include gtk2trayicon.inc}

end.
