{ $Id$}
{
 *****************************************************************************
 *                             GtkWSExtCtrls.pp                              * 
 *                             ----------------                              * 
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
unit GtkWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
  LCLProc, LCLType, LCLIntf, LMessages,
{$IFDEF GTK2}
  gtk2, gdk2, gdk2PixBuf, glib2,
{$ELSE GTK2}
  gtk, gdk, glib, gtk1WSPrivate, graphics,
{$ENDIF GTK2}
  GtkExtra, GtkWsControls,
  GtkGlobals, GtkProc, GtkDef, GtkInt,
  SysUtils, Classes, Controls, ExtCtrls, Forms, Menus,
  WSExtCtrls, WSLCLClasses, WSProc, InterfaceBase;

type

  { TGtkWSPage }

  TGtkWSPage = class(TWSPage)
  published
  end;

  { TGtkWSNotebook }

  TGtkWSNotebook = class(TWSNotebook)
  published
  end;

  { TGtkWSShape }

  TGtkWSShape = class(TWSShape)
  published
  end;

  { TGtkWSCustomSplitter }

  TGtkWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TGtkWSSplitter }

  TGtkWSSplitter = class(TWSSplitter)
  published
  end;

  { TGtkWSPaintBox }

  TGtkWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TGtkWSCustomImage }

  TGtkWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TGtkWSImage }

  TGtkWSImage = class(TWSImage)
  published
  end;

  { TGtkWSBevel }

  TGtkWSBevel = class(TWSBevel)
  published
  end;

  { TGtkWSCustomRadioGroup }

  TGtkWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TGtkWSRadioGroup }

  TGtkWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TGtkWSCustomCheckGroup }

  TGtkWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TGtkWSCheckGroup }

  TGtkWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TGtkWSCustomLabeledEdit }

  TGtkWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TGtkWSLabeledEdit }

  TGtkWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TGtkWSCustomPanel }

  TGtkWSCustomPanel = class(TWSCustomPanel)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSPanel }

  TGtkWSPanel = class(TWSPanel)
  published
  end;

  { TGtkWSCustomTrayIcon }

{$IFDEF GTK1}
  TGtkWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    class function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; override;
  end;
{$ENDIF}

implementation

{$IFDEF GTK1}
uses
  x, xlib, xutil;
{$ENDIF}

class procedure TGtkWSCustomPanel.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := TGtkWidgetset(Widgetset).CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);
  Include(WidgetInfo^.Flags, wwiNoEraseBkgnd);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtkWSCustomPanel.SetColor(const AWinControl: TWinControl);
var
  MainWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;
  MainWidget:=GetFixedWidget(pGtkWidget(AWinControl.handle));
  if MainWidget<>nil then
  GtkWidgetSet.SetWidgetColor(MainWidget,
                              AWinControl.font.color, AWinControl.color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);

  UpdateWidgetStyleOfControl(AWinControl);
end;

{$IFDEF GTK1}
  {$include gtk1trayicon.inc}
{$ENDIF}

end.
