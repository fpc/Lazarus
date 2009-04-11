{ $Id$}
{
 *****************************************************************************
 *                              GtkWSExtDlgs.pp                              * 
 *                              ---------------                              * 
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
unit GtkWSExtDlgs;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, gdkpixbuf, GtkFontCache,
  {$ENDIF}
  Classes, Controls, ExtDlgs, LCLType,
  WSExtDlgs, WSLCLClasses,
  GtkDef, GtkProc, GtkWsControls, GtkInt;

type

  { TGtkWSPreviewFileControl }

  TGtkWSPreviewFileControl = class(TWSPreviewFileControl)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtkWSPreviewFileDialog }

  TGtkWSPreviewFileDialog = class(TWSPreviewFileDialog)
  published
  end;

  { TGtkWSOpenPictureDialog }

  TGtkWSOpenPictureDialog = class(TWSOpenPictureDialog)
  published
  end;

  { TGtkWSSavePictureDialog }

  TGtkWSSavePictureDialog = class(TWSSavePictureDialog)
  published
  end;

  { TGtkWSCalculatorDialog }

  TGtkWSCalculatorDialog = class(TWSCalculatorDialog)
  published
  end;

  { TGtkWSCalculatorForm }

  TGtkWSCalculatorForm = class(TWSCalculatorForm)
  published
  end;

  { TGtkWSCalendarDialogForm }

  TGtkWSCalendarDialogForm = class(TWSCalendarDialogForm)
  published
  end;

  { TGtkWSCalendarDialog }

  TGtkWSCalendarDialog = class(TWSCalendarDialog)
  published
  end;


implementation

{ TGtkWSPreviewFileControl }

class procedure TGtkWSPreviewFileControl.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

class function TGtkWSPreviewFileControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := GtkWidgetset.CreateSimpleClientAreaWidget(AWinControl, True);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(Widget));

  WidgetInfo := GetWidgetInfo(Widget);
  WidgetInfo^.LCLObject := AWinControl;
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  SetCallBacks(Widget, WidgetInfo);
end;

end.
