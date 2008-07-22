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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  private
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtkWSPreviewFileDialog }

  TGtkWSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TGtkWSOpenPictureDialog }

  TGtkWSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TGtkWSSavePictureDialog }

  TGtkWSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TGtkWSCalculatorDialog }

  TGtkWSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TGtkWSCalculatorForm }

  TGtkWSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TGtkWSCalendarDialogForm }

  TGtkWSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TGtkWSCalendarDialog }

  TGtkWSCalendarDialog = class(TWSCalendarDialog)
  private
  protected
  public
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TPreviewFileControl, TGtkWSPreviewFileControl);
//  RegisterWSComponent(TPreviewFileDialog, TGtkWSPreviewFileDialog);
//  RegisterWSComponent(TOpenPictureDialog, TGtkWSOpenPictureDialog);
//  RegisterWSComponent(TSavePictureDialog, TGtkWSSavePictureDialog);
//  RegisterWSComponent(TCalculatorDialog, TGtkWSCalculatorDialog);
//  RegisterWSComponent(TCalculatorForm, TGtkWSCalculatorForm);
//  RegisterWSComponent(TCalendarDialogForm, TGtkWSCalendarDialogForm);
//  RegisterWSComponent(TCalendarDialog, TGtkWSCalendarDialog);
////////////////////////////////////////////////////
end.
