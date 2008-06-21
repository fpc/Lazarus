{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSForms.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSForms;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  Gtk2, Glib2, Gdk2, Gdk2Pixbuf,
  // RTL, FCL, LCL
  SysUtils, Classes, LCLProc, LCLType, Controls, LMessages, InterfaceBase,
  Graphics, Dialogs,Forms, Math,
  // Widgetset
  WSDialogs, WSLCLClasses, WSControls, WSForms, WSProc,
  Gtk2Int, GtkProc, gtk2proc, GtkDef, GtkExtra, GtkGlobals, Gtk2WSControls,
  // Gtk 1 stuff
  gtkwsforms;

type

  { TGtk2WSScrollingWinControl }

  TGtk2WSScrollingWinControl = class(TGtkWSScrollingWinControl)
  private
  protected
  public
  end;

  { TGtk2WSScrollBox }

  TGtk2WSScrollBox = class(TGtkWSScrollBox)
  private
  protected
  public
  end;

  { TGtk2WSCustomFrame }

  TGtk2WSCustomFrame = class(TGtkWSCustomFrame)
  private
  protected
  public
  end;

  { TGtk2WSFrame }

  TGtk2WSFrame = class(TGtkWSFrame)
  private
  protected
  public
  end;

  { TGtk2WSCustomForm }

  TGtk2WSCustomForm = class(TGtkWSCustomForm)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;

{    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;}
  end;

  { TGtk2WSForm }

  TGtk2WSForm = class(TGtkWSForm)
  private
  protected
  public
  end;

  { TGtk2WSHintWindow }

  TGtk2WSHintWindow = class(TGtkWSHintWindow)
  private
  protected
  public
  end;

  { TGtk2WSScreen }

  TGtk2WSScreen = class(TGtkWSScreen)
  private
  protected
  public
  end;

  { TGtk2WSApplicationProperties }

  TGtk2WSApplicationProperties = class(TGtkWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TGtk2WSCustomForm }

class procedure TGtk2WSCustomForm.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  if (TControl(AWidgetInfo^.LCLObject).Parent = nil) then
    with TGTK2WidgetSet(Widgetset) do
    begin
      SetCallback(LM_CONFIGUREEVENT, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_CLOSEQUERY, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallBack(LM_Activate, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    end;

  g_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget), gtkevent_window_state_event,
    gtk_signal_func(@GTKWindowStateEventCB), AWidgetInfo^.LCLObject);
end;

class function TGtk2WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  WidgetInfo: PWidgetInfo;
  p: pointer;          // ptr to the newly created GtkWidget
  Box: Pointer;
  ABorderStyle: TFormBorderStyle;
  WindowType: TGtkWindowType;
  ACustomForm: TCustomForm;
  AResizable: gint;
begin
  // Start of old CreateForm method

  ACustomForm := TCustomForm(AWinControl);

  if ACustomForm.Parent = nil then
  begin
    if csDesigning in ACustomForm.ComponentState then
      ABorderStyle:=bsSizeable
    else
      ABorderStyle:=ACustomForm.BorderStyle;
  end
  else
    ABorderStyle:=bsNone;

  // Maps the border style
  WindowType := FormStyleMap[ABorderStyle];
  if (ABorderStyle=bsNone) and (ACustomForm.FormStyle in fsAllStayOnTop) then
    WindowType := GTK_WINDOW_POPUP;
  if (csDesigning in ACustomForm.ComponentState) then
    WindowType := GTK_WINDOW_TOPLEVEL;

  if ACustomForm.Parent = nil then
  begin
    // create a floating form
    P := gtk_window_new(WindowType);

    // Sets the window as resizable or not
    // Depends on the WM supporting this
    if (csDesigning in ACustomForm.ComponentState) then AResizable := 1
    else AResizable := FormResizableMap[ABorderStyle];

    // gtk_window_set_policy is deprecated in Gtk2
    gtk_window_set_resizable(GTK_WINDOW(P), gboolean(AResizable));

    // Sets the title
    gtk_window_set_title(PGtkWindow(P), AParams.Caption);

    // the clipboard needs a widget
    if (ClipboardWidget = nil) then
      Gtk2WidgetSet.SetClipboardWidget(P);
  end
  else
  begin
    // create a form as child control
    P := gtk_hbox_new(false, 0);
  end;

  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);

  Box := CreateFormContents(ACustomForm, P);
  gtk_container_add(PGtkContainer(P), Box);

  //so we can double buffer ourselves, eg, the Form Designer
  if csDesigning in AWinControl.ComponentState then
    gtk_widget_set_double_buffered(Box, False);

  gtk_widget_show(Box);

  // main menu
  if (ACustomForm.Menu <> nil) and (ACustomForm.Menu.HandleAllocated) then
  begin
    gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle), False, False,0);
  end;

  // End of the old CreateForm method

  {$IFNDEF NoStyle}
  if (ACustomForm.Parent = nil) then
    gtk_widget_set_app_paintable(P, True);
  {$ENDIF}

  if not (csDesigning in AWinControl.ComponentState) then
    WidgetInfo^.UserData := Pointer(1);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(P));
  Set_RC_Name(AWinControl, P);
  SetCallbacks(P, WidgetInfo);
end;

class procedure TGtk2WSCustomForm.SetIcon(const AForm: TCustomForm;
  const AIcon: HICON);
{$ifdef windows}
var
  Old8087CW: Word;

procedure SetCW;
begin
  Old8087CW := Get8087CW;
  Set8087CW($133F);
end;

procedure ResetCW;
begin
  Set8087CW(Old8087CW);
end;
{$endif}

begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon')
  then Exit;

  if AForm.Parent <> nil then Exit;

  {$ifdef windows}
  SetCW;
  {$endif}
  gtk_window_set_icon(PGtkWindow(AForm.Handle), PGdkPixbuf(AIcon));
  {$ifdef windows}
  ResetCW;
  {$endif}
end;

{class function TGtk2WSCustomForm.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleFormFrameBorders(TCustomForm(AWinControl).Menu<>nil);
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
end;

class procedure TGtk2WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  gtk_window_get_modal();
end;}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TGtk2WSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGtk2WSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGtk2WSCustomFrame);
//  RegisterWSComponent(TFrame, TGtk2WSFrame);
  RegisterWSComponent(TCustomForm, TGtk2WSCustomForm);
//  RegisterWSComponent(TForm, TGtk2WSForm);
//  RegisterWSComponent(THintWindow, TGtk2WSHintWindow);
//  RegisterWSComponent(TScreen, TGtk2WSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtk2WSApplicationProperties);
////////////////////////////////////////////////////
end.
