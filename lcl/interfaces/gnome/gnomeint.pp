{
 /***************************************************************************
                         GNOMEINT.pp  -  GNOMEInterface Object
                             -------------------

                   Initial Revision  : Thu Oct 3rd EST 2002


 ***************************************************************************/

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

unit GNOMEInt;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{$DEFINE Gnome1}
{off $DEFINE NoGdkPixbufLib}
{off $DEFINE NoGdkImlib}

uses
  InterfaceBase, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} gtk, gdk,
  glib, SysUtils, LMessages, Classes, Controls, Forms, VclGlobals,
  LCLLinux, LCLType, gtkDef, DynHashArray, LazQueue, GraphType,
  GraphicsMath, gtkInt, {$Ifndef NoGdkImlib}gdk_imlib,{$EndIf}
  libgnome, libart, libgnomeui;

type
  TGnomeObject = class(TGtkObject)
  protected
    procedure CreateComponent(Sender : TObject); override;
  public
    {$I gnomewinapih.inc}
  end;


implementation

uses
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, GTKPRoc;

procedure TGnomeObject.CreateComponent(Sender : TObject);
var
  Caption : ansistring;          // the caption of "Sender"
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  Box       : Pointer;           // currently only used for TBitBtn and TForm and TListView
  ParentForm: TCustomForm;
begin
  Caption   := Sender.ClassName;
  if (Sender is TCustomForm) and (TControl(Sender).FCompStyle = csForm) then begin
    With (Sender as TCustomForm) do begin
      Caption := TControl(Sender).Caption;
      If Caption > '' then begin
        strTemp := StrAlloc(length(Caption) + 1);
        StrPCopy(strTemp, Caption);
      end
      else
        strTemp := nil;

      P := GNOME_APP_NEW(Argv[0], strTemp);

      gtk_window_set_policy (GTK_WINDOW (p), FormResizableMap[BorderStyle],
        FormResizableMap[BorderStyle], 0);

      // the clipboard needs a widget
      if ClipboardWidget=nil then
        SetClipboardWidget(p);

      Box := gtk_vbox_new(False, 0);
      gnome_app_set_contents(p, Box);
      gtk_widget_show(Box);

      // Create the form client area
      TempWidget := gtk_fixed_new();
      gtk_box_pack_end(Box, TempWidget, True, True, 0);
      gtk_widget_show(TempWidget);
      SetFixedWidget(p, TempWidget);
      SetMainWidget(p, TempWidget);

      //drag icons
      if Drag_Icon = nil then
        Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
           gtk_widget_get_colormap (p), @Drag_Mask,
           nil, @IMGDrag_Icon);

      SetLCLObject(p, Sender);
      gtk_object_set_data(pgtkObject(p),'Style',0);
      gtk_object_set_data(pgtkObject(p),'ExStyle',0);

      Handle := THandle(p);
      gtk_object_set_data(pgtkobject(p),'Sender',Sender);
      SetResizeRequest(p);

      Set_RC_Name(sender, p);

      StrDispose(StrTemp);
      gtk_widget_set_app_paintable(p,true);
      HookSignals(Sender);
    end;
  end
  else
    If (Sender is TMenu) and (TMenu(Sender).FCompStyle = csMainMenu) then
    begin
      p := gtk_menu_bar_new();
      // get the VBox, the form has one child, a VBox
      ParentForm:=TCustomForm(TMenu(Sender).Parent);
      if (ParentForm=nil) or (not (ParentForm is TCustomForm)) then
        RaiseException('MainMenu without form');
      if ParentForm.Menu<>TMenu(Sender) then
        RaiseException('form has already a MainMenu');
      SetAccelGroup(p, gtk_accel_group_get_default);
      gtk_widget_show(p);
      gnome_app_set_menus(Pointer(ParentForm.Handle), p);
      gnome_app_enable_layout_config(P, True);
      TMenu(Sender).Items.Handle := HMenu(p);
    end
  else
    inherited CreateComponent(Sender);
end;

{$I gnomewinapi.inc}

initialization

finalization

end.
