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

const
  LAZARUS_STOCK_BUTTON_ALL = 'lazarus_button_all';
  LAZARUS_STOCK_BUTTON_YESALL = 'lazarus_button_yesall';
  LAZARUS_STOCK_BUTTON_NOALL = 'lazarus_button_noall';
  LAZARUS_STOCK_BUTTON_ABORT = 'lazarus_button_abort';
  LAZARUS_STOCK_BUTTON_RETRY = 'lazarus_button_retry';
  LAZARUS_STOCK_BUTTON_IGNORE = 'lazarus_button_ignore';

implementation

uses
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, GTKPRoc, LCLStrConsts;

Function InsertLineBreaks(Font : hFont; Str : PChar; MaxWidth : Longint) : String;
var
  Layout : PGnomeIconTextInfo;
  Line : PGList;
begin
  Layout := gnome_icon_layout_text(PGDIObject(Font)^.GDIFontObject,
    PgChar(Str), ' ', MaxWidth, False);

  Line := Layout^.Rows;
  While Line <> nil do begin
    If Line^.Data <> nil then
      Result := Result + AnsiString(PGnomeIconTextInfoRow(Line^.Data)^.thetext);
    Line := Line^.Next;
    If Line <> nil then
      If Result[Length(Result)] <> #10 then
        Result := Result + #10;
  end;

  Result := Copy(Result, 1, Length(Result));

  gnome_icon_text_info_free(Layout);
end;

procedure TGnomeObject.CreateComponent(Sender : TObject);
var
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  Box       : Pointer;           // currently only used for TBitBtn and TForm and TListView
  ParentForm: TCustomForm;
begin
  if (Sender is TCustomForm) and (TControl(Sender).FCompStyle = csForm) then begin
    With (Sender as TCustomForm) do begin
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
  else begin
    inherited CreateComponent(Sender);
    If (Sender is TCustomEdit) then
      With (Sender as TCustomEdit) do
        If Popupmenu = nil then
          If Pointer(Handle) <> nil then
            gnome_widget_add_help(Pointer(Handle), nil)
  end;
end;

{$I gnomewinapi.inc}

var
  LAZBTNALL,
  LAZBTNYESALL,
  LAZBTNNOALL,
  LAZBTNABORT,
  LAZBTNRETRY,
  LAZBTNIGNORE : PGnomeStockPixmapEntryData;

Procedure InitGnome;
begin
  gnome_init('lazarus', '0.8.5a', argc, argv);

  New(LAZBTNAll);
  With LAZBTNAll^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbALL));
    xpm_data := PPgchar(@IMGALL_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_ALL, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNALL));

  New(LAZBTNYESAll);
  With LAZBTNYESAll^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbYesToAll));
    xpm_data := PPgchar(@IMGALL_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_YESALL, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNYESALL));

  New(LAZBTNNOAll);
  With LAZBTNNOAll^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbNOToAll));
    xpm_data := PPgchar(@IMGALL_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_NOALL, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNNOALL));

  New(LAZBTNABORT);
  With LAZBTNABORT^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbAbort));
    xpm_data := PPgchar(@IMGCancel_X[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_ABORT, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNABORT));

  New(LAZBTNRETRY);
  With LAZBTNRETRY^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbRetry));
    xpm_data := PPgchar(@IMGOK_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_RETRY, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNRETRY));

  New(LAZBTNIGNORE);
  With LAZBTNIGNORE^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := Ampersands2Underscore(PChar(rsMbIgnore));
    xpm_data := PPgchar(@IMGOK_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_IGNORE, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNIGNORE));
end;

Procedure CleanupGnome;
begin
  Dispose(LAZBTNALL);
  Dispose(LAZBTNYESALL);
  Dispose(LAZBTNNOALL);
  Dispose(LAZBTNABORT);
  Dispose(LAZBTNRETRY);
  Dispose(LAZBTNIGNORE);
end;

initialization
  InitGnome;
finalization
  CleanupGnome;
end.

{
  $Log$
  Revision 1.5  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt


}
