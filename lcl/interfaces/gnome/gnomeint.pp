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
  GraphMath, gtkInt, {$Ifndef NoGdkImlib}gdk_imlib,{$EndIf}
  libgnome, libart, libgnomeui;

type
  TGnomeObject = class(TGtkObject)
  private
    Function PromptUserWidget(const DialogCaption, DialogMessage : String;
      DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex : Longint) : Pointer;
  protected
    procedure InitStockItems; override;
    procedure FreeStockItems; override;

    procedure CreateComponent(Sender : TObject); override;
    procedure Init; override;
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

var
  LAZBTNALL,
  LAZBTNYESALL,
  LAZBTNNOALL,
  LAZBTNABORT,
  LAZBTNRETRY,
  LAZBTNIGNORE : PGnomeStockPixmapEntryData;

implementation

uses
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  KeyMap, Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, gtkproc, LCLStrConsts;

Procedure TGnomeObject.InitStockItems;
begin
  Inherited InitStockItems;
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

Procedure TGnomeObject.FreeStockItems;
begin
  Inherited FreeStockItems;
  Dispose(LAZBTNALL);
  Dispose(LAZBTNYESALL);
  Dispose(LAZBTNNOALL);
  Dispose(LAZBTNABORT);
  Dispose(LAZBTNRETRY);
  Dispose(LAZBTNIGNORE);
end;

procedure TGnomeObject.Init;
begin
  // initialize app level gtk engine
  gtk_set_locale ();

  // call init and pass cmd line args
  gnome_init('lazarus', '0.8.5a', argc, argv);

  If Assigned(Screen) then
    FillScreenFonts(Screen.Fonts);

  // read gtk rc file
  FRCFileParsed:=true;
  ParseRCFile;

  // Initialize Stringlist for holding styles
  Styles := TStringlist.Create;

  LoadGDKCursors;

  gtk_key_snooper_install(@GTKKeySnooper, @FKeyStateList);

  // Init tooltips
  FGTKToolTips := gtk_tooltips_new;
  gtk_object_ref(PGTKObject(FGTKToolTips));
  gtk_toolTips_Enable(FGTKToolTips);

  // Init stock objects;
  InitStockItems;

  // clipboard
  ClipboardTypeAtoms[ctPrimarySelection]:=GDK_SELECTION_PRIMARY;
  ClipboardTypeAtoms[ctSecondarySelection]:=GDK_SELECTION_SECONDARY;
  ClipboardTypeAtoms[ctClipboard]:=gdk_atom_intern('CLIPBOARD',0);
end;

procedure TGnomeObject.CreateComponent(Sender : TObject);
var
  Caption : AnsiString;
  StrTemp : PChar;               // same as "caption" but as PChar
  TempWidget,
  TempWidget2 : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  Box       : Pointer;           // currently only used for TBitBtn and TForm and TListView
  ParentForm: TCustomForm;
  CompStyle : Longint;
  DoFinishComp,
  SetupProps : Boolean;
begin
  P := nil;
  DoFinishComp := True;
  SetupProps := False;

  CompStyle := GetCompStyle(Sender);
  Caption   := GetCaption(Sender);
  strTemp := nil;

  Case CompStyle of
    csForm:
      begin
        Assert(Sender is TCustomForm);
        With TCustomForm(Sender) do begin
          If Caption > '' then begin
            strTemp := StrAlloc(length(Caption) + 1);
            StrPCopy(strTemp, Caption);
          end;
        
          P := GNOME_APP_NEW(Argv[0], strTemp);
          gnome_app_enable_layout_config(p, True);
          gtk_window_set_policy (GTK_WINDOW (p), FormResizableMap[BorderStyle],
            FormResizableMap[BorderStyle], 0);

          // the clipboard needs a widget
          if ClipboardWidget=nil then
            SetClipboardWidget(p);

          Box := CreateFormContents(P);
          gnome_app_set_contents(p, Box);
          gtk_widget_show(Box);

          //drag icons
          if Drag_Icon = nil then
            Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
               gtk_widget_get_colormap (p), @Drag_Mask,
               nil, @IMGDrag_Icon);
        end;
      end;
    csMainMenu:
      begin
        p := gtk_menu_bar_new();
        ParentForm:=TCustomForm(TMenu(Sender).Parent);
        if (ParentForm=nil) or (not (ParentForm is TCustomForm)) then
          RaiseException('MainMenu without form');
        if ParentForm.Menu<>TMenu(Sender) then
          RaiseException('form has already a MainMenu');
        gtk_widget_show(p);
        gnome_app_set_menus(Pointer(ParentForm.Handle), P);
      end;
    else
      begin
        inherited CreateComponent(Sender);
        DoFinishComp := False;
      end;
  end;

  If (Sender is TCustomEdit) then
    With (Sender as TCustomEdit) do
      If Popupmenu = nil then
        If Pointer(Handle) <> nil then
          gnome_widget_add_help(Pointer(Handle), nil);

  If DoFinishComp then
    FinishComponentCreate(Sender, P, SetupProps);
end;

{$I gnomewinapi.inc}

end.

{
  $Log$
  Revision 1.12  2002/10/26 15:15:50  lazarus
  MG: broke LCL<->interface circles

  Revision 1.11  2002/10/25 15:27:02  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.10  2002/10/24 22:10:39  lazarus
  AJ: More changes for better code reuse between gnome & gtk interfaces

  Revision 1.9  2002/10/23 20:47:27  lazarus
  AJ: Started Form Scrolling
      Started StaticText FocusControl
      Fixed Misc Dialog Problems
      Added TApplication.Title

  Revision 1.8  2002/10/21 13:15:24  lazarus
  AJ:Try and fall back on default style if nil(aka default theme)

  Revision 1.7  2002/10/21 03:23:34  lazarus
  AJ: rearranged GTK init stuff for proper GNOME init & less duplication between interfaces

  Revision 1.6  2002/10/15 22:28:04  lazarus
  AJ: added forcelinebreaks

  Revision 1.5  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.4  2002/10/12 16:36:40  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.3  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt


}
