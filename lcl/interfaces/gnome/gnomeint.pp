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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 }

unit GnomeInt;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{off $DEFINE NoGdkPixbufLib}
{off $DEFINE NoGdkImlib}

uses
  InterfaceBase, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} gtk, gdk,
  glib, SysUtils, LMessages, Classes, Controls, Forms, LCLIntf, LCLType, gtkDef,
  DynHashArray, LazLinkedList, GraphType, GraphMath, gtkInt,
  {$Ifndef NoGdkImlib}gdk_imlib,{$EndIf}
  libgnome, libart, libgnomeui;

type

  { TGnomeWidgetSet }

  TGnomeWidgetSet = class(TGtkWidgetSet)
  private
    procedure PassCmdLineOptions; override;
    Function PromptUserWidget(const DialogCaption, DialogMessage : String;
      DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex : Longint) : Pointer;
  protected
    procedure InitStockItems; override;
    procedure FreeStockItems; override;

    function CreateComponent(Sender: TObject): THandle; override;
  public
    function  WidgetSetName: string; override;
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
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// GnomeWSActnList,
// GnomeWSArrow,
// GnomeWSButtons,
// GnomeWSCalendar,
// GnomeWSCheckLst,
// GnomeWSCListBox,
// GnomeWSComCtrls,
// GnomeWSControls,
// GnomeWSDbCtrls,
// GnomeWSDBGrids,
// GnomeWSDialogs,
// GnomeWSDirSel,
// GnomeWSEditBtn,
// GnomeWSExtCtrls,
// GnomeWSExtDlgs,
// GnomeWSFileCtrl,
// GnomeWSForms,
// GnomeWSGrids,
// GnomeWSImgList,
// GnomeWSMaskEdit,
// GnomeWSMenus,
// GnomeWSPairSplitter,
// GnomeWSSpin,
// GnomeWSStdCtrls,
// GnomeWSToolwin,
////////////////////////////////////////////////////
  Graphics, Buttons, Menus, GTKWinApiWindow, StdCtrls, ComCtrls, CListBox,
  Calendar, Arrow, Spin, CommCtrl, ExtCtrls, Dialogs, FileCtrl,
  LResources, Math, gtkglobals, gtkproc, LCLStrConsts;

procedure TGnomeWidgetSet.PassCmdLineOptions;
begin
  // call init and pass cmd line args
  gnome_init(PChar(Application.Title), 'Lazarus', argc, argv);
end;

Procedure TGnomeWidgetSet.InitStockItems;
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
    theLabel := RemoveAmpersands(PChar(rsMbYesToAll), Length(rsMbYesToAll));
    xpm_data := PPgchar(@IMGALL_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_YESALL, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNYESALL));

  New(LAZBTNNOAll);
  With LAZBTNNOAll^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := RemoveAmpersands(PChar(rsMbNOToAll), Length(rsMbNOToAll));
    xpm_data := PPgchar(@IMGALL_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_NOALL, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNNOALL));

  New(LAZBTNABORT);
  With LAZBTNABORT^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := RemoveAmpersands(PChar(rsMbAbort), Length(rsMbAbort));
    xpm_data := PPgchar(@IMGCancel_X[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_ABORT, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNABORT));

  New(LAZBTNRETRY);
  With LAZBTNRETRY^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := RemoveAmpersands(PChar(rsMbRetry), Length(rsMbRetry));
    xpm_data := PPgchar(@IMGOK_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_RETRY, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNRETRY));

  New(LAZBTNIGNORE);
  With LAZBTNIGNORE^ do begin
    thetype := GNOME_STOCK_PIXMAP_TYPE_DATA;
    Width := 20;
    Height := 18;
    theLabel := RemoveAmpersands(PChar(rsMbIgnore), Length(rsMbIgnore));
    xpm_data := PPgchar(@IMGOK_Check[0]);
  end;
  gnome_stock_pixmap_register(LAZARUS_STOCK_BUTTON_IGNORE, GNOME_STOCK_PIXMAP_REGULAR,PGnomeStockPixmapEntry(LAZBTNIGNORE));
end;

Procedure TGnomeWidgetSet.FreeStockItems;
begin
  Inherited FreeStockItems;
  Dispose(LAZBTNALL);
  Dispose(LAZBTNYESALL);
  Dispose(LAZBTNNOALL);
  Dispose(LAZBTNABORT);
  Dispose(LAZBTNRETRY);
  Dispose(LAZBTNIGNORE);
end;

function TGnomeWidgetSet.CreateComponent(Sender : TObject): THandle;
var
  //Caption : AnsiString;
  StrTemp : PChar;            // same as "caption" but as PChar
  p       : pointer;          // ptr to the newly created GtkWidget
  Box     : Pointer;          // currently only used for TCustomBitBtn
                              // and TCustomForm and TCustomListView
  ParentForm: TCustomForm;
  CompStyle : Longint;
  DoFinishComp,
  SetupProps : Boolean;
  ACustomForm: TCustomForm;
begin
  P := nil;
  DoFinishComp := True;
  SetupProps := False;

  CompStyle := GetCompStyle(Sender);
  //Caption   := GetCaption(Sender);
  strTemp := nil;

  Case CompStyle of
    csForm:
      begin
        ACustomForm:=TCustomForm(Sender);
        if ACustomForm.Parent=nil then begin
          If ACustomForm.Caption <> '' then begin
            strTemp := StrAlloc(length(ACustomForm.Caption) + 1);
            StrPCopy(strTemp, ACustomForm.Caption);
          end;
        
          P := GNOME_APP_NEW(Argv[0], strTemp);
          gnome_app_enable_layout_config(p, True);
          gtk_window_set_policy (GTK_WINDOW (p),
            FormResizableMap[ACustomForm.BorderStyle],
            FormResizableMap[ACustomForm.BorderStyle], 0);

          // the clipboard needs a widget
          if ClipboardWidget=nil then
            SetClipboardWidget(p);
        end else begin
          P:=gtk_hbox_new(false,1);
        end;

        Box := CreateFormContents(ACustomForm,P);
        
        if ACustomForm.Parent=nil then begin
          gnome_app_set_contents(p, Box);
          //drag icons
          if Drag_Icon = nil then
            Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
               gtk_widget_get_colormap (p), Drag_Mask,
               nil, IMGDrag_Icon);
        end else begin
          gtk_container_add(PGtkContainer(P), Box);
        end;
        
        gtk_widget_show(Box);

        // main menu
        if (ACustomForm.Menu<>nil)
        and (ACustomForm.Menu.HandleAllocated) then begin
          gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle),False,False,0);
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
        P:=Pointer(inherited CreateComponent(Sender));
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
    
  Result:=THandle(P);
end;

function TGnomeWidgetSet.WidgetSetName: string;
begin
  Result:='gnome';
end;

{$I gnomewinapi.inc}

end.
