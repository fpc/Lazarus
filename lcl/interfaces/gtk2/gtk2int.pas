{
 /***************************************************************************
                       gtk2int.pas  -  GTK2 Interface Object
                       -------------------------------------


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

unit Gtk2Int;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils,
  {$IfNDef GTK2_2}
    {$IfNDef Win32}
     XLib, //X, XUtil,
    {$EndIf}
  {$EndIf}

  gdk2pixbuf, gtk2, gdk2, glib2, Pango,

  LMessages, Controls, Forms, LCLProc, LCLStrConsts, LCLIntf, LCLType,
  DynHashArray, GraphType, GraphMath, Graphics, Menus,
  GTKWinApiWindow, StdCtrls, ComCtrls,
  Dialogs, ExtDlgs, LResources, Math, GTKGlobals,
  {Buttons, CListBox, Calendar, Arrow, Spin, FileCtrl, CommCtrl, ExtCtrls, }
  gtkDef, gtkInt;

type

  { TGtk2WidgetSet }

  TGtk2WidgetSet = class(TGtkWidgetSet)
  protected
    procedure AppendText(Sender: TObject; Str: PChar);
    function CreateComponent(Sender : TObject): THandle; override;
    function CreateListView(ListViewObject: TObject): PGtkWidget; override;
    function GetText(Sender: TComponent; var Text: String): Boolean;
    procedure HookSignals(const AGTKObject: PGTKObject; const ALCLObject: TObject); override;
    //function IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; override;
    function LoadStockPixmap(StockID: longint) : HBitmap; override;
    procedure SetCallback(const AMsg: LongInt; const AGTKObject: PGTKObject; const ALCLObject: TObject);override;
    //procedure SetLabel(Sender : TObject; Data : Pointer);
    //function SetProperties(Sender : TObject) : integer; override;
    procedure SetSelectionMode(Sender: TObject; Widget: PGtkWidget;
      MultiSelect, ExtendedSelect: boolean); override;
    //function SetTopIndex(Sender: TObject; NewTopIndex: integer): integer; override;
    procedure UpdateDCTextMetric(DC: TDeviceContext); override;
  public
    function BeginPaint(Handle: hWnd; Var PS : TPaintStruct) : hdc; override;
    function CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT; override;
    function PangoDrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer; //override;
    Function EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer; override;
    function ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;
    function GetCursorPos(var lpPoint: TPoint ): Boolean; override;
    function GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean; override;
    function TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean; override;
  end;

  TGtkListStoreStringList = class(TStrings)
  private
    FColumnIndex : Integer;
    FGtkListStore : PGtkListStore;
    FOwner: TWinControl;
    FSorted : boolean;
    FStates: TGtkListStringsStates;
    FCachedCount: integer;
    FCachedItems: PGtkTreeIter;
    FUpdateCount: integer;
  protected
    function GetCount : integer; override;
    function Get(Index : Integer) : string; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AnObject: TObject); override;
    procedure SetSorted(Val : boolean); virtual;
    procedure ConnectItemCallbacks(Index: integer);
    procedure ConnectItemCallbacks(Li: TGtkTreeIter); virtual;
    procedure ConnectAllCallbacks; virtual;
    procedure RemoveItemCallbacks(Index: integer); virtual;
    procedure RemoveAllCallbacks; virtual;
    procedure UpdateItemCache;
  public
    constructor Create(ListStore : PGtkListStore; ColumnIndex : Integer; TheOwner: TWinControl);
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index : integer; const S: string); override;
    procedure Sort; virtual;
    function IsEqual(List: TStrings): boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    property Sorted : boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;

{$IfDef GTK2_2}//we need a GTK2_2 FLAG somehow
Procedure  gdk_display_get_pointer(display : PGdkDisplay; screen :PGdkScreen; x :Pgint; y : Pgint; mask : PGdkModifierType); cdecl; external gdklib;
function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;

procedure gdk_draw_pixbuf(drawable : PGdkDrawable; gc : PGdkGC; pixbuf : PGdkPixbuf; src_x, src_y, dest_x, dest_y, width, height : gint;
                                             dither : TGdkRgbDither; x_dither, y_dither : gint); cdecl; external gdklib;
{$Else}
  {$IfNDef Win32}
  Function gdk_x11_drawable_get_xdisplay(drawable : PGdkDrawable) :   PDisplay; cdecl; external gdklib;
  Function gdk_x11_drawable_get_xid(drawable : PGdkDrawable) :  Integer; cdecl; external gdklib;
  {$EndIf}
{$EndIf}

implementation
  
uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as litle as posible circles,
// uncomment only those units with implementation
////////////////////////////////////////////////////
// Gtk2WSActnList,
// Gtk2WSArrow,
// Gtk2WSButtons,
// Gtk2WSCalendar,
// Gtk2WSCheckLst,
// Gtk2WSCListBox,
 Gtk2WSComCtrls,
// Gtk2WSControls,
// Gtk2WSDbCtrls,
// Gtk2WSDBGrids,
// Gtk2WSDialogs,
// Gtk2WSDirSel,
// Gtk2WSEditBtn,
// Gtk2WSExtCtrls,
// Gtk2WSExtDlgs,
// Gtk2WSFileCtrl,
// Gtk2WSForms,
// Gtk2WSGrids,
// Gtk2WSImgList,
// Gtk2WSMaskEdit,
// Gtk2WSMenus,
// Gtk2WSPairSplitter,
// Gtk2WSSpin,
 Gtk2WSStdCtrls,
// Gtk2WSToolwin,
////////////////////////////////////////////////////
  gtkProc;

{$include gtk2object.inc}
{$include gtk2winapi.inc}

const
  GtkListStoreItemGtkListTag = 'GtkList';
  GtkListStoreItemLCLListTag = 'LCLList';

{*************************************************************}
{                      TGtkListStoreStringList methods             }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Create
  Params:
  Returns:

 ------------------------------------------------------------------------------}
constructor TGtkListStoreStringList.Create(ListStore : PGtkListStore; ColumnIndex : Integer; TheOwner: TWinControl);
begin
  inherited Create;
  if ListStore = nil then RaiseException(
    'TGtkListStoreStringList.Create Unspecified list store');
  FGtkListStore:= ListStore;

  if (ColumnIndex < 0) or
    (ColumnIndex >= gtk_tree_model_get_n_columns(GTK_TREE_MODEL(ListStore)))
  then
    RaiseException('TGtkListStoreStringList.Create Invalid Column Index');
  FColumnIndex:=ColumnIndex;

  if TheOwner = nil then RaiseException(
    'TGtkListStoreStringList.Create Unspecified owner');
  FOwner:=TheOwner;
  Include(FStates,glsItemCacheNeedsUpdate);
  ConnectAllCallbacks;
end;

destructor TGtkListStoreStringList.Destroy;
begin
  // don't destroy the widgets
  RemoveAllCallbacks;
  ReAllocMem(FCachedItems,0);
  inherited Destroy;
end;

function TGtkListStoreStringList.Add(const S: string): Integer;
begin
  Result:=Count;
  Insert(Count,S);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.SetSorted
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    if FSorted then Sort;
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStoreStringList.ConnectItemCallbacks(Index: integer);

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.ConnectItemCallbacks(Index: integer);
var
  ListItem: TGtkTreeIter;
begin
  UpdateItemCache;
  ListItem:=FCachedItems[Index];
  ConnectItemCallbacks(ListItem);
end;

{------------------------------------------------------------------------------
  procedure TGtkListStoreStringList.ConnectItemCallbacks(Li: PGtkListItem);

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.ConnectItemCallbacks(Li: TGtkTreeIter);
begin
  {gtk_object_set_data(PGtkObject(li.user_data),GtkListItemLCLListTag,Self);
  gtk_object_set_data(PGtkObject(li.user_data),GtkListItemGtkListTag,FGtkList);}
end;

{------------------------------------------------------------------------------
  procedure TGtkListStoreStringList.ConnectAllCallbacks;
 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.ConnectAllCallbacks;
var
  i, Cnt: integer;
begin
  BeginUpdate;
  Cnt:=Count-1;
  for i:=0 to Cnt-1 do
    ConnectItemCallbacks(i);
  EndUpdate;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStoreStringList.RemoveItemCallbacks(Index: integer);

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.RemoveItemCallbacks(Index: integer);
var
  ListItem: TGtkTreeIter;
begin
  UpdateItemCache;
  ListItem:=FCachedItems[Index];
  {gtk_object_set_data(PGtkObject(ListItem),GtkListItemLCLListTag,nil);
  gtk_object_set_data(PGtkObject(ListItem),GtkListItemGtkListTag,nil);}
end;

{------------------------------------------------------------------------------
  procedure TGtkListStoreStringList.RemoveAllCallbacks;

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.RemoveAllCallbacks;
var
  i: integer;
begin
  BeginUpdate;
  for i:=0 to Count-1 do
    RemoveItemCallbacks(i);
  EndUpdate;
end;

procedure TGtkListStoreStringList.UpdateItemCache;
var
  i: integer;
begin
  if not (glsItemCacheNeedsUpdate in FStates) then exit;
  if (FGtkListStore<>nil) then
    FCachedCount:= gtk_tree_model_iter_n_children(GTK_TREE_MODEL(FGtkListStore),nil)
  else
    FCachedCount:=0;
  ReAllocMem(FCachedItems,SizeOf(TGtkTreeIter)*FCachedCount);
  if FGtkListStore<>nil then
    For I := 0 to FCachedCount - 1 do
      gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(FGtkListStore), @FCachedItems[i], nil, I);
  Exclude(FStates,glsItemCacheNeedsUpdate);
end;

procedure TGtkListStoreStringList.PutObject(Index: Integer; AnObject: TObject);
var
  ListItem : TGtkTreeIter;
begin
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStoreStringList.PutObject Out of bounds.')
  else if FGtkListStore<>nil then begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];
    {if ListItem <> nil then begin
      gtk_object_set_data(PGtkObject(ListItem),'LCLStringsObject',AnObject);
    end;}
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Sort
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.Sort;
var
  sl: TStringList;
begin
  BeginUpdate;
  // sort internally (sorting in the widget would be slow and unpretty ;)
  sl:=TStringList.Create;
  sl.Assign(Self);
  sl.Sort; // currently this is quicksort ->
             // Disadvantages: - worst case on sorted list
             //                - not keeping order
             // ToDo: replace by mergesort and add customsort
  Assign(sl);
  sl.Free;
  EndUpdate;
end;

function TGtkListStoreStringList.IsEqual(List: TStrings): boolean;
var
  i, Cnt: integer;
  CmpList: TStringList;
begin
  if List=Self then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if List=nil then exit;
  BeginUpdate;
  Cnt:=Count;
  if (Cnt<>List.Count) then exit;
  CmpList:=TStringList.Create;
  try
    CmpList.Assign(List);
    CmpList.Sorted:=FSorted;
    for i:=0 to Cnt-1 do begin
      if (Strings[i]<>CmpList[i]) or (Objects[i]<>CmpList.Objects[i]) then exit;
    end;
  finally
    CmpList.Free;
  end;
  Result:=true;
  EndUpdate;
end;

procedure TGtkListStoreStringList.BeginUpdate;
begin
  if FUpdateCount=0 then Include(FStates,glsItemCacheNeedsUpdate);
  inc(FUpdateCount);
end;

procedure TGtkListStoreStringList.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount=0 then Include(FStates,glsItemCacheNeedsUpdate);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Assign
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.Assign(Source : TPersistent);
var
  i, Cnt: integer;
begin
  if (Source=Self) or (Source=nil) then exit;
  if ((Source is TGtkListStoreStringList)
    and (TGtkListStoreStringList(Source).FGtkListStore=FGtkListStore))
  then
    RaiseException('TGtkListStoreStringList.Assign: There 2 lists with the same FGtkListStore');
  BeginUpdate;
  try
    if Source is TStrings then begin
      // clearing and resetting can change other properties of the widget,
      // => don't change if the content is already the same
      if IsEqual(TStrings(Source)) then exit;
      Clear;
      Cnt:=TStrings(Source).Count;
      for i:=0 to Cnt - 1 do begin
        AddObject(TStrings(Source)[i],TStrings(Source).Objects[i]);
      end;
      // ToDo: restore other settings

      // Do not call inherited Assign as it does things we do not want to happen
    end else
      inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Get
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkListStoreStringList.Get(Index : integer) : string;
var
  Item : PChar;
  ListItem : TGtkTreeIter;
begin
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStoreStringList.Get Out of bounds.')
  else begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];

    Item := nil;
    gtk_tree_model_get(GTK_TREE_MODEL(FGtkListStore), @ListItem, [FColumnIndex, @Item, -1]);
    if (Item <> nil) then begin
      Result:= StrPas(Item);
      g_free(Item);
    end
    else
      result := '';
  end;
end;

function TGtkListStoreStringList.GetObject(Index: Integer): TObject;
var
  ListItem : TGtkTreeIter;
begin
  Result:=nil;
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStoreStringList.GetObject Out of bounds.')
  else if FGtkListStore<>nil then begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];
    {if ListItem<>nil then begin
      Result:=TObject(gtk_object_get_data(PGtkObject(ListItem),'LCLStringsObject'));
    end;}
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.GetCount
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TGtkListStoreStringList.GetCount: integer;
begin
  if (FGtkListStore<>nil) then begin
    UpdateItemCache;
    Result:=FCachedCount;
  end else begin
    Result:= 0
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Clear
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.Clear;
begin
  RemoveAllCallbacks;
  Include(FStates,glsItemCacheNeedsUpdate);
  gtk_list_store_clear(FGtkListStore)
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Delete
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.Delete(Index : integer);
var
  ListItem : TGtkTreeIter;
begin
  RemoveItemCallbacks(Index);
  Include(FStates,glsItemCacheNeedsUpdate);
  gtk_tree_model_iter_nth_child (FGtkListStore, @ListItem, nil, Index);
  gtk_list_store_remove(FGtkListStore, @ListItem);
end;

function TGtkListStoreStringList.IndexOf(const S: string): Integer;
var
  l, m, r, cmp: integer;
begin
  BeginUpdate;
  if FSorted then begin
    l:=0;
    r:=Count-1;
    m:=l;
    while (l<=r) do begin
      m:=(l+r) shr 1;
      cmp:=AnsiCompareText(S,Strings[m]);

      if cmp<0 then
        r:=m-1
      else if cmp>0 then
        l:=m+1
      else begin
        Result:=m;
        exit;
      end;
    end;
    Result:=-1;
  end else begin
    Result:=inherited IndexOf(S);
  end;
  EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStoreStringList.Insert
  Params:
  Returns:

 ------------------------------------------------------------------------------}
procedure TGtkListStoreStringList.Insert(Index : integer; const S : string);
var
  li : TGtkTreeIter;
  l, m, r, cmp: integer;
begin
  BeginUpdate;
  try
    if FSorted then begin
      l:=0;
      r:=Count-1;
      m:=l;
      while (l<=r) do begin
        m:=(l+r) shr 1;
        cmp:=AnsiCompareText(S,Strings[m]);
        if cmp<0 then
          r:=m-1
        else if cmp>0 then
          l:=m+1
        else
          break;
      end;
      if (m<Count) and (AnsiCompareText(S,Strings[m])>0) then
        inc(m);
      Index:=m;
    end;
    if (Index < 0) or (Index > Count) then
      RaiseException('TGtkListStoreStringList.Insert: Index '+IntToStr(Index)
        +' out of bounds. Count='+IntToStr(Count));
    if Owner = nil then RaiseException(
      'TGtkListStoreStringList.Insert Unspecified owner');

    gtk_list_store_insert(FGtkListStore, @li, Index);
    gtk_list_store_set(FGtkListStore, @li, [FColumnIndex, PChar(S), -1]);

    ConnectItemCallbacks(li);

    Include(FStates,glsItemCacheNeedsUpdate);

  finally
    EndUpdate;
  end;
end;

end.

{
  $Log$
  Revision 1.39  2005/02/05 09:09:52  mattias
  implemented TListView for gtk2   From Andrew Haines

  Revision 1.38  2005/02/03 16:51:29  mattias
  improved gtk2 listbox  from Andrew Haines

  Revision 1.37  2005/01/23 11:43:38  mattias
  fixed expandingtabs in synedit painting

  Revision 1.36  2005/01/22 23:53:43  mattias
  fixed gtk2 intf  from Peter Vreman

  Revision 1.35  2004/09/30 10:35:50  mazen
  * Fix compile probelem under GTK2 related to
    THandle <--> Pointer conversion

  Revision 1.34  2004/09/12 19:08:35  mazen
  * Fix compilation problem with GTK2

  Revision 1.33  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.32  2004/03/17 19:59:56  marc
  * Fixes some typos and changes uses clause cases

  Revision 1.31  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.30  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.29  2004/03/05 00:41:15  marc
  * Renamed TGtk2Object to TGtk2WidgetSet

  Revision 1.28  2004/03/05 00:31:53  marc
  * Renamed TGtkObject to TGtkWidgetSet

  Revision 1.27  2004/01/04 16:44:33  mattias
  updated gtk2 package

  Revision 1.26  2003/10/17 03:21:21  ajgenius
  fix GTK2 compiling for new Keyboard changes

  Revision 1.25  2003/10/15 20:33:37  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.24  2003/10/03 01:25:01  ajgenius
  add more gtk1i<->gtk2 key & event wrappers,
  move more GTK2 workarounds from gtk to gtk2 interface,
  start GTK2 interface SetCallback

  Revision 1.23  2003/10/02 18:15:44  ajgenius
  more gtk2 (check)ListBox implementation

  Revision 1.22  2003/09/24 17:23:54  ajgenius
  more work toward GTK2 - partly fix CheckListBox, & MenuItems

  Revision 1.21  2003/09/22 20:08:56  ajgenius
  break GTK2 object and winapi into includes like the GTK interface

  Revision 1.20  2003/09/22 19:17:26  ajgenius
  begin implementing GtkTreeView for ListBox/CListBox

  Revision 1.19  2003/09/19 00:41:52  ajgenius
  remove USE_PANGO define since pango now apears to work properly.

  Revision 1.18  2003/09/18 21:36:00  ajgenius
  add csEdit to GTK2 interface to start removing use of GtkOldEditable

  Revision 1.17  2003/09/18 17:23:05  ajgenius
  start using GtkTextView for Gtk2 Memo

  Revision 1.16  2003/09/18 14:06:30  ajgenius
  fixed Tgtkobject.drawtext for Pango till the native pango one works better

  Revision 1.15  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.14  2003/09/17 20:30:57  ajgenius
  fix(?) DCOffset for DrawText

  Revision 1.13  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.12  2003/09/15 16:42:02  ajgenius
  mostly fixed ExtTextOut

  Revision 1.11  2003/09/15 03:10:46  ajgenius
  PANGO support for GTK2 now works.. sorta. TextOut/ExtTextOut broken?

  Revision 1.10  2003/09/12 17:40:46  ajgenius
  fixes for GTK2(accel groups, menu accel, 'draw'),
  more work toward Pango(DrawText now works, UpdateDCTextMetric mostly works)

  Revision 1.9  2003/09/10 18:03:47  ajgenius
  more changes for pango -
  partly fixed ref counting,
  added Pango versions of TextOut, CreateFontIndirectEx, and GetTextExtentPoint to the GTK2 interface

  Revision 1.8  2003/09/09 20:46:38  ajgenius
  more implementation toward pango for gtk2

  Revision 1.7  2003/09/09 17:16:24  ajgenius
  start implementing pango routines for GTK2

  Revision 1.6  2003/09/09 04:15:08  ajgenius
  more updates for GTK2, more GTK1 wrappers, removal of more ifdef's, partly fixed signals

  Revision 1.5  2003/09/06 22:56:03  ajgenius
  started gtk2 stock icon overrides
  partial/temp(?) workaround for dc paint offsets

  Revision 1.4  2003/09/06 20:23:53  ajgenius
  fixes for gtk2
  added more wrappers for gtk1/gtk2 converstion and sanity
  removed pointless version $Ifdef GTK2 etc
  IDE now "runs" Tcontrol drawing/using problems
  renders it unuseable however

  Revision 1.3  2003/09/06 17:24:52  ajgenius
  gtk2 changes for pixmap, getcursorpos, mouse events workaround

  Revision 1.2  2003/08/27 20:55:51  mattias
  fixed updating codetools on changing pkg output dir

  Revision 1.1  2002/12/15 11:52:28  mattias
  started gtk2 interface

  Revision 1.15  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.14  2002/11/03 22:40:00  lazarus
  MG: fixed ControlAtPos

  Revision 1.13  2002/10/30 18:45:52  lazarus
  AJ: fixed compiling & removed '_' from custom stock items

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
