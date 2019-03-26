


{ $Id$}
{
 *****************************************************************************
 *                             GtkWSStdCtrls.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit GtkWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  LCLType, LMessages, LCLProc, Controls, Graphics, StdCtrls, Forms,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango, Gtk2WSPrivate,
  {$ELSE}
  glib, gdk, gtk, gdkpixbuf,
  GtkFontCache, Gtk1WSPrivate,
  {$ENDIF}
  InterfaceBase, WSStdCtrls, WSLCLClasses, WSProc, WSControls,
  GtkInt, GtkDef, GTKWinApiWindow, GtkGlobals, GtkProc, GtkExtra, GtkWSPrivate;


type

  { TGtkWSScrollBar }

  TGtkWSScrollBar = class(TWSScrollBar)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetParams(const AScrollBar: TCustomScrollBar); override;
  end;

  { TGtkWSCustomGroupBox }

  TGtkWSCustomGroupBox = class(TWSCustomGroupBox)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
  {$ifdef gtk1}
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  {$endif}
  end;

  { TGtkWSGroupBox }

  TGtkWSGroupBox = class(TWSGroupBox)
  published
  end;

  { TGtkWSCustomComboBox }

  TGtkWSCustomComboBox = class(TWSCustomComboBox)
  published
  {$IFDEF GTK1}
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; override;
    class function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); override;
    class procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  {$ENDIF}
  end;

  { TGtkWSComboBox }

  TGtkWSComboBox = class(TWSComboBox)
  published
  end;

  { TGtkWSCustomListBox }

  TGtkWSCustomListBox = class(TWSCustomListBox)
  published
  {$IFDEF GTK1}
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  {$ENDIF}
  end;

  { TGtkWSListBox }

  TGtkWSListBox = class(TWSListBox)
  published
  end;

  { TGtkWSCustomEdit }

  TGtkWSCustomEdit = class(TWSCustomEdit)
  published
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSCustomMemo }

  TGtkWSCustomMemo = class(TWSCustomMemo)
  published
    {$ifdef GTK1}
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function CreateHandle(const AWinControl: TWinControl;
                        const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure AppendText(const ACustomMemo: TCustomMemo;
                               const AText: string); override;
    class function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit;
                                NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit;
                                 NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit;
                                    NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit;
                                NewReadOnly: boolean); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo;
                                  const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo;
                                const NewWordWrap: boolean); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    {$endif}
  end;

  { TGtkWSEdit }

  TGtkWSEdit = class(TWSEdit)
  published
  end;

  { TGtkWSMemo }

  TGtkWSMemo = class(TWSMemo)
  published
  end;

  { TGtkWSCustomStaticText }

  TGtkWSCustomStaticText = class(TWSCustomStaticText)
  protected
    class function GetLabelWidget(AFrame: PGtkFrame): PGtkLabel;
    class function GetBoxWidget(AFrame: PGtkFrame): PGtkEventBox;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetAlignment(const ACustomStaticText: TCustomStaticText;
                                 const NewAlignment: TAlignment); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TGtkWSStaticText }

  TGtkWSStaticText = class(TWSStaticText)
  published
  end;

  { TGtkWSButtonControl }

  TGtkWSButtonControl = class(TWSButtonControl)
  published
  end;

  { TGtkWSButton }

  TGtkWSButton = class(TWSButton)
  public
    {SetCallbacks is made public so that it can be called from
     TGtkWSBitBtn.CreateHandle.
     TODO: move it to TGtkPrivateButton}
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    {$ifdef Gtk1}
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); override;
    class procedure SetShortcut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortcut); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    {$endif Gtk1}
  end;

  { TGtkWSCustomCheckBox }

  TGtkWSCustomCheckBox = class(TWSCustomCheckBox)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
  {$IFDEF GTK1}
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetState(const ACB: TCustomCheckBox; const ANewState: TCheckBoxState); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  {$ENDIF}
  end;

  { TGtkWSCheckBox }

  TGtkWSCheckBox = class(TWSCheckBox)
  published
  end;

  { TGtkWSToggleBox }

  TGtkWSToggleBox = class(TWSToggleBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtkWSRadioButton }

  TGtkWSRadioButton = class(TWSRadioButton)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

function  WidgetGetSelStart(const Widget: PGtkWidget): integer;
procedure WidgetSetSelLength(const Widget: PGtkWidget; NewLength: integer);

{$ifdef gtk1}
{$I gtk1memostringsh.inc}
{$endif}

implementation

uses
  GtkWSControls;
  
const
  StaticBorderShadowMap: array[TStaticBorderStyle] of TGtkShadowType =
  (
    GTK_SHADOW_NONE,
    GTK_SHADOW_ETCHED_IN,
    GTK_SHADOW_IN
  );

{$ifdef gtk1}
{$I gtk1memostrings.inc}
{$endif}

{ helper routines }



function WidgetGetSelStart(const Widget: PGtkWidget): integer;
begin
  if Widget <> nil then 
  begin
    if PGtkOldEditable(Widget)^.selection_start_pos
       < PGtkOldEditable(Widget)^.selection_end_pos
    then
      Result:= PGtkOldEditable(Widget)^.selection_start_pos
    else
      Result:= PGtkOldEditable(Widget)^.current_pos;// selection_end_pos
  end else 
    Result:= 0;
end;

procedure WidgetSetSelLength(const Widget: PGtkWidget; NewLength: integer);
begin
  if Widget<>nil then 
  begin
    gtk_editable_select_region(PGtkOldEditable(Widget),
      gtk_editable_get_position(PGtkOldEditable(Widget)),
      gtk_editable_get_position(PGtkOldEditable(Widget)) + NewLength);
  end;
end;

{ TGtkWSScrollBar }

class procedure TGtkWSScrollBar.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  if TScrollBar(AWidgetInfo^.LCLObject).Kind = sbHorizontal then
    TGtkWidgetset(Widgetset).SetCallback(LM_HSCROLL, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject)
  else
    TGtkWidgetset(Widgetset).SetCallback(LM_VSCROLL, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
end;

class function TGtkWSScrollBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  with TScrollBar(AWinControl) do
  begin
    Adjustment := PgtkAdjustment(
                   gtk_adjustment_new(1, Min, Max, SmallChange, LargeChange,
                     Pagesize));
                     
    if (Kind = sbHorizontal) then
      Widget := gtk_hscrollbar_new(Adjustment)
    else
      Widget := gtk_vscrollbar_new(Adjustment);
  end;
  gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, Widget);

  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class procedure TGtkWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
var
  Adjustment: PGtkAdjustment;
begin
  with AScrollBar do
  begin
    //set properties for the range
    Adjustment := gtk_range_get_adjustment (GTK_RANGE(Pointer(Handle)));
    Adjustment^.lower := Min;
    Adjustment^.Upper := Max;
    Adjustment^.Value := Position;
    Adjustment^.step_increment := SmallChange;
    Adjustment^.page_increment := LargeChange;
  end;
end;

{ TGtkWSCustomListBox }

{$IFDEF GTK1}

class function TGtkWSCustomListBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  ScrolledWnd: PGtkScrolledWindow absolute Widget;
  ListBox: TCustomListBox absolute AWinControl;
  List: Pointer;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(ScrolledWnd^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(ScrolledWnd, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show(Widget);

  List := gtk_list_new;
  gtk_scrolled_window_add_with_viewport(ScrolledWnd, List);
  gtk_container_set_focus_vadjustment(List,
               gtk_scrolled_window_get_vadjustment(ScrolledWnd));
  gtk_container_set_focus_hadjustment(PGtkContainer(List),
               gtk_scrolled_window_get_hadjustment(ScrolledWnd));
  gtk_widget_show(List);

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  SetMainWidget(Widget, List);
  WidgetInfo^.CoreWidget := List;

  TGtkWidgetSet(WidgetSet).SetSelectionMode(AWinControl, Widget,
    ListBox.MultiSelect, ListBox.ExtendedSelect);

  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Set_RC_Name(AWinControl, Widget);
  
  TGtkPrivateListClass(WSPrivate).SetCallbacks(Widget, WidgetInfo);
end;

class function TGtkWSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var
  ScrolledWindow: PGtkScrolledWindow;
  VertAdj: PGTKAdjustment;
  AdjValue: integer;
  ListWidget: PGtkList;
  AWidget: PGtkWidget;
  GListItem: PGList;
  ListItemWidget: PGtkWidget;
begin
  Result:=-1;

  if ACustomListBox.FCompStyle in [csListBox, csCheckListBox] then
  begin
    AWidget:=PGtkWidget(ACustomListBox.Handle);
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.CoreWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
    if VertAdj=nil then
      AdjValue:=y
    else
      AdjValue:=RoundToInt(VertAdj^.value)+y;
    GListItem:=ListWidget^.children;
    while GListItem<>nil do begin
      inc(Result);
      ListItemWidget:=PGtkWidget(GListItem^.data);
      dec(AdjValue,ListItemWidget^.Allocation.Height);
      if AdjValue<0 then exit;
      GListItem:=GListItem^.next;
    end;
    Result:=-1;
  end;
end;

class function TGtkWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox
  ): integer;
var
  Widget: PGtkWidget;// pointer to gtk-widget
  GList : pGList;    // Only used for listboxes, replace with widget!!!!!
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  {!$IFdef GTK1}
  if Handle<>0 then
  begin
    Widget := nil;
    if Widget = nil then
    begin
      GList:= PGtkList(GetWidgetInfo(Pointer(Handle), True)^.
                        CoreWidget)^.selection;
      if GList <> nil then
        Widget:= PGtkWidget(GList^.data);
    end;
    if Widget = nil then
      Result:= -1
    else
      Result:= gtk_list_child_position(PGtkList(
                    GetWidgetInfo(Pointer(Handle), True)^.
                                  CoreWidget), Widget);
  end
  else
     Result:=-1;
  {!$EndIf}
end;

class function TGtkWSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
var
  ScrolledWindow: PGtkScrolledWindow;
  VertAdj: PGTKAdjustment;
  AdjValue: integer;
  ListWidget: PGtkList;
  AWidget: PGtkWidget;
  GListItem: PGList;
  ListItemWidget: PGtkWidget;
begin
  Result:=false;
  FillChar(ARect,SizeOf(ARect),0);

  if ACustomListBox.FCompStyle in [csListBox, csCheckListBox] then
  begin
    AWidget:=PGtkWidget(ACustomListBox.Handle);
    ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.CoreWidget);
    ScrolledWindow:=PGtkScrolledWindow(AWidget);
    VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);

    if VertAdj=nil then
      AdjValue:=0
    else
      AdjValue:= (-RoundToInt(VertAdj^.value));
      
    GListItem:=ListWidget^.children;
    while GListItem<>nil do
    begin
      ListItemWidget:=PGtkWidget(GListItem^.data);
      if Index=0 then
      begin
        ARect.Left:=0;
        ARect.Top:=AdjValue;
        ARect.Right:=ListItemWidget^.Allocation.Width;
        ARect.Bottom:=ARect.Top+ListItemWidget^.Allocation.Height;
        Result:=true;
        exit;
      end;
      inc(AdjValue,ListItemWidget^.Allocation.Height);
      dec(Index);
      GListItem:=GListItem^.next;
    end;
  end;
end;

class function  TGtkWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox
  ): integer;
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;

  Result := g_list_length(PGtkList(GetWidgetInfo(Pointer(Handle),
     True)^.CoreWidget)^.selection);
end;

class function TGtkWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox;
  const AIndex: integer): boolean;
var
  Handle: HWND;
  Widget      : PGtkWidget; // pointer to gtk-widget (local use when neccessary)
  //GList       : pGList;     // Only used for listboxes, replace with widget!!!!!
  ListItem    : PGtkListItem;// currently only used for listboxes
begin
  Result := false;      { assume: nothing found }
  Handle := ACustomListBox.Handle;

  { Get the child in question of that index }
  Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
  ListItem:= g_list_nth_data(PGtkList(Widget)^.children, AIndex);
  if (ListItem<>nil)
  and (g_list_index(PGtkList(Widget)^.selection, ListItem)>=0)
  then Result:=true

  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.GetSelected ',DbgSName(ACustomListBox),' Index=',dbgs(AIndex),' Selected=',dbgs(Result));
end;

class function  TGtkWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox
  ): TStrings;
var
  Widget: PGtkWidget;// pointer to gtk-widget
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;

  Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
  Result := TGtkListStringList.Create(PGtkList(Widget),
    ACustomListBox, ACustomListBox.fCompStyle = csCheckListBox);
  if ACustomListBox is TCustomListBox then
    TGtkListStringList(Result).Sorted := ACustomListBox.Sorted;
end;

class function  TGtkWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := GetIndexAtXY(ACustomListBox, 0, 0);
end;

class procedure TGtkWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox;
  AIndex: integer; ASelected: boolean);
var
  Widget: PGtkWidget;// pointer to gtk-widget (local use when neccessary)
  Handle: HWND;
begin
  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.SelectItem ',DbgSName(ACustomListBox),' Index=',dbgs(AIndex),' Selected=',dbgs(ASelected));
  Handle := ACustomListBox.Handle;

  Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
  if ASelected then gtk_list_select_item(PGtkList(Widget), AIndex)
  else gtk_list_unselect_item(PGtkList(Widget), AIndex);
end;

class procedure TGtkWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
var
  Handle: HWND;
  Widget: PGtkWidget;// pointer to gtk-widget
begin
  Handle := ACustomListBox.Handle;
  Widget:= PGtkWidget(PGtkBin(Handle)^.child);
  gtk_viewport_set_shadow_type(PGtkViewPort(Widget), BorderStyleShadowMap[ACustomListBox.BorderStyle]);
end;

class procedure TGtkWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox;
  const AIndex: integer);
var
  Handle: HWND;
  Widget: PGtkWidget;
begin
  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.SetItemIndex ',DbgSName(ACustomListBox),' Index=',dbgs(AIndex));
  Handle := ACustomListBox.Handle;
  if Handle<>0 then 
  begin
    LockOnChange(PGtkObject(Handle),+1);
    Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;

    if AIndex >= 0 then
      gtk_list_select_item(PGtkList(Widget), AIndex)
    else
      gtk_list_unselect_all(PGtkList(Widget));

    LockOnChange(PGtkObject(Handle),-1);
  end;
end;

class procedure TGtkWSCustomListBox.SetSelectionMode(
  const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.SetSelectionMode ',DbgSName(ACustomListBox));
  TGtkWidgetSet(WidgetSet).SetSelectionMode(ACustomListBox,
    PGtkWidget(ACustomListBox.Handle), AMultiSelect, AExtendedSelect);
end;

class procedure TGtkWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.SetSorted ',DbgSName(ACustomListBox));
  if AList is TGtkListStringList then
    TGtkListStringList(AList).Sorted := ASorted
  else
    raise Exception.Create('');
end;

class procedure TGtkWSCustomListBox.SetTopIndex(
  const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
var
  ScrolledWindow: PGtkScrolledWindow;
  VertAdj: PGTKAdjustment;
  AdjValue, MaxAdjValue: integer;
  ListWidget: PGtkList;
  AWidget: PGtkWidget;
  GListItem: PGList;
  ListItemWidget: PGtkWidget;
  i: Integer;
  requisition: TGtkRequisition;
begin
  //if CompareText(ACustomListBox.Name,'LBProperties')=0 then
  //  debugln('TGtkWSCustomListBox.SetTopIndex ',DbgSName(ACustomListBox));
  AWidget:=PGtkWidget(ACustomListBox.Handle);
  ListWidget:=PGtkList(GetWidgetInfo(AWidget, True)^.CoreWidget);
  ScrolledWindow:=PGtkScrolledWindow(AWidget);
  AdjValue:=0;
  GListItem:=ListWidget^.children;
  i:=0;
  while GListItem<>nil do begin
    ListItemWidget:=PGtkWidget(GListItem^.data);
    if i>=NewTopIndex then break;
    if ListItemWidget<>nil then begin
      gtk_widget_size_request(ListItemWidget,@requisition);
      inc(AdjValue,requisition.height);
    end;
    //DebugLn(['TGtkWSCustomListBox.SetTopIndex ',i,' AdjValue=',AdjValue,' Flags=',WidgetFlagsToString(ListItemWidget)]);
    inc(i);
    GListItem:=GListItem^.next;
  end;
  VertAdj:=gtk_scrolled_window_get_vadjustment(ScrolledWindow);
  MaxAdjValue:=RoundToInt(VertAdj^.upper);
  if AdjValue>MaxAdjValue then AdjValue:=MaxAdjValue;
  //DebugLn(['TGtkWSCustomListBox.SetTopIndex AdjValue=',AdjValue,' VertAdj^.upper=',VertAdj^.upper,' VertAdj^.page_size=',VertAdj^.page_size]);
  gtk_adjustment_set_value(VertAdj,AdjValue);
end;

class procedure TGtkWSCustomListBox.SetColor(const AWinControl: TWinControl);
var
  AWidget, ListWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  AWidget := PGtkWidget(AWinControl.Handle);
  ListWidget := GetWidgetInfo(AWidget, True)^.CoreWidget;
  GtkWidgetSet.SetWidgetColor(ListWidget, AWinControl.Font.Color,
    AWinControl.Color,
    [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
     GTK_STYLE_BASE]);
end;

class procedure TGtkWSCustomListBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
  GList: PGList;
  ChildWidget: PGTKLabel;
begin
  if not AWinControl.HandleAllocated then exit;

  { Get the selections }
  Widget := GetWidgetInfo(Pointer(AWinControl.Handle))^.CoreWidget;
  GList :=  PGtkList(Widget)^.children;
  while Assigned(GList) do
  begin
    //  DebugLn('TGtkWSCustomListBox.SetFont for item ',PGTKLabel(PGtkBin(GList^.data)^.child)^.thelabel);
    ChildWidget := PGTKLabel(PGtkBin(GList^.data)^.child);

    GtkWidgetSet.SetWidgetColor(PGtkWidget(ChildWidget), AFont.Color, clNone,
      [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
      GtkWidgetSet.SetWidgetFont(PGtkWidget(ChildWidget), AFont);
      GList := GList^.Next;
  end;
end;

{$ENDIF}


{ TGtkWSCustomComboBox }

{$IFDEF GTK1}

function gtkComboBoxChanged(Widget: PGtkWidget; Info: PWidgetInfo): GBoolean; cdecl;
var
  Mess : TLMessage;
  GtkComboWidget: PGtkCombo;
  GtkListWidget: PGtkList;
  LCLIndex: PInteger;
  GtkIndex: Integer;
begin
  Result := CallBackDefaultReturn;

  if ComponentIsDestroyingHandle(TWinControl(Info^.LCLObject)) or
     (Info^.ChangeLock > 0) or
     (gtk_signal_n_emissions_by_name(PGtkObject(widget), 'changed') > 1) or
     (GTK_WIDGET_VISIBLE(PGtkCombo(Info^.CoreWidget)^.popwin)) then exit;

  {$IFDEF EventTrace}
  EventTrace('changed', Info^.LCLObject);
  {$ENDIF}

  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := LM_CHANGED;
  DeliverMessage(Info^.LCLObject, Mess);

  GtkComboWidget := PGtkCombo(Info^.CoreWidget);
  GtkListWidget := PGtkList(GtkComboWidget^.list);
  LCLIndex := PInteger(Info^.UserData);

  //Check if an item is selected
  if (GtkListWidget^.selection <> nil) then
  begin
    GtkIndex := gtk_list_child_position(GtkListWidget,
      PGtkWidget(GtkListWidget^.selection^.data));
    //If the selected item index changed send a LM_SELCHANGE msg
    if LCLIndex^ <> GtkIndex then
    begin
      gtk_list_set_selection_mode(GtkListWidget, GTK_SELECTION_BROWSE);
      LCLIndex^ := GtkIndex;
      Mess.Msg := LM_SELCHANGE;
      DeliverMessage(Info^.LCLObject, Mess);
    end;
  end
  else
  begin
    LCLIndex^ := -1;
    gtk_list_set_selection_mode(GtkListWidget, GTK_SELECTION_SINGLE);
  end;
end;

class function TGtkWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
var
  ComboWidget: PGtkCombo;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'GetDroppedDown') then
    Exit(False);
  ComboWidget:=PGtkCombo(ACustomComboBox.Handle);
  Result := GTK_WIDGET_VISIBLE(ComboWidget^.popwin);
end;

class procedure TGtkWSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);

  procedure gtk_combo_get_pos(combo : PGtkCombo; var x : gint; var  y : gint;
    var height : gint; var width : gint);
  var
    popwin : PGtkbin;
    widget : PGtkWidget;
    popup : PGtkScrolledwindow;
    real_height : gint;
    list_requisition : PGtkRequisition;
    show_hscroll : gboolean;
    show_vscroll : gboolean;
    avail_height : gint;
    min_height : gint;
    alloc_width : gint;
    work_height : gint;
    old_height : gint;
    old_width : gint;
    okay_to_exit : boolean;
  const
    EMPTY_LIST_HEIGHT = 15;
  begin
    show_hscroll := False;
    show_vscroll := False;

    widget := GTK_WIDGET(combo);
    popup  := GTK_SCROLLED_WINDOW (combo^.popup);
    popwin := GTK_BIN (combo^.popwin);

    gdk_window_get_origin (combo^.entry^.window, @x, @y);

    real_height := MIN (combo^.entry^.requisition.height,
                    combo^.entry^.allocation.height);
    y := y + real_height;

    avail_height := gdk_screen_height () - y;

    New(list_requisition);
    if combo^.list<>nil then begin
      gtk_widget_size_request (combo^.list, list_requisition);
    end else begin
      list_requisition^.height:=1;
      list_requisition^.width:=1;
    end;

    min_height := MIN (list_requisition^.height,popup^.vscrollbar^.requisition.height);
    if  GTK_LIST (combo^.list)^.children = nil then
      list_requisition^.height := list_requisition^.height + EMPTY_LIST_HEIGHT;

    alloc_width := (cardinal(widget^.allocation.width) -
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(popwin))) -
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(popwin))^) -
      2 * border_width(GTK_CONTAINER (combo^.popup)^) -
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(PGTKBin(popup)))^) -
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(PGTKBin(popup)))));

    work_height := (2 * cardinal(gtk_widget_get_ythickness(gtk_bin_get_child(popwin))) +
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(popwin))^) +
      2 * border_width(GTK_CONTAINER (combo^.popup)^) +
      2 * border_width(GTK_CONTAINER (gtk_bin_get_child(PGTKBin(popup)))^) +
      2 * cardinal(gtk_widget_get_xthickness(gtk_bin_get_child(PGTKBin(popup)))));

    repeat
      okay_to_exit := True;
      old_width := alloc_width;
      old_height := work_height;

      if ((not show_hscroll) and (alloc_width < list_requisition^.width)) then
      begin
           work_height := work_height +  popup^.hscrollbar^.requisition.height +
          GTK_SCROLLED_WINDOW_CLASS(gtk_object_get_class(combo^.popup))^.scrollbar_spacing;
        show_hscroll := TRUE;
        okay_to_exit := False;
      end;
      if ((not show_vscroll) and (work_height + list_requisition^.height > avail_height)) then
      begin
        if ((work_height + min_height > avail_height) and (y - real_height > avail_height)) then
        begin
          y := y - (work_height + list_requisition^.height + real_height);
          break;
        end;
        alloc_width := alloc_width -
          popup^.vscrollbar^.requisition.width +
          GTK_SCROLLED_WINDOW_CLASS(gtk_object_get_class(combo^.popup))^.scrollbar_spacing;
        show_vscroll := TRUE;
        okay_to_exit := False;
      end;
    until ((old_width <> alloc_width) or (old_height <> work_height) or okay_to_exit);

    width := widget^.allocation.width;
    if (show_vscroll) then
      height := avail_height
    else
      height := work_height + list_requisition^.height;
    if (x < 0) then
      x := 0;

    Dispose(list_requisition);
  end;

var
  ComboWidget: PGtkCombo;
  height, width, x, y : gint;
  old_width, old_height : gint;
begin
  if not WSCheckHandleAllocated(ACustomComboBox, 'SetDroppedDown') then Exit;
  ComboWidget:=PGtkCombo(ACustomComboBox.Handle);
  if ADroppedDown<>GTK_WIDGET_VISIBLE(ComboWidget^.popwin) then begin
    if ADroppedDown then begin
      old_width := ComboWidget^.popwin^.allocation.width;
      old_height := ComboWidget^.popwin^.allocation.height;
      gtk_combo_get_pos(ComboWidget,x,y,height,width);
      if ((old_width <> width) or (old_height <> height)) then
      begin
        gtk_widget_hide (GTK_SCROLLED_WINDOW(ComboWidget^.popup)^.hscrollbar);
        gtk_widget_hide (GTK_SCROLLED_WINDOW(ComboWidget^.popup)^.vscrollbar);
      end;
      gtk_widget_set_uposition (comboWidget^.popwin,x, y);
      gtk_widget_set_usize(ComboWidget^.popwin,width ,height);
      gtk_widget_realize(ComboWidget^.popwin);

      {$IFDEF DebugGDKTraps}
      BeginGDKErrorTrap;
      {$ENDIF}
      gdk_window_resize(ComboWidget^.popwin^.window,width,height);
      {$IFDEF DebugGDKTraps}
      EndGDKErrorTrap;
      {$ENDIF}

      gtk_widget_show (ComboWidget^.popwin);
      gtk_widget_grab_focus(ComboWidget^.popwin);
    end else
      gtk_widget_hide (ComboWidget^.popwin);
  end;
end;

class procedure TGtkWSCustomComboBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  with TGtkWidgetset(Widgetset) do
  begin
    //gtk1 and gtk2 have different 'changed' event handlers
    g_signal_connect(PGtkObject(PGtkCombo(AGtkWidget)^.entry),
      'changed', TGTKSignalFunc(@gtkComboBoxChanged), AWidgetInfo);
    g_signal_connect(PGtkObject(PGtkCombo(AGtkWidget)^.popwin),
      'hide', TGTKSignalFunc(@gtkComboBoxChanged), AWidgetInfo);
    SetCallback(LM_COMMAND, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
  end;
end;

class function TGtkWSCustomComboBox.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  ComboBox: TComboBox absolute AWinControl;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  ComboWidget: PGtkCombo absolute Widget;

  ItemList: TGtkListStringList;
  GtkList: PGtkList;
  Allocation: TGtkAllocation;
  LCLIndex: PInteger;
begin
  Widget := gtk_combo_new();

  SetMainWidget(Widget, ComboWidget^.entry);
  SetMainWidget(Widget, ComboWidget^.button);

  gtk_combo_disable_activate(ComboWidget);
  gtk_combo_set_case_sensitive(ComboWidget, GdkTrue);

  GtkList:=PGtkList(ComboWidget^.List);

  // Items
  ItemList:= TGtkListStringList.Create(GtkList, ComboBox, False);
  gtk_object_set_data(PGtkObject(Widget), GtkListItemLCLListTag, ItemList);
  ItemList.Assign(ComboBox.Items);
  ItemList.Sorted:= ComboBox.Sorted;

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  New(LCLIndex);
  WidgetInfo^.UserData := LCLIndex;
  WidgetInfo^.DataOwner := True;

  // ItemIndex

  // Switching the selection mode is necessary to avoid the following problems:
  // - The first added item is automatically selected when mode is "BROWSE"
  // - Is not possible to select the previous selected item if mode is "BROWSE"
  //   and current index is -1
  // - The item is deselected after a second click if mode is "SINGLE"
  // - The OnSelect event could be fired after add the first item when mode is
  //   "BROWSE". Since LM_SELCHANGE is sent in 'changed' event now, no problem.
  // So basically the mode is set to BROWSE when a item is selected and
  //   "SINGLE" when there's no item selected
  if ComboBox.ItemIndex >= 0 then
  begin
    gtk_list_set_selection_mode(GtkList, GTK_SELECTION_BROWSE);
    gtk_list_select_item(GtkList, ComboBox.ItemIndex);
    LCLIndex^ := ComboBox.ItemIndex;
  end
  else
  begin
    gtk_list_set_selection_mode(GtkList, GTK_SELECTION_SINGLE);
    LCLIndex^ := -1;
  end;

  // MaxLength
  gtk_entry_set_max_length(PGtkEntry(ComboWidget^.entry),guint16(ComboBox.MaxLength));

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle(PtrUInt(Widget));


  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class function  TGtkWSCustomComboBox.GetSelStart(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := WidgetGetSelStart(PGtkWidget(PGtkCombo(ACustomComboBox.Handle
                              )^.entry));
end;

class function  TGtkWSCustomComboBox.GetSelLength(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  with PGtkOldEditable(PGtkCombo(ACustomComboBox.Handle)^.entry)^ do begin
    Result:= Abs(integer(selection_end_pos)-integer(selection_start_pos));
  end;
end;

class function TGtkWSCustomComboBox.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  //DebugLn('TGtkWSCustomComboBox.GetText ',DbgSName(ACustomComboBox),' ',GetWidgetDebugReport(PGtkWidget(ACustomComboBox.Handle)));
  AText := gtk_entry_get_text(PGtkEntry(PGtkCombo(AWinControl.Handle)^.entry));
  //if AWinControl.Name='FileExtensionsComboBox' then
  //  DebugLn('TGtkWSCustomComboBox.GetText ',DbgSName(AWinControl),' ',GetWidgetDebugReport(PGtkWidget(AWinControl.Handle)),' AText="',AText,'"');
  Result:=true;
end;

class function TGtkWSCustomComboBox.GetItemIndex(
  const ACustomComboBox: TCustomComboBox): integer;
var
  ListWidget: PGtkList;
begin
  //DebugLn('TGtkWSCustomComboBox.GetItemIndex ',DbgSName(ACustomComboBox),' ',GetWidgetDebugReport(PGtkWidget(ACustomComboBox.Handle)));
  ListWidget := PGtkList(PGtkCombo(ACustomComboBox.Handle)^.list);
  if ListWidget^.selection <> nil then
    Result := gtk_list_child_position(ListWidget, ListWidget^.selection^.data)
  else
    Result := -1;
end;

class function  TGtkWSCustomComboBox.GetMaxLength(
  const ACustomComboBox: TCustomComboBox): integer;
begin
  Result:= PGtkEntry(PGtkCombo(ACustomComboBox.Handle)^.entry)^.text_max_length;
end;

class procedure TGtkWSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox;
  NewTraverseList: boolean);
var
  GtkCombo: PGtkCombo;
begin
  GtkCombo := GTK_COMBO(Pointer(ACustomComboBox.Handle));
  if ACustomComboBox.ArrowKeysTraverseList then
  begin
    gtk_combo_set_use_arrows(GtkCombo,GdkTrue);
  end else
  begin
    gtk_combo_set_use_arrows(GtkCombo,GdkFalse);
  end;
end;

class procedure TGtkWSCustomComboBox.SetSelStart(
  const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  if NewStart < 0 then NewStart := 0; // prevent SegFault in gtk
  gtk_editable_set_position(
           PGtkOldEditable(PGtkCombo(ACustomComboBox.Handle)^.entry), NewStart);
end;

class procedure TGtkWSCustomComboBox.SetSelLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
  WidgetSetSelLength(PGtkCombo(ACustomComboBox.Handle)^.entry, NewLength);
end;

class procedure TGtkWSCustomComboBox.SetItemIndex(
  const ACustomComboBox: TCustomComboBox; NewIndex: integer);
var
  ComboWidget: PGtkCombo;
  WidgetInfo: PWidgetInfo;
begin
  ComboWidget := PGtkCombo(ACustomComboBox.Handle);
  WidgetInfo := GetWidgetInfo(ComboWidget);
  //Store the LCLIndex
  PInteger(WidgetInfo^.UserData)^ := NewIndex;
  //Avoid calling OnChange/OnSelect events
  Inc(WidgetInfo^.ChangeLock);
  //gtk_list_select_item does not update the list when Index is -1
  if NewIndex > -1 then
  begin
    gtk_list_set_selection_mode(PGtkList(ComboWidget^.list),
      GTK_SELECTION_BROWSE);
    gtk_list_select_item(PGtkList(ComboWidget^.list), NewIndex);
  end
  else
  begin
    gtk_list_set_selection_mode(PGtkList(ComboWidget^.list),
      GTK_SELECTION_SINGLE);
    gtk_entry_set_text(PGtkEntry(ComboWidget^.entry), '');
  end;
  Dec(WidgetInfo^.ChangeLock);
end;

class procedure TGtkWSCustomComboBox.SetMaxLength(
  const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
  gtk_entry_set_max_length(PGtkEntry(PGtkCombo(ACustomComboBox.Handle)^.entry),
                           guint16(NewLength));
end;

class procedure TGtkWSCustomComboBox.SetStyle(
  const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  GtkCombo: PGtkCombo;
begin
  GtkCombo := GTK_COMBO(Pointer(ACustomComboBox.Handle));
  case ACustomComboBox.Style of
    csDropDownList :
      begin
        // do not set ok_if_empty = true, otherwise it can hang focus
        gtk_combo_set_value_in_list(GtkCombo,GdkTrue,GdkTrue);
        gtk_combo_set_use_arrows_always(GtkCombo,GdkTrue);
        gtk_combo_set_case_sensitive(GtkCombo,GdkFalse);
      end;
    else
      begin
        // do not set ok_if_empty = true, otherwise it can hang focus
        gtk_combo_set_value_in_list(GtkCombo,GdkFalse,GdkTrue);
        gtk_combo_set_use_arrows_always(GtkCombo,GdkFalse);
        gtk_combo_set_case_sensitive(GtkCombo,GdkTrue);
      end;
  end;
end;

class procedure TGtkWSCustomComboBox.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  ComboControl: TCustomComboBox absolute AWinControl;
  ComboWidget: PGtkCombo;
  WidgetInfo: PWidgetInfo;
  i: Integer;
begin
  if ComboControl.ReadOnly then
  begin
    i := ComboControl.Items.IndexOf(AText);
    TGtkWSCustomComboBox.SetItemIndex(ComboControl, i);
  end
  else
  begin
    ComboWidget := PGtkCombo(AWinControl.Handle);
    WidgetInfo := GetWidgetInfo(ComboWidget);
    // lock combobox, so that no OnChange event is not fired
    Inc(WidgetInfo^.ChangeLock);
    // set text
    // The String > PChar conversion ensures at least a null terminated string
    gtk_entry_set_text(PGtkEntry(ComboWidget^.entry), PChar(AText));
    // unlock combobox
    Dec(WidgetInfo^.ChangeLock);
  end;
end;

class function  TGtkWSCustomComboBox.GetItems(
  const ACustomComboBox: TCustomComboBox): TStrings;
begin
  Result := TStrings(gtk_object_get_data(PGtkObject(ACustomComboBox.Handle),
                                         GtkListItemLCLListTag));
end;

class procedure TGtkWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
begin
  TGtkListStringList(AList).Sorted := IsSorted;
end;

class procedure TGtkWSCustomComboBox.SetColor(const AWinControl: TWinControl);
var
  AWidget, EntryWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  AWidget := PGtkWidget(AWinControl.Handle);
  EntryWidget := PGtkCombo(AWidget)^.entry;
  GtkWidgetSet.SetWidgetColor(EntryWidget, AWinControl.Font.Color, AWinControl.Color,
    [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,GTK_STYLE_BASE]);
end;

class procedure TGtkWSCustomComboBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  AWidget: PGTKWidget;
  EntryWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  AWidget := PGtkWidget(AWinControl.Handle);
  EntryWidget := PGtkCombo(AWidget)^.entry;

  if EntryWidget<>nil then 
  begin
    GtkWidgetSet.SetWidgetColor(EntryWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    GtkWidgetSet.SetWidgetFont(EntryWidget, AFont);
  end;
end;
{$ENDIF}

{ TGtkWSCustomEdit }

class procedure TGtkWSCustomEdit.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  with TGtkWidgetset(Widgetset) do
  begin
    SetCallback(LM_CHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_ACTIVATE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_CUT, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_COPY, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_PASTE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
  end;
end;

class function TGtkWSCustomEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget; // ptr to the newly created GtkWidget
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_entry_new();
  gtk_editable_set_editable(PGtkEditable(Widget), not TCustomEdit(AWinControl).ReadOnly);
  gtk_widget_show_all(Widget);
  Result := TLCLIntfHandle(PtrUInt(Widget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  if Result = 0 then
    Exit;
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

class function  TGtkWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := WidgetGetSelStart(GetWidgetInfo(Pointer(ACustomEdit.Handle),
                              true)^.CoreWidget);
end;

class function TGtkWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  with PGtkOldEditable(GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.
    CoreWidget)^ do
  begin
    Result:=Abs(integer(selection_end_pos)-integer(selection_start_pos));
  end;
end;

class procedure TGtkWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit;
  NewCase: TEditCharCase);
begin
  // TODO: implement me!
end;

class procedure TGtkWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
begin
  // XXX TODO: GTK 1.x does not support EchoMode emNone.
  // This will have to be coded around, but not a priority
  SetPasswordChar(ACustomEdit, ACustomEdit.PasswordChar);
end;

class procedure TGtkWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  Widget: PGtkWidget;
begin
  Widget:=PGtkWidget(ACustomEdit.Handle);
  if GtkWidgetIsA(Widget, GTK_TYPE_ENTRY) then
    gtk_entry_set_max_length(GTK_ENTRY(Widget), guint16(NewLength));
end;

class procedure TGtkWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit;
  NewChar: char);
var
  Widget: PGtkWidget;
begin
  Widget:=PGtkWidget(ACustomEdit.Handle);
  if GtkWidgetIsA(Widget, GTK_TYPE_ENTRY) then
    gtk_entry_set_visibility(GTK_ENTRY(Widget),
      (ACustomEdit.EchoMode = emNormal) and (NewChar = #0));
end;

class procedure TGtkWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
var
  Widget: PGtkWidget;
begin
  Widget := PGtkWidget(ACustomEdit.Handle);
  if GtkWidgetIsA(Widget, GTK_TYPE_ENTRY) then
    gtk_entry_set_editable(GTK_ENTRY(Widget), not NewReadOnly);
end;

class procedure TGtkWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
var
  Widget: PGtkWidget;
  MaxPos: Integer;
begin
  Widget:=GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.CoreWidget;
  if WidgetGetSelStart(Widget)=NewStart then exit;
  // sometimes the gtk freezes the memo, changes something and emits the change
  // event. Then the LCL gets notified and wants to react: force thaw (unfreeze)
  if GTK_IS_TEXT(Widget) then
  begin
    gtk_text_thaw(PGtkText(Widget));
    MaxPos := gtk_text_get_length(PGtkText(Widget));
  end
  else
  if GTK_IS_ENTRY(Widget) then
    MaxPos := PGtkEntry(Widget)^.text_length
  else
    MaxPos := 0;
  gtk_editable_set_position(PGtkOldEditable(Widget), Min(NewStart, MaxPos));
  WidgetSetSelLength(Widget,0); // Setting the selection start should cancel any selection
end;

class procedure TGtkWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  WidgetSetSelLength(GetWidgetInfo(Pointer(ACustomEdit.Handle),true)^.CoreWidget,
                     NewLength);
end;

class procedure TGtkWSCustomEdit.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  Widget: PGtkWidget;
  Mess : TLMessage;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then
    Exit;
  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText START ',DbgSName(AWinControl),' AText="',AText,'"']);
  {$ENDIF}
  Widget:=PGtkWidget(AWinControl.Handle);
  // some gtk2 versions fire the change event twice
  // lock the event and send the message afterwards
  // see bug http://bugs.freepascal.org/view.php?id=14615
  LockOnChange(PgtkObject(Widget), +1);
  try
    gtk_entry_set_text(PGtkEntry(Widget), PChar(AText));
  finally
    LockOnChange(PgtkObject(Widget), -1);
  end;
  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText SEND TEXTCHANGED message ',DbgSName(AWinControl),' New="',gtk_entry_get_text(PGtkEntry(AWinControl.Handle)),'"']);
  {$ENDIF}
  FillByte(Mess,SizeOf(Mess),0);
  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(AWinControl, Mess);

  {$IFDEF VerboseTWinControlRealText}
  DebugLn(['TGtkWSCustomEdit.SetText END ',DbgSName(AWinControl),' New="',gtk_entry_get_text(PGtkEntry(AWinControl.Handle)),'"']);
  {$ENDIF}
end;

class procedure TGtkWSCustomEdit.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSCustomEdit.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

class procedure TGtkWSCustomEdit.SetColor(const AWinControl: TWinControl);
var
  AWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  AWidget := PGtkWidget(AWinControl.Handle);
  // don't change selected state
  GtkWidgetSet.SetWidgetColor(AWidget, clNone, AWinControl.Color,
    [GTK_STATE_NORMAL, GTK_STYLE_BASE]);
end;

{ TGtkWSCustomStaticText }

class function TGtkWSCustomStaticText.GetLabelWidget(AFrame: PGtkFrame): PGtkLabel;
begin
  {$IFDEF GTK2}
  Result := PGtkLabel(PGtkBin(GetBoxWidget(AFrame))^.child);
  {$ELSE}
  Result := PGtkLabel(GetBoxWidget(AFrame)^.bin.child);
  {$ENDIF}
end;

class function TGtkWSCustomStaticText.GetBoxWidget(AFrame: PGtkFrame): PGtkEventBox;
begin
  {$IFDEF GTK2}
  Result := PGtkEventBox(PGtkBin(AFrame)^.child);
  {$ELSE}
  Result := PGtkEventBox(AFrame^.bin.Child);
  {$ENDIF}
end;

class function TGtkWSCustomStaticText.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  AStaticText: TCustomStaticText;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
  EventBox, LblWidget: PGtkWidget;
begin
  // TStaticText control is a Text area with frame around. Both Text and Area around
  // text can have their own color
  
  // To implement that in gtk we need:
  // 1. GtkLabel to handle Text
  // 2. GtkEventBox to draw color area around GtkLabel (since GtkLabel have no window)
  // 3. GtkFrame to draw frame around Text area
  // GtkFrame is our main widget - it is container and it contains GtkEventBox
  // GtkEventBox is also containter and it contains GtkLabel

  AStaticText := AWinControl as TCustomStaticText;
  Result := TLCLIntfHandle(PtrUInt(gtk_frame_new(nil))); // frame is the main container - to decorate label
  if Result = 0 then Exit;

  gtk_frame_set_shadow_type(PGtkFrame(Result), StaticBorderShadowMap[AStaticText.BorderStyle]);

  EventBox := gtk_event_box_new;  // our area
  LblWidget := gtk_label_new(PChar(TCustomStaticText(AWinControl).Caption)); // our text widget
  gtk_container_add(PGtkContainer(EventBox), LblWidget);
  SetLabelAlignment(PGtkLabel(LblWidget), AStaticText.Alignment);
  gtk_widget_show(LblWidget);
  gtk_widget_show(EventBox);
  gtk_container_add(PGtkContainer(Result), EventBox);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result), dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), AStaticText, AParams);
  WidgetInfo^.CoreWidget := EventBox;
  gtk_object_set_data(PGtkObject(EventBox), 'widgetinfo', WidgetInfo);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, PGtkWidget(Result));
  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

class procedure TGtkWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText;
  const NewAlignment: TAlignment);
var
  LblWidget: PGtkLabel;
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetAlignment')
  then Exit;

  LblWidget := GetLabelWidget(PGtkFrame(ACustomStaticText.Handle));
  SetLabelAlignment(LblWidget, NewAlignment);
end;

class procedure TGtkWSCustomStaticText.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl, PreferredWidth, PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSCustomStaticText.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

class function TGtkWSCustomStaticText.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  // The text is static, so let the LCL fallback to FCaption
  Result := False;
end;

class procedure TGtkWSCustomStaticText.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  FrameWidget: PGtkFrame;
  LblWidget: PGtkLabel;
  DC: HDC;
  ALabel: PChar;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  FrameWidget := PGtkFrame(AWinControl.Handle);
  LblWidget := GetLabelWidget(FrameWidget);

  if TStaticText(AWinControl).ShowAccelChar then
  begin
    DC := Widgetset.GetDC(HWND(PtrUInt(LblWidget)));
    ALabel := TGtkWidgetSet(WidgetSet).ForceLineBreaks(
                          DC, PChar(AText), TStaticText(AWinControl).Width, false);
    Widgetset.DeleteDC(DC);
    GtkWidgetSet.SetLabelCaption(LblWidget, ALabel
       {$IFDEF Gtk1}, AWinControl, PGtkWidget(FrameWidget), 'grab_focus'{$ENDIF});
    StrDispose(ALabel);
  end else
  begin
    gtk_label_set_text(LblWidget, PChar(AText));
    gtk_label_set_pattern(LblWidget, nil);
  end;
end;

class procedure TGtkWSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  if not WSCheckHandleAllocated(ACustomStaticText, 'SetStaticBorderStyle')
  then Exit;
  gtk_frame_set_shadow_type(PGtkFrame(ACustomStaticText.Handle), StaticBorderShadowMap[NewBorderStyle]);
end;

class procedure TGtkWSCustomStaticText.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));

  SignalConnect(AGtkWidget, 'grab_focus', @gtkActivateCB, AWidgetInfo);
end;

class procedure TGtkWSCustomStaticText.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;

  GtkWidgetSet.SetWidgetColor(PGtkWidget(GetBoxWidget(PGtkFrame(AWinControl.Handle))),
                              clNone, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtkWSCustomStaticText.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;
  
  Widget := PGtkWidget(GetLabelWidget(PGtkFrame(AWinControl.Handle)));

  GtkWidgetSet.SetWidgetColor(Widget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
  GtkWidgetSet.SetWidgetFont(Widget, AFont);
end;

{ TGtkWSButton }

function GtkWSButton_Clicked(AWidget: PGtkWidget; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  Msg: TLMessage;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;
  Msg.Msg := LM_CLICKED;
  Result := DeliverMessage(AInfo^.LCLObject, Msg) = 0;
end;

class procedure TGtkWSButton.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  SignalConnect(AGtkWidget, 'clicked', @GtkWSButton_Clicked, AWidgetInfo);
end;

{$IFDEF Gtk1}

class function TGtkWSButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Button: TCustomButton;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Button := AWinControl as TCustomButton;
  Result := TLCLIntfHandle(PtrUInt(gtk_button_new_with_label('button')));
  if Result = 0 then Exit;
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Pointer(Result),'button');
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Pointer(Result), Button, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, PGtkWidget(Result));
  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

class function TGtkWSButton.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  // The button text is static, so let the LCL fallback to FCaption
  Result := False;
end;

class procedure TGtkWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
  if not WSCheckHandleAllocated(AButton, 'SetDefault')
  then Exit;

  if ADefault
  and (GTK_WIDGET_CAN_DEFAULT(pgtkwidget(AButton.Handle))) then
    //gtk_widget_grab_default(pgtkwidget(handle))
  else begin
    {DebugLn('LM_BTNDEFAULT_CHANGED ',TCustomButton(Sender).Name,':',Sender.ClassName,' widget can not grab default ',
      ' visible=',GTK_WIDGET_VISIBLE(PGtkWidget(Handle)),
      ' realized=',GTK_WIDGET_REALIZED(PGtkWidget(Handle)),
      ' mapped=',GTK_WIDGET_MAPPED(PGtkWidget(Handle)),
      '');}
    //  gtk_widget_Draw_Default(pgtkwidget(Handle));  //this isn't right but I'm not sure what to call
  end;
end;

class procedure TGtkWSButton.SetShortcut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortcut);
begin
  if not WSCheckHandleAllocated(AButton, 'SetShortcut')
  then Exit;

  {$IFDEF Gtk1}
  Accelerate(AButton, PGtkWidget(AButton.Handle), ShortCutK1, 'clicked');
  {$ENDIF}
  // gtk2: shortcuts are handled by the LCL
end;

class procedure TGtkWSButton.SetText(const AWinControl: TWinControl; const AText: String);
var
  BtnWidget: PGtkButton;
  LblWidget: PGtkLabel;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText')
  then Exit;

  BtnWidget := PGtkButton(AWinControl.Handle);
  {$IFDEF GTK2}
  LblWidget := PGtkLabel(PGtkBin(BtnWidget)^.Child);
  {$ELSE}
  LblWidget := PGtkLabel(BtnWidget^.Child);
  {$ENDIF}

  if LblWidget = nil
  then begin
    //DebugLn(Format('trace: [WARNING] Button %s(%s) has no label', [AWinControl.Name, AWinControl.ClassName]));
    LblWidget := PGtkLabel(gtk_label_new(''));
    gtk_container_add(PGtkContainer(BtnWidget), PGtkWidget(LblWidget));
  end;

  GtkWidgetSet.SetLabelCaption(LblWidget, AText
           {$IFDEF Gtk1}, AWinControl,PGtkWidget(BtnWidget), 'clicked'{$ENDIF});
end;

class procedure TGtkWSButton.SetColor(const AWinControl: TWinControl);
var
  Widget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  Widget := PGtkWidget(AWinControl.Handle);
  GtkWidgetSet.SetWidgetColor(Widget, clNone, AWinControl.Color,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

class procedure TGtkWSButton.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGTKWidget;
  LblWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  Widget:= PGtkWidget(AWinControl.Handle);
  LblWidget := (pGtkBin(Widget)^.Child);

  if LblWidget <> nil then 
  begin
    GtkWidgetSet.SetWidgetColor(LblWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    GtkWidgetSet.SetWidgetFont(LblWidget, AFont);
  end;
end;

class procedure TGtkWSButton.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSButton.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

{$ENDIF Gtk1}

{ TGtkWSCustomCheckBox }

class procedure TGtkWSCustomCheckBox.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  TGtkWidgetset(WidgetSet).SetCallback(LM_CHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
end;

{$IFDEF Gtk1}

class function TGtkWSCustomCheckBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := gtk_check_button_new_with_label(AParams.Caption);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, PGtkWidget(Result));
  SetCallbacks(PGtkWidget(Result), WidgetInfo);
end;

class function  TGtkWSCustomCheckBox.RetrieveState(
  const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
var
  ToggleButton: PGtkToggleButton;
begin
  ToggleButton:=PGtkToggleButton(ACustomCheckBox.Handle);
  if (gtk_object_get_data(PgtkObject(ToggleButton),'Grayed')<>nil) then
    Result:=cbGrayed
  else if gtk_toggle_button_get_active(ToggleButton) then
    Result := cbChecked
  else
    Result := cbUnChecked;
end;

class procedure TGtkWSCustomCheckBox.SetShortCut(
  const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut);
begin
  Accelerate(ACustomCheckBox, PGtkWidget(ACustomCheckBox.Handle), ShortCutK1,
    'activate_item');
end;

class procedure TGtkWSCustomCheckBox.SetState(const ACB: TCustomCheckBox;
  const ANewState: TCheckBoxState);
var
  GtkObject: PGtkObject;
begin
  if not WSCheckHandleAllocated(ACB, 'SetState')
  then Exit;

  GtkObject := PGtkObject(ACB.Handle);
  LockOnChange(GtkObject,1);
  if ANewState=cbGrayed then
    gtk_object_set_data(GtkObject, 'Grayed', GtkObject)
  else
    gtk_object_set_data(GtkObject, 'Grayed', nil);
  gtk_toggle_button_set_active(PGtkToggleButton(GtkObject), ANewState = cbChecked);
  LockOnChange(GtkObject,-1);
end;

class procedure TGtkWSCustomCheckBox.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl,PreferredWidth,PreferredHeight,
                          WithThemeSpace);
  //debugln('TGtkWSCustomCheckBox.GetPreferredSize ',DbgSName(AWinControl),' PreferredWidth=',dbgs(PreferredWidth),' PreferredHeight=',dbgs(PreferredHeight));
end;

class procedure TGtkWSCustomCheckBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGTKWidget;
  LblWidget: PGtkWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  Widget:= PGtkWidget(AWinControl.Handle);
  LblWidget := (pGtkBin(Widget)^.Child);

  if LblWidget<>nil then 
  begin
    GtkWidgetSet.SetWidgetColor(LblWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
    GtkWidgetSet.SetWidgetFont(LblWidget, AFont);
  end;
end;

{$ENDIF}

{ TGtkWSCustomMemo }

{$ifdef GTK1}

class procedure TGtkWSCustomMemo.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  with TGtkWidgetset(Widgetset) do
  begin
    SetCallback(LM_CHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_ACTIVATE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_CUT, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_COPY, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_PASTE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
  end;
end;

class function TGtkWSCustomMemo.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  P: Pointer;
  TempWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  P := gtk_scrolled_window_new(nil, nil);
  TempWidget := gtk_text_new(nil, nil);
  gtk_container_add(p, TempWidget);

  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(PGtkScrolledWindow(p)^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(p),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);
  gtk_text_set_adjustments(PGtkText(TempWidget),
    gtk_scrolled_window_get_hadjustment(PGtkScrolledWindow(p)),
    gtk_scrolled_window_get_vadjustment(PGtkScrolledWindow(p)));

  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);
  SetMainWidget(p, TempWidget);
  WidgetInfo^.CoreWidget := TempWidget;

  gtk_text_set_editable (PGtkText(TempWidget), not TCustomMemo(AWinControl).ReadOnly);
  if TCustomMemo(AWinControl).WordWrap then
    gtk_text_set_line_wrap(PGtkText(TempWidget), GdkTrue)
  else
    gtk_text_set_line_wrap(PGtkText(TempWidget), GdkFalse);
  gtk_text_set_word_wrap(PGtkText(TempWidget), GdkTrue);

  gtk_widget_show_all(P);
  
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P,dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(P));
  //DebugLn(['TGtkWSCustomMemo.CreateHandle ']);
  Set_RC_Name(AWinControl, P);
  SetCallbacks(P, WidgetInfo);
end;

class procedure TGtkWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo;
  const AText: string);
var
  Widget: PGtkWidget;
  CurMemoLen: gint;
begin
  if Length(AText) = 0 then
    exit;

  Widget:=GetWidgetInfo(Pointer(ACustomMemo.Handle), true)^.CoreWidget;
  gtk_text_freeze(PGtkText(Widget));
  CurMemoLen := gtk_text_get_length(PGtkText(Widget));
  //debugln('TGtkWSCustomMemo.AppendText "',AText,'" CurMemoLen=',dbgs(CurMemoLen));
  gtk_editable_insert_text(PGtkOldEditable(Widget), PChar(AText), Length(AText),
                           @CurMemoLen);
  //debugln('TGtkWSCustomMemo.AppendText B CurMemoLen=',dbgs(CurMemoLen));
  gtk_text_thaw(PGtkText(Widget));
end;

class function TGtkWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
var
Widget: PGtkText;
begin
  Widget:=PGtkText(GetWidgetInfo(Pointer(ACustomMemo.Handle), true)^.CoreWidget);
  Result:=TGtkMemoStrings.Create(Widget, ACustomMemo);
end;

class procedure TGtkWSCustomMemo.SetEchoMode(const ACustomEdit: TCustomEdit;
  NewMode: TEchoMode);
begin
  // no password char in memo
end;

class procedure TGtkWSCustomMemo.SetMaxLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  ImplWidget: PGtkWidget;
  i: integer;
begin
  if (ACustomEdit.MaxLength > 0) then 
  begin
    ImplWidget := GetWidgetInfo(PGtkWidget(ACustomEdit.Handle), true)^.CoreWidget;
    i := gtk_text_get_length(GTK_TEXT(ImplWidget));
    if i > ACustomEdit.MaxLength then 
       gtk_editable_delete_text(PGtkOldEditable(ImplWidget),
                                ACustomEdit.MaxLength, i);
  end;
end;

class procedure TGtkWSCustomMemo.SetPasswordChar(const ACustomEdit: TCustomEdit;
  NewChar: char);
begin
  // no password char in memo
end;

class procedure TGtkWSCustomMemo.SetReadOnly(const ACustomEdit: TCustomEdit;
  NewReadOnly: boolean);
var
  ImplWidget   : PGtkWidget;
begin
  ImplWidget:= GetWidgetInfo(PGtkWidget(ACustomEdit.Handle), true)^.CoreWidget;
  gtk_text_set_editable (GTK_TEXT(ImplWidget), not ACustomEdit.ReadOnly);
end;

class procedure TGtkWSCustomMemo.SetColor(const AWinControl: TWinControl);
var
  AWidget: PGTKWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;
  AWidget := PGtkWidget(AWinControl.Handle);
  AWidget := GetWidgetInfo(AWidget, true)^.CoreWidget;
  GtkWidgetSet.SetWidgetColor(AWidget, clNone, AWinControl.Color,
    [GTK_STATE_NORMAL, GTK_STATE_ACTIVE, GTK_STATE_PRELIGHT, GTK_STATE_SELECTED,
     GTK_STYLE_BASE]);
end;

class procedure TGtkWSCustomMemo.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  AWidget: PGTKWidget;
begin
  if not AWinControl.HandleAllocated then exit;

  AWidget:= PGtkWidget(AWinControl.Handle);
  AWidget:= GetWidgetInfo(AWidget, true)^.CoreWidget;

  if AWidget<>nil then begin
    GtkWidgetSet.SetWidgetColor(AWidget, AFont.Color, clNone,
       [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
        GTK_STYLE_TEXT]);
    GtkWidgetSet.SetWidgetFont(AWidget, AFont);
  end;
end;

class procedure TGtkWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo;
  const NewScrollbars: TScrollStyle);
var
  wHandle: HWND;
begin
  wHandle := ACustomMemo.Handle;
  case ACustomMemo.Scrollbars of
    ssHorizontal:     gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
    ssVertical:       gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
    ssBoth:           gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
    ssAutoHorizontal: gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
    ssAutoVertical:   gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    ssAutoBoth:       gtk_scrolled_window_set_policy(
                        GTK_SCROLLED_WINDOW(wHandle),
                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  else
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(wHandle),
                                   GTK_POLICY_NEVER, GTK_POLICY_NEVER);
  end;
end;

class procedure TGtkWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo;
  const NewWordWrap: boolean);
var
  ImplWidget   : PGtkWidget;
begin
  ImplWidget:= GetWidgetInfo(PGtkWidget(ACustomMemo.Handle), true)^.CoreWidget;
  gtk_text_freeze(PGtkText(ImplWidget));
  if ACustomMemo.WordWrap then
    gtk_text_set_line_wrap(GTK_TEXT(ImplWidget), GdkTrue)
  else
    gtk_text_set_line_wrap(GTK_TEXT(ImplWidget), GdkFalse);
  gtk_text_set_word_wrap(GTK_TEXT(ImplWidget), GdkTrue);
  gtk_text_thaw(PGtkText(ImplWidget));
end;

{$endif}

{ TGtkWSCustomGroupBox }

class procedure TGtkWSCustomGroupBox.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
end;

{$ifdef gtk1}
class function TGtkWSCustomGroupBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  TempWidget: PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p : pointer;          // ptr to the newly created GtkWidget
  Allocation: TGTKAllocation;
  WidgetInfo: PWidgetInfo;
begin
  if AParams.Caption <> '' then
    P := gtk_frame_new(AParams.Caption)
  else
    P := gtk_frame_new(nil);
  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);
  TempWidget := CreateFixedClientWidget;
  gtk_container_add(GTK_CONTAINER(p), TempWidget);
  WidgetInfo^.ClientWidget := TempWidget;
  WidgetInfo^.CoreWidget := TempWidget;
  gtk_object_set_data(PGtkObject(TempWidget), 'widgetinfo', WidgetInfo);
  gtk_widget_show(TempWidget);
  gtk_widget_show(P);

  Result := TLCLIntfHandle(PtrUInt(P));

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(P, @Allocation);

  Set_RC_Name(AWinControl, P);
  SetCallbacks(P, WidgetInfo);
end;

class procedure TGtkWSCustomGroupBox.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Widget: PGtkWidget;
  border_width: Integer;
begin
  Widget := PGtkWidget(AWinControl.Handle);

  PreferredWidth := (gtk_widget_get_xthickness(Widget)) * 2
                    +PGtkFrame(Widget)^.label_width;
  PreferredHeight := Max(gtk_widget_get_ythickness(Widget),
                         PGtkFrame(Widget)^.label_height)
                     + gtk_widget_get_ythickness(Widget);

  if WithThemeSpace then begin
    border_width:=(PGtkContainer(Widget)^.flag0 and bm_TGtkContainer_border_width)
                   shr bp_TGtkContainer_border_width;
    inc(PreferredWidth, border_width);
    inc(PreferredHeight,2*border_width);
  end;
end;

class procedure TGtkWSCustomGroupBox.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;
  if AText <> '' then
    gtk_frame_set_label(PGtkFrame(AWinControl.Handle), PChar(AText))
  else
    gtk_frame_set_label(PGtkFrame(AWinControl.Handle), nil);
end;
{$endif}

{ TGtkWSRadioButton }

class function TGtkWSRadioButton.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget, TempWidget: PGtkWidget;
  LabelWidget: PGtkLabel;
  TempInt: Integer;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  with TRadioButton(AWinControl) do
  begin
    // Look for our parent's control and use the first radio we find for grouping
    TempWidget := nil;
    if (Parent <> nil) then
    begin
      for TempInt := 0 to Parent.ControlCount - 1 do
      begin
        if (Parent.Controls[TempInt] is TRadioButton) and
           TWinControl(Parent.Controls[TempInt]).HandleAllocated then
        begin
          TempWidget := PGtkWidget(TWinControl(Parent.Controls[TempInt]).Handle);
          Break;
        end;
      end;
    end;
    
    if TempWidget <> nil then
      Widget := gtk_radio_button_new_with_label(PGtkRadioButton(TempWidget)^.group,'')
    else
      Widget := gtk_radio_button_new_with_label(nil, '');
      
    LabelWidget := PGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(Widget)^.Button)));
    GtkWidgetSet.SetLabelCaption(LabelWidget, AParams.Caption
       {$IFDEF Gtk1}, AWinControl, Widget, 'clicked'{$ENDIF});
  end;

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  TGtkWSCustomCheckBox.SetCallbacks(Widget, WidgetInfo);
end;

{ TGtkWSToggleBox }

class function TGtkWSToggleBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := gtk_toggle_button_new_with_label(AParams.Caption);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Pointer(Result), AWinControl, AParams);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);
  TGtkWSCustomCheckBox.SetCallbacks(Widget, WidgetInfo);
end;

end.
