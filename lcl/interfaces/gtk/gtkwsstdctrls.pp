{ $Id$}
{
 *****************************************************************************
 *                             GtkWSStdCtrls.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit GtkWSStdCtrls;

{$mode objfpc}{$H+}

interface

uses
  StdCtrls, SysUtils, Controls, Graphics,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  WSStdCtrls, WSLCLClasses, GtkInt, Classes, LCLType, GtkDef,
  GTKWinApiWindow, gtkglobals, gtkproc, InterfaceBase;


type

  { TGtkWSScrollBar }

  TGtkWSScrollBar = class(TWSScrollBar)
  private
  protected
  public
  end;

  { TGtkWSCustomGroupBox }

  TGtkWSCustomGroupBox = class(TWSCustomGroupBox)
  private
  protected
  public
  end;

  { TGtkWSGroupBox }

  TGtkWSGroupBox = class(TWSGroupBox)
  private
  protected
  public
  end;

  { TGtkWSCustomComboBox }

  TGtkWSCustomComboBox = class(TWSCustomComboBox)
  private
  protected
  public
    class function  GetSelStart(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetSelLength(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; override;
    class function  GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; override;

    class procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
      NewTraverseList: boolean); override;
    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); override;
    
    class function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; override;
    class procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); override;
  end;

  { TGtkWSComboBox }

  TGtkWSComboBox = class(TWSComboBox)
  private
  protected
  public
  end;

  { TGtkWSCustomListBox }

  TGtkWSCustomListBox = class(TWSCustomListBox)
  private
  protected
  public
    class function  GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function  GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function  GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean); override;
    class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;
  end;

  { TGtkWSListBox }

  TGtkWSListBox = class(TWSListBox)
  private
  protected
  public
  end;

  { TGtkWSCustomEdit }

  TGtkWSCustomEdit = class(TWSCustomEdit)
  private
  protected
  public
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;

    class procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); override;
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;

  { TGtkWSCustomMemo }

  TGtkWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class procedure AppendText(const ACustomMemo: TCustomMemo; AText: string); override;
{$ifdef GTK1}    
    class procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); override;
    class procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); override;
    class procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); override;
{$endif}
  end;

  { TGtkWSEdit }

  TGtkWSEdit = class(TWSEdit)
  private
  protected
  public
  end;

  { TGtkWSMemo }

  TGtkWSMemo = class(TWSMemo)
  private
  protected
  public
  end;

  { TGtkWSCustomLabel }

  TGtkWSCustomLabel = class(TWSCustomLabel)
  private
  protected
  public
    class procedure SetAlignment(const ACustomLabel: TCustomLabel; const NewAlignment: TAlignment); override;
    class procedure SetLayout(const ACustomLabel: TCustomLabel; const NewLayout: TTextLayout); override;
    class procedure SetWordWrap(const ACustomLabel: TCustomLabel; const NewWordWrap: boolean); override;
  end;

  { TGtkWSLabel }

  TGtkWSLabel = class(TWSLabel)
  private
  protected
  public
  end;

  { TGtkWSButtonControl }

  TGtkWSButtonControl = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TGtkWSCustomCheckBox }

  TGtkWSCustomCheckBox = class(TWSCustomCheckBox)
  private
  protected
  public
    class function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; override;
    class procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; 
      const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); override;
  end;

  { TGtkWSCheckBox }

  TGtkWSCheckBox = class(TWSCheckBox)
  private
  protected
  public
  end;

  { TGtkWSToggleBox }

  TGtkWSToggleBox = class(TWSToggleBox)
  private
  protected
  public
  end;

  { TGtkWSRadioButton }

  TGtkWSRadioButton = class(TWSRadioButton)
  private
  protected
  public
  end;

  { TGtkWSCustomStaticText }

  TGtkWSCustomStaticText = class(TWSCustomStaticText)
  private
  protected
  public
  end;

  { TGtkWSStaticText }

  TGtkWSStaticText = class(TWSStaticText)
  private
  protected
  public
  end;


implementation


{ helper routines }

function  WidgetGetSelStart(const Widget: PGtkWidget): integer;
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

{ TGtkWSCustomListBox }

function  TGtkWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
var
  Widget      : PGtkWidget;            // pointer to gtk-widget
  GList       : pGList;                // Only used for listboxes, replace with widget!!!!!
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  {$IFdef GTK1}
  case ACustomListBox.fCompStyle of
     csListBox, csCheckListBox:
       begin
         if Handle<>0 then begin
           Widget:=nil;
           if TListBox(ACustomListBox).MultiSelect then
             Widget:= PGtkList(GetWidgetInfo(Pointer(Handle), True)^.
                                  CoreWidget)^.last_focus_child;
           if Widget=nil then begin
             GList:= PGtkList(GetWidgetInfo(Pointer(Handle), True)^.CoreWidget)^.selection;
             if GList <> nil then
               Widget:= PGtkWidget(GList^.data);
           end;
           if Widget = nil then
             Result:= -1
           else
             Result:= gtk_list_child_position(PGtkList(
                           GetWidgetInfo(Pointer(Handle), True)^.
                                         CoreWidget), Widget);
         end else
           Result:=-1;
       end;

     csCListBox:
       begin
         GList:= PGtkCList(GetWidgetInfo(Pointer(Handle), True)^.CoreWidget)^.selection;
         if GList = nil then
           Result := -1
         else
           Result := integer(GList^.Data);
       end;
  end;
  {$EndIf}

  {$IFdef GTK2}
  DebugLn('TODO: TGtkWSCustomListBox.GetItemIndex');
  {$EndIf}
end;

function  TGtkWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
var
  Handle: HWND;
begin
  {$IFdef GTK2}
  DebugLn('TODO: TGtkWidgetSet.IntSendMessage3 LM_GETSELCOUNT');
  {$Else}
  Handle := ACustomListBox.Handle;
  case ACustomListBox.fCompStyle of
    csListBox, csCheckListBox :
      Result:=g_list_length(PGtkList(GetWidgetInfo(Pointer(Handle),
                         True)^.CoreWidget)^.selection);
    csCListBox:
      Result:= g_list_length(PGtkCList(GetWidgetInfo(Pointer(Handle),
                         True)^.CoreWidget)^.selection);
  end;
  {$EndIf}
end;

function  TGtkWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  Handle: HWND;
  Widget      : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  GList       : pGList;                // Only used for listboxes, replace with widget!!!!!
  ListItem    : PGtkListItem;          // currently only used for listboxes
begin
  {$IFdef GTK2}
  DebugLn('TODO: TGtkWidgetSet.IntSendMessage3 LM_GETSEL');
  {$Else}
  Result := false;      { assume: nothing found }
  Handle := ACustomListBox.Handle;
  case ACustomListBox.fCompStyle of
    csListBox, csCheckListBox:
      begin
        { Get the child in question of that index }
        Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
        ListItem:= g_list_nth_data(PGtkList(Widget)^.children, AIndex);
        if (ListItem<>nil)
        and (g_list_index(PGtkList(Widget)^.selection, ListItem)>=0)
        then Result:=true
      end;
    csCListBox:
      begin
        { Get the selections }
        Widget:=GetWidgetInfo(Pointer(Handle),True)^.CoreWidget;
        GList:= PGtkCList(Widget)^.selection;
        while Assigned(GList) do begin
          if integer(GList^.data) = AIndex then begin
            Result:=true;
            exit;
          end else
            GList := GList^.Next;
        end;
      end;
  end;
  {$EndIf}

end;

function  TGtkWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
var
  Widget      : PGtkWidget;            // pointer to gtk-widget
  Handle: HWND;
begin
  {$ifdef GTK2}
  DebugLn('TODO: TGtkWSCustomListBox.GetStrings');
  {$else}
  Handle := ACustomListBox.Handle;
  case ACustomListBox.fCompStyle of
    csCListBox:
      begin
        Widget:= GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;

        Result := TGtkCListStringList.Create(PGtkCList(Widget));
        if ACustomListBox is TCustomListBox then
          TGtkCListStringList(Result).Sorted := TCustomListBox(ACustomListBox).Sorted;
      end;

    csCheckListBox, csListBox:
      begin
        Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
        Result := TGtkListStringList.Create(PGtkList(Widget),
          ACustomListBox, ACustomListBox.fCompStyle = csCheckListBox);
        if ACustomListBox is TCustomListBox then
          TGtkListStringList(Result).Sorted := ACustomListBox.Sorted;
      end;
  else
    raise Exception.Create('TGtkWSCustomListBox.GetStrings');
  end;
  {$endif}
end;

procedure TGtkWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
var
  Widget      : PGtkWidget;            // pointer to gtk-widget (local use when neccessary)
  Handle: HWND;
begin
  {$IFdef GTK2}
  DebugLn('TODO: TGtkWidgetSet.IntSendMessage3 LM_SETSEL');
  {$Else}
  Handle := ACustomListBox.Handle;
  case ACustomListBox.fCompStyle of
    csListBox, csCheckListBox:
      begin
        Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
        if ASelected
        then gtk_list_select_item(PGtkList(Widget), AIndex)
        else gtk_list_unselect_item(PGtkList(Widget), AIndex);
      end;
    csCListBox:
      begin
        Widget := GetWidgetInfo(Pointer(Handle), True)^.CoreWidget;
        if ASelected
        then gtk_clist_select_row(PGtkCList(Widget), AIndex, 0)
        else gtk_clist_unselect_row(PGtkCList(Widget), AIndex, 0);
      end;
  end;
  {$Endif}
end;

procedure TGtkWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
var
  Handle: HWND;
  Widget      : PGtkWidget;            // pointer to gtk-widget
begin
  {$IFdef GTK2}
  DebugLn('TODO: TGtkWidgetSet.IntSendMessage3 LM_SETBORDER');
  {$Else}
  Handle := ACustomListBox.Handle;
  if (ACustomListBox.fCompStyle in [csListBox, csCheckListBox]) then
  begin
    { In TempWidget, a viewport is stored }
    Widget:= PGtkWidget(PGtkBin(Handle)^.child);
    if ACustomListBox.BorderStyle = TBorderStyle(bsSingle)
    then
      gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_IN)
    else
      gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_NONE);
  end else 
  if ACustomListBox.fCompStyle = csCListBox then
  begin
    if ACustomListBox.BorderStyle = TBorderStyle(bsSingle)
    then
      gtk_viewport_set_shadow_type(
        PGtkViewPort(PGtkBin(Handle)^.Child), GTK_SHADOW_NONE)
    else
      gtk_viewport_set_shadow_type(
        PGtkViewPort(PGtkBin(Handle)^.Child), GTK_SHADOW_IN);
  end;
  {$Endif}
end;

procedure TGtkWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  Handle: HWND;
begin
  Handle := ACustomListBox.Handle;
  if Handle<>0 then 
  begin
    {$IFdef GTK1}
    case ACustomListBox.fCompStyle of

    csListBox, csCheckListBox:
    begin
      if AIndex >= 0 then 
      begin
        gtk_list_select_item(
          PGtkList(GetWidgetInfo(Pointer(Handle),True)^.CoreWidget), AIndex)
      end else
        gtk_list_unselect_all(
          PGtkList(GetWidgetInfo(Pointer(Handle),True)^.CoreWidget));
    end;

    csCListBox:
      gtk_clist_select_row(PGtkCList(GetWidgetInfo(
        Pointer(Handle), True)^.CoreWidget), AIndex, 1);    // column
    end;
    {$EndIf}

    {$IFdef GTK2}
    DebugLn('TODO: TGtkWSCustomListBox.SetItemIndex');
    {$EndIf}
  end;
end;

procedure TGtkWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
  TGtkWidgetSet(InterfaceObject).SetSelectionMode(ACustomListBox, 
    PGtkWidget(ACustomListBox.Handle), AMultiSelect, AExtendedSelect);
end;

procedure TGtkWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
begin
  case ACustomListBox.fCompStyle of
  csListBox,
  csCheckListBox:
                 TGtkListStringList(AList).Sorted := ASorted;
  {$IfDef GTK1}
  csCListBox: TGtkCListStringList(AList).Sorted := ASorted;
  {$Else}
  else
     DebugLn('TODO: TGtkWidgetSet.IntSendMessage3 LM_SORT');
  {$Endif}
  end
end;

{ TGtkWSCustomComboBox }

function  TGtkWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result := WidgetGetSelStart(PGtkWidget(PGtkCombo(ACustomComboBox.Handle)^.entry));
end;

function  TGtkWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  with PGtkOldEditable(PGtkCombo(ACustomComboBox.Handle)^.entry)^ do begin
    Result:= Abs(integer(selection_end_pos)-integer(selection_start_pos));
  end;
end;

function  TGtkWSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
begin
  // TODO: ugly typecast to tcombobox
  Result:=GetComboBoxItemIndex(TComboBox(ACustomComboBox));
end;

function  TGtkWSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;
begin
  Result:= PGtkEntry(PGtkCombo(ACustomComboBox.Handle)^.entry)^.text_max_length;
end;

procedure TGtkWSCustomComboBox.SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox; 
  NewTraverseList: boolean);
var
  GtkCombo: GTK_COMBO;
begin
  GtkCombo := GTK_COMBO(ACustomComboBox.Handle);
  if ACustomComboBox.ArrowKeysTraverseList then
  begin
    gtk_combo_set_use_arrows(GtkCombo,GdkTrue);
  end else
  begin
    gtk_combo_set_use_arrows(GtkCombo,GdkFalse);
  end;
end;

procedure TGtkWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
begin
  gtk_editable_set_position(PGtkOldEditable(PGtkCombo(ACustomComboBox.Handle)^.entry), NewStart);
end;

procedure TGtkWSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
  WidgetSetSelLength(PGtkCombo(ACustomComboBox.Handle)^.entry, NewLength);
end;

procedure TGtkWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
begin
  // TODO: ugly typecast
  SetComboBoxItemIndex(TComboBox(ACustomComboBox), NewIndex);
end;

procedure TGtkWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
begin
  gtk_entry_set_max_length(PGtkEntry(PGtkCombo(ACustomComboBox.Handle)^.entry), NewLength);
end;

procedure TGtkWSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
var
  GtkCombo: GTK_COMBO;
begin
  GtkCombo := GTK_COMBO(ACustomComboBox.Handle);
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

function  TGtkWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
begin
  Result := TStrings(gtk_object_get_data(PGtkObject(ACustomComboBox.Handle), 'LCLList'));
end;

procedure TGtkWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);
begin
  TGtkListStringList(AList).Sorted := IsSorted;
end;

{ TGtkWSCustomEdit }

function  TGtkWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result := WidgetGetSelStart(GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.CoreWidget);
end;

function  TGtkWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  with PGtkOldEditable(GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.CoreWidget)^ do begin
    Result:=Abs(integer(selection_end_pos)-integer(selection_start_pos));
  end;
end;

procedure TGtkWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
  // TODO: implement me!
end;

procedure TGtkWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  // XXX TODO: GTK 1.x does not support EchoMode emNone.
  // This will have to be coded around, but not a priority
  SetPasswordChar(ACustomEdit, ACustomEdit.PasswordChar);
end;

procedure TGtkWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  gtk_entry_set_max_length(GTK_ENTRY(ACustomEdit.Handle), guint16(NewLength));
end;

procedure TGtkWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  gtk_entry_set_visibility(GTK_ENTRY(ACustomEdit.Handle), 
    (ACustomEdit.EchoMode = emNormal) and (NewChar = #0));
end;

procedure TGtkWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
  gtk_entry_set_editable(GTK_ENTRY(ACustomEdit.Handle), not ACustomEdit.ReadOnly);
end;

procedure TGtkWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  gtk_editable_set_position(PGtkOldEditable(GetWidgetInfo(
    Pointer(ACustomEdit.Handle), true)^.CoreWidget), NewStart);
end;

procedure TGtkWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  WidgetSetSelLength(GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.CoreWidget, NewLength);
end;

{ TGtkWSCustomLabel }

procedure TGtkWSCustomLabel.SetAlignment(const ACustomLabel: TCustomLabel; const NewAlignment: TAlignment);
const
  cLabelAlignX : array[TAlignment] of gfloat = (0.0, 1.0, 0.5);
  cLabelAlignY : array[TTextLayout] of gfloat = (0.0, 0.5, 1.0);
  cLabelAlign : array[TAlignment] of TGtkJustification = (GTK_JUSTIFY_LEFT, GTK_JUSTIFY_RIGHT, GTK_JUSTIFY_CENTER);
var
  wHandle: HWND;
begin
  wHandle := ACustomLabel.Handle;
  gtk_label_set_justify(GTK_LABEL(wHandle), cLabelAlign[NewAlignment]);
  gtk_misc_set_alignment(GTK_MISC(wHandle), cLabelAlignX[NewAlignment],
                        cLabelAlignY[ACustomLabel.Layout]);
end;

procedure TGtkWSCustomLabel.SetLayout(const ACustomLabel: TCustomLabel; const NewLayout: TTextLayout);
begin
  SetAlignment(ACustomLabel, ACustomLabel.Alignment);
end;

procedure TGtkWSCustomLabel.SetWordWrap(const ACustomLabel: TCustomLabel; const NewWordWrap: boolean);
begin
  gtk_label_set_line_wrap(GTK_LABEL(ACustomLabel.Handle), NewWordWrap);
end;

{ TGtkWSCustomCheckBox }

function  TGtkWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  if gtk_toggle_button_get_active (PGtkToggleButton(ACustomCheckBox.Handle))
  then Result := cbChecked
  else Result := cbUnChecked;
end;

procedure TGtkWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const OldShortCut, NewShortCut: TShortCut);
begin
  // ToDo: use accelerator group of Form
  Accelerate(ACustomCheckBox, PGtkWidget(ACustomCheckBox.Handle), NewShortcut, 'activate_item');
end;

{ TGtkWSCustomMemo }

procedure TGtkWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; AText: string);
var
  Widget: PGtkWidget;
  CurMemoLen: cardinal;
begin
  if Length(AText) = 0 then
    exit;

  Widget:=GetWidgetInfo(Pointer(ACustomMemo.Handle), true)^.CoreWidget;
  gtk_text_freeze(PGtkText(Widget));
  CurMemoLen := gtk_text_get_length(PGtkText(Widget));
  gtk_editable_insert_text(PGtkOldEditable(Widget), PChar(AText), Length(AText), @CurMemoLen);
  gtk_text_thaw(PGtkText(Widget));
end;

{$ifdef GTK1}

procedure TGtkWSCustomMemo.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
  // no password char in memo
end;

procedure TGtkWSCustomMemo.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
var
  ImplWidget   : PGtkWidget;
  i: integer;
begin
  if (ACustomEdit.MaxLength >= 0) then begin
    ImplWidget:= GetWidgetInfo(PGtkWidget(ACustomEdit.Handle), true)^.CoreWidget;
    i:= gtk_text_get_length(GTK_TEXT(ImplWidget));
    if i > ACustomEdit.MaxLength then begin
       gtk_editable_delete_text(PGtkOldEditable(ImplWidget),
                                ACustomEdit.MaxLength, i);
    end;
  end;
end;

procedure TGtkWSCustomMemo.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
  // no password char in memo
end;

procedure TGtkWSCustomMemo.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
var
  ImplWidget   : PGtkWidget;
begin
  ImplWidget:= GetWidgetInfo(PGtkWidget(ACustomEdit.Handle), true)^.CoreWidget;
  gtk_text_set_editable (GTK_TEXT(ImplWidget), not ACustomEdit.ReadOnly);
end;

procedure TGtkWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
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

procedure TGtkWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
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

{ TGtkWSCustomCheckBox }

procedure TGtkWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
var
  GtkObject: PGtkObject;
begin
  GtkObject := PGtkObject(ACustomCheckBox.Handle);
  LockOnChange(GtkObject,1);
  gtk_toggle_button_set_active(PGtkToggleButton(GtkObject), NewState = cbChecked);
  LockOnChange(GtkObject,-1);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollBar, TGtkWSScrollBar);
//  RegisterWSComponent(TCustomGroupBox, TGtkWSCustomGroupBox);
//  RegisterWSComponent(TGroupBox, TGtkWSGroupBox);
  RegisterWSComponent(TCustomComboBox, TGtkWSCustomComboBox);
//  RegisterWSComponent(TComboBox, TGtkWSComboBox);
  RegisterWSComponent(TCustomListBox, TGtkWSCustomListBox);
//  RegisterWSComponent(TListBox, TGtkWSListBox);
  RegisterWSComponent(TCustomEdit, TGtkWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TGtkWSCustomMemo);
//  RegisterWSComponent(TEdit, TGtkWSEdit);
//  RegisterWSComponent(TMemo, TGtkWSMemo);
  RegisterWSComponent(TCustomLabel, TGtkWSCustomLabel);
//  RegisterWSComponent(TLabel, TGtkWSLabel);
//  RegisterWSComponent(TButtonControl, TGtkWSButtonControl);
  RegisterWSComponent(TCustomCheckBox, TGtkWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TToggleBox, TGtkWSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtkWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtkWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtkWSStaticText);
////////////////////////////////////////////////////
end.
