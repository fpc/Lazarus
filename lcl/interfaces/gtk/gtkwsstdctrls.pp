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
  StdCtrls, SysUtils,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  WSStdCtrls, WSLCLClasses, GtkInt, Classes, LCLType,
  GTKWinApiWindow, gtkglobals, gtkproc;


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

    class procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); override;
    class procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;
    class procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); override;
    class procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); override;

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
    class function  GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
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

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
  end;

  { TGtkWSCustomMemo }

  TGtkWSCustomMemo = class(TWSCustomMemo)
  private
  protected
  public
    class procedure AppendText(const ACustomMemo: TCustomMemo; AText: string); override;
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

procedure TGtkWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
  gtk_editable_set_position(PGtkOldEditable(GetWidgetInfo(
    Pointer(ACustomEdit.Handle), true)^.CoreWidget), NewStart);
end;

procedure TGtkWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
  WidgetSetSelLength(GetWidgetInfo(Pointer(ACustomEdit.Handle), true)^.CoreWidget, NewLength);
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
//  RegisterWSComponent(TCustomListBox, TGtkWSCustomListBox);
//  RegisterWSComponent(TListBox, TGtkWSListBox);
  RegisterWSComponent(TCustomEdit, TGtkWSCustomEdit);
  RegisterWSComponent(TCustomMemo, TGtkWSCustomMemo);
//  RegisterWSComponent(TEdit, TGtkWSEdit);
//  RegisterWSComponent(TMemo, TGtkWSMemo);
//  RegisterWSComponent(TCustomLabel, TGtkWSCustomLabel);
//  RegisterWSComponent(TLabel, TGtkWSLabel);
//  RegisterWSComponent(TButtonControl, TGtkWSButtonControl);
//  RegisterWSComponent(TCustomCheckBox, TGtkWSCustomCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TCheckBox, TGtkWSCheckBox);
//  RegisterWSComponent(TToggleBox, TGtkWSToggleBox);
//  RegisterWSComponent(TRadioButton, TGtkWSRadioButton);
//  RegisterWSComponent(TCustomStaticText, TGtkWSCustomStaticText);
//  RegisterWSComponent(TStaticText, TGtkWSStaticText);
////////////////////////////////////////////////////
end.
