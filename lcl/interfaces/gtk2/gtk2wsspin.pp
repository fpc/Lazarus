{ $Id$}
{
 *****************************************************************************
 *                               Gtk2WSSpin.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSSpin;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  glib2, gtk2,
  // RTL, FCL, LCL
  SysUtils, Math, Controls, LCLType, LCLProc, Spin, StdCtrls,
  // Widgetset
  Gtk2Extra, Gtk2Def, Gtk2WSStdCtrls,
  Gtk2Proc, WSLCLClasses, WSProc, WSSpin;

type

  { TGtk2WSCustomFloatSpinEdit }

  TGtk2WSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; override;

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    class procedure SetReadOnly(const ACustomEdit: TCustomEdit; ReadOnly: boolean); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

implementation

function GetGtkFloatSpinEditable(Spin: PGtkSpinButton): PGtkEntry;
begin
  Result:=PGtkEntry(@(Spin^.entry));
end;

function GetSpinGtkEditable(const Spin: TWinControl): PGtkEntry;
begin
  Result:=GetGtkFloatSpinEditable({%H-}PGtkSpinButton(Spin.Handle));
end;

{ TGtk2WSCustomFloatSpinEdit }

class procedure TGtk2WSCustomFloatSpinEdit.SetCallbacks(
  const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSCustomEdit.SetCallbacks(AWidget, AWidgetInfo);
end;

class function TGtk2WSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
var
  Entry: PGtkEntry;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelStart') then
    Exit(0);
  Entry := @{%H-}PGtkSpinButton(ACustomEdit.Handle)^.entry;
  Result := Min(Entry^.current_pos, Entry^.selection_bound)
end;

class function TGtk2WSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
var
  AStart, AEnd: gint;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACustomEdit, 'GetSelLength') then
    Exit;
  if gtk_editable_get_selection_bounds(PGtkEditable(GetSpinGtkEditable(ACustomEdit)), @AStart, @AEnd) then
    Result := Abs(AEnd-AStart);
end;

class function TGtk2WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
var
  S: String;
  FL: Double;
begin
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'GetValue') then
    Exit(0);

  Result := gtk_spin_button_get_value({%H-}PGtkSpinButton(ACustomFloatSpinEdit.Handle));

  // gtk2 have different meaning of value vs text in GtkSpinBox when
  // we are dealing with real FloatSpinEdit. #18679.
  // We need this because of validator in gtk2callback.inc -> gtkchanged_spinbox()
  if ACustomFloatSpinEdit.DecimalPlaces > 0  then
  begin
    S := StrPas(gtk_entry_get_text({%H-}PGtkEntry(ACustomFloatSpinEdit.Handle)));
    FL := 0;
    if TryStrToFloat(S, FL) then
      Result := FL;
  end;
end;

class procedure TGtk2WSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelStart') then
    Exit;
  gtk_editable_set_position(GetSpinGtkEditable(ACustomEdit), NewStart);
end;

class procedure TGtk2WSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
var
  Entry: PGtkEntry;
  SelStart: Integer;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetSelLength') then
    Exit;
  Entry := @{%H-}PGtkSpinButton(ACustomEdit.Handle)^.entry;
  SelStart := GetSelStart(ACustomEdit);
  gtk_entry_select_region(Entry,
    SelStart,
    SelStart + NewLength);
end;

class procedure TGtk2WSCustomFloatSpinEdit.SetReadOnly(const ACustomEdit: TCustomEdit; ReadOnly: boolean);
var
  Widget: PGtkWidget;
  AnAdjustment: PGtkAdjustment;
begin
  if not WSCheckHandleAllocated(ACustomEdit, 'SetReadOnly') then
    Exit;
  Widget := {%H-}PGtkWidget(ACustomEdit.Handle);
  if GTK_IS_EDITABLE(Widget) then
    gtk_editable_set_editable(PGtkEditable(Widget), not ReadOnly);

  AnAdjustment:=gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(Widget));
  if ReadOnly then
  begin
    AnAdjustment^.lower := TCustomFloatSpinEdit(ACustomEdit).Value;
    AnAdjustment^.upper := TCustomFloatSpinEdit(ACustomEdit).Value;
  end
  else
  begin
    AnAdjustment^.lower := TCustomFloatSpinEdit(ACustomEdit).MinValue;
    AnAdjustment^.upper := TCustomFloatSpinEdit(ACustomEdit).MaxValue;
  end;
  gtk_spin_button_update(GTK_SPIN_BUTTON(Widget));
end;

class procedure TGtk2WSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  AnAdjustment: PGtkAdjustment;
  wHandle: HWND;
  SpinWidget: PGtkSpinButton;
  AMin, AMax: Double;
begin
  //DebugLn(['TGtkWSCustomFloatSpinEdit.UpdateControl ',dbgsName(ACustomFloatSpinEdit)]);
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'UpdateControl') then
    Exit;
  wHandle := ACustomFloatSpinEdit.Handle;
  SpinWidget:=GTK_SPIN_BUTTON({%H-}Pointer(wHandle));

  if ACustomFloatSpinEdit.MaxValue >= ACustomFloatSpinEdit.MinValue then
  begin
    AMin := ACustomFloatSpinEdit.MinValue;
    AMax := ACustomFloatSpinEdit.MaxValue;
  end else
  begin
    AMin := -MaxDouble;
    AMax := MaxDouble;
  end;

  AnAdjustment:=gtk_spin_button_get_adjustment(SpinWidget);
  if (AnAdjustment^.lower <> AMin)
  or (AnAdjustment^.upper <> AMax) then
  begin
    AnAdjustment^.lower := AMin;
    AnAdjustment^.upper := AMax;
    gtk_adjustment_changed(AnAdjustment);
  end;

  gtk_spin_button_set_digits(SpinWidget, ACustomFloatSpinEdit.DecimalPlaces);
  gtk_spin_button_set_value(SpinWidget,ACustomFloatSpinEdit.Value);
  AnAdjustment^.step_increment := ACustomFloatSpinEdit.Increment;

  SetReadOnly(TCustomEdit(ACustomFloatSpinEdit), ACustomFloatSpinEdit.ReadOnly);
end;

class function TGtk2WSCustomFloatSpinEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Entry: PGtkEntry;
begin
  Adjustment := PGtkAdjustment(gtk_adjustment_new(1, 1, 100, 1, 0,0));
  Widget := gtk_spin_button_new(Adjustment, 1, 0);
  gtk_widget_show_all(Widget);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle({%H-}PtrUInt(Widget));

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
  if Result <> 0 then
  begin
    Entry := GTK_ENTRY(Widget);
    // PGtkEntry(@PGtkSpinButton(Result)^.entry);
    g_object_set(gtk_widget_get_settings(PGtkWidget(Entry)),
      'gtk-entry-select-on-focus', [0, nil]);
  end;
end;

end.
