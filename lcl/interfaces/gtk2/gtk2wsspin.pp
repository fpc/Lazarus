{ $Id$}
{
 *****************************************************************************
 *                               Gtk2WSSpin.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSSpin;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  // RTL, FCL, LCL
  SysUtils, Math, Controls, LCLType, LCLProc, Spin, StdCtrls,
  // Widgetset
  Gtk2Extra, Gtk2Def, Gtk2Int, Gtk2WSControls, Gtk2WSStdCtrls,
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
  Result:=GetGtkFloatSpinEditable(PGtkSpinButton(Spin.Handle));
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
  Entry := @PGtkSpinButton(ACustomEdit.Handle)^.entry;
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
begin
  if not WSCheckHandleAllocated(ACustomFloatSpinEdit, 'GetValue') then
    Exit(0);
  Result := gtk_spin_button_get_value(PGtkSpinButton(ACustomFloatSpinEdit.Handle));
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
  Entry := @PGtkSpinButton(ACustomEdit.Handle)^.entry;
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
  Widget := PGtkWidget(ACustomEdit.Handle);
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
  SpinWidget:=GTK_SPIN_BUTTON(Pointer(wHandle));

  if ACustomFloatSpinEdit.MaxValue >= ACustomFloatSpinEdit.MinValue then
  begin
    AMin := ACustomFloatSpinEdit.MinValue;
    AMax := ACustomFloatSpinEdit.MaxValue;
  end
  else
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

function HandleFloatSpinEditKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer): Boolean;
var
  Entry: PGtkEntry;
  AChar: Array [0..0] of Char;
  ACurPos: Integer;
  ASelLen: Integer;
  FL: Double;
  S: String;
  SSel: String;
  P: PChar;
  SpinButton: PGtkSpinButton;
begin
  Result := False;
  Entry := GTK_ENTRY(Widget);
  if (ABS(Entry^.current_pos - Entry^.selection_bound) >= 0) and
    (Event^.keyval > 31) and (Event^.keyval < 256) then
  begin
    SpinButton := GTK_SPIN_BUTTON(Entry);
    FL := gtk_spin_button_get_value(SpinButton);
    ACurPos := Min(Entry^.current_pos, Entry^.selection_bound);
    ASelLen := ABS(Entry^.current_pos - Entry^.selection_bound);
    S := StrPas(gtk_entry_get_text(Entry));
    // writeln(Format('Value %12.2n text %s start %d len %d',
    //  [FL, StrPas(gtk_entry_get_text(Entry)), ACurPos, ASelLen]));

    if ASelLen > 0 then
    begin
      if ASelLen = length(S) then
      begin
        gtk_spin_button_set_value(SpinButton, 0);
        gtk_editable_set_position(Entry, 1);
      end else
      begin
        SSel := Copy(S, ACurPos + 1, ASelLen);
        if Pos(DecimalSeparator, SSel) > 0 then
          SSel := Copy(S,ACurPos + 1, ASelLen + 1);

        Delete(S, ACurPos + 1, ASelLen);
        Insert(Chr(Event^.keyVal), S, ACurPos + 1);

        // if clocale isn't included in our project we are in trouble.
        S := StringReplace(S,',',DecimalSeparator,[rfReplaceAll]);
        TryStrToFloat(S, FL);

        g_object_set_data(PGObject(SpinButton),'lcl-do-not-change-selection', Data);

        gtk_entry_set_text(Entry, PChar(S));
        gtk_editable_set_position(Entry, ACurPos + 1);

        // do not trigger OnChange in keyrelease
        LockOnChange(PGtkObject(Widget), 1);
        S := FloatToStr(FL);
        P := StrNew(PChar(S));
        g_object_set_data(PGObject(SpinButton),'lcl-eat-next-key-release', P);
        g_object_set_data(PGObject(SpinButton),'lcl-eat-next-key-release-pos', gpointer(ACurPos));
        Result := True;
      end;
    end;
  end;
end;

function GTKEntryKeyPress(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer): GBoolean; cdecl;
begin
  Result := HandleFloatSpinEditKeyPress(Widget, Event, Data);
end;

function HandleFloatSpinEditKeyRelease(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer): GBoolean;
var
  AData: PChar;
  FL: Double;
  APos: Integer;
begin
  Result := False;
  if g_object_get_data(PGObject(Widget),'lcl-eat-next-key-release') <> nil then
  begin
    AData := g_object_get_data(PGObject(Widget),'lcl-eat-next-key-release');
    FL := StrToFloat(StrPas(AData));
    if g_object_get_data(PGObject(Widget),'lcl-eat-next-key-release-pos') <> nil then
      APos := PtrInt(g_object_get_data(PGObject(Widget),'lcl-eat-next-key-release-pos'))
    else
      APos := 0;
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(Widget), FL);
    if APos >= 0 then
      gtk_editable_set_position(GTK_EDITABLE(Widget), APos + 1);
    StrDispose(AData);
    g_object_set_data(PGObject(Widget),'lcl-eat-next-key-release', nil);
    Result := True;
    LockOnChange(PGtkObject(Widget), -1);
  end;
end;

function GTKEntryKeyRelease(Widget: PGtkWidget; Event: PGdkEventKey; Data: gPointer): GBoolean; cdecl;
begin
  Result := HandleFloatSpinEditKeyRelease(Widget, Event, Data);
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
  Result := TLCLIntfHandle(PtrUInt(Widget));

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
  if Result <> 0 then
  begin
    Entry := GTK_ENTRY(Widget);
    // PGtkEntry(@PGtkSpinButton(Result)^.entry);
    g_object_set(gtk_widget_get_settings(PGtkWidget(Entry)),
      'gtk-entry-select-on-focus', [0, nil]);

    // issue #18679 , affected only gtk2 >= 2.18.
    if (gtk_major_version = 2) and (gtk_minor_version >= 18) then
    begin
      g_signal_connect(Entry, 'key-press-event',
        TGCallback(@GTKEntryKeyPress), WidgetInfo);
      g_signal_connect(Entry, 'key-release-event',
        TGCallback(@GTKEntryKeyRelease), WidgetInfo);
    end;
  end;
end;

end.
