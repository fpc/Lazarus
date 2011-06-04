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
  Math, Controls, LCLType, LCLProc, Spin, StdCtrls,
  // Widgetset
  Gtk2Extra, Gtk2Def, Gtk2Int, Gtk2WSControls, Gtk2WSStdCtrls,
  Gtk2Proc, WSLCLClasses, WSSpin;

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

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

function GetGtkSpinEntry(Spin: PGtkSpinButton): PGtkEntry;
function GetSpinGtkEntry(const Spin: TWinControl): PGtkEntry;
function GetGtkFloatSpinEditable(Spin: PGtkSpinButton): PGtkOldEditable;
function GetSpinGtkEditable(const Spin: TWinControl): PGtkOldEditable;

implementation

function GetGtkSpinEntry(Spin: PGtkSpinButton): PGtkEntry;
begin
  Result:=PGtkEntry(@(Spin^.entry));
end;

function GetSpinGtkEntry(const Spin: TWinControl): PGtkEntry;
begin
  Result:=GetGtkSpinEntry(PGtkSpinButton(Spin.Handle));
end;

function GetGtkFloatSpinEditable(Spin: PGtkSpinButton): PGtkOldEditable;
begin
  Result:=PGtkOldEditable(@(Spin^.entry));
end;

function GetSpinGtkEditable(const Spin: TWinControl): PGtkOldEditable;
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
begin
  Result :=WidgetGetSelStart(PGtkWidget(GetSpinGtkEntry(ACustomEdit)));
end;

class function TGtk2WSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  with GetSpinGtkEditable(ACustomEdit)^ do
    Result := Abs(integer(selection_end_pos)-integer(selection_start_pos));
end;

class function TGtk2WSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
begin
  // developer.gnome.org/doc/API/2.2/gtk/GtkSpinButton.html:
  // "function gtk_spin_button_get_value_as_float is deprecated, use gtk_spin_button_get_value() instead"
  Result:=gtk_spin_button_get_value(PGtkSpinButton(ACustomFloatSpinEdit.Handle));
end;

class procedure TGtk2WSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  gtk_editable_set_position(GetSpinGtkEditable(ACustomEdit), NewStart);
end;

class procedure TGtk2WSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  WidgetSetSelLength(PGtkWidget(GetSpinGtkEntry(ACustomEdit)),
                     NewLength);
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
end;

class function TGtk2WSCustomFloatSpinEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Adjustment: PGtkAdjustment;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Adjustment := PGtkAdjustment(gtk_adjustment_new(1, 1, 100, 1, 0,0));
  Widget := gtk_spin_button_new(Adjustment, 1, 0);
  gtk_widget_show_all(Widget);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(Widget));

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  WidgetInfo^.DefaultCursor:=0;
  Set_RC_Name(AWinControl, Widget);
  SetCallbacks(Widget, WidgetInfo);
end;

end.
