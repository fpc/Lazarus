{ $Id$}
{
 *****************************************************************************
 *                               GtkWSSpin.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit GtkWSSpin;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk,
  {$ENDIF}
  GtkInt,
  LCLProc, Spin, StdCtrls, GtkProc, gtkExtra, GtkWSStdCtrls, WSSpin,
  WSLCLClasses, Controls, LCLType;

type

  { TGtkWSCustomFloatSpinEdit }

  TGtkWSCustomFloatSpinEdit = class(TWSCustomFloatSpinEdit)
  private
  protected
  public
    class function  GetSelStart(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetSelLength(const ACustomEdit: TCustomEdit): integer; override;
    class function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single; override;

    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); override;
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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

{ TGtkWSCustomFloatSpinEdit }

class function TGtkWSCustomFloatSpinEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  Result :=WidgetGetSelStart(PGtkWidget(GetSpinGtkEntry(ACustomEdit)));
end;

class function TGtkWSCustomFloatSpinEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  with GetSpinGtkEditable(ACustomEdit)^ do
    Result := Abs(integer(selection_end_pos)-integer(selection_start_pos));
end;

class function TGtkWSCustomFloatSpinEdit.GetValue(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit): single;
begin
  Result:=gtk_spin_button_get_value_as_float(
                                   PGtkSpinButton(ACustomFloatSpinEdit.Handle));
end;

class procedure TGtkWSCustomFloatSpinEdit.SetSelStart(const ACustomEdit: TCustomEdit;
  NewStart: integer);
begin
  gtk_editable_set_position(GetSpinGtkEditable(ACustomEdit), NewStart);
end;

class procedure TGtkWSCustomFloatSpinEdit.SetSelLength(const ACustomEdit: TCustomEdit;
  NewLength: integer);
begin
  WidgetSetSelLength(PGtkWidget(GetSpinGtkEntry(ACustomEdit)),
                     NewLength);
end;

class procedure TGtkWSCustomFloatSpinEdit.UpdateControl(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  AnAdjustment: PGtkAdjustment;
  wHandle: HWND;
  SpinWidget: PGtkSpinButton;
begin
  //DebugLn(['TGtkWSCustomFloatSpinEdit.UpdateControl ',dbgsName(ACustomFloatSpinEdit)]);
  wHandle := ACustomFloatSpinEdit.Handle;
  SpinWidget:=GTK_SPIN_BUTTON(Pointer(wHandle));
  AnAdjustment:=gtk_spin_button_get_adjustment(SpinWidget);
  if (AnAdjustment^.lower<>ACustomFloatSpinEdit.MinValue)
  or (AnAdjustment^.upper<>ACustomFloatSpinEdit.MaxValue) then
  begin
    AnAdjustment^.lower:=ACustomFloatSpinEdit.MinValue;
    AnAdjustment^.upper:=ACustomFloatSpinEdit.MaxValue;
    gtk_adjustment_changed(AnAdjustment);
  end;
  gtk_spin_button_set_digits(SpinWidget, ACustomFloatSpinEdit.DecimalPlaces);
  gtk_spin_button_set_value(SpinWidget,ACustomFloatSpinEdit.Value);
  AnAdjustment^.step_increment := ACustomFloatSpinEdit.Increment;
end;

class function TGtkWSCustomFloatSpinEdit.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  p: PGtkWidget;
begin
  p := gtk_spin_button_new(PgtkAdjustment(
                                        gtk_adjustment_new(1,1,100,1,1,1)),1,0);
  gtk_widget_show_all(p);
  gtkWidgetSet.FinishComponentCreate(AWinControl, P);
  Result := THandle(P);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomFloatSpinEdit, TGtkWSCustomFloatSpinEdit);
//  RegisterWSComponent(TFloatSpinEdit, TGtkWSFloatSpinEdit);
////////////////////////////////////////////////////
end.
