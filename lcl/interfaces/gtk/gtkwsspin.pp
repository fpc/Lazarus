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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Spin, GtkProc, GtkWSStdCtrls, WSSpin, WSLCLClasses, LCLType;

type

  { TGtkWSCustomSpinEdit }

  TGtkWSCustomSpinEdit = class(TWSCustomSpinEdit)
  private
  protected
  public
    class function  GetSelStart(const ACustomSpinEdit: TCustomSpinEdit): integer; override;
    class function  GetSelLength(const ACustomSpinEdit: TCustomSpinEdit): integer; override;
    class function  GetValue(const ACustomSpinEdit: TCustomSpinEdit): single; override;

    class procedure SetSelStart(const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer); override;

    class procedure UpdateControl(const ACustomSpinEdit: TCustomSpinEdit); override;
  end;

function GetGtkSpinEntry(Spin: PGtkSpinButton): PGtkEntry;
function GetSpinGtkEntry(Spin: TCustomSpinEdit): PGtkEntry;
function GetGtkSpinEditable(Spin: PGtkSpinButton): PGtkOldEditable;
function GetSpinGtkEditable(Spin: TCustomSpinEdit): PGtkOldEditable;

implementation

function GetGtkSpinEntry(Spin: PGtkSpinButton): PGtkEntry;
begin
  Result:=PGtkEntry(@(Spin^.entry));
end;

function GetSpinGtkEntry(Spin: TCustomSpinEdit): PGtkEntry;
begin
  Result:=GetGtkSpinEntry(PGtkSpinButton(Spin.Handle));
end;

function GetGtkSpinEditable(Spin: PGtkSpinButton): PGtkOldEditable;
begin
  Result:=PGtkOldEditable(@(Spin^.entry));
end;

function GetSpinGtkEditable(Spin: TCustomSpinEdit): PGtkOldEditable;
begin
  Result:=GetGtkSpinEditable(PGtkSpinButton(Spin.Handle));
end;

{ TGtkWSCustomSpinEdit }

//const
//  GtkValueEmpty: array[boolean] of integer = (0,1);

function TGtkWSCustomSpinEdit.GetSelStart(const ACustomSpinEdit: TCustomSpinEdit
  ): integer;
begin
  Result := WidgetGetSelStart(PGtkWidget(GetSpinGtkEntry(ACustomSpinEdit)));
end;

function TGtkWSCustomSpinEdit.GetSelLength(
  const ACustomSpinEdit: TCustomSpinEdit): integer;
begin
  with GetSpinGtkEditable(ACustomSpinEdit)^ do
    Result := Abs(integer(selection_end_pos)-integer(selection_start_pos));
end;

function TGtkWSCustomSpinEdit.GetValue(const ACustomSpinEdit: TCustomSpinEdit
  ): single;
begin
  Result:=gtk_spin_button_get_value_as_float(
                                        PGtkSpinButton(ACustomSpinEdit.Handle));
end;

procedure TGtkWSCustomSpinEdit.SetSelStart(
  const ACustomSpinEdit: TCustomSpinEdit; NewStart: integer);
begin
  gtk_editable_set_position(GetSpinGtkEditable(ACustomSpinEdit), NewStart);
end;

procedure TGtkWSCustomSpinEdit.SetSelLength(
  const ACustomSpinEdit: TCustomSpinEdit; NewLength: integer);
begin
  WidgetSetSelLength(PGtkWidget(GetSpinGtkEntry(ACustomSpinEdit)),NewLength);
end;

procedure TGtkWSCustomSpinEdit.UpdateControl(
  const ACustomSpinEdit: TCustomSpinEdit);
var
  AnAdjustment: PGtkAdjustment;
  wHandle: HWND;
  SpinWidget: PGtkSpinButton;
begin
  wHandle := ACustomSpinEdit.Handle;
  SpinWidget:=GTK_SPIN_BUTTON(Pointer(wHandle));
  AnAdjustment:=gtk_spin_button_get_adjustment(SpinWidget);
  if (AnAdjustment^.lower<>ACustomSpinEdit.MinValue)
  or (AnAdjustment^.upper<>ACustomSpinEdit.MaxValue) then
  begin
    AnAdjustment^.lower:=ACustomSpinEdit.MinValue;
    AnAdjustment^.upper:=ACustomSpinEdit.MaxValue;
    gtk_adjustment_changed(AnAdjustment);
  end;
  gtk_spin_button_set_digits(SpinWidget, ACustomSpinEdit.Decimal_Places);
  gtk_spin_button_set_value(SpinWidget,ACustomSpinEdit.Value);
  AnAdjustment^.step_increment := ACustomSpinEdit.Climb_Rate;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomSpinEdit, TGtkWSCustomSpinEdit);
//  RegisterWSComponent(TSpinEdit, TGtkWSSpinEdit);
////////////////////////////////////////////////////
end.
