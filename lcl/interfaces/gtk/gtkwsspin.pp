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
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  Spin, WSSpin, WSLCLClasses, LCLType;

type

  { TGtkWSCustomSpinEdit }

  TGtkWSCustomSpinEdit = class(TWSCustomSpinEdit)
  private
  protected
  public
    class procedure UpdateControl(const ACustomSpinEdit: TCustomSpinEdit); override;
  end;

  { TGtkWSSpinEdit }

  TGtkWSSpinEdit = class(TWSSpinEdit)
  private
  protected
  public
  end;


implementation

{ TGtkWSCustomSpinEdit }

procedure TGtkWSCustomSpinEdit.UpdateControl(const ACustomSpinEdit: TCustomSpinEdit);
var
  AnAdjustment: PGtkAdjustment;
  wHandle: HWND;
begin
  wHandle := ACustomSpinEdit.Handle;
  AnAdjustment:=gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(wHandle));
  if (AnAdjustment^.lower<>ACustomSpinEdit.MinValue)
  or (AnAdjustment^.upper<>ACustomSpinEdit.MaxValue) then
  begin
    AnAdjustment^.lower:=ACustomSpinEdit.MinValue;
    AnAdjustment^.upper:=ACustomSpinEdit.MaxValue;
    gtk_adjustment_changed(AnAdjustment);
  end;
  gtk_spin_button_set_digits(GTK_SPIN_BUTTON(wHandle),
                             ACustomSpinEdit.Decimal_Places);
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(wHandle),
                            ACustomSpinEdit.Value);
  GTK_SPIN_BUTTON(wHandle)^.climb_rate:=ACustomSpinEdit.Climb_Rate;
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
