{ $Id$}
{
 *****************************************************************************
 *                             GtkWSCalendar.pp                              * 
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
unit GtkWSCalendar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  Calendar, WSCalendar, WSLCLClasses;

type

  { TGtkWSCalendar }

  TGtkWSCalendar = class(TWSCalendar)
  private
  protected
  public
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
  end;


implementation

function  TGtkWSCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  Year, Month, Day: word;  //used for csCalendar
begin
  gtk_calendar_get_date(PGtkCalendar(ACalendar.Handle), @Year, @Month, @Day);
  //For some reason, the month is zero based.
  Result := EncodeDate(Year,Month+1,Day);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCalendar, TGtkWSCalendar);
////////////////////////////////////////////////////
end.
