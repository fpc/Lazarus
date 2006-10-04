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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  GtkProc, GtkDef, Calendar, WSCalendar, WSLCLClasses;

type

  { TGtkWSCustomCalendar }

  TGtkWSCustomCalendar = class(TWSCustomCalendar)
  private
  protected
  public
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; 
      const ADisplaySettings: TDisplaySettings); override;
    class procedure SetReadOnly(const ACalendar: TCustomCalendar; const AReadOnly: boolean); override;
  end;

function GetGtkCalendar(const ACalendar: TCustomCalendar): PGtkCalendar;

implementation

function GetGtkCalendar(const ACalendar: TCustomCalendar): PGtkCalendar;
var
  WinWidgetInfo: PWidgetInfo;
begin
  Result:=nil;
  if (ACalendar=nil) or (not ACalendar.HandleAllocated) then exit;
  WinWidgetInfo:=GetWidgetInfo(PGtkWidget(ACalendar.Handle), False);
  if WinWidgetInfo=nil then exit;
  Result:=PGtkCalendar(WinWidgetInfo^.CoreWidget);
end;

class function  TGtkWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  Year, Month, Day: guint;  //used for csCalendar
begin
  gtk_calendar_get_date(GetGtkCalendar(ACalendar), @Year, @Month, @Day);
  //For some reason, the month is zero based.
  Result := EncodeDate(Year,Month+1,Day);
end;

class procedure TGtkWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  Year, Month, Day: string;
  GtkCalendar: PGtkCalendar;
begin
  GtkCalendar := GetGtkCalendar(ACalendar);
  Year := FormatDateTime('yyyy', ADateTime);
  Month := FormatDateTime('mm', ADateTime);
  Day := FormatDateTime('dd', ADateTime);
  gtk_calendar_select_month(GtkCalendar,StrtoInt(Month)-1,StrToInt(Year));
  gtk_calendar_select_day(GtkCalendar,StrToInt(Day));
end;

class procedure TGtkWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
var
  num: dword;
  gtkcalendardisplayoptions : TGtkCalendarDisplayOptions;
begin
  num := 0;
  if (dsShowHeadings in ADisplaySettings) then
    num := Num + (1 shl 0);

  if (dsShowDayNames in ADisplaySettings) then
    num := Num  + (1 shl 1);

  if (dsNoMonthChange in ADisplaySettings) then
    num := Num  + (1 shl 2);

  if (dsShowWeekNumbers in ADisplaySettings) then
     num := Num  + (1 shl 3);

  if (dsStartMonday in ADisplaySettings) then
     num := Num  + (1 shl 4);

  gtkCalendarDisplayOptions := TGtkCalendarDisplayOptions(num);
  gtk_Calendar_Display_options(GetGtkCalendar(ACalendar), gtkCalendarDisplayOptions);
end;

class procedure TGtkWSCustomCalendar.SetReadOnly(const ACalendar: TCustomCalendar;
  const AReadOnly: boolean);
var
  GtkCalendar: PGtkCalendar;
begin
  GtkCalendar := GetGtkCalendar(ACalendar);
  if AReadOnly then
    gtk_calendar_freeze(GtkCalendar)
  else
    gtk_calendar_thaw(GtkCalendar);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCalendar, TGtkWSCustomCalendar);
////////////////////////////////////////////////////
end.
