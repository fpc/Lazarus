{ $Id$}
{
 *****************************************************************************
 *                              QtWSCalendar.pp                              * 
 *                              ---------------                              * 
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
unit QtWSCalendar;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt4,
  qtwidgets,
  // LCL
  SysUtils, DateUtils, Controls, Calendar, LCLType, LCLIntf, LCLProc,
  // Widgetset
  WSCalendar, WSLCLClasses;

type

  { TQtWSCustomCalendar }

  TQtWSCustomCalendar = class(TWSCustomCalendar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  public
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ADisplaySettings: TDisplaySettings); override;
    class procedure SetReadOnly(const ACalendar: TCustomCalendar; const AReadOnly: boolean); override;
  end;


implementation

{ TQtWSCustomCalendar }

class function TQtWSCustomCalendar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar.Create(AWinControl, AParams);

  QtCalendar.AttachEvents;

  Result := TLCLIntfHandle(QtCalendar);
end;

class procedure TQtWSCustomCalendar.SetReadOnly(const ACalendar: TCustomCalendar; const AReadOnly: boolean);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);

  if AReadOnly then
   QCalendarWidget_setSelectionMode(QCalendarWidgetH(QtCalendar.Widget), QCalendarWidgetNoSelection)
  else QCalendarWidget_setSelectionMode(QCalendarWidgetH(QtCalendar.Widget), QCalendarWidgetSingleSelection);

  QCalendarWidget_setDateEditEnabled(QCalendarWidgetH(QtCalendar.Widget), not AReadOnly);
end;

class function TQtWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  QtCalendar: TQtCalendar;
  ADate: QDateH;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  ADate := QDate_create;

  try
    QCalendarWidget_selectedDate(QCalendarWidgetH(QtCalendar.Widget), ADate);
    QtCalendar.AYear := QDate_year(ADate);
    QtCalendar.AMonth := QDate_month(ADate);
    QtCalendar.ADay := QDate_day(ADate);
    Result := EncodeDate(QtCalendar.AYear, QtCalendar.AMonth, QtCalendar.ADay);
  finally
    QDate_destroy(ADate);
  end;
end;

class procedure TQtWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  QtCalendar: TQtCalendar;
  ADate: QDateH;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  DecodeDate(ADateTime, QtCalendar.AYear, QtCalendar.AMonth, QtCalendar.ADay);
  ADate := QDate_create(QtCalendar.AYear, QtCalendar.AMonth, QtCalendar.ADay);

  try
    QCalendarWidget_setCurrentPage(QCalendarWidgetH(QtCalendar.Widget), QtCalendar.AYear, QtCalendar.AMonth);
    QCalendarWidget_setSelectedDate(QCalendarWidgetH(QtCalendar.Widget), ADate);
  finally
    QDate_destroy(ADate);
  end;
end;

class procedure TQtWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
 const ADisplaySettings: TDisplaySettings);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);

  QCalendarWidget_setHeaderVisible(QCalendarWidgetH(QtCalendar.Widget), dsShowHeadings in ADisplaySettings);

  if dsShowDayNames in ADisplaySettings then
   QCalendarWidget_setHorizontalHeaderFormat(QCalendarWidgetH(QtCalendar.Widget),QCalendarWidgetShortDayNames)
  else QCalendarWidget_setHorizontalHeaderFormat(QCalendarWidgetH(QtCalendar.Widget), QCalendarWidgetNoHorizontalHeader);
  
  QCalendarWidget_setNavigationBarVisible(QCalendarWidgetH(QtCalendar.Widget), not (dsNoMonthChange in ADisplaySettings));

  if dsShowWeekNumbers in ADisplaySettings then
   QCalendarWidget_setVerticalHeaderFormat(QCalendarWidgetH(QtCalendar.Widget), QCalendarWidgetISOWeekNumbers)
  else QCalendarWidget_setVerticalHeaderFormat(QCalendarWidgetH(QtCalendar.Widget), QCalendarWidgetNoVerticalHeader);
  
  QCalendarWidget_setGridVisible(QCalendarWidgetH(QtCalendar.Widget), dsShowWeekNumbers in ADisplaySettings);

  if dsStartMonday in ADisplaySettings then
   QCalendarWidget_setFirstDayOfWeek(QCalendarWidgetH(QtCalendar.Widget), QtMonday)
  else QCalendarWidget_setFirstDayOfWeek(QCalendarWidgetH(QtCalendar.Widget), QtSunday);
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCalendar, TQtWSCustomCalendar);
////////////////////////////////////////////////////
end.
