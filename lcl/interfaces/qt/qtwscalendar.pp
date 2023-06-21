{ $Id$}
{
 *****************************************************************************
 *                              QtWSCalendar.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  SysUtils, Types, DateUtils, Controls, Calendar, LCLType, LCLIntf, LCLProc,
  // Widgetset
  WSProc, WSCalendar, WSLCLClasses;

type

  { TQtWSCustomCalendar }

  TQtWSCustomCalendar = class(TWSCustomCalendar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ADisplaySettings: TDisplaySettings); override;
    class procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar; const ADayOfWeek: TCalDayOfWeek); override;
    class procedure SetMinMaxDate(const ACalendar: TCustomCalendar; AMinDate, AMaxDate: TDateTime); override;
    class procedure RemoveMinMaxDates(const ACalendar: TCustomCalendar); override;
  end;


implementation

{ TQtWSCustomCalendar }

class function TQtWSCustomCalendar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar.Create(AWinControl, AParams);

  QtCalendar.AttachEvents;

  Result := TLCLHandle(QtCalendar);
end;

class function TQtWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  Result := QtCalendar.DateTime;
end;

class function TQtWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
var
  QtCalendar: TQtCalendar;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'HitTest') then
    Exit;
  QtCalendar := TQtCalendar(ACalendar.Handle);
  Result := TCalendarPart(QtCalendar.HitTest(APoint))
end;

class procedure TQtWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar;
  const ADateTime: TDateTime);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  QtCalendar.BeginUpdate;
  QtCalendar.DateTime := ADateTime;
  QtCalendar.EndUpdate;
end;

class procedure TQtWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
 const ADisplaySettings: TDisplaySettings);
var
  QtCalendar: TQtCalendar;
  HHdrFmt: QCalendarWidgetHorizontalHeaderFormat;
  VHdrFmt: QCalendarWidgetVerticalHeaderFormat;
  SelMode: QCalendarWidgetSelectionMode;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);

  SelMode := QCalendarWidgetSingleSelection;

  if dsShowDayNames in ADisplaySettings then
    HHdrFmt := QCalendarWidgetShortDayNames
  else
    HHdrFmt := QCalendarWidgetNoHorizontalHeader;

  if dsShowWeekNumbers in ADisplaySettings then
    VHdrFmt := QCalendarWidgetISOWeekNumbers
  else
    VHdrFmt := QCalendarWidgetNoVerticalHeader;

  QtCalendar.BeginUpdate;
  QtCalendar.SetDisplaySettings(HHdrFmt, VHdrFmt, SelMode,
   dsShowHeadings in ADisplaySettings, dsShowWeekNumbers in ADisplaySettings);
  QtCalendar.EndUpdate;
end;

class procedure TQtWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
  const ADayOfWeek: TCalDayOfWeek);
var
  QtCalendar: TQtCalendar;
  dow: QtDayOfWeek;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  if ADayOfWeek = dowDefault then begin
    // QLocale_firstDayOfWeek does not yet exist in qt 4.5 --> use Monday as default
    dow := QtMonday;
  end else
    dow := QtDayOfWeek(ord(ADayOfWeek) + 1);
  QtCalendar.BeginUpdate;
  QtCalendar.SetFirstDayOfWeek(dow);
  QtCalendar.EndUpdate;
end;

class procedure TQtWSCustomCalendar.SetMinMaxDate(const ACalendar: TCustomCalendar; AMinDate, AMaxDate: TDateTime);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  QtCalendar.BeginUpdate;
  QtCalendar.MinDate := AMinDate;
  QtCalendar.MaxDate := AMaxDate;
  QtCalendar.EndUpdate;
end;

class procedure TQtWSCustomCalendar.RemoveMinMaxDates(const ACalendar: TCustomCalendar);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  QtCalendar.BeginUpdate;
  QtCalendar.MinDate := MinDateTime;
  QtCalendar.MaxDate := MaxDateTime;
  QtCalendar.EndUpdate;
end;

end.
