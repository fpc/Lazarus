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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
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
  SysUtils, Types, DateUtils, Controls, Calendar, LCLType, LCLIntf, LCLProc,
  // Widgetset
  WSProc, WSCalendar, WSLCLClasses;

type

  { TQtWSCustomCalendar }

  TQtWSCustomCalendar = class(TWSCustomCalendar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ADisplaySettings: TDisplaySettings); override;
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
   dsShowHeadings in ADisplaySettings, dsShowWeekNumbers in ADisplaySettings,
   dsStartMonday in ADisplaySettings);
  QtCalendar.EndUpdate;
end;

end.
