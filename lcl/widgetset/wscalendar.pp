{
 *****************************************************************************
 *                               WSCalendar.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSCalendar;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Types, Calendar,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSFactory;

type
  { TWSCustomCalendar }

  TWSCustomCalendar = class(TWSWinControl)
  published
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; virtual;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; virtual;
    class function GetCurrentView(const ACalendar: TCustomCalendar): TCalendarView; virtual;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); virtual;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; 
      const ADisplaySettings: TDisplaySettings); virtual;
    class procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
      const ADayOfWeek: TCalDayOfWeek); virtual;
  end;
  TWSCustomCalendarClass = class of TWSCustomCalendar;

  { WidgetSetRegistration }

  procedure RegisterCustomCalendar;

implementation

uses
  LResources;

class function  TWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
begin
  Result := 0.0;
end;

class function TWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart;
begin
  Result := cpNoWhere;
end;

class function TWSCustomCalendar.GetCurrentView(const ACalendar: TCustomCalendar
  ): TCalendarView;
begin
  Result := cvMonth;
end;

class procedure TWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
begin
end;

class procedure TWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
begin
end;

class procedure TWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
  const ADayOfWeek: TCalDayOfWeek);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomCalendar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCalendar;
  RegisterPropertyToSkip(TCalendar, 'ReadOnly', 'Obsoleted property', '');
//  if not WSRegisterCustomCalendar then
//    RegisterWSComponent(TCustomCalendar, TWSCustomCalendar);
  Done := True;
end;

end.
