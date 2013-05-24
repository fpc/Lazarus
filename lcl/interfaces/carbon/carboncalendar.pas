{                 --------------------------------------------
                  carboncalendar.pp  -  Carbon calendar
                  --------------------------------------------

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CarbonCalendar;

{$mode objfpc}{$H+}

interface

uses
  // System
  MacOSAll,
  // FCL
  Classes, SysUtils,
  // LCL
  LCLType, Controls,
  // Carbon
  CarbonProc, CarbonPrivate, CarbonCalendarView;

type

  { TCarbonCalendar }

  TCarbonCalendar = class(TCarbonControl)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetDateTime(const aDate: TDateTime);
    function GetDateTime: TDateTime;
  public
    property DateTime: TDateTime read GetDateTime write SetDateTime;
  end;

implementation

{ TCarbonCalendar }

procedure TCarbonCalendar.CreateWidget(const AParams: TCreateParams);
begin
  if OSError(CalendarViewCreate(GetTopParentWindow, ParamsToCarbonRect(AParams), Widget),
       Self, 'CreateWidget', 'CalendarViewCreate') then RaiseCreateWidgetError(LCLObject);
  inherited;
end;

procedure TCarbonCalendar.SetDateTime(const aDate: TDateTime);
var
  date    : CFGregorianDate;
  y, m, d : Word;
begin
  DecodeDate(aDate, y,m,d);
  FillChar(date{%H-}, sizeof(date), 0);
  date.day := d;
  date.month := m;
  date.year := y;
  CalendarSetDate(Widget, date);
end;

function TCarbonCalendar.GetDateTime: TDateTime;
var
  date    : CFGregorianDate;
begin
  CalendarGetDate(Widget, date{%H-});
  Result := EncodeDate(date.year, date.month, date.day);
end;


end.

