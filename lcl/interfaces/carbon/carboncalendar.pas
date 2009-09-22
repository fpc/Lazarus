{ $Id:
                  --------------------------------------------
                  carboncalendar.pp  -  Carbon calendar
                  --------------------------------------------

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
unit CarbonCalendar;

{$mode objfpc}{$H+}

interface

uses
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
  end;

implementation

{ TCarbonCalendar }

procedure TCarbonCalendar.CreateWidget(const AParams: TCreateParams);
begin
  if OSError(CalendarViewCreate(GetTopParentWindow, ParamsToCarbonRect(AParams), Widget),
       Self, 'CreateWidget', 'CalendarViewCreate') then RaiseCreateWidgetError(LCLObject);
end;


end.

