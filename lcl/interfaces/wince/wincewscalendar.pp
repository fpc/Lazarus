{ $Id: win32wscalendar.pp 17576 2008-11-25 02:29:28Z paul $}
{
 *****************************************************************************
 *                            WinCEWSCalendar.pp                             *
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WinCEWSCalendar;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  commctrl,
  Windows,
  // LCL
  Calendar, SysUtils, Controls, LCLType,
  // Widgetset
  WSProc, WSCalendar, WSLCLClasses, WinCEDef, WinCEWSControls;

type
	
  { TWinCEWSCustomCalendar }

  TWinCEWSCustomCalendar = class(TWSCustomCalendar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings); override;
    class procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar; const ADayOfWeek: TCalDayOfWeek); override;
    class procedure SetMinMaxDate(const ACalendar: TCustomCalendar; AMinDate, AMaxDate: TDateTime); override;
    class procedure RemoveMinMaxDates(const ACalendar: TCustomCalendar); override;
  end;

implementation

uses
  WinCEInt, InterfaceBase;

{ TWinCEWSCustomCalendar }

class function TWinCEWSCustomCalendar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_DATE_CLASSES;
  {$ifdef win32}
  InitCommonControlsEx(init);
  {$else}
  InitCommonControlsEx(@init);
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'SysMonthCal32';
    WindowTitle := StrCaption;
    Flags := WS_CHILD or WS_VISIBLE;
    if dsShowWeekNumbers in TCustomCalendar(AWinControl).DisplaySettings then
        Flags := Flags or MCS_WEEKNUMBERS;
    SubClassWndProc := @WindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  SetClassLong(Result, GCL_STYLE, GetClassLong(Result, GCL_STYLE) or CS_DBLCLKS);
  // resize to proper size
  SetBounds(AWinControl, Params.Left, Params.Top, 0, 0);
end;

class procedure TWinCEWSCustomCalendar.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle: HWND;
  lRect: TRect;
begin
  WinHandle := AWinControl.Handle;
  Windows.SendMessage(WinHandle, MCM_GETMINREQRECT, 0, LPARAM(@lRect));
  Width := lRect.Right;
  Height := lRect.Bottom;
end;

class function  TWinCEWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  ST: SystemTime;
begin
  SendMessage(ACalendar.Handle, MCM_GETCURSEL, 0, LPARAM(@ST));
  with ST do
    Result := EncodeDate(WYear,WMonth,WDay);
end;

class function TWinCEWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
var
  HitTestInfo: MCHITTESTINFO;
  HitPart: DWord;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'HitTest') then
    Exit;
  FillChar(HitTestInfo, SizeOf(HitTestInfo), 0);
  HitTestInfo.cbSize := SizeOf(HitTestInfo);
  HitTestInfo.pt := APoint;
  HitPart := SendMessage(ACalendar.Handle, MCM_HITTEST, 0, LPARAM(@HitTestInfo));
  case HitPart of
    MCHT_CALENDARDATE,
    MCHT_CALENDARDATENEXT,
    MCHT_CALENDARDATEPREV: Result := cpDate;
    MCHT_CALENDARWEEKNUM : Result := cpWeekNumber;
    MCHT_TITLEBK: Result := cpTitle;
    MCHT_TITLEMONTH: Result := cpTitleMonth;
    MCHT_TITLEYEAR: Result := cpTitleYear;
    MCHT_TITLEBTNNEXT,
    MCHT_TITLEBTNPREV: Result := cpTitleBtn;
  end;
end;

class procedure TWinCEWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  ST: SystemTime;
begin
  DecodeDate(ADateTime, ST.WYear, ST.WMonth, ST.WDay);
  SendMessage(ACalendar.Handle, MCM_SETCURSEL, 0, Windows.LParam(@ST));
end;

class procedure TWinCEWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings);
var
   Style: LongInt;
begin
  if not WSCheckHandleAllocated(ACalendar, 'SetDisplaySettings') then
     Exit;
   Style := GetWindowLong(ACalendar.Handle, GWL_STYLE);
   if dsShowWeekNumbers in ASettings then
     Style := Style or MCS_WEEKNUMBERS
   else
     Style := Style and not MCS_WEEKNUMBERS;
   SetWindowLong(ACalendar.Handle, GWL_STYLE, Style);
end;

class procedure TWinCEWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar; const ADayOfWeek: TCalDayOfWeek);
begin
  // MonthCal_SetFirstDayOfWeek not supported by WinCE
end;

class procedure TWinCEWSCustomCalendar.SetMinMaxDate(
  const ACalendar: TCustomCalendar; AMinDate, AMaxDate: TDateTime);
var
  ST: packed array[0..1] of TSystemTime;
const
  WinMinDate = TDateTime(-109205.50000000000000000); // 1601-01-01
begin
  if not WSCheckHandleAllocated(ACalendar, 'TWin32WSCustomCalendar.SetMinMaxDate') then
    Exit;
  //Windows won't set the limits if AMin < WinMin, and previous limits will then still apply
  if (AMinDate < WinMinDate) then
    AMinDate := WinMinDate;
  DecodeDate(AMinDate, ST[0].Year, ST[0].Month, ST[0].Day);
  DecodeDate(AMaxDate, ST[1].Year, ST[1].Month, ST[1].Day);
  SendMessage(ACalendar.Handle, MCM_SETRANGE, Windows.WParam(GDTR_MIN or GDTR_MAX), Windows.LParam(@ST));
end;

class procedure TWinCEWSCustomCalendar.RemoveMinMaxDates(
  const ACalendar: TCustomCalendar);
var
  ST: packed array[0..1] of TSystemTime;
begin
  if not WSCheckHandleAllocated(ACalendar, 'TWinCEWSCustomCalendar.RemoveMinMaxDates') then
    Exit;
  FillChar(ST, SizeOf(ST), 0);
  SendMessage(ACalendar.Handle, MCM_SETRANGE, Windows.WParam(GDTR_MIN or GDTR_MAX), Windows.LParam(@ST));
  RecreateWnd(ACalendar); //otherwise does not seem to work (BB)
end;

end.
