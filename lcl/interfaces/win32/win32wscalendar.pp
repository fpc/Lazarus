{ $Id$}
{
 *****************************************************************************
 *                            Win32WSCalendar.pp                             * 
 *                            ------------------                             * 
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
unit Win32WSCalendar;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  CommCtrl, SysUtils, Controls, LCLType, Calendar,
////////////////////////////////////////////////////
  WSCalendar, WSLCLClasses, WSProc, Windows, Win32WSControls,
  win32proc;

type

  { TWin32WSCustomCalendar }

  TWin32WSCustomCalendar = class(TWSCustomCalendar)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function GetConstraints(const AControl: TControl;
       const AConstraints: TObject): Boolean; override;
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings); override;
  end;


implementation

uses
  Win32Int, InterfaceBase;

{ TWin32WSCustomCalendar }

class function TWin32WSCustomCalendar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'SysMonthCal32';
    WindowTitle := StrCaption;
    if dsShowWeekNumbers in TCustomCalendar(AWinControl).DisplaySettings then
      Flags := Flags or MCS_WEEKNUMBERS;
    SubClassWndProc := @WindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, False);
  Result := Params.Window;
  SetClassLong(Result, GCL_STYLE, GetClassLong(Result, GCL_STYLE) or CS_DBLCLKS);
  // resize to proper size
  SetBounds(AWinControl, Params.Left, Params.Top, 0, 0);
end;

class procedure TWin32WSCustomCalendar.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
var
  WinHandle: HWND;
  lRect: TRect;
  TodayWidth: integer;
begin
  WinHandle := AWinControl.Handle;
  Windows.SendMessage(WinHandle, MCM_GETMINREQRECT, 0, LPARAM(@lRect));
  Width := lRect.Right;
  Height := lRect.Bottom;
  // according to msdn to ensure that today string is not clipped we need to choose
  // maximal width between that rectangle and today string width
  // this needs to be done only if we are showing today string
  if (GetWindowLong(WinHandle, GWL_STYLE) and MCS_NOTODAY) = 0 then
  begin
    TodayWidth := Windows.SendMessage(WinHandle, MCM_GETMAXTODAYWIDTH, 0, 0);
    if Width < TodayWidth then
      Width := TodayWidth;
  end;
end;

class function TWin32WSCustomCalendar.GetConstraints(const AControl: TControl;
  const AConstraints: TObject): Boolean;
var
  SizeConstraints: TSizeConstraints absolute AConstraints;
  SizeRect: TRect;
  Height, Width: Integer;
begin
  Result := True;

  if (AConstraints is TSizeConstraints) and TWinControl(AControl).HandleAllocated then
  begin
    Windows.GetWindowRect(TWinControl(AControl).Handle, @SizeRect);
    Height := SizeRect.Bottom - SizeRect.Top;
    Width := SizeRect.Right - SizeRect.Left;
    SizeConstraints.SetInterfaceConstraints(Width, Height, Width, Height);
  end;
end;

class function  TWin32WSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  ST: SystemTime;
begin
  SendMessage(ACalendar.Handle, MCM_GETCURSEL, 0, LPARAM(@ST));
  with ST do
    Result := EncodeDate(WYear,WMonth,WDay);
end;

class function TWin32WSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
var
  HitTestInfo: MCHITTESTINFO;
  HitPart: DWord;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'TWin32WSCustomCalendar.HitTest') then
    Exit;
  FillChar(HitTestInfo, SizeOf(HitTestInfo), 0);
  if WindowsVersion >= wvVista then
    HitTestInfo.cbSize := SizeOf(HitTestInfo)
  else
    HitTestInfo.cbSize := 32;
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

class procedure TWin32WSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  ST: SystemTime;
begin
  DecodeDate(ADateTime, ST.WYear, ST.WMonth, ST.WDay);
  SendMessage(ACalendar.Handle, MCM_SETCURSEL, 0, Windows.LParam(@ST));
end;

class procedure TWin32WSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings);
var
  Style: LongInt;
begin
  if not WSCheckHandleAllocated(ACalendar, 'TWin32WSCustomCalendar.SetDisplaySettings') then
    Exit;
  Style := GetWindowLong(ACalendar.Handle, GWL_STYLE);
  if dsShowWeekNumbers in ASettings then
    Style := Style or MCS_WEEKNUMBERS
  else
    Style := Style and not MCS_WEEKNUMBERS;
  SetWindowLong(ACalendar.Handle, GWL_STYLE, Style);
end;

end.
