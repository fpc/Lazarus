{ $Id: wincedef.pp 9243 2006-05-05 05:52:08Z mattias $
                         ------------------------------
                         gtkdef.pp  -  Type definitions
                         ------------------------------

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date: 2006-05-05 09:22:08 +0330 (Fri, 05 May 2006) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type definitions needed in the Windows <-> LCL interface

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

unit WinCEDef;

{$mode objfpc}{$H+}

interface

uses
  Windows, CTypes, Classes, LCLType, Interfacebase;

const
// Used by TCalendar
  // 1. Messages
  MCM_FIRST             = $1000;
  MCM_GETCURSEL         = MCM_FIRST + 1;
  MCM_SETCURSEL         = MCM_FIRST + 2;
  MCM_GETMAXSELCOUNT    = MCM_FIRST + 3;
  MCM_SETMAXSELCOUNT    = MCM_FIRST + 4;
  MCM_GETSELRANGE       = MCM_FIRST + 5;
  MCM_SETSELRANGE       = MCM_FIRST + 6;
  MCM_GETMONTHRANGE     = MCM_FIRST + 7;
  MCM_SETDAYSTATE       = MCM_FIRST + 8;
  MCM_GETMINREQRECT     = MCM_FIRST + 9;
  MCM_SETCOLOR          = MCM_FIRST + 10;
  MCM_GETCOLOR          = MCM_FIRST + 11;
  MCM_SETTODAY          = MCM_FIRST + 12;
  MCM_GETTODAY          = MCM_FIRST + 13;
  MCM_HITTEST           = MCM_FIRST + 14;
  MCM_SETFIRSTDAYOFWEEK = MCM_FIRST + 15;
  MCM_GETFIRSTDAYOFWEEK = MCM_FIRST + 16;
  MCM_GETRANGE          = MCM_FIRST + 17;
  MCM_SETRANGE          = MCM_FIRST + 18;
  MCM_GETMONTHDELTA     = MCM_FIRST + 19;
  MCM_SETMONTHDELTA     = MCM_FIRST + 20;
  MCM_GETMAXTODAYWIDTH  = MCM_FIRST + 21;
  MCM_GETMAXNONEWIDTH   = MCM_FIRST + 22;
  // 2. colors consts
  MCSC_BACKGROUND   = 0;   // the background color (between months)
  MCSC_TEXT         = 1;   // the dates
  MCSC_TITLEBK      = 2;   // background of the title
  MCSC_TITLETEXT    = 3;
  MCSC_MONTHBK      = 4;   // background within the month cal
  MCSC_TRAILINGTEXT = 5;   // the text color of header & trailing days
  // 3. hit test consts
  MCHT_TITLE     = $00010000;
  MCHT_CALENDAR  = $00020000;
  MCHT_TODAYLINK = $00030000;
  MCHT_NONELINK  = $00040000;
  MCHT_NEXT      = $01000000;   // these indicate that hitting
  MCHT_PREV      = $02000000;  // here will go to the next/prev month
  MCHT_NOWHERE   = $00000000;
  MCHT_TITLEBK          = MCHT_TITLE;
  MCHT_TITLEMONTH       = MCHT_TITLE or $0001;
  MCHT_TITLEYEAR        = MCHT_TITLE or $0002;
  MCHT_TITLEBTNNEXT     = MCHT_TITLE or MCHT_NEXT or $0003;
  MCHT_TITLEBTNPREV     = MCHT_TITLE or MCHT_PREV or $0003;
  MCHT_CALENDARBK       = MCHT_CALENDAR;
  MCHT_CALENDARDATE     = MCHT_CALENDAR or $0001;
  MCHT_CALENDARDATENEXT = MCHT_CALENDARDATE or MCHT_NEXT;
  MCHT_CALENDARDATEPREV = MCHT_CALENDARDATE or MCHT_PREV;
  MCHT_CALENDARDAY      = MCHT_CALENDAR or $0002;
  MCHT_CALENDARWEEKNUM  = MCHT_CALENDAR or $0003;
  // 4. control style consts
  MCS_DAYSTATE      = $0001;
  MCS_MULTISELECT   = $0002;
  MCS_WEEKNUMBERS   = $0004;
  MCS_SHOWNONE      = $0080;
  MCS_NOTODAYCIRCLE = $0008;
  MCS_NOTODAY       = $0010;

type
  MCHITTESTINFO = record
    cbSize: UINT;
    pt    : TPoint;
    uHit  : UINT;          // out param
    st    : SYSTEMTIME;
  end;
  TMCMHitTestInfo = MCHITTESTINFO;
  PMCMHitTestInfo = ^TMCMHitTestInfo;

type
  { lazarus win32 Interface definition for additional timer data needed to find the callback}
  PWinCETimerInfo = ^TWinCETimerinfo;
  TWinCETimerInfo = record
    TimerID: UINT;         // the windows timer ID for this timer
    TimerFunc: TWSTimerProc; // owner function to handle timer
  end;

{$ifdef WinCE}
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MONITORENUMPROC; dwData: LPARAM): LongBool; cdecl; external KernelDLL name 'EnumDisplayMonitors';
function GetMonitorInfoW(hMonitor: HMONITOR; lpmi: PMonitorInfo): LongBool; cdecl; external KernelDLL name 'GetMonitorInfo';
function MonitorFromWindow(hWnd: HWND; dwFlags: DWORD): HMONITOR; cdecl; external KernelDLL name 'MonitorFromWindow';
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; cdecl; external KernelDLL name 'MonitorFromRect';
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; cdecl; external KernelDLL name 'MonitorFromPoint';
{$else}
function EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MONITORENUMPROC; dwData: LPARAM): LongBool; stdcall; external 'user32.dll' name 'EnumDisplayMonitors';
function GetMonitorInfoW(hMonitor: HMONITOR; lpmi: PMonitorInfo): LongBool; stdcall; external 'user32.dll' name 'GetMonitorInfoW';
function MonitorFromWindow(hWnd: HWND; dwFlags: DWORD): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromWindow';
function MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromRect';
function MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR; stdcall; external 'user32.dll' name 'MonitorFromPoint';
{$endif}

var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

implementation

end.
