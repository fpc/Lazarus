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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  Calendar, SysUtils, Controls, LCLType,
////////////////////////////////////////////////////
  WSCalendar, WSLCLClasses, Windows, Win32Def, Win32WSControls;

type

  { TWin32WSCustomCalendar }

  TWin32WSCustomCalendar = class(TWSCustomCalendar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); override;
    class function  GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings); override;
    class procedure SetReadOnly(const ACalendar: TCustomCalendar; const AReadOnly: boolean); override;
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
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := 'SysMonthCal32';
    WindowTitle := StrCaption;
    Flags := WS_CHILD or WS_VISIBLE;
    SubClassWndProc := @WindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
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
begin
  WinHandle := AWinControl.Handle;
  Windows.SendMessage(WinHandle, MCM_GETMINREQRECT, 0, LPARAM(@lRect));
  Width := lRect.Right;
  Height := lRect.Bottom;
end;

class function  TWin32WSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  ST: SystemTime;
begin
  SendMessage(ACalendar.Handle, MCM_GETCURSEL, 0, LPARAM(@ST));
  with ST do
    Result := EncodeDate(WYear,WMonth,WDay);
end;

class procedure TWin32WSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  ST: SystemTime;
begin
  DecodeDate(ADateTime, ST.WYear, ST.WMonth, ST.WDay);
  SendMessage(ACalendar.Handle, MCM_SETCURSEL, 0, Windows.LParam(@ST));
end;

class procedure TWin32WSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar; const ASettings: TDisplaySettings);
begin
  // TODO: implement me!
end;

class procedure TWin32WSCustomCalendar.SetReadOnly(const ACalendar: TCustomCalendar; const AReadOnly: boolean);
begin
  // TODO: implement me!
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomCalendar, TWin32WSCustomCalendar);
////////////////////////////////////////////////////
end.
