{
 *****************************************************************************
 *                             Gtk3WSCalendar.pp                             *
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSCalendar;

{$mode objfpc}{$H+}
{$I gtk3defines.inc}

interface

uses
  // Bindings
  LazGlib2, LazGdk3, LazGtk3,
  // RTL, FCL, LCL
  SysUtils, Types, Classes, Controls, Calendar, LCLType, LMessages,
  InterfaceBase, LCLProc,
  // Widgetset
  WSCalendar, WSLCLClasses, WSProc;

type

  { TGtk2WSCalendar }

  { TGtk3WSCustomCalendar }

  TGtk3WSCustomCalendar = class(TWSCustomCalendar)
  protected
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar;
      const ADisplaySettings: TDisplaySettings); override;
    (*
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override; *)
  end;


implementation
uses gtk3widgets;

{ TGtk3WSCustomCalendar }

(*
class procedure TGtk3WSCustomCalendar.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  with TGtk2Widgetset(Widgetset) do
  begin
    SetCallback(LM_MONTHCHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_YEARCHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_DAYCHANGED, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
  end;
  g_signal_handlers_disconnect_by_func(PGtkObject(AWidgetInfo^.CoreWidget),
      TGTKSignalFunc(@GtkDragDataReceived), AWidgetInfo^.LCLObject);
end;

class function TGtk3WSCustomCalendar.GetCalendar(const ACalendar: TCustomCalendar): PGtkCalendar; //inline;
begin
  Result := PGtkCalendar(GetWidgetInfo({%H-}PGtkWidget(ACalendar.Handle))^.CoreWidget);
end;
*)

class function TGtk3WSCustomCalendar.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLHandle;
(*
var
  FrameWidget, CalendarWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGtkAllocation;
  Requisition: TGtkRequisition;
  *)
begin
  Result := TLCLHandle(TGtk3Calendar.Create(AWinControl, AParams));
  (*
  FrameWidget := gtk_frame_new(nil);
  CalendarWidget := gtk_calendar_new();
  gtk_container_add(PGtkContainer(FrameWidget), CalendarWidget);
  gtk_widget_show_all(FrameWidget);
  // if we don't request it - we have a SIGFPE sometimes
  gtk_widget_size_request(CalendarWidget, @Requisition);

  Result := TLCLHandle({%H-}PtrUInt(FrameWidget));
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(FrameWidget, dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(FrameWidget, AWinControl, AParams);
  WidgetInfo^.CoreWidget := CalendarWidget;
  SetMainWidget(FrameWidget, CalendarWidget);

  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate({%H-}PGtkWidget(Result), @Allocation);

  Set_RC_Name(AWinControl, FrameWidget);
  SetCallBacks(FrameWidget, WidgetInfo);
  *)
end;

class function TGtk3WSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  Year, Month, Day: LongWord;  //used for csCalendar
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACalendar, 'GetDateTime') then
    Exit;
  TGtk3Calendar(ACalendar.Handle).GetDate(Year, Month, Day);
  DebugLn('TGtk3WSCustomCalendar.GetDateTime: Yr=',dbgs(Year),' Mo=',dbgs(Month),' Dy=',dbgs(Day));
  // gtk_calendar_get_date(GetCalendar(ACalendar), @Year, @Month, @Day);
  //For some reason, the month is zero based.
  Result := EncodeDate(Year, Month + 1, Day);
end;

class function TGtk3WSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
{ Reads the private struct of GtkCalendar directly (same approach as the Gtk2
  WS). Layout filled in lazgtk3.pas, stable across Gtk 3.x. }
var
  GtkCal: PGtkCalendar;
  Priv: PGtkCalendarPrivate;
  AllocW: gint;
  Options: TGtkCalendarDisplayOptions;
  BodyY, BodyX, DayH, ArrowW: Integer;
  R: TRect;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'HitTest') then
    Exit;
  GtkCal := PGtkCalendar(TGtk3Calendar(ACalendar.Handle).GetContainerWidget);
  if not Assigned(GtkCal) then
    Exit;
  Priv := GtkCal^.priv1;
  if not Assigned(Priv) then
    Exit;
  Options := GtkCal^.get_display_options;

  if GTK_CALENDAR_SHOW_HEADING in Options then
    BodyY := Priv^.header_h
  else
    BodyY := 0;

  if GTK_CALENDAR_SHOW_WEEK_NUMBERS in Options then
    BodyX := Priv^.week_width
  else
    BodyX := 0;

  if APoint.Y >= BodyY then
  begin
    // body
    if GTK_CALENDAR_SHOW_DAY_NAMES in Options then
      DayH := Priv^.day_name_h
    else
      DayH := 0;

    if (APoint.Y - BodyY - DayH) >= 0 then
    begin
      if APoint.X >= BodyX then
        Result := cpDate
      else
        Result := cpWeekNumber;
    end;
  end
  else
  if BodyY > 0 then
  begin
    Result := cpTitle; // at least in the heading

    ArrowW := Priv^.arrow_width;
    AllocW := PGtkWidget(GtkCal)^.get_allocated_width;

    R.Top := 3;
    R.Bottom := BodyY - 7;
    R.Left := 3;
    // month + side buttons
    R.Right := R.Left + ArrowW + 1;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    R.Left := R.Right + 1;
    R.Right := ArrowW + Integer(Priv^.max_month_width) + 1;
    if PtInRect(R, APoint) then
      Exit(cpTitleMonth);
    R.Left := R.Right;
    R.Right := R.Left + ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    // year + side buttons
    R.Right := AllocW - 3 + 1;
    R.Left := R.Right - ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    R.Right := R.Left;
    R.Left := R.Right - Integer(Priv^.max_year_width);
    if PtInRect(R, APoint) then
      Exit(cpTitleYear);
    R.Right := R.Left;
    R.Left := R.Right - ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
  end;
end;

class procedure TGtk3WSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  Year, Month, Day: Word;
begin
  if not WSCheckHandleAllocated(ACalendar, 'SetDateTime') then
    Exit;
  Year := StrToInt(FormatDateTime('yyyy', ADateTime));
  Month := StrToInt(FormatDateTime('mm', ADateTime));
  Day := StrToInt(FormatDateTime('dd', ADateTime));
  // gtk_calendar_select_month expects month in 0..11 range.
  TGtk3Calendar(ACalendar.Handle).SetDate(Year, Month - 1, Day);
end;

class procedure TGtk3WSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
var
  num: dword;
  // gtkcalendardisplayoptions : TGtkCalendarDisplayOptions;
begin
  if not WSCheckHandleAllocated(ACalendar, 'SetDisplaySettings') then
    Exit;

  num := 0;

  if (dsShowHeadings in ADisplaySettings) then
    num := Num or $01;

  if (dsShowDayNames in ADisplaySettings) then
    num := Num or $02;

  if (dsNoMonthChange in ADisplaySettings) then
    num := Num or $04;

  if (dsShowWeekNumbers in ADisplaySettings) then
    num := Num or $08;
  {
  if (dsStartMonday in ADisplaySettings) then
    num := Num or $20;
  }
  TGtk3Calendar(ACalendar.Handle).setDisplayOptions(TGtkCalendarDisplayOptions(num));
end;

(*
class procedure TGtk3WSCustomCalendar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl, PreferredWidth, PreferredHeight,
                          WithThemeSpace);
end;
*)

end.
