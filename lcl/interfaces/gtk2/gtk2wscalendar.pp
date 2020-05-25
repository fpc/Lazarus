{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSCalendar.pp                             * 
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
unit Gtk2WSCalendar;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  glib2, gdk2, gtk2, SysUtils, Types, Classes,
  // RTL, FCL, LCL
  Controls, Calendar, LCLType, LMessages,
  InterfaceBase, LCLProc,
  // Widgetset
  Gtk2Proc, Gtk2Def, Gtk2Int, Gtk2WsControls,
  WSCalendar, WSLCLClasses, WSProc;

type

  { TGtk2WSCalendar }

  { TGtk2WSCustomCalendar }

  TGtk2WSCustomCalendar = class(TWSCustomCalendar)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class function GetCalendar(const ACalendar: TCustomCalendar): PGtkCalendar; //inline;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; override;
    class function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; override;
    class procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); override;
    class procedure SetDisplaySettings(const ACalendar: TCustomCalendar;
      const ADisplaySettings: TDisplaySettings); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;


implementation

type
  TGtkCalendarPrivate = record
    header_win: PGdkWindow;
    day_name_win: PGdkWindow;
    main_win: PGdkWindow;
    week_win: PGdkWindow;
    arrow_win: array[0..3] of PGdkWindow;

    header_h: guint;
    day_name_h: guint;
    main_h: guint;

    arrow_state: array[0..3] of guint;
    arrow_width: guint;
    max_month_width: guint;
    max_year_width: guint;

    day_width: guint;
    week_width: guint;

    min_day_width: guint;
    max_day_char_width: guint;
    max_day_char_ascent: guint;
    max_day_char_descent: guint;
    max_label_char_ascent: guint;
    max_label_char_descent: guint;
    max_week_char_width: guint;
  end;
  PGtkCalendarPrivate = ^TGtkCalendarPrivate;

{ TGtk2WSCustomCalendar }

class procedure TGtk2WSCustomCalendar.SetCallbacks(const AGtkWidget: PGtkWidget;
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

class function TGtk2WSCustomCalendar.GetCalendar(const ACalendar: TCustomCalendar): PGtkCalendar; //inline;
begin
  Result := PGtkCalendar(GetWidgetInfo({%H-}PGtkWidget(ACalendar.Handle))^.CoreWidget);
end;

class function TGtk2WSCustomCalendar.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  FrameWidget, CalendarWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGtkAllocation;
  Requisition: TGtkRequisition;
begin
  FrameWidget := gtk_frame_new(nil);
  CalendarWidget := gtk_calendar_new();
  gtk_container_add(PGtkContainer(FrameWidget), CalendarWidget);
  gtk_widget_show_all(FrameWidget);
  // if we don't request it - we have a SIGFPE sometimes
  gtk_widget_size_request(CalendarWidget, @Requisition);

  Result := TLCLIntfHandle({%H-}PtrUInt(FrameWidget));
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
end;

class procedure TGtk2WSCustomCalendar.DestroyHandle(
  const AWinControl: TWinControl);
begin
  TGtk2WSWinControl.DestroyHandle(AWinControl);
  //inherited DestroyHandle(AWinControl);
end;

class function TGtk2WSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  Year, Month, Day: guint;  //used for csCalendar
begin
  Result := 0;
  if not WSCheckHandleAllocated(ACalendar, 'GetDateTime') then
    Exit;
  gtk_calendar_get_date(GetCalendar(ACalendar), @Year, @Month, @Day);
  //For some reason, the month is zero based.
  Result := EncodeDate(Year, Month + 1, Day);
end;

class function TGtk2WSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
var
  GtkCalendar: PGtkCalendar;
  Style: PGtkStyle;
  FrameW, FrameH, BodyY, BodyX, DayH, ArrowW: Integer;
  Options: TGtkCalendarDisplayOptions;
  R: TRect;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'HitTest') then
    Exit;

  GtkCalendar := GetCalendar(ACalendar);
  Style := gtk_widget_get_style({%H-}PGtkWidget(ACalendar.Handle));
  FrameW := gtk_widget_get_xthickness(Style);
  FrameH := gtk_widget_get_Ythickness(Style);

  Options := gtk_calendar_get_display_options(GtkCalendar);

  if Ord(Options) and Ord(GTK_CALENDAR_SHOW_HEADING) <> 0 then
    BodyY := PGtkCalendarPrivate(GtkCalendar^.private_data)^.header_h
  else
    BodyY := 0;

  if Ord(Options) and Ord(GTK_CALENDAR_SHOW_WEEK_NUMBERS) <> 0 then
    BodyX := PGtkCalendarPrivate(GtkCalendar^.private_data)^.week_width
  else
    BodyX := 0;

  if APoint.Y >= BodyY + FrameH then
  begin
    // we are in the body
    if Ord(Options) and Ord(GTK_CALENDAR_SHOW_DAY_NAMES) <> 0 then
      DayH := PGtkCalendarPrivate(GtkCalendar^.private_data)^.day_name_h
    else
      DayH := 0;

    if (APoint.Y - BodyY - DayH - FrameH >= 0) then
    begin
      if APoint.X >= BodyX + FrameW then
        Result := cpDate
      else
        Result := cpWeekNumber;
    end;
  end
  else
  if BodyY > 0 then
  begin
    Result := cpTitle; // we are in the header at least

    ArrowW := PGtkCalendarPrivate(GtkCalendar^.private_data)^.arrow_width;

    R.Top := 3 + FrameH;
    R.Bottom := BodyY - 7 + FrameH;
    R.Left := 3 + FrameW;
    // check month + buttons
    R.Right := R.Left + ArrowW + 1;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    R.Left := R.Right + 1;
    R.Right := FrameW + ArrowW + integer(PGtkCalendarPrivate(GtkCalendar^.private_data)^.max_month_width) + 1;
    if PtInRect(R, APoint) then
      Exit(cpTitleMonth);
    R.Left := R.Right;
    R.Right := R.Left + ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    // check year + buttons
    Style := gtk_widget_get_style(PGtkWidget(GtkCalendar));
    R.Right := PGtkWidget(GtkCalendar)^.allocation.width - 3 -
      2 * gtk_widget_get_xthickness(Style) + FrameW + 1;
    R.Left := R.Right - ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
    R.Right := R.Left;
    R.Left := R.Right - PGtkCalendarPrivate(GtkCalendar^.private_data)^.max_year_width;
    if PtInRect(R, APoint) then
      Exit(cpTitleYear);
    R.Right := R.Left;
    R.Left := R.Right - ArrowW;
    if PtInRect(R, APoint) then
      Exit(cpTitleBtn);
  end;
end;

class procedure TGtk2WSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
var
  Year, Month, Day: string;
  GtkCalendar: PGtkCalendar;
begin
  if not WSCheckHandleAllocated(ACalendar, 'SetDateTime') then
    Exit;
  GtkCalendar := GetCalendar(ACalendar);
  Year := FormatDateTime('yyyy', ADateTime);
  Month := FormatDateTime('mm', ADateTime);
  Day := FormatDateTime('dd', ADateTime);
  gtk_calendar_select_month(GtkCalendar, StrtoInt(Month) - 1, StrToInt(Year));
  gtk_calendar_select_day(GtkCalendar, StrToInt(Day));
end;

class procedure TGtk2WSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
var
  num: dword;
  gtkcalendardisplayoptions : TGtkCalendarDisplayOptions;
begin
  if not WSCheckHandleAllocated(ACalendar, 'SetDisplaySettings') then
    Exit;
  num := 0;
  if (dsShowHeadings in ADisplaySettings) then
    num := Num + (1 shl 0);

  if (dsShowDayNames in ADisplaySettings) then
    num := Num  + (1 shl 1);

  if (dsNoMonthChange in ADisplaySettings) then
    num := Num  + (1 shl 2);

  if (dsShowWeekNumbers in ADisplaySettings) then
    num := Num  + (1 shl 3);

  {
  if (dsStartMonday in ADisplaySettings) then
    num := Num  + (1 shl 4);
  }
  gtkCalendarDisplayOptions := TGtkCalendarDisplayOptions(num);
  gtk_Calendar_Display_options(GetCalendar(ACalendar), gtkCalendarDisplayOptions);
end;

class procedure TGtk2WSCustomCalendar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  GetGTKDefaultWidgetSize(AWinControl, PreferredWidth, PreferredHeight,
                          WithThemeSpace);
end;

end.
