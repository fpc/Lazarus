//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCALENDARWIDGET_C_H
#define QCALENDARWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QCalendarWidgetH QCalendarWidget_Create(QWidgetH parent);
C_EXPORT void QCalendarWidget_Destroy(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_sizeHint(QCalendarWidgetH handle, PSize retval);
C_EXPORT void QCalendarWidget_minimumSizeHint(QCalendarWidgetH handle, PSize retval);
C_EXPORT void QCalendarWidget_selectedDate(QCalendarWidgetH handle, QDateH retval);
C_EXPORT int QCalendarWidget_yearShown(QCalendarWidgetH handle);
C_EXPORT int QCalendarWidget_monthShown(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_minimumDate(QCalendarWidgetH handle, QDateH retval);
C_EXPORT void QCalendarWidget_setMinimumDate(QCalendarWidgetH handle, const QDateH date);
C_EXPORT void QCalendarWidget_maximumDate(QCalendarWidgetH handle, QDateH retval);
C_EXPORT void QCalendarWidget_setMaximumDate(QCalendarWidgetH handle, const QDateH date);
C_EXPORT Qt::DayOfWeek QCalendarWidget_firstDayOfWeek(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setFirstDayOfWeek(QCalendarWidgetH handle, Qt::DayOfWeek dayOfWeek);
C_EXPORT bool QCalendarWidget_isNavigationBarVisible(QCalendarWidgetH handle);
C_EXPORT bool QCalendarWidget_isGridVisible(QCalendarWidgetH handle);
C_EXPORT QCalendarWidget::SelectionMode QCalendarWidget_selectionMode(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setSelectionMode(QCalendarWidgetH handle, QCalendarWidget::SelectionMode mode);
C_EXPORT QCalendarWidget::HorizontalHeaderFormat QCalendarWidget_horizontalHeaderFormat(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setHorizontalHeaderFormat(QCalendarWidgetH handle, QCalendarWidget::HorizontalHeaderFormat format);
C_EXPORT QCalendarWidget::VerticalHeaderFormat QCalendarWidget_verticalHeaderFormat(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setVerticalHeaderFormat(QCalendarWidgetH handle, QCalendarWidget::VerticalHeaderFormat format);
C_EXPORT void QCalendarWidget_headerTextFormat(QCalendarWidgetH handle, QTextCharFormatH retval);
C_EXPORT void QCalendarWidget_setHeaderTextFormat(QCalendarWidgetH handle, const QTextCharFormatH format);
C_EXPORT void QCalendarWidget_weekdayTextFormat(QCalendarWidgetH handle, QTextCharFormatH retval, Qt::DayOfWeek dayOfWeek);
C_EXPORT void QCalendarWidget_setWeekdayTextFormat(QCalendarWidgetH handle, Qt::DayOfWeek dayOfWeek, const QTextCharFormatH format);
C_EXPORT void QCalendarWidget_dateTextFormat2(QCalendarWidgetH handle, QTextCharFormatH retval, const QDateH date);
C_EXPORT void QCalendarWidget_setDateTextFormat(QCalendarWidgetH handle, const QDateH date, const QTextCharFormatH format);
C_EXPORT bool QCalendarWidget_isDateEditEnabled(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setDateEditEnabled(QCalendarWidgetH handle, bool enable);
C_EXPORT int QCalendarWidget_dateEditAcceptDelay(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_setDateEditAcceptDelay(QCalendarWidgetH handle, int delay);
C_EXPORT void QCalendarWidget_setSelectedDate(QCalendarWidgetH handle, const QDateH date);
C_EXPORT void QCalendarWidget_setDateRange(QCalendarWidgetH handle, const QDateH min, const QDateH max);
C_EXPORT void QCalendarWidget_setCurrentPage(QCalendarWidgetH handle, int year, int month);
C_EXPORT void QCalendarWidget_setGridVisible(QCalendarWidgetH handle, bool show);
C_EXPORT void QCalendarWidget_setNavigationBarVisible(QCalendarWidgetH handle, bool visible);
C_EXPORT void QCalendarWidget_showNextMonth(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_showPreviousMonth(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_showNextYear(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_showPreviousYear(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_showSelectedDate(QCalendarWidgetH handle);
C_EXPORT void QCalendarWidget_showToday(QCalendarWidgetH handle);

#endif
