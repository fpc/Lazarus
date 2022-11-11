//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcalendarwidget_c.h"

QCalendarWidgetH QCalendarWidget_Create(QWidgetH parent)
{
	return (QCalendarWidgetH) new QCalendarWidget((QWidget*)parent);
}

void QCalendarWidget_Destroy(QCalendarWidgetH handle)
{
	delete (QCalendarWidget *)handle;
}

void QCalendarWidget_sizeHint(QCalendarWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QCalendarWidget *)handle)->sizeHint();
}

void QCalendarWidget_minimumSizeHint(QCalendarWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QCalendarWidget *)handle)->minimumSizeHint();
}

void QCalendarWidget_selectedDate(QCalendarWidgetH handle, QDateH retval)
{
	*(QDate *)retval = ((QCalendarWidget *)handle)->selectedDate();
}

int QCalendarWidget_yearShown(QCalendarWidgetH handle)
{
	return (int) ((QCalendarWidget *)handle)->yearShown();
}

int QCalendarWidget_monthShown(QCalendarWidgetH handle)
{
	return (int) ((QCalendarWidget *)handle)->monthShown();
}

void QCalendarWidget_minimumDate(QCalendarWidgetH handle, QDateH retval)
{
	*(QDate *)retval = ((QCalendarWidget *)handle)->minimumDate();
}

void QCalendarWidget_setMinimumDate(QCalendarWidgetH handle, const QDateH date)
{
	((QCalendarWidget *)handle)->setMinimumDate(*(const QDate*)date);
}

void QCalendarWidget_maximumDate(QCalendarWidgetH handle, QDateH retval)
{
	*(QDate *)retval = ((QCalendarWidget *)handle)->maximumDate();
}

void QCalendarWidget_setMaximumDate(QCalendarWidgetH handle, const QDateH date)
{
	((QCalendarWidget *)handle)->setMaximumDate(*(const QDate*)date);
}

Qt::DayOfWeek QCalendarWidget_firstDayOfWeek(QCalendarWidgetH handle)
{
	return (Qt::DayOfWeek) ((QCalendarWidget *)handle)->firstDayOfWeek();
}

void QCalendarWidget_setFirstDayOfWeek(QCalendarWidgetH handle, Qt::DayOfWeek dayOfWeek)
{
	((QCalendarWidget *)handle)->setFirstDayOfWeek(dayOfWeek);
}

bool QCalendarWidget_isNavigationBarVisible(QCalendarWidgetH handle)
{
	return (bool) ((QCalendarWidget *)handle)->isNavigationBarVisible();
}

bool QCalendarWidget_isGridVisible(QCalendarWidgetH handle)
{
	return (bool) ((QCalendarWidget *)handle)->isGridVisible();
}

QCalendarWidget::SelectionMode QCalendarWidget_selectionMode(QCalendarWidgetH handle)
{
	return (QCalendarWidget::SelectionMode) ((QCalendarWidget *)handle)->selectionMode();
}

void QCalendarWidget_setSelectionMode(QCalendarWidgetH handle, QCalendarWidget::SelectionMode mode)
{
	((QCalendarWidget *)handle)->setSelectionMode(mode);
}

QCalendarWidget::HorizontalHeaderFormat QCalendarWidget_horizontalHeaderFormat(QCalendarWidgetH handle)
{
	return (QCalendarWidget::HorizontalHeaderFormat) ((QCalendarWidget *)handle)->horizontalHeaderFormat();
}

void QCalendarWidget_setHorizontalHeaderFormat(QCalendarWidgetH handle, QCalendarWidget::HorizontalHeaderFormat format)
{
	((QCalendarWidget *)handle)->setHorizontalHeaderFormat(format);
}

QCalendarWidget::VerticalHeaderFormat QCalendarWidget_verticalHeaderFormat(QCalendarWidgetH handle)
{
	return (QCalendarWidget::VerticalHeaderFormat) ((QCalendarWidget *)handle)->verticalHeaderFormat();
}

void QCalendarWidget_setVerticalHeaderFormat(QCalendarWidgetH handle, QCalendarWidget::VerticalHeaderFormat format)
{
	((QCalendarWidget *)handle)->setVerticalHeaderFormat(format);
}

void QCalendarWidget_headerTextFormat(QCalendarWidgetH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QCalendarWidget *)handle)->headerTextFormat();
}

void QCalendarWidget_setHeaderTextFormat(QCalendarWidgetH handle, const QTextCharFormatH format)
{
	((QCalendarWidget *)handle)->setHeaderTextFormat(*(const QTextCharFormat*)format);
}

void QCalendarWidget_weekdayTextFormat(QCalendarWidgetH handle, QTextCharFormatH retval, Qt::DayOfWeek dayOfWeek)
{
	*(QTextCharFormat *)retval = ((QCalendarWidget *)handle)->weekdayTextFormat(dayOfWeek);
}

void QCalendarWidget_setWeekdayTextFormat(QCalendarWidgetH handle, Qt::DayOfWeek dayOfWeek, const QTextCharFormatH format)
{
	((QCalendarWidget *)handle)->setWeekdayTextFormat(dayOfWeek, *(const QTextCharFormat*)format);
}

void QCalendarWidget_dateTextFormat2(QCalendarWidgetH handle, QTextCharFormatH retval, const QDateH date)
{
	*(QTextCharFormat *)retval = ((QCalendarWidget *)handle)->dateTextFormat(*(const QDate*)date);
}

void QCalendarWidget_setDateTextFormat(QCalendarWidgetH handle, const QDateH date, const QTextCharFormatH format)
{
	((QCalendarWidget *)handle)->setDateTextFormat(*(const QDate*)date, *(const QTextCharFormat*)format);
}

bool QCalendarWidget_isDateEditEnabled(QCalendarWidgetH handle)
{
	return (bool) ((QCalendarWidget *)handle)->isDateEditEnabled();
}

void QCalendarWidget_setDateEditEnabled(QCalendarWidgetH handle, bool enable)
{
	((QCalendarWidget *)handle)->setDateEditEnabled(enable);
}

int QCalendarWidget_dateEditAcceptDelay(QCalendarWidgetH handle)
{
	return (int) ((QCalendarWidget *)handle)->dateEditAcceptDelay();
}

void QCalendarWidget_setDateEditAcceptDelay(QCalendarWidgetH handle, int delay)
{
	((QCalendarWidget *)handle)->setDateEditAcceptDelay(delay);
}

void QCalendarWidget_setSelectedDate(QCalendarWidgetH handle, const QDateH date)
{
	((QCalendarWidget *)handle)->setSelectedDate(*(const QDate*)date);
}

void QCalendarWidget_setDateRange(QCalendarWidgetH handle, const QDateH min, const QDateH max)
{
	((QCalendarWidget *)handle)->setDateRange(*(const QDate*)min, *(const QDate*)max);
}

void QCalendarWidget_setCurrentPage(QCalendarWidgetH handle, int year, int month)
{
	((QCalendarWidget *)handle)->setCurrentPage(year, month);
}

void QCalendarWidget_setGridVisible(QCalendarWidgetH handle, bool show)
{
	((QCalendarWidget *)handle)->setGridVisible(show);
}

void QCalendarWidget_setNavigationBarVisible(QCalendarWidgetH handle, bool visible)
{
	((QCalendarWidget *)handle)->setNavigationBarVisible(visible);
}

void QCalendarWidget_showNextMonth(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showNextMonth();
}

void QCalendarWidget_showPreviousMonth(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showPreviousMonth();
}

void QCalendarWidget_showNextYear(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showNextYear();
}

void QCalendarWidget_showPreviousYear(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showPreviousYear();
}

void QCalendarWidget_showSelectedDate(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showSelectedDate();
}

void QCalendarWidget_showToday(QCalendarWidgetH handle)
{
	((QCalendarWidget *)handle)->showToday();
}

