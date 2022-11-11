//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDATETIME_C_H
#define QDATETIME_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QDateH QDate_Create();
C_EXPORT void QDate_Destroy(QDateH handle);
C_EXPORT QDateH QDate_Create2(int y, int m, int d);
C_EXPORT bool QDate_isNull(QDateH handle);
C_EXPORT bool QDate_isValid(QDateH handle);
C_EXPORT int QDate_year(QDateH handle);
C_EXPORT int QDate_month(QDateH handle);
C_EXPORT int QDate_day(QDateH handle);
C_EXPORT int QDate_dayOfWeek(QDateH handle);
C_EXPORT int QDate_dayOfYear(QDateH handle);
C_EXPORT int QDate_daysInMonth(QDateH handle);
C_EXPORT int QDate_daysInYear(QDateH handle);
C_EXPORT int QDate_weekNumber(QDateH handle, int* yearNum);
C_EXPORT void QDate_toString(QDateH handle, PWideString retval, Qt::DateFormat f);
C_EXPORT void QDate_toString2(QDateH handle, PWideString retval, PWideString format);
C_EXPORT bool QDate_setDate(QDateH handle, int year, int month, int day);
C_EXPORT void QDate_getDate(QDateH handle, int* year, int* month, int* day);
C_EXPORT void QDate_addDays(QDateH handle, QDateH retval, qint64 days);
C_EXPORT void QDate_addMonths(QDateH handle, QDateH retval, int months);
C_EXPORT void QDate_addYears(QDateH handle, QDateH retval, int years);
C_EXPORT qint64 QDate_daysTo(QDateH handle, const QDateH AnonParam1);
C_EXPORT void QDate_currentDate(QDateH retval);
C_EXPORT void QDate_fromString(QDateH retval, PWideString s, Qt::DateFormat f);
C_EXPORT void QDate_fromString2(QDateH retval, PWideString s, PWideString format);
C_EXPORT bool QDate_isValid2(int y, int m, int d);
C_EXPORT bool QDate_isLeapYear(int year);
C_EXPORT void QDate_fromJulianDay(QDateH retval, qint64 jd);
C_EXPORT qint64 QDate_toJulianDay(QDateH handle);
C_EXPORT QTimeH QTime_Create();
C_EXPORT void QTime_Destroy(QTimeH handle);
C_EXPORT QTimeH QTime_Create2(int h, int m, int s, int ms);
C_EXPORT bool QTime_isNull(QTimeH handle);
C_EXPORT bool QTime_isValid(QTimeH handle);
C_EXPORT int QTime_hour(QTimeH handle);
C_EXPORT int QTime_minute(QTimeH handle);
C_EXPORT int QTime_second(QTimeH handle);
C_EXPORT int QTime_msec(QTimeH handle);
C_EXPORT void QTime_toString(QTimeH handle, PWideString retval, Qt::DateFormat f);
C_EXPORT void QTime_toString2(QTimeH handle, PWideString retval, PWideString format);
C_EXPORT bool QTime_setHMS(QTimeH handle, int h, int m, int s, int ms);
C_EXPORT void QTime_addSecs(QTimeH handle, QTimeH retval, int secs);
C_EXPORT int QTime_secsTo(QTimeH handle, const QTimeH AnonParam1);
C_EXPORT void QTime_addMSecs(QTimeH handle, QTimeH retval, int ms);
C_EXPORT int QTime_msecsTo(QTimeH handle, const QTimeH AnonParam1);
C_EXPORT void QTime_currentTime(QTimeH retval);
C_EXPORT void QTime_fromString(QTimeH retval, PWideString s, Qt::DateFormat f);
C_EXPORT void QTime_fromString2(QTimeH retval, PWideString s, PWideString format);
C_EXPORT bool QTime_isValid2(int h, int m, int s, int ms);
C_EXPORT QDateTimeH QDateTime_Create();
C_EXPORT void QDateTime_Destroy(QDateTimeH handle);
C_EXPORT QDateTimeH QDateTime_Create3(const QDateH AnonParam1, const QTimeH AnonParam2, Qt::TimeSpec spec);
C_EXPORT QDateTimeH QDateTime_Create4(const QDateTimeH other);
C_EXPORT void QDateTime_swap(QDateTimeH handle, QDateTimeH other);
C_EXPORT bool QDateTime_isNull(QDateTimeH handle);
C_EXPORT bool QDateTime_isValid(QDateTimeH handle);
C_EXPORT void QDateTime_date(QDateTimeH handle, QDateH retval);
C_EXPORT void QDateTime_time(QDateTimeH handle, QTimeH retval);
C_EXPORT Qt::TimeSpec QDateTime_timeSpec(QDateTimeH handle);
C_EXPORT qint64 QDateTime_toMSecsSinceEpoch(QDateTimeH handle);
C_EXPORT void QDateTime_setDate(QDateTimeH handle, const QDateH date);
C_EXPORT void QDateTime_setTime(QDateTimeH handle, const QTimeH time);
C_EXPORT void QDateTime_setTimeSpec(QDateTimeH handle, Qt::TimeSpec spec);
C_EXPORT void QDateTime_setMSecsSinceEpoch(QDateTimeH handle, qint64 msecs);
C_EXPORT void QDateTime_toString(QDateTimeH handle, PWideString retval, Qt::DateFormat f);
C_EXPORT void QDateTime_toString2(QDateTimeH handle, PWideString retval, PWideString format);
C_EXPORT void QDateTime_addDays(QDateTimeH handle, QDateTimeH retval, qint64 days);
C_EXPORT void QDateTime_addMonths(QDateTimeH handle, QDateTimeH retval, int months);
C_EXPORT void QDateTime_addYears(QDateTimeH handle, QDateTimeH retval, int years);
C_EXPORT void QDateTime_addSecs(QDateTimeH handle, QDateTimeH retval, qint64 secs);
C_EXPORT void QDateTime_addMSecs(QDateTimeH handle, QDateTimeH retval, qint64 msecs);
C_EXPORT void QDateTime_toTimeSpec(QDateTimeH handle, QDateTimeH retval, Qt::TimeSpec spec);
C_EXPORT void QDateTime_toLocalTime(QDateTimeH handle, QDateTimeH retval);
C_EXPORT void QDateTime_toUTC(QDateTimeH handle, QDateTimeH retval);
C_EXPORT qint64 QDateTime_daysTo(QDateTimeH handle, const QDateTimeH AnonParam1);
C_EXPORT qint64 QDateTime_secsTo(QDateTimeH handle, const QDateTimeH AnonParam1);
C_EXPORT qint64 QDateTime_msecsTo(QDateTimeH handle, const QDateTimeH AnonParam1);
C_EXPORT void QDateTime_currentDateTime(QDateTimeH retval);
C_EXPORT void QDateTime_currentDateTimeUtc(QDateTimeH retval);
C_EXPORT void QDateTime_fromString(QDateTimeH retval, PWideString s, Qt::DateFormat f);
C_EXPORT void QDateTime_fromString2(QDateTimeH retval, PWideString s, PWideString format);
C_EXPORT void QDateTime_fromMSecsSinceEpoch(QDateTimeH retval, qint64 msecs);
C_EXPORT qint64 QDateTime_currentMSecsSinceEpoch();

#endif
