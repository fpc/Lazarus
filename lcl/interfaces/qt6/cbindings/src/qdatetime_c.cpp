//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdatetime_c.h"

QDateH QDate_Create()
{
	return (QDateH) new QDate();
}

void QDate_Destroy(QDateH handle)
{
	delete (QDate *)handle;
}

QDateH QDate_Create2(int y, int m, int d)
{
	return (QDateH) new QDate(y, m, d);
}

bool QDate_isNull(QDateH handle)
{
	return (bool) ((QDate *)handle)->isNull();
}

bool QDate_isValid(QDateH handle)
{
	return (bool) ((QDate *)handle)->isValid();
}

int QDate_year(QDateH handle)
{
	return (int) ((QDate *)handle)->year();
}

int QDate_month(QDateH handle)
{
	return (int) ((QDate *)handle)->month();
}

int QDate_day(QDateH handle)
{
	return (int) ((QDate *)handle)->day();
}

int QDate_dayOfWeek(QDateH handle)
{
	return (int) ((QDate *)handle)->dayOfWeek();
}

int QDate_dayOfYear(QDateH handle)
{
	return (int) ((QDate *)handle)->dayOfYear();
}

int QDate_daysInMonth(QDateH handle)
{
	return (int) ((QDate *)handle)->daysInMonth();
}

int QDate_daysInYear(QDateH handle)
{
	return (int) ((QDate *)handle)->daysInYear();
}

int QDate_weekNumber(QDateH handle, int* yearNum)
{
	return (int) ((QDate *)handle)->weekNumber(yearNum);
}

void QDate_toString(QDateH handle, PWideString retval, Qt::DateFormat f)
{
	QString t_retval;
	t_retval = ((QDate *)handle)->toString(f);
	copyQStringToPWideString(t_retval, retval);
}

void QDate_toString2(QDateH handle, PWideString retval, PWideString format)
{
	QString t_retval;
	QString t_format;
	copyPWideStringToQString(format, t_format);
	t_retval = ((QDate *)handle)->toString(t_format);
	copyQStringToPWideString(t_retval, retval);
}

bool QDate_setDate(QDateH handle, int year, int month, int day)
{
	return (bool) ((QDate *)handle)->setDate(year, month, day);
}

void QDate_getDate(QDateH handle, int* year, int* month, int* day)
{
	((QDate *)handle)->getDate(year, month, day);
}

void QDate_addDays(QDateH handle, QDateH retval, qint64 days)
{
	*(QDate *)retval = ((QDate *)handle)->addDays(days);
}

void QDate_addMonths(QDateH handle, QDateH retval, int months)
{
	*(QDate *)retval = ((QDate *)handle)->addMonths(months);
}

void QDate_addYears(QDateH handle, QDateH retval, int years)
{
	*(QDate *)retval = ((QDate *)handle)->addYears(years);
}

qint64 QDate_daysTo(QDateH handle, const QDateH AnonParam1)
{
	return (qint64) ((QDate *)handle)->daysTo(*(const QDate*)AnonParam1);
}

void QDate_currentDate(QDateH retval)
{
	*(QDate *)retval = QDate::currentDate();
}

void QDate_fromString(QDateH retval, PWideString s, Qt::DateFormat f)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	*(QDate *)retval = QDate::fromString(t_s, f);
}

void QDate_fromString2(QDateH retval, PWideString s, PWideString format)
{
	QString t_s;
	QString t_format;
	copyPWideStringToQString(s, t_s);
	copyPWideStringToQString(format, t_format);
	*(QDate *)retval = QDate::fromString(t_s, t_format);
}

bool QDate_isValid2(int y, int m, int d)
{
	return (bool) QDate::isValid(y, m, d);
}

bool QDate_isLeapYear(int year)
{
	return (bool) QDate::isLeapYear(year);
}

void QDate_fromJulianDay(QDateH retval, qint64 jd)
{
	*(QDate *)retval = QDate::fromJulianDay(jd);
}

qint64 QDate_toJulianDay(QDateH handle)
{
	return (qint64) ((QDate *)handle)->toJulianDay();
}

QTimeH QTime_Create()
{
	return (QTimeH) new QTime();
}

void QTime_Destroy(QTimeH handle)
{
	delete (QTime *)handle;
}

QTimeH QTime_Create2(int h, int m, int s, int ms)
{
	return (QTimeH) new QTime(h, m, s, ms);
}

bool QTime_isNull(QTimeH handle)
{
	return (bool) ((QTime *)handle)->isNull();
}

bool QTime_isValid(QTimeH handle)
{
	return (bool) ((QTime *)handle)->isValid();
}

int QTime_hour(QTimeH handle)
{
	return (int) ((QTime *)handle)->hour();
}

int QTime_minute(QTimeH handle)
{
	return (int) ((QTime *)handle)->minute();
}

int QTime_second(QTimeH handle)
{
	return (int) ((QTime *)handle)->second();
}

int QTime_msec(QTimeH handle)
{
	return (int) ((QTime *)handle)->msec();
}

void QTime_toString(QTimeH handle, PWideString retval, Qt::DateFormat f)
{
	QString t_retval;
	t_retval = ((QTime *)handle)->toString(f);
	copyQStringToPWideString(t_retval, retval);
}

void QTime_toString2(QTimeH handle, PWideString retval, PWideString format)
{
	QString t_retval;
	QString t_format;
	copyPWideStringToQString(format, t_format);
	t_retval = ((QTime *)handle)->toString(t_format);
	copyQStringToPWideString(t_retval, retval);
}

bool QTime_setHMS(QTimeH handle, int h, int m, int s, int ms)
{
	return (bool) ((QTime *)handle)->setHMS(h, m, s, ms);
}

void QTime_addSecs(QTimeH handle, QTimeH retval, int secs)
{
	*(QTime *)retval = ((QTime *)handle)->addSecs(secs);
}

int QTime_secsTo(QTimeH handle, const QTimeH AnonParam1)
{
	return (int) ((QTime *)handle)->secsTo(*(const QTime*)AnonParam1);
}

void QTime_addMSecs(QTimeH handle, QTimeH retval, int ms)
{
	*(QTime *)retval = ((QTime *)handle)->addMSecs(ms);
}

int QTime_msecsTo(QTimeH handle, const QTimeH AnonParam1)
{
	return (int) ((QTime *)handle)->msecsTo(*(const QTime*)AnonParam1);
}

void QTime_currentTime(QTimeH retval)
{
	*(QTime *)retval = QTime::currentTime();
}

void QTime_fromString(QTimeH retval, PWideString s, Qt::DateFormat f)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	*(QTime *)retval = QTime::fromString(t_s, f);
}

void QTime_fromString2(QTimeH retval, PWideString s, PWideString format)
{
	QString t_s;
	QString t_format;
	copyPWideStringToQString(s, t_s);
	copyPWideStringToQString(format, t_format);
	*(QTime *)retval = QTime::fromString(t_s, t_format);
}

bool QTime_isValid2(int h, int m, int s, int ms)
{
	return (bool) QTime::isValid(h, m, s, ms);
}

QDateTimeH QDateTime_Create()
{
	return (QDateTimeH) new QDateTime();
}

void QDateTime_Destroy(QDateTimeH handle)
{
	delete (QDateTime *)handle;
}

QDateTimeH QDateTime_Create3(const QDateH AnonParam1, const QTimeH AnonParam2, Qt::TimeSpec spec)
{
	return (QDateTimeH) new QDateTime(*(const QDate*)AnonParam1, *(const QTime*)AnonParam2, spec);
}

QDateTimeH QDateTime_Create4(const QDateTimeH other)
{
	return (QDateTimeH) new QDateTime(*(const QDateTime*)other);
}

void QDateTime_swap(QDateTimeH handle, QDateTimeH other)
{
	((QDateTime *)handle)->swap(*(QDateTime*)other);
}

bool QDateTime_isNull(QDateTimeH handle)
{
	return (bool) ((QDateTime *)handle)->isNull();
}

bool QDateTime_isValid(QDateTimeH handle)
{
	return (bool) ((QDateTime *)handle)->isValid();
}

void QDateTime_date(QDateTimeH handle, QDateH retval)
{
	*(QDate *)retval = ((QDateTime *)handle)->date();
}

void QDateTime_time(QDateTimeH handle, QTimeH retval)
{
	*(QTime *)retval = ((QDateTime *)handle)->time();
}

Qt::TimeSpec QDateTime_timeSpec(QDateTimeH handle)
{
	return (Qt::TimeSpec) ((QDateTime *)handle)->timeSpec();
}

qint64 QDateTime_toMSecsSinceEpoch(QDateTimeH handle)
{
	return (qint64) ((QDateTime *)handle)->toMSecsSinceEpoch();
}

void QDateTime_setDate(QDateTimeH handle, const QDateH date)
{
	((QDateTime *)handle)->setDate(*(const QDate*)date);
}

void QDateTime_setTime(QDateTimeH handle, const QTimeH time)
{
	((QDateTime *)handle)->setTime(*(const QTime*)time);
}

void QDateTime_setTimeSpec(QDateTimeH handle, Qt::TimeSpec spec)
{
	((QDateTime *)handle)->setTimeSpec(spec);
}

void QDateTime_setMSecsSinceEpoch(QDateTimeH handle, qint64 msecs)
{
	((QDateTime *)handle)->setMSecsSinceEpoch(msecs);
}

void QDateTime_toString(QDateTimeH handle, PWideString retval, Qt::DateFormat f)
{
	QString t_retval;
	t_retval = ((QDateTime *)handle)->toString(f);
	copyQStringToPWideString(t_retval, retval);
}

void QDateTime_toString2(QDateTimeH handle, PWideString retval, PWideString format)
{
	QString t_retval;
	QString t_format;
	copyPWideStringToQString(format, t_format);
	t_retval = ((QDateTime *)handle)->toString(t_format);
	copyQStringToPWideString(t_retval, retval);
}

void QDateTime_addDays(QDateTimeH handle, QDateTimeH retval, qint64 days)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->addDays(days);
}

void QDateTime_addMonths(QDateTimeH handle, QDateTimeH retval, int months)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->addMonths(months);
}

void QDateTime_addYears(QDateTimeH handle, QDateTimeH retval, int years)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->addYears(years);
}

void QDateTime_addSecs(QDateTimeH handle, QDateTimeH retval, qint64 secs)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->addSecs(secs);
}

void QDateTime_addMSecs(QDateTimeH handle, QDateTimeH retval, qint64 msecs)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->addMSecs(msecs);
}

void QDateTime_toTimeSpec(QDateTimeH handle, QDateTimeH retval, Qt::TimeSpec spec)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->toTimeSpec(spec);
}

void QDateTime_toLocalTime(QDateTimeH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->toLocalTime();
}

void QDateTime_toUTC(QDateTimeH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QDateTime *)handle)->toUTC();
}

qint64 QDateTime_daysTo(QDateTimeH handle, const QDateTimeH AnonParam1)
{
	return (qint64) ((QDateTime *)handle)->daysTo(*(const QDateTime*)AnonParam1);
}

qint64 QDateTime_secsTo(QDateTimeH handle, const QDateTimeH AnonParam1)
{
	return (qint64) ((QDateTime *)handle)->secsTo(*(const QDateTime*)AnonParam1);
}

qint64 QDateTime_msecsTo(QDateTimeH handle, const QDateTimeH AnonParam1)
{
	return (qint64) ((QDateTime *)handle)->msecsTo(*(const QDateTime*)AnonParam1);
}

void QDateTime_currentDateTime(QDateTimeH retval)
{
	*(QDateTime *)retval = QDateTime::currentDateTime();
}

void QDateTime_currentDateTimeUtc(QDateTimeH retval)
{
	*(QDateTime *)retval = QDateTime::currentDateTimeUtc();
}

void QDateTime_fromString(QDateTimeH retval, PWideString s, Qt::DateFormat f)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	*(QDateTime *)retval = QDateTime::fromString(t_s, f);
}

void QDateTime_fromString2(QDateTimeH retval, PWideString s, PWideString format)
{
	QString t_s;
	QString t_format;
	copyPWideStringToQString(s, t_s);
	copyPWideStringToQString(format, t_format);
	*(QDateTime *)retval = QDateTime::fromString(t_s, t_format);
}

void QDateTime_fromMSecsSinceEpoch(QDateTimeH retval, qint64 msecs)
{
	*(QDateTime *)retval = QDateTime::fromMSecsSinceEpoch(msecs);
}

qint64 QDateTime_currentMSecsSinceEpoch()
{
	return (qint64) QDateTime::currentMSecsSinceEpoch();
}

