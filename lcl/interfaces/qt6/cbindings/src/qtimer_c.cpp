//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtimer_c.h"

QTimerH QTimer_Create(QObjectH parent)
{
	return (QTimerH) new QTimer((QObject*)parent);
}

void QTimer_Destroy(QTimerH handle)
{
	delete (QTimer *)handle;
}

bool QTimer_isActive(QTimerH handle)
{
	return (bool) ((QTimer *)handle)->isActive();
}

int QTimer_timerId(QTimerH handle)
{
	return (int) ((QTimer *)handle)->timerId();
}

void QTimer_setInterval(QTimerH handle, int msec)
{
	((QTimer *)handle)->setInterval(msec);
}

int QTimer_interval(QTimerH handle)
{
	return (int) ((QTimer *)handle)->interval();
}

int QTimer_remainingTime(QTimerH handle)
{
	return (int) ((QTimer *)handle)->remainingTime();
}

void QTimer_setTimerType(QTimerH handle, Qt::TimerType atype)
{
	((QTimer *)handle)->setTimerType(atype);
}

Qt::TimerType QTimer_timerType(QTimerH handle)
{
	return (Qt::TimerType) ((QTimer *)handle)->timerType();
}

void QTimer_setSingleShot(QTimerH handle, bool singleShot)
{
	((QTimer *)handle)->setSingleShot(singleShot);
}

bool QTimer_isSingleShot(QTimerH handle)
{
	return (bool) ((QTimer *)handle)->isSingleShot();
}

void QTimer_singleShot(int msec, const QObjectH receiver, const char* member)
{
	QTimer::singleShot(msec, (const QObject*)receiver, member);
}

void QTimer_singleShot2(int msec, Qt::TimerType timerType, const QObjectH receiver, const char* member)
{
	QTimer::singleShot(msec, timerType, (const QObject*)receiver, member);
}

void QTimer_start(QTimerH handle, int msec)
{
	((QTimer *)handle)->start(msec);
}

void QTimer_start2(QTimerH handle)
{
	((QTimer *)handle)->start();
}

void QTimer_stop(QTimerH handle)
{
	((QTimer *)handle)->stop();
}

