//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTIMER_C_H
#define QTIMER_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QTimerH QTimer_Create(QObjectH parent);
C_EXPORT void QTimer_Destroy(QTimerH handle);
C_EXPORT bool QTimer_isActive(QTimerH handle);
C_EXPORT int QTimer_timerId(QTimerH handle);
C_EXPORT void QTimer_setInterval(QTimerH handle, int msec);
C_EXPORT int QTimer_interval(QTimerH handle);
C_EXPORT int QTimer_remainingTime(QTimerH handle);
C_EXPORT void QTimer_setTimerType(QTimerH handle, Qt::TimerType atype);
C_EXPORT Qt::TimerType QTimer_timerType(QTimerH handle);
C_EXPORT void QTimer_setSingleShot(QTimerH handle, bool singleShot);
C_EXPORT bool QTimer_isSingleShot(QTimerH handle);
C_EXPORT void QTimer_singleShot(int msec, const QObjectH receiver, const char* member);
C_EXPORT void QTimer_singleShot2(int msec, Qt::TimerType timerType, const QObjectH receiver, const char* member);
C_EXPORT void QTimer_start(QTimerH handle, int msec);
C_EXPORT void QTimer_start2(QTimerH handle);
C_EXPORT void QTimer_stop(QTimerH handle);

#endif
