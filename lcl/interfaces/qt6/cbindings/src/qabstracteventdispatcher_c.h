//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTEVENTDISPATCHER_C_H
#define QABSTRACTEVENTDISPATCHER_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QAbstractEventDispatcherH QAbstractEventDispatcher_instance(QThreadH thread);
C_EXPORT bool QAbstractEventDispatcher_processEvents(QAbstractEventDispatcherH handle, unsigned int flags);
C_EXPORT void QAbstractEventDispatcher_registerSocketNotifier(QAbstractEventDispatcherH handle, QSocketNotifierH notifier);
C_EXPORT void QAbstractEventDispatcher_unregisterSocketNotifier(QAbstractEventDispatcherH handle, QSocketNotifierH notifier);
C_EXPORT int QAbstractEventDispatcher_registerTimer(QAbstractEventDispatcherH handle, int interval, Qt::TimerType timerType, QObjectH object);
C_EXPORT void QAbstractEventDispatcher_registerTimer2(QAbstractEventDispatcherH handle, int timerId, int interval, Qt::TimerType timerType, QObjectH object);
C_EXPORT bool QAbstractEventDispatcher_unregisterTimer(QAbstractEventDispatcherH handle, int timerId);
C_EXPORT bool QAbstractEventDispatcher_unregisterTimers(QAbstractEventDispatcherH handle, QObjectH object);
C_EXPORT int QAbstractEventDispatcher_remainingTime(QAbstractEventDispatcherH handle, int timerId);
C_EXPORT void QAbstractEventDispatcher_wakeUp(QAbstractEventDispatcherH handle);
C_EXPORT void QAbstractEventDispatcher_interrupt(QAbstractEventDispatcherH handle);
C_EXPORT void QAbstractEventDispatcher_startingUp(QAbstractEventDispatcherH handle);
C_EXPORT void QAbstractEventDispatcher_closingDown(QAbstractEventDispatcherH handle);
C_EXPORT void QAbstractEventDispatcher_installNativeEventFilter(QAbstractEventDispatcherH handle, QAbstractNativeEventFilterH filterObj);
C_EXPORT void QAbstractEventDispatcher_removeNativeEventFilter(QAbstractEventDispatcherH handle, QAbstractNativeEventFilterH filterObj);
C_EXPORT bool QAbstractEventDispatcher_filterNativeEvent(QAbstractEventDispatcherH handle, const QByteArrayH eventType, void* message, long* result);

#endif
