//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstracteventdispatcher_c.h"

QAbstractEventDispatcherH QAbstractEventDispatcher_instance(QThreadH thread)
{
	return (QAbstractEventDispatcherH) QAbstractEventDispatcher::instance((QThread*)thread);
}

bool QAbstractEventDispatcher_processEvents(QAbstractEventDispatcherH handle, unsigned int flags)
{
	return (bool) ((QAbstractEventDispatcher *)handle)->processEvents((QEventLoop::ProcessEventsFlags)flags);
}

void QAbstractEventDispatcher_registerSocketNotifier(QAbstractEventDispatcherH handle, QSocketNotifierH notifier)
{
	((QAbstractEventDispatcher *)handle)->registerSocketNotifier((QSocketNotifier*)notifier);
}

void QAbstractEventDispatcher_unregisterSocketNotifier(QAbstractEventDispatcherH handle, QSocketNotifierH notifier)
{
	((QAbstractEventDispatcher *)handle)->unregisterSocketNotifier((QSocketNotifier*)notifier);
}

int QAbstractEventDispatcher_registerTimer(QAbstractEventDispatcherH handle, int interval, Qt::TimerType timerType, QObjectH object)
{
	return (int) ((QAbstractEventDispatcher *)handle)->registerTimer(interval, timerType, (QObject*)object);
}

void QAbstractEventDispatcher_registerTimer2(QAbstractEventDispatcherH handle, int timerId, int interval, Qt::TimerType timerType, QObjectH object)
{
	((QAbstractEventDispatcher *)handle)->registerTimer(timerId, interval, timerType, (QObject*)object);
}

bool QAbstractEventDispatcher_unregisterTimer(QAbstractEventDispatcherH handle, int timerId)
{
	return (bool) ((QAbstractEventDispatcher *)handle)->unregisterTimer(timerId);
}

bool QAbstractEventDispatcher_unregisterTimers(QAbstractEventDispatcherH handle, QObjectH object)
{
	return (bool) ((QAbstractEventDispatcher *)handle)->unregisterTimers((QObject*)object);
}

int QAbstractEventDispatcher_remainingTime(QAbstractEventDispatcherH handle, int timerId)
{
	return (int) ((QAbstractEventDispatcher *)handle)->remainingTime(timerId);
}

void QAbstractEventDispatcher_wakeUp(QAbstractEventDispatcherH handle)
{
	((QAbstractEventDispatcher *)handle)->wakeUp();
}

void QAbstractEventDispatcher_interrupt(QAbstractEventDispatcherH handle)
{
	((QAbstractEventDispatcher *)handle)->interrupt();
}

void QAbstractEventDispatcher_startingUp(QAbstractEventDispatcherH handle)
{
	((QAbstractEventDispatcher *)handle)->startingUp();
}

void QAbstractEventDispatcher_closingDown(QAbstractEventDispatcherH handle)
{
	((QAbstractEventDispatcher *)handle)->closingDown();
}

void QAbstractEventDispatcher_installNativeEventFilter(QAbstractEventDispatcherH handle, QAbstractNativeEventFilterH filterObj)
{
	((QAbstractEventDispatcher *)handle)->installNativeEventFilter((QAbstractNativeEventFilter*)filterObj);
}

void QAbstractEventDispatcher_removeNativeEventFilter(QAbstractEventDispatcherH handle, QAbstractNativeEventFilterH filterObj)
{
	((QAbstractEventDispatcher *)handle)->removeNativeEventFilter((QAbstractNativeEventFilter*)filterObj);
}

bool QAbstractEventDispatcher_filterNativeEvent(QAbstractEventDispatcherH handle, const QByteArrayH eventType, void* message, qintptr* result)
{
	return (bool) ((QAbstractEventDispatcher *)handle)->filterNativeEvent(*(const QByteArray*)eventType, message, result);
}

