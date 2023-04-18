//******************************************************************************
//  Copyright (c) 2005-2023 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTHREAD_C_H
#define QTHREAD_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT Qt::HANDLE QThread_currentThreadId();
C_EXPORT QThreadH QThread_currentThread();
C_EXPORT int QThread_idealThreadCount();
C_EXPORT void QThread_yieldCurrentThread();
C_EXPORT QThreadH QThread_Create(QObjectH parent);
C_EXPORT void QThread_Destroy(QThreadH handle);
C_EXPORT void QThread_setPriority(QThreadH handle, QThread::Priority priority);
C_EXPORT QThread::Priority QThread_priority(QThreadH handle);
C_EXPORT bool QThread_isFinished(QThreadH handle);
C_EXPORT bool QThread_isRunning(QThreadH handle);
C_EXPORT void QThread_setStackSize(QThreadH handle, uint stackSize);
C_EXPORT uint QThread_stackSize(QThreadH handle);
C_EXPORT void QThread_exit(QThreadH handle, int retcode);
C_EXPORT QAbstractEventDispatcherH QThread_eventDispatcher(QThreadH handle);
C_EXPORT void QThread_setEventDispatcher(QThreadH handle, QAbstractEventDispatcherH eventDispatcher);
C_EXPORT bool QThread_event(QThreadH handle, QEventH event);
C_EXPORT void QThread_start(QThreadH handle, QThread::Priority AnonParam1);
C_EXPORT void QThread_terminate(QThreadH handle);
C_EXPORT void QThread_quit(QThreadH handle);
C_EXPORT bool QThread_wait(QThreadH handle, unsigned long time);
C_EXPORT bool QThread_isInterruptionRequested(QThreadH handle);
C_EXPORT void QThread_requestInterruption(QThreadH handle);
C_EXPORT int QThread_loopLevel(QThreadH handle);
C_EXPORT void QThread_sleep(unsigned long AnonParam1);
C_EXPORT void QThread_msleep(unsigned long AnonParam1);
C_EXPORT void QThread_usleep(unsigned long AnonParam1);

#endif
