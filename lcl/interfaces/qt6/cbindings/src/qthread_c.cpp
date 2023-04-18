//******************************************************************************
//  Copyright (c) 2005-2023 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qthread_c.h"

Qt::HANDLE QThread_currentThreadId()
{
	return (Qt::HANDLE) QThread::currentThreadId();
}

QThreadH QThread_currentThread()
{
	return (QThreadH) QThread::currentThread();
}

int QThread_idealThreadCount()
{
	return (int) QThread::idealThreadCount();
}

void QThread_yieldCurrentThread()
{
	QThread::yieldCurrentThread();
}

QThreadH QThread_Create(QObjectH parent)
{
	return (QThreadH) new QThread((QObject*)parent);
}

void QThread_Destroy(QThreadH handle)
{
	delete (QThread *)handle;
}

void QThread_setPriority(QThreadH handle, QThread::Priority priority)
{
	((QThread *)handle)->setPriority(priority);
}

QThread::Priority QThread_priority(QThreadH handle)
{
	return (QThread::Priority) ((QThread *)handle)->priority();
}

bool QThread_isFinished(QThreadH handle)
{
	return (bool) ((QThread *)handle)->isFinished();
}

bool QThread_isRunning(QThreadH handle)
{
	return (bool) ((QThread *)handle)->isRunning();
}

void QThread_setStackSize(QThreadH handle, uint stackSize)
{
	((QThread *)handle)->setStackSize(stackSize);
}

uint QThread_stackSize(QThreadH handle)
{
	return (uint) ((QThread *)handle)->stackSize();
}

void QThread_exit(QThreadH handle, int retcode)
{
	((QThread *)handle)->exit(retcode);
}

QAbstractEventDispatcherH QThread_eventDispatcher(QThreadH handle)
{
	return (QAbstractEventDispatcherH) ((QThread *)handle)->eventDispatcher();
}

void QThread_setEventDispatcher(QThreadH handle, QAbstractEventDispatcherH eventDispatcher)
{
	((QThread *)handle)->setEventDispatcher((QAbstractEventDispatcher*)eventDispatcher);
}

bool QThread_event(QThreadH handle, QEventH event)
{
	return (bool) ((QThread *)handle)->event((QEvent*)event);
}

void QThread_start(QThreadH handle, QThread::Priority AnonParam1)
{
	((QThread *)handle)->start(AnonParam1);
}

void QThread_terminate(QThreadH handle)
{
	((QThread *)handle)->terminate();
}

void QThread_quit(QThreadH handle)
{
	((QThread *)handle)->quit();
}

bool QThread_wait(QThreadH handle, unsigned long time)
{
	return (bool) ((QThread *)handle)->wait(time);
}

bool QThread_isInterruptionRequested(QThreadH handle)
{
  return (bool) ((QThread *)handle)->isInterruptionRequested();
}

void QThread_requestInterruption(QThreadH handle)
{
  ((QThread *)handle)->requestInterruption();
}

int QThread_loopLevel(QThreadH handle)
{
  return (int) ((QThread *)handle)->loopLevel();
}

void QThread_sleep(unsigned long AnonParam1)
{
	QThread::sleep(AnonParam1);
}

void QThread_msleep(unsigned long AnonParam1)
{
	QThread::msleep(AnonParam1);
}

void QThread_usleep(unsigned long AnonParam1)
{
	QThread::usleep(AnonParam1);
}

