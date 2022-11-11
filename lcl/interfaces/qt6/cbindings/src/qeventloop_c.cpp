//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qeventloop_c.h"

QEventLoopH QEventLoop_Create(QObjectH parent)
{
	return (QEventLoopH) new QEventLoop((QObject*)parent);
}

void QEventLoop_Destroy(QEventLoopH handle)
{
	delete (QEventLoop *)handle;
}

bool QEventLoop_processEvents(QEventLoopH handle, unsigned int flags)
{
	return (bool) ((QEventLoop *)handle)->processEvents((QEventLoop::ProcessEventsFlags)flags);
}

void QEventLoop_processEvents2(QEventLoopH handle, unsigned int flags, int maximumTime)
{
	((QEventLoop *)handle)->processEvents((QEventLoop::ProcessEventsFlags)flags, maximumTime);
}

int QEventLoop_exec(QEventLoopH handle, unsigned int flags)
{
	return (int) ((QEventLoop *)handle)->exec((QEventLoop::ProcessEventsFlags)flags);
}

void QEventLoop_exit(QEventLoopH handle, int returnCode)
{
	((QEventLoop *)handle)->exit(returnCode);
}

bool QEventLoop_isRunning(QEventLoopH handle)
{
	return (bool) ((QEventLoop *)handle)->isRunning();
}

void QEventLoop_wakeUp(QEventLoopH handle)
{
	((QEventLoop *)handle)->wakeUp();
}

bool QEventLoop_event(QEventLoopH handle, QEventH event)
{
	return (bool) ((QEventLoop *)handle)->event((QEvent*)event);
}

void QEventLoop_quit(QEventLoopH handle)
{
	((QEventLoop *)handle)->quit();
}

QEventLoopLockerH QEventLoopLocker_Create()
{
	return (QEventLoopLockerH) new QEventLoopLocker();
}

void QEventLoopLocker_Destroy(QEventLoopLockerH handle)
{
	delete (QEventLoopLocker *)handle;
}

QEventLoopLockerH QEventLoopLocker_Create2(QEventLoopH loop)
{
	return (QEventLoopLockerH) new QEventLoopLocker((QEventLoop*)loop);
}

QEventLoopLockerH QEventLoopLocker_Create3(QThreadH thread)
{
	return (QEventLoopLockerH) new QEventLoopLocker((QThread*)thread);
}

