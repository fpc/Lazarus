//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QEVENTLOOP_C_H
#define QEVENTLOOP_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QEventLoopH QEventLoop_Create(QObjectH parent);
C_EXPORT void QEventLoop_Destroy(QEventLoopH handle);
C_EXPORT bool QEventLoop_processEvents(QEventLoopH handle, unsigned int flags);
C_EXPORT void QEventLoop_processEvents2(QEventLoopH handle, unsigned int flags, int maximumTime);
C_EXPORT int QEventLoop_exec(QEventLoopH handle, unsigned int flags);
C_EXPORT void QEventLoop_exit(QEventLoopH handle, int returnCode);
C_EXPORT bool QEventLoop_isRunning(QEventLoopH handle);
C_EXPORT void QEventLoop_wakeUp(QEventLoopH handle);
C_EXPORT bool QEventLoop_event(QEventLoopH handle, QEventH event);
C_EXPORT void QEventLoop_quit(QEventLoopH handle);
C_EXPORT QEventLoopLockerH QEventLoopLocker_Create();
C_EXPORT void QEventLoopLocker_Destroy(QEventLoopLockerH handle);
C_EXPORT QEventLoopLockerH QEventLoopLocker_Create2(QEventLoopH loop);
C_EXPORT QEventLoopLockerH QEventLoopLocker_Create3(QThreadH thread);

#endif
