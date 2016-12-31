//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTHREAD_HOOK_C_H
#define QTHREAD_HOOK_C_H

#include "qthread_hook.h"

C_EXPORT QThread_hookH QThread_hook_Create(QObjectH handle);
C_EXPORT void QThread_hook_Destroy(QThread_hookH handle);
C_EXPORT void QThread_hook_hook_started(QThread_hookH handle, QHookH hook);
C_EXPORT void QThread_hook_hook_finished(QThread_hookH handle, QHookH hook);

#endif
