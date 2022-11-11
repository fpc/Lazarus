//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSOCKETNOTIFIER_HOOK_C_H
#define QSOCKETNOTIFIER_HOOK_C_H

#include "qsocketnotifier_hook.h"

C_EXPORT QSocketNotifier_hookH QSocketNotifier_hook_Create(QObjectH handle);
C_EXPORT void QSocketNotifier_hook_Destroy(QSocketNotifier_hookH handle);
C_EXPORT void QSocketNotifier_hook_hook_activated(QSocketNotifier_hookH handle, QHookH hook);

#endif
