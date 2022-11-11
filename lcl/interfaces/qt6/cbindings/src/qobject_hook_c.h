//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QOBJECT_HOOK_C_H
#define QOBJECT_HOOK_C_H

#include "qobject_hook.h"

C_EXPORT QObject_hookH QObject_hook_Create(QObjectH handle);
C_EXPORT void QObject_hook_Destroy(QObject_hookH handle);
C_EXPORT void QObject_hook_hook_events(QObject_hookH handle, QHookH hook);
C_EXPORT void QObject_hook_hook_destroyed(QObject_hookH handle, QHookH hook);

#endif
