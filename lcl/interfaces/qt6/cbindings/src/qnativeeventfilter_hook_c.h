//******************************************************************************
//  Copyright (c) 2005-2022 by Matteo Salvi
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

#ifndef Q_NATIVEEVENTFILTER_HOOK_C_H
#define Q_NATIVEEVENTFILTER_HOOK_C_H

#include "qnativeeventfilter_hook.h"
#include "pascalbind.h"

C_EXPORT Q_NativeEventFilter_hookH Q_NativeEventFilter_hook_Create(QCoreApplicationH handle);
C_EXPORT void Q_NativeEventFilter_hook_Destroy(Q_NativeEventFilter_hookH handle);
C_EXPORT void Q_NativeEventFilter_hook_installfilter(Q_NativeEventFilter_hookH handle, QHookH hook);
C_EXPORT void Q_NativeEventFilter_hook_destroyed(Q_NativeEventFilter_hookH handle, QHookH hook);
C_EXPORT void Q_NativeEventFilter_hook_removefilter(Q_NativeEventFilter_hookH handle);

#endif
