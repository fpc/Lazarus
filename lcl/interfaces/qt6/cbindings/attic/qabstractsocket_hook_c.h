//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSOCKET_HOOK_C_H
#define QABSTRACTSOCKET_HOOK_C_H

#include "qabstractsocket_hook.h"

C_EXPORT QAbstractSocket_hookH QAbstractSocket_hook_Create(QObjectH handle);
C_EXPORT void QAbstractSocket_hook_Destroy(QAbstractSocket_hookH handle);
C_EXPORT void QAbstractSocket_hook_hook_hostFound(QAbstractSocket_hookH handle, QHookH hook);
C_EXPORT void QAbstractSocket_hook_hook_connected(QAbstractSocket_hookH handle, QHookH hook);
C_EXPORT void QAbstractSocket_hook_hook_disconnected(QAbstractSocket_hookH handle, QHookH hook);
C_EXPORT void QAbstractSocket_hook_hook_stateChanged(QAbstractSocket_hookH handle, QHookH hook);
C_EXPORT void QAbstractSocket_hook_hook_error(QAbstractSocket_hookH handle, QHookH hook);
C_EXPORT void QAbstractSocket_hook_hook_proxyAuthenticationRequired(QAbstractSocket_hookH handle, QHookH hook);

#endif
