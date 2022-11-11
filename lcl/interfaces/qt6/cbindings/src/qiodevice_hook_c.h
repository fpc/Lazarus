//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIODEVICE_HOOK_C_H
#define QIODEVICE_HOOK_C_H

#include "qiodevice_hook.h"

C_EXPORT QIODevice_hookH QIODevice_hook_Create(QObjectH handle);
C_EXPORT void QIODevice_hook_Destroy(QIODevice_hookH handle);
C_EXPORT void QIODevice_hook_hook_readyRead(QIODevice_hookH handle, QHookH hook);
C_EXPORT void QIODevice_hook_hook_bytesWritten(QIODevice_hookH handle, QHookH hook);
C_EXPORT void QIODevice_hook_hook_aboutToClose(QIODevice_hookH handle, QHookH hook);
C_EXPORT void QIODevice_hook_hook_readChannelFinished(QIODevice_hookH handle, QHookH hook);

#endif
