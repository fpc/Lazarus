//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSERVER_HOOK_C_H
#define QTCPSERVER_HOOK_C_H

#include "qtcpserver_hook.h"

C_EXPORT QTcpServer_hookH QTcpServer_hook_Create(QObjectH handle);
C_EXPORT void QTcpServer_hook_Destroy(QTcpServer_hookH handle);
C_EXPORT void QTcpServer_hook_hook_newConnection(QTcpServer_hookH handle, QHookH hook);
C_EXPORT void QTcpServer_hook_hook_acceptError(QTcpServer_hookH handle, QHookH hook);

#endif
