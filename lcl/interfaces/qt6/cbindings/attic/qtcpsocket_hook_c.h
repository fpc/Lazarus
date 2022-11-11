//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSOCKET_HOOK_C_H
#define QTCPSOCKET_HOOK_C_H

#include "qtcpsocket_hook.h"

C_EXPORT QTcpSocket_hookH QTcpSocket_hook_Create(QObjectH handle);
C_EXPORT void QTcpSocket_hook_Destroy(QTcpSocket_hookH handle);

#endif
