//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QUDPSOCKET_HOOK_C_H
#define QUDPSOCKET_HOOK_C_H

#include "qudpsocket_hook.h"

C_EXPORT QUdpSocket_hookH QUdpSocket_hook_Create(QObjectH handle);
C_EXPORT void QUdpSocket_hook_Destroy(QUdpSocket_hookH handle);

#endif
