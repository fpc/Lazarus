//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTIMER_HOOK_C_H
#define QTIMER_HOOK_C_H

#include "qtimer_hook.h"

C_EXPORT QTimer_hookH QTimer_hook_Create(QObjectH handle);
C_EXPORT void QTimer_hook_Destroy(QTimer_hookH handle);
C_EXPORT void QTimer_hook_hook_timeout(QTimer_hookH handle, QHookH hook);

#endif
