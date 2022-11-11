//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTATUSBAR_HOOK_C_H
#define QSTATUSBAR_HOOK_C_H

#include "qstatusbar_hook.h"

C_EXPORT QStatusBar_hookH QStatusBar_hook_Create(QObjectH handle);
C_EXPORT void QStatusBar_hook_Destroy(QStatusBar_hookH handle);
C_EXPORT void QStatusBar_hook_hook_messageChanged(QStatusBar_hookH handle, QHookH hook);

#endif
