//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDISUBWINDOW_HOOK_C_H
#define QMDISUBWINDOW_HOOK_C_H

#include "qmdisubwindow_hook.h"

C_EXPORT QMdiSubWindow_hookH QMdiSubWindow_hook_Create(QObjectH handle);
C_EXPORT void QMdiSubWindow_hook_Destroy(QMdiSubWindow_hookH handle);
C_EXPORT void QMdiSubWindow_hook_hook_windowStateChanged(QMdiSubWindow_hookH handle, QHookH hook);
C_EXPORT void QMdiSubWindow_hook_hook_aboutToActivate(QMdiSubWindow_hookH handle, QHookH hook);

#endif
