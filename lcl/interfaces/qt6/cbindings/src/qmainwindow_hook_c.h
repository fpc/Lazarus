//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMAINWINDOW_HOOK_C_H
#define QMAINWINDOW_HOOK_C_H

#include "qmainwindow_hook.h"

C_EXPORT QMainWindow_hookH QMainWindow_hook_Create(QObjectH handle);
C_EXPORT void QMainWindow_hook_Destroy(QMainWindow_hookH handle);
C_EXPORT void QMainWindow_hook_hook_iconSizeChanged(QMainWindow_hookH handle, QHookH hook);
C_EXPORT void QMainWindow_hook_hook_toolButtonStyleChanged(QMainWindow_hookH handle, QHookH hook);

#endif
