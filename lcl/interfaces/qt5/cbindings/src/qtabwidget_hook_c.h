//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABWIDGET_HOOK_C_H
#define QTABWIDGET_HOOK_C_H

#include "qtabwidget_hook.h"

C_EXPORT QTabWidget_hookH QTabWidget_hook_Create(QObjectH handle);
C_EXPORT void QTabWidget_hook_Destroy(QTabWidget_hookH handle);
C_EXPORT void QTabWidget_hook_hook_currentChanged(QTabWidget_hookH handle, QHookH hook);
C_EXPORT void QTabWidget_hook_hook_tabCloseRequested(QTabWidget_hookH handle, QHookH hook);

#endif
