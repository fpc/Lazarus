//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABBAR_HOOK_C_H
#define QTABBAR_HOOK_C_H

#include "qtabbar_hook.h"

C_EXPORT QTabBar_hookH QTabBar_hook_Create(QObjectH handle);
C_EXPORT void QTabBar_hook_Destroy(QTabBar_hookH handle);
C_EXPORT void QTabBar_hook_hook_currentChanged(QTabBar_hookH handle, QHookH hook);
C_EXPORT void QTabBar_hook_hook_tabCloseRequested(QTabBar_hookH handle, QHookH hook);
C_EXPORT void QTabBar_hook_hook_tabMoved(QTabBar_hookH handle, QHookH hook);

#endif
