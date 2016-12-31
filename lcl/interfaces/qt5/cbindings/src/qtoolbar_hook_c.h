//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBAR_HOOK_C_H
#define QTOOLBAR_HOOK_C_H

#include "qtoolbar_hook.h"

C_EXPORT QToolBar_hookH QToolBar_hook_Create(QObjectH handle);
C_EXPORT void QToolBar_hook_Destroy(QToolBar_hookH handle);
C_EXPORT void QToolBar_hook_hook_actionTriggered(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_movableChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_allowedAreasChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_orientationChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_iconSizeChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_toolButtonStyleChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_topLevelChanged(QToolBar_hookH handle, QHookH hook);
C_EXPORT void QToolBar_hook_hook_visibilityChanged(QToolBar_hookH handle, QHookH hook);

#endif
