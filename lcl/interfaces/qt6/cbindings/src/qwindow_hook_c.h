//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWINDOW_HOOK_C_H
#define QWINDOW_HOOK_C_H

#include "qwindow_hook.h"

C_EXPORT QWindow_hookH QWindow_hook_Create(QObjectH handle);
C_EXPORT void QWindow_hook_Destroy(QWindow_hookH handle);
C_EXPORT void QWindow_hook_hook_screenChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_modalityChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_windowStateChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_windowTitleChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_xChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_yChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_widthChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_heightChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_minimumWidthChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_minimumHeightChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_maximumWidthChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_maximumHeightChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_visibleChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_visibilityChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_activeChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_contentOrientationChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_focusObjectChanged(QWindow_hookH handle, QHookH hook);
C_EXPORT void QWindow_hook_hook_opacityChanged(QWindow_hookH handle, QHookH hook);

#endif
