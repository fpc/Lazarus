//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGUIAPPLICATION_HOOK_C_H
#define QGUIAPPLICATION_HOOK_C_H

#include "qguiapplication_hook.h"

C_EXPORT QGuiApplication_hookH QGuiApplication_hook_Create(QObjectH handle);
C_EXPORT void QGuiApplication_hook_Destroy(QGuiApplication_hookH handle);
C_EXPORT void QGuiApplication_hook_hook_applicationStateChanged(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_fontDatabaseChanged(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_primaryScreenChanged(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_screenAdded(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_screenRemoved(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_lastWindowClosed(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_focusObjectChanged(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_focusWindowChanged(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_commitDataRequest(QGuiApplication_hookH handle, QHookH hook);
C_EXPORT void QGuiApplication_hook_hook_saveStateRequest(QGuiApplication_hookH handle, QHookH hook);

#endif
