//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBENGINEVIEW_HOOK_C_H
#define QWEBENGINEVIEW_HOOK_C_H

#include "qwebengineview_hook.h"

C_EXPORT QWebEngineView_hookH QWebEngineView_hook_Create(QObjectH handle);
C_EXPORT void QWebEngineView_hook_Destroy(QWebEngineView_hookH handle);
C_EXPORT void QWebEngineView_hook_hook_loadStarted(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_loadProgress(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_loadFinished(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_titleChanged(QWebEngineView_hookH handle, QHookH hook);
/* C_EXPORT void QWebEngineView_hook_hook_statusBarMessage(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_linkClicked(QWebEngineView_hookH handle, QHookH hook); */
C_EXPORT void QWebEngineView_hook_hook_selectionChanged(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_iconChanged(QWebEngineView_hookH handle, QHookH hook);
C_EXPORT void QWebEngineView_hook_hook_urlChanged(QWebEngineView_hookH handle, QHookH hook);

#endif
