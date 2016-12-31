//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBFRAME_HOOK_C_H
#define QWEBFRAME_HOOK_C_H

#include "qwebframe_hook.h"

C_EXPORT QWebHitTestResult_hookH QWebHitTestResult_hook_Create(QObjectH handle);
C_EXPORT void QWebHitTestResult_hook_Destroy(QWebHitTestResult_hookH handle);
C_EXPORT QWebFrame_hookH QWebFrame_hook_Create(QObjectH handle);
C_EXPORT void QWebFrame_hook_Destroy(QWebFrame_hookH handle);
C_EXPORT void QWebFrame_hook_hook_javaScriptWindowObjectCleared(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_provisionalLoad(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_titleChanged(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_urlChanged(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_initialLayoutCompleted(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_iconChanged(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_contentsSizeChanged(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_loadStarted(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_loadFinished(QWebFrame_hookH handle, QHookH hook);
C_EXPORT void QWebFrame_hook_hook_pageChanged(QWebFrame_hookH handle, QHookH hook);

#endif
