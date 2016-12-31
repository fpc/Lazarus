//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBVIEW_HOOK_C_H
#define QWEBVIEW_HOOK_C_H

#include "qwebview_hook.h"

C_EXPORT QWebView_hookH QWebView_hook_Create(QObjectH handle);
C_EXPORT void QWebView_hook_Destroy(QWebView_hookH handle);
C_EXPORT void QWebView_hook_hook_loadStarted(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_loadProgress(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_loadFinished(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_titleChanged(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_statusBarMessage(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_linkClicked(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_selectionChanged(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_iconChanged(QWebView_hookH handle, QHookH hook);
C_EXPORT void QWebView_hook_hook_urlChanged(QWebView_hookH handle, QHookH hook);

#endif
